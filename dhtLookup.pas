unit dhtLookup;
INTERFACE
uses ServerLoop,dht,ObjectModel;

type
  tPID=dht.tPID;
  tSearchNode=object
    ID   :tPID;
    Addr :tNetAddr;
    LastReq:tMTime;
    reqc:byte;
    rplc:byte;{1=replied with cap, 2=replied with peers}
    end;
  tSearch=object(tTask)
    Target:tPID;
    TrID:Word;
    Query:tMemoryStream;
    Nodes:array [0..10] of tSearchNode;
    constructor Init; {for use in descendants}
    constructor Init(const iTarget:tPID);
    protected
    procedure Cleanup; virtual;
    function  AddNode(const iID:tPID; const iAddr:tNetAddr) :integer;
    procedure AddNodes(var s:tMemoryStream);
    procedure LoadNodes; {from dht and node cache}
    procedure Step;
    procedure HandleReply(var sAddr: tNetAddr; var sPID: tPID; op:byte; var st:tMemoryStream); virtual;
    private
    procedure IntHandleReply(msg:tSMsg);
    end;

IMPLEMENTATION
uses opcode,Store2;

const
  cInitAdd=6; {n of peers to add from dht}
  cInitWait=800; {Init to Step delay}
  cAddWait=1; {new peers to Step delay}
  cStepRqc=3; {max requests per step}
  cStepMinDelay=800; {min delta of requests to same peer}
  cStepPeerReqc=6; {max (unsuccessful) requests to peer}
  cStepRplc=6; {?}
  cStepPeriod=900; {max period between steps}

constructor tSearch.Init;
  var i:integer;
  begin
  for i:=high(nodes) downto 0 do Nodes[i].Addr.Clear;
  NewMsgTr(TrID, @IntHandleReply);
  tTask.Init;
end;
constructor tSearch.Init( const iTarget: tPID );
  begin
  tSearch.Init;
  Target:=iTarget;
  with Query do begin
    Init(43);
    WriteByte(opcode.dhtGetNodes);
    Write(TrID,2);
    Write(DHT.MyID,20);
    Write(Target,20);
  end;
  LoadNodes;
  Step;
end;

function tSearch.AddNode(const iID:tPID; const iAddr:tNetAddr) :integer;
  var tpfl,idx,j:byte;
  begin
  idx:=0; result:=-1;
  tpfl:=PrefixLength(iid,Target);
  {write('dhtLookup.AddPeer@',string(@self),' tpfl=',tpfl,' addr=',string(iaddr));}
  for idx:=0 to high(nodes) do begin
    if nodes[idx].addr.isNil then break;
    if nodes[idx].addr=iaddr then begin
      result:=idx;
      {writeln(' update ',idx);}
    exit end;
    if PrefixLength(nodes[idx].id,Target)<tpfl then break;
  end;
  {writeln(' insert ',idx);}
  for j:=high(nodes)-1 downto idx do nodes[j+1]:=nodes[j];
  nodes[idx].id:=iid; nodes[idx].addr:=iaddr;
  nodes[idx].reqc:=0; nodes[idx].rplc:=0;
  result:=idx;
  {if assigned(OnProgress) then OnProgress(tpfl,nodes[idx]);}
end;

procedure tSearch.Step;
  var ix:byte;
  var rqc,rpc,again:byte;
  begin
  {send request to at most 3 peers,
   count nodes that replied
  }
  rqc:=0;rpc:=0; again:=0;
  repeat
  for ix:=0 to high(nodes) do begin
    if nodes[ix].addr.isNil then break;
    if (rqc>=cStepRqc)or(rpc>=cStepRplc) then break;
    if nodes[ix].rplc>=2 then inc(rpc)
    else if (nodes[ix].reqc<=cStepPeerReqc)
         and(rqc<cStepRqc)
         {and again...}
    then begin
        inc(rqc);
        if (mNow-nodes[ix].LastReq)<cStepMinDelay then continue;
        {if nodes[ix].rplc=0 then begin}
          ServerLoop.SendMessage(Query.base^,Query.length,nodes[ix].Addr);
        {end else begin
          ...
        end;}
        writeln('Search@',string(@self),': [',rqc,',',ix,',',nodes[ix].reqc,'] ',string(nodes[ix].addr));
        inc(nodes[ix].reqc);
        nodes[ix].LastReq:=MNow;
    end;
  end; inc(again);
  until (again>1)or(rqc>=cStepRqc)or(rpc>=cStepRplc);
  if rqc=0 then begin
    writeln('Lookup@',string(@self),': Exhausted');
    SendEvent(tevError,nil);
  end
  else Shedule(cStepPeriod,@Step);
end;

procedure tSearch.IntHandleReply(msg:tSMsg);
  var sender:^tPID;
  var op:byte;
  begin
  op:=msg.st.readbyte;
  msg.st.skip(2{trid});
  sender:=msg.st.readptr(20);
  if not DHT.CheckNode(sender^, msg.source, true) then exit;
  HandleReply(msg.source,sender^,op, msg.st);
end;

procedure tSearch.AddNodes(var s:tMemoryStream);
  var id:^tPID;
  var ad:^tNetAddr;
  var node:integer;
  begin
  while s.left>=44 do begin
    id:=s.readptr(20);
    ad:=s.readptr(24);
    if DHT.CheckNode(id^, ad^, false) then continue; {is this needed?}
    node:=self.AddNode(id^, ad^);
    if node<>-1 then ;
  end;
  UnShedule(@Step);
  Shedule(cAddWait,@Step);
end;

procedure tSearch.HandleReply(var sAddr: tNetAddr; var sPID: tPID; op:byte; var st:tMemoryStream);
  var node:integer;
  begin
  node:=self.AddNode(sPID, sAddr);
  if {node=0} sPID=Target then begin
    writeln('Lookup@',string(@self),': Target Found');
    SendEvent(tevComplete,nil);
  exit end;
  if OP=opcode.dhtNodes then begin
    if node<>-1 then nodes[node].rplc:=2;
    self.AddNodes(st);
  end else begin
    {not expecting any values}
    {warning, prehaps?}
  end;
end;

procedure tSearch.Cleanup;
  begin
  Query.Free;
  SetMsgTr(TrID, nil);
  UnShedule(@Step); {this is very important}
end;

procedure tSearch.LoadNodes;
  var bucket:tBucket_ptr;
  var i,n:integer;
  var ctrl:byte;
  begin
  ctrl:=0;
  bucket:=DHT.FindBucket(Target);
  while assigned(bucket) do begin
    for i:=1 to high(bucket^.peer) do begin
      n:=AddNode(bucket^.peer[i].ID,bucket^.peer[i].Addr);
      if n>cInitAdd then exit;
    end;
    bucket:=bucket^.next;
    if (bucket=nil) and (ctrl=0) then begin
      bucket:=GetDhtTable;
      ctrl:=1;
    end;
  end;
  {not enough nodes in best bucket, add whole dht,
  this is not fast operation optimize?}
end;

procedure RecvGetNodes(Msg:tSMsg);
  var bucket:tBucket_ptr;
  var i,ctrl:integer;
  var r:tMemoryStream;
  var trid:word;
  var sID:^tPID;
  var Target:^tKey20;
  begin
  msg.st.skip(1);
  msg.st.read(trid,2);
  sID:=msg.st.ReadPtr(20);
  if not DHT.CheckNode(sID^, msg.source, true) then exit;
  Target:=msg.st.ReadPtr(20);
  r.Init(999);
  r.WriteByte(opcode.dhtNodes);
  r.Write(trid,2);
  r.Write(dht.MyID,20);
  ctrl:=0;
  bucket:=DHT.FindBucket(Target^);
  while assigned(bucket) do begin
    if r.WRBufLen<44 then break;
    for i:=1 to high(bucket^.peer) do begin
      if r.WRBufLen<44 then break;
      r.Write(bucket^.peer[i].Addr,24);
      r.Write(bucket^.peer[i].ID,20);
    end;
    bucket:=bucket^.next;
    if (bucket=nil) and (ctrl=0) then begin
      bucket:=GetDhtTable;
      ctrl:=1;
    end;
  end;
  ServerLoop.SendMessage(r.base^,r.length,msg.source);
  r.Free;
end;

BEGIN
  ServerLoop.SetupOpcode(opcode.dhtGetNodes,@recvGetNodes);
END.
