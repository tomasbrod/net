unit dhtLookup;
INTERFACE
uses ServerLoop,dht,NetAddr;

type
  tPID=dht.tPID;
  tSearchPeer=object(dht.tPeerPub)
    LastReq:tMTime;
    reqc,
    rplc:byte;{1=replied with cap, 2=replied with peers}
    end;
  tSearch=object
    Target:tPID;
    Caps:byte;
    Extra:string[48];
    Peers:array [0..10] of tSearchPeer;
    Passive,Closed:boolean;
    Callback:procedure(const Source:tNetAddr; scaps:byte; exl:word; exp:pointer) of object;
    ObjectPointer:pointer;
    procedure Init;
    procedure Start;
    procedure Close;
    private
    next,prev:^tSearch;
    function AddPeer(const iID:tPID; const iAddr:tNetAddr; setrepl:boolean): pointer;
    procedure Step;
    procedure Periodic;
    end;
IMPLEMENTATION
uses MemStream,opcode,Store2;
var Searches:^tSearch;

procedure tSearch.Init;
  var i:integer;
  begin
  Caps:=0;
  Extra:='';
  Passive:=false;
  Closed:=false;
  Callback:=nil;
  for i:=high(peers) to 0 do Peers[i].Addr.Clear;
end;

procedure tSearch.Start;
  var list:pointer;
  var adc:byte;
  var ipeer:tPeerPub;
  begin
  {$hint todo check for identical search and mark as passive}
  next:=Searches;
  prev:=nil;
  if assigned(Searches) then Searches^.prev:=@self;
  Searches:=@self;
  GetFirstNode(list,target);
  adc:=0;
  writeln('dhtLookup.Start@',string(@self),' target=',string(target),' caps=',caps,' exl=',length(extra));
  while adc<6 do begin
    GetNextNode(list,ipeer);
    if ipeer.addr.isnil then break;
    inc(adc);
    self.AddPeer(ipeer.id,ipeer.addr,false);
  end;
  DoneGetNextNode(list);
  Shedule(800,@Periodic);
end;

function tSearch.AddPeer(const iID:tPID; const iAddr:tNetAddr; setrepl:boolean): pointer;
  var tpfl,idx,j:byte;
  begin
  idx:=0; result:=nil;
  tpfl:=PrefixLength(iid,Target);
  write('dhtLookup.AddPeer@',string(@self),' tpfl=',tpfl,' addr=',string(iaddr));
  for idx:=0 to high(peers) do begin
    write('[',string(peers[idx].addr),']');
    if peers[idx].addr.isNil then break;
    if peers[idx].addr=iaddr then begin
      if setrepl then Inc(peers[idx].rplc);
      result:=@peers[idx];
      writeln(' update ',idx);
    exit end;
    if PrefixLength(peers[idx].id,Target)<tpfl then break;
  end;
  if not setrepl then begin
    writeln(' insert ',idx);
    for j:=high(peers)-1 downto idx do peers[j+1]:=peers[j];
    peers[idx].id:=iid; peers[idx].addr:=iaddr;
    peers[idx].reqc:=0; peers[idx].rplc:=0;
    result:=@peers[idx];
    UnShedule(@Step);
    Shedule(1,@Step);
  end else writeln(' discard');
end;

procedure tSearch.Step;
  var ix:byte;
  var rqc,rpc,again:byte;
  var r:tMemoryStream;
  var buf:array [1..66] of byte;
  begin
  if closed then begin
    if assigned(prev) then prev^.next:=next
    else Searches:=next;
    if assigned(next) then next^.prev:=prev;
    UnShedule(@Periodic);
    FreeMem(@self,sizeof(self));
  exit end;
  write('dhtLookup.Step@',string(@self),': ');
  {send request to at most 3 peers,
   count nodes that replied
  }
  rqc:=0;rpc:=0; again:=0;
  repeat
  for ix:=0 to high(peers) do begin
    if peers[ix].addr.isNil then break;
    if (rqc>=3)or(rpc>=6) then break;
    if peers[ix].rplc>=2 then inc(rpc)
    else if (peers[ix].reqc<7)
         and(rqc<3)
    then begin
        inc(rqc);
        if (mNow-peers[ix].LastReq)<800 then continue;
        r.Init(@buf,0,sizeof(buf));
        r.WriteByte(opcode.dhtRequest);
        r.Write(dht.MyID,sizeof(tPID));
        r.Write(self.Target,sizeof(tPID));
        if peers[ix].rplc=0
        then  r.WriteByte(self.caps)
        else  r.WriteByte(0);
        r.Write(extra[1],length(extra));
        write('[',rqc,':',ix,':',string(peers[ix].addr),' ',peers[ix].rplc,'/',peers[ix].reqc,']');
        inc(peers[ix].reqc);
        peers[ix].LastReq:=MNow;
        ServerLoop.SendMessage(r.base^,r.length,peers[ix].Addr);
    end;
  end; inc(again);
  until (again>1)or(rqc>=3)or(rpc>=6);
  if (rqc)=0 then begin
    writeln('search failed'); Close;
    //search failed or exhausted
    {$warning notify of search failure}
  end else writeln;
end;

procedure tSearch.Periodic;
  begin
  Shedule(900,@Periodic);
  Step;
end;

procedure tSearch.Close;
  begin
  {$hint todo check for identical search and mark as active if active}
  Closed:=true;
end;

procedure UpdateSearch(const ID: tPID; const Addr:tNetAddr; rpc:boolean);
  var sr:^tSearch;
  var p:^tSearchPeer;
  //called by dhr.RecvPers with sender info
  begin
  sr:=Searches; while assigned(sr) do begin
    p:=sr^.AddPeer(ID,Addr,rpc);
    if assigned(p)and rpc then inc(p^.rplc); {mark it as exhausted}
    sr:=sr^.next;
  end;
end;

{c) op, SenderID, Target, caps, extra}
procedure RecvCapable(msg:tSMsg);
  unimplemented;
  var sr:^tSearch;
  var sID,sTarget:^tPID;
  var caps:byte;
  var exl:word;
  var exp:pointer;
  begin
  msg.stream.skip(1);
  sID:=msg.stream.readptr(sizeof(tPID));
  sTarget:=msg.stream.readptr(sizeof(tPID));
  caps:=msg.stream.readByte;
  exl:=msg.stream.RdBufLen;
  exp:=msg.stream.readptr(exl);
  sr:=Searches; while assigned(sr) do begin
    if (sr^.caps=caps)and(sr^.Target=sTarget^) then begin
      sr^.AddPeer(sID^,msg.Source^,true);
      writeln('dhtLookup.AddCapable@',string(sr),' ',string(msg.Source^),' caps=',caps,' exl=',exl);
      if assigned(sr^.callback) then sr^.Callback(msg.Source^,caps,exl,exp);
    end;
    sr:=sr^.next;
  end;
end;

BEGIN
  dht.OnNewPeer:=@UpdateSearch;
  ServerLoop.SetMsgHandler(opcode.dhtCapable,@recvCapable);
END.
