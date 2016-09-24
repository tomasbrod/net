unit DHT;
{
 implementation of custom dht, based on pastry and kademlia.
 keyspace is divided into buckets of limited capacity
 node belongs to bucket, where at least 'depth' bits match 'prefix'
 old>new,
 new>dead
 TODO: weight nodes by IP-Address common prefix length.
 IMPR: REQ send 3 IDs close to trg, rcpt has>0 respond else select
}

{used by: messages, fileshare}

INTERFACE
uses ServerLoop,ObjectModel,ECC,sha512;

TYPE
  tPID=tKey20;
  tPeer_ptr=^tPeer;
  tBucket_ptr=^tBucket;

tPeer=object
    ID   :tPID;
    Addr :tNetAddr;
    ReqDelta:word;
    LastMsgFrom,
    LastResFrom  :tMTime;
    Banned,Verified:boolean;
    Challenge:tEccKey;
    end;

tBucket=object
    Prefix: tPID;
    Depth:  byte;
    peer:   array [1..4] of tPeer;
    ModifyTime: tMTime;
    //ll: ^tll;
    desperate:word;
    next: ^tBucket;
    function MatchPrefix(const tp:tPID):boolean;
    procedure Refresh;
    function IDString:string;
    end;

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

type tPeerList=object
  bkt:^tBucket;
  ix:byte;
  p:^tPeer;
  bans:boolean;
  maxRD:word;
  procedure Init(const id:tPID);
  procedure Init; overload;
  procedure Next;
  private theb:boolean; {fuck identifier}
end DEPRECATED;

var MyID: tPID;

procedure NodeBootstrap(const contact:tNetAddr);
function CheckNode(const id: tPID; const addr: tNetAddr; recv:boolean): boolean;
function FindBucket(const prefix:tPID):tBucket_ptr; overload;
function GetDhtTable:tBucket_ptr;


IMPLEMENTATION
uses opcode,gitver,sysutils;

const
  crdRecvd=0;           {ReqDelta set on message reception}
  crdDoPingThr=1;       {ping often when rd>this}
  crdDontPingThr=4;     {ping less often whien rd>this}
  crdVerifyError=5;     {set on verify error}
  crdOthersCanReAddThr=6;{peer reinitialized when rd>this and suggested by other peer}
  crdInvalidateThr=4;   {request verify when rd>this}
  crdReplacableThr=2;   {new peer can replace old when rd>this}
  cBanDuration=10*60*1000;
  cStichRar=7;
  cNudgeQuietThr=12*1000;
  cRefreshWaitBase=18*1000;
  cRefreshWaitMul=600;
  cRefreshWaitOther=30*1000;
  cRefreshWaitRtrDiv=3;
  cNodesDat='nodes.dat';
  cBootTxt='bootstrap.txt';
  cMaxNodesDat=12;
  cInitAdd=6;           {n of peers to add from dht}
  cInitWait=800;        {? Init to Step delay}
  cAddWait=1;           {new peers to Step delay}
  cStepRqc=3;           {max requests per Step}
  cStepMinDelay=800;    {min delta of requests to same peer}
  cStepPeerReqc=6;      {max (unsuccessful) requests to peer}
  cStepRplc=6;          {? something per Step}
  cStepPeriod=900;      {max period between Steps}

var Table:^tBucket;
{deepest first}

function tBucket.MatchPrefix(const tp:tPID):boolean;
 begin
 result:=(depth=0)or(PrefixLength(prefix,tp)>=depth);
end;

function GetDhtTable:tBucket_ptr;
  begin
  GetDhtTable:=Table;
end;

procedure VerifyInit(b:tBucket_ptr; i:byte); forward;

function tBucket.IDString:string;
  var l:byte;
  begin
  l:=depth div 8;
  if (depth mod 8)>0 then inc(l);
  SetLength(result,l*2);
  if l>0 then BinToHex(@result[1],prefix,l);
  result:=result+'/'+IntToStr(depth);
end;

function FindBucket(const prefix:tPID):tBucket_ptr; overload;
 var cur:^tBucket;
 begin
 cur:=Table;
 result:=nil;
 while (cur<>nil) and (result=nil) do begin
  if cur^.MatchPrefix(prefix) {first matching is deepest}
   then result:=cur;
  cur:=cur^.next;
 end;
 assert(assigned(result) xor (Table=nil));
end;

(*** Mighty Split and Check procedures ***)

procedure SplitBucket(ob:tBucket_ptr);
 procedure Toggle(var prefix:tPID; bit:byte);
  begin
  prefix[bit div 8]:= prefix[bit div 8] xor ($80 shr (bit mod 8));
 end;
 var nb:tBucket_ptr;
 var i:byte;
 begin
 writeln('DHT: SplitBucket ',string(ob^.prefix),'/',ob^.depth);
 {find pref to old bucket, in order to unlink}
 if ob=Table then table:=table^.next else begin
 nb:=Table;
 while assigned(nb) and (nb^.next<>ob) do nb:=nb^.next;
 assert(assigned(nb),'old bucket not in table');
 {unlink}
 nb^.next:=nb^.next^.next; nb:=nil;
 end;
 {increase depth of this bucket}
 Inc(ob^.depth);
 ob^.ModifyTime:=mNow;
 {create new bucket with toggled bit}
 New(nb);
 nb^:=ob^;
 Toggle(nb^.Prefix,nb^.depth-1);
 nb^.next:=ob;
 {clear nodes that do not belong in bucket}
 for i:=1 to high(tBucket.peer) do begin
  if ob^.peer[i].addr.isNil then continue;
  if ob^.MatchPrefix(ob^.peer[i].id)
   then nb^.peer[i].addr.clear
   else ob^.peer[i].addr.clear;
 end;
 writeln('-> ',string(ob^.prefix),'/',ob^.depth);
 for i:=1 to high(tBucket.peer) do if not ob^.peer[i].addr.isnil
  then writeln('-> -> ',string(ob^.peer[i].id));
 writeln('-> ',string(nb^.prefix),'/',nb^.depth);
 for i:=1 to high(tBucket.peer) do if not nb^.peer[i].addr.isnil
  then writeln('-> -> ',string(nb^.peer[i].id));
 if table=nil then table:=nb else begin
  ob:=Table;
  while assigned(ob^.next)and (ob^.next^.depth<=nb^.depth) do ob:=ob^.next;
  ob^.next:=nb;
  writeln('-> after /',ob^.depth);
 end;
 Shedule(2000,@nb^.Refresh);
end;

function CheckNode(const id: tPID; const addr: tNetAddr; recv:boolean): boolean;
 {return false if node is banned}
  var b:^tBucket;
  var i,ifree,idup,iold:byte;
  var adm,idm:boolean;
  label again;
  begin
  Assert(not addr.isNil,'CheckNode with nil address');
  if id=MyID then exit;
  CheckNode:=false;
  again:
  b:=FindBucket(id);
  if not assigned(b) then begin
    New(Table); b:=Table;
    b^.Prefix:=MyID;
    b^.Depth:=0;
    b^.ModifyTime:=mNow;
    b^.next:=nil;
    b^.desperate:=3;
    for i:=1 to high(b^.peer) do b^.peer[i].addr.Clear;
    for i:=1 to high(b^.peer) do b^.peer[i].banned:=false;
    Shedule(2000,@b^.Refresh);
  end;
  {order: update, free, banned, split, bad}
  ifree:=0;idup:=0;iold:=0;
  for i:=1 to high(b^.peer) do begin
    adm:=(b^.peer[i].Addr=addr);
    idm:=(b^.peer[i].ID=ID);
    if adm and (b^.peer[i].Banned) then exit;
    if (ifree=0)and((b^.peer[i].Addr.isNil)or(b^.peer[i].Banned))
      then ifree:=i;
    if adm or idm then begin
      idup:=i;break;end;
    if (ifree=0)and(iold=0)and (b^.peer[i].ReqDelta>crdReplacableThr)
      then iold:=i;
  end;
  if (idup>0) and (recv) and (b^.peer[i].ReqDelta>crdReplacableThr) then begin
    ifree:=idup; idup:=0 end;
  if (idup>0) and (not recv) and (b^.peer[i].ReqDelta>crdOthersCanReAddThr) then begin
    ifree:=idup; idup:=0 end; {not iold, iold causes splits}
  if idup>0 then begin
    {updating}
    if adm and idm then begin
      CheckNode:=true;
      if recv and b^.peer[idup].Verified then begin
        {only update by self and verified, else waiting for auth}
        b^.peer[idup].LastMsgFrom:=mNow;
        b^.peer[idup].ReqDelta:=0;
      end else {dont refresh by others};
    end else begin
        (*{$note don't start CRa too often}
        VerifyInit(b,idup);*)
    end;
  end else begin
    {inserting}
    if (ifree=0) and b^.MatchPrefix(MyID) then begin
      SplitBucket(b);
      goto again;
    end;
    if ifree>0 then i:=ifree
    else if iold>0 then i:=iold
    else exit;
    {add node here}
    writeln('DHT: AddNode ',string(id),string(addr),' to ',b^.IDString,'#',i);
    b^.ModifyTime:=mNow;
    b^.peer[i].ID:=ID;
    b^.peer[i].Addr:=Addr;
    b^.peer[i].LastMsgFrom:=mNow;
    b^.peer[i].LastResFrom:=0;
    b^.peer[I].ReqDelta:=0;
    b^.peer[I].banned:=false;
    b^.peer[I].Verified:=false;
    VerifyInit(b,i);
    CheckNode:=true;
  end
end;

(*** PeerList object ***)

procedure tPeerList.Init(const id:tPID);
 begin
 bans:=false; maxRD:=2;
 bkt:=FindBucket(id); ix:=0; theb:=true;
 p:=nil;
end;

procedure tPeerList.Init;
 begin
 bans:=false; maxRD:=2;
 bkt:=Table; ix:=0; theb:=false;
 p:=nil;
end;

procedure tPeerList.Next;
 begin
 repeat
  if not assigned(bkt) then break;
  inc(ix); {next peer}
  if ix>high(tBucket.peer) then {bucket exhausted} begin
   if theb then begin
    theb:=false;
    bkt:=Table;
   end
   else bkt:=bkt^.next;
   ix:=1;
   if not assigned(bkt) then break;
  end;
  {FIXME: list returns nodes from The bucket second time}
 until (not bkt^.peer[ix].Addr.isNil)
       and(bkt^.peer[ix].ReqDelta<=maxrd)
       and(bans or(bkt^.peer[ix].banned=false));
 if assigned(bkt) then p:=@bkt^.peer[ix] else p:=nil;
end;


(*** Protocol Communication ***)

procedure RecvBeatQ(msg:tSMsg);
  var s:tMemoryStream absolute msg.st;
  var sID:^tPID;
  var Target:^tPID;
  var mark:word;
  var r:tMemoryStream;
  var list:tPeerList;
  begin
  s.skip(1);
  sID:=s.ReadPtr(20);
  Target:=s.ReadPtr(20);
  s.Read(mark,2);
  //writeln('DHT.BeatQ: ',string(msg.source),' Request for ',string(Target^));
  if not CheckNode(sID^,msg.source,true) then exit;
  list.Init(Target^);
  r.Init(199);
  r.WriteByte(opcode.dhtBeatR);
  r.Write(MyID,20);
  r.Write(mark,2);
  while r.WrBufLen>=44 do begin
    list.Next;
    if not assigned(list.bkt) then break; {simply no more peers}
    if list.p^.addr=msg.source then continue;
    r.Write(list.p^.Addr,24);
    r.Write(list.p^.ID,20);
  end;
  SendMessage(r.base^,r.length,msg.source);
  r.Free;
end;

procedure SendBeat(const contact:tNetAddr; const forid: tPID; mark:word);
 var r:tMemoryStream;
 begin
 //writeln('DHT.SendBeat: to ',string(contact),' for ',string(forid));
 r.Init(44);
 r.WriteByte(opcode.dhtBeatQ);
 r.Write(MyID,sizeof(tPID));
 r.Write(ForID,sizeof(tPID));
 r.Write(mark,2);
 SendMessage(r.base^,r.length,contact);
 r.Free;
end;

procedure RecvBeatR(msg:tSMsg);
  var ID:^tPID;
  var Addr:^tNetAddr;
  begin
  msg.st.skip(1);
  ID:=msg.st.ReadPtr(20);
  msg.st.skip(2); //todo
  //writeln('DHT.BeatR: ',string(msg.source),' is ',string(ID^));
  if not CheckNode(ID^,msg.source,true) then exit;
  while msg.st.RdBufLen>44 do begin
    Addr:=msg.st.ReadPtr(24);
    ID:=msg.st.ReadPtr(20);
    CheckNode(ID^,Addr^,false);
  end;
end;

{Messages: (Still valid?)
 d)VfyCh: op, SendPub, PoWork, Challenge, Ver
 e)VfyRe: op, SendPub, PoWork, Respoonse, Ver
           1,      32,     36,        32, =101    +35    
}

procedure SendCheck(var p:tPeer);
  var r:tMemoryStream;
  begin
  r.Init(999);
  r.WriteByte(opcode.dhtCheckQ);
  r.Write(ECC.PublicKey,sizeof(ECC.PublicKey));
  r.Write(ECC.PublicPoW,sizeof(ECC.PublicPoW));
  r.Write(p.Challenge,sizeof(tEccKey));
  r.Write(GIT_VERSION[1],Length(GIT_VERSION));
  writeln('DHT.SendCheck: to ',string(p.addr),' ',r.length,'B');
  SendMessage(r.base^,r.length,p.Addr);
  r.Free;
end;

procedure RecvCheckQ(msg:tSMsg);
  var id:tPID;
  var pub:^tEccKey;
  var pow:^tPoWRec;
  var challenge:^tEccKey;
  var right_resp:tEccKey;
  var r:tMemoryStream;
  begin
  msg.st.skip(1);
  pub:=msg.st.ReadPtr(sizeof(tEccKey));
  pow:=msg.st.ReadPtr(sizeof(tPoWRec));
  challenge:=msg.st.ReadPtr(sizeof(tEccKey));
  {Pub->ID}
  Sha512Buffer(Pub^,sizeof(pub^),id,sizeof(id));
  {CheckNode}
  if not CheckNode(id,msg.source,true) then exit;
  {Verify PoW}
  if not ECC.VerifyPoW(pow^,pub^) then begin
    writeln('DHT.CheckQ: Invalid PoW in request from ',string(msg.source));
  exit end;
  {Solve C/R}
  ECC.CreateResponse(Challenge^, right_resp, pub^);
  {reply}
  r.Init(999);
  r.WriteByte(opcode.dhtCheckR);
  r.Write(ECC.PublicKey,sizeof(ECC.PublicKey));
  r.Write(ECC.PublicPoW,sizeof(ECC.PublicPoW));
  r.Write(right_resp,sizeof(right_resp));
  r.Write(GIT_VERSION[1],Length(GIT_VERSION));
  writeln('DHT.CheckQ: responding to ',string(msg.source),' ',r.length,'B');
  SendMessage(r.base^,r.length,msg.source);
  r.Free;
end;

procedure RecvCheckR(msg:tSMsg);
  var b:^tBucket;
  var i:byte;
  var id:tPID;
  var pub:^tEccKey;
  var pow:^tPoWRec;
  var resp:^tEccKey;
  var right_resp:tEccKey;
  begin
  msg.st.skip(1);
  pub:=msg.st.ReadPtr(sizeof(tEccKey));
  pow:=msg.st.ReadPtr(sizeof(tPoWRec));
  resp:=msg.st.ReadPtr(sizeof(tEccKey));
  {Pub->ID}
  Sha512Buffer(Pub^,sizeof(pub^),id,sizeof(id));
  {ID->bkt:idx}
  b:=FindBucket(id);
  if not assigned(b) then exit;
  i:=1; while b^.peer[i].ID<>ID do begin
    inc(i); if i>high(b^.peer) then exit;
  end;
  {drop banned n unknown}
  if b^.peer[i].Banned then exit;
  b^.peer[i].LastMsgFrom:=mNow;
  {Verify PoW}
  if not ECC.VerifyPoW(pow^,pub^) then begin
    b^.peer[i].Banned:=true;
  writeln('DHT.CheckR: Invalid PoW in response from ',string(msg.source));
  exit end;
  {Verify C/R}
  ECC.CreateResponse(b^.peer[i].Challenge, right_resp, pub^);
  if CompareByte(resp^,right_resp,sizeof(right_resp))<>0 then begin
    b^.peer[i].Banned:=true;
  exit end;
  {set node verified, rqd, last}
  b^.peer[i].Verified:=true;
  b^.peer[i].ReqDelta:=0;
  writeln('DHT.CheckR: Valid response from ',string(msg.source));
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
  writeln('DHT.RecvGetNodes: from ',string(msg.source));
  r.Init(999);//TODO
  r.WriteByte(opcode.dhtNodes);
  r.Write(trid,2);
  r.Write(dht.MyID,20);
  ctrl:=0;
  bucket:=DHT.FindBucket(Target^);
  while assigned(bucket) do begin
    if r.WRBufLen<44 then break;
    for i:=1 to high(bucket^.peer) do begin
      if r.WRBufLen<44 then break;
      if bucket^.peer[i].Addr.isNil then break;
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

procedure NodeBootstrap(const contact:tNetAddr);
 begin
 SendBeat(contact,MyID,0);
 //SendBeat(contact,MyID,0); {xD}
end;

(*** The bucket Refresh procedure ***)

procedure tBucket.Refresh;
 var my,rtr,stich:boolean;
 var i,ol:byte;
 var wait:LongWord;
 var list:tPeerList;
 var debug:ansistring;
 procedure lSend(var peer:tPeer; const trg:tPID);
  begin
  if peer.Verified 
  then SendBeat(peer.addr,trg,0)
  else SendCheck(peer);
  Inc(peer.ReqDelta);
 end;
 begin
 my:=MatchPrefix(MyID);
 ol:=0; rtr:=false;
 {1 of 10 times try to contact dead nodes in attempt to recover from network split}
 debug:='DHT.Refresh('+self.IDString+')';
 stich:=Random(cStichRar)=0;
 for i:=1 to high(tBucket.peer)
  do if (not peer[i].Addr.isNil) and (not peer[i].Banned) then begin
   if peer[i].ReqDelta>=crdDoPingThr then begin
    if (peer[i].ReqDelta<=crdDontPingThr) xor stich then begin
     {this will get rid of half-dead nodes}
     writeln(debug,' R',peer[i].ReqDelta,' ',copy(string(peer[i].id),1,6),string(peer[i].addr));
     lSend(peer[i],prefix);{todo: use random target with prefix}
     rtr:=true;
    end
   end
   else if (ol=0) or (peer[i].LastMsgFrom<peer[ol].LastMsgFrom)
        then ol:=i;
 end;
 {now nudge the most quiet peer, but not too often}
 if (ol>0) and ((mNow-peer[ol].LastMsgFrom)>cNudgeQuietThr) then begin
  writeln(debug,' T',mNow-peer[ol].LastMsgFrom,' ',string(peer[ol].addr));
  lSend(peer[ol],MyID);
 end;
 {try to recover bucket full of bad nodes}
 if (ol=0){and(not rtr)} then begin
  list.Init(Prefix);
  list.bans:=true;
  list.maxRD:=desperate; list.Next;
  if assigned(list.bkt) then begin
   writeln(debug,' V ',copy(string(list.p^.id),1,6),string(list.p^.addr));
   lSend(list.p^,prefix);
  end else inc(desperate);
 end else desperate:=3;
 if my
  then wait:=cRefreshWaitBase+(depth*cRefreshWaitMul)
  else wait:=cRefreshWaitOther;
 if rtr and(not stich) then wait:=wait div cRefreshWaitRtrDiv;
 Shedule(wait,@Refresh);
end;
 
procedure VerifyInit(b:tBucket_ptr; i:byte);
  begin with b^.peer[i] do begin
    Verified:=false;
    ECC.CreateChallenge(challenge);
    SendCheck(b^.peer[i]);
end end;

(*** The Search Object ***)

constructor tSearch.Init;
  var i:integer;
  begin
  for i:=high(nodes) downto 0 do Nodes[i].Addr.Clear;
  NewMsgTr(TrID, @IntHandleReply);
  tTask.Init;
  writeln('Lookup@',string(@self),': Initialize');
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
  assert(not iAddr.isNil,'AddNode with nil address');
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
  writeln('Lookup@',string(@self),': Add [',idx,'] ',string(iAddr));
  for j:=high(nodes)-1 downto idx do nodes[j+1]:=nodes[j];
  nodes[idx].id:=iid; nodes[idx].addr:=iaddr;
  nodes[idx].reqc:=0; nodes[idx].rplc:=0;
  result:=idx;
  {if assigned(OnProgress) then OnProgress(tpfl,nodes[idx]);}
end;

procedure tSearch.Step;
  var ix:byte;
  var r:tMemoryStream;
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
        if nodes[ix].rplc=0 then begin
          ServerLoop.SendMessage(Query.base^,Query.length,nodes[ix].Addr);
        end else begin
          r.Init(43);
          r.WriteByte(opcode.dhtGetNodes);
          r.Write(self.TrID,2);
          r.Write(DHT.MyID,20);
          r.Write(Target,20);
          ServerLoop.SendMessage(R.base^,R.length,nodes[ix].Addr);
          r.Free;
        end;
        writeln('Lookup@',string(@self),': [',ix,',',rqc,',',nodes[ix].reqc,'] ',string(nodes[ix].addr));
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
  writeln('dhtLookup.AddNodes stream offset ',s.tell);
  while s.left>=44 do begin
    ad:=s.readptr(24);
    id:=s.readptr(20);
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
    writeln('Lookup@',string(@self),': Nodes from ',string(sAddr));
    if node<>-1 then nodes[node].rplc:=2;
    self.AddNodes(st);
  end else begin
    writeln('Lookup@',string(@self),': Unknown from ',string(sAddr));
    {not expecting any values}
    {warning, prehaps?}
  end;
end;

procedure tSearch.Cleanup;
  begin
  writeln('Lookup@',string(@self),': Cleanup');
  Query.Free;
  SetMsgTr(TrID, nil);
  UnShedule(@Step); {this is very important}
end;

procedure tSearch.LoadNodes;
  var bucket:tBucket_ptr;
  var i,n:integer;
  var ctrl:byte;
  begin
  writeln('Lookup@',string(@self),': Load Nodes');
  ctrl:=0;
  bucket:=DHT.FindBucket(Target);
  while assigned(bucket) do begin
    for i:=1 to high(bucket^.peer) do begin
      if bucket^.peer[i].Addr.isNil then break;
      n:=AddNode(bucket^.peer[i].ID,bucket^.peer[i].Addr);
      if n>cInitAdd then exit;
    end;
    bucket:=bucket^.next;
    if (bucket=nil) and (ctrl=0) then begin
      {not enough nodes in best bucket, add whole dht,
      this is not fast operation optimize?}
      bucket:=GetDhtTable;
      ctrl:=1;
    end;
  end;
end;

(*** Persistent data Manager ***)

type tPersist=object
  statef:tFileStream;
  bootf:TextFile;
  readcnt:longword;
  procedure OpenState;
  procedure ReadState;
  procedure OpenBS;
  procedure ReadBS;
  procedure SaveState;
  end;
var Persist:tPersist;

procedure tPersist.OpenState;
  begin
  readcnt:=0;
  try
    statef.OpenRO(cNodesDat);
    shedule(1,@ReadState);
  except
    on e:eInOutError do begin
      writeln('DHT.Boot: Cannot open '+cNodesDat+' ',e.Message);
      shedule(1,@OpenBS);
  end end;
end;
procedure tPersist.ReadState;
  var addr:tNetAddr;
  begin
  try
    if statef.Left=0 then raise eReadPastEoF.Create('eof'); //temporary fix
    statef.Read(addr,sizeof(addr));
    //writeln('DHT.ReadState: ',string(addr));
    NodeBootstrap(addr);
    shedule(200,@ReadState);
    inc(readcnt);
  except on e: eReadPastEoF do begin
    writeln('DHT.Boot: Pinged ',readcnt,' nodes from '+cNodesDat);
    statef.Done;
    shedule(1,@OpenBS);
  end end;
end;
procedure tPersist.OpenBS;
  begin
  readcnt:=0;
  try
    Assign(bootf,cBootTxt);
    Reset(bootf);
    shedule(1,@ReadBS);
  except
    on e:eInOutError do begin
      writeln('DHT.Boot: Cannot open '+cBootTxt+' ',e.Message);
      shedule(5000,@SaveState);
  end end;
end;
procedure tPersist.ReadBS;
  var line:string;
  var addr:tNetAddr;
  begin
  if eof(bootf) then begin
    close(bootf);
    shedule(5000,@SaveState);
    writeln('DHT.Boot: Pinged ',readcnt,' nodes from '+cBootTxt);
  end else begin
    readln(bootf,line);
    addr.FromString(line);
    //writeln('DHT.ReadBoot: ',string(addr));
    NodeBootstrap(addr);
    inc(readcnt);
    shedule(300,@ReadBS);
  end;
end;
procedure tPersist.SaveState;
  var bkt:^tBucket;
  var p:integer;
  var cntr:word;
  begin
  //writeln('DHT.SaveState');
  statef.OpenRW(cNodesDat);
  FileTruncate(statef.handle,0);
  bkt:=Table;
  cntr:=0;
  while assigned(bkt) and (cntr<cMaxNodesDat) do begin
    for p:=1 to 4 do begin
      if bkt^.peer[p].Addr.IsNil then continue;
      statef.Write(bkt^.peer[p].Addr,sizeof(tNetAddr));
      inc(cntr);
    end;
    bkt:=bkt^.next;
  end;
  statef.Done;
  shedule(61273,@SaveState);
end;

BEGIN
  assert((sizeof(tNetAddr)+sizeof(tPID))=44);
  SetupOpcode(opcode.dhtBeatQ,@recvBeatQ);
  SetupOpcode(opcode.dhtBeatR,@recvBeatR);
  SetupOpcode(opcode.dhtCheckQ,@recvCheckQ);
  SetupOpcode(opcode.dhtCheckR,@recvCheckR);
  SetupOpcode(opcode.dhtGetNodes,@recvGetNodes);
  Sha512Buffer(PublicKey,sizeof(PublicKey),MyID,sizeof(MyID));
  writeln('DHT: set ID to ',string(MyID),' from ECC');
  Persist.OpenState;
END.
