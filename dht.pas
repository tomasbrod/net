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
uses ServerLoop,NetAddr,Store2,MemStream;
type
  tPID=Store2.tFID; {reQ: ids can be shorter}
  tPeerPub=object
    ID   :tPID;
    Addr :tNetAddr;
    end;
  tCapHandler=function(const source:tNetAddr; caps:byte; const Target:tPID; var extra:tMemoryStream):boolean;

var MyID: tPID;
var OnNewPeer: procedure(const ID: tPID; const Addr:tNetAddr; rpc:boolean)=nil;
procedure NodeBootstrap(const contact:tNetAddr);
procedure GetFirstNode(var ptr:pointer; const Target:tPID);
procedure GetNextNode(var ptr:pointer; out peer:tPeerPub);
procedure DoneGetNextNode(var ptr:pointer);
procedure InsertNode(const peer:tPeerPub);
procedure RegisterCapability(cap:byte; handler:tCapHandler);

IMPLEMENTATION
uses Chat,opcode,ECC,sha512;

type
 tPeer=object(tPeerPub)
   ReqDelta:word;
   LastMsgFrom,
   LastResFrom  :tMTime;
   Banned,Verified:boolean;
   Challenge:tEccKey;
 end;
 tPeer_ptr=^tPeer;
 tBucket_ptr=^tBucket;
 tBucket=object
   Prefix: tPID;
   Depth:  byte;
   peer:   array [1..4] of tPeer;
   ModifyTime: tMTime;
   //ll: ^tll;
   desperate:word;
   next: ^tBucket;
   function MatchPrefix(const tp:tFID):boolean;
   procedure Refresh;
 end;

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

var Table:^tBucket;
var CapHandler: array [1..32] of tCapHandler;
{deepest first}

function tBucket.MatchPrefix(const tp:tFID):boolean;
 begin
 result:=(depth=0)or(PrefixLength(prefix,tp)>=depth);
end;

function FindBucket(const prefix:tFID):tBucket_ptr; overload;
 var cur:^tBucket;
 begin
 cur:=Table;
 result:=nil;
 while (cur<>nil) and (result=nil) do begin
  if cur^.MatchPrefix(prefix) {first matching is deepest}
   then result:=cur;
  cur:=cur^.next;
 end;
end;

operator =(const a,b:tFID):boolean;
 begin
 result:=CompareWord(a,b,10)=0;
end;

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
  while assigned(ob^.next)and (ob^.next^.depth>nb^.depth) do ob:=ob^.next;
  ob^.next:=nb;
  writeln('-> after /',ob^.depth);
 end;
 Shedule(2000,@nb^.Refresh);
end;

procedure VerifyInit(b:tBucket_ptr; i:byte); forward;

function CheckNode(const id: tPID; const addr: tNetAddr; recv:boolean): boolean;
 {return false if node is banned}
  var b:^tBucket;
  var i,ifree,idup,iold:byte;
  var adm,idm:boolean;
  label again;
  begin
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
    writeln('DHT: AddNode ',string(id),string(addr),' to ',string(b^.prefix),'/',b^.depth,'#',i);
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

procedure InsertNode(const peer:tPeerPub);
 begin
 CheckNode(peer.id,peer.addr,true);
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
end;

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

procedure GetNextNode(var ptr:pointer; out peer:tPeerPub);
 begin
 if ptr=nil then begin ptr:=GetMem(sizeof(tPeerList)); tPeerList(ptr^).Init end;
 with tPeerList(ptr^) do begin
  Next; if assigned(bkt)  then peer:=bkt^.peer[ix]
  else peer.addr.clear end end;
procedure GetFirstNode(var ptr:pointer; const Target:tPID);
begin ptr:=GetMem(sizeof(tPeerList)); tPeerList(ptr^).Init(target); end;
procedure DoneGetNextNode(var ptr:pointer);
begin FreeMem(ptr,sizeof(tPeerList)); ptr:=nil; end;

{Messages:
 a)Request: op, SendID, TargetID, caps, adt
 b)Peers  : op, SendID, [addr, ID]
}

procedure RecvRequest(msg:tSMsg);
  var s:tMemoryStream absolute msg.stream;
  var sID:^tPID;
  var Target:^tPID;
  var caps:byte;
  var r:tMemoryStream;
  var list:tPeerList;
  begin
  s.skip(1);
  sID:=s.ReadPtr(20);
  Target:=s.ReadPtr(20);
  caps:=s.ReadByte;
  //writeln('DHT: ',string(msg.source^),' Request for ',string(Target^));
  if not CheckNode(sID^,msg.source^,true) then exit;
  if (caps>0)and(caps<=high(CapHandler))and assigned(CapHandler[caps])
  then if CapHandler[caps](msg.source^,caps,Target^,s) then exit;
  list.Init(Target^);
  {TODO: sometimes it is better to send answer directly}
  r.Init(197);
  r.WriteByte(opcode.dhtPeers);
  r.Write(MyID,20);
  while r.WrBufLen>=44 do begin
    list.Next;
    if not assigned(list.bkt) then break; {simply no more peers}
    //writeln('-> Select to ',string(list.p^.addr));
    if list.p^.addr=msg.source^ then continue;
    r.Write(list.p^.Addr,24);
    r.Write(list.p^.ID,20);
  end;
  SendMessage(r.base^,r.length,msg.source^);
  FreeMem(r.base,r.size);
end;

procedure SendRequest(const contact:tNetAddr; const forid: tPID; caps:byte);
 var r:tMemoryStream;
 begin
 //writeln('DHT: Request to ',string(contact),' for ',string(forid),' caps=',caps);
 r.Init(42);
 r.WriteByte(opcode.dhtRequest);
 r.Write(MyID,sizeof(tFID));
 r.Write(ForID,sizeof(tFID));
 r.WriteByte(caps);
 SendMessage(r.base^,r.length,contact);
 FreeMem(r.base,r.size);
end;

procedure RecvPeers(msg:tSMsg);
  var s:tMemoryStream absolute msg.stream;
  var ID:^tPID;
  var Addr:^tNetAddr;
  begin
  s.skip(1);
  ID:=s.ReadPtr(20);
  //writeln('DHT: ',string(msg.source^),' is ',string(ID^),' Peers');
  if not CheckNode(ID^,msg.source^,true) then exit;
  if assigned(OnNewPeer) then OnNewPeer(ID^,msg.source^,true);
  while s.RdBufLen>44 do begin
    Addr:=s.ReadPtr(24);
    ID:=s.ReadPtr(20);
    CheckNode(ID^,Addr^,false);
    if assigned(OnNewPeer) then OnNewPeer(ID^,Addr^,false);
  end;
end;

{Messages:
 d)VfyCh: op, SendPub, PoWork, Challenge, Ver
 e)VfyRe: op, SendPub, PoWork, Respoonse, Ver
           1,      32,     36,        32, =101    +35    
}
procedure SendVfyCh(var p:tPeer);
  var r:tMemoryStream;
  begin
  r.Init(999);
  r.WriteByte(opcode.dhtVfyCh);
  r.Write(ECC.PublicKey,sizeof(ECC.PublicKey));
  r.Write(ECC.PublicPoW,sizeof(ECC.PublicPoW));
  r.Write(p.Challenge,sizeof(tEccKey));
  r.Write(ServerLoop.VersionString[1],Length(ServerLoop.VersionString));
  //writeln('DHT.CR: Send request to ',string(p.addr),' ',r.length,'B');
  SendMessage(r.base^,r.length,p.Addr);
  r.Free;
end;

procedure RecvVfyCh(msg:tSMsg);
  var id:tPID;
  var pub:^tEccKey;
  var pow:^tPoWRec;
  var challenge:^tEccKey;
  var right_resp:tEccKey;
  var r:tMemoryStream;
  begin
  msg.stream.skip(1);
  pub:=msg.stream.ReadPtr(sizeof(tEccKey));
  pow:=msg.stream.ReadPtr(sizeof(tPoWRec));
  challenge:=msg.stream.ReadPtr(sizeof(tEccKey));
  {Pub->ID}
  Sha512Buffer(Pub^,sizeof(pub^),id,sizeof(id));
  {CheckNode}
  if not CheckNode(id,msg.source^,true) then exit;
  {Verify PoW}
  if not ECC.VerifyPoW(pow^,pub^) then begin
    writeln('DHT.CR: Invalid PoW in request from ',string(msg.source^));
  exit end;
  {Solve C/R}
  ECC.CreateResponse(Challenge^, right_resp, pub^);
  {reply}
  r.Init(999);
  r.WriteByte(opcode.dhtVfyRe);
  r.Write(ECC.PublicKey,sizeof(ECC.PublicKey));
  r.Write(ECC.PublicPoW,sizeof(ECC.PublicPoW));
  r.Write(right_resp,sizeof(right_resp));
  r.Write(ServerLoop.VersionString[1],Length(ServerLoop.VersionString));
  //writeln('DHT.CR: Send response to ',string(msg.source^),' ',r.length,'B');
  SendMessage(r.base^,r.length,msg.source^);
  r.Free;
end;

procedure RecvVfyRe(msg:tSMsg);
  var b:^tBucket;
  var i:byte;
  var id:tPID;
  var pub:^tEccKey;
  var pow:^tPoWRec;
  var resp:^tEccKey;
  var right_resp:tEccKey;
  begin
  msg.stream.skip(1);
  pub:=msg.stream.ReadPtr(sizeof(tEccKey));
  pow:=msg.stream.ReadPtr(sizeof(tPoWRec));
  resp:=msg.stream.ReadPtr(sizeof(tEccKey));
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
  writeln('DHT.CR: Invalid PoW in reqest from ',string(msg.source^));
  exit end;
  {Verify C/R}
  ECC.CreateResponse(b^.peer[i].Challenge, right_resp, pub^);
  if CompareByte(resp^,right_resp,sizeof(right_resp))<>0 then begin
    b^.peer[i].Banned:=true;
  exit end;
  {set node verified, rqd, last}
  b^.peer[i].Verified:=true;
  b^.peer[i].ReqDelta:=0;
  writeln('DHT.CR: Valid response from ',string(msg.source^));
end;


procedure NodeBootstrap(const contact:tNetAddr);
 begin
 SendRequest(contact,MyID,0);
 SendRequest(contact,MyID,0); {xD}
end;

procedure tBucket.Refresh;
 var my,rtr,stich:boolean;
 var i,ol:byte;
 var wait:LongWord;
 var list:tPeerList;
 procedure lSend(var peer:tPeer; const trg:tPID);
  begin
  if peer.Verified 
  then SendRequest(peer.Addr,trg,0)
  else SendVfyCh(peer);
  Inc(peer.ReqDelta);
 end;
 begin
 my:=MatchPrefix(MyID);
 ol:=0; rtr:=false;
 {1 of 10 times try to contact dead nodes in attempt to recover from network split}
 stich:=Random(cStichRar)=0;
 for i:=1 to high(tBucket.peer)
  do if (not peer[i].Addr.isNil) and (not peer[i].Banned) then begin
   if peer[i].ReqDelta>=crdDoPingThr then begin
    if (peer[i].ReqDelta<=crdDontPingThr) xor stich then begin
     {this will get rid of half-dead nodes}
     writeln('DHT: Refresh (R',peer[i].ReqDelta,') ',copy(string(peer[i].id),1,6),string(peer[i].addr));
     lSend(peer[i],prefix);
     rtr:=true;
    end
   end
   else if (ol=0) or (peer[i].LastMsgFrom<peer[ol].LastMsgFrom)
        then ol:=i;
 end;
 {now nudge the most quiet peer, but not too often}
 if (ol>0) and ((mNow-peer[ol].LastMsgFrom)>cNudgeQuietThr) then begin
  //writeln('DHT: Refresh (T',mNow-peer[ol].LastMsgFrom,') #',ol,' ',string(peer[ol].addr));
  lSend(peer[ol],MyID);
 end;
 {try to recover bucket full of bad nodes}
 if (ol=0){and(not rtr)} then begin
  list.Init(Prefix);
  list.bans:=true;
  list.maxRD:=desperate; list.Next;
  if assigned(list.bkt) then begin
   writeln('DHT: Recover ',string(prefix),'/',depth,' try ',copy(string(list.p^.id),1,6),string(list.p^.addr));
   lSend(list.p^,prefix);
  end else inc(desperate);
 end else desperate:=3;
 if my
  then wait:=cRefreshWaitBase+(depth*cRefreshWaitMul)
  else wait:=cRefreshWaitOther;
 if rtr and(not stich) then wait:=wait div cRefreshWaitRtrDiv;
 Shedule(wait,@Refresh);
end;

{to bootstrap: ping address to get ID and insert to bucket/il
ping may get lost: separate bootstrap unit :)
now jut Ass-U-Me wont get lost}
 
procedure VerifyInit(b:tBucket_ptr; i:byte);
  unimplemented;
  begin with b^.peer[i] do begin
    Verified:=false;
    ECC.CreateChallenge(challenge);
    SendVfyCh(b^.peer[i]);
end end;

procedure RegisterCapability(cap:byte; handler:tCapHandler);
  begin
  Assert(CapHandler[cap]=nil);
  CapHandler[cap]:=handler;
end;

BEGIN
  FillChar(CapHandler,sizeof(CapHandler),0);
  assert((sizeof(tNetAddr)+sizeof(tPID))=44);
  SetMsgHandler(opcode.dhtRequest,@recvRequest);
  SetMsgHandler(opcode.dhtPeers,@recvPeers);
  SetMsgHandler(opcode.dhtVfyCh,@recvVfyCh);
  SetMsgHandler(opcode.dhtVfyRe,@recvVfyRe);
  Sha512Buffer(PublicKey,sizeof(PublicKey),MyID,sizeof(MyID));
  writeln('DHT: set ID to ',string(MyID),' from ECC');
END.
