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
uses Chat,opcode,ECC,sha512,CRAuth;

type
 tPeer=object(tPeerPub)
   ReqDelta:word;
   LastMsgFrom,
   LastResFrom  :tMTime;
   Ban:boolean;
   Verify: ^CRAuth.tAuth; {nil when verified}
   procedure VerifyCallback;
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

function CheckNode(const id: tPID; const addr: tNetAddr): boolean;
 {return false if node is banned}
 {update or insert}
 {initiate auth on insert and also on id conflict}
 {replace only old, banned and free slots}
 var b:^tBucket;
 var i,fr:byte;
 var dup:boolean;
 label again;
 begin
 if id=MyID then exit;
 CheckNode:=false;
 again:
 b:=FindBucket(id);
 fr:=0; dup:=false;
 if not assigned(b) then begin
   New(Table); b:=Table;
   b^.Prefix:=MyID;
   b^.Depth:=0;
   b^.ModifyTime:=mNow;
   b^.next:=nil;
   b^.desperate:=3;
   for i:=1 to high(b^.peer) do b^.peer[i].addr.Clear;
   for i:=1 to high(b^.peer) do b^.peer[i].ban:=false;
   Shedule(2000,@b^.Refresh);
 end;
 for i:=1 to high(b^.peer) do begin {check for ban and dup}
   if (b^.peer[i].Ban) and (b^.peer[i].Addr=addr) then exit;
   if (fr=0)and(b^.peer[i].Addr.isNil) then fr:=i;
   if (b^.peer[i].ID=id)or(b^.peer[i].Addr=Addr) then begin
     fr:=i;dup:=(b^.peer[i].ReqDelta<2);break
   end;
 end;
 if fr=0 then for i:=1 to high(b^.peer) do begin {check for old/banned}
   if (b^.peer[i].ReqDelta>=2) then fr:=i;
   if (fr=0) and (b^.peer[i].Ban) then fr:=i;
 end;
 if fr=0 then begin
  if b^.MatchPrefix(MyID) then begin
    SplitBucket(b);
    goto again;
  end (*else bucket is full and not splittable*)
 end else begin
  if dup then begin
   if (b^.peer[i].addr=addr) then begin
     b^.peer[i].LastMsgFrom:=mNow;
     b^.peer[i].ReqDelta:=0;
     CheckNode:=true;
   end else begin
     {todo conflict}
     VerifyInit(b,fr);
   end
  end else begin
   {add node here}
   if (not b^.peer[fr].Addr.isNil)and assigned(b^.peer[fr].Verify)
    then b^.peer[fr].Verify^.Cancel;
   writeln('DHT: AddNode ',string(id),string(addr),' to ',string(b^.prefix),'/',b^.depth,'#',fr);
   b^.ModifyTime:=mNow;
   b^.peer[fr].ID:=ID;
   b^.peer[fr].Addr:=Addr;
   b^.peer[fr].LastMsgFrom:=mNow;
   b^.peer[fr].LastResFrom:=0;
   b^.peer[fr].ReqDelta:=0;
   b^.peer[fr].ban:=false;
   b^.peer[fr].Verify:=nil;
   VerifyInit(b,fr);
   CheckNode:=true;
  end
 end
end;

procedure InsertNode(const peer:tPeerPub);
 begin
 CheckNode(peer.id,peer.addr);
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
       and(bans or(bkt^.peer[ix].ban=false));
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
  if not CheckNode(sID^,msg.source^) then exit;
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
 r.Init(42);{$note todo}
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
  if not CheckNode(ID^,msg.source^) then exit;
  if assigned(OnNewPeer) then OnNewPeer(ID^,msg.source^,true);
  while s.RdBufLen>44 do begin
    Addr:=s.ReadPtr(24);
    ID:=s.ReadPtr(20);
    CheckNode(ID^,Addr^);
    if assigned(OnNewPeer) then OnNewPeer(ID^,Addr^,false);
  end;
end;

procedure NodeBootstrap(const contact:tNetAddr);
 begin
 SendRequest(contact,MyID,0);
 SendRequest(contact,MyID,0); {xD}
end;

const cStichRar=10;
procedure tBucket.Refresh;
 var my,rtr,stich:boolean;
 var i,ol,rv:byte;
 var wait:LongWord;
 var list:tPeerList;
 procedure lSend(var peer:tPeer; const trg:tPID);
  begin
  SendRequest(peer.Addr,trg,0);
  Inc(peer.ReqDelta);
 end;
 begin
 my:=MatchPrefix(MyID);
 ol:=0; rtr:=false;
 {1 of 10 times try to contact dead nodes in attempt to recover from network split}
 stich:=Random(cStichRar)=0;
 for i:=1 to high(tBucket.peer)
  do if (not peer[i].Addr.isNil) and (not peer[i].Ban) then begin
   if peer[i].ReqDelta>0 then begin
    if (peer[i].ReqDelta<=3)xor stich then begin {$warning magic constants}
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
 if (ol>0) and ((mNow-peer[ol].LastMsgFrom)>10000) then begin
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
  then wait:=18000+(depth*600)
  else wait:=30000;
 if rtr and(not stich) then wait:=wait div 3;
 Shedule(wait,@Refresh);
end;

{to bootstrap: ping address to get ID and insert to bucket/il
ping may get lost: separate bootstrap unit :)
now jut Ass-U-Me wont get lost}

procedure VerifyInit(b:tBucket_ptr; i:byte);
 begin
 with b^.peer[i] do begin
  if assigned(Verify) then exit;
  new(Verify);
  Verify^.Callback:=@VerifyCallback;
  Verify^.Init(Addr);
  //writeln('DHT: Starting Verificator for ',string(Addr));
 end
end;
procedure tPeer.VerifyCallback;
 var PubHash:tPID;
 begin
 if Verify^.error>0 then begin
  writeln('DHT: Verificator error ',string(Addr),Verify^.error);
  ReqDelta:=3;
 end else begin
 writeln('DHT: ',copy(string(id),1,6),' version ',Verify^.Version);
 Sha512Buffer(Verify^.RemotePub,sizeof(tEccKey),PubHash,sizeof(PubHash));
 if Verify^.Valid and Verify^.PowValid and (CompareWord(ID,PubHash,10)=0) then
  Ban:=false
 else begin
  Ban:=true;
  writeln('DHT: Verificator failed for ',string(Addr),Verify^.Valid,Verify^.PoWValid,Verify^.error);
 end; end;
 Verify:=nil; {it will free itelf}
end;


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
  Sha512Buffer(PublicKey,sizeof(PublicKey),MyID,sizeof(MyID));
  writeln('DHT: set ID to ',string(MyID),' from ECC');
END.
