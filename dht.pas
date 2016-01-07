unit DHT;
{
 implementation of custom dht, based on pastry and kademlia.
 keyspace is divided into buckets of limited capacity
 node belongs to bucket, where at least 'depth' bits match 'prefix'
 old>new,
 new>dead
 TODO: weight nodes by IP-Address common prefix length.
 TODO: improve node selection
}

{used by: messages, fileshare}

INTERFACE
uses NetAddr,Store1;
type tPID=Store1.tFID; {reQ: ids can be shorter}
type tPeerPub=object
   ID   :tPID;
   Addr :tNetAddr;
   //function IsGood
   //function GetAge
 end;
var MyID:tPID;
procedure NodeBootstrap(const contact:tNetAddr);
procedure GetNextNode(var ptr:pointer; out peer:tPeerPub);
procedure DoneGetNextNode(var ptr:pointer);
procedure InsertNode(const peer:tPeerPub);
type tSearch=object
  callback:procedure of object;
  caps:byte;
  extra:pointer; extralen:word;
  target:tPID;
  procedure Start;
  procedure Cancel;
 end; tSearch_ptr=^tSearch;
function NewSearch:tSearch_ptr;

IMPLEMENTATION
uses ServerLoop,Chat,MemStream,opcode,sha1,ecc,CRAuth;

{## LOW LEVEL ROUTINES AND DATA STORAGE ##}

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
   desperate:word;
   next: ^tBucket;
   function MatchPrefix(const tp:tFID):boolean;
   procedure Refresh;
 end;

var Table:^tBucket;
{deepest first}

function PrefixLength(const a,b:tFID):byte;
 var i:byte;
 var by:byte;
 var m:byte;
 begin
 by:=0;
 i:=0; while(i<=19) do begin
  if a[i]<>b[i] then break;
  inc(i);
 end;
 result:=i*8;
 if i=20 then exit;
 m:=$80;
 while(m>0) do begin
  if (a[i] and m)<>(b[i] and m) then break;
  m:=m shr 1;
  inc(result);
 end;
end;


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
procedure DoneGetNextNode(var ptr:pointer);
begin FreeMem(ptr,sizeof(tPeerList)); ptr:=nil; end;

{## NETWORK MESSAGES ##}

{Messages:
 a)Request: op, SendID, TargetID, caps, adt
 b)Select : op, caps, addr, TargetID, OrigID, adt (66) [ ]
          : op, caps, addr, TatgetID, SendID, adt (66) [*]
 c)Ack    : op, SenderID
 d)Wazzup : op, SenderID
}

procedure RecvRequest(msg:tSMsg);
 var s:tMemoryStream absolute msg.stream;
 var sID:^tPID;
 var rID:^tPID;
 var caps:byte;
 var r:tMemoryStream;
 var list:tPeerList;
 var SendCnt:byte;
 begin
 s.skip(1);
 sID:=s.ReadPtr(20);
 rID:=s.ReadPtr(20);
 caps:=s.ReadByte;
 SendCnt:=0;
 //writeln('DHT: ',string(msg.source^),' Request for ',string(rID^));
 if not CheckNode(sID^,msg.source^) then exit;
 list.Init(rID^);
 {TODO: sometimes it is better to send answer directly}
 (*if assigned(list.bkt) then begin*)
  r.Init(128);
  r.WriteByte(opcode.dhtSelect);
  r.WriteByte(caps);
  r.Write(msg.Source^,sizeof(tNetAddr));
  r.Write(rID^,20);
  r.Write(MyID,20);
  if (s.RdBufLen>0)and(s.RdBufLen<=8) then r.Write(s.RdBuf^,s.RdBufLen);
 (*end
  else writeln('-> empty bucket')
 ;*)
 while SendCnt<4 do begin
  list.Next;
  if not assigned(list.bkt) then break; {simply no more peers}
  //writeln('-> Select to ',string(list.p^.addr));
  if list.p^.addr=msg.source^ then continue;
  SendMessage(r.base^,r.length,list.p^.addr);
  Inc(SendCnt);
 end;
 r.Seek(0);
 r.Trunc;
 r.WriteByte(opcode.dhtWazzup);
 r.Write(MyID,20);
 SendMessage(r.base^,r.length,msg.source^);
 FreeMem(r.base,r.size);
end;

procedure SendRequest(const contact:tNetAddr; const forid: tPID; caps:byte);
 var r:tMemoryStream;
 begin
 r.Init(42);
 r.WriteByte(opcode.dhtRequest);
 r.Write(MyID,sizeof(tFID));
 r.Write(ForID,sizeof(tFID));
 r.WriteByte(caps);
 SendMessage(r.base^,r.length,contact);
 FreeMem(r.base,r.size);
end;

procedure RecvWazzup(msg:tSMsg);
 var s:tMemoryStream absolute msg.stream;
 var hID:^tPID;
 begin
 s.skip(1);
 hID:=s.ReadPtr(20);
 //writeln('DHT: ',string(msg.source^),' is ',string(hID^),' (Wazzup)');
 if CheckNode(hID^,msg.source^) then
 (*UpdateSearch(hID^,msg.source^)*);
end;

procedure NodeBootstrap(const contact:tNetAddr);
 begin
 SendRequest(contact,MyID,0);
 SendRequest(contact,MyID,0); {xD}
end;

procedure RecvSelect(msg:tSMsg);
 var s:tMemoryStream absolute msg.stream;
 var caps:byte;
 var addr:^tNetAddr;
 var rID,sID:^tPID;
 var r:tMemoryStream;
 begin
 s.skip(1);
 caps:=s.ReadByte;
 addr:=s.ReadPtr(sizeof(tNetAddr));
 rID:=s.ReadPtr(20);
 sID:=s.ReadPtr(20);
 if CheckNode(sID^,msg.source^) then exit;
 {TODO: if we can answer the request, JUST DO IT}
 //writeln('DHT: ',string(msg.source^),' Select for ',string(addr^));
 if rID^=MyID then begin
  //writeln('-> self');
 exit end;
 r.Init(21);
 r.WriteByte(opcode.dhtWazzup);
 r.Write(MyID,20);
 //writeln('-> Wazzup to ',string(addr^));
 SendMessage(r.base^,r.length,addr^);
 FreeMem(r.base,r.size);
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
    if (peer[i].ReqDelta<=3)xor stich then begin
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
 if (ol=0)(*and(not rtr)*) then begin
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

{## ECC AUTHENTICATION AND PROOF OF WORK VALIDATION ##}

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
 begin
 if Verify^.error>0 then begin
  writeln('DHT: Verificator error ',string(Addr),Verify^.error);
  ReqDelta:=3;
 end else
 if Verify^.Valid and Verify^.PowValid and (CompareWord(ID,Verify^.RemotePub,10)=0) then
  Ban:=false
 else begin
  Ban:=true;
  writeln('DHT: Verificator failed for ',string(Addr),Verify^.Valid,Verify^.PoWValid,Verify^.error);
 end;
 Verify:=nil; {it will free itelf}
end;

{## LOW LEVEL SEARCH ROUTINES AND DATA STRUCTURES ##}

type
 tSearch2_ptr=^tSearch2;
 tSearchNode_ptr=^tSearchNode;
 tSearchNode=object(tPeerPub)
   next:tSearchNode_ptr;
   Tx:byte;
   Rx:byte;
   TxTime:tMTime;
   score:byte;{cached}
 end;
 tSearch2=object(tSearch)
   next,prev:tSearch2_ptr;
   nodes:^tSearchNode; {sorted: high score first}
   procedure Start;
   procedure Step;
   function Insert(const ID:tPID; const Addr:tNetAddr){inserted}:boolean;
 end;

var SearchList:^tSearch2;

procedure tSearch2.Start;
 var list:tPeerList;
 begin
 list.Init(Self.target);
 list.MaxRD:=4;
 list.Next;
 nodes:=nil;
 while assigned(list.p) do begin
  Self.Insert(list.p^.ID,list.p^.Addr);
  list.Next;
 end;
 ServerLoop.Shedule(1,@Step);
end;

function tSearch2.Insert(const ID:tPID; const Addr:tNetAddr) :boolean;
 var cur:^tSearchNode;
 var insat:^pointer;
 var nscore:byte;
 begin
 result:=false;
 insat:=@nodes;
 cur:=nodes;
 nscore:=PrefixLength(ID,Self.Target);
 while assigned(cur) do begin
  if (cur^.score=nscore)and(cur^.ID=ID) then begin
   inc(cur^.Rx);
   exit; {already}end;
  if (cur^.score>nscore) then insat:=@cur^.next;
  cur:=cur^.next;
 end;
 new(cur);
 cur^.ID   :=    ID;
 cur^.Addr :=  Addr;
 cur^.Score:=nScore;
 cur^.next:=insat^;
 cur^.Tx:=0;
 cur^.Rx:=1;
 cur^.TxTime:=0;
 insat^:=cur;
end;

procedure tSearch2.Step;
 begin
 {check full ID match (caps=0)}
 if (Self.caps=0)and assigned(nodes) and (nodes^.score=160) then begin
  {found}
  Callback; exit
 end;
 {find next node to request}
 {opts: not contacted recently, highscored, }
 {TODO:}
end;

 {Start: Load from bucket Shedule(1,Step)
  Step: check target and Callback,  SendRequest to someone in searchlist
  OnReply: Add to SearchList and Step
 }


BEGIN
 SetMsgHandler(opcode.dhtRequest,@recvRequest);
 SetMsgHandler(opcode.dhtSelect,@recvSelect);
 SetMsgHandler(opcode.dhtReqAck,@recvWazzup);{deprecated}
 SetMsgHandler(opcode.dhtWazzup,@recvWazzup);
END.
