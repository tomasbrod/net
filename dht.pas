unit DHT;
{
 implementation of custom dht, based on pastry and kademlia.
 keyspace is divided into buckets of limited capacity
 node belongs to bucket, where at least 'depth' bits match 'prefix'
 old>new,
 new>dead
 TODO: weight nodes by IP-Address common prefix length.
}

{used by: messages, fileshare}

INTERFACE
uses NetAddr,Store1;
type tPID=Store1.tFID;
type tPeerPub=object
   ID   :tPID;
   Addr :tNetAddr;
   //function IsGood
   //function GetAge
 end;
var MyID:tPID;
procedure NodeBootstrap(const contact:tNetAddr);
procedure GetNextNode(var ibkt:pointer; var ix:byte; out peer:tPeerPub);
procedure InsertNode(const peer:tPeerPub);

IMPLEMENTATION
uses ServerLoop,MemStream,opcode;

type
 tPeer=object(tPeerPub)
   ReqDelta:word;
   LastMsgFrom,
   LastResFrom  :tMTime;
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
   next: tBucket_ptr;
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

function FindBucket(const prefix:tFID):tBucket_ptr;
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

procedure UpdateNode(const id:tFID; const addr:tNetAddr);
 var bkt:^tBucket;
 var i,fr:byte;
 label again;
 begin
 if id=MyID then exit;
 again:
 bkt:=FindBucket(id);
 if not assigned(bkt) then begin
  New(Table); //todo
  bkt:=Table;
  bkt^.Prefix:=MyID;
  bkt^.Depth:=0;
  bkt^.ModifyTime:=mNow;
  bkt^.next:=nil;
  bkt^.desperate:=3;
  for i:=1 to high(bkt^.peer) do bkt^.peer[i].addr.Clear;
  Shedule(2000,@bkt^.Refresh);
 end;
 fr:=0;
 for i:=1 to high(bkt^.peer)
  do if (fr=0)and bkt^.peer[i].addr.isNil then fr:=i
   //else if bkt^.peer[i].addr=addr then fr:=i
   else if bkt^.peer[i].id=id then begin
    if bkt^.peer[i].addr<>addr then exit;
    {found node in the bucket}
    //writeln('DHT: UpdateNode ',string(id));
    // ?? bkt^.ModifyTime:=mNow;
    bkt^.peer[i].LastMsgFrom:=mNow;
    bkt^.peer[i].ReqDelta:=0;
   exit end else if (fr=0) and (bkt^.peer[i].ReqDelta>=2)
                then fr:=i {use non-responding as free};
 if fr=0 then begin
  if bkt^.MatchPrefix(MyID)
   then begin
    SplitBucket(bkt);
    goto again;
   end; {the bucket is full!}
        {drop new node and hope nodes in the bucket are good}
 end else begin
  writeln('DHT: AddNode ',string(id),string(addr),' to ',string(bkt^.prefix),'/',bkt^.depth,'#',fr);
  bkt^.ModifyTime:=mNow;
  bkt^.peer[fr].ID:=ID;
  bkt^.peer[fr].Addr:=Addr;
  bkt^.peer[fr].LastMsgFrom:=mNow;
  bkt^.peer[fr].LastResFrom:=0;
  bkt^.peer[fr].ReqDelta:=0;
 end;
end;

procedure InsertNode(const peer:tPeerPub);
 begin
 UpdateNode(peer.id,peer.addr);
end;

procedure GetNextNode(var ibkt:tBucket_ptr; var ix:byte; const id:tPID; maxrd:word);
 var bkt:^tBucket;
 begin
 if not assigned(ibkt) then exit;
 bkt:=ibkt;
 repeat
  inc(ix);
  if ix>high(tBucket.peer) then begin
   ix:=1;
   bkt:=bkt^.next;
   if not assigned(bkt) then break;
  end;
 until (not bkt^.peer[ix].Addr.isNil)and(bkt^.peer[ix].ReqDelta<maxrd);
 ibkt:=bkt;
end;

procedure GetNextNode(var ibkt:pointer; var ix:byte; out peer:tPeerPub);
 begin
 if ibkt=nil then ibkt:=Table;
 GetNextNode(ibkt,ix,MyID,3);
 if assigned(ibkt)
 then peer:=tBucket(ibkt^).peer[ix]
 else peer.addr.clear;
end;

procedure RecvRequest(msg:tSMsg);
 var s:tMemoryStream absolute msg.stream;
 var hID:^tPID;
 var rID:^tPID;
 var caps:byte;
 var r:tMemoryStream;
 var bkt:^tBucket;
 var i:byte;
 begin
 s.skip(1);
 hID:=s.ReadPtr(20);
 rID:=s.ReadPtr(20);
 caps:=s.ReadByte;
 //writeln('DHT: ',string(msg.source^),' Request for ',string(rID^));
 UpdateNode(hID^,msg.source^);
 {Select peers only from The bucket,
  if it is broken, send none, but still Ack}
 bkt:=FindBucket(rID^);
 r.Init(128);
 if assigned(bkt) then begin
  r.WriteByte(opcode.dhtSelect);
  r.WriteByte(caps);
  r.Write(msg.Source^,sizeof(tNetAddr));
  r.Write(rID^,20);
  r.Write(hID^,20);
  if (s.RdBufLen>0)and(s.RdBufLen<=8) then r.Write(s.RdBuf^,s.RdBufLen);
  for i:=1 to high(tBucket.peer) do begin
   if bkt^.peer[i].addr.isNil then continue;
   if bkt^.peer[i].addr=msg.source^ then continue;
   if bkt^.peer[i].ReqDelta>1 then continue;
   //writeln('-> Select to ',string(bkt^.peer[i].addr));
   SendMessage(r.base^,r.length,bkt^.peer[i].addr);
  end;
  r.Seek(0);
  r.Trunc;
 end
  //else writeln('-> empty bucket')
  ;
 r.WriteByte(opcode.dhtReqAck);
 r.Write(MyID,20);
 //writeln('-> ReqAck to ',string(msg.Source^));
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

procedure RecvReqAck(msg:tSMsg);
 var s:tMemoryStream absolute msg.stream;
 var hID:^tPID;
 begin
 s.skip(1);
 hID:=s.ReadPtr(20);
 //writeln('DHT: ',string(msg.source^),' is ',string(hID^),' (ReqAck)');
 UpdateNode(hID^,msg.source^);
end;

procedure RecvWazzup(msg:tSMsg);
 var s:tMemoryStream absolute msg.stream;
 var hID:^tPID;
 begin
 s.skip(1);
 hID:=s.ReadPtr(20);
 //writeln('DHT: ',string(msg.source^),' is ',string(hID^),' (Wazzup)');
 UpdateNode(hID^,msg.source^);
 //UpdateSearch(hID^,msg.source^);
end;

procedure NodeBootstrap(const contact:tNetAddr);
 begin
 SendRequest(contact,MyID,0);
end;

procedure RecvSelect(msg:tSMsg);
 var s:tMemoryStream absolute msg.stream;
 var caps:byte;
 var addr:^tNetAddr;
 var rID:^tPID;
 var r:tMemoryStream;
 begin
 s.skip(1);
 caps:=s.ReadByte;
 addr:=s.ReadPtr(sizeof(tNetAddr));
 rID:=s.ReadPtr(20);
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

procedure tBucket.Refresh;
 var my,rtr:boolean;
 var i,ol,rv:byte;
 var wait:LongWord;
 var rvb:^tBucket;
 procedure lSend(var peer:tPeer; const trg:tPID);
  begin
  SendRequest(peer.Addr,trg,0);
  Inc(peer.ReqDelta);
 end;
 begin
 my:=MatchPrefix(MyID);
 ol:=0;
 rtr:=false;
 for i:=1 to high(tBucket.peer) do
  if (not peer[i].Addr.isNil) and (peer[i].ReqDelta<4)  then begin
   if peer[i].ReqDelta>0 then begin
    {peer is not responding, but try once more}
    if peer[i].ReqDelta=3
    then writeln('DHT: Refresh (last) ',copy(string(peer[i].id),1,6),string(peer[i].addr));
    lSend(peer[i],prefix);
    rtr:=true;
   end
   else if (ol=0) or (peer[i].LastMsgFrom<peer[ol].LastMsgFrom)
        then ol:=i;
  end;
 {now nudge the most quiet peer}
 if (ol>0) and (not rtr) then begin
  //writeln('DHT: Refresh (T',mNow-peer[ol].LastMsgFrom,') #',ol,' ',string(peer[ol].addr));
  lSend(peer[ol],MyID);
 end;
 if (not rtr)and(ol=0) then begin
  {no usable nodes in this bucket, try to recover from other buckets}
  rv:=0; rvb:=@self;
  GetNextNode(rvb,rv,prefix,desperate);
  if not assigned(rvb) then begin
   rv:=0; rvb:=Table; {in extreme cases, try the whole table}
   GetNextNode(rvb,rv,prefix,desperate);
  end;
  if assigned(rvb) then begin
   writeln('DHT: Broken bucket ',string(prefix),'/',depth,' try ',copy(string(rvb^.peer[rv].id),1,6),string(rvb^.peer[rv].addr));
   lSend(rvb^.peer[rv],prefix);
  end else inc(desperate);
 end else desperate:=3;
 if my
  then wait:=18000+(depth*600)
  else wait:=30000;
 if rtr then wait:=wait div 3;
 Shedule(wait,@Refresh);
end;


{to bootstrap: ping address to get ID and insert to bucket/il
ping may get lost: separate bootstrap unit :)
now jut Ass-U-Me wont get lost}

BEGIN
 SetMsgHandler(opcode.dhtRequest,@recvRequest);
 SetMsgHandler(opcode.dhtSelect,@recvSelect);
 SetMsgHandler(opcode.dhtReqAck,@recvReqAck);
 SetMsgHandler(opcode.dhtWazzup,@recvWazzup);
END.