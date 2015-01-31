unit Neighb experimental;

(*
 To store and search neighbours of peers
*)

INTERFACE

uses
     Peers
    ,NetAddr
    ,Keys
    ,SysUtils
    ;

type tPID=object(Keys.tHash) end;

type tInfo=object
 procedure GoAll; (*sorted by key*) unimplemented;
 procedure GoNearest; (*sorted by hopcount*)
 procedure GoAddr(aaddr: netaddr.t); (*search for a address*) unimplemented;
 procedure GoPID(apid: tPID); (*search for a key, hop count ascending*)
 function  Next:boolean; (*nove to next (or first)*)
 public
 pid:tPID;
 hop:word;
 addr:netaddr.t;
 procedure PeerInfo(out info:Peers.tInfo);
 private neighbnode:pointer; hti:byte; htb:pointer; nearestnode:pointer;
end;

procedure NotifyPeerState( event: byte; info:Peers.tInfo );
procedure NotifyIdle;

procedure AddPerson(const person:tPID);

const cNeighb=7;
type tNeighb=object(tPacket)
 pid:tPID;
 hop:word2;
 trid:word2;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create(atrid:word; apid: tPID; ahop:word );
end;

const cNeighbAck=8;
type tNeighbAck=object(tPacket)
 trid:word2;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create( var inreply:tNeighb );
end;

const cPropagateCount=13;
const cPropagateTTL=5;
const cRetryPeriod:tDateTime= 2000 /MSecsPerDay;

IMPLEMENTATION
uses DataBase;

(*** Data storage ***)
type
 tNearestList_ptr=^tNearestList;
 tNeighbNode_ptr=^tNeighbNode;
 tNeighbRecord_ptr=^tNeighbRecord;
 tBucket_ptr=^tBucket;
 tNeighbRecord=object
  addr:netaddr.t;
  pid:tPID;
  hopcount:word;
 end;
 tNeighbNode=object(tNeighbRecord)
  IdxNearest:tNearestList_ptr;
  next:tNeighbNode_Ptr;
 end;
 tBucket=object
  data:^tNeighbNode;
  next:tBucket_ptr;
 end;
 tNearestList=object
  function InsertSort(n:tNeighbNode_ptr):tNearestList_ptr;
  constructor Init;
  destructor Unlink;
  public
  next,prev:tNearestList_ptr;
  data:^tNeighbRecord;
 end;
 tNeighbTable=object
  procedure Insert(var n:tNeighbRecord);
  procedure SearchBuck(const id:tPID; out i:byte; out pbuck: tBucket_ptr; out buck:tBucket_ptr);
  function  Search(const id:tPID ):tNeighbRecord_ptr;
  procedure DelByAddr( const addr:netaddr.t );
  procedure Init;
  private
  data:array [0..31] of ^tBucket;
  public
  nearest:tNearestList;
 end;
 tUnAckedPackets=array [1..512] of ^tUnAckedPacketMeta;
 tUnAckedPacketMeta=object
  pk:tNeighb;
  rcpt:netaddr.t;
  retry:byte;
  since:tDateTime;
 end;

var ByID :tNeighbTable;
var UnAcked :tUnAckedPackets;

procedure tNeighbTable.Init;
 var i:byte;
 begin
 nearest.data:=nil;
 nearest.prev:=nil;
 nearest.next:=nil;
 for i:=low(data) to high(data) do data[i]:=nil;
end;

procedure tNeighbTable.Insert(var n:tNeighbRecord);
 var i,ni:byte;
 var buckn:^tBucket;
 var nn:^tNeighbNode;
 begin
 i:=LongWord(n.pid) and 31;
 buckn:=data[i];
 {search the pid in the bucket}
 while assigned(buckn) do begin
  if buckn^.data^.pid=n.pid then break;
  buckn:=buckn^.next;
 end;
 if buckn=nil then begin {and add it if no exist}
  buckn:=data[i];
  new(data[i]);
  data[i]^.next:=buckn;
  buckn:=data[i];
 end;
 nn:=buckn^.data;
 {TODO: scan the chain for same address and update that record instead}
 while assigned(nn) do begin
  if nn^.addr=n.addr then break;
  nn:=nn^.next;
 end;
 if nn=nil then begin
  new(nn);{node}
  buckn^.data:=nn;
  nn^.IdxNearest:=nil;
 end else begin
  if nn^.hopcount<n.hopcount then exit; {do not update if prev was better}
  dispose(nn^.IdxNearest,Unlink); {do not create dupl in nearestlist}
 end;
 with nn^ do begin
  addr:=n.addr;
  pid:=n.pid;
  hopcount:=n.hopcount;
  IdxNearest:=Nearest.InsertSort(nn);
 end;
end;

procedure tNeighbTable.SearchBuck(const id:tPID; out i:byte; out pbuck: tBucket_ptr; out buck:tBucket_ptr);
 begin
 i:=LongWord(id) and 31;
 buck:=data[i];
 pbuck:=nil;
 while assigned(buck) do begin
  if buck^.data^.pid=id then begin
   exit;
  end;
  pbuck:=buck;
  buck:=buck^.next;
 end;
end;
 
{procedure tNeighbTable.Pop(const id:tPID; out n:tNeighbRecord);
 var i:byte;
 var pbuck,buck:tNeighbNode_ptr;
 begin
 SearchBuck(id,i,pbuck,buck);
 if not assigned(buck) then raise DataBase.eSerch.Create;
 n:=buck^.data^;
 buck^.data^.IdxNearest^.Unlink;
 dispose(buck^.data^.IdxNearest);
 if assigned(pbuck) then pbuck^.next:=buck^.next else data[i]:=buck^.next;
 dispose(buck);
end;}

function tNeighbTable.Search(const id:tPID ):tNeighbRecord_ptr;
 var i:byte;
 var pbuck,buck:tBucket_ptr;
 begin
 SearchBuck(id,i,pbuck,buck);
 if not assigned(buck) then raise DataBase.eSearch.Create;
 result:=buck^.data;
end;

function tNearestList.InsertSort(n:tNeighbNode_ptr):tNearestList_ptr;
 var p,c,e:^tNearestList;
 begin
 p:=@self;
 c:=self.next;
 while assigned(c) do begin
  if c^.data^.hopcount>n^.hopcount then break;
  p:=c;
  c:=c^.next;
 end;
 new(e,init);
 e^.next:=c;
 e^.prev:=p;
 p^.next:=e;
 if assigned(c) then c^.prev:=e;
 e^.data:=n;
 result:=e;
end;
 
destructor tNearestList.Unlink;
 begin
 if assigned(next) then next^.prev:=prev;
 prev^.next:=next;
 prev:=nil; next:=nil;
end;

procedure tNeighbTable.DelByAddr( const addr:netaddr.t );
 var i:byte;
 var deleted:word;
 var pbuck,buck:^tBucket;
 begin
 deleted:=0;
 for i:=low(data) to high(data) do begin
  pbuck:=nil;
  buck:=data[i];
  while assigned(buck) do begin
   if buck^.data^.addr=addr then begin
    dispose(buck^.data^.IdxNearest,Unlink);
    if assigned(pbuck) then pbuck^.next:=buck^.next else data[i]:=buck^.next;
    dispose(buck);
    inc(deleted);
   end;
   pbuck:=buck;
   buck:=buck^.next;
  end;
 end;
end;

(*** Interface ***)

procedure tInfo.GoAll;
 begin AbstractError; end;
procedure tInfo.GoAddr(aaddr: netaddr.t);
 begin AbstractError; end;

procedure tInfo.GoPID(apid:tPID);
 var pi:tPID;
 begin
 pid.Clear;
 hop:=0;
 addr.clear;
 hti:=0;
 htb:=nil;
 neighbnode:=ByID.Search(apid);
 nearestnode:=nil;
end;

procedure tInfo.GoNearest;
 begin
 pid.Clear;
 hop:=0;
 addr.clear;
 hti:=0;
 htb:=nil;
 neighbnode:=nil;
 nearestnode:=ByID.Nearest.Next;
end;

function tInfo.Next:boolean;
 begin
 if assigned(neighbnode) then begin
  pid:=tNeighbRecord_ptr(neighbnode)^.pid;
  hop:=tNeighbRecord_ptr(neighbnode)^.hopcount;
  addr:=tNeighbRecord_ptr(neighbnode)^.addr;
  neighbnode:=tNeighbNode_ptr(neighbnode)^.next;
  Next:=true;
 end else if assigned(nearestnode) then begin
  pid:=tNearestList_ptr(nearestnode)^.data^.pid;
  hop:=tNearestList_ptr(nearestnode)^.data^.hopcount;
  addr:=tNearestList_ptr(nearestnode)^.data^.addr;
  nearestnode:=tNearestList_ptr(nearestnode)^.Next;
  Next:=true;
 end {else if hti>0 then begin
  if assigned(htb) then
   inc(hti);
   htb:=?;
 end} else Next:=false;
end;

procedure tInfo.PeerInfo(out info:Peers.tInfo);
 begin
 Peers.Get(info,Addr);
end;

(*** Daemon Interaction ***)

procedure NotifyPeerState( event: byte; info:Peers.tInfo );
 var slot:word;
 procedure GetSlot;
  begin
  slot:=low(UnAcked); while (slot<=high(UnAcked)) and assigned(UnAcked[slot]) do inc(slot);
 end;
 var NI:^tNearestList;
 var Sent:word;
 begin
 case event of
  1{new}: begin
   ni:=ByID.Nearest.Next;
   sent:=0;
   while assigned(ni) and (sent<cPropagateCount) and (ni^.data^.hopcount<=cPropagateTTL) do begin
    GetSlot;
    New( UnAcked[Slot] );
    with UnAcked[Slot]^ do begin
     rcpt:=info.addr;
     retry:=0;
     since:=Now;
     pk.Create( Slot, ni^.data^.pid, ni^.data^.hopcount+1 );
     pk.Send(rcpt);
    end;
    ni:=ni^.next;
   end;
  end;
  2{deleted}: ByID.DelByAddr(info.addr);
  0{ping}:{do nothing};
 end;
end;

procedure NotifyIdle;
 var i:word;
 begin
 for i:=low(UnAcked) to high(UnAcked) do if assigned(UnAcked[i]) then with UnAcked[i]^ do begin
  if now-since<=cRetryPeriod then continue else
  if retry>cMaxRetry then Dispose(UnAcked[i]) else begin
   pk.Send(rcpt);
   Inc(retry);
   Since:=Now;
  end;
 end;
end;

(*** Packet handlers ***) 

procedure tNeighb.Handle( const from: NetAddr.t);
 var nr:tNeighbRecord;
 begin
 nr.addr:=from;
 nr.hopcount:=hop;
 nr.pid:=pid;
 ByID.Insert(nr);
end;

procedure tNeighbAck.Handle( const from: NetAddr.t);
 var i:word;
 begin
 i:=TrID;
 Dispose(UnAcked[i]);
end;

(*** Packet constructors, senders and bullshit ***)

constructor tNearestList.Init;
 begin end;

procedure tNeighb.Send( const rcpt: NetAddr.t);
 begin
 inherited Send( rcpt, sizeof(self));
end;

procedure tNeighb.Create(atrid:word; apid: tPID; ahop:word );
 begin
 trid:=atrid;
 pid:=apid;
 hop:=ahop;
 inherited Create(cNeighb);
end;

procedure tNeighbAck.Send( const rcpt: NetAddr.t);
 begin
 inherited Send( rcpt, sizeof(self));
end;

procedure tNeighbAck.Create( var inreply:tNeighb );
 begin
 trid:=inreply.trid;
 inherited Create(cNeighbAck);
end;

(*** Initialization ***)

procedure INIT;
 var i:word;
 begin;
 for i:=low(UnAcked) to high(UnAcked) do UnAcked[i]:=nil;
 ByID.Init;
end;

procedure AddPerson(const person:tPID);
 var nr:tNeighbRecord;
 begin
 nr.addr.Clear;
 nr.hopcount:=0;
 nr.pid:=person;
 ByID.Insert(nr);
end;

INITIALIZATION
 INIT;
END.
