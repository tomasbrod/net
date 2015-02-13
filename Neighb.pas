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

type tPID=Keys.tHash;

type tNeighbRecord=packed object
  addr:netaddr.t;
  pid:tPID;
  hop:word;
end;

type tNeighbInfo=object(tNeighbRecord)
 procedure Init(amaxhop:word);
 procedure Init(aaddr: netaddr.t);
 procedure Init(aaddr: netaddr.t; amaxhop:word);
 function  Next:boolean; (*nove to next (or first)*)
 private hti:integer;
 private cur:pointer;
 private maxhop:Word;
 private byaddr:boolean;
end;

procedure NotifyPeerState( event: byte; info:Peers.tInfo );
procedure NotifyIdle;

procedure AddPerson(const person:tPID);

procedure GetRoute(const dst:tPID; out via:netaddr.t; var cur:pointer);
function GetRoute(const dst:tPID; out via:netaddr.t):boolean;

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
const cRecordTTL=7;
const cRetryPeriod:tDateTime= 2000 /MSecsPerDay;

IMPLEMENTATION
uses DataBase;

procedure Propagate(const pid:tPID; hop:word); forward;

(*** Data storage ***)
type
 tNeighbNode_ptr=^tNeighbNode;
 tNeighbRecord_ptr=^tNeighbRecord;
 tNeighbNode=object(tNeighbRecord)
  next:tNeighbNode_Ptr;
 end;
 tNeighbTable=object
  {procedure Insert(const addr:netaddr.t; const pid:tPID; const hop:Word);}
  {procedure SearchBuck(const id:tPID; out i:byte; out pbuck: tBucket_ptr; out buck:tBucket_ptr);
  function  Search(const id:tPID ):tNeighbRecord_ptr;
  procedure DelByAddr( const addr:netaddr.t );}
  procedure Init;
  public
  slots:array [0..127] of ^tNeighbNode;
 end;
 tUnAckedPackets=array [1..512] of ^tUnAckedPacketMeta;
 tUnAckedPacketMeta=object
  pk:tNeighb;
  rcpt:netaddr.t;
  retry:byte;
  since:tDateTime;
 end;

var Table   :tNeighbTable;
var UnAcked :tUnAckedPackets;

procedure Update(const addr:netaddr.t; const pid:tPID; const hop:Word);
 var slot:byte;
 var cur:^tNeighbNode;
 var insertAfter:^pointer;
 var visibleChange:boolean;
 var deletedSomething:boolean;
 var pncur:^pointer;
 begin
 slot:=LongWord(pid) and high(Table.slots);
 cur:=Table.slots[slot];
 visibleChange:=true;
 deletedSomething:=false;
 pncur:=@Table.Slots[slot];
 insertAfter:=pncur;
 while assigned(cur) do begin
  if cur^.pid=pid then begin
   if cur^.addr=addr then begin
    pncur^:=@cur^.next; Dispose(cur); cur:=pncur^;
    deletedSomething:=true;
    continue;
   end else VisibleChange:=false;
  end;
  if cur^.hop<hop then insertAfter:=@cur^.next;
  pncur:=@cur^.next;
  cur:=cur^.next;
 end;
 if hop<=cRecordTTL then begin
  New(cur);
  InsertAfter^:=cur;
  cur^.hop:=hop; cur^.pid:=pid; cur^.addr:=addr;
 end else VisibleChange:=DeletedSomething;
 if VisibleChange then begin
  Propagate(pid,hop);
 end;
end;

(*** Interface ***)

procedure tNeighbInfo.Init(amaxhop:word);
 begin addr.clear; Init(addr, amaxhop); end;
procedure tNeighbInfo.Init(aaddr: netaddr.t);
 begin Init(aaddr,65535); end;
procedure tNeighbInfo.Init(aaddr: netaddr.t; amaxhop:word);
 begin
 addr:=aaddr;
 maxhop:=amaxhop;
 byAddr:=not addr.isNil;
 hti:=0;
 cur:=nil;
end;

function tNeighbInfo.Next:boolean;
 var match:boolean;
 begin
 repeat
  result:=false;
  while not assigned(cur) do begin
   if hti>High(Table.Slots) then exit;
   cur:=Table.slots[hti];
   inc(hti);
  end;
  match:=((not byAddr)or(tNeighbNode(cur^).addr=addr)) and (tNeighbNode(cur^).hop<=maxhop);
  if not match then cur:=tNeighbNode(cur^).next;
 until match;
 addr:=tNeighbNode(cur^).addr;
 pid:=tNeighbNode(cur^).pid;
 hop:=tNeighbNode(cur^).hop;
 cur:=tNeighbNode(cur^).next;
 result:=true;
end;

procedure GetRoute(const dst:tPID; out via:netaddr.t; var cur:pointer);
 begin
 via.Clear;
 if cur=nil then cur:=Table.slots[LongWord(dst) and high(Table.slots)] else cur:=tNeighbNode(cur^).next;
 if cur=nil then exit;
 while assigned(cur)and(tNeighbNode(cur^).pid<>dst) do cur:=tNeighbNode(cur^).next;
 via:=tNeighbNode(cur^).addr;
end;

function GetRoute(const dst:tPID; out via:netaddr.t):boolean;
 var p:pointer=nil;
 begin
 GetRoute(dst,via,p);
 result:=assigned(p);
end;

(*** Daemon Interaction ***)

procedure SendNeighb(const arcpt:netaddr.t; const apid:tPID; ahop:word);
 var slot:word;
 begin
 slot:=low(UnAcked); while (slot<=high(UnAcked)) and assigned(UnAcked[slot]) do inc(slot);
 if slot>high(UnAcked) then begin
  log.error('Neighb outgoing queue full!');
  exit;
 end;
 New( UnAcked[Slot] );
 with UnAcked[Slot]^ do begin
  rcpt:=arcpt;
  retry:=0;
  since:=Now;
  pk.Create( Slot, apid, ahop );
  pk.Send(rcpt);
  log.debug('send neighb '+string(apid)+' +'+inttostr(ahop)+' slot='+inttostr(slot)+' to '+string(arcpt));
 end;
end;

procedure NotifyPeerState( event: byte; info:Peers.tInfo );
 var inf:tNeighbInfo;
 var nilpid:tPID;
 begin
 nilpid.Clear;
 case event of
  1{new}: begin
   log.debug('Send neighb nodes');
   inf.Init(cPropagateTTL);
   while inf.next do begin
    SendNeighb(info.addr, inf.pid, inf.hop);
   end;
  end;
  2{deleted}: Update(info.addr, nilpid, 65535);
  0{ping}:{do nothing};
 end;
end;

procedure NotifyIdle;
 var i:word;
 begin
 for i:=low(UnAcked) to high(UnAcked) do if assigned(UnAcked[i]) then with UnAcked[i]^ do begin
  log.debug('rs '+IntToStr(Qword(UnAcked[i])));
  if now-since<=cRetryPeriod then continue;
  if retry>cMaxRetry then begin
   log.debug('rsd ');
   Dispose(UnAcked[i]);
   UnAcked[i]:=nil;
  end else begin
   log.debug('retry send neighb slot='+IntToStr(i));
   pk.Send(rcpt);
   Inc(retry);
   Since:=Now;
  end;
 end;
end;

(*** Networking ***)

procedure tNeighb.Handle( const from: NetAddr.t);
 var ack:tNeighbAck;
 begin
 Update(from,pid,word(hop)+2);
 ack.Create(self);
 ack.Send(from);
end;

procedure tNeighbAck.Handle( const from: NetAddr.t);
 var i:word;
 begin
 i:=TrID;
 log.debug('ack send neighb slot='+IntToStr(i));
 Dispose(UnAcked[i]);UnAcked[i]:=nil;
end;

procedure Propagate(const pid:tPID; hop:word);
 var inf:tNeighbInfo;
 begin
 log.debug('Propagate change of '+string(pid)+' hop='+inttostr(hop));
 exit;
 inf.Init(1);
 while inf.next do begin
  if inf.addr.isNil then continue;
  if inf.pid=pid then continue;
  log.debug(' propagate to '+string(inf.addr));
  SendNeighb(inf.addr, pid, hop);
 end;
end;

(*** Packet constructors, senders and bullshit ***)

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

procedure tNeighbTable.Init;
 var i:byte;
 begin
 for i:=low(slots) to high(slots) do slots[i]:=nil;
end;

procedure INIT;
 var i:word;
 begin;
 for i:=low(UnAcked) to high(UnAcked) do UnAcked[i]:=nil;
 Table.Init;
end;

procedure AddPerson(const person:tPID);
 var nr:tNeighbRecord;
 begin
 log.debug('Add person '+string(person));
 nr.addr.Clear;
 nr.hop:=0;
 nr.pid:=person;
 Update(nr.addr,nr.pid,nr.hop);
end;

INITIALIZATION
 INIT;
END.
