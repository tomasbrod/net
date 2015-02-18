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
  propagated:tDateTime;
  updated:tDateTime;
end;

type tInfo=object(tNeighbRecord)
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
procedure GetRoute(const dst:tPID; out via:netaddr.t; out hop:word; var cur:pointer);
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

const cPropagateCount=13;
const cPropagateTTL=5;
const cRecordTTL=7;
const cRetryPeriod:tDateTime= 2000 /MSecsPerDay;
const cPropagatePeriod:tDateTime= 10000 /MSecsPerDay;
const cRecordTimeout:tDateTime= 25000 /MSecsPerDay;

IMPLEMENTATION
uses DataBase;

procedure Propagate(var rec:tNeighbRecord); forward;
procedure DispatchEvent(const pid:tPID); forward;

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

var Table   :tNeighbTable;

procedure Update(const addr:netaddr.t; const pid:tPID; const hop:Word);
 var slot:byte;
 var cur:^tNeighbNode;
 var insertAfter:^pointer;
 var visibleChange:boolean;
 var deletedSomething:boolean;
 var pncur:^pointer;
 begin
 //log.debug('Update neighb '+string(pid)+' via '+string(addr)+' +'+inttostr(hop));
 slot:=LongWord(pid) and high(Table.slots);
 cur:=Table.slots[slot];
 visibleChange:=true;
 deletedSomething:=false;
 pncur:=@Table.Slots[slot];
 insertAfter:=pncur;
 while assigned(cur) do begin
  if cur^.pid=pid then begin
   if cur^.addr=addr then begin
    if cur^.hop=hop then begin
     cur^.updated:=now;
     exit; {it is the same, no need to do anything}
    end;
    pncur^:=cur^.next; Dispose(cur); cur:=pncur^;
    deletedSomething:=true;
    continue;
   end else VisibleChange:=false;
   if cur^.hop=0 then exit;
  end;
  if cur^.hop<hop then insertAfter:=@cur^.next;
  pncur:=@cur^.next;
  cur:=cur^.next;
 end;
 if hop<=cRecordTTL then begin
  New(cur);
  InsertAfter^:=cur;
  cur^.hop:=hop; cur^.pid:=pid; cur^.addr:=addr; cur^.updated:=now;
  if VisibleChange then begin
   DispatchEvent(pid);
   Propagate(cur^);
  end;
 end else VisibleChange:=DeletedSomething;
end;

(*** Daemon Interaction ***)

procedure SendNeighb(const arcpt:netaddr.t; const apid:tPID; ahop:word); forward;

procedure SendNeighbours( const rcpt:netaddr.t );
 var inf:tInfo;
 begin
 inf.Init(cPropagateTTL);
 while inf.next do begin
  if rcpt<>inf.addr then
  SendNeighb(rcpt, inf.pid, inf.hop);
 end;
end;

procedure DeleteByAddr( const addr:netaddr.t );
 var slot:byte;
 var cur,ncur:^tNeighbNode;
 var pncur:^pointer;
 begin
 for slot:=low(table.slots) to high(table.slots) do begin
  cur:=Table.slots[slot];
  pncur:=@Table.Slots[slot];
  while assigned(cur) do begin
   if cur^.addr=addr then begin
    log.debug('Delete neighb (rekt) pid='+string(cur^.pid));
    ncur:=cur^.next;
    pncur^:=ncur;
    Dispose(cur);
    cur:=ncur;
   end else begin
    pncur:=@cur^.next;
    cur:=cur^.next;
   end;
  end;
 end;
end;

procedure NotifyPeerState( event: byte; info:Peers.tInfo );
 begin
 case event of
  1{new}: SendNeighbours(info.addr);
  2{deleted}: DeleteByAddr(info.addr);
  0{ping}:{do nothing};
 end;
end;

procedure NotifyIdle;
 var slot:byte;
 var cur,ncur:^tNeighbNode;
 var pncur:^pointer;
 begin
 for slot:=low(table.slots) to high(table.slots) do begin
  cur:=Table.slots[slot];
  pncur:=@Table.Slots[slot];
  while assigned(cur) do begin
   //log.debug('>>> TABLEDUMP '+string(cur^.pid)+' via '+string(cur^.addr)+' +'+inttostr(cur^.hop)+' @'+IntToStr(LongWord(cur)));
   if (cur^.hop>0) and (now-cur^.updated>cRecordTimeout) then begin
    log.debug('Delete neighb (timeout) pid='+string(cur^.pid));
    ncur:=cur^.next;
    pncur^:=ncur;
    Dispose(cur);
    cur:=ncur;
    continue;
   end;
   if now-cur^.propagated>cPropagatePeriod then begin
    Propagate(cur^);
   end;
   pncur:=@cur^.next;
   cur:=cur^.next;
  end;
 end;
end;

procedure DispatchEvent(const pid:tPID);
 begin
 log.info('Neighbour '+String(pid)+' appeared');
end;

(*** Networking ***)

procedure tNeighb.Handle( const from: NetAddr.t);
 begin
 Update(from,pid,word(hop)+1);
end;

procedure Propagate(var rec:tNeighbRecord);
 var pinf:Peers.tInfo;
 var pinfp:pointer=nil;
 begin
 if now-rec.propagated<cPropagatePeriod then begin
  exit;
 end;
 Peers.Get(pinf,pinfp);
 while assigned(pinfp) do begin
  if pinf.addr<>rec.addr then
  SendNeighb(pinf.addr, rec.pid, rec.hop);
  Peers.Get(pinf,pinfp);
 end;
 rec.propagated:=now;
end;

procedure SendNeighb(const arcpt:netaddr.t; const apid:tPID; ahop:word);
 var pk:tNeighb;
 begin
 pk.Create( 0, apid, ahop );
 pk.Send(arcpt);
end;

(*** Interface ***)

procedure tInfo.Init(amaxhop:word);
 begin addr.clear; Init(addr, amaxhop); end;
procedure tInfo.Init(aaddr: netaddr.t);
 begin Init(aaddr,65535); end;
procedure tInfo.Init(aaddr: netaddr.t; amaxhop:word);
 begin
 addr:=aaddr;
 maxhop:=amaxhop;
 byAddr:=not addr.isNil;
 hti:=0;
 cur:=nil;
end;

function tInfo.Next:boolean;
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
 var dud:word;
 begin
 GetRoute(dst,via,dud,cur);
end;

procedure GetRoute(const dst:tPID; out via:netaddr.t; out hop:word; var cur:pointer);
 begin
 via.Clear;
 if cur=nil then cur:=Table.slots[LongWord(dst) and high(Table.slots)] else cur:=tNeighbNode(cur^).next;
 if cur=nil then exit;
 while assigned(cur)and(tNeighbNode(cur^).pid<>dst) do cur:=tNeighbNode(cur^).next;
 via:=tNeighbNode(cur^).addr;
 hop:=tNeighbNode(cur^).hop;
end;

function GetRoute(const dst:tPID; out via:netaddr.t):boolean;
 var p:pointer=nil;
 begin
 GetRoute(dst,via,p);
 result:=assigned(p);
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

(*** Initialization ***)

procedure tNeighbTable.Init;
 var i:byte;
 begin
 for i:=low(slots) to high(slots) do slots[i]:=nil;
end;

procedure INIT;
 begin;
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
