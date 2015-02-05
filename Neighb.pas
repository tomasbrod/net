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
 private hti:byte;
 private nn:pointer;
 private maxhc:Word;
 private bypid,byaddr:boolean;
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
 tNeighbNode_ptr=^tNeighbNode;
 tNeighbRecord_ptr=^tNeighbRecord;
 tNeighbRecord=object
  addr:netaddr.t;
  pid:tPID;
  hopcount:word;
 end;
 tNeighbNode=object(tNeighbRecord)
  next:tNeighbNode_Ptr;
 end;
 tNeighbTable=object
  procedure Insert(const addr:netaddr.t; const pid:tPID; const hop:Word);
  {procedure SearchBuck(const id:tPID; out i:byte; out pbuck: tBucket_ptr; out buck:tBucket_ptr);
  function  Search(const id:tPID ):tNeighbRecord_ptr;}
  procedure DelByAddr( const addr:netaddr.t );
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

procedure tNeighbTable.Init;
 var i:byte;
 begin
 for i:=low(slots) to high(slots) do slots[i]:=nil;
end;

procedure tNeighbTable.Insert(const addr:netaddr.t; const pid:tPID; const hop:Word);
 var i,ni:byte;
 var nn,pnn:^tNeighbNode;
 begin
 i:=LongWord(pid) and high(slots);
 nn:=slots[i]; pnn:=nil;
 {search the pid/addr in the bucket}
 while assigned(nn) do begin
  if (nn^.pid=pid)and(nn^.addr=addr) then break;
  pnn:=nn;
  nn:=nn^.next;
 end;
 if assigned(nn) then begin
  log.debug('remove record to update neighb record');
  if (nn^.addr=addr)and(nn^.hopcount<hop) then begin
   log.warning('Neighb: increase route hopcount');
  end;	
  if assigned(pnn) then pnn^.next:=nn^.next else slots[i]:=nn^.next; dispose(nn); nn:=nil;
 end;
 log.debug('create new neighb record');
 {insertsort}
 nn:=slots[i]; pnn:=nil;
 while assigned(nn) and (nn^.hopcount<hop) do begin
  pnn:=nn;
  nn:=nn^.next;
  log.debug('ins next');
 end;
 if assigned(pnn) then begin
  new(nn); nn^.next:=pnn^.next; pnn^.next:=nn;
 end else begin
  new(nn); nn^.next:=slots[i]; slots[i]:=nn;
 end;
 {insertsort end}
 if nn^.hopcount=0 then log.warning('Neighb: add route to self!');
 nn^.addr:=addr;
 nn^.pid:=pid;
 nn^.hopcount:=hop;
 log.info('Neighbour: '+string(pid)+' @ '+string(addr)+' +'+IntToStr(hop));
end;

{procedure tNeighbTable.SearchBuck(const id:tPID; out i:byte; out pbuck: tBucket_ptr; out buck:tBucket_ptr);
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
end;}

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

{function tNeighbTable.Search(const id:tPID ):tNeighbRecord_ptr;
 var i:byte;
 var pbuck,buck:tBucket_ptr;
 begin
 SearchBuck(id,i,pbuck,buck);
 if not assigned(buck) then raise DataBase.eSearch.Create;
 result:=buck^.data;
end;}

procedure tNeighbTable.DelByAddr( const addr:netaddr.t );
 var i:byte;
 var deleted:word;
 var pnn,nn:^tNeighbNode;
 begin
 deleted:=0;
 for i:=low(slots) to high(slots) do begin
  pnn:=nil;
  nn:=slots[i];
  while assigned(nn) do begin
   if nn^.addr=addr then begin
    log.debug('Neighb: del route to '+string(nn^.pid)+' via '+string(nn^.addr)+' +'+inttostr(nn^.hopcount));
    if assigned(pnn) then pnn^.next:=nn else slots[i]:=nn^.next;
    nn:=nn^.next;
    (*pnn:=pnn;*)
    inc(deleted);
   end else begin
    pnn:=nn;
    nn:=nn^.next;
   end;
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
 pid:=apid;
 hop:=0;
 addr.clear;
 hti:=0;
 nn:=Table.slots[0];
 maxhc:=65535;
 bypid:=true;
 byaddr:=false;
end;

procedure tInfo.GoNearest;
 begin
 pid.Clear;
 hop:=0;
 addr.clear;
 hti:=0;
 nn:=Table.slots[0];
 maxhc:=1;
 bypid:=false;
 byaddr:=false;
end;

function tInfo.Next:boolean;
 begin
 result:=false;
 while not assigned(nn) do begin
  inc(hti); if hti>high(Table.slots) then exit;
  nn:=Table.slots[hti];
 end;
 Next:=false;
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
 var Sent:word;
 var inf:tInfo;
 begin
 case event of
  1{new}: begin
   log.debug('Send neighb nodes');
   inf.GoNearest;
   ni:=Table.Nearest.Next;
   sent:=0;
   while assigned(ni) and (sent<cPropagateCount) and (ni^.slots^.hopcount<=cPropagateTTL) do begin
    GetSlot;
    New( UnAcked[Slot] );
    with UnAcked[Slot]^ do begin
     rcpt:=info.addr;
     retry:=0;
     since:=Now;
     pk.Create( Slot, ni^.data^.pid, ni^.data^.hopcount );
     pk.Send(rcpt);
     log.debug('send neighb '+string(ni^.data^.pid)+' +'+inttostr(ni^.data^.hopcount)+' slot='+inttostr(slot));
    end;
    ni:=ni^.next;
   end;
  end;
  2{deleted}: Table.DelByAddr(info.addr);
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

(*** Packet handlers ***) 

procedure tNeighb.Handle( const from: NetAddr.t);
 var nr:tNeighbRecord;
 var ack:tNeighbAck;
 begin
 nr.addr:=from;
 nr.hopcount:=word(hop)+1;
 nr.pid:=pid;
 Table.Insert(nr);
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
 Table.Init;
end;

procedure AddPerson(const person:tPID);
 var nr:tNeighbRecord;
 begin
 log.debug('Add person '+string(person));
 nr.addr.Clear;
 nr.hopcount:=0;
 nr.pid:=person;
 Table.Insert(nr);
end;

INITIALIZATION
 INIT;
END.
