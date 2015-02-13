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
 procedure GoNearest;
 procedure GoNear(amaxhop:word);
 procedure GoAddr(aaddr: netaddr.t); (*search for a address*) unimplemented;
 procedure GoPID(apid: tPID); (*search for a key, hop count ascending*)
 function  Next:boolean; (*nove to next (or first)*)
 procedure Delete;
 public
 pid:tPID;
 hop:word;
 addr:netaddr.t;
 procedure PeerInfo(out info:Peers.tInfo);
 private hti:integer;
 private cnn,nn:pointer;
 private pcnn:^pointer;
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

procedure tNeighbTable.Init;
 var i:byte;
 begin
 for i:=low(slots) to high(slots) do slots[i]:=nil;
end;

procedure tNeighbTable.Insert(const addr:netaddr.t; const pid:tPID; const hop:Word);
 var i,ni:byte;
 var nn:^tNeighbNode;
 var pnn:^pointer;
 begin
 i:=LongWord(pid) and high(slots);
 nn:=slots[i]; pnn:=@slots[i];
 {search the pid/addr in the bucket}
 while assigned(nn) do begin
  if (nn^.pid=pid)and(nn^.addr=addr) then break;
  pnn:=@nn^.next;
  nn:=nn^.next;
 end;
 if assigned(nn) then begin
  log.debug('remove record to update neighb record');
  if (nn^.addr=addr)and(nn^.hopcount<hop) then begin
   log.warning('Neighb: increase route hopcount');
  end;	
  pnn^:=nn^.next; dispose(nn);
 end;
 if hop>cPropagateCount then begin
  log.debug('neighb hop limit');
  exit;
 end;
 log.debug('create new neighb record');
 {insertsort begin}
 nn:=slots[i]; pnn:=@slots[i];
 while assigned(nn) and (nn^.hopcount<hop) do begin
  pnn:=@nn^.next;
  nn:=nn^.next;
  log.debug('ins next');
 end;
 new(nn); nn^.next:=pnn^; pnn^:=nn;
 {insertsort end}
 if hop=0 then log.warning('Neighb: add route to self!');
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

{procedure tNeighbTable.DelByAddr( const addr:netaddr.t );
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
end;}

(*** Interface ***)

procedure tInfo.GoAll;
 begin AbstractError; end;

procedure tInfo.GoAddr(aaddr: netaddr.t);
 begin
 pid.clear;
 hop:=0;
 addr:=aaddr;
 hti:=-1;
 nn:=nil; pcnn:=nil;
 maxhc:=65535;
 bypid:=false;
 byaddr:=true;
end;

procedure tInfo.GoNear(amaxhop:word);
 begin
 pid.clear;
 hop:=0;
 addr.clear;
 hti:=-1;
 nn:=nil; pcnn:=nil;
 maxhc:=amaxhop;
 bypid:=false;
 byaddr:=false;
end;

procedure tInfo.GoPID(apid:tPID);
 begin
 pid:=apid;
 hop:=0;
 addr.clear;
 hti:=(LongWord(apid) and high(Table.slots))-1;
 !!! nn:=nil; pcnn:=nil;
 maxhc:=65535;
 bypid:=true;
 byaddr:=false;
end;

function tInfo.Next:boolean;
 var skip:boolean=false;
 begin
 repeat
 result:=false;
 if (not assigned(nn)) and bypid then exit; {pid will not be on any other chain}
 while not assigned(nn) do begin
  inc(hti); if hti>high(Table.slots) then exit;
  nn:=Table.slots[hti];
  pcnn:=@Table.slots[hti];
 end;
 skip:= 
      (bypid and (tNeighbNode(nn^).pid<>pid))
    or(byaddr and (tNeighbNode(nn^).addr<>addr))
    or(tNeighbNode(nn^).hopcount>maxhc);
 if skip and (tNeighbNode(nn^).hopcount>maxhc) then begin
  nn:=nil; {chain is sorted hopcount ascending} continue; end;
 if not skip then begin
  hop:=tNeighbNode(nn^).hopcount;
  pid:=tNeighbNode(nn^).pid;
  addr:=tNeighbNode(nn^).addr;
 end;
 cnn:=nn;
 nn:=tNeighbNode(nn^).next;
 until not skip;
 Next:=true;
end;

procedure tInfo.PeerInfo(out info:Peers.tInfo);
 begin
 Peers.Get(info,Addr);
end;

procedure tInfo.GoNearest;
 begin GoNear(1); end;

procedure tInfo.Delete;
 var nnn:^tNeighbNode;
 begin
 assert(assigned(nn));
 log.debug('Neighb: tInfo del route to '+string(tNeighbNode(nn^).pid)+' via '+string(tNeighbNode(nn^).addr)+' +'+inttostr(tNeighbNode(nn^).hopcount));
 pnn^:=tNeighbNode(nn^).next;
 nnn:=tNeighbNode(nn^).next;
 FreeMem(nn,sizeof(tneighbnode));
 nn:=nnn;
 {potom sa zavola next a ten to nacita}
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

procedure Propagate(const pid:tPID); unimplemented;
 var inf:tInfo;
 var hop:word;
 begin
 inf.GoPID(pid);
 if inf.Next then hop:=inf.hop else hop:=65535;
 log.debug('Propagate change of '+string(pid)+' hop='+inttostr(hop));
 inf.GoNear(1);
 while inf.next do begin
  if inf.addr.isNil then continue;
  if inf.pid=pid then begin
   log.debug('not to originator');
   continue;
  end;
  //log.debug('propagate to '+inf.addr(
  SendNeighb(inf.addr, pid, hop);
 end;
end;

procedure NotifyPeerState( event: byte; info:Peers.tInfo );
 var Sent:word deprecated;
 var inf:tInfo;
 begin
 case event of
  1{new}: begin
   log.debug('Send neighb nodes');
   inf.GoNear(cPropagateTTL);
   while inf.next do begin
    SendNeighb(info.addr, inf.pid, inf.hop);
    Inc(sent);
   end;
   log.debug('Neighb: sent '+inttostr(sent));
  end;
  2{deleted}: begin
   inf.GoAddr(info.addr);
   while inf.next do begin
    inf.Delete; !!!!
    Propagate(inf.pid);
   end;
  end;
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
 Table.Insert(from,pid,word(hop)+1);
 ack.Create(self);
 ack.Send(from);
 Propagate(pid);
end;

procedure tNeighbAck.Handle( const from: NetAddr.t);
 var i:word;
 begin
 i:=TrID;
 log.debug('ack send neighb slot='+IntToStr(i));
 Dispose(UnAcked[i]);UnAcked[i]:=nil;
end;

(*** Packet constructors, senders and bullshit ***)

{constructor tNearestList.Init;
 begin end;}

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
 Table.Insert(nr.addr,nr.pid,nr.hopcount);
end;

INITIALIZATION
 INIT;
END.
