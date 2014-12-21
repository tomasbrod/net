unit Peers;

INTERFACE
uses NetAddr
    ,SysUtils
    ,EventLog
    ;

var Log:tEventLog;

const cAkafukaCooldown = 5000{ms} /MSecsPerDay;
const cMaxRetry = 3{times};
const cMaxDelta = 600000{ms} /MSecsPerDay; {10 minutes to ping? Heh!}
const cAkafukaPeriod = 5000{ms} /MSecsPerDay;
const cAkafukaUnknown  = 10;

{Generic packet}
type
 tPktype =byte;
 tPacket = packed object
  { Base object for all packets }
  pktype :tPktype;
  procedure Create ( const itp :tPktype );
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t; Len:LongInt ); overload;
  procedure AfterProcessing( const from:netaddr.t);
 end;

{Packet structures}
const cAkafuka :tPkType = 1;
const cAkafukaN ='Akafuka';
type tAkafuka =packed object(tPacket)
 {
  A Ping-Pong packet.
  Name is reference to Zidan (Dávid) Sufusky Sufurky
 }
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t);
  procedure Create; overload;
 end;

const cFundeluka :tPkType = 2;
const cFundelukaN = 'Fundeluka';
type tFundeluka= packed object(tPacket)
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t);
  procedure Create; overload;
 end;

{Peer list}
type tInfo= record
  Addr: NetAddr.t;
  Delta: tTime;
  Since: tDateTime;
 end;
procedure Get( out info:tInfo; const addr:netaddr.t ); overload;
procedure Add( addr :NetAddr.T );
{procedure GetList; unimplemented;}
 
var
 SendProc: procedure(const rcpt:netaddr.t; var Data; Len:LongInt);
 StateChangeProc :procedure( event: byte; info:tInfo );

procedure DoAkafuka;
{
 Send Akafuke to all peers.
 Remove not responding peers.
}

type {Exceptions}
 eNoAddress = class(Exception)
 {
  Exception signaling that recipient is not in list of peers.
  Thus no packet can be sent.
 }
  Addr: NetAddr.t;
  constructor Create( iAddr: NetAddr.t );
 end;
 eUnknownSender = class(Exception)
 {
  Exception signaling that sender is not in database
 }
  Addr : NetAddr.t; { id of erroring peer }
  constructor Create( iAddr : NetAddr.t );
 end;
 
procedure SelfTest;
 deprecated;

IMPLEMENTATION
uses 
     DataBase
    ,LinkedList
    ;

{ Data storage }

type tNodeWithAddress=class(LinkedList.tDLNode)
  addr:netaddr.t;
  function Search(iaddr:netaddr.t):tNodeWithAddress;
 end;

type tPeersList=class(tNodeWithAddress)
 delta:tTime;
 since:tDateTime;
 end;
var PeerList:tPeersList;

type tPendingList=class(tNodeWithAddress)
 ofpeer:tPeersList;
 since:tdatetime;
 retry:word;
 procedure CopyFrom(peer:tPeersList);
 procedure FreeSelfAndPeer;
 end;
var PendingList:tPendingList;

function tNodeWithAddress.Search(iaddr:netaddr.t):tNodeWithAddress;
 var cur:tNodeWithAddress;
 begin
 cur:=self;
 if assigned(cur) then
 repeat
  if cur.addr=iaddr then begin
   if cur<>self then cur.Swap(self); {DYNAMIC programming :)}
   result:=cur;
   break;
  end;
  cur:=tNodeWithAddress(cur.Next);
 until cur=self; {becouse the list is circular}
 result:=nil;
end;

procedure tPendingList.CopyFrom(peer:tPeersList);
 begin
 inherited;
 ofpeer:=peer;
 addr:=peer.addr;
 retry:=0;
 since:=now;
end;

procedure tPendingList.FreeSelfAndPeer;
 begin
 if assigned(ofpeer) then ofpeer.free;
 free;
end;

{Private forward declarations}

procedure Add( addr :NetAddr.T; peer:tPeersList ); forward;

{*********************************
 *********** Addresses *********** 
 *********************************}

procedure tPacket.Handle( const from:netaddr.t );
 { do nothing }
 begin
 (*
 { addr shouldbe selected by Daemon }
 assert( not SelectedAddr.isNil );
 {Lookup sender ID from database}
 try
  AkafukaDB.FindAddr( SelectedAddr );
 except
  on DataBase.eSearch do raise eUnknownSender.Create(SelectedAddr);
  on Exception do begin log.msg('While identifiing sender'); raise; end;
 end;
 {Initialize state}
 SelectedID:=AkafukaDB.ID;
 SelectedAddr:=AkafukaDB.Addr;
 SelectedDelta:=AkafukaDB.Delta;
 *)
 log.debug('Received #'+IntToStr(pktype)+' From '+String(from));
end;

{****************************************
 *********** Last Packet time *********** 
 ****************************************}

(*
const cLastField :DataBase.tField = 'last';

type tLastAccessor=object(DataBase.tFieldAccessor)
 constructor Init( id: tID );
 procedure Store( const pktype :tPkType; const Time: System.tTime );
 procedure Load( pktype :tPkType; out Time: System.tTime );
end;

constructor tLastAccessor.Init( id: tID );
 var row: tRow;
 begin
 id.ToString(row);
 inherited Init( sizeof(System.tDateTime), cTable, row, cLastField );
end;

procedure tLastAccessor.Store( const pktype :tPkType; const Time: System.tTime );
 begin
 OverWrite( pktype, Time );
end;
 
procedure tLastAccessor.Load( pktype :tPkType; out Time: System.tTime );
 begin
 try
  Read( Time, pktype );
 except
  on eRangeError do Time:=0;
 end;
end;

function tPacket.TimeSince: System.tTime;
begin
 db.Init( Sender );
 try
  db.Load( pktype, cur );
  result := now - cur;
 finally
  db.Done;
 end;
 result := high(tTime);
end;

procedure tPacket.ResetTimeSince;
 begin
end;
 *)

{*********************************
 *********** Akafuka   ***********
 *********************************}

procedure tAkafuka.Send( const rcpt: NetAddr.t);
 { Add to pendging }
 begin
 log.debug('Sending Akafuka to '+string(rcpt));
 inherited Send( rcpt, sizeof(SELF) );
end;

(*
procedure SaveReportedAddr( const Addr: NetAddr.t );
 var oldrecno:integer;
 begin
 log.msg('Sender reported our address to be '+String(Addr));
 OldRecNo:=AkafukaDB.RecNo;
 try
  try
   AkafukaDB.FindAddr(Addr);
   //if AkafukaDB.ID<>ThisID then raise eInvalidInsert.Create('Attempt to assingn same address to multiple peers');
   AkafukaDB.ID:=ThisID;
   AkafukaDB.Edit;
  except on DataBase.eSearch do begin
   AkafukaDB.Append;
   AkafukaDB.Addr:=Addr;
   AkafukaDB.ID:=ThisID;
   AkafukaDB.Delta:=cAkafukaUnknown;
   AkafukaDB.Retry:=0;
  end; on Exception do begin log.msg('While saving reported address'); raise;
  end; end;
  AkafukaDB.Since:=Now;
  AkafukaDB.Post;
 finally
  AkafukaDB.RecNo:=OldRecNo;
 end;
end;
*)

procedure tAkafuka.Handle( const from: NetAddr.t);
 { Send fundeluka. If not in roster then send akafuka. }
 var fundeluka:tFundeluka;
 var peer:tPeersList;
 var isNew: boolean;
 begin
 inherited;
 log.debug('Received '+cAkafukaN);
 fundeluka.Create;
 if {timesincelast>cAkafukaCooldown} true
  then fundeluka.Send(from);
 peer:=tPeersList(PeerList.Search(from));
 isnew:= not assigned(peer);
 if not assigned(peer)
  then Peers.Add(from);
end;

procedure tFundeluka.Handle( const from: NetAddr.t);
 { Move from pending to roster and update delta. }
 var isnew:boolean;
 var peer:tPeersList;
 var pending:tPendingList;
 begin
 inherited;
 log.debug('Received '+cFundelukaN);
 pending:=tPendingList(PendingList.Search(from));
 if not assigned(pending) then assert(false);
 peer:=tPeersList(pending.ofpeer);
 isnew:= not assigned(peer);
 if isnew then begin peer:=tPeersList.Create ; PeerList.Insert(peer) end;
 peer.delta:=now-pending.since;
 peer.since:=now;
 pending.free;
 log.info('Peer '+string(from)+': delta='+FloatToStr(peer.delta*MSecsPerDay)+' retry='+IntToStr(pending.retry));
end;

procedure DoAkafuka;
 { Retry pending. Check roster for old peers and send them akafuka. }
 var pending:tPendingList;
 var peer:tPeersList;
 var akafuka:tAkafuka;
 begin
 log.debug('DoAkafuka');
 log.debug('Retry pending');
 pending:=tPendingList(PendingList.Next);
 while assigned(pending) do begin
  if (now-pending.since)>cAkafukaPeriod then begin
   if pending.retry>cMaxRetry then begin
    log.info('Peer '+string(pending.addr)+' dropped, not responding');
    pending.freeselfandpeer;
   end else begin
    log.debug('Retry pending '+string(pending.addr));
    akafuka.create;
    akafuka.send(pending.addr);
    inc(pending.retry);
    pending.since:=now;
   end;
  end;
  pending:=tPendingList(pending.next);
 end;
 log.debug('Akafuka old peers');
 peer:=tPeersList(PeerList.Next);
 while assigned(peer) do begin
  if (now-peer.since)>cAkafukaPeriod then begin
   log.debug('Peer '+string(peer.addr)+' is too old, akafuka');
   Peers.Add(peer.addr,peer);
  end;
  peer:=tPeersList(peer.next);
 end;
 log.debug('Akafuka complete');
end;
 
procedure Add( addr :NetAddr.T; peer:tPeersList );
 { add to pending, send akafuka }
 var pending:tPendingList;
 var akafuka:tAkafuka;
 begin
 log.debug('Try to add peer '+string(addr));
 pending:=tPendingList(PendingList.Search(addr));
 if assigned(pending) then begin
  log.debug('Already pending');
 end else begin
  pending:=tPendingList.Create;
  pending.addr:=addr;
  pending.retry:=0;
  pending.since:=now;
  pending.ofpeer:=peer;
  akafuka.create;
  akafuka.send(addr);
 end;
end;

procedure Add( addr :NetAddr.T );
 begin
 Add(addr,nil);
end;

procedure Get( out info:tInfo; const addr:netaddr.t);
 var peer:tPeersList;
 begin
 peer:=tPeersList(PeerList.Search(addr));
 info.addr:=peer.addr;
 info.delta:=peer.delta;
 info.since:=peer.since;
end;
 
{ *** Simple Uninteresting Bullshit ***}

procedure tFundeluka.Create;
begin
 inherited Create(cFundeluka);
end;

procedure tAkafuka.Create;
begin
 inherited Create(cAkafuka);
end;

procedure tFundeluka.Send( const rcpt:NetAddr.t );
begin
 log.info('Sending '+cFundelukaN);
 inherited Send( rcpt, sizeof(SELF) );
end;

{
procedure NetAddr.T.Selected;
begin
 if (not IsSelectedAddr) then raise eInvalidInsert.Create('Not selected');
 self:=SelectedAddr;
end;
}

procedure tPacket.Create ( const itp :tPktype );
 begin
 pktype := itp;
end;

procedure tPacket.AfterProcessing( const from: netaddr.t);
 begin
end;

procedure tPacket.Send( const rcpt:netaddr.t; Len:LongInt);
 var saddr,sid :string;
 begin
 rcpt.ToString(saddr);
 log.debug('Sending #'+IntToStr(pktype)+' to '+saddr+'');
 Assert( SendProc <> nil );
 SendProc( rcpt, self, Len );
end;

constructor eNoAddress.Create( iAddr : NetAddr.t );
var idstr:string;
begin
 iaddr.ToString(idstr);
 inherited Create( 'No Address associated to Peer '+idstr );
 addr:=iaddr;
end;

constructor eUnknownSender.Create( iAddr : NetAddr.t );
 begin
 inherited Create( 'Unknown sender :'+String(iAddr));
 Addr:=iAddr;
end;

procedure SelfTest;
 begin
 AbstractError;
end;

INITIALIZATION
 PeerList:=tPeersList.CreateRoot;
 PendingList:=tPendingList.CreateRoot;
FINALIZATION
 PendingList.Free;
 PeerList.Free;
END.
