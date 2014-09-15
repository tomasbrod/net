unit Peers;

INTERFACE
uses Keys
    ,NetAddr
    ,Sockets
    ,UnixType
    ,SysUtils
    ;

{ TODO: New Peer hook }

{ TODO: Split:
 * tNetAddr
}

type  tPktype =byte;

const cAkafuka :tPkType = 1;
const cAkafukaN :string='Akafuka'  experimental;
const cFundeluka :tPkType = 2;
const cFundelukaN :string = 'Fundeluka'  experimental;
//const PkAccept :set of tPkType = [cAkafuka, cFundeluka];

const cAkafukaCooldown = 5000{ms};
const cAkafukaRetry = 8{times};
const cAkafukaMaxDelta = 600000{ms}; {10 minutes to ping? Heh!}
const cAkafukaPeriod = 600000{ms}; {10 minutes to ping? Heh!}

TYPE

 tID= packed object(Keys.tHash)
 {
  Unique identifier of the peer.
  Technicaly a 160bit SHA1 hash of master public key of peer.
 }

  procedure Selected; {Creates a peer id of currently selected peer}
  { load id of currently selected peer }
  
 end;
  
 tPacket = packed object
  pktype :tPktype;
  sender :tID;
  constructor Create ( const itp :tPktype );
  procedure Handle;
  experimental;
  procedure Send( Len:LongInt ); overload;
  function TimeSince: System.tTime;
  { returns time since last packet from the peer of that type arrived }
 end;

 tAkafuka =packed object(tPacket)
 {
  A Ping-Pong packet.
  Name is reference to Zidan (Dávid) Sufusky Sufurky
 }

  procedure Handle;
  experimental;
  { Executes approportiate actions to respond to this packet }

  procedure Send; overload;
  experimental;
  { mark selected address as akafuka and actually sends the packet }

  constructor Create; overload;
  { Creates akafuka packet }

  private
  Load: byte unimplemented; { load of the sender's system }
  YouSock :NetAddr.T; { address, the packet was sent to }
 end;

 tFundeluka= packed object(tAkafuka)

  procedure Handle;
  experimental;
  { unmarkd selected address as akafuka, computes AkafukaDelta }

  procedure Send; overload;
  experimental;
  { Computes length of the packet and calls send }

  constructor Create; overload;
  { Creates fundeluka packet }

 end;
 
 eNoAddress = class(Exception)
 {
  Exception signaling that peer has no associated sockaddr.
  Thus no packet can be sent.
 }
  ID : tID; { id of erroring peer }
  constructor Create( iid :tID );
 end;
 
var
 ThisID :tID;
 SelectedID : tID;
 SelectedAddr :NetAddr.T;
 IsSelectedID : boolean;
 IsSelectedAddr :boolean;
 SendProc: procedure(var Data; Len:LongInt);
 NewProc :procedure( id :tID );

procedure Select( ID :tID );
{ Selects peer with given ID and automatically picks an sockaddr }

procedure DoAkafuka;
 experimental;
{
 Send Akafuke to all peers.
 Remove not responding peers.
}

procedure Add( addr :NetAddr.T );
{ Add peer only known by its address. }
experimental;

procedure Reset;

procedure SelfTest;

IMPLEMENTATION
uses 
     DataBase
    ,Log
    ;

const cTable :tTable = 'peers';

{*********************************
 *********** Addresses *********** 
 *********************************}

const cAddrField :tField = 'addr';

type tAkafukaInfo=record
 Delta :System.tTime;
 Since :System.tDateTime;
 Retry :Word;
end;

type tAddrInfo=object
 sock :NetAddr.T;
 akafuka :tAkafukaInfo;
end;

type tAddrAccess=object(DataBase.tAccess)
 constructor Init( id: tID );
 constructor Init;
 procedure Find( out pos :tRecord; const Addr :NetAddr.T);
 procedure Add( const Addr :NetAddr.T );
 procedure Add( const info :tAddrInfo );
 procedure Remove( const Addr :tAddrInfo );
end;

constructor tAddrAccess.Init( id: tID );
 var row: tRow;
 begin
 id.ToString(row);
 inherited Init( sizeof(tAddrInfo), cTable, row, cAddrField );
end;

constructor tAddrAccess.Init;
 begin Init( SelectedID ); end;

procedure tAddrAccess.Find( out pos :tRecord; const Addr :NetAddr.T);
 var Ex : tAddrInfo;
 begin
 pos:=0;
 while true do begin
  Read( Ex, pos );
  if Ex.sock=Addr then break;
  inc(pos);
 end;
end;


procedure tAddrAccess.Add( const Addr :NetAddr.T );
 var i:tRecord;
 var Ex : tAddrInfo;
 begin
 if Addr.data.Family = afNil then exit; {Do not associate with nil address}
 try
  Find( i, Addr );
  exit;
 except
  on eRangeError do begin
   Ex.Sock:=Addr;
   Ex.Akafuka.Since:=0;
   Ex.Akafuka.Delta:=0;
   Ex.Akafuka.Retry:=0;
   OverWrite( i, Ex );
  end;
 end;
end;

procedure tAddrAccess.Add( const info :tAddrInfo );
 var i:tRecord;
 var Ex : tAddrInfo;
 begin
 if info.sock.data.Family = afNil then exit; {Do not associate with nil address}
 try
  Find( i, info.sock );
  OverWrite( i, Info );
 except
  on eRangeError do Append( info );
 end;
end;

procedure tAddrAccess.Remove( const Addr :tAddrInfo );
 var i:tRecord;
 var Ex : tAddrInfo;
 begin
 try
  Find( i, Addr.sock );
  Delete( i );
 except
  on eRangeError do;
 end;
end;

procedure Select( ID :tID );
 {- All addresses in the db were available at last Akafuka}
 var db : tAddrAccess;
 var cur : tAddrInfo;
 begin
 if IsSelectedAddr and IsSelectedID and (SelectedID = ID) then exit;
 db.Init( id );
 try
  try
   db.Read( cur, 0 );
   {$NOTE Selects only first address from the db.}
  except
   on eRangeError do raise eNoAddress.Create(ID);
  end;
 finally
  db.Done;
 end;
 SelectedID:=ID;
 SelectedAddr:=cur.sock;
 IsSelectedID:=true;
 IsSelectedAddr:=true;
end;

{****************************************
 *********** Last Packet time *********** 
 ****************************************}

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
var cur: System.tDateTime;
var db :tLastAccessor;
begin
 db.Init( Sender );
 try
  db.Load( pktype, cur );
  result := now - cur;
 finally
  db.Done;
 end;
end;

{*********************************
 *********** Akafuka   ***********
 *********************************}

procedure tPacket.Handle;
 var db :tAddrAccess;
 var time :System.tDateTime;
 var last :tLastAccessor;
 var saddr,sid :string;
 begin
 SelectedID:=Sender;
 isSelectedID:=true;
 { addr shouldbe selected by Daemon }
 assert( IsSelectedAddr );
 SelectedAddr.ToString(saddr);
 SelectedID.ToString(sid);
 log.msg('Recieved #'+IntToStr(pktype)+' From '+sid+' ('+saddr+')');
 log.msg('Last was '+TimeToStr(TimeSince)+' ago');
 db.Init;
 last.Init( Sender );
 time:=Now;
 try
  db.Add( SelectedAddr );
  last.Store(pktype, time);
 finally
  db.done;
  last.Done;
 end;
end;

procedure tAkafuka.Send;
var db :tAddrAccess;
var C :tAddrInfo;
 var r:tRecord;
begin
 inherited Send(sizeof(SELF) - (Sizeof(YouSock)-YouSock.Length) );
 {Remove selected addr from db and append it to akafuka db}
 C.sock:=SelectedAddr;
 db.Init;
 try
  try
   db.Find( r, C.Sock );
   db.Read( C, r );
   Inc(C.akafuka.Retry);
  except on eRangeError do begin
    C.akafuka.Retry:=1;
    C.akafuka.Delta:=0;
    { db.Append( r, C ); r is already set to eof by find }
  end; end;
  C.akafuka.Since:=Now;
  db.OverWrite( r, C );
 finally
  db.done;
 end;
end;

procedure tAkafuka.Handle;
var fundeluka:tFundeluka;
var db: tAddrAccess;
begin
 inherited Handle;
 log.msg('Recieved '+cAkafukaN);
 if (TimeSince > cAkafukaCooldown) then begin
  fundeluka.Create;
  fundeluka.Send;
 end;
end;

procedure tFundeluka.Handle;
var C :tAddrInfo;
var db: tAddrAccess;
var i:tRecord;
 var isNew :boolean;
begin
 inherited Handle;
 { The assoc re-adds peer to db. Now remove it from akafuka db}
 db.init( SelectedID );
 try
  db.Find( I, SelectedAddr );
  db.Read( C, I );
  {
   What if we had sent akafuka to unknown netaddr to get it's ID?
   -> tPacket.Handle had already added the addr to db.
  }
  C.akafuka.Retry:=0;
  if C.Akafuka.Since>0 then begin
   if (C.akafuka.Delta=0) then isNew:=true;
   C.akafuka.Delta:=Now - C.akafuka.Since;
   if C.akafuka.Delta>cAkafukaMaxDelta then db.Delete( I )
   { Drop the peer if delta excedes limit }
    else begin
    db.OverWrite( I, C );
    if isNew and assigned(NewProc) then NewProc( SelectedID );
   end;
  end;
 finally
  db.done;
 end;
 db.Init( ThisID );
 try db.Add( YouSock );
 { Add reported external address }
 finally db.Done; end;
end;

procedure DoAkafuka;
 {
 Go through each peer and
  - Remove timeouted in akafuka.dat
  - Resend Akafuka in akafuka.dat
  - Send Akafuka in addr.dat
 }
 var list:DataBase.tRowList;
 var row:DataBase.tRow;
 var id:tID;
 var db :tAddrAccess;
 var C:tAddrInfo;
 var r:tRecord;
 var akafuka:tAkafuka;
 begin
 list.init(cTable);
 try repeat
  list.Read(row);
  if row='tags' then continue;
  id.FromString(row);
  SelectedID:=id;
  isSelectedId:=true;
  db.init(id);
  r:=0;
  try repeat
   try
    db.Read(C, R );
    if C.Akafuka.Retry>cAkafukaRetry then begin
     db.Delete(r);
     system.write('[');
     continue;
     { no increment, becouse delete shifted records to r }
    end;
   finally
    db.done;
    system.write(']');
   end;
   SelectedAddr:=C.sock;
   isSelectedAddr:=true;
   Akafuka.Create;
   Akafuka.Send;
   inc(R);
  until false; except on eRangeError do ; end;
  db.done;
 until false; except on eRangeError do ; end;
 list.done;
 AbstractError;
end;

procedure Add( addr :NetAddr.T );
 { Send akafuka, the peer should reply fundeluka and packet handler 
 saves the peer to db, and fundeluka packet }
 var Akafuka :tAkafuka;
 begin
 isSelectedID:=false;
 SelectedAddr:=addr;
 isSelectedAddr:=true;
 Akafuka.Create;
 Akafuka.Send;
end;

{ *** Simple Uninteresting Bullshit ***}

constructor tFundeluka.Create;
begin
 inherited Create(cFundeluka);
 YouSock:=SelectedAddr;
end;

constructor tAkafuka.Create;
begin
 inherited Create(cAkafuka);
 YouSock:=SelectedAddr;
end;

procedure tFundeluka.Send;
begin
 inherited Send(sizeof(SELF) - (Sizeof(YouSock)-YouSock.Length) );
end;

{
procedure NetAddr.T.Selected;
begin
 if (not IsSelectedAddr) then raise eInvalidInsert.Create('Not selected');
 self:=SelectedAddr;
end;
}

procedure tID.Selected;
begin
 if (not IsSelectedID) then raise eInvalidInsert.Create('Not selected');
 self:=SelectedID;
end;

constructor tPacket.Create ( const itp :tPktype );
 begin
 pktype := itp;
 Sender:= ThisID;
end;

procedure tPacket.Send(Len:LongInt);
 begin
 //sender := ThisID;
 Assert( SendProc <> nil );
 SendProc( self, Len );
end;

constructor eNoAddress.Create( iid :tID );
var idstr:string;
begin
 iid.ToString(idstr);
 inherited Create( 'No Address associated to Peer '+idstr );
 id:=iid;
end;

procedure Reset;
begin
 IsSelectedID:=false;
 IsSelectedAddr:=false;
end;

procedure TestID;
 var id,rid:tID;
 begin
 rid.FromString('00100000000A00000FF000000000000000000040');
 SelectedID:=rid;
 isSelectedID:=True;
 id.Clear;
 assert( id.isNil );
 id.Selected;
 assert( id = rid );
end;

 
procedure TestAddrInfo;
 var db :tAddrAccess;
 var C :tAddrInfo;
 var na :NetAddr.T;
 var na2 :NetAddr.T;
 var id:tID;
 var r :tRecord;
 begin
 na.data.Family:=afInet;
 na.data.inet.port:=3;
 na.data.inet.addr.s_Addr:=5;
 na2:=na;
 assert( na = na2 );
 id.FromString('00100000000A00000FF000000000000000000040');
 db.init( id );
 db.Add( na );
 db.Find( r, na);
 assert( r=0 );
 na.data.inet.port:=2;
 na2:=na;
 db.Add( na );
 db.Add( na );
 db.Add( na );
 db.Add( na );
 db.LastPos(r);
 assert( r=1 );
 assert( na = na2 );
 db.Find( r, na);
 assert( r=1 );
 db.Read( C, R );
 assert( c.akafuka.retry = 0 );
 na.data.inet.port:=4;
 try db.Find( r, na ); assert(false); except on eRangeError do; end;
 db.purge;
 Reset;
 id.FromString('00100000000A00000FF000000000000000000041');
 db.init( id );
 db.Add( na );
 Assert(SelectedAddr<>na);
 Select( id );
 Assert(SelectedID=id);
 Assert(SelectedAddr=na);
 C.sock:=SelectedAddr;
 C.akafuka.Since:=5;
 db.Add( C );
 db.Find( r, c.sock );
 db.Read( C, R );
 assert( r=0 );
 assert( c.akafuka.since = 5 );
 assert( c.akafuka.retry = 0 );
 db.purge;
end;

{
procedure TestLast;
 var id:tID;
 begin
 id.FromString('00100000000B00000FF000000000000000000040');
 SelectedID:=id;
 isSelectedID:=True;
 id.
}

procedure SelfTest;
 begin
 Reset;
 TestID;
 TestAddrInfo;
 end;


END.
