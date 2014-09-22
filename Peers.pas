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
const cAkafukaN :string='Akafuka';
const cFundeluka :tPkType = 2;
const cFundelukaN :string = 'Fundeluka';
//const PkAccept :set of tPkType = [cAkafuka, cFundeluka];

const cAkafukaCooldown = 5000{ms} /MSecsPerDay;
const cAkafukaRetry = 8{times};
const cAkafukaMaxDelta = 600000{ms} /MSecsPerDay; {10 minutes to ping? Heh!}
const cAkafukaPeriod = 20000{ms} /MSecsPerDay;

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
  procedure Create ( const itp :tPktype );
  procedure Handle;
  procedure Send( Len:LongInt ); overload;
  function TimeSince: System.tTime;
  procedure ResetTimeSince;
  { returns time since last packet from the peer of that type arrived }
 end;

 tAkafuka =packed object(tPacket)
 {
  A Ping-Pong packet.
  Name is reference to Zidan (Dávid) Sufusky Sufurky
 }

  procedure Handle;
  { Executes approportiate actions to respond to this packet }

  procedure Send; overload;
  experimental;
  { mark selected address as akafuka and actually sends the packet }

  procedure Create; overload;
  { Creates akafuka packet }

  private
  Load: byte unimplemented; { load of the sender's system }
  YouSock :NetAddr.T; { address, the packet was sent to }
 end;

 tFundeluka= packed object(tPacket)

  procedure Handle;
  experimental;
  { unmarkd selected address as akafuka, computes AkafukaDelta }

  procedure Send; overload;
  { Computes length of the packet and calls send }

  procedure Create; overload;
  { Creates fundeluka packet }

  private
  Load: byte unimplemented; { load of the sender's system }
  YouSock :NetAddr.T; { address, the packet was sent to }
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
 SelectedAddr :NetAddr.T; {$HINT Should Move SelectedAddr to SocketUtil?}
 IsSelectedID : boolean;
 IsSelectedAddr :boolean;
 SelectedDelta :System.tTime;
 SendProc: procedure(var Data; Len:LongInt);
 NewProc :procedure( id :tID );

procedure Select( ID :tID );
 experimental;
{ Selects peer with given ID and automatically picks an sockaddr }

procedure DoAkafuka;
{$HINT Not actually testet, byt seems to work}
{
 Send Akafuke to all peers.
 Remove not responding peers.
}

procedure Add( addr :NetAddr.T );
{ Add peer only known by its address. }
{$HINT Not actually testet, byt seems to work}

procedure Reset;

procedure SelfTest;
 platform;

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
 //log.msg('AddrAccess $'+IntToHex(LongWord(@self),8)+' Init '+row);
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
 var str:string;
 begin
 Addr.ToString(str);
 if Addr.data.Family = afNil then exit; {Do not associate with nil address}
 try
  Find( i, Addr );
  exit;
 except
  on eRangeError do begin
   //log.msg('Addr Add '+str+' new @'+IntToStr(i));
   Ex.Sock:=Addr;
   Ex.Akafuka.Since:=0;
   Ex.Akafuka.Delta:=-1;
   Ex.Akafuka.Retry:=0;
   OverWrite( i, Ex );
  end;
 end;
end;

procedure tAddrAccess.Add( const info :tAddrInfo );
 var i:tRecord;
 var str:string;
 begin
 info.sock.ToString(str);
 if info.sock.data.Family = afNil then exit; {Do not associate with nil address}
 //log.msg('Addr Add info '+str);
 try
  Find( i, info.sock );
  OverWrite( i, Info );
  //log.msg('ow @'+inttostr(i));
 except
  on eRangeError do Append( info );
 end;
end;

procedure tAddrAccess.Remove( const Addr :tAddrInfo );
 var i:tRecord;
 var str:string;
 begin
 Addr.sock.ToString(str);
 try
  Find( i, Addr.sock );
  Delete( i );
  //log.msg('Addr Del '+str+' @'+IntToStr(i));
 except
  on eRangeError do;
 end;
end;

procedure Select( ID :tID );
 {- All addresses in the db were available at last Akafuka}
 var db : tAddrAccess;
 var cur : tAddrInfo;
 var saddr,sid :string;
 begin
 if IsSelectedAddr and IsSelectedID and (SelectedID = ID) then exit;
 ID.ToString(sid);
 log.msg('Select '+sid);
 db.Init( id );
 try
  try
   db.Read( cur, 0 );
   {$NOTE Selects only first address from the db. Should decide based on Delta and sock.family}
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
 SelectedDelta:=cur.akafuka.Delta;
 SelectedAddr.ToString(saddr);
 log.msg('Selected '+sid+' ('+saddr+')');
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

procedure tPacket.ResetTimeSince;
 var last :tLastAccessor;
 begin
 last.Init( Sender );
 try last.Store(pktype, Now);
 finally last.Done; end;
end;

{*********************************
 *********** Akafuka   ***********
 *********************************}

procedure tPacket.Handle;
 var db :tAddrAccess;
 var saddr,sid :string;
 begin
 SelectedID:=Sender;
 isSelectedID:=true;
 { addr shouldbe selected by Daemon }
 assert( IsSelectedAddr );
 //var saddr,sid :string;
 SelectedAddr.ToString(saddr);
 SelectedID.ToString(sid);
 log.msg('Received #'+IntToStr(pktype)+' From '+sid+' ('+saddr+')');
 log.msg('Last was '+TimeToStr(TimeSince)+'('+FloatToStr(TimeSince*SecsPerDay)+'s) ago');
 db.Init;
 try
  db.Add( SelectedAddr );
 finally
  db.done;
 end;
end;

procedure tAkafuka.Send;
var db :tAddrAccess;
var C :tAddrInfo;
 var r:tRecord;
begin
 log.msg('Sending Akafuka');
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
  //log.msg('Akafuka info: Retry='+IntToStr(C.akafuka.Retry)+' Delta='+FloatToStr(C.akafuka.Delta*SecsPerDay)+'s Since=now');
  db.OverWrite( r, C );
 finally
  db.done;
 end;
 inherited Send(sizeof(SELF) - (Sizeof(YouSock)-YouSock.Length) );
end;

procedure SaveReportedAddr( const Addr: NetAddr.t );
 var db: tAddrAccess;
 var C: tAddrInfo;
 var str:string;
 var r:tRecord;
 begin
 Addr.ToString(str);
 log.msg('Sender Reported our Address '+str);
 db.Init( ThisID );
 try
  try
   db.Find( R, Addr );
   db.Read( C, R );
  except on eRangeError do begin
   C.Sock:=Addr;
   C.Akafuka.Retry:=0;
   C.Akafuka.Delta:=-1;
  end; end;
  C.Akafuka.Since:=Now;
  db.OverWrite( R, C );
 { Add reported external address }
 finally db.Done; end;
end;

procedure tAkafuka.Handle;
var fundeluka:tFundeluka;
begin
 inherited Handle;
 log.msg('Received '+cAkafukaN);
 if (TimeSince > cAkafukaCooldown) then begin
  log.msg('Sending '+cFundelukaN);
  fundeluka.Create;
  fundeluka.Send;
 end else log.msg('Anti-DDoS');
 SaveReportedAddr( YouSock );
 ResetTimeSince;
end;

procedure tFundeluka.Handle;
var C :tAddrInfo;
var db: tAddrAccess;
var i:tRecord;
 var isNew :boolean=false;
begin
 inherited Handle;
 log.msg('Received '+cFundelukaN);
 { The assoc re-adds peer to db. Now remove it from akafuka db}
 db.init( SelectedID );
 try
  db.Find( I, SelectedAddr ); {$HINT OPT: use result from tpacket.handle as index to db}
  //log.msg('Read info @'+IntToStr(i));
  db.Read( C, I );
  {
   What if we had sent akafuka to unknown netaddr to get it's ID?
   -> tPacket.Handle had already added the addr to db.
  }
  if C.Akafuka.Since>0 then begin
   if (C.akafuka.Delta=-1) then isNew:=true;
   if isNew then log.msg('This is new Peer');
   C.akafuka.Delta:=Now - C.akafuka.Since;
   log.msg('Akafuka info: Retry='+IntToStr(C.akafuka.Retry)+' Delta='+FloatToStr(C.akafuka.Delta*SecsPerDay)+'s Since='+DateTimeToStr(C.Akafuka.Since));
   C.akafuka.Retry:=0;
   if C.akafuka.Delta>cAkafukaMaxDelta then begin
    log.msg('AkafukaDelta too big');
    db.Delete( I )
    { Drop the peer if delta excedes limit }
   end else begin
    db.OverWrite( I, C );
    if isNew and assigned(NewProc) then NewProc( SelectedID );
   end;
  end else log.msg('No Akafuka info');
 finally
  db.done;
 end;
 SaveReportedAddr( YouSock );
 ResetTimeSince;
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
 var str:string;
 begin
 list.init(cTable);
 try repeat
  list.Read(row);
  if row='tags' then continue;
  id.FromString(row);
  if id.isNil then continue;
  //log.msg('doing '+row);
  SelectedID:=id;
  isSelectedId:=true;
  db.init(id);
  r:=0;
  try repeat
   try
    db.Read(C, R );
    c.sock.tostring(str);
    log.msg('Read address @'+IntToStr(R)+' '+str+' of '+row);
    if C.Akafuka.Retry>cAkafukaRetry then begin
     log.msg('Retry to big, delete @'+IntToStr(R));
     db.Delete(r);
     continue;
     { no increment, becouse delete shifted records to r }
    end;
    if (Now-C.Akafuka.Since) < cAkafukaPeriod then begin
     log.msg('Akafuka info recent enough');
     Inc(R); {Skip to next} continue;
    end;
   finally
    {database is closed after this loop, not here}
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
end;

procedure Add( addr :NetAddr.T );
 { Send akafuka, the peer should reply fundeluka and packet handler 
 saves the peer to db, and fundeluka packet }
 var Akafuka :tAkafuka;
 begin
 isSelectedID:=false;
 SelectedID.Clear;
 SelectedAddr:=addr;
 isSelectedAddr:=true;
 Akafuka.Create;
 {$HINT This should not create peer db row nil field addr.}
 Akafuka.Send;
end;

{ *** Simple Uninteresting Bullshit ***}

procedure tFundeluka.Create;
begin
 inherited Create(cFundeluka);
 YouSock:=SelectedAddr;
end;

procedure tAkafuka.Create;
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

procedure tPacket.Create ( const itp :tPktype );
 begin
 pktype := itp;
 Sender:= ThisID;
end;

procedure tPacket.Send(Len:LongInt);
 var saddr,sid :string;
 begin
 //sender := ThisID;
 SelectedAddr.ToString(saddr);
 SelectedID.ToString(sid);
 log.msg('Sending #'+IntToStr(pktype)+' to '+sid+' ('+saddr+')');
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
