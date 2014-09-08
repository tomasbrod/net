unit Peers;

INTERFACE
uses GeneralPacket
    ,Keys
    ,Sockets
    ,UnixType
    ,SysUtils
    ;

{ TODO: OnAnyPacketReceive : Assoc }

const cAkafuka :tPkType = 1;
const cAkafukaN :string='Akafuka'  experimental;
const cFundeluka :tPkType = 2;
const cFundelukaN :string = 'Fundeluka'  experimental;
//const PkAccept :set of tPkType = [cAkafuka, cFundeluka];

const cAkafukaCooldown = 5000{ms};
const cAkafukaRetry = 8{times};
const cAkafukaMaxDelta = 600000{ms}; {10 minutes to ping? Heh!}

TYPE

 tNetAddr= packed object
 { Object to store Socket Address }

  function Length :word;
   experimental;
  { Returns length of the structure based on address family }

  procedure Selected;
  { load currently selected address }
  
  procedure ToSocket( var sockaddr :tSockAddr; const socklen :tSockLen);
   unimplemented;

  private
  data :record
  case Family : byte of 
   { note: maximum family is 32 so byte is enough }
   AF_INET :( inet :record 
    sin_port: cushort;
    sin_addr: tInAddr;
   end; );
   0 :(
    pad_pV4IlkA4mKQL :array [0..128] of byte;
   ); 
  end;
 end;
 
 tID= packed object(Keys.tHash)
 {
  Unique identifier of the peer.
  Technicaly a 160bit SHA1 hash of master public key of peer.
 }

  procedure Selected; {Creates a peer id of currently selected peer}
  { load id of currently selected peer }
  
 end;
  

 tAkafuka =packed object(GeneralPacket.T)
 {
  A Ping-Pong packet.
  Name is reference to Zidan (Dávid) Sufusky Sufurky
 }

  procedure Handle;
  { Executes approportiate actions to respond to this packet }

  procedure Send;
  { mark selected address as akafuka and actually sends the packet }

  constructor Create; overload;
  { Creates akafuka packet }

  private
  ID :tID; { of sender }
  Load: byte unimplemented; { load of the sender's system }
  YouSock :Peers.tNetAddr; { address, the packet was sent to }
 end;

 tFundeluka= packed object(tAkafuka)

  procedure Handle;
  { unmarkd selected address as akafuka, computes AkafukaDelta }

  procedure Send;
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
 
var ThisID :tID;

var SelectedID : tID;
var SelectedAddr :tNetAddr;
var IsSelectedID : boolean;
var IsSelectedAddr :boolean;

function TimeSince( pktype :GeneralPacket.tPkType ): System.tTime;
{ returns time since last packet from the peer of that type arrived }
 experimental;

procedure ResetTimeSince( pktype :GeneralPacket.tPkType );
 experimental;

procedure Select( ID :tID );
 experimental;
{ Selects peer with given ID and automatically picks an sockaddr }

procedure DoAkafuka;
 unimplemented;
{
 Send Akafuke to all peers.
 Remove not responding peers.
}

procedure Reset;

IMPLEMENTATION
uses 
     DataBase
     ;

const cTable :tTable = 'peers';

Operator = (aa, ab :Sockets.tInAddr) b : boolean;
begin
 b:=aa.s_addr=ab.s_addr;
end;

Operator = (aa, ab :tNetAddr) b : boolean;
begin
 b:=false;
 if aa.data.Family<>ab.data.Family then exit;
 case aa.data.Family of
  Sockets.AF_INET: if (aa.data.inet.sin_port<>ab.data.inet.sin_port) or (aa.data.inet.sin_addr<>ab.data.inet.sin_addr) then exit;
  Sockets.AF_INET6: AbstractError;
  0: {null addresses are always equal};
  else AbstractError;
 end;
 b:=true;
end;

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
 sock :tNetAddr;
 akafuka :tAkafukaInfo;
end;

type tAddrAccess=object(DataBase.tAccess)
 constructor Init( id: tID );
 constructor Init;
 procedure Find( out pos :tRecord; const Addr :tNetAddr);
  experimental;
 procedure Add( const Addr :tNetAddr );
  experimental;
 procedure Add( const info :tAddrInfo );
  experimental;
 procedure Remove( const Addr :tAddrInfo );
  experimental;
end;

constructor tAddrAccess.Init( id: tID );
 var row: tRow;
 begin
 id.ToString(row);
 inherited Init( sizeof(tAddrInfo), cTable, row, cAddrField );
end;

constructor tAddrAccess.Init;
 begin Init( SelectedID ); end;

procedure tAddrAccess.Find( out pos :tRecord; const Addr :tNetAddr);
 var Ex : tAddrInfo;
 begin
 pos:=0;
 while true do begin
  Read( Ex, pos );
  if Ex.sock=Addr then break;
  inc(pos);
 end;
end;


procedure tAddrAccess.Add( const Addr :tNetAddr );
 var i:tRecord;
 var Ex : tAddrInfo;
 begin
 if Addr.data.Family = 0 then exit; {Do not associate with nil address}
 try
  Find( i, Addr );
  exit;
 except
  on eRangeError do begin
   Ex.Sock:=Addr;
   Ex.Akafuka.Since:=0;
   Ex.Akafuka.Delta:=0;
   Ex.Akafuka.Retry:=0;
   Append( Ex );
  end;
 end;
end;

procedure tAddrAccess.Add( const info :tAddrInfo );
 var i:tRecord;
 var Ex : tAddrInfo;
 begin
 if info.sock.data.Family = 0 then exit; {Do not associate with nil address}
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
 procedure Store( const pktype :GeneralPacket.tPkType; const Time: System.tTime );
 procedure Load( pktype :GeneralPacket.tPkType; out Time: System.tTime );
end;

constructor tLastAccessor.Init( id: tID );
 var row: tRow;
 begin
 id.ToString(row);
 inherited Init( sizeof(System.tDateTime), cTable, row, cLastField );
end;

procedure tLastAccessor.Store( const pktype :GeneralPacket.tPkType; const Time: System.tTime );
 begin
 OverWrite( pktype, Time );
end;
 
procedure tLastAccessor.Load( pktype :GeneralPacket.tPkType; out Time: System.tTime );
 begin
 try
  Read( Time, pktype );
 except
  on eRangeError do Time:=0;
 end;
end;

function TimeSince( pktype :GeneralPacket.tPkType ): System.tTime;
var cur: System.tDateTime;
var db :tLastAccessor;
var ID : tID;
begin
 id.Selected;
 db.Init( id );
 try
  db.Load( pktype, cur );
  result := now - cur;
 finally
  db.Done;
 end;
end;

procedure ResetTimeSince( pktype :GeneralPacket.tPkType );
var cur: System.tDateTime;
var db :tLastAccessor;
var ID : tID;
begin
 id.Selected;
 db.Init( id );
 try
  cur:=now;
  db.Store(pktype, cur);
 finally
  db.Done;
 end;
end;

{*********************************
 *********** Akafuka   ***********
 *********************************}

procedure tAkafuka.Send;
var db :tAddrAccess;
var C :tAddrInfo;
 var r:tRecord;
begin
 Repl(sizeof(SELF) - (Sizeof(YouSock)-YouSock.Length) );
 {Remove selected addr from db and append it to akafuka db}
 C.sock.Selected;
 db.Init( SelectedID );
 try
  db.Find( r, C.Sock );
  db.Read( C, r );
  C.akafuka.Since:=Now;
  C.akafuka.Delta:=0;
  Inc(C.akafuka.Retry);
  db.OverWrite( r, C );
 finally
  db.done;
 end;
end;

procedure tAkafuka.Handle;
var fundeluka:tFundeluka;
var db: tAddrAccess;
var C :tNetAddr;
begin
 db.Init( ID );
 c.Selected;
 try db.Add( C );
 finally db.Done; end;
 if (Peers.TimeSince(cAkafuka) > cAkafukaCooldown) then begin
  fundeluka.Create;
  fundeluka.Send;
 end;
 ResetTimeSince(cAkafuka);
end;

procedure tFundeluka.Handle;
var C :tAddrInfo;
var db: tAddrAccess;
var i:tRecord;
begin
 { The assoc re-adds peer to db. Now remove it from akafuka db}
 db.init( SelectedID );
 try
  db.Find( I, SelectedAddr );
  db.Read( C, I );
  if C.akafuka.Retry=0 then exit;
  { Fundeluka arrived, but we havn't send Akafuka? Igonre for now.}
  C.akafuka.retry:=0;
  C.akafuka.Delta:=Now - C.akafuka.Since;
  db.OverWrite( I, C );
  if C.akafuka.Delta>cAkafukaMaxDelta then db.Delete( I );
  { Drop the peer if delta excedes limit }
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

{ *** Simple Uninteresting Bullshit ***}

constructor tFundeluka.Create;
begin
 inherited Create(cFundeluka);
 ID:=ThisID;
 YouSock.Selected;
end;

constructor tAkafuka.Create;
begin
 inherited Create(cAkafuka);
 ID:=ThisID;
 YouSock.Selected;
end;

procedure tFundeluka.Send;
begin
 Repl(sizeof(SELF) - (Sizeof(YouSock)-YouSock.Length) );
end;

function tNetAddr.Length :Word;
begin
 case data.Family of
  0: result:=sizeof(self)-sizeof(data);
  Sockets.AF_INET: result:=sizeof( data.inet );
  else result:=sizeof(self);
 end;
end;

procedure tNetAddr.Selected;
begin
 if (not IsSelectedAddr) then AbstractError;
 self:=SelectedAddr;
end;

procedure tID.Selected;
begin
 if (not IsSelectedID) then AbstractError;
 self:=SelectedID;
end;

procedure tNetAddr.ToSocket( var sockaddr :tSockAddr; const socklen :tSockLen);
begin
 AbstractError;
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

END.
