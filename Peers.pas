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

procedure OpenDB(var F: File; const id :tID; const Field :tField);
 deprecated; {$NOTE TODO: Use DataBase accessor instead. Maybe override constructors.}
var row: tRow;
begin
 id.ToString(row);
 DataBase.Open(F, cTable, row, Field);
end;

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

type tAddrAccess=object(DataBase.tAccess)
 constructor Init( id: tID );
 constructor Init;
 procedure Add( const Addr :tNetAddr );
  experimental;
 procedure Remove( const Addr :tNetAddr );
  experimental;
end;

constructor tAddrAccess.Init( id: tID );
 var row: tRow;
 begin
 id.ToString(row);
 inherited Init( sizeof(tNetAddr), cTable, row, cAddrField );
end;

constructor tAddrAccess.Init;
 begin Init( SelectedID ); end;

procedure tAddrAccess.Add( const Addr :tNetAddr );
 var i:tRecord;
 var Ex : tNetAddr;
 begin
 if Addr.data.Family = 0 then exit; {Do not associate with nil address}
 try
  i:=0;
  while true do begin
   Read( Ex, i );
   if Ex=Addr then break;
   inc(i);
  end;
  OverWrite( i, Addr );
 except
  on eRangeError do Append( Addr );
 end;
end;

procedure tAddrAccess.Remove( const Addr :tNetAddr );
 var i:tRecord;
 var Ex : tNetAddr;
 begin
 try
  i:=0;
  while true do begin
   Read( Ex, i );
   if Ex=Addr then Delete( i );
   inc(i);
  end;
  OverWrite( i, Addr );
 except
  on eRangeError do;
 end;
end;

procedure Assoc( const nw: tNetAddr; const id: tID );
 deprecated;
 { Associate network address *to* id }
var db : tAddrAccess;
begin
 db.Init( id );
 try
  db.Add( nw );
 finally
  db.Done;
 end;
end;

procedure Assoc( const id: tID );
 deprecated;
var Nw : tNetAddr;
begin
 Nw.Selected;
 Assoc( nw, id );
end;

procedure Remove( const nw :tNetAddr );
 deprecated;
var db : tAddrAccess;
var ID : tID;
begin
 id.Selected;
 db.Init( id );
 try
  db.Remove( nw );
 finally
  db.Done;
 end;
end;

procedure Select( ID :tID );
 {- All addresses in the db were available at last Akafuka}
 var db : tAddrAccess;
 var cur : tNetAddr;
 begin
 if (IsSelectedAddr and IsSelectedID) and (SelectedID = ID) then exit;
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
 SelectedAddr:=cur;
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

const cAkafukaProgressField :tField = 'akafuka';

type tAkafukaProgress=record
 Addr :tNetAddr;
 Since :System.tDateTime;
 Retry :Word;
end;

type tAkafukaAccess=object(DataBase.tFieldAccessor)
 constructor Init( id: tID );
 procedure Add( d :tAkafukaProgress );
  experimental;
 procedure Pop( out d :tAkafukaProgress );
  experimental;
end;

constructor tAkafukaAccess.Init( id: tID );
 var row: tRow;
 begin
 id.ToString(row);
 inherited Init( sizeof(tAkafukaProgress), cTable, row, cAkafukaProgressField );
end;

procedure tAkafukaAccess.Add( d :tAkafukaProgress );
 begin Append( d ); end;
 
procedure tAkafukaAccess.Pop( out d :tAkafukaProgress );
 var i :tRecord;
 begin
  i:=0;
  while true do begin
   Read( d, i );
   if d.Addr = SelectedAddr then begin
    Delete( i );
    break;
   end;
  end;
 end;

procedure tAkafuka.Send;
var db :tAkafukaAccess;
var addr :tAddrAccess;
var C :tAkafukaProgress;
begin
 Repl(sizeof(SELF) - (Sizeof(YouSock)-YouSock.Length) );
 {Remove selected addr from db and append it to akafuka db}
 C.Addr:=SelectedAddr;
 C.Since:=Now;
 C.Retry:=1;
 db.Init( SelectedID );
 addr.Init ( SelectedID );
 try
  addr.remove( SelectedAddr );
  db.Add( C );
 finally
  addr.done;
  db.done;
 end;
end;

procedure tAkafuka.Handle;
var fundeluka:tFundeluka;
begin
 Assoc (ID); {Associate sender's sockaddr with fingerprint.}
 if (Peers.TimeSince(cAkafuka) > cAkafukaCooldown) then begin
  fundeluka.Create;
  fundeluka.Send;
 end;
 ResetTimeSince(cAkafuka);
end;

procedure tFundeluka.Handle;
var C :tAkafukaProgress;
var Delta :System.tTime;
var db :tAkafukaAccess;
begin
 Assoc (ID);
 Assoc (YouSock, ThisID); {associate reported address to us}
 { The assoc re-adds peer to db. Now remove it from akafuka db}
 db.init( SelectedID );
 try
  db.Pop( C );
  Delta:= Now - C.Since;
 finally
  db.done;
 end;
 if Delta>cAkafukaMaxDelta then Remove(SelectedAddr);
 { Drop the peer if delta excedes limit }
end;

procedure DoAkafuka;
{
 Go through each peer and
  - Remove timeouted in akafuka.dat
  - Resend Akafuka in akafuka.dat
  - Send Akafuka in addr.dat
}
begin
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
