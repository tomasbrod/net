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
const cAkafukaUnknown  = 1;

TYPE

 tID= packed object(Keys.tHash)
 {
  Unique identifier of the peer.
  Technicaly a 160bit SHA1 hash of master public key of peer.
 }
 end;
  
 tPacket = packed object
  pktype :tPktype;
  procedure Create ( const itp :tPktype );
  procedure Handle;
  procedure Send( Len:LongInt ); overload;
  function TimeSince: System.tTime;
   unimplemented;
  procedure ResetTimeSince;
  { returns time since last packet from the peer of that type arrived }
   unimplemented;
   deprecated;
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
  SenderID: tID;
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
  SenderID: tID;
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
 SelectedDelta :System.tTime;
 SendProc: procedure(var Data; Len:LongInt);
 NewProc :procedure( id :tID );

procedure Select( ID :tID );
 unimplemented;
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
    ,db
    ,Log
    ,Dbf_Common
    ;

{*********************************
 *********** Addresses *********** 
 *********************************}

type tAkafukaDB = class (DataBase.tDbDataSet)
 private procedure HandleAfterScroll( DataSet: TDataSet );
 public procedure Post; override; //To save changes

 public
 constructor Create; {override;}overload;

 public
 ID: Peers.tID;
 Addr: NetAddr.T;
 Delta: System.tTime;
 Since: tDateTime;
 Retry: Integer;
 procedure FindAddr ( iaddr: NetAddr.T );
end;

constructor tAkafukaDB.Create;
 begin
 inherited Create( nil );
 AfterScroll:=@HandleAfterScroll;
 try
  Open ('peers');
 except
  on Exception {$NOTE Use proper exception class} do begin
   FieldDefs.Add( {0} 'row',   ftAutoInc,  0, True  );
   FieldDefs.Add( {1} 'id',    ftString,  40, True  );
   FieldDefs.Add( {2} 'addr',  ftString,  50, True  );
   FieldDefs.Add( {3} 'delta', ftFloat, 0, True );
   FieldDefs.Add( {4} 'since', ftDateTime, 0, True );
   FieldDefs.Add( {5} 'retry', ftSmallInt, 0, True );
   AddIndex( 'row',  'row',  [ixUnique, ixPrimary]);
   AddIndex( 'id',   'id',   [          ixCaseInsensitive]);
   AddIndex( 'addr', 'addr', [ixUnique, ixCaseInsensitive]);
   AddIndex( 'since', 'DTOS(since)', []);
  end;
 end;
end;

procedure tAkafukaDB.HandleAfterScroll( DataSet: TDataSet );
 begin
 tHash(ID):=Fields[1].AsString;
 Addr:=Fields[2].AsString;
 Delta:=Fields[3].AsFloat;
 Since:=Fields[4].AsDateTime;
 Retry:=Fields[5].AsInteger;
end;
  
procedure tAkafukaDB.Post;
 begin
 Fields[1].AsString:=String(tHash(ID));
 Fields[2].AsString:=String(Addr);
 Fields[3].AsFloat:=Delta;
 Fields[4].AsDateTime:=Since;
 Fields[5].AsInteger:=Retry;
end;

procedure tAkafukaDB.FindAddr ( iaddr: NetAddr.T );
 var AddrStr : string;
 begin
 assert( not iaddr.isNil);
 if iaddr = addr then exit;
 iaddr.ToString(AddrStr);
 if not Locate ( 'addr', AddrStr, [] ) then raise DataBase.eSearch.Create;
end;


var AkafukaDB :tAkafukaDB;

procedure Select( ID :tID );
 {- All addresses in the db were available at last Akafuka}
 var saddr,sid :string;
 begin
 if SelectedAddr.isNil or SelectedID.isNil or (SelectedID = ID) then exit;
 ID.ToString(sid);
 log.msg('Select '+sid);
 (*
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
 *)
 SelectedID:=ID;
 //SelectedAddr:=cur.sock;
 //SelectedDelta:=cur.akafuka.Delta;
 SelectedAddr.ToString(saddr);
 log.msg('Selected '+sid+' ('+saddr+')');
end;

procedure tPacket.Handle;
 begin
 { addr shouldbe selected by Daemon }
 assert( not SelectedAddr.isNil );
 {Lookup sender ID from database}
 try
  AkafukaDB.FindAddr( SelectedAddr );
 except
  on DataBase.eSearch do raise Exception.Create('Sender address not in database');
 end;
 {Initialize state}
 SelectedID:=AkafukaDB.ID;
 SelectedAddr:=AkafukaDB.Addr;
 log.msg('Received #'+IntToStr(pktype)+' From '+String(tHash(SelectedID))+' ('+String(SelectedAddr)+')');
 log.msg('Last was '+TimeToStr(AkafukaDB.Since)+'('+FloatToStr(AkafukaDB.Since*SecsPerDay)+'s) ago');
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
*)

function tPacket.TimeSince: System.tTime;
begin
 (*
 db.Init( Sender );
 try
  db.Load( pktype, cur );
  result := now - cur;
 finally
  db.Done;
 end;
 *)
 result := high(tTime);
end;

procedure tPacket.ResetTimeSince;
 begin
 AbstractError;
end;

{*********************************
 *********** Akafuka   ***********
 *********************************}

procedure tAkafuka.Send;
 begin
 log.msg('Sending Akafuka');
 SenderID:=ThisID;
 YouSock:=SelectedAddr;
 try
  AkafukaDB.FindAddr( SelectedAddr );
  AkafukaDB.Edit;
 except
  on DataBase.eSearch do begin
   AkafukaDB.Append;
   AkafukaDB.ID:=SelectedID;
   AkafukaDB.Addr:=SelectedAddr;
   AkafukaDB.Delta:=cAkafukaUnknown;
   AkafukaDB.Retry:=0;
  end;
 end;
 AkafukaDB.Retry:=AkafukaDB.Retry+1;
 AkafukaDB.Since:=Now;
 AkafukaDB.Post;
 log.msg('Akafuka info: Retry='+IntToStr(AkafukaDB.Retry)+' Delta='+FloatToStr(AkafukaDB.Delta*SecsPerDay)+'s Since=now');
 inherited Send(sizeof(SELF) - (Sizeof(YouSock)-YouSock.Length) );
end;

procedure SaveReportedAddr( const Addr: NetAddr.t );
 var oldrecno:integer;
 begin
 log.msg('Sender reported our address to be '+String(Addr));
 OldRecNo:=AkafukaDB.RecNo;
 try
  try
   AkafukaDB.FindAddr(Addr);
   if AkafukaDB.ID<>ThisID then raise eInvalidInsert.Create('Attempt to assingn same address to multiple peers');
   AkafukaDB.Edit;
  except on Exception do begin
   AkafukaDB.Append;
   AkafukaDB.Addr:=Addr;
   AkafukaDB.ID:=ThisID;
  end; end;
  AkafukaDB.Retry:=0;
  AkafukaDB.Delta:=cAkafukaUnknown;
  AkafukaDB.Since:=Now;
  AkafukaDB.Post;
 finally
  AkafukaDB.RecNo:=OldRecNo;
 end;
end;

procedure tAkafuka.Handle;
var fundeluka:tFundeluka;
var isNew: boolean;
begin
 log.msg('Received '+cAkafukaN);
 try
  inherited Handle;
  isNew:=false;
 except on Exception do begin
  { This means that peer/addr is not yet associated, we fix this and call the handler again }
  isNew:=true;
  log.msg('Add addr;id to db.');
  AkafukaDB.Append;
  AkafukaDB.ID:=SenderID;
  AkafukaDB.Addr:=SelectedAddr;
  AkafukaDB.Since:=Now; {to trigger Akafuka on next DoAkafuka}
  AkafukaDB.Delta:=cAkafukaUnknown;
  AkafukaDB.Retry:=0;
  AkafukaDB.Post;
  inherited Handle; { now it should be OK }
 end; end;
 if isNew or (TimeSince > cAkafukaCooldown) then begin
  log.msg('Sending '+cFundelukaN);
  fundeluka.Create;
  fundeluka.Send;
 end else log.msg('Anti-DDoS, not sending '+cFundelukaN);
 SaveReportedAddr( YouSock );
end;

procedure tFundeluka.Handle;
 begin
 inherited Handle; { this also rejects unknown or invalid senders and leaves db at sender addr }
 if AkafukaDB.ID<>SenderID then raise eInvalidInsert.Create('Invalid Sender ID');
 AkafukaDB.Edit;
 log.msg('Received '+cFundelukaN);
 { Calculate delta, reset timestamp }
 AkafukaDB.Delta:=Now - AkafukaDB.Since;
 AkafukaDB.Since:=Now;
 AkafukaDB.Post;
 log.msg('Akafuka info: Retry='+IntToStr(AkafukaDB.Retry)+' Delta='+FloatToStr(AkafukaDB.Delta*SecsPerDay)+'s Since='+DateTimeToStr(AkafukaDB.Since));
 SaveReportedAddr( YouSock );
end;

procedure DoAkafuka;
 {
 Go through each peer and
  - Remove timeouted in akafuka.dat
  - Resend Akafuka in akafuka.dat
  - Send Akafuka in addr.dat
 }
 var akafuka:tAkafuka;
 begin
 try
  {Lets delete timeouted peers}
  AkafukaDB.IndexName:='since';
  AkafukaDB.SetRange( tDateTime(cAkafukaMaxDelta), 1.7E308, false );
  AkafukaDB.First;
  while not AkafukaDB.EOF do begin
   log.msg('Akafuka Timeout: '+String(tHash(AkafukaDB.ID))+' '+String(AkafukaDB.Addr));
   AkafukaDB.Delete;
   //AkafukaDB.Next;
  end;
  AkafukaDB.CancelRange;
  {Try sending akafuka again}
  AkafukaDB.SetRange( tDateTime(cAkafukaPeriod), 1.7E308, false );
  AkafukaDB.First;
  while not AkafukaDB.EOF do begin
   log.msg('Akafuka Retry: '+String(tHash(AkafukaDB.ID))+' '+String(AkafukaDB.Addr));
   Akafuka.Create;
   SelectedID:=AkafukaDB.ID;
   SelectedAddr:=AkafukaDB.Addr;
   Akafuka.Send;
   AkafukaDB.Next;
  end;
 finally
  AkafukaDB.CancelRange;
  AkafukaDB.Filter:='';
  AkafukaDB.Filtered:=False;
  AkafukaDB.IndexName:='';
 end;
end;

procedure Add( addr :NetAddr.T );
 { Send akafuka, the peer should reply fundeluka and packet handler 
 saves the peer to db, and fundeluka packet }
 var Akafuka :tAkafuka;
 begin
 SelectedID.Clear;
 SelectedAddr:=addr;
 Akafuka.Create;
 Akafuka.Send;
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

procedure tFundeluka.Send;
begin
 SenderID:=ThisID;
 YouSock:=SelectedAddr;
 inherited Send(sizeof(SELF) - (Sizeof(YouSock)-YouSock.Length) );
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

procedure tPacket.Send(Len:LongInt);
 var saddr,sid :string;
 begin
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
 SelectedID.Clear;
 SelectedAddr.Clear;
end;

procedure SelfTest;
 begin
 AbstractError;
end;

INITIALIZATION
 AkafukaDB:=tAkafukaDB.Create;
FINALIZATION
 AkafukaDB.Free;
END.
