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
  procedure Create ( const itp :tPktype );
  procedure Handle;
  procedure Send( Len:LongInt ); overload;
  function TimeSince: System.tTime;
   unimplemented;
  procedure ResetTimeSince;
  { returns time since last packet from the peer of that type arrived }
   unimplemented;
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
 IsSelectedID : boolean deprecated;
 IsSelectedAddr :boolean deprecated;
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
 Delta: tDateTime;
 Since: tDateTime;
 Retry: Integer;
 procedure FindAddr ( iaddr: NetAddr.T );
 procedure FindToRetry;
end;

constructor tAkafukaDB.Create;
 begin
 AfterScroll:=@HandleAfterScroll;
 try
  Open ('peers');
 except
  on Exception {$NOTE Use proper exception class} do begin
   FieldDefs.Add( 'row',   ftAutoInc,  0, True  );
   FieldDefs.Add( 'id',    ftString,  40, True  );
   FieldDefs.Add( 'addr',  ftString,  50, True  );
   FieldDefs.Add( 'delta', ftDateTime, 0, False );
   FieldDefs.Add( 'since', ftDateTime, 0, False );
   FieldDefs.Add( 'retry', ftSmallInt, 0, False );
   AddIndex( 'peersrow',  'row',  [ixUnique, ixPrimary]);
   AddIndex( 'peersid',   'id',   [          ixCaseInsensitive]);
   AddIndex( 'peersaddr', 'addr', [ixUnique, ixCaseInsensitive]);
   AddIndex( 'peerssince', 'DTOS(since)', [  ixCaseInsensitive]);
  end;
 end;
end;

procedure tAkafukaDB.FindAddr ( iaddr: NetAddr.T );
 var AddrStr : string;
 begin
 if iaddr.isNil then raise Exception.Create('Unknown address');
 iaddr.ToString(AddrStr);
 IndexName:= 'peersaddr';
 if not SearchKey ( AddrStr, Dbf_common.stEqual ) then raise Exception.Create('Unknown address');
end;


var AkafukaDB :tAkafukaDB;

procedure Select( ID :tID );
 {- All addresses in the db were available at last Akafuka}
 var saddr,sid :string;
 begin
 if IsSelectedAddr and IsSelectedID and (SelectedID = ID) then exit;
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
 IsSelectedID:=true;
 IsSelectedAddr:=true;
 //SelectedDelta:=cur.akafuka.Delta;
 SelectedAddr.ToString(saddr);
 log.msg('Selected '+sid+' ('+saddr+')');
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
var cur: System.tDateTime;
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
 result := 0;
end;

procedure tPacket.ResetTimeSince;
 begin
 (*
 last.Init( Sender );
 try last.Store(pktype, Now);
 finally last.Done; end;
 *)
end;

{*********************************
 *********** Akafuka   ***********
 *********************************}

procedure tPacket.Handle;
 var saddr,sid :string;
 begin
 {
 SelectedID:=Sender;
 isSelectedID:=true;
 }
 { addr shouldbe selected by Daemon }
 assert( IsSelectedAddr );

 {Lookup sender ID from database}
 try
  AkafukaDB.FindAddr( SelectedAddr );
 except
  on Exception do raise Exception.Create('Sender address not in database');
 end;
 SelectedID.FromString(AkafukaDB.FieldByName('id').AsString);

 SelectedAddr.ToString(saddr);
 SelectedID.ToString(sid);
 log.msg('Received #'+IntToStr(pktype)+' From '+sid+' ('+saddr+')');
 log.msg('Last was '+TimeToStr(TimeSince)+'('+FloatToStr(TimeSince*SecsPerDay)+'s) ago');
end;

procedure tAkafuka.Send;
 var r:tRecord;
begin
 log.msg('Sending Akafuka');
 {Remove selected addr from db and append it to akafuka db}
 //C.sock:=SelectedAddr;
 { Search for the address }
 try
  AkafukaDB.FindAddr( SelectedAddr );
 except
  on Exception do begin
   AkafukaDB.Append;
   SelectedID.ToString(AkafukaDB.FieldByName('id').AsString);
   SelectedAddr.ToString(AkafukaDB.FieldByName('addr').AsString);
   AkafukaDB.FieldByName('delta').AsInteger:=0;
   AkafukaDB.FieldByName('retry').AsInteger:=0;
  end;
 end;
 AkafukaDB.FieldByName('since'):=Now;
 AkafukaDB.Post;
 log.msg('Akafuka info: Retry='+IntToStr(AkafukaDB.FieldByName('retry'))+' Delta='+FloatToStr(AkafukaDB.FieldByName('delta')*SecsPerDay)+'s Since=now');
 inherited Send(sizeof(SELF) - (Sizeof(YouSock)-YouSock.Length) );
end;

procedure SaveReportedAddr( const Addr: NetAddr.t );
 var C: tAddrInfo;
 var str:string;
 var r:tRecord;
 begin
 Addr.ToString(str);
 log.msg('Sender Reported our Address '+str);
 (*
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
 *)
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
var i:tRecord;
 var isNew :boolean=false;
begin
 inherited Handle;
 log.msg('Received '+cFundelukaN);
 { The assoc re-adds peer to db. Now remove it from akafuka db}
 (*
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
 *)
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
 var row:DataBase.tRow;
 var id:tID;
 var C:tAddrInfo;
 var r:tRecord;
 var akafuka:tAkafuka;
 var str:string;
 begin
 (*
 list.init(cTable);
 repeat
  try
   list.Read(row);
  except
   on eRangeError do break;
  end;
  if row='tags' then continue;
  id.FromString(row);
  if id.isNil then continue;
  //log.msg('doing '+row);
  SelectedID:=id;
  isSelectedId:=true;
  db.init(id);
  r:=0;
  repeat
   try
    db.Read(C, R );
   except
    on eRangeError do break;
   end;
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
   SelectedAddr:=C.sock;
   isSelectedAddr:=true;
   Akafuka.Create;
   Akafuka.Send;
   inc(R);
  until false;
  db.done;
 until false;
 list.done;
 *)
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

procedure SelfTest;
 begin
 AbstractError;
end;

INITIALIZATION
 AkafukaDB:=tAkafukaDB.Create;
FINALIZATION
 AkafukaDB.Destroy;
END.
