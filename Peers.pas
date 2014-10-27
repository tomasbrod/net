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
const cAkafukaRetry = 3{times};
const cAkafukaMaxDelta = 600000{ms} /MSecsPerDay; {10 minutes to ping? Heh!}
const cAkafukaPeriod = 5000{ms} /MSecsPerDay;
const cAkafukaUnknown  = 10;

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
 end;

 tAkafuka =packed object(tPacket)
 {
  A Ping-Pong packet.
  Name is reference to Zidan (Dávid) Sufusky Sufurky
 }

  procedure Handle;
  { Executes approportiate actions to respond to this packet }

  procedure Send; overload;
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

 eUnknownSender = class(Exception)
 {
  Exception signaling that sender is not in database
 }
  Addr : NetAddr.t; { id of erroring peer }
  constructor Create( iAddr : NetAddr.t );
 end;
 
var
 ThisID :tID;
 SelectedID : tID;
 SelectedAddr :NetAddr.T; {$HINT Should Move SelectedAddr to SocketUtil?}
 SelectedDelta :System.tTime;
 SendProc: procedure(var Data; Len:LongInt);
 NewProc :procedure( id :tID );

procedure Select( ID :tID );
{ Selects peer with given ID and automatically picks an sockaddr }

procedure DoAkafuka;
{
 Send Akafuke to all peers.
 Remove not responding peers.
}

procedure Add( addr :NetAddr.T );
{ Add peer only known by its address. }

procedure Reset;

procedure SelfTest;
 platform;

IMPLEMENTATION
uses 
     DataBase
    ,db
    ,Log
    ,Dbf_Common
    ,Dbf
    ,Variants
    ;

{*********************************
 *********** Addresses *********** 
 *********************************}

type tAkafukaDB = class (DataBase.tDbDataSet)
 private procedure HandleAfterScroll( DataSet: TDataSet );
 public
 constructor Create; {override;}overload;
 procedure Post; override; //To save changes
 public
 ID: Peers.tID;
 Addr: NetAddr.T;
 Delta: System.tTime;
 Since: tDateTime;
 Retry: Integer;
 procedure FindAddr ( iaddr: NetAddr.T );
 procedure FindID ( aID: tID );
end;

constructor tAkafukaDB.Create;
 begin
 inherited Create( nil );
 AfterScroll:=@HandleAfterScroll;
   OpenMode:=omAutoCreate;
   FieldDefs.Add( {0} 'id',    ftString,  40, True  );
   FieldDefs.Add( {1} 'addr',  ftString,  50, True  );
   FieldDefs.Add( {2} 'delta', ftFloat, 0, True );
   FieldDefs.Add( {3} 'since', ftFloat, 0, True );
   FieldDefs.Add( {4} 'retry', ftSmallInt, 0, True );
   with IndexDefs.Add do begin Name:='addr'; Expression:=Name; Options:=[ixCaseInsensitive]; end;
   {
   with IndexDefs.Add do begin Name:='retry'; Expression:=Name; Options:=[ixDescending]; end;
   }
   with IndexDefs.Add do begin Name:='since'; Expression:=Name; Options:=[]; end;
   with IndexDefs.Add do begin Name:='select'; Expression:='id+STR(delta,16,14)'; Options:=[]; end;
 Open ('peers');
end;

procedure tAkafukaDB.HandleAfterScroll( DataSet: TDataSet );
 begin
 tHash(ID):=Fields[0].AsString;
 Addr:=Fields[1].AsString;
 Delta:=Fields[2].AsFloat;
 Since:=Fields[3].AsFloat;
 Retry:=Fields[4].AsInteger;
end;
  
procedure tAkafukaDB.Post;
 begin
 Fields[0].AsString:=String(tHash(ID));
 Fields[1].AsString:=String(Addr);
 Fields[2].AsFloat:=Delta;
 Fields[3].AsFloat:=Since;
 Fields[4].AsInteger:=Retry;
 inherited Post;
end;

procedure tAkafukaDB.FindAddr ( iaddr: NetAddr.T );
 var AddrStr : string;
 begin
 assert( not iaddr.isNil);
 if iaddr = addr then exit;
 iaddr.ToString(AddrStr);
 if not Locate ( 'addr', AddrStr, [] ) then raise DataBase.eSearch.Create;
end;

procedure tAkafukaDB.FindID ( aID: tID );
 begin
 IndexName:='select';
 SetRange( String(tHash(aID)), String(tHash(aID))+'99999999.99999999');
 {
 First; while not EOF do begin
  log.msg('-- '+String(tHash(aID))+ FloatToStr( Delta ));
  Next;
 end;
 }
 First;
 if EOF then raise DataBase.eSearch.Create;
end;

var AkafukaDB :tAkafukaDB;

procedure Select( ID :tID );
 {- All addresses in the db were available at last Akafuka}
 var saddr,sid :string;
 begin
 if (not SelectedAddr.isNil) and (not SelectedID.isNil) and (SelectedID = ID) then exit;
 ID.ToString(sid);
 log.msg('Select '+sid);
 AkafukaDB.FindID( ID );
 SelectedID:=AkafukaDB.ID;
 SelectedAddr:=AkafukaDB.Addr;
 SelectedDelta:=AkafukaDB.Delta;
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
  on DataBase.eSearch do raise eUnknownSender.Create(SelectedAddr);
  on Exception do begin log.msg('While identifiing sender'); raise; end;
 end;
 {Initialize state}
 SelectedID:=AkafukaDB.ID;
 SelectedAddr:=AkafukaDB.Addr;
 SelectedDelta:=AkafukaDB.Delta;
 log.msg('Received #'+IntToStr(pktype)+' From '+String(tHash(SelectedID))+' ('+String(SelectedAddr)+')');
 {
 log.msg('Last was '+TimeToStr(AkafukaDB.Since)+'('+FloatToStr(AkafukaDB.Since*SecsPerDay)+'s) ago');
 }
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
 //log.msg('Sending Akafuka');
 SenderID:=ThisID;
 YouSock:=SelectedAddr;
 AkafukaDB.Edit;
 AkafukaDB.Retry:=AkafukaDB.Retry+1;
 AkafukaDB.Since:=Now;
 AkafukaDB.Post;
 //log.msg('Akafuka info: Retry='+IntToStr(AkafukaDB.Retry)+' Delta='+FloatToStr(AkafukaDB.Delta*SecsPerDay)+'s');
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

procedure tAkafuka.Handle;
var fundeluka:tFundeluka;
var isNew: boolean;
begin
 //log.msg('Received '+cAkafukaN);
 try
  inherited Handle;
  isNew:=false;
 except on eUnknownSender do begin
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
  fundeluka.Create;
  fundeluka.Send;
 end else log.msg('Anti-DDoS, not sending '+cFundelukaN);
 SaveReportedAddr( YouSock );
end;

procedure tFundeluka.Handle;
 begin
 //log.msg('Received '+cFundelukaN);
 inherited Handle; { this also rejects unknown or invalid senders and leaves db at sender addr }
 AkafukaDB.Edit;

 if SelectedID.isNil then begin
  SelectedID:=SenderID;
  AkafukaDB.ID:=SelectedID;
 end else
  if SelectedID<>SenderID then raise eInvalidInsert.Create('Invalid Sender ID');

 { Calculate delta, reset timestamp }
 AkafukaDB.Delta:=Now - AkafukaDB.Since;
 log.msg('Akafuka: '+FloatToStr(AkafukaDB.Delta*SecsPerDay)+'s after '+IntToStr(AkafukaDB.Retry)+' retries');
 AkafukaDB.Since:=Now;
 AkafukaDB.Retry:=0;
 AkafukaDB.Post;

 if AkafukaDB.Delta > cAkafukaMaxDelta then begin
  log.msg('Akafuka Delta too high');
  AkafukaDB.Delete;
 end;

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
 log.msg('DoAkafuka on '+IntToStr(AkafukaDB.ExactRecordCount)+' records');
 try
  {Try sending akafuka again}
  while true do begin
   AkafukaDB.IndexName:='since';
   AkafukaDB.SetRange( 0, Now-cAkafukaPeriod );
   if AkafukaDB.EOF then break;
   if AkafukaDB.Retry>=cAkafukaRetry then begin
    {Lets delete timeouted peers. But do not delete before period}
    log.msg('Akafuka timeout: '+String(tHash(AkafukaDB.ID))+' '+String(AkafukaDB.Addr));
    AkafukaDB.Delete;
   end else begin
    Akafuka.Create;
    SelectedID:=AkafukaDB.ID;
    SelectedAddr:=AkafukaDB.Addr;
    log.msg('Akafuka Retry: '+String(tHash(AkafukaDB.ID))+' '+String(AkafukaDB.Addr)+' r='+IntToStr(AkafukaDB.Retry));
    Akafuka.Send;
   end;
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
 try
  AkafukaDB.FindAddr( SelectedAddr );
  AkafukaDB.Edit;
  SelectedID:=AkafukaDB.ID;
 except
  on DataBase.eSearch do begin
   AkafukaDB.Append;
   AkafukaDB.ID:=SelectedID;
   AkafukaDB.Addr:=SelectedAddr;
   AkafukaDB.Delta:=cAkafukaUnknown;
   AkafukaDB.Retry:=0;
  end;
 end;
 AkafukaDB.Post;
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
 //log.msg('Sending '+cFundelukaN);
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

constructor eUnknownSender.Create( iAddr : NetAddr.t );
 begin
 inherited Create( 'Unknown sender :'+String(iAddr));
 Addr:=iAddr;
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
 AkafukaDB.Active:=False;
 AkafukaDB.Free;
END.
