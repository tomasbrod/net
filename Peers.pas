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

type
 tPktype =byte;
 tPacket = packed object
  { Base object for all packets }
  pktype :tPktype;
  procedure Create ( const itp :tPktype );
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t; Len:LongInt ); overload;
 end;

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
 
type tInfo= record
  Addr: NetAddr.t;
  Delta: tTime;
 end;
procedure Get( out info:tInfo; const addr:netaddr.t ); overload;
procedure Add( addr :NetAddr.T );
 
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
end;

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
 var entry:tPendingList;
 begin
 entry:=tPendingList.Create;
 with entry do begin
  CopyFrom(rcpt);
  addr:=rcpt;
  since:=now;
  retry:=0;
  delta:=0;
 end;
 PendingList.Insert(entry);


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

procedure tFundeluka.Handle( const from: NetAddr.t);
 { Move from pending to roster and update delta. }
 var isnew:boolean;
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
 isnew:= AkafukaDB.Delta=cAkafukaUnknown;
 AkafukaDB.Delta:=Now - AkafukaDB.Since;
 log.msg('Akafuka: '+FloatToStr(AkafukaDB.Delta*SecsPerDay)+'s after '+IntToStr(AkafukaDB.Retry)+' retries');
 AkafukaDB.Since:=Now;
 AkafukaDB.Retry:=0;
 AkafukaDB.Post;

 if AkafukaDB.Delta > cAkafukaMaxDelta then begin
  log.msg('Akafuka Delta too high');
  AkafukaDB.Delete;
  if assigned(DisappearProc) then DisappearProc( AkafukaDB.ID );
 end else
  if isnew and assigned(AppearProc) then AppearProc( SenderID );

 SaveReportedAddr( YouSock );
end;

procedure DoAkafuka;
 { Retry pending. Check roster for old peers and send them akafuka. }
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
    if assigned(DisappearProc) then DisappearProc( AkafukaDB.ID );
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
 { Send akafuka }
 begin
 SelectedID.Clear;
 SelectedAddr:=addr;
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
 AkafukaDB.Since:=0;
 SelectedID:=AkafukaDB.ID;
 SelectedDelta:=AkafukaDB.Delta;
 AkafukaDB.Post;
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

procedure tPacket.Send( const rcpt:netaddr.t; Len:LongInt);
 var saddr,sid :string;
 begin
 SelectedAddr.ToString(saddr);
 SelectedID.ToString(sid);
 log.msg('Sending #'+IntToStr(pktype)+' to '+sid+' ('+saddr+')');
 Assert( SendProc <> nil );
 SendProc( self, Len );
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
 PeersList:=tPeersList.CreateRoot;
 PendingList:=tPendingList.CreateRoot;
FINALIZATION
 PendingList.Free;
 PeersList.Free;
END.
