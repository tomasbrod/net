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

 tNetAddr=object
 { Object to store Socket Address }

  function Length :word;
   unimplemented;
  { Returns length of the structure based on address family }

  procedure Selected;
  { load currently selected address }
  
  procedure ToSocket( var sockaddr :tSockAddr; const socklen :tSockLen);
   unimplemented;

  private
  data :record
  case Family :word of 
   AF_INET :( inet :record 
    sin_port: cushort;
    sin_addr: tInAddr;
   end; );
   0 :(
    pad_pV4IlkA4mKQL :array [0..128] of byte;
   ); 
  end;
 end;
 
 tID=object(Keys.tHash)
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

 tFundeluka=object(tAkafuka)

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

procedure OpenDB(var F: File; const id :tID; const Field :tField);
const cTable :tTable = 'peers';
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


const cAddrField :tField = 'addr';

type tAddr= object (tNetAddr)
 function TimeSinceLast :System.tTime;
 procedure SetNowLast;
 private
 Last :System.tDateTime;
end deprecated;

function tAddr.TimeSinceLast :System.tTime;
begin Result := Now - Last; end;
procedure tAddr.SetNowLast;
begin Last := Now; end;

{*********************************
 *********** Addresses *********** 
 *********************************}

procedure Assoc( const nw: tNetAddr; const id: tID );
 experimental;
var AddrF : file of tNetAddr;
var Ex : tNetAddr;
begin
 if nw.data.Family = 0 then exit; {Do not associate with nil address}
 OpenDB( AddrF, id, cAddrField);
 try
  while not eof(AddrF) do begin
   Read(AddrF, Ex);
   if Ex=Nw then begin
    Seek(AddrF, FilePos(AddrF)-1 );
    break;
   end;
  end;
  write(AddrF, Nw);
 finally
  close(AddrF);
 end;
end;

procedure Assoc( const nw: tNetAddr );
 unimplemented;
 {
  Search for a peer with the address and associate .
  Or send akafuka to get the id.
 }
begin AbstractError; end;

procedure Assoc( const id: tID );
var Nw : tNetAddr;
begin
 Nw.Selected;
 Assoc( nw, id );
end;

procedure Remove( const nw :tNetAddr );
 experimental;
var AddrF : file of tNetAddr;
var Ex : tNetAddr;
var nwpos: int64;
var ID : tID;
begin
 id.Selected;
 OpenDB( AddrF, id, cAddrField);
 try
  while not EoF(AddrF) do begin
   Read(AddrF, Ex);
   if Ex=Nw then begin
    nwpos:=FilePos(AddrF)-1; {Pos of the offending record}
    if not EoF(AddrF) then begin
     Seek(AddrF, FileSize(AddrF)-1);
     Read(AddrF, Ex);
     Seek(AddrF, nwpos);
     Write(AddrF, Ex);
    end;
    Seek(AddrF, FileSize(AddrF)-1);
    Truncate(AddrF);
    break;
   end;
  end {while};
 finally
  close(AddrF);
 end;
end;

procedure Save( really{?} :boolean );
{ Save the currently selected peer's info }
 deprecated;
begin
 AbstractError;
end;

procedure Select( ID :tID );
{- All addresses in the db were available at last Akafuka}
var F : file of tNetAddr;
var cur : tNetAddr;
begin
 if (IsSelectedAddr and IsSelectedID) and (SelectedID = ID) then exit;
 OpenDB( F, id, cAddrField);
 try
  if eof(F) then raise eNoAddress.Create(ID);
  while not eof(F) do begin
   Read(F, cur);
   if true then break;
   {$NOTE Selects only first address from the db.}
  end;
 finally
  close(F);
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

function TimeSince( pktype :GeneralPacket.tPkType ): System.tTime;
var F : file of System.tDateTime;
var cur: System.tDateTime;
var ID : tID;
begin
 id.Selected;
 OpenDB( F, id, cLastField);
 try
  Seek(F, pktype);
  Read(F, cur);
 finally
  close(F);
 end;
 result := now - cur;
end;

procedure ResetTimeSince( pktype :GeneralPacket.tPkType );
var F : file of System.tDateTime;
var cur: System.tDateTime;
var ID : tID;
begin
 id.Selected;
 cur:=now;
 OpenDB( F, id, cLastField);
 try
  Seek(F, pktype);
  write(F, cur);
 finally
  close(F);
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

procedure tAkafuka.Send;
var F :file of tAkafukaProgress;
var C :tAkafukaProgress;
begin
 {$NOTE Do Not send Padding over the network}
 Repl(sizeof(self));
 {Remove selected addr from db and append it to akafuka db}
 C.Addr:=SelectedAddr;
 C.Since:=Now;
 C.Retry:=1;
 Remove(SelectedAddr);
 OpenDB(F, SelectedID, cAkafukaProgressField);
 try
  Seek(F, FileSize(F));
  Write(F, C);
 finally
  close(F);
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
var F :file of tAkafukaProgress;
var C, Ex :tAkafukaProgress;
var Delta :System.tTime;
var nwpos :int64;
begin
 Assoc (ID);
 Assoc (YouSock, ThisID); {associate reported address to us}
 { The assoc re-adds peer to db. Now remove it from akafuka db}
 OpenDB(F, SelectedID, cAkafukaProgressField);
 {$NOTE Implement Generic file delete.}
 try
  while not EoF(F) do begin
   Read(F, C);
   if C.Addr = SelectedAddr then begin
    Delta:= Now - C.Since;
    nwpos:=FilePos(F)-1; {Pos of the offending record}
    if not EoF(F) then begin
     Seek(F, FileSize(F)-1);
     Read(F, Ex);
     Seek(F, nwpos);
     Write(F, Ex);
    end;
    Seek(F, FileSize(F)-1);
    Truncate(F);
    break;
   end;
  end;
 finally
  close(F);
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
 {$NOTE Do Not send Padding over the network}
 Repl(sizeof(self));
end;

function tNetAddr.Length :Word;
begin
 result:=sizeof(self);
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
