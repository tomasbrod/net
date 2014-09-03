unit Peers;

INTERFACE
uses GeneralPacket
    ,Keys
    ,Sockets
    ,UnixType
    ,SysUtils
    ;

const cAkafuka :tPkType = 1;
const cAkafukaN :string='Akafuka'  experimental;
const cFundeluka :tPkType = 2;
const cFundelukaN :string = 'Fundeluka'  experimental;
//const PkAccept :set of tPkType = [cAkafuka, cFundeluka];

const cHelloCooldown = 5000{ms};

TYPE

 tNetAddr=object
 { Object to store Socket Address }

  function Length :word;
   unimplemented;
  { Returns length of the structure based on address family }

  procedure Selected;
  { load currently selected address }

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
  { Computes length of the packet and calls send }

  constructor Create(rcpt: tID); {akafuka}
  { Creates an akafuka packet }

  constructor Create; {fundeluka}
  { Creates fundeluka packet }

  private
  ID :tID; { of sender }
  Load: byte unimplemented; { load of the sender's system }
  YouSock :Peers.tNetAddr; { address, the packet was sent to }
 end;

 tFundeluka=tAkafuka;
 
 eNoAddress = class(Exception)
 {
  Exception signaling that peer has no associated sockaddr.
  Thus no packet can be sent.
 }
  ID : tID; { id of erroring peer }
  constructor Create( iid :tID );
 end;

var ThisID :tID;

function TimeSinceLast( pktype :GeneralPacket.tPkType ): System.tTime;
{ returns time since last packet from the peer of that type arrived }
 unimplemented;

procedure Select( ID :tID );
{ Selects peer with given ID and automatically picks an sockaddr }

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
  else exit;
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

procedure Assoc( const nw: tNetAddr; const id: tID );
 experimental;
var AddrF : file of tNetAddr;
var Ex : tNetAddr;
begin
 Nw.Selected;
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
    if EoF(AddrF) then begin {offending record is the last}
     Seek(AddrF, nwpos);
     Truncate(AddrF);
    end else begin
     Seek(AddrF, FileSize(AddrF)-1);
     Read(AddrF, Ex);
     Seek(AddrF, FileSize(AddrF)-1);
     Truncate(AddrF);
     Seek(AddrF, nwpos);
     Write(AddrF, nw);
    end;
    {Do not break; even if there should be no more, but just in case} continue;
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

const cLastField :DataBase.tField = 'last';

function TimeSinceLast( pktype :GeneralPacket.tPkType ): System.tTime;
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

procedure tAkafuka.Handle;
var rep:tFundeluka;
begin
 Peers.Assoc (ID); {Associate sender's sockaddr with fingerprint.}
 Assoc(YouSock, ThisID); {Associate reported }
 Peers.Save (true); {Save the peer socaddr to permanent peer cache}
 if (pktype=cAkafuka ) and (Peers.TimeSinceLast(cAkafuka) > cHelloCooldown) then begin
  rep.Create;
  rep.Send;
 end;
end;

constructor tAkafuka.Create;
begin
 inherited Create(cFundeluka);
 ID:=ThisID;
 YouSock.Selected;
end;

constructor tAkafuka.Create(rcpt: tID);
var a:byte;
begin
 inherited Create(cAkafuka);
 ID:=rcpt;
 YouSock.Selected;
end;

procedure tAkafuka.Send;
begin
 Repl(sizeof(self));
end;

var SelectedID : tID unimplemented;
var SelectedAddr :tNetAddr unimplemented;

procedure Select( ID :tID );  unimplemented;
begin
 AbstractError;
 raise eNoAddress.Create( id );
end;

function tNetAddr.Length :Word;
begin
 result:=sizeof(self);
end;

procedure tNetAddr.Selected;
begin
 self:=SelectedAddr;
end;

procedure tID.Selected;
begin
 self:=SelectedID;
end;

constructor eNoAddress.Create( iid :tID );
var idstr:string;
begin
 iid.ToString(idstr);
 inherited Create( 'No Address associated to Peer '+idstr );
 id:=iid;
end;

END.
