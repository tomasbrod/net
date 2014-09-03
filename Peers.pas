unit Peers;

INTERFACE
uses GeneralPacket
    ,Keys
    ,Sockets
    ,UnixType
    ;

const cAkafuka :tPkType = 1;
const cAkafukaN :string='Akafuka'  experimental;
const cFundeluka :tPkType = 2;
const cFundelukaN :string = 'Fundeluka'  experimental;
//const PkAccept :set of tPkType = [cAkafuka, cFundeluka];

const cHelloCooldown = 5000{ms};

TYPE

 tNetAddr=object
  function Length :word;
  procedure Selected;
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
  procedure Selected; {Creates a peer id of currently selected peer}
  procedure GetMy;
 end;
  

 tAkafuka =packed object(GeneralPacket.T)
  procedure Handle;
  procedure Send;
  constructor Create(rcpt: tID); {akafuka}
  constructor Create; {fundeluka}
  private
  ID :tID;
  Load: byte; //Let it be byte. No byte-orde as in Single
  YouSock :Peers.tNetAddr;
 end;
 tFundeluka=tAkafuka;
 

function TimeSinceLast( pktype :GeneralPacket.tPkType ): System.tTime;
(* returns time since last packet from the peer of that type arrived *)
 unimplemented;

procedure Select( ID :tID );

IMPLEMENTATION
uses 
     DataBase
     ,SysUtils
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

procedure Assoc( id: tID );
(* Associate Selected with fpr *)
 experimental;
var AddrF : file of tNetAddr;
var Ex, Nw : tNetAddr;
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
(* Save the currently selected peer's info *)
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
 Peers.Save (true); {Save the peer socaddr to permanent peer cache}
 if (pktype=cAkafuka ) and (Peers.TimeSinceLast(cAkafuka) > cHelloCooldown) then begin
  rep.Create;
  rep.Send;
 end;
end;

constructor tAkafuka.Create;
begin
 inherited Create(cFundeluka);
 ID.GetMy;
end;

constructor tAkafuka.Create(rcpt: tID);
var a:byte;
begin
 inherited Create(cAkafuka);
 ID:=rcpt;
end;

procedure tAkafuka.Send;
begin
 Repl(sizeof(self));
end;

END.
