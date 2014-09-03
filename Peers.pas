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
  procedure GetSender; {Creates from sender of the message}
  private
  data :record
  case family :word of 
   AF_INET :( inet :record 
     sin_port: cushort;
     sin_addr: in_addr;
   end; );
   0 :(
    pad_pV4IlkA4mKQL :array [0..128] of byte;
   ); 
  end;
 end;
 
 tID=object(Keys.tHash)
  procedure GetSelected; {Creates a peer id of currently selected peer}
 end;
  

 tAkafuka =packed object(GeneralPacket.T)
  procedure Handle;
  procedure Send;
  procedure Create(rcpt: tID);
  private
  ID :tID;
  Load: byte; //Let it be byte. No byte-orde as in Single
  YouSock :Peers.tNetAddr;
 end;

 tFundeluka =packed object(tAkafuka)
  procedure Create;
 end;

function TimeSinceLast( pktype :GeneralPacket.tPkType ): System.tTime;
(* returns time since last packet from the peer of that type arrived *)
 unimplemented;

procedure Select( fpr :keys.tFingerprint );

IMPLEMENTATION
uses DataBase;

procedure OpenDB(var F: File; const id :tID; const Field :tField);
const cTable :tTable = 'peers';
var row: tRow;
begin
 id.ToString(row);
 DataBase.Open(F, cTable, row, Field);
end;

Operator = (aa, ab :Sockets.sin_addr) b : boolean;
begin
 b:=aa.s_addr=ab.s_addr;
end;

Operator = (aa, ab :tNetAddr) b : boolean;
begin
 b:=false;
 if aa.Family<>ab.Family then exit;
 case aa.Familly of
  Sockets.AF_INET: if aa.sin_port<>ab.sin_port or aa.sin_addr<>ab.sin_addr then exit;
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
(* Associate sender with fpr *)
 experimental;
var AddrF : file of tNetAddr;
var Ex, Nw : tNetAddr;
label already;
begin
 Nw.GetSender;
 OpenDB( AddrF, id, cAddrField);
 try
  while not eof(AddrF) do begin
   Read(AddrF, Ex);
   if Ex=Nw then exit;
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
begin
 OpenDB( AddrF, id, cAddrField);
 try
  while not EoF(AddrF) do begin
   Read(F, Ex);

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
 id.GetSelected;
 OpenDB( F, id, cLastField);
 try
  Seek(F, pktype);
  Read(F, cur);
 finally
  close(AddrF);
 end;
 result := now - cur;
end;

procedure T.Handle;
var rep:Hello.T;
begin
 Peers.Assoc (Fpr); {Associate sender's sockaddr with fingerprint.}
 Peers.Save (true); {Save the peer socaddr to permanent peer cache}
 if (pktype=cReq ) and (Peers.TimeSinceLast(cReq) < cHelloCooldown)
  then exit; //Anti-DoS
 rep.Create(true);
 rep.Send;
end;

procedure tFundeluka.Create;
begin
 inherited Create(cFundeluka);
 ID:=MyID;
end;

procedure tAkafuka.Create(rcpt: tID);
var a:byte;
begin
 inherited Create(cAkafuka)
 ID:=rcpt;
end;

procedure tAkafuka.Send;
begin
 Repl(sizeof(self));
end;

END.
