unit Peers;

INTERFACE
uses GeneralPacket
    ,Keys
    ,Sockets
    ,UnixType
    ;

const cReq = 1;
const cAns = 2;
const cHelloCooldown = 5000{ms};
const pktype :set of tPkType = [cReq, cAns];

{
 Send hello ...> get hello
 
 This unit handle only incoming hello and does not initiate greeting 
 automatically.
 
}

type
 T =packed object(GeneralPacket.T)
  procedure Handle;
  procedure Send;
  procedure Create ( isReply :boolean );
  private
  Fpr :keys.tFingerprint;
  Load: byte; //Let it be byte. No byte-orde as in Single
  YouSock :Peers.tNetAddrLargest;
 end;

procedure Assoc( fpr :keys.tFingerprint );
(* Associate sender with fpr *)
 unimplemented;

procedure Save( really{?} :boolean );
(* Save the currently selected peer's info *)
 unimplemented;

function TimeSinceLast( pktype :GeneralPacket.tPkType ): System.tTime;
(* returns time since last packet from the peer of that type arrived *)
 unimplemented;

function Select( fpr :keys.tFingerprint );

TYPE
 tNetAddrLargest=object
  function Length :word;
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

IMPLEMENTATION

procedure Assoc( fpr :keys.tFingerprint );
begin
end;

procedure Save( really{?} :boolean );
begin
end;

function TimeSinceLast( pktype :GeneralPacket.tPkType ): System.tTime;
begin
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

procedure T.Create(isReply:boolean);
var a:byte;
begin
 if isReply
  then inherited Create(Hello.cAns)
  else inherited Create(Hello.cReq);
 Fpr:=MyFingerPrint;
end;

procedure T.Send;
begin
 inherited Repl(sizeof(self));
end;

END.
