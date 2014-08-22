unit Hello;

INTERFACE
uses GeneralPacket
    ,Keys
    ,Peers
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
  YouSock :Peers.tNetAddrLargest;
 end;

IMPLEMENTATION

procedure T.Handle;
var rep:Hello.T;
begin
 Peers.Assoc (Fpr); {Associate sender's sockaddr with fingerprint.}
 Peers.Save (true); {Save the peer socaddr to permanent peer cache}
 if (pktype=cReq ) and (Peers.TimeSinceLast(cReq) > cHelloCooldown)
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
