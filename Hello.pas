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
  procedure Create;
  private
  Fpr :keys.tFingerprint;
  YouSock :Peers.tNetAddrLargest;
 end;
 tReply= packed object(T)
  procedure Create;
 end;

IMPLEMENTATION
uses Peers
    ;

procedure T.Handle;
var rep:Hello.tReply;
begin
 Peers.Assoc (Fpr); {Associate sender's sockaddr with fingerprint.}
 Peers.Save (true); {Save the peer socaddr to permanent peer cache}
 if pktype=cReq and (Peers.TimeSinceLast(cReq) > cHelloCooldown)
  then exit; //Anti-DoS
 rep.Create;
 rep.Send;
end;

procedure T.Create;
begin
 inherited Create(Hello.cReq);
 Fpr:=MyFingerPrint;
end;

procedure tReply.Create;
begin
 inherited Create(Hello.cAns)
 Fpr:=MyFingerPrint;
end;

procedure T.Send;
begin
 inherited Send(sizeof(self));
end;

END.


IM