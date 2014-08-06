unit Hello;

INTERFACE
uses GeneralPacket
    ,Keys
    ;

const pktype=1;

type
 tPacket =packed object(GeneralPacket.T)
  procedure Handle;
  procedure Send;
  procedure Create;
  private
  Fingerprint :keys.tFingerprint;
 end;

IMPLEMENTATION
uses Peers
    ,HiHello
    ;

procedure tPacket.Handle;
var repl:HiHello.tPacket;
begin
 if Peers.TimeSinceLast < cHelloCooldown
  then exit; //Anti-DoS
 Peers.Assoc (Fingerprint); {Associate sender's addres with fingerprint.}
 Peers.Save (true);
 repl.Create;
 repl.Send;
end;

procedure tPacket.Create;
var a:byte;
begin
 inherited Create(Hello.pktype);
 Fingerprint:=MyFingerPrint;
end;

procedure tPacketSend;
begin
 Send(sizeof(self));
end;

END.


IM
