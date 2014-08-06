unit HiHello;

INTERFACE
uses GeneralPacket
    ,Keys
    ,Peers
    ;

const pktype=2;

type
 tPacket =packed object(GeneralPacket.T)
  procedure Handle;
  procedure Send;
  procedure Create;
  private
  Fingerprint :keys.tFingerprint; //Who's reporting?
  SockAddr { of our peer} :Peers.tNetAddrLargest;
  (*Warning: Dynamic length, do no create descendants! *)
 end;

IMPLEMENTATION
uses Peers
    ;

PROCEDURE tPacket.Handle;
 unimplemented {
  This coudl lead into DDoS: Eve sends us a Hi with SockAddr of the victim. 
  We assume the address is our and distribute it to our peers. They can 
  then try to contact us on that addres, sending spammy traffic to victim.
  
 };
begin
 Peers.Assoc (Fingerprint); {Associate sender's addres with fingerprint.}
 Peers.AddOur (Sockaddr);
end;

procedure tPacket.Create;
begin
 inherited Create(HiHello.pktype);
 Fingerprint:=MyFingerPrint;
 Peers.Get(Sockaddr);
end;

procedure tPacketSend;
var OurSize :word = sizeof(self)-sizeof(Sockaddr);
begin
 inc(OurSize, Sockaddr.GetLength);
 inherited Send(sizeof(self)-sizeof(Sockaddr)+Sockaddr.GetLength);
end;

END.


IM

