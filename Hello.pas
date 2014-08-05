unit Hello;

(*
   Hello packets are broadcasted ower LAN every cBroadcastPeriod ms. When 
   Hello packet arrives, the sender is added to list of Peers. There is no 
   more to it.
   
   Also, if the Hello packet is recieved in cHelloCooldown ms since last 
   Hello with the same Fingerprint, it is discarded.
      
*)

INTERFACE
uses GeneralPacket
    ,Keys
    ;

const pktype=1;

type
 tPacket =packed object(GeneralPacket.T)
  procedure Handle;
  {
  procedure Send;
  }
  procedure Create;
  private
  Fingerprint :keys.tFingerprint;
 end; {+1=?}

IMPLEMENTATION
uses Peers
    ;

procedure tPacket.Handle;
begin
 if Peers.TimeSinceLast(Hello.pktype) < cHelloCooldown
  then exit; //Anti-DoS
 Peers.Add (Fingerprint);
end;

procedure tPacket.Create;
var a:byte;
begin
 inherited Create;
 Fingerprint:=MyFingerPrint;
 self.pktype:=Hello.pktype;
end;

END.


IM
