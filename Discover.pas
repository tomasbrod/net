program Discover;

uses Peer,Packet;

CONST
 CHowMany=1;

VAR
 p:Peer.TRef;
 pkt:Packet.TAskPeers;
 i:integer;

BEGIN
 for i:=0 to CHowMany do begin
  Peers.GetRandom(p);
  pkt.Create;
  pkt.Send(p);
 end;
END.
