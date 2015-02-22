unit Discovery;

(* Objectives:

 Enable discovery of nodes.
 
 Discovery on:
 
  * the local network (LAN broadcast).
  * the mesh network (WAND)
  * the Internet (HTTP hostlist)
 
 Mechanism uses will be choosen by random until better method is found.
 
 This functionality could be dedicated to Neighb. But Neighb is not request 
 based and discovery should be.

*)

INTERFACE
uses Peers, NetAddr;

procedure NotifyIdle;

const cOptimalPeerCountDefault=6 experimental;
var cOptimalPeerCount: Word =cOptimalPeerCountDefault;

const cRequest=8;
type tRequest=object(tPacket)
 count:byte;
end;

const cOffer=9;
type tOffer=object(tPacket);
 NodeAddr: NetAddr.t;
 {***dynamic length***}
end;
const cFeedback=10 unimplemented;

IMPLEMENTATION

END.