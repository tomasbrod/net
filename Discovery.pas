unit Discovery;

(* Objectives:

 Enable discovery of nodes.
 
 Discovery on:
 
  * the local network (LAN broadcast).
  * the mesh network (WAND)
  * the Internet (HTTP hostlist)
 
 This functionality could be dedicated to Neighb. But Neighb is not request 
 based and discovery should be.

*)

INTERFACE
uses Peers, NetAddr;

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

procedure NotifyIdle;

const cOptimalPeerCountDefault=6 experimental;
var cOptimalPeerCount:word=cOptimalPeerCountDefault;
