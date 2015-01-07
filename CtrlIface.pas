unit CtrlIface;

INTERFACE

const
 {ctrl commands:}
 ccQuit=ord('q');
 ccTerminate=ord('T');
 ccPeerStates=ord('p');
 ccAddPeer=14; {netaddr.t}
 {peers by keys}
 ccGetPeersByMasterID=16; {masterkey >#PeerKeyIds}
 ccGetPeerInfoByID=17;    {subkey    >#PeerKeyInfo}
 {
 cc=;
 }

const
 {ctrl events:}
 ceQuit:byte=15;
 cePeerState:byte=ord('p'); {event:byte, peers.tinfo}
 cePeerKeyIds:byte=17; {count, array count of subkey }
 cePeerKeyInfo:byte=18; { subkey, addr, ping_ms }
 {
 ce:byte=;
 }

IMPLEMENTATION

END.
