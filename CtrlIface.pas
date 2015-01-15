unit CtrlIface;

INTERFACE

const
 {ctrl commands:}
 ccQuit=ord('q');
 ccTerminate=ord('T');
 ccPeerStates=ord('p');
 ccAddPeer=14; {netaddr.t}
 {peers by keys (unimplemented)}
 ccGetAllPeers=15;        {          >#PeerKeyIds}
 ccGetPeersByMasterID=16; {masterkey >#PeerKeyIds}
 ccGetPeerInfoByID=17;    {subkey    >#PeerInfo}
 ccGetPeerInfoByAddr=17;  {netaddr   >#PeerInfo}
 {transfer}
 ccTransferRequest=19;  { ID, NetAddr }
 ccTransferAbort=20;    { ID }
 ccTransferProgress=21; { ID }
 ccTransferListAll=22;
 {
 cc=;
 }

const
 {ctrl events:}
 ceQuit=15;
 cePeerState=ord('p'); {event:byte, addr, ping_ms}
 ceInvalid=ord('?');
 {keys}
 cePeerKeyIds=17; {count, array count of subkey }
 cePeerInfo=18; { subkey, masterkey, addr, ping_ms }
 {transfer}
 ceTransfer=20; { ID, done, total }
 {
 ce:byte=;
 }

IMPLEMENTATION

END.
