unit CtrlIface;

INTERFACE
uses NetAddr,Neighb;

const
 {ctrl commands:}
 ccQuit=ord('q');
 ccTerminate=ord('T');
 ccPeerStates=ord('p');
 ccAddPeer=14;          { netaddr.t }
 {transfer}
 ccTransferRequest=19;  { ID, NetAddr }
 ccTransferAbort=20;    { ID }
 ccTransferProgress=21; { ID }
 ccTransferListAll=22;
 {neighbours}
 ccGetNeighb=15;        {          >#Neighbs}
 ccGetNeighbPID=16;     {key       >#Neighbs}
 ccGetNeighbAddr=17;    {netaddr   >#Neighbs}
 {
 cc=;
 }

const
 {ctrl events:}
 ceQuit=15;
 cePeerState=ord('p'); {event:byte, addr, ping_ms}
 ceInvalid=ord('?');
 {transfer}
 ceTransfer=20; { ID, done, total }
 {neighbours}
 ceNeighbs=17; { [tNeighbInfo], byte(0) }
 ceNeighbState=18; { tNeighbInfo }
type
 tNeighbInfo=record
  pid:neighb.tpid;
  addr:netaddr.t;
  hop:word2;
 end;
 {
 ce:byte=;
 }

IMPLEMENTATION

END.
