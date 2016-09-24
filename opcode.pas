UNIT opcode;
INTERFACE

const {dgram opcode}
 {note: opcodes >=$80 are a reply and have transaction id so they can overlap}
 (*Object Transfer*)
 otCtrl=$09; {client->server}
 otData=$08; {server->client}
  (*Distributed Hash Table*)
  dhtBeatQ=$0A;
  dhtBeatR=$0B;
  dhtCheckQ=$0C;
  dhtCheckR=$0D;
  dhtCheckS=$0E;
  dhtResult=$0F;
  dhtGetNodes=$10;
  dhtTestQuery=$11;
  dhtTestResult=$91;
  dhtNodes=$90;
  (*Mutable/Profile*)
  mutableQuery=$12;
  mutableNotify=$13;
  (*Messages*)

const {chat init}
 upFileServer=2 deprecated;
 crAuthReq=3;
 testChat=32;
const (*ObjectTransfer magic numbers*)
 otSPEED=1; {control type speed report}
 otSIACK=2; {control type size increase ack}
 otFin=3;   {control type finish}
 otReq=$80; {control type request}
 otInfo=$80;      {DataLineEscape type Info}
 otFail=otInfo+1; {DLE type ServerFail}
 otNotFound=otInfo+2; {DLE type ObjectNotFound}
 otEoT=otInfo+3;  {DLE type End Off Transmission}
 otSINC=otInfo+4; {DLE Explicit Ack Request}
 otRateInfo=otInfo+5; {DLE Rate Adjust Debug info}
const {Ctrl opcodes}
 crtlGetInfo=0;
 crtlTerminate=1;
 crtlDhtPeer=2;
 crtlStoreLocalCopy=3;
 crtlStorePut=8;
 crtlStoreGet=4;

IMPLEMENTATION
END.
