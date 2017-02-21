UNIT opcode;
INTERFACE

const {dgram opcode}
 {note: opcodes >=$80 are a reply and have transaction id so they can overlap}
   (*Object Transfer*)
   otOldCtrl=$09;
   otoldData=$08;
   otRequest=$04;
   otReport= $05;
   otStop=   $06;
   otData=   $08;
   otInfo=   $07;
   otDataSync=$09;
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
  (*Tracker*)
  tkQuery=$14;
  tkAnnounce=$15;
  tkResult=$94;
  tkAnnOK=$95;
  (*Profile*)
  profQuery=$12;
  profResult=$92;
  (*Messages*)

const {chat init}
 upFileServer=2 deprecated;
 crAuthReq=3;
 testChat=32;
const (*ObjectTransfer magic numbers*)
  otcOK=0;
  otcEoT=1;
  otcDebug=2;
  otcNotFound=3;
  otcChannelLimit=4;
  otcStorageError=5;
  otcServerThrottle=6;
  otcFail=7;
const {Ctrl opcodes}
 crtlGetInfo=0;
 crtlTerminate=1;
 crtlDhtPeer=2;
 crtlStoreLocalCopy=3;
 crtlStorePut=8;
 crtlStoreGet=4;

IMPLEMENTATION
END.
