UNIT opcode;
INTERFACE

const {dgram opcode}
 {4-8 for TC deprecated}
 {8-9 for ObjectTransfer}
 otCtrl=9; {server->client}
 otData=8; {client->server}
 {10-16 reserved for dht}
 dhtRequest=10;
 dhtPeers=11;
 dhtCapable=16;
 mutableUpdate=17;
const {chat init}
 upFileServer=2 deprecated;
 crAuthReq=3;
 testChat=32;
const {ObjectTransfer magic numbers}
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
const {dht node Capabilities}
 capProfile=1 deprecated;
 capMutable=1;
 capTest=32;

IMPLEMENTATION
END.
