UNIT opcode;
INTERFACE

const {dgram opcode}
 tcdata=4;
 tcdataimm=6;
 tccont=5;
 tceack=7;
 tcdata_no_report=8 unimplemented;
 {10-16 reserved for dht}
 dht=10;
 dhtRequest=10;
 dhtSelect=11;
 dhtReqAck=12;
 dhtWazzup=13;
const {chat init}
 upFileServer=2;
const {FS opcodes}
 {c}upOPEN=7;
 {s}upINFO=8;
 {s}upFAIL=9;
 {c}upLSEG=10;
 {s}upUNAVL=11;
 {c}upSTOP=13;
 {c}upCLOSE=14;
 {c}upWEIGHT=15;
 {s}upEPROTO=16;
 {s}upDONE=17;
 {s}upSEGOK=18;
 upErrMalformed=1;
 upErrHiChan=2;
 upErrChanInUse=3;
 upErrNotFound=4;
 upErrIO=5;
 upErrSegNoGet=6;
 upErrNotOpen=7;
 upErrTroll=99;

IMPLEMENTATION
END.