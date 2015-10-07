UNIT opcode;
INTERFACE

const {dgram opcode}
 tcdata=4;
 tcdataimm=6;
 tccont=5;
 tceack=7;
 tcdata_no_report=8 unimplemented;
const {chat init}
 upFileServer=2;
const {FS opcodes}
 upClose=0;
 {s}upGET=1;
 {r}upINFO=2;
 {r}upFAIL=3;
 {r}upDONE=4;
 {s}upSEG=5;
 {r}upSEGOK=6;
 {s}upFIN=7;
 upErrMalformed=1;
 upErrHiChan=2;
 upErrChanInUse=3;
 
IMPLEMENTATION
END.