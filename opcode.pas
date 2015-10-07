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
 upGET=1;
 upINFO=2;
 upFAIL=3;
 upDONE=4;
 upSEG=5;
 upSEGOK=6;
 upFIN=7;
 
IMPLEMENTATION
END.