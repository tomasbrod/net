unit FileStore;

(* Uniform interface to all kinds of chk files *)

INTERFACE

Uses
  Log
 ,Keys
 ;

type tFID = object (Keys.tHash) end;

IMPLEMENTATION
Uses SimpleFile;

END.