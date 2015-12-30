unit sha512;
{$mode objfpc}{$H+}
interface

{$PACKRECORDS C}
{$L ed25519/sha512.o}

type
 tsha512context = record
   length : QWORD;
   state : array[0..7] of QWORD;
   curlen : LongWord;
   buf : array[0..127] of byte;
 end;
 tsha512digest = array [0..63] of byte;

function sha512init(out md:tsha512context):longint;
 cdecl; external name 'sha512_init';
function sha512update(var md:tsha512context; const buf; buflen:LongWord):longint;
 cdecl; external name 'sha512_update';
function sha512final(var md:tsha512context; out digest:tsha512digest):longint;
 cdecl; external name 'sha512_final';

implementation

end.