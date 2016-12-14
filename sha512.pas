unit sha512;
{$mode objfpc}{$H+}
interface

{$PACKRECORDS C}

type
 tsha512context = record
   state : array[0..7] of QWORD;
   length : QWORD;
   curlen : LongWord;
   buf : array[0..127] of byte;
 end;
 tsha512digest = array [0..63] of byte;

function sha512init(out md:tsha512context):longint;
 cdecl; external name 'sha512_init';
function sha512update(var md:tsha512context; const buf; buflen:LongWord):longint;
 cdecl; external name 'sha512_update';
function sha512finalize(var md:tsha512context):longint;
 cdecl; external name 'sha512_finalize';
function sha512final(var md:tsha512context; out digest):longint; overload;
function sha512final(var md:tsha512context; out digest; len:word):longint; overload;
procedure SHA512Buffer(const Buf; BufLen: LongWord; out digest; digestlen:word );

implementation

{$L alg/sha512.o}

function sha512final(var md:tsha512context; out digest):longint;
  begin
  result:=sha512finalize(md);
  Move( {source} md.state, {dest} digest, 64);
end;

function sha512final(var md:tsha512context; out digest; len:word):longint;
  begin
  result:=sha512finalize(md);
  Move( {source} md.state, {dest} digest, len);
end;

procedure SHA512Buffer(const Buf; BufLen: LongWord; out digest; digestlen:word );
  var ctx:tSha512context;
  begin
  sha512init(ctx);
  sha512update(ctx, buf, buflen);
  sha512final(ctx,digest,digestlen);
end;

end.
