unit sha512;
{$mode objfpc}{$H+}
interface

{$PACKRECORDS C}

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
function sha512final(var md:tsha512context; out digest; len:word):longint;
 overload; {truncated to len bytes}
procedure SHA512Buffer(const Buf; BufLen: LongWord; out digest; digestlen:word );

implementation

{$L ed25519/sha512.o}

function sha512final(var md:tsha512context; out digest; len:word):longint;
  var full:tSha512Digest;
  begin
  result:=sha512final(md, full);
  if result=0 then Move({source}full,{dest}digest,len);
end;

procedure SHA512Buffer(const Buf; BufLen: LongWord; out digest; digestlen:word );
  var ctx:tSha512context;
  begin
  sha512init(ctx);
  sha512update(ctx, buf, buflen);
  sha512final(ctx,digest,digestlen);
end;

end.
