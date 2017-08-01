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
function sha512initkey(out md:tsha512context; const key; keylen:LongWord; pad: byte): longint;
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

function sha512initkey(out md:tsha512context; const key; keylen:LongWord; pad: byte): longint;
  var blk: array [0..127] of byte;
  var keyb: array  [0..127] of byte absolute key;
  var i:integer;
  begin
  result:=sha512init(md);
  if keylen>sizeof(blk) then result:=1;
  if result>0 then exit;
  for i:=0 to keylen-1
    do blk[i]:=keyb[i] xor pad;
  for i:=keylen to high(blk)
    do blk[i]:=pad;
  result:=sha512update(md, blk, sizeof(blk));
end;

procedure SHA512Buffer(const Buf; BufLen: LongWord; out digest; digestlen:word );
  var ctx:tSha512context;
  begin
  sha512init(ctx);
  sha512update(ctx, buf, buflen);
  sha512final(ctx,digest,digestlen);
end;

end.
