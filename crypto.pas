UNIT crypto;
{$mode objfpc}{$H+}

(* Pascal Procedural and OO API to OpenSSL *)
(* and few utility functions *)

interface
uses ctypes, ObjectModel;
{$PACKRECORDS C}
{$LINKLIB crypto}
{$LINKLIB z}


{**** SHA256 ****}
type tSSLDWORD=DWORD;
type tSHA256context = record
  h: array [0..7] of ctypes.cuint;
  Nl, Nh: tSSLDWORD;
  data: packed array [0..15] of tSSLDWORD;
  num, md_len: ctypes.cUInt;
end;
type tsha256digest = tKey32;
function SHA256_Init(var c:tsha256context):cint;
 cdecl; external name 'SHA256_Init';
function SHA256_Update(var c:tsha256context; const data; len:csize_t):cint;
 cdecl; external name 'SHA256_Update';
function SHA256_Final(out md: tKey32; var c:tsha256context):cint;
 cdecl; external name 'SHA256_Final';
procedure SHA256_Transform(var c:tsha256context; var data);
 cdecl; external name 'SHA256_Transform';
type tSHA256 = object
  var ctx:tsha256context;
  procedure Init;
  procedure Update( const data; len: longword);
  procedure Final( out md:tsha256digest);
  procedure TruncFinal( out md; mdlen: longword );
  procedure InitWithKey( const data; len: longword; pad: byte);
end;
procedure SHA256_Buffer( out md; mdlen: word; const data; len:longword);


{**** SHA512 ****}
type tSHA512context = record
  h: array [0..7] of QWORD;
  Nl, Nh: QWORD;
  data: packed array [0..15] of QWORD;
  num, md_len: ctypes.cUInt;
end;
type tsha512digest = tKey64;
function SHA512_Init(var c:tsha512context):cint;
 cdecl; external name 'SHA512_Init';
function SHA512_Update(var c:tsha512context; const data; len:csize_t):cint;
 cdecl; external name 'SHA512_Update';
function SHA512_Final(out md:tsha512digest; var c:tsha512context):cint;
 cdecl; external name 'SHA512_Final';
procedure SHA512_Transform(var c:tsha512context; var data);
 cdecl; external name 'SHA512_Transform';
procedure SHA512_Buffer( out md:tKey64; const data; len:longword);


{**** Rijndael ****}
type tAES_key = record
  rd_key: array [0..59] of ctypes.cULong;
  rounds: ctypes.cInt;
end;
type tAESkey = tAES_key;
function AES_set_encrypt_key(const userKey; bits: cint; out key:tAES_key):cint;
  cdecl; external name 'AES_set_encrypt_key';
function AES_set_decrypt_key(const userKey; bits: cint; out key:tAES_key):cint;
  cdecl; external name 'AES_set_decrypt_key';
procedure AES_encrypt(const inp; out outp; var key: tAES_key);
  cdecl; external name 'AES_encrypt';
procedure AES_decrypt(const inp; out outp; var key: tAES_key);
  cdecl; external name 'AES_decrypt';
function AES_wrap_key(var key: tAES_key; iv: pointer;
  out outp; const inp; inlen: cuint):cint;
  cdecl; external name 'AES_wrap_key';
function AES_unwrap_key(var key: tAES_key; iv: pointer;
  out outp; const inp; inlen: cuint):cint;
  cdecl; external name 'AES_unwrap_key';
type tAES = object
  expanded:  tAES_key;
  procedure InitEnCrypt(const userKey; bits: integer);
  procedure InitDeCrypt(const userKey; bits: integer);
  procedure EnCryptECB(out outp; const inp);
  procedure DeCryptECB(out outp; const inp);
  {!wrapped data is 8 byte LONGER than original data}
  procedure Wrap(out wrapped; const inp; len: longword; iv: pointer);
  function  UnWrap(out unwrapped; const inp; len: longword; iv: pointer): boolean;
end;
type tAES_FB = object (tAES)
  feedback: packed array [0..15] of byte;
  procedure SetIV(const iv);
  procedure EnCryptCBC(out outp; const inp);
  procedure DeCryptCBC(out outp; const inp);
  procedure EnCryptPCBC(out outp; const inp);
  procedure DeCryptPCBC(out outp; const inp);
  procedure EnCryptCFB(out outp; const inp);
  procedure DeCryptCFB(out outp; const inp);
end;

{**** Blowfish ****}
type tBF_key = record
  p : array[0..17] of tSSLDWORD;
  s : array[0..3,0..255] of tSSLDWORD;
end;
procedure BF_set_key(out key: tBF_key; len: cint; const data);
  cdecl; external name 'BF_set_key';
procedure BF_encrypt(var data; var key: tBF_key);
  cdecl; external name 'BF_encrypt';
procedure BF_decrypt(var data; var key: tBF_key);
  cdecl; external name 'BF_decrypt';

{**** UTILS ****}
procedure BlockXOR(out r; const a; const b; len: longword);


{**** KDF ****}
{todo...}


{**** Hash MAC ****}
type tSHA256HMAC = object (tSHA256)
  var outerctx:tsha256context;
  procedure Init( const key; keylen: longword);
  procedure Final( out md:tsha256digest);
end;

{**** Curve 2^255-19 ****}

{procedure CreateSeed(out seed: tKey32);}
procedure Ed25519_CreatekeyPair(out pub:tKey32; var priv:tKey64);
procedure Ed25519_Sign(out signature:tKey64; const message; len:LongWord; const pub:tKey32; const priv:tKey64); inline;
function Ed25519_Verify(const signature:tKey64; const message; len:LongWord; const pub:tKey32):boolean;
function Ed25519_Verify1(var ctx:tSha512Context):boolean; inline;
function Ed25519_Verify2(var ctx:tSha512Context; const signature:tKey64; const pub:tKey32):boolean; inline;
procedure Ed25519_SharedSecret(out shared:tKey32; const pub:tKey32; const priv:tKey64); inline;


{**** GZip DEFLATE ****}
const
  Z_NO_FLUSH=0; Z_PARTIAL_FLUSH=1; Z_SYNC_FLUSH=2; Z_FULL_FLUSH=3; Z_FINISH=4;
  Z_BLOCK=5; Z_TREES=6; Z_OK=0; Z_STREAM_END=1; Z_NEED_DICT=2; Z_ERRNO=-1;
  Z_STREAM_ERROR=-2; Z_DATA_ERROR=-3; Z_MEM_ERROR=-4; Z_BUF_ERROR=-5;
  Z_VERSION_ERROR=-6; Z_NO_COMPRESSION=0; Z_BEST_SPEED=1; Z_BEST_COMPRESSION=9;
  Z_DEFAULT_COMPRESSION=-1; Z_FILTERED=1; Z_HUFFMAN_ONLY=2; Z_RLE=3; Z_FIXED=4;
  Z_DEFAULT_STRATEGY=0; Z_BINARY=0; Z_TEXT=1; Z_ASCII=Z_TEXT; Z_UNKNOWN=2;
  Z_DEFLATED=8; ZLIB_VERSION:pchar='1.2.8';
type tGZipContext = object
  next_in: pointer;
  avail_in: cuint;
  total_in: culong;
  next_out: pointer;
  avail_out: cuint;
  total_out: culong;
  msg: pchar;
  state: pointer;
  alloc_func,zfree,opaque: pointer;
  data_type: cint;
  adler: culong;
  reserved: culong;
end;
function deflateInit2_( var ctx: tGZipContext;
  level, method, windowBits, memLevel, strategy: cint;
  version: pchar; stream_size: cint ): cint;
  cdecl; external name 'deflateInit2_';
function deflate(var ctx: tGZipContext; flush: cint ): cint;
  cdecl; external name 'deflate';
function deflateEnd(var ctx: tGZipContext): cint;
  cdecl; external name 'deflateEnd';
function inflateInit2_(var ctx: tGZipContext; windowBits: cint;
  version: pchar; stream_size: cint ): cint;
  cdecl; external name 'inflateInit2_';
function inflate(var ctx: tGZipContext; flush: cint): cint;
  cdecl; external name 'inflate';
function inflateEnd(var ctx: tGZipContext): cint;
  cdecl; external name 'inflateEnd';
type tGZip = object(tGZipContext)
  eof: boolean;
  procedure InitDeflate;
  procedure Deflate;
end;

(* CRC with polynomial $04C11DB7 (aka table "B") from Zlib *)
function CRC32b( crc: culong; const data; len: cuint ): culong;
  cdecl; external name 'crc32';

(* CRC with polynomial $1EDC6F41 (aka Castagnoli) *)
function CRC32c( iv: DWord; const data; len: longWord ): DWord;

implementation

procedure BlockXOR(out r; const a; const b; len: longword);
  var i:integer;
  begin
  for i:=0 to len-1
    do byte((@r+i)^):=byte((@a+i)^) xor byte((@b+i)^);
end;

procedure SHA512_Buffer( out md: tKey64; const data; len:longword);
  var ctx: tSHA512context;
  begin
  SHA512_Init( ctx);
  SHA512_Update( ctx, data, len);
  SHA512_Final( md, ctx)
end;

procedure SHA256_Buffer( out md; mdlen: word; const data; len:longword);
  var ctx: tSHA256context;
  var full: tSha256digest;
  begin
  SHA256_Init( ctx);
  SHA256_Update( ctx, data, len);
  if mdlen>=32
  then SHA256_Final( tKey32(md), ctx)
  else begin
    SHA256_Final( full, ctx);
    Move(full,md,mdlen);
  end;
end;

procedure tSHA256.Init;
  begin
  SHA256_Init(ctx);
end;

procedure tSha256.InitWithKey( const data; len: longword; pad: byte);
  var block: array [0..63] of byte;
  begin
  if len>64 then len:=64;
  SHA256_Init(ctx);
  FillChar(block,sizeof(block),pad);
  BlockXOR(block,block,data,len);
  SHA256_Update(ctx,block,sizeof(block));
end;

procedure tSHA256.Update( const data; len: longword);
  begin
  SHA256_Update( ctx, data, len);
end;

procedure tSHA256.Final( out md:tsha256digest);
  begin
  SHA256_Final( md, ctx);
end;

procedure tSHA256.TruncFinal( out md; mdlen:LongWord);
  var full: tSha256digest;
  begin
  SHA256_Final( full, ctx);
  Move( full, md, mdlen);
end;

procedure tSHA256HMAC.Init( const key; keylen: longword);
  var block: array [0..63] of byte;
  begin
  if keylen>64 then keylen:=64; {not standard but never used}
  inherited Init; {SHA256_Init(ctx);}
  SHA256_Init(outerctx);
  FillChar(block,sizeof(block),$36);
  BlockXOR(block,block,key,keylen);
  SHA256_Update(ctx,block,sizeof(block));
  FillChar(block,sizeof(block),$5C);
  BlockXOR(block,block,key,keylen);
  SHA256_Update(outerctx,block,sizeof(block));
end;

procedure tSHA256HMAC.Final( out md:tsha256digest);
  begin
  SHA256_Final(md, ctx);
  SHA256_Update(outerctx, md, sizeof(md));
  SHA256_Final(md, outerctx);
end;

procedure tAES.InitEnCrypt(const userKey; bits: integer);
  begin AES_set_encrypt_key(userKey, bits, expanded) end;
procedure tAES.InitDeCrypt(const userKey; bits: integer);
  begin AES_set_decrypt_key(userKey, bits, expanded) end;
procedure tAES.EnCryptECB(out outp; const inp);
  begin AES_encrypt(inp, outp, expanded) end;
procedure tAES.DeCryptECB(out outp; const inp);
  begin AES_decrypt(inp, outp, expanded) end;

procedure tAES.Wrap(out wrapped; const inp; len: longword; iv: pointer);
  begin AES_wrap_key(expanded, iv, wrapped, inp, len) end;

function  tAES.UnWrap(out unwrapped; const inp; len: longword; iv: pointer): boolean;
  begin
  result:= AES_wrap_key(expanded, iv, unwrapped, inp, len) >0;
end;


procedure tAES_FB.SetIV(const iv);
  begin
  Move(iv, feedback, 16);
end;

procedure tAES_FB.EnCryptCBC(out outp; const inp);
  begin
  BlockXOR(feedback, feedback, inp, 16);
  AES_encrypt(feedback, outp, expanded);
  Move(outp,feedback,16);
end;
  
procedure tAES_FB.DeCryptCBC(out outp; const inp);
  begin
  Move(inp,feedback,16);
  AES_decrypt(inp, outp, expanded);
  BlockXOR(outp, outp, feedback, 16);
end;

procedure tAES_FB.EnCryptPCBC(out outp; const inp);
  begin
  BlockXOR(feedback, feedback, inp, 16);
  AES_encrypt(feedback, outp, expanded);
  BlockXOR(feedback, outp, inp, 16);
end;

procedure tAES_FB.DeCryptPCBC(out outp; const inp);
  begin
  AES_decrypt(inp, outp, expanded);
  BlockXOR(outp, outp, feedback, 16);
  BlockXOR(feedback, outp, inp, 16);
end;

procedure tAES_FB.EnCryptCFB(out outp; const inp);
  begin
  AES_encrypt(feedback, outp, expanded);
  BlockXOR(outp, outp, inp, 16);
  Move(outp,feedback,16);
end;

procedure tAES_FB.DeCryptCFB(out outp; const inp);
  begin
  AES_encrypt(feedback, outp, expanded);
  BlockXOR(outp, outp, inp, 16);
  Move(inp,feedback,16);
end;


{$L alg/sc.o}
{$L alg/fe.o}
{$L alg/ge.o}
{$L alg/sign.o}
{$L alg/verify.o}
{$L alg/key_exchange.o}

procedure ed25519_create_keypair(pub,priv,seed:pointer);
 cdecl;external;
procedure ed25519_sign_int(sig,msg:pointer; len:LongWord; pub,priv:pointer);
 cdecl;external name 'ed25519_sign';
function ed25519_verify_int(sig,msg:pointer; len:LongWord; pub:pointer):integer;
 cdecl;external name 'ed25519_verify';
function ed25519_verify_p2(hash,sig,pub:pointer):integer;
 cdecl;external;
procedure ed25519_key_exchange(shared,pub,priv:pointer);
 cdecl;external;

type ge_p3=packed array [1..160] of byte; {opaque}
procedure ge_scalarmult_base(h,a:pointer); cdecl;external;
procedure ge_p3_tobytes(s, h:pointer); cdecl;external;

procedure ed25519_CreateKeyPair(out pub:tKey32; var priv:tKey64);
 var A:ge_p3;
 begin
 priv[ 0] := priv[ 0] and 248;
 priv[31] := priv[31] and  63;
 priv[31] := priv[31]  or  64;
 ge_scalarmult_base(@A, @priv);
 ge_p3_tobytes(@pub, @A);
end;

procedure ed25519_Sign(out signature:tKey64; const message; len:LongWord; const pub:tKey32; const priv:tKey64);
 begin
 ed25519_sign_int(@signature,@message,len,@pub,@priv);
end;

function ed25519_Verify(const signature:tKey64; const message; len:LongWord; const pub:tKey32):boolean;
 var hash:tSha512Context;
 begin
 Sha512_Init(hash);
 Sha512_Update(hash,message,len);
 result:=ed25519_Verify2(hash,signature,pub);
 //assert(result=(ed25519_verify_int(@signature,@message,len,@pub)=1));
end;

function ed25519_Verify1(var ctx:tSha512Context):boolean;
  begin
  Sha512_Init(ctx);
  result:=true;
end;
function ed25519_Verify2(var ctx:tSha512Context; const signature:tKey64; const pub:tKey32):boolean;
  begin
  result:=ed25519_verify_p2(@ctx,@signature,@pub)=1;
end;

procedure ed25519_SharedSecret(out shared:tKey32; const pub:tKey32; const priv:tKey64);
 begin
 ed25519_key_exchange(@shared,@pub,@priv);
end;


procedure tGZip.InitDeflate;
  var rv: integer;
  begin
  self.alloc_func:=nil;
  self.zfree:=nil;
  self.opaque:=nil;
  rv:= deflateInit2_( self,
    {level=}Z_BEST_COMPRESSION,
    {method=}Z_DEFLATED,
    {windowbits=}-15,
    {memLevel=}9,
    {strategy=}Z_DEFAULT_STRATEGY,
    {version=}ZLIB_VERSION,{size=}sizeof(tGZipContext));
  if rv<>Z_OK then raise eXception.Create('libz: '+self.msg);
  eof:=false;
  avail_in:=0;
  avail_out:=0;
end;

procedure tGZip.Deflate;
  var rv: integer;
  var flush: integer;
  begin
  flush:=Z_NO_FLUSH;
  if avail_in=0 then flush:=Z_FINISH;
  if eof then raise eXception.Create('Deflate calld after STREAM_END');
  rv:= crypto.deflate(self, flush);
  if rv=Z_OK then eof:=false
  else if (rv=Z_STREAM_END) and (flush=Z_FINISH) then eof:=true
  else raise eXception.Create('libz: '+self.msg);
end;

type tCRC32Table=array [byte] of DWord;
var crc32c_table: tCRC32Table;
{$IFDEF CRC_CHECK_TABLE}
{$INCLUDE crc32c-tbl.pas}
{$ENDIF}

function CRC32c( iv: DWord; const data; len: longWord ): DWord;
  var p:^byte;
  begin
  p:=@data;
  result:= not iv;
  while len > 0 do begin
    result:= crc32c_table[(result xor p^) and $FF] xor (result shr 8);
    inc(p); dec(len);
  end;
  result:= result xor $FFFFFFFF;
end;

procedure GenerateCRC32Table(out table: tCRC32Table; const Poly: DWord);
  var acc: DWord;
  var sub,row: integer;
  begin
  row:=0;
  for row:=high(byte) downto 0 do begin
    acc:=row;
    for sub:=0 to 7 do begin
      if (acc and 1) <>0
        then acc:=(acc shr 1) xor Poly
        else acc:=(acc shr 1);
    end;
    table[row]:=acc;
  end;
end;


BEGIN
  GenerateCRC32Table(crc32c_table,$82F63B78);
  {$IFDEF CRC_CHECK_TABLE}
  Assert(CompareByte(crc32c_table,crc32c_table_const,sizeof(crc32c_table_const))=0,'CRC32-C table check failed');
  {$ENDIF}
end.
{
procedure ExpandKey(out keystruct: tBlowfishKey; const user_key; len: word);
  begin
  blowfish_key_setup(@user_key,@keystruct,len);
end;
procedure Encrypt(out chipertext; const plaintext; const key:TBLOWFISHKEY);
  begin
  blowfish_encrypt(@plaintext,@chipertext,@key);
end;
procedure Decrypt(out plaintext; const chipertext; const key:TBLOWFISHKEY);
  begin
  blowfish_decrypt(@chipertext,@plaintext,@key);
end;
procedure BlockXOR(out oup; const key; const len:word);
  var i:byte;
  begin
  for i:=0 to len-1 do
    byte((@oup+i)^):=byte((@oup+i)^) xor byte((@key+i)^);
end;
procedure EncryptCBC(out chipertext; const plaintext; const key:TBLOWFISHKEY; var fv);
  begin
  BlockXOR(fv,plaintext,8);
  blowfish_encrypt(@fv,@chipertext,@key);
  Move(chipertext,fv,8);
end;
procedure DecryptCBC(out plaintext; const chipertext; const key:TBLOWFISHKEY; var fv);
  begin
  blowfish_decrypt(@chipertext,@plaintext,@key);
  BlockXOR(plaintext,fv,8);
  Move(chipertext,fv,8);
end;
**** BZip2 ****
const
  BZ_RUN=0; BZ_FLUSH=1; BZ_FINISH=2; BZ_OK=0; BZ_RUN_OK=1; BZ_FLUSH_OK=2;
  BZ_FINISH_OK=3; BZ_STREAM_END=4; BZ_SEQUENCE_ERROR=-1; BZ_PARAM_ERROR=-2;
  BZ_MEM_ERROR=-3; BZ_DATA_ERROR=-4; BZ_DATA_ERROR_MAGIC=-5; BZ_IO_ERROR=-6;
  BZ_UNEXPECTED_EOF=-7; BZ_OUTBUFF_FULL=-8; BZ_CONFIG_ERROR=-9;
type tBZipContext = record
  next_in: pointer;
  avail_in: cuint;
  total_in_lo32: cuint;
  total_in_hi32: cuint;
  next_out: pointer;
  avail_out: cuint;
  total_out_lo32: cuint;
  total_out_hi32: cuint;
  state: pointer;
  bzalloc: pointer;
  bzfree: pointer;
  opaque: pointer;
end;
function BZ2_bzCompressInit(var strm: tBZipContext;
  blockSize100k: cint; verbosity: cint; workFactor: cint ): cint;
  cdecl; external name 'BZ2_bzCompressInit';
function BZ2_bzCompress(var strm: tBZipContext; action: cint): cint;
  cdecl; external name 'BZ2_bzCompress';
function BZ2_bzCompressEnd(var strm: tBZipContext): cint;
  cdecl; external name 'BZ2_bzCompressEnd';
function BZ2_bzDecompressInit(var strm: tBZipContext;
  verbosity: cint; small: cint ): cint;
  cdecl; external name 'BZ2_bzDecompressInit';
function BZ2_bzDecompress(var strm: tBZipContext; action: cint): cint
  cdecl; external name 'BZ2_bzDecompress';
function BZ2_bzDecompressEnd(var strm: tBZipContext): cint;
  cdecl; external name 'BZ2_bzDecompressEnd';
}
