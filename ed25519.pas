{$mode objfpc}
UNIT ed25519;
INTERFACE
uses ObjectModel,Sha512;

type
 tPubKey=tKey32;
 tPrivKey=tKey64;
 tSig=tKey64;

(*procedure CreateSeed(out seed: tKey32);*)
procedure CreatekeyPair(out pub:tPubKey; var priv:tPrivKey);
procedure Sign(out signature:tSig; const message; len:LongWord; const pub:tPubKey; const priv:tPrivKey);
function Verify(const signature:tSig; const message; len:LongWord; const pub:tPubKey):boolean;
function Verify1(var ctx:tSha512Context):boolean;
function Verify2(var ctx:tSha512Context; const signature:tSig; const pub:tPubKey):boolean;
procedure SharedSecret(out shared:tKey32; const pub:tPubKey; const priv:tPrivKey);

IMPLEMENTATION

{$L ed25519/sc.o}
{$L ed25519/fe.o}
{$L ed25519/ge.o}
{$L ed25519/sign.o}
{$L ed25519/verify.o}
{$L ed25519/key_exchange.o}
{$L ed25519/sha512.o}

procedure ed25519_create_keypair(pub,priv,seed:pointer);
 cdecl;external;
procedure ed25519_sign(sig,msg:pointer; len:LongWord; pub,priv:pointer);
 cdecl;external;
function ed25519_verify(sig,msg:pointer; len:LongWord; pub:pointer):integer;
 cdecl;external;
function ed25519_verify_p2(hash,sig,pub:pointer):integer;
 cdecl;external;
procedure ed25519_key_exchange(shared,pub,priv:pointer);
 cdecl;external;

type ge_p3=packed array [1..160] of byte; {opaque}
procedure ge_scalarmult_base(h,a:pointer); cdecl;external;
procedure ge_p3_tobytes(s, h:pointer); cdecl;external;

procedure CreateKeyPair(out pub:tPubKey; var priv:tPrivKey);
 var A:ge_p3;
 begin
 priv[ 0] := priv[ 0] and 248;
 priv[31] := priv[31] and  63;
 priv[31] := priv[31]  or  64;
 ge_scalarmult_base(@A, @priv);
 ge_p3_tobytes(@pub, @A);
end;

procedure Sign(out signature:tSig; const message; len:LongWord; const pub:tPubKey; const priv:tPrivKey);
 begin
 ed25519_sign(@signature,@message,len,@pub,@priv);
end;

function Verify(const signature:tSig; const message; len:LongWord; const pub:tPubKey):boolean;
 var hash:tSha512Context;
 begin
 Sha512Init(hash);
 Sha512Update(hash,message,len);
 result:=Verify2(hash,signature,pub);
 //assert(result=(ed25519_verify(@signature,@message,len,@pub)=1));
end;

function Verify1(var ctx:tSha512Context):boolean;
  begin
  Sha512Init(ctx);
  result:=true;
end;
function Verify2(var ctx:tSha512Context; const signature:tSig; const pub:tPubKey):boolean;
  begin
  result:=ed25519_verify_p2(@ctx,@signature,@pub)=1;
end;

procedure SharedSecret(out shared:tKey32; const pub:tPubKey; const priv:tPrivKey);
 begin
 ed25519_key_exchange(@shared,@pub,@priv);
end;

END.
