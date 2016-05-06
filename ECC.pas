unit ECC;

INTERFACE
uses ed25519,Sha512,MemStream;
type tEccKey=tKey32;
type tPoWRec=packed record
 data:tKey32;
 stamp:LongWord; {NetOrder}
 end;

var SecretKey:tKey64;
var PublicKey:tEccKey;
var PublicPoW:tPoWRec;
var ZeroDigest:tSha512Digest;
const cDig3PowMask=%0000;
const cPoWValidDays=5;
const cTSEpoch=40179;
procedure CreateChallenge(out Challenge: tEccKey);
procedure CreateResponse(const Challenge: tEccKey; out Response: tKey32; const srce:tEccKey);
function VerifyPoW(const proof:tPoWRec; const RemotePub:tEccKey):boolean;

IMPLEMENTATION
uses SysUtils,DateUtils;

procedure CreateChallenge(out Challenge: tEccKey);
 var i:byte;
 begin
 for i:=0 to 31 do challenge[i]:=Random(256);
end;

procedure CreateResponse(const Challenge: tEccKey; out Response: tKey32; const srce:tEccKey);
 var Shared:tEccKey;
 var shactx:tSha512Context;
 begin
 ed25519.SharedSecret(shared,srce,secretkey);
 Sha512Init(shactx);
 Sha512Update(shactx,challenge,sizeof(challenge));
 Sha512Update(shactx,shared,sizeof(shared));
 Sha512Final(shactx,Response,32);
end;

var TSNow:LongWord;

function VerifyPoW(const proof:tPoWRec; const RemotePub:tEccKey; out digest:tSha512Digest ):boolean;
 var shactx:tSha512Context;
 var delta:Integer;
 begin
 delta:=TSNow-BEtoN(proof.stamp);
 if (delta<=cPoWValidDays) and (delta>=-1) then begin
 Sha512Init(shactx);
 Sha512Update(shactx,proof,sizeof(proof));
 Sha512Update(shactx,RemotePub,sizeof(RemotePub));
 Sha512Final(shactx,digest);
 result:=(CompareByte(digest,ZeroDigest,3)=0)and((digest[3]and cDig3PoWMask)=0);
 end else result:=false;
end;

function VerifyPoW(const proof:tPoWRec; const RemotePub:tEccKey):boolean;
 var digest:tSha512Digest;
 begin
 result:=VerifyPoW(proof,RemotePub,digest);
end;

const cPoWFN='proof.dat';
procedure PoWLoadFromFile;
 var f:file of tPoWRec;
 begin
 assign(f,cPoWFN);
 reset(f);
 read(f,PublicPoW);
 close(f);
end;

procedure PoWGenerate;
 var f:file of tPoWRec;
 var i:byte;
 var counter:LongWord;
 var start:tDateTime;
 var digest:tSha512Digest;
 begin
 assign(f,cPoWFN);
 rewrite(f);
 write('ECC: Generating PoW, this may take a while...');
 PublicPow.stamp:=NtoBE(TSNow);
 Start:=Now; counter:=0;
 for i:=0 to 31 do digest[i]:=Random(256);
 repeat
  Move(digest,PublicPoW.data,32);
  inc(counter);
 until VerifyPoW(PublicPoW,PublicKey,digest);
 writeln(' PoW found in ',(Now-start)*SecsPerDay:1:0,'s speed=',counter/((Now-start)*SecsPerDay):1:0,'h/s');
 write(f,PublicPoW);
 close(f);
end;

const cSeckeyFN='secret.dat';
procedure LoadFromFile;
 var f:file of ed25519.tPrivKey;
 begin
 assign(f,cSecKeyFN);
 reset(f);
 read(f,SecretKey);
 close(f);
end;

procedure SaveGenerated;
 var f:file of ed25519.tPrivKey;
 begin
 assign(f,cSecKeyFN);
 rewrite(f);
 //fpchmod
 write(f,SecretKey);
 close(f);
end;

procedure Generate;
 {$IFDEF UNIX}
 var f:file of ed25519.tPrivKey;
 begin
 assign(f,'/dev/urandom');
 reset(f);
 read(f,SecretKey);
 close(f);
 {$ELSE}
 begin
 {$WARNING Not enough Random in windows license key}
 {$ERROR This unit requires UNIX-compatile operating system}
 {$ENDIF}
end;

procedure DerivePublic;
 begin
 CreatekeyPair(PublicKey,SecretKey);
end;

BEGIN
 FillChar(ZeroDigest,sizeof(ZeroDigest),0);
 TSNow:=trunc(Now-cTSEpoch);
 writeln('ECC: Today is D',TSNow);
 try LoadFromFile;
 except on e:Exception do begin
  writeln('ECC: '+cSecKeyFN+' '+e.message+' while loading secret key, generating new keypair');
  Generate;
  (*until (PublicKey[31]=0)and(PublicKey[30]=0);*)
  SaveGenerated;
 end end;
 DerivePublic;
 writeln('ECC: pubkey is ',string(PublicKey));
 try
  PoWLoadFromFile;
  if not VerifyPoW(PublicPoW,PublicKey)
   then raise eXception.Create('invalid or expired proof');
  if (TSNow-BEtoN(PublicPow.Stamp))>=cPoWValidDays
   then raise eXception.Create('proof about to expire');
 except on e:Exception do begin
  writeln('ECC: '+cPOWFN+' '+e.message+' while loading proof');
  PoWGenerate;
 end end;
 //writeln('ECC: ProofOfWork valid for W',BEtoN(PublicPow.Stamp));
END.
