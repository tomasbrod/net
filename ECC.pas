unit ECC;

INTERFACE
uses ed25519,Sha1;
type tEccKey=ed25519.tKey32;
type tPoWTimeStamp=Word;

var SecretKey:ed25519.tKey64;
var PublicKey:tEccKey;
var PublicPoW:tEccKey;
var PublicPoWTS:Word;
var ZeroDigest:tSha1Digest;
const cDig3PowMask=%0010;
const cPoWValidWeeks=1;
const cWeekDays=5;
const cWeekEpoch=40179;
procedure CreateChallenge(out Challenge: tEccKey);
procedure CreateResponse(const Challenge: tEccKey; out Response: tSha1Digest; const srce:tEccKey);
function VerifyPoW(const proof:tEccKey; const RemotePub:tEccKey; const stamp:Word):boolean;

operator :=(k:tEccKey) s:string;

IMPLEMENTATION
uses SysUtils,StrUtils,DateUtils;

procedure CreateChallenge(out Challenge: tEccKey);
 var i:byte;
 begin
 for i:=0 to 31 do challenge[i]:=Random(256);
end;

procedure CreateResponse(const Challenge: tEccKey; out Response: tSha1Digest; const srce:tEccKey);
 var Shared:tEccKey;
 var shactx:tSha1Context;
 begin
 ed25519.SharedSecret(shared,srce,secretkey);
 Sha1Init(shactx);
 Sha1Update(shactx,challenge,sizeof(challenge));
 Sha1Update(shactx,shared,sizeof(shared));
 Sha1Final(shactx,Response);
end;

var week:Word;

function VerifyPoW(const proof:tEccKey; const RemotePub:tEccKey; const stamp:Word):boolean;
 var shactx:tSha1Context;
 var digest:tSha1Digest;
 begin
 if abs(Week-BEtoN(stamp))<=cPoWValidWeeks then begin
 Sha1Init(shactx);
 Sha1Update(shactx,proof,sizeof(proof));
 Sha1Update(shactx,RemotePub,sizeof(RemotePub));
 Sha1Update(shactx,stamp,sizeof(stamp));
 Sha1Final(shactx,digest);
 result:=(CompareByte(digest,ZeroDigest,3)=0)and((digest[3]and cDig3PoWMask)=0);
 end else result:=false;
end;

const cPoWFN='proof.dat';
procedure PoWLoadFromFile;
 var f:file of byte;
 begin
 assign(f,cPoWFN);
 reset(f,1);
 blockread(f,PublicPoW,sizeof(PublicPoW));
 blockread(f,PublicPoWTS,2);
 close(f);
end;
procedure PoWGenerate;
 var f:file of byte;
 var i:byte;
 var counter:LongWord;
 var start:tDateTime;
 begin
 assign(f,cPoWFN);
 rewrite(f,1);
 write('ECC: Generating PoW, this may take a while...');
 PublicPowTS:=NtoBE(Week);
 Start:=Now; counter:=0;
 repeat
  for i:=0 to 31 do PublicPoW[i]:=Random(256);
  inc(counter);
 until VerifyPoW(PublicPoW,PublicKey,PublicPoWTS);
 writeln(' PoW found in ',(Now-start)*SecsPerDay:1:0,'s speed=',counter/((Now-start)*SecsPerDay):1:0,'h/s');
 blockwrite(f,PublicPoW,sizeof(PublicPoW));
 blockwrite(f,PublicPoWTS,sizeof(PublicPoWTS));
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

operator :=(k:tEccKey) s:string;
 begin
 Setlength(s,64);
 BinToHex(@k,@s[1],32);
end;

BEGIN
 FillChar(ZeroDigest,sizeof(ZeroDigest),0);
 week:=trunc((Now-cWeekEpoch)/cWeekDays);
 //writeln('ECC: Today is W',Week);
 try LoadFromFile;
 except on e:Exception do begin
  writeln('ECC: '+e.message+' while loading secret key');
  Generate;
  (*until (PublicKey[31]=0)and(PublicKey[30]=0);*)
  SaveGenerated;
  writeln('ECC: random secret key saved to '+cSecKeyFN);
 end end;
 DerivePublic;
 writeln('ECC: pubkey is ',string(PublicKey));
 try
  PoWLoadFromFile;
  if not VerifyPoW(PublicPoW,PublicKey,PublicPoWTS)
   then raise eXception.Create('invalid or expired proof');
  if (week-BEtoN(PublicPowTS))>=cPoWValidWeeks
   then raise eXception.Create('proof about to expire');
 except on e:Exception do begin
  writeln('ECC: '+e.message+' while loading proof');
  PoWGenerate;
 end end;
 writeln('ECC: ProofOfWork valid for W',BEtoN(PublicPowTS));
END.
