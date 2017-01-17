unit HostKey;

INTERFACE
uses ed25519,Crypto,ObjectModel;
type tEccKey=tKey32;
type tPoWRec=packed record
 data:tKey32;
 stamp:Word6;
 end;

var SecretKey:tKey64;
var PublicKey:tEccKey;
var PublicPoW:tPoWRec;
var PublicPoWReady:boolean;
var ZeroDigest:tSha512Digest;
{$IFDEF ENDIAN_LITTLE}
const cPowMask0:DWORD=$FF0FFFFF;
{$ELSE}
const cPowMask0:DWORD=$FFFF0FFF;
{$ENDIF}
const cPoWValidFor=5*{days}86400;
const cHostIdent:array [1..8] of char='BNHosSW'#26;
procedure CreateChallenge(out Challenge: tKey32);
procedure CreateResponse(const Challenge: tKey32; out Response: tKey32; const srce:tEccKey);
function VerifyPoW(const proof:tPoWRec; const RemotePub:tEccKey):boolean;

IMPLEMENTATION
uses SysUtils{,DateUtils},ServerLoop;

procedure CreateChallenge(out Challenge: tKey32);
 var i:byte;
 begin
 for i:=0 to 31 do challenge[i]:=Random(256);
end;

procedure CreateResponse(const Challenge: tKey32; out Response: tKey32; const srce:tEccKey);
 var Shared:tKey32;
 var shactx:tSha256Context;
 begin
 ed25519.SharedSecret(shared,srce,secretkey);
 SHA256_Init(shactx);
 SHA256_Update(shactx,challenge,sizeof(challenge));
 SHA256_Update(shactx,shared,sizeof(shared));
 SHA256_Final(Response,shactx);
end;

function VerifyPoW(const proof:tPoWRec; const RemotePub:tEccKey ):boolean;
 var shactx:tSha256Context;
 var delta:Int64;
  var digest: tKey32;
  var dig_4dw: dword absolute digest[0];
 begin
 delta:=UnixNow-Int64(proof.stamp);
 if (delta<=cPoWValidFor) and (delta>=-600) then begin
 SHA256_Init(shactx);
 SHA256_Update(shactx,proof,sizeof(proof));
 SHA256_Update(shactx,RemotePub,sizeof(RemotePub));
 SHA256_Final(digest,shactx);
 result:= (dig_4dw and cPoWMask0) =0;
 end else result:=false;
end;

procedure GetOSRandom(buf:pointer; cnt:word);
  {$IFDEF UNIX}
  var f:tHANDLE;
  var q:longint;
  begin
  f:=FileOpen('/dev/random',fmOpenRead or fmShareDenyNone);
  while (cnt>0)and(f>=0) do begin
    q:=FileRead(f,buf^,cnt);
    if q<=0 then break; cnt:=cnt-q; buf:=buf+q;
  end;
  if (f<0) or (q<=0) then begin
    writeln('ERROR reading /dev/random');
    halt(8);
  end else FileClose(f);
  {$ELSE}
  begin
  {$WARNING Random on non-UNIX unimplemented}
  writeln('[WARNING: No random source! Using all zeros, generated keys will be shit.]');
  FillChar(out,0,cnt);
  {$ENDIF}
end;

type tPoWRefreshObj=object procedure Timer; end;
var PoWGenBg:tPoWRefreshObj;
const cSeckeyFN='hostkey.dat';

function PoWGenThr(param:pointer):ptrint;
  var f:tFileStream;
  var i:byte;
  var start:tDateTime;
  var shactx:tSha256Context;
  var wp:tPoWRec;
  var counter:LongWord absolute wp.data;
  var digest: tKey32;
  var dig_4dw: dword absolute digest[0];
  begin result:=0;
  SetThreadName('PoW_Gen');
  wp.stamp:=Word6(UnixNow);
  for i:=0 to 31 do wp.data[i]:=Random(256);
  Start:=Now; counter:=0;
  repeat
    if counter=high(counter) then raise eXception.Create('some shit happend');
    inc(counter);
    SHA256_Init(shactx);
    SHA256_Update(shactx,wp,sizeof(wp));
    SHA256_Update(shactx,PublicKey,sizeof(PublicKey));
    SHA256_Final(digest,shactx);
  until (dig_4dw and cPowMask0) =0;
  EnterCriticalSection(GlobalLock);
  PublicPoW:=wp;
  writeln('HostKey: PoW found in ',(Now-start)*SecsPerDay:3:0,'s speed=',SizeToString(trunc(counter/((Now-start)*SecsPerDay))),'h/s');
  writeln('pow ',string(digest));
  Assert(VerifyPoW(PublicPoW,PublicKey));
  f.OpenRW(cSeckeyFN); f.Seek(72); {This is offset of PoW in hostkey file}
  f.Write(PublicPoW,sizeof(PublicPoW));
  f.Done;
  PoWGenBg.Timer;
  LeaveCriticalSection(GlobalLock);
  EndThread;
end;

procedure tPoWRefreshObj.Timer;
  begin
  PublicPoWReady:=true;
  if  (not VerifyPoW(PublicPoW,PublicKey))
  or  ( (Int64(PublicPoW.stamp)+cPoWValidFor-UnixNow) <600 )
  then begin
    writeln('HostKey: Started generating PoW');
    PublicPoWReady:=false;
    BeginThread(@PoWGenThr,nil);
  end
  else Shedule(120000,@timer);
end;

procedure Load;
  var f:tFileStream;
  var ident:array [1..8] of char;
  begin
  PublicPoWReady:=false;
  f.OpenRW(cSeckeyFN);
  try
    f.Read(ident,sizeof(cHostIdent));
    if CompareByte(ident,cHostIdent,8)<>0 then raise eInOutError.Create(cSeckeyFN+' invalid');
    f.Read(SecretKey,sizeof(SecretKey));
  except on e:eInOutError do begin
    writeln('HostKey: '+cSeckeyFN+' ',e.message,', Generating');
    f.Seek(0);
    GetOSRandom(@SecretKey,64);
    f.Write(cHostIdent,8);
    f.Write(SecretKey,64);
  end end;
  CreatekeyPair(PublicKey,SecretKey);
  writeln('HostKey: '+cSeckeyFN+' ',string(PublicKey));
  try f.Read(PublicPoW,sizeof(PublicPoW));
  except ; end;
  f.Done;
  PoWGenBg.Timer;
end;

BEGIN
 FillChar(ZeroDigest,sizeof(ZeroDigest),0);
  //writeln('HostKey: Today is D',TSNow);
  Load;
  //writeln('HostKey: ProofOfWork valid for W',BEtoN(PublicPow.Stamp));
END.
