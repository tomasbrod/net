unit HostKey;

{BUG: PublicPoWReady is set to false when PoW starts regenerating}

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
{-DEFINE POW_ALT}
{$IFNDEF POW_ALT}
  {$IFDEF ENDIAN_LITTLE}{Intel x86}
const cPowMask0:DWORD=$FF0FFFFF;
  {$ELSE}{ARM shit}
const cPowMask0:DWORD=$FFFF0FFF;
  {$ENDIF}
const cPoWValidFor=5*{days}86400;
{$ELSE}
const cPowMask0:DWORD=$00FF0000;
const cPoWValidFor={1 hour}3600;
{$ENDIF}
const cHostIdent:array [1..8] of char='BNHosSW'#26;
procedure CreateChallenge(out Challenge: tKey32);
procedure CreateResponse(const Challenge: tKey32; out Response: tKey32; const srce:tEccKey);
function VerifyPoW(const proof:tPoWRec; const RemotePub:tEccKey):boolean;

IMPLEMENTATION
uses SysUtils{,DateUtils},ServerLoop,Classes,Database;
var log:tEventLog;

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
    raise eInOutError.Create('ERROR reading /dev/random');
    halt(8);
  end else FileClose(f);
  {$ELSE}
  begin
  {$WARNING Random on non-UNIX unimplemented}
  Log.Error(' No random source! Using all zeros, generated keys will be shit',[]);
  FillChar(out,0,cnt);
  {$ENDIF}
end;

type tPoWRefreshObj=object procedure Timer; end;
var PoWGenBg:tPoWRefreshObj;
const cSeckeyFN='hostkey.dat';
const cPowDK='hostpow';

function PoWGenThr(param:pointer):ptrint;
  var i:byte;
  var start:tDateTime;
  var shactx_initial:tSha256Context;
  var shactx:tSha256Context;
  var wp:packed record
    cnt:DWord;
    data:packed array [4..31] of byte;
    stamp:Word6;
    pk:tKey32;
  end;
  var digest: tKey32;
  var dig_4dw: dword absolute digest[0];
  var speed:qword;
  begin result:=0;
  assert(sizeof(wp)=70);
  assert(sizeof(PublicPoW)=38);
  SetThreadName('PoW_Gen');
  wp.stamp:=Word6(UnixNow);
  for i:=4 to 31 do wp.data[i]:=Random(256);
  SHA256_Init(shactx_initial);
  wp.pk:=PublicKey;
  Start:=Now; wp.cnt:=0;
  repeat
    if wp.cnt=high(wp.cnt) then raise eXception.Create('some shit happend');
    inc(wp.cnt);
    shactx:=shactx_initial;
    SHA256_Update(shactx,wp,sizeof(wp));
    SHA256_Final(digest,shactx);
  until (dig_4dw and cPowMask0) =0;
  EnterCriticalSection(GlobalLock);
  Move(wp,{->}PublicPoW,38);
  try
    speed:=trunc(wp.cnt*1000/(Now-start));
  except
    speed:=0;
  end;
  log.info(' PoW found in %.1Fs speed=%Sh/s',[(Now-start)*SecsPerDay,SizeToString(speed)]);
  log.debug(' pow %S',[string(digest)]);
  Assert(VerifyPoW(PublicPoW,PublicKey));
  Database.dbSet(dbMisc,cPowDK[1],length(cPowDK),@PublicPoW,sizeof(PublicPoW));
  PoWGenBg.Timer;
  LeaveCriticalSection(GlobalLock);
  EndThread;
end;

procedure tPoWRefreshObj.Timer;
  begin
  PublicPoWReady:=VerifyPoW(PublicPoW,PublicKey);
  if  (not PublicPoWReady)
  or  ( (Int64(PublicPoW.stamp)+cPoWValidFor-UnixNow) <600 )
  then begin
    log.info(' Started generating PoW',[]);
    //PublicPoWReady:=false;
    BeginThread(@PoWGenThr,nil);
  end
  else Shedule(120000,@timer);
end;

procedure Load;
  var f:tStream;
  var ident:array [1..8] of char;
  begin
  PublicPoWReady:=false;
  {$IFDEF POW_ALT}
  log.info(' Using aternate pow settings',[]);
  {$endif}
  try
    f:=tFileStream.Create(cSeckeyFN,fmOpenRead);
    try
      f.Read(ident,sizeof(cHostIdent));
      if CompareByte(ident,cHostIdent,8)<>0 then raise eInOutError.Create(cSeckeyFN+' invalid');
      f.Read(SecretKey,sizeof(SecretKey));
    finally
      f.Free;
    end;
  except on e:eInOutError do begin
    log.warn('.Load %S %S, Generating',[cSeckeyFN,e.message]);
    f:=tFileStream.Create(cSeckeyFN,fmCreate);
    try
      GetOSRandom(@SecretKey,64);
      f.Write(cHostIdent,8);
      f.Write(SecretKey,64);
    finally
      f.Free;
    end;
  end end;
  CreatekeyPair(PublicKey,SecretKey);
  log.info(' %S %S',[cSeckeyFN,string(PublicKey)]);
  f:=dbGet(dbMisc,cPoWDK,length(cPoWDK));
  try
    try f.Read(PublicPoW,sizeof(PublicPoW));
    except ; end;
  finally
    f.Free;
  end;
  PoWGenBg.Timer;
end;

BEGIN
  FillChar(ZeroDigest,sizeof(ZeroDigest),0);
  ServerLoop.CreateLog(log,'HostKey');
  //writeln('HostKey: Today is D',TSNow);
  Load;
  //writeln('HostKey: ProofOfWork valid for W',BEtoN(PublicPow.Stamp));
END.
