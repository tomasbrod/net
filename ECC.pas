unit ECC;

INTERFACE
uses ed25519,Sha512,ObjectModel;
type tEccKey=tKey32;
type tPoWRec=packed record
 data:tKey32;
 stamp:LongWord; {NetOrder}
 end;

var SecretKey:tKey64;
var PublicKey:tEccKey;
var PublicPoW:tPoWRec;
var ZeroDigest:tSha512Digest;
const cDig3PowMask=%0010;
const cPoWValidDays=5;
const cTSEpoch=40179;
const cHostIdent:array [1..8] of char='BNHosSW'#26;
procedure CreateChallenge(out Challenge: tEccKey);
procedure CreateResponse(const Challenge: tEccKey; out Response: tKey32; const srce:tEccKey);
function VerifyPoW(const proof:tPoWRec; const RemotePub:tEccKey):boolean;

IMPLEMENTATION
uses SysUtils{,DateUtils},ServerLoop;

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

function VerifyPoW(const proof:tPoWRec; const RemotePub:tEccKey ):boolean;
 var shactx:tSha512Context;
 var delta:Integer;
 var digest:tSha512Digest;
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

const cSeckeyFN='hostkey.dat';

procedure PoWGenerate;
  var i:byte;
  var start:tDateTime;
  var digest:tSha512Digest;
  var shactx:tSha512Context;
  var wp:tPoWRec;
  var counter:LongWord absolute wp.data;
  begin
  wp.stamp:=NtoBE(TSNow);
  for i:=0 to 31 do wp.data[i]:=Random(256);
  Start:=Now; counter:=0;
  repeat
    if counter=high(counter) then raise eXception.Create('some shit happend');
    inc(counter);
    Sha512Init(shactx);
    Sha512Update(shactx,wp,sizeof(wp));
    Sha512Update(shactx,PublicKey,sizeof(PublicKey));
    Sha512Final(shactx,digest);
  until (CompareByte(digest,ZeroDigest,3)=0)and((digest[3]and cDig3PoWMask)=0);
  PublicPoW:=wp;
  writeln('ECC: PoW found in ',(Now-start)*SecsPerDay:3:0,'s speed=',SizeToString(trunc(counter/((Now-start)*SecsPerDay))),'h/s');
  Assert(VerifyPoW(PublicPoW,PublicKey));
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

type tPoWRefreshObj=object thrid:tThreadID; regen:boolean; procedure Timer; procedure Wait; end;
var PoWGenBg:tPoWRefreshObj;
function PoWGenThr(param:pointer):ptrint;
  var f:tFileStream;
  begin result:=0;
  SetThreadName('PoW_Gen');
  PoWGenerate;
  f.OpenRW(cSeckeyFN); f.Seek(72); {This is offset of PoW in hostkey file}
  f.Write(PublicPoW,sizeof(PublicPoW));
  f.Done;
end;
procedure tPoWRefreshObj.Wait;
  begin end;
procedure tPoWRefreshObj.Timer;
  var time:LongWord;
  begin
  time:=600000;
  if thrid>0 then begin
    write('ECC: Waiting for PoW to generate, this may take a while...'); flush(output);
    WaitForThreadTerminate(thrid,high(longint));
    thrid:=0; writeln; end
  else if regen or((TSNow-BEtoN(PublicPow.Stamp))>=cPoWValidDays) then begin
    writeln('ECC: Started generating PoW');
    thrid:=BeginThread(@PoWGenThr,nil);
    {ThreadSetPriority(thrid,-15);}
    if regen then time:=5 else time:=160000;
     regen:=false;
  end;  Shedule(Time,@Timer);
end;

procedure Load;
  var f:tFileStream;
  var ident:array [1..8] of char;
  var regen:boolean;
  begin
  write();
  f.OpenRW(cSeckeyFN);
  try
    f.Read(ident,sizeof(cHostIdent));
    if CompareByte(ident,cHostIdent,8)<>0 then raise eInOutError.Create(cSeckeyFN+' invalid');
    f.Read(SecretKey,sizeof(SecretKey));
  except on e:eInOutError do begin
    writeln('ECC: '+cSeckeyFN+' ',e.message,', Generating');
    f.Seek(0);
    GetOSRandom(@SecretKey,64);
    f.Write(cHostIdent,8);
    f.Write(SecretKey,64);
  end end;
  CreatekeyPair(PublicKey,SecretKey);
  writeln('ECC: '+cSeckeyFN+' ',string(PublicKey));
  try f.Read(PublicPoW,sizeof(PublicPoW));
      if not VerifyPoW(PublicPoW,PublicKey) then raise eInOutError.Create('invalid or expired');
      PoWGenBg.regen:=false;
  except on e:eInOutError do PoWGenBg.regen:=true; end;
  f.Done;
  PoWGenBg.thrid:=0;
  PoWGenBg.Timer;
end;

BEGIN
 FillChar(ZeroDigest,sizeof(ZeroDigest),0);
 TSNow:=trunc(Now-cTSEpoch);
  //writeln('ECC: Today is D',TSNow);
  Load;
  //writeln('ECC: ProofOfWork valid for W',BEtoN(PublicPow.Stamp));
END.
