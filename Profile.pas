UNIT Profile;

INTERFACE
USES ObjectModel,SysUtils;

const cMagic:packed array [1..7] of char='BNProf'#26;
const cFormatVer=6;
type tProfID=tKey20;

type tProfileRead=object
  Valid: Boolean;
  ProfID: tKey20;
  format: byte;
  nick: string[12];
  expires: Int64;
  master_key, sign_key, encr_key, chat_key: tKey32;
  cOldKeys: byte;
  OldKeys: ^tKey32;
  Fullname: AnsiString;
  master_sig: tKey64;
  updated: Int64;
  cHosts,cBackups:byte;
  Hosts,Backups: ^tKey20;
  Textnote: AnsiString;
  cert_sig: tKey64;
  signature: tKey64;
  constructor ReadFrom(var src:tCommonStream; parseLevel:byte);
  constructor InitEmpty;
  procedure DoMasterSign(const priv:tKey64);
  procedure WriteTo(var dst:tCommonStream; const priv:tKey64);
  destructor Done;
  {0=readAll+Verify}
end;

IMPLEMENTATION
uses Sha512,ed25519;

type tHashingStream = object (tCommonStream)
  driver: ^tCommonStream;
  ctx: tSha512Context;
  procedure Read(out buf; cnt:Word); virtual;
  procedure Write(const buf; cnt:word); virtual;
  constructor Init(var idriver:tCommonStream);
end;

procedure tHashingStream.Read(out buf; cnt:Word);
  begin
  driver^.Read(buf,cnt);
  sha512update(ctx,buf,cnt);
end;
procedure tHashingStream.Write(const buf; cnt:Word);
  begin
  driver^.Write(buf,cnt);
  sha512update(ctx,buf,cnt);
end;

constructor tHashingStream.Init(var idriver:tCommonStream);
  begin
  inherited Init;
  driver:=@idriver;
  sha512init(ctx);
end;

constructor tProfileRead.InitEmpty;
  begin
  TextNote:=''; FullName:='';
  hosts:=nil;
  valid:=false;
  cHosts:=0;cBackups:=0;cOldKeys:=0;
  FillChar(nick,sizeof(nick),0);
end;

function ReadLength1Value(var m:tCommonStream; out count:byte; size:longword):pointer;
  begin
  count:=m.ReadByte;
  size:=count*size;
  if count>0 then begin
    result:=GetMem(size);
    m.Read(result^,size);
  end else result:=nil;
end;

constructor tProfileRead.ReadFrom(var src:tCommonStream; parseLevel:byte);
  var Magic:packed array [1..8] of char;
  var i:longword;
  var m,m2:tHashingStream;
  begin
  InitEmpty;
  m.Init(src);
  m.Read(Magic,8);
  if CompareByte(Magic,cMagic,7)<>0
    then raise eFormatError.create('Invalid magic sequence');
  format:=byte(Magic[8]);
  {$IFDEF profLoadOldFormat}
  if Format=5 then begin
    {load into tMemoryStream for better parformace}
    version:=src.ReadWord4;
    Updated:=src.ReadWord4+40179;
    src.Read(nick[1],8);SetLength(nick,8);
    for i:=1 to 8 do if nick[i]=#0 then break;SetLength(nick,i-1);
    src.Read(key,sizeof(key));
    sigexpire:=tDateTime(dword(key.expire))+40179;
    src.Read(enckey,32);
    fullname:=src.ReadShortString;
    if parselevel<9 then begin
      hostsCount:=src.ReadByte;
      backupCount:=src.ReadByte;
      hosts:=GetMem((backupCount+hostsCount)*20);
      src.Read(hosts^,(backupCount+hostsCount)*20);
      textnote:=src.ReadShortString;
      //filerefs:array[b1] of 2,20 TODO
      SHA512Buffer(key.cert, 32, ProfID, 20 );
    end;
    if parseLevel=0 then begin
      //TODO: check signature
      {$warning SECURITY VIOLATION, signature check not implemented}
    end;
  end else{$ENDIF}
  if Format=6 then begin
    valid:=true;
    m.Read(nick[1],12);
    i:=1; while (i<=12)and(nick[i]<>#0) do inc(i); SetLength(nick,i-1);
    updated:=m.ReadWord6;
    m2.Init(m);
    expires:=m2.ReadWord6;
    m2.Read(master_key,32);
    m2.Read(sign_key,32);
    m.Read(master_sig,64);
    valid:=valid and (updated<UnixNow) and (expires>UnixNow);
    valid:=valid
      and ed25519.Verify2(m2.ctx, master_sig, master_key);
    SHA512Buffer(master_key, 32, ProfID, 20 );
    m.Read(encr_key,32);
    m.Read(chat_key,32);
    OldKeys:=ReadLength1Value(m,cOldKeys,32);
    Fullname:=m.ReadShortString;
    Hosts:=ReadLength1Value(m,cHosts,20);
    Backups:=ReadLength1Value(m,cBackups,32);
    Textnote:=m.ReadShortString;
    src.Read(signature,64);
    valid:=valid
      and ed25519.Verify2(m.ctx, signature, sign_key);
  end else raise eFormatError.create('Unknown format  version');
end;

destructor tProfileRead.Done;
  begin
  FreeMem(OldKeys);
  FreeMem(Hosts);
  FreeMem(Backups);
  SetLength(TextNote,0);
end;

procedure tProfileRead.WriteTo(var dst:tCommonStream; const priv:tPrivKey);
  var s:tMemoryStream;
  begin
  s.Init(9999);//FIXME
  try
  with s do begin
    Write(cMagic,7);
    WriteByte(6);
    Write(nick[1],12);
    WriteWord6(updated);
    WriteWord6(expires);
    Write(master_key,32);
    Write(sign_key,32);
    Write(master_sig,64);
    Write(encr_key,32);
    Write(chat_key,32);
    WriteByte(cOldKeys);
    Write(OldKeys^,cOldKeys*32);
    WriteShortString(Fullname);
    WriteByte(cHosts);
    Write(Hosts^,cHosts*20);
    WriteByte(cBackups);
    Write(Backups^,cBackups*20);
    WriteShortString(Textnote);
  end;
  s.Seek(0);
  dst.Write(s.RdBuf^, s.RdBufLen);
  Sign(signature, s.RdBuf^, s.RdBufLen, sign_key, priv);
  dst.Write(signature,64);
  finally s.Free; end;
end;

procedure tProfileRead.DoMasterSign(const priv:tPrivKey);
  var s:tMemoryStream;
  begin
  s.Init(70);
  try
  with s do begin
    WriteWord6(expires);
    Write(master_key,32);
    Write(sign_key,32);
  end;
  s.Seek(0);
  Sign(master_sig, s.RdBuf^, s.RdBufLen, master_key, priv);
  finally s.Free; end;
end;

END.
