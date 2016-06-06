UNIT Profile;

INTERFACE
USES NetAddr,MemStream,SysUtils;

const cMagic:packed array [1..7] of char='BNProf'#26;
const cFormatVer=5;
type tProfID=tKey20;

type tProfKeys=packed record
  cert:tKey32;
  sign:tKey32;
  expire:Word4;
  sigkeycert:tKey64;
end;

type tProfileRead=object
  format:byte;
  version:LongWord;
  Modified:tDateTime;
  nick:string[8];
  ProfID:tProfID;
  sigexpire:tDateTime;
  key:tProfKeys;
  enckey:tKey32;
  valid,CertExpired,CertInval,SigInval:boolean;
  FullName:string[79];
  TextNote:ansistring;
  hosts:^tKey20;
  hostsCount,backupCount,filesCount:byte;
  files:^tKey20;
  files_id:^Word;
  constructor ReadFrom(var src:tCommonStream; parseLevel:byte);
  constructor InitEmpty;
  procedure WriteTo(var dst:tCommonStream; const priv:tKey64);
  destructor Done;
  {0=readAll+sigvfy 1=readAll 9=fixed+name 99=vfy-only}
end;

{File Struct
identifier:string[7]
format:b1
version:w4
daymodif:w4
nick:string[8]
certkey:32
sigkey:32
sigexpire:w4
sigkeycert:64
enckey:32
--end of fixed length--
fullname:string[b1]
hosts_count:b1;
backups_count:b1;
hosts:array[hosts_count+backups_count] of 20
textnote:string[b1]
filerefs:array[b1] of 2,20
signature:64
}

IMPLEMENTATION
uses Sha512,ed25519;

constructor tProfileRead.InitEmpty;
  begin
  SetLength(TextNote,0);
  hosts:=nil;files:=nil;files_id:=nil;
  hostsCount:=0;backupCount:=0;filesCount:=0;
  valid:=false;
  FillChar(nick,sizeof(nick),0);
  modified:=40179;
  sigexpire:=40179;
end;

constructor tProfileRead.ReadFrom(var src:tCommonStream; parseLevel:byte);
  var Magic:packed array [1..8] of char;
  var i:longword;
  begin
  InitEmpty;
  src.Read(Magic,8);
  if CompareByte(Magic,cMagic,7)<>0
    then raise eFormatError.create('Invalid magic sequence');
  format:=byte(Magic[8]);
  if Format=5 then begin
    {load into tMemoryStream for better parformace}
    version:=src.ReadWord4;
    Modified:=src.ReadWord4+40179;
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
  end else raise eFormatError.create('Unknown format  version');
end;

destructor tProfileRead.Done;
  begin
  SetLength(TextNote,0);
  if assigned(hosts) then FreeMem(hosts);
  if assigned(files) then FreeMem(files);
  if assigned(files_id) then FreeMem(files_id);
end;

procedure tProfileRead.WriteTo(var dst:tCommonStream; const priv:tPrivKey);
  var s:tMemoryStream;
  var signature:tSig;
  begin
  s.Init(9999);//FIXME
  with s do begin
  Write(cMagic,7);
  WriteByte(5);
  WriteWord4(Version);
  WriteWord4(system.trunc(Modified)-40179);
  Write(nick[1],8);
  //system.trunc(sigexpire)-40179;
  Write(key,sizeof(key));
  Write(enckey,32);
  WriteShortString(fullname);
  WriteByte(hostsCount);
  WriteByte(backupCount);
  Write(hosts^,(backupCount+hostsCount)*20);
  WriteShortString(textnote);
  FilesCount:=0; WriteByte(filesCount);
  //filerefs:array[b1] of 2,20 TODO
  end;
  Sign(signature, s.base^, s.vlength, key.sign, priv);
  dst.Write(s.base^, s.vlength);
  dst.Write(signature,64);
end;

END.
