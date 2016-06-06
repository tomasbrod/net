program bnedit;
(*command line program for brodnet
  view and edit all brodnet files
  bnedit file.dat : decode and display
  bnedit prof[ile] profile.dat login.dat : edit profile file
  bnedit login login.dat cert.dat : generate login from cert
  bnedit host host-key.dat : generate host key or renew PoW
*)

USES Profile,MemStream,NetAddr,SysUtils,ed25519,Sha512;

const
  cCertKeyIdent:array [1..8] of char='BNCertS'#26;
  cLoginIdent:array [1..8] of char='BNLogin'#26;
  cHostIdent:array [1..8] of char='BNHosSW'#26;

const helphint='Run bnedit without parameters to get help.';

type tFileStream=object(tCommonStream)
  f:tHandle;
  procedure Seek(absolute:LongWord); virtual;
  procedure Read(out buf; cnt:Word); virtual;
  procedure Write(const buf; cnt:word); virtual;
  function  Length:LongWord; virtual;
  function  Tell:LongWord; virtual;
  constructor OpenRO(const fn:string);
  constructor OpenRW(const fn:string);
  destructor Done;
end;

function GetPubFromNodeKey(const fn:string; save_secret:pointer=nil ):tKey32;
  var s:tFileStream;
  var sec:tKey64;
  begin
  s.OpenRO(fn); try
  s.read(sec,64);
  finally s.done end;
  CreateKeyPair(result, sec);
  if assigned(save_secret) then Move(sec,save_secret^,64);
  FillChar(sec,sizeof(sec),0);
end;

procedure LoadUserKey(var sf:tFileStream; out LoginSec:tKey64; var pf:tProfileRead);
  var hash:tsha512context;
  var Ident:array [1..8] of char;
  var pas:string[56];
  begin
  {  if flen=64 then begin
      BlockRead(sf,LoginSec,64);
      CreateKeyPair(pf.key.sign,LoginSec);
      writeln('Loaded simple User key');
      pf.key.cert:=pf.key.sign;
      pf.key.expire:=$FFFFFFFF;
      Sign(pf.key.sigkeycert, pf.key, sizeof(pf.key)-64, pf.key.cert, LoginSec);
    end
  }
  write('Load login key: ');
  sf.Read(ident,8);
  {...}
  sf.Read(pf.key.cert,32);
  sf.Read(LoginSec,64);
  sf.Read(pf.key.expire,68);
  writeln('waiting for password...');
  readln(pas);
  SHA512Init(hash);SHA512Update(hash,LoginSec,64);
  SHA512Update(hash,pas[1],length(pas));
  FillChar(pas,sizeof(pas),0);
  SHA512Final(hash,LoginSec,64);
  CreateKeyPair(pf.key.sign,{<-}LoginSec);
  if not Verify(pf.key.sigkeycert, pf.key, sizeof(pf.key)-64, pf.key.cert)
  then raise eFormatError.Create('Wrong password or corrupted file.');
  writeln('ok, CertKey pub=',string(pf.key.cert){,' expire});
  pf.sigexpire:=tDateTime(DWORD(pf.key.expire))+40179;
end;

procedure tFileStream.Seek(absolute:LongWord);
  begin if FileSeek(f,absolute,fsFromBeginning)<>absolute
  then raise eInOutError.Create('File Seek Error'); end;
procedure tFileStream.Read(out buf; cnt:Word);
  begin  if FileRead(f,buf,cnt)<>cnt
  then raise eInOutError.Create('File Read Error'); end;
procedure tFileStream.Write(const buf; cnt:word);
  begin if FileWrite(f,buf,cnt)<>cnt
  then raise eInOutError.Create('File Write Error'); end;
constructor tFileStream.OpenRO(const fn:string);
  begin
  Inherited Init;
  f:=FileOpen(fn, fmOpenRead);
  if f=-1 then raise eInOutError.Create('File Open for reading Error');
end;
function tFileStream.Length:LongWord;
  var pos:LongInt;
  begin
  pos:=FileSeek(f,0,fsFromCurrent);
  result:=FileSeek(f,0,fsFromEnd);
  Seek(pos);
end;
function tFileStream.Tell:LongWord;
  begin
  result:=FileSeek(f,0,fsFromCurrent);
end;
constructor tFileStream.OpenRW(const fn:string);
  begin
  Inherited Init;
  f:=FileOpen(fn, fmOpenReadWrite);
  if f=-1 then f:=FileCreate(fn, %0110000000); {mode: -rw-------}
  if f=-1 then raise eInOutError.Create('File Open read/write or Create Error');
end;
destructor tFileStream.Done;
  begin FileClose(f); end;

procedure DumpProfile(var txt:text; var pfs:tFileStream);
  var pf:tProfileRead;
  begin
  pfs.Seek(0);
  pf.ReadFrom(pfs,0);
  Writeln('ID: ',string(pf.ProfID));
  Writeln('Version: ',pf.Version,' date ',DateTimeToStr(pf.Modified));
  writeln('Nick: ',pf.nick);
  Writeln('FullName: ',string(pf.FullName));
  Writeln('TextNote: ',pf.TextNote);
  Writeln('PubKeys C/S/E: ',string(pf.key.cert),'/',string(pf.key.sign),'/',string(pf.enckey));
  Writeln('Count of hosts/backups/files: ',pf.hostsCount,'/',pf.backupCount,'/',pf.filesCount);
  writeln('Valid: ',pf.Valid{,' until: ',DateTimeToStr(pf.sigexpire)});
  //valid,CertExpired,CertInval,SigInval:boolean;
end;

procedure DisplayInfoAboutFile(whatfile:string);
  var s:tFileStream;
  var ident:packed array [1..8] of char;
  var tokyoident:array [1..8] of char='ToKyO Ca';
  var hkvsident:array [1..3] of char='Bd'#26;
  var stpakoident:array [1..8] of char='BNStPAK'#26;
  var i:integer;
  begin
  s.OpenRO(whatfile);
  try
  s.Read(ident,8);
  if CompareByte(ident,Profile.cMagic,7)=0 then begin
    writeln(whatfile,': BrodNet Profile v',byte(ident[8]));
    DumpProfile(stdout,s);
  end else
  if CompareByte(ident,tokyoident,8)=0 then begin
    writeln(whatfile,': Tokyo Cabinet database file');
  end else
  if CompareByte(ident,hkvsident,3)=0 then begin
    writeln(whatfile,': HKVS (old brodnet db)');
  end else
  if CompareByte(ident,stpakoident,8)=0 then begin
    writeln(whatfile,': BrodNet Object Store Pack');
  end else
  if CompareByte(ident,cCertKeyIdent,8)=0 then begin
    writeln(whatfile,': BrodNet Login CertSecret');
    writeln('This file is used to CHANGE YOUR login PASSWORD.');
    writeln('Please copy this file to safe removable media and then securely delete it whith "shred".');
  end else
  if CompareByte(ident,cLoginIdent,8)=0 then begin
    writeln(whatfile,': BrodNet Login');
    writeln('This file is used to edit your profile.');
  end else
  if CompareByte(ident,cHostIdent,8)=0 then begin
    writeln(whatfile,': BrodNet Host key');
  end else
  begin
    write(whatfile,': Unknown File, ');
    for i:=1 to 8 do begin
      if ident[i] in [' '..'~']
      then write(ident[i])
      else write('_');
    end;
    writeln;
    writeln('Try "file" command to get more information.');
  end;
  finally
    s.Done;
  end;
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

procedure GenerateLoginFile;
  var lf,cf:tFileStream;
  var gencert:boolean;
  var certsec,loginsec:tKey64;
  var ident:array [1..8] of char;
  var hash:tSha512Context;
  var pas:string[47];
  var keys:Profile.tProfKeys;
  begin
  if paramcount<>3 then begin writeln(helphint); halt(9) end;
  try
    cf.OpenRO(paramstr(3));gencert:=false;
  except
    cf.OpenRW(paramstr(3));gencert:=true;
  end;
  if GenCert then begin
    write('Generate random CertKey: ');
    GetOSRandom(@certsec,64);
    CreateKeyPair(keys.cert,certsec);
    cf.Write(cCertKeyIdent,8);
    cf.Write(certsec,64);
  end else begin
    write('Load CertKey: ');
    if cf.Length>=72 then begin
      cf.Read(ident,8);
      if CompareByte(ident,cCertKeyIdent,8)<>0
      then raise eFormatError.Create('CertSecret file is Invalid or Corrupted');
    end else write('(old format) ');
    cf.Read(certsec,64);
    CreateKeyPair(keys.cert,certsec);
  end;
  writeln('pub=',string(keys.cert));
  cf.Done;
  lf.OpenRW(paramstr(2));
  writeln('New Password...');
  readln(pas);
  write('Generate random LoginKey: ');
  GetOSRandom(@loginsec,64);
  FileTruncate(lf.f,0);
  lf.Write(cLoginIdent,8);
  lf.Write(keys.cert,32);
  lf.Write(loginsec,64);
  keys.expire:=$FFFFFFFF;
  sha512init(hash);
  sha512update(hash,loginsec,64);
  sha512update(hash,pas[1],length(pas));
  sha512final(hash,loginsec);
  CreateKeyPair(keys.sign,loginsec);
  writeln('pub=',string(keys.sign));
  Sign(keys.sigkeycert, keys, sizeof(keys)-64, keys.cert, CertSec);
  FillChar(CertSec,sizeof(CertSec),0);
  FillChar(loginsec,sizeof(loginsec),0);
  lf.Write(keys.expire,68 {exp+sig});
  lf.Done;
  writeln('*-For the Love of Gaben, store ',paramstr(3));
  writeln('*-on safe removable media or delete it if you want,');
  writeln('*-becouse it can be used to change your password');
  writeln('*-and edit your profile and destroy everything!');
end;

function CheckNick(nick:string):boolean;
  var i:integer;
  begin
  result:=false;
  if length(nick)>8 then exit;
  for i:=1 to length(nick) do if nick[i] in [#0..#31,' ',#127..#255]
    then exit;
  result:=true;
end;

procedure EditProfile;
  var pfs,ls:tFileStream;
  var pf:tProfileRead;
  var pubkey:tKey32;
  var LoginSec:tKey64;
  var cmd,param:string;
  var tmpp:LongWord;
  begin
  if paramcount<>3 then begin writeln(helphint); halt(9) end;
  try
    pfs.OpenRW(paramstr(2));
    if pfs.Length=0 then begin
      pf.InitEmpty;
      writeln('Initialize empty profile');
    end else begin
      pf.ReadFrom(pfs,0);
      writeln('Loaded Profile');
    end;
  except
    on e:exception do begin
      writeln(paramstr(2)+': '+e.Message);
      writeln('If you specify non-existent file as parameter 2, empty profile will be created');
      halt(2);
    end;
  end;
  try
    ls.OpenRO(paramstr(3));
    LoadUserKey(ls,LoginSec,pf);
  except
    on e:exception do begin
      writeln(paramstr(3)+': '+e.Message);
      writeln('You can (re-)generate your login. ', helphint);
      halt(3);
    end;
  end;
  pf.Version:=pf.Version+1;
  writeln('waiting for commands...');
  repeat
    readln(input,cmd);
    if (cmd='')or(cmd='QUIT')or(cmd='SAVE')or(cmd='EXIT') then break;
    tmpp:=pos(' ',cmd);
    if tmpp>0 then begin
      param:=copy(cmd,tmpp+1,255);
      if cmd[tmpp-1]=':' then dec(tmpp);
      cmd:=copy(cmd,1,tmpp-1);
    end else param:='';
    cmd:=upcase(cmd);
    if cmd='NAME' then begin
      pf.FullName:=param;
    end else if cmd='NICK' then begin
      if CheckNick(param)
      then pf.nick:=param
      else writeln('Error: Invalid Nickname. Max 8 characters and ASCII only [#33..#126].');
    end else if cmd='NOTE' then begin
      pf.TextNote:=param;
    end else if cmd='VERSION' then begin
      val(param,tmpp);
      if tmpp>DWORD(pf.Version) then pf.Version:=tmpp
      else writeln('Error: VERSION field must be set greater than previous');
    end else if (cmd='HELP')or(cmd='?') then begin
      writeln('--commands available for profile editing--');
      writeln('  syntax: COMMAND parameter...');
      writeln('  semicolon in COMMAND is ignored, command is not case-sensitive');
      writeln('NAME',' Full Name ;set your name');
      writeln('NICK',' nick ;set your nick name, max 8 ASCII chars #32..#126');
      writeln('NOTE',' text ;set additional text, max 255 bytes');
      writeln('VERSION',' number ;override version field');
      writeln('EXIT',' ;save changes and exit (empty line equals EXIT)');
    end else if (cmd='')or(cmd='QUIT')or(cmd='SAVE')or(cmd='EXIT')
    then break
    else writeln('Error: Unknown command: ',cmd,' (use HELP command)');
  until EOF(input);
  pfs.Seek(0);
  FileTruncate(pfs.f,0);
  pf.Modified:=Now;
  pf.WriteTo(pfs,LoginSec);
  FillChar(loginsec,sizeof(loginsec),0);
  writeln('Changes Saved.');
end;

{$I gitver.inc}
const eoln=LineEnding;
const helptext:ansistring
=eoln
+'bnedit file.dat : decode and output contents of the file'+eoln
+'bnedit prof profile.dat login.dat : edit profile file'+eoln
+'bnedit login login.dat certsecret.dat : generate login from certsecret'+eoln
{+'bnedit host host-key.dat : generate host key or renew PoW'+eoln}
+eoln
+'A random certsecret.dat will be created if it does not exist.'+eoln
+'"prof" command reads commands from input, use HELP to get available commands.'+eoln
+eoln
;
BEGIN
  if paramcount=0 then begin
    writeln('No parameters! Here read help text.');
    writeln('bnedit is BrodNet file Viewer and Editor version '+GIT_VERSION+' build ',BUILD_VERSION,'@'+BUILD_HOST);
    write(helptext);
    halt(9);
  end;
  if paramcount=1 then begin
    try
      DisplayInfoAboutFile(paramstr(1));
    except
      on e:eInOutError do begin
        writeln('DisplayInfoAboutFile('+paramstr(1)+'): IO Error: '+e.Message);
        writeln(helphint);
      end;
    end;
  end;
  if paramcount>=2 then case paramstr(1) of
    'login':GenerateLoginFile;
    'prof','profile':EditProfile;
    else begin
      writeln('Unknown operation! Here read help text.');
      write(helptext);
      halt(9);
    end;
  end;
END.

