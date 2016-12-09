program bnedit;
(*command line program for brodnet
  view and edit all brodnet files
  bnedit file.dat : decode and display
  bnedit prof[ile] profile.dat login.dat : edit profile file
  bnedit login login.dat cert.dat : generate login from cert
  bnedit host host-key.dat : generate host key or renew PoW
*)

USES Profile,ObjectModel,SysUtils,DateUtils,ed25519,Sha512,blowfish,gitver;

{$linklib c}

const
  cCertKeyIdent:array [1..8] of char='BNCertS'#26;
  cMasterKeyIdent:array [1..8] of char='BNMastS'#26;
  cLoginIdent:array [1..8] of char='BNLogin'#26;
  cSecretIdent:array [1..8] of char='BNSecKs'#26;
  cHostIdent:array [1..8] of char='BNHosSW'#26;

const helphint='Run bnedit without parameters to get help.';

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

function UnixTimeToStr(u:Int64):ansistring;
  begin
  DateTimeToString(result, 'dd.mm.yyyy hh:nn', UnixToDateTime(u));
end;

procedure DumpProfile(var txt:text; var pfs:tFileStream);
  var pf:tProfileRead;
  begin
  pfs.Seek(0);
  pf.ReadFrom(pfs,0);
  Writeln('ID: ',string(pf.ProfID));
  Writeln('Updated: ',UnixTimeToStr(pf.updated));
  Writeln('Expires: ',UnixTimeToStr(pf.expires));
  writeln('Nick: ',pf.nick);
  Writeln('FullName: ',pf.FullName);
  Writeln('TextNote: ',pf.TextNote);
  writeln('Valid: ',pf.Valid);
  //CertExpired,CertInval,SigInval:boolean;
end;
procedure ShowSecretID(var pfs:tFileStream);
  var pk:tKey32;
  var id:tKey20;
  begin
  pfs.Skip(8+6);
  pfs.Read(pk,32);
  SHA512BUFFER(pk,32,id,20);
  writeln('ID: ',string(id));
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
    writeln(whatfile,': BrodNet Login CertSecret (old format)');
    writeln('Unencrypted user master key.');
    writeln('Change password to convert to new format.');
  end else
  if CompareByte(ident,cLoginIdent,8)=0 then begin
    writeln(whatfile,': BrodNet User Login (old format)');
  end else
  if CompareByte(ident,cMasterKeyIdent,8)=0 then begin
    writeln(whatfile,': BrodNet User Master Secret Key');
  end else
  if CompareByte(ident,cSecretIdent,8)=0 then begin
    writeln(whatfile,': BrodNet User Secret Keyring');
    writeln('This file is used to edit your profile,'#10+
    'decrypt your messages and identify you.');
    ShowSecretID(s);
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

procedure EncryptSaveMasterKey(var mf: tCommonStream; var mastersec:tKey64);
  var enc: tKey64;
  var digest: tKey32;
  var fv: tKey8;
  var exk: tBlowfishKey;
  var i: integer;
  var pwd:string;
  begin
  write('Enter new password for master key: ');
  readln(pwd);
  if (length(pwd)<4)or(length(pwd)>56) then begin
    writeln('Error: password must be 4 to 56 bytes long');
    halt(3);
  end;
  BlowFish.ExpandKey(exk, pwd[0], length(pwd));
  FillChar(fv,sizeof(fv),0);
  for i:=0 to 7
    do blowfish.EncryptCBC(enc[i*8], mastersec[i*8], exk, fv);
  ed25519.CreateKeyPair(digest,mastersec);
  Sha512Buffer(digest,32,digest,32);
  FillChar(masterSec,sizeof(masterSec),0);
  mf.Write(cMasterKeyIdent,8);
  mf.Write(enc,64);
  mf.Write(digest,32);
end;

procedure LoadDecryptMasterKey(var mf: tCommonStream; out mastersec:tKey64; out master:tKey32);
  var enc: tKey64;
  var digest, digest2: tKey32;
  var fv: tKey8;
  var exk: tBlowfishKey;
  var i: integer;
  var pwd:string;
  var ident:array [1..8] of char;
  begin
  mf.Read(Ident,8);
  if CompareByte(ident,cCertKeyIdent,8)=0 then begin
    mf.Read(mastersec,64);
    ed25519.CreateKeyPair(master,mastersec);
  end else
  if CompareByte(ident,cMasterKeyIdent,8)=0 then begin
    mf.Read(enc,64);
    mf.Read(digest,32);
    write('Enter password for master key: ');
    readln(pwd);
    if (length(pwd)<4)or(length(pwd)>56) then begin
      writeln('Error: password must be 4 to 56 bytes long');
      halt(3);
    end;
    BlowFish.ExpandKey(exk, pwd[0], length(pwd));
    FillChar(fv,sizeof(fv),0);
    for i:=0 to 7
      do blowfish.DecryptCBC(mastersec[i*8], enc[i*8], exk, fv);
    ed25519.CreateKeyPair(master,mastersec);
    Sha512Buffer(master,32,digest2,32);
    if CompareByte(digest,digest2,32)<>0 then begin
      writeln('Decryption failed. Invalid password or corrupt file.');
      halt(3);
    end;
  end else raise eFormatError.Create('Invalid Master key file');
end;

procedure EditKeyGen; {bnedit keygen secret.dat master.dat}
  var lf,mf:tFileStream;
  var genmaster:boolean;
  var mastersec:tKey64;
  var loginsec: tkey64;
  var signature: tKey64;
  var dbg: tKey20;
  var ld:packed record
    expires: word6;
    master: tKey32;
    login: tKey32;
    end;
  begin
  if paramcount<>3 then begin writeln(helphint); halt(9) end;
  try
    mf.OpenRO(paramstr(3));genmaster:=false;
  except
    mf.OpenRW(paramstr(3));genmaster:=true;
  end;
  if Genmaster then begin
    writeln('Generating new master key');
    GetOSRandom(@mastersec,64);
    EncryptSaveMasterKey(mf, mastersec);
  end;
  mf.Seek(0);
  LoadDecryptMasterKey(mf, mastersec, ld.master);
  writeln('Master Key loaded, pub=',string(ld.master));
  mf.Done;
  write  ('Generate login key ');
  GetOSRandom(@loginsec,64);
  CreateKeyPair(ld.login,loginsec);
  writeln('pub=',string(ld.login));
  ld.expires:=Word6(UnixNow + 15778463); {6 months}
  writeln('Expiration set to 6 months.');
  sha512buffer(ld,sizeof(ld),dbg,20);
  writeln('dbg: ',string(dbg));
  Sign(signature, ld, sizeof(ld), ld.master, masterSec);
  lf.OpenRW(paramstr(2)); //todo check ident bytes
  lf.Write(cSecretIdent,8);
  lf.Write(ld.expires,6);
  lf.Write(ld.master,32);
  lf.Write(signature,64);
  lf.Write(loginsec,64);
  lf.Done;
  FillChar(masterSec,sizeof(masterSec),0);
  writeln('* For the Love of Gaben, store ',paramstr(3));
  writeln('* on safe removable media and don''t forget your password,');
  writeln('* becouse it can be used to take over your identity!');
end;

procedure EditMasterPassword; {bnedit keygen master.dat}
  var mf:tFileStream;
  var mastersec: tKey64;
  var master: tKey32;
  begin
  if paramcount<>2 then begin writeln(helphint); halt(9) end;
  mf.OpenRW(paramstr(2));
  LoadDecryptMasterKey(mf, mastersec, master);
  writeln('Master Key loaded, pub=',string(master));
  mf.seek(0);
  EncryptSaveMasterKey(mf, mastersec);
  mf.Done;
  FillChar(masterSec,sizeof(masterSec),0);
  writeln('* For the Love of Gaben, store ',paramstr(3));
  writeln('* on safe removable media and don''t forget your password,');
  writeln('* becouse it can be used to take over your identity!');
end;

function CheckNick(var nick:string):boolean;
  var i:integer;
  begin
  result:=false;
  if length(nick)>12 then exit;
  for i:=1 to length(nick) do if nick[i] in [#0..#31,' ',#127..#255]
    then exit;
  result:=true;
end;

procedure EditProfile;
  var pfs,ls:tFileStream;
  var pf:tProfileRead;
  var Ident:array [1..8] of char;
  var LoginSec:tKey64;
  var cmd,param:string;
  var tmpp:LongWord;
  begin
  if paramcount<>3 then begin writeln(helphint); halt(9) end;
  try
    pfs.OpenRW(paramstr(2));
    if pfs.Length=0 then begin
      pf.InitEmpty;
      writeln('Initialized empty profile');
    end else begin
      pf.ReadFrom(pfs,0);
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
    ls.Read(ident,8);
    {todo: vfy ident}
    pf.expires:=ls.ReadWord6;
    ls.Read(pf.master_key,32);
    ls.Read(pf.master_sig,64);
    ls.Read(LoginSec,64);
    CreateKeyPair(pf.sign_key,{<-}LoginSec);
    {todo: verify}
  except
    on e:exception do begin
      writeln(paramstr(3)+': '+e.Message);
      writeln('You can (re-)generate your login. ', helphint);
      halt(3);
    end;
  end;
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
      else writeln('Error: Invalid Nickname. Max 12 characters and ASCII only [#33..#126].');
    end else if cmd='NOTE' then begin
      pf.TextNote:=param;
    end else if (cmd='HELP')or(cmd='?') then begin
      writeln('--commands available for profile editing--');
      writeln('  syntax: COMMAND parameter...');
      writeln('  semicolon in COMMAND is ignored, command is not case-sensitive');
      writeln('NAME',' Full Name ;set your name');
      writeln('NICK',' nick ;set your nick name, max 8 ASCII chars #32..#126');
      writeln('NOTE',' text ;set additional text, max 255 bytes');
      writeln('EXIT',' ;save changes and exit (empty line equals EXIT)');
    end else if (cmd='')or(cmd='QUIT')or(cmd='SAVE')or(cmd='EXIT')
    then break
    else writeln('Error: Unknown command: ',cmd,' (use HELP command)');
  until EOF(input);
  pfs.Seek(0);
  pfs.Trunc(0);
  pf.Updated:=UnixNow;
  pf.WriteTo(pfs,LoginSec);
  FillChar(loginsec,sizeof(loginsec),0);
  writeln('Changes Saved.');
end;

const eoln=LineEnding;
const helptext:ansistring
=eoln
+'bnedit file.dat : decode and output contents of the file'+eoln
+'bnedit prof profile.dat secret.dat : edit profile file'+eoln
+'bnedit keygen secret.dat master_secret.dat : generate secret key from master or both'+eoln
{+'bnedit host host-key.dat : generate host key or renew PoW'+eoln}
+eoln
+'A random master_secret.dat will be created if it does not exist.'+eoln
+'"prof" command reads commands from input, use HELP to get available commands.'+eoln
+eoln
;
BEGIN
  if paramcount=0 then begin
    writeln('No parameters! Here read help text.');
    writeln('bnedit is BrodNet file Viewer and Editor version '+GIT_VERSION);
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
    'keygen':EditKeyGen;
    'chpasswd':EditMasterPassword;
    'prof','profile':EditProfile;
    {prof profile secret command value...}
    {de[crypt] message sender_profile secret}
    {encrypt message input secret rcpt_profile...}
    else begin
      writeln('Unknown operation! Here read help text.');
      write(helptext);
      halt(9);
    end;
  end;
END.

bnedit prof profile.dat secret.dat name "Tomáš Brada" nick Brod 
