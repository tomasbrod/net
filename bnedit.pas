program bnedit;
(*command line program for brodnet
  view and edit all brodnet files
  bnedit file.dat : decode and display
  bnedit prof[ile] profile.dat login.dat : edit profile file
  bnedit login login.dat cert.dat : generate login from cert
  bnedit host host-key.dat : generate host key or renew PoW
*)

USES Profile,ObjectModel,SysUtils,DateUtils,ed25519,Crypto;

{$LINKLIB c}
{$INCLUDE gitver.inc}

const
  cCertKeyIdent:array [1..8] of char='BNCertS'#26;
  cMasterKeyIdent:array [1..8] of char='BNMastS'#26;
  cLoginIdent:array [1..8] of char='BNLogin'#26;
  cSecretIdent:array [1..8] of char='BNSecKs'#26;
  cHostIdent:array [1..8] of char='BNHosSW'#26;
  cMessageIdent:array [1..8] of char='BNMesag'#26;

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
  var i:integer;
  begin
  pfs.Seek(0);
  pf.ReadFrom(pfs,0);
  Writeln('ID: ',string(pf.ProfID));
  Writeln('Updated: ',UnixTimeToStr(pf.updated),' UTC');
  Writeln('Expires: ',UnixTimeToStr(pf.expires),' UTC');
  writeln('Nick: ',pf.nick);
  Writeln('FullName: ',pf.FullName);
  Writeln('TextNote: ',pf.TextNote);
  Writeln('Encryption key: ',string(pf.encr_key));
  Writeln('Old keys: ',pf.cOldKeys);
  for i:=0 to pf.cHosts-1
    do writeln('Host:       ',string(pf.Hosts[i]));
  for i:=0 to pf.cHosts-1
    do writeln('BackupHost: ',string(pf.Hosts[i]));
  writeln('Valid: ',pf.Valid);
  //CertExpired,CertInval,SigInval:boolean;
  pf.Done;
end;

const first_sec_ofs=8+6+32+64+64;

procedure ShowSecretID(var pfs:tFileStream);
  var pk:tKey32;
  var id:tKey20;
  var use:char;
  var left:LongWord;
  var msg:string;
  begin
  pfs.Seek(8+6);
  pfs.Read(pk,32);
  SHA256_Buffer(id,20,pk,32);
  writeln('ID: ',string(id));
  pfs.Seek(first_sec_ofs);
  left:=pfs.Length-first_sec_ofs;
  msg:='SEC X XXXXXXXXXXXXXX...';
  while left>=72 do begin
    pfs.Read(id,7);
    use:=char(pfs.ReadByte);
    pfs.Skip(64);
    dec(left,72);
    msg[5]:=char(use);
    BinToHex(@msg[7],id,7);
    writeln(msg);
  end;
  if left>0 then writeln('Warning: garbage at end of file.');
end;

procedure ShowHostKeyID(var pfs:tFileStream);
  var sk:tKey64;
  var pk:tKey32;
  var id:tKey20;
  begin
  pfs.Read(sk,64);
  ed25519.CreateKeyPair(pk,sk);
  SHA256_Buffer(id,20,pk,32);
  writeln('PUB: ',string(pk));
  writeln('ID:  ',string(id));
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
    ShowHostKeyID(s);
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
  flush(output);
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
  var wrapped: array [1..72] of byte;
  var kek: tKey32;
  var exkek: tAES_key;
  var pwd:string;
  begin
  write('Enter new password for master key: ');
  readln(pwd);
  if (length(pwd)<4)or(length(pwd)>56) then begin
    writeln('Error: password must be 4 to 56 bytes long');
    halt(3);
  end;
  SHA256_Buffer( kek, 32, pwd[1], length(pwd));
  AES_set_encrypt_key( kek, 256, exkek);
  AES_wrap_key(exkek, nil, wrapped, mastersec, 64);
  FillChar(masterSec,sizeof(masterSec),0);
  mf.Write(cMasterKeyIdent,8);
  mf.Write(wrapped,72);
end;

procedure LoadDecryptMasterKey(var mf: tCommonStream; out mastersec:tKey64; out master:tKey32);
  var wrapped: array [1..72] of byte;
  var kek: tKey32;
  var exkek: tAES_key;
  var pwd:string;
  var ident:array [1..8] of char;
  begin
  mf.Read(Ident,8);
  if CompareByte(ident,cCertKeyIdent,8)=0 then begin
    mf.Read(mastersec,64);
    ed25519.CreateKeyPair(master,mastersec);
  end else
  if CompareByte(ident,cMasterKeyIdent,8)=0 then begin
    mf.Read(wrapped,72);
    write('Enter password for master key: ');
    readln(pwd);
    if (length(pwd)<4)or(length(pwd)>56) then begin
      writeln('Error: password must be 4 to 56 bytes long');
      halt(3);
    end;
    SHA256_Buffer( kek, 32, pwd[1], length(pwd));
    AES_set_decrypt_key( kek, 256, exkek);
    if AES_unwrap_key(exkek, nil, mastersec, wrapped, 72)<>64 then begin
      writeln('Decryption failed. Invalid password or corrupt file.');
      halt(3);
    end;
    ed25519.CreateKeyPair(master,mastersec);
  end else raise eFormatError.Create('Invalid Master key file');
end;

procedure KeyringAdd(var lf:tCommonStream; var pub:tKey32; var sec:tKey64; use:char; front:boolean);
  var fkd: array [1..72] of byte;
  var fs: LongWord;
  begin
  fs:=lf.Length;
  assert(fs>=first_sec_ofs);
  if fs<(first_sec_ofs+72) then begin
    front:=false;
    fs:=first_sec_ofs;
  end;
  if not front then begin
    lf.Seek(fs);
    lf.Write(pub,8);
    lf.Write(sec,64);
  end else begin
    lf.Seek(first_sec_ofs);
    lf.Read(fkd,72);
    lf.Seek(first_sec_ofs);
    lf.Write(pub,7);
    lf.WriteByte(ord(use));
    lf.Write(sec,64);
    lf.Seek(fs);
    lf.Write(fkd,72);
  end;
end;

procedure EditKeyGen; {bnedit keygen secret.dat master.dat}
  var lf,mf:tFileStream;
  var genmaster:boolean;
  var mastersec:tKey64;
  var loginsec, encrsec: tkey64;
  var encrpub: tKey32;
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
  writeln('Master Key loaded,      pub=',string(ld.master));
  SHA256_Buffer(dbg,20,ld.master,32);
  writeln('ID=',string(dbg));
  mf.Done;
  write  ('Generate signature key  ');
  GetOSRandom(@loginsec,64);
  CreateKeyPair(ld.login,loginsec);
  writeln('pub=',string(ld.login));
  ld.expires:=Word6(UnixNow + 15778463); {6 months}
  writeln('Expiration set to 6 months.');
  SHA256_Buffer(dbg,20,ld,sizeof(ld));
  writeln('Signing. ',string(dbg));
  Sign(signature, ld, sizeof(ld), ld.master, masterSec);
  write  ('Generate encryption key ');
  GetOSRandom(@encrsec,64);
  CreateKeyPair(encrpub,encrsec);
  writeln('pub=',string(encrpub));
  lf.OpenRW(paramstr(2)); //todo check ident bytes
  lf.Write(cSecretIdent,8);
  lf.Write(ld.expires,6);
  lf.Write(ld.master,32);
  lf.Write(signature,64);
  lf.Write(loginsec,64);
  KeyringAdd(lf,encrpub,encrsec,'M',true);
  lf.Done;
  FillChar(masterSec,64,0);
  FillChar(encrsec,64,0);
  FillChar(loginsec,64,0);
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
  writeln('* For the Love of Gaben, store ',paramstr(2));
  writeln('* on safe removable media and don''t forget your password,');
  writeln('* becouse it can be used to take over your identity!');
end;

procedure KeyringToProfile(var ls:tCommonStream; var pf:tProfileRead; out LoginSec:tKey64);
  var Ident:array [1..8] of char;
  var fk: packed record
    p: array [1..7] of byte;
    u: char;
    s: tKey64;
    end;
  var count,i,j:Integer;
  begin
  ls.Read(ident,8);
  if CompareByte(ident,cSecretIdent,8)<>0
    then raise eFormatError.Create('Keyring file invalid');
  pf.expires:=ls.ReadWord6;
  ls.Read(pf.master_key,32);
  ls.Read(pf.master_sig,64);
  ls.Read(LoginSec,64);
  CreateKeyPair(pf.sign_key,{<-}LoginSec);
  ls.seek(first_sec_ofs); {begin reading secret keys}
  count:=((ls.Length-first_sec_ofs) div 72);
  ls.Read(fk,72); {first key has special place}
  CreateKeyPair(pf.encr_key,{<-}fk.s);
  pf.cOldKeys:=0;
  j:=0;
  pf.OldKeys:=GetMem(pf.cOldKeys+1); {+1 is for regenerate}
  for i:=0 to count-2 do begin
    ls.Read(fk,72);
    if fk.u='M' then begin
      CreateKeyPair(pf.OldKeys[j],fk.s);
      inc(pf.cOldKeys);
      inc(j);
    end;
  end;
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
  var LoginSec:tKey64;
  var cmd,param:string;
  var tmpp:LongWord;
  begin
  if paramcount<>3 then begin writeln(helphint); halt(9) end;
  {additional params:
    ... newenc
    ... addhost hostkey.dat
    ... addhost HOSTKEY
  }
  try
    pfs.OpenRW(paramstr(3));
    if pfs.Length=0 then begin
      pf.InitEmpty;
      writeln('Initialized empty profile');
    end else begin
      pf.ReadFrom(pfs,0);
    end;
  except
    on e:exception do begin
      writeln(paramstr(3)+': '+e.Message);
      writeln('If you specify non-existent file as parameter 3, empty profile will be created');
      halt(2);
    end;
  end;
  try
    ls.OpenRO(paramstr(2));
    KeyringToProfile(ls, pf, LoginSec);
    ls.Done;
  except
    on e:eInvalidOP{exception} do begin
      writeln(paramstr(2)+': '+e.Message);
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
    end else if cmd='NEWKEY' then begin
      writeln('Error: not implemented.');
    end else if (cmd='HELP')or(cmd='?') then begin
      writeln('--commands available for profile editing--');
      writeln('  syntax: COMMAND parameter...');
      writeln('  semicolon in COMMAND is ignored, command is not case-sensitive');
      writeln('NAME',' Full Name ;set your name');
      writeln('NICK',' nick ;set your nick name, max 8 ASCII chars #32..#126');
      writeln('NOTE',' text ;set additional text, max 255 bytes');
      writeln('NEWKEY',' ;generate new encryption key');
      writeln('EXIT',' ;save changes and exit (empty line equals EXIT)');
    end else if (cmd='')or(cmd='QUIT')or(cmd='SAVE')or(cmd='EXIT')
    then break
    else writeln('Error: Unknown command: ',cmd,' (use HELP command)');
  until EOF(input);
  pfs.Seek(0);
  pfs.Trunc(0);
  pf.Updated:=UnixNow;
  pf.WriteTo(pfs,LoginSec);
  pfs.Done;
  FillChar(loginsec,sizeof(loginsec),0);
  writeln('Changes Saved.');
end;

procedure MessageEncrypt;
  var keyring, outmsg, inmsg, rfprof:tFileStream;
  var myID : tKey20;
  var myKey: tKey64;
  var myKeyPub: tkey32;
  var use: char;
  var session, shared, kek: tkey32;
  var exkek: tAES;
  var wrapped: array [1..40] of byte;
  var rprof: tProfileRead;
  var hmacx: tSHA256HMAC;
  var seskx: tAES_FB;
  var hmac: tKey32;
  var i,count:integer;
  var gzc: tGZip;
  var minleft: LongWord;
  var minbuf: array [1..2048] of byte;
  var mencr, mdefl: array [1..16] of byte;
  begin
  writeln('WIP message encrypt');
  writeln('keyring '+paramstr(2));
  keyring.OpenRO(paramstr(2));
  writeln('outmsg '+paramstr(3));
  outmsg.OpenRW(paramstr(3));
  writeln('inmsg '+paramstr(4));
  inmsg.OpenRO(paramstr(4));
  count:=ParamCount-4;
  writeln('rcpt_count ',count);
  {read ID and latest enck from keyring}
  keyring.Seek(8+6);
  keyring.Read(myKeyPub,32);
  SHA256_Buffer(myid,20,myKeyPub,32);
  writeln('myID: ':10,string(myid));
  keyring.Seek(first_sec_ofs+7);
  use:=char(keyring.ReadByte);
  keyring.Read(myKey,64);
  ed25519.CreateKeyPair(myKeyPub, myKey);
  writeln('myKeyPub ':10,string(myKeyPub),' use=',use);
  GetOSRandom(@session,32);
  writeln('sesk ':10,string(session));
  outmsg.Trunc(0);
  outmsg.Write(cMessageIdent,8);
  outmsg.Write(MyID,20);
  outmsg.Write(MyKeyPub,32);
  outmsg.WriteWord6(UnixNow);
  outmsg.WriteByte(count);
  outmsg.WriteByte(0);
  {} {decide: default encrypt to self?}
  if count<=0 then raise exception.create('required at least one recipient');
  for i:=1 to count do begin
    writeln('rcpt prof ',paramstr(4+i));
    rfprof.OpenRO(paramstr(4+i));
    rprof.ReadFrom(rfprof,0);
    writeln('rcpt ID ':10,string(rprof.profID));
    writeln('rcpt encr ':10,string(rprof.encr_key));
    SharedSecret(shared, rprof.encr_key, myKey);
    writeln('shared ':10,string(shared));
    SHA256_Buffer( kek, 32, shared, 32);
    writeln('kek ':10,string(kek));
    exkek.InitEnCrypt(kek, 256);
    exkek.Wrap(wrapped, session, 32, nil);
    outmsg.Write(rprof.profid,20);
    outmsg.Write(rprof.encr_key,7);
    outmsg.WriteByte(69);
    outmsg.Write(wrapped,40);
  end;
  hmacx.Init(session,32);
  seskx.InitEnCrypt(session, 256);
  seskx.SetIV(MyID); {!?}
  gzc.InitDeflate;
  gzc.avail_out:=sizeof(mdefl);
  gzc.next_out:=@mdefl;
  minleft:=inmsg.Length;
  count:=0;
  writeln('hmacx, seskx and deflate initialized');
  repeat
    if (gzc.avail_in=0) and (minleft>0) then begin
      gzc.avail_in:=sizeof(minbuf);
      gzc.next_in:=@minbuf;
      if minleft<sizeof(minbuf) then gzc.avail_in:=minleft;
      inmsg.Read(minbuf,gzc.avail_in);
      hmacx.Update(minbuf,gzc.avail_in);
      minleft:=minleft-gzc.avail_in;
    end;
    gzc.Deflate;
    if (gzc.avail_out<16) and ((gzc.avail_out=0) or gzc.eof) then begin
      if gzc.avail_out>0 then writeln('output buffer ',gzc.avail_out);
      seskx.EnCryptPCBC(mencr, mdefl);
      outmsg.Write(mencr, sizeof(mencr));
      gzc.avail_out:=sizeof(mdefl);
      gzc.next_out:=@mdefl;
      inc(count);
    end;
  until gzc.eof;
  writeln('end of compressed stream, count=',count);
  hmacx.Final(hmac);
  writeln('hmac ':10,string(hmac));
  outmsg.Write(hmac,sizeof(hmac));
  outmsg.Done;
end;
  
  

const eoln=LineEnding;
const helptext:ansistring
=eoln
+'bnedit file.dat : decode and output contents of the file'+eoln
+'bnedit prof secret.dat profile.dat : edit profile file'+eoln
+' reads commands from stdin, use HELP to get available commands.'+eoln
+'bnedit keygen secret.dat master_secret.dat : generate secret key from master or both'+eoln
+' A random master_secret.dat will be created if it does not exist.'+eoln
+'bnedit chpasswd master_secret.dat : change password on your master key'+eoln
+'bnedit encrypt secret.dat message.dat message.txt rcpt_spec... : encrypt message'+eoln
+' rcpt_spec = prio:recipients_profile.dat, prio = 0..9 or to (8), cc(4)'+eoln
+'bnedit de[crypt] secret.dat message.dat [sender_profile.dat] : decrypt and verify a message'+eoln
{+'bnedit host host-key.dat : generate host key or renew PoW'+eoln}
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
    'encrypt': MessageEncrypt;
    {prof secret profile command value...}
    {de[crypt] message secret [sender_profile]}
    {encrypt message input secret rcpt_profile...}
    else begin
      writeln('Unknown operation! Here read help text.');
      write(helptext);
      halt(9);
    end;
  end;
END.

bnedit prof profile.dat secret.dat name "Tomáš Brada" nick Brod 
