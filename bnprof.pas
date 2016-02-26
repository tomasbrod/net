program bnprof;
USES Profile,MemStream,NetAddr,SysUtils,ed25519,Sha512;

var pf:file of byte;
var lsf:file of tKey64;
var LoginSec:tKey64;
var LoginPub:tKey32;
var LoginPubHash{,FileHash}:tKey20;
var pubkey:tKey32;
var hdr:tProfileHeader;
var cmd,param:string;
var tmpp:LongWord;

type tSPMode=(spRewrite,spAdd,spDel,spAddUniq,spDelAll);
procedure SetProp(var pf:file; mode:tSPMode; name:byte; const value; len:word);
  var tmp:packed record len:Word2; tag:byte; end;
  var es:Word;
  var buf:pointer;
  var same:boolean;
  var pos,tfs:LongWord ;
  var fpP:LongWord;
  var fpL:word;
  procedure AddFree;
    begin if (es<=fpL)and(es=len) then begin
      fpP:=pos; fpL:=es;
  end end;
  begin
  tfs:=FileSize(pf);
  pos:=sizeof(tProfileHeader); Seek(pf,pos);
  fpL:=$FFFF;
  while (pos+3)<=tfs do begin
    blockread(pf,tmp,3);
    es:=word(tmp.len)-1;
    //writeln('Tag ',tmp.tag,' at pos ',pos,' len ',es,'/',len);
    if (tmp.tag=0) then AddFree;
    if (tmp.tag=name) then begin
      if (mode in [spAddUniq,spDel])and(len=es) then begin
        if es=0 then same:=true else begin
          buf:=GetMem(len);
          BlockRead(pf,buf^,es);
          same:=CompareByte(value,buf^,es)=0;
          //writeln('same ',same);
          FreeMem(buf,len);
        end;
      end else same:=false;
      if (mode=spRewrite)or( (mode in [spAddUniq,spDel]) and same) then begin
        AddFree;
        tmp.tag:=0;
        Seek(pf,pos);
        blockwrite(pf,tmp,3);
      end;
    end;
    pos:=pos+3+es; Seek(pf,pos);
  end;
  if mode in [spRewrite,spAddUniq] then begin
    if fpL<$FFFF then Seek(pf,fpP);
    tmp.tag:=name;
    tmp.len:=len+1;
    BlockWrite(pf,tmp,3);
    if len>0 then BlockWrite(pf,value,len);
  end;
end;

function GetPubFromSecFile(const fn:string):tKey32;
  var sf:file of tKey64;
  var sec:tKey64;
  begin
  try
  assign(sf,fn);
  reset(sf);
  read(sf,sec);
  finally
  close(sf);
  end;
  CreateKeyPair(result, sec);
end;
function GetPubHash(const pub:tKey32):tKey20;
  var hash:tSha512Context;
  begin
  Sha512Init(hash);
  Sha512Update(hash,pub,sizeof(pub));
  Sha512Final(hash,result,sizeof(result));
end;

procedure DumpProfile(var pf:file; const hdr:tProfileHeader);
  var tmp:packed record len:Word2; tag:byte; end;
  var pos,tfs:LongWord;
  var es:Word;
  var vals:string;
  var hash:tSha512Context;
  begin
  Sha512Init(hash);
  Sha512Update(hash,hdr,sizeof(hdr));
  tfs:=FileSize(pf);
  pos:=sizeof(tProfileHeader); Seek(pf,pos);
  while (pos+3)<=tfs do begin
    blockread(pf,tmp,3);
    es:=word(tmp.len)-1;
    if tmp.tag in [pfName,pfMotd] then begin
      SetLength(vals,es);
      BlockRead(pf,vals[1],es);
    end;
    if tmp.tag=pfHost then begin
      BlockRead(pf,pubkey,es);
      vals:=pubkey;
    end;
    case tmp.tag of
      pfName:writeln('Name: ',vals);
      pfHost:writeln('Login: ',vals);
      pfLink:writeln('Link: ');
      pfMotd:writeln('MOTD: ',vals);
      0:writeln('Unused ',es,' bytes');
      127:;
      else writeln('Unknown (tag=',tmp.tag,') ',es,' bytes');
    end;
    pos:=pos+3+es; Seek(pf,pos);
  end;
end;

procedure SignProfile(var pf:file);
  var buf:pointer;
  var sig:tProfileSig;
  var len:LongWord;
  begin
  len:=FileSize(pf);
  buf:=GetMem(len);
  Seek(pf,0);
  BlockRead(pf,buf^,len);
  ed25519.Sign(sig.sig, buf^, len, LoginPub, LoginSec);
  sig.Len1:=0;
  sig.Len2:=65;
  sig.Tag:=127;
  BlockWrite(pf,sig,sizeof(sig));
end;

function VerifyProfile(var pf:file):boolean;
  var buf:pointer;
  var sig:tProfileSig;
  var len:LongWord;
  var hdr:^tProfileHeader;
  begin
  len:=FileSize(pf);
  len:=len-sizeof(sig);
  buf:=GetMem(len);
  hdr:=buf;
  Seek(pf,0);
  BlockRead(pf,buf^,len);
  BlockRead(pf,sig,sizeof(sig));
  result:=ed25519.Verify(sig.sig, buf^, len, hdr^.LoginPub);
end;

{bnprof prof.dat login-sec.dat <connamds.txt}
BEGIN
	Assign(pf,paramstr(1));
  try
  ReSet(pf,1);
  except on e:eInOutError do begin
    if e.ErrorCode=2 then ReWrite(pf) else raise;
  end end;
  if FileSize(pf)=0 then begin
    hdr.Magic:=pfHeader;
    hdr.Nick:='undefined'
  end else begin
    BlockRead(pf,hdr,sizeof(hdr));
    if CompareByte(hdr.Magic,pfHeader,4)<>0 then raise eXception.Create('Wrong magic number in profile header');
  end;
  if paramstr(2)<>'' then begin
    assign(lsf,paramstr(2));
    ReSet(lsf);
    Read(lsf,LoginSec);
    Close(lsf);
    CreateKeyPair(LoginPub, LoginSec);
    hdr.LoginPub:=LoginPub;
    if FileSize(pf)>=(sizeof(tProfileSig)+sizeof(tProfileHeader)) then begin
      Seek(pf,FileSize(pf)-sizeof(tProfileSig));
      Truncate(pf); {remove file signature}
    end;
    repeat
      readln(input,cmd);
      if cmd='' then break;
      tmpp:=pos(' ',cmd);
      if tmpp>0 then begin
        param:=copy(cmd,tmpp+1,255);
        if cmd[tmpp-1]=':' then dec(tmpp);
        cmd:=copy(cmd,1,tmpp-1);
      end else param:='';
      cmd:=upcase(cmd);
      if cmd='NAME' then begin
        SetProp(pf,spRewrite,pfName,param[1],length(param));
      end else if cmd='NICK' then begin
        hdr.nick:=param;
      end else if cmd='MOTD' then begin
        //if param=''
        SetProp(pf,spRewrite,pfMotd,param[1],length(param));
      end else if cmd='LOGIN' then begin
        pubkey:=param;
        SetProp(pf,spAddUniq,pfHost,pubkey[0],sizeof(pubkey));
      end else if cmd='LOGINSEC' then begin
        try
        pubkey:=GetPubFromSecFile(param);
        except on e:eInOutError do begin
          writeln(stderr,'Failed to read key from file ',param, '(',e.message);
          continue;
        end end;
        SetProp(pf,spAddUniq,pfHost,pubkey[0],sizeof(pubkey));
      end else if cmd='LOGOUT' then begin
        pubkey:=param;
        SetProp(pf,spDel,pfHost,pubkey[0],sizeof(pubkey));
      end else if cmd='UPDATED' then begin
        val(param,tmpp);
        if tmpp>=DWORD(hdr.Update) then hdr.Update:=tmpp
        else writeln('Error: Can not decrease UPDATED field');
      end;
    until EOF(input);
    Seek(pf,0);
    tmpp:=trunc(Now-40179);
    hdr.Update:=DWORD(hdr.Update)+1;
    BlockWrite(pf,hdr,sizeof(hdr));
    SignProfile(pf);
  end else begin
    LoginPubHash:=GetPubHash(hdr.LoginPub);
    Writeln('ID: ',string(LoginPubHash));
    //Writeln('PubKey: ',string(hdr.LoginPub));
    Writeln('Updated: ',DWord(hdr.Update));
    writeln('Nick: ',hdr.nick);
    DumpProfile(pf,hdr);
    writeln('Verify: ',VerifyProfile(pf));
  end;
    
END.

