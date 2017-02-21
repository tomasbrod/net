program bnc;
USES ObjectModel,SockStream,SysUtils,Sockets,BaseUnix,opcode;

{BrodNet CLI programs}
{bncmd communicate with daemon}
{bnedit wiev/edit all types of brodnet files}
{$INCLUDE gitver.inc}

var sck:tSocketStream;
procedure InitUnix;
  var addr:Sockets.sockaddr_un;
  begin
  addr.sun_family:=AF_UNIX;
  addr.sun_path:='ctrl';
  sck.h:=fpSocket(addr.sun_family,SOCK_STREAM,0);
  SC(@fpSocket,sck.h);
  SC(@fpConnect,fpConnect(sck.h,@addr,sizeof(addr)));
  sck.Init(sck.h);
end;

procedure CopySP(var s1:tCommonStream; var s2:tMemoryStream);
  var l:word;
  begin
  l:=s1.ReadWord2;
  if l>s2.size then raise eInvalidMemStreamAccess.Create('Message Too Long');
  s1.Read(s2.base^, l);
  s2.vLength:=l;
  s2.position:=0;
end;

function IsBlob(const id: tKey24): boolean;
  begin
  IsBlob:=((id[23] and 1) = 1);
end;

function StringProfileInfo(var s:tMemoryStream):string;
  var fid:tKey20;
  var Update:Word4;
  var e:byte;
  begin
  e:=s.ReadByte;
  if e=0 then begin
    s.Read(fid,20);
    s.Read(Update,4);
    WriteStr(result,'FID: ',string(fid),LineEnding, 'Update: ',LongWord(Update));
  end else WriteStr(result,'not found, error ',e);
end;

{bnc xxX[commands]Xxx}

var req:tMemoryStream;
var res:tMemoryStream;
var resc:byte;
(*
procedure ShowPUPD;
  var st:byte;
  var ve:LongWord;
  var id:tKey20;
  var ad:tNetAddr;
  var tb:tDateTime;
  var t:string;
  begin
  tb:=Now;
  repeat
    CopySP(sck,rpl); rpl.seek(0);
    st:=rpl.ReadByte;
    ve:=rpl.ReadWord(4);
    rpl.Read(id,20);
    rpl.read(ad,24);
    case st of
      1:t:='LkpEnd';2:t:='Found';3:t:='InvMsg';4:t:='Fetch';
      5:t:='Local';6:t:='AltSrc';7:t:='FFail';8:t:='Valid';
      9:t:='InvOld';10:t:='Inval';11:t:='Notify';12:t:='End';
      13:t:='Peer'
      else t:='?'+IntToStr(st); end;
    writeln(round((Now-tb)*MsecsPerDay):4,' ',t:6,ve:5,string(id):41,string(ad));
  until st=12;
end;*)

procedure ErrParams;
  begin
  writeln('Invalid parameters');
  halt(1);
end;

procedure aSend;
  begin
  req.Seek(0);
  req.writeword2(req.length-4);
  sck.Write(req.base^,req.vlength);
end;
procedure aRecv;
  begin
  res.vlength:=sck.ReadWord2;
  if res.vlength>res.size then raise eInvalidMemStreamAccess.Create('Message Too Long');
  sck.Read(res.base^, res.vlength);
  res.Seek(0);
  resc:=res.ReadByte;
end;

procedure cmdINFO;
  begin
  if paramcount>1 then errParams;
  {no input}
  req.WriteWord2(00);
  aSend;aRecv;
  Writeln(res.ReadStringAll);
  Writeln('bnc  is  ',GIT_VERSION);
end;

procedure cmdSTOP;
  begin
  if paramcount>1 then errParams;
  {no input}
  req.WriteWord2(01);
  aSend;aRecv;
end;

procedure cmdPUT;
  var ins:tFileStream;
  var left,bc:longword;
  begin
  if paramcount<>2 then errParams;
  ins.OpenRO(ParamStr(2));
  req.WriteWord2(23);
  left:=ins.left;
  req.WriteWord4( Left );
  writeln('sending object ',left);
  aSend;
  aRecv;
  if resc<>0 then begin
    writeln('Failed init transfer e=',resc);
    exit;
  end;
  while left>0 do begin
    req.Seek(0);
    if left>req.size then bc:=req.size else bc:=left;
    ins.Read(req.base^,bc);
    sck.Write(req.base^,bc);
    left:=left-bc;
  end;
  aRecv;
  if (res.Left>=24) and (resc=0) then begin
    writeln(string(tKey24(res.ReadPtr(24)^)));
  end else writeln('Failed to send object e=');
end;

procedure cmdPUT_local(m:integer);
  var path:string;
  var e:byte;
  begin
  if paramcount<>2 then errParams;
  path:=paramstr(2);
  if not (path[1] in ['/','\']) then path:=GetCurrentDir+'/'+path;
  if m=1 then begin
    {chmod}
  end;
  if      m=0 then req.WriteWord2(00)
  else if m=1 then req.WriteWord2(03)
  else if m=2 then req.WriteWord2(09);
  req.Write(path[1],Length(path));
  aSend;aRecv;
  if (res.Left>=1) then begin
    e:=res.ReadByte;
    if (e=0) and (res.Left>=20) then begin
      writeln(string(tKey20(res.ReadPtr(20)^)));
    end else begin
      writeln('error ',e,': ',res.ReadStringAll);
    end;
  end else writeln('failed');
end;

procedure cmdGET;
  var left,flen:LongWord;
  begin
  if paramcount<>2 then errParams;
  req.WriteWord2(22);
  req.Write(tKey24(paramstr(2)),24);
  req.WriteWord4(0);
  req.WriteWord4($FFFFFFFF);
  aSend;aRecv;
  if (res.Left>=8) and (resc=0) then begin
    flen:=res.ReadWord4;
    left:=res.ReadWord4;
    writeln('FileSize: ',flen);
    writeln('ReqLen: ',left);
        {sck.Read(res.base^, 4);}
  end else if resc=2 then writeln('Object Not Found')
  else if resc=3 then writeln('Is Link')
  else if resc=4 then writeln('Out of Bounds')
  else writeln('failed e=',resc);
end;

procedure cmdSTAT;
  var id:tKey24;
  var path:ansistring;
  begin
  if paramcount<>2 then errParams;
  id:=tKey24(paramstr(2));
  while not IsBlob(id) do begin
    req.WriteWord2(24);
    req.Write(id,24);
    aSend;aRecv;
    if resc<>0 then begin
      if resc=2 then writeln('Link Object Not Found')
      else writeln('failed e=',resc);
      exit;
    end;
    res.Read(id,24);
    writeln('Link to: ',string(id));
    req.Seek(0);req.Trunc;
  end;
  req.WriteWord2(21);
  req.Write(id,24);
  aSend;aRecv;
  if resc=0 then begin
    path:=res.ReadStringAll;
    Writeln('Path: ',path);
    {that's it?}
  end
  else if resc=2 then writeln('Blob Object Not Found')
  else if resc=4 then writeln('Blob stored Inline')
  else writeln('failed e=',resc);
end;

procedure cmdFETCH;
  begin
  if paramcount<>3 then errParams;
  req.WriteWord2(12);
  req.Write(tKey20(paramstr(2)),20);
  req.Write(tNetAddr(paramstr(3)),18);
  req.WriteByte(64);
  aSend; aRecv;
  if resc=0 then begin
    writeln('ok');
  end else writeln('error ',resc);
  readln;
end;

procedure cmdPEER;
  var st:byte;
  begin
  if paramcount<>2 then errParams;
  req.WriteWord2(02);
  req.Write(tNetAddr(paramstr(2)),18);
  aSend; aRecv; st:=resc;
  if st=0 then begin
    writeln('ok');
  end else writeln('error ',st);
end;

procedure cmdDhtDump;
  var
    Depth,Ban:byte;
    ModifyTime,LastMsgFrom,i,bktsize:LongWord;
    ReqDelta:Word;
    id:tKey20;
    addr:tnetaddr;
  begin
  req.WriteWord2(14);
  aSend; aRecv;
  if resc=0 then begin
    res.Read(ID,20);
    writeln('DHT Dump of node ',string(ID));
    if res.left=0 then writeln('No buckets, DHT is empty.');
    while res.left>0 do begin
      depth:=res.ReadByte;
      bktsize:=res.ReadByte;
      ModifyTime:=res.ReadWord4;
      writeln('Bucket depth ',depth,' mod ',ModifyTime);
      for i:=1 to bktsize do begin
        res.Read(ID,20);
        res.Read(Addr,sizeof(tNetAddr));
        ReqDelta:=res.ReadWord2();
        LastMsgFrom:=res.ReadWord4();
        ban:=res.ReadByte;
        write('  ',string(ID),' ',string(Addr));
        if (ban and 1)=1 then writeln(' Banned')
        else begin
          if (ban and 2)=0 then write(' Unverified');
          if ReqDelta>0 then write(' Retry',ReqDelta);
          writeln(' T',LastMsgFrom div 1000,'s');
        end;
      end;
    end;
  end else writeln('failed e=',resc);
end;


procedure cmdSETPROF;
  var st:byte;
  var id:tKey20;
  var ver:int64;
  begin
  if paramcount<>2 then errParams;
  req.WriteWord2(16);
  id:=paramstr(2);
  req.Write(id,20);
  aSend; aRecv; st:=res.ReadByte;
  if st=0 then begin
    res.read(id,20);
    ver:=res.readword6;
    writeln(string(ID), ' ',ver);
  end else writeln('error ',st);
end;


procedure cmdGETPROF;
  var st:byte;
  var id:tKey20;
  var ver:int64;
  begin
  if paramcount<>2 then errParams;
  req.WriteWord2(15);
  id:=paramstr(2);
  req.Write(id,20);
  aSend; aRecv; st:=res.ReadByte;
  if st=0 then begin
    res.read(id,20);
    ver:=res.readword6;
    writeln(string(ID), ' ',ver);
  end else writeln('error ',st);
end;

BEGIN
  if paramcount<1 then ErrParams;
  req.Init(4096);
  res.Init(req.base,0,req.size);
  req.skip(2);
  InitUnix;
  case upcase(paramstr(1)) of
    'INFO'  : cmdINFO;
    'STOP'  : cmdSTOP;
    'ADDPEER'  : cmdPEER;
    'DHTDUMP'   : cmdDhtDump;
    {'PUTCP' : cmdPUT_local(0);}
    'PUTLN' : cmdPUT_local(1);
    'PUTMV' : cmdPUT_local(2);
    {'GETCP' : cmdGET_local(0);}
    {'GETLN' : cmdGET_local(1);}
    'PUT'   : cmdPUT;
    'GET'   : cmdGET;
    'STAT'  : cmdSTAT;
    'FETCH'  : cmdFETCH;
    'SETPROF'  : cmdSETPROF;
    'GETPROF'  : cmdGETPROF;
    else ErrParams;
  end;
END.

{
  req.Init(4096);
  res.Init(req.base,0,req.size);
  req.skip(2);
  case upcase(paramstr(1)) of
    'INFO': cmdINFO;
    'QUIT': op:=1;
    'EXEC': op:=2;
    'PEER': begin op:=3; pl.Write(tNetAddr(paramstr(2)),sizeof(tNetAddr)) end;
    'COPY': op:=8;
    'GETF': begin op:=10; pl.Write(tKey20(paramstr(2)),20); pl.WriteWord(0,4); pl.WriteWord($FFFFFFFF,4); end;
    'PGET': begin op:=12; pl.Write(tKey20(paramstr(2)),20) end;
    'PSET': begin op:=13; pl.Write(tKey20(paramstr(2)),20) end;
    'PUPD': begin op:=15; pl.Write(tKey20(paramstr(2)),20) end;
    else begin writeln('Invalid command'); halt(3) end;
  end;
  InitUnix;
  if op=8 then begin
    path:=paramstr(2);
    if not (path[1] in ['/','\','~']) then path:=GetCurrentDir+'/'+path;
    pl.Write(path[1],Length(path));
  end;
  cmd.len:=pl.length;cmd.op:=op;
  sck.Write(cmd,sizeof(cmd));
  sck.Write(pl.base^,pl.length);
  CopySP(sck,rpl); rpl.seek(0);
  case op of
    00: writeln(pchar(rpl.base));
    03: CheckStatus(rpl,op,0);
    08: if CheckStatus(rpl,op,20) then writeln(string(tKey20(rpl.ReadPtr(20)^)));
    12: if CheckStatus(rpl,op,24) then writeln(
        'FID: ',string(tKey20(rpl.ReadPtr(20)^)),LineEnding,
        'Update: ',LongWord(rpl.ReadWord(4)));
    13: if CheckStatus(rpl,op,24) then writeln(
        'PID: ',string(tKey20(rpl.ReadPtr(20)^)),LineEnding,
        'Update: ',LongWord(rpl.ReadWord(4)));
    10: if CheckStatus(rpl,op,4) then writeln(
        'Length: ',LongWord(rpl.ReadWord(4)));
    15: if CheckStatus(rpl,op,0) then ShowPUPD;
  end;
END.}

