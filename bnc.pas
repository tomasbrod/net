program bnc;
USES ObjectModel,SockStream,Classes,SysUtils,Sockets,BaseUnix,opcode;

{BrodNet CLI programs}
{bncmd communicate with daemon}
{bnedit wiev/edit all types of brodnet files}
{$INCLUDE gitver.inc}

var sck:tHandleStream;
procedure InitUnix;
  var addr:Sockets.sockaddr_un;
  var h:tSocket;
  begin
  addr.sun_family:=AF_UNIX;
  addr.sun_path:='ctrl';
  h:=fpSocket(addr.sun_family,SOCK_STREAM,0);
  SC(@fpSocket,h);
  SC(@fpConnect,fpConnect(h,@addr,sizeof(addr)));
  sck:=tHandleStream.Create(h);
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
  e:=s.R1;
  if e=0 then begin
    s.Read(fid,20);
    s.Read(Update,4);
    WriteStr(result,'FID: ',string(fid),LineEnding, 'Update: ',LongWord(Update));
  end else WriteStr(result,'not found, error ',e);
end;

{bnc xxX[commands]Xxx}

var req:tMemoryStream;
var res:tMemoryStream;
var resc:word;
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
    st:=rpl.R1;
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
  req.position:=0;
  req.W2(req.size-2);
  sck.WB(req.Memory^,req.size);
  req.clear;
end;
procedure aRecv;
  var l:Word;
  begin
  l:=sck.R2;
  res.Size:=l+2;
  sck.RB((res.Memory+2)^, l);
  res.position:=2;
  resc:=res.R2;
end;

procedure cmdINFO;
  begin
  if paramcount>1 then errParams;
  {no input}
  req.W2(00);
  aSend;aRecv;
  Writeln(res.ReadStringAll);
  //Writeln('bnc  is  ',GIT_VERSION);
end;

procedure cmdSTOP;
  begin
  if paramcount>1 then errParams;
  {no input}
  req.W2(01);
  aSend;aRecv;
end;

procedure cmdPUT;
  var ins:tFileStream;
  var fid:tKey20;
  var left,bc:longword;
  begin
  if paramcount<>2 then errParams;
  ins.Create(ParamStr(2),fmOpenRead);
  req.W2(23);
  left:=ins.left;
  req.W4( Left );
  writeln('sending object ',left);
  aSend;
  aRecv;
  if resc<>0 then begin
    writeln('Failed init transfer e=',resc);
    exit;
  end;
  while left>0 do begin
    req.position:=0;
    req.size:=4096;
    if left>req.size then bc:=req.size else bc:=left;
    ins.RB(req.Memory^,bc);
    sck.WB(req.Memory^,bc);
    left:=left-bc;
  end;
  aRecv;
  if (res.Left>=24) and (resc=0) then begin
    res.rb(fid,24);
    writeln(string(fid));
  end else writeln('Failed to send object e=');
end;

procedure cmdPUT_local(m:integer);
  var path:string;
  var fid:tKey20;
  var e:byte;
  begin
  if paramcount<>2 then errParams;
  path:=paramstr(2);
  if not (path[1] in ['/','\']) then path:=GetCurrentDir+'/'+path;
  if m=1 then begin
    {chmod}
  end;
  if      m=0 then req.W2(00)
  else if m=1 then req.W2(03)
  else if m=2 then req.W2(09);
  req.Write(path[1],Length(path));
  aSend;aRecv;
  if (res.Left>=1) then begin
    e:=res.R1;
    if (e=0) and (res.Left>=20) then begin
      res.rb(fid,24);
      writeln(string(fid));
    end else begin
      writeln('error ',e,': ',res.ReadStringAll);
    end;
  end else writeln('failed');
end;

procedure cmdGET;
  var left,flen:LongWord;
  begin
  if paramcount<>2 then errParams;
  req.W2(22);
  req.Write(tKey24(paramstr(2)),24);
  req.W4(0);
  req.W4($FFFFFFFF);
  aSend;aRecv;
  if (res.Left>=8) and (resc=0) then begin
    flen:=res.R4;
    left:=res.R4;
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
    req.W2(24);
    req.Write(id,24);
    aSend;aRecv;
    if resc<>0 then begin
      if resc=2 then writeln('Link Object Not Found')
      else writeln('failed e=',resc);
      exit;
    end;
    res.Read(id,24);
    writeln('Link to: ',string(id));
    req.clear;
    req.w2(0);
  end;
  req.W2(21);
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
  req.W2(12);
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
  req.W2(3605);
  req.Write(tNetAddr(paramstr(2)),18);
  aSend; aRecv; st:=resc;
  if st=0 then begin
    writeln('ok');
  end else writeln('error ',st);
end;

procedure cmdDhtDump;
  var
    Depth,Ban:byte;
    ModifyTime,LastMsg,LastReply,Verified,i,bktsize:LongWord;
    ReqDelta:Word;
    id:tKey20;
    addr:tnetaddr;
  begin
  req.W2(4451);
  aSend; aRecv;
  if resc=0 then begin
    res.Read(ID,20);
    writeln('DHT Dump of node ',string(ID));
    if res.left=0 then writeln('No buckets, DHT is empty.');
    while res.left>0 do begin
      depth:=res.R1;
      bktsize:=res.R1;
      ModifyTime:=res.R4;
      writeln('Bucket depth ',depth,' mod ',ModifyTime);
      for i:=1 to bktsize do begin
        res.Read(ID,20);
        res.Read(Addr,sizeof(tNetAddr));
        ReqDelta:=res.R2();
        LastMsg:=res.R4();
        LastReply:=res.R4();
        Verified:=res.R4();
        ban:=res.R1;
        write('  ',string(ID),' ',string(Addr));
        if (ban and 1)=1 then write(' CRPending');
        if ReqDelta>0 then write(' Retry',ReqDelta);
        write(' TM',LastMsg div 1000,'s');
        write(' TR',LastReply div 1000,'s');
        writeln(' TV',Verified div 1000,'s');
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
  req.W2(16);
  id:=paramstr(2);
  req.Write(id,20);
  aSend; aRecv; st:=res.R1;
  if st=0 then begin
    res.read(id,20);
    ver:=res.r6;
    writeln(string(ID), ' ',ver);
  end else writeln('error ',st);
end;


procedure cmdGETPROF;
  var st:byte;
  var id:tKey20;
  var ver:int64;
  begin
  if paramcount<>2 then errParams;
  req.W2(15);
  id:=paramstr(2);
  req.Write(id,20);
  aSend; aRecv; st:=res.R1;
  if st=0 then begin
    res.read(id,20);
    ver:=res.r6;
    writeln(string(ID), ' ',ver);
  end else writeln('error ',st);
end;

BEGIN
  if paramcount<1 then ErrParams;
  req:=tMemoryStream.Create;
  res:=tMemoryStream.Create;
  req.w2(0);
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

