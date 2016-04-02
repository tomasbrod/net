program bnc;
USES MemStream,SockStream,NetAddr,SysUtils,Sockets,BaseUnix;

var sck:tSocketStream;
procedure InitUnix;
  var addr:Sockets.sockaddr_un;
  begin
  addr.sun_family:=AF_UNIX;
  addr.sun_path:='ctrl.sock';
  sck.h:=fpSocket(addr.sun_family,SOCK_STREAM,0);
  SC(@fpSocket,sck.h);
  SC(@fpConnect,fpConnect(sck.h,@addr,sizeof(addr)));
  sck.Init(sck.h);
end;


procedure CopySP(var s1:tCommonStream; var s2:tMemoryStream);
  var l:word;
  begin
  s1.Read(l,2);l:=ntohs(l);
  s2.Trunc;
  if l>s2.WrBufLen then raise eInvalidMemStreamAccess.Create('Write out of bounds');
  s1.Read(s2.WrBuf^, l);
  s2.WrEnd(l);
end;
function CheckStatus(var s:tMemoryStream; op:Word; min:Word):boolean;
  var e:byte;
  begin
  e:=s.ReadByte;
  result:=true;
  //writeln(e,' ',s.left);
  if e=0 then begin
    if s.Left<min then begin
      writeln(stderr,'Truncated server response. Got ',s.Left,' expected ',min,' bytes.');
      halt(1);
    end;
  end else begin
    write('Server Error: ');
    if true then case e of
      129: writeln('Server Admitted Failure');
      130: writeln('Not Found');
      001: case op of
        13: writeln('Profile Invalid');
        else writeln(e);
        end;
      else writeln(e);
    end;
    halt(e);
  end;
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
var cmd: packed record
  op,len:Word2 end;
var op:word;
var pl:tMemoryStream;
var rpl:tMemoryStream;
var path:string;
type tMutEvt=(
      meSearchEnd=1, meSearchFound, meSearchInvalid, meFetchStart,
      meFetchLocal,  meFetchSource, meFetchDone,     meFetchError,
      meCheckOK,     meCheckOld,    meFetchBad,      meSendOld,
      meSendNew,     meSendTo,      meSendEnd
             );
procedure ShowPUPD;
  var st:byte;
  var id:tKey20;
  var ad:tNetAddr;
  begin
  repeat
    CopySP(sck,rpl); rpl.seek(0);
    st:=rpl.ReadByte;
    rpl.Read(id,20);
    rpl.read(ad,24);
    writeln(tMutEvt(st),' ',string(id),' ',string(ad));
  until tMutEvt(st)=meSendEnd;
end;
BEGIN
  if paramcount=0 then begin writeln('Invalid parameters'); halt(1) end;
  rpl.Init(4096);
  pl.Init(rpl.base,0,rpl.size);
  case upcase(paramstr(1)) of
    'INFO': op:=0;
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
END.

