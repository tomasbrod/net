uses MemStream,NetAddr
    ,SysUtils,Sockets,UnixType,BaseUnix
     ;


var s_inet:tSocket;

procedure SC(fn:pointer; retval:cint);
 begin
  if retval < 0 then begin
   raise eXception.Create(Format('Socket error %d operation %P',[SocketError,fn]));
  end;
 end;

var CurMark:byte=0;
var PrvMark:byte=0;
var StartT:tDateTime;
var Total:LongWord=0;
var DGcnt:LongWord=0;
{opcode 4=data, 8=data-no-report, 6=data-immediate-ack}
{opcode 5=cont, 7=ack}

procedure HandleMSG(sock:tSocket; var s:tMemoryStream; const from: tSockAddr);
 var opcode:byte;
 var mark:byte;
 var sendbuf:array [1..128] of byte;
 var r:tMemoryStream;
 var rateR:real;
 var rate:DWord; {BytesPerSecond shr 6 (=64)}
 begin
 r.Init(@sendbuf,0,128);
 opcode:=s.ReadByte;
 mark:=s.ReadByte;
 case opcode of
  4:begin
    if mark<>PrvMark then begin
    if mark<>CurMark then begin
     PrvMark:=CurMark;
     CurMark:=mark;
     StartT:=now;
     Total:=1;
     DgCnt:=1;
    end else begin Inc(Total,s.length); Inc(DgCnt) end;
    end;
   end;
  8:;
  6:begin
   r.WriteByte(7);
   r.WriteByte(mark);
   r.WriteWord(s.length,2);
   SC(@fpsendto,fpsendto(s_inet,r.base,r.length,0,@from,sizeof(sockaddr_in)));
   end;
 end;
 if DgCnt<8 then exit;
 if (now-Startt)<(0.4/SecsPerDay) then exit;
 rateR:=Total/((now-Startt)*SecsPerDay);
 writeln('Rate: ',(rateR/1024):7:1);
 rate:=round(rateR/64);
 StartT:=now;
 Total:=1;
 r.WriteByte(5);
 r.WriteByte(mark);
 r.WriteWord(rate,4);
 SC(@fpsendto,fpsendto(s_inet,r.base,r.length,0,@from,sizeof(sockaddr_in)));
end;

procedure s_SetupInet;
 var bind_addr:tInetSockAddr;
 begin
  with bind_addr do begin
   family:=AF_INET;
   port:=htons(3519);
   addr:=0; {any}
   s_inet:=fpSocket(family,SOCK_DGRAM,IPPROTO_UDP);
   SC(@fpSocket,s_inet);
  end;
  SC(@fpBind,fpBind(s_inet,@bind_addr,sizeof(bind_addr)));
 end;

var Terminated:boolean=false;

procedure SignalHandler(sig:cint);CDecl;
 begin
  writeln;
  if terminated then raise eControlC.Create('CtrlC DoubleTap') ;
  Terminated:=true;
  writeln('Shutdown requested');
 end;

procedure Loop;
 var Buffer:array [1..4096] of byte;
 var s:tMemoryStream;
 var pkLen:LongInt;
 var From:tSockAddr;
 var FromLen:LongWord;
 begin
 FromLen:=sizeof(From);
 pkLen:=fprecvfrom(s_inet,@Buffer,sizeof(Buffer),0,@from,@fromlen);
 SC(@fprecvfrom,pkLen);
 //writeln('size ',pkLen,' opcode ',buffer[1]);
 s.Init(@buffer,pkLen,sizeof(buffer));
 HandleMsg(s_inet,s,from);
end;

BEGIN
 s_setupInet;
 fpSignal(SigInt,@SignalHandler);
 fpSignal(SigTerm,@SignalHandler);
 repeat Loop until Terminated;
 write('Standard terminate [');
 CloseSocket(s_inet);
 writeln(']');
END.