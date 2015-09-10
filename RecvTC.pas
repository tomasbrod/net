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

procedure HandleMSG(sock:tSocket; var s:tMemoryStream; const from: tSockAddr);
 var opcode:byte;
 var trid:word;
 var sec:word;
 var sendbuf:array [1..128] of byte;
 var r:tMemoryStream;
 begin
 opcode:=s.readbyte;
 assert(opcode=6);
 s.Read(trid,2);
 s.Read(sec,2);
 r.Init(@sendbuf,0,128);
 r.WriteByte(4);
 trid:=22; //intel :)
 r.Write(trid,2);
 r.Write(sec,2);
 r.WriteWord(s.length,2);
 SC(@fpsendto,fpsendto(s_inet,r.base,r.length,0,@from,sizeof(sockaddr_in)));
end;

procedure s_SetupInet;
 var bind_addr:tInetSockAddr;
 begin
  with bind_addr do begin
   family:=AF_INET;
   port:=htons(3511);
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