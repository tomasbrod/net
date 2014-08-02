{PROGRAM bnDaemon;}
PROGRAM brodnetd;
(*
  Parameters:
   %1 ... io type
     ='S' -> io is socket
     ='C' -> io is character device
 *)
 
{$MODE OBJFPC}{$C+}
USES SysUtils
	,Sockets
	,UnixType
	,GeneralPacket
	,PingPacket
	;

procedure CheckSocket;
begin
 if SocketError<>0 then raise Exception.Create('Socket error '+IntToStr(SocketError));
end;

var
 Packet: array [1..1152] of byte; //MTU is 1280

PROCEDURE ProcessPacket;
begin
 case GeneralPacket.T((@Packet)^).pktype of
  PingPacket.cT: PingPacket.T((@Packet)^).Handle;
  {this should error PingPacket.cT: halt(42);}
 end;
end;

PROCEDURE LoopOnSocket;
 experimental;
const sock :tSocket =0;
var DataLen :LongWord;
var SockAddr :array [0..512] of byte;
    SockAddrLen :LongWord;
begin
 repeat
  //Recieve the incoming packet
  DataLen:= 
     fpRecvFrom(
     sock,
     @Packet,
     sizeof(Packet),
     0,
     @SockAddr,
     @SockAddrLen
  );CheckSocket;
  ProcessPacket;
 until false;
end;

PROCEDURE LoopOnCharDev;
 unimplemented;
begin
 abort;
end;

BEGIN
 case paramstr(1) of
  'S' : LoopOnSocket;
  'C' : LoopOnCharDev;
 end;
END.
