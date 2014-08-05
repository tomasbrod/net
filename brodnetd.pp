{PROGRAM bnDaemon;}
PROGRAM brodnetd;
(*
  Parameters:
   %1 ... io type
     ='S' -> io is socket
     ='C' -> io is character device
   %2 ... data dir
 *)
 
{$MODE OBJFPC}{$C+}
USES SysUtils
	,Sockets
	,UnixType
	,GeneralPacket
	,PingPacket
	;


var
 InPkMem :array [1..1152] of byte; //MTU is 1280
 InPk :^GeneralPacket.T;

PROCEDURE ProcessPacket;
begin
 case InPk^.pktype of
  PingPacket.pktype: PingPacket.T(InPk^).Handle;
  else abort;
 end;
end;

{ Bullshit follows }

procedure CheckSocket;
begin
 if SocketError<>0 then raise Exception.Create('Socket error '+IntToStr(SocketError));
end;

var SockAddr :array [0..512] of byte;
    SockAddrLen :LongWord;
    (* Usefull to send reply *)

procedure SocketRecvImpl(var Data; MaxLen:LongInt);
const sock :tSocket =0;
var DataLen :LongWord;
begin
  //Recieve the incoming packet
  DataLen:= 
     fpRecvFrom(
     sock,
     @Data,
     MaxLen,
     0,
     @SockAddr,
     @SockAddrLen
  );CheckSocket;
end;

procedure SocketReplImpl(var Data; MaxLen:LongInt);
 unimplemented;
begin
 AbstractError;
end;

PROCEDURE LoopOnSocket;
begin
 RecvProc:=@SocketRecvImpl;
 ReplProc:=@SocketReplImpl;
 repeat
  InPk^.Recv(sizeof(InPkMem));
  ProcessPacket;
 until false;
end;

PROCEDURE LoopOnCharDev;
begin
 AbstractError;
end;

BEGIN
 assert(sizeof(InPkMem)<2048);
 InPk:=@InPkMem;
 If paramcount<2 then abort;
 case paramstr(1) of
  'S' : LoopOnSocket;
  'C' : LoopOnCharDev;
  else abort;
 end;
END.
