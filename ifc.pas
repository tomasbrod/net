unit IFC;
{$mode objfpc}

(* This unit listens sockets and sends *)

INTERFACE

uses SysUtils
    ,DataGram
    ,Sockets
;

procedure BroadCast(var d :DataGram.T);
 experimental;
 unimplemented;
(* send datagram to all nearby nodes
   this could be implemented as multicast 
 *)

{
procedure UniCast(var d :DataGram.T; .....);
procedure Init;
}

IMPLEMENTATION

uses unixtype;

var
 inet :record
  sock :unixtype.cint;
  procedure init;
 end;

procedure CheckSocket;
begin
 if SocketError<>0 then raise Exception.Create('Socket error '+IntToStr(SocketError));
end;

procedure BroadCast(var d :DataGram.T);
begin
 fpSend(inet.sock, @D.data, D.length, 0);
 CheckSocket;
end;

type Tmcq=packed record {multicast join request}
     multi :in_addr;
     local :in_addr;
     iface :cint;
    end;

procedure INetInit;
var SAddr :TInetSockAddr;
begin

 SAddr.sin_family:=AF_INET;
 SAddr.sin_addr.s_addr:=INADDR_ANY;
 SAddr.sin_port:=Sockets.htons(1030);
 
 mcq.local.s_addr:=INADDR_ANY;
 mcq.iface:=0;

 inet.sock:= fpSocket(SAddr.sin_family, SOCK_DGRAM, 0); //start socket
 CheckSocket;

 fpSetSockOpt(inet.sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, @mcq, sizeof(mcq)); //Join multcast
 CheckSocket;

 fpBind(inet.sock, @SAddr, sizeof(SAddr));
 CheckSocket;

 //fpConnect(inet.sock, @SAddr, sizeof(SAddr)); //Set address/port so we can use send/recv instead of sendto/recvfrom
 CheckSocket;
 
end;


BEGIN
 INetInit;
END.
