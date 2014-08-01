unit IFC;
{$mode objfpc}

(* This unit listens sockets and sends *)

INTERFACE

uses DataGram
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
 end;

procedure BroadCast(var d :DataGram.T);
begin
 fpSend(inet.sock, @D.data, D.length, 0);
end;

type Tmcq=packed record {multicast join request}
     multi :in_addr;
     local :in_addr;
     iface :cint;
    end;

procedure Init;
 unimplemented{
  todo: error checking
 };
var SAddr :TInetSockAddr;
var mcq :packed record
     multi :in_addr;
     local :in_addr;
     iface :cint;
    end;
const ourmulticast:in_addr=(s_bytes:(1,1,1,1));
 {OMG preco to neslo zadat priamo ako inline constant????
  A co sakra je dotted quad? }
begin
 SAddr.sin_port:=Sockets.htons(1030);
 mcq.multi:=HostToNet(OurMulticast);

 SAddr.sin_family:=AF_INET;
 SAddr.sin_addr:=mcq.multi;
 mcq.local.s_addr:=INADDR_ANY;
 mcq.iface:=0;

 inet.sock:= fpSocket(SAddr.sin_family, SOCK_DGRAM, 0); //start socket
 fpSetSockOpt(inet.sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, @mcq, sizeof(mcq)); //Join multcast
 fpBind(inet.sock, @SAddr, sizeof(SAddr)); //Bind to multicast addr
 fpConnect(inet.sock, @SAddr, sizeof(SAddr)); //Set address/port so we can use send/recv instead of sendto/recvfrom
 fpListen(inet.sock, 0); //set Listen flag //unnecessary??

end;


BEGIN
 Init;
END.
