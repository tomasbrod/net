unit SocketUtil;

INTERFACE
uses Sockets
    ,NetAddr
    ,Encap
    ;

const Std :tSocket=0; {STDIN}
var StdFamily :NetAddr.tFamily =afNil;

procedure Send( const sock :tSocket; const Addr:NetAddr.t; var Data; Len:LongInt);
 overload;
 {$HINT Not actually testet, byt seems to work}

procedure Recv( out Addr:NetAddr.t; var Data; var Len:LongInt);
 overload;
 {$HINT Not actually testet, byt seems to work}

procedure CreateAndSend( var Data; Len:LongInt);
 {$HINT Not actually testet, byt seems to work}

procedure SendOrEncap( var Data; Len:LongInt);
 {$HINT Not actually testet, byt seems to work}

IMPLEMENTATION
uses SysUtils
    ,Log
    ,Peers
    ;

procedure Send(const sock :tSocket; const Addr:NetAddr.t; var Data; Len:LongInt);
 var SockAddr :tSockAddr;
 var addrstr :string;
 begin
 Peers.SelectedAddr.ToString(addrstr);
 log.msg('Sending packet To '+addrstr);
 Addr.ToSocket(SockAddr);
 if fpsendto( {s} sock, {msg} @Data, {len} Len, {flags} 0, {name} @SockAddr, {namelen} SizeOf(SockAddr))<0 then CheckSocket;
end;

procedure Recv( out Addr:NetAddr.t; var Data; var Len:LongInt);
 var SockAddr:tSockAddr;
 var addrlen : tSockLen;
 begin
 addrlen:= sizeof(SockAddr);
 Len:= fprecvfrom( Std, @Data, Len, 0, @SockAddr, @AddrLen );
 if Len<0 then CheckSocket;
 Addr.FromSocket(SockAddr);
 StdFamily:=Addr.data.Family;
end;

procedure CreateAndSend(var Data; Len:LongInt);
 var sock :tSocket;
 begin
 assert(not Peers.SelectedAddr.isNil);
 case Peers.SelectedAddr.data.Family of
  afInet: sock:=fpSocket( AF_INET, SOCK_DGRAM, 0 );
  //afIP6: sock:=fpSocket( AF_INET6, SOCK_DGRAM, 0 );
  else AbstractError;
 end;
 if sock<0 then CheckSocket;
 try
  Send( sock, Peers.SelectedAddr, Data, Len );
 finally
  CloseSocket(sock);
 end;
end;

procedure SendOrEncap(var Data; Len:LongInt);
 var pkencap :^Encap.tEncap=nil;
 var str:string;
 begin
 if Peers.SelectedAddr.data.Family = StdFamily then begin
  assert(not Peers.SelectedAddr.isNil);
  Send( Std, Peers.SelectedAddr, Data, Len );
 end else begin
  WriteStr(str, 'Have to encap becouse dest.AF (',Peers.SelectedAddr.data.Family,') <> OurSocket.AF (',StdFamily,')');
  log.msg(str);
  GetMem( pkencap, SizeOf(pkencap^)+Len );
  pkencap^.Create( Len, Data );
  Peers.SendProc:=@CreateAndSend;
  pkencap^.Send;
  Peers.SendProc:=@SendOrEncap;
  FreeMem( pkencap, SizeOf(pkencap^)+Len );
 end;
end;

END.