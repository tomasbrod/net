unit SocketUtil;

INTERFACE
uses Sockets
	,SysUtils
    ,NetAddr
    ;

type tDataGramSocket=object
 public
 handle: Sockets.tSocket;
 local:netaddr.t;
 procedure Send( const Addr:NetAddr.t; var Data; Len:LongInt);
 procedure Recv( out Addr:NetAddr.t; var Data; var Len:LongInt);
 constructor Bind( const Addr:NetAddr.t; xtype:LongInt );
 constructor Init( family: NetAddr.tFamily; xtype:LongInt );
 destructor Done;
  unimplemented;
 end;

type tListeningSocket=class (tObject)
 public
 handle: Sockets.tSocket;
 constructor Create(aSocket: Sockets.tSocket);
  unimplemented;
 destructor  Destroy; override;
  experimental;
 procedure Listen;
 procedure Accept(out connected:Sockets.tSocket; out addr:NetAddr.t);
  experimental;
 constructor Bind( const Addr:NetAddr.t; xtype:LongInt );
end;

type eSocket=class(Exception)
 code :integer;
 constructor Create( icode: integer; msg: string );
end;

procedure CheckSocket; inline;

IMPLEMENTATION
uses Peers
    ;

procedure tDataGramSocket.Send( const Addr:NetAddr.t; var Data; Len:LongInt);
 var SockAddr :tSockAddrL;
 var addrstr :string;
 begin
 //Addr.ToString(addrstr);
 //log.msg('Sending packet To '+addrstr);
 Addr.ToSocket(SockAddr);
 if fpsendto( {s} handle, {msg} @Data, {len} Len, {flags} 0, {name} @SockAddr, {namelen} SizeOf(SockAddr))<0 then CheckSocket;
end;

procedure tDataGramSocket.Recv( out Addr:NetAddr.t; var Data; var Len:LongInt);
 var SockAddr:tSockAddrL;
 var addrlen : tSockLen;
 begin
 addrlen:= sizeof(SockAddr);
 Len:= fprecvfrom( handle, @Data, Len, 0, @SockAddr, @AddrLen );
 if Len<0 then CheckSocket;
 Addr.FromSocket(SockAddr);
end;

constructor tDataGramSocket.Bind( const Addr:NetAddr.t; xtype:LongInt );
 var SockAddr :tSockAddrL;
 var so:integer;
 begin
 local:=addr;
 Addr.ToSocket(SockAddr);
 handle:= Sockets.fpsocket( SockAddr.sa_family, SOCK_DGRAM, xtype );
 if handle<0 then CheckSocket;
 if addr.data.family=afInet6 then begin
  //Set socket option to only take ipv6
  so:=1;
  if fpSetSockOpt(handle, IPPROTO_IPV6, IPV6_V6ONLY, @so, sizeof(so))<0 then CheckSocket;
 end;
 if fpbind(handle, @SockAddr, sizeof(SockAddr))<0 then CheckSocket;
end;

constructor tDataGramSocket.Init( family: NetAddr.tFamily; xtype:LongInt );
 begin AbstractError; end;

destructor tDataGramSocket.Done;
 begin
 fpShutdown(handle,2);
end;

constructor tListeningSocket.Create(aSocket: Sockets.tSocket);
 begin AbstractError; end;
destructor tListeningSocket.Destroy;
 begin
 fpShutdown(handle,2);
 FileClose(handle);
end;

procedure tListeningSocket.Listen;
 begin
 if fpListen(handle, 0)<0 then CheckSocket;
end;

procedure tListeningSocket.Accept(out connected:Sockets.tSocket; out addr:NetAddr.t);
 var sockaddr:tSockAddrL;
 var addrlen : tSockLen;
 begin
 addrlen:=sizeof(sockaddr);
 connected:=fpAccept(handle,@sockaddr,@addrlen);
 if connected<0 then CheckSocket;
 addr.FromSocket(sockaddr);
end;

constructor tListeningSocket.Bind( const Addr:NetAddr.t; xtype:LongInt );
 var SockAddr :tSockAddrL;
 begin
 Addr.ToSocket(SockAddr);
 handle:= Sockets.fpsocket( SockAddr.sa_family, SOCK_STREAM, xtype );
 if handle<0 then CheckSocket;
 if fpbind(handle, @SockAddr, sizeof(SockAddr))<0 then CheckSocket;
end;

constructor eSocket.Create( icode: integer; msg: string );
 begin
 inherited Create(msg);
 code:=icode;
end;

procedure CheckSocket;
 inline;
var e:integer;
begin
 e:=SocketError;
 if e<>0 then raise eSocket.Create(e, '...'+IntToStr(e));
end;

END.