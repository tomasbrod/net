unit SocketUtil;

INTERFACE
uses Sockets
	,SysUtils
    ,NetAddr
    ;

type tDataGramSocket=object
 public
 handle: Sockets.tSocket;
 procedure Send( const Addr:NetAddr.t; var Data; Len:LongInt);
 procedure Recv( out Addr:NetAddr.t; var Data; var Len:LongInt);
 constructor Bind( const Addr:NetAddr.t; xtype:LongInt );
 constructor Init( family: NetAddr.tFamily; xtype:LongInt );
 destructor Done;
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
 var SockAddr :tSockAddr;
 var addrstr :string;
 begin
 //Addr.ToString(addrstr);
 //log.msg('Sending packet To '+addrstr);
 Addr.ToSocket(SockAddr);
 if fpsendto( {s} handle, {msg} @Data, {len} Len, {flags} 0, {name} @SockAddr, {namelen} SizeOf(SockAddr))<0 then CheckSocket;
end;

procedure tDataGramSocket.Recv( out Addr:NetAddr.t; var Data; var Len:LongInt);
 var SockAddr:tSockAddr;
 var addrlen : tSockLen;
 begin
 addrlen:= sizeof(SockAddr);
 Len:= fprecvfrom( handle, @Data, Len, 0, @SockAddr, @AddrLen );
 if Len<0 then CheckSocket;
 Addr.FromSocket(SockAddr);
end;

constructor tDataGramSocket.Bind( const Addr:NetAddr.t; xtype:LongInt );
 var SockAddr :tSockAddr;
 begin
 Addr.ToSocket(SockAddr);
 handle:= Sockets.fpsocket( SockAddr.sa_family, SOCK_DGRAM, xtype );
 if handle<0 then CheckSocket;
 if fpbind(handle, @SockAddr, sizeof(SockAddr))<0 then CheckSocket;
end;

constructor tDataGramSocket.Init( family: NetAddr.tFamily; xtype:LongInt );
 begin AbstractError; end;

destructor tDataGramSocket.Done;
 begin
 fpShutdown(handle,2);
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
 if e<>0 then raise eSocket.Create(e, '...');
end;

END.