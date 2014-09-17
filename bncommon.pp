{include file}

const cMainCfg:string='g.cfg';
const cMainLog:string='g.log';

var StdSockFamily:NetAddr.tFamily;

procedure Send(const sock :tSocket; const Addr:NetAddr.t; var Data; Len:LongInt);
 overload;
 var SockAddr :tSockAddr;
 var addrstr :string;
 begin
 Peers.SelectedAddr.ToString(addrstr);
 log.msg('Sending packet To '+addrstr);
 Addr.ToSocket(SockAddr);
 if fpsendto( {s} sock, {msg} @Data, {len} Len, {flags} 0, {name} @SockAddr, {namelen} SizeOf(SockAddr))<0 then CheckSocket;
end;

procedure SocketCreateAndSend(var Data; Len:LongInt);
 var sock :tSocket;
 begin
 assert(Peers.IsSelectedAddr);
 case Peers.SelectedAddr.data.family of
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

procedure SocketSendOrEncap(var Data; Len:LongInt);
 const sock :tSocket=0; {STDIN}
 var pkencap :^Encap.tEncap;
 begin
 if Peers.SelectedAddr.data.Family = StdSockFamily then begin
  assert(Peers.IsSelectedAddr);
  Send( sock, Peers.SelectedAddr, Data, Len );
 end else begin
  GetMem( pkencap, SizeOf(pkencap^)+Len );
  pkencap^.Create( Len, Data );
  Peers.SendProc:=@SocketCreateAndSend;
  pkencap^.Send;
  Peers.SendProc:=@SocketSendOrEncap;
  FreeMem( pkencap, SizeOf(pkencap^)+Len );
 end;
end;

PROCEDURE Init;
 var cfg :TINIFile;
 var str :string;
 begin
 DataBase.Prefix:=GetEnvironmentVariable('BRODNETD_DATA');
 if not FindCmdLineSwitch('stderr') then Log.Init( DataBase.Prefix+DirectorySeparator+cMainLog );
 log.msg('Brodnet '+ApplicationName+', in '+Database.Prefix);
 //if Length(DataBase.Prefix)=0 then Abort;
 Peers.SendProc:=@SocketSendOrEncap;
 Peers.NewProc:=nil;
 cfg := TINIFile.Create(DataBase.Prefix+DirectorySeparator+cMainCfg);
 try
  try Peers.ThisID.FromString( cfg.ReadString('PEER','id','') );
  except log.msg('read peer id from config'); raise; end;
 finally
  cfg.Free;
 end;
end;
