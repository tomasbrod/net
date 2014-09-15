{include file}

const cMainCfg:string='g.cfg';
const cMainLog:string='g.log';

procedure SocketSendImpl(var Data; Len:LongInt);
 var sock :tSocket;
 var SockAddr :tSockAddr;
 var addrstr :string;
 begin
 Peers.SelectedAddr.ToSocket(SockAddr);
 Peers.SelectedAddr.ToString(addrstr);
 log.msg('Sending packet To '+addrstr);
 case SockAddr.sa_family of
  AF_INET: sock:=fpSocket (AF_INET,SOCK_DGRAM,0);
  else AbstractError;
 end;
 CheckSocket;
 if fpsendto(
     {s} sock,
     {msg} @Data,
     {len} Len,
     {flags} 0,
     {tox} @SockAddr,
     {tolen} SizeOf(SockAddr)
   )<0 then CheckSocket;
 if CloseSocket(sock)<0 then CheckSocket;
end;

PROCEDURE Init;
 var cfg :TINIFile;
 var str :string;
 begin
 DataBase.Prefix:=GetEnvironmentVariable('BRODNETD_DATA');
 if not FindCmdLineSwitch('stderr') then Log.Init( DataBase.Prefix+DirectorySeparator+cMainLog );
 log.msg('Brodnet '+ApplicationName+', in '+Database.Prefix);
 //if Length(DataBase.Prefix)=0 then Abort;
 Peers.SendProc:=@SocketSendImpl;
 Peers.NewProc:=nil;
 cfg := TINIFile.Create(DataBase.Prefix+DirectorySeparator+cMainCfg);
 try
  try Peers.ThisID.FromString( cfg.ReadString('PEER','id','') );
  except log.msg('read peer id from config'); raise; end;
 finally
  cfg.Free;
 end;
end;
