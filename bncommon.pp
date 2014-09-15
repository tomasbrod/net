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
 fpsendto(
     {s} sock,
     {msg} @Data,
     {len} Len,
     {flags} 0,
     {tox} @SockAddr,
     {tolen} SizeOf(SockAddr)
   ); CheckSocket;
end;

PROCEDURE Init;
 var cfg :TINIFile;
 var str :string;
 begin
 DataBase.Prefix:=GetEnvironmentVariable('BRODNETD_DATA');
 if not FindCmdLineSwitch('stderr') then Log.Init( DataBase.Prefix+DirectorySeparator+cMainLog );
 log.msg('Brodnet Controll, in '+Database.Prefix);
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
