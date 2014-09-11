PROGRAM bnctl;
 
{$MODE OBJFPC}{$C+}
USES SysUtils
    ,Sockets
    ,UnixType
	,DataBase
	,Peers
	,IniFiles
	,Log
	;


type eSocket=class(Exception)
 code :cint;
 constructor Create( icode: cint; msg: string );
end;

constructor eSocket.Create( icode: cint; msg: string );
 begin
 inherited Create(msg);
 code:=icode;
end;

procedure CheckSocket;
 inline;
var e:cint;
begin
 e:=SocketError;
 if e<>0 then raise eSocket.Create(e, '...');
end;

const cMainCfg:string='g.cfg';
const cMainLog:string='g.log';

procedure SocketSendImpl(var Data; Len:LongInt);
 const sock :tSocket =0;
 var SockAddr :tSockAddr;
 var addrstr :string;
 begin
 Peers.SelectedAddr.ToSocket(SockAddr);
 //Peers.SelectedAddr.ToString(addrstr);
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
 
BEGIN
 Log.Init(''); //log to stdout
 try
  Init;
  {ToDo: process cmgline args
   FindCmdLineSwitch('s');
  }
 except
  on e : eSocket do begin
   Log.msg('Socket error '+IntToStr(e.code));
   DumpExceptionBackTrace(Log.F);
  end;
  on e : Exception do begin
   Log.msg(e.ClassName+': '+e.message);
   DumpExceptionBackTrace(Log.F);
  end;
 end;
 Log.msg('Bye');
END.
