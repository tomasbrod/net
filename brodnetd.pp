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
	,DataBase
	,Peers
	,IniFiles
	,Log
	;


var
 InPkMem :array [1..1152] of byte; //MTU is 1280
 InPk :^Peers.tPacket;

PROCEDURE ProcessPacket;
var p:pointer;
 var addrstr:string;
begin
 p:=InPk;
 //Peers.SelectedAddr.ToString( addrstr );
 log.msg('Recieved #'+IntToStr(InPk^.pktype)+' From '+addrstr);
 if InPk^.pktype=Peers.cAkafuka then Peers.tAkafuka(p^).Handle else
 if InPk^.pktype=Peers.cFundeluka then Peers.tFundeluka(p^).Handle else
 Abort;
end;

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

PROCEDURE LoopOnSocket;
 const sock :tSocket =0;
 var SockAddr :tSockAddr;
 var SockAddrLen :LongWord;
 var InPkLen :LongInt;
 begin
 repeat
  Peers.Reset;
  //Recieve the incoming packet
  InPk:=@InPkMem;
  InPkLen:= 
     fpRecvFrom(
     {Socket} sock,
     {packet^} InPk,
     {maxlin} sizeof(InPkMem),
     {flags} 0,
     {addr^} @SockAddr,
     {addrl^} @SockAddrLen
  );
  CheckSocket;
  Peers.isSelectedAddr:=true;
  Peers.SelectedAddr.FromSocket( SockAddr );
  ProcessPacket;
 until true; {TODO: timeout ... }
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

procedure NewPeerHook( id :Peers.tID );
 var ids:string;
 begin
 id.ToString(ids);
 log.msg('Detected new Peer ',ids);
end;

PROCEDURE Init;
 var cfg :TINIFile;
 var str :string;
 begin
 DataBase.Prefix:=GetEnvironmentVariable('BRODNETD_DATA');
 if not FindCmdLineSwitch('stderr') then Log.Init( DataBase.Prefix+DirectorySeparator+cMainLog );
 log.msg('BrodnetDaemon, in '+Database.Prefix);
 //if Length(DataBase.Prefix)=0 then Abort;
 Peers.SendProc:=@SocketSendImpl;
 Peers.NewProc:=@NewPeerHook;
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
  if FindCmdLineSwitch('s') then begin
   Log.msg('Reading from socket');
   LoopOnSocket;
  end else begin
   Log.msg('Use -s to recieve from socket STDIN.');
  end;
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
