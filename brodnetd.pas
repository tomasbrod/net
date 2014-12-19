PROGRAM brodnetd;
 
{$MODE OBJFPC}{$C+}
USES SysUtils
	,Sockets
	,UnixType
	,DataBase
	,Peers
	,IniFiles
	,NetAddr
	,Encap
	,SocketUtil
	,BaseUnix
	
	,EventLog
	;

var Log: tEventLog;

const cRecvWait=1500{ms};

PROCEDURE Idle;
 begin
 log.info('Processing sheduled tasks');
end;

procedure NewPeerHook( event: byte; info:tInfo );
 var ids:string;
 begin
 id.ToString(ids);
 log.info('Detected new Peer '+ids);
end;

PROCEDURE ProcessPacket( pk:Peers.tPacket; len:LongWord );
 var p:pointer;
 var addrstr:string;
begin
 try
 p:=@Pk;
 Peers.SelectedAddr.ToString( addrstr );
 if Pk.pktype=Peers.cAkafuka then Peers.tAkafuka(p^).Handle else
 if Pk.pktype=Peers.cFundeluka then Peers.tFundeluka(p^).Handle else
 if Pk.pktype=Encap.cEncap then Encap.tEncap(p^).Handle else
  begin
  log.error('Received Unknown #'+IntToStr(Pk.pktype)+' ('+IntToStr(SizeOf(Pk))+'B) From '+addrstr);
  Abort;
 end;
 Peers.tPacket(p^).ResetTimeSince;
 except
  on e:Exception do begin
   Log.Error('Processing packet'); 
   Log.Error(e.ClassName+': '+e.message);
 end; end;
end;

var InPkMem: array [1..1024] of byte;
var ReqQuit:boolean=false;

PROCEDURE Loop(socket:tDataGramSocket);
 var FDS : BaseUnix.tFDSet;
 var InPk: ^Peers.tPacket;
 var InPkLen: LongInt;
 begin
 repeat
  {Reset internal state}
  Peers.Reset;
  InPk:=@InPkMem;
  InPkLen:= sizeof(InPkMem);
  BaseUnix.fpfd_zero(FDS);
  BaseUnix.fpfd_set(socket.handle,FDS);
  //Flush(log.F);
  {Wait for data}
  try
  {fpSelect returns >0 if data is available}
   if BaseUnix.fpSelect( socket.handle+1, @FDS, nil, nil, cRecvWait )<=0 then begin
    if not ReqQuit then Idle;
    continue;
   end;
   {Receive}
   //log.msg('Waiting for socket to be ready');
   socket.Recv( Peers.SelectedAddr, InPk^, InPkLen );
  except
   on Exception do begin Log.Error('Reading from socket'); raise end;
  end;
  ProcessPacket(InPk^,InPkLen);
 until (ReqQuit);
end;

procedure TerminateHook (sig : cint); 
 cdecl;
 begin
 log.info('Receiving signal: '+IntToStr(sig));
 ReqQuit:=ReqQuit or true;
end;

procedure StartListening(var socket:tDatagramSocket; const config:tINIFile);
 var addr:NetAddr.T;
 begin
 try addr.FromString( config.ReadString('PEER','inet4_bind','--') );
 except on eConvertError do begin
   log.Error('Invalid inet4_bind config');
   log.Info('example: inet4_bind=//ip4/0.0.0.0/1030');
   raise {todo};
 end; end;
 try socket.Bind(addr,0);
 except on eSocket do begin
   log.error('Failed to bind'{+todo});
   raise {todo};
 end; end;
end;

var Config:tINIFile;
var socket:tDatagramSocket;
 
BEGIN
 {Setup database}
 DataBase.Prefix:=GetEnvironmentVariable('BRODNETD_DATA');
 if DataBase.Prefix='' then DataBase.Prefix:=GetCurrentDir+DirectorySeparator+'data';
 {setup config}
 Config := TINIFile.Create(DataBase.Prefix+DirectorySeparator+'g.ini');
 {setup log}
 Log:=tEventLog.Create(nil);
 Log.LogType:=ltFile;
 //Log.FileName:=Database.Prefix+DirectorySeparator+'g.log'; //temp
 Log.FileName:='/dev/stderr';
 Log.Active:=True;
 Log.info('*** '+ApplicationName+', in '+Database.Prefix);
 
 {Hook callbacks}
 fpSignal(SigInt,@TerminateHook);
 fpSignal(SigTerm,@TerminateHook);
 Peers.StateChangeProc:=@PeerStateHook;
 
 {Setup server socket}
 StartListening(socket, config);
 {Enter the main loop}
 log.info('Entering main loop');
 Loop(socket);
 if ReqQuit then Log.info('Requested shutdown!');
 
 {
 except
  on e : eSocket do begin
   Log.msg('Socket error '+IntToStr(e.code));
   DumpExceptionBackTrace(Log.F);
  end;
  on e : Exception do begin
   Log.msg(e.ClassName+': '+e.message);
   DumpExceptionBackTrace(Log.F);
  end;
 }
 Log.info('STOP');
END.
