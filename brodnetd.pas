PROGRAM brodnetd;
 
{$MODE OBJFPC}{$C+}
USES SysUtils
	,Sockets
	,UnixType
	,DataBase
	,Peers
	,IniFiles
	,NetAddr
	,SocketUtil
	,BaseUnix
	
	,EventLog
	;

var Log: tEventLog;

const cRecvWait=5000{ms};

PROCEDURE Idle;
 var took:tdatetime;
 begin
 took:=now;

 Peers.DoAkafuka;

 took:=now-took;
 log.info('System idle tasks took: '+FloatToStr(took*MsecsPerDay)+'ms');
end;

procedure PeerStateHook( event: byte; info:tInfo );
 var ids:string;
 begin
 info.addr.ToString(ids);
 log.info('Detected Peer state change: addr='+ids+' delta='+FloatToStr(info.delta*MsecsPerDay)+'ms since='+DateTimeToStr(info.since));
end;

PROCEDURE ProcessPacket( pk:Peers.tPacket; len:LongWord; const from:netaddr.t );
 var p:pointer;
 var addrstr:string;
begin
 try
 p:=@Pk;
 from.ToString( addrstr );
 if Pk.pktype=Peers.cAkafuka then Peers.tAkafuka(p^).Handle(from) else
 if Pk.pktype=Peers.cFundeluka then Peers.tFundeluka(p^).Handle(from) else
  begin
  log.error('Received Unknown #'+IntToStr(Pk.pktype)+' ('+IntToStr(SizeOf(Pk))+'B) From '+addrstr);
  Abort;
 end;
 Peers.tPacket(p^).AfterProcessing(from);
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
 var from:netaddr.t;
 begin
 repeat
  {Reset internal state}
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
   socket.Recv( from, InPk^, InPkLen );
  except
   on Exception do begin Log.Error('Reading from socket'); raise end;
  end;
  ProcessPacket(InPk^,InPkLen,from);
 until (ReqQuit);
end;

procedure TerminateHook (sig : cint); 
 cdecl;
 begin
 log.info('Receiving signal: '+IntToStr(sig));
 ReqQuit:=ReqQuit or true;
end;

function TerminateHook2( ctrlbreak:boolean ):boolean;
 begin
 if ctrlbreak then log.info('Control Break') else log.info('Control Cancel');
 TerminateHook(0);
 result:=true;
end;

procedure StartListening(var socket:tDatagramSocket; const config:tINIFile);
 var addr:NetAddr.T;
 begin
 log.debug('Initializing network');
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
 log.info('Listening on: '+string(addr));
 log.info('Idle treshold set to: '+IntToStr(cRecvWait)+'ms');
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
 Log.FileName:='/dev/stderr'; //even more temp
 Log.Active:=True;
 Log.info('*** '+ApplicationName+', in '+Database.Prefix);
 Peers.Log:=Log;
 {Hook callbacks}
 fpSignal(SigInt,@TerminateHook);
 fpSignal(SigTerm,@TerminateHook);
 SysSetCtrlBreakHandler(@TerminateHook2);
 Peers.StateChangeProc:=@PeerStateHook;
 
 {Setup server socket}
 StartListening(socket, config);
 
 {Restore state}
 log.debug('Reading presistent storage');
 Peers.LoadState;

 {Enter the main loop}
 log.info('Entering main loop');
 try
  Loop(socket);
  if ReqQuit then Log.info('Requested shutdown!');
 finally
  
  {Save state}
  log.debug('Saving presistent data');
  Peers.SaveState;
 end;
 
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
 log.error('Failed to stop network, unimplemented');
 Log.info('STOP');
END.
