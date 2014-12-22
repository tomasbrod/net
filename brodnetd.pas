PROGRAM brodnetd;
 
{$MODE OBJFPC}{$C+}
USES SysUtils
    ,StrUtils
	,Sockets
	,UnixType
	,DataBase
	,Peers
	,IniFiles
	,NetAddr
	,SocketUtil
	,sSockets
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

var socketa: array [1..8] of tDatagramSocket;
var socketc:word=0;
var ctrl_socketa: array [1..8] of tDatagramSocket unimplemented;
var ctrl_socketc:word=0;

PROCEDURE Loop;
 var FDS : BaseUnix.tFDSet;
 var FDSM: LongInt=99 unimplemented;
 var InPk: ^Peers.tPacket;
 var InPkLen: LongInt;
 var from:netaddr.t;
 var i:word;
 begin
  {Reset internal state}
  InPk:=@InPkMem;
  InPkLen:= sizeof(InPkMem);
  BaseUnix.fpfd_zero(FDS);
  for i:=1 to socketc do BaseUnix.fpfd_set(socketa[i].handle,FDS);
  (*for i:=1 to ctrl_socketc do BaseUnix.fpfd_set(ctrl_socketa[i].handle,FDS);*)
  {Wait for data}
  try
  {fpSelect returns >0 if data is available}
   if BaseUnix.fpSelect( FDSM, @FDS, nil, nil, cRecvWait )<=0 then begin
    if not ReqQuit then Idle;
   end else begin
   (*
   for i:=1 to ctrl_socketc do if BaseUnix.fpfd_isset(ctrl_socketa[i].handle,FDS)=1 then begin
    log.info('Received command on socket '+IntToStr(i));
    ProcessCommand(ctrl_socketa[i]);
   end;
   *)
   for i:=1 to socketc do if BaseUnix.fpfd_isset(socketa[i].handle,FDS)=1 then begin
    log.info('Received packet on socket '+IntToStr(i));
    {Receive}
    socketa[i].Recv( from, InPk^, InPkLen );
   end;
   end;
  except
   on Exception do raise; {putis}
  end;
end;

procedure TerminateHook (sig : cint); 
 cdecl;
 begin
 if ReqQuit then begin
  log.warning('Shutdown enforced');
  raise eControlC.Create('Forced shutdown');
 end;
 log.warning('Shutting down, please wait');
 ReqQuit:=true;
end;

function TerminateHook2( ctrlbreak:boolean ):boolean;
 begin
 TerminateHook(0);
 result:=true;
end;

procedure StartListening(const config:tINIFile);
 var addr:NetAddr.T;
 var str:ansistring;
 procedure dostr;
  begin
  try addr.FromString( Copy2SpaceDel(str) );
  except on eConvertError do begin
    log.Error('Invalid bind config');
    log.Info('example: bind=//ip4/0.0.0.0/1030');
    raise {todo};
  end; end;
 end;

 begin
 log.debug('Initializing network');
 str:=config.ReadString('DAEMON','bind','');
 socketc:=1; while (str<>'')and(socketc<high(socketa)) do begin
  dostr;
  try
   socketa[socketc].Bind(addr,0);
   //if fpListen(socketa[socketc].handle, 0)<0 then CheckSocket; not working
  except on eSocket do begin
    log.error('Failed to bind to '+string(addr));
    raise {todo};
  end; end;
  log.info('Listening #'+IntToStr(socketc)+' on: '+string(addr));
 inc(socketc); end; dec(socketc);
 (*
 log.debug('Initializing ctrl');
 str:=config.ReadString('DAEMON','ctrl_bind','');
 ...
 *)

 log.info('Idle treshold set to: '+IntToStr(cRecvWait)+'ms');
end;

var Config:tINIFile;

BEGIN
 {Setup database}
 DataBase.Prefix:=GetEnvironmentVariable('BRODNETD_DATA');
 if DataBase.Prefix='' then DataBase.Prefix:=GetCurrentDir+DirectorySeparator+'data';
 {setup config}
 Config := TINIFile.Create(DataBase.Prefix+DirectorySeparator+'g.ini');
 {setup log}
 Log:=tEventLog.Create(nil);
 Log.RaiseExceptionOnError:=True;
 Log.Identification:=ApplicationName(*+':'+IntToStr(GetProcessID)*);
 Log.TimeStampFormat:=(*'yyyy-mm-dd*) 'hh:nn:ss.zzz';
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
 StartListening(config);
 config.Free;
 
 {Restore state}
 log.debug('Reading presistent storage');
 Peers.LoadState;

 {Enter the main loop}
 log.debug('Entering main loop');
 try
  repeat Loop; until ReqQuit;
 finally
  
  {Save state}
  log.debug('Saving presistent data');
  Peers.SaveState;
 
 (*
 except
  on e : eSocket do begin
   Log.msg('Socket error '+IntToStr(e.code));
   DumpExceptionBackTrace(Log.F);
  end;
  on e : Exception do begin
   Log.msg(e.ClassName+': '+e.message);
   DumpExceptionBackTrace(Log.F);
  end;
 *)
  log.error('Failed to stop network, unimplemented');
  log.Free;
 end;
END.
