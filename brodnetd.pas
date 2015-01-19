PROGRAM brodnetd;
 
{$MODE OBJFPC}{$C+}
USES SysUtils
    ,StrUtils

	,Sockets
	,sSockets
	,SocketUtil
	,NetAddr
	,UnixType
	,BaseUnix
	,IniFiles
	,DataBase
	,EventLog
	,Peers
	,Controll
	{,StreamInit}
	,Keys
	,Transfer
	;

var Log: tEventLog;

const cRecvWait=5000{ms};
const cWarnTime=1/MsecsPerDay;
const cForceRest=6000/MSecsPerDay;

var LastIdle:tDateTime;

PROCEDURE Idle;
 var took:tdatetime;
 begin
 took:=now;
 lastidle:=now;

 Peers.DoAkafuka;
 Transfer.DoRetry;

 took:=now-took;
 if took>cwarntime then log.warning('System idle tasks took: '+FloatToStr(took*MsecsPerDay)+'ms');
end;

procedure PeerStateHook( event: byte; info:Peers.tInfo );
 var ids:string;
 begin
 info.addr.ToString(ids);
 if event>0 then log.info('Detected Peer state change: event='+IntToStr(event)+' addr='+ids+' delta='+FloatToStr(info.delta*MsecsPerDay)+'ms since='+DateTimeToStr(info.since));
 Controll.NotifyPeerStateChange(event,info);
end;

procedure TransferProgressHook( id :tFID; done,total:longword; by: byte );
 begin
 log.Info('Transfer state change: fid='+string(id)+' done='+IntToSTr(done)+' total='+IntToStr(total));
 Controll.NotifyTransfer(id,done,total,by);
end;

{
procedure TransferRecvHook( id :tFID; by :byte );
procedure TransferNoSrcHook( id :tFID; by :byte );
}

PROCEDURE ProcessPacket( var pk:Peers.tPacket; len:LongWord; const from:netaddr.t );
 var p:pointer;
 var addrstr:string;
 var took:tdatetime;
begin
 took:=now;
 try
 p:=@Pk;
 from.ToString( addrstr );
 {log.debug('Received from '+addrstr+' sz='+IntToStr(len));}
 case Pk.pktype of
  Peers.cAkafuka: Peers.tAkafuka(p^).Handle(from);
  Peers.cFundeluka: Peers.tFundeluka(p^).Handle(from);
  {
  StreamInit.cRequest: StreamInit.tRequest(p^).Handle(from);
  StreamInit.cReject: StreamInit.tReject(p^).Handle(from);
  StreamInit.cAccept: StreamInit.tAccept(p^).Handle(from);
  }
  Transfer.cRequest: Transfer.tRequest(p^).Handle(from);
  Transfer.cInfo: Transfer.tInfo(p^).Handle(from);
  Transfer.cData: Transfer.tData(p^).Handle(from,len);
  else begin
   log.error('Received Unknown #'+IntToStr(Pk.pktype)+' ('+IntToStr(SizeOf(Pk))+'B) From '+addrstr);
   Abort;
  end;
 end;
 Peers.tPacket(p^).AfterProcessing(from);
 except
  on e:Exception do begin
   Log.Error('Processing packet'); 
   Log.Error(e.ClassName+': '+e.message);
   DumpExceptionBackTrace(stderr);
 end; end;
 took:=now-took;
 if took>cwarntime then log.warning('Processing took: '+FloatToStr(took*MsecsPerDay)+'ms');
end;

var InPkMem: array [1..1024] of byte;
var ReqQuit:boolean=false;

var socketa: array [1..8] of tDatagramSocket;
var socketc:word=0;
var ctrl_socketl: tListeningSocket;

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
  BaseUnix.fpfd_set(ctrl_socketl.handle,FDS);
  Controll.SetSelectFDs(FDS);
  {Wait for data}
  try
  {fpSelect returns >0 if data is available}
   if BaseUnix.fpSelect( FDSM, @FDS, nil, nil, cRecvWait )<=0 then begin
    if not ReqQuit then Idle;
   end else begin

   for i:=1 to socketc do if BaseUnix.fpfd_isset(socketa[i].handle,FDS)=1 then begin
    (*log.debug('Received packet on socket #'+IntToStr(i));*)
    {Receive}
    socketa[i].Recv( from, InPkMem, InPkLen );
    ProcessPacket( InPk^, InPkLen, from );
   end;

    {Check commanders}
    if BaseUnix.fpfd_isset(ctrl_socketl.handle,FDS)=1 then Controll.HandleConnect(ctrl_socketl);
    Controll.Receive(FDS);

   end;
   if now-LastIdle>cForceRest then begin
    log.info('Rest forced ('+FloatToStr((now-LastIdle)*SecsPerDay)+'sec)');
    Idle;
   end;
  except
   on Exception do raise; {putis}
  end;
end;

procedure DoTerminate;
 begin
 if ReqQuit then begin
  log.warning('Shutdown enforced');
  raise eControlC.Create('Forced shutdown');
 end;
 log.warning('Shutting down, please wait');
 ReqQuit:=true;
end;

procedure TerminateHook (sig : cint); 
 cdecl;
 begin
 DoTerminate;
end;

function TerminateHook2( ctrlbreak:boolean ):boolean;
 begin
 DoTerminate;
 result:=true;
end;

procedure PacketSendHook(const rcpt:netaddr.t; var Data; Len:LongInt);
 var i:word;
 begin
 for i:=1 to socketc do with socketa[i] do begin
  if local.data.family=rcpt.data.family then begin
   (*log.debug('Sending to socket #'+IntToSTr(i));*)
   send(rcpt,data,len);
   exit;
  end;
 end;
 log.error('No socket in domain for '+string(rcpt));
 raise exception.create('No socket in domain for '+string(rcpt)); {todo}
end;

procedure StartListening(const config:tINIFile);
 var addr:NetAddr.T;
 var str:ansistring;
 procedure dostr;
  begin
  try addr.FromString( Copy2SpaceDel(str) );
  except on eConvertError do begin
    log.Error('Invalid bind config ('+str+')');
    log.Info('example: *bind=//ip4/0.0.0.0/1030');
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
  except on eSocket do begin
    log.error('Failed to bind to '+string(addr));
    raise {todo};
  end; end;
  log.info('Listening #'+IntToStr(socketc)+' on: '+string(addr));
 inc(socketc); end; dec(socketc);

 log.debug('Initializing ctrl');
 str:=config.ReadString('DAEMON','ctrl_bind','');
 dostr;
 try
  ctrl_socketl:=tListeningSocket.Bind(addr,0);
  ctrl_socketl.Listen;
 except on eSocket do begin
    log.error('Failed to bind to '+string(addr));
    raise {todo};
 end; end;
 log.info('Listening CMD/#0 on: '+string(addr));

 log.info('Idle treshold set to: '+IntToStr(cRecvWait)+'ms');
 log.info('Rest treshold set to: '+FloatToStr(cForceRest*MsecsPerDay)+'ms');
 if not assigned(ctrl_socketl) then raise exception.Create('No controll socket listening');
end;

procedure TEST;
 var addr:netaddr.t;
 begin
 {
 addr:='//ip4/127.0.0.1/1030';
 Peers.Add(addr);
 addr:='//ip4/127.0.0.1/1031';
 Peers.Add(addr);
 }
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
 Controll.Log:=Log;
 {Hook callbacks}
 fpSignal(SigInt,@TerminateHook);
 fpSignal(SigTerm,@TerminateHook);
 SysSetCtrlBreakHandler(@TerminateHook2);
 Peers.StateChangeProc:=@PeerStateHook;
 Peers.SendProc:=@PacketSendHook;
 Controll.OnTerminateRequest:=@DoTerminate;
 Transfer.OnProgress:=@TransferProgressHook;
 
 {Setup server socket}
 StartListening(config);
 config.Free;
 
 {Restore state}
 log.debug('Reading presistent storage');
 Peers.LoadState;

 TEST;
 {Enter the main loop}
 log.debug('Entering main loop');
 try
  repeat Loop; until ReqQuit;
 finally
  
  if not ReqQuit then log.warning('Shutdown enforced');
  {Save state}
  log.debug('Saving presistent data');
  Peers.SaveState;
  Controll.NotifyQuit(not ReqQuit);
  Transfer.NotifyQuit;
 
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
