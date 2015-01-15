program brodnetctl;

{$MODE OBJFPC}{$C+}
USES CustApp
    ,SysUtils
    ,StrUtils
    ,Sockets
	,sSockets
	,SocketUtil
	,NetAddr
	,UnixType
	,BaseUnix
	,CtrlIface
	{,Transfer}
	;

type tApp=class(tCustomApplication)
 public
 procedure Initialize; override;
 destructor Destroy; override;
 protected
 Sock:tSocketStream;
 procedure Connect( trg: NetAddr.t ); virtual;
 {procedure ShowException(); override;}
 procedure DoRun; override;
 procedure SendCommand(cmd:byte); virtual;
 procedure EvCommand(command:AnsiString);
 procedure EvEvent(event:byte);
end;
var App:tApp;

const cRecvWait=2000{ms};

procedure tApp.Connect( trg: NetAddr.t );
 var consocket:tSocket;
 var sockaddr:tSockAddrL;
 begin
 trg.ToSocket(sockaddr);
 consocket:=fpSocket(sockaddr.sa_family,Sockets.SOCK_STREAM,0);
 if consocket=-1 then CheckSocket;
 if fpConnect(consocket,@sockaddr,sizeof(sockaddr))=-1 then CheckSocket;
 Sock:=tSocketStream.Create(consocket);
end;

procedure tApp.Initialize;
 const cdas='s';
 const cdal='daemon-address';
 var DaemonAddr:string;
 begin
 Inherited;
 DaemonAddr:=GetOptionValue(cdas,cdal);
 if DaemonAddr='' then DaemonAddr:='//ip4/127.0.0.1/1031';
 writeln('Connecting to daemon '+DaemonAddr);
 Connect(DaemonAddr);
end;

procedure tApp.DoRun;
 var FDS : BaseUnix.tFDSet;
 var FDSM: LongInt=99 unimplemented;
 var command:string;
 var event:byte;
 begin
 Inherited;
 BaseUnix.fpfd_zero(FDS);
 BaseUnix.fpfd_set(Sock.handle,FDS);
 BaseUnix.fpfd_set(0,FDS); //STDIN
 {Wait for data}
 if BaseUnix.fpSelect( FDSM, @FDS, nil, nil, cRecvWait )<=0 then begin
  write('.');
  exit;
 end;
 if BaseUnix.fpfd_isset(Sock.Handle,FDS)=1 then try
  writeln;write('[');
  try
   event:=Sock.ReadByte;
  except on e:Exception do begin event:=255; end; end;
  EvEvent(event);
  finally
  write(']');
 end else if BaseUnix.fpfd_isset(0,FDS)=1 then begin
  readln(Command);
  if Command='' then begin
   write('>');
   readln(Command);
  end;
  EvCommand(Command);
 end;
end;

procedure tApp.EvCommand(command:AnsiString);
 var par:string;
 procedure AddPeer;
  var a:netaddr.t; begin
  a.FromString(par);
  SendCommand(ccAddPeer);
  Sock.WriteBuffer(a,sizeof(a));
 end;
 procedure CmdTransferRequestProgress; unimplemented;
  {var fid:Transfer.tFID;}
  begin
  {fid.FromString(par);
  SendCommand(ccTransferRequest);
  Sock.WriteBuffer(fid,sizeof(fid));
  SendCommand(ccTransferProgress);
  Sock.WriteBuffer(fid,sizeof(fid));}
 end;
 procedure CmdFID(cmd:byte); unimplemented;
  {var fid:Transfer.tFID;}
  begin
  {fid.FromString(par);
  SendCommand(cmd);
  Sock.WriteBuffer(fid,sizeof(fid));}
 end;
 var sp:word;
 begin
 sp:=pos(' ',command); if sp=0 then par:='' else begin 
 par:=copy(command,sp+1,1000); command:=copy(command,1,sp-1); end;
 case command of
  '':;
  'q': Terminate;
  'stop': SendCommand(ccTerminate);
  'peerstates': SendCommand(ccPeerStates);
  'addpeer': AddPeer;
  'TransReq': CmdTransferRequestProgress;
  'TransReqq': CmdFID(ccTransferRequest);
  'TransAbort': CmdFID(ccTransferAbort);
  'TransProgr': CmdFID(ccTransferProgress);
  'TransList': SendCommand(ccTransferListAll);
  else writeln('Invalid command');
 end;
end;

procedure tApp.EvEvent(event:byte);
 var tmp:byte;
 var tmw:word2;
 var tm4:word4;
 procedure PrintAddr;
  var a:netaddr.t;
  begin
  Sock.ReadBuffer(a,sizeof(a));
  write(string(a));
 end;
 procedure DumpID; unimplemented;
  var a:array [1..20] of byte{Keys.tID};
  begin
  Sock.ReadBuffer(a,sizeof(a));
  {write(string(a));}
 end;
 procedure DumpFID; unimplemented;
  {var fid:Transfer.tFID;}
  begin
  {Sock.ReadBuffer(fid,sizeof(fid));
  write(string(fid));}
 end;
 begin
 case event of
  ceQuit: begin
   write('Daemon terminating');;write(' ');
   tmp:=Sock.ReadByte;
   case tmp of
    0: write('by request');
    1: write('crashed');
    else write('reason=',tmp);
  end; end;
  ceInvalid: write('Invalid command');
  cePeerState: begin
   write('Peer');write(' ');
   tmp:=Sock.ReadByte;
   case tmp of
    0: write('ping');
    1: write('NEW!');
    2: write('REKT');
    else write('ev=',tmp);
   end;;write(' ');
   PrintAddr;;write(' ');
   Sock.ReadBuffer(tmw,2);
   write('delta=',word(tmw),'ms');
  end;
  ceTransfer: begin
   write('Transfer');write(' ');
   DumpFID;
   Sock.ReadBuffer(tm4,4);
   write(longword(tm4));
   Sock.ReadBuffer(tm4,4);
   write('/',longword(tm4),'b');
  end;
  255: begin write('Disconnected!'); Terminate; end;
  else write('?'+IntToStr(event));
 end;
end;

procedure tApp.SendCommand(cmd:byte);
 var i:word;
 begin
 Sock.WriteByte(cmd);
 {
 assert(high(aparam)=high(aparamsize));
 if high(aparam)>1 then for i:=0 to high(aparam) do begin
   writeln('param s ',aparamsize[i]);
   Sock.WriteBuffer(aparam[i]^,aparamsize[i]);
 end;
 }
end;

destructor tApp.Destroy;
 begin
 writeln('[normal exit]');
 SendCommand(ccQuit);
 Sock.Free;
 Inherited;
end;
 
BEGIN
 App:=tApp.Create(nil);
 App.Initialize;
 App.Run;
 App.Free;
END.
