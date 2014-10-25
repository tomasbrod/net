{PROGRAM bnDaemon;}
PROGRAM bnd;
 
{$MODE OBJFPC}{$C+}
USES SysUtils
	,Sockets
	,UnixType
	,DataBase
	,Peers
	,IniFiles
	,NetAddr
	,Log
	,Encap
	,SocketUtil
	,BaseUnix
	;


var
 InPkMem :array [1..1152] of byte; //MTU is 1280
 InPk :^Peers.tPacket;
var InPkLen :LongInt;

const cRecvWait=200{ms};

PROCEDURE ProcessPacket;
var p:pointer;
 var addrstr:string;
begin
 p:=InPk;
 Peers.SelectedAddr.ToString( addrstr );
 if InPk^.pktype=Peers.cAkafuka then Peers.tAkafuka(p^).Handle else
 if InPk^.pktype=Peers.cFundeluka then Peers.tFundeluka(p^).Handle else
 if InPk^.pktype=Encap.cEncap then Encap.tEncap(p^).Handle else
  begin
  log.msg('Received Unknown #'+IntToStr(InPk^.pktype)+' ('+IntToStr(InPkLen)+'B) From '+addrstr);
  Abort;
 end;
end;

PROCEDURE LoopOnSocket;
 var FDS : BaseUnix.tFDSet;
 begin
 repeat
  {Reset internal state}
  Peers.Reset;
  InPk:=@InPkMem;
  InPkLen:= sizeof(InPkMem);
  BaseUnix.fpfd_zero(FDS);
  BaseUnix.fpfd_set(SocketUtil.Std,FDS);
  {Wait for data}
  Log.msg('Reading from socket'); Flush(log.F);
  {fpSelect returns >0 if data is available}
  if BaseUnix.fpSelect( SocketUtil.Std+1, @FDS, nil, nil, cRecvWait )<=0 then begin
   log.msg('No more Data');
   break;
  end;
  {Receive}
  //log.msg('Waiting for socket to be ready');
  SocketUtil.Recv( Peers.SelectedAddr, InPk^, InPkLen );
  ProcessPacket;
 until false; {infinite loop}
end;

{$I bncommon.pp}

procedure NewPeerHook( id :Peers.tID );
 var ids:string;
 begin
 id.ToString(ids);
 log.msg('Detected new Peer '+ids);
end;
 
BEGIN
 Log.Init(''); //log to stdout
 try
  Init;
  Peers.NewProc:=@NewPeerHook;
  if FindCmdLineSwitch('s') then begin
   LoopOnSocket;
  end else begin
   Log.msg('Use -s to receive from socket STDIN.');
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
