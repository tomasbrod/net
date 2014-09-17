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
	;


var
 InPkMem :array [1..1152] of byte; //MTU is 1280
 InPk :^Peers.tPacket;
var InPkLen :LongInt;

PROCEDURE ProcessPacket;
var p:pointer;
 var addrstr:string;
begin
 p:=InPk;
 Peers.SelectedAddr.ToString( addrstr );
 log.msg('Recieved #'+IntToStr(InPk^.pktype)+' ('+IntToStr(InPkLen)+'B) From '+addrstr);
 if InPk^.pktype=Peers.cAkafuka then Peers.tAkafuka(p^).Handle else
 if InPk^.pktype=Peers.cFundeluka then Peers.tFundeluka(p^).Handle else
 if InPk^.pktype=Encap.cEncap then Encap.tEncap(p^).Handle else
 Abort;
end;

PROCEDURE LoopOnSocket;
 begin
 repeat
  Peers.Reset;
  //Recieve the incoming packet
  InPk:=@InPkMem;
  InPkLen:= sizeof(InPkMem);
  Log.msg('Reading from socket');
  SocketUtil.Recv( Peers.SelectedAddr, InPk^, InPkLen );
  Peers.isSelectedAddr:=true;
  ProcessPacket;
  Log.msg('Finished processing input');
  Flush(log.F);
 until false; {TODO: timeout ... }
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
