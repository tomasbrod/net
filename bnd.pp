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
	;


var
 InPkMem :array [1..1152] of byte; //MTU is 1280
 InPk :^Peers.tPacket;

PROCEDURE ProcessPacket;
var p:pointer;
 var addrstr:string;
begin
 p:=InPk;
 Peers.SelectedAddr.ToString( addrstr );
 log.msg('Recieved #'+IntToStr(InPk^.pktype)+' From '+addrstr);
 if InPk^.pktype=Peers.cAkafuka then Peers.tAkafuka(p^).Handle else
 if InPk^.pktype=Peers.cFundeluka then Peers.tFundeluka(p^).Handle else
 Abort;
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
  SockAddrLen:=sizeof(sockaddr); //does  not work without this line
  InPkLen:= 
     fpRecvFrom(
     {Socket} sock,
     {packet^} InPk,
     {maxlin} sizeof(InPkMem),
     {flags} 0,
     {addr^} @SockAddr,
     {addrl^} @SockAddrLen
  );
  log.msg('Received '+inttostr(InPkLen)+'B message');
  if InPkLen<0 then CheckSocket;
  Peers.isSelectedAddr:=true;
  Peers.SelectedAddr.FromSocket( SockAddr );
  ProcessPacket;
 until true; {TODO: timeout ... }
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
