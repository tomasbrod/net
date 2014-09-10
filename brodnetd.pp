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
	;


var
 InPkMem :array [1..1152] of byte; //MTU is 1280
 InPk :^Peers.tPacket;
 Err : TextFile;

PROCEDURE ProcessPacket;
var p:pointer;
begin
 p:=InPk;
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
 begin
 Peers.SelectedAddr.ToSocket(SockAddr);
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
 Writeln('New Peer ',ids);
end;

PROCEDURE Init;
 var cfg :TINIFile;
 begin
 DataBase.Prefix:=GetEnvironmentVariable('BRODNETD_DATA');
 if FindCmdLineSwitch('stderr') then Err:=StdErr else begin
  Assign(Err, DataBase.Prefix+DirectorySeparator+cMainLog );
  try Append(Err); except on eInOutError do Rewrite(Err); end;
 end;
 //if Length(DataBase.Prefix)=0 then Abort;
 Peers.SendProc:=@SocketSendImpl;
 Peers.NewProc:=@NewPeerHook;
 cfg := TINIFile.Create(DataBase.Prefix+DirectorySeparator+cMainCfg);
 try
  Peers.ThisID.FromString( cfg.ReadString('PEER','id','') );
 finally
  cfg.Free;
 end;
end;
 
BEGIN
 if FindCmdLineSwitch('s') then begin
  Init;
  try
   LoopOnSocket;
  except
   on e : eSocket do begin
    writeln(Err,'Socket error '+IntToStr(e.code));
    DumpExceptionBackTrace(Err);
   end;
  end;
  Close(Err);
 end else begin
  writeln('help');
 end;
END.
