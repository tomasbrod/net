unit Controll;
(* Communication between applications and running brodnet daemon. *)

INTERFACE

uses  SocketUtil
     ,NetAddr
     ,Classes
     ,BaseUnix
     ,Sockets
     ,SysUtils
     ,sSockets
     ,Eventlog
     ,Peers
     ;

type tDCFlag=(cfPeerStates,cfPeerStates0);

type tDaemonController=class (tObject)
  public
  io: tStream;
  cmd:byte;
  flags:set of tDCFlag;
  finished:boolean;
  procedure Run; unimplemented;
  constructor Create(asocket:tStream; from:NetAddr.t);
  destructor Destroy; override;
  procedure NotifyPeerStateChange( event: byte; info:Peers.tInfo );
  procedure NotifyQuit( unexcepted:boolean );
  procedure CmdAddPeer;
end;

{Hooks for main loop}
procedure SetSelectFDS(var FDS : BaseUnix.tFDSet);
procedure HandleConnect(const ls:tListeningSocket);
procedure Receive(const FDS : BaseUnix.tFDSet);

{hooks for hooks in daemon}
procedure NotifyPeerStateChange( event: byte; info:Peers.tInfo );
procedure NotifyQuit( unexcepted:boolean );

var Clients : array [1..8] of tDaemonController;
var Log:tEventLog;
var OnTerminateRequest:procedure;

type eFull=class(Exception) end;

{todo:
- Aby sa upratal trochu loop v daemone:
  - poskytne proceduru co modifikuje select-set
  - ohandluje si callback z daemona z vysledkyu zo selectu
  - Chceme abstrahovať socket, aby sa to dalo spustit aj bez daemona?
    - naco, aj tak to bez neho nejde
- manažuje si pripojenych sam
- handluje peerstatechange, terminate, a vsetko
}

IMPLEMENTATION
uses CtrlIface;

procedure tDaemonController.Run;
 begin
 try
 cmd:=io.ReadByte;
 log.debug('Command: #'+IntToStr(cmd));
 case cmd of
  13,10:;
  ccPeerStates: include(flags,cfPeerStates);
  //ccPeerStatesOff: flags:= flags -[cfPeerStates,ccPeerStates];
  ccTerminate: if assigned(OnTerminateRequest) then OnTerminateRequest;
  ccQuit: Finished:=true;
  ccAddPeer:CmdAddPeer;
 end;
 except on e:EReadError do begin
   log.debug('Command errored '+e.classname);
   finished:=true;
   exit;
 end; end;
end;

procedure tDaemonController.NotifyPeerStateChange( event: byte; info:Peers.tInfo );
 begin
 if not (cfPeerStates in flags) then exit;
 //if (event=0) and ( not (cfPeerStates0 in flags)) then exit;
 io.WriteByte(cePeerState);
 io.WriteByte(event);
 io.WriteBuffer(info,sizeof(info));
end;

procedure tDaemonController.CmdAddPeer;
 var addr:netaddr.t;
 begin
 io.ReadBuffer(addr,sizeof(addr));
 Peers.Add(addr);
end;

procedure tDaemonController.NotifyQuit( unexcepted:boolean );
 var exp:byte;
 begin
 io.WriteByte(ceQuit);
 if unexcepted then exp:=1 else exp:=0;
 io.WriteByte(exp);
end;

constructor tDaemonController.Create(asocket:tStream; from:NetAddr.t);
begin
 finished:=false;
 io:=asocket;
 flags:=[];
end;

destructor tDaemonController.Destroy;
 begin
end;

procedure SetSelectFDS(var FDS : BaseUnix.tFDSet);
 var i:word;
 begin
 for i:=1 to high(Clients) do if assigned(Clients[i]) then BaseUnix.fpfd_set((Clients[i].io as tSocketStream).Handle,FDS);
end;

procedure HandleConnect(const ls:tListeningSocket);
 var stream:tSocketStream;
 var ci:word;
 var addr:netaddr.t;
 var consocket:tSocket;
 begin
 ci:=1; while true do begin
  if ci>high(Clients) then break;
  if not assigned(Clients[ci]) then break;
  inc(ci);
 end;
 ls.Accept(consocket,addr);
 stream:=tSocketStream.Create(consocket);
 if ci>high(Clients) then begin
  log.error('Too many clients on command port'); //raise eFull.Create('Too many clients on command port');
  stream.free;
 end else begin
  log.info('Connect on CMD/#'+IntToStr(ci)+' from '+String(addr));
  Clients[ci]:=tDaemonController.Create(stream,addr);
 end;
end;

procedure Receive(const FDS : BaseUnix.tFDSet);
 var i:word;
 begin
 for i:=1 to high(Clients) do if assigned(Clients[i]) and (BaseUnix.fpfd_isset((Clients[i].io as tSocketStream).Handle,FDS)=1) then begin
  log.debug('Recieved on CMD/#'+IntToStr(i));
  Clients[i].Run;
  if Clients[i].Finished then begin
   log.info('Disconnect on CMD/#'+IntToStr(i));
   Clients[i].io.free;
   freeandnil(Clients[i]);
  end
 end;
end;

procedure NotifyPeerStateChange( event: byte; info:Peers.tInfo );
 var i:word;
 begin
 for i:=1 to high(Clients) do if assigned(Clients[i]) then Clients[i].NotifyPeerStateChange(event,info);
end;

procedure NotifyQuit( unexcepted:boolean );
 var i:word;
 begin
 for i:=1 to high(Clients) do if assigned(Clients[i]) then begin
  Clients[i].NotifyQuit(unexcepted);
  Clients[i].io.free;
  freeandnil(Clients[i]);
 end;
end;

procedure Init;
 var i:word;
 begin
 for i:=1 to high(Clients) do Clients[i]:=nil;
end;

INITIALIZATION
 Init;
END.