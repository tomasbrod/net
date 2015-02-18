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
     ,Transfer
     ,Neighb
     ;

{Hooks for main loop}
procedure SetSelectFDS(var FDS : BaseUnix.tFDSet);
procedure HandleConnect(const ls:tListeningSocket);
procedure Receive(const FDS : BaseUnix.tFDSet);

{hooks for hooks in daemon}
procedure NotifyPeerStateChange( event: byte; info:Peers.tInfo );
procedure NotifyTransfer( id :tFID; done,total:longword; by: byte );
procedure NotifyQuit( unexcepted:boolean );
procedure NotifyNeighbState( pid:tPID; addr:netaddr.t; hop:word );

var Log:tEventLog;
var OnTerminateRequest:procedure;
const cTransferBy=82;

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

type tDCFlag=(cfPeerStates,cfPeerStates0);

type tDaemonController=class (tObject)
  public
  io: tStream;
  cmd:byte;
  flags:set of tDCFlag;
  finished:boolean;
  procedure Run;
  constructor Create(asocket:tStream; from:NetAddr.t);
  destructor Destroy; override;
  procedure NotifyPeerStateChange( event: byte; info:Peers.tInfo );
  procedure CmdAddPeer;
  procedure NotifyQuit( unexcepted:boolean );
  procedure CmdInvalid;
  procedure NotifyTransfer( id :tFID; done,total:longword; by: byte );
  procedure CmdRequestTransfer;
  procedure NotifyNeighb( pid:Neighb.tPID; addr: netaddr.t; hopcount: word );
  procedure CmdGetNeighb;
  procedure CmdGetNeighbPID;
end;

var Clients : array [1..8] of tDaemonController;

procedure tDaemonController.Run;
 begin
 try
 cmd:=io.ReadByte;
 log.debug('Command: #'+IntToStr(cmd));
 case cmd of
  13,10:;
  ccPeerStates: flags:=flags><[cfPeerStates];
  //ccPeerStatesOff: flags:= flags -[cfPeerStates,ccPeerStates];
  ccTerminate: if assigned(OnTerminateRequest) then OnTerminateRequest;
  ccQuit: Finished:=true;
  ccAddPeer:CmdAddPeer;
  ccTransferRequest:CmdRequestTransfer;
  ccGetNeighb:CmdGetNeighb;
  ccGetNeighbPID:CmdGetNeighbPID;
  //ccGetNeighbAddr:CmdGetNeighbAddr;
  else CmdInvalid;
 end;
 except on e:EReadError do begin
   log.debug('Command errored '+e.classname);
   finished:=true;
   exit;
 end; end;
end;

{peer controll}
procedure tDaemonController.NotifyPeerStateChange( event: byte; info:Peers.tInfo );
 var w:netaddr.word2;
 begin
 if not (cfPeerStates in flags) then exit;
 if (event=0) and ( not (cfPeerStates0 in flags)) then exit;
 io.WriteByte(cePeerState);
 io.WriteByte(event);
 io.WriteBuffer(info.addr,sizeof(info.addr));
 w:=trunc(info.delta*MSecsPerDay);
 io.WriteBuffer(w,2);
end;
procedure tDaemonController.CmdAddPeer;
 var addr:netaddr.t;
 begin
 io.ReadBuffer(addr,sizeof(addr));
 if addr.IsNil then log.error('Addr is nil') else Peers.Add(addr);
end;

{daemon controll}
procedure tDaemonController.CmdInvalid;
 begin
 io.WriteByte(ceInvalid);
 Finished:=true;
 log.error('Invalid command');
end;
procedure tDaemonController.NotifyQuit( unexcepted:boolean );
 var exp:byte;
 begin
 io.WriteByte(ceQuit);
 if unexcepted then exp:=1 else exp:=0;
 io.WriteByte(exp);
end;

{Transfer controll}
procedure tDaemonController.NotifyTransfer( id :tFID; done,total:longword; by: byte );
 var w:netaddr.word4;
 begin
 io.WriteByte(ceTransfer);
 io.WriteBuffer(id,sizeof(id));
 w:=done; io.WriteBuffer(w,4);
 w:=total; io.WriteBuffer(w,4);
 (*io.WriteByte(by);*)
end;
procedure tDaemonController.CmdRequestTransfer;
 var a:Transfer.tFID;
 var addr:netaddr.t;
 begin
 io.ReadBuffer(a,sizeof(a));
 io.ReadBuffer(addr,sizeof(addr));
 Transfer.RequestFile( addr, a, cTransferBy );
end;

procedure tDaemonController.NotifyNeighb( pid:Neighb.tPID; addr: netaddr.t; hopcount: word );
 var n:tNeighbInfo;
 begin
 if not (cfPeerStates in flags) then exit;
 n.pid:=pid;
 n.addr:=addr;
 n.hop:=hopcount;
 io.WriteByte(ceNeighbState);
 io.WriteBuffer(n,sizeof(n));
end;
procedure NotifyNeighbState( pid:tPID; addr:netaddr.t; hop:word );
 var i:word;
 begin
 for i:=1 to high(Clients) do if assigned(Clients[i]) then Clients[i].NotifyNeighb(pid,addr,hop);
end;

procedure tDaemonController.CmdGetNeighb;
 var n:tNeighbInfo;
 var i:Neighb.tInfo;
 begin
 i.Init(65535);
 while i.Next do begin
  n.pid:=i.pid;
  n.addr:=i.addr;
  n.hop:=i.hop;
  io.WriteByte(ceNeighbs);
  io.WriteBuffer(n,sizeof(n));
 end;
 io.WriteByte(ceNeighbsEnd);
end;

procedure tDaemonController.CmdGetNeighbPID;
 var n:tNeighbInfo;
 var i:pointer=nil;
 var pid:Neighb.tPID;
 begin
 io.ReadBuffer(pid,sizeof(pid));
 n.pid:=pid;
 n.hop:=65535;
 Neighb.GetRoute(pid,n.addr,i);
 while assigned(i) do begin
  io.WriteByte(ceNeighbs);
  io.WriteBuffer(n,sizeof(n));
  Neighb.GetRoute(pid,n.addr,i);
 end;
 io.WriteByte(ceNeighbsEnd);
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

procedure NotifyTransfer( id :tFID; done,total:longword; by: byte );
 var i:word;
 begin
 for i:=1 to high(Clients) do if assigned(Clients[i]) then Clients[i].NotifyTransfer(id,done,total,by);
end;

procedure Init;
 var i:word;
 begin
 for i:=1 to high(Clients) do Clients[i]:=nil;
end;

INITIALIZATION
 Init;
END.