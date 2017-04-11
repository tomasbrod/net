unit RPCsrv;
(** Inter process procedure call server **)

INTERFACE
uses Classes,fgl,BaseUnix,Sockets,ObjectModel,ServerLoop,Crypto;

type
TRPCDelegate = class;

TRPCCon = class
  socket:tSocket;
  ArgsSize,ArgsRcvd: LongWord;
  a, r: tMemoryStream;
  iphash:tKey32;
  delegate:TRPCDelegate;
  procedure Create2;
  destructor Destroy; override;
  procedure Event(ev:word);
  procedure SendTo(msg:tMemoryStream);
  procedure Reset;
end;

TRPCDelegate = class
  con: TRPCCon;
  {procedure OnInput; virtual; abstract;
  procedure OnOutput; virtual;
  procedure WantOutput(enable:boolean);
  constructor Create( icon: TRPCCon );
  destructor Destroy; override;}
end;

TRemoteProc = procedure( con: tRPCCon );
TRemoteProcOpcode = Word;

procedure SetupRemoteProc( opcode: TRemoteProcOpcode; handler: tRemoteProc );

const
  cRPCSuccess=0;
  cRPCUnknown=2; {unknown opcode}
  cRPCInvalid=3; {error in arguments}
  cRPCBegan=4; {delegated process began}

type ERPCInvalid=class(Exception)
  //constructor create;
end;

IMPLEMENTATION

type tRPCServer = object
  listen:tSocket;
  prev_on_terminate:procedure;
  procedure Init;
  procedure ListenEvent(ev:word);
  procedure InitUnix;
end;

const cSocketName='ctrl';

var Server1: tRPCServer;
var Log: tEventLog;
var rpctable:TFPSMap;

procedure tRPCServer.InitUnix;
  var addr:Sockets.sockaddr_un;
  begin
  addr.sun_family:=AF_UNIX;
  addr.sun_path:=cSocketName;
  fpUnlink(@addr.sun_path);
  listen:=fpSocket(addr.sun_family,SOCK_STREAM,0);
  SC(@fpSocket,listen);
  SC(@fpBind,fpBind(listen,@addr,sizeof(addr)));
end;

procedure OnTerminate;
  begin
  fpUnlink(cSocketName);
  if assigned(Server1.prev_on_terminate) then Server1.prev_on_terminate;
end;

procedure tRPCServer.Init;
  begin
  InitUnix;
  SC(@fpListen,fpListen(listen,8));
  ServerLoop.WatchFD(listen,@ListenEvent);
  prev_on_terminate:=ServerLoop.OnTerminate;
  ServerLoop.OnTerminate:=@OnTerminate;
end;

procedure tRPCServer.ListenEvent(ev:word);
  var addr:Sockets.sockaddr_un;
  var addrl:tSockLen;
  var s:tSocket;
  var cl:TRPCCon;
  begin
  addrl:=sizeof(addr);
  s:=fpAccept(listen,@addr,@addrl);
  if s<0 then begin
    log.error(' accept failed',[]);
  exit end;
  cl:=TRPCCon.Create;
  SHA256_Buffer(cl.iphash,32,addr,addrl);
  cl.socket:=s;
  //log.debug(' accepted from iphash=%S',[string(cl.iphash)]);
  cl.Create2;
end;

type tMemoryStreamEX=class(tMemoryStream)
  function Read(var Buffer; Count: LongInt): LongInt; override;
end;

function tMemoryStreamEX.Read(var Buffer; Count: LongInt): LongInt;
  begin
  result:=inherited Read(buffer,count);
  if result<count then ERPCInvalid.Create('Not enough arguments');
end;

procedure tRPCCon.Create2;
  begin
  delegate:=nil;
  a:=tMemoryStream.Create;
  r:=tMemoryStream.Create;
  Reset;
  assert(socket>=0);
  ServerLoop.WatchFD(socket,@Event);
end;
procedure tRPCCon.Reset;
  begin
  delegate:=nil;
  a.Size:=4096;
  ArgsSize:=2;
  ArgsRcvd:=0;
  r.Clear;
  r.W2(0);
end;
destructor tRPCCon.Destroy;
  begin
  delegate.Free;
  a.Free;
  r.Free;
  ServerLoop.WatchFD(socket,nil);
end;

procedure tRPCCon.Event(ev:word);
  var rd:LongInt;
  var ix:integer;
  var opcode:TRemoteProcOpcode;
  label error;
  begin
  //log.debug('.Client(%P).Event %D',[@self,ev]);
  if assigned(delegate) then begin
    {if (ev and POLLIN)>0 then delegate.OnInput;
    if (ev and POLLOUT)>0 then delegate.OnOutput;}
    assert(false);
  end else if (ev and POLLIN)>0 then begin
    rd:=fpRecv(socket,a.Memory+ArgsRcvd,ArgsSize-ArgsRcvd,0);
    if rd<0 then begin
      log.Error('.Client(%P).Event fpRecv failed %D',[@self,SocketError]);
      goto error;
    end else if rd=0 then goto error;
    ArgsRcvd:=ArgsRcvd+rd;
    if ArgsRcvd=2 then begin
      a.Position:=0;
      ArgsSize:=2+a.R2;
    end;
    if ArgsRcvd>=ArgsSize then begin
      if ArgsSize<4 then begin
        log.Error('.Client(%P).Event invalid rpc packet length',[@self]);
        goto error;
      end;
      a.Size:=ArgsSize;
      a.Position:=2;
      opcode:=a.R2;
      ix:=rpctable.IndexOf(@opcode);
      if ix>-1 then try
        TRemoteProc(rpctable.data[ix]^) ( self )
      except
        on ERPCInvalid do r.WriteByte(cRPCInvalid);
      end else r.W2(cRPCUnknown);
      r.Position:=0;
      r.W2(r.Size-2);
      self.SendTo(r);
      self.Reset;
    end;
  end;
  Exit;
  error:
    Self.Free;
    //log.Debug('.Client(%P).Event destroy',[@self]);
    Exit;
end;

procedure SetupRemoteProc( opcode: TRemoteProcOpcode; handler: tRemoteProc );
  begin
  //log.debug(' register RPC OpCode %D',[opcode]);
  rpctable.Add(@opcode,@handler);
end;

procedure tRPCCon.SendTo(msg:tMemoryStream);
  var re,tw:LongInt;
  var pt:pointer;
  begin
  try
  tw:=msg.Size;
  pt:=msg.Memory;
  while tw>0 do begin
    re:=fpSend(socket,pt,tw,0);
    if re<0 then SC(@fpSend,re-1);
    tw:=tw-re;
    pt:=pt+re;
  end;
  except {ignore exceptions}
  end;
end;

BEGIN
  ServerLoop.CreateLog(log,'RPCsrv');
  Server1.Init;
  rpctable:=TFPSMap.Create(sizeof(TRemoteProcOpcode),sizeof(TRemoteProc));
  rpctable.sorted:=true;
  rpctable.duplicates:=dupError;
END.

{*** Old Code ***}
{***Low-Level Part***}

 if (ev and POLLOUT)>0 then begin
    {$ifdef ctlStore}
    if SndObjLeft=0 then begin
      {$endif}
      ServerLoop.WatchFD(s,nil);
      ServerLoop.WatchFD(s,@Event);
      {$ifdef ctlStore}
      Dispose(SndObj,Done);
      SndObj:=nil;
    end else begin
      arg:=GetMem(cBuf);
      rc:=SndObjLeft;
      if rc>cBuf then rc:=cBuf;
      SndObj^.Read(arg^,rc);
      sndc:=fpSend(s,arg,rc,0);
      if sndc<=0 then error:=true
      else if sndc<rc
      then SndObj^.Skip(sndc-rc);
      SndObjLeft:=SndObjLeft-sndc;
      FreeMem(arg,cBuf);
    end;
    {$endif}
 end;
  if (ev and (POLLIN or POLLHUP or POLLERR))>0 then begin
    {$ifdef ctlStore}
    if RcvObjLeft>0 then begin
      arg:=GetMem(cBuf);
      rc:=cBuf; if rc>RcvObjLeft then rc:=RcvObjLeft;
      rc:=fpRecv(s,arg,rc,0);
      if rc>0 then begin
        RcvObj^.Write(arg^,rc);
        RcvObjHctx.Update(arg^,rc);
        RcvObjLeft:=RcvObjLeft-rc;
        if RcvObjLeft=0 then RcvObjComplete;
        rc:=1;
      end;
    end else begin
    {$endif}


{$ifdef ctlStore}
procedure tClient.SendObject(var o:tStoreObject; ilen:LongWord);
  begin
  SndObjLeft:=ilen;
  SndObj:=@o;
  ServerLoop.WatchFD(s,nil);
  ServerLoop.WatchFDRW(s,@Event);
end;
{$endif}
