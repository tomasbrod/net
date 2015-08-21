UNIT ServerLoop;

INTERFACE
uses MemStream,NetAddr;

procedure Main;

type tSMsg=object
 Source: ^tNetAddr;
 Length: {Long}Word;
 Data: pointer;
 stream: tMemoryStream;
 channel: word;
 end;

type tMessageHandler=procedure(msg:tSMsg);

procedure SetMsgHandler(OpCode:byte; handler:tMessageHandler);
procedure SetHiMsgHandler(handler:tMessageHandler);

type tFDEventHandler=procedure(ev:Word) of object;
procedure WatchFD(fd:tHandle; h:tFDEventHandler);

IMPLEMENTATION

USES SysUtils,Sockets,UnixType,BaseUnix
     ;

{aim for most simple implementation, since could be extended anytime}

type tPollTop=0..7;
var s_inet:tSocket;
var pollArr: packed array [tPollTop] of tPollFd;
var hnd: array [1..36] of tMessageHandler;
var HiHnd: tMessageHandler;
type tFdHndDsc=record
 cb: tFDEventHandler; {proc+object}
 end;
var pollHnd: array [tPollTop] of tFdHndDsc;
var pollTop: tPollTop;

procedure IdleStuff;
begin write('.'); end;

procedure SC(fn:pointer; retval:cint);
 begin
  if retval < 0 then begin
   raise eXception.Create(Format('Socket error %d operation %P',[SocketError,fn]));
  end;
 end;

procedure s_SetupInet;
 var bind_addr:tInetSockAddr;
 begin
  with bind_addr do begin
   family:=AF_INET;
   port:=htons(3511);
   addr:=0; {any}
   s_inet:=fpSocket(family,SOCK_DGRAM,IPPROTO_UDP);
   SC(@fpSocket,s_inet);
  end;
  SC(@fpBind,fpBind(s_inet,@bind_addr,sizeof(bind_addr)));
  with PollArr[0] do begin
   fd:=s_inet;
   events:=pollIN;
   revents:=0;
  end;
 end;

var Terminated:boolean=false;

procedure SendMessage(const data; len:word; const rcpt:tSockAddr );
 var rc:Integer;
 begin
 SC(@fpsendto,fpsendto(s_inet,@data,len,0,@rcpt,sizeof(sockaddr_in)));
end;

procedure SignalHandler(sig:cint);CDecl;
 begin
  writeln;
  if terminated then raise eControlC.Create('CtrlC DoubleTap') ;
  Terminated:=true;
  writeln('Shutdown requested');
 end;

{do not waste stack on statics}
 var EventsCount:integer;
 var Buffer:array [1..1024] of byte;
 var pkLen:LongWord;
 var From:tSockAddrL; {use larger struct so everything fits}
 var FromLen:LongWord;
 var FromG:tNetAddr;
 var curhnd:tMessageHandler;
 var Msg:tSMsg;
 var tp:tPollTop;

procedure PrepareHandler;
 begin
 FromG.FromSocket(from);
 if Buffer[1]>128 then curhnd:=HiHnd else curhnd:=hnd[Buffer[1]];
 if not assigned(curhnd) then raise eXception.Create('No handler for opcode '+IntToStr(Buffer[1]));
 Msg.Source:=@FromG; {!thread}
 Msg.Length:=pkLen;
 Msg.Data:=@Buffer; {!thread}
 Msg.stream.Init(@Buffer,pkLen,sizeof(Buffer));
 Msg.channel:=0; {!multisocket}
end;
 
procedure Main;
 begin
 s_setupInet;
 while not terminated  do begin
  EventsCount:=fpPoll(@PollArr[0],PollTop,5000);
  if (eventscount=-1)and terminated then break;
  if eventscount=-1 then break;  {fixme: print error}
  if eventscount=0 then IdleStuff else begin
   {INET socket}
   with PollArr[0] do begin
    if (revents and pollIN)>0 then begin
     FromLen:=sizeof(From);
     pkLen:=fprecvfrom(s_inet,@Buffer,sizeof(Buffer),0,@from,@fromlen);
     SC(@fprecvfrom,pkLen);
     PrepareHandler;
     curhnd(Msg);
     revents:=0;
    end;
   end;
   {INET6...}
   {Generic}
   for tp:=1 to pollTop do if PollArr[tp].revents>0 then begin
    PollHnd[tp].CB(PollArr[tp].rEvents);
    PollArr[tp].revents:=0;
   end;
  end;
 end;
 write('Loop broken [');
 CloseSocket(s_inet);
 writeln(']');
end;

procedure SetMsgHandler(OpCode:byte; handler:tMessageHandler);
begin hnd[OpCode]:=handler; end;
procedure SetHiMsgHandler(handler:tMessageHandler);
begin Hihnd:=handler; end;

procedure WatchFD(fd:tHandle; h:tFDEventHandler);
 var opt: tPollTop;
begin
 if assigned(h) then begin
  PollHnd[pollTop].CB:=h;
  PollArr[pollTop].fd:=fd;
  PollArr[pollTop].events:=POLLERR or POLLHUP or POLLIN or POLLPRI or 
  POLLRDBAND or POLLRDNORM;
  PollArr[pollTop].revents:=0;
  //writeln('Add watch ',pollTop,' on ',fd,' to ',IntToHex(qword(@h),8));
  Inc(PollTop);
 end else for opt:=0 to high(opt) do if PollArr[opt].fd=fd then begin
  if (pollTop-1)>opt then begin
   PollArr[opt]:=PollArr[pollTop-1];
   PollHnd[opt]:=PollHnd[pollTop-1];
  end;
  dec(pollTop);
  PollArr[pollTop].fd:=-1;
  PollArr[pollTop].events:=0;
  PollArr[pollTop].revents:=0;
  break;
 end;
end;

var i:byte;
BEGIN
 fpSignal(SigInt,@SignalHandler);
 fpSignal(SigTerm,@SignalHandler);
 for i:=1 to high(hnd) do hnd[i]:=nil;
 pollTop:=1; {1 for basic listen}
END.