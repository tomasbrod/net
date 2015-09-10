UNIT ServerLoop;

INTERFACE
uses MemStream,NetAddr,UnixType;

procedure Main;

{#Message handling#}
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

procedure SendMessage(const data; len:word; const rcpt:tNetAddr );
{procedure SendReply(const data; len:word; const rcpt:tSMsg );}
procedure SendMessage(const data; len:word; const rcpt:tNetAddr; channel:word );

{#Sheduling and watching#}
type tFDEventHandler=procedure(ev:Word) of object;
type tOnTimer=procedure of object;
procedure WatchFD(fd:tHandle; h:tFDEventHandler);
procedure Shedule(timeout{ms}: LongWord; h:tOnTimer);
procedure UnShedule(h:tOnTimer);
 {note unshed will fail when called from OnTimer proc}

type tTimeVal=UnixType.timeval;
var iNow:tTimeVal;

IMPLEMENTATION

USES SysUtils,Sockets,BaseUnix
     ,Unix
     ;

{aim for most simple implementation, since could be extended anytime}

var s_inet:tSocket;

type tPollTop=0..7;
var pollArr: packed array [tPollTop] of tPollFd;
type tFdHndDsc=record
 cb: tFDEventHandler; {proc+object}
 end;
var pollHnd: array [tPollTop] of tFdHndDsc;
var pollTop: tPollTop;

var hnd: array [1..36] of tMessageHandler;
var HiHnd: tMessageHandler;

type tSheduled_ptr=^tSheduled; tSheduled=record
 left:LongWord;
 cb:tOnTimer;
 next:tSheduled_ptr;
 end;
var ShedTop: ^tSheduled;
var ShedUU: ^tSheduled;
var LastShed: UnixType.timeval;
var PollTimeout:LongInt;

procedure SC(fn:pointer; retval:cint);
 begin
  if retval < 0 then begin
   raise eXception.Create(Format('Socket error %d operation %P',[SocketError,fn]));
  end;
 end;

procedure s_SetupInet;
 var bind_addr:tInetSockAddr;
 var turnon:cint;
 begin
  with bind_addr do begin
   family:=AF_INET;
   port:=htons(3511);
   addr:=0; {any}
   s_inet:=fpSocket(family,SOCK_DGRAM,IPPROTO_UDP);
   SC(@fpSocket,s_inet);
   turnon:=IP_PMTUDISC_DO;
   SC(@fpsetsockopt,fpsetsockopt(s_inet, IPPROTO_IP, IP_MTU_DISCOVER, @turnon, sizeof(turnon)));
  end;
  SC(@fpBind,fpBind(s_inet,@bind_addr,sizeof(bind_addr)));
  with PollArr[0] do begin
   fd:=s_inet;
   events:=pollIN;
   revents:=0;
  end;
 end;

var Terminated:boolean=false;

procedure SendMessage(const data; len:word; const rcpt:tSockAddrL );
 var rc:Integer;
 begin
 {SC(@fpsendto,}fpsendto(s_inet,@data,len,0,@rcpt,sizeof(sockaddr_in)){)};
end;
procedure SendMessage(const data; len:word; const rcpt:tNetAddr );
 var sa:tSockAddrL;
 begin
 rcpt.ToSocket(sa);
 SendMessage(data,len,sa);
end;
procedure SendMessage(const data; len:word; const rcpt:tNetAddr; channel:word );
 begin
 SendMessage(data,len,rcpt);
 {todo: optimization??}
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

procedure ShedRun;
 var cur:^tSheduled;
 var pcur:^pointer;
 var now:UnixType.timeval absolute iNow;
 var delta:LongWord;
 var olTop:^tSheduled;
 begin
 {Sheduling}
 olTop:=ShedTop;
 pcur:=@olTop;
 cur:=pcur^;
 ShedTop:=nil; {unlink the current shed list}
 fpgettimeofday(@Now,nil);
 delta:=(Now.tv_sec-LastShed.tv_sec);
 if delta>6 then delta:=5000 else delta:=(delta*1000)+((Now.tv_usec-LastShed.tv_usec) div 1000);
 LastShed:=Now;
 //writeln('DeltaTime: ',delta);
 while assigned(cur) do begin
  if (cur^.left<=delta)or(cur^.left=0) then begin
   cur^.cb;
   pcur^:=cur^.next;
   cur^.next:=ShedUU;
   ShedUU:=cur;
   cur:=pcur^;
  end else begin
   DEC(cur^.left,delta);
   //writeln('Left: ',cur^.left);
   if pollTimeOut>cur^.left then PollTimeOut:=cur^.left;
   pcur:=@cur^.next;
   cur:=cur^.next;
  end;
 end;
 pcur^:=ShedTop; {append newly added tasks to end of untriggererd list}
 ShedTop:=olTop; {link in the untriggered tasks}
 cur:=olTop;
 while assigned(cur) do begin
  if cur^.left<PollTimeout then PollTimeout:=cur^.left;
  cur:=cur^.next;
 end;
 if pollTimeout=0 then pollTimeOut:=1;
end;

procedure Main;
 begin
 s_setupInet;
 while not terminated  do begin
  PollTimeout:=5000;
  ShedRun;
  EventsCount:=fpPoll(@PollArr[0],PollTop,PollTimeout);
  if (eventscount=-1)and terminated then break;
  if eventscount=-1 then break;  {fixme: print error}
  if eventscount=0 then continue else begin
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
begin assert(hnd[OpCode]=nil); hnd[OpCode]:=handler; end;
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

procedure Shedule(timeout{ms}: LongWord; h:tOnTimer);
 var old:^tSheduled;
 begin
 old:=ShedTop;
 if Assigned(ShedUU) then begin
  ShedTop:=ShedUU;
  ShedUU:=ShedUU^.next;
 end else New(ShedTop);
 ShedTop^.Left:=timeout;
 ShedTop^.CB:=h;
 ShedTop^.Next:=old;
end;

procedure UnShedule(h:tOnTimer);
 var cur:^tSheduled;
 var pcur:^pointer;
 begin
 pcur:=@ShedTop;
 cur:=pcur^;
 while assigned(cur) do begin
  if cur^.cb=h then begin
   pcur^:=cur^.next; {unlink from main list}
   cur^.next:=ShedUU; ShedUU:=cur; {link to unused}
   break;
  end else begin
   pcur:=@cur^.next;
   cur:=pcur^;
  end;
 end;
end;

var i:byte;
BEGIN
 Randomize;
 fpSignal(SigInt,@SignalHandler);
 fpSignal(SigTerm,@SignalHandler);
 for i:=1 to high(hnd) do hnd[i]:=nil;
 pollTop:=1; {1 for basic listen}
 ShedTop:=nil;
 ShedUU:=nil; {todo: allocate a few to improve paging}
 fpgettimeofday(@LastShed,nil);
END.