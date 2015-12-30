UNIT ServerLoop;

INTERFACE
uses MemStream,NetAddr,UnixType,Sockets;

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

function GetSocket(const rcpt:tNetAddr):tSocket;
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

type tObjMessageHandler=procedure(msg:tSMsg) of object;
{deliver message from peer to the object}
procedure SetMsgHandler(OpCode:byte; from:tNetAddr; handler:tObjMessageHandler); overload;
function IsMsgHandled(OpCode:byte; from:tNetAddr):boolean;

function OptIndex(o:string):word;
function OptParamCount(o:word):word;

var OnTerminate:procedure;

type tTimeVal=UnixType.timeval;
type tMTime=DWORD;
var iNow:tTimeVal;
var mNow:tMTime; { miliseconds since start }
                  {overflows in hunderd hours }

IMPLEMENTATION

USES SysUtils,BaseUnix
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
var umNow:integer;

procedure SC(fn:pointer; retval:cint);
 begin
  if retval < 0 then begin
   raise eXception.Create(Format('Socket error %d operation %P',[SocketError,fn]));
  end;
 end;

procedure s_SetupInet;
 var bind_addr:tInetSockAddr;
 var turnon:cint;
 var oi:word;
 begin
  with bind_addr do begin
   sin_family:=AF_INET;
   oi:=OptIndex('-port');
   if oi=0 then sin_port:=htons(3511)
   else begin
    assert(OptParamCount(oi)=1);
    sin_port:=htons(StrToInt(paramstr(oi+1)));
   end;
   sin_addr.s_addr:=0; {any}
   s_inet:=fpSocket(sin_family,SOCK_DGRAM,IPPROTO_UDP);
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

function GetSocket(const rcpt:tNetAddr):tSocket;
 begin
 result:=s_inet;
end;
procedure SendMessage(const data; len:word; const rcpt:tSockAddrL );
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
 end;

{index=iphash+opcode}
type tPeerTableBucket=record
 opcode:byte;
 remote:tNetAddr;
 handler:tObjMessageHandler;
end;
var PT:array [0..255] of ^tPeerTableBucket;
var PT_opcodes: set of 1..high(hnd);

function FindPT(opcode:byte; addr:tNetAddr):Word; { $FFFF=fail}
 var i,o:word;
 begin
 i:=(addr.hash+opcode) mod high(PT); {0..63}
 for o:=0 to high(PT) do begin
  result:=(i+o) mod high(PT);
  if not assigned(PT[result]) then break;
  if (PT[result]^.opcode=opcode) and (PT[result]^.remote=addr) then exit;
 end;
 result:=$FFFF;
end;

function IsMsgHandled(OpCode:byte; from:tNetAddr):boolean;
 begin result:=FindPT(opcode,from)<>$FFFF end;

procedure UnSetMsgHandler(const from:tNetAddr; opcode:byte);
 var i,h:word;
 begin
 h:=FindPT(opcode,from);
 if h=$FFFF then exit;
 Dispose(PT[h]);
 PT[h]:=nil;
 {go reverse exit on null, hash them, match: move to H and stop}
 if h=0 then i:=high(PT) else i:=h-1;
 while (i<>h)and assigned(PT[i]) do begin
  if (PT[i]^.remote.hash+PT[i]^.opcode)=h then begin
   PT[h]:=PT[i];
   PT[i]:=nil;
   break;
  end;
  if i=0 then i:=high(PT) else dec(i);
 end;
end;

procedure SetMsgHandler(OpCode:byte; from:tNetAddr; handler:tObjMessageHandler);
 var h,o,i:word;
 begin
 UnSetMsgHandler(from,opcode);
 if handler=nil then exit;
 h:=(from.hash+opcode) mod high(PT);
 for o:=0 to high(PT) do begin
  i:=(h+o) mod high(PT);
  if not assigned(PT[i]) then break;
 end;
 New(PT[i]);
 PT[i]^.opcode:=OpCode;
 PT[i]^.remote:=from;
 PT[i]^.handler:=handler;
 if opcode<=high(hnd) then Include(PT_opcodes,opcode);
end;

{do not waste stack on statics}
 var EventsCount:integer;
 var Buffer:array [1..4096] of byte;
 var pkLen:LongWord;
 var From:tSockAddrL; {use larger struct so everything fits}
 var FromLen:LongWord;
 var FromG:tNetAddr;
 var curhnd:tMessageHandler;
 var curhndo:tObjMessageHandler;
 var Msg:tSMsg;
 var tp:tPollTop;

function DoSock(var p:tPollFD):boolean;
 var ptidx:word;
 begin
 curhnd:=nil;
 curhndo:=nil;
 result:=false;
 ptidx:=$FFFF;
 if (p.revents and pollIN)=0 then exit else  result:=true;
 FromLen:=sizeof(From);
 pkLen:=fprecvfrom(p.FD,@Buffer,sizeof(Buffer),0,@from,@fromlen);
 SC(@fprecvfrom,pkLen);
 p.revents:=0;
 FromG.FromSocket(from);
 Msg.Source:=@FromG; {!thread}
 Msg.Length:=pkLen;
 Msg.Data:=@Buffer; {!thread}
 Msg.stream.Init(@Buffer,pkLen,sizeof(Buffer));
 Msg.channel:=0; {!multisocket}
 if Buffer[1]>=128 then curhnd:=HiHnd else if Buffer[1]<=high(hnd) then curhnd:=hnd[Buffer[1]];
 if (Buffer[1]>high(hnd))or(Buffer[1] in PT_opcodes) then begin
  ptidx:=FindPT(Buffer[1],FromG);
  if ptidx<$FFFF then curhndo:=PT[ptidx]^.handler;
 end;
end;

procedure ShedRun;
 var cur:^tSheduled;
 var pcur:^pointer;
 var now:UnixType.timeval{ absolute iNow};
 var delta:LongWord;
 var delta_us:LongInt;
 var tasks:word;
 begin
 {Sheduling}
 {gmagic with delta-time, increment mNow, ...}
 fpgettimeofday(@Now,nil);
 delta:=(Now.tv_sec-LastShed.tv_sec);
 delta_us:=Now.tv_usec-LastShed.tv_usec;
 delta:=(delta*1000)+(delta_us div 1000);
 umNow:=umNow+(delta_us mod 1000);
 if delta>6000 then delta:=5000;
 LastShed:=Now;
 mNow:=mNow+Delta;
 if umNow>1000 then begin inc(mNow); dec(umNow,1000) end;
 if umNow<-1000 then begin dec(mNow); inc(umNow,1000) end;
 //writeln('DeltaTime: ',delta);
 {first tick all tasks}
 tasks:=0;
 cur:=ShedTop;
 while assigned(cur) do begin
  if cur^.left<=delta then cur^.left:=0 else begin
   dec(cur^.left,delta);
   {also set next wake time}
   if cur^.left<PollTimeout then PollTimeout:=cur^.left;
  end;
  {count tasks here}
  inc(tasks);
  cur:=cur^.next;
 end;
 {correct floating-point glitch}
 if pollTimeout=0 then pollTimeOut:=1;
 {run first runnable task}
 pcur:=@ShedTop;
 cur:=pcur^;
 while assigned(cur) do begin
  if cur^.left=0 then begin
   {unlink}
   pcur^:=cur^.next;
   {link to unused}
   cur^.next:=ShedUU;
   ShedUU:=cur;
   {call}
   cur^.cb;
   {do rest later}
   pollTimeout:=0;
   break;
  end;
  pcur:=@cur^.next;
  cur:=cur^.next;
 end;
end;

procedure Main;
 begin
 s_setupInet;
 while not terminated  do begin
  PollTimeout:=5000;
  ShedRun;
  EventsCount:=fpPoll(@PollArr[0],PollTop,PollTimeout);
  ShedRun;
  if (eventscount=-1)and terminated then break;
  if eventscount=-1 then break;  {fixme: print error}
  if eventscount=0 then continue else begin
   {INET socket}
   if DoSock(PollArr[0]) then
   if assigned(curhndo) then curhndo(msg)
   else if assigned(curhnd) then curhnd(msg)
   else {raise eXception.Create('}writeln('ServerLoop: No handler for opcode '+IntToStr(Buffer[1]));
   {INET6...}
   {Generic}
   for tp:=1 to pollTop do if PollArr[tp].revents>0 then begin
    PollHnd[tp].CB(PollArr[tp].rEvents);
    PollArr[tp].revents:=0;
   end;
  end;
 end;
 if assigned(onTerminate) then onTerminate;
 CloseSocket(s_inet);
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
 //if ShedTop=nil then AbstractError;
 pcur:=@ShedTop;
 cur:=pcur^;
 while assigned(cur) do begin
  if 0=CompareByte(cur^.cb,h,sizeof(h)) then begin
   pcur^:=cur^.next; {unlink from main list}
   cur^.next:=ShedUU; ShedUU:=cur; {link to unused}
   cur:=pcur^;
  end else begin
   pcur:=@cur^.next;
   cur:=pcur^;
  end;
 end;
end;

var DoShowOpts:boolean=false;
function OptIndex(o:string):word;
 begin
 if DoShowOpts then writeln('Option: ',o);
 result:=paramcount;
 while result>0 do begin
  if o=system.paramstr(result) then break;
  dec(result);
 end;
end;

function OptParamCount(o:word):word;
 var i:word;
 begin
 result:=0;
 if o>0 then for i:=o+1 to paramcount do begin
  if paramstr(i)[1]<>'-' then inc(result)
  else break;
 end;
end;

var i:byte;
var nb:array [0..0] of byte;
BEGIN
 writeln('ServerLoop: BrodNetD');
 mNow:=0;
 umNow:=0;
 Randomize;
 fpSignal(SigInt,@SignalHandler);
 fpSignal(SigTerm,@SignalHandler);
 for i:=1 to high(hnd) do hnd[i]:=nil;
 for i:=1 to high(PT) do PT[i]:=nil;
 PT_opcodes:=[];
 pollTop:=1; {1 for basic listen}
 ShedTop:=nil;
 ShedUU:=nil; {todo: allocate a few to improve paging}
 fpgettimeofday(@LastShed,nil);
 if OptIndex('-h')>0 then DoShowOpts:=true;
 OnTerminate:=nil;
 Flush(OUTPUT);
 SetTextBuf(OUTPUT,nb);
END.
