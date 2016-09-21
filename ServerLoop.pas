UNIT ServerLoop;

INTERFACE
uses ObjectModel,UnixType,Sockets;

procedure Main;
procedure RequestTerminate(c:byte);

{#Message handling#}
type tSMsg=object
 Source: tNetAddr;
 Length: {Long}Word;
 Data: pointer;
 st: tMemoryStream;
 channel: word;
 ttl: word;
 OP: byte;
 end;
type tMessageHandler=procedure(msg:tSMsg);
type tObjMessageHandler=procedure(msg:tSMsg) of object;
procedure SetupOpcode(OpCode:byte; handler:tMessageHandler);
procedure NewMsgTr(out ID:Word; handler:tObjMessageHandler);
procedure SetMsgTr(ID:Word; handler:tObjMessageHandler);
//procedure DelMsgTr(ID:Word);

function GetSocket(const rcpt:tNetAddr):tSocket;
procedure SendMessage(const data; len:word; const rcpt:tNetAddr );
{procedure SendReply(const data; len:word; const rcpt:tSMsg );}
procedure SendMessage(const data; len:word; const rcpt:tNetAddr; channel:word );

{#Sheduling and watching#}
type tFDEventHandler=procedure(ev:Word) of object;
type tOnTimer=procedure of object;
procedure WatchFD(fd:tHandle; h:tFDEventHandler);
procedure WatchFDRW(fd:tHandle; h:tFDEventHandler);
procedure Shedule(timeout{ms}: LongWord; h:tOnTimer);
procedure UnShedule(h:tOnTimer);
 {note unshed will fail when called from OnTimer proc}

function OptIndex(o:string):word;
function OptParamCount(o:word):word;

var OnTerminate:procedure;

type tTimeVal=UnixType.timeval;
type tMTime=DWORD;
var iNow:tTimeVal;
var mNow:tMTime; { miliseconds since start }
                  {overflows in hunderd hours }
function GetMTime:tMTime;
procedure SetThreadName(name:pchar);
procedure SC(fn:pointer; retval:cint);

IMPLEMENTATION

USES SysUtils,BaseUnix
     ,Unix
     ,Linux
     ,gitver
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

type tOpDesc=record
  hnd: tMessageHandler;
end;
var OpDesc: array [1..64] of tOpDesc;
type tTrDesc=record
  hnd: tObjMessageHandler;
end;
var TrDesc: array [0..223] of tTrDesc;
{will be dynamic size later :) }

type tSheduled_ptr=^tSheduled; tSheduled=record
 left:LongWord;
 cb:tOnTimer;
 next:tSheduled_ptr;
 end;
var ShedTop: ^tSheduled;
var ShedUU: ^tSheduled;
var LastShed: tMTime;
var PollTimeout:LongInt;
var Buffer:array [1..4096] of byte;

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
procedure FatalSignalHandler(sig:cint);CDecl;
  begin
  raise eExternal.Create('Unexpected Signal '+IntToStr(sig)) ;
  Terminated:=true;
end;

(*** Extended message delegation ***)

procedure SetupOpcode(OpCode:byte; handler:tMessageHandler);
  begin
  OpDesc[OpCode].hnd:=handler;
end;

procedure NewMsgTr(out ID:Word; handler:tObjMessageHandler);
  var i:integer;
  begin
  for i:=0 to high(TrDesc) do begin
    if assigned(TrDesc[i].hnd) then continue;
    TrDesc[i].hnd:=handler;
    ID:=i;
    exit;
  end;
  raise ERangeError.Create('Too many Transactions (MsgTr)');
end;

procedure SetMsgTr(ID:Word; handler:tObjMessageHandler);
  begin
  assert(ID>high(TrDesc));
  TrDesc[ID].hnd:=handler;
end;

 var EventsCount:integer;
 var tp:tPollTop;

procedure Reciever(var p:tPollFD);
  var pkLen:LongWord;
  var From:tSockAddrL;
  var FromLen:LongWord;
  var Msg:tSMsg;
  var op :byte absolute Buffer[1];
  var tr :word;
  begin
  if (p.revents and pollIN)=0 then exit;
  FromLen:=sizeof(From);
  pkLen:=fprecvfrom(p.FD,@Buffer,sizeof(Buffer),0,@from,@fromlen);
  SC(@fprecvfrom,pkLen);
  p.revents:=0;
  Msg.Source.FromSocket(from);
  Msg.Length:=pkLen;
  Msg.Data:=@Buffer;
  Msg.st.Init(@Buffer,pkLen,sizeof(Buffer));
  Msg.channel:=0; {!multisocket}
  Msg.ttl:=0;
  if op<128 then begin
    if (op>=low(OpDesc))and(op<=high(OpDesc)) and assigned(OpDesc[op].hnd) then begin
      OpDesc[op].hnd(Msg);
    end
    else writeln('ServerLoop: No handler for opcode '+IntToStr(op));
  end else begin
    tr:=(Buffer[2] shl 8) or Buffer[3];
    if (tr<high(TrDesc)) and assigned(TrDesc[TR].hnd) then begin
      TrDesc[TR].hnd(Msg);
    end
    else writeln('ServerLoop: No handler for transaction '+IntToStr(tr)+' opcode '+IntToStr(op));
  end;
end;

var GetMTimeOffsetSec:DWORD=0;
function GetMTime:tMTime;
 {$IFDEF UNIX}
 var time:UnixType.timespec;
 var trans:QWORD;
 begin
 assert(clock_gettime(CLOCK_MONOTONIC,@time)=0);
 trans:=((time.tv_sec-GetMTimeOffsetSec)*1000)+(time.tv_nsec div 1000000);
 GetMTime:=trans and $FFFFFFFF;
 {$ELSE}{$ERROR Not Implemented on non unix}
 begin GetMTime:=0;
{$ENDIF}end;
procedure InitMTime; {$IFDEF UNIX}
 var time:UnixType.timespec;
 begin
 assert(clock_gettime(CLOCK_MONOTONIC,@time)=0);
 GetMTimeOffsetSec:=time.tv_sec;
 {$ELSE}{$ERROR Not Implemented on non unix}
 begin
{$ENDIF}end;

{$IFDEF Linux}
function prctl( option:cint; arg2,arg3,arg4,arg5:culong):cint; cdecl; external;
const PR_SET_NAME=15;
{$ENDIF}
procedure SetThreadName(name:pchar);
{$IFDEF Linux} begin prctl(PR_SET_NAME,culong(pchar(name)),0,0,0)
{$ELSE}begin{$NOTE Custom thread mames not supported}
{$ENDIF} end;

procedure Timer;
 var cur:^tSheduled;
 var pcur:^pointer;
 var delta:LongWord;
 var tasks:word;
 begin
 {Sheduling}
 {gmagic with delta-time, increment mNow, ...}
 mNow:=GetMTime;
 delta:=mNow-LastShed;
 LastShed:=mNow;
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

var ReExec:boolean=false;
procedure Main;
 begin
 s_setupInet;
 while not terminated  do begin
  PollTimeout:=5000;
  Timer;
  Flush(OUTPUT);
  EventsCount:=fpPoll(@PollArr[0],PollTop,PollTimeout);
  Timer;
  if (eventscount=-1)and terminated then break;
  if eventscount=-1 then break;  {fixme: print error}
  if eventscount=0 then continue else begin
   {INET socket}
   Reciever(PollArr[0]);
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
 if ReExec then fpExecv(paramstr(0),argv);
end;

procedure WatchFD(fd:tHandle; h:tFDEventHandler; e:LongWord);
 var opt: tPollTop;
begin
 if assigned(h) then begin
  PollHnd[pollTop].CB:=h;
  PollArr[pollTop].fd:=fd;
  PollArr[pollTop].events:=e;
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
procedure WatchFD(fd:tHandle; h:tFDEventHandler);
  begin
  WatchFD(fd,h,POLLERR or POLLHUP or POLLIN or POLLPRI or 
  POLLRDBAND or POLLRDNORM);
end;
procedure WatchFDRW(fd:tHandle; h:tFDEventHandler);
  begin
  WatchFD(fd,h,POLLERR or POLLHUP or POLLIN or POLLPRI or 
  POLLRDBAND or POLLRDNORM or POLLOUT);
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
procedure RequestTerminate(c:byte);
begin Terminated:=true;
  if c=9 then ReExec:=true;
end;

//var nb:array [0..0] of byte;
BEGIN
 writeln('ServerLoop: ','BrodNetD',' ',GIT_VERSION);
 mNow:=0;
 Randomize;
 fpSignal(SigInt,@SignalHandler);
 fpSignal(SigTerm,@SignalHandler);
 fpSignal(SigPipe,baseunix.signalhandler(SIG_IGN));
 FillChar(OpDesc,sizeof(OpDesc),0);
 FillChar(TrDesc,sizeof(TrDesc),0);
 pollTop:=1; {1 for basic listen}
 ShedTop:=nil;
 ShedUU:=nil; {todo: allocate a few to improve paging}
 InitMTime;
 LastShed:=GetMTime;
 if OptIndex('-h')>0 then DoShowOpts:=true;
 OnTerminate:=nil;
 Flush(OUTPUT);
 //SetTextBuf(OUTPUT,nb);
END.
