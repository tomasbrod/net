UNIT ServerLoop;

INTERFACE
uses Classes,ObjectModel,UnixType,Sockets,IniFiles;

{#Main#}
procedure Main;
procedure RequestTerminate(c:byte);
var OnTerminate:procedure;

{#Version#}
const GIT_VERSION='deprecated' deprecated;
var VER_MINOR: integer external name 'LVER_MINOR';
var VER_PATCH: integer external name 'LVER_PATCH';
var VER_CLEAN: boolean external name 'LVER_CLEAN';
var VER_BUILD: integer external name 'LVER_BUILD';
var VER_HASH: shortstring external name 'LVER_HASH';
//const VER_BRANCH='dev';
//const VER_USER='tomas@manganp';
var VersionString:AnsiString;

{#Log#}
var Log1: tEventLogBaseSink;
procedure CreateLog(out log:tEventLog; ident:string);

{#Message handling#}
type tSMsg=object
   Source: tNetAddr;
   Length: {Long}Word;
   Data: pointer;
   st: tStream;
   channel: word;
   ttl: word;
   tos: byte;
   OP: byte;
  end;
type tMessageHandler=procedure(msg:tSMsg);
type tObjMessageHandler=procedure(msg:tSMsg) of object;
const cDGramSz = 420; {plz do this better}
procedure SetupOpcode(OpCode:byte; handler:tMessageHandler);
procedure NewMsgTr(out ID:Word; handler:tObjMessageHandler);
procedure SetMsgTr(ID:Word; handler:tObjMessageHandler);
//procedure DelMsgTr(ID:Word);

procedure SendMessage(const data; len:word; const rcpt:tNetAddr );
function GetSocket(const rcpt:tNetAddr):tSocket;
function GetSocketTTL(const rcpt:tNetAddr): Word;

{#Sheduling and watching#}
type tFDEventHandler=procedure(ev:Word) of object;
type tOnTimer=procedure of object;
var GlobalLock:tRTLCriticalSection;
procedure WatchFD(fd:tHandle; h:tFDEventHandler);
procedure WatchFDRW(fd:tHandle; h:tFDEventHandler);

procedure Shedule(timeout{ms}: LongWord; h:tOnTimer);
procedure UnShedule(h:tOnTimer);
 {note unshed will fail when called from OnTimer proc}

{#Obscure things#}
function OptIndex(o:string):word;
function OptParamCount(o:word):word;

type tTimeVal=UnixType.timeval;
type tMTime=DWORD;
  {49days 17hours to overflow}
var iNow:tTimeVal;
var mNow:tMTime;
function GetMTime:tMTime;

procedure SetThreadName(name:pchar);

{#Configuration#}
var Config: TIniFile;
function GetCfgStr(name:pchar):String;
function GetCfgNum(name:pchar):Double;

IMPLEMENTATION

USES SysUtils,BaseUnix
     ,Unix
     ,Porting
     ;

(**** Terminate and Signals ****)

var Terminated:boolean=false;
var ReExec:boolean=false;
procedure SignalHandler(sig:cint);CDecl;
 begin
  Log1.LogMessage('ServerLoop',etWarning,' Shutting Down (Signal %D)',[sig]);
  if terminated then raise eControlC.Create('CtrlC DoubleTap') ;
  Terminated:=true;
 end;
procedure FatalSignalHandler(sig:cint);CDecl;
  begin
  raise eExternal.Create('Unexpected Signal '+IntToStr(sig)) ;
  Terminated:=true;
end;
procedure RequestTerminate(c:byte);
begin Terminated:=true;
  if c=9 then ReExec:=true;
end;

(**** Time Tracking ****)

var GetMTimeOffsetSec:Int64=0;

function GetMTime:tMTime;
  var sec:Int64;
  var mil:LongWord;
  begin
  po_monotonicnanoclocks(sec,mil);
  GetMTime:= ((sec-GetMTimeOffsetSec)*1000 + (mil div 1000000)) and $FFFFFFFF;
end;
procedure InitMTime;
  var mil:LongWord;
  begin
  po_monotonicnanoclocks(GetMTimeOffsetSec,mil);
end;

(**** Timer / Sheduler ****)

type tSheduled_ptr=^tSheduled; tSheduled=record
  time:LongWord;
  cb:tOnTimer;
  next:tSheduled_ptr;
 end;
var ShedTop: ^tSheduled;
var ShedUU: ^tSheduled;
var PollTimeout:LongInt;

procedure Shedule(timeout{ms}: LongWord; h:tOnTimer);
  var c:^tSheduled;
  var pp:^pointer;
  begin
  timeout:=timeout+mNow;
  c:=ShedTop;
  pp:=@ShedTop;
  while assigned(c) do begin
    if c^.time>timeout then break;
    pp:=@c^.next;
    c:=c^.next;
  end;
  if Assigned(ShedUU) then begin
    c:=ShedUU;
    ShedUU:=c^.next;
  end else New(c);
  c^.time:=timeout;
  c^.cb:=h;
  c^.next:=pp^;
  pp^:=c;
end;

procedure UnShedule(h:tOnTimer);
  var cur:^tSheduled;
  var pcur:^pointer;
  begin
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

procedure Timer;
  var cur:^tSheduled;
  var delta:LongInt;
  begin
  {Sheduling}
  mNow:=GetMTime;
  cur:=ShedTop;
  while assigned(cur) do begin
    delta:=cur^.time-mNow;
    if delta<=0 then begin
      ShedTop:=cur^.next;
      cur^.next:=ShedUU;
      ShedUU:=cur;
      cur^.cb;
    end else begin
      if delta<PollTimeout then PollTimeout:=delta;
      break;
    end;
    cur:=ShedTop;
  end;
end;

(*** Extended message delegation ***)

var Buffer:array [1..4096] of byte;
type tOpDesc=record
  hnd: tMessageHandler;
end;
var OpDesc: array [1..64] of tOpDesc;
type tTrDesc=record
  hnd: tObjMessageHandler;
end;
var TrDesc: array [0..223] of tTrDesc;

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
  assert(ID<=high(TrDesc));
  TrDesc[ID].hnd:=handler;
end;



procedure Reciever(var p:tPollFD);
  var Msg:tSMsg;
  var op :byte absolute Buffer[1];
  var tr :word;

  procedure RecieverP1;
    var
      AuxBuf:array [1..128] of dword; {size??}
      msgtop:tMsgHdr;
      IOV:Tiovec;
      nret:cint;
      SrcAddr:tSockAddrL;
      cmsg:^tCMsgHdr;
    begin
    FillChar(msgtop,sizeof(msgtop),0);
    FillChar(AuxBuf,sizeof(AuxBuf),9);
    msgtop.IOV:=@IOV;
    msgtop.IOVlen:=1;
    IOV.base:=@Buffer;
    IOV.len:=sizeof(Buffer);
    msgtop.control:=pointer((PtrUInt(@AuxBuf)+15)and(not 15));
    msgtop.controllen:=sizeof(AuxBuf)-16;
    msgtop.name:=@SrcAddr;
    msgtop.namelen:=sizeof(SrcAddr);
    Msg.ttl:=0;
    Msg.tos:=0;
    Msg.channel:=0; {!multisocket}
    nret:= RECVMSG( p.FD, @msgtop, 0 );
    SC(@fprecvfrom,nret);
    cmsg:=pointer(msgtop.control);
    if msgtop.controllen<sizeof(cmsg^) then cmsg:=nil;
    while assigned(cmsg) do begin
      //writeln('attrib level ',cmsg^.level,' name ',cmsg^.name,' len ',cmsg^.len,' v ',cmsg^.val);
      if (cmsg^.level=IPPROTO_IP)and(cmsg^.name=IP_TTL)
        then  Msg.ttl:=cint(cmsg^.val)
      else if (cmsg^.level=IPPROTO_IP)and(cmsg^.name=IP_TOS)
        then  Msg.tos:=byte(pointer(@cmsg^.val)^);
    cmsg:=cmsg_nxthdr(@msgtop, cmsg) end;
    Msg.Source.FromSocket(SrcAddr);
    Msg.Length:=nret;
    Msg.Data:=@Buffer;
    Msg.OP:=op;
    Msg.st:=tBufferStream.Create(@Buffer,Msg.Length);
  end;

  begin
  if (p.revents and pollIN)=0 then exit;
    p.revents:=0;
  RecieverP1;
  if op<128 then begin
    if (op>=low(OpDesc))and(op<=high(OpDesc)) and assigned(OpDesc[op].hnd) then begin
      OpDesc[op].hnd(Msg);
    end
    else Log1.LogMessage('ServerLoop',etWarning,' No handler for opcode %D',[op]);
  end else begin
    tr:=(Buffer[2] shl 8) or Buffer[3];
    if (tr<high(TrDesc)) and assigned(TrDesc[TR].hnd) then begin
      TrDesc[TR].hnd(Msg);
    end
    else Log1.LogMessage('ServerLoop',etWarning,' No handler for transaction %D opcode %D',[tr,op]);
  end;
end;

(**** Logging ****)

procedure CreateLog(out log:tEventLog; ident:string);
  begin
  Log:=tEventLog.Create(Log1,ident);
end;

(**** Config File ****)

var DoShowOpts:boolean=false;

procedure InitConfig;
  var fs:tFileStream;
  var fn:ansistring;
  var oi:word;
  begin
  fn:='bn.cfg';
  oi:=OptIndex('-c');
  if oi>0 then begin
    assert(OptParamCount(oi)=1);
    fn:=ParamStr(oi+1);
  end;
  try
    fs:=tFileStream.Create(fn,fmOpenRead);
    Config:=tIniFile.Create(fs,false);
  except on e:eInOutError do begin
      Log1.LogMessage('ServerLoop',etError,'.ConfigInit(%S): %S',[fn,e.message]);
      Raise;
    end;
  end;
  fs:=nil;
  if not (SetCurrentDir(ExtractFilePath(fn)) and SetCurrentDir(GetCfgStr('directory'))) then
    raise eInOutError.Create('ServerLoop: Error changing working directory');
  Log1.LogMessage('ServerLoop',etInfo,': cfg=%S cwd=%S pid=%D',[fn,GetCurrentDir,GetProcessID]);
end;

function GetCfgStr(name:pchar):String;
  var e:exception;
  begin
  if DoShowOpts then Log1.LogMessage('ServerLoop',etDebug,'.Config: %S',[name]);
  result:=config.ReadString('options',name,#1);
  if result=#1 then begin
    e:=eXception.CreateFmt('Missing option %S in config file',[name]);
    Log1.LogMessage('ServerLoop',etError,'.Config: %S',[e.Message]);
    raise e;
  end;
end;
function GetCfgNum(name:pchar):Double;
  {$hint this is sub-optimal}
  var e:exception;
  begin
  try result:=StrToFloat(GetCfgStr(name));
  except on x:EConvertError do begin
    e:=eXception.Create('Numeric option "'+name+'" expected, '+x.message);
    Log1.LogMessage('ServerLoop',etError,'.Config: %S',[e.Message]);
    raise e;
  end end;
end;

function OptIndex(o:string):word;
 begin
 if DoShowOpts then Log1.LogMessage('ServerLoop',etDebug,'.Option: %S',[o]);
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

(**** Polled Descriptors Registry ****)

type tPollTop=0..7;
var pollArr: packed array [tPollTop] of tPollFd;
type tFdHndDsc=record
 cb: tFDEventHandler; {proc+object}
 end;
var pollHnd: array [tPollTop] of tFdHndDsc;
var pollTop: tPollTop;

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
  WatchFD(fd,h,POLLERR or POLLHUP or POLLIN);
end;
procedure WatchFDRW(fd:tHandle; h:tFDEventHandler);
  begin
  WatchFD(fd,h,POLLERR or POLLHUP or POLLIN or POLLOUT);
end;

procedure s_SetupInet; forward;

procedure Main;
 var tp:tPollTop;
 var EventsCount:integer;
 begin
 s_setupInet;
 Config.Stream.Free;
 Config.Free;
 while not terminated  do begin
  PollTimeout:=5000;
  Timer;
  Flush(OUTPUT);
  LeaveCriticalSection(GlobalLock);
  EventsCount:=fpPoll(@PollArr[0],PollTop,PollTimeout);
  EnterCriticalSection(GlobalLock);
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
 CloseSocket(PollArr[0].fd);
 DoneCriticalSection(GlobalLock);
 if ReExec then fpExecv(paramstr(0),argv);
end;

(**** Sockets ****)
var s_ip4: tSocket;
var s_ip4_ttl:Word;

function GetSocket(const rcpt:tNetAddr):tSocket;
 begin
 result:=s_ip4;
end;
function GetSocketTTL(const rcpt:tNetAddr): Word;
  begin
  result:=s_ip4_ttl;
end;
procedure SendMessage(const data; len:word; const rcpt:tSockAddrL );
 begin
 {SC(@fpsendto,}fpsendto(s_ip4,@data,len,0,@rcpt,sizeof(sockaddr_in)){)};
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

procedure s_SetupInet;
 var bind_addr:tInetSockAddr;
 var optval:cint;
 var optlen:csize_t;
 begin
  with bind_addr do begin
   sin_family:=AF_INET;
   sin_port:=htons(trunc(GetCfgNum('port')));
   sin_addr.s_addr:=0; {any}
   s_ip4:=fpSocket(sin_family,SOCK_DGRAM,IPPROTO_UDP);
   SC(@fpSocket,s_ip4);
   optval:=IP_PMTUDISC_DO;
   SC(@fpsetsockopt,fpsetsockopt(s_ip4, IPPROTO_IP, IP_MTU_DISCOVER, @optval, sizeof(optval)));
   optlen:=sizeof(optval);
   SC(@fpgetsockopt,fpgetsockopt(s_ip4, IPPROTO_IP, IP_TTL, @optval, @optlen));
   s_ip4_ttl:=optval;
   optval:=1;
   SC(@fpsetsockopt,fpsetsockopt(s_ip4, IPPROTO_IP, IP_RECVTTL, @optval, sizeof(optval)));
   SC(@fpsetsockopt,fpsetsockopt(s_ip4, IPPROTO_IP, IP_RECVTOS, @optval, sizeof(optval)));
  end;
  SC(@fpBind,fpBind(s_ip4,@bind_addr,sizeof(bind_addr)));
  with PollArr[0] do begin
   fd:=s_ip4;
   events:=pollIN;
   revents:=0;
  end;
end;

(**** Other ****)
{$IFDEF Linux}
function prctl( option:cint; arg2,arg3,arg4,arg5:culong):cint; cdecl; external;
const PR_SET_NAME=15;
procedure SetThreadName(name:pchar);
begin prctl(PR_SET_NAME,culong(pchar(name)),0,0,0)
end;
{$ELSE}
procedure SetThreadName(name:pchar);
begin{$NOTE Custom thread mames not supported}
end;
{$ENDIF}

(**** Unit Initialization ****)
BEGIN
 VersionString:=Format('BrodNetD %D.%D.%.8S.%D',[VER_MINOR,VER_PATCH,VER_HASH,VER_BUILD]);
 if not VER_CLEAN then VersionString:=VersionString+'.dirty';
 Log1:=tEventLogBaseSink.Create;
 Log1.LogMessage('ServerLoop',etInfo,': %S',[VersionString]);
 InitCriticalSection(GlobalLock);
 EnterCriticalSection(GlobalLock);
 if OptIndex('-h')>0 then DoShowOpts:=true;
 InitConfig;
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
 mNow:=GetMTime;
 OnTerminate:=nil;
 //SetTextBuf(OUTPUT,nb);
 Flush(OUTPUT);
END.
