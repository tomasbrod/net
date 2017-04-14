UNIT Porting;
INTERFACE

{$IFDEF UNIX} {$DEFINE USE_PTHREADS} {$ENDIF}

uses UnixType,CTypes
  {$IFDEF USE_PTHREADS},PThreads{$ENDIF}
  ;

function po_unixtimenow:Int64; inline;
procedure po_monotonicnanoclocks(out seconds:Int64; out nano:LongWord);
procedure po_condwait(var v: pthread_cond_t; var m:pthread_mutex_t; DelayMS: LongWord);

(*** ConditionEvent ***)
type tConditionEventLock=class
  public
  constructor Create;
  destructor Destroy; override;
  procedure WaitLock(timeout_ms: LongWord);
  procedure Unlock;
  procedure Lock;
  procedure SignalUnlock;
  {$IFDEF USE_PTHREADS} protected
  mutex:pthread_mutex_t;
  condv:pthread_cond_t;
  {$ELSE}protected
  cs:System.TRTLCriticalSection;
  ev:System.PRTLEVENT;
  {$ENDIF}
end;

{$packrecords C}
type
  Piovec = ^Tiovec;
  Tiovec = record
    base : pointer;
    len : size_t;
  end;
  Pmsghdr = ^tmsghdr;
  Tmsghdr = record
     name : pointer;
     namelen : socklen_t;
     iov : piovec;
     iovlen : size_t;
     control : pointer;
     controllen : socklen_t;
     flags : cInt;
  end;
  Pcmsghdr = ^Tcmsghdr;
  Tcmsghdr = record
    len   : csize_t;
    level : cInt;
    name  : cInt;
    val   : cInt;
  end;

function sendmsg(__fd: cInt; __message: pmsghdr; __flags: cInt): ssize_t; cdecl; external name 'sendmsg';
function recvmsg(__fd: cInt; __message: pmsghdr; __flags: cInt): ssize_t; cdecl; external name 'recvmsg';
function cmsg_nxthdr(__mhdr:Pmsghdr;__cmsg:Pcmsghdr):Pcmsghdr; cdecl; external name '__cmsg_nxthdr';

IMPLEMENTATION
uses BaseUnix;

function po_unixtimenow:Int64;
  begin
  result:=fptime;
end;

Const
  // Taken from linux/time.h
  {$IFDEF LINUX}
  CLOCK_REALTIME                  = 0;
  CLOCK_MONOTONIC                 = 1;
  CLOCK_PROCESS_CPUTIME_ID        = 2;
  CLOCK_THREAD_CPUTIME_ID         = 3;
  CLOCK_MONOTONIC_RAW             = 4;
  CLOCK_REALTIME_COARSE           = 5;
  CLOCK_MONOTONIC_COARSE          = 6;
  {$ENDIF}

function clock_gettime(clk_id : cint; tp: ptimespec) : cint; cdecl; external name 'clock_gettime';

procedure po_monotonicnanoclocks(out seconds:Int64; out nano:LongWord);
  var time:timespec;
  begin
  assert(clock_gettime(CLOCK_MONOTONIC,@time)=0);
  seconds:=time.tv_sec;
  nano:=time.tv_nsec;
end;

procedure po_condwait(var v: pthread_cond_t; var m:pthread_mutex_t; DelayMS: LongWord);
  var abstime:timespec;
  begin
  clock_gettime(CLOCK_REALTIME, @abstime);
  abstime.tv_sec:=abstime.tv_sec+(DelayMS div 1000); {add seconds to sec}
  DelayMS:=DelayMS mod 1000; {remove seconds from b}
  abstime.tv_nsec:=abstime.tv_nsec+(DelayMS*1000000);{add milis to nsec}
  if abstime.tv_nsec>1000000000 then begin
    abstime.tv_nsec:=abstime.tv_nsec-1000000000;
    abstime.tv_sec:=abstime.tv_sec+1;
  end;
  pthread_cond_timedwait(@v, @m, @abstime);
end;

constructor TConditionEventLock.Create;
  {$IFDEF USE_PTHREADS} begin
  pthread_mutex_init(@mutex,nil);
  pthread_cond_init(@condv,nil);
  {$ELSE} begin
  InitCriticalSection(cs);
  ev:=RTLEventCreate;
  {$ENDIF}
end;
destructor TConditionEventLock.Destroy;
  {$IFDEF USE_PTHREADS} begin
  pthread_cond_destroy(@condv);
  pthread_mutex_destroy(@mutex);
  {$ELSE} begin
  DoneCriticalSection(cs);
  RTLeventdestroy(ev);
  {$ENDIF}
end;

procedure TConditionEventLock.WaitLock(timeout_ms: LongWord);
  {$IFDEF USE_PTHREADS}
  begin
    pthread_mutex_unlock(@mutex);
    po_condwait(condv,mutex,timeout_ms);
  {$ELSE} begin
    RTLEventWaitFor(ev,timeout_ms);
    EnterCriticalSection(cs);
    RTLEventResetEvent(ev);
  {$ENDIF}
end;

procedure TConditionEventLock.Unlock;
  {$IFDEF USE_PTHREADS} begin
  pthread_mutex_unlock(@mutex);
  {$ELSE} begin
  LeaveCriticalSection(cs);
  {$ENDIF}
end;

procedure TConditionEventLock.Lock;
  {$IFDEF USE_PTHREADS} begin
  pthread_mutex_lock(@mutex);
  {$ELSE} begin
  EnterCriticalSection(cs);
  {$ENDIF}
end;

procedure TConditionEventLock.SignalUnlock;
  {$IFDEF USE_PTHREADS} begin
  pthread_cond_signal(@condv);
  pthread_mutex_unlock(@mutex);
  {$ELSE} begin
  RTLEventSetEvent(ev);
  LeaveCriticalSection(cs);
  {$ENDIF}
end;

END.
