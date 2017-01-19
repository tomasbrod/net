UNIT Porting;
INTERFACE
uses PThreads,UnixType;

function po_unixtimenow:Int64; inline;
procedure po_monotonicnanoclocks(out seconds:Int64; out nano:LongWord);

type po_tmutex=pthread_mutex_t;
type po_tcondv=pthread_cond_t;
procedure po_mutex_init(out m:po_tmutex);
procedure po_mutex_done(var m:po_tmutex);
procedure po_cvar_init(out v: po_tcondv; out m:po_tmutex);
procedure po_cvar_done(out v: po_tcondv; out m:po_tmutex);
procedure po_lock(var m:po_tmutex);
procedure po_unlock(var m:po_tmutex);
procedure po_signal(var v: po_tcondv; var m:po_tmutex);
procedure po_wait(var v: po_tcondv; var m:po_tmutex; DelayMS: LongWord);

IMPLEMENTATION
uses CTypes,BaseUnix,Syscall;

function po_unixtimenow:Int64;
  begin
  result:=fptime;
end;

Const
  // Taken from linux/time.h
  CLOCK_REALTIME                  = 0;
  CLOCK_MONOTONIC                 = 1;
  CLOCK_PROCESS_CPUTIME_ID        = 2;
  CLOCK_THREAD_CPUTIME_ID         = 3;
  CLOCK_MONOTONIC_RAW             = 4;
  CLOCK_REALTIME_COARSE           = 5;
  CLOCK_MONOTONIC_COARSE          = 6;

function clock_gettime(clk_id : cint; tp: ptimespec) : cint;
begin
  clock_gettime:=do_SysCall(syscall_nr_clock_gettime,tsysparam(clk_id),tsysparam(tp));
end;

procedure po_monotonicnanoclocks(out seconds:Int64; out nano:LongWord);
  var time:timespec;
  begin
  assert(clock_gettime(CLOCK_MONOTONIC,@time)=0);
  seconds:=time.tv_sec;
  nano:=time.tv_nsec;
end;

procedure po_mutex_init(out m:po_tmutex);
  begin pthread_mutex_init(@m,nil) end;
procedure po_mutex_done(var m:po_tmutex);
  begin pthread_mutex_destroy(@m) end;
procedure po_cvar_init(out v: po_tcondv; out m:po_tmutex);
  begin
  pthread_mutex_init(@m,nil);
  pthread_cond_init(@v,nil);
end;
procedure po_cvar_done(out v: po_tcondv; out m:po_tmutex);
  begin
  pthread_cond_destroy(@v);
  pthread_mutex_destroy(@m);
end;
procedure po_lock(var m:po_tmutex);
  begin pthread_mutex_lock(@m) end;
procedure po_unlock(var m:po_tmutex);
  begin pthread_mutex_unlock(@m) end;
procedure po_signal(var v: po_tcondv; var m:po_tmutex);
  begin
  pthread_cond_signal(@v);
  pthread_mutex_unlock(@m);
end;
procedure po_wait(var v: po_tcondv; var m:po_tmutex; DelayMS: LongWord);
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

END.
