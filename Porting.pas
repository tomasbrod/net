UNIT Porting;
INTERFACE

function po_unixtimenow:Int64; inline;
procedure po_monotonicnanoclocks(out seconds:Int64; out nano:LongWord);

IMPLEMENTATION
uses CTypes,BaseUnix;

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
  cdecl; external name 'clock_gettime';

procedure po_monotonicnanoclocks(out seconds:Int64; out nano:LongWord);
  var time:timespec;
  begin
  assert(clock_gettime(CLOCK_MONOTONIC,@time)=0);
  seconds:=time.tv_sec;
  nano:=time.tv_nsec;
end;

END.
