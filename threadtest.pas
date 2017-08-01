uses cthreads,sysutils;
var ti1:tThreadID; 

function thr1(arg:pointer):ptrint;
  begin
  writeln('.');
  EndThread;
end;

begin
  ti1:=BeginThread(@thr1,nil);
  sleep(1000);
END.
