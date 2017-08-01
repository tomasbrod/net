uses SysUtils,ObjectModel,Classes,ServerLoop;

{
  Unit: log:=ServerLoop.CreateLog('Unit1');
  log.Info('.func1 xD',[]);

  log1 -> stderr
       -> file_append
       -> syslog
       -> winlog
}


var log:tEventLog;
type kek=procedure of object;
const id='zidan';

procedure test(const a; l:longword);
  var s:string;
  begin
  setlength(s,l);
  move(a,s[1],l);
  log.info('.test: string(%D)=%S',[length(s),s]);
end;

begin
  ServerLoop.CreateLog(log,'Program');
  Log.Info('.PASCALMAIN: Test!',[]);
  Log.Error('.PASCALMAIN: scary error',[]);
  Log.Info('.PASCALMAIN: callback size=%D',[sizeof(kek)]);
  Log.Info('.PASCALMAIN: string size=%D',[sizeof(id)]);
  test(id[1],length(id));
  {085131 Info log_test PASCALMAIN Test!}
  FreeAndNil(Log);
end.
