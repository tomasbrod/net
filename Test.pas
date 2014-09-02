{$mode objfpc}
uses sysutils;

type tBigRecord=record b:array [0..65535] of byte; end;
type tBigRecFile=file of tBigRecord;

var iv,nv: tBigRecFile;
var br: tBigRecord;

procedure llza(var F:file);
begin
end;

procedure MyOpen(var F:file);
begin
 assign(F,'unex.$$$');
 {
 reset(nv);
 This would reset the file with record size of 128 not the orig rec size
 }
end;

begin
 assign(iv,'unex.$$$');
 rewrite(iv);
 write(iv,br);
 writeln('Rec ',FileSize(iv));
 close(iv);
 MyOpen(nv);
 reset(nv);
 writeln('Rec ',FileSize(nv));
 close(nv);
end.