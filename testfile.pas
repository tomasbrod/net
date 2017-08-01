uses sysutils;
var f:textfile;
var l:string;
begin
  assign(f,'testfile.pas');
  reset(f);
  repeat
    if eof(f) then writeln('eof');
    readln(f,l);
    writeln('k:',l);
  until eof(f);
end.
