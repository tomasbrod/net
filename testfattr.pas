program testfattr;

uses sysutils;
var fn:string='test.perm';
var at:LongInt;

begin
  at:=FileGetAttr(fn);
  if at<0 then writeln('ERROR ',at);
  writeln('rdonly :',(at and faReadOnly));
  writeln('hidden :',(at and faHidden));
  writeln('sysfile:',(at and faSysFile));
  writeln('volume :',(at and faVolumeID));
  writeln('directo:',(at and faDirectory));
  writeln('archive:',(at and faArchive));
end.
