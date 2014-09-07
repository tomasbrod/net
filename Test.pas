{$mode objfpc}
program Test;

uses GeneralPacket
    ,Keys
    ,SysUtils
    ,DataBase
    ,Peers
    ;

procedure KeysTest;
 var hash :tHash;
 var s :String;
 begin
 s:='';
 with hash do begin
  clear;
  assert( isNil );
  ToString( s );
  Assert( s = '0000000000000000000000000000000000000000' );
  FromString( s );
  Assert( isNil );
  FromString( '00100000000A00000FF000000000000000000040' );
  Assert( not isNil );
  ToString( s );
  assert( s = '00100000000A00000FF000000000000000000040' );
 end;
end;

procedure MyOpen(var F:file);
begin
 assign(F,'unex.$$$');
 {
 reset(nv);
 This would reset the file with record size of 128 not the orig rec size
 }
end;

BEGIN
 DataBaseTest;
 KeysTest;
END.