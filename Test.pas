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

procedure DataBaseTest;
 var db: DataBase.tAccess;
 var c :word;
 var p :tRecord;
 begin
 with db do begin
  try
  Init( sizeof(word), 'test', '001', 'word' );
  Assert ( FileExists( DataBase.Prefix+'/test/001/word.dat' ) );
  c:=76;
  Append(c);
  c:=0;
  LastPos(p);
  assert( p=0 );
  c:=78;
  Append(c);
  c:=0;
  LastPos(p);
  assert( p=1 );
  Read(c, 0);
  assert( c=76 );
  c:=0;
  Delete( 0 );
  Read(c, 0);
  assert( c=78 );
  c:=55;
  OverWrite(0,c);
  c:=0;
  Read(c,0);
  Write(c,^M);
  Assert( c=55 );
  finally 
   db.purge;
   Assert ( not FileExists( DataBase.Prefix+'/test/001/word.dat' ) );
  end;
 end;
end;

procedure DataBaseListTest;
 var list :tRowList;
 var row :tRow;
 begin
 with list do begin
  ForceDirectories( DataBase.Prefix+'/testr/001/' );
  init('testr');
  try
   try repeat
    Read(row);
    assert(row='001');
   until false; except on eRangeError do ; end;
  finally done; end;
 end;
end;

BEGIN
 KeysTest;
 DataBaseTest;
 DataBaseListTest;
END.