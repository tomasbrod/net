program dbftest deprecated;
uses Dbf, db, Dbf_Common;

var Dbf1:tDBF;

BEGIN
 Dbf1:= tDbf.Create(nil);
 with Dbf1 do begin
  FilePathFull := 'data';
  TableName := 'peers.dbf';
  TableLevel := 7;
  with FieldDefs do begin
   Add( 'row', ftAutoInc, 0,	True );
   Add( 'id', ftString, 40,	True );
   Add( 'addr', ftString, 50,	True );
   Add( 'delta', ftDateTime, 0,	False );
   Add( 'since', ftDateTime, 0,	False );
   Add( 'retry', ftSmallInt, 0,	False );
  end;
  CreateTable;
  Active:=True;
  AddIndex('peersrow',	'row',	[ixUnique, ixPrimary]);
  AddIndex('peersid',	'id',	[ixUnique, ixCaseInsensitive]);
  AddIndex('peersaddr',	'addr',	[          ixCaseInsensitive]);
  Close;
 end; 
END.

