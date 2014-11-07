unit SimpleFile;

(*
Managment of files in datastore
*)

INTERFACE
uses Classes, Db;

function OpenStream( fid: tFID; mode: db.tBlobStreamMode ) : Classes.tStream;

IMPLEMENATION
uses 
  log
  ,SysUtils
  ,dbf
  ,dbf_common
  ;

(* The filestore

File based solution:

- Files are stored in separate directory and the filename is like this:

       datadir/objects/first-2-chars/remaining-38-chars

- more complex
- harder to implement ( deleting empty directories? )
- stable against inode corruption
- avoids excessively large blob files

Database solution:

- Everithing is stored in one table.
- Big blob file
- not using benefits of filesystem
- avoids too many files and directories

*)

{Currently we have the database solution here}

type tFileTable = class( tDbDataSet )
 constructor Create; overload;
 end;

var Filestore :tFileTable;

constructor tFileTable.Create;
 begin
 inherited Create(nil);
 OpenMode:=omAutoCreate;
 FieldDefs.Add( {0} 'key',    ftString,  40, True  );
 FieldDefs.Add( {1} 'data',    ftBlob,   0, False  );
 with IndexDefs.Add do begin Name:='key'; Expression:=Name; Options:=[ixPrimary, ixUnique, ixCaseInsensitive]; end;
 Open ('simplefiles');
end;

function OpenStream( fid: tFID; mode: db.tBlobStreamMode ) : Classes.tStream;
 var idstr:string;
 begin
 idstr:= String(tHash(fid));
 if not Filestore.Locate( 'key', idstr, [] ) then begin
  if mode=db.bmWrite then begin
   Filestore.Append;
   Filestore.Field[0].AsString:= idstr;
  end else raise DataBase.eSearch.Create
 end;
 OpenStream:= CreateBlobStream(Filestore.Fields[1], mode);
end;
 

