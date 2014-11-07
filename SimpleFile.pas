unit SimpleFile;

(*
Managment of files in datastore
*)

INTERFACE
uses {srať to, dám všetky jednotky sem}
  Log
  ,DataBase
  ,SysUtils
  ,dbf
  ,dbf_common
  ,Classes
  ,Db
  ,getopts
  ;

type tData = class (tDBFBlobStream)
 (* Read only stream with the data of the file *)
 public
 constructor Create( fid: tFID );
 destructor  Destroy; override;
 { for future reference
 function Read( var Buffer; Count: LongInt ) :LongInt; override;
 function Seek( const Offset: Int64; Origin: TSeekOrigin ):Int64; override
 }
 end;

procedure Add( fid: tFID; stream: tStream );
(* Import data from the stream to database and run necessary checks. Throws 
exception on error *)

function Exists( fid: tFID ) :boolean;

procedure Delete( fid: tFID );

function GetDataSet: tDbDataSet;

IMPLEMENTATION

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
 

