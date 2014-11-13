unit SimpleFiles;

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
  ,FileStore
  ,Keys
  ;

type tData = class (tDBFBlobStream)
 (* Read only stream with the data of the file *)
 public
 constructor Create( fid: tFID );
 { for future reference
 destructor  Destroy; override;
 function Read( var Buffer; Count: LongInt ) :LongInt; override;
 function Seek( const Offset: Int64; Origin: TSeekOrigin ):Int64; override
 }
 end;

procedure Add( fid: tFID; stream: tStream );
(* Import data from the stream to database and run necessary checks. Throws 
exception on error *)
 type eExist= class (Exception) end; (* Attempt to add already-existing object *)
 type eDataCheck= class (Exception) end; (* Hash not matching in the data *)

function Exists( fid: tFID ) :boolean;

procedure Delete( fid: tFID );

{TODO: provide means of listing the files}

IMPLEMENTATION

(* The Store

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

Mixed (small in db large in dir):

- Not too many files
- No big blob file

*)

{Currently we have the database solution here}

type tFileTable = class( DataBase.tDbDataSet )
 constructor Create; overload;
 end;

var Store :tFileTable;

constructor tFileTable.Create;
 begin
 inherited Create(nil);
 FieldDefs.Add( {0} 'key',    ftString,  40, True  );
 FieldDefs.Add( {1} 'data',    ftBlob,   0, False  );
 with IndexDefs.Add do begin Name:='key'; Expression:=Name; Options:=[ixPrimary, ixUnique, ixCaseInsensitive]; end;
 Open ('simplefiles');
end;

constructor tData.Create( fid: tFID );
 var SuccessLocate:boolean;
 begin
 SuccessLocate:= Store.Locate( 'key', String(tHash(fid)), [] );
 if not SuccessLocate then raise DataBase.eSearch.Create;
 inherited Create( Store.Fields[1] );
end;

procedure Add( fid: tFID; stream: tStream );
 var SuccessLocate:boolean;
 var idstr:string;
 var trg:tDbfBlobStream;
 var DataHash :tHash;
 begin
 idstr:= String(tHash(fid));
 SuccessLocate:= Store.Locate( 'key', idstr, [] );
 if SuccessLocate then raise eExist.Create('');
 Store.Append;
 Store.Fields[0].AsString:=idstr;
 trg:=tDbfBlobStream.Create( Store.Fields[1] );
 try
  trg.size:=0;
  trg.CopyFrom( stream, 0 );
  trg.Position:=0; {start from begining}
  DataHash.Compute( trg ); {compute hash}
  if DataHash = tHash(fid) then begin
   trg.Commit;
   Store.Post;
  end else begin
   trg.Cancel;
   raise eDataCheck.Create('');
  end;
 finally trg.Free; end;
end;
  
function Exists( fid: tFID ) :boolean;
 var SuccessLocate:boolean;
 begin
 SuccessLocate:= Store.Locate( 'key', String(tHash(fid)), [] );
 Exists:=SuccessLocate;
end;
 
procedure Delete( fid: tFID );
 var SuccessLocate:boolean;
 begin
 SuccessLocate:= Store.Locate( 'key', String(tHash(fid)), [] );
 if not SuccessLocate then raise DataBase.eSearch.Create;
 Store.Delete;
end;

INITIALIZATION
 Store:=tFileTable.Create;
FINALIZATION
 Store.Destroy;
END.