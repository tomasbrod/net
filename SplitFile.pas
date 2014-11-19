unit SplitFile;

(* Handles storage of splitfiles, storage and reassembly *)

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
  ,SimpleFile
  ,Keys
  ;

function OpenRead( fid: tFID ): tStream;
(* Read only stream with the data of the file assembled *)

function ListOpenRead( fid: tFID ): tStream;
(* Read only stream with the splitfile list data *)

procedure Add( fid: tFID; stream: tStream );
(* Decode and import piecelist from the stream to database and request the 
pieces. Integrity checking is performed after all pieces were downloaded. 
*)

function Exists( fid: tFID ) :boolean;

procedure Delete( fid: tFID );

procedure HandleTransferFinished( fid: tFID );
(* Check if the file is part of any splitfile and if the splitfile is 
downloaded completly, check integrity of that splitfile. *)

{TODO: provide means of listing the files}

IMPLEMENTATION

type tData = class (tStream)
 (* Read only stream with the data of the file assembled *)
 public
 constructor Create( afid: tFID );
 destructor  Destroy; override;
 function Read( var Buffer; Count: LongInt ) :LongInt; override;
 private
 fid:tFID;
 fidstr:string[40];
 pieceno: Word;
 piece: tStream;
 function SwitchPiece:boolean;
 end;


{DataBase:

 Store: fid, no, ref
 Meta: fid, left, type
 
 }

type tStoreTable=class(DataBAse.tDbDataSet)
 constructor Create;
end;
type tMetaTable=class(DataBAse.tDbDataSet)
 constructor Create;
end;

constructor tStoreTable.Create;
 begin
 inherited Create(nil);
 FieldDefs.Add( {0} 'fid',   ftString, 40, True  );
 FieldDefs.Add( {1} 'no',   ftInteger,  0, True  );
 FieldDefs.Add( {2} 'ref',   ftString, 40, True  );
 with IndexDefs.Add do begin 
   Name:='fid+no';
   Expression:='str(key,5,0)+str(no,5,0)';
   Options:=[ixPrimary, ixUnique, ixCaseInsensitive]; end;
 with IndexDefs.Add do begin Name:='ref'; Expression:=Name; Options:=[ixCaseInsensitive]; end;
 Open ('splitfst');
end;

constructor tMetaTable.Create;
 begin
 inherited Create(nil);
 FieldDefs.Add( {0} 'fid',   ftString, 40, True  );
 FieldDefs.Add( {1} 'left',  ftInteger,  0, True  );
 FieldDefs.Add( {2} 'type',  ftInteger,  0, False  );
 with IndexDefs.Add do begin Name:='fid'; Expression:=Name; Options:=[ixCaseInsensitive]; end;
 with IndexDefs.Add do begin Name:='left'; Expression:=Name; Options:=[ixDescending]; end;
 Open ('splitfmt');
end;
 
var Store: tStoreTable;
var Meta: tMetaTable;

function tData.SwitchPiece:boolean;
 var found:boolean;
 begin
 result:=false;
 {Search for the piece}
 Store.IndexName:='fid+no';
 found:=Store.SearchKey( fidstr+IntToStr(pieceno), stEqual );
 //found:=found and (Store.Fields[0].AsString=fidstr);
 if not found then exit;
 {Open blob stream}
 FreeAndNil(Piece);
 Piece:= SimpleFile.OpenRead(tFID(tHash(Store.Fields[2].AsString)));
 result:=true;
end;

function tData.Read( var Buffer; Count: LongInt ) :LongInt;
 var partread:LongInt;
 begin
 result:=0;
 while result<count do begin
  partread:= piece.Read( (@Buffer + result)^, Count );
  result:= result + partread;
  if partread = 0 then begin
   inc(pieceno);
   if not SwitchPiece then break;
  end;
 end;
end;

constructor tData.Create( afid: tFID );
 begin
 inherited Create;
 fid:=afid;
 fidstr:=String(tHash(afid));
 if not (
  Meta.Locate('fid',fidstr,[])
  and (Meta.Fields[1].AsInteger=0) )
 then raise database.esearch.create;
 piece:=nil;
 {
 pieceno:=0;
 while true do begin
  if not SwitchPiece then break;
  inc(pieceno);
 end;
 }
 pieceno:=0;
 if not SwitchPiece then raise DataBase.eSearch.Create;
end;

destructor tData.Destroy;
 begin
 FreeAndNil(piece);
 inherited;
end;

INITIALIZATION
 Store:=tStoreTable.Create;
 Meta:=tMetaTable.Create;
FINALIZATION
 Store.Free;
 Meta.Free;
END.