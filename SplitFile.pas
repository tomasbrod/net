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

type tData = class (tStream)
 (* Read only stream with the data of the file assembled *)
 public
 constructor Create( fid: tFID );
 destructor  Destroy; override;
 function Read( var Buffer; Count: LongInt ) :LongInt; override;
 function Seek( const Offset: Int64; Origin: TSeekOrigin ):Int64; override;
 private
 fid:tFID;
 fidstr:string[40];
 piece: SimpleFile.tData;
 seekcount: Word;
 fposition: LongWord;
 end;

type tDataRedist = class (tStream)
 (* Read only stream with the splitfile list data *)
 public
 constructor Create( fid: tFID );
 destructor  Destroy; override;
 function Read( var Buffer; Count: LongInt ) :LongInt; override;
 function Seek( const Offset: Int64; Origin: TSeekOrigin ):Int64; override;
 private
 fid: tFID;
 fidstr: string[40];
 fposition: LongWord;
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

{DataBase:

 fid, offset, ref
 
 }
 
var Store: DataBAse.tDbDataSet;

procedure TranslateOffset( var Offset: Int64; Origin: TSeekOrigin; Position: Int64 );
 begin
 case Origin of
  soCurrent: Offset:=Offset+Position;
  soBeginning:;
  else raise eStreamError.Create('Unimplemented');
 end;
end;

function tData.Seek( const Offset: Int64; Origin: TSeekOrigin ):Int64;
 var found:boolean;
 var ipofs:LongInt;
 var nOffset:Int64;
 begin
 {Search for the piece}
 Store.IndexName:='fid+offset';
 nOffset:=Offset;
 TranslateOffset(nOffset,Origin, fPosition);
 found:=Store.SearchKey( fidstr+IntToStr(nOffset), stGreaterEqual );
 found:=found and (Store.Fields[0].AsString=fidstr);
 if not found then raise DataBase.eSearch.Create;
 {Open blob stream}
 FreeAndNil(Piece);
 Piece.Create(tFID(tHash(Store.Fields[2].AsString)));
 {seek offset in piece}
 ipofs:=nOffset-Store.Fields[1].AsInteger;
 if ipofs<>Piece.Seek(ipofs,soBeginning) then raise DataBase.eSearch.Create;
 {return}
 Seek:=nOffset;
 fPosition:=nOffset;
end;

function tData.Read( var Buffer; Count: LongInt ) :LongInt;
 var partread:LongInt;
 begin
 result:=0;
 while result<count do begin
  partread:= piece.Read( buffer, count );
  result:= result + partread;
  fposition:= fposition + partread;
  if partread = 0 then try
   Seek(0, soCurrent); { Trigger switch to next piece }
  except on eSearch do break; end;
 end;
end;


END.