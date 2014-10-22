unit DataBase;

INTERFACE
uses Classes,SysUtils,Dbf;

TYPE

tTable = string[15];
tRow = string[63];
tField = string[15];
tRecord = Word;

tDbDataSet=class ( tDbf )
 public
 {constructor Create(AOwner: TComponent); override;}
 procedure Open (const S: string );
 procedure CreateTable;
end;

tFieldAccessor=object
 { To read and write fields in database }
 { eRangeError is raised when the record does not exist}
 constructor Init( const iRecLen :word; const Table :tTable; const Row :tRow; const Field :tField );
 destructor  Done;
 procedure Append( const D );
 procedure Append( out pos :tRecord; const D );
 procedure Read( var D; const pos :tRecord );
 procedure Delete( const pos :tRecord );
  { OverWrite the record with the last record in DB
  and truncate the field by 1 record }
 procedure Purge;
  { Delete the field completly }
 procedure OverWrite( const pos :tRecord; const D ); 
  { Overwrite the record with new value. Automatic expand. } 
 procedure Expand( const pos :tRecord );
  { Expand the field to pos, filling the non-existend record with zeros } 
  experimental;
 procedure LastPos( out pos :tRecord );
  { Get position of last append, read or write operation }
 function TotalCount :Cardinal;
  experimental;
 procedure BlockRead( var Data; offset :Cardinal; count: Cardinal );
 procedure BlockWrite( var Data; offset :Cardinal; count: Cardinal );
 private
 RecLen :word;
 dat :file;
end;
tAccess=tFieldAccessor;

tRowList=object
 { To get listing of fields }
 constructor Init( const Table :tTable );
 destructor Done;
  { Release resources if any. Is called automatically when 
  last field was read. But has to be called manualy otherwise.
  }
 procedure Read( out row :tRow );
  { eRangeError: no more fields are available }
 private
 EoF :boolean;
 Search: SysUtils.tSearchRec;
end;

var Prefix :string ='./data';
{TODO: extract from parameters }

const FieldExtension :string='dat';

procedure Open( out F :File; const Table :tTable; const Row :tRow; const Field :tField );

procedure UnInsert ( var F :file );
 experimental;
{ deletes the record at current position -1 }

{ todo
procedure Init( iprefix: string );
 with file locking
}

IMPLEMENTATION

procedure Open( out F :File; const Table :tTable; const Row :tRow; const Field :tField );
var path,name :tFileName;
begin
 path:=
  Prefix + DirectorySeparator
  + Table + DirectorySeparator
  + Row + DirectorySeparator;
 name:=Field + ExtensionSeparator + FieldExtension;
 {Create the file and directory if not exist}
 if not FileExists(path+name) then begin
  if not DirectoryExists(path) then ForceDirectories(path);
  FileClose(FileCreate(path+name));
 end;
 { And Assign the handle. Note: caller should reset() the file. }
 Assign( F, path + name);
end;

procedure UnInsert ( var F :file );
 var nwpos :int64;
 var Ex :array [0..4095] of byte;
 {$HINT We do not know the record size, but is 4k enough? }
 begin
    nwpos:=FilePos(F);
    Assert(nwpos>0);
    if not EoF(F) then begin
     Seek(F, FileSize(F)-1);
     System.BlockRead(F, Ex, 1);
     Seek(F, nwpos-1);
     {
      nwpos is position of record next of the oddending
      becouse this proc is called after the record to delete was read
     }
     System.BlockWrite(F, Ex, 1);
    end;
    Seek(F, FileSize(F)-1);
    Truncate(F);
    Seek(F, nwpos);
 end;

constructor tFieldAccessor.Init( const iRecLen :word; const Table :tTable; const Row :tRow; const Field :tField );
 begin
 Open( dat, Table, Row, Field );
 Reset( dat, 1 );
 RecLen:=iRecLen;
end;

destructor tFieldAccessor.Done;
 begin
 Close( dat );
end;
procedure tFieldAccessor.Append( const D );
 var p : tRecord;
 begin
 Append( p, D );
end;

procedure tFieldAccessor.Append( out pos :tRecord; const D );
 var p : LongWord;
 begin
 p:=FileSize( dat ) div RecLen;
 pos:=p;
 OverWrite( pos, D );
end;

procedure tFieldAccessor.Read( var D; const pos :tRecord );
 begin
 if ((pos+1)*RecLen) > FileSize( dat ) then raise eRangeError.Create('Record is Not In File');
 Seek( dat, pos*RecLen);
 System.BlockRead( dat, D, RecLen);
end;

procedure tFieldAccessor.Delete( const pos :tRecord );
 var tmp :pointer;
 begin
 if ((pos+1)*RecLen) > FileSize( dat ) then raise eRangeError.Create('Record is Not In File');
 if ((pos+1)*RecLen) < FileSize(dat) then begin
  GetMem( tmp, RecLen );
  try
   Seek( dat, FileSize(dat) - (1*RecLen) );
   System.BlockRead(dat, tmp^, RecLen );
   self.OverWrite( pos, tmp^ );
  finally FreeMem( tmp, RecLen ); end;
 end;
 Seek(dat, FileSize(dat) - (1*RecLen) );
 Truncate(dat);
end;

procedure tFieldAccessor.OverWrite( const pos :tRecord; const D );
 begin
 Seek( dat, (pos*RecLen) );
 System.BlockWrite( dat, D, RecLen );
end;

procedure tFieldAccessor.Expand( const pos :tRecord );
 begin
 Seek( dat, ((pos+1)*RecLen) );
end;

procedure tFieldAccessor.LastPos( out pos :tRecord );
 begin
 Assert( FilePos( dat ) > 0 );
 Assert( (FilePos( dat ) mod RecLen)=0 );
 pos:=(FilePos( dat ) div RecLen)-1;
end;

procedure tFieldAccessor.Purge;
 begin
 Done;
 Erase( dat );
end;

constructor tRowList.Init( const Table :tTable );
 var path :tFileName;
 begin
 path:=
  Prefix + DirectorySeparator
  + Table + DirectorySeparator
 ;
 if not DirectoryExists(path) then ForceDirectories(path);
 EoF:=false;
 if FindFirst( Path + '*', faDirectory, Search )<>0 then Done;
end;

destructor tRowList.Done;
 begin
 if EoF then exit;
 EoF:=true;
 FindClose(Search);
end;

procedure tRowList.Read( out row :tRow );
 begin
  if EoF then raise eRangeError.Create('No More');
  while ((Search.Attr and faDirectory) = 0)or(Search.Name[1]='.')
   do if FindNext(Search)<>0 then begin
    Done;
    raise eRangeError.Create('No More');
   end;
  row:=Search.Name;
  if FindNext(Search)<>0 then Done;
end;

function tFieldAccessor.TotalCount :Cardinal;
 begin
 TotalCount:= FileSize( dat ) div RecLen;
end;

procedure tFieldAccessor.BlockRead( var Data; offset :Cardinal; count: Cardinal );
 begin
 if ((offset+count)*RecLen) > FileSize( dat ) then raise eRangeError.Create('Record is Not In File');
 Seek( dat, offset*RecLen);
 System.BlockRead( dat, Data, RecLen*count);
end;

procedure tFieldAccessor.BlockWrite( var Data; offset :Cardinal; count: Cardinal );
 begin
 Seek( dat, (offset*RecLen) );
 System.BlockWrite( dat, Data, RecLen*count );
end;

{
constructor tDbDataSet.Create(AOwner: TComponent);
 begin
 inherited Create(AOwner);
end;
}

procedure tDbDataSet.Open (const S: string );
 begin
 FilePathFull := DataBase.Prefix;
 OpenMode:=Dbf.omNormal;
 TableName:= S + '.dbf';
end;

procedure tDbDataSet.CreateTable;
 begin
 TableLevel := 7;
 inherited CreateTable;
end;


END.