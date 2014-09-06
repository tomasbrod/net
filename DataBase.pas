unit DataBase;

INTERFACE
uses SysUtils;

TYPE

tTable = string[15];
tRow = string[63];
tField = string[15];
tRecord = Word;

tFieldAccessor=object
 constructor Init( const iRecLen :word; const Table :tTable; const Row :tRow; const Field :tField );
  experimental;
 destructor  Done;
 procedure Append( const D );
  experimental;
 procedure Read( var D; const pos :tRecord );
  experimental;
 procedure Delete( const pos :tRecord );
  experimental;
 procedure Purge;
  experimental;
 procedure OverWrite( const pos :tRecord; const D );
  experimental;
 procedure Expand( const pos :tRecord );
  experimental;
 procedure LastPos( out pos :tRecord );
  experimental;
 private
 RecLen :word;
 dat :file;
end;
tAccess=tFieldAccessor;

tRowList=object
 EoF :boolean;
 constructor Init( const Table :tTable );
  experimental;
 destructor Done;
 procedure Read( out row :tRow );
  experimental;
 private
 Search: SysUtils.tSearchRec;
end;

var Prefix :string ='./data';
{TODO: extract from parameters }

const FieldExtension :string='dat';

procedure Open( out F :File; const Table :tTable; const Row :tRow; const Field :tField );

procedure UnInsert ( var F :file );
 experimental;
{ deletes the record at current position -1 }

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
     BlockRead(F, Ex, 1);
     Seek(F, nwpos-1);
     {
      nwpos is position of record next of the oddending
      becouse this proc is called after the record to delete was read
     }
     BlockWrite(F, Ex, 1);
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

destructor  tFieldAccessor.Done;
 begin
 Close( dat );
end;
procedure tFieldAccessor.Append( const D );
 var pos :tRecord;
 begin
 pos:=FileSize( dat ) div RecLen;
 Expand( pos );
 OverWrite( pos, D );
end;

procedure tFieldAccessor.Read( var D; const pos :tRecord );
 begin
 if ((pos+1)*RecLen) > FileSize( dat ) then raise eRangeError.Create('Record is Not In File');
 Seek( dat, pos);
 BlockRead( dat, D, RecLen);
end;

procedure tFieldAccessor.Delete( const pos :tRecord );
 var tmp :pointer;
 begin
 if ((pos+1)*RecLen) > FileSize( dat ) then raise eRangeError.Create('Record is Not In File');
 if ((pos+1)*RecLen) < FileSize(dat) then begin
  GetMem( tmp, RecLen );
  try
   Seek( dat, FileSize(dat) -1 );
   BlockRead(dat, tmp, RecLen );
   self.OverWrite( pos, tmp );
  finally FreeMem( tmp, RecLen ); end;
 end;
 Seek(dat, FileSize(dat)-1);
 Truncate(dat);
end;

procedure tFieldAccessor.OverWrite( const pos :tRecord; const D );
 begin
 if ((pos+1)*RecLen) > FileSize( dat ) then raise eRangeError.Create('Record is Not In File');
 Seek( dat, (pos*RecLen) );
 BlockWrite( dat, D, RecLen );
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
 Close( dat );
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
 if FindFirst( Path, faDirectory, Search )<>0 then Done;
end;

destructor tRowList.Done;
 begin
 if EoF then exit;
 EoF:=true;
 FindClose(Search);
end;

procedure tRowList.Read( out row :tRow );
 begin
 try
  if EoF then raise eRangeError.Create('Reading Past End');
  while (Search.Attr<>faDirectory) do if FindNext(Search)<>0 then raise eAbort.Create('');
  row:=Search.Name;
  if FindNext(Search)<>0 then raise eAbort.Create('');
 except
  on eAbort do Done;
 end;
end;


END.