unit DataBase;

INTERFACE

TYPE

tTable = string[15];
tRow = string[31];
tField = string[15];

var Prefix :string ='./data';
{TODO: extract from parameters }

const FieldExtension :string='dat';

procedure Open( out F :File; const Table :tTable; const Row :tRow; const Field :tField );

procedure UnInsert ( var F :file );
 experimental;
{ deletes the record at current position -1 }

IMPLEMENTATION
uses SysUtils;

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


END.