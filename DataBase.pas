unit DataBase;

INTERFACE

TYPE

tTable = string[15];
tRow = string[31];
tField = string[15];

var Prefix :string ='./data';
{TODO: extract from parameters }

const FieldExtension :string='dat';

procedure Open( var F :File; const Table :tTable; const Row :tRow; const Field :tField );

IMPLEMENTATION
uses SysUtils;

procedure Open( var F :File; const Table :tTable; const Row :tRow; const Field :tField );
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

END.