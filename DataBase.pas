unit DataBase;

INTERFACE

TYPE

tTable = string[15];
tRow = string[31];
tField = string[15];

var Prefix :string ='./data';

const FieldExtension :string='dat';

procedure Open( var F :FileName; const Table :tTable; const Row :tRow; const Field :tField );

IMPLEMENTATION

procedure Open( var F :FileName; const Table :tTable; const Row :tRow; const Field :tField );
var path,name :tFileName;
begin
 path:=
  Prefix + DirectorySeparator
  + Table + DirectorySeparator
  + Row + DirectorySeparator
 name:=Field + ExtensionSeparator + Extension;
 {Create the file and directory if not exist}
 if not FileExists(path+name) then begin
  if not DirecotryExists(path) then Force Directories(path);
  FileClose(FileCreate(path+name));
 end;
 { And Assign the handle. Note: caller should reset() the file. }
 Assign( F, path + name);
end;

END.