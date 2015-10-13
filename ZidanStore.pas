unit ZidanStore;

INTERFACE

type
tfid=array [0..20] of byte;
tStoreObjectInfo=object
 hnd:file of byte;
 final:boolean;
 rc:Word;
 length:LongWord;
 procedure Open(const fid:tfid);
end;

IMPLEMENTATION
procedure tStoreObjectInfo.Open(const fid:tfid);
 begin
end;

END.
 