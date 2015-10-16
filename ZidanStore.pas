unit ZidanStore;

INTERFACE

type
tfid=array [0..20] of byte;
tStoreObjectInfo=object
 hnd:file of byte;
 final:boolean;
 rc:Word;
 length:LongWord;
 seglen:longword;
 
 procedure Open(const fid:tfid);
 procedure Close;
 procedure SegSeek(ofs:LongWord);
end;

IMPLEMENTATION
procedure tStoreObjectInfo.Open(const fid:tfid);
 begin
 rc:=0;
 final:=true;
 length:=65000;
 Assign(hnd,'/dev/urandom');
 Reset(hnd);
end;
procedure tStoreObjectInfo.Close;
 begin
end;
procedure tStoreObjectInfo.SegSeek(ofs:longword);
 begin
 if ofs=0 then begin
 rc:=0;
 seglen:=65000;
 end else rc:=7;
end;

END.
 