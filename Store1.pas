UNIT Store1;
{Take tracks of files in store}
{just simple, no cleaning, etc}
INTERFACE
uses SysUtils;

type
tfid=array [0..19] of byte;
tStoreObjectInfo=object
 final:boolean;    {hash matched}
 rc:Word; {0=no error 1=not found, other}
 length:LongWord;  {the whole file}
 seglen:longword;  {from cur to end of segment}
 
 procedure Open(const fid:tfid);
 procedure Close;
 procedure SegSeek(ofs:LongWord);
 procedure ReadAhead(cnt:Word; into:pointer);
 procedure WaitRead; {wait for read to finish, rc}
 private
 dh:tHandle; {handle to the data file}
 filename:string[80];
 procedure mkfilen(var d:string; flag:char; const fid:tfid);
end;

IMPLEMENTATION
const prefix='object';

procedure tStoreObjectInfo.mkfilen(var d:string; flag:char; const fid:tfid);
 function hc(b:byte):char;
  begin
  if b<10 then hc:=char(ord('0')+b)
          else hc:=char(ord('A')-10+b);
  end;
 var b,i:byte;
 begin
 d:=prefix+flag+DirectorySeparator;
 b:=system.length(d);
 SetLength(d,b+40);
 for i:=0 to 19 do begin
  filename[b+(i*2)]:=hc(fid[i] shr 4);
  filename[b+(i*2)+1]:=hc(fid[i] and $F);
 end;
end;
procedure tStoreObjectInfo.Open(const fid:tfid);
 begin
 mkfilen(filename,'f',fid);
 dh:=FileOpen(filename,fmOpenRead);
 if dh<>-1 then begin
  rc:=0;
  final:=true;
  length:=1000;
 end else begin
  Writeln('Store1: open failed for file ',filename,', ioresult=',IOResult);
  rc:=2;
 end;
end;
procedure tStoreObjectInfo.ReadAhead(cnt:Word; into:pointer);
 var red:LongWord;
 begin
 //todo, do real async read
 red:=FileRead(dh,into^,cnt);
 if red=cnt then rc:=0 else begin
  //todo
  writeln('Store1: read ',red,' out of ',cnt,' requested bytes');
  rc:=2;
 end;
end;
procedure tStoreObjectInfo.WaitRead; {wait for read to finish, rc}
 begin
 //todo
end;
procedure tStoreObjectInfo.Close;
 begin
 FileClose(dh);
end;
procedure tStoreObjectInfo.SegSeek(ofs:longword);
 begin
 if ofs=0 then begin
 rc:=0;
 seglen:=length;
 end else rc:=7;
end;

END.
 