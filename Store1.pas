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
 procedure SegSeek(ofs:LongWord); unimplemented;
 procedure ReadAhead(cnt:Word; into:pointer);
 procedure WaitRead; {wait for read to finish, rc}
 procedure EnableWrite(const fid:tFID);
 procedure SetLength(len:LongWord);
 procedure WriteSeg(ofs:LongWord;len:word;data:pointer);
 procedure GetMiss(out ofs:LongWord; out len:LongWord; var state:pointer);
 procedure GetMiss(out ofs:LongWord; out len:LongWord);
 private
 dh:tHandle; {handle to the data file}
 filename:string[80];
 segi:pointer{to seg info obj};
end;
tObjectInfo=tStoreObjectInfo;

{Should consult Download on non-final files}

IMPLEMENTATION
const prefix='object';

type
tSegStatic=packed object
 first,after:LongWord;
 end;
tSeg_ptr=^tSeg;
tSeg=object(tSegStatic)
 next:tSeg_ptr;
 end;
tSegInfo_ptr=^tSegInfo;
pSegInfo=tSegInfo_ptr;
tSegInfo=object
 cache:^tSeg;
 name:tFID;
 refc:byte;
 next:tSegInfo_ptr;
 procedure SetSeg(ofs,len:LongWord; state:boolean);
 function GetSegLen(ofs:LongWord):LongWord;
 procedure Free;
 end;
var SegInfoChain:^tSegInfo;


procedure mkfilen(var d:string; flag:char; const fid:tfid);
 function hc(b:byte):char;
  begin
  if b<10 then hc:=char(ord('0')+b)
          else hc:=char(ord('A')-10+b);
  end;
 var b,i:byte;
 begin
 d:=prefix+flag+'/';
 b:=system.length(d);
 SetLength(d,b+40);
 inc(b);
 for i:=0 to 19 do begin
  d[b+(i*2)]:=hc(fid[i] shr 4);
  d[b+(i*2)+1]:=hc(fid[i] and $F);
 end;
end;

function GetSegInfo(const fid:tFID):tSegInfo_ptr;
 var fn:string;
 var fh:file of tSegStatic;
 var cp:^tSeg;
 label nocr;
 begin
 result:=SegInfoChain;
 while assigned(result) do begin
  if CompareWord(result^.name,fid,10)=0 then goto nocr;
  result:=result^.next;
 end;
 mkfilen(fn,'i',fid);
 new(result);
 with result^ do begin
  cache:=nil;
  name:=fid;
  refc:=0;
  next:=nil;
  SegInfoChain:=result;
  Assign(fh,fn);
  {$I-}ReSet(fh);{$I+}if ioresult=0 then begin
   while not eof(fh) do begin
    new(cp);
    read(fh,cp^);
    cp^.next:=cache;
    cache:=cp;
   end;
   close(fh);
  end;
 end;
 nocr:
 Inc(result^.refc);
end;
 
procedure tStoreObjectInfo.Open(const fid:tfid);
 begin
 mkfilen(filename,'f',fid);
 segi:=nil;
 dh:=FileOpen(filename,fmOpenRead or fmShareDenyWrite);
 if dh<>-1 then begin
  rc:=0;
  final:=true;
  length:=FileSeek(dh,0,fsFromEnd);
  FileSeek(dh,0,fsFromBeginning);
 end else begin
  mkfilen(filename,'p',fid);
  final:=false;
  dh:=FileOpen(filename,fmOpenRead or fmShareDenyWrite);
  if dh<>-1 then begin
   rc:=0;
   final:=false;
   length:=FileSeek(dh,0,fsFromEnd);
   FileSeek(dh,0,fsFromBeginning);
   segi:=GetSegInfo(fid);
  end else begin
   Writeln('Store1: open failed for file ',filename,', ioresult=',IOResult);
   rc:=2;
  end;
 end;
end;

procedure tStoreObjectInfo.EnableWrite(const fid:tFID);
 begin
 assert((dh=-1)or(not final));
 if dh=-1 then begin
  {file was close, create}
  dh:=FileCreate(filename);
  {init length and segments}
  length:=0;
  segi:=GetSegInfo(fid);
 end;
 if dh<>-1 then begin
  {file was open, close and reopen rw}
  FileClose(dh);
  dh:=FileOpen(filename,fmOpenReadWrite or fmShareDenyWrite);
 end;
 if dh=-1 then rc:=2 else rc:=0;
end;
procedure tStoreObjectInfo.SetLength(len:LongWord);
 begin
 assert( (length=0)and(not final)and(dh<>-1) );
 length:=len;
 {todo: errors!!!}
 FileSeek(dh,len,fsFromBeginning);
 FileSeek(dh,0,fsFromBeginning);
end;
procedure tSegInfo.SetSeg(ofs,len:LongWord; state:boolean);
 var cp:^tSeg;
 var pcp:^pointer;
 var after:LongWord;
 begin
 assert(state);
 after:=ofs+len;
 pcp:=@cache;
 cp:=cache;
 while assigned(cp) do begin
  if cp^.after=ofs then begin
   {merge left-matching}
   pcp^:=cp^.next;
   ofs:=cp^.first;
   dispose(cp);
   cp:=pcp^;
   continue;
  end;
  if cp^.first=after then begin
   {merge right-matching}
   pcp^:=cp^.next;
   after:=cp^.after;
   dispose(cp);
   cp:=pcp^;
   continue;
  end;
  pcp:=@cp^.next;
  cp:=pcp^;
 end;
 {add the merged seg}
 new(cp);
 cp^.first:=ofs;
 cp^.after:=after;
 cp^.next:=cache;
 cache:=cp;
end;
procedure tStoreObjectInfo.WriteSeg(ofs:LongWord;len:word;data:pointer);
 begin
 {todo: errors!!!}
 FileSeek(dh,ofs,fsFromBeginning);
 FileWrite(dh,data^,len);
 tSegInfo(segi^).SetSeg(ofs,len,true);
end;
procedure tStoreObjectInfo.GetMiss(out ofs:LongWord; out len:LongWord; var state:pointer);
begin
with tSegInfo(segi^) do begin
 assert(false);
end;
end;
procedure tStoreObjectInfo.GetMiss(out ofs:LongWord; out len:LongWord);
 var state:pointer;
 begin
 state:=nil;
 GetMiss(ofs,len,state);
end;


procedure tStoreObjectInfo.ReadAhead(cnt:Word; into:pointer);
 var red:LongWord;
 begin
 //todo, do real async read
 assert(seglen>=cnt);
 red:=FileRead(dh,into^,cnt);
 seglen:=seglen-red;
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
procedure tSegInfo.Free;
 var fn:string;
 var fh:file of tSegStatic;
 var cp:^tSeg;
 begin
 Dec(refc); if refc>0 then exit;
 {save segs, free segs, free}
 mkfilen(fn,'i',name);
 Assign(fh,fn);
 ReWrite(fh);
 while assigned(cache) do begin
  cp:=cache;
  write(fh,cp^);
  cache:=cp^.next;
  dispose(cp);
 end;
 FreeMem(@self,sizeof(self));
end;
procedure tStoreObjectInfo.Close;
 begin
 if assigned(segi) then tSegInfo(segi^).Free;
 FileClose(dh);
end;

function tSegInfo.GetSegLen(ofs:LongWord):LongWord;
 var cp:^tSeg;
 begin
 cp:=cache;
 while assigned(cp) do begin
  if (cp^.first<=ofs)and(cp^.after>ofs) then begin
   GetSegLen:=cp^.after-ofs;
  exit end;
  cp:=cp^.next;
 end;
 GetSegLen:=0;
end;
procedure tStoreObjectInfo.SegSeek(ofs:longword);
 begin
 if final then begin
  if ofs<=length then begin
   seglen:=length-ofs;
   FileSeek(dh,ofs,fsFromBeginning);
   rc:=0;
  end else rc:=5;
 end else if assigned(segi) then begin
  seglen:=tSegInfo(segi^).GetSegLen(ofs);
  if seglen=0 then rc:=4 else if FileSeek(dh,ofs,fsFromBeginning)<>ofs then rc:=3 else rc:=0;
 end else rc:=7;
end;

BEGIN
 SegInfoChain:=nil;
END.
