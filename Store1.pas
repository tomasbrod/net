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
 offset:LongWord;  {only valid when reading}
 name:tFID;
 
 procedure Open(const fid:tfid);
 procedure Close;
 procedure ReadSeg(into:pointer; ofs:LongWord; len:word);
 function  SegmentLength(ofs:LongWord): LongWord;
 procedure GetSegAfter(ofs:LongWord; out base:LongWord; out limit:LongWord);
 procedure EnableWrite(const fid:tFID);
 procedure SetFLength(len:LongWord);
 procedure WriteSeg(ofs:LongWord;len:word;data:pointer);
 procedure VerifyAndReset;
 procedure GetMiss(out ofs:LongWord; out len:LongWord; var state:pointer); unimplemented;
 procedure GetMiss(out ofs:LongWord; out len:LongWord); deprecated;
 private
 dh:tHandle; {handle to the data file}
 filename:string[80];
 segi:pointer{to seg info obj};
 procedure SegSeek(ofs:longword); deprecated;
end;
tObjectInfo=tStoreObjectInfo;

operator :=(a:string) r:tFID;
operator :=(a:tFID) r:string;
{Should consult Download on non-final files}

IMPLEMENTATION
uses SHA1;

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
 finalized:boolean;
 procedure SetSeg(ofs,len:LongWord; state:boolean);
 function GetSegLen(ofs:LongWord):LongWord;
 procedure Free;
 end;
var SegInfoChain:^tSegInfo;

type tFileNameVar=(fvFinal,fvPart,fvInfo);
procedure mkfilen(var d:string; flag:tFileNameVar; const fid:tfid);
 function hc(b:byte):char;
  begin
  if b<10 then hc:=char(ord('0')+b)
          else hc:=char(ord('a')-10+b);
  end;
 var b,i:byte;
 begin
 d:='obj/';
 b:=system.length(d);
 SetLength(d,b+40);
 inc(b);
 for i:=0 to 19 do begin
  d[b+(i*2)]:=hc(fid[i] shr 4);
  d[b+(i*2)+1]:=hc(fid[i] and $F);
 end;
 case flag of
  fvPart: d:=d+'.prt';
  fvInfo: d:=d+'.seg';
  fvFinal:;
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
 mkfilen(fn,fvInfo,fid);
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
 mkfilen(filename,fvFinal,fid);
 segi:=nil;
 Offset:=0;
 name:=fid;
 dh:=FileOpen(filename,fmOpenRead or fmShareDenyWrite);
 if dh<>-1 then begin
  rc:=0;
  final:=true;
  length:=FileSeek(dh,0,fsFromEnd);
  FileSeek(dh,0,fsFromBeginning);
 end else begin
  mkfilen(filename,fvPart,fid);
  final:=false;
  dh:=FileOpen(filename,fmOpenRead or fmShareDenyWrite);
  if dh<>-1 then begin
   rc:=0;
   final:=false;
   length:=FileSeek(dh,0,fsFromEnd);
   FileSeek(dh,0,fsFromBeginning);
   segi:=GetSegInfo(fid);
   if tSegInfo(segi^).finalized then begin
    assert(length>0);
    final:=true;
   end;
  end else begin
   Writeln('Store1: open failed for file ',filename,', ioresult=',IOResult);
   rc:=2;
  end;
 end;
end;

procedure tStoreObjectInfo.EnableWrite(const fid:tFID);
 begin
 writeln('Store1: enaling write');
 assert((dh=-1)or(not final));
 if dh=-1 then begin
  {file was close, create}
  dh:=FileCreate(filename);
  if dh=-1 then begin
   Writeln('Store1: create failed for file ',filename,', ioresult=',IOResult);
   rc:=3; exit end;
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
procedure tStoreObjectInfo.SetFLength(len:LongWord);
 begin
 assert(not final);
 //writeln('Store1: SetFLength ',len);
 length:=len;
 {todo: errors!!!}
 FileSeek(dh,len,fsFromBeginning);
 FileSeek(dh,0,fsFromBeginning);
end;
procedure tSegInfo.SetSeg(ofs,len:LongWord; state:boolean);
 var cp:^tSeg;
 var pcp:^pointer;
 var after:LongWord;
 var op:boolean;
 procedure Dump(c:char);
  begin
  cp:=cache;
  writeln('Store1: dumpCache ',c,' ',LongWord(@self));
  while assigned(cp) do begin
   writeln(cp^.first,'-',cp^.after);
   cp:=cp^.next;
  end;
 end;
 begin
 assert(state);
 after:=ofs+len;
 //Dump('a');
 pcp:=@cache;
 cp:=cache;
 //writeln('Store1: Add: ',ofs,'-',after);
 while assigned(cp) do begin
  op:=false;
  if (ofs<=cp^.first)and(after>=cp^.after) then begin
   {merge complete-encase}
   pcp^:=cp^.next;
   dispose(cp);
   cp:=pcp^;
   continue;
  end;
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
  if (after>cp^.first)and(ofs<=cp^.first)and(after<=cp^.after) then begin writeln('k'); after:=cp^.first; end;
  if (ofs<cp^.after)and(after>=cp^.after)and(ofs>=cp^.first) then begin writeln('l');  ofs:=cp^.after;end;
  if not op then pcp:=@cp^.next;
  cp:=pcp^;
 end;
 //Dump('b');
 {add the merged seg}
 if ofs<>after then begin
 new(cp);
 cp^.first:=ofs;
 cp^.after:=after;
 cp^.next:=cache;
 cache:=cp;
 end;
 //Dump('c');
end;
procedure tStoreObjectInfo.WriteSeg(ofs:LongWord;len:word;data:pointer);
 begin
 {todo: errors!!!}
 FileSeek(dh,ofs,fsFromBeginning);
 FileWrite(dh,data^,len);
 tSegInfo(segi^).SetSeg(ofs,len,true);
end;
procedure tStoreObjectInfo.GetMiss(out ofs:LongWord; out len:LongWord; var state:pointer);
 var cp,cp1,cp2:^tSeg;
 begin with tSegInfo(segi^) do begin
 {find seg with lowest base, return 0..base-1}
 cp1:=nil; cp2:=nil;
 len:=0;
 ofs:=LongWord(state);
 cp:=cache; while assigned(cp) do begin
  if ((cp1=nil)or(cp^.first<cp1^.first))and(cp^.first>=ofs) then cp1:=cp;
 cp:=cp^.next; end;
 if assigned(cp1) then begin
  cp:=cache; while assigned(cp) do begin
   if ((cp2=nil)or(cp^.first<cp2^.first))and(cp^.first>cp1^.first)and(cp^.first>=ofs) then cp2:=cp;
  cp:=cp^.next; end;
  if assigned(cp2) then begin
   ofs:=cp1^.after;
   len:=cp2^.first-ofs;
  end else begin
   ofs:=cp1^.after;
   len:=self.length-ofs;
  end;
 end else len:=self.length-ofs;
 state:=pointer(ofs+len);
end;end;
procedure tStoreObjectInfo.GetMiss(out ofs:LongWord; out len:LongWord);
 var state:pointer;
 begin
 state:=nil;
 GetMiss(ofs,len,state);
end;


procedure tStoreObjectInfo.ReadSeg(into:pointer; ofs:LongWord; len:word);
 var red:LongWord;
 begin
 SegSeek(ofs);
 assert(seglen>=len);
 red:=FileRead(dh,into^,len);
 seglen:=seglen-red;
 offset:=offset+red;
 if red=len then rc:=0 else begin
  //todo
  writeln('Store1: read ',red,' out of ',len,' requested bytes');
  rc:=2;
 end;
end;
procedure tSegInfo.Free;
 var fh:file of tSegStatic;
 var cp:^tSeg;
 var on,nn:string;
 begin
 Dec(refc); if refc>0 then begin writeln('Not saving, ',refc); exit;end;
 {save segs, free segs, free}
 mkfilen(on,fvInfo,name);
 Assign(fh,on);
 writeln('Store1: Saving segment info');
 ReWrite(fh);
 while assigned(cache) do begin
  cp:=cache;
  if not finalized then write(fh,cp^);
  cache:=cp^.next;
  dispose(cp);
 end;
 Close(fh);
 if finalized then begin
  writeln('Store1: segi finalized, renaming datafile, erasing infofile');
  mkfilen(on,fvPart,name);
  mkfilen(nn,fvFinal,name);
  RenameFile(on,nn);
  Erase(fh);
 end;
 FreeMem(@self,sizeof(self));
end;
procedure tStoreObjectInfo.Close;
 begin
 if assigned(segi) then tSegInfo(segi^).Free;
 segi:=nil;
 FileClose(dh);
 dh:=-1;
end;
procedure tStoreObjectInfo.VerifyAndReset;
 var ctx:tSHA1Context;
 var digest:tSHA1Digest;
 var buf: array [1..2048] of byte;
 var red:Integer;
 begin
 SegSeek(0);
 if seglen<length then begin writeln('Not complete! ',length-seglen); exit;end;
 {if check segi... then exit};
 SHA1Init( ctx );
 while seglen>0 do begin
  red:=sizeof(buf);
  if red>seglen then red:=seglen;
  red:=FileRead(dh,buf,red);
  seglen:=seglen-red;
  if red<0 then exit; {todo}
  SHA1Update( ctx, buf, red );
 end;
 SHA1Final( ctx, digest );
 assert(sizeof(digest)=sizeof(tfid));
 if CompareWord(name,digest,10)=0 then begin
  writeln('Store1: hash match');
  {todo: mark final-verified in segi, rename on segi done}
  final:=true;
  assert(assigned(segi));
  with tSegInfo(segi^) do begin
   assert( (cache^.first=0) and (cache^.after=length) and (cache^.next=nil) );
   finalized:=true;
  end;
  Close;
  {set some invalid values to prevent doing anything}
  length:=0; {the object MUST be closed now} seglen:=0;
 end else writeln('Hash not matching ',sha1print(digest),' ',sha1print(name));
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
procedure tStoreObjectInfo.GetSegAfter(ofs:LongWord; out base:LongWord; out limit:LongWord);
 var cp:^tSeg;
 begin
 Assert(not final);
 cp:=tSegInfo(segi^).cache; {FIXME}
 while assigned(cp) do begin
  if (cp^.first>ofs) then begin
   base:=cp^.first;
   limit:=cp^.after-base-1;
  exit end;
  cp:=cp^.next;
 end;
end;
procedure tStoreObjectInfo.SegSeek(ofs:longword);
 begin
 if final then begin
  if ofs<=length then begin
   seglen:=length-ofs;
   if FileSeek(dh,ofs,fsFromBeginning)=ofs then begin
    offset:=ofs;
    rc:=0;
   end else rc:=3;
  end else rc:=5;
 end else if assigned(segi) then begin
  seglen:=tSegInfo(segi^).GetSegLen(ofs);
  if seglen=0 then rc:=4 else if FileSeek(dh,ofs,fsFromBeginning)<>ofs then rc:=3 else rc:=0;
  offset:=ofs;
 end else rc:=7;
end;
function tStoreObjectInfo.SegmentLength(ofs:LongWord): LongWord;
 begin
 if ofs>self.length then begin result:=0;exit end;
 if Final then result:=self.Length-ofs else begin
  result:=tSegInfo(segi^).GetSegLen(ofs);
 end;
end;

operator :=(a:string) r:tFID;
 var i:byte;
 function unhex(c:char):byte;
  begin
  c:=upcase(c);
  if (c<='F')and(c>='A') then unhex:=(ord(c)-ord('A'))+10
  else unhex:=ord(c)-ord('0');
 end;
 begin
 for i:=0 to 19 do r[i]:=(unhex(a[i*2+1])shl 4)or(unhex(a[i*2+2]));
end;
operator :=(a:tFID) r:string;
 begin
 r:=sha1print(a);
end;
BEGIN
 SegInfoChain:=nil;
END.
