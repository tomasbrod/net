UNIT Store2;
{
 Object Manager. Reference Counting. Caching.
 Objects need a header?
}
INTERFACE
USES Sha512,MemStream,SysUtils;

type tFID=MemStream.tKey20;
const cObjHeaderSize=64;
const cObjDir='obj/';
type tStoreObject=object(MemStream.tCommonStream)
  fid:tFID; {these vars are READ ONLY for "reasons"}
  small:boolean;
  base:LongWord;
  constructor Init(id: tFID);
  
  destructor  Close;
  procedure Reference(adj: integer);
  procedure Seek(abs:longword); virtual;
  function  Tell:LongWord; virtual;
  procedure Read(out blk; len:word); virtual;
  procedure Write(const blk; len:word); virtual; unimplemented;
  function  Length:LongWord; virtual;
  private
  handle:file of byte;{$note Use SysUtils file handle (faster)}
  vpos:LongWord;
  vlength:LongWord;
  end;
  tSObj=tStoreObject;
  eObjectNF=class(eXception)
  end;

procedure assignObject(var f:file; const id: tFID); deprecated;
procedure assignTempObject(var f:file; const id: tFID; const ext:string);
procedure Reference(const id: tFID; adj: integer);

//procedure HashObject2(var f:file; out id:tFID);
//function  HashObject2CheckID(var f:file; const id:tFID):boolean;

procedure HashObjectCopy(const fn:ansistring; out id:tFID);

IMPLEMENTATION
USES NetAddr,HKVS;
type tHeader=record
  case byte of
  1:(
    mark:packed array [1..4] of byte;
    RefCount:Word2;
  );
  0:(pad:packed array [1..cObjHeaderSize] of byte);
end;
const cMark:packed array [1..4] of byte=($42,$4E,$4F,$1A);
const cStoreMark:packed array [1..8] of char='BNStPAK'+char($1A);
const cStoreDat='storepak.dat';
type tDescr=packed record
  dbpart,kek:byte;
  Offset:Word4;
  Length:Word4;
  RefCount:Word2;
  pad:packed array [13..16] of byte;
  end;
var db:tHKVS;
var lock:tRTLCriticalSection;

procedure mkfn(const id: tFID; fn:pchar);
  begin
  Move(cObjDir[1],fn[0],4);
  BinToHex(@fn[4], id, 20);
  fn[44]:=#0;
end;

procedure assignObject(var f:file; const id: tFID);
  var fn:array [0..44] of char;
  begin
  mkfn(id,@fn);
  Assign(f, pchar(@fn));
end;

procedure assignTempObject(var f:file; const id: tFID; const ext:string);
  var fn:array [0..48] of char;
  begin
  mkfn(id,@fn);
  fn[44]:='.';
  fn[48]:=#0;
  Move(ext[1],fn[45],3);
  Assign(f, pchar(@fn));
end;

constructor tStoreObject.Init(id: tFID);
  var fn:array [0..44] of char;
  var des:tDescr ;
  var e2:word;
  begin
  Inherited Init;
  EnterCriticalSection(lock);
  small:=db.GetVal(id,des);
  LeaveCriticalSection(lock);
  if small then begin
    System.Assign(handle, cStoreDat);
    System.Reset(handle,1);
    base:=des.Offset;
    vlength:=des.Length;
  end else begin
    mkfn(id,@fn);
    System.Assign(handle, pchar(@fn));
    {$PUSH}{$I-}
    System.Reset(handle,1);
    e2:=IOResult; if e2=2 then raise eObjectNF.Create('Object not found')
    else if e2>0 then raise eInOutError.Create('unexpected error');
    base:=cObjHeaderSize;
    {$R+}
    vlength:=FileSize(handle)-base;
    {$POP}
  end;
  Seek(0);
  fid:=id;
end;
procedure tStoreObject.Reference(adj: integer);
  var hdr:tHeader;
  var des:tDescr;
  begin
  {$note refc may get below zero and then some refs may be lost}
  if small then begin
    EnterCriticalSection(lock);
    db.GetVal(fid,des);
    adj:=adj+Word(des.RefCount);
    if adj<0 then adj:=0;
    des.RefCount:=adj;
    db.SetVal(fid,des);
    LeaveCriticalSection(lock);
  end else begin
    System.Seek(handle,0);
    System.BlockRead(handle,hdr,sizeof(hdr));
    adj:=adj+word(hdr.RefCount);
    if adj<0 then adj:=0;
    hdr.RefCount:=adj;
    hdr.mark:=cMark;
    System.Seek(handle,0);
    System.BlockWrite(handle,hdr,sizeof(hdr));
    Seek(vpos);
  end;
end;
destructor tStoreObject.Close;
  begin System.Close(handle) end;
procedure tStoreObject.Seek(abs:longword);
  begin
  vpos:=abs;
  System.Seek(handle,Base+abs);
end;
procedure tStoreObject.Read(out blk; len:word);
  begin
  if (vpos+len)>vlength then raise eReadPastEoF.Create('Read past EoF');
  System.BlockRead(handle,blk,len);
  vpos:=vpos+len;
end;
function tStoreObject.Tell:LongWord;
  begin Tell:=vpos end;
function tStoreObject.Length:LongWord;
  begin Length:=vlength end;
procedure tStoreObject.Write(const blk; len:word);
  begin AbstractError end;
function CacheKeep(id:tFID; var f:file):boolean;
  unimplemented;
  begin  writeln('Store2: ref=0 '+string(id));  CacheKeep:=true;  end;

procedure Reference(const id: tFID; adj: integer);
  var so:tStoreObject;
  begin
  if adj=0 then exit;
  so.Init(id);
  so.Reference(adj);
  so.Close;
end;

procedure HashAndCopy(var inf,outf:file; out id:tFID; icopy:boolean);
  {read input file, copy to output file, hash to id, dont seek/rename/close}
  var ctx:tSha512Context;
  var buf:packed array [0..1023] of byte;
  var red:LongWord;
  begin
  Sha512Init(ctx);
  repeat
    BlockRead(inf,buf,sizeof(buf),red);
    if icopy then BlockWrite(outf,buf,red);
    Sha512Update(ctx,buf,red);
  until red<sizeof(buf);
  Sha512Final(ctx,id,length(id));
end;

procedure HashObjectCopy(const fn:ansistring; out id:tFID);
  var inf,outf:file of byte;
  var dlen,dofs:LongWord;
  var hdr:tHeader;
  var des:tDescr;
  begin
  Assign(inf,fn);
  Reset(inf,1);
  dlen:=FileSize(inf);
  if dlen<(4*1048576) then begin
    Assign(outf,cStoreDat);
    Reset(outf,1);
    dofs:=FileSize(outf);
    Seek(outf,dlen+dofs-1); BlockWrite(outf,dlen,1);{reserve space}
    Seek(outf,dofs);
    //write('Store2.HashObjectCopy: copying ',dlen,'B to blob at ',dofs,' ');
    HashAndCopy(inf,outf,id,true);
    EnterCriticalSection(lock);
    if db.GetVal(id,des) then begin
      //writeln('already in database');
      Seek(outf,dofs);Truncate(outf);
    end else begin
      FillChar(des,sizeof(des),0);
      des.Offset:=dofs;
      des.Length:=dlen;
      db.SetVal(id,des);
      //writeln('hash ',string(id));
    end;
    LeaveCriticalSection(lock);
  end else AbstractError {$note unimplemented};
end;

procedure InitBlob;
  var outf:file of byte;
  var mark:packed array [1..8] of byte;
  var res:word;
  begin
  Assign(outf,cStoreDat);
  {$I-}
  ReSet(outf,1); res:=IOResult;
  {$I+}
  if (res=2)or((res=0)and(FileSize(outf)=0)) then begin
    ReWrite(outf,1);
    BlockWrite(outf,cStoreMark,8);
    writeln('Store2: Initialized empty pack');
  end else if res>0 then raise eInOutError.Create('unexpected io error')
  else begin
    {$I-}BlockRead(outf,mark,8);{$I+}
    if (IOResult>0)or(CompareByte(mark,cStoreMark,8)<>0)
    then raise eXception.Create('Store Pack file Corrupt'); end;
  Close(outf);
end;

BEGIN
  InitCriticalSection(lock);
  db.Init('storeidx.dat',sizeof(tDescr), 256);
  writeln('Store2: Database initialized, valsz=',db.valsz,' bktsz=',db.bucksz);
  InitBlob;
END.
