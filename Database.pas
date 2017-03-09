unit Database;
{ database abstraction for brodnetd }
{$mode objfpc}
INTERFACE
uses cmem,ServerLoop,sysutils,ObjectModel,Classes;

{$define TOKYO}
{define GDBM}

type tDbSect=(dbObject,dbObjData,dbProfile,dbMisc='/');

function dbGet(sect:tDbSect; const key; ks:longword ): TCustomMemoryStream;

procedure dbSet(sect:tDbSect; const key; ks:longword; s:TCustomMemoryStream );
procedure dbSet(sect:tDbSect; const key; ks:longword; data:pointer; ds:longword );
procedure dbDelete(sect:tDbSect; const key; ks:longword );

var dbKeyList: array of record
    l,vl:longword;
    v:pointer;
  end;
procedure FreeKeyList;

IMPLEMENTATION
{$IFDEF GDBM}
uses gdbm;
var dbHandle:PGDBM_FILE;
{$ENDIF}
{$IFDEF TOKYO}
uses tcdb,ctypes;
var dbHandle:pTCADB;
{$ENDIF}

type TDbMemStream=class(TCustomMemoryStream)
    constructor Create(Ptr: Pointer; ASize: PtrInt);
    destructor Destroy; override;
end;

{libgdbm/tokyo uses libc malloc, so we must do free from libc}
procedure LibCFreeMem( P: pointer ); cdecl; external name 'free';

constructor TDbMemStream.Create(Ptr: Pointer; ASize: PtrInt);
  begin
  Inherited Create;
  SetPointer(ptr,asize);
end;

destructor TDbMemStream.Destroy;
  begin
  if assigned(Memory) then LibCFreeMem(Memory);
  SetPointer(nil,0);
  Inherited;
end;


function dbGet(sect:tDbSect; const key; ks:longword ): TCustomMemoryStream;
  {$IFDEF GDBM}
  var k,v:tDatum;
  begin
  k.dsize:=1+ks;
  k.dptr:=GetMem(k.dsize);
  k.dptr^:=char(sect);
  Move(key,(k.dptr+1)^,ks);
  v:=gdbm_fetch(dbhandle, k);
  FreeMem(k.dptr,k.dsize);
  if v.dptr=nil then v.dsize:=0;
  result.Init(v.dptr,v.dsize,v.dsize);
  {$ENDIF}
  {$IFDEF TOKYO}
  var k,v:pointer;
  var vs:cint;
  begin
  k:=GetMem(ks+1);
  byte(k^):=ord(sect);
  Move(key,(k+1)^,ks);
  v:=tcadbget(dbHandle, k,ks+1, @vs);
  FreeMem(k);
  if v=nil then vs:=0;
  result:=TDbMemStream.Create(v,vs);
  {$ENDIF}
end;

procedure dbSet(sect:tDbSect; const key; ks:longword; s:TCustomMemoryStream );
  {$IFDEF GDBM}
  var k,v:tDatum;
  begin
  k.dsize:=1+ks;
  k.dptr:=GetMem(k.dsize); try
  k.dptr^:=char(sect);
  Move(key,(k.dptr+1)^,ks);
  v.dptr:=s.base;
  v.dsize:=s.vlength;
  gdbm_store(dbhandle,k,v,GDBM_REPLACE);
  finally FreeMem(k.dptr) end;
  {$ENDIF}
  {$IFDEF TOKYO}
  var k:pointer;
  begin
  k:=GetMem(ks+1);
  byte(k^):=ord(sect);
  Move(key,(k+1)^,ks);
  tcadbput(dbHandle, k, ks+1, s.Memory, s.Size);
  FreeMem(k);
  {$ENDIF}
end;
  
procedure dbSet(sect:tDbSect; const key; ks:longword; data:pointer; ds:longword );
  {$IFDEF GDBM}
  var k,v:tDatum;
  begin
  k.dsize:=1+ks;
  k.dptr:=GetMem(k.dsize);
  k.dptr^:=char(sect);
  Move(key,(k.dptr+1)^,ks);
  v.dptr:=data;
  v.dsize:=ds;
  gdbm_store(dbhandle,k,v,GDBM_REPLACE);
  FreeMem(k.dptr);
  {$ENDIF}
  {$IFDEF TOKYO}
  var k:pointer;
  begin
  k:=GetMem(ks+1);
  byte(k^):=ord(sect);
  Move(key,(k+1)^,ks);
  tcadbput(dbHandle, k, ks+1, data, ds);
  FreeMem(k);
  {$ENDIF}
end;
procedure dbDelete(sect:tDbSect; const key; ks:longword );
  {$IFDEF GDBM}
  var k:tDatum;
  begin
  k.dsize:=1+ks;
  k.dptr:=GetMem(k.dsize);
  k.dptr^:=char(sect);
  Move(key,(k.dptr+1)^,ks);
  Move(key,(k.dptr+1)^,ks);
  gdbm_delete(dbhandle,k);
  FreeMem(k.dptr);
  {$ENDIF}
  {$IFDEF TOKYO}
  var k:pointer;
  begin
  k:=GetMem(ks+1);
  byte(k^):=ord(sect);
  Move(key,(k+1)^,ks);
  tcadbout(dbHandle, k, ks+1);
  FreeMem(k);
  {$ENDIF}
end;
procedure LoadListOfKeys;
  {$IFDEF GDBM}
  unimplemented;
  var k,kn,v:tDatum;
  var count:LongWord;
  var i:LongInt;
  begin
  {count:=gdbm_count;
  k:=gdbm_firstkey(dbhandle);
  while assigned(k.dptr) do begin
    v:=gdbm_fetch(dbhandle, k);

    kn:=gdbm_nextkey(dbhandle, k);
    FreeMem(v.dptr,v.dsize);
    FreeMem(k.dptr,k.size);
    k:=kn;
  end;}
  SetLength(dbKeyList,0);
  {$ENDIF}
  {$IFDEF TOKYO}
  var count:LongWord;
  var i:LongInt;
  begin
  count:=tcadbrnum(dbHandle);
  tcadbiterinit(dbHandle);
  SetLength(dbKeyList,count);
  for i:=0 to count-1 do begin
    dbKeyList[i].v:=tcadbiternext(dbHandle, @dbKeyList[i].l);
    dbKeyList[i].vl:=tcadbvsiz(dbHandle, dbKeyList[i].v, dbKeyList[i].l);
  end;
  {$ENDIF}
end;

procedure FreeKeyList;
  var i:longint;
  begin
  for i:=0 to length(dbKeyList)-1 do begin
    LibCFreeMem(dbKeyList[i].v);
  end;
end;

{$IFDEF GDBM}
{$WARNING Implement mutex-locking for gdbm access}
const filename='db1.gdbm';
INITIALIZATION
  dbhandle:=gdbm_open(filename,0, GDBM_WRCREAT or GDBM_FAST, $1B4, nil);
  if dbhandle=nil then begin
    writeln('Database: failed to open or create gdbm file');
    writeln(gdbm_strerror(gdbm_errno));
    halt(1);
  end {else writeln('Database: GDBM '+filename)};
FINALIZATION
  gdbm_close(dbhandle);
{$ENDIF}
{$IFDEF TOKYO}
const filename='db1.tcb';
INITIALIZATION
  dbhandle:=tcadbnew;
  if (dbhandle=nil) or (tcadbopen(dbhandle,filename)=false) then begin
    raise exception.createfmt ('Database: failed to open or create TOKYO %S file',[filename]);
    //writeln(gdbm_strerror(gdbm_errno)); TODO
    halt(1);
  end;
  LoadListOfKeys;
  log1.LogMessage('Database',etInfo,' TOKYO %S %D records',[filename,Length(dbKeyList)]);
FINALIZATION
  tcadbclose(dbhandle);
  tcadbdel(dbhandle);
{$ENDIF}
END.
