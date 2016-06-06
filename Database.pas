unit Database;
{ database abstraction for brodnetd }
{$mode objfpc}
INTERFACE
uses cmem,MemStream;

{define TOKYO}
{define GDBM}

type tDbMemStream=object(tMemoryStream)
    procedure Free; virtual;
end;
type tDbSect=byte;

function dbGet(sect:tDbSect; const key; ks:longword ): tDbMemStream;

procedure dbSet(sect:tDbSect; const key; ks:longword; s:tMemoryStream );
procedure dbSet(sect:tDbSect; const key; ks:longword; data:pointer; ds:longword );
procedure dbDelete(sect:tDbSect; const key; ks:longword );

IMPLEMENTATION
{$IF not (defined(TOKYO) xor defined(GDBM))}
{$DEFINE TOKYO}
{$WARNING Using TokyoCabinet backend}
{$NOTE Define TOKYO for TokyoCabinet backend _or_ GDBM for GNU 'dbm' backend}
{$ENDIF}
{$IFDEF GDBM}
uses gdbm;
var dbHandle:PGDBM_FILE;
{$ENDIF}
{$IFDEF TOKYO}
uses tcdb,ctypes;
var dbHandle:pTCADB;
{$ENDIF}

{libgdbm/tokyo uses libc malloc, so we must do free from libc}
procedure LibCFreeMem( P: pointer ); cdecl; external name 'free';

procedure tDbMemStream.Free;
  begin
  LibCFreeMem(base);
end;


function dbGet(sect:tDbSect; const key; ks:longword ): tDbMemStream;
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
  byte(k^):=sect;
  Move(key,(k+1)^,ks);
  v:=tcadbget(dbHandle, k,ks+1, @vs);
  //FreeMem(k);
  if v=nil then vs:=0;
  result.Init(v,vs,vs);
  {$ENDIF}
end;

procedure dbSet(sect:tDbSect; const key; ks:longword; s:tMemoryStream );
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
  byte(k^):=sect;
  Move(key,(k+1)^,ks);
  tcadbput(dbHandle, k, ks+1, s.base, s.vlength);
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
  byte(k^):=sect;
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
  byte(k^):=sect;
  Move(key,(k+1)^,ks);
  tcadbout(dbHandle, k, ks+1);
  FreeMem(k);
  {$ENDIF}
end;
{$IFDEF GDBM}
const filename='dmeta.gdbm';
INITIALIZATION
  dbhandle:=gdbm_open(filename,0, GDBM_WRCREAT or GDBM_FAST, $1B4, nil);
  if dbhandle=nil then begin
    writeln('Database: failed to open or create gdbm file');
    writeln(gdbm_strerror(gdbm_errno));
    halt(1);
  end else writeln('Database: opened '+filename+' using GDBM.');
FINALIZATION
  gdbm_close(dbhandle);
{$ENDIF}
{$IFDEF TOKYO}
const filename='dmeta.tcb';
INITIALIZATION
  dbhandle:=tcadbnew;
  if (dbhandle=nil) or (tcadbopen(dbhandle,filename)=false) then begin
    writeln('Database: failed to open or create TOKYO file');
    //writeln(gdbm_strerror(gdbm_errno)); TODO
    halt(1);
  end else writeln('Database: opened '+filename+' using TOKYO.');
FINALIZATION
  tcadbclose(dbhandle);
  tcadbdel(dbhandle);
{$ENDIF}
END.