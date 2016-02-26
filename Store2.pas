UNIT Store2;
{
 Object Manager. Reference Counting. Caching.
 Objects need a header?
}
INTERFACE
USES Sha512,MemStream;

type tFID=MemStream.tKey20;
const cObjHeaderSize=64;
const cObjDir='obj/';

procedure assignObject(var f:file; const id: tFID);
procedure assignTempObject(var f:file; const id: tFID; const ext:string);
procedure Reference(const id: tFID; adj: integer);

procedure HashObject(var f:file; out id:tFID);
function  HashObjectCheckID(var f:file; const id:tFID):boolean;

procedure HashObjectCopy(const fn:string; out id:tFID);

IMPLEMENTATION
USES NetAddr;
type tHeader=record
  case byte of
  1:(
    mark:packed array [1..4] of byte;
    RefCount:Word2;
  );
  0:(pad:packed array [1..cObjHeaderSize] of byte);
end;
const cMark:packed array [1..4] of byte=($42,$4E,$4F,$1A);

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

function CacheKeep(id:tFID; var f:file):boolean;
  unimplemented;
  begin  writeln('Store2: ref=0 '+string(id));  CacheKeep:=true;  end;

procedure Reference(const id: tFID; adj: integer);
  var h:tHeader;
  var f:file;
  begin
  if adj=0 then exit;
  assignObject(f,id);  reset(f,1);
  BlockRead(f,h,sizeof(h));
  adj:=adj+word(h.RefCount);
  if adj<0 then adj:=0;
  h.RefCount:=adj;
  h.mark:=cMark;
  Seek(f,0);  BlockWrite(f,h,sizeof(h));
  close(f);
  if (adj=0) and (not CacheKeep(id,f)) then Erase(f);
end;

procedure HashObject(var f:file; out id:tFID; var f2:file; icopy:boolean);
  var ctx:tSha512Context;
  var buf:packed array [0..1023] of byte;
  var red:integer;
  var newname:array [0..44] of char;
  var h:tHeader;
  begin
  Sha512Init(ctx);
  Seek(f,cObjHeaderSize);
  repeat
    if icopy then begin
      BlockRead(f2,buf,sizeof(buf),red);
      BlockWrite(f,buf,red);
    end else BlockRead(f,buf,sizeof(buf),red);
    Sha512Update(ctx,buf,red);
  until red<sizeof(buf);
  Sha512Final(ctx,id,length(id));
  FillWord(h.pad,sizeof(h.pad),$428E);
  h.RefCount:=1;
  h.mark:=cMark;
  Seek(f,0);
  BlockWrite(f,h,sizeof(h));
  Close(f);
  MKFN(id,@newname);
  Rename(f,@newname);
end;
procedure HashObject(var f:file; out id:tFID);
  begin
  HashObject(f,id,file(nil^),false);
end;

function  HashObjectCheckID(var f:file; const id:tFID):boolean;
  var realid:tFID;
  begin
  HashObject(f,realid,file(nil^),false);
  result:=CompareByte(realid,id,20)=0;
  //if not result then Erase(f);
end;

procedure HashObjectCopy(const fn:string; out id:tFID);
  var src,dst:file of byte;
  begin
  assign(src,fn);
  assign(dst,cObjDir+'hoc.tmp');
  reset(src);
  rewrite(dst);
  HashObject(dst,id,src,true);
  Close(src);
end;

END.
