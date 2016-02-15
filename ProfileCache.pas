UNIT ProfileCache;
INTERFACE
USES MemStream,NetAddr,Store2;

function CacheProfile( fid: tFID ): boolean;

IMPLEMENTATION
USES HKVS,sha512,ed25519,Profile;
var db:tHKVS;
type tProfDesc=packed record
  fid:tFID;
  UpdateDay:Word4;
  UpdateCnt:byte;
  pad: packed array [1..7] of byte;
  end;

function CacheProfile( fid: tFID ): boolean;
  var f:file of byte;
  var ph:tProfileHeader;
  var hash:tSha512Context;
  var buf:packed array [0..1023] of byte;
  var sig:tProfileSig absolute buf;
  var fleft,hbs:LongInt;
  begin
  result:=false;
  Sha512Init(hash);
  AssignObject(f,fid);
  Reset(f,1);
  fleft:=FileSize(f)-(Sizeof(ph)+cObjHeaderSize+sizeof(sig));
  if fleft<0 then exit;
  Seek(f,cObjHeaderSize);
  BlockRead(f,ph,sizeof(ph));
  if CompareByte(ph.Magic,pfHeader,4)<>0 then exit;
  Sha512Update(hash,ph,sizeof(ph));
  while fleft>0 do begin
    hbs:=fleft;
    if hbs>sizeof(buf) then hbs:=sizeof(buf);
    BlockRead(f,buf,hbs);
    Sha512Update(hash,buf,hbs);
    fleft:=fleft-hbs;
  end;
  BlockRead(f,sig,sizeof(sig));
  if (sig.Len1<>0)or(sig.Len2<>65)or(sig.tag<>pfSig) then exit;
  result:=ed25519.Verify2(hash, sig.sig, ph.LoginPub);
end;


BEGIN
  db.Init('prof.dat',sizeof(tProfDesc), 128);
  writeln('ProfileCache: Database initialized, valsz=',db.valsz,' bktsz=',db.bucksz);
END.
