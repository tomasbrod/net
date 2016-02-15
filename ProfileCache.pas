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
  var fleft,hbs:LongWord;
  begin
  AssignObject(f,fid);
  Reset(f,1);Seek(f,cObjHeaderSize);
  fleft:=FileSize(f)-cObjHeaderSize;
  result:=fleft>=(Sizeof(ph)+Sizeof(sig));
  if not result then exit;
  writeln('fsok');
  BlockRead(f,ph,sizeof(ph));
  result:=(CompareByte(ph.Magic,pfHeader,4)=0);
  if not result then exit;
  writeln('hdrok');
  fleft:=fleft-Sizeof(ph);
  Sha512Init(hash);
  Sha512Update(hash,ph,sizeof(ph));
  while true do begin
    hbs:=fleft-sizeof(tProfileSig);
    if fleft<=sizeof(tProfileSig) then break;
    if hbs>sizeof(buf) then hbs:=sizeof(buf);
    BlockRead(f,buf,hbs);
    Sha512Update(hash,buf,hbs);
    fleft:=fleft-hbs;
  end;
  BlockRead(f,sig,sizeof(sig));
  result:=(sig.Len1=0)and(sig.Len2=65)and(sig.tag=pfSig);
  if not result then exit;
  writeln('sighok');
  result:=ed25519.Verify2(hash, sig.sig, ph.LoginPub);
end;


BEGIN
  db.Init('prof.dat',sizeof(tProfDesc), 128);
  writeln('ProfileCache: Database initialized, valsz=',db.valsz,' bktsz=',db.bucksz);
END.
