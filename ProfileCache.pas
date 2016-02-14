UNIT ProfileCache;
{
 
}
INTERFACE
USES MemStream,NetAddr,Store2;

function CacheProfile(fid:tFID);

IMPLEMENTATION
USES HKVS,sha512,ed25519,Profile;
var db:tHKVS;
type tProfDesc=packed record
  fid:tFID;
  UpdateDay:Word4;
  UpdateCnt:byte;
  pad: packed array [1..7] of byte;
  end;

function CacheProfile(fid:tFID);
  var f:file of byte;
  var ph:tProfileHeader;
  begin
  AssignObject(f,fid);
  Reset(f,1);
  BlockRead(f,ph,sizeof(ph));
  {$error}
end;


BEGIN
  db.Init('prof.dat',sizeof(tProfDesc), 128);
  writeln('ProfileCache: Database initialized, valsz=',db.valsz,' bktsz=',db.bucksz);
END.
