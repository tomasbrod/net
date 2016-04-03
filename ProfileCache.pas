UNIT ProfileCache;
INTERFACE
USES MemStream,NetAddr,Store2;
type tProfID=tKey20;
     tProfileID=tProfID;
type tProfileMeta=packed record
  fid:tFID;
  Update:Word4;
  pad: packed array [1..8] of byte;
  end;

function CacheProfile( var so:tStoreObject; out id: tProfileID ): boolean;
function GetProfileMeta(id: tProfileID; out meta:tProfileMeta): boolean;

IMPLEMENTATION
USES opcode,ServerLoop,DHT,HKVS,sha512,ed25519,Profile;
var db:tHKVS;
type tProfDesc=tProfileMeta;

function CacheProfile( var so:tStoreObject; out id: tProfileID ): boolean;
  var ph:tProfileHeader;
  var hash:tSha512Context;
  var buf:packed array [0..1023] of byte;
  var oldfid:tFID;
  var oldis:boolean;
  var sig:tProfileSig absolute buf;
  var des:tProfDesc absolute buf;
  var fleft,hbs:LongInt;
  begin
  result:=false;
  so.Seek(0);
  if so.left<((Sizeof(ph)+sizeof(sig))) then exit;
  {read the header}
  so.Read(ph,sizeof(ph));
  if CompareByte(ph.Magic,pfHeader,4)<>0 then exit;
  {calculate id (loginpubhash)}
  Sha512Init(hash);
  Sha512Update(hash,ph.LoginPub,sizeof(ph.LoginPub));
  Sha512Final(hash,id,sizeof(id));
  {check if newer than db}
  oldis:=db.GetVal(id,des);
  if oldis then begin
    oldfid:=des.fid;
    if DWord(des.Update)>=DWord(ph.Update) then begin
      result:= true; exit end;
  end;
  {hash for signature check}
  Sha512Init(hash);
  Sha512Update(hash,ph,sizeof(ph));
  while so.left>sizeof(sig) do begin
    hbs:=so.left-sizeof(sig);
    if hbs>sizeof(buf) then hbs:=sizeof(buf);
    so.Read(buf,hbs);
    Sha512Update(hash,buf,hbs);
  end;
  {load signature}
  so.Read(sig,sizeof(sig));
  if (sig.Len1<>0)or(sig.Len2<>65)or(sig.tag<>pfSig) then exit;
  if not ed25519.Verify2(hash, sig.sig, ph.LoginPub) then exit;
  {update db if all checks passed}
  des.FID:=so.fid;
  des.Update:=ph.Update;
  db.SetVal(id,des);
  if oldis then Store2.Reference(oldfid,-1);
  {reference the new object and dereference the old one}
  so.Reference(+1);
  result:=true;
end;


function GetProfileMeta(id: tProfileID; out meta:tProfileMeta): boolean;
  begin
  result:=db.GetVal(id,meta);
end;

function CaphProfile(const source:tNetAddr; caps:byte; const Target:tPID; var extra:tMemoryStream):boolean;
  var r:tMemoryStream;
  var des:tProfDesc;
  begin
  write('ProfileCache.Cap: ',string(Target));
  result:=db.GetVal(Target,des);
  writeln(' ',result);
  assert(caps=capProfile);
  if result then begin
    r.Init(200);
    r.WriteByte(opcode.dhtCapable);
    r.Write(dht.MyID,20);
    r.Write(Target,20);
    r.WriteByte(caps);
    r.Write(des.FID,sizeof(des.FID));
    r.Write(des.Update,sizeof(des.Update));
    SendMessage(r.base^,r.length,source);
    FreeMem(r.base,r.size);
  end;
  result:=true;
end;

BEGIN
  db.Init('prof.dat',sizeof(tProfDesc), 128);
  writeln('ProfileCache: Database initialized, valsz=',db.valsz,' bktsz=',db.bucksz);
  dht.RegisterCapability(capProfile,@CapHProfile);
END.
