UNIT Mutable;
{

}
INTERFACE
USES MemStream,NetAddr,Store2;
type tFID=Store2.tFID;
     tProfileID=tFID;
type tMutableMeta=packed record
  Fid:tFID;
  Ver:Word4;
  pad: packed array [1..8] of byte;
  end;
{$I Mutable-header.pas}

function SetMutable( var so:tStoreObject; out   id: tFID        ): boolean;
function SetMutable( var so:tStoreObject; out meta:tMutableMeta ): boolean;
function GetMutable( id: tFID; out meta:tMutableMeta ): boolean;
function GetMutable( id: tFID; out fid: tFID         ): boolean;
function GetMutable( id: tFID; out so:  tStoreObject ): boolean; experimental;

type tMutator=object
  {
  search
  fetch
  list
  OnEvent:procedure (ev:tMemoryStream) of object;
  OnComplete:procedure of object;
  }
  b:byte;
  end;

IMPLEMENTATION
USES opcode,ServerLoop,DHT,HKVS,sha512,ed25519;
var db:tHKVS;

function SetMutable( var so:tStoreObject; out   id: tFID        ): boolean;
  var meta:tMutableMeta;
  begin
  result:=SetMutable(so,meta);
  id:=meta.Fid;
end;
function GetMutable( id: tFID; out fid: tFID         ): boolean;
  var meta:tMutableMeta;
  begin
  result:=GetMutable(id,meta);
  fid:=meta.fid;
end;
function GetMutable( id: tFID; out so:  tStoreObject ): boolean;
  var meta:tMutableMeta;
  begin
  result:=GetMutable(id,meta);
  if result then so.Init(meta.fid);
end;


function SetMutable( var so:tStoreObject; out meta:tMutableMeta ): boolean;
  var ph:tMutHdr;
  var hash:tSha512Context;
  var buf:packed array [0..1023] of byte;
  var oldfid:tFID;
  var oldis:boolean;
  var mid:tFID;
  var hbs:LongInt;
  begin
  result:=false;
  so.Seek(0);
  if so.left<Sizeof(ph) then exit;
  {read the header}
  so.Read(ph,sizeof(ph));
  if CompareByte(ph.Magic,cMutHdrMagic,4)<>0 then exit;
  {calculate id (loginpubhash)}
  Sha512Init(hash);
  Sha512Update(hash,ph.Pub,sizeof(ph.Pub));
  Sha512Final(hash,mid,sizeof(mid));
  {check if newer than db}
  oldis:=db.GetVal(mid,meta);
  if oldis then begin
    oldfid:=meta.fid;
    if DWord(meta.Ver)>=DWord(ph.Ver) then begin
      result:= true; exit end;
  end;
  {hash for signature check}
  Sha512Init(hash);
  Sha512Update(hash,ph,sizeof(ph));
  while so.left>0 do begin
    hbs:=so.left;
    if hbs>sizeof(buf) then hbs:=sizeof(buf);
    so.Read(buf,hbs);
    Sha512Update(hash,buf,hbs);
  end;
  {load signature}
  if not ed25519.Verify2(hash, ph.Sig, ph.Pub) then exit;
  {update db if all checks passed}
  meta.FID:=so.fid;
  meta.Ver:=ph.Ver;
  db.SetVal(mid,meta);
  if oldis then Store2.Reference(oldfid,-1);
  {reference the new object and dereference the old one}
  so.Reference(+1);
  result:=true;
end;


function GetMutable( id: tFID; out meta:tMutableMeta ): boolean;
  begin
  result:=db.GetVal(id,meta);
end;

function CaphProfile(const source:tNetAddr; caps:byte; const Target:tPID; var extra:tMemoryStream):boolean;
  var r:tMemoryStream;
  var des:tMutableMeta;
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
    r.Write(des.Ver,sizeof(des.Ver));
    SendMessage(r.base^,r.length,source);
    FreeMem(r.base,r.size);
  end;
  result:=true;
end;

BEGIN
  db.Init('prof.dat',sizeof(tMutableMeta), 128);
  writeln('ProfileCache: Database initialized, valsz=',db.valsz,' bktsz=',db.bucksz);
  dht.RegisterCapability(capProfile,@CapHProfile);
END.
