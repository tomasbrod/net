UNIT ProfCache;
{

}
{ New mutable:
  used only for (user,group) profiles
  metadata stored in files
    1) profile meta (fixed-length)
    2) n*[NodeRef] for something
  updater in separate unit


  unit ProfCache  (store)
  unit ProfUpdate (search and fetch and publish)
  unit Profile    (read (write?) profile files)
}

INTERFACE
USES MemStream,NetAddr,Store2;
type tFID=Store2.tFID;
     tProfileID=tFID;
type tMutableMeta=packed record {size=32B}
  Magic: array [1..8] of char;
  Ver:Word4;
  Fid:tFID;
  end;
const cProfDir='prof/';
const cProfMagic:array [1..8] of char='BNProMe'#26;

{result = profile file valid}
function SetMutable( var so:tStoreObject; out id: tProfileID ): boolean;
function SetMutable( var so:tStoreObject; out ID: tProfileID; out Ver: LongWord ): boolean;
{result = found}
function GetMutable( id: tFID; out FID: tFID; out Ver: LongWord ): boolean;
function GetMutable( id: tFID; out FID: tFID         ): boolean;
function GetMutable( id: tFID; out so:  tStoreObject ): boolean; experimental;

IMPLEMENTATION
USES sha512,ed25519,SysUtils;

function GetMutable( id: tFID; out FID: tFID; out Ver: LongWord ): boolean;
  var Meta:tMutableMeta;
  var f:file of byte;
  begin
  result:=false;
  Assign(f,cProfDir+string(id));
  {$I-}
  ReSet(f);
  if IOResult>0 then exit;
  BlockRead(f,Meta,sizeof(Meta));
  if IOResult>0 then begin writeln('ProfCache: truncated metadata file '+string(id)); exit end;
  {$I+}
  if CompareByte(Meta.Magic,cProfMagic,8)<>0 then begin writeln('ProfCache: invalid signature in metadata file '+string(id)); exit end;
  result:=true;
  FID:=Meta.FID;
  Ver:=Meta.Ver;
end;

function SetMutable( var so:tStoreObject; out meta:tMutableMeta ): boolean;
  {no-op if same file ID}
  {full parse of profile file using Profile to validate format}
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
      meta.fid:=mid;
      result:= true; exit end;
  end;
  {hash for signature check}
  Sha512Init(hash);
  Sha512Update(hash,ph,64);
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
  meta.fid:=mid;
  result:=true;
end;


(*overloads*)
function SetMutable( var so:tStoreObject; out   id: tProfileID ): boolean;
  var Ver: LongWord;
  begin
  result:=SetMutable(so,id,ver);
end;
function GetMutable( id: tFID; out fid: tFID ): boolean;
  var Ver: LongWord;
  begin
  result:=GetMutable(id,fid,ver);
end;
function GetMutable( id: tFID; out so:  tStoreObject ): boolean;
  var Ver: LongWord;
  var fid: tFID;
  begin
  result:=GetMutable(id,fid,ver);
  if result then so.Init(fid);
end;

BEGIN
  CreateDir(cProfDir);
END.
