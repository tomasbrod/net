UNIT ProfCache;
{

}
{
  unit ProfCache  (store)
  unit ProfUpdate (search and fetch and publish)
  unit Profile    (read (write?) profile files)
}

INTERFACE
USES MemStream,NetAddr,Store2;
type tFID=Store2.tFID;
     tProfileID=tFID;
     tProfID=tFID;
const cProfDir=4;

{result = profile file valid}
function SetProf( var so:tStoreObject; out id: tProfileID ): boolean;
function SetProf( var so:tStoreObject; out ID: tProfileID; out Ver: LongWord ): boolean;
{result = found}
function GetProf( id: tFID; out FID: tFID; out Ver: LongWord ): boolean;
function GetProf( id: tFID; out FID: tFID         ): boolean;
function GetProf( id: tFID; out so:  tStoreObject ): boolean; experimental;

IMPLEMENTATION
USES sha512,ed25519,SysUtils,Profile,Database;

function GetProf( id: tFID; out FID: tFID; out Ver: LongWord ): boolean;
  begin
  result:=false;
  with dbGet(cProfDir,id,20) do if Length>0 then begin
    Read(FID,20);
    Ver:=ReadWord4;
    result:=true;
    Free;
  end;
end;

function SetMutable( var so:tStoreObject; out ID: tProfileID; out Ver: LongWord ): boolean;
  {no-op if same file ID}
  {full parse of profile file using Profile to validate format}
  var de:tProfileRead;
  var pubkey:tKey32;
  var isold:boolean;
  var OldFid:tFID;
  var OldVer,NewVer,hbs:LongWord;
  var buf:array [0..511] of byte;
  var hash:tSha512context;
  var signature:tKey64 absolute buf;
  var meta:tMutableMeta absolute buf;
  var pID:tFID absolute meta.fid;
  var f:file of tMutableMeta;
  begin
  result:=false;
  de.Init; de.blk:=@so; de.Init2;
  de.Select(pfLogin);
  de.Read(Pubkey,32);
  de.Read(NewVer,4); NewVer:=BEtoN(NewVer);
  SHA512Buffer(Pubkey,32,pID,sizeof(pID));
  isold:=GetMutable(pID,OldFid,OldVer);
  if isold then begin
    if (OldVer>=NewVer)or(pID=so.fid) then begin
      ID:=pID; Ver:=OldVer;
      result:=true;
    exit end;
  end;
  
  {hash for signature check}
  Sha512Init(hash);
  so.Seek(0);
  while so.left>0 do begin
    hbs:=so.left;
    if hbs>sizeof(buf) then hbs:=sizeof(buf);
    so.Read(buf,hbs);
    Sha512Update(hash,buf,hbs);
  end;
  
  {load signature}
  de.Select(pfSignature);
  de.Read(signature,64);
  if not ed25519.Verify2(hash, signature, pubkey) then exit;
  
  {update db if all checks passed}
  meta.FID:=so.fid;
  meta.Ver:=NewVer;
  meta.Magic:=cProfMagic;
  Assign(f,cProfDir+string(id));
  {$I-}ReSet(f);{$I+} if IOResult>0 then ReWrite(f);
  Write(f,meta); Close(f);
  
  {reference the new object and dereference the old one}
  if isold then Store2.Reference(oldfid,-1);
  so.Reference(+1);
  ID:=pID;
  Ver:=NewVer;
  RESULT:=TRUE;
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
