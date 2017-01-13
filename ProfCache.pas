UNIT ProfCache;
{
  Associate profile ID to latest profile Object.
  Manage profile refcounts.
  Delete unused profiles.
}
{
  unit ProfCache  (store)
  unit ProfUpdate (search and fetch and publish)
  unit Profile    (read and write profile files)
}

INTERFACE
USES ObjectModel,Store2,Profile;
type tFID=Store2.tFID;
     tProfileID=tKey20;
     tProfID=tProfileID;

{
set v*
get v*
Query handler
lookup object
---
refcounting
auto publish
auto update
}

function GetProf( id: tProfID; out FID: tFID; out Ver: Int64 ): boolean;
function GetProf( id: tProfID; out FID: tFID         ): boolean;

procedure SetProf( var so: tStoreObject; const p: tProfileRead );

IMPLEMENTATION
USES SysUtils,Database,ServerLoop,DHT,opcode;

function GetProf( id: tFID; out FID: tFID; out Ver: Int64 ): boolean;
  begin
  result:=false;
  with dbGet(dbProfile,id,20) do if Length>0 then begin
    Read(FID,20);
    Ver:=ReadWord6;
    result:=true;
    Free;
  end;
end;

function GetProf( id: tProfID; out FID: tFID         ): boolean;
  var tmp:int64;
  begin
  GetProf:=GetProf(id,fid,tmp);
end;

procedure SetProf( var so: tStoreObject; const p: tProfileRead );
  var exver:int64;
  var exso:tStoreObject;
  var meta:tMemoryStream;
  begin
  assert(p.Valid=true);
  meta:=dbGet(dbProfile,p.ProfID,20);
   if meta.Length>0 then begin
    meta.Read(exso.FID,20);
    exVer:=meta.ReadWord6;
    meta.Free;
    if exVer < p.Updated then begin
      exso.Init(exso.fid);
      exso.Reference(-1);
    end else exit;
  end;
  meta.Init(26);
  meta.write(so.FID,20);
  meta.writeword6(p.Updated);
  dbSet(dbProfile,p.ProfID,20,meta);
  so.Reference(+1);
end;

procedure CheckProfiles;
  var li:integer;
  {var pid:tFID;}
  {var usageData: QWORD=0;}
  var usageMeta: QWORD=0;
  begin
  for li:=0 to length(dbKeyList)-1 do begin
    if tDbSect(byte(dbKeyList[li].v^))<>dbProfile then continue;
    {Move((dbKeyList[li].v+1)^,pid,20);}
    {}
    Inc(usageMeta,dbKeyList[li].vl+dbKeyList[li].l);
    {Inc(usageData,LongWord(Word4((dbKeyList[li].v+4)^)));}
  end;
  writeln('ProfCache.Usage: meta=',SizeToString(usageMeta),'B');
end;

procedure MaybeStartUpdatingProfile(const src: tNetAddr; const prid: tProfID);
  begin
end;

procedure QueryHandler(msg:tSMsg);
  var r:tMemoryStream;
  var trid:word;
  var sID,Target:^tPID;
  var sFID:^tFID;
  var lFID:tFID;
  var sVer,lVer:Int64;
  begin
  msg.st.skip(1);
  msg.st.read(trid,2);
  sID:=msg.st.ReadPtr(20);
  if not DHT.CheckNode(sID^, msg.source, true) then exit;
  Target:=msg.st.ReadPtr(20);
  sFID:=msg.st.ReadPtr(20);
  sVer:=msg.st.ReadWord6;
  writeln('ProfCache.QueryHandler: ',string(Target),' v',sVer);
  if GetProf( Target^, lFID, lVer )
    and (lVer > sVer)
  then begin
    r.Init(cDgramSz);
    r.WriteByte(opcode.profQuery);
    r.Write(trid,2);
    r.Write(lFID,20);
    r.WriteWord6(lVer);
    r.WriteByte(0{ttl,todo...});
    DHT.GetNodes(r,Target^,99);
    SendMessage(r.base^,r.length,msg.source);
    r.Free;
  end else begin
    MaybeStartUpdatingProfile(msg.Source,Target^);
    DHT.SendNodes(msg.source,Target^,trid);
  end;
end;

BEGIN
  CheckProfiles;
END.
