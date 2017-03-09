UNIT Store;
{
 Object Manager. Reference Counting. Caching.

}
INTERFACE
USES Crypto,Database,SysUtils,ServerLoop,Classes,StreamEX,ObjectModel;

type tFID=ObjectModel.tKey24;
type tFID_ptr=^tFID;
type tsoLocation = (solNormal=1, solInline=2,solReference=3);

function StoreOpen( id: tFID ): TStream;

type eObjectNF=CLASS(eFileNotFound)
end;

const cObjHeaderSize=0;
const cDebugPrintOpen=true;
const cInlineThr=8*1024;
const cStoreDir='obj/';

function GetTempName(const hash:tKey32; const ext:string): string;
function StoreExists(const id: tFID): boolean;
procedure GetBlobInfo(const id: tFID; out mode:byte; out path:Ansistring );
function IsBlob(const id: tFID): boolean; overload;
procedure StoreDelete(const id: tFID); unimplemented;

procedure InsertBlobMem( out id: tFID; ms: TCustomMemoryStream );
procedure InsertBlobRename( const name:string; var hash:tKey32 );
procedure InsertBlobRef( const name:string; var hash:tKey32 );

function ReadLink(const id: tFID): tFID;
function ReadLinkData(const id: tFID): TCustomMemoryStream;
procedure InsertLinkData(const id: tFID; s:TCustomMemoryStream);

IMPLEMENTATION
{$IFDEF UNIX}{for the stat() and permissions}
USES BaseUnix;
{$ENDIF}


{metadata format
  dbObject key format
    sect:b1, file-id:k20
  dbObject value format
    method:b1;
      *normal
        nothing?
      *inline
        data
      *reference
        filename
}


var opendebug:boolean;
var log:tEventLog;

function GetTempName(const hash:tKey32; const ext:string): string;
  var t:string[48];
  begin
  setlength(t,48);
  ToHex(@t[1],hash,24);
  GetTempName:=cStoreDir+t+'.'+ext;
end;

function StoreOpen( id: tFID ): TStream;
  var vl:TStream;
  var StorageMode:tsoLocation;
  var linkcnt:integer;
  var win:tWindowedStream;
  begin
  result:=nil;
  linkcnt:=0;
  StorageMode:=tsoLocation(99);
  while (id[23] and 1) =0 do begin
    if(linkcnt>15) then raise eObjectNF.Create('Too many Links');
    if opendebug then log.debug('.Open.Follow: %S',[string(id)]);
    id:=ReadLink(id);
    inc(linkcnt);
  end;
  vl:=dbGet( dbObject, id, 24 );
  try
    if vl.size=0 then raise eObjectNF.Create('Object not found'{+' '+string(id)});
    StorageMode:=tsoLocation(vl.ReadByte);
    if StorageMode = solInline then begin
        win:=tWindowedStream.Create(vl,vl.size-4,4);
        assert(not win.SourceOwner);
        win.SourceOwner:=true;
        result:=win;
        if opendebug then log.debug('.Open.Inline: %S',[string(id)]);
    end else if StorageMode = solNormal then begin
        result:=tFileStream.Create(cStoreDir+string(id),fmOpenRead);
        if opendebug then log.debug('.Open.Normal: %S',[string(id)]);
    end else if StorageMode = solReference then begin
        vl.skip(3);
        result:=tFileStream.Create(vl.ReadStringAll,fmOpenRead);
        if opendebug then log.debug('.Open.Reference: %S',[string(id)]);
    end else begin
        StorageMode:=tsoLocation(99);
        raise Exception.Create('data corrupt');
    end;
  finally
    if StorageMode<>solInline then vl.Free;
  end;
end;


function ReadLinkData(const id: tFID): TCustomMemoryStream;
  begin
  if (id[23] and 1) =1 then raise eObjectNF.Create('Not a Link'); {change exc class!}
  result:=dbGet( dbObject, id, 24 );
  if result.size=0 then raise eObjectNF.Create('Object not found'{+' '+string(id)});
end;

function ReadLink(const id: tFID): tFID;
  var vl:TCustomMemoryStream;
  begin
  vl:=ReadLinkData(id);
  vl.Read(result,24);
  vl.Free;
end;

function IsBlob(const id: tFID): boolean;
  begin
  IsBlob:=((id[23] and 1) = 1);
end;


procedure GetBlobInfo(const id: tFID; out mode:byte; out path:Ansistring );
  var vl:TCustomMemoryStream;
  begin
  path:='';
  vl:=dbGet( dbObject, id, 24 ); try
    if vl.size=0
      then mode:=0
    else begin
      mode:=vl.ReadByte;
      if mode=ord(solReference) then begin
        vl.Skip(4);
        SetLength(path,vl.Left);
        vl.Read(path[1],vl.Left);
      end else if mode=ord(solNormal)
        then path:=cStoreDir+'/'+string(id);
    end;
  finally vl.Free end;
end;

function StoreExists(const id: tFID): boolean;
  var vl:TCustomMemoryStream;
  begin
  vl:=dbGet( dbObject, id, 24 );
  result:= (vl.size>0);
  vl.Free;
end;

procedure HashAndCopy(inf:tStream; outf:tStream; out hash:tKey32; left:LongWord);
  {read input file, copy to output file, hash to id, dont seek/rename/close}
  var ctx:tSha256;
  var buf:packed array [0..1023] of byte;
  var bs:LongWord;
  begin
  ctx.Init;
  while left>0 do begin
    if left<=sizeof(buf) then bs:=left else bs:=sizeof(buf);
    inf.Read(buf,bs);
    if assigned(outf) then outf.Write(buf,bs);
    ctx.Update(buf,bs);
    left:=left-bs;
  end;
  ctx.Final(hash);
end;

procedure StoreDelete(const id: tFID);
  begin
end;

procedure InsertBlobInline(out id: tFID; fs: TStream; len:LongWord);
  var vl:tMemoryStream;
  var hash:tKey32;
  begin
  vl:=tMemoryStream.Create;
  vl.SetSize(4+len);
  vl.W1(Ord(solInline));
  vl.W1(0);vl.W2(0);
  HashAndCopy(fs,vl,hash,len);
  Move(hash,{->}id,24);
  id[23]:=id[23] or 1;
  dbSet( dbObject, id, 24, vl );
  vl.Free;
  log.info('.InsertBlob.Inline: %S',[string(id)]);
end;

procedure InsertBlobMem( out id: tFID; ms: TCustomMemoryStream );
  {var vl:tMemoryStream;
  var hash:tKey32;}
  begin
  ms.seek(0,soBeginning);
  {if ms.Length < cInlineThr then begin}
    InsertBlobInline(id, ms, ms.Size);
  {end else begin
    todo: use solNormal for biger blobs
  end;}
end;

procedure InsertBlobRef( const name:string; var hash:tKey32 );
  var vl2:TCustomMemoryStream;
  var vl:tMemoryStream;
  var id:tFID;
  var fs:tFileStream;
  var len:LongWord;
  begin
  hash[23]:=hash[23] or 1;
  Move(hash,{->}id,24);
  vl2:=dbGet( dbObject, id, 24 );
  if vl2.size=0 then begin
    fs:=tFileStream.Create(name,fmOpenRead);
    len:=fs.size;
    if len > cInlineThr then begin
      vl:=tMemoryStream.Create;
      vl.SetSize(4+length(name));
      vl.W1(Ord(solReference));
      vl.W1(0);vl.W2(0);
      vl.Write(name[1],length(name));
      dbSet( dbObject, id, 24, vl );
      vl.Free;
      log.info('.InsertBlob.Ref: %S->%S',[string(id),name]);
    end else try
      InsertBlobInline(id, fs, len);
      finally  fs.Free;
    end;
  end else begin
    vl2.Free;
    {do nothing if exists}
  end;
end;

procedure InsertBlobRename( const name:string; var hash:tKey32 );
  var vl2:TCustomMemoryStream;
  var vl:tMemoryStream;
  var id:tFID;
  var fs:tFileStream;
  var len:LongWord;
  var dname:string;
  begin
  hash[23]:=hash[23] or 1;
  Move(hash,{->}id,24);
  vl2:=dbGet( dbObject, id, 24 );
  if vl2.size=0 then begin
    fs:=tFileStream.Create(name,fmOpenRead);
    len:=fs.size;
    if len > cInlineThr then begin
      fs.Free;
      dname:=cStoreDir+string(hash);
      if not RenameFile(name,dname) then raise eInOutError.Create('Failed to rename '+name+' to '+dname);
      vl:=tMemoryStream.Create;
      vl.W1(Ord(solNormal));
      vl.W1(0);vl.W2(0);
      dbSet( dbObject, id, 24, vl );
      vl.Free;
      log.info('.InsertBlob.Rename: %S->%S',[name,string(id)]);
    end else try
      InsertBlobInline(id, fs, len);
      DeleteFile(name);
      finally  fs.Free;
    end;
  end else begin
    vl2.Free;
    DeleteFile(name);
    log.info('.InsertBlob.Deleted %S',[name]);
  end;
end;

procedure InsertLinkData(const id: tFID; s:tCustomMemoryStream);
  begin
  if( ((id[23] and 1)=1) or (s.size<24) )
    then raise eXception.Create('Invalid Link data');
  dbSet( dbObject, id, 24, s );
  log.info('.InsertLinkData %S',[string(id)]);
end;

{$IF FALSE}
procedure CheckObjects;
  var packSize:QWORD;
  procedure InitBlob;
    var outf:tFileStream;
    var mark:packed array [1..8] of byte;
    begin
    outf.OpenRW(cStoreDat);
    if outf.Length=0 then begin
      outf.Write(cStoreMark,8);
      {writeln('Store2: create '+cStoreDat);}
    end else begin
      outf.Read(mark,8);
      if CompareByte(mark,cStoreMark,8)<>0
      then raise eXception.Create('Store2: Pack file Corrupt '+cStoreDat);
    end;
    {writeln('Store2: '+cStoreDat+', '+cStoreDir);}
    PackSize:=outf.Length;
    outf.Done;
  end;
  var li,dv:integer;
  var id,calcid:tFID;
  var so:tStoreObject;
  var usageData: QWORD=0;
  var usagePack: QWORD=0;
  var usageMeta: QWORD=0;
  var usageTemp: QWORD=0;
  var rekt:byte;
  begin
  PackSize:=0;
  {Disabled InitBlob;}
  opendebug:=false;
  for li:=0 to length(dbKeyList)-1 do begin
    if tDbSect(byte(dbKeyList[li].v^))<>dbObject then continue;
    Move((dbKeyList[li].v+1)^,id,20);
    try
      so.Init(id);
      {unref temporary files}
      if so.temp then begin
        so.temp:=false;
        so.Reference(-1); {reference will save temp state}
        if so.opentime_refcount=1 then begin
          Inc(usageTemp,so.Length);
          continue;
        end;
        so.Init(id);
      end;
      rekt:=2;
      {check if files are still read only}
      if so.location in [solObjdir,solReference,solHardlink] then begin
        if so.CheckSetPerm then rekt:=0;
      end;
      {pack cannot get corrupted, check just last byte}
      if so.location = solPack then begin
        dv:=so.length;
        if dv>0 then begin
          so.Seek(dv-1);
          so.ReadByte; {raises}
        end;
        rekt:=0;
      end;
      {inline cannot get corrupted}
      if so.location = solData then rekt:=0;
      {check hash}
      if rekt>0 then begin
        if so.Left>25165824{24MiB} then
          writeln('Store2: Checking ',SizeToString(so.Left),'B ',so.GetPath);
        HashAndCopy(so,nil, calcid, so.Left);
        if calcid=id then rekt:=0;
      end;
    except
      on eInOutError do rekt:=1;
    end;
    if rekt=0 then begin
      if so.location=solData then Inc(usageData,so.Length);
      if so.location=solPack then Inc(usagePack,so.Length);
      Inc(usageMeta,dbKeyList[li].vl+dbKeyList[li].l);
      so.Done;
    end;
    if rekt>0 then writeln('Store2: CORRUPTED object ',string(id));
    if rekt>=2 then so.Reference(-9999);
    if rekt>=1 then dbDelete( dbObject, id, 20 ); {FIXME: so.done?}
  end;
  opendebug:=cDebugPrintOpen;
  writeln('Store2.Usage: inline=',SizeToString(usageData),'B, meta=',
    SizeToString(usageMeta),'B, pack=',SizeToString(usagePack),'B, pack_wasted=',SizeToString(packSize-usagePack),
    'B, temp_del=',SizeToString(usageTemp),'B');
end;
{$ENDIF}

BEGIN
  CreateLog(Log,'Store');
  opendebug:=cDebugPrintOpen;
  CreateDir(cStoreDir);
  //CheckObjects;
END.
