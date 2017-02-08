UNIT Store;
{
 Object Manager. Reference Counting. Caching.

}
INTERFACE
USES Crypto,ObjectModel,Database,SysUtils;

type tFID=ObjectModel.tKey24;
type tFID_ptr=^tFID;
type tsoLocation = (solNormal, solInline,solReference);

type tStoreObject=OBJECT(tCommonStream)
  StorageMode: tsoLocation;
  constructor Init(id: tFID); {open existing file}
  destructor  Done;
  procedure Seek(abs:longword); virtual;
  function  Tell:LongWord; virtual;
  procedure Read(out blk; len:word); virtual;
  function  Length:LongWord; virtual;
  function GetPath(const id: tFID): AnsiString;
  private
  d:record
    case byte of
    1:( ms:tDbMemStream; );
    2:( fs:tFileStream; );
  end;
end;

type eObjectNF=CLASS(eFileNotFound)
end;

const cObjHeaderSize=0;
const cDebugPrintOpen=true;
const cInlineThr=8*1024;

function GetTempName(const hash:tKey32; const ext:string): string;
function ObjectExists(const id: tFID): boolean; overload;
function IsBlob(const id: tFID): boolean; overload;
procedure DeleteObject(const id: tFID); unimplemented;

procedure InsertBlobMem( out id: tFID; ms: tMemoryStream );
procedure InsertBlobRename( const name:string; const hash:tKey32 );
procedure InsertBlobRef( const name:string; const hash:tKey32 );

function ReadLink(const id: tFID): tFID;
function ReadLinkData(const id: tFID): tDbMemStream;
procedure InsertLinkData(const id: tFID; s:tMemoryStream);

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


const cStoreDir='obj/';
var opendebug:boolean;

function GetTempName(const hash:tKey32; const ext:string): string;
  var t:string[48];
  begin
  setlength(t,48);
  BinToHex(@t[1],hash,48);
  GetTempName:=cStoreDir+t+'.'+ext;
end;

constructor tStoreObject.Init(id: tFID);
  var vl:tDbMemStream;
  begin
  Inherited Init;
  StorageMode:=tsoLocation(99);
  while (id[23] and 1) =0 do begin
    if opendebug then writeln('Store2.Open.Follow: ',string(id));
    id:=ReadLink(id);
  end;
  vl:=dbGet( dbObject, id, 24 );
  try
    if vl.length=0 then raise eObjectNF.Create('Object not found'{+' '+string(id)});
    StorageMode:=tsoLocation(vl.ReadByte);
    if StorageMode = solInline then begin
        d.ms:=vl; {this can cause problems}
        Seek(0);
        if opendebug then writeln('Store2.Open.Inline: ',string(id));
    end else if StorageMode = solNormal then begin
        d.fs.OpenRO(cStoreDir+string(id));
        if opendebug then writeln('Store2.Open.Normal: ',string(id));
    end else if StorageMode = solReference then begin
        vl.skip(3);
        d.fs.OpenRO(vl.ReadStringAll);
        if opendebug then writeln('Store2.Open.Reference: ',string(id));
    end else begin
        StorageMode:=tsoLocation(99);
        raise Exception.Create('data corrupt');
    end;
  finally
    if StorageMode<>solInline then vl.Free;
  end;
end;


function ReadLinkData(const id: tFID): tDbMemStream;
  begin
  if (id[23] and 1) =1 then raise eObjectNF.Create('Not a Link'); {change exc class!}
  result:=dbGet( dbObject, id, 24 );
  if result.length=0 then raise eObjectNF.Create('Object not found'{+' '+string(id)});
end;

function ReadLink(const id: tFID): tFID;
  var vl:tDbMemStream;
  begin
  vl:=ReadLinkData(id);
  vl.Read(result,24);
  vl.Free;
end;

function IsBlob(const id: tFID): boolean;
  begin
  IsBlob:=((id[23] and 1) = 1);
end;


function tStoreObject.GetPath(const id: tFID):AnsiString;
  var vl:tDbMemStream;
  begin
  if StorageMode = solReference then begin
    vl:=dbGet( dbObject, id, 20 ); try
    vl.Skip(4);
    SetLength(result,vl.Left);
    vl.Read(result[1],vl.Left);
    finally vl.Free end;
  end
  else result:='';
end;

function ObjectExists(const id: tFID): boolean;
  var vl:tDbMemStream;
  begin
  vl:=dbGet( dbObject, id, 20 );
  result:= (vl.length>0);
  vl.Free;
end;

destructor tStoreObject.Done;
  begin
  case StorageMode of
    solInline: d.ms.Free;
    solNormal,solReference: d.fs.Done;
  end;
  StorageMode:=tsoLocation(99);
end;

procedure tStoreObject.Seek(abs:longword);
  begin
  case StorageMode of
    solInline: d.ms.Seek(abs+4);
    solNormal,solReference: d.fs.Seek(abs);
  end;
end;

procedure tStoreObject.Read(out blk; len:word);
  begin
  case StorageMode of
    solInline: d.ms.Read(blk,len);
    solNormal,solReference: d.fs.Read(blk,len);
  end;
end;

function tStoreObject.Tell:LongWord;
  begin
  case StorageMode of
    solInline: Tell:=d.ms.position-4;
    solNormal,solReference: Tell:=d.fs.Tell;
  end;
end;
function tStoreObject.Length:LongWord;
  begin
  case StorageMode of
    solInline: Length:=d.ms.Length-4;
    solNormal,solReference: Length:=d.fs.Length;
  end;
end;

procedure HashAndCopy(var inf:tCommonStream; outf:pCommonStream; out hash:tKey32; left:LongWord);
  {read input file, copy to output file, hash to id, dont seek/rename/close}
  var ctx:tSha256;
  var buf:packed array [0..1023] of byte;
  var bs:LongWord;
  begin
  ctx.Init;
  while left>0 do begin
    if left<=sizeof(buf) then bs:=left else bs:=sizeof(buf);
    inf.Read(buf,bs);
    if assigned(outf) then outf^.Write(buf,bs);
    ctx.Update(buf,bs);
    left:=left-bs;
  end;
  ctx.Final(hash);
end;

procedure DeleteObject(const id: tFID);
  begin
end;

procedure InsertBlobInline(out id: tFID; fs: tCommonStream; len:LongWord);
  var vl:tMemoryStream;
  var hash:tKey32;
  begin
  vl.Init(4+len);
  vl.WriteByte(Ord(solInline));
  vl.WriteByte(0);vl.WriteWord2(0);
  HashAndCopy(fs,@vl,hash,len);
  Move(hash,{->}id,24);
  id[23]:=id[23] or 1;
  dbSet( dbObject, id, 24, vl );
  vl.Free;
  writeln('Store2.InsertBlob.Inline: ',string(id));
end;

procedure InsertBlobMem( out id: tFID; ms: tMemoryStream );
  {var vl:tMemoryStream;
  var hash:tKey32;}
  begin
  ms.seek(0);
  {if ms.Length < cInlineThr then begin}
    InsertBlobInline(id, ms, ms.Length);
  {end else begin
    todo: use solNormal for biger blobs
  end;}
end;

procedure InsertBlobRef( const name:string; const hash:tKey32 );
  var vl2:tDbMemStream;
  var vl:tMemoryStream;
  var id:tFID;
  var fs:tFileStream;
  var len:LongWord;
  begin
  Move(hash,{->}id,24);
  id[23]:=id[23] or 1;
  vl2:=dbGet( dbObject, id, 20 );
  if vl2.length=0 then begin
    fs.OpenRO(name);
    len:=fs.Length;
    if len > cInlineThr then begin
      vl.Init(4+length(name));
      vl.WriteByte(Ord(solReference));
      vl.WriteByte(0);vl.WriteWord2(0);
      vl.Write(name[1],length(name));
      dbSet( dbObject, id, 24, vl );
      vl.Free;
      writeln('Store.InsertBlob.Ref: ',string(id),'->',name);
    end else try
      InsertBlobInline(id, fs, len);
      finally  fs.Done;
    end;
  end else begin
    vl2.Free;
    {do nothing if exists}
  end;
end;

procedure InsertBlobRename( const name:string; const hash:tKey32 );
  var vl2:tDbMemStream;
  var vl:tMemoryStream;
  var id:tFID;
  var fs:tFileStream;
  var len:LongWord;
  var dname:string;
  begin
  Move(hash,{->}id,24);
  id[23]:=id[23] or 1;
  vl2:=dbGet( dbObject, id, 20 );
  if vl2.length=0 then begin
    fs.OpenRO(name);
    len:=fs.Length;
    if len > cInlineThr then begin
      fs.Done;
      dname:=cStoreDir+string(hash);
      if not RenameFile(name,dname) then raise eInOutError.Create('Failed to rename '+name+' to '+dname);
      vl.Init(4);
      vl.WriteByte(Ord(solNormal));
      vl.WriteByte(0);vl.WriteWord2(0);
      dbSet( dbObject, id, 24, vl );
      vl.Free;
      writeln('Store.InsertBlob.Rename: ',name,'->',string(id));
    end else try
      InsertBlobInline(id, fs, len);
      finally  fs.Done;
    end;
  end else begin
    vl2.Free;
    DeleteFile(name);
    writeln('Store.InsertBlob.Deleted ',name);
  end;
end;

procedure InsertLinkData(const id: tFID; s:tMemoryStream);
  begin
  if( ((id[23] and 1)=1) or (s.Length<24) )
    then raise eXception.Create('Invalid Link data');
  dbSet( dbObject, id, 24, s );
  writeln('Store.InsertLinkData ',string(id));
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
  opendebug:=cDebugPrintOpen;
  CreateDir(cStoreDir);
  //CheckObjects;
END.
