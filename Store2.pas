UNIT Store2;
{
 Object Manager. Reference Counting. Caching.
}
INTERFACE
USES Crypto,ObjectModel,Database,SysUtils;

const cSzlPack:longword=0;
      cSzlData:longword=3584;

type tFID=ObjectModel.tKey20;
type tFID_ptr=^ObjectModel.tKey20;

type tsoLoc=(solObjdir=1,solPack=2,solData=3,solReference=4,solHardlink=5);
{solHardlink and solReference objects are never deleted,
 solPack are deleted only on pack rebuild
 solObjdir and solData are deleted when refcount reaches zero}

type tStoreObject=object(tCommonStream)
    fid:tFID;
    location:tsoLoc;
    temp:boolean; {the file was inserted as temporary}
    opentime_refcount:word;
    constructor Init(id: tFID); {open existing file}
    constructor Insert(var ins: tMemoryStream);
    constructor InsertRename(var ins:tFileStream; const name:string; const hash:tFID);
    {*refcount is set to 1 on Insert* a file not in store}
    {*refcount is increased by 1 on Insert* a file already in store}
    destructor  Done;
    procedure Reference( adj: integer);
    procedure MakeTemp;
      {change reference count on file; file is closed if adj is negative}
    procedure Seek(abs:longword); virtual;
    function  Tell:LongWord; virtual;
    procedure Read(out blk; len:word); virtual;
    function  Length:LongWord; virtual;
    function GetPath:AnsiString;
    private
    base:longword;
    limit:longword;
    d:record
      case byte of
      1:( ms:tDbMemStream; );
      2:( fs:tFileStream; );
    end;
    procedure InsertCopyInline(var ins: tCommonStream);
    function CheckSetPerm :boolean;
  end;

  tSObj=tStoreObject;
  
  eObjectNF=class(eFileNotFound)
  end;

const cObjHeaderSize=0;
const cDebugPrintOpen=true;

function GetTempName(const id: tFID; const ext:string): string;
function ObjectExists(const id: tFID): boolean; overload;
procedure HashAndCopy(var inf:tCommonStream; outf:pCommonStream; out id:tFID; left:LongWord);

IMPLEMENTATION
{$IFDEF UNIX}{for the stat() and permissions}
USES BaseUnix;
{$ENDIF}


{metadata format
  dbObject key format
    sect:b1, file-id:k20
  dbObject value format
    zero:b4
    filesize:w4
    RefCount:w2
    temp_flag:b1
    method:b1 (tsoLoc)
    ---
    filename
    offset
    data
}

type tMeH=packed record
  z1:word4;
  size:word4;
  refc:word2;
  temp:byte;
  loc:byte;
end;

const cStoreMark:packed array [1..8] of char='BNStPAK'+char($1A);
const cStoreDat='storepak.dat';
const cStoreDir='obj/';
var opendebug:boolean;

function GetTempName(const id: tFID; const ext:string): string;
  begin
  GetTempName:=cStoreDir+string(id)+'.'+ext;
end;

constructor tStoreObject.Init(id: tFID);
  var vl:tDbMemStream;
  var mh:^tMeH;
  begin
  Inherited Init;
  location:=tsoLoc(99);
  fid:=id;
  vl:=dbGet( dbObject, id, 20 );
  if vl.length=0 then raise eObjectNF.Create('Object not found'{+' '+string(id)});
  try
  mh:=vl.ReadPtr(sizeof(tMeH));
  base:=0;
  limit:=mh^.size;
  location:=tsoLoc(mh^.loc);
  case location of
    solData: begin
      d.ms:=vl; //this can cause problems
      base:=sizeof(tMeH);
      end;
    solPack: begin
      d.fs.OpenRO(cStoreDat);
      d.fs.Seek(base);
      end;
    solHardlink,solObjDir: d.fs.OpenRO(cStoreDir+string(id));
    solReference: d.fs.OpenRO(vl.ReadStringAll);
    else begin
      location:=tsoLoc(99);
      raise Exception.Create('data corrupt');
      end;
  end;
  if opendebug then writeln('Store2.Open: ',string(fid),' ',location,'/',base,'+',limit);
  opentime_refcount:=mh^.refc;
  temp:=mh^.temp>0;
  finally
    if location<>solData then vl.Free;
  end;
end;

function tStoreObject.GetPath:AnsiString;
  var vl:tDbMemStream;
  begin
  if location=solReference then begin
    vl:=dbGet( dbObject, fid, 20 ); try
    vl.Skip(sizeof(tMeH));
    SetLength(result,vl.Left);
    vl.Read(result[1],vl.Left);
    finally vl.Free end;
  end else
  if (location=solHardlink) or (location=solObjDir) then
    result:=cStoreDir+string(fid)
  else result:='';
end;

function ObjectExists(const id: tFID): boolean;
  var vl:tDbMemStream;
  begin
  vl:=dbGet( dbObject, id, 20 );
  result:= vl.length>0;
  vl.Free;
end;

procedure tStoreObject.Reference( adj: integer);
  var vl:tDbMemStream;
  var mh:^tMeH;
  var a:integer;
  var loc2:tsoLoc;
  begin
  if adj=0 then exit;
  vl:=dbGet( dbObject, fid, 20 );
  mh:=vl.ReadPtr(sizeof(tMeH));
  a:=Word(mh^.refc)+adj;
  if a<0 then a:=0;
  mh^.refc:=Word(a);
  if (mh^.temp=1) and (not temp) then mh^.temp:=0;
  dbSet(dbObject, fid, 20, vl );
  if adj<0 then begin
    loc2:=location;
    self.Done;{<--this is important}
    if a<=0 then begin
      if opendebug then writeln('Store2.Delete: ',string(fid));
      if loc2=solObjdir then begin
        dbDelete( dbObject, fid, 20 );
        DeleteFile( cStoreDir+string(fid) );
      end;
      if loc2=solData then begin
        dbDelete( dbObject, fid, 20 );
      end;
      {TODO: hardlink, check stat and unlink if our is only link}
    end;
  end;
end;

procedure tStoreObject.MakeTemp;
  var vl:tDbMemStream;
  var mh:^tMeH;
  begin
  vl:=dbGet( dbObject, fid, 20 );
  mh:=vl.ReadPtr(sizeof(tMeH));
  if mh^.temp=0 then begin
    mh^.temp:=1;
    mh^.refc:=Word(mh^.refc)+1;
  end;
  temp:=true;
  dbSet(dbObject, fid, 20, vl );
end;

destructor tStoreObject.Done;
  begin
  case location of
    solData: d.ms.Free;
    solHardlink,solObjDir,solPack,solReference: d.fs.Done;
  end;
  location:=tsoLoc(99);
end;

procedure tStoreObject.Seek(abs:longword);
  begin
  case location of
    solData: d.ms.Seek(abs+sizeof(tMeH));
    solPack: begin
      if abs>limit then raise eReadPastEoF.Create('Pack Seek over object Boundary');
      d.fs.Seek(abs+base);
      end;
    solHardlink,solObjDir,solReference: d.fs.Seek(abs);
  end;
end;

procedure tStoreObject.Read(out blk; len:word);
  begin
  case location of
    solData: d.ms.Read(blk,len);
    solPack: begin
      {too expensive to check every time
      if (d.fs.Tell+len)>limit then then raise eReadPastEoF.Create('Pack Read over object Boundary');}
      d.fs.Read(blk,len);
      end;
    solHardlink,solObjDir,solReference: d.fs.Read(blk,len);
  end;
end;

function tStoreObject.Tell:LongWord;
  begin
  case location of
    solData: Tell:=d.ms.position-sizeof(tMeH);
    solPack: Tell:=d.fs.Tell-base;
    solHardlink,solObjDir,solReference: Tell:=d.fs.Tell;
  end;
end;
function tStoreObject.Length:LongWord;
  begin Length:=limit end;




constructor tStoreObject.Insert(var ins: tMemoryStream);
  procedure WriteObject;
    var vl:tMemoryStream;
    var mh:^tMeH;
    var df:tFileStream;
    var hash:tFID absolute fid;
    var ctx:tSHA256;
    var dname:string[24];

    begin
    ins.Seek(0);
    ctx.Init;
    ctx.Update(ins.Base^,ins.vLength);
    ctx.TruncFinal(hash,20);
    vl:=dbGet( dbObject, hash, 20 );
    if vl.length=0 then begin
      dname:=cStoreDir+string(hash);
      df.OpenRW(dname);
      df.Write(ins.Base,ins.vLength);
      df.Done;
      vl.Init(sizeof(tMeH)); mh:=vl.Base; vl.vLength:=sizeof(tMeH);
      location:=solObjdir; mh^.loc:=byte(location);
      limit:=ins.vLength; mh^.size:=ins.vLength;
      mh^.z1:=0; mh^.temp:=0; mh^.refc:=0;
      writeln('Store2.Insert.WriteObject: ',string(hash));
    end;
    dbSet( dbObject, hash, 20, vl );
    vl.Free;
    Self.Init(hash);
    Self.Reference(+1);
  end;

  begin {Insert}
  location:=tsoLoc(99);
  if ins.Length<cSzlData
    then InsertCopyInline(ins)
    else WriteObject;
end;

constructor tStoreObject.InsertRename(var ins:tFileStream; const name:string; const hash:tFID);
  var len:LongWord;
  procedure RenameObject;
    var vl:tMemoryStream;
    var mh:^tMeH;
    var dname:string;

    begin
    {prepare meta}
    vl.Init(sizeof(tMeH)); mh:=vl.Base; vl.vLength:=sizeof(tMeH);
    location:=solObjdir; mh^.loc:=byte(location);
    mh^.size:=len; limit:=len;
    mh^.z1:=0;  mh^.refc:=1;
    mh^.temp:=0;
    dname:=cStoreDir+string(hash);
    fid:=hash;
    {rename file}
    ins.Done;
    if FileExists(dname)
    then DeleteFile(name)
    else begin
      if not RenameFile(name,dname) then raise eInOutError.Create('Failed to rename '+name+' to '+dname);
      CheckSetPerm;
    end;
    {open or create and open}
    try
      Self.Init(hash);
      Self.Reference(+1);
      vl.Free;
      Exit;
    except
      dbSet( dbObject, hash, 20, vl );
      writeln('Store2.Insert.RenameObject: ',string(hash));
      vl.Free;
      Self.Init(hash);
    end;
  end;

begin {InsertRename}
  location:=tsoLoc(99);
  len:=ins.Length;
  if len<cSzlData then begin
    InsertCopyInline(ins);
    ins.Done;
    DeleteFile(name);
  end else RenameObject;
end;


procedure HashAndCopy(var inf:tCommonStream; outf:pCommonStream; out id:tFID; left:LongWord);
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
  ctx.TruncFinal(id,sizeof(id));
end;

procedure tStoreObject.InsertCopyInline(var ins:tCommonStream);
  var vl:tMemoryStream;
  var hash:tFID;
  var len:LongWord;
  var mh:^tMeH;
  var ctx:tSha256;
  begin
  location:=tsoLoc(99);
  ins.Seek(0);
  len:=ins.Length;
  vl.Init(len+sizeof(tMeH)); mh:=vl.Base; vl.vLength:=sizeof(tMeH); vl.Position:=sizeof(tMeH);
  ins.Read(vl.WRBuf^,len);
  ctx.Init;
  ctx.Update(vl.WRBuf^,len);
  ctx.TruncFinal(hash,20);
  vl.WrEnd(len);
  mh^.z1:=0; mh^.size:=len; mh^.refc:=1; mh^.temp:=0; mh^.loc:=byte(solData);
  {common branch (try open else set and open)}
  try
    Self.Init(hash);
    vl.Free;
    Self.Reference(+1);
    Exit;
  except
    dbSet( dbObject, hash, 20, vl );
    writeln('Store2.InsertCopyInline: ',string(hash));
    vl.Free;
    Self.Init(hash);
  end;
end;


function tStoreObject.CheckSetPerm : boolean;
  var fn:string;           {rwxrwxrwx}
  {$IFDEF UNIX}
  const writeable:longword=%010010010;
  const readable:longword =%100000000;
  var info:BaseUnix.Stat;
  var mode:tMode;
  {$endif}
  begin result:=false;
  assert(location=solObjdir);//todo
  fn:=cStoreDir+string(fid);
  {$ifdef UNIX}
  if fpStat(fn,info)<>0 then exit;
  if info.st_size<>limit then exit;
  if (info.st_mode and writeable)=0 then result:=true;
  mode:=info.st_mode and (not writeable); {unset all write bits}
  mode:=mode or readable; {set at least write for user}
  fpChMod(fn,mode);
  {$ELSE}
  if (FileGetAttr(fn) and faReadOnly)>0 then result:=true;
  if d.fs.Length<>limit then exit;
  FileSetAttr(fn,faReadOnly);
  {$ENDIF}
end;

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

BEGIN
  CreateDir(cStoreDir);
  CheckObjects;
END.
