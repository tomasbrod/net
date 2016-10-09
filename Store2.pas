UNIT Store2;
{
 Object Manager. Reference Counting. Caching.
}
INTERFACE
USES Sha512,ObjectModel,Database,SysUtils;

const cSzlPack:longword=0;
      cSzlData:longword=3584;

type tFID=ObjectModel.tKey20;

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
    constructor HashObjectRename(const name:string);
      {store name file, name will not exist (deleted or renamed) on return}
    constructor HashObjectRename(var ins:tFileStream; const name:string; itemp:boolean); overload;
      {same as above, except reuse open file ins, which is then closed}
    constructor HashObjectLinkOrRef(const name:string); experimental;
      {store only the reference to file name; name will be set read-only}
    constructor HashObjectStream(var ins:tMemoryStream); overload;
      {...}
    {*refcount is set to 1 on HashObject* a file not in store}
    {*refcount is increased by 1 on HashObject* a file already in store}
    destructor  Close;
    procedure Reference( adj: integer);
      {change reference count on file; file is closed if adj is negative}
    procedure Seek(abs:longword); virtual;
    function  Tell:LongWord; virtual;
    procedure Read(out blk; len:word); virtual;
    function  Length:LongWord; virtual;
    private
    base:longword;
    limit:longword;
    d:record
      case byte of
      1:( ms:tDbMemStream; );
      2:( fs:tFileStream; );
    end;
    function CheckSetPerm :boolean;
  end;

  tSObj=tStoreObject;
  
  eObjectNF=class(eFileNotFound)
  end;

const cObjHeaderSize=0;
const cDebugPrintOpen=true;

function GetTempName(const id: tFID; const ext:string): string;

procedure Reference(const id: tFID; adj: integer); deprecated;

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
    vl.Free;
  end;
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
  if temp then mh^.temp:=1 else mh^.temp:=0;
  dbSet(dbObject, fid, 20, vl );
  if adj<0 then begin
    loc2:=location;
    self.Close;{<--this is important}
    if opendebug then writeln('Store2.Delete: ',string(fid));
    if (a<=0) and (loc2=solObjdir) then begin
      dbDelete( dbObject, fid, 20 );
      DeleteFile( cStoreDir+string(fid) );
    end;
    if (a<=0) and (loc2=solData) then begin
      dbDelete( dbObject, fid, 20 );
    end;
    {TODO: hardlink, check stat and unlink if our is only link}
  end;
end;

destructor tStoreObject.Close;
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

procedure Reference(const id: tFID; adj: integer);
  var so:tStoreObject;
  begin
  if adj=0 then exit;
  writeln('Store2.DirectReference: ',string(id),' ',adj);
  so.Init(id);
  so.Reference(adj);
  {if adj>=0 then }so.Close;
end;

procedure HashAndCopy(inf,outf:pCommonStream; out id:tFID; left:LongWord);
  {read input file, copy to output file, hash to id, dont seek/rename/close}
  var ctx:tSha512Context;
  var buf:packed array [0..1023] of byte;
  var bs:LongWord;
  begin
  Sha512Init(ctx);
  while left>0 do begin
    if left<=sizeof(buf) then bs:=left else bs:=sizeof(buf);
    inf^.Read(buf,bs);
    if assigned(outf) then outf^.Write(buf,bs);
    Sha512Update(ctx,buf,bs);
    left:=left-bs;
  end;
  Sha512Final(ctx,id,length(id));
end;


constructor tStoreObject.HashObjectRename(const name:string);
  {store name file, name will not exist (deleted or renamed) on return}
  {fail if rename fails}
  var ins:tFileStream;
  begin
  location:=tsoLoc(99);
  ins.OpenRO(name);
  HashObjectRename(ins,name,false);
end;

constructor tStoreObject.HashObjectRename(var ins:tFileStream; const name:string; itemp:boolean);
  {same as above, except reuse open file f, which is then closed}
  var vl:tMemoryStream;
  var hash:tFID;
  var len:LongWord;
  var mh:^tMeH;
  var dname:string;
  begin
  location:=tsoLoc(99);
  len:=ins.Length;
  {optimize storage}
  if len<cSzlData then begin
    vl.Init(len+sizeof(tMeH));
    mh:=vl.WrBuf;
    vl.WrEnd(sizeof(tMeH));
    HashAndCopy(@ins,@vl,hash,len);
    mh^.z1:=0; mh^.size:=len; mh^.refc:=1; mh^.temp:=0; mh^.loc:=byte(solData);
    if itemp then mh^.temp:=1;
  end
  {normal storage}
  else begin
    vl.Init(sizeof(tMeH));
    mh:=vl.WrBuf;
    vl.WrEnd(sizeof(tMeH));
    HashAndCopy(@ins,nil,hash,len);
    mh^.z1:=0; mh^.size:=len; mh^.refc:=1; mh^.temp:=0; mh^.loc:=byte(solObjdir);
    if itemp then mh^.temp:=1;
    dname:=cStoreDir+string(hash);
    if FileExists(dname)
    then DeleteFile(name)
    else begin
      if not RenameFile(name,dname) then raise eInOutError.Create('Failed to rename '+name+' to '+dname);
      CheckSetPerm;
    end;
  end;
  {common branch (try open else set and open)}
  ins.Done;
  try
    Self.Init(hash);
    vl.Free;
    {if opening temp file as temp then dont inc ref}
    if not (itemp and temp) then Self.Reference(+1);
    Exit;
  except
    dbSet( dbObject, hash, 20, vl );
    writeln('Store2.HashObjectRename: ',string(hash));
    vl.Free;
    Self.Init(hash);
  end;
end;

constructor tStoreObject.HashObjectLinkOrRef(const name:string);
  {store only the reference to file name; name will be set read-only}
  {impl: only ref :) }
  var vl:tMemoryStream;
  var hash:tFID;
  var len:LongWord;
  var mh:^tMeH;
  var ins:tFileStream;
  begin
  location:=tsoLoc(99);
  ins.OpenRO(name);
  len:=ins.Length;
  {reference storage}
  vl.Init(sizeof(tMeH)+system.length(name));
  mh:=vl.WrBuf;
  vl.WrEnd(sizeof(tMeH));
  HashAndCopy(@ins,nil,hash,len);
  mh^.z1:=0; mh^.size:=len; mh^.refc:=1; mh^.temp:=0; mh^.loc:=byte(solReference);
  vl.Write(name[1],system.length(name));

  ins.Done;
  try
    Self.Init(hash);
    vl.Free;
    Self.Reference(+1);
    Exit;
  except
    dbSet( dbObject, hash, 20, vl );
    writeln('Store2.HashObjectLinkOrRef: ',string(hash));
    vl.Free;
    Self.Init(hash);
  end;
end;

constructor tStoreObject.HashObjectStream(var ins:tMemoryStream);
  var vl:tMemoryStream;
  var hash:tFID;
  var len:LongWord;
  var mh:^tMeH;
  begin
  location:=tsoLoc(99);
  len:=ins.Length;
  {optimize storage}
    vl.Init(len+sizeof(tMeH));
    mh:=vl.WrBuf;
    vl.WrEnd(sizeof(tMeH));
    HashAndCopy(@ins,@vl,hash,len);
    mh^.z1:=0; mh^.size:=len; mh^.refc:=1; mh^.temp:=0; mh^.loc:=byte(solData);
  {common branch (try open else set and open)}
  try
    Self.Init(hash);
    vl.Free;
    Self.Reference(+1);
    Exit;
  except
    dbSet( dbObject, hash, 20, vl );
    writeln('Store2.HashObjectStream: ',string(hash));
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
  InitBlob;
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
          continue;
          Inc(usageTemp,so.Length);
        end;
        so.Init(id);
      end;
      rekt:=2;
      {check if files are still read only}
      if so.location in [solObjdir{,solReference,solHardlink}] then begin
        if so.CheckSetPerm then begin so.Close; continue end;
      end;
      {pack cannot get corrupted, check just last byte}
      if so.location = solPack then begin
        dv:=so.length;
        if dv>0 then begin
          so.Seek(dv-1);
          so.ReadByte; {raises}
        end;
        so.Close; continue;
      end;
      {check hash}
      HashAndCopy(@so,nil, calcid, so.Left);
      if calcid=id then rekt:=0;
    except
      on eInOutError do rekt:=1;
    end;
    if rekt=0 then begin
      if so.location=solData then Inc(usageData,so.Length);
      if so.location=solPack then Inc(usagePack,so.Length);
      Inc(usageMeta,dbKeyList[li].vl+dbKeyList[li].l);
      so.Close;
    end;
    if rekt>0 then writeln('Store2: corrupted object ',string(id));
    if rekt>=2 then so.Reference(-9999);
    if rekt>=1 then dbDelete( dbObject, id, 20 );
  end;
  opendebug:=cDebugPrintOpen;
  writeln('Store2.Usage: inline=',SizeToString(usageData),', meta=',
    SizeToString(usageMeta),', pack=',SizeToString(usagePack),', pack_wasted=',SizeToString(packSize-usagePack),
    ', temp_del=',SizeToString(usageTemp));
end;

BEGIN
  CreateDir(cStoreDir);
  CheckObjects;
END.
