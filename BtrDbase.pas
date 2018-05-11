UNIT BtrDbase;
(* B+Tree based key-value database. *)

{ Notes:
  *block device backed storage
  *memory-mapping
  *node types
    - superblock
    - branch
    - leaf
    - data
    - free list
    - unstable array spill
  *file structure
    - primary branch area
      - (0-3) superblocks
      - branches
      - roots
    - secondary branch area
      - branches ???
    - mixed area
      - leaves
      - data
      - branches
    - (E) end marker page
  * Primary Branch Area is fully mmap()ed at all time
  * secondary branch area is optional, and mmaped
  * Mixed area will be either read() or mmap()d on demand

  * Free space management
    - can't overwrite page immediately
      - not even by linked list pointer
    - (A) linked list of stable free pages
    - (B) array of unstable freed pages in superblock
    - page must be on array in previous superblock before it can be written
    - possible extent allocator for continuous files

  * Superblock:
    - magic
    - generation number
    - configuration ?
    - root pointer
    - freelist first pointer (primary,secondary,mixed)
    - unstable array
    - unstable array spill pointer
    - free extent tree ?

  * Superblock Lite:
    - magic
    - generation number
    - branch pointer
    - freelist first pointer (primary,secondary,mixed)
    - unstable array pointer
  * Branch:
    - ident
    - (height, parent, generation, ...)
    - prefix
    - separator-pointer pairs
  * Leaf:
    - ident
    - prefix
    - key-value pairs

  Read transactions hold reference to Root, so the roots must not be rewritten.
    - root -> == branch
    - superblock!=root

  * When block is freed:
    - added to unstable list
    -
  * Whent to verify csums?
    - on every access
    - ideally only when it is loaded from disk
      - but os does not tell wheter it was loaded or already in pagecache
    - bitfield for already verified pages? (in primary area)
    - only when requested

  * To read from multiple processes:
    - read superblock on every tx open
      - 

  RTrx gen=1
  WTrx gen=1 -> 2, A:U->F, B:x->F, C:F->x
    - 
}
INTERFACE
IMPLEMENTATION
uses Classes,SysUtils,bn_avl_tree,ObjectModel,BaseUnix,crypto;

Type tDatabase= Class
  type tGenNum= QWord;
  type tPageix= DWord;
  type tRefCount= LongWord;
  type tLLPageGenNode=class
    next: tLLPageGenNode;
    gen: tGenNum;
    addr: tPageix;
  end;
  type tLLGeneration=class
    private {ro by transactions}
    gen: tGenNum;
    root: tCSPtr;
    super: tPageix;
    private {atomic by transaction}
    refc: tRefCount;
    private {under write mutex}
    next: tLLGeneration;
    sync: boolean;
  end;
  type tCSPtr= packed object
    page: Word4;
    csum: DWord;
    //procedure clear;
    //procedure set(ipage:tPageix; isum: dword);
  end;

  public

  FileHandle: THandle;

  (* Memorry-mapped Window into the Primary Branch Area *)
  PrimaryWindow: Pointer;
  PrimaryWindowEnd: Pointer;
  PrimarySize: LongWord;
  MixedStart: tPageIx;
  MixedSize: LongWord;


  (* Generation number on which the oldest open transaction was created *)
  { Unused space: page list, each page one generation
    $UnusedLatest -> page[gen, next, older, arr[pageix]]
    $UnusedOldest -> page (not modified, only moved, freed pages go to unused)
    Open read transaction share-locks the latest node page.
    @In write trx the oldest used node found by following the older pointer until
    exclusive lock succeeds. It still costs syscalls, but so does writing.
    @Put list of pages that blocked reclaim in ascending gen to superblock.
    @Only store Reusable in superblock, not unused.
    $freelist for all areas
    $freetree stub for mix only
  }

  { these should be part of Write transaction (memory only) }
  DirtyPages: tAVLTree;


  pagesize: LongWord;

  constructor OpenH(const ihandle: THandle);
  constructor Format(const ihandle: THandle);

  procedure LoadDatabase;
  procedure ReadUnaligned(var dest; offset: PtrUInt; size: PtrUInt);
  function ChecksumBuf(buffer: pointer; size: PtrUInt): DWord;
  function ChecksumPage(buffer: pointer): DWord;

  procedure FreePage( p: pointer );
  function GetPage( ix: tPageIx ): pointer;
  function GetPage( const ptr: tCSPtr ): pointer;
  (* Special pages: 0=super0 1=super1 2=super2 3=locks -1=last *)
  var SpecialPageIndex: array [0..4] of tPageIx;
end;

Type TWriteTrx= Class
  db: tDatabase;
  constructor Create(idb: tDatabase);
end;

var Log1: tEventLogBaseSink;
var Log: TEventLog;

constructor tWriteTrx.Create(idb: tDatabase);
  begin
  Fail;
end;


type tSuperblockH = packed record
  ident: array [1..8] of char;
  checksum: DWord;
  gen: Word8;
  pagesize: Word4;
  version: word2;
  sync: bytebool;
  height: byte;
  prisize, //1C
  secsize,
  mixsize, //24
  secpos, 
  mixpos: Word4; //2C
  root: tDatabase.tCSPtr;
  utilroot: tDatabase.tCSPtr;
  prifree: tDatabase.tCSPtr;
  secfree: tDatabase.tCSPtr;
  mixfree: tDatabase.tCSPtr;
  levelmix: byte;
  levelsec: byte;
end;
const c_sb_ident: array [1..8] of char = 'BnBtrDb'#0;

type EDbCheckSum= class(Exception)
  //constructor Create(ipageno:tPageNo);
  //property Message: string read What;
end;
type EDbInitialize= class(Exception)
end;
type EDbSuperInit= class(EDbInitialize)
end;

procedure tDatabase.ReadUnaligned(var dest; offset: PtrUInt; size: PtrUInt);
  var rc:TsSize;
  begin
  rc:=baseunix.fppread(FileHandle,@dest,size,offset);
  if rc<>size then raise Exception.Create('PRead failed');
end;

function WrapMMap( fd:THandle; ofs:PtrUInt; size:PtrUInt ): pointer;
  var rc: pointer;
  begin
  rc:= BaseUnix.Fpmmap( {addr} nil, {len} size,
    {prot} PROT_READ,
    {flag} MAP_SHARED,
    {fd} fd, {off} ofs );
  if rc=MAP_FAILED then raise Exception.Create('MMap failed');
  result:=rc;
end;

procedure tDatabase.FreePage( p: pointer );
  begin
  if (p>=PrimaryWindow) and (p<PrimaryWindowEnd)
    then exit;
  FreeMem(p,pagesize);
end;

function tDatabase.GetPage( ix: tPageIx ): pointer;
  begin
  if ix < PrimarySize then begin
    result:= PrimaryWindow + ( ix * pagesize );
  end else begin
    result:=GetMem(pagesize);
    try
      ReadUnaligned( result^, ix * pagesize, pagesize );
    except
      FreeMem(result,pagesize);
      raise;
    end
  end
end;

function tDatabase.GetPage( ptr: tCSPtr ): pointer;
  var sum: DWord;
  var ix: tPageIx;
  begin
  ix:= ptr.page;
  if ix < PrimarySize then begin
    result:= PrimaryWindow + ( ix * pagesize );
      sum:= ChecksumPage(result);
      if(sum<>ptr.csum)
        then raise EDbCheckSum.CreateFmt('Checksum error at page %Dp, correct %S',[DWord(ptr.page),ToHexStr(sum,4)]);
  end else begin
    result:=GetMem(pagesize);
    try
      ReadUnaligned( result^, ix * pagesize, pagesize );
      sum:= ChecksumPage(result);
      if(sum<>ptr.csum)
        then raise EDbCheckSum.CreateFmt('Checksum error at page %Dp, correct %S',[DWord(ptr.page),ToHexStr(sum,4)]);
    except
      FreeMem(result,pagesize);
      raise;
    end
  end
end;

procedure tDatabase.LoadDatabase();
  var sbpagesize: LongWord;
  var super: ^tSuperblockH;
  procedure insertgen(gen: tLLGeneration);
    begin
  end;
  procedure CheckSbAt(sbpos:PtrUInt);
   (* Early Superblock Check *)
    var buf:^tSuperblockH;
    var tmp:DWORD;
    var gen: tLLGeneration;
    var p:^tLLGeneration;
    begin
    buf:=GetMem(512);
    try
      ReadUnaligned(buf^,sbpos,512);
      if buf^.ident <> c_sb_ident then exit;
      if (DWord(buf^.pagesize) = 0)
        or ((sbpos mod DWord(buf^.pagesize)) >0) then exit;
      if buf^.checksum<>ChecksumBuf(pointer(buf)+12,500) then begin
        tmp:=ChecksumBuf(pointer(buf)+12,500);
        log.Warn('.CheckSbAt: Invalid bufblock checksum at %DB',[sbpos]);
        log.info('.CheckSbAt: correct %S',[ToHexStr(tmp,4)]);
        exit end;
      gen:= tLLGeneration.Create;
      gen.gen:= buf^.gen;
      gen.super:= sbpos div DWord(buf^.pagesize);
      gen.sync:= buf^.sync;
      gen.root:= buf^.root;
      gen.refc:= 1; {the 1 reference is from written superblock}
      p:=@OldestGeneration;
      while assigned(p^) and (p^.gen<gen.gen)
        do p:=@p^.next;
      gen.next:=p^;
      p^:=gen;
      if gen.next=nil then begin
        LatestGeneration:=gen;
        if assigned(super) then FreeMem(super,512);
        super:=buf;
        sbpagesize:=DWord(buf^.pagesize);
      end;
    finally
      if buf<>super then FreeMem(buf);
    end
  end;

  procedure LoadAreas;
    begin
    {Focus on base functionality. Custom page size may come later.}
    {Btw: pagesize means mul instead of shl. performace ??}
    if sbpagesize<>4096
      then raise EDbSuperInit.Create('unsupported page size');
    pagesize:= sbpagesize;

    {find a way to calculate this with diffrent page size}
    SpecialPageIndex[0]:=0; {byte offset 0}
    SpecialPageIndex[1]:=65536 div pagesize;
    if SpecialPageIndex[1] <= 2
      then SpecialPageIndex[2]:=SpecialPageIndex[1]+1
      else SpecialPageIndex[2]:=1;
    SpecialPageIndex[3]:=SpecialPageIndex[2]+1;

    PrimaryWindow:= nil;
    PrimarySize:= 0;
    PrimaryWindowEnd:=nil;

    if Word(super^.version)<>1
      then raise EDbInitialize.Create('unknown superblock version');


    if DWord(super^.prisize)>$800000
      then raise EDbInitialize.Create('primary area too large');
    PrimarySize:= DWord(super^.prisize);
    if PrimarySize <= SpecialPageIndex[3]
      then raise EDbInitialize.Create('primary area too small');

    PrimaryWindow:= WrapMMap(FileHandle, 0, PrimarySize * pagesize);
    PrimaryWindowEnd:= PrimaryWindow + (PrimarySize * pagesize) - 1;
    log.debug('.LoadAreas: Primary window at $%P..$%P size %Dp',[PrimaryWindow, PrimaryWindowEnd, PrimarySize]);

    if DWord(super^.secsize)<>0
      then raise EDbInitialize.Create('unsupported secondary area');

    MixedStart:=super^.mixpos;
    MixedSize:= super^.mixsize;    

    if MixedStart<PrimarySize
      then raise EDbInitialize.Create('mixed area overlaps primary area');
    if MixedSize<24
      then raise EDbInitialize.Create('mixed area too small');
    SpecialPageIndex[4]:=MixedStart+(MixedSize-1);
    FreePage(GetPage(SpecialPageIndex[4]));
    log.debug('.LoadAreas: Mixed area at %Dp size %Dp',[MixedStart, MixedSize]);
  end;

  begin
  sbpagesize:=0;
  super:=nil;
    {mark that the allocator is not initialized yet}
    DirtyPages:=nil;
  try
    CheckSbAt(0);
    CheckSbAt(65536);
    if LatestGeneration=nil then
      raise EDbSuperInit.Create('No valid superblocks found');
    log.Info('.LoadSuperblocks: found sb at %Dp with page size %DB generation %D',[LatestGeneration.super,sbpagesize,LatestGeneration.gen]);
    LoadAreas;
  finally
    if assigned(super) then FreeMem(super,512);
  end;
  
  
  {Find latest synced sb and use config from it.}
  {Check if dirty sb is pesent and attempt to recover from it.}

  {find superblock}
  {get config from sb}
end;

{procedure tDatabase.LoadAreas;
  begin
  log.Debug('.LoadPrimaryArea ...',[]);
  check size
  mmap
end;
}

//procedure tDatabase.DoFormat;
//  begin
  {better?
    - only set memory state
    - create write transaction
    - free all pages
    - sync
  }
  {what?
    - determine areas
    
    - write superblock(s)
  }
//end;

constructor tDatabase.OpenH(const ihandle: THandle);
  begin
  Assert(ihandle>=0);
  FileHandle:= ihandle;
  log.Debug('.OpenH handle: %D',[FileHandle]);
  LoadDatabase();
end;

constructor tDatabase.Format(const ihandle: THandle);
  begin
  Assert(ihandle>=0);
  FileHandle:= ihandle;
  log.Debug('.Format handle: %D',[FileHandle]);
  //DoFormat;
end;

(* Unaligned checksum calculation. *)
function tDatabase.ChecksumBuf(buffer:pointer; size: PtrUInt): DWord;
  begin
  result:= NtoLE( Crypto.CRC32c(0, buffer^, size) );
end;

(* Page-aligned checksum calculation. Open for optimization *)
function tDatabase.ChecksumPage(buffer:pointer): DWord;
  begin
  assert((PtrUInt(@buffer) and (pagesize-1)) = 0);
  result:= NtoLE( Crypto.CRC32c(0, buffer^, self.pagesize) );
end;


var db:tDatabase;

BEGIN
  Log1:=tEventLogBaseSink.Create;
  Log:=TEventLog.Create(Log1,'db');
  Log.Info(' BtrDBase: %S',['0.0']);
  log.Info(' sb size: %D',[sizeof(tSuperblockH)]);
  db:=tDatabase.OpenH(FileOpen('btrdb.dat',fmOpenReadWrite));
  db.Free;
END.
