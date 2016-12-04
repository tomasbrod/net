unit ObjectModel;
{ comcatenation of MemStream, NetAddr, Task units }
{ Base object types and utils }
{ Note: see here is a lot of RTL reimplemented,
  i did'nt like the way RTL impemented thinks :( }

{$mode objfpc}
{$PACKENUM 1}
INTERFACE
USES SysUtils,Sockets;

(*** Elemental Types ***)

type tKey16=packed array [0..15] of byte;
type tKey20=packed array [0..19] of byte;
type tKey32=packed array [0..31] of byte;
type tKey64=packed array [0..63] of byte;

type Word2=array [1..2] of byte; {0..65535}
type Word3=array [1..3] of byte; {todo}
type Word4=array [1..4] of byte; {0..4294967295}
type Word6=array [1..6] of byte; {todo}
type Word8=array [1..8] of byte; {todo}

(*** Simple Functions ***)

procedure BinToHex(hexValue:pChar; const orig; len:word);
function PrefixLength(const a,b:tKey20):byte;
function SizeToString( v:LongWord):string;
function IntHash(init:LongWord;const data;len:longword):LongWord;

(*** Base Object types ***)

type
  tCommonStream=object
    constructor Init;

    procedure Seek(absolute:LongWord); virtual;
    function  Tell:LongWord; virtual;
    function  Length:LongWord; virtual;
    procedure Read(out buf; cnt:Word); virtual; overload;
    procedure Write(const buf; cnt:word); virtual; overload;

    procedure Skip(dis:LongInt);
    function  Left:LongWord;
    function  ReadByte:byte;
    function  ReadWord2:word;
    function  ReadWord4:dword;
    procedure WriteByte(v:byte);
    procedure WriteWord2(v:word);
    procedure WriteWord4(v:dword);
    function ReadShortString:shortstring;
    function ReadStringAll:shortstring;
    procedure WriteShortString(s:shortstring);
  end;
  pCommonStream=^tCommonStream;

type
  tTask_ptr=^tTask;
  tTaskEvent=(tevComplete, tevError, tevClose, tevSubTask, tevUser, tevOther);
  tTaskCallback=procedure( task:tTask_ptr; event:tTaskEvent; data:pointer ) of object;
  tTask_SubItem=record
    cb: tTaskCallback;
    weak:boolean;
  end;

  tTask=object
    typeid:word;
    procedure Attach( subscriber:tTask_ptr; callback:tTaskCallback);
    procedure Attach( callback:tTaskCallback );
    procedure AttachWeak( callback:tTaskCallback );
    procedure Detach( callback:tTaskCallback );
    function ProgressPct: single; virtual; {scaled by 10000}
    function GetSubtaskCount: integer; virtual;
    function GetSubTask(i:integer): tTask_ptr; virtual;
    constructor Init;
    protected
    ObserversCount:word;
    procedure Abort; virtual;
    procedure Cleanup; virtual;
    procedure SendEvent(event:tTaskEvent; data:pointer);
    private
    subscriber:^tTask_SubItem;
    subscriberSize:word;
    inSendEvent:Word;
    complSent:boolean;
    procedure TaskIntAttach( callback:tTaskCallback; weak:boolean);
  end;

{type
  tPtrList=object
    firstn,lastn:^tPtrListNode;
    constructor Init;
    destructor Done;
    function AddHead(a:pointer):pointer;
    function AddTail(a:pointer):pointer;
    function PopHead:pointer;
    function PopTail:pointer;
    function Head:pointer;
    function Tail:pointer;
  end;}

(*** Derived Object Types ***)

const cNetAddrIP46_prefix: array [1..12] of byte
      = (0,0,0,0,0,0,0,0,0,0,255,255);

type
  tSockAddrL = packed record
           sa_family: sa_family_t;
           sa_data: array [0..107] of byte;
  end;
  tSockAddr= type tSockAddrL deprecated;

  tFamily=(afNil=0, afInet=1, afInet6 );
 
  tNetAddr= PACKED object
    function Length :word;
    procedure ToSocket( var sockaddr :tSockAddrL );
    procedure FromSocket( var sockaddr :tSockAddrL );
    procedure ToString( var str :String );
    procedure FromString( str :String );
    procedure LocalHost( af: tFamily );
    procedure Clear;
    function  isNil:boolean;
    public
    data :packed record
      port: Word;
      case byte of
      0: ( ip6: tIn6Addr );
      1: (
        ip46_prefix: array [1..12] of byte;
        ip4: tInAddr;
      );
    end{record};
  end{object};

const cDGramSz=768 deprecated;
type
  tMemoryStream=object(tCommonStream)
    vlength: LongWord;
    size: LongWord;
    base: pointer;
    position: LongWord;
    procedure Seek(absolute:LongWord); virtual;
    function  Tell:LongWord; virtual;
    function  Length:LongWord; virtual;
    procedure Read(out buf; cnt:Word); virtual;
    procedure Write(const buf; cnt:word); virtual;
    function  ReadWord(cnt:byte): LongWord; deprecated;
    function  ReadPtr(cnt:Word):pointer;
    procedure Trunc;
    procedure Append;
    procedure WriteWord(v:LongWord; cnt:byte); deprecated;
    constructor Init(ibuf:pointer; ilen,isize:LongWord);
    constructor Init(isize:LongWord);
    procedure Free; virtual;
    function WRBuf:pointer;
    function WRBufLen:LongWord;
    procedure WREnd(used:LongWord);
    function RDBuf:pointer;
    function RDBufLen:LongWord;
    procedure RDEnd(used:LongWord);
  end;

  tFileStream=object(tCommonStream)
    handle:tHandle;
    procedure Seek(absolute:LongWord); virtual;
    procedure Read(out buf; cnt:Word); virtual;
    procedure Write(const buf; cnt:word); virtual;
    function  Length:LongWord; virtual;
    function  Tell:LongWord; virtual;
    procedure Trunc(len:LongWord);
    constructor OpenRO(const fn:string);
    constructor OpenRW(const fn:string);
    constructor OpenHandle(const ihandle:tHandle);
    destructor Done;
  end;

type
  tConfigFile = object
    secarr:ppChar;
    constructor Init(var fs:tFileStream);
    function GetSection(name:pchar):pchar;
    destructor Done;
  end;

  tConfigSection = object
    sect,line:pchar;
    constructor Init(var cfg:tConfigFile; name:pchar);
    function GetKey(name:string):string;
    function GetLine:string;
    procedure Reset;
  end;

(*** to/from-string Conversion Operators ***)

operator :=(a:pointer) r:shortstring;

operator :=(a:tKey20) r:string;
operator :=(a:string) r:tKey20;

operator :=(k:tKey32) s:string;
operator :=(a:string) r:tKey32;

operator := ( at : tNetAddr) aString : string;
operator := ( aString : string) at : tNetAddr;

(*** Host<>Net conversion operators ***)

operator := (net : Word2) host:word;
operator := (host : word) net:Word2;

operator := (net : Word4) host:Dword;
operator := (host : Dword) net:Word4;

operator = (a,b:tKey20) r:boolean;
Operator = (aa, ab :tNetAddr) b : boolean;

(*** Stream Read/Write Overloads for some types ***)
{
procedure WriteBE(var s:tCommonStream; v:Word); overload;
procedure WriteBE3(var s:tCommonStream; v:DWord); overload;
procedure WriteBE(var s:tCommonStream; v:DWord); overload;
procedure WriteBE6(var s:tCommonStream; v:QWord); overload;
procedure WriteBE(var s:tCommonStream; v:QWord); overload;

procedure WriteBE(var s:tCommonStream; v:Word2); overload;
procedure WriteBE(var s:tCommonStream; v:Word3); overload;
procedure WriteBE(var s:tCommonStream; v:Word4); overload;
procedure WriteBE(var s:tCommonStream; v:Word6); overload;
procedure WriteBE(var s:tCommonStream; v:Word8); overload;

procedure WriteBE(var s:tCommonStream; v:tKey20); overload;
procedure WriteBE(var s:tCommonStream; v:tKey32); overload;
procedure WriteBE(var s:tCommonStream; v:tKey64); overload;

procedure WriteBE(var s:tCommonStream; v:tNetAddr); overload;
procedure WriteBE(var s:tCommonStream; v:tNetAddr); overload;
}
(*** Search ***)
type tComparePtrKeyFunc = function (a: pointer; key: pointer): ShortInt;
function FindIndex(a: ppointer; count: LongWord; key: pointer; compare: tComparePtrKeyFunc): LongWord;
procedure PtrListShiftLeft(a: ppointer; max, i:longword);
function PtrListShiftRight(a: ppointer; max, i:longword): boolean;


(*** Other ***)

type eInvalidMemStreamAccess=class(Exception)
  end;
type eReadPastEoF=class(Exception)
  ActuallyReadBytes:LongWord;
  end;
type eInOutError=SysUtils.eInOutError;
type eFileNotFound=class(eInOutError)
  end;

function ConvertFamily( a:tFamily ): sa_family_t;

IMPLEMENTATION
uses StrUtils; {TODO}

{*** MemStram ***}
operator :=(a:tKey20) r:string;
  begin
  SetLength(r,40);
  BinToHex(@r[1], a, 20);
end;
operator :=(a:string) r:tKey20;
  begin
  if HexToBin(@a[1],pchar(@r),20)<20 then raise
  eConvertError.Create('Invalid Hex String');
end;

operator  =(a,b:tKey20) r:boolean;
  begin
  r:=CompareDWord(a,b,5)=0;
end;

function PrefixLength(const a,b:tKey20):byte;
 var i:byte;
 var m:byte;
 begin
 i:=0; while(i<=19) do begin
  if a[i]<>b[i] then break;
  inc(i);
 end;
 result:=i*8;
 if i=20 then exit;
 m:=$80;
 while(m>0) do begin
  if (a[i] and m)<>(b[i] and m) then break;
  m:=m shr 1;
  inc(result);
 end;
end;

operator :=(k:tKey32) s:string;
 begin
 Setlength(s,64);
 BinToHex(@s[1],k,32);
end;
operator :=(a:string) r:tKey32;
  begin
  if HexToBin(@a[1],pchar(@r),32)<32 then raise
  eConvertError.Create('Invalid Hex String');
end;

procedure tMemoryStream.Seek(absolute:LongWord);
 begin
 if absolute>size then raise eInvalidMemStreamAccess.Create('Seek out of bounds');
 position:=absolute;
end;

procedure tMemoryStream.Read(out buf; cnt:Word);
 begin
 if (position+cnt)>vlength then raise eReadPastEoF.Create('Read out of bounds');
 Move((base+position)^,buf,cnt);
 position:=position+cnt;
end;

function tMemoryStream.ReadPtr(cnt:Word):pointer;
 begin
 result:=base+position;
 skip(cnt);
end;

function  tMemoryStream.ReadWord(cnt:byte): LongWord;
  begin
  case cnt of
    1:Read(result,1);
    2:result:=ReadWord2;
    4:result:=ReadWord4;
    else AbstractError;
  end;
end;
 
procedure tMemoryStream.Trunc;
 begin vlength:=position; end;
procedure tMemoryStream.Append;
 begin position:=length; end;
function tMemoryStream.Tell:LongWord;
 begin Tell:=position; end;

procedure tMemoryStream.Write(const buf; cnt:word);
 begin
 if (position+cnt)>size then raise eInvalidMemStreamAccess.Create('Write out of bounds');
 Move(buf,(base+position)^,cnt);
 position:=position+cnt;
 if position>vlength then vlength:=position;
end;

procedure tMemoryStream.WriteWord(v:LongWord; cnt:byte);
  begin
  case cnt of
    1: Write(v,1);
    2: WriteWord2(v);
    4: WriteWord4(v);
    else AbstractError;
  end;
end;

constructor tMemoryStream.Init(ibuf:pointer; ilen,isize:LongWord);
 begin
 Inherited Init;
 base:=ibuf;
 vlength:=ilen;
 size:=isize;
 seek(0);
end;
constructor tMemoryStream.Init(isize:LongWord);
 begin
 Init(GetMem(isize),0,isize);
end;
procedure tMemoryStream.Free;
  begin FreeMem(base,size) end;

function tMemoryStream.Length:LongWord;
 begin result:=vLength end;
function tMemoryStream.WRBuf:pointer;
 begin result:=base+position end;
function tMemoryStream.WRBufLen:LongWord;
 begin result:=size-position end;
procedure tMemoryStream.WREnd(used:LongWord);
 begin RDEnd(used); if position>length then vlength:=position end;
function tMemoryStream.RDBuf:pointer;
 begin result:=base+position end;
function tMemoryStream.RDBufLen:LongWord;
  begin
  if position>=length then result:=0
  else result:=length-position
end;
procedure tMemoryStream.RDEnd(used:LongWord);
 begin skip(used) end;

procedure tCommonStream.Seek(absolute:LongWord); begin AbstractError end;
function  tCommonStream.Tell:LongWord; begin result:=0; AbstractError end;
function  tCommonStream.Length:LongWord; begin result:=0; AbstractError end;
procedure tCommonStream.Read(out buf; cnt:Word); begin AbstractError end;
procedure tCommonStream.Write(const buf; cnt:word); begin AbstractError end;

constructor tCommonStream.Init;
  begin end;
procedure tCommonStream.Skip(dis:LongInt);
  {$PUSH}{$RANGECHECKS ON}
  begin Seek(Tell+dis) end;{$POP}
function  tCommonStream.ReadByte:byte;
  begin self.Read(result,1) end;
procedure tCommonStream.WriteByte(v:byte);
  begin self.Write(v,1) end;
function tCommonStream.Left:LongWord;
  begin Left:=Length-Tell end;
function tCommonStream.ReadShortString:shortstring;
  var l:byte;
  begin
  l:=ReadByte;
  SetLength(result,l);
  Read(result[1],l);
end;
function tCommonStream.ReadStringAll:shortstring;
  var l:byte;
  begin
  l:=Left;
  SetLength(result,l);
  Read(result[1],l);
end;
procedure tCommonStream.WriteShortString(s:shortstring);
  begin
  WriteByte(System.Length(s));
  Write(s[1],System.Length(s));;
end;

function tCommonStream.ReadWord2:word;
  begin
  Read(result,2);
  result:=beton(result);
end;
  function tCommonStream.ReadWord4:dword;
  begin
  Read(result,4);
  result:=beton(result);
end;

procedure tCommonStream.WriteWord2(v:word);
  begin
  v:=ntobe(v);
  Write(v,2);
end;
procedure tCommonStream.WriteWord4(v:dword);
  begin
  v:=ntobe(v);
  Write(v,4);
end;
  
const
  HexTbl: array[0..15] of char='0123456789ABCDEF';
procedure BinToHex(hexValue:pChar; const orig; len:word);
 var i:word;
 var b:array [byte] of byte absolute orig;
 begin
 dec(len);
 for i:=0 to len do begin
  hexValue[i*2+0]:=HexTbl[b[i] shr 4];
  hexValue[i*2+1]:=HexTbl[b[i] and 15];
 end;
end;

operator :=(a:pointer) r:shortstring;
  begin
  r:=IntToHex(ptrint(a),sizeof(pointer));
end;

{*** NetAddr ***}

Operator = (aa, ab :Sockets.tInAddr) b : boolean;
begin
 b:=aa.s_addr=ab.s_addr;
end;

Operator = (aa, ab :tNetAddr) b : boolean;
begin
 b:= CompareByte(aa.data,ab.data,sizeof(aa.data))=0;
end;

function tNetAddr.Length :Word;
begin
 result:=sizeof(self);
end;

function ConvertFamily( a:tFamily ): sa_family_t;
 begin
 case a of
  afInet: result:=Sockets.AF_INET;
  afInet6: result:=Sockets.AF_INET6;
  else AbstractError; 
 end;
end;

procedure tNetAddr.ToSocket( var sockaddr :tSockAddrL );
  begin
  if data.port>0 then begin
    if CompareByte(data.ip46_prefix,cNetAddrIP46_prefix,12)=0 then
    with tInetSockAddr(pointer(@sockaddr)^) do begin
      sin_family:=Sockets.AF_INET;
      sin_port:=data.port;
      sin_addr:=data.ip4;
    end else
    with tInetSockAddr6(pointer(@sockaddr)^) do begin
      sin6_family:=Sockets.AF_INET6;
      sin6_port:=data.port;
      sin6_flowinfo:=0;
      sin6_addr:=data.ip6;
      sin6_scope_id:=0;
    end;
  end else begin
    AbstractError;
  end;
end;

procedure tNetAddr.FromSocket( var sockaddr :tSockAddrL );
begin
 case sockaddr.sa_family of
  Sockets.AF_INET: begin
   data.port:=sockaddr_in(pointer(@sockaddr)^).sin_port;
   data.ip46_prefix:=cNetAddrIP46_prefix;
   data.ip4:=sockaddr_in(pointer(@sockaddr)^).sin_addr;
  end;
  Sockets.AF_INET6: begin
   data.port:=sockaddr_in6(pointer(@sockaddr)^).sin6_port;
   data.ip6:=sockaddr_in6(pointer(@sockaddr)^).sin6_addr;
  end;
  else raise Exception.Create('Unknown AF '+IntToStr(sockaddr.sa_family));
 end;
end;

procedure tNetAddr.ToString( var str :String );
  begin
  if data.port>0 then begin
    if CompareByte(data.ip46_prefix,cNetAddrIP46_prefix,12)=0 then begin
      str:='//ip4/'+Sockets.NetAddrToStr(data.ip4)
      +'/'+IntToStr(BETON(data.port));
    end else begin
      str:='//ip6/'+Sockets.NetAddrToStr6(data.ip6)
      +'/'+IntToStr(BETON(data.port));
    end;
  end else if self.isNil then begin
    str:='//nil';
  end else begin
    str:='//nil/UnknownAddressFamily';
  end;
end;

procedure tNetAddr.FromString( str :String );
 var i:integer;
 var fam:string;
 begin
 if System.Length(str)=0 then begin Clear; exit end;
 if Copy(str,1,2)<>'//' then raise eConvertError.Create('');
 Delete(str,1,2);
 i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
 fam:=copy(str,1,i-1);
 delete(str,1,i);
 if fam='ip4' then begin
  data.ip46_prefix:=cNetAddrIP46_prefix;

  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.ip4:=StrToNetAddr(fam);
  
  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.port:=NTOBE(word(StrToInt(fam)));
  
 end else if fam='ip6' then begin

  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.ip6:=StrToNetAddr6(fam);
  
  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.port:=NTOBE(word(StrToInt(fam)));
  
 end else if fam='nil' then begin
  Clear;
 end else raise eConvertError.Create('');
end;

function IntHash(init:LongWord;const data;len:longword):LongWord;
  var h:LongWord absolute init;
  var i:longword;
  procedure hashstep(v:byte);
    inline;
    begin
      h:=((h shl 5)and $FFFF) xor ((h shr 2)and $FFFF) xor v;
  end;
  begin
  for i:=0 to len do begin
    hashstep(byte((@data+i)^));
  end;
  result:=h;
end;

const cLocalHostIP4:Sockets.tInAddr=( s_bytes:(127,0,0,1) );
const cLocalIP4Port:word=1030;

procedure tNetAddr.LocalHost( af: tFamily );
 begin
 case af of
  afInet: begin
   data.port:=NTOBE(cLocalIP4Port);
   data.ip4:=cLocalHostIP4;
  end;
  afNil: Clear;
  else AbstractError;
 end;
end;

procedure tNetAddr.Clear;
 begin
 FillChar(data,sizeof(data),0);
end;

function  tNetAddr.isNil:boolean;
 const zero:array[1..18] of byte
   =(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
 begin
 isNil:= CompareByte(zero,data,18)=0;
 assert(sizeof(data)=18);
end;

operator := ( at :tNetAddr) aString : string;
 begin
 at.ToString( aString );
end;
operator := ( aString : string) at :tNetAddr;
 begin
 at.FromString( aString );
end;

operator := (net : Word2) host:word;
 var pnet:^word;
 begin
 pnet:=@net;
 host:=BETON( pnet^ );
end;
operator := (net : Word4) host:Dword;
 begin
 host:=BEtoN( DWORD(pointer(@net)^) );
end;

operator := (host : word) net:Word2;
 var pnet:^Word;
 begin
 pnet:=@net;
 pnet^:= NTOBE( host );
end;
operator := (host : Dword) net:Word4;
 begin
 DWORD(pointer(@net)^):=NtoBE( host );
end;

{*** Task ***}

procedure tTask.TaskIntAttach( callback:tTaskCallback; weak:boolean );
  var i:integer;
  begin
  for i:=0 to subscriberSize-1
    do if not assigned(subscriber[i].cb) then begin
      subscriber[i].cb:=callback;
      subscriber[i].weak:=weak;
      if not weak then inc(ObserversCount);
      exit;
  end;
  i:=subscriberSize;
  subscriberSize:=subscriberSize*2;
  ReAllocMem(subscriber,subscriberSize*sizeof(tTask_SubItem));
  subscriber[i].cb:=callback;
  subscriber[i].weak:=weak;
  if not weak then inc(ObserversCount);
  for i:=i to subscriberSize-1
    do subscriber[i].cb:=nil;
end;

procedure tTask.SendEvent(event:tTaskEvent; data:pointer);
  var i:integer;
  begin
  inc(inSendEvent);
  if event in [tevComplete,tevError] then complSent:=true;
  for i:=0 to subscriberSize-1
  do if assigned(subscriber[i].cb)
  then begin
    subscriber[i].cb(@self,event,data);
  end;
  if (ObserversCount=0) and (inSendEvent=1) then begin
    SendEvent(tevClose,nil);
    if not complSent then Abort;
    Cleanup;
  end else dec(inSendEvent);
end;

procedure tTask.Detach( callback:tTaskCallback );
  var i:integer;
  begin
  assert(assigned(callback),'wtf detach nil');
  for i:=0 to subscriberSize-1
  do if subscriber[i].cb=callback then begin
    subscriber[i].cb:=nil;
    if not subscriber[i].weak then dec(ObserversCount);
  end;
  if (ObserversCount=0) and (inSendEvent=0) then begin
    inc(inSendEvent);
    SendEvent(tevClose,nil);
    if not complSent then Abort;
    Cleanup;
  end;
end;

procedure tTask.Cleanup;
  begin
  FreeMem(subscriber,subscriberSize*sizeof(tTask_SubItem));
end;
procedure tTask.Abort;
  begin
end;
constructor tTask.Init;
  var i:integer;
  begin
  complSent:=false;
  typeid:=0;
  inSendEvent:=0;
  subscriberSize:=6;
  subscriber:=GetMem(subscriberSize*sizeof(tTask_SubItem));
  for i:=0 to subscriberSize-1
    do subscriber[i].cb:=nil;
end;

procedure tTask.Attach( subscriber:tTask_ptr; callback:tTaskCallback);
  begin
  TaskIntAttach(callback,false);
  if assigned(subscriber) then subscriber^.SendEvent(tevSubTask,@self);
end;
procedure tTask.Attach( callback:tTaskCallback );
  begin
  TaskIntAttach(callback,false);
end;
procedure tTask.AttachWeak( callback:tTaskCallback );
  begin
  TaskIntAttach(callback,true);
end;
function tTask.ProgressPct: single;
  begin ProgressPct:=0.5 end;
function tTask.GetSubtaskCount: integer;
  begin GetSubTaskCount:=0 end;
function tTask.GetSubTask(i:integer): tTask_ptr;
  begin GetSubTask:=nil end;

{*** FileStream ***}
procedure tFileStream.Seek(absolute:LongWord);
  begin if FileSeek(handle,absolute,fsFromBeginning)<>absolute
  then raise eInOutError.Create('File Seek Error'); end;
procedure tFileStream.Trunc(len:LongWord);
 begin if not FIleTruncate(handle,len)
 then raise eInOutError.Create('File Trunc Error'); end;
procedure tFileStream.Read(out buf; cnt:Word);
  begin  if FileRead(handle,buf,cnt)<>cnt
  then raise eInOutError.Create('File Read Error'); end;
procedure tFileStream.Write(const buf; cnt:word);
  begin if FileWrite(handle,buf,cnt)<>cnt
  then raise eInOutError.Create('File Write Error'); end;
constructor tFileStream.OpenRO(const fn:string);
  begin
  Inherited Init;
  handle:=FileOpen(fn, fmOpenRead or fmShareDenyWrite);
  if handle=-1 then raise eInOutError.Create('File Open for reading Error');
end;
function tFileStream.Length:LongWord;
  var pos:LongInt;
  begin
  pos:=FileSeek(handle,0,fsFromCurrent);
  result:=FileSeek(handle,0,fsFromEnd);
  Seek(pos);
end;
function tFileStream.Tell:LongWord;
  begin
  result:=FileSeek(handle,0,fsFromCurrent);
end;
constructor tFileStream.OpenRW(const fn:string);
  begin
  Inherited Init;
  handle:=FileOpen(fn, fmOpenReadWrite or fmShareDenyWrite);
  if handle=-1 then handle:=FileCreate(fn, %0110000000); {mode: -rw-------}
  if handle=-1 then raise eInOutError.Create('File Open read/write or Create Error '+fn);
end;
constructor tFileStream.OpenHandle(const ihandle:tHandle);
  begin
  inherited Init;
  handle:=ihandle;
end;
destructor tFileStream.Done;
  begin FileClose(handle); handle:=-1; end;

function SizeToString( v:LongWord):string;
  var f:LongWord;
  var e:byte;
  const chars:array [1..3] of char=('k','M','G');
  begin
  e:=0;
  while v>=1024 do begin
    inc(e);
    f:=v mod 1024;
    v:=v div 1024;
  end;
  if e<1
  then result:=IntToStr(v)
  else if f>100
  then result:=IntToStr(v)+chars[e]+IntToStr(round(f/100))
  else result:=IntToStr(v)+chars[e];
end;

{$I ObjectModel-cfg.pas}

(*** Find ***)

function FindIndex(a: ppointer; count: LongWord; key: pointer; compare: tComparePtrKeyFunc): LongWord;
  var l,r:LongWord;
  var cmp:shortint;
  begin {binary search}
  l:=0;
  r:=count;
  while l<r do begin
    result:=(l+r) div 2;
    assert(result<count); //just a safety check
    if result=count then exit;
    cmp:=compare(a[result],key);
    if cmp>0
      then r:=result-1
      else l:=result+1;
  end;
  result:=l;
end;

function PtrListShiftRight(a: ppointer; max, i:longword): boolean;
  var j:longint;
  begin
  if assigned(a[max]) then result:=false
  else begin
    for j:=max-1 downto i do a[j+1]:=a[j];
    result:=true;
  end;
end;

procedure PtrListShiftLeft(a: ppointer; max, i:longword);
  var j:longint;
  begin
  for j:=i to max-1 do a[j]:=a[j+1];
  a[max]:=nil;
end;


END.
