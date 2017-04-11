unit ObjectModel;

{$HINT bug, netaddr crashes to convert sometimes, isNil fails on port 0, toString fails when port 0 and not isNil}

{$mode objfpc}
{$PACKENUM 1}
INTERFACE
USES SysUtils,Sockets,StrUtils,Classes;

(*** Basic Types ***)

type tKey16=packed array [0..15] of byte;
type tKey20=packed array [0..19] of byte;
type tKey24=packed array [0..23] of byte;
type tKey32=packed array [0..31] of byte;
type tKey64=packed array [0..63] of byte;

type Word2=array [1..2] of byte; {0..65535}
type Word3=array [1..3] of byte; {todo}
type Word4=array [1..4] of byte; {0..4294967295}
type Word6=array [1..6] of byte; {todo}
type Word8=array [1..8] of byte; {todo}

(*** Simple Functions ***)

procedure ToHex(hexValue:pChar; const orig; len:word); inline;
function ToHexStr(const orig; len:word): ansistring;
function  UnHex(out BinValue; HexValue: PChar; BinBufSize: Integer): Integer; inline;
function PrefixLength(const a,b:tKey20):byte;
function SizeToString( v:SizeUInt ): string;
function IntHash(init:LongWord;const data;len:longword):LongWord;
function UnixNow:Int64;
function StrCompAt(const a: string; ofs:longword; const b: string): boolean;

(*** Base Object types ***)

(*** Additional functions for Streams ***)
type tStreamHelper = class helper for tStream
  function  ReadShortString:ansistring;
  procedure WriteShortString(s:ansistring);
  function  ReadStringAll:ansistring; inline;
  procedure WriteStringAll(s:ansistring); inline;
  procedure W1 (b: byte); inline;
  procedure W2 (b: Word); inline;
  procedure W4 (b: DWord); inline;
  procedure W6 (b: Int64);
  function R1: byte; inline;
  function R2: Word; inline;
  function R4: DWord; inline;
  function R6: Int64;
  procedure RB(out buf; cnt:LongWord); inline;
  procedure WB(const buf; cnt:LongWord); inline;
  procedure SeekB(const Offset: Int64); inline;
  procedure Skip(const displacement: LongInt); inline;
  procedure WriteZero(count:LongWord);
  function  Left: LongWord;
end;

type tSizeFix=object
  stream:tStream;
  pos:Int64;
  ws:byte;
  procedure Init(istream:tStream;iws:byte);
  procedure Fixup;
end;

type
  tTask=class;
  tTaskEvent=(tevComplete, tevError, tevClose, tevSubTask, tevUser, tevOther);
  tTaskCallback=procedure( task:tTask; event:tTaskEvent; data:pointer ) of object;
  TTask_SubList=^tTaskCallback;

  tTask=class(tObject)
    procedure Attach( subscriber:tTask; callback:tTaskCallback);
    procedure Attach( callback:tTaskCallback );
    procedure AttachWeak( callback:tTaskCallback );
    procedure Detach( callback:tTaskCallback );
    procedure DetachWeak( callback:tTaskCallback );
    function ProgressPct: single; virtual; {scaled by 10000}
    function GetSubtaskCount: integer; virtual;
    function GetSubTask(i:integer): tTask; virtual;
    constructor Create;
    protected
    ObserversCount,WeakObserversCount:word;
    procedure Abort; virtual;
    procedure Cleanup; virtual;
    procedure SendEvent(event:tTaskEvent; data:pointer);
    private
    subscriber: TTask_SubList;
    subscriberSize:word;
    inSendEvent:Word;
    complSent:boolean;
    procedure TaskIntAttach( callback:tTaskCallback; weak:boolean);
    procedure TaskIntDetach( callback:tTaskCallback; weak:boolean);
  end;

type
  TEventLogBaseSink=class(tObject)
    procedure LogMessage(
      const ident: string;
      level: TEventType;
      const Fmt: string;
      const Args: array of Const ); virtual;
    procedure FormatMessage(
      out msg: string;
      time: tDateTime;
      const ident: string;
      level: TEventType;
      const Fmt: string;
      const Args: array of Const ); virtual;
  end;

  TEventLog=class(tObject)
    procedure Log(
      level: TEventType;
      const Fmt: string;
      const Args: array of Const ); inline;
    constructor Create(trg: tEventLogBaseSink; const Aident: string);
    procedure Error( const Fmt: string; const Args: array of Const ); inline;
    procedure Warn( const Fmt: string; const Args: array of Const ); inline;
    procedure Info( const Fmt: string; const Args: array of Const ); inline;
    procedure Debug( const Fmt: string; const Args: array of Const ); inline;
    private
    ident: string;
    target: tEventLogBaseSink;
  end;

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

type tBufferStream=class(TCustomMemoryStream)
  private
  {FCapacity: Int64;}
  fAutoFree:boolean;
  public
  {property Capacity: Int64 read FCapacity;}
  property FreeOnDestroy: boolean read fAutoFree write fAutoFree;
  {constructor Create(Ptr: Pointer; ASize, ACapacity: PtrInt);}
  {constructor Create(ACapacity: PtrInt);}
  constructor Create(Ptr: Pointer; ASize: PtrInt);
  destructor Destroy; override;
  procedure SetPointer(Ptr: Pointer; ASize: PtrInt); inline;
  {function Write( const Buffer; Count: LongInt): LongInt; override;}
  {procedure Clear;}
  function  Left: LongWord;
  {function  SpaceLeft: LongWord;}
  {function  WritePtr: pointer;}
  {procedure WriteSkip(used:LongWord);}
  function ReadPtr(cnt:Word): pointer;
end;
{type TMemoryStream = class(Classes.TMemoryStream)
  (*Standard MS with configurable BlockSize*)
  (*Original has 4k blocks*)
  private
    FBlockSize: PtrInt;
  protected
    function Realloc(var NewCapacity: PtrInt): Pointer; override;
  public
    property BlockSize: PtrInt read FBlockSize;
end;}

(*** to/from-string Conversion Operators ***)

operator :=(a:pointer) r:shortstring;

operator :=(a:tKey20) r:string;
operator :=(a:string) r:tKey20;

operator :=(k:tKey24) s:string;
operator :=(a:string) r:tKey24;

operator :=(k:tKey32) s:string;
operator :=(a:string) r:tKey32;

operator := ( at : tNetAddr) aString : string;
operator := ( aString : string) at : tNetAddr;

(*** Host<>Net conversion operators ***)

operator := (net : Word2) host:word;
operator := (host : word) net:Word2;

operator := (net : Word4) host:Dword;
operator := (host : Dword) net:Word4;

operator := (net : Word6) host:Int64;
operator := (host: Int64) net:Word6;

operator = (a,b:tKey20) r:boolean;
operator = (a,b:tKey24) r:boolean;
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

type eXception=SysUtils.eXception;
type eInvalidBufferStreamAccess=class(Exception)
  end;
type eReadPastEoF=class(Exception)
  ActuallyReadBytes:LongWord;
  end;
type eInOutError=SysUtils.eInOutError;
type eFileNotFound=class(eInOutError)
  end;

function ConvertFamily( a:tFamily ): sa_family_t;
procedure SC(fn:pointer; retval:longint);

IMPLEMENTATION
uses
   Porting
  ,Errors
  ;

{*** MemStram ***}

operator  =(a,b:tKey20) r:boolean;
  begin
  r:=CompareDWord(a,b,5)=0;
end;
operator  =(a,b:tKey24) r:boolean;
  begin
  r:=CompareDWord(a,b,6)=0;
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

procedure ToHex(hexValue:pChar; const orig; len:word);
  begin
  StrUtils.BinToHex(pchar(@orig), hexValue, len);
end;
function  UnHex(out BinValue; HexValue: PChar; BinBufSize: Integer): Integer;
  begin
  result:=StrUtils.HexToBin(HexValue,pchar(@BinValue),BinBufSize);
end;
function ToHexStr(const orig; len:word): ansistring;
  begin
  SetLength(result,len*2);
  StrUtils.BinToHex(pchar(@orig), @result[1], len);
end;

operator :=(a:tKey20) r:string;
  begin
  SetLength(r,40);
  ToHex(@r[1], a, 20);
end;
operator :=(a:string) r:tKey20;
  begin
  if UnHex(r,@a[1],20)<20 then raise
  eConvertError.Create('Invalid Hex String');
end;

operator :=(k:tKey24) s:string;
  begin
  SetLength(s,48);
  ToHex(@s[1], k, 24);
end;
operator :=(a:string) r:tKey24;
  begin
  if UnHex(r,@a[1],24)<24 then raise
  eConvertError.Create('Invalid Hex String');
end;

operator :=(k:tKey32) s:string;
 begin
 Setlength(s,64);
 ToHex(@s[1],k,32);
end;
operator :=(a:string) r:tKey32;
  begin
  if UnHex(r,@a[1],32)<32 then raise
  eConvertError.Create('Invalid Hex String');
end;

procedure SC(fn:pointer; retval:longint);
  var opn:string;
  begin
  if retval < 0 then begin
    if fn=pointer(@fpsocket) then opn:='socket'
    else if fn=pointer(@fpsetsockopt) then opn:='setsockopt'
    else if fn=pointer(@fpsendto) then opn:='sendto'
    else if fn=pointer(@fprecvfrom) then opn:='recvfrom'
    else opn:=Format('operation %P',[fn]);
    raise eXception.Create(opn+':'+StrError(SocketError));
  end;
end;

{
  FCapacity: Int64;
  public
  property Capacity: Int64 read FCapacity;
}
constructor tBufferStream.Create(Ptr: Pointer; ASize: PtrInt);
  begin
  Inherited Create;
  SetPointer(ptr,asize);
  fAutoFree:=false;
end;

destructor tBufferStream.Destroy;
  begin
  if fAutoFree and assigned(Memory) then FreeMem(Memory);
  SetPointer(nil,0);
  Inherited;
end;

procedure tBufferStream.SetPointer(Ptr: Pointer; ASize: PtrInt);
  begin
  inherited SetPointer(ptr,asize);
end;

{constructor tBufferStream.Create(ACapacity: PtrInt);
  var ad:pointer;
  begin
  Inherited Create;
  SetPointer(nil,0);
  ad:=GetMem(ACapacity);
  SetPointer(ad,0);
  fAutoFree:=true;
  FCapacity:=ACapacity;
  fAutoFree:=true;
end;}

{function tBufferStream.Write( const Buffer; Count: LongInt): LongInt;
  var NewPos: PtrInt;
  var fposition:PtrInt;
  begin
  fposition:=GetPosition;
  NewPos:=fposition+count;
  if NewPos>GetSize then begin
    if NewPos>FCapacity then begin
      NewPos:=FCapacity;
      Count:=NewPos-fposition;
    end;
    SetSize(NewPos);
  end;
  System.Move (Buffer,(Memory+fposition)^,Count);
  Position:=NewPos;
  Result:=Count;
end;}

function  tBufferStream.Left: LongWord;
  begin
  result:=Position-Size;
end;

function tBufferStream.ReadPtr(cnt:Word): pointer;
  var NewPos: PtrInt;
  begin
  NewPos:=Position+cnt;
  if NewPos<=Size then begin
    result:=(Memory+Position);
    Position:=NewPos;
  end else raise EReadError.Create('Buffer.ReadPtr out of bounds');
end;

{function TMemoryStream.Realloc(var NewCapacity: PtrInt): Pointer;
  begin
  If NewCapacity<0 Then
    NewCapacity:=0
  else
    begin
      // if growing, grow at least a quarter
      if (NewCapacity>FCapacity) and (NewCapacity < (5*FCapacity) div 4) then
        NewCapacity := (5*FCapacity) div 4;
      // round off to block size.
      NewCapacity := (NewCapacity + (BlockSize-1)) and not (BlockSize-1);
    end;
  // Only now check !
  If NewCapacity=FCapacity then
    Result:=FMemory
  else
    begin
      Result:=Reallocmem(FMemory,Newcapacity);
      If (Result=Nil) and (Newcapacity>0) then
        Raise EStreamError.Create(SMemoryStreamError);
    end;
end;}

procedure tStreamHelper.Skip(const displacement:LongInt);
  {$PUSH}{$RANGECHECKS ON}
  begin Seek(displacement,soFromCurrent) end;{$POP}
procedure tStreamHelper.SeekB(const Offset: Int64);
  begin
  Seek(Offset,soFromBeginning)
end;
function  tStreamHelper.R1:byte;
  begin self.ReadBuffer(result,1) end;
procedure tStreamHelper.W1(b:byte);
  begin self.WriteBuffer(b,1) end;
function tStreamHelper.Left:LongWord;
  begin Left:=Size-Position end;
function tStreamHelper.ReadShortString:ansistring;
  var l:byte;
  begin
  l:=ReadByte;
  SetLength(result,l);
  ReadBuffer(result[1],l);
end;
function tStreamHelper.ReadStringAll:ansistring;
  var l:byte;
  begin
  l:=Left;
  SetLength(result,l);
  ReadBuffer(result[1],l);
end;
procedure tStreamHelper.WriteShortString(s:ansistring);
  begin
  WriteByte(System.Length(s));
  WriteBuffer(s[1],System.Length(s));;
end;
procedure tStreamHelper.WriteStringAll(s:ansistring);
  begin
  WriteBuffer(s[1],System.Length(s));;
end;

function tStreamHelper.R2:word;
  begin
  ReadBuffer(result,2);
  result:=beton(result);
end;
function tStreamHelper.R4:dword;
  begin
  ReadBuffer(result,4);
  result:=beton(result);
end;
function tStreamHelper.R6:Int64;
  begin
  result:=0;
  ReadBuffer((pointer(@result)+2)^,6);
  result:=beton(result);
end;

procedure tStreamHelper.W2(b:word);
  begin
  b:=ntobe(b);
  WriteBuffer(b,2);
end;
procedure tStreamHelper.W4(b:dword);
  begin
  b:=ntobe(b);
  WriteBuffer(b,4);
end;
procedure tStreamHelper.W6(b:Int64);
  begin
  b:=ntobe(b);
  WriteBuffer((pointer(@b)+2)^,6);
end;

procedure tStreamHelper.RB(out buf; cnt:LongWord);
  begin
  ReadBuffer(buf,cnt);
end;
procedure tStreamHelper.WB(const buf; cnt:LongWord);
  begin
  WriteBuffer(buf,cnt);
end;

procedure tStreamHelper.WriteZero(count:LongWord);
  var z:QWORD;
  var rc:LongInt;
  begin
  z:=0;
  while count>=8 do begin
    rc:=Write(z,8);
    if rc<=0 then Raise EWriteError.Create('Error while writing zeros');
    count:=count-rc;
  end;
  if (count and 4) =4 then WriteBuffer(z,4);
  if (count and 2) =2 then WriteBuffer(z,2);
  if (count and 1) =1 then WriteBuffer(z,1);
end;

procedure tSizeFix.Init(istream:tStream; iws:byte);
  begin
  stream:=istream;
  ws:=iws;
  pos:=stream.position;
end;

procedure tSizeFix.Fixup;
  var size:Int64;
  begin
  {$PUSH}{$Q+}{$R+}
  size:=stream.Position-pos;
  stream.Position:=pos;
  case ws of
    1: stream.W1(size);
    2: stream.W2(size);
    4: stream.W4(size);
  end;
  stream.Position:=pos+size;
  {$POP}
end;

function StrCompAt(const a: string; ofs:longword; const b: string): boolean;
  var i: longword;
  begin
  StrCompAt:=false;
  if (length(b)+ofs)>length(a) then exit;
  ofs:=ofs-1;
  for i:=1 to length(b) do begin
    if a[i+ofs]<>b[i] then exit;
  end;
  StrCompAt:=true;
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

function UnixNow:Int64;
  begin
  result:=po_unixtimenow;
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

operator := (net : Word6) host:Int64;
  begin
  host:= net[6]
     or (net[5] shl  8)
     or (net[4] shl 16)
     or (net[3] shl 24)
     or (net[2] shl 32)
     or (net[1] shl 40);
end;

operator := (host: Int64) net:Word6;
 begin
 net[1]:=(host shr 40)and $FF;
 net[2]:=(host shr 32)and $FF;
 net[3]:=(host shr 24)and $FF;
 net[4]:=(host shr 16)and $FF;
 net[5]:=(host shr 8)and $FF;
 net[6]:=host and $FF;
end;

{*** Logging ***}

procedure tEventLog.Error( const Fmt: string; const Args: array of Const );
  begin  Target.LogMessage(ident,etError,fmt,args)  end;
procedure tEventLog.Warn( const Fmt: string; const Args: array of Const );
  begin  Target.LogMessage(ident,etWarning,fmt,args)  end;
procedure tEventLog.Info( const Fmt: string; const Args: array of Const );
  begin  Target.LogMessage(ident,etInfo,fmt,args)  end;
procedure tEventLog.Debug( const Fmt: string; const Args: array of Const );
  begin  Target.LogMessage(ident,etDebug,fmt,args)  end;
procedure tEventLog.Log(
    level: TEventType;
    const Fmt: string;
    const Args: array of Const );
  begin
  Target.LogMessage(ident,level,fmt,args);
end;

constructor tEventLog.Create(trg: tEventLogBaseSink; const Aident: string);
  begin
  Inherited Create;
  ident:=aident;
  target:=trg;
  assert(assigned(target));
end;

procedure tEventLogBaseSink.FormatMessage(
    out msg: string;
    time: tDateTime;
    const ident: string;
    level: TEventType;
    const Fmt: string;
    const Args: array of Const );
  begin
  msg:=FormatDateTime('DDMM-hh:nn:ss',time);
  case level of
    etwarning: msg:=msg+' Warning';
    etError: msg:=msg+' Error';
    etDebug: msg:=msg+' Debug';
    //etInfo: msg:=msg+' Info';
    //else msg:=msg+' Other';
  end;
  msg:=msg+' '+ident+Format(fmt,args);
end;

procedure tEventLogBaseSink.LogMessage(
    const ident: string;
    level: TEventType;
    const Fmt: string;
    const Args: array of Const );
  var msg:String;
  begin
  Self.FormatMessage(msg,now,ident,level,fmt,args);
  writeln(stderr,msg);
end;

{*** Task ***}

procedure tTask.TaskIntAttach( callback:tTaskCallback; weak:boolean );
  var i:integer;
  begin
  assert(assigned(callback),'attach nil');
  for i:=0 to subscriberSize-1
    do if not assigned(subscriber[i]) then begin
      subscriber[i]:=callback;
      if weak then inc(weakObserversCount)
              else inc(ObserversCount);
      exit;
  end;
  i:=subscriberSize;
  subscriberSize:=subscriberSize*2;
  ReAllocMem(subscriber,subscriberSize*sizeof(subscriber^));
  subscriber[i]:=callback;
  if weak then inc(weakObserversCount)
          else inc(ObserversCount);
  for i:=i to subscriberSize-1
    do subscriber[i]:=nil;
end;

procedure tTask.SendEvent(event:tTaskEvent; data:pointer);
  var i:integer;
  begin
  if event in [tevComplete,tevError] then complSent:=true;
  for i:=0 to subscriberSize-1
  do if assigned(subscriber[i])
  then begin
    subscriber[i](self,event,data);
  end;
  if event in [tevComplete,tevError] then begin
    Cleanup;
  end;
end;

procedure tTask.TaskIntDetach( callback:tTaskCallback; weak: boolean );
  var i:integer;
  begin
  assert(assigned(callback),'detach nil');
  for i:=0 to subscriberSize-1
  do if CompareByte(subscriber[i],callback,sizeof(callback))=0 then begin
    subscriber[i]:=nil;
    if weak then dec(weakObserversCount)
            else dec(ObserversCount);
  end;
  if ObserversCount=0 then begin
    {all non-weak detached -> abort and send event}
    {only weak observers left, send event to all}
    SendEvent(tevClose,nil);
    if not complSent then Abort;
    Cleanup;
  end;
end;

procedure tTask.Cleanup;
  begin
  FreeMem(subscriber,subscriberSize*sizeof(subscriber^));
end;
procedure tTask.Abort;
  begin
end;
constructor tTask.Create;
  var i:integer;
  begin
  complSent:=false;
  inSendEvent:=0;
  subscriberSize:=6;
  subscriber:=GetMem(subscriberSize*sizeof(subscriber^));
  for i:=0 to subscriberSize-1
    do subscriber[i]:=nil;
end;

procedure tTask.Attach( subscriber:tTask; callback:tTaskCallback);
  begin
  TaskIntAttach(callback,false);
  if assigned(subscriber) then subscriber.SendEvent(tevSubTask,@self);
end;
procedure tTask.Attach( callback:tTaskCallback );
  begin
  TaskIntAttach(callback,false);
end;
procedure tTask.Detach(callback:tTaskCallback);
  begin
  TaskIntDetach(callback,false);
end;
procedure tTask.DetachWeak( callback:tTaskCallback );
  begin
  TaskIntDetach(callback,true);
end;
procedure tTask.AttachWeak( callback:tTaskCallback );
  begin
  TaskIntAttach(callback,true);
end;
function tTask.ProgressPct: single;
  begin ProgressPct:=0.5 end;
function tTask.GetSubtaskCount: integer;
  begin GetSubTaskCount:=0 end;
function tTask.GetSubTask(i:integer): tTask;
  begin GetSubTask:=nil end;

{*** FileStream ***
procedure tFileStream.Seek(absolute:LongWord);
  begin if FileSeek(handle,absolute,fsFromBeginning)<>absolute
  then raise eInOutError.Create('File Seek Error'); end;
procedure tFileStream.Skip(dis:LongInt);
  begin if FileSeek(handle,dis,fsFromCurrent)<0
  then raise eInOutError.Create('File Seek (skip) Error'); end;
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
  if handle=-1 then handle:=FileCreate(fn, %0110000000); //mode: -rw-------
  if handle=-1 then raise eInOutError.Create('File Open read/write or Create Error '+fn);
end;
constructor tFileStream.OpenHandle(const ihandle:tHandle);
  begin
  inherited Init;
  handle:=ihandle;
end;
destructor tFileStream.Done;
  begin FileClose(handle); handle:=-1; end;
procedure tFileStream.ReadAsMuch(out buf; var cnt:LongWord);
  var rv:LongInt;
  begin
  rv:=FileRead(handle,buf,cnt);
  if rv<0 then raise eInOutError.Create('File Read Error');
  cnt:=rv;
end;}

function SizeToString( v:SizeUInt ):string;
  var f:LongWord;
  var e:byte;
  const chars:array [1..4] of char=('k','M','G', 'T');
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
