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
  tTaskEvent=(tevComplete, tevError, tevClose, tevSubTask);
  tTaskCallback=procedure( task:tTask_ptr; event:tTaskEvent; data:pointer ) of object;
  tTask_SubItem=record
    cb: tTaskCallback;
    weak:boolean;
  end;

  tTask=object
    procedure Attach( subscriber:tTask_ptr; callback:tTaskCallback);
    procedure Attach( callback:tTaskCallback );
    procedure AttachWeak( callback:tTaskCallback );
    procedure Detach( callback:tTaskCallback );
    function ProgressPct: word; virtual; {scaled by 10000}
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

(*** Derived Object Types ***)

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
    function Hash:Word; deprecated;
    procedure LocalHost( af: tFamily );
    procedure Clear;
    function  isNil:boolean;
    public
    data :packed record
      case Family : tFamily of
      afInet :( inet :packed record 
        port: Word;
        addr: tInAddr;
      end; );
      afInet6 :( inet6 :packed record 
        port: Word;
        addr: tIn6Addr;
      end; );
      afNil :(
        pad_pV4IlkA4mKQL :packed array [0..22] of byte;
      ); 
    end{record};
  end{object};

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
    constructor OpenRO(const fn:string);
    constructor OpenRW(const fn:string);
    destructor Done;
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

operator  =(a,b:tKey20) r:boolean;

(*** Other ***)

type eInvalidMemStreamAccess=class(Exception)
  end;
type eReadPastEoF=class(Exception)
  ActuallyReadBytes:LongWord;
  end;
type eFileNotFound=class(eInOutError)
  end;

Operator = (aa, ab :tNetAddr) b : boolean;

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
 begin result:=length-position end;
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
  r:=IntToHex(LongInt(a),8);
end;

{*** NetAddr ***}

Operator = (aa, ab :Sockets.tInAddr) b : boolean;
begin
 b:=aa.s_addr=ab.s_addr;
end;

Operator = (aa, ab :tNetAddr) b : boolean;
begin
 b:=false;
 if aa.data.Family<>ab.data.Family then exit;
 case aa.data.Family of
  afInet: if (aa.data.inet.port<>ab.data.inet.port) or (aa.data.inet.addr<>ab.data.inet.addr) then exit;
  afNil: {null addresses are always equal};
  else AbstractError; 
 end;
 b:=true;
end;

function tNetAddr.Length :Word;
begin
 result:=(sizeof(self)-sizeof(data))+sizeof(data.Family);
 case data.Family of
  afNil: ;
  afInet: result+=sizeof( data.inet );
  afInet6: result+=sizeof( data.inet6 );
  else result:=sizeof(self);
 end;
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
 case data.family of
  afInet: begin
   sockaddr.sa_family:=Sockets.AF_INET;
   Move(data.inet, sockaddr.sa_data, sizeof(data.inet) );
  end;
  afInet6: begin
   sockaddr.sa_family:=Sockets.AF_INET6;
   with tInetSockAddr6(pointer(@sockaddr)^) do begin
    sin6_port:=data.inet6.port;
    sin6_flowinfo:=0;
    sin6_addr:=data.inet6.addr;
    sin6_scope_id:=0;
   end;
  end;
  else AbstractError; 
 end;
end;

procedure tNetAddr.FromSocket( var sockaddr :tSockAddrL );
begin
 case sockaddr.sa_family of
  Sockets.AF_INET: begin
   data.family:=afInet;
   move(sockaddr.sa_data, data.inet, sizeof(data.inet) );
  end;
  Sockets.AF_INET6: begin
   data.family:=afInet6;
   move(sockaddr.sa_data, data.inet6, sizeof(data.inet6) );
  end;
  else raise Exception.Create('Unknown AF '+IntToStr(sockaddr.sa_family));
 end;
end;

procedure tNetAddr.ToString( var str :String );
 begin
 case data.Family of
  afInet: begin
   str:='//ip4/'+Sockets.NetAddrToStr(data.inet.addr)+
    '/'+IntToStr(BETON(data.inet.port));
  end;
  afInet6: begin
   str:='//ip6/'+Sockets.NetAddrToStr6(data.inet6.addr)+
    '/'+IntToStr(BETON(data.inet6.port));
  end;
  afNil: str:='//nil';
  else str:='//nil/UnknownAddressFamily';
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
  data.family:=afInet;

  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.inet.addr:=StrToNetAddr(fam);
  
  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.inet.port:=NTOBE(StrToInt(fam));
  
 end else if fam='ip6' then begin
  data.family:=afInet6;

  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.inet6.addr:=StrToNetAddr6(fam);
  
  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.inet6.port:=NTOBE(StrToInt(fam));
  
 end else if fam='nil' then begin
  data.family:=afNil;
 end else raise eConvertError.Create('');
end;

function tNetAddr.Hash:word;
 var h:word;
 var i:byte;
 procedure hashstep(v:byte);
  begin
  h:=((h shl 5)and $FFFF) xor ((h shr 2)and $FFFF) xor v;
 end;
 begin
 h:=0;
 assert(sizeof(data.family)=1,'simple set size'+IntToStr(sizeof(data.family)));
 hashstep(byte(data.family));
 case data.Family of
  afInet: for i:=1 to 4 do HashStep(data.inet.addr.s_bytes[i]);
  afInet6: for i:=1 to 16 do HashStep(data.inet6.addr.u6_addr8[i]);
  else AbstractError;
 end;
 case data.Family of
  afInet,afInet6: begin 
   HashStep(data.inet.port and $FF);
   HashStep((data.inet.port shr 8) and $FF);
  end;
 end;
 result:=h;
end;

const cLocalHostIP4:Sockets.tInAddr=( s_bytes:(127,0,0,1) );
const cLocalIP4Port:word=1030;

procedure tNetAddr.LocalHost( af: tFamily );
 begin
 data.Family:=af;
 case af of
  afInet: begin
   data.inet.port:=NTOBE(cLocalIP4Port);
   data.inet.addr:=cLocalHostIP4;
  end;
  afNil: ;
  else AbstractError;
 end;
end;

procedure tNetAddr.Clear;
 begin
 self.data.family:=afNil;
end;

function  tNetAddr.isNil:boolean;
 begin
 isNil:= self.data.family=afNil;
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
  inSendEvent:=0;
  subscriberSize:=6;
  subscriber:=GetMem(subscriberSize*sizeof(tTask_SubItem));
  for i:=0 to subscriberSize-1
    do subscriber[i].cb:=nil;
end;

procedure tTask.Attach( subscriber:tTask_ptr; callback:tTaskCallback);
  begin
  TaskIntAttach(callback,false);
  subscriber^.SendEvent(tevSubTask,@self);
end;
procedure tTask.Attach( callback:tTaskCallback );
  begin
  TaskIntAttach(callback,false);
end;
procedure tTask.AttachWeak( callback:tTaskCallback );
  begin
  TaskIntAttach(callback,true);
end;
function tTask.ProgressPct: word;
  begin ProgressPct:=5000 end;
function tTask.GetSubtaskCount: integer;
  begin GetSubTaskCount:=0 end;
function tTask.GetSubTask(i:integer): tTask_ptr;
  begin GetSubTask:=nil end;

{*** FileStream ***}
procedure tFileStream.Seek(absolute:LongWord);
  begin if FileSeek(handle,absolute,fsFromBeginning)<>absolute
  then raise eInOutError.Create('File Seek Error'); end;
procedure tFileStream.Read(out buf; cnt:Word);
  begin  if FileRead(handle,buf,cnt)<>cnt
  then raise eInOutError.Create('File Read Error'); end;
procedure tFileStream.Write(const buf; cnt:word);
  begin if FileWrite(handle,buf,cnt)<>cnt
  then raise eInOutError.Create('File Write Error'); end;
constructor tFileStream.OpenRO(const fn:string);
  begin
  Inherited Init;
  handle:=FileOpen(fn, fmOpenRead);
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
  handle:=FileOpen(fn, fmOpenReadWrite);
  if handle=-1 then handle:=FileCreate(fn, %0110000000); {mode: -rw-------}
  if handle=-1 then raise eInOutError.Create('File Open read/write or Create Error');
end;
destructor tFileStream.Done;
  begin FileClose(handle); end;


END.
