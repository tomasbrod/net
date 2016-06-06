unit MemStream;
{$mode objfpc}
INTERFACE
uses SysUtils;

procedure BinToHex(hexValue:pChar; const orig; len:word);
operator :=(a:pointer) r:shortstring;

type tKey20=packed array [0..19] of byte;
type tKey32=packed array [0..31] of byte;
type tKey64=packed array [0..63] of byte;
operator :=(a:tKey20) r:string;
operator :=(a:string) r:tKey20;
function PrefixLength(const a,b:tKey20):byte;
operator  =(a,b:tKey20) r:boolean;
operator :=(k:tKey32) s:string;
operator :=(a:string) r:tKey32;

type tCommonStream=object
  constructor Init;

  procedure Seek(absolute:LongWord); virtual; abstract;
  function  Tell:LongWord; virtual; abstract;
  function  Length:LongWord; virtual; abstract;
  procedure Read(out buf; cnt:Word); virtual; abstract; overload;
  procedure Write(const buf; cnt:word); virtual; abstract; overload;

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
  //procedure WriteKey(v:tKey20);
  //procedure ReadKey(v:tKey20);
  end;
  pCommonStream=^tCommonStream;
type tMemoryStream=object(tCommonStream)
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

type eInvalidMemStreamAccess=class(Exception)
 {no details jet}
end;
type eReadPastEoF=class(Exception)
 {no details jet}
end;


IMPLEMENTATION
uses StrUtils;

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

(*
procedure tCommonStream.Seek(absolute:LongWord); begin AbstractError end;
function  tCommonStream.Tell:LongWord; begin AbstractError end;
procedure tCommonStream.Read(var buf; cnt:Word); begin AbstractError end;
procedure tCommonStream.Write(const buf; cnt:word); begin AbstractError end;
procedure tCommonStream.Trunc; begin AbstractError end;
*)

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

END.

