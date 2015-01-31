unit Keys;

INTERFACE

uses Classes;

TYPE{s}

 tHash= packed object
  procedure Clear;
  function isNil :boolean;
  procedure ToString( var s :string );
   overload;
  function ToString :string;
   overload;
   deprecated;
  procedure FromString( s :string );
  procedure Compute ( s: tStream ); overload;
  procedure Compute ( var s: file; count:longword ); overload;
  procedure Compute ( s: string ); overload;
  function LastLongWord:LongWord;
  private
  data :array [1..20] of byte;
 end;

Operator = (aa, ab :tHash) b : boolean;

operator := ( ahash: tHash ) astring : string;
operator := ( astring: string ) ahash : tHash;
Operator := (aa :tHash) bb:LongWord;

IMPLEMENTATION
uses SysUtils, sha1;

procedure tHash.Clear;
var i:byte;
begin for i:=low(data) to high(data) do data[i]:=0; end;

function  tHash.isNil :boolean;
var i:byte;
begin
 isNil:=true;
 for i:=low(data) to high(data) do if data[i]<>0 then isNil:=false;
end;

procedure tHash.ToString( var s :string );
var i :byte;
begin
 s:='';
 for i:=low(data) to high(data)
  do s:=s+IntToHex(data[i],2);
end;

function tHash.ToString :string;
var s:string;
begin
 ToString(s);
 ToString:=s;
end;


procedure tHash.FromString( s :string );
var i :byte;
begin
 if length(s)=0 then begin Clear; exit; end;
 if length(s)<>(High(data)*2) then raise eConvertError.Create('Invalid hexadecimal number');
 for i:=low(data) to high(data)
  do data[i]:=StrToInt( 'x'+Copy(s, (2*i)-1, 2) );
end;

procedure tHash.Compute ( s: tStream ); overload;
 var ctx:tSHA1Context;
 var digest:tSHA1Digest;
 var buf: array [1..512] of byte;
 var bl: word;
 begin
 SHA1Init( ctx );
 while true do begin
  bl:= s.Read( buf, sizeof(buf) );
  if bl=0 then break;
  SHA1Update( ctx, buf, bl );
 end;
 SHA1Final( ctx, digest );
 for bl:=0 to 19 do data[bl+1]:=digest[bl];
end;

procedure tHash.Compute ( var s: file; count:longword );
 var ctx:tSHA1Context;
 var digest:tSHA1Digest;
 var buf: array [1..512] of byte;
 var bl: longword;
 begin
 SHA1Init( ctx );
 while count>0 do begin
  bl:=count; if bl>sizeof(buf) then bl:=sizeof(buf);
  BlockRead( s, buf, bl, bl );
  Dec(count,bl);
  if bl=0 then break;
  SHA1Update( ctx, buf, bl );
 end;
 SHA1Final( ctx, digest );
 for bl:=0 to 19 do data[bl+1]:=digest[bl];
end;

procedure tHash.Compute ( s: string ); overload;
 var digest:tSHA1Digest;
 var bl: word;
 begin
 digest:=SHA1String( s );
 for bl:=0 to 19 do data[bl+1]:=digest[bl];
end;

function tHash.LastLongWord:LongWord;
 begin
 result:=data[20] or (data[19]<<8) or (data[18]<<16) or (data[15]<<24);
end;

Operator := (aa :tHash) bb:LongWord;
 begin
 bb:=aa.LastLongWord;
end;

Operator = (aa, ab :tHash) b : boolean;
var i :byte;
begin
 b:=false;
 for i:=low(aa.data) to high(aa.data) do if aa.data[i]<>ab.data[i] then exit;
 b:=true;
end;

operator := ( ahash: tHash ) astring : string;
 begin
 ahash.ToString( astring );
end;
operator := ( astring: string ) ahash : tHash;
 begin
 ahash.FromString( astring );
end;

END.
