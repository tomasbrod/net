unit ContentHash;

INTERFACE

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
  function LastLongWord:LongWord;
  protected
  data :array [1..20] of byte;
 end;

type t=tHash;

Operator = (aa, ab :tHash) b : boolean;

operator := ( ahash: tHash ) astring : string;
operator := ( astring: string ) ahash : tHash;
Operator := (aa :tHash) bb:LongWord;

IMPLEMENTATION
uses SysUtils;

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
