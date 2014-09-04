unit Keys;

INTERFACE

TYPE{s}

 tHash=object
  procedure Clear;
   experimental;
  function isNil :boolean;
   experimental;
  procedure ToString( var s :string );
   overload;
   experimental;
  function ToString :string;
   overload;
   deprecated;
  procedure FromString( s :string );
   experimental;
  private
  data :array [1..20] of byte;
 end;

 tFingerprint =tHash Deprecated;

Operator = (aa, ab :tHash) b : boolean;

VAR
 MyFilgerPrint: tFingerPrint unimplemented;
 
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
 for i:=low(data) to high(data)
  do data[i]:=StrToInt( 'x'+Copy(s, (2*i)-1, 2) );
end;

Operator = (aa, ab :tHash) b : boolean;
var i :byte;
begin
 b:=false;
 for i:=low(aa.data) to high(aa.data) do if aa.data[i]<>ab.data[i] then exit;
 b:=true;
end;


END.
