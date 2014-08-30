unit Keys;

INTERFACE

TYPE{s}

 tHash=object
  procedure Clear;
   experimental;
  function isNil :boolean;
   experimental;
  procedure ToString( var s :string );
   experimental;
  procedure FromString( s :string );
   experimental;
  private
  data :array [0..20] of byte;
 end;

 tFingerprint =tHash Deprecated;

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

procedure tHash.FromString( s :string );
var i :byte;
begin
 for i:=low(data) to high(data)
  do data[i]:=StrToInt( 'x'+Copy(s, (2*i)-1, 2) );
end;

END.
