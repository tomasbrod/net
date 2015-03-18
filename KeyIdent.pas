unit KeyIdent;

INTERFACE

type t=packed object
 function IsNil:boolean;
 procedure Clear;
 protected
 data: array [0..8] of byte;
end;

operator  = (a, b :t) r : boolean;
operator := ( a: t ) astring : string;
operator := ( astring: string ) a : t;
operator := (a :t) r:LongWord;

IMPLEMENTATION
uses SysUtils;

operator  = (a, b :t) r : boolean;
 begin
 r:= (a.data[1]=b.data[1])and (a.data[2]=b.data[2])and 
 (a.data[3]=b.data[3])and (a.data[4]=b.data[4])and (a.data[5]=b.data[5])and 
 (a.data[6]=b.data[6])and (a.data[7]=b.data[7])and (a.data[8]=b.data[8]);
end;

procedure t.Clear;
var i:byte;
begin for i:=low(data) to high(data) do data[i]:=0; end;

function t.isNil :boolean;
var i:byte;
begin
 isNil:=true;
 for i:=low(data) to high(data) do if data[i]<>0 then isNil:=false;
end;

operator := ( a: t ) astring : string;
 var i:byte;
 begin
 SetLength(astring,0);
 for i:=low(a.data) to high(a.data) do
  astring:=astring+IntToHex(a.data[i],2);
end;

operator := ( astring: string ) a : t;
 var i,c:byte;
 begin
 if length(astring)=0 then begin a.Clear; exit; end;
 c:=1;
 for i:=low(a.data) to high(a.data) do begin
  if (astring[c]=' ')or(astring[c]='-') then inc(c,1);
  a.data[i]:=StrToInt( 'x'+Copy(astring, c, 2) );
  inc(c,2);
 end;
end;

operator := (a :t) r:LongWord;
 begin
 r:=a.data[8] or (a.data[7]<<8) or (a.data[6]<<16) or (a.data[5]<<24);
end;

END.