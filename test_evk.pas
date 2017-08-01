uses sysutils;
function evk(depth,i:byte; a,b:byte): byte;
  var z,k:integer;
  begin
  z:=depth-(i*8);
  if z<0 then k:=255
  else if z>7 then k:=0
  else k:=255 shr z;
  result:=(a and not k) or random(k+1);
end;

var i,depth:integer;
begin
  for depth:=0 to 159 do begin
    for i:=0 to 19 do begin
      write(format('%.02x',[evk(depth,i,$00,$FF)]));
    end;
    writeln;
  end;
end.
