uses SysUtils,ObjectModel,crypto,ed25519;

var bc: array [0..2,0..7] of LongWord;
var tc: LongWord;
var rndstate:tKey64;

procedure OneTry;
  var sec:tKey64;
  var pub:tKey32;
  var b,i:integer;
  begin
  SHA512_Buffer( sec, rndstate, 64);
  rndstate:=sec;
  {get a secret key}
  ed25519.CreatekeyPair(pub,sec);
  Inc(tc);
  for b:=0 to 2 do begin
    for i:=0 to 7 do begin
      if (pub[b] and (1 shl i)) >0 then
        inc(bc[b,i]);
    end;
  end;
end;

procedure Init;
  var b:integer;
  begin
  Randomize;
  for b:=0 to 63 do begin
    rndstate[b]:=random(256);
  end;
end;

procedure Show;
  var i:integer;
  begin
  writeln;
  writeln('Keys tried: ',tc);
  for i:=0 to 7 do begin
    write('Bit ',i,' ');
    writeln(bc[0,i]:10,' ',bc[1,i]:10,' ',bc[2,i]:10);
  end;
end;

procedure Run;
  begin
  Init;
  repeat
    OneTry;
    if (tc mod 256)=0 then write('.');
    if (tc mod 4096)=0 then Show;
  until false;
end;

begin run end.
