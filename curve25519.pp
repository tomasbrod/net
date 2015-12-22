{$mode objfpc}
unit curve25519;
interface

{
  curve25519_athlon.h version 20050915
  D. J. Bernstein
  Public domain.
  Converted with h2pas
}

{$L curve25519.a}

type t256BitKey=packed array [0..31] of byte;
const basepoint:t256BitKey=(9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
procedure curve25519_athlon(_para1:pointer; _para2:pointer; _para3:pointer);cdecl;external;
procedure curve25519(out ek:t256BitKey; const e:t256BitKey; const k:t256BitKey);

implementation

procedure curve25519(out ek:t256BitKey; const e:t256BitKey; const k:t256BitKey);
 begin
 curve25519_athlon(@ek,@e,@k);
end;

end.
