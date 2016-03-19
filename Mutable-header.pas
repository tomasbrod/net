{Mutable-header.pas}
type tMutHdr=record
  Magic:packed array [1..6] of byte;
  note: array [0..17] of char; {22 char string or <21 char zString}
  Day:Word4;
  Ver:Word4;
  Pub:tKey32;
  Sig:tKey64;
  end; {sizeof=128} {hashed are 0..63 of header and data}
const cMutHdrMagic:packed array [1..6] of byte=($42,$4e,$4d,$75,$74,$1A);
