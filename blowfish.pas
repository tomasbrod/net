{$mode objfpc}
unit blowfish;
interface
uses ctypes;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const BLOCK_SIZE = 8;

type tBlowfishKey = record
  p : array[0..17] of DWORD;
  s : array[0..3,0..255] of DWORD;
  end;
  tBlowFishKey_ptr=^tBlowfishKey;
type tKey8=packed array [0..7] of byte;
type tKey=tBlowfishKey;

procedure ExpandKey(out keystruct: tBlowfishKey; const user_key; len: word);
procedure Encrypt(out chipertext; const plaintext; const key:TBLOWFISHKEY);
procedure Decrypt(out plaintext; const chipertext; const key:TBLOWFISHKEY);
procedure BlockXOR(out oup; const key; const len:word);
procedure EncryptCBC(out chipertext; const plaintext; const key:TBLOWFISHKEY; var fv);
procedure DecryptCBC(out plaintext; const chipertext; const key:TBLOWFISHKEY; var fv);

procedure blowfish_key_setup(user_key:pointer; keystruct:tBlowFishKey_ptr; len: csize_t);
 cdecl;external;
procedure blowfish_encrypt(plaintext:pointer; chipertext:pointer; key:tBlowFishKey_ptr);
 cdecl;external;
procedure blowfish_decrypt(chipertext:pointer; plaintext:pointer; key:tBlowFishKey_ptr);
 cdecl;external;

implementation

{$L ed25519/blowfish.o}

procedure ExpandKey(out keystruct: tBlowfishKey; const user_key; len: word);
  begin
  blowfish_key_setup(@user_key,@keystruct,len);
end;
procedure Encrypt(out chipertext; const plaintext; const key:TBLOWFISHKEY);
  begin
  blowfish_encrypt(@plaintext,@chipertext,@key);
end;
procedure Decrypt(out plaintext; const chipertext; const key:TBLOWFISHKEY);
  begin
  blowfish_decrypt(@chipertext,@plaintext,@key);
end;
procedure BlockXOR(out oup; const key; const len:word);
  var i:byte;
  begin
  for i:=0 to len-1 do
    byte((@oup+i)^):=byte((@oup+i)^) xor byte((@key+i)^);
end;
procedure EncryptCBC(out chipertext; const plaintext; const key:TBLOWFISHKEY; var fv);
  begin
  BlockXOR(fv,plaintext,8);
  blowfish_encrypt(@fv,@chipertext,@key);
  Move(chipertext,fv,8);
end;
procedure DecryptCBC(out plaintext; const chipertext; const key:TBLOWFISHKEY; var fv);
  begin
  blowfish_decrypt(@chipertext,@plaintext,@key);
  BlockXOR(plaintext,fv,8);
  Move(chipertext,fv,8);
end;

end.
