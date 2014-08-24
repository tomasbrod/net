unit FileTag;

INTERFACE
uses Keys;

type
 T = object
  (* Tags are SHA255 hashes of tag plaintext *)
  procedure Create( plaintext :string);
   unimplemented;
  procedure Clear;
  function  isNil :boolean;
  private
  data: array [1..4] of byte;
 end unimplemented;

IMPLEMENTATION

procedure T.Create( plaintext :string);
begin
 AbstractError;
end;

procedure T.Clear;
var i:byte;
begin for i:=low(data) to high(data) do data[i]:=0; end;

function  T.isNil :boolean;
var i:byte;
begin
 isNil:=true;
 for i:=low(data) to high(data) do if data[i]<>0 then isNil:=false;
end;

END
.