unit Tag;

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
 tCollection = array [1..999] of T; {Dynamic length }

type
 tSearch = object
  public
  procedure Reset;
  procedure Search;
  function Find :boolean;
  function Get :word;
  tags :^array [1..999] of T;
  db   :tFileName;
  private
  list :^tLLNode;
  ptr  :^tLLNode;
 end;
 tLLNode = record
  next, prev : tLLNode_ptr;
  score : integer;
  id : Keys.tHash;
 end;
 tLLNode_ptr = ^tLLNode;

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

procedure tSearch.Reset;
begin
 ptr:=list;
end;

procedure tSearch.Search;
begin
 AbstractError;
 { for each tag, open, add and bubble }
end;


function tSearch.Find :boolean;
begin
 AbstractError;
end;

function tSearch.Get :word;
begin
 AbstractError;
end;

END
.