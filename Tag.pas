unit Tag deprecated;

INTERFACE
uses Keys;

type
 T = object
  (* Tags are SHA255 hashes of tag plaintext *)
  procedure Create( plaintext :string);
   unimplemented;
  procedure Clear;
   experimental;
  function  isNil :boolean;
   experimental;
  procedure ToString( var s :tFileName );
   experimental;
  procedure FromString( s :tFileName );
   experimental;
  private
  data: array [1..4] of byte;
 end;
 tCollection = array [1..999] of T; {Dynamic length }

type
 tSearch = object
  procedure Search;
   unimplemented;
   (* Run the serach on db with tags *)
  function Result ( var match :Keys.tHash ) :boolean;
   experimental;
   (* Fetch one resuilt
      First call gives best maching entry
      Subsequent calls give less matching entries
      Returns false if no more resuilts are available
   *)
  tags :^array [1..999] of T;
  db   :tFileName;
   (* Tags to search for and database to search in *)
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
uses SysUtils;

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

procedure T.ToString( var s :tFileName );
begin
 s:=IntToHex(data[1],2)+IntToHex(data[2],2)+IntToHex(data[3],2)+IntToHex(data[4],2);
end;

procedure T.FromString( s :tFileName );
begin
 data[1]=StrToInt( 'x'+Copy(s, 1,2) );
 data[2]=StrToInt( 'x'+Copy(s, 3,2) );
 data[3]=StrToInt( 'x'+Copy(s, 5,2) );
 data[4]=StrToInt( 'x'+Copy(s, 7,2) );
end;

procedure tSearch.Reset;
begin
 ptr:=list;
end;

const datadir:string='data/';
const tagdir:string='tags/';

procedure AddToList( id: Keys.tHash );
(*
 Search the list and if id is found, increase score otherwise append.
 Then sort the list.
*)
var cur,nw :^tLLNode;
begin
 cur:=list;
 if cur=nil then begin
  New(cur);
  List:=cur;
  cur^.next:=cur;
  cur^.prev:=cur;
  cur^.id:=id;
  cur^.score:=1;
 end else begin
  while (cur<>list) and (cur^.id<>id) do cur:=cur^.next;
  if cur^.id=id then begin
   inc(cur^.score);
   nw:=cur;
   while (nw^.score > cur^.score) and (cur<>list) do cur:=cur^.prev;
   if cur<>nw then begin
    nw:= 
   
  
end;

procedure tSearch.Search;
var tag :^T;
var tagdir, olddir :tFileName;
var DirLs :TSearchRec;
begin
 olddir:=GetCurrentDir;
 SetCurrentDir(datadir+db+'/');
 AbstractError;
 { for each tag, open, add and bubble }
 tag := tags;
 while not tag^.isNull do begin
  tag.ToString( tagdir );
  if FindFirst(tagdir+'/*', FaAnyFile, DirLs)=0 then repeat
   AddToList(DirLs.Name);
  until FindNext(DirLs)<>0;
 end;
 SetCurrentDir(olddir);
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