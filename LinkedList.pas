unit LinkedList deprecated;
INTERFACE

type
 tDLNode=class(tObject)
  public
  procedure Insert(const node:tDLNode);
  procedure InsertBefore(const node:tDLNode);
  procedure Unlink;
  procedure Swap(const node:tDLNode);
  constructor CreateRoot;
  constructor Create;
  destructor Destroy; override;
  (*
  function  getNext:tDLNode;
  function  getPrev:tDLNode;
  *)
  public
  (*magic:(NodeMagic=$0F45E1A7,NodeMagicInvalid);*)
  next,prev:tDLNode;
 end;

IMPLEMENTATION

constructor tDLNode.Create;
 begin
 next:=nil;
 prev:=nil;
end;

constructor tDLNode.CreateRoot;
 begin
 Next:=self;
 Prev:=self;
end;

procedure tDLNode.Insert(const node:tDLNode);
 begin
 node.Next:=Next;
 node.Prev:=self;
 Next.Prev:=node;
 Next:=node;
end;
procedure tDLNode.InsertBefore(const node:tDLNode);
 begin
 node.Next:=self;
 node.Prev:=Prev;
 Prev.Next:=node;
 Prev:=node;
end;

procedure tDLNode.Swap(const node:tDLNode);
 var mp:tDLNode;
 begin
 mp:=Prev;
 self.Unlink;
 node.Insert(self);
 node.Unlink;
 mp  .Insert(node);
end;

procedure tDLNode.Unlink;
 begin
 if assigned(prev) then Prev.Next:=Next;
 if assigned(next) then Next.Prev:=Prev;
 Prev:=nil;
 Next:=nil;
end;

destructor tDLNode.Destroy;
 begin
 Unlink;
 inherited;
end;

(*
function tDLNode.getNext:tDLNode;
begin
 getNext:=next;
end;

function  tDLNode.getPrev:tDLNode;
begin
 getPrev:=prev;
end;
*)

(*
var temp_data:Tpool_obj;
procedure init;
begin
 nodes:=@temp_data;
 nodes^.init(sizeof(tDLNode));
end;
*)

END.
