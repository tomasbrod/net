{ **********************************************************************
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2008 by Mattias Gaertner
    
    Average Level Tree implementation by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Author: Mattias Gaertner

  Being modified by 0xC45F4253 Tomas Brada for BrodNet project.

  Abstract:
    TAVLTree is an Average Level binary Tree. This binary tree is always
    balanced, so that inserting, deleting and finding a node is performed in
    O(log(#Nodes)).
}
unit BN_AVL_Tree;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{off $DEFINE MEM_CHECK}
{off $DEFINE CheckAVLTreeNodeManager}

uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  Classes, SysUtils;

type
  TAVLTree = class;

  TAVLTreeKey = type Pointer;

  { TAVLTreeNode }

  TAVLTreeNode = class
  private
    Parent, Left, Right: TAVLTreeNode;
    Balance: integer; // = RightDepth-LeftDepth  -2..+2, after balancing: -1,0,+1
    function TreeDepth: integer; // longest WAY down. e.g. only one node => 0 !
    procedure ConsistencyCheck(Tree: TAVLTree); virtual;
    function GetCount: SizeInt;
  public
    function Successor: TAVLTreeNode; // next right
    function Precessor: TAVLTreeNode; // next left
    procedure Assign(const other: TAVLTreeNode); virtual;
    function Compare(const other: TAVLTreeNode): integer; virtual;
    function CompareKey(const key: TAVLTreeKey): integer; virtual;
  end;
  TAVLTreeNodeClass = class of TAVLTreeNode;
  TAVLTreeItem= TAVLTreeNode;

  { TAVLTreeNodeEnumerator }

  TAVLTreeNodeEnumerator = class
  protected
    FCurrent: TAVLTreeNode;
    FLowToHigh: boolean;
    FTree: TAVLTree;
  public
    constructor Create(Tree: TAVLTree; aLowToHigh: boolean = true);
    function GetEnumerator: TAVLTreeNodeEnumerator; inline;
    function MoveNext: Boolean;
    property Current: TAVLTreeNode read FCurrent;
    property LowToHigh: boolean read FLowToHigh;
  end;

  TAVLTree = class
  protected
    FCount: SizeInt;
    FNodeClass: TAVLTreeNodeClass;
    FRoot: TAVLTreeNode;
    procedure BalanceAfterInsert(ANode: TAVLTreeNode);
    procedure BalanceAfterDelete(ANode: TAVLTreeNode);
    procedure DeletingNode({%H-}aNode: TAVLTreeNode); virtual;
    function FindInsertPos(ANode: TAVLTreeNode): TAVLTreeNode;
    procedure Init; virtual;
    procedure NodeAdded({%H-}aNode: TAVLTreeNode); virtual;
    procedure RotateLeft(aNode: TAVLTreeNode); virtual;
    procedure RotateRight(aNode: TAVLTreeNode); virtual;
    procedure SwitchPositionWithSuccessor(aNode, aSuccessor: TAVLTreeNode); virtual;
    procedure SetNodeClass(const AValue: TAVLTreeNodeClass);
  public
    constructor Create;
    destructor Destroy; override;
    property NodeClass: TAVLTreeNodeClass read FNodeClass write SetNodeClass; // used for new nodes

    // add, delete, remove, move
    (* Add ANode to the tree. No copy. *)
    procedure Add(ANode: TAVLTreeNode);
    (* Optimized Add for ascending elements. *)
    procedure AddAscendingSequence(Node: TAVLTreeNode; LastAdded: TAVLTreeNode;
      var Successor: TAVLTreeNode);
    (* Remove exactly this node from the tree. Node is not freed. *)
    procedure UnlinkNode(ANode: TAVLTreeNode);
    (* Removes and Destroys node which node.Compare(Key) = 0. Key is not changed. *)
    function DeleteNode(const Key: TAVLTreeNode): boolean;
    (* Removes and Destroys node which node.CompareKey(Key) = 0. *)
    function Delete(Key: TAVLTreeKey): boolean;
    (* Desroys all nodes in the tree. *)
    procedure Clear;
    function Equals(Obj: TObject): boolean; override; // same as IsEqual(aTree,false)
    function IsEqual(aTree: TAVLTree; CheckDataPointer: boolean): boolean; // checks only keys or Data (references), not the data itself, O(n)
    procedure Assign(aTree: TAVLTree); virtual; // clear and copy all Data (references), O(n)

    // search
    property Root: TAVLTreeNode read fRoot;
    property Count: SizeInt read FCount;
    (* Get node where node.CompareKey(Key) = 0, or nil when not found. *)
    function Find(Key: TAVLTreeKey): TAVLTreeNode; // O(log(n))
    (* Get node where node.Compare(ANode) = 0, or nil when not found. *)
    function FindNode(ANode: TAVLTreeNode): TAVLTreeNode; // O(log(n))
    (* Get node matching key or if not found, nearest to the key. *)
    function FindNearest(Key: TAVLTreeKey): TAVLTreeNode; // O(log(n))
    (* Get first node in a tree. *)
    function FindLowest: TAVLTreeNode; // O(log(n))
    (* Get last node in a tree. *)
    function FindHighest: TAVLTreeNode; // O(log(n))
    function FindLeftMostNode(ANode: TAVLTreeNode): TAVLTreeNode;
    function FindRightMostNode(ANode: TAVLTreeNode): TAVLTreeNode;
    function FindSuccessor(ANode: TAVLTreeNode): TAVLTreeNode; inline;
    function FindPrecessor(ANode: TAVLTreeNode): TAVLTreeNode; inline;
    function FindNearestNode(ANode: TAVLTreeNode): TAVLTreeNode;
    // enumerators
    function GetEnumerator: TAVLTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TAVLTreeNodeEnumerator;

    // consistency
    procedure ConsistencyCheck; virtual; // JuMa: changed to procedure and added "virtual".
    //procedure WriteNodeReport(s: TStream; ANode: TAVLTreeNode);
    procedure WriteSubtreeReport(s: TStream; ANode: TAVLTreeNode);
    procedure WriteReportToStream(s: TStream);
  end;

implementation

{ TAVLTreeNodeEnumerator }

constructor TAVLTreeNodeEnumerator.Create(Tree: TAVLTree; aLowToHigh: boolean);
begin
  FTree:=Tree;
  FLowToHigh:=aLowToHigh;
end;

function TAVLTreeNodeEnumerator.GetEnumerator: TAVLTreeNodeEnumerator;
begin
  Result:=Self;
end;

function TAVLTreeNodeEnumerator.MoveNext: Boolean;
begin
  if FLowToHigh then begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Successor
    else
      FCurrent:=FTree.FindLowest;
  end else begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Precessor
    else
      FCurrent:=FTree.FindHighest;
  end;
  Result:=FCurrent<>nil;
end;

{ TAVLTree }

procedure TAVLTree.AddAscendingSequence(Node: TAVLTreeNode; LastAdded: TAVLTreeNode;
  var Successor: TAVLTreeNode);
{ This is an optimized version of "Add" for adding an ascending sequence of
  nodes.
  It uses the LastAdded and Successor to skip searching for an insert position.
  For nodes with same value the order of the sequence is kept.

  Usage:
    LastNode:=nil; // TAvlTreeNode
    Successor:=nil; // TAvlTreeNode
    for i:=1 to 1000 do
      LastNode:=Tree.AddAscendingSequence(TItem.Create(i),LastNode,Successor);
}
var
  InsertPos: TAVLTreeNode;
begin
  if (LastAdded<>nil) and (LastAdded.Compare(Node)<=0)
  and ((Successor=nil) or (Node.Compare(Successor)<=0)) then begin
    // Data is between LastAdded and Successor
    inc(FCount);
    if LastAdded.Right=nil then begin
      Node.Parent:=LastAdded;
      LastAdded.Right:=Node;
    end else begin
      InsertPos:=LastAdded.Right;
      while InsertPos.Left<>nil do
        InsertPos:=InsertPos.Left;
      Node.Parent:=InsertPos;
      InsertPos.Left:=Node;
    end;
    NodeAdded(Node);
    BalanceAfterInsert(Node);
  end else begin
    // normal Add
    Add(Node);
    Successor:=Node.Successor;
  end;
end;

procedure TAVLTree.Add(ANode: TAVLTreeNode);
// add a node. If there are already nodes with the same value it will be
// inserted rightmost
var InsertPos: TAVLTreeNode;
  InsertComp: integer;
begin
  ANode.Left:=nil;
  ANode.Right:=nil;
  inc(FCount);
  if Root<>nil then begin
    InsertPos:=FindInsertPos(ANode);
    InsertComp:=ANode.Compare(InsertPos);
    ANode.Parent:=InsertPos;
    if InsertComp<0 then begin
      // insert to the left
      InsertPos.Left:=ANode;
    end else begin
      // insert to the right
      InsertPos.Right:=ANode;
    end;
    NodeAdded(ANode);
    BalanceAfterInsert(ANode);
  end else begin
    fRoot:=ANode;
    ANode.Parent:=nil;
    NodeAdded(ANode);
  end;
end;

function TAVLTree.FindLowest: TAVLTreeNode;
begin
  Result:=Root;
  if Result<>nil then
    while Result.Left<>nil do Result:=Result.Left;
end;

function TAVLTree.FindHighest: TAVLTreeNode;
begin
  Result:=Root;
  if Result<>nil then
    while Result.Right<>nil do Result:=Result.Right;
end;

procedure TAVLTree.BalanceAfterDelete(ANode: TAVLTreeNode);
var
  OldParent, OldRight, OldRightLeft, OldLeft, OldLeftRight: TAVLTreeNode;
begin
  while ANode<>nil do begin
    if ((ANode.Balance=+1) or (ANode.Balance=-1)) then exit;
    OldParent:=ANode.Parent;
    if (ANode.Balance=0) then begin
      // Treeheight has decreased by one
      if (OldParent=nil) then
        exit;
      if(OldParent.Left=ANode) then
        Inc(OldParent.Balance)
      else
        Dec(OldParent.Balance);
      ANode:=OldParent;
    end else if (ANode.Balance=+2) then begin
      // Node is overweighted to the right
      OldRight:=ANode.Right;
      if (OldRight.Balance>=0) then begin
        // OldRight.Balance is 0 or -1
        // rotate ANode,OldRight left
        RotateLeft(ANode);
        ANode.Balance:=(1-OldRight.Balance); // toggle 0 and 1
        Dec(OldRight.Balance);
        ANode:=OldRight;
      end else begin
        // OldRight.Balance=-1
        { double rotate
          = rotate OldRightLeft,OldRight right
            and then rotate ANode,OldRightLeft left
                  OldParent                           OldParent
                      |                                  |
                    ANode                           OldRightLeft
                       \                               /      \
                    OldRight             =>          ANode    OldRight
                      /                                \         /
               OldRightLeft                OldRightLeftLeft OldRightLeftRight
                   /     \
        OldRightLeftLeft OldRightLeftRight
        }
        OldRightLeft:=OldRight.Left;
        RotateRight(OldRight);
        RotateLeft(ANode);
        if (OldRightLeft.Balance<=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=-1;
        if (OldRightLeft.Balance>=0) then
          OldRight.Balance:=0
        else
          OldRight.Balance:=+1;
        OldRightLeft.Balance:=0;
        ANode:=OldRightLeft;
      end;
    end else begin
      // Node.Balance=-2
      // Node is overweighted to the left
      OldLeft:=ANode.Left;
      if (OldLeft.Balance<=0) then begin
        // rotate OldLeft,ANode right
        RotateRight(ANode);
        ANode.Balance:=(-1-OldLeft.Balance); // toggle 0 and -1
        Inc(OldLeft.Balance);
        ANode:=OldLeft;
      end else begin
        // OldLeft.Balance = 1
        { double rotate left right
          = rotate OldLeft,OldLeftRight left
            and then rotate OldLeft,ANode right
                    OldParent                           OldParent
                        |                                  |
                      ANode                            OldLeftRight
                       /                               /         \
                    OldLeft             =>          OldLeft    ANode
                       \                                \         /
                   OldLeftRight               OldLeftRightLeft OldLeftRightRight
                     /     \
          OldLeftRightLeft OldLeftRightRight
        }
        OldLeftRight:=OldLeft.Right;
        RotateLeft(OldLeft);
        RotateRight(ANode);
        if (OldLeftRight.Balance>=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=+1;
        if (OldLeftRight.Balance<=0) then
          OldLeft.Balance:=0
        else
          OldLeft.Balance:=-1;
        OldLeftRight.Balance:=0;
        ANode:=OldLeftRight;
      end;
    end;
  end;
end;

procedure TAVLTree.DeletingNode(aNode: TAVLTreeNode);
// called by Delete
// Node.Left=nil or Node.Right=nil
begin
  // for descendants to override
end;

procedure TAVLTree.SetNodeClass(const AValue: TAVLTreeNodeClass);
begin
  if FNodeClass=AValue then Exit;
  if Count>0 then
    raise Exception.Create(ClassName+'.SetNodeClass Count='+IntToStr(Count)
      +' Old='+FNodeClass.ClassName+' New='+AValue.ClassName);
  FNodeClass:=AValue;
end;

procedure TAVLTree.BalanceAfterInsert(ANode: TAVLTreeNode);
var
  OldParent, OldRight, OldLeft: TAVLTreeNode;
begin
  OldParent:=ANode.Parent;
  while (OldParent<>nil) do begin
    if (OldParent.Left=ANode) then begin
      // Node is left child
      dec(OldParent.Balance);
      if (OldParent.Balance=0) then exit;
      if (OldParent.Balance=-1) then begin
        ANode:=OldParent;
        OldParent:=ANode.Parent;
        continue;
      end;
      // OldParent.Balance=-2
      if (ANode.Balance=-1) then begin
        { rotate ANode,ANode.Parent right
             OldParentParent        OldParentParent
                   |                     |
               OldParent        =>     ANode
                 /                        \
              ANode                     OldParent
                \                        /
              OldRight               OldRight      }
        RotateRight(OldParent);
        ANode.Balance:=0;
        OldParent.Balance:=0;
      end else begin
        // Node.Balance = +1
        { double rotate
          = rotate ANode,OldRight left and then rotate OldRight,OldParent right
             OldParentParent             OldParentParent
                    |                           |
                OldParent                    OldRight
                   /            =>          /        \
                 ANode                   ANode      OldParent
                    \                       \          /
                   OldRight          OldRightLeft  OldRightRight
                     / \
          OldRightLeft OldRightRight
        }
        OldRight:=ANode.Right;
        RotateLeft(ANode);
        RotateRight(OldParent);
        if (OldRight.Balance<=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=-1;
        if (OldRight.Balance=-1) then
          OldParent.Balance:=1
        else
          OldParent.Balance:=0;
        OldRight.Balance:=0;
      end;
      exit;
    end else begin
      // Node is right child
      Inc(OldParent.Balance);
      if (OldParent.Balance=0) then exit;
      if (OldParent.Balance=+1) then begin
        ANode:=OldParent;
        OldParent:=ANode.Parent;
        continue;
      end;
      // OldParent.Balance = +2
      if(ANode.Balance=+1) then begin
        { rotate OldParent,ANode left
             OldParentParent        OldParentParent
                   |                     |
               OldParent        =>     ANode
                    \                   /
                  ANode               OldParent
                   /                      \
                OldLeft                 OldLeft      }
        RotateLeft(OldParent);
        ANode.Balance:=0;
        OldParent.Balance:=0;
      end else begin
        // Node.Balance = -1
        { double rotate
          = rotate OldLeft,ANode right and then rotate OldParent,OldLeft right
             OldParentParent             OldParentParent
                    |                           |
                OldParent                    OldLeft
                     \            =>        /       \
                    ANode               OldParent   ANode
                     /                     \          /
                  OldLeft          OldLeftLeft  OldLeftRight
                    / \
         OldLeftLeft OldLeftRight
        }
        OldLeft:=ANode.Left;
        RotateRight(ANode);
        RotateLeft(OldParent);
        if (OldLeft.Balance>=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=+1;
        if (OldLeft.Balance=+1) then
          OldParent.Balance:=-1
        else
          OldParent.Balance:=0;
        OldLeft.Balance:=0;
      end;
      exit;
    end;
  end;
end;

procedure TAVLTree.Clear;

  procedure DeleteNodeR(ANode: TAVLTreeNode);
  begin
    if ANode<>nil then begin
      if ANode.Left<>nil then DeleteNodeR(ANode.Left);
      if ANode.Right<>nil then DeleteNodeR(ANode.Right);
      ANode.Destroy;
    end;
  end;

// Clear
begin
  DeleteNodeR(Root);
  fRoot:=nil;
  FCount:=0;
end;

constructor TAVLTree.Create;
begin
  Init;
end;

procedure TAVLTree.UnlinkNode(ANode: TAVLTreeNode);
var
  OldParent, Child: TAVLTreeNode;
begin
  {$IFDEF CheckAVLTreeNodeManager}
  OldParent:=ANode;
  while OldParent.Parent<>nil do OldParent:=OldParent.Parent;
  if OldParent<>Root then
    raise Exception.Create('TAVLTree.Delete'); // not my node
  {$ENDIF}
  if (ANode.Left<>nil) and (ANode.Right<>nil) then begin
    // ANode has both: Left and Right
    // Switch ANode position with Successor
    // Because ANode.Right<>nil the Successor is a child of ANode
    SwitchPositionWithSuccessor(ANode,ANode.Successor);
  end;
  // left or right is nil
  DeletingNode(aNode);
  OldParent:=ANode.Parent;
  ANode.Parent:=nil;
  if ANode.Left<>nil then
    Child:=ANode.Left
  else
    Child:=ANode.Right;
  if Child<>nil then
    Child.Parent:=OldParent;
  if (OldParent<>nil) then begin
    // Node has parent
    if (OldParent.Left=ANode) then begin
      // Node is left child of OldParent
      OldParent.Left:=Child;
      Inc(OldParent.Balance);
    end else begin
      // Node is right child of OldParent
      OldParent.Right:=Child;
      Dec(OldParent.Balance);
    end;
    BalanceAfterDelete(OldParent);
  end else begin
    // Node was Root
    fRoot:=Child;
  end;
  dec(FCount);
  ANode.Destroy;
end;

function TAVLTree.Delete(Key: TAVLTreeKey): boolean;
var
  ANode: TAvlTreeNode;
begin
  ANode:=Find(Key);
  if ANode<>nil then begin
    UnlinkNode(ANode);
    ANode.Destroy;
    Result:=true;
  end else
    Result:=false;
end;

function TAVLTree.DeleteNode(const Key: TAvlTreeNode): boolean;
var
  ANode: TAvlTreeNode;
begin
  ANode:=FindNode(Key);
  if ANode<>nil then begin
    UnlinkNode(ANode);
    ANode.Destroy;
    Result:=true;
  end else
    Result:=false;
end;

destructor TAVLTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TAVLTree.GetEnumerator: TAVLTreeNodeEnumerator;
begin
  Result:=TAVLTreeNodeEnumerator.Create(Self,true);
end;

function TAVLTree.GetEnumeratorHighToLow: TAVLTreeNodeEnumerator;
begin
  Result:=TAVLTreeNodeEnumerator.Create(Self,false);
end;

function TAVLTree.Find(Key: TAVLTreeKey): TAVLTreeNode;
var Comp: integer;
begin
  Result:=Root;
  while (Result<>nil) do begin
    Comp:=Result.CompareKey(Key);
    if Comp=0 then exit;
    if Comp>0 then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TAVLTree.FindNode(ANode: TAVLTreeNode): TAVLTreeNode;
var Comp: integer;
begin
  Result:=Root;
  while (Result<>nil) do begin
    Comp:=Result.Compare(ANode);
    if Comp=0 then exit;
    if Comp>0 then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TAVLTree.FindNearest(Key: TAVLTreeKey): TAVLTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=Result.CompareKey(Key);
    if Comp=0 then exit;
    if Comp>0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else
        exit;
    end;
  end;
end;

function TAVLTree.FindLeftMostNode(ANode: TAVLTreeNode): TAVLTreeNode;
var
  LeftNode: TAVLTreeNode;
begin
  if ANode<>nil then begin
    Result:=ANode;
    repeat
      LeftNode:=Result.Precessor;
      if (LeftNode=nil) or (LeftNode.Compare(ANode)<>0) then break;
      Result:=LeftNode;
    until false;
  end else begin
    Result:=nil;
  end;
end;

function TAVLTree.FindRightMostNode(ANode: TAVLTreeNode): TAVLTreeNode;
var
  RightNode: TAVLTreeNode;
begin
  if ANode<>nil then begin
    Result:=ANode;
    repeat
      RightNode:=Result.Successor;
      if (RightNode=nil) or (RightNode.Compare(ANode)<>0) then break;
      Result:=RightNode;
    until false;
  end else begin
    Result:=nil;
  end;
end;

function TAVLTree.FindNearestNode(ANode: TAVLTreeNode): TAVLTreeNode;
var Comp: integer;
begin
  Result:=Root;
  while (Result<>nil) do begin
    Comp:=Result.Compare(ANode);
    if Comp=0 then exit;
    if Comp>0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else
        exit;
    end;
  end;
end;

function TAVLTree.FindInsertPos(ANode: TAVLTreeNode): TAVLTreeNode;
var Comp: integer;
begin
  Result:=Root;
  while (Result<>nil) do begin
    Comp:=Result.Compare(ANode);
    if Comp>0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else
        exit;
    end;
  end;
end;

procedure TAVLTree.Init;
begin
  FNodeClass:=TAVLTreeNode;
end;

procedure TAVLTree.NodeAdded(aNode: TAVLTreeNode);
begin
  // for descendants to override
end;

procedure TAVLTree.RotateLeft(aNode: TAVLTreeNode);
{    Parent                Parent
       |                     |
      Node        =>       OldRight
      /  \                  /
   Left OldRight          Node
          /               /  \
     OldRightLeft      Left OldRightLeft  }
var
  AParent, OldRight, OldRightLeft: TAVLTreeNode;
begin
  OldRight:=aNode.Right;
  OldRightLeft:=OldRight.Left;
  AParent:=aNode.Parent;
  if AParent<>nil then begin
    if AParent.Left=aNode then
      AParent.Left:=OldRight
    else
      AParent.Right:=OldRight;
  end else
    fRoot:=OldRight;
  OldRight.Parent:=AParent;
  aNode.Parent:=OldRight;
  aNode.Right:=OldRightLeft;
  if OldRightLeft<>nil then
    OldRightLeft.Parent:=aNode;
  OldRight.Left:=aNode;
end;

procedure TAVLTree.RotateRight(aNode: TAVLTreeNode);
{       Parent              Parent
          |                   |
         Node        =>     OldLeft
         /   \                 \
    OldLeft  Right            Node
        \                     /  \
   OldLeftRight      OldLeftRight Right  }
var
  AParent, OldLeft, OldLeftRight: TAVLTreeNode;
begin
  OldLeft:=aNode.Left;
  OldLeftRight:=OldLeft.Right;
  AParent:=aNode.Parent;
  if AParent<>nil then begin
    if AParent.Left=aNode then
      AParent.Left:=OldLeft
    else
      AParent.Right:=OldLeft;
  end else
    fRoot:=OldLeft;
  OldLeft.Parent:=AParent;
  aNode.Parent:=OldLeft;
  aNode.Left:=OldLeftRight;
  if OldLeftRight<>nil then
    OldLeftRight.Parent:=aNode;
  OldLeft.Right:=aNode;
end;

procedure TAVLTree.SwitchPositionWithSuccessor(aNode, aSuccessor: TAVLTreeNode);
{ called by delete, when aNode.Left<>nil and aNode.Right<>nil
  Switch ANode position with Successor
  Because ANode.Right<>nil the Successor is a child of ANode }
var
  OldBalance: Integer;
  OldParent, OldLeft, OldRight,
  OldSuccParent, OldSuccLeft, OldSuccRight: TAVLTreeNode;
begin
  OldBalance:=aNode.Balance;
  aNode.Balance:=aSuccessor.Balance;
  aSuccessor.Balance:=OldBalance;

  OldParent:=aNode.Parent;
  OldLeft:=aNode.Left;
  OldRight:=aNode.Right;
  OldSuccParent:=aSuccessor.Parent;
  OldSuccLeft:=aSuccessor.Left;
  OldSuccRight:=aSuccessor.Right;

  if OldParent<>nil then begin
    if OldParent.Left=aNode then
      OldParent.Left:=aSuccessor
    else
      OldParent.Right:=aSuccessor;
  end else
    fRoot:=aSuccessor;
  aSuccessor.Parent:=OldParent;

  if OldSuccParent<>aNode then begin
    if OldSuccParent.Left=aSuccessor then
      OldSuccParent.Left:=aNode
    else
      OldSuccParent.Right:=aNode;
    aSuccessor.Right:=OldRight;
    aNode.Parent:=OldSuccParent;
    if OldRight<>nil then
      OldRight.Parent:=aSuccessor;
  end else begin
    {  aNode            aSuccessor
         \          =>    \
         aSuccessor       aNode  }
    aSuccessor.Right:=aNode;
    aNode.Parent:=aSuccessor;
  end;

  aNode.Left:=OldSuccLeft;
  if OldSuccLeft<>nil then
    OldSuccLeft.Parent:=aNode;
  aNode.Right:=OldSuccRight;
  if OldSuccRight<>nil then
    OldSuccRight.Parent:=aNode;
  aSuccessor.Left:=OldLeft;
  if OldLeft<>nil then
    OldLeft.Parent:=aSuccessor;
end;

function TAVLTree.FindSuccessor(ANode: TAVLTreeNode): TAVLTreeNode;
begin
  if ANode<>nil then
    Result:=ANode.Successor
  else
    Result:=nil;
end;

function TAVLTree.FindPrecessor(ANode: TAVLTreeNode): TAVLTreeNode;
begin
  if ANode<>nil then
    Result:=ANode.Precessor
  else
    Result:=nil;
end;

procedure TAVLTree.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create('TAVLTree.ConsistencyCheck: '+Msg);
  end;

var
  RealCount: SizeInt;
begin
  RealCount:=0;
  if FRoot<>nil then begin
    FRoot.ConsistencyCheck(Self);
    RealCount:=FRoot.GetCount;
  end;
  if Count<>RealCount then
    E('Count<>RealCount');
end;

function TAVLTree.Equals(Obj: TObject): boolean;
begin
  if Obj is TAVLTree then
    Result:=IsEqual(TAVLTree(Obj),false)
  else
    Result:=inherited Equals(Obj);
end;

function TAVLTree.IsEqual(aTree: TAVLTree; CheckDataPointer: boolean): boolean;
var
  MyNode, OtherNode: TAVLTreeNode;
begin
  if aTree=Self then exit(true);
  Result:=false;
  if aTree=nil then exit;
  if Count<>aTree.Count then exit;
  if NodeClass<>aTree.NodeClass then exit;
  MyNode:=FindLowest;
  OtherNode:=aTree.FindLowest;
  while MyNode<>nil do begin
    if OtherNode=nil then exit;
    if MyNode.Compare(OtherNode)<>0 then exit;
    MyNode:=MyNode.Successor;
    OtherNode:=OtherNode.Successor;
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TAVLTree.Assign(aTree: TAVLTree);

  procedure AssignNode(var MyNode: TAVLTreeNode; OtherNode: TAVLTreeNode);
  begin
    MyNode:=NodeClass.Create;
    MyNode.Assign(OtherNode);
    MyNode.Balance:=OtherNode.Balance;
    if OtherNode.Left<>nil then begin
      AssignNode(MyNode.Left,OtherNode.Left);
      MyNode.Left.Parent:=MyNode;
    end;
    if OtherNode.Right<>nil then begin
      AssignNode(MyNode.Right,OtherNode.Right);
      MyNode.Right.Parent:=MyNode;
    end;
  end;

begin
  if aTree=nil then
    raise Exception.Create('TAVLTree.Assign aTree=nil');
  if IsEqual(aTree,true) then exit;
  Clear;
  NodeClass:=aTree.NodeClass;
  if aTree.Root<>nil then
    AssignNode(fRoot,aTree.Root);
  FCount:=aTree.Count;
end;

procedure TAVLTree.WriteReportToStream(s: TStream);

  procedure WriteStr(const Txt: string);
  begin
    if Txt='' then exit;
    s.Write(Txt[1],length(Txt));
  end;

  procedure WriteTreeNode(ANode: TAVLTreeNode);
  var
    b: String;
    IsLeft: boolean;
    AParent: TAVLTreeNode;
    WasLeft: Boolean;
  begin
    if ANode=nil then exit;
    WriteTreeNode(ANode.Right);
    AParent:=ANode;
    WasLeft:=false;
    b:='';
    while AParent<>nil do begin
      if AParent.Parent=nil then begin
        if AParent=ANode then
          b:='--'+b
        else
          b:='  '+b;
        break;
      end;
      IsLeft:=AParent.Parent.Left=AParent;
      if AParent=ANode then begin
        if IsLeft then
          b:='\-'
        else
          b:='/-';
      end else begin
        if WasLeft=IsLeft then
          b:='  '+b
        else
          b:='| '+b;
      end;
      WasLeft:=IsLeft;
      AParent:=AParent.Parent;
    end;
    b:=b+Format('%s      Self=%p  Parent=%p  Balance=%d',
             [aNode.ToString, Pointer(aNode),Pointer(aNode.Parent), aNode.Balance])
        +LineEnding;
    WriteStr(b);
    WriteTreeNode(ANode.Left);
  end;

// TAVLTree.WriteReportToStream
begin
  WriteStr('-Start-of-AVL-Tree-------------------'+LineEnding);
  WriteTreeNode(fRoot);
  WriteStr('-End-Of-AVL-Tree---------------------'+LineEnding);
end;

{function TAVLTree.ReportAsString: string;
var ms: TMemoryStream;
begin
  Result:='';
  ms:=TMemoryStream.Create;
  try
    WriteReportToStream(ms);
    ms.Position:=0;
    SetLength(Result,ms.Size);
    if Result<>'' then
      ms.Read(Result[1],length(Result));
  finally
    ms.Free;
  end;
end;}

procedure TAVLTree.WriteSubtreeReport(s: TStream; ANode: TAVLTreeNode);

  procedure WriteStr(const Txt: string);
  begin
    if Txt='' then exit;
    s.WriteBuffer(Txt[1],length(Txt));
  end;

  begin {WriteSubtreeReport}
  if ANode=nil then WriteStr('-')
  else begin
    WriteStr(Format('(%S,%.2D,',[ANode.ToString,ANode.Balance]));
    WriteSubtreeReport(s, ANode.Left);
    WriteStr(',');
    WriteSubtreeReport(s, ANode.Right);
    WriteStr(')');
  end;
end;

{ TAVLTreeNode }

procedure TAVLTreeNode.Assign(const other: TAVLTreeNode);
begin
  AbstractError;
end;

function TAVLTreeNode.CompareKey(const key: TAVLTreeKey): integer;
begin
  if Pointer(Self) > Key then Result:=+1 else
  if Pointer(Self) < Key then Result:=-1 else
  Result:=0;
end;

function TAVLTreeNode.Compare(const other: TAVLTreeNode): integer;
begin
  if Pointer(Self) > Pointer(Other) then Result:=+1 else
  if Pointer(Self) < Pointer(Other) then Result:=-1 else
  Result:=0;
end;

function TAVLTreeNode.TreeDepth: integer;
// longest WAY down. e.g. only one node => 0 !
var LeftDepth, RightDepth: integer;
begin
  if Left<>nil then
    LeftDepth:=Left.TreeDepth+1
  else
    LeftDepth:=0;
  if Right<>nil then
    RightDepth:=Right.TreeDepth+1
  else
    RightDepth:=0;
  if LeftDepth>RightDepth then
    Result:=LeftDepth
  else
    Result:=RightDepth;
end;

procedure TAVLTreeNode.ConsistencyCheck(Tree: TAVLTree);

  procedure E(Msg: string);
  begin
    raise Exception.Create('TAVLTreeNode.ConsistencyCheck: '+Msg);
  end;

var
  LeftDepth: SizeInt;
  RightDepth: SizeInt;
begin
  // test left child
  if Left<>nil then begin
    if Left.Parent<>Self then
      E('Left.Parent<>Self');
    if Left.Compare(Self)>0 then
      E('Compare(Left.Data,Data)>0');
    Left.ConsistencyCheck(Tree);
  end;
  // test right child
  if Right<>nil then begin
    if Right.Parent<>Self then
      E('Right.Parent<>Self');
    if Self.Compare(Right)>0 then
      E('Compare(Data,Right.Data)>0');
    Right.ConsistencyCheck(Tree);
  end;
  // test balance
  if Left<>nil then
    LeftDepth:=Left.TreeDepth+1
  else
    LeftDepth:=0;
  if Right<>nil then
    RightDepth:=Right.TreeDepth+1
  else
    RightDepth:=0;
  if Balance<>(RightDepth-LeftDepth) then
    E('Balance['+IntToStr(Balance)+']<>(RightDepth['+IntToStr(RightDepth)+']-LeftDepth['+IntToStr(LeftDepth)+'])');
end;

function TAVLTreeNode.GetCount: SizeInt;
begin
  Result:=1;
  if Left<>nil then inc(Result,Left.GetCount);
  if Right<>nil then inc(Result,Right.GetCount);
end;

function TAVLTreeNode.Successor: TAVLTreeNode;
begin
  Result:=Right;
  if Result<>nil then begin
    while (Result.Left<>nil) do Result:=Result.Left;
  end else begin
    Result:=Self;
    while (Result.Parent<>nil) and (Result.Parent.Right=Result) do
      Result:=Result.Parent;
    Result:=Result.Parent;
  end;
end;

function TAVLTreeNode.Precessor: TAVLTreeNode;
begin
  Result:=Left;
  if Result<>nil then begin
    while (Result.Right<>nil) do Result:=Result.Right;
  end else begin
    Result:=Self;
    while (Result.Parent<>nil) and (Result.Parent.Left=Result) do
      Result:=Result.Parent;
    Result:=Result.Parent;
  end;
end;


end.
