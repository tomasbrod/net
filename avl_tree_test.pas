uses bn_avl_tree,math,sysutils,classes;

var tree:tAvlTree;
type tItem = class(TAVLTreeItem)
  Key: string[63];
  //procedure Assign(const other: TAVLTreeNode); override;
  function Compare(const other: TAVLTreeNode): integer; override;
  function CompareKey(const other: TAVLTreeKey): integer; override;
  constructor Create(const ikey: string);
  function ToString: ansistring; override;
end;

function TItem. CompareKey(const other: TAVLTreeKey): integer;
  begin
  write('[CompareKey]');
  result:=CompareStr(Key,string(other^));
end;

function TItem. Compare(const other: TAVLTreeNode): integer;
  begin
  write('[Compare]');
  result:=CompareStr(Key,tItem(other).Key);
end;

function TItem. ToString: ansistring;
  begin
  result:=Format('%s(%s)',[classname,key]);
end;

constructor TItem. Create(const ikey: string);
  begin
  Key:=ikey;
end;

procedure AddSomeItems;
  var item: tItem;
  begin
  tree:=tAVLTree.Create;
  tree.Add(tItem.Create('peter'));
  tree.Add(tItem.Create('sufusky'));
  tree.Add(tItem.Create('kek'));
  tree.Add(tItem.Create('zidan'));
end;

procedure Dump;
  var item: tItem;
  var ms:tMemoryStream;
  var realstruct:ansistring;
  begin
  ms:=tMemoryStream.Create;
  tree.WriteReportToStream(ms);
  ms.position:=0;
  SetLength(realstruct,ms.size);
  ms.read(realstruct[1],ms.size);
  writeln(realstruct);
  ms.Free;
  item:= tItem(tree.FindLowest);
  while item<>nil do begin
    writeln(item.Key);
    item:=tItem(item.Successor);
  end;
end;

procedure TryFind(key: shortstring);
  var item: tItem;
  begin
  item:=tItem(tree.Find( pointer(@key) ));
  if item<>nil then
    writeln('for ',key,' found: ',item.ToString)
  else
    writeln('for ',key,' nothing found.');
end;

procedure TryFinds;
  begin
  TryFind('kek');
  TryFind('vadim');
end;

procedure AssertStructure(const insert: ansistring; const struct: ansistring);
  var s:tMemoryStream=nil;
  var tree:tAvlTree=nil;
  var node:tItem;
  var i:longword;
  var realstruct:ansistring;
  begin
  s:=tMemoryStream.Create;
  tree:=tAvlTree.Create;
  try
    for i:=1 to length(insert) do begin
      node:=tItem.Create(insert[i]);
      try
        tree.Add(node);
      except
        node.free;
        raise;
      end;
    end;
    tree.WriteSubtreeReport(s,tree.root);
    if((s.size<>length(struct)) or (CompareByte(s.memory^,struct[1],s.size)<>0))
      then begin
      s.position:=0;
      SetLength(realstruct,s.size);
      s.read(realstruct[1],s.size);
      raise Exception.CreateFmt('Tree structure mismatch. Expected %S got %S.',
        [struct,realstruct]);
    end;
  finally
    s.Free;
    realstruct:='';
    tree.Free;
  end;
end;

BEGIN
  AddSomeItems;
  Dump;
  TryFinds;
  tree.Clear;
  tree.free;
  AssertStructure('a','(tItem(a),00,-,-)');
  AssertStructure('abc','(tItem(b),00,(tItem(a),00,-,-),(tItem(c),00,-,-))');
  AssertStructure('acb','(tItem(b),00,(tItem(a),00,-,-),(tItem(c),00,-,-))');
  AssertStructure('cab','(tItem(b),00,(tItem(a),00,-,-),(tItem(c),00,-,-))');
  AssertStructure('cba','(tItem(b),00,(tItem(a),00,-,-),(tItem(c),00,-,-))');
  AssertStructure('bac','(tItem(b),00,(tItem(a),00,-,-),(tItem(c),00,-,-))');
  AssertStructure('bca','(tItem(b),00,(tItem(a),00,-,-),(tItem(c),00,-,-))');
end.
