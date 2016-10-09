Unit CTRL;
INTERFACE
IMPLEMENTATION
{$undef ctlDHT}
{$define ctlStore}
{$undef ctlMutable}
{$define ctlFetch}
USES ServerLoop,opcode
    ,ObjectModel
    ,Sockets,BaseUnix
    ,SysUtils
    ,sha512
    {$ifdef ctlDHT},dht,dhtLookup{$endif}
    ,Store2
    {$ifdef ctlMutable},Mutable{$endif}
    {$ifdef ctlFetch},Fetch{$endif}
    ;

type tClient=object
  s:tSocket;
  hash:tKey20;
  error:boolean;
  SndObj:^Store2.tStoreObject;
  SndObjLeft:LongWord;
  RcvObj:^tFileStream;
  RcvObjLeft:LongWord;
  procedure Init(i_s:tSocket);
  procedure Init2;
  procedure Done;
  procedure Int;
  procedure Event(ev:word);
  procedure SendTo(msg:tMemoryStream);
  procedure SendObject(var o:tStoreObject; ilen:LongWord);
  procedure RcvObjComplete;
  {$ifdef ctlFetch}public
  transf:^Fetch.tFetch;
  {$endif}
  {$ifdef ctlMutable}public
  mutator:^tMutator;
  procedure MutatorEvent( ev:tMutEvt; ver:longword; const fid:tFID; const Src:tNetAddr );
  procedure MutatorComplete;
  {$endif}
end;
{$I CtrlLow.pas}

procedure Terminate(var client:tClient; var a,r:tMemoryStream);
 begin
 writeln('Ctrl.Terminate');
 ServerLoop.RequestTerminate(0);
end;

procedure GetInfo(var client:tClient; var a,r:tMemoryStream);
 const ident:string='xD ' unimplemented;
 begin
 r.Write(ident[1],length(ident));
 //r.Write(VersionString[1],length(VersionString));
 r.WriteByte(0);
end;

{$ifdef ctlDHT}
procedure DhtPeer(var client:tClient; var a,r:tMemoryStream);
  var contact:^tNetAddr;
  begin
  contact:=a.ReadPtr(sizeof(tNetAddr));
  DHT.NodeBootstrap(contact^);
  r.WriteByte(0);
end;
{$endif}

{$ifdef ctlStore}
procedure StoreLocalCopy(var client:tClient; var a,r:tMemoryStream);
  var path:ansistring;
  var l:longword;
  var so:tStoreObject;
  begin
  l:=a.Left;
  if l<2048 then begin
    SetLength(path,l);
    a.Read(path[1],l);
    writeln('Ctrl.StoreLocalCopy: ',path);
    {$note StoreLocalCopy does Reference instead of Copy}
    try
      so.HashObjectLinkOrRef(path);
      r.WriteByte(0);
      r.Write(so.fid,sizeof(so.fid));
      so.Close;
    except
      on eInOutError do r.WriteByte(opcode.otFail);
    end;
  end else client.Error:=true;
end;
{$endif}

procedure StoreGet(var client:tClient; var a,r:tMemoryStream);
  var o:tStoreObject;
  var id:^tFID;
  var ofs,len:LongWord;
  begin
  try
  id:=a.ReadPtr(20);
  ofs:=a.ReadWord4;
  len:=a.ReadWord4;
  except client.error:=true; exit end;
  try
    writeln('Ctrl.StoreGet: ',string(id^),' @',ofs,'+',len);
    o.Init(id^);
  except
    on eObjectNF do begin r.WriteByte(opcode.otNotFound); exit end;
    //on eInOutError do begin r.WriteByte(opcode.otFail); exit end;
  end;
  if ofs>o.Length then r.WriteByte(opcode.otEoT)
  else begin
    o.Seek(ofs);
    if len>o.Left then len:=o.Left;
    r.WriteByte(0);
    r.WriteWord4(len);
    client.SendObject(o,len);
    exit; {closed by low}
  end;
  o.Close;
end;

procedure StorePut(var client:tClient; var a,r:tMemoryStream);
  var len:LongWord;
  begin
  len:=a.ReadWord4; {$note unhandled exceptions}
  New(client.RcvObj,OpenRW(Store2.GetTempName(client.hash,'ctl')));
  client.RcvObjLeft:=len;
end;
procedure tClient.RcvObjComplete;
  var so:tStoreObject;
  var r:tMemoryStream;
  begin
  r.Init(26); {$note unhandled exceptions}
  so.HashObjectRename(RcvObj^,Store2.GetTempName(hash,'ctl'),true);
  r.WriteWord2(21);
  r.WriteByte(0);
  r.Write(so.fid,20);
  self.SendTo(r);
  r.Free;
  so.Close;
end;

{$ifdef ctlMutable}
procedure MutableSet(var client:tClient; var a,r:tMemoryStream);
  var fid:^tFID;
  var meta:tMutableMeta;
  var o:tStoreObject;
  var valid:boolean;
  begin
  fid:=a.ReadPtr(20);
  try
    o.Init(fid^);
    valid:=SetMutable(o, meta);
    if valid then begin
      r.WriteByte(0);
      r.Write(meta.fid,20);
      r.WriteWord(meta.ver,4);
    end else r.WriteByte(1);
    o.Close;
  except
    on eObjectNF do r.WriteByte(opcode.otNotFound);
    on eInOutError do r.WriteByte(opcode.otFail);
    on Exception do r.WriteByte(opcode.otFail);
  end;
end;

procedure MutableGet(var client:tClient; var a,r:tMemoryStream);
  var des:tMutableMeta;
  var found:boolean;
  var pid:^tPID;
  begin
  pid:=a.ReadPtr(20);
  found:=GetMutable(pid^,des);
  if found then begin
    r.WriteByte(0);
    r.Write(des.FID,20);
    r.Write(des.Ver,4);
  end else begin
    r.WriteByte(opcode.otNotFound);
  end;
end;

procedure MutableUpdate(var client:tClient; var a,r:tMemoryStream);
  var pid:^tPID;
  begin
  pid:=a.ReadPtr(20);
  New(client.mutator);
  with client.mutator^ do begin
    Init(pid^);
    onEvent:=@client.MutatorEvent;
    onComplete:=@client.MutatorComplete;
  end;
  r.WriteByte(0);
end;

procedure tClient.MutatorComplete;
  begin
  {result sent in meCheckOK}
end;

procedure tClient.MutatorEvent( ev:tMutEvt; ver:longword; const fid:tFID; const Src:tNetAddr );
  var r:tMemoryStream;
  begin
  r.Init(51);r.WriteWord(49,2);
  r.WriteByte(ORD(ev));
  r.WriteWord(ver,4);
  if @fid<>nil then r.Write(fid,20) else r.Wrend(20);
  if @src<>nil then r.Write(Src,24) else r.Wrend(24);
  assert(r.length=51);
  SendTo(r);
  r.free;
  if ev=meSendEnd then mutator:=nil;
end;
{$endif}

procedure tClient.Init2; begin
  SndObjLeft:=0;
  RcvObjLeft:=0;
  {$ifdef ctlMutable}mutator:=nil;{$endif}
  //trans:=nil;
end;
procedure tCLient.Int; begin
  {$ifdef ctlMutable}
  if assigned(mutator) then mutator^.done; mutator:=nil;
  {$endif}
end;
BEGIN
  Server1.Init;
  FillChar(methods,sizeof(methods),0);
  methods[00].Init(@GetInfo,0);
  methods[01].Init(@Terminate,0);
  {$ifdef ctlDHT}
  methods[02].Init(@DhtPeer,sizeof(tNetAddr));
  {$endif}
  {$ifdef ctlStore}
  methods[03].Init(@StoreLocalCopy,2);
  methods[04].Init(@StoreGet,28);
  methods[08].Init(@StorePut,4);
  {$endif}
  {$ifdef ctlMutable}
  methods[05].Init(@MutableGet,sizeof(tPID));
  methods[06].Init(@MutableSet,sizeof(tFID));
  methods[07].Init(@MutableUpdate,sizeof(tFID));
  {$endif}
END.

