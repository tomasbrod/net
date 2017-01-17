Unit CTRL;
INTERFACE
IMPLEMENTATION
{$define ctlDHT}
{$define ctlStore}
{$undef ctlMutable}
{$define ctlProf}
{$define ctlFetch}
USES ServerLoop,opcode
    ,ObjectModel
    ,Sockets,BaseUnix
    ,SysUtils
    ,Crypto
    {$ifdef ctlDHT},dht{$endif}
    ,Store2
    {$ifdef ctlMutable},Mutable{$endif}
    {$ifdef ctlProf},Profile,ProfCache{$endif}
    {$ifdef ctlFetch},Fetch{$endif}
    ;

type tClient=object
  s:tSocket;
  hash:tKey20;
  error:boolean;
  procedure Init(i_s:tSocket);
  procedure Init2;
  procedure Done;
  procedure Int;
  procedure Event(ev:word);
  procedure SendTo(msg:tMemoryStream);
  {$ifdef ctlStore}public
  SndObj:^Store2.tStoreObject;
  SndObjLeft:LongWord;
  RcvObj:^tFileStream;
  RcvObjLeft:LongWord;
  RcvObjHctx:tSha256;
  procedure SendObject(var o:tStoreObject; ilen:LongWord);
  procedure RcvObjComplete;
  {$endif}
  {$ifdef ctlFetch}public
  transf:Fetch.pFetch;
  procedure FetchEvent( task_:tTask_ptr; ev:tTaskEvent; data:pointer );
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
 begin
 r.Write(VersionString[1],length(VersionString));
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

procedure DhtDump(var client:tClient; var a,r:tMemoryStream);
  var bkt: ^DHT.tBucket;
  var i:integer;
  begin
  r.WriteByte(0);
  bkt:=DHT.GetDHTTable;
  while assigned(bkt) do with bkt^ do begin
    {r.Write(Prefix,20);}
    r.WriteByte(Depth);
    r.WriteByte(high(peer));
    r.WriteWord4(MNow-ModifyTime);
    for i:=1 to high(peer) do with peer[i] do begin
      r.Write(ID,20);
      r.Write(Addr,sizeof(tNetAddr));
      r.WriteWord2(ReqDelta);
      r.WriteWord4(MNow-LastMsgFrom);
      r.WriteByte( ord(Banned) or (ord(Verified) shl 1));
      r.WriteByte( Hops );
    end;
    bkt:=next;
  end;
end;
{$endif}

{$ifdef ctlStore}
  {$ifdef ctlStoreAdv}
procedure StorePutLN(var client:tClient; var a,r:tMemoryStream);
  var path:ansistring;
  var l:longword;
  var so:tStoreObject;
  begin
  l:=a.Left;
  if l>=2048 then begin client.Error:=true; exit end;
  SetLength(path,l);
  a.Read(path[1],l);
  if pos('/dev/',path)=1 then begin
    r.WriteByte(opcode.otFail); exit end;
  writeln('Ctrl.StorePutLN: ',path);
  try
    so.HashObjectLinkOrRef(path);
    r.WriteByte(0);
    r.Write(so.fid,sizeof(so.fid));
    so.Done;
  except
    on eInOutError do r.WriteByte(opcode.otFail);
  end;
end;

procedure StorePutMV(var client:tClient; var a,r:tMemoryStream);
  var path:ansistring;
  var l:longword;
  var so:tStoreObject;
  begin
  l:=a.Left;
  if l>=2048 then begin client.Error:=true; exit end;
  SetLength(path,l);
  a.Read(path[1],l);
  if pos('/dev/',path)=1 then begin
    r.WriteByte(opcode.otFail); exit end;
  writeln('Ctrl.StorePutMV: ',path);
  try
    so.HashObjectRename(path);
    r.WriteByte(0);
    r.Write(so.fid,sizeof(so.fid));
    so.Close;
  except
    on eInOutError do r.WriteByte(opcode.otFail);
  end;
end;
  {$endif}

procedure StoreStat(var client:tClient; var a,r:tMemoryStream);
  var so:tStoreObject;
  var id:^tFID;
  var path:AnsiString;
  begin
  try
    id:=a.ReadPtr(20);
    so.Init(id^);
    r.WriteByte(0);
    r.Write(so.fid,20);
    r.WriteWord4(so.Length);
    r.WriteWord2(so.opentime_refcount);
    r.WriteByte(Ord(so.temp));
    r.WriteByte(Ord(so.Location));
    if so.location=solReference
      then path:=so.GetPath
    else if so.location in [solObjdir,solHardlink]
      then path:=GetCurrentDir+'/'+so.GetPath
      else path:='%';
    r.Write(path[1],length(path));
    r.WriteByte(0);
    so.Done;
  except
    on eObjectNF do r.WriteByte(opcode.otNotFound);
  end;
end;

procedure StoreRefAdj(var client:tClient; var a,r:tMemoryStream);
  var so:tStoreObject;
  var id:^tFID;
  var adj:integer;
  begin
  try
    id:=a.ReadPtr(20);
    adj:=a.ReadByte;
    adj:=adj-128;
    so.Init(id^);
    so.Reference(adj);
    if(adj>0) then so.Done;
    r.WriteByte(0);
  except
    on eObjectNF do r.WriteByte(opcode.otNotFound);
  end;
end;

procedure StoreGet(var client:tClient; var a,r:tMemoryStream);
  var o:^tStoreObject;
  var id:^tFID;
  var ofs,len:LongWord;
  begin
  id:=a.ReadPtr(20);
  ofs:=a.ReadWord4;
  len:=a.ReadWord4;
  try
    writeln('Ctrl.StoreGet: ',string(id^),' @',ofs,'+',len);
    New(o,Init(id^));
  except
    on eObjectNF do begin r.WriteByte(opcode.otNotFound); exit end;
    //on eInOutError do begin r.WriteByte(opcode.otFail); exit end;
  end;
  if ofs>o^.Length then r.WriteByte(opcode.otEoT)
  else begin
    o^.Seek(ofs);
    if len>o^.Left then len:=o^.Left;
    r.WriteByte(0);
    r.WriteWord4(len);
    client.SendObject(o^,len);
    exit; {closed by low}
  end;
  Dispose(o,Done);
end;

procedure StorePut(var client:tClient; var a,r:tMemoryStream);
  var len:LongWord;
  begin
  len:=a.ReadWord4;
  New(client.RcvObj,OpenRW(Store2.GetTempName(client.hash,'ctl')));
  client.RcvObjLeft:=len;
  client.RcvObjHctx.Init;
  writeln('Ctrl.StorePUT: ',len);
end;
procedure tClient.RcvObjComplete;
  var so:tStoreObject;
  var r:tMemoryStream;
  var hash_id:tFID;
  begin
  r.Init(26);
  RcvObjHctx.TruncFinal(hash_id,sizeof(hash_id));
  writeln('Ctrl.RcvObjComplete: '+string(hash_id));
  so.InsertRename(RcvObj^,Store2.GetTempName(hash,'ctl'),hash_id);
  r.WriteWord2(21);
  r.WriteByte(0);
  r.Write(so.fid,20);
  self.SendTo(r);
  r.Free;
  so.MakeTemp;
  so.Reference(-1);
end;
{$endif}

{$ifdef ctlFetch}
procedure FetchStart(var client:tClient; var a,r:tMemoryStream);
  var fid:^tFID;
  var src:^tNetAddr;
  var prio:byte;
  begin
  fid:=a.ReadPtr(20);
  src:=a.ReadPtr(18);
  prio:=a.ReadByte;
  writeln('Ctrl.FetchStart: ');
  if ObjectExists(fid^) then client.FetchEvent(nil, tevUser, fid)
  else begin
    client.transf:=NewFetch(fid^);
    client.transf^.Attach(nil, @client.FetchEvent);
    client.transf^.AddSource(src^);
    client.transf^.Props(prio,0);
  end;
  r.WriteByte(0);
end;

procedure FetchQuery(var client:tClient; var a,r:tMemoryStream);
  begin
  Client.Error:=true;
end;

procedure tClient.FetchEvent( task_:tTask_ptr; ev:tTaskEvent; data:pointer );
  var task:pFetch absolute task_;
  var o:tStoreObject;
  var r:tMemoryStream;
  begin
  r.Init(99);
  r.WriteByte(16);
  if assigned(task) then begin
    r.WriteByte(ord(task^.error));
    if ord(task^.error)>0 then exit;
  end;
  o.Init(tfid(data^));
  o.MakeTemp;
  o.Reference(-1);
end;
{$endif}

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


{$ifdef ctlProf}
procedure ProfSet(var client:tClient; var a,r:tMemoryStream);
  var fid:^tFID;
  var p:tProfileRead;
  var o:tStoreObject;
  begin
  fid:=a.ReadPtr(20);
  try
    o.Init(fid^);
    p.ReadFrom(o,0);
    if not p.valid then raise eInvalidContainer.Create('zidan');
    ProfCache.SetProf( o, p );
    r.WriteByte(0);
    r.Write(p.ProfID,20);
    r.WriteWord6(p.Updated);
    o.Done;
  except
    on eObjectNF do r.WriteByte(opcode.otNotFound);
    on eInOutError do r.WriteByte(opcode.otFail);
    on eFormatError do r.WriteByte(5);
    on eInvalidContainer do r.WriteByte(6);
  end;
end;

procedure ProfGet(var client:tClient; var a,r:tMemoryStream);
  var found:boolean;
  var pid:^tPID;
  var fid:tFID;
  var ver:Int64;
  begin
  pid:=a.ReadPtr(20);
  found:=GetProf(pid^,fid,ver);
  if found then begin
    r.WriteByte(0);
    r.Write(FID,20);
    r.WriteWord6(Ver);
  end else begin
    r.WriteByte(opcode.otNotFound);
  end;
end;
{$endif}

procedure tClient.Init2;
  begin
  {$ifdef ctlMutable}
  mutator:=nil;
  {$endif}
  {$ifdef ctlFetch}
  transf:=nil;
  {$endif}
end;
procedure tCLient.Int; begin
  {$ifdef ctlMutable}
  if assigned(mutator) then mutator^.done; mutator:=nil;
  {$endif}
  {$ifdef ctlFetch} if assigned(transf) then begin
    transf^.Detach(@FetchEvent);
    transf:=nil;
  end; {$endif}
end;
BEGIN
  Server1.Init;
  FillChar(methods,sizeof(methods),0);
  methods[00].Init(@GetInfo,0);
  methods[01].Init(@Terminate,0);
  {$ifdef ctlDHT}
  methods[02].Init(@DhtPeer,sizeof(tNetAddr));
  methods[14].Init(@DhtDump,0);
  {$endif}
  {$ifdef ctlStore}
    {$ifdef ctlStoreAdv}
  methods[03].Init(@StorePutLN,2); {path:string[all]}
  methods[09].Init(@StorePutMV,2); {path:string[allg]}
    {$endif}
  methods[04].Init(@StoreGet,28); {fid:20;ofs,len:Word4}
  methods[08].Init(@StorePut,4); {len:Word4}
  methods[10].Init(@StoreStat,20); {fid:20}
  methods[11].Init(@StoreRefAdj,21); {fid:20, adj:byte+128}
  {$endif}
  {$ifdef ctlFetch}
  methods[12].Init(@FetchStart,39); {fid:20; addr:18; prio:byte}
  methods[13].Init(@FetchQuery,20); {fid:20}
  {$endif}
  {$ifdef ctlMutable}
  methods[05].Init(@MutableGet,sizeof(tPID));
  methods[06].Init(@MutableSet,sizeof(tFID));
  methods[07].Init(@MutableUpdate,sizeof(tFID));
  {$endif}
  {$ifdef ctlProf}
  methods[15].Init(@ProfGet,sizeof(tPID));
  methods[16].Init(@ProfSet,sizeof(tFID));
  {$endif}
END.

