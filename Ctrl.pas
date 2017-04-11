Unit CTRL;
INTERFACE
IMPLEMENTATION
{$define ctlDHT}
{$define ctlStore}
{-define ctlSignedLink}
{-define ctlProf}
{-define ctlFetch}
USES ServerLoop,opcode
    ,ObjectModel
    ,Sockets,BaseUnix
    ,SysUtils
    ,Crypto
    {$ifdef ctlDHT},DHT{$endif}
    ,Store
    {$ifdef ctlSignedLink},Mutable{$endif}
    {$ifdef ctlProf},Profile,ProfCache{$endif}
    {$ifdef ctlFetch},Fetch{$endif}
    ;

type tClient=object
  s:tSocket;
  iphash:tKey32;
  error:boolean;
  procedure Init(i_s:tSocket);
  procedure Init2;
  procedure Done;
  procedure Interrupt;
  procedure Event(ev:word);
  procedure Command(method:word; var args:tMemoryStream); inline;
  procedure SendTo(msg:tMemoryStream);

  procedure Terminate(var a,r:tMemoryStream);
  procedure GetInfo(var a,r:tMemoryStream);

  {$ifdef ctlDHT}public
  procedure DhtPeer(var a,r:tMemoryStream);
  procedure DhtDump(var a,r:tMemoryStream);
  {$endif}

  {$ifdef ctlStore}public
  SndObj:^Store.tStoreObject;
  SndObjLeft:LongWord;
  RcvObj:^tFileStream;
  RcvObjLeft:LongWord;
  RcvObjHctx:tSha256;
  procedure StoreGetPath(var a,r:tMemoryStream);
  procedure StoreGet(var a,r:tMemoryStream);
  procedure StorePut(var a,r:tMemoryStream);
  procedure StoreReadlink(var a,r:tMemoryStream);
  procedure SendObject(var o:tStoreObject; ilen:LongWord);
  procedure RcvObjComplete;
  {$endif}

  {$ifdef ctlSignedLink}
  public
  procedure StoreSetLink(var a,r:tMemoryStream);
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

procedure tClient.Command(method:word; var args:tMemoryStream);
  var resp_stream:tMemoryStream;
  begin
  resp_stream.Init(2048);
  resp_stream.Write(method,2);
  try
  case method of
    000: GetInfo(args,resp_stream);
    001: Terminate(args,resp_stream);
    {$ifdef ctlDHT}
    002: DhtPeer(args,resp_stream);
    014: DhtDump(args,resp_stream);
    {$endif}
    {$ifdef ctlStore}
    021: StoreGetPath(args,resp_stream);
    022: StoreGet(args,resp_stream);
    023: StorePut(args,resp_stream);
    024: StoreReadlink(args,resp_stream);
    {$endif}
    {$ifdef ctlSignedLink}
    025: StoreSetLink(args,resp_stream);
    {$endif}
    else error:=true;
  end;
  except on eReadPastEoF do begin
    resp_stream.writebyte(1); error:=true;
  end end;
  if not error then begin
    resp_stream.Seek(0);
    resp_stream.WriteWord2(resp_stream.Length-2);
    self.SendTo(resp_stream); resp_stream.Free;
  end;
end;
procedure tClient.Init2;
  begin
  {$ifdef ctlMutable}
  mutator:=nil;
  {$endif}
  {$ifdef ctlFetch}
  transf:=nil;
  {$endif}
end;
procedure tCLient.Interrupt; begin
  {$ifdef ctlMutable}
  if assigned(mutator) then mutator^.done; mutator:=nil;
  {$endif}
  {$ifdef ctlFetch} if assigned(transf) then begin
    transf^.Detach(@FetchEvent);
    transf:=nil;
  end; {$endif}
end;

{$I CtrlLow.pas} {**** Commands implementation ****}

procedure tClient.Terminate(var a,r:tMemoryStream);
  begin
  writeln('Ctrl.Terminate');
  r.WriteByte(0);
  ServerLoop.RequestTerminate(0);
end;

procedure tClient.GetInfo(var a,r:tMemoryStream);
  begin
  r.Write(VersionString[1],length(VersionString));
  r.WriteByte(0);
end;

{$ifdef ctlDHT}
procedure tClient.DhtPeer(var a,r:tMemoryStream);
  var contact:^tNetAddr;
  begin
  try
    contact:=a.ReadPtr(sizeof(tNetAddr));
  except
    on eReadPastEoF do begin error:=true; exit; end;
  end;
  DHT.NodeBootstrap(contact^);
  r.WriteByte(0);
end;

procedure tClient.DhtDump(var a,r:tMemoryStream);
  var bkt: ^DHT.tBucket;
  var i:integer;
  begin
  r.WriteByte(0);
  r.Write(DHT.MyID,20);
  bkt:=DHT.GetDHTTable;
  while assigned(bkt) do with bkt^ do begin
    r.WriteByte(Depth);
    r.WriteByte(high(peer));
    r.WriteWord4(MNow-ModifyTime);
    for i:=1 to high(peer) do with peer[i] do begin
      r.Write(ID,20);
      r.Write(Addr,sizeof(tNetAddr));
      r.WriteWord2(ReqDelta);
      r.WriteWord4(MNow-LastMsgFrom);
      r.WriteByte( ord(Banned) or (ord(Verified) shl 1));
    end;
    bkt:=next;
  end;
end;
{$endif}

{$ifdef ctlStore}
  {$ifdef ctlStoreAdv}
procedure tClient.StorePutLN(var client:tClient; var a,r:tMemoryStream);
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

procedure tClient.StorePutMV(var client:tClient; var a,r:tMemoryStream);
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

procedure tClient.StoreGetPath(var a,r:tMemoryStream);
  var id:^tFID;
  var path:AnsiString;
  var mode:byte;
  begin
  id:=a.ReadPtr(24);
  if not IsBlob(id^) then begin
    r.WriteByte(3);
  end else begin
    Store.GetBlobInfo(id^,mode,path);
    if mode=0 then r.WriteByte(2)
    else if mode=ord(solReference) then begin
      r.WriteByte(0);
      r.Write(path[1],length(path));
    end else if mode=ord(solNormal) then begin
      r.WriteByte(0);
      path:=GetCurrentDir+'/'+cStoreDir+string(id^);
      r.Write(path[1],length(path));
    end else r.WriteByte(4);
  end;
end;

procedure tClient.StoreReadlink(var a,r:tMemoryStream);
  var id:^tFID;
  var kl:longword;
  var vl:tMemoryStream;
  begin
  id:=a.ReadPtr(24);
  if IsBlob(id^) then begin
    r.WriteByte(3);
  end else begin
    try
      vl:=Store.ReadLinkData(id^);
    except
      on eObjectNF do begin r.WriteByte(2); exit end;
    end;
    r.WriteByte(0);
    kl:=vl.Left;
    vl.Read(r.WrBuf^,kl);
    r.WrEnd(kl);
  end;
end;

procedure tClient.StoreGet(var a,r:tMemoryStream);
  var o:^tStoreObject;
  var id:^tFID;
  var ofs,len,flen:LongWord;
  begin
  id:=a.ReadPtr(20);
  ofs:=a.ReadWord4;
  len:=a.ReadWord4;
  if not IsBlob(id^) then begin
    r.WriteByte(3); exit;
  end;
  try
    writeln('Ctrl.StoreGet: ',string(id^),' @',ofs,'+',len);
    New(o,Init(id^));
  except
    on eObjectNF do begin r.WriteByte(2); exit end;
  end;
  flen:=o^.Length;
  if ofs>flen
    then len:=0
  else if (flen-ofs)<len
    then len:=(flen-ofs);
  r.WriteByte(0);
  r.WriteWord4(flen);
  r.WriteWord4(len);
  o^.Seek(ofs);
  SendObject(o^,len);
end;

procedure tClient.StorePut(var a,r:tMemoryStream);
  var len:LongWord;
  begin
  len:=a.ReadWord4;
  New(RcvObj,OpenRW(Store.GetTempName(iphash,'ctl')));
  RcvObj^.Trunc(0);
  RcvObjLeft:=len;
  RcvObjHctx.Init;
  writeln('Ctrl.StorePUT: ',len);
  r.WriteByte(0);
end;
procedure tClient.RcvObjComplete;
  var r:tMemoryStream;
  var hash_id:tKey32;
  begin
  r.Init(27); try
  RcvObjHctx.Final(hash_id);
  Dispose(RcvObj,Done);
  writeln('Ctrl.RcvObjComplete: '+string(hash_id));
  Store.InsertBlobRename( Store.GetTempName(iphash,'ctl'), hash_id );
  r.WriteWord2(25);
  r.WriteByte(0);
  r.Write(hash_id,24);
  self.SendTo(r);
  finally r.Free; end;
end;
{$endif}

{$ifdef ctlSignedLink}
procedure tClient.StoreSetLink(var a,r:tMemoryStream);
  var info:tMutableInfo;
  begin
  try
    SetSignedLink( a, info);
  except
    on eInvalidSignature do r.WriteByte(3);
    on eExpired do r.WriteByte(4);
  end;
  if info.isnew then r.WriteByte(0)
                else r.WriteByte(5);
  r.Write(info.id,24);
  r.WriteWord6(info.updated);
  r.WriteWord6(info.expires);  
end;
{$endif}

{$ifdef ctlFetch}
procedure tClient.FetchStart(var client:tClient; var a,r:tMemoryStream);
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

procedure FtClient.etchQuery(var client:tClient; var a,r:tMemoryStream);
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
procedure tClient.MutableSet(var client:tClient; var a,r:tMemoryStream);
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

procedure tClient.MutableGet(var client:tClient; var a,r:tMemoryStream);
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

procedure tClient.MutableUpdate(var client:tClient; var a,r:tMemoryStream);
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
procedure tClient.ProfSet(var client:tClient; var a,r:tMemoryStream);
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

procedure tClient.ProfGet(var client:tClient; var a,r:tMemoryStream);
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

BEGIN
  Server1.Init;
END.
