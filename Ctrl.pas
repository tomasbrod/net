Unit CTRL;
INTERFACE
IMPLEMENTATION
USES ServerLoop,opcode
    ,MemStream,NetAddr,Sockets,BaseUnix
    ,SysUtils
    ,dht,dhtLookup
    ,Store2
    ,Mutable
    //,Fetch
    ;

type tClient=object
  s:tSocket;
  error:boolean;
  SndObj:^Store2.tStoreObject;
  SndObjLeft:LongWord;
  mutator:^tMutator;
  //transf:^Fetch.tFetch;
  procedure Init(i_s:tSocket);
  procedure Init2;
  procedure Done;
  procedure Int;
  procedure Event(ev:word);
  procedure SendTo(msg:tMemoryStream);
  procedure SendObject(var o:tStoreObject; ilen:LongWord);
  procedure MutatorComplete;
  procedure MutatorEvent( ev:tMutEvt; ver:longword; const fid:tFID; const Src:tNetAddr );
end;
{$I CtrlLow.pas}

procedure Terminate(var client:tClient; var a,r:tMemoryStream);
 begin
 ServerLoop.RequestTerminate(0);
end;

procedure GetInfo(var client:tClient; var a,r:tMemoryStream);
 const ident:string=VersionBrand+' ';
 begin
 r.Write(ident[1],length(ident));
 r.Write(VersionString[1],length(VersionString));
 r.WriteByte(0);
end;

procedure DhtPeer(var client:tClient; var a,r:tMemoryStream);
  var contact:^tNetAddr;
  begin
  contact:=a.ReadPtr(sizeof(tNetAddr));
  DHT.NodeBootstrap(contact^);
  r.WriteByte(0);
end;

procedure StoreLocalCopy(var client:tClient; var a,r:tMemoryStream);
  var path:ansistring;
  var l:longword;
  var fid:tFID;
  begin
  l:=a.Left;
  if l<2048 then begin
    SetLength(path,l);
    a.Read(path[1],l);
    //writeln('Ctrl.StoreLocalCopy: ',path);
    try
      Store2.HashObjectCopy(path,fid);
      r.WriteByte(0);
      r.Write(fid,sizeof(fid));
    except
      on eInOutError do r.WriteByte(opcode.otFail);
    end;
  end else client.Error:=true;
end;

procedure StoreGet(var client:tClient; var a,r:tMemoryStream);
  var o:tStoreObject;
  var id:^tFID;
  var ofs,len:LongWord;
  begin
  try
  id:=a.ReadPtr(20);
  ofs:=a.ReadWord(4);
  len:=a.ReadWord(4);
  except client.error:=true; exit end;
  try
    writeln('reqf_',string(id^),' @',ofs,'+',len);
    o.Init(id^);
  except
    on eObjectNF do begin r.WriteByte(opcode.otNotFound); exit end;
    on eInOutError do begin r.WriteByte(opcode.otFail); exit end;
  end;
  if ofs>o.Length then r.WriteByte(opcode.otEoT)
  else begin
    o.Seek(ofs);
    if len>o.Left then len:=o.Left;
    r.WriteByte(0);
    r.WriteWord(len,4);
    (*client.SendObject(o,len);
    exit; {closed by low}*)
  end;
  o.Close;
end;

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
end;

procedure tClient.MutatorEvent( ev:tMutEvt; ver:longword; const fid:tFID; const Src:tNetAddr );
  var r:tMemoryStream;
  begin
  r.Init(49);
  r.WriteByte(ORD(ev));
  r.WriteWord(ver,4);
  r.Write(fid,20);
  r.Write(Src,24);
  SendTo(r);r.free;
  if ev=meSendEnd then mutator:=nil;
end;

procedure tClient.Init2; begin
  SndObjLeft:=0;
  mutator:=nil;
  //trans:=nil;
end;
procedure tCLient.Int; begin
  if assigned(mutator) then mutator^.done; mutator:=nil;
end;
BEGIN
 Server1.Init;
 FillChar(methods,sizeof(methods),0);
 methods[00].Init(@GetInfo,0);
 methods[01].Init(@Terminate,0);
 {ethods[02] restart}
 methods[03].Init(@DhtPeer,sizeof(tNetAddr));
 {ethods[04] DhtInfo}
 {ethods[05] Dht}
 {ethods[06] Dht}
 {ethods[07] StoreInfo}
 methods[08].Init(@StoreLocalCopy,2);
 {ethods[09] StorePut}
 methods[10].Init(@StoreGet,28);
 {ethods[11] StoreRef}
 methods[12].Init(@MutableGet,sizeof(tPID));
 methods[13].Init(@MutableSet,sizeof(tFID));
 {ethods[14].Init(@ProfileList,0);}
 {methods[15].Init(@ProfileUpdate,sizeof(tFID));}
END.
{profile update:
  - async query
  - another query on same connection causes error
  - status messages returned immediatly:
  	(1/2) Searching DHT
  	capProfile: [num] from [ip] fid [fid]
  	(2/2) Fetching version [update]: [fid]
  	[ip]: success/failure/abort
  	* file corrupt or not a profile
}
