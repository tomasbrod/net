Unit CTRL;
INTERFACE
IMPLEMENTATION
USES ServerLoop,opcode
    ,MemStream,NetAddr,Sockets,BaseUnix
    ,SysUtils
    ,dht,dhtLookup
    ,Store2
    //,Fetch
    ;

type tClient=object
  s:tSocket;
  error:boolean;
  SndObj:^Store2.tStoreObject;
  SndObjLeft:LongWord;
  search:^dhtLookup.tSearch;
  //transf:^Fetch.tFetch;
  procedure Init(i_s:tSocket);
  procedure Init2;
  procedure Done;
  procedure Event(ev:word);
  procedure SendTo(msg:tMemoryStream);
  procedure SendObject(var o:tStoreObject; ilen:LongWord);
  {procedure SP1(const pid:tPID);
  procedure SP2(const Source:tNetAddr; var extra:tMemoryStream);}
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

(*procedure ProfileSet(var client:tClient; var a,r:tMemoryStream);
  var fid:^tFID;
  var pid:tPID;
  var o:tStoreObject;
  var valid:boolean;
  begin
  fid:=a.ReadPtr(20);
  try
    o.Init(fid^);
    valid:=CacheProfile(o, pid);
    if valid then begin
      r.WriteByte(0);
      r.Write(pid,20);
      r.WriteWord($FFFFFFFF,4);
    end else r.WriteByte(1);
  except
    on eObjectNF do r.WriteByte(opcode.otNotFound);
    on eInOutError do r.WriteByte(opcode.otFail);
    on Exception do r.WriteByte(opcode.otFail);
  end;
end;

procedure ProfileGet(var client:tClient; var a,r:tMemoryStream);
  var des:tProfileMeta;
  var found:boolean;
  var pid:^tPID;
  begin
  pid:=a.ReadPtr(20);
  found:=GetProfileMeta(pid^,des);
  if found then begin
    r.WriteByte(0);
    r.Write(des.FID,20);
    r.Write(des.Update,4);
  end else begin
    r.WriteByte(opcode.otNotFound);
  end;
end;

procedure ProfileUpdate(var client:tClient; var a,r:tMemoryStream);
  var pid:^tPID;
  begin
  pid:=a.ReadPtr(20);
  client.SP1(pid^);
  r.WriteByte(0);
end; procedure tClient.SP1(const pid:tPID);
  begin
  New(search);
  search^.Init(pid,capProfile,@SP2);
  writeln('Ctrl.SP1: going to lookup profile ',string(search^.Target));
  search^.Start;
end; procedure tCLient.SP2(const Source:tNetAddr; var extra:tMemoryStream);
  var fid:^tfID;
  var upd:LongWord;
  var r:tMemoryStream;
  begin
  if Source.isNil then begin
    r.Init(3);r.WriteWord(1,2);
    r.WriteByte(1);
    SendTo(r);r.Free;
    writeln('Ctrl.SP2: exhausted');
    {todo: fetch}
  end else if extra.left>=24 then begin
    fid:=extra.ReadPtr(20);
    upd:=extra.ReadWord(4);
    r.Init(50);r.skip(2);
    r.WriteByte(2);
    r.WriteWord(upd,4);
    r.Write(source,sizeof(tNetAddr));
    r.Write(fid^,20);
    r.Seek(0);r.WriteWord(r.Length,2);SendTo(r);r.Free;
    writeln('Ctrl.SP2: ',upd,' from ',string(Source),' fid ',string(fid^));
    {todo: save}
  end else writeln('Ctrl.SP2: invalid from ',string(Source));
end;*)

procedure tClient.Init2; begin
  SndObjLeft:=0;
  search:=nil;
  //trans:=nil;
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
 {methods[12].Init(@ProfileGet,sizeof(tPID));}
 {methods[13].Init(@ProfileSet,sizeof(tFID));}
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
