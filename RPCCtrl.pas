Unit CTRL;
{ Remote Procedures that dont belong to separate units }
{ Daemon control,
  Store Put, Get, Link
}

INTERFACE
IMPLEMENTATION

USES ServerLoop,opcode
    ,ObjectModel
    ,Sockets,BaseUnix
    ,SysUtils
    ,Crypto
    ,Store
    {$ifdef ctlSignedLink},Mutable{$endif}
    {$ifdef ctlProf},Profile,ProfCache{$endif}
    {$ifdef ctlFetch},Fetch{$endif}
    ;

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


BEGIN
  Server1.Init;
END.
