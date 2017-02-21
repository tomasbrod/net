UNIT Mutable;
{

}
INTERFACE
USES ObjectModel,Store,SysUtils;

type tMutableInfo=record
  target: tFID;
  id: tFID;
  pubkey: tKey32;
  updated: Int64;
  expires: Int64;
  isnew: boolean;
end;

type
  eInvalidSignature=class(exception) end;
  eExpired=class(exception) end;

const cSignedLinkSize=132;

procedure SetSignedLink( var data: tMemoryStream; out info: tMutableInfo);
procedure ReadLinkInfo( const link_id: tFID; out info: tMutableInfo);

IMPLEMENTATION
USES opcode,ServerLoop,DHT,crypto,ed25519,Database;

procedure ReadLinkInfo( const link_id: tFID; out info: tMutableInfo);
  var vl:tMemoryStream;
  begin
  vl:=ReadLinkData(link_id);
  try
    try
      vl.Read(info.target,24);
      vl.Read(info.pubkey,32);
      info.updated:=vl.ReadWord6;
      info.expires:=vl.ReadWord6;
    except
      on eReadPastEoF do raise Exception.Create('Invalid store link data');
    end;
    SHA256_Buffer( info.id, 24, info.pubkey, 32);
    info.id[23]:=info.id[23] and 254;
    info.isnew:=false;
  finally vl.Free; end;
end;

procedure SetSignedLink( var data: tMemoryStream; out info: tMutableInfo);
  var signeddata,sig:pointer;
  var inp: tMemoryStream;
  var old: tMutableInfo;
  var now2:Int64;
  begin
  inp.Init(data.ReadPtr(cSignedLinkSize),cSignedLinkSize,cSignedLinkSize);
  signeddata:=inp.RdBuf;
  inp.Read(info.target,24);
  inp.Read(info.pubkey,32);
  info.updated:=inp.ReadWord6;
  info.expires:=inp.ReadWord6;
  SHA256_Buffer( info.id, 24, info.pubkey, 32);
  info.id[23]:=info.id[23] and 254;
  now2:=UnixNow;
  if info.updated>now2 then raise eExpired.Create('Link Updated in Future');
  if info.expires<=now2 then raise eExpired.Create('Link Signature Expired');
  {read existing data}
  try
    ReadLinkInfo(info.id,old);
    if (old.updated>=info.updated) {existing is more recent}
    and(old.updated<now2) {existing is not updated in future}
    then begin
      info:=old;
      exit;
    end;
  except
    on eObjectNF do {continue};
  end;
  sig:=inp.readptr(64);
  if not ed25519.Verify(tKey64(sig^),signeddata^,68,info.pubkey)
   then raise eInvalidSignature.Create('Link Signature Invalid');
  writeln('Mutable.Set: ',string(info.id),'->',string(info.target));
  InsertLinkData(info.id, inp);
  info.isnew:=true;
end;

{function CapHMutable(const source:tNetAddr; caps:byte; const Target:tPID; var extra:tMemoryStream):boolean;
  var r:tMemoryStream;
  var des:tMutableMeta;
  begin
  write('Mutable.Cap: ',string(Target));
  result:=db.GetVal(Target,des);
  writeln(' ',result);
  assert(caps=capMutable);
  if result then begin
    r.Init(200);
    r.WriteByte(opcode.dhtCapable);
    r.Write(dht.MyID,20);
    r.Write(Target,20);
    r.WriteByte(caps);
    r.Write(des.FID,sizeof(des.FID));
    r.Write(des.Ver,sizeof(des.Ver));
    SendMessage(r.base^,r.length,source);
    FreeMem(r.base,r.size);
  end;
end;}

BEGIN
  //SetMsgHandler(opcode.mutableUpdate,@recvUpdate);
END.
