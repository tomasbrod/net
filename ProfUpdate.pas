UNIT Mutator;
INTERFACE
USES MemStream,NetAddr,Store2,dhtLookup,Fetch;

type tMutatorRslt=record
  Ver:LongWord;
  Fid:tFID;
  Src:tNetAddr;
  next:^tMutatorRslt;
  end;
type tMutEvt=(
      meSearchEnd=1, meSearchFound, meSearchInvalid, meFetchStart,
      meFetchLocal,  meFetchSource, meFetchError,    meCheckOK,
      meCheckOld,    meCheckBad,    meNotify,      meSendEnd,
      meSearchPeer
             );
type tMutator=object
  Target:tFID; {id of mutable}
  OnEvent:procedure ( ev:tMutEvt; ver:longword; const fid:tFID; const Src:tNetAddr ) of object;
  OnComplete:procedure of object;
  FinalMeta:tMutableMeta;
  Sending:boolean;
  procedure Init(aTarget:tFID);
  procedure Done; unimplemented;
  private
  Search:^dhtLookup.tSearch;
  Fetchj:^tFetch;
  Found:^tMutatorRslt;
  Peers:array [0..5] of tSearchPeer;
  CurrentFetch:tMutatorRslt;
  procedure SRslt(const Source:tNetAddr; var extra:tMemoryStream);
  procedure SProgr(pfl:byte; const p:tSearchPeer);
  procedure FetchStart;
  procedure FetchEvent;
  procedure DoSendLocals;
  procedure DoSendLocals2;
  function  DoCheck( var so:tStoreObject ):boolean;
  procedure Destroy; experimental;
  end;

IMPLEMENTATION
USES opcode,ServerLoop,DHT,HKVS,sha512,ed25519;

(****** Mutator ******)
procedure tMutator.Init(aTarget:tFID);
  begin
  New(search);
  Sending:=false;
  Target:=aTarget;
  OnEvent:=nil;
  OnComplete:=nil;
  search^.Init(Target,capMutable,@SRslt);
  search^.OnProgress:=@SProgr;
  Fetchj:=nil;
  Found:=nil;
  Shedule(1,@search^.Start);
end;
procedure tMutator.Done;
  begin
  {if called while searching: destroy}
  {if called while fetching: destroy}
  {if called while Sending: keep background}
  {bevare when called from OnEvent}
  if not Sending then Destroy
  else begin
    OnComplete:=nil;
    OnEvent:=nil;
    {}
  end;
end;
procedure tMutator.SProgr(pfl:byte; const p:tSearchPeer);
  begin
  if assigned(onEvent) then OnEvent(meSearchPeer,pfl,p.id,p.addr);
end;
procedure tMutator.SRslt(const Source:tNetAddr; var extra:tMemoryStream);
  var p,n:^tMutatorRslt;
  var pp:^pointer;
  var fid:^tfID;
  var Ver:LongWord;
  var a,b:shortint;
  begin
  if Source.isNil then begin
    {copy closest peers}
    b:=0; for a:=0 to high(Search^.Peers) do begin
      if (b>high(Peers))and Search^.Peers[a].addr.isNil then break;
      Peers[b]:=Search^.Peers[a]; inc(b);
    end; if b<=high(Peers) then Peers[b].Addr.Clear;
    if assigned(OnEvent) then OnEvent(meSearchEnd,0,tFID(nil^),tNetAddr(nil^));
    search:=nil;
    Shedule(10,@FetchStart);
  end else if extra.left>=24 then begin
    {real result}
    fid:=extra.ReadPtr(20);
    Ver:=extra.ReadWord(4);
    {store results in linkedlist}
    if assigned(OnEvent) then OnEvent(meSearchFound,Ver,Fid^,Source);
    {highest version first}
    pp:=@Found;  p:=pp^;
    while assigned(p) do begin
      if p^.Src=Source then exit; {$hint not effective}
      if p^.Ver<Ver then break;
      {grop same FIDs together}
      if (p^.Ver=Ver) and (p^.FID=FID^) then break;
    end;
    new(n);  n^.next:=p;  pp^:=n;
    n^.Ver:=ver;
    n^.Fid:=fid^;
    n^.Src:=Source;
  end else if assigned(OnEvent) then OnEvent(meSearchInvalid,0,tKey20(nil^),Source);
end;
procedure tMutator.FetchStart;
  var p:^tMutatorRslt;
  var so:tStoreObject;
  var check:boolean;
  begin
  if assigned(Found) then begin {value found}
    p:=Found;
    CurrentFetch:=p^;
    FetchJ:=nil;
    {$hint, do not fetch older or same as DB, proceed to propagate; but it does not hurt}
    try so.Init(CurrentFetch.fid);
    except {not found: download}
      on eObjectNF do FetchJ:=FetchObject(p^.fid, p^.Src, 48, @self.FetchEvent);
    end;
    if assigned(FetchJ) then begin
      FetchJ^.SetMaxSize(4096);
      if assigned(OnEvent) then OnEvent(meFetchStart,CurrentFetch.Ver,CurrentFetch.Fid,p^.src);
    end else if assigned(OnEvent) then OnEvent(meFetchLocal,CurrentFetch.Ver,CurrentFetch.Fid,tNetAddr(nil^));
    repeat
      p:=p^.next;
      Dispose(Found);
      Found:=p;
      if (p=nil) or (CurrentFetch.fid<>p^.fid) then break;
      if assigned(FetchJ) then begin
        FetchJ^.AddSource(p^.Src);
        if assigned(OnEvent) then OnEvent(meFetchSource,CurrentFetch.Ver,CurrentFetch.Fid,p^.Src);
      end;
    until false;
    {value was found, if job is nil, is already opened from store}
    if FetchJ=nil then begin
      check:=DoCheck(so);
      so.Close;
      if check then DoSendLocals;
    end;
  end
  {no more Found results to try, at least send what we have}
  else DoSendLocals;
end;
procedure tMutator.FetchEvent;
  var so:tStoreObject;
  var check:boolean;
  begin
  if FetchJ^.Done then begin
    //if assigned(OnEvent) then OnEvent(meFetchDone,CurrentFetch.Ver,CurrentFetch.Fid,tNetAddr(nil^));
    {download is OK, proceed to check mutable}
    so.Init(CurrentFetch.FID);
    FetchJ:=nil;
    check:=DoCheck(so);
    so.Reference(-1); {anyway unref the object here, SetMutable does Ref}
    so.Close;
    if check
      {mutable is OK}
      then DoSendLocals
      {mutable is invalid, try next result}
      else Shedule(300,@FetchStart)
    ;
  end else begin
    {download failed, try next result}
    Shedule(300,@FetchStart);
    FetchJ:=nil;
    if assigned(OnEvent) then OnEvent(meFetchError,ORD(FetchJ^.Error),CurrentFetch.Fid,tNetAddr(nil^));
  end;
end;
function tMutator.DoCheck( var so:tStoreObject ):boolean;
  begin
  if SetMutable( so, FinalMeta )and(FinalMeta.Fid=Target) then begin
    {sender may lie about his version, check if >= than expected}
    if (LongWord(FinalMeta.Ver)>=CurrentFetch.Ver) then begin
      Result:=true;
      if assigned(OnEvent) then OnEvent(meCheckOK,FinalMeta.Ver,CurrentFetch.Fid,tNetAddr(nil^));
    end
    else if assigned(OnEvent) then OnEvent(meCheckOld,FinalMeta.Ver,CurrentFetch.Fid,tNetAddr(nil^));
  end
  else if assigned(OnEvent) then OnEvent(meCheckBad,0,CurrentFetch.Fid,tNetAddr(nil^));
end;
procedure tMutator.DoSendLocals;
  var dbMeta:tMutableMeta;
  begin
  Sending:=true;{$note dont Notify if no-result and not in db}
  if GetMutable(Target,dbMeta) then begin
    FinalMeta:=dbMeta;
    {send update to Found and Peers}
    Shedule(300,@DoSendLocals2);
    OnComplete; {signal success, may destroy self, must be called last}
  end else begin
    OnComplete; {signal success, may destroy self, must be called last}
    if assigned(OnEvent) then OnEvent(meSendEnd,0,tFID(nil^),tNetAddr(nil^));
    Destroy;
  end;
end;
procedure tMutator.DoSendLocals2;
  var p:^tMutatorRslt;
  var i:integer;
  var ded:boolean=true;
  procedure SendTo(const trg:tNetAddr);
    var pk:tMemoryStream;
    begin
    if assigned(OnEvent) then OnEvent(meNotify,FinalMeta.Ver,FinalMeta.Fid,Trg);
    pk.Init(45);
    pk.WriteByte(opcode.mutableUpdate);
    pk.WriteWord(FinalMeta.Ver,4);
    pk.Write(Target,20);
    pk.Write(FinalMeta.FID,20);
    ServerLoop.SendMessage(pk.base^,pk.Length, Trg );
    pk.Free;
  end;
  begin
  {send update to Found and Peers}
  {first Founds}
  if assigned(Found) then begin
    p:=Found;
    Found:=p^.Next;
    SendTo(p^.Src);
    Dispose(p);
    ded:=false;
  end else for i:=0 to high(Peers) do if not Peers[i].Addr.isNil then begin
      SendTo(Peers[i].Addr);
      Peers[i].Addr.Clear;
      ded:=false;
      break;
  end;
  Shedule(250,@DoSendLocals2);
  if ded then begin
    if assigned(OnEvent) then OnEvent(meSendEnd,FinalMeta.VER,FinalMeta.FID,tNetAddr(nil^));
    Destroy;
  end;
end;
procedure tMutator.Destroy;
  var p,q:^tMutatorRslt;
  begin
  {kill subworkers}
  if assigned(search) then search^.close;
  if assigned(FetchJ) then FetchJ^.Abort(@FetchEvent);
  {free Found list}
  p:=Found;
  while assigned(p) do begin
    q:=p;
    p:=p^.next;
    Dispose(q);
  end;
  {remove timers}
  UnShedule(@FetchStart);
  UnShedule(@DoSendLocals2);
  {ded}
  FreeMem(@self,sizeof(self));
end;

function CapHMutable(const source:tNetAddr; caps:byte; const Target:tPID; var extra:tMemoryStream):boolean;
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
end;

(****** Upate on Notify ******)
var UpdatesInProgress:Word;
type tMutableUpdate=object
  J:^tFetch;
  FID,MID:tFID;
  Src:tNetAddr;
  procedure ev;
end;

procedure recvUpdate(msg:tSMsg);
  var s:tMemoryStream absolute msg.stream;
  var ver,mver:LongWord;
  var has:boolean;
  var fid,mid:^tFID;
  var meta:tMutableMeta;
  var o:^tMutableUpdate;
  begin
  s.skip(1);
  ver:=s.readword(4);
  mid:=s.readPtr(20);
  fid:=s.readPtr(20);
  {Consult DB}
  has:=GetMutable(mid^,meta); mver:=meta.ver;
  if (not has) or (mver<ver) then begin
    if UpdatesInProgress>=16 then begin
      writeln('Mutable.recvUpdate: too many updates');
    exit end;
    writeln('Mutable.recvUpdate: ',string(mid^),' v',ver,' ',string(fid^));
    {Start Fetch from source}
    new(O);
    O^.MID:=mid^;
    O^.FID:=fid^;
    O^.Src:=msg.source^;
    O^.J:=FetchObject(fid^, msg.source^, 9, @O^.ev);
    if O^.J=nil then O^.EV; {todo...}
  end else writeln('Mutable.recvUpdate: ',string(msg.source^),' v',ver,'<=',mver);
end;
procedure tMutableUpdate.ev;
  var so:tStoreObject;
  var meta:tMutableMeta;
  var valid:boolean;
  begin
  if (J=nil) or (J^.Done) then begin
    so.Init(FID);
    valid:=SetMutable(so,meta);
    if valid then begin
      if meta.fid=mid
      then writeln('Mutable: ',string(meta.fid),' updated to v',LongWord(meta.ver),' ',string(FID))
      else begin
        writeln('Mutable.Update.ev: ',string(Src),' MutID mismatch!');
        {...delete?}
      end;
    end
    else writeln('Mutable.Update.ev: ',string(Src),' invalid signature!');
    so.Reference(-1);
  end else begin
    writeln('Mutable.Update.ev: ',string(Src),' Fetch failed ',J^.Error);
  end;
  FreeMem(@self,sizeof(self));
end;

BEGIN
  dht.RegisterCapability(capMutable,@CapHMutable);
  SetMsgHandler(opcode.mutableUpdate,@recvUpdate);
  UpdatesInProgress:=0;
END.
