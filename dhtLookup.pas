unit dhtLookup;
INTERFACE
uses ServerLoop,dht,NetAddr;

type
  tPID=dht.tPID;
  tSearchPeer=object(dht.tPeerPub)
    Next:^tSearchPeer;
    LastReq:tMTime;
    caps:byte;
    reqc,rplc:byte;
    Extra:string[24];
    end;
  tSearch=object
    Target:tPID;
    Caps:byte;
    Extra:string[24];
    First:^tSearchPeer;
    Passive:boolean;
    Callback:procedure(const peer:tSearchPeer) of object;
    procedure Init;
    procedure Start;
    procedure Done;
    private
    next,prev:^tSearch;
    function AddPeer(const iID:tPID; const iAddr:tNetAddr; setrepl:boolean): pointer;
    procedure AddCapable(const sID:tPID; const Source:tNetAddr; scaps:byte; exl:byte; exp:pointer);
    procedure Step;
    procedure Periodic;
    end;
IMPLEMENTATION
uses MemStream,opcode,Store2;
var Searches:^tSearch;

procedure tSearch.Init;
  begin
  Caps:=0;
  Extra:='';
  First:=nil;
  Passive:=false;
  Callback:=nil;
end;

procedure tSearch.Start;
  var list:pointer;
  var adc:byte;
  var ipeer:tPeerPub;
  begin
  {$hint todo check for identical search and mark as passive}
  next:=Searches;
  prev:=nil;
  if assigned(Searches) then Searches^.prev:=@self;
  Searches:=@self;
  GetFirstNode(list,target);
  adc:=0;
  writeln('dhtLookup.Start@',string(@self),' target=',string(target),' caps=',caps,' exl=',length(extra));
  while adc<10 do begin
    GetNextNode(list,ipeer);
    if ipeer.addr.isnil then break;
    inc(adc);
    self.AddPeer(ipeer.id,ipeer.addr,false);
  end;
  DoneGetNextNode(list);
  Shedule(1,@Periodic);
end;

function tSearch.AddPeer(const iID:tPID; const iAddr:tNetAddr; setrepl:boolean): pointer;
  var cur,this:^tSearchPeer;
  var pcur:^pointer;
  var tpfl,idx:byte;
  label discard;
  begin
  pcur:=@first; cur:=First;
  idx:=0; result:=nil;
  tpfl:=PrefixLength(iid,Target);
  write('dhtLookup.AddPeer@',string(@self),' tpfl=',tpfl,' addr=',string(iaddr));
  while assigned(cur) and (PrefixLength(cur^.id,Target)>=tpfl)
  do begin
    if cur^.addr=iaddr then begin
      if setrepl then Inc(cur^.rplc);
      result:=cur;
      writeln(' update ',idx);
    exit end;
    if idx>10 then goto discard; {$hint magic constant}
    pcur:=@cur^.next; cur:=cur^.next;
    inc(idx);
  end;
  if setrepl then goto discard;
  new(this); this^.next:=cur; pcur^:=this;
  writeln(' insert ',idx);
  with this^ do begin
    id:=iid; addr:=iaddr;
    caps:=0;
    reqc:=0; rplc:=0;
    Extra:='';
  end;
  result:=this;
  exit; discard:
  writeln(' discard');
end;

procedure tSearch.Step;
  var p:^tSearchPeer;
  var pc,nc,again:byte;
  var r:tMemoryStream;
  var buf:array [1..66] of byte;
  begin
  r.Init(@buf,0,sizeof(buf));
  write('dhtLookup.Step@',string(@self),': ');
  p:=First; pc:=0;nc:=0; again:=0;
  while assigned(p) and (pc<4) do begin
    if (p^.reqc=0)
    or( (p^.rplc<=again)
    and (p^.reqc<4) )then begin
      if (mNow-p^.LastReq)>800 then begin
        inc(pc);
        r.WriteByte(opcode.dhtRequest);
        r.Write(dht.MyID,sizeof(tPID));
        r.Write(self.Target,sizeof(tPID));
        r.WriteByte(self.caps);
        r.Write(extra[1],length(extra));
        write('[',pc,':',string(p^.addr),' ',p^.rplc,'/',p^.reqc);
        inc(p^.reqc);
        p^.LastReq:=MNow;
        ServerLoop.SendMessage(r.base^,r.length,p^.Addr);
      end else inc(nc);
    end;
    p:=p^.next;
    if (p=nil) and (again<3) and (nc<=3) then begin p:=First; inc(again); end;
  end;
  if (pc+nc)=0 then begin
    writeln('search failed');
    //search failed
    {$warning notify of search failure}
  end else writeln;
end;

procedure tSearch.AddCapable(const sID:tPID; const Source:tNetAddr; scaps:byte; exl:byte; exp:pointer);
  var p:^tSearchPeer;
  begin
  p:=self.AddPeer(sID,source,true);
  if assigned(p) then begin
    writeln('dhtLookup.AddCapable@',string(@self),' ',string(Source),' scaps=',caps,' exl=',exl);
    p^.caps:=scaps;
    SetLength(p^.Extra,exl);
    if exl>0 then Move(exp^,p^.Extra[1],exl);
    if (Callback<>nil) then Callback(p^);
  end else  writeln('dhtLookup.AddCapable@',string(@self),' discard ',string(Source));
end;

procedure tSearch.Periodic;
  begin
  Shedule(900,@Periodic);
  Step;
end;

procedure tSearch.Done;
  var rs,rsn:^tSearchPeer;
  begin
  {$hint todo check for identical search and mark as active if active}
  rs:=First; while assigned(rs) do begin
    rsn:=rs^.next; Dispose(rs); rs:=rsn;
  end;
  UnShedule(@Periodic);
end;

procedure UpdateSearch(const ID: tPID; const Addr:tNetAddr; rpc:boolean);
  var sr:^tSearch;
  //called by dhr.RecvPers with sender info
  begin
  sr:=Searches; while assigned(sr) do begin
    sr^.AddPeer(ID,Addr,rpc);
    sr:=sr^.next;
  end;
end;

{c) op, SenderID, caps, extra}
procedure RecvCapable(msg:tSMsg);
  unimplemented;
  var sr:^tSearch;
  var sID:^tPID;
  var caps:byte;
  var exl:word;
  var exp:pointer;
  begin
  msg.stream.skip(1);
  sID:=msg.stream.readptr(sizeof(tPID));
  caps:=msg.stream.readByte;
  exl:=msg.stream.RdBufLen;
  if exl>24 then exl:=24;
  exp:=msg.stream.readptr(exl);
  sr:=Searches; while assigned(sr) do begin
    if sr^.caps=caps then begin
      sr^.AddCapable(sID^,msg.Source^,caps,exl,exp);
    end;
    sr:=sr^.next;
  end;
end;


BEGIN
  dht.OnNewPeer:=@UpdateSearch;
  ServerLoop.SetMsgHandler(opcode.dhtCapable,@recvCapable);
END.
