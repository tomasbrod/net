UNIT UPMGR;
{Upload Manager for brodnetd}

INTERFACE
USES Chat,TC,opcode,ServerLoop,MemStream,NetAddr;

IMPLEMENTATION
USES ZidanStore;

type
tAggr_ptr=^tAggr;
tPrv_ptr=^tPrv;
tPrv=object
 aggr:tAggr_ptr;
 ch: ^tChat;
 chan: byte;
 next,prev: tPrv_ptr;
 weight,wcur:Word;
 isOpen,Active:boolean;
 seglen:LongWord;
 datafile:file of byte;
 procedure Init(ag:tAggr_ptr; var nchat:tChat; msg: tSMsg);
 procedure OnMsg(msg:tSMsg; data:boolean);
 procedure IdleTimeout;
 procedure ChatTimeout(willwait:LongWord);
 procedure Cont;
 procedure DoGET(const fid:tfid; base,limit:LongWord);
 procedure DoSEG(base,limit:LongWord);
 procedure Start;
 procedure Stop;
 procedure DoClose;
end;
tAggr=object
 tcs: tTCS;
 rekt:boolean;
 next,prev: tAggr_ptr;
 prv:^tPrv;
 Cnt:Byte;
 procedure UnRef;
 procedure Cont;
 procedure Init(const source:tNetAddr);
 procedure Done;
 procedure TCTimeout;
end;

var Peers:^tAggr;

{Requests
Close();
GET(filehash:20; baseHi:word2; base:word4; limit:word4);
SEG(baseHi:word2; base:word4; limit:word4);
}{Responses
INFO(sizeHi:word2; size:word4; final:byte; seglen:word4);
FAIL(code:byte;...);
DONE();
}

procedure tPrv.DoGET(const fid:tfid; base,limit:LongWord);
 var err:tmemorystream;
 var info:tStoreObjectInfo;
 begin
 Assert(not(active or isOpen)); //todo
 ch^.Ack;
 info.Open(fid);
 if not info.final then begin
  info.rc:=200;
  Close(info.hnd);
 end;
 if info.rc>0 then begin
  ch^.StreamInit(err,3);
  err.WriteByte(upFAIL);
  if (info.rc=1)or(not info.final) then err.WriteByte(upErrNotFound)
  else begin err.WriteByte(upErrIO); err.WriteByte(info.rc) end;
  ch^.Send(err);
 end else begin
  ch^.StreamInit(err,12);
  err.WriteByte(upINFO);
  datafile:=info.hnd;
  isopen:=true;
  err.WriteWord(0,2);
  err.WriteWord(info.length,4);
  seglen:=limit;
  if info.length<seglen then seglen:=info.length;
  err.WriteByte(1);
  err.WriteWord(seglen,4);
  ch^.Send(err);
  Start;
 end;
end;

procedure tPrv.DoSEG(base,limit:LongWord);
 begin
end;

procedure tPrv.Start;
 begin
 Assert(isOpen);
 Assert(not Active);
 Active:=true;
 UnShedule(@IdleTimeout);
 if not assigned(aggr^.prv) then begin
  next:=@self;
  prev:=@self;
  aggr^.prv:=@self;
 end else begin
  next:=aggr^.prv^.next;
  prev:=aggr^.prv;
  prev^.next:=@self;
  next^.prev:=@self;
 end;
 wcur:=weight;
end;

procedure tPrv.Stop;
 begin
 Assert(isOpen);
 Assert(Active);
 if prev<>@self then begin
  prev^.next:=next;
  next^.prev:=prev;
 end else next:=nil;
 if aggr^.prv=@self then aggr^.prv:=next;
 active:=false;
 Shedule(20000,@IdleTimeout);
end;

procedure tPrv.Cont;
 var s:tMemoryStream;
 var sz:LongWord;
 var rs:LongWord;
 var buf:array [1..4096] of byte;
 begin
 Assert(Active and isOpen);
 sz:=SegLen;
 if SegLen>high(buf) then sz:=high(buf) else sz:=SegLen;
 sz:=aggr^.tcs.MaxSize(sz);
 //s.Init(GetMem(sz),0,sz);
 s.Init(@buf,0,sz);
 aggr^.tcs.WriteHeaders(s);
 Assert(s.WrBufLen=sz); //really?
 BlockRead(datafile,s.WrBuf^,s.WrBufLen,rs);
 s.WrEnd(rs);
 Assert(RS=s.WrBufLen);//todo
 aggr^.tcs.Send(s);
 //FreeMem(s.base,s.size);
 SegLen:=SegLen-sz;
 dec(wcur);
 if wcur=0 then begin
  wcur:=weight;
  aggr^.prv:=next;
 end;
end;

procedure tPrv.DoClose;
 begin
 if Active then Stop;
 if isOpen then Close(datafile);
 isOpen:=false;
 UnShedule(@IdleTimeout);
 ch^.Ack;
 ch^.Close;
 aggr^.UnRef;
 FreeMem(@self,sizeof(self));
end;

procedure tPrv.OnMsg(msg:tSMsg; data:boolean);
 var op:byte;
 var hash:tfid;
 var base:LongWord;
 var limit:LongWord;
 var err:tmemorystream;
 label malformed;
 begin
 if not data then exit; //todo
 Assert(not(aggr^.rekt and active));
 if aggr^.rekt then exit;
 if msg.stream.RdBufLen<1 then goto malformed;
 op:=msg.stream.ReadByte;
 case op of
  upClose: DoClose;
  upGET: begin
         if msg.stream.RdBufLen<>30 then goto malformed;
         msg.stream.Read(hash,20);
         if msg.stream.ReadWord(2)>0 then goto malformed;
         base:=msg.stream.ReadWord(4);
         limit:=msg.stream.ReadWord(4);
         DoGet(hash,base,limit);
         end;
{  upSEG: begin
         if msg.stream.RdBufLen<10 then goto malformed;
         if msg.stream.ReadWord(2)>0 then goto malformed;
         base:=msg.stream.ReadWord(4);
         limit:=msg.stream.ReadWord(4);
         DoSEG(base, limit);
         end;
}
  else goto malformed;
 end;
 exit; malformed:
 ch^.StreamInit(err,2);
 err.WriteByte(upFAIL);
 err.WriteByte(upErrMalformed);
 ch^.Send(err);
end;

procedure tPrv.ChatTimeout(willwait:LongWord);
 var wasactive:boolean;
 begin
 if WillWait<30000 then exit;
 wasactive:=active;
 if Active then Stop;
 if isOpen then Close(datafile);
 isOpen:=false;
 ch^.Close;
 ch:=nil;
 if wasactive then IdleTimeout {else it is sheduled};
end;
procedure tPrv.IdleTimeout;
 var err:tMemoryStream;
 begin
 if assigned(ch) then begin {chat is still not rekt}
  ch^.StreamInit(err,1);
  err.WriteByte(upClose);
  ch^.Send(err);
  ch^.Close;
 end;
 if Active then Stop;
 if isOpen then Close(datafile);
 aggr^.UnRef;
 FreeMem(@self,sizeof(self));
end;

procedure tPrv.Init(ag:tAggr_ptr; var nchat:tChat; msg: tSMsg);
 begin
 ch:=@nchat;
 ch^.Callback:=@OnMsg;
 ch^.TMHook:=@ChatTimeout;
 aggr:=ag;
 next:=nil;
 prev:=nil;
 chan:=msg.stream.readbyte;
 weight:=100;
 wcur:=0;
 isOpen:=false; Active:=false;
 inc(aggr^.Cnt);
 Shedule(15000,@IdleTimeout);
 OnMsg(msg,true);
end;

procedure tAggr.Init(const source:tNetAddr);
 begin
 writeln('upmgr: init');
 next:=Peers;
 prev:=nil;
 rekt:=false;
 if assigned(Peers) then Peers^.prev:=@self;
 Peers:=@self;
 tcs.Init(source);
 tcs.CanSend:=@Cont;
 tcs.maxTimeout:=8;
 tcs.OnTimeout:=@TCTimeout;
 prv:=nil;
 cnt:=0;
end;

procedure tAggr.TCTimeout;
 var pprv:pointer;
 begin
 writeln('TCTimeout');
 while assigned(prv) do begin
  pprv:=prv;
  prv^.IdleTimeout;
  Assert(pprv<>prv);
 end;
 Done;
end;
procedure tAggr.Cont;
 begin
 assert(assigned(prv));
 prv^.Cont;
end;
procedure tAggr.UnRef;
 begin
 Assert(cnt>0);
 Dec(Cnt);
 if cnt=0 then begin
  Done;
  FreeMem(@self,sizeof(self));
 end;
end;
procedure tAggr.Done;
 begin
 if rekt then exit;
 writeln('upmgr: close');
 rekt:=true;
 tcs.Done;
 if assigned(prev) then prev^.next:=next else Peers:=next;
 if assigned(next) then next^.prev:=prev;
end;

function FindAggr({const} addr:tNetAddr): tAggr_ptr;
 begin
 result:=Peers;
 while assigned(result) do begin
  if result^.tcs.remote=addr then exit;
  assert(result^.prev=result);
  result:=result^.next;
 end;
end;

procedure ChatHandler(var nchat:tChat; msg:tSMsg);
 var ag:^tAggr;
 var pr:^tPrv;
 var s:tMemoryStream;
 const cMax=16;
 begin
 if msg.stream.RdBufLen<2 then begin
  writeln('upmgr: malformed init');
  nchat.StreamInit(s,16);
  s.WriteByte(upFAIL);
  s.writebyte(upErrMalformed);
  nchat.Send(s);
  nchat.Close;
 exit end;
 {first get the ag}
 ag:=FindAggr(msg.source^);
 if assigned(ag) then begin
 {check}
 if ag^.Cnt>=cMax then begin
  nchat.StreamInit(s,16);
  s.WriteByte(upFAIL);
  s.WriteByte(upErrHiChan);
  s.WriteByte(cMax);
  s.WriteByte(ag^.Cnt);
  nchat.Send(s);
  nchat.Close;
 exit end;
 end else begin
  New(ag);
  ag^.init(msg.source^);
 end;
 New(pr);
 pr^.Init(ag,nchat,msg);
end;

BEGIN
 SetChatHandler(opcode.upFileServer,@ChatHandler);
END.