UNIT UPMGR;
{Upload Manager for brodnetd}

INTERFACE
USES Chat,TC,opcode,ServerLoop,MemStream,NetAddr;

IMPLEMENTATION
USES Store1;

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
 oinfo:tStoreObjectInfo;
 procedure Init(ag:tAggr_ptr; var nchat:tChat; msg: tSMsg);
 procedure OnMsg(msg:tSMsg; data:boolean);
 procedure Cont;
 procedure DoGET(const fid:tfid; base,limit:LongWord);
 procedure DoSEG(base,limit:LongWord);
 procedure DoClose;
 procedure Start;
 procedure Stop;
 procedure ChatTimeout(willwait:LongWord);
 procedure Close;
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
 procedure TCTimeout;
 procedure Done;
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
 begin
 if isOpen then oinfo.Close;
 if Active then Stop; //opt
 ch^.Ack;
 oinfo.Open(fid);
 {if not oinfo.final then begin
  oinfo.rc:=200;
  Close(oinfo.hnd);
 end;}
 if oinfo.rc>0 then begin
  ch^.StreamInit(err,3);
  err.WriteByte(upFAIL);
  if oinfo.rc=1 then err.WriteByte(upErrNotFound)
  else begin err.WriteByte(upErrIO); err.WriteByte(oinfo.rc) end;
  ch^.Send(err);
 end else begin
  isopen:=true;
  DoSeg(base,limit);
 end;
end;

procedure tPrv.DoSEG(base,limit:LongWord);
 var err:tmemorystream;
 begin
 if isOpen then begin
  ch^.StreamInit(err,12);
  oinfo.SegSeek(base);
  if oinfo.rc>0 then begin
   err.WriteByte(upFAIL);
   err.WriteByte(upErrIO);
   err.WriteByte(oinfo.rc);
   ch^.Send(err);
   if Active then Stop;
  end else begin
  err.WriteByte(upINFO);
  err.WriteWord(0,2);
  err.WriteWord(oinfo.length,4);
  seglen:=limit;
  if oinfo.seglen<seglen then seglen:=oinfo.seglen;
  if oinfo.final
  then err.WriteByte(1)
  else err.WriteByte(0);
  err.WriteWord(seglen,4);
  ch^.Send(err);
  if not Active then Start;
 end end else begin
  ch^.StreamInit(err,2);
  err.WriteByte(upFAIL);
  err.WriteByte(upErrSegNoGet);
  ch^.Send(err);
 end;
end;

procedure tPrv.Start;
 begin
 Assert(isOpen);
 Assert(not Active);
 Active:=true;
 UnShedule(@Close);
 writeln('upmgr: Startig transfer');
 if not assigned(aggr^.prv) then begin
  next:=@self;
  prev:=@self;
  aggr^.prv:=@self;
  aggr^.tcs.Start;
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
 Shedule(20000,@Close);
 writeln('upmgr: Stop');
end;

procedure tPrv.Cont;
 var s:tMemoryStream;
 var sz:LongWord;
 var buf:array [1..4096] of byte;
 begin
 //writeln('upmgr: CONT! ',chan);
 Assert(Active and isOpen);
 sz:=aggr^.tcs.MaxSize(sizeof(buf))-5; {1mark+4base}
 if sz>SegLen then sz:=SegLen;
 //s.Init(GetMem(sz),0,sz);
 Assert((sz+5)<=sizeof(buf));
 s.Init(@buf,0,sizeof(buf)); aggr^.tcs.WriteHeaders(s);
 s.WriteByte(Chan);
 s.WriteWord(DWORD(oinfo.Offset),4);
 Assert(sz<=s.WrBufLen);
 oinfo.ReadAhead(sz,s.WrBuf); //todo
 oinfo.WaitRead;
 Assert(oinfo.rc=0); //todo
 s.WrEnd(sz);
 aggr^.tcs.Send(s);
 //FreeMem(s.base,s.size);
 SegLen:=SegLen-sz;
 dec(wcur);
 //FIXME: wait for ack of previous message!
 if SegLen=0 then begin
  ch^.StreamInit(s,2);
  s.WriteByte(upDONE);
  ch^.Send(s);
  Stop;
 end else
 if (wcur=0) then begin
  wcur:=weight;
  aggr^.prv:=next;
 end;
end;

procedure tPrv.DoClose;
 begin
 ch^.Ack;
 Close;
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
 writeln('upmgr: opcode=',op,' sz=',msg.stream.RdBufLen);
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
  upSEG: begin
         if msg.stream.RdBufLen<10 then goto malformed;
         if msg.stream.ReadWord(2)>0 then goto malformed;
         base:=msg.stream.ReadWord(4);
         limit:=msg.stream.ReadWord(4);
         DoSEG(base, limit);
         end;
  else goto malformed;
 end;
 exit; malformed:
 ch^.StreamInit(err,2);
 err.WriteByte(upFAIL);
 err.WriteByte(upErrMalformed);
 ch^.Send(err);
 writeln('upmgr: malformed request stage=1');
end;

{######Timeouts and Shit#######}
procedure tPrv.ChatTimeout(willwait:LongWord);
 begin
 if WillWait<8000 then exit;
 writeln('upmgr: Chat timeout');
 Close;
end;
procedure tPrv.Close;
 var err:tMemoryStream;
 begin
 assert(assigned(ch));
 ch^.StreamInit(err,1);
 err.WriteByte(upClose);
 try ch^.Send(err); except end;
 if Active then Stop;
 if isOpen then oinfo.Close;
 isOpen:=false;
 ch^.Close;
 ch:=nil;
 UnShedule(@Close);
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
 chan:=msg.stream.readbyte; {todo: except}
 writeln('upmgr: prv init ',string(msg.source^),' chan=',chan);
 weight:=100;
 wcur:=0;
 isOpen:=false; Active:=false;
 inc(aggr^.Cnt);
 Shedule(5000,@Close);
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
 tcs.Limit.Rate:=20*1024*1024; {20MB}
 prv:=nil;
 cnt:=0;
end;

procedure tAggr.TCTimeout;
 var pprv:pointer;
 begin
 writeln('upmgr: TCTimeout');
 while assigned(prv) do begin
  assert(not rekt);
  pprv:=prv;
  prv^.Close;
  if rekt then exit;
  Assert(pprv<>prv);
 end;
 Done;
end;
procedure tAggr.Cont;
 begin
 if not assigned(prv) then exit;
 prv^.Cont;
end;
procedure tAggr.UnRef;
 begin
 Assert(cnt>0);
 Dec(Cnt);
 writeln('upmgr: aggr unrefd');
 if cnt=0 then begin
  Done;
  FreeMem(@self,sizeof(self));
 end;
end;
procedure tAggr.Done;
 begin
 assert(not rekt);
 writeln('upmgr: aggr close');
 rekt:=true;
 tcs.Done;
 if assigned(prev) then prev^.next:=next else Peers:=next;
 if assigned(next) then next^.prev:=prev;
end;

function FindAggr({const} addr:tNetAddr): tAggr_ptr;
 begin
 result:=Peers;
 while assigned(result) do begin
  if assigned(result^.next) then assert(result^.next^.prev=result);
  if result^.tcs.remote=addr then exit;
  result:=result^.next;
 end;
end;

procedure ChatHandler(var nchat:tChat; msg:tSMsg);
 var ag:^tAggr;
 var pr:^tPrv;
 var s:tMemoryStream;
 const cMax=16;
 begin
 writeln('upmgr: ChatHandler');
 msg.stream.skip({the initcode}1);
 if msg.stream.RdBufLen<2 then begin
  writeln('upmgr: malformed init');
  nchat.StreamInit(s,16);
  s.WriteByte(upFAIL);
  s.writebyte(upErrMalformed);
  nchat.Send(s);
  nchat.Close;
  writeln('upmgr: malformed request stage=0');
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