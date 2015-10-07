UNIT UPMGR;
{Upload Manager for brodnetd}

{mission:
  - read files
  - add demux info
  - add sequence/offset
  - handle retransmit requests
  - handle all requests
  - deprioritize/cancel uploads
 keep one TC connection per peer (+expire-delete)
 => need 'chat' protocol
}

INTERFACE
USES Chat,TC,opcode,ServerLoop,MemStream,NetAddr;

IMPLEMENTATION

type
tAggr_ptr=^tAggr;
tPrv=object
 u:byte;
end;
tPrv_ptr=^tPrv;
tAggr=object
 idle: boolean;
 tcs: tTCS;
 ch:^tChat;
 prv: array [0..15] of ^tPrv;
 cprv:byte;{current}
 next,prev: tAggr_ptr;
 procedure OnCont;
 procedure OnMsg(msg: tSMsg; data: boolean);
 procedure Init(var nchat:tChat; msg:tSMsg);
 procedure ForceClose;
 procedure Done;
 procedure IdleTimeout; procedure TCTimeout; procedure CHTimeout(willwait:LongWOrd);
 procedure SendTestReply;
 procedure ExpandPrv(last:byte);
end;

var Peers:^tAggr;

{Requests
Close();
GET(channel:byte; filehash:20; baseHi:word2; base:word4; limit:word4);
SEG(channel:byte; baseHi:word2; base:word4; limit:word4);
FIN(channel:byte; avail:byte);
}{Responses
INFO(channel:byte; struct);
FAIL(channel:byte; code:byte);
DONE(channel:byte);
}

procedure tAggr.OnMsg(msg: tSMsg; data: boolean);
 var op:byte;
 begin
 if data then begin
  op:=msg.stream.readbyte;
  case op of
  opcode.upClose: begin
   ch^.Ack;
   Done;
   FreeMem(@self,sizeof(self));
   end;
  opcode.upGET: ReqGET(msg);
  99: SendTestReply;
  end{case};
 end{data};
end;

procedure tAggr.ReqGET(msg:tSMsg);
 var chan:byte;
 var filehash: array [1..20] of byte;
 var basehi:word;
 var base:LongWord;
 var limit:LongWord;
 begin
 if msg.stream.RdBufLen<31 then begin
  SendError(opcode.upErrMalformed); exit end;
 chan:=msg.stream.ReadByte;
 if chan>high(prv) then begin
  SendError(opcode.upErrHiChan,chan); exit end;
 if assigned(prv[chan]) then begin
  SendError(opcode.upErrChanInUse,chan); exit end;
 msg.stream.Read(FileHash,20);
 basehi:=msg.stream.ReadWord(2);
 base:=msg.stream.Read(4);
 limit:=msg.stream.Read(4);
 New(prv[chan]);
 ch^.Ack;
 with prv[chan]^ do begin
  channel:=chan;
  aggr:=@self;
  Init(filehash,basehi,base,limit);
 end;
end;
 
procedure tAggr.OnCont;
 var pprv:byte;
 begin
 pprv:=cprv;
 repeat
 repeat
  if cprv>=length(prv) then cprv:=0 else inc(cprv);
  if cprv=pprv then begin
   idle:=true;
   Shedule(15000,@IdleTimeout);
   exit;
  end;
 until prv[cprv].u>0;
 {}
 until tcs.txLastSize>0;
end;

procedure tAggr.SendTestReply;
 var s:tMemoryStream;
 begin
 writeln('upmgr: test');
 s.Init(GetMem(56),0,56);
 ch^.AddHeaders(s);
 s.WriteByte(98);
 s.WriteByte(42);
 ch^.send(s);
end;

procedure tAggr.Init(var nchat:tChat; msg:tSMsg);
 var i:byte;
 begin
 writeln('upmgr: init');
 next:=Peers;
 prev:=nil;
 if assigned(Peers) then Peers^.prev:=@self;
 Peers:=@self;
 ch:=@nchat;
 tcs.Init(msg.source^);
 tcs.CanSend:=@OnCont;
 tcs.maxTimeout:=8;
 tcs.OnTimeout:=@TCTimeout;
 for i:=0 to high(prv) do prv[i]:=nil;
 cprv:=0;
 ch^.Callback:=@OnMsg;
 ch^.TMHook:=@CHTimeout;
 writeln('upmgr: send ack to init');
 ch^.Ack;
 idle:=true;
 Shedule(15000,@IdleTimeout);
end;

procedure tAggr.IdleTimeout;
 begin if not idle then exit;
 writeln('Idle Timeout');
 ForceClose end;
procedure tAggr.TCTimeout;
 begin
 writeln('TCTimeout');
 ForceClose end;
procedure tAggr.CHTimeout(willwait:LongWOrd);
 begin if willwait<30000 then exit;
 writeln('ChatTimeout');
 ForceClose end;

procedure tAggr.ForceClose;
 var s:tMemoryStream;
 begin
 writeln('upmgr: force close');
 s.Init(GetMem(56),0,56);
 ch^.AddHeaders(s);
 s.WriteByte(opcode.upClose);
 s.WriteByte(22);
 try
  ch^.send(s);
 except end;
 Done; {fixme sheduler}
 FreeMem(@self,sizeof(self));
end;

procedure tAggr.Done;
 begin
 writeln('upmgr: close');
 ch^.Close;
 tcs.Done;
 UnShedule(@IdleTimeout);
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
 var dup:^tAggr;
 begin
 {check dup}
 dup:=FindAggr(msg.source^);
 if assigned(dup) then begin
  Dup^.ForceClose;
  Dup^.Done;
 end else begin
  New(dup);
 end;
 Dup^.Init(nchat,msg);
end;

BEGIN
 SetChatHandler(opcode.upFileServer,@ChatHandler);
END.