UNIT Upload;
{Upload Manager for brodnetd}

INTERFACE
USES Chat,opcode,ServerLoop,MemStream,NetAddr,Store1;

IMPLEMENTATION

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
 {procedure Init(ag:tAggr_ptr; var nchat:tChat; msg: tSMsg);
 procedure OnMsg(msg:tSMsg; data:boolean);
 procedure Cont;
 procedure DoGET(const fid:tfid; base,limit:LongWord);
 procedure DoSEG(base,limit:LongWord);
 procedure DoClose;
 procedure Start;
 procedure Stop;
 procedure ChatTimeout(willwait:LongWord);
 procedure Close;}
end;
tAggrThr=object
 thrid:tThreadID;
 crit:tRtlCriticalSection;
 stop:ByteBool;
 remote:tSockAddrL;
 prv1:^tPrv;
 size1,size2:Word;
 mark1,mark2:Byte;
 rate:Single;
 burst:byte;
 MarkData:LongWord;
 MarkStart:tMTime;
end;
tAggr=object
 thr:tAggrThr;
 remote:tNetAddr;
 refc:byte;
 
 acks:Word; {ack counter, for timeouts}
 timeout:word;
 rateIF,sizeIF,
 limRate,limSize:Single;

 next,prev: tAggr_ptr;
 procedure Start(sprv:tPrv_ptr);
 procedure Stop(sprv:tPrv_ptr);
 procedure Free;
 procedure Init(const source:tNetAddr);
 procedure CalcRates(rxRate:Single);
 procedure Periodic;
 procedure OnCont(msg:tSMsg);
 procedure OnAck(msg:tSMsg);
end;

var Peers:^tAggr;

procedure tAggr.Init(const source:tNetAddr);
 begin
 writeln('upmgr: init');
 next:=Peers;
 prev:=nil;
 if assigned(Peers) then Peers^.prev:=@self;
 Peers:=@self;
 refc:=0;
 acks:=0;
 timeout:=0;
 rateIF:=1;
 sizeIF:=1;
 limRate:=20*1024*1024;
 limSize:=4096;
 InitCriticalSection(thr.crit);
 source.ToSocket(thr.remote);
 remote:=source;
 thr.prv1:=nil;
end;

function ThrStart(p:pointer):LongInt;
 begin with tAggrThr(p^) do begin
 { ( read; ) send; wait; repeat}
end end;

procedure tAggr.Start(sprv:tPrv_ptr);
 begin
 if assigned(thr.prv1) then begin
  sprv^.next:=thr.prv1^.next;
  sprv^.prev:=thr.prv1;
  sprv^.prev^.next:=sprv;
  sprv^.next^.prev:=sprv;
 end else begin
  sprv^.next:=sprv;
  sprv^.prev:=sprv;
  thr.prv1:=sprv;
  thr.stop:=false;
  thr.MarkData:=0;
  thr.MarkStart:=mNow;
  CalcRates(20480);
  thr.thrid:=BeginThread(@ThrStart,@thr{,var,stack});
 end;
 sprv^.wcur:=sprv^.weight;
end;

procedure tAggr.CalcRates(rxRate:Single);
 var txRate:Single;
 var RateFill:Single;
 var pMark:byte;
 const limRateIF=3;
 begin
 EnterCriticalSection(thr.crit);
 pMark:=thr.mark1;
 if thr.MarkStart=0
 then begin txRate:=rxRate; thr.Rate:=rxRate end
 else txRate:=thr.MarkData/((mNow-thr.MarkStart)/1000{*SecsPerDay});
 RateFill:=rxRate/txRate;
 write('speed: ',(rxRate/1024):1:3,'kB/s (',(RateFill*100):3:1,'% of ',txRate/1024:1:3,'), ');
 if RateFill<0.85 then begin
  writeln('limit');
  if RateFill<0.5 then thr.size1:=128;
  thr.Rate:=rxRate;
  RateIF:=RateIF/2;
  repeat thr.mark1:=Random(256) until (thr.mark1<>pMark);
  thr.MarkData:=0; {include on-the-wire data if increasing}
 end else
 if (txRate/thr.Rate)>0.7 then begin
  writeln('pass');
  thr.Rate:=1+txRate*(RateIF+1);
  if thr.Rate>limRate then thr.Rate:=limRate
  else RateIF:=RateIF*2;
  if RateIF>limRateIF then RateIF:=LimRateIF;
 end;
 if (thr.Rate/thr.size1)<4 then thr.size1:=thr.Rate/5;
 if thr.size1<120 then thr.size1:=128;
 sizeIF:=sizeIF/2;
 thr.size2:=round(thr.size1*(1+SizeIF));
 thr.mark2:=Random(256);
 LeaveCriticalSection(thr.crit);
end;

procedure tAggr.Periodic;
 begin
 Shedule(2000,@Periodic);
 if acks=0 then begin
  inc(Timeout);
  CalcRates(512);
 end;
 acks:=0;
 Shedule(2000,@Periodic);
end;

procedure tAggr.OnCont(msg:tSMsg);
 var op,rmark:byte;
 var rRate:LongWord;
 begin
 op:=msg.stream.readbyte;
 assert(op=opcode.tccont);
 rmark:=msg.stream.readbyte;
 if rmark=thr.mark1 then begin
  inc(acks);
  timeout:=0;
  rrate:=msg.stream.readword(4);
  CalcRates(rRate*64);
 end;
end;
 
procedure tAggr.OnAck(msg:tSMsg);
 var op,rmark:byte;
 var rSize:LongWord;
 const limSizeIF=1;
 begin
 op:=msg.stream.readbyte;
 assert(op=opcode.tceack);
 rmark:=msg.stream.readbyte;
 rsize:=msg.stream.readword(2);
 if (rmark<>thr.mark2)and(rmark<>thr.mark1) then exit;
 inc(acks);
 Timeout:=0;
 if rsize<thr.size1 then exit;
 SizeIF:=((rSize/thr.Size1)-1)*2;
 if SizeIF>limSizeIF then SizeIF:=limSizeIF;
 EnterCriticalSection(thr.crit);
 thr.size1:=rSize;
 thr.size2:=round(thr.size1*(1+SizeIF));
 thr.mark2:=Random(256);
 LeaveCriticalSection(thr.crit);
end;

procedure tAggr.Stop(sprv:tPrv_ptr);
 begin
end;

procedure tAggr.Free;
 begin
 Assert(refc>0);
 Dec(refc);
 if refc=0 then begin
  writeln('upmgr: aggr close');
  DoneCriticalSection(thr.crit);
  if assigned(prev) then prev^.next:=next else Peers:=next;
  if assigned(next) then next^.prev:=prev;
  FreeMem(@self,sizeof(self));
 end else
 writeln('upmgr: aggr unrefd');
end;

function FindAggr({const} addr:tNetAddr): tAggr_ptr;
 begin
 result:=Peers;
 while assigned(result) do begin
  if assigned(result^.next) then assert(result^.next^.prev=result);
  if result^.remote=addr then exit;
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
 if ag^.refc>=cMax then begin
  nchat.StreamInit(s,16);
  s.WriteByte(upFAIL);
  s.WriteByte(upErrHiChan);
  s.WriteByte(cMax);
  s.WriteByte(ag^.refc);
  nchat.Send(s);
  nchat.Close;
 exit end;
 end else begin
  New(ag);
  ag^.init(msg.source^);
 end;
 New(pr);
 //pr^.Init(ag,nchat,msg);
end;

BEGIN
 SetChatHandler(opcode.upFileServer,@ChatHandler);
END.