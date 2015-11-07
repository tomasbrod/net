UNIT Upload;
{Upload Manager for brodnetd}

INTERFACE
USES Chat,opcode,ServerLoop,MemStream,NetAddr,Store1;

IMPLEMENTATION
uses UploadThread;

type
tAggr_ptr=^tAggr;
tPrv_ptr=^tPrv;
tPrv=object
 chan:byte;
 aggr:tAggr_ptr;
 us:tUploadSegment;
 ch:^tChat;
 isOpen,Active:boolean;
 procedure Init(nchat:tChat);
 procedure OnMSG(msg:tSMsg;data:boolean);
 procedure DoGET(const fid:tfid; base,limit:LongWord);
 procedure DoSEG(base,limit:LongWord);
 procedure Stop;
 procedure Start;
 procedure Close;
 procedure ChatTimeout(willwait:LongWord);
end;
tAggr=object
 thr:tUploadThr;
 remote:tNetAddr;
 refc:byte;
 chan:array[0..11] of ^tPrv;
 acks:Word; {ack counter, for timeouts}
 timeout:word;
 rateIF,sizeIF,
 limRate,limSize:Single;
 next,prev: tAggr_ptr;
 procedure Free(ac:byte);{called by closing prv}
 procedure Done;
 procedure Start(ac:byte);
 procedure Stop(ac:byte);
 procedure Init(const source:tNetAddr);
 procedure CalcRates(rxRate:Single);
 procedure Periodic;
 procedure OnCont(msg:tSMsg);
 procedure OnAck(msg:tSMsg);
end;

var Peers:^tAggr;


procedure tPrv.Init(nchat:tChat);
 begin
 ch:=@nchat;
 ch^.Callback:=@OnMsg;
 ch^.TMHook:=@ChatTimeout;
 us.weight:=100;
 isOpen:=false; Active:=false;
 Shedule(5000,@Close);
end;

procedure tPrv.DoGET(const fid:tfid; base,limit:LongWord);
 var err:tmemorystream;
 begin
 Stop;
 if isOpen then us.oi.Close;
 isOpen:=false;
 ch^.Ack;
 us.oi.Open(fid);
 {if not oinfo.final then begin
  oinfo.rc:=200;
  Close(oinfo.hnd);
 end;}
 if us.oi.rc>0 then begin
  ch^.StreamInit(err,3);
  err.WriteByte(upFAIL);
  if us.oi.rc=1 then err.WriteByte(upErrNotFound)
  else begin err.WriteByte(upErrIO); err.WriteByte(us.oi.rc) end;
  ch^.Send(err);
 end else begin
  isopen:=true;
  DoSEG(base,limit);
 end;
end;

procedure tPrv.DoSEG(base,limit:LongWord);
 var err:tmemorystream;
 begin
 if isOpen then begin
  Stop;//for thread safety
  ch^.StreamInit(err,12);
  us.oi.SegSeek(base);
  if us.oi.rc>0 then begin
   err.WriteByte(upFAIL);
   err.WriteByte(upErrIO);
   err.WriteByte(us.oi.rc);
   ch^.Send(err);
   if Active then Stop;
  end else begin
  err.WriteByte(upINFO);
  err.WriteWord(0,2);
  err.WriteWord(us.oi.length,4);
  us.SegLen:=limit;
  if us.oi.seglen<us.SegLen then us.SegLen:=us.oi.seglen;
  if us.oi.final
  then err.WriteByte(1)
  else err.WriteByte(0);
  err.WriteWord(us.SegLen,4);
  ch^.Send(err);
  Start;
 end end else begin
  ch^.StreamInit(err,2);
  err.WriteByte(upFAIL);
  err.WriteByte(upErrSegNoGet);
  ch^.Send(err);
 end;
end;

procedure tPrv.Stop;
 begin
 if active then begin
  active:=False;
  Shedule(8000,@Close);
  aggr^.Stop(chan);
 end;
end;
procedure tPrv.Start;
 begin
 assert(isOpen);
 if not active then UnShedule(@Close);
 active:=true;
 aggr^.Start(chan);
end;

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
 thr.Init(source);
 remote:=source;
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

procedure SendError(var ch:tChat;e1,e2:byte);
 var s:tMemoryStream;
 begin
 ch.StreamInit(s,3);
 s.WriteByte(upFAIL);
 s.WriteByte(e1);
 s.WriteByte(e2);
 ch.Send(s);
 ch.Close;
end;

procedure ChatHandler(var nchat:tChat; msg:tSMsg);
 var ag:^tAggr;
 var pr:^tPrv;
 var chan:byte;
 begin
 writeln('Upload: ChatHandler');
 msg.stream.skip({the initcode}1);
 if msg.stream.RdBufLen<2 then begin SendError(nchat,upErrMalformed,0); exit end;
 chan:=msg.stream.ReadByte;
 if chan=high(tAggr.chan) then begin Senderror(nchat,upErrHiChan,0); exit end;
 ag:=FindAggr(msg.source^);
 if not assigned(ag) then begin
  New(ag);
  ag^.init(msg.source^);
 end else if assigned(ag^.chan[chan]) then begin SendError(nchat,upErrChanInUse,0); exit end;
 New(pr);
 pr^.aggr:=ag;
 pr^.chan:=chan;
 ag^.chan[chan]:=pr;
 inc(ag^.refc);
 pr^.Init(nchat);
 pr^.OnMSG(msg,true);
end;

procedure tAggr.Free(ac:byte);
 begin
 assert(assigned(chan[ac]));
 chan[ac]:=nil;
 dec(refc);
 if refc=0 then Done;
end;
procedure tAggr.Done;
 begin
 thr.Done;
 UnShedule(@Periodic);
 FreeMem(@Self,sizeof(self));
end;

procedure tAggr.Start(ac:byte);
 begin
 assert(assigned(chan[ac]));
 EnterCriticalSection(thr.crit);
 assert(not assigned(thr.chans[ac]));
 thr.chans[ac]:=@chan[ac]^.us;
 if thr.stop then Shedule(2000,@Periodic);
 thr.Start;
 LeaveCriticalSection(thr.crit);
end;
 
procedure tAggr.Stop(ac:byte);
 begin
 assert(assigned(chan[ac]));
 EnterCriticalSection(thr.crit);
 assert(assigned(thr.chans[ac]));
 thr.chans[ac]:=nil;
 LeaveCriticalSection(thr.crit);
end;

procedure tPrv.ChatTimeout(willwait:LongWord);
 begin
 if WillWait<8000 then exit;
 writeln('Upload: Chat timeout');
 Close;
end;
procedure tPrv.Close;
 var err:tMemoryStream;
 begin
 assert(assigned(ch));
 ch^.StreamInit(err,1);
 err.WriteByte(upClose);
 ch^.Send(err);
 Stop;
 if isOpen then us.oi.Close;
 isOpen:=false;
 ch^.Close;
 ch:=nil;
 UnShedule(@Close);
 Aggr^.Free(chan);
 FreeMem(@self,sizeof(self));
end;

procedure tAggr.Periodic;
 var i:byte;
 begin
 if thr.stop then exit;
 if acks=0 then begin
  inc(Timeout);
  if timeout>=5 then begin
   refc:=255;
   for i:=0 to high(chan) do if assigned(chan[i]) then chan[i]^.Close;
  Done;exit;end;
  CalcRates(512);
 end else timeout:=0;
 acks:=0;
 Shedule(2000,@Periodic);
end;

procedure tPrv.OnMSG(msg:tSMsg;data:boolean);
 var op:byte;
 var hash:tfid;
 var base:LongWord;
 var limit:LongWord;
 var err:tmemorystream;
 label malformed;
 begin
 if not data then exit; //todo
 if msg.stream.RdBufLen<1 then goto malformed;
 op:=msg.stream.ReadByte;
 writeln('Upload: opcode=',op,' sz=',msg.stream.RdBufLen);
 case op of
  upClose: begin
           ch^.Ack;
           Close;
           end;
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
 writeln('Upload: malformed request stage=1');
end;

{$I UploadTC.pas}

BEGIN
 SetChatHandler(opcode.upFileServer,@ChatHandler);
END.