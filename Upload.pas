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
 uc:UploadThread.tChannel;
 ch:^tChat;
 isOpen,Active:boolean;
 procedure Init(var nchat:tChat);
 procedure OnMSG(msg:tSMsg;data:boolean);
 procedure NotifyDOne;
 procedure DoOPEN(const fid:tfid);
 procedure DoLSEG(count:byte; base:array of LongWord; limit:array of LongWord);
 procedure DoWEIGHT(nweight:word);
 procedure Stop;
 procedure Start;
 procedure Close;
 procedure Close(tell:boolean); overload; inline;
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
 procedure ResetMark;
end;

var Peers:^tAggr;
procedure SendError(var ch:tChat;e1,e2:byte); forward;
function FindAggr({const} addr:tNetAddr): tAggr_ptr; forward;

{                  PROTOCOL                  }
{ CLIENT                           SERVER(us)}{
init upFileServer <channel>       -> ACK
upOPEN <id>        -> upINFO <length> <final>
upOPEN <id> <b,l>  -> upINFO <length> <final> <avl-bytes>
                   -> upFAIL <code> <code2> <details>
upLSEG [b,l]       -> upSEGOK <available-bytes>
                   -> upUNAVL <next-avl>
                   -> upFAIL upErrIO
upSTOP             -> ACK
upCLOSE            -> ACK
upWEIGHT <weight>  -> ACK

special server messages:
upEPROTO <code> <details> (protocol violation)
upCLOSE (close by server, usualy timeout)
error conditions:
upErrHiChan (channel number too high or too many connections)
upErrChanInUse
upErrNotFound (file was not found)
upErrIO (other error while opening/reading/seeking)
upEPROTO upErrNotOpen (LSEG without OPEN or afer STOP)
upEPROTO upErrTroll (trolling)
notes:
OPEN message can be merged with init, saving a round-trip
}

procedure tPrv.DoOPEN(const fid:tfid);
 var err:tmemorystream;
 begin
 writeln('Upload: ',string(ch^.remote),'/',chan,' OPEN');
 Stop;
 if isOpen then uc.oi.Close;
 isOpen:=false;
 ch^.Ack;
 uc.oi.Open(fid);
 {if not oinfo.final then begin
  oinfo.rc:=200;
  Close(oinfo.hnd);
 end;}
 if uc.oi.rc>0 then begin
  ch^.StreamInit(err,3);
  err.WriteByte(upFAIL);
  if uc.oi.rc=1 then err.WriteByte(upErrNotFound)
  else begin err.WriteByte(upErrIO); err.WriteByte(uc.oi.rc) end;
  ch^.Send(err);
 end else begin
  isopen:=true;
  ch^.StreamInit(err,10);
  err.WriteByte(upINFO);
  err.WriteWord(uc.oi.length,4);
  if uc.oi.final then err.WriteByte(1) else err.WriteByte(0);
  err.WriteWord(0,4);
  ch^.Send(err);
 end;
end;

procedure tPrv.DoLSEG(count:byte; base,limit: array of LongWord);
 var err:tmemorystream;
 var i:byte;
 var l,fb:LongWord;
 var tbytes:LongWOrd;
 begin
 writeln('Upload: ',string(ch^.remote),'/',chan,' LSEG');
 if not isOpen then begin
  ch^.StreamInit(err,3);
  err.WriteByte(upEPROTO);
  err.WriteByte(upErrNotOpen);
  ch^.send(err);
  writeln('notOpen');
 exit end;
 if count=0 then begin
  ch^.StreamInit(err,3);
  err.WriteByte(upEPROTO);
  err.WriteByte(100);
  ch^.send(err);
  writeln('ZeroCount');
 exit end;
 stop;
 uc.seg:=0;
 tbytes:=0;
 for i:=1 to count do begin
  if limit[i-1]=0 then begin
   ch^.StreamInit(err,3);
   err.WriteByte(upEPROTO);
   err.WriteByte(101);
   ch^.send(err);
   writeln('ZeroLimit');
  exit end;
  l:=uc.oi.SegmentLength(base[i-1]);
  if l>0 then begin
   inc(uc.seg);
   uc.s[uc.seg].base:=base[i-1];
   if l>limit[i-1] then l:=limit[i-1];
   uc.s[uc.seg].len:=l;
   inc(tbytes,l);
  end else if i=1 then begin
   {first failed, try find some seg}
   uc.oi.GetSegAfter(base[0],fb,l);
   ch^.StreamInit(err,5);
   if l=0 then fb:=0;
   err.WriteByte(upUNAVL);
   err.WriteWord(fb,4);
   ch^.Send(err);
   exit;
  end;
 end;
 ch^.StreamInit(err,6);
 err.WriteByte(upSEGOK);
 err.WriteWord(tbytes,4);
 err.WriteByte(uc.seg);
 ch^.Send(err);
 Start;
end;

procedure tPrv.DoWEIGHT(nweight:word);
 begin
 if nweight<50 then nweight:=50;
 uc.Weight:=nweight;
 ch^.Ack;
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
 writeln(chan);
 if chan>high(tAggr.chan) then begin Senderror(nchat,upErrHiChan,chan); exit end;
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
 if msg.stream.RdBufLen>0 {the request may be empty}
 then pr^.OnMSG(msg,true);
end;
procedure tPrv.OnMSG(msg:tSMsg;data:boolean);
 var op:byte;
 var hash:tfid;
 var base:LongWord;
 var limit:LongWord;
 var err:tmemorystream;
 var count:byte;
 var lbas:array [0..23] of LongWOrd;
 var llim:array [0..23] of LongWOrd;
 label malformed;
 begin
 if not data then exit; //todo
 if msg.stream.RdBufLen<1 then goto malformed;
 op:=msg.stream.ReadByte;
 writeln('Upload: ',string(ch^.remote),' opcode=',op,' sz=',msg.stream.RdBufLen);
 case op of
  upClose: begin
         ch^.Ack;
         Close(false);
         end;
  upOPEN: begin
         if msg.stream.RdBufLen<20 then goto malformed;
         msg.stream.Read(hash,20);
         DoOPEN(hash);
         end;
  upLSEG: begin
         count:=0;
         while (msg.stream.RdBufLen>0)and(count<=high(lbas)) do begin
          if msg.stream.RdBufLen<8 then goto malformed;
          lbas[count]:=msg.stream.ReadWord(4);
          llim[count]:=msg.stream.ReadWord(4);
          inc(count);
         end;
         DoLSEG(count,lbas,llim);
         end;
  upWEIGHT: begin
         if msg.stream.RdBufLen<>2 then goto malformed;
         base:=msg.stream.ReadWord(2);
         DoWEIGHT(base);
         end;
  else goto malformed;
 end;
 exit; malformed:
 ch^.StreamInit(err,3);
 err.WriteByte(upEPROTO);
 err.WriteByte(upErrMalformed);
 ch^.Send(err);
 writeln('Upload: malformed request stage=1');
end;

procedure tPrv.Init(var nchat:tChat);
 begin
 ch:=@nchat;
 ch^.Callback:=@OnMsg;
 ch^.TMHook:=@ChatTimeout;
 uc.weight:=100;
 isOpen:=false; Active:=false;
 Shedule(5000,@Close);
 writeln('Upload: prv for ',string(ch^.remote),'/',chan,' init');
end;

procedure tPrv.NotifyDone;
 var err:tmemorystream;
 begin
 Stop;
 ch^.StreamInit(err,2);
 err.WriteByte(upDONE);
 ch^.Send(err);
end;

procedure tPrv.Stop;
 begin
 if active then begin
  active:=False;
  Shedule(20000,@Close);
  aggr^.Stop(chan);
 end;
 writeln('Upload: prv for ',string(ch^.remote),'/',chan,' stop');
end;
procedure tPrv.Start;
 begin
 assert(isOpen);
 if not active then UnShedule(@Close);
 active:=true;
 aggr^.Start(chan);
 writeln('Upload: prv for ',string(ch^.remote),'/',chan,' start');
end;

procedure tPrv.Close(tell:boolean);
 var err:tMemoryStream;
 begin
 assert(assigned(ch));
 writeln('Upload: prv for ',string(ch^.remote),'/',chan,' close');
 if tell then begin
  ch^.StreamInit(err,1);
  err.WriteByte(upClose);
  ch^.Send(err);
 end;
 Stop;
 if isOpen then uc.oi.Close;
 isOpen:=false;
 ch^.Close;
 ch:=nil;
 UnShedule(@Close);
 Aggr^.Free(chan);
 FreeMem(@self,sizeof(self));
end;
procedure tPrv.Close;
 begin
 Close(true);
end;

procedure tPrv.ChatTimeout(willwait:LongWord);
 begin
 if WillWait<8000 then exit;
 writeln('Upload: prv for ',string(ch^.remote),'/',chan,' ChatTimeout');
 Close;
end;

{***AGGREGATOR***}

procedure tAggr.Init(const source:tNetAddr);
 begin
 next:=Peers;
 prev:=nil;
 if assigned(Peers) then Peers^.prev:=@self;
 Peers:=@self;
 refc:=0;
 acks:=0;
 timeout:=0;
 rateIF:=1;
 sizeIF:=1;
 limRate:=2000*1024*1024;
 limSize:=4096;
 remote:=source;
 writeln('Upload: aggr for ',string(remote),' init');
 thr.Init(source);
 CalcRates(2048);
 SetMsgHandler(opcode.tccont,remote,@OnCont);
 SetMsgHandler(opcode.tceack,remote,@OnAck);
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

procedure tAggr.Free(ac:byte);
 begin
 assert(assigned(chan[ac]));
 chan[ac]:=nil;
 dec(refc);
 if refc=0 then Done;
end;
procedure tAggr.Done;
 begin
 write('Upload: aggr for ',string(remote),' done');
 thr.Done; writeln(' thrdone');
 UnShedule(@Periodic);
 if assigned(prev) then prev^.next:=next else Peers:=next;
 if assigned(next) then next^.prev:=prev;
 SetMsgHandler(opcode.tccont,remote,nil);
 SetMsgHandler(opcode.tceack,remote,nil);
 FreeMem(@Self,sizeof(self));
end;

procedure tAggr.Start(ac:byte);
 begin
 writeln('Upload: aggr for ',string(remote),' start chan ',ac);
 assert(assigned(chan[ac]));
 EnterCriticalSection(thr.crit);
 assert(not assigned(thr.chans[ac]));
 thr.chans[ac]:=@chan[ac]^.uc;
 chan[ac]^.uc.wcur:=chan[ac]^.uc.weight;
 UnShedule(@Periodic);
 Shedule(700,@Periodic);
 if thr.stop or thr.wait then ResetMark else {do not reset if running};
 thr.Start; {wake up, or start if not running}
 LeaveCriticalSection(thr.crit);
end;
 
procedure tAggr.Stop(ac:byte);
 begin
 writeln('Upload: aggr for ',string(remote),' stop chan ',ac);
 assert(assigned(chan[ac]));
 EnterCriticalSection(thr.crit);
 assert(assigned(thr.chans[ac]));
 thr.chans[ac]:=nil;
 LeaveCriticalSection(thr.crit);
end;

procedure tAggr.Periodic;
 var i:byte;
 var e:boolean;
 begin
 if (thr.stop)or(thr.wait) then begin
  for i:=0 to high(chan) do if assigned(chan[i]) then with chan[i]^ do begin
   if not active then continue;
   EnterCriticalSection(thr.crit);
   e:=uc.Seg=0;
   LeaveCriticalSection(thr.crit);
   if e then NotifyDone;
  end;
 exit end;
 if acks=0 then begin
  inc(Timeout);
  if timeout>=10 then begin
   refc:=255;
   for i:=0 to high(chan) do if assigned(chan[i]) then chan[i]^.Close;
  Done;exit;end;
  if timeout=4 then CalcRates(512);
 end else timeout:=0;
 acks:=0;
 Shedule(700,@Periodic);
end;

{$I UploadTC.pas}

BEGIN
 Peers:=nil;
 SetChatHandler(opcode.upFileServer,@ChatHandler);
END.