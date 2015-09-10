unit TC;
{TransmissionControll over UDP
ack all packets
send at fixed rate
increase rate
decrease rate if ack rate does not increase

some dg larger
if pass set payload to that

trid: diff on each side, send with the other side id
pktypes:
 data
  trid:2;sec:2;payload:XX
 cont (send next)
  trid:2;sec:2;len:WordNetOrder
 ctrl (feedback to sender)
  trid; arbitrary data
sec: mark:1 unused:1
}
INTERFACE
uses MemStream,NetAddr;

type tTCSSe=record
 Rate:Real; {sending rate}
 Size:word; {datagram size}
 RateIF:single; {rate increase fraction}
 SizeIF:single; {size increase fraction}
 {new:=old+old*IF}
 end;

type tTCS=object {this is sender part}
 {in order methods should be set/called}
 procedure Init; {set defaults for vars}
 public
 rid,lid:Word;
 remote:tNetAddr;
 mark,markc:byte;
 isTimeout:boolean;
 isCanSend:boolean;
 paused:boolean; {if this is true, do not call start, call send directly (size in cur.size)}
 rxRecTime:tDateTime; {last recent}
 rxRecSize:word;
 rxCumTime:tTime; {cumulative for current mark}
 rxCumSize:longword;
 txLastSize:word; {last recent}
 txLastTime:tDateTime; {last recent}
 txCumTime:tTime; {cumulative for current mark}
 txCumSize:longword;
 trySize:Word; {experimental size}
 isTrySize:boolean;
 SizeIncScarcity:Word; {inverse probability of size experiment}
 siMark:byte; {size increase mark}
 Cur:tTCSSe; {current values}
 Limit:tTCSSe; {maximum alloved}
 Initial:tTCSSe; {after start/timeout}
 minRateIF:single; {used after rate decrease}
 minSizeIF:single; {used after rate increase}
 {statistic todo}
 OnCtrl: procedure(var s:tMemoryStream) of object;
 OnGTimeout: procedure of object;
 CanSend: procedure(msize:word) of object;   {called when transmit possible}
 procedure Start; {start the transmission}
 procedure WriteHeaders(var s:tMemoryStream); {add headers before the data, return payload size}
 {add ur own data to stream, but max plsize bytes}
 procedure Send(var s:tMemoryStream);
 private
 {timer callbacks}
 procedure TransmitDelay; {delay sending packets immediatlely}
 procedure AdjustSpeed;
 procedure TimeoutIncreaseSize;
 procedure OnCont(rmark:byte;rsize:word); {cont packet recved}
 procedure Done; {unregister all callbacks}
 end;

procedure RegTxer(var t:tTCS);
procedure DelTxer(var t:tTCS);

IMPLEMENTATION
uses ServerLoop,SysUtils;

{register cont and ctrl opcodes and deliver them to senders}
var Txers:array [0..31] of ^tTCS;

procedure RegTxer(var t:tTCS);
 var tn:byte;
 begin
 //for tn:=0 to high(TXERS) do if txers[tn]=nil then break;
 tn:=t.lid and high(txers);
 assert(not assigned(txers[tn]));
 txers[tn]:=@t;
 t.lid:=(t.lid and (not high(txers))) or tn; {mask and set}
end;

procedure DelTxer(var t:tTCS);
 var tn:byte;
 begin
 t.Done;
 tn:=t.lid and high(txers);
 txers[tn]:=nil;
end;

type tTCSp=^tTCS;
function GetTxer(lid:word):tTCSp;
 var tn:byte;
 begin
 tn:=lid and high(txers);
 result:=txers[tn];
 if assigned(result) and (result^.lid<>lid) then result:=nil; {drop mismatched}
 {todo: check sender address match}
end;

procedure RecvCtrl(msg:ServerLoop.tSMsg);
 var t:^tTCS;
 var lid:word;
 begin
 msg.stream.skip(1); {skip opcode}
 msg.stream.Read(lid,2); {dont reorder bytes, lid is no number}
 t:=GetTxer(lid);
 if not assigned(t) then exit;
 t^.OnCtrl(msg.stream);
end;
 
procedure RecvCont(msg:ServerLoop.tSMsg);
 var t:^tTCS;
 var lid:word;
 var rmark:byte;
 var rsize:word;
 begin
 msg.stream.skip(1); {skip opcode}
 msg.stream.Read(lid,2); {dont reorder bytes, lid is no number}
 rmark:=msg.stream.ReadByte;
 msg.stream.Skip(1); {skip unused sec}
 rsize:=msg.stream.ReadWord(2);
 t:=GetTxer(lid);
 if not assigned(t) then exit;
 t^.OnCont(rmark,rsize);
end;

procedure tTCS.Init;
 begin
 lid:=Random(65535);
 rid:=65535;
 remote.clear;
 SizeIncScarcity:=20; {inverse probability of size experiment}
 Limit.Rate:=2*1024*1024*1024; {2GB}
 isTimeout:=false;
 Limit.Size:=4096;
 Limit.RateIF:=4;
 Limit.SizeIF:=3;
 Initial.Rate:=256;
 Initial.Size:=32+5;
 Initial.RateIF:=10;
 Initial.SizeIF:=2;
 minRateIF:=0.01;
 minSizeIF:=0.05;
 paused:=false;
 {statistic todo}
 OnCtrl:=nil;
 OnGTimeout:=nil;
 CanSend:=nil;
end;
procedure tTCS.Start; {start the transmission}
 begin
 assert(rid<655350);
 Assert(assigned(CanSend) );
 Assert(not remote.isnil);
 Cur:=Initial;
 markc:=0;
 mark:=Random(256);
 isTrySize:=false;
 isCanSend:=false;
 txLastSize:=0;
 paused:=false;
 Shedule(80,@TransmitDelay);
 Shedule(2000,@AdjustSpeed);
end;

procedure tTCS.WriteHeaders(var s:tMemoryStream);
 begin
 s.WriteByte(6);
 s.Write(rid,2);
 if isTrySize then begin
  s.writebyte(siMark);
 end else begin
  s.WriteByte(mark);
 end;
 s.WriteByte(0);
end;

procedure tTCS.Send(var s:tMemoryStream);
 begin
 if isTrySize then assert(s.length<=trySize) else assert(s.Length<=cur.size);
 isTrySize:=false;
 paused:=false;
 isCanSend:=false;
 ServerLoop.SendMessage(s.base^,s.length,remote);
 if txLastSize=0 then begin
  txCumTime:=0;
  txCumSize:=0;
 end else begin
  txCumTime:=txCumTime+((Now-txLastTime)*SecsPerDay);
  txCumSize:=txCumSize+txLastSize;
 end;
 txLastTime:=Now;
 txLastSize:=s.length;
end;

procedure tTCS.OnCont(rmark:byte;rsize:word);
 var rnow:tDateTime;
 var delta:real;
 begin
 if (rmark=mark)or((trySize>0)and(rmark=simark)) then begin
  rnow:=Now;
  inc(markc);
  if markc=1 then begin
   rxCumTime:=0;
   rxCumSize:=0; {ignore this size since no info how long it sending}
   if isTimeout then begin
    isTimeout:=false;
    Shedule(80,@TransmitDelay);
    writeln('TIMEOUT RECOVERY');
   end;
  end else begin
   delta:=(rnow-rxRecTime)*SecsPerDay;
   rxCumTime:=rxCumTime+delta;
   rxCumSize:=rxCumSize+rsize;
   //writeln('told size is ',rsize, 'delta ',round(delta*1000));
  end;
  rxRecTime:=rnow;
  rxRecSize:=rsize;
  if (markc>200)or(rxCumSize>640000) then begin
   UnShedule(@AdjustSpeed); {do not wait}
   AdjustSpeed; {adjust now!}
  end;
 end;
 if rmark=simark then begin
  isTrySize:=false;
  TrySize:=0;
  if rsize>cur.size then begin
   cur.SizeIF:=((rSize/cur.Size)-1)*2;
   if cur.SizeIF>Limit.SizeIF then Cur.SizeIF:=Limit.SizeIF;
   if (rsize/cur.rate)<=0.3 then cur.size:=rSize; {use new size for all transmit}
   //writeln('New size ',cur.Size);
   UnShedule(@TimeoutIncreaseSize);
  end;
 end;
end;

procedure tTCS.AdjustSpeed;
 var rxRate:real;
 var RateFill:single;
 var txRate:real;
 begin
 if isCanSend then begin paused:=true; exit end; {nothing to transmit, sleep forever}
 if isTimeout then begin Start; exit end;
 if markc>3 then begin {only proceed with enough data}
  rxrate:=rxCumSize/rxCumTime;
  if txCumTime>0.01 then txrate:=txCumSize/txCumTime
  else txrate:=cur.Rate;
  RateFill:=rxrate/txRate;
  write('speed: ',(rxrate/1024):1:4,'kB/s @',(txRate/1024):1:4,'kB/s (',(RateFill*100):3:1,'%), ');
  if RateFill<0.85 then begin
   write('limit, ');
   {we hit the limit}
   cur.Rate:=rxrate;
   cur.RateIF:=minRateIF;
   {cur.Size:=round(cur.size-(cur.Size/4));}
   //cur.SizeIF:=minSizeIF;
  end else begin
   write('pass, ');
   {rates are ok}
   cur.Rate:=txrate+(cur.Rate*cur.RateIF);
   if cur.Rate>limit.Rate then cur.Rate:=Limit.Rate;
   cur.RateIF:=cur.RateIF*2;
   if cur.RateIF>limit.RateIF then cur.RateIF:=Limit.RateIF;
   repeat mark:=Random(256) until mark<>siMark;
  end;
 end else begin
  {this is timeout! reset to safe rates}
  write('timeout, ');
  Cur:=Initial;
  isTimeout:=true;
 end;
 //writeln('txwait ',((cur.size/cur.rate)*1000):1:1);
 markc:=0;
 txLastSize:=0;
 writeln('adjust to ',(Cur.Rate/1024):1:4,'kB/s mark', mark, ' size=',cur.Size);
 (*txLastSize:=0;*)
 Shedule(1600,@AdjustSpeed);
end;

procedure tTCS.TransmitDelay;
 var txwait:real;
 var burst:byte;
 begin
 isCanSend:=true;
 if (not isTimeout)and(TrySize=0)and(Random(SizeIncScarcity)=0)and(cur.Size<Limit.Size) then begin
  repeat siMark:=Random(256) until siMark<>Mark;
  isTrySize:=true;
  trySize:=round(cur.Size+(cur.Size*cur.SizeIF));
  if trySize>Limit.Size then trySize:=Limit.Size;
  //writeln('Try size ',trySize);
  CanSend(trySize-5);
  txwait:=((txLastSize/cur.rate)*1000);
  Shedule(round(txwait),@TransmitDelay);
  if not isCanSend then Shedule(2500,@TimeoutIncreaseSize)
 end else begin
 txwait:=0;
 burst:=0;
 repeat
  CanSend(Cur.Size-5);
  txwait:=txwait+((txLastSize/cur.rate)*1000);
  if isTrySize then break;
  if isTimeout then exit;
  inc(burst);
 until (txwait>20)or(burst>200);
 //writeln('Burst ',burst);
 Shedule(round(txwait),@TransmitDelay);
 end;
end;

procedure tTCS.TimeoutIncreaseSize;
 begin
 isTrySize:=false;
 //writeln('Size Inc timeout');
 cur.SizeIF:=cur.SizeIF/8;
 {make sure we increase at least by 2 bytes}
 if (cur.SizeIF*cur.Size)<1 then cur.SizeIF:=1/cur.Size;
 trySize:=0;
end;

procedure tTCS.Done; {unregister all callbacks}
 begin
 UnShedule(@AdjustSpeed);
 UnShedule(@TransmitDelay);
 UnShedule(@TimeoutIncreaseSize);
end;

BEGIN
 FillByte(txers,sizeof(txers),0); {make'em nil}
 SetMsgHandler(4,@RecvCont);
 SetMsgHandler(5,@RecvCtrl);
END.
