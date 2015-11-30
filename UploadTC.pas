{include file}
procedure tAggr.ResetMark;
 var pMark:byte;
 begin
 pMark:=thr.mark;
 thr.MarkData:=0; thr.MarkTime:=0; {prevent timing screwup in thread}
 repeat thr.mark:=Random(255)+1 until (thr.mark<>pMark);
end;

procedure tAggr.CalcRates(rxRate:Single);
 var txRate:Single;
 var RateFill:Single;
 const limRateIF=1;
 begin
 EnterCriticalSection(thr.crit);
 if thr.MarkTime=0 then thr.MarkTime:=1;
 if thr.MarkData=0
 then begin
  writeln('RESET INITIAL');
  txRate:=rxRate; thr.Rate:=4096;
  ResetMark;
 end else txRate:=thr.MarkData/(thr.MarkTime/1000);
 if rxRate=0 then rxRate:=1;
 if txRate=0 then txRate:=1;
 RateFill:=rxRate/txRate;
 write('speed: ',(rxRate/1024):8:2,'kB/s (',(RateFill*100):3:0,'% of ',txRate/1024:8:2,'), ');
 if RateFill<0.90 then begin
  write('limit');
  if RateFill<0.5 then thr.size1:=round(thr.size1*0.75);
  thr.Rate:=rxRate;
  RateIF:=RateIF*0.1*RateFill;
 end else
 if (txRate/thr.Rate)>0.7 then begin
  write('pass');
  thr.Rate:=1+txRate*(RateIF+1);
  if thr.Rate>limRate then thr.Rate:=limRate
  else RateIF:=RateIF+0.1;
  if RateIF>limRateIF then RateIF:=LimRateIF;
 end else write('3hard');
 ResetMark; {TODO: do not do this}
 if thr.size1<120 then thr.size1:=128;
 {no ack to size inc packet, back up}
 sizeIF:=sizeIF/8;
 {but at least 1 byte increase}
 if (thr.size1*SizeIF)<1 then SizeIF:=1/thr.Size1;
 {freq...}
 if (thr.Size1/thr.Rate)>0.11 then begin thr.size1:=100; thr.rate:=4096; end;
 write(', if=',RateIF:6:4);
 write(', size=',thr.size1:5,'+',SizeIF:6:4);
 {request ack, also triggers MTU discovery}
 thr.size2:=thr.size1;
 thr.size1:=thr.size2-1; {???}
 writeln;
 LeaveCriticalSection(thr.crit);
end;

procedure tAggr.OnCont(msg:tSMsg);
 var op,rmark:byte;
 var rRate:LongWord;
 begin
 op:=msg.stream.readbyte;
 assert(op=opcode.tccont);
 rmark:=msg.stream.readbyte;
 if rmark=thr.mark then begin
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
 if (rmark<>thr.mark) then exit;
 inc(acks);
 Timeout:=0;
 {do nothing if timeout recovery or not increase}
 if (rsize<=thr.size1)or(timeout>0) then exit;
 EnterCriticalSection(thr.crit);
 {try to maintain frequency}
 if (rSize/thr.Rate)<0.11
  then thr.size1:=rSize {use the new size as main size}
  else sizeIF:=0;
 {increase increase fastor}
 SizeIF:=SizeIF*2; if SizeIF>limSizeIF then SizeIF:=limSizeIF;
 assert(thr.size2=0); {wtf?}
 {calc new packet size}
 thr.size2:=round(thr.Size1*(1+SizeIF));
 {do nothing if they are equal}
 if thr.size1=thr.size2 then thr.size2:=0
 {else writeln('Set size2: ',thr.size2,' ',thr.size1)};
 LeaveCriticalSection(thr.crit);
end;