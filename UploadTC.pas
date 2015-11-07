{include file}
procedure tAggr.CalcRates(rxRate:Single);
 var txRate:Single;
 var RateFill:Single;
 var pMark:byte;
 const limRateIF=3;
 begin
 EnterCriticalSection(thr.crit);
 pMark:=thr.mark1;
 if thr.MarkTime=0
 then begin txRate:=rxRate; thr.Rate:=rxRate end
 else txRate:=thr.MarkData/((thr.MarkTime)/1000{*SecsPerDay});
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
 if (thr.Rate/thr.size1)<4 then thr.size1:=round(thr.Rate/5);
 if thr.size1<120 then thr.size1:=128;
 sizeIF:=sizeIF/2;
 thr.size2:=round(thr.size1*(1+SizeIF));
 thr.mark2:=Random(256);
 LeaveCriticalSection(thr.crit);
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