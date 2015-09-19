unit TC;
{TransmissionControll over UDP
some dg larger
if pass set payload to that

useful for file transfer, voip should only consult the current rate
and detect congestion based on latency

Used by UploadManager. 1 TC per peer.

Suspend: return from CanSend without sending :)
Resume: call start

opcodes:
 data=4
  mark:1;payload:XX
 data-no-report=8
 data-imm-ack=6
 cont=5
  mark:1;rate:Word4(shr 6)
 ack=7
  mark:1;len:Word2
}
INTERFACE
uses MemStream,NetAddr,ServerLoop;

type tTCSSe=record
 Rate:Real; {sending rate}
 Size:word; {datagram size}
 RateIF:single; {rate increase fraction}
 SizeIF:single; {size increase fraction}
 {new:=old+old*IF}
 end;

type tTCS=object {this is sender part}
 {in order methods should be set/called}
 procedure Init(const iremote:tNetAddr); {set defaults for vars}
 public
 remote:tNetAddr;
 Mark:byte;
 MarkStart:tDateTime; {when the mark was started}
 MarkData:LongWord; {how much data sent}
 txLastSize:Word; {is zero if suspend}
 siMark:byte;
 siNow,siWait:boolean;
 isTimeout:word;
 Cur:tTCSSe; {current values}
 Limit:tTCSSe; {maximum alloved}
 Initial:tTCSSe; {after start/timeout}
 minRateIF:single; {used after rate decrease}
 CanSend: procedure of object;   {called when transmit possible}
 procedure Start; {start the transmission}
 function MaxSize(req:word):word;
 procedure WriteHeaders(var s:tMemoryStream); {add headers before the data}
 procedure Send(var s:tMemoryStream);
 private
 {timer callbacks}
 procedure TransmitDelay;
 procedure Timeout;
 procedure OnCont(msg:ServerLoop.tSMsg);
 procedure OnAck(msg:ServerLoop.tSMsg);
 procedure Done; {unregister all callbacks}
 end;

IMPLEMENTATION
uses SysUtils;

procedure tTCS.Init(const iremote:tNetAddr);
 begin
 remote:=iremote;
 SetMsgHandler(5,remote,@OnCont);
 SetMsgHandler(7,remote,@OnAck);
 Limit.Rate:=2*1024*1024*1024; {2GB}
 Limit.Size:=4096;
 Limit.RateIF:=1;
 Limit.SizeIF:=2;
 Initial.Rate:={20*}1024;
 Initial.Size:=32+5;
 Initial.RateIF:=0.5;
 Initial.SizeIF:=2;
 minRateIF:=0.01;
 CanSend:=nil;
 Cur:=Initial; 
 txLastSize:=0;
end;

procedure tTCS.Start; {start the transmission}
 begin
 Assert(assigned(CanSend) ); Assert(not remote.isnil);
 assert(txLastSize=0);
 mark:=Random(256); MarkData:=0;
 siMark:=0;
 isTimeout:=0;
 Shedule(80,@TransmitDelay); 
 Shedule(3000,@Timeout); 
end;

function tTCS.MaxSize(req:word):word;
 begin
 req:=req-2;{headers}
 if siNow
 then result:=round(cur.Size*(1+cur.SizeIF))
 else result:=cur.Size;
 dec(result,2);
 if result>req then result:=req;
end;

procedure tTCS.WriteHeaders(var s:tMemoryStream);
 begin
 if siNow then begin
  s.WriteByte(6);{opcode}
  s.WriteByte(siMark);
 end else if isTimeout=0 then begin
  s.WriteByte(4);{opcode}
  s.WriteByte(mark);
 end else begin
  s.WriteByte(6);{opcode}
  s.WriteByte(simark);
 end;
end;

procedure tTCS.Send(var s:tMemoryStream);
 begin
 ServerLoop.SendMessage(s.base^,s.length,remote);
 if MarkData=0 then begin
  MarkStart:=Now;
  MarkData:=1;
 end else MarkData:=MarkData+s.length;
 txLastSize:=s.length;
 siNow:=false;
end;

procedure tTCS.OnCont(msg:ServerLoop.tSMsg);
 var rnow:tDateTime;
 var RateFill:single;
 var txRate:real;
 var rxRate:real;
 var rmark:byte;
 var rrate:longword;
 var opcode:byte;
 begin
 opcode:=msg.stream.ReadByte; {skip opcode}
 rmark:=msg.stream.ReadByte;
 assert(opcode=5);
 rrate:=msg.stream.ReadWord(4);
 if (rmark=Mark) then begin
  rnow:=Now;
  rxRate:=(rrate*64); {B/s}
  txRate:=MarkData/((rnow-MarkStart)*SecsPerDay);
  RateFill:=rxRate/txRate;
  write('speed: ',(rxRate/1024):1:3,'kB/s (',(RateFill*100):3:1,'% of ',txRate/1024:1:3,'), ');
  UnShedule(@Timeout); 
  Shedule(2000,@Timeout); 
  if RateFill<0.85 then begin
   write('limit, ');
   cur.Rate:=rxrate;
   cur.RateIF:=minRateIF;
  end else 
  if (txRate/cur.Rate)<0.7 then begin
    write('3hard, ');
  end else begin
   write('pass, ');
   cur.Rate:=txRate*(cur.RateIF+1);
   if cur.Rate>limit.Rate then cur.Rate:=Limit.Rate
   else cur.RateIF:=cur.RateIF*2;
   if cur.RateIF>limit.RateIF then cur.RateIF:=Limit.RateIF;
  end;
  repeat mark:=Random(256) until (mark<>rMark);
  MarkData:=0;
  writeln('-> ',(Cur.Rate/1024):1:4,'kB/s if=',cur.RateIF:6:4);
  if siWait then begin
   cur.SizeIF:=cur.SizeIF/2;
  end;
  siMark:=0;
end end;
 
procedure tTCS.OnAck(msg:ServerLoop.tSMsg);
 var rmark:byte;
 var rsize:word;
 var opcode:byte;
 begin
 opcode:=msg.stream.ReadByte; {skip opcode}
 rmark:=msg.stream.ReadByte;
 assert(opcode=7);
 rsize:=msg.stream.ReadWord(2);
 if rmark<>simark then exit;
 if isTimeout>0 then begin
   Shedule(80,@TransmitDelay); 
   isTimeout:=0;
 end else
 if rsize>cur.size then begin
   writeln('size inc to ',rsize);
   cur.SizeIF:=((rSize/cur.Size)-1)*2;
   if cur.SizeIF>Limit.SizeIF then Cur.SizeIF:=Limit.SizeIF;
   if (rsize/cur.rate)<=0.3 then cur.size:=rSize; {use new size for all transmit}
 end;
 if rsize>=cur.size then siWait:=false;
end;

procedure tTCS.Timeout;
 begin
 if txLastSize=0 then exit; {suspend}
 cur:=initial;
 mark:=Random(256); MarkData:=0;
 siMark:=0;
 Inc(isTimeout);
 Shedule(80,@TransmitDelay); 
 Shedule(3000,@Timeout);
end;

procedure tTCS.TransmitDelay;
 var txwait:real;
 var burst:word;
 begin
 txLastSize:=0;
 txwait:=0;
 burst:=0;
 if (siMark=0)and(cur.Size<limit.Size){and(random(10)=0)}and(istimeout=0) then begin
  siNow:=true;
  siWait:=true;
  siMark:=random(255)+1;
 end;
 repeat
  CanSend;
  if txLastSize=0 then exit; {pause}
  if (isTimeout>0) then exit; {no burst, no shedule next}
  //txwait:=txwait+(txLastSize/cur.rate);
  txwait:=(MarkData/cur.Rate)-((Now-MarkStart)*SecsPerDay);
  inc(burst);
  siNow:=false;
 until (txwait>0.02)or(burst>200);
 if txwait<0.02 then txwait:=0.01;
 //writeln(txwait:1:3,burst);
 Shedule(round(txwait*1000),@TransmitDelay);
end;

procedure tTCS.Done; {unregister all callbacks}
 begin
 UnShedule(@TransmitDelay);
 UnShedule(@Timeout);
 SetMsgHandler(5,remote,nil);
 SetMsgHandler(7,remote,nil);
end;

BEGIN
END.
