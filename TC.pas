unit TC;
{TransmissionControll over UDP
some dg larger
if pass set payload to that

useful for file transfer, voip should only consult the current rate
and detect congestion based on latency

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
 remote:tNetAddr;
 Mark:byte;
 MarkStart:tDateTime; {when the mark was started}
 MarkData:LongWord; {how much data sent}
 txLastSize:Word;
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
 //procedure TimeoutCont;
 procedure OnCont(rmark:byte;rrate:real);
 //procedure OnAck(rmark:byte;rsize:word);
 procedure Done; {unregister all callbacks}
 end;

procedure RegTxer(var t:tTCS);
procedure DelTxer(var t:tTCS);

IMPLEMENTATION
uses ServerLoop,SysUtils;

var Txers:array [0..31] of ^tTCS;

procedure RegTxer(var t:tTCS);
 var tn:byte;
 begin
 for tn:=0 to high(TXERS) do if txers[tn]=nil then break;
 assert(not assigned(txers[tn]));
 txers[tn]:=@t;
end;

procedure DelTxer(var t:tTCS);
 var tn:byte;
 begin
 tn:=0; while tn<=high(TXERS) do if txers[tn]=@t then break else inc(tn);
 assert(tn<=high(TXERS));
 t.Done;
 txers[tn]:=nil;
end;

type tTCSp=^tTCS;
function GetTxer(const cource:tNetAddr):tTCSp;
 var tn:byte;
 begin
 result:=nil;
 tn:=0; while tn<=high(TXERS) do if txers[tn]^.remote=cource then break else inc(tn);
 if tn<=high(TXERS) then result:=txers[tn];
end;

procedure RecvCont(msg:ServerLoop.tSMsg);
 var t:^tTCS;
 var rmark:byte;
 var rrate:longword;
 begin
 t:=GetTxer(msg.source^);
 if not assigned(t) then exit;
 msg.stream.skip(1); {skip opcode}
 rmark:=msg.stream.ReadByte;
 rrate:=msg.stream.ReadWord(4);
 t^.OnCont(rmark,rrate);
end;

procedure tTCS.Init;
 begin
 remote.clear;
 //SizeIncScarcity:=20; {inverse probability of size experiment}
 Limit.Rate:=2*1024*1024*1024; {2GB}
 Limit.Size:=4096;
 Limit.RateIF:=1;
 Limit.SizeIF:=2;
 Initial.Rate:=20*1024;
 Initial.Size:={32+5}1024;
 Initial.RateIF:=0.5;
 Initial.SizeIF:=2;
 minRateIF:=0.01;
 CanSend:=nil;
end;

procedure tTCS.Start; {start the transmission}
 begin
 Assert(assigned(CanSend) ); Assert(not remote.isnil);
 Cur:=Initial; 
 mark:=Random(256); MarkData:=0;
 Shedule(80,@TransmitDelay); 
end;

function tTCS.MaxSize(req:word):word;
 begin
 if req>cur.Size then MaxSize:=cur.Size else MaxSize:=req;
end;

procedure tTCS.WriteHeaders(var s:tMemoryStream);
 begin
 {if isTrySize then begin
 end else begin}
  s.WriteByte(4);{opcode}
  s.WriteByte(mark);
 {end;}
end;

procedure tTCS.Send(var s:tMemoryStream);
 begin
 ServerLoop.SendMessage(s.base^,s.length,remote);
 if MarkData=0 then begin
  MarkStart:=Now;
  MarkData:=1;
 end else MarkData:=MarkData+s.length;
 txLastSize:=s.length;
end;

procedure tTCS.OnCont(rmark:byte;rrate:real);
 var rnow:tDateTime;
 var RateFill:single;
 var txRate:real;
 var rxRate:real;
 begin
 if (rmark=Mark) then begin
  rnow:=Now;
  rxRate:=(rrate*64); {B/s}
  txRate:=MarkData/((rnow-MarkStart)*SecsPerDay);
  RateFill:=rxRate/txRate;
  write('speed: ',(rxRate/1024):1:3,'kB/s (',(RateFill*100):3:1,'% of ',txRate/1024:1:3,'), ');
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
 end;
 (*
 if rmark=simark then begin
  isTrySize:=false;
  TrySize:=0;
  if rsize>cur.size then begin
   cur.SizeIF:=((rSize/cur.Size)-1)*2;
   if cur.SizeIF>Limit.SizeIF then Cur.SizeIF:=Limit.SizeIF;
   if (rsize/cur.rate)<=0.3 then cur.size:=rSize; {use new size for all transmit}
   UnShedule(@TimeoutIncreaseSize);
  end;
 end;
 *)
end;

procedure tTCS.TransmitDelay;
 var txwait:real;
 var burst:word;
 begin
 txLastSize:=0;
 txwait:=0;
 burst:=0;
 repeat
  CanSend;
  if txLastSize=0 then exit;{pause}
  //txwait:=txwait+(txLastSize/cur.rate);
  txwait:=(MarkData/cur.Rate)-((Now-MarkStart)*SecsPerDay);
  inc(burst);
 until (txwait>0.02)or(burst>200);
 if txwait<0.02 then txwait:=0.01;
 //writeln(txwait:1:3,burst);
 Shedule(round(txwait*1000),@TransmitDelay);
end;

procedure tTCS.Done; {unregister all callbacks}
 begin
 UnShedule(@TransmitDelay);
end;

BEGIN
 FillByte(txers,sizeof(txers),0); {make'em nil}
 SetMsgHandler(5,@RecvCont);
 //SetMsgHandler(7,@RecvCtrl);
END.
