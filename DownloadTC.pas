{Include file}
 tAggr=object
  Rate:Real;
  ByteCnt:LongWord;
  DgrCnt:LongWord;
  CurMark,PrvMark:byte;
  StartT:tMTime;
  Jobs: array [0..15] of ^tJob;
  refs,acnt:byte;
  ChanOfs:byte;
  DgrCntCheck:LongWord;
  remote:tNetAddr;
  next:tAggr_ptr;
  procedure Init(const src:tNetAddr);
  procedure MsgDATA(sz:Word;mark:byte);
  procedure MsgIMME(sz:Word;mark:byte);
  procedure Recv(msg:tSMsg);
  procedure SendRate;
  procedure Periodic;
  procedure Done;
  procedure Start(ix:byte);
  procedure Stop(ix:byte);
 end;
var AggrChain:^tAggr;

function GetAggr(const remote:tNetAddr):tAggr_ptr;
 var a:^tAggr;
 var p:^pointer;
 begin
 p:=@AggrChain;
 a:=AggrChain;
 while assigned(a) do begin
  if a^.remote=remote then begin
   GetAggr:=a;
   p^:=a^.next;
   a^.next:=AggrChain;
   AggrChain:=a^.next;
   exit;
  end;
 end;
 GetAggr:=nil;
end;
procedure tAggr.Init(const src:tNetAddr);
 begin
 acnt:=0;
 Rate:=0;
 ByteCnt:=0;
 DgrCnt:=0;
 CurMark:=0;PrvMark:=0;
 StartT:=mNow;
 refs:=high(Jobs); while refs>0 do begin Jobs[refs]:=nil; dec(refs) end;
 ChanOfs:=Random(255-high(Jobs));
 DgrCntCheck:=0;
 remote:=src;
 SetMsgHandler(opcode.tcdata,remote,@Recv);
 SetMsgHandler(opcode.tcdataimm,remote,@Recv);
end;
 
procedure tAggr.Recv(msg:tSMsg);
 var op:byte;
 var chan:byte;
 var mark:byte;
 var base:DWORD;
 var delta:tMTime;
 begin
 op:=msg.stream.readbyte;
 mark:=msg.stream.readbyte;
 if op=opcode.tcdataimm then MsgIMME(msg.length,mark);
 MsgDATA(msg.length,mark);
 if DgrCnt>=8 then begin
  delta:=(mNow-StartT){*MSecsPerDay};
  if delta>=400
  then SendRate;
 end;
 chan:=msg.stream.readbyte;
 base:=msg.stream.ReadWord(4);
 if (chan<=high(Jobs))and assigned(Jobs[chan]) then Jobs[chan]^.MsgDATA(base,msg.stream.RDBufLen,msg.stream.RDBuf);
end;

procedure tAggr.MsgIMME(sz:Word; mark:byte);
 var r:tMemoryStream;
 var buf:array [1..4] of byte;
 begin
 r.Init(@buf,0,sizeof(buf));
 r.WriteByte(opcode.tceack);
 r.WriteByte(mark);
 r.WriteWord(sz,2);
 SendMessage(r.base^,r.length,remote);
end;

procedure tAggr.MsgDATA(sz:Word; mark:byte);
 begin
 if mark<>PrvMark then begin
  if mark<>CurMark then begin
   PrvMark:=CurMark;
   CurMark:=mark;
   StartT:=mNow;
   ByteCnt:=1;
   DgrCnt:=1;
   //writeln('Mark Reset: ',PrvMark,'->',CurMark);
  end else begin Inc(ByteCnt,sz); Inc(DgrCnt); end;
  inc(DgrCntCheck);
 end;
 //writeln('Download: got ',DgrCnt,'dg,',ByteCnt,'B in ',delta,'ms');
end;

procedure tAggr.SendRate;
 var r:tMemoryStream;
 var rateb: DWord; {BytesPerSecond shr 6 (=64)}
 var buf:array [1..6] of byte;
 var delta:tMTime;
 begin
 delta:=(mNow-StartT){*MSecsPerDay};
 rate:=(ByteCnt/delta)*1000;
 //writeln('Download: rate ',(rate/1024):7:1, 'kB/s');
 rateb:=round((rate)/64);
 StartT:=mNow;
 ByteCnt:=1;
 DgrCnt:=0;
 r.Init(@buf,0,sizeof(buf));
 r.WriteByte(opcode.tccont);
 r.WriteByte(CurMark);
 r.WriteWord(rateb,4);
 SendMessage(r.base^,r.length,remote);
end;

procedure tAggr.Periodic;
 begin
 if DgrCntCheck>1 then begin
  DgrCntCheck:=0;
  Shedule(5000,@Periodic);
 exit end;
 writeln('Download: Periodic check failed, unimplemented!');
 //todo do
end;

procedure tAggr.Done;
 var a:^tAggr;
 var p:^pointer;
 begin
 p:=@AggrChain;
 a:=AggrChain;
 while assigned(a) do begin
  if a=@self then begin
   p^:=next;
  break end;
 end;
 UnShedule(@Periodic);
 SetMsgHandler(opcode.tcdata,remote,nil);
 SetMsgHandler(opcode.tcdataimm,remote,nil);
 FreeMem(@self,sizeof(self));
end;

procedure tAggr.Start(ix:byte);
 begin
 if Jobs[ix]^.active then exit;
 if acnt=0 then Shedule(5000,@Periodic);
 inc(acnt);
 Jobs[ix]^.active:=true;
end;

procedure tAggr.Stop(ix:byte);
 begin
 if not Jobs[ix]^.active then exit;
 dec(acnt);
 Jobs[ix]^.active:=false;
 if acnt=0 then begin
  UnShedule(@Periodic);
  SendRate;
 end;
end;
