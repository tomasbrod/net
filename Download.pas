unit Download;
{manage downloads}

INTERFACE
uses NetAddr,
     ServerLoop,opcode,MemStream;
{
 Same idea here as upmgr. Have an tAggr for each peer reporting speed and 
 tJob for each file request saving to file and reqesting missed segments. 
 The aggr should have limit of paralele jobs. Jobs will first be linked in 
 tAggr queue and then started as slots become available.
 
 After node restart, notify requester with a No-Source error. But should 
 not be forgotten. More advanced DM could consult CHK or Category the file 
 was found.
}
type
 tJob_ptr=pointer;//^tJob;
 tAggr=object
  Rate:Real;
  ByteCnt:LongWord;
  DgrCnt:LongWord;
  CurMark,PrvMark:byte;
  StartT:tMTime;
  Jobs: array [0..15] of tJob_ptr;
  refs:byte;
  ChanOfs:byte;
  DgrCntCheck:LongWord;
  remote:tNetAddr;
  procedure Init(const src:tNetAddr);
  procedure MsgDATA(sz:Word;mark:byte);
  procedure MsgIMME(sz:Word;mark:byte);
  procedure Recv(msg:tSMsg);
  procedure Periodic;
  procedure Done;
 end;
IMPLEMENTATION

procedure tAggr.Init(const src:tNetAddr);
 begin
 Rate:=0;
 ByteCnt:=0;
 DgrCnt:=0;
 CurMark:=0;PrvMark:=0;
 StartT:=mNow;
 refs:=high(Jobs); while refs>0 do begin Jobs[refs]:=nil; dec(refs) end;
 ChanOfs:=Random(255-high(Jobs));
 DgrCntCheck:=0;
 Shedule(5000,@Periodic);
 remote:=src;
 SetMsgHandler(opcode.tcdata,src,@Recv);
 SetMsgHandler(opcode.tcdataimm,src,@Recv);
end;
 
procedure tAggr.Recv(msg:tSMsg);
 var op:byte;
 var mark:byte;
 var chan:byte;
 begin
 op:=msg.stream.readbyte;
 mark:=msg.stream.readbyte;
 if op=opcode.tcdataimm then MsgIMME(msg.length,mark);
 MsgDATA(msg.length,mark);
 chan:=msg.stream.readbyte;
 //delegate to others todo
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
 var r:tMemoryStream;
 var rateb: DWord; {BytesPerSecond shr 6 (=64)}
 var buf:array [1..6] of byte;
 var delta:tMTime;
 begin
 if mark<>PrvMark then begin
  if mark<>CurMark then begin
   PrvMark:=CurMark;
   CurMark:=mark;
   StartT:=mNow;
   ByteCnt:=1;
   DgrCnt:=1;
  end else begin Inc(ByteCnt,sz); Inc(DgrCnt) end;
  inc(DgrCntCheck);
 end;
 if DgrCnt<8 then exit;
 delta:=(mNow-StartT){*MSecsPerDay};
 if delta<400 then exit;
 rate:=(ByteCnt/delta)*1000;
 writeln('Download: rate ',(rate/1024):7:1, 'kB/s');
 rateb:=round((rate)/64);
 StartT:=mNow;
 ByteCnt:=1;
 r.Init(@buf,0,sizeof(buf));
 r.WriteByte(opcode.tccont);
 r.WriteByte(mark);
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
 begin
 UnShedule(@Periodic);
end;

END.