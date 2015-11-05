unit Download;
{manage downloads}

INTERFACE
uses NetAddr,
     ServerLoop,opcode,MemStream
    ,Store1
    ,Chat
    ;
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
 pDownloadJob=^tDownloadJob;
 tDownloadJob=object
  total,done:LongWord;
  missc:LongWord;
  state:(stStop,stActive,stDone,stError);
  error:byte;
  error2:byte;
  fid:tFID;
  procedure Start;
  procedure Abort;
 end;
function GetJob(const fid:tFID):pDownloadJob;
function NewJob(const source:tNetAddr; const fid: tFID):pDownloadJob;

IMPLEMENTATION
type
 tJob=object(tDownloadJob)
  aggr:pointer;
  ix:byte;
  so:tStoreObjectInfo;
  ch:tChat;
  rofs,rlen:LongWord;{b:l of to be requested block}
  procedure Init(const source:tNetAddr; const ifid: tFID);
  procedure MsgDATA(base,length:LongWord; data:pointer);
  procedure Start;
  procedure StartTransfer(preamble:boolean);
  procedure Abort;
  procedure ReplyGET(msg:tSMsg; data:boolean);
  procedure ReplyClose(msg:tSMsg; data:boolean);
 end;
 tAggr_ptr=^tAggr;
 tAggr=object
  Rate:Real;
  ByteCnt:LongWord;
  DgrCnt:LongWord;
  CurMark,PrvMark:byte;
  StartT:tMTime;
  Jobs: array [0..15] of ^tJob;
  refs:byte;
  ChanOfs:byte;
  DgrCntCheck:LongWord;
  remote:tNetAddr;
  next:tAggr_ptr;
  procedure Init(const src:tNetAddr);
  procedure MsgDATA(sz:Word;mark:byte);
  procedure MsgIMME(sz:Word;mark:byte);
  procedure Recv(msg:tSMsg);
  procedure Periodic;
  procedure Done;
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
function GetJob(const fid:tFID):pDownloadJob;
 var a:^tAggr;
 var i:byte;
 var p:^pointer;
 begin
 p:=@AggrChain;
 a:=AggrChain;
 while assigned(a) do begin
  for i:=0 to high(tAggr.Jobs) do begin
   if CompareWord(a^.Jobs[i],fid,10)=0 then begin
    GetJob:=a^.Jobs[i];
    assert(a^.Jobs[i]^.ix=i);
    assert(a^.Jobs[i]^.aggr=a);
    p^:=a^.next;
    a^.next:=AggrChain;
    AggrChain:=a^.next;
    exit;
   end;
  end;
 end;
 GetJob:=nil;
end;

function NewJob(const source:tNetAddr; const fid: tFID):pDownloadJob;
 begin
 result:=GetJob(fid);
 if assigned(result) then exit;
 result:=GetMem(sizeof(tJob));
 tJob(pointer(result)^).init(source,fid);
end;

procedure tJob.Init(const source:tNetAddr; const ifid: tFID);
 var dw:^tAggr;
 begin
 error:=0;
 error2:=0;
 fid:=ifid;
 so.Open(fid);
 done:=0;
 missc:=0;
 if so.final then begin
  done:=total;
  state:=stDone;
  rlen:=0;
 exit end;
 state:=stStop{,stActive,stDone,stError)};
 if so.rc=0 then begin
  writeln('Download: resuming');
  total:=so.Length;
  so.GetMiss(rofs,rlen);
 end else begin
  writeln('Download: start from zero');
  total:=0;
  rofs:=0;
  rlen:=8192;
 end;
 so.EnableWrite(fid);
 if so.rc<>0 then begin
  state:=stError;
  error:=253;
  error2:=so.rc;
 exit end;
 dw:=GetAggr(source);
 if not assigned(aggr) then begin
  new(dw);
  dw^.Init(source);
  ix:=0;
 end else begin
  ix:=0; while (ix<high(tAggr.Jobs)) and (dw^.Jobs[ix]=nil) do inc(ix);
  assert(dw^.Jobs[ix]=nil);{todo}
 end;
 aggr:=dw;
 dw^.Jobs[ix]:=@self;
 ch.Init(source);
 assert( ((state=stStop)and(rlen>0))or((state=stDone)and(rlen=0)) );
end;
procedure tJob.Start;
 begin
 assert( (state=stStop)and(rlen>0) );
 StartTransfer(true);
 //Shedule(20000,@HardTimeout);
 state:={stStop,}stActive{,stDone,stError};
end;
procedure tJob.StartTransfer(preamble:boolean);
 var s:tMemoryStream;
 begin
 assert( rlen>0 );
 ch.Callback:=@ReplyGET;
 ch.streaminit(s,33);
 if preamble then begin
 {service}s.WriteByte(opcode.upFileServer);
 {channel}s.WriteByte(ix);
 {opcode }s.WriteByte(opcode.upGET);
 {file   }s.Write(fid,20);
 end else
 {opcode }s.WriteByte(opcode.upSEG);
 {basehi }s.WriteWord(0,2);
 {base   }s.WriteWord(rofs,4);
 {limit  }s.WriteWord(rlen,4);
 ch.Send(s);
end;
procedure tJob.ReplyGET(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.stream;
 var op:byte;
 var rsize,rseg:LongWord;
 var rfinal:byte;
 begin
 {reply from GET request}
 write('Download: ReplyGET: ');
 if not data then begin
  writeln('ack');
 end else begin
  ch.Ack;
  op:=msg.stream.ReadByte;
  if op=upFAIL then begin
   state:=stError;
   try
   error:=r.ReadByte;
   error2:=r.ReadByte;
   except end;
   writeln('FAIL ',error,'-',error2);
  end
  else if op=upINFO then begin
   {rsizehi}r.skip(2);
   rsize  :=r.ReadWord(4);
   rfinal :=r.readbyte;
   rseg   :=r.readword(4);
   writeln('INFO size=',rsize,' final=',rfinal,' seg=',rseg);
   if (rsize<>so.length) then writeln('Download: length mismatch ',so.length,'->',rsize);
   total:=rsize;
   so.SetFLength(total);
   //UnShedule(@HardTimeout);
  end else if op=opcode.upDONE then begin
   writeln('DONE');
   assert(so.Length>0);
   so.GetMiss(rofs,rlen);
   if rlen=0 then begin
    state:=stDone;
    writeln('Download: completed');
   end else StartTransfer(false);
  end else begin
   if op=upClose then writeln('CLOSE') else writeln('unknown');
   state:=stError;
   error:=254;
   error2:=op;
  end;
 end;
end;

procedure tJob.Abort;
 var s:tMemoryStream;
 begin
 assert(state=stActive);
 ch.Callback:=@ReplyClose;
 //Shedule(20000,@HardTimeout);
 ch.streaminit(s,2);
 {opcode }s.WriteByte(opcode.upClose);
 ch.Send(s);
 state:=stError;
 error:=255;
end;
procedure tJob.ReplyClose(msg:tSMsg; data:boolean);
 begin
 writeln('Download: ReplyClose');
end;

procedure tJob.MsgDATA(base,length:LongWord; data:pointer);
 begin
 so.WriteSeg(base,length,data);
 done:=done+length;
end;

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
 var chan:byte;
 var mark:byte;
 var base:DWORD;
 begin
 op:=msg.stream.readbyte;
 mark:=msg.stream.readbyte;
 if op=opcode.tcdataimm then MsgIMME(msg.length,mark);
 MsgDATA(msg.length,mark);
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

procedure tDownloadJob.Start;
 begin
 tJob(pointer(@self)^).Start;
end;
procedure tDownloadJob.Abort;
 begin
 tJob(pointer(@self)^).Abort;
end;

END.