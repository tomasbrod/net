UNIT ObjTrans;
{
  Object Transfer Client
}
INTERFACE
uses ObjectModel,Store2,ServerLoop;

type
tAggr_ptr=^tAggr;
tJob=object
  public {writable}
  Weight:0..127;
  Callback:procedure of object;
  public {read-only}
  Received:LongWord;
  Total:LongWord;
  error:word;{0progress,1done,2ioerror,3timeout,4full,OT-DLEs}
  dataf:tFileStream; {do not touch while active}
  FID:tFID;
  procedure Init(const srce:tNetAddr; const iFID:tFID);
  procedure Start;
  procedure Done;
  procedure MakeRequest(diag:byte);
  private
  aggr:tAggr_ptr;
  ch:byte;
  FirstSeg:pointer;
  MaxReqCnt:word;
  RetryCounter:word;
  DgrCntSinceReq:LongWord;
  procedure OnData(msg:tMemoryStream);
  procedure OnTimeout; {nothing is flowing thru aggr or afterrequest check}
  end;

tAggr=object
  Remote:tNetAddr;
  private
  channel: array [1..16] of ^tJob;
  refc,ncpt,idletick: byte;
  next:tAggr_ptr;
  ByteCnt:LongWord;
  DgrCnt:LongWord;
  DgrCntCheck:LongWord;
  StartT:tMTime;
  ReadyForData:boolean;
  procedure Init(const src:tNetAddr);
  procedure Add(var job: tJob);
  procedure Del(var job: tJob);
  procedure OnData(msg:tSMsg);
  procedure Periodic;
  end;

IMPLEMENTATION
uses opcode;

var AggrChain:^tAggr;

type
tSeg=object
  first,after:LongWord;
  end;
tSegItem=object(tSeg)
  next:^tSegItem;
  end;

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

procedure tJob.Init(const srce:tNetAddr; const iFID:tFID);
  begin
  Weight:=32;
  Callback:=nil;
  Received:=0;
  Total:=$FFFFFFFF;
  MaxReqCnt:=99;
  RetryCounter:=0;
  FID:=iFID;
  {$note Open SEG file and read it  ToDo}
  aggr:=GetAggr(srce);
  if not assigned(aggr) then begin
    new(aggr);
    aggr^.ReadyForData:=false;
    aggr^.next:=AggrChain;
    AggrChain:=aggr;
    aggr^.Init(srce);
  end;
  ch:=0;
  aggr^.Add(self);
  if ch=0 then begin
    error:=4;
    Done; exit;
  end;
  error:=0;
end;

procedure tJob.Start;
  begin
  Assert( (Weight+opcode.otReq)<=high(byte));
  Assert(assigned(callback));
  MakeRequest(1);
end;

procedure tJob.Done;
  begin
  if error<>1 then dataf.Done;
  UnShedule(@OnTimeout);
  aggr^.Del(self);
end;

{Strategy for fixing holes without waiting for DONE message:
 * stuff all small segments to LSEG
 * put large one at end
 * when datagrams from the large arrive, Repeat
}

procedure tJob.MakeRequest(diag:byte);
 var s:tMemoryStream;
 var b,l,ReqLen:LongWord;
 var ReqCnt:byte;
 var seg:^tSegItem;
 const ReqLenLim=20000000;
 begin
 write('ObjTrans.',string(aggr^.remote),'#',ch,'.MakeRequest',diag);
  seg:=nil;
  ReqLen:=0;
  ReqCnt:=0;
  s.Init(185); {23+(18*99)}
  s.WriteByte(opcode.otCtrl);
  s.WriteByte(opcode.otReq+Weight);
  s.WriteByte(ch);
  s.Write(FID,20);
  repeat
    if seg=nil then begin
      b:=0;
      seg:=FirstSeg;
      if assigned(seg) and (seg^.first=0) then begin
        b:=seg^.after;
        seg:=seg^.next;
      end;
    end else begin
      b:=seg^.after;
      seg:=seg^.next;
    end;
    if assigned(seg) then l:=seg^.first-b else l:=Total-b;
    if l=0 then break;
    if (ReqLen+l)>ReqLenLim then l:=ReqLenLim-ReqLen;
    write(' ',b,'+',l);
    s.WriteByte(0);
    s.WriteWord4(b);
    s.WriteWord4(l);
    inc(ReqLen,l);
    inc(ReqCnt);
  until (s.WrBufLen<9)or(ReqLen>=ReqLenLim)or(ReqCnt>=MaxReqCnt);
  if ReqLen=0 then begin
    writeln(' done');
    FreeMem(s.base,s.size);
    error:=1; Done;
    if assigned(callback) then Callback;
  end else begin
    writeln(' send ',s.Length);
    SendMessage(s.Base^,s.Length,aggr^.Remote);
    FreeMem(s.base,s.size);
    UnShedule(@OnTimeout);
    Shedule(750,@OnTimeout);
    DgrCntSinceReq:=0;
    //HighestRequestBase:=b;
  end;
end;

procedure tJob.OnData(msg:tMemoryStream);
  var hiOfs:byte;
  var Offset:LongWord;
  var dtlen:word;
  procedure SetSegment(first,after:LongWord);
    var p:^pointer;
    var c,k:^tSegItem;
    begin
    p:=@FirstSeg; c:=p^; k:=nil;
    while assigned(c) do begin
      {merge cur to new}
      if (c^.first<first)and(c^.after>=first) then first:=c^.first;
      if (c^.after>after)and(c^.first<=after) then after:=c^.after;
      {remove cur if fully contained in new}
      if (first<=c^.first)and(after>=c^.after) then begin
        p^:=c^.next;
        if assigned(k) then dispose(c) else k:=c;
        c:=p^; continue;
      end;
      p:=@c^.next; c:=p^;
      if first>=after then exit;
    end;
    {merge completed, insert new segment}
    if not assigned(k) then new(k);
    k^.first:=first;
    k^.after:=after;
    p:=@FirstSeg; c:=p^; while assigned(c) and (c^.first<first)
      do begin p:=@c^.next; c:=p^ end;
    k^.next:=c;
    p^:=k;
  end;
  begin
  hiOfs:=msg.ReadByte;
  if hiOfs<otInfo then begin
    Offset:=msg.ReadWord4;
    dtlen:=msg.RdBufLen;
    dataf.Seek(Offset+cObjHeaderSize);
    dataf.Write(msg.RdBuf^,dtlen);{$note iocheck todo}
    SetSegment(Offset,Offset+dtlen);
    RetryCounter:=0;
    Inc(DgrCntSinceReq);
    Inc(Received,dtLen);
  {otSINC handled in Aggr}
  end else if hiOfs=otInfo then begin
    hiOfs:=msg.ReadByte;
    Offset:=msg.ReadWord4;
    {ttl:=msg.ReadByte;}
    MaxReqCnt:=msg.ReadByte;
    Total:=Offset; {$hint dangerous}
  end else if (hiOfs=otFail) or (hiOfs=otNotFound) then begin
    error:=hiOfs;
    Done;
    if assigned(callback) then Callback;
  end
  else if hiOfs=otEoT then MakeRequest(3);
end;

procedure tJob.OnTimeout;
  {called by aggr or resheduled in MakeRequest}
  begin
  if DgrCntSinceReq=0 then begin
    if RetryCounter>=13 then begin
      error:=3;
      Done;
      if assigned(callback) then Callback;
    end else begin
      MakeRequest(4);
      Inc(RetryCounter);
    end;
  end
  else DgrCntSinceReq:=0; {next time aggr calls its probably serious}
end;

procedure tAggr.OnData(msg:tSMsg);
  var chn,oh:byte;
  var sm:Word;
  var slen:LongWord;
  var rate:single;
  var s:tMemoryStream absolute msg.st;
  var debugmsg:string[127];
  begin
  s.Skip(1);
  chn:=s.ReadByte;
  oh:=s.ReadByte;
  if DgrCnt=0 then StartT:=mNow;
  Inc(ByteCnt,s.Length);
  Inc(DgrCnt); Inc(DgrCntCheck);
  if oh=otRateInfo then begin
    SetLength(debugmsg,s.RdBufLen);
    s.Read(debugmsg[1],s.RdBufLen);
    writeln('ObjTrans.',string(remote),'#',chn,'.ServerDebug: '+debugmsg);
  exit end;
  if chn=0 then exit;
  if (chn>high(channel)) or (not assigned(channel[chn])) then begin
    s.Seek(0); s.Trunc;
    s.WriteByte(otCtrl);
    s.WriteByte(otFin);
    s.WriteByte(chn);
    SendMessage(s.base^,s.length,Remote);
  exit end;
  if oh=otSINC then s.Read(sm,2) else s.seek(s.position-1);
  channel[chn]^.OnData(s);
  if oh=otSINC then begin
    slen:=s.Length;
    s.Seek(0); s.Trunc;
    s.WriteByte(otCtrl); s.WriteByte(otSIACK);
    s.Write(sm,2);
    s.WriteWord2(slen);
    SendMessage(s.base^,s.length,Remote);
  end else if (DgrCnt>=8) and ((mNow-StartT)>=400) then begin
    s.Seek(0); s.Trunc;
    s.WriteByte(otCtrl); s.WriteByte(otSPEED);
    rate:=(ByteCnt/(mNow-StartT))*16;
    s.WriteWord4(round(rate));
    ByteCnt:=1;
    DgrCnt:=0;
    SendMessage(s.base^,s.length,Remote);
  end;
end;


procedure tAggr.Init(const src:tNetAddr);
  var i:integer;
  begin
  Remote:=src;
  refc:=0; ncpt:=0;
  ReadyForData:=true;
  Shedule(915,@Periodic);
  for i:=1 to high(channel) do channel[i]:=nil;
  ByteCnt:=0;
  DgrCnt:=0;
  DgrCntCheck:=0;
  StartT:=0;
end;

procedure tAggr.Add(var job: tJob);
  var i:integer;
  begin
  Assert(job.ch=0);
  i:=ncpt;
  repeat
    if i=high(channel) then i:=1 else i:=i+1;
    if channel[i]=nil then begin
      job.ch:=i; ncpt:=i;
      channel[job.ch]:=@job;
      inc(refc);
      idletick:=0;
      break;
    end;
  until i=ncpt;
end;
procedure tAggr.Del(var job: tJob);
  begin
  Assert(job.ch>0);
  Assert(assigned(channel[job.ch]));
  dec(refc);
  channel[job.ch]:=nil;
  job.ch:=0;
end;

procedure tAggr.Periodic;
  var a:^tAggr;
  var p:^pointer;
  var i:integer (*absolute p*);
  begin
  if refc>0 then begin
    {check DgrCntCheck and issue Timeout}
    if DgrCntCheck=0 then begin
      for i:=1 to high(channel) do if assigned(channel[i])
        then channel[i]^.OnTimeout;
    end else DgrCntCheck:=0;
  end else begin
    {check idle time and delete self}
    if idletick>17 then begin
      p:=@AggrChain;a:=p^; while assigned(a) do begin
        if a=@self then begin p^:=next; break end;
        p:=@a^.next;a:=p^;
      end;
      ReadyForData:=false;
      FreeMem(@self,sizeof(self)); EXIT;
    end else inc(idletick);
  end;
  Shedule(700,@Periodic);
end;

procedure DataHandler(msg:tSMsg);
  var aggr:^tAggr;
  begin
  aggr:=AggrChain;
  while assigned(aggr) do begin
    if (aggr^.ReadyForData)
    and (aggr^.Remote=Msg.Source) then begin
      aggr^.OnData(Msg);
      break;
    end;
  end;
end;

BEGIN
  SetupOpcode(opcode.otData,@DataHandler);
END.

{

type tPeerTableBucket=record
 handler:tObjMessageHandler;
 remote:tNetAddr;
 len:word;
 opcode:byte;
end;
var PT:array [0..255] of ^tPeerTableBucket;
const PT_mod=high(pt)+1;

function FindPT(prefix:pointer; prefixlen:word; const from:tNetAddr ):word;
  var h:longword;
  var i,o:word;
  begin
  h:=IntHash(0,from,from.length); h:=IntHash(h,prefix^,prefixlen);
  i:=(h and $FFFF) xor (h shr 16) mod pt_mod;
  for o:=0 to high(PT) do begin
    result:=(i+o) mod pt_mod;
    if  PT[result]=nil then exit;
    if  (PT[result]^.opcode=byte(prefix^))
    and (PT[result]^.remote=from)
    and ((prefixlen=1)or(CompareByte(PT[result]^.opcode,prefix^,prefixlen)=0))
    then exit;
  end;
  result:=$FFFF;
end;
  
procedure SetMsgHandler(Prefix:pointer; PrefixLen:word; const From:tNetAddr; handler:tObjMessageHandler);
  var i:word;
  begin
  Assert(PrefixLen>0);
  Assert(OpDesc[byte(Prefix^)].match>0);
  i:=FindPT(prefix,prefixlen,from);
  if i=$FFFF then exit;
  if PT[i]<>nil then begin FreeMem(PT[i]); PT[i]:=nil end;
  if handler<>nil then begin
    PT[i]:=GetMem(sizeof(PT[i]^)+PrefixLen-1);
    PT[i]^.handler:=Handler;
    PT[i]^.remote:=From;
    PT[i]^.Len:=PrefixLen;
    Move(Prefix^,PT[i]^.opcode,PrefixLen);
  end;
end;
}
