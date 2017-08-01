UNIT ObjTrans;
{
  Object Transfer Client
}
INTERFACE
uses ObjectModel,Classes,Store,ServerLoop;

type
tAggr_ptr=^tAggr;
tSeg=object
  first,after:LongWord;
  end;
tSegItem=object(tSeg)
  next:^tSegItem;
  end;

tJob=object
  public {writable}
  Weight:0..127;
  Callback:procedure of object;
  public {read-only}
  Received:LongWord;
  Total:LongWord;
  error:integer;{0progress,1done,-2ioerror,-3timeout,-4full,OTinfo}
  dataf:tStream; {do not touch while active}
  FID:tFID;
  aggr:tAggr_ptr;
  procedure Init;
  procedure Start;
  procedure Done;
  procedure MakeRequest(diag:byte);
  private
  ch:byte;
  FirstSeg:^tSegItem;
  RetryCounter:word;
  DgrCntSinceReq:LongWord;
  procedure OnData(msg:tMemoryStream);
  procedure OnInfo(icode:byte; ifilelen:LongWord; ireqlen:LongWord; msg:tMemoryStream);
  procedure OnTimeout; {nothing is flowing thru aggr or afterrequest check}
  end;

tAggr=object
  Remote:tNetAddr;
  refc: byte;
  distance: longint;
  max_channels{highest channel number},
  max_segments{one less then real max}: byte;
  procedure ResetIdle;
  private
  channel: array [1..16] of ^tJob;
  ncpt, idletick: byte;
  MarkValue: byte;
  LargestDgr: Word;
  ByteCnt:LongWord;
  DgrCnt:LongWord;
  DgrCntCheck:LongWord;
  StartT:tMTime;
  ReadyForData:boolean;
  procedure Init;
  procedure Add(var job: tJob);
  procedure Del(var job: tJob);
  procedure OnInfo(msg:tSMsg);
  procedure OnData(msg:tSMsg);
  procedure Periodic;
  end;

function GetAggr(const iRemote: tNetAddr; create: boolean): tAggr_ptr;

IMPLEMENTATION
uses opcode,SysUtils;

var Aggrs: array [0..511] of ^tAggr;

function compareAggr(a: pointer; key: pointer): ShortInt;
  begin
  if assigned(a)
  then result:=CompareByte(tAggr(a^).Remote, tNetAddr(key^), sizeof(tNetAddr))
  else result:=1; {outside of array, go left}
end;

function GetAggr(const iRemote: tNetAddr; create: boolean): tAggr_ptr;
  var i:longword;
  begin
  i:=FindIndex(@Aggrs[0], high(Aggrs)+1, @iremote, @compareAggr);
  if i<=high(Aggrs) then begin
    if assigned(Aggrs[i]) and (Aggrs[i]^.Remote=iRemote)
    then result:=Aggrs[i]
    else if create
    and( (Aggrs[i]=nil) or PtrListShiftRight(@Aggrs[0], High(Aggrs), i) )then begin
        New(Aggrs[i]);
        Aggrs[i]^.Remote:=iRemote;
        Aggrs[i]^.Init;
        result:=Aggrs[i];
    end else result:=nil;
  end else result:=nil;
end;

procedure tJob.Init;
  begin
  Weight:=32;
  Callback:=nil;
  Received:=0;
  Total:=$FFFFFFFF;
  RetryCounter:=0;
  FirstSeg:=nil;
  Aggr:=nil;
  ch:=0;
  error:=1;
end;

procedure tJob.Start;
  begin
  Assert(assigned(callback));
  Assert(assigned(aggr));
  assert(ch=0);
  {set fid and open temporary data file}
  {attach to connection}
  aggr^.Add(self);
  if ch=0 then begin
    error:=-4;
  end else begin
    {make the request}
    error:=0;
    MakeRequest(1);
  end;
end;

procedure tJob.Done;
  var cp,dp:^tSegItem;
  begin
  UnShedule(@OnTimeout);
  if assigned(aggr) then aggr^.Del(self);
  cp:=FirstSeg;
  while assigned(cp) do begin
    dp:=cp;
    cp:=cp^.next;
    Dispose(dp);
  end;
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
 const ReqLenLim=5000000;
 begin
 write('ObjTrans.',string(aggr^.remote),'#',ch,'.MakeRequest',diag);
  seg:=nil;
  ReqLen:=0;
  ReqCnt:=0;
  {$hint derive request size from connection datagram size}
  s:=tMemoryStream.Create;
  s.w1(opcode.otRequest);
  s.w1(ch);
  s.w1(Weight);
  s.w1(0{flags});
  s.Write(FID,24);
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
    s.w1(0);
    s.w4(b);
    s.w4(l);
    inc(ReqLen,l);
    inc(ReqCnt);
  until (ReqCnt>=40)or(ReqLen>=ReqLenLim)or(ReqCnt>aggr^.max_segments);
  if ReqLen=0 then begin
    writeln(' done');
    s.Free;
    error:=1;
    Callback;
  end else begin
    writeln(' send ',s.size);
    SendMessage(s.memory^,s.size,aggr^.Remote);
    s.free;
    UnShedule(@OnTimeout);
    Shedule(750,@OnTimeout);
    DgrCntSinceReq:=0;
    //HighestRequestBase:=b;
  end;
end;

procedure tJob.OnInfo(icode:byte; ifilelen:LongWord; ireqlen:LongWord; msg:tMemoryStream);
  begin
  if (icode=0) or (icode=1) then begin
    Total:=ifilelen;
    {...:=}{ireqlen};
    if ireqlen=0 then begin
      {$hint some rate limiting}
      MakeRequest(3);
    end;
  end else begin
    error:=icode;
    Callback;
  end
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
      if first>=after then begin
        if assigned(k) then dispose(k);
      exit end;
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
  hiOfs:=msg.r1;
  assert(hiOfs=0{!!!});
  Offset:=msg.r4;
  dtlen:=msg.left;
  dataf.Position:=Offset;
  dataf.Write((msg.memory+msg.position)^,dtlen);
  {$hint iocheck todo}
  SetSegment(Offset,Offset+dtlen);
  RetryCounter:=0;
  Inc(DgrCntSinceReq);
  Inc(Received,dtLen);
end;

procedure tJob.OnTimeout;
  {called by aggr or resheduled in MakeRequest}
  begin
  if DgrCntSinceReq=0 then begin
    if RetryCounter>=13 then begin
      error:=-3;
      Callback;
    end else begin
      MakeRequest(4);
      Inc(RetryCounter);
    end;
  end
  else DgrCntSinceReq:=0; {next time aggr calls its probably serious}
end;

procedure tAggr.OnInfo(msg:tSMsg);
  var rr:packed record op,chn,flg,code:byte end;
  var flenhi:byte;
  var flen,rlen:LongWord;
  var s:tMemoryStream absolute msg.st;
  var debugmsg:string[127];
  begin
  {common things}
  s.Read(rr,4);
  {handle debug info}
  if rr.code=2 then begin
    SetLength(debugmsg,s.Left);
    s.Read(debugmsg[1],length(debugmsg));
    writeln('ObjTrans.',string(remote),'#',rr.chn,'.ServerDebug: '#10+debugmsg);
  EXIT end;
  max_channels:=s.r1;
  max_segments:=s.r1;
  distance:=s.r1-msg.ttl;
  if rr.chn>0 then begin
    flenhi:=s.r1;
    flen:=s.r4;
    rlen:=s.r4;
    assert(flenhi=0{!!!});
    if (rr.chn<=high(channel)) and assigned(channel[rr.chn]) then begin
      channel[rr.chn]^.OnInfo(rr.code,flen,rlen,s);
    end {else invalid info...}
  end else begin
    {info/error to all channels...}
  end;
end;

procedure tAggr.OnData(msg:tSMsg);
  var op,chn,mark:byte;
  var rate:single;
  var s:tMemoryStream absolute msg.st;
  begin
  {common things}
  op:=s.r1;
  mark:=s.r1;
  chn:=s.r1;
  {handle invalid channel}
  if chn=0 then exit;
  if (chn>high(channel)) or (not assigned(channel[chn])) then begin
    s.clear;
    s.w1(opcode.otStop);
    s.w1(chn);
    SendMessage(s.memory^,s.size,Remote);
  exit end;
  {measurements}
  if (DgrCnt=0)or(MarkValue<>mark) then begin
    {single delayed packet can mess up this}
    StartT:=mNow;
    ByteCnt:=0;
    DgrCnt:=0;
    MarkValue:=mark;
    LargestDgr:=0;
  end;
  Inc(ByteCnt,s.Size);
  Inc(DgrCnt);
  Inc(DgrCntCheck);
  if LargestDgr<s.Size then LargestDgr:=s.Size;
  {give channel data up}
  channel[chn]^.OnData(s);
  {send report}
  if (op=otDataSync) or ((DgrCnt>=3) and ((mNow-StartT)>=100)) then begin
    s.Clear;
    s.w1(otReport);
    s.w1(MarkValue);
    s.w1(0{flags});
    s.w1(0{TTL});
    rate:=(ByteCnt/(mNow-StartT))*16;
    s.w4(round(rate));
    s.w2(LargestDgr);
    StartT:=mNow;
    ByteCnt:=1;
    DgrCnt:=0;
    SendMessage(s.memory^,s.size,Remote);
  end;
end;


procedure tAggr.Init;
  var i:integer;
  begin
  refc:=0; ncpt:=0;
  ReadyForData:=true;
  Shedule(915,@Periodic);
  for i:=1 to high(channel) do channel[i]:=nil;
  ByteCnt:=0;
  DgrCnt:=0;
  DgrCntCheck:=0;
  StartT:=0;
  LargestDgr:=32;
  Distance:=65535;
  max_channels:=32;
  max_segments:=15;
  MarkValue:=255;
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

procedure tAggr.ResetIdle;
  begin
  if (RefC=0) and (idletick>0) then idletick:=1;
end;

procedure tAggr.Periodic;
  var i:integer;
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
      writeln('ObjTrans.tAggr.Periodic.IdleDeleteSelf ',string(remote));
      i:=FindIndex(@Aggrs[0], high(Aggrs)+1, @Remote, @compareAggr);
      writeln('ObjTrans.tAggr.Periodic.IdleDeleteSelf ',i);
      Aggrs[i]:=nil;
      ReadyForData:=false;
      FreeMem(@self,sizeof(self));
    EXIT end;
    inc(idletick);
  end;
  Shedule(700,@Periodic);
end;

procedure DataHandler(msg:tSMsg);
  var aggr:^tAggr;
  begin
  aggr:=GetAggr(Msg.Source, False);
  if assigned(aggr) and (aggr^.ReadyForData) then begin
    if byte(msg.Data^)=opcode.otInfo
      then aggr^.OnInfo(Msg)
      else aggr^.OnData(Msg);
  end;
end;

BEGIN
  SetupOpcode(opcode.otData,@DataHandler);
  SetupOpcode(opcode.otDataSync,@DataHandler);
  SetupOpcode(opcode.otInfo,@DataHandler);
END.
