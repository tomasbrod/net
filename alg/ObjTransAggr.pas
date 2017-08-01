UNIT ObjTransAggr;
{
  Object Transfer Client, aggregator part
}
INTERFACE
uses ObjectModel,ServerLoop;

type
tOTAggr_ptr=^tOTAggr;
tOTConsumer_ptr=^tOTChannel deprecated;

tOTChannel=object
  stall: Boolean;
  iwait: Byte; {0=not waiting, 1=waiting, >2=backoff}
  conn: tOTAggr_ptr;
  chn: byte;
  constructor Init;
  function Attach(const iRemote: tNetAddr): boolean;
  destructor Done;
  procedure OnRawData(s: tMemoryStream); virtual;
  procedure OnTimeout; virtual;
  procedure SetWait(v:boolean);
  
end;

tOTAggr=object
  Remote: tNetAddr;
  Distance: Word; {network distance}
  private
  channel: array [1..32] of ^tOTChannel;
  ByteCnt:LongWord; {Speed}
  DgrCnt:LongWord;  {Speed}
  FlowDgrCnt: LongWord; {Stalled channel detection}
  StartT: tMtime;   {Speed}
  IdleTicks: Byte;
  RefC: Byte; {count of used channels}
  procedure Periodic;
  procedure Init;
  procedure OnData(msg:tSMsg);
end;

function OTGetAggr(const iRemote: tNetAddr): tOTAggr_ptr;
function OTGetExistingAggr(const iRemote: tNetAddr): tOTAggr_ptr;

IMPLEMENTATION
uses opcode;

var Aggrs: array [0..511] of ^tOTAggr;

function compareAggr(a: pointer; key: pointer): ShortInt;
  begin
  if assigned(a)
  then result:=CompareByte(tOTAggr(a^).Remote, tNetAddr(key^), sizeof(tNetAddr))
  else result:=1; {outside of array, go left}
end;

function OTGetAggr(const iRemote: tNetAddr): tOTAggr_ptr;
  var i:longword;
  var a:^tOTAggr;
  begin
  i:=FindIndex(@Aggrs[0], high(Aggrs)+1, @iremote, @compareAggr);
  result:=nil;
  if i>high(Aggrs) then exit;
  if assigned(Aggrs[i]) then begin
    if Aggrs[i]^.Remote=iRemote then begin
      result:=Aggrs[i];
      exit;
    end;
    {move to right}
    if not PtrListShiftRight(@Aggrs[0], High(Aggrs), i) then exit;
  end;
  New(A);
  Aggrs[i]:=A;
  A^.Remote:=iRemote;
  A^.Init;
  result:=A;
end;

function OTGetExistingAggr(const iRemote: tNetAddr): tOTAggr_ptr;
  var i:longword;
  begin
  i:=FindIndex(@Aggrs[0], high(Aggrs)+1, @iremote, @compareAggr);
  if i>high(Aggrs) then result:=nil
  else result:=Aggrs[i];
end;

procedure tOTAggr.Init;
  var i:LongWord;
  begin
  Distance:=High(Word);
  ByteCnt:=0;
  DgrCnt:=0;
  StartT:=0;
  FlowDgrCnt:=0;
  IdleTicks:=0;
  RefC:=0;
  for i:=1 to high(channel) do channel[i]:=nil;
  Shedule(440,@Periodic);
end;

procedure tOTAggr.Periodic;
  var i:LongWord;
  begin
  if refc>0 then begin
    if FlowDgrCnt>770 then FlowDgrCnt:=0;
    {check DgrCnt and timeout all channels}
    {check stall and wait per channels}
    for i:=1 to high(channel) do if assigned(channel[i]) then begin
      if DgrCnt=0 then
        channel[i]^.OnTimeOut
      else begin
        if ((FlowDgrCnt=0) and channel[i]^.stall) or (channel[i]^.iwait=1) then begin
          channel[i]^.OnTimeOut;
          break;
        end;
      end;
    end;
  end else begin
    {check idle time and delete self}
    if IdleTicks>34 then begin
      I:=FindIndex(@Aggrs[0], high(Aggrs)+1, @remote, @compareAggr);
      Assert((i<=high(Aggrs)) and assigned(Aggrs[i]));
      PtrListShiftLeft(@Aggrs[0],high(Aggrs),i);
      UnShedule(@Periodic);
      FreeMem(@self,sizeof(self));
      EXIT;
    end else Inc(idleticks);
  end;
  Shedule(400,@Periodic);
end;

procedure tOTAggr.OnData(msg:tSMsg);
  var chn,oh:byte;
  var sm:Word;
  var slen:LongWord;
  var s:tMemoryStream absolute msg.st;
  var rate:single;
  var debugmsg:string[127];
  begin
  s.Skip(1);
  chn:=s.ReadByte;
  oh:=s.ReadByte;
  if DgrCnt=0 then StartT:=mNow; {first datagram, start measuring speed}
  Inc(ByteCnt,s.Length);
  Inc(DgrCnt);
  Inc(FlowDgrCnt);
  if oh=otRateInfo then begin
    SetLength(debugmsg,s.RdBufLen);
    s.Read(debugmsg[1],s.RdBufLen);
    writeln('ObjTrans.',string(remote),'#',chn,'.ServerDebug: '+debugmsg);
  exit end;
  if chn=0 then exit;
  if (chn>high(channel)) or (nil=channel[chn]) then begin
    s.Seek(0); s.Trunc;
    s.WriteByte(otCtrl);
    s.WriteByte(otFin);
    s.WriteByte(chn);
    SendMessage(s.base^,s.length,Remote);
  exit end;
  if oh=otSINC then s.Read(sm,2) else s.seek(s.position-1);
  channel[chn]^.OnRawData(s);
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
    StartT:=mNow;
    SendMessage(s.base^,s.length,Remote);
  end;
end;

function tOTChannel.Attach(const iRemote: tNetAddr): boolean;
  var i: integer;
  begin
  result:=false;
  assert(conn=nil); assert(chn=0);
  conn:=OTGetAggr(iRemote);
  if conn=nil then exit;
  for i:=1 to high(conn^.channel) do begin
    if conn^.channel[i]=nil then begin
      conn^.channel[i]:=@self;
      Inc(conn^.RefC);
      chn:=i;
      stall:=true;
      iwait:=0;
      result:=true;
      break;
    end;
  end;
end;
      
destructor tOTChannel.Done;
  begin
  if (conn<>nil)and(chn>0) then begin
    conn^.channel[chn]:=nil;
    dec(conn^.RefC);
  end;
end;

procedure tOTChannel.SetWait(v:boolean);
  begin
  if v then iwait:=1
       else iwait:=0;
end;

constructor tOTChannel.Init;
  begin
  conn:=nil; chn:=0;
end;
procedure tOTChannel.OnRawData(s: tMemoryStream);
  begin AbstractError end;
procedure tOTChannel.OnTimeout;
  begin AbstractError end;

procedure DataHandler(msg:tSMsg);
  var aggr:^tOTAggr;
  begin
  aggr:=OTGetExistingAggr(msg.Source);
  if aggr<>nil then aggr^.OnData(Msg);
end;

BEGIN
  SetupOpcode(opcode.otData,@DataHandler);
END.
