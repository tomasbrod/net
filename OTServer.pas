unit OTServer;

{$DEFINE USE_UNIXDGQ}

INTERFACE
IMPLEMENTATION
USES Sockets,BaseUnix,ServerLoop,ObjectModel,opcode,SysUtils,Store,Classes,Porting;

var thread: record
  ID: tThreadID;
  event:tConditionEventLock;
  flag:boolean;
  source:tNetAddr;
  dgdata:array [1..1024] of byte;
  dglen:longword;
end;

var PollTimeout:LongInt;
var mNow:tMTime; {override for our thread}
var sharedbuf:array [1..2048] of byte; {->1k}

type
tClient_ptr=^tClient; tChannel_ptr=^tChannel;
tSegDescr=record
  b:LongWord;
  l:LongWord;
end;
tChannel=object
  cli: tClient_ptr;
  chn: byte;
  weight,eotrtr:byte;
  opened:boolean;
  fo:tStream;
  FID:tFID;
  seg:array [0..47] of tSegDescr;
  si:shortint;
  procedure Reset(const cmd:tCustomMemoryStream);
  procedure Finish;
  procedure Tick;
  procedure FillBuff(var s:tMemoryStream; sz:LongWord);
end;
tClient=object
  {serverside client object}
  next:tClient_ptr;
  addr:tNetAddr; socket:tSocket; saddr:tSockAddrL;
  socket_ttl:Word;
  {channel sheduler}
  channel:array [1..32] of ^tChannel;
  ccnt:byte; cur:byte; wcur:word;
  {Transmission Cotroll}
  MarkData:LongWord; MarkStart:tMTime;
  txDelay:tMTime;
  Rate:single;
  Size,Size2:word;
  rateIF,sizeIF:Single;
  {Timeouts and Acks}
  AckCount:Word;
  idleTicks:Word;
  MarkValue:byte;
  procedure CalcRates(rxRate:Single);
  procedure Send(const data; len:Word);
  procedure SendInfo(chn: byte; error_code: byte; req_length: LongWord; const msg: string);
  procedure Recv(op:byte; cmd:tCustomMemoryStream);
  procedure Init(const iaddr:tNetAddr);
  procedure NewChannel(ch:byte);
  procedure RecvReport(const s:tCustomMemoryStream);
  function  SlowTick:boolean;
  procedure Tick;
  procedure SwitchChannel;
  procedure SwitchChannel(ch:byte);
end;

function FindClient(const addr:tNetAddr; creat:boolean):tClient_ptr; forward;
procedure SetPollTimeout(when:TMtime); forward;
function Clamp(len:LongWord;ofs:LongWord;max:LongWord):LongWord; forward;
var Clients:tClient_ptr;

procedure tClient.Init(const iaddr:tNetAddr);
  var i:byte;
  begin
  addr:=iaddr;
  addr.ToSocket(saddr);
  socket:=GetSocket(addr);
  socket_ttl:=GetSocketTTL(addr);
  for i:=high(channel) downto 1 do channel[i]:=nil;
  cur:=0; ccnt:=0; wcur:=0;
  IdleTicks:=0; AckCount:=0;
  txDelay:=0;
  MarkData:=0;
  CalcRates(2);
  SendInfo(0, 2, 0, 'Client initialized');
end;

procedure tClient.CalcRates(rxRate:Single);
  var txRate:Single;
  var RateFill:Single;
  var dbg:String[127];
	begin
  dbg:='';
	if MarkData=0 then begin
		txRate:=rxRate; Rate:=4;
		Size:=128; Size2:=0;
		RateIF:=2; SizeIF:=1;
	end else begin
    if (mNow-MarkStart)=0 then exit;
    txRate:=(MarkData)/(mNow-MarkStart);
    if txRate>Rate then txRate:=Rate;
  end;
	if rxRate=0 then rxRate:=1;
	if txRate=0 then txRate:=1;
	RateFill:=rxRate/txRate;
	dbg:=dbg+Format('rx %8.1F tx %8.2F %3.0F%% ',[rxRate,txRate,RateFill*100]);
	if RateFill<0.90 then begin
		dbg:=dbg+'lim';
		if RateFill<0.5 then Size:=round(Size*0.75);
		Rate:=rxRate;
		RateIF:=RateIF*0.1*RateFill;
	end else if (txRate/Rate)>0.7 then begin
		dbg:=dbg+'pas';
		Rate:=txRate*(RateIF+1);
		//if thr.Rate>limRate then thr.Rate:=limRate else
		RateIF:=RateIF*2+0.001;
		if RateIF>1 then RateIF:=1;
	end else dbg:=dbg+'3ha';
	MarkData:=0;	MarkStart:=mNow;
	if Size<31 then Size:=31;
	{now do packet size}
  if SizeIF>1 then SizeIF:=1;
	if (size*SizeIF)<1 then SizeIF:=1/Size;
	if (Size/Rate)>100 then begin
    Size:=49; Size2:=0; SizeIF:=0; Rate:=0.5; RateIF:=0.8; {TODO!}
    dbg:=dbg+'fr'
  end else begin
    dbg:=dbg+'  ';
    Size2:=round(Size*(1+SizeIF));
  end;
  if Size2>sizeof(sharedbuf) then size2:=sizeof(sharedbuf);
  if Size2=Size then Size2:=0;
	dbg:=dbg+format(' | tx %8.2F %6.4F sz %5D %5D',[Rate,RateIF,Size,Size2]);
  SendInfo(0, 2, 0, dbg);
end;

procedure tClient.Tick;
  var s:tMemoryStream;
  begin
  if ccnt=0 then begin cur:=0; exit end;
  if mNow>=TxDelay then begin
    if (wcur=0)or(channel[cur]^.weight=0) then SwitchChannel;
    if wcur>0 then begin
      assert(channel[cur]^.weight>0);
      s:=tMemoryStream.Create;
      s.W1(otData);
      s.W1(cur);
      s.WB(MarkValue,1);
      if Size2>0 then begin
        channel[cur]^.FillBuff(s,Size2-5);
        Size2:=0;
      end
      else channel[cur]^.FillBuff(s,Size-2);
      dec(wcur);
      //writeln('txstat: data=',MarkData,' rate=',rate,' time=',mNow-MarkStart,' should=',MarkData/Rate);
      txDelay:= trunc((MarkData/Rate) + MarkStart);
      //writeln('txdelay ',txDelay-mNow, ' data:',MarkData,' time:',mNow-MarkStart);
      SetPollTimeout(txDelay);
    end else cur:=0;
  end else SetPollTimeout(TxDelay);
end;

procedure tChannel.Reset(const cmd:tCustomMemoryStream);
  var id:tFID;
  var prio,s2:integer;
  var tmp:tSegDescr;
  var reqlen:LongWord;
  begin
  {34}
  prio:=cmd.r1;
  cmd.rb(id,24);
  if (not opened)or(FID<>ID) then begin
    if opened then fo.Destroy;
    try
      if not IsBlob(id) then raise eObjectNF.Create('');
      FID:=id;
      fo:=StoreOpen(FID);
      opened:=true;
    except on e: eObjectNF do begin
      cli^.SendInfo(chn, 3, 0, '');
      Finish; exit;
    end; end;
  end;
  s2:=1+high(seg)-(cmd.Left div 9);
  if s2<0 then s2:=0;
  si:=s2; reqlen:=0;
  seg[high(seg)].l:=0;
  seg[high(seg)].b:=0;
  while s2<=high(seg) do begin
    cmd.r1;
    tmp.b:=cmd.r4;
    tmp.l:=Clamp(cmd.r4,tmp.b,fo.size);
    seg[s2]:=tmp;
    inc(s2); reqlen:=reqlen+tmp.l;
  end;
  cli^.SendInfo(chn, 0, reqlen, '');
  if reqlen=0 then weight:=0 else begin
    fo.position:=seg[si].b;
    weight:=prio+1;//*6
    cli^.SwitchChannel(chn); //activate this channel
  end;
end;

procedure tChannel.Finish;
  begin
  cli^.SendInfo(chn,2,0,'Finish channel');
  if opened then fo.Destroy;
  opened:=false;
  cli^.channel[chn]:=nil;
  dec(cli^.ccnt);
  FreeMem(@self,sizeof(self));
end;

procedure tChannel.FillBuff(var s:tMemoryStream; sz:LongWord);
  begin
  s.w1(0);{high part of offset, must be <128}
  s.w4(seg[si].b);
  sz:=sz-5;
  if sz>seg[si].l then sz:=seg[si].l;
  s.size:=s.position+sz;
  fo.RB((s.memory+s.position)^,sz); {todo errorcheck}
  if true then begin
    seg[si].b:=seg[si].b+sz;
    seg[si].l:=seg[si].l-sz;
    cli^.Send(s.memory^,s.size);
  end;
  if seg[si].l=0 then begin
    si:=si+1;
    if si>high(seg) then begin {or(red<sz)}
      weight:=0; eotrtr:=1;
      cli^.SendInfo(chn,otcEoT,0,'EoT in FillBuff');
    end
    else fo.position:=seg[si].b;
  end;
end;

procedure tChannel.Tick;
  begin
  //writeln(format('channel %d tick w %d si %d r %d',[chn,weight,si,eotrtr]));
  if weight>0 then exit;
  if si<=high(seg) then exit;
  if eotrtr<6 then begin
      cli^.SendInfo(chn,otcEoT,0,'EoT in FillBuff');
    inc(eotrtr);
  end else Finish;
end;

function tClient.SlowTick:boolean;
	var ch:byte;
	begin
	SlowTick:=false;
  //writeln('OTServer.tClient.SlowTick: ccnt=',ccnt,' IdleTicks=',idleticks,' cur=',cur,' AckCount=',AckCount);
	if ccnt=0 then begin
		if IdleTicks>=80 then begin
      SendInfo(0,2,0,'Closing, no channels and inactivity');
			SlowTick:=true;
		end else Inc(IdleTicks);
		exit;{cached for TC, do not even tick}
	end else
	if AckCount=0 then begin
		Inc(IdleTicks);
		if IdleTicks>=20 then begin
      SendInfo(0,2,0,'Closing while acitve, no response from client');
			{close all channels then self}
			for ch:=1 to high(channel) do if assigned(channel[ch]) then channel[ch]^.Finish;
			SlowTick:=true;
			exit;
		end;
		if (IdleTicks=2) and (MarkData>1000) then CalcRates(0.5);
	end;
	AckCount:=0;
	for ch:=1 to high(channel) do if assigned(channel[ch]) then channel[ch]^.Tick;
end;

procedure tClient.Recv(op:byte; cmd:tCustomMemoryStream);
  var ch:byte;
  var cmdsz:longword;
  begin
  cmdsz:=cmd.left;
  if (op=otRequest)and(cmdsz>=35) then begin
    ch:=cmd.r1;
    if (ch<=high(channel))and(ch>0) then begin
      if channel[ch]=nil then NewChannel(ch);
      channel[ch]^.Reset(cmd);
    end else SendInfo(ch, 4, 0, '');
  end;
  if (op=otStop)and(cmdsz>=1) then begin
    ch:=cmd.r1;
    if (ch>0) and (ch<=high(channel)) and assigned(channel[ch]) then begin
      IdleTicks:=0;
      channel[ch]^.Finish;
    end;
  end;
  if (op=otReport) and (cmdsz>=8) then begin
    RecvReport(cmd)
  end;
end;

procedure tClient.Send(const data; len:Word);
	begin
	MarkData:=MarkData+len;
	fpSendTo(socket,@data,len,0,@saddr,sizeof(saddr));
end;

procedure tClient.RecvReport(const s:tCustomMemoryStream);
	var rMark:byte;
  var rSize:Word;
	var rRate:LongWord;
	begin
	s.RB(rMark,1);
	rrate:=s.r4;
	rsize:=s.r2;
  IdleTicks:=0;
  if rMark=MarkValue then begin
    inc(AckCount);
    if rSize>Size then begin
      if ((rSize/Rate)<=40)and(rSize<=sizeof(sharedbuf)) then begin
        Size:=rSize;
        SizeIF:=SizeIF*2;
      end;
    end
    else sizeIF:=sizeIF/2;
    MarkValue:=Random(65535);
    CalcRates(rRate/16);
  end
  else SendInfo(0,2,0,'Report wrong mark');
end;


var LastSlowTick:tMTime;
const cSlowTick=100;
function SFSThread(param:pointer):PtrInt;
 var cmd:tBufferStream;
 var source:^tNetAddr;
 var op:byte;
 var client:^tClient;
 var p1:^pointer;
 begin
 result:=0;
 SetThreadName('ObjTransServer');
 cmd:=tBufferStream.Create(@sharedbuf,0);
 mNow:=GetMTime;
 LastSlowTick:=mNow;
 PollTimeout:=0;
 repeat
    thread.event.WaitLock(PollTimeout);
    mNow:=GetMTime;
    if thread.flag then begin
      thread.event.unlock;
      cmd.SetPointer(@thread.dgdata,thread.dglen);
      source:=@thread.source;
      if cmd.left>=2 then begin
        op:=cmd.r1;
        client:=FindClient(source^,op=otRequest);
        if assigned(client) then Client^.Recv(op,cmd);
      end;
      thread.flag:=false;
    end else thread.event.unlock;
  if assigned(clients) then begin
   if (mNow-LastSlowTick)>cSlowTick then begin
    LastSlowTick:=mNow;
    PollTimeout:=cSlowTick;
    client:=Clients;p1:=@Clients;
    while assigned(client) do begin
     if Client^.SlowTick then begin
      p1^:=client^.next;
      Dispose(client);
     end else p1:=@client^.next;
     client:=p1^;
    end;
   end else PollTimeout:=1+(LastSlowTick+cSlowTick)-mNow;
   {also tick all active clients}
   client:=Clients;
   while assigned(client) do begin
    if Client^.cur>0 then Client^.Tick;
    client:=client^.next;
   end;
  end else PollTimeout:=57616;
 until false;
end;

function Clamp(len:LongWord;ofs:LongWord;max:LongWord):LongWord;
 begin
 if (ofs+len)<=max then result:=len else begin
  if ofs>max
   then result:=0
   else result:=max-ofs;
 end;
end;
procedure SetPollTimeout(when:TMtime); inline;
 var nw:LongInt;
 begin
 nw:=when-mNow; if nw<0 then nw:=0;
 if (nw<PollTimeout) then begin
  PollTimeout:=nw;
 end;
end;

procedure tClient.SendInfo(chn: byte; error_code: byte; req_length: LongWord; const msg: string);
  var filelen:LongWord;
  var s:tMemoryStream;
  begin
  writeln('OTServer.',string(addr),'#',chn,'.Debug: '+msg);
  s:=tMemoryStream.create;
  s.w1(otInfo);
  s.w1(chn);
  s.w1(error_code);
  if error_code<>2 then begin
    s.w1(High(tClient.channel));
    s.w1(High(tChannel.seg));
    s.w1(socket_ttl);
    if (chn>0) and channel[chn]^.opened
      then filelen:=channel[chn]^.fo.size
      else filelen:=$FFFFFFFF;
    s.w1(0);
    s.w4(filelen);
    s.w4(req_length);
  end;
  if (error_code=2)and(msg<>'')
    then writeln('OTServer.',string(addr),'#',chn,'.Debug: '+msg);
  s.WB(msg[1],length(msg));
  Send(s.memory^,s.size);
end;

procedure tClient.SwitchChannel;
 var i,j:byte;
 begin
 wcur:=0;
 for i:=0 to high(channel)-1 do begin
  j:=((i+cur) mod high(channel))+1;
  if channel[j]=nil then continue;
  if channel[j]^.weight>0 then break;
 end;
 SwitchChannel(j);
end;
procedure tClient.SwitchChannel(ch:byte);
 begin
 if not assigned(channel[ch]) then exit;
 if cur=0 then begin {reset mark if resume}
  MarkData:=0;
  MarkStart:=mNow;
 end;
 cur:=ch;
 wcur:=channel[ch]^.weight*6;
end;
procedure tClient.NewChannel(ch:byte);
 begin
 New(channel[ch]);
 inc(ccnt);
 with channel[ch]^ do begin
  chn:=ch;
  cli:=@self;
  opened:=false;
end end;

function FindClient(const addr:tNetAddr; creat:boolean):tClient_ptr;
 begin
 result:=Clients;
 while assigned(result) do begin
  if result^.addr=addr then exit;
  result:=result^.next;
 end;
 if creat then begin
  new(result);
  result^.next:=Clients;
  result^.Init(addr); 
  Clients:=result;
 end;
end;

procedure CtrlForward(msg:tSMsg);
  begin {Forward message to thread with sender addr}
  thread.event.lock;
  if  (not thread.flag)
  and (msg.length<=sizeof(thread.dgdata))
  then begin
    Move(msg.Data^,{->}thread.dgdata,msg.Length);
    thread.dglen:=msg.Length;
    thread.source:=msg.Source;
    thread.flag:=true;
    thread.event.SignalUnlock;
  end else thread.event.unlock;
end;


BEGIN
 clients:=nil;
 SetupOpcode(opcode.otRequest,@CtrlForward);
 SetupOpcode(opcode.otReport,@CtrlForward);
 SetupOpcode(opcode.otStop,@CtrlForward);
 thread.event:=tConditionEventLock.create;
 thread.id:=BeginThread(@SFSThread);
 Assert(thread.id>0);
END.
