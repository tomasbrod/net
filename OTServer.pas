unit OTServer;

{$DEFINE USE_UNIXDGQ}

INTERFACE
IMPLEMENTATION
USES Sockets,BaseUnix,Porting,ServerLoop,ObjectModel,opcode,SysUtils,Store;

var thread: record
  ID: tThreadID;
  mutex:po_tmutex;
  condv:po_tcondv;
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
 fo:tStoreObject;
 FID:tFID;
 seg:array [0..47] of tSegDescr;
 si:shortint;
 procedure Reset(prio:byte; const cmd:tMemoryStream);
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
 AckMark:LongWord;
 procedure CalcRates(rxRate:Single);
 procedure Send(const data; len:Word);
 procedure SendDebug(chn:byte; const msg:string);
 procedure Recv(op:byte; cmd:tMemoryStream);
 procedure Init(const iaddr:tNetAddr);
 procedure NewChannel(ch:byte);
 procedure RecvSIACK(const s:tMemoryStream);
 procedure RecvSPEED(const s:tMemoryStream);
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
 SendDebug(0,'Client initialized');
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
	sizeIF:=sizeIF/2;
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
	SendDebug(0,dbg);
end;
procedure tClient.Tick;
 var s:tMemoryStream;
 begin
 if ccnt=0 then begin cur:=0; exit end;
 if mNow>=TxDelay then begin
  if (wcur=0)or(channel[cur]^.weight=0) then SwitchChannel;
  if wcur>0 then begin
   assert(channel[cur]^.weight>0);
   s.Init(@sharedbuf,0,sizeof(sharedbuf));
   s.WriteByte(otData);
   s.WriteByte(cur);
   if Size2>0 then begin
    s.WriteByte(otSINC);
    AckMark:=Random(65535);
    s.Write(AckMark,2);
    channel[cur]^.FillBuff(s,Size2-5); {$warning Size and Size2 is not limited!}
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

procedure tChannel.Reset(prio:byte; const cmd:tMemoryStream);
 var id:^tFID;
 var fn:array [0..44] of char;
 var info:tMemoryStream;
 var s2:integer;
 var tmp:tSegDescr;
 var reqlen:LongWord;
 begin
 id:=cmd.ReadPtr(24);
 info.Init(@fn,0,sizeof(fn));
 info.WriteByte(otData);
 info.WriteByte(chn);
 if (not opened)or(FID<>ID^) then begin
   if opened then fo.Done;
   try
    if not IsBlob(id^) then raise eObjectNF.Create('');
    FID:=id^;
    fo.Init(FID);
    opened:=true;
   except on eObjectNF do begin
    info.WriteByte(otNotFound);
    cli^.Send(info.base^,info.length);
   Finish;  exit end end;
 end;
 info.WriteByte(otInfo);
 info.WriteByte(0);
 info.WriteWord4(fo.length);
 info.WriteByte(High(seg));
 info.WriteByte(cli^.socket_ttl);
 s2:=1+high(seg)-(cmd.Left div 9);
 if s2<0 then s2:=0;
 si:=s2; reqlen:=0;
 seg[high(seg)].l:=0;
 seg[high(seg)].b:=0;
 while s2<=high(seg) do begin
  cmd.ReadByte;
  tmp.b:=cmd.ReadWord4;
  tmp.l:=Clamp(cmd.ReadWord4,tmp.b,fo.length);
  seg[s2]:=tmp;
  inc(s2); reqlen:=reqlen+tmp.l;
 end;
 info.WriteWord4(reqlen);
 cli^.Send(info.base^,info.length);
 if reqlen=0 then Finish else begin
  fo.Seek(seg[si].b);
  weight:=prio+1;//*6
  cli^.SwitchChannel(chn); //activate this channel
 end;
end;
procedure tChannel.Finish;
 begin
 cli^.SendDebug(chn,'Finish channel');
 if opened then fo.Done;
 opened:=false;
 cli^.channel[chn]:=nil;
 dec(cli^.ccnt);
 FreeMem(@self,sizeof(self));
end;
procedure tChannel.FillBuff(var s:tMemoryStream; sz:LongWord);
 var msgeot:string[3];
 begin
 s.WriteByte(0);{high part of offset, must be <128}
 s.WriteWord4(seg[si].b);
 sz:=sz-5;
 if sz>seg[si].l then sz:=seg[si].l;
 fo.Read(s.WrBuf^,sz); {todo errorcheck}
 if true then begin
   seg[si].b:=seg[si].b+sz;
   seg[si].l:=seg[si].l-sz;
   s.WrEnd(sz);
   cli^.Send(s.base^,s.length);
 end;
  if seg[si].l=0 then begin
    si:=si+1;
    if si>high(seg) then begin {or(red<sz)}
      weight:=0; eotrtr:=1;
      msgeot:=char(otData)+char(chn)+char(otEoT);
      //cli^.SendDebug(chn,'EoT in FillBuff');
      cli^.Send(msgeot[1],3);
    end
    else fo.Seek(seg[si].b);
  end;
end;
procedure tChannel.Tick;
 var msgeot:string[3];
 begin
 //writeln(format('channel %d tick w %d si %d r %d',[chn,weight,si,eotrtr]));
 if weight>0 then exit;
 if si<=high(seg) then exit;
 if eotrtr<6 then begin
  msgeot:=char(otData)+char(chn)+char(otEoT);
  cli^.Send(msgeot[1],3);
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
      SendDebug(0,'Closing, no channels and inactivity');
			SlowTick:=true;
		end else Inc(IdleTicks);
		exit;{cached for TC, do not even tick}
	end else
	if AckCount=0 then begin
		Inc(IdleTicks);
		if IdleTicks>=20 then begin
      SendDebug(0,'Closing while acitve, no response from client');
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

procedure tClient.Recv(op:byte; cmd:tMemoryStream);
 var ch:byte;
 begin
 try
 if op>=3 then ch:=cmd.ReadByte else ch:=0;
 if (op>otReq)and(ch>0)and(cmd.RdBufLen>=29) then begin
  if ch>high(channel) then exit; {$NOTE grace on channel overflow}
  if channel[ch]=nil then NewChannel(ch);
  channel[ch]^.Reset(op-otReq,cmd);
 end else if (op=otFin)and assigned(channel[ch]) then begin
  IdleTicks:=0;
  channel[ch]^.Finish
 end else if op=otSIACK then RecvSIACK(cmd)
 else if op=otSPEED then RecvSPEED(cmd);
 except
  on eReadPastEoF do writeln('OT: uncecked datagram size');
 end;
end;
procedure tClient.Send(const data; len:Word);
	begin
	MarkData:=MarkData+len;
	fpSendTo(socket,@data,len,0,@saddr,sizeof(saddr));
end;

procedure tClient.RecvSIACK(const s:tMemoryStream);
	var rMark,rSize:Word;
	begin
	s.Read(rMark,2);
	rSize:=s.ReadWord2;
  IdleTicks:=0;
  Inc(AckCount);
	if (rMark<>AckMark)or(rsize<=Size) then begin
    SendDebug(0,'SIACK wrong mark or size is not greater');
  exit end;
  if ((rSize/Rate)>40)or(rSize>sizeof(sharedbuf)) then begin
    Size2:=0;
    //SendDebug(0,'SIACK frequency too low ');
  end else begin
    Size:=rSize;
    SizeIF:=SizeIF*2; if SizeIF>1 then SizeIF:=1;
    Size2:=round(Size*(1+SizeIF));
    if (Size2<=Size)or(Size2>sizeof(sharedbuf)) then Size2:=0;
    SendDebug(0,format('sz %4D %5.3F sz2 %4D',[Size,SizeIF,Size2]));
  end;
end;
procedure tClient.RecvSPEED(const s:tMemoryStream);
	var rRate:LongWord;
	begin
	rrate:=s.readword4;
	inc(AckCount);
	IdleTicks:=0;
	CalcRates(rRate/16);
end;

{timers:
 every 1 sec: timeouts (all clients, all channels =>slow)
 every interrupt: TxWait (all clients)
 use GetTickCount!!!!!
}
var LastSlowTick:tMTime;
const cSlowTick=100;
function SFSThread(param:pointer):PtrInt;
 var cmd:tMemoryStream;
 var source:^tNetAddr;
 var op:byte;
 var client:^tClient;
 var p1:^pointer;
 begin
 result:=0;
 SetThreadName('ObjTransServer');
 cmd.Init(@sharedbuf,0,sizeof(sharedbuf));
 mNow:=GetMTime;
 LastSlowTick:=mNow;
 PollTimeout:=0;
 repeat
    po_lock(thread.mutex);
    po_wait(thread.condv,thread.mutex,PollTimeout);
    mNow:=GetMTime;
    if thread.flag then begin
      po_unlock(thread.mutex);
      cmd.Init(@thread.dgdata,thread.dglen,sizeof(thread.dgdata));
      source:=@thread.source;
      if cmd.rdbuflen>=2 then begin
        cmd.skip(1);
        op:=cmd.ReadByte;
        client:=FindClient(source^,op>=otReq);
        if assigned(client) then Client^.Recv(op,cmd);
      end;
      thread.flag:=false;
    end else po_unlock(thread.mutex);
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
procedure tClient.SendDebug(chn:byte; const msg:string);
  var buf:array [0..127] of byte;
  var s:tMemoryStream;
  begin
  writeln('OTServer.',string(addr),'#',chn,'.Debug: '+msg);
  s.Init(@buf,0,sizeof(buf));
  s.WriteByte(otData);
  s.WriteByte(chn);
  s.WriteByte(otRateInfo);
  s.Write(msg[1],length(msg));
  Send(buf,s.length);
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
  po_lock(thread.mutex);
  if  (not thread.flag)
  and (msg.length<=sizeof(thread.dgdata))
  then begin
    Move(msg.Data^,{->}thread.dgdata,msg.Length);
    thread.dglen:=msg.Length;
    thread.source:=msg.Source;
    thread.flag:=true;
    po_signal(thread.condv,thread.mutex);
  end;
  po_unlock(thread.mutex);
end;


BEGIN
 clients:=nil;
 SetupOpcode(opcode.otCtrl,@CtrlForward);
 po_cvar_init(thread.condv,thread.mutex);
 thread.id:=BeginThread(@SFSThread);
 Assert(thread.id>0);
END.
