unit OTServer;

INTERFACE
IMPLEMENTATION
USES Sockets,BaseUnix,ServerLoop,NetAddr,MemStream,opcode,SysUtils,Store2;

{ Protocol:
 Client init with REQ (op=otCtrl,st=req,ch>0) to server
 Client can bind addr+otData...
 Server send info and proceed sending segments of file
 Client must send SPEED periodicaly and SIACK when requested (st=flow)
 Server->Client data and controll are sent with opcode otData,
   then channel number
   highest bit of first byte of offset determine packet type
   0:data
   1:ctrl
   128=ifno, 129=fail, 130=EoT 131=SINC 132=rateinfo
 Server should send EoT at least 5 times before cloing.
 Simple and fast. Transfer begins within 1 round trip time.
 messages to server:
  op=otCtrl op=otReq+prio ch=x id:20 [ofs:5 len:4]
  op=otCtrl op=otFin ch=x
  op=otCtrl speed/siack
 messages to client:
  info length:5 max-seg-req:1
  fail code:1 message:string
  rateinfo message:string
}

var thrID:tThreadID;
var sock:tSockPairArray;
var PollTimeout:LongInt;
var mNow:tMTime; {override for our thread}
var sharedbuf:array [1..2048] of byte; {->1k}

type tFID=Store2.tFID;
type
tClient_ptr=^tClient; tChannel_ptr=^tChannel;
tChannel=object
 cli: tClient_ptr;
 chn: byte;
 weight,eotrtr:byte;
 opened:boolean;
 fo:tSObj;
 segofs:LongWord;{cache}
 seglen:LongWord;{no seglist in sync yet}
 procedure Reset(prio:byte; const cmd:tMemoryStream);
 procedure Finish;
 procedure Tick;
 procedure FillBuff(var s:tMemoryStream; sz:LongWord);
end;
tClient=object
 {serverside client object}
 next:tClient_ptr;
 addr:tNetAddr; socket:tSocket; saddr:tSockAddrL;
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
		RateIF:=1; SizeIF:=1;
	end else begin
    if (mNow-MarkStart)=0 then exit;
    txRate:=(MarkData)/(mNow-MarkStart);
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
		RateIF:=RateIF+0.1;
		if RateIF>1 then RateIF:=1;
	end else dbg:=dbg+'3ha';
	MarkData:=0;	MarkStart:=mNow;
	if Size<96 then Size:=104;
	{now do packet size}
	sizeIF:=sizeIF/2;
	if (size*SizeIF)<1 then SizeIF:=1/Size;
	if (Size/Rate)>70 then begin Size:=32; Rate:=0.5;
	 dbg:=dbg+'fr' end else dbg:=dbg+'  ';
	Size2:=round(Size*(1+SizeIF));
  if Size2>sizeof(sharedbuf) then size2:=sizeof(sharedbuf);
  if Size2=Size then Size2:=0;
	dbg:=dbg+format(' | rt %8.2F %6.4F sz %5D %6.4F %5D',[Rate,RateIF,Size,SizeIF,Size2]);
	SendDebug(0,dbg);
end;
procedure tClient.Tick;
 var s:tMemoryStream;
 begin
 if ccnt=0 then begin cur:=0; exit end;
 if mNow>=TxDelay then begin
  if (wcur=0)or(channel[cur]^.weight=0) then SwitchChannel;
  if wcur>0 then begin
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
 begin
 id:=cmd.ReadPtr(20);
 fn:='obj/';
 BinToHex(@fn[4],id^,20); fn[44]:=#0;
 if opened then fo.Close;
 info.Init(@fn,0,sizeof(fn));
 info.WriteByte(otData);
 info.WriteByte(chn);
 try
  fo.Init(id^);
  opened:=true;
 except on eObjectNF do begin
  info.WriteByte(otNotFound);
  cli^.Send(info.base^,info.length);
  Finish;  exit end end;
 info.WriteByte(otInfo);
 info.WriteByte(0);
 info.WriteWord(fo.length,4);
 info.WriteByte(1);
 seglen:=0;
 segofs:=0;
 while cmd.RdBufLen>=9 do begin
  cmd.Skip(1);
  segofs:=cmd.ReadWord(4);
  seglen:=cmd.ReadWord(4);
  seglen:=Clamp(seglen,segofs,fo.length);
  break{singleseg};
 end;
 info.WriteWord(seglen,4);
 cli^.Send(info.base^,info.length);
 if seglen=0 then Finish else begin
  fo.Seek(segofs);
  cli^.IdleTicks:=0;
  weight:=prio+1;//*6
  cli^.SwitchChannel(chn); //activate this channel
 end;
end;
procedure tChannel.Finish;
 begin
 cli^.SendDebug(chn,'Finish channel');
 if opened then fo.Close;
 opened:=false;
 cli^.channel[chn]:=nil;
 dec(cli^.ccnt);
 FreeMem(@self,sizeof(self));
end;
procedure tChannel.FillBuff(var s:tMemoryStream; sz:LongWord);
 var msgeot:string[3];
 begin
 s.WriteByte(0);{high part of offset, must be <128}
 s.WriteWord(segofs,4);
 sz:=sz-5;
 if sz>seglen then sz:=seglen;
 fo.Read(s.WrBuf^,sz); {todo errorcheck}
 if true then begin
   segofs:=segofs+sz;
   seglen:=seglen-sz;
   s.WrEnd(sz);
   cli^.Send(s.base^,s.length);
 end;
 if (seglen=0){or(red<sz)} then begin
  weight:=0; eotrtr:=1;
  msgeot:=char(otData)+char(chn)+char(otEoT);
  cli^.Send(msgeot[1],3);
 end;
end;
procedure tChannel.Tick;
 var msgeot:string[3];
 begin
 if weight>0 then exit;
 if seglen>0 then exit;
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
		if IdleTicks>=16 then begin
      SendDebug(0,'Closing, no channels and inactivity');
			SlowTick:=true;
		end else Inc(IdleTicks);
		exit;{cached for TC, do not even tick}
	end;
	if (cur>0)and(AckCount=0) then begin
		Inc(IdleTicks);
		if IdleTicks>=10 then begin
      SendDebug(0,'Closing while acitve, no response from client');
			{close all channels then self}
			for ch:=1 to high(channel) do if assigned(channel[ch]) then channel[ch]^.Finish;
			SlowTick:=true;
			exit;
		end;
		if IdleTicks=2 then CalcRates(0.5);
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
  on MemStream.eReadPastEoF do writeln('OT: uncecked datagram size');
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
	rSize:=s.ReadWord(2);
  IdleTicks:=0;
  Inc(AckCount);
	if (rMark<>AckMark)or(rsize<=Size) then begin
    SendDebug(0,'SIACK wrong mark or size is not greater');
  exit end;
  if ((rSize/Rate)>40)or(rSize>sizeof(sharedbuf)) then begin
    Size2:=0;
    SendDebug(0,'SIACK frequency too low ');
  end else begin
    Size:=rSize;
    SizeIF:=SizeIF*2; if SizeIF>1 then SizeIF:=1;
    Size2:=round(Size*(1+SizeIF));
    if (Size2<=Size)or(Size2>sizeof(sharedbuf)) then Size2:=0;
    SendDebug(0,format('SIACK sz %4D %5.3F sz2 %4D',[Size,SizeIF,Size2]));
  end;
end;
procedure tClient.RecvSPEED(const s:tMemoryStream);
	var rRate:LongWord;
	begin
	rrate:=s.readword(4);
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
const cSlowTick=600;
function SFSThread(param:pointer):PtrInt;
 var cmd:tMemoryStream;
 var rc:LongInt;
 var source:^tNetAddr;
 var op:byte;
 var PollStruct:array [0..0] of BaseUnix.pollfd;
 var client:^tClient;
 var p1:^pointer;
 begin
 result:=0;
 SetThreadName('ObjTransServer');
 PollStruct[0].fd:=sock[1];
 PollStruct[0].Events:=BaseUnix.PollIN;
 cmd.Init(@sharedbuf,0,sizeof(sharedbuf));
 mNow:=GetMTime;
 LastSlowTick:=mNow;
 PollTimeout:=0;
 repeat
  PollStruct[0].rEvents:=0;
  Assert(fpPoll(@PollStruct,1,PollTimeout)>=0);
  mNow:=GetMTime;
  if PollStruct[0].rEvents=PollIN then begin
   cmd.Init(@sharedbuf,0,sizeof(sharedbuf));
   rc:=fpRecv(sock[1],cmd.base,cmd.size,0);
   assert(rc>=0); if rc=0 then break;
   cmd.vlength:=rc;
   source:=cmd.ReadPtr(sizeof(tNetAddr));
   if cmd.rdbuflen>=2 then begin
    op:=cmd.ReadByte;
    client:=FindClient(source^,op>=otReq);
    if assigned(client) then Client^.Recv(op,cmd);
   end;
  end;
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
 if (wcur>0)and(cur>0) then exit;
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
 end;
 if creat then begin
  new(result);
  result^.next:=Clients;
  result^.Init(addr); 
  Clients:=result;
 end;
end;

procedure CtrlHandler(msg:tSMsg);
 var buf:array [0..1024+sizeof(tNetAddr)] of byte;
 var s:tMemoryStream;
 begin {Forward message to thread with sender addr}
 s.Init(@buf,0,sizeof(buf));
 s.Write(msg.source^,sizeof(tNetAddr));
 msg.stream.skip(1);
 s.Write(msg.stream.RdBuf^,msg.Stream.RdBufLen);
 fpSend(sock[0],s.base,s.length,0);
end;

BEGIN
 {$IFDEF windows}{$WARNING Windows does not support AF_LOCAL sockets!}{$ENDIF}
 assert(fpSocketpair(AF_LOCAL,SOCK_DGRAM,0,@sock)=0);
 thrid:=BeginThread(@SFSThread);
 if thrid=0 then;
 SetMsgHandler(opcode.otCtrl,@CtrlHandler);
 clients:=nil;
END.
