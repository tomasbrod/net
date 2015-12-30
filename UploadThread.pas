unit UploadThread;
{coprocessor to Upload unit. Move I/O out of main thread}
INTERFACE
uses Store1,Sockets,NetAddr;

type tSegment=record
 base,len:LongWord;
end;
type tChannel=record
 s: array [1..24] of tSegment;
 seg:byte;
 weight:Word;
 wcur:Word;
 oi:tStoreObjectInfo;
 end;
type tUploadThr=object
 thrid:tThreadID;
 crit:tRtlCriticalSection;
 socket:tSocket;
 remote:tSockAddrL;
 size1,size2:Word;
 mark:Byte;
 rate:Single;
 MarkTime:LongWord;{ms}
 MarkData:LongWord;
 chans:array [0..11] of ^tChannel;
 curc:byte;
 buffer:array [0..2047] of byte;
 stop:boolean; {the therad is stopped or stopping}
 wait:boolean; {the therad is waiting for data}
 waitc:byte;

 procedure Main;
 procedure Init(source:tNetAddr);
 procedure Start;
 procedure Done;
end;

IMPLEMENTATION
uses MemStream,ServerLoop,SysUtils,opcode;

procedure tUploadThr.Init(source:tNetAddr);
 var i:integer;
 begin
 InitCriticalSection(crit);
 source.ToSocket(remote);
 socket:=GetSocket(source);
 MarkData:=0;
 MarkTime:=0;
 stop:=true;
 wait:=false;
 for i:=0 to high(chans) do chans[i]:=nil;
end;

procedure tUploadThr.Main;
 var pch:byte;
 var s:tMemoryStream;
 var sz:Word;
 var txwait,delta:single;//msec
 var LastTime:tDateTime;//days
 var chan:^tChannel;
 var seg:^tSegment;
 begin
 txwait:=0;
 delta:=0;
 while not stop do begin
 EnterCriticalSection(crit);
 pch:=0;
 {find usable channel}
 while (chans[curc]=nil)or(chans[curc]^.wcur=0)or(chans[curc]^.seg=0) do begin
  if assigned(chans[curc])and(chans[curc]^.WCur=0) then chans[curc]^.WCur:=chans[curc]^.weight;
  inc(curc);
  inc(pch);
  if curc>high(chans) then curc:=0;
  if pch>(high(chans)+1) then begin wait:=true; break; end;
 end;
 if wait then begin
  LeaveCriticalSection(crit);
  if waitc>10
  then stop:=true
  else inc(waitc);
  sleep(200);
  continue;
 end;
 waitc:=0;
 LastTime:=SysUtils.Now;
 chan:=chans[curc];
 seg:=@chan^.s[chan^.seg];
 s.Init(@buffer,0,high(buffer));
 {prepare header}
 if size2>s.size then size2:=0;
 if size2=0 then begin
  sz:=size1; if size1>s.size then sz:=s.size; 
  s.WriteByte(opcode.tcdata);
 end else begin
  sz:=size2; if sz>s.size then sz:=s.size;
  s.WriteByte(opcode.tcdataimm);
  size2:=0;
 end;
 Assert(seg^.len>0);
 s.WriteByte(mark);
 s.WriteByte(curc);
 s.WriteWord(seg^.base,4);
 dec(sz,s.length);
 if sz>seg^.Len then sz:=seg^.Len;
 assert(sz<=seg^.len);
 chan^.oi.ReadSeg(s.WrBuf,seg^.base,sz);
 Assert(chan^.oi.rc=0,'IO error reading segment');
 s.WrEnd(sz);
 assert((Seg^.Len-sz)>=0);
 Dec(Seg^.Len,sz);
 Dec(chan^.WCur);
 if Seg^.Len=0 then Dec(chan^.seg)
 else Inc(Seg^.Base,sz);
 LeaveCriticalSection(crit);
 fpSendTo(socket,s.base,s.length,0,@remote,sizeof(remote));
 txwait:=((MarkData/Rate)*1000)-(MarkTime);
 MarkData:=MarkData+s.length;
 if txWait>1000 then begin writeln('!!! txwait=',round(txWait)); txWait:=1000;end;
 if txWait>0 then Sleep(round(txWait));
 Delta:=Delta+((SysUtils.Now-LastTime)*MSecsPerDay);
 if Delta>5000 then Delta:=3000;
 if Delta<0 then Delta:=0;
 MarkTime:=MarkTime+trunc(Delta);
 Delta:=frac(Delta);
 end;
end;

function thrfunc(p:pointer):PtrInt;
 begin
 tUploadThr(p^).Main;
 thrfunc:=9;
end;
procedure tUploadThr.Start;
 begin
 wait:=false;
 if not stop then exit;
 stop:=false;
 MarkData:=0;
 MarkTime:=0;
 thrid:=BeginThread(@ThrFunc,@self);
end;

procedure tUploadThr.Done;
 begin
 if stop then exit;
 EnterCriticalSection(crit);
 stop:=true;
 LeaveCriticalSection(crit);
 WaitForThreadterminate(thrid,65535);
 DoneCriticalSection(crit);
end;
END.