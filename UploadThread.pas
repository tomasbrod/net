unit UploadThread;
{coprocessor to Upload unit. Move I/O out of main thread}
INTERFACE
uses Store1,Sockets,NetAddr;

type tUploadSegment=record
 seglen:LongWord;
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
 mark1,mark2:Byte;
 rate:Single;
 MarkTime:LongWord;{ms}
 MarkData:LongWord;
 chans:array [0..11] of ^tUploadSegment;
 curc:byte;
 buffer:array [0..2047] of byte;
 stop:boolean;
 wait:boolean;

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
 for i:=0 to high(chans) do chans[i]:=nil;
end;

procedure tUploadThr.Main;
 var pch:byte;
 var s:tMemoryStream;
 var sz:Word;
 var txwait,delta:single;//msec
 var LastTime:tDateTime;//days
 begin
 txwait:=0;
 delta:=0;
 repeat
 EnterCriticalSection(crit);
 if stop then begin
  LeaveCriticalSection(crit);
  exit
 end;
 LastTime:=SysUtils.Now;
 pch:=0;
 {find usable channel}
 while (chans[curc]=nil)or(chans[curc]^.wcur=0)or(chans[curc]^.SegLen=0) do begin
  if assigned(chans[curc])and(chans[curc]^.WCur=0) then chans[curc]^.WCur:=chans[curc]^.weight;
  inc(curc);
  inc(pch);
  if curc>high(chans) then curc:=0;
  if pch>(high(chans)+1) then begin wait:=true; break; end;
 end;
 if wait then begin
  LeaveCriticalSection(crit);
  sleep(500);
  continue;
 end;
 s.Init(@buffer,0,high(buffer));
 {prepare header}
 if size2>s.size then size2:=0;
 if size2=0 then begin
  sz:=size1; if size1>s.size then sz:=s.size; 
  s.WriteByte(opcode.tcdata);
  s.WriteByte(mark1);
 end else begin
  sz:=size2; if sz>s.size then size2:=s.size;
  s.WriteByte(opcode.tcdataimm);
  s.WriteByte(mark2);
  size2:=0;
 end;
 s.WriteByte(curc);
 s.WriteWord(chans[curc]^.oi.offset,4);
 dec(sz,s.length);
 if sz>chans[curc]^.SegLen then sz:=chans[curc]^.SegLen;
 chans[curc]^.oi.ReadAhead(sz,s.WrBuf);
 chans[curc]^.oi.WaitRead;
 Assert(chans[curc]^.oi.rc=0);
 s.WrEnd(sz);
 Dec(chans[curc]^.SegLen,sz);
 Dec(chans[curc]^.WCur);
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
 until false;
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
 EnterCriticalSection(crit);
 stop:=true;
 LeaveCriticalSection(crit);
 writeln('UploadThread: wait to terminate');
 WaitForThreadterminate(thrid,999999);
 writeln('UploadThread: doned');
 DoneCriticalSection(crit);
end;
END.