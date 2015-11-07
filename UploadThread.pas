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
 LastTime:=Now;
 pch:=curc;
 txwait:=((MarkData/Rate)*1000)-(MarkTime);
  {find usable channel}
 while (chans[curc]=nil)or(chans[curc]^.wcur=0)or(chans[curc]^.SegLen=0) do begin
  if assigned(chans[curc])and(chans[curc]^.WCur=0) then chans[curc]^.WCur:=chans[curc]^.weight;
  inc(curc);
  if curc>high(chans) then curc:=0;
  if curc=pch then stop:=true;
 end;
 if stop then begin
  LeaveCriticalSection(crit);
  exit
 end;
 s.Init(@buffer,0,high(buffer));
 {prepare header}
 if size2=0 then begin
  sz:=size1; if size1>s.size then sz:=s.size; 
  s.WriteByte(opcode.tcdata);
  s.WriteByte(mark1);
 end else begin
  sz:=size2; if sz>s.size then sz:=s.size;
  s.WriteByte(opcode.tcdataimm);
  s.WriteByte(mark2);
 end;
 s.WriteByte(curc);
 s.WriteWord(chans[curc]^.oi.offset,4);
 dec(sz,s.length);
 if sz>chans[curc]^.SegLen then sz:=chans[curc]^.oi.SegLen;
 chans[curc]^.oi.ReadAhead(sz,s.WrBuf);
 chans[curc]^.oi.WaitRead;
 Assert(chans[curc]^.oi.rc=0);
 s.WrEnd(sz);
 Dec(chans[curc]^.SegLen,sz);
 Dec(chans[curc]^.WCur);
 LeaveCriticalSection(crit);
 fpSendTo(socket,s.base,s.length,0,@remote,sizeof(remote));
 Sleep(round(txWait));
 Delta:=Delta+((LastTime-Now)*MSecsPerDay);
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
 if not stop then exit;
 stop:=false;
 thrid:=BeginThread(@ThrFunc,@self);
end;

procedure tUploadThr.Done;
 begin
 EnterCriticalSection(crit);
 stop:=true;
 LeaveCriticalSection(crit);
 WaitForThreadterminate(thrid,999999);
 DoneCriticalSection(crit);
end;
END.