unit TestFS;

INTERFACE
IMPLEMENTATION
USES ServerLoop,Chat,SysUtils,MemStream,NetAddr,opcode,Download;

type t=object
 ch: tChat;
 dw:Download.tAggr;
 //procedure UserInput
 procedure ST1(msg:tSMsg; data:boolean);
 procedure ST2(msg:tSMsg; data:boolean);
 procedure ST3(msg:tSMsg; data:boolean);
 procedure Rekt;
 procedure HardTimeout;
end;

procedure t.ST1(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.stream;
 var s:tMemoryStream;
 var op:byte;
 begin
 {reply from GET request}
 write('TestFS: ST1 reply from FS: ');
 if not data then begin
  writeln('ack');
 end else begin
  ch.Ack;
  op:=msg.stream.ReadByte;
  if op=upFAIL then writeln('FAIL ',r.readbyte{,'-',r.readbyte})
  else if op=upINFO then begin
   r.skip(2);
   writeln('INFO size=',r.ReadWord(4),' final=',r.readbyte,' seg=',r.readword(4));
   ch.Callback:=@ST2;
   UnShedule(@HardTimeout);
  end else if op=upClose then writeln('CLOSE')
  else writeln('unknown');
 end;
end;
procedure t.ST2(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.stream;
 var s:tMemoryStream;
 var op:byte;
 begin
 {Status Message}
 write('TestFS: ST2 reply from FS: ');
 if data then begin
  ch.Ack;
  op:=msg.stream.ReadByte;
  if op=upCLOSE then writeln('CLOSE ')
  else if op=upDONE then begin
   writeln('DONE');
   halt(99);
   ch.streaminit(s,1);
   s.WriteByte(opcode.upClose);
   ch.Send(s);
   ch.Callback:=@ST3;
  end;
 end else writeln('ack (unexpected)');
end;
procedure t.ST3(msg:tSMsg; data:boolean);
 begin
 {ACK to Close}
 write('TestFS: ST3 reply from FS: ');
 if data then writeln('unepected data') else begin
  writeln('ack');
  ch.DisposeHook:=@Rekt;
  ch.Close;
  dw.Done;
 end;
end;

var cnt:LongWOrd=0;
procedure IgnoreData(msg:tSMsg);
 begin
 cnt:=cnt+msg.length;
 //Writeln('Data: ',cnt);
end;

procedure t.Rekt;
 begin
 writeln('TestFS: rekt');
 UnShedule(@HardTimeout);
 FreeMem(@self,sizeof(self));
end;

procedure t.HardTimeout;
 begin
 writeln('TestFS: hardtimeout');
 ch.DisposeHook:=@Rekt;
 ch.Close;
end;

procedure init;
 var o:^t;
 var oi:word;
 var s:tMemoryStream;
 const opt='-test-fs';
 begin
 oi:=OptIndex(opt);
 if oi>0 then begin
  assert(OptParamCount(oi)=1,opt+'(rcpt:tNetAddr)');
  writeln('TestFS: rcpt '+paramstr(oi+1));
  new(o); with o^ do begin
   ch.Init(paramstr(oi+1));
   ch.Callback:=@ST1;
   dw.Init(ch.remote);
   Shedule(20000,@HardTimeout);
   ch.streaminit(s,33);
   s.WriteByte(opcode.upFileServer);
   s.WriteByte({channel}99);
   s.WriteByte(opcode.upGET);
   s.Skip(20);
   s.WriteWord(0,2);
   s.WriteWord(0,4);
   s.WriteWord($FFFFFFFF,4);
   ch.Send(s);
   //ServerLoop.SetMsgHandler(4,@IgnoreData);
   //ServerLoop.SetMsgHandler(6,@IgnoreData);
  end;
 end;
end;

BEGIN
 init;
END.