unit TestFS;

INTERFACE
IMPLEMENTATION
USES ServerLoop,Chat,SysUtils,MemStream,NetAddr,opcode;

type t=object
 ch: tChat;
 //procedure UserInput
 procedure ST1(msg:tSMsg; data:boolean);
 procedure ST2(msg:tSMsg; data:boolean);
 procedure ST3(msg:tSMsg; data:boolean);
 procedure Rekt;
 procedure HardTimeout;
end;

procedure t.ST1(msg:tSMsg; data:boolean);
 var s:tMemoryStream;
 begin
 write('TestFS: ST1 reply from FS: ');
 if not data then begin
  writeln('ack');
  s.init(GetMem(56),0,56);
  ch.AddHeaders(s);
  s.WriteByte(99);
  ch.Send(s);
  ch.Callback:=@ST2;
  halt(32);
 end else writeln('unexpected data');
end;
procedure t.ST2(msg:tSMsg; data:boolean);
 var s:tMemoryStream;
 begin
 write('TestFS: ST2 reply from FS: ');
 if data then begin
  writeln(msg.stream.ReadByte,'-',msg.stream.ReadByte);
  s.init(GetMem(56),0,56);
  ch.AddHeaders(s);
  s.WriteByte(opcode.upClose);
  ch.Send(s);
  ch.Callback:=@ST3;
 end else writeln('ack');
end;
procedure t.ST3(msg:tSMsg; data:boolean);
 begin
 write('TestFS: ST3 reply from FS: ');
 if data then writeln('unepected data') else begin
  writeln('ack');
  UnShedule(@HardTimeout);
  ch.DisposeHook:=@Rekt;
  ch.Close;
 end;
end;

procedure t.Rekt;
 begin
 writeln('TestFS: rekt');
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
   Shedule(7000,@HardTimeout);
   s.init(GetMem(56),0,56);
   ch.AddHeaders(s);
   s.WriteByte(opcode.upFileServer);
   ch.Send(s);
  end;
 end;
end;

BEGIN
 init;
END.