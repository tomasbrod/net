unit TestFS;

INTERFACE
IMPLEMENTATION
USES ServerLoop,Chat,SysUtils,MemStream,NetAddr,opcode,Download,Store1;

type t=object
 job:^tDownloadJob;
 //procedure UserInput
 procedure Periodic;
 procedure Rekt;
 procedure HardTimeout;
end;

procedure t.Periodic;
 begin
 write('TestFS: ',job^.state);
 if job^.state=stError then write(job^.error,'-',job^.error2);
 writeln(' total=',job^.total,' done=',job^.done);
 Shedule(800,@Periodic);
end;

procedure t.Rekt;
 begin
 writeln('TestFS: rekt');
 UnShedule(@HardTimeout);
 UnShedule(@Periodic);
 FreeMem(@self,sizeof(self));
end;

procedure t.HardTimeout;
 begin
 writeln('TestFS: hardtimeout');
 //ch.DisposeHook:=@Rekt;
 //ch.Close;
end;

procedure init;
 var o:^t;
 var oi:word;
 var s:tMemoryStream;
 const opt='-test-fs';
 var fid:tFID;
 begin
 oi:=OptIndex(opt);
 if oi>0 then begin
  assert(OptParamCount(oi)=1,opt+'(rcpt:tNetAddr)');
  writeln('TestFS: rcpt '+paramstr(oi+1));
  new(o); with o^ do begin
   Shedule(20000,@HardTimeout);
   Shedule(20,@Periodic);
   FillChar(fid,sizeof(fid),0);
   job:=NewJob(paramstr(oi+1),fid);
   if job^.state=stStop then job^.Start;
  end;
 end;
end;

BEGIN
 init;
END.