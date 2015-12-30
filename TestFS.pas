unit TestFS;

INTERFACE
IMPLEMENTATION
USES ServerLoop,Chat,SysUtils,MemStream,NetAddr,opcode,Download,Store1,sha1;

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
 if job^.state>=stError then write(job^.error,'-',job^.error2);
 writeln(' total=',job^.total,' done=',job^.done,' miss=',job^.missc);
 if job^.state<>stActive then Rekt else
 Shedule(800,@Periodic);
end;

procedure t.Rekt;
 begin
 writeln('TestFS: rekt');
 Job^.Free;
 UnShedule(@HardTimeout);
 UnShedule(@Periodic);
 FreeMem(@self,sizeof(self));
end;

procedure t.HardTimeout;
 begin
 writeln('TestFS: hardtimeout');
 Rekt;
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
  assert(OptParamCount(oi)=2,opt+'(rcpt:tNetAddr fid:sha1)');
  fid:=tFID(paramstr(oi+2));
  writeln('TestFS: rcpt '+paramstr(oi+1),' ',sha1print(fid));
  new(o); with o^ do begin
   //Shedule(15000,@HardTimeout);
   Shedule(20,@Periodic);
   job:=NewJob(paramstr(oi+1),fid);
   if job^.state=stStop then job^.Start;
  end;
 end;
end;

BEGIN
 init;
END.