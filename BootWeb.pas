UNIT BootWeb2;
INTERFACE

procedure BootWeb(url:AnsiString);

IMPLEMENTATION
USES ServerLoop,AsyncProcess,MemStream,sysutils;

type
 tWBS=object
 async:tAPX;
 procedure EventAsync;
 procedure EventTimeout;
 procedure Init(url:ansistring);
 procedure DoneFreeSelf;
 end;

procedure tWBS.Init(url:ansistring);
 var args:array [0..2] of pchar;
 var buffer:pointer;
 var stream:^tMemoryStream;
 begin
 {todo: create buffer and stream, create arg list, set callback, run}
 args[0]:='httpfetch';
 args[1]:=pchar(url);
 args[2]:=nil;
 GetMem(buffer,1024);
 new(stream);
 stream^.Init(buffer,0,1024);
 async.Init(@args,stream^);
 async.event:=@EventAsync;
 Shedule(10000,@EventTimeout); {10s timeout}
end;

procedure tWBS.EventAsync;
 begin
 UnShedule(@EventTimeout);
 try
 if (async.exitsignal=0)and(async.exitcode=0) then begin
  writeln('async success');
  {parse output}
 end else raise eXception.createfmt('async failed, signal=%U code=%U',[async.exitsignal,async.exitcode]);
 finally
  DoneFreeSelf;
 end;
end;

procedure tWBS.EventTimeout;
 begin
 {timeout is automatically unsheduled}
 async.Kill;
 DoneFreeSelf;
 raise eXception.create('async timeout');
end;

procedure tWBS.DoneFreeSelf;
 begin
 FreeMem(async.output^.base,async.output^.size);
 Dispose(async.output);
 FreeMem(@self,sizeof(self));
end;

procedure BootWeb(url:AnsiString);
 var o:^tWBS;
 begin
 New(O);
 o^.Init(url);
end;

BEGIN
 BootWeb('http://localhost/');
END.