unit AsyncProcess;
INTERFACE
USES MemStream;

type tapx=object
 pid:SizeUInt;
 output: ^tMemoryStream;
 exitcode:Integer;
 exitsignal:Word;
 event: procedure of object; {notification on statuc change}
 procedure Init(const args:ppchar; var ioutput:tMemoryStream);
 procedure Kill;
 private
 opipe:tHandle;
 procedure PipeEvent(ev:word);
 end;

IMPLEMENTATION
USES ServerLoop
    ,Sockets
    ,NetAddr
    ,BaseUnix
    ,SysUtils
    ;

procedure IOCHECK(prfx: string);
 var e:eOSError;
 begin
 e:=eOSError.CreateFmt('%S %U',[prfx,fpGetErrno]);
 e.ErrorCode:=fpGetErrno;
 raise e;
end;

procedure tapx.PipeEvent(ev:word);
 var rl:longint;
 var ecode:LongInt;
 begin
 if (ev and POLLIN)>0 then begin
  if output^.WRBufLen=0 then begin
   fpKill(pid,SIGPIPE); {no space left for output, kill}
  end else begin
  rl:=fpRead(opipe,output^.WRBuf,output^.wrbuflen);
  if rl=-1 then IOCHECK('read from pipe');
  output^.wrend(rl);
  //writeln('pipeRead ',rl,' ev=',inttohex(ev,4));
  end;
 end;
 if (ev and POLLHUP)>0 then begin
  {the pipe was broken}
  ServerLoop.WatchFD(opipe,nil);
  fpClose(opipe);
  fpWaitPid(pid,ecode,0); {wait to terminate and remove zombie}
  {this could block it child closed stdout but did not terminate}
  pid:=0;
  if wifexited(ecode) then exitcode:=wexitstatus(ecode);
  if wifsignaled(ecode) then exitsignal:=wtermsig(ecode);
  if assigned(event) then event;
 end;
end;

procedure tapx.Init(const args:ppchar; var ioutput:tMemoryStream);
 var spid:tPID;
 var pipes:tFilDes;
 begin
 output:=@ioutput;
 exitcode:=-1;
 exitsignal:=0;
 pid:=0;
 if fpPipe(pipes)=-1 then IOCHECK('pipe()');
 spid:=fpFork;
 if spid<0 then IOCHECK('fork()');
 if spid>0 then begin
  pid:=spid;
  Opipe:=pipes[0];  {save reading end}
  fpClose(pipes[1]); {close input of pipe}
  ServerLoop.WatchFD(opipe,@PipeEvent);
 end else begin
  fpDup2(pipes[1],1); {stdout:=pipeinput}
  fpClose(0); {stdin:=nil}
  fpClose(pipes[0]);
  fpClose(pipes[1]);
  fpExecv(args[0],args);
  halt(127); {this is another process, cannot really raise here if exec fails}
 end;
end;

procedure tAPX.Kill;
 var ecode:LongInt;
 begin
 ServerLoop.WatchFD(opipe,nil);
 fpClose(opipe);
 fpKill(pid,SIGTERM);
 fpWaitPid(pid,ecode,0);
 pid:=0;
end;

END.