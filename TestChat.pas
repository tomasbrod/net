unit TestChat;

INTERFACE
IMPLEMENTATION
USES ServerLoop,Chat,SysUtils,MemStream,NetAddr;

type t=object
 ch: tChat;
 //procedure UserInput
 procedure Reply(msg:tSMsg; data:boolean);
 procedure Rekt;
 procedure Timeout(willwait:LongWord);
 procedure HardTimeout;
end;

procedure t.Reply(msg:tSMsg; data:boolean);
 var d:string;
 begin
 write('TestChat: ');
 if data then begin
  setlength(d,msg.stream.RDBufLen); 
  msg.stream.read(d[1],msg.stream.RDBufLen);
  writeln('reply '+d);
 end else writeln('ack');
end;

procedure t.Rekt;
 begin
 writeln('TestChat: rekt');
 FreeMem(@self,sizeof(self));
end;

procedure t.Timeout(willwait:LongWord);
 begin
 if willwait>=16000 then begin
  writeln('TestChat: timeout, give up');
  ch.Close;
 end else
 writeln('TestChat: resend willwait='+IntToStr(willwait));
end;

procedure t.HardTimeout;
 begin
 writeln('TestChat: hardtimeout');
 ch.Close;
end;

procedure ChatHandler(var nchat:tChat; msg:tSMsg);
 var d:ansistring;
 var s:tMemoryStream;
 begin
 write('TestChat: ');
 msg.stream.skip(1);
 setlength(d,msg.stream.RDBufLen); 
 msg.stream.read(d[1],msg.stream.RDBufLen);
 writeln('msg '+d);
 //nchat.Ack;
 s.init(GetMem(56),0,56);
 nchat.AddHeaders(s);
 d:='Test Chat Reply!';
 s.Write(d[1],length(d));
 nchat.Send(s);
 nchat.Close;
end;
 

procedure init;
 var o:^t;
 var oi:word;
 var s:tMemoryStream;
 const opt='-test-chat';
 var msg:string[50];
 begin
 oi:=OptIndex(opt);
 if oi>0 then begin
  assert(OptParamCount(oi)=1,opt+'(rcpt:tNetAddr)');
  writeln('TestChat: rcpt '+paramstr(oi+1));
  {init chat to rcpt, send message wait reply, print reply, done}
  new(o); with o^ do begin
   ch.Init(paramstr(oi+1));
   ch.Callback:=@Reply;
   ch.DisposeHook:=@Rekt;
   ch.TMHook:=@Timeout;
   Shedule(7000,@HardTimeout);
   s.init(GetMem(56),0,56);
   ch.AddHeaders(s);
   msg:='Test Chat Message!';
   s.WriteByte(32);
   s.Write(msg[1],length(msg));
   ch.Send(s);
  end;
 end;
 SetChatHandler(32,@ChatHandler);
end;

BEGIN
 init;
END.