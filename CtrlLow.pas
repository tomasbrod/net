UNIT CtrlLow;
{Remote controll of brodnetd, server part}
INTERFACE
USES Sockets,MemStream
    ;
type tClient=object
 s:tSocket;
 procedure Init(i_s:tSocket);
 procedure Done;
 procedure Event(ev:word);
 procedure Reply(msg:tMemoryStream);
end;
type tMethodProc=procedure(var client:tClient; msg:tMemoryStream);
type tMethod=object
 ptr:tMethodProc;
 len:word;
 procedure Init(proc:tMethodProc; argl:word);
end;
{change this if Ctrl fails to compile}
var methods:array [0..32] of tMethod;
IMPLEMENTATION
USES ServerLoop
    ,BaseUnix
    ,SysUtils
;
type tServer=object
 listen:tSocket;
 procedure init;
 procedure ListenEvent(ev:word);
 procedure InitUnix;
 procedure InitInet(port:word);
end;

procedure tServer.InitUnix;
 var addr:Sockets.sockaddr_un;
 begin
 addr.sun_family:=AF_UNIX;
 addr.sun_path:='ctrl.sock'; //automagically converted to c-string
 fpUnlink(@addr.sun_path);
 listen:=fpSocket(addr.sun_family,SOCK_STREAM,0);
 SC(@fpSocket,listen);
 SC(@fpBind,fpBind(listen,@addr,sizeof(addr)));
end;
procedure tServer.InitInet(port:word);
 var addr:Sockets.sockaddr_in;
 begin
 addr.sin_family:=AF_INET;
 addr.sin_addr.s_bytes[1]:=127;
 addr.sin_addr.s_bytes[2]:=0;
 addr.sin_addr.s_bytes[3]:=0;
 addr.sin_addr.s_bytes[4]:=1;
 addr.sin_port:=htons(port);
 listen:=fpSocket(addr.sin_family,SOCK_STREAM,0);
 SC(@fpSocket,listen);
 SC(@fpBind,fpBind(listen,@addr,sizeof(addr)));
end;
procedure tServer.Init;
 var oi:byte;
 begin
 oi:=OptIndex('-ctrl-port');
 if oi=0 then InitUnix
 else begin
  assert(OptParamCount(oi)=1);
  InitInet(StrToInt(paramstr(oi+1)));
 end;
 SC(@fpListen,fpListen(listen,8));
 ServerLoop.WatchFD(listen,@ListenEvent);
end;

procedure tServer.ListenEvent(ev:word);
 var addr:Sockets.sockaddr_un;
 var addrl:tSockLen;
 var s:tSocket;
 var cl:^tClient;
 begin
 addrl:=sizeof(addr);
 s:=fpAccept(listen,@addr,@addrl);
 if s<0 then begin
  writeln('Controll: accept error'); exit end;
 writeln('Controll: accept');
 New(cl);
 cl^.Init(s);
end;

procedure tClient.Init(i_s:tSocket);
 begin
 s:=i_s;
 ServerLoop.WatchFD(s,@Event);
end;

procedure tClient.Done;
 begin
 ServerLoop.WatchFD(s,nil);
 fpClose(s);
 FreeMem(@self,sizeof(self));
end;

procedure ReadBuf(s:tSocket; dst:pointer; l:longWord; var rc:LongInt);
 var rd:LongInt;
 begin
 if rc<>1 then exit;
 assert(l>0);
 repeat
  rd:=fpRecv(s,dst,l,0);
  if rd<=0 then break;
  dst:=dst+rd;
  l:=l-rd;
 until l=0;
 if rd<1 then rc:=rd;
end;

procedure tClient.Event(ev:word);
 var hdr:record
         method:Word;
         length:Word;
         end;
 var arg:pointer;
 var rc:LongInt;
 var msg_stream:tMemoryStream;
 const cBuf=256;
 begin
 rc:=1;
 arg:=nil;
 {read header, read arguments, exec}
 ReadBuf(s,@hdr,sizeof(hdr),rc);
 hdr.method:=ntohs(hdr.method);
 hdr.length:=ntohs(hdr.length);
 if rc=1 then
 if (hdr.method>high(methods))or(hdr.method<low(methods)) then rc:=-4 else
 if hdr.length<>methods[hdr.method].len then rc:=-4;
 if rc=1 then arg:=GetMem(cBuf);
 if (hdr.length>0)and(rc=1) then begin
  {arg:=GetMem(hdr.length);}
  ReadBuf(s,arg,hdr.length,rc);
  if rc<>1 then FreeMem(arg,hdr.length);
 end;
 if rc<>1 then begin
  case rc of
   -1:writeln('Controll: recv failed ',rc);
   0 :writeln('Controll: end');
   -4:writeln('Controll: method ',hdr.method,'+',hdr.length,' not supported');
   else writeln('Controll: error');
  end;
  Done;
 exit end;
 msg_stream.Init(arg,hdr.length,cBuf);
 Writeln('Controll: method=',hdr.method,' arglen=',hdr.length);
 methods[hdr.method].ptr(self,msg_stream);
 if hdr.length>0 then FreeMem(arg,hdr.length);
end;

procedure tClient.Reply(msg:tMemoryStream);
 var re:LongInt;
 var szw:word;
 begin
 msg.Seek(0);
 szw:=htons(msg.length);
 fpSend(s,@szw,1,0);
 fpSend(s,pointer(@szw)+1,1,0);
 while msg.RdBufLen>0 do begin
  re:=fpSend(s,msg.RdBuf,msg.RdBufLen,0);
  if re<0 then AbstractError;
  msg.RdEnd(re);
 end;
end;

procedure tMethod.Init(proc:tMethodProc; argl:word);
 begin
 ptr:=proc;
 len:=argl;
end;

var Server1:tServer;
BEGIN
 Server1.Init;
END.