{INCLUDE FILE}
{***Low-Level Part***}

type tMethodProc=procedure(var client:tClient; var a,r:tMemoryStream);
type tMethod=object
 ptr:tMethodProc;
 len:word;
 procedure Init(proc:tMethodProc; argl:word);
end;
{change this if Ctrl fails to compile}
var methods:array [0..32] of tMethod;
type tServer=object
 listen:tSocket;
 prevot:procedure;
 procedure init;
 procedure ListenEvent(ev:word);
 procedure InitUnix;
 procedure InitInet(port:word);
end;
var Server1:tServer;

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
procedure OnTerminate;
  begin
  fpUnlink('ctrl.sock');
  if assigned(Server1.PrevOT) then Server1.PrevOT;
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
 PrevOT:=ServerLoop.OnTerminate;
 ServerLoop.OnTerminate:=@OnTerminate;
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
  writeln('CtrlLow.ListenEvent: accept error'); exit end;
 {writeln('CtrlLow.ListenEvent: accept');}
 New(cl);
 cl^.Init(s);
  SHA256_Buffer(cl^.hash,20,addr,addrl);
end;

procedure tClient.Init(i_s:tSocket);
 begin
 s:=i_s;
 error:=false;
 ServerLoop.WatchFD(s,@Event);
 {$ifdef ctlStore}
 SndObj:=nil;
 SndObjLeft:=0;
 RcvObjLeft:=0;
 {$endif}
 Init2;
end;

procedure tClient.Done;
 begin
 Int;
 if assigned(SndObj) then Dispose(SndObj,Done);
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
 if l<>0 then rc:=rd;
end;

procedure tClient.Event(ev:word);
 var hdr:record
         length:Word;
         method:Word;
         end;
 var arg:pointer;
 var rc,sndc:LongInt;
 var msg_stream,resp_stream:tMemoryStream;
 const cBuf=2048;
 begin
 rc:=1;
 arg:=nil;
 if (ev and POLLOUT)>0 then begin
    {$ifdef ctlStore}
    if SndObjLeft=0 then begin
      {$endif}
      ServerLoop.WatchFD(s,nil);
      ServerLoop.WatchFD(s,@Event);
      Dispose(SndObj,Done);
      SndObj:=nil;
      {$ifdef ctlStore}
    end else begin
      arg:=GetMem(cBuf);
      rc:=SndObjLeft;
      if rc>cBuf then rc:=cBuf;
      SndObj^.Read(arg^,rc);
      sndc:=fpSend(s,arg,rc,0);
      if sndc<=0 then error:=true
      else if sndc<rc
      then SndObj^.Skip(sndc-rc);
      SndObjLeft:=SndObjLeft-sndc;
      FreeMem(arg,cBuf);
    end;
    {$endif}
 end;
  if (ev and (POLLIN or POLLHUP or POLLERR))>0 then begin
    {$ifdef ctlStore}
    if RcvObjLeft>0 then begin
      arg:=GetMem(cBuf);
      rc:=cBuf; if rc>RcvObjLeft then rc:=RcvObjLeft;
      rc:=fpRecv(s,arg,rc,0);
      if rc>0 then begin
        RcvObj^.Write(arg^,rc);
        RcvObjHctx.Update(arg^,rc);
        RcvObjLeft:=RcvObjLeft-rc;
        if RcvObjLeft=0 then RcvObjComplete;
        rc:=1;
      end;
    end else begin
    {$endif}
      {read header, read arguments, exec}
      hdr.length:=0;
      ReadBuf(s,@hdr,sizeof(hdr),rc);
      if rc=1 then begin
        hdr.method:=ntohs(hdr.method);
        hdr.length:=ntohs(hdr.length);
        if (hdr.method>high(methods))(*or(hdr.method<low(methods))*)or(not assigned(methods[hdr.method].ptr)) then rc:=-4 else
        if (methods[hdr.method].len=0)and(hdr.length>0) then rc:=-4;
        if hdr.length<methods[hdr.method].len then rc:=-4; {range check error? here?}
        if hdr.length>cBuf then rc:=-4;
      end;
      if (hdr.length>0)and(rc=1) then begin
        arg:=GetMem(hdr.length);
        ReadBuf(s,arg,hdr.length,rc);
      end;
      if rc=1 then begin
        msg_stream.Init(arg,hdr.length,cBuf);
        resp_stream.Init(cBuf);
        resp_stream.Write(arg,2);
        //Writeln('CtrlLow.Event: method=',hdr.method,' arglen=',hdr.length);
        Int;
        methods[hdr.method].ptr(self,msg_stream,resp_stream);
        if not error then begin
          resp_stream.Seek(0);
          resp_stream.WriteWord2(resp_stream.Length-2);
          self.SendTo(resp_stream); resp_stream.Free;
        end;
      end;
      if hdr.length>0 then FreeMem(arg,hdr.length);
    {$ifdef ctlStore}
    end;
    {$endif}
    if rc<>1 then begin
      case rc of
        -1:writeln('CtrlLow.Event: recv failed ',rc);
        0 :{writeln('CtrlLow.Event: end')};
        -4:writeln('CtrlLow.Event: method ',hdr.method,'+',hdr.length,' not supported');
        else writeln('CtrlLow.Event: error ',rc);
      end;
      error:=true;
    end;
 end;
 if error then Done;
end;

procedure tClient.SendTo(msg:tMemoryStream);
 var re:LongInt;
 begin
 try
 msg.Seek(0);
 while msg.RdBufLen>0 do begin
  re:=fpSend(s,msg.RdBuf,msg.RdBufLen,0);
  if re<0 then SC(@fpSend,re-1);
  msg.RdEnd(re);
 end;
 except {ignore exceptions}
 end;
end;

{$ifdef ctlStore}
procedure tClient.SendObject(var o:tStoreObject; ilen:LongWord);
  begin
  SndObjLeft:=ilen;
  SndObj:=@o;
  ServerLoop.WatchFD(s,nil);
  ServerLoop.WatchFDRW(s,@Event);
end;
{$endif}

procedure tMethod.Init(proc:tMethodProc; argl:word);
 begin
 assert(ptr=nil);
 ptr:=proc;
 len:=argl;
end;

