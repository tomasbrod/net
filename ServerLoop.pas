UNIT ServerLoop;

INTERFACE
uses MemStream,NetAddr;

procedure Loop;

type tSMsg=object
 Source: ^tNetAddr;
 Length: {Long}Word;
 Data: pointer;
 stream: tMemoryStream;
 channel: word;
 end;

type tMessageHandler=procedure(msg:tSMsg);

procedure SetOpcodeMsgHandler(OpCode:byte; handler:tMessageHandler);
procedure SetHiMsgHandler(handler:tMessageHandler);

IMPLEMENTATION

USES SysUtils,Sockets,UnixType,BaseUnix
     ;

{aim for most simple implementation, since could be extended anytime}

var s_inet:tSocket;
var pollArr: array [0..0] of tPollFd;

procedure SC(fn:pointer; retval:cint);
 begin
  if retval < 0 then begin
   raise eXception.Create(Format('Socket error %d operation %P',[SocketError,fn]));
  end;
 end;

procedure s_SetupInet;
 var bind_addr:tInetSockAddr;
 begin
  with bind_addr do begin
   family:=AF_INET;
   port:=htons(3511);
   addr:=0; {any}
   s_inet:=fpSocket(family,SOCK_DGRAM,IPPROTO_UDP);
   SC(@fpSocket,s_inet);
  end;
  SC(@fpBind,fpBind(s_inet,@bind_addr,sizeof(bind_addr)));
  with PollArr[0] do begin
   fd:=s_inet;
   events:=pollIN or pollERR;
   revents:=0;
  end;
 end;

var Terminated:boolean=false;

procedure SendMessage(const data; len:word; const rcpt:tSockAddr );
 var rc:Integer;
 begin
 SC(@fpsendto,fpsendto(s_inet,@data,len,0,@rcpt,sizeof(sockaddr_in)));
end;

procedure HandleMsg(s:tSocket; const buffer; len:LongWord; from:tSockAddr);
 begin
  writeln;
  write('HandleMsg: ');
  if len<1 then exit;
  case char((@buffer+0)^) of
   'b':writeln('Brod-Net');
   'd':begin
    writeln('Mainline DHT');
    RPC.SendMessage:=@SendMessage;
    RPC.ReceiveMessage(buffer,len,from);
    end;
   else writeln('Unknown');
  end;
 end;

procedure SignalHandler(sig:cint);CDecl;
 begin
  writeln;
  if terminated then raise eControlC.Create('CtrlC DoubleTap') ;
  Terminated:=true;
  writeln('Shutdown requested');
 end;

procedure Loop;
 var EventsCount:integer;
 var Buffer:array [1..1024] of byte;
 var pkLen:LongWord;
 var From:tSockAddr;
 var FromLen:LongWord;
 begin
  EventsCount:=fpPoll(@PollArr[0],1,5000);
  if eventscount=0 then write('.') else begin
   {INET socket}
   with PollArr[0] do begin
    if (revents and pollIN)>0 then begin
     FromLen:=sizeof(From);
     pkLen:=fprecvfrom(s_inet,@Buffer,sizeof(Buffer),0,@from,@fromlen);
     SC(@fprecvfrom,fromlen);
     HandleMsg(s_inet,Buffer,pkLen,from);
     revents:=0;
    end;
   end;
   {INET6...}
  end;
 end;

BEGIN
 s_setupInet;
 fpSignal(SigInt,@SignalHandler);
 fpSignal(SigTerm,@SignalHandler);
 repeat Loop until Terminated;
 write('Standard terminate [');
 CloseSocket(s_inet);
 writeln(']');
END.