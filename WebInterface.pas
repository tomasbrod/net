{$C+}{$MODE OBJFPC}
uses StrUtils,SysUtils


    ,Sockets
	,sSockets
	,SocketUtil
	,NetAddr
	,CtrlIface
	,Keys
	,Transfer
	,Neighb
	;

var Sock:tSocketStream;
const cRecvWait=2000{ms};

procedure PutLine(s:string);
 begin
 Write(S,#13#10);
end;
procedure Connect;
 var consocket:tSocket;
 var sockaddr:tSockAddrL;
 var DaemonAddr:string;
 var trg:netaddr.t;
 begin
 DaemonAddr:=GetEnvironmentVariable('BRODNETWEB_DAEMONCTRL');
 if DaemonAddr='' then DaemonAddr:='//ip4/127.0.0.1/1031';
 trg:=DaemonAddr;
 trg.ToSocket(sockaddr);
 consocket:=fpSocket(sockaddr.sa_family,Sockets.SOCK_STREAM,0);
 if consocket=-1 then CheckSocket;
 if fpConnect(consocket,@sockaddr,sizeof(sockaddr))=-1 then CheckSocket;
 Sock:=tSocketStream.Create(consocket);
end;

function EnsureConnection:boolean;
 begin
 if assigned(sock) then begin
  result:=true;
  exit;
 end;
 try
  Connect;
  result:=true;
 except on e:exception do begin
  PutLine('HTTP/1.1 500 Error');
  PutLine('Content-Type: text/plain; charset=UTF-8');
  PutLine('');
  Writeln('Can not connect to daemon controll interface ('+e.ClassName+')');
  //DumpExceptionBackTrace(output);
  result:=false;
 end; end;
end;
(*
procedure tApp.EvCommand(command:AnsiString);
 var par:ansistring;
 procedure AddPeer;
  var a:netaddr.t; begin
  a.FromString(par);
  SendCommand(ccAddPeer);
  Sock.WriteBuffer(a,sizeof(a));
 end;
 procedure CmdTransferRequestProgress(progress:boolean); unimplemented;
  var fid:Transfer.tFID;
  var a:netaddr.t;
  begin
  fid.FromString(Copy2SpaceDel(par));
  a.FromString(par);
  SendCommand(ccTransferRequest);
  Sock.WriteBuffer(fid,sizeof(fid));
  Sock.WriteBuffer(a,sizeof(a));
  if progress then begin
   SendCommand(ccTransferProgress);
   Sock.WriteBuffer(fid,sizeof(fid));
  end;
 end;
 procedure CmdFID(cmd:byte);
  var fid:Transfer.tFID;
  begin
  fid.FromString(par);
  SendCommand(cmd);
  Sock.WriteBuffer(fid,sizeof(fid));
 end;
 procedure CmdPID(cmd:byte);
  var pid:Neighb.tPID;
  begin
  pid.FromString(par);
  SendCommand(cmd);
  Sock.WriteBuffer(pid,sizeof(pid));
 end;
 var sp:word;
 begin
 sp:=pos(' ',command); if sp=0 then par:='' else begin 
 par:=copy(command,sp+1,1000); command:=copy(command,1,sp-1); end;
 case command of
  '':;
  'q': Terminate;
  'stop': SendCommand(ccTerminate);
  'peerstates': SendCommand(ccPeerStates);
  'addpeer': AddPeer;
  'TransReq': CmdTransferRequestProgress(true);
  'TransReqq': CmdTransferRequestProgress(false);
  'TransAbort': CmdFID(ccTransferAbort);
  'TransStat': CmdFID(ccTransferProgress);
  'TransList': SendCommand(ccTransferListAll);
  'NeighbList': SendCommand(ccGetNeighb);
  'Route','NeighbPID': CmdPID(ccGetNeighbPid);
  //'NeighbAddr': CmdAddr(ccGetNeighbAddr);
  else writeln('Invalid command');
 end;
end;

procedure tApp.EvEvent(event:byte);
 var tmp:byte;
 var tmw:word2;
 var tm4:word4;
 procedure PrintAddr;
  var a:netaddr.t;
  begin
  Sock.ReadBuffer(a,sizeof(a));
  write(string(a));
 end;
 procedure DumpID; unimplemented;
  var a:array [1..20] of byte{Keys.tID};
  begin
  Sock.ReadBuffer(a,sizeof(a));
  {write(string(a));}
 end;
 procedure DumpFID; unimplemented;
  var fid:Transfer.tFID;
  begin
  Sock.ReadBuffer(fid,sizeof(fid));
  write(string(fid));
 end;
 begin
 case event of
  ceQuit: begin
   write('Daemon terminating');;write(' ');
   tmp:=Sock.ReadByte;
   case tmp of
    0: write('by request');
    1: write('crashed');
    else write('reason=',tmp);
  end; end;
  ceInvalid: write('Invalid command');
  cePeerState: begin
   write('Peer');write(' ');
   tmp:=Sock.ReadByte;
   case tmp of
    0: write('ping');
    1: write('NEW!');
    2: write('REKT');
    else write('ev=',tmp);
   end;;write(' ');
   PrintAddr;;write(' ');
   Sock.ReadBuffer(tmw,2);
   write('delta=',word(tmw),'ms');
  end;
  ceTransfer: begin
   write('Transfer');write(' ');
   DumpFID;write(': ');
   Sock.ReadBuffer(tm4,4);
   write(longword(tm4));
   Sock.ReadBuffer(tm4,4);
   write('/',longword(tm4),'b');
  end;
  255: begin write('Disconnected!'); Terminate; end;
  else write('?'+IntToStr(event));
 end;
end;
*)
procedure HandleUnknownEvent(event:byte);
 var tmp:byte;
 begin
 case event of
  ceQuit: begin
   write('Daemon terminating');write(' ');
   tmp:=Sock.ReadByte;
   case tmp of
    0: write('by request');
    1: write('crashed');
    else write('reason=',tmp);
  end; end;
  ceInvalid: write('Invalid command');
  else write('?'+IntToStr(event));
 end;
 writeln;
 flush(output);
 raise exception.create('Unexpected message from daemon');
end;

var Terminated:boolean;

procedure GetLine(out s:ansistring);
 begin
 Readln(S);
 if length(s)=0 then exit;
 if s[length(s)]=#10 then SetLength(s,length(s)-1);
end;

procedure CopyHeader(const header:string; name:string; var value:string);
 begin
 if (Copy(header,1,length(name))=name) and (copy(header,length(name)+1,2)=': ') then
  value:=copy(header,length(name)+3,9999);
end;

procedure CopyQuery(const query:string; name:string; var value:string);
 var p1:longint;
 var after:string;
 begin
 p1:=pos(name+'=',query);
 p1:=p1+length(name)+1;
 after:=ExtractSubstr(query,p1,['&']);
 value:='';
 p1:=1; while p1<=length(after) do begin
  case after[p1] of
   '+': value:=value+' ';
   '%': begin
    try value:=value+ Char(StrToInt('$'+Copy(after,p1+1,2))); except end;
    inc(p1,2);
    end;
   else value:=value+after[p1];
  end;
 inc(p1);
 end;
end;

var Keepalive:boolean=true;

procedure ServeAsset(path:string);
 var f:file of byte;
 var r:longint;
 var ctype:string;
 var buf:string[255];
 var readed:Cardinal;
 begin
 Terminated:=not Keepalive;
 if path='' then path:='index.htm';
 case UpCase(ExtractFileExt(path)) of
  '.TXT': ctype:='text/plain';
  '.HTM': ctype:='text/html';
  '.CSS': ctype:='text/css';
  '.PNG': ctype:='image/png';
  else ctype:='';
 end;
 if copy(ctype,1,5)='text/' then ctype:=ctype+'; charset=UTF-8';
 if (pos('..',path)=0)and(path[1]<>'/') then begin
  Assign(f,path);
  {$I-}
  Reset(f);
  {$I+}
  r:=IOResult;
 end else begin
  r:=5;
 end;
 case r of
  0: begin
   PutLine('HTTP/1.1 200 Ok');
   if not Terminated then PutLine('Connection: Keep-Alive');
   PutLine('Content-Length: '+IntToStr(FileSize(f)));
   if Length(ctype)>0 then PutLine('Content-Type: '+ctype);
   PutLine('Expires: Fri, 30 Oct 2024 14:19:41 GMT');
   PutLine('');
   Flush(output);
   repeat
    BlockRead(f,buf[1],255,readed);
    SetLength(buf,readed);
    write(buf);
   until readed<255;
   close(f);
  end;
  2: begin
   PutLine('HTTP/1.1 404 Not Found');
   PutLine('Content-Type: text/plain');
   PutLine('');
   PutLine('Asset not found');
   terminated:=true;
  end;
  else begin
   PutLine('HTTP/1.1 500 Internal Error');
   PutLine('Content-Type: text/plain');
   PutLine('');
   PutLine('Error opening file: '+IntToStr(r));
   terminated:=true;
  end;
 end;
end;

procedure PutBasicOkHeader(ct:string; cache:boolean);
 begin
  PutLine('HTTP/1.1 200 OK');
  PutLine('Content-Type: '+ct+'; charset=UTF-8');
  if not cache then PutLine('Expires: Fri, 30 Oct 1998 14:19:41 GMT');
end;


procedure ServeNeighb(const module,path,extra:string);
 var event:byte;
 var i:tNeighbInfo;
 begin
 case path of
  '': ServeAsset('neighbIndex.htm');
  'list': begin
   if not EnsureConnection then exit;
   PutBasicOKHeader('text/plain',false);
   PutLine('');
   writeln('The Neighbours');writeln;
   Sock.WriteByte(ccGetNeighb);
   repeat
    event:=Sock.ReadByte;
    case event of
     ceNeighbs: begin
      Sock.ReadBuffer(i,sizeof(i));
      writeln(String(i.pid)+' via '+String(i.addr)+' +'+IntToStr(word(i.hop)));
     end;
     ceNeighbsEnd: write('no more');
     else HandleUnknownEvent(event);
    end;
   until event=ceNeighbsEnd;
  end;
 end;
end;

procedure redirect(trg:string);
 begin
 PutLine('HTTP/1.1 302 Found');
 PutLine('Location: '+trg);
 PutLine('Content-Length: 0');
 PutLine(''); Terminated:=false;
end;

procedure ServePeers(const module,path,extra:string);
 var addrstr:string;
 var addr:netaddr.t;
 begin
 case path of
  '': ServeAsset('peersIndex.htm');
  'add': begin
   if not EnsureConnection then exit;
   CopyQuery(extra,'addr',addrstr);
   PutBasicOKHeader('text/plain',false);
   PutLine('');
   Writeln('Add peer '+addrstr);
   try addr:=addrstr; except on eConvertError do begin
    Writeln('Convert Error!');
    exit;
   end; end;
   Sock.WriteByte(ccAddPeer);
   Sock.WriteBuffer(addr,sizeof(addr));
  end;
 end;
end;

procedure CmdStop;
 var event:byte;
 begin
 if not EnsureConnection then exit;
 Sock.WriteByte(ccTerminate);
  PutBasicOKHeader('text/plain',false);
  PutLine('');
  writeln('Requested daemon to terminate.');
  event:=Sock.ReadByte;
  try HandleUnknownEvent(event); except end;
end;

procedure Server;
 var LB,query,Module,Path:ansistring;
 var Method,Proto:string;
 var HV:string;
 var AcceptLanguage:string='';
 var Connection:string='';
 begin
 GetLine(LB); if LB='' then exit;
 method:=Copy2SpaceDel(LB);
 query:=Copy2SpaceDel(LB);
 proto:=Copy2SpaceDel(LB);
 query:=Copy(query,2,9999);
 path:=Copy2SymbDel(query,'?');
 module:=Copy2SymbDel(path,'/');
 repeat GetLine(LB); if LB='' then break;
  CopyHeader(LB,'Accept-Language',AcceptLanguage);
  CopyHeader(LB,'Connection',Connection);
 until false;
 if Connection<>'Keep-Alive' then Keepalive:=false;
 if Method='HEAD' then begin
  PutLine('HTTP/1.1 405 Method Not Allowed');
  PutLine('Allow: GET');
  PutLine('Content-Length: 0');
  PutLine('Connection: Keep-Alive');Terminated:=false;
  PutLine('');
  exit;
 end;
 case module of
  '','asset': begin
   Terminated:=not Keepalive;
   ServeAsset(path);
   end;
  'neighb': ServeNeighb(module,path,query);
  'peers': ServePeers(module,path,query);
  'stop': CmdStop;
  
  else begin
   PutLine('HTTP/1.1 400 Bad Request');
   PutLine('');
   PutLine('Hello, World!');
   PutLine('Method='+Method+' proto='+proto);
   PutLine('Module='+Module+' path='+path+' extra="'+query+'"');
   PutLine('lang: '+AcceptLanguage);
   PutLine('con: '+Connection);
   PutLine('Keepalive='+IfThen(keepalive,'true','false'));
  end;
 end;
 Flush(output);
end;

BEGIN
 SetCurrentDir(GetEnvironmentVariable('BRODNETWEB_DATA'));
 Sock:=nil;
 try
  repeat
   terminated:=true;
   Server;
   flush(output);
  until terminated or eof;
 finally
  if assigned(Sock) then Sock.WriteByte(ccQuit);
  FreeAndNil(Sock);
 end;
end.