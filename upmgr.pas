UNIT UPMGR;
{Upload Manager for brodnetd}

{all file requests in one chat
 -chat
 >FILETRANSFER
 <ACK
 ...
 Close:><CLOSE
}

{mission:
  - read files
  - add demux info
  - add sequence/offset
  - handle retransmit requests
  - handle all requests
  - deprioritize/cancel uploads
 keep one TC connection per peer (+expire-delete)
 => need 'chat' protocol
}

{to download a file:
 >GET channel CHK hash ofs+len (if len>0 start transfer)
 <INFO/FAIL
 <DONE channel
 [
 >SEG channel ofs len
 <SEGOK/fail
 <DONE channel
 ]
 >FIN channel
}

INTERFACE
USES Chat,TC,opcode,ServerLoop,MemStream,NetAddr;

IMPLEMENTATION

type
tAggr_ptr=^tAggr;
tPrv=object
 u:byte;
end;
tPrv_ptr=^tPrv;
tAggr=object
 state: byte; {0=idle}
 tcs: tTCS;
 ch: ^tChat;
 prv: array of tPrv;
 next,prev: tAggr_ptr;
 //procedure OnCont;
 procedure OnMsg(msg: tSMsg; data: boolean);
 procedure Init(var nchat:tChat; msg:tSMsg);
 procedure Done;
 procedure SendTestReply;
end;

var Peers:^tAggr;

procedure tAggr.OnMsg(msg: tSMsg; data: boolean);
 var op:byte;
 begin
 if data then begin
  op:=msg.stream.readbyte;
  case op of
  opcode.upClose: begin
   ch^.Ack;
   Done;
   FreeMem(@self,sizeof(self));
   end;
  99: SendTestReply;
  end{case};
 end{data};
end;

procedure tAggr.SendTestReply;
 var s:tMemoryStream;
 begin
 s.Init(GetMem(56),0,56);
 ch^.AddHeaders(s);
 s.WriteByte(98);
 s.WriteByte(42);
 ch^.send(s);
end;

procedure tAggr.Init(var nchat:tChat; msg:tSMsg);
 begin
 next:=Peers;
 prev:=nil;
 if assigned(Peers) then Peers^.prev:=@self;
 Peers:=@self;
 ch:=@nchat;
 tcs.Init(msg.source^);
 //tcs.CanSend:=@OnCont;
 SetLength(prv,2);
 prv[0].u:=0;
 prv[1].u:=0;
 ch^.Callback:=@OnMsg;
 //ch^.TMHook
 writeln('upmgr: send ack to init');
 ch^.Ack;
 state:=0;
end;

procedure tAggr.Done;
 var s:tMemoryStream;
 begin
 {s.Init(GetMem(56),0,56);
 ch^.AddHeaders(s);
 s.WriteByte(opcode.upClose);
 s.WriteByte(22);
 ch^.send(s);}
 ch^.Close;
 tcs.Done;
 if assigned(prev) then prev^.next:=next else Peers:=next;
 if assigned(next) then next^.prev:=prev;
 state:=$FF;
end;

function FindAggr({const} addr:tNetAddr): tAggr_ptr;
 begin
 result:=Peers;
 while assigned(result) do begin
  if result^.tcs.remote=addr then exit;
  assert(result^.prev=result);
  result:=result^.next;
 end;
end;

procedure ChatHandler(var nchat:tChat; msg:tSMsg);
 var dup:^tAggr;
 begin
 {check dup}
 dup:=FindAggr(msg.source^);
 if assigned(dup) then begin
  Dup^.Done;
 end else begin
  New(dup);
 end;
 writeln('upmgr: init');
 Dup^.Init(nchat,msg);
end;

BEGIN
 SetChatHandler(opcode.upFileServer,@ChatHandler);
END.