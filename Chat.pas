unit Chat;
{
 Implement two-way realiable acked lock-step protocol
}
INTERFACE
uses NetAddr,ServerLoop,MemStream;

type tChat=object
 remote:tNetAddr;
 opcode:byte;
 txSeq:Word;
 rxSeq:Word;
 rxAcked:boolean;
 closed:boolean;
 RTT:LongWord;{in ms}
 Callback: procedure(msg:tSMsg; data:boolean) of object; {client must maintain active chats}
 OnTimeout: procedure of object;
 OnDispose: procedure of object;
 procedure Init(const iremote:tNetAddr);
 procedure SetTimeout(acktm,repltm:LongInt);
 procedure AddHeaders(var s:tMemoryStream);
 procedure StreamInit(var s:tMemoryStream; l:word);
 procedure Send(s:tMemoryStream);
  {the stream can be invalidated, but the buffer must not be modified or freed}
 procedure Ack;
 procedure Close;
 private
 txPk:pointer; txLen:word; {last sent, not acked msg}
 txTime:tDateTime;
 tmAck,tmReply:LongWord;{ms}
 procedure InitFrom(const iremote:tNetAddr; iopcode:byte);
 procedure Done;
 procedure Resend;
 procedure OnReply(msg:tSMsg);
 procedure ReplyTimeout;
end;

type tChatHandler=procedure(var nchat:tChat; msg:tSMsg);
procedure SetChatHandler(initcode:byte; handler:tChatHandler);

{ download manager create FileRequest
 File Request open chat session to server
 upmgr accepts chat and send reply
 FileRequest acks, chat is then closed after TimeWait
 upmgr starts TC transfer
 transfer finished, upmgr send new Chat to FileRequest
 FileRequest acks, chat is closed on both ends
 FileRequest can open new chat if blocks are missing

 => chat msgs must be created with New, disposed by Chat
 => there is TimeWait, no references are to the Chat, except Sheduler, it Disposes itself.
}

{ Chats are the HiMsg. Use hash table from ServerLoop, works for HiMsg too. }

IMPLEMENTATION
uses SysUtils;
procedure tChat.Init(const iremote:tNetAddr);
 begin
 remote:=iremote;
 opcode:=128+Random(128); {$warning possible overflow}
 while ServerLoop.IsMsgHandled(opcode,remote) do inc(opcode);
 InitFrom(remote,opcode);
end;
procedure tChat.InitFrom(const iremote:tNetAddr; iopcode:byte);
 begin
 remote:=iremote;
 opcode:=iopcode;
 SetMsgHandler(opcode,remote,@OnReply);
 txSeq:=0;
 rxSeq:=0;
 rxAcked:=true; {to not ack pk 0}
 closed:=false;
 txPk:=nil;
 txLen:=0;
 Callback:=nil;
 OnTimeout:=nil;
 OnDispose:=nil;
 RTT:=200; {a default for timeouts}
 txTime:=0;
 tmAck:=0;
 tmReply:=0;
end;
{struct
 opcode:byte
 seq:2
 ack_seq:2
 data:xx
}

procedure tCHat.AddHeaders(var s:tMemoryStream);
 begin s.skip(5) end;
procedure tChat.StreamInit(var s:tMemoryStream; l:word);
 begin
 s.Init(GetMem(l+5),0,l+5);
 AddHeaders(s);
end;
procedure tChat.SetTimeout(acktm,repltm:LongInt);
 begin
 assert(assigned(OnTimeout));
 tmAck:=acktm;
 tmReply:=repltm;
end;

procedure tChat.Send(s:tMemoryStream);
 begin
 if txLen>0 then begin
  FreeMem(txPk,txLen);
  UnShedule(@Resend);
 end;
 //assert(assigned(callback));
 Inc(txSeq);
 s.Seek(0);
 s.WriteByte(opcode);
 s.WriteWord(txSeq,2);
 if not rxAcked then begin
  s.WriteWord(rxSeq,2);
  rxAcked:=true;
 end else s.WriteWord(0,2);
 txPk:=s.base;
 txLen:=s.Length;
 ServerLoop.SendMessage(txPk^,txLen,remote);
 ServerLoop.Shedule(RTT*2,@Resend);
 txTime:=Now;
end;

procedure tChat.Ack;
 var s:tMemoryStream;
 begin
 if not rxAcked then begin
  s.Init(GetMem(5),0,5);
  s.WriteByte(opcode);
  s.WriteWord(0,2);
  s.WriteWord(rxSeq,2);
  ServerLoop.SendMessage(s.base^,s.length,remote);
  FreeMem(s.base,s.length);
  rxAcked:=true;
  if assigned(OnTimeout) and (tmReply>0) then Shedule(tmReply,@ReplyTimeout);
 end;
end;

procedure tChat.Close;
 begin
 assert(not closed);
 Ack;
 closed:=true;
 callback:=nil; {avoid calling}
 ontimeout:=nil;
 UnShedule(@ReplyTimeout); {fuck it}
 //writeln('Chat: closing');
 if txLen=0 {no packets in flight} then begin
  Shedule(5000{todo},@Done); {wait for something lost}
 end;
end;

procedure tChat.Done;
 begin
 if txLen>0 then FreeMem(txPk,txLen);
 SetMsgHandler(opcode,remote,nil);
 UnShedule(@Resend);
 UnShedule(@ReplyTimeout);
 if assigned(OnDispose) then OnDispose
 else FreeMem(@self,sizeof(self));
 //writeln('Chat: closed');
end;

procedure tChat.Resend;
 {timeout waiting for ack}
 begin
 {check for timeout and closed}
 if RTT<1 then RTT:=2; RTT:=RTT*2;
 if closed and (RTT>5000) then begin
  Done;
  exit
 end;
 if (not closed) and (tmAck>0) and (RTT>tmAck) then begin
  if assigned(ontimeout) then OnTimeout;
  Done;
  exit
 end;
 {resend}
 //writeln('Chat: retry');
 ServerLoop.SendMessage(txPk^,txLen,remote);
 txTime:=Now;
 {reshedule}
 ServerLoop.Shedule(RTT,@Resend);
end;

procedure tChat.OnReply(msg:tSMsg);
 var seq,aseq:Word;
 var s:tMemoryStream;
 begin
 msg.stream.skip(1{opcode});
 seq:=msg.stream.ReadWord(2);
 aseq:=msg.stream.ReadWord(2);
 if aseq>0 then {ack of our msg} begin
  if (aseq=txSeq)and(txLen>0) {it is current} then begin
   if txTime>0 then RTT:=Round((Now-txTime)*MsecsPerDay);
   FreeMem(txPk,txLen);
   UnShedule(@Resend);
   if Closed then Shedule(5,@Done);{wtf?}
   TxLen:=0;
   txPk:=nil;
   if assigned(callback) then callback(msg,false);
   if assigned(OnTimeout) and (tmReply>0) then Shedule(tmReply,@ReplyTimeout);
  end else {write(' old-ack')it is ack of old data, do nothing};
 end;
 if seq>0 then {some data} begin
  if seq<=rxSeq then {remote didnt get our ack} begin
   s.Init(GetMem(5),0,5);
   s.WriteByte(opcode);
   s.WriteWord(0,2);
   s.WriteWord(rxSeq,2);
   ServerLoop.SendMessage(s.base^,s.length,remote);
   FreeMem(s.base,s.length);
   if seq=rxSeq then rxacked:=true;
  end else begin
   {some useful data!}
   rxSeq:=seq;
   rxAcked:=false;
   UnShedule(@ReplyTimeout);
   if assigned(callback) then callback(msg,true);
  end;
 end;
end;

procedure tChat.ReplyTimeout;
 begin
 assert(assigned(OnTimeout));
 OnTimeout;
 {...}
end;

var ChatHandlers: array [1..32] of tChatHandler;

procedure SetChatHandler(initcode:byte; handler:tChatHandler);
 begin
 assert(ChatHandlers[initcode]=nil);
 ChatHandlers[initcode]:=handler;
end;

procedure OnHiMsg(msg:tSMsg);
 {new chat was received!}
 var opcode:byte;
 var seq,aseq:word;
 var hnd:tChatHandler;
 var nchat:^tChat;
 var ix:byte;
 begin
 opcode:=msg.stream.ReadByte;
 assert(not IsMsgHandled(opcode,msg.source^));
 seq:=msg.stream.ReadWord(2);
 aseq:=msg.stream.ReadWord(2);
 if (seq<>1)or(aseq>0) then exit; {invalid initial state}
 ix:=msg.stream.ReadByte;
 if (ix<1)or(ix>high(ChatHandlers)) then exit;
 hnd:=ChatHandlers[ix];
 if not assigned(hnd) then raise eXception.Create('No handler for initcode '+IntToStr(ix));
 msg.stream.seek(msg.stream.position-1);{unskip the initcode}
 nchat:=GetMem(sizeof(tChat));
 nchat^.InitFrom(msg.Source^,opcode);
 nchat^.rxacked:=false;
 nchat^.rxSeq:=1;
 hnd(nchat^,msg);
end;

BEGIN
 FillChar(ChatHandlers,sizeof(chathandlers),0);
 ServerLoop.SetHiMsgHandler(@OnHiMsg);
END.
