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
 callback: procedure(msg:tSMsg; data:boolean) of object; {client must maintain active chats}
 TMhook  : procedure(willwait:LongWord      ) of object;
 DisposeHook: procedure of object; {called instead of freeing self}
 procedure Init(const iremote:tNetAddr);
 procedure AddHeaders(var s:tMemoryStream);
 procedure StreamInit(var s:tMemoryStream; l:word);
 procedure Send(s:tMemoryStream);
  {the stream can be invalidated, but the buffer must not be modified or freed}
 procedure Ack;
 procedure Close;
 private
 txPk:pointer; txLen:word; {last sent, not acked msg}
 txTime:tDateTime;
 procedure InitFrom(const iremote:tNetAddr; iopcode:byte);
 procedure Done;
 procedure Resend;
 procedure OnReply(msg:tSMsg);
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
 opcode:=128;
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
 callback:=nil;
 TMhook:=nil;
 DisposeHook:=nil;
 RTT:=200; {a default for timeouts}
 txTime:=0;
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

procedure tChat.Send(s:tMemoryStream);
 begin
 assert(txLen=0);
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
 end;
end;

procedure tChat.Close;
 begin
 assert(not closed);
 Ack;
 closed:=true;
 //writeln('Chat: closing');
 if txLen=0 {no packets in flight} then begin
  Shedule(15000{todo},@Done); {wait for something lost}
  callback:=nil; {avoid calling}
  tmhook:=nil;
 end;
end;

procedure tChat.Done;
 begin
 {called from sheduler, Done is unsheduled, Resend is not sheduled since ack was received when Done was sheduled}
 if txLen>0 then FreeMem(txPk,txLen);
 SetMsgHandler(opcode,remote,nil);
 if assigned(DisposeHook) then DisposeHook
 else FreeMem(@self,sizeof(self));
 //writeln('Chat: closed');
end;

procedure tChat.Resend;
 {timeout waiting for ack}
 begin
 {resend and reshedule}
 if txLen=0 then exit;
 txTime:=0;
 if RTT<1 then RTT:=2;
 RTT:=RTT*2;
 if assigned(TMhook) and (not closed) then begin
  TMhook(RTT);
  if closed then begin
   Done; {if hook decided to close then abort}
   exit;
  end;
 end;
 if closed and (RTT<400) then RTT:=400;
 if (RTT>=5000) and closed then begin
  Done {give up}
 end else begin
  {finally resend the msg}
  //writeln('Chat: retry');
  ServerLoop.SendMessage(txPk^,txLen,remote);
  ServerLoop.Shedule(RTT,@Resend);
 end;
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
   TxLen:=0;
   txPk:=nil;
   if assigned(callback) then callback(msg,false);
   ServerLoop.UnShedule(@Resend);
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
   if assigned(callback) then callback(msg,true);
  end;
 end;
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