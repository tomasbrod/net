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
 txPk:pointer; txLen:word; {last sent, not acked msg}
 RTT:LongWord;{in ms}
 callback: procedure(msg:tSMsg;data:boolean) of object; {client must maintain active chats}
 TMhook: procedure(willwait:LongWord) of object;
 procedure Init(const iremote:tNetAddr);
 procedure AddHeaders(var s:tMemoryStream);
 procedure Send(s:tMemoryStream);
  {the stream can be invalidated, but the buffer must not be modified or freed}
 procedure Ack;
 procedure Close;
 private
 txTime:tDateTime;
 procedure InitFrom(const iremote:tNetAddr; iopcode:byte);
 procedure Done;
 procedure Resend;
 procedure OnReply(msg:tSMsg);
end;
type tChatHandler=procedure(var nchat:tChat; msg:tSMsg);

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
 txSeq:=0;
 rxSeq:=0;
 rxAcked:=true; {to not ack pk 0}
 closed:=false;
 txPk:=nil;
 txLen:=0;
 callback:=nil;
 TMhook:=nil;
 RTT:=500; {a default for timeouts}
 txTime:=0;
end;
{struct
 opcode:byte
 seq:2
 ack_seq:2
 data:xx
}

procedure tChat.Send(s:tMemoryStream);
 begin
 assert(txLen=0);
 assert(assigned(callback));
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
 closed:=true;
 if txLen=0 {no packets in flight} then begin
  Shedule(3000{todo},@Done); {wait for something lost}
  callback:=nil; {avoid calling}
 end;
end;

procedure tChat.Done;
 begin
 {called from sheduler, Done is unsheduled, Resend is not sheduled since ack was received when Done was sheduled}
 FreeMem(txPk,txLen);
 SetMsgHandler(opcode,remote,nil);
 FreeMem(@self,sizeof(self));
end;

procedure tChat.Resend;
 {timeout waiting for ack}
 begin
 {resend and reshedule}
 txTime:=0;
 RTT:=RTT*2;
 if assigned(TMhook) and (not closed) then begin
  TMhook(RTT*2);
  if closed then begin
   Done; {if hook decided to close then abort}
   exit;
  end;
 end;
 if (RTT>32000) and closed
 then Done {give up}
 else if txLen>0 then begin
  ServerLoop.SendMessage(txPk^,txLen,remote);
  ServerLoop.Shedule(RTT*2,@Resend);
 end;
end;

procedure tChat.OnReply(msg:tSMsg);
 var seq,aseq:Word;
 var s:tMemoryStream;
 begin
 msg.stream.skip(1{opcode});
 seq:=msg.stream.ReadWord(2);
 aseq:=msg.stream.ReadWord(2);
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
   callback(msg,true);
  end;
 end;
 if aseq>0 then {ack of our msg} begin
  if (aseq=rxSeq)and(txLen>0) {it is current} then begin
   if txTime>0 then RTT:=Round((Now-txTime)*MsecsPerDay);
   FreeMem(txPk,txLen);
   TxLen:=0;
   txPk:=nil;
   callback(msg,false);
  end else {it is ack of old data, do nothing};
 end;
end;

procedure OnHiMsg(msg:tSMsg);
 {new chat was received!}
 var opcode:byte;
 var seq,aseq:word;
 var hnd:tChatHandler;
 var nchat:^tChat;
 begin
 opcode:=msg.stream.ReadByte;
 seq:=msg.stream.ReadWord(2);
 aseq:=msg.stream.ReadWord(2);
 if (seq<>1)and(aseq>0) then exit; {invalid initial state}
 hnd:=GetHnd(msg.stream.ReadByte);
 msg.stream.seek(msg.stream.position-1);{unskip the initcode}
 New(nchat);
 nchat^.InitFrom(msg.Source^,opcode);
 hnd(nchat^,msg);
end;

BEGIN
 ServerLoop.SetHiMsgHandler(@OnHiMsg);
END.