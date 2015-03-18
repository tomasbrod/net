unit Message;

INTERFACE
uses Neighb,ContentHash,KeyWords;

procedure Send( data: contenthash.t; recipient: tPID; array of tKeyWord );
 {tags are composed of (3)file keywords, (2)recipient tags and (1)additional tags}

 { every hop the tags are replaced with tags of the recipient if known, else unmodified }

{packets}
const cSend=10;
type  tSend=object(tPacket)
 trid:byte;
 recipient: tPID;
 MsgBody: tFID;
 keywords :array [1..8] of tKeyWord;
 tags :array [1..56] of tKeyWord;
 end; {approx 300 Byte}
const cConfirm=11;
type  tConfirm=object(tPacket)
 recipient: tPID;
 MsgBody: tFID;
 confirm: tFID;
 end;
const cAck=12;
type  tAckStatus= (magGood=4, magNeighb, magStatic, magTags, magFree
                  ,magFail=18, magAlready, magNoInterest, magReject, magSpam, magBadSig
                  ,magMax=31 );
type  tAck=object(tPacket)
 trid:byte; {0 if asynchronous >=1 if in response to tSend}
 recipient: tPID;
 MsgBody: tFID;
 status: tAckStatus;
 end;

const cStorage=10; {identifier for StorageManager to not delete message files}

IMPLEMENTATION
uses SysUtils
    ,Neighb
    ,Transfer
    ,Storage
    ,NetAddr;

type tCHK=ContentHash.t;
type tMsgMeta=packed object
 recipient: tPID;
 MsgBody: tFID;
 confirm: tFID;
 from: tPID unimplemented;
 received: tDateTime;
 accessed: tDateTime;
 affil: (maNone=1, maStatic=8, maSender=10);
 tags :array [1..56] of tTag unimplemented;
 statCount :array [tAckStatus] of LongWord;
 statLast :array [tAckStatus] of tDateTime;
 procedure Init;
 end;
type tMsgMetaDs=object(tMsgMeta)
 procedure Load(id:tCHK;pos:word); overload; unimplemented;
 procedure Load(id:tCHK;rcpt:tPID); overload; unimplemented;
 procedure Save;
 procedure Delete; unimplemented;
end;

procedure tMsgMeta.Init;
 var i:byte;
 begin
 confirm.clear;
 received:=now;
 accessed:=now;
 FillChar(statCount,sizeof(statCount),0);
 FillChar(statLast,sizeof(statLast),0); {!}
 {msg.tags!}
end;

type tPending=object(tSend);
 rcpt:netaddr.t;
 retry:byte;
end;
var Pending:array [1..15] of ^tPending;
var LastPendingRetry:tDateTime;
 
procedure Send( data: contenthash.t; recipient: tPID; array of tKeyWord );
 var msg:tMsgMetaDS;
 begin
 msg.Init;
 msg.recipient:=recipient;
 msg.MsgBody:=data;
 msg.affil:=maSender;
 {msg.tags}
 msg.Save;
 Storage.AddReference(data,cStorage);
 proc(msg);
end;

procedure proc(msg:tMsgMetaDS);
 var routes:array [1..1] of netaddr.t;
 var rx:byte;
 begin
 Roure.GetRoute(routes,msg.recipient);
 for rx:=low(routes) to high(routes) do begin
  SendTo(msg,routes[rx]);
 end;
 {
 tags!
 }
end;

procedure SendTo( var msg:tMsgMetaDs; const rcpt: netaddr.t );
 var px:byte;
 begin
 px:=low(pending); while assigned(pending[px]) and (px<=high(pending)) do inc(px);
 if px>high(pending) then error;
 New(pending[px]);
 pending[px]^.rcpt:=rcpt;
 with pending[px]^ do begin
  trid:=px;
  recipient: msg.recipient;
  MsgBody:=msg.MsgBody;
  tags:=msg.tags;
 end;
 pending[px]^.retry:=1;
 pending[px]^.Send(rcpt);
 LastPendingRetry:=now;
end;

{
procedure NotifyIdle;
 - retry pending
 - scan store and delete/resend
}

END.