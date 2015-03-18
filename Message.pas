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
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t );
 end; {approx 300 Byte}
const cConfirm=11;
type  tConfirm=object(tPacket)
 recipient: tPID;
 MsgBody: tFID;
 confirm: tFID;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t );
 end;
type  tDeliveryStatus= 
                  (mdsGood=4, mdsNeighb, mdsStatic, mdsTags, mdsFree
                  ,mdsFail=18, mdsAlready, mdsNoInterest, mdsReject, mdsSpam, mdsBadSig
                  ,mdsMax=31 );
const cAck=12;
type  tAck=object(tPacket) {to confirm reception of tSend}
 trid:byte;
 status: tDeliveryStatus;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t );
 end;
const cProgress=13;
type  tProgress=object(tPacket)
 recipient: tPID;
 MsgBody: tFID;
 status: tDeliveryStatus;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t );
 end;

const cStorage=10; {identifier for StorageManager to not delete message files}

IMPLEMENTATION
uses SysUtils
    ,Neighb
    ,Transfer
    ,Storage
    ,NetAddr;

const cRetryPeriod = 2000 /MSecsPerDay;
const cRetryMax= 3;
const cExpireUnafil= 7 /MinutesPerDay;
const cExpireStatic= 5;
const cExpireSender= 20;

type tCHK=ContentHash.t;
type tMsgMeta=packed object
 recipient: tPID;
 MsgBody: tFID;
 confirm: tFID;
 missBody,missConfirm:boolean;
 from: tPID unimplemented;
 received: tDateTime;
 accessed: tDateTime;
 affil: (maNone=1, maStatic=8, maSender=10);
 tags :array [1..56] of tTag unimplemented;
 statCount :array [tDeliveryStatus] of LongWord;
 statLast :array [tDeliveryStatus] of tDateTime;
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
 missBody:=false;
 missConfirm:=false;
 confirm.clear;
 received:=now;
 accessed:=now;
 received:=now;
 FillChar(statCount,sizeof(statCount),0);
 FillChar(statLast,sizeof(statLast),0); {!}
 {msg.tags!}
end;

type tPending=record 
 rcpt:netaddr.t;
 retry:byte;
 case kind:byte of
  cSend: (send:tSend);
  cConfirm: (confirm:tConfirm);
end;
var Pending:array [1..15] of ^tPending;
var LastPendingRetry:tDateTime;

const StorePrefix='msg'+DirectorySeparator;
var LastStoreScan:tDateTime;
 
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
 pending[px]^.retry:=1;
 pending[px]^.kind:=cSend;
 with pending[px]^.send do begin
  Create(cSend);
  trid:=px;
  recipient:=msg.recipient;
  MsgBody:=msg.MsgBody;
  tags:=msg.tags;
  Send(rcpt);
 end;
 LastPendingRetry:=now;
end;

procedure RetryPending;
 var px:byte;
 begin
 for px:=low(pending) to high(pending) do if assigned(pending[px]) then with pending[px]^ do begin
  if retry>cRetryMax then begin
   dispose(pending[px]);
   pending[px]:=nil;
   log;
  end else begin
   case kind of
    cSend: send.Send(rcpt);
    cConfirm: confirm.Send(rcpt);
   end;
   Inc(retry);
  end;
 end;
 LastPendingRetry:=now;
end;

procedure ScanStore;
 unimplemented;
 begin
 {load and call proc on each msg}
end;

procedure NotifyIdle;
 begin
 if now-LastPendingRetry > cRetryPeriod then RetryPending;
 if now-LastStoreScan > cRetryPeriod then ScanStore;
end;

{handlers}
procedure tSend.Handle( const from: NetAddr.t);
 var msg:tMsgMetaDs;
 var ack:tAck;
 var done,total:longword;
 begin 
 try
  msg.Load(MsgBody,recipient);
 except
  msg.Init;
  msg.recipient:=recipient;
  msg.MsgBody:=MsgBody;
  {from:=}
  {affil: (maNone=1, maStatic=8, maSender=10);}
  missBody:=true;
  msg.tags:=tags;
 end;
 msg.accessed:=Now;
 {merge msg.tags and tags ... }
 msg.Save;
 ack.Create(cAck);
 ack.trid:=trid;
 if (msg.MissBody)and(msg.confirm.isNil) then begin
  if msg.affil=maStatic then ack.status:=mdsStatic
  {else if ... then ack.status:=mds...}
  else ack.status:=mdsGood;
  Transfer.RequestFile( from, MsgBody, cStorage );
 end else maStatic: ack.status:=mdsAlready;
 ack.Send(from);
 if (not msg.confirm.isNil)and(not msg.MissConfirm) then SendConfirmTo(msg, from);
end;

procedure tConfirm.Handle( const from: NetAddr.t);
 begin 
 AbstractError;
end;
procedure tAck.Handle( const from: NetAddr.t);
 begin 
 AbstractError;
end;


{senders}
procedure tSend.Send( const rcpt: NetAddr.t );
 begin inherited Send( rcpt, sizeof(self); end;
procedure tConfirm.Send( const rcpt: NetAddr.t );
 begin inherited Send( rcpt, sizeof(self); end;
procedure tAck.Send( const rcpt: NetAddr.t );
 begin inherited Send( rcpt, sizeof(self); end;
procedure tProgress.Send( const rcpt: NetAddr.t );
 begin inherited Send( rcpt, sizeof(self); end;


END.