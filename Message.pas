unit Message;

INTERFACE

{packets}
type tMsgSend=object(tPacket)
 recipient: tPID;
 MsgBody: tFID;
 tags :array [1..64] of tKeyWord;
 end;
type tMsgConfirm=object(tPacket)
 recipient: tPID;
 MsgBody: tFID;
 confirm: tFID;
 end;
type tMsgAck=object(tPacket)
 recipient: tPID;
 MsgBody: tFID;
 status: (magOK, magAlready, magNoInterest, magGeneralReject);
 end;

IMPLEMENTATION
uses Neighb,Transfer;

type tMsgMeta=object
 recipient: tPID;
 MsgBody: tFID;
 confirm: tFID;
 from: tPID;
 received: tDateTime;
 accessed: tDateTime;
 affil: (maNone=1, maStatic=8, maSender=10);
 tags :array [1..64] of tTag;
 {
 countHop: Word;
 countAlready: Word;
 }
 end;

END.