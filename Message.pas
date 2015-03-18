unit Message;

INTERFACE
uses Neighb,ContentHash;

procedure Send( data: contenthash.t; recipient: tPID; aTags: byte{?} );
 {tags are composed of (3)file keywords, (2)recipient tags and (1)additional tags}

 { every hop the tags are replaced with tags of the recipient if known, else unmodified }

{packets}
const cSend=10;
type  tSend=object(tPacket)
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
 recipient: tPID;
 MsgBody: tFID;
 status: tAckStatus;
 end;

const cStorage=10; {identifier for StorageManager to not delete message files}

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
 statCount :array [tAckStatus] of LongWord;
 statLast :array [tAckStatus] of tDateTime;
 end;

END.