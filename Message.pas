UNIT Message;

INTERFACE
USES ObjectModel,SysUtils;

const cMagic:packed array [1..8] of char='BNMesag'#26;

type tMessageRecipient=record
    id: tKey20;
    prefix: array [1..3] of byte;
    priority: byte;
    wrapped_session_key: array [1..40] of byte;
end;

type tMessageRead=object
  senderid: tKey20;
  sender_encr_key: tKey32;
  created, expires: Int64;
  cRcpts: byte;
  flags: byte;
  rcpts: ^tMessageRecipient;
  {todo}
  constructor ReadFrom(var src:tCommonStream; parseLevel:byte);
  constructor InitEmpty;
  destructor Done;
  {0=readAll+Verify}
end;

IMPLEMENTATION
uses Crypto,ed25519;

constructor tMessageRead.InitEmpty;
  begin
  cRcpts:=0; Rcpts:=nil;
end;

function ReadLength1Value(var m:tCommonStream; out count:byte; size:longword):pointer;
  begin
  count:=m.ReadByte;
  size:=count*size;
  if count>0 then begin
    result:=GetMem(size);
    m.Read(result^,size);
  end else result:=nil;
end;

constructor tMessageRead.ReadFrom(var src:tCommonStream; parseLevel:byte);
  var Magic:packed array [1..8] of char;
  var i:longword;
  var m: tCommonStream absolute src; {todo}
  begin
  InitEmpty;
  m.Read(Magic,8);
  if CompareByte(Magic,cMagic,8)<>0
    then raise eFormatError.create('Invalid magic sequence');
  m.Read(SenderID,20);
  m.Read(Sender_encr_key,32);
  created:=m.ReadWord6;
  cRcpts:=m.ReadByte;
  flags:=m.ReadByte;
  expires:=created+m.ReadWord4;
  rcpts:=GetMem(sizeof(tMessageRecipient)*cRcpts);
  for i:=0 to cRcpts-1 do begin
    m.Read(rcpts[i].id,20);
    m.Read(rcpts[i].prefix,3);
    m.Read(rcpts[i].priority,1);
    m.Read(rcpts[i].wrapped_session_key,40);
  end;
end;

destructor tMessageRead.Done;
  begin
  FreeMem(Rcpts);
end;

END.
