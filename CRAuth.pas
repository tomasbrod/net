unit CRAuth;
{Challenge-Response Authenticator}
INTERFACE
USES NetAddr,ECC,SHA1,Chat,ServerLoop,MemStream,opcode;
type
 tAuth=object
  ch:tChat;
  Challenge:tEccKey;
  RemotePub:tEccKey;
  Valid:Boolean;
  PoWValid:Boolean;
  error:byte;
  Callback:procedure of object;
  procedure Init(const iRemote:tNetAddr);
  procedure Cancel;
  private
  procedure ReplyRes(msg:tSMsg; data:boolean);
  procedure ReplyPow(msg:tSMsg; data:boolean);
  procedure Done;
  procedure Timeout;
  procedure Conclusion;
 end;

IMPLEMENTATION

procedure tAuth.Init(const iRemote:tNetAddr);
 var ms:tMemoryStream;
 begin
 Assert(assigned(Callback) and (not iRemote.isNil));
 Valid:=FALSE;
 PoWValid:=FALSE;
 Error:=255;
 Ch.Init(iRemote);
 Ch.OnDispose:=@Done;
 Ch.OnTimeout:=@Timeout;
 Ch.Callback:=@ReplyRes;
 Ch.SetTimeout(8001,3000);
 {generate and send challenge}
 Ch.StreamInit(ms,66);
 ECC.CreateChallenge(challenge);
 Ms.WriteByte(opcode.crAuthReq);
 Ms.WriteByte(1);
 Ms.Write(ECC.PublicKey,sizeof(PublicKey));
 Ms.Write(challenge,sizeof(challenge));
 Ch.Send(Ms);
end;

procedure tAuth.ReplyRes(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.Stream;
 var status:byte;
 var resp:^tEccKey;
 var vresp:tSha1Digest;
 begin
 if not data then exit;
 status:=r.readbyte; {todo, set error (eg: unsuported meth)}
 r.Read(RemotePub,sizeof(tEccKey));
 resp:=r.readptr(sizeof(tEccKey));
 ECC.CreateResponse(Challenge,vresp,RemotePub);
 Valid:=CompareByte(resp^,vresp,sizeof(vresp))=0;
 if (status and 128)>0 then begin
  {expecting pow}
  Ch.Callback:=@ReplyPow;
  Ch.Ack;
 end else Conclusion;
end;
procedure tAuth.ReplyPow(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.Stream;
 var ptp:byte;{Proof TyPe}
 var nonce:^tPoWRec;
 begin
 if not data then exit;
 ptp:=r.readbyte; {todo}
 nonce:=r.ReadPtr(sizeof(tPoWRec));
 PoWValid:=VerifyPoW(nonce^,RemotePub);
 Conclusion;
end;
procedure tAuth.Timeout;
 begin
 error:=251;
 Callback;
 Ch.Close;
end;
procedure tAuth.Conclusion;
 var ms:tMemoryStream;
 begin
 error:=0;
 Ch.StreamInit(ms,2);
 ms.WriteByte(byte(Valid));
 ms.WriteByte(byte(PowValid));
 Ch.Send(ms);
 Callback;
 ch.Close;
end;
procedure tAuth.Done;
 begin
 {called by chat}
 FreeMem(@self,sizeof(self));
end;

type tServer=object
 ch:^tChat;
 pub:tEccKey;
 procedure SendRep(msg:tSMsg; data:boolean);
 procedure SendPow(msg:tSMsg; data:boolean);
 procedure Last(msg:tSMsg; data:boolean);
 procedure Close;
end;

procedure AuthHandler(var ch:tChat; msg:tSMsg);
 var srv:^tServer;
 begin
 msg.stream.skip(1); {initcode}
 new(srv);
 srv^.ch:=@ch;
 ch.OnTimeout:=@srv^.Close;
 srv^.SendRep(msg,true);
 {reply with hash}
 {wait ack}
 {reply pow}
 {wait reply}
end;

procedure tServer.SendRep(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.Stream;
 var ms:tMemoryStream;
 var ver:byte;
 var chal:^tEccKey;
 var resp:tSha1Digest;
 begin
 ver:=r.ReadByte; {todo}
 r.Read(pub,sizeof(pub));
 chal:=r.readptr(sizeof(tEccKey));
 CreateResponse(chal^,resp,pub);
 ch^.StreamInit(ms,66); {todo}
 ms.WriteByte(128);
 ms.Write(PublicKey,sizeof(PublicKey));
 ms.Write(resp,sizeof(resp));
 ch^.Callback:=@SendPoW;
 ch^.SetTimeout(8000,0);{no reply expected}
 ch^.send(ms);
end;

procedure tServer.SendPow(msg:tSMsg; data:boolean);
 var ms:tMemoryStream;
 begin
 if data then exit;
 ch^.StreamInit(ms,66); {todo}
 ms.WriteByte(2);
 ms.Write(PublicPoW,sizeof(PublicPoW));
 ch^.Callback:=@Last;
 ch^.SetTimeout(8000,2000);
 ch^.send(ms);
end;

procedure tServer.Last(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.Stream;
 var Valid,ValidPoW:byte;
 begin
 if not data then exit; {unlikely}
 Valid:=r.ReadByte;
 ValidPoW:=r.ReadByte;
 if (Valid<>1)or(ValidPoW<>1) then begin
  write('CRAuth: Our auth failed on remote, reason pub=',Valid,' pow=',ValidPoW);
  Writeln(' remote ',string(ch^.remote),' ',string(pub));
 end;
 Close;
end;
procedure tServer.Close;
 begin
 ch^.Close;
 FreeMem(@self,sizeof(self));
end;

procedure tAuth.Cancel;
 begin
 error:=247;
 Ch.Close;
end;

BEGIN
 SetChatHandler(opcode.crAuthReq,@AuthHandler);
END.
