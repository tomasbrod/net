unit StreamInit;

INTERFACE
uses Peers,NetAddr;

type tRequest=object(tPacket)
 service :netaddr.Word2;
 procedure Handle( const from: NetAddr.t);
 unimplemented;
 procedure Send( const rcpt: NetAddr.t);
 procedure Create(const service:word2; const reason:word2); //overload;
end;
type tReject=object(tPacket)
 service :netaddr.Word2;
 reason  :byte;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create; //overload;
end;
type tAccept=object(tPacket)
 service :netaddr.Word2;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create; //overload;
end;

IMPLEMENTATION

procedure tRequest.Handle( const from: NetAddr.t);
 procedure Reject(const reason:word);
  var p:tReject;
  begin
  p.Create(service,reason);
  p.Send(from);
 end;
 var SrvDescr:;
 var ConInit:;
 begin
 SrvDescr:=Services.find(service);
 if assigned(SrvDescr) then begin
  SrvDescr.;
 end else Reject(rrNotFound);
end;

END.
