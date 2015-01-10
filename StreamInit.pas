unit StreamInit;

INTERFACE
uses Peers,NetAddr;

{ Listenning app should send tBind to daemon (regulary) from in's socket. 

Daemon sends tRequest2 to app. App should connect() it's socklet to the 
remote and send tAccept (or tReject). The app may consult daemon for info 
about the client.  }

{ Listenning app should send tBind to daemon (regulary) from it's socket. }
type tBind=object(tPacket)
 service :netaddr.Word2;
 public
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create(const service:word2; const reason:word2);
end;

{ Connecting App sends tRequest to daemon on listeners side. }
const cRequest=7;
type tRequest=object(tPacket)
 service :netaddr.Word2;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create(const service:word2; const reason:word2); //overload;
end;

{ Listenig app recieves tConnect from daemon. And records the remote addr. }
type tConnect=object(tPacket)
 service :netaddr.Word2;
 remote :netaddr.t;
 procedure Send( const rcpt: NetAddr.t);
 procedure Create(const service:word2; const reason:word2); //overload;
end;

{ Or Daemon may reject the request, if no app is listening. }
const cReject=8;
const rrServiceUnavail=ord('s');
type tReject=object(tPacket)
 service :netaddr.Word2;
 reason  :byte;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create; //overload;
end;

{ Listening app (repeat) sends tAccept to remote (until it receives tAccept 
from remote). } { Connecting app receies tAccept and sends tAccept back. } 
const cAccept=9;
type tAccept=object(tPacket)
 service :netaddr.Word2;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create; //overload;
end;

{ The apps may now communicate freely. }

IMPLEMENTATION

procedure Reject(const rcpt:NetAddr.t; const reason:word);
 var p:tReject;
 begin
 p.Create(service,reason);
 p.Send(rcpt);
end;

procedure tRequest.Handle( const from: NetAddr.t);
 var Srv:;
 var ConInit:;
 begin
 Srv:=Services.find(service);
 if assigned(Srv) then begin
  Send(Srv.);
  SrvDescr.;
 end else Reject(rrServiceUnavail);
end;

END.
