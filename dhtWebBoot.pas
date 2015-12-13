unit dhtWebBoot;

INTERFACE

procedure Bootstrap(url:ansistring);

IMPLEMENTATION
uses sockets,netdb,uriparser,NetAddr;

type t=object
 s:tSocket;
 procedure Init(url:ansistring);
end;

procedure t.Init(url:ansistring);
 var uri:tURI;
 var he:tHostEntry;
 var addr:tNetAddr;
 begin
 uri:=ParseURI(url);
 if uri.port=0 then uri.port:=80;
 uri.path:=uri.path+uri.document;
 writeln('Boot: ',uri.host,' ',uri.port,' ',uri.path,' ',uri.params);
 if NetDB.ResolveHostByName(uri.host,he) then begin
  addr.data.family:=afInet;
  addr.data.inet.addr:=he.Addr;
  addr.data.inet.port:=htons(uri.port);
  writeln('Boot: ',string(addr)+'/'+uri.path+'?'+uri.params);
 end else writeln('fuck');
end;

procedure Bootstrap(url:ansistring);
 var o:^t;
 begin
 New(o);
 o^.Init(url);
end;

END.