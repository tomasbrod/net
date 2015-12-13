unit TestDHT;

INTERFACE
IMPLEMENTATION
USES ServerLoop,NetAddr,SysUtils,DHT,dhtWebBoot;
type t=object
 procedure init;
end;
procedure t.init;
 var oi:word;
 const opt='-test-dht';
 const opt2='-test-boot';
 var addr:tNetAddr;
 begin
 oi:=OptIndex(opt);
 if oi>0 then begin
  assert(OptParamCount(oi)=1,opt+'(rcpt:tNetAddr)');
  addr:=paramstr(oi+1);
  writeln('TestDHT: rcpt '+paramstr(oi+1));
  DHT.NodeBootstrap(addr);
 end;
 oi:=OptIndex(opt2);
 if oi>0 then begin
  assert(OptParamCount(oi)=1,opt2+'(url)');
  Bootstrap(paramstr(oi+1));
 end;
end;

var o:t;
BEGIN
 Shedule(10,@o.init);
END.