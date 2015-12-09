unit TestDHT;

INTERFACE
IMPLEMENTATION
USES ServerLoop,NetAddr,SysUtils,DHT;
type t=object
 procedure init;
end;
procedure t.init;
 var oi:word;
 const opt='-test-dht';
 var addr:tNetAddr;
 begin
 oi:=OptIndex(opt);
 if oi>0 then begin
  assert(OptParamCount(oi)=1,opt+'(rcpt:tNetAddr fid:sha1)');
  addr:=paramstr(oi+1);
  writeln('TestDHT: rcpt '+paramstr(oi+1));
  DHT.NodeBootstrap(addr);
 end;
end;

var o:t;
BEGIN
 Shedule(10,@o.init);
END.