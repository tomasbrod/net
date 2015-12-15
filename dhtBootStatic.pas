unit dhtBootStatic;
{
  Bootstrap the dht from set of static nodes
  from file.
}

INTERFACE

IMPLEMENTATION
uses NetAddr,ServerLoop,DHT,SysUtils;

type t=object
 procedure Boot;
end;

procedure t.Boot;
 var bs:TextFile;
 const bsfn='bootstrap.txt';
 var line:string;
 var addr:tNetAddr;
 begin
 assign(bs,bsfn);
 try
  reset(bs);
 except
  writeln('BootStatic: Error opening file '+bsfn);
  exit;
 end;
 try
  while not eof(bs) do begin
   readln(bs,line);
   try addr.FromString(line);
   except on eConvertError do begin
    writeln('BootStatic: ConvertError ',line,' to tNetAddr');
    continue;
   end end;
   DHT.NodeBootstrap(addr);
  end;
 finally
  close(bs);
 end;
end;

var o:t;
BEGIN
 Shedule(1000,@o.boot);
END.