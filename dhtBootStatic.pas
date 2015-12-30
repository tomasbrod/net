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
 procedure BootCmdline;
end;

procedure t.BootCmdline;
 var addr:tNetAddr;
 var oi:word;
 var cnt:word;
 const opt='-boot';
 begin
 oi:=OptIndex(opt);
 if oi>0 then begin
  cnt:=OptParamCount(oi);
  assert(cnt>=1,opt+'(addr+)');
  for oi:=oi+1 to cnt do begin
   addr.FromString(paramstr(oi+1));
   DHT.NodeBootstrap(addr);
  end;
 end;
end;

procedure t.Boot;
 var bs:TextFile;
 const bsfn='bootstrap.txt';
 var line:string;
 var addr:tNetAddr;
 begin
 BootCmdLine;
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
 Shedule(700,@o.boot);
END.