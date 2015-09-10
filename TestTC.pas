unit TestTC;
INTERFACE
IMPLEMENTATION
USES ServerLoop
    ,TC
    ,MemStream
    ;
type t=object
 tcs:TC.tTCS;
 cnt:byte;
 buf: array [1..4096] of char;
 procedure CanSend(size:word);
 procedure Init;
 end;

procedure t.CanSend(size:word);
 var s:tMemoryStream;
 begin
 s.Init(@buf,0,4096);
 tcs.WriteHeaders(s);
 if size>s.size then size:=s.size;
 s.Skip(size-1);
 s.WriteByte(9);
 tcs.Send(s);
end;

procedure t.Init;
 begin
 cnt:=0;
 tcs.Init;
 tcs.rid:=42;
 tcs.lid:=22;
 tcs.Remote.FromString('//ip4/192.168.1.47/3511');
 tcs.CanSend:=@CanSend;
 TC.RegTXer(tcs);
 tcs.Start;
 writeln('TestTC: Transfer started');
end;

var o:t;
BEGIN
 o.Init;
END.