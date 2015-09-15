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
 procedure CanSend;
 procedure Init;
 end;

procedure t.CanSend;
 var s:tMemoryStream;
 var size:word;
 begin
 s.Init(@buf,0,4096);
 size:=tcs.MaxSize(4096);
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
 tcs.Remote.FromString('//ip4/192.168.1.49/3519');
 tcs.CanSend:=@CanSend;
 TC.RegTXer(tcs);
 tcs.Start;
 writeln('TestTC: Transfer started');
end;

var o:t;
BEGIN
 o.Init;
END.