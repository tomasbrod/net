Unit CTRL;
INTERFACE
IMPLEMENTATION
USES ServerLoop
    ,CtrlLow
    ,MemStream
    ;
       {(var client:tClient; msg:tMemoryStream)}
procedure Terminate(var client:tClient; msg:tMemoryStream);
 begin
 ServerLoop.RequestTerminate;
end;
procedure GetInfo(var client:tClient; msg:tMemoryStream);
 const ident:string='BrodNetD orig 0.0-dev';
 begin
 msg.seek(0);
 msg.length:=0;
 msg.Write(ident[1],length(ident));
 client.reply(msg);
end;

BEGIN
 methods[0].Init(@GetInfo,0);
 methods[1].Init(@Terminate,0);
END.