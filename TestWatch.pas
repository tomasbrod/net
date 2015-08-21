unit TestWatch;

INTERFACE
IMPLEMENTATION
uses ServerLoop,SysUtils;

type tObj=object
 var f:text;
 var h:tHandle;
 procedure Event1(ev:Word);
 procedure Init;
end;

procedure tObj.Init;
 begin
 assign(f,'');
 reset(f);
 h:=GetFileHandle(f);
 writeln('Input handle ',h);
 WatchFD(h,@Event1);
end;

procedure tObj.Event1(ev:Word);
 begin
 writeln('Event1 ',IntToHex(ev,4));
 readln;
 WatchFD(h,nil);
end;

var o:tObj;
BEGIN
 o.Init;
END.