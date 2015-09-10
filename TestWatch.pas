unit TestWatch;

INTERFACE
IMPLEMENTATION
uses ServerLoop,SysUtils;

type tObj=object
 var f:text;
 var h:tHandle;
 procedure Event1(ev:Word);
 procedure Event2;
 procedure Init;
end;

procedure tObj.Init;
 begin
 assign(f,'');
 reset(f);
 h:=GetFileHandle(f);
 writeln('TestWatch: Input handle ',h);
 WatchFD(h,@Event1);
 Shedule(32,@Event2);
end;

procedure tObj.Event1(ev:Word);
 begin
 writeln('TestWatch: Event1 ',IntToHex(ev,4));
 readln;
 WatchFD(h,nil);
 UnShedule(@Event2);
end;
procedure tObj.Event2;
 begin
 writeln('TestWatch: Event2 ');
 WatchFD(h,nil);
end;

var o:tObj;
BEGIN
 o.Init;
END.