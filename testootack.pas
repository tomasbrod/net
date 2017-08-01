 uses SysUtils,ObjectModel;

 type
  tSampleTask=object (tTask)
    protected
    procedure Abort;virtual;
    public
    procedure Test69;
  end;

type t=object
  procedure H( task:tTask_ptr; event:tTaskEvent; data:pointer );
  procedure W( task:tTask_ptr; event:tTaskEvent; data:pointer );
  end;

procedure tSampleTask.Abort;
  begin
  writeln('task abandoned');
end;

procedure tSampleTask.Test69;
  begin
  self.SendEvent(tevComplete,nil);
end;

var o:t;
var t1,t2:tSampleTask;

procedure t.H( task:tTask_ptr; event:tTaskEvent; data:pointer );
  begin
  write('H Event ',event,' from ');
  if task=@t1 then writeln('t1');
  if task=@t2 then writeln('t2');
  task^.Detach(@h);
end;
procedure t.W( task:tTask_ptr; event:tTaskEvent; data:pointer );
  begin
  write('W Event ',event,' from ');
  if task=@t1 then writeln('t1');
  if task=@t2 then writeln('t2');
  //if event<>tevClose then task^.Detach(@w);
end;
  

BEGIN
  t1.Init;
  t2.Init;
  writeln('attach t1 to @h.O');
  t1.Attach(@o.H);
  t1.AttachWeak(@o.W);
  writeln('attach t2 to @h.O');
  t2.AttachWeak(@o.W);
  t2.Attach(@o.H);
  writeln;
  writeln('test69 on t1');
  t1.Test69;
  writeln;
  writeln('detach t2');
  t2.Detach(@o.H);
end.
