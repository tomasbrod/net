UNIT Task;
{
  Parent object to all tasks that need to notify their ovners of state change
}

INTERFACE

TYPE

  tTask_ptr=^tTask;
  tTaskEvent=(tevComplete, tevError, tevClose, tevSubTask);
  tTaskCallback=procedure( task:tTask_ptr; event:tTaskEvent; data:pointer ) of object;
  (* Calback for immediate-action event; polling for progress *)
  
  tTaskSubItem=record
    cb: tTaskCallback;
    weak:boolean;
  end;

  tTask=object
    procedure Attach( subscriber:tTask_ptr; callback:tTaskCallback);
    procedure Attach( callback:tTaskCallback );
    procedure AttachWeak( callback:tTaskCallback );
    procedure Detach( callback:tTaskCallback );
    function ProgressPct: word; virtual; {scaled by 10000}
    function GetSubtaskCount: integer; virtual;
    function GetSubTask(i:integer): tTask_ptr; virtual;
    destructor Done;
    constructor Init;
    protected
    procedure Abandoned; virtual; abstract;
    procedure SendEvent(event:tTaskEvent; data:pointer);
    private
    subscriber:^tTaskSubItem;
    subscriberSize:word;
    inSendEvent:Word;
    procedure TaskIntAttach( callback:tTaskCallback; weak:boolean);
  end;

IMPLEMENTATION

procedure tTask.TaskIntAttach( callback:tTaskCallback; weak:boolean );
  var i:integer;
  begin
  for i:=0 to subscriberSize-1
    do if not assigned(subscriber[i].cb) then begin
      subscriber[i].cb:=callback;
      subscriber[i].weak:=weak;
      exit;
  end;
  i:=subscriberSize;
  subscriberSize:=subscriberSize*2;
  ReAllocMem(subscriber,subscriberSize*sizeof(tTaskSubItem));
  subscriber[i].cb:=callback;
  subscriber[i].weak:=weak;
  for i:=i to subscriberSize-1
    do subscriber[i].cb:=nil;
end;

procedure tTask.SendEvent(event:tTaskEvent; data:pointer);
  var i:integer;
  begin
  inSendEvent:=1;
  for i:=0 to subscriberSize-1
  do if assigned(subscriber[i].cb)
  then begin
    subscriber[i].cb(@self,event,data);
  end;
  if inSendEvent=2 then begin
    SendEvent(tevClose,nil);
    Abandoned;
  end else inSendEvent:=0;
end;

procedure tTask.Detach( callback:tTaskCallback );
  var i,l,w:integer;
  begin
  l:=0;
  assert(assigned(callback),'wtf detach nil');
  for i:=0 to subscriberSize-1
  do if subscriber[i].cb=callback then begin
    subscriber[i].cb:=nil;
  end else if assigned(subscriber[i].cb) then begin
    if subscriber[i].weak=false then inc(l);
  end;
  if (l=0) and (inSendEvent=0) then begin
    SendEvent(tevClose,nil);
    Abandoned;
  end;
  if (l=0) and (inSendEvent=1) then inSendEvent:=2;
end;

destructor tTask.Done;
  begin
  FreeMem(subscriber,subscriberSize*sizeof(tTaskSubItem));
end;
constructor tTask.Init;
  var i:integer;
  begin
  inSendEvent:=0;
  subscriberSize:=6;
  subscriber:=GetMem(subscriberSize*sizeof(tTaskSubItem));
  for i:=0 to subscriberSize-1
    do subscriber[i].cb:=nil;
end;

procedure tTask.Attach( subscriber:tTask_ptr; callback:tTaskCallback);
  begin
  TaskIntAttach(callback,false);
  subscriber^.SendEvent(tevSubTask,@self);
end;
procedure tTask.Attach( callback:tTaskCallback );
  begin
  TaskIntAttach(callback,false);
end;
procedure tTask.AttachWeak( callback:tTaskCallback );
  begin
  TaskIntAttach(callback,true);
end;
function tTask.ProgressPct: word;
  begin ProgressPct:=5000 end;
function tTask.GetSubtaskCount: integer;
  begin GetSubTaskCount:=0 end;
function tTask.GetSubTask(i:integer): tTask_ptr;
  begin GetSubTask:=nil end;


END.
