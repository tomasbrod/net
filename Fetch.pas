unit Fetch;
{
  Easy to use interface to ObjTrans.
  Keep record of runnong OT jobs.
  Attach multiple callbacks to single job.
}

INTERFACE
uses NetAddr,Store2,ObjTrans;

type tCb1=procedure of object;
type tCb2=procedure(total,done:LongWord; rate:single) of object;
type tErr=(
  stRunning=0,errFilesys, {local filesystem error}
  errCorrupt,  {file downloaded was corrupt}
  errNoReply,  {remote server did not reply}
  errProtocol, {invalid response from server}
  errTimeout,  {timeout during transfer}
  errNotFound, {file not found on remote server}
  errServer    {remote server admitted failure});
type
pFetch=^tFetch;
tFetch=object
  public
  Done: boolean;
  Error: tErr;
  procedure Abort(callback:tCb1); experimental;
  {$hint get fraction done, total size, rate, size}
  private
  observers: array of tCb1;
  transfer:ObjTrans.tJob;
  next,prev:^tFetch;
  procedure OTJobHandler;
  end;

function FetchObject(fid:tFID; srce:tNetAddr; prio:Byte; callback:tCb1): pFetch;
{returns nil if file is present}

IMPLEMENTATION
uses SysUtils,opcode,MemStream;

var Running:^tFetch;

function FetchObject(fid:tFID; srce:tNetAddr; prio:Byte; callback:tCb1): pFetch;
  var f:file of byte;
  var iores:word;
  var p:^tFetch;
  var oi:word;
  begin
  result:=nil;
  {$I-}
  AssignObject(f, fid);
  Reset(f); Close(f);
  iores:=IOResult;
  {$I+}
  if IORes=0 then exit;
  if IORes>3 then raise eInOutError.Create('I/O Error '+IntToStr(iores));
  {check for running transfer}
  p:=Running;
  while assigned(p) do if p^.transfer.fid=fid then break else p:=p^.next;
  {start transfer or attach to the running}
  if assigned(p) then begin
    oi:=length(p^.observers);
    SetLength(p^.observers,oi+1);
    p^.observers[oi]:=callback;
    if p^.transfer.Weight<prio then p^.transfer.Weight:=prio;
  end else begin
    new(p);
    p^.done:=false;
    p^.error:=stRunning;
    setLength(p^.observers,1);
    if assigned(Running) then Running^.prev:=p;
    p^.next:=Running; p^.prev:=nil; Running:=p;
    p^.observers[0]:=callback;
    p^.transfer.Init(srce,fid);
    {$note todo handle init errors better}
    if p^.transfer.error>0 then raise eXception.Create('Failed to initialize transfer error='+IntToStr(p^.transfer.error));
    p^.transfer.Weight:=prio;
    p^.transfer.Callback:=@p^.OTJobHandler;
    p^.transfer.Start;
  end;
  result:=p;
end;

procedure tFetch.OTJobHandler;
  var i:integer;
  begin
  case transfer.error of
    1:begin
      if HashObjectCheckID(transfer.dataf,transfer.fid)
      then begin
        done:=true;
        if length(observers)>1 then Reference(transfer.fid,length(observers)-1);
      end else error:=errCorrupt;
    end;
    2:error:=errFileSys;
    3:error:=errNoReply;
    {4:cannot start more jobs;}
    opcode.otFail:error:=errServer;
    opcode.otNotFound:error:=errNotFound;
    else AbstractError;
  end;
  writeln('Fetch.OTJobHandler: ',done,error);
  for i:=0 to high(observers) do observers[i];
end;

procedure tFetch.Abort(callback:tCb1);
  var i,l:word;
  begin
  l:=Length(observers);
  if l>1 then begin
    for i:=0 to high(observers) do if observers[i]=callback then  observers[i]:=observers[l];
    SetLength(observers,l-1);
  end else begin
    transfer.Done;
    SetLength(observers,0);
    FreeMem(@self,sizeof(self));
  end;
end;


END.