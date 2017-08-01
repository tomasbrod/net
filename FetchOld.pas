unit Fetch;
{
  Easy to use interface to ObjTrans.
  Keep record of runnong OT jobs.
  Attach multiple callbacks to single job.
  Handle multiple sources of file.
}

INTERFACE
uses ObjectModel,Store2,ObjTrans;

{current impl:
  connect to first node and if that fails try the others
next: dovnload from 4 nodes in parallel
next: download from nodes with lowest hop count
}

type tErr=(
  stRunning,
  errFilesys, {local filesystem error}
  errCorrupt,  {file downloaded was corrupt}
  errNoReply,  {remote server did not reply}
  errProtocol, {invalid response from server}
  errTimeout,  {timeout during transfer}
  errNotFound, {file not found on remote server}
  errServer    {remote server admitted failure});
type
pFetch=^tFetch;
tFetch=object(tTask)
  public
  Running,Errored,Completed:boolean;
  Error: tErr;
  FID:tFID;
  fMaxSize:LongWord;
  fWeight:byte;
  procedure AddSource(const srce:tNetAddr);
  procedure SetMaxSize(i:LongWord); unimplemented;
  procedure SetWeight(i:byte);
  {$hint get total size, rate, downloaded size}
  function ProgressPct: single; virtual;
  constructor Init(const ifid:tFID); {use NewFetch}
  protected
  transfer:ObjTrans.tJob;
  next,prev:^tFetch;
  procedure Abort; virtual;
  procedure Cleanup; virtual;
  procedure Handler;
  procedure FromCache;
  end;


{usage:
  create with NewFetch
    attach callback
    set weight,max_size
    add sources
  on error or success callback is called
  start error or file exist: callback sheduled
}

IMPLEMENTATION
uses SysUtils,opcode,ServerLoop;

var RunList:^tFetch;

function NewFetch(fid:tFID): pFetch;
  var so:tStoreObject;
  var p:^tFetch;
  begin
  result:=nil;
  try
    so.Init(fid);
    so.Reference(+1);
    so.Close;
    New(p,Init(fid));
    p^.Completed:=true;p^.Error:=errFileSys;
    ServerLoop.Shedule(0,@p^.FromCache);
    exit;
  except
    on eObjectNF do;
  end;
  {check for running transfer}
  p:=RunList;
  while assigned(p) do if p^.transfer.fid=fid then break else p:=p^.next;
  {start transfer or attach to the running}
  if not assigned(p) then begin
    new(p,Init(fid));
    if assigned(RunList) then RunList^.prev:=p;
    p^.next:=RunList; p^.prev:=nil; RunList:=p;
  end;
  result:=p;
end;

constructor tFetch.Init(const ifid:tfid);
  begin
  inherited Init;
  Running:=false;
  Errored:=false;
  Completed:=false;
  error:=errNoReply;
  fid:=ifid;
  typeid:=1;
  fWeight:=32;
  fMaxSize:=0;
end;

procedure tFetch.Abort;
  begin
  if Running then transfer.Done;
  if assigned(prev) then prev^.next:=next else RunList:=next;
  if assigned(next) then next^.prev:=prev;
end;
procedure tFetch.Cleanup;
  begin
  inherited Cleanup;
  FreeMem(@self,sizeof(self));
end;

procedure tFetch.SetMaxSize(i:LongWord);
  begin
  assert(i>0);
  if i>fMaxSize then fMaxSize:=i;
end;
procedure tFetch.SetWeight(i:byte);
  begin
  if i>fweight then fweight:=i;
  transfer.weight:=fweight;
end;

procedure tFetch.AddSource(const srce:tNetAddr);
  begin
  if Completed then exit;
  if not Running then begin
    transfer.Init(srce,fid);
    if transfer.error>0 then begin
      Shedule(0,@Handler);
      if transfer.error=4 then raise eXception.Create('Cannot start more transfer jobs');
      Errored:=true;
    exit end;
    transfer.callback:=@Handler;
    transfer.dataf.OpenRW(GetTempName(fid,'prt')); //except
    transfer.weight:=fweight;
    transfer.Start;
    Running:=true;
  end else begin
    //unimplemented
  end;
end;

procedure tFetch.Handler;
  var so:tStoreObject;
  begin
  assert( (running or errored) and (not completed) );
  Running:=false;
  case transfer.error of
    1:begin
      so.HashObjectRename(transfer.dataf,GetTempName(fid,'prt'),false);
      if so.fid=transfer.fid
      then begin
        Completed:=true;
        Errored:=false;
        so.Reference(ObserversCount);
      end else error:=errCorrupt;
    end;
    2:error:=errFileSys;
    3:error:=errNoReply;
    opcode.otFail:error:=errServer;
    opcode.otNotFound:error:=errNotFound;
    else AbstractError;
  end;
  if assigned(prev) then prev^.next:=next else RunList:=next;
  if assigned(next) then next^.prev:=prev;
  assert( (completed or errored) and (not running) );
  if Completed
    then SendEvent(tevComplete,nil)
    else SendEvent(tevError,nil);
end;

procedure tFetch.FromCache;
  begin
  assert(not(running or errored));
  if Completed
    then SendEvent(tevComplete,nil)
    else SendEvent(tevError,nil);
end;

function tFetch.ProgressPct: single;
  begin
  if transfer.Total>0
  then ProgressPct:= transfer.Received/transfer.Total
  else ProgressPct:= transfer.Received;
end;

END.
