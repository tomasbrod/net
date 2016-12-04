UNIT Fetch;

INTERFACE
USES ObjectModel,ObjTrans,Store2;

type tFetchError=(fjsComplete=0, fjsIOError, fjsLarge, fjsNotFound, fjsFull, fjsCorrupt, fjsNoResp, fjsFail, fjsNoSource, fjsNew);
type tSourceItem=object
  Address: tNetAddr;
  state: tFetchError;
  score: integer;
  Distance: word;
  Speed: single unimplemented;
  next: ^tSourceItem;
end;

{error priority: Success, Used, Large, NotFound, Full, Corrupt, NoResp, Fail}

type
  pFetch=^tFetchJob;

tFetchJob=object(tTask)
  public
  constructor Init(const ifid:tFID);
  function ProgressPct: single; virtual; {scaled by 10000}
  protected
  procedure Abort; virtual;
  procedure Cleanup; virtual;
  public
  Error: tFetchError;
  procedure AddSource(const srce:tNetAddr);
  procedure Props(iweight:byte; imaxsize:LongWord);
  protected
  procedure DelayedStart;
  procedure Handler;
  function CheckObjectHash: boolean;
  protected
  Weight:byte;
  MaxSize:LongWord;
  AltSrc, CurSrc: ^tSourceItem;
  Next, Prev: ^tFetchJob;
  SheduledStart: boolean;
  Trx: ObjTrans.tJob;
end;

function NewFetch(fid:tFID): pFetch;

IMPLEMENTATION
uses SysUtils,ServerLoop,OpCode;

var Jobs: ^tFetchJob;

procedure tFetchJob.DelayedStart;
  var s:^tSourceItem;
  var a:^ObjTrans.tAggr;
  begin
  {super-inteligent function}
  if not (error in [fjsComplete,fjsIOError]) then begin
    {select best source}
    s:=CurSrc;
    while assigned(s) do begin
      {criteria: fjsNew, Distance, RefC}
      if s^.state in [fjsNew {,fjsFull}] then begin
        writeln(Format('Fetch@%P.DelayedStart: address=%S',[@self,string(s^.address)]));
        s^.state:=fjsNoSource;
        a:=ObjTrans.GetAggr(s^.Address, True);
        if (a=nil) or (a^.maxchan=a^.refc) then begin
          s^.state:=fjsFull;
          if fjsFull<error then error:=fjsFull;
          writeln('Full');
          continue;
        end;
        CurSrc:=s;
        Trx.Init;
        Trx.Weight:=Weight;
        Trx.Callback:=@Handler;
        Trx.Aggr:=a;
        Trx.DataF.Trunc(0);
        Trx.Start;
        if Trx.Error=0 then (**)EXIT(**); //success :)
        s^.state:=fjsFail;
        if fjsFail<error then error:=fjsFail;
      end;
      s:=s^.next;
    end;
    if error>fjsNoSource then error:=fjsNoSource;
  end;
  {no source could be used}
  {maybe if full, todo: shedule to try again later}
  {send event}
  writeln(Format('Fetch@%P.DelayedStart SendEvent %D',[@self,ord(error)]));
  if error=fjsComplete
    then SendEvent(tevComplete,@Trx.FID)
    else SendEvent(tevError,@Trx.FID);
end;

procedure tFetchJob.Handler;
  var LocError: tFetchError;
  begin
  {evaluate Trx event}
  writeln(Format('Fetch@%P.Handler %D',[@self,Trx.Error]));
  case Trx.Error of
    {0progress,1done,2ioerror,3timeout,4full,OT-DLEs}
    0: begin
      {check size, not used, not implemented in Trx}
      end;
    1: begin
      LocError:=fjsComplete;
      if not CheckObjectHash then LocError:=fjsCorrupt;
      end;
    2: LocError:=fjsIOError; {io-failure}
    3: LocError:=fjsNoResp;
    4: LocError:=fjsFull; {server fail msg}
    opcode.otFail: LocError:=fjsFail;
    opcode.otNotFound: LocError:=fjsNotFound;
    else LocError:=fjsFail;
  end;
  {save Trx event to source item}
  if assigned(CurSrc) then cursrc^.state:=LocError;
  if locerror<error then error:=locerror;
  //writeln('Fetch@',string(@self),'.Handler locerror=',LocError,' globError=',Error);
  {more...}
  Trx.Done;
  DelayedStart;
end;

function tFetchJob.CheckObjectHash: boolean;
  var so:tStoreObject;
  var hash:tFID;
  begin
  {file from Trx, file-name, not temporary, not prehashed}
  writeln(Format('Fetch@%P Hashing %DB',[@self,Trx.Total]));
  Trx.DataF.Seek(0);
  {todo: Hash in second thread}
  HashAndCopy(Trx.DataF, nil, hash, Trx.DataF.Length);
  so.InsertRename(Trx.DataF,Store2.GetTempName(Trx.fid,'prt'),hash);
  {if id matches requested add enough refs else unref}
  result:=so.FID=Trx.FID;
  if result then begin
    so.Reference(ObserversCount-1);
    so.Done;
  end
    else so.Reference(-1+1);
end;

function NewFetch(fid:tFID): pFetch;
  var p:^tFetchJob;
  begin
  result:=nil;
  {check for running Trx}
  p:=Jobs;
  while assigned(p) do if p^.Trx.fid=fid then break else p:=p^.next;
  {start Trx or attach to the running}
  if not assigned(p) then begin
    new(p,Init(fid));
    if assigned(Jobs) then Jobs^.prev:=p;
    p^.next:=Jobs; p^.prev:=nil; Jobs:=p;
  end;
  result:=p;
end;

procedure tFetchJob.Props(iweight:byte; imaxsize:LongWord);
  begin
  assert((iweight>0)and(iweight<128));
  if imaxsize>MaxSize then MaxSize:=imaxsize;
  if iweight>weight then weight:=iweight;
  Trx.Weight:=Weight;
end;

constructor tFetchjob.Init(const ifid:tFID);
  begin
  AltSrc:=nil; Next:=nil; Prev:=nil; CurSrc:=nil;
  Inherited Init;
  Trx.Init;
  Trx.FID:=iFID;
  Weight:=1;
  MaxSize:=1;
  SheduledStart:=false;
  Trx.DataF.OpenRW(GetTempName(Trx.FID,'prt'));
end;

procedure tFetchJob.Cleanup;
  var itm:^tSourceItem;
  var cp:^tFetchJob;
  var pp:^pointer;
  begin
  writeln(Format('Fetch@%P.Cleanup',[@self]));
  while assigned(AltSrc) do begin
    itm:=AltSrc;
    AltSrc:=itm^.next;
    Dispose(itm);
  end;
  if error<>fjsComplete {hashing closes the file}
    then Trx.DataF.Done;
  if assigned(prev) then prev^.next:=next;
  if assigned(next) then next^.prev:=prev;
  if Jobs=@self then Jobs:=next;
  Inherited;
  FreeMem(@self,sizeof(self));
end;

procedure tFetchJob.Abort;
  begin
  writeln(Format('Fetch@%P.Abort %S',[@self,string(Trx.FID)]));
  if SheduledStart then UnShedule(@DelayedStart);
  Trx.Done;
  Inherited;
end;

function tFetchjob.ProgressPct: single;
  begin
  ProgressPct:=9;
end;

procedure tFetchjob.AddSource(const srce:tNetAddr);
  var asr: ^tSourceItem;
  var pp: ^pointer;
  var a:^ObjTrans.tAggr;
  var score,tmp:integer;
  begin
  {calculate score}
  a:=ObjTrans.GetAggr(srce, false);
  score:=17;
  if assigned(a) then begin
    tmp:=a^.refc;
    if tmp=a^.maxchan then score:=9999;
    if tmp>14
      then inc(score,70)
      else inc(score,tmp);
    tmp:=a^.Distance;
    if tmp<65535
      then inc(score,3*tmp)
      else inc(score,16);
    a^.ResetIdle;
  end;
  {check duplicates}
  pp:=@AltSrc; asr:=pp^;
  while assigned(asr) do begin
    if asr^.Address=srce then exit;
    if asr^.score<=score then pp:=@asr^.next;
    asr:=asr^.next;
  end;
  {add to source list}
  writeln(Format('Fetch@%P.AddSource %S score %D fid=%S',[@self,string(srce),score,string(Trx.FID)]));
  New(asr); asr^.next:=pp^; pp^:=asr;
  if CurSrc=nil then CurSrc:=asr;
  asr^.Address:=srce;
  asr^.Score:=score;
  if assigned(a) then asr^.Distance:=a^.distance else asr^.Distance:=65535;
  asr^.state:=fjsNew;
  {shedule start}
  error:=fjsNew;
  if not SheduledStart then Shedule(1,@DelayedStart);
  SheduledStart:=true;
end;

END.
