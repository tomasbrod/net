unit ott;
{OT test}

INTERFACE
{todo:
  merge MemStream, Task, NetAddr}
IMPLEMENTATION
uses ServerLoop,ObjectModel,opcode,Fetch,Store2;

type t=object
  job:pFetch;
  {lookup:dhtLookup.tSearch;}
 procedure DoTestFetch;
 //procedure DoTestLookup;
 procedure FetchComplete( task_:tTask_ptr; event:tTaskEvent; data:pointer );
end;

procedure t.DoTestFetch;
  const id1:array [0..19] of byte=($CD,$BF,$75,$83,$B1,$59,$44,$60,$A9,$A5,$CC,$F3,$E8,$E0,$B7,$F1,$3D,$1A,$6B,$DB);
  const id2:array [0..19] of byte=($E2,$05,$D4,$BF,$17,$A8,$5B,$67,$B2,$44,$EF,$FC,$83,$A2,$23,$D0,$1F,$98,$12,$6D);
  var id3:tFID;
  var o:tStoreObject;
  begin
  id3:='D57465C655B1ED5DD6C04949CCA67007DB13EB52';
  if ObjectExists(id3) then FetchComplete(nil, tevUser, @id3)
  else begin
    writeln('ott: start fetch');
    job:=NewFetch(id3);
    job^.Attach(nil, @FetchComplete);
    job^.AddSource('//ip4/147.229.176.19/3511');
    job^.Props(64,0);
  end;
end;

procedure t.FetchComplete( task_:tTask_ptr; event:tTaskEvent; data:pointer );
  var task:pFetch absolute task_;
  var o:tStoreObject;
  begin
  if assigned(task) then begin
    writeln('ott: job complete, error=',task^.error);
    if ord(task^.error)>0 then exit;
  end else writeln('ott: from cache');
  o.Init(tfid(data^));
  o.Reference(+1);
  o.Reference(-1);
end;

var o:t;
BEGIN
writeln('ott: here');
shedule(1,@o.DOTestFetch);
end.

