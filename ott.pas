unit ott;
{OT test}

INTERFACE
IMPLEMENTATION
uses ServerLoop,MemStream,opcode,NetAddr,Fetch,Store2,dhtLookup;

type t=object
  job:pFetch;
  lookup:dhtLookup.tSearch;
 procedure DoTestFetch;
 //procedure DoTestLookup;
 procedure FetchComplete;
end;

procedure t.DoTestFetch;
 const id1:array [0..19] of byte=($CD,$BF,$75,$83,$B1,$59,$44,$60,$A9,$A5,$CC,$F3,$E8,$E0,$B7,$F1,$3D,$1A,$6B,$DB);
 const id2:array [0..19] of byte=($E2,$05,$D4,$BF,$17,$A8,$5B,$67,$B2,$44,$EF,$FC,$83,$A2,$23,$D0,$1F,$98,$12,$6D);
 begin
 writeln('ott: start fetch');
 job:=FetchObject(id2,'//ip4/127.0.0.1/3512', 48, @FetchComplete);
 if not assigned(job) then FetchComplete;
end;

procedure t.FetchComplete;
  begin
  if (job=nil)or(job^.done) then writeln('ott: fetch complete')
  else begin
    writeln('ott: fetch error ',job^.error);
  end;
end;

var o:t;
BEGIN
shedule(1,@o.DOTestFetch);
end.

