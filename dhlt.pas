unit dhlt;
{DHT Lookup Test}

INTERFACE
IMPLEMENTATION
uses ServerLoop,MemStream,opcode,NetAddr,dhtLookup;

type t=object
  job:tSearch;
 procedure DoIt;
 procedure SearchResult(const Source:tNetAddr; scaps:byte; exl:word; exp:pointer);
end;

procedure t.DoIt;
 const id1:array [0..19] of byte=($CD,$BF,$75,$83,$B1,$59,$44,$60,$A9,$A5,$CC,$F3,$E8,$E0,$B7,$F1,$3D,$1A,$6B,$DB);
 begin
 job.Init;
 job.Target:=id1;
 job.Caps:=32;
 job.Extra:='Hello!';
 job.Callback:=@SearchResult;
 writeln('dhlt: start lookup');
 job.Start;
end;

procedure t.SearchResult(const Source:tNetAddr; scaps:byte; exl:word; exp:pointer);
  begin
  writeln('dhlt: called back ',string(source),' caps=',scaps,' exl=',exl);
end;

var o:t;
BEGIN
shedule(1,@o.DOIT);
end.

