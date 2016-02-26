unit TestPRL;
interface
uses Store2,ServerLoop,MemStream,ProfileCache,opcode,NetAddr,dhtLookup;

implementation
type t=object
  prid:tFID;
  oi:byte;
  job:^tSearch;
  procedure init;
  procedure SearchResult(const Source:tNetAddr; scaps:byte; exl:word; exp:pointer);
  end;
var O:t;

procedure t.Init;
  begin
  oi:=OptIndex('-test-prl');
  if (oi=0) then exit;
  assert(OptParamCount(oi)=1);
  New(job);job^.Init;
  job^.Target:=paramstr(oi+1);
  job^.Caps:=capProfile;
  job^.Callback:=@SearchResult;
  writeln('TestPRL.Init: going to lookup profile ',string(job^.Target));
  Shedule(3000,@job^.Start);
end;

procedure t.SearchResult(const Source:tNetAddr; scaps:byte; exl:word; exp:pointer);
  begin
  writeln('TestRPL.SearchResult: ',string(source),' caps=',scaps,' extra[',exl,']');
end;

BEGIN
 o.init;
END.
