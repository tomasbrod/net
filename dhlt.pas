unit dhlt;
{DHT Lookup Test}

INTERFACE
IMPLEMENTATION
uses ServerLoop,MemStream,opcode,NetAddr,dhtLookup,dht;

type t=object
  job:^tSearch;
 procedure DoIt;
 procedure SearchResult(const Source:tNetAddr; scaps:byte; exl:word; exp:pointer);
end;

procedure t.DoIt;
 const id1:array [0..19] of byte=($CD,$BF,$75,$83,$B1,$59,$44,$60,$A9,$A5,$CC,$F3,$E8,$E0,$B7,$F1,$3D,$1A,$6B,$DB);
 begin
 New(job);
 job^.Init;
 job^.Target:=id1;
 job^.Caps:=32;
 job^.Extra:='Hello!';
 job^.Callback:=@SearchResult;
 writeln('dhlt: start lookup');
 job^.Start;
end;

procedure t.SearchResult(const Source:tNetAddr; scaps:byte; exl:word; exp:pointer);
  var rx:string[100];
  begin
  SetLength(rx,exl);
  Move(exp^,rx[1],exl);
  writeln('dhlt: called back ',string(source),' caps=',scaps,' extra[',exl,']=',rx);
end;

function Cap32Handler(const source:tNetAddr; caps:byte; const Target:tPID; var extra:tMemoryStream):boolean;
  var r:tMemoryStream;
  var rx:string[100];
  const cResp:string='Schmeterling!';
  begin
  SetLength(rx,extra.RdBufLen);
  extra.Read(rx[1],extra.RdBufLen);
  writeln('dhlt.Cap32Handler: ',rx);
  r.Init(200);
  r.WriteByte(opcode.dhtCapable);
  r.Write(dht.MyID,20);
  r.Write(Target,20);
  r.WriteByte(32);
  r.Write(cResp[1],length(cResp));
  SendMessage(r.base^,r.length,source);
  FreeMem(r.base,r.size);
  result:=true;
end;

var o:t;
BEGIN
if OptIndex('-dhlt')>0 then begin
  writeln('dhlt: giving dht time to initialize: 3s');
  shedule(3000,@o.DOIT);
end;
dht.RegisterCapability(32,@Cap32Handler);
end.

