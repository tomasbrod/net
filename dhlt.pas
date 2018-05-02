unit dhlt;
{DHT Lookup Test}

INTERFACE
IMPLEMENTATION
uses Classes,ServerLoop,ObjectModel,opcode,DHT;

TYPE
  tSampleSearch=class(tCustomSearch)
    Values:ansiString;
    constructor Create(const iTarget:tPID);
    protected
    procedure Cleanup; override;
    procedure Exhausted; override;
    procedure HandleReply(var sAddr: tNetAddr; var sPID: tPID; op:byte; st:tStream); override;
    end;
  t=object
    SampleSearch:tSampleSearch;
    procedure DoIt;
    procedure SearchResult( task:tTask; event:tTaskEvent; data:pointer );
    end;

procedure t.DoIt;
  const id1:array [0..19] of byte=($CD,$BF,$75,$83,$B1,$59,$44,$60,$A9,$A5,$CC,$F3,$E8,$E0,$B7,$F1,$3D,$1A,$6B,$DB);
  begin
  SampleSearch:=tSampleSearch.Create(id1);
  SampleSearch.Attach(@SearchResult);
  writeln('dhlt: start lookup');
end;

procedure t.SearchResult( task:tTask; event:tTaskEvent; data:pointer );
  begin
  writeln('dhlt: called back ',event,assigned(data),SampleSearch.Values);
end;

constructor tSampleSearch.Create(const iTarget:tPID);
  const cReq:string='Hello!';
  begin
  inherited Create;
  Target:=iTarget;
  Query:=tMemoryStream.Create;
  with Query do begin
    w1(opcode.dhtTestQuery);
    wb(TrID,2);
    wb(DHT.MyID,20);
    wb(Target,20);
    wb(cReq[1],system.length(cReq));
  end;
  LoadNodes;
  Shedule(1,@Step);
end;

procedure tSampleSearch.Exhausted;
  begin
  if Values=''
  then SendEvent(tevError,nil)
  else SendEvent(tevComplete,nil);
end;

procedure tSampleSearch.Cleanup;
  begin
  values:='';
  inherited;
end;

procedure tSampleSearch.HandleReply(var sAddr: tNetAddr; var sPID: tPID; op:byte; st:tStream);
  var node:integer;
  begin
  node:=self.AddNode(sPID, sAddr);
  if OP=opcode.dhtNodes then begin
    writeln('SampleSearch@',string(@self),': Nodes from ',string(sAddr));
    if node<>-1 then nodes[node].rplc:=2;
    self.AddNodes(st);
  end else if OP=opcode.dhtTestResult then begin
    writeln('SampleSearch@',string(@self),': Result from ',string(sAddr));
    if node<>-1 then nodes[node].rplc:=1;
    setlength(self.values,st.left);
    st.read(self.values[1],length(self.values));
    SendEvent(tevComplete,nil); exit;
  end else begin
    writeln('SampleSearch@',string(@self),': Unknown from ',string(sAddr));
    {warning, prehaps?}
  end;
end;

procedure TestQueryHandler(msg:tSMsg);
  var r:tMemoryStream;
  const cResp:string='Schmeterling!';
  var trid:word;
  var sID:tPID;
  var Target:tKey20;
  var strmsg:string;
  begin
  msg.st.skip(1);
  msg.st.rb(trid,2);
  msg.st.rb(sID,20);
  if not DHT.CheckNode(sID, msg.source, true) then exit;
  msg.st.rb(Target,20);
  strmsg:=msg.st.ReadStringAll;
  writeln('dhlt.TestQueryHandler: ',strmsg);
  r:=tMemoryStream.Create; try
  r.w1(opcode.dhtTestResult);
  r.wb(trid,2);
  r.wb(dht.MyID,20);
  r.wb(cResp[1],length(cResp));
  SendMessage(r.Memory^,r.size,msg.source);
  finally r.Free end;
end;

var o:t;
BEGIN
  if OptIndex('-dhlt')>0 then begin
    writeln('dhlt: giving dht time to initialize: 3s');
    shedule(3000,@o.DOIT);
  end;
  ServerLoop.SetupOpcode(opcode.dhtTestQuery,@TestQueryHandler);
end.

