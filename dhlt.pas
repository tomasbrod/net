unit dhlt;
{DHT Lookup Test}

INTERFACE
IMPLEMENTATION
uses ServerLoop,ObjectModel,opcode,DHT;

TYPE
  tSampleSearch=object(tSearch)
    Values:ansiString;
    constructor Init(const iTarget:tPID);
    protected
    procedure Cleanup; virtual;
    procedure HandleReply(var sAddr: tNetAddr; var sPID: tPID; op:byte; var st:tMemoryStream); virtual;
    end;
  t=object
    SampleSearch:^tSampleSearch;
    procedure DoIt;
    procedure SearchResult( task:tTask_ptr; event:tTaskEvent; data:pointer );
    end;

procedure t.DoIt;
  const id1:array [0..19] of byte=($CD,$BF,$75,$83,$B1,$59,$44,$60,$A9,$A5,$CC,$F3,$E8,$E0,$B7,$F1,$3D,$1A,$6B,$DB);
  begin
  New(SampleSearch);
  with SampleSearch^ do begin
    writeln('dhlt: start lookup');
    Init(id1);
    Attach(@SearchResult);
  end;
end;

procedure t.SearchResult(task:tTask_ptr; event:tTaskEvent; data:pointer );
  begin
  writeln('dhlt: called back ',SampleSearch^.Values);
end;

constructor tSampleSearch.Init(const iTarget:tPID);
  const cReq:string='Hello!';
  begin
  tSearch.Init;
  Target:=iTarget;
  with Query do begin
    Init(200);
    WriteByte(opcode.dhtTestQuery);
    Write(TrID,2);
    Write(DHT.MyID,20);
    Write(Target,20);
    Write(cReq[1],system.length(cReq));
  end;
  LoadNodes;
  Shedule(1,@Step);
end;

procedure tSampleSearch.Cleanup;
  begin
  values:='';
  inherited;
end;

procedure tSampleSearch.HandleReply(var sAddr: tNetAddr; var sPID: tPID; op:byte; var st:tMemoryStream);
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
  end else begin
    writeln('SampleSearch@',string(@self),': Unknown from ',string(sAddr));
    {warning, prehaps?}
  end;
end;

procedure TestQueryHandler(msg:tSMsg);
  var r:tMemoryStream;
  const cResp:string='Schmeterling!';
  var trid:word;
  var sID:^tPID;
  var Target:^tKey20;
  var strmsg:string;
  begin
  msg.st.skip(1);
  msg.st.read(trid,2);
  sID:=msg.st.ReadPtr(20);
  if not DHT.CheckNode(sID^, msg.source, true) then exit;
  Target:=msg.st.ReadPtr(20);
  strmsg:=msg.st.ReadStringAll;
  writeln('dhlt.TestQueryHandler: ',strmsg);
  r.Init(200);
  r.WriteByte(opcode.dhtTestResult);
  r.Write(trid,2);
  r.Write(dht.MyID,20);
  r.Write(cResp[1],length(cResp));
  SendMessage(r.base^,r.length,msg.source);
  r.Free;
end;

var o:t;
BEGIN
  if OptIndex('-dhlt')>0 then begin
    writeln('dhlt: giving dht time to initialize: 3s');
    shedule(3000,@o.DOIT);
  end;
  ServerLoop.SetupOpcode(opcode.dhtTestQuery,@TestQueryHandler);
end.

