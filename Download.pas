unit Download;
{manage downloads}

INTERFACE
uses NetAddr,
     ServerLoop,opcode,MemStream
    ,Store1
    ,Chat
    ,Sha1
    ;
{
 Same idea here as upmgr. Have an tAggr for each peer reporting speed and 
 tJob for each file request saving to file and reqesting missed segments. 
 The aggr should have limit of paralele jobs. Jobs will first be linked in 
 tAggr queue and then started as slots become available.
 
 After node restart, notify requester with a No-Source error. But should 
 not be forgotten. More advanced DM could consult CHK or Category the file 
 was found.
}
type
 pDownloadJob=^tDownloadJob;
 tDownloadJob=object
  total,done:LongWord;
  missc:LongWord;
  state:(stStop,stActive,stDone,stError,stLocalError);
  error:byte;
  error2:byte;
  fid:tFID;
  procedure Start;
  procedure Free;
  procedure Abort;
  protected
  refc:byte;
 end;
function GetJob(const fid:tFID):pDownloadJob;
function NewJob(const source:tNetAddr; const fid: tFID):pDownloadJob;

IMPLEMENTATION
{TODO: cache for chats}
type
 tAggr_ptr=^tAggr;
 tJob=object(tDownloadJob)
  aggr:tAggr_ptr;
  ix:byte;
  so:tStoreObjectInfo;
  ch:^tChat;
  active:boolean;
  procedure Init(const source:tNetAddr; const ifid: tFID);
  procedure MsgDATA(base,length:LongWord; data:pointer);
  procedure Start;
  procedure Free;
  procedure Abort;
  procedure Close;
  private
  HighestRequestBase, RemoteSkipTo :LongWord;
  procedure ReplyOPEN(msg:tSMsg; data:boolean);
  procedure ReplyLSEG(msg:tSMsg; data:boolean);
  procedure HandleFAIL(r:tMemoryStream);
  procedure HandleEPROTO(r:tMemoryStream);
  procedure ReplyDONE(msg:tSMsg; data:boolean);
  procedure MakeRequest;
 end;
{$I DownloadTC.pas}

function GetJob(const fid:tFID):pDownloadJob;
 var a:^tAggr;
 var i:byte;
 var p:^pointer;
 begin
 p:=@AggrChain;
 a:=AggrChain;
 while assigned(a) do begin
  for i:=0 to high(tAggr.Jobs) do if assigned(a^.Jobs[i]) then begin
   if CompareWord(a^.Jobs[i],fid,10)=0 then begin
    GetJob:=a^.Jobs[i];
    Inc(a^.refs);
    assert(a^.Jobs[i]^.ix=i);
    assert(a^.Jobs[i]^.aggr=a);
    exit;
   end else break{for};
  end;
  p:=@a^.next; a:=p^;
 end;
 GetJob:=nil;
end;

function NewJob(const source:tNetAddr; const fid: tFID):pDownloadJob;
 begin
 result:=GetJob(fid);
 if assigned(result) then exit;
 result:=GetMem(sizeof(tJob));
 tJob(pointer(result)^).init(source,fid);
end;

procedure tJob.Init(const source:tNetAddr; const ifid: tFID);
 var dw:^tAggr;
 begin
 refc:=1;
 error:=0;
 error2:=0;
 fid:=ifid;
 active:=false;
 so.Open(fid);
 done:=0;
 missc:=0;
 if so.final then begin
  done:=total;
  state:=stDone;
  aggr:=nil;
  so.Close;
 exit end;
 state:=stStop;
 {todo: initialize Done}
 so.EnableWrite(fid);
 if so.rc<>0 then begin
  state:=stLocalError;
  error:=255;
  error2:=so.rc;
 exit end;
 dw:=GetAggr(source);
 if not assigned(aggr) then begin
  new(dw);
  dw^.Init(source);
  ix:=0;
 end else begin
  ix:=0; while (ix<high(tAggr.Jobs)) and (dw^.Jobs[ix]=nil) do inc(ix);
  assert(dw^.Jobs[ix]=nil);{todo}
 end;
 aggr:=dw;
 dw^.Jobs[ix]:=@self;
 inc(dw^.refs);
 if state=stStop then begin
  New(CH);
  ch^.Init(source);
 end;
 //assert( ((state=stStop)and(rlen>0))or((state=stDone)and(rlen=0)) );
end;

procedure tJob.Start;
 var s:tMemoryStream;
 begin
 writeln('Download: job start');
 assert( state=stStop );
 state:=stActive;
 ch^.Callback:=@ReplyOPEN;
 ch^.streaminit(s,33);
 {service}s.WriteByte(opcode.upFileServer);
 {channel}s.WriteByte(ix);
 {opcode }s.WriteByte(opcode.upOPEN);
 {file   }s.Write(fid,20);
 {todo: request first segment}
 ch^.Send(s);
end;
procedure tJob.ReplyOPEN(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.stream;
 var op:byte;
 var rsize:LongWord;
 var rfinal:byte;
 begin
 if not data then exit;
 op:=msg.stream.ReadByte;
 {valid responses: INFO FAIL EPROTO}
 if op=upFAIL then HandleFAIL(r)
 else if op=upINFO then begin
  rsize  :=r.ReadWord(4);
  rfinal :=r.readbyte;
  writeln('INFO size=',rsize,' final=',rfinal);
  self.total:=rsize;
  if so.length<>rsize then begin
   if so.length>0 then writeln('Download: warning: size mismatch!');
   so.SetFLength(rsize);
  end;
  MakeRequest;
 end
 else HandleEPROTO(r);
end;
{Strategy for fixing holes without waiting for DONE message:
 * stuff all small segments to LSEG
 * put large one at end
 * when datagrams from the large arrive, Repeat
}

procedure tJob.MakeRequest;
 var s:tMemoryStream;
 var b,l,trl:LongWord;
 var cnt:byte;
 var mst:Pointer;
 const clim=83886080;
 begin
 write('Download: job MakeRequest');
 mst:=nil;
 trl:=0;
 cnt:=0;
 ch^.Callback:=@ReplyLSEG;
 ch^.streaminit(s,180);
 s.WriteByte(upLSEG);
 repeat
  {todo: skipto}
  so.GetMiss(b,l,mst);
  if l=0 then break;
  if (trl+l)>clim then l:=clim-trl;
  write(' ',b,'+',l);
  s.WriteWord(b,4);
  s.WriteWord(l,4);
  inc(trl,l);
  inc(cnt);
 until (s.WrBufLen<8)or(trl>=clim);
 writeln(' for ',trl,'B in ',cnt,' ',s.Length);
 if trl=0 then begin
  state:=stDone;
  writeln('Verifu!!!!!');
  so.VerifyAndReset;
  if not so.final then begin
   state:=stLocalError;
   error:=254;
   writeln('Verifu Faialed!!!!!');
  end;
  Aggr^.Stop(ix);
  Close;
 exit end;
 ch^.Send(s);
 if cnt>1 then HighestRequestBase:=b else HighestRequestBase:=$FFFFFFFF;
end;
procedure tJob.ReplyLSEG(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.stream;
 var op:byte;
 var avail:LongWord;
 begin
 if not data then exit;
 op:=msg.stream.ReadByte;
 {valid responses: SEGOK UNAVL FAIL}
 if op=upFAIL then HandleFAIL(r)
 else if op=upUNAVL then begin
  avail:=msg.stream.ReadWord(4);
  writeln('Download: job ReplyLSEG: UNAVL avail=',avail);
  if avail=0 then begin
   state:=stLocalError;
   error:=253;
   Close;
  end else begin
   RemoteSkipTo:=avail;
   MakeRequest;
  end;
 end
 else if op=upSEGOK then begin
  avail:=msg.stream.ReadWord(4);
  MissC:=avail;
  writeln('Download: job ReplyLSEG: SEGOK avail=',avail);
  aggr^.Start(ix);
  ch^.Callback:=@ReplyDONE;
  ch^.Ack;
 end
 else if op=upDONE then begin
 end {ignore, done is sent async so it can hang in-flight a bit}
 else HandleEPROTO(r);
end;
procedure tJob.ReplyDONE(msg:tSMsg; data:boolean);
 var r:tMemoryStream absolute msg.stream;
 var op:byte;
 begin
 if not data then exit;
 op:=msg.stream.ReadByte;
 {valid responses: DONE}
 if op=upDONE then begin
  writeln('Download: ReplyDONE: DONE, miss=',MissC);
  MakeRequest;
 end else HandleEPROTO(r);
end;
procedure tJob.HandleFAIL(r:tMemoryStream);
 begin
 writeln('Download: FAIL');
 state:=stError;
 try
  error:=r.readByte;
  error2:=r.readByte;
 except end;
 Close;
end;
procedure tJob.HandleEPROTO(r:tMemoryStream);
 begin
 r.Seek(r.position-1);
 try error2:=r.ReadByte; except end;
 writeln('Download: EPROTO ',error2);
 state:=stLocalError;
 error:=252;
 Close;
end;

procedure tJob.Close;
 var s:tMemoryStream;
 begin
 ch^.streaminit(s,2);
 {opcode }s.WriteByte(opcode.upClose);
 ch^.Send(s);
 ch^.Close;
 writeln('chat close');
 aggr^.Stop(ix);
end;
procedure tJob.Abort;
 begin
 assert(state=stActive);
 state:=stLocalError;
 error:=251;
 Close;
end;

procedure tJob.MsgDATA(base,length:LongWord; data:pointer);
 begin
 so.WriteSeg(base,length,data);
 done:=done+length;
 if MissC<=length
 then MakeRequest
 else dec(MissC,length);
 if base>=HighestRequestBase then {TODO, last segment in list, MakeRequest};
end;

procedure tJob.Free;
 begin
 Dec(refc);
 if refc=0 then begin
  writeln('Download: job closing');
  if state=stStop then Close
  else if state=stActive then Abort;
  {tu ja EAV, nieco s pointermi}
  if assigned(aggr) then begin
   aggr^.Jobs[ix]:=nil;
   dec(aggr^.refs);
   if aggr^.refs=0 then aggr^.Done;
  end;
  if state<>stDone then so.Close;
  FreeMem(@self,sizeof(tJob));
 end else writeln('not closing ',refc);
end;

procedure tDownloadJob.Start;
 begin
 tJob(pointer(@self)^).Start;
end;
procedure tDownloadJob.Abort;
 begin
 tJob(pointer(@self)^).Abort;
end;
procedure tDownloadJob.Free;
 begin
 tJob(pointer(@self)^).Free;
end;
END.