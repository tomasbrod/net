UNIT Transfer;

{
 To send and recieve CHK files over brodnet.
 Also to assemble pieced and parted files.
 
 TODO: dispose parts completly, tData should have complete index!
       - we request 4 chunks, reply will be delayed, we request the chunks 
       again, reply will come, we request another chunk but reply to prev 
       will come, corrupting the transfer.
 
}

INTERFACE
USES Peers
    ,Keys
    ,NetAddr
    ,SysUtils
    ;

const cChunkLength=512{b}; {should not be changed}
const cMaxChunksPerRequest=16; {limit to chunks that we serve for single request}
const cChunksPerRequest=6;     {limit to chunks that we request in single request}
const cRetryPeriod = 2000{ms} /MSecsPerDay; {if reply not arrive under this, we send another request}
const cMaxDelta = Peers.cMaxDelta {10/MinutesPerDay}; {if nothing is received under this, transfer is aborted}
const cTimeWait = 5000 /MSecsPerDay;

CONST
 cRequest=4;
 cInfo   =5;
 cData   =6;

type tFID=object(Keys.tHash)
 end;
 
TYPE {--Packets--}

 tRequest=object(Peers.tPacket)
  procedure Create( iid :tFID; itrid: byte; ichunk: longword; icount :Word );
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t);
  private
  id     :tFID;
  chunk  :NetAddr.Word4;
  TrID   :byte;
  count  :byte;
 end;
 
 {$Z1}tInfoMetaType= (imtNone);
 
 tInfo=object(tPacket)
  procedure Create( const aTrID:byte; const aFID:tFID );
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t);
  private
  TrID  :byte;
  count :NetAddr.Word4; {count*cChunkLength = size of the file}
  metadata:record
   case metatype:tInfoMetaType of
   imtNone:( b:byte; );
  end;
 end;
 
 tData=object(Peers.tPacket)
  procedure Handle( const from: NetAddr.t; length:longword );
  procedure CreateSend( const aTrID:byte; const aFID:tFID; aChunk:longword; const rcpt: NetAddr.t);
  private
  TrID :byte;
  part :Word4;
  PayLoad: array [1..cChunkLength] of byte;
 end;

 {
 tDataShort=object(Peers.tPacket)
  procedure Handle( const from: NetAddr.t; length:longword );
  private
  TrID :byte;
  part :byte;
  PayLoad: array [1..cChunkLength] of byte;
 end;
 }
 
 tError=object(Peers.tPacket) 
 end unimplemented;

type tDataFile=file of byte;

var OnRecv  :procedure( id :tFID; by :byte );
var OnProgress :procedure( id :tFID; done,total:longword; by: byte );

procedure SendFile( id: tFID );
 experimental; deprecated {all files must be requested};

procedure RequestFile( const source :netaddr.t; id :tFID );
 experimental;
procedure RequestFile( const src :netaddr.t; id :tFID; by: byte );

procedure RecvFileAbort( id :tFID );
 unimplemented;

var OnNoSrc :procedure( id :tFID; by :byte );
 {To update source, call RequestFile with valid source address.}

procedure DoRetry;
procedure NotifyQuit;

IMPLEMENTATION
uses 
     DataBase
    ,LinkedList
    ;

{ Data storage }

type tSuspendedTransfer=object
 fid        :tFID;
 completed  :longword; {also marks the next chunk to be requested}
                       { should be in tTransfer, becouse is computed from filesize...}
 total      :longword;
 by         :byte;
 InfoReceived:Boolean;
 end;

type tPendingInfo=object
 since:tDateTime;
 part:byte deprecated;
 retry:byte;
end;

type tTransfer=object(tSuspendedTransfer)
 storage    :tDataFile;
 source     :netaddr.t;
 trid       :byte;
 received   :byte;
 pending    :set of 0..cChunksPerRequest-1;
 last       :tDateTime;
 Terminated :boolean;
 Timeouted  :boolean;
 procedure DoRun;
 procedure HandleInf(count:longword);
 procedure HandleDat(part:longword; var PayLoad; length:longword );
 procedure Done;
 private
 procedure DispatchEvent;
 procedure SetPending(i,c:word);
 end;

const cMaxTransfers=32;
var TransferList:array [1..cMaxTransfers] of ^tTransfer;

function TransferByFID(ifid:tFID):word; forward;

procedure Load( out tra: tSuspendedTransfer; const fid: tFID ); forward;
procedure UnSave( const fid: tFID ); forward;

function TransferByFID(ifid:tFID):word;
 begin
 result:=1;
 while (result<=cMaxTransfers)and( (TransferList[result]=nil )or( TransferList[result]^.fid<>ifid)  ) do inc(result);
 if result>cMaxTransfers then raise eSearch.Create;
end;

procedure RequestFile( const source :netaddr.t; id :tFID ); inline;
 begin RequestFile(source, id, 0 ); end;

procedure RequestFile( const src :netaddr.t; id :tFID; by: byte );
 var i:word;
 procedure Create;
  begin
  i:=1;
  while (i<=cMaxTransfers)and(assigned(TransferList[i])) do inc(i);
  if i>cMaxTransfers then raise Exception.Create('Too many transfers');
  new(TransferList[i]);
 end;
 procedure Resume;
  var Tr:tSuspendedTransfer;
  begin
  Load(Tr,id);
  Create;
  with TransferList[i]^ do begin
   fid        :=tr.fid;
   completed  :=tr.completed;
   total      :=tr.total;
   by         :=tr.by;
   InfoReceived:=tr.InfoReceived;
  end;
 end;

 begin
 try
  i:=TransferByFID(id);
  log.error('Try to add duplicate transfer by '+inttostr(by));
  exit;
  AbstractError;
 except
  on eSearch do try Resume;
  except
   on eSearch do begin
    Create;
    with TransferList[i]^ do begin
     fid        :=id;
     completed  :=0;
     total      :=0;
     by         :=by;
     InfoReceived:=false;
     {check for presence of downloaded}
     DataBase.dbAssign(storage,'chk'+DirectorySeparator+string(id)); reset(storage); try
     if FileSize(storage)>0 then begin
      log.info('File already downloaded');
      InfoReceived:=true; Total:=(FileSize(storage) div cChunkLength)+1; Completed:=Total;
     end; finally close(storage); end;
    end;
   end;
  end;
 end;
 with TransferList[i]^ do begin
  source     :=src;
  received   :=0;
  FillChar(pending,sizeof(pending),0);
  last       :=now;
  Terminated :=false;
  timeouted  :=false;
  trid       :=i;
  DataBase.dbAssign(storage,'chk'+DirectorySeparator+string(id)); reset(storage);
  log.debug('Transfer '+IntToStr(trid)+' set source='+string(source)+' fid='+string(fid));
  DoRun; {TEST}
 end;
end;

procedure RecvFileAbort( id :tFID );
 var i:word;
 begin
 i:=TransferByFID(id);
 with TransferList[i]^ do begin
  Done;
 end;
end;

procedure tInfo.Handle( const from: NetAddr.t);
 begin
 if 
    (TrID<high(TransferList))
    and assigned(TransferList[TrID])
 then begin
  TransferList[TrID]^.HandleInf(count);
 end;
end;

procedure tData.Handle( const from: NetAddr.t; length:longword );
 var pl:longword;
 begin
 pl:=length-(sizeof(self)-sizeof(PayLoad));
 if 
    (TrID<high(TransferList))
    and assigned(TransferList[TrID])
 then begin
  TransferList[TrID]^.HandleDat(part,PayLoad,pl);
 end;
end;

procedure DoRetry;
 var i:word;
 var tr:^tTransfer;
 begin
 for i:=1 to high(TransferList) do if assigned(TransferList[i]) then begin
  if TransferList[i]^.Terminated then begin
   tr:=TransferList[i];
   TransferList[i]:=nil;
   if (tr^.timeouted)and assigned(OnNoSrc) then OnNoSrc(tr^.fid,tr^.by);
   log.debug('Transfer disposed '+string(tr^.fid));
   dispose(tr);
  end else begin
   log.debug('Transfer.DoRun '+string(TransferList[i]^.fid));
   TransferList[i]^.DoRun;
  end;
 end;
end;

procedure tTransfer.Done;
 begin
 SetPending(0,0);
 Close(storage);
 Terminated:=true;
end;

procedure Load( out tra: tSuspendedTransfer; const fid: tFID );
 var f:file of tSuspendedTransfer;
 begin
 DataBase.dbAssign(f,'transfer.dat');
 reset(f);
 try
  repeat
   if eof(f) then raise eSearch.Create;
   read(f,tra);
   log.debug('s '+string(tra.fid));
  until tra.fid=fid;
 finally
  close(f);
 end;
end;

procedure UnSave( const fid: tFID );
 var f:file of tSuspendedTransfer;
 var Tr:tSuspendedTransfer;
 begin
 DataBase.dbAssign(f,'transfer.dat'); reset(f);
 try 
  while not eof(f) do begin
   read(f,tr);
   if tr.fid=fid then DataBase.UnInsert(f{,tr});
  end;
 finally close(f); end;
end;

procedure Save(var tr:tSuspendedTransfer);
 var f:file of tSuspendedTransfer; begin
 log.debug('save');
 UnSave(tr.fid);
 DataBase.dbAssign(f,'transfer.dat'); reset(f);
 try write(f,tr);
 finally close(f); end;
end;

procedure tTransfer.HandleInf(count:longword);
 begin
 last:=now;
 if (not Terminated)and(not InfoReceived) then begin
  total:=count;
  InfoReceived:=true;
  log.debug(string(fid)+' info received, total='+IntToStr(total));
  Save(self);
  DispatchEvent;
 end else log.error('protocol desync: info received in invalid state for '+string(fid));
end;

procedure tTransfer.HandleDat(part:longword; var PayLoad; length:longword );
 procedure NextBatch;
  var req:tRequest;
  var requested:byte;
  begin
  received:=0;
  requested:=cChunksPerRequest;
  if requested>(total-completed) then requested:=total-completed;
  (*log.debug('requesting '+IntToStr(requested)+' more chunks');*)
  SetPending(0,requested);
  req.Create(fid,TrID,completed,requested);
  req.Send(source);
 end;
 var islast:boolean;
 begin
 last:=now;
 if (not Terminated) then begin
  assert(((part-completed)>=0)and((part-completed)<cChunksPerRequest));
  if (part-completed) in pending then begin
   assert(length<=cChunkLength);
   isLast:=InfoReceived and (completed+received+1=total);
   if (length<cChunkLength)and(not islast) then raise Exception.Create('Incomplete datapacket in middle of file');
   Seek(storage,part*cChunkLength);
   BlockWrite(storage,PayLoad,length);
   Exclude(pending,part-completed);
   Inc(received);
   (*log.debug(string(fid)+' data received, total='+IntToStr(total)+' compl='+IntToStr(completed)+' req='+IntToStr(requested)+' rec='+IntToStr(received));*)
   if (pending=[])or IsLast then begin
    (*log.debug('batch complete');*)
    completed:=completed+received;
    if InfoReceived then begin
     if not IsLast then NextBatch; (*else log.debug('file completed');*)
    end {else wait info};
   end {else wait more};
  end {else error};
 end {else log.error('protocol desync: data received in invalid state')};
end;

procedure tTransfer.DispatchEvent;
 unimplemented;
 begin
 (*log.info('Transfer '+string(fid)+': '+IntToStr(completed)+'/'+IntToStr(total));*)
 if assigned(OnProgress) then OnProgress(fid,completed,total,by);
 if (completed=total)and assigned(OnRecv) then OnRecv(fid,by);
 {   if assigned(OnNoSrc) then OnNoSrc(fid,by);}
end;

(*
 fid        :tFID;
 completed  :longword; {also marks the next chunk to be requested}
 total      :longword;
 by         :byte;
 flags: set of (fInfoReceived,fStarted,fSuspended,fAborted);
 storage    :tDataFile;
 source     :netaddr.t;
 requested  :word;
 received   :word;
 pending    :array [0..cChunksPerRequest-1] of ^tPendingInfo;
 last       :tDateTime;
*)
procedure tTransfer.SetPending(i,c:word);
 var z:word;
 begin
 for z:=low(pending) to high(pending) do begin
  if (z>=i)and(z<i+c) then begin
   Include(pending,z);
  end else Exclude(pending,z);
 end;
end;

procedure tTransfer.DoRun;
 function Verify:boolean;
  var hash:tHash;
  begin
  seek(storage,0);
  hash.Compute(storage,filesize(storage));
  if hash<>fid then begin
   log.error('Transfer Hash Mismatch  in '+string(fid)+' from '+string(source));
   log.debug('Hash of file='+string(hash));
   Completed:=0;
   InfoReceived:=false;
   pending:=[];
   result:=false;
   AbstractError;
 end else begin
  log.debug('Transfer hash matched :)');
  result:=true;
 end; end;
 var req:tRequest;
 var rqc:word;
 var i:word;
 begin
 if (completed=total)and(InfoReceived) then begin
  if Verify then begin
   UnSave(fid);
   Done; end;
 end else
 if pending=[] then begin
  log.debug('Starting not started');
  req.Create( fid, trid, completed, cChunksPerRequest );
  SetPending( 0, cChunksPerRequest ); received:=0;
  req.Send(source);
  Save(self);
 end else
 if not InfoReceived then begin
  log.debug('Re-requesting info');
  req.Create( fid, trid, 0, 0 );
  req.Send(source);
 end else
 if (now-last)>cMaxDelta then begin
  log.debug('Timeout => suspend');
  Save(self);
  Timeouted:=true;
  Done;
 end else
 if (now-last)>cRetryPeriod then begin
  log.debug('Retry pending ');
  i:=0;
  while (i<=high(pending)) and (pending*[i]=[]) do inc(i); //find first assigned
  rqc:=1;
  while (i+rqc<=high(pending)) and (i+rqc in pending) do inc(rqc); //find last assigned
  if i<=high(pending) then begin
   log.debug('req retry '+intToStr(rqc));
   req.Create( fid, trid, completed+i, rqc );
   req.Send(source);
  end;
  Save(self);
 end else begin
  log.debug('nothing special');
  Save(self);
 end;
 DispatchEvent;
end;
 {if now-last>cRetryTimeout}
 {todo:
  - retry pending		IMPL
  - dispose aborted and suspended	UP
  - suspend				IMPL
  - save	IMPL
  - OnNoSrc	UP
  - start not started	IMPL
  - request info		IMPL
  - request next		IMPL
 }

procedure NotifyQuit;
 var i:word;
 var c:word=0;
 begin
 for i:=1 to high(TransferList) do if assigned(TransferList[i]) then with TransferList[i]^ do begin
  Done;
  Save(TransferList[i]^);
  log.warning('Suspend transport: '+string(fid));
  dispose(TransferList[i]);
  Inc(c);
 end;
 log.info('Suspended '+IntToStr(c)+' transfers.');
end;

procedure SendFile( id: tFID );
 begin
 AbstractError;
end;

procedure tRequest.Send(const rcpt:netaddr.t);
 begin inherited Send(rcpt,sizeof(self));
 end;
procedure tInfo.Send(const rcpt:netaddr.t);
 begin inherited Send(rcpt,sizeof(self));
 end;

procedure tRequest.Create( iid :tFID; itrid: byte; ichunk: longword; icount :Word );
 begin
 inherited Create(cRequest);
 id:=iid;
 trid:=itrid;
 chunk:=ichunk;
 count:=icount;
end;

procedure tRequest.Handle( const from: NetAddr.t);
 procedure sendinfo;
  var inf:tInfo;
  begin
  inf.Create(TrID,ID);
  inf.Send(from);
 end;
 var part:byte;
 var dat:tData;
 begin
 log.debug('Request for '+String(id)+':'+IntToStr(longword(chunk))+'+'+IntToStr(longword(count))+' from '+string(from)+' #'+IntToStr(TrID));
 if longword(chunk)=0 then SendInfo;
 if count>0 then for part:=0 to count-1 do 
  dat.CreateSend( TrID, ID, longword(chunk)+part, from );
end;

procedure tData.CreateSend( const aTrID:byte; const aFID:tFID; aChunk:longword; const rcpt: NetAddr.t);
 var f:tDataFile;
 var br:longword;
 begin
 inherited Create(cData);
 DataBase.dbAssign(f,'chk'+DirectorySeparator+string(afid)); reset(f);
 try
  br:=cChunkLength*(aChunk);
  if br>FileSize(f) then raise eRangeError.Create('Reading past EOF');
  seek(f,br);
  blockread(f,PayLoad,cChunkLength,br);
 finally
  close(f);
 end;
 part:=aChunk;
 TrID:=aTrID;
 inherited Send(rcpt,(sizeof(self)-sizeof(PayLoad))+br);
end;
 
procedure tInfo.Create( const aTrID:byte; const aFID:tFID );
 var f:tDataFile;
 var br:longword;
 var Tr:tSuspendedTransfer;
 begin
 try {Get size from live transfer}
  Tr:=TransferList[TransferByFID(aFID)]^;
 except on eSearch do try {Get size from suspended transfer}
   Load(Tr,aFID);
  except on eSearch do begin {Get size from finished file}
    DataBase.dbAssign(f,'chk'+DirectorySeparator+string(afid)); reset(f);
    try br:=FileSize(f); 
    tr.InfoReceived:=(br>0);
    if (br mod cChunkLength)>0 then Tr.total:=(br div cChunkLength)+1 else Tr.total:=(br div cChunkLength);
    finally close(f); end;
  end; end;
 end;
 inherited Create(cInfo);
 if not Tr.InfoReceived then raise eXception.Create('Transfer: No info for file');
 count := Tr.Total;
 TrID  :=    aTrID;
end;

procedure INIT;
 var i:word;
 begin
 for i:=1 to cMaxTransfers do TransferList[i]:=nil
end;

INITIALIZATION
 OnRecv:=nil;
 OnProgress:=nil;
 OnRecv:=nil;
 INIT;
END.