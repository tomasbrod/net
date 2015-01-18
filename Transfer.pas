UNIT Transfer;

{
 To send and recieve CHK files over brodnet.
 Also to assemble pieced and parted files.
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

CONST
 cRequest=4;
 cInfo   =5;
 cData   =6;

type tFID=object(Keys.tHash)
 end;
 
TYPE {--Packets--}

 tRequest=object(Peers.tPacket)
  procedure Create( iid :tFID; itrid: byte; ichunk, icount :Word );
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
  procedure CreateSend( const aTrID:byte; const aFID:tFID; aChunk:longword; aPart :byte; const rcpt: NetAddr.t);
  private
  TrID :byte;
  part :byte;
  PayLoad: array [1..cChunkLength] of byte;
 end;
 
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

IMPLEMENTATION
uses 
     DataBase
    ,LinkedList
    ;

{ Data storage }

type tSuspendedTransfer=object
 fid        :tFID;
 completed  :longword; {also marks the next chunk to be requested}
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
 requested  :word;
 received   :word;
 pending    :array [0..cChunksPerRequest-1] of ^tPendingInfo;
 last       :tDateTime;
 Terminated :boolean;
 timeouted  :boolean;
 procedure DoRun;
 procedure HandleInf(count:longword);
 procedure HandleDat(part:byte; var PayLoad; length:longword );
 procedure DispatchEvent;
 procedure SetPending(i,c:word);
 end;

const cMaxTransfers=32;
var TransferList:array [1..cMaxTransfers] of ^tTransfer;

function TransferByFID(ifid:tFID):word; forward;

procedure Load( out tra: tSuspendedTransfer; const fid: tFID ); forward;

procedure tedst;
begin
 TransferList[1]^.DoRun;
end;

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
    end;
   end;
  end;
 end;
 with TransferList[i]^ do begin
  source     :=src;
  requested  :=0;
  received   :=0;
  FillChar(pending,sizeof(pending),0);
  last       :=0;
  Terminated :=false;
  timeouted  :=false;
  trid       :=i;
  DataBase.dbAssign(storage,'chk'+DirectorySeparator+string(fid)); reset(storage);
  {maybe DoRun;}
  log.debug('Transfer '+IntToStr(trid)+' set source='+string(source)+' fid='+string(fid));
 end;
end;

procedure RecvFileAbort( id :tFID );
 var i:word;
 begin
 i:=TransferByFID(id);
 with TransferList[i]^ do begin
  close(storage);
  Terminated:=true;
 end;
end;

procedure tInfo.Handle( const from: NetAddr.t);
 begin
 if 
    (TrID<high(TransferList))
    and assigned(TransferList[TrID])
 then begin
  log.debug('Info for: '+IntToStr(TrID));
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
  log.debug('Data for: '+IntToStr(TrID));
  TransferList[TrID]^.HandleDat(part,PayLoad,pl);
 end;
end;

procedure DoRetry;
 var i:word;
 var tr:^tTransfer;
 begin
 for i:=1 to high(TransferList) do if assigned(TransferList[i]) then begin
  log.debug('DoRetry on transfer '+IntToStr(i));
  if TransferList[i]^.Terminated then begin
   tr:=TransferList[i];
   tr^.SetPending(0,0);
   TransferList[i]:=nil;
   if (tr^.timeouted)and assigned(OnNoSrc) then OnNoSrc(tr^.fid,tr^.by);
   dispose(tr);
   log.debug('disposed');
  end else TransferList[i]^.DoRun;
 end;
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
  until tra.fid=fid;
 finally
  close(f);
 end;
end;

procedure tTransfer.HandleInf(count:longword);
 begin
 last:=now;
 if (not Terminated)and(not InfoReceived) then begin
  total:=count;
  InfoReceived:=true;
  log.debug(string(fid)+' info received, total='+IntToStr(total));
 end else log.error('protocol desync: info received in invalid state for '+string(fid));
end;

procedure tTransfer.HandleDat(part:byte; var PayLoad; length:longword );
 procedure NextBatch;
  var req:tRequest;
  begin
  requested:=cChunksPerRequest;
  if requested>(total-completed) then requested:=total-completed;
  log.debug('requesting '+IntToStr(requested)+' more chunks');
  SetPending(0,requested);
  req.Create(fid,TrID,completed,requested);
  req.Send(source);
 end;
 var islast:boolean;
 begin
 last:=now;
 if (not Terminated) then begin
  if assigned(pending[part]) then begin
   assert(length<=cChunkLength);
   isLast:=InfoReceived and (completed+received+1=total);
   if (length<cChunkLength)and(not islast) then raise Exception.Create('Incomplete datapacket in middle of file');
   Seek(storage,(completed+part)*cChunkLength);
   BlockWrite(storage,PayLoad,length);
   Inc(received);
   dispose(pending[part]);
   pending[part]:=nil; //for sure
   log.debug(string(fid)+' data received, total='+IntToStr(total)+' compl='+IntToStr(completed)+' req='+IntToStr(requested)+' rec='+IntToStr(received));
   if (requested=received)or IsLast then begin
    log.debug('batch complete');
    completed:=completed+received;
    if InfoReceived then begin
     if IsLast then log.debug('file completed') else NextBatch;
    end {else wait info};
   end {else wait more};
  end {else error};
 end {else log.error('protocol desync: data received in invalid state')};
end;

procedure tTransfer.DispatchEvent;
 unimplemented;
 begin
 log.info('Transfer '+string(fid)+': '+IntToStr(completed)+'/'+IntToStr(total));
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
   if not assigned(pending[z]) then new(pending[z]);
   pending[z]^.since:=now;
   pending[z]^.retry:=0;
  end else dispose(pending[z]);
 end;
end;

procedure tTransfer.DoRun;
 procedure Save;
  var f:file of tSuspendedTransfer; begin
  DataBase.dbAssign(f,'transfer.dat'); reset(f);
  try write(f,tSuspendedTransfer(self));
  finally close(f); end;
 end;
 var req:tRequest;
 var rqc:word;
 var i:word;
 begin
 log.debug('DoRun on '+string(fid));
 if (completed=total)and(InfoReceived) then begin
  DispatchEvent;
  close(storage);
  Terminated:=true;
 end else
 if requested=0 then begin
  log.debug('Starting not started');
  req.Create( fid, trid, 0, cChunksPerRequest );
  SetPending( 0, cChunksPerRequest );
  req.Send(source);
  requested:=cChunksPerRequest;
  DispatchEvent;
 end else
 if not InfoReceived then begin
  log.debug('Re-requesting info');
  req.Create( fid, trid, 0, 0 );
  req.Send(source);
 end else
 if (now-last)>cMaxDelta then begin
  log.debug('Timeout => suspend');
  Save;
  Timeouted:=true;
  close(storage);
  Terminated:=true;
 end else
 if (now-last)>cRetryPeriod then begin
  log.debug('Retry pending '+IntToStr(requested-received));
  i:=0;
  while (i<=high(pending)) and (not assigned(pending[i])) do inc(i); //find first assigned
  rqc:=1;
  while (i+rqc<=high(pending)) and assigned(pending[i+rqc]) do inc(rqc); //find last assigned
  if i<=high(pending) then begin
   log.debug('req retry '+intToStr(rqc));
   assert(rqc<=(requested-received));
   req.Create( fid, trid, completed+i, rqc );
   req.Send(source);
   while rqc>0 do with pending[i+rqc-1]^ do begin
    since:=now;
    inc(retry);
    dec(rqc);
   end;
  end;
 end else begin
  log.debug('nothing special');
  DispatchEvent;
 end;
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

procedure tRequest.Create( iid :tFID; itrid: byte; ichunk, icount :Word );
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
 for part:=0 to count-1 do 
  dat.CreateSend( TrID, ID, chunk, part, from );
end;

procedure tData.CreateSend( const aTrID:byte; const aFID:tFID; aChunk:longword; aPart :byte; const rcpt: NetAddr.t);
 var f:tDataFile;
 var br:longword;
 begin
 inherited Create(cData);
 DataBase.dbAssign(f,'chk'+DirectorySeparator+string(afid)); reset(f);
 try
  br:=cChunkLength*(aChunk+aPart);
  if br>FileSize(f) then raise eRangeError.Create('Reading past EOF');
  seek(f,br);
  blockread(f,PayLoad,cChunkLength,br);
 finally
  close(f);
 end;
 part:=aPart;
 TrID:=aTrID;
 inherited Send(rcpt,(sizeof(self)-sizeof(PayLoad))+br);
end;
 
procedure tInfo.Create( const aTrID:byte; const aFID:tFID );
 var f:tDataFile;
 var br:longword;
 begin
 inherited Create(cInfo);
 DataBase.dbAssign(f,'chk'+DirectorySeparator+string(afid)); reset(f);
 try
  br:=FileSize(f);
 finally
  close(f);
 end;
 TrID:=aTrID;
 if (br mod cChunkLength)>0 then count:=(br div cChunkLength)+1 else count:=(br div cChunkLength);
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