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

const cChunkLength=512{b};
const cMaxChunksPerRequest=16;
const cChunksPerRequest=6;
CONST
 cRequest:tpktype=4;
 cInfo:tpktype=5;
 cData:tpktype=6;

const cRetryPeriod = 2000{ms} /MSecsPerDay;
const cRetryMax = 8;

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
  procedure Handle( const from: NetAddr.t; length:longword ); overload;
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
procedure RequestFile( const source :netaddr.t; id :tFID; by: byte );

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
 flags: set of (fInfoReceived,fStarted,fSuspended,fAborted);
 end;

type tPendingInfo=object
 since:tDateTime;
 part:byte deprecated;
 retry:byte;
end;

type tTransfer=object(tSuspendedTransfer)
 storage    :tDataFile;
 source     :netaddr.t;
 requested  :word;
 received   :word;
 pending    :array [0..cChunksPerRequest-1] of ^tPendingInfo;
 last       :tDateTime;
 procedure DoRun(trid:byte);
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
 TransferList[1]^.DoRun(0);
end;

function TransferByFID(ifid:tFID):word;
 begin
 result:=1;
 while (result<=cMaxTransfers)and( (TransferList[result]<>nil)or(TransferList[result]^.fid=ifid)  ) do inc(result);
 if result>cMaxTransfers then raise eSearch.Create;
end;

procedure RequestFile( const source :netaddr.t; id :tFID ); inline;
 begin RequestFile(source, id, 0 ); end;
procedure RequestFile( const source :netaddr.t; id :tFID; by: byte );
 var i:word;
 procedure Create;
  begin
  i:=1;
  while (i<=cMaxTransfers)and(not assigned(TransferList[i])) do inc(i);
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
   flags      :=tr.flags;
  end;
 end;

 begin
 try
  i:=TransferByFID(id);
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
     flags      :=[];
    end;
   end;
  end;
 end;
 with TransferList[i]^ do begin
  source     :=source;
  requested  :=0;
  received   :=0;
  SetPending(0,0);
  last       :=0;
  {maybe DoRun;}
 end;
end;

procedure RecvFileAbort( id :tFID );
 var i:word;
 begin
 i:=TransferByFID(id);
 with TransferList[i]^ do begin
  Exclude(flags,fStarted);
  Include(flags,fAborted);
  SetPending(0,0);
 end;
end;

procedure tInfo.Handle( const from: NetAddr.t);
 begin
 if 
    (TrID<high(TransferList))
    and assigned(TransferList[TrID])
    and (fStarted in TransferList[TrID]^.flags)
 then TransferList[TrID]^.HandleInf(count);
end;

procedure tData.Handle( const from: NetAddr.t; length:longword );
 var pl:longword;
 begin
 pl:=length-(sizeof(self)-sizeof(PayLoad));
 if 
    (TrID<high(TransferList))
    and assigned(TransferList[TrID])
    and (fStarted in TransferList[TrID]^.flags)
 then TransferList[TrID]^.HandleDat(part,PayLoad,pl);
end;

procedure DoRetry;
 var i:word;
 var fid:tFID;
 var by:byte;
 begin
 for i:=1 to high(TransferList) do if assigned(TransferList[i]) then begin
  if fAborted in TransferList[i]^.flags then dispose(TransferList[i])
  else if fSuspended in TransferList[i]^.flags then begin
   by:=TransferList[i]^.by;
   fid:=TransferList[i]^.fid;
   dispose(TransferList[i]);
   if assigned(OnNoSrc) then OnNoSrc(fid,by);
  end else TransferList[i]^.DoRun(i);
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
 if flags=[fStarted] then begin
  total:=count;
  flags:=flags+[fInfoReceived];
 end {else log.error('protocol desync: info received in invalid state')};
end;

procedure tTransfer.HandleDat(part:byte; var PayLoad; length:longword );
 var islast:boolean;
 begin
 if (fStarted in flags)and([fSuspended,fAborted]*flags=[]) then begin
  if assigned(pending[part]) then begin
   Seek(storage,completed+part);
   assert(length<=cChunkLength);
   isLast:=(requested=part+1)and(completed+requested=total);
   if (length<cChunkLength)and(not islast) then raise Exception.Create('Incomplete datapabket in middle of file');
   BlockWrite(storage,PayLoad,length);
   if islast then flags:=(flags-[fStarted])+[fCompleted];
   inc(received);
   dispose(pending[part]);
   pending[part]:=nil; //for sure
  end {else error};
 end {else log.error('protocol desync: data received in invalid state')};
end;

procedure tTransfer.DispatchEvent;
 unimplemented;
 begin
 log.info('Transfer '+string(fid)+': '+IntToStr(completed)+'/'+IntToStr(total));
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

procedure tTransfer.DoRun(trid:byte);
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
 if ((completed=total)and(fInfoReceived in flags))or(fCompleted in flags) then
  SetPending(0,0);
  flags:=flags+fCompleted;
 end;
 if (flags*[fStarted,fAborted,fSuspended]=[]) then begin
  {- start not started }
  log.debug('Starting not started');
  req.Create( fid, trid, 0, cChunksPerRequest );
  SetPending( 0, cChunksPerRequest );
  req.Send(source);
  requested:=cChunksPerRequest;
 end else
 if flags*[fStarted,fInfoReceived]=[fStarted] then begin
  log.debug('Re-requesting info');
  req.Create( fid, trid, 0, 0 );
  req.Send(source);
 end else
 if now-last>cMaxRetry then begin
  log.debug('Timeout => suspend');
  Save;
  SetPending( 0, 0 );
  flags:=flags+[fSuspended];
 end else
 if (received=requested) then begin
  rqc:=total-completed;
  if rqc>cChunksPerRequest then rqc:=cChunksPerRequest;
  log.debug('reqest next '+IntToStr(rqc));
  req.Create( fid, trid, completed-1, rqc );
  req.Send(source);
  requested:=rqc;
  received:=0;
  SetPending( 0, rqc );
 end else begin
  log.debug('Retry pending');
  i:=0;
  while (i<=high(pending)) and (not assigned(pending[i])) do inc(i); //find first assigned
  rqc:=1;
  while (i+rqc<=high(pending)) and assigned(pending[i+rqc]) do inc(rqc); //find last assigned
  if i<=high(pending) then begin
   log.debug('req retry '+intToStr(rqc)+' of '+inttostr(requested-received)+' pending');
   assert(rqc<=(requested-received));
   req.Create( fid, trid, completed+i, rqc );
   req.Send(source);
   while rqc>0 do with pending[i+rqc-1]^ do begin
    since:=now;
    inc(retry);
    dec(rqc);
   end;
  end;
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
 begin inherited Send(rcpt,sizeof(self)); end;
procedure tInfo.Send(const rcpt:netaddr.t);
 begin inherited Send(rcpt,sizeof(self)); end;

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
 var part:longword;
 var dat:tData;
 begin
 log.debug('Request for '+String(id)+':'+IntToStr(longword(chunk))+'+'+IntToStr(longword(count))+' from '+string(from));
 if longword(chunk)=0 then SendInfo;
 for part:=chunk to longword(chunk)+count do
  dat.CreateSend( TrID, ID, chunk, part, from );
end;

procedure tData.CreateSend( const aTrID:byte; const aFID:tFID; aChunk:longword; aPart :byte; const rcpt: NetAddr.t);
 var f:tDataFile;
 var br:longword;
 begin
 inherited Create(cData);
 DataBase.dbAssign(f,'chk'+DirectorySeparator+string(afid)); reset(f);
 try
  seek(f,cChunkLength*(aChunk+aPart));
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
 if (br mod cChunkLength)>0 then count:=(br div cChunkLength)+1 else count:=(br div cChunkLength);
end;

INITIALIZATION
 OnRecv:=nil;
 OnProgress:=nil;
 OnRecv:=nil;
END.