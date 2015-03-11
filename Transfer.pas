UNIT Transfer;

{
 To send and recieve CHK files over brodnet.
 Also to assemble pieced and parted files.
 
 DONE: Do not save and load from statefile too often (kills performance). 

 DONE:Use different filename for incomplete files.
 
 TODO: Trunctate the incomplete file when a download is started (and not 
 running already), to be sure.
 
 TODO: Make loading and saving procs of tTransfer object type.
 
 TODO: Save state to separate file for each transfer, not global statefile.
 DONE: Do not save any metadata. Trunc off the pending parts if download fails.

 DONE: Refactor the whole RequestFile proc. Main objectives:

  * Exit if transfer already running. DONE
  * try to Resume suspended transfer with the new source (and exit). DONE
  * Exit if the file is present finished (not *.part). DONE
  * Create a new transfer. DONE
 
 TODO: Make tData constructor try read the .part file as second option.
 
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
const cMaxDelta = 30 /SecsPerDay; {if nothing is received under this, transfer is aborted}
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
 
 tDataFile=file of byte;
 tData=object(Peers.tPacket)
  procedure Handle( const from: NetAddr.t; length:longword );
  //procedure CreateSend( const aTrID:byte; const aFID:tFID; aChunk:longword; const rcpt: NetAddr.t);
  procedure CreateSend( const aTrID:byte; var f:tdatafile; aChunk:longword; const rcpt: NetAddr.t);
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

type tBy=0..31;
type tBys=set of tBy;

var OnRecv  :procedure( id :tFID; by :tBys );
var OnProgress :procedure( id :tFID; done,total:longword; by: tBys );

procedure SendFile( id: tFID );
 experimental; deprecated {all files must be requested};

procedure RequestFile( const source :netaddr.t; id :tFID );
 experimental;
procedure RequestFile( const src :netaddr.t; id :tFID; aBy: tBy );

procedure RecvFileAbort( id :tFID );
 unimplemented;

procedure Query( id :tFID; out done,total:longword );
 experimental;

var OnNoSrc :procedure( id :tFID; by :tBys );
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
 total      :longword;
 by         :tBys;
 InfoReceived:Boolean;
 end;

type tTransfer=object(tSuspendedTransfer)
 storage    :tDataFile;
 source     :netaddr.t;
 trid       :byte;
 pending    :set of 0..cChunksPerRequest-1;
 received   :byte;
 last       :tDateTime;
 Terminated :boolean;
 Timeouted  :boolean;
 saved      :boolean;
 {constructor;} procedure Init( aFID: tFID; aSource: netaddr.t );
 procedure DoRun;
 procedure HandleInf(count:longword);
 procedure HandleDat(part:longword; var PayLoad; length:longword );
 procedure Done;
 private
 procedure RequestNextBatch;
 procedure DispatchEvent;
 procedure SetPending(i,c:word);
 end;

const cMaxTransfers=32;
const cPartExt:string[5]='.part';
const cPartMetaExt:string[8]='.partdat' deprecated;
var TransferList:array [1..cMaxTransfers] of ^tTransfer;

function TransferByFID(ifid:tFID):word; forward;

function TransferByFID(ifid:tFID):word;
 begin
 result:=1;
 while (result<=cMaxTransfers)and( (TransferList[result]=nil )or( TransferList[result]^.fid<>ifid)  ) do inc(result);
end;

procedure RequestFile( const source :netaddr.t; id :tFID ); inline;
 begin RequestFile(source, id, 0 ); end;

procedure RequestFile( const src :netaddr.t; id :tFID; aBy: tBy );
 var i:word;
 procedure Create;
  begin
  i:=1;
  while (i<=cMaxTransfers)and(assigned(TransferList[i])) do inc(i);
  if i>cMaxTransfers then raise Exception.Create('Too many transfers');
  new(TransferList[i]);
 end;
 begin
 i:=TransferByFID(id);
 if i<=cMaxTransfers then begin
  Include(TransferList[i]^.by,aby);
  log.error('Try to add duplicate transfer by '+inttostr(aby));
  exit;
 end else begin
  Create;
  with TransferList[i]^ do begin
   trid:=i;
   Init(id,src);
   by:=[aBy];
   log.debug('Transfer '+IntToStr(trid)+' set source='+string(source)+' fid='+string(fid));
   {DoRun; TEST}
  end;
 end;
end;

procedure tTransfer.Init( aFID: tFID; aSource: netaddr.t );
 begin
 fid        :=aFID;
 completed  :=0;
 total      :=0;
 by         :=[];
 {trid set by caller}
 received   :=0;
 InfoReceived:=false;
 Terminated :=false;
 Timeouted  :=false;
 Saved      :=false;
 source     :=aSource;
 pending    :=[];
 last       :=Now;
 DataBase.dbAssign(storage,'chk'+DirectorySeparator+string(fid)); reset(storage);
 if FileSize(storage)>0 then begin
  Close(storage); {the file is already downloaded, so pretend like just downloaded it and exit}
  Rename(Storage,DataBase.Prefix+DirectorySeparator+'chk'+DirectorySeparator+string(fid)+cPartExt);
  reset(storage);
  completed:= FileSize(storage) div cChunkLength;
  total:= completed;
 end else begin
  close(Storage);
  erase(Storage);
  DataBase.dbAssign(storage,'chk'+DirectorySeparator+string(fid)+cPartExt); reset(storage);
  completed:= FileSize(storage) div cChunkLength;
 end;
end;

procedure RecvFileAbort( id :tFID );
 var i:word;
 begin
 i:=TransferByFID(id);
 if i<=cMaxTransfers then with TransferList[i]^ do begin
  Done;
 end;
end;

procedure Query( id :tFID; out done,total:longword );
 var i:word;
 var downloaded:tDataFile;
 begin
 i:=TransferByFID(id);
 if i<=cMaxTransfers then begin
  done:=TransferList[i]^.completed;
  total:=TransferList[i]^.total;
 end else begin
  DataBase.dbAssign(downloaded,'chk'+DirectorySeparator+string(id));
  reset(downloaded);
  done:= FileSize(downloaded) div cChunkLength;
  total:=done;
  if done=0 then erase(downloaded) else close(downloaded);
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
   log.debug('Tr'+IntToStr(i)+' disposed');
   dispose(tr);
  end else begin
   //log.debug('Transfer '+i+' DoRun');
   TransferList[i]^.DoRun;
  end;
 end;
end;

procedure tTransfer.Done;
 begin
 if (pending<>[])and(completed>0) then begin
  Seek(storage,completed*cChunkLength);
  Truncate(storage);
 end;
 SetPending(0,0);
 try Close(storage); except end;
 Terminated:=true;
end;

procedure tTransfer.HandleInf(count:longword);
 begin
 last:=now;
 if (not Terminated)and(not InfoReceived) then begin
  total:=count;
  InfoReceived:=true;
  //log.debug(string(fid)+' info received, total='+IntToStr(total));
  //Save(self);
  DispatchEvent;
 end else log.error('protocol desync: info received in invalid state for '+string(fid));
end;

procedure tTransfer.HandleDat(part:longword; var PayLoad; length:longword );
 begin
 last:=now;
 if (not Terminated) then begin
  assert(((part-completed)>=0)and((part-completed)<cChunksPerRequest));
  if (part-completed) in pending then begin
   assert(length<=cChunkLength);
   Seek(storage,part*cChunkLength);
   BlockWrite(storage,PayLoad,length);
   Exclude(pending,part-completed);
   Inc(received);
   (*log.debug(string(fid)+' data received, total='+IntToStr(total)+' compl='+IntToStr(completed)+' req='+IntToStr(requested)+' rec='+IntToStr(received));*)
   if pending=[] then begin
    (*log.debug('batch complete');*)
    completed:=completed+received;
    if InfoReceived then begin
     if completed < total then RequestNextBatch; (*else log.debug('file completed');*)
    end {else wait info};
   end {else wait pending};
  end {else desync not requested};
 end {else log.error('protocol desync: data received in invalid state')};
end;

procedure tTransfer.RequestNextBatch;
 var req:tRequest;
 var requested:LongWord;
 begin
 received:=0;
 if total>0 then requested:=total-completed else requested:=cChunksPerRequest;
 if requested>cChunksPerRequest then requested:=cChunksPerRequest;
 (*log.debug('requesting '+IntToStr(requested)+' more chunks');*)
 SetPending(0,requested);
 req.Create(fid,TrID,completed,requested);
 req.Send(source);
end;

procedure tTransfer.DispatchEvent;
 unimplemented;
 begin
 (*log.info('Transfer '+string(fid)+': '+IntToStr(completed)+'/'+IntToStr(total));*)
 if assigned(OnProgress) then OnProgress(fid,completed,total,by);
 if (completed=total)and assigned(OnRecv) then OnRecv(fid,by);
 {   if assigned(OnNoSrc) then OnNoSrc(fid,by);}
end;

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
   log.error('Tr'+IntToStr(TrID)+' Hash Mismatch  in '+string(fid)+' from '+string(source));
   log.debug('Hash of file='+string(hash));
   Completed:=0;
   InfoReceived:=false;
   pending:=[];
   result:=false;
   AbstractError;
 end else begin
  log.debug('Tr'+IntToStr(TrID)+' hash matched :)');
  result:=true;
 end; end;
 var req:tRequest;
 var rqc:word;
 var i:word;
 begin
 if (completed=total)and(InfoReceived) then begin
  if Verify then begin
   Close(Storage);
   Rename(storage,DataBase.Prefix+DirectorySeparator+'chk'+DirectorySeparator+string(fid));
   Done;
  end;
 end else
 if (not InfoReceived) and (completed>0) then begin
  log.debug('Tr'+IntToStr(TrID)+' Requesting info');
  req.Create(fid,trid,0,0);
  req.Send(source);
 end else
 if pending=[] then begin
  log.debug('Tr'+IntToStr(TrID)+' Starting not started');
  RequestNextBatch;
 end else
 if (now-last)>cMaxDelta then begin
  log.debug('Tr'+IntToStr(TrID)+' Timeout');
  Timeouted:=true;
  Done;
 end else
 if (now-last)>cRetryPeriod then begin
  log.debug('Tr'+IntToStr(TrID)+' Retry pending');
  i:=0;
  while (i<=high(pending)) and (not (i in pending)) do inc(i); //find first assigned
  rqc:=1;
  while (i+rqc<=high(pending)) and (i+rqc in pending) do inc(rqc); //find last assigned
  if i<=high(pending) then begin
   log.debug('req retry '+intToStr(rqc));
   req.Create( fid, trid, completed+i, rqc );
   req.Send(source);
  end;
 end else begin
  //log.debug('transfer nothing special');
 end;
 DispatchEvent;
end;

procedure NotifyQuit;
 var i:word;
 var c:word=0;
 begin
 for i:=1 to high(TransferList) do if assigned(TransferList[i]) then with TransferList[i]^ do begin
  Done;
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
  try
   inf.Create(TrID,ID);
  except log.error('Failed to create info for '+string(id)); exit; end;
  inf.Send(from);
 end;
 var part:byte;
 var dat:tData;
 var f :tDataFile;
 begin
 //log.debug('Request for '+String(id)+':'+IntToStr(longword(chunk))+'+'+IntToStr(longword(count))+' from '+string(from)+' #'+IntToStr(TrID));
 if longword(chunk)=0 then SendInfo;
 DataBase.dbAssign(f,'chk'+DirectorySeparator+string(id)); reset(f);
 if filesize(f)=0 then begin
  DataBase.dbAssign(f,'chk'+DirectorySeparator+string(id)+cPartExt); reset(f);
 end;
 if count>0 then for part:=0 to count-1 do 
  dat.CreateSend( TrID, f, longword(chunk)+part, from );
 close(f);
end;

procedure tData.CreateSend( const aTrID:byte; var f:tdatafile; aChunk:longword; const rcpt: NetAddr.t);
 var br:longword;
 begin
 inherited Create(cData);
 br:=cChunkLength*(aChunk);
 if br>FileSize(f) then begin
  log.error('Transfer request past EOF');
  exit;
 end;
 seek(f,br);
 blockread(f,PayLoad,cChunkLength,br);
 part:=aChunk;
 TrID:=aTrID;
 inherited Send(rcpt,(sizeof(self)-sizeof(PayLoad))+br);
end;
 
procedure tInfo.Create( const aTrID:byte; const aFID:tFID );
 var f:tDataFile;
 var br:longword;
 var Tr:word;
 begin
 {Get size from live transfer}
 Tr:=TransferByFID(aFID);
 if Tr>high(TransferList) then begin
  {get size from finished}
  DataBase.dbAssign(f,'chk'+DirectorySeparator+string(afid)); reset(f);
  try
   br:=FileSize(f);
   if (br mod cChunkLength)>0 then br:=(br div cChunkLength)+1 else br:=(br div cChunkLength);
  finally close(f); end;
 end else br:=TransferList[Tr]^.total;
 inherited Create(cInfo);
 if br=0 then raise eXception.Create('Transfer: No info for file');
 count :=       br;
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