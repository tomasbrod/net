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
 cGet:tpktype=4;
 cDat:tpktype=5;
 cPcs:tpktype=6;

const cRetryPeriod = 2000{ms} /MSecsPerDay;
const cRetryMax = 8;

type tFID=object(Keys.tHash)
 end;
 
TYPE {--Packets--}

 tRequest=object(Peers.tPacket)
  procedure Create( id :tFID; part, count :Word );
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
  procedure CreateSend( const aTrID:byte; const aFID:tFID; aPart :byte; const rcpt: NetAddr.t);
  private
  TrID :byte;
  part :byte;
  PayLoad: array [1..cChunkLength] of byte;
 end;
 
 tError=object(Peers.tPacket) 
 end unimplemented;

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
 flags      :set of (fInfoReceived,fStarted,fSuspended,fAborted);
 end;

type tPendingList=class(tDLNode)
 since:tDateTime;
 part:byte;
end;

type tTransfer=object(tSuspendedTransfer)
 source     :netaddr.t;
 requested  :word;
 pending    :tPendingList;
 last       :tDateTime;
 procedure DoRun;
 procedure HandleInf(count:longword);
 procedure HandleDat(part:byte; var PayLoad; length:longword );
 end;

const cMaxTransfers=32;
var TransferList:array [1..cMaxTransfers] of ^tTransfer;
var TransferIdx:word;

function TransferByFID(ifid:tFID):word; forward;

procedure Load( out tra: tSuspendedTransfer; const fid: tFID ); forward;

procedure tedst;
begin
 TransferList[1]^.DoRun;
end;

type tDataFile=file of byte;

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
  pending    :=tPendingList.CreateRoot;
  last       :=0;
  {maybe: DoRun;}
 end;
end;

procedure RecvFileAbort( id :tFID );
 var i:word;
 begin
 i:=TransferByFID(id);
 with TransferList[i]^ do begin
  Exclude(flags,fStarted);
  Include(flags,fAborted);
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
 begin
 if 
    (TrID<high(TransferList))
    and assigned(TransferList[TrID])
    and (fStarted in TransferList[TrID]^.flags)
 then TransferList[TrID]^.HandleDat(part,PayLoad,length);
end;

procedure DoRetry;
 var i:word;
 begin
 for i:=1 to high(TransferList) 
 do if assigned(TransferList[i])
 then TransferList[i]^.DoRun;
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

procedure SendFile( id: tFID );
 begin
 AbstractError;
end;

(*
pootis:
tRequest.Create(tFID,Word,Word);
tRequest.Handle(const t);
tRequest.Send(const t);
tInfo.Create(var tRequest);
tInfo.Send(const t);
procedure tData.CreateSend( const aTrID:byte; const aFID:tFID; aPart :byte; const rcpt: NetAddr.t);

deprecate:
SendFile(tFID);

tTransfer.DoRun;
tTransfer.HandleInf(LongWord);
tTransfer.HandleDat(Byte,var <Formal type>,LongWord);

*)

{$IFDEF n}

procedure tGet.Create( id :tFID; part, count :Word );
 var p:word;
 var pdb :tPartAccess;
 begin
 inherited Create( cGet );
 self.id:=id;
 self.part:=part;
 self.count:=count;
 pdb.init( id );
 try
  for p:=part to part+count
   do pdb.SetRequested( p );
 finally
  pdb.done;
 end;
end;

procedure tGet.Send;
 begin
 Peers.tPacket.Send( sizeof(self) );
end;

procedure tDat.Create( id :tFID; part :Word );
 var db:tDataAccess;
 var cLen:LongInt;
 var Len :word;
 var Ofs:LongWord;
 begin
 inherited Create( cDat );
 self.id:= id;
 self.part:= part;
 db.init( id );
 try
  Ofs:= part * high(self.PayLoad);
  cLen:= db.TotalCount - Ofs;
  if cLen<1 then raise eRangeError.Create('');
  if cLen>high(self.PayLoad) then cLen:=high(self.PayLoad);
  Len:=cLen;
  if (db.TotalCount mod high(self.PayLoad))=0
   then self.total:= (db.TotalCount div high(self.PayLoad))
   else self.total:= (db.TotalCount div high(self.PayLoad)) +1
  ;
  db.BlockRead( self.PayLoad, Ofs, cLen );
 finally db.done; end;
end;

procedure tDat.Send;
 begin
 Peers.tPacket.Send( sizeof(self) );
end;

procedure tPcs.Create( id:tFID; part:Word );
 var db:tPieceAccess;
 var cLen:LongInt;
 var Len :word;
 var Ofs:LongWord;
 begin
 inherited Create( cPcs );
 self.id:= id;
 self.part:= part;
 db.init( id );
 try
  Ofs:= part * high(self.PayLoad);
  cLen:= db.TotalCount - Ofs;
  if cLen<1 then raise eRangeError.Create('');
  if cLen>high(self.PayLoad) then cLen:=high(self.PayLoad);
  Len:=cLen;
  if (db.TotalCount mod high(self.PayLoad))=0
   then self.total:= (db.TotalCount div high(self.PayLoad))
   else self.total:= (db.TotalCount div high(self.PayLoad)) +1
  ;
  db.BlockRead( self.PayLoad, Ofs, cLen );
 finally db.done; end;
end;

procedure tPcs.Send;
 begin
 Peers.tPacket.Send( sizeof(self) );
end;

procedure tDat.Handle;
 var pdb :tPartAccess;
 var db :tDataAccess;
 var Ofs:LongWord;
 begin
 Ofs:= word(part) * high(self.PayLoad);
 //if GetSource( id )<>Sender then raise exception.create('auth');
 db.init( id );
 pdb.init( id );
 if not pdb.isRequested( part ) then raise exception.create('Recieved an not requested part');
 {the above line also prevents overwrites of completed data}
 db.BlockWrite( self.PayLoad, Ofs, high(self.PayLoad));
 pdb.SetDone( part );
 pdb.SetTotal( total );
 pdb.done;
 db.done;
end;

procedure tPcs.Handle;
 var db :tPieceAccess;
 var Ofs:LongWord;
 begin
 Ofs:= word(part) * high(self.PayLoad);
 //if GetSource( id )<>Sender then raise exception.create('auth');
 db.init( id ); try
  //db.parts.init( id );
  if not db.parts.isRequested( part ) then raise exception.create('Recieved an not requested part of pieces');
  db.BlockWrite( self.PayLoad, Ofs, high(self.PayLoad));
  db.parts.SetDone( part );
  db.parts.SetTotal( total );
 finally db.done; end;
end;

procedure tGet.Handle;
 var c :word;
 var pcs:tPcs;
 var dat:tDat;
 var pieced:boolean;
 var lpart, lcount :word;
 var pcsdb :tPieceAccess;
 begin
 pcsdb.init( id ); try
  pieced:=pcsdb.IsPieced;
 lpart:=self.part;
 lcount:=self.count;
 for c:=lpart to lpart+lcount do begin
  if not pcsdb.parts.isComplete( c ) then continue;
  if pieced then begin
   pcs.Create( id, c );
   pcs.Send;
  end else begin
   dat.Create( id, c );
   dat.Send;
  end;
 end;
 finally pcsdb.done; end;
end;

function Retry( id :tFID ) :Integer;
 const cMaxGetCount=42;
 const cMaxGet=4;
 var db :tPartAccess;
 var part :word;
 var top  :word;
 var get :tGet;
 begin
 db.init( id );
 part:=0;
 Result:=0;
 while Result < cMaxGet do begin
  try db.FindToRetry( part );
  except on eRangeError do break; end;
  top:=part+1;
  while ((top-part)<=cMaxGetCount) and db.isToRetry( top ) do top:=top+1;
  get.create( id, part, top-part-1 );
  get.send;
  inc(Result);
 end;
end;

procedure DoRetry;
 var gdb: tDownloadsAccess;
 var cur: tFID;
 var R: tRecord;
 var src :tSourceAccess;
 begin
 R:=0;
 gdb.init;
 try
  repeat
   gdb.read(cur, R);
   src.init( cur ); try
    try src.select;
    except on eNoSource do begin
      if assigned(onNoSrc) then onNoSrc( cur );
      inc( R );
      continue;
    end; end;
    if Transfer.Retry( cur ) = 0
       then gdb.delete( R )
       else inc( R )
    ;
   finally src.done; end;
  until false;
 finally
  gdb.done;
 end;
end;

{*********** DataBase *********}

constructor tPartAccess.Init( fid :tFID );
 var row: DataBase.tRow;
begin
 fid.ToString(Row);
 tAccess.Init( sizeof(tPartInfo), 'object', Row, 'part' );
end;

procedure tPartAccess.getState( out state :tPartInfo; part :Word );
 begin
 Read(state, part);
end;

procedure tPartAccess.setState( part: word; state :tPartInfo );
 begin
 OverWrite(part, state);
end;

procedure tPartAccess.SetRequested( part :Word );
 var state: tPartInfo;
 begin
 try getState( state, part );
 except on eRangeError do state.state:=psUnknown; end;
 if state.state>=psComplete then raise exception.create('Trying to request complete piece');
 state.state:=psWaiting;
 state.retry:= state.retry+1;
 state.since:=Now;
 setState( part, state);
end;

function tPartAccess.isRequested( part :Word) :boolean;
 var state: tPartInfo;
 begin
 getState( state, part );
 isRequested:=(state.state=psWaiting);
end;

function tPartAccess.isComplete( part :Word ) :boolean;
 var state: tPartInfo;
 begin
 getState( state, part );
 isComplete:=(state.state=psComplete);
end;

procedure tPartAccess.SetDone( part :Word );
 var state: tPartInfo;
 begin
 getState( state, part );
 assert(state.state=psWaiting);
 state.state:=psComplete;
 state.since:=Now;
 setState( part, state);
end;

procedure tPartAccess.SetTotal( total :Word );
 var r:tRecord;
 var c:tPartInfo;
 begin
 for r:=0 to total do begin
  try
   getState(c,r);
  except
   on eRangeError do begin
    c.state:=psPassive;
    c.since:=Now;
    c.retry:=0;
   end;
  end;
  setState(r,c);
 end;
end;

procedure tPartAccess.Abort;
 begin
 Purge;
end;

function tPartAccess.isToRetry( part :Word ):boolean;
 var state: tPartInfo;
 begin
 getState( state, part );
 isToRetry:= 
             (state.state in [psUnknown,psPassive]) or
             (
               (state.state=psWaiting)and
               ((Now-state.since)>cRetryPeriod)
             )
 ;
end;

procedure tPartAccess.FindToRetry( var part :Word );
 begin
 repeat
  try if isToRetry( part ) then break;
  except
   on eRangeError do raise eRangeError.Create('No more parts to retry @Transfer.');
  end;
  inc( part );
 until false;
end;

{$ENDIF}

END.