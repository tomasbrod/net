UNIT Transfer;

{
 To send and recieve CHK files over brodnet.
}

INTERFACE
USES Peers
    ,Keys
    ,NetAddr
    ,SysUtils
    ;

CONST
 cDataLength=768;
 cGet:tpktype=4;
 cDat:tpktype=5;
 cPcs:tpktype=6;

const cRetryPeriod = 2000{ms} /MSecsPerDay;
const cRetryMax = 8;

TYPE

 tFID=object(Keys.tHash)
 end;
 
 tGet=object(Peers.tPacket)
  procedure Create( id :tFID; part, count :Word );
  procedure Handle;
  procedure Send;
  private
  id :tFID;
  part :NetAddr.Word2;
  count :NetAddr.Word2;
 end;
 
 tDat=object(Peers.tPacket)
  procedure Create( id :tFID; part :Word );
  procedure Handle;
  procedure Send;
  private
  id :tFID;
  part :NetAddr.Word2;
  total :NetAddr.Word2;
  PayLoad: array [1..cDataLength] of byte;
 end;

 tPcs=object(Peers.tPacket)
  procedure Create( id :tFID; part :Word );
  procedure Handle;
  procedure Send;
  private
  id :tFID;
  part :NetAddr.Word2;
  total :NetAddr.Word2;
  PayLoad: array [1..(cDataLength div sizeof(tFID))] of tFID;
 end;
 
 eNoSource=class(Exception)
  fid: tFID;
  constructor Create( ifid: tFid );
 end;

var OnNoSrc :procedure( id :tHash );
var OnRecv  :procedure( id :tHash );

procedure SendFile( id: tFID );

procedure RecvFile( id :tFID );

procedure RecvFileAbort( id :tFID );

procedure DoRetry;

procedure Retry( id :tFID );

IMPLEMENTATION
uses DataBase
    ;

function IsPieced( id:tFID ):boolean;
 forward;
procedure GlobalAddDownload( id:tFID );
 forward;
procedure AddSource( id:tFID; source:Peers.tID );
 forward;

TYPE { database accessors }

 tPartAccess=object(DataBase.tAccess)
  constructor Init( fid :tFID );
  procedure SetRequested( part :Word );
  procedure SetDone( part :Word );
  procedure SetTotal( total :Word );
  procedure Abort;
  function isToRetry( part :Word ):boolean;
  procedure FindToRetry( var part :Word );
  { Finds first part to retry, starting at part, throws eRangeError on no more }
 end experimental;

 tDataAccess=object(DataBase.tAccess)
  constructor Init( fid :tFID );
 end experimental;

 tPiecesAccess=object(DataBase.tAccess)
  constructor Init( fid :tFID );
 end experimental;

 tSourcesAccess=object(DataBase.tAccess)
  constructor Init( fid :tFID );
  procedure Select;
 end experimental;

 tDownloadsAccess=object(DataBase.tAccess)
  constructor Init;
  procedure Find( var R: DataBase.tRecord; fid :tID );
  procedure Add( fid :tID );
  procedure Remove( fid :tID );
 end unimplemented;

procedure SendFile( id: tFID );
 var dat :tDat;
 var pcs :tPcs;
 {No tak bude na stacku nepotrebna premenna, svet sa nezruti.}
 begin
 if IsPieced( id ) then begin
  pcs.Create( id, 0 );
  pcs.Send;
 end else begin
  dat.Create( id, 0 );
  dat.Send;
 end;
end;

procedure RecvFile( id:tFID );
 begin
 GlobalAddDownload( id );
 AddSource( id, Peers.SelectedID );
 Retry( ID );
end;

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
 var db:tPiecesAccess;
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
 db.BlockWrite( self.PayLoad, Ofs, high(self.PayLoad));
 pdb.SetDone( part );
 pdb.SetTotal( total );
 pdb.done;
 db.done;
end;

procedure tPcs.Handle;
 var pdb :tPartAccess;
 var db :tPiecesAccess;
 var Ofs:LongWord;
 begin
 Ofs:= word(part) * high(self.PayLoad);
 //if GetSource( id )<>Sender then raise exception.create('auth');
 db.init( id );
 pdb.init( id );
 db.BlockWrite( self.PayLoad, Ofs, high(self.PayLoad));
 pdb.SetDone( part );
 pdb.SetTotal( total );
 pdb.done;
 db.done;
end;

procedure tGet.Handle;
 var c :word;
 var pcs:tPcs;
 var dat:tDat;
 var pieced:boolean;
 var lpart, lcount :word;
 begin
 pieced:=IsPieced( id );
 lpart:=self.part;
 lcount:=self.count;
 for c:=lpart to lpart+lcount do begin
  if not pieced then begin
   dat.Create( id, c );
   dat.Send;
  end else begin
   pcs.Create( id, c );
   pcs.Send;
  end;
 end;
end;

procedure Retry( id :tFID );
 const cMaxGetCount=42;
 const cMaxGet=4;
 var db :tPartAccess;
 var part :word;
 var top  :word;
 var get :tGet;
 var pks :byte;
 begin
 db.init( id );
 part:=0;
 pks:=0;
 for pks:=1 to cMaxGet do begin
  try db.FindToRetry( part );
  except on eRangeError do break; end;
  top:=part+1;
  while ((top-part)<=cMaxGetCount) and db.isToRetry( top ) do top:=top+1;
  get.create( id, part, top-part-1 );
  get.send;
 end;
end;

procedure DoRetry;
 var gdb: tDownloadsAccess;
 var cur: tFID;
 var R: tRecord;
 var src :tSourcesAccess;
 begin
 R:=0;
 gdb.init;
 try
  repeat
   gdb.read(cur, R);
   src.init( cur );
   try src.select;
   except on eNoSource do begin
     src.done;
     if assigned(onNoSrc) then onNoSrc( cur );
     raise;
   end; end;
   src.done;
   Retry( cur );
   inc( R );
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
 

{
}

{
Retry(tFID);
IsPieced(tFID):Boolean;"
GlobalAddDownload(tFID);"
AddSource(tFID,tID);"
}

END.