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
CONST
 cGet:tpktype=4;
 cDat:tpktype=5;
 cPcs:tpktype=6;

const cRetryPeriod = 2000{ms} /MSecsPerDay;
const cRetryMax = 8;
TYPE

 tFID=object(Keys.tHash)
 end;
 
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
 
 tInfoMetaType= packed (imtNone);
 
 tInfo=object(tPacket)
  procedure Create( var req:tRequest );
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t);
  private
  TrID  :byte;
  count :NetAddr.Word.4; {count*cChunkLength = size of the file}
  metadata:record
   case metatype:tInfoMetaType of
   imtNone:();
  end;
 
 tData=object(Peers.tPacket)
  procedure Create( req:tRequest; part :byte );
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t);
  private
  TrID :byte;
  part :byte;
  PayLoad: array [1..cChunkLength] of byte;
 end;
 
 tError=object(Peers.tPacket) 
 end unimplemented;
 
 eNoSource=class(Exception)
  fid: tFID;
  constructor Create( ifid: tFid );
 end;

var OnNoSrc :procedure( id :tFID );
var OnRecv  :procedure( id :tFID );
var OnProgress :procedure( id :tFID; done,total:longword );

procedure SendFile( id: tFID );
 experimental;

procedure RequestFile( id :tFID );
 experimental;
procedure RequestFile( id :tFID; by: byte );
 unimplemented;

procedure RecvFileAbort( id :tFID );
 unimplemented;

procedure DoRetry;

IMPLEMENTATION
uses DataBase
    ;

TYPE { database accessors }

(*
 
 ## Database:
 
 - parts:
  - holds info and data about not yet downloaded parts
  - no fully downloaded
  - state:
    - passive: retry = 0
    - waiting: retry > 0
    - complete: ?
 - files:
  - info and data of simple files
 - pices:
  - reference of pieced files to simple files

 
*)

type tPartsDB = class (DataBase.tDbDataSet)
 public
 constructor Create; {override;}overload;
end;

constructor tPartsDB.Create;
 begin
 inherited Create( nil );
   OpenMode:=omAutoCreate;
   FieldDefs.Add( {0} 'id',    ftString,  40, True  );
   FieldDefs.Add( {1} 'num', ftLargeInt, 0, True );
   FieldDefs.Add( {2} 'done', ftBoolean, 0, True );
   FieldDefs.Add( {3} 'since', ftFloat, 0, True );
   FieldDefs.Add( {4} 'retry', ftSmallInt, 0, True );
   FieldDefs.Add( {5} 'data', ftBlob, 0, True );
   //with IndexDefs.Add do begin Name:='addr'; Expression:=Name; Options:=[ixCaseInsensitive]; end;
 Open ('parts');
end;

type tPartedDB = class (DataBase.tDbDataSet)
 public
 constructor Create; {override;}overload;
end;

constructor tPartedDB.Create;
 begin
 inherited Create( nil );
   OpenMode:=omAutoCreate;
   FieldDefs.Add( {0} 'id',    ftString,  40, True  );
   FieldDefs.Add( {1} 'num', ftLargeInt, 0, True );
   FieldDefs.Add( {2} 'done', ftBoolean, 0, True );
   FieldDefs.Add( {3} 'since', ftFloat, 0, True );
   FieldDefs.Add( {4} 'retry', ftSmallInt, 0, True );
   FieldDefs.Add( {5} 'data', ftBlob, 0, True );
   //with IndexDefs.Add do begin Name:='addr'; Expression:=Name; Options:=[ixCaseInsensitive]; end;
 Open ('parted');
end;

 tPartInfo=object
  state :( psUnknown, psPassive, psWaiting, psComplete );
  since :tDateTime;
  retry :word;
 end;
 tPartAccess=object(DataBase.tAccess)
  constructor Init( fid :tFID );
  procedure SetRequested( part :Word );
  function  isRequested( part :Word) :boolean;
  procedure SetDone( part :Word );
  function isComplete( part :Word ) :boolean;
  procedure SetTotal( total :Word );
  procedure Abort;
  function isToRetry( part :Word ):boolean;
  procedure FindToRetry( var part :Word );
  { Finds first part to retry, starting at part, throws eRangeError on no more }
  private
  procedure getState( out state :tPartInfo; part :Word ); virtual;
  procedure setState( part: word; state :tPartInfo ); virtual;
 end experimental;

 tDataAccess=object(DataBase.tAccess)
  constructor Init( fid :tFID );
 end experimental;

 tPieceInfo=record
  content :tFID;
 end;
 tPieceAccess=object(DataBase.tAccess)
  parts :tPartAccess;
  constructor Init( fid :tFID );
  destructor Done;
  function IsPieced :boolean;
  private
  {
  procedure getState( out state :tPartInfo; part :Word ); virtual;
  procedure setState( part: word; state :tPartInfo ); virtual;
  }
 end experimental;

 tSourceInfo=record
  peer :Peers.tID;
  pad  :array [1..64] of byte;
 end;
 tSourceAccess=object(DataBase.tAccess)
  constructor Init( fid :tFID );
  procedure Select;
 end experimental;

 tDownloadsAccess=object(DataBase.tAccess)
  constructor Init;
  procedure Find( var R: DataBase.tRecord; fid :tFID );
  procedure Add( fid :tFID );
  procedure Remove( fid :tFID );
 end unimplemented;

function tPieceAccess.IsPieced:boolean;
 var R: tPieceInfo;
// var status:tPartInfo;
 begin
 try
  Read(R, 0);
  IsPieced:=not R.content.isNil;
 except
  on eRangeError do IsPieced:=false;
 end;
end;

procedure SendFile( id: tFID );
 var dat :tDat;
 var pcs :tPcs;
 {No tak bude na stacku nepotrebna premenna, svet sa nezruti.}
 var pcsdb :tPieceAccess;
 begin
 pcsdb.init( id ); try
 if pcsdb.IsPieced then begin
  pcs.Create( id, 0 );
  pcs.Send;
 end else begin
  dat.Create( id, 0 );
  dat.Send;
 end;
 finally pcsdb.done; end;
end;

procedure RecvFile( id:tFID );
 var dwndb :tDownloadsAccess;
 var srcdb :tSourceAccess;
 var pdb :tPartAccess;
 var get :tGet;
 begin
 dwndb.init; srcdb.init( id ); pdb.init( id ); try
  srcdb.append( Peers.SelectedID ); {$HINT this should be 'add', but implementing find seems overkill}
  pdb.SetRequested( 0 ); {this fails when already requested, but at least we have added the source}
  dwndb.add( id );
 finally pdb.done; srcdb.done; dwndb.done; end;
 get.create( id, 0, 0 );
 get.send;
end;

procedure RecvFileAbort( id :tFID );
 var db :tDownloadsAccess;
 {
 var srcdb :tSourceAccess;
 var pcsdb :tPieceAccess;
 var partdb :tPartAccess;
 }
 begin
 db.init; try
  db.remove( id );
 finally db.done; end;
 {
 srcdb.init( id );
 srcdb.purge;
 pcsdb.init( id );
 pcsdb.purge;
 partdb.init( id );
 partdb.purge;
 }
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
 

{
}

{
Retry(tFID);
GlobalAddDownload(tFID);"
AddSource(tFID,tID);"
}

END.