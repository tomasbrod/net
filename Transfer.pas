UNIT Transfer;

{
 To send and recieve CHK files over brodnet.
}

INTERFACE
USES Peers
    ,Keys
    ,NetAddr
    ;

CONST
 cDataLength=768;
 cGet:tpktype=4;
 cDat:tpktype=5;
 cPcs:tpktype=6;

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

var OnNoSrc :procedure( id :tHash );
var OnRecv  :procedure( id :tHash );

procedure SendFile( id: tFID );

procedure RecvFile( id :tFID );

procedure DoRetry;

procedure Retry( id :tFID );

IMPLEMENTATION
uses SysUtils
    ,DataBase
    ;

function IsPieced( id:tFID ):boolean;
 forward;
procedure GlobalAddDownload( id:tFID );
 forward;
procedure AddSource( id:tFID; source:Peers.tID );
 forward;

TYPE { database accessors }

 tPartAccess=object(DataBase.tAccess)
  ToRetrySearchFrom :Word;
  constructor Init( fid :tFID );
  procedure SetRequested( part :Word );
  procedure SetDone( part :Word );
  procedure SetTotal( total :Word );
  {
  procedure Abort;
  procedure ToRetry( var part :Word; out count :Word );
  }
 end experimental;

 tDataAccess=object(DataBase.tAccess)
  constructor Init( fid :tFID );
 end experimental;

 tPiecesAccess=object(DataBase.tAccess)
  constructor Init( fid :tFID );
 end experimental;

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

{
DoRetry;"
Retry(tFID);"
IsPieced(tFID):Boolean;"
GlobalAddDownload(tFID);"
AddSource(tFID,tID);"
}

END.