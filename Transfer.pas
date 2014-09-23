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

 tID=object(Keys.tHash)
 end;
 
 tGet=object(Peers.tPacket)
  procedure Create( id :tID; part, count :Word );
  procedure Handle;
  procedure Send;
  private
  id :tID;
  part :NetAddr.Word2;
  count :NetAddr.Word2;
 end;
 
 tDat=object(Peers.tPacket)
  procedure Create( id :tID; part :Word );
  procedure Handle;
  procedure Send;
  private
  id :tID;
  part :NetAddr.Word2;
  total :NetAddr.Word2;
  PayLoad: array [1..cDataLength] of byte;
 end;

 tPcs=object(Peers.tPacket)
  procedure Create( id :tID; part :Word );
  procedure Handle;
  procedure Send;
  private
  id :tID;
  part :NetAddr.Word2;
  total :NetAddr.Word2;
  PayLoad: array [1..(cDataLength div sizeof(tID))] of tID;
 end;

var OnNoSrc :procedure( id :tHash );
var OnRecv  :procedure( id :tHash );

procedure SendFile( id: tID );

procedure RecvFile( id :tID );

procedure DoRetry;

procedure Retry( id :tID );

IMPLEMENTATION
uses SysUtils;

function IsPieced( id:tID ):boolean;
 forward;
procedure GlobalAddDownload( id:tID );
 forward;
procedure AddSource( id:tID; source:Peers.tID );
 forward;

procedure SendFile( id: tID );
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

procedure RecvFile( id:tID );
 begin
 GlobalAddDownload( id );
 AddSource( id, Peers.SelectedID );
 Retry( ID );
end;

procedure tGet.Create( id :tID; part, count :Word );
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

procedure tDat.Create( id :tID; part :Word );
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
  cLen:= db.Length - Ofs;
  if cLen<1 then raise eRangeError.Create('');
  if cLen>high(self.PayLoad) then cLen:=high(self.PayLoad);
  Len:=cLen;
  if (db.Length mod high(self.PayLoad))=0
   then self.total:= (db.Length div high(self.PayLoad))
   else self.total:= (db.Length div high(self.PayLoad)) +1
  ;
  db.BlockRead( self.PayLoad, Ofs, cLen );
 finally db.done; end;
end;

procedure tDat.Send;
 begin
 Peers.tPacket.Send( sizeof(self) );
end;

procedure tPcs.Create( id:tID; part:Word );
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
  cLen:= db.Length - Ofs;
  if cLen<1 then raise eRangeError.Create('');
  if cLen>high(self.PayLoad) then cLen:=high(self.PayLoad);
  Len:=cLen;
  if (db.Length mod high(self.PayLoad))=0
   then self.total:= (db.Length div high(self.PayLoad))
   else self.total:= (db.Length div high(self.PayLoad)) +1
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
tPartAccess
tDataAccess
tPiecesAccess
DoRetry;"
Retry(tID);"
IsPieced(tID):Boolean;"
GlobalAddDownload(tID);"
AddSource(tID,tID);"
}

END.