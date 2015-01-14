unit Parted experimental;

INTERFACE
USES  SysUtils
     ,NetAddr
     ,Peers
     ,Keys
     ;

CONST
 cDataLength=512;

TYPE

 tRequest=object(Peers.tPacket)
  procedure Create( id: Keys.tHash; part, count :Word );
  procedure Send;
  private
  id :Keys.tHash;
  part, count :NetAddr.Word2;
 end;

 tResponse=object(Peers.{tLightPacket}tPacket)
  procedure Create( id: Keys.tHash; part, total :Word );
  function StartOfData :Pointer;
  procedure Send;
  private
  id :Keys.tHash;
  part, total :NetAddr.Word2;
 end;

IMPLEMENTATION

{
Uses ObjDB;
procedure SendParts( id :Keys.tHash; base, limit :Word );
 var db:ObjDB.tDataAccess;
 var response :^tResponse;
 var Len :LongInt;
 var cn :word;
 begin
 db.init( id );
 for cn:=base to base+limit do begin
  Len:= db.Length - (cn*cDataLength);
  if Len<1 then AbstractError;
  if Len>cDataLength then Len:=cDataLength;
  GetMem( response, Len );
  try
   //Peers.Select( Sender );
   with response^ do begin
    Create( cn, roundup(db.Length/cDataLength) );
    db.BlockRead( StartOfData^, cn*cDataLength, Len );
    Send;
   end;
  finally
   FreeMem( response, Len );
  end;
 end;
}

procedure tRequest.Create( id: Keys.tHash; part, count :Word );
 begin
 self.id:=id;
 self.part:=part;
 self.count:=count;
end;

procedure tRequest.Send;
 begin Peers.tPacket.Send( sizeof(self) ); end;

procedure tResponse.Create( id: Keys.tHash; part, total :Word );
 begin
 self.id:=id;
 self.part:=part;
 self.total:=total;
end;

procedure tResponse.Send;
 begin Peers.tPacket.Send( sizeof(self) ); end;

function tResponse.StartOfData :Pointer;
 begin
 StartOfData:=pointer(@self) + Sizeof(self);
end;

END.