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

function IsPieced( id:tID ):boolean;
 forward;
procedure Retry( id :tID );
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

