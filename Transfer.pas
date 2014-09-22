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

 tGet=object(Peers.tPacket)
  procedure Create( id :Keys.tHash; part, count :Word );
  procedure Handle;
  procedure Send;
  private
  id :Keys.tHash;
  part :NetAddr.Word2;
  count :NetAddr.Word2;
 end;
 
 tDat=object(Peers.tPacket)
  procedure Create( id :Keys.tHash; part :Word );
  procedure Handle;
  procedure Send;
  private
  id :Keys.tHash;
  part :NetAddr.Word2;
  total :NetAddr.Word2;
  PayLoad: array [1..cDataLength] of byte;
 end;

 tPcs=object(Peers.tPacket)
  procedure Create( id :Keys.tHash; part :Word );
  procedure Handle;
  procedure Send;
  private
  id :Keys.tHash;
  part :NetAddr.Word2;
  total :NetAddr.Word2;
  PayLoad: array [1..(cDataLength div sizeof(Keys.tHash))] of Keys.tHash;
 end;

procedure SendFile( id: tID );

procedure RecvFile( id :tID );

procedure DoRetry;

