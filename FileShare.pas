unit FileShare;

INTERFACE
uses GeneralPacket
    ,Keys
    ;

const cReq = 4;
const cAns = 5;
const pktype :set of tPkType = [cReq, cAns];

type
 tAsk =packed object(GeneralPacket.T)
  procedure Handle;
  procedure Create;
  procedure AddTag( itag :HashedTag );
  procedure AddTag( itag :string );
  procedure Send;
  private
  tags :tUltimateHashesOfTags; {*Warning: dynamic size!}
 end;

type
 tPut= packed object(GeneralPacket.T)
  procedure Handle;
  procedure Create( ifh:tKeys.hash ) ;
  procedure AddTag( itag :HashedTag );
  procedure AddTag( itag :string );
  procedure Send;
  private
  FileHash :tKeys.tHash;
  tags :tUltimateHashesOfTags; {*Warning: dynamic size!}
 end;

IMPLEMENTATION
uses Peers
    ;

END
.