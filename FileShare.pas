unit FileShare;

INTERFACE
uses GeneralPacket
    ,Keys
    ;

const cReq = 4;
const cAns = 5;
const pktype :set of tPkType = [cReq, cAns];
const cMaxTagsPerFile=284;

type
 tTagHash = object
  data: array [1..4] of byte;
 end unimplemented;

type
 tAsk =packed object(GeneralPacket.T)
  procedure Handle;
  procedure Create;
  procedure AddTag( itag :HashedTag );
  procedure AddTag( itag :string );
  procedure Send;
  private
  tags :array [1..cMaxTagsPerFile] of tTagHash;
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
  tags :array [1..cMaxTagsPerFile] of tTagHash;
 end;

IMPLEMENTATION
uses Peers
    ;

END
.