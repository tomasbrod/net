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
  (* Tags are SHA255 hashes of tag plaintext *)
  procedure Create( plaintext :string);
  procedure Clear;
  function  isNil :boolean;
  private
  data: array [1..4] of byte;
 end unimplemented;

type
 T =packed object(GeneralPacket.T)
  procedure Handle;
  procedure Create; (* ASK packet *)
  procedure Create( ifh :Keys.tHash ); (* PUT packet *)
  procedure AddTag( itag :tTagHash );
  procedure AddTag( itag :string ); (* automatically converts to tTagHash *)
  procedure Send;
  private
  fh :Keys.tHash;
  tags :array [1..cMaxTagsPerFile] of tTagHash;
 end;

IMPLEMENTATION
uses Peers
    ;

procedure T.Create;
var i:byte;
begin
 inherited Create(cReq);
 fh.Clear;
 for i:=low(tags) to high(tags) do tags[i].Clear;
end;

procedure T.Create ( ifh :Keys );
var i:byte;
begin
 inherited Create(cAns);
 fh:=ifh;
 for i:=low(tags) to high(tags) do tags[i].Clear;
end;

END
.