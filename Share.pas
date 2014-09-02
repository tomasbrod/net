unit Share;

INTERFACE
uses GeneralPacket
    ,Keys
    ,Tag
    ;

const cReq = 4;
const cAns = 5;
const pktype :set of tPkType = [cReq, cAns];
const cMaxTagsPerFile=284;

type
 T =packed object(GeneralPacket.T)
  procedure Handle;
  procedure Create; (* ASK packet *)
  procedure Create( ifh :Keys.tHash ); (* PUT packet *)
  procedure AddTag( itag :FileTag.T );
  procedure AddTag( itag :string ); (* automatically converts to tTagHash *)
  procedure Send;
  private
  fh :Keys.tHash;
  tags :array [1..cMaxTagsPerFile] of FileTag.T;
 end;

IMPLEMENTATION
uses Peers
    ;

procedure T.Create;
var i:integer;
begin
 inherited Create(cReq);
 fh.Clear;
 for i:=low(tags) to high(tags) do tags[i].Clear;
end;

procedure T.Create ( ifh :Keys.tHash );
var i:integer;
begin
 inherited Create(cAns);
 fh:=ifh;
 for i:=low(tags) to high(tags) do tags[i].Clear;
end;

procedure T.AddTag( itag :FileTag.T );
var i :integer;
begin
 for i:=low(tags) to high(tags) do if tags[i].isNil then begin
  tags[i]:=itag;
  break;
 end;
end;

procedure T.AddTag( itag :string );
var th: FileTag.T;
begin
 th.Create( itag );
 AddTag( th );
end;

procedure T.Handle;
var search :Files.tSearch;
var fr :Files.tRef;
var pk :T;
BEGIN
 if pktype=cReq then begin
  search.ByMatch(tags, high(tags) );
  while search.Find do begin
   fr:=search.Get;
   pk.Create(fr.Hash);
   pk.Tags=fr.Tags^;
  end;
 end;
 if pktype=cAns then begin
  Files.AddIndex(fh, tags, high(tags) );
 end;
END;
   
   

END
.