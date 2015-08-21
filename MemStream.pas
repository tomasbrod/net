unit MemStream;

INTERFACE
uses SysUtils;
type tMemoryStream=object
 length: LongWord;
 size: LongWord;
 data: pointer;
 position: pointer;
 procedure Seek(absolute:LongWord);
 procedure Skip(cnt:Word);
 procedure Read(var buf; cnt:Word);
 function  ReadByte:byte;
 function  ReadWord(cnt:byte): LongWord; experimental;
 procedure Rewind;
 procedure Append;
 function  Tell:LongWord;
 procedure Write(var buf; cnt:word);
 procedure WriteByte(v:byte);
 procedure WriteWord(v:LongWord; cnt:byte); experimental;
 procedure Init(ibuf:pointer; ilen,isize:LongWord);
 end;

type eInvalidMemStreamAccess=class(Exception)
 {no details jet}
end;

IMPLEMENTATION

procedure tMemoryStream.Seek(absolute:LongWord);
 begin
 if absolute>=size then raise eInvalidMemStreamAccess.Create('Seek out of bounds');
 position:=data+absolute;
end;

procedure tMemoryStream.Skip(cnt:Word);
 begin
 if cnt>=size then raise eInvalidMemStreamAccess.Create('Seek out of bounds');
 position:=position+cnt;
end;

procedure tMemoryStream.Read(var buf; cnt:Word);
 begin
 if (position+cnt)>=(data+length) then raise eInvalidMemStreamAccess.Create('Read out of bounds');
 Move(position^,buf,cnt);
 position:=position+cnt;
end;

function  tMemoryStream.ReadByte:byte;
 begin Read(result, 1); end;

function  tMemoryStream.ReadWord(cnt:byte): LongWord;
 {$IFDEF ENDIAN_LITTLE}
 var tm:packed array [0..3] of byte;
 var i:byte;
 begin
 if (position+cnt)>=(data+length) then raise eInvalidMemStreamAccess.Create('Read out of bounds');
 for i:=cnt-1 downto 0 do begin
  tm[i]:=byte((position)^);
  inc(position);
 end;
 {$ELSE}
 begin
 Read(tm[4-cnt],cnt);
 {$ENDIF}
 ReadWord:=LongWord(pointer(@tm)^);
end;
 
procedure tMemoryStream.Rewind;
 begin position:=data; end;
procedure tMemoryStream.Append;
 begin position:=data+length; end;
function  tMemoryStream.Tell:LongWord;
 begin Tell:=position-data; end;
procedure tMemoryStream.Write(var buf; cnt:word);
 begin
 if (position+cnt)>=(data+size) then raise eInvalidMemStreamAccess.Create('Write out of bounds');
 Move(buf,position^,cnt);
 length:=position-data;
end;
procedure tMemoryStream.WriteByte(v:byte);
 begin Write(v,1); end;

procedure tMemoryStream.WriteWord(v:LongWord; cnt:byte);
 var tm:packed array [0..3] of byte absolute v;
 var i:byte;
 begin
 {$IFDEF ENDIAN_LITTLE}
 if (position+cnt)>=(data+size) then raise eInvalidMemStreamAccess.Create('Write out of bounds');
 for i:=cnt-1 to 0 do begin
  byte(position^):=tm[i];
  inc(position);
 end;
 {$ELSE}
 Write(tm[4-cnt],cnt);
 {$ENDIF}
 length:=position-data;
end;

procedure tMemoryStream.Init(ibuf:pointer; ilen,isize:LongWord);
 begin
 data:=ibuf;
 length:=ilen;
 size:=isize;
end;

END.

