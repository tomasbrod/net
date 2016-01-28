unit MemStream;

INTERFACE
uses SysUtils;

procedure BinToHex(hexValue:pChar; const orig; len:word);

type tMemoryStream=object
 length: LongWord;
 size: LongWord;
 base: pointer;
 position: LongWord;
 procedure Seek(absolute:LongWord);
 procedure Skip(cnt:Word);
 procedure Read(var buf; cnt:Word);
 function  ReadPtr(cnt:Word):pointer;
 function  ReadByte:byte;
 function  ReadWord(cnt:byte): LongWord;
 procedure Trunc;
 procedure Append;
 function  Tell:LongWord;
 procedure Write(const buf; cnt:word);
 procedure WriteByte(v:byte);
 procedure WriteWord(v:LongWord; cnt:byte);
 procedure Init(ibuf:pointer; ilen,isize:LongWord);
 procedure Init(isize:LongWord);
 function WRBuf:pointer;
 function WRBufLen:LongWord;
 procedure WREnd(used:LongWord);
 function RDBuf:pointer;
 function RDBufLen:LongWord;
 procedure RDEnd(used:LongWord);
 end;

type eInvalidMemStreamAccess=class(Exception)
 {no details jet}
end;
type eReadPastEoF=class(Exception)
 {no details jet}
end;


IMPLEMENTATION

procedure tMemoryStream.Seek(absolute:LongWord);
 begin
 if absolute>size then raise eInvalidMemStreamAccess.Create('Seek out of bounds');
 position:=absolute;
end;

procedure tMemoryStream.Skip(cnt:Word);
 begin
 Seek(position+cnt);
end;

procedure tMemoryStream.Read(var buf; cnt:Word);
 begin
 if (position+cnt)>length then raise eReadPastEoF.Create('Read out of bounds');
 Move((base+position)^,buf,cnt);
 position:=position+cnt;
end;

function tMemoryStream.ReadPtr(cnt:Word):pointer;
 begin
 result:=base+position;
 skip(cnt);
end;

function  tMemoryStream.ReadByte:byte;
 begin Read(result, 1); end;

function  tMemoryStream.ReadWord(cnt:byte): LongWord;
 {$IFDEF ENDIAN_LITTLE}
 var tm:packed array [0..3] of byte;
 var i:byte;
 begin
 FillChar(tm,4,0);
 if (position+cnt)>length then raise eReadPastEoF.Create('Read out of bounds');
 for i:=cnt-1 downto 0 do begin
  tm[i]:=byte((base+position)^);
  inc(position);
 end;
 {$ELSE}
 begin
 Read(tm[4-cnt],cnt);
 {$ENDIF}
 ReadWord:=LongWord(pointer(@tm)^);
end;
 
procedure tMemoryStream.Trunc;
 begin length:=position; end;
procedure tMemoryStream.Append;
 begin position:=length; end;
function tMemoryStream.Tell:LongWord;
 begin Tell:=position; end;

procedure tMemoryStream.Write(const buf; cnt:word);
 begin
 if (position+cnt)>size then raise eInvalidMemStreamAccess.Create('Write out of bounds');
 Move(buf,(base+position)^,cnt);
 position:=position+cnt;
 if position>length then length:=position;
end;
procedure tMemoryStream.WriteByte(v:byte);
 begin Write(v,1); end;

procedure tMemoryStream.WriteWord(v:LongWord; cnt:byte);
 var tm:packed array [0..3] of byte absolute v;
 var i:byte;
 begin
 {$IFDEF ENDIAN_LITTLE}
 if (position+cnt)>size then raise eInvalidMemStreamAccess.Create('Write out of bounds');
 for i:=cnt-1 downto 0 do begin
  byte((base+position)^):=tm[i];
  inc(position);
 end;
 if position>length then length:=position;
 {$ELSE}
 Write(tm[4-cnt],cnt);
 {$ENDIF}
end;

procedure tMemoryStream.Init(ibuf:pointer; ilen,isize:LongWord);
 begin
 base:=ibuf;
 length:=ilen;
 size:=isize;
 seek(0);
end;
procedure tMemoryStream.Init(isize:LongWord);
 begin
 Init(GetMem(isize),0,isize);
end;

function tMemoryStream.WRBuf:pointer;
 begin result:=base+position end;
function tMemoryStream.WRBufLen:LongWord;
 begin result:=size-position end;
procedure tMemoryStream.WREnd(used:LongWord);
 begin RDEnd(used); if position>length then length:=position end;
function tMemoryStream.RDBuf:pointer;
 begin result:=base+position end;
function tMemoryStream.RDBufLen:LongWord;
 begin result:=length-position end;
procedure tMemoryStream.RDEnd(used:LongWord);
 begin skip(used) end;

const
  HexTbl: array[0..15] of char='0123456789ABCDEF';
procedure BinToHex(hexValue:pChar; const orig; len:word);
 var i:word;
 var b:array [byte] of byte absolute orig;
 begin
 dec(len);
 for i:=0 to len do begin
  hexValue[i*2+0]:=HexTbl[b[i] shr 4];
  hexValue[i*2+1]:=HexTbl[b[i] and 15];
 end;
end;

END.

