UNIT SockStream;
INTERFACE
USES Sockets,BaseUnix,ObjectModel;

type tSocketStream=object(tCommonStream)
  h:tSocket;
  constructor Init(isck:tSocket);
  procedure Read(out blk; len:word); virtual;
  procedure Write(const blk; len:word); virtual;
  end;
procedure SC(fn:pointer; retval:cint); inline;

IMPLEMENTATION
uses SysUtils;

procedure SC(fn:pointer; retval:cint); inline;
  begin
  if retval < 0 then begin
    raise eXception.Create(Format('Socket error %d operation %P',[SocketError,fn]));
  end;
end;

constructor tsocketstream.init(isck:tsocket);
  begin Inherited init;
  h:=isck end;
procedure tSocketStream.Read(out blk; len:word);
  var rd:LongInt;
  var dst:pointer;
  begin dst:=@blk;
  while len>0 do begin
    rd:=fpRecv(h,dst,len,0);
    if rd<=0 then SC(@fpRecv,rd-1);
    dst:=dst+rd;
    len:=len-rd;
  end;
end;

procedure tSocketStream.Write(const blk; len:word);
  var wd:LongInt;
  var src:pointer;
  begin
  src:=@blk;
  while len>0 do begin
  wd:=fpSend(h,src,len,0);
  if wd<=0 then SC(@fpSend,wd-1);
  src:=src+wd;
  len:=len-wd;
 end;
end;

END.
