UNIT SockStream deprecated;
INTERFACE
USES Sockets,BaseUnix,ObjectModel;


procedure SC(fn:pointer; retval:cint); inline;

IMPLEMENTATION
uses SysUtils;

procedure SC(fn:pointer; retval:cint); inline;
  begin
  if retval < 0 then begin
    raise eXception.Create(Format('Socket error %d operation %P',[SocketError,fn]));
  end;
end;

END.
