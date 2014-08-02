unit PingPacket;

INTERFACE
uses GeneralPacket;

const cT=1;


type
 T =object(GeneralPacket.T)
  procedure Handle;
 end;

IMPLEMENTATION

procedure T.Handle;
 unimplemented;
begin
 AbstractError;
end;

END.
