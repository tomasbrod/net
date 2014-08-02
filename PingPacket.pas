unit PingPacket;

INTERFACE
uses GeneralPacket;

const pktype=1;


type
 T =object(GeneralPacket.T)
  procedure Handle;
  {
  procedure Send;
  procedure Create;
  }
 end;

IMPLEMENTATION
{uses PongPacket;}

procedure T.Handle;
 unimplemented;
{var Pong:PongPacket.T;}
begin
 {
 Pong.Create;
 Pong.Repl;
 }
 AbstractError;
end;

END.
