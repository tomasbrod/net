UNIT GeneralPacket;

INTERFACE

var 
  RecvProc: procedure(var Data; MaxLen:LongInt) of object;
  SendProc: procedure(var Data; MaxLen:LongInt) of object;

type 
 T =object
  pktype :byte;
  procedure Recv(MaxLen:LongInt);
  procedure Send(MaxLen:LongInt);
 end;

IMPLEMENTATION

procedure T.Recv(MaxLen:LongInt);
begin
 RecvProc(self,MaxLen);
end;

procedure T.Send(MaxLen:LongInt);
begin
 SendProc(self,MaxLen);
end;

END.
