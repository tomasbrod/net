UNIT GeneralPacket;

INTERFACE

var 
  RecvProc: procedure(var Data; MaxLen:LongInt);
  ReplProc: procedure(var Data; MaxLen:LongInt);

type 
 T =object
  pktype :byte;
  procedure Recv(MaxLen:LongInt);
  procedure Repl(MaxLen:LongInt);
 end;

IMPLEMENTATION

procedure T.Recv(MaxLen:LongInt);
begin
 RecvProc(self,MaxLen);
end;

procedure T.Repl(MaxLen:LongInt);
begin
 ReplProc(self,MaxLen);
end;

END.
