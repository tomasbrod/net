UNIT GeneralPacket;

INTERFACE

var 
  RecvProc: procedure(var Data; MaxLen:LongInt);
  ReplProc: procedure(var Data; MaxLen:LongInt);

type
 tPktype =byte;
 T =object
  pktype :tPktype;
  procedure Recv(MaxLen:LongInt);
  procedure Create (itp :tPktype);
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

procedure T.Create (itp :tPktype);
begin
 pktype := itp;
end;


END.
