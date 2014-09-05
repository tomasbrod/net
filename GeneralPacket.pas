UNIT GeneralPacket;

INTERFACE

var 
  RecvProc: procedure(var Data; MaxLen:LongInt);
  ReplProc: procedure(var Data; MaxLen:LongInt);

type
 tPktype =byte;
 T = packed object
  pktype :tPktype;
  procedure Recv(MaxLen:LongInt);
  constructor Create (itp :tPktype);
  procedure Handle; unimplemented;
  procedure Repl(MaxLen:LongInt);
   deprecated; { bad design }
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

constructor T.Create (itp :tPktype);
begin
 pktype := itp;
end;

procedure T.Handle; begin AbstractError; end;

END.
