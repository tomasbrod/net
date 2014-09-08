UNIT GeneralPacket;

INTERFACE

var 
  RecvProc: procedure(var Data; MaxLen:LongInt);
  SendProc: procedure(var Data; MaxLen:LongInt);

type
 tPktype =byte;
 T = packed object
  pktype :tPktype;
  procedure Recv(MaxLen:LongInt);
  constructor Create (itp :tPktype);
  procedure Handle; unimplemented;
  procedure Send(MaxLen:LongInt);
   deprecated; { bad design }
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

constructor T.Create (itp :tPktype);
begin
 pktype := itp;
end;

procedure T.Handle; begin AbstractError; end;

END.
