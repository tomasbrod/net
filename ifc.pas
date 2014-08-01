unit IFC;

(* This unit listens sockets and sends *)

INTERFACE

uses DataGram
;

procedure BroadCast(var DataGram.T);

{
procedure UniCast(var DataGram.T; .....);
procedure Init;
}

IMPLEMENTATION

END.
