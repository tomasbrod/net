PROGRAM bnDaemon;
(*
  Parameters:
   %1 ... io type
     ='S' -> io is socket
     ='C' -> io is character device
 *)
 
USES SysUtils
	,Sockets
	;

var
 InputBuffer:array [1..2048] of byte;


PROCEDURE LoopOnSocket;
 experimental;
const sock:tSocket=0;
var rlength;
begin
 repeat
  recv
 unitl false;
end;

PROCEDURE LoopOnCharDev;
 unimplemented;
begin
 abort;
end;

BEGIN
 case paramstr(1) of
  'S' : LoopOnSocket;
  'C' : LoopOnCharDev;
 end;
END.
