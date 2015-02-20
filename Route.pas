unit Route;

(* Objectives:

 Provide consistent api to get routes (netaddr of next peer) to queries by 
 ID or TAG sorted by quality.

*)

INTERFACE
uses NetAddr, Neighb;

procedure GetRoute( out peer:array of netaddr.t; PID: Neighb.tPID );

{procedure GetRoute( out peer:netaddr.t; PID: Neighb.tPID; TAGs: tTagSet );}

IMPLEMENTATION
uses Peers, Keys;

procedure GetRoute( out peer:array of netaddr.t; PID: Neighb.tPID );
 var i:word;
 var cur:pointer;
 begin
 cur:=nil;
 for i:=low(peer) to high(peer) do begin
  Neighb.GetRoute(PID, peer[i], cur );
  if cur=nil then begin
   peer[i].Clear;
   break;
  end;
 end;
end;


END.