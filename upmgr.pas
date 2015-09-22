UNIT UPMGR;
{Upload Manager for brodnetd}
{mission:
 multiplex uploads thru TC connection
  - read files
  - add demux info
  - add sequence/offset
  - handle retransmit requests
  - handle all requests
  - deprioritize/cancel uploads
 keep one TC connection per peer (+expire-delete)
 => need 'chat' protocol
}

{to download a file:
 - send file request (chat)
   (GET channel CHK hash ofset length)
   - channel, client choose, reuse
 - prepare recv channel
 - wait reply (positive/negative)
}

INTERFACE
USES TC;

type tUH = procedure(var tcs:tTCS): of object;
procedure AddUpload(rcpt:tNetAddr; channel:byte; handler:tUH);
{delete with handler=nil}

IMPLEMENTATION

type tPeer_ptr=^tPeer; tPeer=object
 tcs: tTCS;
 prv: ^tUH; {dynamic array}
 prvc: word; {number of allocated items unused are nil}
 next:tPeer_ptr;
 procedure OnCont;
 function AllocChannel:word;
end;
var Peers:^tPeer;

function FindPeer(const addr:tNetAddr): tPeer_ptr;
 begin
 result:=Peers;
 while assigned(result) do begin
  if result^.tcs.remote=addr then exit;
  result:=result^.next;
 end;
end;

procedure AddUpload(rcpt:tNetAddr; channel:byte; handler:tUH);
 var peer:^tPeer;
 begin
 peer:=FindPeer(rcpt);
 if not assigned(peer) then begin
  New(peer);
  peer^.next:=Peers;
  peer^.prvc:=channel+1;
  peer^.prv:=GetMem(sizeof(peer^.prv)*peer^.prvc);
  Peers:=peer;
  peer^.tcs.Init(rcpt);
  peer^.tcs.CanSend:=@OnCont;
 end else begin
 
 end;
 if peer^.tcs.txLastSize=0 then peer^.tcs.Start;
 
 
