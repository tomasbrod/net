unit Encap;

{
 Purpose of this unit is to allow bnctrl program to do actions that require 
 sending network packets from daemon's socket.
 
 Because if bnctl creates a new socket to send (eg) Akafuka the source port 
 will differ from the port used by daemon and we will newer get Fundeluka.
 
 Daemon shoud NEVER handle packets of this type from non-localhost.
}

INTERFACE
uses Peers
    ,NetAddr
    ,Log
    ;

const cEncap:tpktype=3;
const cEncapN:string='Encap';

type

 tEncap=packed object(Peers.tPacket)
  {This packet is created by daemon, when attempted to send data to address 
  of family different from daemon's socket's family}
 
  procedure Handle;
  procedure Create( Len:Word; var Data{:Peers.tPacket} );
  {Caller should allocate sizeof(tEncap) + Len bytes of memory for this object.}
  procedure Send;
  private
  Len :NetAddr.Word2;
  DestID : Peers.tID;
  Dest :NetAddr.t; {static size. for now}
  {Data follows; dynamic size}
  function StartOfData :pointer;
 end;

IMPLEMENTATION
uses SysUtils
    ;

procedure tEncap.Handle;
 var UnEncap: ^Peers.tPacket;
 var LenW:Word;
 begin
 { Do not call to inherited handle, because we do not want milion of localhost addresses in database.}
 log.msg('Received Encap');
 LenW:=Len;
 GetMem( UnEncap, LenW );
 Peers.SelectedAddr:= Dest;
 Peers.SelectedID:= DestID;
 Move( StartOfData^, UnEncap^, LenW );
 UnEncap^.Send(LenW); {This will send the packet from _our_ socket}
end;

procedure tEncap.Create( Len:Word; var Data );
 begin
 inherited Create(cEncap);
 assert(not Peers.SelectedAddr.isNil);
 self.Dest:=Peers.SelectedAddr;
 self.DestID:=Peers.SelectedID;
 self.Len:=Len;
 Move( Data, StartOfData^, Len );
end;

function tEncap.StartOfData :pointer;
 begin
 StartOfData:= pointer(@self) + SizeOf(self) {- sizeof(Dest) + Dest.Length};
end;

procedure tEncap.Send;
 var LenW:Word;
 begin
 LenW:=Len;
 Peers.SelectedAddr.LocalHost(Dest.data.Family);
 Peers.SelectedID.Clear;
 inherited Send( SizeOf(self) + LenW );
end;

END.