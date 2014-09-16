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
  procedure Create( Len:LongWord; var Data{:Peers.tPacket} );
  {Caller should allocate sizeof(tEncap) + Len bytes of memory for this object.}
  procedure Send;
  private
  Len :{NetAddr.Word2}LongWord platform;
  Dest :NetAddr.t; {static size. for now}
  {Data follows; dynamic size}
 end;

IMPLEMENTATION
uses SysUtils
    ;

procedure tEncap.Handle;
 var UnEncap: ^Peers.tPacket;
 var StartOfData:Pointer;
 begin
 { Do not call to inherited handle, because we do not want milion of localhost addresses in database.}
 log.msg('Received Encap');
 GetMem( UnEncap, Len );
 StartOfData:= pointer(@self) + SizeOf(self) {- sizeof(Dest) + Dest.Length};
 Peers.SelectedAddr:= Dest;
 Peers.IsSelectedAddr:= True;
 Move( StartOfData^, UnEncap^, Len );
 UnEncap^.Send(Len); {This will send the packet from _our_ socket}
end;

procedure tEncap.Create( Len:LongWord; var Data );
 begin
 inherited Create(cEncap);
 assert(Peers.IsSelectedAddr);
 self.Dest:=Peers.SelectedAddr;
 self.Len:=Len;
end;

procedure tEncap.Send;
 begin
 Peers.SelectedAddr.LocalHost(Dest.data.Family);
 inherited Send( SizeOf(self) + Len );
end;

END.