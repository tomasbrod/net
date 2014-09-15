unit Remote;

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

const cCtrl:tpktype=3;
const cCtrlN:string='RemoteControll';

type

 tCtrl=packed object(Peers.tPacket)
  procedure Handle;
  constructor PeerAdd( iAddr :NetAddr.t );
  constructor DoAkafuka;
  private
  data: packed record
   case func : ( fPeerAdd=86, fDoAkafuka ) of
    fPeerAdd: (
     Addr :NetAddr.t;
    );
    fDoAkafuka:();
  end;
 end;

IMPLEMENTATION
uses SysUtils
    ;

procedure tCtrl.Handle;
 begin
 inherited Handle;
 log.msg('Received RemoteControll #'+IntToStr(byte(data.func)));
 case data.func of
  fPeerAdd: Peers.Add( data.Addr );
  fDoAkafuka: Peers.DoAkafuka;
 end;
end;

constructor tCtrl.PeerAdd( iAddr :NetAddr.t );
 begin
 Create(cCtrl);
 data.Func:=fPeerAdd;
 data.Addr:=iAddr;
 Send( sizeof(self) );
 end;
 
constructor tCtrl.DoAkafuka;
 begin
 Create(cCtrl);
 data.Func:=fDoAkafuka;
 Send( sizeof(self) );
end;

END.