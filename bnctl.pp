PROGRAM bnctl;
 
{$MODE OBJFPC}{$C+}
USES SysUtils
    ,Sockets
    ,UnixType
	,DataBase
	,Peers
	,IniFiles
	,Log
	,NetAddr
	,Remote
	;

{$I bncommon.pp}

procedure PeerAdd;
 var addr:NetAddr.t;
 var ctrl:Remote.tCtrl;
 begin
 addr.fromstring(paramstr(1));
 ctrl.PeerAdd( Addr );
end;

procedure PeerDoAkafuka;
 var ctrl:Remote.tCtrl;
 begin
 ctrl.DoAkafuka;
end;

procedure PeerInfo;
 var addr:NetAddr.t;
 var id :Peers.tID;
 var str:string;
 begin
 id.FromString(paramstr(1));
 Peers.Select(id);
 Peers.SelectedID.ToString(str);
 writeln(output,'ID  :',str);
 Peers.SelectedAddr.ToString(str);
 writeln(output,'ADDR:',str);
 writeln(output,'DLTA:','?');
end;

BEGIN
 Log.F:=stderr; //log to stderr
 try
  Init;
  SelectedAddr.FromString('//ip4/127.0.0.1/1030');
  if FindCmdLineSwitch('pa') then PeerAdd;
  //if FindCmdLineSwitch('pl') then PeerList;
  if FindCmdLineSwitch('pi') then PeerInfo;
  if FindCmdLineSwitch('pakafuka') then PeerDoAkafuka;

  {ToDo: process cmdline args
   FindCmdLineSwitch('s');
  }
 except
  on e : eSocket do begin
   Log.msg('Socket error '+IntToStr(e.code));
   DumpExceptionBackTrace(Log.F);
  end;
  on e : Exception do begin
   Log.msg(e.ClassName+': '+e.message);
   DumpExceptionBackTrace(Log.F);
  end;
 end;
 Log.msg('Bye');
END.
