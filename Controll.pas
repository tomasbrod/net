unit Controll;
(* Communication between applications and running brodnet daemon. *)

INTERFACE

uses  SocketUtil
     ,sSockets{#fcl.net}
     ;

type tDaemonController=class (tObject)
  public
  Socket: tSocketStream;
  procedure Run; unimplemented;
  constructor Create;
  destructor Destroy; override;
end;

IMPLEMENTATION

procedure tDaemonController.Run;
 begin
 abstracterror;
end;

constructor tDaemonController.Create;
 begin
 abstracterror;
end;

destructor tDaemonController.Destroy;
 begin
 abstracterror;
end;

END.