unit Controll;
(* Communication between applications and running brodnet daemon. *)

INTERFACE

uses  SocketUtil
     ,NetAddr
     ,Classes
     ;

type tDaemonController=class (tObject)
  public
  Socket: tStream;
  finished:boolean;
  procedure Run; unimplemented;
  constructor Create(asocket:tStream; from:NetAddr.t);
  destructor Destroy; override;
end;

{todo:
- Aby sa upratal trochu loop v daemone:
  - poskytne proceduru co modifikuje select-set
  - ohandluje si callback z daemona z vysledkyu zo selectu
  - Chceme abstrahovať socket, aby sa to dalo spustit aj bez daemona?
    - naco, aj tak to bez neho nejde
- manažuje si pripojenych sam
- handluje peerstatechange, terminate, a vsetko
}

IMPLEMENTATION

procedure tDaemonController.Run;
 begin
 abstracterror;
end;

constructor tDaemonController.Create(asocket:tStream; from:NetAddr.t);
begin
 finished:=false;
 socket:=asocket;
end;

destructor tDaemonController.Destroy;
 begin
end;

END.