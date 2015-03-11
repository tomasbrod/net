program brodnetgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMainForm, uFormAddPeer, uFormDaemonConnect;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TFormAddPeer, FormAddPeer);
  Application.CreateForm(TFormDaemonConnect, FormDaemonConnect);
  Application.Run;
end.

