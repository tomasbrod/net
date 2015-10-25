program brodnet;

{$mode objfpc}{$H+}

uses
  Classes,fpg_main,Forms1;

procedure MainProc;
var frm:tFormRoot;
begin
  fpgApplication.Initialize;
  frm:=tFormRoot.Create(nil);
  fpgApplication.MainForm:=frm;
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


