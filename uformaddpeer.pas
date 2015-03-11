unit uFormAddPeer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormAddPeer }

  TFormAddPeer = class(TForm)
    Button3: TButton;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    procedure Button3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormAddPeer: TFormAddPeer;

implementation

Uses uMainForm,CtrlIface,NetAddr;

{$R *.lfm}

{ TFormAddPeer }

procedure TFormAddPeer.Button3Click(Sender: TObject);
var a:netaddr.t;
 begin
  try
  a:=Edit1.Text;
  MainForm.Daemon.WriteByte(ccAddPeer);
  MainForm.Daemon.WriteBuffer(a,sizeof(a));
  Self.Hide;
  except
   on e:eConvertError do ShowMessage('Wrong format '+e.message);
  end;
end;

end.

