unit uFormDaemonConnect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TFormDaemonConnect }

  TFormDaemonConnect = class(TForm)
    Button1: TButton;
    EditHost: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditPort: TSpinEdit;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormDaemonConnect: TFormDaemonConnect;

implementation

{$R *.lfm}

uses uMainForm,sSockets;

{ TFormDaemonConnect }

procedure TFormDaemonConnect.Button1Click(Sender: TObject);
begin
   try
    MainForm.Daemon:=tInetSocket.Create(edithost.text,editport.value);
    MainForm.LogListBox.Items.Append('Connected to '+edithost.text+':'+IntToStr(editport.value));
    MainForm.Listener:=tListenerThr.Create(false);
   except
    on e:eSocketError do begin
     MainForm.LogListBox.Items.Append('Connection failed: '+e.Message);
     FreeAndNil(MainForm.Daemon);
    end;
   end;
 self.hide;
end;

end.

