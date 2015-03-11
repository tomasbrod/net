unit uMainForm; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ComCtrls, EditBtn, Grids, sSockets, cthreads;
type

tListenerThr=class(TThread)
  private procedure ShowStatus;
  private event:byte;
  protected procedure Execute; override;
  public Constructor Create(CreateSuspended : boolean);
  public disabled:boolean;
end;

  { TmainForm }

  TmainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBoxLogEnabled: TCheckBox;
    LogListBox: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemAddPeer: TMenuItem;
    MenuItemDaemonDisconnect: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemDaemonConnect: TMenuItem;
    MenuItemDaemon: TMenuItem;
    PageControl1: TPageControl;
    StringGridNeighb: TStringGrid;
    TabSheetNeighb: TTabSheet;
    TabSheetLog: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure CheckBoxLogEnabledChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItemAddPeerClick(Sender: TObject);
    procedure MenuItemDaemonConnectClick(Sender: TObject);
    procedure MenuItemDaemonDisconnectClick(Sender: TObject);
    procedure TabSheetNeighbShow(Sender: TObject);
    procedure OnDisconnect;

  private
    { private declarations }
  public
    { public declarations }
    daemon: tInetSocket;
    listener: tListenerThr;
end;

var
  mainForm: TmainForm;

implementation

{$R *.lfm}

uses uFormAddPeer,uFormDaemonConnect,CtrlIface,Neighb,Keys,NetAddr;

{ TmainForm }

procedure TmainForm.MenuItem1Click(Sender: TObject);
begin
 LogListBox.Items.Append('Lol '+DateTimeToStr(now));
 StringGridNeighb.Cells[0,1]:='Zidan';
end;

procedure TmainForm.MenuItem3Click(Sender: TObject);
begin
  Daemon.WriteByte(ccTerminate);
end;

procedure TmainForm.MenuItemAddPeerClick(Sender: TObject);
begin
 FormAddPeer.Show;
end;

procedure TmainForm.MenuItemDaemonConnectClick(Sender: TObject);
begin
  MenuItemDaemonDisconnectClick(sender);
  FormDaemonConnect.Show;
end;

procedure TmainForm.MenuItemDaemonDisconnectClick(Sender: TObject);
begin
 if assigned(daemon) then begin
  Daemon.WriteByte(ccQuit);
  OnDisconnect;
 end;
end;

procedure tMainForm.OnDisconnect;
begin
  FreeAndNil(Daemon);
  Listener.Terminate;
  CheckBoxLogEnabled.Checked:=false;
  CheckBoxLogEnabled.Enabled:=false;
end;

procedure TmainForm.TabSheetNeighbShow(Sender: TObject);
 begin
  StringGridNeighb.RowCount:={StringGridNeighb.FixedRows} 1;
  if not assigned(Daemon) then exit;
  Daemon.WriteByte(ccGetNeighb);
end;

procedure TmainForm.Button1Click(Sender: TObject);
begin
 LogListBox.Items.Clear;
end;

procedure TmainForm.CheckBoxLogEnabledChange(Sender: TObject);
begin
  if assigned(daemon) then
  Daemon.WriteByte(ccPeerStates);
end;

procedure TmainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MenuItemDaemonDisconnectClick(Sender);
end;

procedure TmainForm.FormShow(Sender: TObject);
begin
  Daemon:=nil;
  FormDaemonConnect.Show;
end;

constructor tListenerThr.Create(CreateSuspended : boolean);
 begin
 FreeOnTerminate := False;
 inherited Create(CreateSuspended);
end;

procedure tListenerThr.ShowStatus;
 var tmp:byte;
 var tmw:netaddr.word2;
 var line:string;
 procedure PrintAddr;
  var a:netaddr.t;
  begin
  MainForm.Daemon.ReadBuffer(a,sizeof(a));
  line:=line+string(a);
 end;
 procedure DumpNeighb;
  var i:tNeighbInfo;
  var rowi:longint;
  begin
  MainForm.Daemon.ReadBuffer(i,sizeof(i));
  with MainForm.StringGridNeighb do begin
      rowi:=RowCount;
      RowCount:=RowCount +1;
      Cells[0, rowi] := String(i.pid);
      Cells[1, rowi] := String(i.addr);
      Cells[2, rowi] := IntToStr(word(i.hop));
  end;
 end;
 begin
 line:='';
 case event of
  ceQuit: begin
   line:='Daemon ';
   tmp:=MainForm.Daemon.ReadByte;
   case tmp of
    0: line:=line+' terminating by request';
    1: line:=line+' crashed';
    else line:=line+'terminating reason='+IntToStr(tmp);
  end; end;
  ceInvalid: line:='Invalid command';
  255: begin line:='Daemon disconnected'; MainForm.OnDisconnect; end;
  cePeerState: begin
   line:='Peer ';
   tmp:=MainForm.Daemon.ReadByte;
   case tmp of
    0: line:=line+'ping';
    1: line:=line+'NEW!';
    2: line:=line+'REKT';
    else line:=line+'ev='+inttostr(tmp);
   end;line:=line+' ';
   PrintAddr;line:=line+' ';
   MainForm.Daemon.ReadBuffer(tmw,2);
   line:=line+'delta='+IntToSTr(word(tmw))+'ms';
  end;
  ceNeighbs:repeat
    DumpNeighb;
    event:=MainForm.Daemon.ReadByte;
   until (event=ceNeighbsEnd) or (event<>ceNeighbs);
  else Line:='?event'+IntToStr(event);
 end;
 if length(line)>0 then begin
  MainForm.LogListBox.Items.Add(line);
  MainForm.TabSheetLog.Show;
 end;
end;

procedure tListenerThr.Execute;
 begin
 while (not Terminated) and assigned(MainForm.Daemon) do begin
  if MainForm.Daemon.Read(event,1)<>1 then event:=255;
  Synchronize(@Showstatus);
 end;
end;

end.

