unit Forms1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main,
  fpg_form, fpg_button, fpg_tab, fpg_edit,
  fpg_label, fpg_checkbox,fpg_gauge, fpg_panel,fpg_dialogs;
type

  TFormTestFS = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: FormTestFS}
    GroupBox1: TfpgGroupBox;
    EditFID: TfpgEdit;
    ButtonDelete: TfpgButton;
    ButtonLookup: TfpgButton;
    EditSRC: TfpgEdit;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    GroupBox2: TfpgGroupBox;
    LabelTotal: TfpgLabel;
    LabelRate: TfpgLabel;
    LabelMiss: TfpgLabel;
    GaugeProgress: TfpgGauge;
    Label3: TfpgLabel;
    Label4: TfpgLabel;
    Label5: TfpgLabel;
    LabelError: TfpgLabel;
    ButtonStart: TfpgButton;
    ButtonAbort: TfpgButton;
    CheckAttach: TfpgCheckBox;
    {@VFD_HEAD_END: FormTestFS}
  public
    procedure AfterCreate; override;
  end;

  TFormRoot = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: FormRoot}
    Label2: TfpgLabel;
    ConnStatus: TfpgLabel;
    PageControl1: TfpgPageControl;
    TabSheetInfo: TfpgTabSheet;
    Label1: TfpgLabel;
    TabSheetDebug: TfpgTabSheet;
    ButtonTestFS: TfpgButton;
    TabSheetConn: TfpgTabSheet;
    {@VFD_HEAD_END: FormRoot}
    FormTestFS: tFormTestFS;
    procedure ClickDebugDownload(Sender: TObject);
    procedure evClose(Sender: TObject;var CloseAction: TCloseAction);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TFormTestFS.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: FormTestFS}
  Name := 'FormTestFS';
  SetPosition(762, 190, 350, 329);
  WindowTitle := 'BrodNet - TestFS';
  Hint := '';
  IconName := '';

  GroupBox1 := TfpgGroupBox.Create(self);
  with GroupBox1 do
  begin
    Name := 'GroupBox1';
    SetPosition(0, 2, 350, 160);
    Anchors := [anLeft,anRight,anTop];
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'FileServer Test Parameters';
  end;

  EditFID := TfpgEdit.Create(GroupBox1);
  with EditFID do
  begin
    Name := 'EditFID';
    SetPosition(10, 32, 330, 24);
    ExtraHint := '';
    FontDesc := 'FreeMono-10:bold:antialias=true';
    Hint := '';
    ParentShowHint := False;
    ShowHint := True;
    TabOrder := 1;
    Text := '9cb70c44932a00dbe74ca392b3694329fa894768';
  end;

  ButtonDelete := TfpgButton.Create(GroupBox1);
  with ButtonDelete do
  begin
    Name := 'ButtonDelete';
    SetPosition(100, 59, 52, 23);
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := 'Delete local file, so it can be downloaded again.';
    ImageName := '';
    ParentShowHint := False;
    ShowHint := True;
    TabOrder := 2;
  end;

  ButtonLookup := TfpgButton.Create(GroupBox1);
  with ButtonLookup do
  begin
    Name := 'ButtonLookup';
    SetPosition(12, 59, 80, 23);
    Text := 'Lookup';
    Enabled := False;
    FontDesc := '#Label1';
    Hint := 'Lookup source in DHT';
    ImageName := '';
    ParentShowHint := False;
    ShowHint := True;
    TabOrder := 3;
  end;

  EditSRC := TfpgEdit.Create(GroupBox1);
  with EditSRC do
  begin
    Name := 'EditSRC';
    SetPosition(12, 100, 284, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := 'From whom to download. In NetAddr format.';
    ParentShowHint := False;
    ShowHint := True;
    TabOrder := 4;
    Text := '//ip4/127.162.32.220/7778';
  end;

  Label1 := TfpgLabel.Create(GroupBox1);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(12, 15, 120, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'FileID:';
  end;

  Label2 := TfpgLabel.Create(GroupBox1);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(12, 83, 80, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Source:';
  end;

  GroupBox2 := TfpgGroupBox.Create(self);
  with GroupBox2 do
  begin
    Name := 'GroupBox2';
    SetPosition(0, 170, 350, 133);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Progress and Results';
  end;

  LabelTotal := TfpgLabel.Create(GroupBox2);
  with LabelTotal do
  begin
    Name := 'LabelTotal';
    SetPosition(80, 20, 80, 15);
    FontDesc := '#Label1';
    Hint := 'Size of the file';
    ParentShowHint := False;
    ShowHint := True;
    Text := '--';
  end;

  LabelRate := TfpgLabel.Create(GroupBox2);
  with LabelRate do
  begin
    Name := 'LabelRate';
    SetPosition(80, 60, 80, 15);
    FontDesc := '#Label1';
    Hint := 'Blocks that need to be redownloaded.';
    ParentShowHint := False;
    ShowHint := True;
    Text := '--';
  end;

  LabelMiss := TfpgLabel.Create(GroupBox2);
  with LabelMiss do
  begin
    Name := 'LabelMiss';
    SetPosition(80, 40, 80, 15);
    FontDesc := '#Label1';
    Hint := 'Speed of the transfer';
    ParentShowHint := False;
    ShowHint := True;
    Text := '--';
  end;

  GaugeProgress := TfpgGauge.Create(GroupBox2);
  with GaugeProgress do
  begin
    Name := 'GaugeProgress';
    SetPosition(16, 100, 320, 25);
    Color := TfpgColor($BFBFBEC4);
    FirstColor := TfpgColor($BF000000);
    Hint := '%Total Bytes';
    ParentShowHint := False;
    Progress := 0;
    SecondColor := TfpgColor($BF434FFF);
    ShowHint := True;
  end;

  Label3 := TfpgLabel.Create(GroupBox2);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(12, 40, 60, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Rate:';
  end;

  Label4 := TfpgLabel.Create(GroupBox2);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(12, 60, 60, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Missed:';
  end;

  Label5 := TfpgLabel.Create(GroupBox2);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(12, 20, 60, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Total:';
  end;

  LabelError := TfpgLabel.Create(GroupBox2);
  with LabelError do
  begin
    Name := 'LabelError';
    SetPosition(180, 20, 80, 50);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'ok';
  end;

  ButtonStart := TfpgButton.Create(GroupBox1);
  with ButtonStart do
  begin
    Name := 'ButtonStart';
    SetPosition(12, 130, 80, 23);
    Text := 'Start';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
  end;

  ButtonAbort := TfpgButton.Create(GroupBox1);
  with ButtonAbort do
  begin
    Name := 'ButtonAbort';
    SetPosition(100, 130, 80, 23);
    Text := 'Abort';
    Enabled := False;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 8;
  end;

  CheckAttach := TfpgCheckBox.Create(GroupBox1);
  with CheckAttach do
  begin
    Name := 'CheckAttach';
    SetPosition(163, 60, 76, 19);
    BoxLayout := tbRightBox;
    Enabled := False;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 10;
    Text := 'Attached:';
  end;

  {@VFD_BODY_END: FormTestFS}
  {%endregion}
end;


procedure TFormRoot.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: FormRoot}
  Name := 'FormRoot';
  SetPosition(334, 209, 329, 292);
  WindowTitle := 'BrodNet - root';
  Hint := '';
  IconName := '';
  OnClose:=@evClose;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 271, 88, 20);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Connection: ';
  end;

  ConnStatus := TfpgLabel.Create(self);
  with ConnStatus do
  begin
    Name := 'ConnStatus';
    SetPosition(104, 272, 80, 15);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := '---';
  end;

  PageControl1 := TfpgPageControl.Create(self);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(0, 0, 331, 268);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    TabOrder := 0;
  end;

  TabSheetInfo := TfpgTabSheet.Create(PageControl1);
  with TabSheetInfo do
  begin
    Name := 'TabSheetInfo';
    SetPosition(3, 24, 325, 241);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Main';
  end;

  Label1 := TfpgLabel.Create(TabSheetInfo);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(24, 16, 140, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Welcome to Brodnet.';
  end;

  TabSheetDebug := TfpgTabSheet.Create(PageControl1);
  with TabSheetDebug do
  begin
    Name := 'TabSheetDebug';
    SetPosition(3, 24, 325, 241);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Debug/Testing';
  end;

  ButtonTestFS := TfpgButton.Create(TabSheetDebug);
  with ButtonTestFS do
  begin
    Name := 'ButtonTestFS';
    SetPosition(10, 10, 80, 30);
    Text := 'Download';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick:=@ClickDebugDownload;
  end;

  TabSheetConn := TfpgTabSheet.Create(PageControl1);
  with TabSheetConn do
  begin
    Name := 'TabSheetConn';
    SetPosition(3, 24, 325, 241);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Daemon';
  end;

  {@VFD_BODY_END: FormRoot}
  {%endregion}
  FormTestFS := tFormTestFS.Create(fpgApplication);
end;

procedure tFormRoot.evClose(Sender: TObject;var CloseAction: TCloseAction);
begin
end;

procedure tFormRoot.ClickDebugDownload(Sender: TObject);
 begin
 FormTestFS.Show;
end;

 
end.
