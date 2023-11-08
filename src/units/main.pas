unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls;

const
  APP_VERSION = '2.3';
  COMPIL_DATE_TIME = 'December 01, 2023 @ 05:41 PM';

type

  { TfrmMain }
  TGameVersion = (gvShenmue, gvShenmue2);

  TfrmMain = class(TForm)
    btBrowseOldAfs: TButton;
    btSaveNewIdx: TButton;
    btBrowseOldIdx: TButton;
    btBrowseModAfs: TButton;
    btGo: TButton;
    cbConfig: TCheckBox;
    editNewIdx: TEdit;
    editOldIdx: TEdit;
    editOldAfs: TEdit;
    editModAfs: TEdit;
    GroupBox2: TGroupBox;
    lblOldAfs: TLabel;
    lblNewIdx: TLabel;
    lblOldIdx: TLabel;
    lblModAfs: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    templateChkBox: TCheckBox;
    GroupBox1: TGroupBox;
    rgGame: TRadioGroup;
    procedure btBrowseModAfsClick(Sender: TObject);
    procedure btBrowseOldAfsClick(Sender: TObject);
    procedure btBrowseOldIdxClick(Sender: TObject);
    procedure btGoClick(Sender: TObject);
    procedure btSaveNewIdxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgGameClick(Sender: TObject);
    procedure templateChkBoxClick(Sender: TObject);
  private
    fOldAfsName, fOldIdxName, fModAfsName, fNewIdxName: TFileName;
    fGameVersion: TGameVersion;
    procedure GroupBoxActivation(const Activated: Boolean);
    procedure TemplateActivation(const Activated: Boolean);
    function VerifyFiles(const UseTemplate: Boolean): Boolean;
    function StartTemplateCreation: Boolean;
    function StartCreation: Boolean;
    procedure QueueIdxCreation(const UseTemplate: Boolean);
    procedure SetGameVersion(const Value: TGameVersion);
  public
    procedure StatusChange(const StatusText: String);
    function MsgBox(CustomMessage, CustomCaption: String; Flags: Integer): Integer;
    property GameVersion: TGameVersion read fGameVersion write SetGameVersion;
  end;

var
  frmMain: TfrmMain;

implementation
uses uidxcreation, uidxtemplatecreation, xmlutils, s2idx_intf;

{$R *.lfm}

procedure TfrmMain.btBrowseOldIdxClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'IDX file (*.idx)|*.idx';
  OpenDialog1.Title := 'Open original IDX...';
  if OpenDialog1.Execute then
  begin
    editOldIdx.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmMain.btGoClick(Sender: TObject);
begin
  fOldIdxName := editOldIdx.Text;
  fOldAfsName := editOldAfs.Text;
  fModAfsName := editModAfs.Text;
  fNewIdxName := editNewIdx.Text;

  case GameVersion of
    gvShenmue: QueueIdxCreation(templateChkBox.Checked);
    gvShenmue2: CreateShenmue2Idx(fModAfsName, fNewIdxName);
  end;
end;

procedure TfrmMain.btSaveNewIdxClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'IDX file (*.idx)|*.idx';
  SaveDialog1.Title := 'Save new IDX to...';

  if editNewIdx.Text <> '' then
  begin
    if DirectoryExists(ExtractFilePath(editNewIdx.Text)) then begin
      SaveDialog1.InitialDir := ExtractFilePath(editNewIdx.Text);
      SaveDialog1.FileName := ExtractFileName(editNewIdx.Text);
    end;
  end;

  if SaveDialog1.Execute then
  begin
    editNewIdx.Text := SaveDialog1.FileName;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  binPath: String;
begin
  TemplateActivation(False);
  Caption := 'IDX Creator v'+APP_VERSION;

  binPath := ExtractFilePath(Application.ExeName);
  {$IFNDEF DARWIN}LoadConfig(binPath+'config.xml');{$ENDIF}
  {$IFDEF DARWIN}LoadConfig(LeftStr(binPath, Pos('idxwrite.app', binPath)-1)+'config.xml'){$ENDIF}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  binPath: String;
begin
  if cbConfig.Checked then begin
    binPath := ExtractFilePath(Application.ExeName);
    {$IFNDEF DARWIN}SaveConfig(binPath+'config.xml');{$ENDIF}
    {$IFDEF DARWIN}SaveConfig(LeftStr(binPath, Pos('idxwrite.app', binPath)-1)+'config.xml'){$ENDIF}
  end;
end;

procedure TfrmMain.rgGameClick(Sender: TObject);
begin
  case rgGame.ItemIndex of
    0: GameVersion := gvShenmue;
    1: GameVersion := gvShenmue2;
  end;
end;

procedure TfrmMain.templateChkBoxClick(Sender: TObject);
begin
  with templateChkBox do begin
      TemplateActivation(Checked);
  end;
end;

procedure TfrmMain.btBrowseOldAfsClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'AFS file (*.afs)|*.afs';
  OpenDialog1.Title := 'Open original AFS...';
  if OpenDialog1.Execute then begin
    editOldAfs.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmMain.btBrowseModAfsClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'AFS file (*.afs)|*.afs';
  OpenDialog1.Title := 'Open modified AFS...';
  if OpenDialog1.Execute then begin
    editModAfs.Text := OpenDialog1.FileName;
    if editNewIdx.Text = '' then
    begin
      editNewIdx.Text := ChangeFileExt(OpenDialog1.FileName, '.IDX');
      try
        editNewIdx.SelectAll;
        editNewIdx.SetFocus;
      except
      end;
    end;
  end;
end;

procedure TfrmMain.GroupBoxActivation(const Activated: Boolean);
begin
  GroupBox1.Enabled := True;
  GroupBox2.Enabled := True;
end;

procedure TfrmMain.TemplateActivation(const Activated: Boolean);
begin
  lblOldAfs.Enabled := Activated;
  lblOldIdx.Enabled := Activated;
  editOldAfs.Enabled := Activated;
  editOldIdx.Enabled := Activated;
  btBrowseOldAfs.Enabled := Activated;
  btBrowseOldIdx.Enabled := Activated;
end;

function TfrmMain.VerifyFiles(const UseTemplate: Boolean): Boolean;
begin
  Result := False;
  if (fModAfsName <> '') and (fNewIdxName <> '') then
  begin
    if (UseTemplate) and (fOldIdxName <> '') and (fOldAfsName <> '') then
    begin
      Result := True;
    end
    else if not UseTemplate then
    begin
      Result := True;
    end
    else begin
      Result := False;
    end;
  end;
end;

function TfrmMain.StartTemplateCreation: Boolean;
var
  idxThread: TIdxTemplateCreation;
begin
  Result := False;
  idxThread := TIdxTemplateCreation.Create(fNewIdxName, fModAfsName, fOldIdxName, fOldAfsName);

  repeat
    Application.ProcessMessages;
  until (idxThread.ThreadTerminated);

  //If no error...
  if not idxThread.ErrorRaised then
  begin
    Result := True;
  end;

  idxThread.Free;
end;

function TfrmMain.StartCreation: Boolean;
var
  idxThread: TIdxCreation;
begin
  Result := False;
  idxThread := TIdxCreation.Create(fModAfsName, fNewIdxName);

  repeat
    Application.ProcessMessages;
  until (idxThread.ThreadTerminated);

  if not idxThread.ErrorRaised then
  begin
    Result := True;
  end;

  idxThread.Free;
end;

procedure TfrmMain.QueueIdxCreation(const UseTemplate: Boolean);
var
  threadCompleted: Boolean;
begin
  if VerifyFiles(UseTemplate) then
  begin
    GroupBoxActivation(False);

    //Starting creation
    StatusChange('Starting creation...');
    if UseTemplate then
    begin
      threadCompleted := StartTemplateCreation;
    end
    else begin
      threadCompleted := StartCreation;
    end;

    //Modifying form
    if threadCompleted then
    begin
      StatusChange('Creation completed for '+ExtractFileName(fNewIdxName)+'!');
    end
    else begin
      StatusChange('"'+ExtractFileName(fModAfsName)+'" is not a valid Shenmue I AFS file. IDX creation stopped...');
    end;
    GroupBoxActivation(True);
  end
  else begin
    StatusChange('Error: input or ouput file not defined properly...');
  end;
end;

procedure TfrmMain.SetGameVersion(const Value: TGameVersion);
begin
  fGameVersion := Value;

  Self.rgGame.ItemIndex := Integer(GameVersion);
  case GameVersion of
    gvShenmue: begin
        Self.templateChkBox.Enabled := True;
        if Self.templateChkBox.Checked then
        begin
          TemplateActivation(True);
        end;
    end;
    gvShenmue2: begin
        Self.templateChkBox.Enabled := False;
        TemplateActivation(False);
    end;
  end;
end;

procedure TfrmMain.StatusChange(const StatusText: String);
begin
  StatusBar1.Panels[0].Text := StatusText;
end;

function TfrmMain.MsgBox(CustomMessage, CustomCaption: String; Flags: Integer): Integer;
begin
  Result := Application.MessageBox(PChar(CustomMessage), PChar(CustomCaption), Flags);
end;

end.

