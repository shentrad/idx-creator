unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls;

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
  private
    fOldAfsName, fOldIdxName, fModAfsName, fNewIdxName: TFileName;
    fGameVersion: TGameVersion;
  public
    property GameVersion: TGameVersion read fGameVersion write SetGameVersion;
  end;

var
  frmMain: TfrmMain;

implementation
uses {uidxcreation, uidxtemplatecreation, }xmlutils{, S2IDX_INTF};

{$R *.lfm}

end.

