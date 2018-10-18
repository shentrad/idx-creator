unit uidxcreation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, uafsstruct, uidxstruct, usrfstruct{$IFDEF LCL}, Forms, progress{$ENDIF};

type
  TIdxCreation = class(TThread)
  private
    fModAfsName: TFileName;
    fNewIdxName: TFileName;
    fProgressCount: Integer; //Used to track progress
    {$IFDEF LCL}
    fCurrentTask: String;
    fProgressWindow: TfrmProgress;
    {$ENDIF}
    procedure CreateIdx(var AfsStruct: TAfsStruct; var IdxStruct: TIdxStruct; var SrfStruct: TSrfStruct; const Reversed: Boolean);
    function VerifyOrder(var AfsStruct: TAfsStruct): Boolean;
    function DeleteSubStr(Str, SubStr: String): String;
    procedure SyncCurrentTasl(const Task: String);
    {$IFDEF LCL}
    procedure SyncPercentage;
    procedure SyncDefaultFormValue;
    procedure UpdatePercentage;
    procedure UpdateCurrentTask;
    procedure UpdateDefaultFormValue;
    procedure CancelBtnClick(Sender: TObject);
    {$ENDIF}
    procedure CloseThread(Sender: TObject);
  protected
    procedure Execute; override;
  public
    ThreadTerminated: Boolean;
    ErrorRaised: Boolean;
    constructor Create(const AfsFileName, IdxFileName: TFileName);
  end;


implementation

end.

