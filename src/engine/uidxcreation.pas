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
    procedure SyncCurrentTask(const Task: String);
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

constructor TIdxCreation.Create(const AfsFileName, IdxFileName: TFileName);
begin
  inherited Create(False);

  fModAfsName := AfsFileName;
  fNewIdxName := IdxFileName;
  {$IFDEF LCL}fProgressWindow := TfrmProgress.Create(nil);{$ENDIF}

  ThreadTerminated := False;
  ErrorRaised := False;
  OnTerminate := @CloseThread;
end;

procedure TIdxCreation.Execute;
var
  newAfs: TAfsStruct;
  newIdx: TIdxStruct;
  newSrf: TSrfStruct;
  reversedOrder: Boolean;
begin
  try
    try
      //Creating instance and loading files if needed
      newAfs := TAfsStruct.Create;
      newAfs.LoadFromFile(fModAfsName);
      newIdx := TIdxStruct.Create;
      newSrf := TSrfStruct.Create;

      fProgressCount := newAfs.Count;
      if fProgressCount > 0 then begin
        {$IFDEF LCL}SyncDefaultFormValue;{$ENDIF}

        //Verifying file order
        reversedOrder := VerifyOrder(newAfs);
        CreateIdx(newAfs, newIdx, newSrf, reversedOrder);

        if not Terminated then begin
          SyncCurrentTask('Saving new IDX...');
          newIdx.SaveToFile(fNewIdxName);
        end;
      end
      else begin
        ErrorRaised := True;
        Terminate;
      end;
    except
      ErrorRaised := True;
    end;
  finally
    newAfs.Free;
    newIdx.Free;
    newSrf.Free;
  end;

  {$IFDEF LCL}CloseThread(Self);{$ENDIF}
end;

procedure TIdxCreation.CreateIdx(var AfsStruct: TAfsStruct; var IdxStruct: TIdxStruct; var SrfStruct: TSrfStruct; const Reversed: Boolean);
var
  idxEntry: TIdxEntry;
  afsEntry: TAfsEntry;
  i, j, srfNum, subCnt, subOffset, startFile, endFile: Integer;
  strBuf: String;
begin
  //Default values
  startFile := 0;
  endFile := -1;
  srfNum := 0;
  subCnt := 0;

  if Reversed then begin
    Inc(startFile);
  end;

  //Adding entries until a SRF is found
  for i:=0 to AfsStruct.Count-1 do begin
    if Terminated then begin
      Break;
    end;

    //Verifying if it's a SRF
    afsEntry := AfsStruct[i];
    strBuf := ExtractFileExt(afsEntry.FileName);
    if LowerCase(strBuf) <> '.srf' then begin
      //Adding a new entry to the IDX
      SyncCurrentTask('Adding IDX entry... file #'+IntToStr(i));
      idxEntry := TIdxEntry.Create;
      idxEntry.Name := DeleteSubStr(afsEntry.FileName, ExtractFileExt(afsEntry.FileName));
      idxEntry.FileNumber := i;

      idxEntry.LinkedSrf := 0;
      idxEntry.SubOffset := 0;
      if not Reversed then begin
        Inc(endFile);
      end
      else begin
        if not subCnt > SrfStruct.Count then begin
          idxEntry.LinkedSrf := srfNum;
          idxEntry.SubOffset := SrfStruct[subCnt].Offset;
          Inc(subCnt);
        end
        else begin
            ErrorRaised := True;
            Terminate;
        end;
      end;

      IdxStruct.Add(idxEntry);
    end
    else begin
      SrfStruct.LoadFromFile(fModAfsName, afsEntry.Offset, afsEntry.Size);
      IdxStruct.SrfCount := IdxStruct.SrfCount+1;
      SyncCurrentTask('Parsing SRF... file #'+IntToStr(i));

      //Parsing SRF to find subtitles offset
      if not Reversed then begin
        for j:=startFile to endFile do begin
          //Modifying IDX entry;
            idxEntry := idxStruct[i-j];
            idxEntry.LinkedSrf := i;
            subOffset := SrfStruct[j-startFile].Offset;
            idxEntry.SubOffset := subOffset;
        end;
        startFile := i+1;
      end
      else begin
          srfNum := i;
          subCnt := 0;
      end;
    end;
    {$IFDEF LCL}SyncPercentage;{$ENDIF}
  end;
end;

function TIdxCreation.VerifyOrder(var AfsStruct: TAfsStruct): Boolean;
var
  strBuf: String;
begin
  Result := False;
  strBuf := ExtractFileExt(AfsStruct[0].FileName);
  if LowerCase(strBuf) = '.srf' then begin
    Result := True;
  end;
end;

function TIdxCreation.DeleteSubStr(Str, SubStr: String): String;
begin
  if Pos(SubStr, Str) <> 0 then begin
    Delete(Str, Pos(SubStr, Str), Length(SubStr));
    Result := Str;
  end
  else begin
    Result := Str;
  end;
end;

procedure TIdxCreation.SyncCurrentTask(const Task: string);
begin
  {$IFDEF LCL}
  fCurrentTask := Task;
  Synchronize(@UpdateCurrentTask);
  {$ELSE}
  WriteLn('[i]', Task);
  {$ENDIF}
end;

{$IFDEF LCL}

procedure TIdxCreation.SyncPercentage;
begin
  Synchronize(@UpdatePercentage);
end;

procedure TIdxCreation.SyncDefaultFormValue;
begin
  Synchronize(@UpdateDefaultFormValue);
end;

procedure TIdxCreation.UpdatePercentage;
var
  i: Integer;
  floatBuf: Real;
begin
  i := fProgressWindow.ProgressBar1.Position;
  fProgressWindow.ProgressBar1.Position := i+1;
  floatBuf := SimpleRoundTo((100*(i+1))/fProgressCount, -2);
  fProgressWindow.Panel1.Caption := FloatToStr(floatBuf)+'%';
  Application.ProcessMessages;
end;

procedure TIdxCreation.UpdateCurrentTask;
begin
  fProgressWindow.lblCurrentTask.Caption := 'Current task: '+fCurrentTask;
end;

procedure TIdxCreation.UpdateDefaultFormValue;
begin
  //Setting progress form main value
  fProgressWindow.Caption := 'Creation in progress... '+ExtractFileName(fNewIdxName);
  fProgressWindow.lblCurrentTask.Caption := 'Current task:';
  fProgressWindow.Position := poMainFormCenter;
  fProgressWindow.ProgressBar1.Max := fProgressCount;
  fProgressWindow.btCancel.OnClick := @CancelBtnClick;
  fProgressWindow.Panel1.Caption := '0%';
  fProgressWindow.Show;
end;

procedure TIdxCreation.CancelBtnClick(Sender: TObject);
begin
  Terminate;
end;

{$ENDIF}

procedure TIdxCreation.CloseThread(Sender: TObject);
begin
  {$IFDEF LCL}
  if Assigned(fProgressWindow) then begin
    fProgressWindow.Release;
  end;
  {$ENDIF}
  ThreadTerminated := True;
end;

end.

