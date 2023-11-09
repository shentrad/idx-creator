unit s2idx_intf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function CreateShenmue2Idx(const ModifiedAfs, OutputIdx: String): Boolean;

implementation
uses {$IFDEF LCL}main, Forms, Math, progress,{$ENDIF} s2idx;

{$IFDEF LCL}
var
  fProgressWindow: TfrmProgress;

procedure S2Idx_OnStart(const MaxFiles: Integer);
begin
  fProgressWindow := TfrmProgress.Create(nil);

  //Setting progress form main values
  fProgressWindow.Caption := 'Creation in progress...';
  fProgressWindow.lblCurrentTask.Caption := 'Current task:';
  fProgressWindow.Position := poMainFormCenter;
  fProgressWindow.ProgressBar1.Max := MaxFiles;
  fProgressWindow.btCancel.Enabled := False;
  fprogressWindow.Panel1.Caption := '0%';
  fProgressWindow.Show;
end;

procedure S2Idx_OnStatus(const Message: String);
begin
  frmMain.StatusChange(Message)
end;

procedure S2Idx_OnProgress();
var
  i: Integer;
  floatBuf: Real;
begin
  i := fProgressWindow.ProgressBar1.Position;
  fProgressWindow.ProgressBar1.Position := i+1;
  floatBuf := SimpleRoundTo((100*(i+1))/fProgressWindow.ProgressBar1.Max, -2);
  fProgressWindow.Panel1.Caption := FloatToStr(floatBuf)+'%';
  Application.ProcessMessages;
end;

procedure S2Idx_OnCompleted();
begin
  if Assigned(fProgressWindow) then
  begin
    fProgressWindow.Release;
  end;
end;

{$ELSE}

procedure S2IDX_OnStart(const MaxFiles: Integer);
begin

end;

procedure S2IDX_OnStatus(const Message: string);
begin
  WriteLn('[i] ', Message);
end;

procedure S2IDX_OnProgress();
begin

end;

procedure S2IDX_OnCompleted();
begin

end;

{$ENDIF}

function CreateShenmue2Idx(const ModifiedAfs, OutputIdx: String): Boolean;
var
  S2IdxCreator: TS2IdxCreator;
begin
  Result := False;
  S2IdxCreator := TS2IdxCreator.Create;
  try
    S2IdxCreator.OnStart := @S2Idx_OnStart;
    S2IdxCreator.OnStatus :=  @S2Idx_OnStatus;
    S2IdxCreator.OnProgress := @S2Idx_OnProgress;
    S2IdxCreator.OnCompleted := @S2Idx_OnCompleted;
    try
      Result := S2IdxCreator.MakeIdx(ModifiedAfs, OutputIdx);
    except
      Result := False;
    end;
  finally
    S2IdxCreator.Free;
    if not Result then if FileExists(OutputIdx) then DeleteFile(OutputIdx);
  end;
end;

end.

