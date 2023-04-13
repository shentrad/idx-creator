unit s1idx_intf;

{$mode objfpc}{$H+}

interface

uses
  System, SysUtils, uidxtemplatecreation, uidxcreation;

procedure CreateShenmue1Idx(const ModifiedAFS, OutputIDX: String); overload;
procedure CreateShenmue1Idx(const ModifiedAFS, OutputIDX, TemplateAFS, TemplateIDX: String); overload;

implementation

function StartTemplateCreation(const ModifiedAFS, OutputIDX, TemplateAFS, TemplateIDX: String): Boolean;
var
  idxThread: TIdxTemplateCreation;
begin
  Result := False;
  idxThread := TIdxTemplateCreation.Create(OutputIDX, ModifiedAFS, TemplateIDX, TemplateAFS);
  repeat
    sleep(10);
  until (idxThread.ThreadTerminated);

  //If there's no error...
  if not idxThread.ErrorRaised then begin
    Result := True;
  end;

  idxThread.Free;
end;

function StartCreation(const ModifiedAFS, OutputIDX: String): Boolean;
var
  idxThread: TIdxCreation;
begin
  Result := False;
  idxThread := TIdxCreation.Create(ModifiedAFS, OutputIDX);

  repeat
    Sleep(10);
  until (idxThread.ThreadTerminated);

  //If there's no error...
  if not idxThread.ErrorRaised then begin
    Result := True;
  end;

  idxThread.Free;
end;

procedure CreateShenmue1Idx(const ModifiedAFS, OutputIDX: String);
begin
  CreateShenmue1Idx(ModifiedAFS, OutputIDX, '', '');
end;

procedure CreateShenmue1Idx(const ModifiedAFS, OutputIDX, TemplateAFS, TemplateIDX: String);
var
  thOK: Boolean;
begin
  //Starting creation...
  WriteLn('[i] Starting creation...');
  if (TemplateAFS <> '') and (TemplateIDX <> '') then begin
    thOK := StartTemplateCreation(ModifiedAFS, OutputIDX, TemplateAFS, TemplateIDX);
  end
  else begin
    thOK := StartCreation(ModifiedAFS, OutputIDX);
  end;

  if thOK then begin
    WriteLn('[i] Creation completed for '+ExtractFileName(OutputIDX)+' !');
  end
  else begin
    WriteLn('[!] "'+ExtractFileName(ModifiedAFS)+'" is not a valid Shenmue I AFS file. IDX creation stopped.');
  end;
end;

end.

