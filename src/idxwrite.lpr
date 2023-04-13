program idxwrite;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, progress, uintlist, charsutil, main, xmlutils, uidxstruct, usrfstruct,
  uafsstruct, uidxcreation, uidxtemplatecreation, s1idx_intf, s2idx, s2idx_intf;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

