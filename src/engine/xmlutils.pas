unit xmlutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure LoadConfig(const FileName: TFileName);
procedure SaveConfig(const FileName: TFileName);

implementation
uses laz2_DOM, laz2_XMLWrite, laz2_XMLRead, main;

procedure LoadConfig(const FileName: TFileName);
var
  xmlDoc: TXMLDocument;
  autoSaveBool, templateBool, oldIdx, oldAfs, newAfs, newIdx, game, wWidth: TDOMNode;
begin
  if FileExists(FileName) then begin
    ReadXMLFile(xmlDoc, FileName);
    if xmlDoc.DocumentElement.NodeName <> 'idxcreatorcfg' then Exit;
    try
      autoSaveBool := xmlDoc.DocumentElement.FindNode('remember');

      if (Assigned(autoSaveBool)) and (autoSaveBool.TextContent = '-1') then begin
        //Auto-save config checkbox
        frmMain.cbConfig.Checked := StrToBool(autoSaveBool.TextContent);

        //Fetching nodes content
        templateBool := xmlDoc.DocumentElement.FindNode('template');
        wWidth := xmlDoc.DocumentElement.FindNode('width');
        oldIdx := xmlDoc.DocumentElement.FindNode('oldidx');
        oldAfs := xmlDoc.DocumentElement.FindNode('oldafs');
        newAfs := xmlDoc.DocumentElement.FindNode('newafs');
        newIdx := xmlDoc.DocumentElement.FindNode('newidx');
        game := xmlDoc.DocumentElement.FindNode('game');

        if Assigned(templateBool) then frmMain.templateChkBox.Checked := StrToBool(templateBool.TextContent);
        if Assigned(wWidth) then frmMain.Width := StrToInt(wWidth.TextContent);
        if Assigned(oldIdx) then frmMain.editOldIdx.Text := oldIdx.TextContent;
        if Assigned(oldAfs) then frmMain.editOldAfs.Text := oldAfs.TextContent;
        if Assigned(newAfs) then frmMain.editModAfs.Text := newAfs.TextContent;
        if Assigned(newIdx) then frmMain.editNewIdx.Text := newIdx.TextContent;

        if Assigned(game) then begin
          case game.TextContent of
            's1': frmMain.GameVersion := gvShenmue;
            's2': frmMain.GameVersion := gvShenmue2;
          end;
        end;
      end;
    finally
      xmlDoc.Free;
    end;
  end
  else begin
    SaveConfig(FileName);
  end;
end;

procedure SaveConfig(const FileName: TFileName);
var
  xmlDoc: TXMLDocument;
  xmlRoot, xmlChild, xmlText: TDOMNode;

  procedure AddTextNode(RootNode: TDOMNode; NodeName: String; NodeValue: String);
  begin
    xmlChild := xmlDoc.CreateElement(NodeName);
    xmlText := xmlDoc.CreateTextNode(NodeValue);
    xmlChild.AppendChild(xmlText);
    RootNode.AppendChild(xmlChild);
  end;

begin
  xmlDoc := TXMLDocument.Create;

  try
    //Creating the root node
    xmlRoot := xmlDoc.CreateElement('idxcreatorcfg');
    xmlDoc.AppendChild(xmlRoot);
    xmlRoot := xmlDoc.DocumentElement;

    //Writing config values
    AddTextNode(xmlRoot, 'remember', BoolToStr(frmMain.cbConfig.Checked));
    AddTextNode(xmlRoot, 'template', BoolToStr(frmMain.templateChkBox.Checked));
    AddTextNode(xmlRoot, 'width', IntToStr(frmMain.Width));
    AddTextNode(xmlRoot, 'oldidx', frmMain.editOldIdx.Text);
    AddTextNode(xmlRoot, 'oldafs', frmMain.editOldAfs.Text);
    AddTextNode(xmlRoot, 'newafs', frmMain.editModAfs.Text);
    AddTextNode(xmlRoot, 'newidx', frmMain.editNewIdx.Text);

    case frmMain.GameVersion of
      gvShenmue: AddTextNode(xmlRoot, 'game', 's1');
      gvShenmue2: AddTextNode(xmlRoot, 'game', 's2');
    end;

    WriteXMLFile(xmlDoc, FileName);
  finally
    xmlDoc.Free;
  end;
end;

end.

