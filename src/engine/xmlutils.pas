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
  currentNode: TDOMNode;
  oldIdx, oldAfs, newAfs, newIdx, game: String;
begin
  if FileExists(FileName) then begin
    ReadXMLFile(xmlDoc, FileName);
    if xmlDoc.DocumentElement.NodeName <> 'idxcreatorcfg' then Exit;

    try
      try
        currentNode := xmlDoc.DocumentElement.FindNode('remember');
        if Assigned(currentNode) then frmMain.cbConfig.Checked := StrToBool(currentNode.FirstChild.NodeValue);
      except end;

      if (Assigned(currentNode)) and (currentNode.FirstChild.NodeValue = 'true') then begin
        //Template groupbox
        currentNode := xmlDoc.DocumentElement.FindNode('template');
        if Assigned(currentNode) then frmMain.templateChkBox.Checked := StrToBool(currentNode.FirstChild.NodeValue);

        oldIdx := xmlDoc.DocumentElement.FindNode('oldidx').FirstChild.NodeValue;
        oldAfs := xmlDoc.DocumentElement.FindNode('oldafs').FirstChild.NodeValue;
        newAfs := xmlDoc.DocumentElement.FindNode('newafs').FirstChild.NodeValue;
        newIdx := xmlDoc.DocumentElement.FindNode('newidx').FirstChild.NodeValue;
        game := xmlDoc.DocumentElement.FindNode('game').FirstChild.NodeValue;

        frmMain.editOldIdx.Text := oldIdx;
        frmMain.editOldAfs.Text := oldAfs;
        frmMain.editModAfs.Text := newAfs;
        frmMain.editNewIdx.Text := newIdx;

        if game = 's2' then
          frmMain.GameVersion := gvShenmue2;
        else
          frmMain.GameVersion := gvShenmue;
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
    AddTextNode(xmlRoot, 'oldIdx', frmMain.editOldIdx.Text);
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

