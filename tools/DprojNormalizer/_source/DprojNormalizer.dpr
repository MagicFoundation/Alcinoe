program DprojNormalizer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ALXmlDoc,
  ALCommon,
  ALStringList,
  ALFiles,
  ALString;

{***********************************************************}
Procedure _SortAttributesByNodeName(const aNode: TalXmlNode);
var i: integer;
Begin
  if ANode = nil then exit;
  if ANode.AttributeNodes <> nil then
    ANode.AttributeNodes.CustomSort(
      function(List: TALXMLNodeList; Index1, Index2: Integer): Integer
      begin
        result := ALCompareStr(List[index1].NodeName, List[index2].NodeName);
      end);
  //-----
  if aNode.ChildNodes <> nil then
    for i := 0 to aNode.ChildNodes.Count - 1 do
      _SortAttributesByNodeName(aNode.ChildNodes[i]);
End;

{***************************************************************}
Procedure _SortNodesByNameAndAttributes(const aNode: TalXmlNode);
var LXmlStr1, LXmlStr2: AnsiString;
    i: integer;
Begin
  if ANode = nil then exit;
  if ANode.ChildNodes <> nil then
    ANode.ChildNodes.CustomSort(
      function(List: TALXMLNodeList; Index1, Index2: Integer): Integer
      begin
        result := ALCompareStr(List[index1].NodeName, List[index2].NodeName);
        if (result = 0) then begin
          List[index1].SaveToXML(LXmlStr1);  // << yes I know it's ugly
          List[index2].SaveToXML(LXmlStr2);  // << but I m lazzy
          result := ALCompareStr(LXmlStr1, LXmlStr2);
        end;
      end);
  //-----
  if aNode.ChildNodes <> nil then
    for i := 0 to aNode.ChildNodes.Count - 1 do
      _SortNodesByNameAndAttributes(aNode.ChildNodes[i]);
End;

{*******************************************************************}
Procedure _RemoveEmptyProjectExtensionsNode(const aNode: TalXmlNode);
var i: integer;
Begin
  if ANode = nil then exit;
  if aNode.ChildNodes <> nil then
    for i := aNode.ChildNodes.Count - 1 downto 0 do
      if (aNode.ChildNodes[i].NodeName = 'ProjectExtensions') and
         ((aNode.ChildNodes[i].ChildNodes = nil) or
          (aNode.ChildNodes[i].ChildNodes.Count = 0)) then
        aNode.ChildNodes.Delete(i);
End;

{***************************************************}
Procedure _RemoveCDataNodes(const aNode: TalXmlNode);
var LValue: AnsiString;
    LParentNode: TalXmlNode;
    i: integer;
Begin
  if ANode = nil then exit;
  if aNode.NodeType = ntCData then begin
    LValue := aNode.Text;
    LParentNode := ANode.ParentNode;
    if LParentNode.ChildNodes.Count <> 1 then
      raise Exception.Create('Error 2F418A94-9F42-4418-90D7-ED8EC120D1B0');
    LParentNode.ChildNodes.Clear;
    LPArentNode.Text := LValue;
  end;
  //-----
  if aNode.ChildNodes <> nil then
    for i := 0 to aNode.ChildNodes.Count - 1 do
      _RemoveCDataNodes(aNode.ChildNodes[i]);
End;

var
  LDProjFilename: AnsiString;
  LCreateBackup: Boolean;
  LXmlDoc: TalXmlDocument;
  LXmlStr: AnsiString;
  LItemGroupNode: TalXmlNode;
  LProjectExtensionsNode: TalXmlNode;
  LDeploymentNode: TalXmlNode;
  LBorlandProjectNode: TalXmlNode;
  LDeployFileNode: TalXmlNode;
  LDelphiCompileNodes: Tarray<TalXmlNode>;
  LDelphiCompileNode: TalXmlNode;
  LBuildConfigurationNodes: Tarray<TalXmlNode>;
  LBuildConfigurationNode: TalXmlNode;
  LProjectRootNodes: Tarray<TalXmlNode>;
  LProjectRootNode: TalXmlNode;
  LEnabledNode: TalXmlNode;
  I, j: integer;

begin

  try

    //Init project params 
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    //init LDProjFilename / LCreateBackup
    LDProjFilename := ALTrim(ansiString(paramstr(1)));
    LCreateBackup := not ALSameText(ALTrim(ansiString(paramstr(2))), 'false');

    //create the LXmlDoc
    LXmlDoc := TALXmlDocument.Create('root');
    Try

      //load the LXmlDoc
      LXmlDoc.Options := [];
      LXmlDoc.ParseOptions := [];
      LXmlDoc.LoadFromFile(LDProjFilename);

      //init LProjectExtensionsNode
      LProjectExtensionsNode := LXmlDoc.DocumentElement.ChildNodes.FindNode('ProjectExtensions');

      //init LBorlandProjectNode
      LBorlandProjectNode := nil;
      if LProjectExtensionsNode <> nil then LBorlandProjectNode := LProjectExtensionsNode.ChildNodes.FindNode('BorlandProject');

      //init LDeploymentNode
      LDeploymentNode := nil;
      if LBorlandProjectNode <> nil then LDeploymentNode := LBorlandProjectNode.ChildNodes.FindNode('Deployment');

      //init LItemGroupNode
      LItemGroupNode := LXmlDoc.DocumentElement.ChildNodes.FindNode('ItemGroup');

      //order ItemGroup
      _SortAttributesByNodeName(LItemGroupNode);
      _SortNodesByNameAndAttributes(LItemGroupNode);

      //put DelphiCompile at the top (don't know if it's matter)
      if LItemGroupNode <> nil then begin
        setlength(LDelphiCompileNodes, 0);
        while True do begin
          LDelphiCompileNode := LItemGroupNode.ChildNodes.FindNode('DelphiCompile');
          if LDelphiCompileNode <> nil then begin
            Setlength(LDelphiCompileNodes, length(LDelphiCompileNodes)+1);
            LDelphiCompileNodes[length(LDelphiCompileNodes) - 1] := LItemGroupNode.ChildNodes.Extract(LDelphiCompileNode);
          end
          else break;
        end;
        for I := High(LDelphiCompileNodes) downto Low(LDelphiCompileNodes) do
          LItemGroupNode.ChildNodes.insert(0, LDelphiCompileNodes[i]);
      end;

      //put BuildConfiguration at the end (don't know if it's matter)
      if LItemGroupNode <> nil then begin
        setlength(LBuildConfigurationNodes, 0);
        while True do begin
          LBuildConfigurationNode := LItemGroupNode.ChildNodes.FindNode('BuildConfiguration');
          if LBuildConfigurationNode <> nil then begin
            Setlength(LBuildConfigurationNodes, length(LBuildConfigurationNodes)+1);
            LBuildConfigurationNodes[length(LBuildConfigurationNodes) - 1] := LItemGroupNode.ChildNodes.Extract(LBuildConfigurationNode);
          end
          else break;
        end;
        for I := Low(LBuildConfigurationNodes) to High(LBuildConfigurationNodes) do
          LItemGroupNode.ChildNodes.add(LBuildConfigurationNodes[i]);
      end;

      //order LDeploymentNode
      _SortAttributesByNodeName(LDeploymentNode);
      _SortNodesByNameAndAttributes(LDeploymentNode);

      //remove from deployment unnecessary items
      //https://quality.embarcadero.com/browse/RSP-28003
      if LDeploymentNode <> nil then
        for I := LDeploymentNode.ChildNodes.Count - 1 downto 0 do begin
          LEnabledNode := nil;
          LDeployFileNode := LDeploymentNode.ChildNodes[i];
          if (ALSameText(LDeployFileNode.NodeName, 'DeployFile')) then begin
            for j := LDeployFileNode.ChildNodes.Count - 1 downto 0 do begin
              LEnabledNode := LDeployFileNode.ChildNodes[j].ChildNodes.FindNode('Enabled');
              if (LEnabledNode <> nil) and (ALSameText(LEnabledNode.Text,'false')) then break;
            end;
            if (not ALSameText(LDeployFileNode.attributes['Class'], 'File')) and
               ((LEnabledNode = nil) or                                           // normally we can also update other properties of a deploy file not only
                (not ALSameText(LEnabledNode.Text,'false'))) then                 // enabled, but i consider we can only update enabled
              LDeploymentNode.ChildNodes.Delete(i);
          end
          else if (ALSameText(LDeployFileNode.NodeName, 'DeployClass')) or    // this DeployClass seam not correctly updated
                  (ALSameText(LDeployFileNode.NodeName, 'ProjectRoot')) then  // so I prefer to delete them (don't know what could be the consequence)
            LDeploymentNode.ChildNodes.Delete(i);                             // and ProjectRoot seam also to be useless
        end;

      //put ProjectRoot at the end (don't know if it's matter)
      if LDeploymentNode <> nil then begin
        setlength(LProjectRootNodes, 0);
        while True do begin
          LProjectRootNode := LDeploymentNode.ChildNodes.FindNode('ProjectRoot');
          if LProjectRootNode <> nil then begin
            Setlength(LProjectRootNodes, length(LProjectRootNodes)+1);
            LProjectRootNodes[length(LProjectRootNodes) - 1] := LDeploymentNode.ChildNodes.Extract(LProjectRootNode);
          end
          else break;
        end;
        for I := Low(LProjectRootNodes) to High(LProjectRootNodes) do
          LDeploymentNode.ChildNodes.add(LProjectRootNodes[i]);
      end;

      //Remove Empty ProjectExtensions Node
      _RemoveEmptyProjectExtensionsNode(LXmlDoc.DocumentElement);

      //remove CData nodes
      _RemoveCDataNodes(LXmlDoc.DocumentElement);

      //save the file to LXmlStr
      LXmlDoc.SaveToXML(LXmlStr);

      //now add the indent to the node
      LXmlDoc.Options := [doNodeAutoIndent];
      LXmlDoc.LoadFromXML(LXmlStr);
      LXmlDoc.SaveToXML(LXmlStr);

      //save the dproj
      if LCreateBackup then begin
        if ALFileExists(LDProjFilename + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [LDProjFilename + '.bak']);
        if not ALrenameFile(LDProjFilename, LDProjFilename+ '.bak') then raiseLastOsError;
      end;
      ALSaveStringToFile(cAlUTF8Bom + LXmlStr, LDProjFilename);

    Finally
      ALFreeAndNil(LXmlDoc);
    End;

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;
end.
