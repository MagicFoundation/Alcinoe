program DProjNormalizer;

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
    for var i := 0 to aNode.ChildNodes.Count - 1 do
      _SortAttributesByNodeName(aNode.ChildNodes[i]);
End;

{************************************************************************}
Procedure _SortChildNodesByNodeNameAndAttributes(const aNode: TalXmlNode);
Begin
  if ANode = nil then exit;
  if ANode.ChildNodes <> nil then
    ANode.ChildNodes.CustomSort(
      function(List: TALXMLNodeList; Index1, Index2: Integer): Integer
      begin
        result := ALCompareStr(List[index1].NodeName, List[index2].NodeName);
        if (result = 0) then begin
          var LXmlStr1: AnsiString;
          var LXmlStr2: AnsiString;
          List[index1].SaveToXML(LXmlStr1);  // << yes I know it's ugly
          List[index2].SaveToXML(LXmlStr2);  // << but I m lazzy
          result := ALCompareStr(LXmlStr1, LXmlStr2);
        end;
      end);
  //-----
  if aNode.ChildNodes <> nil then
    for var i := 0 to aNode.ChildNodes.Count - 1 do
      _SortChildNodesByNodeNameAndAttributes(aNode.ChildNodes[i]);
End;

{*******************************************************************}
Procedure _RemoveEmptyProjectExtensionsNode(const aNode: TalXmlNode);
Begin
  if ANode = nil then exit;
  if aNode.ChildNodes <> nil then
    for var i := aNode.ChildNodes.Count - 1 downto 0 do
      if (aNode.ChildNodes[i].NodeName = 'ProjectExtensions') and
         ((aNode.ChildNodes[i].ChildNodes = nil) or
          (aNode.ChildNodes[i].ChildNodes.Count = 0)) then
        aNode.ChildNodes.Delete(i);
End;

{***************************************************}
Procedure _RemoveCDataNodes(const aNode: TalXmlNode);
Begin
  if ANode = nil then exit;
  if aNode.NodeType = ntCData then begin
    var LValue := aNode.Text;
    var LParentNode := ANode.ParentNode;
    if LParentNode.ChildNodes.Count <> 1 then
      raise Exception.Create('Error 2F418A94-9F42-4418-90D7-ED8EC120D1B0');
    LParentNode.ChildNodes.Clear;
    LPArentNode.Text := LValue;
  end;
  //-----
  if aNode.ChildNodes <> nil then
    for var i := 0 to aNode.ChildNodes.Count - 1 do
      _RemoveCDataNodes(aNode.ChildNodes[i]);
End;

begin

  try

    //Init project params 
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    //init LDProjFilename / LCreateBackup
    var LDProjFilename: String;
    var LCreateBackup: Boolean;
    var LParamLst := TALStringListU.Create;
    try
      for var I := 1 to ParamCount do
        LParamLst.Add(ParamStr(i));
      LDProjFilename := ALTrimU(LParamLst.Values['-DProj']);
      LCreateBackup := not ALSameTextU(ALTrimU(LParamLst.Values['-CreateBackup']), 'false');
    finally
      ALFreeAndNil(LParamLst);
    end;
    if LDProjFilename = '' then begin
      LDProjFilename := ALTrimU(paramstr(1));
      LCreateBackup := not ALSameTextU(ALTrimU(paramstr(2)), 'false');
    end;
    if LDProjFilename = '' then raise Exception.Create('Usage: DProjNormalizer.exe -DProj="<DprojFilename>" -CreateBackup=<true/false>');

    //create the LDProjXmlDoc
    var LDProjXmlDoc := TALXmlDocument.Create('root');
    Try

      //load the LDProjXmlDoc
      LDProjXmlDoc.Options := [];
      LDProjXmlDoc.ParseOptions := [];
      LDProjXmlDoc.LoadFromFile(LDProjFilename);

      //init LProjectExtensionsNode
      var LProjectExtensionsNode := LDProjXmlDoc.DocumentElement.ChildNodes.FindNode('ProjectExtensions');
      if LProjectExtensionsNode = nil then raise Exception.Create('ProjectExtensions node not found!');

      //init LBorlandProjectNode
      var LBorlandProjectNode := LProjectExtensionsNode.ChildNodes.FindNode('BorlandProject');
      if LBorlandProjectNode = nil then raise Exception.Create('ProjectExtensions.BorlandProject node not found!');

      //init LDeploymentNode
      var LDeploymentNode := LBorlandProjectNode.ChildNodes.FindNode('Deployment');
      if LDeploymentNode = nil then raise Exception.Create('ProjectExtensions.BorlandProject.Deployment node not found!');

      //check version attribute <Deployment Version="3">
      //if LDeploymentNode.Attributes['Version'] <> '3' then
      //  raise Exception.Create('ProjectExtensions.BorlandProject.Deployment.Version is not in the expected value (3)');

      //init LItemGroupNode
      var LItemGroupNode := LDProjXmlDoc.DocumentElement.ChildNodes.FindNode('ItemGroup');
      if LItemGroupNode = nil then raise Exception.Create('ItemGroup node not found!');

      //order ItemGroup
      _SortAttributesByNodeName(LItemGroupNode);
      _SortChildNodesByNodeNameAndAttributes(LItemGroupNode);

      //put DelphiCompile at the top (don't know if it's matter)
      var LDelphiCompileNodes: Tarray<TalXmlNode>;
      setlength(LDelphiCompileNodes, 0);
      while True do begin
        var LDelphiCompileNode := LItemGroupNode.ChildNodes.FindNode('DelphiCompile');
        if LDelphiCompileNode <> nil then begin
          Setlength(LDelphiCompileNodes, length(LDelphiCompileNodes)+1);
          LDelphiCompileNodes[length(LDelphiCompileNodes) - 1] := LItemGroupNode.ChildNodes.Extract(LDelphiCompileNode);
        end
        else break;
      end;
      for var I := High(LDelphiCompileNodes) downto Low(LDelphiCompileNodes) do
        LItemGroupNode.ChildNodes.insert(0, LDelphiCompileNodes[i]);

      //put BuildConfiguration at the end (don't know if it's matter)
      var LBuildConfigurationNodes: Tarray<TalXmlNode>;
      setlength(LBuildConfigurationNodes, 0);
      while True do begin
        var LBuildConfigurationNode := LItemGroupNode.ChildNodes.FindNode('BuildConfiguration');
        if LBuildConfigurationNode <> nil then begin
          Setlength(LBuildConfigurationNodes, length(LBuildConfigurationNodes)+1);
          LBuildConfigurationNodes[length(LBuildConfigurationNodes) - 1] := LItemGroupNode.ChildNodes.Extract(LBuildConfigurationNode);
        end
        else break;
      end;
      for var I := Low(LBuildConfigurationNodes) to High(LBuildConfigurationNodes) do
        LItemGroupNode.ChildNodes.add(LBuildConfigurationNodes[i]);

      //order LDeploymentNode
      _SortAttributesByNodeName(LDeploymentNode);
      _SortChildNodesByNodeNameAndAttributes(LDeploymentNode);

      //remove from deployment unnecessary items
      //https://quality.embarcadero.com/browse/RSP-28003
      for var I := LDeploymentNode.ChildNodes.Count - 1 downto 0 do begin
        var LEnabledNode: TALXmlNode := nil;
        var LDeployFileNode := LDeploymentNode.ChildNodes[i];
        if (ALSameText(LDeployFileNode.NodeName, 'DeployFile')) then begin
          for var j := LDeployFileNode.ChildNodes.Count - 1 downto 0 do begin
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
      var LProjectRootNodes: Tarray<TalXmlNode>;
      setlength(LProjectRootNodes, 0);
      while True do begin
        var LProjectRootNode := LDeploymentNode.ChildNodes.FindNode('ProjectRoot');
        if LProjectRootNode <> nil then begin
          Setlength(LProjectRootNodes, length(LProjectRootNodes)+1);
          LProjectRootNodes[length(LProjectRootNodes) - 1] := LDeploymentNode.ChildNodes.Extract(LProjectRootNode);
        end
        else break;
      end;
      for var I := Low(LProjectRootNodes) to High(LProjectRootNodes) do
        LDeploymentNode.ChildNodes.add(LProjectRootNodes[i]);

      //Remove Empty ProjectExtensions Node
      _RemoveEmptyProjectExtensionsNode(LDProjXmlDoc.DocumentElement);

      //remove CData nodes
      _RemoveCDataNodes(LDProjXmlDoc.DocumentElement);

      //save the file to LXmlStr
      var LXmlStr: AnsiString;
      LDProjXmlDoc.SaveToXML(LXmlStr);

      //now add the indent to the node
      LDProjXmlDoc.Options := [doNodeAutoIndent];
      LDProjXmlDoc.LoadFromXML(LXmlStr);
      LDProjXmlDoc.SaveToXML(LXmlStr);

      //save the dproj
      if LCreateBackup then begin
        if ALFileExistsU(LDProjFilename + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [LDProjFilename + '.bak']);
        if not ALrenameFileU(LDProjFilename, LDProjFilename+ '.bak') then raiseLastOsError;
      end;
      ALSaveStringToFile(cAlUTF8Bom + LXmlStr, LDProjFilename);

    Finally
      ALFreeAndNil(LDProjXmlDoc);
    End;

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;

end.
