program DProjNormalizer;

{$APPTYPE CONSOLE}

{$R *.res}

{$I ..\..\..\Source\Alcinoe.inc}

uses
  system.IOUtils,
  System.AnsiStrings,
  System.SysUtils,
  Alcinoe.XMLDoc,
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.Files,
  Alcinoe.StringUtils;

//
// https://quality.embarcadero.com/browse/RSP-28003
// This process orders all nodes and performs the following actions:
//
// * Remove All DCCReference Nodes:
//    <DCCReference Include="Unit1.pas">
//       <Form>Form1</Form>
//       <FormType>fmx</FormType>
//    </DCCReference>
//
// * Remove Empty <Disabled/> Nodes from JavaReference Nodes:
//    <JavaReference Include="android\Merged\libs\androidx.activity-activity-1.5.1.jar">
//      <ContainerId>ClassesdexFile64</ContainerId>
//      <Disabled/>
//    </JavaReference>
//   =>
//    <JavaReference Include="android\Merged\libs\androidx.activity-activity-1.5.1.jar">
//      <ContainerId>ClassesdexFile64</ContainerId>
//    </JavaReference>
//
// * Remove All DeployClass Nodes:
//    <DeployClass Name="AdditionalDebugSymbols">
//        <Platform Name="iOSSimulator">
//            <Operation>1</Operation>
//        </Platform>
//        <Platform Name="OSX32">
//          <RemoteDir>Contents\MacOS</RemoteDir>
//            <Operation>1</Operation>
//        </Platform>
//        <Platform Name="Win32">
//            <Operation>0</Operation>
//        </Platform>
//    </DeployClass>
//
// * Remove All ProjectRoot Elements:
//    <ProjectRoot Platform="Android" Name="$(PROJECTNAME)"/>
//
// * Deletes <DeployFile> nodes unless they have a Class attribute equal to
//   'File' and an Enabled attribute not equal to false. Enabled attribute
//   not equal to false to prevents the IDE from automatically re-adding and
//   deploying these files when the project is reopened:
//    <DeployFile LocalName="$(BDS)\bin\Artwork\Android\FM_LauncherIcon_144x144.png" Configuration="Debug" Class="Android_LauncherIcon144">
//        <Platform Name="Android">
//            <RemoteName>ic_launcher.png</RemoteName>
//            <Overwrite>true</Overwrite>
//        </Platform>
//    </DeployFile>
//
// * Remove Empty ProjectExtensions Nodes:
//    <ProjectExtensions/>
//
// * Transforms nodes with CDATA sections into regular text nodes:
//    <PreBuildEvent><![CDATA["..\..\..\..\Alcinoe\Tools\RJarSwapper\RJarSwapper.bat" -RJarDir="Android\Merged\libs\" -IsAabPackage="false"]]></PreBuildEvent>
//    <PreBuildEvent>"..\..\..\..\Alcinoe\Tools\RJarSwapper\RJarSwapper.bat" -RJarDir="Android\Merged\libs\" -IsAabPackage="false"</PreBuildEvent>
//

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if the structure of the *.dproj didn''t changed by following the instructions in References\BlankApplication\README.md'}
{$ENDIF}

{***********************************************************}
Procedure _SortAttributesByNodeName(const aNode: TalXmlNode);
Begin
  if ANode = nil then exit;
  if ANode.AttributeNodes <> nil then
    ANode.AttributeNodes.CustomSort(
      function(List: TALXMLNodeList; Index1, Index2: Integer): Integer
      begin
        result := ALCompareStrA(List[index1].NodeName, List[index2].NodeName);
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
        result := ALCompareStrA(List[index1].NodeName, List[index2].NodeName);
        if (result = 0) then begin
          var LXmlStr1: AnsiString;
          var LXmlStr2: AnsiString;
          List[index1].SaveToXML(LXmlStr1);  // << yes I know it's ugly
          List[index2].SaveToXML(LXmlStr2);  // << but I m lazzy
          result := ALCompareStrA(LXmlStr1, LXmlStr2);
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

{***************************************************************}
Procedure _ReplaceCDataNodesByTextNodes(const aNode: TalXmlNode);
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
      _ReplaceCDataNodesByTextNodes(aNode.ChildNodes[i]);
End;

begin

  try

    // Init project params
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    // init LDProjFilename / LCreateBackup
    var LDProjFilename: String;
    var LCreateBackup: Boolean;
    var LParamLst := TALStringListW.Create;
    try
      for var I := 1 to ParamCount do
        LParamLst.Add(ParamStr(i));
      LDProjFilename := ALTrim(LParamLst.Values['-DProj']);
      LCreateBackup := not ALSameTextW(ALTrim(LParamLst.Values['-CreateBackup']), 'false');
    finally
      ALFreeAndNil(LParamLst);
    end;
    if LDProjFilename = '' then begin
      LDProjFilename := ALTrim(paramstr(1));
      LCreateBackup := not ALSameTextW(ALTrim(paramstr(2)), 'false');
    end;
    if LDProjFilename = '' then raise Exception.Create('Usage: DProjNormalizer.exe -DProj="<DprojFilename>" -CreateBackup=<true/false>');

    // create the LDProjXmlDoc
    var LDProjXmlDoc := TALXmlDocument.Create('root');
    Try

      // load the LDProjXmlDoc
      LDProjXmlDoc.Options := [];
      LDProjXmlDoc.ParseOptions := [];
      LDProjXmlDoc.LoadFromFile(LDProjFilename);

      // init LProjectExtensionsNode
      var LProjectExtensionsNode := LDProjXmlDoc.DocumentElement.ChildNodes.FindNode('ProjectExtensions');
      if LProjectExtensionsNode = nil then raise Exception.Create('ProjectExtensions node not found!');

      // init LBorlandProjectNode
      var LBorlandProjectNode := LProjectExtensionsNode.ChildNodes.FindNode('BorlandProject');
      if LBorlandProjectNode = nil then raise Exception.Create('ProjectExtensions.BorlandProject node not found!');

      // init LDeploymentNode
      var LDeploymentNode := LBorlandProjectNode.ChildNodes.FindNode('Deployment');

      // init LItemGroupNode
      var LItemGroupNode := LDProjXmlDoc.DocumentElement.ChildNodes.FindNode('ItemGroup');
      if LItemGroupNode = nil then raise Exception.Create('ItemGroup node not found!');

      // order ItemGroup
      _SortAttributesByNodeName(LItemGroupNode);
      _SortChildNodesByNodeNameAndAttributes(LItemGroupNode);

      // put DelphiCompile at the top (don't know if it's matter)
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

      // put BuildConfiguration at the end (don't know if it's matter)
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

      // order LDeploymentNode
      if LDeploymentNode <> nil then begin
        _SortAttributesByNodeName(LDeploymentNode);
        _SortChildNodesByNodeNameAndAttributes(LDeploymentNode);
      end;

      // Remove Empty <Disabled/> Nodes from JavaReference Nodes:
      //  <JavaReference Include="android\Merged\libs\androidx.activity-activity-1.5.1.jar">
      //    <ContainerId>ClassesdexFile64</ContainerId>
      //    <Disabled/>
      //  </JavaReference>
      // =>
      //  <JavaReference Include="android\Merged\libs\androidx.activity-activity-1.5.1.jar">
      //    <ContainerId>ClassesdexFile64</ContainerId>
      //  </JavaReference>
      for var I := LItemGroupNode.ChildNodes.Count - 1 downto 0 do begin
        var LJavaReferenceNode := LItemGroupNode.ChildNodes[i];
        if ALSameTextA(LJavaReferenceNode.NodeName, 'JavaReference') then begin
          var LDisabledNode := LJavaReferenceNode.ChildNodes.FindNode('Disabled');
          if (LDisabledNode <> nil) and (ALSameTextA(LDisabledNode.Text, 'false') or (LDisabledNode.Text='')) then
            LJavaReferenceNode.ChildNodes.Remove(LDisabledNode);
        end;
      end;

      // Remove All DCCReference Nodes:
      //  <DCCReference Include="Unit1.pas">
      //     <Form>Form1</Form>
      //     <FormType>fmx</FormType>
      //  </DCCReference>
      for var I := LItemGroupNode.ChildNodes.Count - 1 downto 0 do begin
        var LDCCReferenceNode := LItemGroupNode.ChildNodes[i];
        if ALSameTextA(LDccReferenceNode.NodeName, 'DCCReference') then
          LItemGroupNode.ChildNodes.Delete(i);
      end;

      // Remove All DeployClass Nodes:
      //  <DeployClass Name="AdditionalDebugSymbols">
      //      <Platform Name="iOSSimulator">
      //          <Operation>1</Operation>
      //      </Platform>
      //      <Platform Name="OSX32">
      //        <RemoteDir>Contents\MacOS</RemoteDir>
      //          <Operation>1</Operation>
      //      </Platform>
      //      <Platform Name="Win32">
      //          <Operation>0</Operation>
      //      </Platform>
      //  </DeployClass>
      //
      // Remove All ProjectRoot Elements:
      //  <ProjectRoot Platform="Android" Name="$(PROJECTNAME)"/>
      //
      // Deletes <DeployFile> nodes unless they have a Class attribute equal to
      // 'File' or 'Skia' and an Enabled attribute not equal to false. Enabled attribute
      // not equal to false to prevents the IDE from automatically re-adding and
      // deploying these files when the project is reopened:
      //  <DeployFile LocalName="$(BDS)\bin\Artwork\Android\FM_LauncherIcon_144x144.png" Configuration="Debug" Class="Android_LauncherIcon144">
      //      <Platform Name="Android">
      //          <RemoteName>ic_launcher.png</RemoteName>
      //          <Overwrite>true</Overwrite>
      //      </Platform>
      //  </DeployFile>
      if LDeploymentNode <> nil then begin
        for var I := LDeploymentNode.ChildNodes.Count - 1 downto 0 do begin
          var LEnabledNode: TALXmlNode := nil;
          var LDeployFileNode := LDeploymentNode.ChildNodes[i];
          if (ALSameTextA(LDeployFileNode.NodeName, 'DeployFile')) then begin
            for var j := LDeployFileNode.ChildNodes.Count - 1 downto 0 do begin
              LEnabledNode := LDeployFileNode.ChildNodes[j].ChildNodes.FindNode('Enabled');
              if (LEnabledNode <> nil) and (ALSameTextA(LEnabledNode.Text,'false')) then break;
            end;
            if (not ALSameTextA(LDeployFileNode.attributes['Class'], 'File')) and
               (not ALSameTextA(LDeployFileNode.attributes['Class'], 'Skia')) and
               ((LEnabledNode = nil) or                             // normally we can also update other properties of a deploy file not only
                (not ALSameTextA(LEnabledNode.Text,'false'))) then  // enabled, but i consider we can only update enabled
              LDeploymentNode.ChildNodes.Delete(i);
          end
          else if (ALSameTextA(LDeployFileNode.NodeName, 'DeployClass')) or    // this DeployClass seam not correctly updated
                  (ALSameTextA(LDeployFileNode.NodeName, 'ProjectRoot')) then  // so I prefer to delete them (don't know what could be the consequence)
            LDeploymentNode.ChildNodes.Delete(i);                              // and ProjectRoot seam also to be useless
        end;
      end;

      // Remove Empty ProjectExtensions Nodes:
      //  <ProjectExtensions/>
      _RemoveEmptyProjectExtensionsNode(LDProjXmlDoc.DocumentElement);

      // Transforms nodes with CDATA sections into regular text nodes:
      //  <PreBuildEvent><![CDATA["..\..\..\..\Alcinoe\Tools\RJarSwapper\RJarSwapper.bat" -RJarDir="Android\Merged\libs\" -IsAabPackage="false"]]></PreBuildEvent>
      //  <PreBuildEvent>"..\..\..\..\Alcinoe\Tools\RJarSwapper\RJarSwapper.bat" -RJarDir="Android\Merged\libs\" -IsAabPackage="false"</PreBuildEvent>
      _ReplaceCDataNodesByTextNodes(LDProjXmlDoc.DocumentElement);

      // save the file to LXmlStr
      var LXmlStr: AnsiString;
      LDProjXmlDoc.SaveToXML(LXmlStr);

      // now add the indent to the node
      LDProjXmlDoc.Options := [doNodeAutoIndent];
      LDProjXmlDoc.LoadFromXML(LXmlStr);
      LDProjXmlDoc.SaveToXML(LXmlStr);

      // save the dproj
      if LCreateBackup then begin
        if Tfile.Exists(LDProjFilename + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [LDProjFilename + '.bak']);
        Tfile.Move(LDProjFilename, LDProjFilename+ '.bak');
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
