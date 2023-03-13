program UsesNormalizer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Alcinoe.XMLDoc,
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.Files,
  Alcinoe.StringUtils;

procedure _cleanUses(Const AFilename: AnsiString; Const ASection: AnsiString);
begin

  Var LSrcStr: AnsiString := ALGetStringFromFile(Afilename);
  Var P1: Integer := ALPosIgnoreCaseA(#13#10+ASection+#13#10, LSrcStr);
  if P1 <= 0 then
    raise Exception.Create('Could not find the section "'+ASection+'" in '+ AFilename);
  P1 := ALPosIgnoreCaseA('uses', LSrcStr, P1);
  if P1 <= 0 then
    raise Exception.Create('Could not find the uses clause after the section "'+ASection+'" in '+ AFilename);
  //
  //we support only one ; in the Whole uses clause
  //so implementation like
  //
  //uses
  //  FMX.Memo.Types,
  //  {$IF defined(IOS)
  //  FMX.Platform.Device.iOS.pas;
  //  {$ELSE}
  //  FMX.Platform.Device.Android.pas;
  //  {$ENDIF}
  //
  //is forbidden
  //

  Finally
    ALFreeAndNil(LSrcLst);
  End;
end;


begin

  try

    //Init project params 
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    //init LDProjFilename / LCreateBackup
    var LDProjFilename := ALTrim(ansiString(paramstr(1)));
    if LDProjFilename = '' then raise Exception.Create('Usage: UsesNormalizer.exe "<DprojFilename>" <createBackup>(ie: true/false)');
    var LCreateBackup := not ALSameTextA(ALTrim(ansiString(paramstr(2))), 'false');

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
      if LDeploymentNode.Attributes['Version'] <> '3' then
        raise Exception.Create('ProjectExtensions.BorlandProject.Deployment.Version is not in the expected value (3)');

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
        if (ALSameTextA(LDeployFileNode.NodeName, 'DeployFile')) then begin
          for var j := LDeployFileNode.ChildNodes.Count - 1 downto 0 do begin
            LEnabledNode := LDeployFileNode.ChildNodes[j].ChildNodes.FindNode('Enabled');
            if (LEnabledNode <> nil) and (ALSameTextA(LEnabledNode.Text,'false')) then break;
          end;
          if (not ALSameTextA(LDeployFileNode.attributes['Class'], 'File')) and
             ((LEnabledNode = nil) or                                           // normally we can also update other properties of a deploy file not only
              (not ALSameTextA(LEnabledNode.Text,'false'))) then                 // enabled, but i consider we can only update enabled
            LDeploymentNode.ChildNodes.Delete(i);
        end
        else if (ALSameTextA(LDeployFileNode.NodeName, 'DeployClass')) or    // this DeployClass seam not correctly updated
                (ALSameTextA(LDeployFileNode.NodeName, 'ProjectRoot')) then  // so I prefer to delete them (don't know what could be the consequence)
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
        if ALFileExists(LDProjFilename + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [LDProjFilename + '.bak']);
        if not ALRenameFileA(LDProjFilename, LDProjFilename+ '.bak') then raiseLastOsError;
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
