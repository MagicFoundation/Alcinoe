program DeployMan;

{$APPTYPE CONSOLE}

{$R *.res}

{$I ..\..\..\..\Source\Alcinoe.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  system.AnsiStrings,
  Alcinoe.JSONDoc,
  Alcinoe.StringUtils,
  Alcinoe.Execute,
  Alcinoe.Common,
  Alcinoe.XMLDoc,
  Alcinoe.StringList;

var
  WritelnDuplicatesToSkip: TALStringListW;

{**************************************}
Procedure OverWrite(const AStr: String);
begin
  var LStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if LStdOut = INVALID_HANDLE_VALUE then RaiseLastOsError;
  var LInfo : CONSOLE_SCREEN_BUFFER_INFO;
  if not GetConsoleScreenBufferInfo(LStdOut, LInfo) then raiseLastOsError;
  Write(#13);
  if Linfo.dwSize.X - 1 > 0 then Write(StringOfChar(' ', Linfo.dwSize.X - 1));
  Write(#13);
  if length(AStr) > Linfo.dwSize.X - 1 then System.Write(ALCopyStr(AStr, 1, Linfo.dwSize.X - 4) + '...')
  else if length(AStr) > 0 then System.Write(AStr);
end;

{****************}
Procedure WriteLN(
            const AStr: String;
            const AForegroundColor: TALConsoleColor = TALConsoleColor.ccDarkWhite;
            const ASkipDuplicates: Boolean = false); overload;
begin
  if (not ASkipDuplicates) or (WritelnDuplicatesToSkip.IndexOf(AStr) < 0) then begin
    WritelnDuplicatesToSkip.Add(AStr);
    OverWrite('');
    ALWriteln(AStr, AForegroundColor);
  end;
end;

{***********************************************}
Procedure ExecuteCmdLine(const ACmdLine: String);
begin
  Var LInputStream := TMemorystream.Create;
  Var LOutputStream := TStringStream.Create;
  try
    OverWrite(ACmdLine);
    Var LCmdLineResult := ALWinExecW(
                            ACmdLine, // const aCommandLine: String;
                            LInputStream, // const aInputStream: Tstream;
                            LOutputStream); //const aOutputStream: TStream;
    if LCmdLineResult <> 0 then
      raise Exception.Createfmt('Failed to execute %s'#13#10'%s', [ACmdLine, LOutputStream.DataString]);
  finally
    ALFreeandNil(LInputStream);
    ALFreeandNil(LOutputStream);
  end;
end;

{*******************************************************************}
//we need this function for debuging because their is a bug in delphi
//that make we can not debug inlined var when they are inside the
//begin ... end of the dpr
procedure Kickoff;
begin
  try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    {$REGION 'Init local vars'}
    var LNoInteraction: Boolean;
    {$ENDREGION}

    {$REGION 'create local objects'}
    var LParamLst := TALStringListW.Create;
    Var LPaths := TALJSONDocumentW.Create;
    var LConfigs := TALStringListA.Create;
    var LPlatforms := TALStringListA.Create;
    {$ENDREGION}

    try

      {$REGION 'Init LParamLst'}
      for var I := 1 to ParamCount do
        LParamLst.Add(ParamStr(i));
      {$ENDREGION}

      {$REGION 'Init LNoInteraction'}
      LNoInteraction := AlStrToBool(ALTrim(LParamLst.Values['-NoInteraction']));
      {$ENDREGION}

      {$REGION 'Init LDProjFilename'}
      var LDProjFilename := ExpandFileName(ALTrim(LParamLst.Values['-DProj']));
      if LDProjFilename = '' then raise Exception.Create('DProj param is mandatory');
      if not Tfile.Exists(LDProjFilename) then raise Exception.Create('DProj file not exists');
      var LDProjDir := ALExtractFilePath(LDProjFilename);
      {$ENDREGION}

      {$REGION 'Init LConfigs'}
      LConfigs.LineBreak := ';';
      LConfigs.Text := AnsiString(LParamLst.Values['-Configurations']);
      if LConfigs.Count = 0 then begin
        LConfigs.Add('Debug');
        LConfigs.Add('Release');
      end;
      {$ENDREGION}

      {$REGION 'Init LPlatforms'}
      LPlatforms.CaseSensitive := False;
      LPlatforms.LineBreak := ';';
      LPlatforms.Text := AnsiString(LParamLst.Values['-Platforms']);
      if LPlatforms.Count = 0 then begin
        LPlatforms.Add('Android');
        LPlatforms.Add('Android64');
        LPlatforms.Add('iOSDevice64');
        LPlatforms.Add('iOSSimARM64');
      end;
      for var LPlatform in LPlatforms do begin
        if (LPlatform <> 'Android') and
           (LPlatform <> 'Android64') and
           (LPlatform <> 'iOSDevice64') and
           (LPlatform <> 'iOSSimARM64') and
           (LPlatform <> 'OSX64') and
           (LPlatform <> 'OSXARM64') then raise Exception.Create('Invalid Platforms parameter');
      end;
      {$ENDREGION}

      {$REGION 'Init LDProjNormalizer'}
      var LDProjNormalizer := ExpandFileName(ALTrim(LParamLst.Values['-DProjNormalizer']));
      {$ENDREGION}

      {$REGION 'Init LPaths'}
      var LPathsLst := TALStringListW.Create;
      Try
        LPathsLst.LineBreak := ';';
        LPathsLst.Text := LParamLst.Values['-Paths'];
        if LPathsLst.Count = 0 then raise Exception.Create('Paths param is mandatory');
        for var I := 0 to LPathsLst.Count - 1 do begin
          Var LPathLst := TALStringListW.Create;
          try
            LPathLst.LineBreak := '|';
            LPathLst.Text := LPathsLst[I]; // .\_Build\Sample\DirA|true|.\PlugIns
            if LPathLst.Count <> 3 then raise Exception.Create('Paths param must be in the format <LocalPath>|<IncludeSubDirs>|<RemotePath>');
            var LLocalPath := ExpandFileName(ALTrim(LPathLst[0])); // C:\Dev\MagicFoundation\Alcinoe\Tools\DeployMan\_Build\Sample\DirA
            var LRemotePath := LPathLst[2]; // .\PlugIns
            if TDirectory.Exists(LLocalPath) then begin
              LLocalPath := ALIncludeTrailingPathDelimiterW(LLocalPath); // C:\Dev\MagicFoundation\Alcinoe\Tools\DeployMan\_Build\Sample\DirA\
              var LSearchOption: TSearchOption;
              if AlStrToBool(LPathLst[1]) then LSearchOption := TSearchOption.soAllDirectories
              else LSearchOption := TSearchOption.soTopDirectoryOnly;
              var LFiles := TDirectory.GetFiles(LLocalPath, '*', LSearchOption);
              for var Lfile in LFiles do begin
                With LPaths.AddChild('path', ntObject) do begin
                  Var LTmpRemotepath := Tpath.Combine(LRemotePath, ExtractRelativePath(LLocalPath, Lfile)); // .\PlugIns\notificationservice.appex\Info.plist
                  Addchild('localpath').Text := ExtractRelativePath(LDProjDir, Lfile); // DirA\notificationservice.appex\Info.plist;
                  Addchild('remotepath').Text := LTmpRemotepath;
                  Writeln(LTmpRemotepath);
                end;
              end;
            end
            else if TFile.Exists(LLocalPath) then begin
              With LPaths.AddChild('path', ntObject) do begin
                Addchild('localpath').Text := ExtractRelativePath(LDProjDir, LLocalPath); // google-services.json
                Addchild('remotepath').Text := LRemotePath;
                Writeln(LRemotePath);
              end;
            end
            else raise Exception.CreateFmt('Could not find %s', [LLocalPath]);
          finally
            ALFreeAndNil(LPathLst);
          end;
        end;
      Finally
        ALFreeAndNil(LPathsLst);
      End;
      {$ENDREGION}

      {$REGION 'Update dproj'}
      var LdprojXmlDoc := TalXmlDocument.Create('root');
      try

        //init LdprojXmlDoc
        LdprojXmlDoc.LoadFromFile(LDProjFilename);

        //init LProjectExtensionsNode
        var LProjectExtensionsNode := LDProjXmlDoc.DocumentElement.ChildNodes.FindNode('ProjectExtensions');
        if LProjectExtensionsNode = nil then raise Exception.Create('ProjectExtensions node not found!');

        //init LBorlandProjectNode
        var LBorlandProjectNode := LProjectExtensionsNode.ChildNodes.FindNode('BorlandProject');
        if LBorlandProjectNode = nil then raise Exception.Create('ProjectExtensions.BorlandProject node not found!');

        //init LDeploymentNode
        var LDeploymentNode := LBorlandProjectNode.ChildNodes.FindNode('Deployment');
        if LDeploymentNode = nil then raise Exception.Create('ProjectExtensions.BorlandProject.Deployment node not found!');

        //remove from deployment all items with Class="File"
        //<DeployFile LocalName="android\res\drawable\common_google_signin_btn_icon_disabled.xml" Configuration="Release" Class="File">
        //    <Platform Name="Android">
        //        <RemoteDir>res\drawable</RemoteDir>
        //        <RemoteName>common_google_signin_btn_icon_disabled.xml</RemoteName>
        //        <Overwrite>true</Overwrite>
        //    </Platform>
        //    <Platform Name="Android64">
        //        <RemoteDir>res\drawable</RemoteDir>
        //        <RemoteName>common_google_signin_btn_icon_disabled.xml</RemoteName>
        //        <Overwrite>true</Overwrite>
        //    </Platform>
        //</DeployFile>
        for var I := LDeploymentNode.ChildNodes.Count - 1 downto 0 do begin
          var LDeployFileNode := LDeploymentNode.ChildNodes[i];
          if ALSameTextA(LDeployFileNode.NodeName, 'DeployFile') then begin
            if ALSameTextA(LDeployFileNode.Attributes['Class'], 'File') then begin
              if LDeployFileNode.ChildNodes.Count = 0 then raise Exception.Create('Error A892E02E-A8BA-4003-AEF1-A81271AD0A9F');
              for Var J := LDeployFileNode.ChildNodes.Count - 1 downto 0 do begin
                var LPlatformNode := LDeployFileNode.ChildNodes[j];
                if not ALSameTextA(LPlatformNode.NodeName, 'Platform') then raise Exception.Create('Error 65714071-86F2-4796-82F8-9F01C5C9824B');
                var LName := LPlatformNode.Attributes['Name'];
                if LName = '' then raise Exception.Create('Error 42C3C299-BA31-4B17-9EA4-8320E0048848');
                if (LPlatforms.IndexOfName(LName) >= 0) then begin
                  LDeployFileNode.ChildNodes.Delete(j);
                  continue;
                end;
              end;
              if LDeployFileNode.ChildNodes.Count = 0 then
                LDeploymentNode.ChildNodes.Delete(i);
            end;
          end;
        end;

        //add to deployment all items from local res\ folder
        for Var I := 0 to Lpaths.ChildNodes.Count - 1 do begin
          var Lpath := Lpaths.ChildNodes[i];
          Var LLocalPath := AnsiString(Lpath.GetChildNodeValueText('localpath', '')); // DirA\notificationservice.appex\Info.plist
          Var LRemotePath := AnsiString(Lpath.GetChildNodeValueText('remotepath', '')); // .\PlugIns\notificationservice.appex\Info.plist
          for var LConfig in LConfigs do begin
            With LDeploymentNode.AddChild('DeployFile') do begin
              Attributes['LocalName'] := LLocalPath; // DirA\notificationservice.appex\Info.plist
              Attributes['Configuration'] := LConfig;
              Attributes['Class'] := 'File';
              for var LPlatForm in LPlatforms do begin
                With AddChild('Platform') do begin
                  Attributes['Name'] := LPlatForm;
                  Addchild('RemoteDir').Text := ALExcludeTrailingPathDelimiterA(ALExtractFilePath(LRemotePath)); // .\PlugIns\notificationservice.appex
                  Addchild('RemoteName').Text := ALExtractFileName(LRemotePath); // Info.plist
                  Addchild('Overwrite').Text := 'true';
                end;
              end;
            end;
          end;
        end;

        //save LdprojXmlDoc
        var LDprojXmlSrc: AnsiString;
        LdprojXmlDoc.SaveToXML(LDprojXmlSrc);
        LdprojXmlDoc.Options := [doNodeAutoIndent];
        LdprojXmlDoc.LoadFromXML(LDprojXmlSrc);
        LdprojXmlDoc.SaveToFile(LDProjFilename);

      finally
        ALFreeAndNil(LdprojXmlDoc);
      end;
      {$ENDREGION}

      {$REGION 'Update LDProjNormalizer'}
      if LDProjNormalizer <> '' then
        ExecuteCmdLine('"'+LDProjNormalizer+'" "' +LDProjFilename + '" false');
      {$ENDREGION}

    finally

      {$REGION 'Free local objects'}
      ALFreeAndNil(LParamLst);
      ALFreeAndNil(LPaths);
      ALFreeAndNil(LConfigs);
      ALFreeAndNil(LPlatforms);
      {$ENDREGION}

    end;

    if not LNoInteraction then begin
      Writeln('');
      Writeln('Finished');
      Writeln('Press <Enter> key to quit');
      Readln;
    end;

  except
    on E: Exception do begin
      Writeln(E.ClassName+': '+E.Message, TALConsoleColor.ccRed);
      Writeln('');
      Writeln('Usage:');
      Writeln('  DeployMan.exe');
      Writeln('    -DProj=Path to the project file (*.dproj).');
      Writeln('    -Paths=<LocalPath>|<IncludeSubDirs>|<RemotePath>. Separate paths with '';''.');
      Writeln('    -Configurations=Default Debug;Release. Separate Configurations with '';''.');
      Writeln('    -Platforms=Default Android;Android64;iOSDevice64;iOSSimARM64. Separate Platforms with '';''.');
      Writeln('    -DProjNormalizer=Path to the Alcinoe DProjNormalizer tool.');
      Writeln('    -NoInteraction=Non-interactive mode.');
      Writeln('');
      Writeln('Example:');
      Writeln('  DeployMan.exe^');
      Writeln('    -DProj=c:\MyProject\MyProject.dproj^');
      Writeln('    -Paths=c:\MyProject\MyDirA|true|./MyDirA;c:\MyProject\MyFileB.json|false|./MyFileB.jsonB^');
      Writeln('    -DProjNormalizer=c:\Alcinoe\Tools\DeployProjNormalizer\DeployProjNormalizer.exe^');
      Writeln('    -Platforms=iOSDevice64');
      Writeln('');
      Writeln('');
      Writeln('DeployMan failed!');
      Writeln('Press <Enter> key to quit');
      Readln;
      halt(1);
    end;
  end;
end;

begin
  WritelnDuplicatesToSkip := TALStringListW.Create;
  try
    kickoff;
  finally
    ALFreeAndNil(WritelnDuplicatesToSkip);
  end;
end.
