program EnvOptionsProjUpdater;

{$APPTYPE CONSOLE}

{$R *.res}

{$I ..\..\..\Source\Alcinoe.inc}

uses
  System.AnsiStrings,
  system.IOUtils,
  System.SysUtils,
  System.Win.Registry,
  System.Classes,
  Winapi.Windows,
  Alcinoe.XMLDoc,
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.Localization,
  Alcinoe.StringUtils;

begin

  try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    var LParamLst := TALStringListW.Create;
    try
      for var I := 1 to ParamCount do
        LParamLst.Add(ParamStr(i));

      // Retrieve the BDSVersion
      var LBDSVersionStr: String := ALTrim(LParamLst.Values['-BDSVersion']);
      LParamLst.Values['-BDSVersion'] := '';
      if LBDSVersionStr = '' then begin
        var LRegistry := TRegistry.Create;
        try
          LRegistry.RootKey:=HKEY_CURRENT_USER;
          if not LRegistry.OpenKeyReadOnly('SOFTWARE\Embarcadero\BDS') then
            raise Exception.Create('Could not find Embarcadero Delphi Registry');
          var LSubKeyNames := TStringList.Create;
          Try
            LRegistry.GetKeyNames(LSubKeyNames);
            var LBDSVersionFloat: Double := 0;
            for var LName in LSubKeyNames do begin
              var LTmpCompilerVersionFloat := ALStrToFloatDef(LName, 0);
              if LTmpCompilerVersionFloat > LBDSVersionFloat then begin
                LBDSVersionStr := LName;
                LBDSVersionFloat := LTmpCompilerVersionFloat;
              end;
            end;
            if LBDSVersionStr = '' then raise Exception.Create('Incorrect Embarcadero Delphi Registry');
            LRegistry.CloseKey;
          Finally
            ALFreeAndNil(LSubKeyNames);
          End;
        finally
          ALFreeAndNil(LRegistry);
        end;
      end;

      // Init LEnvOptionsProjFilename
      var LEnvOptionsProjFilename := GetEnvironmentVariable('AppData') + '\Embarcadero\BDS\'+LBDSVersionStr+'\EnvOptions.proj';
      if not TFile.Exists(LEnvOptionsProjFilename) then
        raise Exception.Create('EnvOptions.proj not found');

      // Create LEnvOptionsDoc
      var LEnvOptionsDoc := TALXmlDocument.Create('root');
      Try
        LEnvOptionsDoc.Options := [doNodeAutoCreate];
        LEnvOptionsDoc.ParseOptions := [];
        LEnvOptionsDoc.LoadFromFile(LEnvOptionsProjFilename);

        // init LPropertyGroupNode
        var LPlatform := ALTrim(LParamLst.Values['-Platform']);
        LParamLst.Values['-Platform'] := '';
        if LPlatform = '' then raise Exception.Create('Platform is mandatory');
        var LPropertyGroupNode := ALFindXmlNodeByNameAndAttribute(
                                    LEnvOptionsDoc.DocumentElement,
                                    'PropertyGroup'{NodeName},
                                    'Condition'{AttributeName},
                                    '''$(Platform)''=='''+ansiString(LPlatform)+''''{AttributeValue});
        if LPropertyGroupNode = nil then raise Exception.Create('PropertyGroup node not found');

        // update LPropertyGroupNode with custom values
        for var I := 0 to LParamLst.count - 1 do begin
          var LParamName := AnsiString(LParamLst.Names[I]);
          if LParamName = '' then continue;
          if LParamName[low(LParamName)] <> '-' then
            Raise Exception.Create('Invalid params');
          delete(LParamName, 1, 1);
          var LParamValue := AnsiString(LParamLst.ValueFromIndex[I]);
          LParamValue := ALStringReplaceA(LParamValue, '<br>', #10, [RfReplaceALL]);
          LParamValue := ALStringReplaceA(LParamValue, '<br/>', #10, [RfReplaceALL]);
          LPropertyGroupNode.ChildNodes[LParamName].Text := LParamValue;
        end;

        // save the file to LXmlStr
        var LXmlStr: AnsiString;
        LEnvOptionsDoc.SaveToXML(LXmlStr);

        // now add the indent to the node
        LEnvOptionsDoc.Options := [doNodeAutoIndent];
        LEnvOptionsDoc.LoadFromXML(LXmlStr);
        LEnvOptionsDoc.SaveToXML(LXmlStr);

        // save the EnvOptions.Proj
        ALSaveStringToFile(cAlUTF8Bom + LXmlStr, LEnvOptionsProjFilename);

      Finally
        ALFreeAndNil(LEnvOptionsDoc);
      End;

    finally
      ALFreeAndNil(LParamLst);
    end;

  except
    on E: Exception do begin
      ALWriteln(E.ClassName+': '+E.Message, TALConsoleColor.ccRed);
      ALWriteln('');
      ALWriteln('Usage:');
      ALWriteln('  EnvOptionsProjUpdater.exe');
      ALWriteln('    -BDSVersion=BDS version');
      ALWriteln('    -Platform=OSXARM64, OSX64, iOSDevice64, iOSSimARM64, Win32, Win64, Win64x, Android64, Android, Linux64');
      ALWriteln('    -{OptionName}={OptionValue}');
      ALWriteln('');
      ALWriteln('Example:');
      ALWriteln('  EnvOptionsProjUpdater.exe^');
      ALWriteln('    -BDSVersion=23.0^');
      ALWriteln('    -Platform=iOSDevice64^');
      ALWriteln('    -ENV_PF_DevTeamIdAppStore=8I28DI72HJ^');
      ALWriteln('    -ENV_PF_AppIdentifierAppStore=8I28DI72HJ.com.my.app^');
      ALWriteln('');
      ALWriteln('');
      ALWriteln('Update failed!');
      ALWriteln('Press <Enter> key to quit');
      Readln;
      halt(1);
    end;
  end;

end.
