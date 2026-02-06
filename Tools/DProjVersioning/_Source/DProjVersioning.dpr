program DProjVersioning;

{$APPTYPE CONSOLE}

{$R *.res}

{$I ..\..\..\Source\Alcinoe.inc}

uses
  System.SysUtils,
  System.math,
  System.AnsiStrings,
  System.IOUtils,
  Alcinoe.FileUtils,
  Alcinoe.StringList,
  Alcinoe.Common,
  Alcinoe.StringUtils;

{********************************************************************}
function GetDProjVersionName(const ADProjSrc: AnsiString): AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetVersionNameFromPropertyGroup(const APropertyGroupSrc: AnsiString; const AName: AnsiString): AnsiString;
  begin
    var P1 := ALPosIgnoreCaseA(AName+'=', APropertyGroupSrc);
    if P1 > 0 then begin
      inc(P1,length(AName+'='));
      var P2 := ALPosA(';', APropertyGroupSrc, P1);
      if P2 <= 0 then P2 := Maxint;
      var P3 := ALPosA('<', APropertyGroupSrc, P1);
      if P3 <= 0 then P3 := Maxint;
      P2 := Min(P2,P3);
      Result := ALTrim(AlCopyStr(APropertyGroupSrc, P1, P2-P1));
    end;
  end;

begin
  result := '';
  var P1 := ALPosIgnoreCaseA('<PropertyGroup', ADProjSrc);
  if P1 <= 0 then raise Exception.Create('Error 3D289977-95D5-4331-9ADF-96BBC53A8F94');
  while P1 > 0 do begin
    var P2 := ALPosIgnoreCaseA('</PropertyGroup>', ADProjSrc, P1);
    if P2 <= 0 then raise Exception.Create('Error E34F391B-1D0B-4600-B030-97C41D76F524');
    var LPropertyGroupSrc := ALCopyStr(ADProjSrc, P1, P2 - P1);
    //-----
    Result := _GetVersionNameFromPropertyGroup(LPropertyGroupSrc, 'versionName');
    if result <> '' then break;
    //-----
    Result := _GetVersionNameFromPropertyGroup(LPropertyGroupSrc, 'CFBundleVersion');
    if result <> '' then break;
    //-----
    Result := _GetVersionNameFromPropertyGroup(LPropertyGroupSrc, 'CFBundleShortVersionString');
    if result <> '' then break;
    //-----
    Result := _GetVersionNameFromPropertyGroup(LPropertyGroupSrc, 'FileVersion');
    if result <> '' then break;
    //-----
    Result := _GetVersionNameFromPropertyGroup(LPropertyGroupSrc, 'ProductVersion');
    if result <> '' then break;
    //-----
    P1 := ALPosIgnoreCaseA('<PropertyGroup', ADProjSrc, P1 + 1);
  end;
  if Result = '' then raise Exception.Create('Version Info was not found inside the dproj. Please in project options include version information.');
  var LStringList := TALStringListA.Create;
  try
    LStringList.LineBreak := '.';
    LStringList.Text := ALTrim(Result);
    Result := '';
    for var I := 0 to Min(2, LStringList.Count - 1) do
      Result := result + ALIfThenA(i > 0, '.') + LStringList[i];
  finally
    ALFreeAndNil(LStringList);
  end;
end;

{*****************************************************************}
function GetDProjBuildNumber(const ADProjSrc: AnsiString): integer;
begin
  Result := -1;
  var P1 := ALPosIgnoreCaseA('<PropertyGroup', ADProjSrc);
  if P1 <= 0 then raise Exception.Create('Error 5B4726FC-8713-4CA0-B349-26F1CE765278');
  while P1 > 0 do begin
    var P2 := ALPosIgnoreCaseA('</PropertyGroup>', ADProjSrc, P1);
    if P2 <= 0 then raise Exception.Create('Error E012416D-AE48-4589-90E5-A574A5DEEFB4');
    //-----
    var P3 := ALPosIgnoreCaseA('<VerInfo_Build>', ADProjSrc, P1);
    if (P3 > P1) and (P3 < P2) then begin
      inc(P3,length('<VerInfo_Build>'));
      var P4 := ALPosIgnoreCaseA('</VerInfo_Build>', ADProjSrc, P3);
      if P4 <= 0 then raise Exception.Create('Error C8CD8534-EC5A-46BA-B2E5-DEFBF0A51989');
      var LBuildNumberStr := AlCopyStr(ADProjSrc, P3, P4-P3);
      var LBuildNumberInt: integer;
      if not ALTryStrtoInt(LBuildNumberStr, LBuildNumberInt) then raise Exception.Create('Error E26473C3-1FE9-437C-A86D-5D62E362AA46');
      result := max(result, LBuildNumberInt);
    end;
    //-----
    P1 := ALPosIgnoreCaseA('<PropertyGroup', ADProjSrc, P1 + 1);
  end;
  if Result = -1 then
    raise Exception.Create('Version Info was not found inside the dproj. Please in project options include version information.');
end;

{*****************************************}
procedure UpdateProjMajorMinorPatchVersion(
            var ADProjSrc: AnsiString;
            const ABuildNumber: Integer;
            const AMajorNumber: integer;
            Const AMinorNumber: integer;
            Const AReleaseNumber: integer);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateNameValueInSrcString(
              var AStrSrc: AnsiString;
              const AName: AnsiString;
              const AValue: AnsiString);
  begin
    var P1 := ALPosIgnoreCaseA(AName+'=', AStrSrc);
    if P1 > 0 then begin
      inc(P1,length(AName+'='));
      var P2 := ALPosA(';', AStrSrc, P1);
      if P2 <= 0 then P2 := Maxint;
      var P3 := ALPosA('<', AStrSrc, P1);
      if P3 <= 0 then P3 := Maxint;
      P2 := Min(P2,P3);
      delete(AStrSrc,P1,P2-P1);
      Insert(AValue, AStrSrc, P1);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateXmlNodeValue(
              var AXmlSrc: AnsiString;
              const AName: AnsiString;
              const AValue: AnsiString); overload;
  begin
    var P1 := ALPosIgnoreCaseA('<'+AName+'>', AXmlSrc);
    if P1 > 0 then begin
      inc(P1,length('<'+AName+'>'));
      var P2 := ALPosIgnoreCaseA('</'+AName+'>', AXmlSrc, P1);
      if P2 <= 0 then raise Exception.Create('Error 1194E1A4-ED12-4981-ABC8-B5BE8D781362');
      delete(AXmlSrc, P1, P2-P1);
      insert(AValue, AXmlSrc, P1);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateXmlNodeValue(
              var AXmlSrc: AnsiString;
              const ANodeName: AnsiString;
              const AAttributeName: AnsiString;
              const AAttributeValue: AnsiString;
              Const ANodeValue: AnsiString); overload;
  begin
    var P1 := ALPosIgnoreCaseA('<'+ANodeName, AXmlSrc);
    while P1 > 0 do begin
      var P2 := ALPosA('>', AXmlSrc, P1); // <VersionInfo Name="MajorVer">
      if P2 <= 0 then raise Exception.Create('Error 3CA9247C-6509-4D66-A200-DEE4FF844072');
      var P3 := ALPosIgnoreCaseA(AAttributeName+'="'+AAttributeValue+'"', AXmlSrc, P1);
      if (P3 > P1) and (P3 < P2) then begin
        P3 := ALPosA('/>', AXmlSrc, P1); // <VersionInfo Name="MajorVer"/>
        if P3 = P2 - 1 then begin
          delete(AXmlSrc,P3,2);
          insert('>' + ANodeValue+'</'+ANodeName + '>', AXmlSrc, P3);
        end
        else begin
          P3 := P2 + 1;
          var P4 := ALPosA('<', AXmlSrc, P3);
          if P4 <= 0 then raise Exception.Create('Error 92AF413C-F2E1-4A69-A1EB-158C5CF95548');
          delete(AXmlSrc,P3,P4-P3);
          insert(ANodeValue, AXmlSrc, P3);
        end;
      end;
      P1 := ALPosIgnoreCaseA('<'+ANodeName, AXmlSrc, P1 + 1);
    end;
  end;

begin
  var P1 := ALPosIgnoreCaseA('<PropertyGroup', ADProjSrc);
  if P1 <= 0 then raise Exception.Create('Error 165484CD-78AD-4064-9ACD-80C4ECC95E2D');
  while P1 > 0 do begin
    var P2 := ALPosIgnoreCaseA('</PropertyGroup>', ADProjSrc, P1);
    if P2 <= 0 then raise Exception.Create('Error CFC183DF-EE69-4E48-B78A-C8EB8DF5F85A');
    var LPropertyGroupSrc := ALCopyStr(ADProjSrc, P1, P2 - P1);
    //-----
    _UpdateXmlNodeValue(        LPropertyGroupSrc, 'VerInfo_MajorVer',           ALIntToStrA(AMajorNumber));
    _UpdateXmlNodeValue(        LPropertyGroupSrc, 'VerInfo_MinorVer',           ALIntToStrA(AMinorNumber));
    _UpdateXmlNodeValue(        LPropertyGroupSrc, 'VerInfo_Release',            ALIntToStrA(AReleaseNumber));
    _UpdateXmlNodeValue(        LPropertyGroupSrc, 'VerInfo_Build',              ALIntToStrA(ABuildNumber));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'versionCode',                ALIntToStrA(ABuildNumber));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'versionName',                ALIntToStrA(AMajorNumber)+ '.' + ALIntToStrA(AMinorNumber) + '.' + ALIntToStrA(AReleaseNumber));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'CFBundleVersion',            ALIntToStrA(AMajorNumber)+ '.' + ALIntToStrA(AMinorNumber) + '.' + ALIntToStrA(AReleaseNumber));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'CFBundleShortVersionString', ALIntToStrA(AMajorNumber)+ '.' + ALIntToStrA(AMinorNumber) + '.' + ALIntToStrA(AReleaseNumber));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'FileVersion',                ALIntToStrA(AMajorNumber)+ '.' + ALIntToStrA(AMinorNumber) + '.' + ALIntToStrA(AReleaseNumber) + '.0');
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'ProductVersion',             ALIntToStrA(AMajorNumber)+ '.' + ALIntToStrA(AMinorNumber) + '.' + ALIntToStrA(AReleaseNumber) + '.0');
    //-----
    Delete(ADProjSrc,P1, P2-P1);
    Insert(LPropertyGroupSrc, ADProjSrc, P1);
    //-----
    P1 := ALPosIgnoreCaseA('<PropertyGroup', ADProjSrc, P1 + 1);
  end;
  //-----
  _UpdateXmlNodeValue(ADProjSrc,'VersionInfo',    'Name','MajorVer',       ALIntToStrA(AMajorNumber));
  _UpdateXmlNodeValue(ADProjSrc,'VersionInfo',    'Name','MinorVer',       ALIntToStrA(AMinorNumber));
  _UpdateXmlNodeValue(ADProjSrc,'VersionInfo',    'Name','Release',        ALIntToStrA(AReleaseNumber));
  _UpdateXmlNodeValue(ADProjSrc,'VersionInfo',    'Name','Build',          ALIntToStrA(ABuildNumber));
  _UpdateXmlNodeValue(ADProjSrc,'VersionInfoKeys','Name','FileVersion',    ALIntToStrA(AMajorNumber)+ '.' + ALIntToStrA(AMinorNumber) + '.' + ALIntToStrA(AReleaseNumber) + '.0');
  _UpdateXmlNodeValue(ADProjSrc,'VersionInfoKeys','Name','ProductVersion', ALIntToStrA(AMajorNumber)+ '.' + ALIntToStrA(AMinorNumber) + '.' + ALIntToStrA(AReleaseNumber) + '.0');
end;

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
      var LDProjFilename := ALTrim(LParamLst.Values['-DProj']);
      if LDProjFilename = '' then raise Exception.Create('Usage: DProjVersioning.exe -DProj="<DprojFilename>" -Action=<GetVersionName/IncMajorMinorPatchVersion> -MajorNumber=<x> -MinorNumber=<y> -PatchBase=<z> -CreateBackup=<true/false>');
      var LSrcStr := AlGetStringFromFile(LDProjFilename);

      // -GetVersionName
      If LParamLst.IndexOf('-GetVersionName') >= 0 then begin
        Writeln(GetDProjVersionName(LSrcStr));
      end

      // -GetVersionInfo
      else If LParamLst.IndexOf('-GetVersionInfo') >= 0 then begin
        Writeln(GetDProjVersionName(LSrcStr) + '+' + ALIntTostrA(GetDProjBuildNumber(LSrcStr)));
      end

      // -SetVersionInfo
      else If LParamLst.values['-SetVersionInfo'] <> '' then begin
        var LVersionInfo := ALTrim(LParamLst.values['-SetVersionInfo']); // 2.0.5+190
        var P1 := ALposW('+', LVersionInfo);
        If P1 <= 0 Then raise Exception.CreateFmt('Invalid value for -SetVersionInfo: "%s". Expected format: <VersionName>+<BuildNumber> (example: 2.0.5+190).', [LVersionInfo]);
        var LBuildNumber: Integer;
        if not ALTryStrToInt(ALTrim(ALCopyStr(LVersionInfo, P1+1, ALMaxInt)), LBuildNumber) then // 190
          raise Exception.CreateFmt('Invalid value for -SetVersionInfo: "%s". Expected format: <VersionName>+<BuildNumber> (example: 2.0.5+190).', [LVersionInfo]);
        var LVersionName := ALTrim(ALCopyStr(LVersionInfo, 1, P1-1)); // 2.0.5
        var LLst := TALStringListW.Create;
        try
          LLst.LineBreak := '.';
          LLst.Text := LVersionName;
          if LLst.Count <> 3 then raise Exception.CreateFmt('Invalid value for -SetVersionInfo: "%s". Expected format: <VersionName>+<BuildNumber> (example: 2.0.5+190).', [LVersionInfo]);
          var LMajorNumber: integer;
          if not ALTryStrToInt(ALTrim(LLst[0]), LMajorNumber) then raise Exception.CreateFmt('Invalid value for -SetVersionInfo: "%s". Expected format: <VersionName>+<BuildNumber> (example: 2.0.5+190).', [LVersionInfo]);
          var LMinorNumber: integer;
          if not ALTryStrToInt(ALTrim(LLst[1]), LMinorNumber) then raise Exception.CreateFmt('Invalid value for -SetVersionInfo: "%s". Expected format: <VersionName>+<BuildNumber> (example: 2.0.5+190).', [LVersionInfo]);
          var LReleaseNumber: integer;
          if not ALTryStrToInt(ALTrim(LLst[2]), LReleaseNumber) then raise Exception.CreateFmt('Invalid value for -SetVersionInfo: "%s". Expected format: <VersionName>+<BuildNumber> (example: 2.0.5+190).', [LVersionInfo]);
          UpdateProjMajorMinorPatchVersion(
            LSrcStr, // var ADProjSrc: AnsiString;
            LBuildNumber, // const ABuildNumber: Integer;
            LMajorNumber, // const AMajorNumber: integer;
            LMinorNumber, // Const AMinorNumber: integer;
            LReleaseNumber); // Const APatchBase: integer);
        finally
          ALFreeAndNil(LLst);
        end;
        if not ALSameTextW(ALTrim(LParamLst.Values['-CreateBackup']), 'false') then begin
          if Tfile.exists(LDProjFilename + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [LDProjFilename + '.bak']);
          Tfile.move(LDProjFilename, LDProjFilename+ '.bak');
        end;
        // Workaround: AlSaveStringToFile occasionally fails with:
        //   EFCreateError: Cannot create file "MyProject.dproj".
        //   The process cannot access the file because it is being used by another process.
        // Pause briefly to give the lock time to clear (e.g., Git or another background tool
        // may still be touching the file).
        For var i := 1 to 100 do begin
          try
            AlSaveStringToFile(LSrcStr, LDProjFilename);
          Except
            on E: Exception do begin
              if i >= 100 then Raise;
              Sleep(100);
            end;
          end;
          Break;
        end;
      end

      // -IncVersion
      else If LParamLst.IndexOf('-IncVersion') >= 0 then begin
        var LBuildNumber := GetDProjBuildNumber(LSrcStr);
        var LVersionName := String(GetDProjVersionName(LSrcStr));
        var LLst := TALStringListW.Create;
        try
          LLst.LineBreak := '.';
          LLst.Text := LVersionName;
          if LLst.Count <> 3 then raise Exception.CreateFmt('Invalid VersionName "%s". Expected x.y.z (e.g. 2.0.5)', [LVersionName]);
          var LMajorNumber: integer;
          if not ALTryStrToInt(ALTrim(LLst[0]), LMajorNumber) then raise Exception.CreateFmt('Invalid VersionName "%s". Expected x.y.z (e.g. 2.0.5)', [LVersionName]);
          var LMinorNumber: integer;
          if not ALTryStrToInt(ALTrim(LLst[1]), LMinorNumber) then raise Exception.CreateFmt('Invalid VersionName "%s". Expected x.y.z (e.g. 2.0.5)', [LVersionName]);
          var LReleaseNumber: integer;
          if not ALTryStrToInt(ALTrim(LLst[2]), LReleaseNumber) then raise Exception.CreateFmt('Invalid VersionName "%s". Expected x.y.z (e.g. 2.0.5)', [LVersionName]);
          var LOverriddenMajorNumber := ALStrToIntDef(LParamLst.Values['-MajorNumber'], LMajorNumber);
          var LOverriddenMinorNumber := ALStrToIntDef(LParamLst.Values['-MinorNumber'], LMinorNumber);
          if (LOverriddenMajorNumber <> LMajorNumber) or (LOverriddenMinorNumber <> LMinorNumber) then LReleaseNumber := -1;
          UpdateProjMajorMinorPatchVersion(
            LSrcStr, // var ADProjSrc: AnsiString;
            LBuildNumber+1, // const ABuildNumber: Integer;
            LOverriddenMajorNumber, // const AMajorNumber: integer;
            LOverriddenMinorNumber, // Const AMinorNumber: integer;
            LReleaseNumber+1); // Const AReleaseNumber: integer);
        finally
          ALFreeAndNil(LLst);
        end;
        if not ALSameTextW(ALTrim(LParamLst.Values['-CreateBackup']), 'false') then begin
          if Tfile.exists(LDProjFilename + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [LDProjFilename + '.bak']);
          Tfile.move(LDProjFilename, LDProjFilename+ '.bak');
        end;
        // Workaround: AlSaveStringToFile occasionally fails with:
        //   EFCreateError: Cannot create file "MyProject.dproj".
        //   The process cannot access the file because it is being used by another process.
        // Pause briefly to give the lock time to clear (e.g., Git or another background tool
        // may still be touching the file).
        For var i := 1 to 100 do begin
          try
            AlSaveStringToFile(LSrcStr, LDProjFilename);
          Except
            on E: Exception do begin
              if i >= 100 then Raise;
              Sleep(100);
            end;
          end;
          Break;
        end;
      end

      // Error
      Else
        raise Exception.Create('No action specified. Use -GetVersionName, -GetVersionInfo, -SetVersionInfo="<VersionName>+<BuildNumber>", or -IncVersion');

    finally
      ALFreeAndNil(LParamLst);
    end;

  except
    on E: Exception do begin
      ALWriteln(E.ClassName+': '+E.Message, TALConsoleColor.ccRed);
      Writeln('');
      Writeln('Usage:');
      Writeln('  DProjVersioning.exe');
      Writeln('    -DProj="<file.dproj>"');
      Writeln('    -GetVersionName | -GetVersionInfo | -IncVersion | -SetVersionInfo="<VersionName>+<BuildNumber>"');
      Writeln('    [-CreateBackup=false]');
      Writeln('');
      Writeln('Examples:');
      Writeln('  DProjVersioning.exe -DProj="c:\MyApp\MyApp.dproj" -GetVersionInfo');
      Writeln('  DProjVersioning.exe -DProj="c:\MyApp\MyApp.dproj" -SetVersionInfo "2.0.5+190"');
      Writeln('  DProjVersioning.exe -DProj="c:\MyApp\MyApp.dproj" -IncVersion -CreateBackup=false');
      Writeln('');
      Writeln('DProjVersioning failed!');
      Writeln('Press <Enter> key to quit');
      Readln;
      halt(1);
    end;
  end;

end.