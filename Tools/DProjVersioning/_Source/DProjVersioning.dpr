program DProjVersioning;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.math,
  System.AnsiStrings,
  System.IOUtils,
  Alcinoe.Files,
  Alcinoe.StringList,
  Alcinoe.Common,
  Alcinoe.StringUtils;

{********************************************************************}
function getdProjVersionName(const aDProjSrc: AnsiString): ansiString;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetVersionNameFromPropertyGroup(const aPropertyGroupSrc: AnsiString; const aName: ansiString): ansiString;
  var P1, P2, P3: integer;
  begin
    P1 := ALPosIgnoreCaseA(aName+'=', aPropertyGroupSrc);
    if P1 > 0 then begin
      inc(P1,length(aName+'='));
      P2 := ALPosA(';', aPropertyGroupSrc, P1);
      if P2 <= 0 then P2 := Maxint;
      P3 := ALPosA('<', aPropertyGroupSrc, P1);
      if P3 <= 0 then P3 := Maxint;
      P2 := Min(P2,P3);
      Result := ALTrim(AlCopyStr(aPropertyGroupSrc, P1, P2-P1));
    end;
  end;

Var
  LPropertyGroupSrc: AnsiString;
  LStringList: TALStringListA;
  P1, P2: integer;
  I: integer;

begin
  result := '';
  P1 := ALPosIgnoreCaseA('<PropertyGroup', aDProjSrc);
  if P1 <= 0 then raise Exception.Create('Error 3D289977-95D5-4331-9ADF-96BBC53A8F94');
  while P1 > 0 do begin
    P2 := ALPosIgnoreCaseA('</PropertyGroup>', aDProjSrc, P1);
    if P2 <= 0 then raise Exception.Create('Error E34F391B-1D0B-4600-B030-97C41D76F524');
    LPropertyGroupSrc := ALCopyStr(aDProjSrc, P1, P2 - P1);
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
    P1 := ALPosIgnoreCaseA('<PropertyGroup', aDProjSrc, P1 + 1);
  end;
  if Result = '' then raise Exception.Create('Version Info was not found inside the dproj. Please in project options include version information and choose auto increment build number');
  LStringList := TALStringListA.Create;
  try
    LStringList.LineBreak := '.';
    LStringList.Text := ALTrim(Result);
    Result := '';
    for I := 0 to Min(2, LStringList.Count - 1) do
      Result := result + ALIfThenA(i > 0, '.') + LStringList[i];
  finally
    ALFreeAndNil(LStringList);
  end;
end;

{*****************************************************************}
function getdProjBuildNumber(const aDProjSrc: AnsiString): integer;
Var LBuildNumberStr: ansiString;
    LBuildNumberInt: integer;
    P1, P2, P3, P4: integer;
begin
  Result := -1;
  P1 := ALPosIgnoreCaseA('<PropertyGroup', aDProjSrc);
  if P1 <= 0 then raise Exception.Create('Error 5B4726FC-8713-4CA0-B349-26F1CE765278');
  while P1 > 0 do begin
    P2 := ALPosIgnoreCaseA('</PropertyGroup>', aDProjSrc, P1);
    if P2 <= 0 then raise Exception.Create('Error E012416D-AE48-4589-90E5-A574A5DEEFB4');
    //-----
    P3 := ALPosIgnoreCaseA('<VerInfo_Build>', aDProjSrc, P1);
    if (P3 > P1) and (P3 < P2) then begin
      inc(P3,length('<VerInfo_Build>'));
      P4 := ALPosIgnoreCaseA('</VerInfo_Build>', aDProjSrc, P3);
      if P4 <= 0 then raise Exception.Create('Error C8CD8534-EC5A-46BA-B2E5-DEFBF0A51989');
      LBuildNumberStr := AlCopyStr(aDProjSrc, P3, P4-P3);
      if not ALTryStrtoInt(LBuildNumberStr, LBuildNumberInt) then raise Exception.Create('Error E26473C3-1FE9-437C-A86D-5D62E362AA46');
      result := max(result, LBuildNumberInt);
    end;
    //-----
    P1 := ALPosIgnoreCaseA('<PropertyGroup', aDProjSrc, P1 + 1);
  end;
  if Result = -1 then
    raise Exception.Create('Version Info was not found inside the dproj. Please in project options include version information and choose auto increment build number');
end;

{*****************************************}
procedure UpdateProjMajorMinorPatchVersion(
            var aDProjSrc: AnsiString;
            const aBuildNumber: Integer;
            const aMajorNumber: integer;
            Const aMinorNumber: integer;
            Const aPatchOffset: integer);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateNameValueInSrcString(
              var aStrSrc: AnsiString;
              const aName: ansiString;
              const aValue: ansiString);
  var P1, P2, P3: integer;
  begin
    P1 := ALPosIgnoreCaseA(aName+'=', aStrSrc);
    if P1 > 0 then begin
      inc(P1,length(aName+'='));
      P2 := ALPosA(';', aStrSrc, P1);
      if P2 <= 0 then P2 := Maxint;
      P3 := ALPosA('<', aStrSrc, P1);
      if P3 <= 0 then P3 := Maxint;
      P2 := Min(P2,P3);
      delete(aStrSrc,P1,P2-P1);
      Insert(aValue, aStrSrc, P1);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateXmlNodeValue(
              var aXmlSrc: AnsiString;
              const aName: ansiString;
              const aValue: ansiString); overload;
  var P1, P2: integer;
  begin
    P1 := ALPosIgnoreCaseA('<'+aName+'>', aXmlSrc);
    if P1 > 0 then begin
      inc(P1,length('<'+aName+'>'));
      P2 := ALPosIgnoreCaseA('</'+aName+'>', aXmlSrc, P1);
      if P2 <= 0 then raise Exception.Create('Error 1194E1A4-ED12-4981-ABC8-B5BE8D781362');
      delete(aXmlSrc, P1, P2-P1);
      insert(aValue, aXmlSrc, P1);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateXmlNodeValue(
              var aXmlSrc: AnsiString;
              const aNodeName: ansiString;
              const aAttributeName: AnsiString;
              const aAttributeValue: AnsiString;
              Const aNodeValue: ansiString); overload;
  var P1, P2, P3, P4: integer;
  begin
    P1 := ALPosIgnoreCaseA('<'+aNodeName, aXmlSrc);
    while P1 > 0 do begin
      P2 := ALPosA('>', aXmlSrc, P1); // <VersionInfo Name="MajorVer">
      if P2 <= 0 then raise Exception.Create('Error 3CA9247C-6509-4D66-A200-DEE4FF844072');
      P3 := ALPosIgnoreCaseA(aAttributeName+'="'+aAttributeValue+'"', aXmlSrc, P1);
      if (P3 > P1) and (P3 < P2) then begin
        P3 := ALPosA('/>', aXmlSrc, P1); // <VersionInfo Name="MajorVer"/>
        if P3 = P2 - 1 then begin
          delete(aXmlSrc,P3,2);
          insert('>' + aNodeValue+'</'+aNodeName + '>', aXmlSrc, P3);
        end
        else begin
          P3 := P2 + 1;
          P4 := ALPosA('<', aXmlSrc, P3);
          if P4 <= 0 then raise Exception.Create('Error 92AF413C-F2E1-4A69-A1EB-158C5CF95548');
          delete(aXmlSrc,P3,P4-P3);
          insert(aNodeValue, aXmlSrc, P3);
        end;
      end;
      P1 := ALPosIgnoreCaseA('<'+aNodeName, aXmlSrc, P1 + 1);
    end;
  end;

Var
  LPropertyGroupSrc: AnsiString;
  P1, P2: integer;

begin
  P1 := ALPosIgnoreCaseA('<PropertyGroup', aDProjSrc);
  if P1 <= 0 then raise Exception.Create('Error 165484CD-78AD-4064-9ACD-80C4ECC95E2D');
  while P1 > 0 do begin
    P2 := ALPosIgnoreCaseA('</PropertyGroup>', aDProjSrc, P1);
    if P2 <= 0 then raise Exception.Create('Error CFC183DF-EE69-4E48-B78A-C8EB8DF5F85A');
    LPropertyGroupSrc := ALCopyStr(aDProjSrc, P1, P2 - P1);
    //-----
    _UpdateXmlNodeValue(        LPropertyGroupSrc, 'VerInfo_MajorVer',           ALIntToStrA(aMajorNumber));
    _UpdateXmlNodeValue(        LPropertyGroupSrc, 'VerInfo_MinorVer',           ALIntToStrA(aMinorNumber));
    _UpdateXmlNodeValue(        LPropertyGroupSrc, 'VerInfo_Release',            ALIntToStrA(max(0,aBuildNumber - aPatchOffset)));
    _UpdateXmlNodeValue(        LPropertyGroupSrc, 'VerInfo_Build',              ALIntToStrA(aBuildNumber));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'versionCode',                ALIntToStrA(aBuildNumber));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'versionName',                ALIntToStrA(aMajorNumber)+ '.' + ALIntToStrA(aMinorNumber) + '.' + ALIntToStrA(max(0,aBuildNumber - aPatchOffset)));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'CFBundleVersion',            ALIntToStrA(aMajorNumber)+ '.' + ALIntToStrA(aMinorNumber) + '.' + ALIntToStrA(max(0,aBuildNumber - aPatchOffset)));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'CFBundleShortVersionString', ALIntToStrA(aMajorNumber)+ '.' + ALIntToStrA(aMinorNumber) + '.' + ALIntToStrA(max(0,aBuildNumber - aPatchOffset)));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'FileVersion',                ALIntToStrA(aMajorNumber)+ '.' + ALIntToStrA(aMinorNumber) + '.' + ALIntToStrA(max(0,aBuildNumber - aPatchOffset)) + '.' + ALIntToStrA(aBuildNumber));
    _UpdateNameValueInSrcString(LPropertyGroupSrc, 'ProductVersion',             ALIntToStrA(aMajorNumber)+ '.' + ALIntToStrA(aMinorNumber) + '.' + ALIntToStrA(max(0,aBuildNumber - aPatchOffset)) + '.' + ALIntToStrA(aBuildNumber));
    //-----
    Delete(aDProjSrc,P1, P2-P1);
    Insert(LPropertyGroupSrc, aDProjSrc, P1);
    //-----
    P1 := ALPosIgnoreCaseA('<PropertyGroup', aDProjSrc, P1 + 1);
  end;
  //-----
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfo',    'Name','MajorVer',       ALIntToStrA(aMajorNumber));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfo',    'Name','MinorVer',       ALIntToStrA(aMinorNumber));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfo',    'Name','Release',        ALIntToStrA(max(0,aBuildNumber - aPatchOffset)));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfo',    'Name','Build',          ALIntToStrA(aBuildNumber));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfoKeys','Name','FileVersion',    ALIntToStrA(aMajorNumber)+ '.' + ALIntToStrA(aMinorNumber) + '.' + ALIntToStrA(max(0,aBuildNumber - aPatchOffset)) + '.' + ALIntToStrA(aBuildNumber));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfoKeys','Name','ProductVersion', ALIntToStrA(aMajorNumber)+ '.' + ALIntToStrA(aMinorNumber) + '.' + ALIntToStrA(max(0,aBuildNumber - aPatchOffset)) + '.' + ALIntToStrA(aBuildNumber));
end;

begin

  try

    {$region 'init params'}
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);
    var LDProjFilename: String;
    var LAction: AnsiString;
    var LMajorNumber: integer;
    var LMinorNumber: integer;
    var LPatchOffset: integer;
    var LCreateBackup: Boolean;
    var LParamLst := TALStringListW.Create;
    try
      for var I := 1 to ParamCount do
        LParamLst.Add(ParamStr(i));
      LDProjFilename := ALTrim(LParamLst.Values['-DProj']);
      LAction := ansiString(LParamLst.Values['-Action']); // getVersionName | incMajorMinorPatchVersion | decMajorMinorPatchVersion
      LMajorNumber := ALStrToIntDef(LParamLst.Values['-MajorNumber'], 1);
      LMinorNumber := ALStrToIntDef(LParamLst.Values['-MinorNumber'], 0);
      LPatchOffset := ALStrToIntDef(LParamLst.Values['-PatchOffset'], 0);
      LCreateBackup := not ALSameTextW(ALTrim(LParamLst.Values['-CreateBackup']), 'false');
    finally
      ALFreeAndNil(LParamLst);
    end;
    if LDProjFilename = '' then begin
      LDProjFilename := ALTrim(paramstr(1));
      LAction := ansiString(paramstr(2)); // getVersionName | incMajorMinorPatchVersion | decMajorMinorPatchVersion
      LMajorNumber := ALStrToIntDef(ansiString(paramstr(3)), 1);
      LMinorNumber := ALStrToIntDef(ansiString(paramstr(4)), 0);
      LPatchOffset := ALStrToIntDef(ansiString(paramstr(5)), 0);
      LCreateBackup := not ALSameTextW(ALTrim(paramstr(6)), 'false');
    end;
    if LDProjFilename = '' then raise Exception.Create('Usage: DProjVersioning.exe -DProj="<DprojFilename>" -Action=<getVersionName/incMajorMinorPatchVersion/decMajorMinorPatchVersion> -MajorNumber=<x> -MinorNumber=<y> -PatchOffset=<z> -CreateBackup=<true/false>');
    {$endregion}

    {$region 'getVersionName'}
    If ALSameTextA(LAction, 'getVersionName') then begin
      var LSrcStr := AlGetStringFromFile(LDProjFilename);
      Writeln(getdProjVersionName(LSrcStr));
    end
    {$endregion}

    {$region 'incMajorMinorPatchVersion'}
    else If ALSameTextA(LAction, 'incMajorMinorPatchVersion') then begin
      var LSrcStr := AlGetStringFromFile(LDProjFilename);
      var LBuildNumber := getdProjBuildNumber(LSrcStr);
      UpdateProjMajorMinorPatchVersion(
        LSrcStr, // var aDProjSrc: AnsiString;
        LBuildNumber + 1, // const aBuildNumber: Integer;
        LMajorNumber, // const aMajorNumber: integer;
        LMinorNumber, // Const aMinorNumber: integer;
        LPatchOffset); // Const aPatchOffset: integer);
      if LCreateBackup then begin
        if Tfile.exists(LDProjFilename + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [LDProjFilename + '.bak']);
        Tfile.move(LDProjFilename, LDProjFilename+ '.bak');
      end;
      AlSaveStringToFile(LSrcStr, LDProjFilename);
    end
    {$endregion}

    {$region 'decMajorMinorPatchVersion'}
    else If ALSameTextA(LAction, 'decMajorMinorPatchVersion') then begin
      var LSrcStr := AlGetStringFromFile(LDProjFilename);
      var LBuildNumber := getdProjBuildNumber(LSrcStr);
      UpdateProjMajorMinorPatchVersion(
        LSrcStr, // var aDProjSrc: AnsiString;
        max(0, LBuildNumber - 1), // const aBuildNumber: Integer;
        LMajorNumber, // const aMajorNumber: integer;
        LMinorNumber, // Const aMinorNumber: integer;
        LPatchOffset); // Const aPatchOffset: integer);
      if LCreateBackup then begin
        if Tfile.exists(LDProjFilename + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [LDProjFilename + '.bak']);
        Tfile.Move(LDProjFilename, LDProjFilename+ '.bak');
      end;
      AlSaveStringToFile(LSrcStr, LDProjFilename);
    end
    {$endregion}

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;

end.
