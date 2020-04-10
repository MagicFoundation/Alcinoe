program DprojVersioning;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.math,
  ALFiles,
  ALString;

{********************************************************************}
function getdProjVersionName(const aDProjSrc: AnsiString): ansiString;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetVersionNameFromPropertyGroup(const aPropertyGroupSrc: AnsiString; const aName: ansiString): ansiString;
  var P1, P2, P3: integer;
  begin
    P1 := ALPosExIgnoreCase(aName+'=', aPropertyGroupSrc);
    if P1 > 0 then begin
      inc(P1,length(aName+'='));
      P2 := AlPosEx(';', aPropertyGroupSrc, P1);
      if P2 <= 0 then P2 := Maxint;
      P3 := AlPosEx('<', aPropertyGroupSrc, P1);
      if P3 <= 0 then P3 := Maxint;
      P2 := Min(P2,P3);
      Result := ALTrim(AlCopyStr(aPropertyGroupSrc, P1, P2-P1));
    end;
  end;

Var
  LPropertyGroupSrc: AnsiString;
  P1, P2: integer;

begin
  result := '';
  P1 := ALPosExIgnoreCase('<PropertyGroup', aDProjSrc);
  if P1 <= 0 then raise Exception.Create('Error 3D289977-95D5-4331-9ADF-96BBC53A8F94');
  while P1 > 0 do begin
    P2 := ALPosExIgnoreCase('</PropertyGroup>', aDProjSrc, P1);
    if P2 <= 0 then raise Exception.Create('Error E34F391B-1D0B-4600-B030-97C41D76F524');
    LPropertyGroupSrc := ALCopyStr(aDProjSrc, P1, P2 - P1);
    //-----
    Result := _GetVersionNameFromPropertyGroup(LPropertyGroupSrc, 'versionName');
    if result <> '' then break;
    //-----
    Result := _GetVersionNameFromPropertyGroup(LPropertyGroupSrc, 'FileVersion');
    if result <> '' then break;
    //-----
    Result := _GetVersionNameFromPropertyGroup(LPropertyGroupSrc, 'ProductVersion');
    if result <> '' then break;
    //-----
    P1 := ALPosExIgnoreCase('<PropertyGroup', aDProjSrc, P1 + 1);
  end;
  if Result = '' then
    raise Exception.Create('Version Info was not found inside the dproj. Please in project options include version information and choose auto increment build number');
end;

{*****************************************************************}
function getdProjBuildNumber(const aDProjSrc: AnsiString): integer;
Var LBuildNumberStr: ansiString;
    LBuildNumberInt: integer;
    P1, P2, P3, P4: integer;
begin
  Result := -1;
  P1 := ALPosExIgnoreCase('<PropertyGroup', aDProjSrc);
  if P1 <= 0 then raise Exception.Create('Error 5B4726FC-8713-4CA0-B349-26F1CE765278');
  while P1 > 0 do begin
    P2 := ALPosExIgnoreCase('</PropertyGroup>', aDProjSrc, P1);
    if P2 <= 0 then raise Exception.Create('Error E012416D-AE48-4589-90E5-A574A5DEEFB4');
    //-----
    P3 := ALPosExIgnoreCase('<VerInfo_Build>', aDProjSrc, P1);
    if (P3 > P1) and (P3 < P2) then begin
      inc(P3,length('<VerInfo_Build>'));
      P4 := ALPosExIgnoreCase('</VerInfo_Build>', aDProjSrc, P3);
      if P4 <= 0 then raise Exception.Create('Error C8CD8534-EC5A-46BA-B2E5-DEFBF0A51989');
      LBuildNumberStr := AlCopyStr(aDProjSrc, P3, P4-P3);
      if not ALTryStrtoInt(LBuildNumberStr, LBuildNumberInt) then raise Exception.Create('Error E26473C3-1FE9-437C-A86D-5D62E362AA46');
      result := max(result, LBuildNumberInt);
    end;
    //-----
    P1 := ALPosExIgnoreCase('<PropertyGroup', aDProjSrc, P1 + 1);
  end;
  if Result = -1 then
    raise Exception.Create('Version Info was not found inside the dproj. Please in project options include version information and choose auto increment build number');
end;

{*******************************************************************}
procedure UpdateProjMajorMinorPatchVersion(var aDProjSrc: AnsiString;
                                           const aBuildNumber: Integer;
                                           const aMajorNumber: integer;
                                           Const aMinorNumber: integer;
                                           Const aPatchOffset: integer);


  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateBuildNumberInPropertyGroup(var aPropertyGroupSrc: AnsiString);
  var P1, P2: integer;
  begin
    P1 := ALPosExIgnoreCase('<VerInfo_Build>', aPropertyGroupSrc);
    if P1 > 0 then begin
      inc(P1,length('<VerInfo_Build>'));
      P2 := ALPosExIgnoreCase('</VerInfo_Build>', aPropertyGroupSrc, P1);
      if P2 <= 0 then raise Exception.Create('Error 1194E1A4-ED12-4981-ABC8-B5BE8D781362');
      delete(aPropertyGroupSrc, P1, P2-P1);
      insert(alinttostr(aBuildNumber), aPropertyGroupSrc, P1);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateVersionCodeInPropertyGroup(var aPropertyGroupSrc: AnsiString; const aName: ansiString);
  var P1, P2, P3: integer;
  begin
    P1 := ALPosExIgnoreCase(aName+'=', aPropertyGroupSrc);
    if P1 > 0 then begin
      inc(P1,length(aName+'='));
      P2 := AlPosEx(';', aPropertyGroupSrc, P1);
      if P2 <= 0 then P2 := Maxint;
      P3 := AlPosEx('<', aPropertyGroupSrc, P1);
      if P3 <= 0 then P3 := Maxint;
      P2 := Min(P2,P3);
      delete(aPropertyGroupSrc,P1,P2-P1);
      Insert(alinttostr(aBuildNumber), aPropertyGroupSrc, P1);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateVersionNameInPropertyGroup(var aPropertyGroupSrc: AnsiString; const aName: ansiString);
  var P1, P2, P3: integer;
  begin
    P1 := ALPosExIgnoreCase(aName+'=', aPropertyGroupSrc);
    if P1 > 0 then begin
      inc(P1,length(aName+'='));
      P2 := AlPosEx(';', aPropertyGroupSrc, P1);
      if P2 <= 0 then P2 := Maxint;
      P3 := AlPosEx('<', aPropertyGroupSrc, P1);
      if P3 <= 0 then P3 := Maxint;
      P2 := Min(P2,P3);
      delete(aPropertyGroupSrc,P1,P2-P1);
      Insert(alinttostr(aMajorNumber)+ '.' + alinttostr(aMinorNumber) + '.' + alinttostr(max(0,aBuildNumber - aPatchOffset)), aPropertyGroupSrc, P1);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateXmlNodeValue(var aXmlSrc: AnsiString;
                                const aNodeName: ansiString;
                                const aAttributeName: AnsiString;
                                const aAttributeValue: AnsiString;
                                Const aNodeValue: ansiString);
  var P1, P2, P3, P4: integer;
  begin
    P1 := ALPosExIgnoreCase('<'+aNodeName, aXmlSrc);
    while P1 > 0 do begin
      P2 := AlPosEx('>', aXmlSrc, P1); // <VersionInfo Name="MajorVer">
      if P2 <= 0 then raise Exception.Create('Error 3CA9247C-6509-4D66-A200-DEE4FF844072');
      P3 := alposExIgnoreCase(aAttributeName+'="'+aAttributeValue+'"', aXmlSrc, P1);
      if (P3 > P1) and (P3 < P2) then begin
        P3 := P2 + 1;
        P4 := AlPosEx('<', aXmlSrc, P3);
        if P4 <= 0 then raise Exception.Create('Error 92AF413C-F2E1-4A69-A1EB-158C5CF95548');
        delete(aXmlSrc,P3,P4-P3);
        insert(aNodeValue, aXmlSrc, P3);
      end;
      P1 := ALPosExIgnoreCase('<'+aNodeName, aXmlSrc, P1 + 1);
    end;
  end;

Var
  LPropertyGroupSrc: AnsiString;
  P1, P2: integer;

begin
  P1 := ALPosExIgnoreCase('<PropertyGroup', aDProjSrc);
  if P1 <= 0 then raise Exception.Create('Error 165484CD-78AD-4064-9ACD-80C4ECC95E2D');
  while P1 > 0 do begin
    P2 := ALPosExIgnoreCase('</PropertyGroup>', aDProjSrc, P1);
    if P2 <= 0 then raise Exception.Create('Error CFC183DF-EE69-4E48-B78A-C8EB8DF5F85A');
    LPropertyGroupSrc := ALCopyStr(aDProjSrc, P1, P2 - P1);
    //-----
    _UpdateBuildNumberInPropertyGroup(LPropertyGroupSrc);
    _UpdateVersionCodeInPropertyGroup(LPropertyGroupSrc, 'versionCode');
    _UpdateVersionNameInPropertyGroup(LPropertyGroupSrc, 'FileVersion');
    _UpdateVersionNameInPropertyGroup(LPropertyGroupSrc, 'ProductVersion');
    _UpdateVersionNameInPropertyGroup(LPropertyGroupSrc, 'versionName');
    _UpdateVersionNameInPropertyGroup(LPropertyGroupSrc, 'CFBundleVersion');
    _UpdateVersionNameInPropertyGroup(LPropertyGroupSrc, 'CFBundleShortVersionString');
    //-----
    Delete(aDProjSrc,P1, P2-P1);
    Insert(LPropertyGroupSrc, aDProjSrc, P1);
    //-----
    P1 := ALPosExIgnoreCase('<PropertyGroup', aDProjSrc, P1 + 1);
  end;
  //-----
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfo','Name','MajorVer',alinttostr(aMajorNumber));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfo','Name','MinorVer',alinttostr(aMinorNumber));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfo','Name','Release',alinttostr(max(0,aBuildNumber - aPatchOffset)));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfo','Name','Build',alinttostr(aBuildNumber));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfoKeys','Name','FileVersion',alinttostr(aMajorNumber)+ '.' + alinttostr(aMinorNumber) + '.' + alinttostr(max(0,aBuildNumber - aPatchOffset)));
  _UpdateXmlNodeValue(aDProjSrc,'VersionInfoKeys','Name','ProductVersion',alinttostr(aMajorNumber)+ '.' + alinttostr(aMinorNumber) + '.' + alinttostr(max(0,aBuildNumber - aPatchOffset)));
end;

Var LAction: AnsiString;
    LDProjFilename: AnsiString;
    LSrcStr: AnsiString;
    LBuildNumber: integer;
    LMajorNumber: integer;
    LMinorNumber: integer;
    LPatchOffset: integer;
    LCreateBackup: Boolean;

begin

  try

    {$region 'init params'}
    LDProjFilename := ansiString(paramstr(1));
    LAction := ansiString(paramstr(2)); // getVersionName | incMajorMinorPatchVersion
    LMajorNumber := ALStrToIntDef(ansiString(paramstr(3)), 1);
    LMinorNumber := ALStrToIntDef(ansiString(paramstr(4)), 0);
    LPatchOffset := ALStrToIntDef(ansiString(paramstr(5)), 0);
    LCreateBackup := not ALSameText(ALTrim(ansiString(paramstr(6))), 'false');
    {$endregion}

    {$region 'getVersionName'}
    If ALSameText(LAction, 'getVersionName') then begin
      LSrcStr := AlGetStringFromFile(LDProjFilename);
      Writeln(getdProjVersionName(LSrcStr));
    end
    {$endregion}

    {$region 'incMajorMinorPatchVersion'}
    else If ALSameText(LAction, 'incMajorMinorPatchVersion') then begin
      LSrcStr := AlGetStringFromFile(LDProjFilename);
      LBuildNumber := getdProjBuildNumber(LSrcStr);
      UpdateProjMajorMinorPatchVersion(LSrcStr, // var aDProjSrc: AnsiString;
                                       LBuildNumber + 1, // const aBuildNumber: Integer;
                                       LMajorNumber, // const aMajorNumber: integer;
                                       LMinorNumber, // Const aMinorNumber: integer;
                                       LPatchOffset); // Const aPatchOffset: integer);
      if LCreateBackup then begin
        if ALFileExists(LDProjFilename + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [LDProjFilename + '.bak']);
        if not ALrenameFile(LDProjFilename, LDProjFilename+ '.bak') then raiseLastOsError;
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
