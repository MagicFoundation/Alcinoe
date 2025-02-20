program DeployProjNormalizer;

{$APPTYPE CONSOLE}

{$R *.res}

{$I ..\..\..\Source\Alcinoe.inc}

uses
  System.AnsiStrings,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  system.Math,
  Alcinoe.XMLDoc,
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.Files,
  Alcinoe.StringUtils;

//
// This process will create from the dproj a new deployproj file from scratch,
// orders all nodes and performs the following actions:
//
// * Add all <DeployFile> nodes found in the dproj
//
// * Add all <RcItem> nodes found in the dproj
//
// * Add all files which may not necessarily be present in
//   <DeployFile> or <RcItem>:
//   * Add iOS resource files:
//       iOS_AppStore1024
//       iPhone_AppIcon120
//       iPhone_AppIcon180
//       iPad_AppIcon152
//       iPad_AppIcon167
//       iPhone_Spotlight80
//       iPhone_Spotlight120
//       iPad_SpotLight80
//       iPhone_Setting58
//       iPhone_Setting87
//       iPad_Setting58
//       iPhone_Notification40
//       iPhone_Notification60
//       iPad_Notification40
//       iPhone_Launch2x
//       iPhone_LaunchDark2x
//       iPhone_Launch3x
//       iPhone_LaunchDark3x
//       iPad_Launch2x
//       iPad_LaunchDark2x
//   * Do NOT add Android resource files. Thoses files must
//     be added via the AndroidMerger tool:
//       Android_Strings
//       Android_Colors
//       Android_ColorsDark
//       AndroidSplashImageDef
//       AndroidSplashImageDefV21
//       AndroidSplashStyles
//       AndroidSplashStylesV21
//       AndroidSplashStylesV31
//       Android_AdaptiveIcon
//       Android_AdaptiveIconV33
//       Android_VectorizedNotificationIcon
//       Android_AdaptiveIconMonochrome
//       Android_AdaptiveIconForeground
//       Android_AdaptiveIconBackground
//       Android_VectorizedSplash
//       Android_VectorizedSplashDark
//       Android_VectorizedSplashV31
//       Android_VectorizedSplashV31Dark
//       Android_LauncherIcon36
//       Android_LauncherIcon48
//       Android_LauncherIcon72
//       Android_LauncherIcon96
//       Android_LauncherIcon144
//       Android_LauncherIcon192
//       Android_SplashImage426
//       Android_SplashImage470
//       Android_SplashImage640
//       Android_SplashImage960
//       Android_NotificationIcon24
//       Android_NotificationIcon36
//       Android_NotificationIcon48
//       Android_NotificationIcon72
//       Android_NotificationIcon96
//   * do NOT add the AndroidGDBServer (Android debugger for debugging 32bit Android applications)
//   * Do NOT add the AndroidLibnativeMipsFile (to show the message "Application does not support this device" at startup)
//   * Do NOT add the AndroidLibnativeArmeabiFile/AndroidLibnativeArmeabiv7aFile (to show the message "Application does not support this device" at startup)
//   * Add the ProjectOutput/ProjectOutput_Android32 (The main binary)
//   * Add the Skia (Skia Binary)
//   * Add the ProjectAndroidManifest (The Android manifest file)
//   * Add the AndroidClasses (This file contains the Java libraries that the application uses)
//   * Add the AndroidClassesDexFile (This file contains the Java libraries that the application uses)
//   * Add the ProjectOSXDebug (Debugging Information)
//   * Add the ProjectOSXEntitlements (Key-value pairs that grant an executable permission to use a service or technology)
//   * Add the ProjectOSXInfoPList (Info.plist file to store configuration data)
//   * Add the ProjectOSXResource (Icns_MainIcns)
//   * Add the ProjectiOSDeviceDebug (Debugging Information)
//   * Add the ProjectiOSEntitlements (Key-value pairs that grant an executable permission to use a service or technology)
//   * Add the ProjectiOSInfoPList (Info.plist file to store configuration data)
//   * Add the ProjectiOSLaunchScreen (launch screen)
//   * Do NOT add any Win32 files
//   * Do NOT add any Win64 files
//

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if the structure of the *.deployproj didn''t changed by following the instructions in References\BlankApplication\README.md'}
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


{**********************************************************************************}
function _DeployProjToXMLString(const aDeployProjXmlDoc: TalXmlDocument):AnsiString;
begin

  //order the node
  _SortAttributesByNodeName(aDeployProjXmlDoc.DocumentElement);
  _SortChildNodesByNodeNameAndAttributes(aDeployProjXmlDoc.DocumentElement);

  //save the file to LXmlStr
  aDeployProjXmlDoc.SaveToXML(Result);

  //now add the indent to the node
  aDeployProjXmlDoc.Options := [doNodeAutoIndent];
  aDeployProjXmlDoc.LoadFromXML(Result);
  aDeployProjXmlDoc.SaveToXML(Result);

end;

{***********************}
function _getProjectRoot(
           const aProjectName: ansiString;
           const aPlatFormName: AnsiString): ansiString;
begin
  result := aProjectName +
            ALifThenA(
              //----
              (aPlatformName = 'iOSDevice64') or
              (aPlatformName = 'iOSSimulator') or
              (aPlatformName = 'iOSSimARM64') or
              (aPlatformName = 'OSX64') or
              (aPlatformName = 'OSXARM64'),
              //----
              '.app');
end;

{********************}
function _getProperty(
           const aProperties: TALStringListA; // contain items like Cfg_1_iOSDevice64#xxxx=yyyyy
           const aConfigs: TALStringListA; // contain items like Cfg_1=Debug
           const aPlatFormName: ansiString;
           const aConfigName: AnsiString;
           const aPropertyName: ansiString): ansiString;
begin
  var LConfigKey := aConfigs.Values[aConfigName]; // Cfg_1
                    result := aProperties.Values[LConfigKey+'_'+aPlatFormName+'#'+aPropertyName]; // Cfg_1_iOSDevice64#xxxx
  if result='' then result := aProperties.Values[LConfigKey+'#'+aPropertyName];                   // Cfg_1#xxxx
  if result='' then result := aProperties.Values['Base_'+aPlatFormName+'#'+aPropertyName];        // Base_iOSDevice64#xxxx
  if result='' then result := aProperties.Values['Base#'+aPropertyName];                          // Base#xxxx
end;

{************************}
function _getExeOutputDir(
           const aProperties: TALStringListA; // contain items like Cfg_1_iOSDevice64=..\..\$(Platform)\$(Config)
           const aConfigs: TALStringListA; // contain items like Cfg_1=Debug
           const aPlatFormName: ansiString;
           const aConfigName: AnsiString): ansiString;
begin
  result := _getProperty(
              aProperties, // const aProperties: TALStringListA; // contain items like Cfg_1_iOSDevice64=..\..\$(Platform)\$(Config)
              aConfigs, // const aConfigs: TALStringListA; // contain items like Cfg_1=Debug
              aPlatFormName, // const aPlatFormName: ansiString;
              aConfigName, // const aConfigName: AnsiString
              'DCC_ExeOutput'); // const aPropertyName: String;
  result := ALStringReplaceA(result, '$(Platform)', aPlatFormName, [rfIgnoreCase, RfReplaceALL]);
  result := ALStringReplaceA(result, '$(Config)', aConfigName, [rfIgnoreCase, RfReplaceALL]);
  if (result <> '') and (result[high(result)] <> '\') then result := result + '\';
  if ALPosA('.\', result) = 1 then delete(Result, 1, 2);
end;

{***********************}
Procedure _addDeployFile(
            const aAlreadyDeployedFiles: TALStringListA;
            const aItemGroupNode: TalXmlNode;
            const aConfigName: AnsiString;
            const aCondition: AnsiString;
            const aInclude: AnsiString;
            const aDeployClass: AnsiString;
            const aLocalCommand: AnsiString;
            const aOperation: AnsiString;
            aOverwrite: AnsiString;
            const aRemoteCommand: AnsiString;
            const aRemoteDir: AnsiString;
            const aRemoteName: AnsiString;
            aRequired: AnsiString;
            const aEnabled: Boolean = True); overload;
Begin
  var LPlatFormName := aItemGroupNode.Attributes['Condition']; // '$(Platform)'=='Android'
  //-----
  if (aInclude <> '') then begin
    Var LKey := LPlatFormName+'#'+aConfigName+'#'+aCondition+'#'+aInclude; // '$(Platform)'=='Android'#Debug#'$(SKIADIR)'!=''#..\..\Android64\Debug\splash_image_def.xml
    if aAlreadyDeployedFiles.IndexOf(LKey) >= 0 then exit;
    if not aEnabled then aAlreadyDeployedFiles.Add(LKey);
  end;
  //-----
  if (aRemoteDir <> '') and (aRemoteName <> '') then begin
    Var LRemoteDir := aRemoteDir;
    while (LRemoteDir <> '') and (LRemoteDir[low(LRemoteDir)] = '\') do delete(LRemoteDir,low(LRemoteDir),1);
    while (LRemoteDir <> '') and (LRemoteDir[high(LRemoteDir)] = '\') do delete(LRemoteDir,high(LRemoteDir),1);
    Var LRemoteName := aRemoteName;
    while (LRemoteName <> '') and (LRemoteName[low(LRemoteName)] = '\') do delete(LRemoteName,low(LRemoteName),1);
    while (LRemoteName <> '') and (LRemoteName[high(LRemoteName)] = '\') do delete(LRemoteName,high(LRemoteName),1);
    Var LKey := LPlatFormName+'#'+aConfigName+'#'+aCondition+'#'+aDeployClass+'#'+LRemoteDir+'#'+LRemoteName; // '$(Platform)'=='Android'#Debug#'$(SKIADIR)'!=''#File#res\drawable-mdpi-v4#abc_scrubber_control_off_mtrl_alpha.png
    if aAlreadyDeployedFiles.IndexOf(LKey) >= 0 then exit;
    aAlreadyDeployedFiles.Add(LKey);
  end;
  //-----
  if not aEnabled then exit;
  //-----
  Var LDeployFileNode := aItemGroupNode.AddChild('DeployFile');
  if (aConfigName <> '') and (aCondition <> '') then LDeployFileNode.Attributes['Condition'] := '''$(Config)''=='''+aConfigName+''' And ' + aCondition
  else if (aConfigName <> '') then LDeployFileNode.Attributes['Condition'] := '''$(Config)''=='''+aConfigName+''''
  else if (aCondition <> '') then LDeployFileNode.Attributes['Condition'] := aCondition;
  if aInclude <> '' then LDeployFileNode.Attributes['Include'] := aInclude;
  with LDeployFileNode.AddChild('DeployClass') do
    if aDeployClass <> '' then Text := aDeployClass;
  with LDeployFileNode.AddChild('LocalCommand') do
    if aLocalCommand <> '' then Text := aLocalCommand;
  with LDeployFileNode.AddChild('Operation') do
    if aOperation <> '' then Text := aOperation;
  with LDeployFileNode.AddChild('Overwrite') do
    if aOverwrite <> '' then begin
      aOverwrite := ALLowercase(aOverwrite);
      aOverwrite[low(aOverwrite)] := ALUpcase(aOverwrite[low(aOverwrite)]);
      Text := aOverwrite;
    end;
  with LDeployFileNode.AddChild('RemoteCommand') do
    if aRemoteCommand <> '' then Text := aRemoteCommand;
  with LDeployFileNode.AddChild('RemoteDir') do
    if aRemoteDir <> '' then text := aRemoteDir;
  with LDeployFileNode.AddChild('RemoteName') do
    if aRemoteName <> '' then text := aRemoteName;
  If (aRequired = '') and (ALSameTextA(aDeployClass, 'ProjectOutput')) then aRequired := 'True';
  if aRequired <> '' then begin
    aRequired := ALLowercase(aRequired);
    aRequired[low(aRequired)] := ALUpcase(aRequired[low(aRequired)]);
    with LDeployFileNode.AddChild('Required') do
      text := aRequired;
  end;
End;

{***********************}
Procedure _addDeployFile(
            const aAlreadyDeployedFiles: TALStringListA;
            const aProperties: TALStringListA;
            const aConfigs: TALStringListA;
            const aPlatFormName: ansiString;
            const aConfigName: AnsiString;
            const aPropertyName: ansiString;
            const aItemGroupNode: TalXmlNode;
            const aDeployClass: AnsiString;
            const aLocalCommand: AnsiString;
            const aOperation: AnsiString;
            aOverwrite: AnsiString;
            const aRemoteCommand: AnsiString;
            const aRemoteDir: AnsiString;
            const aRemoteName: AnsiString;
            aRequired: AnsiString;
            const aEnabled: Boolean = True); overload;
Begin
  var LInclude := _getProperty(
                    aProperties, // const aProperties: TALStringListA;
                    aconfigs, //const aConfigs: TALStringListA;
                    aPlatFormName, // const aPlatFormName: ansiString;
                    aConfigName, // const aConfigName: AnsiString;
                    aPropertyName); // const aPropertyName: ansiString
  if Linclude <> '' then begin
    var LRemoteName := aRemoteName;
    if LRemoteName = '' then LRemoteName := ALExtractFileName(Linclude);
    _addDeployFile(
      aAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
      aItemGroupNode, // const aItemGroupNode: TalXmlNode;
      aConfigName, // const aConfigName: AnsiString;
      '', // const aCondition: AnsiString;
      LInclude, // const aInclude: AnsiString;
      aDeployClass, // const aDeployClass: AnsiString;
      aLocalCommand, // const aLocalCommand: AnsiString;
      aOperation, // const aOperation: AnsiString;
      aOverwrite, // aOverwrite: AnsiString;
      aRemoteCommand, // const aRemoteCommand: AnsiString;
      aRemoteDir, // const aRemoteDir: AnsiString;
      LRemoteName, // const aRemoteName: AnsiString;
      aRequired, // aRequired: AnsiString;
      aEnabled); // const aEnabled: Boolean = True
  end;
End;

begin

  try

    //Init project params
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    //init LDProjFilename / LDeployProjFilename / LProjectName / LOnlySort
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
    if LDProjFilename = '' then raise Exception.Create('Usage: DeployProjNormalizer.exe -DProj="<DprojFilename>" -CreateBackup=<true/false>');
    var LProjectName := ALExtractFileName(AnsiString(LDProjFilename), true{RemoveFileExt});
    var LDeployProjFilename := ALExtractFilePath(LDProjFilename) + String(LProjectName) + '.deployproj';

    //create the LDProjXmlDoc / LDeployProjXmlDoc
    var LDProjXmlDoc := TALXmlDocument.Create('Project');
    var LDeployProjXmlDoc := TALXmlDocument.Create('Project');
    var LPlatforms := TALStringListA.Create;
    var LConfigs := TALStringListA.Create;
    var LProperties := TALStringListA.Create;
    var LAlreadyDeployedFiles := TALStringListA.Create;
    Try

      //init LAlreadyDeployedFiles
      LAlreadyDeployedFiles.Sorted := True;
      LAlreadyDeployedFiles.Duplicates := TDuplicates.DupError;

      //load the LDProjXmlDoc
      LDProjXmlDoc.Options := [];
      LDProjXmlDoc.ParseOptions := [];
      LDProjXmlDoc.LoadFromFile(LDProjFilename);

      //init LDeployProjXmlDoc
      LDeployProjXmlDoc.Options := [];
      LDeployProjXmlDoc.ParseOptions := [];

      //remove the Prolog node <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
      LDeployProjXmlDoc.ChildNodes.Delete(0);

      //<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      LDeployProjXmlDoc.DocumentElement.Attributes['xmlns'] := LDProjXmlDoc.DocumentElement.Attributes['xmlns'];

      //<Import Condition="Exists(&#39;$(BDS)\bin\CodeGear.Deployment.targets&#39;)" Project="$(BDS)\bin\CodeGear.Deployment.targets"/>
      Var LDeployProjImportNode := LDeployProjXmlDoc.DocumentElement.AddChild('Import');
      LDeployProjImportNode.Attributes['Project'] := '$(BDS)\bin\CodeGear.Deployment.targets';
      LDeployProjImportNode.Attributes['Condition'] := 'Exists(''$(BDS)\bin\CodeGear.Deployment.targets'')';

      //init LProjectExtensionsNode
      var LProjectExtensionsNode := LDProjXmlDoc.DocumentElement.ChildNodes.FindNode('ProjectExtensions');
      if LProjectExtensionsNode = nil then raise Exception.Create('ProjectExtensions node not found!');

      //init LBorlandProjectNode
      var LBorlandProjectNode := LProjectExtensionsNode.ChildNodes.FindNode('BorlandProject');
      if LBorlandProjectNode = nil then raise Exception.Create('ProjectExtensions.BorlandProject node not found!');

      //init LDeploymentNode
      var LDeploymentNode := LBorlandProjectNode.ChildNodes.FindNode('Deployment');
      if LDeploymentNode = nil then raise Exception.Create('ProjectExtensions.BorlandProject.Deployment node not found!');

      //check version attribute <Deployment Version="4">
      //if LDeploymentNode.Attributes['Version'] <> '4' then
      //  raise Exception.Create('ProjectExtensions.BorlandProject.Deployment.Version is not in the expected value (4)');

      //init LItemGroupNode
      var LItemGroupNode := LDProjXmlDoc.DocumentElement.ChildNodes.FindNode('ItemGroup');
      if LItemGroupNode = nil then raise Exception.Create('ItemGroup node not found!');

      //init LPlatforms
      var LPlatformsNode := LBorlandProjectNode.ChildNodes.FindNode('Platforms');
      if LPlatformsNode = nil then raise Exception.Create('Platforms node not found!');
      for var I := 0 to LPlatformsNode.ChildNodes.Count - 1 do begin
        var LPlatformNode := LPlatformsNode.ChildNodes[i];
        if not ALSameTextA(LPlatformNode.Text, 'True') then continue;
        Var LplatformName := LPlatformNode.Attributes['value'];
        if LplatformName = '' then raise Exception.Create('Error D76FB179-4F0D-4CBF-93CA-E3D41E58C273');
        var LDeployProjItemGroupNode := LDeployProjXmlDoc.DocumentElement.AddChild('ItemGroup');
        LDeployProjItemGroupNode.Attributes['Condition'] := '''$(Platform)''=='''+LplatformName+'''';
        LPlatforms.AddObject(LplatformName, LDeployProjItemGroupNode);
      end;

      //init Lconfigs
      for var I := 0 to LItemGroupNode.ChildNodes.Count - 1 do begin
        var LBuildConfigurationNode := LItemGroupNode.ChildNodes[i];
        if not ALSameTextA(LBuildConfigurationNode.NodeName, 'BuildConfiguration') then continue;
        var LConfigName := LBuildConfigurationNode.Attributes['Include'];
        if LConfigName = '' then raise Exception.Create('Error 316E8622-C0D6-482A-AC18-3166F461A9CA');
        if ALSameTextA(LConfigName, 'Base') then continue;
        var LKeyNode := LBuildConfigurationNode.ChildNodes.FindNode('Key');
        if LKeyNode = nil then raise Exception.Create('Error 12A4543B-527B-45B4-B87D-D9CD04D66568');
        Lconfigs.AddNameValue(LConfigName,LKeyNode.Text); // Debug=Cfg_1
      end;

      //init LProperties
      var LProjectVersion: Single := 0;
      for var I := 0 to LDProjXmlDoc.DocumentElement.ChildNodes.Count - 1 do begin
        Var LPropertyGroupNode := LDProjXmlDoc.DocumentElement.ChildNodes[i];
        if (LPropertyGroupNode.NodeName = 'PropertyGroup') and
           (LPropertyGroupNode.attributes['Condition'] <> '') then begin
          var Lcondition := LPropertyGroupNode.attributes['Condition']; // '$(Base_iOSDevice64)'!=''
          Lcondition := ALStringReplaceA(Lcondition,'''$(','',[]); // // Base_iOSDevice64)'!=''
          Lcondition := ALStringReplaceA(Lcondition,')''!=''''','',[]); // // Base_iOSDevice64
          for var J := 0 to LPropertyGroupNode.ChildNodes.Count - 1 do begin
            var LPropertyNode := LPropertyGroupNode.ChildNodes[J]; // <Icns_MainIcns>icons\ico_round_16_32_48_128_256_512.icns</Icns_MainIcns>
            if LPropertyNode.Text <> '' then LProperties.AddNameValue(Lcondition+'#'+LPropertyNode.NodeName, LPropertyNode.Text); // Cfg_1_iOSDevice64#Icns_MainIcns=icons\ico_round_16_32_48_128_256_512.icns
          end;
        end
        else if (LPropertyGroupNode.NodeName = 'PropertyGroup') and
                (LPropertyGroupNode.attributes['Condition'] = '') then begin
          Var LProjectVersionNode := LPropertyGroupNode.ChildNodes.FindNode('ProjectVersion');
          //if (LProjectVersionNode = nil) or
          //   ((LProjectVersionNode.Text <> '19.2' {Sydney}) and
          //    (LProjectVersionNode.Text <> '19.5' {Alexandria})) then
          //  raise Exception.Create('Unsupported Delphi compiler. Please consult Readme.txt');
          LProjectVersion := ALStrToFloat(LProjectVersionNode.Text, ALDefaultFormatSettingsA);
        end;
      end;
      if SameValue(LProjectVersion, 0) then
        raise Exception.Create('Unsupported Delphi compiler. Please consult Readme.txt');

      //init LSkiaEnabled
      var LSkiaEnabled := ALPosIgnoreCaseA(';SKIA;',';'+LProperties.Values['Base#DCC_Define']+';') > 0;

      //Merge DeployClass in DeployFile
      for var I := LDeploymentNode.ChildNodes.Count - 1 downto 0 do begin
        var LDeployClassNode := LDeploymentNode.ChildNodes[i];
        if LDeployClassNode.NodeName = 'DeployClass' then begin
          var LName := LDeployClassNode.Attributes['Name'];
          for var J := 0 to LDeploymentNode.ChildNodes.Count - 1 do begin
            var LDeployFileNode := LDeploymentNode.ChildNodes[J];
            if (LDeployFileNode.NodeName = 'DeployFile') and
               (ALSameTextA(LDeployFileNode.attributes['Class'], LName)) then begin
              for var k := 0 to LDeployClassNode.ChildNodes.Count - 1 do begin
                var LDeployClassPlatformNode := LDeployClassNode.ChildNodes[K];
                if LDeployClassPlatformNode.NodeName <> 'Platform' then
                  raise Exception.Create('Error 81F1D6C6-B472-4C10-8CB1-50EC2DB047D6');
                var LDeployClassPlatformName := LDeployClassPlatformNode.Attributes['Name'];
                if LDeployClassPlatformName = '' then raise Exception.Create('Error 0D6C4FAB-145C-4799-A8C4-4313C5125CCA');
                for var L := 0 to LDeployFileNode.ChildNodes.Count - 1 do begin
                  var LDeployFilePlatformNode := LDeployFileNode.ChildNodes[L];
                  if LDeployFilePlatformNode.NodeName <> 'Platform' then
                    raise Exception.Create('Error D6497915-23CC-4D0D-9855-774741D74886');
                  var LDeployFilePlatformName := LDeployFilePlatformNode.Attributes['Name'];
                  if LDeployFilePlatformName = '' then raise Exception.Create('Error CC0A9209-4B03-4E9A-A543-B0B92371880E');
                  if ALSameTextA(LDeployFilePlatformName, LDeployClassPlatformName) then begin
                    for var M := 0 to LDeployClassPlatformNode.ChildNodes.Count - 1 do begin
                      var LNodeName := LDeployClassPlatformNode.ChildNodes[m].NodeName;
                      if LDeployFilePlatformNode.ChildNodes.FindNode(LNodeName) = nil then
                        LDeployFilePlatformNode.AddChild(LNodeName).Text := LDeployClassPlatformNode.ChildNodes[m].Text;
                    end;
                    var LRequired := LDeployClassNode.Attributes['Required'];
                    if (LRequired <> '') and
                       (LDeployFilePlatformNode.ChildNodes.FindNode('Required') = nil) then
                      LDeployFilePlatformNode.AddChild('Required').Text := LRequired;
                  end;
                end;
              end;
            end;
          end;
          LDeploymentNode.ChildNodes.Delete(i);
        end;
      end;

      //add ProjectExtensions
      var LProjectFileVersionNode := LProjectExtensionsNode.ChildNodes.FindNode('ProjectFileVersion');
      if (LProjectFileVersionNode = nil) or (LProjectFileVersionNode.Text <> '12') then
        raise Exception.Create('ProjectExtensions.ProjectFileVersion is not in the expected value (12)');
      LDeployProjXmlDoc.DocumentElement.AddChild('ProjectExtensions').AddChild('ProjectFileVersion').Text := LProjectFileVersionNode.Text;

      //loop on all Deployment
      for var I := 0 to LDeploymentNode.ChildNodes.Count - 1 do begin

        //From DPROJ:
        //-----------
        //<DeployFile Class="File" Configuration="Debug" LocalName="..\..\..\..\..\Alcinoe\Libraries\jar\org.webrtc\jni\arm64-v8a\libjingle_peerconnection_so.so">
        //  <Platform Name="Android">
        //    <Overwrite>true</Overwrite>
        //    <RemoteDir>library\lib\arm64-v8a\</RemoteDir>
        //    <RemoteName>libjingle_peerconnection_so.so</RemoteName>
        //  </Platform>
        //  <Platform Name="Android64">
        //    <Overwrite>true</Overwrite>
        //    <RemoteDir>library\lib\arm64-v8a\</RemoteDir>
        //    <RemoteName>libjingle_peerconnection_so.so</RemoteName>
        //  </Platform>
        //</DeployFile>
        //
        //
        //To DEPLOYPROJ:
        //--------------
        //<DeployFile Include="..\..\..\..\..\Alcinoe\Libraries\jar\org.webrtc\jni\arm64-v8a\libjingle_peerconnection_so.so" Condition="''$(Config)''==''Debug''">
        //  <DeployClass>File</DeployClass>
        //  <LocalCommand/>
        //  <Operation>0</Operation>
        //  <Overwrite>True</Overwrite>
        //  <RemoteCommand/>
        //  <RemoteDir>\library\lib\arm64-v8a\</RemoteDir>
        //  <RemoteName>libjingle_peerconnection_so.so</RemoteName>
        //</DeployFile>

        //init LDeployFileNode
        var LDeployFileNode := LDeploymentNode.ChildNodes[i];

        //loop on all <Platform Name="xxx">
        for var j := 0 to LDeployFileNode.ChildNodes.Count - 1 do begin

          //<Platform Name="Android64">
          var LPlatformNode := LDeployFileNode.ChildNodes[J];
          if LPlatformNode.NodeName <> 'Platform' then
            raise Exception.Create('Error C752BC06-AA98-432B-B731-96C480A3829C');
          var LPlatformName := LPlatformNode.Attributes['Name'];
          if LPlatformName = '' then raise Exception.Create('Error 092D559B-B524-4F46-A2EC-3575DA2D401A');
          Var LIndex := Lplatforms.IndexOf(LPlatformName);
          if LIndex < 0 then continue;
          var LDeployProjItemGroupNode := TALXmlNode(Lplatforms.Objects[LIndex]);

          //<Overwrite>true</Overwrite>
          Var LOverwrite: AnsiString := 'True';
          var LOverwriteNode := LPlatformNode.ChildNodes.FindNode('Overwrite');
          if (LOverwriteNode <> nil) then LOverwrite := LOverwriteNode.Text;

          //<Operation>1</Operation>
          Var LOperation: AnsiString := '0';
          var LOperationNode := LPlatformNode.ChildNodes.FindNode('Operation');
          if (LOperationNode <> nil) then LOperation := LOperationNode.Text;

          //<RemoteDir>library\lib\arm64-v8a\</RemoteDir>
          Var LRemoteDir: AnsiString := '';
          var LPlatformRemoteDirNode := LPlatformNode.ChildNodes.FindNode('RemoteDir');
          if LPlatformRemoteDirNode <> nil then LRemoteDir := LPlatformRemoteDirNode.Text;
          if ALPosA('.\', LRemoteDir) = 1 then delete(LRemoteDir, 1, 1);
          if (LRemoteDir <> '') and (LRemoteDir[low(LRemoteDir)] <> '\') then LRemoteDir := '\' + LRemoteDir;
          if (LRemoteDir <> '') and (LRemoteDir[high(LRemoteDir)] <> '\') then LRemoteDir := LRemoteDir + '\';
          if (LRemoteDir = '') then LRemoteDir := '\';
          LRemoteDir := _getProjectRoot(LProjectName, LPlatformName) + LRemoteDir;

          //<RemoteName>libjingle_peerconnection_so.so</RemoteName>
          var LRemoteName := ALExtractFileName(LDeployFileNode.Attributes['LocalName']);
          var LPlatformRemoteNameNode := LPlatformNode.ChildNodes.FindNode('RemoteName');
          if LPlatformRemoteNameNode <> nil then LRemoteName := LPlatformRemoteNameNode.Text;

          //<Enabled>false</Enabled>
          var LPlatformEnabledNode := LPlatformNode.ChildNodes.FindNode('Enabled');
          var LEnabled := (LPlatformEnabledNode = nil) or (not ALSameTextA(LPlatformEnabledNode.Text, 'false'));

          //create LDeployProjDeployFileNode
          _addDeployFile(
            LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
            LDeployProjItemGroupNode, // const aItemGroupNode: TalXmlNode;
            LDeployFileNode.Attributes['Configuration'], // const aConfigName: AnsiString;
            LDeployFileNode.Attributes['Condition'], // const aCondition: AnsiString;
            LDeployFileNode.Attributes['LocalName'], // const aInclude: String;
            LDeployFileNode.Attributes['Class'], // const aDeployClass: String;
            '', // const aLocalCommand: String;
            LOperation, // const aOperation: String;
            LOverwrite, // aOverwrite: String;
            '', // const aRemoteCommand: String;
            LRemoteDir, // const aRemoteDir: String;
            LRemoteName, // const aRemoteName: String);
            LDeployFileNode.Attributes['Required'], // aRequired: AnsiString;
            LEnabled); // aEnabled: Boolean

        end;

      end;

      //loop on all ItemGroup
      for var I := 0 to LItemGroupNode.ChildNodes.Count - 1 do begin

        //From DPROJ:
        //-----------
        //<RcItem Include="resources\stopwatch_viewed_white_120x120.png">
        //  <ResourceId>stopwatch_viewed_white_120x120</ResourceId>
        //  <ResourceType>RCDATA</ResourceType>
        //</RcItem>
        //
        //
        //To DEPLOYPROJ:
        //--------------
        //<DeployFile Condition="''$(Config)''==''Release''" Include="resources\stopwatch_viewed_white_120x120.png">
        //  <DeployClass>ProjectFile</DeployClass>
        //  <LocalCommand/>
        //  <Operation>0</Operation>
        //  <Overwrite>True</Overwrite>
        //  <RemoteCommand/>
        //  <RemoteDir>sample\assets\internal\</RemoteDir>
        //  <RemoteName>stopwatch_viewed_white_120x120.png</RemoteName>
        //</DeployFile>

        //init LDeployFileNode
        var LRcItemNode := LItemGroupNode.ChildNodes[i];
        if LRcItemNode.NodeName <> 'RcItem' then continue;

        //create LDeployProjDeployFileNode
        for var J := 0 to LplatForms.Count - 1 do
          for var K := 0 to LConfigs.Count - 1 do begin
            var LPlatformName := LplatForms[J];
            var LRemoteDir: AnsiString;
            if ALSameTextA(LPlatformName, 'iOSDevice64') or
               ALSameTextA(LPlatformName, 'iOSSimulator') or
               ALSameTextA(LplatFormName, 'iOSSimARM64') then LRemoteDir := _getProjectRoot(LProjectName, LPlatformName) + '\StartUp\Documents\'
            else if ALSameTextA(LPlatformName, 'OSX64') or
                    ALSameTextA(LPlatformName, 'OSXARM64') then LRemoteDir := _getProjectRoot(LProjectName, LPlatformName) + '\Contents\Resources\StartUp\'
            else if ALSameTextA(LPlatformName, 'Android') or
                    ALSameTextA(LPlatformName, 'Android64') then LRemoteDir := _getProjectRoot(LProjectName, LPlatformName) + '\assets\internal\'
            else continue;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[J]), // const aItemGroupNode: TalXmlNode;
              LConfigs.Names[k], // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              LRcItemNode.Attributes['Include'], // const aInclude: String;
              'ProjectFile', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '0', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              LRemoteDir, // const aRemoteDir: String;
              ALExtractFileName(LRcItemNode.Attributes['Include']), // const aRemoteName: String);
              ''); // aRequired: AnsiString
          end;
      end;

      //Deploy all mandatory files, which may not necessarily be present in Deployment/ItemGroup.
      for var I := 0 to LplatForms.Count - 1 do begin
        var LplatFormName := LplatForms[i];
        for var J := 0 to LConfigs.Count - 1 do begin
          var LConfigName := LConfigs.Names[j];

          if ALSameTextA(LplatFormName, 'iOSDevice64') or
             ALSameTextA(LplatFormName, 'iOSSimulator') or
             ALSameTextA(LplatFormName, 'iOSSimARM64') then begin

            //<iOS_AppStore1024>$(BDS)\bin\Artwork\iOS\iPhone\FM_ApplicationIcon_1024x1024.png</iOS_AppStore1024>
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iOS_AppStore1024', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iOS_AppStore1024', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;

            //<iPhone_AppIcon120>$(BDS)\bin\Artwork\iOS\iPhone\FM_ApplicationIcon_120x120.png</iPhone_AppIcon120>
            //<iPhone_AppIcon180>$(BDS)\bin\Artwork\iOS\iPhone\FM_ApplicationIcon_180x180.png</iPhone_AppIcon180>
            //<iPad_AppIcon152>$(BDS)\bin\Artwork\iOS\iPad\FM_ApplicationIcon_152x152.png</iPad_AppIcon152>
            //<iPad_AppIcon167>$(BDS)\bin\Artwork\iOS\iPad\FM_ApplicationIcon_167x167.png</iPad_AppIcon167>
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_AppIcon120', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_AppIcon120', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_AppIcon180', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_AppIcon180', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPad_AppIcon152', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPad_AppIcon152', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPad_AppIcon167', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPad_AppIcon167', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;

            //<iPhone_Spotlight80>$(BDS)\bin\Artwork\iOS\iPhone\FM_SpotlightSearchIcon_80x80.png</iPhone_Spotlight80>
            //<iPhone_Spotlight120>$(BDS)\bin\Artwork\iOS\iPhone\FM_SpotlightSearchIcon_120x120.png</iPhone_Spotlight120>
            //<iPad_SpotLight80>$(BDS)\bin\Artwork\iOS\iPad\FM_SpotlightSearchIcon_80x80.png</iPad_SpotLight80>
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_Spotlight80', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_Spotlight80', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_Spotlight120', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_Spotlight120', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPad_SpotLight80', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPad_SpotLight80', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;

            //<iPhone_Setting58>$(BDS)\bin\Artwork\iOS\iPhone\FM_SettingIcon_58x58.png</iPhone_Setting58>
            //<iPhone_Setting87>$(BDS)\bin\Artwork\iOS\iPhone\FM_SettingIcon_87x87.png</iPhone_Setting87>
            //<iPad_Setting58>$(BDS)\bin\Artwork\iOS\iPad\FM_SettingIcon_58x58.png</iPad_Setting58>
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_Setting58', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_Setting58', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_Setting87', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_Setting87', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPad_Setting58', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPad_Setting58', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;

            //<iPhone_Notification40>$(BDS)\bin\Artwork\iOS\iPhone\FM_NotificationIcon_40x40.png</iPhone_Notification40>
            //<iPhone_Notification60>$(BDS)\bin\Artwork\iOS\iPhone\FM_NotificationIcon_60x60.png</iPhone_Notification60>
            //<iPad_Notification40>$(BDS)\bin\Artwork\iOS\iPad\FM_NotificationIcon_40x40.png</iPad_Notification40>
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_Notification40', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_Notification40', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_Notification60', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_Notification60', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPad_Notification40', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPad_Notification40', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\AppIcon.appiconset\', // const aRemoteDir: String;
              '', // const aRemoteName: String;
              ''); // aRequired: AnsiString;


            //<iPhone_Launch2x>$(BDS)\bin\Artwork\iOS\iPhone\FM_LaunchImage_2x.png</iPhone_Launch2x>
            //<iPhone_LaunchDark2x>$(BDS)\bin\Artwork\iOS\iPhone\FM_LaunchImageDark_2x.png</iPhone_LaunchDark2x>
            //<iPhone_Launch3x>$(BDS)\bin\Artwork\iOS\iPhone\FM_LaunchImage_3x.png</iPhone_Launch3x>
            //<iPhone_LaunchDark3x>$(BDS)\bin\Artwork\iOS\iPhone\FM_LaunchImageDark_3x.png</iPhone_LaunchDark3x>
            //<iPad_Launch2x>$(BDS)\bin\Artwork\iOS\iPad\FM_LaunchImage_2x.png</iPad_Launch2x>
            //<iPad_LaunchDark2x>$(BDS)\bin\Artwork\iOS\iPad\FM_LaunchImageDark_2x.png</iPad_LaunchDark2x>
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_Launch2x', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_Launch2x', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\LaunchScreenImage.imageset\', // const aRemoteDir: String;
              'FM_LaunchScreenImage_iPhone@2x.png', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_LaunchDark2x', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_LaunchDark2x', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\LaunchScreenImage.imageset\', // const aRemoteDir: String;
              'FM_LaunchScreenImage_iPhoneDark@2x.png', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_Launch3x', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_Launch3x', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\LaunchScreenImage.imageset\', // const aRemoteDir: String;
              'FM_LaunchScreenImage_iPhone@3x.png', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPhone_LaunchDark3x', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPhone_LaunchDark3x', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\LaunchScreenImage.imageset\', // const aRemoteDir: String;
              'FM_LaunchScreenImage_iPhoneDark@3x.png', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPad_Launch2x', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPad_Launch2x', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\LaunchScreenImage.imageset\', // const aRemoteDir: String;
              'FM_LaunchScreenImage_iPad@2x.png', // const aRemoteName: String;
              ''); // aRequired: AnsiString;
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'iPad_LaunchDark2x', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'iPad_LaunchDark2x', // const aDeployClass: String;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\Assets\LaunchScreenImage.imageset\', // const aRemoteDir: String;
              'FM_LaunchScreenImage_iPadDark@2x.png', // const aRemoteName: String;
              ''); // aRequired: AnsiString;

          end;

          //-----------------
          //Android/Android64
          //Avoid automatically deploying all these files as they
          //conflict with the tasks performed by the AndroidMerger tool.
          //Android_Strings: Do not know how it's used, it's always empty
          //Android_Colors: Used if "use accent color" is set to something else than black in icons > notification
          //Android_ColorsDark: (added in 20.1{Athens})
          //AndroidSplashImageDef: use if "include splash images" is set in icons > splash images
          //AndroidSplashImageDefV21: (added in 20.1{Athens})
          //AndroidSplashStyles: use if "include splash images" is set in icons > splash images
          //AndroidSplashStylesV21: use if "include splash images" is set in icons > splash images
          //AndroidSplashStylesV31: (added in 20.1{Athens})
          //Android_AdaptiveIcon: (added in 20.1{Athens})
          //Android_AdaptiveIconV33: (added in 20.1{Athens})
          //Android_VectorizedNotificationIcon: (added in 20.1{Athens})
          //Android_AdaptiveIconMonochrome: (added in 20.1{Athens}) Taken from <Android_AdaptiveIconMonochrome>$(BDS)\bin\Artwork\Android\FM_AdaptiveIcon_Monochrome.xml</Android_AdaptiveIconMonochrome>
          //Android_AdaptiveIconForeground: (added in 20.1{Athens}) Taken from <Android_AdaptiveIconForeground>$(BDS)\bin\Artwork\Android\FM_AdaptiveIcon_Foreground.xml</Android_AdaptiveIconForeground>
          //Android_AdaptiveIconBackground: (added in 20.1{Athens}) Taken from <Android_AdaptiveIconBackground>$(BDS)\bin\Artwork\Android\FM_AdaptiveIcon_Background.xml</Android_AdaptiveIconBackground>
          //Android_VectorizedSplash: (added in 20.1{Athens}) Taken from <Android_VectorizedSplash>$(BDS)\bin\Artwork\Android\FM_VectorizedSplash.xml</Android_VectorizedSplash>
          //Android_VectorizedSplashDark: (added in 20.1{Athens}) Taken from <Android_VectorizedSplashDark>$(BDS)\bin\Artwork\Android\FM_VectorizedSplashDark.xml</Android_VectorizedSplashDark>
          //Android_VectorizedSplashV31: (added in 20.1{Athens}) Taken from <Android_VectorizedSplashV31>$(BDS)\bin\Artwork\Android\FM_VectorizedSplashV31.xml</Android_VectorizedSplashV31>
          //Android_VectorizedSplashV31Dark: (added in 20.1{Athens}) Taken from <Android_VectorizedSplashV31Dark>$(BDS)\bin\Artwork\Android\FM_VectorizedSplashV31Dark.xml</Android_VectorizedSplashV31Dark>
          //Android_LauncherIcon36: Taken from <Android_LauncherIcon36>$(BDS)\bin\Artwork\Android\FM_LauncherIcon_36x36.png</Android_LauncherIcon36>
          //Android_LauncherIcon48: Taken from <Android_LauncherIcon48>$(BDS)\bin\Artwork\Android\FM_LauncherIcon_48x48.png</Android_LauncherIcon48>
          //Android_LauncherIcon72: Taken from <Android_LauncherIcon72>$(BDS)\bin\Artwork\Android\FM_LauncherIcon_72x72.png</Android_LauncherIcon72>
          //Android_LauncherIcon96: Taken from <Android_LauncherIcon96>$(BDS)\bin\Artwork\Android\FM_LauncherIcon_96x96.png</Android_LauncherIcon96>
          //Android_LauncherIcon144: Taken from <Android_LauncherIcon144>$(BDS)\bin\Artwork\Android\FM_LauncherIcon_144x144.png</Android_LauncherIcon144>
          //Android_LauncherIcon192: Taken from <Android_LauncherIcon192>$(BDS)\bin\Artwork\Android\FM_LauncherIcon_192x192.png</Android_LauncherIcon192>
          //Android_SplashImage426: Taken from <Android_SplashImage426>$(BDS)\bin\Artwork\Android\FM_SplashImage_426x320.png</Android_SplashImage426>
          //Android_SplashImage470: Taken from <Android_SplashImage470>$(BDS)\bin\Artwork\Android\FM_SplashImage_470x320.png</Android_SplashImage470>
          //Android_SplashImage640: Taken from <Android_SplashImage640>$(BDS)\bin\Artwork\Android\FM_SplashImage_640x480.png</Android_SplashImage640>
          //Android_SplashImage960: Taken from <Android_SplashImage960>$(BDS)\bin\Artwork\Android\FM_SplashImage_960x720.png</Android_SplashImage960>
          //Android_NotificationIcon24: Taken from <Android_NotificationIcon24>$(BDS)\bin\Artwork\Android\FM_NotificationIcon_24x24.png</Android_NotificationIcon24>
          //Android_NotificationIcon36: Taken from <Android_NotificationIcon36>$(BDS)\bin\Artwork\Android\FM_NotificationIcon_36x36.png</Android_NotificationIcon36>
          //Android_NotificationIcon48: Taken from <Android_NotificationIcon48>$(BDS)\bin\Artwork\Android\FM_NotificationIcon_48x48.png</Android_NotificationIcon48>
          //Android_NotificationIcon72: Taken from <Android_NotificationIcon72>$(BDS)\bin\Artwork\Android\FM_NotificationIcon_72x72.png</Android_NotificationIcon72>
          //Android_NotificationIcon96: Taken from <Android_NotificationIcon96>$(BDS)\bin\Artwork\Android\FM_NotificationIcon_96x96.png</Android_NotificationIcon96>

          //-------
          //Android
          //GDB is Android debugger. Specifically, Delphi uses GDB for debugging 32bit Android applications. For debugging 64bit
          //Android applications it uses different debugger LLDB. GDB is deprecated and will be removed soon
          //https://source.android.com/devices/tech/debug/gdb
          //if (ALSameTextA(LplatFormName, 'Android')) and
          //   (ALSameTextA(LConfigName, 'Debug')) then
          //  _addDeployFile(
          //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
          //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
          //    LConfigName, // const aConfigName: AnsiString;
          //    '', // const aCondition: AnsiString;
          //    '$(NDKBasePath)\prebuilt\android-arm\gdbserver\gdbserver', // const aInclude: String;
          //    'AndroidGDBServer', // const aDeployClass: String;
          //    '', // const aLocalCommand: String;
          //    '1', // const aOperation: String;
          //    'True', // aOverwrite: String;
          //    '', // const aRemoteCommand: String;
          //    _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\armeabi-v7a\', // const aRemoteDir: String;
          //    'gdbserver', // const aRemoteName: String;
          //    ''); // aRequired: AnsiString

          //-----------------
          //Android/Android64
          //mips: code compiled for the MIPS32r1 and later architecture (32 bits, MIPS support has been removed from the Android NDK in r17).
          //this file is simply to show the message "Application does not support this device" at startup
          //if (ALSameTextA(LplatFormName, 'Android') or ALSameTextA(LplatFormName, 'Android64')) then
          //  _addDeployFile(
          //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
          //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
          //    LConfigName, // const aConfigName: AnsiString;
          //    '', // const aCondition: AnsiString;
          //    '$(BDS)\lib\android\'+ALLowerCase(LConfigName)+'\mips\libnative-activity.so', // const aInclude: String;
          //    'AndroidLibnativeMipsFile', // const aDeployClass: String;
          //    '', // const aLocalCommand: String;
          //    '1', // const aOperation: String;
          //    'True', // aOverwrite: String;
          //    '', // const aRemoteCommand: String;
          //    _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\mips\', // const aRemoteDir: String;
          //    'lib'+LProjectName+'.so', // const aRemoteName: String;
          //    ''); // aRequired: AnsiString

          //-----------------
          //Android/Android64
          //armeabi: code compiled for the old ARMv5 architecture (32 bits, ARMv5 support has been removed from Android NDK in r17)
          //this file is simply to show the message "Application does not support this device" at startup
          //if (ALSameTextA(LplatFormName, 'Android') or ALSameTextA(LplatFormName, 'Android64')) then
          //  _addDeployFile(
          //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
          //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
          //    LConfigName, // const aConfigName: AnsiString;
          //    '', // const aCondition: AnsiString;
          //    '$(BDS)\lib\android\'+ALLowerCase(LConfigName)+'\armeabi\libnative-activity.so', // const aInclude: String;
          //    'AndroidLibnativeArmeabiFile', // const aDeployClass: String;
          //    '', // const aLocalCommand: String;
          //    '1', // const aOperation: String;
          //    'True', // aOverwrite: String;
          //    '', // const aRemoteCommand: String;
          //    _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\armeabi\', // const aRemoteDir: String;
          //    'lib'+LProjectName+'.so', // const aRemoteName: String;
          //    ''); // aRequired: AnsiString

          //-----------------
          //Android/Android64
          //armeabi-v7a: compiled code for ARMv7 architecture (32 bits)
          if (ALSameTextA(LplatFormName, 'Android')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+'lib'+LProjectName+'.so', // const aInclude: String;
              'ProjectOutput', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\armeabi-v7a\', // const aRemoteDir: String;
              'lib'+LProjectName+'.so', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
          //-----
          if (ALSameTextA(LplatFormName, 'Android64')) then begin
            if ALSameTextA(
                 _getProperty(
                   LProperties, // const aProperties: TALStringListA;
                   Lconfigs, //const aConfigs: TALStringListA;
                   LPlatFormName, // const aPlatFormName: ansiString;
                   LConfigName, // const aConfigName: AnsiString;
                   'DCC_GenerateAndroidAppBundleFile'), // const aPropertyName: ansiString;
                 'True') then //const aConfigName: AnsiString)
              _addDeployFile(
                LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
                TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
                LConfigName, // const aConfigName: AnsiString;
                '', // const aCondition: AnsiString;
                _getExeOutputDir(LProperties,LConfigs,'Android',LConfigName)+'lib'+LProjectName+'.so', // const aInclude: String;
                'ProjectOutput_Android32', // const aDeployClass: String;
                '', // const aLocalCommand: String;
                '1', // const aOperation: String;
                'True', // aOverwrite: String;
                '', // const aRemoteCommand: String;
                _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\armeabi-v7a\', // const aRemoteDir: String;
                'lib'+LProjectName+'.so', // const aRemoteName: String;
                ''); // aRequired: AnsiString
            //this file is simply to show the message "Application does not support this device" at startup
            //but it's not present in the "release" directory (c:\Program Files (x86)\Embarcadero\Studio\22.0\lib\android\release\armeabi-v7a\)
            //so I prefere to skip it - https://quality.embarcadero.com/browse/RSP-40256
            //else
            //  _addDeployFile(
            //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
            //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
            //    LConfigName, // const aConfigName: AnsiString;
            //    '', // const aCondition: AnsiString;
            //    '$(BDS)\lib\android\'+ALLowerCase(LConfigName)+'\armeabi-v7a\libnative-activity.so', // const aInclude: String;
            //    'AndroidLibnativeArmeabiv7aFile', // const aDeployClass: String;
            //    '', // const aLocalCommand: String;
            //    '1', // const aOperation: String;
            //    'True', // aOverwrite: String;
            //    '', // const aRemoteCommand: String;
            //    _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\armeabi-v7a\', // const aRemoteDir: String;
            //    'lib'+LProjectName+'.so', // const aRemoteName: String;
            //    ''); // aRequired: AnsiString
          end;

          //---------
          //Android64
          //arm64-v8a: compiled code for ARMv8 architecture (64 bits)
          if (ALSameTextA(LplatFormName, 'Android64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+'lib'+LProjectName+'.so', // const aInclude: String;
              'ProjectOutput', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\arm64-v8a\', // const aRemoteDir: String;
              'lib'+LProjectName+'.so', // const aRemoteName: String;
              'true'); // aRequired: AnsiString

          //-------
          //Android
          //Skia: The compiled skia library
          if (LSkiaEnabled) and
             (ALSameTextA(LplatFormName, 'Android')) and
             (compareValue(LProjectVersion, 20.1{Athens}) = 0) then begin
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''!=''''', // const aCondition: AnsiString;
              '$(SKIADIR)\Binary\Shared\Android\libsk4d.so', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\armeabi-v7a\', // const aRemoteDir: String;
              'libsk4d.so', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''==''''', // const aCondition: AnsiString;
              '$(BDS)\binandroid32\libsk4d.so', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\armeabi-v7a\', // const aRemoteDir: String;
              'libsk4d.so', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
          end;

          //---------
          //Android64
          //Skia: The compiled skia library
          if (LSkiaEnabled) and
             (ALSameTextA(LplatFormName, 'Android64')) and
             (compareValue(LProjectVersion, 20.1{Athens}) = 0) then begin
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''!='''' and ''$(AndroidAppBundle)''==''true''', // const aCondition: AnsiString;
              '$(SKIADIR)\Binary\Shared\Android\libsk4d.so', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\armeabi-v7a\', // const aRemoteDir: String;
              'libsk4d.so', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''!=''''', // const aCondition: AnsiString;
              '$(SKIADIR)\Binary\Shared\Android64\libsk4d.so', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\arm64-v8a\', // const aRemoteDir: String;
              'libsk4d.so', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''=='''' and ''$(AndroidAppBundle)''==''true''', // const aCondition: AnsiString;
              '$(BDS)\binandroid32\libsk4d.so', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\armeabi-v7a\', // const aRemoteDir: String;
              'libsk4d.so', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''==''''', // const aCondition: AnsiString;
              '$(BDS)\binandroid64\libsk4d.so', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\library\lib\arm64-v8a\', // const aRemoteDir: String;
              'libsk4d.so', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
          end;

          //-----------------
          //Android/Android64
          //The manifest file describes essential information about your app to the Android build tools, the Android
          //operating system, and Google Play
          if (ALSameTextA(LplatFormName, 'Android') or ALSameTextA(LplatFormName, 'Android64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+ 'AndroidManifest.xml', // const aInclude: String;
              'ProjectAndroidManifest', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\', // const aRemoteDir: String;
              'AndroidManifest.xml', // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //-----------------
          //Android/Android64
          //The classes.dex file is a Dalvik Executable file that all Android applications must have. This file contains
          //the Java libraries that the application uses.
          if (ALSameTextA(LplatFormName, 'Android') or ALSameTextA(LplatFormName, 'Android64')) and
             (compareValue(LProjectVersion, 19.5{Alexandria}) >= 0) and
             (compareValue(LProjectVersion, 20.2{Athens 12.2}) <= 0) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName) + _getProjectRoot(LProjectName, LPlatformName) + '.classes', // const aInclude: String;
              'AndroidClasses', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '64', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName) + '\classes\', // const aRemoteDir: String;
              _getProjectRoot(LProjectName, LPlatformName) + '.classes', // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //-----------------
          //Android/Android64
          //The classes.dex file is a Dalvik Executable file that all Android applications must have. This file contains
          //the Java libraries that the application uses.
          if (ALSameTextA(LplatFormName, 'Android') or ALSameTextA(LplatFormName, 'Android64')) and
             (compareValue(LProjectVersion, 19.5{Alexandria}) < 0) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+ 'classes.dex', // const aInclude: String;
              'AndroidClassesDexFile', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\classes\', // const aRemoteDir: String;
              'classes.dex', // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //--------------
          //OSX64/OSXARM64
          //The main binary
          if (ALSameTextA(LplatFormName, 'OSX64')) or
             (ALSameTextA(LplatFormName, 'OSXARM64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName, // const aInclude: String;
              'ProjectOutput', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\Contents\MacOS\', // const aRemoteDir: String;
              LProjectName, // const aRemoteName: String;
              'True'); // aRequired: AnsiString

          //-----
          //OSX64
          //Skia: The compiled skia library
          if (LSkiaEnabled) and
             (ALSameTextA(LplatFormName, 'OSX64')) and
             (compareValue(LProjectVersion, 20.1{Athens}) = 0) then begin
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''!=''''', // const aCondition: AnsiString;
              '$(SKIADIR)\Binary\Shared\OSX64\libsk4d.dylib', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\Contents\MacOS\', // const aRemoteDir: String;
              'libsk4d.dylib', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''==''''', // const aCondition: AnsiString;
              '$(BDS)\binosx64\libsk4d.dylib', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\Contents\MacOS\', // const aRemoteDir: String;
              'libsk4d.dylib', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
          end;

          //--------
          //OSXARM64
          //Skia: The compiled skia library
          if (LSkiaEnabled) and
             (ALSameTextA(LplatFormName, 'OSXARM64')) and
             (compareValue(LProjectVersion, 20.1{Athens}) = 0) then begin
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''!=''''', // const aCondition: AnsiString;
              '$(SKIADIR)\Binary\Shared\OSXARM64\libsk4d.dylib', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\Contents\MacOS\', // const aRemoteDir: String;
              'libsk4d.dylib', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '''$(SKIADIR)''==''''', // const aCondition: AnsiString;
              '$(BDS)\binosxarm64\libsk4d.dylib', // const aInclude: String;
              'Skia', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\Contents\MacOS\', // const aRemoteDir: String;
              'libsk4d.dylib', // const aRemoteName: String;
              'True'); // aRequired: AnsiString
          end;

          //--------------
          //OSX64/OSXARM64
          //Debugging Information
          //https://developer.apple.com/documentation/xcode/building-your-app-to-include-debugging-information
          if ((ALSameTextA(LplatFormName, 'OSX64')) or
              (ALSameTextA(LplatFormName, 'OSXARM64'))) and
             ((compareValue(LProjectVersion, 19.5{Alexandria}) < 0) or
              (ALSameTextA(LConfigName, 'Debug'))) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName+'.dSYM', // const aInclude: String;
              'ProjectOSXDebug', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).app.dSYM\Contents\Resources\DWARF\', // const aRemoteDir: String;
              LProjectName, // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //--------------
          //OSX64/OSXARM64
          //Key-value pairs that grant an executable permission to use a service or technology.
          if (ALSameTextA(LplatFormName, 'OSX64')) or
             (ALSameTextA(LplatFormName, 'OSXARM64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName+'.entitlements', // const aInclude: String;
              'ProjectOSXEntitlements', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\', // const aRemoteDir: String;
              LProjectName+'.entitlements', // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //--------------
          //OSX64/OSXARM64
          //Every app and plug-in uses an Info.plist file to store configuration data in a place
          //where the system can easily access it. macOS and iOS use Info.plist files to determine
          //what icon to display for a bundle, what document types an app supports, and many other
          //behaviors that have an impact outside the bundle itself.
          if (ALSameTextA(LplatFormName, 'OSX64')) or
             (ALSameTextA(LplatFormName, 'OSXARM64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName+'.info.plist', // const aInclude: String;
              'ProjectOSXInfoPList', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\Contents\', // const aRemoteDir: String;
              'Info.plist', // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //--------------
          //OSX64/OSXARM64
          if (ALSameTextA(LplatFormName, 'OSX64')) or
             (ALSameTextA(LplatFormName, 'OSXARM64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              LProperties, // const aProperties: TALStringListA;
              LConfigs, // const aConfigs: TALStringListA;
              LPlatFormName, // const aPlatFormName: ansiString;
              LConfigName, // const aConfigName: AnsiString;
              'Icns_MainIcns', // const aPropertyName: ansiString;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              'ProjectOSXResource', // const aDeployClass: AnsiString;
              '', // const aLocalCommand: AnsiString;
              '1', // const aOperation: AnsiString;
              'True', // aOverwrite: AnsiString;
              '', // const aRemoteCommand: AnsiString;
              _getProjectRoot(LProjectName, LPlatformName)+'\Contents\Resources\', // const aRemoteDir: AnsiString;
              LProjectName + '.icns', // const aRemoteName: AnsiString;
              ''); // aRequired: AnsiString;

          //-----------
          //iOSDevice64
          //The main binary
          if (ALSameTextA(LplatFormName, 'iOSDevice64')) or
             (ALSameTextA(LplatFormName, 'iOSSimulator')) or
             (ALSameTextA(LplatFormName, 'iOSSimARM64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName, // const aInclude: String;
              'ProjectOutput', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\', // const aRemoteDir: String;
              LProjectName, // const aRemoteName: String;
              'True'); // aRequired: AnsiString

          //-----------
          //iOSDevice64
          //Debugging Information
          //https://developer.apple.com/documentation/xcode/building-your-app-to-include-debugging-information
          if ((ALSameTextA(LplatFormName, 'iOSDevice64')) or
              (ALSameTextA(LplatFormName, 'iOSSimARM64'))) and
             ((compareValue(LProjectVersion, 19.5{Alexandria}) < 0) or
              (ALSameTextA(LConfigName, 'Debug'))) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName+'.dSYM', // const aInclude: String;
              'ProjectiOSDeviceDebug', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).app.dSYM\Contents\Resources\DWARF\', // const aRemoteDir: String;
              LProjectName, // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //-----------
          //iOSDevice64
          //Key-value pairs that grant an executable permission to use a service or technology.
          if (ALSameTextA(LplatFormName, 'iOSDevice64')) or
             (ALSameTextA(LplatFormName, 'iOSSimARM64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName+'.entitlements', // const aInclude: String;
              'ProjectiOSEntitlements', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\', // const aRemoteDir: String;
              LProjectName+'.entitlements', // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //-----------
          //iOSDevice64
          //Every app and plug-in uses an Info.plist file to store configuration data in a place
          //where the system can easily access it. macOS and iOS use Info.plist files to determine
          //what icon to display for a bundle, what document types an app supports, and many other
          //behaviors that have an impact outside the bundle itself.
          if (ALSameTextA(LplatFormName, 'iOSDevice64')) or
             (ALSameTextA(LplatFormName, 'iOSSimulator')) or
             (ALSameTextA(LplatFormName, 'iOSSimARM64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName+'.info.plist', // const aInclude: String;
              'ProjectiOSInfoPList', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '1', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\', // const aRemoteDir: String;
              'Info.plist', // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //-----------
          //iOSDevice64
          //A launch screen appears instantly when your app starts up and is quickly replaced with the
          //app's first screen, giving the impression that your app is fast and responsive. The
          //launch screen isn’t an opportunity for artistic expression. It’s solely intended to
          //enhance the perception of your app as quick to launch and immediately ready for use.
          //Every app must supply a launch screen.
          if (ALSameTextA(LplatFormName, 'iOSDevice64')) or
             (ALSameTextA(LplatFormName, 'iOSSimulator')) or
             (ALSameTextA(LplatFormName, 'iOSSimARM64')) then
            _addDeployFile(
              LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
              TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
              LConfigName, // const aConfigName: AnsiString;
              '', // const aCondition: AnsiString;
              _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName+'.launchscreen', // const aInclude: String;
              'ProjectiOSLaunchScreen', // const aDeployClass: String;
              '', // const aLocalCommand: String;
              '64', // const aOperation: String;
              'True', // aOverwrite: String;
              '', // const aRemoteCommand: String;
              _getProjectRoot(LProjectName, LPlatformName)+'\..\$(PROJECTNAME).launchscreen\', // const aRemoteDir: String;
              LProjectName+'.launchscreen', // const aRemoteName: String;
              ''); // aRequired: AnsiString

          //-----
          //Win32
          //The main binary
          //if (ALSameTextA(LplatFormName, 'Win32')) then
          //  _addDeployFile(
          //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
          //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
          //    LConfigName, // const aConfigName: AnsiString;
          //    '', // const aCondition: AnsiString;
          //    _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName+'.exe', // const aInclude: String;
          //    'ProjectOutput', // const aDeployClass: String;
          //    '', // const aLocalCommand: String;
          //    '0', // const aOperation: String;
          //    'True', // aOverwrite: String;
          //    '', // const aRemoteCommand: String;
          //    _getProjectRoot(LProjectName, LPlatformName)+'\', // const aRemoteDir: String;
          //    LProjectName+'.exe', // const aRemoteName: String;
          //    'True'); // aRequired: AnsiString

          //-----
          //Win32
          //Skia: The compiled skia library
          //if (LSkiaEnabled) and
          //   (ALSameTextA(LplatFormName, 'Win32')) and
          //   (compareValue(LProjectVersion, 20.1{Athens}) >= 0) then begin
          //  _addDeployFile(
          //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
          //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
          //    LConfigName, // const aConfigName: AnsiString;
          //    '''$(SKIADIR)''!=''''', // const aCondition: AnsiString;
          //    '$(SKIADIR)\Binary\Shared\Win32\sk4d.dll', // const aInclude: String;
          //    'Skia', // const aDeployClass: String;
          //    '', // const aLocalCommand: String;
          //    '0', // const aOperation: String;
          //    'True', // aOverwrite: String;
          //    '', // const aRemoteCommand: String;
          //    _getProjectRoot(LProjectName, LPlatformName)+'\', // const aRemoteDir: String;
          //    'sk4d.dll', // const aRemoteName: String;
          //    'True'); // aRequired: AnsiString
          //  _addDeployFile(
          //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
          //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
          //    LConfigName, // const aConfigName: AnsiString;
          //    '''$(SKIADIR)''==''''', // const aCondition: AnsiString;
          //    '$(BDS)\bin\sk4d.dll', // const aInclude: String;
          //    'Skia', // const aDeployClass: String;
          //    '', // const aLocalCommand: String;
          //    '0', // const aOperation: String;
          //    'True', // aOverwrite: String;
          //    '', // const aRemoteCommand: String;
          //    _getProjectRoot(LProjectName, LPlatformName)+'\', // const aRemoteDir: String;
          //    'sk4d.dll', // const aRemoteName: String;
          //    'True'); // aRequired: AnsiString
          //end;

          //-----
          //Win64
          //The main binary
          //if (ALSameTextA(LplatFormName, 'Win64')) then
          //  _addDeployFile(
          //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
          //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
          //    LConfigName, // const aConfigName: AnsiString;
          //    '', // const aCondition: AnsiString;
          //    _getExeOutputDir(LProperties,LConfigs,LPlatFormName,LConfigName)+LProjectName+'.exe', // const aInclude: String;
          //    'ProjectOutput', // const aDeployClass: String;
          //    '', // const aLocalCommand: String;
          //    '0', // const aOperation: String;
          //    'True', // aOverwrite: String;
          //    '', // const aRemoteCommand: String;
          //    _getProjectRoot(LProjectName, LPlatformName)+'\', // const aRemoteDir: String;
          //    LProjectName+'.exe', // const aRemoteName: String;
          //    'True'); // aRequired: AnsiString

          //-----
          //Win64
          //Skia: The compiled skia library
          //if (LSkiaEnabled) and
          //   (ALSameTextA(LplatFormName, 'Win64')) and
          //   (compareValue(LProjectVersion, 20.1{Athens}) >= 0) then begin
          //  _addDeployFile(
          //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
          //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
          //    LConfigName, // const aConfigName: AnsiString;
          //    '''$(SKIADIR)''!=''''', // const aCondition: AnsiString;
          //    '$(SKIADIR)\Binary\Shared\Win64\sk4d.dll', // const aInclude: String;
          //    'Skia', // const aDeployClass: String;
          //    '', // const aLocalCommand: String;
          //    '0', // const aOperation: String;
          //    'True', // aOverwrite: String;
          //    '', // const aRemoteCommand: String;
          //    _getProjectRoot(LProjectName, LPlatformName)+'\', // const aRemoteDir: String;
          //    'sk4d.dll', // const aRemoteName: String;
          //    'True'); // aRequired: AnsiString
          //  _addDeployFile(
          //    LAlreadyDeployedFiles, // const aAlreadyDeployedFiles: TALStringListA;
          //    TalXmlNode(LplatForms.Objects[I]), // const aItemGroupNode: TalXmlNode;
          //    LConfigName, // const aConfigName: AnsiString;
          //    '''$(SKIADIR)''==''''', // const aCondition: AnsiString;
          //    '$(BDS)\bin64\sk4d.dll', // const aInclude: String;
          //    'Skia', // const aDeployClass: String;
          //    '', // const aLocalCommand: String;
          //    '0', // const aOperation: String;
          //    'True', // aOverwrite: String;
          //    '', // const aRemoteCommand: String;
          //    _getProjectRoot(LProjectName, LPlatformName)+'\', // const aRemoteDir: String;
          //    'sk4d.dll', // const aRemoteName: String;
          //    'True'); // aRequired: AnsiString
          //end;

        end;

      end;

      //save the backup of DeployProj
      if (LCreateBackup) and
         (Tfile.exists(LDeployProjFilename)) and
         (not Tfile.exists(LDeployProjFilename + '.bak')) then begin
        var LBackupDeployProjXmlDoc := TALXmlDocument.Create('Project');
        try
          LBackupDeployProjXmlDoc.Options := [];
          LBackupDeployProjXmlDoc.ParseOptions := [];
          LBackupDeployProjXmlDoc.LoadFromFile(LDeployProjFilename);
          var LBackupXmlStr := _DeployProjToXMLString(LBackupDeployProjXmlDoc);
          ALSaveStringToFile(cAlUTF8Bom + LBackupXmlStr, LDeployProjFilename + '.bak');
        finally
          ALFreeAndNil(LBackupDeployProjXmlDoc);
        end;
      end;

      //save the DeployProj
      var LXmlStr := _DeployProjToXMLString(LDeployProjXmlDoc);
      ALSaveStringToFile(cAlUTF8Bom + LXmlStr, LDeployProjFilename);

    Finally
      ALFreeAndNil(LDProjXmlDoc);
      ALFreeAndNil(LDeployProjXmlDoc);
      ALFreeAndNil(LPlatforms);
      ALFreeAndNil(LConfigs);
      ALFreeAndNil(LProperties);
      ALFreeAndNil(LAlreadyDeployedFiles);
    End;

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;
end.
