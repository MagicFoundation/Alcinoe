@echo off

set ALBaseDir=%~dp0

"%ALBaseDir%\..\..\..\..\Tools\DeployMan\DeployMan.exe"^
 -DProj="..\ALFirebaseMessaging.dproj"^
 -Paths=".\GoogleServices\GoogleService-Info.plist|false|.\GoogleService-Info.plist;.\PlugIns|true|.\PlugIns"^
 -Platforms="iOSDevice64;iOSSimARM64"^
 -DProjNormalizer="%ALBaseDir%\..\..\..\..\Tools\DProjNormalizer\DProjNormalizer.exe"^