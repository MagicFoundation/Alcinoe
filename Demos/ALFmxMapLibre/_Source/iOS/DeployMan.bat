@echo off

if "%ALBaseDir%"=="" (
  call "%~dp0\..\..\..\..\InitEnvironment.bat" >nul
  IF ERRORLEVEL 1 goto ERROR  
)

set CurrentDir=%~dp0

set "Paths=%CurrentDir%\..\..\..\..\Libraries\ios\MapLibre\Dynamic\MapLibre.xcframework\ios-arm64|true|.\Frameworks"

call "%ALBaseDir%\Tools\DeployMan\DeployMan.exe"^
 -DProj="%CurrentDir%\..\ALFmxMapLibreDemo.dproj"^
 -Paths="%Paths%"^
 -Configurations="Debug;Release"^
 -Platforms="iOSDevice64;iOSSimARM64"^
 -DProjNormalizer="%ALBaseDir%\Tools\DProjNormalizer\DProjNormalizer.exe"^
 -NoInteraction=true
IF ERRORLEVEL 1 goto EXIT

:FINISHED

echo.
echo Finished
PAUSE
goto EXIT

:ERROR

PAUSE
EXIT /B 1

:EXIT