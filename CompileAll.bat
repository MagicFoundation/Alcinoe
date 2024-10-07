@echo off
SETLOCAL
cls

REM -------------------
REM Choose the compiler
REM -------------------

:CHOOSE_COMPILER

echo --------
echo Compiler
echo --------
echo.

echo 1) Athens
echo 2) Alexandria (deprecated)

set COMPILER=
set /P COMPILER=Enter number to select a compiler (Empty to auto select): %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input

if "%COMPILER%"=="" (
  set ALDelphiVersion=
  goto INIT_ENVIRONMENT
)
if "%COMPILER%"=="1" (
  set ALDelphiVersion=23.0
  goto INIT_ENVIRONMENT
)
if "%COMPILER%"=="2" (
  set ALDelphiVersion=22.0
  goto INIT_ENVIRONMENT
)
echo.
goto CHOOSE_COMPILER


REM ----------------
REM Init Environment
REM ----------------

:INIT_ENVIRONMENT

call "%~dp0InitEnvironment.bat"
IF ERRORLEVEL 1 goto ERROR
echo.


REM --------------
REM Security check
REM --------------

if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR


REM --------------------------------------------
REM Copy and patch localy the delphi source code
REM --------------------------------------------

:COPY_AND_PATCH_DELPHI_SOURCE

echo ------------------
echo Delphi source code
echo ------------------
echo.

set ALCopyAndPatchDelphiSource=
set /P ALCopyAndPatchDelphiSource=Copy the Delphi source code and patch it locally (Y/N, default=Y)?: %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
echo.

if "%ALCopyAndPatchDelphiSource%"=="" set ALCopyAndPatchDelphiSource=Y
if "%ALCopyAndPatchDelphiSource%"=="y" set ALCopyAndPatchDelphiSource=Y
if "%ALCopyAndPatchDelphiSource%"=="n" set ALCopyAndPatchDelphiSource=N
if "%ALCopyAndPatchDelphiSource%"=="Y" goto DO_COPY_AND_PATCH_DELPHI_SOURCE
if "%ALCopyAndPatchDelphiSource%"=="N" goto DOWNLOAD_LIBRARIES
goto COPY_AND_PATCH_DELPHI_SOURCE

:DO_COPY_AND_PATCH_DELPHI_SOURCE

call "%ALBaseDir%\Embarcadero\%ALDelphiName%\Update.bat"
IF ERRORLEVEL 1 goto ERROR
echo.


REM ----------------------
REM Download the libraries
REM ----------------------

:DOWNLOAD_LIBRARIES

echo ---------
echo Libraries
echo ---------
echo.

set ALDownloadLibraries=
set /P ALDownloadLibraries=Download libraries (Y/N, default=Y)?: %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
echo.

if "%ALDownloadLibraries%"=="" set ALDownloadLibraries=Y
if "%ALDownloadLibraries%"=="y" set ALDownloadLibraries=Y
if "%ALDownloadLibraries%"=="n" set ALDownloadLibraries=N
if "%ALDownloadLibraries%"=="Y" goto DO_DOWNLOAD_LIBRARIES
if "%ALDownloadLibraries%"=="N" goto BUILD_DPROJNORMALIZER
goto DOWNLOAD_LIBRARIES

:DO_DOWNLOAD_LIBRARIES

call "%ALBaseDir%\Libraries\ios\DownloadLibraries.bat"
IF ERRORLEVEL 1 goto ERROR
echo.

call "%ALBaseDir%\Libraries\jar\DownloadLibraries.bat"
IF ERRORLEVEL 1 goto ERROR
echo.


REM ----------------------------------------
REM Build DProjNormalizer and UnitNormalizer
REM ----------------------------------------

:BUILD_DPROJNORMALIZER

echo ----------------------------------------
echo Build DProjNormalizer and UnitNormalizer
echo ----------------------------------------
echo.

echo [36mMSBuild DProjNormalizer.dproj /p:config=Release /p:Platform=Win64[0m
MSBuild "%ALBaseDir%\Tools\DProjNormalizer\_Source\DProjNormalizer.dproj" /p:Config=Release /p:Platform=Win64 /t:Build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR
echo.

echo [36mMSBuild UnitNormalizer.dproj /p:config=Release /p:Platform=Win64[0m
MSBuild "%ALBaseDir%\Tools\UnitNormalizer\_Source\UnitNormalizer.dproj" /p:Config=Release /p:Platform=Win64 /t:Build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR
echo.


REM -------------------
REM Normalize all units
REM -------------------

echo -------------------
echo Normalize all units
echo -------------------
echo.

call "%ALBaseDir%\Tools\UnitNormalizer\UnitNormalizer.exe" -Dir="%ALBaseDir%\Demos\" -CreateBackup="false" -FilesToIgnore="superxmlparser.pas;supertimezone.pas;superobject.pas;superdate.pas;dwsXPlatform.pas;dwsUtils.pas;dwsJSON.pas" -NoInteraction=true
IF ERRORLEVEL 1 goto ERROR
call "%ALBaseDir%\Tools\UnitNormalizer\UnitNormalizer.exe" -Dir="%ALBaseDir%\Source\" -FilesToIgnore="ZLibExGZ.pas;ZLibExApi.pas;ZLibEx.pas;Grijjy.SymbolTranslator.pas;Grijjy.ErrorReporting.pas;Alcinoe.iOSapi.ImageIO.pas" -CreateBackup="false" -NoInteraction=true
IF ERRORLEVEL 1 goto ERROR
call "%ALBaseDir%\Tools\UnitNormalizer\UnitNormalizer.exe" -Dir="%ALBaseDir%\Tests\" -CreateBackup="false" -NoInteraction=true
IF ERRORLEVEL 1 goto ERROR
call "%ALBaseDir%\Tools\UnitNormalizer\UnitNormalizer.exe" -Dir="%ALBaseDir%\Tools\" -CreateBackup="false" -NoInteraction=true
IF ERRORLEVEL 1 goto ERROR
echo.


REM -----------------
REM Build & Run Tests
REM -----------------

:RUN_TESTS

echo -----
echo Tests
echo -----
echo.

set ALRunTests=
set /P ALRunTests=Run tests (Y/N, default=Y)?: %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
echo.

if "%ALRunTests%"=="" set ALRunTests=Y
if "%ALRunTests%"=="y" set ALRunTests=Y
if "%ALRunTests%"=="n" set ALRunTests=N
if "%ALRunTests%"=="Y" goto DO_RUN_TESTS
if "%ALRunTests%"=="N" goto BUILD_BPL
goto RUN_TESTS

:DO_RUN_TESTS

call "%ALBaseDir%\Tests\DUnitX\RunTests.bat"
IF ERRORLEVEL 1 goto ERROR
echo.


REM ---------
REM Build bpl 
REM ---------

:BUILD_BPL

echo ---------
echo Build BPL
echo ---------
echo.

SET FileName=%ALBaseDir%\Libraries\bpl\Alcinoe\Win32\%ALDelphiName%
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

Call :BUILD_PROJECT "%ALBaseDir%\Source\Packages" "" "Alcinoe%ALDelphiName%.dproj" "Win32"
IF ERRORLEVEL 1 goto ERROR


REM ----------
REM Build jars 
REM ----------

:BUILD_JARS

echo ----------
echo Build Jars
echo ----------
echo.

set ALBuildJars=
set /P ALBuildJars=Build Jars (Y/N, default=Y)?: %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
echo.

if "%ALBuildJars%"=="" set ALBuildJars=Y
if "%ALBuildJars%"=="y" set ALBuildJars=Y
if "%ALBuildJars%"=="n" set ALBuildJars=N
if "%ALBuildJars%"=="Y" goto DO_BUILD_JARS
if "%ALBuildJars%"=="N" goto BUILD_TOOLS
goto BUILD_JARS

:DO_BUILD_JARS

call "%ALBaseDir%\compileJar.bat"
IF ERRORLEVEL 1 goto ERROR
echo.


REM -----------
REM Build Tools
REM -----------

:BUILD_TOOLS

echo -----------
echo Build Tools
echo -----------
echo.

set ALBuildTools=
set /P ALBuildTools=Build tools (Y/N, default=Y)?: %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
echo.

if "%ALBuildTools%"=="" set ALBuildTools=Y
if "%ALBuildTools%"=="y" set ALBuildTools=Y
if "%ALBuildTools%"=="n" set ALBuildTools=N
if "%ALBuildTools%"=="Y" goto DO_BUILD_TOOLS
if "%ALBuildTools%"=="N" goto BUILD_DEMOS
goto BUILD_TOOLS

:DO_BUILD_TOOLS

Call :BUILD_PROJECT "%ALBaseDir%\Tools\AndroidMerger" "_Build\Source" "AndroidMerger.dproj" "Win64"
Call :BUILD_PROJECT "%ALBaseDir%\Tools\DeployMan" "_Build\Source" "DeployMan.dproj" "Win64"
Call :BUILD_PROJECT "%ALBaseDir%\Tools\DeployProjNormalizer" "_Source" "DeployProjNormalizer.dproj" "Win64"
Call :BUILD_PROJECT "%ALBaseDir%\Tools\DProjNormalizer" "_Source" "DProjNormalizer.dproj" "Win64"
Call :BUILD_PROJECT "%ALBaseDir%\Tools\DProjVersioning" "_Source" "DProjVersioning.dproj" "Win64"
Call :BUILD_PROJECT "%ALBaseDir%\Tools\NativeBridgeFileGenerator" "_Build\Source" "NativeBridgeFileGeneratorHelper.dproj" "Win64"
Call :BUILD_PROJECT "%ALBaseDir%\Tools\UnitNormalizer" "_Source" "UnitNormalizer.dproj" "Win64"
Call :BUILD_PROJECT "%ALBaseDir%\Tools\CodeRenaming" "_Source" "CodeRenaming.dproj" "Win64"

REM -----------
REM Build demos
REM -----------

:BUILD_DEMOS

echo -----------
echo Build demos
echo -----------
echo.

set ALBuildDemos=
set /P ALBuildDemos=Build demos (Y/N, default=Y)?: %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input

if "%ALBuildDemos%"=="" set ALBuildDemos=Y
if "%ALBuildDemos%"=="y" set ALBuildDemos=Y
if "%ALBuildDemos%"=="n" set ALBuildDemos=N
if "%ALBuildDemos%"=="Y" goto DO_BUILD_DEMOS
if "%ALBuildDemos%"=="N" goto FINISHED
goto BUILD_DEMOS

:DO_BUILD_DEMOS

Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALAnimation" "_Source" "ALAnimationDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALCipher" "_Source" "ALCipherDemo.dproj"
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALConfetti" "_Source" "ALConfettiDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALDatabaseBenchmark" "_Source" "ALDatabaseBenchmark.dproj"
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALFacebookLogin" "_Source" "ALFacebookLogin.dproj"
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALNotificationService" "_Source" "ALNotificationServiceDemo.dproj"
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALFmxControls" "_Source" "ALFmxControls.dproj"
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALFmxFilterEffects" "_Source" "ALFmxFilterEffectsDemo.dproj"
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALFmxGraphics" "_Source" "ALFmxGraphicsDemo.dproj"
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALGeoPositionSensor" "_Source" "ALGeoPositionSensorDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALJsonDoc" "_Source" "ALJsonDocDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALLibPhoneNumber" "_Source" "ALLibPhoneNumberDemo.dproj"
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALLiveVideoChat\Client" "_Source" "ALLiveVideoChatClient.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALLiveVideoChat\Server" "_Source" "ALLiveVideoChatServer.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALNNTPClient" "_Source" "ALNNTPClientDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALPhpRunner" "_Source" "ALPhpRunnerDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALPOP3Client" "_Source" "ALPOP3ClientDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALRTTI" "_Source" "ALRTTIDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALSMTPClient" "_Source" "ALSMTPClientDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALSortedListBenchmark" "_Source" "ALSortedListBenchmark.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALSqlite3Client" "_Source" "ALSqlite3clientDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALStressHTTPServer" "_Source" "ALStressHTTPServer.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALStringBenchmark" "_Source" "ALStringBenchmark.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALWinHTTPClient" "_Source" "ALWinHTTPClientDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALWinHTTPWebSocketClient" "_Source" "ALWinHTTPWebSocketClientDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALWinInetHTTPClient" "_Source" "ALWinInetHTTPClientDemo.dproj"
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALXmlDoc" "_Source" "ALXmlDocDemo.dproj"

xcopy "%ALBaseDir%\Libraries\dll\tbbmalloc\win32\tbbmalloc.dll" "%ALBaseDir%\Demos\ALDatabaseBenchmark\Win32\Release" /s
IF ERRORLEVEL 1 goto ERROR

xcopy "%ALBaseDir%\Libraries\dll\tbbmalloc\win64\tbbmalloc.dll" "%ALBaseDir%\Demos\ALDatabaseBenchmark\Win64\Release" /s
IF ERRORLEVEL 1 goto ERROR

goto FINISHED


REM -----------------------
REM Function BUILD_FMX_DEMO
REM -----------------------

:BUILD_FMX_DEMO

SET FileName=%~1\Android\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

SET FileName=%~1\Android64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

SET FileName=%~1\iOSDevice64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

SET FileName=%~1\iOSSimARM64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

SET FileName=%~1\OSX64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

SET FileName=%~1\OSXARM64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

Call :BUILD_VCL_DEMO "%~1" "%~2" "%~3"
Call :BUILD_PROJECT "%~1" "%~2" "%~3" "Android"
Call :BUILD_PROJECT "%~1" "%~2" "%~3" "Android64"
Call :BUILD_PROJECT "%~1" "%~2" "%~3" "iOSDevice64"
REM Call :BUILD_PROJECT "%~1" "%~2" "%~3" "iOSSimARM64"
REM Call :BUILD_PROJECT "%~1" "%~2" "%~3" "OSX64"
REM Call :BUILD_PROJECT "%~1" "%~2" "%~3" "OSXARM64"

EXIT /B 0


REM -----------------------
REM Function BUILD_VCL_DEMO
REM -----------------------

:BUILD_VCL_DEMO

REM %~1 the base directory
REM %~2 the source directory relative to the base directory
REM %~3 the dproj filename without path

SET FileName=%~1\Win32\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

SET FileName=%~1\Win64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

Call :BUILD_PROJECT "%~1" "%~2" "%~3" "Win32"
Call :BUILD_PROJECT "%~1" "%~2" "%~3" "Win64"

EXIT /B 0


REM ----------------------
REM Function BUILD_PROJECT
REM ----------------------

:BUILD_PROJECT

REM %~1 the base directory
REM %~2 the source directory relative to the base directory
REM %~3 the dproj filename without path
REM %~4 the platform

SET FileName=%~1\%~2\dbgout.log
if exist "%FileName%" del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%~1\*.skincfg
if exist "%FileName%" del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%~1\*.rsm
if exist "%FileName%" del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%~1\*.identcache
if exist "%FileName%" del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%~1\*.dproj.local
if exist "%FileName%" del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%~1\*.groupproj.local
if exist "%FileName%" del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%~1\*.deployproj.local
if exist "%FileName%" del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%~1\%~2\Dcu\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"

if "%~4"=="Android" (
  echo [36mMerge Android Libraries for %~3[0m
  call "%~1\%~2\Android\MergeLibraries.bat"
  IF ERRORLEVEL 1 goto ERROR
)

call "%ALBaseDir%\Tools\DProjNormalizer\DProjNormalizer.exe" -DProj="%~1\%~2\%~3" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild %~3 /p:config=Release /p:Platform=%~4[0m
MSBuild "%~1\%~2\%~3" /p:Config=Release /p:Platform=%~4 /t:Build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR
echo.

SET ALDeploy=N
if "%~4"=="Android" Set ALDeploy=Y
if "%~4"=="Android64" Set ALDeploy=Y
if "%ALDeploy%"=="Y" (

  if exist *.deployproj del *.deployproj /s
  if exist *.deployproj goto ERROR

  call "%ALBaseDir%\Tools\DeployProjNormalizer\DeployProjNormalizer.exe" -DProj="%~1\%~2\%~3" -CreateBackup="false"
  IF ERRORLEVEL 1 goto ERROR

  MSBuild "%~1\%~2\%~3" /p:Config=Release /p:Platform=%~4 /t:Deploy /verbosity:minimal
  IF ERRORLEVEL 1 goto ERROR

)

SET FileName=%~1\%~2\Dcu\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"

SET FileName=%~1\%~2\dbgout.log
if exist "%FileName%" del "%FileName%" /s
if exist "%FileName%" goto ERROR

EXIT /B 0


REM -------------------
REM FINISHED/ERROR/EXIT
REM -------------------

:FINISHED

echo.
echo Finished
PAUSE
goto EXIT

:ERROR

PAUSE
EXIT /B 1

:EXIT