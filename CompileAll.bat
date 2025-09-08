@echo off
SETLOCAL
cls

REM To deploy the app to the Apple Store, set the following environment variables:
REM   * Alcinoe_Mac_Host=IP address of the Mac computer used for deployment.
REM   * Alcinoe_Mac_Username=Username of the account on the Mac computer.
REM   * Alcinoe_Mac_Connection_Profile_Name=Name of the PAServer connection profile.
REM   * Alcinoe_EnvOptions_iOSDevice64_DevAppStore=Retrieve this value from: C:\Users\{UserName}\AppData\Roaming\Embarcadero\BDS\xx.0\EnvOptions.proj (after deploying the app from the IDE).
REM   * Alcinoe_EnvOptions_iOSDevice64_DevTeamIdAppStore=Retrieve this value from: C:\Users\{UserName}\AppData\Roaming\Embarcadero\BDS\xx.0\EnvOptions.proj (after deploying the app from the IDE).
REM   * Alcinoe_ALFmxControlsDemo_EnvOptions_iOSDevice64_MobileProvisionAppStore=Retrieve this value from: C:\Users\{UserName}\AppData\Roaming\Embarcadero\BDS\xx.0\EnvOptions.proj (after deploying the app from the IDE).
REM   * Alcinoe_ALFmxDynamicListBoxDemo_EnvOptions_iOSDevice64_MobileProvisionAppStore=Retrieve this value from: C:\Users\{UserName}\AppData\Roaming\Embarcadero\BDS\xx.0\EnvOptions.proj (after deploying the app from the IDE).
REM 
REM You must also generate an app-specific password for ALTool. Follow these steps:
REM  1. Generate an App-Specific Password:
REM     * Go to appleid.apple.com and sign in with your Apple ID.
REM     * In your account settings, navigate to the Security section.
REM     * Locate the option to generate an app-specific password, then click "Create an app-specific password."
REM     * Follow the on-screen instructions and provide a label ^(e.g., "altool"^) for your reference.
REM  2. Store the Username/Password Pair in Windows Credential Manager:
REM     * Open Credential Manager from the Control Panel.
REM     * Select the Windows Credentials tab.
REM     * Click "Add a generic credential."
REM     * Internet or network address: alcinoe.altool
REM     * User Name:{UserName} 
REM     * Password:{Password} 

REM -------------------
REM Choose the compiler
REM -------------------

:CHOOSE_COMPILER

echo --------
echo Compiler
echo --------
echo.
echo Before proceeding, ensure that the Android, iOS, and macOS 
echo platforms are correctly added in the SDK Manager.
echo For detailed setup instructions, visit: 
echo https://github.com/MagicFoundation/PlatformSDKs
echo.

echo 1) Athens
echo 2) Alexandria (deprecated)

set COMPILER=
set /P COMPILER="Select a compiler (Empty to auto select):" %=%
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


REM ----------------------------------------------------
REM Ask if user wants to perform all steps automatically
REM ----------------------------------------------------

echo -------------------------------
echo Perform all steps automatically
echo -------------------------------
echo.

set ALNoPrompts=
set /P ALNoPrompts="Perform all steps automatically without prompts? (Y/N, default=Y)?:" %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
echo.

if "%ALNoPrompts%"=="" set ALNoPrompts=Y
if "%ALNoPrompts%"=="y" set ALNoPrompts=Y


REM --------------------------------------------
REM Copy and patch localy the delphi source code
REM --------------------------------------------

:COPY_AND_PATCH_DELPHI_SOURCE

echo ------------------
echo Delphi source code
echo ------------------
echo.

if "%ALNoPrompts%"=="Y" (
  set ALCopyAndPatchDelphiSource=Y
) else (
  set ALCopyAndPatchDelphiSource=
  set /P ALCopyAndPatchDelphiSource="Copy the Delphi source code and patch it locally (Y/N, default=Y)?:" %=%
  more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
  echo.
)

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

if "%ALNoPrompts%"=="Y" (
  set ALDownloadLibraries=Y
) else (
  set ALDownloadLibraries=
  set /P ALDownloadLibraries="Download libraries (Y/N, default=Y)?:" %=%
  more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
  echo.
)

if "%ALDownloadLibraries%"=="" set ALDownloadLibraries=Y
if "%ALDownloadLibraries%"=="y" set ALDownloadLibraries=Y
if "%ALDownloadLibraries%"=="n" set ALDownloadLibraries=N
if "%ALDownloadLibraries%"=="Y" goto DO_DOWNLOAD_LIBRARIES
if "%ALDownloadLibraries%"=="N" goto BUILD_CORETOOLS
goto DOWNLOAD_LIBRARIES

:DO_DOWNLOAD_LIBRARIES

call "%ALBaseDir%\Libraries\ios\DownloadLibraries.bat"
IF ERRORLEVEL 1 goto ERROR
echo.

call "%ALBaseDir%\Libraries\jar\DownloadLibraries.bat"
IF ERRORLEVEL 1 goto ERROR
echo.


REM ----------------
REM Build Core Tools
REM ----------------

:BUILD_CORETOOLS

echo ----------------
echo Build Core Tools
echo ----------------
echo.

echo [36mMSBuild DProjNormalizer.dproj /p:config=Release /p:Platform=Win64 /t:Build[0m
MSBuild "%ALBaseDir%\Tools\DProjNormalizer\_Source\DProjNormalizer.dproj" /p:DCC_UseMSBuildExternally=true /p:Config=Release /p:Platform=Win64 /t:Build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR
echo.

echo [36mMSBuild DeployProjNormalizer.dproj /p:config=Release /p:Platform=Win64 /t:Build[0m
MSBuild "%ALBaseDir%\Tools\DeployProjNormalizer\_Source\DeployProjNormalizer.dproj" /p:DCC_UseMSBuildExternally=true /p:Config=Release /p:Platform=Win64 /t:Build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR
echo.

echo [36mMSBuild UnitNormalizer.dproj /p:config=Release /p:Platform=Win64 /t:Build[0m
MSBuild "%ALBaseDir%\Tools\UnitNormalizer\_Source\UnitNormalizer.dproj" /p:DCC_UseMSBuildExternally=true /p:Config=Release /p:Platform=Win64 /t:Build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR
echo.

echo [36mMSBuild CodeBuilder.dproj /p:config=Release /p:Platform=Win64 /t:Build[0m
MSBuild "%ALBaseDir%\Tools\CodeBuilder\_Source\CodeBuilder.dproj" /p:DCC_UseMSBuildExternally=true /p:Config=Release /p:Platform=Win64 /t:Build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR
echo.


REM -----------------------------------------
REM Normalize all Units, DProj and DeployProj
REM -----------------------------------------

echo -----------------------------------------
echo Normalize all Units, DProj and DeployProj
echo -----------------------------------------
echo.

call "%ALBaseDir%\NormalizeAll.bat"
IF ERRORLEVEL 1 goto ERROR
echo.


REM -------------------------
REM Auto-Generate Source Code
REM -------------------------

echo -------------------------
echo Auto-Generate Source Code
echo -------------------------
echo.

call "%ALBaseDir%\Tools\CodeBuilder\CodeBuilder.exe" -NoInteraction=true
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

if "%ALNoPrompts%"=="Y" (
  set ALRunTests=Y
) else (
  set ALRunTests=
  set /P ALRunTests="Run tests (Y/N, default=Y)?:" %=%
  more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
  echo.
)

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

if "%ALNoPrompts%"=="Y" (
  set ALBuildJars=Y
) else (
  set ALBuildJars=
  set /P ALBuildJars="Build Jars (Y/N, default=Y)?:" %=%
  more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
  echo.
)

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

if "%ALNoPrompts%"=="Y" (
  set ALBuildTools=Y
) else (
  set ALBuildTools=
  set /P ALBuildTools="Build tools (Y/N, default=Y)?:" %=%
  more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
  echo.
)

if "%ALBuildTools%"=="" set ALBuildTools=Y
if "%ALBuildTools%"=="y" set ALBuildTools=Y
if "%ALBuildTools%"=="n" set ALBuildTools=N
if "%ALBuildTools%"=="Y" goto DO_BUILD_TOOLS
if "%ALBuildTools%"=="N" goto BUILD_DEMOS
goto BUILD_TOOLS

:DO_BUILD_TOOLS

Call :BUILD_PROJECT "%ALBaseDir%\Tools\AndroidMerger" "_Build\Source" "AndroidMerger.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\AppIconGenerator" "_Source" "AppIconGenerator.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\CodeBuilder" "_Source" "CodeBuilder.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\CodeRenaming" "_Source" "CodeRenaming.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\DeployMan" "_Build\Source" "DeployMan.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\DeployProjNormalizer" "_Source" "DeployProjNormalizer.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\DProjNormalizer" "_Source" "DProjNormalizer.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\DProjVersioning" "_Source" "DProjVersioning.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\EnvOptionsProjUpdater" "_Source" "EnvOptionsProjUpdater.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\NativeBridgeFileGenerator" "_Build\Source" "NativeBridgeFileGeneratorHelper.dproj" "Win64" || GOTO ERROR
Call :BUILD_PROJECT "%ALBaseDir%\Tools\UnitNormalizer" "_Source" "UnitNormalizer.dproj" "Win64" || GOTO ERROR
if "%DXVCL%"=="" goto BUILD_DEMOS
Call :BUILD_PROJECT "%ALBaseDir%\Tools\CodeProfiler" "_Source" "CodeProfiler.dproj" "Win64" || GOTO ERROR


REM -----------
REM Build demos
REM -----------

:BUILD_DEMOS

echo -----------
echo Build demos
echo -----------
echo.

if "%ALNoPrompts%"=="Y" (
  set ALBuildDemos=Y
) else (
  set ALBuildDemos=
  set /P ALBuildDemos="Build demos (Y/N, default=Y)?:" %=%
  more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
  echo.
)

if "%ALBuildDemos%"=="" set ALBuildDemos=Y
if "%ALBuildDemos%"=="y" set ALBuildDemos=Y
if "%ALBuildDemos%"=="n" set ALBuildDemos=N
if "%ALBuildDemos%"=="Y" goto DO_BUILD_DEMOS
if "%ALBuildDemos%"=="N" goto CREATE_COMPILED_ARCHIVES
goto BUILD_DEMOS

:DO_BUILD_DEMOS

Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALFmxControls" "_Source" "ALFmxControlsDemo.dproj" || PAUSE
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALFmxDynamicListBox" "_Source" "ALFmxDynamicListBoxDemo.dproj" || PAUSE
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALFmxGraphics" "_Source" "ALFmxGraphicsDemo.dproj" || PAUSE
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALAnimation" "_Source" "ALAnimationDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALCipher" "_Source" "ALCipherDemo.dproj" || PAUSE
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALConfetti" "_Source" "ALConfettiDemo.dproj" || PAUSE
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALFacebookLogin" "_Source" "ALFacebookLoginDemo.dproj" || PAUSE
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALNotificationService" "_Source" "ALNotificationServiceDemo.dproj" || PAUSE
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALFmxFilterEffects" "_Source" "ALFmxFilterEffectsDemo.dproj" || PAUSE
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALGeoLocationSensor" "_Source" "ALGeoLocationSensorDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALJsonDoc" "_Source" "ALJsonDocDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALLibPhoneNumber" "_Source" "ALLibPhoneNumberDemo.dproj" || PAUSE
Call :BUILD_FMX_DEMO "%ALBaseDir%\Demos\ALLiveVideoChat\Client" "_Source" "ALLiveVideoChatClient.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALLiveVideoChat\Server" "_Source" "ALLiveVideoChatServer.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALNNTPClient" "_Source" "ALNNTPClientDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALPhpRunner" "_Source" "ALPhpRunnerDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALPOP3Client" "_Source" "ALPOP3ClientDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALRTTI" "_Source" "ALRTTIDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALSMTPClient" "_Source" "ALSMTPClientDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALSortedListBenchmark" "_Source" "ALSortedListBenchmark.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALSqlite3Client" "_Source" "ALSqlite3clientDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALStringBenchmark" "_Source" "ALStringBenchmark.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALXmlDoc" "_Source" "ALXmlDocDemo.dproj" || PAUSE
if "%DXVCL%"=="" goto DEPLOY_TO_APP_STORE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALDatabaseBenchmark" "_Source" "ALDatabaseBenchmark.dproj" || PAUSE
xcopy "%ALBaseDir%\Libraries\dll\tbbmalloc\win32\tbbmalloc.dll" "%ALBaseDir%\Demos\ALDatabaseBenchmark\Win32\Release" /s >nul 2>&1
xcopy "%ALBaseDir%\Libraries\dll\tbbmalloc\win64\tbbmalloc.dll" "%ALBaseDir%\Demos\ALDatabaseBenchmark\Win64\Release" /s >nul 2>&1
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALWinHTTPClient" "_Source" "ALWinHTTPClientDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALWinHTTPWebSocketClient" "_Source" "ALWinHTTPWebSocketClientDemo.dproj" || PAUSE
Call :BUILD_VCL_DEMO "%ALBaseDir%\Demos\ALWinInetHTTPClient" "_Source" "ALWinInetHTTPClientDemo.dproj" || PAUSE


REM -----------------------
REM Deploy to the App Store
REM -----------------------

:DEPLOY_TO_APP_STORE

if "%Alcinoe_Mac_Connection_Profile_Name%"=="" GOTO CREATE_COMPILED_ARCHIVES

SET ALTOOL_USERNAME=
for /f "usebackq delims=" %%i in (
  `powershell -NoProfile -ExecutionPolicy Bypass -File "%ALBaseDir%\Tools\GetWindowsCredential\GetWindowsCredential.ps1" -TargetName "alcinoe.altool" -Field UserName`
) do set ALTOOL_USERNAME=%%i

SET ALTOOL_PASSWORD=
for /f "usebackq delims=" %%i in (
  `powershell -NoProfile -ExecutionPolicy Bypass -File "%ALBaseDir%\Tools\GetWindowsCredential\GetWindowsCredential.ps1" -TargetName "alcinoe.altool" -Field Password`
) do set ALTOOL_PASSWORD=%%i

If "%ALTOOL_PASSWORD%"=="" (
  Echo You must generate an app-specific password for ALTool. Follow these steps:
  Echo  1. Generate an App-Specific Password:
  Echo     * Go to appleid.apple.com and sign in with your Apple ID.
  Echo     * In your account settings, navigate to the Security section.
  Echo     * Locate the option to generate an app-specific password, then click "Create an app-specific password."
  Echo     * Follow the on-screen instructions and provide a label ^(e.g., "altool"^) for your reference.
  Echo  2. Store the Username/Password Pair in Windows Credential Manager:
  Echo     * Open Credential Manager from the Control Panel.
  Echo     * Select the Windows Credentials tab.
  Echo     * Click "Add a generic credential."
  Echo     * Internet or network address: alcinoe.altool
  Echo     * User Name:{UserName} 
  Echo     * Password:{Password} 
  GOTO ERROR
)

echo -----------------------
echo Deploy to the App Store
echo -----------------------
echo.  

if "%ALNoPrompts%"=="Y" (
  set DEPLOYTOAPPSTORE=Y
) else (
  set DEPLOYTOAPPSTORE=
  set /P DEPLOYTOAPPSTORE="Upload the Binary to the App Store (Y/N, default=Y)?:" %=%
  more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
  echo.
)
if "%DEPLOYTOAPPSTORE%"=="n" (SET DEPLOYTOAPPSTORE=N)
if "%DEPLOYTOAPPSTORE%"=="y" (SET DEPLOYTOAPPSTORE=Y)
if "%DEPLOYTOAPPSTORE%"=="" (SET DEPLOYTOAPPSTORE=Y)

if "%DEPLOYTOAPPSTORE%"=="N" (
  echo.
  goto CREATE_COMPILED_ARCHIVES
)
if "%DEPLOYTOAPPSTORE%"=="Y" ( 
  goto DO_DEPLOY_TO_APP_STORE 
)
echo.
goto DEPLOY_TO_APP_STORE

:DO_DEPLOY_TO_APP_STORE

echo Uploading ALFmxControlsDemo.ipa to the Mac 
ssh %Alcinoe_Mac_Username%@%Alcinoe_Mac_Host% "rm -rf /tmp/ALFmxControlsDemo.ipa"
IF ERRORLEVEL 1 goto ERROR
scp -q %ALBaseDir%\Demos\ALFmxControls\iOSDevice64\Release\ALFmxControlsDemo.ipa %Alcinoe_Mac_Username%@%Alcinoe_Mac_Host%:/tmp/ALFmxControlsDemo.ipa
IF ERRORLEVEL 1 goto ERROR
echo Uploading ALFmxControlsDemo.ipa to the App Store
ssh %Alcinoe_Mac_Username%@%Alcinoe_Mac_Host% "xcrun altool --upload-app --type ios --file /tmp/ALFmxControlsDemo.ipa --username %ALTOOL_USERNAME% --password %ALTOOL_PASSWORD%"
IF ERRORLEVEL 1 goto ERROR
ssh %Alcinoe_Mac_Username%@%Alcinoe_Mac_Host% "rm -rf /tmp/ALFmxControlsDemo.ipa"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Uploading ALFmxDynamicListBoxDemo.ipa to the Mac 
ssh %Alcinoe_Mac_Username%@%Alcinoe_Mac_Host% "rm -rf /tmp/ALFmxDynamicListBoxDemo.ipa"
IF ERRORLEVEL 1 goto ERROR
scp -q %ALBaseDir%\Demos\ALFmxDynamicListBox\iOSDevice64\Release\ALFmxDynamicListBoxDemo.ipa %Alcinoe_Mac_Username%@%Alcinoe_Mac_Host%:/tmp/ALFmxDynamicListBoxDemo.ipa
IF ERRORLEVEL 1 goto ERROR
echo Uploading ALFmxDynamicListBoxDemo.ipa to the App Store
ssh %Alcinoe_Mac_Username%@%Alcinoe_Mac_Host% "xcrun altool --upload-app --type ios --file /tmp/ALFmxDynamicListBoxDemo.ipa --username %ALTOOL_USERNAME% --password %ALTOOL_PASSWORD%"
IF ERRORLEVEL 1 goto ERROR
ssh %Alcinoe_Mac_Username%@%Alcinoe_Mac_Host% "rm -rf /tmp/ALFmxDynamicListBoxDemo.ipa"
IF ERRORLEVEL 1 goto ERROR
echo.


REM ------------------------
REM Create Compiled Archives
REM ------------------------

:CREATE_COMPILED_ARCHIVES

if "%Alcinoe_Mac_Connection_Profile_Name%"=="" GOTO CLEANUP_TEMP_FILES

echo ------------------------
echo Create Compiled Archives
echo ------------------------
echo.

SET FileName=%ALBaseDir%\Compiled\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"

SET FileName=%ALBaseDir%\Demos\AllDemos.groupproj.local
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

if "%ALBuildDemos%"=="Y" (
  echo Copy %ALBaseDir%\Demos to %ALBaseDir%\Compiled\Demos
  xcopy "%ALBaseDir%\Demos" "%ALBaseDir%\Compiled\Demos\" /s >nul
  IF ERRORLEVEL 1 goto ERROR
)

if "%ALBuildTools%"=="Y" (
  echo Copy %ALBaseDir%\Tools to %ALBaseDir%\Compiled\Tools
  xcopy "%ALBaseDir%\Tools" "%ALBaseDir%\Compiled\Tools\" /s >nul
  IF ERRORLEVEL 1 goto ERROR
)

echo Clean %ALBaseDir%\Compiled

for /r "%ALBaseDir%\Compiled" %%D in (_Source) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (Android) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (Android64) do (
  if exist "%%D" ( 
    xcopy "%%D\*.aab" "%ALBaseDir%\Compiled\tmp\" /s >nul 2>&1
    xcopy "%%D\*.apk" "%ALBaseDir%\Compiled\tmp\" /s >nul 2>&1
    rmdir /s /q "%%D" 
    xcopy "%ALBaseDir%\Compiled\tmp\*.aab" "%%D\" /s >nul 2>&1
    xcopy "%ALBaseDir%\Compiled\tmp\*.apk" "%%D\" /s >nul 2>&1
    rmdir /s /q "%ALBaseDir%\Compiled\tmp\" 
  )
)

for /r "%ALBaseDir%\Compiled" %%D in (iOSDevice64) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (OSX64) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (OSXARM64) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (_Build) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (_Design) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (tmp) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (Output) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (OutputAndroid) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (OutputIOS) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

for /r "%ALBaseDir%\Compiled" %%D in (Data) do (
  if exist "%%D" ( rmdir /s /q "%%D" )
)

SET FileName=%ALBaseDir%\Compiled\Tools\AllTools.groupproj
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%ALBaseDir%\Compiled\Demos\AllDemos.groupproj
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%ALBaseDir%\Compiled\Demos\ALLiveVideoChat\ALLiveVideoChat.groupproj
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%ALBaseDir%\Compiled\Demos\ALLiveVideoChat\ALLiveVideoChat.groupproj.local
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

if "%ALBuildDemos%"=="Y" (
  echo Pack %ALBaseDir%\Compiled\Demos
  PowerShell -Command "Compress-Archive -Path '%ALBaseDir%\Compiled\Demos\*' -DestinationPath '%ALBaseDir%\Compiled\Demos-Compiled.zip' -Force"
  IF ERRORLEVEL 1 goto ERROR
)

if "%ALBuildTools%"=="Y" (
  echo Pack %ALBaseDir%\Compiled\Tools
  PowerShell -Command "Compress-Archive -Path '%ALBaseDir%\Compiled\Tools\*' -DestinationPath '%ALBaseDir%\Compiled\Tools-Compiled.zip' -Force"
  IF ERRORLEVEL 1 goto ERROR
)

SET FileName=%ALBaseDir%\Compiled\Demos\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

SET FileName=%ALBaseDir%\Compiled\Tools\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR

echo.

goto CLEANUP_TEMP_FILES


REM ------------------
REM Cleanup Temp Files
REM ------------------

:CLEANUP_TEMP_FILES

echo ------------------
echo Cleanup Temp Files
echo ------------------
echo.

echo del "%ALBaseDir%\*.cmds"
del "%ALBaseDir%\*.cmds" /s >nul

goto FINISHED


REM -----------------------
REM Function BUILD_FMX_DEMO
REM -----------------------

:BUILD_FMX_DEMO

SET FileName=%~1\Android\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\Android64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\iOSDevice64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\iOSSimARM64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\OSX64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\OSXARM64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" EXIT /B 1

call "%ALBaseDir%\Tools\DProjVersioning\DProjVersioning.exe" -DProj="%~1\%~2\%~3" -Action=incMajorMinorPatchVersion -MajorNumber=2 -MinorNumber=0 -PatchBase=0 -CreateBackup="false"
IF ERRORLEVEL 1 EXIT /B 1

Call :BUILD_VCL_DEMO "%~1" "%~2" "%~3"
IF ERRORLEVEL 1 EXIT /B 1

REM Call :BUILD_PROJECT "%~1" "%~2" "%~3" "Android"
REM IF ERRORLEVEL 1 EXIT /B 1

Call :BUILD_PROJECT "%~1" "%~2" "%~3" "Android64"
IF ERRORLEVEL 1 EXIT /B 1

Call :BUILD_PROJECT "%~1" "%~2" "%~3" "iOSDevice64"
IF ERRORLEVEL 1 EXIT /B 1

REM Call :BUILD_PROJECT "%~1" "%~2" "%~3" "iOSSimARM64"
REM IF ERRORLEVEL 1 EXIT /B 1

REM Call :BUILD_PROJECT "%~1" "%~2" "%~3" "OSX64"
REM IF ERRORLEVEL 1 EXIT /B 1

Call :BUILD_PROJECT "%~1" "%~2" "%~3" "OSXARM64"
IF ERRORLEVEL 1 EXIT /B 1

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
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\Win64\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" EXIT /B 1

Call :BUILD_PROJECT "%~1" "%~2" "%~3" "Win32"
IF ERRORLEVEL 1 EXIT /B 1

Call :BUILD_PROJECT "%~1" "%~2" "%~3" "Win64"
IF ERRORLEVEL 1 EXIT /B 1

EXIT /B 0


REM ----------------------
REM Function BUILD_PROJECT
REM ----------------------

:BUILD_PROJECT

REM %~1 the base directory
REM %~2 the source directory relative to the base directory
REM %~3 the dproj filename without path
REM %~4 the platform

SET FileName=%~1\*.rsm
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\%~2\dbgout.log
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\%~2\*.skincfg
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\%~2\*.identcache
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\%~2\*.dproj.local
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\%~2\*.groupproj.local
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\%~2\*.deployproj.local
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

SET FileName=%~1\%~2\Dcu\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" EXIT /B 1
mkdir "%FileName%"

REM if "%~4"=="Android64" (
REM   echo [36mMerge Android Libraries for %~3[0m
REM   call "%~1\%~2\Android\MergeLibraries.bat"
REM   IF ERRORLEVEL 1 EXIT /B 1
REM   echo.
REM )

set EnvOptions_iOSDevice64_MobileProvisionAppStore=
set EnvOptions_iOSDevice64_CFBundleIdentifierAppStore=
if "%~3"=="ALFmxControlsDemo.dproj" (
  set EnvOptions_iOSDevice64_MobileProvisionAppStore=%Alcinoe_ALFmxControlsDemo_EnvOptions_iOSDevice64_MobileProvisionAppStore%
  set EnvOptions_iOSDevice64_CFBundleIdentifierAppStore=io.magicfoundation.alcinoe.alfmxcontrolsdemo
)
if "%~3"=="ALFmxDynamicListBoxDemo.dproj" (
  set EnvOptions_iOSDevice64_MobileProvisionAppStore=%Alcinoe_ALFmxDynamicListBoxDemo_EnvOptions_iOSDevice64_MobileProvisionAppStore%
  set EnvOptions_iOSDevice64_CFBundleIdentifierAppStore=io.magicfoundation.alcinoe.alfmxdynamiclistboxdemo
)
REM <key>get-task-allow</key>
REM <false/>
REM <key>com.apple.developer.team-identifier</key>
REM <string>%Alcinoe_EnvOptions_iOSDevice64_DevTeamIdAppStore%</string>
SET "EnvOptions_iOSDevice64_EntitlementExtraKeyValuesAppStore=<key>get-task-allow</key><br/>  <false/><br/>  <key>com.apple.developer.team-identifier</key><br/>  <string>%Alcinoe_EnvOptions_iOSDevice64_DevTeamIdAppStore%</string>"

SET ALRunEnvOptionsProjUpdater=N
if "%~4"=="iOSDevice64" (
  if "%~3"=="ALFmxControlsDemo.dproj" Set ALRunEnvOptionsProjUpdater=Y
  if "%~3"=="ALFmxDynamicListBoxDemo.dproj" Set ALRunEnvOptionsProjUpdater=Y
)
if "%Alcinoe_Mac_Connection_Profile_Name%"=="" Set ALRunEnvOptionsProjUpdater=N
if "%ALRunEnvOptionsProjUpdater%"=="Y" (
  call "%ALBaseDir%\Tools\EnvOptionsProjUpdater\EnvOptionsProjUpdater.exe"^
   -BDSVersion="%ALDelphiVersion%"^
   -Platform="iOSDevice64"^
   -ENV_PF_DevDebug=^
   -ENV_PF_MobileProvisionDebug=^
   -ENV_PF_MobileProvisionPathDebug=^
   -ENV_PF_DevTeamIdDebug=^
   -ENV_PF_AppIdentifierDebug=^
   -ENV_PF_KeyChainAccessDebug=^
   -ENV_PF_EntitlementExtraKeyValuesDebug=^
   -ENV_PF_AutoMobileProvisionDebug=^
   -ENV_PF_AutoCertificateDebug=^
   -ENV_PF_CFBundleIdentifierDebug=^
   -ENV_PF_DevAdHoc=^
   -ENV_PF_MobileProvisionAdHoc=^
   -ENV_PF_MobileProvisionPathAdHoc=^
   -ENV_PF_DevTeamIdAdHoc=^
   -ENV_PF_AppIdentifierAdHoc=^
   -ENV_PF_KeyChainAccessAdhoc=^
   -ENV_PF_EntitlementExtraKeyValuesAdhoc=^
   -ENV_PF_AutoMobileProvisionAdHoc=^
   -ENV_PF_AutoCertificateAdHoc=^
   -ENV_PF_CFBundleIdentifierAdHoc=^
   -ENV_PF_InstallAppOnDeviceAdHoc=^
   -ENV_PF_DevAppStore="%Alcinoe_EnvOptions_iOSDevice64_DevAppStore%"^
   -ENV_PF_MobileProvisionAppStore="%EnvOptions_iOSDevice64_MobileProvisionAppStore%"^
   -ENV_PF_MobileProvisionPathAppStore="/Users/%Alcinoe_Mac_Username%/Library/MobileDevice/Provisioning Profiles/%EnvOptions_iOSDevice64_MobileProvisionAppStore%.mobileprovision"^
   -ENV_PF_DevTeamIdAppStore="%Alcinoe_EnvOptions_iOSDevice64_DevTeamIdAppStore%"^
   -ENV_PF_AppIdentifierAppStore="%Alcinoe_EnvOptions_iOSDevice64_DevTeamIdAppStore%.%EnvOptions_iOSDevice64_CFBundleIdentifierAppStore%"^
   -ENV_PF_KeyChainAccessAppStore="%Alcinoe_EnvOptions_iOSDevice64_DevTeamIdAppStore%.%EnvOptions_iOSDevice64_CFBundleIdentifierAppStore%;com.apple.token;"^
   -ENV_PF_EntitlementExtraKeyValuesAppStore="%EnvOptions_iOSDevice64_EntitlementExtraKeyValuesAppStore%"^
   -ENV_PF_AutoMobileProvisionAppStore=^
   -ENV_PF_AutoCertificateAppStore=^
   -ENV_PF_CFBundleIdentifierAppStore="%Alcinoe_EnvOptions_iOSDevice64_DevTeamIdAppStore%.%EnvOptions_iOSDevice64_CFBundleIdentifierAppStore%"
  IF ERRORLEVEL 1 EXIT /B 1 
)

SET ALDeploy=N
if "%~4"=="Android64" Set ALDeploy=Y
if "%~4"=="iOSDevice64" (
  if "%~3"=="ALFmxControlsDemo.dproj" Set ALDeploy=Y
  if "%~3"=="ALFmxDynamicListBoxDemo.dproj" Set ALDeploy=Y
)
if "%Alcinoe_Mac_Connection_Profile_Name%"=="" Set ALDeploy=N
if "%ALDeploy%"=="Y" (
  echo [36mMSBuild %~3 /p:config=Release /p:Platform=%~4 /t:Build;Deploy[0m
  echo Clean PAServer scratch-dir
  ssh %Alcinoe_Mac_Username%@%Alcinoe_Mac_Host% "rm -rf /Users/%Alcinoe_Mac_Username%/PAServer/scratch-dir"
  IF ERRORLEVEL 1 EXIT /B 1
  if "%~4"=="iOSDevice64" ( 
    MSBuild "%~1\%~2\%~3" /p:DCC_UseMSBuildExternally=true /p:Config=Release /p:Platform=%~4 /p:Profile=%Alcinoe_Mac_Connection_Profile_Name% /t:Build;Deploy /verbosity:minimal
  )
  if "%~4" neq "iOSDevice64" ( 
    MSBuild "%~1\%~2\%~3" /p:DCC_UseMSBuildExternally=true /p:Config=Release /p:Platform=%~4 /t:Build;Deploy /verbosity:minimal
  )
  IF ERRORLEVEL 1 EXIT /B 1
  echo.
)
if "%ALDeploy%"=="N" (
  echo [36mMSBuild %~3 /p:config=Release /p:Platform=%~4 /t:Build[0m
  MSBuild "%~1\%~2\%~3" /p:DCC_UseMSBuildExternally=true /p:Config=Release /p:Platform=%~4 /t:Build /verbosity:minimal
  IF ERRORLEVEL 1 EXIT /B 1
  echo.
)

SET FileName=%~1\%~2\Dcu\
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" EXIT /B 1
mkdir "%FileName%"

SET FileName=%~1\%~2\dbgout.log
if exist "%FileName%" del "%FileName%" /s >nul
if exist "%FileName%" EXIT /B 1

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