@echo off

REM -------------------
REM Choose the compiler
REM -------------------

:CHOOSE_COMPILER

set COMPILER=
  
cls
echo 1) Rio
echo 2) Sydney

set COMPILER=
set /P COMPILER=Enter number to select a compiler: %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL as instruction below set ERRORLEVEL to 1 if empty input

if "%COMPILER%"=="1" (
  set DELPHI_NAME=rio
  set DELPHI_VERSION=20.0
  goto INIT_LOCAL_VARS
)
if "%COMPILER%"=="2" (
  set DELPHI_NAME=sydney
  set DELPHI_VERSION=21.0
  goto INIT_LOCAL_VARS
)

goto CHOOSE_COMPILER


REM ---------------
REM Init local vars
REM ---------------

:INIT_LOCAL_VARS

FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%DELPHI_VERSION%" /v RootDir`) DO set DelphiRootDir=%%A %%B 
set DelphiRootDir=%DelphiRootDir:~0,-1%
set DelphiBinDir=%DelphiRootDir%bin

call "%DelphiBinDir%\rsvars.bat"
IF ERRORLEVEL 1 goto ERROR

FOR %%a IN ("%%~dp0") DO set "ProjectDir=%%~dpa"
IF %ProjectDir:~-1%==\ SET ProjectDir=%ProjectDir:~0,-1%

set INPUT=
set /P INPUT=Build demos (Y/N)?: %=%


REM -------------------
REM Normalize all units
REM -------------------

call "%ProjectDir%\tools\UnitNormalizer\UnitNormalizer.exe" "%ProjectDir%\source\" "false"
IF ERRORLEVEL 1 goto ERROR

call "%ProjectDir%\tools\UnitNormalizer\UnitNormalizer.exe" "%ProjectDir%\demos\" "false"
IF ERRORLEVEL 1 goto ERROR

call "%ProjectDir%\tools\UnitNormalizer\UnitNormalizer.exe" "%ProjectDir%\tools\" "false"
IF ERRORLEVEL 1 goto ERROR


REM -----------------
REM clean directories
REM -----------------

SET FileName=%ProjectDir%\*.rsm
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ProjectDir%\*.stat
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ProjectDir%\*.identcache
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ProjectDir%\*.dproj.local
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ProjectDir%\*.groupproj.local
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ProjectDir%\*.deployproj.local
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ProjectDir%\source\dcu\Win32\%DELPHI_NAME%
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"

SET FileName=%ProjectDir%\source\hpp\Win32\%DELPHI_NAME%
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"

SET FileName=%ProjectDir%\lib\bpl\alcinoe\Win32\%DELPHI_NAME%
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"


REM ---------
REM Build bpl 
REM ---------

MSBuild "%ProjectDir%\source\Alcinoe_%DELPHI_NAME%.dproj" /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 goto ERROR


REM ----------
REM Build jars 
REM ----------

call compileJar.bat off "%DelphiRootDir%"
IF ERRORLEVEL 1 goto ERROR


REM -----------
REM Build Demos 
REM -----------


if "%INPUT%"=="Y" goto BUILD_DEMOS
if "%INPUT%"=="y" goto BUILD_DEMOS
goto FINISHED

:BUILD_DEMOS

SET FileName=%ProjectDir%\demos\*.vlb
del "%FileName%" /s
if exist "%FileName%" goto ERROR

CHDIR "%ProjectDir%\demos\"
FOR /d /R %%J IN (Android) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST "%%J" echo rmdir - %%J			
    IF EXIST "%%J" rmdir /s /q "%%J"
    if EXIST "%%J" goto ERROR
  )
)
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
FOR /d /R %%J IN (Android64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST "%%J" echo rmdir - %%J			
    IF EXIST "%%J" rmdir /s /q "%%J"
    if EXIST "%%J" goto ERROR
  )
)
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
FOR /d /R %%J IN (iOSSimulator) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST "%%J" echo rmdir - %%J			
    IF EXIST "%%J" rmdir /s /q "%%J"
    if EXIST "%%J" goto ERROR
  )
)
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
FOR /d /R %%J IN (iOSDevice64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST "%%J" echo rmdir - %%J			
    IF EXIST "%%J" rmdir /s /q "%%J"
    if EXIST "%%J" goto ERROR
  )
)
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
FOR /d /R %%J IN (win32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST "%%J" echo rmdir - %%J			
    IF EXIST "%%J" rmdir /s /q "%%J"
    if EXIST "%%J" goto ERROR
  )
)
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
FOR /d /R %%J IN (win64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST "%%J" echo rmdir - %%J			
    IF EXIST "%%J" rmdir /s /q "%%J"
    if EXIST "%%J" goto ERROR
  )
)
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
FOR /d /R %%J IN (dcu) DO (	
  IF EXIST "%%J" echo rmdir - %%J			
  IF EXIST "%%J" (
    rmdir /s /q "%%J"
    if EXIST "%%J" goto ERROR
    mkdir "%%J"
  )
)
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
FOR /R %%J IN (*.dproj) DO (	
  echo %%J			
  MSBuild "%%J" /p:Config=Release /p:Platform=Win32 /t:Build
  IF ERRORLEVEL 1 PAUSE
  MSBuild "%%J" /p:Config=Release /p:Platform=Win64 /t:Build
  IF ERRORLEVEL 1 PAUSE
)
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=Android /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=Android /t:Deploy
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=Android64 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=Android64 /t:Deploy
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=iOSSimulator /t:Build
REM IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=OSX64 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
MSBuild ALFirebaseMessaging\_source\ALFirebaseMessaging.dproj /p:Config=Release /p:Platform=Android /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFirebaseMessaging\_source\ALFirebaseMessaging.dproj /p:Config=Release /p:Platform=Android /t:Deploy
IF ERRORLEVEL 1 PAUSE
MSBuild ALFirebaseMessaging\_source\ALFirebaseMessaging.dproj /p:Config=Release /p:Platform=Android64 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFirebaseMessaging\_source\ALFirebaseMessaging.dproj /p:Config=Release /p:Platform=Android64 /t:Deploy
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFirebaseMessaging\_source\ALFirebaseMessaging.dproj /p:Config=Release /p:Platform=iOSSimulator /t:Build
REM IF ERRORLEVEL 1 PAUSE
MSBuild ALFirebaseMessaging\_source\ALFirebaseMessaging.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFirebaseMessaging\_source\ALFirebaseMessaging.dproj /p:Config=Release /p:Platform=OSX64 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=Android /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=Android /t:Deploy
IF ERRORLEVEL 1 PAUSE
MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=Android64 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=Android64 /t:Deploy
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=iOSSimulator /t:Build
REM IF ERRORLEVEL 1 PAUSE
MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=OSX64 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
MSBuild ALFmxFilterEffects\_source\ALFmxFilterEffectsDemo.dproj /p:Config=Release /p:Platform=Android /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxFilterEffects\_source\ALFmxFilterEffectsDemo.dproj /p:Config=Release /p:Platform=Android /t:Deploy
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxFilterEffects\_source\ALFmxFilterEffectsDemo.dproj /p:Config=Release /p:Platform=Android64 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxFilterEffects\_source\ALFmxFilterEffectsDemo.dproj /p:Config=Release /p:Platform=Android64 /t:Deploy
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFmxFilterEffects\_source\ALFmxFilterEffectsDemo.dproj /p:Config=Release /p:Platform=iOSSimulator /t:Build
REM IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxFilterEffects\_source\ALFmxFilterEffectsDemo.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFmxFilterEffects\_source\ALFmxFilterEffectsDemo.dproj /p:Config=Release /p:Platform=OSX64 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=Android /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=Android /t:Deploy
IF ERRORLEVEL 1 PAUSE
MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=Android64 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=Android64 /t:Deploy
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=iOSSimulator /t:Build
REM IF ERRORLEVEL 1 PAUSE
MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=OSX64 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR "%ProjectDir%\"


SET FileName=ALFmxControls
xcopy "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\%FileName%\Android\Release" rmdir /s /q "%ProjectDir%\demos\%FileName%\Android\Release"
IF EXIST "%ProjectDir%\demos\%FileName%\Android\Release" goto ERROR
mkdir "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\%FileName%\Android\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\%FileName%\Android\%FileName%.apk"
if exist "%FileName%" goto ERROR

xcopy "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android64"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\%FileName%\Android64\Release" rmdir /s /q "%ProjectDir%\demos\%FileName%\Android64\Release"
IF EXIST "%ProjectDir%\demos\%FileName%\Android64\Release" goto ERROR
mkdir "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\%FileName%\Android64\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\%FileName%\Android64\%FileName%.apk"
if exist "%FileName%" goto ERROR


SET FileName=ALFirebaseMessaging
xcopy "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\%FileName%\Android\Release" rmdir /s /q "%ProjectDir%\demos\%FileName%\Android\Release"
IF EXIST "%ProjectDir%\demos\%FileName%\Android\Release" goto ERROR
mkdir "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\%FileName%\Android\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\%FileName%\Android\%FileName%.apk"
if exist "%FileName%" goto ERROR

xcopy "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android64"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\%FileName%\Android64\Release" rmdir /s /q "%ProjectDir%\demos\%FileName%\Android64\Release"
IF EXIST "%ProjectDir%\demos\%FileName%\Android64\Release" goto ERROR
mkdir "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\%FileName%\Android64\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\%FileName%\Android64\%FileName%.apk"
if exist "%FileName%" goto ERROR


SET FileName=ALFacebookLogin
xcopy "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\%FileName%\Android\Release" rmdir /s /q "%ProjectDir%\demos\%FileName%\Android\Release"
IF EXIST "%ProjectDir%\demos\%FileName%\Android\Release" goto ERROR
mkdir "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\%FileName%\Android\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\%FileName%\Android\%FileName%.apk"
if exist "%FileName%" goto ERROR

xcopy "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android64"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\%FileName%\Android64\Release" rmdir /s /q "%ProjectDir%\demos\%FileName%\Android64\Release"
IF EXIST "%ProjectDir%\demos\%FileName%\Android64\Release" goto ERROR
mkdir "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\%FileName%\Android64\%FileName%.apk" "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\%FileName%\Android64\%FileName%.apk"
if exist "%FileName%" goto ERROR


SET FileName=ALFmxFilterEffects
xcopy "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%Demo\bin\%FileName%Demo.apk" "%ProjectDir%\demos\%FileName%\Android"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\%FileName%\Android\Release" rmdir /s /q "%ProjectDir%\demos\%FileName%\Android\Release"
IF EXIST "%ProjectDir%\demos\%FileName%\Android\Release" goto ERROR
mkdir "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%Demo\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\%FileName%\Android\%FileName%Demo.apk" "%ProjectDir%\demos\%FileName%\Android\Release\%FileName%Demo\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\%FileName%\Android\%FileName%Demo.apk"
if exist "%FileName%" goto ERROR

xcopy "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%Demo\bin\%FileName%Demo.apk" "%ProjectDir%\demos\%FileName%\Android64"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\%FileName%\Android64\Release" rmdir /s /q "%ProjectDir%\demos\%FileName%\Android64\Release"
IF EXIST "%ProjectDir%\demos\%FileName%\Android64\Release" goto ERROR
mkdir "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%Demo\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\%FileName%\Android64\%FileName%Demo.apk" "%ProjectDir%\demos\%FileName%\Android64\Release\%FileName%Demo\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\%FileName%\Android64\%FileName%Demo.apk"
if exist "%FileName%" goto ERROR


SET FileName=ALLiveVideoChatClient
xcopy "%ProjectDir%\demos\ALLiveVideoChat\client\Android\Release\%FileName%\bin\%FileName%.apk" "%ProjectDir%\demos\ALLiveVideoChat\client\Android"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\ALLiveVideoChat\client\Android\Release" rmdir /s /q "%ProjectDir%\demos\ALLiveVideoChat\client\Android\Release"
IF EXIST "%ProjectDir%\demos\ALLiveVideoChat\client\Android\Release" goto ERROR
mkdir "%ProjectDir%\demos\ALLiveVideoChat\client\Android\Release\%FileName%\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\ALLiveVideoChat\client\Android\%FileName%.apk" "%ProjectDir%\demos\ALLiveVideoChat\client\Android\Release\%FileName%\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\ALLiveVideoChat\client\Android\%FileName%.apk"
if exist "%FileName%" goto ERROR

xcopy "%ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release\%FileName%\bin\%FileName%.apk" "%ProjectDir%\demos\ALLiveVideoChat\client\Android64"
IF ERRORLEVEL 1 goto ERROR
IF EXIST "%ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release" rmdir /s /q "%ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release"
IF EXIST "%ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release" goto ERROR
mkdir "%ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release\%FileName%\bin\"
IF ERRORLEVEL 1 goto ERROR
xcopy "%ProjectDir%\demos\ALLiveVideoChat\client\Android64\%FileName%.apk" "%ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release\%FileName%\bin"
IF ERRORLEVEL 1 goto ERROR
del "%ProjectDir%\demos\ALLiveVideoChat\client\Android64\%FileName%.apk"
if exist "%FileName%" goto ERROR

CHDIR "%ProjectDir%\demos\"
FOR /d /R %%J IN (dcu) DO (	
  IF EXIST "%%J" echo rmdir - %%J			
  IF EXIST "%%J" (
    rmdir /s /q "%%J"
    if EXIST "%%J" goto ERROR
    mkdir "%%J"
  )
)
CHDIR "%ProjectDir%\"

CHDIR "%ProjectDir%\demos\"
FOR /d /R %%J IN (iOSDevice64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST "%%J" echo rmdir - %%J			
    IF EXIST "%%J" rmdir /s /q "%%J"
    if EXIST "%%J" goto ERROR
  )
)
CHDIR "%ProjectDir%\"

xcopy "%ProjectDir%\lib\dll\tbbmalloc\win32\tbbmalloc.dll" "%ProjectDir%\demos\ALDatabaseBenchmark\win32" /s
IF ERRORLEVEL 1 goto ERROR

xcopy "%ProjectDir%\lib\dll\tbbmalloc\win64\tbbmalloc.dll" "%ProjectDir%\demos\ALDatabaseBenchmark\win64" /s
IF ERRORLEVEL 1 goto ERROR


REM ----
REM EXIT
REM ----

:FINISHED

SET FileName=%ProjectDir%\source\dcu\Win32\%DELPHI_NAME%
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"

@echo Finished
PAUSE
goto EXIT 

:ERROR
pause

:EXIT