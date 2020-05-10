@echo off

call "C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\rsvars.bat"
IF ERRORLEVEL 1 goto ERROR

FOR %%a IN ("%%~dp0") DO set "ProjectDir=%%~dpa"
IF %ProjectDir:~-1%==\ SET ProjectDir=%ProjectDir:~0,-1%

set INPUT=
set /P INPUT=Build demos (Y/N)?: %=%

call %ProjectDir%\tools\UnitNormalizer\UnitNormalizer.exe "%ProjectDir%\source\" "false"
IF ERRORLEVEL 1 goto ERROR

call %ProjectDir%\tools\UnitNormalizer\UnitNormalizer.exe "%ProjectDir%\demos\" "false"
IF ERRORLEVEL 1 goto ERROR

call %ProjectDir%\tools\UnitNormalizer\UnitNormalizer.exe "%ProjectDir%\tools\" "false"
IF ERRORLEVEL 1 goto ERROR

SET FileName=%ProjectDir%\*.skincfg
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=%ProjectDir%\*.rsm
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=%ProjectDir%\*.stat
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=%ProjectDir%\*.identcache
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=%ProjectDir%\*.dproj.local
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=%ProjectDir%\*.deployproj.local
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=%ProjectDir%\source\dcu\Win32\rio
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=%ProjectDir%\source\hpp\Win32\rio
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=%ProjectDir%\lib\bpl\alcinoe\Win32\rio
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

MSBuild %ProjectDir%\source\Alcinoe_rio.dproj /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 goto ERROR

call compilejar.bat off
IF ERRORLEVEL 1 goto ERROR

if "%INPUT%"=="Y" goto BUILD_DEMOS
if "%INPUT%"=="y" goto BUILD_DEMOS
goto FINISHED

:BUILD_DEMOS

SET FileName=%ProjectDir%\demos\*.vlb
del %FileName% /s
if exist %FileName% goto ERROR

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (Android) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (Android64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (iOSSimulator) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (iOSDevice32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (iOSDevice64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (Osx32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (win32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (win64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (dcu) DO (	
  IF EXIST %%J echo rmdir - %%J			
  IF EXIST %%J (
    rmdir /s /q %%J
    if EXIST %%J goto ERROR
    mkdir %%J
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /R %%J IN (*.dproj) DO (	
  echo %%J			
  MSBuild %%J /p:Config=Release /p:Platform=Win32 /t:Build
  IF ERRORLEVEL 1 PAUSE
  MSBuild %%J /p:Config=Release /p:Platform=Win64 /t:Build
  IF ERRORLEVEL 1 PAUSE
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
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
MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=iOSDevice32 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFmxControls\_source\ALFmxControls.dproj /p:Config=Release /p:Platform=OSX32 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
MSBuild ALFirebaseMessagingDemo\_source\ALFirebaseMessagingDemo.dproj /p:Config=Release /p:Platform=Android /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFirebaseMessagingDemo\_source\ALFirebaseMessagingDemo.dproj /p:Config=Release /p:Platform=Android /t:Deploy
IF ERRORLEVEL 1 PAUSE
MSBuild ALFirebaseMessagingDemo\_source\ALFirebaseMessagingDemo.dproj /p:Config=Release /p:Platform=Android64 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFirebaseMessagingDemo\_source\ALFirebaseMessagingDemo.dproj /p:Config=Release /p:Platform=Android64 /t:Deploy
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFirebaseMessagingDemo\_source\ALFirebaseMessagingDemo.dproj /p:Config=Release /p:Platform=iOSSimulator /t:Build
REM IF ERRORLEVEL 1 PAUSE
MSBuild ALFirebaseMessagingDemo\_source\ALFirebaseMessagingDemo.dproj /p:Config=Release /p:Platform=iOSDevice32 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFirebaseMessagingDemo\_source\ALFirebaseMessagingDemo.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFirebaseMessagingDemo\_source\ALFirebaseMessagingDemo.dproj /p:Config=Release /p:Platform=OSX32 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
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
MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=iOSDevice32 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFacebookLogin\_source\ALFacebookLogin.dproj /p:Config=Release /p:Platform=OSX32 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
MSBuild ALFmxEffects\_source\ALFmxEffects.dproj /p:Config=Release /p:Platform=Android /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxEffects\_source\ALFmxEffects.dproj /p:Config=Release /p:Platform=Android /t:Deploy
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxEffects\_source\ALFmxEffects.dproj /p:Config=Release /p:Platform=Android64 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxEffects\_source\ALFmxEffects.dproj /p:Config=Release /p:Platform=Android64 /t:Deploy
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFmxEffects\_source\ALFmxEffects.dproj /p:Config=Release /p:Platform=iOSSimulator /t:Build
REM IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxEffects\_source\ALFmxEffects.dproj /p:Config=Release /p:Platform=iOSDevice32 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxEffects\_source\ALFmxEffects.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALFmxEffects\_source\ALFmxEffects.dproj /p:Config=Release /p:Platform=OSX32 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
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
MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=iOSDevice32 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
REM MSBuild ALLiveVideoChat\client\_source\ALLiveVideoChatClient.dproj /p:Config=Release /p:Platform=OSX32 /t:Build
REM IF ERRORLEVEL 1 PAUSE
CHDIR %ProjectDir%\

SET FileName=ALFmxControls
xcopy %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\%FileName%\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\%FileName%\Android\Release rmdir /s /q %ProjectDir%\demos\%FileName%\Android\Release
IF EXIST %ProjectDir%\demos\%FileName%\Android\Release goto ERROR
mkdir %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\%FileName%\Android\%FileName%.apk %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\%FileName%\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\%FileName%\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\%FileName%\Android64\Release rmdir /s /q %ProjectDir%\demos\%FileName%\Android64\Release
IF EXIST %ProjectDir%\demos\%FileName%\Android64\Release goto ERROR
mkdir %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\%FileName%\Android64\%FileName%.apk %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\%FileName%\Android64\%FileName%.apk
if exist %FileName% goto ERROR


SET FileName=ALFirebaseMessagingDemo
xcopy %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\%FileName%\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\%FileName%\Android\Release rmdir /s /q %ProjectDir%\demos\%FileName%\Android\Release
IF EXIST %ProjectDir%\demos\%FileName%\Android\Release goto ERROR
mkdir %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\%FileName%\Android\%FileName%.apk %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\%FileName%\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\%FileName%\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\%FileName%\Android64\Release rmdir /s /q %ProjectDir%\demos\%FileName%\Android64\Release
IF EXIST %ProjectDir%\demos\%FileName%\Android64\Release goto ERROR
mkdir %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\%FileName%\Android64\%FileName%.apk %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\%FileName%\Android64\%FileName%.apk
if exist %FileName% goto ERROR


SET FileName=ALFacebookLogin
xcopy %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\%FileName%\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\%FileName%\Android\Release rmdir /s /q %ProjectDir%\demos\%FileName%\Android\Release
IF EXIST %ProjectDir%\demos\%FileName%\Android\Release goto ERROR
mkdir %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\%FileName%\Android\%FileName%.apk %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\%FileName%\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\%FileName%\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\%FileName%\Android64\Release rmdir /s /q %ProjectDir%\demos\%FileName%\Android64\Release
IF EXIST %ProjectDir%\demos\%FileName%\Android64\Release goto ERROR
mkdir %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\%FileName%\Android64\%FileName%.apk %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\%FileName%\Android64\%FileName%.apk
if exist %FileName% goto ERROR


SET FileName=ALFmxEffects
xcopy %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\%FileName%\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\%FileName%\Android\Release rmdir /s /q %ProjectDir%\demos\%FileName%\Android\Release
IF EXIST %ProjectDir%\demos\%FileName%\Android\Release goto ERROR
mkdir %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\%FileName%\Android\%FileName%.apk %ProjectDir%\demos\%FileName%\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\%FileName%\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\%FileName%\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\%FileName%\Android64\Release rmdir /s /q %ProjectDir%\demos\%FileName%\Android64\Release
IF EXIST %ProjectDir%\demos\%FileName%\Android64\Release goto ERROR
mkdir %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\%FileName%\Android64\%FileName%.apk %ProjectDir%\demos\%FileName%\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\%FileName%\Android64\%FileName%.apk
if exist %FileName% goto ERROR


SET FileName=ALLiveVideoChatClient
xcopy %ProjectDir%\demos\ALLiveVideoChat\client\Android\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\ALLiveVideoChat\client\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\ALLiveVideoChat\client\Android\Release rmdir /s /q %ProjectDir%\demos\ALLiveVideoChat\client\Android\Release
IF EXIST %ProjectDir%\demos\ALLiveVideoChat\client\Android\Release goto ERROR
mkdir %ProjectDir%\demos\ALLiveVideoChat\client\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\ALLiveVideoChat\client\Android\%FileName%.apk %ProjectDir%\demos\ALLiveVideoChat\client\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\ALLiveVideoChat\client\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy %ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release\%FileName%\bin\%FileName%.apk %ProjectDir%\demos\ALLiveVideoChat\client\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST %ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release rmdir /s /q %ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release
IF EXIST %ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release goto ERROR
mkdir %ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy %ProjectDir%\demos\ALLiveVideoChat\client\Android64\%FileName%.apk %ProjectDir%\demos\ALLiveVideoChat\client\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del %ProjectDir%\demos\ALLiveVideoChat\client\Android64\%FileName%.apk
if exist %FileName% goto ERROR

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (dcu) DO (	
  IF EXIST %%J echo rmdir - %%J			
  IF EXIST %%J (
    rmdir /s /q %%J
    if EXIST %%J goto ERROR
    mkdir %%J
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (iOSDevice32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

CHDIR %ProjectDir%\demos\
FOR /d /R %%J IN (iOSDevice64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR %ProjectDir%\

xcopy %ProjectDir%\lib\dll\tbbmalloc\win32\tbbmalloc.dll %ProjectDir%\demos\ALDatabaseBenchmark\win32 /s
IF ERRORLEVEL 1 goto ERROR

xcopy %ProjectDir%\lib\dll\tbbmalloc\win64\tbbmalloc.dll %ProjectDir%\demos\ALDatabaseBenchmark\win64 /s
IF ERRORLEVEL 1 goto ERROR


:FINISHED

SET FileName=%ProjectDir%\source\dcu\Win32\rio
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

@echo Finished
PAUSE
goto EXIT 

:ERROR
pause

:EXIT
