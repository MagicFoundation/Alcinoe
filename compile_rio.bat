@echo off

call "C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\rsvars.bat"

set INPUT=
set /P INPUT=Build demos (Y/N)?: %=%

SET FileName=*.skincfg
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.rsm
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.stat
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.identcache
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.dproj.local
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.deployproj.local
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=source\dcu\Win32\rio
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=source\hpp\Win32\rio
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=lib\bpl\alcinoe\Win32\rio
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

MSBuild source\Alcinoe_rio.dproj /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 goto ERROR

call compilejar.bat off

if "%INPUT%"=="Y" goto BUILD_DEMOS
if "%INPUT%"=="y" goto BUILD_DEMOS
goto FINISHED

:BUILD_DEMOS

SET FileName=demos\*.vlb
del %FileName% /s
if exist %FileName% goto ERROR

CHDIR demos\
FOR /d /R %%J IN (Android) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (Android64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (iOSSimulator) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (iOSDevice32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (iOSDevice64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (Osx32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (win32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (win64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (dcu) DO (	
  IF EXIST %%J echo rmdir - %%J			
  IF EXIST %%J (
    rmdir /s /q %%J
    if EXIST %%J goto ERROR
    mkdir %%J
  )
)
CHDIR ..

CHDIR demos\
FOR /R %%J IN (*.dproj) DO (	
  echo %%J			
  MSBuild %%J /p:Config=Release /p:Platform=Win32 /t:Build
  IF ERRORLEVEL 1 PAUSE
  MSBuild %%J /p:Config=Release /p:Platform=Win64 /t:Build
  IF ERRORLEVEL 1 PAUSE
)
CHDIR ..

CHDIR demos\
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
CHDIR ..

CHDIR demos\
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
CHDIR ..

CHDIR demos\
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
CHDIR ..

CHDIR demos\
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
CHDIR ..

CHDIR demos\
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
CHDIR ..

SET FileName=ALFmxControls
xcopy demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk demos\%FileName%\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\%FileName%\Android\Release rmdir /s /q demos\%FileName%\Android\Release
IF EXIST demos\%FileName%\Android\Release goto ERROR
mkdir demos\%FileName%\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\%FileName%\Android\%FileName%.apk demos\%FileName%\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\%FileName%\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk demos\%FileName%\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\%FileName%\Android64\Release rmdir /s /q demos\%FileName%\Android64\Release
IF EXIST demos\%FileName%\Android64\Release goto ERROR
mkdir demos\%FileName%\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\%FileName%\Android64\%FileName%.apk demos\%FileName%\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\%FileName%\Android64\%FileName%.apk
if exist %FileName% goto ERROR


SET FileName=ALFirebaseMessagingDemo
xcopy demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk demos\%FileName%\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\%FileName%\Android\Release rmdir /s /q demos\%FileName%\Android\Release
IF EXIST demos\%FileName%\Android\Release goto ERROR
mkdir demos\%FileName%\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\%FileName%\Android\%FileName%.apk demos\%FileName%\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\%FileName%\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk demos\%FileName%\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\%FileName%\Android64\Release rmdir /s /q demos\%FileName%\Android64\Release
IF EXIST demos\%FileName%\Android64\Release goto ERROR
mkdir demos\%FileName%\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\%FileName%\Android64\%FileName%.apk demos\%FileName%\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\%FileName%\Android64\%FileName%.apk
if exist %FileName% goto ERROR


SET FileName=ALFacebookLogin
xcopy demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk demos\%FileName%\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\%FileName%\Android\Release rmdir /s /q demos\%FileName%\Android\Release
IF EXIST demos\%FileName%\Android\Release goto ERROR
mkdir demos\%FileName%\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\%FileName%\Android\%FileName%.apk demos\%FileName%\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\%FileName%\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk demos\%FileName%\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\%FileName%\Android64\Release rmdir /s /q demos\%FileName%\Android64\Release
IF EXIST demos\%FileName%\Android64\Release goto ERROR
mkdir demos\%FileName%\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\%FileName%\Android64\%FileName%.apk demos\%FileName%\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\%FileName%\Android64\%FileName%.apk
if exist %FileName% goto ERROR


SET FileName=ALFmxEffects
xcopy demos\%FileName%\Android\Release\%FileName%\bin\%FileName%.apk demos\%FileName%\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\%FileName%\Android\Release rmdir /s /q demos\%FileName%\Android\Release
IF EXIST demos\%FileName%\Android\Release goto ERROR
mkdir demos\%FileName%\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\%FileName%\Android\%FileName%.apk demos\%FileName%\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\%FileName%\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy demos\%FileName%\Android64\Release\%FileName%\bin\%FileName%.apk demos\%FileName%\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\%FileName%\Android64\Release rmdir /s /q demos\%FileName%\Android64\Release
IF EXIST demos\%FileName%\Android64\Release goto ERROR
mkdir demos\%FileName%\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\%FileName%\Android64\%FileName%.apk demos\%FileName%\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\%FileName%\Android64\%FileName%.apk
if exist %FileName% goto ERROR


SET FileName=ALLiveVideoChatClient
xcopy demos\ALLiveVideoChat\client\Android\Release\%FileName%\bin\%FileName%.apk demos\ALLiveVideoChat\client\Android 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\ALLiveVideoChat\client\Android\Release rmdir /s /q demos\ALLiveVideoChat\client\Android\Release
IF EXIST demos\ALLiveVideoChat\client\Android\Release goto ERROR
mkdir demos\ALLiveVideoChat\client\Android\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\ALLiveVideoChat\client\Android\%FileName%.apk demos\ALLiveVideoChat\client\Android\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\ALLiveVideoChat\client\Android\%FileName%.apk
if exist %FileName% goto ERROR

xcopy demos\ALLiveVideoChat\client\Android64\Release\%FileName%\bin\%FileName%.apk demos\ALLiveVideoChat\client\Android64 
IF ERRORLEVEL 1 goto ERROR
IF EXIST demos\ALLiveVideoChat\client\Android64\Release rmdir /s /q demos\ALLiveVideoChat\client\Android64\Release
IF EXIST demos\ALLiveVideoChat\client\Android64\Release goto ERROR
mkdir demos\ALLiveVideoChat\client\Android64\Release\%FileName%\bin\
IF ERRORLEVEL 1 goto ERROR
xcopy demos\ALLiveVideoChat\client\Android64\%FileName%.apk demos\ALLiveVideoChat\client\Android64\Release\%FileName%\bin 
IF ERRORLEVEL 1 goto ERROR
del demos\ALLiveVideoChat\client\Android64\%FileName%.apk
if exist %FileName% goto ERROR

CHDIR demos\
FOR /d /R %%J IN (dcu) DO (	
  IF EXIST %%J echo rmdir - %%J			
  IF EXIST %%J (
    rmdir /s /q %%J
    if EXIST %%J goto ERROR
    mkdir %%J
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (iOSDevice32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (iOSDevice64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
    if EXIST %%J goto ERROR
  )
)
CHDIR ..

xcopy lib\dll\tbbmalloc\win32\tbbmalloc.dll demos\ALDatabaseBenchmark\win32 /s
IF ERRORLEVEL 1 goto ERROR

xcopy lib\dll\tbbmalloc\win64\tbbmalloc.dll demos\ALDatabaseBenchmark\win64 /s
IF ERRORLEVEL 1 goto ERROR


:FINISHED

SET FileName=source\dcu\Win32\rio
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

@echo Finished
PAUSE
goto EXIT 

:ERROR
pause

:EXIT
