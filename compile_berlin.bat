@SET BDS=C:\Program Files (x86)\Embarcadero\Studio\18.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v3.5
@SET FrameworkVersion=v3.5
@SET PATH=%FrameworkDir%;%PATH%

@echo off

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

SET FileName=source\dcu\Win32\berlin
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=source\hpp\Win32\berlin
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=lib\bpl\alcinoe\Win32\berlin
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

MSBuild source\Alcinoe_berlin.dproj /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 goto ERROR

call compilejar_berlin.bat off

if "%INPUT%"=="Y" goto BUILD_DEMOS
if "%INPUT%"=="y" goto BUILD_DEMOS
goto FINISHED

:BUILD_DEMOS

CHDIR demos\
MSBuild ALFmxControls\_source\ALFmxControls_berlin.dproj /p:Config=Release /p:Platform=Win32 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls_berlin.dproj /p:Config=Release /p:Platform=Win64 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls_berlin.dproj /p:Config=Release /p:Platform=Android /t:Build
MSBuild ALFmxControls\_source\ALFmxControls_berlin.dproj /p:Config=Release /p:Platform=Android /t:Deploy
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls_berlin.dproj /p:Config=Release /p:Platform=iOSSimulator /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls_berlin.dproj /p:Config=Release /p:Platform=iOSDevice32 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls_berlin.dproj /p:Config=Release /p:Platform=iOSDevice64 /t:Build
IF ERRORLEVEL 1 PAUSE
MSBuild ALFmxControls\_source\ALFmxControls_berlin.dproj /p:Config=Release /p:Platform=OSX32 /t:Build
IF ERRORLEVEL 1 PAUSE
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (Release) DO (	  
  Echo.%%J | findstr /C:"Android">nul && (
    IF EXIST %%J del %%J * /q     
  ) || (
    REM skip if not Android/Release
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (dcu) DO (	
  IF EXIST %%J echo rmdir - %%J			
  IF EXIST %%J (
    rmdir /s /q %%J
    mkdir %%J
  )
)
CHDIR ..

:FINISHED

SET FileName=source\dcu\Win32\berlin
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

@echo Finished
PAUSE
goto EXIT 

:ERROR
pause

:EXIT
