@SET BDS=C:\Program Files (x86)\Embarcadero\Studio\18.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v3.5
@SET FrameworkVersion=v3.5
@SET PATH=%FrameworkDir%;%PATH%

@echo off

set INPUT=
set /P INPUT=Build demos (Y/N)?: %=%

SET FileName=*.skincfg
del %FileName% /s
if exist %FileName% pause

SET FileName=*.rsm
del %FileName% /s
if exist %FileName% pause

SET FileName=*.stat
del %FileName% /s
if exist %FileName% pause

SET FileName=*.identcache
del %FileName% /s
if exist %FileName% pause

SET FileName=*.dproj.local
del %FileName% /s
if exist %FileName% pause

SET FileName=*.deployproj.local
del %FileName% /s
if exist %FileName% pause

SET FileName=*.deployproj
del %FileName% /s
if exist %FileName% pause

SET FileName=*.dres
del %FileName% /s
if exist %FileName% pause

SET FileName=*.rc
del %FileName% /s
if exist %FileName% pause

SET FileName=source\dcu\Win32\berlin
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% pause
mkdir %FileName%

SET FileName=source\hpp\Win32\berlin
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% pause
mkdir %FileName%

SET FileName=lib\alcinoe\Win32\berlin
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% pause
mkdir %FileName%

MSBuild source\Alcinoe_berlin.dproj /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 pause

call compilejar_berlin.bat off

if "%INPUT%"=="Y" goto BUILD_DEMOS
if "%INPUT%"=="y" goto BUILD_DEMOS
goto END

:BUILD_DEMOS

SET FileName=demos\*.vlb
del %FileName% /s
if exist %FileName% pause

CHDIR demos\
FOR /d /R %%J IN (Android) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    IF EXIST %%J echo rmdir - %%J			
    IF EXIST %%J rmdir /s /q %%J
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

CHDIR demos\
FOR /R %%J IN (*.dproj) DO (	
  echo %%J			
  MSBuild %%J /p:Config=Release /p:Platform=Win32 /t:Build
  IF ERRORLEVEL 1 pause
  MSBuild %%J /p:Config=Release /p:Platform=Win64 /t:Build
  IF ERRORLEVEL 1 pause
)
CHDIR ..

CHDIR demos\
FOR /R %%J IN (ALFmx*.dproj) DO (	
  echo %%J			
  MSBuild %%J /p:Config=Release /p:Platform=Android /t:Build
  MSBuild %%J /p:Config=Release /p:Platform=Android /t:Deploy
  IF ERRORLEVEL 1 pause
  MSBuild %%J /p:Config=Release /p:Platform=iOSSimulator /t:Build
  IF ERRORLEVEL 1 pause
  MSBuild %%J /p:Config=Release /p:Platform=iOSDevice32 /t:Build
  IF ERRORLEVEL 1 pause
  MSBuild %%J /p:Config=Release /p:Platform=iOSDevice64 /t:Build
  IF ERRORLEVEL 1 pause
  MSBuild %%J /p:Config=Release /p:Platform=OSX32 /t:Build
  IF ERRORLEVEL 1 pause
)
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

xcopy dll\tbbmalloc\win32\tbbmalloc.dll demos\ALDatabaseBenchmark\win32 /s
IF ERRORLEVEL 1 pause

xcopy dll\tbbmalloc\win64\tbbmalloc.dll demos\ALDatabaseBenchmark\win64 /s
IF ERRORLEVEL 1 pause


:END

SET FileName=source\dcu\Win32\berlin
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% pause
mkdir %FileName%

@echo Finished
PAUSE 
