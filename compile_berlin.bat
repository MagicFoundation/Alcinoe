@SET BDS=C:\Program Files (x86)\Embarcadero\Studio\18.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v3.5
@SET FrameworkVersion=v3.5
@SET PATH=%FrameworkDir%;%PATH%

@echo off

set INPUT=
set /P INPUT=Build demos (Y/N)?: %=%

del *.skincfg /s
IF ERRORLEVEL 1 pause

del *.rsm /s
IF ERRORLEVEL 1 pause

del *.stat /s
IF ERRORLEVEL 1 pause

del *.identcache /s
IF ERRORLEVEL 1 pause

del *.dproj.local /s
IF ERRORLEVEL 1 pause

del *.deployproj.local /s
IF ERRORLEVEL 1 pause

rmdir /s /q source\dcu\Win32\berlin
IF ERRORLEVEL 1 pause

mkdir source\dcu\Win32\berlin
IF ERRORLEVEL 1 pause

rmdir /s /q source\hpp\Win32\berlin
IF ERRORLEVEL 1 pause

mkdir source\hpp\Win32\berlin
IF ERRORLEVEL 1 pause

rmdir /s /q lib\alcinoe\Win32\berlin
IF ERRORLEVEL 1 pause

mkdir lib\alcinoe\Win32\berlin
IF ERRORLEVEL 1 pause

MSBuild source\Alcinoe_berlin.dproj /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 pause

if "%INPUT%"=="Y" goto BUILD_DEMOS
if "%INPUT%"=="y" goto BUILD_DEMOS
goto END

:BUILD_DEMOS

del demos\*.vlb /s
IF ERRORLEVEL 1 pause

CHDIR demos\
FOR /d /R %%J IN (Android) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    @IF EXIST %%J echo rmdir - %%J			
    @IF EXIST %%J rmdir /s /q %%J
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (iOSSimulator) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    @IF EXIST %%J echo rmdir - %%J			
    @IF EXIST %%J rmdir /s /q %%J
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (iOSDevice32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    @IF EXIST %%J echo rmdir - %%J			
    @IF EXIST %%J rmdir /s /q %%J
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (iOSDevice64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    @IF EXIST %%J echo rmdir - %%J			
    @IF EXIST %%J rmdir /s /q %%J
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (Osx32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    @IF EXIST %%J echo rmdir - %%J			
    @IF EXIST %%J rmdir /s /q %%J
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (win32) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    @IF EXIST %%J echo rmdir - %%J			
    @IF EXIST %%J rmdir /s /q %%J
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (win64) DO (	  
  Echo.%%J | findstr /C:"_source">nul && (
    REM do not delete inside /_source/
  ) || (
    @IF EXIST %%J echo rmdir - %%J			
    @IF EXIST %%J rmdir /s /q %%J
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (dcu) DO (	
  @IF EXIST %%J echo rmdir - %%J			
  @IF EXIST %%J (
    rmdir /s /q %%J
    mkdir %%J
  )
  IF ERRORLEVEL 1 pause
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
    @IF EXIST %%J del %%J * /q     
  ) || (
    REM skip if not Android/Release
  )
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (dcu) DO (	
  @IF EXIST %%J echo rmdir - %%J			
  @IF EXIST %%J (
    rmdir /s /q %%J
    mkdir %%J
  )
)
CHDIR ..

xcopy dll\tbbmalloc\win32\tbbmalloc.dll demos\ALDatabaseBenchmark\win32 /s
IF ERRORLEVEL 1 pause

xcopy dll\tbbmalloc\win64\tbbmalloc.dll demos\ALDatabaseBenchmark\win64 /s
IF ERRORLEVEL 1 pause

del Alcinoe.zip

C:\Progra~2\7-Zip\7za.exe a -tzip -r Alcinoe.zip * -x!_svn* -x!.svn* -x!*.dcu -x!*.bpl -x!*__history* -x!references* -x!archive*
IF ERRORLEVEL 1 pause

:END

rmdir /s /q source\dcu\Win32\berlin
IF ERRORLEVEL 1 pause

mkdir source\dcu\Win32\berlin
IF ERRORLEVEL 1 pause

@echo Finished
PAUSE 
