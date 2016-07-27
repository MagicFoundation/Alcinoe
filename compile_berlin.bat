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

rmdir /s /q lib\alcinoe\Win32\berlin
IF ERRORLEVEL 1 pause

mkdir lib\alcinoe\Win32\berlin
IF ERRORLEVEL 1 pause

MSBuild source\Alcinoe_berlin.dproj /t:build /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 pause

if "%INPUT%"=="Y" goto BUILD_DEMOS
if "%INPUT%"=="y" goto BUILD_DEMOS
goto END

:BUILD_DEMOS

del demos\*.exe /s
IF ERRORLEVEL 1 pause

del demos\*.dll /s
IF ERRORLEVEL 1 pause

del demos\*.vlb /s
IF ERRORLEVEL 1 pause

del demos\*.ini /s
IF ERRORLEVEL 1 pause

del demos\*.html /s
IF ERRORLEVEL 1 pause

CHDIR demos\
FOR /d /R %%J IN (iOSSimulator) DO (	
  @IF EXIST %%J echo rmdir - %%J			
  @IF EXIST %%J rmdir /s /q %%J
  IF ERRORLEVEL 1 pause
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (iOSDevice32) DO (	
  @IF EXIST %%J echo rmdir - %%J			
  @IF EXIST %%J rmdir /s /q %%J
  IF ERRORLEVEL 1 pause
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (Osx32) DO (	
  @IF EXIST %%J echo rmdir - %%J			
  @IF EXIST %%J rmdir /s /q %%J
  IF ERRORLEVEL 1 pause
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (Android) DO (	
  @IF EXIST %%J echo rmdir - %%J			
  @IF EXIST %%J rmdir /s /q %%J
  IF ERRORLEVEL 1 pause
)
CHDIR ..

CHDIR demos\
FOR /d /R %%J IN (Debug) DO (	
  @IF EXIST %%J echo rmdir - %%J			
  @IF EXIST %%J rmdir /s /q %%J
  IF ERRORLEVEL 1 pause
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

del demos\ALCheckSource\*.dproj /s
IF ERRORLEVEL 1 pause

del demos\ALCheckSource\*.res /s
IF ERRORLEVEL 1 pause

CHDIR demos\
FOR /R %%J IN (*.dproj) DO (	
  echo %%J			
  MSBuild %%J /t:build /p:Config=Release /p:Platform=Win32
  MSBuild %%J /t:build /p:Config=RELEASE /p:Platform=Win64
  IF ERRORLEVEL 1 pause
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

xcopy dll\tbbmalloc\win32\tbbmalloc.dll demos\ALDatabaseBenchmark\win32 /s
IF ERRORLEVEL 1 pause

xcopy dll\tbbmalloc\win64\tbbmalloc.dll demos\ALDatabaseBenchmark\win64 /s
IF ERRORLEVEL 1 pause

if exist ..\BlowPipeEmail\_build\source\BlowPipeEmail.dpr (

  del ..\BlowPipeEmail\*.rsm /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeEmail\*.stat /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeEmail\*.dcu /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeEmail\*.exe /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeEmail\*.identcache /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeEmail\*.dproj.local /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeEmail\*.vlb /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeEmail\*.skincfg /s
  IF ERRORLEVEL 1 pause
  
  MSBuild ..\BlowPipeEmail\_build\source\BlowPipeEmail.dproj /t:build /p:Config=Release /p:Platform=Win32
  IF ERRORLEVEL 1 pause
  
  xcopy ..\BlowPipeEmail\BlowPipeEmail.exe demos\BlowPipeEmail /s
  IF ERRORLEVEL 1 pause
  
  xcopy ..\BlowPipeEmail\sqlite3.dll demos\BlowPipeEmail /s
  IF ERRORLEVEL 1 pause

  del ..\BlowPipeEmail\*.dcu /s
  IF ERRORLEVEL 1 pause

)  

if exist ..\BlowPipeSMS\_build\source\BlowPipeSMS.dpr (

  del ..\BlowPipeSMS\*.rsm /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeSMS\*.stat /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeSMS\*.dcu /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeSMS\*.exe /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeSMS\*.identcache /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeSMS\*.dproj.local /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeSMS\*.vlb /s
  IF ERRORLEVEL 1 pause
  
  del ..\BlowPipeSMS\*.skincfg /s
  IF ERRORLEVEL 1 pause
  
  MSBuild ..\BlowPipeSMS\_build\source\BlowPipeSMS.dproj /t:build /p:Config=Release /p:Platform=Win32
  IF ERRORLEVEL 1 pause
  
  xcopy ..\BlowPipeSMS\BlowPipeSMS.exe demos\BlowPipeSMS /s
  IF ERRORLEVEL 1 pause
  
  xcopy ..\BlowPipeSMS\sqlite3.dll demos\BlowPipeSMS /s
  IF ERRORLEVEL 1 pause

  del ..\BlowPipeSMS\*.dcu /s
  IF ERRORLEVEL 1 pause

)  

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
