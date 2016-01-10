@SET BDS=C:\Program Files (x86)\Embarcadero\Studio\17.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v3.5
@SET FrameworkVersion=v3.5
@SET PATH=%FrameworkDir%;%PATH%

@echo off

del *.rsm /s
IF ERRORLEVEL 1 goto ERROR

del *.stat /s
IF ERRORLEVEL 1 goto ERROR

del *.identcache /s
IF ERRORLEVEL 1 goto ERROR

del *.dproj.local /s
IF ERRORLEVEL 1 goto ERROR

del *.dcu /s
IF ERRORLEVEL 1 goto ERROR

del demos\*.exe /s
IF ERRORLEVEL 1 goto ERROR

del demos\*.dll /s
IF ERRORLEVEL 1 goto ERROR

del demos\*.vlb /s
IF ERRORLEVEL 1 goto ERROR

del demos\*.ini /s
IF ERRORLEVEL 1 goto ERROR

del demos\*.html /s
IF ERRORLEVEL 1 goto ERROR

del *.skincfg /s
IF ERRORLEVEL 1 goto ERROR

del demos\ALCheckSource\*.dproj /s
IF ERRORLEVEL 1 goto ERROR

del demos\ALCheckSource\*.res /s
IF ERRORLEVEL 1 goto ERROR

rmdir /s /q dcu
IF ERRORLEVEL 1 goto ERROR

mkdir dcu
IF ERRORLEVEL 1 goto ERROR

rmdir /s /q lib\alcinoe
IF ERRORLEVEL 1 goto ERROR

mkdir lib\alcinoe
IF ERRORLEVEL 1 goto ERROR

MSBuild source\Alcinoe.dproj /t:build /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 goto ERROR

CHDIR demos\
FOR /R %%J IN (*.dproj) DO (	
  echo %%J			
  MSBuild %%J /t:build /p:Config=Release /p:Platform=Win32
  MSBuild %%J /t:build /p:Config=RELEASE /p:Platform=Win64
  IF ERRORLEVEL 1 pause
)
CHDIR ..

pause

if exist ..\BlowPipeEmail\_build\source\BlowPipeEmail.dpr (

  del ..\BlowPipeEmail\*.rsm /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeEmail\*.stat /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeEmail\*.dcu /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeEmail\*.exe /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeEmail\*.identcache /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeEmail\*.dproj.local /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeEmail\*.vlb /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeEmail\*.skincfg /s
  IF ERRORLEVEL 1 goto ERROR
  
  MSBuild ..\BlowPipeEmail\_build\source\BlowPipeEmail.dproj /t:build /p:Config=Release /p:Platform=Win32
  IF ERRORLEVEL 1 goto ERROR
  
  pause
  
  xcopy ..\BlowPipeEmail\BlowPipeEmail.exe demos\BlowPipeEmail /s
  IF ERRORLEVEL 1 goto ERROR
  
  xcopy ..\BlowPipeEmail\sqlite3.dll demos\BlowPipeEmail /s
  IF ERRORLEVEL 1 goto ERROR

  del ..\BlowPipeEmail\*.dcu /s
  IF ERRORLEVEL 1 goto ERROR

)  

if exist ..\BlowPipeSMS\_build\source\BlowPipeSMS.dpr (

  del ..\BlowPipeSMS\*.rsm /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeSMS\*.stat /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeSMS\*.dcu /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeSMS\*.exe /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeSMS\*.identcache /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeSMS\*.dproj.local /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeSMS\*.vlb /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\BlowPipeSMS\*.skincfg /s
  IF ERRORLEVEL 1 goto ERROR
  
  MSBuild ..\BlowPipeSMS\_build\source\BlowPipeSMS.dproj /t:build /p:Config=Release /p:Platform=Win32
  IF ERRORLEVEL 1 goto ERROR
  
  pause
  
  xcopy ..\BlowPipeSMS\BlowPipeSMS.exe demos\BlowPipeSMS /s
  IF ERRORLEVEL 1 goto ERROR
  
  xcopy ..\BlowPipeSMS\sqlite3.dll demos\BlowPipeSMS /s
  IF ERRORLEVEL 1 goto ERROR

  del ..\BlowPipeSMS\*.dcu /s
  IF ERRORLEVEL 1 goto ERROR

)  

del Alcinoe.zip

C:\Progra~2\7-Zip\7za.exe a -tzip -r Alcinoe.zip * -x!_svn* -x!.svn* -x!*.dcu -x!*.bpl -x!*__history* -x!references* -x!archive*
IF ERRORLEVEL 1 goto ERROR

del *.dcu /s
IF ERRORLEVEL 1 goto ERROR

GOTO END

:ERROR
PAUSE
EXIT

:END
 