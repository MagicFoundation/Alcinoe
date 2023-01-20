@SET BDS=C:\Program Files\Embarcadero\RAD Studio\9.0
@SET BDSCOMMONDIR=C:\Users\Public\Documents\RAD Studio\9.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v3.5
@SET FrameworkVersion=v3.5
@SET FrameworkSDKDir=
@SET PATH=%FrameworkDir%;%FrameworkSDKDir%;C:\Program Files\Embarcadero\RAD Studio\9.0\bin;C:\Program Files\Embarcadero\RAD Studio\9.0\bin64;%PATH%
@SET LANGDIR=EN

@echo off

del *.rsm /s
IF ERRORLEVEL 1 goto ERROR

del *.identcache /s
IF ERRORLEVEL 1 goto ERROR

del *.dproj.local /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.dcu /s
del lib\dxe2\*.dcu /s
del source\*.dcu /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.bpl /s
del lib\dxe2\*.bpl /s
del source\*.bpl /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.~bpl /s
del lib\dxe2\*.~bpl /s
del source\*.~bpl /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.exe /s
del lib\dxe2\*.exe /s
del source\*.exe /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.ini /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.html /s
IF ERRORLEVEL 1 goto ERROR

MSBuild source\Alcinoe_dxe2.dproj /t:build /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 goto ERROR

pause

MSBuild source\Alcinoe_dxe2.dproj /t:build /p:Config=Release /p:Platform=Win64
IF ERRORLEVEL 1 goto ERROR

pause

MSBuild demo\ProjectGroupDemo.dxe2.groupproj /t:build /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 goto ERROR

pause

del demo\*.dcu /s
IF ERRORLEVEL 1 goto ERROR

MSBuild demo\ProjectGroupDemo.dxe2.groupproj /t:build /p:Config=Release /p:Platform=Win64
IF ERRORLEVEL 1 goto ERROR

pause

if exist ..\..\BlowPipeEmail\_build\Source\BlowPipeEmail.dpr (

  del ..\..\BlowPipeEmail\*.rsm /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeEmail\*.dcu /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeEmail\*.exe /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeEmail\*.ini /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeEmail\*.identcache /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeEmail\*.dproj.local /s
  IF ERRORLEVEL 1 goto ERROR
  
  MSBuild ..\..\BlowPipeEmail\_build\Source\BlowPipeEmail.dproj /t:build /p:Config=Release /p:Platform=Win32
  IF ERRORLEVEL 1 goto ERROR
  
  pause
  
  xcopy ..\..\BlowPipeEmail\BlowPipeEmail.exe demo\BlowPipeEmail /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeEmail\*.dcu /s
  IF ERRORLEVEL 1 goto ERROR

)  

if exist ..\..\BlowPipeSMS\_build\Source\BlowPipeSMS.dpr (

  del ..\..\BlowPipeSMS\*.rsm /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeSMS\*.dcu /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeSMS\*.exe /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeSMS\*.ini /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeSMS\*.identcache /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeSMS\*.dproj.local /s
  IF ERRORLEVEL 1 goto ERROR
  
  MSBuild ..\..\BlowPipeSMS\_build\Source\BlowPipeSMS.dproj /t:build /p:Config=Release /p:Platform=Win32
  IF ERRORLEVEL 1 goto ERROR
  
  pause
  
  xcopy ..\..\BlowPipeSMS\BlowPipeSMS.exe demo\BlowPipeSMS /s
  IF ERRORLEVEL 1 goto ERROR
  
  del ..\..\BlowPipeSMS\*.dcu /s
  IF ERRORLEVEL 1 goto ERROR

)  

del Release\Alcinoe.zip

C:\Progra~1\7-Zip\7za.exe a -tzip -r release\Alcinoe.zip * -x!_svn* -x!.svn* -x!*.dcu -x!*.bpl -x!*__history* -x!release*
IF ERRORLEVEL 1 goto ERROR

del *.dcu /s
IF ERRORLEVEL 1 goto ERROR

GOTO END

:ERROR
PAUSE
EXIT

:END
 