@SET BDS=C:\Program Files\CodeGear\RAD Studio\5.0
@SET BDSCOMMONDIR=C:\Users\Public\Documents\RAD Studio\5.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\
@SET FrameworkVersion=v2.0.50727
@SET FrameworkSDKDir=

@ECHO Setting environment for using CodeGear RAD Studio tools

@SET PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%PATH%

@echo off

del *.identcache /s
IF ERRORLEVEL 1 goto ERROR

del *.dproj.local /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.dcu /s
del lib\d2007\*.dcu /s
del source\*.dcu /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.bpl /s
del lib\d2007\*.bpl /s
del source\*.bpl /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.~bpl /s
del lib\d2007\*.~bpl /s
del source\*.~bpl /s
IF ERRORLEVEL 1 goto ERROR

del demo\*.exe /s
del lib\d2007\*.exe /s
del source\*.exe /s
IF ERRORLEVEL 1 goto ERROR

MSBuild source\Alcinoe_d2007.dproj /t:build /p:Configuration=RELEASE
IF ERRORLEVEL 1 goto ERROR

pause

MSBuild demo\ProjectGroupDemo.d2007.groupproj /t:build /p:Configuration=RELEASE
IF ERRORLEVEL 1 goto ERROR

pause

del release\Alcinoe.zip

C:\Progra~1\7-Zip\7za.exe a -tzip -r release\Alcinoe.zip * -x!_svn* -x!.svn* -x!*.dcu -x!*.bpl -x!*__history* -x!release*
IF ERRORLEVEL 1 goto ERROR

del *.dcu /s
IF ERRORLEVEL 1 goto ERROR

GOTO END

:ERROR
PAUSE
EXIT

:END
 