@SET BDS=C:\Program Files\CodeGear\RAD Studio\5.0
@SET BDSCOMMONDIR=C:\Users\Public\Documents\RAD Studio\5.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\
@SET FrameworkVersion=v2.0.50727
@SET FrameworkSDKDir=

@ECHO Setting environment for using CodeGear RAD Studio tools

@SET PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%PATH%

@echo off

del *.dcu /s
IF ERRORLEVEL 1 goto ERROR

del *.bpl /s
IF ERRORLEVEL 1 goto ERROR

del *.~bpl /s
IF ERRORLEVEL 1 goto ERROR

del *.exe /s
IF ERRORLEVEL 1 goto ERROR

del *.ini /s
IF ERRORLEVEL 1 goto ERROR

del *.html /s
IF ERRORLEVEL 1 goto ERROR

MSBuild source\Alcinoe_D2007.dproj /t:build /p:Configuration=RELEASE
IF ERRORLEVEL 1 goto ERROR

pause

MSBuild demo\ProjectGroupDemo.groupproj /t:build /p:Configuration=RELEASE
IF ERRORLEVEL 1 goto ERROR

pause

del release\Alcinoe.rar

C:\Progra~1\WinRAR\RAR.exe a -ep1 -r release\Alcinoe.rar -x*\_svn* -x*.dcu -x*.bpl -x*\__history* -x*\release\* *.*
IF ERRORLEVEL 1 goto ERROR

del *.dcu /s
IF ERRORLEVEL 1 goto ERROR

GOTO END

:ERROR
PAUSE
EXIT

:END
 