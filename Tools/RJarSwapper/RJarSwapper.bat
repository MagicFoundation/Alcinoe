@echo off
setlocal

REM ---------------------------------------------
REM Add this Pre-Build event (android platform) :
REM <Alcinoe>\Tools\RJarSwapper\RJarSwapper.bat -RJarDir="<directory where r-apk.jar and r-aab.jar are located>" -IsAabPackage="true if you build an .aab package, false if you build an .apk"
REM ---------------------------------------------

:parmloop
if "%~1" neq "" set "%~1=%~2"&shift&shift&goto parmloop

set RJarDir=%-RJarDir%
set RJarFile=%RJarDir%\r.jar

del "%RJarFile%"
if exist "%RJarFile%" goto ERROR

if "%-IsAabPackage%" == "true" goto IsAabPackage
goto IsApkPackage

:IsAabPackage 
copy "%RJarDir%\r-aab.jar" "%RJarFile%"
goto EXIT

:IsApkPackage
copy "%RJarDir%\r-apk.jar" "%RJarFile%"
goto EXIT

:ERROR

:EXIT