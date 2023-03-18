@echo off
setlocal

REM ---------------------------------------------
REM Add this Pre-Build event (android platform) :
REM <Alcinoe>\Tools\RJarSwapper\RJarSwapper.bat -RJarDir="<directory where r-apk.jar and r-aab.jar are located>" -IsAabPackage="true if you build an .aab package, false if you build an .apk"
REM ---------------------------------------------

:parmloop
if "%~1" neq "" set "%~1=%~2"&shift&shift&goto parmloop

set RJarDir=%-RJarDir%
REM set RJarFile=%RJarDir%\r.jar
set RJar32bitFile=%RJarDir%\32bit\r.jar
set RJar64bitFile=%RJarDir%\64bit\r.jar

REM del "%RJarFile%"
REM if exist "%RJarFile%" goto ERROR

del "%RJar32bitFile%"
if exist "%RJar32bitFile%" goto ERROR

del "%RJar64bitFile%"
if exist "%RJar64bitFile%" goto ERROR

if "%-IsAabPackage%" == "true" goto IsAabPackage
goto IsApkPackage

:IsAabPackage 
REM copy "%RJarDir%\r-aab.jar" "%RJarFile%" > nul
copy "%RJarDir%\r-aab.jar" "%RJar32bitFile%" > nul
copy "%RJarDir%\r-aab.jar" "%RJar64bitFile%" > nul
goto EXIT

:IsApkPackage
REM copy "%RJarDir%\r-apk.jar" "%RJarFile%" > nul
copy "%RJarDir%\r-apk.jar" "%RJar32bitFile%" > nul
copy "%RJarDir%\r-apk.jar" "%RJar64bitFile%" > nul
goto EXIT

:ERROR

:EXIT