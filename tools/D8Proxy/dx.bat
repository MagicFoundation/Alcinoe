@echo off

REM ----------------------------------------------
REM 
REM This batch file is a proxy to D8.bat
REM https://quality.embarcadero.com/browse/RSP-21513
REM https://developer.android.com/studio/command-line/d8 
REM 
REM ----------------------------------------------

setlocal enableextensions enabledelayedexpansion

set librariesRootPath=C:\Dev\Alcinoe\lib\jar

set FOLDER=%0
for /D %%D in (%FOLDER%) do (
    set PARENT=%%~dpD
)
set logFile=%PARENT%dx.log
set tmpFile=%PARENT%dx.~tmp

rem Define file size limit for the log file with 4 MiB.
set "SizeLimit=1048576"
for %%I in ("%logFile%") do if %%~zI GEQ %SizeLimit% del "%logFile%" /s

IF EXIST %logFile% (
  @echo. >> %logFile% 2>&1
  @echo *************************** >> %logFile% 2>&1
  @echo. >> %logFile% 2>&1
) ELSE (
  @echo. > %logFile%
)

SET _dxCmd=%0 %* 
@echo _dxCmd=%_dxCmd% >> %logFile% 2>&1
set _extraParams=

Echo.%_dxCmd% | findstr /C:"alcinoe-webrtc.jar">nul && (
  set _extraParams= --classpath=%librariesRootPath%\org.webrtc\webrtc.jar
) || (
  Echo.%_dxCmd% | findstr /C:"webrtc.jar">nul && (
    REM nothing to do
  ) || (
    set _extraParams= --no-desugaring
  )
)
set _extraParams=%_extraParams% --release
@echo _extraParams=%_extraParams% >> %logFile% 2>&1

SET _d8Cmd=%_dxCmd:dx.bat"=d8.bat"!_extraParams! --lib C:\SDKs\android-sdk-windows\platforms\android-28\android.jar%
SET _d8Cmd=%_d8Cmd:\classes.dex=%
SET _d8Cmd=%_d8Cmd:--dex =%
@echo _d8Cmd=%_d8Cmd% >> %logFile% 2>&1
   
call %_d8Cmd% > %tmpFile% 2>&1
FOR /F "usebackq" %%A IN ('%tmpFile%') DO set size=%%~zA
if %size% GTR 0 (
  more %tmpFile% >> %logFile% 2>&1
  more %tmpFile%
  del %tmpFile% /s
  Exit /b 1
)
del %tmpFile% /s





