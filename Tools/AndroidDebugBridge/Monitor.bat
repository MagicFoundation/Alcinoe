@echo off
SETLOCAL

REM ----------------
REM Init Environment
REM ----------------

if "%ALBaseDir%"=="" (
  
  Set InitEnvironmentQuietMode=1
  call "%~dp0\..\..\InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR
  
)

REM ------------
REM Init ADBPath
REM ------------

set ADBPath=
for /f "tokens=2*" %%A in ('reg query "HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\%ALDelphiVersion%\Environment Variables" /v "BDSPLATFORMSDKSDIR"') do set ADBPath=%%B
set ADBPath=%ADBPath%\android\platform-tools\adb.exe
IF NOT EXIST "%ADBPath%" (
  echo Error: Unable to locate adb.exe using the path specified in HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\%ALDelphiVersion%\Environment Variables. Please ensure that the ADB path is correctly set in the registry and that adb.exe is available in the specified location.
  goto ERROR
)


REM --------------
REM Choose Command
REM --------------

:CHOOSE_COMMAND

echo 1) Monitor CPU and Memory Usage (Real-Time) [adb shell top]
echo 2) Get Detailed Memory Breakdown [adb shell dumpsys meminfo]

set COMMAND=
set /P COMMAND="Enter the command you want to run:" %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input

:Init_PackageName

if NOT "%Package_Name%"=="" GOTO RUN_COMMAND

set Package_Name=
set /P Package_Name=Enter the package name of the app (Empty to skip): %=%
echo.

:RUN_COMMAND

if "%COMMAND%"=="1" (
  if NOT "%Package_Name%"=="" GOTO adb_shell_top_PACKAGE
  goto adb_shell_top_ALL
)
if "%COMMAND%"=="2" (
  goto adb_shell_dumpsys_meminfo
)
goto CHOOSE_COMMAND


REM -------------
REM adb shell top
REM -------------

:adb_shell_top_ALL

call "%ADBPath%" shell top
GOTO END

:adb_shell_top_PACKAGE

for /f "tokens=*" %%i in ('"%ADBPath%" shell pidof %Package_Name%') do "%ADBPath%" shell top -p %%i
GOTO END


REM -------------------------
REM adb shell dumpsys meminfo
REM -------------------------

:adb_shell_dumpsys_meminfo

call "%ADBPath%" shell dumpsys meminfo %Package_Name%
GOTO END

:ERROR
REM EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:END
PAUSE