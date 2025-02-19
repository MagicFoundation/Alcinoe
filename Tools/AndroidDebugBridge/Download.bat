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


REM ---------------------------------
REM Choose androidFile and destFolder
REM ---------------------------------

if NOT "%androidFile%"=="" GOTO RUN_COMMAND

echo Enter the full path of the file/folder to download (e.g., /storage/emulated/0/Download/myfile.txt):
set androidFile=
set /P androidFile="" %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input

echo Enter the destination folder on your PC (leave empty for current folder):
set destFolder=
set /P destFolder="" %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
if "%destFolder%"=="" set destFolder=.

set /p deleteFile="Do you want to delete the file on the Android device? (Y/N, default=no): "
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
if "%deleteFile%"=="y" set ALNoPrompts=Y


REM -----------------
REM Download the file
REM -----------------

:RUN_COMMAND

call "%ADBPath%" pull "%androidFile%" "%destFolder%"
IF ERRORLEVEL 1 goto ERROR

if "%deleteFile%"=="Y" (
  call "%ADBPath%" shell rm -f "%androidFile%"
  IF ERRORLEVEL 1 goto ERROR
)

@echo Finished
GOTO END




:ERROR
REM EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:END
PAUSE