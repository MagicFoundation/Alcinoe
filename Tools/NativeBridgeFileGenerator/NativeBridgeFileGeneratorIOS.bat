@echo off
SETLOCAL

REM ----------------
REM Init Environment
REM ----------------

call "..\..\InitEnvironment.bat"
IF ERRORLEVEL 1 goto ERROR


REM --------------
REM Security check
REM --------------

if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR


REM -----------------
REM Update local vars
REM -----------------

FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%ALDelphiVersion%\Environment Variables" /v BDSPLATFORMSDKSDIR`) DO set BDSPLATFORMSDKSDIR=%%A
FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%ALDelphiVersion%\PlatformSDKs" /v Default_iOSDevice64`) DO set Default_iOSDevice64=%%A
set PlatformSDK=%Default_iOSDevice64%
set SystemRoot=%BDSPLATFORMSDKSDIR%\%PlatformSDK%

echo BDSPLATFORMSDKSDIR=%BDSPLATFORMSDKSDIR%
echo PlatformSDK=%PlatformSDK%
echo SystemRoot=%SystemRoot%
echo.


REM ---------------------------
REM Create TMPDir and OutputDir
REM ---------------------------

SET TMPDir=%ALBaseDir%\Tools\NativeBridgeFileGenerator\Tmp
IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR
mkdir "%TMPDir%"
  
SET OutputDir=%ALBaseDir%\Tools\NativeBridgeFileGenerator\OutputIOS
IF EXIST "%OutputDir%" rmdir /s /q "%OutputDir%"
IF EXIST "%OutputDir%" goto ERROR
mkdir "%OutputDir%"


REM ---------------------------------------
REM Copy to TMPDir all data from SystemRoot
REM ---------------------------------------

echo Copy "%SystemRoot%" to "%TMPDir%"
xcopy "%SystemRoot%" "%TMPDir%" /E /Q
Set TMPSystemRoot=%TMPDir%


REM ------------------------------
REM Ask the library framework path
REM ------------------------------

echo.
echo Please copy all the custom Frameworks you need in a single
echo directory and gave below the path to this directory (empty to skip):
set CustomFrameworksPath=
set /P CustomFrameworksPath=%=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input


REM -------------------------------------------------
REM Copy to TMPDir all data from CustomFrameworksPath
REM -------------------------------------------------

if "%CustomFrameworksPath%" NEQ "" (
  echo Copy "%CustomFrameworksPath%" to "%TMPDir%\System\Library\Frameworks"
  xcopy "%CustomFrameworksPath%" "%TMPDir%\System\Library\Frameworks" /E /Q
)


REM -----------------
REM call SdkTransform
REM -----------------

"%ALDelphiDir%\bin\SdkTransform.exe"^
 -cc1^
 -g^
 -w^
 -D TARGET_OS_IPHONE^
 -isysroot "%TMPSystemRoot%"^
 -isystem "%TMPSystemRoot%\usr\include"^
 -isystem "%TMPSystemRoot%\usr\lib\clang\include"^
 -F "%TMPSystemRoot%\System\Library\Frameworks"^
 -triple thumbv7-apple-ios^
 -fdiagnostics-show-option^
 -fexceptions^
 -fobjc-exceptions^
 -x objective-c^
 -std=gnu99^
 -nobuiltininc^
 -nostdinc++^
 -nostdsysteminc^
 -fblocks^
 --out:%OutputDir%
IF ERRORLEVEL 1 goto ERROR


REM -------------
REM remove TMPDir
REM -------------

IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR

SET FileName=%ALBaseDir%\Tools\NativeBridgeFileGenerator\tempht.m
del "%FileName%" /s
if exist "%FileName%" goto ERROR

GOTO FINISHED


REM -------------------
REM FINISHED/ERROR/EXIT
REM -------------------

:FINISHED

echo.
echo Finished
PAUSE
goto EXIT

:ERROR

PAUSE
EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:EXIT