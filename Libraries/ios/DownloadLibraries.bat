@echo off
SETLOCAL

REM ----------------
REM Init Environment
REM ----------------

if "%ALBaseDir%"=="" (    
  Set Standalone=1
  call "..\..\InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR
  echo.
)


REM --------------
REM Security check
REM --------------

if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR


REM -----------------
REM Main Instructions
REM -----------------

Call :DOWNLOAD_FACEBOOK_LIBRARY "https://github.com/facebook/facebook-ios-sdk/releases/download/v15.1.0/FacebookSDK-Static_XCFramework.zip" "%ALBaseDir%\Libraries\ios\facebook"
Call :DOWNLOAD_FIREBASE_LIBRARY "https://github.com/firebase/firebase-ios-sdk/releases/download/10.2.0/Firebase.zip" "%ALBaseDir%\Libraries\ios\firebase"

goto FINISHED


REM ----------------------------------
REM Function DOWNLOAD_FIREBASE_LIBRARY
REM ----------------------------------

:DOWNLOAD_FIREBASE_LIBRARY

REM %~1 the url where to download the library
REM %~2 the Directory where to extract the library

echo [36mDownload %~1[0m

set SrcUrl=%~1
Set DestDir=%~2
IF EXIST "%DestDir%" rmdir /s /q "%DestDir%"
IF EXIST "%DestDir%" goto ERROR
mkdir "%DestDir%"

SET TMPDir=%ALBaseDir%\Tmp
IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR
mkdir "%TMPDir%"

curl --fail --location "%SrcUrl%" --output "%TMPDir%\tmp.zip"
IF ERRORLEVEL 1 goto ERROR

tar -xf "%TMPDir%\tmp.zip" -C "%TMPDir%"
IF ERRORLEVEL 1 goto ERROR

Robocopy "%TMPDir%\Firebase" "%DestDir%" /MOVE /E /NFL /NDL /NJH /NJS
IF %ERRORLEVEL% NEQ 1 goto ERROR

IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR

EXIT /B 0


REM ----------------------------------
REM Function DOWNLOAD_FACEBOOK_LIBRARY
REM ----------------------------------

:DOWNLOAD_FACEBOOK_LIBRARY

REM %~1 the url where to download the library
REM %~2 the Directory where to extract the library

echo [36mDownload %~1[0m

set SrcUrl=%~1
Set DestDir=%~2
IF EXIST "%DestDir%" rmdir /s /q "%DestDir%"
IF EXIST "%DestDir%" goto ERROR
mkdir "%DestDir%"

SET TMPDir=%ALBaseDir%\Tmp
IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR
mkdir "%TMPDir%"

curl --fail --location "%SrcUrl%" --output "%TMPDir%\tmp.zip"
IF ERRORLEVEL 1 goto ERROR

tar -xf "%TMPDir%\tmp.zip" -C "%TMPDir%"
IF ERRORLEVEL 1 goto ERROR

Robocopy "%TMPDir%\XCFrameworks" "%DestDir%" /MOVE /E /NFL /NDL /NJH /NJS
IF %ERRORLEVEL% NEQ 1 goto ERROR

IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR

EXIT /B 0


REM -------------------
REM FINISHED/ERROR/EXIT
REM -------------------

:FINISHED

echo.
echo iOS libraries downloaded successfully
if "%Standalone%"=="1" PAUSE 
goto EXIT

:ERROR

PAUSE
EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:EXIT