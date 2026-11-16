@echo off
SETLOCAL

REM ----------------
REM Init Environment
REM ----------------

if "%ALBaseDir%"=="" (    
  Set Standalone=1
  call "%~dp0\..\..\InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR
  echo.
)


REM --------------
REM Security check
REM --------------

if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR


REM -------------------------------------
REM Check if symbolic links can be created
REM -------------------------------------

REM The Mac Catalyst slices of the frameworks contain symbolic links, which
REM Windows can only create as administrator or in Developer Mode. Otherwise
REM tar fails on them, so the Mac Catalyst slices are skipped.

set TarExclude=
set SymlinkTest=%TEMP%\ALSymlinkTest.tmp
del "%SymlinkTest%" >nul 2>&1
mklink "%SymlinkTest%" "%SymlinkTest%.target" >nul 2>&1
IF ERRORLEVEL 1 (
  set TarExclude=--exclude "*maccatalyst*"
  echo [33mWARNING: the Mac Catalyst slices of the frameworks will be skipped.[0m
  echo.
  echo [33mMac Catalyst is the platform used to run iPad apps on macOS. Its slices in the[0m
  echo [33mxcframeworks use the standard macOS framework layout, which relies on symbolic[0m
  echo [33mlinks, e.g. Headers is a link to Versions/Current/Headers.[0m
  echo.
  echo [33mWindows only allows creating symbolic links when running as administrator or[0m
  echo [33mwhen Developer Mode is enabled, which is not the case here, so tar would fail[0m
  echo [33mto extract these slices.[0m
  echo.
  echo [33mThis has no impact on Delphi iOS apps: the iOS device and iOS Simulator slices[0m
  echo [33mare downloaded as usual. To also download the Mac Catalyst slices, enable[0m
  echo [33mDeveloper Mode in Settings ^> System ^> For developers, or run this script[0m
  echo [33mas administrator.[0m
  echo.
) ELSE (
  del "%SymlinkTest%"
)


REM -----------------
REM Main Instructions
REM -----------------

Call :DOWNLOAD_FACEBOOK_LIBRARY "https://github.com/facebook/facebook-ios-sdk/releases/download/v18.0.0/FacebookSDK-Static_XCFramework.zip" "%ALBaseDir%\Libraries\ios\Facebook\Static"
Call :DOWNLOAD_FACEBOOK_LIBRARY "https://github.com/facebook/facebook-ios-sdk/releases/download/v18.0.0/FacebookSDK_Dynamic.xcframework.zip" "%ALBaseDir%\Libraries\ios\Facebook\Dynamic"
                                
Call :DOWNLOAD_FIREBASE_LIBRARY "https://github.com/firebase/firebase-ios-sdk/releases/download/10.12.0/Firebase.zip" "%ALBaseDir%\Libraries\ios\Firebase\Static"

Call :DOWNLOAD_MAPLIBRE_LIBRARY "https://github.com/maplibre/maplibre-native/releases/download/ios-v6.31.0/MapLibre.static.xcframework.zip" "%ALBaseDir%\Libraries\ios\MapLibre\Static"
Call :DOWNLOAD_MAPLIBRE_LIBRARY "https://github.com/maplibre/maplibre-native/releases/download/ios-v6.31.0/MapLibre.dynamic.xcframework.zip" "%ALBaseDir%\Libraries\ios\MapLibre\Dynamic"

goto FINISHED


REM ----------------------------------
REM Function DOWNLOAD_MAPLIBRE_LIBRARY
REM ----------------------------------

:DOWNLOAD_MAPLIBRE_LIBRARY

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

mkdir "%TMPDir%\Output"

tar -xf "%TMPDir%\tmp.zip" -C "%TMPDir%\Output" %TarExclude%
IF ERRORLEVEL 1 goto ERROR

Robocopy "%TMPDir%\Output" "%DestDir%" /MOVE /E /NFL /NDL /NJH /NJS
IF %ERRORLEVEL% NEQ 1 goto ERROR

IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR

EXIT /B 0


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

tar -xf "%TMPDir%\tmp.zip" -C "%TMPDir%" %TarExclude%
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

tar -xf "%TMPDir%\tmp.zip" -C "%TMPDir%" %TarExclude%
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