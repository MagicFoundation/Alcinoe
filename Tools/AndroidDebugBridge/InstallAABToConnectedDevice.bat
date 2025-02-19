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

REM -------------------
REM CHOOSE_AAB_FILENAME
REM -------------------

:CHOOSE_AAB_FILENAME

if NOT "%AAB_FILENAME%"=="" GOTO DEPLOY

set AAB_FILENAME=
set /P AAB_FILENAME=Enter aab filename: %=%

IF not exist "%AAB_FILENAME%" (
  set AAB_FILENAME=
  goto CHOOSE_AAB_FILENAME
)

set KEYSTORE_FILENAME=
set /P KEYSTORE_FILENAME=Enter keystore filename: %=%

set KEYSTORE_ALIAS=
set /P KEYSTORE_ALIAS=Enter keystore alias: %=%

set KEYSTORE_PWD=
set /P KEYSTORE_PWD=Enter keystore password: %=%

REM ------
REM DEPLOY
REM ------

:DEPLOY

SET TmpApks=%~dp0\~tmp.apks
IF EXIST "%TmpApks%" del "%TmpApks%"
IF EXIST "%TmpApks%" goto ERROR

echo create app.apks
java -jar %~dp0\bundletool.jar build-apks --connected-device --bundle=%AAB_FILENAME% --output=%TmpApks% --adb=%ADBPath% --ks=%KEYSTORE_FILENAME% --ks-key-alias=%KEYSTORE_ALIAS% --ks-pass=pass:%KEYSTORE_PWD%
IF ERRORLEVEL 1 goto ERROR

echo Install app.apks to connected device
java -jar %~dp0\bundletool.jar install-apks --apks=%TmpApks% --adb=%ADBPath%
IF ERRORLEVEL 1 goto ERROR

del "%TmpApks%"
IF ERRORLEVEL 1 goto ERROR

@echo Finished
GOTO END

:ERROR
REM EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:END
PAUSE