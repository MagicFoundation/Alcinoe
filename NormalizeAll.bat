@echo off
SETLOCAL

REM ----------------
REM Init Environment
REM ----------------

if "%ALBaseDir%"=="" (  
  
  Set Standalone=1
  call "%~dp0InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR
  
)


REM --------------
REM Security check
REM --------------

if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR


REM -------------------
REM Normalize all units
REM -------------------

call "%ALBaseDir%\Tools\UnitNormalizer\UnitNormalizer.exe" -Dir="%ALBaseDir%\Demos\" -CreateBackup="false" -FilesToIgnore="superxmlparser.pas;supertimezone.pas;superobject.pas;superdate.pas;dwsXPlatform.pas;dwsUtils.pas;dwsJSON.pas;dwsStrings.pas"
IF ERRORLEVEL 1 goto ERROR
call "%ALBaseDir%\Tools\UnitNormalizer\UnitNormalizer.exe" -Dir="%ALBaseDir%\Source\" -FilesToIgnore="ZLibExGZ.pas;ZLibExApi.pas;ZLibEx.pas;Grijjy.SymbolTranslator.pas;Grijjy.ErrorReporting.pas;Alcinoe.iOSapi.ImageIO.pas" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR
call "%ALBaseDir%\Tools\UnitNormalizer\UnitNormalizer.exe" -Dir="%ALBaseDir%\Tests\" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR
call "%ALBaseDir%\Tools\UnitNormalizer\UnitNormalizer.exe" -Dir="%ALBaseDir%\Tools\" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR

call "%ALBaseDir%\Tools\DProjNormalizer\DProjNormalizer.exe" -Dir="%ALBaseDir%\Demos\" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR
call "%ALBaseDir%\Tools\DProjNormalizer\DProjNormalizer.exe" -Dir="%ALBaseDir%\Source\" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR
call "%ALBaseDir%\Tools\DProjNormalizer\DProjNormalizer.exe" -Dir="%ALBaseDir%\Tests\" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR
call "%ALBaseDir%\Tools\DProjNormalizer\DProjNormalizer.exe" -Dir="%ALBaseDir%\Tools\" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR

call "%ALBaseDir%\Tools\DeployProjNormalizer\DeployProjNormalizer.exe" -Dir="%ALBaseDir%\Demos\" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR


REM -------------------
REM FINISHED/ERROR/EXIT
REM -------------------

:FINISHED

echo.
echo Units, DProj, and DeployProj have been normalized successfully
if "%Standalone%"=="1" PAUSE 
goto EXIT

:ERROR

PAUSE
EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:EXIT