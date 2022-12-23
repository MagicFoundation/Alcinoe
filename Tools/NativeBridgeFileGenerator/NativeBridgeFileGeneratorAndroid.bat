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


REM ---------------------------
REM Ask the library Identifiers
REM ---------------------------

echo Please enter the library Identifiers in the format GroupID:ArtifactID:Version
set Library=
set /P Library=%=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input


REM ---------------------------
REM Create TMPDir and OutputDir
REM ---------------------------

SET TMPDir=%ALBaseDir%\Tools\NativeBridgeFileGenerator\Tmp
IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR
  
SET OutputDir=%ALBaseDir%\Tools\NativeBridgeFileGenerator\OutputAndroid
IF EXIST "%OutputDir%" rmdir /s /q "%OutputDir%"
IF EXIST "%OutputDir%" goto ERROR
mkdir "%OutputDir%"


REM -----------------------------------
REM call java2op via AndroidMerger Tool
REM -----------------------------------

call "..\AndroidMerger\AndroidMerger.exe" -LocalMavenRepositoryDir="%ALBaseDir%\Libraries\jar\" -Libraries="%Library%" -DownloadDependencies=1 -OutputDir="%TMPDir%" -GenerateNativeBridgeFile=1 -NoInteraction=1
IF ERRORLEVEL 1 goto ERROR

xcopy "%TMPDir%\JavaInterfaces.pas" "%OutputDir%\JavaInterfaces.pas*" /V
IF ERRORLEVEL 1 goto ERROR


REM -------------
REM remove TMPDir
REM -------------

IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR

SET FileName=%ALBaseDir%\Tools\NativeBridgeFileGenerator\jar.log
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