@echo off
SETLOCAL

for /f "tokens=3" %%a in ('java -version 2^>^&1') do (
    set "java_version=%%a"
    goto compare_version
)

:compare_version
echo Detected Java version: %java_version%
if not "%java_version:~1,3%"=="1.8" (
    echo Java2OP requires Java 1.8, please set your %%JAVA_HOME%% to
    echo your Java 1.8 bin directory and the same for your %%PATH%%
    echo.
)

REM ----------------
REM Init Environment
REM ----------------

call "%~dp0\..\..\InitEnvironment.bat"
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
REM Ask the Compare Master File
REM ---------------------------

echo.
echo Please gave below the path to the source file that
echo you would like to compare with (empty to skip):
set CompareMasterFile=
set /P CompareMasterFile=%=%
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

call "..\AndroidMerger\AndroidMerger.exe" -LocalMavenRepositoryDir="%ALBaseDir%\Libraries\jar\" -Libraries="%Library%" -DownloadDependencies=1 -OutputDir="%TMPDir%" -GenerateNativeBridgeFile=1 -NoInteraction=1 -UseGradle=1
IF ERRORLEVEL 1 goto ERROR

xcopy "%TMPDir%\JavaInterfaces*.pas" "%OutputDir%\" /V
IF ERRORLEVEL 1 goto ERROR


REM -----------------------------------------------
REM call NativeBridgeFileGeneratorHelper to compare
REM -----------------------------------------------

if "%CompareMasterFile%" NEQ "" (
  echo Compare "%CompareMasterFile%" to "%OutputDir%"
  call "%ALBaseDir%\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorHelper.exe" -Action="Compare" -MasterFile="%CompareMasterFile%" -OutputDir="%OutputDir%" -Platform="Android"
)


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