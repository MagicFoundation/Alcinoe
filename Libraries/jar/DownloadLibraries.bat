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

REM Call :DOWNLOAD_LIBRARY "https://artifactory.wetransform.to/artifactory/libs-snapshot" "org.webrtc" "google-webrtc" "1.0.25331"

goto FINISHED


REM -------------------------
REM Function DOWNLOAD_LIBRARY
REM -------------------------

:DOWNLOAD_LIBRARY

REM %~1 the maven base url (https://repo1.maven.org/maven2)
REM %~2 the groupID (androidx.annotation)
REM %~3 the artifactID (annotation)
REM %~4 the version (1.0.0)

echo [36mDownload %~2:%~3:%~4[0m

Set groupID4URL=%~2
Set groupID4URL=%groupID4URL:.=/%
Set SrcUrl=%~1/%groupID4URL%/%~3/%~4/%~3-%~4
Set groupID4DIR=%~2
Set groupID4DIR=%groupID4DIR:.=\%
Set DestDIR=%ALBaseDir%\Libraries\jar\%groupID4DIR%\%~3\%~4\
IF EXIST "%DestDIR%" rmdir /s /q "%DestDIR%"
IF EXIST "%DestDIR%" goto ERROR
mkdir "%DestDIR%"

curl --fail --location "%SrcUrl%.aar" --output "%DestDIR%\%~3-%~4.aar"
IF ERRORLEVEL 1 (
  curl --fail --location "%SrcUrl%.jar" --output "%DestDIR%\%~3-%~4.jar"
  IF ERRORLEVEL 1 goto ERROR
)
curl --fail --location "%SrcUrl%.pom" --output "%DestDIR%\%~3-%~4.pom"
IF ERRORLEVEL 1 goto ERROR

EXIT /B 0


REM -------------------
REM FINISHED/ERROR/EXIT
REM -------------------

:FINISHED

echo.
echo Android libraries downloaded successfully
if "%Standalone%"=="1" PAUSE 
goto EXIT

:ERROR

PAUSE
EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:EXIT