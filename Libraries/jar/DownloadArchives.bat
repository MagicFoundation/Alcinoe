@echo off
SETLOCAL

REM --------
REM Starting
REM --------

Call :DOWNLOAD_ANDROIDX_LIBRARIES "1.0.0"

goto FINISHED

REM ------------------------------------
REM Function DOWNLOAD_ANDROIDX_LIBRARIES
REM ------------------------------------

:DOWNLOAD_ANDROIDX_LIBRARIES

REM %~1 the version of the androidX (1.0.0)

Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.annotation" "annotation" "%~1" "jar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.asynclayoutinflater" "asynclayoutinflater" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.collection" "collection" "%~1" "jar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.coordinatorlayout" "coordinatorlayout" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.cursoradapter" "cursoradapter" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.customview" "customview" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.documentfile" "documentfile" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.drawerlayout" "drawerlayout" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.interpolator" "interpolator" "%~1" "aar"
if not "%~1" == "1.0.0" Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.lifecycle" "runtime" "%~1" "aar"
if not "%~1" == "1.0.0" Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.lifecycle" "viewmodel" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.loader" "loader" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.localbroadcastmanager" "localbroadcastmanager" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.print" "print" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.slidingpanelayout" "slidingpanelayout" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.swiperefreshlayout" "swiperefreshlayout" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.versionedparcelable" "versionedparcelable" "%~1" "aar"
Call :DOWNLOAD_ANDROIDX_LIBRARY "androidx.viewpager" "viewpager" "%~1" "aar"

EXIT /B 0


REM ----------------------------------
REM Function DOWNLOAD_ANDROIDX_LIBRARY
REM ----------------------------------

:DOWNLOAD_ANDROIDX_LIBRARY

REM %~1 the groupID (androidx.annotation)
REM %~2 the ArtifactID (annotation)
REM %~3 the version (1.0.0)
REM %~4 archive type (aar or jar)

SET BaseURL=https://maven.google.com
Set groupID4URL=%~1
Set groupID4URL=%groupID4URL:.=/%

Call :DOWNLOAD_LIBRARY "%BaseURL%/%groupID4URL%/%~2/%~3/%~2-%~3.%~4" "%~1\%~2-%~3.%~4"
Call :DOWNLOAD_LIBRARY "%BaseURL%/%groupID4URL%/%~2/%~3/%~2-%~3.pom" "%~1\%~2-%~3.pom"

EXIT /B 0


REM -------------------------
REM Function DOWNLOAD_LIBRARY
REM -------------------------

:DOWNLOAD_LIBRARY

REM %~1 the source url (https://maven.google.com/androidx/annotation/annotation/1.1.0/annotation-1.1.0.jar)
REM %~2 the dest filename (androidx.annotation\annotation-1.1.0.jar)

echo [36mDownload %~1[0m

del "%~2"
if exist "%~2" goto ERROR
curl --fail --location "%~1" --output "%~2"
IF ERRORLEVEL 1 goto ERROR

EXIT /B 0


REM -------------------
REM FINISHED/ERROR/EXIT
REM -------------------

:FINISHED

echo.
echo Archives downloaded successfully
PAUSE 
goto EXIT

:ERROR

PAUSE
EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:EXIT