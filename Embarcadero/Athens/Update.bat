@echo off
setlocal enabledelayedexpansion

if "%ALBaseDir%"=="" (
  cls  
  Set Standalone=1
  set ALDelphiVersion=23.0
  call "%~dp0\..\..\InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR  
)

FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%ALDelphiVersion%" /v RootDir`) DO set EmbSourceDir=%%A %%B 
set EmbSourceDir=%EmbSourceDir:~0,-1%source

if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR

SET FileName=%ALBaseDir%\Embarcadero\Athens\fmx
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
IF EXIST "%FileName%" goto ERROR
mkdir "%FileName%"

SET FileName=%ALBaseDir%\Embarcadero\Athens\internet
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
IF EXIST "%FileName%" goto ERROR
mkdir "%FileName%"

SET FileName=%ALBaseDir%\Embarcadero\Athens\Indy10
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
IF EXIST "%FileName%" goto ERROR
mkdir "%FileName%"
mkdir "%FileName%\Core"
mkdir "%FileName%\Protocols"
mkdir "%FileName%\System"

SET FileName=%ALBaseDir%\Embarcadero\Athens\rtl
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
IF EXIST "%FileName%" goto ERROR
mkdir "%FileName%"
mkdir "%FileName%\ios"
mkdir "%FileName%\osx"
mkdir "%FileName%\android"
mkdir "%FileName%\common"
mkdir "%FileName%\net"

echo Copy "%EmbSourceDir%\fmx"
xcopy /Q "%EmbSourceDir%\fmx" "%ALBaseDir%\Embarcadero\Athens\fmx"
IF ERRORLEVEL 1 goto ERROR

echo Copy "%EmbSourceDir%\internet"
xcopy /Q "%EmbSourceDir%\internet" "%ALBaseDir%\Embarcadero\Athens\internet"
IF ERRORLEVEL 1 goto ERROR

echo Copy "%EmbSourceDir%\Indy10\Core"
xcopy /Q "%EmbSourceDir%\Indy10\Core" "%ALBaseDir%\Embarcadero\Athens\Indy10\Core"
IF ERRORLEVEL 1 goto ERROR

echo Copy "%EmbSourceDir%\Indy10\Protocols"
xcopy /Q "%EmbSourceDir%\Indy10\Protocols" "%ALBaseDir%\Embarcadero\Athens\Indy10\Protocols"
IF ERRORLEVEL 1 goto ERROR

echo Copy "%EmbSourceDir%\Indy10\System"
xcopy /Q "%EmbSourceDir%\Indy10\System" "%ALBaseDir%\Embarcadero\Athens\Indy10\System"
IF ERRORLEVEL 1 goto ERROR

IF EXIST "%EmbSourceDir%\rtl\ios" (
  echo Copy "%EmbSourceDir%\rtl\ios"
  xcopy /Q "%EmbSourceDir%\rtl\ios" "%ALBaseDir%\Embarcadero\Athens\rtl\ios"
  IF ERRORLEVEL 1 goto ERROR
)

IF EXIST "%EmbSourceDir%\rtl\osx" (
  echo Copy "%EmbSourceDir%\rtl\osx"
  xcopy /Q "%EmbSourceDir%\rtl\osx" "%ALBaseDir%\Embarcadero\Athens\rtl\osx"
  IF ERRORLEVEL 1 goto ERROR
)

IF EXIST "%EmbSourceDir%\rtl\android" (
  echo Copy "%EmbSourceDir%\rtl\android"
  xcopy /Q "%EmbSourceDir%\rtl\android" "%ALBaseDir%\Embarcadero\Athens\rtl\android"
  IF ERRORLEVEL 1 goto ERROR
)

IF EXIST "%EmbSourceDir%\rtl\common" (
  echo Copy "%EmbSourceDir%\rtl\common"
  xcopy /Q "%EmbSourceDir%\rtl\common" "%ALBaseDir%\Embarcadero\Athens\rtl\common"
  IF ERRORLEVEL 1 goto ERROR
)

echo Copy "%EmbSourceDir%\rtl\net"
xcopy /Q "%EmbSourceDir%\rtl\net" "%ALBaseDir%\Embarcadero\Athens\rtl\net"
IF ERRORLEVEL 1 goto ERROR

echo Patch the locally copied source code
git -C "%ALBaseDir%" apply --ignore-space-change --ignore-whitespace --whitespace=nowarn .\Embarcadero\Athens\Athens.patch -v

echo Remove warnings from the copied files
for /f "delims=" %%a IN ('dir /b /s %ALBaseDir%\Embarcadero\Athens\*.pas') do Call :ADD_HINTS_OFF "%%a"

goto FINISHED


:ADD_HINTS_OFF

REM %~1 the pas file

SET TmpFileName=%ALBaseDir%\Embarcadero\Athens\~temp.pas
IF EXIST "%TmpFileName%" del "%TmpFileName%"
IF EXIST "%TmpFileName%" goto ERROR
certutil -dump "%~1" | findstr /C:"ef bb bf" > nul
if %errorlevel% equ 0 (
  set /p firstLine=<"%~1"
  set first3=!firstLine:~0,3!
  set restOfLine=!firstLine:~3!
  echo ﻿{$HINTS OFF}{$WARNINGS OFF}{!first3!^}!restOfLine!>"%TmpFileName%"
  more +1 "%~1" >> "%TmpFileName%"
) else (
  echo {$HINTS OFF}{$WARNINGS OFF}>"%TmpFileName%"
  type "%~1">>"%TmpFileName%"
)
del "%~1"
move "%TmpFileName%" "%~1" >nul

EXIT /B 0


REM -------------------
REM FINISHED/ERROR/EXIT
REM -------------------

:FINISHED

echo Delphi source code has been copied and patched successfully
if "%Standalone%"=="1" PAUSE 
goto EXIT

:ERROR

PAUSE
EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:EXIT