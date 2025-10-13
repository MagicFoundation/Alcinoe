@echo off
setlocal enabledelayedexpansion

if "%ALBaseDir%"=="" (
  cls  
  Set Standalone=1
  set ALDelphiVersion=37.0
  call "%~dp0\..\..\InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR  
)

for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS\%ALDelphiVersion%" /v "RootDir"') do set EmbSourceDir=%%B
set EmbSourceDir=%EmbSourceDir:~0,-1%\source
if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR

SET FileName=%ALBaseDir%\Embarcadero\Florence\fmx
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
IF EXIST "%FileName%" goto ERROR
mkdir "%FileName%"

SET FileName=%ALBaseDir%\Embarcadero\Florence\rtl
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
IF EXIST "%FileName%" goto ERROR

SET FileName=%ALBaseDir%\Embarcadero\Florence\data
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
IF EXIST "%FileName%" goto ERROR

echo Copy "%EmbSourceDir%\fmx"
xcopy /Q "%EmbSourceDir%\fmx" "%ALBaseDir%\Embarcadero\Florence\fmx"
IF ERRORLEVEL 1 goto ERROR

echo Patch the locally copied source code
git -C "%ALBaseDir%" apply --ignore-space-change --ignore-whitespace --whitespace=nowarn .\Embarcadero\Florence\Florence.patch -v

echo Remove warnings from the copied files
for /f "delims=" %%a IN ('dir /b /s %ALBaseDir%\Embarcadero\Florence\*.pas') do Call :ADD_HINTS_OFF "%%a"

goto FINISHED


:ADD_HINTS_OFF

REM %~1 the pas file

SET TmpFileName=%ALBaseDir%\Embarcadero\Florence\~temp.pas
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