@echo off

REM ------------
REM Init ALBaseDir 
REM ------------

set ALBaseDir=%~dp0
IF "%ALBaseDir:~-1%"=="\" SET ALBaseDir=%ALBaseDir:~0,-1%
if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR


REM ---------------
REM Call rsvars.bat 
REM ---------------

set ALDelphiName=
IF "%ALDelphiVersion%"=="" (
  IF EXIST "%PROGRAMFILES(X86)%\Embarcadero\Studio\23.0\bin\rsvars.bat" (
    ECHO Found Delphi 12 Athens
    set ALDelphiName=Athens
    set ALDelphiVersion=23.0
  ) ELSE (
  IF EXIST "%PROGRAMFILES(X86)%\Embarcadero\Studio\22.0\bin\rsvars.bat" (
    ECHO Found Delphi 11 Alexandria
    set ALDelphiName=Alexandria
    set ALDelphiVersion=22.0
  ) ELSE (
  IF EXIST "%PROGRAMFILES(X86)%\Embarcadero\Studio\21.0\bin\rsvars.bat" (
    ECHO Found Delphi 10.4 Sydney
    set ALDelphiName=Sydney
    set ALDelphiVersion=21.0
  ) ELSE (
  IF EXIST "%PROGRAMFILES(X86)%\Embarcadero\Studio\20.0\bin\rsvars.bat" (
    ECHO Found Delphi 10.3 Rio
    set ALDelphiName=Rio
    set ALDelphiVersion=20.0
  ) ELSE (
  IF EXIST "%PROGRAMFILES(X86)%\Embarcadero\Studio\19.0\bin\rsvars.bat" (
    ECHO Found Delphi 10.2 Tokyo
    set ALDelphiName=Tokyo
    set ALDelphiVersion=19.0
  ) ELSE (
  IF EXIST "%PROGRAMFILES(X86)%\Embarcadero\Studio\18.0\bin\rsvars.bat" (
    ECHO Found Delphi 10.1 Berlin
    set ALDelphiName=Berlin
    set ALDelphiVersion=18.0
  ) ELSE (
  IF EXIST "%PROGRAMFILES(X86)%\Embarcadero\Studio\17.0\bin\rsvars.bat" (
    ECHO Found Delphi 10 Seattle
    set ALDelphiName=Seattle
    set ALDelphiVersion=17.0
  )))))))
) ELSE (
  IF "%ALDelphiVersion%"=="23.0" (
    set ALDelphiName=Athens
  ) ELSE (
  IF "%ALDelphiVersion%"=="22.0" (
    set ALDelphiName=Alexandria
  ) ELSE (
  IF "%ALDelphiVersion%"=="21.0" (
    set ALDelphiName=Sydney
  ) ELSE (
  IF "%ALDelphiVersion%"=="20.0" (
    set ALDelphiName=Rio
  ) ELSE (
  IF "%ALDelphiVersion%"=="19.0" (
    set ALDelphiName=Tokyo
  ) ELSE (
  IF "%ALDelphiVersion%"=="18.0" (
    set ALDelphiName=Berlin
  ) ELSE (
  IF "%ALDelphiVersion%"=="17.0" (
    set ALDelphiName=Seattle
  )))))))
)

IF "%ALDelphiName%"=="" (
  ECHO Could not found Delphi
  goto ERROR
)
  
set ALDelphiDir=%PROGRAMFILES(X86)%\Embarcadero\Studio\%ALDelphiVersion%\
CALL "%ALDelphiDir%\bin\rsvars.bat"
IF ERRORLEVEL 1 goto ERROR

goto FINISHED 


REM -------------------
REM FINISHED/ERROR/EXIT
REM -------------------

:ERROR
EXIT /B 1

:FINISHED
goto EXIT

:EXIT