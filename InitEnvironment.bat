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

setlocal enabledelayedexpansion
set LocalDelphiName=
set LocalDelphiVersion=%ALDelphiVersion%
set LocalDelphiDir=
IF "%LocalDelphiVersion%"=="" (
  for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS\23.0" /v "RootDir"') do set LocalDelphiDir=%%B
  IF EXIST "!LocalDelphiDir!\bin\rsvars.bat" (
    IF "!InitEnvironmentQuietMode!"=="" ( 
      ECHO Found Delphi 12 Athens
    )
    set LocalDelphiName=Athens
    set LocalDelphiVersion=23.0
  ) ELSE (
  for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS\22.0" /v "RootDir"') do set LocalDelphiDir=%%B
  IF EXIST "!LocalDelphiDir!\bin\rsvars.bat" (
    IF "!InitEnvironmentQuietMode!"=="" (
      ECHO Found Delphi 11 Alexandria
    )
    set LocalDelphiName=Alexandria
    set LocalDelphiVersion=22.0
  ) ELSE (
  for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS\21.0" /v "RootDir"') do set LocalDelphiDir=%%B
  IF EXIST "!LocalDelphiDir!\bin\rsvars.bat" (
    IF "!InitEnvironmentQuietMode!"=="" (
      ECHO Found Delphi 10.4 Sydney
    )
    set LocalDelphiName=Sydney
    set LocalDelphiVersion=21.0
  ) ELSE (
  for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS\20.0" /v "RootDir"') do set LocalDelphiDir=%%B
  IF EXIST "!LocalDelphiDir!\bin\rsvars.bat" (
    IF "!InitEnvironmentQuietMode!"=="" (
      ECHO Found Delphi 10.3 Rio
    )
    set LocalDelphiName=Rio
    set LocalDelphiVersion=20.0
  ) ELSE (
  for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS\19.0" /v "RootDir"') do set LocalDelphiDir=%%B
  IF EXIST "!LocalDelphiDir!\bin\rsvars.bat" (
    IF "!InitEnvironmentQuietMode!"=="" (
      ECHO Found Delphi 10.2 Tokyo
    )
    set LocalDelphiName=Tokyo
    set LocalDelphiVersion=19.0
  ) ELSE (
  for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS\18.0" /v "RootDir"') do set LocalDelphiDir=%%B
  IF EXIST "!LocalDelphiDir!\bin\rsvars.bat" (
    IF "!InitEnvironmentQuietMode!"=="" (
      ECHO Found Delphi 10.1 Berlin
    )
    set LocalDelphiName=Berlin
    set LocalDelphiVersion=18.0
  ) ELSE (
  for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS\17.0" /v "RootDir"') do set LocalDelphiDir=%%B
  IF EXIST "!LocalDelphiDir!\bin\rsvars.bat" (
    IF "!InitEnvironmentQuietMode!"=="" (
      ECHO Found Delphi 10 Seattle
    )
    set LocalDelphiName=Seattle
    set LocalDelphiVersion=17.0
  )))))))
) ELSE (
  for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS\%LocalDelphiVersion%" /v "RootDir"') do set LocalDelphiDir=%%B
  IF "%LocalDelphiVersion%"=="23.0" (
    set LocalDelphiName=Athens
  ) ELSE (
  IF "%LocalDelphiVersion%"=="22.0" (
    set LocalDelphiName=Alexandria
  ) ELSE (
  IF "%LocalDelphiVersion%"=="21.0" (
    set LocalDelphiName=Sydney
  ) ELSE (
  IF "%LocalDelphiVersion%"=="20.0" (
    set LocalDelphiName=Rio
  ) ELSE (
  IF "%LocalDelphiVersion%"=="19.0" (
    set LocalDelphiName=Tokyo
  ) ELSE (
  IF "%LocalDelphiVersion%"=="18.0" (
    set LocalDelphiName=Berlin
  ) ELSE (
  IF "%LocalDelphiVersion%"=="17.0" (
    set LocalDelphiName=Seattle
  )))))))
)
endlocal & set "ALDelphiName=%LocalDelphiName%" & set "ALDelphiVersion=%LocalDelphiVersion%" & set "ALDelphiDir=%LocalDelphiDir%"

IF "%ALDelphiName%"=="" set ErrorFlag=1
IF "%ALDelphiVersion%"=="" set ErrorFlag=1
IF "%ALDelphiDir%"=="" set ErrorFlag=1
IF "%ErrorFlag%"=="1" (
  ECHO Could not found Delphi
  goto ERROR
)
  
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