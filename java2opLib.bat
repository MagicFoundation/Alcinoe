@echo off

REM ----------------------------------------------
REM Update the path below according to your system
REM ----------------------------------------------

setlocal EnableDelayedExpansion
set EMBARCADERO_BIN_DIR=c:\program files (x86)\embarcadero\studio\21.0\bin


REM ---------------
REM clean directory
REM ---------------

Set CurrDir="%CD%"
SET OutputDir="%CurrDir%\_outputjava2op_lib"
IF EXIST %OutputDir% rmdir /s /q %OutputDir%
IF EXIST %OutputDir% goto ERROR
mkdir %OutputDir%


REM ----------------
REM call initjava2op
REM ----------------

set filenames=
for /f %%G in ('dir .\lib\jar\*.jar /s /b /a-d') do (
  if not "%%~G" == "%CD%\lib\jar\org.webrtc\webrtc.jar" (
    if not "%%~G" == "%CD%\lib\jar\org.jetbrains.kotlin\kotlin-stdlib.jar" (
      set filenames=!filenames! %%~G
    )
  )
)

call initjava2op.bat
IF ERRORLEVEL 1 goto ERROR
call "%EMBARCADERO_BIN_DIR%\converters\java2op\java2op.exe" -jar%filenames% -unit %OutputDir%\JavaInterfaces.pas
IF ERRORLEVEL 1 goto ERROR
xcopy %CurrDir%\jar.log %OutputDir%\jar.log* /V
IF ERRORLEVEL 1 goto ERROR
del %CurrDir%\jar.log
IF ERRORLEVEL 1 goto ERROR


REM ----
REM EXIT
REM ----

goto EXIT

:ERROR
pause

:EXIT