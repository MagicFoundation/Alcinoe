echo off

setlocal EnableDelayedExpansion

Set CurrDir="%CD%"
SET OutputDir="%CurrDir%\_outputjava2op_lib"
IF EXIST %OutputDir% rmdir /s /q %OutputDir%
IF EXIST %OutputDir% goto ERROR
mkdir %OutputDir%

set filenames=
for /f %%G in ('dir .\lib\jar\*.jar /s /b /a-d') do (
  if not "%%~G" == "%CD%\lib\jar\org.webrtc\webrtc.jar" (
    set filenames=!filenames! %%~G
  )
)

call initjava2op.bat
IF ERRORLEVEL 1 goto ERROR
call "C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\converters\java2op\java2op.exe" -jar%filenames% -unit %OutputDir%\JavaInterfaces.pas
IF ERRORLEVEL 1 goto ERROR
xcopy %CurrDir%\jar.log %OutputDir%\jar.log* /V
IF ERRORLEVEL 1 goto ERROR
del %CurrDir%\jar.log
IF ERRORLEVEL 1 goto ERROR

goto EXIT


:ERROR
pause

:EXIT