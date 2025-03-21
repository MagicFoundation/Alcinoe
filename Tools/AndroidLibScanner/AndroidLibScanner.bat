@echo off

set ALBaseDir=%~dp0

echo Please enter the library or libraries you want to use, separated by a semicolon (;).
echo Example: androidx.activity:activity:1.8.1
set /p Libraries="Enter Libraries: "

"%ALBaseDir%\..\AndroidMerger\AndroidMerger.exe"^
 -LocalMavenRepositoryDir="%ALBaseDir%\..\Libraries\jar\"^
 -Libraries="%Libraries%"^
 -OutputDir="%ALBaseDir%\Output"^
 -NoInteraction=true
IF ERRORLEVEL 1 goto ERROR
 
echo.
echo Finished
pause
goto EXIT


:ERROR
EXIT

:EXIT