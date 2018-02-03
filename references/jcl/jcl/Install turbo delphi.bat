SETLOCAL
pushd "%~dp0"

echo Launching JCL installer...

start .\bin\JediInstaller.exe %2 %3 %4 %5 %6 %7 %8 %9
if ERRORLEVEL 1 goto FailStart
goto FINI

:FailStart
.\bin\JediInstaller.exe %2 %3 %4 %5 %6 %7 %8 %9
goto FINI

:FailedCompile
echo.
echo.
echo An error occured while compiling the installer. Installation aborted.
echo.
pause

:FINI

popd
ENDLOCAL