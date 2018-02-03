@echo off

SETLOCAL
pushd "%~dp0"

if not exist "..\.git" goto CheckJediIncNotFound

:: Check if git if available
call git --version 2>NUL >NUL
if ERRORLEVEL 1 goto CannotInitializeSubModules
:: Initialize git submodules
echo Initializing/Updating git submodules...
pushd .
cd ..
call git submodule update --init
if ERRORLEVEL 1 goto CannotInitializeSubModules
popd
goto CheckJediIncNotFound

:CannotInitializeSubModules
if exist "source\include\jedi\jedi.inc" goto StartInstall
echo.
echo The "jcl\source\include\jedi" git submodule can't be initialized. jedi.inc not found.
echo You can download the required files from https://github.com/project-jedi/jedi
echo.
goto FailedCompile

:CheckJediIncNotFound
if exist "source\include\jedi\jedi.inc" goto StartInstall
echo.
echo Include file "source\include\jedi\jedi.inc" not found.
echo You can download the required files from https://github.com/project-jedi/jedi
echo.
goto FailedCompile

:StartInstall
SET DELPHIVERSION=%1

cd install

echo.
echo ===================================================================
echo Compiling JediIncCheck...
build\dcc32ex.exe %INSTALL_VERBOSE% -q -w -E..\bin -I..\source\include JediIncCheck.dpr
if ERRORLEVEL 1 goto FailedCompile
:: New Delphi versions output "This product doesn't support command line compiling" and then exit with ERRORLEVEL 0
if not exist ..\bin\JediIncCheck.exe goto FailedCompile

..\bin\JediIncCheck.exe
if ERRORLEVEL 1 goto OutdatedJediInc


:: Build installer start helper
if exist ..\bin\JCLCmdStarter.exe goto SkipCmdStarter
build\dcc32ex.exe -Q -B -E..\bin build\JCLCmdStarter.dpr >NUL 2>NUL
::if ERRORLEVEL 1 goto FailedCompile
:SkipCmdStarter

:: compile installer
echo.
echo ===================================================================
echo Compiling JediInstaller...
build\dcc32ex.exe %INSTALL_VERBOSE% --runtime-package-rtl --runtime-package-vcl -q -dJCLINSTALL -E..\bin -I..\source\include -U..\source\common;..\source\windows JediInstaller.dpr
if ERRORLEVEL 1 goto FailedCompile
:: New Delphi versions output "This product doesn't support command line compiling" and then exit with ERRORLEVEL 0
if not exist ..\bin\JediInstaller.exe goto FailedCompile

echo.
echo ===================================================================
echo Launching JCL installer...

::start ..\bin\JediInstaller.exe %*
if not exist ..\bin\JCLCmdStarter.exe goto FailStart
..\bin\JCLCmdStarter.exe ..\bin\JediInstaller.exe %*
if ERRORLEVEL 1 goto FailStart
goto FINI

:FailStart
..\bin\JediInstaller.exe %*
goto FINI

:OutdatedJediInc
echo.
echo The "source\include\jedi\jedi.inc" include file is outdated.
echo You can download the newest version from https://github.com/project-jedi/jedi
echo.

:FailedCompile
echo.
echo.
echo An error occured while compiling the installer. Installation aborted.
echo.
pause

:FINI
cd ..
SET DELPHIVERSION=

popd
ENDLOCAL