@echo off
SETLOCAL

REM ----------------
REM Init Environment
REM ----------------

if "%ALBaseDir%"=="" (
  
  cls
  echo ----------------
  echo Init Environment
  echo ----------------
  echo.
  
  Set Standalone=1
  call "..\InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR
  echo.
  
)


REM --------------
REM Security check
REM --------------

if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR


REM ---------------------
REM Build ALTests Project
REM ---------------------

:BUILD_TESTS

echo ---------------------
echo Build ALTests Project
echo ---------------------
echo.

if NOT "%Standalone%"=="1" GOTO DO_BUILD_TESTS

set BuildTests=
set /P BuildTests=Build ALTests (Y/N, default=Y)?: %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
echo.

if "%BuildTests%"=="" set BuildTests=Y
if "%BuildTests%"=="y" set BuildTests=Y
if "%BuildTests%"=="n" set BuildTests=N
if "%BuildTests%"=="Y" goto DO_BUILD_TESTS
if "%BuildTests%"=="N" goto RUN_TESTS
goto BUILD_TESTS

:DO_BUILD_TESTS

SET FileName=%ALBaseDir%\Tests\*.identcache
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ALBaseDir%\Tests\*.dproj.local
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ALBaseDir%\Tests\*.groupproj.local
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ALBaseDir%\Tests\*.deployproj.local
del "%FileName%" /s
if exist "%FileName%" goto ERROR

SET FileName=%ALBaseDir%\Tests\_Source\Dcu
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"

SET FileName=%ALBaseDir%\Tests\Win32
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"

SET FileName=%ALBaseDir%\Tests\Win64
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
if exist "%FileName%" goto ERROR
mkdir "%FileName%"

call "%ALBaseDir%\Tools\UnitNormalizer\UnitNormalizer.exe" -Dir="%ALBaseDir%\Tests\_Source" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR

call "%ALBaseDir%\Tools\DProjNormalizer\DProjNormalizer.exe" -DProj="%ALBaseDir%\Tests\_Source\ALTests.dproj" -CreateBackup="false"
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Debug /p:Platform=Win32[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Debug;DCC_MapFile=3 /p:Platform=Win32 /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Debug /p:Platform=Win64[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Debug;DCC_MapFile=3 /p:Platform=Win64 /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Release /p:Platform=Android[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Release /p:Platform=Android /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Release /p:Platform=Android64[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Release /p:Platform=Android64 /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Release /p:Platform=iOSDevice64[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Release /p:Platform=iOSDevice64 /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Release /p:Platform=iOSSimARM64[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Release /p:Platform=iOSSimARM64 /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Release /p:Platform=OSX64[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Release /p:Platform=OSX64 /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Release /p:Platform=OSXARM64[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Release /p:Platform=OSXARM64 /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Release /p:Platform=Win32[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Release /p:Platform=Win32 /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR

echo.
echo [36mMSBuild ALTests.dproj /p:config=Release /p:Platform=Win64[0m
msbuild "%ALBaseDir%\Tests\_Source\ALTests.dproj" /p:config=Release /p:Platform=Win64 /t:build /verbosity:minimal
IF ERRORLEVEL 1 goto ERROR
echo.


REM ---------
REM Run Tests
REM ---------

:RUN_TESTS

if "%ALRunTests%"=="" set ALRunTests=Y
if NOT "%ALRunTests%"=="Y" GOTO FINISHED

echo ---------
echo Run tests
echo ---------

if NOT "%Standalone%"=="1" GOTO RUN_WIN32_WIN64_TESTS_WITHOUT_CODE_COVERAGE

echo.

:ASK_CODE_COVERAGE

set RunTestsWithCodeCoverage=
set /P RunTestsWithCodeCoverage=Run tests with code coverage (Y/N, default=N)?: %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input

if "%RunTestsWithCodeCoverage%"=="" set RunTestsWithCodeCoverage=N
if "%RunTestsWithCodeCoverage%"=="y" set RunTestsWithCodeCoverage=Y
if "%RunTestsWithCodeCoverage%"=="n" set RunTestsWithCodeCoverage=N
if "%RunTestsWithCodeCoverage%"=="Y" goto RUN_TESTS_WITH_CODE_COVERAGE
if "%RunTestsWithCodeCoverage%"=="N" goto RUN_TESTS_WITHOUT_CODE_COVERAGE
goto ASK_CODE_COVERAGE


REM ----------------------------
REM Run tests with code coverage
REM ----------------------------

:RUN_TESTS_WITH_CODE_COVERAGE

echo You can download DelphiCodeCoverage from https://github.com/DelphiCodeCoverage/DelphiCodeCoverage
echo and build it using {DelphiCodeCoverage}\Build_x64.bat 
set DelphiCodeCoverageDir=
set /P DelphiCodeCoverageDir=Enter the path where DelphiCodeCoverage is located (Empty for c:\Dev\DelphiCodeCoverage): %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
if "%DelphiCodeCoverageDir%"=="" set DelphiCodeCoverageDir=c:\Dev\DelphiCodeCoverage


REM ----------------------------------
REM Run Win32 tests With code coverage
REM ----------------------------------

echo.
echo Run Win32 tests
call "%DelphiCodeCoverageDir%"\build\Win32\CodeCoverage.exe ^
-e %ALBaseDir%\Tests\Win32\Debug\ALTests.exe ^
-m %ALBaseDir%\Tests\Win32\Debug\ALTests.map ^
-a ^^-^^-hidebanner ^^-^^-exitbehavior:Continue ^^-^^-consolemode:Quiet ^
-sd %ALBaseDir%\Tests\_Source\ ^
-u ALString ^
-ife ^
-html ^
-od %ALBaseDir%\Tests\Win32\Debug\ ^
-dproj %ALBaseDir%\Tests\_Source\ALTests.dproj
IF ERRORLEVEL 1 goto ERROR


REM ----------------------------------
REM Run Win64 tests With code coverage
REM ----------------------------------

echo.
echo Run Win64 tests
call "%DelphiCodeCoverageDir%"\build\Win64\CodeCoverage.exe ^
-e %ALBaseDir%\Tests\Win64\Debug\ALTests.exe ^
-m %ALBaseDir%\Tests\Win64\Debug\ALTests.map ^
-a ^^-^^-hidebanner ^^-^^-exitbehavior:Continue ^^-^^-consolemode:Quiet ^
-sd %ALBaseDir%\Tests\_Source\ ^
-u ALString ^
-ife ^
-html ^
-od %ALBaseDir%\Tests\Win64\Debug\ ^
-dproj %ALBaseDir%\Tests\_Source\ALTests.dproj
IF ERRORLEVEL 1 goto ERROR

goto FINISHED 


REM -------------------------------
REM Run tests without code coverage
REM -------------------------------

:RUN_TESTS_WITHOUT_CODE_COVERAGE

echo.
echo 1) Run Win32 tests
echo 2) Run Win64 tests
echo 3) Both Win32 and Win64 tests

:ASK_PLATFORM

set Platform=
set /P Platform=Enter number to select a platform (Empty to Win64): %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input

if "%Platform%"=="" goto RUN_WIN64_TESTS_WITHOUT_CODE_COVERAGE
if "%Platform%"=="1" goto RUN_WIN32_TESTS_WITHOUT_CODE_COVERAGE
if "%Platform%"=="2" goto RUN_WIN64_TESTS_WITHOUT_CODE_COVERAGE
if "%Platform%"=="3" goto RUN_WIN32_WIN64_TESTS_WITHOUT_CODE_COVERAGE
goto ASK_PLATFORM


REM -------------------------------------
REM Run Win32 tests without code coverage
REM -------------------------------------

:RUN_WIN32_TESTS_WITHOUT_CODE_COVERAGE

echo.
echo Run Win32 tests
call %ALBaseDir%\Tests\Win32\Release\ALTests.exe --exitbehavior:Continue --hidebanner
IF ERRORLEVEL 1 goto ERROR

goto FINISHED 


REM -------------------------------------
REM Run Win64 tests without code coverage
REM -------------------------------------

:RUN_WIN64_TESTS_WITHOUT_CODE_COVERAGE

echo.
echo Run Win64 tests
call %ALBaseDir%\Tests\Win64\Release\ALTests.exe --exitbehavior:Continue --hidebanner
IF ERRORLEVEL 1 goto ERROR

goto FINISHED 


REM -----------------------------------------------
REM Run Win32 and Win64 tests without code coverage
REM -----------------------------------------------

:RUN_WIN32_WIN64_TESTS_WITHOUT_CODE_COVERAGE

echo.
echo Run Win32 tests
call %ALBaseDir%\Tests\Win32\Release\ALTests.exe --exitbehavior:Continue --consolemode:Quiet --hidebanner
IF ERRORLEVEL 1 goto ERROR

echo.
echo Run Win64 tests
call %ALBaseDir%\Tests\Win64\Release\ALTests.exe --exitbehavior:Continue --consolemode:Quiet --hidebanner
IF ERRORLEVEL 1 goto ERROR

goto FINISHED 




:ERROR

if "%Standalone%"=="1" pause
EXIT /B 1

:FINISHED

if "%Standalone%"=="1" (
 echo.
 echo Finished
 PAUSE
)