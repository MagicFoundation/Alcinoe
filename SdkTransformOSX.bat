@echo off

REM ----------------------------------------------
REM Update the path below according to your system
REM ----------------------------------------------

set EMBARCADERO_BIN_DIR=c:\program files (x86)\embarcadero\studio\21.0\bin
set MACOS_SDK_ROOT="C:\SDKs\MacOSX11.1.sdk"
set MACOS_SDK_INCLUDE="C:\SDKs\MacOSX11.1.sdk\usr\include"
set MACOS_SDK_CLANG="C:\SDKs\MacOSX11.1.sdk\usr\lib\clang"
set MACOS_SDK_FRAMEWORKS="C:\SDKs\MacOSX11.1.sdk\System\Library\Frameworks"


REM ---------------
REM clean directory
REM ---------------

Set CurrDir="%CD%"
SET OutputDir="%CurrDir%\_outputsdktransform_osx"
IF EXIST %OutputDir% rmdir /s /q %OutputDir%
IF EXIST %OutputDir% goto ERROR
mkdir %OutputDir%


REM -----------------
REM call SdkTransform
REM -----------------

cd "%EMBARCADERO_BIN_DIR%"

"SdkTransform.exe" ^
-cc1 ^
-g ^
-w ^
--macsdk ^
-D TARGET_OS_MAC ^
-isysroot %MACOS_SDK_ROOT% ^
-isystem %MACOS_SDK_INCLUDE% ^
-isystem %MACOS_SDK_CLANG% ^
-F %MACOS_SDK_FRAMEWORKS% ^
-triple x86_64-apple-macosx-clang++ ^
-fdiagnostics-show-option ^
-fexceptions ^
-fobjc-exceptions ^
-x objective-c ^
-std=gnu99 ^
-nobuiltininc ^
-nostdinc++ ^
-nostdsysteminc ^
-fblocks ^
--out:%OutputDir%
IF ERRORLEVEL 1 goto ERROR

cd "%CurrDir%"


REM ----
REM EXIT
REM ----

SET Filename=tempht.m
IF EXIST %Filename% del %Filename%

goto EXIT

:ERROR
pause

:EXIT