@echo off

REM ----------------------------------------------
REM Update the path below according to your system
REM ----------------------------------------------

set EMBARCADERO_BIN_DIR=c:\program files (x86)\embarcadero\studio\21.0\bin
set IOS_SDK_ROOT="C:\SDKs\iPhoneOS14.4.sdk"
set IOS_SDK_INCLUDE="C:\SDKs\iPhoneOS14.4.sdk\usr\include"
set IOS_SDK_CLANG="C:\SDKs\iPhoneOS14.4.sdk\usr\lib\clang"
set IOS_SDK_FRAMEWORKS="C:\SDKs\iPhoneOS14.4.sdk\System\Library\Frameworks"


REM ---------------
REM clean directory
REM ---------------

Set CurrDir="%CD%"
SET OutputDir="%CurrDir%\_outputsdktransform_ios"
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
-D TARGET_OS_IPHONE ^
-isysroot %IOS_SDK_ROOT% ^
-isystem %IOS_SDK_INCLUDE% ^
-isystem %IOS_SDK_CLANG% ^
-F %IOS_SDK_FRAMEWORKS% ^
-triple thumbv7-apple-ios ^
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