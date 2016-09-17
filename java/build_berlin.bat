@echo off
cls

setlocal

REM -----------------------------------------------------
REM
REM Update the path below according to your system
REM Please notice that we use the SDK of Marshmallow (23)
REM instead of the default lollipop (22) used by Delphi
REM Berlin. This because we want the text selection
REM like https://developer.android.com/about/versions/marshmallow/android-6.0-changes.html#behavior-text-selection
REM Please install the SDK build tools and the SDK Platform 
REM of Marshmallow (23) using C:\Users\Public\Documents\Embarcadero\Studio\18.0\PlatformSDKs\android-sdk-windows\SDK Manager.exe
REM
REM -----------------------------------------------------

if x%ANDROID% == x set ANDROID="C:\Users\Public\Documents\Embarcadero\Studio\18.0\PlatformSDKs\android-sdk-windows"
set ANDROID_PLATFORM=%ANDROID%\platforms\android-23
set DX_LIB=%ANDROID%\build-tools\23.0.3\lib
set DX_PATH=%ANDROID%\build-tools\23.0.3
set JDK_PATH="C:\Program Files\Java\jdk1.7.0_25\bin"
set EMBO_DEX_DEBUG="C:\Program Files (x86)\Embarcadero\Studio\18.0\lib\android\debug\classes.dex"
set EMBO_DEX_RELEASE="C:\Program Files (x86)\Embarcadero\Studio\18.0\lib\android\release\classes.dex"
set PROJ_DIR=%CD%
set VERBOSE=0

del dex\berlin\*.class /s /q



REM ---------------------------
REM
REM build the debug classes.dex
REM
REM ---------------------------

rmdir output /s /q

echo.
echo Compiling the Java Sources (debug)
mkdir output 2> nul
mkdir output\classes 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=-verbose
%JDK_PATH%\javac %VERBOSE_FLAG% -Xlint:deprecation -cp %ANDROID_PLATFORM%\android.jar -d output\classes ^
src\com\alcinoe\widget\ALEditText.java ^
src\com\alcinoe\view\inputmethod\ALSoftInputListener.java ^
src\com\alcinoe\text\method\ALKeyPreImeListener.java

echo Creating jar containing the new classes
mkdir output\jar 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=v
%JDK_PATH%\jar c%VERBOSE_FLAG%f output\jar\test_classes.jar -C output\classes com

echo Converting from jar to dex
mkdir output\dex 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=--verbose
call %DX_LIB%\dx.jar --dex %VERBOSE_FLAG% --output=%PROJ_DIR%\output\dex\test_classes.dex --positions=lines %PROJ_DIR%\output\jar\test_classes.jar

echo Merging dex files
java -cp %DX_LIB%\dx.jar com.android.dx.merge.DexMerger %PROJ_DIR%\output\dex\classes.dex %PROJ_DIR%\output\dex\test_classes.dex %EMBO_DEX_DEBUG%

echo move output\dex\classes.dex
copy output\dex\classes.dex dex\berlin\debug\classes.dex



REM -----------------------------
REM
REM build the release classes.dex
REM
REM -----------------------------

rmdir output /s /q

echo.
echo Compiling the Java Sources (release)
mkdir output 2> nul
mkdir output\classes 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=-verbose
%JDK_PATH%\javac %VERBOSE_FLAG% -Xlint:deprecation -cp %ANDROID_PLATFORM%\android.jar -d output\classes ^
src\com\alcinoe\widget\ALEditText.java ^
src\com\alcinoe\view\inputmethod\ALSoftInputListener.java ^
src\com\alcinoe\text\method\ALKeyPreImeListener.java

echo Creating jar containing the new classes
mkdir output\jar 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=v
%JDK_PATH%\jar c%VERBOSE_FLAG%f output\jar\test_classes.jar -C output\classes com

echo Converting from jar to dex
mkdir output\dex 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=--verbose
call %DX_LIB%\dx.jar --dex %VERBOSE_FLAG% --output=%PROJ_DIR%\output\dex\test_classes.dex --positions=lines %PROJ_DIR%\output\jar\test_classes.jar

echo Merging dex files
java -cp %DX_LIB%\dx.jar com.android.dx.merge.DexMerger %PROJ_DIR%\output\dex\classes.dex %PROJ_DIR%\output\dex\test_classes.dex %EMBO_DEX_RELEASE%

echo move output\dex\classes.dex
copy output\dex\classes.dex dex\berlin\release\classes.dex



REM ---------------------------
REM
REM remove the output directory
REM
REM ---------------------------

rmdir output /s /q

:Exit

endlocal
