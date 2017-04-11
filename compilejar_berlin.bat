@echo off

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
set JDK_PATH="C:\Program Files\Java\jdk1.7.0_25\bin"
set CONFIRM=%1
if x%CONFIRM% == x set CONFIRM=on

SET FileName=lib\jar\alcinoe\*.jar
del %FileName% /s
if exist %FileName% pause

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% pause

echo Compiling the Java Sources
mkdir source\output 2> nul
%JDK_PATH%\javac -Xlint:deprecation -cp %ANDROID_PLATFORM%\android.jar;lib\jar\facebook\facebook-android-sdk.jar;c:\temp\android-support-v4.jar -d source\output ^
source\java\com\alcinoe\widget\ALControlHostLayout.java ^
source\java\com\alcinoe\widget\ALEditText.java ^
source\java\com\alcinoe\view\inputmethod\ALSoftInputListener.java ^
source\java\com\alcinoe\text\method\ALKeyPreImeListener.java ^
source\java\com\alcinoe\facebook\ALFaceBookAppInvite.java
IF ERRORLEVEL 1 pause

echo Creating jar containing the new classes
%JDK_PATH%\jar cf lib\jar\alcinoe\alcinoe.jar -C source\output com
IF ERRORLEVEL 1 pause

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% pause

echo Jar created successfully
if x%CONFIRM% == xon PAUSE 

:Exit

endlocal
