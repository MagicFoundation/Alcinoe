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
REM JDK 1.8/1.7 Compatibility Gotcha: http://www.draconianoverlord.com/2014/04/01/jdk-compatibility.html
REM
REM -----------------------------------------------------

if x%ANDROID% == x set ANDROID="C:\Users\Public\Documents\Embarcadero\Studio\18.0\PlatformSDKs\android-sdk-windows"
set ANDROID_PLATFORM=%ANDROID%\platforms\android-25
set FMX_JAR="C:\Program Files (x86)\Embarcadero\Studio\18.0\lib\android\release\fmx.jar"
set JDK_PATH="C:\Program Files\Java\jdk1.8.0_131\bin"
set JDK_PATH1_7="C:\Program Files\Java\jdk1.7.0_80\bin"
set CONFIRM=%1
if x%CONFIRM% == x set CONFIRM=on

SET FileName=lib\jar\alcinoe\*.jar
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

echo Compiling the Java Sources
mkdir source\output 2> nul
%JDK_PATH%\javac -source 1.7 -target 1.7 -bootclasspath %JDK_PATH1_7%\jre\lib\rt.jar -Xlint:deprecation -cp^
 %ANDROID_PLATFORM%\android.jar;%FMX_JAR%;^
lib\jar\shortcutbadger\shortcutbadger.jar;^
lib\jar\facebook\facebook-android-sdk.jar;^
lib\jar\support-v4\support-core-utils.jar;^
lib\jar\support-v4\support-fragment.jar;^
lib\jar\support-v4\support-compat.jar;^
lib\jar\support-annotations\support-annotations.jar;^
lib\jar\google-play-services\play-services-base.jar;^
lib\jar\google-play-services\play-services-basement.jar;^
lib\jar\google-play-services\play-services-appinvite.jar;^
lib\jar\firebase\firebase-iid.jar;^
lib\jar\firebase\firebase-messaging.jar^
 -d source\output^
 source\java\com\alcinoe\widget\*.java^
 source\java\com\alcinoe\view\inputmethod\*.java^
 source\java\com\alcinoe\text\method\*.java^
 source\java\com\alcinoe\facebook\*.java^
 source\java\com\alcinoe\firebase\iid\*.java^
 source\java\com\alcinoe\firebase\messaging\*.java^
 source\java\com\alcinoe\googleplayservices\*.java
IF ERRORLEVEL 1 goto ERROR

echo Creating jar containing the new classes
%JDK_PATH1_7%\jar cf lib\jar\alcinoe\alcinoe.jar -C source\output com
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

echo Jar created successfully
if x%CONFIRM% == xon PAUSE 
goto EXIT

:ERROR
pause

:EXIT

endlocal
