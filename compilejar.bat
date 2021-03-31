@echo off

REM ----------------------------------------------
REM Update the path below according to your system
REM ----------------------------------------------

if x%ANDROID% == x set ANDROID="C:\SDKs\android"
set ANDROID_PLATFORM=%ANDROID%\platforms\android-30
set FMX_JAR="C:\Program Files (x86)\Embarcadero\Studio\21.0\lib\android\release\fmx.jar"
set JDK_PATH1_8="C:\Program Files\AdoptOpenJDK\jdk-8.0.282.8-hotspot\bin"
set CONFIRM=%1
if x%CONFIRM% == x set CONFIRM=on


REM ---------------
REM clean directory
REM ---------------

SET FileName=lib\jar\com.alcinoe
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%


REM ------------------------
REM Build alcinoe-common.jar
REM ------------------------

echo Build alcinoe-common.jar

mkdir lib\jar\com.alcinoe\com.alcinoe.common

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir source\output 2> nul
%JDK_PATH1_8%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
 -d source\output^
 source\java\com\alcinoe\util\*.java^
 source\java\com\alcinoe\content\*.java^
 source\java\com\alcinoe\location\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH1_8%\jar cf lib\jar\com.alcinoe\com.alcinoe.common\alcinoe-common.jar -C source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM ----------------------------
REM Build alcinoe-datepicker.jar
REM ----------------------------

echo Build alcinoe-datepicker.jar

mkdir lib\jar\com.alcinoe\com.alcinoe.datepicker

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir source\output 2> nul
%JDK_PATH1_8%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
 -d source\output^
 source\java\com\alcinoe\datepicker\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH1_8%\jar cf lib\jar\com.alcinoe\com.alcinoe.datepicker\alcinoe-datepicker.jar -C source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM --------------------------
REM Build alcinoe-edittext.jar
REM --------------------------

echo Building alcinoe-edittext.jar

mkdir lib\jar\com.alcinoe\com.alcinoe.edittext

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir source\output 2> nul
%JDK_PATH1_8%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
 -d source\output^
 source\java\com\alcinoe\edittext\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH1_8%\jar cf lib\jar\com.alcinoe\com.alcinoe.edittext\alcinoe-edittext.jar -C source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM ------------------------
REM Build alcinoe-webrtc.jar
REM ------------------------

echo Build alcinoe-webrtc.jar

mkdir lib\jar\com.alcinoe\com.alcinoe.webrtc

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir source\output 2> nul
%JDK_PATH1_8%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
lib\jar\com.android.support\support-annotations.jar;^
lib\jar\org.webrtc\webrtc.jar^
 -d source\output^
 source\java\com\alcinoe\webrtc\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH1_8%\jar cf lib\jar\com.alcinoe\com.alcinoe.webrtc\alcinoe-webrtc.jar -C source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM ---------------------------
REM Build alcinoe-appsflyer.jar
REM ---------------------------

echo Build alcinoe-appsflyer.jar

mkdir lib\jar\com.alcinoe\com.alcinoe.appsflyer

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir source\output 2> nul
%JDK_PATH1_8%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
lib\jar\com.appsflyer\af-android-sdk.jar^
 -d source\output^
 source\java\com\alcinoe\appsflyer\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH1_8%\jar cf lib\jar\com.alcinoe\com.alcinoe.appsflyer\alcinoe-appsflyer.jar -C source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM --------------------------
REM Build alcinoe-firebase.jar
REM --------------------------

echo Build alcinoe-firebase.jar

mkdir lib\jar\com.alcinoe\com.alcinoe.firebase

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir source\output 2> nul
%JDK_PATH1_8%\javac^
 -Xlint:unchecked^
 -cp %ANDROID_PLATFORM%\android.jar;%FMX_JAR%;^
lib\jar\com.android.support\support-core-utils.jar;^
lib\jar\com.android.support\support-compat.jar;^
lib\jar\com.android.support\support-annotations.jar;^
lib\jar\com.google.android.gms\play-services-basement.jar;^
lib\jar\com.google.firebase\firebase-iid.jar;^
lib\jar\com.google.firebase\firebase-messaging.jar^
 -d source\output^
 source\java\com\alcinoe\firebase\iid\*.java^
 source\java\com\alcinoe\firebase\messaging\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH1_8%\jar cf lib\jar\com.alcinoe\com.alcinoe.firebase\alcinoe-firebase.jar -C source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM --------------------------
REM Build alcinoe-facebook.jar
REM --------------------------

echo Build alcinoe-facebook.jar

mkdir lib\jar\com.alcinoe\com.alcinoe.facebook

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir source\output 2> nul
%JDK_PATH1_8%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
lib\jar\com.android.support\support-fragment.jar;^
lib\jar\com.android.support\support-annotations.jar;^
lib\jar\com.facebook.android\facebook-common.jar;^
 -d source\output^
 source\java\com\alcinoe\facebook\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH1_8%\jar cf lib\jar\com.alcinoe\com.alcinoe.facebook\alcinoe-facebook.jar -C source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM ---------------------------------
REM Build alcinoe-installreferrer.jar
REM ---------------------------------

echo Build alcinoe-installreferrer.jar

mkdir lib\jar\com.alcinoe\com.alcinoe.installreferrer

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir source\output 2> nul
%JDK_PATH1_8%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
lib\jar\com.android.installreferrer\installreferrer.jar;^
 -d source\output^
 source\java\com\alcinoe\installreferrer\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH1_8%\jar cf lib\jar\com.alcinoe\com.alcinoe.installreferrer\alcinoe-installreferrer.jar -C source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM ----
REM EXIT
REM ----

echo Jar(s) created successfully
if x%CONFIRM% == xon PAUSE 
goto EXIT

:ERROR
PAUSE
EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:EXIT

endlocal
