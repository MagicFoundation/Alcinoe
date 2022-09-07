@echo off

REM ----------------------------------------------
REM Update the path below according to your system
REM ----------------------------------------------

set ANDROID="C:\SDKs\android"
set ANDROID_PLATFORM=%ANDROID%\platforms\android-30
set JDK_PATH="C:\Program Files\AdoptOpenJDK\jdk-8.0.282.8-hotspot\bin"
set DEFAULT_DELPHI_VERSION=21.0


REM -----------------
REM Update local vars
REM -----------------
set CONFIRM=%1
if "x%CONFIRM%" == "x" set CONFIRM=on
set DelphiRootDirFromParams=%~2
FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%DEFAULT_DELPHI_VERSION%" /v RootDir`) DO set DelphiRootDirFromReg=%%A %%B 
set DelphiRootDirFromReg=%DelphiRootDirFromReg:~0,-1%
set FMX_JAR="%DelphiRootDirFromParams%lib\android\release\fmx.jar"
if "x%DelphiRootDirFromParams%" == "x" set FMX_JAR="%DelphiRootDirFromReg%lib\android\release\fmx.jar"


REM ---------------
REM clean directory
REM ---------------

SET FileName=Lib\jar\com.alcinoe
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%


REM ------------------------
REM Build alcinoe-common.jar
REM ------------------------

echo Build alcinoe-common.jar

mkdir Lib\jar\com.alcinoe\com.alcinoe.common

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir Source\output 2> nul
%JDK_PATH%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
 -d Source\output^
 Source\java\com\alcinoe\util\*.java^
 Source\java\com\alcinoe\content\*.java^
 Source\java\com\alcinoe\location\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH%\jar cf Lib\jar\com.alcinoe\com.alcinoe.common\alcinoe-common.jar -C Source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM ----------------------------
REM Build alcinoe-datepicker.jar
REM ----------------------------

echo Build alcinoe-datepicker.jar

mkdir Lib\jar\com.alcinoe\com.alcinoe.datepicker

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir Source\output 2> nul
%JDK_PATH%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
 -d Source\output^
 Source\java\com\alcinoe\datepicker\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH%\jar cf Lib\jar\com.alcinoe\com.alcinoe.datepicker\alcinoe-datepicker.jar -C Source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM --------------------------
REM Build alcinoe-edittext.jar
REM --------------------------

echo Building alcinoe-edittext.jar

mkdir Lib\jar\com.alcinoe\com.alcinoe.edittext

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir Source\output 2> nul
%JDK_PATH%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
 -d Source\output^
 Source\java\com\alcinoe\edittext\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH%\jar cf Lib\jar\com.alcinoe\com.alcinoe.edittext\alcinoe-edittext.jar -C Source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM ------------------------
REM Build alcinoe-webrtc.jar
REM ------------------------

echo Build alcinoe-webrtc.jar

mkdir Lib\jar\com.alcinoe\com.alcinoe.webrtc

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir Source\output 2> nul
%JDK_PATH%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
Lib\jar\com.android.support\support-annotations.jar;^
Lib\jar\org.webrtc\webrtc.jar^
 -d Source\output^
 Source\java\com\alcinoe\webrtc\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH%\jar cf Lib\jar\com.alcinoe\com.alcinoe.webrtc\alcinoe-webrtc.jar -C Source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM ---------------------------
REM Build alcinoe-appsflyer.jar
REM ---------------------------

echo Build alcinoe-appsflyer.jar

mkdir Lib\jar\com.alcinoe\com.alcinoe.appsflyer

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir Source\output 2> nul
%JDK_PATH%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
Lib\jar\com.appsflyer\af-android-sdk.jar^
 -d Source\output^
 Source\java\com\alcinoe\appsflyer\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH%\jar cf Lib\jar\com.alcinoe\com.alcinoe.appsflyer\alcinoe-appsflyer.jar -C Source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM --------------------------
REM Build alcinoe-firebase.jar
REM --------------------------

echo Build alcinoe-firebase.jar

mkdir Lib\jar\com.alcinoe\com.alcinoe.firebase

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir Source\output 2> nul
%JDK_PATH%\javac^
 -Xlint:unchecked^
 -cp %ANDROID_PLATFORM%\android.jar;%FMX_JAR%;^
Lib\jar\com.android.support\support-core-utils.jar;^
Lib\jar\com.android.support\support-compat.jar;^
Lib\jar\com.android.support\support-annotations.jar;^
Lib\jar\com.google.android.gms\play-services-basement.jar;^
Lib\jar\com.google.firebase\firebase-iid.jar;^
Lib\jar\com.google.firebase\firebase-messaging.jar^
 -d Source\output^
 Source\java\com\alcinoe\firebase\iid\*.java^
 Source\java\com\alcinoe\firebase\messaging\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH%\jar cf Lib\jar\com.alcinoe\com.alcinoe.firebase\alcinoe-firebase.jar -C Source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM --------------------------
REM Build alcinoe-facebook.jar
REM --------------------------

echo Build alcinoe-facebook.jar

mkdir Lib\jar\com.alcinoe\com.alcinoe.facebook

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir Source\output 2> nul
%JDK_PATH%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
Lib\jar\com.android.support\support-fragment.jar;^
Lib\jar\com.android.support\support-annotations.jar;^
Lib\jar\com.facebook.android\facebook-common.jar;^
 -d Source\output^
 Source\java\com\alcinoe\facebook\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH%\jar cf Lib\jar\com.alcinoe\com.alcinoe.facebook\alcinoe-facebook.jar -C Source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR


REM ---------------------------------
REM Build alcinoe-installreferrer.jar
REM ---------------------------------

echo Build alcinoe-installreferrer.jar

mkdir Lib\jar\com.alcinoe\com.alcinoe.installreferrer

SET FileName=Source\output
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR

mkdir Source\output 2> nul
%JDK_PATH%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ANDROID_PLATFORM%\android.jar;^
Lib\jar\com.android.installreferrer\installreferrer.jar;^
 -d Source\output^
 Source\java\com\alcinoe\installreferrer\*.java
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output\com\alcinoe\*.class
if exist %FileName% del %FileName%
if exist %FileName% goto ERROR

%JDK_PATH%\jar cf Lib\jar\com.alcinoe\com.alcinoe.installreferrer\alcinoe-installreferrer.jar -C Source\output com\alcinoe\
IF ERRORLEVEL 1 goto ERROR

SET FileName=Source\output
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
