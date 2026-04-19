@echo off
SETLOCAL

REM ----------------
REM Init Environment
REM ----------------

if "%ALBaseDir%"=="" (  
  
  Set Standalone=1
  call "%~dp0InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR
  
)


REM --------------
REM Security check
REM --------------

if not exist "%ALBaseDir%\Source\Alcinoe.inc" goto ERROR


REM -------------------------
REM Ask with library to build
REM -------------------------

set LibraryToBuild=
if NOT "%Standalone%"=="1" GOTO SKIP_INTERACTION

echo.
echo 1) alcinoe-broadcastreceiver
echo 2) alcinoe-datepicker
echo 3) alcinoe-edittext
echo 4) alcinoe-facebook-share
echo 5) alcinoe-firebase-messaging
echo 6) alcinoe-installreferrer
echo 7) alcinoe-webkit
echo 8) alcinoe-http

set /P LibraryToBuild=Select a library (Empty for all): %=%
more < nul > nul & REM This instruction to clear the ERRORLEVEL because previous instruction set ERRORLEVEL to 1 if empty input
echo.

:SKIP_INTERACTION


REM -----------------
REM Update local vars
REM -----------------

FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%ALDelphiVersion%" /v RootDir`) DO set DelphiRootDirFromReg=%%A %%B
set FMXJAR=%DelphiRootDirFromReg%lib\android\release\fmx.jar
FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%ALDelphiVersion%\PlatformSDKs" /v Default_Android`) DO set Default_Android=%%A
FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%ALDelphiVersion%\PlatformSDKs" /v Default_Android64`) DO set Default_Android64=%%A
set PlatformSDK=%Default_Android64%
if "%PlatformSDK%" == "" set PlatformSDK=%Default_Android%
FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%ALDelphiVersion%\PlatformSDKs\%PlatformSDK%" /v SDKApiLevelPath`) DO set SDKApiLevelPath=%%A
FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\%ALDelphiVersion%\PlatformSDKs\%PlatformSDK%" /v JDKPath`) DO set JDKPath=%%A %%B
set JDKBinPath="%JDKPath%\bin"

echo FMXJAR=%FMXJAR%
echo PlatformSDK=%PlatformSDK%
echo SDKApiLevelPath=%SDKApiLevelPath%
echo JDKBinPath=%JDKBinPath%
echo.


REM ---------------
REM clean Libraries
REM ---------------

if NOT "%LibraryToBuild%"=="" GOTO SKIP_clean_Libraries
SET FileName=%ALBaseDir%\Libraries\jar\io\magicfoundation\alcinoe
IF EXIST "%FileName%" rmdir /s /q "%FileName%"
IF EXIST "%FileName%" goto ERROR
mkdir "%FileName%"
:SKIP_clean_Libraries


REM --------------
REM Create Tmp dir
REM --------------

SET TMPDir=%ALBaseDir%\Tmp
IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR
mkdir "%TMPDir%"
SET TMPLibrariesDir=%TMPDir%\Libraries
SET TMPOutputDir=%TMPDir%\Output
SET TMPDependenciesFile=%TMPDir%\Dependencies.txt


REM ----------------------------------
REM Go to the library we want to build
REM ----------------------------------

if "%LibraryToBuild%"=="" goto ALL_LIBRARIES
if "%LibraryToBuild%"=="1" goto alcinoe_broadcastreceiver
if "%LibraryToBuild%"=="2" goto alcinoe_datepicker
if "%LibraryToBuild%"=="3" goto alcinoe_edittext
if "%LibraryToBuild%"=="4" goto alcinoe_facebook_share
if "%LibraryToBuild%"=="5" goto alcinoe_firebase_messaging
if "%LibraryToBuild%"=="6" goto alcinoe_installreferrer
if "%LibraryToBuild%"=="7" goto alcinoe_webkit
if "%LibraryToBuild%"=="8" goto alcinoe_http
goto FINISHED

:ALL_LIBRARIES


REM -----------------------------------
REM Build alcinoe-broadcastreceiver.jar
REM -----------------------------------

:alcinoe_broadcastreceiver
echo [36mBuild alcinoe-broadcastreceiver[0m
type nul > %TMPDependenciesFile%
SET ClassPath="%SDKApiLevelPath%\android.jar"
SET SourceFiles=%SourceFiles% %ALBaseDir%\Source\Java\io\magicfoundation\alcinoe\broadcastreceiver\*.java
Call :BUILD_JAR "io.magicfoundation.alcinoe" "alcinoe-broadcastreceiver" "1.0.2"
if NOT "%LibraryToBuild%"=="" GOTO FINISHED


REM ----------------------------
REM Build alcinoe-datepicker.jar
REM ----------------------------

:alcinoe_datepicker
echo [36mBuild alcinoe-datepicker[0m
type nul > %TMPDependenciesFile%
SET ClassPath="%SDKApiLevelPath%\android.jar"
SET SourceFiles=%ALBaseDir%\Source\Java\io\magicfoundation\alcinoe\datepicker\*.java
Call :BUILD_JAR "io.magicfoundation.alcinoe" "alcinoe-datepicker" "1.0.0"
if NOT "%LibraryToBuild%"=="" GOTO FINISHED


REM --------------------------
REM Build alcinoe-edittext.jar
REM --------------------------

:alcinoe_edittext
echo [36mBuild alcinoe-edittext[0m
type nul > %TMPDependenciesFile%
SET ClassPath="%SDKApiLevelPath%\android.jar"
SET SourceFiles=%ALBaseDir%\Source\Java\io\magicfoundation\alcinoe\edittext\*.java
Call :BUILD_JAR "io.magicfoundation.alcinoe" "alcinoe-edittext" "1.0.0"
if NOT "%LibraryToBuild%"=="" GOTO FINISHED


REM ------------------------
REM Build alcinoe-webkit.jar
REM ------------------------

:alcinoe_webkit
echo [36mBuild alcinoe-webkit[0m
type nul > %TMPDependenciesFile%
SET ClassPath="%SDKApiLevelPath%\android.jar"
Call :UPDATE_ClASSPATH "https://dl.google.com/android/maven2" "androidx.annotation" "annotation" "1.3.0"
SET SourceFiles=%ALBaseDir%\Source\Java\io\magicfoundation\alcinoe\webkit\*.java
Call :BUILD_JAR "io.magicfoundation.alcinoe" "alcinoe-webkit" "1.0.0"
if NOT "%LibraryToBuild%"=="" GOTO FINISHED


REM ----------------------
REM Build alcinoe-http.jar
REM ----------------------

:alcinoe_http
echo [36mBuild alcinoe-http[0m
type nul > %TMPDependenciesFile%
SET ClassPath="%SDKApiLevelPath%\android.jar"
Call :UPDATE_ClASSPATH "https://repo1.maven.org/maven2" "io.magicfoundation.alcinoe" "alcinoe-broadcastreceiver" "1.0.2"
Call :UPDATE_ClASSPATH "https://dl.google.com/android/maven2" "androidx.annotation" "annotation" "1.3.0"
Call :UPDATE_ClASSPATH "https://dl.google.com/android/maven2" "androidx.work" "work-runtime" "2.11.0"
REM Mandatory dependencies of androidx.work:work-runtime:2.11.0 required to build the JAR
REM Use Tools\AndroidLibScanner\AndroidLibScanner.bat to determine the required versions of those dependencies
Call :UPDATE_ClASSPATH "https://repo1.maven.org/maven2" "org.jetbrains.kotlin" "kotlin-stdlib" "2.1.20"
Call :UPDATE_ClASSPATH "https://repo1.maven.org/maven2" "com.google.guava" "listenablefuture" "1.0"
SET SourceFiles=%ALBaseDir%\Source\Java\io\magicfoundation\alcinoe\http\*.java
Call :BUILD_JAR "io.magicfoundation.alcinoe" "alcinoe-http" "1.0.0"

SET HttpLibDir=%ALBaseDir%\Libraries\jar\io\magicfoundation\alcinoe\alcinoe-http\1.0.0\
SET HttpLibFilename=alcinoe-http-1.0.0
SET AndroidManifestFilename=%HttpLibDir%\AndroidManifest.xml
IF EXIST "%AndroidManifestFilename%" del "%AndroidManifestFilename%" /s > nul
IF EXIST "%AndroidManifestFilename%" goto ERROR

@echo ^<?xml version="1.0" encoding="utf-8"?^>>> %AndroidManifestFilename%
@echo ^<manifest xmlns:android="http://schemas.android.com/apk/res/android">> %AndroidManifestFilename%
@echo           package="io.magicfoundation.alcinoe.http"^>>> %AndroidManifestFilename%
@echo   ^<application^>>> %AndroidManifestFilename%
@echo     ^<receiver android:name="io.magicfoundation.alcinoe.broadcastreceiver.ALBroadcastReceiver">> %AndroidManifestFilename%
@echo               android:exported="false"^>>> %AndroidManifestFilename%
@echo       ^<intent-filter^>>> %AndroidManifestFilename%
@echo         ^<action android:name="io.magicfoundation.alcinoe.http.action.HTTP_COMPLETED" /^>>> %AndroidManifestFilename%
@echo       ^</intent-filter^>>> %AndroidManifestFilename%
@echo     ^</receiver^>>> %AndroidManifestFilename%
@echo   ^</application^>>> %AndroidManifestFilename%
@echo ^</manifest^>>> %AndroidManifestFilename%

IF EXIST "%HttpLibDir%\%HttpLibFilename%.zip" del "%HttpLibDir%\%HttpLibFilename%.zip" /s >nul
IF EXIST "%HttpLibDir%\%HttpLibFilename%.zip" goto ERROR
IF EXIST "%HttpLibDir%\%HttpLibFilename%.aar" del "%HttpLibDir%\%HttpLibFilename%.aar" /s >nul
IF EXIST "%HttpLibDir%\%HttpLibFilename%.aar" goto ERROR
rename "%HttpLibDir%\%HttpLibFilename%.jar" classes.jar
powershell -command "Compress-Archive -Path '%HttpLibDir%\AndroidManifest.xml','%HttpLibDir%\classes.jar' -DestinationPath '%HttpLibDir%\%HttpLibFilename%.zip'"
rename "%HttpLibDir%\%HttpLibFilename%.zip" %HttpLibFilename%.aar
del "%AndroidManifestFilename%" /s > nul
del "%HttpLibDir%\classes.jar" /s > nul

if NOT "%LibraryToBuild%"=="" GOTO FINISHED


REM ------------------------------------
REM Build alcinoe-firebase-messaging.jar
REM ------------------------------------

:alcinoe_firebase_messaging
echo [36mBuild alcinoe-firebase-messaging[0m
type nul > %TMPDependenciesFile%
SET ClassPath="%SDKApiLevelPath%\android.jar"
Call :UPDATE_ClASSPATH "https://dl.google.com/android/maven2" "com.google.firebase" "firebase-messaging" "23.2.0"
REM Mandatory dependencies of com.google.firebase:firebase-messaging:23.2.0 required to build the JAR
REM Use Tools\AndroidLibScanner\AndroidLibScanner.bat to determine the required versions of those dependencies
Call :UPDATE_ClASSPATH "https://dl.google.com/android/maven2" "androidx.lifecycle" "lifecycle-livedata-core" "2.0.0"
Call :UPDATE_ClASSPATH "https://dl.google.com/android/maven2" "com.google.android.gms" "play-services-basement" "18.1.0"
@echo androidx.appcompat:appcompat:1.6.1>> %TMPDependenciesFile%
SET SourceFiles=%ALBaseDir%\Source\Java\io\magicfoundation\alcinoe\firebase\messaging\*.java
Call :BUILD_JAR "io.magicfoundation.alcinoe" "alcinoe-firebase-messaging" "1.0.1"

SET FirebaseMessagingLibDir=%ALBaseDir%\Libraries\jar\io\magicfoundation\alcinoe\alcinoe-firebase-messaging\1.0.1\
SET FirebaseMessagingLibFilename=alcinoe-firebase-messaging-1.0.1
SET AndroidManifestFilename=%FirebaseMessagingLibDir%\AndroidManifest.xml
IF EXIST "%AndroidManifestFilename%" del "%AndroidManifestFilename%" /s > nul
IF EXIST "%AndroidManifestFilename%" goto ERROR

@echo ^<?xml version="1.0" encoding="utf-8"?^>>> %AndroidManifestFilename%
@echo ^<manifest xmlns:android="http://schemas.android.com/apk/res/android">> %AndroidManifestFilename%
@echo           package="io.magicfoundation.alcinoe.alcinoe-firebase-messaging"^>>> %AndroidManifestFilename%
@echo   ^<application^>>> %AndroidManifestFilename%
@echo     ^<service android:name="io.magicfoundation.alcinoe.firebase.messaging.ALFirebaseMessagingService">> %AndroidManifestFilename%
@echo              android:directBootAware="true">> %AndroidManifestFilename%
@echo              android:exported="true"^>>> %AndroidManifestFilename%
@echo       ^<intent-filter^>>> %AndroidManifestFilename%
@echo           ^<action android:name="com.google.firebase.MESSAGING_EVENT"/^>>> %AndroidManifestFilename%
@echo       ^</intent-filter^>>> %AndroidManifestFilename%
@echo     ^</service^>>> %AndroidManifestFilename%
@echo   ^</application^>>> %AndroidManifestFilename%
@echo ^</manifest^>>> %AndroidManifestFilename%

IF EXIST "%FirebaseMessagingLibDir%\%FirebaseMessagingLibFilename%.zip" del "%FirebaseMessagingLibDir%\%FirebaseMessagingLibFilename%.zip" /s >nul
IF EXIST "%FirebaseMessagingLibDir%\%FirebaseMessagingLibFilename%.zip" goto ERROR
IF EXIST "%FirebaseMessagingLibDir%\%FirebaseMessagingLibFilename%.aar" del "%FirebaseMessagingLibDir%\%FirebaseMessagingLibFilename%.aar" /s >nul
IF EXIST "%FirebaseMessagingLibDir%\%FirebaseMessagingLibFilename%.aar" goto ERROR
rename "%FirebaseMessagingLibDir%\%FirebaseMessagingLibFilename%.jar" classes.jar
powershell -command "Compress-Archive -Path '%FirebaseMessagingLibDir%\AndroidManifest.xml','%FirebaseMessagingLibDir%\classes.jar' -DestinationPath '%FirebaseMessagingLibDir%\%FirebaseMessagingLibFilename%.zip'"
rename "%FirebaseMessagingLibDir%\%FirebaseMessagingLibFilename%.zip" %FirebaseMessagingLibFilename%.aar
del "%AndroidManifestFilename%" /s > nul
del "%FirebaseMessagingLibDir%\classes.jar" /s > nul

if NOT "%LibraryToBuild%"=="" GOTO FINISHED


REM --------------------------------
REM Build alcinoe-facebook-share.jar
REM --------------------------------

:alcinoe_facebook_share
echo [36mBuild alcinoe-facebook-share[0m
type nul > %TMPDependenciesFile%
SET ClassPath="%SDKApiLevelPath%\android.jar"
Call :UPDATE_ClASSPATH "https://repo1.maven.org/maven2" "com.facebook.android" "facebook-common" "18.0.3"
REM Mandatory dependencies of com.facebook.android:facebook-common:18.0.3 required to build the JAR
REM Use Tools\AndroidLibScanner\AndroidLibScanner.bat to determine the required versions of those dependencies
Call :UPDATE_ClASSPATH "https://dl.google.com/android/maven2" "androidx.fragment" "fragment" "1.3.0"
Call :UPDATE_ClASSPATH "https://dl.google.com/android/maven2" "androidx.annotation" "annotation" "1.1.0"
SET SourceFiles=%ALBaseDir%\Source\Java\io\magicfoundation\alcinoe\facebook\share\*.java
Call :BUILD_JAR "io.magicfoundation.alcinoe" "alcinoe-facebook-share" "1.0.2"
if NOT "%LibraryToBuild%"=="" GOTO FINISHED


REM ---------------------------------
REM Build alcinoe-installreferrer.jar
REM ---------------------------------

:alcinoe_installreferrer
echo [36mBuild alcinoe-installreferrer[0m
type nul > %TMPDependenciesFile%
SET ClassPath="%SDKApiLevelPath%\android.jar"
Call :UPDATE_ClASSPATH "https://dl.google.com/android/maven2" "com.android.installreferrer" "installreferrer" "2.2"
SET SourceFiles=%ALBaseDir%\Source\Java\io\magicfoundation\alcinoe\installreferrer\*.java
Call :BUILD_JAR "io.magicfoundation.alcinoe" "alcinoe-installreferrer" "1.0.0"
if NOT "%LibraryToBuild%"=="" GOTO FINISHED

 
REM -------------
REM Goto Finished
REM -------------

GOTO FINISHED


REM ------------------
REM Function BUILD_JAR
REM ------------------

:UPDATE_ClASSPATH

REM %~2 the groupId (io.magicfoundation.alcinoe)
REM %~3 the artifactId (alcinoe-facebook)
REM %~4 the version (1.0.0)

Call :DOWNLOAD_LIBRARY "%~1" "%~2" "%~3" "%~4"

Set groupID4DIR=%~2
Set groupID4DIR=%groupID4DIR:.=\%
Set PartialPath=%groupID4DIR%\%~3\%~4\%~3-%~4

if exist "%ALBaseDir%\Libraries\jar\%PartialPath%.aar" (
  Call :UNZIP "%PartialPath%"
  SET ClassPath=%ClassPath%;"%TMPLibrariesDir%\%PartialPath%\classes.jar"
) else (
  SET ClassPath=%ClassPath%;"%ALBaseDir%\Libraries\jar\%PartialPath%.jar"
)
@echo %~2:%~3:%~4>> %TMPDependenciesFile%

EXIT /B 0


REM -------------------------
REM Function DOWNLOAD_LIBRARY
REM -------------------------

:DOWNLOAD_LIBRARY

REM %~1 the maven base url (https://repo1.maven.org/maven2)
REM %~2 the groupID (androidx.annotation)
REM %~3 the artifactID (annotation)
REM %~4 the version (1.0.0)

Set groupID4URL=%~2
Set groupID4URL=%groupID4URL:.=/%
Set SrcUrl=%~1/%groupID4URL%/%~3/%~4/%~3-%~4
Set groupID4DIR=%~2
Set groupID4DIR=%groupID4DIR:.=\%
Set DestDIR=%ALBaseDir%\Libraries\jar\%groupID4DIR%\%~3\%~4\

IF EXIST "%DestDIR%\%~3-%~4.pom" (
  EXIT /B 0
)
IF EXIST "%DestDIR%" rmdir /s /q "%DestDIR%"
IF EXIST "%DestDIR%" goto ERROR
mkdir "%DestDIR%"

echo [33mDownload %~2:%~3:%~4[0m

curl --fail --location "%SrcUrl%.aar" --output "%DestDIR%\%~3-%~4.aar"
IF ERRORLEVEL 1 (
  curl --fail --location "%SrcUrl%.jar" --output "%DestDIR%\%~3-%~4.jar"
  IF ERRORLEVEL 1 goto ERROR
)
curl --fail --location "%SrcUrl%.pom" --output "%DestDIR%\%~3-%~4.pom"
IF ERRORLEVEL 1 goto ERROR

EXIT /B 0


REM --------------
REM Function UNZIP
REM --------------

:UNZIP

REM %~1 the partial path of the library (androidx\core\core\1.2.0\core-1.2.0)

Set ArchiveFilename=%ALBaseDir%\Libraries\jar\%~1.aar
Set OutPutPath=%TMPLibrariesDir%\%~1

IF EXIST "%OutPutPath%\tmp.zip" EXIT /B 0

xcopy "%ArchiveFilename%" "%OutPutPath%\tmp.zip*" /Q > nul
IF ERRORLEVEL 1 goto ERROR

powershell -command "Expand-Archive -Force '%OutPutPath%\tmp.zip' '%OutPutPath%'"
IF ERRORLEVEL 1 goto ERROR

del "%OutPutPath%\tmp.zip"
IF EXIST "%OutPutPath%\tmp.zip" goto ERROR

EXIT /B 0


REM ------------------
REM Function BUILD_JAR
REM ------------------

:BUILD_JAR

REM %~1 the groupId (io.magicfoundation.alcinoe)
REM %~2 the artifactId (alcinoe-facebook)
REM %~3 the version (1.0.0)

SET GroupId4Directory=%~1
SET GroupId4Directory=%GroupId4Directory:.=\%
SET JARDirectory=%ALBaseDir%\Libraries\jar\%GroupId4Directory%\%~2\%~3\

SET JARFilename=%JARDirectory%\%~2-%~3.jar
IF EXIST "%JARFilename%" del "%JARFilename%" /s >nul
IF EXIST "%JARFilename%" goto ERROR

SET POMFilename=%JARDirectory%\%~2-%~3.pom
IF EXIST "%POMFilename%" del "%POMFilename%" /s >nul
IF EXIST "%POMFilename%" goto ERROR

IF EXIST "%TMPOutputDir%" rmdir /s /q "%TMPOutputDir%"
IF EXIST "%TMPOutputDir%" goto ERROR
mkdir "%TMPOutputDir%"
 
%JDKBinPath%\javac^
 -Xlint:unchecked^
 -Xlint:deprecation^
 -cp %ClassPath%^
 -d %TMPOutputDir%^
 %SourceFiles%
IF ERRORLEVEL 1 goto ERROR

IF EXIST "%JARDirectory%" rmdir /s /q "%JARDirectory%"
IF EXIST "%JARDirectory%" goto ERROR
MKDIR "%JARDirectory%"
%JDKBinPath%\jar cf "%JARFilename%" -C "%TMPOutputDir%" %GroupId4Directory%
IF ERRORLEVEL 1 goto ERROR

@echo ^<project xmlns="http://maven.apache.org/POM/4.0.0"> %POMFilename%
@echo          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">> %POMFilename%
@echo          xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"^>>> %POMFilename%
@echo   ^<modelVersion^>4.0.0^</modelVersion^>>> %POMFilename%
@echo   ^<groupId^>%~1^</groupId^>>> %POMFilename%
@echo   ^<artifactId^>%~2^</artifactId^>>> %POMFilename%
@echo   ^<version^>%~3^</version^>>> %POMFilename%
@echo   ^<dependencies^>>> %POMFilename%
for /f "tokens=1-3 delims=:" %%i in (%TMPDependenciesFile%) do (
  @echo     ^<dependency^>>> %POMFilename%
  @echo       ^<groupId^>%%i^</groupId^>>> %POMFilename%
  @echo       ^<artifactId^>%%j^</artifactId^>>> %POMFilename%
  @echo       ^<version^>%%k^</version^>>> %POMFilename%
  @echo     ^</dependency^>>> %POMFilename%
)
@echo   ^</dependencies^>>> %POMFilename%
@echo ^</project^>>> %POMFilename%

EXIT /B 0


REM -------------------
REM FINISHED/ERROR/EXIT
REM -------------------

:FINISHED

IF EXIST "%TMPDir%" rmdir /s /q "%TMPDir%"
IF EXIST "%TMPDir%" goto ERROR
echo.
echo Jars created successfully
if "%Standalone%"=="1" PAUSE 
goto EXIT

:ERROR

PAUSE
EXIT 1 & REM without /B to Close CMD.exe in case this batch is a subroutine and the caller forget to catch the ERRORLEVEL

:EXIT