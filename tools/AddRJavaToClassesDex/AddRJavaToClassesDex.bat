@echo OFF
setlocal enabledelayedexpansion

REM ----------------------------------------------
REM Add this Post Build event (android platform) :
REM C:\Dev\Alcinoe\tools\AddRJavaToClassesDex\AddRJavaToClassesDex.bat "$(PROJECTDIR)" "$(OUTPUTPATH)" "$(SDKAaptPath)" "$(SDKApiLevelPath)" "$(JavaDxPath)" "$(JDKPath)"
REM 
REM Example of the generated content of each params : 
REM $(PROJECTDIR) = C:\MyProject\_source
REM $(OutputPath) = C:\MyProject\Android\Release\MyProject.so 
REM $(SDKAaptPath) = C:\Users\Public\Documents\Embarcadero\Studio\18.0\PlatformSDKs\android-sdk-windows\build-tools\23.0.3\Aapt.exe
REM $(SDKApiLevelPath) = C:\Users\Public\Documents\Embarcadero\Studio\18.0\PlatformSDKs\android-sdk-windows\platforms\android-23\android.jar
REM $(JavaDxPath) = C:\Users\Public\Documents\Embarcadero\Studio\18.0\PlatformSDKs\android-sdk-windows\build-tools\23.0.3\dx.bat
REM $(JDKPath) = C:\Program Files\Java\jdk1.7.0_25
REM 
REM Your project must follow exactly this structure:
REM $(PROJECTDIR)\android
REM $(PROJECTDIR)\android\libraries
REM $(PROJECTDIR)\android\libraries\(library1_packagename_ex:com.google.android.gms)\
REM $(PROJECTDIR)\android\libraries\(library1_packagename_ex:com.google.android.gms)\AndroidManifest.xml
REM $(PROJECTDIR)\android\libraries\(library1_packagename_ex:com.google.android.gms)\res
REM $(PROJECTDIR)\android\libraries\(library2_packagename_ex:com.facebook)\
REM $(PROJECTDIR)\android\libraries\(library2_packagename_ex:com.facebook)\AndroidManifest.xml
REM $(PROJECTDIR)\android\libraries\(library2_packagename_ex:com.facebook)\res
REM ....
REM $(PROJECTDIR)\android\res
REM 
REM ----------------------------------------------


set batchDir=%~dp0
set batchDir=%batchDir:~0,-1%
set logFile=%batchDir%\AddRJavaToClassesDex.log

@echo CommandLine=%0 %* > %logFile%

set ProjectDir=%1
@echo ProjectDir=%ProjectDir% >> %logFile% 2>&1

set SDKAaptPath=%3
@echo SDKAaptPath=%SDKAaptPath% >> %logFile% 2>&1

set SDKApiLevelPath=%4
@echo SDKApiLevelPath=%SDKApiLevelPath% >> %logFile% 2>&1

set JavaDxPath=%5
@echo JavaDxPath=%JavaDxPath% >> %logFile% 2>&1

set FOLDER=%JavaDxPath%
for /D %%D in (%FOLDER%) do (
    set PARENT=%%~dpD
)
set JavaDxJarPath=%PARENT%lib\dx.jar
@echo JavaDxJarPath=%JavaDxJarPath% >> %logFile% 2>&1

set JDKPath=%6\bin
@echo JDKPath=%JDKPath% >> %logFile% 2>&1

set FOLDER=%2
for /D %%D in (%FOLDER%) do (
    set PARENT=%%~dpD
)
set OutputPath=%PARENT%classes.dex
@echo OutputPath=%OutputPath% >> %logFile% 2>&1

set APKPath=%~nx2
set APKPath=%APKPath:~0,-3%
set APKPath=%PARENT%%APKPath%\bin\%APKPath%.apk
@echo APKPath=%APKPath% >> %logFile% 2>&1

set TMPDir=%batchDir%\tmp
set outputTextSymbols=%TMPDir%\



REM --------------------------------------
REM Delete existing R.java related classes
REM --------------------------------------

IF EXIST %TMPDir% rmdir /s /q %TMPDir%
IF EXIST %TMPDir% goto ERROR
mkdir %TMPDir% >> %logFile% 2>&1
mkdir %TMPDir%\src >> %logFile% 2>&1
mkdir %TMPDir%\obj >> %logFile% 2>&1



REM ---------------------
REM Loop on all libraries
REM ---------------------

CHDIR %ProjectDir%\android\libraries\
for /d %%D in (*) do (	
  
  IF EXIST %%~fD\AndroidManifest.xml (

    REM ------------------------------------------
    @echo. >> %logFile% 2>&1
    @echo Create R.java - %%~nxD >> %logFile% 2>&1
    REM ------------------------------------------

    @echo %SDKAaptPath% package -f -m -M %%~fD\AndroidManifest.xml -I %SDKApiLevelPath% -S %ProjectDir%\android\res -J %TMPDir%\src --auto-add-overlay --output-text-symbols %outputTextSymbols% >> %logFile% 2>&1
    %SDKAaptPath% package -f -m -M %%~fD\AndroidManifest.xml -I %SDKApiLevelPath% -S %ProjectDir%\android\res -J %TMPDir%\src --auto-add-overlay --output-text-symbols %outputTextSymbols% >> %logFile% 2>&1
    IF ERRORLEVEL 1 goto ERROR



    REM -----------------------------------------------------------
    @echo. >> %logFile% 2>&1
    @echo Compile R.java into R$ classes - %%~nxD >> %logFile% 2>&1
    REM -----------------------------------------------------------

    set package=%%~nxD
    set package=!package:.=\!

    @echo %JDKPath%\javac -d %TMPDir%\obj %TMPDir%\src\!package!\*.java >> %logFile% 2>&1
    %JDKPath%\javac -d %TMPDir%\obj %TMPDir%\src\!package!\*.java >> %logFile% 2>&1
    IF ERRORLEVEL 1 goto ERROR

  )

)



REM -------------------------------------------------------------------
@echo. >> %logFile% 2>&1
@echo Build a classes.dex that include the R$ classes >> %logFile% 2>&1
REM -------------------------------------------------------------------

powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::CreateFromDirectory('%TMPDir%\obj', '%TMPDir%\obj.zip'); }"
IF ERRORLEVEL 1 goto ERROR
@echo call %JavaDxPath% --dex --output=%TMPDir%\classes.dex %TMPDir%\obj.zip >> %logFile% 2>&1
call %JavaDxPath% --dex --output=%TMPDir%\classes.dex %TMPDir%\obj.zip >> %logFile% 2>&1
IF ERRORLEVEL 1 goto ERROR



REM -----------------------------------------
@echo. >> %logFile% 2>&1
@echo Merge the classes.dex >> %logFile% 2>&1
REM -----------------------------------------

@echo %JDKPath%\java -cp %JavaDxJarPath% com.android.dx.merge.DexMerger %TMPDir%\final_classes.dex %TMPDir%\classes.dex %OutputPath% >> %logFile% 2>&1
%JDKPath%\java -cp %JavaDxJarPath% com.android.dx.merge.DexMerger %TMPDir%\final_classes.dex %TMPDir%\classes.dex %OutputPath% >> %logFile% 2>&1
IF ERRORLEVEL 1 goto ERROR



REM ----------------------------------------
@echo. >> %logFile% 2>&1
@echo Move the classes.dex >> %logFile% 2>&1
REM ----------------------------------------

del %OutputPath% /q >> %logFile% 2>&1
copy %TMPDir%\final_classes.dex %OutputPath% >> %logFile% 2>&1


REM -------------------------
@echo. >> %logFile% 2>&1
@echo R.txt - You can compare against the output of: >> %logFile% 2>&1
REM -------------------------

@echo %SDKAaptPath% dump resources %APKPath% >> %logFile% 2>&1
@echo. >> %logFile% 2>&1
type %outputTextSymbols%R.txt >> %logFile%


REM --------------------------------------
@echo. >> %logFile% 2>&1
@echo remove the tmp dir >> %logFile% 2>&1
REM --------------------------------------

rmdir %TMPDir% /s /q >> %logFile% 2>&1


REM ----------------------------
@echo. >> %logFile% 2>&1
@echo Finished >> %logFile% 2>&1
REM ----------------------------


:ERROR

:EXIT