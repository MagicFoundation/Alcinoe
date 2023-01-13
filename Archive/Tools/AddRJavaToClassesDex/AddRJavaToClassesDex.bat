@echo OFF
setlocal enabledelayedexpansion

REM ----------------------------------------------
REM Add this Post Build event (android platform) :
REM C:\Dev\MagicFoundation\Alcinoe\Tools\AddRJavaToClassesDex\AddRJavaToClassesDex.bat "$(PROJECTDIR)" "$(OUTPUTPATH)" "$(SDKAaptPath)" "$(SDKApiLevelPath)" "$(BDS)" "$(JDKPath)" true/false(true if you build an .aab package, false if you build an .apk)
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

set Aapt2name=Aapt2.exe
set SDKAapt2Path=%SDKAaptPath:Aapt.exe=!Aapt2name!%
@echo SDKAapt2Path=%SDKAapt2Path% >> %logFile% 2>&1

set UseAapt2=%7
if "%UseAapt2%" == "true" (
  @echo UseAapt2=true >> %logFile% 2>&1
) ELSE (
  @echo UseAapt2=false >> %logFile% 2>&1
)

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



REM -----------------------------------------
REM Compile all Android resources using AAPT2
REM -----------------------------------------

if "%UseAapt2%" == "true" (

  REM -------------------------------------------------
  @echo. >> %logFile% 2>&1
  @echo Compile all Android resources >> %logFile% 2>&1
  REM -------------------------------------------------

  @echo %SDKAapt2Path% compile --dir %ProjectDir%\android\res -o %TMPDir%\compiled_res.flata >> %logFile% 2>&1
  %SDKAapt2Path% compile --dir %ProjectDir%\android\res -o %TMPDir%\compiled_res.flata
  IF ERRORLEVEL 1 goto ERROR

)



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

    if "%UseAapt2%" == "true" (
      @echo %SDKAapt2Path% link --auto-add-overlay -I %SDKApiLevelPath% --manifest %%~fD\AndroidManifest.xml -R %TMPDir%\compiled_res.flata -o %TMPDir%\linked_res.ap_ --java %TMPDir%\src --output-text-symbols %TMPDir%\R.txt >> %logFile% 2>&1
      %SDKAapt2Path% link --auto-add-overlay -I %SDKApiLevelPath% --manifest %%~fD\AndroidManifest.xml -R %TMPDir%\compiled_res.flata -o %TMPDir%\linked_res.ap_ --java %TMPDir%\src --output-text-symbols %TMPDir%\R.txt
      IF ERRORLEVEL 1 goto ERROR
    ) ELSE (
      @echo %SDKAaptPath% package -f -m -M %%~fD\AndroidManifest.xml -I %SDKApiLevelPath% -S %%~fD\res -J %TMPDir%\src --auto-add-overlay --output-text-symbols %outputTextSymbols% >> %logFile% 2>&1
      %SDKAaptPath% package -f -m -M %%~fD\AndroidManifest.xml -I %SDKApiLevelPath% -S %ProjectDir%\android\res -J %TMPDir%\src --auto-add-overlay --output-text-symbols %outputTextSymbols% >> %logFile% 2>&1
      IF ERRORLEVEL 1 goto ERROR
    )        
   
   
    
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
REM - @echo. >> %logFile% 2>&1
REM -@echo Build a classes.dex that include the R$ classes >> %logFile% 2>&1
REM -------------------------------------------------------------------

REM -powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::CreateFromDirectory('%TMPDir%\obj', '%TMPDir%\obj.zip'); }"
REM -IF ERRORLEVEL 1 goto ERROR
REM -@echo call %JavaDxPath% --dex --output=%TMPDir%\classes.dex %TMPDir%\obj.zip >> %logFile% 2>&1
REM -call %JavaDxPath% --dex --output=%TMPDir%\classes.dex %TMPDir%\obj.zip >> %logFile% 2>&1
REM -IF ERRORLEVEL 1 goto ERROR



REM -----------------------------------------
REM -@echo. >> %logFile% 2>&1
REM -@echo Merge the classes.dex >> %logFile% 2>&1
REM -----------------------------------------

REM -@echo %JDKPath%\java -cp %JavaDxJarPath% com.android.dx.merge.DexMerger %TMPDir%\final_classes.dex %TMPDir%\classes.dex %OutputPath% >> %logFile% 2>&1
REM -%JDKPath%\java -cp %JavaDxJarPath% com.android.dx.merge.DexMerger %TMPDir%\final_classes.dex %TMPDir%\classes.dex %OutputPath% >> %logFile% 2>&1
REM -IF ERRORLEVEL 1 goto ERROR


REM -goto ERROR

REM -@ping -n 30 localhost> nul

REM -IF EXIST C:\Dev\MagicFoundation\Alcinoe\Demos\ALFirebaseMessaging\Android64\Debug\dex_list.txt GOTO ERROR

REM -"c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --dexmerge="C:\Program Files\Eclipse Adoptium\jdk-11.0.16.101-hotspot\bin\java.exe,c:\program files (x86)\embarcadero\studio\22.0\bin\Android\r8-3.3.28.jar,C:\Dev\MagicFoundation\Alcinoe\Demos\ALFirebaseMessaging\Android64\Debug\ALFirebaseMessaging.classes,23,C:\temp\dex\dex_list.txt" 

jar -cf c:\Dev\MagicFoundation\Alcinoe\Demos\ALFirebaseMessaging\_source\android\libraries\com.ALFirebaseMessaging.app\R.jar c:\Dev\MagicFoundation\Alcinoe\Tools\AddRJavaToClassesDex\tmp\obj\ 




REM ----------------------------------------
REM -@echo. >> %logFile% 2>&1
REM -@echo Move the classes.dex >> %logFile% 2>&1
REM ----------------------------------------

REM -del %OutputPath% /q >> %logFile% 2>&1
REM -copy %TMPDir%\final_classes.dex %OutputPath% >> %logFile% 2>&1


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

REM -rmdir %TMPDir% /s /q >> %logFile% 2>&1


REM ----------------------------
@echo. >> %logFile% 2>&1
@echo Finished >> %logFile% 2>&1
REM ----------------------------


:ERROR

:EXIT