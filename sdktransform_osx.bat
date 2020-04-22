Set CurrDir="%CD%"
SET OutputDir="%CurrDir%\_outputsdktransform_osx"
IF EXIST %OutputDir% rmdir /s /q %OutputDir%
IF EXIST %OutputDir% goto ERROR
mkdir %OutputDir%

cd "c:\program files (x86)\embarcadero\studio\20.0\bin\"

"c:\program files (x86)\embarcadero\studio\20.0\bin\SdkTransform.exe" ^
-cc1 ^
-g ^
-w ^
--macsdk ^
-D TARGET_OS_MAC ^
-isysroot "C:\SDKs\MacOSX10.15.sdk" ^
-isystem "C:\SDKs\MacOSX10.15.sdk\usr\include" ^
-isystem "C:\SDKs\iPhoneOS12.2.sdk\usr\lib\clang" ^
-F "C:\SDKs\MacOSX10.15.sdk\System\Library\Frameworks" ^
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

SET Filename=tempht.m
IF EXIST %Filename% del %Filename%

goto EXIT

:ERROR
pause

:EXIT