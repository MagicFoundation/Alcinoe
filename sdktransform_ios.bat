Set CurrDir="%CD%"
SET OutputDir="%CurrDir%\_outputsdktransform_ios"
IF EXIST %OutputDir% rmdir /s /q %OutputDir%
IF EXIST %OutputDir% goto ERROR
mkdir %OutputDir%

cd "c:\program files (x86)\embarcadero\studio\20.0\bin\"

"c:\program files (x86)\embarcadero\studio\20.0\bin\SdkTransform.exe" ^
-cc1 ^
-g ^
-w ^
-D TARGET_OS_IPHONE ^
-isysroot "C:\SDKs\iPhoneOS13.4.sdk" ^
-isystem "C:\SDKs\iPhoneOS13.4.sdk\usr\include" ^
-isystem "C:\SDKs\iPhoneOS11.2.sdk\usr\lib\clang" ^
-F "C:\SDKs\iPhoneOS13.4.sdk\System\Library\Frameworks" ^
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

SET Filename=tempht.m
IF EXIST %Filename% del %Filename%

goto EXIT

:ERROR
pause

:EXIT