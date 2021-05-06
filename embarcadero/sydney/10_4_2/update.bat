@echo off

FOR %%a IN ("%%~dp0") DO set "ProjectDir=%%~dpa"
IF %ProjectDir:~-1%==\ SET ProjectDir=%ProjectDir:~0,-1%

SET FileName=%ProjectDir%\fmx
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=%ProjectDir%\rtl
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%
mkdir %FileName%\ios
mkdir %FileName%\android
mkdir %FileName%\win

echo Copy "c:\Program Files (x86)\Embarcadero\Studio\21.0\source\fmx" to "%ProjectDir%\fmx"
xcopy "c:\Program Files (x86)\Embarcadero\Studio\21.0\source\fmx" "%ProjectDir%\fmx"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "c:\Program Files (x86)\Embarcadero\Studio\21.0\source\rtl\ios" to "%ProjectDir%\rtl\ios"
xcopy "c:\Program Files (x86)\Embarcadero\Studio\21.0\source\rtl\ios" "%ProjectDir%\rtl\ios"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "c:\Program Files (x86)\Embarcadero\Studio\21.0\source\rtl\android" to "%ProjectDir%\rtl\android"
xcopy "c:\Program Files (x86)\Embarcadero\Studio\21.0\source\rtl\android" "%ProjectDir%\rtl\android"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "c:\Program Files (x86)\Embarcadero\Studio\21.0\source\rtl\win" to "%ProjectDir%\rtl\win"
xcopy "c:\Program Files (x86)\Embarcadero\Studio\21.0\source\rtl\win" "%ProjectDir%\rtl\win"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Patch the source code
git apply --ignore-space-change --ignore-whitespace sydney_10_4_2.patch -v
IF ERRORLEVEL 1 goto ERROR
echo.

FOR %%a IN ("%ProjectDir%\rtl\win\*") DO IF /i NOT "%%~nxa"=="Winapi.Isapi2.pas" DEL "%%a"

:FINISHED
@echo Finished
PAUSE
goto EXIT 

:ERROR
pause

:EXIT