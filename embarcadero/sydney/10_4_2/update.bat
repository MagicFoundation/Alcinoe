@echo off

FOR /F "usebackq tokens=3*" %%A IN (`reg query "HKCU\Software\Embarcadero\BDS\21.0" /v RootDir`) DO set EmbSourceDir=%%A %%B 
set EmbSourceDir=%EmbSourceDir:~0,-1%source

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

echo Copy "%EmbSourceDir%\fmx" to "%ProjectDir%\fmx"
xcopy "%EmbSourceDir%\fmx" "%ProjectDir%\fmx"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "%EmbSourceDir%\rtl\ios" to "%ProjectDir%\rtl\ios"
xcopy "%EmbSourceDir%\rtl\ios" "%ProjectDir%\rtl\ios"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "%EmbSourceDir%\rtl\android" to "%ProjectDir%\rtl\android"
xcopy "%EmbSourceDir%\rtl\android" "%ProjectDir%\rtl\android"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "%EmbSourceDir%\rtl\win" to "%ProjectDir%\rtl\win"
xcopy "%EmbSourceDir%\rtl\win" "%ProjectDir%\rtl\win"
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