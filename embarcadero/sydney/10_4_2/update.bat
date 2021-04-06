@echo off

set EmbDir=c:\Program Files (x86)\Embarcadero\Studio\21.0\source

FOR %%a IN ("%%~dp0") DO set "ProjectDir=%%~dpa"
IF %ProjectDir:~-1%==\ SET ProjectDir=%ProjectDir:~0,-1%

SET FileName=%ProjectDir%\embarcadero\sydney\10_4_2\fmx
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=%ProjectDir%\embarcadero\sydney\10_4_2\rtl
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%
mkdir %FileName%\ios
mkdir %FileName%\android

echo Copy "%EmbDir%\fmx" to "%ProjectDir%\embarcadero\sydney\10_4_2\fmx"
xcopy "%EmbDir%\fmx" "%ProjectDir%\embarcadero\sydney\10_4_2\fmx"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "%EmbDir%\rtl\ios" to "%ProjectDir%\embarcadero\sydney\10_4_2\rtl\ios"
xcopy "%EmbDir%\rtl\ios" "%ProjectDir%\embarcadero\sydney\10_4_2\rtl\ios"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "%EmbDir%\rtl\android" to "%ProjectDir%\embarcadero\sydney\10_4_2\rtl\android"
xcopy "%EmbDir%\rtl\android" "%ProjectDir%\embarcadero\sydney\10_4_2\rtl\android"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Patch the source code
git apply sydney_10_4_2.patch -v
IF ERRORLEVEL 1 goto ERROR
echo.

:FINISHED
@echo Finished
PAUSE
goto EXIT 

:ERROR
pause

:EXIT
