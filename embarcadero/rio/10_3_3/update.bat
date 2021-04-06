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

set EmbDir=c:\Program Files (x86)\Embarcadero\Studio\20.0\source

echo Copy "%EmbDir%\fmx" to "%ProjectDir%\embarcadero\rio\10_3_3\fmx"
xcopy "%EmbDir%\fmx" "%ProjectDir%\embarcadero\rio\10_3_3\fmx"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "%EmbDir%\rtl\ios" to "%ProjectDir%\embarcadero\rio\10_3_3\rtl\ios"
xcopy "%EmbDir%\rtl\ios" "%ProjectDir%\embarcadero\rio\10_3_3\rtl\ios"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "%EmbDir%\rtl\android" to "%ProjectDir%\embarcadero\rio\10_3_3\rtl\android"
xcopy "%EmbDir%\rtl\android" "%ProjectDir%\embarcadero\rio\10_3_3\rtl\android"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Patch the source code
git apply rio_10_3_3.patch -v
IF ERRORLEVEL 1 goto ERROR
echo.

:FINISHED
@echo Finished
PAUSE
goto EXIT 

:ERROR
pause

:EXIT
