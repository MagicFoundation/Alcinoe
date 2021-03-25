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

echo Copy "c:\Program Files (x86)\Embarcadero\Studio\20.0\source\fmx" to "%ProjectDir%\fmx"
xcopy "c:\Program Files (x86)\Embarcadero\Studio\20.0\source\fmx" "%ProjectDir%\fmx"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "c:\Program Files (x86)\Embarcadero\Studio\20.0\source\rtl\ios" to "%ProjectDir%\rtl\ios"
xcopy "c:\Program Files (x86)\Embarcadero\Studio\20.0\source\rtl\ios" "%ProjectDir%\rtl\ios"
IF ERRORLEVEL 1 goto ERROR
echo.

echo Copy "c:\Program Files (x86)\Embarcadero\Studio\20.0\source\rtl\android" to "%ProjectDir%\rtl\android"
xcopy "c:\Program Files (x86)\Embarcadero\Studio\20.0\source\rtl\android" "%ProjectDir%\rtl\android"
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