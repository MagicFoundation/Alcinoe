@echo off

call "C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\rsvars.bat"

SET FileName=*.skincfg
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.rsm
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.stat
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.identcache
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.dproj.local
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=*.deployproj.local
del %FileName% /s
if exist %FileName% goto ERROR

SET FileName=source\dcu\Win32\tokyo
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=source\hpp\Win32\tokyo
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

SET FileName=lib\bpl\alcinoe\Win32\tokyo
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

MSBuild source\Alcinoe_tokyo.dproj /p:Config=Release /p:Platform=Win32
IF ERRORLEVEL 1 goto ERROR

call compilejar.bat off

:FINISHED

SET FileName=source\dcu\Win32\tokyo
IF EXIST %FileName% rmdir /s /q %FileName%
IF EXIST %FileName% goto ERROR
mkdir %FileName%

@echo Finished
PAUSE
goto EXIT 

:ERROR
pause

:EXIT
