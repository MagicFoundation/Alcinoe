@echo off

del *.suo /s /a +h
IF ERRORLEVEL 1 goto ERROR

del *.user /s 
IF ERRORLEVEL 1 goto ERROR

del *.ncb /s 
IF ERRORLEVEL 1 goto ERROR

del *.sdf /s 
IF ERRORLEVEL 1 goto ERROR

del *.sbr /s 
IF ERRORLEVEL 1 goto ERROR

del *.log /s 
IF ERRORLEVEL 1 goto ERROR

rmdir .\source\x64 /s /q
IF ERRORLEVEL 1 goto ERROR

rmdir .\source\ipch /s /q
IF ERRORLEVEL 1 goto ERROR

rmdir .\source\Win32 /s /q
IF ERRORLEVEL 1 goto ERROR

GOTO END

:ERROR
PAUSE
EXIT

:END
 