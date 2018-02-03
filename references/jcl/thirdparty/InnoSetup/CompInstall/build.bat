@echo off
SETLOCAL

SET JCLROOT=..\..\..\jcl

SET DCC=E:\Borland\Delphi7\Bin\dcc32.exe
if not exist "%DCC%" SET DCC=dcc32.exe

"%DCC%" "-U%JCLROOT%\source\Common;%JCLROOT%\source\Windows" "-I%JCLROOT%\source\Include" -nsWinapi;System -Q -$D- -E.. CompInstall.dpr

ENDLOCAL

pause