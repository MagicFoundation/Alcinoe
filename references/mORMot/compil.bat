@echo off
cls

rem goto SkipOldDelphi

set DCC=c:\progs\delphi5\bin\dcc32.exe
set DelphiVersion=Delphi 5
call compilpil.bat

set DCC=c:\progs\delphi6\bin\dcc32.exe
set DelphiVersion=Delphi 6
call compilpil.bat

:SkipOldDelphi

set DelphiVersion=
set LVCL=LVCL
call compilpil.bat
set LVCL=

set DelphiVersion=
call compilpil.bat

set DCC="c:\progs\CodeGear\RAD Studio\5.0\bin\dcc32.exe"
if not exist %DCC% set DCC="c:\progs\Delphi2007\bin\dcc32.exe"
set DelphiVersion=Delphi 2007
call compilpil.bat

set DCC="c:\progs\Embarcadero\RAD Studio\10.0\bin\dcc32.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE3\bin\dcc32.exe"
set DelphiVersion=Delphi XE3 Win32
call compilpil.bat

set DCC="c:\progs\Embarcadero\RAD Studio\11.0\bin\dcc32.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE4\bin\dcc32.exe"
set DelphiVersion=Delphi XE4 Win32
call compilpil.bat

set DCC="c:\progs\Embarcadero\RAD Studio\11.0\bin\dcc64.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE4\bin\dcc64.exe"
set DelphiVersion=Delphi XE4 Win64
call compilpil.bat

set DCC="c:\progs\Embarcadero\Studio\14.0\bin\dcc32.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE6\bin\dcc32.exe"
set DelphiVersion=Delphi XE6 Win32
call compilpil.bat

set DCC="c:\progs\Embarcadero\Studio\14.0\bin\dcc64.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE6\bin\dcc64.exe"
set DelphiVersion=Delphi XE6 Win64
call compilpil.bat

set DCC="c:\progs\Embarcadero\XE7\bin\fastdcc32.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE7\bin\dcc32.exe"
set DelphiVersion=Delphi XE7 Win32
call compilpil.bat

set DCC="c:\progs\Embarcadero\XE7\bin\fastdcc64.exe"
if not exist %DCC% set DCC="c:\progs\DelphiXE7\bin\dcc64.exe"
set DelphiVersion=Delphi XE7 Win64
call compilpil.bat

pause