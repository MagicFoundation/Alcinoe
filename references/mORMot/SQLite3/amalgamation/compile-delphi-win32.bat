@echo off

cd ..
attrib -r sqlite3.obj
del sqlite3.obj

set bcc=c:\progs\DelphiXE7
rem set bcc=d:\dev\bcc

cd amalgamation
%bcc%\bin\bcc32 -6 -Oi -O2 -c -d -u- sqlite3mc.c
copy sqlite3mc.obj ..\sqlite3.obj
del sqlite3mc.obj
cd ..

attrib +r sqlite3.obj

cd amalgamation
pause
