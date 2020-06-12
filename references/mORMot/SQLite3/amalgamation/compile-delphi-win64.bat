@echo off

cd ..
attrib -r sqlite3.o 
del sqlite3.o

set bcc=c:\progs\DelphiXE7
rem set bcc=d:\Dev\bcc64ce

cd amalgamation
%bcc%\bin\bcc64 -isystem "%bcc%\include" -isystem "%bcc%\include\windows\sdk" -isystem "%bcc%\include\dinkumware64" -isystem "%bcc%\include\windows\crtl" -O2 -c -DWIN64 sqlite3mc.c
copy sqlite3mc.o ..\sqlite3.o
del sqlite3mc.o
cd ..

attrib +r sqlite3.o

cd amalgamation
pause
