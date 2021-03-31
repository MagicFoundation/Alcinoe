@echo off

set DST2=..\..\..\lib2\static\delphi\sqlite3.o

attrib -r ..\sqlite3.o 
del ..\sqlite3.o
del %DST2%

set bcc=d:\dev\DelphiXE7
rem set bcc=d:\Dev\bcc64ce

echo ---------------------------------------------------
echo Compiling for Delphi Win64 using %bcc%

%bcc%\bin\bcc64 -isystem "%bcc%\include" -isystem "%bcc%\include\windows\sdk" -isystem "%bcc%\include\dinkumware64" -isystem "%bcc%\include\windows\crtl" -O2 -c -DWIN64 sqlite3mc.c

copy sqlite3mc.o ..\sqlite3.o
copy sqlite3mc.o %DST2%
attrib +r ..\sqlite3.o

rem pause
