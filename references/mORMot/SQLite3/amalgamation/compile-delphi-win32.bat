@echo off

set DST2=..\..\..\lib2\static\delphi\sqlite3.obj

attrib -r ..\sqlite3.obj
del ..\sqlite3.obj
del %DST2%

set bcc=d:\dev\DelphiXE7
rem set bcc=d:\dev\bcc

echo ---------------------------------------------------
echo Compiling for Delphi Win32 using %bcc%

%bcc%\bin\bcc32 -6 -Oi -O2 -c -d -u- sqlite3mc.c

copy sqlite3mc.obj ..\sqlite3.obj
copy sqlite3mc.obj %DST2%
attrib +r ..\sqlite3.obj

rem pause
