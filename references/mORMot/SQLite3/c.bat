@echo off

attrib -r sqlite3fts3.obj
attrib -r sqlite3.obj
del sqlite3fts3.obj
del sqlite3.obj

set bcc=\dev\bccXE7
rem set bcc=d:\dev\bcc101

%bcc%\bin\bcc32 -6 -O2 -c -d -DSQLITE_ENABLE_FTS3 -u- sqlite3.c
copy sqlite3.obj sqlite3fts3.obj

rem %bcc%\bin\bcc32 -S -6 -O2 -c -d -DSQLITE_ENABLE_FTS3 -u- sqlite3.c

%bcc%\bin\bcc32 -6 -O2 -c -d -u- sqlite3.c

attrib +r sqlite3fts3.obj
attrib +r sqlite3.obj

pause
