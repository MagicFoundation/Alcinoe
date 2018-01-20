@rem Use MINGW to compile our version of sqlite3.c amalgation file for FPC compatibility

set mingwvers=5.2.0
set mingw=c:\progs\mingw\i686-%mingwvers%-posix-dwarf-rt_v4-rev0\mingw32

set path=%path%;%mingw%\bin

@rem need to create the destination folder only once

mkdir ..\fpc-win32
mkdir ..\fpc-linux32
@rem need to copy these files only once
copy %mingw%\i686-w64-mingw32\lib\libkernel32.a ..\fpc-win32
copy %mingw%\lib\gcc\i686-w64-mingw32\%mingwvers%\libgcc.a  ..\fpc-win32

cd ..\fpc-win32

attrib -r sqlite3.o 
del sqlite3.o

gcc -O2 -c -DSQLITE_ENABLE_FTS3 ..\SQLite3\sqlite3.c

attrib +r sqlite3.o 

pause
