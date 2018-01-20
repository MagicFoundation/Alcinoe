@rem Use MINGW64 to compile sqlite3.c amalgation file for FPC compatibility

set mingwvers=5.2.0
set mingw=c:\progs\mingw\x86_64-%mingwvers%-posix-seh-rt_v4-rev0\mingw64

set path=%path%;%mingw%\bin

@rem echo path

@rem need to create the destination folder only once

mkdir ..\fpc-win64
@rem need to copy these files only once
copy %mingw%\x86_64-w64-mingw32\lib\libkernel32.a ..\fpc-win64
copy %mingw%\x86_64-w64-mingw32\lib\libmsvcrt.a ..\fpc-win64
copy %mingw%\lib\gcc\x86_64-w64-mingw32\%mingwvers%\libgcc.a  ..\fpc-win64

cd ..\fpc-win64

attrib -r sqlite3-64.o 
del sqlite3-64.o
attrib -r ..\SQLite3\exe\sqlite3-64.dll
del ..\SQLite3\exe\sqlite3-64.dll

gcc -O2 -shared -DSQLITE_MMAP_READWRITE -DSQLITE_ENABLE_RTREE=1 -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_JSON1 -DWIN64 -DNDEBUG -D_WINDOWS -D_USRDLL -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DTHREADSAFE=1 -DTEMP_STORE=1 -m64 -I. ..\SQLite3\amalgamation\sqlite3.c -o ..\SQLite3\exe\sqlite3-64.dll -Wl,--out-implib,libsqlite3-64.a

@rem here we use -O1 since -O2 triggers unexpected GPF :(
gcc -c -O1 -static -DSQLITE_ENABLE_RTREE=1 -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_JSON1 -DWIN64 -DNDEBUG -D_WINDOWS -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DTHREADSAFE=1 -DTEMP_STORE=1 -DSQLITE_MAX_EXPR_DEPTH=0 -fno-stack-check -fno-stack-protector -mno-stack-arg-probe -m64 -I. ..\SQLite3\sqlite3.c -o sqlite3-64.o

attrib +r sqlite3-64.o 
attrib +r ..\SQLite3\exe\sqlite3-64.dll

pause
