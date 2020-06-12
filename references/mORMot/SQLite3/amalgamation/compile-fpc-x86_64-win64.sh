#!/bin/sh

ARCH=x86_64-win64
GCC=x86_64-w64-mingw32-gcc
STATIC=../../static/$ARCH
DST=$STATIC/sqlite3.o

rm $DST
rm sqlite3-$ARCH.o

echo
echo ---------------------------------------------------
echo Compiling static for FPC on $ARCH using $GCC
$GCC -O2 -static -DWIN64 -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -m64 -DNDEBUG -D_WINDOWS -c sqlite3mc.c -o sqlite3-$ARCH.o
cp sqlite3-$ARCH.o $DST

DLL=sqlite3-64.dll
rm $DLL
rm $STATIC/$DLL

echo
echo ---------------------------------------------------
echo Compiling $DLL using $GCC
$GCC -O2 -shared -DSQLITE_MMAP_READWRITE -DSQLITE_ENABLE_RTREE=1 -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4  -DDSQLITE_ENABLE_FTS5 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_JSON1 -DSQLITE_ENABLE_DESERIALIZE -DWIN64 -DNDEBUG -D_WINDOWS -D_USRDLL -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_MAX_EXPR_DEPTH=0 -DSQLITE_THREADSAFE=1 -DTEMP_STORE=1 -m64 sqlite3.c -o $DLL -Wl,--out-implib,libsqlite3-64.a
cp $DLL $STATIC
cp libsqlite3-64.a $STATIC

