#!/bin/sh

ARCH=x86_64-win64
GCC=x86_64-w64-mingw32-gcc
STATIC=../../static/$ARCH
LIB2=../../../lib2/static
STATIC2=$LIB2/$ARCH
STATIC2DELPHI=$LIB2/delphi
DST=$STATIC/sqlite3.o
DST2=$STATIC2/sqlite3.o

rm $DST
rm $DST2
rm sqlite3-$ARCH.o

echo
echo ---------------------------------------------------
echo Compiling static for FPC on $ARCH using $GCC
$GCC -O2 -static -DWIN64 -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -m64 -DNDEBUG -D_WINDOWS -c sqlite3mc.c -o sqlite3-$ARCH.o
cp sqlite3-$ARCH.o $DST
cp sqlite3-$ARCH.o $DST2

DLL=sqlite3-64.dll
rm $DLL
rm $STATIC/$DLL
rm $STATIC2DELPHI/$DLL
A=libsqlite3-64.a
rm $STATIC/$A
rm $STATIC2/$A

echo
echo ---------------------------------------------------
echo Compiling $DLL using $GCC
$GCC -O2 -shared -DSQLITE_MMAP_READWRITE -DSQLITE_ENABLE_RTREE=1 -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4  -DSQLITE_ENABLE_FTS5 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_RBU -DSQLITE_ENABLE_JSON1 -DSQLITE_ENABLE_DESERIALIZE -DWIN64 -DNDEBUG -D_WINDOWS -D_USRDLL -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_MAX_EXPR_DEPTH=0 -DSQLITE_THREADSAFE=1 -DTEMP_STORE=1 -m64 sqlite3.c -o $DLL -Wl,--out-implib,libsqlite3-64.a
cp $DLL $STATIC
cp $DLL $STATIC2DELPHI
cp $A $STATIC
cp $A $STATIC2

