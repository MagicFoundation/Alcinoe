#!/bin/sh

ARCH=i386-darwin
DST=../../static/$ARCH/sqlite3.o
DST2=../../../lib2/static/$ARCH/sqlite3.o

CROSS=/home/ab/fpcup/cross
SDK=$CROSS/lib/x86-darwin/MacOSX10.11.sdk\usr
GCC=$CROSS/bin/x86-darwin/i386-apple-darwin15

rm $DST
rm $DST2
rm sqlite3-$ARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $ARCH using $GCC
$GCC-clang -static -target i386-apple-darwin15 -O2 -m32 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -I$SDK/include -c sqlite3mc.c -o sqlite3-$ARCH.o
cp sqlite3-$ARCH.o $DST
cp sqlite3-$ARCH.o $DST2

$GCC-libtool -static sqlite3-$ARCH.o -o ../../static/$ARCH/libsqlite3.a
$GCC-libtool -static sqlite3-$ARCH.o -o ../../../lib2/static/$ARCH/libsqlite3.a
