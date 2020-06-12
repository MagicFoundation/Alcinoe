#!/bin/sh

ARCH=aarch64-linux
GCC=aarch64-linux-gnu-gcc
DST=../../static/$ARCH/sqlite3.o

rm $DST
rm sqlite3-$ARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $ARCH using $GCC
$GCC -static -O1 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -c sqlite3mc.c -o sqlite3-$ARCH.o
cp sqlite3-$ARCH.o $DST

