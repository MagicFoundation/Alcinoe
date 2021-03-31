#!/bin/sh

FPCARCH=x86_64-linux
GCC=gcc-7
DST=../../static/$FPCARCH/sqlite3.o
DST2=../../../lib2/static/$FPCARCH/sqlite3.o

rm $DST
rm $DST2
rm sqlite3-$FPCARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $FPCARCH using $GCC
$GCC -static -fno-pic -fno-stack-protector -O2 -m64 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -c sqlite3mc.c -o sqlite3-$FPCARCH.o
cp sqlite3-$FPCARCH.o $DST
cp sqlite3-$FPCARCH.o $DST2
