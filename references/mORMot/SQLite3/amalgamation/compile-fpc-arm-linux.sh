#!/bin/sh

ARCH=arm-linux

CROSS=/home/ab/fpcup/cross/bin/$ARCH
GCC=$CROSS/arm-linux-gnueabihf-gcc
DST=../../static/$ARCH/sqlite3.o
DST2=../../../lib2/static/$ARCH/sqlite3.o

rm $DST
rm $DST2
rm sqlite3-$ARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $ARCH using $GCC
$GCC -static -O1 -marm -march=armv7-a+fp -I$CROSS/include -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -D__ARM_PCS_VFP -mfloat-abi=hard -c sqlite3mc.c -o sqlite3-$ARCH.o
cp sqlite3-$ARCH.o $DST
cp sqlite3-$ARCH.o $DST2

