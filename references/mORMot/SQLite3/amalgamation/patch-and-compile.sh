#!/bin/sh

echo Patch SQLite3 sources

rm sqlite3patched.c
./patchsqlite3.sh >sqlite3patched.c

# ./compile-fpc-i386-linux.sh
# ./compile-fpc-x86_64-linux.sh

echo
echo Use Wine BC++ compilers for Delphi Win32/Win64

wine cmd /c compile-delphi-win32.bat
wine cmd /c compile-delphi-win64.bat

echo
echo Use Wine fpcupdeluxe cross-compilers for FPC Linux i386/x64

wine cmd /c compile-fpc-i386-linux.bat
wine cmd /c compile-fpc-x86_64-linux.bat

echo
echo Use Native cross-compilers for FPC Win32/Win64

./compile-fpc-i386-win32.sh
./compile-fpc-x86_64-win64.sh

echo
echo Use Native fpcupdeluxe cross-compilers for FPC Darwin

./compile-fpc-i386-darwin.sh
./compile-fpc-x86_64-darwin.sh

echo
echo Use Native fpcupdeluxe cross-compilers for FPC Linux arm/aarch64-linux

./compile-fpc-arm-linux.sh
./compile-fpc-aarch64-linux.sh

echo
echo Use Native fpcupdeluxe cross-compilers for FPC FreeBSD i386/x64

./compile-fpc-i386-freebsd.sh
./compile-fpc-x86_64-freebsd.sh

echo
echo Use Native fpcupdeluxe cross-compilers for FPC OpenBSD i386/x64

./compile-fpc-i386-openbsd.sh
./compile-fpc-x86_64-openbsd.sh
