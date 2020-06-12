#!/bin/sh

rm sqlite3patched.c
./patchsqlite3.sh >sqlite3patched.c

./compile-fpc-i386-linux.sh
./compile-fpc-x86_64-linux.sh

./compile-fpc-i386-win32.sh
./compile-fpc-x86_64-win64.sh

./compile-fpc-i386-darwin.sh
./compile-fpc-x86_64-darwin.sh

./compile-fpc-arm-linux.sh
./compile-fpc-aarch64-linux.sh

