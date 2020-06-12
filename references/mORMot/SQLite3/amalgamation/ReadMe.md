# SQLite3 Database Engine With Encryption

## How To Compile The SQlite3 Engine

1. Copy here the latest amalgamation files from  https://www.sqlite.org/download.html

2. Run the `patch-and-compile.sh` script to patch main `sqlite3.c` and cross-compile it for FPC

3. Run `c*.bat` to generate the `sqlite3.o` and `sqlite3.obj` for Delphi Win32/Win64

4. Don't forget to tune the expected *SQLite3* version text in `SynSQLite3Static.pas`

## Reference Only - Do Not Use

This source code is included as reference.

**You should not have to compile it by yourself.**

We supply and validate the proper static `.o` `.obj` files within our https://github/synopse repository, or directly from https://synopse.info/files/sqlite3fpc.7z or https://synopse.info/files/sqlite3obj.7z


## Cross-Compile

Scripts are supplied to cross-compile from Linux to other systems.

It will use either the cross-compiler as installed by `fpcupdeluxe` or you should manually add some packages.

Here are some instructions for Debian/Ubuntu.

### Cross-Compile to Linux i386 from Linux x86_64

Install the following package:

    sudo apt install libc6-dev:i386

granted the following has been run beforehand:

    dpkg --add-architecture i386

Note that `gcc-multilib` is not compatible with the arm/aarch64 cross-compilers.

This is a known debian/ubuntu bug from https://bugs.launchpad.net/ubuntu/+source/gcc-defaults/+bug/1300211

### Cross-Compile To Win32 And Win64

Install the following package:

    sudo apt install mingw-w64

### Cross-Compile to Arm And AArch64

Install the following packages:

   sudo apt install gcc-arm-linux-gnueabihf gcc-aarch64-linux-gnu

## Acknowledgment

Cut-down and corrected from https://github.com/utelle/SQLite3MultipleCiphers for bcc32 compilation.

Original MIT License - (c) 2006-2020 Ulrich Telle 