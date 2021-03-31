# SQLite3 Database Engine With Encryption

## Reference Only - Do Not Use

This source code is included as reference.

**You should not have to compile the SQLite3 c code by yourself.**

We supply and validate the proper static `.o` `.obj` files within our https://github/synopse repository, or directly from https://synopse.info/files/sqlite3fpc.7z (for FPC) or https://synopse.info/files/sqlite3obj.7z (for Delphi).

## How To Compile The SQlite3 Engine

1. Copy here the latest amalgamation files from  https://www.sqlite.org/download.html

2. Run the `patch-and-compile.sh` script to patch main `sqlite3.c` and cross-compile it for FPC

3. Run `c*.bat` to generate the `sqlite3.o` and `sqlite3.obj` for Delphi Win32/Win64

4. Don't forget to tune the expected *SQLite3* version text in `SynSQLite3Static.pas`


## Cross-Compile

Scripts are supplied to cross-compile from Linux to other systems.

It will use either the cross-compiler as installed by `fpcupdeluxe` or you should manually add some packages.

Here are some instructions for Debian/Ubuntu.

### Cross-Compile to Linux i386 from Linux x86_64

Install the following package:

    sudo apt install libc6-dev:i386

granted the following has been run beforehand:

    dpkg --add-architecture i386

You may also try the `gcc-multilib` package as alternative.

### Cross-Compile To Win32 And Win64

Install the following package:

    sudo apt install mingw-w64

### Cross-Compile to Darwin / Linux ARM/AARCH64

Ensure you installed the latest version of the corresponding cross-compilers in `fpcupdeluxe` (in the *Cross* tab), and modify the `*.sh` path if necessary, from its default value:

    CROSS=/home/ab/fpcup/cross/bin/$ARCH 

## Acknowledgment

Our c wrapper is a cut-down, deeply adapted, version from https://github.com/utelle/SQLite3MultipleCiphers to use our `SynCrypto` unit and ensure compatibily with *bcc32/bcc64* Embarcadero compilers.

Original MIT License - (c) 2006-2020 Ulrich Telle 