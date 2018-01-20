# To compile our version of sqlite3.c amalgation file for FPC compatibility

# need to create the destination folder only once
mkdir .\fpc-darwin32

rm .\fpc-darwin32\sqlite3.o
rm .\fpc-darwin32\libsqlite3.a

# i386-apple-darwin15-clang.exe -c sqlite3.c -O1 -m32 -target i386-apple-darwin15 -mmacosx-version-min=10.11 -DSQLITE_ENABLE_FTS3 -o .\fpc-darwin32\sqlite3.o
i386-apple-darwin15-clang.exe -c sqlite3.c -O1 -m32 -target i386-apple-darwin15 -DSQLITE_ENABLE_FTS3 -o .\fpc-darwin32\sqlite3.o
i386-apple-darwin15-libtool.exe -static .\fpc-darwin32\sqlite3.o -o .\fpc-darwin32\libsqlite3.a

echo "Done !"

pause