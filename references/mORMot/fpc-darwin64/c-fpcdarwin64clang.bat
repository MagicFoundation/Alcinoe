# To compile our version of sqlite3.c amalgation file for FPC compatibility

# need to create the destination folder only once
mkdir .\fpc-darwin64

rm .\fpc-darwin64\sqlite3.o
rm .\fpc-darwin64\libsqlite3.a

x86_64-apple-darwin15-clang.exe -c sqlite3.c -O1 -m64 -DSQLITE_ENABLE_FTS3 -o .\fpc-darwin64\sqlite3.o
x86_64-apple-darwin15-libtool.exe -static .\fpc-darwin64\sqlite3.o -o .\fpc-darwin64\libsqlite3.a

echo "Done !"

pause