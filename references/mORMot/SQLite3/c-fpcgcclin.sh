# To compile our version of sqlite3.c amalgation file for FPC compatibility

# need to create the destination folder only once
mkdir ../fpc-linux32
# need to copy this file only once (use the right source path depending on your system)
cp /usr/lib/gcc/i686-linux-gnu/4.8/libgcc.a ../fpc-linux32

cd ../fpc-linux32

rm sqlite3.o

# gcc -c sqlite3.c -O2 -ldl -lpthread -lc -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_RTREE
# to get 32 bit on 64 bit systems
#gcc -c sqlite3.c -O2 -ldl -lpthread -lc

gcc -c ../SQLite3/sqlite3.c -O2 -m32 -ldl -lpthread -lc -DSQLITE_ENABLE_FTS3

# to get 32 bit on 64 bit systems
# gcc -c sqlite3.c -O2 -m32 -ldl -lpthread -lc
echo "Done !"
