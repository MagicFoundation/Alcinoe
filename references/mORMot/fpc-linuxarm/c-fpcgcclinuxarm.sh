# To compile our version of sqlite3.c amalgation file for FPC compatibility

# need to create the destination folder only once
cp /usr/lib/gcc/arm-linux-gnueabihf/5.4.0/libgcc.a ./

rm sqlite3.o

# gcc -c sqlite3.c -O2 -ldl -lpthread -lc -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_RTREE
# to get 32 bit on 64 bit systems
#gcc -c sqlite3.c -O2 -ldl -lpthread -lc

gcc -c sqlite3.c -O2 -ldl -lpthread -lc -fno-stack-check -fno-stack-protector -DSQLITE_ENABLE_FTS3

# to get 32 bit on 64 bit systems
# gcc -c sqlite3.c -O2 -m32 -ldl -lpthread -lc
echo "Done !"
