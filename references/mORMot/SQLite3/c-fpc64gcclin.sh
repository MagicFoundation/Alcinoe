# To compile our version of sqlite3.c amalgation file for FPC compatibility

# need to create the destination folder only once
mkdir ../fpc-linux64
# need to copy this file only once (use the right source path depending on your system)
cp /usr/lib/gcc/x86_64-linux-gnu/5/libgcc.a ../fpc-linux64

cd ../fpc-linux64

rm sqlite3-64.o

gcc -c ../SQLite3/sqlite3.c -o sqlite3-64.o -O1 -ldl -lpthread -lc -DSQLITE_ENABLE_RTREE=1 -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_JSON1 -DWIN64 -DNDEBUG -D_WINDOWS -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DTHREADSAFE=1 -DTEMP_STORE=1 -DSQLITE_MAX_EXPR_DEPTH=0

echo "Done !"
