/*
** Name:        sqlite3mc_vfs.h
** Purpose:     Header file for VFS of SQLite3 Multiple Ciphers support
** Author:      Ulrich Telle
** Created:     2020-03-01
** Copyright:   (c) 2020 Ulrich Telle
** License:     MIT
*/

#ifndef SQLITE3MC_VFS_H_

#include <stdlib.h>
#include <stdint.h>

SQLITE_API const char* sqlite3mc_vfs_name();
SQLITE_API void sqlite3mc_vfs_terminate();
SQLITE_API int sqlite3mc_vfs_initialize(sqlite3_vfs* vfsDefault, int makeDefault);


#endif /* SQLITE3MC_VFS_H_ */
