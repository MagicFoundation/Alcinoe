/*
** Name:        sqlite3mc_vfs.c
** Purpose:     Implementation of SQLite VFS for Multiple Ciphers
** Author:      Ulrich Telle
** Created:     2020-02-28
** Copyright:   (c) 2020 Ulrich Telle
** License:     MIT
** Patched by:  Arnaud Bouchez for BCC32 proper compilation
*/

#include "sqlite3mc_vfs.h"
#include "sqlite3.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

/*
** Type definitions
*/
typedef struct sqlite3mc_db sqlite3mc_db;
typedef struct sqlite3mc_file sqlite3mc_file;
typedef struct sqlite3mc_vfs sqlite3mc_vfs;

/*
** MultiCipher file structure
*/
struct sqlite3mc_db
{
  sqlite3mc_db* dbNext;        /* Pointer to next entry in list */
  sqlite3* db;                 /* Database connection */
  int dbIndex;                 /* Database index (0 for main database, >= 2 for attached databases) */
};

struct sqlite3mc_file
{
  sqlite3_file base;           /* sqlite3_file I/O methods */
  sqlite3_file* pFile;         /* Real underlying OS file */
  const char* zFileName;       /* File name */
  int openFlags;               /* Open flags */
  sqlite3mc_file* pMainNext;   /* Next main db file */
  sqlite3mc_file* pMainDb;     /* Main database to which this one is attached */
  sqlite3mc_db* pDb;           /* Head of list of database connections */
  Codec* codec;                /* Codec if encrypted */
  int pageNo;                  /* Page number (in case of journal files) */
};

/*
** MultiCipher VFS structure
*/
struct sqlite3mc_vfs
{
	sqlite3_vfs base;      /* MultiCipher VFS shim methods */
  sqlite3_vfs* pVfs;     /* Underlying VFS */
  sqlite3_mutex* mutex;  /* Mutex to protect pMain */
  sqlite3mc_file* pMain; /* List of main database files */
};

#define REALVFS(p) (((sqlite3mc_vfs*)(p))->pVfs)
#define REALFILE(p) (((sqlite3mc_file*)(p))->pFile)

/*
** Prototypes for VFS methods
*/

static int mcVfsOpen(sqlite3_vfs* pVfs, const char* zName, sqlite3_file* pFile, int flags, int* pOutFlags);
static int mcVfsDelete(sqlite3_vfs* pVfs, const char* zName, int syncDir);
static int mcVfsAccess(sqlite3_vfs* pVfs, const char* zName, int flags, int* pResOut);
static int mcVfsFullPathname(sqlite3_vfs* pVfs, const char* zName, int nOut, char* zOut);
static void* mcVfsDlOpen(sqlite3_vfs* pVfs, const char* zFilename);
static void mcVfsDlError(sqlite3_vfs* pVfs, int nByte, char* zErrMsg);
static void (*mcVfsDlSym(sqlite3_vfs* pVfs, void* p, const char* zSymbol))(void);
static void mcVfsDlClose(sqlite3_vfs* pVfs, void* p);
static int mcVfsRandomness(sqlite3_vfs* pVfs, int nByte, char* zOut);
static int mcVfsSleep(sqlite3_vfs* pVfs, int microseconds);
static int mcVfsCurrentTime(sqlite3_vfs* pVfs, double* pOut);
static int mcVfsGetLastError(sqlite3_vfs* pVfs, int nErr, char* zOut);
static int mcVfsCurrentTimeInt64(sqlite3_vfs* pVfs, sqlite3_int64* pOut);
static int mcVfsSetSystemCall(sqlite3_vfs* pVfs, const char* zName, sqlite3_syscall_ptr pNewFunc);
static sqlite3_syscall_ptr mcVfsGetSystemCall(sqlite3_vfs* pVfs, const char* zName);
static const char* mcVfsNextSystemCall(sqlite3_vfs* pVfs, const char* zName);

/*
** Prototypes for IO methods
*/

static int mcIoClose(sqlite3_file* pFile);
static int mcIoRead(sqlite3_file* pFile, void*, int iAmt, sqlite3_int64 iOfst);
static int mcIoWrite(sqlite3_file* pFile,const void*,int iAmt, sqlite3_int64 iOfst);
static int mcIoTruncate(sqlite3_file* pFile, sqlite3_int64 size);
static int mcIoSync(sqlite3_file* pFile, int flags);
static int mcIoFileSize(sqlite3_file* pFile, sqlite3_int64* pSize);
static int mcIoLock(sqlite3_file* pFile, int lock);
static int mcIoUnlock(sqlite3_file* pFile, int lock);
static int mcIoCheckReservedLock(sqlite3_file* pFile, int *pResOut);
static int mcIoFileControl(sqlite3_file* pFile, int op, void *pArg);
static int mcIoSectorSize(sqlite3_file* pFile);
static int mcIoDeviceCharacteristics(sqlite3_file* pFile);
static int mcIoShmMap(sqlite3_file* pFile, int iPg, int pgsz, int map, void volatile** p);
static int mcIoShmLock(sqlite3_file* pFile, int offset, int n, int flags);
static void mcIoShmBarrier(sqlite3_file* pFile);
static int mcIoShmUnmap(sqlite3_file* pFile, int deleteFlag);
static int mcIoFetch(sqlite3_file* pFile, sqlite3_int64 iOfst, int iAmt, void** pp);
static int mcIoUnfetch(sqlite3_file* pFile, sqlite3_int64 iOfst, void* p);

#define SQLITE3MC_VFS_NAME ("multicipher")

static const int walFrameHeaderSize = 24;
static const int walFileHeaderSize = 32;

static sqlite3mc_vfs mcVfsGlobal =
{
  {
    3,                      /* iVersion */
    0,                      /* szOsFile */
    1024,                   /* mxPathname */
    0,                      /* pNext */
    SQLITE3MC_VFS_NAME,     /* zName */
    0,                      /* pAppData */
    mcVfsOpen,              /* xOpen */
    mcVfsDelete,            /* xDelete */
    mcVfsAccess,            /* xAccess */
    mcVfsFullPathname,      /* xFullPathname */
    mcVfsDlOpen,            /* xDlOpen */
    mcVfsDlError,           /* xDlError */
    mcVfsDlSym,             /* xDlSym */
    mcVfsDlClose,           /* xDlClose */
    mcVfsRandomness,        /* xRandomness */
    mcVfsSleep,             /* xSleep */
    mcVfsCurrentTime,       /* xCurrentTime */
    mcVfsGetLastError,      /* xGetLastError */
    mcVfsCurrentTimeInt64,  /* xCurrentTimeInt64 */
    mcVfsSetSystemCall,     /* xSetSystemCall */
    mcVfsGetSystemCall,     /* xGetSystemCall */
    mcVfsNextSystemCall     /* xNextSystemCall */
  },
  NULL
};

static sqlite3_io_methods mcIoMethodsGlobal =
{
  3,                          /* iVersion */
  mcIoClose,                  /* xClose */
  mcIoRead,                   /* xRead */
  mcIoWrite,                  /* xWrite */
  mcIoTruncate,               /* xTruncate */
  mcIoSync,                   /* xSync */
  mcIoFileSize,               /* xFileSize */
  mcIoLock,                   /* xLock */
  mcIoUnlock,                 /* xUnlock */
  mcIoCheckReservedLock,      /* xCheckReservedLock */
  mcIoFileControl,            /* xFileControl */
  mcIoSectorSize,             /* xSectorSize */
  mcIoDeviceCharacteristics,  /* xDeviceCharacteristics */
  mcIoShmMap,                 /* xShmMap */
  mcIoShmLock,                /* xShmLock */
  mcIoShmBarrier,             /* xShmBarrier */
  mcIoShmUnmap,               /* xShmUnmap */
  mcIoFetch,                  /* xFetch */
  mcIoUnfetch,                /* xUnfetch */
};

/*
** Internal functions
*/

/*
** Add an item to the list of main database files, if it is not already present.
*/
static void mcMainListAdd(sqlite3mc_file* pFile)
{
  assert( (pFile->openFlags & SQLITE_OPEN_MAIN_DB) );
  sqlite3_mutex_enter(mcVfsGlobal.mutex);
  pFile->pMainNext = mcVfsGlobal.pMain;
  mcVfsGlobal.pMain = pFile;
  sqlite3_mutex_leave(mcVfsGlobal.mutex);
}

/*
** Remove an item from the list of main database files.
*/
static void mcMainListRemove(sqlite3mc_file* pFile)
{
  sqlite3mc_file** pMainPrev;
  sqlite3_mutex_enter(mcVfsGlobal.mutex);
  for (pMainPrev = &mcVfsGlobal.pMain; *pMainPrev && *pMainPrev != pFile; pMainPrev = &((*pMainPrev)->pMainNext)){}
  if (*pMainPrev) *pMainPrev = pFile->pMainNext;
  pFile->pMainNext = 0;
  sqlite3_mutex_leave(mcVfsGlobal.mutex);
}

/*
** Given that zFileName points to a buffer containing a database file name passed to 
** either the xOpen() or xAccess() VFS method, search the list of main database files
** for a file handle opened by the same database connection on the corresponding
** database file.
*/
static sqlite3mc_file* mcFindDbMainFileName(sqlite3mc_vfs* mcVfs, const char* zFileName)
{
  sqlite3mc_file* pDb;
  sqlite3_mutex_enter(mcVfs->mutex);
  for (pDb = mcVfs->pMain; pDb && (pDb->zFileName != zFileName); 
    pDb = pDb->pMainNext){}
  sqlite3_mutex_leave(mcVfs->mutex);
  return pDb;
}

#if 0
/*
** Given that dbIndex is the index of a main database or attached database within a
** database connection, search the list of main database files for the file handle
** of the corresponding database file.
*/
static sqlite3mc_file* mcFindDbMainFile(sqlite3mc_vfs* mcVfs, sqlite3* db, int dbIndex)
{
  sqlite3mc_file* pDbMain;
  sqlite3_mutex_enter(mcVfs->mutex);
  for (pDbMain = mcVfs->pMain; pDbMain && mcDbListFind(pDbMain, db, dbIndex) == NULL; pDbMain = pDbMain->pMainNext) {}
  sqlite3_mutex_leave(mcVfs->mutex);
  return pDbMain;
}
#endif

/*
** Find the codec of the database file
** corresponding to the database index.
*/
SQLITE_PRIVATE Codec* sqlite3mcGetCodec(sqlite3* db, const char* zDbName)
{
  Codec* codec = NULL;
  const char* dbFileName = sqlite3_db_filename(db, zDbName);
  sqlite3mc_file* pDbMain = mcFindDbMainFileName(&mcVfsGlobal, dbFileName);
  if (pDbMain)
  {
    codec = pDbMain->codec;
  }
  return codec;
}

/*
** Find the codec of the main database file.
*/
SQLITE_PRIVATE Codec* sqlite3mcGetMainCodec(sqlite3* db)
{
  return sqlite3mcGetCodec(db, "main");
}

/*
** Set the codec of the database file with the given database index.
*/
SQLITE_PRIVATE void sqlite3mcSetCodec(sqlite3* db, const char* zFileName, Codec* codec)
{
  sqlite3mc_file* pDbMain = mcFindDbMainFileName(&mcVfsGlobal, zFileName);
  
  //fprintf(stdout,"sqlite3mcSetCodec %s as %x\n", zFileName, pDbMain); 

  if (pDbMain)
  {
    if (pDbMain->codec)
    {
      sqlite3mcCodecFree(pDbMain->codec);
    }
    pDbMain->codec = codec;
  }
  else
  {
    /*
    ** No main database file handle found (e.g. vaccum or attached
    ** from memory db): free codec
    */
    sqlite3mcCodecFree(codec);
  }
}

/*
** Implementation of VFS methods
*/

static int mcVfsOpen(sqlite3_vfs* pVfs, const char* zName, sqlite3_file* pFile, int flags, int* pOutFlags)
{
  int rc;
  const char* dbFileName;
  sqlite3mc_vfs* mcVfs = (sqlite3mc_vfs*) pVfs;
  sqlite3mc_file* mcFile = (sqlite3mc_file*) pFile;
  mcFile->pFile = (sqlite3_file*) &mcFile[1];
  mcFile->openFlags = flags;
  mcFile->zFileName = zName;
  mcFile->pDb = NULL;
  mcFile->codec = 0;
  mcFile->pMainDb = 0;
  mcFile->pMainNext = 0;
  mcFile->pageNo = 0;
  
  //fprintf(stdout,"mcVfsOpen %s %x\n", zName, flags); 
  
  if (zName)
  {
    if (flags & SQLITE_OPEN_MAIN_DB)
    {
      /* A main database has just been opened.
      */
      mcFile->zFileName = zName;
    }
#if 1
    else if (flags & SQLITE_OPEN_TEMP_DB)
    {
      mcFile->zFileName = zName;
  }
#endif
#if 0
    else if (flags & SQLITE_OPEN_TRANSIENT_DB)
    {
    }
#endif
    else if (flags & SQLITE_OPEN_MAIN_JOURNAL)
    {
      mcFile->zFileName = zName;
      dbFileName = sqlite3_filename_database(zName);
      mcFile->pMainDb = mcFindDbMainFileName(&mcVfsGlobal, dbFileName);
    }
#if 0
    else if (flags & SQLITE_OPEN_TEMP_JOURNAL)
    {
    }
#endif
    else if (flags & SQLITE_OPEN_SUBJOURNAL)
    {
      mcFile->zFileName = zName;
      dbFileName = sqlite3_filename_database(zName);
      mcFile->pMainDb = mcFindDbMainFileName(&mcVfsGlobal, dbFileName);
    }
#if 0
    else if (flags & SQLITE_OPEN_MASTER_JOURNAL)
    {
    }
#endif
    else if (flags & SQLITE_OPEN_WAL)
    {
      mcFile->zFileName = zName;
      dbFileName = sqlite3_filename_database(zName);
      mcFile->pMainDb = mcFindDbMainFileName(&mcVfsGlobal, dbFileName);
    }
  }

  rc = REALVFS(pVfs)->xOpen(REALVFS(pVfs), zName, mcFile->pFile, flags, pOutFlags);
  if (rc == SQLITE_OK)
  {
    pFile->pMethods = &mcIoMethodsGlobal;
    if (flags & SQLITE_OPEN_MAIN_DB)
    {
      mcMainListAdd(mcFile);
    }
  }
  return rc;
}

static int mcVfsDelete(sqlite3_vfs* pVfs, const char* zName, int syncDir)
{
  return REALVFS(pVfs)->xDelete(REALVFS(pVfs), zName, syncDir);
}

static int mcVfsAccess(sqlite3_vfs* pVfs, const char* zName, int flags, int* pResOut)
{
  return REALVFS(pVfs)->xAccess(REALVFS(pVfs), zName, flags, pResOut);
}

static int mcVfsFullPathname(sqlite3_vfs* pVfs, const char* zName, int nOut, char* zOut)
{
  return REALVFS(pVfs)->xFullPathname(REALVFS(pVfs), zName, nOut, zOut);
}

static void* mcVfsDlOpen(sqlite3_vfs* pVfs, const char* zFilename)
{
  return REALVFS(pVfs)->xDlOpen(REALVFS(pVfs), zFilename);
}

static void mcVfsDlError(sqlite3_vfs* pVfs, int nByte, char* zErrMsg)
{
  REALVFS(pVfs)->xDlError(REALVFS(pVfs), nByte, zErrMsg);
}

static void (*mcVfsDlSym(sqlite3_vfs* pVfs, void* p, const char* zSymbol))(void)
{
  return REALVFS(pVfs)->xDlSym(REALVFS(pVfs), p, zSymbol);
}

static void mcVfsDlClose(sqlite3_vfs* pVfs, void* p)
{
  REALVFS(pVfs)->xDlClose(REALVFS(pVfs), p);
}

static int mcVfsRandomness(sqlite3_vfs* pVfs, int nByte, char* zOut)
{
  return REALVFS(pVfs)->xRandomness(REALVFS(pVfs), nByte, zOut);
}

static int mcVfsSleep(sqlite3_vfs* pVfs, int microseconds)
{
  return REALVFS(pVfs)->xSleep(REALVFS(pVfs), microseconds);
}

static int mcVfsCurrentTime(sqlite3_vfs* pVfs, double* pOut)
{
  return REALVFS(pVfs)->xCurrentTime(REALVFS(pVfs), pOut);
}

static int mcVfsGetLastError(sqlite3_vfs* pVfs, int code, char* pOut)
{
  return REALVFS(pVfs)->xGetLastError(REALVFS(pVfs), code, pOut);
}

static int mcVfsCurrentTimeInt64(sqlite3_vfs* pVfs, sqlite3_int64* pOut)
{
  return REALVFS(pVfs)->xCurrentTimeInt64(REALVFS(pVfs), pOut);
}

static int mcVfsSetSystemCall(sqlite3_vfs* pVfs, const char* zName, sqlite3_syscall_ptr pNewFunc)
{
  return REALVFS(pVfs)->xSetSystemCall(REALVFS(pVfs), zName, pNewFunc);
}

static sqlite3_syscall_ptr mcVfsGetSystemCall(sqlite3_vfs* pVfs, const char* zName)
{
  return REALVFS(pVfs)->xGetSystemCall(REALVFS(pVfs), zName);
}

static const char* mcVfsNextSystemCall(sqlite3_vfs* pVfs, const char* zName)
{
  return REALVFS(pVfs)->xNextSystemCall(REALVFS(pVfs), zName);
}

/*
** IO methods
*/

static int mcIoClose(sqlite3_file* pFile)
{
  sqlite3mc_file* p = (sqlite3mc_file*) pFile;
  int rc;
  if (p->openFlags & SQLITE_OPEN_MAIN_DB)
  {
    mcMainListRemove(p);
  }
  if (p->codec)
  {
    sqlite3mcCodecFree(p->codec);
    p->codec = 0;
  }

  assert(p->pMainNext == 0 && mcVfsGlobal.pMain != p);
  rc = REALFILE(pFile)->pMethods->xClose(REALFILE(pFile));
  return rc;
}

static int mcReadMainDb(sqlite3_file* pFile, void* buffer, int count, sqlite3_int64 offset)
{
  int rc = SQLITE_OK;
  int pageNo;
  void* bufferDecrypted;
  sqlite3mc_file* mcFile = (sqlite3mc_file*) pFile;

  /*
  ** Special case: read 16 bytes salt from beginning of database file without decrypting
  */
  if (offset == 0 && count == 16)
  {
    return rc;
  }

  if (mcFile->codec != 0 && CodecIsEncrypted(mcFile->codec))
  {
    const int pageSize = sqlite3mcGetPageSize(mcFile->codec);
    const int deltaOffset = offset % pageSize;
    const int deltaCount = count % pageSize;
    if (deltaOffset || deltaCount)
    {
      const sqlite3_int64 prevOffset = offset - deltaOffset;
      unsigned char* pageBuffer = CodecGetPageBuffer(mcFile->codec);
      rc = REALFILE(pFile)->pMethods->xRead(REALFILE(pFile), pageBuffer, pageSize, prevOffset);
      if (rc == SQLITE_IOERR_SHORT_READ)
      {
        return rc;
      }
      pageNo = prevOffset / pageSize + 1;
      bufferDecrypted = sqlite3mcCodec(mcFile->codec, pageBuffer, pageNo, 3);
      if (deltaOffset)
      {
        memcpy(buffer, pageBuffer + deltaOffset, count);
      }
      else
      {
        memcpy(buffer, pageBuffer, count);
      }
    }
    else
    {
      unsigned char* data = (unsigned char*)buffer;
      int pageNo = offset / pageSize + 1;
      int nPages = count / pageSize;
      int iPage;
      for (iPage = 0; iPage < nPages; ++iPage)
      {
        void* bufferDecrypted = sqlite3mcCodec(mcFile->codec, data, pageNo, 3);
        data += pageSize;
        offset += pageSize;
        ++pageNo;
      }
    }
  }
  return rc;
}

static int mcReadMainJournal(sqlite3_file* pFile, const void* buffer, int count, sqlite3_int64 offset)
{
  int rc = SQLITE_OK;
  sqlite3mc_file* mcFile = (sqlite3mc_file*)pFile;
  Codec* codec = (mcFile->pMainDb) ? mcFile->pMainDb->codec : 0;

  if (codec != 0 && CodecIsEncrypted(codec))
  {
    const int pageSize = sqlite3mcGetPageSize(codec);

    if (count == pageSize && mcFile->pageNo != 0)
    {
      void* bufferDecrypted = sqlite3mcCodec(codec, (char*)buffer, mcFile->pageNo, 3);
      mcFile->pageNo = 0;
    }
    else if (count == 4)
    {
      mcFile->pageNo = sqlite3Get4byte(buffer);
    }
  }
  return rc;
}

static int mcReadSubJournal(sqlite3_file* pFile, const void* buffer, int count, sqlite3_int64 offset)
{
  int rc = SQLITE_OK;
  sqlite3mc_file* mcFile = (sqlite3mc_file*)pFile;
  Codec* codec = (mcFile->pMainDb) ? mcFile->pMainDb->codec : 0;

  if (codec != 0 && CodecIsEncrypted(codec))
  {
    const int pageSize = sqlite3mcGetPageSize(codec);

    if (count == pageSize && mcFile->pageNo != 0)
    {
      void* bufferDecrypted = sqlite3mcCodec(codec, (char*) buffer, mcFile->pageNo, 3);
    }
    else if (count == 4)
    {
      mcFile->pageNo = sqlite3Get4byte(buffer);
    }
  }
  return rc;
}


static int mcReadWal(sqlite3_file* pFile, const void* buffer, int count, sqlite3_int64 offset)
{
  int rc = SQLITE_OK;
  sqlite3mc_file* mcFile = (sqlite3mc_file*)pFile;
  Codec* codec = (mcFile->pMainDb) ? mcFile->pMainDb->codec : 0;

  if (codec != 0 && CodecIsEncrypted(codec))
  {
    const int pageSize = sqlite3mcGetPageSize(codec);

    if (count == pageSize)
    {
      int pageNo = 0;
      char ac[4];
      rc = REALFILE(pFile)->pMethods->xRead(REALFILE(pFile), ac, 4, offset - walFrameHeaderSize);
      if (rc == SQLITE_OK)
      {
        pageNo = sqlite3Get4byte(ac);
      }
      if (pageNo != 0)
      {
        void* bufferDecrypted = sqlite3mcCodec(codec, (char*)buffer, pageNo, 3);
      }
    }
  }
  return rc;
}

static int mcIoRead(sqlite3_file* pFile, void* buffer, int count, sqlite3_int64 offset)
{
  sqlite3mc_file* mcFile = (sqlite3mc_file*)pFile;
  int rc = REALFILE(pFile)->pMethods->xRead(REALFILE(pFile), buffer, count, offset);
  if (rc == SQLITE_IOERR_SHORT_READ)
  {
    return rc;
  }

  if (mcFile->openFlags & SQLITE_OPEN_MAIN_DB)
  {
    rc = mcReadMainDb(pFile, buffer, count, offset);
  }
#if 0
  else if (mcFile->openFlags & SQLITE_OPEN_TEMP_DB)
  {
  }
#endif
#if 0
  else if (mcFile->openFlags & SQLITE_OPEN_TRANSIENT_DB)
  {
  }
#endif
  else if (mcFile->openFlags & SQLITE_OPEN_MAIN_JOURNAL)
  {
    rc = mcReadMainJournal(pFile, buffer, count, offset);
  }
#if 0
  else if (mcFile->openFlags & SQLITE_OPEN_TEMP_JOURNAL)
  {
  }
#endif
  else if (mcFile->openFlags & SQLITE_OPEN_SUBJOURNAL)
  {
    rc = mcReadSubJournal(pFile, buffer, count, offset);
  }
#if 0
  else if (mcFile->openFlags & SQLITE_OPEN_MASTER_JOURNAL)
  {
  }
#endif
  else if (mcFile->openFlags & SQLITE_OPEN_WAL)
  {
    rc = mcReadWal(pFile, buffer, count, offset);
  }
  return rc;
}

static int mcWriteMainDb(sqlite3_file* pFile, const void* buffer, int count, sqlite3_int64 offset)
{
  int rc = SQLITE_OK;
  sqlite3mc_file* mcFile = (sqlite3mc_file*)pFile;

  if (mcFile->codec != 0 && CodecIsEncrypted(mcFile->codec))
  {
    const int pageSize = sqlite3mcGetPageSize(mcFile->codec);
    const int deltaOffset = offset % pageSize;
    const int deltaCount = count % pageSize;

    if (deltaOffset || deltaCount)
    {
      rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
    }
    else
    {
      char* data = (char*)buffer;
      int pageNo = offset / pageSize + 1;
      int nPages = count / pageSize;
      int iPage;
      for (iPage = 0; iPage < nPages; ++iPage)
      {
        void* bufferEncrypted = sqlite3mcCodec(mcFile->codec, data, pageNo, 6);
        rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), bufferEncrypted, pageSize, offset);
        data += pageSize;
        offset += pageSize;
        ++pageNo;
      }
    }
  }
  else
  {
    rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
  }
  return rc;
}

static int mcWriteMainJournal(sqlite3_file* pFile, const void* buffer, int count, sqlite3_int64 offset)
{
  int rc = SQLITE_OK;
  sqlite3mc_file* mcFile = (sqlite3mc_file*)pFile;
  Codec* codec = (mcFile->pMainDb) ? mcFile->pMainDb->codec : 0;

  if (codec != 0 && CodecIsEncrypted(codec))
  {
    const int pageSize = sqlite3mcGetPageSize(codec);
    const int frameSize = pageSize + 4 + 4;

    if (count == pageSize && mcFile->pageNo != 0)
    {
      void* bufferEncrypted = sqlite3mcCodec(codec, (char*)buffer, mcFile->pageNo, 7);
      rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), bufferEncrypted, pageSize, offset);
    }
    else
    {
      rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
      if (count == 4)
      {
        mcFile->pageNo = (rc == SQLITE_OK) ? sqlite3Get4byte(buffer) : 0;
      }
    }
  }
  else
  {
    rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
  }
  return rc;
}

static int mcWriteSubJournal(sqlite3_file* pFile, const void* buffer, int count, sqlite3_int64 offset)
{
  int rc = SQLITE_OK;
  sqlite3mc_file* mcFile = (sqlite3mc_file*)pFile;
  Codec* codec = (mcFile->pMainDb) ? mcFile->pMainDb->codec : 0;

  if (codec != 0 && CodecIsEncrypted(codec))
  {
    const int pageSize = sqlite3mcGetPageSize(codec);
    const int frameSize = pageSize + 4;

    if (count == pageSize && mcFile->pageNo != 0)
    {
      void* bufferEncrypted = sqlite3mcCodec(codec, (char*) buffer, mcFile->pageNo, 7);
      rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), bufferEncrypted, pageSize, offset);
    }
    else
    {
      rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
      if (count == 4)
      {
        mcFile->pageNo = (rc == SQLITE_OK) ? sqlite3Get4byte(buffer) : 0;
      }
    }
  }
  else
  {
    rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
  }
  return rc;
}

static int mcWriteWal(sqlite3_file* pFile, const void* buffer, int count, sqlite3_int64 offset)
{
  int rc = SQLITE_OK;
  sqlite3mc_file* mcFile = (sqlite3mc_file*)pFile;
  Codec* codec = (mcFile->pMainDb) ? mcFile->pMainDb->codec : 0;

  if (codec != 0 && CodecIsEncrypted(codec))
  {
    const int pageSize = sqlite3mcGetPageSize(codec);

    if (count == pageSize)
    {
      int pageNo = 0;
      char ac[4];
      rc = REALFILE(pFile)->pMethods->xRead(REALFILE(pFile), ac, 4, offset - walFrameHeaderSize);
      if (rc == SQLITE_OK)
      {
        pageNo = sqlite3Get4byte(ac);
      }
      if (pageNo != 0)
      {
        void* bufferEncrypted = sqlite3mcCodec(codec, (char*)buffer, pageNo, 7);
        rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), bufferEncrypted, pageSize, offset);
      }
      else
      {
        rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
      }
    }
    else
    {
      rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
    }
  }
  else
  {
    rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
  }
  return rc;
}

static int mcIoWrite(sqlite3_file* pFile, const void* buffer, int count, sqlite3_int64 offset)
{
  int rc = SQLITE_OK;
  int doDefault = 1;
  sqlite3mc_file* mcFile = (sqlite3mc_file*) pFile;

  if (mcFile->openFlags & SQLITE_OPEN_MAIN_DB)
  {
    rc = mcWriteMainDb(pFile, buffer, count, offset);
  }
#if 0
  else if (mcFile->openFlags & SQLITE_OPEN_TEMP_DB)
  {
  }
#endif
#if 0
  else if (mcFile->openFlags & SQLITE_OPEN_TRANSIENT_DB)
  {
  }
#endif
  else if (mcFile->openFlags & SQLITE_OPEN_MAIN_JOURNAL)
  {
    rc = mcWriteMainJournal(pFile, buffer, count, offset);
  }
#if 0
  else if (mcFile->openFlags & SQLITE_OPEN_TEMP_JOURNAL)
  {
  }
#endif
  else if (mcFile->openFlags & SQLITE_OPEN_SUBJOURNAL)
  {
    rc = mcWriteSubJournal(pFile, buffer, count, offset);
}
#if 0
  else if (mcFile->openFlags & SQLITE_OPEN_MASTER_JOURNAL)
  {
  }
#endif
  else if (mcFile->openFlags & SQLITE_OPEN_WAL)
  {
    rc = mcWriteWal(pFile, buffer, count, offset);
  }
  else
  {
    rc = REALFILE(pFile)->pMethods->xWrite(REALFILE(pFile), buffer, count, offset);
  }
  return rc;
}

static int mcIoTruncate(sqlite3_file* pFile, sqlite3_int64 size)
{
  return REALFILE(pFile)->pMethods->xTruncate(REALFILE(pFile), size);
}

static int mcIoSync(sqlite3_file* pFile, int flags)
{
  return REALFILE(pFile)->pMethods->xSync(REALFILE(pFile), flags);
}

static int mcIoFileSize(sqlite3_file* pFile, sqlite3_int64* pSize)
{
  return REALFILE(pFile)->pMethods->xFileSize(REALFILE(pFile), pSize);
}

static int mcIoLock(sqlite3_file* pFile, int lock)
{
  return REALFILE(pFile)->pMethods->xLock(REALFILE(pFile), lock);
}

static int mcIoUnlock(sqlite3_file* pFile, int lock)
{
  return REALFILE(pFile)->pMethods->xUnlock(REALFILE(pFile), lock);
}

static int mcIoCheckReservedLock(sqlite3_file* pFile, int* pResOut)
{
  return REALFILE(pFile)->pMethods->xCheckReservedLock(REALFILE(pFile), pResOut);
}

static int mcIoFileControl(sqlite3_file* pFile, int op, void* pArg)
{
  int rc = SQLITE_OK;
  int doReal = 1;
  sqlite3mc_file* p = (sqlite3mc_file*) pFile;

  /*
  ** TODO: Handle pragmas
  */
  switch (op)
  {
    case SQLITE_FCNTL_PDB:
      {
#if 0
        /* pArg points to the sqlite3* handle for which the database file  was opened */
        /* In shared cache mode this function is invoked for every use of the database file in a connection */
        /* Unfortunately there is no notification, when a database file is no longer used by a connection (close in normal mode) */
        sqlite3* db = *((sqlite3**) pArg);
#endif
    }
      break;
    case SQLITE_FCNTL_PRAGMA:
      {
#if 0
      /* Handle database file specific pragmas */
        char* pragmaName = ((char**) pArg)[1];
        char* pragmaValue = ((char**) pArg)[2];
        if (sqlite3StrICmp(pragmaName, "...") == 0)
        {
          /* Action */
          /* ((char**) pArg)[0] = sqlite3_mprintf("error msg.");*/
          doReal = 0;
        }
#endif
      }
      break;
    default:
      break;
  }
  if (doReal)
  {
    rc = REALFILE(pFile)->pMethods->xFileControl(REALFILE(pFile), op, pArg);
  }
  return rc;
}

static int mcIoSectorSize(sqlite3_file* pFile)
{
  return REALFILE(pFile)->pMethods->xSectorSize(REALFILE(pFile));
}

static int mcIoDeviceCharacteristics(sqlite3_file* pFile)
{
  return REALFILE(pFile)->pMethods->xDeviceCharacteristics(REALFILE(pFile));
}

static int mcIoShmMap(sqlite3_file* pFile, int iPg, int pgsz, int map, void volatile** p)
{
  return REALFILE(pFile)->pMethods->xShmMap(REALFILE(pFile), iPg, pgsz, map, p);
}

static int mcIoShmLock(sqlite3_file* pFile, int offset, int n, int flags)
{
  return REALFILE(pFile)->pMethods->xShmLock(REALFILE(pFile), offset, n, flags);
}

static void mcIoShmBarrier(sqlite3_file* pFile)
{
  REALFILE(pFile)->pMethods->xShmBarrier(REALFILE(pFile));
}

static int mcIoShmUnmap(sqlite3_file* pFile, int deleteFlag)
{
  return REALFILE(pFile)->pMethods->xShmUnmap(REALFILE(pFile), deleteFlag);
}

static int mcIoFetch(sqlite3_file* pFile, sqlite3_int64 iOfst, int iAmt, void** pp)
{
  return REALFILE(pFile)->pMethods->xFetch(REALFILE(pFile), iOfst, iAmt, pp);
}

static int mcIoUnfetch( sqlite3_file* pFile, sqlite3_int64 iOfst, void* p)
{
  return REALFILE(pFile)->pMethods->xUnfetch(REALFILE(pFile), iOfst, p);
}

/*
** MultiCipher external API functions
*/

SQLITE_API const char* sqlite3mc_vfs_name()
{
  return SQLITE3MC_VFS_NAME;
}

/*
** Terminate and unregister the SQLite3 Multi Cipher VFS
*/
SQLITE_API void sqlite3mc_vfs_terminate()
{
  if (mcVfsGlobal.pMain == 0)
  {
    sqlite3_mutex_free(mcVfsGlobal.mutex);
    sqlite3_vfs_unregister(&mcVfsGlobal.base);
  }
}

/*
** Initialize SQLite3 Multi Cipher VFS that accesses the underlying file-system
** via the current default VFS.
*/
SQLITE_API int sqlite3mc_vfs_initialize(sqlite3_vfs* vfsDefault, int makeDefault)
{
  int rc = SQLITE_OK;
  
  if (!vfsDefault)
  {
    return SQLITE_NOTFOUND;
  }
 
  mcVfsGlobal.base.szOsFile = sizeof(sqlite3mc_file) + vfsDefault->szOsFile;
  mcVfsGlobal.base.mxPathname = vfsDefault->mxPathname;
  mcVfsGlobal.pVfs = vfsDefault;
  mcVfsGlobal.mutex = 0;
  mcVfsGlobal.pMain = 0;
  mcVfsGlobal.mutex = sqlite3_mutex_alloc(SQLITE_MUTEX_RECURSIVE);
  
  if (mcVfsGlobal.mutex == 0)
  {
    rc = SQLITE_NOMEM;
  }
  else
  {
    rc = sqlite3_vfs_register(&mcVfsGlobal.base, makeDefault);
    if (rc != SQLITE_OK)
    {
      sqlite3_mutex_free(mcVfsGlobal.mutex);
      mcVfsGlobal.mutex = 0;
    }
  }
  
  //fprintf(stdout,"sqlite3mc_vfs_initialize returned %d\n", rc); 
  
  return rc;
}

