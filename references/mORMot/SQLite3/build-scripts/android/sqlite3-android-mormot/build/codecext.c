/*
** Implements SQLITE_HAS_CODEC for SQlite3 amalgamation file
**
** adapted from https://github.com/utelle/wxsqlite3 patches
** wxWindows Library Licence, Version 3.1
*/

#ifdef SQLITE_HAS_CODEC

void sqlite3_activate_see(const char *info)
{
}

/*
// Free the encryption data structure associated with a pager instance.
// (called from the modified code in pager.c) 
*/
void sqlite3CodecFree(void *pCodecArg)
{
  if (pCodecArg)
  {
    CodecTerm(pCodecArg);
    sqlite3_free(pCodecArg);
    pCodecArg = NULL;
  }
}

void sqlite3CodecSizeChange(void *pArg, int pageSize, int reservedSize)
{
}

/*
// Encrypt/Decrypt functionality, called by pager.c
*/
void* sqlite3Codec(void* pCodecArg, void* data, Pgno nPageNum, int nMode)
{
  Codec* codec = NULL;
  int pageSize;
  if (pCodecArg == NULL)
  {
    return data;
  }
  codec = (Codec*) pCodecArg;
  if (!CodecIsEncrypted(codec))
  {
    return data;
  }
  
  pageSize = sqlite3BtreeGetPageSize(CodecGetBtree(codec));

  switch(nMode)
  {
    case 0: /* Undo a "case 7" journal file encryption */
    case 2: /* Reload a page */
    case 3: /* Load a page */
      if (CodecHasReadKey(codec))
      {
        CodecDecrypt(codec, nPageNum, (unsigned char*) data, pageSize);
      }
      break;

    case 6: /* Encrypt a page for the main database file */
      if (CodecHasWriteKey(codec))
      {
        unsigned char* pageBuffer = CodecGetPageBuffer(codec);
        memcpy(pageBuffer, data, pageSize);
        data = pageBuffer;
        CodecEncrypt(codec, nPageNum, (unsigned char*) data, pageSize, 1);
      }
      break;

    case 7: /* Encrypt a page for the journal file */
      /* Under normal circumstances, the readkey is the same as the writekey.  However,
         when the database is being rekeyed, the readkey is not the same as the writekey.
         The rollback journal must be written using the original key for the
         database file because it is, by nature, a rollback journal.
         Therefore, for case 7, when the rollback is being written, always encrypt using
         the database's readkey, which is guaranteed to be the same key that was used to
         read the original data.
      */
      if (CodecHasReadKey(codec))
      {
        unsigned char* pageBuffer = CodecGetPageBuffer(codec);
        memcpy(pageBuffer, data, pageSize);
        data = pageBuffer;
        CodecEncrypt(codec, nPageNum, (unsigned char*) data, pageSize, 0);
      }
      break;
  }
  return data;
}

void* mySqlite3PagerGetCodec(
  Pager *pPager
);

void mySqlite3PagerSetCodec(
  Pager *pPager,
  void *(*xCodec)(void*,void*,Pgno,int),
  void (*xCodecSizeChng)(void*,int,int),
  void (*xCodecFree)(void*),
  void *pCodec
);

int sqlite3CodecAttach(sqlite3* db, int nDb, const void* zKey, int nKey)
{
  /* Attach a key to a database. */
  Codec* codec = (Codec*) sqlite3_malloc(sizeof(Codec));
  CodecInit(codec);

  sqlite3_mutex_enter(db->mutex);

  /* No key specified, could mean either use the main db's encryption or no encryption */
  if (zKey == NULL || nKey <= 0)
  {
    /* No key specified */
    if (nDb != 0 && nKey > 0)
    {
      Codec* mainCodec = (Codec*) mySqlite3PagerGetCodec(sqlite3BtreePager(db->aDb[0].pBt));
      /* Attached database, therefore use the key of main database, if main database is encrypted */
      if (mainCodec != NULL && CodecIsEncrypted(mainCodec))
      {
        CodecCopy(codec, mainCodec);
        CodecSetBtree(codec, db->aDb[nDb].pBt);
#if (SQLITE_VERSION_NUMBER >= 3006016)
        mySqlite3PagerSetCodec(sqlite3BtreePager(db->aDb[nDb].pBt), sqlite3Codec, sqlite3CodecSizeChange, sqlite3CodecFree, codec);
#else
#if (SQLITE_VERSION_NUMBER >= 3003014)
        sqlite3PagerSetCodec(sqlite3BtreePager(db->aDb[nDb].pBt), sqlite3Codec, codec);
#else
        sqlite3pager_set_codec(sqlite3BtreePager(db->aDb[nDb].pBt), sqlite3Codec, codec);
#endif
        db->aDb[nDb].pAux = codec;
        db->aDb[nDb].xFreeAux = sqlite3CodecFree;
#endif
      }
      else
      {
        CodecSetIsEncrypted(codec, 0);
        sqlite3CodecFree(codec);
      }
    }
    else
    {
      CodecSetIsEncrypted(codec, 0);
      sqlite3CodecFree(codec);
    }
  }
  else
  {
    /* Key specified, setup encryption key for database */
    CodecSetIsEncrypted(codec, 1);
    CodecSetHasReadKey(codec, 1);
    CodecSetHasWriteKey(codec, 1);
    CodecGenerateReadKey(codec, (char*) zKey, nKey);
    CodecCopyKey(codec, 1);
    CodecSetBtree(codec, db->aDb[nDb].pBt);
#if (SQLITE_VERSION_NUMBER >= 3006016)
    mySqlite3PagerSetCodec(sqlite3BtreePager(db->aDb[nDb].pBt), sqlite3Codec, sqlite3CodecSizeChange, sqlite3CodecFree, codec);
#else
#if (SQLITE_VERSION_NUMBER >= 3003014)
    sqlite3PagerSetCodec(sqlite3BtreePager(db->aDb[nDb].pBt), sqlite3Codec, codec);
#else
    sqlite3pager_set_codec(sqlite3BtreePager(db->aDb[nDb].pBt), sqlite3Codec, codec);
#endif
    db->aDb[nDb].pAux = codec;
    db->aDb[nDb].xFreeAux = sqlite3CodecFree;
#endif
  }

  sqlite3_mutex_leave(db->mutex);

  return SQLITE_OK;
}

void sqlite3CodecGetKey(sqlite3* db, int nDb, void** zKey, int* nKey)
{
  /*
  // The unencrypted password is not stored for security reasons
  // therefore always return NULL
  // If the main database is encrypted a key length of 1 is returned.
  // In that case an attached database will get the same encryption key
  // as the main database if no key was explicitly given for the attached database.
  */
  Codec* mainCodec = (Codec*) mySqlite3PagerGetCodec(sqlite3BtreePager(db->aDb[0].pBt));
  int keylen = (mainCodec != NULL && CodecIsEncrypted(mainCodec)) ? 1 : 0;
  *zKey = NULL;
  *nKey = keylen;
}

static int dbFindIndex(sqlite3* db, const char* zDb)
{
  int dbIndex = 0;
  if (zDb != NULL)
  {
    int found = 0;
    int index;
    for (index = 0; found == 0 && index < db->nDb; ++index)
    {
      struct Db* pDb = &db->aDb[index];
#if (SQLITE_VERSION_NUMBER >= 3015000)
      if (strcmp(pDb->zDbSName, zDb) == 0)
#else
      if (strcmp(pDb->zName, zDb) == 0)
#endif
      {
        found = 1;
        dbIndex = index;
      }
    }
    if (found == 0) dbIndex = 0;
  }
  return dbIndex;
}

int sqlite3_key(sqlite3 *db, const void *zKey, int nKey)
{
  /* The key is only set for the main database, not the temp database  */
  return sqlite3_key_v2(db, "main", zKey, nKey);
}

int sqlite3_key_v2(sqlite3 *db, const char *zDbName, const void *zKey, int nKey)
{
  /* The key is only set for the main database, not the temp database  */
  int dbIndex = dbFindIndex(db, zDbName);
  return sqlite3CodecAttach(db, dbIndex, zKey, nKey);
}

int sqlite3_rekey_v2(sqlite3 *db, const char *zDbName, const void *zKey, int nKey)
{
  /* Changes the encryption key for an existing database. */
  int dbIndex = dbFindIndex(db, zDbName);
  int rc = SQLITE_ERROR;
  Btree* pbt = db->aDb[dbIndex].pBt;
  Pager* pPager = sqlite3BtreePager(pbt);
  Codec* codec = (Codec*) mySqlite3PagerGetCodec(pPager);

  if ((zKey == NULL || nKey == 0) && (codec == NULL || !CodecIsEncrypted(codec)))
  {
    /*
    // Database not encrypted and key not specified
    // therefore do nothing
	*/
    return SQLITE_OK;
  }

  if (codec == NULL || !CodecIsEncrypted(codec))
  {
    /*
    // Database not encrypted, but key specified
    // therefore encrypt database
	*/
    if (codec == NULL)
    {
      codec = (Codec*) sqlite3_malloc(sizeof(Codec));
	    CodecInit(codec);
    }

    CodecSetIsEncrypted(codec, 1);
    CodecSetHasReadKey(codec, 0); /* Original database is not encrypted */
    CodecSetHasWriteKey(codec, 1);
    CodecGenerateWriteKey(codec, (char*) zKey, nKey);
    CodecSetBtree(codec, pbt);
#if (SQLITE_VERSION_NUMBER >= 3006016)
    mySqlite3PagerSetCodec(pPager, sqlite3Codec, sqlite3CodecSizeChange, sqlite3CodecFree, codec);
#else
#if (SQLITE_VERSION_NUMBER >= 3003014)
    sqlite3PagerSetCodec(pPager, sqlite3Codec, codec);
#else
    sqlite3pager_set_codec(pPager, sqlite3Codec, codec);
#endif
    db->aDb[dbIndex].pAux = codec;
    db->aDb[dbIndex].xFreeAux = sqlite3CodecFree;
#endif
  }
  else if (zKey == NULL || nKey == 0)
  {
    /*
    // Database encrypted, but key not specified
    // therefore decrypt database
    // Keep read key, drop write key
	*/
    CodecSetHasWriteKey(codec, 0);
  }
  else
  {
    /*
    // Database encrypted and key specified
    // therefore re-encrypt database with new key
    // Keep read key, change write key to new key
	*/
    CodecGenerateWriteKey(codec, (char*) zKey, nKey);
    CodecSetHasWriteKey(codec, 1);
  }

  sqlite3_mutex_enter(db->mutex);

  /* Start transaction */
  rc = sqlite3BtreeBeginTrans(pbt, 1, NULL);
  if (!rc)
  {
    int pageSize = sqlite3BtreeGetPageSize(pbt);
    Pgno nSkip = WX_PAGER_MJ_PGNO(pageSize);
#if (SQLITE_VERSION_NUMBER >= 3003014)
    DbPage *pPage;
#else
    void *pPage;
#endif
    Pgno n;
    /* Rewrite all pages using the new encryption key (if specified) */
#if (SQLITE_VERSION_NUMBER >= 3007001)
    Pgno nPage;
    int nPageCount = -1;
    sqlite3PagerPagecount(pPager, &nPageCount);
    nPage = nPageCount;
#elif (SQLITE_VERSION_NUMBER >= 3006000)
    int nPageCount = -1;
    int rc = sqlite3PagerPagecount(pPager, &nPageCount);
    Pgno nPage = (Pgno) nPageCount;
#elif (SQLITE_VERSION_NUMBER >= 3003014)
    Pgno nPage = sqlite3PagerPagecount(pPager);
#else
    Pgno nPage = sqlite3pager_pagecount(pPager);
#endif

    for (n = 1; rc == SQLITE_OK && n <= nPage; n++)
    {
      if (n == nSkip) continue;
#if (SQLITE_VERSION_NUMBER >= 3010000)
      rc = sqlite3PagerGet(pPager, n, &pPage, 0);
#elif (SQLITE_VERSION_NUMBER >= 3003014)
      rc = sqlite3PagerGet(pPager, n, &pPage);
#else
      rc = sqlite3pager_get(pPager, n, &pPage);
#endif
      if (!rc)
      {
#if (SQLITE_VERSION_NUMBER >= 3003014)
        rc = sqlite3PagerWrite(pPage);
        sqlite3PagerUnref(pPage);
#else
        rc = sqlite3pager_write(pPage);
        sqlite3pager_unref(pPage);
#endif
      }
    }
  }

  if (rc == SQLITE_OK)
  {
    /* Commit transaction if all pages could be rewritten */
    rc = sqlite3BtreeCommit(pbt);
  }
  if (rc != SQLITE_OK)
  {
    /* Rollback in case of error */
#if (SQLITE_VERSION_NUMBER >= 3008007)
    /* Unfortunately this change was introduced in version 3.8.7.2 which cannot be detected using the SQLITE_VERSION_NUMBER */
    /* That is, compilation will fail for version 3.8.7 or 3.8.7.1  ==> Please change manually ... or upgrade to 3.8.7.2 or higher */
    sqlite3BtreeRollback(pbt, SQLITE_OK, 0);
#elif (SQLITE_VERSION_NUMBER >= 3007011)
    sqlite3BtreeRollback(pbt, SQLITE_OK);
#else
    sqlite3BtreeRollback(pbt);
#endif
  }

  sqlite3_mutex_leave(db->mutex);

  if (rc == SQLITE_OK)
  {
    /* Set read key equal to write key if necessary */
    if (CodecHasWriteKey(codec))
    {
      CodecCopyKey(codec, 0);
      CodecSetHasReadKey(codec, 1);
    }
    else
    {
      CodecSetIsEncrypted(codec, 0);
    }
  }
  else
  {
    /* Restore write key if necessary */
    if (CodecHasReadKey(codec))
    {
      CodecCopyKey(codec, 1);
    }
    else
    {
      CodecSetIsEncrypted(codec, 0);
    }
  }

  if (!CodecIsEncrypted(codec))
  {
    /* Remove codec for unencrypted database */
#if (SQLITE_VERSION_NUMBER >= 3006016)
    mySqlite3PagerSetCodec(pPager, NULL, NULL, NULL, NULL);
#else
#if (SQLITE_VERSION_NUMBER >= 3003014)
    sqlite3PagerSetCodec(pPager, NULL, NULL);
#else
    sqlite3pager_set_codec(pPager, NULL, NULL);
#endif
    db->aDb[dbIndex].pAux = NULL;
    db->aDb[dbIndex].xFreeAux = NULL;
    sqlite3CodecFree(codec);
#endif
  }
  return rc;
}

int sqlite3_rekey(sqlite3 *db, const void *zKey, int nKey)
{
  return sqlite3_rekey_v2(db, "main", zKey, nKey);
}

#endif /* SQLITE_HAS_CODEC */
