/*
** Refactored from https://github.com/utelle/SQLite3MultipleCiphers
** Original MIT License - (c) 2006-2020 Ulrich Telle
*/

/*
** Include a "special" version of the VACUUM command
** (not needed with our fixed page-size algorithm)
*/
// #include "rekeyvacuum.c" 

/*
** Include "multi-cipher" algorithms
** (not included in our Synopse-specific encryption)
*/
// #include "cipher_common.h"

SQLITE_API void
sqlite3_activate_see(const char *info)
{
}

/*
** Our Synopse-specific algorithm don't append anything to the page size
*/
SQLITE_PRIVATE int
sqlite3mcGetPageSize(Codec* codec)
{
  return codec->m_btShared->pageSize;
} 

/*
** Free the encryption data structure associated with a pager instance.
** (called from the modified code in pager.c) 
*/
SQLITE_PRIVATE void
sqlite3mcCodecFree(void *pCodecArg)
{
  if (pCodecArg)
  {
    CodecTerm(pCodecArg);
    sqlite3_free(pCodecArg);
    pCodecArg = NULL;
  }
}

SQLITE_PRIVATE void
mcReportCodecError(BtShared* pBt, int error)
{
  pBt->pPager->errCode = error;
  setGetterMethod(pBt->pPager);
  pBt->db->errCode = error;
}

/*
// Encrypt/Decrypt functionality, called by pager.c
*/
SQLITE_PRIVATE void*
sqlite3mcCodec(void* pCodecArg, void* data, Pgno nPageNum, int nMode)
{
  int rc = SQLITE_OK;
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

  pageSize = sqlite3mcGetPageSize(codec);

  switch(nMode)
  {
    case 0: /* Undo a "case 7" journal file encryption */
    case 2: /* Reload a page */
    case 3: /* Load a page */
      if (CodecHasReadKey(codec))
      {
        rc = CodecDecrypt(codec, nPageNum, (unsigned char*) data, pageSize);
        if (rc != SQLITE_OK) mcReportCodecError(CodecGetBtShared(codec), rc);
      }
      break;

    case 6: /* Encrypt a page for the main database file */
      if (CodecHasWriteKey(codec))
      {
        unsigned char* pageBuffer = CodecGetPageBuffer(codec);
        memcpy(pageBuffer, data, pageSize);
        data = pageBuffer;
        rc = CodecEncrypt(codec, nPageNum, (unsigned char*) data, pageSize, 1);
        if (rc != SQLITE_OK) mcReportCodecError(CodecGetBtShared(codec), rc);
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
        rc = CodecEncrypt(codec, nPageNum, (unsigned char*) data, pageSize, 0);
        if (rc != SQLITE_OK) mcReportCodecError(CodecGetBtShared(codec), rc);
      }
      break;
  }
  return data;
}

/* some prototypes of functions implemented in sqlite3mc_vfs.c */

SQLITE_PRIVATE Codec*
sqlite3mcGetMainCodec(sqlite3* db);

SQLITE_PRIVATE Codec*
sqlite3mcGetCodec(sqlite3* db, const char* zDbName);

SQLITE_PRIVATE void
sqlite3mcSetCodec(sqlite3* db, const char* zFileName, Codec* codec);

SQLITE_PRIVATE int
sqlite3mcCodecAttach(sqlite3* db, int nDb, const char* zPath, const void* zKey, int nKey)
{
  /* Attach a key to a database. */
  Codec* codec = (Codec*) sqlite3_malloc(sizeof(Codec));
  int rc = (codec != NULL) ? CodecInit(codec) : SQLITE_NOMEM;
  if (rc != SQLITE_OK)
  {
    /* Unable to allocate memory for the codec base structure */
    return rc;
  }

  sqlite3_mutex_enter(db->mutex);
  CodecSetDb(codec, db);

  /* No key specified, could mean either use the main db's encryption or no encryption */
  if (zKey == NULL || nKey <= 0)
  {
    /* No key specified */
    if (nDb != 0 && nKey > 0)
    {
      /* Main database possibly encrypted, no key explicitly given for attached database */
      Codec* mainCodec = sqlite3mcGetMainCodec(db);
      /* Attached database, therefore use the key of main database, if main database is encrypted */
      if (mainCodec != NULL && CodecIsEncrypted(mainCodec))
      {
        rc = CodecCopyCipher(codec, mainCodec);
        if (rc == SQLITE_OK)
        {
          CodecSetBtree(codec, db->aDb[nDb].pBt);
          zPath = sqlite3_db_filename(db, db->aDb[nDb].zDbSName); 
          sqlite3mcSetCodec(db, zPath, codec);
        }
        else
        {
          /* Replicating main codec failed, do not attach incomplete codec */
          sqlite3mcCodecFree(codec);
        }
      }
      else
      {
        /* Main database not encrypted */
        sqlite3mcCodecFree(codec);
      }
    }
    else
    {
      /* Main database not encrypted, no key given for attached database */
      sqlite3mcCodecFree(codec);
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
    zPath = sqlite3_db_filename(db, db->aDb[nDb].zDbSName); 
	  sqlite3mcSetCodec(db, zPath, codec);
  }

  sqlite3_mutex_leave(db->mutex);

  return rc;
}

SQLITE_PRIVATE void
sqlite3mcCodecGetKey(sqlite3* db, int nDb, void** zKey, int* nKey)
{
  /*
  ** The unencrypted password is not stored for security reasons
  ** therefore always return NULL
  ** If the main database is encrypted a key length of 1 is returned.
  ** In that case an attached database will get the same encryption key
  ** as the main database if no key was explicitly given for the attached database.
  */
  Codec* codec = sqlite3mcGetCodec(db, db->aDb[nDb].zDbSName);
  int keylen = (codec != NULL && CodecIsEncrypted(codec)) ? 1 : 0;
  *zKey = NULL;
  *nKey = keylen;
}

SQLITE_API int
sqlite3_rekey_v2(sqlite3 *db, const char *zDbName, const void *zKey, int nKey)
{
  /* Changes the encryption key for an existing database. */
  int rc = SQLITE_ERROR;
  int nPagesize;
  Pager* pPager;
  Codec* codec;
  Btree* pBt;
  const char* dbFileName = sqlite3_db_filename(db, zDbName);
  int dbIndex = (zDbName) ? sqlite3FindDbName(db, zDbName) : 0;
  if (dbIndex < 0)
  {
    return rc;
  }
  
  pBt = db->aDb[dbIndex].pBt;
  nPagesize = sqlite3BtreeGetPageSize(pBt);
  
  if (zKey != NULL && nKey < 0)
  {
    /* Key is zero-terminated string */
    nKey = sqlite3Strlen30(zKey);
  }
  
  pPager = sqlite3BtreePager(pBt);
  codec = sqlite3mcGetCodec(db, zDbName);

  if ((zKey == NULL || nKey == 0) && (codec == NULL || !CodecIsEncrypted(codec)))
  {
    /* Database not encrypted and key not specified, therefore do nothing	*/
    return SQLITE_OK;
  }

  sqlite3_mutex_enter(db->mutex);

  if (codec == NULL || !CodecIsEncrypted(codec))
  {
    /* Database not encrypted, but key specified, therefore encrypt database	*/
    if (codec == NULL)
    {
      codec = (Codec*) sqlite3_malloc(sizeof(Codec));
      rc = (codec != NULL) ? CodecInit(codec) : SQLITE_NOMEM;
    }
    if (rc == SQLITE_OK)
    {
      CodecSetIsEncrypted(codec, 1);
      CodecSetHasReadKey(codec, 0); /* Original database is not encrypted */
      CodecSetHasWriteKey(codec, 1);
      CodecGenerateWriteKey(codec, (char*) zKey, nKey);
      CodecSetDb(codec, db);
      CodecSetBtree(codec, pBt);
  	  sqlite3mcSetCodec(db, dbFileName, codec);
    }
    else
    {
      return rc;
    }
  }
  else if (zKey == NULL || nKey == 0)
  {
    /* Database encrypted, but key not specified, therefore decrypt database */
    /* Keep read key, drop write key */
    CodecSetHasWriteKey(codec, 0);
  }
  else
  {
    /* Database encrypted and key specified, therefore re-encrypt database with new key */
    /* Keep read key, change write key to new key */
    CodecGenerateWriteKey(codec, (char*) zKey, nKey);
    CodecSetHasWriteKey(codec, 1);
  }

  /* Start transaction */
#if (SQLITE_VERSION_NUMBER >= 3025000)
  rc = sqlite3BtreeBeginTrans(pBt, 1, 0);
#else
  rc = sqlite3BtreeBeginTrans(pBt, 1);
#endif
  if (!rc)
  {
    int pageSize = sqlite3BtreeGetPageSize(pBt);
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
    rc = sqlite3BtreeCommit(pBt);
  }
  if (rc != SQLITE_OK)
  {
    /* Rollback in case of error */
#if (SQLITE_VERSION_NUMBER >= 3008007)
    /* Unfortunately this change was introduced in version 3.8.7.2 which cannot be detected using the SQLITE_VERSION_NUMBER */
    /* That is, compilation will fail for version 3.8.7 or 3.8.7.1  ==> Please change manually ... or upgrade to 3.8.7.2 or higher */
    sqlite3BtreeRollback(pBt, SQLITE_OK, 0);
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
    sqlite3mcSetCodec(db, zDbName, NULL);
  }
  return rc;
}

SQLITE_API int
sqlite3_key_v2(sqlite3 *db, const char *zDbName, const void *zKey, int nKey)
{
  int rc = SQLITE_ERROR;
  int dbIndex;
  if (zKey != NULL && nKey < 0)
  {
    /* Key is zero-terminated string */
    nKey = sqlite3Strlen30(zKey);
  }
  if ((db != NULL) && (zKey != NULL) && (nKey > 0))
  {
    const char* dbFileName = sqlite3_db_filename(db, zDbName);
    
    /* The key is only set for the main database, not the temp database  */
    dbIndex = (zDbName) ? sqlite3FindDbName(db, zDbName) : 0;
    if (dbIndex >= 0)
    {
      rc = sqlite3mcCodecAttach(db, dbIndex, dbFileName, zKey, nKey);
    }
    else
    {
      rc = SQLITE_ERROR;
    }
  }
  return rc;
}

SQLITE_API int
sqlite3_key(sqlite3 *db, const void *zKey, int nKey)
{
  /* The key is only set for the main database, not the temp database  */
  return sqlite3_key_v2(db, "main", zKey, nKey);
}

SQLITE_API int
sqlite3_rekey(sqlite3 *db, const void *zKey, int nKey)
{
  return sqlite3_rekey_v2(db, "main", zKey, nKey);
}

/*
** Functions called from patched SQLite version
*/

SQLITE_PRIVATE int
sqlite3mcFileControlPragma(sqlite3* db, const char* zDbName, int op, void* pArg)
{
  int rc = sqlite3_file_control(db, zDbName, op, pArg);
  /* not implemented: handle cipher pragmas */
  return rc;
}

/*
** Process URI filename query parameters relevant to the SQLite Encryption
** Extension.  Return true if any of the relevant query parameters are
** seen and return false if not.
*/
SQLITE_PRIVATE int
sqlite3mcCodecQueryParameters(sqlite3* db, const char* zDb, const char* zUri)
{
  int rc = 1;
  const char* zKey;
  if ((zKey = sqlite3_uri_parameter(zUri, "hexkey")) != 0 && zKey[0])
  {
    u8 iByte;
    int i;
    char zDecoded[40];
    for (i = 0, iByte = 0; i < sizeof(zDecoded) * 2 && sqlite3Isxdigit(zKey[i]); i++)
    {
      iByte = (iByte << 4) + sqlite3HexToInt(zKey[i]);
      if ((i & 1) != 0) zDecoded[i / 2] = iByte;
    }
    sqlite3_key_v2(db, zDb, zDecoded, i / 2);
  }
  else if ((zKey = sqlite3_uri_parameter(zUri, "key")) != 0)
  {
    sqlite3_key_v2(db, zDb, zKey, sqlite3Strlen30(zKey));
  }
  else if ((zKey = sqlite3_uri_parameter(zUri, "textkey")) != 0)
  {
    sqlite3_key_v2(db, zDb, zKey, -1);
  }
  else
  {
    rc = 0;
  }
  return rc;
}

SQLITE_PRIVATE int
sqlite3mcHandleAttachKey(sqlite3* db, const char* zName, const char* zPath, sqlite3_value* pKey, char** zErrDyn)
{
  int rc = SQLITE_OK;
  int nKey;
  char* zKey;
  int keyType = sqlite3_value_type(pKey);
  switch (keyType)
  {
    case SQLITE_INTEGER:
    case SQLITE_FLOAT:
      /* Invalid data type for key */
      *zErrDyn = sqlite3DbStrDup(db, "Invalid key value");
      rc = SQLITE_ERROR;
      break;

    case SQLITE_TEXT:
    case SQLITE_BLOB:
      /* Key parameter specified in ATTACH statement */
      nKey = sqlite3_value_bytes(pKey);
      zKey = (char*) sqlite3_value_blob(pKey);
      rc = sqlite3mcCodecAttach(db, db->nDb - 1, zPath, zKey, nKey);
      break;

    case SQLITE_NULL:
      /* No key specified.  Use the key from URI filename, or if none,
      ** use the key from the main database. */
            
      if (sqlite3mcCodecQueryParameters(db, zName, zPath) == 0)
      {
        /* this is buggy and leaks memory - will use the key from main DB anyway
          see https://github.com/utelle/SQLite3MultipleCiphers/issues/10
          
        sqlite3mcCodecGetKey(db, 0, (void**) &zKey, &nKey);
        if (nKey)
        { 
          rc = sqlite3mcCodecAttach(db, db->nDb - 1, zPath, zKey, nKey);
        }
        */      
      } 
      break;
  }

  return rc;
}

SQLITE_PRIVATE int
sqlite3mcHandleMainKey(sqlite3* db, const char* zPath)
{
  int rc = SQLITE_OK; 
  /* Not implemented: implement URI cipher parameters */
  return rc;
} 


