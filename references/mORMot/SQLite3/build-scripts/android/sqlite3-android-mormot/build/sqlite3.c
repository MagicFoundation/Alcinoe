/*
** Wrapper around SQlite3 amalgamation file with proper options and code
**
** Please download and put sqlite3.c in amalgamation/ sub-folder
** from https://sqlite.org/download.html
*/

/*
** Define all symbols expected by SynSQLite3Static.pas
**
** See also https://www.sqlite.org/compile.html#recommended_compile_time_options
*/

#define SQLITE_DEFAULT_MEMSTATUS 0
// don't need any debug here, and don't even define sqlite3_status()
#define SQLITE_THREADSAFE 1
// assuming multi-thread safety is made by caller - in our framework, there is
// only one thread using the database connection at the same time, but there could
// be multiple database connection at the same time (previous was 0 could be unsafe)
// - this option is also needed by codecext.c
#define SQLITE_OMIT_SHARED_CACHE 1
// no need of shared cache in a threadsafe calling model
#define SQLITE_OMIT_AUTOINIT 1
//  sqlite3_initialize() is done in unit initialization -> no AUTOINIT
#define SQLITE_OMIT_DEPRECATED 1
//  spare some code size
#define SQLITE_LIKE_DOESNT_MATCH_BLOBS 1
// historical function, never used
#define SQLITE_ENABLE_FTS3 1
#define SQLITE_ENABLE_FTS3_PARENTHESIS 1
#define SQLITE_ENABLE_FTS4 1
#define SQLITE_ENABLE_FTS5 1
// enable all FTS engines
#define SQLITE_ENABLE_RBU 1
// "Resumable Bulk Update" (or OTA) is not used/published yet
#define SQLITE_ENABLE_JSON1 1
// add JSON extension
#define SQLITE_MAX_EXPR_DEPTH 0
// no SQL depth limit, since we trust the input and expect the best performance
#define SQLITE_OMIT_LOAD_EXTENSION 1
// we don't need/allow extension in an embedded engine
#define SQLITE_OMIT_COMPILEOPTION_DIAGS 1
// we don't need Compilation Options Diagnostics in our embedded engine
#define SQLITE_OMIT_PROGRESS_CALLBACK 1
// we don't need sqlite3_progress_handler() API function
#define SQLITE_ENABLE_RTREE 1
// the RTREE extension is now (from v.1.8/3.7) compiled into the engine
#define SQLITE_ENABLE_DESERIALIZE
// enables  sqlite3_serialize() and sqlite3_deserialize()
#define SQLITE_HAS_CODEC 1
// see code below

/*
** Compile the official SQLite3 amalgamation file
*/

#if defined(__BORLANDC__)
#define __STDC__ 1
#endif

#if defined(__BORLANDC__)
#undef __STDC__
#endif

#include "amalgamation/sqlite3.c"
// to be downloaded from https://sqlite.org/download.html


#ifdef SQLITE_HAS_CODEC

/*
** handle SQLITE_HAS_CODEC
** adapted from https://github.com/utelle/wxsqlite3 patches
** wxWindows Library Licence, Version 3.1
*/

/*
** Get the codec argument for this pager on newer library versions
*/

void* mySqlite3PagerGetCodec(
  Pager *pPager
){
#if (SQLITE_VERSION_NUMBER >= 3006016)
  return sqlite3PagerGetCodec(pPager);
#else
  return (pPager->xCodec) ? pPager->pCodecArg : NULL;
#endif
}

/*
** Set the codec argument for this pager on newer library versions
*/

void mySqlite3PagerSetCodec(
  Pager *pPager,
  void *(*xCodec)(void*,void*,Pgno,int),
  void (*xCodecSizeChng)(void*,int,int),
  void (*xCodecFree)(void*),
  void *pCodec
){
  sqlite3PagerSetCodec(pPager, xCodec, xCodecSizeChng, xCodecFree, pCodec);
}

/*
** Define the Codec types as needed by codecext.c
**
*/

#define WX_PAGER_MJ_PGNO(x) ((PENDING_BYTE/(x))+1)
// ATTENTION: Macro similar to that in pager.c

#define KEYLENGTH 304
// match maximum possible AESContextSize, with 8 bytes alignment

// We embed two SynCrypto's TAES objects in the Codec struct
typedef struct _Codec
{
  int           m_isEncrypted;
  int           m_hasReadKey;
  unsigned char m_readKey[KEYLENGTH];
  int           m_hasWriteKey;
  unsigned char m_writeKey[KEYLENGTH];
  Btree*        m_bt; /* Pointer to B-tree used by DB */
  unsigned char m_page[SQLITE_MAX_PAGE_SIZE+24];
} Codec;

static void CodecInit(Codec* codec)
{
  codec->m_isEncrypted = 0;
  codec->m_hasReadKey  = 0;
  codec->m_hasWriteKey = 0;
}

static void CodecCopy(Codec* codec, Codec* other)
{
  codec->m_isEncrypted = other->m_isEncrypted;
  codec->m_hasReadKey  = other->m_hasReadKey;
  codec->m_hasWriteKey = other->m_hasWriteKey;
  memcpy(&codec->m_readKey, &other->m_readKey, KEYLENGTH);
  memcpy(&codec->m_writeKey, &other->m_writeKey, KEYLENGTH);
  codec->m_bt = other->m_bt;
}

// implemented in pascal using SynCrypto optimized AES functions
extern void CodecGenerateReadKey(Codec* codec, char* userPassword, int passwordLength);
extern void CodecGenerateWriteKey(Codec* codec, char* userPassword, int passwordLength);
extern void CodecEncrypt(Codec* codec, int page, unsigned char* data, int len, int useWriteKey);
extern void CodecDecrypt(Codec* codec, int page, unsigned char* data, int len);
extern void CodecTerm(Codec* codec);

unsigned char* CodecGetReadKey(Codec* codec)
{
  return codec->m_readKey;
}

unsigned char* CodecGetWriteKey(Codec* codec)
{
  return codec->m_writeKey;
}

static void CodecCopyKey(Codec* codec, int read2write)
{
  if (read2write)
  {
    memcpy(&codec->m_writeKey, &codec->m_readKey, KEYLENGTH);
  }
  else
  {
    memcpy(&codec->m_readKey, &codec->m_writeKey, KEYLENGTH);
  }
}

static void CodecSetIsEncrypted(Codec* codec, int isEncrypted)
{
  codec->m_isEncrypted = isEncrypted;
}

static void CodecSetHasReadKey(Codec* codec, int hasReadKey)
{
  codec->m_hasReadKey = hasReadKey;
}

static void CodecSetHasWriteKey(Codec* codec, int hasWriteKey)
{
  codec->m_hasWriteKey = hasWriteKey;
}

static void CodecSetBtree(Codec* codec, Btree* bt)
{
  codec->m_bt = bt;
}

static int CodecIsEncrypted(Codec* codec)
{
  return codec->m_isEncrypted;
}

static int CodecHasReadKey(Codec* codec)
{
  return codec->m_hasReadKey;
}

static int CodecHasWriteKey(Codec* codec)
{
  return codec->m_hasWriteKey;
}

static Btree* CodecGetBtree(Codec* codec)
{
  return codec->m_bt;
}

static unsigned char* CodecGetPageBuffer(Codec* codec)
{
  return &codec->m_page[4];
}

#include "codecext.c"
// from https://github.com/utelle/wxsqlite3/blob/master/sqlite3/secure/src/codecext.c

#endif
