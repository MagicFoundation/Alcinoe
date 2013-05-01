{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Yury Plashenkov (yury@plashenkov.com)
              based on http://code.google.com/p/sqlite3delphi/
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALSQLite3Wrapper
Version:      4.00

Description:  SQLite3.dll API Interface Unit

              SQLite is a software library that implements a self-contained,
              serverless, zero-configuration, transactional SQL database
              engine. The source code for SQLite is in the public domain and
              is thus free for use for any purpose, commercial or private.
              SQLite is the most widely deployed SQL database engine
              in the world.

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     26/06/2012: Add xe2 support

Link :        http://www.sqlite.org/
              http://code.google.com/p/sqlite3delphi/
              svn checkout http://sqlite3delphi.googlecode.com/svn/trunk/

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALSqlite3Wrapper;

interface

uses Windows;

const
  cALSqlite3_INVALIDELIBVERSION   = 'Incorrect Sqlite3.dll version';
  cALSqlite3_CANTLOADLIB          = 'Can''t load library: %s.';

type
  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = array[0..MaxInt div SizeOf(PAnsiChar) - 1] of PAnsiChar;

type
  PSQLite3 = type Pointer;

type
  TSQLite3Callback = function(pArg: Pointer; nCol: Integer; argv: PPAnsiCharArray; colv: PPAnsiCharArray): Integer; cdecl;

const
  SQLITE_OK         = 0;
  SQLITE_ERROR      = 1;
  SQLITE_INTERNAL   = 2;
  SQLITE_PERM       = 3;
  SQLITE_ABORT      = 4;
  SQLITE_BUSY       = 5;
  SQLITE_LOCKED     = 6;
  SQLITE_NOMEM      = 7;
  SQLITE_READONLY   = 8;
  SQLITE_INTERRUPT  = 9;
  SQLITE_IOERR      = 10;
  SQLITE_CORRUPT    = 11;
  SQLITE_NOTFOUND   = 12;
  SQLITE_FULL       = 13;
  SQLITE_CANTOPEN   = 14;
  SQLITE_PROTOCOL   = 15;
  SQLITE_EMPTY      = 16;
  SQLITE_SCHEMA     = 17;
  SQLITE_TOOBIG     = 18;
  SQLITE_CONSTRAINT = 19;
  SQLITE_MISMATCH   = 20;
  SQLITE_MISUSE     = 21;
  SQLITE_NOLFS      = 22;
  SQLITE_AUTH       = 23;
  SQLITE_FORMAT     = 24;
  SQLITE_RANGE      = 25;
  SQLITE_NOTADB     = 26;
  SQLITE_ROW        = 100;
  SQLITE_DONE       = 101;

const
  SQLITE_IOERR_READ              = SQLITE_IOERR or (1 shl 8);
  SQLITE_IOERR_SHORT_READ        = SQLITE_IOERR or (2 shl 8);
  SQLITE_IOERR_WRITE             = SQLITE_IOERR or (3 shl 8);
  SQLITE_IOERR_FSYNC             = SQLITE_IOERR or (4 shl 8);
  SQLITE_IOERR_DIR_FSYNC         = SQLITE_IOERR or (5 shl 8);
  SQLITE_IOERR_TRUNCATE          = SQLITE_IOERR or (6 shl 8);
  SQLITE_IOERR_FSTAT             = SQLITE_IOERR or (7 shl 8);
  SQLITE_IOERR_UNLOCK            = SQLITE_IOERR or (8 shl 8);
  SQLITE_IOERR_RDLOCK            = SQLITE_IOERR or (9 shl 8);
  SQLITE_IOERR_DELETE            = SQLITE_IOERR or (10 shl 8);
  SQLITE_IOERR_BLOCKED           = SQLITE_IOERR or (11 shl 8);
  SQLITE_IOERR_NOMEM             = SQLITE_IOERR or (12 shl 8);
  SQLITE_IOERR_ACCESS            = SQLITE_IOERR or (13 shl 8);
  SQLITE_IOERR_CHECKRESERVEDLOCK = SQLITE_IOERR or (14 shl 8);
  SQLITE_IOERR_LOCK              = SQLITE_IOERR or (15 shl 8);
  SQLITE_IOERR_CLOSE             = SQLITE_IOERR or (16 shl 8);
  SQLITE_IOERR_DIR_CLOSE         = SQLITE_IOERR or (17 shl 8);
  SQLITE_LOCKED_SHAREDCACHE      = SQLITE_LOCKED or (1 shl 8);

const
  SQLITE_OPEN_READONLY       = $00000001;
  SQLITE_OPEN_READWRITE      = $00000002;
  SQLITE_OPEN_CREATE         = $00000004;
  SQLITE_OPEN_DELETEONCLOSE  = $00000008;
  SQLITE_OPEN_EXCLUSIVE      = $00000010;
  SQLITE_OPEN_MAIN_DB        = $00000100;
  SQLITE_OPEN_TEMP_DB        = $00000200;
  SQLITE_OPEN_TRANSIENT_DB   = $00000400;
  SQLITE_OPEN_MAIN_JOURNAL   = $00000800;
  SQLITE_OPEN_TEMP_JOURNAL   = $00001000;
  SQLITE_OPEN_SUBJOURNAL     = $00002000;
  SQLITE_OPEN_MASTER_JOURNAL = $00004000;
  SQLITE_OPEN_NOMUTEX        = $00008000;
  SQLITE_OPEN_FULLMUTEX      = $00010000;
  SQLITE_OPEN_SHAREDCACHE    = $00020000;
  SQLITE_OPEN_PRIVATECACHE   = $00040000;

const
  SQLITE_IOCAP_ATOMIC      = $00000001;
  SQLITE_IOCAP_ATOMIC512   = $00000002;
  SQLITE_IOCAP_ATOMIC1K    = $00000004;
  SQLITE_IOCAP_ATOMIC2K    = $00000008;
  SQLITE_IOCAP_ATOMIC4K    = $00000010;
  SQLITE_IOCAP_ATOMIC8K    = $00000020;
  SQLITE_IOCAP_ATOMIC16K   = $00000040;
  SQLITE_IOCAP_ATOMIC32K   = $00000080;
  SQLITE_IOCAP_ATOMIC64K   = $00000100;
  SQLITE_IOCAP_SAFE_APPEND = $00000200;
  SQLITE_IOCAP_SEQUENTIAL  = $00000400;

const
  SQLITE_LOCK_NONE      = 0;
  SQLITE_LOCK_SHARED    = 1;
  SQLITE_LOCK_RESERVED  = 2;
  SQLITE_LOCK_PENDING   = 3;
  SQLITE_LOCK_EXCLUSIVE = 4;

const
  SQLITE_SYNC_NORMAL   = $00002;
  SQLITE_SYNC_FULL     = $00003;
  SQLITE_SYNC_DATAONLY = $00010;

type
  PSQLite3File = ^TSQLite3File;
  PSQLite3IOMethods = ^TSQLite3IOMethods;

  sqlite3_file = record
    pMethods: PSQLite3IOMethods;
  end;
  TSQLite3File = sqlite3_file;

  sqlite3_io_methods = record
    iVersion: Integer;
    xClose: function(id: PSQLite3File): Integer; cdecl;
    xRead: function(id: PSQLite3File; pBuf: Pointer; iAmt: Integer; iOfst: Int64): Integer; cdecl;
    xWrite: function(id: PSQLite3File; const pBuf: Pointer; iAmt: Integer; iOfst: Int64): Integer; cdecl;
    xTruncate: function(id: PSQLite3File; size: Int64): Integer; cdecl;
    xSync: function(id: PSQLite3File; flags: Integer): Integer; cdecl;
    xFileSize: function(id: PSQLite3File; var pSize: Int64): Integer; cdecl;
    xLock: function(id: PSQLite3File; locktype: Integer): Integer; cdecl;
    xUnlock: function(id: PSQLite3File; locktype: Integer): Integer; cdecl;
    xCheckReservedLock: function(f: PSQLite3File; var pResOut: Integer): Integer; cdecl;
    xFileControl: function(id: PSQLite3File; op: Integer; pArg: Pointer): Integer; cdecl;
    xSectorSize: function(id: PSQLite3File): Integer; cdecl;
    xDeviceCharacteristics: function(id: PSQLite3File): Integer; cdecl;
  end;
  TSQLite3IOMethods = sqlite3_io_methods;

const
  SQLITE_FCNTL_LOCKSTATE   = 1;
  SQLITE_GET_LOCKPROXYFILE = 2;
  SQLITE_SET_LOCKPROXYFILE = 3;
  SQLITE_LAST_ERRNO        = 4;

type
  PSQLite3Mutex = type Pointer;

type
  PSQLite3VFS = ^TSQLite3VFS;
  sqlite3_vfs = record
    iVersion: Integer;
    szOsFile: Integer;
    mxPathname: Integer;
    pNext: PSQLite3VFS;
    zName: PAnsiChar;
    pAppData: Pointer;
    xOpen: function(pVfs: PSQLite3VFS; const zName: PAnsiChar; id: PSQLite3File; flags: Integer; pOutFlags: PInteger): Integer; cdecl;
    xDelete: function(pVfs: PSQLite3VFS; const zName: PAnsiChar; syncDir: Integer): Integer; cdecl;
    xAccess: function(pVfs: PSQLite3VFS; const zName: PAnsiChar; flags: Integer; var pResOut: Integer): Integer; cdecl;
    xFullPathname: function(pVfs: PSQLite3VFS; const zName: PAnsiChar; nOut: Integer; zOut: PAnsiChar): Integer; cdecl;
    xDlOpen: function(pVfs: PSQLite3VFS; const zFilename: PAnsiChar): Pointer; cdecl;
    xDlError: procedure(pVfs: PSQLite3VFS; nByte: Integer; zErrMsg: PAnsiChar); cdecl;
    xDlSym: function(pVfs: PSQLite3VFS; pHandle: Pointer; const zSymbol: PAnsiChar): Pointer; cdecl;
    xDlClose: procedure(pVfs: PSQLite3VFS; pHandle: Pointer); cdecl;
    xRandomness: function(pVfs: PSQLite3VFS; nByte: Integer; zOut: PAnsiChar): Integer; cdecl;
    xSleep: function(pVfs: PSQLite3VFS; microseconds: Integer): Integer; cdecl;
    xCurrentTime: function(pVfs: PSQLite3VFS; var prNow: Double): Integer; cdecl;
    xGetLastError: function(pVfs: PSQLite3VFS; nBuf: Integer; zBuf: PAnsiChar): Integer; cdecl;
  end;
  TSQLite3VFS = sqlite3_vfs;

const
  SQLITE_ACCESS_EXISTS    = 0;
  SQLITE_ACCESS_READWRITE = 1;
  SQLITE_ACCESS_READ      = 2;

type
  sqlite3_mem_methods = record
    xMalloc: function(nByte: Integer): Pointer; cdecl;
    xFree: procedure(pPrior: Pointer); cdecl;
    xRealloc: function(pPrior: Pointer; nByte: Integer): Pointer; cdecl;
    xSize: function(pPrior: Pointer): Integer; cdecl;
    xRoundup: function(n: Integer): Integer; cdecl;
    xInit: function(NotUsed: Pointer): Integer; cdecl;
    xShutdown: procedure(NotUsed: Pointer); cdecl;
    pAppData: Pointer;
  end;
  TSQLite3MemMethods = sqlite3_mem_methods;

const
  SQLITE_CONFIG_SINGLETHREAD = 1;
  SQLITE_CONFIG_MULTITHREAD  = 2;
  SQLITE_CONFIG_SERIALIZED   = 3;
  SQLITE_CONFIG_MALLOC       = 4;
  SQLITE_CONFIG_GETMALLOC    = 5;
  SQLITE_CONFIG_SCRATCH      = 6;
  SQLITE_CONFIG_PAGECACHE    = 7;
  SQLITE_CONFIG_HEAP         = 8;
  SQLITE_CONFIG_MEMSTATUS    = 9;
  SQLITE_CONFIG_MUTEX        = 10;
  SQLITE_CONFIG_GETMUTEX     = 11;
  //SQLITE_CONFIG_CHUNKALLOC   = 12;
  SQLITE_CONFIG_LOOKASIDE    = 13;
  SQLITE_CONFIG_PCACHE       = 14;
  SQLITE_CONFIG_GETPCACHE    = 15;

const
  SQLITE_DBCONFIG_LOOKASIDE  = 1001;

type
  TSQLite3BusyCallback = function(ptr: Pointer; count: Integer): Integer; cdecl;

type
  TSQLite3AuthorizerCallback = function(pAuthArg: Pointer; code: Integer; const zTab: PAnsiChar; const zCol: PAnsiChar; const zDb: PAnsiChar; const zAuthContext: PAnsiChar): Integer; cdecl;

const
  SQLITE_DENY   = 1;
  SQLITE_IGNORE = 2;

const
  SQLITE_CREATE_INDEX        = 1;
  SQLITE_CREATE_TABLE        = 2;
  SQLITE_CREATE_TEMP_INDEX   = 3;
  SQLITE_CREATE_TEMP_TABLE   = 4;
  SQLITE_CREATE_TEMP_TRIGGER = 5;
  SQLITE_CREATE_TEMP_VIEW    = 6;
  SQLITE_CREATE_TRIGGER      = 7;
  SQLITE_CREATE_VIEW         = 8;
  SQLITE_DELETE              = 9;
  SQLITE_DROP_INDEX          = 10;
  SQLITE_DROP_TABLE          = 11;
  SQLITE_DROP_TEMP_INDEX     = 12;
  SQLITE_DROP_TEMP_TABLE     = 13;
  SQLITE_DROP_TEMP_TRIGGER   = 14;
  SQLITE_DROP_TEMP_VIEW      = 15;
  SQLITE_DROP_TRIGGER        = 16;
  SQLITE_DROP_VIEW           = 17;
  SQLITE_INSERT              = 18;
  SQLITE_PRAGMA              = 19;
  SQLITE_READ                = 20;
  SQLITE_SELECT              = 21;
  SQLITE_TRANSACTION         = 22;
  SQLITE_UPDATE              = 23;
  SQLITE_ATTACH              = 24;
  SQLITE_DETACH              = 25;
  SQLITE_ALTER_TABLE         = 26;
  SQLITE_REINDEX             = 27;
  SQLITE_ANALYZE             = 28;
  SQLITE_CREATE_VTABLE       = 29;
  SQLITE_DROP_VTABLE         = 30;
  SQLITE_FUNCTION            = 31;
  SQLITE_SAVEPOINT           = 32;
  SQLITE_COPY                = 0;

type
  TSQLite3TraceCallback = procedure(pTraceArg: Pointer; const zTrace: PAnsiChar); cdecl;
  TSQLite3ProfileCallback = procedure(pProfileArg: Pointer; const zSql: PAnsiChar; elapseTime: UInt64); cdecl;

type
  TSQLite3ProgressCallback = function(pProgressArg: Pointer): Integer; cdecl;

type
  PSQLite3Stmt = type Pointer;

const
  SQLITE_LIMIT_LENGTH              = 0;
  SQLITE_LIMIT_SQL_LENGTH          = 1;
  SQLITE_LIMIT_COLUMN              = 2;
  SQLITE_LIMIT_EXPR_DEPTH          = 3;
  SQLITE_LIMIT_COMPOUND_SELECT     = 4;
  SQLITE_LIMIT_VDBE_OP             = 5;
  SQLITE_LIMIT_FUNCTION_ARG        = 6;
  SQLITE_LIMIT_ATTACHED            = 7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH = 8;
  SQLITE_LIMIT_VARIABLE_NUMBER     = 9;
  SQLITE_LIMIT_TRIGGER_DEPTH       = 10;

type
  PSQLite3Value = ^TSQLite3Value;
  sqlite3_value = type Pointer;
  TSQLite3Value = sqlite3_value;

  PPSQLite3ValueArray = ^TPSQLite3ValueArray;
  TPSQLite3ValueArray = array[0..MaxInt div SizeOf(PSQLite3Value) - 1] of PSQLite3Value;

type
  PSQLite3Context = type Pointer;

type
  TSQLite3DestructorType = procedure(p: Pointer); cdecl;

const
  SQLITE_STATIC    = Pointer(0);
  SQLITE_TRANSIENT = Pointer(-1);

const
  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;
  SQLITE_TEXT    = 3;
  SQLITE3_TEXT   = 3;

type
  TSQLite3RegularFunction = procedure(ctx: PSQLite3Context; n: Integer; apVal: PPSQLite3ValueArray); cdecl;
  TSQLite3AggregateStep = procedure(ctx: PSQLite3Context; n: Integer; apVal: PPSQLite3ValueArray); cdecl;
  TSQLite3AggregateFinalize = procedure(ctx: PSQLite3Context); cdecl;

const
  SQLITE_UTF8          = 1;
  SQLITE_UTF16LE       = 2;
  SQLITE_UTF16BE       = 3;
  SQLITE_UTF16         = 4;
  SQLITE_ANY           = 5;
  SQLITE_UTF16_ALIGNED = 8;

type
  TSQLite3AuxDataDestructor = procedure(pAux: Pointer); cdecl;

type
  TSQLite3CollationCompare = procedure(pUser: Pointer; n1: Integer; const z1: Pointer; n2: Integer; const z2: Pointer); cdecl;
  TSQLite3CollationDestructor = procedure(pUser: Pointer); cdecl;

type
  TSQLite3CollationNeededCallback = procedure(pCollNeededArg: Pointer; db: PSQLite3; eTextRep: Integer; const zExternal: PAnsiChar); cdecl;
  TSQLite3CollationNeededCallback16 = procedure(pCollNeededArg: Pointer; db: PSQLite3; eTextRep: Integer; const zExternal: PWideChar); cdecl;

type
  TSQLite3CommitCallback = function(pCommitArg: Pointer): Integer; cdecl;
  TSQLite3RollbackCallback = procedure(pRollbackArg: Pointer); cdecl;

type
  TSQLite3UpdateCallback = procedure(pUpdateArg: Pointer; op: Integer; const zDb: PAnsiChar; const zTbl: PAnsiChar; iKey: Int64); cdecl;

type
  TSQLiteAutoExtensionEntryPoint = procedure; cdecl;

type
  TSQLite3FTS3Func = procedure(pContext: PSQLite3Context; argc: Integer; argv: PPSQLite3ValueArray); cdecl;

type
  PSQLite3VTab = ^TSQLite3VTab;
  PSQLite3IndexInfo = ^TSQLite3IndexInfo;
  PSQLite3VTabCursor = ^TSQLite3VTabCursor;
  PSQLite3Module = ^TSQLite3Module;

  sqlite3_module = record
    iVersion: Integer;
    xCreate: function(db: PSQLite3; pAux: Pointer; argc: Integer; const argv: PPAnsiCharArray; var ppVTab: PSQLite3VTab; var pzErr: PAnsiChar): Integer; cdecl;
    xConnect: function(db: PSQLite3; pAux: Pointer; argc: Integer; const argv: PPAnsiCharArray; var ppVTab: PSQLite3VTab; var pzErr: PAnsiChar): Integer; cdecl;
    xBestIndex: function(pVTab: PSQLite3VTab; pInfo: PSQLite3IndexInfo): Integer; cdecl;
    xDisconnect: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xDestroy: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xOpen: function(pVTab: PSQLite3VTab; var ppCursor: PSQLite3VTabCursor): Integer; cdecl;
    xClose: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    xFilter: function(pVtabCursor: PSQLite3VTabCursor; idxNum: Integer; const idxStr: PAnsiChar; argc: Integer; argv: PPSQLite3ValueArray): Integer; cdecl;
    xNext: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    xEof: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    xColumn: function(pVtabCursor: PSQLite3VTabCursor; sContext: PSQLite3Context; p2: Integer): Integer; cdecl;
    xRowid: function(pVtabCursor: PSQLite3VTabCursor; var pRowid: Int64): Integer; cdecl;
    xUpdate: function(pVtab: PSQLite3VTab; nArg: Integer; ppArg: PPSQLite3ValueArray; var pRowid: Int64): Integer; cdecl;
    xBegin: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xSync: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xCommit: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xRollback: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xFindFunction: function(pVtab: PSQLite3VTab; nArg: Integer; const zName: PAnsiChar; var pxFunc: TSQLite3FTS3Func; var ppArg: Pointer): Integer; cdecl;
    xRename: function(pVtab: PSQLite3VTab; const zNew: PAnsiChar): Integer; cdecl;
  end;
  TSQLite3Module = sqlite3_module;

  sqlite3_index_constraint = record
    iColumn: Integer;
    op: Byte;
    usable: Byte;
    iTermOffset: Integer;
  end;
  TSQLite3IndexConstraint = sqlite3_index_constraint;

  PSQLite3IndexConstraintArray = ^TSQLite3IndexConstraintArray;
  TSQLite3IndexConstraintArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraint) - 1] of TSQLite3IndexConstraint;

  sqlite3_index_orderby = record
    iColumn: Integer;
    desc: Byte;
  end;
  TSQLite3IndexOrderBy = sqlite3_index_orderby;

  PSQLite3IndexOrderByArray = ^TSQLite3IndexOrderByArray;
  TSQLite3IndexOrderByArray = array[0..MaxInt div SizeOf(TSQLite3IndexOrderBy) - 1] of TSQLite3IndexOrderBy;

  sqlite3_index_constraint_usage = record
    argvIndex: Integer;
    omit: Byte;
  end;
  TSQLite3IndexConstraintUsage = sqlite3_index_constraint_usage;

  PSQLite3IndexConstraintUsageArray = ^TSQLite3IndexConstraintUsageArray;
  TSQLite3IndexConstraintUsageArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraintUsage) - 1] of TSQLite3IndexConstraintUsage;

  sqlite3_index_info = record
    nConstraint: Integer;
    aConstraint: PSQLite3IndexConstraintArray;
    nOrderBy: Integer;
    aOrderBy: PSQLite3IndexOrderByArray;
    aConstraintUsage: PSQLite3IndexConstraintUsageArray;
    idxNum: Integer;
    idxStr: PAnsiChar;
    needToFreeIdxStr: Integer;
    orderByConsumed: Integer;
    estimatedCost: Double;
  end;
  TSQLite3IndexInfo = sqlite3_index_info;

  sqlite3_vtab = record
    pModule: PSQLite3Module;
    nRef: Integer;
    zErrMsg: PAnsiChar;
  end;
  TSQLite3VTab = sqlite3_vtab;

  sqlite3_vtab_cursor = record
    pVtab: PSQLite3VTab;
  end;
  TSQLite3VTabCursor = sqlite3_vtab_cursor;

const
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;

type
  TSQLite3ModuleDestructor = procedure(pAux: Pointer); cdecl;

type
  PSQLite3Blob = type Pointer;

type
  sqlite3_mutex_methods = record
    xMutexInit: function: Integer; cdecl;
    xMutexEnd: function: Integer; cdecl;
    xMutexAlloc: function(id: Integer): PSQLite3Mutex; cdecl;
    xMutexFree: procedure(p: PSQLite3Mutex); cdecl;
    xMutexEnter: procedure(p: PSQLite3Mutex); cdecl;
    xMutexTry: function(p: PSQLite3Mutex): Integer; cdecl;
    xMutexLeave: procedure(p: PSQLite3Mutex); cdecl;
    xMutexHeld: function(p: PSQLite3Mutex): Integer; cdecl;
    xMutexNotheld: function(p: PSQLite3Mutex): Integer; cdecl;
  end;
  TSQLite3MutexMethods = sqlite3_mutex_methods;

const
  SQLITE_MUTEX_FAST          = 0;
  SQLITE_MUTEX_RECURSIVE     = 1;
  SQLITE_MUTEX_STATIC_MASTER = 2;
  SQLITE_MUTEX_STATIC_MEM    = 3;
  SQLITE_MUTEX_STATIC_MEM2   = 4;
  SQLITE_MUTEX_STATIC_OPEN   = 4;
  SQLITE_MUTEX_STATIC_PRNG   = 5;
  SQLITE_MUTEX_STATIC_LRU    = 6;
  SQLITE_MUTEX_STATIC_LRU2   = 7;

const
  SQLITE_TESTCTRL_FIRST               = 5;
  SQLITE_TESTCTRL_PRNG_SAVE           = 5;
  SQLITE_TESTCTRL_PRNG_RESTORE        = 6;
  SQLITE_TESTCTRL_PRNG_RESET          = 7;
  SQLITE_TESTCTRL_BITVEC_TEST         = 8;
  SQLITE_TESTCTRL_FAULT_INSTALL       = 9;
  SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS = 10;
  SQLITE_TESTCTRL_PENDING_BYTE        = 11;
  SQLITE_TESTCTRL_ASSERT              = 12;
  SQLITE_TESTCTRL_ALWAYS              = 13;
  SQLITE_TESTCTRL_RESERVE             = 14;
  SQLITE_TESTCTRL_OPTIMIZATIONS       = 15;
  SQLITE_TESTCTRL_ISKEYWORD           = 16;
  SQLITE_TESTCTRL_LAST                = 16;

const
  SQLITE_STATUS_MEMORY_USED        = 0;
  SQLITE_STATUS_PAGECACHE_USED     = 1;
  SQLITE_STATUS_PAGECACHE_OVERFLOW = 2;
  SQLITE_STATUS_SCRATCH_USED       = 3;
  SQLITE_STATUS_SCRATCH_OVERFLOW   = 4;
  SQLITE_STATUS_MALLOC_SIZE        = 5;
  SQLITE_STATUS_PARSER_STACK       = 6;
  SQLITE_STATUS_PAGECACHE_SIZE     = 7;
  SQLITE_STATUS_SCRATCH_SIZE       = 8;

const
  SQLITE_DBSTATUS_LOOKASIDE_USED = 0;

const
  SQLITE_STMTSTATUS_FULLSCAN_STEP = 1;
  SQLITE_STMTSTATUS_SORT          = 2;

type
  PSQLite3PCache = type Pointer;

type
  sqlite3_pcache_methods = record
    pArg: Pointer;
    xInit: function(pArg: Pointer): Integer; cdecl;
    xShutdown: procedure(pArg: Pointer); cdecl;
    xCreate: function(szPage: Integer; bPurgeable: Integer): PSQLite3PCache; cdecl;
    xCachesize: procedure(pCache: PSQLite3PCache; nCachesize: Integer); cdecl;
    xPagecount: function(pCache: PSQLite3PCache): Integer; cdecl;
    xFetch: function(pCache: PSQLite3PCache; key: Cardinal; createFlag: Integer): Pointer; cdecl;
    xUnpin: procedure(pCache: PSQLite3PCache; pPg: Pointer; discard: Integer); cdecl;
    xRekey: procedure(pCache: PSQLite3PCache; pPg: Pointer; oldKey: Cardinal; newKey: Cardinal); cdecl;
    xTruncate: procedure(pCache: PSQLite3PCache; iLimit: Cardinal); cdecl;
    xDestroy: procedure(pCache: PSQLite3PCache); cdecl;
  end;
  TSQLite3PCacheMethods = sqlite3_pcache_methods;

type
  PSQLite3Backup = type Pointer;

type
  TSQLite3UnlockNotifyCallback = procedure(apArg: PPointerArray; nArg: Integer); cdecl;

type
  TALSqlite3Library = class(TObject)
  private
    Flibsqlite3: THandle;
  public
    sqlite3_libversion: function: PAnsiChar; cdecl;
    sqlite3_sourceid: function: PAnsiChar; cdecl;
    sqlite3_libversion_number: function: Integer; cdecl;
    sqlite3_threadsafe: function: Integer; cdecl;
    sqlite3_close: function(db: PSQLite3): Integer; cdecl;
    sqlite3_exec: function(db: PSQLite3; const sql: PAnsiChar; callback: TSQLite3Callback; pArg: Pointer; errmsg: PPAnsiChar): Integer; cdecl;
    sqlite3_initialize: function: Integer; cdecl;
    sqlite3_shutdown: function: Integer; cdecl;
    sqlite3_os_init: function: Integer; cdecl;
    sqlite3_os_end: function: Integer; cdecl;
    sqlite3_config: function(op: Integer{; ...}): Integer; cdecl;
    sqlite3_db_config: function(db: PSQLite3; op: Integer{; ...}): Integer; cdecl;
    sqlite3_extended_result_codes: function(db: PSQLite3; onoff: Integer): Integer; cdecl;
    sqlite3_last_insert_rowid: function(db: PSQLite3): Int64; cdecl;
    sqlite3_changes: function(db: PSQLite3): Integer; cdecl;
    sqlite3_total_changes: function(db: PSQLite3): Integer; cdecl;
    sqlite3_interrupt: procedure(db: PSQLite3); cdecl;
    sqlite3_complete: function(const sql: PAnsiChar): Integer; cdecl;
    sqlite3_complete16: function(const sql: PWideChar): Integer; cdecl;
    sqlite3_busy_handler: function(db: PSQLite3; xBusy: TSQLite3BusyCallback; pArg: Pointer): Integer; cdecl;
    sqlite3_busy_timeout: function(db: PSQLite3; ms: Integer): Integer; cdecl;
    sqlite3_get_table: function(db: PSQLite3; const zSql: PAnsiChar; var pazResult: PPAnsiCharArray; pnRow: PInteger; pnColumn: PInteger; pzErrmsg: PPAnsiChar): Integer; cdecl;
    sqlite3_free_table: procedure(result: PPAnsiCharArray); cdecl;
    sqlite3_mprintf: function(const zFormat: PAnsiChar{; ...}): PAnsiChar; cdecl;
    sqlite3_vmprintf: function(const zFormat: PAnsiChar; ap: Pointer{va_list}): PAnsiChar; cdecl;
    sqlite3_snprintf: function(n: Integer; zBuf: PAnsiChar; const zFormat: PAnsiChar{; ...}): PAnsiChar; cdecl;
    sqlite3_malloc: function(n: Integer): Pointer; cdecl;
    sqlite3_realloc: function(pOld: Pointer; n: Integer): Pointer; cdecl;
    sqlite3_free: procedure(p: Pointer); cdecl;
    sqlite3_memory_used: function: Int64; cdecl;
    sqlite3_memory_highwater: function(resetFlag: Integer): Int64; cdecl;
    sqlite3_randomness: procedure(N: Integer; P: Pointer); cdecl;
    sqlite3_set_authorizer: function(db: PSQLite3; xAuth: TSQLite3AuthorizerCallback; pUserData: Pointer): Integer; cdecl;
    sqlite3_trace: function(db: PSQLite3; xTrace: TSQLite3TraceCallback; pArg: Pointer): Pointer; cdecl;
    sqlite3_profile: function(db: PSQLite3; xProfile: TSQLite3ProfileCallback; pArg: Pointer): Pointer; cdecl;
    sqlite3_progress_handler: procedure(db: PSQLite3; nOps: Integer; xProgress: TSQLite3ProgressCallback; pArg: Pointer); cdecl;
    sqlite3_open: function(const filename: PAnsiChar; var ppDb: PSQLite3): Integer; cdecl;
    sqlite3_open16: function(const filename: PWideChar; var ppDb: PSQLite3): Integer; cdecl;
    sqlite3_open_v2: function(const filename: PAnsiChar; var ppDb: PSQLite3; flags: Integer; const zVfs: PAnsiChar): Integer; cdecl;
    sqlite3_errcode: function(db: PSQLite3): Integer; cdecl;
    sqlite3_extended_errcode: function(db: PSQLite3): Integer; cdecl;
    sqlite3_errmsg: function(db: PSQLite3): PAnsiChar; cdecl;
    sqlite3_errmsg16: function(db: PSQLite3): PWideChar; cdecl;
    sqlite3_limit: function(db: PSQLite3; limitId: Integer; newLimit: Integer): Integer; cdecl;
    sqlite3_prepare: function(db: PSQLite3; const zSql: PAnsiChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPAnsiChar): Integer; cdecl;
    sqlite3_prepare_v2: function(db: PSQLite3; const zSql: PAnsiChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPAnsiChar): Integer; cdecl;
    sqlite3_prepare16: function(db: PSQLite3; const zSql: PWideChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPWideChar): Integer; cdecl;
    sqlite3_prepare16_v2: function(db: PSQLite3; const zSql: PWideChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPWideChar): Integer; cdecl;
    sqlite3_sql: function(pStmt: PSQLite3Stmt): PAnsiChar; cdecl;
    sqlite3_bind_blob: function(pStmt: PSQLite3Stmt; i: Integer; const zData: Pointer; n: Integer; xDel: TSQLite3DestructorType): Integer; cdecl;
    sqlite3_bind_double: function(pStmt: PSQLite3Stmt; i: Integer; rValue: Double): Integer; cdecl;
    sqlite3_bind_int: function(p: PSQLite3Stmt; i: Integer; iValue: Integer): Integer; cdecl;
    sqlite3_bind_int64: function(pStmt: PSQLite3Stmt; i: Integer; iValue: Int64): Integer; cdecl;
    sqlite3_bind_null: function(pStmt: PSQLite3Stmt; i: Integer): Integer; cdecl;
    sqlite3_bind_text: function(pStmt: PSQLite3Stmt; i: Integer; const zData: PAnsiChar; n: Integer; xDel: TSQLite3DestructorType): Integer; cdecl;
    sqlite3_bind_text16: function(pStmt: PSQLite3Stmt; i: Integer; const zData: PWideChar; nData: Integer; xDel: TSQLite3DestructorType): Integer; cdecl;
    sqlite3_bind_value: function(pStmt: PSQLite3Stmt; i: Integer; const pValue: PSQLite3Value): Integer; cdecl;
    sqlite3_bind_zeroblob: function(pStmt: PSQLite3Stmt; i: Integer; n: Integer): Integer; cdecl;
    sqlite3_bind_parameter_count: function(pStmt: PSQLite3Stmt): Integer; cdecl;
    sqlite3_bind_parameter_name: function(pStmt: PSQLite3Stmt; i: Integer): PAnsiChar; cdecl;
    sqlite3_bind_parameter_index: function(pStmt: PSQLite3Stmt; const zName: PAnsiChar): Integer; cdecl;
    sqlite3_clear_bindings: function(pStmt: PSQLite3Stmt): Integer; cdecl;
    sqlite3_column_count: function(pStmt: PSQLite3Stmt): Integer; cdecl;
    sqlite3_column_name: function(pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_name16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
    sqlite3_column_database_name: function(pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_database_name16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
    sqlite3_column_table_name: function(pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_table_name16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
    sqlite3_column_origin_name: function(pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_origin_name16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
    sqlite3_column_decltype: function(pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_column_decltype16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
    sqlite3_step: function(pStmt: PSQLite3Stmt): Integer; cdecl;
    sqlite3_data_count: function(pStmt: PSQLite3Stmt): Integer; cdecl;
    sqlite3_column_blob: function(pStmt: PSQLite3Stmt; iCol: Integer): Pointer; cdecl;
    sqlite3_column_bytes: function(pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
    sqlite3_column_bytes16: function(pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
    sqlite3_column_double: function(pStmt: PSQLite3Stmt; iCol: Integer): Double; cdecl;
    sqlite3_column_int: function(pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
    sqlite3_column_int64: function(pStmt: PSQLite3Stmt; iCol: Integer): Int64; cdecl;
    sqlite3_column_text: function(pStmt: PSQLite3Stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_text16: function(pStmt: PSQLite3Stmt; iCol: Integer): PWideChar; cdecl;
    sqlite3_column_type: function(pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
    sqlite3_column_value: function(pStmt: PSQLite3Stmt; iCol: Integer): PSQLite3Value; cdecl;
    sqlite3_finalize: function(pStmt: PSQLite3Stmt): Integer; cdecl;
    sqlite3_reset: function(pStmt: PSQLite3Stmt): Integer; cdecl;
    sqlite3_create_function: function(db: PSQLite3; const zFunctionName: PAnsiChar; nArg: Integer; eTextRep: Integer; pApp: Pointer; xFunc: TSQLite3RegularFunction; xStep: TSQLite3AggregateStep; xFinal: TSQLite3AggregateFinalize): Integer; cdecl;
    sqlite3_create_function16: function(db: PSQLite3; const zFunctionName: PWideChar; nArg: Integer; eTextRep: Integer; pApp: Pointer; xFunc: TSQLite3RegularFunction; xStep: TSQLite3AggregateStep; xFinal: TSQLite3AggregateFinalize): Integer; cdecl;
    sqlite3_value_blob: function(pVal: PSQLite3Value): Pointer; cdecl;
    sqlite3_value_bytes: function(pVal: PSQLite3Value): Integer; cdecl;
    sqlite3_value_bytes16: function(pVal: PSQLite3Value): Integer; cdecl;
    sqlite3_value_double: function(pVal: PSQLite3Value): Double; cdecl;
    sqlite3_value_int: function(pVal: PSQLite3Value): Integer; cdecl;
    sqlite3_value_int64: function(pVal: PSQLite3Value): Int64; cdecl;
    sqlite3_value_text: function(pVal: PSQLite3Value): PAnsiChar; cdecl;
    sqlite3_value_text16: function(pVal: PSQLite3Value): PWideChar; cdecl;
    sqlite3_value_text16le: function(pVal: PSQLite3Value): Pointer; cdecl;
    sqlite3_value_text16be: function(pVal: PSQLite3Value): Pointer; cdecl;
    sqlite3_value_type: function(pVal: PSQLite3Value): Integer; cdecl;
    sqlite3_value_numeric_type: function(pVal: PSQLite3Value): Integer; cdecl;
    sqlite3_aggregate_context: function(p: PSQLite3Context; nBytes: Integer): Pointer; cdecl;
    sqlite3_user_data: function(p: PSQLite3Context): Pointer; cdecl;
    sqlite3_context_db_handle: function(p: PSQLite3Context): PSQLite3; cdecl;
    sqlite3_get_auxdata: function(pCtx: PSQLite3Context; N: Integer): Pointer; cdecl;
    sqlite3_set_auxdata: procedure(pCtx: PSQLite3Context; N: Integer; pAux: Pointer; xDelete: TSQLite3AuxDataDestructor); cdecl;
    sqlite3_result_blob: procedure(pCtx: PSQLite3Context; const z: Pointer; n: Integer; xDel: TSQLite3DestructorType); cdecl;
    sqlite3_result_double: procedure(pCtx: PSQLite3Context; rVal: Double); cdecl;
    sqlite3_result_error: procedure(pCtx: PSQLite3Context; const z: PAnsiChar; n: Integer); cdecl;
    sqlite3_result_error16: procedure(pCtx: PSQLite3Context; const z: PWideChar; n: Integer); cdecl;
    sqlite3_result_error_toobig: procedure(pCtx: PSQLite3Context); cdecl;
    sqlite3_result_error_nomem: procedure(pCtx: PSQLite3Context); cdecl;
    sqlite3_result_error_code: procedure(pCtx: PSQLite3Context; errCode: Integer); cdecl;
    sqlite3_result_int: procedure(pCtx: PSQLite3Context; iVal: Integer); cdecl;
    sqlite3_result_int64: procedure(pCtx: PSQLite3Context; iVal: Int64); cdecl;
    sqlite3_result_null: procedure(pCtx: PSQLite3Context); cdecl;
    sqlite3_result_text: procedure(pCtx: PSQLite3Context; const z: PAnsiChar; n: Integer; xDel: TSQLite3DestructorType); cdecl;
    sqlite3_result_text16: procedure(pCtx: PSQLite3Context; const z: PWideChar; n: Integer; xDel: TSQLite3DestructorType); cdecl;
    sqlite3_result_text16le: procedure(pCtx: PSQLite3Context; const z: Pointer; n: Integer; xDel: TSQLite3DestructorType); cdecl;
    sqlite3_result_text16be: procedure(pCtx: PSQLite3Context; const z: Pointer; n: Integer; xDel: TSQLite3DestructorType); cdecl;
    sqlite3_result_value: procedure(pCtx: PSQLite3Context; pValue: PSQLite3Value); cdecl;
    sqlite3_result_zeroblob: procedure(pCtx: PSQLite3Context; n: Integer); cdecl;
    sqlite3_create_collation: function(db: PSQLite3; const zName: PAnsiChar; eTextRep: Integer; pUser: Pointer; xCompare: TSQLite3CollationCompare): Integer; cdecl;
    sqlite3_create_collation_v2: function(db: PSQLite3; const zName: PAnsiChar; eTextRep: Integer; pUser: Pointer; xCompare: TSQLite3CollationCompare; xDestroy: TSQLite3CollationDestructor): Integer; cdecl;
    sqlite3_create_collation16: function(db: PSQLite3; const zName: PWideChar; eTextRep: Integer; pUser: Pointer; xCompare: TSQLite3CollationCompare): Integer; cdecl;
    sqlite3_collation_needed: function(db: PSQLite3; pCollNeededArg: Pointer; xCollNeeded: TSQLite3CollationNeededCallback): Integer; cdecl;
    sqlite3_collation_needed16: function(db: PSQLite3; pCollNeededArg: Pointer; xCollNeeded16: TSQLite3CollationNeededCallback16): Integer; cdecl;
    sqlite3_sleep: function(ms: Integer): Integer; cdecl;
    sqlite3_get_autocommit: function(db: PSQLite3): Integer; cdecl;
    sqlite3_db_handle: function(pStmt: PSQLite3Stmt): PSQLite3; cdecl;
    sqlite3_next_stmt: function(pDb: PSQLite3; pStmt: PSQLite3Stmt): PSQLite3Stmt; cdecl;
    sqlite3_commit_hook: function(db: PSQLite3; xCallback: TSQLite3CommitCallback; pArg: Pointer): Pointer; cdecl;
    sqlite3_rollback_hook: function(db: PSQLite3; xCallback: TSQLite3RollbackCallback; pArg: Pointer): Pointer; cdecl;
    sqlite3_update_hook: function(db: PSQLite3; xCallback: TSQLite3UpdateCallback; pArg: Pointer): Pointer; cdecl;
    sqlite3_enable_shared_cache: function(enable: Integer): Integer; cdecl;
    sqlite3_release_memory: function(n: Integer): Integer; cdecl;
    sqlite3_soft_heap_limit64: function(n: Int64): int64; cdecl;
    sqlite3_table_column_metadata: function(db: PSQLite3; const zDbName: PAnsiChar; const zTableName: PAnsiChar; const zColumnName: PAnsiChar; const pzDataType: PPAnsiChar; const pzCollSeq: PPAnsiChar; pNotNull: PInteger; pPrimaryKey: PInteger; pAutoinc: PInteger): Integer; cdecl;
    sqlite3_load_extension: function(db: PSQLite3; const zFile: PAnsiChar; const zProc: PAnsiChar; pzErrMsg: PPAnsiChar): Integer; cdecl;
    sqlite3_enable_load_extension: function(db: PSQLite3; onoff: Integer): Integer; cdecl;
    sqlite3_auto_extension: function(xEntryPoint: TSQLiteAutoExtensionEntryPoint): Integer; cdecl;
    sqlite3_reset_auto_extension: procedure; cdecl;
    sqlite3_create_module: function(db: PSQLite3; const zName: PAnsiChar; const p: PSQLite3Module; pClientData: Pointer): Integer; cdecl;
    sqlite3_create_module_v2: function(db: PSQLite3; const zName: PAnsiChar; const p: PSQLite3Module; pClientData: Pointer; xDestroy: TSQLite3ModuleDestructor): Integer; cdecl;
    sqlite3_declare_vtab: function(db: PSQLite3; const zSQL: PAnsiChar): Integer; cdecl;
    sqlite3_overload_function: function(db: PSQLite3; const zFuncName: PAnsiChar; nArg: Integer): Integer; cdecl;
    sqlite3_blob_open: function(db: PSQLite3; const zDb: PAnsiChar; const zTable: PAnsiChar; const zColumn: PAnsiChar; iRow: Int64; flags: Integer; var ppBlob: PSQLite3Blob): Integer; cdecl;
    sqlite3_blob_close: function(pBlob: PSQLite3Blob): Integer; cdecl;
    sqlite3_blob_bytes: function(pBlob: PSQLite3Blob): Integer; cdecl;
    sqlite3_blob_read: function(pBlob: PSQLite3Blob; Z: Pointer; N: Integer; iOffset: Integer): Integer; cdecl;
    sqlite3_blob_write: function(pBlob: PSQLite3Blob; const z: Pointer; n: Integer; iOffset: Integer): Integer; cdecl;
    sqlite3_vfs_find: function(const zVfsName: PAnsiChar): PSQLite3VFS; cdecl;
    sqlite3_vfs_register: function(pVfs: PSQLite3VFS; makeDflt: Integer): Integer; cdecl;
    sqlite3_vfs_unregister: function(pVfs: PSQLite3VFS): Integer; cdecl;
    sqlite3_mutex_alloc: function(id: Integer): PSQLite3Mutex; cdecl;
    sqlite3_mutex_free: procedure(p: PSQLite3Mutex); cdecl;
    sqlite3_mutex_enter: procedure(p: PSQLite3Mutex); cdecl;
    sqlite3_mutex_try: function(p: PSQLite3Mutex): Integer; cdecl;
    sqlite3_mutex_leave: procedure(p: PSQLite3Mutex); cdecl;
    sqlite3_db_mutex: function(db: PSQLite3): PSQLite3Mutex; cdecl;
    sqlite3_file_control: function(db: PSQLite3; const zDbName: PAnsiChar; op: Integer; pArg: Pointer): Integer; cdecl;
    sqlite3_test_control: function(op: Integer{; ...}): Integer; cdecl;
    sqlite3_status: function(op: Integer; var pCurrent: Integer; var pHighwater: Integer; resetFlag: Integer): Integer; cdecl;
    sqlite3_db_status: function(db: PSQLite3; op: Integer; var pCur: Integer; var pHiwtr: Integer; resetFlg: Integer): Integer; cdecl;
    sqlite3_stmt_status: function(pStmt: PSQLite3Stmt; op: Integer; resetFlg: Integer): Integer; cdecl;
    sqlite3_backup_init: function(pDest: PSQLite3; const zDestName: PAnsiChar; pSource: PSQLite3; const zSourceName: PAnsiChar): PSQLite3Backup; cdecl;
    sqlite3_backup_step: function(p: PSQLite3Backup; nPage: Integer): Integer; cdecl;
    sqlite3_backup_finish: function(p: PSQLite3Backup): Integer; cdecl;
    sqlite3_backup_remaining: function(p: PSQLite3Backup): Integer; cdecl;
    sqlite3_backup_pagecount: function(p: PSQLite3Backup): Integer; cdecl;
    sqlite3_strnicmp: function(const zLeft: PAnsiChar; const zRight: PAnsiChar; N: Integer): Integer; cdecl;
    constructor Create; virtual;
    destructor Destroy; override;
    function Loaded: Boolean; virtual;
    function Unload: Boolean; virtual;
    function Load(const lib: AnsiString = 'sqlite3.dll'; const initialize: boolean = True): Boolean; virtual;
  end;

implementation

uses sysutils;

{***********************************}
constructor TALSqlite3Library.Create;
begin
  Flibsqlite3 := 0;
end;

{***********************************}
destructor TALSqlite3Library.Destroy;
begin
  Unload;
  inherited Destroy;
end;

{*****************************************}
function TALSqlite3Library.Loaded: Boolean;
begin
  Result := Flibsqlite3 > HINSTANCE_ERROR;
end;

{*****************************************}
function TALSqlite3Library.Unload: Boolean;
begin
  Result := True;
  if Loaded then begin
    sqlite3_shutdown; // A call to sqlite3_shutdown() is an "effective" call if it is the first call to sqlite3_shutdown()
                      // since the last sqlite3_initialize(). Only an effective call to sqlite3_shutdown() does any deinitialization.
                      // All other valid calls to sqlite3_shutdown() are harmless no-ops.
    Result := Boolean(FreeLibrary(Flibsqlite3));
    Flibsqlite3 := 0;
    sqlite3_libversion := nil;
    sqlite3_sourceid := nil;
    sqlite3_libversion_number := nil;
    sqlite3_threadsafe := nil;
    sqlite3_close := nil;
    sqlite3_exec := nil;
    sqlite3_initialize := nil;
    sqlite3_shutdown := nil;
    sqlite3_os_init := nil;
    sqlite3_os_end := nil;
    sqlite3_config := nil;
    sqlite3_db_config := nil;
    sqlite3_extended_result_codes := nil;
    sqlite3_last_insert_rowid := nil;
    sqlite3_changes := nil;
    sqlite3_total_changes := nil;
    sqlite3_interrupt := nil;
    sqlite3_complete := nil;
    sqlite3_complete16 := nil;
    sqlite3_busy_handler := nil;
    sqlite3_busy_timeout := nil;
    sqlite3_get_table := nil;
    sqlite3_free_table := nil;
    sqlite3_mprintf := nil;
    sqlite3_vmprintf := nil;
    sqlite3_snprintf := nil;
    sqlite3_malloc := nil;
    sqlite3_realloc := nil;
    sqlite3_free := nil;
    sqlite3_memory_used := nil;
    sqlite3_memory_highwater := nil;
    sqlite3_randomness := nil;
    sqlite3_set_authorizer := nil;
    sqlite3_trace := nil;
    sqlite3_profile := nil;
    sqlite3_progress_handler := nil;
    sqlite3_open := nil;
    sqlite3_open16 := nil;
    sqlite3_open_v2 := nil;
    sqlite3_errcode := nil;
    sqlite3_extended_errcode := nil;
    sqlite3_errmsg := nil;
    sqlite3_errmsg16 := nil;
    sqlite3_limit := nil;
    sqlite3_prepare := nil;
    sqlite3_prepare_v2 := nil;
    sqlite3_prepare16 := nil;
    sqlite3_prepare16_v2 := nil;
    sqlite3_sql := nil;
    sqlite3_bind_blob := nil;
    sqlite3_bind_double := nil;
    sqlite3_bind_int := nil;
    sqlite3_bind_int64 := nil;
    sqlite3_bind_null := nil;
    sqlite3_bind_text := nil;
    sqlite3_bind_text16 := nil;
    sqlite3_bind_value := nil;
    sqlite3_bind_zeroblob := nil;
    sqlite3_bind_parameter_count := nil;
    sqlite3_bind_parameter_name := nil;
    sqlite3_bind_parameter_index := nil;
    sqlite3_clear_bindings := nil;
    sqlite3_column_count := nil;
    sqlite3_column_name := nil;
    sqlite3_column_name16 := nil;
    sqlite3_column_database_name := nil;
    sqlite3_column_database_name16 := nil;
    sqlite3_column_table_name := nil;
    sqlite3_column_table_name16 := nil;
    sqlite3_column_origin_name := nil;
    sqlite3_column_origin_name16 := nil;
    sqlite3_column_decltype := nil;
    sqlite3_column_decltype16 := nil;
    sqlite3_step := nil;
    sqlite3_data_count := nil;
    sqlite3_column_blob := nil;
    sqlite3_column_bytes := nil;
    sqlite3_column_bytes16 := nil;
    sqlite3_column_double := nil;
    sqlite3_column_int := nil;
    sqlite3_column_int64 := nil;
    sqlite3_column_text := nil;
    sqlite3_column_text16 := nil;
    sqlite3_column_type := nil;
    sqlite3_column_value := nil;
    sqlite3_finalize := nil;
    sqlite3_reset := nil;
    sqlite3_create_function := nil;
    sqlite3_create_function16 := nil;
    sqlite3_value_blob := nil;
    sqlite3_value_bytes := nil;
    sqlite3_value_bytes16 := nil;
    sqlite3_value_double := nil;
    sqlite3_value_int := nil;
    sqlite3_value_int64 := nil;
    sqlite3_value_text := nil;
    sqlite3_value_text16 := nil;
    sqlite3_value_text16le := nil;
    sqlite3_value_text16be := nil;
    sqlite3_value_type := nil;
    sqlite3_value_numeric_type := nil;
    sqlite3_aggregate_context := nil;
    sqlite3_user_data := nil;
    sqlite3_context_db_handle := nil;
    sqlite3_get_auxdata := nil;
    sqlite3_set_auxdata := nil;
    sqlite3_result_blob := nil;
    sqlite3_result_double := nil;
    sqlite3_result_error := nil;
    sqlite3_result_error16 := nil;
    sqlite3_result_error_toobig := nil;
    sqlite3_result_error_nomem := nil;
    sqlite3_result_error_code := nil;
    sqlite3_result_int := nil;
    sqlite3_result_int64 := nil;
    sqlite3_result_null := nil;
    sqlite3_result_text := nil;
    sqlite3_result_text16 := nil;
    sqlite3_result_text16le := nil;
    sqlite3_result_text16be := nil;
    sqlite3_result_value := nil;
    sqlite3_result_zeroblob := nil;
    sqlite3_create_collation := nil;
    sqlite3_create_collation_v2 := nil;
    sqlite3_create_collation16 := nil;
    sqlite3_collation_needed := nil;
    sqlite3_collation_needed16 := nil;
    sqlite3_sleep := nil;
    sqlite3_get_autocommit := nil;
    sqlite3_db_handle := nil;
    sqlite3_next_stmt := nil;
    sqlite3_commit_hook := nil;
    sqlite3_rollback_hook := nil;
    sqlite3_update_hook := nil;
    sqlite3_enable_shared_cache := nil;
    sqlite3_release_memory := nil;
    sqlite3_soft_heap_limit64 := nil;
    sqlite3_table_column_metadata := nil;
    sqlite3_load_extension := nil;
    sqlite3_enable_load_extension := nil;
    sqlite3_auto_extension := nil;
    sqlite3_reset_auto_extension := nil;
    sqlite3_create_module := nil;
    sqlite3_create_module_v2 := nil;
    sqlite3_declare_vtab := nil;
    sqlite3_overload_function := nil;
    sqlite3_blob_open := nil;
    sqlite3_blob_close := nil;
    sqlite3_blob_bytes := nil;
    sqlite3_blob_read := nil;
    sqlite3_blob_write := nil;
    sqlite3_vfs_find := nil;
    sqlite3_vfs_register := nil;
    sqlite3_vfs_unregister := nil;
    sqlite3_mutex_alloc := nil;
    sqlite3_mutex_free := nil;
    sqlite3_mutex_enter := nil;
    sqlite3_mutex_try := nil;
    sqlite3_mutex_leave := nil;
    sqlite3_db_mutex := nil;
    sqlite3_file_control := nil;
    sqlite3_test_control := nil;
    sqlite3_status := nil;
    sqlite3_db_status := nil;
    sqlite3_stmt_status := nil;
    sqlite3_backup_init := nil;
    sqlite3_backup_step := nil;
    sqlite3_backup_finish := nil;
    sqlite3_backup_remaining := nil;
    sqlite3_backup_pagecount := nil;
    sqlite3_strnicmp := nil;
  end;
end;

{************************************************************************************************************}
function TALSqlite3Library.Load(const lib: AnsiString = 'sqlite3.dll'; const initialize: boolean = True): Boolean;
Begin
  Result := Loaded;
  if not Result then begin
    Flibsqlite3 := LoadLibraryA(PAnsiChar(lib));
    if Loaded then begin
      sqlite3_libversion := GetProcAddress(Flibsqlite3,'sqlite3_libversion');
      sqlite3_sourceid := GetProcAddress(Flibsqlite3,'sqlite3_sourceid');
      sqlite3_libversion_number := GetProcAddress(Flibsqlite3,'sqlite3_libversion_number');
      sqlite3_threadsafe := GetProcAddress(Flibsqlite3,'sqlite3_threadsafe');
      sqlite3_close := GetProcAddress(Flibsqlite3,'sqlite3_close');
      sqlite3_exec := GetProcAddress(Flibsqlite3,'sqlite3_exec');
      sqlite3_initialize := GetProcAddress(Flibsqlite3,'sqlite3_initialize');
      sqlite3_shutdown := GetProcAddress(Flibsqlite3,'sqlite3_shutdown');
      sqlite3_os_init := GetProcAddress(Flibsqlite3,'sqlite3_os_init');
      sqlite3_os_end := GetProcAddress(Flibsqlite3,'sqlite3_os_end');
      sqlite3_config := GetProcAddress(Flibsqlite3,'sqlite3_config');
      sqlite3_db_config := GetProcAddress(Flibsqlite3,'sqlite3_db_config');
      sqlite3_extended_result_codes := GetProcAddress(Flibsqlite3,'sqlite3_extended_result_codes');
      sqlite3_last_insert_rowid := GetProcAddress(Flibsqlite3,'sqlite3_last_insert_rowid');
      sqlite3_changes := GetProcAddress(Flibsqlite3,'sqlite3_changes');
      sqlite3_total_changes := GetProcAddress(Flibsqlite3,'sqlite3_total_changes');
      sqlite3_interrupt := GetProcAddress(Flibsqlite3,'sqlite3_interrupt');
      sqlite3_complete := GetProcAddress(Flibsqlite3,'sqlite3_complete');
      sqlite3_complete16 := GetProcAddress(Flibsqlite3,'sqlite3_complete16');
      sqlite3_busy_handler := GetProcAddress(Flibsqlite3,'sqlite3_busy_handler');
      sqlite3_busy_timeout := GetProcAddress(Flibsqlite3,'sqlite3_busy_timeout');
      sqlite3_get_table := GetProcAddress(Flibsqlite3,'sqlite3_get_table');
      sqlite3_free_table := GetProcAddress(Flibsqlite3,'sqlite3_free_table');
      sqlite3_mprintf := GetProcAddress(Flibsqlite3,'sqlite3_mprintf');
      sqlite3_vmprintf := GetProcAddress(Flibsqlite3,'sqlite3_vmprintf');
      sqlite3_snprintf := GetProcAddress(Flibsqlite3,'sqlite3_snprintf');
      sqlite3_malloc := GetProcAddress(Flibsqlite3,'sqlite3_malloc');
      sqlite3_realloc := GetProcAddress(Flibsqlite3,'sqlite3_realloc');
      sqlite3_free := GetProcAddress(Flibsqlite3,'sqlite3_free');
      sqlite3_memory_used := GetProcAddress(Flibsqlite3,'sqlite3_memory_used');
      sqlite3_memory_highwater := GetProcAddress(Flibsqlite3,'sqlite3_memory_highwater');
      sqlite3_randomness := GetProcAddress(Flibsqlite3,'sqlite3_randomness');
      sqlite3_set_authorizer := GetProcAddress(Flibsqlite3,'sqlite3_set_authorizer');
      sqlite3_trace := GetProcAddress(Flibsqlite3,'sqlite3_trace');
      sqlite3_profile := GetProcAddress(Flibsqlite3,'sqlite3_profile');
      sqlite3_progress_handler := GetProcAddress(Flibsqlite3,'sqlite3_progress_handler');
      sqlite3_open := GetProcAddress(Flibsqlite3,'sqlite3_open');
      sqlite3_open16 := GetProcAddress(Flibsqlite3,'sqlite3_open16');
      sqlite3_open_v2 := GetProcAddress(Flibsqlite3,'sqlite3_open_v2');
      sqlite3_errcode := GetProcAddress(Flibsqlite3,'sqlite3_errcode');
      sqlite3_extended_errcode := GetProcAddress(Flibsqlite3,'sqlite3_extended_errcode');
      sqlite3_errmsg := GetProcAddress(Flibsqlite3,'sqlite3_errmsg');
      sqlite3_errmsg16 := GetProcAddress(Flibsqlite3,'sqlite3_errmsg16');
      sqlite3_limit := GetProcAddress(Flibsqlite3,'sqlite3_limit');
      sqlite3_prepare := GetProcAddress(Flibsqlite3,'sqlite3_prepare');
      sqlite3_prepare_v2 := GetProcAddress(Flibsqlite3,'sqlite3_prepare_v2');
      sqlite3_prepare16 := GetProcAddress(Flibsqlite3,'sqlite3_prepare16');
      sqlite3_prepare16_v2 := GetProcAddress(Flibsqlite3,'sqlite3_prepare16_v2');
      sqlite3_sql := GetProcAddress(Flibsqlite3,'sqlite3_sql');
      sqlite3_bind_blob := GetProcAddress(Flibsqlite3,'sqlite3_bind_blob');
      sqlite3_bind_double := GetProcAddress(Flibsqlite3,'sqlite3_bind_double');
      sqlite3_bind_int := GetProcAddress(Flibsqlite3,'sqlite3_bind_int');
      sqlite3_bind_int64 := GetProcAddress(Flibsqlite3,'sqlite3_bind_int64');
      sqlite3_bind_null := GetProcAddress(Flibsqlite3,'sqlite3_bind_null');
      sqlite3_bind_text := GetProcAddress(Flibsqlite3,'sqlite3_bind_text');
      sqlite3_bind_text16 := GetProcAddress(Flibsqlite3,'sqlite3_bind_text16');
      sqlite3_bind_value := GetProcAddress(Flibsqlite3,'sqlite3_bind_value');
      sqlite3_bind_zeroblob := GetProcAddress(Flibsqlite3,'sqlite3_bind_zeroblob');
      sqlite3_bind_parameter_count := GetProcAddress(Flibsqlite3,'sqlite3_bind_parameter_count');
      sqlite3_bind_parameter_name := GetProcAddress(Flibsqlite3,'sqlite3_bind_parameter_name');
      sqlite3_bind_parameter_index := GetProcAddress(Flibsqlite3,'sqlite3_bind_parameter_index');
      sqlite3_clear_bindings := GetProcAddress(Flibsqlite3,'sqlite3_clear_bindings');
      sqlite3_column_count := GetProcAddress(Flibsqlite3,'sqlite3_column_count');
      sqlite3_column_name := GetProcAddress(Flibsqlite3,'sqlite3_column_name');
      sqlite3_column_name16 := GetProcAddress(Flibsqlite3,'sqlite3_column_name16');
      sqlite3_column_database_name := GetProcAddress(Flibsqlite3,'sqlite3_column_database_name');
      sqlite3_column_database_name16 := GetProcAddress(Flibsqlite3,'sqlite3_column_database_name16');
      sqlite3_column_table_name := GetProcAddress(Flibsqlite3,'sqlite3_column_table_name');
      sqlite3_column_table_name16 := GetProcAddress(Flibsqlite3,'sqlite3_column_table_name16');
      sqlite3_column_origin_name := GetProcAddress(Flibsqlite3,'sqlite3_column_origin_name');
      sqlite3_column_origin_name16 := GetProcAddress(Flibsqlite3,'sqlite3_column_origin_name16');
      sqlite3_column_decltype := GetProcAddress(Flibsqlite3,'sqlite3_column_decltype');
      sqlite3_column_decltype16 := GetProcAddress(Flibsqlite3,'sqlite3_column_decltype16');
      sqlite3_step := GetProcAddress(Flibsqlite3,'sqlite3_step');
      sqlite3_data_count := GetProcAddress(Flibsqlite3,'sqlite3_data_count');
      sqlite3_column_blob := GetProcAddress(Flibsqlite3,'sqlite3_column_blob');
      sqlite3_column_bytes := GetProcAddress(Flibsqlite3,'sqlite3_column_bytes');
      sqlite3_column_bytes16 := GetProcAddress(Flibsqlite3,'sqlite3_column_bytes16');
      sqlite3_column_double := GetProcAddress(Flibsqlite3,'sqlite3_column_double');
      sqlite3_column_int := GetProcAddress(Flibsqlite3,'sqlite3_column_int');
      sqlite3_column_int64 := GetProcAddress(Flibsqlite3,'sqlite3_column_int64');
      sqlite3_column_text := GetProcAddress(Flibsqlite3,'sqlite3_column_text');
      sqlite3_column_text16 := GetProcAddress(Flibsqlite3,'sqlite3_column_text16');
      sqlite3_column_type := GetProcAddress(Flibsqlite3,'sqlite3_column_type');
      sqlite3_column_value := GetProcAddress(Flibsqlite3,'sqlite3_column_value');
      sqlite3_finalize := GetProcAddress(Flibsqlite3,'sqlite3_finalize');
      sqlite3_reset := GetProcAddress(Flibsqlite3,'sqlite3_reset');
      sqlite3_create_function := GetProcAddress(Flibsqlite3,'sqlite3_create_function');
      sqlite3_create_function16 := GetProcAddress(Flibsqlite3,'sqlite3_create_function16');
      sqlite3_value_blob := GetProcAddress(Flibsqlite3,'sqlite3_value_blob');
      sqlite3_value_bytes := GetProcAddress(Flibsqlite3,'sqlite3_value_bytes');
      sqlite3_value_bytes16 := GetProcAddress(Flibsqlite3,'sqlite3_value_bytes16');
      sqlite3_value_double := GetProcAddress(Flibsqlite3,'sqlite3_value_double');
      sqlite3_value_int := GetProcAddress(Flibsqlite3,'sqlite3_value_int');
      sqlite3_value_int64 := GetProcAddress(Flibsqlite3,'sqlite3_value_int64');
      sqlite3_value_text := GetProcAddress(Flibsqlite3,'sqlite3_value_text');
      sqlite3_value_text16 := GetProcAddress(Flibsqlite3,'sqlite3_value_text16');
      sqlite3_value_text16le := GetProcAddress(Flibsqlite3,'sqlite3_value_text16le');
      sqlite3_value_text16be := GetProcAddress(Flibsqlite3,'sqlite3_value_text16be');
      sqlite3_value_type := GetProcAddress(Flibsqlite3,'sqlite3_value_type');
      sqlite3_value_numeric_type := GetProcAddress(Flibsqlite3,'sqlite3_value_numeric_type');
      sqlite3_aggregate_context := GetProcAddress(Flibsqlite3,'sqlite3_aggregate_context');
      sqlite3_user_data := GetProcAddress(Flibsqlite3,'sqlite3_user_data');
      sqlite3_context_db_handle := GetProcAddress(Flibsqlite3,'sqlite3_context_db_handle');
      sqlite3_get_auxdata := GetProcAddress(Flibsqlite3,'sqlite3_get_auxdata');
      sqlite3_set_auxdata := GetProcAddress(Flibsqlite3,'sqlite3_set_auxdata');
      sqlite3_result_blob := GetProcAddress(Flibsqlite3,'sqlite3_result_blob');
      sqlite3_result_double := GetProcAddress(Flibsqlite3,'sqlite3_result_double');
      sqlite3_result_error := GetProcAddress(Flibsqlite3,'sqlite3_result_error');
      sqlite3_result_error16 := GetProcAddress(Flibsqlite3,'sqlite3_result_error16');
      sqlite3_result_error_toobig := GetProcAddress(Flibsqlite3,'sqlite3_result_error_toobig');
      sqlite3_result_error_nomem := GetProcAddress(Flibsqlite3,'sqlite3_result_error_nomem');
      sqlite3_result_error_code := GetProcAddress(Flibsqlite3,'sqlite3_result_error_code');
      sqlite3_result_int := GetProcAddress(Flibsqlite3,'sqlite3_result_int');
      sqlite3_result_int64 := GetProcAddress(Flibsqlite3,'sqlite3_result_int64');
      sqlite3_result_null := GetProcAddress(Flibsqlite3,'sqlite3_result_null');
      sqlite3_result_text := GetProcAddress(Flibsqlite3,'sqlite3_result_text');
      sqlite3_result_text16 := GetProcAddress(Flibsqlite3,'sqlite3_result_text16');
      sqlite3_result_text16le := GetProcAddress(Flibsqlite3,'sqlite3_result_text16le');
      sqlite3_result_text16be := GetProcAddress(Flibsqlite3,'sqlite3_result_text16be');
      sqlite3_result_value := GetProcAddress(Flibsqlite3,'sqlite3_result_value');
      sqlite3_result_zeroblob := GetProcAddress(Flibsqlite3,'sqlite3_result_zeroblob');
      sqlite3_create_collation := GetProcAddress(Flibsqlite3,'sqlite3_create_collation');
      sqlite3_create_collation_v2 := GetProcAddress(Flibsqlite3,'sqlite3_create_collation_v2');
      sqlite3_create_collation16 := GetProcAddress(Flibsqlite3,'sqlite3_create_collation16');
      sqlite3_collation_needed := GetProcAddress(Flibsqlite3,'sqlite3_collation_needed');
      sqlite3_collation_needed16 := GetProcAddress(Flibsqlite3,'sqlite3_collation_needed16');
      sqlite3_sleep := GetProcAddress(Flibsqlite3,'sqlite3_sleep');
      sqlite3_get_autocommit := GetProcAddress(Flibsqlite3,'sqlite3_get_autocommit');
      sqlite3_db_handle := GetProcAddress(Flibsqlite3,'sqlite3_db_handle');
      sqlite3_next_stmt := GetProcAddress(Flibsqlite3,'sqlite3_next_stmt');
      sqlite3_commit_hook := GetProcAddress(Flibsqlite3,'sqlite3_commit_hook');
      sqlite3_rollback_hook := GetProcAddress(Flibsqlite3,'sqlite3_rollback_hook');
      sqlite3_update_hook := GetProcAddress(Flibsqlite3,'sqlite3_update_hook');
      sqlite3_enable_shared_cache := GetProcAddress(Flibsqlite3,'sqlite3_enable_shared_cache');
      sqlite3_release_memory := GetProcAddress(Flibsqlite3,'sqlite3_release_memory');
      sqlite3_soft_heap_limit64 := GetProcAddress(Flibsqlite3,'sqlite3_soft_heap_limit64');
      sqlite3_table_column_metadata := GetProcAddress(Flibsqlite3,'sqlite3_table_column_metadata');
      sqlite3_load_extension := GetProcAddress(Flibsqlite3,'sqlite3_load_extension');
      sqlite3_enable_load_extension := GetProcAddress(Flibsqlite3,'sqlite3_enable_load_extension');
      sqlite3_auto_extension := GetProcAddress(Flibsqlite3,'sqlite3_auto_extension');
      sqlite3_reset_auto_extension := GetProcAddress(Flibsqlite3,'sqlite3_reset_auto_extension');
      sqlite3_create_module := GetProcAddress(Flibsqlite3,'sqlite3_create_module');
      sqlite3_create_module_v2 := GetProcAddress(Flibsqlite3,'sqlite3_create_module_v2');
      sqlite3_declare_vtab := GetProcAddress(Flibsqlite3,'sqlite3_declare_vtab');
      sqlite3_overload_function := GetProcAddress(Flibsqlite3,'sqlite3_overload_function');
      sqlite3_blob_open := GetProcAddress(Flibsqlite3,'sqlite3_blob_open');
      sqlite3_blob_close := GetProcAddress(Flibsqlite3,'sqlite3_blob_close');
      sqlite3_blob_bytes := GetProcAddress(Flibsqlite3,'sqlite3_blob_bytes');
      sqlite3_blob_read := GetProcAddress(Flibsqlite3,'sqlite3_blob_read');
      sqlite3_blob_write := GetProcAddress(Flibsqlite3,'sqlite3_blob_write');
      sqlite3_vfs_find := GetProcAddress(Flibsqlite3,'sqlite3_vfs_find');
      sqlite3_vfs_register := GetProcAddress(Flibsqlite3,'sqlite3_vfs_register');
      sqlite3_vfs_unregister := GetProcAddress(Flibsqlite3,'sqlite3_vfs_unregister');
      sqlite3_mutex_alloc := GetProcAddress(Flibsqlite3,'sqlite3_mutex_alloc');
      sqlite3_mutex_free := GetProcAddress(Flibsqlite3,'sqlite3_mutex_free');
      sqlite3_mutex_enter := GetProcAddress(Flibsqlite3,'sqlite3_mutex_enter');
      sqlite3_mutex_try := GetProcAddress(Flibsqlite3,'sqlite3_mutex_try');
      sqlite3_mutex_leave := GetProcAddress(Flibsqlite3,'sqlite3_mutex_leave');
      sqlite3_db_mutex := GetProcAddress(Flibsqlite3,'sqlite3_db_mutex');
      sqlite3_file_control := GetProcAddress(Flibsqlite3,'sqlite3_file_control');
      sqlite3_test_control := GetProcAddress(Flibsqlite3,'sqlite3_test_control');
      sqlite3_status := GetProcAddress(Flibsqlite3,'sqlite3_status');
      sqlite3_db_status := GetProcAddress(Flibsqlite3,'sqlite3_db_status');
      sqlite3_stmt_status := GetProcAddress(Flibsqlite3,'sqlite3_stmt_status');
      sqlite3_backup_init := GetProcAddress(Flibsqlite3,'sqlite3_backup_init');
      sqlite3_backup_step := GetProcAddress(Flibsqlite3,'sqlite3_backup_step');
      sqlite3_backup_finish := GetProcAddress(Flibsqlite3,'sqlite3_backup_finish');
      sqlite3_backup_remaining := GetProcAddress(Flibsqlite3,'sqlite3_backup_remaining');
      sqlite3_backup_pagecount := GetProcAddress(Flibsqlite3,'sqlite3_backup_pagecount');
      sqlite3_strnicmp := GetProcAddress(Flibsqlite3,'sqlite3_strnicmp');


      Result := assigned(sqlite3_libversion) and
                assigned(sqlite3_sourceid) and
                assigned(sqlite3_libversion_number) and
                assigned(sqlite3_threadsafe) and
                assigned(sqlite3_close) and
                assigned(sqlite3_exec) and
                assigned(sqlite3_initialize) and
                assigned(sqlite3_shutdown) and
                assigned(sqlite3_os_init) and
                assigned(sqlite3_os_end) and
                assigned(sqlite3_config) and
                assigned(sqlite3_db_config) and
                assigned(sqlite3_extended_result_codes) and
                assigned(sqlite3_last_insert_rowid) and
                assigned(sqlite3_changes) and
                assigned(sqlite3_total_changes) and
                assigned(sqlite3_interrupt) and
                assigned(sqlite3_complete) and
                assigned(sqlite3_complete16) and
                assigned(sqlite3_busy_handler) and
                assigned(sqlite3_busy_timeout) and
                assigned(sqlite3_get_table) and
                assigned(sqlite3_free_table) and
                assigned(sqlite3_mprintf) and
                assigned(sqlite3_vmprintf) and
                assigned(sqlite3_snprintf) and
                assigned(sqlite3_malloc) and
                assigned(sqlite3_realloc) and
                assigned(sqlite3_free) and
                assigned(sqlite3_memory_used) and
                assigned(sqlite3_memory_highwater) and
                assigned(sqlite3_randomness) and
                assigned(sqlite3_set_authorizer) and
                assigned(sqlite3_trace) and
                assigned(sqlite3_profile) and
                assigned(sqlite3_progress_handler) and
                assigned(sqlite3_open) and
                assigned(sqlite3_open16) and
                assigned(sqlite3_open_v2) and
                assigned(sqlite3_errcode) and
                assigned(sqlite3_extended_errcode) and
                assigned(sqlite3_errmsg) and
                assigned(sqlite3_errmsg16) and
                assigned(sqlite3_limit) and
                assigned(sqlite3_prepare) and
                assigned(sqlite3_prepare_v2) and
                assigned(sqlite3_prepare16) and
                assigned(sqlite3_prepare16_v2) and
                assigned(sqlite3_sql) and
                assigned(sqlite3_bind_blob) and
                assigned(sqlite3_bind_double) and
                assigned(sqlite3_bind_int) and
                assigned(sqlite3_bind_int64) and
                assigned(sqlite3_bind_null) and
                assigned(sqlite3_bind_text) and
                assigned(sqlite3_bind_text16) and
                assigned(sqlite3_bind_value) and
                assigned(sqlite3_bind_zeroblob) and
                assigned(sqlite3_bind_parameter_count) and
                assigned(sqlite3_bind_parameter_name) and
                assigned(sqlite3_bind_parameter_index) and
                assigned(sqlite3_clear_bindings) and
                assigned(sqlite3_column_count) and
                assigned(sqlite3_column_name) and
                assigned(sqlite3_column_name16) and
                assigned(sqlite3_column_database_name) and
                assigned(sqlite3_column_database_name16) and
                assigned(sqlite3_column_table_name) and
                assigned(sqlite3_column_table_name16) and
                assigned(sqlite3_column_origin_name) and
                assigned(sqlite3_column_origin_name16) and
                assigned(sqlite3_column_decltype) and
                assigned(sqlite3_column_decltype16) and
                assigned(sqlite3_step) and
                assigned(sqlite3_data_count) and
                assigned(sqlite3_column_blob) and
                assigned(sqlite3_column_bytes) and
                assigned(sqlite3_column_bytes16) and
                assigned(sqlite3_column_double) and
                assigned(sqlite3_column_int) and
                assigned(sqlite3_column_int64) and
                assigned(sqlite3_column_text) and
                assigned(sqlite3_column_text16) and
                assigned(sqlite3_column_type) and
                assigned(sqlite3_column_value) and
                assigned(sqlite3_finalize) and
                assigned(sqlite3_reset) and
                assigned(sqlite3_create_function) and
                assigned(sqlite3_create_function16) and
                assigned(sqlite3_value_blob) and
                assigned(sqlite3_value_bytes) and
                assigned(sqlite3_value_bytes16) and
                assigned(sqlite3_value_double) and
                assigned(sqlite3_value_int) and
                assigned(sqlite3_value_int64) and
                assigned(sqlite3_value_text) and
                assigned(sqlite3_value_text16) and
                assigned(sqlite3_value_text16le) and
                assigned(sqlite3_value_text16be) and
                assigned(sqlite3_value_type) and
                assigned(sqlite3_value_numeric_type) and
                assigned(sqlite3_aggregate_context) and
                assigned(sqlite3_user_data) and
                assigned(sqlite3_context_db_handle) and
                assigned(sqlite3_get_auxdata) and
                assigned(sqlite3_set_auxdata) and
                assigned(sqlite3_result_blob) and
                assigned(sqlite3_result_double) and
                assigned(sqlite3_result_error) and
                assigned(sqlite3_result_error16) and
                assigned(sqlite3_result_error_toobig) and
                assigned(sqlite3_result_error_nomem) and
                assigned(sqlite3_result_error_code) and
                assigned(sqlite3_result_int) and
                assigned(sqlite3_result_int64) and
                assigned(sqlite3_result_null) and
                assigned(sqlite3_result_text) and
                assigned(sqlite3_result_text16) and
                assigned(sqlite3_result_text16le) and
                assigned(sqlite3_result_text16be) and
                assigned(sqlite3_result_value) and
                assigned(sqlite3_result_zeroblob) and
                assigned(sqlite3_create_collation) and
                assigned(sqlite3_create_collation_v2) and
                assigned(sqlite3_create_collation16) and
                assigned(sqlite3_collation_needed) and
                assigned(sqlite3_collation_needed16) and
                assigned(sqlite3_sleep) and
                assigned(sqlite3_get_autocommit) and
                assigned(sqlite3_db_handle) and
                assigned(sqlite3_next_stmt) and
                assigned(sqlite3_commit_hook) and
                assigned(sqlite3_rollback_hook) and
                assigned(sqlite3_update_hook) and
                assigned(sqlite3_enable_shared_cache) and
                assigned(sqlite3_release_memory) and
                assigned(sqlite3_soft_heap_limit64) and
                assigned(sqlite3_table_column_metadata) and
                assigned(sqlite3_load_extension) and
                assigned(sqlite3_enable_load_extension) and
                assigned(sqlite3_auto_extension) and
                assigned(sqlite3_reset_auto_extension) and
                assigned(sqlite3_create_module) and
                assigned(sqlite3_create_module_v2) and
                assigned(sqlite3_declare_vtab) and
                assigned(sqlite3_overload_function) and
                assigned(sqlite3_blob_open) and
                assigned(sqlite3_blob_close) and
                assigned(sqlite3_blob_bytes) and
                assigned(sqlite3_blob_read) and
                assigned(sqlite3_blob_write) and
                assigned(sqlite3_vfs_find) and
                assigned(sqlite3_vfs_register) and
                assigned(sqlite3_vfs_unregister) and
                assigned(sqlite3_mutex_alloc) and
                assigned(sqlite3_mutex_free) and
                assigned(sqlite3_mutex_enter) and
                assigned(sqlite3_mutex_try) and
                assigned(sqlite3_mutex_leave) and
                assigned(sqlite3_db_mutex) and
                assigned(sqlite3_file_control) and
                assigned(sqlite3_test_control) and
                assigned(sqlite3_status) and
                assigned(sqlite3_db_status) and
                assigned(sqlite3_stmt_status) and
                assigned(sqlite3_backup_init) and
                assigned(sqlite3_backup_step) and
                assigned(sqlite3_backup_finish) and
                assigned(sqlite3_backup_remaining) and
                assigned(sqlite3_backup_pagecount) and
                assigned(sqlite3_strnicmp);

      if not Result then begin
        Unload;
        raise Exception.Create(cALSqlite3_INVALIDELIBVERSION);
      end
      else if initialize then begin
        if sqlite3_initialize <> SQLITE_OK then begin
          Unload;
          raise Exception.CreateFmt(cALSQLite3_CANTLOADLIB, [lib]);
        end;
      end;
    end
    else raise Exception.CreateFmt(cALSqlite3_CANTLOADLIB, [lib]);
  end;
end;

end.

