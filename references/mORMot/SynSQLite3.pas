/// SQLite3 Database engine direct access
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSQLite3;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Alfred Glaenzer (alf)
  - Eric Grange
  - Vaclav

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


     SQLite3 3.34.1 database engine
    ********************************

   Brand new SQLite3 library to be used with Delphi/FPC
  - FLEXIBLE: in process, local or remote access (JSON RESTFUL HTTP server)
  - STANDARD: full UTF-8 and Unicode, SQLite3 engine (enhanced but not hacked)
  - SECURE: tested, multi-thread oriented, atomic commit, encryption ready
  - SIMPLE: staticaly linked into a single Delphi unit (via SynSQLite3Static)
    or standard external dll - via TSQLite3LibraryDynamic
  - LIGHT: use native classes, not TDataSet nor TDataSource
  - SMART: queries share a JSON-based memory cache for immediate response
  - FAST: tuned pascal and i386 assembler code with use of FastMM4/SynScaleMM
  - FREE: full source code provided, with permissive licence

  - unitary tested with provided regression tests
  - includes RTREE extension for doing very fast range queries
  - can include FTS3 full text search engine (MATCH operator) after sqlite3.c
    recompile (by default, FTS3 is not compiled, saving more than 50KB of code)
  - uses only newest API (sqlite3_prepare_v2) and follow SQLite3 official documentation
  - uses purely UTF-8 encoded strings: Ansi/Unicode conversion routines included,
    Delphi 2009 ready (but Unicode works very well with older Delphi versions)
  - optional on the fly fast encryption of the data on disk
  - use an optional and efficient caching mechanism (TSynCache based) for the
    most used SELECT statements, using our TSQLTableJSON as fast data source
  - record retrieval from its ID is speed up via SQL statement preparation
  - uses ISO 8601:2004 format to properly handle date/time values in TEXT field
  - can be easily updated from official SQLite3 source code (see comments in
    the source code of this unit)
  - compiled without thread mutex: the caller has to be thread-safe aware
    (this is faster on most configuration, since mutex has to be acquired once):
    low level sqlite3_*() functions are not thread-safe, as TSQLRequest and
    TSQLBlobStream which just wrap them; but TSQLDataBase is thread-safe, as
    mORMot's TSQLTableDB/TSQLRestServerDB/TSQLRestClientDB which use TSQLDataBase
  - compiled with SQLITE_OMIT_SHARED_CACHE define
  - compatible with our LVCL 'Very LIGHT VCL routines' framework
    for building light but fast GUI servers software

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$ifdef FPC}
  dynlibs,
  {$endif}
  {$else}
  {$ifdef KYLIX3}
  LibC,
  SyncObjs,
  SynKylix,
  {$endif}
  {$ifdef FPC}
  {$ifdef Linux}
  SynFPCLinux,
  {$ifdef BSDNOTDARWIN}
  dl,
  {$else}
  DynLibs,
  {$endif}
  {$endif}
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  {$ifndef LVCL}
  Contnrs,
  {$endif}
  SynCommons,
  SynTable,
  SynLog;


{ ************ direct access to sqlite3.c / sqlite3.obj consts and functions }

{$ifdef BSD}
  {$linklib c}
  {$linklib pthread}
{$endif}

{$ifdef FPC}
  {$packrecords C}
  {$packenum 4}
{$endif}

type
  /// internaly store the SQLite3 database handle
  TSQLite3DB = type PtrUInt;

  /// internaly store the SQLite3 statement handle
  // - This object is variously known as a "prepared statement" or a "compiled
  // SQL statement" or simply as a "statement".
  // - Create the object using sqlite3.prepare_v2() or a related function.
  // - Bind values to host parameters using the sqlite3.bind_*() interfaces.
  // - Run the SQL by calling sqlite3.step() one or more times.
  // - Reset the statement using sqlite3.reset() then go back to "Bind" step.
  //  Do this zero or more times.
  // - Destroy the object using sqlite3.finalize().
  TSQLite3Statement = type PtrUInt;

  /// internaly store the SQLite3 blob handle
  TSQLite3Blob = type PtrUInt;

  /// internaly store a SQLite3 Dynamically Typed Value Object
  // - SQLite uses the sqlite3.value object to represent all values that
  // can be stored in a database table, which are mapped to this TSQLite3Value type
  // - SQLite uses dynamic typing for the values it stores
  // - Values stored in sqlite3.value objects can be integers, floating point
  // values, strings, BLOBs, or NULL
  TSQLite3Value = type PtrUInt;

  /// internal store a SQLite3 Function Context Object
  // - The context in which an SQL function executes is stored in an sqlite3.context
  // object, which is mapped to this TSQLite3FunctionContext type
  // - A pointer to an sqlite3.context object is always first parameter to
  // application-defined SQL functions, i.e. a TSQLFunctionFunc prototype
  TSQLite3FunctionContext = type PtrUInt;

  /// internaly store a SQLite3 Backup process handle
  TSQLite3Backup = type PtrUInt;

  /// internaly store of SQLite3 values, as used by TSQLFunctionFunc
  TSQLite3ValueArray = array[0..63] of TSQLite3Value;

const
  {$ifdef MSWINDOWS}
  {$ifdef CPU64}
  // see https://synopse.info/files/SQLite3-64.7z
  SQLITE_LIBRARY_DEFAULT_NAME = 'sqlite3-64.dll';
  {$else}
  SQLITE_LIBRARY_DEFAULT_NAME = 'sqlite3.dll';
  {$endif}
  {$else}
  {$ifdef Linux}
    {$ifdef Android}
    SQLITE_LIBRARY_DEFAULT_NAME = 'libsqlite.so';
    {$else}
      {$ifdef Darwin}
      SQLITE_LIBRARY_DEFAULT_NAME = 'libsqlite3.dylib';
      {$else}
      SQLITE_LIBRARY_DEFAULT_NAME = 'libsqlite3.so.0';
      {$endif}
    {$endif}
  {$else}
  SQLITE_LIBRARY_DEFAULT_NAME = 'libsqlite3.so';
  {$endif}
  {$endif}

  /// internal SQLite3 type as Integer
  SQLITE_INTEGER = 1;
  /// internal SQLite3 type as Floating point value
  SQLITE_FLOAT = 2;
  /// internal SQLite3 type as Text
  SQLITE_TEXT = 3;
  /// internal SQLite3 type as Blob
  SQLITE_BLOB = 4;
  /// internal SQLite3 type as NULL
  SQLITE_NULL = 5;

  /// text is UTF-8 encoded
  SQLITE_UTF8     = 1;
  /// text is UTF-16 LE encoded
  SQLITE_UTF16LE  = 2;
  /// text is UTF-16 BE encoded
  SQLITE_UTF16BE  = 3;
  /// text is UTF-16 encoded, using the system native byte order
  SQLITE_UTF16    = 4;
  /// sqlite3.create_function don't care about text encoding
  SQLITE_ANY      = 5;
  /// used by sqlite3.create_collation() only
  SQLITE_UTF16_ALIGNED = 8;


  /// sqlite_exec() return code: no error occured
  SQLITE_OK = 0;
  /// sqlite_exec() return code: SQL error or missing database - legacy generic code
  SQLITE_ERROR = 1;
  /// sqlite_exec() return code: An internal logic error in SQLite
  SQLITE_INTERNAL = 2;
  /// sqlite_exec() return code: Access permission denied
  SQLITE_PERM = 3;
  /// sqlite_exec() return code: Callback routine requested an abort
  SQLITE_ABORT = 4;
  /// sqlite_exec() return code: The database file is locked
  SQLITE_BUSY = 5;
  /// sqlite_exec() return code: A table in the database is locked
  SQLITE_LOCKED = 6;
  /// sqlite_exec() return code: A malloc() failed
  SQLITE_NOMEM = 7;
  /// sqlite_exec() return code: Attempt to write a readonly database
  SQLITE_READONLY = 8;
  /// sqlite_exec() return code: Operation terminated by sqlite3.interrupt()
  SQLITE_INTERRUPT = 9;
  /// sqlite_exec() return code: Some kind of disk I/O error occurred
  SQLITE_IOERR = 10;
  /// sqlite_exec() return code: The database disk image is malformed
  SQLITE_CORRUPT = 11;
  /// sqlite_exec() return code: (Internal Only) Table or record not found
  SQLITE_NOTFOUND = 12;
  /// sqlite_exec() return code: Insertion failed because database is full
  SQLITE_FULL = 13;
  /// sqlite_exec() return code: Unable to open the database file
  SQLITE_CANTOPEN = 14;
  /// sqlite_exec() return code: (Internal Only) Database lock protocol error
  SQLITE_PROTOCOL = 15;
  /// sqlite_exec() return code: Database is empty
  SQLITE_EMPTY = 16;
  /// sqlite_exec() return code: The database schema changed, and unable to be recompiled
  SQLITE_SCHEMA = 17;
  /// sqlite_exec() return code: Too much data for one row of a table
  SQLITE_TOOBIG = 18;
  /// sqlite_exec() return code: Abort due to contraint violation
  SQLITE_CONSTRAINT = 19;
  /// sqlite_exec() return code: Data type mismatch
  SQLITE_MISMATCH = 20;
  /// sqlite_exec() return code: Library used incorrectly
  SQLITE_MISUSE = 21;
  /// sqlite_exec() return code: Uses OS features not supported on host
  SQLITE_NOLFS = 22;
  /// sqlite_exec() return code: Authorization denied
  SQLITE_AUTH = 23;
  /// sqlite_exec() return code: Auxiliary database format error
  SQLITE_FORMAT = 24;
  /// sqlite_exec() return code: 2nd parameter to sqlite3.bind out of range
  SQLITE_RANGE = 25;
  /// sqlite_exec() return code: File opened that is not a database file
  SQLITE_NOTADB = 26;

  /// sqlite3.step() return code: another result row is ready
  SQLITE_ROW = 100;
  /// sqlite3.step() return code: has finished executing
  SQLITE_DONE = 101;

  /// possible error codes for sqlite_exec() and sqlite3.step()
  // - as verified by sqlite3_check()
  SQLITE_ERRORS = [SQLITE_ERROR..SQLITE_ROW-1];

  /// The database is opened in read-only mode
  // - if the database does not already exist, an error is returned
  // - Ok for sqlite3.open_v2()
  SQLITE_OPEN_READONLY = $00000001;
  /// The database is opened for reading and writing if possible, or reading
  // only if the file is write protected by the operating system
  // - In either case the database must already exist, otherwise an error is
  // returned
  // - Ok for sqlite3.open_v2()
  SQLITE_OPEN_READWRITE = $00000002;
  /// In conjunction with SQLITE_OPEN_READWRITE, optionally create the database
  // file if it does not exist
  // - The database is opened for reading and writing if possible, or reading
  // only if the file is write protected by the operating system
  // - In either case the database must already exist, otherwise an error is returned
  SQLITE_OPEN_CREATE = $00000004;
  /// URI filename interpretation is enabled if the SQLITE_OPEN_URI flag is set
  // in the fourth argument to sqlite3.open_v2(), or if it has been enabled
  // globally using the SQLITE_CONFIG_URI option with the sqlite3.config() method
  // or by the SQLITE_USE_URI compile-time option.
  // - As of SQLite version 3.7.7, URI filename interpretation is turned off by
  // default, but future releases of SQLite might enable URI filename
  // interpretation by default
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_URI = $00000040;  // Ok for sqlite3_open_v2()
  /// If the SQLITE_OPEN_NOMUTEX flag is set, then the database will remain in
  // memory
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_MEMORY = $00000080;  // Ok for sqlite3_open_v2()
  /// If the SQLITE_OPEN_NOMUTEX flag is set, then the database connection opens
  // in the multi-thread threading mode as long as the single-thread mode has
  // not been set at compile-time or start-time
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_NOMUTEX = $00008000;  // Ok for sqlite3_open_v2()
  /// If the SQLITE_OPEN_FULLMUTEX flag is set then the database connection opens
  // in the serialized threading mode unless single-thread was previously selected
  // at compile-time or start-time
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_FULLMUTEX = $00010000;  // Ok for sqlite3_open_v2()
  /// The SQLITE_OPEN_SHAREDCACHE flag causes the database connection to be
  // eligible to use shared cache mode, regardless of whether or not shared
  // cache is enabled using sqlite3.enable_shared_cache()
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_SHAREDCACHE = $00020000;  // Ok for sqlite3_open_v2()
  /// The SQLITE_OPEN_PRIVATECACHE flag causes the database connection to not
  // participate in shared cache mode even if it is enabled
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_PRIVATECACHE = $00040000;

{
  SQLITE_OPEN_DELETEONCLOSE  = $00000008;  // VFS only
  SQLITE_OPEN_EXCLUSIVE      = $00000010;  // VFS only
  SQLITE_OPEN_AUTOPROXY      = $00000020;  // VFS only
  SQLITE_OPEN_MAIN_DB        = $00000100;  // VFS only
  SQLITE_OPEN_TEMP_DB        = $00000200;  // VFS only
  SQLITE_OPEN_TRANSIENT_DB   = $00000400;  // VFS only
  SQLITE_OPEN_MAIN_JOURNAL   = $00000800;  // VFS only
  SQLITE_OPEN_TEMP_JOURNAL   = $00001000;  // VFS only
  SQLITE_OPEN_SUBJOURNAL     = $00002000;  // VFS only
  SQLITE_OPEN_MASTER_JOURNAL = $00004000;  // VFS only
  SQLITE_OPEN_WAL            = $00080000;  // VFS only
}

  /// DestroyPtr set to SQLITE_STATIC if data is constant and will never change
  // - SQLite assumes that the text or BLOB result is in constant space and
  // does not copy the content of the parameter nor call a destructor on the
  // content when it has finished using that result
  SQLITE_STATIC = pointer(0);

  /// DestroyPtr set to SQLITE_TRANSIENT for SQLite3 to make a private copy of
  // the data into space obtained from from sqlite3.malloc() before it returns
  // - this is the default behavior in our framework
  // - note that we discovered that under Win64, sqlite3.result_text() expects
  // SQLITE_TRANSIENT_VIRTUALTABLE=pointer(integer(-1)) and not pointer(-1)
  SQLITE_TRANSIENT = pointer(-1);

  /// DestroyPtr set to SQLITE_TRANSIENT_VIRTUALTABLE for setting results to
  // SQlite3 virtual tables columns
  // - due to a bug of the SQlite3 engine under Win64
  SQLITE_TRANSIENT_VIRTUALTABLE = pointer(integer(-1));

  /// pseudo database file name used to create an in-memory database
  // - an SQLite database is normally stored in a single ordinary disk file -
  // however, in certain circumstances, the database might be stored in memory,
  // if you pass SQLITE_MEMORY_DATABASE_NAME to TSQLDatabase.Create() instead of
  // a real disk file name
  // - this instance will cease to exist as soon as the database connection
  // is closed, i.e. when calling TSQLDatabase.Free
  // - every ':memory:' database is distinct from every other - so, creating two
  // TSQLDatabase instances each with the filename SQLITE_MEMORY_DATABASE_NAME
  //  will create two independent in-memory databases
  SQLITE_MEMORY_DATABASE_NAME = ':memory:';

  SQLITE_CONFIG_SINGLETHREAD = 1;
  SQLITE_CONFIG_MULTITHREAD = 2;
  SQLITE_CONFIG_SERIALIZED = 3;
  SQLITE_CONFIG_MALLOC = 4;
  SQLITE_CONFIG_GETMALLOC = 5;
  SQLITE_CONFIG_SCRATCH = 6;
  SQLITE_CONFIG_PAGECACHE = 7;
  SQLITE_CONFIG_HEAP = 8;
  SQLITE_CONFIG_MEMSTATUS = 9;
  SQLITE_CONFIG_MUTEX = 10;
  SQLITE_CONFIG_GETMUTEX = 11;
  SQLITE_CONFIG_LOOKASIDE = 13;
  SQLITE_CONFIG_PCACHE = 14;
  SQLITE_CONFIG_GETPCACHE = 15;
  SQLITE_CONFIG_LOG = 16;
  SQLITE_CONFIG_URI = 17;
  SQLITE_CONFIG_PCACHE2 = 18;
  SQLITE_CONFIG_GETPCACHE2 = 19;
  SQLITE_CONFIG_COVERING_INDEX_SCAN = 20;
  SQLITE_CONFIG_SQLLOG = 21;
  SQLITE_CONFIG_MMAP_SIZE = 22;
  SQLITE_CONFIG_WIN32_HEAPSIZE = 23;

  SQLITE_DBCONFIG_LOOKASIDE = 1001;
  SQLITE_DBCONFIG_ENABLE_FKEY = 1002;
  SQLITE_DBCONFIG_ENABLE_TRIGGER = 1003;
  SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER = 1004;
  SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION = 1005;

type
  /// type for a custom destructor for the text or BLOB content
  // - set to @sqlite3InternalFree if a Value must be released via Freemem()
  // - set to @sqlite3InternalFreeObject if a Value must be released via
  // TObject(p).Free
  TSQLDestroyPtr = procedure(p: pointer); cdecl;

  /// SQLite3 collation (i.e. sort and comparison) function prototype
  // - this function MUST use s1Len and s2Len parameters during the comparison:
  // s1 and s2 are not zero-terminated
  // - used by sqlite3.create_collation low-level function
  TSQLCollateFunc = function(CollateParam: pointer; s1Len: integer; s1: pointer;
    s2Len: integer; s2: pointer) : integer; cdecl;

  /// SQLite3 user function or aggregate callback prototype
  // - argc is the number of supplied parameters, which are available in argv[]
  // (you can call ErrorWrongNumberOfArgs(Context) in case of unexpected number)
  // - use sqlite3.value_*(argv[*]) functions to retrieve a parameter value
  // - then set the result using sqlite3.result_*(Context,*) functions
  TSQLFunctionFunc = procedure(Context: TSQLite3FunctionContext;
    argc: integer; var argv: TSQLite3ValueArray); cdecl;

  /// SQLite3 user final aggregate callback prototype
  TSQLFunctionFinal = procedure(Context: TSQLite3FunctionContext); cdecl;

  /// SQLite3 callback prototype to handle SQLITE_BUSY errors
  // - The first argument to the busy handler is a copy of the user pointer which
  // is the third argument to sqlite3.busy_handler().
  // - The second argument to the busy handler callback is the number of times
  // that the busy handler has been invoked for this locking event.
  // - If the busy callback returns 0, then no additional attempts are made to
  // access the database and SQLITE_BUSY or SQLITE_IOERR_BLOCKED is returned.
  // - If the callback returns non-zero, then another attempt is made to open
  // the database for reading and the cycle repeats.
  TSQLBusyHandler = function(user: pointer; count: integer): integer;
     cdecl;

  PFTSMatchInfo = ^TFTSMatchInfo;
  /// map the matchinfo function returned BLOB value
  // - i.e. the default 'pcx' layout, for both FTS3 and FTS4
  // - see http://www.sqlite.org/fts3.html#matchinfo
  // - used for the FTS3/FTS4 ranking of results by TSQLRest.FTSMatch method
  // and the internal RANK() function as proposed in
  // http://www.sqlite.org/fts3.html#appendix_a
  TFTSMatchInfo = packed record
    nPhrase: integer;
    nCol: integer;
    hits: array[1..9] of record
      this_row: integer;
      all_rows: integer;
      docs_with_hits: integer;
    end;
  end;

  PSQLite3Module = ^TSQLite3Module;
  PSQLite3VTab = ^TSQLite3VTab;
  PSQLite3VTabCursor = ^TSQLite3VTabCursor;

  /// records WHERE clause constraints of the form "column OP expr"
  // - Where "column" is a column in the virtual table, OP is an operator like
  // "=" or "<", and EXPR is an arbitrary expression
  // - So, for example, if the WHERE clause contained a term like this:
  // $ a = 5
  // Then one of the constraints would be on the "a" column with operator "="
  // and an expression of "5"
  // - For example, if the WHERE clause contained something like this:
  // $  x BETWEEN 10 AND 100 AND 999>y
  // The query optimizer might translate this into three separate constraints:
  // ! x >= 10
  // ! x <= 100
  // ! y < 999
  TSQLite3IndexConstraint = record
    /// Column on left-hand side of constraint
    // - The first column of the virtual table is column 0
    // - The ROWID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index.
    iColumn: integer;
    /// Constraint operator
    // - OP is =, <, <=, >, or >= using one of the SQLITE_INDEX_CONSTRAINT_* values
    op: byte;
    /// True if this constraint is usable
    // - The aConstraint[] array contains information about all constraints that
    // apply to the virtual table. But some of the constraints might not be usable
    // because of the way tables are ordered in a join. The xBestIndex method
    // must therefore only consider constraints that have a usable flag which is
    // true, and just ignore contraints with usable set to false
    usable: bytebool;
    /// Used internally - xBestIndex() should ignore this field
    iTermOffset: integer;
  end;
  PSQLite3IndexConstraintArray = ^TSQLite3IndexConstraintArray;
  TSQLite3IndexConstraintArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraint)-1] of TSQLite3IndexConstraint;

  /// ORDER BY clause, one item per column
  TSQLite3IndexOrderBy = record
    /// Column number
    // - The first column of the virtual table is column 0
    // - The ROWID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index.
    iColumn: integer;
    /// True for DESC.  False for ASC.
    desc: bytebool;
  end;
  PSQLite3IndexOrderByArray = ^TSQLite3IndexOrderByArray;
  TSQLite3IndexOrderByArray = array[0..MaxInt div SizeOf(TSQLite3IndexOrderBy)-1] of TSQLite3IndexOrderBy;

  /// define what information is to be passed to xFilter() for a given WHERE
  // clause constraint of the form "column OP expr"
  TSQLite3IndexConstraintUsage = record
    /// If argvIndex>0 then the right-hand side of the corresponding
    // aConstraint[] is evaluated and becomes the argvIndex-th entry in argv
    // - Exactly one entry should be set to 1, another to 2, another to 3, and
    // so forth up to as many or as few as the xBestIndex() method wants.
    // - The EXPR of the corresponding constraints will then be passed in as
    // the argv[] parameters to xFilter()
    // - For example, if the aConstraint[3].argvIndex is set to 1, then when
    // xFilter() is called, the argv[0] passed to xFilter will have the EXPR
    // value of the aConstraint[3] constraint.
    argvIndex: Integer;
    /// If omit is true, then the constraint is assumed to be fully handled
    // by the virtual table and is not checked again by SQLite
    // - By default, the SQLite core double checks all constraints on each
    // row of the virtual table that it receives. If such a check is redundant,
    // xBestFilter() method can suppress that double-check by setting this field
    omit: bytebool;
  end;
  PSQLite3IndexConstraintUsageArray = ^TSQLite3IndexConstraintUsageArray;
  TSQLite3IndexConstraintUsageArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraintUsage) - 1] of TSQLite3IndexConstraintUsage;

  /// Structure used as part of the virtual table interface to pass information
  // into and receive the reply from the xBestIndex() method of a virtual table module
  // - Outputs fields will be passed as parameter to the xFilter() method, and
  // will be initialized to zero by SQLite
  // - For instance, xBestIndex() method fills the idxNum and idxStr fields with
  // information that communicates an indexing strategy to the xFilter method.
  // The information in idxNum and idxStr is arbitrary as far as the SQLite core
  // is concerned. The SQLite core just copies the information through to the
  // xFilter() method. Any desired meaning can be assigned to idxNum and idxStr
  // as long as xBestIndex() and xFilter() agree on what that meaning is.
  // Use the SetInfo() method of this object in order to make a temporary copy
  // of any needed data.
  TSQLite3IndexInfo = record
    /// input: Number of entries in aConstraint array
    nConstraint: integer;
    /// input: List of WHERE clause constraints of the form "column OP expr"
    aConstraint: PSQLite3IndexConstraintArray;
    /// input: Number of terms in the aOrderBy array
    nOrderBy: integer;
    /// input: List of ORDER BY clause, one per column
    aOrderBy: PSQLite3IndexOrderByArray;
    /// output: filled by xBestIndex() method with information about what
    // parameters to pass to xFilter() method
    // - has the same number of items than the aConstraint[] array
    // - should set the aConstraintUsage[].argvIndex to have the corresponding
    // argument in xFilter() argc/argv[] expression list
    aConstraintUsage: PSQLite3IndexConstraintUsageArray;
    /// output: Number used to identify the index
    idxNum: integer;
    /// output: String, possibly obtained from sqlite3.malloc()
    // - may contain any variable-length data or class/record content, as
    // necessary
    idxStr: PAnsiChar;
    /// output: Free idxStr using sqlite3.free() if true (=1)
    needToFreeIdxStr: integer;
    /// output: True (=1) if output is already ordered
    // - i.e. if the virtual table will output rows in the order specified
    // by the ORDER BY clause
    // - if False (=0), will indicate to the SQLite core that it will need to
    // do a separate sorting pass over the data after it comes out
    // of the virtual table
    orderByConsumed: integer;
    /// output: Estimated cost of using this index
    // - Should be set to the estimated number of disk access operations
    // required to execute this query against the virtual table
    // - The SQLite core will often call xBestIndex() multiple times with
    // different constraints, obtain multiple cost estimates, then choose the
    // query plan that gives the lowest estimate
    estimatedCost: Double;
    /// output: Estimated number of rows returned  (since 3.8.2)
    // - may be set to an estimate of the number of rows returned by the
    // proposed query plan. If this value is not explicitly set, the default
    // estimate of 25 rows is used
    estimatedRows: Int64;
    /// output: Mask of SQLITE_INDEX_SCAN_* flags  (since 3.9.0)
    // - may be set to SQLITE_INDEX_SCAN_UNIQUE to indicate that the virtual
    // table will return only zero or one rows given the input constraints.
    // Additional bits of the idxFlags field might be understood in later
    // versions of SQLite
    idxFlags: Integer;
    /// input: Mask of columns used by statement   (since 3.10.0)
    // - indicates which fields of the virtual table are actually used by the
    // statement being prepared. If the lowest bit of colUsed is set, that means
    // that the first column is used. The second lowest bit corresponds to the
    // second column. And so forth. If the most significant bit of colUsed is
    // set, that means that one or more columns other than the first 63 columns
    // are used.
    // - If column usage information is needed by the xFilter method, then the
    // required bits must be encoded into either the idxNum or idxStr output fields
    colUsed: UInt64;
  end;

  /// Virtual Table Instance Object
  // - Every virtual table module implementation uses a subclass of this object
  // to describe a particular instance of the virtual table.
  // - Each subclass will be tailored to the specific needs of the module
  // implementation. The purpose of this superclass is to define certain fields
  // that are common to all module implementations. This structure therefore
  // contains a pInstance field, which will be used to store a class instance
  // handling the virtual table as a pure Delphi class: the TSQLVirtualTableModule
  // class will use it internaly
  TSQLite3VTab = record
    /// The module for this virtual table
    pModule: PSQLite3Module;
    /// no longer used
    nRef: integer;
    /// Error message from sqlite3.mprintf()
    // - Virtual tables methods can set an error message by assigning a string
    // obtained from sqlite3.mprintf() to zErrMsg.
    // - The method should take care that any prior string is freed by a call
    // to sqlite3.free() prior to assigning a new string to zErrMsg.
    // - After the error message is delivered up to the client application,
    // the string will be automatically freed by sqlite3.free() and the zErrMsg
    // field will be zeroed.
    zErrMsg: PUTF8Char;
    /// this will be used to store a Delphi class instance handling the Virtual Table
    pInstance: TObject;
  end;

  /// Virtual Table Cursor Object
  // - Every virtual table module implementation uses a subclass of the following
  // structure to describe cursors that point into the virtual table and are
  // used to loop through the virtual table.
  // - Cursors are created using the xOpen method of the module and are destroyed
  // by the xClose method. Cursors are used by the xFilter, xNext, xEof, xColumn,
  // and xRowid methods of the module.
  // - Each module implementation will define the content of a cursor structure
  // to suit its own needs.
  // - This superclass exists in order to define fields of the cursor that are
  // common to all implementationsThis structure therefore contains a pInstance
  // field, which will be used to store a class instance handling the virtual
  // table as a pure Delphi class: the TSQLVirtualTableModule class will use
  // it internaly
  TSQLite3VTabCursor = record
    /// Virtual table of this cursor
    pVtab: PSQLite3VTab;
    /// this will be used to store a Delphi class instance handling the cursor
    pInstance: TObject;
  end;

  /// defines a module object used to implement a virtual table.
  // - Think of a module as a class from which one can construct multiple virtual
  // tables having similar properties. For example, one might have a module that
  // provides read-only access to comma-separated-value (CSV) files on disk.
  // That one module can then be used to create several virtual tables where each
  // virtual table refers to a different CSV file.
  // - The module structure contains methods that are invoked by SQLite to perform
  // various actions on the virtual table such as creating new instances of a
  // virtual table or destroying old ones, reading and writing data, searching
  // for and deleting, updating, or inserting rows.
  TSQLite3Module = record
    /// defines the particular edition of the module table structure
    // - Currently, handled iVersion is 2, but in future releases of SQLite the
    // module structure definition might be extended with additional methods and
    // in that case the iVersion value will be increased
    iVersion: integer;
    /// called to create a new instance of a virtual table in response to a
    // CREATE VIRTUAL TABLE statement
    // - The job of this method is to construct the new virtual table object (an
    // PSQLite3VTab object) and return a pointer to it in ppVTab
    // - The DB parameter is a pointer to the SQLite database connection that is
    // executing the CREATE VIRTUAL TABLE statement
    // - The pAux argument is the copy of the client data pointer that was the
    // fourth argument to the sqlite3.create_module_v2() call that registered
    // the virtual table module
    // - The argv parameter is an array of argc pointers to null terminated strings
    // - The first string, argv[0], is the name of the module being invoked. The
    // module name is the name provided as the second argument to sqlite3.create_module()
    // and as the argument to the USING clause of the CREATE VIRTUAL TABLE
    // statement that is running.
    // - The second, argv[1], is the name of the database in which the new virtual
    // table is being created. The database name is "main" for the primary
    // database, or "temp" for TEMP database, or the name given at the end of
    // the ATTACH statement for attached databases.
    // - The third element of the array, argv[2], is the name of the new virtual
    // table, as specified following the TABLE keyword in the CREATE VIRTUAL
    // TABLE statement
    // - If present, the fourth and subsequent strings in the argv[] array report
    // the arguments to the module name in the CREATE VIRTUAL TABLE statement
    // - As part of the task of creating a new PSQLite3VTab structure, this method
    // must invoke sqlite3.declare_vtab() to tell the SQLite core about the
    // columns and datatypes in the virtual table
    xCreate: function(DB: TSQLite3DB; pAux: Pointer;
      argc: Integer; const argv: PPUTF8CharArray;
      var ppVTab: PSQLite3VTab; var pzErr: PUTF8Char): Integer; cdecl;
    /// xConnect is called to establish a new connection to an existing virtual table,
    // whereas xCreate is called to create a new virtual table from scratch
    // - It has the same parameters and constructs a new PSQLite3VTab structure
    // - xCreate and xConnect methods are only different when the virtual table
    // has some kind of backing store that must be initialized the first time the
    // virtual table is created. The xCreate method creates and initializes the
    // backing store. The xConnect method just connects to an existing backing store.
    xConnect: function(DB: TSQLite3DB; pAux: Pointer;
      argc: Integer; const argv: PPUTF8CharArray;
      var ppVTab: PSQLite3VTab; var pzErr: PUTF8Char): Integer; cdecl;
    /// Used to determine the best way to access the virtual table
    // - The pInfo parameter is used for input and output parameters
    // - The SQLite core calls the xBestIndex() method when it is compiling a query
    // that involves a virtual table. In other words, SQLite calls this method when
    // it is running sqlite3.prepare() or the equivalent.
    // - By calling this method, the SQLite core is saying to the virtual table
    // that it needs to access some subset of the rows in the virtual table and
    // it wants to know the most efficient way to do that access. The xBestIndex
    // method replies with information that the SQLite core can then use to
    // conduct an efficient search of the virtual table, via the xFilter() method.
    // - While compiling a single SQL query, the SQLite core might call xBestIndex
    // multiple times with different settings in pInfo. The SQLite
    // core will then select the combination that appears to give the best performance.
    // - The information in the pInfo structure is ephemeral and may be overwritten
    // or deallocated as soon as the xBestIndex() method returns. If the
    // xBestIndex() method needs to remember any part of the pInfo structure,
    // it should make a copy. Care must be taken to store the copy in a place
    // where it will be deallocated, such as in the idxStr field with
    // needToFreeIdxStr set to 1.
    xBestIndex: function(var pVTab: TSQLite3VTab; var pInfo: TSQLite3IndexInfo): Integer;
      cdecl;
    /// Releases a connection to a virtual table
    // - Only the pVTab object is destroyed. The virtual table is not destroyed and
    // any backing store associated with the virtual table persists. This method
    // undoes the work of xConnect.
    xDisconnect: function(pVTab: PSQLite3VTab): Integer; cdecl;
    /// Releases a connection to a virtual table, just like the xDisconnect method,
    // and it also destroys the underlying table implementation.
    // - This method undoes the work of xCreate
    // - The xDisconnect method is called whenever a database connection that uses
    // a virtual table is closed. The xDestroy method is only called when a
    // DROP TABLE statement is executed against the virtual table.
    xDestroy: function(pVTab: PSQLite3VTab): Integer; cdecl;
    /// Creates a new cursor used for accessing (read and/or writing) a virtual table
    // - A successful invocation of this method will allocate the memory for the
    // TPSQLite3VTabCursor (or a subclass), initialize the new object, and
    // make ppCursor point to the new object. The successful call then returns SQLITE_OK.
    // - For every successful call to this method, the SQLite core will later
    // invoke the xClose method to destroy the allocated cursor.
    // - The xOpen method need not initialize the pVtab field of the ppCursor structure.
    // The SQLite core will take care of that chore automatically.
    // - A virtual table implementation must be able to support an arbitrary number
    // of simultaneously open cursors.
    // - When initially opened, the cursor is in an undefined state. The SQLite core
    // will invoke the xFilter method on the cursor prior to any attempt to
    // position or read from the cursor.
    xOpen: function(var pVTab: TSQLite3VTab; var ppCursor: PSQLite3VTabCursor): Integer;
      cdecl;
    /// Closes a cursor previously opened by xOpen
    // - The SQLite core will always call xClose once for each cursor opened using xOpen.
    // - This method must release all resources allocated by the corresponding xOpen call.
    // - The routine will not be called again even if it returns an error. The
    // SQLite core will not use the pVtabCursor again after it has been closed.
    xClose: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    /// Begins a search of a virtual table
    // - The first argument is a cursor opened by xOpen.
    // - The next two arguments define a particular search index previously chosen
    // by xBestIndex(). The specific meanings of idxNum and idxStr are unimportant
    // as long as xFilter() and xBestIndex() agree on what that meaning is.
    // - The xBestIndex() function may have requested the values of certain
    // expressions using the aConstraintUsage[].argvIndex values of its pInfo
    // structure. Those values are passed to xFilter() using the argc and argv
    // parameters.
    // - If the virtual table contains one or more rows that match the search criteria,
    // then the cursor must be left point at the first row. Subsequent calls to
    // xEof must return false (zero). If there are no rows match, then the cursor
    // must be left in a state that will cause the xEof to return true (non-zero).
    // The SQLite engine will use the xColumn and xRowid methods to access that row content.
    // The xNext method will be used to advance to the next row.
    // - This method must return SQLITE_OK if successful, or an sqlite error code
    // if an error occurs.
    xFilter: function(var pVtabCursor: TSQLite3VTabCursor; idxNum: Integer; const idxStr: PAnsiChar;
      argc: Integer; var argv: TSQLite3ValueArray): Integer; cdecl;
    /// Advances a virtual table cursor to the next row of a result set initiated by xFilter
    // - If the cursor is already pointing at the last row when this routine is called,
    // then the cursor no longer points to valid data and a subsequent call to the
    // xEof method must return true (non-zero).
    // - If the cursor is successfully advanced to another row of content, then
    // subsequent calls to xEof must return false (zero).
    // - This method must return SQLITE_OK if successful, or an sqlite error code
    // if an error occurs.
    xNext: function(var pVtabCursor: TSQLite3VTabCursor): Integer; cdecl;
    /// Checks if cursor reached end of rows
    // - Must return false (zero) if the specified cursor currently points to a
    // valid row of data, or true (non-zero) otherwise
    xEof: function(var pVtabCursor: TSQLite3VTabCursor): Integer; cdecl;
    /// The SQLite core invokes this method in order to find the value for the
    // N-th column of the current row
    // - N is zero-based so the first column is numbered 0.
    // - The xColumn method may return its result back to SQLite using one of the
    // standard sqlite3.result_*() functions with the specified sContext
    // - If the xColumn method implementation calls none of the sqlite3.result_*()
    // functions, then the value of the column defaults to an SQL NULL.
    // - The xColumn method must return SQLITE_OK on success.
    // - To raise an error, the xColumn method should use one of the result_text()
    // methods to set the error message text, then return an appropriate error code.
    xColumn: function(var pVtabCursor: TSQLite3VTabCursor; sContext: TSQLite3FunctionContext;
      N: Integer): Integer; cdecl;
    /// Should fill pRowid with the rowid of row that the virtual table cursor
    // pVtabCursor is currently pointing at
    xRowid: function(var pVtabCursor: TSQLite3VTabCursor; var pRowid: Int64): Integer;
      cdecl;
    /// Makes a change to a virtual table content (insert/delete/update)
    // - The nArg parameter specifies the number of entries in the ppArg[] array
    // - The value of nArg will be 1 for a pure delete operation or N+2 for an
    // insert or replace or update where N is the number of columns in the table
    // (including any hidden columns)
    // - The ppArg[0] parameter is the rowid of a row in the virtual table to be deleted.
    // If ppArg[0] is an SQL NULL, then no deletion occurs
    // - The ppArg[1] parameter is the rowid of a new row to be inserted into the
    // virtual table. If ppArg[1] is an SQL NULL, then the implementation must
    // choose a rowid for the newly inserted row. Subsequent ppArg[] entries
    // contain values of the columns of the virtual table, in the order that
    // the columns were declared. The number of columns will match the table
    // declaration that the xConnect or xCreate method made using the
    // sqlite3.declare_vtab() call. All hidden columns are included.
    // - When doing an insert without a rowid (nArg>1, ppArg[1] is an SQL NULL),
    // the implementation must set pRowid to the rowid of the newly inserted row;
    // this will become the value returned by the sqlite3.last_insert_rowid()
    // function. Setting this value in all the other cases is a harmless no-op;
    // the SQLite engine ignores the pRowid return value if nArg=1 or ppArg[1]
    // is not an SQL NULL.
    // - Each call to xUpdate() will fall into one of cases shown below. Note
    // that references to ppArg[i] mean the SQL value held within the ppArg[i]
    // object, not the ppArg[i] object itself:
    // $ nArg = 1
    // The single row with rowid equal to ppArg[0] is deleted. No insert occurs.
    // $ nArg > 1
    // $ ppArg[0] = NULL
    // A new row is inserted with a rowid ppArg[1] and column values in ppArg[2]
    // and following. If ppArg[1] is an SQL NULL, the a new unique rowid is
    // generated automatically.
    // $ nArg > 1
    // $ ppArg[0] <> NULL
    // $ ppArg[0] = ppArg[1]
    // The row with rowid ppArg[0] is updated with new values in ppArg[2] and
    // following parameters.
    // $ nArg > 1
    // $ ppArg[0] <> NULL
    // $ ppArg[0] <> ppArg[1]
    // The row with rowid ppArg[0] is updated with rowid ppArg[1] and new values
    // in ppArg[2] and following parameters. This will occur when an SQL statement
    // updates a rowid, as in the statement:
    // $ UPDATE table SET rowid=rowid+1 WHERE ...;
    // - The xUpdate() method must return SQLITE_OK if and only if it is successful.
    // If a failure occurs, the xUpdate() must return an appropriate error code.
    // On a failure, the pVTab.zErrMsg element may optionally be replaced with
    // a custom error message text.
    // - If the xUpdate() method violates some constraint of the virtual table
    // (including, but not limited to, attempting to store a value of the
    // wrong datatype, attempting to store a value that is too large or too small,
    // or attempting to change a read-only value) then the xUpdate() must fail
    // with an appropriate error code.
    // - There might be one or more TSQLite3VTabCursor objects open and in use on
    // the virtual table instance and perhaps even on the row of the virtual
    // table when the xUpdate() method is invoked. The implementation of xUpdate()
    // must be prepared for attempts to delete or modify rows of the table out
    // from other existing cursors. If the virtual table cannot accommodate such
    // changes, the xUpdate() method must return an error code.
    xUpdate: function(var pVTab: TSQLite3VTab; nArg: Integer;
      var ppArg: TSQLite3ValueArray; var pRowid: Int64): Integer; cdecl;
    /// Begins a transaction on a virtual table
    // - This method is always followed by one call to either the xCommit or
    // xRollback method.
    // - Virtual table transactions do not nest, so the xBegin method will not be
    // invoked more than once on a single virtual table without an intervening
    // call to either xCommit or xRollback. For nested transactions, use
    // xSavepoint, xRelease and xRollBackTo methods.
    // - Multiple calls to other methods can and likely will occur in between the
    // xBegin and the corresponding xCommit or xRollback.
    xBegin: function(var pVTab: TSQLite3VTab): Integer; cdecl;
    /// Signals the start of a two-phase commit on a virtual table
    // - This method is only invoked after call to the xBegin method and prior
    // to an xCommit or xRollback.
    // - In order to implement two-phase commit, the xSync method on all virtual
    // tables is invoked prior to invoking the xCommit method on any virtual table.
    // - If any of the xSync methods fail, the entire transaction is rolled back.
    xSync: function(var pVTab: TSQLite3VTab): Integer; cdecl;
    /// Causes a virtual table transaction to commit
    xCommit: function(var pVTab: TSQLite3VTab): Integer; cdecl;
    /// Causes a virtual table transaction to rollback
    xRollback: function(var pVTab: TSQLite3VTab): Integer; cdecl;
    /// Called during sqlite3.prepare() to give the virtual table implementation
    // an opportunity to overload SQL functions
    // - When a function uses a column from a virtual table as its first argument,
    // this method is called to see if the virtual table would like to overload
    // the function. The first three parameters are inputs: the virtual table,
    // the number of arguments to the function, and the name of the function.
    // If no overloading is desired, this method returns 0. To overload the
    // function, this method writes the new function implementation into pxFunc
    // and writes user data into ppArg and returns 1.
    // - Note that infix functions (LIKE, GLOB, REGEXP, and MATCH) reverse the
    // order of their arguments. So "like(A,B)" is equivalent to "B like A".
    // For the form "B like A" the B term is considered the first argument to the
    // function. But for "like(A,B)" the A term is considered the first argument.
    // - The function pointer returned by this routine must be valid for the
    // lifetime of the pVTab object given in the first parameter.
    xFindFunction: function(var pVTab: TSQLite3VTab; nArg: Integer; const zName: PAnsiChar;
      var pxFunc: TSQLFunctionFunc; var ppArg: Pointer): Integer; cdecl;
    /// Provides notification that the virtual table implementation that the
    // virtual table will be given a new name
    // - If this method returns SQLITE_OK then SQLite renames the table.
    // - If this method returns an error code then the renaming is prevented.
    xRename: function(var pVTab: TSQLite3VTab; const zNew: PAnsiChar): Integer;
       cdecl;
    /// Starts a new transaction with the virtual table
    // - SAVEPOINTs are a method of creating transactions, similar to BEGIN and
    // COMMIT, except that the SAVEPOINT and RELEASE commands are named and
    // may be nested. See @http://www.sqlite.org/lang_savepoint.html
    // - iSavepoint parameter indicates the unique name of the SAVEPOINT
    xSavepoint: function(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer;
       cdecl;
    /// Merges a transaction into its parent transaction, so that the specified
    // transaction and its parent become the same transaction
    // - Causes all savepoints back to and including the most recent savepoint
    // with a matching identifier to be removed from the transaction stack
    // - Some people view RELEASE as the equivalent of COMMIT for a SAVEPOINT.
    // This is an acceptable point of view as long as one remembers that the
    // changes committed by an inner transaction might later be undone by a
    // rollback in an outer transaction.
    // - iSavepoint parameter indicates the unique name of the SAVEPOINT
    xRelease: function(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer;
       cdecl;
    /// Reverts the state of the virtual table content back to what it was just
    // after the corresponding SAVEPOINT
    // - iSavepoint parameter indicates the unique name of the SAVEPOINT
    xRollbackTo: function(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer;
       cdecl;
  end;

  /// Compile-Time Authorization Callback prototype
  // - The authorizer callback is invoked as SQL statements are being compiled by
  // sqlite3.prepare2() e.g.
  // - The authorizer callback should return SQLITE_OK to allow the action,
  // SQLITE_IGNORE to disallow the specific action but allow the SQL statement
  // to continue to be compiled, or SQLITE_DENY to cause the entire SQL statement
  // to be rejected with an error.
  // - If the authorizer callback returns any value other than SQLITE_IGNORE,
  // SQLITE_OK, or SQLITE_DENY then the sqlite3.prepare_v2() or equivalent call
  // that triggered the authorizer will fail with an error message.
  // - The first pUserData parameter to the authorizer callback is a copy of the
  // third parameter to the sqlite3.set_authorizer() interface
  // - The second parameter to the callback is an integer action code that
  // specifies the particular action to be authorized:
  // - The third through sixth parameters to the callback are zero-terminated
  // strings that contain additional details about the action to be authorized.
  // - Here is a list of handled code constant, and their associated zTab / zCol
  // parameters:
  // !    const                       zTab            zCol
  // $ SQLITE_CREATE_INDEX          Index Name      Table Name
  // $ SQLITE_CREATE_TABLE          Table Name      nil
  // $ SQLITE_CREATE_TEMP_INDEX     Index Name      Table Name
  // $ SQLITE_CREATE_TEMP_TABLE     Table Name      nil
  // $ SQLITE_CREATE_TEMP_TRIGGER   Trigger Name    Table Name
  // $ SQLITE_CREATE_TEMP_VIEW      View Name       nil
  // $ SQLITE_CREATE_TRIGGER        Trigger Name    Table Name
  // $ SQLITE_CREATE_VIEW           View Name       nil
  // $ SQLITE_DELETE                Table Name      nil
  // $ SQLITE_DROP_INDEX            Index Name      Table Name
  // $ SQLITE_DROP_TABLE            Table Name      nil
  // $ SQLITE_DROP_TEMP_INDEX       Index Name      Table Name
  // $ SQLITE_DROP_TEMP_TABLE       Table Name      nil
  // $ SQLITE_DROP_TEMP_TRIGGER     Trigger Name    Table Name
  // $ SQLITE_DROP_TEMP_VIEW        View Name       nil
  // $ SQLITE_DROP_TRIGGER          Trigger Name    Table Name
  // $ SQLITE_DROP_VIEW             View Name       nil
  // $ SQLITE_INSERT                Table Name      nil
  // $ SQLITE_PRAGMA                Pragma Name     1st arg or nil
  // $ SQLITE_READ                  Table Name      Column Name
  // $ SQLITE_SELECT                nil             nil
  // $ SQLITE_TRANSACTION           Operation       nil
  // $ SQLITE_UPDATE                Table Name      Column Name
  // $ SQLITE_ATTACH                Filename        nil
  // $ SQLITE_DETACH                Database Name   nil
  // $ SQLITE_ALTER_TABLE           Database Name   Table Name
  // $ SQLITE_REINDEX               Index Name      nil
  // $ SQLITE_ANALYZE               Table Name      nil
  // $ SQLITE_CREATE_VTABLE         Table Name      Module Name
  // $ SQLITE_DROP_VTABLE           Table Name      Module Name
  // $ SQLITE_FUNCTION              nil             Function Name
  // $ SQLITE_SAVEPOINT             Operation       Savepoint Name
  // - The 5th parameter to the authorizer callback is the name of the database
  // ('main', 'temp', etc.) if applicable.
  // - The 6th parameter to the authorizer callback is the name of the inner-most
  // trigger or view that is responsible for the access attempt or nil if this
  // access attempt is directly from top-level SQL code.
  TSQLAuthorizerCallback = function(pUserData: Pointer; code: Integer;
    const zTab, zCol, zDb, zAuthContext: PAnsiChar): Integer; cdecl;

  /// Callback function invoked when a row is updated, inserted or deleted,
  // after sqlite3.update_hook() registration
  // - The first pUpdateArg argument is a copy of the third argument to
  // sqlite3.update_hook().
  // - The second op argument is one of SQLITE_INSERT, SQLITE_DELETE, or SQLITE_UPDATE,
  // depending on the operation that caused the callback to be invoked.
  // - The third and fourth zDB / zTbl arguments contain pointers to the database
  // and table name containing the affected row.
  // - The final iRowID parameter is the rowid of the row. In the case of an update,
  // this is the rowid after the update takes place.
  // - The update hook implementation must not do anything that will modify the
  // database connection that invoked the update hook. Any actions to modify the
  // database connection must be deferred until after the completion of the
  // sqlite3.step() call that triggered the update hook. Note that
  // sqlite3.prepare_v2() and sqlite3.step() both modify their database
  // connections for the meaning of "modify" in this paragraph.
  TSQLUpdateCallback = procedure(pUpdateArg: Pointer; op: Integer;
    const zDb, zTbl: PUTF8Char; iRowID: Int64); cdecl;

  /// Commit And Rollback Notification Callback function after
  // sqlite3.commit_hook() or sqlite3.rollback_hook() registration
  // - The callback implementation must not do anything that will modify the
  // database connection that invoked the callback. Any actions to modify the
  // database connection must be deferred until after the completion of the
  // sqlite3.step() call that triggered the commit or rollback hook in the
  // first place. Note that sqlite3.prepare_v2() and sqlite3.step() both modify
  // their database connections for the meaning of "modify" in this paragraph.
  // - When the commit hook callback routine returns zero, the COMMIT operation
  // is allowed to continue normally. If the commit hook returns non-zero, then
  // the COMMIT is converted into a ROLLBACK. The rollback hook is invoked on
  // a rollback that results from a commit hook returning non-zero, just as
  // it would be with any other rollback.
  // - For the purposes of this API, a transaction is said to have been rolled
  // back if an explicit "ROLLBACK" statement is executed, or an error or
  // constraint causes an implicit rollback to occur. The rollback callback
  // is not invoked if a transaction is automatically rolled back because the
  // database connection is closed.
  TSQLCommitCallback = function(pArg: Pointer): Integer; cdecl;


  /// events monitored by sqlite3.trace_v2() tracing logic
  // - stmStmt callback is invoked when a prepared statement first begins
  // running and possibly at other times during the execution of the prepared
  // statement, such as at the start of each trigger subprogram. The P argument
  // is a pointer to the prepared statement. The X argument is a pointer to a
  // string which is the unexpanded SQL text of the prepared statement or an
  // SQL comment that indicates the invocation of a trigger.
  // - stmProfile callback provides approximately the same information as was
  // provided by the deprecated sqlite3.profile() callback. The P argument is
  // a pointer to the prepared statement and the X argument points to a 64-bit
  // integer which is the estimated of the number of nanosecond that the
  // prepared statement took to run. The stmProfile callback is invoked when
  // the statement finishes.
  // - stmRow callback is invoked whenever a prepared statement generates
  // a single row of result. The P argument is a pointer to the prepared
  // statement and the X argument is unused.
  // - stmClose callback is invoked when a database connection closes. The
  // P argument is a pointer to the database connection object and the X
  // argument is unused.
  TSQLTraceMask = set of (stmStmt, stmProfile, stmRow, stmClose);

  /// Callback function registered by sqlite3.trace_v2()
  // - the Trace argument has one of the TSQLTraceMask items set, to indicate
  // why the callback was invoked
  // - UserData argument is a copy of the context pointer, as provided at
  // sqlite3.trace_v2() call
  // - P and X arguments are pointers whose meanings depend on Trace content:
  // see TSQLTraceMask for the various use cases
  TSQLTraceCallback = procedure(Trace: TSQLTraceMask; UserData,P,X: pointer);
    cdecl;

  /// Callback function registered by sqlite3.profile()
  // - this procedure will be invoked as each SQL statement finishes
  // - warning: sqlite3.profile() function is considered experimental and is
  // subject to change in future versions of SQLite
  TSQLProfileCallback = procedure(ProfileArg: Pointer; Profile: PUTF8Char;
    ProfileNanoSeconds: Int64); cdecl;

  /// defines the interface between SQLite and low-level memory allocation routines
  // - as used by sqlite3.config(SQLITE_CONFIG_MALLOC,pMemMethods);
  TSQLite3MemMethods = record
    /// Memory allocation function
    xMalloc: function(size: integer): pointer; cdecl;
    /// Free a prior allocation
    xFree: procedure(ptr: pointer); cdecl;
    /// Resize an allocation
    xRealloc: function(ptr: pointer; size: integer): pointer; cdecl;
    /// Return the size of an allocation
    xSize: function(ptr: pointer): integer; cdecl;
    /// Round up request size to allocation size
    xRoundup: function(size: integer): integer; cdecl;
    /// Initialize the memory allocator
    xInit: function(appData: pointer): integer; cdecl;
    /// Deinitialize the memory allocator
    xShutdown: procedure(appData: pointer); cdecl;
    /// Argument to xInit() and xShutdown()
    pAppData: pointer;
  end;

  {$M+}
  /// wrapper around all SQLite3 library API calls
  // - abstract class allowing direct binding of static sqlite3.obj
  // (TSQLite3LibrayStatic) or with an external library (TSQLite3LibraryDynamic)
  // - a global sqlite3: TSQLite3Library will be defined in this unit, so
  // you should call sqlite3.open() instead of sqlite3_open() for instance
  // - if your project refers to SynSQLite3Static unit, it will initialize a
  // TSQLite3LibrayStatic instance
  TSQLite3Library = class
  protected
    fUseInternalMM: boolean;
    fVersionNumber: cardinal;
    fVersionText: RawUTF8;
    function GetVersion: RawUTF8;
  public
    /// initialize the SQLite3 database code
    // - automaticaly called by the initialization block of this unit
    // - so sqlite3.c is compiled with SQLITE_OMIT_AUTOINIT defined
    initialize: function: integer; cdecl;

    /// shutdown the SQLite3 database core
    // - automaticaly called by the finalization block of this unit
    shutdown: function: integer; cdecl;

    /// Open a SQLite3 database filename, creating a DB handle
    // - filename must be UTF-8 encoded (filenames containing international
    // characters must be converted to UTF-8 prior to passing them)
    // - allocate a sqlite3 object, and return its handle in DB
    // - return SQLITE_OK on success
    // - an error code (see SQLITE_* const) is returned otherwise - sqlite3.errmsg()
    // can be used to obtain an English language description of the error
    // - Whatever or not an error occurs when it is opened, resources associated with
    // the database connection handle should be released by passing it to
    // sqlite3.close() when it is no longer required
    open: function(filename: PUTF8Char; var DB: TSQLite3DB): integer; cdecl;

    /// Open a SQLite3 database filename, creating a DB handle
    // - sqlite3.open_v2() interface works like sqlite3.open() except that it
    // accepts two additional parameters for additional control over the new
    // database connection.
    // - flags parameter to sqlite3.open_v2() can take one of SQLITE_OPEN_READONLY,
    // SQLITE_OPEN_READWRITE or (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
    // values, optionally combined with the SQLITE_OPEN_NOMUTEX,
    // SQLITE_OPEN_FULLMUTEX, SQLITE_OPEN_SHAREDCACHE, SQLITE_OPEN_PRIVATECACHE,
    // and/or SQLITE_OPEN_URI flags
    // - If the flags parameter is not one of the combinations shown above optionally
    // combined with other SQLITE_OPEN_* bits then the behavior is undefined.
    // - The fourth parameter is the name of the sqlite3_vfs object that defines
    // the operating system interface that the new database connection should use.
    // If the fourth parameter is a nil pointer then the default sqlite3_vfs
    // object is used
    open_v2: function(filename: PUTF8Char; var DB: TSQLite3DB; flags: integer;
      zVfszVfs: PUTF8Char): integer; cdecl;

    ///  specify the encryption key on a newly opened database connection
    // - Assigned(key)=false if encryption is not available for this .dll
    // - SynSQLite3Static will use its own internal encryption format
    // - key/keylen may be a JSON-serialized TSynSignerParams object, or will use
    // AES-OFB-128 after SHAKE_128 with rounds=1000 and a fixed salt on plain password text
    key: function(DB: TSQLite3DB; key: pointer; keyLen: Integer): integer; cdecl;

    /// change the encryption key on a database connection that is already opened
    // -  can also decrypt a previously encrypted database (so that it is accessible
    // from any version of SQLite) by specifying a nil key
    // - Assigned(rekey)=false if encryption is not available, i.e. if
    // NOSQLITE3STATIC is defined
    // - also see ChangeSQLEncryptTablePassWord() procedure
    rekey: function(DB: TSQLite3DB; key: pointer; keyLen: Integer): integer; cdecl;

    /// Destructor for the sqlite3 object, which handle is DB
    //  - Applications should finalize all prepared statements and close all BLOB handles
    // associated with the sqlite3 object prior to attempting to close the object
    // (sqlite3.next_stmt() interface can be used for this task)
    // - if invoked while a transaction is open, the transaction is automatically rolled back
    //  - SynSQLite3Static will use its own internal function for handling properly
    // its own encryption format
    close: function(DB: TSQLite3DB): integer; cdecl;

    /// Return the version of the SQLite database engine, in ascii format
    // - currently returns '3.34.1', when used with our SynSQLite3Static unit
    // - if an external SQLite3 library is used, version may vary
    // - you may use the VersionText property (or Version for full details) instead
    libversion: function: PUTF8Char; cdecl;

    /// Returns English-language text that describes an error,
    // using UTF-8 encoding (which, with English text, is the same as Ansi).
    // - Memory to hold the error message string is managed internally.
    // The application does not need to worry about freeing the result.
    // However, the error string might be overwritten or deallocated by
    // subsequent calls to other SQLite interface functions.
    errmsg: function(DB: TSQLite3DB): PUTF8Char; cdecl;

    /// returns the numeric result code or extended result code for the most
    // recent failed sqlite3 API call associated with a database connection
    extended_errcode: function(DB: TSQLite3DB): integer; cdecl;

    /// add SQL functions or aggregates or to redefine the behavior of existing
    // SQL functions or aggregates
    // - The first parameter is the database connection to which the SQL function is
    // to be added. If an application uses more than one database connection then
    // application-defined SQL functions must be added to each database connection
    // separately.
    // - The second parameter is the name of the SQL function to be created or redefined.
    // The length of the name is limited to 255 bytes in a UTF-8 representation,
    // exclusive of the zero-terminator. Note that the name length limit is in
    // UTF-8 bytes, not characters nor UTF-16 bytes. Any attempt to create a
    // function with a longer name will result in SQLITE_MISUSE being returned.
    // - The third parameter (nArg) is the number of arguments that the SQL
    // function or aggregate takes. If this parameter is -1, then the SQL
    // function or aggregate may take any number of arguments between 0 and the
    // SQLITE_LIMIT_FUNCTION_ARG current limit. If the third parameter is less
    // than -1 or greater than 127 then the behavior is undefined.
    // - The fourth parameter, eTextRep, specifies what text encoding this SQL
    // function prefers for its parameters. Every SQL function implementation must
    // be able to work with UTF-8, UTF-16le, or UTF-16be. But some implementations
    // may be more efficient with one encoding than another. When multiple
    // implementations of the same function are available, SQLite will pick the one
    // that involves the least amount of data conversion. If there is only a single
    // implementation which does not care what text encoding is used, then the
    // fourth argument should be SQLITE_ANY.
    // - The fifth parameter, pApp, is an arbitrary pointer. The implementation
    // of the function can gain access to this pointer using sqlite3.user_data().
    // - The seventh, eighth and ninth parameters, xFunc, xStep and xFinal, are
    // pointers to C-language functions that implement the SQL function or aggregate.
    // A scalar SQL function requires an implementation of the xFunc callback only;
    // nil pointers must be passed as the xStep and xFinal parameters. An aggregate
    // SQL function requires an implementation of xStep and xFinal and nil pointer
    // must be passed for xFunc. To delete an existing SQL function or aggregate,
    // pass nil pointers for all three function callbacks.
    // - It is permitted to register multiple implementations of the same functions
    // with the same name but with either differing numbers of arguments or
    // differing preferred text encodings. SQLite will use the implementation
    // that most closely matches the way in which the SQL function is used.
    create_function: function(DB: TSQLite3DB; FunctionName: PUTF8Char;
      nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSQLFunctionFunc;
      xFinal: TSQLFunctionFinal): Integer; cdecl;

    /// add SQL functions or aggregates or to redefine the behavior of existing
    // SQL functions or aggregates, including destruction
    // - if the additinal xDestroy parameter is not nil, then it is invoked when
    // the function is deleted, either by being overloaded or when the database
    // connection closes.
    // - When the destructure callback of the tenth parameter is invoked, it is
    // passed a single argument which is a copy of the pointer which was the fifth
    // parameter to sqlite3.create_function_v2().
    // - this function is not available in older revisions - e.g. 3.6.*
    create_function_v2: function(DB: TSQLite3DB; FunctionName: PUTF8Char;
      nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSQLFunctionFunc;
      xFinal: TSQLFunctionFinal; xDestroy: TSQLDestroyPtr): Integer; cdecl;

    /// add SQL functions or aggregates or to redefine the behavior of existing
    // SQL functions or aggregates, including  extra callback functions needed
    // by aggregate window functions
    // - see https://www.sqlite.org/windowfunctions.html#aggregate_window_functions
    // - sixth, seventh, eighth and ninth parameters (xStep, xFinal, xValue
    // and xInverse) passed to this function are pointers to callbacks that
    // implement the new aggregate window function. xStep and xFinal must both
    // be non-nil. xValue and xInverse may either both be nil, in which case a
    // regular aggregate function is created, or must both be non-nil, in which
    // case the new function may be used as either an aggregate or aggregate
    // window function
    // - this function is not available in older revisions, i.e. before 3.25.2
    create_window_function: function(DB: TSQLite3DB; FunctionName: PUTF8Char;
      nArg, eTextRep: integer; pApp: pointer; xStep: TSQLFunctionFunc;
      xFinal, xValue: TSQLFunctionFinal; xInverse: TSQLFunctionFunc;
      xDestroy: TSQLDestroyPtr): Integer; cdecl;

    /// Define New Collating Sequences
    // - add new collation sequences to the database connection specified
    // - collation name is to be used in CREATE TABLE t1 (a COLLATE CollationName);
    // or in SELECT * FROM t1 ORDER BY c COLLATE CollationName;
    // - StringEncoding is either SQLITE_UTF8 either SQLITE_UTF16
    // - TSQLDataBase.Create add WIN32CASE, WIN32NOCASE and ISO8601 collations
    create_collation: function(DB: TSQLite3DB; CollationName: PUTF8Char;
      StringEncoding: integer; CollateParam: pointer; cmp: TSQLCollateFunc): integer; cdecl;

    /// Returns the rowid of the most recent successful INSERT into the database
    last_insert_rowid: function(DB: TSQLite3DB): Int64; cdecl;

    /// Set A Busy Timeout
    // - This routine sets a busy handler that sleeps for a specified amount of time
    // when a table is locked. The handler will sleep multiple times until at least
    // "ms" milliseconds of sleeping have accumulated. After at least "ms" milliseconds
    // of sleeping, the handler returns 0 which causes sqlite3.step() to return
    // SQLITE_BUSY or SQLITE_IOERR_BLOCKED.
    // - Calling this routine with an argument less than or equal to zero turns off
    // all busy handlers.
    // - There can only be a single busy handler for a particular database connection
    // any given moment. If another busy handler was defined (using
    // sqlite3.busy_handler()) prior to calling this routine, that other busy handler
    // is cleared.
    busy_timeout: function(DB: TSQLite3DB; Milliseconds: integer): integer; cdecl;

    /// Register A Callback To Handle SQLITE_BUSY Errors
    // - This routine sets a callback function that might be invoked whenever an
    // attempt is made to open a database table that another thread or process has locked.
    // - If the busy callback is nil, then SQLITE_BUSY or SQLITE_IOERR_BLOCKED is
    // returned immediately upon encountering the lock. If the busy callback is not
    // nil, then the callback might be invoked with two arguments.
    // - The default busy callback is nil.
    busy_handler: function(DB: TSQLite3DB;
      CallbackPtr: TSQLBusyHandler; user: Pointer): integer;  cdecl;

    /// Compile a SQL query into byte-code
    // - SQL must contains an UTF8-encoded null-terminated string query
    // - SQL_bytes contains -1 (to stop at the null char) or the number of bytes in
    // the input string, including the null terminator
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S will contain an handle of the resulting statement (an opaque sqlite3.stmt
    // object) on success, or will 0 on error - the calling procedure is responsible
    // for deleting the compiled SQL statement using sqlite3.finalize() after it has
    // finished with it
    // - in this "v2" interface, the prepared statement that is returned contains a
    // copy of the original SQL text
    // - this routine only compiles the first statement in SQL, so SQLtail is left pointing
    // to what remains uncompiled
    prepare_v2: function(DB: TSQLite3DB; SQL: PUTF8Char; SQL_bytes: integer;
      var S: TSQLite3Statement; var SQLtail: PUTF8Char): integer; cdecl;

    /// Delete a previously prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - this routine can be called at any point during the execution of the prepared
    //  statement. If the virtual machine has not completed execution when this routine
    //  is called, that is like encountering an error or an interrupt. Incomplete updates
    //  may be rolled back and transactions canceled, depending on the circumstances,
    //  and the error code returned will be SQLITE_ABORT
    finalize: function(S: TSQLite3Statement): integer; cdecl;

    /// Find the next prepared statement
    // - this interface returns a handle to the next prepared statement after S,
    // associated with the database connection DB.
    // - if S is 0 then this interface returns a pointer to the first prepared
    // statement associated with the database connection DB.
    // - if no prepared statement satisfies the conditions of this routine, it returns 0
    next_stmt: function(DB: TSQLite3DB; S: TSQLite3Statement): TSQLite3Statement; cdecl;

    /// Reset a prepared statement object back to its initial state, ready to be re-Prepared
    // - if the most recent call to sqlite3.step(S) returned SQLITE_ROW or SQLITE_DONE,
    // or if sqlite3.step(S) has never before been called with S, then sqlite3.reset(S)
    // returns SQLITE_OK.
    // - return an appropriate error code if the most recent call to sqlite3.step(S) failed
    // - any SQL statement variables that had values bound to them using the sqlite3.bind_*()
    // API retain their values. Use sqlite3.clear_bindings() to reset the bindings.
    reset: function(S: TSQLite3Statement): integer; cdecl;

    ///  returns true (non-zero) if and only if the prepared statement X
    // makes no direct changes to the content of the database file
    // - Transaction control statements such as BEGIN, COMMIT, ROLLBACK, SAVEPOINT,
    // and RELEASE cause sqlite3.stmt_readonly() to return true, since the statements
    // themselves do not actually modify the database but rather they control the
    // timing of when other statements modify the database. The ATTACH and DETACH
    // statements also cause sqlite3.stmt_readonly() to return true since, while
    // those statements change the configuration of a database connection, they
    // do not make changes to the content of the database files on disk.
    stmt_readonly: function(S: TSQLite3Statement): integer; cdecl;

    /// Evaluate An SQL Statement, returning a result status:
    // - SQLITE_BUSY means that the database engine was unable to acquire the database
    // locks it needs to do its job. If the statement is a COMMIT or occurs outside of
    // an explicit transaction, then you can retry the statement. If the statement
    // is not a COMMIT and occurs within a explicit transaction then you should
    // rollback the transaction before continuing.
    // - SQLITE_DONE means that the statement has finished executing successfully.
    // sqlite3.step() should not be called again on this virtual machine without
    // first calling sqlite3.reset() to reset the virtual machine state back.
    // - SQLITE_ROW is returned each time a new row of data is ready for processing by
    // the caller. The values may be accessed using the column access functions below.
    // sqlite3.step() has to be called again to retrieve the next row of data.
    // - SQLITE_MISUSE means that the this routine was called inappropriately. Perhaps
    // it was called on a prepared statement that has already been finalized or on
    // one that had previously returned SQLITE_ERROR or SQLITE_DONE. Or it could be
    // the case that the same database connection is being used by two or more threads
    // at the same moment in time.
    // - SQLITE_SCHEMA means that the database schema changes, and the SQL statement
    // has been recompiled and run again, but the schame changed in a way that makes
    // the statement no longer valid, as a fatal error.
    // - another specific error code is returned on fatal error
    step: function(S: TSQLite3Statement): integer; cdecl;

    /// get the number of columns in the result set for the statement
    column_count: function(S: TSQLite3Statement): integer; cdecl;

    /// datatype code for the initial data type of a result column
    // - returned value is one of SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT,
    // SQLITE_BLOB or SQLITE_NULL
    // - S is the SQL statement, after sqlite3.step(S) returned SQLITE_ROW
    // - Col is the column number, indexed from 0 to sqlite3.column_count(S)-1
    // - must be called before any sqlite3.column_*() statement, which may result in
    // an implicit type conversion: in this case, value is undefined
    column_type: function(S: TSQLite3Statement; Col: integer): integer; cdecl;

    /// returns a zero-terminated UTF-8 string containing the declared datatype
    // of a result column
    column_decltype: function(S: TSQLite3Statement; Col: integer): PAnsiChar; cdecl;

    /// returns the name of a result column as a zero-terminated UTF-8 string
    column_name: function(S: TSQLite3Statement; Col: integer): PUTF8Char; cdecl;

    /// number of bytes for a BLOB or UTF-8 string result
    // - S is the SQL statement, after sqlite3.step(S) returned SQLITE_ROW
    // - Col is the column number, indexed from 0 to sqlite3.column_count(S)-1
    // - an implicit conversion into UTF-8 text is made for a numeric value or
    // UTF-16 column: you must call sqlite3.column_text() or sqlite3.column_blob()
    // before calling sqlite3.column_bytes() to perform the conversion itself
    column_bytes: function(S: TSQLite3Statement; Col: integer): integer; cdecl;

    /// get the value handle of the Col column in the current row of prepared statement S
    // - this handle represent a sqlite3.value object
    // - this handle can then be accessed with any sqlite3.value_*() function below
    column_value: function(S: TSQLite3Statement; Col: integer): TSQLite3Value; cdecl;

    /// converts the Col column in the current row prepared statement S
    // into a floating point value and returns a copy of that value
    // - NULL is converted into 0.0
    // - INTEGER is converted into corresponding floating point value
    // - TEXT or BLOB is converted from all correct ASCII numbers with 0.0 as default
    column_double: function(S: TSQLite3Statement; Col: integer): double; cdecl;

    /// converts the Col column in the current row prepared statement S
    // into a 32 bit integer value and returns a copy of that value
    // - NULL is converted into 0
    // - FLOAT is truncated into corresponding integer value
    // - TEXT or BLOB is converted from all correct ASCII numbers with 0 as default
    column_int: function(S: TSQLite3Statement; Col: integer): integer; cdecl;

    /// converts the Col column in the current row prepared statement S
    // into a 64 bit integer value and returns a copy of that value
    // - NULL is converted into 0
    // - FLOAT is truncated into corresponding integer value
    // - TEXT or BLOB is converted from all correct ASCII numbers with 0 as default
    column_int64: function(S: TSQLite3Statement; Col: integer): int64; cdecl;

    /// converts the Col column in the current row prepared statement S
    // into a zero-terminated UTF-8 string and returns a pointer to that string
    // - NULL is converted into nil
    // - INTEGER or FLOAT are converted into ASCII rendering of the numerical value
    // - TEXT is returned directly (with UTF-16 -> UTF-8 encoding if necessary)
    // - BLOB add a zero terminator if needed
    column_text: function(S: TSQLite3Statement; Col: integer): PUTF8Char; cdecl;

    /// converts the Col column in the current row prepared statement S
    // into a zero-terminated UTF-16 string and returns a pointer to that string
    // - NULL is converted into nil
    // - INTEGER or FLOAT are converted into ASCII rendering of the numerical value
    // - TEXT is returned directly (with UTF-8 -> UTF-16 encoding if necessary)
    // - BLOB add a zero terminator if needed
    column_text16: function(S: TSQLite3Statement; Col: integer): PWideChar; cdecl;

    /// converts the Col column in the current row of prepared statement S
    // into a BLOB and then returns a pointer to the converted value
    // - NULL is converted into nil
    // - INTEGER or FLOAT are converted into ASCII rendering of the numerical value
    // - TEXT and BLOB are returned directly
    column_blob: function(S: TSQLite3Statement; Col: integer): PAnsiChar; cdecl;


    /// datatype code for a sqlite3.value object, specified by its handle
    // - returned value is one of SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT,
    // SQLITE_BLOB or SQLITE_NULL
    // - must be called before any sqlite3.value_*() statement, which may result in
    // an implicit type conversion: in this case, value is undefined
    value_type: function(Value: TSQLite3Value): integer; cdecl;

    /// attempts to apply numeric affinity to the value
    // - This means that an attempt is made to convert the value to an integer or
    // floating point. If such a conversion is possible without loss of information
    // (in other words, if the value is a string that looks like a number) then the
    // conversion is performed. Otherwise no conversion occurs. The datatype after
    // conversion is returned.
    // - returned value is one of SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT,
    // SQLITE_BLOB or SQLITE_NULL
    value_numeric_type: function(Value: TSQLite3Value): integer; cdecl;

    /// number of bytes for a sqlite3.value object, specified by its handle
    // - used after a call to sqlite3.value_text() or sqlite3.value_blob()
    //  to determine buffer size (in bytes)
    value_bytes: function(Value: TSQLite3Value): integer; cdecl;

    /// converts a sqlite3.value object, specified by its handle,
    // into a floating point value and returns a copy of that value
    value_double: function(Value: TSQLite3Value): double; cdecl;

    /// converts a sqlite3.value object, specified by its handle,
    // into an integer value and returns a copy of that value
    value_int64: function(Value: TSQLite3Value): Int64; cdecl;

    /// converts a sqlite3.value object, specified by its handle,
    // into an UTF-8 encoded string, and returns a copy of that value
    value_text: function(Value: TSQLite3Value): PUTF8Char; cdecl;

    /// converts a sqlite3.value object, specified by its handle,
    // into a blob memory, and returns a copy of that value
    value_blob: function(Value: TSQLite3Value): pointer; cdecl;


    /// sets the return value of the application-defined function to be NULL
    result_null: procedure(Context: TSQLite3FunctionContext); cdecl;

    /// sets the return value of the application-defined function to be the 64-bit
    // signed integer value given in the 2nd argument
    result_int64: procedure(Context: TSQLite3FunctionContext; Value: Int64); cdecl;

    /// sets the result from an application-defined function to be a floating point
    // value specified by its 2nd argument
    result_double: procedure(Context: TSQLite3FunctionContext; Value: double); cdecl;

    /// sets the result from an application-defined function to be the BLOB
    // - content is pointed to by the Value and which is Value_bytes bytes long
    // - set DestroyPtr as SQLITE_STATIC (nil) for static binding
    // - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    // copy of the data (this is the prefered way in our Framework)
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    // or to @sqlite3InternalFreeObject if Value must be released via a Free method
    result_blob: procedure(Context: TSQLite3FunctionContext; Value: Pointer;
      Value_bytes: Integer=0; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT); cdecl;

    /// sets the return value of the application-defined function to be a text string
    // which is represented as UTF-8
    // - if Value_bytes is negative, then SQLite takes result text from the Value
    // parameter through the first zero character
    // - if Value_bytes is non-negative, then as many bytes (NOT characters: this
    // parameter must include the #0 terminator) of the text pointed to by the
    // Value parameter are taken as the application-defined function result
    // - set DestroyPtr as SQLITE_STATIC (nil) for static binding
    // - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    // copy of the data (this is the prefered way in our Framework)
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    // or to @sqlite3InternalFreeObject if Value must be released via a Free method
    result_text: procedure(Context: TSQLite3FunctionContext; Value: PUTF8Char;
      Value_bytes: Integer=-1; DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT); cdecl;

    /// sets the result of the application-defined function to be a copy the unprotected
    // sqlite3.value object specified by the 2nd parameter
    // - The sqlite3.result_value() interface makes a copy of the sqlite3.value so
    // that the sqlite3.value specified in the parameter may change or be deallocated
    // after sqlite3.result_value() returns without harm
    result_value: procedure(Context: TSQLite3FunctionContext; Value: TSQLite3Value); cdecl;

    /// cause the implemented SQL function to throw an exception
    // - SQLite interprets the error message string from sqlite3.result_error() as UTF-8
    // - if MsgLen is negative, Msg must be #0 ended, or MsgLen must tell the numnber of
    // characters in the Msg UTF-8 buffer
    result_error: procedure(Context: TSQLite3FunctionContext; Msg: PUTF8Char; MsgLen: integer=-1); cdecl;

    /// returns a copy of the pointer that was the pUserData parameter (the 5th
    // parameter) of the sqlite3.create_function() routine that originally
    // registered the application defined function
    // - This routine must be called from the same thread in which the
    // application-defined function is running
    user_data: function(Context: TSQLite3FunctionContext): pointer; cdecl;

    /// returns a copy of the pointer to the database connection (the 1st parameter)
    // of the sqlite3.create_function() routine that originally registered the
    // application defined function
    context_db_handle: function(Context: TSQLite3FunctionContext): TSQLite3DB; cdecl;

    /// Implementations of aggregate SQL functions use this routine to allocate
    // memory for storing their state.
    // - The first time the sqlite3.aggregate_context(C,N) routine is called for a
    // particular aggregate function, SQLite allocates N of memory, zeroes out that
    // memory, and returns a pointer to the new memory. On second and subsequent calls
    // to sqlite3.aggregate_context() for the same aggregate function instance, the
    // same buffer is returned. sqlite3.aggregate_context() is normally called once
    // for each invocation of the xStep callback and then one last time when the
    // xFinal callback is invoked. When no rows match an aggregate query, the xStep()
    // callback of the aggregate function implementation is never called and xFinal()
    // is called exactly once. In those cases, sqlite3.aggregate_context() might be
    // called for the first time from within xFinal().
    // - The sqlite3.aggregate_context(C,N) routine returns a nil pointer if N is
    // less than or equal to zero or if a memory allocate error occurs.
    // - The amount of space allocated by sqlite3.aggregate_context(C,N) is
    // determined by the N parameter on first successful call. Changing the value
    // of N in subsequent call to sqlite3.aggregate_context() within the same
    // aggregate function instance will not resize the memory allocation.
    // - SQLite automatically frees the memory allocated by sqlite3.aggregate_context()
    // when the aggregate query concludes.
    aggregate_context: function(Context: TSQLite3FunctionContext;
       nBytes: integer): pointer; cdecl;

    /// Bind a Text Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set. The leftmost SQL parameter
    // has an index of 1.
    // - Text must contains an UTF8-encoded null-terminated string query
    // - Text_bytes contains -1 (to stop at the null char) or the number of chars
    // in the input string, excluding the null terminator
    // - set DestroyPtr as SQLITE_STATIC (nil) for static binding
    // - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    // copy of the data (this is the prefered way in our Framework)
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    bind_text: function(S: TSQLite3Statement; Param: integer;
      Text: PUTF8Char; Text_bytes: integer=-1;
      DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT): integer; cdecl;
      // note that the official SQLite3 documentation could lead into misunderstanding:
      // Text_bytes must EXCLUDE the null terminator, otherwise a #0 is appended to
      // all column values

    /// Bind a Blob Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    // - Buf must point to a memory buffer of Buf_bytes bytes
    // - Buf_bytes contains the number of bytes in Buf
    // - set DestroyPtr as SQLITE_STATIC (nil) for static binding
    // - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    // copy of the data (this is the prefered way in our Framework)
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    bind_blob: function(S: TSQLite3Statement; Param: integer; Buf: pointer; Buf_bytes: integer;
      DestroyPtr: TSQLDestroyPtr=SQLITE_TRANSIENT): integer; cdecl;

    /// bind a ZeroBlob buffer to a parameter
    // - uses a fixed amount of memory (just an integer to hold its size) while
    // it is being processed. Zeroblobs are intended to serve as placeholders
    // for BLOBs whose content is later written using incremental BLOB I/O routines.
    // - a negative value for the Size parameter results in a zero-length BLOB
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    bind_zeroblob: function(S: TSQLite3Statement; Param: integer; Size: integer): integer; cdecl;

    /// Bind a floating point Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    // - Value is the floating point number to bind
    bind_double: function(S: TSQLite3Statement; Param: integer; Value: double): integer; cdecl;

    /// Bind a 32 bits Integer Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    // - Value is the 32 bits Integer to bind
    bind_int: function(S: TSQLite3Statement; Param: integer; Value: integer): integer; cdecl;

    /// Bind a 64 bits Integer Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    // - Value is the 64 bits Integer to bind
    bind_int64: function(S: TSQLite3Statement; Param: integer; Value: Int64): integer; cdecl;

    /// Bind a NULL Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    bind_null: function(S: TSQLite3Statement; Param: integer): integer; cdecl;

    /// Reset All Bindings On A Prepared Statement
    clear_bindings: function(S: TSQLite3Statement): integer; cdecl;

    /// Number Of SQL Parameters for a prepared statement
    // - returns the index of the largest (rightmost) parameter. For all forms
    // except ?NNN, this will correspond to the number of unique parameters.
    // - If parameters of the ?NNN type are used, there may be gaps in the list.
    bind_parameter_count: function(S: TSQLite3Statement): integer; cdecl;

    /// Open a BLOB For Incremental I/O
    // - returns a BLOB handle for row RowID, column ColumnName, table TableName
    // in database DBName; in other words, the same BLOB that would be selected by:
    // ! SELECT ColumnName FROM DBName.TableName WHERE rowid = RowID;
    blob_open: function(DB: TSQLite3DB; DBName, TableName, ColumnName: PUTF8Char;
      RowID: Int64; Flags: Integer; var Blob: TSQLite3Blob): Integer; cdecl;

    /// Move a BLOB Handle to a New Row
    // - will point to a different row of the same database table
    // - this is faster than closing the existing handle and opening a new one
    blob_reopen: function(Blob: TSQLite3Blob; RowID: Int64): Integer; cdecl;

    /// Close A BLOB Handle
    blob_close: function(Blob: TSQLite3Blob): Integer; cdecl;

    /// Read Data From a BLOB Incrementally
    blob_read: function(Blob: TSQLite3Blob; const Data; Count, Offset: Integer): Integer; cdecl;

    /// Write Data To a BLOB Incrementally
    blob_write: function(Blob: TSQLite3Blob; const Data; Count, Offset: Integer): Integer; cdecl;

    /// Return The Size Of An Open BLOB
    blob_bytes: function(Blob: TSQLite3Blob): Integer; cdecl;

    /// Used to register a new virtual table module name
    // - The module name is registered on the database connection specified by the
    // first DB parameter.
    // - The name of the module is given by the second parameter.
    // - The third parameter is a pointer to the implementation of the virtual table
    // module.
    // - The fourth parameter is an arbitrary client data pointer that is passed
    // through into the xCreate and xConnect methods of the virtual table module
    // when a new virtual table is be being created or reinitialized.
    // - The fifth parameter can be used to specify a custom destructor for the
    // pClientData buffer. SQLite will invoke the destructor function (if it is
    // not nil) when SQLite no longer needs the pClientData pointer. The
    // destructor will also be invoked if call to sqlite3.create_module_v2() fails.
    create_module_v2: function(DB: TSQLite3DB; const zName: PAnsiChar;
      var p: TSQLite3Module; pClientData: Pointer; xDestroy: TSQLDestroyPtr): Integer; cdecl;

    /// Declare the Schema of a virtual table
    // - The xCreate() and xConnect() methods of a virtual table module call this
    // interface to declare the format (the names and datatypes of the columns) of
    // the virtual tables they implement. The string can be deallocated and/or reused
    // as soon as the sqlite3.declare_vtab() routine returns.
    // - If a column datatype contains the special keyword "HIDDEN" (in any
    // combination of upper and lower case letters) then that keyword it is omitted
    // from the column datatype name and the column is marked as a hidden column
    // internally. A hidden column differs from a normal column in three respects:
    // 1. Hidden columns are not listed in the dataset returned by "PRAGMA table_info",
    // 2. Hidden columns are not included in the expansion of a "*" expression in
    // the result set of a SELECT, and 3. Hidden columns are not included in the
    // implicit column-list used by an INSERT statement that lacks an explicit
    // column-list.
    declare_vtab: function(DB: TSQLite3DB; const zSQL: PAnsiChar): Integer; cdecl;

    /// Registers an authorizer callback to a specified DB connection
    // - Only a single authorizer can be in place on a database connection at a time
    // - Each call to sqlite3.set_authorizer overrides the previous call
    // - Disable the authorizer by installing a nil callback
    // - The authorizer is disabled by default
    set_authorizer: function(DB: TSQLite3DB; xAuth: TSQLAuthorizerCallback;
      pUserData: Pointer): Integer; cdecl;

    /// Register Data Change Notification Callbacks
    // - The sqlite3.update_hook() interface registers a callback function with
    // the database connection identified by the first argument to be invoked
    // whenever a row is updated, inserted or deleted.
    // - Any callback set by a previous call to this function for the same
    // database connection is overridden.
    // - sqlite3.update_hook(D,C,P) function returns the P argument from the
    // previous call on the same database connection D, or nil for the first
    // call on database connection D.
    // - The update hook is not invoked when internal system tables are modified
    // (i.e. sqlite_master and sqlite_sequence).
    // - In the current implementation, the update hook is not invoked when
    // duplication rows are deleted because of an ON CONFLICT REPLACE clause.
    // Nor is the update hook invoked when rows are deleted using the truncate
    // optimization. The exceptions defined in this paragraph might change in
    // a future release of SQLite.
    // - Note that you should also trace COMMIT and ROLLBACK commands (calling
    // sqlite3.commit_hook() and sqlite3.rollback_hook() functions) if you want to
    // ensure that the notified update was not canceled by a later Rollback.
    update_hook: function(DB: TSQLite3DB; xCallback: TSQLUpdateCallback;
      pArg: pointer): pointer; cdecl;

    /// Register Commit Notification Callbacks
    // - The sqlite3.commit_hook() interface registers a callback function to be
    // invoked whenever a transaction is committed.
    // - Any callback set by a previous call to sqlite3.commit_hook() for the same
    // database connection is overridden.
    // - Registering a nil function disables the Commit callback.
    // - The sqlite3.commit_hook(DB,C,P) function returns the P argument from the
    // previous call of the same function on the same database connection DB, or nil
    // for the first call for each function on DB.
    commit_hook: function(DB: TSQLite3DB; xCallback: TSQLCommitCallback;
      pArg: Pointer): Pointer; cdecl;

    // Register Rollback Notification Callbacks
    // - The sqlite3.rollback_hook() interface registers a callback function to be
    // invoked whenever a transaction is rolled back.
    // - Any callback set by a previous call to sqlite3.rollback_hook() for the same
    // database connection is overridden.
    // - Registering a nil function disables the Rollback callback.
    // - The sqlite3.rollback_hook(D,C,P) function returns the P argument from the
    // previous call of the same function on the same database connection D, or nil
    // for the first call for each function on D.
    rollback_hook: function(DB: TSQLite3DB;  xCallback: TSQLCommitCallback;
      pArg: Pointer): Pointer; cdecl;

    /// Count The Number Of Rows Modified
    // - This function returns the number of database rows that were changed or
    // inserted or deleted by the most recently completed SQL statement on the
    // database connection specified by the first parameter. Only changes that
    // are directly specified by the INSERT, UPDATE, or DELETE statement are counted.
    // Auxiliary changes caused by triggers or foreign key actions are not counted.
    // Use the sqlite3.total_changes() function to find the total number of changes
    // including changes caused by triggers and foreign key actions.
    // - If a separate thread makes changes on the same database connection while
    // sqlite3.changes() is running then the value returned is unpredictable and not
    // meaningful.
    changes: function(DB: TSQLite3DB): Integer; cdecl;

    /// Total Number Of Rows Modified
    // - This function returns the number of row changes caused by INSERT, UPDATE or
    // DELETE statements since the database connection was opened. The count returned
    // by sqlite3.total_changes() includes all changes from all trigger contexts and
    // changes made by foreign key actions. However, the count does not include
    // changes used to implement REPLACE constraints, do rollbacks or ABORT
    // processing, or DROP TABLE processing. The count does not include rows of
    // views that fire an INSTEAD OF trigger, though if the INSTEAD OF trigger makes
    // changes of its own, those changes are counted. The sqlite3.total_changes()
    // function counts the changes as soon as the statement that makes them is
    // completed (when the statement handle is passed to sqlite3.reset()
    // or sqlite3.finalize()).
    // - If a separate thread makes changes on the same database connection while
    // sqlite3.total_changes() is running then the value returned is unpredictable
    // and not meaningful.
    total_changes: function(DB: TSQLite3DB): Integer; cdecl;

    /// Returns a pointer to a block of memory at least N bytes in length
    // - should call native malloc() function, i.e. GetMem() in this unit
    malloc: function(n: Integer): Pointer; cdecl;

    /// Attempts to resize a prior memory allocation
    // - should call native realloc() function, i.e. ReallocMem() in this unit
    realloc: function(pOld: Pointer; n: Integer): Pointer; cdecl;

    /// Releases memory previously returned by sqlite3.malloc() or sqlite3.realloc()
    // - should call native free() function, i.e. FreeMem() in this unit
    // - renamed free_ in order not to override TObject.Free method
    free_: procedure(p: Pointer); cdecl;

    /// Returns the number of bytes of memory currently outstanding (malloced but not freed)
    memory_used: function: Int64; cdecl;

    /// Returns the maximum value of sqlite3.memory_used() since the high-water mark
    // was last reset
    memory_highwater: function(resetFlag: Integer): Int64; cdecl;

    /// Register callback function that can be used for tracing the execution of
    // SQL statements
    // - registers a trace callback function Callback against database connection
    // DB, using property mask TSQLTraceMask and context pointer UserData
    // - if the Callback parameter is nil or if the TSQLTraceMask mask is zero,
    // then tracing is disabled
    // - parameters of the Callback functions depend of the TSQLTraceMask involved
    trace_v2: function(DB: TSQLite3DB; Mask: TSQLTraceMask; Callback: TSQLTraceCallback;
      UserData: Pointer): Pointer; cdecl;

    /// Allows the size of various constructs to be limited on a connection
    // by connection basis
    // - The first parameter is the database connection whose limit is to be
    // set or queried
    // - The second parameter is one of the limit categories that define a
    // class of constructs to be size limited - see TSQLLimitCategory enumerate
    // - The third parameter is the new limit for that construct. If the new
    // limit is a negative number, the limit is unchanged.
    // - Regardless of whether or not the limit was changed, the sqlite3.limit()
    // interface returns the prior value of the limit. Hence, to find the current
    // value of a limit without changing it, simply invoke this interface with
    // the third parameter set to -1.
    limit: function(DB: TSQLite3DB; id,newValue: integer): integer; cdecl;

    /// Initialize a backup process of a given SQLite3 database instance
    // - The DestDB and DestDatabaseName arguments are the database connection
    // associated with the destination database and the database name,
    // respectively.  The database name is "main" for the main database,
    // "temp" for the temporary database, or the name specified after
    // the AS keyword in an ATTACH statement for an attached database.
    // - The SourceDB and SourceDatabaseName arguments identify the database
    // connection and database name of the source database, respectively.
    // - The source and destination database connections (parameters SourceDB and
    // DestDB) must be different or else function will fail with an error.
    // - If an error occurs within backup_init(), then nil is returned and an
    // error code and error message are stored in the destination database
    // connection DestDB. The error code and message for the failed call to
    // backup_init() can be retrieved using the errcode() or errmsg() functions.
    // - A successful call to backup_init() returns a pointer to an TSQLite3Backup
    // object. The TSQLite3Backup object may be used with the backup_step() and
    // backup_finish() functions to perform the specified backup operation.
    backup_init: function(DestDB: TSQLite3DB; DestDatabaseName: PUTF8Char;
      SourceDB: TSQLite3DB; SourceDatabaseName: PUTF8Char): TSQLite3Backup; cdecl;

    /// perform a backup step to transfer the data between the two databases
    // - backup_step() will copy up to nPages pages between the source and
    // destination databases specified by TSQLite3Backup object Backup.
    // - If nPages is negative, all remaining source pages are copied.
    // - If backup_step() successfully copies nPages pages and there are still more
    // pages to be copied, then the function returns SQLITE_OK.
    // - If backup_step() successfully finishes copying all pages from source to
    // destination, then it returns SQLITE_DONE.
    // - If an error occurs while running backup_step(), an error code is returned.
    // - As well as SQLITE_OK and SQLITE_DONE, a call to backup_step() may return
    // SQLITE_READONLY, SQLITE_NOMEM, SQLITE_BUSY, SQLITE_LOCKED, or an
    // SQLITE_IOERR_XXX extended error code. The function might return
    // SQLITE_READONLY if the destination database was opened read-only, or is
    // using WAL journaling and the destination and source page sizes differ, or
    // the destination database is an in-memory database and the destination and
    // source page sizes differ. SQLITE_BUSY indicates that the file-system lock
    // did not succeed: in this case the call to backup_step() can be retried later.
    // If the source database connection is being used to write to the source
    // database when backup_step() is called, then SQLITE_LOCKED is returned
    // immediately. Again, in this case the call to backup_step() can be retried
    // later on. If SQLITE_IOERR_XXX, SQLITE_NOMEM, or SQLITE_READONLY is returned,
    // then there is no point in retrying the call to backup_step(). These errors
    // are considered fatal. The application must accept that the backup operation
    // has failed and pass the backup operation handle to the backup_finish() to
    // release associated resources.
    // - The first call to sqlite3_backup_step() obtains an exclusive lock on the
    // destination file. The exclusive lock is not released until either
    // backup_finish() is called or the backup operation is complete and
    // backup_step() returns SQLITE_DONE. Every call to backup_step() obtains a
    // shared lock on the source database that lasts for the duration of the
    // backup_step() call.
    // - Because the source database is not locked between calls to backup_step(),
    // the source database may be modified mid-way through the backup process.
    // If the source database is modified by an external process or via a database
    // connection other than the one being used by the backup operation, then the
    // backup will be automatically restarted by the next call to backup_step().
    // If the source database is modified by the using the same database connection
    // as is used by the backup operation (which is the case in the SynSQLite3 and
    // mORMotSQLite3 units), then the backup database is automatically updated at
    // the same time, so you won't loose any data.
    backup_step: function(Backup: TSQLite3Backup; nPages: integer): integer; cdecl;
    /// finalize a Backup process on a given database
    // - When backup_step() has returned SQLITE_DONE, or when the application
    // wishes to abandon the backup operation, the application should destroy the
    // TSQLite3Backup by passing it to backup_finish().
    // - The backup_finish() interfaces releases all resources associated with the
    // TSQLite3Backup object. If backup_step() has not yet returned SQLITE_DONE,
    // then any active write-transaction on the destination database is rolled back.
    // - The TSQLite3Backup object is invalid and may not be used following a call
    // to backup_finish().
    // - The value returned by backup_finish is SQLITE_OK if no backup_step() errors
    // occurred, regardless or whether or not backup_step() completed. If an
    // out-of-memory condition or IO error occurred during any prior backup_step()
    // call on the same TSQLite3Backup object, then backup_finish() returns the
    // corresponding error code.
    // - A return of SQLITE_BUSY or SQLITE_LOCKED from backup_step() is not a
    // permanent error and does not affect the return value of backup_finish().
    backup_finish: function(Backup: TSQLite3Backup): integer; cdecl;

    /// returns the number of pages still to be backed up for a given Backup
    // - The values returned by this function is only updated by backup_step().
    // If the source database is modified during a backup operation, then the
    // value is not updated to account for any extra pages that need to be
    // updated or the size of the source database file changing.
    backup_remaining: function(Backup: TSQLite3Backup): integer; cdecl;

    /// returns the the total number of pages in the source database file
    // for a given Backup process
    // - The values returned by this function is only updated by backup_step().
    // If the source database is modified during a backup operation, then the
    // value is not updated to account for any extra pages that need to be
    // updated or the size of the source database file changing.
    backup_pagecount: function(Backup: TSQLite3Backup): integer; cdecl;

    /// serialize a database
    // - returns a pointer to memory that is a serialization of the Schema
    // database on database connection DB
    // - if Size is not nil, then the size of the database in bytes is written into Size^
    // - for an ordinary on-disk database file, the serialization is just a copy
    // of the disk file; for an in-memory database or a "TEMP" database, the
    // serialization is the same sequence of bytes which would be written to disk
    // if that database where backed up to disk
    // - caller is responsible for freeing the returned value (using free_)
    // to avoid a memory leak
    serialize: function(DB: TSQLite3DB; Schema: PUTF8Char; Size: PInt64;
      Flags: integer): pointer; cdecl;

    /// deserialize a database
    // - causes the database connection DB to disconnect from database Schema
    // and then reopen Schema as an in-memory database based on the serialization
    // contained in Data; the serialized database Data is DBSize bytes in size
    // - BufSize is the size of the buffer Data, which might be larger than DBSize
    deserialize: function(DB: TSQLite3DB; Schema: PUTF8Char; Data: pointer;
      DBSize, BufSize: Int64; Flags: integer): pointer; cdecl;

    /// sets and/or queries the soft limit on the amount of heap memory
    // that may be allocated by SQLite
    // - SQLite strives to keep heap memory utilization below the soft heap limit
    // by reducing the number of pages held in the page cache as heap memory usages
    // approaches the limit. The soft heap limit is "soft" because even though
    // SQLite strives to stay below the limit, it will exceed the limit rather
    // than generate an SQLITE_NOMEM error. In other words, the soft heap limit
    // is advisory only
    // - The return value from soft_heap_limit64() is the size of the soft heap
    // limit prior to the call, or negative in the case of an error. If the
    // argument N is negative then no change is made to the soft heap limit.
    // Hence, the current size of the soft heap limit can be determined by
    // invoking soft_heap_limit64() with a negative argument
    // - This function is useful when you have many SQLite databases open at
    // the same time, as the cache-size setting is per-database (connection),
    // while this limit is global for the process, so this allows to limit the
    // total cache size
    soft_heap_limit64: function(N: Int64): Int64; cdecl;

    /// used to make global configuration changes to current database
    config: function(operation: integer): integer;
      {$ifndef DELPHI5OROLDER}cdecl varargs;{$endif}

    /// used to make global configuration changes to current database connection
    db_config: function(DestDB: TSQLite3DB; operation: integer): integer;
      {$ifndef DELPHI5OROLDER}cdecl varargs;{$endif}

    /// initialize the internal version numbers
    constructor Create; virtual;
    /// will change the SQLite3 configuration to use Delphi/FPC memory manager
    // - this will reduce memory fragmentation, and enhance speed, especially
    // under multi-process activity
    // - this method should be called before sqlite3.initialize()
    procedure ForceToUseSharedMemoryManager; virtual;
    /// returns the current version number as a plain integer
    // - equals e.g. 3008003001 for '3.8.3.1'
    property VersionNumber: cardinal read fVersionNumber;
    /// returns the current version number as a text
    // - equals e.g. '3.8.3.1'
    // - use the Version property for the full information about this instance
    property VersionText: RawUTF8 read fVersionText;
  published
    /// will return the class name and SQLite3 version number
    // - if self (e.g. global sqlite3) is nil, will return ''
    property Version: RawUTF8 read GetVersion;
  end;
  {$M-}

  /// allow access to an exernal SQLite3 library engine
  // - you can e.g. replace the main sqlite3 engine with any external library:
  // ! FreeAndNil(sqlite3); // release any previous instance (e.g. static)
  // ! sqlite3 := TSQLite3LibraryDynamic.Create;
  TSQLite3LibraryDynamic = class(TSQLite3Library)
  protected
    {$ifdef FPC}
    fHandle: TLibHandle;
    {$else}
    fHandle: THandle;
    {$endif}
    fLibraryName: TFileName;
  public
    /// initialize the specified external library
    // - raise an ESQLite3Exception on error
    constructor Create(const LibraryName: TFileName=SQLITE_LIBRARY_DEFAULT_NAME); reintroduce;
    /// unload the external library
    destructor Destroy; override;
  published
    property LibraryName: TFileName read fLibraryName;
  end;


/// an internal function which calls Freemem(p)
// - can be used to free some PUTF8Char pointer allocated by Delphi Getmem()
procedure sqlite3InternalFree(p: pointer); cdecl;

/// an internal function which calls TObject(p).Free
// - can be used to free some Delphi class instance
procedure sqlite3InternalFreeObject(p: pointer); cdecl;

/// an internal function which calls RawByteString(p) := ''
// - can be used to free some Delphi class instance
// - use a local tmp: pointer variable to prepare the reference count, e.g.
// !  tmp := nil;
// !  RawUTF8(tmp) := Text; // fast COW assignment
// !  sqlite3.result_text(Context,tmp,length(Text)+1,sqlite3InternalFreeRawByteString);
procedure sqlite3InternalFreeRawByteString(p: pointer); cdecl;

/// wrapper around sqlite3.result_error() to be called if wrong number of arguments
procedure ErrorWrongNumberOfArgs(Context: TSQLite3FunctionContext);

/// wrapper around sqlite3.result_error() validating the expected number of arguments
function CheckNumberOfArgs(Context: TSQLite3FunctionContext; expected,sent: integer): boolean;

/// create a TSQLite3Module.pzErr UTF-8 text buffer according to the given
// Delphi exception
procedure ExceptionToSqlite3Err(E: Exception; var pzErr: PUTF8Char);

/// set a TSQLVar into a SQlite3 result context
// - will call the corresponding sqlite3.result_*() function and return true,
// or will return false if the TSQLVar type is not handled
function SQLVarToSQlite3Context(const Res: TSQLVar; Context: TSQLite3FunctionContext): boolean;

/// set a UTF-8 string into a SQlite3 result context
// - this function will use copy-on-write assignment of Text, with no memory
// allocation, then let sqlite3InternalFreeRawByteString release its reference count
procedure RawUTF8ToSQlite3Context(const Text: RawUTF8; Context: TSQLite3FunctionContext;
  VoidTextAsNull: boolean);

{$ifndef NOVARIANTS}

/// set a variant value into a SQlite3 result context
// - will call the corresponding sqlite3.result_*() function, using
// SQLVarToSQlite3Context() after a call to VariantToSQLVar()
procedure VariantToSQlite3Context(const Value: Variant; Context: TSQLite3FunctionContext);

/// set a JSON value into a SQlite3 result context
// - a JSON object or array would be returned at plain TEXT, or other simple
// JSON text or number would be returned as the corresponding SQLite3 value
procedure JsonToSQlite3Context(json: PUTF8Char; Context: TSQLite3FunctionContext);

{$endif}

/// set a SQLite3 value into a TSQLVar
// - will call the corresponding sqlite3.value_*() function to retrieve the
// data with the less overhead (e.g. memory allocation or copy) as possible
procedure SQlite3ValueToSQLVar(Value: TSQLite3Value; var Res: TSQLVar);



const
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;
  SQLITE_INDEX_CONSTRAINT_LIKE  = 65;
  SQLITE_INDEX_CONSTRAINT_GLOB  = 66;
  SQLITE_INDEX_CONSTRAINT_REGEXP = 67;

  SQLITE_DENY   = 1;
  SQLITE_IGNORE = 2;

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


  /// SQL statement to get all tables names in the current database file
  // (taken from official SQLite3 documentation)
  SQL_GET_TABLE_NAMES =
    'SELECT name FROM sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%'';';

type
  /// the main possible return codes, including error codes
  TSQLite3ErrorCode = (
    secUnknown,
    secOK,secERROR,secINTERNAL,secPERM,secABORT,secBUSY,secLOCKED,secNOMEM,
    secREADONLY,secINTERRUPT,secIOERR,secCORRUPT,secNOTFOUND,secFULL,secCANTOPEN,
    secPROTOCOL,secEMPTY,secSCHEMA,secTOOBIG,secCONSTRAINT,secMISMATCH,secMISUSE,
    secNOLFS,secAUTH,secFORMAT,secRANGE,secNOTADB, secROW,secDONE);

  /// custom SQLite3 dedicated Exception type
  ESQLite3Exception = class(ESynException)
  protected
    fErrorCode: integer;
    fSQLite3ErrorCode: TSQLite3ErrorCode;
  public
    /// the DB which raised this exception
    DB: TSQLite3DB;
    /// create the exception, getting the message from DB
    constructor Create(aDB: TSQLite3DB; aErrorCode: integer; const aSQL: RawUTF8); reintroduce; overload;
  published
    /// the corresponding error code, e.g. 21 (for SQLITE_MISUSE)
    property ErrorCode: integer read fErrorCode;
    /// the corresponding error code, e.g. secMISUSE
    property SQLite3ErrorCode: TSQLite3ErrorCode read fSQLite3ErrorCode;
  end;

/// convert a SQLite3 result code into a TSQLite3ErrorCode item
function sqlite3_resultToErrorCode(aResult: integer): TSQLite3ErrorCode;

/// convert a SQLite3 result code into the corresponding SQLite constant name
// - e.g. sqlite3_resultToErrorText(SQLITE_OK)='SQLITE_OK'
function sqlite3_resultToErrorText(aResult: integer): RawUTF8;

/// convert a TSQLite3ErrorCode item into the corresponding SQLite constant name
// - e.g. ErrorCodeToText(secOK)='SQLITE_OK'
function ErrorCodeToText(err: TSQLite3ErrorCode): RawUTF8;

/// test the result state of a sqlite3.*() function
// - raise a ESQLite3Exception if the result state is within SQLITE_ERRORS
// - return the result state otherwise (SQLITE_OK,SQLITE_ROW,SQLITE_DONE e.g.)
function sqlite3_check(DB: TSQLite3DB; aResult: integer; const SQL: RawUTF8=''): integer;

var
  /// global access to linked SQLite3 library API calls
  // - you should call sqlite3.open() instead of sqlite3_open() for instance
  // - points either to the statically linked sqlite3.obj, or to an external
  // library (e.g. sqlite3.dll under Windows)
  // - your project should use EITHER SynSQLite3Static unit OR create a
  // TSQLite3LibraryDynamic instance:
  // ! FreeAndNil(sqlite3); // release any previous instance
  // ! sqlite3 := TSQLite3LibraryDynamic.Create;
  // - caller should free the sqlite3 instance only with
  // ! FreeAndNil(sqlite3);
  // to avoid issues with the automatic freeing in finalization section
  sqlite3: TSQLite3Library;



  { ************ objects for high-level access SQLite3 database engine }

type
  /// available file-level write access wait mode of the SQLite3 engine
  // - when synchronous is smFull (which is the default setting), the SQLite
  // database engine will use the xSync method of the VFS to ensure that all
  // content is safely written to the disk surface prior to continuing. This
  // ensures that an operating system crash or power failure will not corrupt
  // the database. FULL synchronous is very safe, but it is also slower.
  // - when synchronous is smNormal, the SQLite database engine will still
  // sync at the most critical moments, but less often than in FULL mode. There
  // is a very small (though non-zero) chance that a power failure at just the
  // wrong time could corrupt the database in NORMAL mode. But in practice,
  // you are more likely to suffer a catastrophic disk failure or some other
  // unrecoverable hardware fault.
  // - when synchronous is smOff, SQLite continues without syncing as soon as
  // it has handed data off to the operating system. If the application running
  // SQLite crashes, the data will be safe, but the database might become
  // corrupted if the operating system crashes or the computer loses power
  // before that data has been written to the disk surface. On the other hand,
  // some operations are as much as 50 or more times faster with synchronous OFF.
  TSQLSynchronousMode = (smOff, smNormal, smFull);

  /// available file-level database connection locking-mode
  // - lmNormal locking-mode (the default unless overridden at compile-time using
  // SQLITE_DEFAULT_LOCKING_MODE), a database connection unlocks the database
  // file at the conclusion of each read or write transaction.
  // - when the locking-mode is set to lmExclusive, the database connection
  // never releases file-locks. The first time the database is read in
  // lmExclusive mode, a shared lock is obtained and held. The first time the
  // database is written, an exclusive lock is obtained and held. Database locks
  // obtained by a connection in lmExclusive mode may be released either by
  // closing the database connection, or by setting the locking-mode back to
  // lmNormal using this pragma and then accessing the database file (for read
  // or write). Simply setting the locking-mode to lmNormal is not enough - locks
  // are not released until the next time the database file is accessed.
  // - lmExclusive gives much better write performance, and could be used when
  // needed, in case of a heavy loaded mORMot server
  TSQLLockingMode = (lmNormal, lmExclusive);

  /// available Run-Time limit categories
  // - as expected by sqlite3.limit() function and TSQLDatabase.Limit property
  // - lcLength The maximum size of any string or BLOB or table row, in bytes.
  // - lcSQLLength The maximum length of an SQL statement, in bytes.
  // - lcColumn The maximum number of columns in a table definition or in the
  // result set of a SELECT or the maximum number of columns in an index or in
  // an ORDER BY or GROUP BY clause.
  // - lcExprDepth The maximum depth of the parse tree on any expression.
  // - lcCompoundSelect The maximum number of terms in a compound SELECT statement.
  // - lcVDBEop The maximum number of instructions in a virtual machine program
  // used to implement an SQL statement. This limit is not currently enforced,
  // though that might be added in some future release of SQLite.
  // - lcFunctionArg The maximum number of arguments on a function.
  // - lcAttached The maximum number of attached databases.
  // - lcLikePatternLength The maximum length of the pattern argument to the
  // LIKE or GLOB operators.
  // - lcVariableNumber The maximum number of parameters in an SQL statement.
  // - lcTriggerDepth The maximum depth of recursion for triggers.
  TSQLLimitCategory = (lcLength, lcSQLLength, lcColumn, lcExprDepth,
    lcCompoundSelect, lcVDBEop, lcFunctionArg, lcAttached, lcLikePatternLength,
    lcVariableNumber, lcTriggerDepth);

  {$M+}
  TSQLDatabase = class;
  {$M-}

  TSQLBlobStream = class;

  PSQLRequest = ^TSQLRequest;

  /// wrapper to a SQLite3 request
  // - defined as a record, so that it may be allocated on the stack
  // - do not forget to call the Close method to release the request resources
  {$ifdef UNICODE}
  TSQLRequest = record
  {$else}
  TSQLRequest = object
  {$endif}
  private
    fDB: TSQLite3DB;
    fRequest: TSQLite3Statement;
    fNextSQL: PUTF8Char;
    fFieldCount: integer;
    function GetReadOnly: Boolean;
    function GetParamCount: integer;

  // 1. general request process
  public
    /// Prepare a UTF-8 encoded SQL statement
    // - compile the SQL into byte-code
    // - parameters ? ?NNN :VV @VV $VV can be bound with Bind*() functions below
    // - raise an ESQLite3Exception on any error, unless NoExcept is TRUE
    function Prepare(DB: TSQLite3DB; const SQL: RawUTF8; NoExcept: boolean=false): integer;
    /// Prepare a WinAnsi SQL statement
    // - behave the same as Prepare()
    function PrepareAnsi(DB: TSQLite3DB; const SQL: WinAnsiString): integer;
    /// Prepare the next SQL command initialized in previous Prepare()
    // - raise an ESQLite3Exception on any error
    function PrepareNext: integer;
    /// Evaluate An SQL Statement, returning the sqlite3.step() result status:
    // - return SQLITE_ROW on success, with data ready to be retrieved via the
    // Field*() methods
    // - return SQLITE_DONE if the SQL commands were executed
    // - raise an ESQLite3Exception on any error
    function Step: integer;
    /// Reset A Prepared Statement Object
    // - reset a prepared statement object back to its initial state,
    // ready to be re-executed.
    // - any SQL statement variables that had values bound to them using the Bind*()
    // function below retain their values. Use BindReset() to reset the bindings
    // - return SQLITE_OK on success, or the previous Step error code
    function Reset: integer;
    /// Execute all SQL statements already prepared by a call to Prepare()
    // - the statement is closed
    // - raise an ESQLite3Exception on any error
    procedure ExecuteAll; overload;
    /// Execute all SQL statements in the aSQL UTF-8 encoded string
    // - internaly call Prepare() then Step then PrepareNext until end of aSQL
    // - Close is always called internaly
    // - raise an ESQLite3Exception on any error
    procedure ExecuteAll(aDB: TSQLite3DB; const aSQL: RawUTF8); overload;
    /// Execute one SQL statement already prepared by a call to Prepare()
    // - the statement is closed
    // - raise an ESQLite3Exception on any error
    procedure Execute; overload;
    /// Execute one SQL statement in the aSQL UTF-8 encoded string
    // - Execute the first statement in aSQL: call Prepare() then Step once
    // - Close is always called internaly
    // - raise an ESQLite3Exception on any error
    procedure Execute(aDB: TSQLite3DB; const aSQL: RawUTF8); overload;
    /// Execute one SQL statement in the aSQL UTF-8 encoded string
    // - Execute the first statement in aSQL: call Prepare() then Step once
    // - Close is always called internaly
    // - returns TRUE on success, and raise no ESQLite3Exception on error, but returns FALSE
    function ExecuteNoException(aDB: TSQLite3DB; const aSQL: RawUTF8): boolean;
    /// Execute a SQL statement which return integers from the aSQL UTF-8 encoded string
    // - Execute the first statement in aSQL
    // - this statement must get (at least) one field/column result of INTEGER
    // - return result as a dynamic array of Int64 in ID
    // - return count of row in integer function result (may be < length(ID))
    // - raise an ESQLite3Exception on any error
    function Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; var ID: TInt64DynArray): integer; overload;
    /// Execute a SQL statement which return one integer from the aSQL UTF-8 encoded string
    // - Execute the first statement in aSQL
    // - this statement must get (at least) one field/column result of INTEGER
    // - return result as an unique Int64 in ID
    // - raise an ESQLite3Exception on any error }
    procedure Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; out ID: Int64); overload;
    /// Execute a SQL statement which return one TEXT value from the aSQL UTF-8 encoded string
    // - Execute the first statement in aSQL
    // - this statement must get (at least) one field/column result of TEXT
    // - raise an ESQLite3Exception on any error
    procedure Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; out Value: RawUTF8); overload;
    /// Execute a SQL statement which return TEXT from the aSQL UTF-8 encoded string
    // - Execute the first statement in aSQL
    // - this statement must get (at least) one field/column result of TEXT
    // - return result as a dynamic array of RawUTF8 in ID
    // - return count of row in integer function result (may be < length(ID))
    // - raise an ESQLite3Exception on any error
    function Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; var Values: TRawUTF8DynArray): integer; overload;
    /// Execute one SQL statement which return the results in JSON format
    // - JSON format is more compact than XML and well supported
    // - Execute the first statement in aSQL
    // - if SQL is '', the statement should have been prepared, reset and bound if necessary
    // - raise an ESQLite3Exception on any error
    // - JSON data is added to TStream, with UTF-8 encoding
    // - if Expand is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expand is false, JSON data is serialized (used in TSQLTableJSON)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data (no conversion into TEXT, as with
    // TSQLTableDB) - so will work for sftBlob, sftBlobDynArray and sftBlobRecord
    // - returns the number of data rows added to JSON (excluding the headers)
    function Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; JSON: TStream;
      Expand: boolean=false): PtrInt; overload;
    /// Execute one SQL statement which return the results in JSON format
    // - use internaly Execute() above with a TRawByteStringStream, and return a string
    // - BLOB field value is saved as Base64, e.g. '"\uFFF0base64encodedbinary"'
    // - returns the number of data rows added to JSON (excluding the headers)
    // in the integer variable mapped by aResultCount (if any)
    // - if any error occurs, the ESQLite3Exception is handled and '' is returned
    function ExecuteJSON(aDB: TSQLite3DB; const aSQL: RawUTF8; Expand: boolean=false;
      aResultCount: PPtrInt=nil): RawUTF8;
    /// Execute all SQL statements in the aSQL UTF-8 encoded string, results will
    // be written as ANSI text in OutFile
    procedure ExecuteDebug(aDB: TSQLite3DB; const aSQL: RawUTF8; var OutFile: Text);
    /// close the Request handle
    // - call it even if an ESQLite3Exception has been raised
    procedure Close;

    /// read-only access to the Request (SQLite3 statement) handle
    property Request: TSQLite3Statement read fRequest;
    /// read-only access to the SQLite3 database handle
    property RequestDB: TSQLite3DB read fDB;
    /// returns true if the current prepared statement makes no direct changes
    // to the content of the database file
    // - Transaction control statements such as BEGIN, COMMIT, ROLLBACK, SAVEPOINT,
    // and RELEASE cause this property to return true, since the statements
    // themselves do not actually modify the database but rather they control the
    // timing of when other statements modify the database. The ATTACH and DETACH
    // statements also cause this property to return true since, while
    // those statements change the configuration of a database connection, they
    // do not make changes to the content of the database files on disk.
    property IsReadOnly: Boolean read GetReadOnly;

  // 2. Bind parameters to a SQL query (for the last prepared statement)
  public
    /// Reset All Bindings On A Prepared Statement
    // - Contrary to the intuition of many, Reset() does not reset the bindings
    // on a prepared statement. Use this routine to reset all host parameter
    procedure BindReset;
    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESQLite3Exception on any error
    procedure BindNull(Param: Integer);
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESQLite3Exception on any error
    procedure Bind(Param: Integer; Value: Int64); overload;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESQLite3Exception on any error
    procedure Bind(Param: Integer; Value: double); overload;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESQLite3Exception on any error
    // - this function will use copy-on-write assignment of Value, with no memory
    // allocation, then let sqlite3InternalFreeRawByteString release the variable
    procedure Bind(Param: Integer; const Value: RawUTF8); overload;
    /// bind a generic VCL string to a parameter
    // - with versions prior to Delphi 2009, you may loose some content here:
    // Bind(Param: integer; Value: RawUTF8) is the prefered method
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESQLite3Exception on any error
    procedure BindS(Param: Integer; const Value: string);
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESQLite3Exception on any error
    procedure Bind(Param: Integer; Data: pointer; Size: integer); overload;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESQLite3Exception on any error
    // - this function will use copy-on-write assignment of Data, with no memory
    // allocation, then let sqlite3InternalFreeRawByteString release the variable
    procedure BindBlob(Param: Integer; const Data: RawByteString);
    /// bind a Blob TCustomMemoryStream buffer to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESQLite3Exception on any error
    procedure Bind(Param: Integer; Data: TCustomMemoryStream); overload;
    /// bind a ZeroBlob buffer to a parameter
    // - uses a fixed amount of memory (just an integer to hold its size) while
    // it is being processed. Zeroblobs are intended to serve as placeholders
    // for BLOBs whose content is later written using incremental BLOB I/O routines
    // (as with TSQLBlobStream created from TSQLDataBase.Blob() e.g.).
    // - a negative value for the Size parameter results in a zero-length BLOB
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESQLite3Exception on any error
    procedure BindZero(Param: Integer; Size: integer);

  // 3. Field attributes after a sucessfull Step() (returned SQLITE_ROW)
  public
    /// the field name of the current ROW
    function FieldName(Col: integer): RawUTF8;
    /// the field index matching this name
    // - return -1 if not found
    function FieldIndex(const aColumnName: RawUTF8): integer;
    /// return the field as a sqlite3.value object handle, first Col is 0
    function FieldValue(Col: integer): TSQLite3Value;
    /// return a field integer value, first Col is 0
    function FieldInt(Col: integer): Int64;
    /// return a field floating point value, first Col is 0
    function FieldDouble(Col: integer): double;
    /// return a field UTF-8 encoded text value, first Col is 0
    function FieldUTF8(Col: integer): RawUTF8;
    /// return a text value value as generic VCL string, first Col is 0
    // - note that prior to Delphi 2009, you may loose content during conversion
    function FieldS(Col: integer): string;
    /// return a field as Win-Ansi (i.e. code page 1252) encoded text value, first Col is 0
    function FieldA(Col: integer): WinAnsiString;
    /// return a field RawUnicode encoded text value, first Col is 0
    function FieldW(Col: integer): RawUnicode;
    /// return a field as a blob value (RawByteString/TSQLRawBlob is an AnsiString),
    // first Col is 0
    function FieldBlob(Col: integer): RawByteString;
    /// return a field as a TStream blob value, first Col is 0
    // - caller shall release the returned TStream instance
    function FieldBlobToStream(Col: integer): TStream;
    /// return TRUE if the column value is NULL, first Col is 0
    function FieldNull(Col: Integer): Boolean;
    /// return the field type of this column
    // - retrieve the "SQLite3" column type as returned by sqlite3.column_type -
    //  i.e. SQLITE_NULL, SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT, or SQLITE_BLOB
    function FieldType(Col: Integer): integer;
    /// return the type of this column, as declared at creation
    // - textual type used for CREATE TABLE of the corresponding column, as
    // returned by sqlite3.column_decltype()
    function FieldDeclaredType(Col: Integer): RawUTF8;
    /// return the generic VCL string type of this column, as declared at creation
    // - textual type used for CREATE TABLE of corresponding column, as
    // returned by sqlite3.column_decltype()
    // - note that prior to Delphi 2009, you may loose content during conversion
    function FieldDeclaredTypeS(Col: Integer): string;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure FieldsToJSON(WR: TJSONWriter; DoNotFetchBlobs: boolean=false);
    /// the column/field count of the current ROW
    // - fields numerotation starts with 0
    property FieldCount: integer read fFieldCount;
    /// the bound parameters count
    property ParamCount: integer read GetParamCount;
  end;

  /// used to retrieve a prepared statement
  TSQLStatementCache = record
    /// associated SQL statement
    StatementSQL: RawUTF8;
    /// associated prepared statement, ready to be executed after binding
    Statement: TSQLRequest;
    /// used to monitor execution time
    Timer: TSynMonitor;
  end;
  /// used to store all prepared statement
  TSQLStatementCacheDynArray = array of TSQLStatementCache;

  /// handle a cache of prepared statements
  // - is defined either as an object either as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  {$ifdef UNICODE}
  TSQLStatementCached = record
  {$else}
  TSQLStatementCached = object
  {$endif}
    /// prepared statements with parameters for faster SQLite3 execution
    // - works for SQL code with ? internal parameters
    Cache: TSQLStatementCacheDynArray;
    /// current number of items in the Cache[] array
    Count: integer;
    /// hashing wrapper associated to the Cache[] array
    Caches: TDynArrayHashed;
    /// the associated SQLite3 database instance
    // - any direct access to this cache list should be protected via DB.Lock
    DB: TSQLite3DB;
    /// intialize the cache
    procedure Init(aDB: TSQLite3DB);
    /// add or retrieve a generic SQL (with ? parameters) statement from cache
    function Prepare(const GenericSQL: RawUTF8; WasPrepared: PBoolean=nil;
      ExecutionTimer: PPPrecisionTimer=nil; ExecutionMonitor: PSynMonitor=nil): PSQLRequest;
    /// used internaly to release all prepared statements from Cache[]
    procedure ReleaseAllDBStatements;
    /// could be used e.g. for statistics
    // - will use internally the function StatementCacheTotalTimeCompare()
    procedure SortCacheByTotalTime(var aIndex: TIntegerDynArray);
  end;

  /// those classes can be used to define custom SQL functions inside a TSQLDataBase
  TSQLDataBaseSQLFunction = class
  protected
    fInternalFunction: TSQLFunctionFunc;
    fSQLName: RawUTF8;
    fFunctionParametersCount: integer;
    function CreateFunction(DB: TSQLite3DB): Integer; virtual;
  public
    /// initialize the corresponding SQL function
    // - expects at least the low-level TSQLFunctionFunc implementation (in
    // sqlite3.create_function() format) and the number of expected parameters
    // - if the function name is not specified, it will be retrieved from the type
    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
    constructor Create(aFunction: TSQLFunctionFunc; aFunctionParametersCount: Integer;
      const aFunctionName: RawUTF8=''); reintroduce;
    /// the internal function prototype
    // - ready to be assigned to sqlite3.create_function() xFunc parameter
    property InternalFunction: TSQLFunctionFunc read fInternalFunction;
    /// the SQL function name, as called from the SQL statement
    // - the same function name may be registered several times with a diverse
    // number of parameters (e.g. to implement optional parameters)
    property FunctionName: RawUTF8 read fSQLName;
    /// the number of parameters expected by the SQL function
    property FunctionParametersCount: integer read fFunctionParametersCount;
  end;

  /// to be used to define custom SQL functions for dynamic arrays BLOB search
  TSQLDataBaseSQLFunctionDynArray = class(TSQLDataBaseSQLFunction)
  protected
    fDummyDynArray: TDynArray;
    fDummyDynArrayValue: pointer;
  public
    /// initialize the corresponding SQL function
    // - if the function name is not specified, it will be retrieved from the type
    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
    // - the SQL function will expect two parameters: the first is the BLOB
    // field content, and the 2nd is the array element to search (set with
    // TDynArray.ElemSave() or with BinToBase64WithMagic(aDynArray.ElemSave())
    // if called via a Client and a JSON prepared parameter)
    // - you should better use the already existing faster SQL functions
    // Byte/Word/Integer/Cardinal/Int64/CurrencyDynArrayContains() if possible
    // (this implementation will allocate each dynamic array into memory before
    // comparison, and will be therefore slower than those optimized versions)
    constructor Create(aTypeInfo: pointer; aCompare: TDynArraySortCompare;
      const aFunctionName: RawUTF8=''); reintroduce;
  end;

  /// Stored Procedure prototype, used by TSQLDataBase.Execute() below
  // - called for every row of a Statement
  // - the implementation may update the database directly by using a
  // local or shared TSQLRequest
  // - the TSQLRequest may be shared and prepared before the call for even
  // faster access than with a local TSQLRequest
  // - no TSQLDataBase or higher levels objects can be used inside this method,
  // since all locking and try..finally protection is outside it
  // - can optionnaly trigger a ESQLite3Exception on any error
  TOnSQLStoredProc = procedure(const Statement: TSQLRequest) of object;

  /// TSQLDataBase.TransactionBegin can be deferred, immediate, or exclusive
  // - tbDeferred means that no locks are acquired on the database until the
  // database is first accessed. Thus with a deferred transaction, the BEGIN
  // statement itself does nothing to the filesystem. Locks are not acquired
  // until the first read or write operation. The first read operation against
  // a database creates a SHARED lock and the first write operation creates a
  // RESERVED lock. Because the acquisition of locks is deferred until they are
  // needed, it is possible that another thread or process could create a
  // separate transaction and write to the database after the BEGIN on the
  // current thread has executed.
  // - If the transaction is tbImmediate, then RESERVED locks are acquired
  // on all databases as soon as the BEGIN command is executed, without waiting
  // for the database to be used. After a BEGIN IMMEDIATE, no other database
  // connection will be able to write to the database or do a BEGIN IMMEDIATE
  // or BEGIN EXCLUSIVE. Other processes can continue to read from the database,
  // however.
  // - A tbExclusive transaction causes EXCLUSIVE locks to be acquired on all
  // databases. After a BEGIN EXCLUSIVE, no other database connection except
  // for read_uncommitted connections will be able to read the database and
  // no other connection without exception will be able to write the database
  // until the transaction is complete.
  TSQLDataBaseTransactionBehaviour = (
    tbDeferred,
    tbImmediate,
    tbExclusive);

  {$M+}
  TSQLDatabaseBackupThread = class;
  {$M-}

  /// callback called asynchronously during TSQLDatabase.BackupBackground()
  // - implementation should return TRUE to continue the process: if the method
  // returns FALSE, backup will be aborted, and destination file deleted
  // - this method allows to monitor the backup process, thanks to
  // TSQLDatabaseBackupThread properties (especialy the Step property)
  // - this method will be executed in the context of the associated
  // TSQLDatabaseBackupThread: so you should use Synchronize() to update the UI
  TSQLDatabaseBackupEvent = function(Sender: TSQLDatabaseBackupThread): boolean of object;

  /// simple wrapper for direct SQLite3 database manipulation
  // - embed the SQLite3 database calls into a common object
  // - thread-safe call of all SQLite3 queries (SQLITE_THREADSAFE 0 in sqlite.c)
  // - can cache last results for SELECT statements, if property UseCache is true:
  //  this can speed up most read queries, for web server or client UI e.g.
  TSQLDataBase = class(TSynPersistentLock)
  protected
    fDB: TSQLite3DB;
    fFileName: TFileName;
    fFileNameWithoutPath: TFileName;
    fPageSize, fFileDefaultPageSize: cardinal;
    fFileDefaultCacheSize: integer;
    fIsMemory: boolean;
    fPassword: RawUTF8;
    fTransactionActive: boolean;
    /// if not nil, cache is used - see UseCache property
    fCache: TSynCache;
    fInternalState: PCardinal;
    fBusyTimeout: Integer;
    fOpenV2Flags: Integer;
    fBackupBackgroundInProcess: TSQLDatabaseBackupThread;
    fBackupBackgroundLastTime: RawUTF8;
    fBackupBackgroundLastFileName: TFileName;
    fUseCacheSize: integer;
    {$ifdef WITHLOG}
    fLogResultMaximumSize: integer;
    fLog: TSynLogClass;
    {$endif}
    /// store TSQLDataBaseSQLFunction instances
    fSQLFunctions: TSynObjectList;
    function GetUseCache: boolean;
    procedure SetUseCache(const Value: boolean);
    procedure SetBusyTimeout(const ms: Integer);
    function GetUserVersion: cardinal;
    procedure SetUserVersion(const Value: cardinal);
    procedure SetWALMode(Value: Boolean);
    function GetWALMode: boolean;
    procedure SetSynchronous(const Value: TSQLSynchronousMode);
    function GetSynchronous: TSQLSynchronousMode;
    procedure SetLockingMode(const Value: TSQLLockingMode);
    function GetLockingMode: TSQLLockingMode;
    function GetCacheSize: cardinal;
    procedure SetCacheSize(const Value: cardinal);
    function GetPageSize: cardinal;
    procedure SetPageSize(const Value: cardinal);
    function GetPageCount: cardinal;
    function GetFileSize: Int64;
    function GetMemoryMappedMB: cardinal;
    procedure SetMemoryMappedMB(const Value: cardinal);
    function GetLimit(Category: TSQLLimitCategory): integer;
    procedure SetLimit(Category: TSQLLimitCategory; Value: integer);
    function GetBackupBackgroundInProcess: boolean;
    function SQLShouldBeLogged(const aSQL: RawUTF8): boolean;
    function GetSQLite3Library: TSQLite3Library; // class function = bug in D2005
  public
    /// enter the internal mutex: called before any DB access
    // - provide the SQL statement about to be executed: handle proper caching
    // - if the SQL statement is void, assume a SELECT statement (no cache flush)
    procedure Lock(const aSQL: RawUTF8); overload;
    /// enter the internal mutex without any cache flush
    // - same as Lock('');
    procedure Lock; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// flush the internal statement cache, and enter the internal mutex
    // - same as Lock('ALTER');
    procedure LockAndFlushCache;
    /// leave the internal mutex: called after any DB access
    procedure UnLock;
      {$ifdef HASINLINE}inline;{$endif}
    /// enter the internal mutex: called before any DB access
    // - provide the SQL statement about to be executed: handle proper caching
    // - if this SQL statement has an already cached JSON response, return it and
    // don't enter the internal mutex: no UnLockJSON() call is necessary
    // - if this SQL statement is not a SELECT, cache is flushed and
    // the next call to UnLockJSON() won't add any value to the cache since
    // this statement is not a SELECT and doesn't have to be cached!
    // - if aResultCount does map to an integer variable, it will be filled
    // with the returned row count of data (excluding field names) in the result
    function LockJSON(const aSQL: RawUTF8; aResultCount: PPtrInt): RawUTF8;
    /// leave the internal mutex: called after any DB access
    // - caller must provide the JSON result for the SQL statement previously set
    //  by LockJSON()
    // - do proper caching of the JSON response for this SQL statement
    procedure UnLockJSON(const aJSONResult: RawUTF8; aResultCount: PtrInt);
    /// (re)open the database from file fFileName
    // - TSQLDatabase.Create already opens the database: this method is to be
    // used only on particular cases, e.g. to close temporary a DB file and
    // allow making a backup on its content
    // - returns the SQLITE_* status code, as retrieved from sqlite3.open()
    // so that it should be SQLITE_OK on success
    function DBOpen: integer; virtual;
    /// close the opened database
    // - TSQLDatabase.Destroy already closes the database: this method is to be
    // used only on particular cases, e.g. to close temporary a DB file and
    // allow making a backup on its content
    // - returns the SQLITE_* status code, as retrieved from sqlite3.close(fDB)
    // so that it should be SQLITE_OK on success
    function DBClose: integer;
    {$ifndef DELPHI5OROLDER}
    /// for SQLite >= 3.11 - enable registation of a custom tokenizer
    // - see details at http://sqlite.org/fts3.html#f3tknzr
    function EnableCustomTokenizer: integer;
    {$endif}
  public
    /// open a SQLite3 database file
    // - open an existing database file or create a new one if no file exists
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run);
    // the password may be a JSON-serialized TSynSignerParams object, or will use
    // AES-OFB-128 after SHAKE_128 with rounds=1000 and a fixed salt on plain
    // password text; note that our custom encryption is not compatible with the
    // official SQLite Encryption Extension module
    // - you can specify some optional flags for sqlite3.open_v2() as
    // SQLITE_OPEN_READONLY or SQLITE_OPEN_READWRITE instead of supplied default
    // value (which corresponds to the sqlite3.open() behavior)
    // - by default, 10000 pages are used to cache data in memory (using around
    // 40 MB of RAM), but you may specify another value for performance tuning
    // - SYSTEMNOCASE collation is added (our custom fast UTF-8 case insensitive
    // UTF8ILComp() function, which is used also in the SQLite3UI unit for
    // coherency and efficiency
    // - ISO8601 collation is added (TDateTime stored as ISO-8601 encoded TEXT)
    // - WIN32CASE and WIN32NOCASE collations are added (use slow but accurate Win32 CompareW)
    // - some additional SQl functions are registered: MOD, SOUNDEX/SOUNDEXFR/SOUNDEXES,
    // RANK, CONCAT, TIMELOG, TIMELOGUNIX, JSONGET/JSONHAS/JSONSET and TDynArray-Blob
    // Byte/Word/Integer/Cardinal/Int64/Currency/RawUTF8DynArrayContains
    // - initialize a internal mutex to ensure that all access to the database is atomic
    // - raise an ESQLite3Exception on any error
    constructor Create(const aFileName: TFileName; const aPassword: RawUTF8='';
      aOpenV2Flags: integer=0; aDefaultCacheSize: integer=10000;
      aDefaultPageSize: integer=4096); reintroduce;
    /// close a database and free its memory and context
    //- if TransactionBegin was called but not commited, a RollBack is performed
    destructor Destroy; override;
    /// Execute all SQL statements in aSQL UTF-8 encoded string
    // - can be prepared with TransactionBegin()
    // - raise an ESQLite3Exception on any error
    procedure ExecuteAll(const aSQL: RawUTF8);
    /// Execute one SQL statements in aSQL UTF-8 encoded string
    // - can be prepared with TransactionBegin()
    // - raise an ESQLite3Exception on any error
    procedure Execute(const aSQL: RawUTF8); overload;
    /// Execute one SQL statement which return integers from the aSQL UTF-8 encoded string
    // - Execute the first statement in aSQL
    // - this statement must get a one field/column result of INTEGER
    // - return result as a dynamic array of RawUTF8, as TEXT result
    // - return count of row in integer function result (may be < length(ID))
    // - raise an ESQLite3Exception on any error
    function Execute(const aSQL: RawUTF8; var ID: TInt64DynArray): integer; overload;
    /// Execute one SQL statement returning TEXT from the aSQL UTF-8 encoded string
    // - Execute the first statement in aSQL
    // - this statement must get (at least) one field/column result of TEXT
    // - return result as a dynamic array of RawUTF8 in ID
    // - return count of row in integer function result (may be < length(ID))
    // - raise an ESQLite3Exception on any error
    function Execute(const aSQL: RawUTF8; var Values: TRawUTF8DynArray): integer; overload;
    /// Execute one SQL statement which returns one integer from the aSQL UTF-8 encoded string
    // - Execute the first statement in aSQL
    // - this statement must get a one field/column result of INTEGER
    // - raise an ESQLite3Exception on any error
    procedure Execute(const aSQL: RawUTF8; out ID: Int64; NoLog: boolean=false); overload;
    /// Execute one SQL statement which returns one UTF-8 encoded string value
    // - Execute the first statement in aSQL
    // - this statement must get a one field/column result of TEXT
    // - raise an ESQLite3Exception on any error
    procedure Execute(const aSQL: RawUTF8; out ID: RawUTF8; NoLog: boolean=false); overload;
    /// Execute one SQL statements in aSQL UTF-8 encoded string
    // - can be prepared with TransactionBegin()
    // - raise no Exception on error, but returns FALSE in such case
    function ExecuteNoException(const aSQL: RawUTF8): boolean;
    /// Seamless execution of a SQL statement which returns one integer
    // - Execute the first statement in aSQL
    // - this statement must get a one field/column result of INTEGER
    // - returns 0 on any error
    function ExecuteNoExceptionInt64(const aSQL: RawUTF8): Int64;
    /// Seamless execution of a SQL statement which returns one UTF-8 encoded string
    // - Execute the first statement in aSQL
    // - this statement must get a one field/column result of TEXT
    // - returns '' on any error
    function ExecuteNoExceptionUTF8(const aSQL: RawUTF8): RawUTF8;
    /// Execute one SQL statement returning its results in JSON format
    // - the BLOB data is encoded as '"\uFFF0base64encodedbinary"'
    function ExecuteJSON(const aSQL: RawUTF8; Expand: boolean=false; aResultCount: PPtrInt=nil): RawUTF8;
    /// begin a transaction
    // - Execute SQL statements with Execute() procedure below
    // - must be ended with Commit on success
    // - must be aborted with Rollback after an ESQLite3Exception raised
    // - The default transaction behavior is tbDeferred
    procedure TransactionBegin(aBehavior: TSQLDataBaseTransactionBehaviour = tbDeferred);
    /// end a transaction: write all Execute() statements to the disk
    procedure Commit;
    /// abort a transaction: restore the previous state of the database
    procedure RollBack;
    /// return the last Insert Rowid
    function LastInsertRowID: Int64;
    /// count the number of rows modified by the last SQL statement
    // - this method returns the number of database rows that were changed or
    // inserted or deleted by the most recently completed SQL statement on the
    // database connection specified by the first parameter. Only changes that
    // are directly specified by the INSERT, UPDATE, or DELETE statement are counted.
    // - wrapper around the sqlite3.changes() low-level function
    function LastChangeCount: integer;
    /// return the number of row changes caused by INSERT, UPDATE or
    // DELETE statements since the database connection was opened
    // - wrapper around the sqlite3.total_changes() low-level function
    function TotalChangeCount: integer;

    /// get all table names contained in this database file
    procedure GetTableNames(var Names: TRawUTF8DynArray);
    /// get all field names for a specified Table
    procedure GetFieldNames(var Names: TRawUTF8DynArray; const TableName: RawUTF8);
    /// check if the given table do exist
    function HasTable(const Name: RawUTF8): boolean;
    /// add a SQL custom function to the SQLite3 database engine
    // - the supplied aFunction instance will be used globally and freed
    // by TSQLDataBase.Destroy destructor
    // - will do nothing if the same function name and parameters count have
    // already been registered (you can register then same function name with
    // several numbers of parameters)
    // - you may use the overloaded function, which is a wrapper around:
    // ! Demo.RegisterSQLFunction(
    // !   TSQLDataBaseSQLFunction.Create(InternalSQLFunctionCharIndex,2,'CharIndex'));
    procedure RegisterSQLFunction(aFunction: TSQLDataBaseSQLFunction); overload;
    /// add a SQL custom function to the SQLite3 database engine
    // - will do nothing if the same function name and parameters count have
    // already been registered (you can register then same function name with
    // several numbers of parameters)
    // - typical use may be:
    // ! Demo.RegisterSQLFunction(InternalSQLFunctionCharIndex,2,'CharIndex');
    procedure RegisterSQLFunction(aFunction: TSQLFunctionFunc;
      aFunctionParametersCount: Integer; const aFunctionName: RawUTF8); overload;
    /// add a SQL custom function for a dynamic array to the database
    // - the resulting SQL function will expect two parameters: the first is the
    // BLOB field content, and the 2nd is the array element to search (as set with
    // TDynArray.ElemSave() or with BinToBase64WithMagic(aDynArray.ElemSave())
    // if called via a Client and a JSON prepared parameter)
    // - if the function name is not specified, it will be retrieved from the type
    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
    // - you should better use the already existing faster SQL functions
    // Byte/Word/Integer/Cardinal/Int64/CurrencyDynArrayContains() if possible
    // (this implementation will allocate each dynamic array into memory before
    // comparison, and will be therefore slower than those optimized versions -
    // but it will be always faster than Client-Server query, in all cases)
    procedure RegisterSQLFunction(aDynArrayTypeInfo: pointer; aCompare: TDynArraySortCompare;
      const aFunctionName: RawUTF8=''); overload;

    /// open a BLOB incrementally for read[/write] access
    // - find a BLOB located in row RowID, column ColumnName, table TableName
    // in database DBName; in other words, the same BLOB that would be selected by:
    // ! SELECT ColumnName FROM DBName.TableName WHERE rowid = RowID;
    // - use after a TSQLRequest.BindZero() to reserve Blob memory
    // - if RowID=0, then the last inserted RowID is used (beware that this
    // value won't be thread-safe, if another thread run another INSERT)
    // - will raise an ESQLite3Exception on any error
    function Blob(const DBName, TableName, ColumnName: RawUTF8;
      RowID: Int64; ReadWrite: boolean=false): TSQLBlobStream;
    /// backup of the opened Database into an external file name
    // - warning: this method won't use the SQLite Online Backup API
    // - database is closed, VACCUUMed, copied, then reopened: it's very fast for
    // small databases, but is blocking and should be an issue
    // - if you use some virtual tables, they won't be restored after backup:
    // this method would probably fail e.g. in the context of mORMot.pas
    function Backup(const BackupFileName: TFileName): boolean;
    /// backup of the opened Database into an external file name
    // - this method will use the SQLite Online Backup API and a dedicated
    // background thread for the process
    // - this will be asynchronous, and would block the main database process
    // only when copying the StepPageNumber numer of pages for each step,
    // waiting StepSleepMS milliseconds before performing the next step:
    // as a result, the copy operation can be done incrementally, by blocks of
    // StepPageNumber pages, in which case the source database does not need
    // to be locked for the duration of the copy, only for the brief periods of
    // time when it is actually being read from: this allows other database
    // users to continue uninterrupted (at least during StepSleepMS
    // milliseconds) while a backup is running
    // - if StepPageNumber is -1, the whole DB will be copied in a single step,
    // therefore in blocking mode, e.g. with BackupBackgroundWaitUntilFinished
    // - if SynLzCompress is TRUE, the backup file would be compressed using
    // FileSynLZ() function - you may use BackupUnSynLZ() class method to
    // uncompress the .dbsynlz file into a proper SQLite3 file
    // - the supplied OnProgress event handler will be called at each step, in
    // the context of the background thread
    // - the background thread will be released when the process is finished
    // - if only one connection to the database does exist (e.g. if you use only
    // one TSQLDataBase instance on the same database file), any modification
    // to the source database during the background process will be included in
    // the backup - so this method will work perfectly e.g. for mORMot.pas
    // - if specified, a password will be used to cypher BackupFileName on disk
    // (it will work only with SynSQLite3Static) - you can uncypher the resulting
    // encrypted database file later via ChangeSQLEncryptTablePassWord()
    // - returns TRUE if backup started as expected, or FALSE in case of error
    // (e.g. if there is already another backup started, if the source or
    // destination databases are locked or invalid, or if the sqlite3.dll is too
    // old and does not support the Online Backup API)
    // - you can also use this method to save an SQLite3 ':memory:' database,
    // perhaps in conjunction with the BackupBackgroundWaitUntilFinished method
    function BackupBackground(const BackupFileName: TFileName;
      StepPageNumber, StepSleepMS: Integer; OnProgress: TSQLDatabaseBackupEvent;
      SynLzCompress: boolean=false; const aPassword: RawUTF8=''): boolean;
    /// background backup to another opened database instance
    // - in respect to BackupBackground method, it will use an existing database
    // the actual process
    // - by design, SynLZCompress or aPassword parameters are unavailable
    function BackupBackgroundToDB(BackupDB: TSQLDatabase;
      StepPageNumber, StepSleepMS: Integer; OnProgress: TSQLDatabaseBackupEvent): boolean;
    /// wait until any previous BackupBackground() is finished
    // - warning: this method won't call the Windows message loop, so should not
    // be called from main thread, unless the UI may become unresponsive: you
    // should better rely on OnProgress() callback for any GUI application
    // - by default, it will wait for ever so that process is finished, but you
    // can set a time out (in seconds) after which the process will be aborted
    // - could be used with BackupBackground() and StepPageNumber=-1 to perform
    // a whole copy of a database in one shot:
    // ! if aDB.BackupBackground('backup.db3',-1,0,nil) then
    // !   aDB.BackupBackgroundWaitUntilFinished;
    procedure BackupBackgroundWaitUntilFinished(TimeOutSeconds: Integer=-1);
    /// uncompress a .dbsynlz backup file as previously compressed with BackupSynLZ()
    // or if SynLZCompress parameter is TRUE for BackupBackground() method
    // - any DestDB file name would be overwritten
    // - returns TRUE on success, FALSE on failure
    class function BackupUnSynLZ(const SourceSynLZ, DestDB: TFileName): boolean;
    /// compress a SQlite3 file into a proprietary but efficient .dbsynlz layout
    // - same format than BackupUnSynLZ() class method or if SynLZCompress
    // parameter is TRUE for BackupBackground() method
    // - the SourceDB file should not be active (e.g. be a backup file), i.e.
    // not currently opened by the SQlite3 engine, otherwise behavior is unknown
    // - returns TRUE on success, FALSE on failure
    class function BackupSynLZ(const SourceDB, DestSynLZ: TFileName;
      EraseSourceDB: boolean): boolean;
    /// returns TRUE if the supplied name is a SQlite3 .dbsynlz compressed file
    // - i.e. on the format generated by the BackupUnSynLZ() class method or
    // if SynLZCompress parameter is TRUE for BackupBackground() method
    class function IsBackupSynLZFile(const SynLZFile: TFileName): boolean;
    /// flush the internal SQL-based JSON cache content
    // - to be called when the regular Lock/LockJSON methods are not called,
    // e.g. with external tables as defined in SQLite3DB unit
    // - will also increment the global InternalState property value (if set)
    procedure CacheFlush;

    /// read-only access to the SQLite3 database handle
    property DB: TSQLite3DB read fDB;
    /// read-only access to the SQlite3 password used for encryption
    // - may be a JSON-serialized TSynSignerParams object, or will use AES-OFB-128
    // after SHAKE_128 with rounds=1000 and a fixed salt on plain password text
    property Password: RawUTF8 read fPassword;
    /// read-only access to the SQLite3 database filename opened without its path
    property FileNameWithoutPath: TFileName read fFileNameWithoutPath;
    /// access to the internal JSON cache, used by ExecuteJSON() method
    // - see UseCache property and CacheFlush method
    property Cache: TSynCache read fCache;
    /// retrieve of define a limit on the current database connection
    // - see TSQLLimitCategory for a details of all available limits
    // - see @http://www.sqlite.org/c3ref/limit.html
    property Limit[Category: TSQLLimitCategory]: integer read GetLimit write SetLimit;
    {$ifdef WITHLOG}
    /// access to the log class associated with this SQLite3 database engine
    // - can be customized, e.g. by overriden TSQLRestServerDB.SetLogClass()
    property Log: TSynLogClass read fLog write fLog;
    /// sets a maximum size (in bytes) to be logged as sllResult rows
    // - by default, is set to 512 bytes, which sounds a good compromise
    // since it does not make sense to log all the JSON content retrieved from
    // the database engine, when a huge SELECT is executed
    property LogResultMaximumSize: integer read fLogResultMaximumSize write fLogResultMaximumSize;
    {$endif}
    /// this integer pointer (if not nil) is incremented when any SQL statement
    // changes the database contents (i.e. any not SELECT statement)
    // - this pointer is thread-safe updated, inside a critical section
    property InternalState: PCardinal read fInternalState write fInternalState;
  published
    /// read-only access to the SQLite3 database filename opened
    property FileName: TFileName read fFileName;
    /// equals TRUE if the SQLite3 database was created as ':memory:'
    // (i.e. SQLITE_MEMORY_DATABASE_NAME)
    property IsMemory: boolean read fIsMemory;
    /// if this property is set, all ExecuteJSON() responses will be cached
    // - cache is flushed on any write access to the DB (any not SELECT statement)
    // - cache is consistent only if ExecuteJSON() Expand parameter is constant
    // - cache is used by TSQLDataBase.ExecuteJSON() and TSQLTableDB.Create()
    property UseCache: boolean read GetUseCache write SetUseCache;
    /// cache size in JSON bytes, to be set before UseCache is set to true
    // - default is 16MB
    property UseCacheSize: integer read fUseCacheSize write fUseCacheSize;
    /// return TRUE if a Transaction begun
    property TransactionActive: boolean read fTransactionActive;
    /// sets a busy handler that sleeps for a specified amount of time
    // (in milliseconds) when a table is locked, before returning an error
    property BusyTimeout: Integer read fBusyTimeout write SetBusyTimeout;
    /// query or change the suggested maximum number of database disk pages
    // that SQLite will hold in memory at once per open database file
    // - DBOpen method will set this cache size to a big 10000 default, which
    // sounds reasonnable in the context of a server application (will use
    // up to 40 MB of memory cache, with the default PageSize of 4096 bytes)
    // - when you change the cache size using the cache_size pragma, the change
    // only endures for the current session. The cache size reverts to the
    // default value when the database is closed and reopened
    // - we do not handle negative values here (i.e. KB of RAM), since it won't
    // work if the linked SQLite3 library is version 3.7.9 and earlier
    property CacheSize: cardinal read GetCacheSize write SetCacheSize;
    /// query or change the page size of the database
    // - the page size must be a power of two between 512 and 65536 inclusive
    // - DBOpen method will set the PageSize to 4096 (if the database is not
    // encrypted), which sounds better than the default 1024 value - you should
    // not have to set this property usually
    // - setting this property will only cause an immediate change in the page
    // size if it is issued while the database is still empty, prior to the
    // first CREATE TABLE statement; if this property is used to specify a new
    // page size just prior to running the VACUUM command and if the database
    // is not in WAL journal mode then VACUUM will change the page size to the
    // new value for the newly created database file
    property PageSize: cardinal read GetPageSize write SetPageSize;
    /// return the total number of pages in the database file
    property PageCount: cardinal read GetPageCount;
    /// return the total number of bytes in the database file
    // - computes PageSize*PageCount
    property FileSize: Int64 read GetFileSize;
    /// query or change the Write-Ahead Logging mode for the database
    // - beginning with version 3.7 of the SQLite3 engine, a new "Write-Ahead Log"
    // option (hereafter referred to as "WAL") is optionaly available
    // - WAL might be very slightly slower (perhaps 1% or 2% slower) than the
    // traditional rollback-journal approach in applications that do mostly reads
    // and seldom write; but WAL provides more concurrency as readers do not block
    // writers and a writer does not block readers. Reading and writing can
    // proceed concurrently. With our SQLite3 framework, it's not needed.
    // - by default, this option is not set: only implement if you really need it,
    // but our SQlite3 framework use locked access to the databse, so there
    // should be no benefit of WAL for the framework; but if you call
    // directly TSQLDatabase instances in your code, it may be useful to you
    property WALMode: Boolean read GetWALMode write SetWALMode;
    /// query or change the SQlite3 file-based syncrhonization mode, i.e. the
    // way it waits for the data to be flushed on hard drive
    // - default smFull is very slow, but achieve 100% ACID behavior
    // - smNormal is faster, and safe until a catastrophic hardware failure occurs
    // - smOff is the fastest, data should be safe if the application crashes,
    // but database file may be corrupted in case of failure at the wrong time
    property Synchronous: TSQLSynchronousMode read GetSynchronous write SetSynchronous;
    /// query or change the SQlite3 file-based locking mode, i.e. the
    // way it locks the file
    // - default lmNormal is ACID and safe
    // - lmExclusive gives better performance in case of a number of write
    // transactions, so can be used to release a mORMot server power: but you
    // won't be able to access the database file from outside the process (like
    // a "normal" database engine)
    property LockingMode: TSQLLockingMode read GetLockingMode write SetLockingMode;
    /// enables or disables disk content access using memory-mapped I/O
    // - 0 to disable it (the default, because of potential disadvantages)
    // - set to a number of Mega Bytes value of memory for the mapping
    // - expects a SQLite3 engine version >= 3.7.17
    // - Memory-Mapped I/O is NOT compatible with password encryption as
    // implemented in our SynSQLite3Static unit
    property MemoryMappedMB: cardinal read GetMemoryMappedMB write SetMemoryMappedMB;
    /// retrieve or set the user_version stored in the SQLite3 database file
    // - user-version is a 32-bit signed integer stored in the database header
    //- it can be used to change the database in case of format upgrade (e.g.
    // refresh some hand-made triggers)
    property user_version: cardinal read GetUserVersion write SetUserVersion;
    /// reflects how the database connection was created in the constructor
    property OpenV2Flags: Integer read fOpenV2Flags;
    /// is set to TRUE while a BackupBackground() process is still running
    // - see also BackupBackgroundWaitUntilFinished() method
    property BackupBackgroundInProcess: boolean read GetBackupBackgroundInProcess;
    /// how much time did the latest BackupBackground() finished process take
    property BackupBackgroundLastTime: RawUTF8 read fBackupBackgroundLastTime;
    /// the latest BackupBackground() process file name
    property BackupBackgroundLastFileName: TFileName read fBackupBackgroundLastFileName;
    /// the SQLite3 library which is currently running
    // - part of TSQLDatabase published properties, to publish e.g. Version
    property SQLite3Library: TSQLite3Library read GetSQLite3Library;
  end;

  /// used to read or write a BLOB Incrementaly
  // - data is read/written directly from/to the SQLite3 BTree
  // - data can be written after a TSQLRequest.BindZero() call to reserve memory
  // - this TStream has a fixed size, but Position property can be used to rewind
  TSQLBlobStream = class(TStream)
  protected
    fBlob: TSQLite3Blob;
    fDB: TSQLite3DB;
    fSize, fPosition: integer;
    fWritable: boolean;
  public
    /// Opens a BLOB located in row RowID, column ColumnName, table TableName
    // in database DBName; in other words, the same BLOB that would be selected by:
    // ! SELECT ColumnName FROM DBName.TableName WHERE rowid = RowID;
    constructor Create(aDB: TSQLite3DB; const DBName, TableName,
      ColumnName: RawUTF8; RowID: Int64; ReadWrite: boolean);
    /// release the BLOB object
    destructor Destroy; override;
    /// read Count bytes from the opened BLOB in Buffer
    function Read(var Buffer; Count: Longint): Longint; override;
    /// write is allowed for in-place replacement (resizing is not allowed)
    // - Create() must have been called with ReadWrite=true
    function Write(const Buffer; Count: Longint): Longint; override;
    /// change the current read position
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// reuse this class instance with another row of the same table
    // - will update the stream size, and also rewind position to the beginning
    // - it is actually faster than creating a new TSQLBlobStream instance
    procedure ChangeRow(RowID: Int64);
    /// read-only access to the BLOB object handle
    property Handle: TSQLite3Blob read fBlob;
  end;

  /// kind of event triggerred during TSQLDatabase.BackupBackground() process
  // - you can use (Sender.Step in backupAnyStep), to check for normal step,
  // or (Sender.Step in backupFinished) to check for process end
  TSQLDatabaseBackupEventStep = (
    backupNone, backupStart, backupSuccess, backupFailure,
    backupStepOk, backupStepBusy, backupStepLocked, backupStepSynLz);

  /// background thread used for TSQLDatabase.BackupBackground() process
  TSQLDatabaseBackupThread = class(TThread)
  protected
    fBackupDestFile: TFileName;
    fSourceDB: TSQLDatabase;
    fDestDB: TSQLDatabase;
    fStepPageNumber, fStepSleepMS: Integer;
    fBackup: TSQLite3Backup;
    fStep: TSQLDatabaseBackupEventStep;
    fStepNumberToFinish, fStepNumberTotal: integer;
    fStepSynLzCompress: boolean;
    fOnProgress: TSQLDatabaseBackupEvent;
    fError: Exception;
    fTimer: TPrecisionTimer;
    fOwnerDest: boolean;
    /// main process
    procedure Execute; override;
  public
    /// initialize the background thread
    // - execution is started immediately - caller may call the WaitFor
    // inherited method to run the process in blocking mode
    constructor Create(Backup: TSQLite3Backup; Source, Dest: TSQLDatabase;
      StepPageNumber,StepSleepMS: Integer; SynLzCompress: boolean;
      OnProgress: TSQLDatabaseBackupEvent; OwnerDest: boolean=true); reintroduce;
    /// the source database of the backup process
    property SourceDB: TSQLDatabase read fSourceDB;
    /// the destination database of the backup process
    property DestDB: TSQLDatabase read fDestDB;
    /// the raised exception in case of backupFailure notification
    property FailureError: Exception read fError;
    /// the backup target database file name
    property BackupDestFile: TFileName read fBackupDestFile;
  published
    /// the current state of the backup process
    // - only set before a call to TSQLDatabaseBackupEvent
    property Step: TSQLDatabaseBackupEventStep read fStep;
    /// the number of pages which remain before end of backup
    // - only set before a call to TSQLDatabaseBackupEvent with backupStep* event
    property StepNumberToFinish: integer read fStepNumberToFinish;
    /// the number of pages for the whole database
    // - only set before a call to TSQLDatabaseBackupEvent with backupStep* event
    property StepNumberTotal: integer read fStepNumberTotal;
    /// if .dbsynlz compression would be done on the backup file
    // - would use FileSynLZ(), so compress in chunks of 128 MB
    property StepSynLzCompress: boolean read fStepSynLzCompress;
  end;

const
  /// identify the iterative step events during TSQLDatabase.BackupBackground()
  // - you can use (Sender.Step in backupAnyStep), to check for normal step
  backupAnyStep = [backupStepOk,backupStepBusy,backupStepLocked,backupStepSynLz];
  /// identify the end step events during TSQLDatabase.BackupBackground()
  // - you can use (Sender.Step in backupFinished) to check for process end
  backupFinished = [backupSuccess,backupFailure];


{$ifdef WITHLOG}
var
  /// the TSynLog class used for logging for all our SynSQlite3 related functions
  // - you may override it with TSQLLog, if available from mORMot.pas
  // - since not all exceptions are handled specificaly by this unit, you
  // may better use a common TSynLog class for the whole application or module
  SynSQLite3Log: TSynLogClass = TSynLog;
{$endif}


/// check from the file beginning if sounds like a valid SQLite3 file
// - returns true if a database file is encrypted or not
// - optional retrieve the file page size from header
function IsSQLite3File(const FileName: TFileName; PageSize: PInteger=nil): boolean;

/// check if sounds like an encrypted SQLite3 file
function IsSQLite3FileEncrypted(const FileName: TFileName): boolean;

/// comparison function using TSQLStatementCache.Timer.TimeInMicroSec
function StatementCacheTotalTimeCompare(const A,B): integer;


const
  /// a magic text constant which will prevent any JSON result to be cached
  // in TSQLDataBase, if present in the SQL statement
  // - to be used e.g. when you put some pointers as bound parameters
  SQLDATABASE_NOCACHE: RawUTF8 = '/*nocache*/';

  /// the "magic" number used to identify .dbsynlz compressed files, as
  // created by TSQLDataBase.BackupSynLZ() or if SynLZCompress parameter is TRUE
  // for the TSQLDataBase.BackupBackground() method
  // - note that the SynDBExplorer tool is able to recognize such files, and
  // open them directly - or use the DBSynLZ.dpr command-line sample tool
  SQLITE3_MAGIC = $ABA5A5AB;

  /// the "magic" 16 bytes header stored at the begining of every SQlite3 file
  SQLITE_FILE_HEADER: array[0..15] of AnsiChar = 'SQLite format 3';
var
  SQLITE_FILE_HEADER128: THash128Rec absolute SQLITE_FILE_HEADER;


implementation

{ ************ direct access to sqlite3.c / sqlite3.obj consts and functions }

function IsSQLite3File(const FileName: TFileName; PageSize: PInteger): boolean;
var F: THandle;
    Header: THash256Rec;
begin
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if F=INVALID_HANDLE_VALUE then
    result := false else begin
    result := (FileRead(F,Header,sizeof(Header))=SizeOf(Header)) and
              (Header.d0=SQLITE_FILE_HEADER128.Lo) and
              // don't check header 8..15 (may equal encrypted bytes 16..23)
              (Header.b[21]=64) and (Header.b[22]=32) and (Header.b[23]=32);
    if result and (PageSize<>nil) then
      // header bytes 16..23 are always stored unencrypted
      PageSize^ := Integer(Header.b[16]) shl 8+Header.b[17];
    FileClose(F);
  end;
end;

function IsSQLite3FileEncrypted(const FileName: TFileName): boolean;
var F: THandle;
    Header: THash256Rec;
begin // see CodecEncrypt/CodecDecrypt in SynSQLite3Static
  result := false;
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if F=INVALID_HANDLE_VALUE then
    exit;
  if (FileRead(F,Header,SizeOf(Header))=SizeOf(Header)) and
     (Header.d0=SQLITE_FILE_HEADER128.Lo) and
     // header bytes 8..15 are encrypted bytes 16..23
     (Header.d1<>SQLITE_FILE_HEADER128.Hi) and
     // header bytes 16..23 are stored unencrypted
     (Header.b[21]=64) and (Header.b[22]=32) and (Header.b[23]=32) then
      result := true;
  FileClose(F);
end;


// ************ objects to access SQLite3 database engine


{ TSQLDataBase }

function TSQLDataBase.Blob(const DBName, TableName, ColumnName: RawUTF8;
  RowID: Int64; ReadWrite: boolean): TSQLBlobStream;
begin
  if self=nil then begin
    result := nil;
    exit; // avoid GPF in case of call from a static-only server
  end;
  Lock;
  try
    if RowID=0 then
      RowID := LastInsertRowID; // warning: won't work on multi-thread process
    result := TSQLBlobStream.Create(DB,DBName,TableName,ColumnName,RowID,ReadWrite);
  finally
    UnLock;
  end;
end;

procedure TSQLDataBase.Rollback;
begin
  if (self=nil) or not fTransactionActive then
    exit;
  Execute('ROLLBACK TRANSACTION;');
  fTransactionActive := false;
end;

procedure TSQLDataBase.TransactionBegin(aBehavior: TSQLDataBaseTransactionBehaviour = tbDeferred);
const
  TBTOKENS: array[TSQLDataBaseTransactionBehaviour] of RawUTF8 = (
    '', 'IMMEDIATE ', 'EXCLUSIVE '); // see http://www.sqlite.org/lang_transaction.html
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  if fTransactionActive then
    try
      Execute('ROLLBACK TRANSACTION;');
    finally
      fTransactionActive := false;
    end;
  Execute('BEGIN '+TBTOKENS[aBehavior]+'TRANSACTION;');
  fTransactionActive := true;
end;

procedure TSQLDataBase.Commit;
begin
  if (Self<>nil) and fTransactionActive then
  try
    Execute('COMMIT TRANSACTION;');
  finally
    fTransactionActive := false;
  end;
end;

{ from WladiD about all SQLite3 collation functions:
  If a field with your custom collate ISO8601 is empty '' (not NULL),
  then SQLite calls the registered collate function with s1len=0 or s2len=0,
  but the pointers s1 or s2 map to the string of the previous call }

function Utf16SQLCompCase(CollateParam: pointer; s1Len: integer; S1: pointer;
    s2Len: integer; S2: pointer) : integer; cdecl;
begin
  if s1Len<=0 then
    if s2Len<=0 then
      result := 0 else
      result := -1 else
    if s2Len<=0 then
      result := 1 else
      result := CompareStringW(
        LOCALE_USER_DEFAULT,0,S1,s1len shr 1,S2,s2Len shr 1)-2;
end;

function Utf16SQLCompNoCase(CollateParam: pointer; s1Len: integer; s1: pointer;
    s2Len: integer; s2: pointer) : integer; cdecl;
begin
  if s1Len<=0 then
    if s2Len<=0 then
      result := 0 else
      result := -1 else
  if s2Len<=0 then
    result := 1 else
    result := CompareStringW(
      LOCALE_USER_DEFAULT,NORM_IGNORECASE,S1,s1len shr 1,S2,s2Len shr 1)-2;
end;

function Utf8SQLCompNoCase(CollateParam: pointer; s1Len: integer; s1: pointer;
    s2Len: integer; s2: pointer) : integer; cdecl;
begin
  if s1Len<=0 then
    if s2Len<=0 then
      result := 0 else
      result := -1 else
  if s2Len<=0 then
    result := 1 else
    result := UTF8ILComp(s1,s2,s1Len,s2Len);
end;

function Utf8SQLDateTime(CollateParam: pointer; s1Len: integer; s1: pointer;
    s2Len: integer; s2: pointer) : integer; cdecl;
var V1,V2: TDateTime; // will handle up to .sss milliseconds resolution
begin
  if s1Len<=0 then // see WladiD note above
    s1 := nil;
  if s2Len<=0 then
    s2 := nil;
  if s1=s2 then
    result := 0 else begin
    Iso8601ToDateTimePUTF8CharVar(s1,s1Len,V1);
    Iso8601ToDateTimePUTF8CharVar(s2,s2Len,V2);
    if (V1=0) or (V2=0) then // any invalid date -> compare as UTF-8 strings
      result := UTF8ILComp(s1,s2,s1Len,s2Len) else
      if SameValue(V1,V2,1/MSecsPerDay) then
        result := 0 else
        if V1<V2 then
          result := -1 else
          result := +1;
  end;
end;

const
  NULCHAR: AnsiChar = #0;

function SQLVarToSQlite3Context(const Res: TSQLVar; Context: TSQLite3FunctionContext): boolean;
var tmp: array[0..31] of AnsiChar;
begin
  case Res.VType of
    ftNull:
      sqlite3.result_null(Context);
    ftInt64:
      sqlite3.result_int64(Context,Res.VInt64);
    ftDouble:
      sqlite3.result_double(Context,Res.VDouble);
    ftCurrency:
      sqlite3.result_double(Context,Res.VCurrency);
    ftDate: begin
      DateTimeToIso8601ExpandedPChar(Res.VDateTime,tmp,'T',svoDateWithMS in Res.Options);
      sqlite3.result_text(Context,tmp,-1,SQLITE_TRANSIENT_VIRTUALTABLE);
    end;
    // WARNING! use pointer(integer(-1)) instead of SQLITE_TRANSIENT=pointer(-1)
    // due to a bug in SQLite3 current implementation of virtual tables in Win64
    ftUTF8:
      if Res.VText=nil then
       sqlite3.result_text(Context,@NULCHAR,0,SQLITE_STATIC) else
       sqlite3.result_text(Context,Res.VText,-1,SQLITE_TRANSIENT_VIRTUALTABLE);
    ftBlob:
      sqlite3.result_blob(Context,Res.VBlob,Res.VBlobLen,SQLITE_TRANSIENT_VIRTUALTABLE);
    else begin
      sqlite3.result_null(Context);
      {$ifdef WITHLOG}
      SynSQLite3Log.DebuggerNotify(sllWarning,'SQLVarToSQlite3Context(%)',[ord(Res.VType)]);
      {$endif}
      result := false; // not handled type (will set null value)
      exit;
    end;
  end;
  result := true;
end;

procedure SQlite3ValueToSQLVar(Value: TSQLite3Value; var Res: TSQLVar);
var ValueType: Integer;
begin
  Res.Options := [];
  ValueType := sqlite3.value_type(Value);
  case ValueType of
  SQLITE_NULL:
    Res.VType := ftNull;
  SQLITE_INTEGER: begin
    Res.VType := ftInt64;
    Res.VInt64 := sqlite3.value_int64(Value);
  end;
  SQLITE_FLOAT: begin
    Res.VType := ftDouble;
    Res.VDouble := sqlite3.value_double(Value);
  end;
  SQLITE_TEXT:  begin
    Res.VType := ftUTF8;
    Res.VText := sqlite3.value_text(Value);
  end;
  SQLITE_BLOB: begin
    Res.VType := ftBlob;
    Res.VBlobLen := sqlite3.value_bytes(Value);
    Res.VBlob := sqlite3.value_blob(Value);
  end;
  else begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify(sllWarning,'SQlite3ValueToSQLVar(%)',[ValueType]);
    {$endif}
    Res.VType := ftUnknown;
  end;
  end;
end;

procedure ErrorWrongNumberOfArgs(Context: TSQLite3FunctionContext);
begin
  sqlite3.result_error(Context, 'wrong number of arguments');
end;

function CheckNumberOfArgs(Context: TSQLite3FunctionContext; expected,sent: integer): boolean;
var msg: ShortString;
begin
  if sent<>expected then begin
    FormatShort('wrong number of arguments: expected %, got %',[expected,sent],msg);
    sqlite3.result_error(Context,@msg[1],ord(msg[0]));
    result := false;
  end else
    result := true;
end;

procedure ExceptionToSqlite3Err(E: Exception; var pzErr: PUTF8Char);
var U: RawUTF8;
begin
  U := StringToUTF8(E.Message);
  pzErr := sqlite3.malloc(length(U));
  MoveFast(pointer(U)^,pzErr^,length(U));
end;

procedure InternalSoundex(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
begin
  if CheckNumberOfArgs(Context,1,argc) then
    sqlite3.result_int64(Context, SoundExUTF8(sqlite3.value_text(argv[0])));
end;

procedure InternalSoundexFr(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
begin
  if CheckNumberOfArgs(Context,1,argc) then
    sqlite3.result_int64(Context, SoundExUTF8(sqlite3.value_text(argv[0]),nil,sndxFrench));
end;

procedure InternalSoundexEs(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
begin
  if CheckNumberOfArgs(Context,1,argc) then
    sqlite3.result_int64(Context, SoundExUTF8(sqlite3.value_text(argv[0]),nil,sndxSpanish));
end;

procedure InternalMod(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var A1, A2: Int64;
begin // implements the MOD() function, just like Oracle and others
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit; // two parameters expected
  end;
  A1 := sqlite3.value_int64(argv[0]);
  A2 := sqlite3.value_int64(argv[1]);
  if A2=0 then // avoid computation exception, returns NULL
    sqlite3.result_null(Context) else
    sqlite3.result_int64(Context,A1 mod A2);
end;

procedure InternalTimeLog(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var TimeLog: TTimeLogBits;
begin
  if argc<>1 then begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  TimeLog.Value := sqlite3.value_int64(argv[0]);
  RawUTF8ToSQlite3Context(TimeLog.Text(True,'T'),Context,false);
end;

procedure InternalTimeLogUnix(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var TimeLog: TTimeLogBits;
begin
  if argc<>1 then begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  TimeLog.Value := sqlite3.value_int64(argv[0]);
  sqlite3.result_int64(Context,TimeLog.ToUnixTime);
end;

procedure InternalRank(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
// supplies the same "RANK" internal function as proposed in
// http://www.sqlite.org/fts3.html#appendix_a
var MI: PFTSMatchInfo;
    p,c: integer;
    score: Double;
begin
  if argc>=1 then begin
    MI := sqlite3.value_blob(argv[0]);
    // rank(nil) for example select rank(matchinfo(tabName)) without corresponding MATCH clause
    if MI=nil then begin
       sqlite3.result_double(Context,0);
       exit;
    end;
    if argc=MI^.nCol+1 then begin
      score := 0;
      for p := 1 to MI^.nPhrase do
        for c := 1 to MI^.nCol do
        with MI^.hits[c] do
          if this_row>0 then
            score := score+(this_row/all_rows)*sqlite3.value_double(argv[c]);
      sqlite3.result_double(Context,score);
      exit; // success: don't call sqlite3.result_error()
    end;
  end;
  ErrorWrongNumberOfArgs(Context);
end;

procedure sqlite3InternalFree(p: pointer); cdecl;
begin
  Freemem(p);
end;

procedure sqlite3InternalFreeObject(p: pointer); cdecl;
begin
  TObject(p).Free;
end;

procedure sqlite3InternalFreeRawByteString(p: pointer); cdecl;
begin
  RawByteString(p) := '';
end;


// supplies a CONCAT() function to process fast string concatenation
type
  PConcatRec = ^TConcatRec;
  TConcatRec = record
    result: PUTF8Char;
    resultlen: PtrInt;
  end;

procedure InternalConcatStep(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var sep, txt: PUTF8Char;
    seplen, txtlen: PtrInt;
begin
  if argc=2 then
    with PConcatRec(sqlite3.aggregate_context(Context,sizeof(TConcatRec)))^ do begin
      // +1 below for adding a final #0
      txt := sqlite3.value_text(argv[0]);
      txtlen := SynCommons.StrLen(txt);
      if result=nil then
        GetMem(result,txtlen+1)
      else begin
        sep := sqlite3.value_text(argv[1]);
        seplen := SynCommons.StrLen(sep);
        ReallocMem(result,resultlen+txtlen+seplen+1);
        MoveFast(sep^,result[resultlen],seplen);
        inc(resultlen,seplen);
      end;
      MoveFast(txt^,result[resultlen],txtlen+1);
      inc(resultlen,txtlen);
    end else
    ErrorWrongNumberOfArgs(Context);
end;

procedure InternalConcatFinal(Context: TSQLite3FunctionContext); cdecl;
begin
  with PConcatRec(sqlite3.aggregate_context(Context,sizeof(TConcatRec)))^ do
    // sqlite3InternalFree will call Freemem(PConcatRec()^.result)
    sqlite3.result_text(Context,result,resultlen+1,sqlite3InternalFree);
end;

procedure InternalIntegerDynArray(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var Blob: pointer;
    PI: PIntegerArray;
    Count: integer;
begin // SQL function: IntegerDynArrayContains(BlobField,10) returning a boolean
  if not CheckNumberOfArgs(Context,2,argc) then
    exit;
  Blob := sqlite3.value_blob(argv[0]);
  if Blob<>nil then begin
    PI := IntegerDynArrayLoadFrom(Blob,Count); // fast map into in-memory array
    if not IntegerScanExists(pointer(PI),Count,sqlite3.value_int64(argv[1])) then
      Blob := nil;
  end;
  sqlite3.result_int64(Context,Int64(Blob<>nil));
end;

procedure InternalSimpleInt64DynArray(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var Blob: pointer;
    Count, ElemSize: integer;
    V: Int64;
begin // Byte/Word/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
  // for currency, expect I64 value = aCurrency*10000 = PInt64(@aCurrency)^
  if not CheckNumberOfArgs(Context,2,argc) then
    exit;
  Blob := sqlite3.value_blob(argv[0]);
  if Blob<>nil then begin // search into direct in-memory mapping (no allocation)
    Blob := SimpleDynArrayLoadFrom(Blob,sqlite3.user_data(Context),Count,ElemSize,{nohash32=}true);
    if Blob<>nil then begin
      V := sqlite3.value_int64(argv[1]);
      sqlite3.result_int64(Context,Int64(true)); // exit will report value found
      if AnyScanExists(Blob,@V,Count,ElemSize) then
        exit;
    end;
  end;
  sqlite3.result_int64(Context,Int64(false)); // not found
end;

procedure InternalRawUTF8DynArray(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var Blob: pointer;
    Value: PUTF8Char;
begin // SQL function: RawUTF8DynArrayContainsCase/NoCase(BlobField,'Text') returning a boolean
  if not CheckNumberOfArgs(Context,2,argc) then
    exit;
  Blob := sqlite3.value_blob(argv[0]);
  if Blob<>nil then begin
    Value := sqlite3.value_text(argv[1]);
    if RawUTF8DynArrayLoadFromContains(Blob,Value,SynCommons.StrLen(Value),
       sqlite3.user_data(Context)=nil)<0 then
      Blob := nil;
  end;
  sqlite3.result_int64(Context,Int64(Blob<>nil));
end;

procedure RawUTF8ToSQlite3Context(const Text: RawUTF8; Context: TSQLite3FunctionContext;
  VoidTextAsNull: boolean);
var tmp: pointer;
begin
  if Text='' then
    if VoidTextAsNull then
      sqlite3.result_null(Context) else
      sqlite3.result_text(Context,@NULCHAR,0,SQLITE_STATIC) else begin
      tmp := nil;
      RawUTF8(tmp) := Text; // fast COW assignment
      sqlite3.result_text(Context,tmp,length(Text)+1,sqlite3InternalFreeRawByteString);
    end;
end;

{$ifndef NOVARIANTS}

procedure VariantToSQlite3Context(const Value: Variant; Context: TSQLite3FunctionContext);
var res: TSQLVar;
    tmp: RawByteString;
begin
  VariantToSQLVar(Value,tmp,res);
  SQLVarToSQlite3Context(res,Context);
end;

procedure JsonToSQlite3Context(json: PUTF8Char; Context: TSQLite3FunctionContext);
var tmp: Variant;
    start: PUTF8Char;
begin
  if json=nil then
    sqlite3.result_null(Context) else begin
    start := GotoNextNotSpace(json);
    if start^ in ['[','{'] then begin
      // JSON object or array is returned as plain TEXT
      json := GotoNextJSONObjectOrArray(start);
      if json=nil then
        sqlite3.result_null(Context) else begin
        json^ := #0; // truncate to the matching object or array
        sqlite3.result_text(Context,start,json-start+1,SQLITE_TRANSIENT);
      end;
    end else
      // JSON simple types (text, numbers) would be converted via a variant
      if VariantLoadJSON(tmp,start,nil,nil)=nil then
        sqlite3.result_null(Context) else
        VariantToSQlite3Context(tmp,Context);
  end;
end;

{$else}

procedure JsonToSQlite3Context(json: PUTF8Char; Context: TSQLite3FunctionContext);
var j: RawJSON;
begin // JsonGet() would return the raw JSON for Delphi 5
  GetJSONItemAsRawJSON(json,j);
  RawUTF8ToSQlite3Context(j,Context,true);
end;

{$endif}

procedure InternalJsonGet(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
  function returnObject(w: PUTF8Char): boolean;
  begin
    if w<>nil then
      repeat
        case w^ of
        #0: break;
        ',','*': begin
          result := true;
          exit;
        end;
        end;
        inc(w);
      until false;
    result := false;
  end;
var where,json: PUTF8Char;
begin // JsonGet(VariantField,'PropName') returns the value of a JSON object
      // JsonGet(VariantField,'Obj1.Obj2.PropName') to search by path
      // JsonGet(VariantField,0) returns the 1st item in the JSON array
      // JsonGet(VariantField,'Prop1,Prop2') returns the values as a JSON object
      // JsonGet(VariantField,'Prop*') returns the values as a JSON object
      // JsonGet(VariantField,'Obj1.Obj2.Prop1,Obj1.Prop2') to search by path
      // JsonGet(VariantField,'Obj1.Obj2.Prop*,Obj1.Prop2') to search by path
  if not CheckNumberOfArgs(Context,2,argc) then
    exit;
  if sqlite3.value_type(argv[0])<>SQLITE_TEXT then
    sqlite3.result_null(Context) else
  case sqlite3.value_type(argv[1]) of // fast SAX search (no memory allocation)
  SQLITE_TEXT: begin
    json := sqlite3.value_text(argv[0]);
    where := sqlite3.value_text(argv[1]);
    if returnObject(where) then
      RawUTF8ToSQlite3Context(JsonObjectsByPath(json,where),Context,true) else
      JsonToSQlite3Context(JsonObjectByPath(json,where),Context);
  end;
  SQLITE_INTEGER: begin
    json := JSONArrayItem(sqlite3.value_text(argv[0]),sqlite3.value_int64(argv[1]));
    JsonToSQlite3Context(json,Context);
  end;
  else
    sqlite3.result_null(Context);
  end;
end;

procedure InternalJsonHas(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
begin // JsonHas(VariantField,'PropName') returns TRUE if matches a JSON object property
      // JsonHas(VariantField,'Obj1.Obj2.PropName') to search by path
      // JsonHas(VariantField,0) returns TRUE if the JSON array has at least one item
  if not CheckNumberOfArgs(Context,2,argc) then
    exit;
  if sqlite3.value_type(argv[0])<>SQLITE_TEXT then
    sqlite3.result_int64(Context,Int64(false)) else
  case sqlite3.value_type(argv[1]) of // fast SAX search (no memory allocation)
  SQLITE_TEXT:
    sqlite3.result_int64(Context,ord(JsonObjectByPath(
      sqlite3.value_text(argv[0]),sqlite3.value_text(argv[1]))<>nil));
  SQLITE_INTEGER:
    sqlite3.result_int64(Context,ord(JSONArrayItem(
      sqlite3.value_text(argv[0]),sqlite3.value_int64(argv[1]))<>nil));
  else
    sqlite3.result_int64(Context,Int64(false));
  end;
end;

{$ifndef NOVARIANTS}
procedure InternalJsonSet(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var doc: TDocVariantData;
    json: PUTF8Char;
    tmp: RawUTF8;
    v: PVariant;
begin // JsonSet(VariantField,'PropName','abc') to set a value
      // JsonSet(VariantField,'Obj1.Obj2.PropName','def') to set by path
  if not CheckNumberOfArgs(Context,3,argc) then
    exit;
  if sqlite3.value_type(argv[0])<>SQLITE_TEXT then
    sqlite3.result_null(Context) else begin
    json := sqlite3.value_text(argv[0]);
    FastSetString(tmp,json,SynCommons.StrLen(json));
    doc.InitJSONInPlace(pointer(tmp),JSON_OPTIONS_FAST);
    v := doc.GetPVariantByPath(sqlite3.value_text(argv[1]));
    if v<>nil then begin
      json := sqlite3.value_text(argv[2]);
      FastSetString(tmp,json,SynCommons.StrLen(json));
      VariantLoadJSON(v^,pointer(tmp),nil,@JSON_OPTIONS[true]);
      RawUTF8ToSQlite3Context(doc.ToJSON,Context,false);
    end else begin
      FastSetString(tmp,json,SynCommons.StrLen(json));
      RawUTF8ToSQlite3Context(tmp,Context,false);
    end;
  end;
end;
{$endif}

constructor TSQLDataBase.Create(const aFileName: TFileName; const aPassword: RawUTF8;
  aOpenV2Flags, aDefaultCacheSize,aDefaultPageSize: integer);
var result: integer;
begin
  inherited Create; // initialize fSafe
  if sqlite3=nil then
    raise ESQLite3Exception.CreateUTF8('%.Create: No SQLite3 libray available'+
      ' - you shall either add SynSQLite3Static to your project uses clause, '+
      'or run sqlite3 := TSQLite3LibraryDynamic.Create(..)',[self]);
  {$ifdef WITHLOG}
  fLog := SynSQLite3Log; // leave fLog=nil if no Logging wanted
  fLogResultMaximumSize := 512;
  {$endif}
  if SysUtils.Trim(aFileName)='' then
    raise ESQLite3Exception.CreateUTF8('%.Create('''')',[self]);
  if aOpenV2Flags=0 then
    fOpenV2Flags := SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE else
    fOpenV2Flags := aOpenV2Flags;
  fFileDefaultPageSize := aDefaultPageSize;
  fFileDefaultCacheSize := aDefaultCacheSize;
  if (fOpenV2Flags<>(SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)) and
     not Assigned(sqlite3.open_v2) then
    raise ESQLite3Exception.CreateUTF8(
      'Your % version of SQLite3 does not support custom OpenV2Flags=%',
      [sqlite3.libversion,fOpenV2Flags]);
  fFileName := aFileName;
  if fFileName=SQLITE_MEMORY_DATABASE_NAME then
    fIsMemory := true else
    fFileNameWithoutPath := ExtractFileName(fFileName);
  fPassword := aPassword;
  fUseCacheSize := 16384*1024;
  fSQLFunctions := TSynObjectList.Create;
  result := DBOpen;
  if result<>SQLITE_OK then
    raise ESQLite3Exception.Create(fDB,result,'DBOpen');
end;

destructor TSQLDataBase.Destroy;
{$ifdef WITHLOG}
var FPCLog: ISynLog;
begin
  FPCLog := fLog.Enter('Destroy %',[fFileNameWithoutPath],self);
{$else}
begin
{$endif}
  if DB<>0 then
  try
    Rollback; // any unfinished transaction is rollbacked
  finally
    (*
    { Applications should finalize all prepared statements and close all BLOB handles
      associated with the sqlite3 object prior to attempting to close the object }
    repeat
      S := sqlite3.next_stmt(DB,0); // 0: get first prepared statement for DB
      if S=0 then
        break;
      // if code was correctly protected with try/finally, as in
      // TSQLDataBase.Execute() and TSQLRequest.Execute(), we should never go here
      // -> BUT it seems that the FTS3 leaves some statements open at closing
      // assert(false,FileName); // debug purpose, but not FTS3 ready
    until not (sqlite3.finalize(S) in [SQLITE_OK,SQLITE_ABORT]);
    { BUT the problem is that if you use FTS3, the statements will be released
      twice (i.e. one time above and next time in sqlite3.close below),
      so some GPF will occur :(
    -> we don't release any statement in case of FTS3 usage, and rely on our
      framework, which protects all SQL statements with try..finally clauses }
    *)
    DBClose;
  end;
  FillZero(fPassword);
  fCache.Free;
  fSQLFunctions.Free;
  inherited Destroy;
end;

function TSQLDataBase.SQLShouldBeLogged(const aSQL: RawUTF8): boolean;
begin
  result := false;
  {$ifdef WITHLOG}
  if (self=nil) or (fLog=nil) or not (sllSQL in fLog.Family.Level) then
    exit;
  if not IdemPChar(pointer(aSQL),'PRAGMA ') or (PosEx('=',aSQL)>0) then
    result := true;
  {$endif}
end;

procedure TSQLDataBase.ExecuteAll(const aSQL: RawUTF8);
var R: TSQLRequest;
{$ifdef WITHLOG} log: ISynLog; {$endif}
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  {$ifdef WITHLOG}
  if SQLShouldBeLogged(aSQL) then begin
    log := fLog.Enter(self{$ifndef DELPHI5OROLDER},'ExecuteAll'{$endif});
    if log<>nil then
      log.Log(sllSQL,aSQL,self,4096);
  end;
  {$endif WITHLOG}
  LockAndFlushCache; // don't trust aSQL -> assume modify -> inc(InternalState^)
  try
    R.ExecuteAll(DB,aSQl);
  finally
    UnLock;
  end;
end;

procedure TSQLDataBase.Execute(const aSQL: RawUTF8);
var R: TSQLRequest;
    Timer: TPrecisionTimer;
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  Timer.Start;
  Lock(aSQL); // run one statement -> we can trust isSelect()
  try
    R.Execute(DB,aSQL);
  finally
    UnLock;
    {$ifdef WITHLOG}
    fLog.Add.Log(sllSQL,'% % %',[Timer.Stop,FileNameWithoutPath,aSQL],self);
    {$endif}
  end;
end;

function TSQLDataBase.Execute(const aSQL: RawUTF8; var ID: TInt64DynArray): integer;
var R: TSQLRequest;
{$ifdef WITHLOG} log: ISynLog; {$endif}
begin
  if self=nil then begin
    result := 0;
    exit; // avoid GPF in case of call from a static-only server
  end;
  {$ifdef WITHLOG}
  if SQLShouldBeLogged(aSQL) then begin
    log := fLog.Enter(self{$ifndef DELPHI5OROLDER},'Execute'{$endif});
    if log<>nil then
      log.Log(sllSQL,aSQL,self,2048);
  end;
  {$endif}
  Lock(aSQL);
  try
    result := R.Execute(DB,aSQL,ID);
  finally
    UnLock;
  end;
end;

procedure TSQLDataBase.Execute(const aSQL: RawUTF8; out ID: Int64; NoLog: boolean);
var R: TSQLRequest;
    Timer: TPrecisionTimer;
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  Timer.Start;
  Lock(aSQL);
  try
    R.Execute(DB,aSQL,ID);
  finally
    UnLock;
    {$ifdef WITHLOG}
    if not NoLog then
      fLog.Add.Log(sllSQL,'% % returned % for %',[Timer.Stop,FileNameWithoutPath,ID,aSQL],self);
    {$endif}
  end;
end;

procedure TSQLDataBase.Execute(const aSQL: RawUTF8; out ID: RawUTF8; NoLog: boolean);
var R: TSQLRequest;
    Timer: TPrecisionTimer;
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  Timer.Start;
  Lock(aSQL);
  try
    R.Execute(DB,aSQL,ID);
  finally
    UnLock;
    {$ifdef WITHLOG}
    if not NoLog then
      fLog.Add.Log(sllSQL,'% % returned [%] for %',
        [Timer.Stop,FileNameWithoutPath,ID,aSQL],self);
    {$endif}
  end;
end;

function TSQLDataBase.ExecuteNoException(const aSQL: RawUTF8): boolean;
var R: TSQLRequest;
    Timer: TPrecisionTimer;
begin
  result := false;
  if (self=nil) or (DB=0) then
    exit; // avoid GPF in case of call from a static-only server
  Timer.Start;
  Lock(aSQL); // run one statement -> we can trust IsCacheable()
  try
    result := R.ExecuteNoException(DB,aSQL);
  finally
    UnLock;
    {$ifdef WITHLOG}
    fLog.Add.Log(sllSQL,'% % % = %',[Timer.Stop,FileNameWithoutPath,aSQL,BOOL_STR[result]],self);
    {$endif}
  end;
end;

function TSQLDataBase.ExecuteNoExceptionInt64(const aSQL: RawUTF8): Int64;
begin
  if (self=nil) or (DB=0) then
    result := 0 else
    try
      Execute(aSQL,result,true);
    except
      result := 0;
    end;
end;

function TSQLDataBase.ExecuteNoExceptionUTF8(const aSQL: RawUTF8): RawUTF8;
begin
  if (self=nil) or (DB=0) then
    result := '' else
    try
      Execute(aSQL,result,true);
    except
      result := '';
    end;
end;

function TSQLDataBase.ExecuteJSON(const aSQL: RawUTF8; Expand: boolean=false;
  aResultCount: PPtrInt=nil): RawUTF8;
var R: TSQLRequest;
    Count: PtrInt;
    Timer: TPrecisionTimer;
begin
  if self=nil then begin
    result := '';
    exit; // avoid GPF in case of call from a static-only server
  end;
  Timer.Start;
  result := LockJSON(aSQL,aResultCount); // lock and try getting the request from the cache
  if result='' then // only Execute the DB request if not got from cache
  try
    result := R.ExecuteJSON(DB,aSQL,Expand,@Count);
    if aResultCount<>nil then
      aResultCount^ := Count;
  finally
    UnLockJSON(result,Count);
    {$ifdef WITHLOG}
    fLog.Add.Log(sllSQL,'% % returned % bytes %',
      [Timer.Stop,FileNameWithoutPath,length(result),aSQL],self);
    {$endif}
  end;
end;

function TSQLDataBase.Execute(const aSQL: RawUTF8; var Values: TRawUTF8DynArray): integer;
var R: TSQLRequest;
    Timer: TPrecisionTimer;
begin
  result := 0;
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  Timer.Start;
  Lock(aSQL);
  try
    result := R.Execute(DB,aSQL,Values);
  finally
    UnLock;
    {$ifdef WITHLOG}
    fLog.Add.Log(sllSQL,'% % returned % rows %',
      [Timer.Stop,FileNameWithoutPath,length(Values),aSQL],self);
    {$endif}
  end;
end;

function TSQLDataBase.LastInsertRowID: Int64;
begin
  if (self=nil) or (DB=0) then
    result := 0 else
    try
      Lock;
      result := sqlite3.last_insert_rowid(DB);
    finally
      UnLock;
    end;
end;

function TSQLDataBase.LastChangeCount: integer;
begin
  if (self=nil) or (DB=0) then
    result := 0 else
    try
      Lock;
      result := sqlite3.changes(DB);
    finally
      UnLock;
    end;
end;

function TSQLDataBase.TotalChangeCount: integer;
begin
  if (self=nil) or (DB=0) or not Assigned(sqlite3.total_changes) then
    result := 0 else
    try
      Lock;
      result := sqlite3.total_changes(DB);
    finally
      UnLock;
    end;
end;

procedure TSQLDataBase.GetTableNames(var Names: TRawUTF8DynArray);
begin // SQL statement taken from official SQLite3 FAQ
  SetLength(Names,Execute(SQL_GET_TABLE_NAMES,Names));
end;

function TSQLDataBase.HasTable(const Name: RawUTF8): boolean;
var names: TRawUTF8DynArray;
begin
  GetTableNames(names);
  result := FindPropName(names, Name)>=0;
end;

procedure TSQLDataBase.GetFieldNames(var Names: TRawUTF8DynArray; const TableName: RawUTF8);
var R: TSQLRequest;
    n: integer;
begin
  if (self=nil) or (fDB=0) then
    exit; // avoid GPF in case of call from a static-only server
  Lock;
  try
    try
      R.Prepare(fDB,'PRAGMA table_info('+TableName+');'); // ESQLite3Exception
      n := 0;
      while R.Step=SQLITE_ROW do // cid,name,type,notnull,dflt_value,pk
        AddRawUTF8(Names,n,sqlite3.column_text(R.Request,1));
      SetLength(Names,n);
    finally
      R.Close;
    end;
  finally
    UnLock;
  end;
end;

function TSQLDataBase.GetUseCache: boolean;
begin
  result := (self<>nil) and (fCache<>nil);
end;

procedure TSQLDataBase.SetUseCache(const Value: boolean);
begin
  if self<>nil then
    if Value<>UseCache then
      if Value then
        fCache := TSynCache.Create(fUseCacheSize,true) else
        FreeAndNil(fCache);
end;

function IsCacheable(const aSQL: RawUTF8): boolean;
begin
  result := isSelect(pointer(aSQL)) and (PosEx(SQLDATABASE_NOCACHE,aSQL)=0);
end;

procedure TSQLDataBase.Lock(const aSQL: RawUTF8);
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  if (aSQL='') or IsCacheable(aSQL) then
    fSafe.Lock  else // on non-concurent calls, is very fast
    LockAndFlushCache; // INSERT UPDATE DELETE statements need to flush cache
end;

procedure TSQLDataBase.LockAndFlushCache;
begin
  if self=nil then
    exit; // avoid GPF in case of call from a static-only server
  fSafe.Lock; // on non-concurent calls, this API is very fast
  try
    CacheFlush;
  except
    on Exception do begin // ensure critical section is left even on error
      fSafe.UnLock;
      raise;
    end;
  end;
end;

procedure TSQLDataBase.Lock;
begin
  if self<>nil then
    fSafe.Lock; // on non-concurent calls, this API is very fast
end;

procedure TSQLDataBase.UnLock;
begin
  if self<>nil then
    fSafe.UnLock; // on non-concurent calls, this API is very fast
end;

function TSQLDataBase.LockJSON(const aSQL: RawUTF8; aResultCount: PPtrInt): RawUTF8;
begin
  if self=nil then begin
    result := '';
    exit; // avoid GPF in case of call from a static-only server
  end;
  fSafe.Lock; // cache access is also protected by fSafe
  try
    if IsCacheable(aSQL) then begin
      result := fCache.Find(aSQL,aResultCount); // try to get JSON result from cache
      if result<>'' then begin
        {$ifdef WITHLOG}
        if fLog<>nil then begin
          fLog.Add.Log(sllSQL,'from cache % %',[FileNameWithoutPath,aSQL],self);
          fLog.Add.Log(sllResult,result,self,fLogResultMaximumSize);
        end;
        {$endif}
        fSafe.UnLock; // found in cache -> leave critical section
      end;
    end else begin
      // UPDATE, INSERT or any non SELECT statement
      CacheFlush;
      result := '';
    end;
  except
    on Exception do begin // ensure critical section is left even on error
      fSafe.UnLock;
      raise;
    end;
  end;
end;

procedure TSQLDataBase.UnLockJSON(const aJSONResult: RawUTF8; aResultCount: PtrInt);
begin
  if self<>nil then
  try
    {$ifdef WITHLOG}
    if fLog<>nil then
      fLog.Add.Log(sllResult,aJSONResult,self,fLogResultMaximumSize);
    {$endif}
    fCache.Add(aJSONResult,aResultCount); // no-op if Reset was made just before
  finally
    fSafe.UnLock; // on non-concurent calls, this API is very fast
  end;
end;

function TSQLDataBase.Backup(const BackupFileName: TFileName): boolean;
{$ifdef WITHLOG}
var Log: ISynLog;
begin
  Log := fLog.Enter('Backup % -> %',[fFileNameWithoutPath,BackupFileName],self);
{$else}
begin
{$endif}
  if self=nil then begin
    result := false;
    exit; // avoid GPF in case of call from a static-only server
  end;
  Rollback; // any unfinished transaction is rollbacked
  Execute('VACUUM;');
  LockAndFlushCache;
  try
    try
      {$ifdef WITHLOG}
      if Log<>nil then
        Log.Log(sllTrace,'close',self);
      {$endif}
      DBClose;
      {$ifdef WITHLOG}
      if Log<>nil then
        Log.Log(sllTrace,'copy file',self);
      {$endif}
      result := CopyFile(fFileName,BackupFileName,false);
    finally
      {$ifdef WITHLOG}
      if Log<>nil then
        Log.Log(sllTrace,'reopen',self);
      {$endif}
      DBOpen;
    end;
  finally
    UnLock;
  end;
end;

function TSQLDataBase.GetBackupBackgroundInProcess: boolean;
begin
  result := (self<>nil) and (fBackupBackgroundInProcess<>nil);
end;

function TSQLDataBase.GetSQLite3Library: TSQLite3Library;
begin // class function may be better, but fails on Delphi 2005
  result := sqlite3;
end;

function TSQLDataBase.BackupBackground(const BackupFileName: TFileName;
  StepPageNumber, StepSleepMS: Integer; OnProgress: TSQLDatabaseBackupEvent;
  SynLzCompress: boolean; const aPassword: RawUTF8): boolean;
var Dest: TSQLDatabase;
    Backup: TSQLite3Backup;
begin
  result := false;
  if (self=nil) or (BackupFileName='') or not Assigned(sqlite3.backup_init) or
     (fBackupBackgroundInProcess<>nil) then
    exit;
  {$ifdef WITHLOG}
  fLog.Add.Log(sllDB,'BackupBackground("%") started on %',
    [BackupFileName,FileNameWithoutPath],self);
  {$endif}
  if FileExists(BackupFileName) then
    if not DeleteFile(BackupFileName) then
      exit;
  // see https://bitbucket.org/egrange/sql3bak for proper parameters
  Dest := TSQLDatabase.Create(BackupFileName,aPassword,0,1);
  Dest.SetLockingMode(lmExclusive);
  Dest.SetSynchronous(smOff);
  Dest.ExecuteNoException('PRAGMA journal_mode=MEMORY');
  Dest.ExecuteNoException('PRAGMA temp_store=MEMORY');
  Backup := sqlite3.backup_init(Dest.DB,'main',DB,'main');
  if Backup=0 then begin
    Dest.Free;
    exit;
  end;
  fBackupBackgroundInProcess := TSQLDatabaseBackupThread.Create(
    Backup,self,Dest,StepPageNumber,StepSleepMS,SynLzCompress,OnProgress);
  result := true;
end;

function TSQLDataBase.BackupBackgroundToDB(BackupDB: TSQLDatabase;
  StepPageNumber, StepSleepMS: Integer; OnProgress: TSQLDatabaseBackupEvent): boolean;
var Backup: TSQLite3Backup;
begin
  result := false;
  if (self=nil) or (BackupDB=nil) or not Assigned(sqlite3.backup_init) or
     (fBackupBackgroundInProcess<>nil) then
    exit;
  {$ifdef WITHLOG}
  fLog.Add.Log(sllDB,'BackupBackgroundToDB("%") started on %',
    [BackupDB.FileName,FileNameWithoutPath],self);
  {$endif}
  Backup := sqlite3.backup_init(BackupDB.DB,'main',DB,'main');
  if Backup=0 then
    exit;
  fBackupBackgroundInProcess := TSQLDatabaseBackupThread.Create(
    Backup,self,BackupDB,StepPageNumber,StepSleepMS,false,OnProgress,false);
  result := true;
end;

procedure TSQLDataBase.BackupBackgroundWaitUntilFinished(TimeOutSeconds: Integer);
var i: integer;
begin
  if fBackupBackgroundInProcess<>nil then
    if TimeOutSeconds<0 then // TimeOutSeconds=-1 for infinite wait
      while fBackupBackgroundInProcess<>nil do
        SleepHiRes(10) else begin
      for i := 1 to TimeOutSeconds*100 do begin // wait for process end
        SleepHiRes(10);
        if fBackupBackgroundInProcess=nil then
          exit;
      end;
      Lock;
      if fBackupBackgroundInProcess<>nil then
        fBackupBackgroundInProcess.Terminate; // notify Execute loop abortion
      UnLock;
      for i := 1 to 500 do begin // wait 5 seconds for process to be aborted
        SleepHiRes(10);
        if fBackupBackgroundInProcess=nil then
          break;
      end;
    end;
end;

class function TSQLDataBase.BackupSynLZ(const SourceDB, DestSynLZ: TFileName;
  EraseSourceDB: boolean): boolean;
begin
  result := FileSynLZ(SourceDB, DestSynLZ, SQLITE3_MAGIC);
  if result and EraseSourceDB then
    result := DeleteFile(SourceDB);
end;

class function TSQLDataBase.BackupUnSynLZ(const SourceSynLZ, DestDB: TFileName): boolean;
begin
  result := FileUnSynLZ(SourceSynLZ, DestDB, SQLITE3_MAGIC);
end;

class function TSQLDataBase.IsBackupSynLZFile(const SynLZFile: TFileName): boolean;
begin
  result := FileIsSynLZ(SynLZFile, SQLITE3_MAGIC);
end;

function TSQLDataBase.DBClose: integer;
{$ifdef WITHLOG}
var log: ISynLog;
{$endif}
begin
  result := SQLITE_OK;
  if (self=nil) or (fDB=0) then
    exit;
  {$ifdef WITHLOG}
  log := fLog.Enter(self{$ifndef DELPHI5OROLDER},'DBClose'{$endif});
  if log<>nil then
    log.Log(sllDB,'closing [%] %',[FileName, KB(GetFileSize)],self);
  {$endif}
  if (sqlite3=nil) or not Assigned(sqlite3.close) then
    raise ESQLite3Exception.CreateUTF8('%.DBClose called with no sqlite3 global',[self]);
  if fBackupBackgroundInProcess<>nil then
    BackupBackgroundWaitUntilFinished;
  result := sqlite3.close(fDB);
  fDB := 0;
  fPageSize := 0;
end;

{$ifndef DELPHI5OROLDER}
function TSQLDataBase.EnableCustomTokenizer: integer;
{$ifdef WITHLOG}
var log: ISynLog;
{$endif}
begin
  result := SQLITE_OK;
  if (self=nil) or (fDB=0) then
    exit;
  {$ifdef WITHLOG}
  log := fLog.Enter;
  if log<>nil then
    log.Log(sllDB,'Enable custom tokenizer for [%]',[FileName],self);
  {$endif}
  if (sqlite3=nil) or not Assigned(sqlite3.db_config) then
    raise ESQLite3Exception.CreateUTF8('%.EnableCustomTokenizer called with no sqlite3 engine',[self]);
  result := sqlite3.db_config(fDB, SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER, 1);
end;
{$endif}

function TSQLDataBase.DBOpen: integer;
var utf8: RawUTF8;
    i: integer;
{$ifdef WITHLOG}
    log: ISynLog;
begin
  log := fLog.Enter('DBOpen %',[fFileNameWithoutPath],self);
{$else}
begin
{$endif WITHLOG}
  if fDB<>0 then
    raise ESQLite3Exception.Create('DBOpen called twice');
  // open the database with the proper API call
  if (sqlite3=nil) or not Assigned(sqlite3.open) then
    raise ESQLite3Exception.Create('DBOpen called with no sqlite3 global');
  utf8 := StringToUTF8(fFileName);
  {$ifdef LINUX}
  // for WAL to work under Linux - see http://www.sqlite.org/vfs.html
  if assigned(sqlite3.open_v2) and (fPassword='') then begin
    result := sqlite3.open_v2(pointer(utf8),fDB,fOpenV2Flags,'unix-excl');
    if result<>SQLITE_OK then // may be 'unix-excl' is not supported by the library
      result := sqlite3.open_v2(pointer(utf8),fDB,fOpenV2Flags,nil);
  end else
  {$else}
  if fOpenV2Flags<>(SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE) then
    result := sqlite3.open_v2(pointer(utf8),fDB,fOpenV2Flags,nil) else
  {$endif LINUX}
    result := sqlite3.open(pointer(utf8),fDB);
  if result<>SQLITE_OK then begin
    {$ifdef WITHLOG}
    if log<>nil then
      log.Log(sllError,'sqlite3_open ("%") failed with error % (%): %',
        [utf8,sqlite3_resultToErrorText(result),result,sqlite3.errmsg(fDB)]);
    {$endif WITHLOG}
    sqlite3.close(fDB); // should always be closed, even on failure
    fDB := 0;
    exit;
  end;
  // initialize optional encryption (if supported by the compiled engine)
  if Assigned(sqlite3.key) and (fPassword<>'') and (fFileName<>'') and
     (fFileName<>SQLITE_MEMORY_DATABASE_NAME) then
    sqlite3.key(fDB,pointer(fPassword),length(fPassword));
  // tune up execution context (before accessing the database)
  if not fIsMemory then begin
    if (fOpenV2Flags and SQLITE_OPEN_CREATE<>0) and (fFileDefaultPageSize<>0) then
      PageSize := fFileDefaultPageSize;
    if fFileDefaultCacheSize<>0 then
      CacheSize := fFileDefaultCacheSize; // 10000 by default (i.e. 40 MB)
  end;
  // always try to check for proper database content (and password)
  if not ExecuteNoException('select count(*) from sqlite_master') then begin
    result :=  SQLITE_NOTADB; // likely a password error
    sqlite3.close(fDB); // should always be closed, even on failure
    fDB := 0;
    exit;
  end;
  // our custom fast UTF-8 WinAnsi case insensitive compare, using NormToUpper[]
  sqlite3.create_collation(DB,'SYSTEMNOCASE',SQLITE_UTF8,nil,Utf8SQLCompNoCase);
  // our custom fast ISO-8601 date time encoded
  sqlite3.create_collation(DB,'ISO8601',SQLITE_UTF8,nil,Utf8SQLDateTime);
  // two slow but always accurate compare, using the Win32 Unicode API
  sqlite3.create_collation(DB,'WIN32CASE',SQLITE_UTF16,nil,Utf16SQLCompCase);
  sqlite3.create_collation(DB,'WIN32NOCASE',SQLITE_UTF16,nil,Utf16SQLCompNoCase);
  // note: standard SQLite3 NOCASE collation is used for AnsiString
  // register the MOD() user function, similar to the standard % operator
  sqlite3.create_function(DB,'MOD',2,SQLITE_ANY,nil,InternalMod,nil,nil);
  // register TIMELOG(), returning a ISO-8601 date/time from TTimeLog value
  sqlite3.create_function(DB,'TIMELOG',1,SQLITE_ANY,nil,InternalTimeLog,nil,nil);
  // register TIMELOGUNIX(), returning Unix Epoch seconds from TTimeLog value
  sqlite3.create_function(DB,'TIMELOGUNIX',1,SQLITE_ANY,nil,InternalTimeLogUnix,nil,nil);
  // some user functions
  sqlite3.create_function(DB,'SOUNDEX',1,SQLITE_UTF8,nil,InternalSoundex,nil,nil);
  sqlite3.create_function(DB,'SOUNDEXFR',1,SQLITE_UTF8,nil,InternalSoundexFr,nil,nil);
  sqlite3.create_function(DB,'SOUNDEXES',1,SQLITE_UTF8,nil,InternalSoundexEs,nil,nil);
  // rank() function as proposed in http://www.sqlite.org/fts3.html#appendix_a
  sqlite3.create_function(DB,'RANK',-1,SQLITE_ANY,nil,InternalRank,nil,nil);
  // CONCAT() function to process fast string concatenation
  sqlite3.create_function(DB,'CONCAT',2,SQLITE_UTF8,nil,nil,
    InternalConcatStep,InternalConcatFinal);
  // functions to handle some standard dynamic array BLOB content in SQL
  // IntegerDynArrayContains(BlobField,10) returning a boolean
  sqlite3.create_function(DB,'INTEGERDYNARRAYCONTAINS',2,SQLITE_ANY,
    nil,InternalIntegerDynArray,nil,nil);
  // Byte/Word/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
  sqlite3.create_function(DB,'BYTEDYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TByteDynArray),InternalSimpleInt64DynArray,nil,nil);
  sqlite3.create_function(DB,'WORDDYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TWordDynArray),InternalSimpleInt64DynArray,nil,nil);
  sqlite3.create_function(DB,'CARDINALDYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TCardinalDynArray),InternalSimpleInt64DynArray,nil,nil);
  sqlite3.create_function(DB,'INT64DYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TInt64DynArray),InternalSimpleInt64DynArray,nil,nil);
  sqlite3.create_function(DB,'CURRENCYDYNARRAYCONTAINS',2,SQLITE_ANY,
    TypeInfo(TInt64DynArray),InternalSimpleInt64DynArray,nil,nil);
  // RawUTF8DynArrayContainsCase/NoCase(BlobField,'Text') returning a boolean
  sqlite3.create_function(DB,'RAWUTF8DYNARRAYCONTAINSCASE',2,SQLITE_ANY,nil,
    InternalRawUTF8DynArray,nil,nil);
  sqlite3.create_function(DB,'RAWUTF8DYNARRAYCONTAINSNOCASE',2,SQLITE_ANY,
    @UTF8ILComp,InternalRawUTF8DynArray,nil,nil);
  // JSON related functions (e.g. for ORM storing variants as JSON UTF-8 text)
  sqlite3.create_function(DB,'JSONGET',2,SQLITE_ANY,nil,InternalJsonGet,nil,nil);
  sqlite3.create_function(DB,'JSONHAS',2,SQLITE_ANY,nil,InternalJsonHas,nil,nil);
  {$ifndef NOVARIANTS}
  sqlite3.create_function(DB,'JSONSET',3,SQLITE_ANY,nil,InternalJsonSet,nil,nil);
  {$endif}
  // reallocate all TSQLDataBaseSQLFunction for re-Open (TSQLRestServerDB.Backup)
  for i := 0 to fSQLFunctions.Count-1 do
    TSQLDataBaseSQLFunction(fSQLFunctions.List[i]).CreateFunction(DB);
  {$ifdef WITHLOG}
  i := CacheSize;
  if i<0 then
    i := (-i) shr 10 else
    i := PageSize*CacheSize;
  if log<>nil then
    log.Log(sllDB,'"%" database file (%) opened with PageSize=% CacheSize=% (%)',
      [FileName,KB(GetFileSize),PageSize,CacheSize,KB(i)],self);
  {$endif}
end;

function TSQLDataBase.GetUserVersion: cardinal;
begin
  result := ExecuteNoExceptionInt64('PRAGMA user_version');
end;

procedure TSQLDataBase.SetUserVersion(const Value: cardinal);
begin
  ExecuteNoException('PRAGMA user_version='+Int32ToUTF8(Value));
end;

function TSQLDataBase.GetCacheSize: cardinal;
begin
  result := ExecuteNoExceptionInt64('PRAGMA cache_size');
end;

procedure TSQLDataBase.SetCacheSize(const Value: cardinal);
begin
  ExecuteNoException('PRAGMA cache_size='+UInt32ToUTF8(Value));
end;

function TSQLDataBase.GetPageSize: cardinal;
begin
  if fPageSize=0 then // can be cached, since not change once opened
    fPageSize := ExecuteNoExceptionInt64('PRAGMA page_size');
  result := fPageSize;
end;

procedure TSQLDataBase.SetPageSize(const Value: cardinal);
begin
  if ExecuteNoException('PRAGMA page_size='+UInt32ToUTF8(Value)) then
    fPageSize := Value;
end;

function TSQLDataBase.GetPageCount: cardinal;
begin
  result := ExecuteNoExceptionInt64('PRAGMA page_count');
end;

function TSQLDataBase.GetFileSize: Int64;
begin
  result := GetPageCount;
  result := result*GetPageSize;
end;

procedure TSQLDataBase.SetSynchronous(const Value: TSQLSynchronousMode);
begin
  ExecuteNoException('PRAGMA synchronous='+UInt32ToUTF8(ord(Value)));
end;

procedure TSQLDataBase.SetMemoryMappedMB(const Value: cardinal);
begin
  ExecuteNoException('PRAGMA mmap_size='+Int64ToUTF8(Value shl 20));
end;

function TSQLDataBase.GetMemoryMappedMB: cardinal;
begin
  result := ExecuteNoExceptionInt64('PRAGMA mmap_size') shr 20;
end;

function TSQLDataBase.GetSynchronous: TSQLSynchronousMode;
begin
  result := TSQLSynchronousMode(ExecuteNoExceptionInt64('PRAGMA synchronous'));
end;

procedure TSQLDataBase.SetLockingMode(const Value: TSQLLockingMode);
const CMD: array[TSQLLockingMode] of RawUTF8 = ('NORMAL;','EXCLUSIVE;');
begin
  ExecuteNoException('PRAGMA locking_mode='+CMD[value]);
end;

function TSQLDataBase.GetLockingMode: TSQLLockingMode;
var tmp: RawUTF8;
begin
  tmp := ExecuteNoExceptionUTF8('PRAGMA locking_mode');
  if IdemPropNameU(tmp,'EXCLUSIVE') then
    result := lmExclusive else
    result := lmNormal;
end;

procedure TSQLDataBase.SetWALMode(Value: Boolean);
const CMD: array[boolean] of RawUTF8 = ('DELETE;','WAL;');
begin
  ExecuteNoException('PRAGMA journal_mode='+CMD[value]);
end;

function TSQLDataBase.GetWALMode: boolean;
begin
  result := IdemPropNameU(ExecuteNoExceptionUTF8('PRAGMA journal_mode'),'wal');
end;

procedure TSQLDataBase.SetBusyTimeout(const ms: Integer);
begin
  if (self=nil) or (fDB=0) then
    exit;
  sqlite3.busy_timeout(DB,ms);
  fBusyTimeout := ms;
end;

function TSQLDataBase.GetLimit(Category: TSQLLimitCategory): integer;
begin
  if (self=nil) or (fDB=0) or not Assigned(sqlite3.limit) then
    result := 0 else
    result := sqlite3.limit(fDB,ord(Category),-1);
end;

procedure TSQLDataBase.SetLimit(Category: TSQLLimitCategory; Value: integer);
begin
  if (self<>nil) and Assigned(sqlite3.limit) then
    sqlite3.limit(fDB,ord(Category),Value);
end;

procedure TSQLDataBase.CacheFlush;
begin
  if self=nil then
    exit;
  if InternalState<>nil then
    inc(InternalState^); 
  if fCache.Reset then
   {$ifdef WITHLOG}
    if fLog<>nil then
      fLog.Add.Log(sllCache,'% cache flushed',[FileNameWithoutPath],self);
    {$endif}
end;

procedure TSQLDataBase.RegisterSQLFunction(aFunction: TSQLDataBaseSQLFunction);
var i: integer;
begin
  if (self=nil) or (aFunction=nil) then
    exit;
  for i := 0 to fSQLFunctions.Count-1 do
    with TSQLDataBaseSQLFunction(fSQLFunctions.List[i]) do
    if (FunctionParametersCount=aFunction.FunctionParametersCount) and
       IdemPropNameU(FunctionName,aFunction.FunctionName) then begin
      aFunction.Free;
      exit; // already registered with the same name and parameters count
    end;
  {$ifdef WITHLOG}
  if fLog<>nil then
    fLog.Add.Log(sllDB,'% RegisterSQLFunction("%") %',
      [FileNameWithoutPath,aFunction.FunctionName],self);
  {$endif}
  fSQLFunctions.Add(aFunction);
  if DB<>0 then
    // DB already opened -> register this custom function
    aFunction.CreateFunction(DB);
end;

procedure TSQLDataBase.RegisterSQLFunction(aDynArrayTypeInfo: pointer;
  aCompare: TDynArraySortCompare; const aFunctionName: RawUTF8);
begin
  RegisterSQLFunction(
    TSQLDataBaseSQLFunctionDynArray.Create(aDynArrayTypeInfo,aCompare,aFunctionName));
end;

procedure TSQLDataBase.RegisterSQLFunction(aFunction: TSQLFunctionFunc;
  aFunctionParametersCount: Integer; const aFunctionName: RawUTF8);
begin
  RegisterSQLFunction(TSQLDataBaseSQLFunction.Create(aFunction,aFunctionParametersCount,aFunctionName));
end;


{ TSQLRequest }

procedure TSQLRequest.Bind(Param: Integer; Value: Int64);
begin
  sqlite3_check(RequestDB,sqlite3.bind_int64(Request,Param,Value),'bind_int64');
end;

procedure TSQLRequest.Bind(Param: Integer; Value: double);
begin
  sqlite3_check(RequestDB,sqlite3.bind_double(Request,Param,Value),'bind_double');
end;

procedure TSQLRequest.Bind(Param: Integer; const Value: RawUTF8);
var tmp: pointer;
begin
  // note that the official SQLite3 documentation is missleading:
  // sqlite3.bind_text(Text_bytes) must EXCLUDE the null terminator, otherwise a
  // #0 is appended to all column values -> so length(Value) is needed below
  if pointer(Value)=nil then
    // avoid to bind '' as null
    sqlite3_check(RequestDB,sqlite3.bind_text(Request,Param,@NULCHAR,
      0,SQLITE_STATIC)) else begin
    // assign RawUTF8 value by reference, to avoid memory allocation
    tmp := nil;
    RawByteString(tmp) := Value;
    // sqlite3InternalFreeRawByteString will decrease RefCount
    sqlite3_check(RequestDB,sqlite3.bind_text(Request,Param,tmp,length(Value),
      sqlite3InternalFreeRawByteString),'bind_text');
  end;
end;

procedure TSQLRequest.BindS(Param: Integer; const Value: string);
var P: PUTF8Char;
    len: integer;
begin
  if pointer(Value)=nil then begin // avoid to bind '' as null
    sqlite3_check(RequestDB,sqlite3.bind_text(Request,Param,@NULCHAR,0,SQLITE_STATIC));
    exit;
  end;
  len := length(Value);
  GetMem(P,len*3+1);
  {$ifdef UNICODE}
  len := RawUnicodeToUtf8(P,len*3,pointer(Value),len,[]);
  {$else}
  len := CurrentAnsiConvert.AnsiBufferToUTF8(P,pointer(Value),len)-P;
  {$endif}
  sqlite3_check(RequestDB,sqlite3.bind_text(Request,Param,P,len,@sqlite3InternalFree),'bind_text');
end;

procedure TSQLRequest.Bind(Param: Integer; Data: pointer; Size: integer);
begin
  sqlite3_check(RequestDB,sqlite3.bind_blob(Request,Param,Data,Size,
    SQLITE_TRANSIENT),'bind_blob'); // make private copy of the data
end;

procedure TSQLRequest.BindBlob(Param: Integer; const Data: RawByteString);
var tmp: pointer;
begin
  // assign RawByteString value by reference, to avoid memory allocation
  tmp := nil;
  RawByteString(tmp) := Data;
  // sqlite3InternalFreeRawByteString will decrease RefCount
  sqlite3_check(RequestDB,sqlite3.bind_blob(Request,Param,tmp,length(Data),
    sqlite3InternalFreeRawByteString),'bind_blob');
end;

procedure TSQLRequest.Bind(Param: Integer; Data: TCustomMemoryStream);
begin
  Bind(Param,Data.Memory,Data.Size);
end;

procedure TSQLRequest.BindNull(Param: Integer);
begin
  sqlite3_check(RequestDB,sqlite3.bind_null(Request,Param));
end;

procedure TSQLRequest.BindReset;
begin
  if Request<>0 then
    sqlite3.clear_bindings(Request);
end;

procedure TSQLRequest.BindZero(Param, Size: integer);
begin
  sqlite3_check(RequestDB,sqlite3.bind_zeroblob(Request,Param,Size));
end;

procedure TSQLRequest.Close;
begin
  if Request=0 then
    exit;
  {$ifdef RESETFPUEXCEPTION}
  with TSynFPUException.ForLibraryCode do
  {$endif}
    sqlite3.finalize(Request);
  fRequest := 0;
  fFieldCount := 0;
end;

procedure TSQLRequest.ExecuteAll;
begin
  if RequestDB=0 then
    raise ESQLite3Exception.Create(0,SQLITE_CANTOPEN,'ExecuteAll');
  try
    repeat
      repeat
      until Step<>SQLITE_ROW; // all steps of this statement
    until PrepareNext=SQLITE_DONE; // all statements
  finally
    Close; // always release statement
  end;
end;

procedure TSQLRequest.Execute;
begin
  if RequestDB=0 then
    raise ESQLite3Exception.Create(0,SQLITE_CANTOPEN,'Execute');
  try
    repeat
    until Step<>SQLITE_ROW; // Execute all steps of the first statement
  finally
    Close; // always release statement
  end;
end;

procedure TSQLRequest.ExecuteAll(aDB: TSQLite3DB; const aSQL: RawUTF8);
begin
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    ExecuteAll;
  finally
    Close; // always release statement, even if done normally in EngineExecuteAll
  end;
end;

procedure TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8);
begin
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    Execute;
  finally
    Close; // always release statement, even if done normally in Execute
  end;
end;

function TSQLRequest.ExecuteNoException(aDB: TSQLite3DB; const aSQL: RawUTF8): boolean;
begin // avoid sqlite3_check() calls for no ESQLite3Exception
  result := false;
  if (aDB<>0) and (aSQL<>'') then
    try
      if not(Prepare(aDB,aSQL,{noexcept=}true) in SQLITE_ERRORS) and (Request<>0) and
         not(sqlite3.step(Request) in SQLITE_ERRORS) then
        result := true;
    finally
      Close; // always release statement, even if done normally in Execute
    end;
end;

function TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; var ID: TInt64DynArray): integer;
var Res: integer;
begin
  result := 0;
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount>0 then
    repeat
      res := Step;
      if res=SQLITE_ROW then
        AddInt64(ID,result,sqlite3.column_int64(Request,0)); // first column value
    until res=SQLITE_DONE;
  finally
    Close; // always release statement
  end;
end;

procedure TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; out ID: Int64);
begin
  ID := 0;
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount>0 then
    if Step=SQLITE_ROW then
      ID := sqlite3.column_int64(Request,0); // get first column value
  finally
    Close; // always release statement
  end;
end;

procedure TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; out Value: RawUTF8);
begin
  Value := '';
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount>0 then
    if Step=SQLITE_ROW then
      Value := sqlite3.column_text(Request,0); // get first column value
  finally
    Close; // always release statement
  end;
end;

function TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; var Values: TRawUTF8DynArray): integer;
var Res: integer;
begin
  result := 0;
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount>0 then
    repeat
      res := Step;
      if res=SQLITE_ROW then
        AddRawUTF8(Values,result,sqlite3.column_text(Request,0)); // first column value
    until res=SQLITE_DONE;
  finally
    Close; // always release statement
  end;
end;

function TSQLRequest.Execute(aDB: TSQLite3DB; const aSQL: RawUTF8; JSON: TStream;
  Expand: boolean): PtrInt;
// expand=true: [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
// expand=false: { "FieldCount":2,"Values":["col1","col2",val11,"val12",val21,..] }
var i: integer;
    W: TJSONWriter;
    tmp: TTextWriterStackBuffer;
begin
  result := 0;
  W := TJSONWriter.Create(JSON,Expand,false,nil,0,@tmp);
  try
    // prepare the SQL request
    if aSQL<>'' then // if not already prepared, reset and bound by caller
      Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    if FieldCount<=0 then begin
      W.CancelAllVoid;
      exit;
    end;
    // get col names and types
    SetLength(W.ColNames,FieldCount);
    for i := 0 to FieldCount-1 do
      W.ColNames[i] := sqlite3.column_name(Request,i);
    W.AddColumns; // write or init field names for appropriate JSON Expand
    if Expand then
      W.Add('[');
    // write rows data
    repeat
      case Step of
      SQLITE_ROW: begin
        inc(result);
        FieldsToJSON(W);
        W.Add(',');
      end;
      SQLITE_DONE:
        break;
      end;
    until false;
    if (result=0) and W.Expand then begin
      // we want the field names at least, even with no data: we allow RowCount=0
      W.Expand := false; //  {"FieldCount":2,"Values":["col1","col2"]}
      W.CancelAll;
      for i := 0 to FieldCount-1 do
        W.ColNames[i] := sqlite3.column_name(Request,i);
      W.AddColumns;
    end;
    W.EndJSONObject(0,result);
  finally
    try
      if aSQL<>'' then
        Close; // always release statement (if not prepared and closed by caller)
    finally
      W.Free;
    end;
  end;
end;

procedure TSQLRequest.ExecuteDebug(aDB: TSQLite3DB; const aSQL: RawUTF8; var OutFile: Text);
var Res, i, n: integer;
begin
  {$I-}
  writeln;
  try
    Prepare(aDB,aSQL); // will raise an ESQLite3Exception on error
    repeat
      repeat
        Res := Step;
        if Res=SQLITE_ROW then begin
          n := FieldCount-1;
          for i := 0 to n do begin
            write(OutFile,FieldA(i));
            if i<n then
              write(OutFile,'|');
          end;
          writeln(OutFile);
        end;
      until Res=SQLITE_DONE;
    until PrepareNext=SQLITE_DONE;
  finally
    {$I+}ioresult;
    Close; // always release statement
  end;
end;

function TSQLRequest.ExecuteJSON(aDB: TSQLite3DB; const aSQL: RawUTF8;
  Expand: boolean=false; aResultCount: PPtrInt=nil): RawUTF8;
var Stream: TRawByteStringStream;
    RowCount: PtrInt;
begin
  Stream := TRawByteStringStream.Create;
  try
    try
      RowCount := Execute(aDB,aSQL,Stream,Expand); // create JSON data in Stream
      if aResultCount<>nil then
        aResultCount^ := RowCount;
      result := Stream.DataString;
    except
      on ESQLite3Exception do
        result := '';
    end;
    // Close has been called in Execute() above since aSQL<>''
  finally
    Stream.Free;
  end;
end;

function TSQLRequest.FieldA(Col: integer): WinAnsiString;
var P: PUTF8Char;
    L,L2: integer;
begin
  result := '';
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldA');
  P := sqlite3.column_text(Request,Col);
  L := SynCommons.StrLen(P); // faster than sqlite3.column_bytes(Request,Col)
  if L>0 then begin
    SetLength(result,L);
    L2 := UTF8ToWinPChar(pointer(result),P,L);
    if L2<>L then
      SetLength(result,L2);
  end;
end;

function TSQLRequest.FieldBlob(Col: integer): RawByteString;
var P: PAnsiChar;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldBlob');
  P := sqlite3.column_blob(Request,Col);
  SetString(result,P,sqlite3.column_bytes(Request,Col));
end;

function TSQLRequest.FieldBlobToStream(Col: integer): TStream;
begin
  result := TRawByteStringStream.Create(FieldBlob(Col));
end;

function TSQLRequest.FieldDouble(Col: integer): double;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldDouble');
  result := sqlite3.column_double(Request,Col);
end;

function TSQLRequest.FieldInt(Col: integer): Int64;
begin // internaly, SQLite always uses Int64 -> pure Integer function is useless
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldInt');
  result := sqlite3.column_int64(Request,Col);
end;

function TSQLRequest.FieldName(Col: integer): RawUTF8;
var P: PUTF8Char;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldName');
  P := sqlite3.column_name(Request,Col);
  FastSetString(result,P,SynCommons.StrLen(P));
end;

function TSQLRequest.FieldIndex(const aColumnName: RawUTF8): integer;
begin
  if Request=0 then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_MISUSE,'FieldIndex');
  for result := 0 to FieldCount-1 do
    if StrIComp(pointer(aColumnName),sqlite3.column_name(Request,result))=0 then
      exit;
  result := -1; // not found
end;

function TSQLRequest.FieldNull(Col: Integer): Boolean;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldNull');
  result := sqlite3.column_type(Request,Col)=SQLITE_NULL;
end;

function TSQLRequest.FieldType(Col: Integer): integer;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldType');
  result := sqlite3.column_type(Request,Col);
end;

function TSQLRequest.FieldDeclaredType(Col: Integer): RawUTF8;
var P: PUTF8Char;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_RANGE,'FieldDeclaredType');
  P := pointer(sqlite3.column_decltype(Request,Col));
  FastSetString(result,P,SynCommons.StrLen(P));
end;

function TSQLRequest.FieldDeclaredTypeS(Col: Integer): String;
var P: PUTF8Char;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_RANGE,'FieldDeclaredTypeS');
  P := pointer(sqlite3.column_decltype(Request,Col));
  result := UTF8DecodeToString(P,SynCommons.StrLen(P));
end;

function TSQLRequest.FieldUTF8(Col: integer): RawUTF8;
var P: PUTF8Char;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldUTF8');
  P := pointer(sqlite3.column_text(Request,Col));
  FastSetString(result,P,SynCommons.StrLen(P));
end;

function TSQLRequest.FieldS(Col: integer): string;
{$ifdef UNICODE}
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldS');
  result := sqlite3.column_text16(Request,Col);
end;
{$else}
var P: PUTF8Char;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldS');
  P := pointer(sqlite3.column_text(Request,Col));
  CurrentAnsiConvert.UTF8BufferToAnsi(P,SynCommons.StrLen(P),RawByteString(result));
end;
{$endif}

function TSQLRequest.FieldValue(Col: integer): TSQLite3Value;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldValue');
  result := sqlite3.column_value(Request,Col);
end;

function TSQLRequest.FieldW(Col: integer): RawUnicode;
var P: PWideChar;
begin
  if cardinal(Col)>=cardinal(FieldCount) then
    raise ESQLite3Exception.Create(RequestDB, SQLITE_RANGE,'FieldW');
  P := sqlite3.column_text16(Request,Col);
  SetString(result,PAnsiChar(pointer(P)),StrLenW(P)*2+1);
end;

function TSQLRequest.Prepare(DB: TSQLite3DB; const SQL: RawUTF8; NoExcept: boolean): integer;
begin
  fDB := DB;
  fRequest := 0;
  if DB=0 then
    raise ESQLite3Exception.Create(DB,SQLITE_CANTOPEN,SQL);
  {$ifdef RESETFPUEXCEPTION} // safest to reset x87 exceptions
  with TSynFPUException.ForLibraryCode do
  {$endif}
  begin
    result := sqlite3.prepare_v2(RequestDB, pointer(SQL), length(SQL)+1,
      fRequest, fNextSQL);
    while (result=SQLITE_OK) and (Request=0) do begin // comment or white-space
      if fNextSQL^ = #0 then // statement contains only comment
        raise ESQLite3Exception.Create(DB,SQLITE_EMPTY,SQL);
      result := sqlite3.prepare_v2(RequestDB, fNextSQL, -1, fRequest, fNextSQL);
    end;
    fFieldCount := sqlite3.column_count(fRequest);
    if not NoExcept then
      sqlite3_check(RequestDB,result,SQL);
  end;
end;

function TSQLRequest.PrepareAnsi(DB: TSQLite3DB; const SQL: WinAnsiString): integer;
begin
  result := Prepare(DB,WinAnsiToUtf8(SQL));
end;

function TSQLRequest.PrepareNext: integer;
begin
  if (Request=0) or (fNextSQL^=#0) then
    result := SQLITE_DONE else begin
    Close; // free previous statement
    result := sqlite3.prepare_v2(RequestDB, fNextSQL, -1, fRequest, fNextSQL);
    while (result=SQLITE_OK) and (Request=0) and (fNextSQL^<>#0) do
      // comment or white-space -> ignore
      result := sqlite3.prepare_v2(RequestDB, fNextSQL, -1, fRequest, fNextSQL);
    fFieldCount := sqlite3.column_count(fRequest);
    sqlite3_check(RequestDB,result,'PrepareNext');
    if Request=0 then
      result := SQLITE_DONE; // nothing more to add
  end;
end;

function TSQLRequest.Reset: integer;
begin
  if Request=0 then
    raise ESQLite3Exception.Create('TSQLRequest.Reset called with no previous Request');
  {$ifdef RESETFPUEXCEPTION} // safest to reset x87 exceptions
  with TSynFPUException.ForLibraryCode do
  {$endif}
    result := sqlite3.reset(Request); // no check here since it was PREVIOUS state
end;

function TSQLRequest.Step: integer;
{$ifdef RESETFPUEXCEPTION} // safest to reset x87 exceptions - inlined TSynFPUException
var cw87: word;
{$endif}
begin
  if Request=0 then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_MISUSE,'Step');
  {$ifdef RESETFPUEXCEPTION}
  cw87 := Get8087CW;
  try
  {$endif}
    result := sqlite3_check(RequestDB,sqlite3.step(Request),'Step');
  {$ifdef RESETFPUEXCEPTION}
  finally
    Set8087CW(cw87);
  end;
  {$endif}
end;

function TSQLRequest.GetReadOnly: Boolean;
begin
  if Request=0 then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_MISUSE,'IsReadOnly');
  result := sqlite3.stmt_readonly(Request)<>0;
end;

procedure TSQLRequest.FieldsToJSON(WR: TJSONWriter; DoNotFetchBlobs: boolean);
var i: integer;
begin
  if Request=0 then
    raise ESQLite3Exception.Create(RequestDB,SQLITE_MISUSE,'FieldsToJSON');
  if WR.Expand then
    WR.Add('{');
  for i := 0 to FieldCount-1 do begin
    if WR.Expand then
      WR.AddString(WR.ColNames[i]); // '"'+ColNames[]+'":'
    case sqlite3.column_type(Request,i) of // fast evaluation: type may vary
      SQLITE_BLOB:
        if DoNotFetchBlobs then
          WR.AddShort('null') else
          WR.WrBase64(sqlite3.column_blob(Request,i),
            sqlite3.column_bytes(Request,i),true); // withMagic=true
      SQLITE_NULL:
        WR.AddShort('null'); // returned also for ""
      SQLITE_INTEGER:
        WR.Add(sqlite3.column_int64(Request,i));
      SQLITE_FLOAT:
        WR.AddDouble(sqlite3.column_double(Request,i));
      SQLITE_TEXT: begin
        WR.Add('"');
        WR.AddJSONEscape(sqlite3.column_text(Request,i),0);
        WR.Add('"');
       end;
    end; // case ColTypes[]
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

function TSQLRequest.GetParamCount: integer;
begin
  if Request=0 then
    result := 0 else
    result := sqlite3.bind_parameter_count(Request);
end;


{ ESQLite3Exception }

constructor ESQLite3Exception.Create(aDB: TSQLite3DB; aErrorCode: integer;
  const aSQL: RawUTF8);
var msg: RawUTF8;
begin
  fErrorCode := aErrorCode;
  fSQLite3ErrorCode := sqlite3_resultToErrorCode(aErrorCode);
  FormatUTF8('Error % (%) [%] using %',
    [ErrorCodeToText(SQLite3ErrorCode),aErrorCode,aSQL,sqlite3.VersionText],msg);
  if aDB=0 then
    msg := msg+' with aDB=nil' else begin
    msg := FormatUTF8('% - %',[msg,sqlite3.errmsg(aDB)]);
    if Assigned(sqlite3.extended_errcode) then
      msg := FormatUTF8('%, extended_errcode=%',[msg,sqlite3.extended_errcode(aDB)]);
  end;
  DB := aDB;
  inherited Create(UTF8ToString(msg));
end;

function sqlite3_check(DB: TSQLite3DB; aResult: integer; const SQL: RawUTF8): integer;
begin
  if (DB=0) or (aResult in SQLITE_ERRORS) then // possible error codes
    raise ESQLite3Exception.Create(DB,aResult,SQL);
  result := aResult;
end;

function sqlite3_resultToErrorCode(aResult: integer): TSQLite3ErrorCode;
begin
  case aResult of
  SQLITE_OK..SQLITE_NOTADB:
    result := TSQLite3ErrorCode(aResult+ord(secOK));
  SQLITE_ROW..SQLITE_DONE:
    result := TSQLite3ErrorCode(aResult+ord(secROW));
  else
    result := secUnknown;
  end;
end;

function ErrorCodeToText(err: TSQLite3ErrorCode): RawUTF8;
begin
  result := 'SQLITE_'+TrimLeftLowerCaseShort(GetEnumName(
    TypeInfo(TSQLite3ErrorCode),ord(err)));
end;

function sqlite3_resultToErrorText(aResult: integer): RawUTF8;
begin
  result := ErrorCodeToText(sqlite3_resultToErrorCode(aResult));
end;


{ TSQLBlobStream }

constructor TSQLBlobStream.Create(aDB: TSQLite3DB; const DBName, TableName,
  ColumnName: RawUTF8; RowID: Int64; ReadWrite: boolean);
begin
  fDB := aDB;
  fWritable := ReadWrite;
  sqlite3_check(aDB,sqlite3.blob_open(aDB,pointer(DBName),pointer(TableName),
    pointer(ColumnName),RowID,integer(ReadWrite),fBlob),'blob_open');
  fSize := sqlite3.blob_bytes(fBlob);
end;

destructor TSQLBlobStream.Destroy;
begin
  sqlite3.blob_close(fBlob);
  inherited;
end;

function TSQLBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  result := fSize-fPosition; // bytes available left
  if Count<result then // read only inside the Blob size
    result := Count;
  if result<>0 then begin
    sqlite3_check(fDB,sqlite3.blob_read(fBlob,Buffer,result,fPosition));
    inc(fPosition,result);
  end;
end;

function TSQLBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: fPosition := Offset;
    soFromCurrent:   Inc(fPosition,Offset);
    soFromEnd:       fPosition := fSize+Offset;
  end;
  if fPosition>fSize then
    fPosition := fSize;
  Result := fPosition;
end;

procedure TSQLBlobStream.ChangeRow(RowID: Int64);
begin
  if not Assigned(sqlite3.blob_reopen) then
    raise ESQLite3Exception.Create('blob_reopen API not available');
  sqlite3_check(fDB,sqlite3.blob_reopen(fBlob,RowID),'blob_reopen');
  fPosition := 0;
  fSize := sqlite3.blob_bytes(fBlob);
end;

function TSQLBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  result := fSize-fPosition; // bytes available left
  if Count<result then
    result := Count; // write only inside the Blob size
  if result<>0 then begin
    sqlite3_check(fDB,sqlite3.blob_write(fBlob,Buffer,result,fPosition));
    inc(fPosition,result);
  end;
end;


{ TSQLDataBaseSQLFunction }

constructor TSQLDataBaseSQLFunction.Create(aFunction: TSQLFunctionFunc;
  aFunctionParametersCount: Integer; const aFunctionName: RawUTF8);
begin
  fInternalFunction := aFunction;
  fFunctionParametersCount := aFunctionParametersCount;
  if aFunctionName='' then
    fSQLName := RawUTF8(copy(ClassName,2,maxInt)) else
    fSQLName := aFunctionName;
end;

function TSQLDataBaseSQLFunction.CreateFunction(DB: TSQLite3DB): Integer;
begin
  if self<>nil then
    result := sqlite3.create_function(DB,pointer(fSQLName),
      FunctionParametersCount,SQLITE_ANY,self,fInternalFunction,nil,nil) else
    result := SQLITE_ERROR;
end;


{ TSQLDataBaseSQLFunctionDynArray }

procedure InternalSQLFunctionDynArrayBlob(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var P, item: PAnsiChar;
    PLen, itemLen: PtrInt;
    caller: TSQLDataBaseSQLFunctionDynArray;
begin
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit; // two parameters expected
  end;
  P := sqlite3.value_blob(argv[0]);
  PLen := sqlite3.value_bytes(argv[0]);
  item := sqlite3.value_blob(argv[1]);
  itemLen := sqlite3.value_bytes(argv[1]);
  caller := sqlite3.user_data(Context);
  if (P<>nil) and (PLen>0) and (item<>nil) and (itemLen>0) and (caller<>nil) then
  with caller.fDummyDynArray do
  try // temporary allocate all dynamic array content
    try
      if (LoadFrom(P,nil,{nohash=}true,P+PLen)=nil) or
         (ElemLoadFind(item,item+itemLen)<0) then
        P := nil; // not found
    finally
      Clear; // always release temporary array content
    end;
  except
    on Exception do begin
      sqlite3.result_error(Context,'Invalid BLOB content');
      exit;
    end;
  end else
    P := nil;
  sqlite3.result_int64(Context,Int64(P<>nil));
end;

constructor TSQLDataBaseSQLFunctionDynArray.Create(aTypeInfo: pointer;
  aCompare: TDynArraySortCompare; const aFunctionName: RawUTF8);
begin
  fDummyDynArray.Init(aTypeInfo,fDummyDynArrayValue);
  fDummyDynArray.Compare := aCompare;
  inherited Create(InternalSQLFunctionDynArrayBlob,2,aFunctionName);
end;


{ TSQLStatementCached }

procedure TSQLStatementCached.Init(aDB: TSQLite3DB);
begin
  Caches.InitSpecific(TypeInfo(TSQLStatementCacheDynArray),Cache,djRawUTF8,@Count);
  DB := aDB;
end;

function TSQLStatementCached.Prepare(const GenericSQL: RawUTF8;
  WasPrepared: PBoolean; ExecutionTimer: PPPrecisionTimer; ExecutionMonitor: PSynMonitor): PSQLRequest;
var added: boolean;
    ndx: integer;
begin
  ndx := Caches.FindHashedForAdding(GenericSQL,added);
  with Cache[ndx] do begin
    if added then begin
      StatementSQL := GenericSQL;
      Statement.Prepare(DB,GenericSQL);
      Timer := TSynMonitor.Create;
      if WasPrepared<>nil then
        WasPrepared^ := true;
    end else begin
      if Timer=nil then // there was a Statement.Prepare exception on previous call
        raise ESQLite3Exception.CreateUTF8('TSQLStatementCached.Prepare failed [%]', [GenericSQL]);
      if Statement.Request<>0 then
        Statement.Reset;
      if WasPrepared<>nil then
        WasPrepared^ := false;
    end;
    if ExecutionTimer<>nil then begin
      Timer.ProcessStartTask;
      ExecutionTimer^ := @Timer.InternalTimer;
      if ExecutionMonitor<>nil then
        ExecutionMonitor^ := Timer;
    end;
    result := @Statement;
  end;
end;

procedure TSQLStatementCached.ReleaseAllDBStatements;
var i: integer;
begin
  for i := 0 to Count-1 do begin
    Cache[i].Statement.Close; // close prepared statement
    Cache[i].Timer.Free;
  end;
  Caches.Clear;
  Caches.ReHash; // need to refresh all hashs
end;

function StatementCacheTotalTimeCompare(const A,B): integer;
var i64: Int64;
begin
  i64 := TSQLStatementCache(A).Timer.InternalTimer.TimeInMicroSec-
         TSQLStatementCache(B).Timer.InternalTimer.TimeInMicroSec;
  if i64<0 then
    result := -1 else
  if i64>0 then
    result := 1 else
    result := 0;
end;

procedure TSQLStatementCached.SortCacheByTotalTime(var aIndex: TIntegerDynArray);
begin
  Caches.{$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$endif}CreateOrderedIndex(
    aIndex,StatementCacheTotalTimeCompare);
end;


{ TSQLDatabaseBackupThread }

constructor TSQLDatabaseBackupThread.Create(Backup: TSQLite3Backup;
  Source, Dest: TSQLDatabase; StepPageNumber, StepSleepMS: Integer;
  SynLzCompress: boolean; OnProgress: TSQLDatabaseBackupEvent; OwnerDest: boolean);
begin
  fTimer.Start;
  fBackup := Backup;
  fSourceDB := Source;
  fDestDB := Dest;
  fBackupDestFile := Dest.fFileName;
  if StepPageNumber=0 then
    fStepPageNumber := 1 else
    fStepPageNumber := StepPageNumber;
  if (cardinal(StepSleepMS)<=1000) and (StepPageNumber>0) then
    fStepSleepMS := StepSleepMS;
  fOnProgress := OnProgress;
  fStepSynLzCompress := SynLzCompress;
  fOwnerDest := OwnerDest;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TSQLDatabaseBackupThread.Execute;
{$ifdef WITHLOG}
var log: ISynLog;
{$endif}
  procedure NotifyProgressAndContinue(aStep: TSQLDatabaseBackupEventStep);
  begin
    fStep := aStep;
    {$ifdef WITHLOG}
    if Assigned(log) then
      log.Log(sllTrace,'%', [self]);
    {$endif}
    if Assigned(fOnProgress) then
      if not fOnProgress(self) then
        raise ESQLite3Exception.CreateUtf8('%.Execute aborted by OnProgress=false',[self]);
  end;
var res: integer;
    fn, fn2: TFileName;
begin
  fn := fDestDB.FileName;
  {$ifdef WITHLOG}
  SetCurrentThreadName('% [%] [%]',[self,fSourceDB.FileName,fn]);
  log := SynSQLite3Log.Enter(self{$ifndef DELPHI5OROLDER},'Execute'{$endif});
  {$endif}
  try
    try
      NotifyProgressAndContinue(backupStart);
      repeat
        fSourceDB.Lock; // naive multi-thread protection of main process
        res := sqlite3.backup_step(fBackup,fStepPageNumber);
        fSourceDB.UnLock;
        fStepNumberToFinish := sqlite3.backup_remaining(fBackup);
        fStepNumberTotal := sqlite3.backup_pagecount(fBackup);
        case res of
        SQLITE_OK:
          NotifyProgressAndContinue(backupStepOk);
        SQLITE_BUSY: begin
          NotifyProgressAndContinue(backupStepBusy);
          if fStepSleepMS=0 then
            SleepHiRes(1);
        end;
        SQLITE_LOCKED:
          NotifyProgressAndContinue(backupStepLocked);
        SQLITE_DONE:
          break;
        else raise ESQLite3Exception.Create(fDestDB.DB,res,'Backup');
        end;
        if Terminated then
          raise ESQLite3Exception.Create('Backup process forced to terminate');
        SleepHiRes(fStepSleepMS);
      until false;
      if fDestDB<>nil then begin
        sqlite3.backup_finish(fBackup);
        // close destination backup database
        if fOwnerDest then
          FreeAndNil(fDestDB);
        fDestDB :=  nil;
      end;
      if not IdemPChar(pointer(fn),SQLITE_MEMORY_DATABASE_NAME) then begin
        if fStepSynLzCompress then begin
          NotifyProgressAndContinue(backupStepSynLz);
          fn2 := ChangeFileExt(fn, '.db.tmp');
          DeleteFile(fn2);
          if not RenameFile(fn,fn2)  then
            raise ESQLite3Exception.CreateUTF8('%.Execute: RenameFile(%,%) failed',
              [self,fn,fn2]);
          if not TSQLDatabase.BackupSynLZ(fn2,fn,true) then
            raise ESQLite3Exception.CreateUTF8('%.Execute: BackupSynLZ(%,%) failed',
              [self,fn,fn2]);
          {$ifdef WITHLOG}
          if Assigned(log) then
            log.Log(sllTrace,'TSQLDatabase.BackupSynLZ into % %',
              [KB(FileSize(fn)),fn],self);
          {$endif}
        end;
        fSourceDB.fBackupBackgroundLastFileName := ExtractFileName(fn);
      end;
      NotifyProgressAndContinue(backupSuccess);
    finally
      if fDestDB<>nil then begin
        sqlite3.backup_finish(fBackup);
        if fOwnerDest then
          fDestDB.Free; // close destination backup database if not already
      end;
      fSourceDB.fBackupBackgroundLastTime := fTimer.Stop;
      fSourceDB.Lock;
      fSourceDB.fBackupBackgroundInProcess := nil;
      fSourceDB.Unlock;
    end;
  except
    on E: Exception do begin
      fError := E;
      fStep := backupFailure;
      if Assigned(fOnProgress) then
        fOnProgress(self);
    end;
  end;
  {$ifdef WITHLOG}
  log := nil;
  SynSQLite3Log.Add.NotifyThreadEnded;
  {$endif}
end;


{ TSQLite3Library }

constructor TSQLite3Library.Create;
var V: PUTF8Char;
begin
  if Assigned(libversion) then begin
    V := libversion;
    fVersionText := RawUTF8(V);
    fVersionNumber := GetNextItemCardinal(V,'.')*1000000000+
      GetNextItemCardinal(V,'.')*1000000+GetNextItemCardinal(V,'.')*1000+
      GetNextItemCardinal(V,'.'); // convert into e.g. 3008003001
  end;
end;

{$ifndef DELPHI5OROLDER}

{$ifdef FPC} // under FPC, MemSize() returns the value expected by xSize()

function xMalloc(size: integer): pointer; cdecl;
begin
  result := GetMem(size);
end;

procedure xFree(ptr: pointer); cdecl;
begin
  FreeMem(ptr);
end;

function xRealloc(ptr: pointer; size: integer): pointer; cdecl;
begin
  result := ReAllocMem(ptr,size);
end;

function xSize(ptr: pointer): integer; cdecl;
begin
  result := MemSize(ptr);
end;

{$else} // under Delphi, we need to store the size as 4 bytes header for xSize()

function xMalloc(size: integer): pointer; cdecl;
begin
  GetMem(result,size+4);
  PInteger(result)^ := size;
  inc(PInteger(result));
end;

procedure xFree(ptr: pointer); cdecl;
begin
  dec(PInteger(ptr));
  FreeMem(ptr);
end;

function xRealloc(ptr: pointer; size: integer): pointer; cdecl;
begin
  dec(PInteger(ptr));
  ReallocMem(ptr,size+4);
  PInteger(ptr)^ := size;
  inc(PInteger(ptr));
  result := ptr;
end;

function xSize(ptr: pointer): integer; cdecl;
begin
  if ptr=nil then
    result := 0 else begin
    dec(PInteger(ptr));
    result := PInteger(ptr)^;
  end;
end;

{$endif FPC}

{$endif DELPHI5OROLDER}

function xRoundup(size: integer): integer; cdecl;
begin
  result := size;
end;
function xInit(appData: pointer): integer; cdecl;
begin
  result := SQLITE_OK;
end;
procedure xShutdown(appData: pointer); cdecl;
begin
end;

procedure TSQLite3Library.ForceToUseSharedMemoryManager;
{$ifdef DELPHI5OROLDER}
begin // varargs attribute was unsupported on Delphi 5
{$else}
// due to FPC's linker limitation, all wrapper functions should be defined outside
var mem: TSQLite3MemMethods;
    res: integer;
    {$ifdef FPC_X64}
    mm: TMemoryManager;
    {$endif FPC_X64}
begin
  if not Assigned(config) then
    exit;
  {$ifdef FPC_X64} // SQLite3 prototypes match FPC RTL functions on x86_64 ABI
  GetMemoryManager(mm);
  mem.xMalloc := @mm.Getmem;
  mem.xFree := @mm.Freemem;
  mem.xSize := @mm.MemSize;
  {$else}
  mem.xMalloc := @xMalloc;
  mem.xFree := @xFree;
  mem.xSize := @xSize;
  {$endif FPC_X64}
  mem.xRealloc := @xRealloc;
  mem.xRoundup := @xRoundup;
  mem.xInit := @xInit;
  mem.xShutdown := @xShutdown;
  mem.pAppData := nil;
  try
    res := config(SQLITE_CONFIG_MALLOC,@mem);
  except
    res := SQLITE_INTERNAL;
  end;
  if res<>SQLITE_OK then begin
    {$ifdef WITHLOG}
    SynSQLite3Log.Add.Log(sllError,'SQLITE_CONFIG_MALLOC failed as %',[res]);
    {$endif}
  end else
    fUseInternalMM := true;
{$endif DELPHI5OROLDER}
end;

function TSQLite3Library.GetVersion: RawUTF8;
const MM: array[boolean] of string[2] = ('ex','in');
begin
  if self=nil then
    result := 'No TSQLite3Library available' else
    FormatUTF8('% % with %ternal MM',[self,fVersionText,MM[fUseInternalMM]],result);
end;


{ TSQLite3LibraryDynamic }

const
  SQLITE3_ENTRIES: array[0..91] of TFileName =
  ('initialize','shutdown','open','open_v2','key','rekey','close',
   'libversion','errmsg','extended_errcode',
   'create_function','create_function_v2', 'create_window_function',
   'create_collation','last_insert_rowid','busy_timeout','busy_handler',
   'prepare_v2','finalize','next_stmt','reset','stmt_readonly','step',
   'column_count','column_type','column_decltype','column_name','column_bytes',
   'column_value','column_double','column_int','column_int64','column_text',
   'column_text16','column_blob','value_type','value_numeric_type',
   'value_bytes','value_double','value_int64','value_text','value_blob',
   'result_null','result_int64','result_double','result_blob','result_text',
   'result_value','result_error','user_data','context_db_handle',
   'aggregate_context','bind_text','bind_blob','bind_zeroblob','bind_double',
   'bind_int','bind_int64','bind_null','clear_bindings','bind_parameter_count',
   'blob_open','blob_reopen','blob_close','blob_read','blob_write','blob_bytes',
   'create_module_v2','declare_vtab','set_authorizer','update_hook',
   'commit_hook','rollback_hook','changes','total_changes','malloc', 'realloc',
   'free','memory_used','memory_highwater','trace_v2','limit',
   'backup_init','backup_step','backup_finish','backup_remaining',
   'backup_pagecount','serialize','deserialize','soft_heap_limit64',
   'config','db_config');

constructor TSQLite3LibraryDynamic.Create(const LibraryName: TFileName);
var P: PPointerArray;
    i: integer;
    vers: PUTF8Char;
begin
  fLibraryName := LibraryName;
  {$ifdef MSWINDOWS}
  fHandle := SafeLoadLibrary(LibraryName);
  if fHandle=0 then
  {$else}
    {$ifdef BSDNOTDARWIN}
    fHandle := TLibHandle(dlopen(PChar(LibraryName),0));
    if fHandle=TLibHandle(nil) then
    {$else}
    fHandle := LoadLibrary({$ifndef FPC}pointer{$endif}(LibraryName));
    if fHandle=0 then
    {$endif}
  {$endif MSWINDOWS}
    raise ESQLite3Exception.CreateUTF8('%.Create: Unable to load % - %',
      [self,LibraryName,SysErrorMessage(GetLastError)]);
  P := @@initialize;
  for i := 0 to High(SQLITE3_ENTRIES) do
    P^[i] := {$ifdef BSDNOTDARWIN}dlsym{$else}GetProcAddress{$endif}(
      fHandle,PChar('sqlite3_'+SQLITE3_ENTRIES[i]));
  if not Assigned(initialize) or not Assigned(libversion) or
     not Assigned(open) or not Assigned(close) or not Assigned(create_function) or
     not Assigned(prepare_v2) or not Assigned(create_module_v2) then begin
    if Assigned(libversion) then
      vers := libversion else
      vers := nil;
    {$ifdef BSDNOTDARWIN}
    dlclose(fHandle);
    fHandle := TLibHandle(nil);
    {$else}
    FreeLibrary(fHandle);
    fHandle := 0;
    {$endif}
    raise ESQLite3Exception.CreateUTF8('%.Create: TOO OLD % % - need 3.7 at least!',
      [self,LibraryName,vers]);
  end; // some APIs like config() key() or trace() may not be available
  inherited Create; // set fVersionNumber/fVersionText
  {$ifdef WITHLOG}
  SynSQLite3Log.Add.Log(sllInfo,'Loaded external % version %',[LibraryName,Version]);
  {$endif}
end;

destructor TSQLite3LibraryDynamic.Destroy;
begin
  {$ifdef BSDNOTDARWIN}
  if fHandle<>TLibHandle(nil) then
    dlclose(fHandle);
  {$else}
  if fHandle<>0 then
    FreeLibrary(fHandle);
  {$endif}
  inherited;
end;


initialization

finalization
  FreeAndNil(sqlite3); // sqlite3.Free is not reintrant e.g. as .bpl in IDE
end.
