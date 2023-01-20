unit ALSqlite3Wrapper;

//
// to make this unit easy to maintain I over simplify it
// instead of converting all the sqlite3.h to pascal
// I convert only the functions I need, and in this way i know it's
// work because I test it. If i will need more functions, then
// I can look in sqlite3.h and compare with the convertion
// in https://github.com/plashenkov/SQLite3-Delphi-FPC and
// in system.Sqlite.pas. I think the translation made in
// https://github.com/plashenkov/SQLite3-Delphi-FPC are better
// than the one made in system.Sqlite.pas like for exemple:
// https://stackoverflow.com/questions/48726053/what-does-this-mean-in-pascal-const-char-pztail/48727639#48727639
// but the naming made in system.Sqlite.pas is more close to
// the naming made in sqlite3.h
//

interface

Uses
  Winapi.Windows;

type
  SQLite3 = Pointer;
  SQLite3_Stmt = Pointer;
  sqlite_int64 = Int64;
  sqlite3_int64 = sqlite_int64;

const
  SQLITE_OK          = 0;   // Successful result
  SQLITE_ERROR       = 1;   // SQL error or missing database
  SQLITE_INTERNAL    = 2;   // Internal logic error in SQLite
  SQLITE_PERM        = 3;   // Access permission denied
  SQLITE_ABORT       = 4;   // Callback routine requested an abort
  SQLITE_BUSY        = 5;   // The database file is locked
  SQLITE_LOCKED      = 6;   // A table in the database is locked
  SQLITE_NOMEM       = 7;   // A malloc() failed
  SQLITE_READONLY    = 8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT   = 9;   // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR      = 10;   // Some kind of disk I/O error occurred
  SQLITE_CORRUPT    = 11;   // The database disk image is malformed
  SQLITE_NOTFOUND   = 12;   // Unknown opcode in sqlite3_file_control()
  SQLITE_FULL       = 13;   // Insertion failed because database is full
  SQLITE_CANTOPEN   = 14;   // Unable to open the database file
  SQLITE_PROTOCOL   = 15;   // Database lock protocol error
  SQLITE_EMPTY      = 16;   // Database is empty
  SQLITE_SCHEMA     = 17;   // The database schema changed
  SQLITE_TOOBIG     = 18;   // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT = 19;   // Abort due to constraint violation
  SQLITE_MISMATCH   = 20;   // Data type mismatch
  SQLITE_MISUSE     = 21;   // Library used incorrectly
  SQLITE_NOLFS      = 22;   // Uses OS features not supported on host
  SQLITE_AUTH       = 23;   // Authorization denied
  SQLITE_FORMAT     = 24;   // Auxiliary database format error
  SQLITE_RANGE      = 25;   // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB     = 26;   // File opened that is not a database file
  SQLITE_NOTICE     = 27;   // Notifications from sqlite3_log()
  SQLITE_WARNING    = 28;   // Warnings from sqlite3_log()
  SQLITE_ROW        = 100;  // sqlite3_step() has another row ready
  SQLITE_DONE       = 101;  // sqlite3_step() has finished executing

const
  SQLITE_OPEN_READONLY         = $00000001; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_READWRITE        = $00000002; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_CREATE           = $00000004; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_DELETEONCLOSE    = $00000008; // VFS only
  SQLITE_OPEN_EXCLUSIVE        = $00000010; // VFS only
  SQLITE_OPEN_AUTOPROXY        = $00000020; // VFS only
  SQLITE_OPEN_URI              = $00000040; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_MEMORY           = $00000080; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_MAIN_DB          = $00000100; // VFS only
  SQLITE_OPEN_TEMP_DB          = $00000200; // VFS only
  SQLITE_OPEN_TRANSIENT_DB     = $00000400; // VFS only
  SQLITE_OPEN_MAIN_JOURNAL     = $00000800; // VFS only
  SQLITE_OPEN_TEMP_JOURNAL     = $00001000; // VFS only
  SQLITE_OPEN_SUBJOURNAL       = $00002000; // VFS only
  SQLITE_OPEN_MASTER_JOURNAL   = $00004000; // VFS only
  SQLITE_OPEN_NOMUTEX          = $00008000; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_FULLMUTEX        = $00010000; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_SHAREDCACHE      = $00020000; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_PRIVATECACHE     = $00040000; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_WAL              = $00080000; // VFS only

const
  SQLITE_CONFIG_SINGLETHREAD         = 1;  // nil
  SQLITE_CONFIG_MULTITHREAD          = 2;  // nil
  SQLITE_CONFIG_SERIALIZED           = 3;  // nil
  SQLITE_CONFIG_MALLOC               = 4;  // sqlite3_mem_methods*
  SQLITE_CONFIG_GETMALLOC            = 5;  // sqlite3_mem_methods*
  SQLITE_CONFIG_SCRATCH              = 6;  // void*, int sz, int N
  SQLITE_CONFIG_PAGECACHE            = 7;  // void*, int sz, int N
  SQLITE_CONFIG_HEAP                 = 8;  // void*, int nByte, int min
  SQLITE_CONFIG_MEMSTATUS            = 9;  // boolean
  SQLITE_CONFIG_MUTEX               = 10;  // sqlite3_mutex_methods*
  SQLITE_CONFIG_GETMUTEX            = 11;  // sqlite3_mutex_methods*
  SQLITE_CONFIG_LOOKASIDE           = 13;  // int int
  SQLITE_CONFIG_PCACHE              = 14;  // no-op
  SQLITE_CONFIG_GETPCACHE           = 15;  // no-op
  SQLITE_CONFIG_LOG                 = 16;  // xFunc, void*
  SQLITE_CONFIG_URI                 = 17;  // int
  SQLITE_CONFIG_PCACHE2             = 18;  // sqlite3_pcache_methods2*
  SQLITE_CONFIG_GETPCACHE2          = 19;  // sqlite3_pcache_methods2*
  SQLITE_CONFIG_COVERING_INDEX_SCAN = 20;  // int
  SQLITE_CONFIG_SQLLOG              = 21;  // xSqllog, void*
  SQLITE_CONFIG_MMAP_SIZE           = 22;  // sqlite3_int64, sqlite3_int64
  SQLITE_CONFIG_WIN32_HEAPSIZE      = 23;  // int nByte
  SQLITE_CONFIG_PCACHE_HDRSZ        = 24;  // int *psz
  SQLITE_CONFIG_PMASZ               = 25;  // unsigned int szPma

const
  SQLITE_INTEGER  = 1;
  SQLITE_FLOAT    = 2;
  SQLITE_BLOB     = 4;
  SQLITE_NULL     = 5;
  SQLITE_TEXT     = 3;
  SQLITE3_TEXT    = 3;

type
  TALSqlite3Library = class(TObject)
  private
    Flibsqlite3: THandle;
  public
    sqlite3_libversion: function: PAnsiChar; cdecl;
    sqlite3_sourceid: function: PAnsiChar; cdecl;
    sqlite3_libversion_number: function: Integer; cdecl;
    sqlite3_threadsafe: function: Integer; cdecl;
    sqlite3_close: function(DbConnection: SQLite3): Integer; cdecl;
    sqlite3_close_v2: function(DbConnection: SQLite3): Integer; cdecl;
    sqlite3_initialize: function: Integer; cdecl;
    sqlite3_shutdown: function: Integer; cdecl;
    sqlite3_config: function(Option: Integer{; ...}): Integer; cdecl; {varargs;}
    sqlite3_db_config: function(DbConnection: SQLite3; op: Integer{; ...}): Integer; cdecl; {varargs;}
    sqlite3_open: function(const filename: PAnsiChar; out ppDb: SQLite3): Integer; cdecl;
    sqlite3_open16: function(const filename: PChar; out ppDb: SQLite3): Integer; cdecl;
    sqlite3_open_v2: function(const filename: PAnsiChar; out ppDb: SQLite3; flags: Integer; const zVfs: PAnsiChar): Integer; cdecl;
    sqlite3_errcode: function(db: SQLite3): Integer; cdecl;
    sqlite3_extended_errcode: function(db: SQLite3): Integer; cdecl;
    sqlite3_errmsg: function(db: SQLite3): PAnsiChar; cdecl;
    sqlite3_errmsg16: function(db: SQLite3): PChar; cdecl;
    sqlite3_prepare: function(db: SQLite3; const zSql: PAnsiChar; nByte: Integer; out ppStmt: SQLite3_Stmt; const pzTail: PPAnsiChar): Integer; cdecl;    //
    sqlite3_prepare_v2: function(db: SQLite3; const zSql: PAnsiChar; nByte: Integer; out ppStmt: SQLite3_Stmt; const pzTail: PPAnsiChar): Integer; cdecl; // const pzTail: PPAnsiChar and not out pzTail: MarshaledAString like defined in System.sqlite.pas
    sqlite3_prepare16: function(db: SQLite3; const zSql: PChar; nByte: Integer; out ppStmt: SQLite3_Stmt; const pzTail: PPChar): Integer; cdecl;          // https://stackoverflow.com/questions/48726053/what-does-this-mean-in-pascal-const-char-pztail/48727639#48727639
    sqlite3_prepare16_v2: function(db: SQLite3; const zSql: PChar; nByte: Integer; out ppStmt: SQLite3_Stmt; const pzTail: PPChar): Integer; cdecl;       //
    sqlite3_column_count: function(Statement: SQLite3_Stmt): Integer; cdecl;
    sqlite3_column_name: function(Statement: SQLite3_Stmt; N: Integer): PAnsiChar; cdecl;
    sqlite3_step: function(Statement: SQLite3_Stmt): Integer; cdecl;
    sqlite3_column_blob: function(Statement: SQLite3_Stmt; iCol: Integer): Pointer; cdecl;
    sqlite3_column_bytes: function(Statement: SQLite3_Stmt; iCol: Integer): Integer; cdecl;
    sqlite3_column_bytes16: function(Statement: SQLite3_Stmt; iCol: Integer): Integer; cdecl;
    sqlite3_column_double: function(Statement: SQLite3_Stmt; iCol: Integer): Double; cdecl;
    sqlite3_column_int: function(Statement: SQLite3_Stmt; iCol: Integer): Integer; cdecl;
    sqlite3_column_int64: function(Statement: SQLite3_Stmt; iCol: Integer): sqlite3_int64; cdecl;
    sqlite3_column_text: function(Statement: SQLite3_Stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_text16: function(Statement: SQLite3_Stmt; iCol: Integer): PChar; cdecl;
    sqlite3_column_type: function(Statement: SQLite3_Stmt; iCol: Integer): Integer; cdecl;
    sqlite3_finalize: function(pStmt: SQLite3_Stmt): Integer; cdecl;
    sqlite3_enable_shared_cache: function(enable: Integer): Integer; cdecl;
    constructor Create; virtual;
    destructor Destroy; override;
    function Loaded: Boolean; virtual;
    function Unload: Boolean; virtual;
    function Load(const lib: AnsiString = 'sqlite3.dll'; const initialize: boolean = True): Boolean; virtual;
  end;

implementation

Uses
  System.sysutils;

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
  Result := Flibsqlite3 <> 0;
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
    sqlite3_close_v2 := nil;
    sqlite3_initialize := nil;
    sqlite3_shutdown := nil;
    sqlite3_config := nil;
    sqlite3_db_config := nil;
    sqlite3_open := nil;
    sqlite3_open16 := nil;
    sqlite3_open_v2 := nil;
    sqlite3_errcode := nil;
    sqlite3_extended_errcode := nil;
    sqlite3_errmsg := nil;
    sqlite3_errmsg16 := nil;
    sqlite3_prepare := nil;
    sqlite3_prepare_v2 := nil;
    sqlite3_prepare16 := nil;
    sqlite3_prepare16_v2 := nil;
    sqlite3_column_count := nil;
    sqlite3_column_name := nil;
    sqlite3_step := nil;
    sqlite3_column_blob := nil;
    sqlite3_column_bytes := nil;
    sqlite3_column_bytes16 := nil;
    sqlite3_column_double := nil;
    sqlite3_column_int := nil;
    sqlite3_column_int64 := nil;
    sqlite3_column_text := nil;
    sqlite3_column_text16 := nil;
    sqlite3_column_type := nil;
    sqlite3_finalize := nil;
    sqlite3_enable_shared_cache := nil;
  end;
end;

{****************************************************************************************************************}
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
      sqlite3_close_v2 := GetProcAddress(Flibsqlite3,'sqlite3_close_v2');
      sqlite3_initialize := GetProcAddress(Flibsqlite3,'sqlite3_initialize');
      sqlite3_shutdown := GetProcAddress(Flibsqlite3,'sqlite3_shutdown');
      sqlite3_config := GetProcAddress(Flibsqlite3,'sqlite3_config');
      sqlite3_db_config := GetProcAddress(Flibsqlite3,'sqlite3_db_config');
      sqlite3_open := GetProcAddress(Flibsqlite3,'sqlite3_open');
      sqlite3_open16 := GetProcAddress(Flibsqlite3,'sqlite3_open16');
      sqlite3_open_v2 := GetProcAddress(Flibsqlite3,'sqlite3_open_v2');
      sqlite3_errcode := GetProcAddress(Flibsqlite3,'sqlite3_errcode');
      sqlite3_extended_errcode := GetProcAddress(Flibsqlite3,'sqlite3_extended_errcode');
      sqlite3_errmsg := GetProcAddress(Flibsqlite3,'sqlite3_errmsg');
      sqlite3_errmsg16 := GetProcAddress(Flibsqlite3,'sqlite3_errmsg16');
      sqlite3_prepare := GetProcAddress(Flibsqlite3,'sqlite3_prepare');
      sqlite3_prepare_v2 := GetProcAddress(Flibsqlite3,'sqlite3_prepare_v2');
      sqlite3_prepare16 := GetProcAddress(Flibsqlite3,'sqlite3_prepare16');
      sqlite3_prepare16_v2 := GetProcAddress(Flibsqlite3,'sqlite3_prepare16_v2');
      sqlite3_column_count := GetProcAddress(Flibsqlite3,'sqlite3_column_count');
      sqlite3_column_name := GetProcAddress(Flibsqlite3,'sqlite3_column_name');
      sqlite3_step := GetProcAddress(Flibsqlite3,'sqlite3_step');
      sqlite3_column_blob := GetProcAddress(Flibsqlite3,'sqlite3_column_blob');
      sqlite3_column_bytes := GetProcAddress(Flibsqlite3,'sqlite3_column_bytes');
      sqlite3_column_bytes16 := GetProcAddress(Flibsqlite3,'sqlite3_column_bytes16');
      sqlite3_column_double := GetProcAddress(Flibsqlite3,'sqlite3_column_double');
      sqlite3_column_int := GetProcAddress(Flibsqlite3,'sqlite3_column_int');
      sqlite3_column_int64 := GetProcAddress(Flibsqlite3,'sqlite3_column_int64');
      sqlite3_column_text := GetProcAddress(Flibsqlite3,'sqlite3_column_text');
      sqlite3_column_text16 := GetProcAddress(Flibsqlite3,'sqlite3_column_text16');
      sqlite3_column_type := GetProcAddress(Flibsqlite3,'sqlite3_column_type');
      sqlite3_finalize := GetProcAddress(Flibsqlite3,'sqlite3_finalize');
      sqlite3_enable_shared_cache := GetProcAddress(Flibsqlite3,'sqlite3_enable_shared_cache');

      Result := assigned(sqlite3_libversion) and
                assigned(sqlite3_sourceid) and
                assigned(sqlite3_libversion_number) and
                assigned(sqlite3_threadsafe) and
                assigned(sqlite3_close) and
                assigned(sqlite3_close_v2) and
                assigned(sqlite3_initialize) and
                assigned(sqlite3_shutdown) and
                assigned(sqlite3_config) and
                assigned(sqlite3_db_config) and
                assigned(sqlite3_open) and
                assigned(sqlite3_open16) and
                assigned(sqlite3_open_v2) and
                assigned(sqlite3_errcode) and
                assigned(sqlite3_extended_errcode) and
                assigned(sqlite3_errmsg) and
                assigned(sqlite3_errmsg16) and
                assigned(sqlite3_prepare) and
                assigned(sqlite3_prepare_v2) and
                assigned(sqlite3_prepare16) and
                assigned(sqlite3_prepare16_v2) and
                assigned(sqlite3_column_count) and
                assigned(sqlite3_column_name) and
                assigned(sqlite3_step) and
                assigned(sqlite3_column_blob) and
                assigned(sqlite3_column_bytes) and
                assigned(sqlite3_column_bytes16) and
                assigned(sqlite3_column_double) and
                assigned(sqlite3_column_int) and
                assigned(sqlite3_column_int64) and
                assigned(sqlite3_column_text) and
                assigned(sqlite3_column_text16) and
                assigned(sqlite3_column_type) and
                assigned(sqlite3_finalize) and
                assigned(sqlite3_enable_shared_cache);

      if not Result then begin
        Unload;
        raise Exception.Create('Incorrect Sqlite3.dll version');
      end
      else if initialize then begin
        if sqlite3_initialize <> SQLITE_OK then begin
          Unload;
          raise Exception.CreateFmt('Can''t load library: %s.', [lib]);
        end;
      end;
    end
    else raise Exception.CreateFmt('Can''t load library: %s.', [lib]);
  end;
end;

end.
