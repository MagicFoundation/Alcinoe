{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for SQLite             }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainSqLiteDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZCompatibility, ZPlainDriver, SysUtils, classes;

const
  WINDOWS_DLL_LOCATION = 'sqlite.dll';
  WINDOWS_DLL3_LOCATION = 'sqlite3.dll';
  LINUX_DLL_LOCATION = 'libsqlite.so';
  LINUX_DLL3_LOCATION = 'libsqlite3.so';

  SQLITE_ISO8859   = 1;
  MASTER_NAME      = 'sqlite_master';
  TEMP_MASTER_NAME = 'sqlite_temp_master';

  { Return values for sqlite_exec() and sqlite_step() }
  SQLITE_OK           = 0;   // Successful result
  SQLITE_ERROR        = 1;   // SQL error or missing database
  SQLITE_INTERNAL     = 2;   // An internal logic error in SQLite
  SQLITE_PERM         = 3;   // Access permission denied
  SQLITE_ABORT        = 4;   // Callback routine requested an abort
  SQLITE_BUSY         = 5;   // The database file is locked
  SQLITE_LOCKED       = 6;   // A table in the database is locked
  SQLITE_NOMEM        = 7;   // A malloc() failed
  SQLITE_READONLY     = 8;   // Attempt to write a readonly database
  _SQLITE_INTERRUPT    = 9;   // Operation terminated by sqlite_interrupt()
  SQLITE_IOERR        = 10;  // Some kind of disk I/O error occurred
  SQLITE_CORRUPT      = 11;  // The database disk image is malformed
  SQLITE_NOTFOUND     = 12;  // (Internal Only) Table or record not found
  SQLITE_FULL         = 13;  // Insertion failed because database is full
  SQLITE_CANTOPEN     = 14;  // Unable to open the database file
  SQLITE_PROTOCOL     = 15;  // Database lock protocol error
  SQLITE_EMPTY        = 16;  // (Internal Only) Database table is empty
  SQLITE_SCHEMA       = 17;  // The database schema changed
  SQLITE_TOOBIG       = 18;  // Too much data for one row of a table
  SQLITE_CONSTRAINT   = 19;  // Abort due to contraint violation
  SQLITE_MISMATCH     = 20;  // Data type mismatch
  SQLITE_MISUSE       = 21;  // Library used incorrectly
  SQLITE_NOLFS        = 22;  // Uses OS features not supported on host
  SQLITE_AUTH         = 23;  // Authorization denied
  SQLITE_FORMAT       = 24;  // Auxiliary database format error
  SQLITE_RANGE        = 25;  // 2nd parameter to sqlite_bind out of range
  SQLITE_NOTADB       = 26;  // File opened that is not a database file
  SQLITE_ROW          = 100;  // sqlite_step() has another row ready
  SQLITE_DONE         = 101;  // sqlite_step() has finished executing

  SQLITE_NUMERIC      = -1;
  SQLITE_TEXT         = -2;
  SQLITE_ARGS         = -3;

  {
    The second parameter to the access authorization function above will
    be one of the values below.  These values signify what kind of operation
    is to be authorized.  The 3rd and 4th parameters to the authorization
    function will be parameters or NULL depending on which of the following
    codes is used as the second parameter.  The 5th parameter is the name
    of the database ("main", "temp", etc.) if applicable.  The 6th parameter
    is the name of the inner-most trigger or view that is responsible for
    the access attempt or NULL if this access attempt is directly from
    input SQL code.

                                             Arg-3           Arg-4
  }
  SQLITE_COPY                  = 0;  // Table Name      File Name
  SQLITE_CREATE_INDEX          = 1;  // Index Name      Table Name
  SQLITE_CREATE_TABLE          = 2;  // Table Name      NULL
  SQLITE_CREATE_TEMP_INDEX     = 3;  // Index Name      Table Name
  SQLITE_CREATE_TEMP_TABLE     = 4;  // Table Name      NULL
  SQLITE_CREATE_TEMP_TRIGGER   = 5;  // Trigger Name    Table Name
  SQLITE_CREATE_TEMP_VIEW      = 6;  // View Name       NULL
  SQLITE_CREATE_TRIGGER        = 7;  // Trigger Name    Table Name
  SQLITE_CREATE_VIEW           = 8;  // View Name       NULL
  SQLITE_DELETE                = 9;  // Table Name      NULL
  SQLITE_DROP_INDEX            = 10; // Index Name      Table Name
  SQLITE_DROP_TABLE            = 11; // Table Name      NULL
  SQLITE_DROP_TEMP_INDEX       = 12; // Index Name      Table Name
  SQLITE_DROP_TEMP_TABLE       = 13; // Table Name      NULL
  SQLITE_DROP_TEMP_TRIGGER     = 14; // Trigger Name    Table Name
  SQLITE_DROP_TEMP_VIEW        = 15; // View Name       NULL
  SQLITE_DROP_TRIGGER          = 16; // Trigger Name    Table Name
  SQLITE_DROP_VIEW             = 17; // View Name       NULL
  SQLITE_INSERT                = 18; // Table Name      NULL
  SQLITE_PRAGMA                = 19; // Pragma Name     1st arg or NULL
  SQLITE_READ                  = 20; // Table Name      Column Name
  SQLITE_SELECT                = 21; // NULL            NULL
  SQLITE_TRANSACTION           = 22; // NULL            NULL
  SQLITE_UPDATE                = 23; // Table Name      Column Name
  SQLITE_ATTACH                = 24; // Filename        NULL
  SQLITE_DETACH                = 25; // Database Name   NULL

  { The return value of the authorization function should be one of the
    following constants: }
  SQLITE_DENY    = 1;   // Abort the SQL statement with an error
  SQLITE_IGNORE = 2;   // Don't allow access, but don't generate an error

type
  Psqlite = Pointer;
  Psqlite_func = Pointer;
  Psqlite_vm = Pointer;

{ ************** Plain API Function types definition ************* }

  Tsqlite_callback = function(p1: Pointer; p2: Integer; var p3: PAnsiChar;
    var p4: PAnsiChar): Integer; cdecl;
  Tsqlite_simple_callback = function(p1: Pointer): Integer; cdecl;
  Tsqlite_simple_callback0 = function(p1: Pointer): Pointer; cdecl;
  Tsqlite_busy_callback = function(p1: Pointer; const p2: PAnsiChar;
    p3: Integer): Integer; cdecl;

  Tsqlite_function_callback = procedure(p1: Psqlite_func; p2: Integer;
    const p3: PPAnsiChar); cdecl;
  Tsqlite_finalize_callback = procedure(p1: Psqlite_func); cdecl;
  Tsqlite_auth_callback = function(p1: Pointer; p2: Integer; const p3: PAnsiChar;
    const p4: PAnsiChar; const p5: PAnsiChar; const p6: PAnsiChar): Integer; cdecl;
  Tsqlite_trace_callback = procedure(p1: Pointer; const p2: PAnsiChar); cdecl;

  Tsqlite_open = function(const filename: PAnsiChar;var Qsqlite: Psqlite): Integer; cdecl;

  Tsqlite_close = procedure(db: Psqlite); cdecl;
  Tsqlite_column_count = function(db: Psqlite): Integer; cdecl;
  Tsqlite_column_bytes = function(db: Psqlite; iCol: Integer): PAnsiChar; cdecl;

  //  NEW : FST  100214
  Tsqlite_column_rawbytes = function(db: Psqlite; iCol: Integer): integer; cdecl;
  Tsqlite_column_blob = function(db:PSqlite;iCol:integer):PAnsiChar; cdecl;

  Tsqlite_column_name = function(db: Psqlite; iCol: Integer): PAnsiChar; cdecl;
  Tsqlite_column_decltype = function(db: Psqlite; iCol: Integer): PAnsiChar; cdecl;
  Tsqlite_exec = function(db: Psqlite; const sql: PAnsiChar;
    sqlite_callback: Tsqlite_callback; arg: Pointer;
    var errmsg: PAnsiChar): Integer; cdecl;
  Tsqlite_errmsg = function(db: Psqlite): PAnsiChar; cdecl;
  Tsqlite_last_insert_rowid = function(db: Psqlite): Integer; cdecl;
  Tsqlite_changes = function(db: Psqlite): Integer; cdecl;
  Tsqlite_last_statement_changes = function(db: Psqlite): Integer; cdecl;
  Tsqlite_error_string = function(code: Integer): PAnsiChar; cdecl;
  Tsqlite_interrupt = procedure(db: Psqlite); cdecl;
  Tsqlite_complete = function(const sql: PAnsiChar): Integer; cdecl;
  Tsqlite_busy_handler = procedure(db: Psqlite;
    callback: Tsqlite_busy_callback; ptr: Pointer); cdecl;
  Tsqlite_busy_timeout = procedure(db: Psqlite; ms: Integer); cdecl;
  Tsqlite_get_table = function(db: Psqlite; const sql: PAnsiChar;
    var resultp: PPAnsiChar; var nrow: Integer; var ncolumn: Integer;
    var errmsg: PAnsiChar): Integer; cdecl;
  Tsqlite_free_table = procedure(var result: PAnsiChar); cdecl;
  Tsqlite_freemem = procedure(ptr: Pointer); cdecl;
  Tsqlite_libversion = function: PAnsiChar; cdecl;
  Tsqlite_libencoding = function: PAnsiChar; cdecl;

  Tsqlite_create_function = function(db: Psqlite; const zName: PAnsiChar;
    nArg: Integer; callback: Tsqlite_function_callback;
    pUserData: Pointer): Integer; cdecl;
  Tsqlite_create_aggregate = function(db: Psqlite; const zName: PAnsiChar;
    nArg: Integer; callback: Tsqlite_function_callback;
    finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer; cdecl;
  Tsqlite_function_type = function(db: Psqlite; const zName: PAnsiChar;
    datatype: Integer): Integer; cdecl;

  Tsqlite_set_result_string = function(func: Psqlite_func; const arg: PAnsiChar;
    len: Integer; UN: Tsqlite_simple_callback): PAnsiChar; cdecl;

  Tsqlite_set_result_int = procedure(func: Psqlite_func; arg: Integer); cdecl;
  Tsqlite_set_result_double = procedure(func: Psqlite_func; arg: Double); cdecl;
  Tsqlite_set_result_error = procedure(func: Psqlite_func; const arg: PAnsiChar;
    len: Integer); cdecl;
  Tsqlite_user_data = function(func: Psqlite_func): Pointer; cdecl;
  Tsqlite_aggregate_context = function(func: Psqlite_func;
    nBytes: Integer): Pointer; cdecl;
  Tsqlite_aggregate_count = function(func: Psqlite_func): Integer; cdecl;

  Tsqlite_set_authorizer = function(db: Psqlite;
    callback: Tsqlite_auth_callback; pUserData: Pointer): Integer; cdecl;
  Tsqlite_trace = function(db: Psqlite; callback: Tsqlite_trace_callback;
    ptr: Pointer): Pointer; cdecl;

  Tsqlite_compile = function(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
     var ppVm: Psqlite_vm; pzTail: PAnsiChar): Integer; cdecl;
  Tsqlite_step = function(pVm: Psqlite_vm): Integer; cdecl;
  Tsqlite_finalize = function(vm: Psqlite_vm): Integer; cdecl;
  Tsqlite_reset = function(vm: Psqlite_vm): Integer; cdecl;
  Tsqlite_bind = function(vm: Psqlite_vm; idx: Integer; const value: PAnsiChar;
    len: Integer; copy: Integer): Integer; cdecl;
{  Tsqlite_bind_double = function(vm: Psqlite_vm; idx: Integer; const value: PAnsiChar;
    len: Integer; copy: Integer): Integer; cdecl;
  Tsqlite_bind_int = function(vm: Psqlite_vm; idx: Integer; const value: PAnsiChar;
    len: Integer; copy: Integer): Integer; cdecl;
  Tsqlite_bind_int64 = function(vm: Psqlite_vm; idx: Integer; const value: PAnsiChar;
    len: Integer; copy: Integer): Integer; cdecl;
  Tsqlite_bind_null = function(vm: Psqlite_vm; idx: Integer; const value: PAnsiChar;
    len: Integer; copy: Integer): Integer; cdecl;
  Tsqlite_bind_text = function(vm: Psqlite_vm; idx: Integer; const value: PAnsiChar;
    len: Integer; copy: Integer): Integer; cdecl;}

  Tsqlite_progress_handler = procedure(db: Psqlite; p1: Integer;
    callback: Tsqlite_simple_callback; ptr: Pointer); cdecl;
  Tsqlite_commit_hook = function(db: Psqlite; callback: Tsqlite_simple_callback;
    ptr: Pointer): Pointer; cdecl;

  Tsqlite_open_encrypted = function(const zFilename: PAnsiChar;
    const pKey: PAnsiChar; nKey: Integer; var pErrcode: Integer;
    var pzErrmsg: PAnsiChar): Psqlite; cdecl;
  Tsqlite_rekey = function(db: Psqlite; const pKey: Pointer;
    nKey: Integer): Integer; cdecl;
  Tsqlite_key = function(db: Psqlite; const pKey: Pointer;
    nKey: Integer): Integer; cdecl;

{ ************* Plain API Function variables definition ************ }
TZSQLite_API = record
  sqlite_open: Tsqlite_open;
  sqlite_close: Tsqlite_close;
  sqlite_column_count: Tsqlite_column_count;
  sqlite_column_bytes: Tsqlite_column_bytes;

//  NEW : FST  100214
  sqlite_column_rawbytes: Tsqlite_column_rawbytes;
  sqlite_column_blob:TSqlite_column_blob;

  sqlite_column_name: Tsqlite_column_name;
  sqlite_column_decltype: Tsqlite_column_decltype;
  sqlite_exec: Tsqlite_exec;
  sqlite_errmsg: Tsqlite_errmsg;
  sqlite_last_insert_rowid: Tsqlite_last_insert_rowid;
  sqlite_changes: Tsqlite_changes;
  sqlite_last_statement_changes: Tsqlite_last_statement_changes;
  sqlite_error_string: Tsqlite_error_string;
  sqlite_interrupt: Tsqlite_interrupt;
  sqlite_complete: Tsqlite_complete;
  sqlite_busy_handler: Tsqlite_busy_handler;
  sqlite_busy_timeout: Tsqlite_busy_timeout;
  sqlite_get_table: Tsqlite_get_table;
  sqlite_free_table: Tsqlite_free_table;
  sqlite_freemem: Tsqlite_freemem;
  sqlite_libversion: Tsqlite_libversion;
  sqlite_libencoding: Tsqlite_libencoding;
  sqlite_create_function: Tsqlite_create_function;
  sqlite_create_aggregate: Tsqlite_create_aggregate;
  sqlite_function_type: Tsqlite_function_type;
  sqlite_set_result_string: Tsqlite_set_result_string;
  sqlite_set_result_int: Tsqlite_set_result_int;
  sqlite_set_result_double: Tsqlite_set_result_double;
  sqlite_set_result_error: Tsqlite_set_result_error;
  sqlite_user_data: Tsqlite_user_data;
  sqlite_aggregate_context: Tsqlite_aggregate_context;
  sqlite_aggregate_count: Tsqlite_aggregate_count;
  sqlite_set_authorizer: Tsqlite_set_authorizer;
  sqlite_trace: Tsqlite_trace;
  sqlite_compile: Tsqlite_compile;
  sqlite_step: Tsqlite_step;
  sqlite_finalize: Tsqlite_finalize;
  sqlite_reset: Tsqlite_reset;
  sqlite_bind: Tsqlite_bind;
  sqlite_progress_handler: Tsqlite_progress_handler;
  sqlite_commit_hook: Tsqlite_commit_hook;
  sqlite_open_encrypted: Tsqlite_open_encrypted;
  sqlite_rekey: Tsqlite_rekey;
  sqlite_key: Tsqlite_key;
end;

type

  {** Represents a generic interface to SQLite native API. }
  IZSQLitePlainDriver = interface (IZPlainDriver)
    ['{B931C952-3076-4ECB-9630-D900E8DB9869}']

    function Open(const filename: PAnsiChar; mode: Integer;
      var errmsg: PAnsiChar): Psqlite;
    procedure Close(db: Psqlite);
    function Execute(db: Psqlite; const sql: PAnsiChar;
      sqlite_callback: Tsqlite_callback; arg: Pointer;
      var errmsg: PAnsiChar): Integer;
    function LastInsertRowId(db: Psqlite): Integer;
    function Changes(db: Psqlite): Integer;
    function LastStatementChanges(db: Psqlite): Integer;
    function ErrorString(code: Integer): PAnsiChar;
    procedure Interrupt(db: Psqlite);
    function Complete(const sql: PAnsiChar): Integer;

    procedure BusyHandler(db: Psqlite; callback: Tsqlite_busy_callback;
      ptr: Pointer);
    procedure BusyTimeout(db: Psqlite; ms: Integer);

    function GetTable(db: Psqlite; const sql: PAnsiChar; var resultp: PPAnsiChar;
      var nrow: Integer; var ncolumn: Integer; var errmsg: PAnsiChar): Integer;
    procedure FreeTable(var result: PAnsiChar);
    procedure FreeMem(ptr: Pointer);
    function LibVersion: PAnsiChar;
    function LibEncoding: PAnsiChar;

    function CreateFunction(db: Psqlite; const zName: PAnsiChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      pUserData: Pointer): Integer;
    function CreateAggregate(db: Psqlite; const zName: PAnsiChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
    function FunctionType(db: Psqlite; const zName: PAnsiChar;
      datatype: Integer): Integer;
    function SetResultString(func: Psqlite_func; const arg: PAnsiChar;
      len: Integer): PAnsiChar;
    procedure SetResultInt(func: Psqlite_func; arg: Integer);
    procedure SetResultDouble(func: Psqlite_func; arg: Double);
    procedure SetResultError(func: Psqlite_func; const arg: PAnsiChar; len: Integer);
    function UserData(func: Psqlite_func): Pointer;
    function AggregateContext(func: Psqlite_func; nBytes: Integer): Pointer;
    function AggregateCount(func: Psqlite_func): Integer;

    function SetAuthorizer(db: Psqlite; callback: Tsqlite_auth_callback;
      pUserData: Pointer): Integer;
    function Trace(db: Psqlite; callback: Tsqlite_trace_callback;
      ptr: Pointer): Pointer;

    function Compile(db: Psqlite; const zSql: PAnsiChar;nBytes: Integer;
      var pzTail: PAnsiChar; var ppVm: Psqlite_vm; var pzErrmsg: PAnsiChar): Integer;
    function Step(pVm: Psqlite_vm; var pN: Integer; var pazValue: PPAnsiChar;
      var pazColName: PPAnsiChar): Integer;
    function Finalize(vm: Psqlite_vm; var pzErrMsg: PAnsiChar): Integer;
    function Reset(vm: Psqlite_vm; var pzErrMsg: PAnsiChar): Integer;
    function Bind(vm: Psqlite_vm; idx: Integer; const value: PAnsiChar;
      len: Integer; copy: Integer): Integer;

    procedure ProgressHandler(db: Psqlite; p1: Integer;
      callback: Tsqlite_simple_callback; ptr: Pointer);
    function CommitHook(db: Psqlite; callback: Tsqlite_simple_callback;
      ptr: Pointer): Pointer;

    function OpenEncrypted(const zFilename: PAnsiChar; const pKey: PAnsiChar;
      nKey: Integer; var pErrcode: Integer; var pzErrmsg: PAnsiChar): Psqlite;
    function ReKey(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
    function Key(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;

//  NEW : FST  100214
    function getBlob(pVm: Psqlite_vm; columnID: integer): TMemoryStream;
  end;

  {** Implements a base driver for SQLite}
  TZSQLiteBaseDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZSQLitePlainDriver)
  protected
    SQLite_API : TZSQLite_API;
    // procedure LoadApi; override; ->completely done in version dependent child classes
  public
    constructor Create;

    function Open(const filename: PAnsiChar; mode: Integer;
      var errmsg: PAnsiChar): Psqlite;
    procedure Close(db: Psqlite);
    function Execute(db: Psqlite; const sql: PAnsiChar;
      sqlite_callback: Tsqlite_callback; arg: Pointer;
      var errmsg: PAnsiChar): Integer;
    function LastInsertRowId(db: Psqlite): Integer;
    function Changes(db: Psqlite): Integer;
    function LastStatementChanges(db: Psqlite): Integer;
    function ErrorString(code: Integer): PAnsiChar;
    procedure Interrupt(db: Psqlite);
    function Complete(const sql: PAnsiChar): Integer;

    procedure BusyHandler(db: Psqlite; callback: Tsqlite_busy_callback;
      ptr: Pointer);
    procedure BusyTimeout(db: Psqlite; ms: Integer);

    function GetTable(db: Psqlite; const sql: PAnsiChar; var resultp: PPAnsiChar;
      var nrow: Integer; var ncolumn: Integer; var errmsg: PAnsiChar): Integer;
    procedure FreeTable(var result: PAnsiChar);
    procedure FreeMem(ptr: Pointer);
    function LibVersion: PAnsiChar;
    function LibEncoding: PAnsiChar;

    function CreateFunction(db: Psqlite; const zName: PAnsiChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      pUserData: Pointer): Integer; virtual;
    function CreateAggregate(db: Psqlite; const zName: PAnsiChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
    function FunctionType(db: Psqlite; const zName: PAnsiChar;
      datatype: Integer): Integer;
    function SetResultString(func: Psqlite_func; const arg: PAnsiChar;
      len: Integer): PAnsiChar;
    procedure SetResultInt(func: Psqlite_func; arg: Integer);
    procedure SetResultDouble(func: Psqlite_func; arg: Double);
    procedure SetResultError(func: Psqlite_func; const arg: PAnsiChar; len: Integer);
    function UserData(func: Psqlite_func): Pointer;
    function AggregateContext(func: Psqlite_func; nBytes: Integer): Pointer;
    function AggregateCount(func: Psqlite_func): Integer;

    function SetAuthorizer(db: Psqlite; callback: Tsqlite_auth_callback;
      pUserData: Pointer): Integer;
    function Trace(db: Psqlite; callback: Tsqlite_trace_callback;
      ptr: Pointer): Pointer;

    function Compile(db: Psqlite; const zSql: PAnsiChar;
      nBytes: Integer;var pzTail: PAnsiChar;
      var ppVm: Psqlite_vm; var pzErrmsg: PAnsiChar): Integer;
    function Step(pVm: Psqlite_vm; var pN: Integer; var pazValue: PPAnsiChar;
      var pazColName: PPAnsiChar): Integer;
    function Finalize(vm: Psqlite_vm; var pzErrMsg: PAnsiChar): Integer;
    function Reset(vm: Psqlite_vm; var pzErrMsg: PAnsiChar): Integer;
    function Bind(vm: Psqlite_vm; idx: Integer; const value: PAnsiChar;
      len: Integer; copy: Integer): Integer;

    procedure ProgressHandler(db: Psqlite; p1: Integer;
      callback: Tsqlite_simple_callback; ptr: Pointer);
    function CommitHook(db: Psqlite; callback: Tsqlite_simple_callback;
      ptr: Pointer): Pointer;

    function OpenEncrypted(const zFilename: PAnsiChar; const pKey: PAnsiChar;
      nKey: Integer; var pErrcode: Integer; var pzErrmsg: PAnsiChar): Psqlite;
    function ReKey(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
    function Key(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;

//  NEW : FST  100214
    function getBlob(pVm: Psqlite_vm; columnID: integer): TMemoryStream;
  end;

  {** Implements a driver for SQLite 3 }
  TZSQLite3PlainDriver = class (TZSQLiteBaseDriver, IZPlainDriver, IZSQLitePlainDriver)
  protected
    procedure LoadApi; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

implementation

uses ZPlainLoader;

{ TZSQLiteBaseDriver }

constructor TZSQLiteBaseDriver.Create;
begin
   inherited create;
   FLoader := TZNativeLibraryLoader.Create([]);
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
  {$ENDIF}
end;

function TZSQLiteBaseDriver.AggregateContext(func: Psqlite_func;
  nBytes: Integer): Pointer;
begin
  Result := SQLite_API.sqlite_aggregate_context(func, nBytes);
end;

function TZSQLiteBaseDriver.AggregateCount(func: Psqlite_func): Integer;
begin
  Result := SQLite_API.sqlite_aggregate_count(func);
end;

function TZSQLiteBaseDriver.Bind(vm: Psqlite_vm; idx: Integer;
  const value: PAnsiChar; len, copy: Integer): Integer;
begin
  Result := SQLite_API.sqlite_bind(vm, idx, value, len, copy);
end;

procedure TZSQLiteBaseDriver.BusyHandler(db: Psqlite;
  callback: Tsqlite_busy_callback; ptr: Pointer);
begin
  SQLite_API.sqlite_busy_handler(db, callback, ptr);
end;

procedure TZSQLiteBaseDriver.BusyTimeout(db: Psqlite; ms: Integer);
begin
  SQLite_API.sqlite_busy_timeout(db, ms);
end;

function TZSQLiteBaseDriver.Changes(db: Psqlite): Integer;
begin
  Result := SQLite_API.sqlite_changes(db);
end;

function TZSQLiteBaseDriver.CommitHook(db: Psqlite;
  callback: Tsqlite_simple_callback; ptr: Pointer): Pointer;
begin
  Result := SQLite_API.sqlite_commit_hook(db, callback, ptr);
end;

function TZSQLiteBaseDriver.Compile(db: Psqlite; const zSql: PAnsiChar;
  nBytes: Integer;var pzTail: PAnsiChar;
  var ppVm: Psqlite_vm; var pzErrmsg: PAnsiChar): Integer;

begin
  Result := SQLite_API.sqlite_compile(db, zSql, -1, ppVm, nil);
  pzErrmsg := nil;
end;

function TZSQLiteBaseDriver.Complete(const sql: PAnsiChar): Integer;
begin
  Result := SQLite_API.sqlite_complete(sql);
end;

function TZSQLiteBaseDriver.CreateAggregate(db: Psqlite;
  const zName: PAnsiChar; nArg: Integer; callback: Tsqlite_function_callback;
  finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLiteBaseDriver.CreateFunction(db: Psqlite;
  const zName: PAnsiChar; nArg: Integer; callback: Tsqlite_function_callback;
  pUserData: Pointer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLiteBaseDriver.ErrorString(code: Integer): PAnsiChar;
begin
   case code of
    SQLITE_OK:         Result := 'not an error';
    SQLITE_ERROR:      Result := 'SQL logic error or missing database';
    SQLITE_INTERNAL:   Result := 'internal SQLite implementation flaw';
    SQLITE_PERM:       Result := 'access permission denied';
    SQLITE_ABORT:      Result := 'callback requested query abort';
    SQLITE_BUSY:       Result := 'database is locked';
    SQLITE_LOCKED:     Result := 'database table is locked';
    SQLITE_NOMEM:      Result := 'out of memory';
    SQLITE_READONLY:   Result := 'attempt to write a readonly database';
    _SQLITE_INTERRUPT:  Result := 'interrupted';
    SQLITE_IOERR:      Result := 'disk I/O error';
    SQLITE_CORRUPT:    Result := 'database disk image is malformed';
    SQLITE_NOTFOUND:   Result := 'table or record not found';
    SQLITE_FULL:       Result := 'database is full';
    SQLITE_CANTOPEN:   Result := 'unable to open database file';
    SQLITE_PROTOCOL:   Result := 'database locking protocol failure';
    SQLITE_EMPTY:      Result := 'table contains no data';
    SQLITE_SCHEMA:     Result := 'database schema has changed';
    SQLITE_TOOBIG:     Result := 'too much data for one table row';
    SQLITE_CONSTRAINT: Result := 'constraint failed';
    SQLITE_MISMATCH:   Result := 'datatype mismatch';
    SQLITE_MISUSE:     Result := 'library routine called out of sequence';
    SQLITE_NOLFS:      Result := 'kernel lacks large file support';
    SQLITE_AUTH:       Result := 'authorization denied';
    SQLITE_FORMAT:     Result := 'auxiliary database format error';
    SQLITE_RANGE:      Result := 'bind index out of range';
    SQLITE_NOTADB:     Result := 'file is encrypted or is not a database';
   else
      Result := 'unknown error';
  end;
end;

function TZSQLiteBaseDriver.Execute(db: Psqlite; const sql: PAnsiChar;
  sqlite_callback: Tsqlite_callback; arg: Pointer;
  var errmsg: PAnsiChar): Integer;
begin
  errmsg:= nil;
  Result := SQLite_API.sqlite_exec(db, sql, sqlite_callback, arg, errmsg);
end;

function TZSQLiteBaseDriver.Finalize(vm: Psqlite_vm;
  var pzErrMsg: PAnsiChar): Integer;
begin
   Result := SQLite_API.sqlite_finalize(vm);
   pzErrMsg := nil;
end;

procedure TZSQLiteBaseDriver.FreeMem(ptr: Pointer);
begin
  SQLite_API.sqlite_freemem(ptr);
end;

procedure TZSQLiteBaseDriver.FreeTable(var result: PAnsiChar);
begin
  SQLite_API.sqlite_free_table(result);
end;

function TZSQLiteBaseDriver.FunctionType(db: Psqlite;
  const zName: PAnsiChar; datatype: Integer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLiteBaseDriver.GetTable(db: Psqlite; const sql: PAnsiChar;
  var resultp: PPAnsiChar; var nrow, ncolumn: Integer;
  var errmsg: PAnsiChar): Integer;
begin
  Result := SQLite_API.sqlite_get_table(db, sql, resultp, nrow, ncolumn,
    errmsg);
end;

procedure TZSQLiteBaseDriver.Interrupt(db: Psqlite);
begin
  SQLite_API.sqlite_interrupt(db);
end;

function TZSQLiteBaseDriver.LastInsertRowId(db: Psqlite): Integer;
begin
  Result := SQLite_API.sqlite_last_insert_rowid(db);
end;

function TZSQLiteBaseDriver.LastStatementChanges(db: Psqlite): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLiteBaseDriver.LibEncoding: PAnsiChar;
begin
  Result := nil;
end;

function TZSQLiteBaseDriver.LibVersion: PAnsiChar;
begin
  Result := SQLite_API.sqlite_libversion;
end;

function TZSQLiteBaseDriver.Open(const filename: PAnsiChar; mode: Integer;
  var errmsg: PAnsiChar): Psqlite;
var
  Result0: Psqlite;
{$IFNDEF DELPHI12_UP}
  Version: string;
  FileNameString: String;
{$ENDIF}
begin
  Result0:= nil;
{$IFDEF DELPHI12_UP}
    SQLite_API.sqlite_open(filename, Result0);
{$ELSE}
  Version := LibVersion;
  FileNameString := filename;
  if (Version > '3.2.5') then
    SQLite_API.sqlite_open(PAnsiChar(AnsiToUTF8(FileNameString)), Result0)
  else
    SQLite_API.sqlite_open(filename, Result0);
{$ENDIF}
  Result := Result0;
end;

function TZSQLiteBaseDriver.OpenEncrypted(const zFilename, pKey: PAnsiChar;
  nKey: Integer; var pErrcode: Integer; var pzErrmsg: PAnsiChar): Psqlite;
begin
  pErrcode := SQLITE_MISUSE;
  pzErrmsg := 'function is not used in the current version of the library';
  Result:= nil;
end;

procedure TZSQLiteBaseDriver.ProgressHandler(db: Psqlite; p1: Integer;
  callback: Tsqlite_simple_callback; ptr: Pointer);
begin
  SQLite_API.sqlite_progress_handler(db, p1, callback, ptr);
end;

function TZSQLiteBaseDriver.ReKey(db: Psqlite; const pKey: Pointer;
  nKey: Integer): Integer;
begin
  if @SQLite_API.sqlite_rekey = nil then
  begin
    Result := SQLITE_OK;
  end
  else
  begin
    Result := SQLite_API.sqlite_rekey(db, pKey, nKey);
  end;
end;

function TZSQLiteBaseDriver.Key(db: Psqlite; const pKey: Pointer;
  nKey: Integer): Integer;
begin
  if @SQLite_API.sqlite_key = nil then
  begin
    Result := SQLITE_OK;
  end
  else
  begin
    Result := SQLite_API.sqlite_key(db, pKey, nKey);
  end;
end;

function TZSQLiteBaseDriver.Reset(vm: Psqlite_vm;
  var pzErrMsg: PAnsiChar): Integer;
begin
  Result := SQLite_API.sqlite_reset(vm);
  pzErrMsg := nil;
end;

function TZSQLiteBaseDriver.SetAuthorizer(db: Psqlite;
  callback: Tsqlite_auth_callback; pUserData: Pointer): Integer;
begin
  Result := SQLite_API.sqlite_set_authorizer(db, callback, pUserData);
end;

procedure TZSQLiteBaseDriver.SetResultDouble(func: Psqlite_func;
  arg: Double);
begin
  SQLite_API.sqlite_set_result_double(func, arg);
end;

procedure TZSQLiteBaseDriver.SetResultError(func: Psqlite_func;
  const arg: PAnsiChar; len: Integer);
begin
  SQLite_API.sqlite_set_result_error(func, arg, len);
end;

procedure TZSQLiteBaseDriver.SetResultInt(func: Psqlite_func;
  arg: Integer);
begin
  SQLite_API.sqlite_set_result_int(func, arg);
end;

function TZSQLiteBaseDriver.SetResultString(func: Psqlite_func;
  const arg: PAnsiChar; len: Integer): PAnsiChar;
begin
  Result := SQLite_API.sqlite_set_result_string(func, arg, len, nil);
end;


//  NEW : FST  100214
function TZSQLiteBaseDriver.getBlob(pVm:Psqlite_vm;columnID:integer):TMemoryStream;
var P : Pointer;
    len : integer;
begin
   result := TMemoryStream.Create;
   P := SQLite_API.sqlite_column_blob(pVm,columnID-1);
   len := SQLite_API.sqlite_column_rawbytes(pVm,ColumnID-1);
   result.WriteBuffer(P^,len);
end;

function TZSQLiteBaseDriver.Step(pVm: Psqlite_vm; var pN: Integer;
  var pazValue, pazColName: PPAnsiChar): Integer;
var
    i: Integer;
    val,cname,ctype: PAnsiChar;
    pazValue0, pazColName0, pazColType: PPAnsiChar;
begin
  pazValue0 := nil; // satisfy compiler
  Result := SQLite_API.sqlite_step(pVm);
  if (Result = SQLITE_ROW) or (Result = SQLITE_DONE) then
  begin
    pN:= SQLite_API.sqlite_column_count(pVm);
    if Result = SQLITE_ROW then
    begin
      pazValue:= AllocMem(SizeOf(PPAnsiChar)*(pN+1));
      pazValue0:= pazValue;
    end;
    pazColName:= AllocMem(SizeOf(PPAnsiChar)*(pN+1)*2);
    pazColName0:= pazColName;
    pazColType:= pazColName;

    Inc(pazColType, pN);
    for i := 0 to pN - 1 do
    begin
      if Result = SQLITE_ROW then
      begin
        cname:= SQLite_API.sqlite_column_name(pVm, i);
        ctype:= SQLite_API.sqlite_column_decltype(pVm, i);
        val  := SQLite_API.sqlite_column_bytes(pVm, i);
        pazValue0^ := val;
        inc(pazValue0);
      end
      else
      begin
        cname:= SQLite_API.sqlite_column_name(pVm, i);
        ctype:= SQLite_API.sqlite_column_decltype(pVm, i);
      end;
      pazColName0^:= cname;
      pazColType^ := ctype;
      inc(pazColName0);
      inc(pazColType);
    end;
    if Result = SQLITE_ROW then
         pazValue0^ := nil;
    pazColType^:= nil;
    if Result = SQLITE_DONE then
         pazValue := nil;
  end;
end;

function TZSQLiteBaseDriver.Trace(db: Psqlite;
  callback: Tsqlite_trace_callback; ptr: Pointer): Pointer;
begin
  Result := SQLite_API.sqlite_trace(db, callback, ptr);
end;

function TZSQLiteBaseDriver.UserData(func: Psqlite_func): Pointer;
begin
  Result := SQLite_API.sqlite_user_data(func);
end;

procedure TZSQLiteBaseDriver.Close(db: Psqlite);
begin
  SQLite_API.sqlite_close(db);
end;

{ TZSQLite3PlainDriver }

procedure TZSQLite3PlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  @SQLite_API.sqlite_open                   := GetAddress('sqlite3_open');
  @SQLite_API.sqlite_close                  := GetAddress('sqlite3_close');
  @SQLite_API.sqlite_column_count           := GetAddress('sqlite3_column_count');
  @SQLite_API.sqlite_column_bytes           := GetAddress('sqlite3_column_text');

//  NEW : FST  100214
  @SQLite_API.sqlite_column_rawbytes        := GetAddress('sqlite3_column_bytes');
  @SQLite_API.sqlite_Column_blob            := GetAddress('sqlite3_column_blob');

  @SQLite_API.sqlite_column_name            := GetAddress('sqlite3_column_name');
  @SQLite_API.sqlite_column_decltype        := GetAddress('sqlite3_column_decltype');
  @SQLite_API.sqlite_exec                   := GetAddress('sqlite3_exec');
  @SQLite_API.sqlite_last_insert_rowid      := GetAddress('sqlite3_last_insert_rowid');
  @SQLite_API.sqlite_changes                := GetAddress('sqlite3_changes');
//  @SQLite_API.sqlite_last_statement_changes := GetAddress('sqlite3_last_statement_changes');
  @SQLite_API.sqlite_errmsg                 := GetAddress('sqlite3_errmsg');
  @SQLite_API.sqlite_interrupt              := GetAddress('sqlite3_interrupt');
  @SQLite_API.sqlite_complete               := GetAddress('sqlite3_complete');
  @SQLite_API.sqlite_busy_handler           := GetAddress('sqlite3_busy_handler');
  @SQLite_API.sqlite_busy_timeout           := GetAddress('sqlite3_busy_timeout');
  @SQLite_API.sqlite_get_table              := GetAddress('sqlite3_get_table');
  @SQLite_API.sqlite_free_table             := GetAddress('sqlite3_free_table');
  @SQLite_API.sqlite_freemem                := GetAddress('sqlite3_free');
  @SQLite_API.sqlite_libversion             := GetAddress('sqlite3_libversion');
//  @SQLite_API.sqlite_libencoding            := GetAddress('sqlite3_libencoding');
//  @SQLite_API.sqlite_create_function        := GetAddress('sqlite3_create_function');
//  @SQLite_API.sqlite_create_aggregate       := GetAddress('sqlite3_create_aggregate');
//  @SQLite_API.sqlite_function_type          := GetAddress('sqlite3_function_type');
  @SQLite_API.sqlite_set_result_string      := GetAddress('sqlite3_result_string');
  @SQLite_API.sqlite_set_result_int         := GetAddress('sqlite3_result_int');
  @SQLite_API.sqlite_set_result_double      := GetAddress('sqlite3_result_double');
  @SQLite_API.sqlite_set_result_error       := GetAddress('sqlite3_result_error');
  @SQLite_API.sqlite_user_data              := GetAddress('sqlite3_user_data');
  @SQLite_API.sqlite_aggregate_context      := GetAddress('sqlite3_aggregate_context');
  @SQLite_API.sqlite_aggregate_count        := GetAddress('sqlite3_aggregate_count');
  @SQLite_API.sqlite_set_authorizer         := GetAddress('sqlite3_set_authorizer');
  @SQLite_API.sqlite_trace                  := GetAddress('sqlite3_trace');
  @SQLite_API.sqlite_compile                := GetAddress('sqlite3_prepare');
  @SQLite_API.sqlite_step                   := GetAddress('sqlite3_step');
  @SQLite_API.sqlite_finalize               := GetAddress('sqlite3_finalize');
  @SQLite_API.sqlite_reset                  := GetAddress('sqlite3_reset');
//  @SQLite_API.sqlite_bind                   := GetAddress('sqlite3_bind');
  @SQLite_API.sqlite_progress_handler       := GetAddress('sqlite3_progress_handler');
  @SQLite_API.sqlite_commit_hook            := GetAddress('sqlite3_commit_hook');
//  @SQLite_API.sqlite_open_encrypted         := GetAddress('sqlite3_open_encrypted');
  @SQLite_API.sqlite_rekey                  := GetAddress('sqlite3_rekey');
  @SQLite_API.sqlite_key                    := GetAddress('sqlite3_key');
  end;
end;

constructor TZSQLite3PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL3_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL3_LOCATION);
  {$ENDIF}
end;

function TZSQLite3PlainDriver.GetProtocol: string;
begin
  Result := 'sqlite-3';
end;

function TZSQLite3PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for SQLite 3';
end;

end.

