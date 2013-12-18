{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for MySQL              }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{    Thanks to :                                          }
{               Pascal Data Objects Library               }
{                                                         }
{    Copyright (c) 2006 John Marino, www.synsport.com     }
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

unit ZPlainMySqlDriver;

interface

{$I ZPlain.inc}

uses Classes, ZClasses, ZPlainDriver, ZCompatibility, ZPlainMySqlConstants;

const
{$IFNDEF UNIX}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  WINDOWS_DLL_LOCATION = 'libmysql.dll';
  WINDOWS_DLL_LOCATION_EMBEDDED = 'libmysqld.dll';
  {$ENDIF}
  WINDOWS_DLL41_LOCATION = 'libmysql41.dll';
  WINDOWS_DLL41_LOCATION_EMBEDDED = 'libmysqld41.dll';
  WINDOWS_DLL50_LOCATION = 'libmysql50.dll';
  WINDOWS_DLL50_LOCATION_EMBEDDED = 'libmysqld50.dll';
  WINDOWS_DLL51_LOCATION = 'libmysql51.dll';
  WINDOWS_DLL51_LOCATION_EMBEDDED = 'libmysqld51.dll';
  WINDOWS_DLL55_LOCATION = 'libmysql55.dll';
  WINDOWS_DLL55_LOCATION_EMBEDDED = 'libmysqld55.dll';
{$ELSE}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  LINUX_DLL_LOCATION = 'libmysqlclient.so';
  LINUX_DLL_LOCATION_EMBEDDED = 'libmysqld.so';
  {$ENDIF}
  LINUX_DLL41_LOCATION = 'libmysqlclient.so.14';
  LINUX_DLL41_LOCATION_EMBEDDED = 'libmysqld.so.14';
  LINUX_DLL50_LOCATION = 'libmysqlclient.so.15';
  LINUX_DLL50_LOCATION_EMBEDDED = 'libmysqld.so.15';
  LINUX_DLL51_LOCATION = 'libmysqlclient.so.16';
  LINUX_DLL51_LOCATION_EMBEDDED = 'libmysqld.so.16';
{$ENDIF}

type
  {** Represents a generic interface to MySQL native API. }
  IZMySQLPlainDriver = interface (IZPlainDriver)
    ['{D1CB3F6C-72A1-4125-873F-791202ACC5F0}']
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    procedure Despose(var Handle: PZMySQLConnect);

    function GetAffectedRows(Handle: PZMySQLConnect): Int64;
    // char_set_name
    procedure Close(Handle: PZMySQLConnect);
    function Connect(Handle: PZMySQLConnect; const Host, User, Password: PAnsiChar): PZMySQLConnect;
    function CreateDatabase(Handle: PZMySQLConnect; const Database: PAnsiChar): Integer;
    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    procedure Debug(Debug: PAnsiChar);
    function DropDatabase(Handle: PZMySQLConnect; const Database: PAnsiChar): Integer;
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    // eof
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PAnsiChar;
    function GetEscapeString(StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;
    function FetchField(Res: PZMySQLResult): PZMySQLField;
    // fetch_field_direct
    // fetch_fields
    function FetchLengths(Res: PZMySQLResult): PULong;
    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;
    // field_tell
    procedure FreeResult(Res: PZMySQLResult);
    function GetClientInfo: PAnsiChar;
    function GetHostInfo(Handle: PZMySQLConnect): PAnsiChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetServerInfo(Handle: PZMySQLConnect): PAnsiChar;
    // info
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function GetListDatabases(Handle: PZMySQLConnect; Wild: PAnsiChar): PZMySQLResult;
    function GetListFields(Handle: PZMySQLConnect; const Table, Wild: PAnsiChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect; const Wild: PAnsiChar): PZMySQLResult;
    // num_fields
    function GetNumRows(Res: PZMySQLResult): Int64;
    function SetOptions(Handle: PZMySQLConnect; Option: TMySQLOption; const Arg: PAnsiChar): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;
    function ExecQuery(Handle: PZMySQLConnect; const Query: PAnsiChar): Integer;
    function RealConnect(Handle: PZMySQLConnect; const Host, User, Password, Db: PAnsiChar; Port: Cardinal; UnixSocket: PAnsiChar; ClientFlag: Cardinal): PZMySQLConnect;
    function GetRealEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PAnsiChar; Length: Integer): Integer;
    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    // row_tell
    function SelectDatabase(Handle: PZMySQLConnect; const Database: PAnsiChar): Integer;
    function SslSet(Handle: PZMySQLConnect; const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
    function GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;

    // my_init
    // thread_init
    // thread_end
    // thread_safe

    // server_init
    // server_end

    // change_user
    // field_count
    // function GetClientVersion: AnsiString;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMySqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!

    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    //function GetServerVersion (Handle: PZMySQLConnect): AnsiString;
    // hex_string
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    // set_character_set
    // set_server_option
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;
    // warning_count

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    // stmt_attr_get
    // stmt_attr_set
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    procedure SeekPreparedData(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    // stmt_fetch_column
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    // stmt_free_result
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    // stmt_param_metadata
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
    // stmt_reset
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function SeekPreparedRow(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    // stmt_row_tell
    // stmt_send_long_data
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PAnsiChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    // get_character_set_info

    {non API functions}
    function GetFieldType(Field: PZMySQLField): TMysqlFieldTypes;
    function GetFieldFlags(Field: PZMySQLField): Integer;
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PAnsiChar;
    function GetFieldTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigName(Field: PZMySQLField): PAnsiChar;
    function GetFieldLength(Field: PZMySQLField): ULong;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PAnsiChar;
    procedure SetDriverOptions(Options: TStrings); // changed by tohenk, 2009-10-11
  end;

  {** Implements a base driver for MySQL}

  { TZMySQLBaseDriver }

  TZMySQLBaseDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZMySQLPlainDriver)
  protected
    MYSQL_API : TZMYSQL_API;
    ServerArgs: array of PAnsiChar;
    ServerArgsLen: Integer;
    IsEmbeddedDriver: Boolean;
    procedure LoadApi; override;
    procedure BuildServerArguments(Options: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Debug(Debug: PAnsiChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PAnsiChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; virtual;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    procedure Despose(var Handle: PZMySQLConnect);

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PAnsiChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PAnsiChar; Port: Cardinal;
      UnixSocket: PAnsiChar; ClientFlag: Cardinal): PZMySQLConnect;
    function GetRealEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PAnsiChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PAnsiChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;
    function SslSet(Handle: PZMySQLConnect; const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMySqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!
    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    procedure SeekPreparedData(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function SeekPreparedRow(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PAnsiChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TMySQLOption;
      const Arg: PAnsiChar): Integer;
    function GetEscapeString(StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;

    function GetServerInfo(Handle: PZMySQLConnect): PAnsiChar;
    function GetClientInfo: PAnsiChar;
    function GetHostInfo(Handle: PZMySQLConnect): PAnsiChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    function GetListDatabases(Handle: PZMySQLConnect;
      Wild: PAnsiChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect;
      const Wild: PAnsiChar): PZMySQLResult;
    function GetNumRows(Res: PZMySQLResult): Int64;
    function GetListFields(Handle: PZMySQLConnect;
      const Table, Wild: PAnsiChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;

    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PULong;
    function FetchField(Res: PZMySQLResult): PZMySQLField;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset):
      PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;

    function GetFieldType(Field: PZMySQLField): TMysqlFieldTypes;
    function GetFieldFlags(Field: PZMySQLField): Integer;
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PAnsiChar;
    function GetFieldTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigName(Field: PZMySQLField): PAnsiChar;
    function GetFieldLength(Field: PZMySQLField): ULong;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PAnsiChar;
    procedure SetDriverOptions(Options: TStrings); virtual; // changed by tohenk, 2009-10-11
  end;

  {** Implements a driver for MySQL 4.1 }

  { TZNewMySQL41PlainDriver }

  TZMySQL41PlainDriver = class (TZMysqlBaseDriver)
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Implements a driver for MySQL 4.1 }

  { TZNewMySQLD41PlainDriver }

  TZMySQLD41PlainDriver = class (TZMySQL41PlainDriver)
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZNewMySQL5PlainDriver }

  TZMySQL5PlainDriver = class (TZMysqlBaseDriver)
  protected
    procedure LoadApi; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZNewMySQLD5PlainDriver }

  TZMySQLD5PlainDriver = class (TZMySQL5PlainDriver)
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

implementation
uses SysUtils, ZSysUtils, ZPlainLoader;

{ TZMySQLPlainBaseDriver }

procedure TZMySQLBaseDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  @MYSQL_API.mysql_affected_rows          := GetAddress('mysql_affected_rows');
  @MYSQL_API.mysql_character_set_name     := GetAddress('mysql_character_set_name');
  @MYSQL_API.mysql_close                  := GetAddress('mysql_close');
  @MYSQL_API.mysql_connect                := GetAddress('mysql_connect');
  @MYSQL_API.mysql_create_db              := GetAddress('mysql_create_db');
  @MYSQL_API.mysql_data_seek              := GetAddress('mysql_data_seek');
  @MYSQL_API.mysql_debug                  := GetAddress('mysql_debug');
  @MYSQL_API.mysql_drop_db                := GetAddress('mysql_drop_db');
  @MYSQL_API.mysql_dump_debug_info        := GetAddress('mysql_dump_debug_info');
  @MYSQL_API.mysql_eof                    := GetAddress('mysql_eof');
  @MYSQL_API.mysql_errno                  := GetAddress('mysql_errno');
  @MYSQL_API.mysql_error                  := GetAddress('mysql_error');
  @MYSQL_API.mysql_escape_string          := GetAddress('mysql_escape_string');
  @MYSQL_API.mysql_fetch_field            := GetAddress('mysql_fetch_field');
  @MYSQL_API.mysql_fetch_field_direct     := GetAddress('mysql_fetch_field_direct');
  @MYSQL_API.mysql_fetch_fields           := GetAddress('mysql_fetch_fields');
  @MYSQL_API.mysql_fetch_lengths          := GetAddress('mysql_fetch_lengths');
  @MYSQL_API.mysql_fetch_row              := GetAddress('mysql_fetch_row');
  @MYSQL_API.mysql_field_seek             := GetAddress('mysql_field_seek');
  @MYSQL_API.mysql_field_tell             := GetAddress('mysql_field_tell');
  @MYSQL_API.mysql_free_result            := GetAddress('mysql_free_result');
  @MYSQL_API.mysql_get_client_info        := GetAddress('mysql_get_client_info');
  @MYSQL_API.mysql_get_host_info          := GetAddress('mysql_get_host_info');
  @MYSQL_API.mysql_get_proto_info         := GetAddress('mysql_get_proto_info');
  @MYSQL_API.mysql_get_server_info        := GetAddress('mysql_get_server_info');
  @MYSQL_API.mysql_info                   := GetAddress('mysql_info');
  @MYSQL_API.mysql_init                   := GetAddress('mysql_init');
  @MYSQL_API.mysql_insert_id              := GetAddress('mysql_insert_id');
  @MYSQL_API.mysql_kill                   := GetAddress('mysql_kill');
  @MYSQL_API.mysql_list_dbs               := GetAddress('mysql_list_dbs');
  @MYSQL_API.mysql_list_fields            := GetAddress('mysql_list_fields');
  @MYSQL_API.mysql_list_processes         := GetAddress('mysql_list_processes');
  @MYSQL_API.mysql_list_tables            := GetAddress('mysql_list_tables');
  @MYSQL_API.mysql_num_fields             := GetAddress('mysql_num_fields');
  @MYSQL_API.mysql_num_rows               := GetAddress('mysql_num_rows');
  @MYSQL_API.mysql_options                := GetAddress('mysql_options');
  @MYSQL_API.mysql_ping                   := GetAddress('mysql_ping');
  @MYSQL_API.mysql_query                  := GetAddress('mysql_query');
  @MYSQL_API.mysql_real_connect           := GetAddress('mysql_real_connect');
  @MYSQL_API.mysql_real_escape_string     := GetAddress('mysql_real_escape_string');
  @MYSQL_API.mysql_real_query             := GetAddress('mysql_real_query');
  @MYSQL_API.mysql_refresh                := GetAddress('mysql_refresh');
  @MYSQL_API.mysql_row_seek               := GetAddress('mysql_row_seek');
  @MYSQL_API.mysql_row_tell               := GetAddress('mysql_row_tell');
  @MYSQL_API.mysql_select_db              := GetAddress('mysql_select_db');
  @MYSQL_API.mysql_shutdown               := GetAddress('mysql_shutdown');
  @MYSQL_API.mysql_ssl_set                := GetAddress('mysql_ssl_set');
  @MYSQL_API.mysql_stat                   := GetAddress('mysql_stat');
  @MYSQL_API.mysql_store_result           := GetAddress('mysql_store_result');
  @MYSQL_API.mysql_thread_id              := GetAddress('mysql_thread_id');
  @MYSQL_API.mysql_use_result             := GetAddress('mysql_use_result');

  @MYSQL_API.my_init                      := GetAddress('my_init');
  @MYSQL_API.mysql_thread_init            := GetAddress('mysql_thread_init');
  @MYSQL_API.mysql_thread_end             := GetAddress('mysql_thread_end');
  @MYSQL_API.mysql_thread_safe            := GetAddress('mysql_thread_safe');

  @MYSQL_API.mysql_server_init            := GetAddress('mysql_server_init');
  @MYSQL_API.mysql_server_end             := GetAddress('mysql_server_end');

  @MYSQL_API.mysql_change_user            := GetAddress('mysql_change_user');
  @MYSQL_API.mysql_field_count            := GetAddress('mysql_field_count');

  @MYSQL_API.mysql_get_client_version     := GetAddress('mysql_get_client_version');

  @MYSQL_API.mysql_send_query      := GetAddress('mysql_send_query');
  @MYSQL_API.mysql_read_query_result := GetAddress('mysql_read_query_result');

  @MYSQL_API.mysql_autocommit             := GetAddress('mysql_autocommit');
  @MYSQL_API.mysql_commit                 := GetAddress('mysql_commit');
  @MYSQL_API.mysql_get_server_version     := GetAddress('mysql_get_server_version');
  @MYSQL_API.mysql_hex_string             := GetAddress('mysql_hex_string');
  @MYSQL_API.mysql_more_results           := GetAddress('mysql_more_results');
  @MYSQL_API.mysql_next_result            := GetAddress('mysql_next_result');
  @MYSQL_API.mysql_rollback               := GetAddress('mysql_rollback');
  @MYSQL_API.mysql_set_character_set      := GetAddress('mysql_set_character_set');
  @MYSQL_API.mysql_set_server_option      := GetAddress('mysql_set_server_option');
  @MYSQL_API.mysql_sqlstate               := GetAddress('mysql_sqlstate');
  @MYSQL_API.mysql_warning_count          := GetAddress('mysql_warning_count');
  {API for PREPARED STATEMENTS}
  @MYSQL_API.mysql_stmt_affected_rows     := GetAddress('mysql_stmt_affected_rows');
  @MYSQL_API.mysql_stmt_attr_get          := GetAddress('mysql_stmt_attr_get');
  @MYSQL_API.mysql_stmt_attr_set          := GetAddress('mysql_stmt_attr_set');
  @MYSQL_API.mysql_stmt_bind_param        := GetAddress('mysql_stmt_bind_param');
  @MYSQL_API.mysql_stmt_bind_result       := GetAddress('mysql_stmt_bind_result');
  @MYSQL_API.mysql_stmt_close             := GetAddress('mysql_stmt_close');
  @MYSQL_API.mysql_stmt_data_seek         := GetAddress('mysql_stmt_data_seek');
  @MYSQL_API.mysql_stmt_errno             := GetAddress('mysql_stmt_errno');
  @MYSQL_API.mysql_stmt_error             := GetAddress('mysql_stmt_error');
  @MYSQL_API.mysql_stmt_execute           := GetAddress('mysql_stmt_execute');
  @MYSQL_API.mysql_stmt_fetch             := GetAddress('mysql_stmt_fetch');
  @MYSQL_API.mysql_stmt_fetch_column      := GetAddress('mysql_stmt_fetch_column');
  @MYSQL_API.mysql_stmt_field_count       := GetAddress('mysql_stmt_field_count');
  @MYSQL_API.mysql_stmt_free_result       := GetAddress('mysql_stmt_free_result');
  @MYSQL_API.mysql_stmt_init              := GetAddress('mysql_stmt_init');
  @MYSQL_API.mysql_stmt_insert_id         := GetAddress('mysql_stmt_insert_id');
  @MYSQL_API.mysql_stmt_num_rows          := GetAddress('mysql_stmt_num_rows');
  @MYSQL_API.mysql_stmt_param_count       := GetAddress('mysql_stmt_param_count');
  @MYSQL_API.mysql_stmt_param_metadata    := GetAddress('mysql_stmt_param_metadata');
  @MYSQL_API.mysql_stmt_prepare           := GetAddress('mysql_stmt_prepare');
  @MYSQL_API.mysql_stmt_reset             := GetAddress('mysql_stmt_reset');
  @MYSQL_API.mysql_stmt_result_metadata   := GetAddress('mysql_stmt_result_metadata');
  @MYSQL_API.mysql_stmt_row_seek          := GetAddress('mysql_stmt_row_seek');
  @MYSQL_API.mysql_stmt_row_tell          := GetAddress('mysql_stmt_row_tell');
  @MYSQL_API.mysql_stmt_send_long_data    := GetAddress('mysql_stmt_send_long_data');
  @MYSQL_API.mysql_stmt_sqlstate          := GetAddress('mysql_stmt_sqlstate');
  @MYSQL_API.mysql_stmt_store_result      := GetAddress('mysql_stmt_store_result');
  end;
end;

procedure TZMySQLBaseDriver.BuildServerArguments(Options: TStrings);
var
  TmpList: TStringList;
  i: Integer;
begin
  TmpList := TStringList.Create;
  try
    TmpList.Add(ParamStr(0));
    for i := 0 to Options.Count - 1 do
      if SameText(SERVER_ARGUMENTS_KEY_PREFIX,
                  Copy(Options.Names[i], 1,
                       Length(SERVER_ARGUMENTS_KEY_PREFIX))) then
{$IFDEF VER140}
        TmpList.Add(Options.Values[Options.Names[i]]);
{$ELSE}
        TmpList.Add(Options.ValueFromIndex[i]);
{$ENDIF}
    //Check if DataDir is specified, if not, then add it to the Arguments List
    if TmpList.Values['--datadir'] = '' then
       TmpList.Add('--datadir='+EMBEDDED_DEFAULT_DATA_DIR);
    ServerArgsLen := TmpList.Count;
    SetLength(ServerArgs, ServerArgsLen);
    for i := 0 to ServerArgsLen - 1 do
      {$IFDEF DELPHI12_UP}
      ServerArgs[i] := StrNew(PAnsiChar(UTF8String(TmpList[i])));
      {$ELSE}
      ServerArgs[i] := StrNew(PAnsiChar(TmpList[i]));
      {$ENDIF}
  finally
    TmpList.Free;
  end;
end;

constructor TZMySQLBaseDriver.Create;
begin
   inherited create;
   FLoader := TZNativeLibraryLoader.Create([]);
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
  {$ENDIF}
{$ENDIF}
  ServerArgsLen := 0;
  SetLength(ServerArgs, ServerArgsLen);
  IsEmbeddedDriver := False;
end;

destructor TZMySQLBaseDriver.Destroy;
begin
  if (FLoader.Loaded) and (@MYSQL_API.mysql_server_end <> nil) then
    MYSQL_API.mysql_server_end;

  inherited Destroy;
end;

procedure TZMySQLBaseDriver.Close(Handle: PZMySQLConnect);
begin
  MYSQL_API.mysql_close(Handle);
end;

function TZMySQLBaseDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PAnsiChar): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQLBaseDriver.SslSet(Handle: PZMySQLConnect;
  const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_ssl_set(Handle, Key, Cert, Ca, Capath, Cipher);
end;

function TZMySQLBaseDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_create_db(Handle, Database);
end;

procedure TZMySQLBaseDriver.Debug(Debug: PAnsiChar);
begin
  MYSQL_API.mysql_debug(Debug);
end;

function TZMySQLBaseDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_drop_db(Handle, Database);
end;

function TZMySQLBaseDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_dump_debug_info(Handle);
end;

function TZMySQLBaseDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_query(Handle, Query);
end;

function TZMySQLBaseDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PAnsiChar; Length: Integer): Integer;
begin
  Result := MYSQL_API.mysql_real_query(Handle, Query, Length);
end;

function TZMySQLBaseDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := MYSQL_API.mysql_fetch_field(Res);
end;

function TZMySQLBaseDriver.FetchLengths(Res: PZMySQLResult): PULong;
begin
  Result := MYSQL_API.mysql_fetch_lengths(Res);
end;

function TZMySQLBaseDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := MYSQL_API.mysql_fetch_row(Res);
end;

procedure TZMySQLBaseDriver.FreeResult(Res: PZMySQLResult);
begin
  MYSQL_API.mysql_free_result(Res);
end;

function TZMySQLBaseDriver.GetAffectedRows(Handle: PZMySQLConnect): Int64;
begin
  Result := MYSQL_API.mysql_affected_rows(Handle);
end;

function TZMySQLBaseDriver.GetClientInfo: PAnsiChar;
begin
  Result := MYSQL_API.mysql_get_client_info;
end;

function TZMySQLBaseDriver.GetEscapeString(StrTo, StrFrom: PAnsiChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_escape_string(StrTo, StrFrom, Length);
end;

function TZMySQLBaseDriver.GetHostInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_get_host_info(Handle);
end;

function TZMySQLBaseDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PAnsiChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_dbs(Handle, Wild);
end;

function TZMySQLBaseDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PAnsiChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQLBaseDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_processes(Handle);
end;

function TZMySQLBaseDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PAnsiChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_tables(Handle, Wild);
end;

function TZMySQLBaseDriver.GetNumRows(Res: PZMySQLResult): Int64;
begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_num_rows (Res);
end;

function TZMySQLBaseDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_get_proto_info(Handle);
end;

function TZMySQLBaseDriver.GetServerInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_get_server_info(Handle);
end;

function TZMySQLBaseDriver.GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_stat(Handle);
end;

function TZMySQLBaseDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_thread_id(Handle);
end;

function TZMySQLBaseDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
  begin
    MYSQL_API.mysql_server_init(ServerArgsLen, ServerArgs, @SERVER_GROUPS);
  end;
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

function TZMySQLBaseDriver.getLastInsertID (Handle: PZMySQLConnect): Int64;
begin
    Result := MYSQL_API.mysql_insert_id(PMYSQL(Handle));
end;

procedure TZMySQLBaseDriver.Despose(var Handle: PZMySQLConnect);
begin
  Handle := nil;
end;

function TZMySQLBaseDriver.Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
begin
  Result := MYSQL_API.mysql_kill(Handle, Pid);
end;

function TZMySQLBaseDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_ping(Handle);
end;

function TZMySQLBaseDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PAnsiChar; Port: Cardinal; UnixSocket: PAnsiChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQLBaseDriver.GetRealEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PAnsiChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_real_escape_string(Handle, StrTo, StrFrom, Length);
end;

function TZMySQLBaseDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := MYSQL_API.mysql_refresh(Handle, Options);
end;

procedure TZMySQLBaseDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  MYSQL_API.mysql_data_seek(Res, Offset);
end;

function TZMySQLBaseDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_field_seek(Res, Offset);
end;

function TZMySQLBaseDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := MYSQL_API.mysql_row_seek(Res, Row);
end;

function TZMySQLBaseDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_select_db(Handle, Database);
end;

function TZMySQLBaseDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TMySQLOption; const Arg: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_options(Handle,TMySqlOption(Option), Arg);
end;

function TZMySQLBaseDriver.Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMySqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := MYSQL_API.mysql_shutdown(Handle,shutdown_level);
end;

function TZMySQLBaseDriver.SetAutocommit(Handle: PZMySQLConnect; mode: Boolean): Boolean;
var
    my_bool, my_mode: Byte;
begin
    if (mode = True) then
        my_mode := 1
    else
        my_mode := 0;
    my_bool := MYSQL_API.mysql_autocommit(PMYSQL(Handle), my_mode);
    Result := (my_bool = 0);
end;

function TZMySQLBaseDriver.Commit(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_commit(PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQLBaseDriver.CheckAnotherRowset(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool :=  MYSQL_API.mysql_more_results (PMYSQL(Handle));
    if (my_bool = 0) then
        Result := False
    else
        Result := True;
end;

function TZMySQLBaseDriver.RetrieveNextRowset(Handle: PZMySQLConnect): Integer;
begin
    Result := MYSQL_API.mysql_next_result (PMYSQL(Handle));
end;

function TZMySQLBaseDriver.Rollback (Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_rollback(PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQLBaseDriver.getSQLState (Handle: PZMySQLConnect): AnsiString;
begin
    Result := MYSQL_API.mysql_sqlstate (PMYSQL(Handle));
end;

function TZMySQLBaseDriver.GetPreparedAffectedRows(Handle: PZMySqlPrepStmt): Int64;
begin
    Result :=  MYSQL_API.mysql_stmt_affected_rows (PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.BindParameters(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
begin
    Result := MYSQL_API.mysql_stmt_bind_param (PMYSQL_STMT(Handle), PMYSQL_BIND50(bindArray));
end;

function TZMySQLBaseDriver.BindResult(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
begin
    Result := MYSQL_API.mysql_stmt_bind_result (PMYSQL_STMT(Handle), PMYSQL_BIND50(bindArray));
end;

function TZMySQLBaseDriver.ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_stmt_close(PMYSQL_STMT(PrepStmtHandle));
    if (my_bool = 0) then
        Result := nil
    else
        Result := PrepStmtHandle;
end;

procedure TZMySQLBaseDriver.SeekPreparedData(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
begin
  MYSQL_API.mysql_stmt_data_seek(PMYSQL_STMT(PrepStmtHandle), Offset);
end;

function TZMySQLBaseDriver.GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt):Integer;
begin
    Result := MYSQL_API.mysql_stmt_errno(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.GetLastPreparedError(Handle: PZMySqlPrepStmt):AnsiString;
begin
    Result := MYSQL_API.mysql_stmt_error(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.ExecuteStmt(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_execute (PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.FetchBoundResults(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_fetch (PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_field_count(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
begin
    Result := MYSQL_API.mysql_stmt_init(PMYSQL(Handle));
end;

function TZMySQLBaseDriver.GetPreparedInsertID(Handle: PZMySqlPrepStmt): Int64;
begin
    Result := MYSQL_API.mysql_stmt_insert_id (PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.GetPreparedNumRows(Handle: PZMySqlPrepStmt): Int64;
begin
    if (Handle = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_stmt_num_rows (PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal;
begin
    Result := MYSQL_API.mysql_stmt_param_count (PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
begin
    Result := MYSQL_API.mysql_stmt_prepare(PMYSQL_STMT(PrepStmtHandle), Query, Length);
end;

function TZMySQLBaseDriver.GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
begin
    Result := MYSQL_API.mysql_stmt_result_metadata (PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.SeekPreparedRow(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
    Result := MYSQL_API.mysql_stmt_row_seek (PMYSQL_STMT(Handle), Row);
end;

function TZMySQLBaseDriver.GetPreparedSQLState(Handle: PZMySqlPrepStmt): PAnsiChar;
begin
    Result := MYSQL_API.mysql_stmt_sqlstate (PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_store_result (PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_store_result(Handle);
end;

function TZMySQLBaseDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_use_result(Handle);
end;

function TZMySQLBaseDriver.GetLastError(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_error(Handle);
end;

function TZMySQLBaseDriver.GetFieldType(Field: PZMySQLField): TMysqlFieldTypes;
begin
  Result := PMYSQL_FIELD(Field)^._type;
end;

function TZMySQLBaseDriver.GetFieldFlags(Field: PZMySQLField): Integer;
begin
  Result := PMYSQL_FIELD(Field)^.flags;
end;

function TZMySQLBaseDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := MYSQL_API.mysql_num_rows(Res);
end;

function TZMySQLBaseDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
 result := MYSQL_API.mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

function TZMySQLBaseDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := MYSQL_API.mysql_num_fields(Res);
end;

function TZMySQLBaseDriver.GetFieldDecimals(Field: PZMySQLField): Integer;
begin
  Result := PMYSQL_FIELD(Field)^.decimals;
end;

function TZMySQLBaseDriver.GetFieldLength(Field: PZMySQLField): ULong;
begin
    Result := PMYSQL_FIELD(Field)^.length;
end;

function TZMySQLBaseDriver.GetFieldMaxLength(Field: PZMySQLField): Integer;
begin
  Result := PMYSQL_FIELD(Field)^.max_length;
end;

function TZMySQLBaseDriver.GetFieldName(Field: PZMySQLField): PAnsiChar;
begin
  Result := PMYSQL_FIELD(Field)^.name;
end;

function TZMySQLBaseDriver.GetFieldTable(Field: PZMySQLField): PAnsiChar;
begin
  Result := PMYSQL_FIELD(Field)^.table;
end;

function TZMySQLBaseDriver.GetFieldOrigTable(Field: PZMySQLField): PAnsiChar;
begin
  Result := PMYSQL_FIELD(Field)^.org_table;
end;

function TZMySQLBaseDriver.GetFieldOrigName(Field: PZMySQLField): PAnsiChar;
begin
  Result := PMYSQL_FIELD(Field)^.org_name;
end;

function TZMySQLBaseDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PAnsiChar;
begin
  Result := PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQLBaseDriver.GetLastErrorCode(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_errno(PMYSQL(Handle));
end;

function TZMySQLBaseDriver.GetClientVersion: Integer;
begin
 Result := MYSQL_API.mysql_get_client_version;
end;

function TZMySQLBaseDriver.GetServerVersion(
  Handle: PZMySQLConnect): Integer;
begin
 Result := MYSQL_API.mysql_get_server_version(Handle);
end;

procedure TZMySQLBaseDriver.SetDriverOptions(Options: TStrings);
var
  PreferedLibrary: String;

begin
  PreferedLibrary := Options.Values['Library'];
  if PreferedLibrary <> '' then
    Loader.AddLocation(PreferedLibrary);
  if IsEmbeddedDriver then
    BuildServerArguments(Options);
end;

{ TZMySQL41PlainDriver }

constructor TZMySQL41PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL41_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL41_LOCATION);
  {$ENDIF}
end;

function TZMySQL41PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-4.1';
end;

function TZMySQL41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 4.1+';
end;

{ TZMySQLD41PlainDriver }

constructor TZMySQLD41PlainDriver.Create;
begin
  inherited Create;
  // only include embedded library
  FLoader.ClearLocations;
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION_EMBEDDED);
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL41_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL41_LOCATION_EMBEDDED);
  {$ENDIF}
  IsEmbeddedDriver := True;
end;

function TZMySQLD41PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-4.1';
end;

function TZMySQLD41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 4.1+';
end;

{ TZMySQL5PlainDriver }

procedure TZMySQL5PlainDriver.LoadApi;
begin
  inherited LoadApi;

  with Loader do
  begin
  @MYSQL_API.mysql_get_character_set_info := GetAddress('mysql_get_character_set_info');
  @MYSQL_API.mysql_stmt_next_result       := GetAddress('mysql_stmt_next_result');
  end;
end;

constructor TZMySQL5PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL50_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL51_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL55_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL50_LOCATION);
    FLoader.AddLocation(LINUX_DLL51_LOCATION);
  {$ENDIF}
end;

function TZMySQL5PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-5';
end;

function TZMySQL5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 5.0+';
end;

{ TZMySQLD5PlainDriver }

constructor TZMySQLD5PlainDriver.Create;
begin
  inherited Create;
  // only include embedded library
  FLoader.ClearLocations;
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION_EMBEDDED);
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL50_LOCATION_EMBEDDED);
    FLoader.AddLocation(WINDOWS_DLL51_LOCATION_EMBEDDED);
    FLoader.AddLocation(WINDOWS_DLL55_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL50_LOCATION_EMBEDDED);
    FLoader.AddLocation(LINUX_DLL51_LOCATION_EMBEDDED);
  {$ENDIF}
  IsEmbeddedDriver := True;
end;

function TZMySQLD5PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-5';
end;

function TZMySQLD5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 5+';
end;

end.


