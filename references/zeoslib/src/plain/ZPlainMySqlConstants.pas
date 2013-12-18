{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi plain interface to libmysql.dll           }
{                     Version 4.1                         }
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

unit ZPlainMySqlConstants;

interface

{$I ZPlain.inc}

uses
   ZCompatibility;

const
{ General Declarations }
  MYSQL_ERRMSG_SIZE    = 512;
  SQLSTATE_LENGTH      = 5;

  MYSQL_PORT           = 3306;
  LOCAL_HOST           = 'localhost';

{ Enum Field Types }
{  FIELD_TYPE_DECIMAL   = 0;
  FIELD_TYPE_TINY      = 1;
  FIELD_TYPE_SHORT     = 2;
  FIELD_TYPE_LONG      = 3;
  FIELD_TYPE_FLOAT     = 4;
  FIELD_TYPE_DOUBLE    = 5;
  FIELD_TYPE_NULL      = 6;
  FIELD_TYPE_TIMESTAMP = 7;
  FIELD_TYPE_LONGLONG  = 8;
  FIELD_TYPE_INT24     = 9;
  FIELD_TYPE_DATE      = 10;
  FIELD_TYPE_TIME      = 11;
  FIELD_TYPE_DATETIME  = 12;
  FIELD_TYPE_YEAR      = 13;
  FIELD_TYPE_NEWDATE   = 14;
  FIELD_TYPE_VARCHAR   = 15; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_BIT       = 16; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_NEWDECIMAL = 246; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_ENUM      = 247;
  FIELD_TYPE_SET       = 248;
  FIELD_TYPE_TINY_BLOB = 249;
  FIELD_TYPE_MEDIUM_BLOB = 250;
  FIELD_TYPE_LONG_BLOB = 251;
  FIELD_TYPE_BLOB      = 252;
  FIELD_TYPE_VAR_STRING = 253;
  FIELD_TYPE_STRING    = 254;
  FIELD_TYPE_GEOMETRY  = 255; }
{ For Compatibility }
{  FIELD_TYPE_CHAR      = FIELD_TYPE_TINY;
  FIELD_TYPE_INTERVAL  = FIELD_TYPE_ENUM; }

  { Field's flags }
  NOT_NULL_FLAG          = 1;     { Field can't be NULL }
  PRI_KEY_FLAG           = 2;     { Field is part of a primary key }
  UNIQUE_KEY_FLAG        = 4;     { Field is part of a unique key }
  MULTIPLE_KEY_FLAG      = 8;     { Field is part of a key }
  BLOB_FLAG              = 16;    { Field is a blob }
  UNSIGNED_FLAG          = 32;    { Field is unsigned }
  ZEROFILL_FLAG          = 64;    { Field is zerofill }
  BINARY_FLAG            = 128;   { Field is binary }
  ENUM_FLAG              = 256;   { Field is an enum }
  AUTO_INCREMENT_FLAG    = 512;   { Field is a autoincrement field }
  TIMESTAMP_FLAG         = 1024;  { Field is a timestamp }
  SET_FLAG               = 2048;  { Field is a set }
  NUM_FLAG               = 32768; { Field is num (for clients) }
  PART_KEY_FLAG	         = 16384; { Intern; Part of some key }
  GROUP_FLAG	           = 32768; { Intern: Group field }
  UNIQUE_FLAG            = 65536; { Intern: Used by sql_yacc }
  BINCMP_FLAG            = $20000; { Intern: Used by sql_yacc }
  GET_FIXED_FIELDS_FLAG  = $40000; { Used to get fields in item tree }
  FIELD_IN_PART_FUNC_FLAG= $80000; { Field part of partition func }
  FIELD_IN_ADD_INDEX     = $100000; { Intern: Field used in ADD INDEX }
  FIELD_IS_RENAMED       = $200000; { Intern: Field is being renamed}

{ Client Connection Options }
  _CLIENT_LONG_PASSWORD	  = 1;	  { new more secure passwords }
  _CLIENT_FOUND_ROWS	  = 2;	  { Found instead of affected rows }
  _CLIENT_LONG_FLAG	  = 4;	  { Get all column flags }
  _CLIENT_CONNECT_WITH_DB = 8;	  { One can specify db on connect }
  _CLIENT_NO_SCHEMA	  = 16;	  { Don't allow database.table.column }
  _CLIENT_COMPRESS	  = 32;	  { Can use compression protcol }
  _CLIENT_ODBC		  = 64;	  { Odbc client }
  _CLIENT_LOCAL_FILES	  = 128;  { Can use LOAD DATA LOCAL }
  _CLIENT_IGNORE_SPACE	  = 256;  { Ignore spaces before '(' }
  _CLIENT_CHANGE_USER     = 512;  { Support the mysql_change_user() }
  _CLIENT_INTERACTIVE     = 1024; { This is an interactive client }
  _CLIENT_SSL             = 2048; { Switch to SSL after handshake }
  _CLIENT_IGNORE_SIGPIPE  = 4096; { IGNORE sigpipes }
  _CLIENT_TRANSACTIONS    = 8196; { Client knows about transactions }
  _CLIENT_RESERVED        = 16384; { Old flag for 4.1 protocol  }
  _CLIENT_SECURE_CONNECTION = 32768; { New 4.1 authentication }
  _CLIENT_MULTI_STATEMENTS = 65536; { Enable/disable multi-stmt support }
  _CLIENT_MULTI_RESULTS   = 131072; { Enable/disable multi-results }
  _CLIENT_PS_MULTI_RESULTS = 262144; { Enable Multi-results in PS-protocol }
  _CLIENT_PLUGIN_AUTH      = 524288;
  _CLIENT_SSL_VERIFY_SERVER_CERT = 1073741824;
  _CLIENT_REMEMBER_OPTIONS = 2147483648; {Enable/disable multi-results }

{THD: Killable}
  MYSQL_SHUTDOWN_KILLABLE_CONNECT    = 1;
  MYSQL_SHUTDOWN_KILLABLE_TRANS      = 2;
  MYSQL_SHUTDOWN_KILLABLE_LOCK_TABLE = 4;
  MYSQL_SHUTDOWN_KILLABLE_UPDATE     = 8;

{prepared fetch results}
  STMT_FETCH_OK         = 0;
  STMT_FETCH_ERROR      = 1;
  STMT_FETCH_NO_DATA    = 100;
  STMT_FETCH_DATA_TRUNC = 101;

type
  TMySqlOption = (
    MYSQL_OPT_CONNECT_TIMEOUT,
    MYSQL_OPT_COMPRESS,
    MYSQL_OPT_NAMED_PIPE,
    MYSQL_INIT_COMMAND,
    MYSQL_READ_DEFAULT_FILE,
    MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR,
    MYSQL_SET_CHARSET_NAME,
    MYSQL_OPT_LOCAL_INFILE,
    MYSQL_OPT_PROTOCOL,
    MYSQL_SHARED_MEMORY_BASE_NAME,
    MYSQL_OPT_READ_TIMEOUT,
    MYSQL_OPT_WRITE_TIMEOUT,
    MYSQL_OPT_USE_RESULT,
    MYSQL_OPT_USE_REMOTE_CONNECTION,
    MYSQL_OPT_USE_EMBEDDED_CONNECTION,
    MYSQL_OPT_GUESS_CONNECTION,
    MYSQL_SET_CLIENT_IP,
    MYSQL_SECURE_AUTH,
    MYSQL_REPORT_DATA_TRUNCATION, 
    MYSQL_OPT_RECONNECT,
    MYSQL_OPT_SSL_VERIFY_SERVER_CERT,
    MYSQL_PLUGIN_DIR,
    MYSQL_DEFAULT_AUTH
  );

  PUSED_MEM=^USED_MEM;
  USED_MEM = packed record
    next:       PUSED_MEM;
    left:       Integer;
    size:       Integer;
  end;

  PERR_PROC = ^ERR_PROC;
  ERR_PROC = procedure;

  PMEM_ROOT = ^MEM_ROOT;
  MEM_ROOT = packed record
    free:          PUSED_MEM;
    used:          PUSED_MEM;
    pre_alloc:     PUSED_MEM;
    min_malloc:    Integer;
    block_size:    Integer;
    block_num:     Integer;
    first_block_usage: Integer;
    error_handler: PERR_PROC;
  end;

  MYSQL_ROW = array[00..$ff] of PAnsiChar;
  PMYSQL_ROW = ^MYSQL_ROW;

  PMYSQL_ROWS = ^MYSQL_ROWS;
  MYSQL_ROWS = record
    next:       PMYSQL_ROWS;
    data:       PMYSQL_ROW;
  end;

  MYSQL_ROW_OFFSET = PMYSQL_ROWS;

  MYSQL_DATA = record
    Rows:       Int64;
    Fields:     Cardinal;
    Data:       PMYSQL_ROWS;
    Alloc:      MEM_ROOT;
  end;
  PMYSQL_DATA = ^MYSQL_DATA;

  MYSQL_FIELD_OFFSET = UInt;

  PMYSQL_OPTIONS = ^_MYSQL_OPTIONS;
  _MYSQL_OPTIONS = record
    connect_timeout:          UInt;
    read_timeout:             UInt;
    write_timeout:            UInt;
    port:                     UInt;
    protocol:                 UInt;
    client_flag:              ULong;
    host:                     PAnsiChar;
    user:                     PAnsiChar;
    password:                 PAnsiChar;
    unix_socket:              PAnsiChar;
    db:                       PAnsiChar;
    init_commands:            Pointer;
    my_cnf_file:              PAnsiChar;
    my_cnf_group:             PAnsiChar;
    charset_dir:              PAnsiChar;
    charset_name:             PAnsiChar;
    ssl_key:                  PAnsiChar;
    ssl_cert:                 PAnsiChar;
    ssl_ca:                   PAnsiChar;
    ssl_capath:               PAnsiChar;
    ssl_cipher:               PAnsiChar;
    shared_memory_base_name:  PAnsiChar;
    max_allowed_packet:       ULong;
    use_ssl:                  Byte;
    compress:                 Byte;
    named_pipe:               Byte;
    unused1:                  Byte;
    unused2:                  Byte;
    unused3:                  Byte;
    unused4:                  Byte;
    methods_to_use:           TMySqlOption;
    client_ip:                PAnsiChar;
    secure_auth:              Byte;
    local_infile_init:        Pointer;
    local_infile_read:        Pointer;
    local_infile_end:         Pointer;
    local_infile_error:       Pointer;
    local_infile_userdata:    Pointer;
  end;

    PZMySQLConnect = Pointer;
    PZMySQLResult = Pointer;
    PZMySQLRow = Pointer;
    PZMySQLField = Pointer;
    PZMySQLRowOffset = Pointer;
    PZMySqlPrepStmt = Pointer;
    PZMysqlBindArray = Pointer;

{ Enum Field Types }
      TMysqlFieldTypes = (
  FIELD_TYPE_DECIMAL   = 0,
  FIELD_TYPE_TINY      = 1,
  FIELD_TYPE_SHORT     = 2,
  FIELD_TYPE_LONG      = 3,
  FIELD_TYPE_FLOAT     = 4,
  FIELD_TYPE_DOUBLE    = 5,
  FIELD_TYPE_NULL      = 6,
  FIELD_TYPE_TIMESTAMP = 7,
  FIELD_TYPE_LONGLONG  = 8,
  FIELD_TYPE_INT24     = 9,
  FIELD_TYPE_DATE      = 10,
  FIELD_TYPE_TIME      = 11,
  FIELD_TYPE_DATETIME  = 12,
  FIELD_TYPE_YEAR      = 13,
  FIELD_TYPE_NEWDATE   = 14,
  FIELD_TYPE_VARCHAR   = 15, //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_BIT       = 16, //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_NEWDECIMAL = 246, //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_ENUM      = 247,
  FIELD_TYPE_SET       = 248,
  FIELD_TYPE_TINY_BLOB = 249,
  FIELD_TYPE_MEDIUM_BLOB = 250,
  FIELD_TYPE_LONG_BLOB = 251,
  FIELD_TYPE_BLOB      = 252,
  FIELD_TYPE_VAR_STRING = 253,
  FIELD_TYPE_STRING    = 254,
  FIELD_TYPE_GEOMETRY  = 255
    );

  { Options for mysql_set_option }
  TMySqlSetOption = (
    MYSQL_OPTION_MULTI_STATEMENTS_ON,
    MYSQL_OPTION_MULTI_STATEMENTS_OFF
  );

  TMysqlStmtAttrType = (
    STMT_ATTR_UPDATE_MAX_LENGTH,
    STMT_ATTR_CURSOR_TYPE,
    STMT_ATTR_PREFETCH_ROWS
  );

  TMysqlShutdownLevel = (
    SHUTDOWN_DEFAULT = 0,
    SHUTDOWN_WAIT_CONNECTIONS = MYSQL_SHUTDOWN_KILLABLE_CONNECT,
    SHUTDOWN_WAIT_TRANSACTIONS = MYSQL_SHUTDOWN_KILLABLE_TRANS,
    SHUTDOWN_WAIT_UPDATES = MYSQL_SHUTDOWN_KILLABLE_UPDATE,
    SHUTDOWN_WAIT_ALL_BUFFERS = (MYSQL_SHUTDOWN_KILLABLE_UPDATE shl 1),
    SHUTDOWN_WAIT_CRITICAL_BUFFERS,
    KILL_QUERY = 254,
    KILL_CONNECTION = 255
  );

TMYSQL_CLIENT_OPTIONS =
( CLIENT_LONG_PASSWORD,	{  = 1;	   new more secure passwords }
  CLIENT_FOUND_ROWS ,	{	  = 2;	   Found instead of affected rows }
  CLIENT_LONG_FLAG	 ,	{ = 4;	   Get all column flags }
  CLIENT_CONNECT_WITH_DB ,	{ = 8;	   One can specify db on connect }
  CLIENT_NO_SCHEMA	 ,	{  = 16;	   Don't allow database.table.column }
  CLIENT_COMPRESS	 ,	{  = 32;	   Can use compression protcol }
  CLIENT_ODBC		 ,	{  = 64;	   Odbc client }
  CLIENT_LOCAL_FILES	  ,	{ = 128;   Can use LOAD DATA LOCAL }
  CLIENT_IGNORE_SPACE	 ,	{  = 256;   Ignore spaces before '(' }
  CLIENT_CHANGE_USER    ,	{  = 512;   Support the mysql_change_user() }
  CLIENT_INTERACTIVE    ,	{  = 1024;  This is an interactive client }
  CLIENT_SSL     ,	{         = 2048;  Switch to SSL after handshake }
  CLIENT_IGNORE_SIGPIPE  ,	{ = 4096;  IGNORE sigpipes }
  CLIENT_TRANSACTIONS    ,	{ = 8196;  Client knows about transactions }
  CLIENT_RESERVED     ,	{    = 16384;  Old flag for 4.1 protocol  }
  CLIENT_SECURE_CONNECTION  ,	{= 32768;  New 4.1 authentication }
  CLIENT_MULTI_STATEMENTS  ,	{= 65536;  Enable/disable multi-stmt support }
  CLIENT_MULTI_RESULTS  ,	{  = 131072;  Enable/disable multi-results }
  CLIENT_PS_MULTI_RESULTS,  {2^18 = 262144; Enable Multi-results in PS-protocol}
  CLIENT_PLUGIN_AUTH,{2^19 = 524288}
  CLIENT_OPT_20,  {2^20 = 1048576}
  CLIENT_OPT_21,   {2^21 = 2097152 }
  CLIENT_OPT_22,  {2^22 = 4194304}
  CLIENT_OPT_23,  {2^23 = 8388608 }
  CLIENT_OPT_24,   {2^24 = 16777216 }
  CLIENT_OPT_25,   {2^25 = 33554432}
  CLIENT_OPT_26,    {2^26 = 67108864}
  CLIENT_OPT_27,    {2^27 = 134217728}
  CLIENT_OPT_28,    {2^28 = 268435456}
  CLIENT_OPT_29,    {2^29 = 536870912}
  CLIENT_SSL_VERIFY_SERVER_CERT,    {2^30 = 1073741824}
  CLIENT_REMEMBER_OPTIONS	{ = 2147483648; Enable/disable multi-results });

  TMysqlStmtState = (
    MYSQL_STMT_INIT_DONE = 1,
    MYSQL_STMT_PREPARE_DONE,
    MYSQL_STMT_EXECUTE_DONE,
    MYSQL_STMT_FETCH_DONE
  );

  MYSQL_TIME = record
    year:                UInt;
    month:               UInt;
    day:                 UInt;
    hour:                UInt;
    minute:              UInt;
    second:              UInt;
    second_part:         ULong;
    neg:                 Byte;
  end;
  PMYSQL_TIME = ^MYSQL_TIME;

  PLIST = ^LIST;
  LIST = record
    prev:       PLIST;
    next:       PLIST;
    data:       Pointer;
  end;

  PMYSQL_FIELD = ^MYSQL_FIELD;
  MYSQL_FIELD = record
    name:             PAnsiChar;   // Name of column
    org_name:         PAnsiChar;   // Original column name, if an alias
    table:            PAnsiChar;   // Table of column if column was a field
    org_table:        PAnsiChar;   // Org table name if table was an alias
    db:               PAnsiChar;   // Database for table
    catalog:	       PAnsiChar;   // Catalog for table
    def:              PAnsiChar;   // Default value (set by mysql_list_fields)
    length:           ULong; // Width of column
    max_length:       ULong; // Max width of selected set
    name_length:      UInt;
    org_name_length:  UInt;
    table_length:     UInt;
    org_table_length: UInt;
    db_length:        UInt;
    catalog_length:   UInt;
    def_length:       UInt;
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
    charsetnr:        UInt; // Character set
    _type:            TMysqlFieldTypes; // Type of field. Se mysql_com.h for types
  end;

  PMYSQL_BIND41 = ^MYSQL_BIND41;
  MYSQL_BIND41 =  record
    // 4.1.22 definition
    length:           PULong;
    is_null:          PByte;
    buffer:           PAnsiChar;
    buffer_type:      TMysqlFieldTypes;
    buffer_length:    ULong;
    //internal fields
    inter_buffer:     PByte;
    offset:           ULong;
    internal_length:  ULong;
    param_number:     UInt;
    pack_length:      UInt;
    is_unsigned:      Byte;
    long_data_used:   Byte;
    internal_is_null: Byte;
    store_param_func: Pointer;
    fetch_result:     Pointer;
    skip_result:      Pointer;
  end;

  PMYSQL_BIND50 = ^MYSQL_BIND50;
  MYSQL_BIND50 =  record
    // 5.0.67 definition
    length:            PULong;
    is_null:           PByte;
    buffer:            PAnsiChar;
    error:             PByte;
    buffer_type:       TMysqlFieldTypes;
    buffer_length:     ULong;
    row_ptr:           PByte;
    offset:            ULong;
    length_value:      ULong;
    param_number:      UInt;
    pack_length:       UInt;
    error_value:       Byte;
    is_unsigned:       Byte;
    long_data_used:    Byte;
    is_null_value:     Byte;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
  end;

  PMYSQL_BIND51 = ^MYSQL_BIND51;
  MYSQL_BIND51 =  record
    // 5.1.30 definition (Still valid for 5.5.8)
    length:            PULong;
    is_null:           PByte;
    buffer:            PAnsiChar;
    error:             PByte;
    row_ptr:           PByte;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
    buffer_length:     ULong;
    offset:            ULong;
    length_value:      ULong;
    param_number:      UInt;
    pack_length:       UInt;
    buffer_type:       TMysqlFieldTypes;
    error_value:       Byte;
    is_unsigned:       Byte;
    long_data_used:    Byte;
    is_null_value:     Byte;
    extension:         Pointer;
  end;

  PMYSQL_BIND60 = ^MYSQL_BIND60;
  MYSQL_BIND60 =  record
    // 6.0.8 definition
    length:            PULong;
    is_null:           PByte;
    buffer:            PAnsiChar;
    error:             PByte;
    row_ptr:           PByte;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
    buffer_length:     ULong;
    offset:            ULong;
    length_value:      ULong;
    param_number:      UInt;
    pack_length:       UInt;
    buffer_type:       TMysqlFieldTypes;
    error_value:       Byte;
    is_unsigned:       Byte;
    long_data_used:    Byte;
    is_null_value:     Byte;
    extension:         Pointer;
  end;

  PDOBindRecord2 = record
      buffer:    Array of Byte;
      length:    ULong;
      is_null:   Byte;
  end;

  PMYSQL = ^MYSQL;

  MYSQL  = pointer;

  PMY_CHARSET_INFO = ^MY_CHARSET_INFO;
  MY_CHARSET_INFO = record
    number:         UInt;
    state:          UInt;
    csname:         PAnsiChar;
    name:           PAnsiChar;
    comment:        PAnsiChar;
    dir:            PAnsiChar;
    mbminlen:       UInt;
    mbmaxlen:       UInt;
  end;
  // Structure of the MYSQL_RES record isn't used anymore.
  // Access to the fields should be done using library functions
  // Reason : the structure of these records tend to change now and then.
  PMYSQL_RES = Pointer;

  PREP_STMT_STATE=(
    MY_ST_UNKNOWN,
    MY_ST_PREPARE,
    MY_ST_EXECUTE);

  PMYSQL_STMT = Pointer;

{ ****************** Plain API Types definition ***************** }

type

{ ************** Plain API Function types definition ************* }

  { Functions to get information from the MYSQL and MYSQL_RES structures
    Should definitely be used if one uses shared libraries. }
  Tmysql_affected_rows          = function(Handle: PMYSQL): ULongLong;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_character_set_name     = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_close                  = procedure(Handle: PMYSQL);                                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_connect                = function(Handle: PMYSQL; const Host, User, Passwd: PAnsiChar): PMYSQL;   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_create_db              = function(Handle: PMYSQL; const Db: PAnsiChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_data_seek              = procedure(Result: PMYSQL_RES; Offset: ULongLong);                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_debug                  = procedure(Debug: PAnsiChar);                                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_drop_db                = function(Handle: PMYSQL; const Db: PAnsiChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_dump_debug_info        = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_eof                    = function(Result: PMYSQL_RES): Byte;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_errno                  = function(Handle: PMYSQL): UInt;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_error                  = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_escape_string          = function(PTo, PFrom: PAnsiChar; Len: ULong): ULong;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_field            = function(Result: PMYSQL_RES): PMYSQL_FIELD;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_field_direct     = function(Result: PMYSQL_RES; FieldNo: UInt): PMYSQL_FIELD;       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_fields           = function(Result: PMYSQL_RES): PMYSQL_FIELD;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_lengths          = function(Result: PMYSQL_RES): PULong;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_row              = function(Result: PMYSQL_RES): PMYSQL_ROW;                            {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_field_seek             = function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_field_tell             = function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_free_result            = procedure(Result: PMYSQL_RES);                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_client_info        = function: PAnsiChar;                                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_host_info          = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_proto_info         = function(Handle: PMYSQL): UInt;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_server_info        = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_info                   = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_init                   = function(Handle: PMYSQL): PMYSQL;                                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_insert_id              = function(Handle: PMYSQL): ULongLong;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_kill                   = function(Handle: PMYSQL; Pid: ULong): Integer;                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_dbs               = function(Handle: PMYSQL; Wild: PAnsiChar): PMYSQL_RES;                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_fields            = function(Handle: PMYSQL; const Table, Wild: PAnsiChar): PMYSQL_RES;      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_processes         = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_tables            = function(Handle: PMYSQL; const Wild: PAnsiChar): PMYSQL_RES;             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_num_fields             = function(Result: PMYSQL_RES): UInt;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_num_rows               = function(Result: PMYSQL_RES): ULongLong;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_options                = function(Handle: PMYSQL; Option: TMySqlOption; const Arg: PAnsiChar): Integer;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_ping                   = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_query                  = function(Handle: PMYSQL; const Query: PAnsiChar): Integer;               {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_connect           = function(Handle: PMYSQL; const Host, User, Passwd, Db: PAnsiChar;
                                           Port: UInt; const UnixSocket: PAnsiChar; ClientFlag: ULong): PMYSQL;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_escape_string     = function(Handle: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: ULong): ULong;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_query             = function(Handle: PMYSQL; const Query: PAnsiChar; Length: ULong): Integer;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_refresh                = function(Handle: PMYSQL; Options: UInt): Integer;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_row_seek               = function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS;      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_row_tell               = function(Result: PMYSQL_RES): PMYSQL_ROWS;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_select_db              = function(Handle: PMYSQL; const Db: PAnsiChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_ssl_set                = function(Handle: PMYSQL; const key, cert, CA, CApath, cipher:
                                  PAnsiChar): Byte;                                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stat                   = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_store_result           = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_id              = function(Handle: PMYSQL): ULong;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_use_result             = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Set up and bring down a thread; these function should be called for each thread in an application which
    opens at least one MySQL connection.  All uses of the connection(s) should be between these function calls. }
  Tmy_init                      = procedure;                                                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_init            = function: Byte;                                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_end             = procedure;                                                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_safe            = function: UInt;                                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Set up and bring down the server; to ensure that applications will work when linked against either the
    standard client library or the embedded server library, these functions should be called. }
  Tmysql_server_init            = function(Argc: Integer; Argv, Groups: Pointer): Integer;            {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_server_end             = procedure;                                                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_change_user            = function(mysql: PMYSQL; const user: PAnsiChar; const passwd: PAnsiChar; const db: PAnsiChar): Byte;
  Tmysql_field_count            = function(Handle: PMYSQL): UInt;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};


  Tmysql_get_client_version     = function: ULong;                                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_send_query = function(mysql: PMYSQL; const query: PAnsiChar;
    length: ULong): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_read_query_result = function(mysql: PMYSQL): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};


  Tmysql_autocommit             = function(Handle: PMYSQL; const mode: Byte): Byte;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_commit                 = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_server_version     = function(Handle: PMYSQL): ULong;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_hex_string             = function(PTo, PFrom: PAnsiChar; Len: ULong): ULong;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_more_results           = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_next_result            = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_rollback               = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_set_character_set      = function(Handle: PMYSQL; csname: PAnsiChar): Integer;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_set_server_option      = function(Handle: PMYSQL; Option: TMysqlSetOption): Integer;          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_shutdown               = function(Handle: PMYSQL; shutdown_level: TMysqlShutdownLevel): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_sqlstate               = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_warning_count          = function(Handle: PMYSQL): UInt;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
{BELOW are new PREPARED STATEMENTS}
  Tmysql_stmt_affected_rows     = function(stmt: PMYSQL_STMT): ULongLong;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_attr_get          = function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
                                  arg: PAnsiChar): Byte;                                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_attr_set          = function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
                                  const arg: PAnsiChar): Byte;                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_bind_param        = function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte;              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_bind_result       = function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte;              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_close             = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_data_seek         = procedure(stmt: PMYSQL_STMT; offset: ULongLong);                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_errno             = function(stmt: PMYSQL_STMT): UInt;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_error             = function(stmt: PMYSQL_STMT): PAnsiChar;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_execute           = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_fetch             = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_fetch_column      = function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}; column: UInt;
                                  offset: ULong): Integer;                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_field_count       = function(stmt: PMYSQL_STMT): UInt;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_free_result       = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_init              = function(Handle: PMYSQL): PMYSQL_STMT;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_insert_id         = function(stmt: PMYSQL_STMT): ULongLong;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_next_result       = function(stmt: PMYSQL_STMT): Integer;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_num_rows          = function(stmt: PMYSQL_STMT): ULongLong;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_param_count       = function(stmt: PMYSQL_STMT): ULong;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_param_metadata    = function(stmt: PMYSQL_STMT): PMYSQL_RES;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_prepare           = function(stmt: PMYSQL_STMT; const query: PAnsiChar; length: ULong):
                                  Integer;                                                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_reset             = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_result_metadata   = function(stmt: PMYSQL_STMT): PMYSQL_RES;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_row_seek          = function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS;     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_row_tell          = function(stmt: PMYSQL_STMT): PMYSQL_ROWS;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_send_long_data    = function(stmt: PMYSQL_STMT; parameter_number: UInt; const
                                  data: PAnsiChar; length: ULong): Byte;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_sqlstate          = function(stmt: PMYSQL_STMT): PAnsiChar;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_store_result      = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_get_character_set_info = procedure(Handle: PMYSQL; cs: PMY_CHARSET_INFO);                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{ ************** Collection of Plain API Function types definition ************* }
TZMYSQL_API = record
  mysql_affected_rows:          Tmysql_affected_rows;	          {mysql 3.2}
  mysql_character_set_name:     Tmysql_character_set_name;      {mysql 3.2}
  mysql_close:                  Tmysql_close;                   {mysql 3.2}
  mysql_connect:                Tmysql_connect;                 {mysql 3.2} {deprecated for mysql_real_connect}
  mysql_create_db:              Tmysql_create_db;               {mysql 3.2} {deprecated for mysql_query}
  mysql_data_seek:              Tmysql_data_seek;               {mysql 3.2}
  mysql_debug:                  Tmysql_debug;                   {mysql 3.2}
  mysql_drop_db:                Tmysql_drop_db;                 {mysql 3.2} {deprecated for mysql_query}
  mysql_dump_debug_info:        Tmysql_dump_debug_info;         {mysql 3.2}
  mysql_eof:                    Tmysql_eof;                     {mysql 3.2} {deprecated for mysql_error/mysql_errno}
  mysql_errno:                  Tmysql_errno;	                {mysql 3.2}
  mysql_error:                  Tmysql_error;                   {mysql 3.2}
  mysql_escape_string:          Tmysql_escape_string;           {mysql 3.2} {deprecated for mysql_real_escape_string}
  mysql_fetch_field:            Tmysql_fetch_field;             {mysql 3.2}
  mysql_fetch_field_direct:     Tmysql_fetch_field_direct;      {mysql 3.2}
  mysql_fetch_fields:           Tmysql_fetch_fields;            {mysql 3.2}
  mysql_fetch_lengths:          Tmysql_fetch_lengths;           {mysql 3.2}
  mysql_fetch_row:              Tmysql_fetch_row;               {mysql 3.2}
  mysql_field_seek:             Tmysql_field_seek;              {mysql 3.2}
  mysql_field_tell:             Tmysql_field_tell;              {mysql 3.2}
  mysql_free_result:            Tmysql_free_result;             {mysql 3.2}
  mysql_get_client_info:        Tmysql_get_client_info;         {mysql 3.2}
  mysql_get_host_info:          Tmysql_get_host_info;           {mysql 3.2}
  mysql_get_proto_info:         Tmysql_get_proto_info;          {mysql 3.2}
  mysql_get_server_info:        Tmysql_get_server_info;         {mysql 3.2}
  mysql_info:                   Tmysql_info;                    {mysql 3.2}
  mysql_init:                   Tmysql_init;                    {mysql 3.2}
  mysql_insert_id:              Tmysql_insert_id;               {mysql 3.2}
  mysql_kill:                   Tmysql_kill;                    {mysql 3.2}
  mysql_list_dbs:               Tmysql_list_dbs;                {mysql 3.2}
  mysql_list_fields:            Tmysql_list_fields;             {mysql 3.2}
  mysql_list_processes:         Tmysql_list_processes;          {mysql 3.2}
  mysql_list_tables:            Tmysql_list_tables;             {mysql 3.2}
  mysql_num_fields:             Tmysql_num_fields;              {mysql 3.2}
  mysql_num_rows:               Tmysql_num_rows;                {mysql 3.2}
  mysql_options:                Tmysql_options;                 {mysql 3.2}
  mysql_ping:                   Tmysql_ping;	                   {mysql 3.2}
  mysql_query:                  Tmysql_query;                   {mysql 3.2} {deprecated for mysql_real_query}
  mysql_real_connect:           Tmysql_real_connect;            {mysql 3.2}
  mysql_real_escape_string:     Tmysql_real_escape_string;      {mysql 3.2}
  mysql_real_query:             Tmysql_real_query;              {mysql 3.2}
  mysql_refresh:                Tmysql_refresh;                 {mysql 3.2}
  mysql_row_seek:               Tmysql_row_seek;                {mysql 3.2}
  mysql_row_tell:               Tmysql_row_tell;                {mysql 3.2}
  mysql_select_db:              Tmysql_select_db;               {mysql 3.2}
  mysql_shutdown:               Tmysql_shutdown;                {mysql 3.2} {new argument 4.1}
  mysql_ssl_set:                Tmysql_ssl_set;                 {mysql 3.2}
  mysql_stat:                   Tmysql_stat;                    {mysql 3.2}
  mysql_store_result:           Tmysql_store_result;            {mysql 3.2}
  mysql_thread_id:              Tmysql_thread_id;               {mysql 3.2}
  mysql_use_result:             Tmysql_use_result;              {mysql 3.2}

  {API for THREADED FUNCTIONS }
  my_init:                      Tmy_init;                       {mysql 3.2}
  mysql_thread_init:            Tmysql_thread_init;             {mysql 3.2}
  mysql_thread_end:             Tmysql_thread_end;              {mysql 3.2}
  mysql_thread_safe:            tmysql_thread_safe;             {mysql 3.2}

  {API for EMBEDDED SERVER  }
  mysql_server_init:            Tmysql_server_init;             {mysql 3.2}
  mysql_server_end:             Tmysql_server_end;              {mysql 3.2}

  mysql_change_user:            Tmysql_change_user;             {mysql 3.23}
  mysql_field_count:            Tmysql_field_count;             {mysql 3.22}

  mysql_get_client_version:     Tmysql_get_client_version;      {mysql 4.0}

  mysql_send_query:     Tmysql_send_query;
  mysql_read_query_result: Tmysql_read_query_result;

  mysql_autocommit:             Tmysql_autocommit;              {mysql 4.1}
  mysql_commit:                 Tmysql_commit;                  {mysql 4.1}
  mysql_get_server_version:     Tmysql_get_server_version;      {mysql 4.1}
  mysql_hex_string:             Tmysql_hex_string;              {mysql 4.1.8}
  mysql_more_results:           Tmysql_more_results;            {mysql 4.1}
  mysql_next_result:            Tmysql_next_result;             {mysql 4.1}
  mysql_rollback:               Tmysql_rollback;                {mysql 4.1}
  mysql_set_character_set:      Tmysql_set_character_set;       {mysql 4.1.13}
  mysql_set_server_option:      Tmysql_set_server_option;       {mysql 4.1}
  mysql_sqlstate:               Tmysql_sqlstate;                {mysql 4.1}
  mysql_warning_count:          Tmysql_warning_count;           {mysql 4.1}
  {API for PREPARED STATEMENTS}
  mysql_stmt_affected_rows:     Tmysql_stmt_affected_rows;      {mysql 4.1.0}
  mysql_stmt_attr_get:          Tmysql_stmt_attr_get;           {mysql 4.1.2}
  mysql_stmt_attr_set:          Tmysql_stmt_attr_set;           {mysql 4.1.2} {augmented 5.0.2/6}
  mysql_stmt_bind_param:        Tmysql_stmt_bind_param;         {mysql 4.1.2}
  mysql_stmt_bind_result:       Tmysql_stmt_bind_result;        {mysql 4.1.2}
  mysql_stmt_close:             Tmysql_stmt_close;              {mysql 4.1.0}
  mysql_stmt_data_seek:         Tmysql_stmt_data_seek;          {mysql 4.1.1}
  mysql_stmt_errno:             Tmysql_stmt_errno;              {mysql 4.1.0}
  mysql_stmt_error:             Tmysql_stmt_error;              {mysql 4.1.0}
  mysql_stmt_execute:           Tmysql_stmt_execute;            {mysql 4.1.2}
  mysql_stmt_fetch:             Tmysql_stmt_fetch;              {mysql 4.1.2}
  mysql_stmt_fetch_column:      Tmysql_stmt_fetch_column;       {mysql 4.1.2}
  mysql_stmt_field_count:       Tmysql_stmt_field_count;        {mysql 4.1.3}
  mysql_stmt_free_result:       Tmysql_stmt_free_result;        {mysql 4.1.1}
  mysql_stmt_init:              Tmysql_stmt_init;               {mysql 4.1.2}
  mysql_stmt_insert_id:         Tmysql_stmt_insert_id;          {mysql 4.1.2}
  mysql_stmt_next_result:       Tmysql_stmt_next_result;        {mysql 5.5.3}
  mysql_stmt_num_rows:          Tmysql_stmt_num_rows;           {mysql 4.1.1}
  mysql_stmt_param_count:       Tmysql_stmt_param_count;        {mysql 4.1.2}
  mysql_stmt_param_metadata:    Tmysql_stmt_param_metadata;     {mysql 4.1.2}
  mysql_stmt_prepare:           Tmysql_stmt_prepare;            {mysql 4.1.2}
  mysql_stmt_reset:             Tmysql_stmt_reset;              {mysql 4.1.1}
  mysql_stmt_result_metadata:   Tmysql_stmt_result_metadata;    {mysql 4.1.2}
  mysql_stmt_row_seek:          Tmysql_stmt_row_seek;           {mysql 4.1.1}
  mysql_stmt_row_tell:          Tmysql_stmt_row_tell;           {mysql 4.1.1}
  mysql_stmt_send_long_data:    Tmysql_stmt_send_long_data;     {mysql 4.1.2}
  mysql_stmt_sqlstate:          Tmysql_stmt_sqlstate;           {mysql 4.1.1}
  mysql_stmt_store_result:      Tmysql_stmt_store_result;       {mysql 4.1.0}

  mysql_get_character_set_info: Tmysql_get_character_set_info;  {mysql 5.0.10}
end;

const
  EMBEDDED_DEFAULT_DATA_DIR = {$IFDEF WIN32}
                               '.\data\'
                              {$ELSE} './data/'
                              {$ENDIF};
  SERVER_ARGUMENTS_KEY_PREFIX = 'ServerArgument';
  SERVER_GROUPS : array [0..2] of PAnsiChar = ('embedded'#0, 'server'#0, nil);

  DEFAULT_PARAMS : array [0..2] of PAnsiChar = ('not_used'#0,
                                            '--datadir='+EMBEDDED_DEFAULT_DATA_DIR+#0,
                                            '--set-variable=key_buffer_size=32M'#0);

implementation


end.
