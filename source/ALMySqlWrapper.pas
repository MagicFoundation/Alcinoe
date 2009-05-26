{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Sergey Seroukhov (Zeos Database Objects)
              based on Mysql-direct library by Cristian Nicola
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALMySqlWrapper
Version:      3.51

Description:  MysQL libmysql.dll Version 5 API Interface Unit

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

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

History :

Link :        http://www.sourceforge.net/projects/zeoslib
              http://dev.mysql.com/doc/refman/5.0/en/c-api-functions.html

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALMySqlWrapper;

interface

uses windows,
     sysutils;

const

  {resource string}
  cALMySql_INVALIDEIBVERSION   = 'Incorrect Database Server version, check compiler options.';
  cALMySql_CANTLOADLIB         = 'Can''t load library: %s.';

{ General Declarations }
  MYSQL_ERRMSG_SIZE    = 512;
  SQLSTATE_LENGTH      = 5;
  SCRAMBLE_LENGTH      = 20;

  MYSQL_PORT           = 3306;
  LOCAL_HOST           = 'localhost';
  NAME_LEN             = 64;
  PROTOCOL_VERSION     = 10;
  FRM_VER              = 6;

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
  GROUP_FLAG	         = 32768; { Intern: Group field }
  UNIQUE_FLAG            = 65536; { Intern: Used by sql_yacc }
  BINCMP_FLAG            = $20000; { Intern: Used by sql_yacc }

{ Server Administration Refresh Options }
  REFRESH_GRANT	           = 1;     { Refresh grant tables }
  REFRESH_LOG		       = 2;     { Start on new log file }
  REFRESH_TABLES	       = 4;     { close all tables }
  REFRESH_HOSTS	           = 8;     { Flush host cache }
  REFRESH_STATUS           = 16;    { Flush status variables }
  REFRESH_THREADS          = 32;    { Flush status variables }
  REFRESH_SLAVE            = 64;    { Reset master info abd restat slave thread }
  REFRESH_MASTER           = 128;   { Remove all bin logs in the index and truncate the index }
  REFRESH_READ_LOCK        = 16384; { Lock tables for read }
  REFRESH_FAST		       = 32768; { Intern flag }
  REFRESH_QUERY_CACHE	   = 65536;
  REFRESH_QUERY_CACHE_FREE = $20000; { Pack query cache }
  REFRESH_DES_KEY_FILE	   = $40000;
  REFRESH_USER_RESOURCES   = $80000;

{ Client Connection Options }
  _CLIENT_LONG_PASSWORD	    = 1;	 { new more secure passwords }
  _CLIENT_FOUND_ROWS	    = 2;	 { Found instead of affected rows }
  _CLIENT_LONG_FLAG	        = 4;	 { Get all column flags }
  _CLIENT_CONNECT_WITH_DB   = 8;	 { One can specify db on connect }
  _CLIENT_NO_SCHEMA	        = 16;	 { Don't allow database.table.column }
  _CLIENT_COMPRESS	        = 32;	 { Can use compression protcol }
  _CLIENT_ODBC		        = 64;    { Odbc client }
  _CLIENT_LOCAL_FILES	    = 128;   { Can use LOAD DATA LOCAL }
  _CLIENT_IGNORE_SPACE	    = 256;   { Ignore spaces before '(' }
  _CLIENT_PROTOCOL_41	    = 512;   { New 4.1 protocol }
  _CLIENT_INTERACTIVE	    = 1024;  { This is an interactive client }
  _CLIENT_SSL               = 2048;  { Switch to SSL after handshake }
  _CLIENT_IGNORE_SIGPIPE    = $1000;    { IGNORE sigpipes }
  _CLIENT_TRANSACTIONS	    = $2000;    { Client knows about transactions }
  _CLIENT_RESERVED          = $4000;    { Old flag for 4.1 protocol  }
  _CLIENT_SECURE_CONNECTION = $8000;    { New 4.1 authentication }
  _CLIENT_MULTI_STATEMENTS  = $10000;   { Enable/disable multi-stmt support }
  _CLIENT_MULTI_RESULTS     = $20000;   { Enable/disable multi-results }
  _CLIENT_REMEMBER_OPTIONS  = $8000000; {Enable/disable multi-results }

  SERVER_STATUS_IN_TRANS          = 1;   {Transaction has started}
  SERVER_STATUS_AUTOCOMMIT        = 2;   {Server in Autocommit Mode}
  SERVER_STATUS_MORE_RESULTS      = 4;   {More results on server}
  SERVER_MORE_RESULTS_EXISTS      = 8;   {Multiple query, next query exists}
  SERVER_QUERY_NO_GOOD_INDEX_USED = 16;
  SERVER_QUERY_NO_INDEX_USED      = 32;
  SERVER_STATUS_DB_DROPPED        = 256; {A database was dropped}

  NET_READ_TIMEOUT          = 30;        {timeout on read}
  NET_WRITE_TIMEOUT         = 60;        {timeout on write}
  NET_WAIT_TIMEOUT          = 28800;     {wait for new query}

{ Net type }
  NET_TYPE_TCPIP     = 0;
  NET_TYPE_SOCKET    = 1;
  NET_TYPE_NAMEDPIPE = 2;

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
    PZMySQLConnect = Pointer;
    PZMySQLResult = Pointer;
    PZMySQLRow = Pointer;
    PZMySQLField = Pointer;
    PZMySQLRowOffset = Pointer;
    PZMySqlPrepStmt = Pointer;
    PZMysqlBindArray = Pointer;

{ Enum Field Types }
    TMysqlFieldTypes = (
        MYSQL_TYPE_DECIMAL,
        MYSQL_TYPE_TINY,         {BIND}
        MYSQL_TYPE_SHORT,        {BIND}
        MYSQL_TYPE_LONG,         {BIND}
        MYSQL_TYPE_FLOAT,        {BIND}
        MYSQL_TYPE_DOUBLE,       {BIND}
        MYSQL_TYPE_NULL,
        MYSQL_TYPE_TIMESTAMP,    {BIND}
        MYSQL_TYPE_LONGLONG,     {BIND}
        MYSQL_TYPE_INT24,
        MYSQL_TYPE_DATE,         {BIND}
        MYSQL_TYPE_TIME,         {BIND}
        MYSQL_TYPE_DATETIME,     {BIND}
        MYSQL_TYPE_YEAR,
        MYSQL_TYPE_NEWDATE,
        MYSQL_TYPE_VARCHAR, //<--ADDED by fduenas 20-06-2006
        MYSQL_TYPE_BIT,     //<--ADDED by fduenas 20-06-2006
        MYSQL_TYPE_NEWDECIMAL,
        MYSQL_TYPE_ENUM,
        MYSQL_TYPE_SET,
        MYSQL_TYPE_TINY_BLOB,    {BIND}
        MYSQL_TYPE_MEDIUM_BLOB,  {BIND}
        MYSQL_TYPE_LONG_BLOB,    {BIND}
        MYSQL_TYPE_BLOB,         {BIND}
        MYSQL_TYPE_VAR_STRING,   {BIND}
        MYSQL_TYPE_STRING,       {BIND}
        MYSQL_TYPE_GEOMETRY
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

  TMySqlProtocolType = (
    MYSQL_PROTOCOL_DEFAULT,
    MYSQL_PROTOCOL_TCP,
    MYSQL_PROTOCOL_SOCKET,
    MYSQL_PROTOCOL_PIPE,
    MYSQL_PROTOCOL_MEMORY
  );

  TMysqlStmtState = (
    MYSQL_STMT_INIT_DONE = 1,
    MYSQL_STMT_PREPARE_DONE,
    MYSQL_STMT_EXECUTE_DONE,
    MYSQL_STMT_FETCH_DONE
  );

  MYSQL_TIME = record
    year:                Cardinal;
    month:               Cardinal;
    day:                 Cardinal;
    hour:                Cardinal;
    minute:              Cardinal;
    second:              Cardinal;
    neg:                 Byte;
    second_part:         Int64;
  end;

  PLIST = ^LIST;
  LIST = record
    prev:       PLIST;
    next:       PLIST;
    data:       Pointer;
  end;

  PMYSQL_BIND2 = ^MYSQL_BIND2;
  MYSQL_BIND2 =  record
    length:            PLongInt;
    is_null:           PByte;
    buffer:            PChar;
    error:             PByte;
    buffer_type:       Byte;
    buffer_length:     LongInt;
    row_ptr:           PByte;
    offset:            LongInt;
    length_value:      LongInt;
    param_number:      Cardinal;
    pack_length:       Cardinal;
    error_value:       Byte;
    is_unsigned:       Byte;
    long_data_used:    Byte;
    is_null_value:     Byte;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
  end;

  PDOBindRecord2 = record
      buffer:    Array of Byte;
      length:    LongWord;
      is_null:   Byte;
  end;

const
  EMBEDDED_DEFAULT_DATA_DIR = '.\data\';
  SERVER_ARGUMENTS_KEY_PREFIX = 'ServerArgument';
  SERVER_GROUPS : array [0..2] of PChar = ('embedded'#0, 'server'#0, nil);
  DEFAULT_PARAMS : array [0..2] of PChar = ('not_used'#0,
                                            '--datadir='+EMBEDDED_DEFAULT_DATA_DIR+#0,
                                            '--set-variable=key_buffer_size=32M'#0);

const
{ General Declarations }
//  PROTOCOL_VERSION     = 10;
//  FRM_VER              = 6;

{ Enum Field Types }
  FIELD_TYPE_DECIMAL   = 0;
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
  FIELD_TYPE_GEOMETRY  = 255;

{ For Compatibility }
  FIELD_TYPE_CHAR      = FIELD_TYPE_TINY;
  FIELD_TYPE_INTERVAL  = FIELD_TYPE_ENUM;

  MAX_MYSQL_MANAGER_ERR = 256;
  MAX_MYSQL_MANAGER_MSG = 256;

  MANAGER_OK           = 200;
  MANAGER_INFO         = 250;
  MANAGER_ACCESS       = 401;
  MANAGER_CLIENT_ERR   = 450;
  MANAGER_INTERNAL_ERR = 500;

type
  TClientCapabilities = (
    CLIENT_LONG_PASSWORD,
    CLIENT_FOUND_ROWS,
    CLIENT_LONG_FLAG,
    CLIENT_CONNECT_WITH_DB,
    CLIENT_NO_SCHEMA,
    CLIENT_COMPRESS,
    CLIENT_ODBC,
    CLIENT_LOCAL_FILES,
    CLIENT_IGNORE_SPACE
  );

  TSetClientCapabilities = set of TClientCapabilities;

  TRefreshOptions = (
    _REFRESH_GRANT,
    _REFRESH_LOG,
    _REFRESH_TABLES,
    _REFRESH_HOSTS,
    _REFRESH_FAST
  );
  TSetRefreshOptions = set of TRefreshOptions;

  TMySqlStatus = (
    MYSQL_STATUS_READY,
    MYSQL_STATUS_GET_RESULT,
    MYSQL_STATUS_USE_RESULT
  );

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
    MYSQL_SECURE_AUTH
  );

  TMySqlRplType = (
    MYSQL_RPL_MASTER,
    MYSQL_RPL_SLAVE,
    MYSQL_RPL_ADMIN
  );

  TMySqlServerCommand = (
    COM_SLEEP,
    COM_QUIT,
    COM_INIT_DB,
    COM_QUERY,
    COM_FIELD_LIST,
    COM_CREATE_DB,
    COM_DROP_DB,
    COM_REFRESH,
    COM_SHUTDOWN,
    COM_STATISTICS,
    COM_PROCESS_INFO,
    COM_CONNECT,
    COM_PROCESS_KILL,
    COM_DEBUG,
    COM_PING,
    COM_TIME,
    COM_DELAYED_INSERT,
    COM_CHANGE_USER,
    COM_BINLOG_DUMP,
    COM_TABLE_DUMP,
    COM_CONNECT_OUT,
    COM_REGISTER_SLAVE,
    COM_PREPARE,
    COM_EXECUTE,
    COM_LONG_DATA,
    COM_CLOSE_STMT,
    COM_RESET_STMT,
    COM_SET_OPTION,
    COM_END
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

  NET = record
    vio:              Pointer;
    buff:             PChar;
    buff_end:         PChar;
    write_pos:        PChar;
    read_pos:         PChar;
    fd:               Integer;
    max_packet:       Cardinal;
    max_packet_size:  Cardinal;
    pkt_nr:           Cardinal;
    compress_pkt_nr:  Cardinal;
    write_timeout:    Cardinal;
    read_timeout:     Cardinal;
    retry_count:      Cardinal;
    fcntl:            Integer;
    compress:         Byte;
    remain_in_buf:    LongInt;
    length:           LongInt;
    buf_length:       LongInt;
    where_b:          LongInt;
    return_status:    Pointer;
    reading_or_writing: Char;
    save_char:        Char;
    no_send_ok:       Byte;
    last_error:       array[1..MYSQL_ERRMSG_SIZE] of Char;
    sqlstate:         array[1..SQLSTATE_LENGTH + 1] of Char;
    last_errno:       Cardinal;
    error:            Char;
    query_cache_query: Pointer;
    report_error:     Byte;
    return_errno:     Byte;
  end;

  PMYSQL_FIELD = ^MYSQL_FIELD;
  MYSQL_FIELD = record
    name:             PChar;   // Name of column
    org_name:         PChar;   // Original column name, if an alias
    table:            PChar;   // Table of column if column was a field
    org_table:        PChar;   // Org table name if table was an alias
    db:               PChar;   // Database for table
    catalog:	      PChar;   // Catalog for table
    def:              PChar;   // Default value (set by mysql_list_fields)
    length:           LongInt; // Width of column
    max_length:       LongInt; // Max width of selected set
    name_length:      Cardinal;
    org_name_length:  Cardinal;
    table_length:     Cardinal;
    org_table_length: Cardinal;
    db_length:        Cardinal;
    catalog_length:   Cardinal;
    def_length:       Cardinal;
    flags:            Cardinal; // Div flags
    decimals:         Cardinal; // Number of decimals in field
    charsetnr:        Cardinal; // Character set
    _type:            Cardinal; // Type of field. Se mysql_com.h for types
  end;

  MYSQL_FIELD_OFFSET = Cardinal;

  MYSQL_ROW = array[00..$ff] of PChar;
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

  PMYSQL_OPTIONS = ^_MYSQL_OPTIONS;
  _MYSQL_OPTIONS = record
    connect_timeout:          Cardinal;
    read_timeout:             Cardinal;
    write_timeout:            Cardinal;
    port:                     Cardinal;
    protocol:                 Cardinal;
    client_flag:              LongInt;
    host:                     PChar;
    user:                     PChar;
    password:                 PChar;
    unix_socket:              PChar;
    db:                       PChar;
    init_commands:            Pointer;
    my_cnf_file:              PChar;
    my_cnf_group:             PChar;
    charset_dir:              PChar;
    charset_name:             PChar;
    ssl_key:                  PChar;
    ssl_cert:                 PChar;
    ssl_ca:                   PChar;
    ssl_capath:               PChar;
    ssl_cipher:               PChar;
    shared_memory_base_name:  PChar;
    max_allowed_packet:       LongInt;
    use_ssl:                  Byte;
    compress:                 Byte;
    named_pipe:               Byte;
    rpl_probe:                Byte;
    rpl_parse:                Byte;
    no_master_reads:          Byte;
    separate_thread:          Byte;
    methods_to_use:           TMySqlOption;
    client_ip:                PChar;
    secure_auth:              Byte;
    local_infile_init:        Pointer;
    local_infile_read:        Pointer;
    local_infile_end:         Pointer;
    local_infile_error:       Pointer;
    local_infile_userdata:    Pointer;
  end;

  PMY_CHARSET_INFO = ^MY_CHARSET_INFO;
  MY_CHARSET_INFO = record
    number:         Cardinal;
    state:          Cardinal;
    csname:         PChar;
    name:           PChar;
    comment:        PChar;
    dir:            PChar;
    mbminlen:       Cardinal;
    mbmaxlen:       Cardinal;
  end;

  PMYSQL_METHODS =  ^MYSQL_METHODS;
  PMYSQL = ^MYSQL;

  MYSQL  = pointer;

  MYSQL_RES = record
    row_count:       Int64;
    fields:          PMYSQL_FIELD;
    data:            PMYSQL_DATA;
    data_cursor:     PMYSQL_ROWS;
    lengths:         PLongInt;
    handle:          PMYSQL;
    field_alloc:     MEM_ROOT;
    field_count:     Integer;
    current_field:   Integer;
    row:             PMYSQL_ROW;
    current_row:     PMYSQL_ROW;
    eof:             Byte;
    unbuffered_fetch_cancelled: Byte;
    methods:         PMYSQL_METHODS;
  end;
  PMYSQL_RES = ^MYSQL_RES;

  PREP_STMT_STATE=(
    MY_ST_UNKNOWN,
    MY_ST_PREPARE,
    MY_ST_EXECUTE);

  PMYSQL_BIND = ^MYSQL_BIND;
  MYSQL_BIND = record
    length:           PLongInt;
    is_null:          PByte;
    buffer:           PChar;
    buffer_type:      Cardinal;
    buffer_length:    LongInt;
    inter_buffer:     PByte;
    offset:           LongInt;
    internal_length:  LongInt;
    param_number:     Cardinal;
    long_data_used:   Byte;
    binary_data:      Byte;
    null_field:       Byte;
    internal_is_null: Byte;
    store_param_func: procedure(_net: NET; param: PMYSQL_BIND);
    fetch_result:     procedure(param: PMYSQL_BIND; row: PMYSQL_ROW);
  end;

  PMYSQL_STMT = ^MYSQL_STMT;
  MYSQL_STMT = record
    handle:               PMYSQL;
    params:               PMYSQL_BIND;
    result:               PMYSQL_RES;
    bind:                 PMYSQL_BIND;
    fields:               PMYSQL_FIELD;
    list:                 LIST;
    current_row:          PByte;
    last_fetched_buffer:  PByte;
    query:                PChar;
    mem_root:             MEM_ROOT;
    last_fetched_column:  Int64;
    stmt_id:              LongInt;
    last_errno:           Cardinal;
    param_count:          Cardinal;
    field_count:          Cardinal;
    state:                PREP_STMT_STATE;
    last_error:           array[1..MYSQL_ERRMSG_SIZE] of Char;
    sqlstate:             array[1..SQLSTATE_LENGTH + 1] of Char;
    long_alloced:         Byte;
    send_types_to_server: Byte;
    param_buffers:        Byte;
    res_buffers:          Byte;
    result_buffered:      Byte;
  end;

  MYSQL_METHODS = record
    read_query_result: function(handle: PMYSQL): Byte;
    advanced_command:  function(handle: PMYSQL; command: TMySqlServerCommand;
      header: PChar; header_length: LongInt; const arg: PChar;
      arg_length: LongInt; skip_check: Byte): Byte;
    read_rows: function( handle: PMYSQL; mysql_fields: PMYSQL_FIELD;
      fields: Cardinal): PMYSQL_DATA;
    use_result: function(handle: PMYSQL): PMYSQL_RES;
    fetch_lengths: procedure(_to: PLongInt; column: MYSQL_ROW;
      field_count: Cardinal);
    list_fields: function(handle: PMYSQL): PMYSQL_FIELD;
    read_prepare_result: function(handle: PMYSQL; stmt: PMYSQL_STMT): Byte;
    stmt_execute: function(stmt: PMYSQL_STMT): Integer;
    read_binary_rows: function(stmt: PMYSQL_STMT): PMYSQL_DATA;
    unbuffered_fetch: function(handle: PMYSQL; row: PMYSQL_ROW): Integer;
    free_embedded_thd: procedure(handle: PMYSQL);
    read_statisticd: function(handle: PMYSQL): PChar;
  end;

  TModifyType = (MODIFY_INSERT, MODIFY_UPDATE, MODIFY_DELETE);
  TQuoteOptions = (QUOTE_STRIP_CR,QUOTE_STRIP_LF);
  TQuoteOptionsSet = set of TQuoteOptions;

  { Options for mysql_set_option }
  TMySqlSetOption = (
    MYSQL_OPTION_MULTI_STATEMENTS_ON,
    MYSQL_OPTION_MULTI_STATEMENTS_OFF
  );

  TALMySqlVersion_API = (MYSQL5);

  TALMySqlLibrary = class(TObject)
  private
    Flibmysql: THandle;
    FVersion_API: TALMySqlVersion_API;
  public
    mysql_affected_rows: function(Handle: PMYSQL): Int64; stdcall;
    mysql_character_set_name: function(Handle: PMYSQL): PChar; stdcall;
    mysql_close: procedure(Handle: PMYSQL); stdcall;
    mysql_data_seek: procedure(Result: PMYSQL_RES; Offset: Int64); stdcall;
    mysql_debug: procedure(Debug: PChar); stdcall;
    mysql_dump_debug_info: function(Handle: PMYSQL): Integer; stdcall;
    mysql_eof: function(Result: PMYSQL_RES): Byte; stdcall;
    mysql_errno: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_error: function(Handle: PMYSQL): PChar; stdcall;
    mysql_escape_string: function(PTo, PFrom: PChar; Len: Cardinal): Cardinal; stdcall;
    mysql_fetch_field: function(Result: PMYSQL_RES): PMYSQL_FIELD; stdcall;
    mysql_fetch_field_direct: function(Result: PMYSQL_RES; FieldNo: Cardinal): PMYSQL_FIELD; stdcall;
    mysql_fetch_fields: function(Result: PMYSQL_RES): PMYSQL_FIELD; stdcall;
    mysql_fetch_lengths: function(Result: PMYSQL_RES): PLongInt; stdcall;
    mysql_fetch_row: function(Result: PMYSQL_RES): PMYSQL_ROW; stdcall;
    mysql_field_seek: function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; stdcall;
    mysql_field_tell: function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET; stdcall;
    mysql_free_result: procedure(Result: PMYSQL_RES); stdcall;
    mysql_get_client_info: function: PChar; stdcall;
    mysql_get_host_info: function(Handle: PMYSQL): PChar; stdcall;
    mysql_get_proto_info: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_get_server_info: function(Handle: PMYSQL): PChar; stdcall;
    mysql_info: function(Handle: PMYSQL): PChar; stdcall;
    mysql_init: function(Handle: PMYSQL): PMYSQL; stdcall;
    mysql_insert_id: function(Handle: PMYSQL): Int64; stdcall;
    mysql_kill: function(Handle: PMYSQL; Pid: LongInt): Integer; stdcall;
    mysql_list_dbs: function(Handle: PMYSQL; Wild: PChar): PMYSQL_RES; stdcall;
    mysql_list_fields: function(Handle: PMYSQL; const Table, Wild: PChar): PMYSQL_RES; stdcall;
    mysql_list_processes: function(Handle: PMYSQL): PMYSQL_RES; stdcall;
    mysql_list_tables: function(Handle: PMYSQL; const Wild: PChar): PMYSQL_RES; stdcall;
    mysql_num_fields: function(Result: PMYSQL_RES): Cardinal; stdcall;
    mysql_num_rows: function(Result: PMYSQL_RES): Int64; stdcall;
    mysql_options: function(Handle: PMYSQL; Option: TMySqlOption; const Arg: PChar): Integer; stdcall;
    mysql_ping: function(Handle: PMYSQL): Integer; stdcall;
    mysql_query: function(Handle: PMYSQL; const Query: PChar): Integer; stdcall;
    mysql_real_connect: function(Handle: PMYSQL; const Host, User, Passwd, Db: PChar; Port: Cardinal; const UnixSocket: PChar; ClientFlag: Cardinal): PMYSQL; stdcall;
    mysql_real_escape_string: function(Handle: PMYSQL; PTo: PChar; const PFrom: PChar; length: Cardinal): Cardinal; stdcall;
    mysql_real_query: function(Handle: PMYSQL; const Query: PChar; Length: Cardinal): Integer;stdcall;
    mysql_refresh: function(Handle: PMYSQL; Options: Cardinal): Integer; stdcall;
    mysql_row_seek: function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS; stdcall;
    mysql_row_tell: function(Result: PMYSQL_RES): PMYSQL_ROWS; stdcall;
    mysql_select_db: function(Handle: PMYSQL; const Db: PChar): Integer; stdcall;
    mysql_ssl_set: function(Handle: PMYSQL; const key, cert, CA, CApath, cipher:PChar): Byte; stdcall;
    mysql_stat: function(Handle: PMYSQL): PChar; stdcall;
    mysql_store_result: function(Handle: PMYSQL): PMYSQL_RES; stdcall;
    mysql_thread_id: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_use_result: function(Handle: PMYSQL): PMYSQL_RES; stdcall;
    my_init: procedure; stdcall;
    mysql_thread_init: function: Byte; stdcall;
    mysql_thread_end: procedure; stdcall;
    mysql_thread_safe: function: Cardinal; stdcall;
    mysql_server_init: function(Argc: Integer; Argv, Groups: Pointer): Integer; stdcall;
    mysql_server_end: procedure; stdcall;
    mysql_change_user: function(mysql: PMYSQL; const user: PChar; const passwd: PChar; const db: PChar): Byte;
    mysql_field_count: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_get_client_version: function: Cardinal; stdcall;
    mysql_send_query: function(mysql: PMYSQL; const query: PChar;length: Cardinal): Integer; stdcall;
    mysql_read_query_result: function(mysql: PMYSQL): Integer; stdcall;
    mysql_master_query: function(mysql: PMYSQL; const query: PChar;length: Cardinal): Byte; stdcall;
    mysql_slave_query: function(mysql: PMYSQL; const query: PChar; length: Cardinal): Byte; stdcall;
    mysql_enable_rpl_parse: procedure(mysql: PMYSQL); stdcall;
    mysql_disable_rpl_parse: procedure(mysql: PMYSQL); stdcall;
    mysql_rpl_parse_enabled: function(mysql: PMYSQL): Integer; stdcall;
    mysql_enable_reads_from_master: procedure(mysql: PMYSQL); stdcall;
    mysql_disable_reads_from_master: procedure(mysql: PMYSQL); stdcall;
    mysql_rpl_query_type: function(const query: PChar; len: Integer): TMySqlRplType; stdcall;
    mysql_rpl_probe: function(mysql: PMYSQL): Byte; stdcall;
    mysql_autocommit: function(Handle: PMYSQL; const mode: Byte): Byte; stdcall;
    mysql_commit: function(Handle: PMYSQL): Byte; stdcall;
    mysql_get_server_version: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_hex_string: function(PTo, PFrom: Pchar; Len: Cardinal): Cardinal; stdcall;
    mysql_more_results: function(Handle: PMYSQL): Byte; stdcall;
    mysql_next_result: function(Handle: PMYSQL): Integer; stdcall;
    mysql_rollback: function(Handle: PMYSQL): Byte; stdcall;
    mysql_set_character_set: function(Handle: PMYSQL; csname: PChar): Integer; stdcall;
    mysql_set_server_option: function(Handle: PMYSQL; Option: TMysqlSetOption): Integer; stdcall;
    mysql_shutdown: function(Handle: PMYSQL; shutdown_level: TMysqlShutdownLevel): Integer;
    mysql_sqlstate: function(Handle: PMYSQL): PChar; stdcall;
    mysql_warning_count: function(Handle: PMYSQL): Cardinal; stdcall;
    mysql_stmt_affected_rows: function(stmt: PMYSQL_STMT): Int64; stdcall;
    mysql_stmt_attr_get: function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; arg: PChar): Integer; stdcall;
    mysql_stmt_attr_set: function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; const arg: PChar): Integer; stdcall;
    mysql_stmt_bind_param: function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND2): Byte; stdcall;
    mysql_stmt_bind_result: function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND2): Byte; stdcall;
    mysql_stmt_close: function(stmt: PMYSQL_STMT): Byte; stdcall;
    mysql_stmt_data_seek: procedure(stmt: PMYSQL_STMT; offset: Int64); stdcall;
    mysql_stmt_errno: function(stmt: PMYSQL_STMT): Cardinal; stdcall;
    mysql_stmt_error: function(stmt: PMYSQL_STMT): PChar; stdcall;
    mysql_stmt_execute: function(stmt: PMYSQL_STMT): Integer; stdcall;
    mysql_stmt_fetch: function(stmt: PMYSQL_STMT): Integer; stdcall;
    mysql_stmt_fetch_column: function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND2; column: Cardinal; offset: Cardinal): Integer; stdcall;
    mysql_stmt_field_count: function(stmt: PMYSQL_STMT): Cardinal; stdcall;
    mysql_stmt_free_result: function(stmt: PMYSQL_STMT): Byte; stdcall;
    mysql_stmt_init: function(Handle: PMYSQL): PMYSQL_STMT; stdcall;
    mysql_stmt_insert_id: function(stmt: PMYSQL_STMT): Int64; stdcall;
    mysql_stmt_num_rows: function(stmt: PMYSQL_STMT): Int64; stdcall;
    mysql_stmt_param_count: function(stmt: PMYSQL_STMT): Cardinal; stdcall;
    mysql_stmt_param_metadata: function(stmt: PMYSQL_STMT): PMYSQL_RES; stdcall;
    mysql_stmt_prepare: function(stmt: PMYSQL_STMT; const query: PChar; length: Cardinal): Integer; stdcall;
    mysql_stmt_reset: function(stmt: PMYSQL_STMT): Byte; stdcall;
    mysql_stmt_result_metadata: function(stmt: PMYSQL_STMT): PMYSQL_RES; stdcall;
    mysql_stmt_row_seek: function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS; stdcall;
    mysql_stmt_row_tell: function(stmt: PMYSQL_STMT): PMYSQL_ROWS; stdcall;
    mysql_stmt_send_long_data: function(stmt: PMYSQL_STMT; parameter_number: Cardinal; const data: PChar; length: Cardinal): Byte; stdcall;
    mysql_stmt_sqlstate: function(stmt: PMYSQL_STMT): PChar; stdcall;
    mysql_stmt_store_result: function(stmt: PMYSQL_STMT): Integer; stdcall;
    mysql_get_character_set_info: procedure(Handle: PMYSQL; cs: PMY_CHARSET_INFO); stdcall;
    constructor Create(ApiVer: TALMySqlVersion_API); virtual;
    destructor Destroy; override;
    function Loaded: Boolean; virtual;
    function Unload: Boolean; virtual;
    function Load(const lib: string = 'libmysql.dll'): Boolean; virtual;
  end;

implementation


{ TALMySqlLibrary }

{**************************************************************}
constructor TALMySqlLibrary.Create(ApiVer: TALMySqlVersion_API);
begin
  Flibmysql := 0;
  FVersion_API := ApiVer;
end;

{*********************************}
destructor TALMySqlLibrary.Destroy;
begin
  Unload;
  inherited Destroy;
end;

{***************************************}
function TALMySqlLibrary.Loaded: Boolean;
begin
  Result := Flibmysql > HINSTANCE_ERROR;
end;

function TALMySqlLibrary.Unload: Boolean;
begin
  Result := True;
  if Loaded then begin
    Result := Boolean(FreeLibrary(Flibmysql));
    Flibmysql := 0;
    mysql_affected_rows := nil;
    mysql_character_set_name := nil;
    mysql_close := nil;
    mysql_data_seek := nil;
    mysql_debug := nil;
    mysql_dump_debug_info := nil;
    mysql_eof := nil;
    mysql_errno := nil;
    mysql_error := nil;
    mysql_escape_string := nil;
    mysql_fetch_field := nil;
    mysql_fetch_field_direct := nil;
    mysql_fetch_fields := nil;
    mysql_fetch_lengths := nil;
    mysql_fetch_row := nil;
    mysql_field_seek := nil;
    mysql_field_tell := nil;
    mysql_free_result := nil;
    mysql_get_client_info := nil;
    mysql_get_host_info := nil;
    mysql_get_proto_info := nil;
    mysql_get_server_info := nil;
    mysql_info := nil;
    mysql_init := nil;
    mysql_insert_id := nil;
    mysql_kill := nil;
    mysql_list_dbs := nil;
    mysql_list_fields := nil;
    mysql_list_processes := nil;
    mysql_list_tables := nil;
    mysql_num_fields := nil;
    mysql_num_rows := nil;
    mysql_options := nil;
    mysql_ping := nil;
    mysql_query := nil;
    mysql_real_connect := nil;
    mysql_real_escape_string := nil;
    mysql_real_query := nil;
    mysql_refresh := nil;
    mysql_row_seek := nil;
    mysql_row_tell := nil;
    mysql_select_db := nil;
    mysql_ssl_set := nil;
    mysql_stat := nil;
    mysql_store_result := nil;
    mysql_thread_id := nil;
    mysql_use_result := nil;
    my_init := nil;
    mysql_thread_init := nil;
    mysql_thread_end := nil;
    mysql_thread_safe := nil;
    mysql_server_init := nil;
    mysql_server_end := nil;
    mysql_change_user := nil;
    mysql_field_count := nil;
    mysql_get_client_version := nil;
    mysql_send_query := nil;
    mysql_read_query_result := nil;
    mysql_master_query := nil;
    mysql_slave_query := nil;
    mysql_enable_rpl_parse := nil;
    mysql_disable_rpl_parse := nil;
    mysql_rpl_parse_enabled := nil;
    mysql_enable_reads_from_master := nil;
    mysql_disable_reads_from_master := nil;
    mysql_rpl_query_type := nil;
    mysql_rpl_probe := nil;
    mysql_autocommit := nil;
    mysql_commit := nil;
    mysql_get_server_version := nil;
    mysql_hex_string := nil;
    mysql_more_results := nil;
    mysql_next_result := nil;
    mysql_rollback := nil;
    mysql_set_character_set := nil;
    mysql_set_server_option := nil;
    mysql_shutdown := nil;
    mysql_sqlstate := nil;
    mysql_warning_count := nil;
    mysql_stmt_affected_rows := nil;
    mysql_stmt_attr_get := nil;
    mysql_stmt_attr_set := nil;
    mysql_stmt_bind_param := nil;
    mysql_stmt_bind_result := nil;
    mysql_stmt_close := nil;
    mysql_stmt_data_seek := nil;
    mysql_stmt_errno := nil;
    mysql_stmt_error := nil;
    mysql_stmt_execute := nil;
    mysql_stmt_fetch := nil;
    mysql_stmt_fetch_column := nil;
    mysql_stmt_field_count := nil;
    mysql_stmt_free_result := nil;
    mysql_stmt_init := nil;
    mysql_stmt_insert_id := nil;
    mysql_stmt_num_rows := nil;
    mysql_stmt_param_count := nil;
    mysql_stmt_param_metadata := nil;
    mysql_stmt_prepare := nil;
    mysql_stmt_reset := nil;
    mysql_stmt_result_metadata := nil;
    mysql_stmt_row_seek := nil;
    mysql_stmt_row_tell := nil;
    mysql_stmt_send_long_data := nil;
    mysql_stmt_sqlstate := nil;
    mysql_stmt_store_result := nil;
    mysql_get_character_set_info := nil;
  end;
end;

{********************************************************}
function TALMySqlLibrary.Load(const lib: string): Boolean;
Begin
  Result := Loaded;
  if not Result then begin
    Flibmysql := LoadLibrary(PChar(lib));
    if Loaded then begin
      mysql_affected_rows := GetProcAddress(Flibmysql,'mysql_affected_rows');
      mysql_character_set_name := GetProcAddress(Flibmysql,'mysql_character_set_name');
      mysql_close := GetProcAddress(Flibmysql,'mysql_close');
      mysql_data_seek := GetProcAddress(Flibmysql,'mysql_data_seek');
      mysql_debug := GetProcAddress(Flibmysql,'mysql_debug');
      mysql_dump_debug_info := GetProcAddress(Flibmysql,'mysql_dump_debug_info');
      mysql_eof := GetProcAddress(Flibmysql,'mysql_eof');
      mysql_errno := GetProcAddress(Flibmysql,'mysql_errno');
      mysql_error := GetProcAddress(Flibmysql,'mysql_error');
      mysql_escape_string := GetProcAddress(Flibmysql,'mysql_escape_string');
      mysql_fetch_field := GetProcAddress(Flibmysql,'mysql_fetch_field');
      mysql_fetch_field_direct := GetProcAddress(Flibmysql,'mysql_fetch_field_direct');
      mysql_fetch_fields := GetProcAddress(Flibmysql,'mysql_fetch_fields');
      mysql_fetch_lengths := GetProcAddress(Flibmysql,'mysql_fetch_lengths');
      mysql_fetch_row := GetProcAddress(Flibmysql,'mysql_fetch_row');
      mysql_field_seek := GetProcAddress(Flibmysql,'mysql_field_seek');
      mysql_field_tell := GetProcAddress(Flibmysql,'mysql_field_tell');
      mysql_free_result := GetProcAddress(Flibmysql,'mysql_free_result');
      mysql_get_client_info := GetProcAddress(Flibmysql,'mysql_get_client_info');
      mysql_get_host_info := GetProcAddress(Flibmysql,'mysql_get_host_info');
      mysql_get_proto_info := GetProcAddress(Flibmysql,'mysql_get_proto_info');
      mysql_get_server_info := GetProcAddress(Flibmysql,'mysql_get_server_info');
      mysql_info := GetProcAddress(Flibmysql,'mysql_info');
      mysql_init := GetProcAddress(Flibmysql,'mysql_init');
      mysql_insert_id := GetProcAddress(Flibmysql,'mysql_insert_id');
      mysql_kill := GetProcAddress(Flibmysql,'mysql_kill');
      mysql_list_dbs := GetProcAddress(Flibmysql,'mysql_list_dbs');
      mysql_list_fields := GetProcAddress(Flibmysql,'mysql_list_fields');
      mysql_list_processes := GetProcAddress(Flibmysql,'mysql_list_processes');
      mysql_list_tables := GetProcAddress(Flibmysql,'mysql_list_tables');
      mysql_num_fields := GetProcAddress(Flibmysql,'mysql_num_fields');
      mysql_num_rows := GetProcAddress(Flibmysql,'mysql_num_rows');
      mysql_options := GetProcAddress(Flibmysql,'mysql_options');
      mysql_ping := GetProcAddress(Flibmysql,'mysql_ping');
      mysql_query := GetProcAddress(Flibmysql,'mysql_query');
      mysql_real_connect := GetProcAddress(Flibmysql,'mysql_real_connect');
      mysql_real_escape_string := GetProcAddress(Flibmysql,'mysql_real_escape_string');
      mysql_real_query := GetProcAddress(Flibmysql,'mysql_real_query');
      mysql_refresh := GetProcAddress(Flibmysql,'mysql_refresh');
      mysql_row_seek := GetProcAddress(Flibmysql,'mysql_row_seek');
      mysql_row_tell := GetProcAddress(Flibmysql,'mysql_row_tell');
      mysql_select_db := GetProcAddress(Flibmysql,'mysql_select_db');
      mysql_ssl_set := GetProcAddress(Flibmysql,'mysql_ssl_set');
      mysql_stat := GetProcAddress(Flibmysql,'mysql_stat');
      mysql_store_result := GetProcAddress(Flibmysql,'mysql_store_result');
      mysql_thread_id := GetProcAddress(Flibmysql,'mysql_thread_id');
      mysql_use_result := GetProcAddress(Flibmysql,'mysql_use_result');
      my_init := GetProcAddress(Flibmysql,'my_init');
      mysql_thread_init := GetProcAddress(Flibmysql,'mysql_thread_init');
      mysql_thread_end := GetProcAddress(Flibmysql,'mysql_thread_end');
      mysql_thread_safe := GetProcAddress(Flibmysql,'mysql_thread_safe');
      mysql_server_init := GetProcAddress(Flibmysql,'mysql_server_init');
      mysql_server_end := GetProcAddress(Flibmysql,'mysql_server_end');
      mysql_change_user := GetProcAddress(Flibmysql,'mysql_change_user');
      mysql_field_count := GetProcAddress(Flibmysql,'mysql_field_count');
      mysql_get_client_version := GetProcAddress(Flibmysql,'mysql_get_client_version');
      mysql_send_query := GetProcAddress(Flibmysql,'mysql_send_query');
      mysql_read_query_result := GetProcAddress(Flibmysql,'mysql_read_query_result');
      mysql_master_query := GetProcAddress(Flibmysql,'mysql_master_query');
      mysql_slave_query := GetProcAddress(Flibmysql,'mysql_slave_query');
      mysql_enable_rpl_parse := GetProcAddress(Flibmysql,'mysql_enable_rpl_parse');
      mysql_disable_rpl_parse := GetProcAddress(Flibmysql,'mysql_disable_rpl_parse');
      mysql_rpl_parse_enabled := GetProcAddress(Flibmysql,'mysql_rpl_parse_enabled');
      mysql_enable_reads_from_master := GetProcAddress(Flibmysql,'mysql_enable_reads_from_master');
      mysql_disable_reads_from_master := GetProcAddress(Flibmysql,'mysql_disable_reads_from_master');
      mysql_rpl_query_type := GetProcAddress(Flibmysql,'mysql_rpl_query_type');
      mysql_rpl_probe := GetProcAddress(Flibmysql,'mysql_rpl_probe');
      mysql_autocommit := GetProcAddress(Flibmysql,'mysql_autocommit');
      mysql_commit := GetProcAddress(Flibmysql,'mysql_commit');
      mysql_get_server_version := GetProcAddress(Flibmysql,'mysql_get_server_version');
      mysql_hex_string := GetProcAddress(Flibmysql,'mysql_hex_string');
      mysql_more_results := GetProcAddress(Flibmysql,'mysql_more_results');
      mysql_next_result := GetProcAddress(Flibmysql,'mysql_next_result');
      mysql_rollback := GetProcAddress(Flibmysql,'mysql_rollback');
      mysql_set_character_set := GetProcAddress(Flibmysql,'mysql_set_character_set');
      mysql_set_server_option := GetProcAddress(Flibmysql,'mysql_set_server_option');
      mysql_shutdown := GetProcAddress(Flibmysql,'mysql_shutdown');
      mysql_sqlstate := GetProcAddress(Flibmysql,'mysql_sqlstate');
      mysql_warning_count := GetProcAddress(Flibmysql,'mysql_warning_count');
      mysql_stmt_affected_rows := GetProcAddress(Flibmysql,'mysql_stmt_affected_rows');
      mysql_stmt_attr_get := GetProcAddress(Flibmysql,'mysql_stmt_attr_get');
      mysql_stmt_attr_set := GetProcAddress(Flibmysql,'mysql_stmt_attr_set');
      mysql_stmt_bind_param := GetProcAddress(Flibmysql,'mysql_stmt_bind_param');
      mysql_stmt_bind_result := GetProcAddress(Flibmysql,'mysql_stmt_bind_result');
      mysql_stmt_close := GetProcAddress(Flibmysql,'mysql_stmt_close');
      mysql_stmt_data_seek := GetProcAddress(Flibmysql,'mysql_stmt_data_seek');
      mysql_stmt_errno := GetProcAddress(Flibmysql,'mysql_stmt_errno');
      mysql_stmt_error := GetProcAddress(Flibmysql,'mysql_stmt_error');
      mysql_stmt_execute := GetProcAddress(Flibmysql,'mysql_stmt_execute');
      mysql_stmt_fetch := GetProcAddress(Flibmysql,'mysql_stmt_fetch');
      mysql_stmt_fetch_column := GetProcAddress(Flibmysql,'mysql_stmt_fetch_column');
      mysql_stmt_field_count := GetProcAddress(Flibmysql,'mysql_stmt_field_count');
      mysql_stmt_free_result := GetProcAddress(Flibmysql,'mysql_stmt_free_result');
      mysql_stmt_init := GetProcAddress(Flibmysql,'mysql_stmt_init');
      mysql_stmt_insert_id := GetProcAddress(Flibmysql,'mysql_stmt_insert_id');
      mysql_stmt_num_rows := GetProcAddress(Flibmysql,'mysql_stmt_num_rows');
      mysql_stmt_param_count := GetProcAddress(Flibmysql,'mysql_stmt_param_count');
      mysql_stmt_param_metadata := GetProcAddress(Flibmysql,'mysql_stmt_param_metadata');
      mysql_stmt_prepare := GetProcAddress(Flibmysql,'mysql_stmt_prepare');
      mysql_stmt_reset := GetProcAddress(Flibmysql,'mysql_stmt_reset');
      mysql_stmt_result_metadata := GetProcAddress(Flibmysql,'mysql_stmt_result_metadata');
      mysql_stmt_row_seek := GetProcAddress(Flibmysql,'mysql_stmt_row_seek');
      mysql_stmt_row_tell := GetProcAddress(Flibmysql,'mysql_stmt_row_tell');
      mysql_stmt_send_long_data := GetProcAddress(Flibmysql,'mysql_stmt_send_long_data');
      mysql_stmt_sqlstate := GetProcAddress(Flibmysql,'mysql_stmt_sqlstate');
      mysql_stmt_store_result := GetProcAddress(Flibmysql,'mysql_stmt_store_result');
      mysql_get_character_set_info := GetProcAddress(Flibmysql,'mysql_get_character_set_info');

      Result := assigned(mysql_affected_rows) and
                assigned(mysql_character_set_name) and
                assigned(mysql_close) and
                assigned(mysql_data_seek) and
                assigned(mysql_debug) and
                assigned(mysql_dump_debug_info) and
                assigned(mysql_eof) and
                assigned(mysql_errno) and
                assigned(mysql_error) and
                assigned(mysql_escape_string) and
                assigned(mysql_fetch_field) and
                assigned(mysql_fetch_field_direct) and
                assigned(mysql_fetch_fields) and
                assigned(mysql_fetch_lengths) and
                assigned(mysql_fetch_row) and
                assigned(mysql_field_seek) and
                assigned(mysql_field_tell) and
                assigned(mysql_free_result) and
                assigned(mysql_get_client_info) and
                assigned(mysql_get_host_info) and
                assigned(mysql_get_proto_info) and
                assigned(mysql_get_server_info) and
                assigned(mysql_info) and
                assigned(mysql_init) and
                assigned(mysql_insert_id) and
                assigned(mysql_kill) and
                assigned(mysql_list_dbs) and
                assigned(mysql_list_fields) and
                assigned(mysql_list_processes) and
                assigned(mysql_list_tables) and
                assigned(mysql_num_fields) and
                assigned(mysql_num_rows) and
                assigned(mysql_options) and
                assigned(mysql_ping) and
                assigned(mysql_query) and
                assigned(mysql_real_connect) and
                assigned(mysql_real_escape_string) and
                assigned(mysql_real_query) and
                assigned(mysql_refresh) and
                assigned(mysql_row_seek) and
                assigned(mysql_row_tell) and
                assigned(mysql_select_db) and
                assigned(mysql_ssl_set) and
                assigned(mysql_stat) and
                assigned(mysql_store_result) and
                assigned(mysql_thread_id) and
                assigned(mysql_use_result) and
                assigned(my_init) and
                assigned(mysql_thread_init) and
                assigned(mysql_thread_end) and
                assigned(mysql_thread_safe) and
                assigned(mysql_server_init) and
                assigned(mysql_server_end) and
                assigned(mysql_change_user) and
                assigned(mysql_field_count) and
                assigned(mysql_get_client_version) and
                assigned(mysql_send_query) and
                assigned(mysql_read_query_result) and
                assigned(mysql_master_query) and
                assigned(mysql_slave_query) and
                assigned(mysql_enable_rpl_parse) and
                assigned(mysql_disable_rpl_parse) and
                assigned(mysql_rpl_parse_enabled) and
                assigned(mysql_enable_reads_from_master) and
                assigned(mysql_disable_reads_from_master) and
                assigned(mysql_rpl_query_type) and
                assigned(mysql_rpl_probe) and
                assigned(mysql_autocommit) and
                assigned(mysql_commit) and
                assigned(mysql_get_server_version) and
                assigned(mysql_hex_string) and
                assigned(mysql_more_results) and
                assigned(mysql_next_result) and
                assigned(mysql_rollback) and
                assigned(mysql_set_character_set) and
                assigned(mysql_set_server_option) and
                assigned(mysql_shutdown) and
                assigned(mysql_sqlstate) and
                assigned(mysql_warning_count) and
                assigned(mysql_stmt_affected_rows) and
                assigned(mysql_stmt_attr_get) and
                assigned(mysql_stmt_attr_set) and
                assigned(mysql_stmt_bind_param) and
                assigned(mysql_stmt_bind_result) and
                assigned(mysql_stmt_close) and
                assigned(mysql_stmt_data_seek) and
                assigned(mysql_stmt_errno) and
                assigned(mysql_stmt_error) and
                assigned(mysql_stmt_execute) and
                assigned(mysql_stmt_fetch) and
                assigned(mysql_stmt_fetch_column) and
                assigned(mysql_stmt_field_count) and
                assigned(mysql_stmt_free_result) and
                assigned(mysql_stmt_init) and
                assigned(mysql_stmt_insert_id) and
                assigned(mysql_stmt_num_rows) and
                assigned(mysql_stmt_param_count) and
                assigned(mysql_stmt_param_metadata) and
                assigned(mysql_stmt_prepare) and
                assigned(mysql_stmt_reset) and
                assigned(mysql_stmt_result_metadata) and
                assigned(mysql_stmt_row_seek) and
                assigned(mysql_stmt_row_tell) and
                assigned(mysql_stmt_send_long_data) and
                assigned(mysql_stmt_sqlstate) and
                assigned(mysql_stmt_store_result) and
                assigned(mysql_get_character_set_info);

      if not Result then begin
        Unload;
        raise Exception.Create(cALMySql_INVALIDEIBVERSION);
      end;
    end
    else raise Exception.CreateFmt(cALMySql_INVALIDEIBVERSION, [lib]);
  end;
end;

end.

