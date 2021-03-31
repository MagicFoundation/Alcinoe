{*******************************************************************************
Author(s):
Sergey Seroukhov (Zeos Database Objects)
based on Mysql-direct library by Cristian Nicola

MysQL libmysql.dll Version 5 API Interface Unit

Link :
http://www.sourceforge.net/projects/zeoslib
http://dev.mysql.com/doc/refman/5.0/en/c-api-functions.html
*******************************************************************************}

unit ALMySqlWrapper;

interface

Uses
  Winapi.Windows;

const

  { resource string }
  cALMySql_INVALIDELIBVERSION   = 'Incorrect Database Server Version.';
  cALMySql_CANTLOADLIB          = 'Can''t load library: %s.';

  { taken from errmsg.h }
  CR_UNKNOWN_ERROR = 2000;
  CR_SOCKET_CREATE_ERROR = 2001;
  CR_CONNECTION_ERROR = 2002;
  CR_CONN_HOST_ERROR = 2003;
  CR_IPSOCK_ERROR = 2004;
  CR_UNKNOWN_HOST = 2005;
  CR_SERVER_GONE_ERROR = 2006;
  CR_VERSION_ERROR = 2007;
  CR_OUT_OF_MEMORY = 2008;
  CR_WRONG_HOST_INFO = 2009;
  CR_LOCALHOST_CONNECTION = 2010;
  CR_TCP_CONNECTION = 2011;
  CR_SERVER_HANDSHAKE_ERR = 2012;
  CR_SERVER_LOST = 2013;
  CR_COMMANDS_OUT_OF_SYNC = 2014;
  CR_NAMEDPIPE_CONNECTION = 2015;
  CR_NAMEDPIPEWAIT_ERROR = 2016;
  CR_NAMEDPIPEOPEN_ERROR = 2017;
  CR_NAMEDPIPESETSTATE_ERROR = 2018;
  CR_CANT_READ_CHARSET = 2019;
  CR_NET_PACKET_TOO_LARGE = 2020;
  CR_EMBEDDED_CONNECTION = 2021;
  CR_PROBE_SLAVE_STATUS = 2022;
  CR_PROBE_SLAVE_HOSTS = 2023;
  CR_PROBE_SLAVE_CONNECT = 2024;
  CR_PROBE_MASTER_CONNECT = 2025;
  CR_SSL_CONNECTION_ERROR = 2026;
  CR_MALFORMED_PACKET = 2027;
  CR_WRONG_LICENSE = 2028;

  CR_NULL_POINTER = 2029;
  CR_NO_PREPARE_STMT = 2030;
  CR_PARAMS_NOT_BOUND = 2031;
  CR_DATA_TRUNCATED = 2032;
  CR_NO_PARAMETERS_EXISTS = 2033;
  CR_INVALID_PARAMETER_NO = 2034;
  CR_INVALID_BUFFER_USE = 2035;
  CR_UNSUPPORTED_PARAM_TYPE = 2036;

  CR_SHARED_MEMORY_CONNECTION = 2037;
  CR_SHARED_MEMORY_CONNECT_REQUEST_ERROR = 2038;
  CR_SHARED_MEMORY_CONNECT_ANSWER_ERROR = 2039;
  CR_SHARED_MEMORY_CONNECT_FILE_MAP_ERROR = 2040;
  CR_SHARED_MEMORY_CONNECT_MAP_ERROR = 2041;
  CR_SHARED_MEMORY_FILE_MAP_ERROR = 2042;
  CR_SHARED_MEMORY_MAP_ERROR = 2043;
  CR_SHARED_MEMORY_EVENT_ERROR = 2044;
  CR_SHARED_MEMORY_CONNECT_ABANDONED_ERROR = 2045;
  CR_SHARED_MEMORY_CONNECT_SET_ERROR = 2046;
  CR_CONN_UNKNOW_PROTOCOL = 2047;
  CR_INVALID_CONN_HANDLE = 2048;
  CR_SECURE_AUTH = 2049;
  CR_FETCH_CANCELED = 2050;
  CR_NO_DATA = 2051;
  CR_NO_STMT_METADATA = 2052;
  CR_NO_RESULT_SET = 2053;
  CR_NOT_IMPLEMENTED = 2054;
  CR_SERVER_LOST_EXTENDED = 2055;
  CR_STMT_CLOSED = 2056;
  CR_NEW_STMT_METADATA = 2057;
  CR_ALREADY_CONNECTED = 2058;
  CR_AUTH_PLUGIN_CANNOT_LOAD = 2059;

  { General Declarations }
  MYSQL_ERRMSG_SIZE    = 512;
  SQLSTATE_LENGTH      = 5;

  MYSQL_PORT           = 3306;
  LOCAL_HOST           = 'localhost';

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

  PMYSQL_FIELDS = ^MYSQL_FIELDS;
  MYSQL_FIELDS = array[0..MaxInt div SizeOf(MYSQL_FIELD) - 1] of MYSQL_FIELD;

  PMYSQL_LENGTHS = ^TMYSQL_LENGTHS;
  TMYSQL_LENGTHS = array[0..MaxInt div SizeOf(longword) - 1] of longword;

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

  TALMySqlVersion_API = (MYSQL50, MYSQL55);

  TALMySqlLibrary = class(TObject)
  private
    Flibmysql: THandle;
    FVersion_API: TALMySqlVersion_API;
  public
    { Functions to get information from the MYSQL and MYSQL_RES structures
    Should definitely be used if one uses shared libraries. }
    mysql_affected_rows: function(Handle: PMYSQL): ULongLong; stdcall;
    mysql_character_set_name: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_close: procedure(Handle: PMYSQL); stdcall;
    mysql_data_seek: procedure(Result: PMYSQL_RES; Offset: ULongLong); stdcall;
    mysql_debug: procedure(Debug: PAnsiChar); stdcall;
    mysql_dump_debug_info: function(Handle: PMYSQL): Integer; stdcall;
    mysql_eof: function(Result: PMYSQL_RES): Byte; stdcall; {deprecated for mysql_error/mysql_errno}
    mysql_errno: function(Handle: PMYSQL): UInt; stdcall;
    mysql_error: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_escape_string: function(PTo, PFrom: PAnsiChar; Len: ULong): ULong; stdcall; {deprecated for mysql_real_escape_string}
    mysql_fetch_field: function(Result: PMYSQL_RES): PMYSQL_FIELD; stdcall;
    mysql_fetch_field_direct: function(Result: PMYSQL_RES; FieldNo: UInt): PMYSQL_FIELD; stdcall;
    mysql_fetch_fields: function(Result: PMYSQL_RES): PMYSQL_FIELDS; stdcall;
    mysql_fetch_lengths: function(Result: PMYSQL_RES): PMYSQL_LENGTHS; stdcall;
    mysql_fetch_row: function(Result: PMYSQL_RES): PMYSQL_ROW; stdcall;
    mysql_field_seek: function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; stdcall;
    mysql_field_tell: function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET; stdcall;
    mysql_free_result: procedure(Result: PMYSQL_RES); stdcall;
    mysql_get_client_info: function: PAnsiChar; stdcall;
    mysql_get_host_info: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_get_proto_info: function(Handle: PMYSQL): UInt; stdcall;
    mysql_get_server_info: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_info: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_init: function(Handle: PMYSQL): PMYSQL; stdcall;
    mysql_insert_id: function(Handle: PMYSQL): ULongLong; stdcall;
    mysql_kill: function(Handle: PMYSQL; Pid: ULong): Integer; stdcall;
    mysql_list_dbs: function(Handle: PMYSQL; Wild: PAnsiChar): PMYSQL_RES; stdcall;
    mysql_list_fields: function(Handle: PMYSQL; const Table, Wild: PAnsiChar): PMYSQL_RES; stdcall;
    mysql_list_processes: function(Handle: PMYSQL): PMYSQL_RES; stdcall;
    mysql_list_tables: function(Handle: PMYSQL; const Wild: PAnsiChar): PMYSQL_RES; stdcall;
    mysql_num_fields: function(Result: PMYSQL_RES): UInt; stdcall;
    mysql_num_rows: function(Result: PMYSQL_RES): ULongLong; stdcall;
    mysql_options: function(Handle: PMYSQL; Option: TMySqlOption; const Arg: PAnsiChar): Integer; stdcall;
    mysql_ping: function(Handle: PMYSQL): Integer; stdcall;
    mysql_query: function(Handle: PMYSQL; const Query: PAnsiChar): Integer; stdcall; {deprecated for mysql_real_query}
    mysql_real_connect: function(Handle: PMYSQL; const Host, User, Passwd, Db: PAnsiChar; Port: UInt; const UnixSocket: PAnsiChar; ClientFlag: ULong): PMYSQL; stdcall;
    mysql_real_escape_string: function(Handle: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: ULong): ULong; stdcall;
    mysql_real_query: function(Handle: PMYSQL; const Query: PAnsiChar; Length: ULong): Integer; stdcall;
    mysql_refresh: function(Handle: PMYSQL; Options: UInt): Integer; stdcall;
    mysql_row_seek: function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS; stdcall;
    mysql_row_tell: function(Result: PMYSQL_RES): PMYSQL_ROWS; stdcall;
    mysql_select_db: function(Handle: PMYSQL; const Db: PAnsiChar): Integer; stdcall;
    mysql_ssl_set: function(Handle: PMYSQL; const key, cert, CA, CApath, cipher: PAnsiChar): Byte; stdcall;
    mysql_stat: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_store_result: function(Handle: PMYSQL): PMYSQL_RES; stdcall;
    mysql_thread_id: function(Handle: PMYSQL): ULong; stdcall;
    mysql_use_result: function(Handle: PMYSQL): PMYSQL_RES; stdcall;
    { Set up and bring down a thread; these function should be called for each thread in an application which
    opens at least one MySQL connection. All uses of the connection(s) should be between these function calls. }
    my_init: procedure; stdcall;
    mysql_thread_init: function: Byte; stdcall;
    mysql_thread_end: procedure; stdcall;
    mysql_thread_safe: function: UInt; stdcall;
    { Set up and bring down the server; to ensure that applications will work when linked against either the
    standard client library or the embedded server library, these functions should be called. }
    mysql_library_init: function(Argc: Integer; Argv, Groups: Pointer): Integer; stdcall;
    mysql_library_end: procedure; stdcall;
    mysql_server_init: function(Argc: Integer; Argv, Groups: Pointer): Integer; stdcall;
    mysql_server_end: procedure; stdcall;
    mysql_change_user: function(mysql: PMYSQL; const user: PAnsiChar; const passwd: PAnsiChar; const db: PAnsiChar): Byte;
    mysql_field_count: function(Handle: PMYSQL): UInt; stdcall;
    mysql_get_client_version: function: ULong; stdcall;
    mysql_send_query: function(mysql: PMYSQL; const query: PAnsiChar; length: ULong): Integer; stdcall;
    mysql_read_query_result: function(mysql: PMYSQL): Byte; stdcall;
    mysql_autocommit: function(Handle: PMYSQL; const mode: Byte): Byte; stdcall;
    mysql_commit: function(Handle: PMYSQL): Byte; stdcall;
    mysql_get_server_version: function(Handle: PMYSQL): ULong; stdcall;
    mysql_hex_string: function(PTo, PFrom: PAnsiChar; Len: ULong): ULong; stdcall;
    mysql_more_results: function(Handle: PMYSQL): Byte; stdcall;
    mysql_next_result: function(Handle: PMYSQL): Integer; stdcall;
    mysql_rollback: function(Handle: PMYSQL): Byte; stdcall;
    mysql_set_character_set: function(Handle: PMYSQL; csname: PAnsiChar): Integer; stdcall;
    mysql_set_server_option: function(Handle: PMYSQL; Option: TMysqlSetOption): Integer; stdcall;
    mysql_shutdown: function(Handle: PMYSQL; shutdown_level: TMysqlShutdownLevel): Integer; stdcall; {new argument 4.1}
    mysql_sqlstate: function(Handle: PMYSQL): PAnsiChar; stdcall;
    mysql_warning_count: function(Handle: PMYSQL): UInt; stdcall;
    {BELOW are new PREPARED STATEMENTS}
    mysql_stmt_affected_rows: function(stmt: PMYSQL_STMT): ULongLong; stdcall;
    mysql_stmt_attr_get: function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; arg: PAnsiChar): Byte; stdcall;
    mysql_stmt_attr_set: function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; const arg: PAnsiChar): Byte; stdcall; {augmented 5.0.2/6}
    mysql_stmt_bind_param: function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte; stdcall;
    mysql_stmt_bind_result: function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte; stdcall;
    mysql_stmt_close: function(stmt: PMYSQL_STMT): Byte; stdcall;
    mysql_stmt_data_seek: procedure(stmt: PMYSQL_STMT; offset: ULongLong); stdcall;
    mysql_stmt_errno: function(stmt: PMYSQL_STMT): UInt; stdcall;
    mysql_stmt_error: function(stmt: PMYSQL_STMT): PAnsiChar; stdcall;
    mysql_stmt_execute: function(stmt: PMYSQL_STMT): Integer; stdcall;
    mysql_stmt_fetch: function(stmt: PMYSQL_STMT): Integer; stdcall;
    mysql_stmt_fetch_column: function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}; column: UInt; offset: ULong): Integer; stdcall;
    mysql_stmt_field_count: function(stmt: PMYSQL_STMT): UInt; stdcall;
    mysql_stmt_free_result: function(stmt: PMYSQL_STMT): Byte; stdcall;
    mysql_stmt_init: function(Handle: PMYSQL): PMYSQL_STMT; stdcall;
    mysql_stmt_insert_id: function(stmt: PMYSQL_STMT): ULongLong; stdcall;
    mysql_stmt_next_result: function(stmt: PMYSQL_STMT): Integer; stdcall;
    mysql_stmt_num_rows: function(stmt: PMYSQL_STMT): ULongLong; stdcall;
    mysql_stmt_param_count: function(stmt: PMYSQL_STMT): ULong; stdcall;
    mysql_stmt_param_metadata: function(stmt: PMYSQL_STMT): PMYSQL_RES; stdcall;
    mysql_stmt_prepare: function(stmt: PMYSQL_STMT; const query: PAnsiChar; length: ULong): Integer; stdcall;
    mysql_stmt_reset: function(stmt: PMYSQL_STMT): Byte; stdcall;
    mysql_stmt_result_metadata: function(stmt: PMYSQL_STMT): PMYSQL_RES; stdcall;
    mysql_stmt_row_seek: function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS; stdcall;
    mysql_stmt_row_tell: function(stmt: PMYSQL_STMT): PMYSQL_ROWS; stdcall;
    mysql_stmt_send_long_data: function(stmt: PMYSQL_STMT; parameter_number: UInt; const data: PAnsiChar; length: ULong): Byte; stdcall;
    mysql_stmt_sqlstate: function(stmt: PMYSQL_STMT): PAnsiChar; stdcall;
    mysql_stmt_store_result: function(stmt: PMYSQL_STMT): Integer; stdcall;
    mysql_get_character_set_info: procedure(Handle: PMYSQL; cs: PMY_CHARSET_INFO); stdcall;
    constructor Create(ApiVer: TALMySqlVersion_API); virtual;
    destructor Destroy; override;
    function Loaded: Boolean; virtual;
    function Unload: Boolean; virtual;
    function Load(const lib: AnsiString = 'libmysql.dll'): Boolean; virtual;
  end;

implementation

Uses
  System.sysutils;

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

{***************************************}
function TALMySqlLibrary.Unload: Boolean;
begin
  Result := True;
  if Loaded then begin
    mysql_library_end; // Should be called before your program exits.
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
    mysql_library_init := nil;
    mysql_library_end := nil;
    mysql_server_init := nil;
    mysql_server_end := nil;
    mysql_change_user := nil;
    mysql_field_count := nil;
    mysql_get_client_version := nil;
    mysql_send_query := nil;
    mysql_read_query_result := nil;
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
    mysql_stmt_next_result := nil;
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

{*****************************************************************************}
function TALMySqlLibrary.Load(const lib: AnsiString = 'libmysql.dll'): Boolean;
Begin
  Result := Loaded;
  if not Result then begin
    Flibmysql := LoadLibraryA(PAnsiChar(lib));
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

      //in mysql.h of MySQL50
      //#define mysql_library_init mysql_server_init
      //#define mysql_library_end mysql_server_end
      mysql_library_init := GetProcAddress(Flibmysql,'mysql_server_init');
      mysql_library_end := GetProcAddress(Flibmysql,'mysql_server_end');

      mysql_server_init := GetProcAddress(Flibmysql,'mysql_server_init');
      mysql_server_end := GetProcAddress(Flibmysql,'mysql_server_end');
      mysql_change_user := GetProcAddress(Flibmysql,'mysql_change_user');
      mysql_field_count := GetProcAddress(Flibmysql,'mysql_field_count');
      mysql_get_client_version := GetProcAddress(Flibmysql,'mysql_get_client_version');
      mysql_send_query := GetProcAddress(Flibmysql,'mysql_send_query');
      mysql_read_query_result := GetProcAddress(Flibmysql,'mysql_read_query_result');
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
      mysql_stmt_next_result := GetProcAddress(Flibmysql,'mysql_stmt_next_result');
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
                (
                 (FVersion_API in [MYSQL55]) or
                  assigned(my_init)
                )
                and
                assigned(mysql_thread_init) and
                assigned(mysql_thread_end) and
                assigned(mysql_thread_safe) and
                assigned(mysql_library_init) and
                assigned(mysql_library_end) and
                assigned(mysql_server_init) and
                assigned(mysql_server_end) and
                assigned(mysql_change_user) and
                assigned(mysql_field_count) and
                assigned(mysql_get_client_version) and
                assigned(mysql_send_query) and
                assigned(mysql_read_query_result) and
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
                (
                 (FVersion_API = MYSQL50) or
                 assigned(mysql_stmt_next_result)
                )
                and
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
        raise Exception.Create(cALMySql_INVALIDELIBVERSION);
      end
      else begin
        // Should be called before any other MySQL function
        if mysql_library_init(0,nil,nil) <> 0 then begin
          Unload;
          raise Exception.CreateFmt(cALMySql_CANTLOADLIB, [lib]);
        end;
      end;
    end
    else raise Exception.CreateFmt(cALMySql_CANTLOADLIB, [lib]);
  end;
end;

end.


