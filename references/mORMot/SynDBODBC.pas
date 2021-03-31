/// ODBC 3.x library direct access classes to be used with our SynDB architecture
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBODBC;

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
  - Esteban Martin (EMartin)
  - squirrel
  - zed

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

  TODO:
  - implement array binding of parameters
    http://msdn.microsoft.com/en-us/library/windows/desktop/ms709287
  - implement row-wise binding when all columns are inlined
    http://msdn.microsoft.com/en-us/library/windows/desktop/ms711730
}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  SysUtils,
  {$ifndef DELPHI5OROLDER}
  Variants,
  {$endif}
  {$ifdef FPC}
  dynlibs,
  {$endif}
  Classes,
  SynCommons,
  SynLog,
  SynTable,
  SynDB;


{ -------------- TODBC* classes and types implementing an ODBC library connection  }

type
  /// generic Exception type, generated for ODBC connection
  EODBCException = class(ESQLDBException);

  /// will implement properties shared by the ODBC library
  TODBCConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  protected
    fDriverDoesNotHandleUnicode: Boolean;
    fSQLDriverConnectPrompt: Boolean;
    /// this overridden method will hide de DATABASE/PWD fields in ODBC connection string
    function GetDatabaseNameSafe: RawUTF8; override;
    /// this overridden method will retrieve the kind of DBMS from the main connection
    function GetDBMS: TSQLDBDefinition; override;
  public
    /// initialize the connection properties
    // - will raise an exception if the ODBC library is not available
    // - SQLConnect() API will be used if aServerName is set: it should contain
    // the ODBC Data source name as defined in "ODBC Data Source Administrator"
    // tool (C:\Windows\SysWOW64\odbcad32.exe for 32bit app on Win64) - in this
    // case, aDatabaseName will be ignored
    // - SQLDriverConnect() API will be used if aServerName is '' and
    // aDatabaseName is set - in this case, aDatabaseName should contain a
    // full connection string like (e.g. for a local SQLEXPRESS instance):
    // ! 'DRIVER=SQL Server Native Client 10.0;UID=.;server=.\SQLEXPRESS;'+
    // !   'Trusted_Connection=Yes;MARS_Connection=yes'
    // see @http://msdn.microsoft.com/en-us/library/ms715433
    // or when using Firebird ODBC:
    // ! 'DRIVER=Firebird/InterBase(r) driver;CHARSET=UTF8;UID=SYSDBA;PWD=masterkey;'
    // !   'DBNAME=MyServer/3051:C:\database\myData.fdb'
    // ! 'DRIVER=Firebird/InterBase(r) driver;CHARSET=UTF8;DBNAME=dbfile.fdb;'+
    // !   'CLIENT=fbembed.dll'
    // for IBM DB2 and its official driver:
    // !  'Driver=IBM DB2 ODBC DRIVER;Database=SAMPLE;'+
    // !    'Hostname=localhost;Port=50000;UID=db2admin;Pwd=db2Password'
    // for PostgreSQL - driver from http://ftp.postgresql.org/pub/odbc/versions/msi
    // ! 'Driver=PostgreSQL Unicode;Database=postgres;'+
    // !   'Server=localhost;Port=5432;UID=postgres;Pwd=postgresPassword'
    // for MySQL - driver from https://dev.mysql.com/downloads/connector/odbc
    // (note: 5.2.6 and 5.3.1 driver seems to be slow in ODBC.FreeHandle)
    // ! 'Driver=MySQL ODBC 5.2 UNICODE Driver;Database=test;'+
    // !   'Server=localhost;Port=3306;UID=root;Pwd='
    // for IBM Informix and its official driver:
    // ! 'Driver=IBM INFORMIX ODBC DRIVER;Database=SAMPLE;'+
    // !   'Host=localhost;Server=<instance name on host>;Service=<service name
    // !   in ../drivers/etc/services>;Protocol=olsoctcp;UID=<Windows/Linux user account>;
    // !   Pwd=<Windows/Linux user account password>'
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    // - this overridden method will create an TODBCConnection instance
    function NewConnection: TSQLDBConnection; override;
    /// get all table names
    // - will retrieve the corresponding metadata from ODBC library if SQL
    // direct access was not defined
    procedure GetTableNames(out Tables: TRawUTF8DynArray); override;
    /// get all view names
    // - will retrieve the corresponding metadata from ODBC library if SQL
    // direct access was not defined
    procedure GetViewNames(out Views: TRawUTF8DynArray); override;
    /// retrieve the column/field layout of a specified table
    // - will also check if the columns are indexed
    // - will retrieve the corresponding metadata from ODBC library if SQL
    // direct access was not defined (e.g. for dDB2)
    procedure GetFields(const aTableName: RawUTF8; out Fields: TSQLDBColumnDefineDynArray); override;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
    /// retrieve a list of stored procedure names from current connection
    procedure GetProcedureNames(out Procedures: TRawUTF8DynArray); override;
    /// retrieve procedure input/output parameter information
    // - aProcName: stored procedure name to retrieve parameter infomation.
    // - Parameters: parameter list info (name, datatype, direction, default)
    procedure GetProcedureParameters(const aProcName: RawUTF8; out Parameters: TSQLDBProcColumnDefineDynArray); override;
    /// if full connection string may prompt the user for additional information
    // - property used only with SQLDriverConnect() API (i.e. when aServerName
    // is '' and aDatabaseName contains a full connection string)
    // - set to TRUE to allow UI prompt if needed
    property SQLDriverConnectPrompt: boolean read fSQLDriverConnectPrompt
      write fSQLDriverConnectPrompt;
  end;

  /// implements a direct connection to the ODBC library
  TODBCConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fODBCProperties: TODBCConnectionProperties;
    fEnv: pointer;
    fDbc: pointer;
    fDBMS: TSQLDBDefinition;
    fDBMSName, fDriverName, fDBMSVersion, fSQLDriverFullString: RawUTF8;
  public
    /// connect to a specified ODBC database
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the ODBC library, i.e. create the DB instance
    // - should raise an Exception on error
    procedure Connect; override;
    /// stop connection to the ODBC library, i.e. release the DB instance
    // - should raise an Exception on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;

    /// initialize a new SQL query statement for the given connection
    // - the caller should free the instance after use
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    // - current implementation do not support nested transaction with those
    // methods: exception will be raised in such case
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;

    /// the remote DBMS type, as retrieved at ODBC connection opening
    property DBMS: TSQLDBDefinition read fDBMS;
    /// the full connection string (expanded from ServerName)
    property SQLDriverFullString: RawUTF8 read fSQLDriverFullString;
  published
    /// the remote DBMS name, as retrieved at ODBC connection opening
    property DBMSName: RawUTF8 read fDBMSName;
    /// the remote DBMS version, as retrieved at ODBC connection opening
    property DBMSVersion: RawUTF8 read fDBMSVersion;
    /// the local driver name, as retrieved at ODBC connection opening
    property DriverName: RawUTF8 read fDriverName;
  end;

  /// implements a statement using a ODBC connection
  TODBCStatement = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fStatement: pointer;
    fColData: TRawByteStringDynArray;
    fSQLW: RawUnicode;
    procedure AllocStatement;
    procedure DeallocStatement;
    procedure BindColumns;
    procedure GetData(var Col: TSQLDBColumnProperty; ColIndex: integer);
    function GetCol(Col: integer; ExpectedType: TSQLDBFieldType): TSQLDBStatementGetCol;
    function MoreResults: boolean;
  public
    /// create a ODBC statement instance, from an existing ODBC connection
    // - the Execute method can be called once per TODBCStatement instance,
    //   but you can use the Prepare once followed by several ExecutePrepared methods
    // - if the supplied connection is not of TOleDBConnection type, will raise
    //   an exception
    constructor Create(aConnection: TSQLDBConnection); override;
    // release all associated memory and ODBC handles
    destructor Destroy; override;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    //   to retrieve the data rows
    // - raise an EODBCException or ESQLDBException on any error
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: Boolean=false); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this overridden method will log the SQL statement if sllSQL has been
    //   enabled in SynDBLog.Family.Level
    // - raise an EODBCException or ESQLDBException on any error
    procedure ExecutePrepared; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an EODBCException on any error
    procedure Reset; override;

    /// After a statement has been prepared via Prepare() + ExecutePrepared() or
    //   Execute(), this method must be called one or more times to evaluate it
    // - you shall call this method before calling any Column*() methods
    // - return TRUE on success, with data ready to be retrieved by Column*()
    // - return FALSE if no more row is available (e.g. if the SQL statement
    //  is not a SELECT but an UPDATE or INSERT command)
    // - access the first or next row of data from the SQL Statement result:
    //   if SeekFirst is TRUE, will put the cursor on the first row of results,
    //   otherwise, it will fetch one row of data, to be called within a loop
    // - raise an EODBCException or ESQLDBException exception on any error
    function Step(SeekFirst: boolean=false): boolean; override;
    /// close the ODBC statement cursor resources
    procedure ReleaseRows; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    // - should retrieve directly the 64 bit Currency content, to avoid
    // any rounding/conversion error from floating-point types
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUTF8(Col: integer): RawUTF8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    // - ColumnBlob() will return the binary content of the field is was not ftBlob,
    //  e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
    //  or a direct mapping of the RawUnicode
    function ColumnBlob(Col: integer): RawByteString; override;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - fast overridden implementation with no temporary variable
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure ColumnsToJSON(WR: TJSONWriter); override;
    /// returns the number of rows updated by the execution of this statement
    function UpdateCount: integer; override;
  end;

{$ifdef MSWINDOWS}
/// List all ODBC drivers installed, by reading the Windows Registry
// - aDrivers is the output driver list container, which should be either nil (to
// create a new TStringList), or any existing TStrings instance (may be from VCL
// - aIncludeVersion: include the DLL driver version as <driver name>=<dll version>
// in aDrivers (somewhat slower)
function ODBCInstalledDriversList(const aIncludeVersion: Boolean; var aDrivers: TStrings): boolean;
{$endif MSWINDOWS}


implementation

{$ifdef MSWINDOWS}
uses
  Registry;
{$endif MSWINDOWS}

{ -------------- ODBC library interfaces, constants and types }

const
  SQL_NULL_DATA = -1;
  SQL_DATA_AT_EXEC = -2;
  SQL_NO_TOTAL = -4;

  // return values from functions
  SQL_SUCCESS = 0;
  SQL_SUCCESS_WITH_INFO = 1;

  SQL_NO_DATA = 100;

  SQL_PARAM_TYPE_UNKNOWN = 0;
  SQL_PARAM_INPUT = 1;
  SQL_PARAM_INPUT_OUTPUT = 2;
  SQL_RESULT_COL = 3;
  SQL_PARAM_OUTPUT = 4;
  SQL_RETURN_VALUE = 5;
  SQL_PARAM_DATA_AVAILABLE = 101;

  SQL_ERROR = (-1);
  SQL_INVALID_HANDLE = (-2);

  SQL_STILL_EXECUTING = 2;
  SQL_NEED_DATA = 99;

  // flags for null-terminated string
  SQL_NTS = (-3);
  SQL_NTSL = (-3);

  // maximum message length
  SQL_MAX_MESSAGE_LENGTH = 512;

  // date/time length constants
  SQL_DATE_LEN = 10;
  // add P+1 if precision is nonzero
  SQL_TIME_LEN = 8;
  // add P+1 if precision is nonzero
  SQL_TIMESTAMP_LEN = 19;

  // handle type identifiers
  SQL_HANDLE_ENV = 1;
  SQL_HANDLE_DBC = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

  // env attribute
  SQL_ATTR_ODBC_VERSION = 200;
  SQL_ATTR_CONNECTION_POOLING = 201;
  SQL_ATTR_CP_MATCH = 202;
  SQL_ATTR_OUTPUT_NTS = 10001;
  SQL_OV_ODBC3 = pointer(3);

  // values for SQLStatistics()
  SQL_INDEX_UNIQUE = 0;
  SQL_INDEX_ALL = 1;
  SQL_QUICK = 0;
  SQL_ENSURE = 1;

  // connection attributes
  SQL_ACCESS_MODE = 101;
  SQL_AUTOCOMMIT = 102;
  SQL_LOGIN_TIMEOUT = 103;
  SQL_OPT_TRACE = 104;
  SQL_OPT_TRACEFILE = 105;
  SQL_TRANSLATE_DLL = 106;
  SQL_TRANSLATE_OPTION = 107;
  SQL_TXN_ISOLATION = 108;
  SQL_CURRENT_QUALIFIER = 109;
  SQL_ODBC_CURSORS = 110;
  SQL_QUIET_MODE = 111;
  SQL_PACKET_SIZE = 112;
  SQL_ATTR_AUTO_IPD = 10001;
  SQL_ATTR_METADATA_ID = 10014;

  // statement attributes
  SQL_ATTR_APP_ROW_DESC = 10010;
  SQL_ATTR_APP_PARAM_DESC = 10011;
  SQL_ATTR_IMP_ROW_DESC = 10012;
  SQL_ATTR_IMP_PARAM_DESC = 10013;
  SQL_ATTR_CURSOR_SCROLLABLE = (-1);
  SQL_ATTR_CURSOR_SENSITIVITY = (-2);

  // SQL_ATTR_CURSOR_SCROLLABLE values
  SQL_NONSCROLLABLE = 0;
  SQL_SCROLLABLE = 1;

	// SQL_AUTOCOMMIT options
  SQL_AUTOCOMMIT_OFF = pointer(0);
  SQL_AUTOCOMMIT_ON = pointer(1);

  // identifiers of fields in the SQL descriptor
  SQL_DESC_COUNT = 1001;
  SQL_DESC_TYPE = 1002;
  SQL_DESC_LENGTH = 1003;
  SQL_DESC_OCTET_LENGTH_PTR = 1004;
  SQL_DESC_PRECISION = 1005;
  SQL_DESC_SCALE = 1006;
  SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
  SQL_DESC_NULLABLE = 1008;
  SQL_DESC_INDICATOR_PTR = 1009;
  SQL_DESC_DATA_PTR = 1010;
  SQL_DESC_NAME = 1011;
  SQL_DESC_UNNAMED = 1012;
  SQL_DESC_OCTET_LENGTH = 1013;
  SQL_DESC_ALLOC_TYPE = 1099;

  // identifiers of fields in the diagnostics area
  SQL_DIAG_RETURNCODE = 1;
  SQL_DIAG_NUMBER = 2;
  SQL_DIAG_ROW_COUNT = 3;
  SQL_DIAG_SQLSTATE = 4;
  SQL_DIAG_NATIVE = 5;
  SQL_DIAG_MESSAGE_TEXT = 6;
  SQL_DIAG_DYNAMIC_FUNCTION = 7;
  SQL_DIAG_CLASS_ORIGIN = 8;
  SQL_DIAG_SUBCLASS_ORIGIN = 9;
  SQL_DIAG_CONNECTION_NAME = 10;
  SQL_DIAG_SERVER_NAME = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

  // SQL data type codes
  SQL_UNKNOWN_TYPE = 0;
  SQL_CHAR = 1;
  SQL_NUMERIC = 2;
  SQL_DECIMAL = 3;
  SQL_INTEGER = 4;
  SQL_SMALLINT = 5;
  SQL_FLOAT = 6;
  SQL_REAL = 7;
  SQL_DOUBLE = 8;
  SQL_DATETIME = 9;
  SQL_DATE = 9;
  SQL_INTERVAL = 10;
  SQL_TIME = 10;
  SQL_TIMESTAMP = 11;
  SQL_VARCHAR = 12;
  SQL_LONGVARCHAR = -1;
  SQL_BINARY = -2;
  SQL_VARBINARY = -3;
  SQL_LONGVARBINARY = -4;
  SQL_BIGINT = -5;
  SQL_TINYINT = -6;
  SQL_BIT = -7;
  SQL_WCHAR = -8;
  SQL_WVARCHAR = -9;
  SQL_WLONGVARCHAR = -10;
  SQL_GUID = -11;

  // One-parameter shortcuts for date/time data types
  SQL_TYPE_DATE = 91;
  SQL_TYPE_TIME = 92;
  SQL_TYPE_TIMESTAMP = 93;

  // C datatype to SQL datatype mapping
  SQL_C_CHAR = SQL_CHAR;
  SQL_C_WCHAR = SQL_WCHAR;
  SQL_C_LONG = SQL_INTEGER;
  SQL_C_SHORT = SQL_SMALLINT;
  SQL_C_FLOAT = SQL_REAL;
  SQL_C_DOUBLE = SQL_DOUBLE;
  SQL_C_NUMERIC = SQL_NUMERIC;
  SQL_C_DEFAULT = 99;
  SQL_SIGNED_OFFSET = (-20);
  SQL_UNSIGNED_OFFSET = (-22);
  SQL_C_DATE = SQL_DATE;
  SQL_C_TIME = SQL_TIME;
  SQL_C_TIMESTAMP = SQL_TIMESTAMP;
  SQL_C_TYPE_DATE = SQL_TYPE_DATE;
  SQL_C_TYPE_TIME = SQL_TYPE_TIME;
  SQL_C_TYPE_TIMESTAMP = SQL_TYPE_TIMESTAMP;
  SQL_C_BINARY = SQL_BINARY;
  SQL_C_BIT = SQL_BIT;
  SQL_C_SBIGINT = (SQL_BIGINT+SQL_SIGNED_OFFSET);
  SQL_C_UBIGINT = (SQL_BIGINT+SQL_UNSIGNED_OFFSET);
  SQL_C_TINYINT = SQL_TINYINT;
  SQL_C_SLONG = (SQL_C_LONG+SQL_SIGNED_OFFSET);
  SQL_C_SSHORT = (SQL_C_SHORT+SQL_SIGNED_OFFSET);
  SQL_C_STINYINT = (SQL_TINYINT+SQL_SIGNED_OFFSET);
  SQL_C_ULONG = (SQL_C_LONG+SQL_UNSIGNED_OFFSET);
  SQL_C_USHORT = (SQL_C_SHORT+SQL_UNSIGNED_OFFSET);
  SQL_C_UTINYINT = (SQL_TINYINT+SQL_UNSIGNED_OFFSET);

  // Driver specific SQL data type defines.
  // Microsoft has -150 thru -199 reserved for Microsoft SQL Server Native Client driver usage.
  SQL_SS_VARIANT = (-150);
  SQL_SS_UDT = (-151);
  SQL_SS_XML = (-152);
  SQL_SS_TABLE = (-153);
  SQL_SS_TIME2 = (-154);
  SQL_SS_TIMESTAMPOFFSET = (-155);

  // Statement attribute values for cursor sensitivity
  SQL_UNSPECIFIED = 0;
  SQL_INSENSITIVE = 1;
  SQL_SENSITIVE = 2;

  // GetTypeInfo() request for all data types
  SQL_ALL_TYPES = 0;

  // Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData()
  SQL_DEFAULT = 99;

  // SQLSQLLEN GetData() code indicating that the application row descriptor
  // specifies the data type
  SQL_ARD_TYPE = (-99);

  SQL_APD_TYPE = (-100);

  // SQL date/time type subcodes
  SQL_CODE_DATE = 1;
  SQL_CODE_TIME = 2;
  SQL_CODE_TIMESTAMP = 3;

  // CLI option values
  SQL_FALSE = 0;
  SQL_TRUE = 1;

  // values of NULLABLE field in descriptor
  SQL_NO_NULLS = 0;
  SQL_NULLABLE = 1;

  // Value returned by SQLGetTypeInfo() to denote that it is
  // not known whether or not a data type supports null values.
  SQL_NULLABLE_UNKNOWN = 2;

  // Values returned by SQLGetTypeInfo() to show WHERE clause supported
  SQL_PRED_NONE = 0;
  SQL_PRED_CHAR = 1;
  SQL_PRED_BASIC = 2;

  // values of UNNAMED field in descriptor
  SQL_NAMED = 0;
  SQL_UNNAMED = 1;

  // values of ALLOC_TYPE field in descriptor
  SQL_DESC_ALLOC_AUTO = 1;
  SQL_DESC_ALLOC_USER = 2;

  // FreeStmt() options
  SQL_CLOSE = 0;
  SQL_DROP = 1;
  SQL_UNBIND = 2;
  SQL_RESET_PARAMS = 3;

  // Codes used for FetchOrientation in SQLFetchScroll() and SQLDataSources()
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;

  // Other codes used for FetchOrientation in SQLFetchScroll()
  SQL_FETCH_LAST = 3;
  SQL_FETCH_PRIOR = 4;
  SQL_FETCH_ABSOLUTE = 5;
  SQL_FETCH_RELATIVE = 6;

  // SQLEndTran() options
  SQL_COMMIT = 0;
  SQL_ROLLBACK = 1;

  // null handles returned by SQLAllocHandle()
  SQL_NULL_HENV = 0;
  SQL_NULL_HDBC = 0;
  SQL_NULL_HSTMT = 0;
  SQL_NULL_HDESC = 0;

  // null handle used in place of parent handle when allocating HENV
  SQL_NULL_HANDLE = nil;

  // Information requested by SQLGetInfo()
  SQL_MAX_DRIVER_CONNECTIONS = 0;
  SQL_MAXIMUM_DRIVER_CONNECTIONS = SQL_MAX_DRIVER_CONNECTIONS;
  SQL_MAX_CONCURRENT_ACTIVITIES = 1;
  SQL_MAXIMUM_CONCURRENT_ACTIVITIES = SQL_MAX_CONCURRENT_ACTIVITIES;
  SQL_DATA_SOURCE_NAME = 2;
  SQL_FETCH_DIRECTION = 8;
  SQL_SERVER_NAME = 13;
  SQL_SEARCH_PATTERN_ESCAPE = 14;
  SQL_DRIVER_NAME = 6;
  SQL_DBMS_NAME = 17;
  SQL_DBMS_VER = 18;
  SQL_ACCESSIBLE_TABLES = 19;
  SQL_ACCESSIBLE_PROCEDURES = 20;
  SQL_CURSOR_COMMIT_BEHAVIOR = 23;
  SQL_DATA_SOURCE_READ_ONLY = 25;
  SQL_DEFAULT_TXN_ISOLATION = 26;
  SQL_IDENTIFIER_CASE = 28;
  SQL_IDENTIFIER_QUOTE_CHAR = 29;
  SQL_MAX_COLUMN_NAME_LEN = 30;
  SQL_MAXIMUM_COLUMN_NAME_LENGTH = SQL_MAX_COLUMN_NAME_LEN;
  SQL_MAX_CURSOR_NAME_LEN = 31;
  SQL_MAXIMUM_CURSOR_NAME_LENGTH = SQL_MAX_CURSOR_NAME_LEN;
  SQL_MAX_SCHEMA_NAME_LEN = 32;
  SQL_MAXIMUM_SCHEMA_NAME_LENGTH = SQL_MAX_SCHEMA_NAME_LEN;
  SQL_MAX_CATALOG_NAME_LEN = 34;
  SQL_MAXIMUM_CATALOG_NAME_LENGTH = SQL_MAX_CATALOG_NAME_LEN;
  SQL_MAX_TABLE_NAME_LEN = 35;
  SQL_SCROLL_CONCURRENCY = 43;
  SQL_TXN_CAPABLE = 46;
  SQL_TRANSACTION_CAPABLE = SQL_TXN_CAPABLE;
  SQL_USER_NAME = 47;
  SQL_TXN_ISOLATION_OPTION = 72;
  SQL_TRANSACTION_ISOLATION_OPTION = SQL_TXN_ISOLATION_OPTION;
  SQL_INTEGRITY = 73;
  SQL_GETDATA_EXTENSIONS = 81;
  SQL_NULL_COLLATION = 85;
  SQL_ALTER_TABLE = 86;
  SQL_ORDER_BY_COLUMNS_IN_SELECT = 90;
  SQL_SPECIAL_CHARACTERS = 94;
  SQL_MAX_COLUMNS_IN_GROUP_BY = 97;
  SQL_MAXIMUM_COLUMNS_IN_GROUP_BY = SQL_MAX_COLUMNS_IN_GROUP_BY;
  SQL_MAX_COLUMNS_IN_INDEX = 98;
  SQL_MAXIMUM_COLUMNS_IN_INDEX = SQL_MAX_COLUMNS_IN_INDEX;
  SQL_MAX_COLUMNS_IN_ORDER_BY = 99;
  SQL_MAXIMUM_COLUMNS_IN_ORDER_BY = SQL_MAX_COLUMNS_IN_ORDER_BY;
  SQL_MAX_COLUMNS_IN_SELECT = 100;
  SQL_MAXIMUM_COLUMNS_IN_SELECT = SQL_MAX_COLUMNS_IN_SELECT;
  SQL_MAX_COLUMNS_IN_TABLE = 101;
  SQL_MAX_INDEX_SIZE = 102;
  SQL_MAXIMUM_INDEX_SIZE = SQL_MAX_INDEX_SIZE;
  SQL_MAX_ROW_SIZE = 104;
  SQL_MAXIMUM_ROW_SIZE = SQL_MAX_ROW_SIZE;
  SQL_MAX_STATEMENT_LEN = 105;
  SQL_MAXIMUM_STATEMENT_LENGTH = SQL_MAX_STATEMENT_LEN;
  SQL_MAX_TABLES_IN_SELECT = 106;
  SQL_MAXIMUM_TABLES_IN_SELECT = SQL_MAX_TABLES_IN_SELECT;
  SQL_MAX_USER_NAME_LEN = 107;
  SQL_MAXIMUM_USER_NAME_LENGTH = SQL_MAX_USER_NAME_LEN;
  SQL_OJ_CAPABILITIES = 115;
  SQL_OUTER_JOIN_CAPABILITIES = SQL_OJ_CAPABILITIES;

  // Options for SQLDriverConnect
  SQL_DRIVER_NOPROMPT = 0;
  SQL_DRIVER_COMPLETE = 1;
  SQL_DRIVER_PROMPT = 2;
  SQL_DRIVER_COMPLETE_REQUIRED = 3;

  // SQLSetStmtAttr SQL Server Native Client driver specific defines.
  // Statement attributes
  SQL_SOPT_SS_BASE                      = 1225;
  SQL_SOPT_SS_TEXTPTR_LOGGING           = (SQL_SOPT_SS_BASE+0); // Text pointer logging
  SQL_SOPT_SS_CURRENT_COMMAND           = (SQL_SOPT_SS_BASE+1); // dbcurcmd SQLGetStmtOption only
  SQL_SOPT_SS_HIDDEN_COLUMNS            = (SQL_SOPT_SS_BASE+2); // Expose FOR BROWSE hidden columns
  SQL_SOPT_SS_NOBROWSETABLE             = (SQL_SOPT_SS_BASE+3); // Set NOBROWSETABLE option
  SQL_SOPT_SS_REGIONALIZE               = (SQL_SOPT_SS_BASE+4); // Regionalize output character conversions
  SQL_SOPT_SS_CURSOR_OPTIONS            = (SQL_SOPT_SS_BASE+5); // Server cursor options
  SQL_SOPT_SS_NOCOUNT_STATUS            = (SQL_SOPT_SS_BASE+6); // Real vs. Not Real row count indicator
  SQL_SOPT_SS_DEFER_PREPARE             = (SQL_SOPT_SS_BASE+7); // Defer prepare until necessary
  SQL_SOPT_SS_QUERYNOTIFICATION_TIMEOUT = (SQL_SOPT_SS_BASE+8); // Notification timeout
  SQL_SOPT_SS_QUERYNOTIFICATION_MSGTEXT = (SQL_SOPT_SS_BASE+9); // Notification message text
  SQL_SOPT_SS_QUERYNOTIFICATION_OPTIONS = (SQL_SOPT_SS_BASE+10);// SQL service broker name
  SQL_SOPT_SS_PARAM_FOCUS               = (SQL_SOPT_SS_BASE+11);// Direct subsequent calls to parameter related methods to set properties on constituent columns/parameters of container types
  SQL_SOPT_SS_NAME_SCOPE                = (SQL_SOPT_SS_BASE+12);// Sets name scope for subsequent catalog function calls
  SQL_SOPT_SS_MAX_USED                  = SQL_SOPT_SS_NAME_SCOPE;

  SQL_IS_POINTER                        = (-4);
  SQL_IS_UINTEGER                       = (-5);
  SQL_IS_INTEGER                        = (-6);
  SQL_IS_USMALLINT                      = (-7);
  SQL_IS_SMALLINT                       = (-8);

type
  SqlSmallint = Smallint;
  SqlDate = Byte;
  SqlTime = Byte;
  SqlDecimal = Byte;
  SqlDouble = Double;
  SqlFloat = Double;
  SqlInteger = integer;
  SqlUInteger = cardinal;
  SqlNumeric = Byte;
  SqlPointer = Pointer;
  SqlReal = Single;
  SqlUSmallint = Word;
  SqlTimestamp = Byte;
  SqlVarchar = Byte;
  PSqlSmallint = ^SqlSmallint;
  PSqlInteger = ^SqlInteger;

  SqlReturn = SqlSmallint;
  SqlLen = PtrInt;
  SqlULen = PtrUInt;
  {$ifdef CPU64}
  SqlSetPosIRow = PtrUInt;
  {$else}
  SqlSetPosIRow = Word;
  {$endif}
  PSqlLen = ^SqlLen;

  SqlHandle = Pointer;
  SqlHEnv = SqlHandle;
  SqlHDbc = SqlHandle;
  SqlHStmt = SqlHandle;
  SqlHDesc = SqlHandle;
  SqlHWnd = LongWord;

  {$A-}
  /// memory structure used to store SQL_C_TYPE_TIMESTAMP values
  {$ifdef USERECORDWITHMETHODS}SQL_TIMESTAMP_STRUCT = record
    {$else}SQL_TIMESTAMP_STRUCT = object{$endif}
    Year:     SqlSmallint;
    Month:    SqlUSmallint;
    Day:      SqlUSmallint;
    Hour:     SqlUSmallint;
    Minute:   SqlUSmallint;
    Second:   SqlUSmallint;
    Fraction: SqlUInteger;
    /// convert an ODBC date and time into Delphi TDateTime
    // - depending on the original column data type specified, it will return
    // either a TDate (for SQL_TYPE_DATE), either a TTime (for SQL_TYPE_TIME),
    // either a TDateTime content (for SQL_TYPE_TIMESTAMP)
    function ToDateTime(DataType: SqlSmallint=SQL_TYPE_TIMESTAMP): TDateTime;
    /// convert an ODBC date and time into its textual expanded ISO-8601
    // - will fill up to 21 characters, including double quotes
    // - depending on the column data type specified, it will return either an
    // ISO-8601 date (for SQL_TYPE_DATE), either a time (for SQL_TYPE_TIME),
    // either a full date+time ISO-8601 content (for SQL_TYPE_TIMESTAMP)
    function ToIso8601(Dest: PUTF8Char; DataType: SqlSmallint;
      WithMS: boolean=false): integer;
    /// convert a TDateTime into ODBC date or timestamp
    // - returns the corresponding C type, i.e. either SQL_C_TYPE_DATE,
    // either SQL_C_TYPE_TIMESTAMP and the corresponding size in bytes
    function From(DateTime: TDateTime; var ColumnSize: SqlLen): SqlSmallint;
  end;
  SQL_TIME_STRUCT	= record
    Hour:     SqlUSmallint;
    Minute:   SqlUSmallint;
    Second:   SqlUSmallint;
  end;
  SQL_DATE_STRUCT	= record
    year:	SQLSMALLINT;
    month:	SQLUSMALLINT;
    day:	SQLUSMALLINT;
  end;
  {$A+}
  PSQL_TIMESTAMP_STRUCT = ^SQL_TIMESTAMP_STRUCT;

  /// direct access to the ODBC library
  // - this wrapper will initialize both Ansi and Wide versions of the ODBC
  // driver functions, and will work with 32 bit and 64 bit version of the
  // interfaces, on Windows or POSIX platforms
  // - within this unit, we will only use Wide version, and UTF-8 conversion
  TODBCLib = class(TSQLDBLib)
  public
    AllocEnv: function (var EnvironmentHandle: SqlHEnv): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    AllocHandle: function(HandleType: SqlSmallint; InputHandle: SqlHandle;
      var OutputHandle: SqlHandle): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    AllocStmt: function(ConnectionHandle: SqlHDbc; var StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    BindCol: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      TargetType: SqlSmallint; TargetValue: SqlPointer;
      BufferLength: SqlLen; StrLen_or_Ind: PSqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    BindParameter: function (StatementHandle: SqlHStmt; ParameterNumber: SqlUSmallint;
      InputOutputType, ValueType, ParameterType: SqlSmallint; ColumnSize: SqlULen;
      DecimalDigits: SqlSmallint; ParameterValue: SqlPointer; BufferLength: SqlLen;
      var StrLen_or_Ind: SqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Cancel: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    CloseCursor: function(StatementHandle: SqlHStmt): SqlReturn;
     {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColAttributeA: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      FieldIdentifier: SqlUSmallint; CharacterAttribute: PAnsiChar;
      BufferLength: SqlSmallint; StringLength: PSqlSmallint; NumericAttributePtr: PSqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColAttributeW: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      FieldIdentifier: SqlUSmallint; CharacterAttribute: PWideChar;
      BufferLength: SqlSmallint; StringLength: PSqlSmallint; NumericAttributePtr: PSqlLen): SqlReturn;
       {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColumnsA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar;  NameLength2: SqlSmallint;
      TableName: PAnsiChar;   NameLength3: SqlSmallint;
      ColumnName: PAnsiChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ColumnsW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      TableName: PWideChar;   NameLength3: SqlSmallint;
      ColumnName: PWideChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    StatisticsA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar;  NameLength2: SqlSmallint;
      TableName: PAnsiChar;   NameLength3: SqlSmallint;
      Unique, Reserved: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    StatisticsW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      TableName: PWideChar;   NameLength3: SqlSmallint;
      Unique, Reserved: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ConnectA: function(ConnectionHandle: SqlHDbc;
      ServerName: PAnsiChar; NameLength1: SqlSmallint;
      UserName: PAnsiChar; NameLength2: SqlSmallint;
      Authentication: PAnsiChar; NameLength3: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ConnectW: function(ConnectionHandle: SqlHDbc;
      ServerName: PWideChar; NameLength1: SqlSmallint;
      UserName: PWideChar; NameLength2: SqlSmallint;
      Authentication: PWideChar; NameLength3: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    CopyDesc: function(SourceDescHandle, TargetDescHandle: SqlHDesc): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DataSourcesA: function(EnvironmentHandle: SqlHEnv; Direction: SqlUSmallint;
      ServerName: PAnsiChar;  BufferLength1: SqlSmallint; var NameLength1: SqlSmallint;
      Description: PAnsiChar; BufferLength2: SqlSmallint; var NameLength2: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DataSourcesW: function(EnvironmentHandle: SqlHEnv; Direction: SqlUSmallint;
      ServerName: PWideChar;  BufferLength1: SqlSmallint; var NameLength1: SqlSmallint;
      Description: PWideChar; BufferLength2: SqlSmallint; var NameLength2: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DescribeColA: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      ColumnName: PAnsiChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint;
      var DataType: SqlSmallint; var ColumnSize: SqlULen; var DecimalDigits: SqlSmallint;
      var Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    DescribeColW: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      ColumnName: PWideChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint;
      var DataType: SqlSmallint; var ColumnSize: SqlULen; var DecimalDigits: SqlSmallint;
      var Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Disconnect: function(ConnectionHandle: SqlHDbc): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    EndTran: function(HandleType: SqlSmallint; Handle: SqlHandle;
      CompletionType: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ErrorA: function(EnvironmentHandle: SqlHEnv; ConnectionHandle: SqlHDbc; StatementHandle: SqlHStmt;
      Sqlstate: PAnsiChar; var NativeError: SqlInteger;
      MessageText: PAnsiChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ErrorW: function(EnvironmentHandle: SqlHEnv; ConnectionHandle: SqlHDbc; StatementHandle: SqlHStmt;
      Sqlstate: PWideChar; var NativeError: SqlInteger;
      MessageText: PWideChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ExecDirectA: function(StatementHandle: SqlHStmt;
      StatementText: PAnsiChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ExecDirectW: function(StatementHandle: SqlHStmt;
      StatementText: PWideChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Execute: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    Fetch: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FetchScroll: function(StatementHandle: SqlHStmt;
      FetchOrientation: SqlSmallint; FetchOffset: SqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeConnect: function(ConnectionHandle: SqlHDbc): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeEnv: function(EnvironmentHandle: SqlHEnv): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeHandle: function(HandleType: SqlSmallint; Handle: SqlHandle): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    FreeStmt: function(StatementHandle: SqlHStmt; Option: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetConnectAttrA: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; BufferLength: SqlInteger; pStringLength: pSqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetConnectAttrW: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; BufferLength: SqlInteger; pStringLength: pSqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetCursorNameA: function(StatementHandle: SqlHStmt;
      CursorName: PAnsiChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetCursorNameW: function(StatementHandle: SqlHStmt;
      CursorName: PWideChar; BufferLength: SqlSmallint; var NameLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetData: function(StatementHandle: SqlHStmt; ColumnNumber: SqlUSmallint;
      TargetType: SqlSmallint; TargetValue: SqlPointer; BufferLength: SqlLen;
      StrLen_or_Ind: PSqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescFieldA: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      FieldIdentifier: SqlSmallint; Value: SqlPointer; BufferLength: SqlInteger;
      var StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescFieldW: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      FieldIdentifier: SqlSmallint; Value: SqlPointer; BufferLength: SqlInteger;
      var StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescRecA: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      Name: PAnsiChar; BufferLength: SqlSmallint; var StringLength: SqlSmallint;
      var _Type, SubType: SqlSmallint; var Length: SqlLen;
      var Precision, Scale, Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDescRecW: function(DescriptorHandle: SqlHDesc; RecNumber: SqlSmallint;
      Name: PWideChar; BufferLength: SqlSmallint; var StringLength: SqlSmallint;
      var _Type, SubType: SqlSmallint; var Length: SqlLen;
      var Precision, Scale, Nullable: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagFieldA: function(HandleType: SqlSmallint; Handle: SqlHandle;
      RecNumber, DiagIdentifier: SqlSmallint;
      DiagInfo: SqlPointer; BufferLength: SqlSmallint; var StringLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagFieldW: function(HandleType: SqlSmallint; Handle: SqlHandle;
      RecNumber, DiagIdentifier: SqlSmallint;
      DiagInfo: SqlPointer; BufferLength: SqlSmallint; var StringLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagRecA: function(HandleType: SqlSmallint; Handle: SqlHandle; RecNumber: SqlSmallint;
      Sqlstate: PAnsiChar; var NativeError: SqlInteger;
      MessageText: PAnsiChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetDiagRecW: function(HandleType: SqlSmallint; Handle: SqlHandle; RecNumber: SqlSmallint;
      Sqlstate: PWideChar; var NativeError: SqlInteger;
      MessageText: PWideChar; BufferLength: SqlSmallint; var TextLength: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    MoreResults: function(StatementHandle: SqlHStmt): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    PrepareA: function(StatementHandle: SqlHStmt;
      StatementText: PAnsiChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    PrepareW: function(StatementHandle: SqlHStmt;
      StatementText: PWideChar; TextLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    RowCount: function(StatementHandle: SqlHStmt; var RowCount: SqlLen): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    NumResultCols: function(StatementHandle: SqlHStmt; var ColumnCount: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetInfoA: function(ConnectionHandle: SqlHDbc; InfoType: SqlUSmallint;
      InfoValuePtr: SqlPointer; BufferLength: SqlSmallint; StringLengthPtr: PSqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    GetInfoW: function(ConnectionHandle: SqlHDbc; InfoType: SqlUSmallint;
      InfoValuePtr: SqlPointer; BufferLength: SqlSmallint; StringLengthPtr: PSqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetStmtAttrA: function(StatementHandle: SqlHStmt; Attribute: SqlInteger;
      Value: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetStmtAttrW: function(StatementHandle: SqlHStmt; Attribute: SqlInteger;
      Value: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetEnvAttr: function(EnvironmentHandle: SqlHEnv; Attribute: SqlInteger;
      ValuePtr: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetConnectAttrA: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SetConnectAttrW: function(ConnectionHandle: SqlHDbc; Attribute: SqlInteger;
      ValuePtr: SqlPointer; StringLength: SqlInteger): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    TablesA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar; NameLength2: SqlSmallint;
      TableName: PAnsiChar; NameLength3: SqlSmallint;
      TableType: PAnsiChar; NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    TablesW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar; NameLength2: SqlSmallint;
      TableName: PWideChar; NameLength3: SqlSmallint;
      TableType: PWideChar; NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ForeignKeysA: function(StatementHandle: SqlHStmt;
      PKCatalogName: PAnsiChar; NameLength1: SqlSmallint;
      PKSchemaName: PAnsiChar; NameLength2: SqlSmallint;
      PKTableName: PAnsiChar; NameLength3: SqlSmallint;
      FKCatalogName: PAnsiChar; NameLength4: SqlSmallint;
      FKSchemaName: PAnsiChar; NameLength5: SqlSmallint;
      FKTableName: PAnsiChar; NameLength6: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    ForeignKeysW: function(StatementHandle: SqlHStmt;
      PKCatalogName: PWideChar; NameLength1: SqlSmallint;
      PKSchemaName: PWideChar; NameLength2: SqlSmallint;
      PKTableName: PWideChar; NameLength3: SqlSmallint;
      FKCatalogName: PWideChar; NameLength4: SqlSmallint;
      FKSchemaName: PWideChar; NameLength5: SqlSmallint;
      FKTableName: PWideChar; NameLength6: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLDriverConnectA: function(ConnectionHandle: SqlHDbc; WindowHandle: SQLHWnd;
      InConnectionString: PAnsiChar; StringLength1: SqlSmallint;
      OutConnectionString: PAnsiChar; BufferLength: SqlSmallint;
      var StringLength2Ptr: SqlSmallint; DriverCompletion: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLDriverConnectW: function(ConnectionHandle: SqlHDbc; WindowHandle: SQLHWnd;
      InConnectionString: PWideChar; StringLength1: SqlSmallint;
      OutConnectionString: PWideChar; BufferLength: SqlSmallint;
      var StringLength2Ptr: SqlSmallint; DriverCompletion: SqlUSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLProcedureColumnsA: function(StatementHandle: SqlHStmt;
      CatalogName: PAnsiChar; NameLength1: SqlSmallint;
      SchemaName: PAnsiChar;  NameLength2: SqlSmallint;
      ProcName: PAnsiChar;   NameLength3: SqlSmallint;
      ColumnName: PAnsiChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLProcedureColumnsW: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      ProcName: PWideChar;   NameLength3: SqlSmallint;
      ColumnName: PWideChar;  NameLength4: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    SQLProcedures: function(StatementHandle: SqlHStmt;
      CatalogName: PWideChar; NameLength1: SqlSmallint;
      SchemaName: PWideChar;  NameLength2: SqlSmallint;
      ProcName: PWideChar;   NameLength3: SqlSmallint): SqlReturn;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
  public
    /// load the ODBC library
    // - and retrieve all SQL*() addresses for ODBC_ENTRIES[] items
    constructor Create;
    /// raise an exception on error
    procedure Check(Conn: TSQLDBConnection; Stmt: TSQLDBStatement; Status: SqlReturn;
      HandleType: SqlSmallint; Handle: SqlHandle; InfoRaiseException: Boolean=false;
      LogLevelNoRaise: TSynLogInfo=sllNone);
      {$ifdef HASINLINE} inline; {$endif}
    /// generic process of error handle
    procedure HandleError(Conn: TSQLDBConnection; Stmt: TSQLDBStatement;
      Status: SqlReturn; HandleType: SqlSmallint; Handle: SqlHandle;
      InfoRaiseException: Boolean; LogLevelNoRaise: TSynLogInfo);
    /// wrapper around SQLGetDiagField() API call
    function GetDiagField(StatementHandle: SqlHStmt): RawUTF8;
    /// wrapper around GetInfo() API call
    procedure GetInfoString(ConnectionHandle: SqlHDbc; InfoType: SqlUSmallint;
      var Dest: RawUTF8);
  end;

const
  {$ifdef MSWINDOWS}
  ODBC_LIB = 'odbc32.dll';
  {$else}
  ODBC_LIB = 'libodbc.so.1';
  {$endif}

  ODBC_ENTRIES: array[0..66] of PChar =
    ('SQLAllocEnv','SQLAllocHandle','SQLAllocStmt',
     'SQLBindCol','SQLBindParameter','SQLCancel','SQLCloseCursor',
     'SQLColAttribute','SQLColAttributeW','SQLColumns','SQLColumnsW',
     'SQLStatistics','SQLStatisticsW','SQLConnect','SQLConnectW',
     'SQLCopyDesc','SQLDataSources','SQLDataSourcesW',
     'SQLDescribeCol','SQLDescribeColW','SQLDisconnect','SQLEndTran',
     'SQLError','SQLErrorW','SQLExecDirect','SQLExecDirectW','SQLExecute',
     'SQLFetch','SQLFetchScroll','SQLFreeConnect','SQLFreeEnv','SQLFreeHandle',
     'SQLFreeStmt','SQLGetConnectAttr','SQLGetConnectAttrW',
     'SQLGetCursorName','SQLGetCursorNameW','SQLGetData',
     'SQLGetDescField','SQLGetDescFieldW','SQLGetDescRec','SQLGetDescRecW',
     'SQLGetDiagField','SQLGetDiagFieldW','SQLGetDiagRec','SQLGetDiagRecW',
     'SQLMoreResults','SQLPrepare','SQLPrepareW','SQLRowCount','SQLNumResultCols',
     'SQLGetInfo','SQLGetInfoW','SQLSetStmtAttr','SQLSetStmtAttrW','SQLSetEnvAttr',
     'SQLSetConnectAttr','SQLSetConnectAttrW','SQLTables','SQLTablesW',
     'SQLForeignKeys','SQLForeignKeysW','SQLDriverConnect','SQLDriverConnectW',
     'SQLProcedureColumnsA','SQLProcedureColumnsW','SQLProcedures');

var
  ODBC: TODBCLib = nil;

{$ifdef MSWINDOWS}
function ODBCInstalledDriversList(const aIncludeVersion: Boolean; var aDrivers: TStrings): Boolean;

  // expand environment variables, i.e %windir%
  // adapted from http://delphidabbler.com/articles?article=6
  function ExpandEnvVars(const aStr: string): string;
  var size: Integer;
  begin
    // Get required buffer size
    size := ExpandEnvironmentStrings(pointer(aStr),nil,0);
    if size>0 then begin
      // Read expanded string into result string
      SetLength(result, size-1);
      ExpandEnvironmentStrings(pointer(aStr),pointer(result),size);
    end else
      result := aStr; // return the original file name
  end;

  function GetFullFileVersion(const aFileName: TFileName): string;
  begin
    with TFileVersion.Create(aFileName,0,0,0,0) do
    try // five digits by section for easy version number comparison as string
      result := Format('%0.5d.%0.5d.%0.5d.%0.5d',[Major,Minor,Release,Build]);
    finally
      Free;
    end;
  end;

var
  I: Integer;
  lDriver: string;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    result := OpenKeyReadOnly('Software\ODBC\ODBCINST.INI\ODBC Drivers') or
              OpenKeyReadOnly('Software\ODBC\ODBCINST.INI');
    if result then begin
      if not Assigned(aDrivers) then
        aDrivers := TStringList.Create;
      GetValueNames(aDrivers);
      if aIncludeVersion then
      for I := 0 to aDrivers.Count-1 do begin
        CloseKey;
        result := OpenKeyReadOnly('Software\ODBC\ODBCINST.INI\' + aDrivers[I]);
        if result then begin
          // expand environment variable, i.e %windir%
          lDriver := ExpandEnvVars(ReadString('Driver'));
          aDrivers[I] := aDrivers[I] + '=' + GetFullFileVersion(lDriver);
        end;
      end;
    end;
  finally
    Free;
  end;
end;
{$else}
// TODO: ODBCInstalledDriversList for Linux
{$endif MSWINDOWS}


{ TODBCConnection }

procedure TODBCConnection.Connect;
const
  DBMS_NAMES: array[0..8] of PAnsiChar = (
    'ORACLE','MICROSOFT SQL','ACCESS','MYSQL','SQLITE','FIREBIRD','INTERBASE',
    'POSTGRE','INFORMIX');
  DBMS_TYPES: array[-1..high(DBMS_NAMES)] of TSQLDBDefinition = (
    dDefault,dOracle,dMSSQL,dJet,dMySQL,dSQLite,dFirebird,dFirebird,dPostgreSql,dInformix);
  DRIVER_NAMES: array[0..21] of PAnsiChar = (
    'SQLSRV','LIBTDSODBC','IVSS','IVMSSS','PBSS',
    'DB2CLI','LIBDB2','IVDB2','PBDB2','MSDB2','CWBODBC', 'MYODBC',
    'SQORA','MSORCL','PBOR','IVOR', 'ODBCFB','IB', 'SQLITE',
    'PSQLODBC', 'NXODBCDRIVER', 'ICLIT09B'
    );
  DRIVER_TYPES: array[-1..high(DRIVER_NAMES)] of TSQLDBDefinition = (
    dDefault, dMSSQL,dMSSQL,dMSSQL,dMSSQL,dMSSQL,
    dDB2,dDB2,dDB2,dDB2,dDB2,dDB2, dMySQL,
    dOracle,dOracle,dOracle,dOracle, dFirebird,dFirebird, dSQLite,
    dPostgreSQL, dNexusDB, dInformix);
  DRIVERCOMPLETION: array[boolean] of SqlUSmallint = (
    SQL_DRIVER_NOPROMPT, SQL_DRIVER_PROMPT);
var Log: ISynLog;
    Len: SqlSmallint;
begin
  Log := SynDBLog.Enter(self,'Connect');
  Disconnect; // force fDbc=nil
  if fEnv=nil then
    if (ODBC=nil) or (ODBC.AllocHandle(SQL_HANDLE_ENV,SQL_NULL_HANDLE,fEnv)=SQL_ERROR) then
      raise EODBCException.CreateUTF8('%: Unable to allocate an environment handle',[self]);
  with ODBC do
  try
    // connect
    Check(self,nil,SetEnvAttr(fEnv,SQL_ATTR_ODBC_VERSION,SQL_OV_ODBC3,0),SQL_HANDLE_ENV,fEnv);
    Check(self,nil,AllocHandle(SQL_HANDLE_DBC,fEnv,fDbc),SQL_HANDLE_ENV,fEnv);
    with fODBCProperties do
      if fServerName<>'' then
        Check(self,nil,ConnectA(fDbc,pointer(fServerName),length(fServerName),
          pointer(fUserID),length(fUserID),pointer(fPassWord),length(fPassWord)),
          SQL_HANDLE_DBC,fDbc) else
      if fDatabaseName='' then
        raise EODBCException.Create(
          'Need ServerName=DataSourceName or DataBaseName=FullConnectString') else begin
        SetString(fSQLDriverFullString,nil,1024);
        fSQLDriverFullString[1] := #0;
        Len := 0;
        Check(self,nil,SQLDriverConnectA(fDbc,
          {$ifdef MSWINDOWS}GetDesktopWindow{$else}0{$endif},
          Pointer(fDatabaseName),length(fDatabaseName),pointer(fSQLDriverFullString),
          length(fSQLDriverFullString),Len,
          DRIVERCOMPLETION[fODBCProperties.fSQLDriverConnectPrompt]),SQL_HANDLE_DBC,fDbc);
        SetLength(fSQLDriverFullString,Len);
      end;
    // retrieve information of the just created connection
    GetInfoString(fDbc,SQL_DRIVER_NAME,fDriverName);
    GetInfoString(fDbc,SQL_DBMS_NAME,fDBMSName);
    GetInfoString(fDbc,SQL_DBMS_VER,fDBMSVersion);
    // guess DBMS type from driver name or DBMS name
    fDBMS := DRIVER_TYPES[IdemPCharArray(pointer(fDriverName),DRIVER_NAMES)];
    if fDBMS=dDefault then
      fDBMS := DBMS_TYPES[IdemPCharArray(pointer(fDBMSName),DBMS_NAMES)];
    if fDBMS=dDefault then
      raise EODBCException.CreateUTF8(
        '%.Connect: unrecognized provider DBMSName=% DriverName=% DBMSVersion=%',
        [self,DBMSName,DriverName,DBMSVersion]);
    if Log<>nil then
      Log.Log(sllDebug,'Connected to % using % % recognized as %',
        [DBMSName,DriverName,DBMSVersion,fProperties.DBMSEngineName]);
    // notify any re-connection
    inherited Connect;
  except
    on E: Exception do begin
      self.Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

constructor TODBCConnection.Create(aProperties: TSQLDBConnectionProperties);
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self,'Create');
  if not aProperties.InheritsFrom(TODBCConnectionProperties) then
    raise EODBCException.CreateUTF8('Invalid %.Create(%)',[self,aProperties]);
  fODBCProperties := TODBCConnectionProperties(aProperties);
  inherited Create(aProperties);
end;

destructor TODBCConnection.Destroy;
begin
  inherited Destroy;
  if (ODBC<>nil) and (fEnv<>nil) then
    ODBC.FreeHandle(SQL_HANDLE_ENV,fEnv);
end;

procedure TODBCConnection.Disconnect;
var log: ISynLog;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if (ODBC<>nil) and (fDbc<>nil) then
    with ODBC do begin
      log := SynDBLog.Enter(self,'Disconnect');
      Disconnect(fDbc);
      FreeHandle(SQL_HANDLE_DBC,fDbc);
      fDbc := nil;
    end;
  end;
end;

function TODBCConnection.IsConnected: boolean;
begin
  result := fDbc<>nil;
end;

function TODBCConnection.NewStatement: TSQLDBStatement;
begin
  result := TODBCStatement.Create(self);
end;

procedure TODBCConnection.Commit;
begin
  inherited Commit; // dec(fTransactionCount)
  with ODBC do
  try
    Check(self,nil,EndTran(SQL_HANDLE_DBC,fDBc,SQL_COMMIT),SQL_HANDLE_DBC,fDBc);
    Check(self,nil,SetConnectAttrW(fDBc,SQL_AUTOCOMMIT,SQL_AUTOCOMMIT_ON,0),
      SQL_HANDLE_DBC,fDBc); // back to default AUTO COMMIT ON mode
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TODBCConnection.Rollback;
begin
  inherited RollBack;
  with ODBC do begin
    Check(self,nil,EndTran(SQL_HANDLE_DBC,fDBc,SQL_ROLLBACK),SQL_HANDLE_DBC,fDBc);
    Check(self,nil,SetConnectAttrW(fDBc,SQL_AUTOCOMMIT,SQL_AUTOCOMMIT_ON,0),
      SQL_HANDLE_DBC,fDBc); // back to default AUTO COMMIT ON mode
  end;
end;

procedure TODBCConnection.StartTransaction;
var log: ISynLog;
begin
  log := SynDBLog.Enter(self,'StartTransaction');
  if TransactionCount>0 then
    raise EODBCException.CreateUTF8('% do not support nested transactions',[self]);
  inherited StartTransaction;
  ODBC.Check(self,nil,ODBC.SetConnectAttrW(fDBc,SQL_AUTOCOMMIT,SQL_AUTOCOMMIT_OFF,0),
    SQL_HANDLE_DBC,fDBc);
end;


{ TODBCStatement }

procedure TODBCStatement.AllocStatement;
var hDbc: SqlHDbc;
begin
  if fStatement<>nil then
    raise EODBCException.CreateUTF8('%.AllocStatement called twice',[self]);
  fCurrentRow := 0;
  fTotalRowsRetrieved := 0;
  if not fConnection.Connected then
    fConnection.Connect;
  hDbc := (fConnection as TODBCConnection).fDbc;
  with ODBC do
    Check(nil,self,AllocHandle(SQL_HANDLE_STMT,hDBC,fStatement),SQL_HANDLE_DBC,hDBC);
end;

procedure TODBCStatement.DeallocStatement;
begin
  if fStatement<>nil then
    // avoid Informix exception and log exception race condition
    try
      try
        ODBC.Check(nil,self,ODBC.FreeHandle(SQL_HANDLE_STMT,fStatement),SQL_HANDLE_DBC,
          (fConnection as TODBCConnection).fDbc);
      except
      end;
    finally
      fStatement := Nil;
    end;
end;

function ODBCColumnToFieldType(DataType, ColumnPrecision, ColumnScale: integer): TSQLDBFieldType;
begin // ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob
  case DataType of
    SQL_DECIMAL, SQL_NUMERIC, SQL_FLOAT: begin
      result := ftDouble;
      if ColumnPrecision=10 then
        case ColumnScale of
        0:    result := ftInt64;
        1..4: result := ftCurrency;
        end;
    end;
    SQL_REAL, SQL_DOUBLE:
      result := ftDouble;
    SQL_SMALLINT, SQL_INTEGER, SQL_TINYINT, SQL_BIT, SQL_BIGINT:
      result := ftInt64;
    SQL_BINARY, SQL_VARBINARY, SQL_LONGVARBINARY:
      result := ftBlob;
    SQL_TIME, SQL_DATETIME,
    SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TYPE_TIMESTAMP:
      result := ftDate;
    else // all other types will be converted to text
      result := ftUtf8;
  end;
end;

const
  /// internal mapping to handled GetData() type for Column*() methods
  // - numerical values (integer or floating-point) are converted to SQL_C_CHAR
  // - date/time to SQL_C_TYPE_TIMESTAMP object
  // - text columns to SQL_C_WCHAR (for proper UTF-8 data retrieval)
  // - BLOB columns to SQL_C_BINARY
  ODBC_TYPE_TOC: array[TSQLDBFieldType] of ShortInt = (
   SQL_C_CHAR, SQL_C_CHAR, SQL_C_CHAR, SQL_C_CHAR, SQL_C_CHAR,
   SQL_C_TYPE_TIMESTAMP, SQL_C_WCHAR, SQL_C_BINARY);
   // ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob

procedure TODBCStatement.BindColumns;
var nCols, NameLength, DataType, DecimalDigits, Nullable: SqlSmallint;
    ColumnSize: SqlULen;
    c, siz: integer;
    Name: array[byte] of WideChar;
begin
  ReleaseRows;
  with ODBC do begin
    Check(nil,self,NumResultCols(fStatement,nCols),SQL_HANDLE_STMT,fStatement);
    SetLength(fColData,nCols);
    fColumn.Capacity := nCols;
    for c := 1 to nCols do begin
      Check(nil,self,DescribeColW(fStatement,c,Name,256,NameLength,DataType,ColumnSize,
        DecimalDigits,Nullable),SQL_HANDLE_STMT,fStatement);
      with PSQLDBColumnProperty(fColumn.AddAndMakeUniqueName(
         RawUnicodeToUtf8(Name,NameLength)))^ do begin
        ColumnValueInlined := true;
        ColumnValueDBType := DataType;
        if ColumnSize>65535 then
          ColumnSize := 0; // avoid out of memory error for BLOBs
        ColumnValueDBSize := ColumnSize;
        ColumnNonNullable := (Nullable=SQL_NO_NULLS);
        ColumnType := ODBCColumnToFieldType(DataType,10,DecimalDigits);
        if ColumnType=ftUTF8 then
          if ColumnSize=0 then
            siz := 256 else
            siz := ColumnSize*2+16 else // guess max size as WideChar buffer
            siz := ColumnSize;
        if siz<64 then
          siz := 64; // ODBC never truncates fixed-length data: ensure minimum
        if siz>Length(fColData[c-1]) then
          SetLength(fColData[c-1],siz);
      end;
    end;
    assert(fColumnCount=nCols);
  end;
end;

procedure TODBCStatement.GetData(var Col: TSQLDBColumnProperty; ColIndex: integer);
var ExpectedDataType: ShortInt;
    ExpectedDataLen: integer;
    Status: SqlReturn;
    Indicator: SqlLen;
    P: PAnsiChar;
  function IsTruncated: boolean;
  begin
    result := (Indicator>0) and (ODBC.GetDiagField(fStatement)='01004');
  end;
  procedure CheckStatus;
  begin
    if Status<>SQL_SUCCESS then
      ODBC.HandleError(nil,self,Status,SQL_HANDLE_STMT,fStatement,false,sllNone);
  end;
  procedure RaiseError;
  begin
    raise EODBCException.CreateUTF8('%.GetCol: [%] column had Indicator=%',
      [self,Col.ColumnName,Indicator]);
  end;
begin
  ExpectedDataType := ODBC_TYPE_TOC[Col.ColumnType];
  ExpectedDataLen := length(fColData[ColIndex]);
  //FillcharFast(pointer(fColData[ColIndex])^,ExpectedDataLen,ord('~'));
  Status := ODBC.GetData(fStatement,ColIndex+1,ExpectedDataType,
    pointer(fColData[ColIndex]),ExpectedDataLen,@Indicator);
  Col.ColumnDataSize := Indicator;
  if Status<>SQL_SUCCESS then
    if Status=SQL_SUCCESS_WITH_INFO then
      if Col.ColumnType in FIXEDLENGTH_SQLDBFIELDTYPE then
        Status := SQL_SUCCESS else // allow rounding problem
      if IsTruncated then begin
        if Col.ColumnType<>ftBlob then begin
          dec(ExpectedDataLen,SizeOf(WideChar)); // ignore null termination
          inc(Indicator,SizeOf(WideChar)); // always space for additional #0
        end;
        SetLength(fColData[ColIndex],Indicator);
        P := pointer(fColData[ColIndex]);
        inc(P,ExpectedDataLen);
        ExpectedDataLen := Indicator-ExpectedDataLen;
        //FillcharFast(P^,ExpectedDataLen,ord('~'));
        Status := ODBC.GetData(fStatement,ColIndex+1,ExpectedDataType,
          P,ExpectedDataLen,@Indicator);
        CheckStatus;
      end else
      CheckStatus else
    CheckStatus;
  if Indicator>=0 then
    case Status of
    SQL_SUCCESS, SQL_NO_DATA:
      Col.ColumnDataState := colDataFilled;
    else RaiseError;
    end else
  case Indicator of
  SQL_NULL_DATA:
    Col.ColumnDataState := colNull;
  SQL_NO_TOTAL:
    if Col.ColumnType in FIXEDLENGTH_SQLDBFIELDTYPE then
      Col.ColumnDataState := colDataFilled else
      raise EODBCException.CreateUTF8('%.GetCol: [%] column has no size',
        [self,Col.ColumnName]);
  else RaiseError;
  end;
end;

function TODBCStatement.GetCol(Col: integer; ExpectedType: TSQLDBFieldType): TSQLDBStatementGetCol;
var c: Integer;
begin // colNull, colWrongType, colTmpUsed, colTmpUsedTruncated
  CheckCol(Col); // check Col<fColumnCount
  if not Assigned(fStatement) or (fColData=nil) then
    raise EODBCException.CreateUTF8('%.Column*() with no prior Execute',[self]);
  // get all fColData[] (driver may be without SQL_GD_ANY_ORDER)
  for c := 0 to fColumnCount-1 do
    if fColumns[c].ColumnDataState=colNone then
      GetData(fColumns[c], c);
  // retrieve information for the specified column
  if (ExpectedType=ftNull) or (fColumns[Col].ColumnType=ExpectedType) or
     (fColumns[Col].ColumnDataState=colNull) then
    result := fColumns[Col].ColumnDataState else
    result := colWrongType;
end;

function TODBCStatement.MoreResults: boolean;
var R: SqlReturn;
begin
  R := ODBC.MoreResults(fStatement);
  case R of
    SQL_NO_DATA:
      result := false; // no more results
    SQL_SUCCESS, SQL_SUCCESS_WITH_INFO:
      result := true; // got next
    else begin
      ODBC.Check(nil,self, R, SQL_HANDLE_STMT, fStatement); // error
      result := false; // makes compiler happy
    end;
  end;
end;

function TODBCStatement.ColumnBlob(Col: integer): RawByteString;
var res: TSQLDBStatementGetCol;
begin
  res := GetCol(Col,ftBlob);
  case res of
    colNull:
      result := '';
    colWrongType:
      ColumnToTypedValue(Col,ftBlob,result);
    else result := copy(fColData[Col],1,fColumns[Col].ColumnDataSize);
  end;
end;

function TODBCStatement.ColumnUTF8(Col: integer): RawUTF8;
var res: TSQLDBStatementGetCol;
begin
  res := GetCol(Col,ftUTF8);
  case res of
    colNull:
      result := '';
    colWrongType:
      ColumnToTypedValue(Col,ftUTF8,result);
    else
      RawUnicodeToUtf8(pointer(fColData[Col]),fColumns[Col].ColumnDataSize shr 1,result);
  end;
end;

function TODBCStatement.ColumnCurrency(Col: integer): currency;
begin
  case GetCol(Col,ftCurrency) of
    colNull:      result := 0;
    colWrongType: ColumnToTypedValue(Col,ftCurrency,result);
    else PInt64(@result)^ := StrToCurr64(Pointer(fColData[Col])); // as SQL_C_CHAR
  end;
end;

function TODBCStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  case GetCol(Col,ftDate) of
    colNull:      result := 0;
    colWrongType: ColumnToTypedValue(Col,ftDate,result);
    else result := PSQL_TIMESTAMP_STRUCT(Pointer(fColData[Col]))^.ToDateTime(
      fColumns[Col].ColumnValueDBType);
  end;
end;

function TODBCStatement.ColumnDouble(Col: integer): double;
begin
  case GetCol(Col,ftDouble) of
    colNull:      result := 0;
    colWrongType: ColumnToTypedValue(Col,ftDouble,result);
    else result := GetExtended(Pointer(fColData[Col])); // encoded as SQL_C_CHAR
  end;
end;

function TODBCStatement.ColumnInt(Col: integer): Int64;
begin
  case GetCol(Col,ftInt64) of
    colNull:      result := 0;
    colWrongType: ColumnToTypedValue(Col,ftInt64,result);
    else SetInt64(Pointer(fColData[Col]),result); // encoded as SQL_C_CHAR
  end;
end;

function TODBCStatement.ColumnNull(Col: integer): boolean;
begin // will check for NULL but never returns colWrongType
  result := GetCol(Col,ftNull)=colNull;
end;

procedure TODBCStatement.ColumnsToJSON(WR: TJSONWriter);
var res: TSQLDBStatementGetCol;
    col: integer;
    tmp: array[0..31] of AnsiChar;
begin
  if not Assigned(fStatement) or (CurrentRow<=0) then
    raise EODBCException.CreateUTF8('%.ColumnsToJSON() with no prior Step',[self]);
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount-1 do // fast direct conversion from OleDB buffer
  with fColumns[col] do begin
    if WR.Expand then
      WR.AddFieldName(ColumnName); // add '"ColumnName":'
    res := GetCol(Col,ColumnType);
    if res=colNull then
      WR.AddShort('null') else
      case ColumnType of
      ftInt64:
        WR.AddNoJSONEscape(Pointer(fColData[Col]));  // already as SQL_C_CHAR
      ftDouble, ftCurrency:
        WR.AddFloatStr(Pointer(fColData[Col]));      // already as SQL_C_CHAR
      ftDate:
        WR.AddNoJSONEscape(@tmp,PSQL_TIMESTAMP_STRUCT(Pointer(fColData[Col]))^.
          ToIso8601(tmp,ColumnValueDBType,fForceDateWithMS));
      ftUTF8: begin
        WR.Add('"');
        if ColumnDataSize>1 then
          WR.AddJSONEscapeW(Pointer(fColData[Col]),ColumnDataSize shr 1);
        WR.Add('"');
      end;
      ftBlob:
        if fForceBlobAsNull then
          WR.AddShort('null') else
          WR.WrBase64(pointer(fColData[Col]),ColumnDataSize,true);
      else assert(false);
    end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

constructor TODBCStatement.Create(aConnection: TSQLDBConnection);
begin
  if not aConnection.InheritsFrom(TODBCConnection) then
    raise EODBCException.CreateUTF8('%.Create(%)',[self,aConnection]);
  inherited Create(aConnection);
end;

destructor TODBCStatement.Destroy;
begin
  try
    DeallocStatement;
  finally
    inherited Destroy;
  end;
end;

const
  NULCHAR: WideChar = #0;

procedure TODBCStatement.ExecutePrepared;
const
  ODBC_IOTYPE_TO_PARAM: array[TSQLDBParamInOutType] of ShortInt = (
    SQL_PARAM_INPUT, SQL_PARAM_OUTPUT, SQL_PARAM_INPUT_OUTPUT);
  IDList_type: WideString = 'IDList';
  StrList_type: WideString = 'StrList';

  function CType2SQL(CDataType: integer): integer;
  begin
    case CDataType of
     SQL_C_CHAR:
      case fDBMS of
        dInformix:         result := SQL_INTEGER;
        else               result := SQL_VARCHAR;
      end;
     SQL_C_TYPE_DATE:      result := SQL_TYPE_DATE;
     SQL_C_TYPE_TIMESTAMP: result := SQL_TYPE_TIMESTAMP;
     SQL_C_WCHAR:
      case fDBMS of
        dInformix:         result := SQL_VARCHAR;
        else               result := SQL_WVARCHAR;
      end;
     SQL_C_BINARY:         result := SQL_VARBINARY;
     SQL_C_SBIGINT:        result := SQL_BIGINT;
     SQL_C_DOUBLE:         result := SQL_DOUBLE;
     else raise EODBCException.CreateUTF8(
       '%.ExecutePrepared: Unexpected ODBC C type %',[self,CDataType]);
    end;
  end;

var p, k: integer;
    status: SqlReturn;
    InputOutputType, CValueType, ParameterType, DecimalDigits: SqlSmallint;
    ColumnSize: SqlULen;
    ParameterValue: SqlPointer;
    ItemSize, BufferSize: SqlLen;
    ItemPW: PWideChar;
    timestamp: SQL_TIMESTAMP_STRUCT;
    ansitext: boolean;
    StrLen_or_Ind: array of PtrInt;
    ArrayData: array of record
      StrLen_or_Ind: array of PtrInt;
      WData: RawUnicode;
    end;
label retry;
begin
  SQLLogBegin(sllSQL);
  if fStatement=nil then
    raise EODBCException.CreateUTF8('%.ExecutePrepared called without previous Prepare',[self]);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  ansitext := TODBCConnection(fConnection).fODBCProperties.fDriverDoesNotHandleUnicode;
  try
    // 1. bind parameters
    if (fParamsArrayCount>0) and (fDBMS<>dMSSQL) then
      raise EODBCException.CreateUTF8('%.BindArray() not supported',[self]);
    if fParamCount>0 then begin
      SetLength(StrLen_or_Ind,fParamCount);
      if (fParamsArrayCount>0) then
        SetLength(ArrayData,fParamCount);
      for p := 0 to fParamCount-1 do
      with fParams[p] do begin
        StrLen_or_Ind[p] := SQL_NTS;
        ParameterValue := nil;
        CValueType := ODBC_TYPE_TOC[VType];
        ParameterType := CType2SQL(CValueType);
        InputOutputType := ODBC_IOTYPE_TO_PARAM[VInOut];
        ColumnSize := 0;
        DecimalDigits := 0;
        if (fDBMS=dMSSQL) and (VArray<>nil) then begin
          // bind an array as one object - metadata only at the moment
          if (VInOut<>paramIn) then
            raise EODBCException.CreateUTF8(
              '%.ExecutePrepared: Unsupported array parameter direction #%',[self,p+1]);
          CValueType := SQL_C_DEFAULT;
          ParameterType := SQL_SS_TABLE;
          ColumnSize := Length(VArray);
          // Type name must be specified as a Unicode value, even in applications that are built as ANSI applications
          case VType of
            ftInt64: ParameterValue := pointer(IDList_type);
            ftUTF8:  ParameterValue := pointer(StrList_type);
            else raise EODBCException.CreateUTF8(
              '%.ExecutePrepared: Unsupported array parameter type #%',[self,p+1]);
          end;
          BufferSize := Length(WideString(ParameterValue))*SizeOf(WideChar);
          StrLen_or_Ind[p] := Length(VArray);
        end else begin
          // Bind one simple parameter value
          case VType of
          ftNull:
            StrLen_or_Ind[p] := SQL_NULL_DATA;
          ftInt64:
            if VInOut=paramIn then
              VData := Int64ToUTF8(VInt64) else begin
              CValueType := SQL_C_SBIGINT;
              ParameterValue := pointer(@VInt64);
            end;
          ftDouble: begin
            CValueType := SQL_C_DOUBLE;
	    // in case of "Invalid character value for cast specification" error
            // for small digits like 0.01, -0.0001 under Linux msodbcsql17 should
            // be updated to >= 17.5.2
            ParameterValue := pointer(@VInt64);
          end;
          ftCurrency:
            if VInOut=paramIn then
              VData := Curr64ToStr(VInt64) else begin
              CValueType := SQL_C_DOUBLE;
              unaligned(PDouble(@VInt64)^) := PCurrency(@VInt64)^;
              ParameterValue := pointer(@VInt64);
            end;
          ftDate: begin
            CValueType := timestamp.From(PDateTime(@VInt64)^,BufferSize);
            SetString(VData,PAnsiChar(@timestamp),BufferSize);
            // A workaround for "[ODBC Driver 13 for SQL Server]Datetime field overflow. Fractional second precision exceeds the scale specified in the parameter binding"
            // Implemented according to http://rightondevelopment.blogspot.com/2009/10/sql-server-native-client-100-datetime.html
            if fDBMS=dMSSQL then // Starting from MSSQL 2008 client DecimalDigits must not be 0
              DecimalDigits := 3; // Possibly can be set to either 3 (datetime) or 7 (datetime2)
          end;
          ftUTF8:
            if ansitext then begin
  retry:      VData := CurrentAnsiConvert.UTF8ToAnsi(VData);
              CValueType := SQL_C_CHAR;
            end else begin
              VData := Utf8DecodeToRawUnicode(VData);
              if (fDBMS=dMSSQL) then begin // statements like CONTAINS(field, ?) do not accept NVARCHAR(max)
                ColumnSize := length(VData) shr 1; // length in characters
                if (ColumnSize > 4000) then // > 8000 bytes - use varchar(max)
                  ColumnSize := 0;
              end;
            end;
          ftBlob:
            StrLen_or_Ind[p] := length(VData);
          else
            raise EODBCException.CreateUTF8(
              '%.ExecutePrepared: invalid bound parameter #%',[self,p+1]);
          end;
          if ParameterValue=nil then begin
            if pointer(VData)=nil then
              ParameterValue := @NULCHAR else
              ParameterValue := pointer(VData);
            BufferSize := length(VData);
            if CValueType=SQL_C_CHAR then
              inc(BufferSize) else // include last #0
            if CValueType=SQL_C_WCHAR then
              BufferSize := BufferSize shr 1;
          end else
            BufferSize := SizeOf(Int64);
        end;
        status := ODBC.BindParameter(fStatement, p+1, InputOutputType, CValueType,
         ParameterType, ColumnSize, DecimalDigits, ParameterValue, BufferSize, StrLen_or_Ind[p]);
        if (status=SQL_ERROR) and not ansitext and (ODBC.GetDiagField(fStatement)='HY004') then begin
          TODBCConnection(fConnection).fODBCProperties.fDriverDoesNotHandleUnicode := true;
          ansitext := true;
          VData := RawUnicodeToUtf8(pointer(VData),StrLenW(pointer(VData)));
          goto retry; // circumvent restriction of non-Unicode ODBC drivers
        end;
        ODBC.Check(nil,self,status,SQL_HANDLE_STMT,fStatement);
        // populate array data
        if (fDBMS=dMSSQL) and (VArray<>nil) then begin
          // first set focus on param
          status := ODBC.SetStmtAttrA(fStatement, SQL_SOPT_SS_PARAM_FOCUS, SQLPOINTER(p+1), SQL_IS_INTEGER);
          if not (status in [SQL_SUCCESS,SQL_NO_DATA]) then
            ODBC.HandleError(nil,self,status,SQL_HANDLE_STMT,fStatement,false,sllNone);
          // bind the only column
          SetLength(ArrayData[p].StrLen_or_Ind,Length(VArray));
          CValueType := SQL_C_WCHAR;
          ParameterType := SQL_WVARCHAR;
          // all data should be passed in a single buffer of fixed size records
          // so find out the size of a record, i.e. maximum string length
          BufferSize := 0;
          for k := 0 to high(VArray) do begin
            if VType=ftUTF8 then
              VArray[k] := SynCommons.UnQuoteSQLString(VArray[k]);
            ItemSize := Utf8ToUnicodeLength(pointer(VArray[k]));
            if ItemSize>BufferSize then
              BufferSize := ItemSize;
          end;
          inc(BufferSize); // add space for #0
          SetLength(ArrayData[p].WData,BufferSize*Length(VArray)*SizeOf(WideChar));
          ItemPW := pointer(ArrayData[p].WData);
          for k := 0 to high(VArray) do begin
            ArrayData[p].StrLen_or_Ind[k] := UTF8ToWideChar(ItemPW,pointer(VArray[k]),
              BufferSize,length(VArray[k]));
           inc(ItemPW,BufferSize);
          end;
          status := ODBC.BindParameter(fStatement,1,SQL_PARAM_INPUT,CValueType,
            ParameterType,0,0,pointer(ArrayData[p].WData),BufferSize*SizeOf(WideChar),
            ArrayData[p].StrLen_or_Ind[0]);
          if not (status in [SQL_SUCCESS,SQL_NO_DATA]) then
            ODBC.HandleError(nil,self,status,SQL_HANDLE_STMT,fStatement,false,sllNone);
          // Reset param focus
          status := ODBC.SetStmtAttrA(fStatement,SQL_SOPT_SS_PARAM_FOCUS,SQLPOINTER(0),SQL_IS_INTEGER);
          if not (status in [SQL_SUCCESS,SQL_NO_DATA]) then
            ODBC.HandleError(nil,self,status,SQL_HANDLE_STMT,fStatement,false,sllNone);
        end;
      end;
    end;
    // 2. execute prepared statement
    status := ODBC.Execute(fStatement);
    if not (status in [SQL_SUCCESS,SQL_NO_DATA]) then
      ODBC.HandleError(nil,self,status,SQL_HANDLE_STMT,fStatement,false,sllNone);
    if fExpectResults then
      BindColumns;
  finally
    // 3. release and/or retrieve OUT bound parameters
    for p := 0 to fParamCount-1 do
    with fParams[p] do
    case VType of
      ftCurrency:
        if VInOut<>paramIn then
          PCurrency(@VInt64)^ := unaligned(PDouble(@VInt64)^);
      ftDate:
        if VInOut<>paramIn then
          PDateTime(@VInt64)^ := PSQL_TIMESTAMP_STRUCT(VData)^.ToDateTime;
      ftUTF8:
        if ansitext then
          VData := CurrentAnsiConvert.AnsiBufferToRawUTF8(pointer(VData),StrLen(pointer(VData))) else
          VData := RawUnicodeToUtf8(pointer(VData),StrLenW(pointer(VData)));
    end;
  end;
  SQLLogEnd;
end;

procedure TODBCStatement.Reset;
begin
  if fStatement<>nil then begin
    ReleaseRows;
    if fParamCount>0 then
      ODBC.Check(nil,self,ODBC.FreeStmt(fStatement,SQL_RESET_PARAMS),SQL_HANDLE_STMT,fStatement);
  end;
  inherited Reset;
end;

procedure TODBCStatement.ReleaseRows;
begin
  fColData := nil;
  if fColumnCount>0 then begin
    if fStatement<>nil then
      ODBC.CloseCursor(fStatement); // no check needed
    fColumn.Clear;
    fColumn.ReHash;
  end;
  inherited ReleaseRows;
end;

function TODBCStatement.UpdateCount: integer;
var RowCount: SqlLen;
begin
  if (fStatement<>nil) and not fExpectResults then
    ODBC.Check(nil,self,ODBC.RowCount(fStatement,RowCount),SQL_HANDLE_STMT,fStatement) else
    RowCount := 0;
  result := RowCount;
end;

procedure TODBCStatement.Prepare(const aSQL: RawUTF8; ExpectResults: Boolean);
begin
  SQLLogBegin(sllDB);
  if (fStatement<>nil) or (fColumnCount>0) then
    raise EODBCException.CreateUTF8('%.Prepare should be called only once',[self]);
  // 1. process SQL
  inherited Prepare(aSQL,ExpectResults); // set fSQL + Connect if necessary
  fSQLW := Utf8DecodeToRawUnicode(fSQL);
  // 2. prepare statement and bind result columns (if any)
  AllocStatement;
  try
    ODBC.Check(nil,self,ODBC.PrepareW(fStatement,pointer(fSQLW),length(fSQLW) shr 1),
      SQL_HANDLE_STMT,fStatement);
    SQLLogEnd;
  except
    on E: Exception do begin
      ODBC.FreeHandle(SQL_HANDLE_STMT,fStatement);
      fStatement := nil;
      raise;
    end;
  end;
end;

function TODBCStatement.Step(SeekFirst: boolean): boolean;
const CMD: array[boolean] of smallint = (SQL_FETCH_NEXT,SQL_FETCH_FIRST);
var status: SqlReturn;
    i, sav: integer;
begin
  result := false;
  sav := fCurrentRow;
  fCurrentRow := 0;
  if not Assigned(fStatement) or (fColumnCount=0) then
    exit; // no row available at all (e.g. for SQL UPDATE) -> return false
  for i := 0 to fColumnCount-1 do
    fColumns[i].ColumnDataState := colNone; // force load all fColData[]
  with ODBC do begin
    status := FetchScroll(fStatement,CMD[SeekFirst],0);
    case status of
    SQL_NO_DATA:
      exit;
    SQL_SUCCESS, SQL_SUCCESS_WITH_INFO: begin // ignore WITH_INFO messages
      fCurrentRow := sav+1;
      inc(fTotalRowsRetrieved);
      result := true; // mark data available for Column*() methods
    end;
    else HandleError(nil,self,status,SQL_HANDLE_STMT,fStatement,false,sllNone);
    end;
  end;
end;


{ TODBCLib }

procedure TODBCLib.Check(Conn: TSQLDBConnection; Stmt: TSQLDBStatement;
  Status: SqlReturn; HandleType: SqlSmallint; Handle: SqlHandle;
  InfoRaiseException: Boolean=false; LogLevelNoRaise: TSynLogInfo=sllNone);
begin
  if Status<>SQL_SUCCESS then
    HandleError(Conn,Stmt,Status,HandleType,Handle,InfoRaiseException,LogLevelNoRaise);
end;

constructor TODBCLib.Create;
var P: PPointer;
    i: integer;
begin
  TryLoadLibrary([ODBC_LIB], EODBCException);
  P := @@AllocEnv;
  for i := 0 to High(ODBC_ENTRIES) do begin
    P^ := GetProcAddress(fHandle,ODBC_ENTRIES[i]);
    if P^=nil then begin
      FreeLibrary(fHandle);
      fHandle := 0;
      raise EODBCException.CreateUTF8('Invalid %: missing %',[fLibraryPath,ODBC_ENTRIES[i]]);
    end;
    inc(P);
  end;
end;

function TODBCLib.GetDiagField(StatementHandle: SqlHStmt): RawUTF8;
var Status: array[0..7] of AnsiChar;
    StringLength: SqlSmallint;
begin
  if ODBC.GetDiagFieldA(SQL_HANDLE_STMT,StatementHandle,1,SQL_DIAG_SQLSTATE,
     @Status,SizeOf(Status),StringLength)=0 then
    SetString(result,PAnsiChar(@Status),StringLength) else
    result := '';
end;

procedure TODBCLib.GetInfoString(ConnectionHandle: SqlHDbc; InfoType: SqlUSmallint;
  var Dest: RawUTF8);
var Len: SqlSmallint;
    Info: array[byte] of WideChar;
begin
  Len := 0;
  Check(nil,nil,GetInfoW(ConnectionHandle,InfoType,@Info,SizeOf(Info)shr 1,@Len),
    SQL_HANDLE_DBC,ConnectionHandle);
  Dest := RawUnicodeToUtf8(Info,Len shr 1);
end;

procedure TODBCLib.HandleError(Conn: TSQLDBConnection; Stmt: TSQLDBStatement;
  Status: SqlReturn; HandleType: SqlSmallint; Handle: SqlHandle;
  InfoRaiseException: Boolean; LogLevelNoRaise: TSynLogInfo);
const FMT: PUTF8Char = '%[%] % (%)'#13#10;
var Sqlstate: array[0..6] of WideChar;
    MessageText: array[0..1023] of WideChar;
    RecNum, NativeError: SqlInteger;
    TextLength: SqlSmallint;
    msg: RawUTF8;
begin
  if (Handle=nil) or (Status=SQL_INVALID_HANDLE) then
    msg := 'Invalid handle' else begin
    RecNum := 1;
    while ODBC.GetDiagRecW(HandleType,Handle,RecNum,
       Sqlstate,NativeError,MessageText,1024,TextLength) and (not 1)=0 do begin
      while (textlength>0) and (MessageText[textlength-1]<' ') do begin
        dec(textlength);
        MessageText[textlength] := #0; // trim #13/#10 right of MessageText
      end;
      msg := FormatUTF8(FMT,[msg,Sqlstate,MessageText,NativeError]);
      inc(RecNum);
    end;
    if msg='' then
      msg := 'Unspecified error';
    if (Status=SQL_SUCCESS_WITH_INFO) and not InfoRaiseException then begin
      LogLevelNoRaise := sllInfo;
      if (Conn=nil) and (Stmt<>nil) then
        Conn := Stmt.Connection;
      if Conn<>nil then
        with Conn.Properties do
          if Assigned(OnStatementInfo) then
            OnStatementInfo(Stmt,msg);
    end;
  end;
  if LogLevelNoRaise<>sllNone then
    SynDBLog.Add.Log(LogLevelNoRaise,msg) else
    if Stmt=nil then
      raise EODBCException.CreateUTF8('% error: %',[self,msg]) else
      raise EODBCException.CreateUTF8('% - % error: %',[Stmt,self,msg]);
end;


{ TODBCConnectionProperties }

constructor TODBCConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  if ODBC=nil then
    GarbageCollectorFreeAndNil(ODBC,TODBCLib.Create);
  inherited Create(aServerName,aDatabaseName,aUserID,aPassWord);
  // stored UserID is used by SQLSplitProcedureName
  if (aUserID = '') then
    FUserID := FindIniNameValue(pointer(SynCommons.UpperCase(
      StringReplaceAll(aDatabaseName,';',sLineBreak))), 'UID=');
end;

function TODBCConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TODBCConnection.Create(self);
end;

procedure TODBCConnectionProperties.GetFields(const aTableName: RawUTF8;
  out Fields: TSQLDBColumnDefineDynArray);
var Schema, Table: RawUTF8;
    F: TSQLDBColumnDefine;
    i,n,DataType: integer;
    status: SqlReturn;
    FA: TDynArray;
begin
  inherited; // first try from SQL, if any (faster)
  if Fields<>nil then
    exit; // already retrieved directly from engine
  Split(aTableName,'.',Schema,Table);
  if Table='' then begin
    Table := Schema;
    Schema := '%';
  end;
  Table := SynCommons.UpperCase(Table);
  Schema := SynCommons.UpperCase(Schema);
  try
    // get column definitions
    with TODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      status := ODBC.ColumnsA(fStatement,nil,0,pointer(Schema),SQL_NTS,
        pointer(Table),SQL_NTS,nil,0);
      if status=SQL_SUCCESS then begin
        BindColumns;
        if not Step then
          status := SQL_NO_DATA; // no info -> retry without schema
      end;
      if status<>SQL_SUCCESS then begin
        DeallocStatement;
        AllocStatement;
        status := ODBC.ColumnsA(fStatement,nil,0,nil,0,pointer(Table),SQL_NTS,nil,0);
        ODBC.Check(Connection,nil,status,SQL_HANDLE_STMT,fStatement);
        BindColumns;
        Step;
      end;
      FA.Init(TypeInfo(TSQLDBColumnDefineDynArray),Fields,@n);
      FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
      FillcharFast(F,SizeOf(F),0);
      if fCurrentRow>0 then // Step done above
      repeat
        F.ColumnName := Trim(ColumnUTF8(3)); // Column*() should be done in order
        DataType := ColumnInt(4);
        F.ColumnTypeNative := Trim(ColumnUTF8(5));
        F.ColumnLength := ColumnInt(6);
        F.ColumnScale := ColumnInt(8);
        F.ColumnPrecision := ColumnInt(9);
        F.ColumnType:= ODBCColumnToFieldType(DataType,F.ColumnPrecision,F.ColumnScale);
        F.ColumnIndexed := (fDBMS in [dFirebird,dDB2]) and
          IsRowID(pointer(F.ColumnName)); // ID UNIQUE field create an implicit index
        FA.Add(F);
      until not Step;
      SetLength(Fields,n);
    finally
      Free; // TODBCStatement release
    end;
    // get indexes
    if n>0 then
      with TODBCStatement.Create(MainConnection) do
      try
        AllocStatement;
        status := ODBC.StatisticsA(fStatement,nil,0,pointer(Schema),SQL_NTS,
          pointer(Table),SQL_NTS,SQL_INDEX_ALL,SQL_QUICK);
        if status<>SQL_SUCCESS then // e.g. driver does not support schema
          status := ODBC.StatisticsA(fStatement,nil,0,nil,0,pointer(Table),
            SQL_NTS,SQL_INDEX_ALL,SQL_QUICK);
        ODBC.Check(Connection,nil,status,SQL_HANDLE_STMT,fStatement);
        BindColumns;
        while Step do begin
          F.ColumnName := Trim(ColumnUTF8(8));
          i := FA.Find(F);
          if i>=0 then
            Fields[i].ColumnIndexed := true;
        end;
      finally
        Free; // TODBCStatement release
      end;
  except
    on Exception do
      Fields := nil;
  end;
end;

procedure TODBCConnectionProperties.GetTableNames(out Tables: TRawUTF8DynArray);
var n: integer;
    schema, tablename: RawUTF8;
begin
  inherited; // first try from SQL, if any (faster)
  if Tables<>nil then
    exit; // already retrieved directly from engine
  try
    with TODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      ODBC.Check(Connection,nil,ODBC.TablesA(fStatement,nil,0,nil,0,nil,0,'TABLE',SQL_NTS),SQL_HANDLE_STMT,fStatement);
      BindColumns;
      n := 0;
      while Step do begin
        schema := Trim(ColumnUTF8(1));
        tablename := Trim(ColumnUTF8(2));
        if schema<>'' then
          tablename := schema+'.'+tablename;
        AddSortedRawUTF8(Tables,n,tablename);
      end;
      SetLength(Tables,n);
    finally
      Free; // TODBCStatement release
    end;
  except
    on Exception do
      SetLength(Tables,0);
  end;
end;

procedure TODBCConnectionProperties.GetViewNames(out Views: TRawUTF8DynArray);
var n: integer;
    schema, tablename: RawUTF8;
begin
  inherited; // first try from SQL, if any (faster)
  if Views<>nil then
    exit; // already retrieved directly from engine
  try
    with TODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      ODBC.Check(Connection,nil,ODBC.TablesA(fStatement,nil,0,nil,0,nil,0,'VIEW',SQL_NTS),SQL_HANDLE_STMT,fStatement);
      BindColumns;
      n := 0;
      while Step do begin
        schema := Trim(ColumnUTF8(1));
        tablename := Trim(ColumnUTF8(2));
        if schema<>'' then
          tablename := schema+'.'+tablename;
        AddSortedRawUTF8(Views,n,tablename);
      end;
      SetLength(Views,n);
    finally
      Free; // TODBCStatement release
    end;
  except
    on Exception do
      SetLength(Views,0);
  end;
end;

procedure TODBCConnectionProperties.GetForeignKeys;
begin
  try
    with TODBCStatement.Create(MainConnection) do
    try
      AllocStatement;
      ODBC.Check(Connection,nil,
        ODBC.ForeignKeysA(fStatement,nil,0,nil,0,nil,0,nil,0,nil,0,'%',SQL_NTS),
        SQL_HANDLE_STMT,fStatement);
      BindColumns;
      while Step do
        fForeignKeys.Add(
          Trim(ColumnUTF8(5))+'.'+Trim(ColumnUTF8(6))+'.'+Trim(ColumnUTF8(7)),
          Trim(ColumnUTF8(1))+'.'+Trim(ColumnUTF8(2))+'.'+Trim(ColumnUTF8(3)));
    finally
      Free; // TODBCStatement release
    end;
  except
    on Exception do ; // just ignore errors here
  end;
end;

procedure TODBCConnectionProperties.GetProcedureNames(out Procedures: TRawUTF8DynArray);
var Schema: RawUTF8;
    n: integer;
    status: SqlReturn;
    Stmt: TODBCStatement;
begin
  inherited; // first try from SQL, if any (faster)
  if Procedures<>nil then
    exit; // already retrieved directly from engine
  SetSchemaNameToOwner(Schema);
  Schema := SynCommons.UpperCase(Schema);
  try
    // get procedure list
    Stmt := TODBCStatement.Create(MainConnection);
    try
      Stmt.AllocStatement;
      status := ODBC.SQLProcedures(Stmt.fStatement,nil,0,pointer(Schema),SQL_NTS,nil,0);
      ODBC.Check(Stmt.Connection,nil,status,SQL_HANDLE_STMT,Stmt.fStatement);
      Stmt.BindColumns;
      n := 0;
      while Stmt.Step do begin
        AddSortedRawUTF8(Procedures,n,Trim(Stmt.ColumnUTF8(2))); // PROCEDURE_NAME column
      end;
      SetLength(Procedures,n);
    finally
      Stmt.Free; // TODBCStatement release
    end;
  except
    on Exception do
      Procedures := nil;
  end;
end;

procedure TODBCConnectionProperties.GetProcedureParameters(const aProcName: RawUTF8;
  out Parameters: TSQLDBProcColumnDefineDynArray);
var Schema, Package, Proc: RawUTF8;
    P: TSQLDBProcColumnDefine;
    PA: TDynArray;
    n,DataType: integer;
    status: SqlReturn;
    Stmt: TODBCStatement;
begin
  inherited; // first try from SQL, if any (faster)
  if Parameters<>nil then
    exit; // already retrieved directly from engine
  SQLSplitProcedureName(aProcName,Schema,Package,Proc);
  Proc := SynCommons.UpperCase(Proc);
  Package := SynCommons.UpperCase(Package);
  Schema := SynCommons.UpperCase(Schema);
  if Package<>'' then
    Proc := Package+'.'+Proc;
  try
    // get column definitions
    Stmt := TODBCStatement.Create(MainConnection);
    try
      Stmt.AllocStatement;
      status := ODBC.SQLProcedureColumnsA(Stmt.fStatement,nil,0,
        pointer(Schema),SQL_NTS,pointer(Proc),SQL_NTS,nil,0);
      if status=SQL_SUCCESS then begin
        Stmt.BindColumns;
        if not Stmt.Step then
          status := SQL_NO_DATA; // no info -> retry without schema
      end;
      if status<>SQL_SUCCESS then begin
        Stmt.DeallocStatement;
        Stmt.AllocStatement;
        status := ODBC.SQLProcedureColumnsA(Stmt.fStatement,nil,0,
          nil,0,pointer(Proc),SQL_NTS,nil,0);
        ODBC.Check(Stmt.Connection,nil,status,SQL_HANDLE_STMT,Stmt.fStatement);
        Stmt.BindColumns;
        Stmt.Step;
      end;
      PA.Init(TypeInfo(TSQLDBColumnDefineDynArray),Parameters,@n);
      FillcharFast(P,SizeOf(P),0);
      if Stmt.fCurrentRow>0 then // Step done above
      repeat
        P.ColumnName := Trim(Stmt.ColumnUTF8(3)); // Column*() should be in order
        case Stmt.ColumnInt(4) of
          SQL_PARAM_INPUT:        P.ColumnParamType := paramIn;
          SQL_PARAM_INPUT_OUTPUT: P.ColumnParamType := paramInOut;
          else                    P.ColumnParamType := paramOut;
        end;
        DataType := Stmt.ColumnInt(5);
        P.ColumnTypeNative := Trim(Stmt.ColumnUTF8(6));
        P.ColumnLength := Stmt.ColumnInt(7);
        P.ColumnScale := Stmt.ColumnInt(8);
        P.ColumnPrecision := Stmt.ColumnInt(9);
        P.ColumnType:= ODBCColumnToFieldType(DataType,P.ColumnPrecision,P.ColumnScale);
        PA.Add(P);
      until not Stmt.Step;
      SetLength(Parameters,n);
    finally
      Stmt.Free; // TODBCStatement release
    end;
  except
    on Exception do
      Parameters := nil;
  end;
end;

function TODBCConnectionProperties.GetDatabaseNameSafe: RawUTF8;
var
  lPWD: RawUTF8;
begin
  lPWD := FindIniNameValue(pointer(StringReplaceAll(fDatabaseName,';',sLineBreak)), 'PWD=');
  result := StringReplaceAll(fDatabaseName,lPWD,'***');
end;

function TODBCConnectionProperties.GetDBMS: TSQLDBDefinition;
begin
  if fDBMS=dUnknown then
    with MainConnection as TODBCConnection do begin
      if not IsConnected then
        Connect; // retrieve DBMS property
      self.fDBMS := DBMS;
    end;
  result := fDBMS;
end;


{ SQL_TIMESTAMP_STRUCT }

function SQL_TIMESTAMP_STRUCT.From(DateTime: TDateTime; var ColumnSize: SqlLen): SqlSmallint;
var Y,MS: word;
begin
  DecodeDate(DateTime,Y,Month,Day);
  Year := Y;
  if frac(DateTime)=0 then begin
    PInt64(@Hour)^ := 0;
    Fraction := 0;
  end else begin
    DecodeTime(DateTime,Hour,Minute,Second,MS);
    Fraction := SqlUInteger(MS)*1000000;
  end;
  if PInt64(@Hour)^=0 then begin
    result := SQL_C_TYPE_DATE;
    ColumnSize := SizeOf(SqlUSmallint)*3;
  end else begin
    result := SQL_C_TYPE_TIMESTAMP;
    ColumnSize := SizeOf(SQL_TIMESTAMP_STRUCT);
  end;
end;

function SQL_TIMESTAMP_STRUCT.ToDateTime(DataType: SqlSmallint=SQL_TYPE_TIMESTAMP): TDateTime;
var time: TDateTime;
begin
  if DataType=SQL_TYPE_TIME then
    result := 0 else
    result := EncodeDate(Year,Month,Day);
  if (DataType<>SQL_TYPE_DATE) and (PInt64(@Hour)^<>0) and
     TryEncodeTime(Hour,Minute,Second,Fraction div 1000000,time) then
    result := result+time;
end;

function SQL_TIMESTAMP_STRUCT.ToIso8601(Dest: PUTF8Char; DataType: SqlSmallint;
  WithMS: boolean=false): integer;
begin
  Dest^ := '"';
  inc(Dest);
  if DataType<>SQL_TYPE_TIME then begin
    DateToIso8601PChar(Dest,true,Year,Month,Day);
    inc(Dest,10);
  end;
  if (DataType<>SQL_TYPE_DATE) and (PInt64(@Hour)^<>0) and (Hour<24) and
     (Minute<60) and (Second<60) then begin // we use 'T' as TTextWriter.AddDateTime
    TimeToIso8601PChar(Dest,true,Hour,Minute,Second,Fraction div 1000000,'T',WithMS);
    if WithMS then begin
      inc(Dest,13);
      result := 25;
    end else begin
      inc(Dest,9);
      result := 21;
    end;
  end else
    result := 12; // only date
  Dest^ := '"';
end;

initialization
  TODBCConnectionProperties.RegisterClassNameForDefinition;
end.
