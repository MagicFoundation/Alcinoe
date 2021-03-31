{*******************************************************************************
An object to query Sqlite3 database and get the result In Xml format or in
Json/Bson format
*******************************************************************************}

unit AlSqlite3Client;

interface

Uses
  System.SysUtils,
  System.Contnrs,
  System.SyncObjs,
  AlXmlDoc,
  ALJsonDoc,
  AlSqlite3Wrapper,
  ALString,
  ALStringList;

Type

  {-----------------------------------------------------------------------------------------}
  TalSqlite3ClientSelectXMLDataOnNewRowFunct = reference to Procedure(XMLRowData: TalXmlNode;
                                                                      const ViewTag: AnsiString;
                                                                      ExtData: Pointer;
                                                                      Var Continue: Boolean);
  TalSqlite3ClientSelectJSONDataOnNewRowFunct = reference to Procedure(JSONRowData: TALJSONNode;
                                                                       const ViewTag: AnsiString;
                                                                       ExtData: Pointer;
                                                                       Var Continue: Boolean);

  {--------------------------------}
  EALSqlite3Error = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(const aErrorMsg: AnsiString; aErrorCode: Integer); overload;
    property ErrorCode: Integer read FErrorCode;
  end;

  {-------------------------------}
  TalSqlite3Client = Class(Tobject)
  private
    fLibrary: TALSqlite3Library;
    FownLibrary: Boolean;
    fSqlite3: SQLite3;
    fNullString: AnsiString;
    finTransaction: Boolean;
    function  GetConnected: Boolean;
    function  GetInTransaction: Boolean;
  protected
    function loadCachedData(const Key: AnsiString;
                            var DataStr: AnsiString): Boolean; virtual;
    Procedure SaveDataToCache(const Key: ansiString;
                              const CacheThreshold: integer;
                              const DataStr: ansiString); virtual;
    procedure CheckAPIError(Error: Boolean);
    procedure initObject; virtual;
    procedure OnSelectDataDone(const SQL: AnsiString;
                               const RowTag: AnsiString;
                               const ViewTag: AnsiString;
                               Skip: integer;
                               First: Integer;
                               CacheThreshold: Integer;
                               TimeTaken: Double); virtual;
    procedure OnUpdateDataDone(const SQL: AnsiString;
                               TimeTaken: Double); virtual;
  public
    Constructor Create(const lib: AnsiString = 'sqlite3.dll'; const initializeLib: Boolean = True); overload; virtual;
    Constructor Create(lib: TALSqlite3Library); overload; virtual;
    Destructor Destroy; Override;
    procedure config(Option: Integer);
    procedure initialize; //can not be put in the create because config can/must be call prior initialize
    procedure shutdown;   //can not be put in the create because config can/must be call after shutdown
    procedure enable_shared_cache(enable: boolean);
    Procedure Connect(const DatabaseName: AnsiString;
                      const flags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE);
    Procedure Disconnect;
    Procedure TransactionStart;
    Procedure TransactionCommit;
    Procedure TransactionRollback;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         const ViewTag: AnsiString;
                         Skip: integer;  // used only if value is > 0
                         First: Integer; // used only if value is > 0
                         CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                   // cache or not. Values <= 0 deactivate the cache
                         JSONDATA: TALJSONNode;
                         OnNewRowFunct: TalSqlite3ClientSelectJSONDataOnNewRowFunct;
                         ExtData: Pointer); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalSqlite3ClientSelectJsonDataOnNewRowFunct;
                         ExtData: Pointer); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TalSqlite3ClientSelectJsonDataOnNewRowFunct;
                         ExtData: Pointer); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         Skip: integer;
                         First: Integer;
                         JsonDATA: TalJsonNode); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         JsonDATA: TalJsonNode); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         JsonDATA: TalJsonNode); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         const ViewTag: AnsiString;
                         Skip: integer;  // used only if value is > 0
                         First: Integer; // used only if value is > 0
                         CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                   // cache or not. Values <= 0 deactivate the cache
                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    procedure UpdateData(SQLs: TALStrings); overload; virtual;
    procedure UpdateData(const SQL: AnsiString); overload; virtual;
    procedure UpdateData(const SQLs: array of AnsiString); overload; virtual;
    Property  Connected: Boolean Read GetConnected;
    Property  InTransaction: Boolean read GetInTransaction;
    Property  NullString: AnsiString Read fNullString Write fNullString;
    property  Lib: TALSqlite3Library read FLibrary;
  end;

  {----------------------------------------}
  TalSqlite3ConnectionPoolContainer = record
    ConnectionHandle: SQLite3;
    LastAccessDate: UInt64;
  End;
  TalSqlite3ConnectionPool = array of TalSqlite3ConnectionPoolContainer;

  {---------------------------------------------}
  TalSqlite3ConnectionPoolClient = Class(Tobject)
  Private
    FLibrary: TALSqlite3Library;
    FownLibrary: Boolean;
    FConnectionPool: TalSqlite3ConnectionPool;
    FConnectionPoolCount: integer;
    FConnectionPoolCapacity: integer;
    FConnectionPoolCS: TCriticalSection;
    FDatabaseRWCS: TMultiReadExclusiveWriteSynchronizer;
    FDatabaseWriteLocked: Boolean;
    FWorkingConnectionCount: Integer;
    FReleasingAllconnections: Boolean;
    FLastConnectionGarbage: UInt64;
    FConnectionMaxIdleTime: integer;
    FDataBaseName: AnsiString;
    FOpenConnectionFlags: integer;
    FOpenConnectionPragmaStatements: TALStrings;
    FNullString: AnsiString;
  Protected
    function loadCachedData(const Key: AnsiString;
                            var DataStr: AnsiString): Boolean; virtual;
    Procedure SaveDataToCache(const Key: ansiString;
                              const CacheThreshold: integer;
                              const DataStr: ansiString); virtual;
    procedure CheckAPIError(ConnectionHandle: SQLite3; Error: Boolean);
    function  GetDataBaseName: AnsiString; virtual;
    procedure initObject(const aDataBaseName: AnsiString;
                         const aOpenConnectionFlags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
                         const aOpenConnectionPragmaStatements: AnsiString = ''); virtual;
    Function  AcquireConnection(const readonly: boolean = False): SQLite3; virtual;
    Procedure ReleaseConnection(var ConnectionHandle: SQLite3;
                                const CloseConnection: Boolean = False); virtual;
    procedure OnSelectDataDone(const SQL: AnsiString;
                               const RowTag: AnsiString;
                               const ViewTag: AnsiString;
                               Skip: integer;
                               First: Integer;
                               CacheThreshold: Integer;
                               TimeTaken: Double); virtual;
    procedure OnUpdateDataDone(const SQL: AnsiString;
                               TimeTaken: Double); virtual;
  Public
    Constructor Create(const aDataBaseName: AnsiString;
                       const aOpenConnectionFlags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
                       const aOpenConnectionPragmaStatements: AnsiString = '';
                       const alib: AnsiString = 'sqlite3.dll';
                       const initializeLib: Boolean = True); overload; virtual;
    Constructor Create(const aDataBaseName: AnsiString;
                       alib: TALSqlite3Library;
                       const aOpenConnectionFlags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
                       const aOpenConnectionPragmaStatements: AnsiString = ''); overload; virtual;
    Destructor  Destroy; Override;
    procedure config(Option: Integer);
    procedure initialize; //can not be put in the create because config can/must be call prior initialize
    procedure shutdown;   //can not be put in the create because config can/must be call after shutdown
    procedure enable_shared_cache(enable: boolean);
    Procedure ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True); virtual;
    Procedure TransactionStart(Var ConnectionHandle: SQLite3; const ReadOnly: boolean = False); virtual;
    Procedure TransactionCommit(var ConnectionHandle: SQLite3;
                                const CloseConnection: Boolean = False); virtual;
    Procedure TransactionRollback(var ConnectionHandle: SQLite3;
                                  const CloseConnection: Boolean = False); virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         const ViewTag: AnsiString;
                         Skip: integer;  // used only if value is > 0
                         First: Integer; // used only if value is > 0
                         CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                   // cache or not. Values <= 0 deactivate the cache
                         JSONDATA: TALJSONNode;
                         OnNewRowFunct: TalSqlite3ClientSelectJSONDataOnNewRowFunct;
                         ExtData: Pointer;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalSqlite3ClientSelectJsonDataOnNewRowFunct;
                         ExtData: Pointer;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TalSqlite3ClientSelectJsonDataOnNewRowFunct;
                         ExtData: Pointer;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         Skip: integer;
                         First: Integer;
                         JsonDATA: TalJsonNode;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         JsonDATA: TalJsonNode;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         JsonDATA: TalJsonNode;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         const ViewTag: AnsiString;
                         Skip: integer;  // used only if value is > 0
                         First: Integer; // used only if value is > 0
                         CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                   // cache or not. Values <= 0 deactivate the cache
                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    procedure UpdateData(SQLs: TALStrings;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    procedure UpdateData(const SQL: AnsiString;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    procedure UpdateData(const SQLs: array of AnsiString;
                         const ConnectionHandle: SQLite3 = nil); overload; virtual;
    Function  ConnectionCount: Integer;
    Function  WorkingConnectionCount: Integer;
    property  DataBaseName: AnsiString read GetDataBaseName;
    property  ConnectionMaxIdleTime: integer read FConnectionMaxIdleTime write fConnectionMaxIdleTime;
    Property  NullString: AnsiString Read fNullString Write fNullString;
    property  Lib: TALSqlite3Library read FLibrary;
  end;

implementation

Uses
  Winapi.Windows,
  System.classes,
  System.Diagnostics,
  ALCipher,
  ALWindows;

{***********************************************************************************}
constructor EALSqlite3Error.Create(const aErrorMsg: AnsiString; aErrorCode: Integer);
begin
  fErrorCode := aErrorCode;
  inherited create(String(aErrorMsg));
end;

{**********************************************}
function TalSqlite3Client.GetConnected: Boolean;
begin
  result := assigned(fSQLite3);
end;

{**************************************************}
function TalSqlite3Client.GetInTransaction: Boolean;
begin
  result := finTransaction;
end;

{*************************************************************}
function TalSqlite3Client.loadCachedData(const Key: AnsiString;
                                         var DataStr: AnsiString): Boolean;
begin
  result := false; //virtual need to be overriden
end;

{***************************************************************}
Procedure TalSqlite3Client.SaveDataToCache(const Key: ansiString;
                                           const CacheThreshold: integer;
                                           const DataStr: ansiString);
begin
  //virtual need to be overriden
end;

{*******************************************************}
procedure TalSqlite3Client.CheckAPIError(Error: Boolean);
Begin
  if Error then begin
    if assigned(Fsqlite3) then raise EALSqlite3Error.Create(AnsiString(fLibrary.sqlite3_errmsg(Fsqlite3)), fLibrary.sqlite3_errcode(Fsqlite3)) // !! take care that sqlite3_errmsg(Fsqlite3) return an UTF8 !!
    else raise EALSqlite3Error.Create('Sqlite3 error', -1);
  end;
end;

{************************************}
procedure TalSqlite3Client.initObject;
begin
  fSqlite3 := nil;
  finTransaction := False;
  fNullString := '';
end;

{**************************************************************************************************************}
constructor TalSqlite3Client.Create(const lib: AnsiString = 'sqlite3.dll'; const initializeLib: Boolean = True);
begin
  fLibrary := TALSqlite3Library.Create;
  try
    fLibrary.Load(lib, initializeLib);
    FownLibrary := True;
    initObject;
  Except
    fLibrary.free;
    raise;
  end;
end;

{**********************************************************}
constructor TalSqlite3Client.Create(lib: TALSqlite3Library);
begin
  fLibrary := lib;
  FownLibrary := False;
  initObject;
end;

{**********************************}
destructor TalSqlite3Client.Destroy;
begin
  If Connected then disconnect;
  if FownLibrary then fLibrary.Free;
  inherited;
end;

{*****************************************************************************
The sqlite3_config() interface is used to make global configuration changes to
SQLite in order to tune SQLite to the specific needs of the application. The
default configuration is recommended for most applications and so this routine
is usually not necessary. It is provided to support rare applications with
unusual needs.

The sqlite3_config() interface is not threadsafe. The application must insure
that no other SQLite interfaces are invoked by other threads while
sqlite3_config() is running. Furthermore, sqlite3_config() may only be invoked
prior to library initialization using sqlite3_initialize() or after shutdown by
sqlite3_shutdown(). If sqlite3_config() is called after sqlite3_initialize() and
before sqlite3_shutdown() then it will return SQLITE_MISUSE. Note, however, that
sqlite3_config() can be called as part of the implementation of an
application-defined sqlite3_os_init().

SQLITE_CONFIG_SINGLETHREAD
  There are no arguments to this option. This option sets the threading mode to
  Single-thread. In other words, it disables all mutexing and puts SQLite into a
  mode where it can only be used by a single thread. If SQLite is compiled with
  the SQLITE_THREADSAFE=0 compile-time option then it is not possible to change
  the threading mode from its default value of Single-thread and so
  sqlite3_config() will return SQLITE_ERROR if called with the
  SQLITE_CONFIG_SINGLETHREAD configuration option.

SQLITE_CONFIG_MULTITHREAD
  There are no arguments to this option. This option sets the threading mode to
  Multi-thread. In other words, it disables mutexing on database connection and
  prepared statement objects. The application is responsible for serializing
  access to database connections and prepared statements. But other mutexes are
  enabled so that SQLite will be safe to use in a multi-threaded environment as
  long as no two threads attempt to use the same database connection at the same
  time. If SQLite is compiled with the SQLITE_THREADSAFE=0 compile-time option
  then it is not possible to set the Multi-thread threading mode and
  sqlite3_config() will return SQLITE_ERROR if called with the
  SQLITE_CONFIG_MULTITHREAD configuration option.

SQLITE_CONFIG_SERIALIZED
  There are no arguments to this option. This option sets the threading mode to
  Serialized. In other words, this option enables all mutexes including the
  recursive mutexes on database connection and prepared statement objects. In
  this mode (which is the default when SQLite is compiled with
  SQLITE_THREADSAFE=1) the SQLite library will itself serialize access to
  database connections and prepared statements so that the application is free
  to use the same database connection or the same prepared statement in
  different threads at the same time. If SQLite is compiled with the
  SQLITE_THREADSAFE=0 compile-time option then it is not possible to set the
  Serialized threading mode and sqlite3_config() will return SQLITE_ERROR if
  called with the SQLITE_CONFIG_SERIALIZED configuration option.}
procedure TalSqlite3Client.config(Option: Integer);
begin
  CheckAPIError(FLibrary.sqlite3_config(Option) <> SQLITE_OK);
end;

{************************************}
procedure TalSqlite3Client.initialize;
begin
  CheckAPIError(FLibrary.sqlite3_initialize <> SQLITE_OK);
end;

{**********************************}
procedure TalSqlite3Client.shutdown;
begin
  CheckAPIError(FLibrary.sqlite3_shutdown <> SQLITE_OK);
end;

{****************************************************************************
This routine enables or disables the sharing of the database cache and schema
data structures between connections to the same database. Sharing is enabled if
the argument is true and disabled if the argument is false.

Cache sharing is enabled and disabled for an entire process. This is a change as
of SQLite version 3.5.0. In prior versions of SQLite, sharing was enabled or
disabled for each thread separately.

The cache sharing mode set by this interface effects all subsequent calls to
sqlite3_open(), sqlite3_open_v2(), and sqlite3_open16(). Existing database
connections continue use the sharing mode that was in effect at the time they
were opened.

This routine returns SQLITE_OK if shared cache was enabled or disabled
successfully. An error code is returned otherwise.

Shared cache is disabled by default. But this might change in future releases of
SQLite. Applications that care about shared cache setting should set it
explicitly.

Note: This method is disabled on MacOS X 10.7 and iOS version 5.0 and will
always return SQLITE_MISUSE. On those systems, shared cache mode should be
enabled per-database connection via sqlite3_open_v2() with
SQLITE_OPEN_SHAREDCACHE.

This interface is threadsafe on processors where writing a 32-bit integer is
atomic.}
procedure TalSqlite3Client.enable_shared_cache(enable: boolean);
begin
  if enable then CheckAPIError(FLibrary.sqlite3_enable_shared_cache(1) <> SQLITE_OK)
  else CheckAPIError(FLibrary.sqlite3_enable_shared_cache(0) <> SQLITE_OK);
end;

{**********************************************************
The flags parameter to sqlite3_open_v2() can take one of the following three
values, optionally combined with the SQLITE_OPEN_NOMUTEX, SQLITE_OPEN_FULLMUTEX,
SQLITE_OPEN_SHAREDCACHE, SQLITE_OPEN_PRIVATECACHE, and/or SQLITE_OPEN_URI flags:

SQLITE_OPEN_READONLY
  The database is opened in read-only mode. If the database does not already exist,
  an error is returned.

SQLITE_OPEN_READWRITE
  The database is opened for reading and writing if possible, or reading only if
  the file is write protected by the operating system. In either case the
  database must already exist, otherwise an error is returned.

SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE
  The database is opened for reading and writing, and is created if it does not
  already exist. This is the behavior that is always used for sqlite3_open() and
  sqlite3_open16().

If the SQLITE_OPEN_NOMUTEX flag is set, then the database connection opens in
the multi-thread threading mode as long as the single-thread mode has not been
set at compile-time or start-time. If the SQLITE_OPEN_FULLMUTEX flag is set then
the database connection opens in the serialized threading mode unless
single-thread was previously selected at compile-time or start-time.
The SQLITE_OPEN_SHAREDCACHE flag causes the database connection to be eligible
to use shared cache mode, regardless of whether or not shared cache is enabled
using sqlite3_enable_shared_cache(). The SQLITE_OPEN_PRIVATECACHE flag causes
the database connection to not participate in shared cache mode even if it
is enabled.

If the filename is ":memory:", then a private, temporary in-memory database is
created for the connection. This in-memory database will vanish when the
database connection is closed. Future versions of SQLite might make use of
additional special filenames that begin with the ":" character. It is
recommended that when a database filename actually does begin with a ":"
character you should prefix the filename with a pathname such as "./" to avoid
ambiguity.

If the filename is an empty string, then a private, temporary on-disk database
will be created. This private database will be automatically deleted as soon as
the database connection is closed.}
procedure TalSqlite3Client.connect(const DatabaseName: AnsiString;
                                   const flags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE);
begin
  if connected then raise Exception.Create('Already connected');
  Try
    CheckAPIError(fLibrary.sqlite3_open_v2(PAnsiChar(DatabaseName), FSqlite3, flags, nil) <> SQLITE_OK);
  Except
    //A database connection handle is usually returned in *ppDb, even if an error occurs.
    //Whether or not an error occurs when it is opened, resources associated with the
    //database connection handle should be released by passing it to sqlite3_close()
    //when it is no longer required
    if assigned(FSqlite3) then fLibrary.sqlite3_close_v2(FSqlite3);
    FSqlite3 := nil;
    Raise;
  End;
end;

{****************************************************************************************
{Applications must finalize all prepared statements and close all BLOB handles associated
 with the sqlite3 object prior to attempting to close the object. If sqlite3_close() is
 called on a database connection that still has outstanding prepared statements or
 BLOB handles, then it returns SQLITE_BUSY.
 If sqlite3_close() is invoked while a transaction is open, the transaction is
 automatically rolled back.}
procedure TalSqlite3Client.Disconnect;
begin
  If not connected then exit;
  if InTransaction then TransactionRollback;
  try
    FLibrary.sqlite3_close_v2(FSqlite3);
  Except
    //Disconnect must be a "safe" procedure because it's mostly called in
    //finalization part of the code that it is not protected
    //that the bulsheet of SQLite3 to answer SQLITE_BUSY instead of free
    //everything
  End;
  FSqlite3 := Nil;
end;

{******************************************}
procedure TalSqlite3Client.TransactionStart;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');
  if InTransaction then raise Exception.Create('Another transaction is active');

  //execute the query
  UpdateData('BEGIN TRANSACTION');
  finTransaction := True;

end;

{*******************************************}
procedure TalSqlite3Client.TransactionCommit;
begin

  //Error if we are not connected
  if not InTransaction then raise Exception.Create('No active transaction to commit');

  //Execute the Query
  UpdateData('COMMIT TRANSACTION');
  finTransaction := False;

end;

{*********************************************}
procedure TalSqlite3Client.TransactionRollback;
begin

  //Error if we are not connected
  if not InTransaction then raise Exception.Create('No active transaction to rollback');

  //Execute the Query
  Try
    UpdateData('ROLLBACK TRANSACTION');
  Except
    //some error can happen if the network go down for exemple
    //i don't really know what to do in this case of error
    //but what else we can do ? commit => exept => rollback => except ???
  End;
  finTransaction := False;

end;

{****************************************************************}
procedure TalSqlite3Client.OnSelectDataDone(const SQL: AnsiString;
                                            const RowTag: AnsiString;
                                            const ViewTag: AnsiString;
                                            Skip: integer;
                                            First: Integer;
                                            CacheThreshold: Integer;
                                            TimeTaken: Double);
begin
  // virtual
end;

{****************************************************************}
procedure TalSqlite3Client.OnUpdateDataDone(const SQL: AnsiString;
                                            TimeTaken: Double);
begin
  // virtual
end;

{**********************************************************}
Procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      const RowTag: AnsiString;
                                      const ViewTag: AnsiString;
                                      Skip: integer;  // used only if value is > 0
                                      First: Integer; // used only if value is > 0
                                      CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                                // cache or not. Values <= 0 deactivate the cache
                                      JSONDATA: TALJSONNode;
                                      OnNewRowFunct: TalSqlite3ClientSelectJSONDataOnNewRowFunct;
                                      ExtData: Pointer);

Var LStmt: SQLite3_Stmt;
    LStepResult: integer;
    LColumnCount: Integer;
    LColumnIndex: integer;
    LColumnNames: Array of AnsiString;
    LNewRec: TalJsonNode;
    LValueRec: TalJsonNode;
    LViewRec: TalJsonNode;
    LRecIndex: integer;
    LRecAdded: integer;
    LContinue: Boolean;
    LJsonDocument: TalJsonDocument;
    LUpdateRowTagByFieldValue: Boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;
    LTmpRowTag: ansiString;

begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //only OnNewRowFunct / JsonDATA can be used
  if assigned(OnNewRowFunct) then JsonDATA := nil;

  //clear the JsonDATA
  if assigned(JsonDATA) then LJsonDocument := Nil
  else begin
    LJsonDocument := TALJsonDocument.create;
    JsonDATA := LJsonDocument.Node;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LJsonDocument)) and
       ((Jsondata.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(AlFormat('BSON#%s#%s#%s#%s', [RowTag,
                                                                  alinttostr(Skip),
                                                                  alinttostr(First),
                                                                  SQL]));
      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := Jsondata.AddChild(ViewTag, ntObject)
        else LViewRec := Jsondata;

        //assign the tmp data to the JsonData
        LViewRec.LoadFromBsonString(LCacheStr, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //start the TstopWatch
    LStopWatch.Reset;
    LStopWatch.Start;

    //prepare the query
    LStmt := nil;
    CheckAPIError(FLibrary.sqlite3_prepare_v2(FSqlite3, PAnsiChar(SQL), length(SQL), LStmt, nil) <> SQLITE_OK);
    Try

      //Return the number of columns in the result set returned by the
      //prepared statement. This routine returns 0 if pStmt is an SQL statement
      //that does not return data (for example an UPDATE).
      LColumnCount := FLibrary.sqlite3_column_count(LStmt);

      //init the aColumnNames array
      setlength(LColumnNames,LColumnCount);
      For LColumnIndex := 0 to LColumnCount - 1 do
        LColumnNames[LColumnIndex] := FLibrary.sqlite3_column_name(LStmt, LColumnIndex);

      //init the aViewRec
      if (ViewTag <> '') and (not assigned(LJsonDocument)) then LViewRec := Jsondata.AddChild(ViewTag, ntObject)
      else LViewRec := Jsondata;

      //init aUpdateRowTagByFieldValue
      if AlPos('&>',RowTag) = 1 then begin
        LTmpRowTag := ALcopyStr(RowTag,3,maxint);
        LUpdateRowTagByFieldValue := LTmpRowTag <> '';
      end
      else begin
        LTmpRowTag := RowTag;
        LUpdateRowTagByFieldValue := False;
      end;

      //loop throught all row
      LRecIndex := 0;
      LRecAdded := 0;
      while True do begin

        //retrieve the next row
        LStepResult := FLibrary.sqlite3_step(LStmt);

        //break if no more row
        if LStepResult = SQLITE_DONE then break

        //download the row
        else if LStepResult = SQLITE_ROW then begin

          //process if > Skip
          inc(LRecIndex);
          If LRecIndex > Skip then begin

            //init NewRec
            if (LTmpRowTag <> '') and (not assigned(LJsonDocument)) then LNewRec := LViewRec.AddChild(LTmpRowTag, ntObject)
            Else LNewRec := LViewRec;

            //loop throught all column
            For LColumnIndex := 0 to LColumnCount - 1 do begin
              LValueRec := LNewRec.AddChild(ALlowercase(LColumnNames[LColumnIndex]));
              Case FLibrary.sqlite3_column_type(LStmt, LColumnIndex) of
                SQLITE_INTEGER: LValueRec.int64 := FLibrary.sqlite3_column_int64(LStmt, LColumnIndex);
                SQLITE_FLOAT: LValueRec.Float := FLibrary.sqlite3_column_double(LStmt, LColumnIndex);
                SQLITE_TEXT: LValueRec.Text :=  AnsiString(FLibrary.sqlite3_column_text(LStmt, LColumnIndex)); // Strings returned by sqlite3_column_text() and sqlite3_column_text16(), even empty strings, are always zero-terminated.
                SQLITE_NULL: LValueRec.Null := true;
                //SQLITE_BLOB: todo
                else raise Exception.Create('Unsupported column type');
              end;
              if LUpdateRowTagByFieldValue and (LValueRec.NodeName=LNewRec.NodeName) then LNewRec.NodeName := ALLowerCase(LValueRec.Text);
            end;

            //handle OnNewRowFunct
            if assigned(OnNewRowFunct) then begin
              LContinue := True;
              OnNewRowFunct(LNewRec, ViewTag, ExtData, LContinue);
              if Not LContinue then Break;
            end;

            //free the node if aJsonDocument
            if assigned(LJsonDocument) then LJsonDocument.Node.ChildNodes.Clear;

            //handle the First
            inc(LRecAdded);
            If (First > 0) and (LRecAdded >= First) then Break;

          end;

        end

        //misc error, raise an exception
        else CheckAPIError(True);

      end;

    Finally
      //free the memory used by the API
      CheckAPIError(FLibrary.sqlite3_finalize(LStmt) <> SQLITE_OK);
    End;

    //do the OnSelectDataDone
    LStopWatch.Stop;
    OnSelectDataDone(SQL,
                     RowTag,
                     ViewTag,
                     Skip,
                     First,
                     CacheThreshold,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //save to the cache
    If LCacheKey <> '' then begin

      //save the data
      LViewRec.SaveToBsonString(LCacheStr);
      SaveDataToCache(LCacheKey,
                      CacheThreshold,
                      LCacheStr);

    end;

  Finally
    if assigned(LJsonDocument) then LJsonDocument.free;
  End;

end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      Skip: Integer;
                                      First: Integer;
                                      OnNewRowFunct: TalSqlite3ClientSelectJsonDataOnNewRowFunct;
                                      ExtData: Pointer);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, // JsonDATA,
             OnNewRowFunct,
             ExtData);
end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      OnNewRowFunct: TalSqlite3ClientSelectJsonDataOnNewRowFunct;
                                      ExtData: Pointer);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // JsonDATA,
             OnNewRowFunct,
             ExtData);
end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      const RowTag: AnsiString;
                                      Skip: Integer;
                                      First: Integer;
                                      JsonDATA: TalJsonNode);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             JsonDATA,
             nil, // OnNewRowFunct,
             nil); // ExtData,
end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      const RowTag: AnsiString;
                                      JsonDATA: TalJsonNode);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JsonDATA,
             nil, // OnNewRowFunct,
             nil); // ExtData,
end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      JsonDATA: TalJsonNode);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JsonDATA,
             nil, // OnNewRowFunct,
             nil); // ExtData,
end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      const RowTag: AnsiString;
                                      const ViewTag: AnsiString;
                                      Skip: integer;  // used only if value is > 0
                                      First: Integer; // used only if value is > 0
                                      CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                               // cache or not. Values <= 0 deactivate the cache
                                      XMLDATA: TalXMLNode;
                                      OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      const FormatSettings: TALFormatSettings);

Var LStmt: SQLite3_Stmt;
    LStepResult: integer;
    LColumnCount: Integer;
    LColumnIndex: integer;
    LColumnNames: Array of AnsiString;
    LNewRec: TalXmlNode;
    LValueRec: TalXmlNode;
    LViewRec: TalXmlNode;
    LRecIndex: integer;
    LRecAdded: integer;
    LContinue: Boolean;
    LXmlDocument: TalXmlDocument;
    LUpdateRowTagByFieldValue: Boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;
    LTmpRowTag: ansiString;

begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //only OnNewRowFunct / XMLDATA can be used
  if assigned(OnNewRowFunct) then XMLDATA := nil;

  //clear the XMLDATA
  if assigned(XMLDATA) then LXmlDocument := Nil
  else begin
    LXmlDocument := TALXmlDocument.create('root');
    XMLDATA := LXmlDocument.DocumentElement;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LXmlDocument)) and
       ((XMLdata.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(AlFormat('XML#%s#%s#%s#%s#%s', [RowTag,
                                                                    alinttostr(Skip),
                                                                    alinttostr(First),
                                                                    ALGetFormatSettingsID(FormatSettings),
                                                                    SQL]));
      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := XMLdata.AddChild(ViewTag)
        else LViewRec := XMLdata;

        //assign the tmp data to the XMLData
        LViewRec.LoadFromXML(LCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //start the TstopWatch
    LStopWatch.Reset;
    LStopWatch.Start;

    //prepare the query
    LStmt := nil;
    CheckAPIError(FLibrary.sqlite3_prepare_v2(FSqlite3, PAnsiChar(SQL), length(SQL), LStmt, nil) <> SQLITE_OK);
    Try

      //Return the number of columns in the result set returned by the
      //prepared statement. This routine returns 0 if pStmt is an SQL statement
      //that does not return data (for example an UPDATE).
      LColumnCount := FLibrary.sqlite3_column_count(LStmt);

      //init the aColumnNames array
      setlength(LColumnNames,LColumnCount);
      For LColumnIndex := 0 to LColumnCount - 1 do
        LColumnNames[LColumnIndex] := FLibrary.sqlite3_column_name(LStmt, LColumnIndex);

      //init the aViewRec
      if (ViewTag <> '') and (not assigned(LXmlDocument)) then LViewRec := XMLdata.AddChild(ViewTag)
      else LViewRec := XMLdata;

      //init aUpdateRowTagByFieldValue
      if AlPos('&>',RowTag) = 1 then begin
        LTmpRowTag := ALcopyStr(RowTag,3,maxint);
        LUpdateRowTagByFieldValue := LTmpRowTag <> '';
      end
      else begin
        LTmpRowTag := RowTag;
        LUpdateRowTagByFieldValue := False;
      end;

      //loop throught all row
      LRecIndex := 0;
      LRecAdded := 0;
      while True do begin

        //retrieve the next row
        LStepResult := FLibrary.sqlite3_step(LStmt);

        //break if no more row
        if LStepResult = SQLITE_DONE then break

        //download the row
        else if LStepResult = SQLITE_ROW then begin

          //process if > Skip
          inc(LRecIndex);
          If LRecIndex > Skip then begin

            //init NewRec
            if (LTmpRowTag <> '') and (not assigned(LXmlDocument)) then LNewRec := LViewRec.AddChild(LTmpRowTag)
            Else LNewRec := LViewRec;

            //loop throught all column
            For LColumnIndex := 0 to LColumnCount - 1 do begin
              LValueRec := LNewRec.AddChild(ALlowercase(LColumnNames[LColumnIndex]));
              Case FLibrary.sqlite3_column_type(LStmt, LColumnIndex) of
                SQLITE_FLOAT: LValueRec.Text := ALFloattostr(FLibrary.sqlite3_column_double(LStmt, LColumnIndex), FormatSettings);
                SQLITE_INTEGER,
                SQLITE3_TEXT: LValueRec.Text :=  AnsiString(FLibrary.sqlite3_column_text(LStmt, LColumnIndex)); // Strings returned by sqlite3_column_text() and sqlite3_column_text16(), even empty strings, are always zero-terminated.
                SQLITE_NULL: LValueRec.Text := fNullString;
                //SQLITE_BLOB: todo
                else raise Exception.Create('Unsupported column type');
              end;
              if LUpdateRowTagByFieldValue and (LValueRec.NodeName=LNewRec.NodeName) then LNewRec.NodeName := ALLowerCase(LValueRec.Text);
            end;

            //handle OnNewRowFunct
            if assigned(OnNewRowFunct) then begin
              LContinue := True;
              OnNewRowFunct(LNewRec, ViewTag, ExtData, LContinue);
              if Not LContinue then Break;
            end;

            //free the node if aXmlDocument
            if assigned(LXmlDocument) then LXmlDocument.DocumentElement.ChildNodes.Clear;

            //handle the First
            inc(LRecAdded);
            If (First > 0) and (LRecAdded >= First) then Break;

          end;

        end

        //misc error, raise an exception
        else CheckAPIError(True);

      end;

    Finally
      //free the memory used by the API
      CheckAPIError(FLibrary.sqlite3_finalize(LStmt) <> SQLITE_OK);
    End;

    //do the OnSelectDataDone
    LStopWatch.Stop;
    OnSelectDataDone(SQL,
                     RowTag,
                     ViewTag,
                     Skip,
                     First,
                     CacheThreshold,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //save to the cache
    If LCacheKey <> '' then begin

      //save the data
      LViewRec.SaveToXML(LCacheStr, true{SaveOnlyChildNodes});
      SaveDataToCache(LCacheKey,
                      CacheThreshold,
                      LCacheStr);

    end;

  Finally
    if assigned(LXmlDocument) then LXmlDocument.free;
  End;

end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      Skip: Integer;
                                      First: Integer;
                                      OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, // XMLDATA,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // XMLDATA,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      const RowTag: AnsiString;
                                      Skip: Integer;
                                      First: Integer;
                                      XMLDATA: TalXMLNode;
                                      const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings);
end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      const RowTag: AnsiString;
                                      XMLDATA: TalXMLNode;
                                      const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings);
end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      XMLDATA: TalXMLNode;
                                      const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings);
end;

{***********************************************************}
procedure TalSqlite3Client.UpdateData(const SQL: AnsiString);
Var LStmt: SQLite3_Stmt;
    LStopWatch: TStopWatch;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  LStopWatch := TstopWatch.Create;

  //start the TstopWatch
  LStopWatch.Reset;
  LStopWatch.Start;

  //prepare the query
  CheckAPIError(FLibrary.sqlite3_prepare_v2(FSqlite3, PAnsiChar(SQL), length(SQL), LStmt, nil) <> SQLITE_OK);
  Try

    //retrieve the next row
    CheckAPIError(not (FLibrary.sqlite3_step(LStmt) in [SQLITE_DONE, SQLITE_ROW]));

  Finally
    //free the memory used by the API
    CheckAPIError(FLibrary.sqlite3_finalize(LStmt) <> SQLITE_OK);
  End;

  //do the OnUpdateDataDone
  LStopWatch.Stop;
  OnUpdateDataDone(SQL,
                   LStopWatch.Elapsed.TotalMilliseconds);

end;

{*********************************************************************}
procedure TalSqlite3Client.UpdateData(const SQLs: array of AnsiString);
var I: integer;
begin
  for I := Low(SQLs) to High(SQLs) do
    UpdateData(SQLs[I]);
end;

{******************************************************}
procedure TalSqlite3Client.UpdateData(SQLs: TALStrings);
var I: integer;
begin
  for I := 0 to sqls.Count - 1 do
    UpdateData(SQLs[I]);
end;

{************************************************************************************************}
procedure TalSqlite3ConnectionPoolClient.CheckAPIError(ConnectionHandle: SQLite3; Error: Boolean);
begin
  if Error then begin
    if assigned(ConnectionHandle) then raise EALSqlite3Error.Create(AnsiString(fLibrary.sqlite3_errmsg(ConnectionHandle)), fLibrary.sqlite3_errcode(ConnectionHandle)) // !! take care that sqlite3_errmsg(Fsqlite3) return an UTF8 !!
    else raise EALSqlite3Error.Create('Sqlite3 error', -1);
  end
end;

{******************************************************************}
function TalSqlite3ConnectionPoolClient.GetDataBaseName: AnsiString;
begin
  result := FdatabaseName;
end;

{**********************************************************************************}
procedure TalSqlite3ConnectionPoolClient.initObject(const aDataBaseName: AnsiString;
                                                    const aOpenConnectionFlags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
                                                    const aOpenConnectionPragmaStatements: AnsiString = '');
begin
  FDataBaseName:= aDataBaseName;
  FOpenConnectionFlags := aOpenConnectionFlags;
  FOpenConnectionPragmaStatements := TALStringList.Create;
  FOpenConnectionPragmaStatements.LineBreak := ';';
  FOpenConnectionPragmaStatements.Text := aOpenConnectionPragmaStatements;
  setlength(FConnectionPool,0);
  FConnectionPoolCount := 0;
  FConnectionPoolCapacity := 0;
  FConnectionPoolCS:= TCriticalSection.create;
  FDatabaseRWCS := TMultiReadExclusiveWriteSynchronizer.Create;
  FDatabaseWriteLocked := False;
  FWorkingConnectionCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionGarbage := GettickCount64;
  FConnectionMaxIdleTime := 1200000; // 1000 * 60 * 20 = 20 min
  FNullString := '';
end;


{********************************************************************************}
constructor TalSqlite3ConnectionPoolClient.Create(const aDataBaseName: AnsiString;
                                                  const aOpenConnectionFlags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
                                                  const aOpenConnectionPragmaStatements: AnsiString = '';
                                                  const alib: AnsiString = 'sqlite3.dll';
                                                  const initializeLib: Boolean = True);
begin
  fLibrary := TALSqlite3Library.Create;
  try
    fLibrary.Load(alib, False);
    config(SQLITE_CONFIG_MULTITHREAD);
    if initializeLib then initialize;
    FownLibrary := True;
    initObject(aDataBaseName,
               aOpenConnectionFlags,
               aOpenConnectionPragmaStatements);
  Except
    fLibrary.free;
    raise;
  end;
end;

{********************************************************************************}
constructor TalSqlite3ConnectionPoolClient.Create(const aDataBaseName: AnsiString;
                                                  alib: TALSqlite3Library;
                                                  const aOpenConnectionFlags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
                                                  const aOpenConnectionPragmaStatements: AnsiString = '');
begin
  fLibrary := alib;
  FownLibrary := False;
  initObject(aDataBaseName,
             aOpenConnectionFlags,
             aOpenConnectionPragmaStatements);
end;

{************************************************}
destructor TalSqlite3ConnectionPoolClient.Destroy;
begin

  //Release all connections
  ReleaseAllConnections;

  //free object
  FOpenConnectionPragmaStatements.free;
  FConnectionPoolCS.free;
  FDatabaseRWCS.Free;
  if FownLibrary then fLibrary.Free;

  //inherite
  inherited;

end;

{***************************************************************************}
function TalSqlite3ConnectionPoolClient.loadCachedData(const Key: AnsiString;
                                                       var DataStr: AnsiString): Boolean;
begin
  result := false; //virtual need to be overriden
end;

{*****************************************************************************}
Procedure TalSqlite3ConnectionPoolClient.SaveDataToCache(const Key: ansiString;
                                                         const CacheThreshold: integer;
                                                         const DataStr: ansiString);
begin
  //virtual need to be overriden
end;

{***************************************************************}
procedure TalSqlite3ConnectionPoolClient.config(Option: Integer);
begin
  CheckAPIError(nil, FLibrary.sqlite3_config(Option) <> SQLITE_OK);
end;

{**************************************************}
procedure TalSqlite3ConnectionPoolClient.initialize;
begin
  CheckAPIError(nil, FLibrary.sqlite3_initialize <> SQLITE_OK);
end;

{************************************************}
procedure TalSqlite3ConnectionPoolClient.shutdown;
begin
  CheckAPIError(nil, FLibrary.sqlite3_shutdown <> SQLITE_OK);
end;

{****************************************************************************}
procedure TalSqlite3ConnectionPoolClient.enable_shared_cache(enable: boolean);
begin
  if enable then CheckAPIError(nil, FLibrary.sqlite3_enable_shared_cache(1) <> SQLITE_OK)
  else CheckAPIError(nil, FLibrary.sqlite3_enable_shared_cache(0) <> SQLITE_OK);
end;

{**************************************************************************************************}
function TalSqlite3ConnectionPoolClient.AcquireConnection(const readonly: boolean = False): SQLite3;
Var LTickCount: UInt64;
    LDoPragma: Boolean;
Begin

  //init aDoPragma
  LDoPragma := False;

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    LTickCount := GetTickCount64;
    if LTickCount - fLastConnectionGarbage > (60000 {every minutes})  then begin

      while FConnectionPoolCount > 0 do begin
        if LTickCount - FConnectionPool[0].Lastaccessdate > FConnectionMaxIdleTime then begin

          Try
            FLibrary.sqlite3_close_v2(FConnectionPool[0].ConnectionHandle);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
            //that the bulsheet of SQLite3 to answer SQLITE_BUSY instead of free
            //everything
          End;

          Dec(FConnectionPoolCount);
          if  FConnectionPoolCount > 0 then
          begin
            System.Move(FConnectionPool[1], FConnectionPool[0],
              (FConnectionPoolCount) * SizeOf(TalSqlite3ConnectionPoolContainer));
          end;

        end
        else break;
      end;
      FLastConnectionGarbage := LTickCount;
    end;

    //acquire the new connection from the pool
    If FConnectionPoolCount > 0 then begin
      Result := FConnectionPool[FConnectionPoolCount - 1].ConnectionHandle;
      Dec(FConnectionPoolCount);
    end

    //create a new connection
    else begin
      Result := nil;
      Try
        CheckAPIError(result, fLibrary.sqlite3_open_v2(PAnsiChar(fDatabaseName), result, FOpenConnectionFlags, nil) <> SQLITE_OK);
        LDoPragma := True;
      Except
        //A database connection handle is usually returned in *ppDb, even if an error occurs.
        //Whether or not an error occurs when it is opened, resources associated with the
        //database connection handle should be released by passing it to sqlite3_close()
        //when it is no longer required
        if assigned(result) then fLibrary.sqlite3_close_v2(result);
        Raise;
      End;
    end;

    //increase the connection count
    inc(FWorkingConnectionCount);

  //get out of the synchronization
  finally
    FConnectionPoolCS.Release;
  end;

  //only one writer at a time OR multi reader at a time
  if ReadOnly then FDatabaseRWCS.BeginRead
  else begin
    FDatabaseRWCS.BeginWrite;
    FDatabaseWriteLocked := True;
  end;

  //execute the pragma here because before we can have an exception database is locked
  try
    if LDoPragma then UpdateData(FOpenConnectionPragmaStatements,result);
  Except

    //synchronize the code
    FConnectionPoolCS.Acquire;
    dec(FWorkingConnectionCount);
    FConnectionPoolCS.Release;

    //release the lock
    if FDatabaseWriteLocked then begin
      FDatabaseWriteLocked := False;
      FDatabaseRWCS.EndWrite;
    end
    else FDatabaseRWCS.EndRead;

    //free the result
    if assigned(result) then fLibrary.sqlite3_close_v2(result);

    //raise the exception
    Raise;

  End;

End;

{***************************************************************************************}
{Applications must finalize all prepared statements and close all BLOB handles associated
 with the sqlite3 object prior to attempting to close the object. If sqlite3_close() is
 called on a database connection that still has outstanding prepared statements or
 BLOB handles, then it returns SQLITE_BUSY.
 If sqlite3_close() is invoked while a transaction is open, the transaction is
 automatically rolled back.}
procedure TalSqlite3ConnectionPoolClient.ReleaseConnection(var ConnectionHandle: SQLite3;
                                                           const CloseConnection: Boolean = False);
begin

  //security check
  if not assigned(ConnectionHandle) then raise exception.Create('Connection handle can not be null');

  //release the connection
  FConnectionPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      if FConnectionPoolCount = FConnectionPoolCapacity then begin
        if FConnectionPoolCapacity > 64 then FConnectionPoolCapacity := FConnectionPoolCapacity + (FConnectionPoolCapacity div 4) else
          if FConnectionPoolCapacity > 8 then FConnectionPoolCapacity := FConnectionPoolCapacity + 16 else
            FConnectionPoolCapacity := FConnectionPoolCapacity + 4;
        SetLength(FConnectionPool, FConnectionPoolCapacity);
      end;
      FConnectionPool[FConnectionPoolCount].ConnectionHandle := ConnectionHandle;
      FConnectionPool[FConnectionPoolCount].LastAccessDate := GetTickCount64;
      Inc(FConnectionPoolCount);
    end

    //close the connection
    else begin
      try
        FLibrary.sqlite3_close_v2(ConnectionHandle);
      Except
        //Disconnect must be a "safe" procedure because it's mostly called in
        //finalization part of the code that it is not protected
      end;
    end;

    //set the connectionhandle to nil
    ConnectionHandle := nil;

    //dec the WorkingConnectionCount
    Dec(FWorkingConnectionCount);

  finally
    FConnectionPoolCS.Release;
  end;

  //release the lock
  if FDatabaseWriteLocked then begin
    FDatabaseWriteLocked := False;
    FDatabaseRWCS.EndWrite;
  end
  else FDatabaseRWCS.EndRead;

end;

{***********************************************************************************************************}
procedure TalSqlite3ConnectionPoolClient.ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True);
begin

  {we do this to forbid any new thread to create a new transaction}
  FReleasingAllconnections := True;
  Try

    //wait that all transaction are finished
    if WaitWorkingConnections then
      while true do begin
        FConnectionPoolCS.Acquire;
        Try
          if FWorkingConnectionCount <= 0 then break;
        finally
          FConnectionPoolCS.Release;
        end;
        sleep(1);
      end;

    {free all database}
    FConnectionPoolCS.Acquire;
    Try
      while FConnectionPoolCount > 0 do begin
        Try
          FLibrary.sqlite3_close_v2(FConnectionPool[FConnectionPoolcount - 1].ConnectionHandle);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;
        Dec(FConnectionPoolCount);
      end;
      FLastConnectionGarbage := GetTickCount64;
    finally
      FConnectionPoolCS.Release;
    end;

  finally
    //Do not forbid anymore new thread to create a new transaction
    FReleasingAllconnections := False;
  End;

end;

{**************************************************************************************}
procedure TalSqlite3ConnectionPoolClient.TransactionStart(Var ConnectionHandle: SQLite3;
                                                          const ReadOnly: boolean = False);
begin

  //ConnectionHandle must be null
  if assigned(ConnectionHandle) then raise exception.Create('Connection handle must be null');

  //init the aConnectionHandle
  ConnectionHandle := AcquireConnection(ReadOnly);
  try

    //start the transaction
    UpdateData('BEGIN TRANSACTION', ConnectionHandle);

  except
    ReleaseConnection(ConnectionHandle, True);
    raise;
  end;

end;

{***************************************************************************************}
procedure TalSqlite3ConnectionPoolClient.TransactionCommit(var ConnectionHandle: SQLite3;
                                                           const CloseConnection: Boolean = False);
begin

  //security check
  if not assigned(ConnectionHandle) then raise exception.Create('Connection handle can not be null');

  //commit the transaction
  UpdateData('COMMIT TRANSACTION', ConnectionHandle);

  //release the connection
  ReleaseConnection(ConnectionHandle, CloseConnection);

end;

{*****************************************************************************************}
procedure TalSqlite3ConnectionPoolClient.TransactionRollback(var ConnectionHandle: SQLite3;
                                                             const CloseConnection: Boolean = False);
var LTmpCloseConnection: Boolean;
begin

  //security check
  if not assigned(ConnectionHandle) then raise exception.Create('Connection handle can not be null');

  //rollback the connection
  LTmpCloseConnection := CloseConnection;
  Try
    Try
      UpdateData('ROLLBACK TRANSACTION', ConnectionHandle);
    except
      //to not raise an exception, most of the time TransactionRollback
      //are call inside a try ... except
      //raising the exception here will hide the first exception message
      //it's not a problem to hide the error here because closing the
      //connection will normally rollback the data
      LTmpCloseConnection := True;
    End;
  Finally

    //release the connection
    ReleaseConnection(ConnectionHandle, LTmpCloseConnection);

  End;

end;

{******************************************************************************}
procedure TalSqlite3ConnectionPoolClient.OnSelectDataDone(const SQL: AnsiString;
                                                          const RowTag: AnsiString;
                                                          const ViewTag: AnsiString;
                                                          Skip: integer;
                                                          First: Integer;
                                                          CacheThreshold: Integer;
                                                          TimeTaken: Double);
begin
  // virtual
end;

{******************************************************************************}
procedure TalSqlite3ConnectionPoolClient.OnUpdateDataDone(const SQL: AnsiString;
                                                          TimeTaken: Double);
begin
  // virtual
end;

{************************************************************************}
Procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    const RowTag: AnsiString;
                                                    const ViewTag: AnsiString;
                                                    Skip: integer;  // used only if value is > 0
                                                    First: Integer; // used only if value is > 0
                                                    CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                                              // cache or not. Values <= 0 deactivate the cache
                                                    JSONDATA: TALJSONNode;
                                                    OnNewRowFunct: TalSqlite3ClientSelectJSONDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionHandle: SQLite3 = nil);

Var LStmt: SQLite3_Stmt;
    LStepResult: integer;
    LColumnCount: Integer;
    LColumnIndex: integer;
    LColumnNames: Array of AnsiString;
    LNewRec: TalJsonNode;
    LValueRec: TalJsonNode;
    LViewRec: TalJsonNode;
    LRecIndex: integer;
    LRecAdded: integer;
    LTmpConnectionHandle: SQLite3;
    LOwnConnection: Boolean;
    LContinue: Boolean;
    LJsonDocument: TalJsonDocument;
    LUpdateRowTagByFieldValue: Boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;
    LTmpRowTag: ansiString;

begin

  //only OnNewRowFunct / JsonDATA can be used
  if assigned(OnNewRowFunct) then JsonDATA := nil;

  //clear the JsonDATA
  if assigned(JsonDATA) then LJsonDocument := Nil
  else begin
    LJsonDocument := TALJsonDocument.create;
    JsonDATA := LJsonDocument.Node;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LJsonDocument)) and
       ((Jsondata.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(AlFormat('BSON#%s#%s#%s#%s', [RowTag,
                                                                  alinttostr(Skip),
                                                                  alinttostr(First),
                                                                  SQL]));
      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := Jsondata.AddChild(ViewTag, ntObject)
        else LViewRec := Jsondata;

        //assign the tmp data to the JsonData
        LViewRec.LoadFromBsonString(LCacheStr, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //acquire a connection and start the transaction if necessary
    LTmpConnectionHandle := ConnectionHandle;
    LOwnConnection := (not assigned(ConnectionHandle));
    if LOwnConnection then TransactionStart(LTmpConnectionHandle, True);
    Try

      //start the TstopWatch
      LStopWatch.Reset;
      LStopWatch.Start;

      //prepare the query
      LStmt := nil;
      CheckAPIError(LTmpConnectionHandle, FLibrary.sqlite3_prepare_v2(LTmpConnectionHandle, PAnsiChar(SQL), length(SQL), LStmt, nil) <> SQLITE_OK);
      Try

        //Return the number of columns in the result set returned by the
        //prepared statement. This routine returns 0 if pStmt is an SQL statement
        //that does not return data (for example an UPDATE).
        LColumnCount := FLibrary.sqlite3_column_count(LStmt);

        //init the aColumnNames array
        setlength(LColumnNames,LColumnCount);
        For LColumnIndex := 0 to LColumnCount - 1 do
          LColumnNames[LColumnIndex] := FLibrary.sqlite3_column_name(LStmt, LColumnIndex);

        //init the aViewRec
        if (ViewTag <> '') and (not assigned(LJsonDocument))  then LViewRec := Jsondata.AddChild(ViewTag, ntObject)
        else LViewRec := Jsondata;

        //init aUpdateRowTagByFieldValue
        if AlPos('&>',RowTag) = 1 then begin
          LTmpRowTag := ALcopyStr(RowTag,3,maxint);
          LUpdateRowTagByFieldValue := LTmpRowTag <> '';
        end
        else begin
          LTmpRowTag := RowTag;
          LUpdateRowTagByFieldValue := False;
        end;

        //loop throught all row
        LRecIndex := 0;
        LRecAdded := 0;
        while True do begin

          //retrieve the next row
          LStepResult := FLibrary.sqlite3_step(LStmt);

          //break if no more row
          if LStepResult = SQLITE_DONE then break

          //download the row
          else if LStepResult = SQLITE_ROW then begin

            //process if > Skip
            inc(LRecIndex);
            If LRecIndex > Skip then begin

              //init NewRec
              if (LTmpRowTag <> '') and (not assigned(LJsonDocument))  then LNewRec := LViewRec.AddChild(LTmpRowTag, ntobject)
              Else LNewRec := LViewRec;

              //loop throught all column
              For LColumnIndex := 0 to LColumnCount - 1 do begin
                LValueRec := LNewRec.AddChild(ALlowercase(LColumnNames[LColumnIndex]));
                Case FLibrary.sqlite3_column_type(LStmt, LColumnIndex) of
                  SQLITE_INTEGER: LValueRec.int64 := FLibrary.sqlite3_column_int64(LStmt, LColumnIndex);
                  SQLITE_FLOAT: LValueRec.Float := FLibrary.sqlite3_column_double(LStmt, LColumnIndex);
                  SQLITE_TEXT: LValueRec.Text :=  AnsiString(FLibrary.sqlite3_column_text(LStmt, LColumnIndex)); // Strings returned by sqlite3_column_text() and sqlite3_column_text16(), even empty strings, are always zero-terminated.
                  SQLITE_NULL: LValueRec.Null := true;
                  //SQLITE_BLOB: todo
                  else raise Exception.Create('Unsupported column type');
                end;
                if LUpdateRowTagByFieldValue and (LValueRec.NodeName=LNewRec.NodeName) then LNewRec.NodeName := ALLowerCase(LValueRec.Text);
              end;

              //handle OnNewRowFunct
              if assigned(OnNewRowFunct) then begin
                LContinue := True;
                OnNewRowFunct(LNewRec, ViewTag, ExtData, LContinue);
                if Not LContinue then Break;
              end;

              //free the node if aJsonDocument
              if assigned(LJsonDocument) then LJsonDocument.Node.ChildNodes.Clear;

              //handle the First
              inc(LRecAdded);
              If (First > 0) and (LRecAdded >= First) then Break;

            end;

          end

          //misc error, raise an exception
          else CheckAPIError(LTmpConnectionHandle, True);

        end;

      Finally
        //free the memory used by the API
        CheckAPIError(LTmpConnectionHandle, FLibrary.sqlite3_finalize(LStmt) <> SQLITE_OK);
      End;

      //do the OnSelectDataDone
      LStopWatch.Stop;
      OnSelectDataDone(SQL,
                       RowTag,
                       ViewTag,
                       Skip,
                       First,
                       CacheThreshold,
                       LStopWatch.Elapsed.TotalMilliseconds);

      //save to the cache
      If LCacheKey <> '' then begin

        //save the data
        LViewRec.SaveToBsonString(LCacheStr);
        SaveDataToCache(LCacheKey,
                        CacheThreshold,
                        LCacheStr);

      end;

      //commit the transaction and release the connection if owned
      if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

    except
      On E: Exception do begin

        //rollback the transaction and release the connection if owned
        if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

        //raise the error
        raise;

      end;
    end;

  finally
    if assigned(LJsonDocument) then LJsonDocument.free;
  end;

end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    Skip: Integer;
                                                    First: Integer;
                                                    OnNewRowFunct: TalSqlite3ClientSelectJsonDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, // JsonDATA,
             OnNewRowFunct,
             ExtData,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    OnNewRowFunct: TalSqlite3ClientSelectJsonDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // JsonDATA,
             OnNewRowFunct,
             ExtData,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    const RowTag: AnsiString;
                                                    Skip: Integer;
                                                    First: Integer;
                                                    JsonDATA: TalJsonNode;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             JsonDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    const RowTag: AnsiString;
                                                    JsonDATA: TalJsonNode;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JsonDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    JsonDATA: TalJsonNode;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JsonDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    const RowTag: AnsiString;
                                                    const ViewTag: AnsiString;
                                                    Skip: integer;  // used only if value is > 0
                                                    First: Integer; // used only if value is > 0
                                                    CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                                              // cache or not. Values <= 0 deactivate the cache
                                                    XMLDATA: TalXMLNode;
                                                    OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const FormatSettings: TALFormatSettings;
                                                    const ConnectionHandle: SQLite3 = nil);

Var LStmt: SQLite3_Stmt;
    LStepResult: integer;
    LColumnCount: Integer;
    LColumnIndex: integer;
    LColumnNames: Array of AnsiString;
    LNewRec: TalXmlNode;
    LValueRec: TalXmlNode;
    LViewRec: TalXmlNode;
    LRecIndex: integer;
    LRecAdded: integer;
    LTmpConnectionHandle: SQLite3;
    LOwnConnection: Boolean;
    LContinue: Boolean;
    LXmlDocument: TalXmlDocument;
    LUpdateRowTagByFieldValue: Boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;
    LTmpRowTag: ansiString;

begin

  //only OnNewRowFunct / XMLDATA can be used
  if assigned(OnNewRowFunct) then XMLDATA := nil;

  //clear the XMLDATA
  if assigned(XMLDATA) then LXmlDocument := Nil
  else begin
    LXmlDocument := TALXmlDocument.create('root');
    XMLDATA := LXmlDocument.DocumentElement;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LXmlDocument)) and
       ((XMLdata.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(AlFormat('XML#%s#%s#%s#%s#%s', [RowTag,
                                                                    alinttostr(Skip),
                                                                    alinttostr(First),
                                                                    ALGetFormatSettingsID(FormatSettings),
                                                                    SQL]));
      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := XMLdata.AddChild(ViewTag)
        else LViewRec := XMLdata;

        //assign the tmp data to the XMLData
        LViewRec.LoadFromXML(LCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //acquire a connection and start the transaction if necessary
    LTmpConnectionHandle := ConnectionHandle;
    LOwnConnection := (not assigned(ConnectionHandle));
    if LOwnConnection then TransactionStart(LTmpConnectionHandle, True);
    Try

      //start the TstopWatch
      LStopWatch.Reset;
      LStopWatch.Start;

      //prepare the query
      LStmt := nil;
      CheckAPIError(LTmpConnectionHandle, FLibrary.sqlite3_prepare_v2(LTmpConnectionHandle, PAnsiChar(SQL), length(SQL), LStmt, nil) <> SQLITE_OK);
      Try

        //Return the number of columns in the result set returned by the
        //prepared statement. This routine returns 0 if pStmt is an SQL statement
        //that does not return data (for example an UPDATE).
        LColumnCount := FLibrary.sqlite3_column_count(LStmt);

        //init the aColumnNames array
        setlength(LColumnNames,LColumnCount);
        For LColumnIndex := 0 to LColumnCount - 1 do
          LColumnNames[LColumnIndex] := FLibrary.sqlite3_column_name(LStmt, LColumnIndex);

        //init the aViewRec
        if (ViewTag <> '') and (not assigned(LXmlDocument))  then LViewRec := XMLdata.AddChild(ViewTag)
        else LViewRec := XMLdata;

        //init aUpdateRowTagByFieldValue
        if AlPos('&>',RowTag) = 1 then begin
          LTmpRowTag := ALcopyStr(RowTag,3,maxint);
          LUpdateRowTagByFieldValue := LTmpRowTag <> '';
        end
        else begin
          LTmpRowTag := RowTag;
          LUpdateRowTagByFieldValue := False;
        end;

        //loop throught all row
        LRecIndex := 0;
        LRecAdded := 0;
        while True do begin

          //retrieve the next row
          LStepResult := FLibrary.sqlite3_step(LStmt);

          //break if no more row
          if LStepResult = SQLITE_DONE then break

          //download the row
          else if LStepResult = SQLITE_ROW then begin

            //process if > Skip
            inc(LRecIndex);
            If LRecIndex > Skip then begin

              //init NewRec
              if (LTmpRowTag <> '') and (not assigned(LXmlDocument))  then LNewRec := LViewRec.AddChild(LTmpRowTag)
              Else LNewRec := LViewRec;

              //loop throught all column
              For LColumnIndex := 0 to LColumnCount - 1 do begin
                LValueRec := LNewRec.AddChild(ALlowercase(LColumnNames[LColumnIndex]));
                Case FLibrary.sqlite3_column_type(LStmt, LColumnIndex) of
                  SQLITE_FLOAT: LValueRec.Text := ALFloattostr(FLibrary.sqlite3_column_double(LStmt, LColumnIndex), FormatSettings);
                  SQLITE_INTEGER,
                  SQLITE3_TEXT: LValueRec.Text :=  AnsiString(FLibrary.sqlite3_column_text(LStmt, LColumnIndex)); // Strings returned by sqlite3_column_text() and sqlite3_column_text16(), even empty strings, are always zero-terminated.
                  SQLITE_NULL: LValueRec.Text := fNullString;
                  //SQLITE_BLOB: todo
                  else raise Exception.Create('Unsupported column type');
                end;
                if LUpdateRowTagByFieldValue and (LValueRec.NodeName=LNewRec.NodeName) then LNewRec.NodeName := ALLowerCase(LValueRec.Text);
              end;

              //handle OnNewRowFunct
              if assigned(OnNewRowFunct) then begin
                LContinue := True;
                OnNewRowFunct(LNewRec, ViewTag, ExtData, LContinue);
                if Not LContinue then Break;
              end;

              //free the node if aXmlDocument
              if assigned(LXmlDocument) then LXmlDocument.DocumentElement.ChildNodes.Clear;

              //handle the First
              inc(LRecAdded);
              If (First > 0) and (LRecAdded >= First) then Break;

            end;

          end

          //misc error, raise an exception
          else CheckAPIError(LTmpConnectionHandle, True);

        end;

      Finally
        //free the memory used by the API
        CheckAPIError(LTmpConnectionHandle, FLibrary.sqlite3_finalize(LStmt) <> SQLITE_OK);
      End;

      //do the OnSelectDataDone
      LStopWatch.Stop;
      OnSelectDataDone(SQL,
                       RowTag,
                       ViewTag,
                       Skip,
                       First,
                       CacheThreshold,
                       LStopWatch.Elapsed.TotalMilliseconds);

      //save to the cache
      If LCacheKey <> '' then begin

        //save the data
        LViewRec.SaveToXML(LCacheStr, true{SaveOnlyChildNodes});
        SaveDataToCache(LCacheKey,
                        CacheThreshold,
                        LCacheStr);

      end;

      //commit the transaction and release the connection if owned
      if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

    except
      On E: Exception do begin

        //rollback the transaction and release the connection if owned
        if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

        //raise the error
        raise;

      end;
    end;

  finally
    if assigned(LXmlDocument) then LXmlDocument.free;
  end;

end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    Skip: Integer;
                                                    First: Integer;
                                                    OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const FormatSettings: TALFormatSettings;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, // XMLDATA,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    OnNewRowFunct: TalSqlite3ClientSelectXMLDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const FormatSettings: TALFormatSettings;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // XMLDATA,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    const RowTag: AnsiString;
                                                    Skip: Integer;
                                                    First: Integer;
                                                    XMLDATA: TalXMLNode;
                                                    const FormatSettings: TALFormatSettings;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    const RowTag: AnsiString;
                                                    XMLDATA: TalXMLNode;
                                                    const FormatSettings: TALFormatSettings;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    XMLDATA: TalXMLNode;
                                                    const FormatSettings: TALFormatSettings;
                                                    const ConnectionHandle: SQLite3 = nil);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.UpdateData(const SQL: AnsiString;
                                                    const ConnectionHandle: SQLite3 = nil);
Var LStmt: SQLite3_Stmt;
    LTmpConnectionHandle: SQLite3;
    LOwnConnection: Boolean;
    LStopWatch: TStopWatch;
begin

  //acquire a connection and start the transaction if necessary
  LTmpConnectionHandle := ConnectionHandle;
  LOwnConnection := (not assigned(ConnectionHandle));
  if LOwnConnection then TransactionStart(LTmpConnectionHandle, False);
  Try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //start the TstopWatch
    LStopWatch.Reset;
    LStopWatch.Start;

    //prepare the query
    CheckAPIError(LTmpConnectionHandle, FLibrary.sqlite3_prepare_v2(LTmpConnectionHandle, PAnsiChar(SQL), length(SQL), LStmt, nil) <> SQLITE_OK);
    Try

      //retrieve the next row
      CheckAPIError(LTmpConnectionHandle, not (FLibrary.sqlite3_step(LStmt) in [SQLITE_DONE, SQLITE_ROW]));

    Finally
      //free the memory used by the API
      CheckAPIError(LTmpConnectionHandle, FLibrary.sqlite3_finalize(LStmt) <> SQLITE_OK);
    End;

    //do the OnUpdateDataDone
    LStopWatch.Stop;
    OnUpdateDataDone(SQL,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //commit the transaction and release the connection if owned
    if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

  except
    On E: Exception do begin

      //rollback the transaction and release the connection if owned
      if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{**********************************************************************************}
procedure TalSqlite3ConnectionPoolClient.UpdateData(const SQLs: array of AnsiString;
                                                    const ConnectionHandle: SQLite3 = nil);
Var LTmpConnectionHandle: SQLite3;
    LOwnConnection: Boolean;
    I: integer;
begin

  //acquire a connection and start the transaction if necessary
  LTmpConnectionHandle := ConnectionHandle;
  LOwnConnection := (not assigned(ConnectionHandle));
  if LOwnConnection then TransactionStart(LTmpConnectionHandle, False);
  Try

    //update the data
    for I := Low(SQLs) to High(SQLs) do
      UpdateData(SQLs[I],
                 LTmpConnectionHandle);

    //commit the transaction and release the connection if owned
    if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

  except
    On E: Exception do begin

      //rollback the transaction and release the connection if owned
      if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{*******************************************************************}
procedure TalSqlite3ConnectionPoolClient.UpdateData(SQLs: TALStrings;
                                                    const ConnectionHandle: SQLite3 = nil);
Var LTmpConnectionHandle: SQLite3;
    LOwnConnection: Boolean;
    I: integer;
begin

  //acquire a connection and start the transaction if necessary
  LTmpConnectionHandle := ConnectionHandle;
  LOwnConnection := (not assigned(ConnectionHandle));
  if LOwnConnection then TransactionStart(LTmpConnectionHandle, False);
  Try

    //update the data
    for I := 0 to SQLs.Count - 1 do
      UpdateData(SQLs[I],
                 LTmpConnectionHandle);

    //commit the transaction and release the connection if owned
    if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

  except
    On E: Exception do begin

      //rollback the transaction and release the connection if owned
      if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{***************************************************************}
function TalSqlite3ConnectionPoolClient.ConnectionCount: Integer;
begin
  FConnectionPoolCS.Acquire;
  Try
    Result := FConnectionPoolCount + FWorkingConnectionCount;
  finally
    FConnectionPoolCS.Release;
  end;
end;

{**********************************************************************}
function TalSqlite3ConnectionPoolClient.WorkingConnectionCount: Integer;
begin
  FConnectionPoolCS.Acquire;
  Try
    Result := FWorkingConnectionCount;
  finally
    FConnectionPoolCS.Release;
  end;
end;

end.
