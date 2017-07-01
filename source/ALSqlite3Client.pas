{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)

product:      ALSqlite3Client
Version:      4.00

Description:  An object to query Sqlite3 database and get
              the result In Xml stream

              SQLite is a software library that implements a self-contained,
              serverless, zero-configuration, transactional SQL database
              engine. The source code for SQLite is in the public domain and
              is thus free for use for any purpose, commercial or private.
              SQLite is the most widely deployed SQL database engine
              in the world.

Know bug :

History :     26/06/2012: Add xe2 support

Link :        http://www.sqlite.org/

**************************************************************}
unit AlSqlite3Client;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.SysUtils,
     System.Contnrs,
     System.SyncObjs,
     {$ELSE}
     SysUtils,
     Contnrs,
     SyncObjs,
     {$IFEND}
     AlXmlDoc,
     AlSqlite3Wrapper,
     ALString,
     ALStringList;

Type

  {--------------------------------------}
  {$IF CompilerVersion >= 23} {Delphi XE2}
  TalSqlite3ClientSelectDataOnNewRowFunct = reference to Procedure(XMLRowData: TalXmlNode;
                                                                   const ViewTag: AnsiString;
                                                                   ExtData: Pointer;
                                                                   Var Continue: Boolean);
  {$ELSE}
  TalSqlite3ClientSelectDataOnNewRowFunct = Procedure(XMLRowData: TalXmlNode;
                                                      const ViewTag: AnsiString;
                                                      ExtData: Pointer;
                                                      Var Continue: Boolean);
  {$IFEND}

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
    fSqlite3: PSQLite3;
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
    Function  GetFieldValue(aSqlite3stmt: PSQLite3Stmt;
                            aIndex: Integer;
                            const aFormatSettings: TALFormatSettings): AnsiString;
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
                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
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
    ConnectionHandle: PSQLite3;
    LastAccessDate: int64;
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
    FLastConnectionGarbage: Int64;
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
    procedure CheckAPIError(ConnectionHandle: PSQLite3; Error: Boolean);
    function  GetDataBaseName: AnsiString; virtual;
    Function  GetFieldValue(aSqlite3stmt: PSQLite3Stmt;
                            aIndex: Integer;
                            const aFormatSettings: TALFormatSettings): AnsiString; virtual;
    procedure initObject(const aDataBaseName: AnsiString;
                         const aOpenConnectionFlags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
                         const aOpenConnectionPragmaStatements: AnsiString = ''); virtual;
    Function  AcquireConnection(const readonly: boolean = False): PSQLite3; virtual;
    Procedure ReleaseConnection(var ConnectionHandle: PSQLite3;
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
    Procedure TransactionStart(Var ConnectionHandle: PSQLite3; const ReadOnly: boolean = False); virtual;
    Procedure TransactionCommit(var ConnectionHandle: PSQLite3;
                                const CloseConnection: Boolean = False); virtual;
    Procedure TransactionRollback(var ConnectionHandle: PSQLite3;
                                  const CloseConnection: Boolean = False); virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         const ViewTag: AnsiString;
                         Skip: integer;  // used only if value is > 0
                         First: Integer; // used only if value is > 0
                         CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                   // cache or not. Values <= 0 deactivate the cache

                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PSQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PSQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PSQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PSQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PSQLite3 = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PSQLite3 = nil); overload; virtual;
    procedure UpdateData(SQLs: TALStrings;
                         const ConnectionHandle: PSQLite3 = nil); overload; virtual;
    procedure UpdateData(const SQL: AnsiString;
                         const ConnectionHandle: PSQLite3 = nil); overload; virtual;
    procedure UpdateData(const SQLs: array of AnsiString;
                         const ConnectionHandle: PSQLite3 = nil); overload; virtual;
    Function  ConnectionCount: Integer;
    Function  WorkingConnectionCount: Integer;
    property  DataBaseName: AnsiString read GetDataBaseName;
    property  ConnectionMaxIdleTime: integer read FConnectionMaxIdleTime write fConnectionMaxIdleTime;
    Property  NullString: AnsiString Read fNullString Write fNullString;
    property  Lib: TALSqlite3Library read FLibrary;
  end;

implementation

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.classes,
     System.Diagnostics,
     {$ELSE}
     classes,
     Diagnostics,
     {$IFEND}
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

{*****************************************************************}
function TalSqlite3Client.GetFieldValue(aSqlite3stmt: PSQLite3Stmt;
                                        aIndex: Integer;
                                        const aFormatSettings: TALFormatSettings): AnsiString;
begin
  Case FLibrary.sqlite3_column_type(aSqlite3stmt, aIndex) of
    SQLITE_FLOAT: Result := ALFloattostr(FLibrary.sqlite3_column_double(aSqlite3stmt, aIndex), aFormatSettings);
    SQLITE_INTEGER,
    SQLITE3_TEXT: result :=  AnsiString(FLibrary.sqlite3_column_text(aSqlite3stmt, aIndex)); // Strings returned by sqlite3_column_text() and sqlite3_column_text16(), even
                                                                                             // empty strings, are always zero-terminated.
    SQLITE_NULL: result := fNullString;
    else raise Exception.Create('Unsupported column type');
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
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      const RowTag: AnsiString;
                                      const ViewTag: AnsiString;
                                      Skip: integer;  // used only if value is > 0
                                      First: Integer; // used only if value is > 0
                                      CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                                // cache or not. Values <= 0 deactivate the cache

                                      XMLDATA: TalXMLNode;
                                      OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      const FormatSettings: TALFormatSettings);
Var astmt: PSQLite3Stmt;
    aStepResult: integer;
    aColumnCount: Integer;
    aColumnIndex: integer;
    aColumnNames: Array of AnsiString;
    aNewRec: TalXmlNode;
    aValueRec: TalXmlNode;
    aViewRec: TalXmlNode;
    aRecIndex: integer;
    aRecAdded: integer;
    aContinue: Boolean;
    aXmlDocument: TalXmlDocument;
    aUpdateRowTagByFieldValue: Boolean;
    aStopWatch: TStopWatch;
    aCacheKey: ansiString;
    aCacheStr: ansiString;
    aTmpRowTag: ansiString;

begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //only OnNewRowFunct / XMLDATA can be used
  if assigned(OnNewRowFunct) then XMLDATA := nil;

  //clear the XMLDATA
  if assigned(XMLDATA) then aXmlDocument := Nil
  else begin
    aXmlDocument := TALXmlDocument.create('root');
    XMLDATA := aXmlDocument.DocumentElement;
  end;

  try

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    aCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(aXmlDocument)) and
       ((XMLdata.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      aCacheKey := ALStringHashSHA1(RowTag + '#' +
                                    alinttostr(Skip) + '#' +
                                    alinttostr(First) + '#' +
                                    ALGetFormatSettingsID(FormatSettings) + '#' +
                                    SQL);
      if loadcachedData(aCacheKey, aCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then aViewRec := XMLdata.AddChild(ViewTag)
        else aViewRec := XMLdata;

        //assign the tmp data to the XMLData
        aViewRec.LoadFromXML(aCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //start the TstopWatch
    aStopWatch.Reset;
    aStopWatch.Start;

    //prepare the query
    astmt := nil;
    CheckAPIError(FLibrary.sqlite3_prepare_v2(FSqlite3, PAnsiChar(SQL), length(SQL), astmt, nil) <> SQLITE_OK);
    Try

      //Return the number of columns in the result set returned by the
      //prepared statement. This routine returns 0 if pStmt is an SQL statement
      //that does not return data (for example an UPDATE).
      aColumnCount := FLibrary.sqlite3_column_count(astmt);

      //init the aColumnNames array
      setlength(aColumnNames,aColumnCount);
      For aColumnIndex := 0 to aColumnCount - 1 do
        aColumnNames[aColumnIndex] := FLibrary.sqlite3_column_name(astmt, aColumnIndex);

      //init the aViewRec
      if (ViewTag <> '') and (not assigned(aXmlDocument)) then aViewRec := XMLdata.AddChild(ViewTag)
      else aViewRec := XMLdata;

      //init aUpdateRowTagByFieldValue
      if AlPos('&>',RowTag) = 1 then begin
        aTmpRowTag := ALcopyStr(RowTag,3,maxint);
        aUpdateRowTagByFieldValue := aTmpRowTag <> '';
      end
      else begin
        aTmpRowTag := RowTag;
        aUpdateRowTagByFieldValue := False;
      end;

      //loop throught all row
      aRecIndex := 0;
      aRecAdded := 0;
      while True do begin

        //retrieve the next row
        aStepResult := FLibrary.sqlite3_step(astmt);

        //break if no more row
        if aStepResult = SQLITE_DONE then break

        //download the row
        else if aStepResult = SQLITE_ROW then begin

          //process if > Skip
          inc(aRecIndex);
          If aRecIndex > Skip then begin

            //init NewRec
            if (aTmpRowTag <> '') and (not assigned(aXmlDocument)) then aNewRec := aViewRec.AddChild(aTmpRowTag)
            Else aNewRec := aViewRec;

            //loop throught all column
            For aColumnIndex := 0 to aColumnCount - 1 do begin
              aValueRec := aNewRec.AddChild(ALlowercase(aColumnNames[aColumnIndex]));
              aValueRec.Text := GetFieldValue(astmt,
                                              aColumnIndex,
                                              FormatSettings);
              if aUpdateRowTagByFieldValue and (aValueRec.NodeName=aNewRec.NodeName) then aNewRec.NodeName := ALLowerCase(aValueRec.Text);
            end;

            //handle OnNewRowFunct
            if assigned(OnNewRowFunct) then begin
              aContinue := True;
              OnNewRowFunct(aNewRec, ViewTag, ExtData, aContinue);
              if Not aContinue then Break;
            end;

            //free the node if aXmlDocument
            if assigned(aXmlDocument) then aXmlDocument.DocumentElement.ChildNodes.Clear;

            //handle the First
            inc(aRecAdded);
            If (First > 0) and (aRecAdded >= First) then Break;

          end;

        end

        //misc error, raise an exception
        else CheckAPIError(True);

      end;

    Finally
      //free the memory used by the API
      CheckAPIError(FLibrary.sqlite3_finalize(astmt) <> SQLITE_OK);
    End;

    //do the OnSelectDataDone
    aStopWatch.Stop;
    OnSelectDataDone(SQL,
                     RowTag,
                     ViewTag,
                     Skip,
                     First,
                     CacheThreshold,
                     aStopWatch.Elapsed.TotalMilliseconds);

    //save to the cache
    If aCacheKey <> '' then begin

      //save the data
      aViewRec.SaveToXML(aCacheStr, true{SaveOnlyChildNodes});
      SaveDataToCache(aCacheKey,
                      CacheThreshold,
                      aCacheStr);

    end;

  Finally
    if assigned(aXmlDocument) then aXmlDocument.free;
  End;

end;

{**********************************************************}
procedure TalSqlite3Client.SelectData(const SQL: AnsiString;
                                      Skip: Integer;
                                      First: Integer;
                                      OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
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
                                      OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
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
Var astmt: PSQLite3Stmt;
    aStopWatch: TStopWatch;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  aStopWatch := TstopWatch.Create;

  //start the TstopWatch
  aStopWatch.Reset;
  aStopWatch.Start;

  //prepare the query
  CheckAPIError(FLibrary.sqlite3_prepare_v2(FSqlite3, PAnsiChar(SQL), length(SQL), astmt, nil) <> SQLITE_OK);
  Try

    //retrieve the next row
    CheckAPIError(not (FLibrary.sqlite3_step(astmt) in [SQLITE_DONE, SQLITE_ROW]));

  Finally
    //free the memory used by the API
    CheckAPIError(FLibrary.sqlite3_finalize(astmt) <> SQLITE_OK);
  End;

  //do the OnUpdateDataDone
  aStopWatch.Stop;
  OnUpdateDataDone(SQL,
                   aStopWatch.Elapsed.TotalMilliseconds);

end;

{*********************************************************************}
procedure TalSqlite3Client.UpdateData(const SQLs: array of AnsiString);
var i: integer;
begin
  for I := Low(SQLs) to High(SQLs) do
    UpdateData(SQLs[i]);
end;

{******************************************************}
procedure TalSqlite3Client.UpdateData(SQLs: TALStrings);
var i: integer;
begin
  for I := 0 to sqls.Count - 1 do
    UpdateData(SQLs[i]);
end;

{*************************************************************************************************}
procedure TalSqlite3ConnectionPoolClient.CheckAPIError(ConnectionHandle: PSQLite3; Error: Boolean);
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

{*******************************************************************************}
function TalSqlite3ConnectionPoolClient.GetFieldValue(aSqlite3stmt: PSQLite3Stmt;
                                                      aIndex: Integer;
                                                      const aFormatSettings: TALFormatSettings): AnsiString;
begin
  Case FLibrary.sqlite3_column_type(aSqlite3stmt, aIndex) of
    SQLITE_FLOAT: Result := ALFloattostr(FLibrary.sqlite3_column_double(aSqlite3stmt, aIndex), aFormatSettings);
    SQLITE_INTEGER,
    SQLITE3_TEXT: result :=  AnsiString(FLibrary.sqlite3_column_text(aSqlite3stmt, aIndex)); // Strings returned by sqlite3_column_text(), even empty strings, are always zero terminated
                                                                                             // Note: what's happen if #0 is inside the string ?
    SQLITE_NULL: result := fNullString;
    else raise Exception.Create('Unsupported column type');
  end;
end;

{**********************************************************************************}
procedure TalSqlite3ConnectionPoolClient.initObject(const aDataBaseName: AnsiString;
                                                    const aOpenConnectionFlags: integer = SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
                                                    const aOpenConnectionPragmaStatements: AnsiString = '');
begin
  FDataBaseName:= aDataBaseName;
  FOpenConnectionFlags := aOpenConnectionFlags;
  FOpenConnectionPragmaStatements := TALStringList.Create;
  FOpenConnectionPragmaStatements.Text := ALTrim(AlStringReplace(aOpenConnectionPragmaStatements,';',#13#10,[rfReplaceAll]));
  setlength(FConnectionPool,0);
  FConnectionPoolCount := 0;
  FConnectionPoolCapacity := 0;
  FConnectionPoolCS:= TCriticalSection.create;
  FDatabaseRWCS := TMultiReadExclusiveWriteSynchronizer.Create;
  FDatabaseWriteLocked := False;
  FWorkingConnectionCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionGarbage := ALGettickCount64;
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

{***************************************************************************************************}
function TalSqlite3ConnectionPoolClient.AcquireConnection(const readonly: boolean = False): PSQLite3;
Var aTickCount: int64;
    aDoPragma: Boolean;
Begin

  //init aDoPragma
  aDoPragma := False;

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    aTickCount := ALGetTickCount64;
    if aTickCount - fLastConnectionGarbage > (60000 {every minutes})  then begin

      while FConnectionPoolCount > 0 do begin
        if aTickCount - FConnectionPool[0].Lastaccessdate > FConnectionMaxIdleTime then begin

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
      FLastConnectionGarbage := aTickCount;
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
        aDoPragma := True;
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
    if aDoPragma then UpdateData(FOpenConnectionPragmaStatements,result);
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
procedure TalSqlite3ConnectionPoolClient.ReleaseConnection(var ConnectionHandle: PSQLite3;
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
      FConnectionPool[FConnectionPoolCount].LastAccessDate := ALGetTickCount64;
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
      FLastConnectionGarbage := ALGetTickCount64;
    finally
      FConnectionPoolCS.Release;
    end;

  finally
    //Do not forbid anymore new thread to create a new transaction
    FReleasingAllconnections := False;
  End;

end;

{***************************************************************************************}
procedure TalSqlite3ConnectionPoolClient.TransactionStart(Var ConnectionHandle: PSQLite3;
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

{****************************************************************************************}
procedure TalSqlite3ConnectionPoolClient.TransactionCommit(var ConnectionHandle: PSQLite3;
                                                           const CloseConnection: Boolean = False);
begin

  //security check
  if not assigned(ConnectionHandle) then raise exception.Create('Connection handle can not be null');

  //commit the transaction
  UpdateData('COMMIT TRANSACTION', ConnectionHandle);

  //release the connection
  ReleaseConnection(ConnectionHandle, CloseConnection);

end;

{******************************************************************************************}
procedure TalSqlite3ConnectionPoolClient.TransactionRollback(var ConnectionHandle: PSQLite3;
                                                             const CloseConnection: Boolean = False);
var aTmpCloseConnection: Boolean;
begin

  //security check
  if not assigned(ConnectionHandle) then raise exception.Create('Connection handle can not be null');

  //rollback the connection
  aTmpCloseConnection := CloseConnection;
  Try
    Try
      UpdateData('ROLLBACK TRANSACTION', ConnectionHandle);
    except
      //to not raise an exception, most of the time TransactionRollback
      //are call inside a try ... except
      //raising the exception here will hide the first exception message
      //it's not a problem to hide the error here because closing the
      //connection will normally rollback the data
      aTmpCloseConnection := True;
    End;
  Finally

    //release the connection
    ReleaseConnection(ConnectionHandle, aTmpCloseConnection);

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
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    const RowTag: AnsiString;
                                                    const ViewTag: AnsiString;
                                                    Skip: integer;  // used only if value is > 0
                                                    First: Integer; // used only if value is > 0
                                                    CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                                              // cache or not. Values <= 0 deactivate the cache
                                                    XMLDATA: TalXMLNode;
                                                    OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const FormatSettings: TALFormatSettings;
                                                    const ConnectionHandle: PSQLite3 = nil);

Var astmt: PSQLite3Stmt;
    aStepResult: integer;
    aColumnCount: Integer;
    aColumnIndex: integer;
    aColumnNames: Array of AnsiString;
    aNewRec: TalXmlNode;
    aValueRec: TalXmlNode;
    aViewRec: TalXmlNode;
    aRecIndex: integer;
    aRecAdded: integer;
    aTmpConnectionHandle: PSQLite3;
    aOwnConnection: Boolean;
    aContinue: Boolean;
    aXmlDocument: TalXmlDocument;
    aUpdateRowTagByFieldValue: Boolean;
    aStopWatch: TStopWatch;
    aCacheKey: ansiString;
    aCacheStr: ansiString;
    aTmpRowTag: ansiString;

begin

  //only OnNewRowFunct / XMLDATA can be used
  if assigned(OnNewRowFunct) then XMLDATA := nil;

  //clear the XMLDATA
  if assigned(XMLDATA) then aXmlDocument := Nil
  else begin
    aXmlDocument := TALXmlDocument.create('root');
    XMLDATA := aXmlDocument.DocumentElement;
  end;

  try

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    aCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(aXmlDocument)) and
       ((XMLdata.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      aCacheKey := ALStringHashSHA1(RowTag + '#' +
                                    alinttostr(Skip) + '#' +
                                    alinttostr(First) + '#' +
                                    ALGetFormatSettingsID(FormatSettings) + '#' +
                                    SQL);
      if loadcachedData(aCacheKey, aCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then aViewRec := XMLdata.AddChild(ViewTag)
        else aViewRec := XMLdata;

        //assign the tmp data to the XMLData
        aViewRec.LoadFromXML(aCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //acquire a connection and start the transaction if necessary
    aTmpConnectionHandle := ConnectionHandle;
    aOwnConnection := (not assigned(ConnectionHandle));
    if aOwnConnection then TransactionStart(aTmpConnectionHandle, True);
    Try

      //start the TstopWatch
      aStopWatch.Reset;
      aStopWatch.Start;

      //prepare the query
      astmt := nil;
      CheckAPIError(aTmpConnectionHandle, FLibrary.sqlite3_prepare_v2(aTmpConnectionHandle, PAnsiChar(SQL), length(SQL), astmt, nil) <> SQLITE_OK);
      Try

        //Return the number of columns in the result set returned by the
        //prepared statement. This routine returns 0 if pStmt is an SQL statement
        //that does not return data (for example an UPDATE).
        aColumnCount := FLibrary.sqlite3_column_count(astmt);

        //init the aColumnNames array
        setlength(aColumnNames,aColumnCount);
        For aColumnIndex := 0 to aColumnCount - 1 do
          aColumnNames[aColumnIndex] := FLibrary.sqlite3_column_name(astmt, aColumnIndex);

        //init the aViewRec
        if (ViewTag <> '') and (not assigned(aXmlDocument))  then aViewRec := XMLdata.AddChild(ViewTag)
        else aViewRec := XMLdata;

        //init aUpdateRowTagByFieldValue
        if AlPos('&>',RowTag) = 1 then begin
          aTmpRowTag := ALcopyStr(RowTag,3,maxint);
          aUpdateRowTagByFieldValue := aTmpRowTag <> '';
        end
        else begin
          aTmpRowTag := RowTag;
          aUpdateRowTagByFieldValue := False;
        end;

        //loop throught all row
        aRecIndex := 0;
        aRecAdded := 0;
        while True do begin

          //retrieve the next row
          aStepResult := FLibrary.sqlite3_step(astmt);

          //break if no more row
          if aStepResult = SQLITE_DONE then break

          //download the row
          else if aStepResult = SQLITE_ROW then begin

            //process if > Skip
            inc(aRecIndex);
            If aRecIndex > Skip then begin

              //init NewRec
              if (aTmpRowTag <> '') and (not assigned(aXmlDocument))  then aNewRec := aViewRec.AddChild(aTmpRowTag)
              Else aNewRec := aViewRec;

              //loop throught all column
              For aColumnIndex := 0 to aColumnCount - 1 do begin
                aValueRec := aNewRec.AddChild(ALlowercase(aColumnNames[aColumnIndex]));
                aValueRec.Text := GetFieldValue(astmt,
                                                aColumnIndex,
                                                FormatSettings);
                if aUpdateRowTagByFieldValue and (aValueRec.NodeName=aNewRec.NodeName) then aNewRec.NodeName := ALLowerCase(aValueRec.Text);
              end;

              //handle OnNewRowFunct
              if assigned(OnNewRowFunct) then begin
                aContinue := True;
                OnNewRowFunct(aNewRec, ViewTag, ExtData, aContinue);
                if Not aContinue then Break;
              end;

              //free the node if aXmlDocument
              if assigned(aXmlDocument) then aXmlDocument.DocumentElement.ChildNodes.Clear;

              //handle the First
              inc(aRecAdded);
              If (First > 0) and (aRecAdded >= First) then Break;

            end;

          end

          //misc error, raise an exception
          else CheckAPIError(aTmpConnectionHandle, True);

        end;

      Finally
        //free the memory used by the API
        CheckAPIError(aTmpConnectionHandle, FLibrary.sqlite3_finalize(astmt) <> SQLITE_OK);
      End;

      //do the OnSelectDataDone
      aStopWatch.Stop;
      OnSelectDataDone(SQL,
                       RowTag,
                       ViewTag,
                       Skip,
                       First,
                       CacheThreshold,
                       aStopWatch.Elapsed.TotalMilliseconds);

      //save to the cache
      If aCacheKey <> '' then begin

        //save the data
        aViewRec.SaveToXML(aCacheStr, true{SaveOnlyChildNodes});
        SaveDataToCache(aCacheKey,
                        CacheThreshold,
                        aCacheStr);

      end;

      //commit the transaction and release the connection if owned
      if aOwnConnection then TransactionCommit(aTmpConnectionHandle);

    except
      On E: Exception do begin

        //rollback the transaction and release the connection if owned
        if aOwnConnection then TransactionRollback(aTmpConnectionHandle, true);

        //raise the error
        raise;

      end;
    end;

  finally
    if assigned(aXmlDocument) then aXmlDocument.free;
  end;

end;

{************************************************************************}
procedure TalSqlite3ConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                    Skip: Integer;
                                                    First: Integer;
                                                    OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const FormatSettings: TALFormatSettings;
                                                    const ConnectionHandle: PSQLite3 = nil);
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
                                                    OnNewRowFunct: TalSqlite3ClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const FormatSettings: TALFormatSettings;
                                                    const ConnectionHandle: PSQLite3 = nil);
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
                                                    const ConnectionHandle: PSQLite3 = nil);
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
                                                    const ConnectionHandle: PSQLite3 = nil);
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
                                                    const ConnectionHandle: PSQLite3 = nil);
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
                                                    const ConnectionHandle: PSQLite3 = nil);
Var astmt: PSQLite3Stmt;
    aTmpConnectionHandle: PSQLite3;
    aOwnConnection: Boolean;
    aStopWatch: TStopWatch;
begin

  //acquire a connection and start the transaction if necessary
  aTmpConnectionHandle := ConnectionHandle;
  aOwnConnection := (not assigned(ConnectionHandle));
  if aOwnConnection then TransactionStart(aTmpConnectionHandle, False);
  Try

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //start the TstopWatch
    aStopWatch.Reset;
    aStopWatch.Start;

    //prepare the query
    CheckAPIError(aTmpConnectionHandle, FLibrary.sqlite3_prepare_v2(aTmpConnectionHandle, PAnsiChar(SQL), length(SQL), astmt, nil) <> SQLITE_OK);
    Try

      //retrieve the next row
      CheckAPIError(aTmpConnectionHandle, not (FLibrary.sqlite3_step(astmt) in [SQLITE_DONE, SQLITE_ROW]));

    Finally
      //free the memory used by the API
      CheckAPIError(aTmpConnectionHandle, FLibrary.sqlite3_finalize(astmt) <> SQLITE_OK);
    End;

    //do the OnUpdateDataDone
    aStopWatch.Stop;
    OnUpdateDataDone(SQL,
                     aStopWatch.Elapsed.TotalMilliseconds);

    //commit the transaction and release the connection if owned
    if aOwnConnection then TransactionCommit(aTmpConnectionHandle);

  except
    On E: Exception do begin

      //rollback the transaction and release the connection if owned
      if aOwnConnection then TransactionRollback(aTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{**********************************************************************************}
procedure TalSqlite3ConnectionPoolClient.UpdateData(const SQLs: array of AnsiString;
                                                    const ConnectionHandle: PSQLite3 = nil);
Var aTmpConnectionHandle: PSQLite3;
    aOwnConnection: Boolean;
    i: integer;
begin

  //acquire a connection and start the transaction if necessary
  aTmpConnectionHandle := ConnectionHandle;
  aOwnConnection := (not assigned(ConnectionHandle));
  if aOwnConnection then TransactionStart(aTmpConnectionHandle, False);
  Try

    //update the data
    for I := Low(SQLs) to High(SQLs) do
      UpdateData(SQLs[i],
                 aTmpConnectionHandle);

    //commit the transaction and release the connection if owned
    if aOwnConnection then TransactionCommit(aTmpConnectionHandle);

  except
    On E: Exception do begin

      //rollback the transaction and release the connection if owned
      if aOwnConnection then TransactionRollback(aTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{*******************************************************************}
procedure TalSqlite3ConnectionPoolClient.UpdateData(SQLs: TALStrings;
                                                    const ConnectionHandle: PSQLite3 = nil);
Var aTmpConnectionHandle: PSQLite3;
    aOwnConnection: Boolean;
    i: integer;
begin

  //acquire a connection and start the transaction if necessary
  aTmpConnectionHandle := ConnectionHandle;
  aOwnConnection := (not assigned(ConnectionHandle));
  if aOwnConnection then TransactionStart(aTmpConnectionHandle, False);
  Try

    //update the data
    for I := 0 to SQLs.Count - 1 do
      UpdateData(SQLs[i],
                 aTmpConnectionHandle);

    //commit the transaction and release the connection if owned
    if aOwnConnection then TransactionCommit(aTmpConnectionHandle);

  except
    On E: Exception do begin

      //rollback the transaction and release the connection if owned
      if aOwnConnection then TransactionRollback(aTmpConnectionHandle, true);

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
