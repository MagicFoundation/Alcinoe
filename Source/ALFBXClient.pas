{*******************************************************************************
product:      ALFBXClient (Alcinoe FireBird Express Client)
Description:  Retrieving Data as XML from Firebird Server.
Link :        http://www.progdigy.com/modules.php?name=UIB
*******************************************************************************}

unit ALFBXClient;

interface

uses
  Winapi.Windows,
  System.classes,
  System.SysUtils,
  System.Contnrs,
  System.SyncObjs,
  AlXmlDoc,
  ALAVLBinaryTree,
  ALFBXLib,
  ALFBXBase,
  ALString,
  ALStringList;

Type

  {----------------------------------------------------------------------------------}
  TALFBXClientSelectDataOnNewRowFunct = reference to Procedure(XMLRowData: TalXmlNode;
                                                               const ViewTag: AnsiString;
                                                               ExtData: Pointer;
                                                               Var Continue: Boolean);

  {----------------------------}
  TALFBXClientSQLParam = record
    Value: AnsiString;
    IsNull: Boolean;
    class function Create: TALFBXClientSQLParam; static; inline;
  end;
  TALFBXClientSQLParams = array of TALFBXClientSQLParam;

  {----------------------------------}
  TALFBXClientSelectDataQUERY = record
    SQL: AnsiString;
    Params: TALFBXClientSQLParams; // use to replace the ? in SQL like
                                   // Select ... from TableA(FieldA) where Values = ?
    RowTag: AnsiString;
    ViewTag: AnsiString;
    Skip: integer; // used only if value is > 0
    First: Integer; // used only if value is > 0
    CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                             // cache or not. Values <= 0 deactivate the cache
    class function Create: TALFBXClientSelectDataQUERY; static; inline;
  end;
  TALFBXClientSelectDataQUERIES = array of TALFBXClientSelectDataQUERY;

  {----------------------------------}
  TALFBXClientUpdateDataQUERY = record
    SQL: AnsiString;
    Params: TALFBXClientSQLParams; // use to replace the ? in SQL like
                                   // insert into TableA(FieldA) Values(?)
    class function Create: TALFBXClientUpdateDataQUERY; static; inline;
  end;
  TALFBXClientUpdateDataQUERIES = array of TALFBXClientUpdateDataQUERY;

  {------------------------------------}
  TALFBXClientMonitoringIOStats = record
    page_reads,
    page_writes,
    page_fetches,
    page_marks: int64;
  end;

  {----------------------------------------}
  TALFBXClientMonitoringRecordStats = record
    record_idx_reads,
    record_seq_reads,
    record_inserts,
    record_updates,
    record_deletes,
    record_backouts,
    record_purges,
    record_expunges: int64;
  end;

  {----------------------------------------}
  TALFBXClientMonitoringMemoryUsage = record
    memory_used,
    memory_allocated,
    max_memory_used,
    max_memory_allocated: int64;
  end;

  {---------------------------}
  TALFBXClient = Class(Tobject)
  Private
    FSQLDIALECT: word;
    FownLibrary: Boolean;
    FLibrary: TALFBXLibrary;
    fDBHandle: IscDbHandle;
    fTraHandle: IscTrHandle;
    fStmtHandle: IscStmtHandle;
    fSqlda: TALFBXSQLResult;
    fStmtSQL: AnsiString;
    fNullString: AnsiString;
    fCharSet: TALFBXCharacterSet;
    fDefaultReadTPB: AnsiString;
    fDefaultWriteTPB: AnsiString;
    function  GetConnected: Boolean;
    function  GetInTransaction: Boolean;
    function  GetConnectionID: Integer;
    function  GetTransactionID: Cardinal;
  Protected
    function loadCachedData(const Key: AnsiString;
                            var DataStr: AnsiString): Boolean; virtual;
    Procedure SaveDataToCache(const Key: ansiString;
                              const CacheThreshold: integer;
                              const DataStr: ansiString); virtual;
    function GetFieldValue(aSQLDA: TALFBXSQLResult;
                           aDBHandle: IscDbHandle;
                           aTraHandle: IscTrHandle;
                           aIndex: Integer;
                           const aFormatSettings: TALFormatSettings): AnsiString;
    procedure initObject; virtual;
    procedure OnSelectDataDone(const Query: TALFBXClientSelectDataQUERY;
                               TimeTaken: double); virtual;
    procedure OnUpdateDataDone(const Query: TALFBXClientUpdateDataQUERY;
                               TimeTaken: double); virtual;
  Public
    Constructor Create(ApiVer: TALFBXVersion_API; const lib: AnsiString = GDS32DLL); overload; virtual;
    Constructor Create(lib: TALFBXLibrary); overload; virtual;
    Destructor Destroy; Override;
    procedure GetMonitoringInfos(ConnectionID,
                                 TransactionID: int64;
                                 const StatementSQL: AnsiString;
                                 Var IOStats: TALFBXClientMonitoringIOStats;
                                 Var RecordStats: TALFBXClientMonitoringRecordStats;
                                 Var MemoryUsage: TALFBXClientMonitoringMemoryUsage;
                                 Const SkipIOStats: Boolean = False;
                                 Const SkipRecordStats: Boolean = False;
                                 Const SkipMemoryUsage: Boolean = False);
    function  GetDataBaseInfoInt(const item: Integer): Integer;
    function  GetDataBaseInfoString(const item: Integer): AnsiString;
    function  GetDataBaseInfoDateTime(const item: Integer): TDateTime;
    procedure GetUserNames(UserNames: TALStrings);
    procedure CreateDatabase(const SQL: AnsiString);
    procedure DropDatabase;
    Procedure Connect(const DataBaseName,
                            Login,
                            Password,
                            CharSet: AnsiString;
                      const ExtraParams: AnsiString = ''); overload;
    Procedure Connect(const DataBaseName,
                            Login,
                            Password,
                            CharSet: AnsiString;
                      Numbuffers: integer); overload;
    Procedure Disconnect;
    Procedure TransactionStart(const TPB: AnsiString);
    Procedure TransactionCommit;
    Procedure TransactionCommitRetaining;
    Procedure TransactionRollback;
    Procedure TransactionRollbackRetaining;
    Function  Prepare(const SQL: AnsiString): TALFBXStatementType;
    Procedure SelectData(const Queries: TALFBXClientSelectDataQUERIES;
                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const Query: TALFBXClientSelectDataQUERY;
                         OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const Queries: TALFBXClientSelectDataQUERIES;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const Query: TALFBXClientSelectDataQUERY;
                         XMLDATA: TalXMLNode;
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
    Procedure SelectData(const SQL: AnsiString;
                         const Params: Array of AnsiString;
                         const RowTag: AnsiString;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const Params: Array of AnsiString;
                         const RowTag: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const Params: Array of AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    procedure UpdateData(const Queries: TALFBXClientUpdateDataQUERIES); overload; virtual;
    procedure UpdateData(const Query: TALFBXClientUpdateDataQUERY); overload; virtual;
    procedure UpdateData(SQLs: TALStrings); overload; virtual;
    procedure UpdateData(const SQL: AnsiString); overload; virtual;
    procedure UpdateData(const SQL: AnsiString;
                         const Params: Array of AnsiString); overload; virtual;
    procedure UpdateData(const SQLs: array of AnsiString); overload; virtual;
    Property  Connected: Boolean Read GetConnected;
    property  SqlDialect: word read FSqlDialect;
    Property  InTransaction: Boolean read GetInTransaction;
    Property  NullString: AnsiString Read fNullString Write fNullString;
    property  Lib: TALFBXLibrary read FLibrary;
    property  CharSet: TALFBXCharacterSet read fCharSet;
    property  DefaultReadTPB: AnsiString read fDefaultReadTPB Write fDefaultReadTPB;
    property  DefaultWriteTPB: AnsiString read fDefaultWriteTPB write fDefaultWriteTPB;
    property  TransactionID: Cardinal read GetTransactionID;
    property  ConnectionID: Integer read GetConnectionID;
  end;

  {-----------------------------------------------------------------------------}
  TALFBXConnectionStatementPoolBinTreeNode = class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Lib: TALFBXLibrary;
    StmtHandle: IscStmtHandle;
    Sqlda: TALFBXSQLResult;
    LastAccessDate: int64;
    OwnsObjects: Boolean;
    Constructor Create; Override;
    destructor Destroy; Override;
  end;

  {---------------------------------------------------------------------}
  TALFBXConnectionStatementPoolBinTree = class(TALStringKeyAVLBinaryTree)
  Public
    LastGarbage: UInt64;
    Constructor Create; override;
  end;

  {----------------------------------------------------}
  TALFBXConnectionWithStmtPoolContainer = Class(TObject)
    DBHandle: IscDbHandle;
    StatementPool: TALFBXConnectionStatementPoolBinTree;
    LastAccessDate: int64;
  End;

  {-------------------------------------------------------}
  TALFBXConnectionWithoutStmtPoolContainer = Class(TObject)
    DBHandle: IscDbHandle;
    LastAccessDate: int64;
  End;

  {-------------------------------------------------}
  TALFBXReadTransactionPoolContainer = Class(TObject)
    DBHandle: IscDbHandle;
    TraHandle: IscTrHandle;
    LastAccessDate: int64;
  End;

  {-----------------------------------------------}
  TALFBXReadStatementPoolContainer = Class(TObject)
    DBHandle: IscDbHandle;
    TraHandle: IscTrHandle;
    StmtHandle: IscStmtHandle;
    Sqlda: TALFBXSQLResult;
    LastAccessDate: int64;
  End;

  {-------------------------------------------------------------------}
  TALFBXStringKeyPoolBinTreeNode = class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Pool: TObjectList;
    Constructor Create; Override;
    destructor Destroy; Override;
  end;

  {-----------------------------------------}
  TALFBXConnectionPoolClient = Class(Tobject)
  Private
    FSQLDIALECT: word;
    FLibrary: TALFBXLibrary;
    FownLibrary: Boolean;

    //--CONNECTION WITH STATEMENT POOL--
    FConnectionWithStmtPool: TObjectList;                   // pool of connection With list of statements
    FConnectionWithStmtPoolCS: TCriticalSection;
    FWorkingConnectionWithStmtCount: Integer;
    FLastConnectionWithStmtGarbage: Int64;
    //--CONNECTION WITH STATEMENT POOL--

    //--CONNECTION WITHOUT STATEMENT POOL--
    FConnectionWithoutStmtPool: TObjectList;                // pool of connection WITHOUT list of statements
    FConnectionWithoutStmtPoolCS: TCriticalSection;
    FWorkingConnectionWithoutStmtCount: Integer;
    FLastConnectionWithoutStmtGarbage: Int64;
    //--CONNECTION WITHOUT STATEMENT POOL--

    //--READ TRANSACTION POOL--
    FReadTransactionPool: TALStringKeyAVLBinaryTree;     // pool of READ ONLY READ COMMITED TRANSACTION
    FReadTransactionPoolCS: TCriticalSection;
    FWorkingReadTransactionCount: Integer;
    FLastReadTransactionGarbage: UInt64;
    //--READ TRANSACTION POOL--

    //--READ STATEMENT POOL--
    FReadStatementPool: TALStringKeyAVLBinaryTree;       // pool of STATEMENT with READ ONLY READ COMMITED TRANSACTION
    FReadStatementPoolCS: TCriticalSection;
    FWorkingReadStatementCount: Integer;
    FLastReadStatementGarbage: UInt64;
    //--READ STATEMENT POOL--

    FReleasingAllconnections: Boolean;
    FConnectionMaxIdleTime: integer;
    FTransactionMaxIdleTime: integer;
    FStatementMaxIdleTime: integer;
    FDataBaseName: AnsiString;
    fCharSet: TALFBXCharacterSet;
    fOpenConnectionParams: AnsiString;
    FNullString: AnsiString;
    FLogin: AnsiString;
    FPassword: AnsiString;
    fDefaultReadTPB: AnsiString;
    fDefaultWriteTPB: AnsiString;
  Protected
    function loadCachedData(const Key: AnsiString;
                            var DataStr: AnsiString): Boolean; virtual;
    Procedure SaveDataToCache(const Key: ansiString;
                              const CacheThreshold: integer;
                              const DataStr: ansiString); virtual;
    function GetCloseConnectionByErrCode(aGDSCode: Integer): Boolean; virtual;
    function GetDataBaseName: AnsiString; virtual;
    function GetFieldValue(aSQLDA: TALFBXSQLResult;
                           aDBHandle: IscDbHandle;
                           aTraHandle: IscTrHandle;
                           aIndex: Integer;
                           const aFormatSettings: TALFormatSettings): AnsiString; virtual;
    Procedure AcquireConnectionWithStmt(var DBHandle: IscDbHandle;
                                        var StatementPool: TALFBXConnectionStatementPoolBinTree); virtual;
    Procedure ReleaseConnectionWithStmt(var DBHandle: IscDbHandle;
                                        var StatementPool: TALFBXConnectionStatementPoolBinTree;
                                        const CloseConnection: Boolean = False); virtual;
    Function  AcquireConnectionWithoutStmt: IscDbHandle; virtual;
    Procedure ReleaseConnectionWithoutStmt(var DBHandle: IscDbHandle;
                                            const CloseConnection: Boolean = False); virtual;
    Procedure AcquireReadTransaction(var DBHandle: IscDbHandle;
                                     var TraHandle: IscTrHandle;
                                     const TPB: AnsiString); virtual;
    Procedure ReleaseReadTransaction(var DBHandle: IscDbHandle;
                                     var TraHandle: IscTrHandle;
                                     const TPB: AnsiString;
                                     const CloseConnection: Boolean = False); virtual;
    Procedure AcquireReadStatement(const SQL: AnsiString;
                                   var DBHandle: IscDbHandle;
                                   var TraHandle: IscTrHandle;
                                   var StmtHandle: IscStmtHandle;
                                   var Sqlda: TALFBXSQLResult;
                                   const TPB: AnsiString); virtual;
    Procedure ReleaseReadStatement(const SQL: AnsiString;
                                   var DBHandle: IscDbHandle;
                                   var TraHandle: IscTrHandle;
                                   var StmtHandle: IscStmtHandle;
                                   var Sqlda: TALFBXSQLResult;
                                   const TPB: AnsiString;
                                   const CloseConnection: Boolean = False); virtual;
    procedure initObject(const aDataBaseName,
                               aLogin,
                               aPassword,
                               aCharSet: AnsiString;
                         const aNumbuffers: integer = -1;
                         const aOpenConnectionExtraParams: AnsiString = ''); virtual;
    procedure OnSelectDataDone(const Query: TALFBXClientSelectDataQUERY;
                               TimeTaken: double); virtual;
    procedure OnUpdateDataDone(const Query: TALFBXClientUpdateDataQUERY;
                               TimeTaken: double); virtual;
  Public
    Constructor Create(const aDataBaseName,
                             aLogin,
                             aPassword,
                             aCharSet: AnsiString;
                       aApiVer: TALFBXVersion_API;
                       const alib: AnsiString = GDS32DLL;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: AnsiString = ''); overload; virtual;
    Constructor Create(const aDataBaseName,
                             aLogin,
                             aPassword,
                             aCharSet: AnsiString;
                       alib: TALFBXLibrary;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: AnsiString = ''); overload; virtual;
    Destructor  Destroy; Override;
    Procedure ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True); virtual;
    procedure GetMonitoringInfos(ConnectionID,
                                 TransactionID: int64;
                                 const StatementSQL: AnsiString;
                                 Var IOStats: TALFBXClientMonitoringIOStats;
                                 Var RecordStats: TALFBXClientMonitoringRecordStats;
                                 Var MemoryUsage: TALFBXClientMonitoringMemoryUsage;
                                 Const SkipIOStats: Boolean = False;
                                 Const SkipRecordStats: Boolean = False;
                                 Const SkipMemoryUsage: Boolean = False);
    function  GetConnectionID(DBHandle: IscDbHandle): Integer;
    function  GetTransactionID(TraHandle: IscTrHandle): Cardinal;
    function  GetDataBaseInfoInt(const item: Integer;
                                 const DBHandle: IscDbHandle= nil): Integer;
    function  GetDataBaseInfoString(const item: Integer;
                                    const DBHandle: IscDbHandle= nil): AnsiString;
    function  GetDataBaseInfoDateTime(const item: Integer;
                                      const DBHandle: IscDbHandle= nil): TDateTime;
    Procedure TransactionStart(Var DBHandle: IscDbHandle;
                               var TraHandle: IscTrHandle;
                               var StatementPool: TALFBXConnectionStatementPoolBinTree;
                               const TPB: AnsiString); overload; virtual;
    Procedure TransactionCommit(var DBHandle: IscDbHandle;
                                var TraHandle: IscTrHandle;
                                var StatementPool: TALFBXConnectionStatementPoolBinTree;
                                const CloseConnection: Boolean = False); overload; virtual;
    Procedure TransactionRollback(var DBHandle: IscDbHandle;
                                  var TraHandle: IscTrHandle;
                                  var StatementPool: TALFBXConnectionStatementPoolBinTree;
                                  const CloseConnection: Boolean = False); overload; virtual;
    Procedure TransactionStart(Var DBHandle: IscDbHandle;
                               var TraHandle: IscTrHandle;
                               const TPB: AnsiString); overload; virtual;
    Procedure TransactionCommit(var DBHandle: IscDbHandle;
                                var TraHandle: IscTrHandle;
                                const CloseConnection: Boolean = False); overload; virtual;
    Procedure TransactionRollback(var DBHandle: IscDbHandle;
                                  var TraHandle: IscTrHandle;
                                  const CloseConnection: Boolean = False); overload; virtual;
    Procedure TransactionCommitRetaining(TraHandle: IscTrHandle); virtual;
    Procedure TransactionRollbackRetaining(TraHandle: IscTrHandle); virtual;
    Function  Prepare(const SQL: AnsiString;
                      Var DBHandle: IscDbHandle;
                      var TraHandle: IscTrHandle;
                      var StmtHandle: IscStmtHandle;
                      var Sqlda: TALFBXSQLResult;
                      const TPB: AnsiString = ''): TALFBXStatementType;
    Procedure SelectData(const Queries: TALFBXClientSelectDataQUERIES;
                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const Query: TALFBXClientSelectDataQUERY;
                         OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const Queries: TALFBXClientSelectDataQUERIES;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const Query: TALFBXClientSelectDataQUERY;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const Params: Array of AnsiString;
                         const RowTag: AnsiString;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const Params: Array of AnsiString;
                         const RowTag: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const Params: Array of AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    procedure UpdateData(const Queries: TALFBXClientUpdateDataQUERIES;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    procedure UpdateData(const Query: TALFBXClientUpdateDataQUERY;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    procedure UpdateData(SQLs: TALStrings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    procedure UpdateData(const SQL: AnsiString;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    procedure UpdateData(const SQL: AnsiString;
                         const Params: Array of AnsiString;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    procedure UpdateData(const SQLs: array of AnsiString;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                         const TPB: AnsiString = ''); overload; virtual;
    Function  ConnectionCount: Integer;
    Function  WorkingConnectionCount: Integer;
    property  SqlDialect: word read FSqlDialect;
    property  DataBaseName: AnsiString read GetDataBaseName;
    property  Login: AnsiString read FLogin;
    property  Password: AnsiString read FPassword;
    property  ConnectionMaxIdleTime: integer read FConnectionMaxIdleTime write fConnectionMaxIdleTime;
    property  TransactionMaxIdleTime: integer read FTransactionMaxIdleTime write fTransactionMaxIdleTime;
    property  StatementMaxIdleTime: integer read FStatementMaxIdleTime write fStatementMaxIdleTime;
    Property  NullString: AnsiString Read fNullString Write fNullString;
    property  Lib: TALFBXLibrary read FLibrary;
    property  CharSet: TALFBXCharacterSet read fCharSet;
    property  DefaultReadTPB: AnsiString read fDefaultReadTPB Write fDefaultReadTPB;
    property  DefaultWriteTPB: AnsiString read fDefaultWriteTPB write fDefaultWriteTPB;
  end;

  {-------------------------------------------------------------------------------------------------------------}
  TALFBXEventThreadEvent = reference to Procedure (Sender: TObject; const EventName: AnsiString; Count: Integer);
  TALFBXEventThreadException = reference to procedure (Sender: TObject; Error: Exception);

  {--------------------------------}
  TALFBXEventThread = class(TThread)
  private
    fConnectionMaxIdleTime: Cardinal;
    FDBHandle: IscDbHandle;
    FQueueEvent: boolean;
    fResultBuffer: PAnsiChar;
    FSignal: Thandle;
    FCompleted: Boolean;
    FStarted: Boolean;
    FEventCanceled: Boolean;
    FWaitingSignal: Boolean;
    FLibrary: TALFBXLibrary;
    FownLibrary: Boolean;
    FDataBaseName: AnsiString;
    fCharSet: TALFBXCharacterSet;
    fOpenConnectionParams: AnsiString;
    fEventNamesArr: array[0..14] of AnsiString;
    fEventNamesCount: integer;
    fOnEvent: TALFBXEventThreadEvent;
    fOnException: TALFBXEventThreadException;
  protected
    procedure initObject(const aDataBaseName,
                               aLogin,
                               aPassword,
                               aCharSet: AnsiString;
                         const aEventNames: AnsiString;
                         aConnectionMaxIdleTime: integer;
                         aNumbuffers: integer;
                         const aOpenConnectionExtraParams: AnsiString;
                         aOnEvent: TALFBXEventThreadEvent;
                         aOnException: TALFBXEventThreadException); virtual;
    procedure DoEvent(const EventName: AnsiString; Count: Integer); virtual;
    procedure DoException(Error: Exception); virtual;
  public
    constructor Create(const aDataBaseName,
                             aLogin,
                             aPassword,
                             aCharSet: AnsiString;
                       const aEventNames: AnsiString; // ; separated value like EVENT1;EVENT2; etc...
                       aApiVer: TALFBXVersion_API;
                       aOnEvent: TALFBXEventThreadEvent;
                       aOnException: TALFBXEventThreadException;
                       const alib: AnsiString = GDS32DLL;
                       const aConnectionMaxIdleTime: integer = -1;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: AnsiString = ''); overload; virtual;
    Constructor Create(const aDataBaseName,
                             aLogin,
                             aPassword,
                             aCharSet: AnsiString;
                       const aEventNames: AnsiString; // ; separated value like EVENT1;EVENT2; etc...
                       alib: TALFBXLibrary;
                       aOnEvent: TALFBXEventThreadEvent;
                       aOnException: TALFBXEventThreadException;
                       const aConnectionMaxIdleTime: integer = -1;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: AnsiString = ''); overload; virtual;
    constructor Create(const aDataBaseName,
                             aLogin,
                             aPassword,
                             aCharSet: AnsiString;
                       const aEventNames: AnsiString; // ; separated value like EVENT1;EVENT2; etc...
                       aApiVer: TALFBXVersion_API;
                       const alib: AnsiString = GDS32DLL;
                       const aConnectionMaxIdleTime: integer = -1;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: AnsiString = ''); overload; virtual;
    Constructor Create(const aDataBaseName,
                             aLogin,
                             aPassword,
                             aCharSet: AnsiString;
                       const aEventNames: AnsiString; // ; separated value like EVENT1;EVENT2; etc...
                       alib: TALFBXLibrary;
                       const aConnectionMaxIdleTime: integer = -1;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: AnsiString = ''); overload; virtual;
    Destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Execute; override;
    property  Signal: Thandle read FSignal;
    property  OnEvent: TALFBXEventThreadEvent read fOnEvent write fOnEvent;
    property  OnException: TALFBXEventThreadException read fOnException write fOnException;
  end;

Const  cALFbxClientDefaultReadNOWaitTPB = isc_tpb_version3 + //Transaction version number is used internally by the InterBase engine. It is always be
                                                             //the first attribute specified in the TPB, and must always be set to isc_tpb_version3.
                                                             //isc_tpb_version3 = InterBase version 3 transaction

                                          isc_tpb_read +     //Access mode describes the actions that can be performed by the functions associated with
                                                             //the transaction. Valid access modes are:
                                                             //  * isc_tpb_read: Read-only access mode that allows a transaction only to select data
                                                             //                  from tables
                                                             //  * isc_tpb_write: Read-write access mode of that allows a transaction to select, insert,
                                                             //                   update, and delete table data [Default]

                                          isc_tpb_read_committed + isc_tpb_rec_version +
                                                             // Isolation level describes the view of the database given a transaction as it relates to
                                                             // actions performed by other simultaneously occurring transactions.
                                                             // Valid isolation levels are:
                                                             //  * isc_tpb_concurrency: High throughput, high concurrency transaction with acceptable
                                                             //                         consistency; use of this parameter takes full advantage of the InterBase
                                                             //                         multi-generational transaction model [Default]
                                                             //                         By default, after a transaction starts it cannot access committed changes
                                                             //                         to a table made by other simultaneous transactions, even though it shares
                                                             //                         access to the table with them. Such a transaction has an isolation level of
                                                             //                         isc_tpb_concurrency, meaning it can have concurrent access to tables also
                                                             //                         accessed simultaneously by other transactions.
                                                             //  * isc_tpb_consistency: Table-locking transaction model
                                                             //                         InterBase also supports a restrictive isolation level. isc_tpb_consistency
                                                             //                         prevents a transaction from accessing tables if they are written to by other
                                                             //                         transactions; it also prevents other transactions from writing to a table
                                                             //                         once this transaction writes to it. This isolation level is designed to
                                                             //                         guarantee that if a transaction writes to a table before other simultaneous
                                                             //                         read and write transactions, then only it can change a table?s data. Because
                                                             //                         it essentially restricts (and often prevents) shared access to tables,
                                                             //                         isc_tpb_consistency should be used with care.
                                                             //  * isc_tpb_read_committed, isc_tpb_rec_version: High throughput, high concurrency transaction
                                                             //                                                 that can read changes committed by other concurrent
                                                             //                                                 transactions. Use of this parameter takes full advantage
                                                             //                                                 of the InterBase multi-generational transaction model.
                                                             //                                                 * isc_tpb_rec_version: Enables an isc_tpb_read_committed
                                                             //                                                   transaction to read the most recently
                                                             //                                                   committed version of a record even if
                                                             //                                                   other, uncommitted versions are pending.
                                                             //                                                 -------
                                                             //                                                 isc_tpb_read_committed, offers all the advantages of the
                                                             //                                                 isc_tpb_concurrency isolation level and additionally enables
                                                             //                                                 a transaction to access changes committed by other
                                                             //                                                 simultaneous transactions. Two other parameters,
                                                             //                                                 isc_tpb_rec_version, and isc_tpb_no_rec_version, should be
                                                             //                                                 used with the isc_tpb_read_committed parameter. They offer
                                                             //                                                 refined control over the committed changes a transaction is
                                                             //                                                 permitted to access:
                                                             //                                                 * isc_tpb_rec_version specifies that a transaction can read
                                                             //                                                   the latest committed version of a row, even if a more recent
                                                             //                                                   uncommitted version is pending.

                                                             //  * isc_tpb_read_committed, isc_tpb_no_rec_version: High throughput, high concurrency transaction
                                                             //                                                    that can read changes committed by other concurrent
                                                             //                                                    transactions. Use of this parameter takes full advantage
                                                             //                                                    of the InterBase multi-generational transaction model.
                                                             //                                                    * isc_tpb_no_rec_version: Enables an isc_tpb_read_committed
                                                             //                                                      transaction to read only the latest committed version of
                                                             //                                                      a record. If an uncommitted version of a record is
                                                             //                                                      pending and isc_tpb_wait is also specified, then the
                                                             //                                                      transaction waits for the pending record to be committed
                                                             //                                                      or rolled back before proceeding. Otherwise, a lock
                                                             //                                                      conflict error is reported at once.
                                                             //                                                    -------
                                                             //                                                    isc_tpb_read_committed, offers all the advantages of the
                                                             //                                                    isc_tpb_concurrency isolation level and additionally enables
                                                             //                                                    a transaction to access changes committed by other
                                                             //                                                    simultaneous transactions. Two other parameters,
                                                             //                                                    isc_tpb_rec_version, and isc_tpb_no_rec_version, should be
                                                             //                                                    used with the isc_tpb_read_committed parameter. They offer
                                                             //                                                    refined control over the committed changes a transaction is
                                                             //                                                    permitted to access:
                                                             //                                                    * isc_tpb_no_rec_version, the default refinement,
                                                             //                                                      specifies that a transaction can only read the latest
                                                             //                                                      version of a row. If a change to a row is pending, but
                                                             //                                                      not yet committed, the row cannot be read.

                                          isc_tpb_nowait;    // Lock resolution describes how a transaction should react if a lock conflict occurs. Valid
                                                             // lock resolutions are:
                                                             // * isc_tpb_wait: Lock resolution specifies that the transaction is to wait until locked
                                                             //                 resources are released before retrying an operation [Default]
                                                             // * isc_tpb_nowait: Lock resolution specifies that the transaction is not to wait for locks to be
                                                             //                   released, but instead, a lock conflict error should be returned immediately

       cALFbxClientDefaultWriteNOWaitTPB = isc_tpb_version3 + //Transaction version number is used internally by the InterBase engine. It is always be
                                                              //the first attribute specified in the TPB, and must always be set to isc_tpb_version3.
                                                              //isc_tpb_version3 = InterBase version 3 transaction

                                           isc_tpb_write +    //Access mode describes the actions that can be performed by the functions associated with
                                                              //the transaction. Valid access modes are:
                                                              //  * isc_tpb_read: Read-only access mode that allows a transaction only to select data
                                                              //                  from tables
                                                              //  * isc_tpb_write: Read-write access mode of that allows a transaction to select, insert,
                                                              //                   update, and delete table data [Default]

                                           isc_tpb_read_committed + isc_tpb_no_rec_version +
                                                              // Isolation level describes the view of the database given a transaction as it relates to
                                                              // actions performed by other simultaneously occurring transactions.
                                                              // Valid isolation levels are:
                                                              //  * isc_tpb_concurrency: High throughput, high concurrency transaction with acceptable
                                                              //                         consistency; use of this parameter takes full advantage of the InterBase
                                                              //                         multi-generational transaction model [Default]
                                                              //                         By default, after a transaction starts it cannot access committed changes
                                                              //                         to a table made by other simultaneous transactions, even though it shares
                                                              //                         access to the table with them. Such a transaction has an isolation level of
                                                              //                         isc_tpb_concurrency, meaning it can have concurrent access to tables also
                                                              //                         accessed simultaneously by other transactions.
                                                              //  * isc_tpb_consistency: Table-locking transaction model
                                                              //                         InterBase also supports a restrictive isolation level. isc_tpb_consistency
                                                              //                         prevents a transaction from accessing tables if they are written to by other
                                                              //                         transactions; it also prevents other transactions from writing to a table
                                                              //                         once this transaction writes to it. This isolation level is designed to
                                                              //                         guarantee that if a transaction writes to a table before other simultaneous
                                                              //                         read and write transactions, then only it can change a table?s data. Because
                                                              //                         it essentially restricts (and often prevents) shared access to tables,
                                                              //                         isc_tpb_consistency should be used with care.
                                                              //  * isc_tpb_read_committed, isc_tpb_rec_version: High throughput, high concurrency transaction
                                                              //                                                 that can read changes committed by other concurrent
                                                              //                                                 transactions. Use of this parameter takes full advantage
                                                              //                                                 of the InterBase multi-generational transaction model.
                                                              //                                                 * isc_tpb_rec_version: Enables an isc_tpb_read_committed
                                                              //                                                   transaction to read the most recently
                                                              //                                                   committed version of a record even if
                                                              //                                                   other, uncommitted versions are pending.
                                                              //                                                 -------
                                                              //                                                 isc_tpb_read_committed, offers all the advantages of the
                                                              //                                                 isc_tpb_concurrency isolation level and additionally enables
                                                              //                                                 a transaction to access changes committed by other
                                                              //                                                 simultaneous transactions. Two other parameters,
                                                              //                                                 isc_tpb_rec_version, and isc_tpb_no_rec_version, should be
                                                              //                                                 used with the isc_tpb_read_committed parameter. They offer
                                                              //                                                 refined control over the committed changes a transaction is
                                                              //                                                 permitted to access:
                                                              //                                                 * isc_tpb_rec_version specifies that a transaction can read
                                                              //                                                   the latest committed version of a row, even if a more recent
                                                              //                                                   uncommitted version is pending.

                                                              //  * isc_tpb_read_committed, isc_tpb_no_rec_version: High throughput, high concurrency transaction
                                                              //                                                    that can read changes committed by other concurrent
                                                              //                                                    transactions. Use of this parameter takes full advantage
                                                              //                                                    of the InterBase multi-generational transaction model.
                                                              //                                                    * isc_tpb_no_rec_version: Enables an isc_tpb_read_committed
                                                              //                                                      transaction to read only the latest committed version of
                                                              //                                                      a record. If an uncommitted version of a record is
                                                              //                                                      pending and isc_tpb_wait is also specified, then the
                                                              //                                                      transaction waits for the pending record to be committed
                                                              //                                                      or rolled back before proceeding. Otherwise, a lock
                                                              //                                                      conflict error is reported at once.
                                                              //                                                    -------
                                                              //                                                    isc_tpb_read_committed, offers all the advantages of the
                                                              //                                                    isc_tpb_concurrency isolation level and additionally enables
                                                              //                                                    a transaction to access changes committed by other
                                                              //                                                    simultaneous transactions. Two other parameters,
                                                              //                                                    isc_tpb_rec_version, and isc_tpb_no_rec_version, should be
                                                              //                                                    used with the isc_tpb_read_committed parameter. They offer
                                                              //                                                    refined control over the committed changes a transaction is
                                                              //                                                    permitted to access:
                                                              //                                                    * isc_tpb_no_rec_version, the default refinement,
                                                              //                                                      specifies that a transaction can only read the latest
                                                              //                                                      version of a row. If a change to a row is pending, but
                                                              //                                                      not yet committed, the row cannot be read.

                                           isc_tpb_nowait;    // Lock resolution describes how a transaction should react if a lock conflict occurs. Valid
                                                              // lock resolutions are:
                                                              // * isc_tpb_wait: Lock resolution specifies that the transaction is to wait until locked
                                                              //                 resources are released before retrying an operation [Default]
                                                              // * isc_tpb_nowait: Lock resolution specifies that the transaction is not to wait for locks to be
                                                              //

       cALFbxClientDefaultReadWaitTPB = isc_tpb_version3 +
                                        isc_tpb_read +
                                        isc_tpb_read_committed + isc_tpb_rec_version +
                                        isc_tpb_wait;

       cALFbxClientDefaultWriteWaitTPB = isc_tpb_version3 +
                                         isc_tpb_write +
                                         isc_tpb_read_committed + isc_tpb_no_rec_version +
                                         isc_tpb_wait;

implementation

uses
  System.Diagnostics,
  ALCipher,
  ALWindows,
  ALCommon,
  alfbxError;

{***************************************************************}
class function TALFBXClientSQLParam.Create: TALFBXClientSQLParam;
begin
  with result do begin
    Value := '';
    IsNull := false;
  end;
end;

{*****************************************************************************}
class function TALFBXClientSelectDataQUERY.Create: TALFBXClientSelectDataQUERY;
begin
  with result do begin
    SQL := '';
    setlength(Params, 0);
    RowTag := '';
    ViewTag := '';
    Skip := -1;
    First := -1;
    CacheThreshold := -1;
  end;
end;

{*****************************************************************************}
class function TALFBXClientUpdateDataQUERY.Create: TALFBXClientUpdateDataQUERY;
begin
  with result do begin
    SQL := '';
    setlength(Params, 0);
  end;
end;

{*********************************************}
function TALFBXClient.GetConnectionID: integer;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //result
  result := GetDataBaseInfoInt(isc_info_attachment_id);

end;

{***********************************************}
function TALFBXClient.GetTransactionID: Cardinal;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');
  if not InTransaction then raise Exception.Create('No active transaction');

  //result
  Result := Flibrary.TransactionGetId(fTraHandle)

end;

{******************************************}
function TALFBXClient.GetConnected: Boolean;
begin
  result := assigned(fDBHandle)
end;

{**********************************************}
function TALFBXClient.GetInTransaction: Boolean;
begin
  result := assigned(fTraHandle)
end;

{*****************************************************}
procedure TALFBXClient.GetMonitoringInfos(ConnectionID,
                                          TransactionID: int64;
                                          const StatementSQL: AnsiString;
                                          Var IOStats: TALFBXClientMonitoringIOStats;
                                          Var RecordStats: TALFBXClientMonitoringRecordStats;
                                          Var MemoryUsage: TALFBXClientMonitoringMemoryUsage;
                                          Const SkipIOStats: Boolean = False;
                                          Const SkipRecordStats: Boolean = False;
                                          Const SkipMemoryUsage: Boolean = False);

Var LXMLDATA: TalXmlDocument;
    LSelectPart: AnsiString;
    LFromPart: AnsiString;
    LJoinPart: AnsiString;
    LWherePart: AnsiString;

    L_saved_TraHandle: IscTrHandle;
    L_saved_StmtHandle: IscStmtHandle;
    L_saved_Sqlda: TALFBXSQLResult;
    L_saved_StmtSQL: AnsiString;

Begin

  //The key term of the monitoring feature is an activity snapshot. It represents the current state of the database, comprising a
  //variety of information about the database itself, active attachments and users, transactions, prepared and running statements, and more.
  //
  //A snapshot is created the first time any of the monitoring tables is being selected from in the given transaction and it is
  //preserved until the transaction ends, in order that multiple-table queries (e.g., master-detail ones) will always return a consistent view of the data.
  //
  //In other words, the monitoring tables always behave like a snapshot table stability ("consistency") transaction, even if the host transaction
  //has been started with a lower isolation level.
  //
  //To refresh the snapshot, the current transaction should be finished and the monitoring tables should be queried in a new transaction context.

  //security check
  if not Connected then raise Exception.Create('Not connected');

  //build the aSelectPart;
  LSelectPart := ALIfThen(not SkipIOStats,     'MON$PAGE_READS as PAGE_READS, '+
                                               'MON$PAGE_WRITES as PAGE_WRITES, '+
                                               'MON$PAGE_FETCHES as PAGE_FETCHES, '+
                                               'MON$PAGE_MARKS as PAGE_MARKS, ') +
                 ALIfThen(not SkipRecordStats, 'MON$RECORD_SEQ_READS as RECORD_SEQ_READS, '+
                                               'MON$RECORD_IDX_READS as RECORD_IDX_READS, '+
                                               'MON$RECORD_INSERTS as RECORD_INSERTS, '+
                                               'MON$RECORD_UPDATES as RECORD_UPDATES, '+
                                               'MON$RECORD_DELETES as RECORD_DELETES, '+
                                               'MON$RECORD_BACKOUTS as RECORD_BACKOUTS, '+
                                               'MON$RECORD_PURGES as RECORD_PURGES, '+
                                               'MON$RECORD_EXPUNGES as RECORD_EXPUNGES, ') +
                 ALIfThen(not SkipMemoryUsage, 'MON$MEMORY_USED as MEMORY_USED, '+
                                               'MON$MEMORY_ALLOCATED as MEMORY_ALLOCATED, '+
                                               'MON$MAX_MEMORY_USED as MAX_MEMORY_USED, '+
                                               'MON$MAX_MEMORY_ALLOCATED as MAX_MEMORY_ALLOCATED, ');
  if LSelectPart <> '' then delete(LSelectPart,length(LSelectPart) - 1, 1)
  else Exit;

  //build the aFromPart
  if StatementSQL <> '' then       LFromPart := 'MON$STATEMENTS'
  else if TransactionID <> -1 then LFromPart := 'MON$TRANSACTIONS'
  else if ConnectionID <> -1 then  LFromPart := 'MON$ATTACHMENTS'
  else                             LFromPart := 'MON$DATABASE';

  //build the aJoinPart
  LJoinPart := ALIfThen(not SkipIOStats,     'JOIN MON$IO_STATS ON MON$IO_STATS.MON$STAT_ID = '        +LFromPart+'.MON$STAT_ID ') +
               ALIfThen(not SkipRecordStats, 'JOIN MON$RECORD_STATS ON MON$RECORD_STATS.MON$STAT_ID = '+LFromPart+'.MON$STAT_ID ') +
               ALIfThen(not SkipMemoryUsage, 'JOIN MON$MEMORY_USAGE ON MON$MEMORY_USAGE.MON$STAT_ID = '+LFromPart+'.MON$STAT_ID ');

  //build the aWherePart
  if StatementSQL <> '' then       LWherePart := 'WHERE '+
                                                 'MON$SQL_TEXT = ' + ALQuotedStr(StatementSQL) +
                                                 ALIfThen(TransactionID <> -1, ' AND MON$TRANSACTION_ID = '+ALIntToStr(TransactionID)) +
                                                 ALIfThen(ConnectionID <> -1,  ' AND MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID))
  else if TransactionID <> -1 then LWherePart := 'WHERE '+
                                                 'MON$TRANSACTION_ID = '+ALIntToStr(TransactionID) +
                                                 ALIfThen(ConnectionID <> -1,  ' AND MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID))
  else if ConnectionID <> -1 then  LWherePart := 'WHERE '+
                                                 'MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID)
  else                             LWherePart := '';


  //Save the old data
  L_saved_TraHandle := fTraHandle;
  L_saved_StmtHandle := fStmtHandle;
  L_saved_Sqlda := fSqlda;
  L_saved_StmtSQL := fStmtSQL;
  Try

    //clear the handle
    fTraHandle := nil;
    fStmtHandle := nil;
    fSqlda := nil;
    fStmtSQL := '';

    //start the TMP transaction
    TransactionStart(DefaultReadTPB);
    Try

      //get the data from the monitoring table
      LXMLDATA := TALXmlDocument.create('root');
      try

        SelectData('SELECT '+
                     LSelectPart +
                   'FROM ' +
                     LFromPart + ' ' +
                   LJoinPart +
                   LWherePart,
                   'rec',
                   LXMLDATA.DocumentElement,
                   ALDefaultFormatSettings);

        if LXMLDATA.DocumentElement.ChildNodes.Count <> 1 then raise Exception.Create('Can not get the monitoring stats');
        with LXMLDATA.DocumentElement.ChildNodes[0] do begin

          if not SkipIOStats then begin
            with IOStats do begin
              page_reads := ALStrToInt64(ChildNodes['page_reads'].text);
              page_writes := ALStrToInt64(ChildNodes['page_writes'].text);
              page_fetches := ALStrToInt64(ChildNodes['page_fetches'].text);
              page_marks := ALStrToInt64(ChildNodes['page_marks'].text);
            end;
          end;

          if not SkipRecordStats then begin
            with RecordStats do begin
              record_seq_reads := ALStrToInt64(ChildNodes['record_seq_reads'].text);
              record_idx_reads := ALStrToInt64(ChildNodes['record_idx_reads'].text);
              record_inserts := ALStrToInt64(ChildNodes['record_inserts'].text);
              record_updates := ALStrToInt64(ChildNodes['record_updates'].text);
              record_deletes := ALStrToInt64(ChildNodes['record_deletes'].text);
              record_backouts := ALStrToInt64(ChildNodes['record_backouts'].text);
              record_purges := ALStrToInt64(ChildNodes['record_purges'].text);
              record_expunges := ALStrToInt64(ChildNodes['record_expunges'].text);
            end;
          end;

          if not SkipMemoryUsage then begin
            with MemoryUsage do begin
              memory_used := ALStrToInt64(ChildNodes['memory_used'].text);
              memory_allocated := ALStrToInt64(ChildNodes['memory_allocated'].text);
              max_memory_used := ALStrToInt64(ChildNodes['max_memory_used'].text);
              max_memory_allocated := ALStrToInt64(ChildNodes['max_memory_allocated'].text);
            end;
          end;

        end;

      finally
        LXMLDATA.free;
      end;

      //commit the TMP transaction
      TransactionCommit;

    Except

      //roolback the TMP transaction
      TransactionRollBack;
      Raise;

    End;

  Finally

    //get back the original transaction data
    fTraHandle := L_saved_TraHandle;
    fStmtHandle := L_saved_StmtHandle;
    fSqlda := L_saved_Sqlda;
    fStmtSQL := L_saved_StmtSQL;

  End;

end;

{*********************************************************}
function TALFBXClient.loadCachedData(const Key: AnsiString;
                                     var DataStr: AnsiString): Boolean;
begin
  result := false; //virtual need to be overriden
end;

{***********************************************************}
Procedure TALFBXClient.SaveDataToCache(const Key: ansiString;
                                       const CacheThreshold: integer;
                                       const DataStr: ansiString);
begin
  //virtual need to be overriden
end;

{*********************************************************}
function TALFBXClient.GetFieldValue(aSQLDA:TALFBXSQLResult;
                                    aDBHandle: IscDbHandle;
                                    aTraHandle: IscTrHandle;
                                    aIndex: Integer;
                                    const aFormatSettings: TALFormatSettings): AnsiString;
  {-------------------------}
  Procedure InternalReadBlob;
  var BlobHandle: IscBlobHandle;
  begin
    with FLibrary do begin
      BlobHandle := nil;
      BlobOpen(aDBHandle, aTraHandle, BlobHandle, aSQLDA.AsQuad[aIndex]);
      try
        Result := BlobReadString(BlobHandle);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;

Begin
  If not aSQLDA.IsNull[aIndex] then
    Case aSQLDA.SQLType[aIndex] of
      SQL_TIMESTAMP : Result := ALDatetimetostr(aSQLDA.AsDateTime[aIndex], aFormatSettings);
      SQL_TYPE_TIME : Result := ALTimetostr(aSQLDA.AsTime[aIndex], aFormatSettings);
      SQL_TYPE_DATE : Result := ALDatetostr(aSQLDA.AsDate[aIndex], aFormatSettings);
      SQL_DOUBLE    : Result := ALFloatToStr(aSQLDA.AsDouble[aIndex], aFormatSettings);
      SQL_FLOAT,
      SQL_D_FLOAT   : Result := ALFloatToStr(aSQLDA.AsSingle[aIndex], aFormatSettings);
      SQL_INT64,
      SQL_LONG,
      SQL_SHORT     : begin
                        if aSQLDA.SQLScale[aIndex] < 0 then Result := ALFloatToStr(aSQLDA.asDouble[Aindex],aFormatSettings)
                        else result := aSQLDA.AsAnsiString[Aindex];
                      end;
      SQL_BLOB      : InternalReadBlob;
      else Result := aSQLDA.AsAnsiString[Aindex];
    end
    else result := fNullString;
end;

{********************************}
procedure TALFBXClient.initObject;
begin
  fDefaultReadTPB := cALFbxClientDefaultReadNOWaitTPB;
  fDefaultWriteTPB := cALFbxClientDefaultWriteNOWaitTPB;
  fDBHandle := nil;
  fTraHandle := nil;
  fStmtHandle := nil;
  fSqlda := nil;
  fStmtSQL := '';
  FSQLDIALECT := 3;
  fCharSet := csnone;
  fNullString := '';
end;

{*******************************************************************************************}
constructor TALFBXClient.Create(ApiVer: TALFBXVersion_API; const lib: AnsiString = GDS32DLL);
begin
  FLibrary := TALFBXLibrary.Create(ApiVer);
  fLibrary.Load(lib);
  FownLibrary := True;
  initObject;
end;

{**************************************************}
constructor TALFBXClient.Create(lib: TALFBXLibrary);
begin
  FLibrary := lib;
  FownLibrary := False;
  initObject;
end;

{******************************}
destructor TALFBXClient.Destroy;
begin
  if Connected then disconnect;
  if FownLibrary then FLibrary.Free;
  inherited;
end;

{**********************************************************************}
function  TALFBXClient.GetDataBaseInfoInt(const item: Integer): Integer;
begin
  If not connected then raise Exception.Create('Not connected');
  case item of
    isc_info_implementation,
    isc_info_base_level:
    result := byte(FLibrary.DatabaseInfoString(FDbHandle, item, 8)[5]);
    else result := FLibrary.DatabaseInfoIntValue(FDbHandle, AnsiChar(item));
  end;
end;

{****************************************************************************}
function  TALFBXClient.GetDataBaseInfoString(const item: Integer): AnsiString;
var LSize: byte;
    LData: AnsiString;
begin
  If not connected then raise Exception.Create('Not connected');
  LData := FLibrary.DatabaseInfoString(FDbHandle, item, 256);
  case Item of
    isc_info_cur_logfile_name,
    isc_info_wal_prv_ckpt_fname: begin
                                   LSize := byte(LData[4]);
                                   ALMove(LData[5], LData[1], LSize);
                                   SetLength(LData, LSize);
                                 end;
    else begin
      LSize := byte(LData[5]);
      ALMove(LData[6], LData[1], LSize);
      SetLength(LData, LSize);
    end;
  end;
  Result := AnsiString(LData);
end;

{*****************************************************************************}
function  TALFBXClient.GetDataBaseInfoDateTime(const item: Integer): TDateTime;
begin
  If not connected then raise Exception.Create('Not connected');
  result := FLibrary.DatabaseInfoDateTime(FDbHandle, item);
end;

{*********************************************************}
procedure TALFBXClient.GetUserNames(UserNames: TALStrings);
var data: AnsiString;
    p: PAnsiChar;
    len: integer;
begin
  If not connected then raise Exception.Create('Not connected');
  data := FLibrary.DatabaseInfoString(FDbHandle, isc_info_user_names, 256);
  p := PAnsiChar(data);
  while byte(p^) = isc_info_user_names do begin
    len := byte(p[3]);
    inc(p, 4);
    UserNames.Add(ALCopyStr(p, 0, len));
    inc(p, len);
  end;
end;

{***********************************************************}
procedure TALFBXClient.CreateDatabase(const SQL: AnsiString);
begin
  if connected then raise Exception.Create('You must disconnect first');

  Try

    Flibrary.DSQLExecuteImmediate(fDBHandle, fTraHandle, SQL, FSQLDIALECT, nil);
    try
      Flibrary.DetachDatabase(fDBHandle);
    Except
      //what else we can do here in case of an exception ?
    end;

  finally
    fDBHandle := nil;
    fTraHandle := nil;
  End;

end;

{**********************************}
procedure TALFBXClient.DropDatabase;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');
  if InTransaction then raise Exception.Create('Transaction is active');

  //drop the database
  FLibrary.DatabaseDrop(FDbHandle);

  //close the connection
  fDBHandle := nil;

end;

{************************************************}
procedure TALFBXClient.Connect(const DataBaseName,
                                     Login,
                                     Password,
                                     CharSet: AnsiString;
                               const ExtraParams: AnsiString = '');
Var LParams: AnsiString;
begin
  if connected then raise Exception.Create('Already connected');
  fCharSet :=  ALFBXStrToCharacterSet(CharSet);
  LParams := 'user_name = '+Login+'; '+
             'password = '+Password+'; '+
             'lc_ctype = '+CharSet;
  if ExtraParams <> '' then LParams := LParams + '; ' + ExtraParams;
  Try
    FLibrary.AttachDatabase(DataBaseName,
                            fDBHandle,
                            LParams);
  Except
    fDBHandle := nil;
    raise;
  End;
end;

{************************************************}
procedure TALFBXClient.connect(const DataBaseName,
                                     Login,
                                     Password,
                                     CharSet: AnsiString;
                               Numbuffers: integer);
begin
  Connect(DataBaseName,
          Login,
          Password,
          CharSet,
          'num_buffers = '+ALIntToStr(Numbuffers));
end;

{********************************}
procedure TALFBXClient.Disconnect;
begin
  If not connected then exit;
  if InTransaction then TransactionRollback;
  Try
    Flibrary.DetachDatabase(fDBHandle);
  Except
    //ok some exception are raised here when the connection is for exemple
    //lost or when the Firebird server was shuntdown
    //but i hope the resource of the DBHandle are always free
    //if not i don't know a way to free it !
  End;
  fDBHandle := nil;
end;

{*************************************************************}
procedure TALFBXClient.TransactionStart(const TPB: AnsiString);
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');
  if InTransaction then raise Exception.Create('transaction is already active');

  //Start the transaction
  Try
    Flibrary.TransactionStart(fTraHandle,
                              fDBHandle,
                              TPB);
  Except
    fTraHandle := nil;
    raise;
  End;

end;

{***************************************}
procedure TALFBXClient.TransactionCommit;
begin

  //Error if we are not connected
  if not InTransaction then raise Exception.Create('No active transaction to commit');

  //free old statement
  if assigned(fStmtHandle) then Flibrary.DSQLFreeStatement(fStmtHandle, DSQL_drop);
  if assigned(fSqlda) then fSqlda.free;
  fStmtHandle := nil;
  fSqlda := nil;
  fStmtSQL := '';

  //commit the transaction
  Flibrary.TransactionCommit(fTraHandle);
  fTraHandle := nil;

end;

{************************************************}
procedure TALFBXClient.TransactionCommitRetaining;
begin

  //Error if we are not connected
  if not InTransaction then raise Exception.Create('No active transaction to commit');

  //commit the transaction
  Flibrary.TransactionCommitRetaining(fTraHandle);

end;

{*****************************************}
procedure TALFBXClient.TransactionRollback;
begin

  //Error if we are not connected
  if not InTransaction then raise Exception.Create('No active transaction to rollback');

  //free old statement
  Try
    if assigned(fStmtHandle) then Flibrary.DSQLFreeStatement(fStmtHandle, DSQL_drop);
  Except
    //what else we can do ?
  End;
  if assigned(fSqlda) then fSqlda.free;
  fStmtHandle := nil;
  fSqlda := nil;
  fStmtSQL := '';

  //rollback the transaction
  Try
    Flibrary.TransactionRollback(fTraHandle);
  Except
    //some error can happen if the network go down for exemple
    //i don't really know if in this case of error the fTRAHandle will be very
    //free or not ... but what else we can do ? commit => exept => rollback => except ???
  End;
  fTraHandle := nil;

end;

{**************************************************}
procedure TALFBXClient.TransactionRollbackRetaining;
begin

  //Error if we are not inTransaction
  if not InTransaction then raise Exception.Create('No active transaction to rollback');

  //rollback the transaction
  Flibrary.TransactionRollbackRetaining(fTraHandle);

end;

{************************************************************************}
Function TALFBXClient.Prepare(const SQL: AnsiString): TALFBXStatementType;
begin

  //Error if we are not inTransaction
  if not InTransaction then raise Exception.Create('No active transaction');

  //free old statement
  if assigned(fStmtHandle) then Flibrary.DSQLFreeStatement(fStmtHandle, DSQL_drop);
  if assigned(fSqlda) then fSqlda.free;
  fStmtHandle := nil;
  fSqlda := nil;
  fStmtSQL := '';

  //create the sqlda result
  fSqlda := TALFBXSQLResult.Create(fCharSet);
  Try

    //init the aStmtHandle
    Flibrary.DSQLAllocateStatement(fDBHandle, fStmtHandle);
    try

      //prepare the SQL
      Result := Flibrary.DSQLPrepare(fDBHandle, fTraHandle, fStmtHandle, SQL, FSQLDIALECT, fSqlda);

      //init fStmtSQL
      fStmtSQL := SQL;

    except

      try
        Flibrary.DSQLFreeStatement(fStmtHandle, DSQL_drop);
      Except
        //to not hide the original exception
      end;

      raise;

    end;

  Except
    fStmtHandle := nil;
    fSqlda.free;
    fSqlda := nil;
    fStmtSQL := '';
    raise;
  end;

end;

{*******************************************************************************}
procedure TALFBXClient.OnSelectDataDone(const Query: TALFBXClientSelectDataQUERY;
                                        TimeTaken: double);
begin
  // virtual
end;

{*******************************************************************************}
procedure TALFBXClient.OnUpdateDataDone(const Query: TALFBXClientUpdateDataQUERY;
                                        TimeTaken: double);
begin
  // virtual
end;

{*****************************************************************************}
procedure TALFBXClient.SelectData(const Queries: TALFBXClientSelectDataQUERIES;
                                  XMLDATA: TalXMLNode;
                                  OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                                  ExtData: Pointer;
                                  const FormatSettings: TALFormatSettings);

Var LSqlpa: TALFBXSQLParams;
    LParamsIndex: integer;
    LBlobhandle: IscBlobHandle;
    LQueriesIndex: integer;
    LViewRec: TalXmlNode;
    LDropStmt: Boolean;
    LXmlDocument: TalXmlDocument;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;

    {------------------------------------}
    Procedure InternalSelectDataFetchRows;
    var LColumnIndex: integer;
        LRecIndex: integer;
        LRecAdded: integer;
        LContinue: Boolean;
        LNewRec: TalXmlNode;
        LValueRec: TalXmlNode;
        LUpdateRowTagByFieldValue: Boolean;
    Begin

      //init the aViewRec
      if (Queries[LQueriesIndex].ViewTag <> '') and (not assigned(LXmlDocument)) then LViewRec := XMLdata.AddChild(Queries[LQueriesIndex].ViewTag)
      else LViewRec := XMLdata;

      //init aUpdateRowTagByFieldValue
      if AlPos('&>',Queries[LQueriesIndex].RowTag) = 1 then begin
        delete(Queries[LQueriesIndex].RowTag, 1, 2);
        LUpdateRowTagByFieldValue := Queries[LQueriesIndex].RowTag <> '';
      end
      else LUpdateRowTagByFieldValue := False;

      //retrieve all row
      LRecIndex := 0;
      LRecAdded := 0;
      while Flibrary.DSQLFetch(fDBHandle, fTraHandle, fStmtHandle, FSQLDIALECT, fsqlda) do begin

        //process if > Skip
        inc(LRecIndex);
        If LRecIndex > Queries[LQueriesIndex].Skip then begin

          //init NewRec
          if (Queries[LQueriesIndex].RowTag <> '') and (not assigned(LXmlDocument)) then LNewRec := LViewRec.AddChild(Queries[LQueriesIndex].RowTag)
          Else LNewRec := LViewRec;

          //loop throught all column
          For LColumnIndex := 0 to fsqlda.FieldCount - 1 do begin
            LValueRec := LNewRec.AddChild(ALlowercase(fsqlda.AliasName[LColumnIndex]));
            if (fSQLDA.SQLType[LColumnIndex] = SQL_BLOB) then LValueRec.ChildNodes.Add(
                                                                                       LValueRec.OwnerDocument.CreateNode(
                                                                                                                          GetFieldValue(fsqlda,
                                                                                                                                        fDBHandle,
                                                                                                                                        fTRAHandle,
                                                                                                                                        LColumnIndex,
                                                                                                                                        FormatSettings),
                                                                                                                          ntCData
                                                                                                                         )
                                                                                       )
            else LValueRec.Text := GetFieldValue(fsqlda,
                                                 fDBHandle,
                                                 fTRAHandle,
                                                 LColumnIndex,
                                                 FormatSettings);
            if LUpdateRowTagByFieldValue and (LValueRec.NodeName=LNewRec.NodeName) then LNewRec.NodeName := ALLowerCase(LValueRec.Text);
          end;

          //handle OnNewRowFunct
          if assigned(OnNewRowFunct) then begin
            LContinue := True;
            OnNewRowFunct(LNewRec, Queries[LQueriesIndex].ViewTag, ExtData, LContinue);
            if Not LContinue then Break;
          end;

          //free the node if aXmlDocument
          if assigned(LXmlDocument) then LXmlDocument.DocumentElement.ChildNodes.Clear;

          //handle the First
          inc(LRecAdded);
          If (Queries[LQueriesIndex].First > 0) and (LRecAdded >= Queries[LQueriesIndex].First) then Break;

        end;

      end;

    end;

begin

  //exit if no SQL
  if length(Queries) = 0 then Exit;

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

  Try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //loop on all the SQL
    For LQueriesIndex := 0 to length(Queries) - 1 do begin

      //Handle the CacheThreshold
      LCacheKey := '';
      If (Queries[LQueriesIndex].CacheThreshold > 0) and
         (not assigned(LXmlDocument)) and
         (((length(Queries) = 1) and
           (XMLdata.ChildNodes.Count = 0)) or  // else the save will not work
          (Queries[LQueriesIndex].ViewTag <> '')) then begin

        //try to load from from cache
        LCacheKey := ALStringHashSHA1(Queries[LQueriesIndex].RowTag + '#' +
                                      alinttostr(Queries[LQueriesIndex].Skip) + '#' +
                                      alinttostr(Queries[LQueriesIndex].First) + '#' +
                                      ALGetFormatSettingsID(FormatSettings) + '#' +
                                      Queries[LQueriesIndex].SQL);
        if loadcachedData(LCacheKey, LCacheStr) then begin

          //init the aViewRec
          if (Queries[LQueriesIndex].ViewTag <> '') then LViewRec := XMLdata.AddChild(Queries[LQueriesIndex].ViewTag)
          else LViewRec := XMLdata;

          //assign the tmp data to the XMLData
          LViewRec.LoadFromXML(LCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

          //go to the next loop
          continue;

        end;

      end;

      //start the TstopWatch
      LStopWatch.Reset;
      LStopWatch.Start;

      //prepare if neccessary
      if (not assigned(fSqlda)) or
         (not assigned(fStmtHandle)) or
         (Queries[LQueriesIndex].SQL <> fStmtSQL) then begin
        Prepare(Queries[LQueriesIndex].SQL);
        LDropStmt := True;
      end
      else LDropStmt := False;

      try

        //if their is params
        if length(Queries[LQueriesIndex].Params) > 0 then begin

          //create the aSqlpa object
          LSqlpa := TALFBXSQLParams.Create(fCharSet);

          try

            //loop throught all Params Fields
            for LParamsIndex := 0 to length(Queries[LQueriesIndex].Params) - 1 do begin

              //with current Params Fields
              with Queries[LQueriesIndex].Params[LParamsIndex] do begin

                //isnull
                if IsNull then begin
                  LSqlpa.AddFieldType('', uftVarchar);
                  LSqlpa.IsNull[LParamsIndex] := True;
                end

                //IsBlob
                else if length(value) > high(smallint) then begin
                  LSqlpa.AddFieldType('', uftBlob);
                  LBlobhandle := nil;
                  LSqlpa.AsQuad[LParamsIndex] := Flibrary.BlobCreate(fDBHandle,fTraHandle,LBlobhandle);
                  Try
                    FLibrary.BlobWriteString(LBlobhandle,Value);
                  Finally
                    FLibrary.BlobClose(LBlobhandle);
                  End;
                end

                //all the other
                else begin
                  LSqlpa.AddFieldType('', uftVarchar);
                  LSqlpa.AsAnsiString[LParamsIndex] := Value;
                end;

              end;

            end;

            //execute the sql with the params
            FLibrary.DSQLExecute(fTraHandle, fStmtHandle, FSQLDIALECT, LSqlpa);

            //fetch the rows
            InternalSelectDataFetchRows;

          finally
            LSqlpa.free;
          end;

        end

        //if their is NO params
        else begin

          //execute the SQL wihtout params
          FLibrary.DSQLExecute(fTraHandle, fStmtHandle, FSQLDIALECT, nil);

          //fetch the rows
          InternalSelectDataFetchRows;

        end;

      finally

        //free the statement
        if LDropStmt then begin

          try
            Flibrary.DSQLFreeStatement(fStmtHandle, DSQL_drop);
          Except
            //what else we can do here ?
            //this can happen if connection lost for exemple
            //i preferre to hide this exception to not hide previous exception
          end;
          fSqlda.free;
          fStmtHandle := nil;
          fSqlda := nil;
          fStmtSQL := '';

        end

        //else simply close the cursor
        else begin

          try
            Flibrary.DSQLFreeStatement(fStmtHandle, DSQL_close);
          Except
            //what else we can do here ?
            //this can happen if connection lost for exemple
            //i preferre to hide this exception to not hide previous exception
          end;

        end;

      end;

      //do the OnSelectDataDone
      LStopWatch.Stop;
      OnSelectDataDone(Queries[LQueriesIndex],
                       LStopWatch.Elapsed.TotalMilliseconds);

      //save to the cache
      If LCacheKey <> '' then begin

        //save the data
        LViewRec.SaveToXML(LCacheStr, true{SaveOnlyChildNodes});
        SaveDataToCache(LCacheKey,
                        Queries[LQueriesIndex].CacheThreshold,
                        LCacheStr);

      end;

    End;

  Finally
    if assigned(LXmlDocument) then LXmlDocument.free;
  End;

end;

{*************************************************************************}
procedure TALFBXClient.SelectData(const Query: TALFBXClientSelectDataQUERY;
                                  OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                                  ExtData: Pointer;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := Query;
  SelectData(LSelectDataQUERIES,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{******************************************************}
procedure TALFBXClient.SelectData(const SQL: AnsiString;
                                  Skip: Integer;
                                  First: Integer;
                                  OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                                  ExtData: Pointer;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  LSelectDataQUERIES[0].skip := Skip;
  LSelectDataQUERIES[0].First := First;
  SelectData(LSelectDataQUERIES,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{******************************************************}
procedure TALFBXClient.SelectData(const SQL: AnsiString;
                                  OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                                  ExtData: Pointer;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  SelectData(LSelectDataQUERIES,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{*****************************************************************************}
procedure TALFBXClient.SelectData(const Queries: TALFBXClientSelectDataQUERIES;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TALFormatSettings);
begin

  SelectData(Queries,
             XMLDATA,
             nil,
             nil,
             FormatSettings);

end;

{*************************************************************************}
procedure TALFBXClient.SelectData(const Query: TALFBXClientSelectDataQUERY;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := Query;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{******************************************************}
procedure TALFBXClient.SelectData(const SQL: AnsiString;
                                  const RowTag: AnsiString;
                                  Skip: Integer;
                                  First: Integer;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  LSelectDataQUERIES[0].RowTag := RowTag;
  LSelectDataQUERIES[0].skip := Skip;
  LSelectDataQUERIES[0].First := First;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{******************************************************}
procedure TALFBXClient.SelectData(const SQL: AnsiString;
                                  const RowTag: AnsiString;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  LSelectDataQUERIES[0].RowTag := RowTag;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{******************************************************}
procedure TALFBXClient.SelectData(const SQL: AnsiString;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{******************************************************}
procedure TALFBXClient.SelectData(const SQL: AnsiString;
                                  const Params: Array of AnsiString;
                                  const RowTag: AnsiString;
                                  Skip: Integer;
                                  First: Integer;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
    I: integer;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  setlength(LSelectDataQUERIES[0].params, length(Params));
  for I := 0 to length(Params) - 1 do begin
    LSelectDataQUERIES[0].params[I] := TALFBXClientSQLParam.Create;
    LSelectDataQUERIES[0].params[I].Value := Params[I];
    LSelectDataQUERIES[0].params[I].IsNull := Params[I] = fNullString;
  end;
  LSelectDataQUERIES[0].RowTag := RowTag;
  LSelectDataQUERIES[0].skip := Skip;
  LSelectDataQUERIES[0].First := First;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{******************************************************}
procedure TALFBXClient.SelectData(const SQL: AnsiString;
                                  const Params: Array of AnsiString;
                                  const RowTag: AnsiString;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
    I: integer;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  setlength(LSelectDataQUERIES[0].params, length(Params));
  for I := 0 to length(Params) - 1 do begin
    LSelectDataQUERIES[0].params[I] := TALFBXClientSQLParam.Create;
    LSelectDataQUERIES[0].params[I].Value := Params[I];
    LSelectDataQUERIES[0].params[I].IsNull := Params[I] = fNullString;
  end;
  LSelectDataQUERIES[0].RowTag := RowTag;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{******************************************************}
procedure TALFBXClient.SelectData(const SQL: AnsiString;
                                  const Params: Array of AnsiString;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TALFormatSettings);
var LSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
    I: integer;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  setlength(LSelectDataQUERIES[0].params, length(Params));
  for I := 0 to length(Params) - 1 do begin
    LSelectDataQUERIES[0].params[I] := TALFBXClientSQLParam.Create;
    LSelectDataQUERIES[0].params[I].Value := Params[I];
    LSelectDataQUERIES[0].params[I].IsNull := Params[I] = fNullString;
  end;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{******************************************************************************}
procedure TALFBXClient.UpdateData(const Queries: TALFBXClientUpdateDataQUERIES);
Var LSqlpa: TALFBXSQLParams;
    LBlobhandle: IscBlobHandle;
    LParamsIndex: integer;
    LQueriesIndex: integer;
    LDropStmt: Boolean;
    LStopWatch: TStopWatch;
begin

  //exit if no SQL
  if length(Queries) = 0 then Exit;

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  LStopWatch := TstopWatch.Create;

  {loop on all the SQL}
  For LQueriesIndex := 0 to length(Queries) - 1 do begin

    //start the TstopWatch
    LStopWatch.Reset;
    LStopWatch.Start;

    //if their is params
    if length(Queries[LQueriesIndex].Params) > 0 then begin

      //prepare if neccessary
      if (not assigned(fSqlda)) or
         (not assigned(fStmtHandle)) or
         (Queries[LQueriesIndex].SQL <> fStmtSQL) then begin
        Prepare(Queries[LQueriesIndex].SQL);
        LDropStmt := True;
      end
      else LDropStmt := False;

      try

        //create the aSqlpa object
        LSqlpa := TALFBXSQLParams.Create(fCharSet);
        try

          //loop throught all Params Fields
          for LParamsIndex := 0 to length(Queries[LQueriesIndex].Params) - 1 do begin

            //with current Params Fields
            with Queries[LQueriesIndex].Params[LParamsIndex] do begin

              //isnull
              if IsNull then begin
                LSqlpa.AddFieldType('', uftVarchar);
                LSqlpa.IsNull[LParamsIndex] := True;
              end

              //IsBlob
              else if length(value) > high(smallint) then begin
                LSqlpa.AddFieldType('', uftBlob);
                LBlobhandle := nil;
                LSqlpa.AsQuad[LParamsIndex] := Flibrary.BlobCreate(fDBHandle,fTraHandle,LBlobhandle);
                Try
                  FLibrary.BlobWriteString(LBlobhandle,Value);
                Finally
                  FLibrary.BlobClose(LBlobhandle);
                End;
              end

              //all the other
              else begin
                LSqlpa.AddFieldType('', uftVarchar);
                LSqlpa.AsAnsiString[LParamsIndex] := Value;
              end;

            end;

          end;

          //execute the SQL
          FLibrary.DSQLExecute(fTraHandle, fStmtHandle, FSQLDIALECT, LSqlpa);

        finally
          LSqlpa.free;
        end;

      finally

        //free the statement
        if LDropStmt then begin

          try
            Flibrary.DSQLFreeStatement(fStmtHandle, DSQL_drop);
          Except
            //what else we can do here ?
            //this can happen if connection lost for exemple
            //i preferre to hide this exception to not hide previous exception
          end;
          fSqlda.free;
          fStmtHandle := nil;
          fSqlda := nil;
          fStmtSQL := '';

        end;

      end;

    end

    //if their is NO params
    else begin

      //in case the query was not prepared
      if (not assigned(fSqlda)) or
         (not assigned(fStmtHandle)) or
         (Queries[LQueriesIndex].SQL <> fStmtSQL) then begin

        Flibrary.DSQLExecuteImmediate(fDBHandle, fTraHandle, Queries[LQueriesIndex].SQL, FSQLDIALECT, nil);

      end

      //in case the query was prepared
      else begin

        //execute the SQL
        FLibrary.DSQLExecute(fTraHandle, fStmtHandle, FSQLDIALECT, nil);

      end;

    end;

    //do the OnUpdateDataDone
    LStopWatch.Stop;
    OnUpdateDataDone(Queries[LQueriesIndex],
                     LStopWatch.Elapsed.TotalMilliseconds);

  end;

end;

{**************************************************************************}
procedure TALFBXClient.UpdateData(const Query: TALFBXClientUpdateDataQUERY);
var LUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
begin
  setlength(LUpdateDataQUERIES,1);
  LUpdateDataQUERIES[0] := Query;
  UpdateData(LUpdateDataQUERIES);
end;

{**************************************************}
procedure TALFBXClient.UpdateData(SQLs: TALStrings);
Var LSQLsIndex : integer;
    LUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
begin
  setlength(LUpdateDataQUERIES,SQLs.Count);
  For LSQLsIndex := 0 to SQLs.Count - 1 do begin
    LUpdateDataQUERIES[LSQLsIndex] := TALFBXClientUpdateDataQUERY.Create;
    LUpdateDataQUERIES[LSQLsIndex].SQL := SQLs[LSQLsIndex];
  end;
  UpdateData(LUpdateDataQUERIES);
end;

{*******************************************************}
procedure TALFBXClient.UpdateData(const SQL: AnsiString);
Var LUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
begin
  setlength(LUpdateDataQUERIES,1);
  LUpdateDataQUERIES[0] := TALFBXClientUpdateDataQUERY.Create;
  LUpdateDataQUERIES[0].SQL := SQL;
  UpdateData(LUpdateDataQUERIES);
end;

{******************************************************}
procedure TALFBXClient.UpdateData(const SQL: AnsiString;
                                  const Params: Array of AnsiString);
Var LUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
    i: integer;
begin
  setlength(LUpdateDataQUERIES,1);
  LUpdateDataQUERIES[0] := TALFBXClientUpdateDataQUERY.Create;
  LUpdateDataQUERIES[0].SQL := SQL;
  setlength(LUpdateDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    LUpdateDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    LUpdateDataQUERIES[0].params[i].Value := Params[i];
    LUpdateDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  UpdateData(LUpdateDataQUERIES);
end;

{*****************************************************************}
procedure TALFBXClient.UpdateData(const SQLs: array of AnsiString);
Var LUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
    i: integer;
begin
  setlength(LUpdateDataQUERIES,length(SQLs));
  for I := 0 to length(SQLs) - 1 do begin
    LUpdateDataQUERIES[i] := TALFBXClientUpdateDataQUERY.Create;
    LUpdateDataQUERIES[i].SQL := SQLs[i];
  end;
  UpdateData(LUpdateDataQUERIES);
end;

{*******************************************************************}
procedure TALFBXConnectionPoolClient.GetMonitoringInfos(ConnectionID,
                                                        TransactionID: int64;
                                                        const StatementSQL: AnsiString;
                                                        Var IOStats: TALFBXClientMonitoringIOStats;
                                                        Var RecordStats: TALFBXClientMonitoringRecordStats;
                                                        Var MemoryUsage: TALFBXClientMonitoringMemoryUsage;
                                                        Const SkipIOStats: Boolean = False;
                                                        Const SkipRecordStats: Boolean = False;
                                                        Const SkipMemoryUsage: Boolean = False);

Var LXMLDATA: TalXmlDocument;
    LSelectPart: AnsiString;
    LFromPart: AnsiString;
    LJoinPart: AnsiString;
    LWherePart: AnsiString;

    LDBHandle: IscDbHandle;
    LTraHandle: IscTrHandle;

Begin

  //The key term of the monitoring feature is an activity snapshot. It represents the current state of the database, comprising a
  //variety of information about the database itself, active attachments and users, transactions, prepared and running statements, and more.
  //
  //A snapshot is created the first time any of the monitoring tables is being selected from in the given transaction and it is
  //preserved until the transaction ends, in order that multiple-table queries (e.g., master-detail ones) will always return a consistent view of the data.
  //
  //In other words, the monitoring tables always behave like a snapshot table stability ("consistency") transaction, even if the host transaction
  //has been started with a lower isolation level.
  //
  //To refresh the snapshot, the current transaction should be finished and the monitoring tables should be queried in a new transaction context.

  //build the aSelectPart;
  LSelectPart := ALIfThen(not SkipIOStats,     'MON$PAGE_READS as PAGE_READS, '+
                                               'MON$PAGE_WRITES as PAGE_WRITES, '+
                                               'MON$PAGE_FETCHES as PAGE_FETCHES, '+
                                               'MON$PAGE_MARKS as PAGE_MARKS, ') +
                 ALIfThen(not SkipRecordStats, 'MON$RECORD_SEQ_READS as RECORD_SEQ_READS, '+
                                               'MON$RECORD_IDX_READS as RECORD_IDX_READS, '+
                                               'MON$RECORD_INSERTS as RECORD_INSERTS, '+
                                               'MON$RECORD_UPDATES as RECORD_UPDATES, '+
                                               'MON$RECORD_DELETES as RECORD_DELETES, '+
                                               'MON$RECORD_BACKOUTS as RECORD_BACKOUTS, '+
                                               'MON$RECORD_PURGES as RECORD_PURGES, '+
                                               'MON$RECORD_EXPUNGES as RECORD_EXPUNGES, ') +
                 ALIfThen(not SkipMemoryUsage, 'MON$MEMORY_USED as MEMORY_USED, '+
                                               'MON$MEMORY_ALLOCATED as MEMORY_ALLOCATED, '+
                                               'MON$MAX_MEMORY_USED as MAX_MEMORY_USED, '+
                                               'MON$MAX_MEMORY_ALLOCATED as MAX_MEMORY_ALLOCATED, ');
  if LSelectPart <> '' then delete(LSelectPart,length(LSelectPart) - 1, 1)
  else Exit;

  //build the aFromPart
  if StatementSQL <> '' then       LFromPart := 'MON$STATEMENTS'
  else if TransactionID <> -1 then LFromPart := 'MON$TRANSACTIONS'
  else if ConnectionID <> -1 then  LFromPart := 'MON$ATTACHMENTS'
  else                             LFromPart := 'MON$DATABASE';

  //build the aJoinPart
  LJoinPart := ALIfThen(not SkipIOStats,     'JOIN MON$IO_STATS ON MON$IO_STATS.MON$STAT_ID = '        +LFromPart+'.MON$STAT_ID ') +
               ALIfThen(not SkipRecordStats, 'JOIN MON$RECORD_STATS ON MON$RECORD_STATS.MON$STAT_ID = '+LFromPart+'.MON$STAT_ID ') +
               ALIfThen(not SkipMemoryUsage, 'JOIN MON$MEMORY_USAGE ON MON$MEMORY_USAGE.MON$STAT_ID = '+LFromPart+'.MON$STAT_ID ');

  //build the aWherePart
  if StatementSQL <> '' then       LWherePart := 'WHERE '+
                                                 'MON$SQL_TEXT = ' + ALQuotedStr(StatementSQL) +
                                                 ALIfThen(TransactionID <> -1, ' AND MON$TRANSACTION_ID = '+ALIntToStr(TransactionID)) +
                                                 ALIfThen(ConnectionID <> -1,  ' AND MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID))
  else if TransactionID <> -1 then LWherePart := 'WHERE '+
                                                 'MON$TRANSACTION_ID = '+ALIntToStr(TransactionID) +
                                                 ALIfThen(ConnectionID <> -1,  ' AND MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID))
  else if ConnectionID <> -1 then  LWherePart := 'WHERE '+
                                                 'MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID)
  else                             LWherePart := '';


  //clear the handle
  LDBHandle := nil;
  LTraHandle := nil;

  //start the TMP transaction
  TransactionStart(LDBHandle,
                   LTraHandle,
                   DefaultReadTPB);
  Try

    //get the data from the monitoring table
    LXMLDATA := TALXmlDocument.create('root');
    try

      SelectData('SELECT '+
                   LSelectPart +
                 'FROM ' +
                   LFromPart + ' ' +
                 LJoinPart +
                 LWherePart,
                 'rec',
                 LXMLDATA.DocumentElement,
                 ALDefaultFormatSettings,
                 LDBHandle,
                 LTraHandle);

      if LXMLDATA.DocumentElement.ChildNodes.Count <> 1 then raise Exception.Create('Can not get the monitoring stats');
      with LXMLDATA.DocumentElement.ChildNodes[0] do begin

        if not SkipIOStats then begin
          with IOStats do begin
            page_reads := ALStrToInt64(ChildNodes['page_reads'].text);
            page_writes := ALStrToInt64(ChildNodes['page_writes'].text);
            page_fetches := ALStrToInt64(ChildNodes['page_fetches'].text);
            page_marks := ALStrToInt64(ChildNodes['page_marks'].text);
          end;
        end;

        if not SkipRecordStats then begin
          with RecordStats do begin
            record_seq_reads := ALStrToInt64(ChildNodes['record_seq_reads'].text);
            record_idx_reads := ALStrToInt64(ChildNodes['record_idx_reads'].text);
            record_inserts := ALStrToInt64(ChildNodes['record_inserts'].text);
            record_updates := ALStrToInt64(ChildNodes['record_updates'].text);
            record_deletes := ALStrToInt64(ChildNodes['record_deletes'].text);
            record_backouts := ALStrToInt64(ChildNodes['record_backouts'].text);
            record_purges := ALStrToInt64(ChildNodes['record_purges'].text);
            record_expunges := ALStrToInt64(ChildNodes['record_expunges'].text);
          end;
        end;

        if not SkipMemoryUsage then begin
          with MemoryUsage do begin
            memory_used := ALStrToInt64(ChildNodes['memory_used'].text);
            memory_allocated := ALStrToInt64(ChildNodes['memory_allocated'].text);
            max_memory_used := ALStrToInt64(ChildNodes['max_memory_used'].text);
            max_memory_allocated := ALStrToInt64(ChildNodes['max_memory_allocated'].text);
          end;
        end;

      end;

    finally
      LXMLDATA.free;
    end;

    //commit the TMP transaction
    TransactionCommit(LDBHandle,
                      LTraHandle);

  Except

    //roolback the TMP transaction
    TransactionRollBack(LDBHandle,
                        LTraHandle);
    Raise;

  End;

end;

{************************************************************************}
function TALFBXConnectionPoolClient.GetFieldValue(aSQLDA: TALFBXSQLResult;
                                                  aDBHandle: IscDbHandle;
                                                  aTraHandle: IscTrHandle;
                                                  aIndex: Integer;
                                                  const aFormatSettings: TALFormatSettings): AnsiString;
  {-------------------------}
  Procedure InternalReadBlob;
  var BlobHandle: IscBlobHandle;
  begin
    with FLibrary do begin
      BlobHandle := nil;
      BlobOpen(aDBHandle, aTraHandle, BlobHandle, aSQLDA.AsQuad[aIndex]);
      try
        Result := BlobReadString(BlobHandle);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;

Begin
  If not aSQLDA.IsNull[aIndex] then begin
    Case aSQLDA.SQLType[aIndex] of
      SQL_TIMESTAMP : Result := ALDatetimetostr(aSQLDA.AsDateTime[aIndex], aFormatSettings);
      SQL_TYPE_TIME : Result := ALTimetostr(aSQLDA.AsTime[aIndex], aFormatSettings);
      SQL_TYPE_DATE : Result := ALDatetostr(aSQLDA.AsDate[aIndex], aFormatSettings);
      SQL_DOUBLE    : Result := ALFloatToStr(aSQLDA.AsDouble[aIndex], aFormatSettings);
      SQL_FLOAT,
      SQL_D_FLOAT   : Result := ALFloatToStr(aSQLDA.AsSingle[aIndex], aFormatSettings);
      SQL_INT64,
      SQL_LONG,
      SQL_SHORT     : begin
                        if aSQLDA.SQLScale[aIndex] < 0 then Result := ALFloatToStr(aSQLDA.asDouble[Aindex],aFormatSettings)
                        else result := aSQLDA.AsAnsiString[Aindex];
                      end;
      SQL_BLOB      : InternalReadBlob;
      else Result := aSQLDA.AsAnsiString[Aindex];
    end;
  end
  else result := fNullString;
end;

{******************************************************************}
procedure TALFBXConnectionPoolClient.initObject(const aDataBaseName,
                                                      aLogin,
                                                      aPassword,
                                                      aCharSet: AnsiString;
                                                const aNumbuffers: integer = -1;
                                                const aOpenConnectionExtraParams: AnsiString = '');
begin
  fDefaultReadTPB := cALFbxClientDefaultReadNOWaitTPB;
  fDefaultWriteTPB := cALFbxClientDefaultWriteNOWaitTPB;
  FDataBaseName:= aDataBaseName;
  FCharset:= ALFBXStrToCharacterSet(aCharSet);
  fLogin := aLogin;
  fPassword := aPassword;
  fOpenConnectionParams := 'user_name = '+aLogin+'; '+
                           'password = '+aPassword+'; '+
                           'lc_ctype = '+aCharSet;
  if aNumbuffers > -1 then fOpenConnectionParams := fOpenConnectionParams + '; num_buffers = ' + ALIntToStr(aNumbuffers);
  if aOpenConnectionExtraParams <> '' then fOpenConnectionParams := fOpenConnectionParams + '; ' + aOpenConnectionExtraParams;
  FSQLDIALECT := 3;
  FConnectionWithStmtPool:= TObjectList.Create(True);
  FConnectionWithStmtPoolCS:= TCriticalSection.create;
  FWorkingConnectionWithStmtCount:= 0;
  FLastConnectionWithStmtGarbage:= GetTickCount64;
  FConnectionWithoutStmtPool:= TObjectList.Create(True);
  FConnectionWithoutStmtPoolCS:= TCriticalSection.create;
  FReadTransactionPool:= TALStringKeyAVLBinaryTree.Create;
  FReadTransactionPoolCS:= TCriticalSection.Create;
  FReadStatementPool:= TALStringKeyAVLBinaryTree.Create;
  FReadStatementPoolCS:= TCriticalSection.Create;
  FWorkingConnectionWithoutStmtCount:= 0;
  FWorkingReadTransactionCount:= 0;
  FWorkingReadStatementCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionWithoutStmtGarbage := GetTickCount64;
  FLastReadTransactionGarbage := GetTickCount64;
  FLastReadStatementGarbage := GetTickCount64;
  FConnectionMaxIdleTime := 1200000; // 1000 * 60 * 20 = 20 min
  FTransactionMaxIdleTime := 300000; // 1000 * 60 * 5 = 5 min
  FStatementMaxIdleTime := 300000; // 1000 * 60 * 5 = 5 min
  FNullString := '';
end;

{****************************************************************}
constructor TALFBXConnectionPoolClient.Create(const aDataBaseName,
                                                    aLogin,
                                                    aPassword,
                                                    aCharSet: AnsiString;
                                              aApiVer: TALFBXVersion_API;
                                              const alib: AnsiString = GDS32DLL;
                                              const aNumbuffers: integer = -1;
                                              const aOpenConnectionExtraParams: AnsiString = '');
begin
  fLibrary := TALFBXLibrary.Create(aApiVer);
  fLibrary.Load(alib);
  FownLibrary := True;
  initObject(aDataBaseName,
             aLogin,
             aPassword,
             aCharSet,
             aNumbuffers,
             aOpenConnectionExtraParams);
end;

{****************************************************************}
constructor TALFBXConnectionPoolClient.Create(const aDataBaseName,
                                                    aLogin,
                                                    aPassword,
                                                    aCharSet: AnsiString;
                                              alib: TALFBXLibrary;
                                              const aNumbuffers: integer = -1;
                                              const aOpenConnectionExtraParams: AnsiString = '');
begin
  fLibrary := alib;
  FownLibrary := False;
  initObject(aDataBaseName,
             aLogin,
             aPassword,
             aCharSet,
             aNumbuffers,
             aOpenConnectionExtraParams);
end;

{********************************************}
destructor TALFBXConnectionPoolClient.Destroy;
begin

  //Release all connections
  ReleaseAllConnections;

  //free object
  FConnectionWithStmtPool.free;
  FConnectionWithStmtPoolCS.free;
  FConnectionWithoutStmtPool.free;
  FConnectionWithoutStmtPoolCS.free;
  FReadTransactionPool.free;
  FReadTransactionPoolCS.free;
  FReadStatementPool.free;
  FReadStatementPoolCS.free;
  if FownLibrary then fLibrary.Free;

  //inherite
  inherited;

end;

{**************************************************************************}
function  TALFBXConnectionPoolClient.GetDataBaseInfoInt(const item: Integer;
                                                        const DBHandle: IscDbHandle= nil): Integer;
Var LTmpDBHandle: IscDbHandle;
begin
  LTmpDBHandle := DBHandle;
  if not assigned(LTmpDBHandle) then LTmpDBHandle := AcquireConnectionWithoutStmt;
  try
    case item of
      isc_info_implementation,
      isc_info_base_level:
      result := byte(FLibrary.DatabaseInfoString(LTmpDBHandle, item, 8)[5]);
      else result := FLibrary.DatabaseInfoIntValue(LTmpDBHandle, AnsiChar(item));
    end;
  finally
    if not assigned(DBHandle) then ReleaseConnectionWithoutStmt(LTmpDBHandle);
  end;
end;

{*****************************************************************************}
function  TALFBXConnectionPoolClient.GetDataBaseInfoString(const item: Integer;
                                                           const DBHandle: IscDbHandle= nil): AnsiString;
Var LTmpDBHandle: IscDbHandle;
    LSize: byte;
    LData: AnsiString;
begin
  LTmpDBHandle := DBHandle;
  if not assigned(LTmpDBHandle) then LTmpDBHandle := AcquireConnectionWithoutStmt;
  try
    LData := FLibrary.DatabaseInfoString(LTmpDBHandle, item, 256);
    case Item of
      isc_info_cur_logfile_name,
      isc_info_wal_prv_ckpt_fname: begin
                                     LSize := byte(LData[4]);
                                     ALMove(LData[5], LData[1], LSize);
                                     SetLength(LData, LSize);
                                   end;
      else begin
        LSize := byte(LData[5]);
        ALMove(LData[6], LData[1], LSize);
        SetLength(LData, LSize);
      end;
    end;
    Result := AnsiString(LData);
  finally
    if not assigned(DBHandle) then ReleaseConnectionWithoutStmt(LTmpDBHandle);
  end;
end;

{*******************************************************************************}
function  TALFBXConnectionPoolClient.GetDataBaseInfoDateTime(const item: Integer;
                                                             const DBHandle: IscDbHandle= nil): TDateTime;
Var LTmpDBHandle: IscDbHandle;
begin
  LTmpDBHandle := DBHandle;
  if not assigned(LTmpDBHandle) then LTmpDBHandle := AcquireConnectionWithoutStmt;
  try
    result := FLibrary.DatabaseInfoDateTime(LTmpDBHandle, item);
  finally
    if not assigned(DBHandle) then ReleaseConnectionWithoutStmt(LTmpDBHandle);
  end;
end;

{**********************************************************************************}
function TALFBXConnectionPoolClient.GetConnectionID(DBHandle: IscDbHandle): Integer;
begin
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  result := GetDataBaseInfoInt(isc_info_attachment_id, DBHandle);
end;

{*************************************************************************************}
function TALFBXConnectionPoolClient.GetTransactionID(TraHandle: IscTrHandle): Cardinal;
begin
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');
  Result := Flibrary.TransactionGetId(TraHandle)
end;

{**************************************************************}
function TALFBXConnectionPoolClient.GetDataBaseName: AnsiString;
begin
  result := FdatabaseName;
end;

{**}
Type
  TALFBXConnectionPoolClient_ConnectionStatementPoolIterateExtData = Record
    ConnectionPoolClient: TALFBXConnectionPoolClient;
    TickCountCurrentdate: int64;
    LstBinaryTreeNodeToDelete: TobjectList;
  End;

{**************************************************************************************************}
procedure ALFBXConnectionPoolClient_ConnectionStatementPoolIterateFunct(aTree: TALBaseAVLBinaryTree;
                                                                        aNode: TALBaseAVLBinaryTreeNode;
                                                                        aExtData: Pointer;
                                                                        Var aContinue: Boolean);
begin

  //typecast the aExtData
  With TALFBXConnectionPoolClient_ConnectionStatementPoolIterateExtData(aExtData^) do

    //if the Statement is expired
    if TickCountCurrentdate - TALFBXConnectionStatementPoolBinTreeNode(aNode).Lastaccessdate > ConnectionPoolClient.StatementMaxIdleTime then LstBinaryTreeNodeToDelete.Add(aNode)

end;

{***************************************************************************************}
Procedure TALFBXConnectionPoolClient.AcquireConnectionWithStmt(var DBHandle: IscDbHandle;
                                                               var StatementPool: TALFBXConnectionStatementPoolBinTree);
Var LConnectionStatementPoolIterateExtData: TALFBXConnectionPoolClient_ConnectionStatementPoolIterateExtData;
    LConnectionWithStmtPoolContainer: TALFBXConnectionWithStmtPoolContainer;
    LTickCount: int64;
    I: integer;
Begin

  //synchronize the code
  FConnectionWithStmtPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    LTickCount := GetTickCount64;
    if LTickCount - FLastConnectionWithStmtGarbage > (60000 {every minutes})  then begin
      while FConnectionWithStmtPool.Count > 0 do begin
        LConnectionWithStmtPoolContainer := TALFBXConnectionWithStmtPoolContainer(FConnectionWithStmtPool[0]);
        if LTickCount - LConnectionWithStmtPoolContainer.Lastaccessdate > FConnectionMaxIdleTime then begin

          //first drop all statements
          LConnectionWithStmtPoolContainer.StatementPool.free;

          //now drop the connection
          Try
            fLibrary.DetachDatabase(LConnectionWithStmtPoolContainer.DBHandle);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;

          //Free the container.
          FConnectionWithStmtPool.Delete(0);

        end
        else break;
      end;
      FLastConnectionWithStmtGarbage := GetTickCount64;
    end;

    //acquire the new connection from the pool
    If FConnectionWithStmtPool.Count > 0 then begin
      LConnectionWithStmtPoolContainer := TALFBXConnectionWithStmtPoolContainer(FConnectionWithStmtPool[FConnectionWithStmtPool.count - 1]);
      DBHandle := LConnectionWithStmtPoolContainer.DBHandle;
      StatementPool := LConnectionWithStmtPoolContainer.StatementPool;
      FConnectionWithStmtPool.Delete(FConnectionWithStmtPool.count - 1);
    end

    //create a new connection
    else begin
      DBHandle := nil;
      StatementPool := nil;
      FLibrary.AttachDatabase(DataBaseName,
                              DBHandle,
                              fOpenConnectionParams);
      StatementPool := TALFBXConnectionStatementPoolBinTree.Create;
    end;

    //increase the connection count
    inc(FWorkingConnectionWithStmtCount);

  //get out of the synchronization
  finally
    FConnectionWithStmtPoolCS.Release;
  end;

  // delete the old unused statements
  if GetTickCount64 - StatementPool.LastGarbage > (60000 {every minutes})  then begin

    //create aExtData.LstNodeToDelete
    LConnectionStatementPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
    Try

      //init aExtData.ConnectionPoolClient and aExtData.TickCount
      LConnectionStatementPoolIterateExtData.ConnectionPoolClient := Self;
      LConnectionStatementPoolIterateExtData.TickCountCurrentdate := GetTickCount64;

      //iterate all StatementPool node
      StatementPool.Iterate(ALFBXConnectionPoolClient_ConnectionStatementPoolIterateFunct,
                            true,
                            @LConnectionStatementPoolIterateExtData);

      //delete all StatementPool node that have an empty pool
      for I := 0 to LConnectionStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
        StatementPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(LConnectionStatementPoolIterateExtData.LstBinaryTreeNodeToDelete[I]).id);

      //init the StatementPool.LastGarbage
      StatementPool.LastGarbage := GetTickCount64;

    Finally
      LConnectionStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
    End;

  end;

End;

{***************************************************************************************}
Procedure TALFBXConnectionPoolClient.ReleaseConnectionWithStmt(var DBHandle: IscDbHandle;
                                                               var StatementPool: TALFBXConnectionStatementPoolBinTree;
                                                               const CloseConnection: Boolean = False);
Var LConnectionWithStmtPoolContainer: TALFBXConnectionWithStmtPoolContainer;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(StatementPool) then raise exception.Create('StatementPool can not be null');

  //release the connection
  FConnectionWithStmtPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      LConnectionWithStmtPoolContainer := TALFBXConnectionWithStmtPoolContainer.Create;
      LConnectionWithStmtPoolContainer.DBHandle := DBHandle;
      LConnectionWithStmtPoolContainer.StatementPool:= StatementPool;
      LConnectionWithStmtPoolContainer.LastAccessDate := GetTickCount64;
      FConnectionWithStmtPool.add(LConnectionWithStmtPoolContainer);
    end

    else begin

      //free the statements
      StatementPool.free;

      //close the connection
      try
        FLibrary.DetachDatabase(DBHandle);
      Except
        //yes the function before can do an exception if the network connection
        //was dropped... but not our bussiness what we can do ?
      end;

    end;

    //set the connectionhandle to nil
    DBHandle := nil;
    StatementPool := nil;

    //dec the WorkingConnectionCount
    Dec(FWorkingConnectionWithStmtCount);

  finally
    FConnectionWithStmtPoolCS.Release;
  end;

end;

{****************************************************************************}
function TALFBXConnectionPoolClient.AcquireConnectionWithoutStmt: IscDbHandle;
Var LConnectionWithoutStmtPoolContainer: TALFBXConnectionWithoutStmtPoolContainer;
    LTickCount: int64;
Begin

  //synchronize the code
  FConnectionWithoutStmtPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    LTickCount := GetTickCount64;
    if LTickCount - FLastConnectionWithoutStmtGarbage > (60000 {every minutes})  then begin
      while FConnectionWithoutStmtPool.Count > 0 do begin
        LConnectionWithoutStmtPoolContainer := TALFBXConnectionWithoutStmtPoolContainer(FConnectionWithoutStmtPool[0]);
        if LTickCount - LConnectionWithoutStmtPoolContainer.Lastaccessdate > FConnectionMaxIdleTime then begin

          //drop the connection
          Try
            fLibrary.DetachDatabase(LConnectionWithoutStmtPoolContainer.DBHandle);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;

          //Free the container.
          FConnectionWithoutStmtPool.Delete(0);

        end
        else break;
      end;
      FLastConnectionWithoutStmtGarbage := GetTickCount64;
    end;

    //acquire the new connection from the pool
    If FConnectionWithoutStmtPool.Count > 0 then begin
      LConnectionWithoutStmtPoolContainer := TALFBXConnectionWithoutStmtPoolContainer(FConnectionWithoutStmtPool[FConnectionWithoutStmtPool.count - 1]);
      Result := LConnectionWithoutStmtPoolContainer.DBHandle;
      FConnectionWithoutStmtPool.Delete(FConnectionWithoutStmtPool.count - 1);
    end

    //create a new connection
    else begin
      Result := nil;
      FLibrary.AttachDatabase(DataBaseName,
                              Result,
                              fOpenConnectionParams);
    end;

    //increase the connection count
    inc(FWorkingConnectionWithoutStmtCount);

  //get out of the synchronization
  finally
    FConnectionWithoutStmtPoolCS.Release;
  end;

End;

{******************************************************************************************}
procedure TALFBXConnectionPoolClient.ReleaseConnectionWithoutStmt(var DBHandle: IscDbHandle;
                                                                   const CloseConnection: Boolean = False);
Var LConnectionWithoutStmtPoolContainer: TALFBXConnectionWithoutStmtPoolContainer;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');

  //release the connection
  FConnectionWithoutStmtPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      LConnectionWithoutStmtPoolContainer := TALFBXConnectionWithoutStmtPoolContainer.Create;
      LConnectionWithoutStmtPoolContainer.DBHandle := DBHandle;
      LConnectionWithoutStmtPoolContainer.LastAccessDate := GetTickCount64;
      FConnectionWithoutStmtPool.add(LConnectionWithoutStmtPoolContainer);
    end

    else begin

      //close the connection
      try
        FLibrary.DetachDatabase(DBHandle);
      Except
        //yes the function before can do an exception if the network connection
        //was dropped... but not our bussiness what we can do ?
      end;

    end;

    //set the connectionhandle to nil
    DBHandle := nil;

    //dec the WorkingConnectionCount
    Dec(FWorkingConnectionWithoutStmtCount);

  finally
    FConnectionWithoutStmtPoolCS.Release;
  end;

end;

{**}
Type
  TALFBXConnectionPoolClient_ReadTransactionPoolIterateExtData = Record
    ConnectionPoolClient: TALFBXConnectionPoolClient;
    TickCountCurrentdate: int64;
    LstBinaryTreeNodeToDelete: TobjectList;
  End;

{**********************************************************************************************}
procedure ALFBXConnectionPoolClient_ReadTransactionPoolIterateFunct(aTree: TALBaseAVLBinaryTree;
                                                                    aNode: TALBaseAVLBinaryTreeNode;
                                                                    aExtData: Pointer;
                                                                    Var aContinue: Boolean);

Var LReadTransactionPool: TobjectList;
    LReadTransactionPoolContainer: TalFBXReadTransactionPoolContainer;

begin

  //typecast the aExtData
  With TALFBXConnectionPoolClient_ReadTransactionPoolIterateExtData(aExtData^) do begin

    //init aReadTransactionPool
    LReadTransactionPool := TALFBXStringKeyPoolBinTreeNode(aNode).Pool;

    //still their is some item in the aReadTransactionPool
    while LReadTransactionPool.Count > 0 do begin

      //init aReadTransactionPoolContainer
      LReadTransactionPoolContainer := TalFBXReadTransactionPoolContainer(LReadTransactionPool[0]);

      //if the aReadTransactionPoolContainer is expired
      if TickCountCurrentdate - LReadTransactionPoolContainer.Lastaccessdate > ConnectionPoolClient.TransactionMaxIdleTime then begin

        Try

          //commit the transaction and release the connection
          ConnectionPoolClient.TransactionCommit(LReadTransactionPoolContainer.DBHandle,
                                                 LReadTransactionPoolContainer.TraHandle);

        Except

          //roolback the transaction (and free the connection)
          if assigned(LReadTransactionPoolContainer.TraHandle) then ConnectionPoolClient.TransactionRollback(LReadTransactionPoolContainer.DBHandle,
                                                                                                             LReadTransactionPoolContainer.TraHandle,
                                                                                                             True) // const CloseConnection: Boolean = False

          //simply free the connection
          else if assigned(LReadTransactionPoolContainer.DBHandle) then ConnectionPoolClient.ReleaseConnectionWithoutStmt(LReadTransactionPoolContainer.DBHandle,
                                                                                                                           True); // const CloseConnection: Boolean = False

        End;

        //free the TransactionPoolContainer
        LReadTransactionPool.Delete(0);

      end

      //if the aReadTransactionPoolContainer is NOT expired, break the loop
      else break;

    end;

    //update LstNodeToDelete
    if LReadTransactionPool.Count = 0 then LstBinaryTreeNodeToDelete.Add(aNode);

  end;

end;

{************************************************************************************}
procedure TALFBXConnectionPoolClient.AcquireReadTransaction(var DBHandle: IscDbHandle;
                                                            var TraHandle: IscTrHandle;
                                                            const TPB: AnsiString);

Var LReadTransactionPoolIterateExtData: TALFBXConnectionPoolClient_ReadTransactionPoolIterateExtData;
    LReadTransactionPoolContainer: TalFBXReadTransactionPoolContainer;
    LStringKeyPoolBinTreeNode: TALStringKeyAVLBinaryTreeNode;
    I: integer;

Begin

  //security check, TPB can not be empty and
  //must be only read commited read only
  if (TPB = '') or
     ((alpos(isc_tpb_read_committed,TPB) <= 0) or
      (alpos(isc_tpb_read, TPB) <= 0) or
      (alpos(isc_tpb_write, TPB) > 0))
  then raise Exception.Create('Wrong Params');
  if assigned(DBHandle) then raise exception.Create('Connection handle must be null');
  if assigned(TraHandle) then raise exception.Create('Transaction handle must be null');

  //synchronize the code
  FReadTransactionPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire transaction: currently releasing all connections');

    //delete the old unused Transaction
    if GetTickCount64 - FLastReadTransactionGarbage > (60000 {every minutes})  then begin

      //create aExtData.LstNodeToDelete
      LReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
      Try

        //init aExtData.ConnectionPoolClient and aExtData.TickCount
        LReadTransactionPoolIterateExtData.ConnectionPoolClient := Self;
        LReadTransactionPoolIterateExtData.TickCountCurrentdate := GetTickCount64;

        //iterate all FReadTransactionPool node
        FReadTransactionPool.Iterate(ALFBXConnectionPoolClient_ReadTransactionPoolIterateFunct,
                                     true,
                                     @LReadTransactionPoolIterateExtData);

        //delete all FReadTransactionPool node that have an empty pool
        for I := 0 to LReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
          FReadTransactionPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(LReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete[I]).id);

        //init the FLastReadTransactionGarbage
        FLastReadTransactionGarbage := GetTickCount64;

      Finally
        LReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
      End;

    end;

    //look for a node with TPB id in the ReadTransactionPool. if not found create it
    LStringKeyPoolBinTreeNode := FReadTransactionPool.FindNode(TPB);
    if not assigned(LStringKeyPoolBinTreeNode) then begin
      LStringKeyPoolBinTreeNode := TALFBXStringKeyPoolBinTreeNode.Create;
      LStringKeyPoolBinTreeNode.ID := TPB;
      if not FReadTransactionPool.AddNode(LStringKeyPoolBinTreeNode) then begin
        LStringKeyPoolBinTreeNode.free;
        Raise Exception.Create('Can not add any more node in transaction pool binary tree');
      end;
    end;

    //if the if still some transaction in the queue
    If TALFBXStringKeyPoolBinTreeNode(LStringKeyPoolBinTreeNode).Pool.Count > 0 then begin
      with TALFBXStringKeyPoolBinTreeNode(LStringKeyPoolBinTreeNode) do begin
        LReadTransactionPoolContainer := TalFBXReadTransactionPoolContainer(Pool[Pool.count - 1]);
        DBHandle := LReadTransactionPoolContainer.DBHandle;
        TraHandle := LReadTransactionPoolContainer.TraHandle;
        Pool.Delete(Pool.count - 1);
      end;
    end

    //Else create a new Transaction
    else begin
      DBHandle := nil;
      TraHandle := nil;
      TransactionStart(DBHandle,
                       TraHandle,
                       TPB);
    end;

    //increase the Transaction count
    inc(FWorkingReadTransactionCount);

  //get out of the synchronization
  finally
    FReadTransactionPoolCS.Release;
  end;

End;

{************************************************************************************}
procedure TALFBXConnectionPoolClient.ReleaseReadTransaction(var DBHandle: IscDbHandle;
                                                            var TraHandle: IscTrHandle;
                                                            const TPB: AnsiString;
                                                            const CloseConnection: Boolean = False);

Var LReadTransactionPoolContainer: TalFBXReadTransactionPoolContainer;
    LStringKeyPoolBinTreeNode: TALStringKeyAVLBinaryTreeNode;

begin

  //security check
  if (TPB = '')  then raise Exception.Create('Wrong Params');
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');

  //release the transaction
  FReadTransactionPoolCS.Acquire;
  Try

    //add the transaction to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin

      //look for the node in FReadTransactionPool
      LStringKeyPoolBinTreeNode := FReadTransactionPool.FindNode(TPB);
      if not assigned(LStringKeyPoolBinTreeNode) then begin
        LStringKeyPoolBinTreeNode := TALFBXStringKeyPoolBinTreeNode.Create;
        LStringKeyPoolBinTreeNode.ID := TPB;
        if not FReadTransactionPool.AddNode(LStringKeyPoolBinTreeNode) then begin
          LStringKeyPoolBinTreeNode.free;
          Raise Exception.Create('Can not add any more node in transaction pool binary tree');
        end;
      end;

      //add the item to the pool
      LReadTransactionPoolContainer := TalFBXReadTransactionPoolContainer.Create;
      LReadTransactionPoolContainer.DBHandle := DBHandle;
      LReadTransactionPoolContainer.TraHandle := TraHandle;
      LReadTransactionPoolContainer.LastAccessDate := GetTickCount64;
      TALFBXStringKeyPoolBinTreeNode(LStringKeyPoolBinTreeNode).Pool.add(LReadTransactionPoolContainer);

    end

    //close the Transaction
    else begin

      Try

        //commit the transaction and release the connection
        TransactionCommit(DBHandle,
                          TraHandle,
                          CloseConnection);

      Except

        //roolback the transaction (and free the connection)
        if assigned(TraHandle) then TransactionRollback(DBHandle,
                                                        TraHandle,
                                                        True) // const CloseConnection: Boolean = False

        //simply free the connection
        else if assigned(DBHandle) then ReleaseConnectionWithoutStmt(DBHandle,
                                                                      True); // const CloseConnection: Boolean = False

      End;

    end;

    //set the connectionhandle to nil
    DBHandle := nil;
    TraHandle := nil;

    //dec the WorkingTransactionCount
    Dec(FWorkingReadTransactionCount);

  finally
    FReadTransactionPoolCS.Release;
  end;

end;

{**}
Type
  TALFBXConnectionPoolClient_ReadStatementPoolIterateExtData = Record
    ConnectionPoolClient: TALFBXConnectionPoolClient;
    TickCountCurrentdate: int64;
    LstBinaryTreeNodeToDelete: TobjectList;
  End;

{********************************************************************************************}
procedure ALFBXConnectionPoolClient_ReadStatementPoolIterateFunct(aTree: TALBaseAVLBinaryTree;
                                                                  aNode: TALBaseAVLBinaryTreeNode;
                                                                  aExtData: Pointer;
                                                                  Var aContinue: Boolean);

Var LReadStatementPool: TobjectList;
    LReadStatementPoolContainer: TalFBXReadStatementPoolContainer;

begin

  //typecast the aExtData
  With TALFBXConnectionPoolClient_ReadStatementPoolIterateExtData(aExtData^) do begin

    //init aReadStatementPool
    LReadStatementPool := TALFBXStringKeyPoolBinTreeNode(aNode).Pool;

    //still their is some item in the aReadStatementPool
    while LReadStatementPool.Count > 0 do begin

      //init aReadStatementPoolContainer
      LReadStatementPoolContainer := TalFBXReadStatementPoolContainer(LReadStatementPool[0]);

      //if the aReadStatementPoolContainer is expired
      if TickCountCurrentdate - LReadStatementPoolContainer.Lastaccessdate > ConnectionPoolClient.StatementMaxIdleTime then begin

        Try

          //drop the statement
          try
            ConnectionPoolClient.lib.DSQLFreeStatement(LReadStatementPoolContainer.StmtHandle, DSQL_drop);
          Except
            //what else we can do here ?
            //this can happen if connection lost for exemple
          end;
          LReadStatementPoolContainer.Sqlda.free;
          LReadStatementPoolContainer.StmtHandle := nil;
          LReadStatementPoolContainer.Sqlda := nil;

          //commit and release the connection
          ConnectionPoolClient.TransactionCommit(LReadStatementPoolContainer.DBHandle,
                                                 LReadStatementPoolContainer.TraHandle);

        Except

          //roolback the transaction (and free the connection)
          if assigned(LReadStatementPoolContainer.TraHandle) then ConnectionPoolClient.TransactionRollback(LReadStatementPoolContainer.DBHandle,
                                                                                                           LReadStatementPoolContainer.TraHandle,
                                                                                                           True) // const CloseConnection: Boolean = False

          //simply free the connection
          else if assigned(LReadStatementPoolContainer.DBHandle) then ConnectionPoolClient.ReleaseConnectionWithoutStmt(LReadStatementPoolContainer.DBHandle,
                                                                                                                         True); // const CloseConnection: Boolean = False

        End;

        //free the ReadStatementPoolContainer
        LReadStatementPool.Delete(0);

      end

      //if the aReadStatementPoolContainer is NOT expired, break the loop
      else break;

    end;

    //update LstNodeToDelete
    if LReadStatementPool.Count = 0 then LstBinaryTreeNodeToDelete.Add(aNode);

  end;

end;

{******************************************************************************}
procedure TALFBXConnectionPoolClient.AcquireReadStatement(const SQL: AnsiString;
                                                          var DBHandle: IscDbHandle;
                                                          var TraHandle: IscTrHandle;
                                                          var StmtHandle: IscStmtHandle;
                                                          var Sqlda: TALFBXSQLResult;
                                                          const TPB: AnsiString);

Var LReadStatementPoolIterateExtData: TALFBXConnectionPoolClient_ReadStatementPoolIterateExtData;
    LReadStatementPoolContainer: TalFBXReadStatementPoolContainer;
    LStringKeyPoolBinTreeNode: TALStringKeyAVLBinaryTreeNode;
    I: integer;

Begin

  //security check, SQL and TPB can not be empty and
  //must be only read commited read only
  if (SQL = '') or
     (TPB = '') or
     ((alpos(isc_tpb_read_committed,TPB) <= 0) or
      (alpos(isc_tpb_read, TPB) <= 0) or
      (alpos(isc_tpb_write, TPB) > 0))
  then raise Exception.Create('Wrong Params');
  if assigned(DBHandle) then raise exception.Create('Connection handle must be null');
  if assigned(TraHandle) then raise exception.Create('Transaction handle must be null');
  if assigned(StmtHandle) then raise exception.Create('Statement handle must be null');
  if assigned(Sqlda) then raise exception.Create('Statement descriptor must be null');

  //synchronize the code
  FReadStatementPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire Statement: currently releasing all connections');

    //delete the old unused Statement
    if GetTickCount64 - FLastReadStatementGarbage > (60000 {every minutes})  then begin

      //create aExtData.LstNodeToDelete
      LReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
      Try

        //init aExtData.ConnectionPoolClient and aExtData.TickCount
        LReadStatementPoolIterateExtData.ConnectionPoolClient := Self;
        LReadStatementPoolIterateExtData.TickCountCurrentdate := GetTickCount64;

        //iterate all FReadStatementPool node
        FReadStatementPool.Iterate(ALFBXConnectionPoolClient_ReadStatementPoolIterateFunct,
                                   true,
                                   @LReadStatementPoolIterateExtData);

        //delete all FReadStatementPool node that have an empty pool
        for I := 0 to LReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
          FReadStatementPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(LReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete[I]).id);

        //init the FLastReadStatementGarbage
        FLastReadStatementGarbage := GetTickCount64;

      Finally
        LReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
      End;

    end;

    //look for a node with TPB + SQL id in the ReadStatementPool. if not found create it
    LStringKeyPoolBinTreeNode := FReadStatementPool.FindNode(TPB+'#'+SQL);
    if not assigned(LStringKeyPoolBinTreeNode) then begin
      LStringKeyPoolBinTreeNode := TALFBXStringKeyPoolBinTreeNode.Create;
      LStringKeyPoolBinTreeNode.ID := TPB+'#'+SQL;
      if not FReadStatementPool.AddNode(LStringKeyPoolBinTreeNode) then begin
        LStringKeyPoolBinTreeNode.free;
        Raise Exception.Create('Can not add any more node in statement pool binary tree');
      end;
    end;

    //if the if still some Statement in the queue
    If TALFBXStringKeyPoolBinTreeNode(LStringKeyPoolBinTreeNode).Pool.Count > 0 then begin
      with TALFBXStringKeyPoolBinTreeNode(LStringKeyPoolBinTreeNode) do begin
        LReadStatementPoolContainer := TalFBXReadStatementPoolContainer(Pool[Pool.count - 1]);
        DBHandle := LReadStatementPoolContainer.DBHandle;
        TraHandle := LReadStatementPoolContainer.TraHandle;
        StmtHandle := LReadStatementPoolContainer.StmtHandle;
        Sqlda := LReadStatementPoolContainer.Sqlda;
        Pool.Delete(Pool.count - 1);
      end;
    end

    //Else create a new Statement
    else begin
      DBHandle := nil;
      TraHandle := nil;
      StmtHandle := nil;
      Sqlda := nil;
      Prepare(SQL,
              DBHandle,
              TraHandle,
              StmtHandle,
              Sqlda,
              TPB);
    end;

    //increase the Statement count
    inc(FWorkingReadStatementCount);

  //get out of the synchronization
  finally
    FReadStatementPoolCS.Release;
  end;

End;

{******************************************************************************}
procedure TALFBXConnectionPoolClient.ReleaseReadStatement(const SQL: AnsiString;
                                                          var DBHandle: IscDbHandle;
                                                          var TraHandle: IscTrHandle;
                                                          var StmtHandle: IscStmtHandle;
                                                          var Sqlda: TALFBXSQLResult;
                                                          const TPB: AnsiString;
                                                          const CloseConnection: Boolean = False);

Var LReadStatementPoolContainer: TalFBXReadStatementPoolContainer;
    LStringKeyPoolBinTreeNode: TALStringKeyAVLBinaryTreeNode;

begin

  //security check
  //security check, SQL and TPB can not be empty and
  //must be only read commited read only
  if (SQL = '') or
     (TPB = '') or
     ((alpos(isc_tpb_read_committed,TPB) <= 0) or
      (alpos(isc_tpb_read, TPB) <= 0) or
      (alpos(isc_tpb_write, TPB) > 0))
  then raise Exception.Create('Wrong Params');
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');
  if not assigned(StmtHandle) then raise exception.Create('Statement handle can not be null');
  if not assigned(Sqlda) then raise exception.Create('Statement descriptor can not be null');

  //release the Statement
  FReadStatementPoolCS.Acquire;
  Try

    //add the Statement to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin

      //look for the node in FReadStatementPool
      LStringKeyPoolBinTreeNode := FReadStatementPool.FindNode(TPB+'#'+SQL);
      if not assigned(LStringKeyPoolBinTreeNode) then begin
        LStringKeyPoolBinTreeNode := TALFBXStringKeyPoolBinTreeNode.Create;
        LStringKeyPoolBinTreeNode.ID := TPB+'#'+SQL;
        if not FReadStatementPool.AddNode(LStringKeyPoolBinTreeNode) then begin
          LStringKeyPoolBinTreeNode.free;
          Raise Exception.Create('Can not add any more node in statement pool binary tree');
        end;
      end;

      //add the item to the pool
      LReadStatementPoolContainer := TalFBXReadStatementPoolContainer.Create;
      LReadStatementPoolContainer.DBHandle := DBHandle;
      LReadStatementPoolContainer.TraHandle := TraHandle;
      LReadStatementPoolContainer.StmtHandle := StmtHandle;
      LReadStatementPoolContainer.Sqlda := Sqlda;
      LReadStatementPoolContainer.LastAccessDate := GetTickCount64;
      TALFBXStringKeyPoolBinTreeNode(LStringKeyPoolBinTreeNode).Pool.add(LReadStatementPoolContainer);

    end

    //close the Statement
    else begin

      Try

        //drop the statement
        try
          FLibrary.DSQLFreeStatement(StmtHandle, DSQL_drop);
        Except
          //what else we can do here ?
          //this can happen if connection lost for exemple
        end;
        Sqlda.free;

        //commit and release the connection
        TransactionCommit(DBHandle,
                          TraHandle);

      Except

        //roolback the transaction (and free the connection)
        if assigned(TraHandle) then TransactionRollback(DBHandle,
                                                        TraHandle,
                                                        True) // const CloseConnection: Boolean = False

        //simply free the connection
        else if assigned(DBHandle) then ReleaseConnectionWithoutStmt(DBHandle,
                                                                     True); // const CloseConnection: Boolean = False

      End;

    end;

    //set the connectionhandle to nil
    DBHandle := nil;
    TraHandle := nil;
    StmtHandle := nil;
    Sqlda := nil;

    //dec the WorkingStatementCount
    Dec(FWorkingReadStatementCount);

  finally
    FReadStatementPoolCS.Release;
  end;

end;

{*******************************************************************************************************}
procedure TALFBXConnectionPoolClient.ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True);
Var LConnectionWithStmtPoolContainer: TALFBXConnectionWithStmtPoolContainer;
    LConnectionWithoutStmtPoolContainer: TALFBXConnectionWithoutStmtPoolContainer;
    LReadTransactionPoolIterateExtData: TALFBXConnectionPoolClient_ReadTransactionPoolIterateExtData;
    LReadStatementPoolIterateExtData: TALFBXConnectionPoolClient_ReadStatementPoolIterateExtData;
    I: integer;
begin

  //we do this to forbid any new thread to create a new transaction
  FReleasingAllconnections := True;
  Try

    /////////////
    //STATEMENT//
    /////////////

    //wait that all statements are released
    if WaitWorkingConnections then begin
      while true do begin
        FReadStatementPoolCS.Acquire;
        Try
          if FWorkingReadStatementCount <= 0 then break;
        finally
          FReadStatementPoolCS.Release;
        end;
        sleep(1);
      end;
    end;

    //free all statements
    FReadStatementPoolCS.Acquire;
    Try

      LReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
      Try

        //init aExtData.ConnectionPoolClient and aExtData.TickCount
        LReadStatementPoolIterateExtData.ConnectionPoolClient := Self;
        LReadStatementPoolIterateExtData.TickCountCurrentdate :=  high(int64); //this will cause to delete all node

        //iterate all FReadStatementPool node
        FReadStatementPool.Iterate(ALFBXConnectionPoolClient_ReadStatementPoolIterateFunct,
                                   true,
                                   @LReadStatementPoolIterateExtData);

        //delete all FReadStatementPool node that have an empty pool
        for I := 0 to LReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
          FReadStatementPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(LReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete[I]).id);

        //init the FLastReadStatementGarbage
        FLastReadStatementGarbage := GetTickCount64;

      Finally
        LReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
      End;

    Finally
      FReadStatementPoolCS.release;
    End;




    ///////////////
    //TRANSACTION//
    ///////////////

    //wait that all connections are released
    if WaitWorkingConnections then begin
      while true do begin
        FReadTransactionPoolCS.Acquire;
        Try
          if FWorkingReadTransactionCount <= 0 then break;
        finally
          FReadTransactionPoolCS.Release;
        end;
        sleep(1);
      end;
    end;

    //free all transactions
    FReadTransactionPoolCS.Acquire;
    Try

      LReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
      Try

        //init aExtData.ConnectionPoolClient and aExtData.TickCount
        LReadTransactionPoolIterateExtData.ConnectionPoolClient := Self;
        LReadTransactionPoolIterateExtData.TickCountCurrentdate :=  high(int64); //this will cause to delete all node

        //iterate all FReadTransactionPool node
        FReadTransactionPool.Iterate(ALFBXConnectionPoolClient_ReadTransactionPoolIterateFunct,
                                     true,
                                     @LReadTransactionPoolIterateExtData);

        //delete all FReadTransactionPool node that have an empty pool
        for I := 0 to LReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
          FReadTransactionPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(LReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete[I]).id);

        //init the FLastReadTransactionGarbage
        FLastReadTransactionGarbage := GetTickCount64;

      Finally
        LReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
      End;

    Finally
      FReadTransactionPoolCS.Release;
    End;




    /////////////////////////////
    //CONNECTION WITH STATEMENT//
    /////////////////////////////

    //wait that all connections are released
    if WaitWorkingConnections then begin
      while true do begin
        FConnectionWithStmtPoolCS.Acquire;
        Try
          if FWorkingConnectionWithStmtCount <= 0 then break;
        finally
          FConnectionWithStmtPoolCS.Release;
        end;
        sleep(1);
      end;
    end;

    //free all connections
    FConnectionWithStmtPoolCS.Acquire;
    Try
      while FConnectionWithStmtPool.Count > 0 do begin

        //init aConnectionWithStmtPoolContainer
        LConnectionWithStmtPoolContainer := TALFBXConnectionWithStmtPoolContainer(FConnectionWithStmtPool[FConnectionWithStmtPool.count - 1]);

        //first drop all statements
        LConnectionWithStmtPoolContainer.StatementPool.free;

        //now drop the connection
        Try
          fLibrary.DetachDatabase(LConnectionWithStmtPoolContainer.DBHandle);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;

        //Free the container.
        FConnectionWithStmtPool.Delete(FConnectionWithStmtPool.count - 1);

      end;
      FLastConnectionWithStmtGarbage := GetTickCount64;
    finally
      FConnectionWithStmtPoolCS.Release;
    end;




    ////////////////////////////////
    //CONNECTION WITHOUT STATEMENT//
    ////////////////////////////////

    //wait that all connections are released
    if WaitWorkingConnections then begin
      while true do begin
        FConnectionWithoutStmtPoolCS.Acquire;
        Try
          if FWorkingConnectionWithoutStmtCount <= 0 then break;
        finally
          FConnectionWithoutStmtPoolCS.Release;
        end;
        sleep(1);
      end;
    end;

    //free all connections
    FConnectionWithoutStmtPoolCS.Acquire;
    Try
      while FConnectionWithoutStmtPool.Count > 0 do begin

        //init aConnectionWithoutStmtPoolContainer
        LConnectionWithoutStmtPoolContainer := TALFBXConnectionWithoutStmtPoolContainer(FConnectionWithoutStmtPool[FConnectionWithoutStmtPool.count - 1]);

        //now drop the connection
        Try
          fLibrary.DetachDatabase(LConnectionWithoutStmtPoolContainer.DBHandle);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;

        //free the container
        FConnectionWithoutStmtPool.Delete(FConnectionWithoutStmtPool.count - 1);

      end;
      FLastConnectionWithoutStmtGarbage := GetTickCount64;
    finally
      FConnectionWithoutStmtPoolCS.Release;
    end;


  finally
    //Do not forbid anymore new thread to create a new connections/transactions/statements
    FReleasingAllconnections := False;
  End;

end;

{******************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionStart(Var DBHandle: IscDbHandle;
                                                      var TraHandle: IscTrHandle;
                                                      var StatementPool: TALFBXConnectionStatementPoolBinTree;
                                                      const TPB: AnsiString);
begin

  //DBHandle, TraHandle and StatementPool must be null
  if assigned(DBHandle) then raise exception.Create('Connection handle must be null');
  if assigned(TraHandle) then raise exception.Create('Transaction handle must be null');
  if assigned(StatementPool) then raise exception.Create('Statement pool object must be null');

  //init the aConnectionHandle
  AcquireConnectionWithStmt(DBHandle, StatementPool);
  try

    //Start the transaction
    Flibrary.TransactionStart(TraHandle,
                              DBHandle,
                              TPB);

  except
    ReleaseConnectionWithStmt(DBHandle, StatementPool, True);
    TraHandle := nil;
    StatementPool := nil;
    raise;
  end;

end;

{*******************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionCommit(var DBHandle: IscDbHandle;
                                                       var TraHandle: IscTrHandle;
                                                       var StatementPool: TALFBXConnectionStatementPoolBinTree;
                                                       const CloseConnection: Boolean = False);
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');
  if not assigned(StatementPool) then raise exception.Create('Statement pool object can not be null');

  //commit the transaction
  FLibrary.TransactionCommit(TraHandle);
  TraHandle := nil;

  //release the connection
  ReleaseConnectionWithStmt(DBHandle,
                            StatementPool,
                            CloseConnection);

end;

{*********************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionRollback(var DBHandle: IscDbHandle;
                                                         var TraHandle: IscTrHandle;
                                                         var StatementPool: TALFBXConnectionStatementPoolBinTree;
                                                         const CloseConnection: Boolean = False);
var LTmpCloseConnection: Boolean;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');
  if not assigned(StatementPool) then raise exception.Create('Statement pool object can not be null');

  //rollback the connection
  LTmpCloseConnection := CloseConnection;
  Try
    Try
      FLibrary.TransactionRollback(TraHandle);
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
    TraHandle := nil;
    ReleaseConnectionWithStmt(DBHandle,
                              StatementPool,
                              LTmpCloseConnection);

  End;

end;

{******************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionStart(Var DBHandle: IscDbHandle;
                                                      var TraHandle: IscTrHandle;
                                                      const TPB: AnsiString);
begin

  //DBHandle, TraHandle and StatementPool must be null
  if assigned(DBHandle) then raise exception.Create('Connection handle must be null');
  if assigned(TraHandle) then raise exception.Create('Transaction handle must be null');

  //init the aConnectionHandle
  DBHandle := AcquireConnectionWithoutStmt;
  try

    //Start the transaction
    Flibrary.TransactionStart(TraHandle,
                              DBHandle,
                              TPB);

  except
    ReleaseConnectionWithoutStmt(DBHandle, True);
    TraHandle := nil;
    raise;
  end;

end;

{*******************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionCommit(var DBHandle: IscDbHandle;
                                                       var TraHandle: IscTrHandle;
                                                       const CloseConnection: Boolean = False);
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');

  //commit the transaction
  FLibrary.TransactionCommit(TraHandle);
  TraHandle := nil;

  //release the connection
  ReleaseConnectionWithoutStmt(DBHandle, CloseConnection);

end;

{*********************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionRollback(var DBHandle: IscDbHandle;
                                                         var TraHandle: IscTrHandle;
                                                         const CloseConnection: Boolean = False);
var LTmpCloseConnection: Boolean;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');

  //rollback the connection
  LTmpCloseConnection := CloseConnection;
  Try
    Try
      FLibrary.TransactionRollback(TraHandle);
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
    TraHandle := nil;
    ReleaseConnectionWithoutStmt(DBHandle, LTmpCloseConnection);

  End;

end;

{**************************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionCommitRetaining(TraHandle: IscTrHandle);
begin

  //security check
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');

  //commit the transaction
  FLibrary.TransactionCommitRetaining(TraHandle);

end;

{****************************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionRollbackRetaining(TraHandle: IscTrHandle);
begin

  //security check
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');

  //rollback the connection
  FLibrary.TransactionRollbackRetaining(TraHandle);

end;

{****************************************************************}
function TALFBXConnectionPoolClient.Prepare(const SQL: AnsiString;
                                            Var DBHandle: IscDbHandle;
                                            var TraHandle: IscTrHandle;
                                            var StmtHandle: IscStmtHandle;
                                            var Sqlda: TALFBXSQLResult;
                                            const TPB: AnsiString = ''): TALFBXStatementType;
Var LReleaseConnectionHandleonError: Boolean;
begin

  //check the params
  if (
      (not assigned(DBHandle)) and
      (assigned(TraHandle))
     )
     or
     (
      (assigned(DBHandle)) and
      (not assigned(TraHandle))
     )
     or
     (assigned(StmtHandle))
     or
     (assigned(Sqlda))
     or
     (TPB = '')
  then raise Exception.Create('Wrong Params');

  //init the DBHandle and TraHandle
  if not assigned(DBHandle) then begin
    TransactionStart(DBHandle,
                     TraHandle,
                     TPB);
    LReleaseConnectionHandleonError := True;
  end
  else LReleaseConnectionHandleonError := False;
  try

    //create the sqlda result
    Sqlda := TALFBXSQLResult.Create(fCharSet);
    Try

      //init the aStmtHandle
      Flibrary.DSQLAllocateStatement(DBHandle, StmtHandle);
      try

        //prepare the SQL
        Result := Flibrary.DSQLPrepare(DBHandle, TraHandle, StmtHandle, SQL, FSQLDIALECT, Sqlda);

      except

        try
          Flibrary.DSQLFreeStatement(StmtHandle, DSQL_drop);
        Except
          //to not hide the original exception
        end;

        raise;

      end;

    Except
      StmtHandle := nil;
      Sqlda.free;
      Sqlda := nil;
      raise;
    end;

  except
    if LReleaseConnectionHandleonError then TransactionRollBack(DBHandle,
                                                                TraHandle,
                                                                true); // const CloseConnection: Boolean = False
    raise;
  end;

end;

{*********************************************************************************************}
procedure TALFBXConnectionPoolClient.OnSelectDataDone(const Query: TALFBXClientSelectDataQUERY;
                                                      TimeTaken: double);
begin
  // virtual
end;

{*********************************************************************************************}
procedure TALFBXConnectionPoolClient.OnUpdateDataDone(const Query: TALFBXClientUpdateDataQUERY;
                                                      TimeTaken: double);
begin
  // virtual
end;

{*******************************************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const Queries: TALFBXClientSelectDataQUERIES;
                                                XMLDATA: TalXMLNode;
                                                OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                                                ExtData: Pointer;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');

  {$REGION 'Var Declarations'}
  Var LSqlpa: TALFBXSQLParams;
      LParamsIndex: integer;
      LBlobhandle: IscBlobHandle;
      LQueriesIndex: integer;
      LViewRec: TalXmlNode;
      LXmlDocument: TalXmlDocument;
      LTmpDBHandle: IscDbHandle;
      LTmpTraHandle: IscTrHandle;
      LTmpStmtHandle: IscStmtHandle;
      LTmpSqlda: TALFBXSQLResult;
      LTmpStatementPoolNode: TALStringKeyAVLBinaryTreeNode;
      LTmpTPB: AnsiString;
      LGDSCode: integer;
      LReadOnlyReadCommitedTPB: Boolean;
      LConnectionWasAcquiredFromPool: Boolean;
      LTransactionWasAcquiredFromPool: Boolean;
      LStatementWasAcquiredFromPool: Boolean;
      LStopWatch: TStopWatch;
      LCacheKey: ansiString;
      LCacheStr: ansiString;
  {$ENDREGION}

  {$REGION 'InternalSelectDataFetchRows'}
  Procedure InternalSelectDataFetchRows;
  var LColumnIndex: integer;
      LRecIndex: integer;
      LRecAdded: integer;
      LContinue: Boolean;
      LNewRec: TalXmlNode;
      LValueRec: TalXmlNode;
      LUpdateRowTagByFieldValue: Boolean;
  Begin

    //init the aViewRec
    if (Queries[LQueriesIndex].ViewTag <> '') and (not assigned(LXmlDocument)) then LViewRec := XMLdata.AddChild(Queries[LQueriesIndex].ViewTag)
    else LViewRec := XMLdata;

    //init aUpdateRowTagByFieldValue
    if AlPos('&>',Queries[LQueriesIndex].RowTag) = 1 then begin
      delete(Queries[LQueriesIndex].RowTag, 1, 2);
      LUpdateRowTagByFieldValue := Queries[LQueriesIndex].RowTag <> '';
    end
    else LUpdateRowTagByFieldValue := False;

    //retrieve all row
    LRecIndex := 0;
    LRecAdded := 0;
    while Flibrary.DSQLFetch(LTmpDBHandle, LTmpTraHandle, LTmpStmtHandle, FSQLDIALECT, LTmpSqlda) do begin

      //process if > Skip
      inc(LRecIndex);
      If LRecIndex > Queries[LQueriesIndex].Skip then begin

        //init NewRec
        if (Queries[LQueriesIndex].RowTag <> '') and (not assigned(LXmlDocument)) then LNewRec := LViewRec.AddChild(Queries[LQueriesIndex].RowTag)
        Else LNewRec := LViewRec;

        //loop throught all column
        For LColumnIndex := 0 to LTmpSqlda.FieldCount - 1 do begin
          LValueRec := LNewRec.AddChild(ALlowercase(LTmpSqlda.AliasName[LColumnIndex]));
          if (LTmpSqlda.SQLType[LColumnIndex] = SQL_BLOB) then LValueRec.ChildNodes.Add(
                                                                                        LValueRec.OwnerDocument.CreateNode(
                                                                                                                           GetFieldValue(LTmpSqlda,
                                                                                                                                         LTmpDBHandle,
                                                                                                                                         LTmpTraHandle,
                                                                                                                                         LColumnIndex,
                                                                                                                                         FormatSettings),
                                                                                                                           ntCData
                                                                                                                          )
                                                                                        )
          else LValueRec.Text := GetFieldValue(LTmpSqlda,
                                               LTmpDBHandle,
                                               LTmpTraHandle,
                                               LColumnIndex,
                                               FormatSettings);
          if LUpdateRowTagByFieldValue and (LValueRec.NodeName=LNewRec.NodeName) then LNewRec.NodeName := ALLowerCase(LValueRec.Text);
        end;

        //handle OnNewRowFunct
        if assigned(OnNewRowFunct) then begin
          LContinue := True;
          OnNewRowFunct(LNewRec, Queries[LQueriesIndex].ViewTag, ExtData, LContinue);
          if Not LContinue then Break;
        end;

        //free the node if aXmlDocument
        if assigned(LXmlDocument) then LXmlDocument.DocumentElement.ChildNodes.Clear;

        //handle the First
        inc(LRecAdded);
        If (Queries[LQueriesIndex].First > 0) and (LRecAdded >= Queries[LQueriesIndex].First) then Break;

      end;

    end;

  end;
  {$ENDREGION}

begin

  {$REGION 'exit if no SQL'}
  if length(Queries) = 0 then Exit;
  {$ENDREGION}

  {$REGION 'Check the params'}
  if (
      (not assigned(DBHandle)) and
      (assigned(TraHandle))
     )
     or
     (
      (assigned(DBHandle)) and
      (not assigned(TraHandle))
     )
     or
     (
      (assigned(StatementPool)) and
      (not assigned(DBHandle))
     )
  then raise Exception.Create('Wrong Params');
  {$ENDREGION}

  {$REGION 'init OnNewRowFunct/XMLDATA'}
  if assigned(OnNewRowFunct) then XMLDATA := nil;
  {$ENDREGION}

  {$REGION 'clear the XMLDATA'}
  if assigned(XMLDATA) then LXmlDocument := Nil
  else begin
    LXmlDocument := TALXmlDocument.create('root');
    XMLDATA := LXmlDocument.DocumentElement;
  end;
  {$ENDREGION}

  Try

    {$REGION 'init local variable'}
    LTmpDBHandle := DBHandle;
    LTmpTraHandle := TraHandle;
    LTmpStmtHandle := nil;
    LTmpSqlda := nil;
    LTmpTPB := TPB;
    if LTmpTPB = '' then LTmpTPB := fDefaultReadTPB;
    LReadOnlyReadCommitedTPB := (alpos(isc_tpb_read_committed,LTmpTPB) > 0) and
                                (alpos(isc_tpb_read, LTmpTPB) > 0) and
                                (alpos(isc_tpb_write, LTmpTPB) <= 0);
    LConnectionWasAcquiredFromPool := False;
    LTransactionWasAcquiredFromPool := False;
    LStatementWasAcquiredFromPool := False;
    {$ENDREGION}

    Try

      {$REGION 'init aStopWatch'}
      LStopWatch := TstopWatch.Create;
      {$ENDREGION}

      {$REGION 'loop on all the SQL'}
      For LQueriesIndex := 0 to length(Queries) - 1 do begin

        {$REGION 'Handle the CacheThreshold'}
        LCacheKey := '';
        If (Queries[LQueriesIndex].CacheThreshold > 0) and
           (not assigned(LXmlDocument)) and
           (((length(Queries) = 1) and
             (XMLdata.ChildNodes.Count = 0)) or  // else the save will not work
            (Queries[LQueriesIndex].ViewTag <> '')) then begin

          //try to load from from cache
          LCacheKey := ALStringHashSHA1(Queries[LQueriesIndex].RowTag + '#' +
                                        alinttostr(Queries[LQueriesIndex].Skip) + '#' +
                                        alinttostr(Queries[LQueriesIndex].First) + '#' +
                                        ALGetFormatSettingsID(FormatSettings) + '#' +
                                        Queries[LQueriesIndex].SQL);
          if loadcachedData(LCacheKey, LCacheStr) then begin

            //init the aViewRec
            if (Queries[LQueriesIndex].ViewTag <> '') then LViewRec := XMLdata.AddChild(Queries[LQueriesIndex].ViewTag)
            else LViewRec := XMLdata;

            //assign the tmp data to the XMLData
            LViewRec.LoadFromXML(LCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

            //go to the next loop
            continue;

          end;

        end;
        {$ENDREGION}

        {$REGION 'Reset aStopWatch'}
        LStopWatch.Reset;
        LStopWatch.Start;
        {$ENDREGION}

        {$REGION 'Create/acquire the connection/transaction/statement'}

        {$REGION 'init aTmpStatementPoolNode'}
        if assigned(StatementPool) then LTmpStatementPoolNode := StatementPool.FindNode(Queries[LQueriesIndex].SQL)
        else LTmpStatementPoolNode := nil;
        {$ENDREGION}

        {$REGION 'if aTmpStatementPoolNode is null'}
        if not assigned(LTmpStatementPoolNode) then begin

          {$REGION 'if it''s a readonly Read Commited TPB AND DBHandle = nil'}
          if (LReadOnlyReadCommitedTPB) and
             (not assigned(DBHandle)) then begin

            //not assigned(DBHandle) mean that the aTmpDBHandle and aTmpTraHandle
            //are nil or assigned in previous loop only throught
            //AcquireReadTransaction or AcquireReadStatement (but if via AcquireReadStatement
            //then it's was already releasead ad the end of the current loop)

            {$REGION 'if their is some params'}
            if length(Queries[LQueriesIndex].Params) > 0 then begin

              //release previous connection if need
              if LTransactionWasAcquiredFromPool then begin
                ReleaseReadTransaction(LTmpDBHandle,
                                       LTmpTraHandle,
                                       LTmpTPB);
                LTransactionWasAcquiredFromPool := False;
              end;

              //acquire a statement from the pool
              AcquireReadStatement(Queries[LQueriesIndex].SQL,
                                   LTmpDBHandle,
                                   LTmpTraHandle,
                                   LTmpStmtHandle,
                                   LTmpSqlda,
                                   LTmpTPB);
              LStatementWasAcquiredFromPool := True;

            end
            {$ENDREGION}

            {$REGION 'if their is NO param'}
            else begin

              //acquire a transaction in the pool
              if not LTransactionWasAcquiredFromPool then begin
                AcquireReadTransaction(LTmpDBHandle,
                                       LTmpTraHandle,
                                       LTmpTPB);
                LTransactionWasAcquiredFromPool := True;
              end;

              //Prepare the statement
              Prepare(Queries[LQueriesIndex].SQL,
                      LTmpDBHandle,
                      LTmpTraHandle,
                      LTmpStmtHandle,
                      LTmpSqlda,
                      LTmpTPB);
              LStatementWasAcquiredFromPool := False;

            end;
            {$ENDREGION}

          end
          {$ENDREGION}

          {$REGION 'if it''s NOT a readonly Read Commited TPB or DBHandle <> nil'}
          else begin

            //assigned(DBHandle) or NOT a readonly Read Commited TPB mean that the aTmpDBHandle and aTmpTraHandle
            //are nil or given via the params or assigned in previous loop only throught TransactionStart

            //Start the transaction
            if not assigned(LTmpDBHandle) then begin
              TransactionStart(LTmpDBHandle,
                               LTmpTraHandle,
                               LTmpTPB);
              LConnectionWasAcquiredFromPool := True;
            end;

            //prepare the statement
            Prepare(Queries[LQueriesIndex].SQL,
                    LTmpDBHandle,
                    LTmpTraHandle,
                    LTmpStmtHandle,
                    LTmpSqlda,
                    LTmpTPB);
            LStatementWasAcquiredFromPool := False;

          end;
          {$ENDREGION}

        end
        {$ENDREGION}

        {$REGION 'else if aTmpStatementPoolNode is NOT null'}
        else begin

          //if aTmpStatementPoolNode is NOT null then DBHandle and TraHandle are not null too
          //so just init the aTmpStmtHandle and aTmpSqlda from aTmpStatementPoolNode

          with TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode) do begin
            LTmpStmtHandle := StmtHandle;
            LTmpSqlda := Sqlda;
          end;

        end;
        {$ENDREGION}

        {$ENDREGION}

        try

          {$REGION 'Execute the statement'}

          {$REGION 'if their is params'}
          if length(Queries[LQueriesIndex].Params) > 0 then begin

            //create the aSqlpa object
            LSqlpa := TALFBXSQLParams.Create(fCharSet);

            try

              //loop throught all Params Fields
              for LParamsIndex := 0 to length(Queries[LQueriesIndex].Params) - 1 do begin

                //with current Params Fields
                with Queries[LQueriesIndex].Params[LParamsIndex] do begin

                  //isnull
                  if IsNull then begin
                    LSqlpa.AddFieldType('', uftVarchar);
                    LSqlpa.IsNull[LParamsIndex] := True;
                  end

                  //IsBlob
                  else if length(value) > high(smallint) then begin
                    LSqlpa.AddFieldType('', uftBlob);
                    LBlobhandle := nil;
                    LSqlpa.AsQuad[LParamsIndex] := Flibrary.BlobCreate(LTmpDBHandle,LTmpTraHandle,LBlobhandle);
                    Try
                      FLibrary.BlobWriteString(LBlobhandle,Value);
                    Finally
                      FLibrary.BlobClose(LBlobhandle);
                    End;
                  end

                  //all the other
                  else begin
                    LSqlpa.AddFieldType('', uftVarchar);
                    LSqlpa.AsAnsiString[LParamsIndex] := Value;
                  end;

                end;

              end;

              //execute the sql with the params
              FLibrary.DSQLExecute(LTmpTraHandle, LTmpStmtHandle, FSQLDIALECT, LSqlpa);

              //fetch the rows
              InternalSelectDataFetchRows;

            finally
              LSqlpa.free;
            end;

          end
          {$ENDREGION}

          {$REGION 'if their is NO params'}
          else begin

            //execute the SQL wihtout params
            FLibrary.DSQLExecute(LTmpTraHandle, LTmpStmtHandle, FSQLDIALECT, nil);

            //fetch the rows
            InternalSelectDataFetchRows;

          end;
          {$ENDREGION}

          {$ENDREGION}

        finally

          {$REGION 'drop/close/release the statement'}

          {$REGION 'If NO StatementPool was given in params'}
          if not assigned(StatementPool) then begin

            {$REGION 'if the statement was acquired from the pool - CLOSE IT and RELEASE IT'}
            if LStatementWasAcquiredFromPool then begin

              try

                //close the cursor
                Flibrary.DSQLFreeStatement(LTmpStmtHandle, DSQL_Close);

                //release the statement in the pool
                ReleaseReadStatement(Queries[LQueriesIndex].SQL,
                                     LTmpDBHandle,
                                     LTmpTraHandle,
                                     LTmpStmtHandle,
                                     LTmpSqlda,
                                     LTmpTPB);

              Except

                //free the statement in the pool
                ReleaseReadStatement(Queries[LQueriesIndex].SQL,
                                     LTmpDBHandle,
                                     LTmpTraHandle,
                                     LTmpStmtHandle,
                                     LTmpSqlda,
                                     LTmpTPB,
                                     true); // CloseConnection

              end;

            end
            {$ENDREGION}

            {$REGION 'else if the statement was NOT acquired from the pool - DROP IT'}
            else begin

              try
                Flibrary.DSQLFreeStatement(LTmpStmtHandle, DSQL_drop);
              Except
                //what else we can do here ?
                //this can happen if connection lost for exemple
                //i preferre to hide this exception to not hide previous exception
              end;
              LTmpSqlda.free;

            end;
            {$ENDREGION}

          end
          {$ENDREGION}

          {$REGION 'Else if a statementPool was given in the params'}
          else begin

            //add to the statement pool only if their is some params or
            //if assigned aTmpStatementPoolNode
            if (length(Queries[LQueriesIndex].Params) > 0) or
               (assigned(LTmpStatementPoolNode)) then begin

              //first close the statement
              try
                Flibrary.DSQLFreeStatement(LTmpStmtHandle, DSQL_close);
              Except
                //what else we can do here ?
                //this can happen if connection lost for exemple
                //i preferre to hide this exception to not hide previous exception
              end;

              //if the statement was already in the pool, simply update it LastAccessDate
              if assigned(LTmpStatementPoolNode) then TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode).LastAccessDate := GetTickCount64

              //else add it to the statement pool
              else begin
                LTmpStatementPoolNode := TALFBXConnectionStatementPoolBinTreeNode.Create;
                LTmpStatementPoolNode.ID := Queries[LQueriesIndex].SQL;
                TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode).Lib := Lib;
                TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode).StmtHandle := LTmpStmtHandle;
                TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode).Sqlda := LTmpSqlda;
                if not StatementPool.AddNode(LTmpStatementPoolNode) then LTmpStatementPoolNode.Free; // don't raise any exception to not hide previous error
              end;

            end

            //else drop the statements handle
            else begin

              try
                Flibrary.DSQLFreeStatement(LTmpStmtHandle, DSQL_drop);
              Except
                //what else we can do here ?
                //this can happen if connection lost for exemple
                //i preferre to hide this exception to not hide previous exception
              end;
              LTmpSqlda.free;

            end;

          end;
          {$ENDREGION}

          {$REGION 'nil aTmpStmtHandle and aTmpSqlda'}
          LTmpStmtHandle := nil;
          LTmpSqlda := nil;
          {$ENDREGION}

          {$ENDREGION}

        end;

        {$REGION 'do the OnSelectDataDone'}
        LStopWatch.Stop;
        OnSelectDataDone(Queries[LQueriesIndex],
                         LStopWatch.Elapsed.TotalMilliseconds);
        {$ENDREGION}

        {$REGION 'save to the cache'}
        If LCacheKey <> '' then begin

          //save the data
          LViewRec.SaveToXML(LCacheStr, true{SaveOnlyChildNodes});
          SaveDataToCache(LCacheKey,
                          Queries[LQueriesIndex].CacheThreshold,
                          LCacheStr);

        end;
        {$ENDREGION}

      End;
      {$ENDREGION}

      {$REGION 'Commit or release the transaction'}
      if LTransactionWasAcquiredFromPool then ReleaseReadTransaction(LTmpDBHandle,
                                                                     LTmpTraHandle,
                                                                     LTmpTPB)
      else if LConnectionWasAcquiredFromPool then TransactionCommit(LTmpDBHandle,
                                                                    LTmpTraHandle);
      {$ENDREGION}

    Except

      {$REGION 'On Exception'}
      On E: Exception do begin

        {get the gdscode}
        if E is EALFBXError then LGDSCode := (E as EALFBXError).GDSCode
        else LGDSCode := -1;

        //rollback the transaction and release the connection if owned
        if LTransactionWasAcquiredFromPool then ReleaseReadTransaction(LTmpDBHandle,
                                                                       LTmpTraHandle,
                                                                       LTmpTPB,
                                                                       GetCloseConnectionByErrCode(LGDSCode))
        else if LConnectionWasAcquiredFromPool then TransactionRollback(LTmpDBHandle,
                                                                        LTmpTraHandle,
                                                                        GetCloseConnectionByErrCode(LGDSCode));

        //Database @1 shutdown
        if (LGDSCode = isc_shutdown) or        // Database @1 shutdown
           (LGDSCode = isc_shutinprog)         // Database @1 shutdown in progress
        then ReleaseAllConnections(False);

        //raise the error
        raise;

      end;
      {$ENDREGION}

    End;

  Finally

    {$REGION 'Free aXmlDocument'}
    if assigned(LXmlDocument) then LXmlDocument.free;
    {$ENDREGION}

  End;

end;

{***************************************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const Query: TALFBXClientSelectDataQUERY;
                                                OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                                                ExtData: Pointer;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := Query;
  SelectData(LSelectDataQUERIES,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                Skip: integer;
                                                First: Integer;
                                                OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                                                ExtData: Pointer;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  LSelectDataQUERIES[0].skip := Skip;
  LSelectDataQUERIES[0].First := First;
  SelectData(LSelectDataQUERIES,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                                                ExtData: Pointer;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  SelectData(LSelectDataQUERIES,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{*******************************************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const Queries: TALFBXClientSelectDataQUERIES;
                                                XMLDATA: TalXMLNode;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
begin

  SelectData(Queries,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);

end;

{***************************************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const Query: TALFBXClientSelectDataQUERY;
                                                XMLDATA: TalXMLNode;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := Query;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                const RowTag: AnsiString;
                                                Skip: integer;
                                                First: Integer;
                                                XMLDATA: TalXMLNode;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  LSelectDataQUERIES[0].RowTag := RowTag;
  LSelectDataQUERIES[0].skip := Skip;
  LSelectDataQUERIES[0].First := First;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                const RowTag: AnsiString;
                                                XMLDATA: TalXMLNode;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  LSelectDataQUERIES[0].RowTag := RowTag;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                XMLDATA: TalXMLNode;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                const Params: array of AnsiString;
                                                const RowTag: AnsiString;
                                                Skip: integer;
                                                First: Integer;
                                                XMLDATA: TalXMLNode;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
    I: integer;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  setlength(LSelectDataQUERIES[0].params, length(Params));
  for I := 0 to length(Params) - 1 do begin
    LSelectDataQUERIES[0].params[I] := TALFBXClientSQLParam.Create;
    LSelectDataQUERIES[0].params[I].Value := Params[I];
    LSelectDataQUERIES[0].params[I].IsNull := Params[I] = fNullString;
  end;
  LSelectDataQUERIES[0].RowTag := RowTag;
  LSelectDataQUERIES[0].skip := Skip;
  LSelectDataQUERIES[0].First := First;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                const Params: array of AnsiString;
                                                const RowTag: AnsiString;
                                                XMLDATA: TalXMLNode;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
    I: integer;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  setlength(LSelectDataQUERIES[0].params, length(Params));
  for I := 0 to length(Params) - 1 do begin
    LSelectDataQUERIES[0].params[I] := TALFBXClientSQLParam.Create;
    LSelectDataQUERIES[0].params[I].Value := Params[I];
    LSelectDataQUERIES[0].params[I].IsNull := Params[I] = fNullString;
  end;
  LSelectDataQUERIES[0].RowTag := RowTag;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                const Params: array of AnsiString;
                                                XMLDATA: TalXMLNode;
                                                const FormatSettings: TALFormatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
var LSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
    I: integer;
begin
  setlength(LSelectDataQUERIES,1);
  LSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  LSelectDataQUERIES[0].Sql := Sql;
  setlength(LSelectDataQUERIES[0].params, length(Params));
  for I := 0 to length(Params) - 1 do begin
    LSelectDataQUERIES[0].params[I] := TALFBXClientSQLParam.Create;
    LSelectDataQUERIES[0].params[I].Value := Params[I];
    LSelectDataQUERIES[0].params[I].IsNull := Params[I] = fNullString;
  end;
  SelectData(LSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{*******************************************************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(const Queries: TALFBXClientUpdateDataQUERIES;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');

  {$REGION 'Var Declarations'}
  Var LSqlpa: TALFBXSQLParams;
      LBlobhandle: IscBlobHandle;
      LParamsIndex: integer;
      LQueriesIndex: integer;
      LTmpDBHandle: IscDbHandle;
      LTmpTraHandle: IscTrHandle;
      LTmpStmtHandle: IscStmtHandle;
      LTmpSqlda: TALFBXSQLResult;
      LTmpStatementPool: TALFBXConnectionStatementPoolBinTree;
      LTmpStatementPoolNode: TALStringKeyAVLBinaryTreeNode;
      LTmpTPB: AnsiString;
      LGDSCode: integer;
      LConnectionWasAcquiredFromPool: Boolean;
      LStopWatch: TStopWatch;
  {$ENDREGION}

begin

  {$REGION 'Exit if no SQL'}
  if length(Queries) = 0 then Exit;
  {$ENDREGION}

  {$REGION 'Check the params'}
  if (
      (not assigned(DBHandle)) and
      (assigned(TraHandle))
     )
     or
     (
      (assigned(DBHandle)) and
      (not assigned(TraHandle))
     )
     or
     (
      (assigned(StatementPool)) and
      (not assigned(DBHandle))
     )
  then raise Exception.Create('Wrong Params');
  {$ENDREGION}

  {$REGION 'init local variable'}
  LTmpDBHandle := DBHandle;
  LTmpTraHandle := TraHandle;
  LTmpStmtHandle := nil;
  LTmpSqlda := nil;
  LTmpTPB := TPB;
  if LTmpTPB = '' then LTmpTPB := fDefaultWriteTPB;
  LTmpStatementPool := nil;
  if not assigned(LTmpDBHandle) then begin
    TransactionStart(LTmpDBHandle,
                     LTmpTraHandle,
                     LTmpStatementPool,
                     TPB);
    LConnectionWasAcquiredFromPool := true;
  end
  else begin
    LTmpStatementPool := StatementPool;
    LConnectionWasAcquiredFromPool := False;
  end;
  {$ENDREGION}

  try

    {$REGION 'init aStopWatch'}
    LStopWatch := TstopWatch.Create;
    {$ENDREGION}

    {$REGION 'loop on all the SQL'}
    For LQueriesIndex := 0 to length(Queries) - 1 do begin

      {$REGION 'Reset aStopWatch'}
      LStopWatch.Reset;
      LStopWatch.Start;
      {$ENDREGION}

      {$REGION 'init aTmpStatementPoolNode'}
      if assigned(LTmpStatementPool) then LTmpStatementPoolNode := LTmpStatementPool.FindNode(Queries[LQueriesIndex].SQL)
      else LTmpStatementPoolNode := nil;
      {$ENDREGION}

      {$REGION 'if their is params or aTmpStatementPoolNode <> nil'}
      if (length(Queries[LQueriesIndex].Params) > 0) or
         (assigned(LTmpStatementPoolNode)) then begin

        {$REGION 'Prepare or acquire the statement'}
        //if aTmpStatementPoolNode found
        if assigned(LTmpStatementPoolNode) then begin
          with TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode) do begin
            LTmpStmtHandle := StmtHandle;
            LTmpSqlda := Sqlda;
          end;
        end

        //if aTmpStatementPoolNode was not founded (prepare the statement)
        else begin
          Prepare(Queries[LQueriesIndex].SQL,
                  LTmpDBHandle,          //not nil => will not change
                  LTmpTraHandle,         //not nil => will not change
                  LTmpStmtHandle,        //nil => will be assigned
                  LTmpSqlda,             //nil => will be assigned
                  LTmpTPB);
        end;
       {$ENDREGION}

        try

          {$REGION 'Execute the statement'}
          //create the aSqlpa object
          LSqlpa := TALFBXSQLParams.Create(fCharSet);
          try

            //loop throught all Params Fields
            for LParamsIndex := 0 to length(Queries[LQueriesIndex].Params) - 1 do begin

              //with current Params Fields
              with Queries[LQueriesIndex].Params[LParamsIndex] do begin

                //isnull
                if IsNull then begin
                  LSqlpa.AddFieldType('', uftVarchar);
                  LSqlpa.IsNull[LParamsIndex] := True;
                end

                //IsBlob
                else if length(value) > high(smallint) then begin
                  LSqlpa.AddFieldType('', uftBlob);
                  LBlobhandle := nil;
                  LSqlpa.AsQuad[LParamsIndex] := Flibrary.BlobCreate(LTmpDBHandle,LTmpTraHandle,LBlobhandle);
                  Try
                    FLibrary.BlobWriteString(LBlobhandle,Value);
                  Finally
                    FLibrary.BlobClose(LBlobhandle);
                  End;
                end

                //all the other
                else begin
                  LSqlpa.AddFieldType('', uftVarchar);
                  LSqlpa.AsAnsiString[LParamsIndex] := Value;
                end;

              end;

            end;

            //execute the SQL
            FLibrary.DSQLExecute(LTmpTraHandle, LTmpStmtHandle, FSQLDIALECT, LSqlpa);

          finally
            LSqlpa.free;
          end;
         {$ENDREGION}

        finally

          {$REGION 'drop or pool the statement'}

          //if the statement was already in the pool, simply update it LastAccessDate
          if assigned(LTmpStatementPoolNode) then TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode).LastAccessDate := GetTickCount64

          //else add it to the statement pool OR DROP it
          else begin

            //if their is a StatementPool then add a new node with the prepared statement
            if assigned(LTmpStatementPool) then begin
              LTmpStatementPoolNode := TALFBXConnectionStatementPoolBinTreeNode.Create;
              LTmpStatementPoolNode.ID := Queries[LQueriesIndex].SQL;
              TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode).Lib := Lib;
              TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode).StmtHandle := LTmpStmtHandle;
              TALFBXConnectionStatementPoolBinTreeNode(LTmpStatementPoolNode).Sqlda := LTmpSqlda;
              if not LTmpStatementPool.AddNode(LTmpStatementPoolNode) then LTmpStatementPoolNode.Free; // don't raise any exception to not hide previous error
            end

            //if their is NO StatementPool then DROP the prepared statement
            else begin
              try
                Flibrary.DSQLFreeStatement(LTmpStmtHandle, DSQL_drop);
              Except
                //what else we can do here ?
                //this can happen if connection lost for exemple
                //i preferre to hide this exception to not hide previous exception
              end;
              LTmpSqlda.free;
            end;

          end;

          LTmpStmtHandle := nil;
          LTmpSqlda := nil;
          {$ENDREGION}

        end;

      end
      {$ENDREGION}

      {$REGION 'if their is NO params and aTmpStatementPoolNode = nil'}
      else begin

        //simply call DSQLExecuteImmediate
        Flibrary.DSQLExecuteImmediate(LTmpDBHandle, LTmpTraHandle, Queries[LQueriesIndex].SQL, FSQLDIALECT, nil);

      end;
      {$ENDREGION}

      {$REGION 'do the OnUpdateDataDone'}
      LStopWatch.Stop;
      OnUpdateDataDone(Queries[LQueriesIndex],
                       LStopWatch.Elapsed.TotalMilliseconds);
      {$ENDREGION}

    end;
    {$ENDREGION}

    {$REGION 'Commit the transaction'}
    if LConnectionWasAcquiredFromPool then begin
      TransactionCommit(LTmpDBHandle,
                        LTmpTraHandle,
                        LTmpStatementPool);
    end;
    {$ENDREGION}

  Except

    {$REGION 'On Exception'}
    On E: Exception do begin

      {get the gdscode}
      if E is EALFBXError then LGDSCode := (E as EALFBXError).GDSCode
      else LGDSCode := -1;

      //rollback the transaction and release the connection if owned
      if LConnectionWasAcquiredFromPool then begin
        TransactionRollback(LTmpDBHandle,
                            LTmpTraHandle,
                            LTmpStatementPool,
                            GetCloseConnectionByErrCode(LGDSCode)); // const CloseConnection: Boolean = False
      end;

      //Database @1 shutdown
      if (LGDSCode = isc_shutdown) or        // Database @1 shutdown
         (LGDSCode = isc_shutinprog)         // Database @1 shutdown in progress
      then ReleaseAllConnections(False);

      //raise the error
      raise;

    end;
    {$ENDREGION}

  end;

end;

{***************************************************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(const Query: TALFBXClientUpdateDataQUERY;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
Var LUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
begin
  setlength(LUpdateDataQUERIES,1);
  LUpdateDataQUERIES[0] := Query;
  UpdateData(LUpdateDataQUERIES,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{***************************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(SQLs: TALStrings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
Var LSQLsIndex : integer;
    LUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
begin
  setlength(LUpdateDataQUERIES,SQLs.Count);
  For LSQLsIndex := 0 to SQLs.Count - 1 do begin
    LUpdateDataQUERIES[LSQLsIndex] := TALFBXClientUpdateDataQUERY.Create;
    LUpdateDataQUERIES[LSQLsIndex].SQL := SQLs[LSQLsIndex];
  end;
  UpdateData(LUpdateDataQUERIES,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(const SQL: AnsiString;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
Var LUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
begin
  setlength(LUpdateDataQUERIES,1);
  LUpdateDataQUERIES[0] := TALFBXClientUpdateDataQUERY.Create;
  LUpdateDataQUERIES[0].SQL := SQL;
  UpdateData(LUpdateDataQUERIES,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{********************************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(const SQL: AnsiString;
                                                const Params: Array of AnsiString;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
Var LUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
    i: integer;
begin
  setlength(LUpdateDataQUERIES,1);
  LUpdateDataQUERIES[0] := TALFBXClientUpdateDataQUERY.Create;
  LUpdateDataQUERIES[0].SQL := SQL;
  setlength(LUpdateDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    LUpdateDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    LUpdateDataQUERIES[0].params[i].Value := Params[i];
    LUpdateDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  UpdateData(LUpdateDataQUERIES,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{******************************************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(const SQLs: array of AnsiString;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
Var LUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
    i: integer;
begin
  setlength(LUpdateDataQUERIES,length(SQLs));
  for I := 0 to length(SQLs) - 1 do begin
    LUpdateDataQUERIES[i] := TALFBXClientUpdateDataQUERY.Create;
    LUpdateDataQUERIES[i].SQL := SQLs[i];
  end;
  UpdateData(LUpdateDataQUERIES,
             DBHandle,
             TraHandle,
             StatementPool,
             TPB);
end;

{***********************************************************}
function TALFBXConnectionPoolClient.ConnectionCount: Integer;
begin
  FConnectionWithStmtPoolCS.Acquire;
  try
    FConnectionWithoutStmtPoolCS.Acquire;
    Try
      Result := FConnectionWithStmtPool.Count    + FWorkingConnectionWithStmtCount +
                FConnectionWithoutStmtPool.Count + FWorkingConnectionWithoutStmtCount;
    finally
      FConnectionWithoutStmtPoolCS.Release;
    end;
  finally
    FConnectionWithStmtPoolCS.Release;
  end;
end;

{******************************************************************}
function TALFBXConnectionPoolClient.WorkingConnectionCount: Integer;
begin
  FConnectionWithStmtPoolCS.Acquire;
  try
    FConnectionWithoutStmtPoolCS.Acquire;
    Try
      Result := FWorkingConnectionWithStmtCount +
                FWorkingConnectionWithoutStmtCount;
    finally
      FConnectionWithoutStmtPoolCS.Release;
    end;
  finally
    FConnectionWithStmtPoolCS.Release;
  end;
end;

{***********************************************************************}
function TALFBXConnectionPoolClient.loadCachedData(const Key: AnsiString;
                                                   var DataStr: AnsiString): Boolean;
begin
  result := false; //virtual need to be overriden
end;

{*************************************************************************}
Procedure TALFBXConnectionPoolClient.SaveDataToCache(const Key: ansiString;
                                                     const CacheThreshold: integer;
                                                     const DataStr: ansiString);
begin
  //virtual need to be overriden
end;

{******************************************************************************************}
function TALFBXConnectionPoolClient.GetCloseConnectionByErrCode(aGDSCode: Integer): Boolean;
begin
  result := (aGDSCode <> isc_bad_segstr_handle) and     // Invalid BLOB handle
            (aGDSCode <> isc_bad_segstr_id) and         // Invalid BLOB ID
            (aGDSCode <> isc_deadlock) and              // Deadlock
            (aGDSCode <> isc_foreign_key) and           // Violation of FOREIGN KEY constraint "@1" on table "@2"
            (aGDSCode <> isc_invalid_array_id) and      // Invalid blob id
            (aGDSCode <> isc_lock_conflict) and         // Lock conflict on no wait transaction
            (aGDSCode <> isc_lock_timeout) and          // Lock time-out on wait transaction
            (aGDSCode <> isc_no_dup) and                // Attempt to store duplicate value (visible to active transactions) in unique index "@1"
            (aGDSCode <> isc_unique_key_violation) and  // Violation of PRIMARY or UNIQUE KEY constraint "@1" on table "@2"
            (aGDSCode <> isc_update_conflict);          // Update conflicts with concurrent update
end;

{************************************************}
constructor TALFBXStringKeyPoolBinTreeNode.Create;
begin
  inherited;
  Pool := TObjectList.Create(True);
end;

{************************************************}
destructor TALFBXStringKeyPoolBinTreeNode.Destroy;
begin
  Pool.free;
  inherited;
end;

{**********************************************************}
constructor TALFBXConnectionStatementPoolBinTreeNode.Create;
begin

  //inherit
  inherited;

  //init object var
  Lib := nil;
  StmtHandle := nil;
  Sqlda  := nil;
  LastAccessDate := GetTickCount64;
  OwnsObjects := True;

end;

{**********************************************************}
destructor TALFBXConnectionStatementPoolBinTreeNode.Destroy;
begin

  //if OwnsObjects only free the statement
  if OwnsObjects then begin

    //drop the statement
    try
      if assigned(StmtHandle) then Lib.DSQLFreeStatement(StmtHandle, DSQL_drop);
    Except
      //what else we can do here ?
      //this can happen if connection lost for exemple
    end;
    if assigned(Sqlda) then Sqlda.free;

  end;

  //inherit
  inherited;

end;

{******************************************************}
constructor TALFBXConnectionStatementPoolBinTree.Create;
begin
  inherited;
  LastGarbage := GetTickCount64;
end;

{********************************************************}
{!!we guess that this procedure will be not multithread!!
but we have a strange bug when Fsignal is TEvent, when we
disconnect the FBserver, them an EaccessViolation in ntdll
is raise in the waitfor in the execute function}
procedure ALFBXEventCallback(UserData: Pointer; Length: Smallint; Updated: PAnsiChar); cdecl;
begin
  if (Assigned(UserData) and Assigned(Updated)) then begin
    with TALFBXEventThread(UserData) do begin
      if FEventCanceled then begin
        SetEvent(FSignal);
        Exit;
      end;
      ALMove(Updated^, fResultBuffer^, Length);
      FQueueEvent := True;
      SetEvent(FSignal);
    end;
  end
  else begin
    //if Updated = nil then it's look like it's an error
    //like connection lost for exemple or a call to EventCancel
    with TALFBXEventThread(UserData) do begin
      if FEventCanceled then begin
        SetEvent(FSignal);
        Exit;
      end;
      FQueueEvent := False;
      SetEvent(FSignal);
    end;
  end;
end;

{*********************************************************}
procedure TALFBXEventThread.initObject(const aDataBaseName,
                                             aLogin,
                                             aPassword,
                                             aCharSet: AnsiString;
                                       const aEventNames: AnsiString;
                                       aConnectionMaxIdleTime: integer;
                                       aNumbuffers: integer;
                                       const aOpenConnectionExtraParams: AnsiString;
                                       aOnEvent: TALFBXEventThreadEvent;
                                       aOnException: TALFBXEventThreadException);
Var LLst: TALStrings;
    i: integer;
begin
  //if we put lower than tpNormal it seam than the
  //EventThread.Free will never return !
  //Priority := tpNormal;
  FreeOnTerminate := False;
  if FConnectionMaxIdleTime <= 0 then FConnectionMaxIdleTime := INFINITE
  else FConnectionMaxIdleTime := aConnectionMaxIdleTime;
  FDBHandle := nil;
  FQueueEvent := False;
  fResultBuffer := Nil;
  FSignal := CreateEvent(nil, true, false, '');
  fcompleted := False;
  fStarted := False;
  FEventCanceled := False;
  FWaitingSignal := False;
  fOnEvent := aOnEvent;
  fOnException := aOnException;
  FDataBaseName:= aDataBaseName;
  FCharset:= ALFBXStrToCharacterSet(aCharSet);
  fOpenConnectionParams := 'user_name = '+aLogin+'; '+
                           'password = '+aPassword+'; '+
                           'lc_ctype = '+aCharSet;
  if aNumbuffers > -1 then fOpenConnectionParams := fOpenConnectionParams + '; num_buffers = ' + ALIntToStr(aNumbuffers);
  if aOpenConnectionExtraParams <> '' then fOpenConnectionParams := fOpenConnectionParams + '; ' + aOpenConnectionExtraParams;
  LLst := TALStringList.Create;
  Try
    LLst.Text := ALTrim(alStringReplace(aEventNames,';',#13#10,[rfReplaceALL]));
    i := 0;
    while (i <= 14) and (i <= LLst.Count - 1) do begin
      fEventNamesArr[i] := ALTrim(LLst[i]);
      inc(i);
    end;
    fEventNamesCount := i;
    while i <= 14 do begin
      fEventNamesArr[i] := '';
      inc(i);
    end;
  Finally
    LLst.Free;
  End;
end;

{*******************************************************}
constructor TALFBXEventThread.Create(const aDataBaseName,
                                           aLogin,
                                           aPassword,
                                           aCharSet: AnsiString;
                                     const aEventNames: AnsiString; // ; separated value like EVENT1;EVENT2; etc...
                                     aApiVer: TALFBXVersion_API;
                                     aOnEvent: TALFBXEventThreadEvent;
                                     aOnException: TALFBXEventThreadException;
                                     const alib: AnsiString = GDS32DLL;
                                     const aConnectionMaxIdleTime: integer = -1;
                                     const aNumbuffers: integer = -1;
                                     const aOpenConnectionExtraParams: AnsiString = '');
begin
  fLibrary := TALFBXLibrary.Create(aApiVer);
  fLibrary.Load(alib);
  FownLibrary := True;
  initObject(aDataBaseName,
             aLogin,
             aPassword,
             aCharSet,
             aEventNames,
             aConnectionMaxIdleTime,
             aNumbuffers,
             aOpenConnectionExtraParams,
             aOnEvent,
             aOnException);
  inherited Create(False); // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010
end;

{*******************************************************}
Constructor TALFBXEventThread.Create(const aDataBaseName,
                                           aLogin,
                                           aPassword,
                                           aCharSet: AnsiString;
                                     const aEventNames: AnsiString; // ; separated value like EVENT1;EVENT2; etc...
                                     alib: TALFBXLibrary;
                                     aOnEvent: TALFBXEventThreadEvent;
                                     aOnException: TALFBXEventThreadException;
                                     const aConnectionMaxIdleTime: integer = -1;
                                     const aNumbuffers: integer = -1;
                                     const aOpenConnectionExtraParams: AnsiString = '');
begin
  fLibrary := alib;
  FownLibrary := False;
  initObject(aDataBaseName,
             aLogin,
             aPassword,
             aCharSet,
             aEventNames,
             aConnectionMaxIdleTime,
             aNumbuffers,
             aOpenConnectionExtraParams,
             aOnEvent,
             aOnException);
  inherited Create(False);  // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010
end;

{*******************************************************}
constructor TALFBXEventThread.Create(const aDataBaseName,
                                           aLogin,
                                           aPassword,
                                           aCharSet: AnsiString;
                                     const aEventNames: AnsiString; // ; separated value like EVENT1;EVENT2; etc...
                                     aApiVer: TALFBXVersion_API;
                                     const alib: AnsiString = GDS32DLL;
                                     const aConnectionMaxIdleTime: integer = -1;
                                     const aNumbuffers: integer = -1;
                                     const aOpenConnectionExtraParams: AnsiString = '');
begin
  fLibrary := TALFBXLibrary.Create(aApiVer);
  fLibrary.Load(alib);
  FownLibrary := True;
  initObject(aDataBaseName,
             aLogin,
             aPassword,
             aCharSet,
             aEventNames,
             aConnectionMaxIdleTime,
             aNumbuffers,
             aOpenConnectionExtraParams,
             nil,
             nil);
  inherited Create(False); // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010
end;

{*******************************************************}
constructor TALFBXEventThread.Create(const aDataBaseName,
                                           aLogin,
                                           aPassword,
                                           aCharSet: AnsiString;
                                     const aEventNames: AnsiString; // ; separated value like EVENT1;EVENT2; etc...
                                     alib: TALFBXLibrary;
                                     const aConnectionMaxIdleTime: integer = -1;
                                     const aNumbuffers: integer = -1;
                                     const aOpenConnectionExtraParams: AnsiString = '');
begin
  fLibrary := alib;
  FownLibrary := False;
  initObject(aDataBaseName,
             aLogin,
             aPassword,
             aCharSet,
             aEventNames,
             aConnectionMaxIdleTime,
             aNumbuffers,
             aOpenConnectionExtraParams,
             nil,
             nil);
  inherited Create(False);  // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010
end;

{********************************************}
procedure TALFBXEventThread.AfterConstruction;
begin
  inherited;
  while (not fStarted) do sleep(1);
end;

{***********************************}
destructor TALFBXEventThread.Destroy;
begin

  //first set terminated to true
  If not Terminated then Terminate;

  //in case the execute in waiting fire the Fsignal
  while (not fWaitingSignal) and (not fCompleted) do sleep(1);
  if (not fCompleted) then setEvent(FSignal);
  while (not fCompleted) do sleep(1);

  //close the fSignal handle
  CloseHandle(FSignal);

  //free the library
  if FownLibrary then fLibrary.Free;

  //destroy the object
  inherited;

end;

{*******************************************************************************}
procedure TALFBXEventThread.DoEvent(const EventName: AnsiString; Count: Integer);
begin
  if assigned(fOnEvent) then fOnEvent(self, EventName, Count);
end;

{********************************************************}
procedure TALFBXEventThread.DoException(Error: Exception);
begin
  if assigned(fonException) then fonException(self, Error);
end;

{**********************************}
procedure TALFBXEventThread.Execute;
var LEventBuffer: PAnsiChar;
    LEventBufferLen: Smallint;
    LEventID: Integer;
    LStatusVector: TALFBXStatusVector;

    {-----------------------------}
    Procedure InternalFreeLocalVar;
    Begin
      //free the aEventID
      if LEventID <> 0 then begin
        FEventCanceled := True;
        Try
          ResetEvent(Fsignal);
          FLibrary.EventCancel(FDbHandle, LEventID);
          //in case the connection or fbserver crash the Fsignal will
          //be never signaled
          WaitForSingleObject(FSignal, 60000);
        Except
          //in case of error what we can do except suppose than the event was canceled ?
          //in anyway we will reset the FDbHandle after
        End;
        FEventCanceled := False;
      end;
      LEventID := 0;

      //free the aEventBuffer
      if assigned(LEventBuffer) then begin
        Try
          FLibrary.IscFree(LEventBuffer);
        Except
          //paranoia mode ... i never see it's can raise any error here
        End;
      end;
      LEventBuffer := nil;

      //free the FResultBuffer
      if assigned(FResultBuffer) then begin
        Try
          FLibrary.IscFree(FResultBuffer);
        Except
          //paranoia mode ... i never see it's can raise any error here
        End;
      end;
      FResultBuffer := nil;

      //free the FDBHandle
      if assigned(FDBHandle) then begin
        Try
          FLibrary.DetachDatabase(FDBHandle);
        Except
          //yes the function before can do an exception if the network connection
          //was dropped... but not our bussiness what we can do ?
        End;
      end;
      FDBHandle := Nil;
    End;

var LCurrentEventIdx: integer;
    LMustResetDBHandle: Boolean;

begin
  //to be sure that the thread was stated
  fStarted := True;

  LEventBuffer := nil;
  LEventID := 0;
  LEventBufferLen := 0;
  LMustResetDBHandle := True;

  while not Terminated do begin
    Try

      //if the DBHandle is not assigned the create it
      //FDBHandle can not be assigned if for exemple
      //an error (disconnection happen)
      if LMustResetDBHandle then begin

        //set the FMustResetDBHandle to false
        LMustResetDBHandle := False;

        //free the local var
        InternalFreeLocalVar;

        //First init FDBHandle
        FLibrary.AttachDatabase(FDataBaseName,
                                FDBHandle,
                                fOpenConnectionParams);

        //register the EventBlock
        LEventBufferLen := FLibrary.EventBlock(LEventBuffer,
                                               fResultBuffer,
                                               fEventNamesCount,
                                               PAnsiChar(fEventNamesArr[0]),
                                               PAnsiChar(fEventNamesArr[1]),
                                               PAnsiChar(fEventNamesArr[2]),
                                               PAnsiChar(fEventNamesArr[3]),
                                               PAnsiChar(fEventNamesArr[4]),
                                               PAnsiChar(fEventNamesArr[5]),
                                               PAnsiChar(fEventNamesArr[6]),
                                               PAnsiChar(fEventNamesArr[7]),
                                               PAnsiChar(fEventNamesArr[8]),
                                               PAnsiChar(fEventNamesArr[9]),
                                               PAnsiChar(fEventNamesArr[10]),
                                               PAnsiChar(fEventNamesArr[11]),
                                               PAnsiChar(fEventNamesArr[12]),
                                               PAnsiChar(fEventNamesArr[13]),
                                               PAnsiChar(fEventNamesArr[14]));

        //the First EventQueue
        ResetEvent(Fsignal);
        FLibrary.EventQueue(FdbHandle,
                            LEventID,
                            LEventBufferLen,
                            LEventBuffer,
                            @ALFBXEventCallback,
                            self);
        if WaitForSingleObject(FSignal, 60000) <> WAIT_OBJECT_0 then raise Exception.Create('Timeout in the first call to isc_que_events');
        FLibrary.EventCounts(LStatusVector,
                             LEventBufferLen,
                             LEventBuffer,
                             fResultBuffer);

        //set the FQueueEvent to false in case the next
        //WaitForSingleObject fired because of a timeout
        FQueueEvent := False;

        //the 2nd EventQueue
        ResetEvent(Fsignal);
        FLibrary.EventQueue(FdbHandle,
                            LEventID,
                            LEventBufferLen,
                            LEventBuffer,
                            @ALFBXEventCallback,
                            self);

      end;

      //if terminated then exit;
      if Terminated then Break;

      //set fWaitingsignal
      fWaitingsignal := True;

      //stop the thread stile a event appear
      WaitForSingleObject(FSignal, FConnectionMaxIdleTime); //every 20 minutes reset the connection

      //set fWaitingsignal
      fWaitingsignal := False;

      //if terminated then exit;
      if Terminated then Break;

      //if an event was set
      if (FQueueEvent) then begin

        //retrieve the list of event
        FLibrary.EventCounts(LStatusVector,
                             LEventBufferLen,
                             LEventBuffer,
                             fResultBuffer);

        //if it was the event
        for LCurrentEventIdx := 0 to 14 do
          if LStatusVector[LCurrentEventIdx] <> 0 then DoEvent(fEventNamesArr[LCurrentEventIdx],LStatusVector[LCurrentEventIdx]);

        //reset the FQueueEvent
        FQueueEvent := False;

        //start to listen again
        ResetEvent(Fsignal);
        FLibrary.EventQueue(FdbHandle,
                            LEventID,
                            LEventBufferLen,
                            LEventBuffer,
                            @ALFBXEventCallback,
                            self);

      end

      //it must be an error somewhere
      else LMustResetDBHandle := True;

    Except
      on E: Exception do begin
        //Reset the DBHandle
        LMustResetDBHandle := True;
        DoException(E);
      end;
    End;
  end;


  Try
    //free the local var
    InternalFreeLocalVar;
  Except
    on E: Exception do begin
      DoException(E);
    end;
  End;


  //set completed to true
  //we need to to this because i don't know why
  //but on isapi the waitfor (call in thread.free)
  //never return.
  //but i don't remenbered if the free was call in the initialization
  //section of the ISAPI DLL (and that bad to do something like this
  //in initialization or finalization).
  fcompleted := True;
end;

end.
