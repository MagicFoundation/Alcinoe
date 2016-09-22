{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)

product:      ALFBXClient (Alcinoe FireBird Express Client)
Version:      4.00

Description:  Retrieving Data as XML from Firebird Server.

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

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

History :     02/03/2010: add aNumbuffers: Integer to the connect
              27/01/2012: add the keyword &> (ex: &>xxx) at the beginning of
                          the rowtag to update the rowtag by the value of the
                          field xxx
              29/01/2012: Add also a way to do Update SQL with params
              26/06/2012: Add xe2 support
              03/03/2013: remove TALFBXClientSQLParam.isBlob => useless !

Link :        http://www.progdigy.com/modules.php?name=UIB

**************************************************************}
unit ALFBXClient;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     System.classes,
     System.SysUtils,
     System.Contnrs,
     System.SyncObjs,
     {$ELSE}
     Windows,
     classes,
     SysUtils,
     Contnrs,
     SyncObjs,
     {$IFEND}
     AlXmlDoc,
     ALAVLBinaryTree,
     ALFBXLib,
     ALFBXBase,
     ALString,
     ALStringList;

Type

  {$IF CompilerVersion >= 23} {Delphi XE2}
  TALFBXClientSelectDataOnNewRowFunct = reference to Procedure(XMLRowData: TalXmlNode;
                                                               const ViewTag: AnsiString;
                                                               ExtData: Pointer;
                                                               Var Continue: Boolean);
  {$ELSE}
  TALFBXClientSelectDataOnNewRowFunct = Procedure(XMLRowData: TalXmlNode;
                                                  const ViewTag: AnsiString;
                                                  ExtData: Pointer;
                                                  Var Continue: Boolean);
  {$IFEND}

  {----------------------------}
  TALFBXClientSQLParam = record
    Value: AnsiString;
    IsNull: Boolean;
    class function Create: TALFBXClientSQLParam; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
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
    class function Create: TALFBXClientSelectDataQUERY; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
  end;
  TALFBXClientSelectDataQUERIES = array of TALFBXClientSelectDataQUERY;

  {----------------------------------}
  TALFBXClientUpdateDataQUERY = record
    SQL: AnsiString;
    Params: TALFBXClientSQLParams; // use to replace the ? in SQL like
                                   // insert into TableA(FieldA) Values(?)
    class function Create: TALFBXClientUpdateDataQUERY; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
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
    LastGarbage: Int64;
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
    FLastReadTransactionGarbage: Int64;
    //--READ TRANSACTION POOL--

    //--READ STATEMENT POOL--
    FReadStatementPool: TALStringKeyAVLBinaryTree;       // pool of STATEMENT with READ ONLY READ COMMITED TRANSACTION
    FReadStatementPoolCS: TCriticalSection;
    FWorkingReadStatementCount: Integer;
    FLastReadStatementGarbage: Int64;
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

  {$IF CompilerVersion >= 23} {Delphi XE2}
  TALFBXEventThreadEvent = reference to Procedure (Sender: TObject; const EventName: AnsiString; Count: Integer);
  TALFBXEventThreadException = reference to procedure (Sender: TObject; Error: Exception);
  {$ELSE}
  TALFBXEventThreadEvent = Procedure (Sender: TObject; const EventName: AnsiString; Count: Integer) of object;
  TALFBXEventThreadException = procedure (Sender: TObject; Error: Exception) of object;
  {$IFEND}

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

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.Diagnostics,
     {$ELSE}
     Diagnostics,
     {$IFEND}
     ALCipher,
     ALWindows,
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

Var aXMLDATA: TalXmlDocument;
    aSelectPart: AnsiString;
    aFromPart: AnsiString;
    aJoinPart: AnsiString;
    aWherePart: AnsiString;

    a_saved_TraHandle: IscTrHandle;
    a_saved_StmtHandle: IscStmtHandle;
    a_saved_Sqlda: TALFBXSQLResult;
    a_saved_StmtSQL: AnsiString;

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
  aSelectPart := ALIfThen(not SkipIOStats,     'MON$PAGE_READS as PAGE_READS, '+
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
  if aSelectPart <> '' then delete(aSelectPart,length(aSelectPart) - 1, 1)
  else Exit;

  //build the aFromPart
  if StatementSQL <> '' then       aFromPart := 'MON$STATEMENTS'
  else if TransactionID <> -1 then aFromPart := 'MON$TRANSACTIONS'
  else if ConnectionID <> -1 then  aFromPart := 'MON$ATTACHMENTS'
  else                             aFromPart := 'MON$DATABASE';

  //build the aJoinPart
  aJoinPart := ALIfThen(not SkipIOStats,     'JOIN MON$IO_STATS ON MON$IO_STATS.MON$STAT_ID = '        +aFromPart+'.MON$STAT_ID ') +
               ALIfThen(not SkipRecordStats, 'JOIN MON$RECORD_STATS ON MON$RECORD_STATS.MON$STAT_ID = '+aFromPart+'.MON$STAT_ID ') +
               ALIfThen(not SkipMemoryUsage, 'JOIN MON$MEMORY_USAGE ON MON$MEMORY_USAGE.MON$STAT_ID = '+aFromPart+'.MON$STAT_ID ');

  //build the aWherePart
  if StatementSQL <> '' then       aWherePart := 'WHERE '+
                                                 'MON$SQL_TEXT = ' + ALQuotedStr(StatementSQL) +
                                                 ALIfThen(TransactionID <> -1, ' AND MON$TRANSACTION_ID = '+ALIntToStr(TransactionID)) +
                                                 ALIfThen(ConnectionID <> -1,  ' AND MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID))
  else if TransactionID <> -1 then aWherePart := 'WHERE '+
                                                 'MON$TRANSACTION_ID = '+ALIntToStr(TransactionID) +
                                                 ALIfThen(ConnectionID <> -1,  ' AND MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID))
  else if ConnectionID <> -1 then  aWherePart := 'WHERE '+
                                                 'MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID)
  else                             aWherePart := '';


  //Save the old data
  a_saved_TraHandle := fTraHandle;
  a_saved_StmtHandle := fStmtHandle;
  a_saved_Sqlda := fSqlda;
  a_saved_StmtSQL := fStmtSQL;
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
      aXMLDATA := TALXmlDocument.create('root');
      try

        SelectData('SELECT '+
                     aSelectPart +
                   'FROM ' +
                     aFromPart + ' ' +
                   aJoinPart +
                   aWherePart,
                   'rec',
                   aXMLDATA.DocumentElement,
                   ALDefaultFormatSettings);

        if aXMLDATA.DocumentElement.ChildNodes.Count <> 1 then raise Exception.Create('Can not get the monitoring stats');
        with aXMLDATA.DocumentElement.ChildNodes[0] do begin

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
        aXMLDATA.free;
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
    fTraHandle := a_saved_TraHandle;
    fStmtHandle := a_saved_StmtHandle;
    fSqlda := a_saved_Sqlda;
    fStmtSQL := a_saved_StmtSQL;

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
var aSize: byte;
    aData: AnsiString;
begin
  If not connected then raise Exception.Create('Not connected');
  aData := FLibrary.DatabaseInfoString(FDbHandle, item, 256);
  case Item of
    isc_info_cur_logfile_name,
    isc_info_wal_prv_ckpt_fname: begin
                                   aSize := byte(aData[4]);
                                   ALMove(aData[5], aData[1], aSize);
                                   SetLength(aData, aSize);
                                 end;
    else begin
      aSize := byte(aData[5]);
      ALMove(aData[6], aData[1], aSize);
      SetLength(aData, aSize);
    end;
  end;
  Result := AnsiString(aData);
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
Var aParams: AnsiString;
begin
  if connected then raise Exception.Create('Already connected');
  fCharSet :=  ALFBXStrToCharacterSet(CharSet);
  aParams := 'user_name = '+Login+'; '+
             'password = '+Password+'; '+
             'lc_ctype = '+CharSet;
  if ExtraParams <> '' then aParams := aParams + '; ' + ExtraParams;
  Try
    FLibrary.AttachDatabase(DataBaseName,
                            fDBHandle,
                            aParams);
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

Var aSqlpa: TALFBXSQLParams;
    aParamsIndex: integer;
    aBlobhandle: IscBlobHandle;
    aQueriesIndex: integer;
    aViewRec: TalXmlNode;
    aDropStmt: Boolean;
    aXmlDocument: TalXmlDocument;
    aStopWatch: TStopWatch;
    aCacheKey: ansiString;
    aCacheStr: ansiString;

    {------------------------------------}
    Procedure InternalSelectDataFetchRows;
    var aColumnIndex: integer;
        aRecIndex: integer;
        aRecAdded: integer;
        aContinue: Boolean;
        aNewRec: TalXmlNode;
        aValueRec: TalXmlNode;
        aUpdateRowTagByFieldValue: Boolean;
    Begin

      //init the aViewRec
      if (Queries[aQueriesIndex].ViewTag <> '') and (not assigned(aXmlDocument)) then aViewRec := XMLdata.AddChild(Queries[aQueriesIndex].ViewTag)
      else aViewRec := XMLdata;

      //init aUpdateRowTagByFieldValue
      if AlPos('&>',Queries[aQueriesIndex].RowTag) = 1 then begin
        delete(Queries[aQueriesIndex].RowTag, 1, 2);
        aUpdateRowTagByFieldValue := Queries[aQueriesIndex].RowTag <> '';
      end
      else aUpdateRowTagByFieldValue := False;

      //retrieve all row
      aRecIndex := 0;
      aRecAdded := 0;
      while Flibrary.DSQLFetch(fDBHandle, fTraHandle, fStmtHandle, FSQLDIALECT, fsqlda) do begin

        //process if > Skip
        inc(aRecIndex);
        If aRecIndex > Queries[aQueriesIndex].Skip then begin

          //init NewRec
          if (Queries[aQueriesIndex].RowTag <> '') and (not assigned(aXmlDocument)) then aNewRec := aViewRec.AddChild(Queries[aQueriesIndex].RowTag)
          Else aNewRec := aViewRec;

          //loop throught all column
          For aColumnIndex := 0 to fsqlda.FieldCount - 1 do begin
            aValueRec := aNewRec.AddChild(ALlowercase(fsqlda.AliasName[aColumnIndex]));
            if (fSQLDA.SQLType[aColumnIndex] = SQL_BLOB) then avalueRec.ChildNodes.Add(
                                                                                       avalueRec.OwnerDocument.CreateNode(
                                                                                                                          GetFieldValue(fsqlda,
                                                                                                                                        fDBHandle,
                                                                                                                                        fTRAHandle,
                                                                                                                                        aColumnIndex,
                                                                                                                                        FormatSettings),
                                                                                                                          ntCData
                                                                                                                         )
                                                                                       )
            else aValueRec.Text := GetFieldValue(fsqlda,
                                                 fDBHandle,
                                                 fTRAHandle,
                                                 aColumnIndex,
                                                 FormatSettings);
            if aUpdateRowTagByFieldValue and (aValueRec.NodeName=aNewRec.NodeName) then aNewRec.NodeName := ALLowerCase(aValueRec.Text);
          end;

          //handle OnNewRowFunct
          if assigned(OnNewRowFunct) then begin
            aContinue := True;
            OnNewRowFunct(aNewRec, Queries[aQueriesIndex].ViewTag, ExtData, aContinue);
            if Not aContinue then Break;
          end;

          //free the node if aXmlDocument
          if assigned(aXmlDocument) then aXmlDocument.DocumentElement.ChildNodes.Clear;

          //handle the First
          inc(aRecAdded);
          If (Queries[aQueriesIndex].First > 0) and (aRecAdded >= Queries[aQueriesIndex].First) then Break;

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
  if assigned(XMLDATA) then aXmlDocument := Nil
  else begin
    aXmlDocument := TALXmlDocument.create('root');
    XMLDATA := aXmlDocument.DocumentElement;
  end;

  Try

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //loop on all the SQL
    For aQueriesIndex := 0 to length(Queries) - 1 do begin

      //Handle the CacheThreshold
      aCacheKey := '';
      If (Queries[aQueriesIndex].CacheThreshold > 0) and
         (not assigned(aXmlDocument)) and
         (((length(Queries) = 1) and
           (XMLdata.ChildNodes.Count = 0)) or  // else the save will not work
          (Queries[aQueriesIndex].ViewTag <> '')) then begin

        //try to load from from cache
        aCacheKey := ALStringHashSHA1(Queries[aQueriesIndex].RowTag + '#' +
                                      alinttostr(Queries[aQueriesIndex].Skip) + '#' +
                                      alinttostr(Queries[aQueriesIndex].First) + '#' +
                                      ALGetFormatSettingsID(FormatSettings) + '#' +
                                      Queries[aQueriesIndex].SQL);
        if loadcachedData(aCacheKey, aCacheStr) then begin

          //init the aViewRec
          if (Queries[aQueriesIndex].ViewTag <> '') then aViewRec := XMLdata.AddChild(Queries[aQueriesIndex].ViewTag)
          else aViewRec := XMLdata;

          //assign the tmp data to the XMLData
          aViewRec.LoadFromXML(aCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

          //go to the next loop
          continue;

        end;

      end;

      //start the TstopWatch
      aStopWatch.Reset;
      aStopWatch.Start;

      //prepare if neccessary
      if (not assigned(fSqlda)) or
         (not assigned(fStmtHandle)) or
         (Queries[aQueriesIndex].SQL <> fStmtSQL) then begin
        Prepare(Queries[aQueriesIndex].SQL);
        aDropStmt := True;
      end
      else aDropStmt := False;

      try

        //if their is params
        if length(Queries[aQueriesIndex].Params) > 0 then begin

          //create the aSqlpa object
          aSqlpa := TALFBXSQLParams.Create(fCharSet);

          try

            //loop throught all Params Fields
            for aParamsIndex := 0 to length(Queries[aQueriesIndex].Params) - 1 do begin

              //with current Params Fields
              with Queries[aQueriesIndex].Params[aParamsIndex] do begin

                //isnull
                if IsNull then begin
                  aSqlpa.AddFieldType('', uftVarchar);
                  aSqlpa.IsNull[aParamsIndex] := True;
                end

                //IsBlob
                else if length(value) > high(smallint) then begin
                  aSqlpa.AddFieldType('', uftBlob);
                  aBlobhandle := nil;
                  aSqlpa.AsQuad[aParamsIndex] := Flibrary.BlobCreate(fDBHandle,fTraHandle,aBlobHandle);
                  Try
                    FLibrary.BlobWriteString(aBlobHandle,Value);
                  Finally
                    FLibrary.BlobClose(aBlobHandle);
                  End;
                end

                //all the other
                else begin
                  aSqlpa.AddFieldType('', uftVarchar);
                  aSqlpa.AsAnsiString[aParamsIndex] := Value;
                end;

              end;

            end;

            //execute the sql with the params
            FLibrary.DSQLExecute(fTraHandle, fStmtHandle, FSQLDIALECT, asqlpa);

            //fetch the rows
            InternalSelectDataFetchRows;

          finally
            asqlpa.free;
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
        if aDropStmt then begin

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
      aStopWatch.Stop;
      OnSelectDataDone(Queries[aQueriesIndex],
                       aStopWatch.Elapsed.TotalMilliseconds);

      //save to the cache
      If aCacheKey <> '' then begin

        //save the data
        aViewRec.SaveToXML(aCacheStr, true{SaveOnlyChildNodes});
        SaveDataToCache(aCacheKey,
                        Queries[aQueriesIndex].CacheThreshold,
                        aCacheStr);

      end;

    End;

  Finally
    if assigned(aXmlDocument) then aXmlDocument.free;
  End;

end;

{*************************************************************************}
procedure TALFBXClient.SelectData(const Query: TALFBXClientSelectDataQUERY;
                                  OnNewRowFunct: TALFBXClientSelectDataOnNewRowFunct;
                                  ExtData: Pointer;
                                  const FormatSettings: TALFormatSettings);
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := Query;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  aSelectDataQUERIES[0].skip := Skip;
  aSelectDataQUERIES[0].First := First;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := Query;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  aSelectDataQUERIES[0].RowTag := RowTag;
  aSelectDataQUERIES[0].skip := Skip;
  aSelectDataQUERIES[0].First := First;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  aSelectDataQUERIES[0].RowTag := RowTag;
  SelectData(aSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{******************************************************}
procedure TALFBXClient.SelectData(const SQL: AnsiString;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TALFormatSettings);
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
    i: integer;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  setlength(aSelectDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    aSelectDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    aSelectDataQUERIES[0].params[i].Value := Params[i];
    aSelectDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  aSelectDataQUERIES[0].RowTag := RowTag;
  aSelectDataQUERIES[0].skip := Skip;
  aSelectDataQUERIES[0].First := First;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
    i: integer;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  setlength(aSelectDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    aSelectDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    aSelectDataQUERIES[0].params[i].Value := Params[i];
    aSelectDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  aSelectDataQUERIES[0].RowTag := RowTag;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
    i: integer;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  setlength(aSelectDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    aSelectDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    aSelectDataQUERIES[0].params[i].Value := Params[i];
    aSelectDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  SelectData(aSelectDataQUERIES,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{******************************************************************************}
procedure TALFBXClient.UpdateData(const Queries: TALFBXClientUpdateDataQUERIES);
Var aSqlpa: TALFBXSQLParams;
    aBlobhandle: IscBlobHandle;
    aParamsIndex: integer;
    aQueriesIndex: integer;
    aDropStmt: Boolean;
    aStopWatch: TStopWatch;
begin

  //exit if no SQL
  if length(Queries) = 0 then Exit;

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  aStopWatch := TstopWatch.Create;

  {loop on all the SQL}
  For aQueriesIndex := 0 to length(Queries) - 1 do begin

    //start the TstopWatch
    aStopWatch.Reset;
    aStopWatch.Start;

    //if their is params
    if length(Queries[aQueriesIndex].Params) > 0 then begin

      //prepare if neccessary
      if (not assigned(fSqlda)) or
         (not assigned(fStmtHandle)) or
         (Queries[aQueriesIndex].SQL <> fStmtSQL) then begin
        Prepare(Queries[aQueriesIndex].SQL);
        aDropStmt := True;
      end
      else aDropStmt := False;

      try

        //create the aSqlpa object
        aSqlpa := TALFBXSQLParams.Create(fCharSet);
        try

          //loop throught all Params Fields
          for aParamsIndex := 0 to length(Queries[aQueriesIndex].Params) - 1 do begin

            //with current Params Fields
            with Queries[aQueriesIndex].Params[aParamsIndex] do begin

              //isnull
              if IsNull then begin
                aSqlpa.AddFieldType('', uftVarchar);
                aSqlpa.IsNull[aParamsIndex] := True;
              end

              //IsBlob
              else if length(value) > high(smallint) then begin
                aSqlpa.AddFieldType('', uftBlob);
                aBlobhandle := nil;
                aSqlpa.AsQuad[aParamsIndex] := Flibrary.BlobCreate(fDBHandle,fTraHandle,aBlobHandle);
                Try
                  FLibrary.BlobWriteString(aBlobHandle,Value);
                Finally
                  FLibrary.BlobClose(aBlobHandle);
                End;
              end

              //all the other
              else begin
                aSqlpa.AddFieldType('', uftVarchar);
                aSqlpa.AsAnsiString[aParamsIndex] := Value;
              end;

            end;

          end;

          //execute the SQL
          FLibrary.DSQLExecute(fTraHandle, fStmtHandle, FSQLDIALECT, aSqlpa);

        finally
          aSqlpa.free;
        end;

      finally

        //free the statement
        if aDropStmt then begin

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
         (Queries[aQueriesIndex].SQL <> fStmtSQL) then begin

        Flibrary.DSQLExecuteImmediate(fDBHandle, fTraHandle, Queries[aQueriesIndex].SQL, FSQLDIALECT, nil);

      end

      //in case the query was prepared
      else begin

        //execute the SQL
        FLibrary.DSQLExecute(fTraHandle, fStmtHandle, FSQLDIALECT, nil);

      end;

    end;

    //do the OnUpdateDataDone
    aStopWatch.Stop;
    OnUpdateDataDone(Queries[aQueriesIndex],
                     aStopWatch.Elapsed.TotalMilliseconds);

  end;

end;

{**************************************************************************}
procedure TALFBXClient.UpdateData(const Query: TALFBXClientUpdateDataQUERY);
var aUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
begin
  setlength(aUpdateDataQUERIES,1);
  aUpdateDataQUERIES[0] := Query;
  UpdateData(aUpdateDataQUERIES);
end;

{**************************************************}
procedure TALFBXClient.UpdateData(SQLs: TALStrings);
Var aSQLsIndex : integer;
    aUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
begin
  setlength(aUpdateDataQUERIES,SQLs.Count);
  For aSQLsIndex := 0 to SQLs.Count - 1 do begin
    aUpdateDataQUERIES[aSQLsIndex] := TALFBXClientUpdateDataQUERY.Create;
    aUpdateDataQUERIES[aSQLsIndex].SQL := SQLs[aSQLsIndex];
  end;
  UpdateData(aUpdateDataQUERIES);
end;

{*******************************************************}
procedure TALFBXClient.UpdateData(const SQL: AnsiString);
Var aUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
begin
  setlength(aUpdateDataQUERIES,1);
  aUpdateDataQUERIES[0] := TALFBXClientUpdateDataQUERY.Create;
  aUpdateDataQUERIES[0].SQL := SQL;
  UpdateData(aUpdateDataQUERIES);
end;

{******************************************************}
procedure TALFBXClient.UpdateData(const SQL: AnsiString;
                                  const Params: Array of AnsiString);
Var aUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
    i: integer;
begin
  setlength(aUpdateDataQUERIES,1);
  aUpdateDataQUERIES[0] := TALFBXClientUpdateDataQUERY.Create;
  aUpdateDataQUERIES[0].SQL := SQL;
  setlength(aUpdateDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    aUpdateDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    aUpdateDataQUERIES[0].params[i].Value := Params[i];
    aUpdateDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  UpdateData(aUpdateDataQUERIES);
end;

{*****************************************************************}
procedure TALFBXClient.UpdateData(const SQLs: array of AnsiString);
Var aUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
    i: integer;
begin
  setlength(aUpdateDataQUERIES,length(SQLs));
  for I := 0 to length(SQLs) - 1 do begin
    aUpdateDataQUERIES[i] := TALFBXClientUpdateDataQUERY.Create;
    aUpdateDataQUERIES[i].SQL := SQLs[i];
  end;
  UpdateData(aUpdateDataQUERIES);
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

Var aXMLDATA: TalXmlDocument;
    aSelectPart: AnsiString;
    aFromPart: AnsiString;
    aJoinPart: AnsiString;
    aWherePart: AnsiString;

    aDBHandle: IscDbHandle;
    aTraHandle: IscTrHandle;

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
  aSelectPart := ALIfThen(not SkipIOStats,     'MON$PAGE_READS as PAGE_READS, '+
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
  if aSelectPart <> '' then delete(aSelectPart,length(aSelectPart) - 1, 1)
  else Exit;

  //build the aFromPart
  if StatementSQL <> '' then       aFromPart := 'MON$STATEMENTS'
  else if TransactionID <> -1 then aFromPart := 'MON$TRANSACTIONS'
  else if ConnectionID <> -1 then  aFromPart := 'MON$ATTACHMENTS'
  else                             aFromPart := 'MON$DATABASE';

  //build the aJoinPart
  aJoinPart := ALIfThen(not SkipIOStats,     'JOIN MON$IO_STATS ON MON$IO_STATS.MON$STAT_ID = '        +aFromPart+'.MON$STAT_ID ') +
               ALIfThen(not SkipRecordStats, 'JOIN MON$RECORD_STATS ON MON$RECORD_STATS.MON$STAT_ID = '+aFromPart+'.MON$STAT_ID ') +
               ALIfThen(not SkipMemoryUsage, 'JOIN MON$MEMORY_USAGE ON MON$MEMORY_USAGE.MON$STAT_ID = '+aFromPart+'.MON$STAT_ID ');

  //build the aWherePart
  if StatementSQL <> '' then       aWherePart := 'WHERE '+
                                                 'MON$SQL_TEXT = ' + ALQuotedStr(StatementSQL) +
                                                 ALIfThen(TransactionID <> -1, ' AND MON$TRANSACTION_ID = '+ALIntToStr(TransactionID)) +
                                                 ALIfThen(ConnectionID <> -1,  ' AND MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID))
  else if TransactionID <> -1 then aWherePart := 'WHERE '+
                                                 'MON$TRANSACTION_ID = '+ALIntToStr(TransactionID) +
                                                 ALIfThen(ConnectionID <> -1,  ' AND MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID))
  else if ConnectionID <> -1 then  aWherePart := 'WHERE '+
                                                 'MON$ATTACHMENT_ID = '+ALIntToStr(ConnectionID)
  else                             aWherePart := '';


  //clear the handle
  aDBHandle := nil;
  aTraHandle := nil;

  //start the TMP transaction
  TransactionStart(aDBHandle,
                   aTraHandle,
                   DefaultReadTPB);
  Try

    //get the data from the monitoring table
    aXMLDATA := TALXmlDocument.create('root');
    try

      SelectData('SELECT '+
                   aSelectPart +
                 'FROM ' +
                   aFromPart + ' ' +
                 aJoinPart +
                 aWherePart,
                 'rec',
                 aXMLDATA.DocumentElement,
                 ALDefaultFormatSettings,
                 aDBHandle,
                 aTraHandle);

      if aXMLDATA.DocumentElement.ChildNodes.Count <> 1 then raise Exception.Create('Can not get the monitoring stats');
      with aXMLDATA.DocumentElement.ChildNodes[0] do begin

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
      aXMLDATA.free;
    end;

    //commit the TMP transaction
    TransactionCommit(aDBHandle,
                      aTraHandle);

  Except

    //roolback the TMP transaction
    TransactionRollBack(aDBHandle,
                        aTraHandle);
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
  FLastConnectionWithStmtGarbage:= ALGettickCount64;
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
  FLastConnectionWithoutStmtGarbage := ALGettickCount64;
  FLastReadTransactionGarbage := ALGettickCount64;
  FLastReadStatementGarbage := ALGettickCount64;
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
Var aTmpDBHandle: IscDbHandle;
begin
  aTmpDBHandle := DBHandle;
  if not assigned(aTmpDBHandle) then aTmpDBHandle := AcquireConnectionWithoutStmt;
  try
    case item of
      isc_info_implementation,
      isc_info_base_level:
      result := byte(FLibrary.DatabaseInfoString(aTmpDBHandle, item, 8)[5]);
      else result := FLibrary.DatabaseInfoIntValue(aTmpDBHandle, AnsiChar(item));
    end;
  finally
    if not assigned(DBHandle) then ReleaseConnectionWithoutStmt(aTmpDBHandle);
  end;
end;

{*****************************************************************************}
function  TALFBXConnectionPoolClient.GetDataBaseInfoString(const item: Integer;
                                                           const DBHandle: IscDbHandle= nil): AnsiString;
Var aTmpDBHandle: IscDbHandle;
    aSize: byte;
    aData: AnsiString;
begin
  aTmpDBHandle := DBHandle;
  if not assigned(aTmpDBHandle) then aTmpDBHandle := AcquireConnectionWithoutStmt;
  try
    aData := FLibrary.DatabaseInfoString(aTmpDBHandle, item, 256);
    case Item of
      isc_info_cur_logfile_name,
      isc_info_wal_prv_ckpt_fname: begin
                                     aSize := byte(aData[4]);
                                     ALMove(aData[5], aData[1], aSize);
                                     SetLength(aData, aSize);
                                   end;
      else begin
        aSize := byte(aData[5]);
        ALMove(aData[6], aData[1], aSize);
        SetLength(aData, aSize);
      end;
    end;
    Result := AnsiString(aData);
  finally
    if not assigned(DBHandle) then ReleaseConnectionWithoutStmt(aTmpDBHandle);
  end;
end;

{*******************************************************************************}
function  TALFBXConnectionPoolClient.GetDataBaseInfoDateTime(const item: Integer;
                                                             const DBHandle: IscDbHandle= nil): TDateTime;
Var aTmpDBHandle: IscDbHandle;
begin
  aTmpDBHandle := DBHandle;
  if not assigned(aTmpDBHandle) then aTmpDBHandle := AcquireConnectionWithoutStmt;
  try
    result := FLibrary.DatabaseInfoDateTime(aTmpDBHandle, item);
  finally
    if not assigned(DBHandle) then ReleaseConnectionWithoutStmt(aTmpDBHandle);
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
Var aConnectionStatementPoolIterateExtData: TALFBXConnectionPoolClient_ConnectionStatementPoolIterateExtData;
    aConnectionWithStmtPoolContainer: TALFBXConnectionWithStmtPoolContainer;
    aTickCount: int64;
    i: integer;
Begin

  //synchronize the code
  FConnectionWithStmtPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    aTickCount := ALGetTickCount64;
    if aTickCount - FLastConnectionWithStmtGarbage > (60000 {every minutes})  then begin
      while FConnectionWithStmtPool.Count > 0 do begin
        aConnectionWithStmtPoolContainer := TALFBXConnectionWithStmtPoolContainer(FConnectionWithStmtPool[0]);
        if aTickCount - aConnectionWithStmtPoolContainer.Lastaccessdate > FConnectionMaxIdleTime then begin

          //first drop all statements
          aConnectionWithStmtPoolContainer.StatementPool.free;

          //now drop the connection
          Try
            fLibrary.DetachDatabase(aConnectionWithStmtPoolContainer.DBHandle);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;

          //Free the container.
          FConnectionWithStmtPool.Delete(0);

        end
        else break;
      end;
      FLastConnectionWithStmtGarbage := ALGetTickCount64;
    end;

    //acquire the new connection from the pool
    If FConnectionWithStmtPool.Count > 0 then begin
      aConnectionWithStmtPoolContainer := TALFBXConnectionWithStmtPoolContainer(FConnectionWithStmtPool[FConnectionWithStmtPool.count - 1]);
      DBHandle := aConnectionWithStmtPoolContainer.DBHandle;
      StatementPool := aConnectionWithStmtPoolContainer.StatementPool;
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
  if ALGetTickCount64 - StatementPool.LastGarbage > (60000 {every minutes})  then begin

    //create aExtData.LstNodeToDelete
    aConnectionStatementPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
    Try

      //init aExtData.ConnectionPoolClient and aExtData.TickCount
      aConnectionStatementPoolIterateExtData.ConnectionPoolClient := Self;
      aConnectionStatementPoolIterateExtData.TickCountCurrentdate := ALGetTickCount64;

      //iterate all StatementPool node
      StatementPool.Iterate(ALFBXConnectionPoolClient_ConnectionStatementPoolIterateFunct,
                            true,
                            @aConnectionStatementPoolIterateExtData);

      //delete all StatementPool node that have an empty pool
      for I := 0 to aConnectionStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
        StatementPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(aConnectionStatementPoolIterateExtData.LstBinaryTreeNodeToDelete[i]).id);

      //init the StatementPool.LastGarbage
      StatementPool.LastGarbage := ALGetTickCount64;

    Finally
      aConnectionStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
    End;

  end;

End;

{***************************************************************************************}
Procedure TALFBXConnectionPoolClient.ReleaseConnectionWithStmt(var DBHandle: IscDbHandle;
                                                               var StatementPool: TALFBXConnectionStatementPoolBinTree;
                                                               const CloseConnection: Boolean = False);
Var aConnectionWithStmtPoolContainer: TALFBXConnectionWithStmtPoolContainer;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(StatementPool) then raise exception.Create('StatementPool can not be null');

  //release the connection
  FConnectionWithStmtPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      aConnectionWithStmtPoolContainer := TALFBXConnectionWithStmtPoolContainer.Create;
      aConnectionWithStmtPoolContainer.DBHandle := DBHandle;
      aConnectionWithStmtPoolContainer.StatementPool:= StatementPool;
      aConnectionWithStmtPoolContainer.LastAccessDate := ALGetTickCount64;
      FConnectionWithStmtPool.add(aConnectionWithStmtPoolContainer);
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
Var aConnectionWithoutStmtPoolContainer: TALFBXConnectionWithoutStmtPoolContainer;
    aTickCount: int64;
Begin

  //synchronize the code
  FConnectionWithoutStmtPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    aTickCount := ALGetTickCount64;
    if aTickCount - FLastConnectionWithoutStmtGarbage > (60000 {every minutes})  then begin
      while FConnectionWithoutStmtPool.Count > 0 do begin
        aConnectionWithoutStmtPoolContainer := TALFBXConnectionWithoutStmtPoolContainer(FConnectionWithoutStmtPool[0]);
        if aTickCount - aConnectionWithoutStmtPoolContainer.Lastaccessdate > FConnectionMaxIdleTime then begin

          //drop the connection
          Try
            fLibrary.DetachDatabase(aConnectionWithoutStmtPoolContainer.DBHandle);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;

          //Free the container.
          FConnectionWithoutStmtPool.Delete(0);

        end
        else break;
      end;
      FLastConnectionWithoutStmtGarbage := ALGetTickCount64;
    end;

    //acquire the new connection from the pool
    If FConnectionWithoutStmtPool.Count > 0 then begin
      aConnectionWithoutStmtPoolContainer := TALFBXConnectionWithoutStmtPoolContainer(FConnectionWithoutStmtPool[FConnectionWithoutStmtPool.count - 1]);
      Result := aConnectionWithoutStmtPoolContainer.DBHandle;
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
Var aConnectionWithoutStmtPoolContainer: TALFBXConnectionWithoutStmtPoolContainer;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');

  //release the connection
  FConnectionWithoutStmtPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      aConnectionWithoutStmtPoolContainer := TALFBXConnectionWithoutStmtPoolContainer.Create;
      aConnectionWithoutStmtPoolContainer.DBHandle := DBHandle;
      aConnectionWithoutStmtPoolContainer.LastAccessDate := ALGetTickCount64;
      FConnectionWithoutStmtPool.add(aConnectionWithoutStmtPoolContainer);
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

Var aReadTransactionPool: TobjectList;
    aReadTransactionPoolContainer: TalFBXReadTransactionPoolContainer;

begin

  //typecast the aExtData
  With TALFBXConnectionPoolClient_ReadTransactionPoolIterateExtData(aExtData^) do begin

    //init aReadTransactionPool
    aReadTransactionPool := TALFBXStringKeyPoolBinTreeNode(aNode).Pool;

    //still their is some item in the aReadTransactionPool
    while aReadTransactionPool.Count > 0 do begin

      //init aReadTransactionPoolContainer
      aReadTransactionPoolContainer := TalFBXReadTransactionPoolContainer(aReadTransactionPool[0]);

      //if the aReadTransactionPoolContainer is expired
      if TickCountCurrentdate - aReadTransactionPoolContainer.Lastaccessdate > ConnectionPoolClient.TransactionMaxIdleTime then begin

        Try

          //commit the transaction and release the connection
          ConnectionPoolClient.TransactionCommit(aReadTransactionPoolContainer.DBHandle,
                                                 aReadTransactionPoolContainer.TraHandle);

        Except

          //roolback the transaction (and free the connection)
          if assigned(aReadTransactionPoolContainer.TraHandle) then ConnectionPoolClient.TransactionRollback(aReadTransactionPoolContainer.DBHandle,
                                                                                                             aReadTransactionPoolContainer.TraHandle,
                                                                                                             True) // const CloseConnection: Boolean = False

          //simply free the connection
          else if assigned(aReadTransactionPoolContainer.DBHandle) then ConnectionPoolClient.ReleaseConnectionWithoutStmt(aReadTransactionPoolContainer.DBHandle,
                                                                                                                           True); // const CloseConnection: Boolean = False

        End;

        //free the TransactionPoolContainer
        aReadTransactionPool.Delete(0);

      end

      //if the aReadTransactionPoolContainer is NOT expired, break the loop
      else break;

    end;

    //update LstNodeToDelete
    if aReadTransactionPool.Count = 0 then LstBinaryTreeNodeToDelete.Add(aNode);

  end;

end;

{************************************************************************************}
procedure TALFBXConnectionPoolClient.AcquireReadTransaction(var DBHandle: IscDbHandle;
                                                            var TraHandle: IscTrHandle;
                                                            const TPB: AnsiString);

Var aReadTransactionPoolIterateExtData: TALFBXConnectionPoolClient_ReadTransactionPoolIterateExtData;
    aReadTransactionPoolContainer: TalFBXReadTransactionPoolContainer;
    aStringKeyPoolBinTreeNode: TALStringKeyAVLBinaryTreeNode;
    i: integer;

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
    if ALGetTickCount64 - FLastReadTransactionGarbage > (60000 {every minutes})  then begin

      //create aExtData.LstNodeToDelete
      aReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
      Try

        //init aExtData.ConnectionPoolClient and aExtData.TickCount
        aReadTransactionPoolIterateExtData.ConnectionPoolClient := Self;
        aReadTransactionPoolIterateExtData.TickCountCurrentdate := ALGetTickCount64;

        //iterate all FReadTransactionPool node
        FReadTransactionPool.Iterate(ALFBXConnectionPoolClient_ReadTransactionPoolIterateFunct,
                                     true,
                                     @aReadTransactionPoolIterateExtData);

        //delete all FReadTransactionPool node that have an empty pool
        for I := 0 to aReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
          FReadTransactionPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(aReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete[i]).id);

        //init the FLastReadTransactionGarbage
        FLastReadTransactionGarbage := ALGetTickCount64;

      Finally
        aReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
      End;

    end;

    //look for a node with TPB id in the ReadTransactionPool. if not found create it
    aStringKeyPoolBinTreeNode := FReadTransactionPool.FindNode(TPB);
    if not assigned(aStringKeyPoolBinTreeNode) then begin
      aStringKeyPoolBinTreeNode := TALFBXStringKeyPoolBinTreeNode.Create;
      aStringKeyPoolBinTreeNode.ID := TPB;
      if not FReadTransactionPool.AddNode(aStringKeyPoolBinTreeNode) then begin
        aStringKeyPoolBinTreeNode.free;
        Raise Exception.Create('Can not add any more node in transaction pool binary tree');
      end;
    end;

    //if the if still some transaction in the queue
    If TALFBXStringKeyPoolBinTreeNode(aStringKeyPoolBinTreeNode).Pool.Count > 0 then begin
      with TALFBXStringKeyPoolBinTreeNode(aStringKeyPoolBinTreeNode) do begin
        aReadTransactionPoolContainer := TalFBXReadTransactionPoolContainer(Pool[Pool.count - 1]);
        DBHandle := aReadTransactionPoolContainer.DBHandle;
        TraHandle := aReadTransactionPoolContainer.TraHandle;
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

Var aReadTransactionPoolContainer: TalFBXReadTransactionPoolContainer;
    aStringKeyPoolBinTreeNode: TALStringKeyAVLBinaryTreeNode;

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
      aStringKeyPoolBinTreeNode := FReadTransactionPool.FindNode(TPB);
      if not assigned(aStringKeyPoolBinTreeNode) then begin
        aStringKeyPoolBinTreeNode := TALFBXStringKeyPoolBinTreeNode.Create;
        aStringKeyPoolBinTreeNode.ID := TPB;
        if not FReadTransactionPool.AddNode(aStringKeyPoolBinTreeNode) then begin
          aStringKeyPoolBinTreeNode.free;
          Raise Exception.Create('Can not add any more node in transaction pool binary tree');
        end;
      end;

      //add the item to the pool
      aReadTransactionPoolContainer := TalFBXReadTransactionPoolContainer.Create;
      aReadTransactionPoolContainer.DBHandle := DBHandle;
      aReadTransactionPoolContainer.TraHandle := TraHandle;
      aReadTransactionPoolContainer.LastAccessDate := ALGetTickCount64;
      TALFBXStringKeyPoolBinTreeNode(aStringKeyPoolBinTreeNode).Pool.add(aReadTransactionPoolContainer);

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

Var aReadStatementPool: TobjectList;
    aReadStatementPoolContainer: TalFBXReadStatementPoolContainer;

begin

  //typecast the aExtData
  With TALFBXConnectionPoolClient_ReadStatementPoolIterateExtData(aExtData^) do begin

    //init aReadStatementPool
    aReadStatementPool := TALFBXStringKeyPoolBinTreeNode(aNode).Pool;

    //still their is some item in the aReadStatementPool
    while aReadStatementPool.Count > 0 do begin

      //init aReadStatementPoolContainer
      aReadStatementPoolContainer := TalFBXReadStatementPoolContainer(aReadStatementPool[0]);

      //if the aReadStatementPoolContainer is expired
      if TickCountCurrentdate - aReadStatementPoolContainer.Lastaccessdate > ConnectionPoolClient.StatementMaxIdleTime then begin

        Try

          //drop the statement
          try
            ConnectionPoolClient.lib.DSQLFreeStatement(aReadStatementPoolContainer.StmtHandle, DSQL_drop);
          Except
            //what else we can do here ?
            //this can happen if connection lost for exemple
          end;
          aReadStatementPoolContainer.Sqlda.free;
          aReadStatementPoolContainer.StmtHandle := nil;
          aReadStatementPoolContainer.Sqlda := nil;

          //commit and release the connection
          ConnectionPoolClient.TransactionCommit(aReadStatementPoolContainer.DBHandle,
                                                 aReadStatementPoolContainer.TraHandle);

        Except

          //roolback the transaction (and free the connection)
          if assigned(aReadStatementPoolContainer.TraHandle) then ConnectionPoolClient.TransactionRollback(aReadStatementPoolContainer.DBHandle,
                                                                                                           aReadStatementPoolContainer.TraHandle,
                                                                                                           True) // const CloseConnection: Boolean = False

          //simply free the connection
          else if assigned(aReadStatementPoolContainer.DBHandle) then ConnectionPoolClient.ReleaseConnectionWithoutStmt(aReadStatementPoolContainer.DBHandle,
                                                                                                                         True); // const CloseConnection: Boolean = False

        End;

        //free the ReadStatementPoolContainer
        aReadStatementPool.Delete(0);

      end

      //if the aReadStatementPoolContainer is NOT expired, break the loop
      else break;

    end;

    //update LstNodeToDelete
    if aReadStatementPool.Count = 0 then LstBinaryTreeNodeToDelete.Add(aNode);

  end;

end;

{******************************************************************************}
procedure TALFBXConnectionPoolClient.AcquireReadStatement(const SQL: AnsiString;
                                                          var DBHandle: IscDbHandle;
                                                          var TraHandle: IscTrHandle;
                                                          var StmtHandle: IscStmtHandle;
                                                          var Sqlda: TALFBXSQLResult;
                                                          const TPB: AnsiString);

Var aReadStatementPoolIterateExtData: TALFBXConnectionPoolClient_ReadStatementPoolIterateExtData;
    aReadStatementPoolContainer: TalFBXReadStatementPoolContainer;
    aStringKeyPoolBinTreeNode: TALStringKeyAVLBinaryTreeNode;
    i: integer;

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
    if ALGetTickCount64 - FLastReadStatementGarbage > (60000 {every minutes})  then begin

      //create aExtData.LstNodeToDelete
      aReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
      Try

        //init aExtData.ConnectionPoolClient and aExtData.TickCount
        aReadStatementPoolIterateExtData.ConnectionPoolClient := Self;
        aReadStatementPoolIterateExtData.TickCountCurrentdate := ALGetTickCount64;

        //iterate all FReadStatementPool node
        FReadStatementPool.Iterate(ALFBXConnectionPoolClient_ReadStatementPoolIterateFunct,
                                   true,
                                   @aReadStatementPoolIterateExtData);

        //delete all FReadStatementPool node that have an empty pool
        for I := 0 to aReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
          FReadStatementPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(aReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete[i]).id);

        //init the FLastReadStatementGarbage
        FLastReadStatementGarbage := ALGetTickCount64;

      Finally
        aReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
      End;

    end;

    //look for a node with TPB + SQL id in the ReadStatementPool. if not found create it
    aStringKeyPoolBinTreeNode := FReadStatementPool.FindNode(TPB+'#'+SQL);
    if not assigned(aStringKeyPoolBinTreeNode) then begin
      aStringKeyPoolBinTreeNode := TALFBXStringKeyPoolBinTreeNode.Create;
      aStringKeyPoolBinTreeNode.ID := TPB+'#'+SQL;
      if not FReadStatementPool.AddNode(aStringKeyPoolBinTreeNode) then begin
        aStringKeyPoolBinTreeNode.free;
        Raise Exception.Create('Can not add any more node in statement pool binary tree');
      end;
    end;

    //if the if still some Statement in the queue
    If TALFBXStringKeyPoolBinTreeNode(aStringKeyPoolBinTreeNode).Pool.Count > 0 then begin
      with TALFBXStringKeyPoolBinTreeNode(aStringKeyPoolBinTreeNode) do begin
        aReadStatementPoolContainer := TalFBXReadStatementPoolContainer(Pool[Pool.count - 1]);
        DBHandle := aReadStatementPoolContainer.DBHandle;
        TraHandle := aReadStatementPoolContainer.TraHandle;
        StmtHandle := aReadStatementPoolContainer.StmtHandle;
        Sqlda := aReadStatementPoolContainer.Sqlda;
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

Var aReadStatementPoolContainer: TalFBXReadStatementPoolContainer;
    aStringKeyPoolBinTreeNode: TALStringKeyAVLBinaryTreeNode;

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
      aStringKeyPoolBinTreeNode := FReadStatementPool.FindNode(TPB+'#'+SQL);
      if not assigned(aStringKeyPoolBinTreeNode) then begin
        aStringKeyPoolBinTreeNode := TALFBXStringKeyPoolBinTreeNode.Create;
        aStringKeyPoolBinTreeNode.ID := TPB+'#'+SQL;
        if not FReadStatementPool.AddNode(aStringKeyPoolBinTreeNode) then begin
          aStringKeyPoolBinTreeNode.free;
          Raise Exception.Create('Can not add any more node in statement pool binary tree');
        end;
      end;

      //add the item to the pool
      aReadStatementPoolContainer := TalFBXReadStatementPoolContainer.Create;
      aReadStatementPoolContainer.DBHandle := DBHandle;
      aReadStatementPoolContainer.TraHandle := TraHandle;
      aReadStatementPoolContainer.StmtHandle := StmtHandle;
      aReadStatementPoolContainer.Sqlda := Sqlda;
      aReadStatementPoolContainer.LastAccessDate := ALGetTickCount64;
      TALFBXStringKeyPoolBinTreeNode(aStringKeyPoolBinTreeNode).Pool.add(aReadStatementPoolContainer);

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
Var aConnectionWithStmtPoolContainer: TALFBXConnectionWithStmtPoolContainer;
    aConnectionWithoutStmtPoolContainer: TALFBXConnectionWithoutStmtPoolContainer;
    aReadTransactionPoolIterateExtData: TALFBXConnectionPoolClient_ReadTransactionPoolIterateExtData;
    aReadStatementPoolIterateExtData: TALFBXConnectionPoolClient_ReadStatementPoolIterateExtData;
    i: integer;
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

      aReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
      Try

        //init aExtData.ConnectionPoolClient and aExtData.TickCount
        aReadStatementPoolIterateExtData.ConnectionPoolClient := Self;
        aReadStatementPoolIterateExtData.TickCountCurrentdate :=  high(int64); //this will cause to delete all node

        //iterate all FReadStatementPool node
        FReadStatementPool.Iterate(ALFBXConnectionPoolClient_ReadStatementPoolIterateFunct,
                                   true,
                                   @aReadStatementPoolIterateExtData);

        //delete all FReadStatementPool node that have an empty pool
        for I := 0 to aReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
          FReadStatementPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(aReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete[i]).id);

        //init the FLastReadStatementGarbage
        FLastReadStatementGarbage := ALGetTickCount64;

      Finally
        aReadStatementPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
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

      aReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete := TObjectlist.Create(False);
      Try

        //init aExtData.ConnectionPoolClient and aExtData.TickCount
        aReadTransactionPoolIterateExtData.ConnectionPoolClient := Self;
        aReadTransactionPoolIterateExtData.TickCountCurrentdate :=  high(int64); //this will cause to delete all node

        //iterate all FReadTransactionPool node
        FReadTransactionPool.Iterate(ALFBXConnectionPoolClient_ReadTransactionPoolIterateFunct,
                                     true,
                                     @aReadTransactionPoolIterateExtData);

        //delete all FReadTransactionPool node that have an empty pool
        for I := 0 to aReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete.Count - 1 do
          FReadTransactionPool.DeleteNode(TALFBXStringKeyPoolBinTreeNode(aReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete[i]).id);

        //init the FLastReadTransactionGarbage
        FLastReadTransactionGarbage := ALGetTickCount64;

      Finally
        aReadTransactionPoolIterateExtData.LstBinaryTreeNodeToDelete.Free;
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
        aConnectionWithStmtPoolContainer := TALFBXConnectionWithStmtPoolContainer(FConnectionWithStmtPool[FConnectionWithStmtPool.count - 1]);

        //first drop all statements
        aConnectionWithStmtPoolContainer.StatementPool.free;

        //now drop the connection
        Try
          fLibrary.DetachDatabase(aConnectionWithStmtPoolContainer.DBHandle);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;

        //Free the container.
        FConnectionWithStmtPool.Delete(FConnectionWithStmtPool.count - 1);

      end;
      FLastConnectionWithStmtGarbage := ALGetTickCount64;
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
        aConnectionWithoutStmtPoolContainer := TALFBXConnectionWithoutStmtPoolContainer(FConnectionWithoutStmtPool[FConnectionWithoutStmtPool.count - 1]);

        //now drop the connection
        Try
          fLibrary.DetachDatabase(aConnectionWithoutStmtPoolContainer.DBHandle);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;

        //free the container
        FConnectionWithoutStmtPool.Delete(FConnectionWithoutStmtPool.count - 1);

      end;
      FLastConnectionWithoutStmtGarbage := ALGetTickCount64;
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
var aTmpCloseConnection: Boolean;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');
  if not assigned(StatementPool) then raise exception.Create('Statement pool object can not be null');

  //rollback the connection
  aTmpCloseConnection := CloseConnection;
  Try
    Try
      FLibrary.TransactionRollback(TraHandle);
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
    TraHandle := nil;
    ReleaseConnectionWithStmt(DBHandle,
                              StatementPool,
                              aTmpCloseConnection);

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
var aTmpCloseConnection: Boolean;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');
  if not assigned(TraHandle) then raise exception.Create('Transaction handle can not be null');

  //rollback the connection
  aTmpCloseConnection := CloseConnection;
  Try
    Try
      FLibrary.TransactionRollback(TraHandle);
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
    TraHandle := nil;
    ReleaseConnectionWithoutStmt(DBHandle, aTmpCloseConnection);

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
Var aReleaseConnectionHandleonError: Boolean;
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
    aReleaseConnectionHandleonError := True;
  end
  else aReleaseConnectionHandleonError := False;
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
    if aReleaseConnectionHandleonError then TransactionRollBack(DBHandle,
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

  {$IFDEF undef}{$REGION 'Var Declarations'}{$ENDIF}
  Var aSqlpa: TALFBXSQLParams;
      aParamsIndex: integer;
      aBlobhandle: IscBlobHandle;
      aQueriesIndex: integer;
      aViewRec: TalXmlNode;
      aXmlDocument: TalXmlDocument;
      aTmpDBHandle: IscDbHandle;
      aTmpTraHandle: IscTrHandle;
      aTmpStmtHandle: IscStmtHandle;
      aTmpSqlda: TALFBXSQLResult;
      aTmpStatementPoolNode: TALStringKeyAVLBinaryTreeNode;
      aTmpTPB: AnsiString;
      aGDSCode: integer;
      aReadOnlyReadCommitedTPB: Boolean;
      aConnectionWasAcquiredFromPool: Boolean;
      aTransactionWasAcquiredFromPool: Boolean;
      aStatementWasAcquiredFromPool: Boolean;
      aStopWatch: TStopWatch;
      aCacheKey: ansiString;
      aCacheStr: ansiString;
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

  {$IFDEF undef}{$REGION 'InternalSelectDataFetchRows'}{$ENDIF}
  Procedure InternalSelectDataFetchRows;
  var aColumnIndex: integer;
      aRecIndex: integer;
      aRecAdded: integer;
      aContinue: Boolean;
      aNewRec: TalXmlNode;
      aValueRec: TalXmlNode;
      aUpdateRowTagByFieldValue: Boolean;
  Begin

    //init the aViewRec
    if (Queries[aQueriesIndex].ViewTag <> '') and (not assigned(aXmlDocument)) then aViewRec := XMLdata.AddChild(Queries[aQueriesIndex].ViewTag)
    else aViewRec := XMLdata;

    //init aUpdateRowTagByFieldValue
    if AlPos('&>',Queries[aQueriesIndex].RowTag) = 1 then begin
      delete(Queries[aQueriesIndex].RowTag, 1, 2);
      aUpdateRowTagByFieldValue := Queries[aQueriesIndex].RowTag <> '';
    end
    else aUpdateRowTagByFieldValue := False;

    //retrieve all row
    aRecIndex := 0;
    aRecAdded := 0;
    while Flibrary.DSQLFetch(aTmpDBHandle, aTmpTraHandle, aTmpStmtHandle, FSQLDIALECT, aTmpSqlda) do begin

      //process if > Skip
      inc(aRecIndex);
      If aRecIndex > Queries[aQueriesIndex].Skip then begin

        //init NewRec
        if (Queries[aQueriesIndex].RowTag <> '') and (not assigned(aXmlDocument)) then aNewRec := aViewRec.AddChild(Queries[aQueriesIndex].RowTag)
        Else aNewRec := aViewRec;

        //loop throught all column
        For aColumnIndex := 0 to aTmpSqlda.FieldCount - 1 do begin
          aValueRec := aNewRec.AddChild(ALlowercase(aTmpSqlda.AliasName[aColumnIndex]));
          if (aTmpSQLDA.SQLType[aColumnIndex] = SQL_BLOB) then avalueRec.ChildNodes.Add(
                                                                                        avalueRec.OwnerDocument.CreateNode(
                                                                                                                           GetFieldValue(aTmpSqlda,
                                                                                                                                         aTmpDBHandle,
                                                                                                                                         aTmpTRAHandle,
                                                                                                                                         aColumnIndex,
                                                                                                                                         FormatSettings),
                                                                                                                           ntCData
                                                                                                                          )
                                                                                        )
          else aValueRec.Text := GetFieldValue(aTmpSqlda,
                                               aTmpDBHandle,
                                               aTmpTRAHandle,
                                               aColumnIndex,
                                               FormatSettings);
          if aUpdateRowTagByFieldValue and (aValueRec.NodeName=aNewRec.NodeName) then aNewRec.NodeName := ALLowerCase(aValueRec.Text);
        end;

        //handle OnNewRowFunct
        if assigned(OnNewRowFunct) then begin
          aContinue := True;
          OnNewRowFunct(aNewRec, Queries[aQueriesIndex].ViewTag, ExtData, aContinue);
          if Not aContinue then Break;
        end;

        //free the node if aXmlDocument
        if assigned(aXmlDocument) then aXmlDocument.DocumentElement.ChildNodes.Clear;

        //handle the First
        inc(aRecAdded);
        If (Queries[aQueriesIndex].First > 0) and (aRecAdded >= Queries[aQueriesIndex].First) then Break;

      end;

    end;

  end;
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

begin

  {$IFDEF undef}{$REGION 'exit if no SQL'}{$ENDIF}
  if length(Queries) = 0 then Exit;
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

  {$IFDEF undef}{$REGION 'Check the params'}{$ENDIF}
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
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

  {$IFDEF undef}{$REGION 'init OnNewRowFunct/XMLDATA'}{$ENDIF}
  if assigned(OnNewRowFunct) then XMLDATA := nil;
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

  {$IFDEF undef}{$REGION 'clear the XMLDATA'}{$ENDIF}
  if assigned(XMLDATA) then aXmlDocument := Nil
  else begin
    aXmlDocument := TALXmlDocument.create('root');
    XMLDATA := aXmlDocument.DocumentElement;
  end;
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

  Try

    {$IFDEF undef}{$REGION 'init local variable'}{$ENDIF}
    aTmpDBHandle := DBHandle;
    aTmpTraHandle := TraHandle;
    aTmpStmtHandle := nil;
    aTmpSqlda := nil;
    aTmpTPB := TPB;
    if aTmpTPB = '' then aTmpTPB := fDefaultReadTPB;
    aReadOnlyReadCommitedTPB := (alpos(isc_tpb_read_committed,aTmpTPB) > 0) and
                                (alpos(isc_tpb_read, aTmpTPB) > 0) and
                                (alpos(isc_tpb_write, aTmpTPB) <= 0);
    aConnectionWasAcquiredFromPool := False;
    aTransactionWasAcquiredFromPool := False;
    aStatementWasAcquiredFromPool := False;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    Try

      {$IFDEF undef}{$REGION 'init aStopWatch'}{$ENDIF}
      aStopWatch := TstopWatch.Create;
      {$IFDEF undef}{$ENDREGION}{$ENDIF}

      {$IFDEF undef}{$REGION 'loop on all the SQL'}{$ENDIF}
      For aQueriesIndex := 0 to length(Queries) - 1 do begin

        {$IFDEF undef}{$REGION 'Handle the CacheThreshold'}{$ENDIF}
        aCacheKey := '';
        If (Queries[aQueriesIndex].CacheThreshold > 0) and
           (not assigned(aXmlDocument)) and
           (((length(Queries) = 1) and
             (XMLdata.ChildNodes.Count = 0)) or  // else the save will not work
            (Queries[aQueriesIndex].ViewTag <> '')) then begin

          //try to load from from cache
          aCacheKey := ALStringHashSHA1(Queries[aQueriesIndex].RowTag + '#' +
                                        alinttostr(Queries[aQueriesIndex].Skip) + '#' +
                                        alinttostr(Queries[aQueriesIndex].First) + '#' +
                                        ALGetFormatSettingsID(FormatSettings) + '#' +
                                        Queries[aQueriesIndex].SQL);
          if loadcachedData(aCacheKey, aCacheStr) then begin

            //init the aViewRec
            if (Queries[aQueriesIndex].ViewTag <> '') then aViewRec := XMLdata.AddChild(Queries[aQueriesIndex].ViewTag)
            else aViewRec := XMLdata;

            //assign the tmp data to the XMLData
            aViewRec.LoadFromXML(aCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

            //go to the next loop
            continue;

          end;

        end;
        {$IFDEF undef}{$ENDREGION}{$ENDIF}

        {$IFDEF undef}{$REGION 'Reset aStopWatch'}{$ENDIF}
        aStopWatch.Reset;
        aStopWatch.Start;
        {$IFDEF undef}{$ENDREGION}{$ENDIF}

        {$IFDEF undef}{$REGION 'Create/acquire the connection/transaction/statement'}{$ENDIF}

        {$IFDEF undef}{$REGION 'init aTmpStatementPoolNode'}{$ENDIF}
        if assigned(StatementPool) then aTmpStatementPoolNode := StatementPool.FindNode(Queries[aQueriesIndex].SQL)
        else aTmpStatementPoolNode := nil;
        {$IFDEF undef}{$ENDREGION}{$ENDIF}

        {$IFDEF undef}{$REGION 'if aTmpStatementPoolNode is null'}{$ENDIF}
        if not assigned(aTmpStatementPoolNode) then begin

          {$IFDEF undef}{$REGION 'if it''s a readonly Read Commited TPB AND DBHandle = nil'}{$ENDIF}
          if (aReadOnlyReadCommitedTPB) and
             (not assigned(DBHandle)) then begin

            //not assigned(DBHandle) mean that the aTmpDBHandle and aTmpTraHandle
            //are nil or assigned in previous loop only throught
            //AcquireReadTransaction or AcquireReadStatement (but if via AcquireReadStatement
            //then it's was already releasead ad the end of the current loop)

            {$IFDEF undef}{$REGION 'if their is some params'}{$ENDIF}
            if length(Queries[aQueriesIndex].Params) > 0 then begin

              //release previous connection if need
              if aTransactionWasAcquiredFromPool then begin
                ReleaseReadTransaction(aTmpDBHandle,
                                       aTmpTraHandle,
                                       aTmpTPB);
                aTransactionWasAcquiredFromPool := False;
              end;

              //acquire a statement from the pool
              AcquireReadStatement(Queries[aQueriesIndex].SQL,
                                   aTmpDBHandle,
                                   aTmpTraHandle,
                                   aTmpStmtHandle,
                                   aTmpSqlda,
                                   aTmpTPB);
              aStatementWasAcquiredFromPool := True;

            end
            {$IFDEF undef}{$ENDREGION}{$ENDIF}

            {$IFDEF undef}{$REGION 'if their is NO param'}{$ENDIF}
            else begin

              //acquire a transaction in the pool
              if not aTransactionWasAcquiredFromPool then begin
                AcquireReadTransaction(aTmpDBHandle,
                                       aTmpTraHandle,
                                       aTmpTPB);
                aTransactionWasAcquiredFromPool := True;
              end;

              //Prepare the statement
              Prepare(Queries[aQueriesIndex].SQL,
                      aTmpDBHandle,
                      aTmpTraHandle,
                      aTmpStmtHandle,
                      aTmpSqlda,
                      aTmpTPB);
              aStatementWasAcquiredFromPool := False;

            end;
            {$IFDEF undef}{$ENDREGION}{$ENDIF}

          end
          {$IFDEF undef}{$ENDREGION}{$ENDIF}

          {$IFDEF undef}{$REGION 'if it''s NOT a readonly Read Commited TPB or DBHandle <> nil'}{$ENDIF}
          else begin

            //assigned(DBHandle) or NOT a readonly Read Commited TPB mean that the aTmpDBHandle and aTmpTraHandle
            //are nil or given via the params or assigned in previous loop only throught TransactionStart

            //Start the transaction
            if not assigned(aTmpDBHandle) then begin
              TransactionStart(aTmpDBHandle,
                               aTmpTraHandle,
                               aTmpTPB);
              aConnectionWasAcquiredFromPool := True;
            end;

            //prepare the statement
            Prepare(Queries[aQueriesIndex].SQL,
                    aTmpDBHandle,
                    aTmpTraHandle,
                    aTmpStmtHandle,
                    aTmpSqlda,
                    aTmpTPB);
            aStatementWasAcquiredFromPool := False;

          end;
          {$IFDEF undef}{$ENDREGION}{$ENDIF}

        end
        {$IFDEF undef}{$ENDREGION}{$ENDIF}

        {$IFDEF undef}{$REGION 'else if aTmpStatementPoolNode is NOT null'}{$ENDIF}
        else begin

          //if aTmpStatementPoolNode is NOT null then DBHandle and TraHandle are not null too
          //so just init the aTmpStmtHandle and aTmpSqlda from aTmpStatementPoolNode

          with TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode) do begin
            aTmpStmtHandle := StmtHandle;
            aTmpSqlda := Sqlda;
          end;

        end;
        {$IFDEF undef}{$ENDREGION}{$ENDIF}

        {$IFDEF undef}{$ENDREGION}{$ENDIF}

        try

          {$IFDEF undef}{$REGION 'Execute the statement'}{$ENDIF}

          {$IFDEF undef}{$REGION 'if their is params'}{$ENDIF}
          if length(Queries[aQueriesIndex].Params) > 0 then begin

            //create the aSqlpa object
            aSqlpa := TALFBXSQLParams.Create(fCharSet);

            try

              //loop throught all Params Fields
              for aParamsIndex := 0 to length(Queries[aQueriesIndex].Params) - 1 do begin

                //with current Params Fields
                with Queries[aQueriesIndex].Params[aParamsIndex] do begin

                  //isnull
                  if IsNull then begin
                    aSqlpa.AddFieldType('', uftVarchar);
                    aSqlpa.IsNull[aParamsIndex] := True;
                  end

                  //IsBlob
                  else if length(value) > high(smallint) then begin
                    aSqlpa.AddFieldType('', uftBlob);
                    aBlobhandle := nil;
                    aSqlpa.AsQuad[aParamsIndex] := Flibrary.BlobCreate(aTmpDBHandle,aTmpTraHandle,aBlobHandle);
                    Try
                      FLibrary.BlobWriteString(aBlobHandle,Value);
                    Finally
                      FLibrary.BlobClose(aBlobHandle);
                    End;
                  end

                  //all the other
                  else begin
                    aSqlpa.AddFieldType('', uftVarchar);
                    aSqlpa.AsAnsiString[aParamsIndex] := Value;
                  end;

                end;

              end;

              //execute the sql with the params
              FLibrary.DSQLExecute(aTmpTraHandle, aTmpStmtHandle, FSQLDIALECT, asqlpa);

              //fetch the rows
              InternalSelectDataFetchRows;

            finally
              asqlpa.free;
            end;

          end
          {$IFDEF undef}{$ENDREGION}{$ENDIF}

          {$IFDEF undef}{$REGION 'if their is NO params'}{$ENDIF}
          else begin

            //execute the SQL wihtout params
            FLibrary.DSQLExecute(aTmpTraHandle, aTmpStmtHandle, FSQLDIALECT, nil);

            //fetch the rows
            InternalSelectDataFetchRows;

          end;
          {$IFDEF undef}{$ENDREGION}{$ENDIF}

          {$IFDEF undef}{$ENDREGION}{$ENDIF}

        finally

          {$IFDEF undef}{$REGION 'drop/close/release the statement'}{$ENDIF}

          {$IFDEF undef}{$REGION 'If NO StatementPool was given in params'}{$ENDIF}
          if not assigned(StatementPool) then begin

            {$IFDEF undef}{$REGION 'if the statement was acquired from the pool - CLOSE IT and RELEASE IT'}{$ENDIF}
            if aStatementWasAcquiredFromPool then begin

              try

                //close the cursor
                Flibrary.DSQLFreeStatement(aTmpStmtHandle, DSQL_Close);

                //release the statement in the pool
                ReleaseReadStatement(Queries[aQueriesIndex].SQL,
                                     aTmpDBHandle,
                                     aTmpTraHandle,
                                     aTmpStmtHandle,
                                     aTmpSqlda,
                                     aTmpTPB);

              Except

                //free the statement in the pool
                ReleaseReadStatement(Queries[aQueriesIndex].SQL,
                                     aTmpDBHandle,
                                     aTmpTraHandle,
                                     aTmpStmtHandle,
                                     aTmpSqlda,
                                     aTmpTPB,
                                     true); // CloseConnection

              end;

            end
            {$IFDEF undef}{$ENDREGION}{$ENDIF}

            {$IFDEF undef}{$REGION 'else if the statement was NOT acquired from the pool - DROP IT'}{$ENDIF}
            else begin

              try
                Flibrary.DSQLFreeStatement(aTmpStmtHandle, DSQL_drop);
              Except
                //what else we can do here ?
                //this can happen if connection lost for exemple
                //i preferre to hide this exception to not hide previous exception
              end;
              aTmpSqlda.free;

            end;
            {$IFDEF undef}{$ENDREGION}{$ENDIF}

          end
          {$IFDEF undef}{$ENDREGION}{$ENDIF}

          {$IFDEF undef}{$REGION 'Else if a statementPool was given in the params'}{$ENDIF}
          else begin

            //add to the statement pool only if their is some params or
            //if assigned aTmpStatementPoolNode
            if (length(Queries[aQueriesIndex].Params) > 0) or
               (assigned(aTmpStatementPoolNode)) then begin

              //first close the statement
              try
                Flibrary.DSQLFreeStatement(aTmpStmtHandle, DSQL_close);
              Except
                //what else we can do here ?
                //this can happen if connection lost for exemple
                //i preferre to hide this exception to not hide previous exception
              end;

              //if the statement was already in the pool, simply update it LastAccessDate
              if assigned(aTmpStatementPoolNode) then TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode).LastAccessDate := AlGetTickCount64

              //else add it to the statement pool
              else begin
                aTmpStatementPoolNode := TALFBXConnectionStatementPoolBinTreeNode.Create;
                aTmpStatementPoolNode.ID := Queries[aQueriesIndex].SQL;
                TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode).Lib := Lib;
                TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode).StmtHandle := aTmpStmtHandle;
                TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode).Sqlda := aTmpSqlda;
                if not StatementPool.AddNode(aTmpStatementPoolNode) then aTmpStatementPoolNode.Free; // don't raise any exception to not hide previous error
              end;

            end

            //else drop the statements handle
            else begin

              try
                Flibrary.DSQLFreeStatement(aTmpStmtHandle, DSQL_drop);
              Except
                //what else we can do here ?
                //this can happen if connection lost for exemple
                //i preferre to hide this exception to not hide previous exception
              end;
              aTmpSqlda.free;

            end;

          end;
          {$IFDEF undef}{$ENDREGION}{$ENDIF}

          {$IFDEF undef}{$REGION 'nil aTmpStmtHandle and aTmpSqlda'}{$ENDIF}
          aTmpStmtHandle := nil;
          aTmpSqlda := nil;
          {$IFDEF undef}{$ENDREGION}{$ENDIF}

          {$IFDEF undef}{$ENDREGION}{$ENDIF}

        end;

        {$IFDEF undef}{$REGION 'do the OnSelectDataDone'}{$ENDIF}
        aStopWatch.Stop;
        OnSelectDataDone(Queries[aQueriesIndex],
                         aStopWatch.Elapsed.TotalMilliseconds);
        {$IFDEF undef}{$ENDREGION}{$ENDIF}

        {$IFDEF undef}{$REGION 'save to the cache'}{$ENDIF}
        If aCacheKey <> '' then begin

          //save the data
          aViewRec.SaveToXML(aCacheStr, true{SaveOnlyChildNodes});
          SaveDataToCache(aCacheKey,
                          Queries[aQueriesIndex].CacheThreshold,
                          aCacheStr);

        end;
        {$IFDEF undef}{$ENDREGION}{$ENDIF}

      End;
      {$IFDEF undef}{$ENDREGION}{$ENDIF}

      {$IFDEF undef}{$REGION 'Commit or release the transaction'}{$ENDIF}
      if aTransactionWasAcquiredFromPool then ReleaseReadTransaction(aTmpDBHandle,
                                                                     aTmpTraHandle,
                                                                     aTmpTPB)
      else if aConnectionWasAcquiredFromPool then TransactionCommit(aTmpDBHandle,
                                                                    aTmpTraHandle);
      {$IFDEF undef}{$ENDREGION}{$ENDIF}

    Except

      {$IFDEF undef}{$REGION 'On Exception'}{$ENDIF}
      On E: Exception do begin

        {get the gdscode}
        if E is EALFBXError then aGDSCode := (E as EALFBXError).GDSCode
        else aGDSCode := -1;

        //rollback the transaction and release the connection if owned
        if aTransactionWasAcquiredFromPool then ReleaseReadTransaction(aTmpDBHandle,
                                                                       aTmpTraHandle,
                                                                       aTmpTPB,
                                                                       GetCloseConnectionByErrCode(aGDSCode))
        else if aConnectionWasAcquiredFromPool then TransactionRollback(aTmpDBHandle,
                                                                        aTmpTraHandle,
                                                                        GetCloseConnectionByErrCode(aGDSCode));

        //Database @1 shutdown
        if (aGDSCode = isc_shutdown) or        // Database @1 shutdown
           (aGDSCode = isc_shutinprog)         // Database @1 shutdown in progress
        then ReleaseAllConnections(False);

        //raise the error
        raise;

      end;
      {$IFDEF undef}{$ENDREGION}{$ENDIF}

    End;

  Finally

    {$IFDEF undef}{$REGION 'Free aXmlDocument'}{$ENDIF}
    if assigned(aXmlDocument) then aXmlDocument.free;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := Query;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  aSelectDataQUERIES[0].skip := Skip;
  aSelectDataQUERIES[0].First := First;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := Query;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  aSelectDataQUERIES[0].RowTag := RowTag;
  aSelectDataQUERIES[0].skip := Skip;
  aSelectDataQUERIES[0].First := First;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  aSelectDataQUERIES[0].RowTag := RowTag;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
    i: integer;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  setlength(aSelectDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    aSelectDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    aSelectDataQUERIES[0].params[i].Value := Params[i];
    aSelectDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  aSelectDataQUERIES[0].RowTag := RowTag;
  aSelectDataQUERIES[0].skip := Skip;
  aSelectDataQUERIES[0].First := First;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
    i: integer;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  setlength(aSelectDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    aSelectDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    aSelectDataQUERIES[0].params[i].Value := Params[i];
    aSelectDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  aSelectDataQUERIES[0].RowTag := RowTag;
  SelectData(aSelectDataQUERIES,
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
var aSelectDataQUERIES: TalFBXClientSelectDataQUERIES;
    i: integer;
begin
  setlength(aSelectDataQUERIES,1);
  aSelectDataQUERIES[0] := TALFBXClientSelectDataQUERY.Create;
  aSelectDataQUERIES[0].Sql := Sql;
  setlength(aSelectDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    aSelectDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    aSelectDataQUERIES[0].params[i].Value := Params[i];
    aSelectDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  SelectData(aSelectDataQUERIES,
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

  {$IFDEF undef}{$REGION 'Var Declarations'}{$ENDIF}
  Var aSqlpa: TALFBXSQLParams;
      aBlobhandle: IscBlobHandle;
      aParamsIndex: integer;
      aQueriesIndex: integer;
      aTmpDBHandle: IscDbHandle;
      aTmpTraHandle: IscTrHandle;
      aTmpStmtHandle: IscStmtHandle;
      aTmpSqlda: TALFBXSQLResult;
      aTmpStatementPool: TALFBXConnectionStatementPoolBinTree;
      aTmpStatementPoolNode: TALStringKeyAVLBinaryTreeNode;
      aTmpTPB: AnsiString;
      aGDSCode: integer;
      aConnectionWasAcquiredFromPool: Boolean;
      aStopWatch: TStopWatch;
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

begin

  {$IFDEF undef}{$REGION 'Exit if no SQL'}{$ENDIF}
  if length(Queries) = 0 then Exit;
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

  {$IFDEF undef}{$REGION 'Check the params'}{$ENDIF}
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
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

  {$IFDEF undef}{$REGION 'init local variable'}{$ENDIF}
  aTmpDBHandle := DBHandle;
  aTmpTraHandle := TraHandle;
  aTmpStmtHandle := nil;
  aTmpSqlda := nil;
  aTmpTPB := TPB;
  if aTmpTPB = '' then aTmpTPB := fDefaultWriteTPB;
  aTmpStatementPool := nil;
  if not assigned(aTmpDBHandle) then begin
    TransactionStart(aTmpDBHandle,
                     aTmpTraHandle,
                     aTmpStatementPool,
                     TPB);
    aConnectionWasAcquiredFromPool := true;
  end
  else begin
    aTmpStatementPool := StatementPool;
    aConnectionWasAcquiredFromPool := False;
  end;
  {$IFDEF undef}{$ENDREGION}{$ENDIF}

  try

    {$IFDEF undef}{$REGION 'init aStopWatch'}{$ENDIF}
    aStopWatch := TstopWatch.Create;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'loop on all the SQL'}{$ENDIF}
    For aQueriesIndex := 0 to length(Queries) - 1 do begin

      {$IFDEF undef}{$REGION 'Reset aStopWatch'}{$ENDIF}
      aStopWatch.Reset;
      aStopWatch.Start;
      {$IFDEF undef}{$ENDREGION}{$ENDIF}

      {$IFDEF undef}{$REGION 'init aTmpStatementPoolNode'}{$ENDIF}
      if assigned(aTmpStatementPool) then aTmpStatementPoolNode := aTmpStatementPool.FindNode(Queries[aQueriesIndex].SQL)
      else aTmpStatementPoolNode := nil;
      {$IFDEF undef}{$ENDREGION}{$ENDIF}

      {$IFDEF undef}{$REGION 'if their is params or aTmpStatementPoolNode <> nil'}{$ENDIF}
      if (length(Queries[aQueriesIndex].Params) > 0) or
         (assigned(aTmpStatementPoolNode)) then begin

        {$IFDEF undef}{$REGION 'Prepare or acquire the statement'}{$ENDIF}
        //if aTmpStatementPoolNode found
        if assigned(aTmpStatementPoolNode) then begin
          with TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode) do begin
            aTmpStmtHandle := StmtHandle;
            aTmpSqlda := Sqlda;
          end;
        end

        //if aTmpStatementPoolNode was not founded (prepare the statement)
        else begin
          Prepare(Queries[aQueriesIndex].SQL,
                  aTmpDBHandle,          //not nil => will not change
                  aTmpTraHandle,         //not nil => will not change
                  aTmpStmtHandle,        //nil => will be assigned
                  aTmpSqlda,             //nil => will be assigned
                  aTmpTPB);
        end;
       {$IFDEF undef}{$ENDREGION}{$ENDIF}

        try

          {$IFDEF undef}{$REGION 'Execute the statement'}{$ENDIF}
          //create the aSqlpa object
          aSqlpa := TALFBXSQLParams.Create(fCharSet);
          try

            //loop throught all Params Fields
            for aParamsIndex := 0 to length(Queries[aQueriesIndex].Params) - 1 do begin

              //with current Params Fields
              with Queries[aQueriesIndex].Params[aParamsIndex] do begin

                //isnull
                if IsNull then begin
                  aSqlpa.AddFieldType('', uftVarchar);
                  aSqlpa.IsNull[aParamsIndex] := True;
                end

                //IsBlob
                else if length(value) > high(smallint) then begin
                  aSqlpa.AddFieldType('', uftBlob);
                  aBlobhandle := nil;
                  aSqlpa.AsQuad[aParamsIndex] := Flibrary.BlobCreate(aTmpDBHandle,aTmpTraHandle,aBlobHandle);
                  Try
                    FLibrary.BlobWriteString(aBlobHandle,Value);
                  Finally
                    FLibrary.BlobClose(aBlobHandle);
                  End;
                end

                //all the other
                else begin
                  aSqlpa.AddFieldType('', uftVarchar);
                  aSqlpa.AsAnsiString[aParamsIndex] := Value;
                end;

              end;

            end;

            //execute the SQL
            FLibrary.DSQLExecute(aTmpTraHandle, aTmpStmtHandle, FSQLDIALECT, aSqlpa);

          finally
            aSqlpa.free;
          end;
         {$IFDEF undef}{$ENDREGION}{$ENDIF}

        finally

          {$IFDEF undef}{$REGION 'drop or pool the statement'}{$ENDIF}

          //if the statement was already in the pool, simply update it LastAccessDate
          if assigned(aTmpStatementPoolNode) then TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode).LastAccessDate := AlGetTickCount64

          //else add it to the statement pool OR DROP it
          else begin

            //if their is a StatementPool then add a new node with the prepared statement
            if assigned(aTmpStatementPool) then begin
              aTmpStatementPoolNode := TALFBXConnectionStatementPoolBinTreeNode.Create;
              aTmpStatementPoolNode.ID := Queries[aQueriesIndex].SQL;
              TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode).Lib := Lib;
              TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode).StmtHandle := aTmpStmtHandle;
              TALFBXConnectionStatementPoolBinTreeNode(aTmpStatementPoolNode).Sqlda := aTmpSqlda;
              if not aTmpStatementPool.AddNode(aTmpStatementPoolNode) then aTmpStatementPoolNode.Free; // don't raise any exception to not hide previous error
            end

            //if their is NO StatementPool then DROP the prepared statement
            else begin
              try
                Flibrary.DSQLFreeStatement(aTmpStmtHandle, DSQL_drop);
              Except
                //what else we can do here ?
                //this can happen if connection lost for exemple
                //i preferre to hide this exception to not hide previous exception
              end;
              aTmpSqlda.free;
            end;

          end;

          aTmpStmtHandle := nil;
          aTmpSqlda := nil;
          {$IFDEF undef}{$ENDREGION}{$ENDIF}

        end;

      end
      {$IFDEF undef}{$ENDREGION}{$ENDIF}

      {$IFDEF undef}{$REGION 'if their is NO params and aTmpStatementPoolNode = nil'}{$ENDIF}
      else begin

        //simply call DSQLExecuteImmediate
        Flibrary.DSQLExecuteImmediate(aTmpDBHandle, aTmpTraHandle, Queries[aQueriesIndex].SQL, FSQLDIALECT, nil);

      end;
      {$IFDEF undef}{$ENDREGION}{$ENDIF}

      {$IFDEF undef}{$REGION 'do the OnUpdateDataDone'}{$ENDIF}
      aStopWatch.Stop;
      OnUpdateDataDone(Queries[aQueriesIndex],
                       aStopWatch.Elapsed.TotalMilliseconds);
      {$IFDEF undef}{$ENDREGION}{$ENDIF}

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

    {$IFDEF undef}{$REGION 'Commit the transaction'}{$ENDIF}
    if aConnectionWasAcquiredFromPool then begin
      TransactionCommit(aTmpDBHandle,
                        aTmpTraHandle,
                        aTmpStatementPool);
    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

  Except

    {$IFDEF undef}{$REGION 'On Exception'}{$ENDIF}
    On E: Exception do begin

      {get the gdscode}
      if E is EALFBXError then aGDSCode := (E as EALFBXError).GDSCode
      else aGDSCode := -1;

      //rollback the transaction and release the connection if owned
      if aConnectionWasAcquiredFromPool then begin
        TransactionRollback(aTmpDBHandle,
                            aTmpTraHandle,
                            aTmpStatementPool,
                            GetCloseConnectionByErrCode(aGDSCode)); // const CloseConnection: Boolean = False
      end;

      //Database @1 shutdown
      if (aGDSCode = isc_shutdown) or        // Database @1 shutdown
         (aGDSCode = isc_shutinprog)         // Database @1 shutdown in progress
      then ReleaseAllConnections(False);

      //raise the error
      raise;

    end;
    {$IFDEF undef}{$ENDREGION}{$ENDIF}

  end;

end;

{***************************************************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(const Query: TALFBXClientUpdateDataQUERY;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const StatementPool: TALFBXConnectionStatementPoolBinTree = nil;
                                                const TPB: AnsiString = '');
Var aUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
begin
  setlength(aUpdateDataQUERIES,1);
  aUpdateDataQUERIES[0] := Query;
  UpdateData(aUpdateDataQUERIES,
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
Var aSQLsIndex : integer;
    aUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
begin
  setlength(aUpdateDataQUERIES,SQLs.Count);
  For aSQLsIndex := 0 to SQLs.Count - 1 do begin
    aUpdateDataQUERIES[aSQLsIndex] := TALFBXClientUpdateDataQUERY.Create;
    aUpdateDataQUERIES[aSQLsIndex].SQL := SQLs[aSQLsIndex];
  end;
  UpdateData(aUpdateDataQUERIES,
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
Var aUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
begin
  setlength(aUpdateDataQUERIES,1);
  aUpdateDataQUERIES[0] := TALFBXClientUpdateDataQUERY.Create;
  aUpdateDataQUERIES[0].SQL := SQL;
  UpdateData(aUpdateDataQUERIES,
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
Var aUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
    i: integer;
begin
  setlength(aUpdateDataQUERIES,1);
  aUpdateDataQUERIES[0] := TALFBXClientUpdateDataQUERY.Create;
  aUpdateDataQUERIES[0].SQL := SQL;
  setlength(aUpdateDataQUERIES[0].params, length(Params));
  for i := 0 to length(Params) - 1 do begin
    aUpdateDataQUERIES[0].params[i] := TALFBXClientSQLParam.Create;
    aUpdateDataQUERIES[0].params[i].Value := Params[i];
    aUpdateDataQUERIES[0].params[i].IsNull := Params[i] = fNullString;
  end;
  UpdateData(aUpdateDataQUERIES,
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
Var aUpdateDataQUERIES: TalFBXClientUpdateDataQUERIES;
    i: integer;
begin
  setlength(aUpdateDataQUERIES,length(SQLs));
  for I := 0 to length(SQLs) - 1 do begin
    aUpdateDataQUERIES[i] := TALFBXClientUpdateDataQUERY.Create;
    aUpdateDataQUERIES[i].SQL := SQLs[i];
  end;
  UpdateData(aUpdateDataQUERIES,
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
  LastAccessDate := alGetTickCount64;
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
  LastGarbage := AlGetTickCount64;
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
Var aLst: TALStrings;
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
  aLst := TALStringList.Create;
  Try
    Alst.Text := ALTrim(alStringReplace(aEventNames,';',#13#10,[rfReplaceALL]));
    i := 0;
    while (i <= 14) and (i <= Alst.Count - 1) do begin
      fEventNamesArr[i] := ALTrim(Alst[i]);
      inc(i);
    end;
    fEventNamesCount := i;
    while i <= 14 do begin
      fEventNamesArr[i] := '';
      inc(i);
    end;
  Finally
    Alst.Free;
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
var aEventBuffer: PAnsiChar;
    aEventBufferLen: Smallint;
    aEventID: Integer;
    aStatusVector: TALFBXStatusVector;

    {-----------------------------}
    Procedure InternalFreeLocalVar;
    Begin
      //free the aEventID
      if aEventID <> 0 then begin
        FEventCanceled := True;
        Try
          ResetEvent(Fsignal);
          FLibrary.EventCancel(FDbHandle, aEventID);
          //in case the connection or fbserver crash the Fsignal will
          //be never signaled
          WaitForSingleObject(FSignal, 60000);
        Except
          //in case of error what we can do except suppose than the event was canceled ?
          //in anyway we will reset the FDbHandle after
        End;
        FEventCanceled := False;
      end;
      aEventID := 0;

      //free the aEventBuffer
      if assigned(aEventBuffer) then begin
        Try
          FLibrary.IscFree(aEventBuffer);
        Except
          //paranoia mode ... i never see it's can raise any error here
        End;
      end;
      aEventBuffer := nil;

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

var aCurrentEventIdx: integer;
    aMustResetDBHandle: Boolean;

begin
  //to be sure that the thread was stated
  fStarted := True;

  aEventBuffer := nil;
  aEventID := 0;
  aEventBufferLen := 0;
  aMustResetDBHandle := True;

  while not Terminated do begin
    Try

      //if the DBHandle is not assigned the create it
      //FDBHandle can not be assigned if for exemple
      //an error (disconnection happen)
      if aMustResetDBHandle then begin

        //set the FMustResetDBHandle to false
        aMustResetDBHandle := False;

        //free the local var
        InternalFreeLocalVar;

        //First init FDBHandle
        FLibrary.AttachDatabase(FDataBaseName,
                                FDBHandle,
                                fOpenConnectionParams);

        //register the EventBlock
        aEventBufferLen := FLibrary.EventBlock(aEventBuffer,
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
                            aEventID,
                            aEventBufferLen,
                            aEventBuffer,
                            @ALFBXEventCallback,
                            self);
        if WaitForSingleObject(FSignal, 60000) <> WAIT_OBJECT_0 then raise Exception.Create('Timeout in the first call to isc_que_events');
        FLibrary.EventCounts(aStatusVector,
                             aEventBufferLen,
                             aEventBuffer,
                             fResultBuffer);

        //set the FQueueEvent to false in case the next
        //WaitForSingleObject fired because of a timeout
        FQueueEvent := False;

        //the 2nd EventQueue
        ResetEvent(Fsignal);
        FLibrary.EventQueue(FdbHandle,
                            aEventID,
                            aEventBufferLen,
                            aEventBuffer,
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
        FLibrary.EventCounts(aStatusVector,
                             aEventBufferLen,
                             aEventBuffer,
                             fResultBuffer);

        //if it was the event
        for aCurrentEventIdx := 0 to 14 do
          if aStatusVector[aCurrentEventIdx] <> 0 then DoEvent(fEventNamesArr[aCurrentEventIdx],aStatusVector[aCurrentEventIdx]);

        //reset the FQueueEvent
        FQueueEvent := False;

        //start to listen again
        ResetEvent(Fsignal);
        FLibrary.EventQueue(FdbHandle,
                            aEventID,
                            aEventBufferLen,
                            aEventBuffer,
                            @ALFBXEventCallback,
                            self);

      end

      //it must be an error somewhere
      else aMustResetDBHandle := True;

    Except
      on E: Exception do begin
        //Reset the DBHandle
        aMustResetDBHandle := True;
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
