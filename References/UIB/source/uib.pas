(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(* Contributor: Olivier Guilbaud <oguilb@free.fr>                               *)
(*                                                                              *)
(********************************************************************************)

unit uib;

{$I uib.inc}

(*------------------------------------------------------------------------------
  This is a cascading programming style.

..............oOo...............................oOo.........oOo.................
 States        |    Operations                   |  Commands | Components
..............oOo...............................oOo.........oOo.................
 qsDataBase    |  BeginDataBase(L)               |           | TUIBDataBase
--------------------------------------------------------------------------------
 qsTransaction |  |-> BeginTransaction           |           | TUIBTransaction
--------------------------------------------------------------------------------
 qsExecImme    |      |-> BeginExecImme .........|.[ExecSQL] | TUIBQuery
 qsStatement   |      |-> BeginStatement         |           |
 qsPrepare     |      |   |-> BeginPrepare       |           |
 qsExecute     |      |   |   |-> BeginExecute   |.[Execute] |
               |      |   |   |   |-> Next ......|.[Open]    |
               |      |   |   |   |   |          |           |
               | R <- E   E   E   E   E          | [Fields]  |
               |          |   |   |   |          |           |
 qsExecute     |          |   |   |<- EndExecute |.[Close]   |
 qsPrepare     |          |   |<- EndPrepare     |           |
 qsStatement   |          |<- EndStatement       |           |
 -------------------------------------------------------------------------------
 qsTransaction |          EndTransaction         |           | TUIBTransaction
..............oOo...............................oOo.........oOo.................
 LEGEND
   E  = Except
   R  = Raise
   -> = Call
------------------------------------------------------------------------------*)

interface
uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF DELPHI14_UP} Rtti, {$ENDIF}
  SyncObjs, Classes, Contnrs, SysUtils, uiblib, uibase,
  uibsqlparser, uibconst;

type

 {Oo.........................................................................oO
                              TUIBComponent

      Synchronise Databases, Transactions and Queries.

    TUIBLibrary    | TUIBDatabase     | TUIBTransaction  | TUIBQuery
   ==========================================================================
    Lock <---------|------------------|------------------|-----------------o
                   | Lock <-----------|------------------|---------------o
                   |                  | Lock <-----------|-------------o
                   |                  |                  | Lock   <--o
                   |                  |                  | UnLock <--o
                   |                  | UnLock <---------|-------------o
                   | UnLock <---------|------------------|---------------o
    UnLock <-------|------------------|------------------|-----------------o

      Note: With Interbase 7, no need to synchronise anything but removing
      Synchronisation you have exactly the same performance than IB6.01 with
      Synchronisation on a single CPU !

  Oo.........................................................................oO}

{ All UIB components inherith from this class to encapsulate Critical Sections.
  Critical Sections make UIB THread Safe. }
{$IFDEF UIB_NO_COMPONENT}
  TUIBComponent = class(TObject) end;
{$ELSE}
  TUIBComponent = class(TComponent) end;
{$ENDIF}

  // Forward declarations
  TUIBTransaction = class;
  TUIBQuery = class;
  TUIBStatement = class;
  TUIBDataBase = class;
  TUIBEvents = class;

  { The list of MetaData Objects returned by TUIBDatabase.GetMetadata function. }
  TMetaDataOptions = class(TPersistent)
  private
    FObjects: TOIDDatabases;
    FTables: TOIDTables;
    FViews: TOIDViews;
    FProcedures: TOIDProcedures;
    FUDFs: TOIDUDFs;
    FRoles: TOIDRoles;
    FSysInfos: boolean;
  public
   { @exclude }
    constructor Create;
  published
    { Metadata objects (Procedure, Generator, Exception, UDF, Role). }
    property Objects: TOIDDatabases read FObjects write FObjects default ALLObjects;
    { Table properties (TableField, Primary, Foreign, TableTrigger, Unique, Index, Check)}
    property Tables: TOIDTables read FTables write FTables default ALLTables;
    { View properties (Fields & Triggers)}
    property Views: TOIDViews read FViews write FViews default AllViews;
    { Procedure properties (input & output parametters). }
    property Procedures: TOIDProcedures read FProcedures write FProcedures default AllProcedures;
    { UDFs properties (Fields). }
    property UDFs: TOIDUDFs read FUDFs write FUDFs default AllUDFs;
    { Roles properties (Grants). }
    property Roles: TOIDRoles read FRoles write FRoles default AllRoles;
    { Include System tables, triggers and domains. }
    property SysInfos: boolean read FSysInfos write FSysInfos default False;
  end;

  TShutdownOption = (sdCache, sdAttachment, sdTransaction, sdForce);
  TShutdownOptions = set of TShutdownOption;

  PTableOperation = ^TTableOperation;
  TTableOperation = packed record
    TableId: Word;
    Count: Integer;
  end;

  TOnInfoTableOpCount = procedure(Sender: TObject; TableOp: PTableOperation) of object;
  TOnInfoIntegerCount = procedure(Sender: TObject; Value: Integer) of object;
  TOnInfoStringCount = procedure(Sender: TObject; Value: string) of object;

  TUIBDataBase = class(TUIBComponent)
  private
    FLibrary: TUIBLibrary;
    FLiBraryName: TFileName;
    FDbHandle: IscDbHandle;
    FHandleShared: boolean;
    FParams: TStrings;
    FDatabaseName: TFileName;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FTransactions: TList;
    FStatements: TList;
    FOnConnectionLost: TNotifyEvent;
    FExceptions: TList;
    FMetadata: TObject;
    FEventNotifiers: TList;
    FCharacterSet: TCharacterSet;
    FSQLDialect: Word;
    FStoreInDFM: Boolean;

    FMetaDataOptions: TMetaDataOptions;
    FOnInfoReadSeqCount: TOnInfoTableOpCount;
    FOnInfoReadIdxCount: TOnInfoTableOpCount;
    FOnInfoUpdateCount: TOnInfoTableOpCount;
    FOnInfoInsertCount: TOnInfoTableOpCount;
    FOnInfoDeleteCount: TOnInfoTableOpCount;
    FOnInfoBackoutCount: TOnInfoTableOpCount;
    FOnInfoPurgeCount: TOnInfoTableOpCount;
    FOnInfoExpungeCount: TOnInfoTableOpCount;
    FOnInfoActiveTransactions: TOnInfoIntegerCount;
    FOnInfoLimbo: TOnInfoIntegerCount;
    FOnInfoUserNames: TOnInfoStringCount;

    function ReadParamString(Param: String; Default: String = ''): String;
    procedure WriteParamString(Param: String; Value: String);
    function ReadParamInteger(Param: String; Default: Integer): Integer;
    procedure WriteParamInteger(Param: String; Value: Integer);
    procedure SetParams(const Value: TStrings);
    procedure SetDatabaseName(const Value: TFileName);
    procedure SetConnected(const Value: boolean);
    function GetConnected: boolean;
    procedure SetSQLDialect(const Value: Integer);
    function GetSQLDialect: Integer;
    function GetCharacterSet: TCharacterSet;
    procedure SetCharacterSet(const Value: TCharacterSet);
    function GetPassWord: string;
    function GetUserName: string;
    procedure SetPassWord(const Value: string);
    procedure SetUserName(const Value: string);
    procedure AddTransaction(Transaction: TUIBTransaction);
    procedure RemoveTransaction(Transaction: TUIBTransaction);
    procedure ClearTransactions;
    procedure CloseTransactions;
    procedure AddStatement(Statement: TUIBStatement);
    procedure RemoveStatement(Statement: TUIBStatement);
    procedure ClearStatements;
    procedure CloseStatements;
    procedure SetDbHandle(const Value: IscDbHandle);
    procedure SetLibraryName(const Lib: TFileName);
    function GetTransactions(const Index: Cardinal): TUIBTransaction;
    function GetTransactionsCount: Cardinal;
    function GetSegmentSize: Word;
    procedure SetSegmentSize(const Value: Word);
    function GetShutdown: TShutdownOptions;
    procedure SetShutdown(const Value: TShutdownOptions);
    procedure doOnParamChange(Sender: TObject);

    procedure UnRegisterEvents;
    procedure RegisterEvents;
    procedure RemoveEventNotifier(Event: TUIBEvents);
    procedure AddEventNotifier(Event: TUIBEvents);

    function GetInfoIntValue(const item: Integer): integer;
{$IFDEF FB20_UP}
    function GetInfoDateTimeValue(const item: Integer): TDateTime;
{$ENDIF}
    function GetInfoBooleanValue(const item: Integer): boolean;
    function GetInfoStringValue(const item: integer): string;
    function GetInfoOperationsCount(const item: Integer): Integer;
    function GetInfoIntCount(const item: Integer): Integer;
    function GetInfoStringCount(const item: Integer): Integer;
    function GetInfoDbId(const Index: Integer): string;
    function GetRole: string;
    procedure SetRole(const Value: string);
    procedure ClearEvents;
  protected
    function CanConnect : Boolean; virtual;
    procedure DoOnConnectionLost(Lib: TUIBLibrary); virtual;
    procedure DoOnGetDBExceptionClass(Number: Integer; out Excep: EUIBExceptionClass); virtual;
  public
    { Constructor method. }
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
    { Destructor method. }
    destructor Destroy; override;
    { Execute a SQL statement without the need to have the database connected,
      it is usefull to create a database by SQL. }
    procedure ExecuteImmediate(const Statement: string);
    { Remove all Interbase Exception class registered using 'RegistedException'. }
    procedure ClearExceptions;
    { Associate an Interbase Exception with a Delphi exception, ID is the Exception Identifier number. }
    procedure RegisterException(Excpt: EUIBExceptionClass; ID: Integer); overload;
    { Associate an Interbase Exception with a Delphi exception, Name is the Interbase Exception name. }
    function RegisterException(Excpt: EUIBExceptionClass; const Name: string): Integer; overload;
    { Remove the Registered Exception number. }
    procedure UnRegisterException(Number: Integer);
    { Remove the Registered Exception class. }
    procedure UnRegisterExceptions(Excpt: EUIBExceptionClass);
    { Create a database with a default page size of 4096. }
    procedure CreateDatabase(DefaultCharacterSet: TCharacterSet; PageSize: Integer = 4096);
    { Drop the database. }
    procedure DropDatabase;
    { Return a TMetaDatabase class corresponding to the current connection. }
    function GetMetadata(Refresh: boolean = False): TObject;

    { Activate all Triggers }
    procedure ActiveAllTriggers;
    { Deactivate all Triggers }
    procedure DeactiveAllTriggers;
    { Recompute selectivity of all indices }
    procedure RecomputeSelectivityIndices;
    { Recompile all procedures, usefull when indices are modified }
    procedure RecompileAllProcedures;
    { Recompile all triggers, usefull when indices are modified }
    procedure RecompileAllTriggers;

{$IFDEF FB25_UP}
    { re-enables delivery of a cancel execution that was previously disabled.
      The 'cancel' state is effective by default, being initialized when the attachment is created.}
    function CancelEnable: Boolean;
    { disables execution of CancelRaise requests.
      It can be useful when your program is executing critical operations,
      such as cleanup, for example.}
    function CancelDisable: Boolean;
    { forcibly close client side of connection. Useful if you need to close a connection urgently.
      All active transactions will be rolled back by the server. 'Success' is always returned to the application.
      Use with care !}
    function CancelAbort: Boolean;
    { Usually fb_cancel_raise is called when you need to stop a long-running request.
      It is called from a separate thread, not from the signal handler,
      because it is not async signal safe.}
    function CancelRaise: Boolean;
{$ENDIF}

    { The DbHandle can be used to share the current connection with other Interbase components like IBX. }
    property DbHandle: IscDbHandle read FDbHandle write SetDbHandle;
    { Determine if the DbHandle is initialized by another component. }
    property IsHandleShared : boolean read FHandleShared;
    { List all transactions connected to the database component. }
    property Transactions[const Index: Cardinal]: TUIBTransaction read GetTransactions;
    { Number of connected transactions. }
    property TransactionsCount: Cardinal read GetTransactionsCount;
    { Can be used to access the low level API. }
    property Lib: TUIBLibrary read FLibrary;

    { Number of page reads. }
    property InfoReads: Integer index isc_info_reads read GetInfoIntValue;
    { Number of page writes. }
    property InfoWrites: Integer index isc_info_writes read GetInfoIntValue;
    { Number of reads from the memory buffer cache. }
    property InfoFetches: Integer index isc_info_fetches read GetInfoIntValue;
    { Number of writes to the memory buffer cache. }
    property InfoMarks: Integer index isc_info_marks read GetInfoIntValue;
    { Number of bytes per page of the attached database, use with
      "InfoAllocation" property to determine the size of the database. }
    property InfoPageSize: Integer index isc_info_page_size read GetInfoIntValue;
    { Number of memory buffers currently allocated. }
    property InfoNumBuffers: Integer index isc_info_num_buffers read GetInfoIntValue;
    { Page buffers from header page }
    property InfoSetPageBuffers: Integer index isc_info_set_page_buffers read GetInfoIntValue;
    { Amount of server memory (in bytes) currently in use. }
    property InfoCurrentMemory: Integer index isc_info_current_memory read GetInfoIntValue;
    { Maximum amount of memory (in bytes) used at one time since the first
      process attached to the database. }
    property InfoMaxMemory: Integer index isc_info_max_memory read GetInfoIntValue;
    { Attachment ID. }
    property InfoAttachmentId: Integer index isc_info_attachment_id read GetInfoIntValue;
    { On-disk structure (ODS) major version number. }
    property InfoOdsVersion: Integer index isc_info_ods_version read GetInfoIntValue;
    { On-disk structure (ODS) minor version number. }
    property InfoOdsMinorVersion: Integer index isc_info_ods_minor_version read GetInfoIntValue;
    { Number of database pages allocated. }
    property InfoAllocation: Integer index isc_info_allocation read GetInfoIntValue;
    { Number of transactions that are committed between "sweeps" to remove
      database record versions that are no longer needed. }
    property InfoSweepInterval: Integer index isc_info_sweep_interval read GetInfoIntValue;
    { false indicates space is reserved on each database page for holding
        backup versions of modified records [Default]
      true indicates no space is reserved for such records. }
    property InfoNoReserve: boolean index isc_info_no_reserve read GetInfoBooleanValue;
    { Specify the mode in which database writes are performed
      (false for asynchronous, true for synchronous). }
    property InfoForcedWrites: boolean index isc_info_forced_writes read GetInfoBooleanValue;
    { Number of database page errors. }
    property InfoPageErrors: Integer index isc_info_page_errors read GetInfoIntValue;
    { Number of Blob page errors. }
    property InfoBPageErrors: Integer index isc_info_bpage_errors read GetInfoIntValue;
    { Number of record level errors. }
    property InfoRecordErrors: Integer index isc_info_record_errors read GetInfoIntValue;
    { Number of data page errors. }
    property InfoDPageErrors: Integer index isc_info_dpage_errors read GetInfoIntValue;
    { Number of index page errors. }
    property InfoIPageErrors: Integer index isc_info_ipage_errors read GetInfoIntValue;
    { Number of pointer page errors. }
    property InfoPPageErrors: Integer index isc_info_ppage_errors read GetInfoIntValue;
    { Number of transaction page errors. }
    property InfoTPageErrors: Integer index isc_info_tpage_errors read GetInfoIntValue;
    { Database SQL Dialect (1,2,3). }
    property InfoDbSqlDialect: Integer index isc_info_db_sql_dialect read GetInfoIntValue;
    { Is the database read only ? }
    property InfoDbReadOnly: boolean index isc_info_db_read_only read GetInfoBooleanValue;
    { Database size in pages. }
    property InfoDbSizeInPages: Integer index isc_info_db_size_in_pages read GetInfoIntValue;
    { Database file name. }
    property InfoDbFileName: string index 1 read GetInfoDbId;
    { Database site name. }
    property InfoDbSiteName: string index 2 read GetInfoDbId;
    { Database implementation number, cf. isc_info_db_impl_XXX. }
    property InfoImplementation: Integer index isc_info_implementation read GetInfoIntValue;
    { Database version (level) number. }
    property InfoBaseLevel: Integer index isc_info_base_level read GetInfoIntValue;
    { Version identification string of the database implementation. }
    property InfoVersion: string index isc_info_isc_version read GetInfoStringValue;
    { Number of sequential sequential table scans (row reads) done on each
      table since the database was last attached. }
    property InfoReadSeqCount: Integer index isc_info_read_seq_count read GetInfoOperationsCount;
    { Number of reads done via an index since the database was last attached. }
    property InfoReadIdxCount: Integer index isc_info_read_idx_count read GetInfoOperationsCount;
    { Number of database updates since the database was last attached. }
    property InfoUpdateCount: Integer index isc_info_update_count read GetInfoOperationsCount;
    { Number of inserts into the database since the database was last attached. }
    property InfoInsertCount: Integer index isc_info_insert_count read GetInfoOperationsCount;
    { Number of database deletes since the database was last attached. }
    property InfoDeleteCount: Integer index isc_info_delete_count read GetInfoOperationsCount;
    { Number of removals of a version of a record. }
    property InfoBackoutCount: Integer index isc_info_backout_count read GetInfoOperationsCount;
    { Number of removals of old versions of fully mature records (records that
      are committed, so that older ancestor versions are no longer needed). }
    property InfoPurgeCount: Integer index isc_info_purge_count read GetInfoOperationsCount;
    { Number of removals of a record and all of its ancestors, for records
      whose deletions have been committed. }
    property InfoExpungeCount: Integer index isc_info_expunge_count read GetInfoOperationsCount;
    property InfoLimbo: integer index isc_info_limbo read GetInfoIntCount;
    { Number of users currently attached to the database.
      Use this property with the "OnInfoUserNames" event to retrieve the user names. }
    property InfoUserNames: Integer index isc_info_user_names read GetInfoStringCount;
  {$IFDEF FB102_UP}
    { Cached "oldest interesting" transaction. }
    property InfoOldestTransaction: Integer index isc_info_oldest_transaction read GetInfoIntValue;
    { Cached "oldest active" transaction. }
    property InfoOldestActive: Integer index isc_info_oldest_active read GetInfoIntValue;
    { Cached "oldest snapshot" of all active transactions. }
    property InfoOldestSnapshot: Integer index isc_info_oldest_snapshot read GetInfoIntValue;
    { Next transaction id. }
    property InfoNextTransaction: Integer index isc_info_next_transaction read GetInfoIntValue;
    { Database provider. }
    property InfoDbProvider: Integer index isc_info_db_provider read GetInfoIntValue;
    { Database Class. }
    property InfoDbClass: Integer index isc_info_db_class read GetInfoIntValue;
    { User's charset specified in parameters. }
    property InfoAttCharset: Integer index frb_info_att_charset read GetInfoIntValue;
    { Firebird version. }
    property InfoFirebirdVersion: string index isc_info_firebird_version read GetInfoStringValue;
    { Return number of active transactions. }
    property InfoActiveTransactions: Integer index isc_info_active_transactions read GetInfoIntCount;
  {$ENDIF}
  {$IFDEF FB20_UP}
    property InfoActiveTransactionsCount: Integer index isc_info_active_tran_count read GetInfoIntValue;
    property InfoCreationDate: TDateTime index isc_info_creation_date read GetInfoDateTimeValue;
  {$ENDIF}
  {$IFDEF FB21_UP}
    property InfoDBFileSize: Integer index isc_info_db_file_size read GetInfoIntValue;
  {$ENDIF}
  {$IFDEF IB7_UP}
    property InfoDbReads: Integer index isc_info_db_reads read GetInfoIntValue;
    property InfoDbWrites: Integer index isc_info_db_writes read GetInfoIntValue;
    property InfoDbFetches: Integer index isc_info_db_fetches read GetInfoIntValue;
    property InfoDbMarks: Integer index isc_info_db_marks read GetInfoIntValue;
    property InfoDbGroupCommit: boolean index isc_info_db_group_commit read GetInfoBooleanValue;
  {$ENDIF}
  {$IFDEF IB71_UP}
    property InfoAttCharset: Integer index isc_info_att_charset read GetInfoIntValue;
    property InfoSvrMinVer: Integer index isc_info_svr_min_ver read GetInfoIntValue;
  {$ENDIF}
  published
    { DataBase connection parametters. }
    property Params: TStrings read FParams write SetParams;
    { Database file name. }
    property DatabaseName: TFileName read FDatabaseName write SetDatabaseName;
    { The SQL dialect gives access to DSQL features, set the dialect to 1 or 3.
      Dialect 3 gives access to features introduced in InterBase 6. }
    property SQLDialect: Integer read GetSQLDialect write SetSQLDialect default 3;
    { Character set to be utilized. }
    property CharacterSet: TCharacterSet read GetCharacterSet write SetCharacterSet;
    { Set the user name. Default = SYSDBA. }
    property UserName: string read GetUserName write SetUserName;
    { Set the Password. Default = masterkey. }
    property PassWord: string read GetPassWord write SetPassWord;
    { Define wich library the connection use.}
    property LibraryName: TFileName read FLiBraryName write SetLibraryName stored FStoreInDFM;
    { Define if the LibraryName property must be saved into the DFM }
    property StoreInDFM: Boolean read FStoreInDFM write FStoreInDFM default True;
    { This event occur after the component is connected to database. }
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    { This event occur before the component is connected to database. }
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    { This event occur after the component is disconnected from database. }
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    { This event occur before the component is disconnected from database. }
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    { When connection lost, Database, Transactions and Queries are automatically closed.
      Only one exception is raised to terminate the current stack and this event occur. }
    property OnConnectionLost: TNotifyEvent read FOnConnectionLost write FOnConnectionLost;
    { The blob segment size used to write in database, this parametter depend on hard drive. }
    property SegmentSize: Word read GetSegmentSize write SetSegmentSize default 16*1024;
    { The list of MetaData Objects returned by GetMetadata. }
    property MetaDataOptions: TMetaDataOptions read FMetaDataOptions;
    { Helpful property to shudown other database connections. }
    property Shutdown: TShutdownOptions read GetShutdown write SetShutdown default [];
    { Connect or disconnect a database. }
    property Connected: boolean read GetConnected write SetConnected default False;
    property Role: string read GetRole write SetRole;

    property OnInfoReadSeqCount: TOnInfoTableOpCount read FOnInfoReadSeqCount write FOnInfoReadSeqCount;
    property OnInfoReadIdxCount: TOnInfoTableOpCount read FOnInfoReadIdxCount write FOnInfoReadIdxCount;
    property OnInfoUpdateCount: TOnInfoTableOpCount read FOnInfoUpdateCount write FOnInfoUpdateCount;
    property OnInfoInsertCount: TOnInfoTableOpCount read FOnInfoInsertCount write FOnInfoInsertCount;
    property OnInfoDeleteCount: TOnInfoTableOpCount read FOnInfoDeleteCount write FOnInfoDeleteCount;
    property OnInfoBackoutCount: TOnInfoTableOpCount read FOnInfoBackoutCount write FOnInfoBackoutCount;
    property OnInfoPurgeCount: TOnInfoTableOpCount read FOnInfoPurgeCount write FOnInfoPurgeCount;
    property OnInfoExpungeCount: TOnInfoTableOpCount read FOnInfoExpungeCount write FOnInfoExpungeCount;
    property OnInfoActiveTransactions: TOnInfoIntegerCount read FOnInfoActiveTransactions write FOnInfoActiveTransactions;
    property OnInfoLimbo: TOnInfoIntegerCount read FOnInfoLimbo write FOnInfoLimbo;
    property OnInfoUserNames: TOnInfoStringCount read FOnInfoUserNames write FOnInfoUserNames;
  end;

  { Describe how a transaction is closed. }
  TEndTransMode = (
    etmDefault,          // Use default Transaction Action
    etmStayIn,           // keep transaction without commit or rollback
    etmCommit,           // commit transaction
    etmCommitRetaining,  // commit transaction and keep transaction handle
    etmRollback,         // rollback transaction
    etmRollbackRetaining // rollback transaction and keep transaction handle
  );

  { Indicate the Query state.
    order is important ! }
  TQueryState = (
    qsDataBase,    // have a database handle
    qsTransaction, // have a transaction handle
    qsExecImme,    // Query executed immediately without the need of statement handle
    qsStatement,   // have a statement handle
    qsPrepare,     // Query prepared
    qsExecute      // Query executed
  );

  {Oo.......................................................................oO
                                  TUIBTransaction
   Oo.......................................................................oO}

  {This evenet occur before to end the transaction, you can change the ETM parametter.}
  TOnEndTransaction = procedure(Sender: TObject; var Mode: TEndTransMode) of object;

  { The Transaction component. }
  TUIBTransaction = class(TUIBComponent)
  private
    FDataBase: TUIBDataBase;
    FDataBases: TList;
    FTrHandle: IscTrHandle;
    FSQLComponent: TList;
    FOptions   : TTransParams;
    FLockRead  : string;
    FLockWrite : string;
  {$IFDEF FB20_UP}
    FLockTimeout: Word;
  {$ENDIF}
    FOnStartTransaction: TNotifyEvent;
    FOnEndTransaction: TOnEndTransaction;
    FAutoStart: boolean;
    FAutoStop: boolean;
    FDefaultAction: TEndTransMode;
    function GetInTransaction: boolean;
    function GetOptions: TTransParams;
    procedure SetOptions(const Value: TTransParams);
    function GetLockRead: string;
    function GetLockWrite: string;
    procedure SetLockRead(const Value: string);
    procedure SetLockWrite(const Value: string);
    function GetDataBase: TUIBDataBase;
    procedure BeginDataBase;
    procedure BeginTransaction(Auto: boolean = True);
    function EndTransaction(ETM: TEndTransMode; From: TUIBStatement;
      Auto: boolean): boolean;
    procedure AddSQLComponent(Component: TUIBStatement);
    procedure RemoveSQLComponent(Component: TUIBStatement);
    procedure ClearSQLComponents;
    procedure Close(const Mode: TEndTransMode; Auto: boolean);
    function GetStatements(const Index: Integer): TUIBStatement;
    function GetStatementsCount: Integer;
    procedure ClearDataBases;
    function GetDatabases(const Index: Integer): TUIBDataBase;
    function GetDatabasesCount: Integer;
    procedure SetDefaultAction(const Value: TEndTransMode);
  protected
  {$IFNDEF UIB_NO_COMPONENT}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  {$ENDIF}
    procedure SetDataBase(const ADatabase: TUIBDataBase); virtual;
  public
    { Constructor method. }
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
    { Destructor method.}
    destructor Destroy; override;
    { Add a database to the transaction. }
    procedure AddDataBase(ADataBase: TUIBDataBase);
    { Remove a database from a transaction. }
    procedure RemoveDatabase(ADataBase: TUIBDataBase); overload;
    { Remove a database from a transaction. }
    procedure RemoveDatabase(Index: Integer); overload;
    {Start Transaction.}
    Procedure StartTransaction;
    {Commit transaction.}
    procedure Commit;
    {Commit transaction but keep transaction handle.}
    procedure CommitRetaining;
    {Rollback transaction.}
    procedure RollBack;
    {Rollback transaction but keep transaction handle.}
    procedure RollBackRetaining;
    { Execute SQL immediately without allocating statement. }
    procedure ExecuteImmediate(const sql: string);
    { Retrieve transaction ID. }
    function GetTransactionID: Cardinal;
{$IFDEF IB71_UP}
    { Interbase 7.1 spceficic, Release a savepoint.
      On Firebird 1.5 this must be call by SQL.}
    procedure SavepointRelease(const Name: string);
    { Interbase 7.1 spceficic, RollBack a savepoint.
      On Firebird 1.5 this must be call by SQL.}
    procedure SavepointRollback(const Name: string; Option: Word = 0);
    { Interbase 7.1 spceficic, Start a savepoint.
      On Firebird 1.5 this must be call by SQL.}
    procedure SavepointStart(const Name: string);
{$ENDIF}
    {Indicate if the transaction is active.}
    property InTransaction: boolean read GetInTransaction;
    { Transaction handle.}
    property TrHandle: IscTrHandle read FTrHandle;
    { Queries connected to this transaction.}
    property Statements[const Index: Integer]: TUIBStatement read GetStatements;
    { Number of Queries connected to this transaction.}
    property StatementsCount: Integer read GetStatementsCount;
    { Get all databases attached to the transaction. }
    property Databases[const Index: Integer]: TUIBDataBase read GetDatabases;
    { How many databases attached to the transaction. }
    property DatabasesCount: Integer read GetDatabasesCount;
  published
    {Database connection.}
    property DataBase  : TUIBDataBase read GetDataBase write SetDataBase;
    {Transaction parametters.}
    property Options   : TTransParams   read GetOptions    write SetOptions default [tpConcurrency,tpWait,tpWrite];
    {List of the tables to lock for read, tpLockRead option must set. ex: 'Table1;Table2'}
    property LockRead  : string         read GetLockRead   write SetLockRead;
    {List of the tables to lock for write, tpLockWrite option must set. ex: 'Table1;Table2'}
    property LockWrite : string         read GetLockWrite  write SetLockWrite;
    {This event occur after a transaction is started.}
    property OnStartTransaction: TNotifyEvent read FOnStartTransaction write FOnStartTransaction;
    {This evenet occur before to end the transaction, you can change the ETM parametter.}
    property OnEndTransaction: TOnEndTransaction read FOnEndTransaction write FOnEndTransaction;
    {If True, transaction automatically started when needed.
     if False you must explicitely call "starttransaction".}
    property AutoStart: boolean read FAutoStart write FAutoStart default True;
    {default = false, if True you need to close transaction explicitly.}
    property AutoStop: boolean read FAutoStop write FAutoStop default True;
    {Transaction default action if closed automaticaly, commit or rollback only.}
    property DefaultAction: TEndTransMode read FDefaultAction write SetDefaultAction default etmCommit;
  {$IFDEF FB20_UP}
    property LockTimeout: Word read FLockTimeout write FLockTimeout default 0;
  {$ENDIF}
  end;

{$IFDEF DELPHI14_UP}
  IUIBEnumerator<T> = interface
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  IUIBEnumerable<T> = interface
    function GetEnumerator: IUIBEnumerator<T>;
  end;

  TUIBMethodFilter<T> = reference to function(const item: T): Boolean;

  TUIBEnumerable<T> = class(TInterfacedObject, IUIBEnumerable<T>)
  private
    FQuery: TUIBStatement;
    FFilter: TUIBMethodFilter<T>;
  protected
    function GetEnumerator: IUIBEnumerator<T>;
  public
    constructor Create(AQuery: TUIBStatement; AFilter: TUIBMethodFilter<T>);
  end;

  TUIBEnumerator<T> = class(TInterfacedObject, IUIBEnumerator<T>)
  private
    FQuery: TUIBStatement;
    FFilter: TUIBMethodFilter<T>;
    FCtx: TRttiContext;
    FCurrent: T;
    FCursor: Integer;
  protected
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  public
    constructor Create(AQuery: TUIBStatement; AFilter: TUIBMethodFilter<T>);
    destructor Destroy; override;
  end;
{$ENDIF}

  { Simple query component. }
  TUIBStatement = class(TUIBComponent)
  private
    FCurrentState: TQueryState;
    FTransaction: TUIBTransaction;
    FDataBase: TUIBDataBase;
    FStHandle: IscStmtHandle;
    FOnError: TEndTransMode;
    FCursorName: string;
    FSQLResult: TSQLResult;
    FCachedFetch: boolean;
    FFetchBlobs: boolean;
    FBufferChunks: Cardinal;
    FQuickScript: boolean;
    FSQL: TStrings;
    FParsedSQL: string;
    FParameter: TSQLParams;
    FParseParams: boolean;
    FOnClose: TNotifyEvent;
    FStatementType: TUIBStatementType;
    FUseCursor: boolean;
    function GetPlan: string;
    function GetStatementType: TUIBStatementType;
    procedure SetSQL(const Value: TStrings);
    procedure DoSQLChange(Sender: TObject);
    function GetFields: TSQLResult;
    function GetEof: boolean;
    function FindDataBase: TUIBDataBase;
    function GetRowsAffected: Cardinal;
    function GetBof: boolean;
  protected
    procedure SetTransaction(const Transaction: TUIBTransaction); virtual;
    procedure SetDataBase(ADataBase: TUIBDataBase);
  {$IFNDEF UIB_NO_COMPONENT}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  {$ENDIF}
    procedure NeedTransaction; virtual;

    procedure BeginTransaction; virtual;
    procedure BeginStatement; virtual;
    procedure BeginPrepare(describeParams: boolean = false); virtual;
    procedure BeginExecute; virtual;
    procedure BeginExecImme; virtual;

    procedure EndTransaction(const ETM: TEndTransMode; Auto: boolean); virtual;
    procedure EndStatement(const ETM: TEndTransMode; Auto, Drop: boolean); virtual;
    procedure EndPrepare(const ETM: TEndTransMode; Auto, Drop: boolean); virtual;
    procedure EndExecute(const ETM: TEndTransMode; Auto, Drop: boolean); virtual;
    procedure EndExecImme(const ETM: TEndTransMode; Auto, Drop: boolean); virtual;

    procedure InternalNext; virtual;
    procedure InternalPrior; virtual;
    procedure InternalTryCache(const Mode: TEndTransMode; Auto: boolean); virtual;
    procedure InternalClose(const Mode: TEndTransMode; Auto, Drop: boolean); virtual;

    function  ParamsClass: TSQLParamsClass; virtual;
    function  ResultClass: TSQLResultClass; virtual;

    procedure InternalGetBlobSize(sqlda: TSQLDA; const Index: Word; out Size: Cardinal);
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; Stream: TStream); overload;
    procedure InternalReadBlobB(sqlda: TSQLDA; const Index: Word; var str: RawByteString);
    procedure InternalReadBlobA(sqlda: TSQLDA; const Index: Word; var str: AnsiString); overload;
    procedure InternalReadBlobW(sqlda: TSQLDA; const Index: Word; var str: UnicodeString); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var str: string); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var Value: Variant); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; Buffer: Pointer); overload;

    property QuickScript: boolean read FQuickScript write FQuickScript  default False;

  public
    { Constructor method. }
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
    { Destructor method. }
    destructor Destroy; override;
    { Close the statement. You can commit or rollback the transaction when closing. }
    procedure Close(const Mode: TEndTransMode = etmStayIn); virtual;
    { Fetch all records returned by the query. }
    procedure CloseCursor;
    procedure FetchAll;
    { Open the query and fetch the first record if FetchFirst = true. }
    procedure Open(FetchFirst: boolean = True);
    { Prepare the query. }
    procedure Prepare(describeParams: boolean = false);
    { Execute the query. }
    procedure Execute;
    { Execute the query or the script (QuickScript = true) immediately. }
    procedure ExecSQL;
    { Get the next record. }
    procedure Next;
    { Get the prior record. }
    procedure Prior;
    { Get the last record. }
    procedure Last;
    { Get the first record. }
    procedure First;
    { Read a the blob in a stream by index. }
    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    { Read a the blob in a RawByteString by index. }
    procedure ReadBlobB(const Index: Word; var str: RawByteString); overload;
    { Read a the blob in a AnsiString by index. }
    procedure ReadBlobA(const Index: Word; var str: AnsiString); overload;
    { Read a the blob in a UnicodeString by index. }
    procedure ReadBlobW(const Index: Word; var str: UnicodeString); overload;
    { Read a the blob in a String by index. }
    procedure ReadBlob(const Index: Word; var str: string); overload;
    { Read a the blob in a String by index. }
    function ReadBlob(const Index: Word): string; overload;
    { Read a the blob in a Variant by index. }
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    { Read a the blob in a PREALLOCATED buffer by index. }
    procedure ReadBlob(const Index: Word; Buffer: Pointer); overload;
    { Read a the blob in a stream by name. }
    procedure ReadBlob(const name: string; Stream: TStream); overload;
    { Read a the blob in a RawByteString by name. }
    procedure ReadBlobB(const name: string; var str: RawByteString); overload;
    { Read a the blob in a AnsiString by name. }
    procedure ReadBlobA(const name: string; var str: AnsiString); overload;
    { Read a the blob in a UnicodeString by name. }
    procedure ReadBlobW(const name: string; var str: UnicodeString); overload;
    { Read a the blob in a string by name. }
    procedure ReadBlob(const name: string; var str: string); overload;
    { Read a the blob in a string by name. }
    function ReadBlob(const name: string): string; overload;
    { Read a the blob in a Variant by name. }
    procedure ReadBlob(const name: string; var Value: Variant); overload;
    { Read a the blob in a PREALLOCATED buffer by name. }
    procedure ReadBlob(const name: string; Buffer: Pointer); overload;

    { The the blob value of a parametter using a Stream. }
    procedure ParamsSetBlob(const Index: Word; Stream: TStream); overload;
    { The the blob value of a parametter using a RawByteString. }
    procedure ParamsSetBlobB(const Index: Word; const str: RawByteString); overload;
    { The the blob value of a parametter using a AnsiString. }
    procedure ParamsSetBlobA(const Index: Word; const str: AnsiString); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlobW(const Index: Word; const str: UnicodeString); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlob(const Index: Word; const str: string); overload;
    { The the blob value of a parametter using a Buffer. }
    procedure ParamsSetBlob(const Index: Word; Buffer: Pointer; Size: Cardinal); overload;

    { The the blob value of a parametter using a Stream. }
    procedure ParamsSetBlob(const Name: string; Stream: TStream); overload;
    { The the blob value of a parametter using a rawbytestring. }
    procedure ParamsSetBlobB(const Name: string; const str: RawByteString); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlobA(const Name: string; const str: AnsiString); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlobW(const Name: string; const str: UnicodeString); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlob(const Name: string; const str: string); overload;
    { The the blob value of a parametter using a Buffer. }
    procedure ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Cardinal); overload;

    { Get the the blob size of the current record. }
    function FieldBlobSize(const Index: Word): Cardinal;
    { Get the blob size of the corresonding parametter. }
    function ParamBlobSize(const Index: Word): Cardinal;

{$IFDEF DELPHI14_UP}
    function All<T>: IUIBEnumerable<T>;
    function Select<T>(const AFilter: TUIBMethodFilter<T>): IUIBEnumerable<T>;
{$ENDIF}

    { Return the rows affected by this statement }
    procedure AffectedRows(out SelectedRows, InsertedRows, UpdatedRows, DeletedRows: Cardinal);

    { The internal statement handle. }
    property StHandle: IscStmtHandle read FStHandle;
    { Use fields to read the current record. }
    property Fields: TSQLResult read GetFields;
    { use Params to set parametters, the param names are set dynamically
      parsing the SQL query, by default the param values are null string.
      The first time you set a parametter value, the field type is defined.  }
    property Params: TSQLParams read FParameter;
    { All UIB statements declare a unique cursor name, another query can use
      this cursor to modify the current cursor, this feature is for unidirectionnal
      statements !!.<br>
      ex: UPDATE proj_dept_budget SET projected_budget = :value WHERE CURRENT OF %s; }
    property CursorName: string read FCursorName;
    { Indicate the current state of the query. }
    property CurrentState: TQueryState read FCurrentState;
    { if true there isn't anymore record to fetch. }
    property Eof: boolean read GetEof;
    property Bof: boolean read GetBof;
    { @exclude }
    property ParseParams: boolean read FParseParams write FParseParams;
    { The plan used internally by interbase (the query must be prepared). }
    property Plan: string read GetPlan;
    { Get the current statement type (the query must be prepared). }
    property StatementType: TUIBStatementType read GetStatementType;
    { Return the number of rows affected by the query (stInsert, stUpdate or stDelete). }
    property RowsAffected: Cardinal read GetRowsAffected;
    property UseCursor: boolean read FUseCursor write FUseCursor default True;
  published
    { The sql query. }
    property SQL: TStrings read FSQL write SetSQL;
    { Transaction of the query. }
    property Transaction: TUIBTransaction read FTransaction write SetTransaction;
    { Connected database, in most cases you don't need to set this property, it is
      only needed if the transaction concern more than one database. }
    property DataBase: TUIBDataBase read FDataBase write SetDataBase;
    { If an error occur, this action is applied to the connected transaction. }
    property OnError: TEndTransMode read FOnError write FOnError default etmRollback;
    { If true all record are saved in memory. }
    property CachedFetch: boolean read FCachedFetch write FCachedFetch default True;
    { If true the blob data is fetched with the record. }
    property FetchBlobs: boolean read FFetchBlobs write FFetchBlobs default False;
    { Use BufferChunks to get or set the number of records for which the query
      allocates buffer space at any time. When the query’s buffer is full,
      trying to fetch an additional record causes the dataset to reallocate
      the buffer so that it has enough memory to hold an additional BufferChunks
      records. <br>
      Note: When CachedFetch is False, BufferChunks has no meaning. }
    property BufferChunks: Cardinal read FBufferChunks write FBufferChunks default 1000;
    { OnClose event. }
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  {Oo.......................................................................oO
                                  TUIBQuery
   Oo.......................................................................oO}
  { The query component. }
  TUIBQuery = class(TUIBStatement)
  public
    { Helper method to buid the SQL query needed to execute the stored procedure.
      Input data type found using this method. }
    procedure BuildStoredProc(const StoredProc: string; forSelect: boolean = true);
  published
    { If true you can use this component as a fast script component where each line is a query.
      You must use the ExecSQL method ! }
    property QuickScript;
  end;

  { Parsing event, occur on each query executed. }
  TOnParse = procedure(Sender: TObject; NodeType: TSQLStatement;
    const Statement: string) of object;

  { Executing error event, occur if executed query is failed. }
  TOnExecuteError = procedure(Sender: TObject; Error: Exception; SQLText:
    string; var Handled: Boolean) of object;

  { The script component. }
  TUIBScript = class(TUIBComponent)
  private
    FQuery: TUIBQuery;
    FScript: TStrings;
    FAutoDDL: boolean;
    FOnParse: TOnParse;
    FOnComment: TOnComment;
    FOnExecuteError: TOnExecuteError;
    procedure SetTransaction(const Value: TUIBTransaction);
    function GetTransaction: TUIBTransaction;
    procedure SetScript(const Value: TStrings);
    procedure SetOnError(const Value: TEndTransMode);
    function GetOnError: TEndTransMode;
    function GetDatabase: TUIBDataBase;
    procedure SetDatabase(const Value: TUIBDataBase);
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
    destructor Destroy; override;
    procedure ExecuteScript;
  published
    property Transaction: TUIBTransaction read GetTransaction write SetTransaction;
    property Database: TUIBDataBase read GetDatabase write SetDatabase;
    property Script: TStrings read FScript write SetScript;
    property AutoDDL: boolean read FAutoDDL write FAutoDDL default True;
    property OnParse: TOnParse read FOnParse write FOnParse;
    property OnComment: TOnComment read FOnComment write FOnComment;
    property OnError: TEndTransMode read GetOnError write SetOnError default etmRollback;
    property OnExecuteError: TOnExecuteError read FOnExecuteError write FOnExecuteError;

  end;

  TUIBProtocol = (
    proLocalHost,
    proTCPIP,
    proNetBEUI
  );

  TUIBService = class(TUIBComponent)
  private
    FLiBraryName: string;
    FUserName: string;
    FPassWord: string;
    FHost    : string;
    FProtocol: TUIBProtocol;
    procedure SetLibraryName(const Lib: String);
  protected
    FLibrary: TUIBLibrary;
    FHandle  : IscSvcHandle;
    procedure BeginService; virtual;
    procedure EndService; virtual;
    function CreateParam(code: AnsiChar; const Value: RawByteString): RawByteString; overload;
    function CreateParam(code: AnsiChar; Value: Integer): RawByteString; overload;
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
    destructor Destroy; override;
  published
    property UserName: string read FUserName write FUserName;
    property PassWord: string read FPassWord write FPassWord;
    property Host: string read FHost write FHost;
    property Protocol: TUIBProtocol read FProtocol write FProtocol default proLocalHost;
    { Define wich library the connection use.}
    property LibraryName: string read FLiBraryName write SetLibraryName;
  end;

  TVerboseEvent = procedure(Sender: TObject; Message: string) of object;

  TUIBBackupRestore = class(TUIBService)
  private
    FBackupFiles: TStrings;
    FDatabase: TFileName;
    FOnVerbose: TVerboseEvent;
    FVerbose: boolean;
    procedure SetBackupFiles(const Value: TStrings);
    function CreateStartSPB: RawByteString; virtual; abstract;
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
    destructor Destroy; override;
    procedure Run;
  published
    property BackupFiles: TStrings read FBackupFiles write SetBackupFiles;
    property Database: TFileName read FDatabase write FDatabase;
    property OnVerbose: TVerboseEvent read FOnVerbose write FOnVerbose;
    property Verbose: boolean read FVerbose write FVerbose default false;
  end;

  TBackupOption = (boIgnoreChecksums, boIgnoreLimbo, boMetadataOnly,
    boNoGarbageCollection, boOldMetadataDesc, boNonTransportable,
    boConvertExtTables, boExpand);
  TBackupOptions = set of TBackupOption;

  TUIBBackup = class(TUIBBackupRestore)
  private
    FOptions: TBackupOptions;
    function CreateStartSPB: RawByteString; override;
  published
    property Options: TBackupOptions read FOptions write FOptions default [];
  end;

  TRestoreOption = (roDeactivateIndexes, roNoShadow, roNoValidityCheck,
    roOneRelationAtATime, roReplace, roCreateNewDB, roUseAllSpace
    {$IFDEF IB71_UP},roValidate{$ENDIF});

  TRestoreOptions = set of TRestoreOption;

  TUIBRestore = class(TUIBBackupRestore)
  private
    FOptions: TRestoreOptions;
    FPageSize: Cardinal;
    function CreateStartSPB: RawByteString; override;
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
  published
    property Options: TRestoreOptions read FOptions write FOptions default [roCreateNewDB];
    property PageSize: Cardinal read FPageSize write FPageSize default 0;
  end;

  TSecurityAction = (saAddUser, saDeleteUser, saModifyUser, saDisplayUser, saDisplayUsers);
  TSecurityParam = (spRole, spUser, spPass, spFirstName, spMiddleName, spLastName, spUserID, spGroupID{$IFDEF FB25_UP}, spAdmin{$ENDIF});
  TSecurityParams = set of TSecurityParam;

  TUserInfo = class(TObject)
  public
    UserName: string;
    FirstName: string;
    MiddleName: string;
    LastName: string;
    GroupID: Integer;
    UserID: Integer;
  end;

  TUIBSecurity = class(TUIBService)
  private
    FIntegerParams: array[ord(spUserID)..ord(high(TSecurityParam))] of Integer;
    FStringParams: array[ord(spRole)..ord(spLastName)] of string;
    FModifiedParams: TSecurityParams;
    FUserInfos: TObjectList;
    procedure ClearParams;
{$IFDEF FB25_UP}
    function GetBooleanParam(aParam: Integer): Boolean;
{$ENDIF}
    function GetIntegerParam(aParam: Integer): Integer;
    function GetStringParam(aParam: Integer): string;
    function GetUserInfo(aIndex: Integer): TUserInfo;
    function GetUserInfoCount: Integer;
{$IFDEF FB25_UP}
    procedure SetBooleanParam(aParam: Integer; const aValue: Boolean);
{$ENDIF}
    procedure SetIntegerParam(aParam: Integer; const aValue: Integer);
    procedure SetStringParam(aParam: Integer; const aValue: string);
    procedure RunAction(aAction: TSecurityAction);
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
    destructor Destroy; override;
    { tell the server to add the user specified to the security database }
    procedure AddUser;
    { tell the sever to remove the user specified from the security database }
    procedure DeleteUser;
    { tell the server to make modifications to the record in the security database specified by the user }
    procedure ModifyUser;
    { retrieves user information for the specified user }
    procedure DisplayUser;
    { retrieves user information for all users }
    procedure DisplayUsers;
    { information for user(s) retrieved by DisplayUser(s) }
    property UserInfo[Index: Integer] : TUserInfo read GetUserInfo;
    { number of user(s) retrieved by DisplayUser(s) }
    property UserInfoCount: Integer read GetUserInfoCount;
  published
    { SQL role name to use when connecting to the security database }
    property Role: string index ord(spRole) read GetStringParam write SetStringParam;
    { The username to add, modify or remove from the security database (max 31 chars) }
    property User: string index ord(spUser) read GetStringParam write SetStringParam;
    { Password for the user being added/modified (max 32 only first 8 used) }
    property Pass: string index ord(spPass) read GetStringParam write SetStringParam;
    { The first name of the user being added/modified }
    property FirstName: string index ord(spFirstName) read GetStringParam write SetStringParam;
    { The middle name of the user being added/modified }
    property MiddleName: string index ord(spMiddleName) read GetStringParam write SetStringParam;
    { The last name of the user being added/modified }
    property LastName: string index ord(spLastName) read GetStringParam write SetStringParam;
    { An integer that specifies a user ID of the user being added/modified }
    property UserID: Integer index ord(spUserID) read GetIntegerParam write SetIntegerParam;
    { An integer that specifies a group ID of the user being added/modified }
    property GroupID: Integer index ord(spGroupID) read GetIntegerParam write SetIntegerParam;
{$IFDEF FB25_UP}
    { A Boolean that specifies if user is admin }
    property Admin: Boolean index ord(spAdmin) read GetBooleanParam write SetBooleanParam;
{$ENDIF}
  end;

  TRepairOption = (roValidateDB, roValidateFull, roSweepDB, roMendDB,
    roListLimboTrans, roCheckDB, roIgnoreChecksum, roKillShadows);
  TRepairOptions = set of TRepairOption;

  TUIBRepair = class(TUIBService)
  private
    FOptions: TRepairOptions;
    FDatabase: string;
    FOnVerbose: TVerboseEvent;
    FVerbose: boolean;
  protected
    function CreateStartSPB: RawByteString; virtual;
  public
    procedure Run;
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
  published
    property Options: TRepairOptions read FOptions write FOptions;
    property Database: string read FDatabase write FDatabase;
    property OnVerbose: TVerboseEvent read FOnVerbose write FOnVerbose;
    property Verbose: boolean read FVerbose write FVerbose default false;
  end;

  TOnEvent = procedure(Sender: TObject; const EventName: string; Count: Integer;
    var Cancel: boolean) of object;
  TOnExceptionEvent = procedure(Error: Exception) of object;

  TUIBEventThread = class;

  TUIBEvents = class(TUIBComponent)
  private
    FOnEvent: TOnEvent;
    FOnException: TOnExceptionEvent;
    FThreads: TList;
    FDatabase: TUIBDataBase;
    FEvents: TStrings;
    FAutoRegister: boolean;
    FThreadException : boolean;
    FRegistered : boolean;
    FSyncMainThread: boolean;
    procedure SetDatabase(value: TUIBDataBase);
    procedure SetEvents(Value: TStrings);
    procedure SetRegistered(const Value: boolean);
    procedure SetAutoRegister(const Value: boolean);
  protected
  {$IFNDEF UIB_NO_COMPONENT}
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  {$ENDIF}
  public
    constructor Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent); override{$ENDIF};
    destructor Destroy; override;
  published
    property AutoRegister: boolean read FAutoRegister write SetAutoRegister;
    property Database: TUIBDataBase read FDatabase write SetDatabase;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: boolean read FRegistered write SetRegistered;
    property SyncMainThread: boolean read FSyncMainThread write FSyncMainThread default true;
    property OnEvent: TOnEvent read FOnEvent write FOnEvent;
    property OnException: TOnExceptionEvent read FOnException write FOnException;
  end;

  TUIBEventThread = class(TThread)
  private
    FCurrentEvent: Integer;
    FEventID: Integer;
    FEventBuffer: PAnsiChar;
    FEventBufferLen: Smallint;
    FResultBuffer: PAnsiChar;
    FSignal: TSimpleEvent;
    FQueueEvent: boolean;
    FBlock: integer;
    FOwner: TUIBEvents;
    FExceptObject: TObject;
    FExceptAddr: Pointer;
    FCancelAlerts : boolean;
    FStatusVector: TStatusVector;
    FSyncMainThread: boolean;
    function HandleException: boolean;
    function FindDataBase: TUIBDataBase;
    procedure SyncEventQueue;
    procedure SyncEventCancel;
    procedure SyncOnEvent;
    procedure SyncHandleException;
    procedure SyncTerminate(sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TUIBEvents; Block: Integer;
      SyncMainThread: boolean); virtual;
    destructor Destroy; override;
  end;

  TShutdownMode = (smForced, smDenyTransaction, smDenyAttachment);

  TUIBConfig = class(TUIBService)
  private
    FDatabaseName: TFileName;
  public
    procedure ShutdownDatabase(Options: TShutdownMode; Wait: Integer);
    procedure SetSweepInterval(Value: Integer);
    procedure SetDBSqlDialect(Value: Integer);
    procedure SetPageBuffers(Value: Integer);
    procedure ActivateShadow;
    procedure BringDatabaseOnline;
    procedure SetReserveSpace(Value: Boolean);
    procedure SetAsyncMode(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
  published
    property DatabaseName: TFileName read FDatabaseName write FDatabaseName;
  end;

  TUIBServerInfo = class(TUIBService)
  private
    FOnInfoAttachments: TOnInfoIntegerCount;
    FOnInfoDatabases: TOnInfoIntegerCount;
    FOnInfoDbName: TOnInfoStringCount;
  public
    procedure GetServerInfo;
  published
    property OnInfoAttachments: TOnInfoIntegerCount read FOnInfoAttachments write FOnInfoAttachments;
    property OnInfoDatabases: TOnInfoIntegerCount read FOnInfoDatabases write FOnInfoDatabases;
    property OnInfoDbName: TOnInfoStringCount read FOnInfoDbName write FOnInfoDbName;
  end;

implementation
uses
{$IFDEF UNICODE}
  AnsiStrings,
{$ENDIF}
  uibmetadata;

type
  PExceptionInfo = ^TExceptionInfo;
  TExceptionInfo = record
    ExepClass: EUIBExceptionClass;
    ID: Integer;
  end;

{$IFDEF UNIX}
const
  INFINITE = $FFFFFFFF;
{$ENDIF}

{ TUIBDataBase }

procedure TUIBDataBase.AddTransaction(Transaction: TUIBTransaction);
begin
  if (FTransactions = nil) then
    FTransactions := TList.Create;
  FTransactions.Add(Transaction);
end;

procedure TUIBDataBase.ClearTransactions;
begin
  while (FTransactions <> nil) do
    TUIBTransaction(FTransactions.Last).RemoveDatabase(Self);
end;

procedure TUIBDataBase.CloseStatements;
var
  i: Integer;
begin
  if (FStatements <> nil) then
    for i := 0 to FStatements.Count - 1 do
      TUIBStatement(FStatements.Items[i]).InternalClose(etmStayIn, true, true);
end;

procedure TUIBDataBase.CloseTransactions;
var
  i: Integer;
begin
  if (FTransactions <> nil) then
    for i := 0 to FTransactions.Count - 1 do
      TUIBTransaction(FTransactions.Items[i]).Close(etmDefault, True);
end;

constructor TUIBDataBase.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FStatements := nil;
  FTransactions := nil;
  FLibrary := TUIBLibrary.Create;
  FLibraryName := GetClientLibrary;
  FLibrary.OnConnectionLost := DoOnConnectionLost;
  FLibrary.OnGetDBExceptionClass := DoOnGetDBExceptionClass;
  FDbHandle := nil;
  FHandleShared := False;
  FParams := TStringList.Create;
  TStringList(FParams).OnChange := doOnParamChange;
  SQLDialect := 3;
  CharacterSet := GetSystemCharacterset;
  FExceptions := TList.Create;
  FEventNotifiers := TList.Create;
  FMetadata := nil;
  FMetaDataOptions := TMetaDataOptions.Create;
  FStoreInDFM := True;
end;

destructor TUIBDataBase.Destroy;
begin
  Connected := False;
  ClearStatements;
  ClearTransactions;
  ClearEvents;
  TStringList(FParams).Free;
  ClearExceptions;
  FExceptions.Free;
  FEventNotifiers.Free;
  FLibrary.Free;
  FMetaDataOptions.Free;
  inherited;
end;

procedure TUIBDataBase.DoOnConnectionLost(Lib: TUIBLibrary);
begin
  Lib.RaiseErrors := False;
  try
    Connected := False;
  finally
    Lib.RaiseErrors := True;
    if Assigned(FOnConnectionLost) then
      FOnConnectionLost(Self);
  end;
end;

function TUIBDataBase.GetCharacterSet: TCharacterSet;
begin
  Result := FCharacterSet;
end;

function TUIBDataBase.GetConnected: boolean;
begin
  result := FDbHandle <> nil;
end;

function TUIBDataBase.GetPassWord: string;
begin
  result := ReadParamString('password');
end;

function TUIBDataBase.GetSQLDialect: Integer;
begin
  Result := FSQLDialect;
end;

procedure TUIBDataBase.ExecuteImmediate(const Statement: string);
begin
  FLibrary.Load(FLiBraryName);
{$IFDEF UNICODE}
  FLibrary.DSQLExecuteImmediate(MBUEncode(Statement, CharacterSetCP[CharacterSet]), SQLDialect);
{$ELSE}
  FLibrary.DSQLExecuteImmediate(Statement, SQLDialect);
{$ENDIF}
end;

procedure TUIBDataBase.CreateDatabase(DefaultCharacterSet: TCharacterSet; PageSize: Integer);
var TrHandle: IscTrHandle;
const
  CreateDb = 'CREATE DATABASE ''%s'' USER ''%s'' PASSWORD ''%s'' ' +
    'SET NAMES ''%s'' PAGE_SIZE %d DEFAULT CHARACTER SET %s';
begin
  TrHandle := nil;
  Connected := False;
  FLibrary.Load(FLiBraryName);
{$IFDEF UNICODE}
  FLibrary.DSQLExecuteImmediate(FDbHandle, TrHandle,
    AnsiString(Format(CreateDb, [DatabaseName, UserName, PassWord,
                     CharacterSetStr[CharacterSet],
                     PageSize, CharacterSetStr[DefaultCharacterSet]])),
    SQLDialect);
{$ELSE}
  FLibrary.DSQLExecuteImmediate(FDbHandle, TrHandle,
    Format(CreateDb, [DatabaseName, UserName, PassWord,
           CharacterSetStr[CharacterSet],
           PageSize, CharacterSetStr[DefaultCharacterSet]]),
    SQLDialect);
{$ENDIF}
end;

procedure TUIBDataBase.DropDatabase;
begin
  Connected := True;
  FLibrary.Load(FLiBraryName);
  FLibrary.DatabaseDrop(FDbHandle);
  ClearTransactions;
  ClearEvents;
  ClearExceptions;
  FDbHandle := nil;
end;

function TUIBDataBase.GetUserName: string;
begin
  result := ReadParamString('user_name');
end;

function TUIBDataBase.ReadParamInteger(Param: String;
  Default: Integer): Integer;
begin
  Result := StrToInt(ReadParamString(Param, IntToStr(Default)));
end;

function TUIBDataBase.ReadParamString(Param, Default: String): String;
var
  I: Integer;
begin
  I := FParams.IndexOfName(Param);
  if I >= 0 then
  begin
    Result := Copy(FParams[I], Length(Param) + 2, Maxint);
    Exit;
  end;
  Result := Default;
end;

procedure TUIBDataBase.RemoveTransaction(Transaction: TUIBTransaction);
begin
  if (FTransactions <> nil) then
  begin
    FTransactions.Remove(Transaction);
    if FTransactions.Count = 0 then
    begin
      FTransactions.free;
      FTransactions := nil;
    end;
  end;
end;

procedure TUIBDataBase.SetCharacterSet(const Value: TCharacterSet);
begin
  WriteParamString('lc_ctype', string(CharacterSetStr[Value]));
end;

procedure TUIBDataBase.SetConnected(const Value: boolean);
begin
  if (Value = Connected) then Exit;
  with FLibrary do
  case Value of
    True  :
      begin
        if not CanConnect then
          Exit;
        if Assigned(BeforeConnect) then BeforeConnect(Self);
        FLibrary.Load(FLiBraryName);
        if not FHandleShared then
        begin
{$IFDEF FB25_UP}
{$IFDEF UNICODE}
          if FParams.IndexOf('utf8_filename') >= 0 then
            AttachDatabase(MBUEncode(FDatabaseName, CP_UTF8), FDbHandle, AnsiString(FParams.Text), BreakLine) else
{$ENDIF}
{$ENDIF}
            AttachDatabase(AnsiString(FDatabaseName), FDbHandle, AnsiString(FParams.Text), BreakLine);
        end;
        RegisterEvents;
        if Assigned(AfterConnect) then AfterConnect(Self);
      end;
    False :
      begin
        if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
        CloseStatements;
        CloseTransactions;
        UnRegisterEvents;
        if FMetadata <> nil then
          FreeAndNil(FMetadata);
        if FHandleShared then
        begin
          FDbHandle := nil;
          FHandleShared := False;
        end else
          DetachDatabase(FDbHandle);
        if Assigned(AfterDisconnect) then AfterDisconnect(Self);
      end;
  end;
end;

procedure TUIBDataBase.SetDatabaseName(const Value: TFileName);
begin
  FDatabaseName := Value;
{$IFNDEF UIB_NO_COMPONENT}
  if (csDesigning in ComponentState) then
    Connected := False;
{$ENDIF}
end;

procedure TUIBDataBase.SetDbHandle(const Value: IscDbHandle);
begin
  if (FDbHandle = nil) or ((FDbHandle <> nil) and FHandleShared) then
  begin
    FLibrary.Load(FLiBraryName);
    FDbHandle := Value;
    FHandleShared := (FDbHandle <> nil);
  end else
    raise Exception.Create(EUIB_DBHANDLEALREADYSET);
end;

procedure TUIBDataBase.SetLibraryName(const Lib: TFileName);
begin
  SetConnected(False);
  FLibrary.UnLoad;
  FLiBraryName := Lib;
end;

function TUIBDataBase.GetTransactions(const Index: Cardinal): TUIBTransaction;
begin
  if FTransactions <> nil then
    Result := FTransactions.Items[Index] else
    raise EListError.CreateFmt(EUIB_INDEXERROR,[Index]);
end;

function TUIBDataBase.GetTransactionsCount: Cardinal;
begin
  if FTransactions <> nil then
    Result := FTransactions.Count else
    Result := 0;
end;

procedure TUIBDataBase.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TUIBDataBase.SetPassWord(const Value: string);
begin
  WriteParamString('password', Value);
end;

procedure TUIBDataBase.SetSQLDialect(const Value: Integer);
begin
  WriteParamInteger('sql_dialect', Value);
end;

procedure TUIBDataBase.SetUserName(const Value: string);
begin
  WriteParamString('user_name', Value);
end;

procedure TUIBDataBase.WriteParamInteger(Param: String; Value: Integer);
begin
  WriteParamString(Param, IntToStr(Value));
end;

procedure TUIBDataBase.WriteParamString(Param, Value: String);
var
  I: Integer;
  S: string;
begin
  S := Param + '=' + Value;
  I := FParams.IndexOfName(Param);
  if I >= 0 then
    FParams[I] := S
  else
    FParams.Add(S);
end;

procedure TUIBDataBase.ClearExceptions;
var i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    FreeMem(FExceptions[i]);
  FExceptions.Clear;
end;

procedure TUIBDataBase.ClearStatements;
begin
  while (FStatements <> nil) do
    TUIBStatement(FStatements.Last).DataBase := nil;
end;

procedure TUIBDataBase.RegisterException(Excpt: EUIBExceptionClass;
  ID: Integer);
var
  ExcepInfo: PExceptionInfo;
  i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    if PExceptionInfo(FExceptions[i]).ID = ID then
      raise Exception.CreateFmt(EUIB_EXPTIONREGISTERED, [ID]);
  GetMem(ExcepInfo, SizeOf(TExceptionInfo));
  ExcepInfo.ExepClass := Excpt;
  ExcepInfo.ID := ID;
  FExceptions.Add(ExcepInfo);
end;

function TUIBDataBase.RegisterException(Excpt: EUIBExceptionClass;
  const Name: string): Integer;
var
  Transaction: TUIBTransaction;
  Query: TUIBQuery;
begin
  Result := -1;
  Transaction := TUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  Query := TUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    Transaction.DataBase := Self;
    Query.Transaction := Transaction;
    Query.CachedFetch := False;
    Query.SQL.Text := 'SELECT RDB$EXCEPTION_NUMBER FROM RDB$EXCEPTIONS WHERE RDB$EXCEPTION_NAME = ?';
    Query.Params.AsString[0] := UpperCase(Name);
    Query.Open;
    if not Query.Eof then
    begin
      Result := Query.Fields.AsInteger[0];
      RegisterException(Excpt, Result);
    end;
    Query.Close(etmCommit);
    if (Result = - 1) then
      raise Exception.CreateFmt(EUIB_EXCEPTIONNOTFOUND, [Name]);
  finally
    Query.Free;
    Transaction.Free;
  end;
end;

procedure TUIBDataBase.UnRegisterException(Number: Integer);
var i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    if PExceptionInfo(FExceptions[i]).ID = Number then
    begin
      FreeMem(FExceptions[i]);
      FExceptions.Delete(i);
      Break;
    end;
end;

procedure TUIBDataBase.UnRegisterExceptions(Excpt: EUIBExceptionClass);
var i: Integer;
begin
  i := 0;
  while i < FExceptions.Count do
  begin
    if (PExceptionInfo(FExceptions[i]).ExepClass = Excpt) then
    begin
      FreeMem(FExceptions[i]);
      FExceptions.Delete(i);
    end else
    inc(i);
  end;
end;

procedure TUIBDataBase.DoOnGetDBExceptionClass(Number: Integer; out Excep: EUIBExceptionClass);
var i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    if (PExceptionInfo(FExceptions[i]).ID = Number) then
    begin
      Excep := PExceptionInfo(FExceptions[i]).ExepClass;
      Exit;
    end;
  Excep := EUIBException;
end;

procedure TUIBDataBase.doOnParamChange(Sender: TObject);
begin
  FCharacterSet := StrToCharacterSet(RawbyteString(ReadParamString('lc_ctype', 'NONE')));
  FSQLDialect := ReadParamInteger('sql_dialect', 3);
end;

function TUIBDataBase.GetMetadata(Refresh: boolean = False): TObject;
var
  Transaction: TUIBTransaction;
begin
  if Refresh and (FMetadata <> nil) then
    FreeAndNil(FMetadata);
  if (FMetadata = nil) then
  begin
    Transaction := TUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
    try
      Transaction.Database := Self;
      FMetadata := TMetaDataBase.Create(nil, -1);
      with TMetaDataBase(FMetadata) do
      begin
        OIDDatabases := FMetaDataOptions.Objects;
        OIDTables := FMetaDataOptions.Tables;
        OIDViews := FMetaDataOptions.Views;
        OIDProcedures := FMetaDataOptions.Procedures;
        OIDUDFs := FMetaDataOptions.UDFs;
        OIDRoles := FMetaDataOptions.Roles;
        SysInfos := FMetaDataOptions.FSysInfos
      end;
      try
        TMetaDataBase(FMetadata).LoadFromDatabase(Transaction);
        Transaction.Commit;
      except
        FreeAndNil(FMetadata);
        raise;
      end;
    finally
      Transaction.Free;
    end;
  end;
  Result := FMetadata;
end;

function TUIBDataBase.GetSegmentSize: Word;
begin
  Result := FLibrary.SegMentSize;
end;

procedure TUIBDataBase.SetSegmentSize(const Value: Word);
begin
  FLibrary.SegMentSize := Value;
end;

function TUIBDataBase.GetShutdown: TShutdownOptions;
begin
  try
    case sizeof(TShutdownOptions) of
      1: PByte(@result)^ := ReadParamInteger('shutdown', 0);
      2: PWord(@result)^ := ReadParamInteger('shutdown', 0);
      4: PInteger(@result)^ := ReadParamInteger('shutdown', 0);
    end;
  except
    WriteParamInteger('shutdown', 0);
  end;
end;

procedure TUIBDataBase.SetShutdown(const Value: TShutdownOptions);
begin
  case sizeof(TShutdownOptions) of
    1: WriteParamInteger('shutdown', PByte(@Value)^);
    2: WriteParamInteger('shutdown', PWord(@Value)^);
    4: WriteParamInteger('shutdown', PInteger(@Value)^);
  end;
end;

function TUIBDataBase.GetInfoIntValue(const item: Integer): integer;
begin
  SetConnected(true);
  case item of
    isc_info_implementation,
    isc_info_base_level:
    result := byte(FLibrary.DatabaseInfoString(FDbHandle, item, 8)[5]);
  else
    result := FLibrary.DatabaseInfoIntValue(FDbHandle, AnsiChar(item));
  end;
end;

{$IFDEF FB20_UP}
function TUIBDataBase.GetInfoDateTimeValue(const item: Integer): TDateTime;
begin
  SetConnected(true);
  result := FLibrary.DatabaseInfoDateTime(FDbHandle, item);
end;
{$ENDIF}

function TUIBDataBase.GetInfoBooleanValue(const item: Integer): boolean;
begin
  result := GetInfoIntValue(item) <> 0;
end;

function TUIBDataBase.GetInfoStringValue(const item: integer): string;
var
  size: byte;
  data: RawByteString;
begin
  SetConnected(true);
  data := FLibrary.DatabaseInfoString(FDbHandle, item, 256);
  case Item of
    isc_info_cur_logfile_name, isc_info_wal_prv_ckpt_fname:
      begin
        size := byte(data[4]);
        Move(data[5], data[1], size);
        SetLength(data, size);
      end;
  else
    size := byte(data[5]);
    Move(data[6], data[1], size);
    SetLength(data, size);
  end;
  Result := string(data);
end;

function TUIBDataBase.GetInfoOperationsCount(
  const item: Integer): Integer;
var
  data: AnsiString;
  i: Integer;
  p: PTableOperation;
begin
  SetConnected(true);
  result := 0;
  data := FLibrary.DatabaseInfoString(FDbHandle, Item, 8);
  for i := 0 to PWord(@data[2])^ div sizeof(TTableOperation) - 1 do
  begin
    p := PTableOperation(@data[4+ i * sizeof(TTableOperation)]);
    inc(result, p^.Count);
    case item of
      isc_info_read_seq_count: if assigned(FOnInfoReadSeqCount) then FOnInfoReadSeqCount(self, p);
      isc_info_read_idx_count: if assigned(FOnInfoReadIdxCount) then FOnInfoReadIdxCount(self, p);
      isc_info_update_count  : if assigned(FOnInfoUpdateCount) then FOnInfoUpdateCount(self, p);
      isc_info_insert_count  : if assigned(FOnInfoInsertCount) then FOnInfoInsertCount(self, p);
      isc_info_delete_count  : if assigned(FOnInfoDeleteCount) then FOnInfoDeleteCount(self, p);
      isc_info_backout_count : if assigned(FOnInfoBackoutCount) then FOnInfoBackoutCount(self, p);
      isc_info_purge_count   : if assigned(FOnInfoPurgeCount) then FOnInfoPurgeCount(self, p);
      isc_info_expunge_count : if assigned(FOnInfoExpungeCount) then FOnInfoExpungeCount(self, p);
    end;
  end;
end;

function TUIBDataBase.GetInfoIntCount(const item: Integer): Integer;
var
  data: AnsiString;
  p: PAnsiChar;
begin
  SetConnected(true);
  data := FLibrary.DatabaseInfoString(FDbHandle, item, 256);
  p := PAnsiChar(data);
  result := 0;
  while byte(p^) = item do
  begin
    inc(result, 1);
  {$IFDEF FB102_UP}
    case item of
      isc_info_active_transactions:
        if assigned(FOnInfoActiveTransactions) then
          FOnInfoActiveTransactions(self, PInteger(@p[3])^);
    end;
  {$ENDIF}
    inc(p, 7);
  end;
end;

function TUIBDataBase.GetInfoStringCount(const item: Integer): Integer;
var
  data: AnsiString;
  p: PAnsiChar;
  len: integer;
begin
  SetConnected(true);
  data := FLibrary.DatabaseInfoString(FDbHandle, item, 256);
  p := PAnsiChar(data);
  result := 0;
  while byte(p^) = item do
  begin
    inc(result, 1);
    len := byte(p[3]);
    inc(p, 4);
    case item of
      isc_info_user_names:
        if assigned(FOnInfoUserNames) then
          FOnInfoUserNames(self, string(copy(p, 0, len)));
    end;
    inc(p, len);
  end;
end;

function TUIBDataBase.GetInfoDbId(const Index: Integer): string;
var
  data: AnsiString;
  p: PAnsiChar;
  i: Integer;
begin
  SetConnected(true);
  Data := FLibrary.DatabaseInfoString(FDBHandle, isc_info_db_id, 1024);
  p := @data[5];
  for i := 1 to ord(data[4]) do
  begin
    if Index = i then
    begin
      result := string(copy(p+1, 0, ord(p^)));
      Break;
    end;
    inc(p, ord(p^)+1);
  end;
end;

procedure TUIBDataBase.UnRegisterEvents;
var i: Integer;
begin
  for i := 0 to FEventNotifiers.Count - 1 do
    TUIBEvents(FEventNotifiers.Items[i]).Registered := false;
end;

procedure TUIBDataBase.RegisterEvents;
var i: Integer;
begin
  for i := 0 to FEventNotifiers.Count - 1 do
    with TUIBEvents(FEventNotifiers.Items[i]) do
      if AutoRegister then
        Registered := true;
end;

procedure TUIBDataBase.RemoveEventNotifier(Event: TUIBEvents);
var
  i : integer;
begin
  i := FEventNotifiers.IndexOf(Event);
  if (i >= 0) then
    FEventNotifiers.Delete(i);
end;

procedure TUIBDataBase.RemoveStatement(Statement: TUIBStatement);
begin
  if (FStatements <> nil) then
  begin
    FStatements.Remove(Statement);
    if FStatements.Count = 0 then
    begin
      FStatements.free;
      FStatements := nil;
    end;
  end;
end;

procedure TUIBDataBase.AddEventNotifier(Event: TUIBEvents);
begin
  FEventNotifiers.Add(Event);
end;

procedure TUIBDataBase.AddStatement(Statement: TUIBStatement);
begin
  if (FStatements = nil) then
    FStatements := TList.Create;
  FStatements.Add(Statement);
end;

procedure TUIBDataBase.ActiveAllTriggers;
var
  triggers: TUIBQuery;
  tr: TUIBTransaction;
begin
  tr := TUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  triggers := TUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.DataBase := Self;
    triggers.CachedFetch := false;
    triggers.Transaction := tr;
    triggers.SQL.Text :=
      'SELECT T.RDB$TRIGGER_NAME FROM RDB$TRIGGERS T '+
      'LEFT JOIN RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = T.RDB$TRIGGER_NAME '+
      'WHERE ((T.RDB$SYSTEM_FLAG = 0) OR (T.RDB$SYSTEM_FLAG IS NULL)) '+
      'AND (C.RDB$TRIGGER_NAME IS NULL) AND (T.RDB$TRIGGER_INACTIVE = 1)';
    triggers.Open;
    while not triggers.Eof do
    begin
      tr.ExecuteImmediate(format('ALTER TRIGGER %s ACTIVE', [triggers.Fields.AsString[0]]));
      triggers.Next;
    end;
  finally
    triggers.Free;
    tr.Free;
  end;
end;

procedure TUIBDataBase.DeactiveAllTriggers;
var
  triggers: TUIBQuery;
  tr: TUIBTransaction;
begin
  tr := TUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  triggers := TUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.DataBase := Self;
    triggers.CachedFetch := false;
    triggers.Transaction := tr;
    triggers.SQL.Text :=
      'SELECT T.RDB$TRIGGER_NAME FROM RDB$TRIGGERS T '+
      'LEFT JOIN RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = T.RDB$TRIGGER_NAME '+
      'WHERE ((T.RDB$SYSTEM_FLAG = 0) OR (T.RDB$SYSTEM_FLAG IS NULL)) '+
      'AND (C.RDB$TRIGGER_NAME IS NULL) AND (T.RDB$TRIGGER_INACTIVE = 0)';
    triggers.Open;
    while not triggers.Eof do
    begin
      tr.ExecuteImmediate(format('ALTER TRIGGER %s INACTIVE', [triggers.Fields.AsString[0]]));
      triggers.Next;
    end;
  finally
    triggers.Free;
    tr.Free;
  end;
end;

procedure TUIBDataBase.RecomputeSelectivityIndices;
var
  indices: TUIBQuery;
  tr: TUIBTransaction;
begin
  tr := TUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  indices := TUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.DataBase := Self;
    indices.CachedFetch := false;
    indices.Transaction := tr;
    indices.SQL.Text := 'SELECT RDB$INDEX_NAME FROM RDB$INDICES';
    indices.Open;
    while not indices.Eof do
    begin
      tr.ExecuteImmediate(format('SET STATISTICS INDEX %s', [indices.Fields.AsString[0]]));
      indices.Next;
    end;
  finally
    indices.Free;
    tr.Free;
  end;
end;

procedure TUIBDataBase.RecompileAllProcedures;
var
  Meta: TMetaDataBase;
  tr: TUIBTransaction;
  i: integer;
begin
  meta := TMetaDataBase.Create(nil, -1);
  tr := TUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.Database := Self;
    meta.OIDDatabases := [OIDProcedure];
    Meta.LoadFromDatabase(tr);
    for i := 0 to Meta.ProceduresCount - 1 do
      tr.ExecuteImmediate(Meta.Procedures[i].AsAlterDDL);
  finally
    meta.Free;
    tr.Free;
  end;
end;

procedure TUIBDataBase.RecompileAllTriggers;
var
  triggers: TUIBQuery;
  tr: TUIBTransaction;
begin
  tr := TUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  triggers := TUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  try
    tr.DataBase := Self;
    triggers.CachedFetch := false;
    triggers.FetchBlobs := true;
    triggers.Transaction := tr;
    triggers.SQL.Text :=
      'SELECT T.RDB$TRIGGER_NAME, T.RDB$TRIGGER_SOURCE FROM RDB$TRIGGERS T '+
      'LEFT JOIN RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = T.RDB$TRIGGER_NAME '+
      'WHERE ((T.RDB$SYSTEM_FLAG = 0) OR (T.RDB$SYSTEM_FLAG IS NULL)) AND (C.RDB$TRIGGER_NAME IS NULL)';
    triggers.Open;
    while not triggers.Eof do
    begin
      with triggers.Fields do
        tr.ExecuteImmediate(format('ALTER TRIGGER %s'#13'%s', [AsString[0], AsString[1]]));
      triggers.Next;
    end;
  finally
    triggers.Free;
    tr.Free;
  end;
end;

{$IFDEF FB25_UP}
function TUIBDataBase.CancelEnable: Boolean;
begin
  if FDbHandle <> nil then
    Result := FLibrary.DatabaseCancelOperation(FDbHandle, fb_cancel_enable) else
    Result := False;
end;

function TUIBDataBase.CancelDisable: Boolean;
begin
  if FDbHandle <> nil then
    Result := FLibrary.DatabaseCancelOperation(FDbHandle, fb_cancel_disable) else
    Result := False;
end;

function TUIBDataBase.CancelAbort: Boolean;
begin
  if FDbHandle <> nil then
    Result := FLibrary.DatabaseCancelOperation(FDbHandle, fb_cancel_abort) else
    Result := False;
end;

function TUIBDataBase.CancelRaise: Boolean;
begin
  if FDbHandle <> nil then
    Result := FLibrary.DatabaseCancelOperation(FDbHandle, fb_cancel_raise) else
    Result := False;
end;
{$ENDIF}

function TUIBDataBase.GetRole: string;
begin
  Result := ReadParamString('sql_role_name');
end;

procedure TUIBDataBase.SetRole(const Value: string);
begin
  WriteParamString('sql_role_name', Value);
end;

procedure TUIBDataBase.ClearEvents;
var
  i: integer;
begin
  for i := 0 to FEventNotifiers.Count - 1 do
    TUIBEvents(FEventNotifiers[i]).SetDatabase(nil);
end;

function TUIBDataBase.CanConnect: Boolean;
begin
  Result := True;
end;

{ TUIBStatement }

procedure TUIBStatement.SetTransaction(const Transaction: TUIBTransaction);
begin
  if (FTransaction <> Transaction) then
  begin
    if (FTransaction <> nil) then
    begin
      InternalTryCache(etmStayIn, True);
      FTransaction.RemoveSQLComponent(Self);
    end;
    FTransaction := Transaction;
    if (Transaction <> nil) then
    begin
      Transaction.AddSQLComponent(Self);
      if Transaction.DataBase <> nil then
      begin
        FParameter.CharacterSet := Transaction.DataBase.CharacterSet;
        // if not a multibase transaction affect database
        if Transaction.DatabasesCount <= 1 then
          Database := Transaction.DataBase;
      end;
    end;
  end;
end;

procedure TUIBStatement.SetDataBase(ADataBase: TUIBDataBase);
begin
  if (FDataBase <> ADataBase) then
  begin
    InternalClose(etmStayIn, True, True);
    if FDataBase <> nil then
      FDataBase.RemoveStatement(Self);
    FDataBase := ADataBase;
    if FDataBase <> nil then
    begin
      FDataBase.AddStatement(Self);
      FParameter.CharacterSet := FDataBase.CharacterSet;
    end;
  end;
end;

procedure TUIBStatement.BeginTransaction;
begin
  NeedTransaction;
  FCurrentState := qsTransaction;
end;

procedure TUIBStatement.Close(const Mode: TEndTransMode);
begin
  InternalTryCache(Mode, False);
end;

procedure TUIBStatement.Open(FetchFirst: boolean = True);
begin
  // if you reopen the same query I Close
  // the cursor, clean sql result and
  // execute the query again to save
  // the prepare time !
  if (FCurrentState = qsExecute) then
    CloseCursor;

  if FetchFirst then
    InternalNext else
    BeginExecute;
end;

procedure TUIBStatement.NeedTransaction;
begin
  if FTransaction <> nil then
    FTransaction.BeginTransaction else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
end;

procedure TUIBStatement.Next;
begin
  if (FCurrentState <> qsExecute) then
    raise Exception.Create(EUIB_MUSTBEOPEN);
  InternalNext;
end;

procedure TUIBStatement.Prior;
begin
  InternalPrior;
end;

procedure TUIBStatement.Last;
begin
  FetchAll;
end;

procedure TUIBStatement.First;
begin
  if (FSQLResult <> nil) and
   (FSQLResult.RecordCount > 0) then
   FSQLResult.CurrentRecord := 0;
end;

procedure TUIBStatement.FetchAll;
begin
  while not Eof do Next;
end;

procedure TUIBStatement.Execute;
begin
  BeginExecute;
end;

procedure TUIBStatement.ExecSQL;
begin
  if FCurrentState > qsExecImme then
    BeginExecute else // it shouldn't happen ...
    BeginExecImme;
end;

procedure TUIBStatement.Prepare(describeParams: boolean = false);
begin
  if (FCurrentState < qsPrepare) then
    BeginPrepare(describeParams);
end;

procedure TUIBStatement.InternalNext;
begin
  if (FCurrentState < qsExecute) then
    BeginExecute;

  if FSQLResult.FieldCount > 0 then
  if Fields.ScrollEOF then
    Fields.Next else
  begin
    with FindDataBase, FLibrary do
    try
      if FSQLResult.FetchBlobs then
        DSQLFetchWithBlobs(FDbHandle, FTransaction.FTrHandle, FStHandle, GetSQLDialect, FSQLResult) else
        DSQLFetch(FDbHandle, FTransaction.FTrHandle, FStHandle, GetSQLDialect, FSQLResult);
    except
      if FOnError <> etmStayIn then
        EndExecute(FOnError, False, True);
      raise;
    end;
  end;
end;

procedure TUIBStatement.InternalPrior;
begin
  if Fields.CachedFetch then
  begin
    if Fields.CurrentRecord > 0 then
      Fields.CurrentRecord := Fields.CurrentRecord - 1;
  end else
    raise Exception.Create(EUIB_CACHEDFETCHNOTSET);
end;

procedure TUIBStatement.InternalReadBlobW(sqlda: TSQLDA; const Index: Word;
  var str: UnicodeString);
var
  aStr: RawByteString;
begin
  InternalReadBlobB(sqlda, Index, aStr);
  str := MBUDecode(aStr, CharacterSetCP[sqlda.CharacterSet]);
end;

procedure TUIBStatement.InternalTryCache(const Mode: TEndTransMode; Auto: boolean);
begin
  if FDatabase <> nil then
  begin
    CloseCursor;
    if FTransaction <> nil then
      FTransaction.EndTransaction(Mode, Self, Auto);
  end else
    InternalClose(Mode, Auto, True);
end;

procedure TUIBStatement.EndTransaction(const ETM: TEndTransMode; Auto: boolean);
begin
  if FTransaction <> nil then
  begin
    if FTransaction.EndTransaction(ETM, Self, Auto) then
      FCurrentState := qsDataBase;
  end else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
end;

procedure TUIBStatement.BeginStatement;
begin
  BeginTransaction;
  with FindDataBase.FLibrary do
  try
{$IFDEF FB25_UP}
    if FStHandle = nil then
{$ELSE}
    FStHandle := nil;
{$ENDIF}
      DSQLAllocateStatement(FindDataBase.FDbHandle, FStHandle);
  except
    EndTransaction(FOnError, False);
    raise;
  end;
  FCurrentState := qsStatement;
end;

procedure TUIBStatement.EndStatement(const ETM: TEndTransMode; Auto, Drop: boolean);
begin
  with FindDataBase.FLibrary do
{$IFDEF FB25_UP}
    if Drop then
    begin
{$ENDIF}
      DSQLFreeStatement(FStHandle, DSQL_drop);
      FStHandle := nil;
{$IFDEF FB25_UP}
    end else
      DSQLFreeStatement(FStHandle, DSQL_unprepare);
{$ENDIF}

  FCurrentState := qsTransaction;
  if (ETM <> etmStayIn) then
    EndTransaction(ETM, Auto);

  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TUIBStatement.BeginPrepare(describeParams: boolean = false);
begin
  if (FStHandle = nil) then
    BeginStatement else
    BeginTransaction;

  FSQLResult := ResultClass.Create(FindDataBase.CharacterSet, 0, FCachedFetch, FFetchBlobs, FBufferChunks);
  with FindDataBase, FLibrary do
  try
    if (FQuickScript or (not FParseParams)) then
{$IFDEF UNICODE}
      FStatementType := DSQLPrepare(FDbHandle, FTransaction.FTrHandle, FStHandle,
        MBUEncode(FSQL.Text, CharacterSetCP[CharacterSet]), GetSQLDialect, FSQLResult) else
      FStatementType := DSQLPrepare(FDbHandle, FTransaction.FTrHandle, FStHandle,
        MBUEncode(FParsedSQL, CharacterSetCP[CharacterSet]), GetSQLDialect, FSQLResult);
{$ELSE}
      FStatementType := DSQLPrepare(FDbHandle, FTransaction.FTrHandle, FStHandle,
        FSQL.Text, GetSQLDialect, FSQLResult) else
      FStatementType := DSQLPrepare(FDbHandle, FTransaction.FTrHandle, FStHandle,
        FParsedSQL, GetSQLDialect, FSQLResult);
{$ENDIF}
      FCursorName := 'C' + inttostr(PtrInt(FStHandle));
      if FUseCursor and (FStatementType in [stSelect, stSelectForUpdate]) then
        DSQLSetCursorName(FStHandle, AnsiString(FCursorName));
      if describeParams and (FParameter.ParamCount > 0) then
        DSQLDescribeBind(FStHandle, GetSQLDialect, FParameter);
  except
    FSQLResult.free;
    FSQLResult := nil;
    EndStatement(FOnError, False, True);
    raise;
  end;
  FCurrentState := qsPrepare;
end;

procedure TUIBStatement.EndPrepare(const ETM: TEndTransMode; Auto, Drop: boolean);
begin
  FSQLResult.free;
  FSQLResult := nil;
  FCurrentState := qsStatement;
  EndStatement(ETM, Auto, Drop);
end;

procedure TUIBStatement.BeginExecute;
begin
  if (FSQLResult = nil) then BeginPrepare;
  with FindDataBase, FLibrary do
  try
    FTransaction.BeginTransaction(true);
    if (FStatementType = stExecProcedure) then
      DSQLExecute2(FTransaction.FTrHandle, FStHandle,
        GetSQLDialect, FParameter, FSQLResult) else
      DSQLExecute(FTransaction.FTrHandle, FStHandle,
        GetSQLDialect, FParameter);
  except
    if (FOnError <> etmStayIn) then
      EndPrepare(FOnError, False, True);
    raise;
  end;
  FCurrentState := qsExecute;
end;

procedure TUIBStatement.EndExecute(const ETM: TEndTransMode; Auto, Drop: boolean);
begin
  FCurrentState := qsPrepare;
  EndPrepare(ETM, Auto, Drop);
end;

procedure TUIBStatement.AffectedRows(out SelectedRows, InsertedRows, UpdatedRows,
  DeletedRows: Cardinal);
begin
  with FindDataBase, FLibrary do
    DSQLInfoRowsAffected2(FStHandle, SelectedRows, InsertedRows, UpdatedRows, DeletedRows);
end;

procedure TUIBStatement.BeginExecImme;
var
  I: Integer;
  procedure ExecuteQuery(const AQuery: string; sqlParams: TSQLParams);
  begin
    if (Trim(AQuery) = '') then exit;
    with FindDataBase, FLibrary do
    try
{$IFDEF UNICODE}
      DSQLExecuteImmediate(FDbHandle, FTransaction.FTrHandle,
        MBUEncode(AQuery, CharacterSetCP[CharacterSet]), GetSQLDialect, sqlParams);
{$ELSE}
      DSQLExecuteImmediate(FDbHandle, FTransaction.FTrHandle,
        AQuery, GetSQLDialect, sqlParams);
{$ENDIF}
    except
      if (FOnError <> etmStayIn) then
        EndExecImme(FOnError, False, False);
      raise;
    end;
  end;
begin
  BeginTransaction;
  if FQuickScript then
    for i := 0 to FSQL.Count - 1 do
    begin
      ExecuteQuery(FSQL.Strings[i], nil);
    end else
      if FParseParams then
        ExecuteQuery(FParsedSQL, FParameter) else
        ExecuteQuery(FSQL.Text, FParameter);
  FCurrentState := qsExecImme;
end;

procedure TUIBStatement.EndExecImme(const ETM: TEndTransMode; Auto, Drop: boolean);
begin
{$IFDEF FB25_UP}
  if (FStHandle <> nil) then
    EndStatement(ETM, Auto, Drop);

{$ENDIF}

  FCurrentState := qsTransaction;
  if (ETM <> etmStayIn) then
    EndTransaction(ETM, Auto);
end;

function TUIBStatement.ParamsClass: TSQLParamsClass;
begin
  Result := TSQLParams;
end;

function TUIBStatement.ResultClass: TSQLResultClass;
begin
  Result := TSQLResult;
end;

procedure TUIBStatement.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
end;

function TUIBStatement.GetPlan: string;
begin
  if (FCurrentState < qsPrepare) then
    Raise Exception.Create(EUIB_MUSTBEPREPARED)else
      Result := FindDataBase.FLibrary.DSQLInfoPlan(FStHandle);
end;

function TUIBStatement.GetStatementType: TUIBStatementType;
begin
  if (FCurrentState < qsPrepare) then
    Raise Exception.Create(EUIB_MUSTBEPREPARED) else
    Result := FStatementType;
end;

procedure TUIBStatement.DoSQLChange(Sender: TObject);
begin
  InternalClose(etmStayIn, True, False);
  if (not FQuickScript or FParseParams) then
    FParsedSQL := FParameter.Parse(FSQL.Text);
end;

function TUIBStatement.GetFields: TSQLResult;
begin
  if (FSQLResult = nil) then
    raise Exception.Create(EUIB_QUERYNOTOPEN);
  Result := FSQLResult;
end;

function TUIBStatement.GetEof: boolean;
begin
  if Assigned(FSQLResult) then
    Result := FSQLResult.Eof else
    Result := True;
end;

function TUIBStatement.GetBof: boolean;
begin
  if Assigned(FSQLResult) then
    Result := FSQLResult.Bof else
    Result := True;
end;

function TUIBStatement.FindDataBase: TUIBDataBase;
begin
  if FDataBase <> nil then
    result := FDataBase else
     if FTransaction <> nil then
       result := FTransaction.FDataBase else
       raise Exception.Create(EUIB_DATABASENOTDEF);
end;

{$IFNDEF UIB_NO_COMPONENT}
procedure TUIBStatement.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent = FTransaction) and (Operation = opRemove)) then
    SetTransaction(nil);
  if ((AComponent = FDataBase) and (Operation = opRemove)) then
    SetDataBase(nil);
end;
{$ENDIF}

constructor TUIBStatement.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FSQLResult := nil;
  FUseCursor := True;
  FCurrentState := qsDataBase;
  FSQL         := TStringList.Create;
  TStringList(FSQL).OnChange := DoSQLChange;
  FCachedFetch := True;
  FetchBlobs   := False;
  FQuickScript := False;
  FOnError     := etmRollback;
  FParameter   := ParamsClass.Create(GetSystemCharacterset);
  FCursorName  := '';
  FBufferChunks := 1000;
  FParseParams := True;
{$IFNDEF UIB_NO_COMPONENT}
  if (AOwner is TUIBTransaction) then
    Transaction := TUIBTransaction(AOwner) else
{$ENDIF}
    FTransaction := nil;
  FStHandle := nil;
end;

destructor TUIBStatement.Destroy;
begin
  FSQL.Free;
  FParameter.free;
  FParameter := nil;
  InternalClose(etmStayIn, False, True);
  SetDataBase(nil);
  SetTransaction(nil);
  inherited;
end;

procedure TUIBStatement.ReadBlobB(const Index: Word; var str: RawByteString);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlobB(Index, Str) else
    InternalReadBlobB(Fields, Index, str);
end;

procedure TUIBStatement.ReadBlobA(const Index: Word; var Str: AnsiString);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlobA(Index, Str) else
    InternalReadBlobA(Fields, Index, str);
end;

procedure TUIBStatement.ReadBlob(const Index: Word; Stream: TStream);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Stream) else
    InternalReadBlob(Fields, Index, Stream);
end;

procedure TUIBStatement.ReadBlob(const Index: Word; var Value: Variant);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Value) else
    InternalReadBlob(Fields, Index, Value);
end;

procedure TUIBStatement.ReadBlob(const name: string; Stream: TStream);
begin
  ReadBlob(Fields.GetFieldIndex(AnsiString(Name)), Stream);
end;

procedure TUIBStatement.ReadBlobA(const name: string; var str: AnsiString);
begin
  ReadBlobA(Fields.GetFieldIndex(AnsiString(Name)), str);
end;

procedure TUIBStatement.ReadBlobB(const name: string; var str: RawByteString);
begin
  ReadBlobB(Fields.GetFieldIndex(AnsiString(Name)), str);
end;

procedure TUIBStatement.ReadBlob(const name: string; var Value: Variant);
begin
  ReadBlob(Fields.GetFieldIndex(AnsiString(Name)), Value);
end;

procedure TUIBStatement.ParamsSetBlob(const Index: Word; Stream: TStream);
var BlobHandle: IscBlobHandle;
begin
  NeedTransaction;
  BlobHandle := nil;
  with FindDataBase.FLibrary do
  begin
    Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteStream(BlobHandle, Stream);
    finally
      BlobClose(BlobHandle);
    end;
  end;
end;

procedure TUIBStatement.ParamsSetBlobA(const Index: Word; const str: AnsiString);
begin
{$IFDEF UNICODE}
   ParamsSetBlobW(Index, string(str));
{$ELSE}
   ParamsSetBlobB(Index, str);
{$ENDIF}
end;

procedure TUIBStatement.ParamsSetBlobB(const Index: Word; const str: RawByteString);
var
  BlobHandle: IscBlobHandle;
begin
  NeedTransaction;
  BlobHandle := nil;
  with FindDataBase.FLibrary do
  begin
    Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteString(BlobHandle, str);
    finally
      BlobClose(BlobHandle);
    end;
  end;
end;

procedure TUIBStatement.ParamsSetBlob(const Index: Word; Buffer: Pointer;
  Size: Cardinal);
var BlobHandle: IscBlobHandle;
begin
  NeedTransaction;
  BlobHandle := nil;
  with FindDataBase.FLibrary do
  begin
    Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteSegment(BlobHandle, Size, Buffer);
    finally
      BlobClose(BlobHandle);
    end;
  end;
end;

procedure TUIBStatement.ParamsSetBlob(const Name: string; Stream: TStream);
var BlobHandle: IscBlobHandle;
begin
  NeedTransaction;
  BlobHandle := nil;
  with FindDataBase.FLibrary do
  begin
    Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteStream(BlobHandle, Stream);
    finally
      BlobClose(BlobHandle);
    end;
  end;
end;

procedure TUIBStatement.ParamsSetBlobA(const Name: string; const str: AnsiString);
begin
{$IFDEF UNICODE}
   ParamsSetBlobW(Name, string(str));
{$ELSE}
   ParamsSetBlobB(Name, str);
{$ENDIF}
end;

procedure TUIBStatement.ParamsSetBlobB(const Name: string; const str: RawByteString);
var
  BlobHandle: IscBlobHandle;
begin
  NeedTransaction;
  BlobHandle := nil;
  with FindDataBase.FLibrary do
  begin
    Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteString(BlobHandle, str);
    finally
      BlobClose(BlobHandle);
    end;
  end;
end;

procedure TUIBStatement.ParamsSetBlobW(const Name: string; const str: UnicodeString);
begin
  ParamsSetBlobB(Name, MBUEncode(str, CharacterSetCP[Params.CharacterSet]))
end;

procedure TUIBStatement.ParamsSetBlob(const Name: string; const str: string);
begin
{$IFDEF UNICODE}
  ParamsSetBlobW(Name, str);
{$ELSE}
  ParamsSetBlobA(Name, str);
{$ENDIF}
end;

procedure TUIBStatement.ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Cardinal);
var BlobHandle: IscBlobHandle;
begin
  NeedTransaction;
  BlobHandle := nil;
  with FindDataBase.FLibrary do
  begin
    Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteSegment(BlobHandle, Size, Buffer);
    finally
      BlobClose(BlobHandle);
    end;
  end;
end;

procedure TUIBStatement.ParamsSetBlobW(const Index: Word;
  const str: UnicodeString);
begin
  ParamsSetBlobB(Index, MBUEncode(str, CharacterSetCP[Params.CharacterSet]))
end;

procedure TUIBStatement.ParamsSetBlob(const Index: Word; const str: string);
begin
{$IFDEF UNICODE}
   ParamsSetBlobW(Index, str);
{$ELSE}
   ParamsSetBlobA(Index, str);
{$ENDIF}
end;

procedure TUIBStatement.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  Stream: TStream);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
    with FindDataBase.FLibrary do
    begin
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, sqlda.AsQuad[Index]);
      try
        BlobSaveToStream(BlobHandle, Stream);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;
end;

procedure TUIBStatement.InternalReadBlobA(sqlda: TSQLDA; const Index: Word;
  var str: AnsiString);
{$IFDEF UNICODE}
var
  data: UnicodeString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  InternalReadBlobW(sqlda, Index, data);
  str := AnsiString(data);
{$ELSE}
  InternalReadBlobB(sqlda, Index, str);
{$ENDIF}
end;

procedure TUIBStatement.InternalReadBlob(sqlda: TSQLDA; const Index: Word; var str: string);
begin
{$IFDEF UNICODE}
  InternalReadBlobW(sqlda, Index, str);
{$ELSE}
  InternalReadBlobA(sqlda, Index, str);
{$ENDIF}
end;

procedure TUIBStatement.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  var Value: Variant);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
    with FindDataBase.FLibrary do
    begin
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, sqlda.AsQuad[Index]);
      try
        BlobReadVariant(BlobHandle, Value);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;
end;

procedure TUIBStatement.InternalClose(const Mode: TEndTransMode;
  Auto, Drop: boolean);
begin
  case FCurrentState of
    qsStatement : EndStatement(Mode, Auto, Drop);
    qsExecImme  : EndExecImme(Mode, Auto, Drop);
    qsPrepare   : EndPrepare(Mode, Auto, Drop);
    qsExecute   : EndExecute(Mode, Auto, Drop);
  end;
end;

procedure TUIBStatement.InternalGetBlobSize(sqlda: TSQLDA; const Index: Word; out Size: Cardinal);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
    with FindDataBase.FLibrary do
    begin
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, sqlda.AsQuad[Index]);
      try
        BlobSize(BlobHandle, Size);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;
end;

function TUIBStatement.FieldBlobSize(const Index: Word): Cardinal;
begin
  if Fields.FetchBlobs then
    Result := Fields.GetBlobSize(Index) else
    InternalGetBlobSize(Fields, Index, Result);
end;

function TUIBStatement.ParamBlobSize(const Index: Word): Cardinal;
begin
  InternalGetBlobSize(Params, Index, Result);
end;

procedure TUIBStatement.ReadBlob(const Index: Word; Buffer: Pointer);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Buffer) else
    InternalReadBlob(Fields, Index, Buffer);
end;

procedure TUIBStatement.ReadBlob(const name: string; Buffer: Pointer);
begin
  ReadBlob(Fields.GetFieldIndex(AnsiString(Name)), Buffer);
end;

function TUIBStatement.ReadBlob(const Index: Word): string;
begin
  ReadBlob(Index, Result);
end;

function TUIBStatement.ReadBlob(const name: string): string;
begin
  ReadBlob(Name, Result);
end;

procedure TUIBStatement.ReadBlobW(const name: string; var str: UnicodeString);
begin
  ReadBlobW(Fields.GetFieldIndex(AnsiString(Name)), str);
end;

procedure TUIBStatement.ReadBlob(const name: string; var str: string);
begin
{$IFDEF UNICODE}
   ReadBlobW(name, str);
{$ELSE}
   ReadBlobA(name, str);
{$ENDIF}
end;

procedure TUIBStatement.ReadBlobW(const Index: Word; var str: UnicodeString);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlobW(Index, Str) else
    InternalReadBlobW(Fields, Index, str);
end;

procedure TUIBStatement.ReadBlob(const Index: Word; var str: string);
begin
{$IFDEF UNICODE}
   ReadBlobW(Index, str);
{$ELSE}
   ReadBlobA(Index, str);
{$ENDIF}
end;

procedure TUIBStatement.InternalReadBlob(sqlda: TSQLDA;
  const Index: Word; Buffer: Pointer);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if sqlda.IsNull[Index] then
     Exit else
  begin
    with FindDataBase.FLibrary do
    begin
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, sqlda.AsQuad[Index]);
      try
        BlobReadSizedBuffer(BlobHandle, Buffer);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;
end;

procedure TUIBStatement.InternalReadBlobB(sqlda: TSQLDA; const Index: Word;
  var str: RawByteString);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if sqlda.IsNull[Index] then
     str := '' else
  begin
    with FindDataBase.FLibrary do
    begin
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, sqlda.AsQuad[Index]);
      try
        BlobReadString(BlobHandle, str);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;
end;

function TUIBStatement.GetRowsAffected: Cardinal;
begin
  if (FCurrentState < qsPrepare) then
    Raise Exception.Create(EUIB_MUSTBEPREPARED) else
    Result := FindDataBase.FLibrary.DSQLInfoRowsAffected(FStHandle, FStatementType);
end;

procedure TUIBStatement.CloseCursor;
begin
  if (FCurrentState = qsExecute) then
  begin
    try
      FSQLResult.ClearRecords;
      if FUseCursor and (FStatementType in [stSelect, stSelectForUpdate]) then
        with FindDataBase.FLibrary do
          DSQLFreeStatement(FStHandle, DSQL_close);
    except
      InternalClose(FOnError, False, True);
      raise;
    end;
    FCurrentState := qsPrepare;
  end;
end;

{$IFDEF DELPHI14_UP}
function TUIBStatement.Select<T>(const AFilter: TUIBMethodFilter<T>): IUIBEnumerable<T>;
begin
  Result := TUIBEnumerable<T>.Create(Self, AFilter);
end;
{$ENDIF}

{$IFDEF DELPHI14_UP}
function TUIBStatement.All<T>: IUIBEnumerable<T>;
begin
  Result := TUIBEnumerable<T>.Create(Self, nil);
end;
{$ENDIF}

{ TUIBQuery }

procedure TUIBQuery.BuildStoredProc(const StoredProc: string; forSelect: boolean = true);
var
  i, r: Integer;
  Str: string;
begin
  InternalClose(etmStayIn, True, False);
  r := 0;
  TStringList(FSQL).OnChange := nil;
  try
    Params.Clear;
    FParsedSQL :=
      'SELECT RDB$FIELD_TYPE, RDB$PARAMETER_NAME, RDB$FIELD_SCALE, RDB$PARAMETER_TYPE '+
       'FROM RDB$PROCEDURE_PARAMETERS PRM JOIN RDB$FIELDS FLD ON '+
       'PRM.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME '+
      'WHERE '+
          'PRM.RDB$PROCEDURE_NAME = ''' + SQLUnQuote(StoredProc) + ''' '+
      'ORDER BY RDB$PARAMETER_TYPE, PRM.RDB$PARAMETER_NUMBER';
    Open;
    try
      while not Eof do
      begin
        with Fields do
        if AsSmallint[3] = 0 then
        begin
          if AsSmallint[2] < 0 then
          begin
            case Fields.AsSmallint[0] of
              blr_short:  Params.AddFieldType(Trim(AsString[1]), uftNumeric, - AsSmallint[2], 4);
              blr_long :  Params.AddFieldType(Trim(AsString[1]), uftNumeric, - AsSmallint[2], 7);
              blr_int64,
              blr_quad,
              blr_double: Params.AddFieldType(Trim(AsString[1]), uftNumeric, - AsSmallint[2], 15);
            else
              Raise Exception.Create(EUIB_UNEXPECTEDERROR);
            end;
          end else
          case Fields.AsSmallint[0] of
            blr_text,
            blr_text2,
            blr_varying,
            blr_varying2,
            blr_cstring,
            blr_cstring2  : Params.AddFieldType(Trim(AsString[1]), uftChar);
            blr_float,
            blr_d_float   : Params.AddFieldType(Trim(AsString[1]), uftFloat);
            blr_short     : Params.AddFieldType(Trim(AsString[1]), uftSmallint);
            blr_long      : Params.AddFieldType(Trim(AsString[1]), uftInteger);
            blr_quad      : Params.AddFieldType(Trim(AsString[1]), uftQuad);
            blr_double    : Params.AddFieldType(Trim(AsString[1]), uftDoublePrecision);
            blr_timestamp : Params.AddFieldType(Trim(AsString[1]), uftTimestamp);
            blr_blob,
            blr_blob_id   : Params.AddFieldType(Trim(AsString[1]), uftBlob);
            blr_sql_date  : Params.AddFieldType(Trim(AsString[1]), uftDate);
            blr_sql_time  : Params.AddFieldType(Trim(AsString[1]), uftTime);
            blr_int64     : Params.AddFieldType(Trim(AsString[1]), uftInt64);
          {$IFDEF IB7_UP}
            blr_boolean_dtype : Params.AddFieldType(Trim(AsString[1]), uftBoolean);
          {$ENDIF}
          else
            // shouldn't occur but ...
            raise Exception.Create(EUIB_UNEXPECTEDERROR);
          end
        end else
          inc(r);
        Next;
      end;
      if (Params.FieldCount > 0) then
      begin
        FParsedSQL := ' (';
        Str        := ' (';
        for i := 0 to Params.FieldCount - 1 do
        begin
          FParsedSQL := FParsedSQL + '?,';
          Str        := Str        + ':'+ Params.FieldName[i] +','
        end;
        FParsedSQL[Length(FParsedSQL)] := ')';
        Str[Length(Str)] := ')';
        if ((r > 0) and forSelect) then
          begin
            FParsedSQL := 'SELECT * FROM ' + SQLQuote(StoredProc) + FParsedSQL;
            FSQL.Text  := 'SELECT * FROM ' + SQLQuote(StoredProc) + Str;
          end else
          begin
            FParsedSQL := 'EXECUTE PROCEDURE ' + SQLQuote(StoredProc) + FParsedSQL;
            FSQL.Text  := 'EXECUTE PROCEDURE ' + SQLQuote(StoredProc) + Str;
          end;
      end else
        begin
          if r > 0 then
            FParsedSQL := 'SELECT * FROM ' + SQLQuote(StoredProc) else
            FParsedSQL := 'EXECUTE PROCEDURE ' + SQLQuote(StoredProc);
          FSQL.Text := FParsedSQL;
        end;
    except
      FParsedSQL := '';
      Params.Clear;
      InternalClose(FOnError, False, True);
      raise;
    end;
  finally
    InternalClose(etmStayIn, True, True);
    TStringList(FSQL).OnChange := DoSQLChange;
  end;
end;

{ TUIBTransaction }

constructor TUIBTransaction.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FOptions       := [tpConcurrency,tpWait,tpWrite];
  FTrHandle      := nil;
  FDataBases     := TList.Create;
  FAutoStart     := True;
  FAutoStop      := True;
  FDefaultAction := etmCommit;
{$IFDEF FB20_UP}
  FLockTimeout   := 0;
{$ENDIF}
end;

destructor TUIBTransaction.Destroy;
begin
  ClearSQLComponents;
  Close(etmDefault, True);
  ClearDataBases;
  FDataBases.Free;
  inherited;
end;

{$IFNDEF UIB_NO_COMPONENT}
procedure TUIBTransaction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent is TUIBDataBase) and (Operation = opRemove)) then
    RemoveDatabase(TUIBDataBase(AComponent));
end;
{$ENDIF}

procedure TUIBTransaction.SetDataBase(const ADatabase: TUIBDataBase);
var
  i: Integer;
begin
  RemoveDatabase(FDataBase);
  AddDataBase(ADatabase);
  FDataBase := ADatabase;
  if (FDataBase <> nil) and (FSQLComponent <> nil) then
    for i := 0 to FSQLComponent.Count - 1 do
      TUIBStatement(FSQLComponent[i]).FParameter.CharacterSet := FDataBase.CharacterSet;
end;

procedure TUIBTransaction.Close(const Mode: TEndTransMode; Auto: boolean);
var
  i: Integer;
begin
  if FSQLComponent <> nil then
    for i := 0 to FSQLComponent.Count -1 do
      TUIBQuery(FSQLComponent.Items[i]).InternalTryCache(etmStayIn, Auto);
  EndTransaction(Mode, nil, Auto);
end;

function TUIBTransaction.GetStatements(const Index: Integer): TUIBStatement;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Items[Index] else
    raise EListError.CreateFmt(EUIB_INDEXERROR,[Index]);
end;

function TUIBTransaction.GetStatementsCount: Integer;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Count else
    Result := 0;
end;

procedure TUIBTransaction.ClearDataBases;
var i: Integer;
begin
  FDataBase := nil;
  for i := 0 to FDataBases.Count - 1 do
    TUIBDataBase(FDataBases[i]).RemoveTransaction(Self);
  FDataBases.Clear;
end;

function TUIBTransaction.GetDatabases(const Index: Integer): TUIBDataBase;
begin
  Result := FDataBases[Index];
end;

function TUIBTransaction.GetDatabasesCount: Integer;
begin
  Result := FDataBases.Count;
end;

procedure TUIBTransaction.BeginDataBase;
var i: Integer;
begin
  if (FDataBase = nil) then
    raise Exception.Create(EUIB_DATABASENOTDEF);
  for i := 0 to FDataBases.Count - 1 do
    TUIBDataBase(FDataBases[i]).Connected := True;
end;

procedure TUIBTransaction.BeginTransaction(Auto: boolean = True);
type
  TEBDynArray = array of TISCTEB;
var
  Buffer: Pointer;
  i: Integer;
  ATPB: AnsiString;
begin
  BeginDataBase;
  with FDataBase.FLibrary do
  if (FTrHandle = nil) then
  begin
    If Auto and (not FAutoStart) then
      raise EUIBException.Create(EUIB_EXPLICITTRANS);

    if FDataBases.Count = 1 then
    begin
      TransactionStart(FTrHandle, FDataBase.FDbHandle, CreateTRParams(FOptions, FLockRead, FLockWrite{$IFDEF FB20_UP}, FLockTimeout{$ENDIF}));
    end else
    begin
      GetMem(Buffer,  SizeOf(TISCTEB) * FDataBases.Count);
      try
        ATPB := CreateTRParams(FOptions, FLockRead, FLockWrite);
        for i := 0 to FDataBases.Count - 1 do
          with TEBDynArray(Buffer)[i] do
          begin
            Handle  := @TUIBDatabase(FDataBases[i]).FDbHandle;
            Len     := Length(ATPB);
            Address := PAnsiChar(ATPB);
          end;
        TransactionStartMultiple(FTrHandle, FDataBases.Count, Buffer);
      finally
        FreeMem(Buffer);
      end;
    end;
    if Assigned(FOnStartTransaction) then
      FOnStartTransaction(Self);
  end;
end;

function TUIBTransaction.EndTransaction(ETM: TEndTransMode; From: TUIBStatement;
  Auto: boolean): boolean;
var i: Integer;
begin
  Result := False;
  // don't lock if it is not necessary
  if (ETM = etmStayIn) then Exit;
  // Default Action
  if (ETM = etmDefault) then ETM := FDefaultAction;
  if (FTrHandle <> nil) then
    with FDataBase.FLibrary do
    try
      if Assigned(FOnEndTransaction) then
        FOnEndTransaction(Self, ETM);
      if (ETM in [etmCommit, etmRollback]) then
      begin
        if FSQLComponent <> nil then
          for i := 0 to FSQLComponent.Count -1 do
            if (From <> FSQLComponent.Items[i]) then
              TUIBQuery(FSQLComponent.Items[i]).InternalTryCache(etmStayIn, Auto);
      end;

      Assert( FAutoStop or (not Auto), EUIB_NOAUTOSTOP);

      case ETM of
        etmCommit            :
          begin
            TransactionCommit(FTrHandle);
            Result := True;
          end;
        etmCommitRetaining   : TransactionCommitRetaining(FTrHandle);
        etmRollback          :
          begin
            TransactionRollback(FTrHandle);
            Result := True;
          end;
        etmRollbackRetaining : TransactionRollbackRetaining(FTrHandle);
      end;
    except
      case ETM of
        etmCommit, etmRollback :
          TransactionRollback(FTrHandle);
        etmCommitRetaining, etmRollbackRetaining :
          TransactionRollbackRetaining(FTrHandle);
      end;
      raise;
    end;
end;

procedure TUIBTransaction.AddSQLComponent(Component: TUIBStatement);
begin
  if (FSQLComponent = nil) then
    FSQLComponent := TList.Create;
  FSQLComponent.Add(Component);
end;

procedure TUIBTransaction.ClearSQLComponents;
begin
  while (FSQLComponent <> nil) do
    TUIBQuery(FSQLComponent.Last).SetTransaction(nil);
end;

procedure TUIBTransaction.RemoveSQLComponent(Component: TUIBStatement);
begin
  if (FSQLComponent <> nil) then
  begin
    FSQLComponent.Remove(Component);
    if (FSQLComponent.Count = 0) then
    begin
      FSQLComponent.free;
      FSQLComponent := nil;
    end;
  end;
end;

procedure TUIBTransaction.AddDataBase(ADataBase: TUIBDataBase);
var i: Integer;
begin
  if (ADataBase <> nil) then
  begin
    for i := 0 to FDataBases.Count - 1 do
      if FDataBases[i] = ADataBase then
        Exit;
    Close(etmDefault, True);
    FDataBases.Add(ADataBase);
    ADataBase.AddTransaction(Self);
  end;
end;

procedure TUIBTransaction.RemoveDatabase(ADataBase: TUIBDataBase);
var
  i: Integer;
begin
  if (ADataBase <> nil) then
  begin
    for i := 0 to FDataBases.Count - 1 do
      if FDataBases[i] = ADataBase then
      begin
        Close(etmDefault, True);
        ADataBase.RemoveTransaction(Self);
        FDataBases.Delete(i);
        Exit;
      end;
    if ADataBase = FDataBase then
      FDataBase := nil;
  end;
end;

procedure TUIBTransaction.RemoveDatabase(Index: Integer);
begin
  with TUIBDataBase(FDataBases[Index]) do
  begin
    Close(etmDefault, True);
    RemoveTransaction(Self);
    FDataBases.Delete(Index);
  end;
end;

procedure TUIBTransaction.Commit;
begin
  EndTransaction(etmCommit, nil, False);
end;

procedure TUIBTransaction.CommitRetaining;
begin
  EndTransaction(etmCommitRetaining, nil, False);
end;

procedure TUIBTransaction.RollBack;
begin
  EndTransaction(etmRollback, nil, False);
end;

procedure TUIBTransaction.RollBackRetaining;
begin
  EndTransaction(etmRollbackRetaining, nil, False);
end;

{$IFDEF IB71_UP}
procedure TUIBTransaction.SavepointRelease(const Name: string);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointRelease(FTrHandle, Name);
end;

procedure TUIBTransaction.SavepointRollback(const Name: string; Option: Word = 0);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointRollback(FTrHandle, Name, Option);
end;

procedure TUIBTransaction.SavepointStart(const Name: string);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointStart(FTrHandle, Name);
end;
{$ENDIF}

function TUIBTransaction.GetInTransaction: boolean;
begin
  Result := (FTrHandle <> nil);
end;

function TUIBTransaction.GetOptions: TTransParams;
begin
  Result := FOptions;
end;

procedure TUIBTransaction.SetOptions(const Value: TTransParams);
begin
  FOptions := Value;
end;

function TUIBTransaction.GetLockRead: string;
begin
  Result := FLockRead;
end;

function TUIBTransaction.GetLockWrite: string;
begin
  Result := FLockWrite;
end;

procedure TUIBTransaction.SetLockRead(const Value: string);
begin
  FLockRead := Value;
end;

procedure TUIBTransaction.SetLockWrite(const Value: string);
begin
  FLockWrite := Value;
end;

function TUIBTransaction.GetDataBase: TUIBDataBase;
begin
  Result := FDataBase;
end;

procedure TUIBTransaction.StartTransaction;
begin
  BeginTransaction(False);
end;

procedure TUIBTransaction.SetDefaultAction(const Value: TEndTransMode);
begin
  Assert(Value in [etmCommit, etmRollBack], 'Commit or Rollback only.');
  FDefaultAction := Value;
end;

procedure TUIBTransaction.ExecuteImmediate(const sql: string);
begin
  BeginTransaction;
  with FDataBase, FLibrary do
{$IFDEF UNICODE}
    DSQLExecuteImmediate(FDataBase.FDbHandle,
      FTrHandle, MBUEncode(sql, CharacterSetCP[CharacterSet]), GetSQLDialect);
{$ELSE}
    DSQLExecuteImmediate(FDataBase.FDbHandle,
      FTrHandle, sql, GetSQLDialect);
{$ENDIF}
end;

function TUIBTransaction.GetTransactionID: Cardinal;
begin
  if InTransaction then
    Result := GetDataBase.FLibrary.TransactionGetId(FTrHandle) else
    Result := 0;
end;

{ TUIBService }

procedure TUIBService.BeginService;
var
  SPB: AnsiString;

  procedure AddString(id: AnsiChar; const Value: AnsiString);
  begin
    if (Value <> '') then
      SPB := SPB + id + AnsiChar(length(Value)) + Value;
  end;
begin
  SPB := isc_spb_version + isc_spb_current_version;
  AddString(isc_spb_user_name, AnsiString(FUserName));
  AddString(isc_spb_password, AnsiString(FPassWord));
  FLibrary.Load(FLiBraryName);
  case FProtocol of
    proLocalHost : FLibrary.ServiceAttach('service_mgr', FHandle, SPB);
    proTCPIP     : FLibrary.ServiceAttach(AnsiString(Fhost) + ':service_mgr', FHandle, SPB);
    proNetBEUI   : FLibrary.ServiceAttach('\\' + AnsiString(Fhost) + '\service_mgr', FHandle, SPB);
  end;
end;

constructor TUIBService.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FLibrary := TUIBLibrary.Create;
  FLibraryName := GetClientLibrary;
  FProtocol := proLocalHost;
  FHandle := nil;
end;

destructor TUIBService.Destroy;
begin
  inherited;
  FLibrary.Free;
end;

procedure TUIBService.SetLibraryName(const Lib: String);
begin
  FLibrary.UnLoad;
  FLiBraryName := Lib;
end;

procedure TUIBService.EndService;
begin
  FLibrary.ServiceDetach(FHandle);
end;

function TUIBService.CreateParam(code: AnsiChar;
  const Value: RawByteString): RawByteString;
var Len: Word;
begin
  Len := Length(Value);
  if len > 0 then
    Result := code + PAnsiChar(@Len)[0] + PAnsiChar(@Len)[1] + Value else
    result := '';
end;

function TUIBService.CreateParam(code: AnsiChar; Value: Integer): RawByteString;
begin
  result := code + PAnsiChar(@Value)[0] + PAnsiChar(@Value)[1] + PAnsiChar(@Value)[2] + PAnsiChar(@Value)[3];
end;

{ TUIBBackupRestore }

constructor TUIBBackupRestore.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FBackupFiles := TStringList.Create;
  FVerbose := false;
end;

destructor TUIBBackupRestore.Destroy;
begin
  FBackupFiles.Free;
  inherited;
end;

procedure TUIBBackupRestore.SetBackupFiles(const Value: TStrings);
begin
  FBackupFiles.Assign(Value);
end;

procedure TUIBBackupRestore.Run;
var
  Buffer: RawByteString;
  Len: Word;
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, CreateStartSPB);
    if FVerbose then
    begin
      SetLength(Buffer, 1024);
      while true do
      begin
        FLibrary.ServiceQuery(FHandle, '', isc_info_svc_line, Buffer);
        if (Buffer[1] <> isc_info_svc_line) then
          raise Exception.Create(EUIB_UNEXPECTEDERROR);
        Len := PWord(@Buffer[2])^;
        if (len > 0)  then
        begin
          if Assigned(FOnVerbose) then
            FOnVerbose(self, string(copy(Buffer, 4, len)));
        end else
          Break;
      end;
    end;
  finally
    EndService;
  end;
end;

{ TUIBBackup }

function TUIBBackup.CreateStartSPB: RawByteString;
var
  Len: Word;
  i: Integer;
  FileName: AnsiString;
  FileLength: Integer;
  function GetValue(Index: Integer): string;
  begin
    if Index >= 0 then
      Result := Copy(FBackupFiles.Strings[Index], Length(FBackupFiles.Names[Index]) + 2, MaxInt) else
      Result := '';
  end;
begin
  // backup service   ibservices
  Result := isc_action_svc_backup;

  // DB Name
  Result := Result + isc_spb_dbname;
  Len := Length(FDatabase);
  Result := Result + PAnsiChar(@Len)[0] + PAnsiChar(@Len)[1];
  Result := Result + AnsiString(FDatabase);

  for i := 0 to FBackupFiles.Count - 1 do
  begin
    FileName := AnsiString(FBackupFiles.Names[i]);
    if FileName = '' then
      FileName := AnsiString(FBackupFiles[i]);
    if FileName <> '' then
    begin
      // Backup file
      Result := Result + isc_spb_bkp_file;
      Len := Length(FileName);
      Result := Result + PAnsiChar(@Len)[0] + PAnsiChar(@Len)[1];
      Result := Result + FileName;
      // Backup file length
      if TryStrToInt(GetValue(i), FileLength) then
      begin
        Result := Result + isc_spb_bkp_length;
        Result := Result + PAnsiChar(@FileLength)[0] + PAnsiChar(@FileLength)[1] +
          PAnsiChar(@FileLength)[2] + PAnsiChar(@FileLength)[3];
      end;
    end;
  end;

  if FVerbose then
    Result := Result + isc_spb_verbose;

  if (FOptions <> []) then
    Result := Result + isc_spb_options + PAnsiChar(@FOptions)^ + #0#0#0;
end;

{ TUIBRestore }

constructor TUIBRestore.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FOptions := [roCreateNewDB];
  FPageSize := 0;
end;

function TUIBRestore.CreateStartSPB: RawByteString;
var
  Len: Word;
  i: Integer;
  FileName: AnsiString;
  Opts: Cardinal;
begin
  // backup service   ibservices
  Result := isc_action_svc_restore;

  for i := 0 to FBackupFiles.Count - 1 do
  begin
    FileName := AnsiString(FBackupFiles[i]);
    if FileName <> '' then
    begin
      // Backup file
      Result := Result + isc_spb_bkp_file;
      Len := Length(FileName);
      Result := Result + PAnsiChar(@Len)[0] + PAnsiChar(@Len)[1];
      Result := Result + FileName;
    end;
  end;

  // DB Name
  Result := Result + isc_spb_dbname;
  Len := Length(FDatabase);
  Result := Result + PAnsiChar(@Len)[0] + PAnsiChar(@Len)[1];
  Result := Result + AnsiString(FDatabase);

  if FVerbose then
    Result := Result + isc_spb_verbose;

  if (FOptions <> []) then
  begin
    Opts := PByte(@FOptions)^ shl 8;
    Result := Result + isc_spb_options + PAnsiChar(@Opts)[0] +
      PAnsiChar(@Opts)[1] + PAnsiChar(@Opts)[2] + PAnsiChar(@Opts)[3];
  end;

  if FPageSize > 0 then
    Result := Result + isc_spb_res_page_size + PAnsiChar(@FPageSize)[0] +
      PAnsiChar(@FPageSize)[1] + PAnsiChar(@FPageSize)[2] + PAnsiChar(@FPageSize)[3];
end;

{ TUIBSecurity }

constructor TUIBSecurity.Create{$IFNDEF UIB_NO_COMPONENT}(aOwner: TComponent){$ENDIF};
begin
  inherited;
  FUserInfos := TObjectList.Create(True);
end;

destructor TUIBSecurity.Destroy;
begin
  inherited;
  FUserInfos.Free;
end;

procedure TUIBSecurity.AddUser;
begin
  RunAction(saAddUser);
end;

procedure TUIBSecurity.DeleteUser;
begin
  RunAction(saDeleteUser);
end;

procedure TUIBSecurity.ModifyUser;
begin
  RunAction(saModifyUser);
end;

procedure TUIBSecurity.DisplayUser;
begin
  RunAction(saDisplayUser);
end;

procedure TUIBSecurity.DisplayUsers;
begin
  RunAction(saDisplayUsers);
end;

procedure TUIBSecurity.ClearParams;
var
  P : Integer;
begin
  for P := Low(FIntegerParams) to High(FIntegerParams) do
    FIntegerParams[P] := 0;
  for P := Low(FStringParams) to High(FStringParams) do
    FStringParams[P] := '';
  FModifiedParams := [];
end;

function TUIBSecurity.GetUserInfoCount: Integer;
begin
  Result := FUserInfos.Count;
end;

function TUIBSecurity.GetUserInfo(aIndex: Integer): TUserInfo;
begin
  Result := TUserInfo(FUserInfos[aIndex]);
end;

{$IFDEF FB25_UP}
function TUIBSecurity.GetBooleanParam(aParam: Integer): Boolean;
begin
  Result := FIntegerParams[aParam] <> 0;
end;
{$ENDIF}

function TUIBSecurity.GetIntegerParam(aParam: Integer): Integer;
begin
  Result := FIntegerParams[aParam];
end;

function TUIBSecurity.GetStringParam(aParam: Integer): string;
begin
  Result := FStringParams[aParam];
end;

{$IFDEF FB25_UP}
procedure TUIBSecurity.SetBooleanParam(aParam: Integer; const aValue: Boolean);
begin
  FIntegerParams[aParam] := ord(aValue);
  Include(FModifiedParams, TSecurityParam(aParam));
end;
{$ENDIF}

procedure TUIBSecurity.SetIntegerParam(aParam: Integer; const aValue: Integer);
begin
  FIntegerParams[aParam] := aValue;
  Include(FModifiedParams, TSecurityParam(aParam));
end;

procedure TUIBSecurity.SetStringParam(aParam: Integer; const aValue: string);
begin
  FStringParams[aParam] := aValue;
  Include(FModifiedParams, TSecurityParam(aParam));
end;

procedure TUIBSecurity.RunAction(aAction: TSecurityAction);
const
  IscActions : array[TSecurityAction] of AnsiChar = (
    isc_action_svc_add_user, isc_action_svc_delete_user, isc_action_svc_modify_user,
    isc_action_svc_display_user, isc_action_svc_display_user);
  IscParams : array[TSecurityParam] of AnsiChar = (
    isc_spb_sql_role_name, isc_spb_sec_username, isc_spb_sec_password,
    isc_spb_sec_firstname, isc_spb_sec_middlename, isc_spb_sec_lastname,
    isc_spb_sec_userid, isc_spb_sec_groupid {$IFDEF FB25_UP},isc_spb_sec_admin{$ENDIF});
var
  StartParams, Buffer: RawByteString;
  Position: Integer;

  procedure AddStringParam(P: TSecurityParam);
  var
    Value : AnsiString;
    Len : UShort;
  begin
    Value := AnsiString(GetStringParam(ord(P)));
    Len := Length(Value);
    StartParams := StartParams + IscParams[P] + PAnsiChar(@Len)[0] + PAnsiChar(@Len)[1] + Value;
  end;

  procedure AddIntegerParam(P: TSecurityParam);
  var
    Value : Integer;
  begin
    Value := GetIntegerParam(ord(P));
    StartParams := StartParams + IscParams[P] + PAnsiChar(@Value)[0] + PAnsiChar(@Value)[1] +
                                                PAnsiChar(@Value)[2] + PAnsiChar(@Value)[3];
  end;

  procedure ParseCode(Code: AnsiChar);
  begin
    if (Buffer[Position] <> Code) then
      raise Exception.Create(EUIB_SERVICESPARSING);
    Inc(Position);
  end;

  function ParseString(P: TSecurityParam): string;
  var
    Len: UShort;
  begin
    ParseCode(IscParams[P]);
    Len := PWord(@Buffer[Position])^;
    Position := Position + 2;
    SetString(Result, PAnsiChar(@Buffer[Position]), Len);
    Position := Position + Len;
  end;

  function ParseInteger(P: TSecurityParam): Integer;
  begin
    ParseCode(IscParams[P]);
    Result := PInteger(@Buffer[Position])^;
    Position := Position + 4;
  end;

var
  P: TSecurityParam;
  S: string;
  U: TUserInfo;
begin
  StartParams := IscActions[aAction];

  if spRole in FModifiedParams then
    AddStringParam(spRole);

  if aAction <> saDisplayUsers then
  begin { actions, other than saDisplayUsers, require a valid user name }
    S := UserName;
    if (S = '') or (Pos(' ', S) > 0) then
      raise Exception.CreateFmt(EUIB_INVALIDUSERNAME, [S]);
    AddStringParam(spUser);
  end;

  if aAction in [saAddUser, saModifyUser] then
  { only saAddUser and saModifyUser actions require these,
    and only when explicitly set }
  begin
    for P := spPass to spLastName do
      if P in FModifiedParams then
        AddStringParam(P);
    for P := spUserID to spGroupID do
      if P in FModifiedParams then
        AddIntegerParam(P);
  end;

  BeginService;
  try
    FLibrary.ServiceStart(FHandle, StartParams);
    if aAction in [saDisplayUser, saDisplayUsers] then
    begin
      SetLength(Buffer, 32000);
      FLibrary.ServiceQuery(FHandle, AnsiString(''), AnsiChar(isc_info_svc_get_users), Buffer);
      Position := 1;
      ParseCode(isc_info_svc_get_users);
      FUserInfos.Clear;
      Inc(Position, 2); // skip combined length info
      while Buffer[Position] <> AnsiChar(isc_info_end) do
      begin
        U := TUserInfo.Create;
        with U do
        try
          UserName := ParseString(spUser);
          FirstName := ParseString(spFirstName);
          MiddleName := ParseString(spMiddleName);
          LastName := ParseString(spLastName);
          UserID := ParseInteger(spUserID);
          GroupID := ParseInteger(spGroupID);
          FUserInfos.Add(U);
        except
          U.Free;
          raise;
        end;
      end;
    end;
  finally
    EndService;
  end;
  ClearParams;
end;

{ TUIBScript }

constructor TUIBScript.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FQuery := TUIBQuery.Create{$IFNDEF UIB_NO_COMPONENT}(nil){$ENDIF};
  FQuery.ParseParams := False;
  FScript := TStringList.Create;
  FAutoDDL := True;
end;

destructor TUIBScript.Destroy;
begin
  FQuery.Free;
  FScript.Free;
  inherited;
end;

procedure TUIBScript.ExecuteScript;
var
  Parser: TUIBSQLParser;
  st: TSQLStatement;
  i: Integer;
  j: TCharacterSet;
  Dialect: Integer;
  TrHandle: IscTrHandle;
  str: String;
  Found: boolean;
  Handled: boolean;

  procedure CheckDatabase;
  begin
    if (Transaction = nil) then
      raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
  end;

  procedure TryExecute;
  begin
    try
      if FQuery.Params.ParamCount > 0 then
        FQuery.Prepare(true) else
        FQuery.ExecSQL;
    except
      on E: Exception do
      begin
        if Assigned(FOnExecuteError) then
        begin
          Handled := False;
          FOnExecuteError(Self, E, FQuery.SQL.Text, Handled);
          if not Handled then
            raise;
        end else
          raise;
      end;
    end;
  end;

begin
  Parser := TUIBSQLParser.Create(FScript);
  Parser.OnComment := FOnComment;
  try
    while true do
    begin
      st := Parser.NextStatement;
      if st = ssEOF then
        Break;
      if Assigned(FOnParse) then
        FOnParse(self, st, Parser.Statement);
      case st of
        ssSetSqlDialect:
          begin
            CheckDatabase;
            if TryStrToInt(Parser.Params.Values['DIALECT'], Dialect) then
              FQuery.FindDataBase.SQLDialect := Dialect else
              raise Exception.Create(EUIB_PARSESQLDIALECT);
          end;
        ssSetNames:
          begin
            CheckDatabase;
            str := Parser.Params.Values['CHARACTER'];
            Found := false;
            for j := low(TCharacterSet) to high(TCharacterSet) do
              if (CompareText(string(CharacterSetStr[j]), str) = 0) then
              begin
                FQuery.FindDataBase.CharacterSet := j;
                found := true;
                Break;
              end;
            if not found then
              raise Exception.Create(EUIB_PARSESETNAMES);
          end;
        ssCreateDatabase:
          begin
            CheckDatabase;
            FQuery.FindDataBase.Connected := False;
            TrHandle := nil;
            with FQuery.FindDataBase do
            begin
              FLibrary.Load(FLiBraryName);
              // I MUST provide the real DB Handle (not nil)
              // because altering forein key can fail otherwise.
{$IFDEF UNICODE}
              FLibrary.DSQLExecuteImmediate(
                FDbHandle, TrHandle, MBUEncode(Parser.Statement, CharacterSetCP[CharacterSet]), SQLDialect);
{$ELSE}
              FLibrary.DSQLExecuteImmediate(
                FDbHandle, TrHandle, Parser.Statement, SQLDialect);
{$ENDIF}
            end;
            FQuery.FindDataBase.DatabaseName := Parser.Params.Values['DATABASE'];
            FQuery.FindDataBase.UserName := Parser.Params.Values['USER'];
            FQuery.FindDataBase.PassWord := Parser.Params.Values['PASSWORD'];
          end;
        ssConnect:
          with FQuery.FindDataBase do
          begin
            Connected := False;
            DatabaseName := Parser.Params.Values['DATABASE'];
            UserName     := Parser.Params.Values['USER'];
            PassWord     := Parser.Params.Values['PASSWORD'];
            Connected := True;
          end;
        ssAutoDDL:
          begin
            if (Parser.Params.Values['AUTODDL'] = 'ON') then
              FAutoDDL := True else
              FAutoDDL := False;
          end;
        ssCommit:
          begin
            if FQuery.CurrentState > qsPrepare then
              Transaction.CommitRetaining else
              Transaction.Commit;
          end;
        ssRollback:
          begin
            if FQuery.CurrentState > qsPrepare then
              Transaction.RollBackRetaining else
              Transaction.Rollback;
          end;
      {$IFDEF IB71_UP}
        ssSetSavepoint:
          Transaction.SavepointStart(Parser.Params.Values['SYMBOL']);
        ssReleaseSavepoint:
          Transaction.SavepointRelease(Parser.Params.Values['SYMBOL']);
        ssUndoSavepoint:
          Transaction.SavepointRollback(Parser.Params.Values['SYMBOL']);
      {$ENDIF}
        ssSelect, // perhaps a select statement execute a procedure ...
        ssInsertInto,
        ssDelete,
        ssUpdate:
          begin
            FQuery.SQL.Text := trim(Parser.Statement);
            TryExecute;
            if FQuery.Params.ParamCount = 0 then
              FQuery.Close(etmStayIn);
          end;
        ssBulkParams:
          begin
            for i := 0 to Parser.Params.Count - 1 do
            begin
              str := Parser.Params[i];
              case TSQLToken(PtrInt(Parser.Params.Objects[i])) of
                toValString:
                  begin
                    if FQuery.Params.IsBlob[i] then
                      FQuery.ParamsSetBlob(i, str) else
                      FQuery.Params.AsString[i] := str;
                  end;
                toValNumber: FQuery.Params.AsInt64[i] := StrToInt64(str);
                toValFloat : FQuery.Params.AsDouble[i] := StrToFloat(str);
                toNULL     : FQuery.Params.IsNull[i] := true;
              else
                raise Exception.Create(EUIB_UNEXPECTEDERROR);
              end;
            end;

            FQuery.Execute;
          end
      else
        // DDL ...
        FQuery.SQL.Text := trim(Parser.Statement);
        TryExecute;
        //if not FBulk then
          if FAutoDDL then
            FQuery.Close(etmCommit) else
            FQuery.Close(etmStayIn);
      end;
    end;
  finally
    FQuery.Close(etmStayIn);
    Parser.Free;
  end;
end;

function TUIBScript.GetDatabase: TUIBDataBase;
begin
  Result := FQuery.DataBase;
end;

function TUIBScript.GetOnError: TEndTransMode;
begin
  Result := FQuery.OnError;
end;

function TUIBScript.GetTransaction: TUIBTransaction;
begin
  Result := FQuery.Transaction;
end;

procedure TUIBScript.SetDatabase(const Value: TUIBDataBase);
begin
  FQuery.DataBase := Value;
end;

procedure TUIBScript.SetOnError(const Value: TEndTransMode);
begin
  FQuery.OnError := Value;
end;

procedure TUIBScript.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

procedure TUIBScript.SetTransaction(const Value: TUIBTransaction);
begin
  FQuery.Transaction := Value;
end;

{ TMetaDataOptions }

constructor TMetaDataOptions.Create;
begin
  inherited;
  FObjects := ALLOBjects;
  FTables := ALLTables;
  FViews := ALLViews;
  FProcedures := ALLProcedures;
  FUDFs := ALLUDFs;
  FRoles := ALLRoles;
  FSysInfos := False;
end;

{ TUIBRepair }

constructor TUIBRepair.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FVerbose := false;
end;

function TUIBRepair.CreateStartSPB: RawByteString;
var
  Len: Word;
  Param: byte;
begin
  result := isc_action_svc_repair;

  // DB Name
  Result := Result + isc_spb_dbname;
  Len := Length(FDatabase);
  Result := Result + PAnsiChar(@Len)[0] + PAnsiChar(@Len)[1];
  Result := Result + AnsiString(FDatabase);

  if (roSweepDB in FOptions) then
    Param := isc_spb_rpr_sweep_db else
    Param := 0;
  if (roValidateDB in FOptions) then
    Param := Param or isc_spb_rpr_validate_db;

  if (Param <> 0) then
    Result := Result + isc_spb_options + AnsiChar(Param) + #0#0#0;

  if (roListLimboTrans in FOptions) then
    Param := isc_spb_rpr_list_limbo_trans else
    Param := 0;
  if (roCheckDB in FOptions) then
    Param := Param or isc_spb_rpr_check_db;
  if (roIgnoreChecksum in FOptions) then
    Param := Param or isc_spb_rpr_ignore_checksum;
  if (roKillShadows in FOptions) then
    Param := Param or isc_spb_rpr_kill_shadows;
  if (roMendDB in FOptions) then
    Param := Param or isc_spb_rpr_mend_db;
  if (roValidateFull in FOptions) then
  begin
    Param := Param or isc_spb_rpr_full;
     if not (roMendDB in FOptions) then
       Param := Param or isc_spb_rpr_validate_db;
  end;
  if (Param <> 0) then
    Result := Result + isc_spb_options + AnsiChar(Param) + #0#0#0;
end;

procedure TUIBRepair.Run;
var
  Buffer: RawByteString;
  Len: Word;
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, CreateStartSPB);
    if FVerbose then
    begin
      SetLength(Buffer, 1024);
      while true do
      begin
        FLibrary.ServiceQuery(FHandle, '', isc_info_svc_line, Buffer);
        if (Buffer[1] <> isc_info_svc_line) then
          raise Exception.Create(EUIB_UNEXPECTEDERROR);
        Len := PWord(@Buffer[2])^;
        if (len > 0)  then
        begin
          if Assigned(FOnVerbose) then
            FOnVerbose(self, string(copy(Buffer, 4, len)));
        end else
          Break;
      end;
    end;
  finally
    EndService;
  end;
end;

{ TUIBEvents }

constructor TUIBEvents.Create{$IFNDEF UIB_NO_COMPONENT}(AOwner: TComponent){$ENDIF};
begin
  inherited;
  FSyncMainThread := true;
  FThreadException := False;
  FDatabase := nil;
  FAutoRegister := False;
  FEvents := TStringList.Create;
  FThreads := TList.Create;
end;

destructor TUIBEvents.Destroy;
begin
  Registered := false;
  Database := nil;
  FThreads.Free;
  FEvents.Free;
  inherited Destroy;
end;

{$IFNDEF UIB_NO_COMPONENT}
procedure TUIBEvents.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
  begin
    Registered := false;
    FDatabase := nil;
  end;
end;
{$ENDIF}

procedure TUIBEvents.SetEvents(value: TStrings);
begin
  FEvents.Assign(value);
end;

procedure TUIBEvents.SetDatabase(value: TUIBDataBase);
var
  WasRegistered: boolean;
begin
  if (Value <> FDatabase) then
  begin
{$IFNDEF UIB_NO_COMPONENT}
    if (csDesigning in ComponentState) then
      FDatabase := Value else
{$ENDIF}
    begin
      WasRegistered := Registered;
      if WasRegistered then
        Registered := false;
      try
        if Assigned(FDatabase) then
          FDatabase.RemoveEventNotifier(Self);
        FDatabase := Value;
        if Assigned(FDatabase) then
          FDatabase.AddEventNotifier(Self);
      finally
        if WasRegistered and Assigned(FDatabase) then
          Registered := true;
      end;
    end;
  end;
end;

procedure TUIBEvents.SetRegistered(const Value : boolean);

  procedure RegisterEvents;
  var
    i: Integer;
  begin
    if (FThreads.Count = 0) then
    begin
      if (FEvents.Count > 0) then
      begin
        for i := 0 to ((FEvents.Count - 1) div 15) do
          FThreads.Add(TUIBEventThread.Create(Self, i, FSyncMainThread));
      end;
    end;
  end;

  procedure UnregisterEvents;
  var
    i: Integer;
  begin
    for i := FThreads.Count - 1 downto 0 do
      with TUIBEventThread(FThreads[i]) do
      begin
        FThreads.Delete(i);
        if not Terminated then
          free;
      end;
  end;

begin
  if FRegistered = Value then Exit;

  FRegistered := Value;
{$IFNDEF UIB_NO_COMPONENT}
  if (csDesigning in ComponentState) then
    Exit;
{$ENDIF}
  if (Value) then
  begin
    if FDatabase <> nil then
    begin
      FDatabase.Connected := true;
      RegisterEvents
    end;
  end else
    UnRegisterEvents;
end;

procedure TUIBEvents.SetAutoRegister(const Value: boolean);
begin
  if FAutoRegister <> Value then
  begin
    FAutoRegister := Value;
    if FAutoRegister and (not Registered) and
       Assigned(FDatabase) and FDatabase.Connected then
      Registered := true;
  end;
end;

{ TUIBEventThread }

function TUIBEventThread.FindDataBase: TUIBDataBase;
begin
  if (FOwner <> nil) and (FOwner.Database <> nil) then
    result := FOwner.Database else
    raise Exception.Create(EUIB_DATABASENOTDEF);
end;

procedure EventCallback(UserData: Pointer; Length: Smallint; Updated: PAnsiChar); cdecl;
begin
  if (Length > 0) and (Updated <> nil) then
  if (Assigned(UserData) and Assigned(Updated)) then
  with TUIBEventThread(UserData) do
  begin
    Move(Updated^, FResultBuffer^, Length);
    FQueueEvent := True;
    FSignal.SetEvent;
  end;
end;

procedure TUIBEventThread.SyncOnEvent;
begin
  FOwner.FOnEvent(FOwner, FOwner.FEvents[((FBlock * 15) + FCurrentEvent)],
    FStatusVector[FCurrentEvent], FCancelAlerts)
end;

procedure TUIBEventThread.SyncHandleException;
begin
  ShowException(FExceptObject, FExceptAddr);
end;

function TUIBEventThread.HandleException: boolean;
begin
  if (not FOwner.FThreadException) then
  begin
    Result := True;
    FOwner.FThreadException := True;
    FExceptObject := ExceptObject;
    FExceptAddr := ExceptAddr;
    try
      if not (FExceptObject is EAbort) then
        if FSyncMainThread then
          Synchronize(SyncHandleException) else
          SyncHandleException;
    finally
      FExceptObject := nil;
      FExceptAddr := nil;
    end;
  end else
    Result := False;
end;

procedure TUIBEventThread.Execute;
var
  arr: array[0..14] of AnsiString;
  count, i: integer;
  first: boolean;
begin
  FEventBuffer := nil;
  FResultBuffer := nil;
  FEventBufferLen := 0;
  first := True;
  FQueueEvent := false;
  count := (FOwner.FEvents.Count - (FBlock * 15));
  if count > 15 then
    count := 15;
  for i := 0 to count - 1 do
    Arr[i] := AnsiString(FOwner.FEvents[i + FBlock * 15]);
  with FindDataBase.FLibrary do
    FEventBufferLen := EventBlock(FEventBuffer, FResultBuffer, count,
      PAnsiChar(Arr[0]), PAnsiChar(Arr[1]), PAnsiChar(Arr[2]), PAnsiChar(Arr[3]),
      PAnsiChar(Arr[4]), PAnsiChar(Arr[5]), PAnsiChar(Arr[6]), PAnsiChar(Arr[7]),
      PAnsiChar(Arr[8]), PAnsiChar(Arr[9]), PAnsiChar(Arr[10]), PAnsiChar(Arr[11]),
      PAnsiChar(Arr[12]), PAnsiChar(Arr[13]), PAnsiChar(Arr[14]));
  FSignal.ResetEvent;
  if FSyncMainThread then
    Synchronize(SyncEventQueue) else
    SyncEventQueue;
  try
    while not Terminated do
    begin
      FSignal.WaitFor(INFINITE);
      if Terminated then
        Break;
      if (FQueueEvent or first) then
      begin
        FindDataBase.FLibrary.EventCounts(FStatusVector, FEventBufferLen,
          FEventBuffer, FResultBuffer);
        if (Assigned(FOwner.FOnEvent) and (not first)) then
        begin
          FCancelAlerts := false;
          FCurrentEvent := 0;
          while (FCurrentEvent < count) do
          begin
            if (FStatusVector[FCurrentEvent] <> 0) then
              if FSyncMainThread then
                Synchronize(SyncOnEvent) else
                SyncOnEvent;
            inc(FCurrentEvent);
          end;
        end;
        first := False;
        FQueueEvent := False;
        FSignal.ResetEvent;
        if FSyncMainThread then
          Synchronize(SyncEventQueue) else
          SyncEventQueue;
      end;
    end;

    if FSyncMainThread then
      Synchronize(SyncEventCancel) else
      SyncEventCancel;

    with FindDataBase.FLibrary do
    begin
      IscFree(FEventBuffer);
      IscFree(FResultBuffer);
    end;

    ReturnValue := 0;
  except
    if HandleException then
      ReturnValue := 1 else
      ReturnValue := 0;
  end;
end;

constructor TUIBEventThread.Create(Owner: TUIBEvents;
  Block: Integer; SyncMainThread: boolean);
begin
  inherited Create(false);
  FSyncMainThread := SyncMainThread;
  FCurrentEvent := 0;
  FEventID := 0;
  FOwner := Owner;
  FExceptObject := nil;
  FExceptAddr := nil;
  FCancelAlerts := false;
  FSignal := TSimpleEvent.Create;
  FBlock := Block;
  OnTerminate := SyncTerminate;
end;

destructor TUIBEventThread.Destroy;
begin
  Terminate;
  FSignal.SetEvent;
  WaitFor;
  FSignal.Free;
  inherited Destroy;
end;

procedure TUIBEventThread.SyncEventCancel;
var
  db: TUIBDataBase;
begin
  try
    db := FindDataBase;
    with db, Flibrary do
      EventCancel(FDbHandle, FEventID);
  except
    on E : Exception do
      if Assigned(FOwner.FOnException) then
          FOwner.FOnException(E);
  end;
end;

procedure TUIBEventThread.SyncEventQueue;
var db: TUIBDataBase;
begin
  try
    db := FindDataBase;
    with db, FLibrary do
      EventQueue(FdbHandle, FEventID, FEventBufferLen, FEventBuffer,
        @EventCallback, self);
  except
    on E : Exception do
      if Assigned(FOwner.FOnException) then
          FOwner.FOnException(E);
  end;
end;

procedure TUIBEventThread.SyncTerminate(sender: TObject);
var
  ThreadIdx: Integer;
begin
  with FOwner, FThreads do
  begin
    ThreadIdx := IndexOf(self);
    if (ThreadIdx > -1) then
      Delete(ThreadIdx);
    if (ReturnValue = 1) then
    begin
      if Registered then
        Registered := false;
      FThreadException := False;
    end
  end;
end;

{ TUIBConfig }

procedure TUIBConfig.ActivateShadow;
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, AnsiString(FDatabaseName)) +
      CreateParam(isc_spb_options, isc_spb_prp_activate));
  finally
    EndService;
  end;
end;

procedure TUIBConfig.BringDatabaseOnline;
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, AnsiString(FDatabaseName)) +
      CreateParam(isc_spb_options, isc_spb_prp_db_online));
  finally
    EndService;
  end;
end;

procedure TUIBConfig.SetAsyncMode(Value: Boolean);
const
  AsyncMode: array[boolean] of AnsiChar =
    (isc_spb_prp_wm_sync, isc_spb_prp_wm_async);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, AnsiString(FDatabaseName)) +
      isc_spb_prp_write_mode + AsyncMode[Value]);
  finally
    EndService;
  end;
end;

procedure TUIBConfig.SetDBSqlDialect(Value: Integer);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, AnsiString(FDatabaseName)) +
      CreateParam(isc_spb_prp_set_sql_dialect, Value));
  finally
    EndService;
  end;
end;

procedure TUIBConfig.SetPageBuffers(Value: Integer);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, AnsiString(FDatabaseName)) +
      CreateParam(isc_spb_prp_page_buffers, Value));
  finally
    EndService;
  end;
end;

procedure TUIBConfig.SetReadOnly(Value: Boolean);
const
  ReadOnly: array[boolean] of AnsiChar =
    (isc_spb_prp_am_readwrite, isc_spb_prp_am_readonly);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, AnsiString(FDatabaseName)) +
      isc_spb_prp_access_mode + ReadOnly[Value]);
  finally
    EndService;
  end;
end;

procedure TUIBConfig.SetReserveSpace(Value: Boolean);
const
  ReserveSpace: array[boolean] of AnsiChar =
    (isc_spb_prp_res_use_full, isc_spb_prp_res);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, AnsiString(FDatabaseName)) +
      isc_spb_prp_reserve_space + ReserveSpace[Value]);
  finally
    EndService;
  end;
end;

procedure TUIBConfig.SetSweepInterval(Value: Integer);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, AnsiString(FDatabaseName)) +
      CreateParam(isc_spb_prp_sweep_interval, Value));
  finally
    EndService;
  end;
end;

procedure TUIBConfig.ShutdownDatabase(Options: TShutdownMode;
  Wait: Integer);
const
  ShutdownMode: array[TShutdownMode] of AnsiChar =
    (isc_spb_prp_shutdown_db, isc_spb_prp_deny_new_transactions, isc_spb_prp_deny_new_attachments);
begin
  BeginService;
  try
    FLibrary.ServiceStart(FHandle, isc_action_svc_properties +
      CreateParam(isc_spb_dbname, AnsiString(FDatabaseName)) +
      CreateParam(ShutdownMode[Options], Wait));
  finally
    EndService;
  end;
end;

{ TUIBServerInfo }

procedure TUIBServerInfo.GetServerInfo;
var
  Buffer: RawByteString;
  Code: Byte;
  Position: Integer;
  Value: Integer;
  DbName: AnsiString;

  function ParseByte: Byte;
  begin
    Result := PByte(@Buffer[Position])^;
    Inc(Position);
  end;

  function ParseWord: Word;
  begin
    Result := PWord(@Buffer[Position])^;
    Inc(Position, 2);
  end;

  function ParseInteger: Integer;
  begin
    Result := PInteger(@Buffer[Position])^;
    Inc(Position, 4);
  end;

  function ParseString: AnsiString;
  var
    Len: Word;
  begin
    Result := '';
    Len := ParseWord;
    SetLength(Result, Len);
    Move(Buffer[Position], Result[1], Len);
    Inc(Position, Len);
  end;

begin
  BeginService;
  try
    {
      Hope 2048 bytes is enought : 12 bytes are used to return number of
      attachments and databases header and footer of the data opacket.
      2036 bytes remains for databases names.
      Each is at least 3 + length_of_db_name bytes.
      Each db_name can be 2^16 bytes. I think an average value for db_name can
      be 50 bytes. So that Firebird Service API can return around 40 database
      names. Feel free to adjust according to you needs. You will get an
      EUIB_UNEXPECTEDERROR if the database names cannot fit into Buffer due to
      isc_info_truncated.
    }
    SetLength(Buffer,2048);
    FLibrary.ServiceQuery(FHandle, '', isc_info_svc_svr_db_info, Buffer);
  finally
    EndService;
  end;

  { Check header }
  if Buffer[1] <> isc_info_svc_svr_db_info then
    raise Exception.Create(EUIB_UNEXPECTEDERROR);

  { Parse response }
  Position := 2;
  while Buffer[Position] <> AnsiChar(isc_info_flag_end) do
  begin
    Code := ParseByte;
    case Code of
    isc_info_truncated:
      raise Exception.Create(EUIB_UNEXPECTEDERROR);
    isc_spb_num_att:
    begin
      Value := ParseInteger;
      if Assigned(FOnInfoAttachments) then
        FOnInfoAttachments(Self, Value);
    end;
    isc_spb_num_db:
    begin
      Value := ParseInteger;
      if Assigned(FOnInfoDatabases) then
        FOnInfoDatabases(Self, Value);
    end;
    Byte(isc_spb_dbname):
      begin
        DbName := ParseString;
        if Assigned(FOnInfoDbName) then
          FOnInfoDbName(Self, string(DbName));
      end;
    end;
  end;
end;

{$IFDEF DELPHI14_UP}
{ TUIBEnumerable<T> }

constructor TUIBEnumerable<T>.Create(AQuery: TUIBStatement; AFilter: TUIBMethodFilter<T>);
begin
  FQuery := AQuery;
  FFilter := AFilter;
end;

function TUIBEnumerable<T>.GetEnumerator: IUIBEnumerator<T>;
begin
  Result := TUIBEnumerator<T>.Create(FQuery, FFilter);
end;

{ TUIBEnumerator<T> }

constructor TUIBEnumerator<T>.Create(AQuery: TUIBStatement; AFilter: TUIBMethodFilter<T>);
begin
  FQuery := AQuery;
  FFilter := AFilter;
  FCtx := TRttiContext.Create;
  FCursor := -1;
  if (FQuery.CurrentState < qsExecute) or not FQuery.CachedFetch then
    FQuery.Open(True) else
    FQuery.First;
end;

destructor TUIBEnumerator<T>.Destroy;
begin
  FCtx.Free;
  inherited;
end;

function TUIBEnumerator<T>.GetCurrent: T;
begin
  Result := FCurrent;
end;

function TUIBEnumerator<T>.MoveNext: Boolean;
begin
  if FQuery.Eof then Exit(False);

  if FCursor <> -1 then
    FQuery.Next;
  if FQuery.Eof then Exit(False);
  Inc(FCursor);
  FCurrent := FQuery.Fields.GetAsType<T>(FCtx);
  if Assigned(FFilter) then
    while not FQuery.Eof do
    begin
      if FFilter(FCurrent) then
        Break;
      FQuery.Next;
      if FQuery.Eof then Exit(False);
      Inc(FCursor);
      FCurrent := FQuery.Fields.GetAsType<T>(FCtx);
    end;
  Result := not FQuery.Eof;
end;
{$ENDIF}

{$IFNDEF FPC}
initialization
  IsMultiThread := true;
{$ENDIF}

end.
