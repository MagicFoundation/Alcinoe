/// SQLite3 embedded Database engine used as the mORMot SQL kernel
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotSQLite3;

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
  - Ondrej
  - Mario Moretti

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
}

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 WITHLOG

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$ifdef FPC}
  SynFPCLinux,
  BaseUnix,
  {$endif}
  {$ifdef KYLIX3}
  Types,
  LibC,
  SynKylix,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  {$ifndef LVCL}
  SyncObjs, // for TCriticalSection inlining
  Contnrs,
  {$endif}
  SynZip,
  SynCommons,
  SynLog,
  SynSQLite3,
  SynTable,
  mORMot;

{.$define WITHUNSAFEBACKUP}
{ define this if you really need the old blocking TSQLRestServerDB backup methods
  - those methods are deprecated - you should use DB.BackupBackground() instead }


{ ****************** SQLite3 database used as kernel of our mORMot framework }

type
  /// Execute a SQL statement in the local SQLite3 database engine, and get
  // result in memory
  // - all DATA (even the BLOB fields) is converted into UTF-8 TEXT
  // - uses a TSQLTableJSON internaly: faster than sqlite3_get_table()
  // (less memory allocation/fragmentation) and allows efficient caching
  TSQLTableDB = class(TSQLTableJSON)
  private
  public
    /// Execute a SQL statement, and init TSQLTable fields
    // - FieldCount=0 if no result is returned
    // - the BLOB data is converted into TEXT: you have to retrieve it with
    //  a special request explicitely (note that JSON format returns BLOB data)
    // - uses a TSQLTableJSON internaly: all currency is transformed to its floating
    //   point TEXT representation, and allows efficient caching
    // - if the SQL statement is in the DB cache, it's retrieved from its cached
    //   value: our JSON parsing is a lot faster than SQLite3 engine itself,
    //   and uses less memory
    // - will raise an ESQLException on any error
    constructor Create(aDB: TSQLDatabase; const Tables: array of TSQLRecordClass;
      const aSQL: RawUTF8; Expand: boolean); reintroduce;
  end;

  /// class-reference type (metaclass) of a REST server using SQLite3 as main engine
  TSQLRestServerDBClass = class of TSQLRestServerDB;

  TSQLVirtualTableModuleServerDB = class;

  /// REST server with direct access to a SQLite3 database
  // - caching is handled at TSQLDatabase level
  // - SQL statements for record retrieval from ID are prepared for speed
  TSQLRestServerDB = class(TSQLRestServer)
  private
    /// access to the associated SQLite3 database engine
    fDB: TSQLDataBase;
    /// initialized by Create(aModel,aDBFileName)
    fOwnedDB: TSQLDataBase;
    /// prepared statements with parameters for faster SQLite3 execution
    // - used for SQL code with :(%): internal parameters
    fStatementCache: TSQLStatementCached;
    /// used during GetAndPrepareStatement() execution (run in global lock)
    fStatement: PSQLRequest;
    fStaticStatement: TSQLRequest;
    fStatementTimer: PPrecisionTimer;
    fStatementMonitor: TSynMonitor;
    fStaticStatementTimer: TPrecisionTimer;
    fStatementSQL: RawUTF8;
    fStatementGenericSQL: RawUTF8;
    fStatementMaxParam: integer;
    fStatementLastException: RawUTF8;
    fStatementTruncateSQLLogLen: integer;
    fStatementPreparedSelectQueryPlan: boolean;
    /// check if a VACUUM statement is possible
    // - VACUUM in fact DISCONNECT all virtual modules (sounds like a SQLite3
    // design problem), so calling it during process could break the engine
    // - if you can safely run VACUUM, returns TRUE and release all active
    // SQL statements (otherwise VACUUM will fail)
    // - if there are some static virtual tables, returns FALSE and do nothing:
    // in this case, VACUUM will be a no-op
    function PrepareVacuum(const aSQL: RawUTF8): boolean;
  protected
    fBatchMethod: TSQLURIMethod;
    fBatchOptions: TSQLRestBatchOptions;
    fBatchTableIndex: integer;
    fBatchID: TIDDynArray;
    fBatchIDCount: integer;
    fBatchIDMax: TID;
    fBatchValues: TRawUTF8DynArray;
    fBatchValuesCount: integer;
    constructor RegisteredClassCreateFrom(aModel: TSQLModel;
      aServerHandleAuthentication: boolean; aDefinition: TSynConnectionDefinition); override;
    /// retrieve a TSQLRequest instance in fStatement
    // - will set @fStaticStatement if no :(%): internal parameters appear:
    // in this case, the TSQLRequest.Close method must be called
    // - will set a @fStatementCache[].Statement, after having bounded the
    // :(%): parameter values; in this case, TSQLRequest.Close must not be called
    // - expect sftBlob, sftBlobDynArray and sftBlobRecord properties
    // to be encoded as ':("\uFFF0base64encodedbinary"):'
    procedure GetAndPrepareStatement(const SQL: RawUTF8; ForceCacheStatement: boolean);
    /// free a static prepared statement on success or from except on E: Exception block
    procedure GetAndPrepareStatementRelease(E: Exception=nil; const Msg: ShortString='';
      ForceBindReset: boolean=false);
    /// create or retrieve from the cache a TSQLRequest instance in fStatement
    // - called e.g. by GetAndPrepareStatement()
    procedure PrepareStatement(Cached: boolean);
    /// reset the cache if necessary
    procedure SetNoAJAXJSON(const Value: boolean); override;
    {$ifdef WITHLOG}
    /// overriden method which will also set the DB.LogClass
    procedure SetLogClass(aClass: TSynLogClass); override;
    {$endif}
    /// overridden methods for direct sqlite3 database engine call:
    function MainEngineList(const SQL: RawUTF8; ForceAJAX: Boolean; ReturnedRowCount: PPtrInt): RawUTF8; override;
    function MainEngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    function MainEngineAdd(TableModelIndex: integer; const SentData: RawUTF8): TID; override;
    function MainEngineUpdate(TableModelIndex: integer; ID: TID; const SentData: RawUTF8): boolean; override;
    function MainEngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function MainEngineDeleteWhere(TableModelIndex: Integer; const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    function MainEngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function MainEngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    function MainEngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    function MainEngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
    function EngineExecute(const aSQL: RawUTF8): boolean; override;
    procedure InternalStat(Ctxt: TSQLRestServerURIContext; W: TTextWriter); override;
    procedure InternalInfo(var info: TDocVariantData); override;
    /// execute one SQL statement
    // - intercept any DB exception and return false on error, true on success
    // - optional LastInsertedID can be set (if ValueInt/ValueUTF8 are nil) to
    // retrieve the proper ID when aSQL is an INSERT statement (thread safe)
    // - optional LastChangeCount can be set (if ValueInt/ValueUTF8 are nil) to
    // retrieve the modified row count when aSQL is an UPDATE statement (thread safe)
    function InternalExecute(const aSQL: RawUTF8; ForceCacheStatement: boolean;
      ValueInt: PInt64=nil; ValueUTF8: PRawUTF8=nil; ValueInts: PInt64DynArray=nil;
      LastInsertedID: PInt64=nil; LastChangeCount: PInteger=nil): boolean;
    // overridden method returning TRUE for next calls to EngineAdd
    // will properly handle operations until InternalBatchStop is called
    function InternalBatchStart(Method: TSQLURIMethod;
      BatchOptions: TSQLRestBatchOptions): boolean; override;
    // internal method called by TSQLRestServer.RunBatch() to process fast
    // multi-INSERT statements to the SQLite3 engine
    procedure InternalBatchStop; override;
  public
    /// begin a transaction (implements REST BEGIN Member)
    // - to be used to speed up some SQL statements like Insert/Update/Delete
    // - must be ended with Commit on success
    // - must be aborted with Rollback if any SQL statement failed
    // - return true if no transaction is active, false otherwise
    function TransactionBegin(aTable: TSQLRecordClass; SessionID: cardinal=1): boolean; override;
    /// end a transaction (implements REST END Member)
    // - write all pending SQL statements to the disk
    procedure Commit(SessionID: cardinal=1; RaiseException: boolean=false); override;
    /// abort a transaction (implements REST ABORT Member)
    // - restore the previous state of the database, before the call to TransactionBegin
    procedure RollBack(SessionID: cardinal=1); override;

     /// overridden method for direct SQLite3 database engine call
     // - it will update all BLOB fields at once, in one SQL statement
    function UpdateBlobFields(Value: TSQLRecord): boolean; override;
     /// overridden method for direct SQLite3 database engine call
     // - it will retrieve all BLOB fields at once, in one SQL statement
    function RetrieveBlobFields(Value: TSQLRecord): boolean; override;

    {$ifndef KYLIX3}
    /// backup of the opened Database into an external stream (e.g. a file,
    //  compressed or not)
    // - DEPRECATED: use DB.BackupBackground() instead
    // - this method doesn't use the SQLite Online Backup API, but low-level
    // database file copy which may lock the database process if the data
    // is consistent - consider using DB.BackupBackground() method instead
    // - database is closed, VACCUUMed, copied, then reopened
    function Backup(Dest: TStream): boolean; deprecated;
    /// backup of the opened Database into a .gz compressed file
    // - DEPRECATED: use DB.BackupBackground() instead
    // - this method doesn't use the SQLite Online Backup API, but low-level
    //   database file copy which may lock the database process if the data
    //   is consistent - consider using DB.BackupBackground() method instead
    // - database is closed, VACCUUMed, compressed into .gz file, then reopened
    // - default compression level is 2, which is very fast, and good enough for
    //   a database file content: you may change it into the default 6 level
    function BackupGZ(const DestFileName: TFileName;
      CompressionLevel: integer=2): boolean; deprecated;
    {$endif}

    /// restore a database content on the fly
    // - database is closed, source DB file is replaced by the supplied content,
    //   then reopened
    // - there are cases where this method will fail and return FALSE: consider
    //   shuting down the server, replace the file, then relaunch the server instead
    function Restore(const ContentToRestore: RawByteString): boolean;
    /// restore a database content on the fly, from a .gz compressed file
    // - database is closed, source DB file is replaced by the supplied content,
    //  then reopened
    // - there are cases where this method will fail and return FALSE: consider
    //   shuting down the server, replace the file, then relaunch the server instead
    function RestoreGZ(const BackupFileName: TFileName): boolean;
    /// used e.g. by IAdministratedDaemon to implement "pseudo-SQL" commands
    procedure AdministrationExecute(const DatabaseName,SQL: RawUTF8;
      var result: TServiceCustomAnswer); override;
    /// retrieves the per-statement detailed timing, as a TDocVariantData
    procedure ComputeDBStats(out result: variant); overload;
    /// retrieves the per-statement detailed timing, as a TDocVariantData
    function ComputeDBStats: variant; overload;

    /// initialize the associated DB connection
    // - called by Create and on Backup/Restore just after DB.DBOpen
    // - will register all *_in() functions for available TSQLRecordRTree
    // - will register all modules for available TSQLRecordVirtualTable*ID
    // with already registered modules via RegisterVirtualTableModule()
    // - you can override this method to call e.g. DB.RegisterSQLFunction()
    procedure InitializeEngine; virtual;
    /// call this method when the internal DB content is known to be invalid
    // - by default, all REST/CRUD requests and direct SQL statements are
    // scanned and identified as potentially able to change the internal SQL/JSON
    // cache used at SQLite3 database level; but some virtual tables (e.g.
    // TSQLRestStorageExternal classes defined in SQLite3DB) could flush
    // the database content without proper notification
    // - this overridden implementation will call TSQLDataBase.CacheFlush method
    procedure FlushInternalDBCache; override;
    /// call this method to flush the internal SQL prepared statements cache
    // - you should not have to flush the cache, only e.g. before a DROP TABLE
    // - in all cases, running this method would never harm, nor be slow
    procedure FlushStatementCache;
    /// execute one SQL statement, and apply an Event to every record
    // - lock the database during the run
    // - call a fast "stored procedure"-like method for each row of the request;
    // this method must use low-level DB access in any attempt to modify the
    // database (e.g. a prepared TSQLRequest with Reset+Bind+Step), and not
    // the TSQLRestServerDB.Engine*() methods which include a Lock(): this Lock()
    // is performed by the main loop in EngineExecute() and any attempt to
    // such high-level call will fail into an endless loop
    // - caller may use a transaction in order to speed up StoredProc() writing
    // - intercept any DB exception and return false on error, true on success
    function StoredProcExecute(const aSQL: RawUTF8; StoredProc: TOnSQLStoredProc): boolean;
  public
    /// initialize a REST server with a SQLite3 database
    // - any needed TSQLVirtualTable class should have been already registered
    // via the RegisterVirtualTableModule() method
    constructor Create(aModel: TSQLModel; aDB: TSQLDataBase;
      aHandleUserAuthentication: boolean=false; aOwnDB: boolean=false); reintroduce; overload; virtual;
    /// initialize a REST server with a database, by specifying its filename
    // - TSQLRestServerDB will initialize a owned TSQLDataBase, and free it on Destroy
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run)
    // - it will then call the other overloaded constructor to initialize the server
    constructor Create(aModel: TSQLModel; const aDBFileName: TFileName;
      aHandleUserAuthentication: boolean=false; const aPassword: RawUTF8='';
      aDefaultCacheSize: integer=10000; aDefaultPageSize: integer=4096); reintroduce; overload;
    /// initialize a REST server with a database, and a temporary Database Model
    // - a Model will be created with supplied tables, and owned by the server
    // - if you instantiate a TSQLRestServerFullMemory or TSQLRestServerDB
    // with this constructor, an in-memory engine will be created, with
    // enough abilities to run regression tests, for instance
    constructor CreateWithOwnModel(const aTables: array of TSQLRecordClass;
      const aDBFileName: TFileName; aHandleUserAuthentication: boolean=false;
      const aRoot: RawUTF8='root'; const aPassword: RawUTF8='';
      aDefaultCacheSize: integer=10000; aDefaultPageSize: integer=4096); overload;
    /// initialize a REST server with an in-memory SQLite3 database
    // - could be used for test purposes
    constructor Create(aModel: TSQLModel; aHandleUserAuthentication: boolean=false); overload; override;
    /// initialize a REST server with an in-memory SQLite3 database and a
    // temporary Database Model
    // - could be used for test purposes
    constructor CreateWithOwnModel(const aTables: array of TSQLRecordClass;
      aHandleUserAuthentication: boolean=false); overload;
    /// close database and free used memory
    destructor Destroy; override;
    /// save the TSQLRestServerDB properties into a persistent storage object
    // - RegisteredClassCreateFrom() will expect Definition.DatabaseName to store
    // the DBFileName, and optionally encrypt the file using Definition.Password
    procedure DefinitionTo(Definition: TSynConnectionDefinition); override;
    /// Missing tables are created if they don't exist yet for every TSQLRecord
    // class of the Database Model
    // - you must call explicitely this before having called StaticDataCreate()
    // - all table description (even Unique feature) is retrieved from the Model
    // - this method also create additional fields, if the TSQLRecord definition
    // has been modified; only field adding is available, field renaming or
    // field deleting are not allowed in the FrameWork (in such cases, you must
    // create a new TSQLRecord type)
    procedure CreateMissingTables(user_version: cardinal=0;
      Options: TSQLInitializeTableOptions=[]); override;
    /// search for the last inserted ID in a table
    // - will execute not default select max(rowid) from Table, but faster
    // $ select rowid from Table order by rowid desc limit 1
    function TableMaxID(Table: TSQLRecordClass): TID; override;
    /// after how many bytes a sllSQL statement log entry should be truncated
    // - default is 0, meaning no truncation
    // - typical value is 2048 (2KB), which will avoid any heap allocation
    property StatementTruncateSQLLogLen: integer read fStatementTruncateSQLLogLen
      write fStatementTruncateSQLLogLen;
    /// executes (therefore log) the QUERY PLAN for each prepared statement
    property StatementPreparedSelectQueryPlan: boolean
      read fStatementPreparedSelectQueryPlan write fStatementPreparedSelectQueryPlan;
  published
    /// associated database
    property DB: TSQLDataBase read fDB;
    /// contains some textual information about the latest Exception raised
    // during SQL statement execution
    property StatementLastException: RawUTF8 read fStatementLastException;
  end;

  /// REST client with direct access to a SQLite3 database
  // - a hidden TSQLRestServerDB server is created and called internaly
  TSQLRestClientDB = class(TSQLRestClientURI)
  private
    // use internaly a TSQLRestServerDB to access data in the proper JSON format
    fServer: TSQLRestServerDB;
    fOwnedServer: TSQLRestServerDB;
    fOwnedDB: TSQLDataBase;
    fInternalHeader: RawUTF8;
    function getDB: TSQLDataBase;
  protected
    /// method calling the RESTful server fServer
    procedure InternalURI(var Call: TSQLRestURIParams); override;
    /// overridden protected method do nothing (direct DB access has no connection)
    function InternalCheckOpen: boolean; override;
    /// overridden protected method do nothing (direct DB access has no connection)
    procedure InternalClose; override;
  public
    /// initializes the class, and creates an internal TSQLRestServerDB to
    // internaly answer to the REST queries
    // - aServerClass could be TSQLRestServerDB by default
    constructor Create(aClientModel, aServerModel: TSQLModel; aDB: TSQLDataBase;
      aServerClass: TSQLRestServerDBClass;
      aHandleUserAuthentication: boolean=false); reintroduce; overload;
    /// same as above, from a SQLite3 filename specified
    // - an internal TSQLDataBase will be created internaly and freed on Destroy
    // - aServerClass could be TSQLRestServerDB by default
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run)
    constructor Create(aClientModel, aServerModel: TSQLModel; const aDBFileName: TFileName;
      aServerClass: TSQLRestServerDBClass; aHandleUserAuthentication: boolean=false;
      const aPassword: RawUTF8=''; aDefaultCacheSize: integer=10000); reintroduce; overload;
    /// initialize the class, for an existing TSQLRestServerDB
    // - the client TSQLModel will be cloned from the server's one
    // - the TSQLRestServerDB and TSQLDatabase instances won't be managed by the
    // client, but will access directly to the server
    constructor Create(aRunningServer: TSQLRestServerDB); reintroduce; overload;
    /// release the server
    destructor Destroy; override;

    /// retrieve a list of members as a TSQLTable (implements REST GET Collection)
    // - this overridden method call directly the database to get its result,
    // without any URI() call, but with use of DB JSON cache if available
    // - other TSQLRestClientDB methods use URI() function and JSON conversion
    // of only one record properties values, which is very fast
    function List(const Tables: array of TSQLRecordClass; const SQLSelect: RawUTF8='ID';
      const SQLWhere: RawUTF8=''): TSQLTableJSON; override;
    /// associated Server
    property Server: TSQLRestServerDB read fServer;
    /// associated database
    property DB: TSQLDataBase read getDB;
  end;

  /// define a Virtual Table module for a stand-alone SQLite3 engine
  // - it's not needed to free this instance: it will be destroyed by the SQLite3
  // engine together with the DB connection
  TSQLVirtualTableModuleSQLite3 = class(TSQLVirtualTableModule)
  protected
    fDB: TSQLDataBase;
    /// used internaly to register the module to the SQLite3 engine
    fModule: TSQLite3Module;
  public
    /// initialize the module for a given DB connection
    // - internally set fModule and call sqlite3_create_module_v2(fModule)
    // - will raise EBusinessLayerException if aDB is incorrect, or SetDB() has
    // already been called for this module
    // - will call sqlite3_check() to raise the corresponding ESQLite3Exception
    // - in case of success (no exception), the SQLite3 engine will release the
    // module by itself; but in case of error (an exception is raised), it is
    // up to the caller to intercept it via a try..except and free the
    // TSQLVirtualTableModuleSQLite3 instance
    procedure Attach(aDB: TSQLDataBase);
    /// retrieve the file name to be used for a specific Virtual Table
    // - overridden method returning a file located in the DB file folder, and
    // '' if the main DB was created as SQLITE_MEMORY_DATABASE_NAME (i.e.
    // ':memory:' so that no file should be written)
    // - of course, if a custom FilePath property value is specified, it will be
    // used, even if the DB is created as SQLITE_MEMORY_DATABASE_NAME
    function FileName(const aTableName: RawUTF8): TFileName; override;
    /// the associated SQLite3 database connection
    property DB: TSQLDataBase read fDB;
  end;

  /// define a Virtual Table module for a TSQLRestServerDB SQLite3 engine
  TSQLVirtualTableModuleServerDB = class(TSQLVirtualTableModuleSQLite3)
  public
    /// register the Virtual Table to the database connection of a TSQLRestServerDB server
    // - in case of an error, an excepton will be raised
    constructor Create(aClass: TSQLVirtualTableClass; aServer: TSQLRestServer); override;
  end;


  /// REST storage sharded over several SQlite3 instances
  // - numerotated '*0000.dbs' SQLite3 files would contain the sharded data
  // - here *.dbs is used as extension, to avoid any confusion with regular
  // SQLite3 database files (*.db or *.db3)
  // - when the server is off (e.g. on periodic version upgrade), you may safely
  // delete/archive some oldest *.dbs files, for easy and immediate purge of
  // your database content: such process would be much faster and cleaner than
  // regular "DELETE FROM TABLE WHERE ID < ?" + "VACUUM" commands
  TSQLRestStorageShardDB = class(TSQLRestStorageShard)
  protected
    fShardRootFileName: TFileName;
    fSynchronous: TSQLSynchronousMode;
    fInitShardsIsLast: boolean;
    fCacheSizePrevious, fCacheSizeLast: integer;
    procedure InitShards; override;
    function InitNewShard: TSQLRest; override;
    function DBFileName(ShardIndex: Integer): TFileName; virtual;
  public
    /// initialize the table storage redirection for sharding over SQLite3 DB
    // - if no aShardRootFileName is set, the executable folder and stored class
    // table name would be used
    // - typical use may be:
    // ! Server.StaticDataAdd(TSQLRestStorageShardDB.Create(TSQLRecordSharded,Server,500000))
    // - you may define some low-level tuning of SQLite3 process via aSynchronous
    // / aCacheSizePrevious / aCacheSizeLast / aMaxShardCount parameters, if
    // the default smOff / 1MB / 2MB / 100 values are not enough
    constructor Create(aClass: TSQLRecordClass; aServer: TSQLRestServer;
      aShardRange: TID; aOptions: TSQLRestStorageShardOptions=[];
      const aShardRootFileName: TFileName=''; aMaxShardCount: integer=100;
      aSynchronous: TSQLSynchronousMode=smOff;
      aCacheSizePrevious: integer=250; aCacheSizeLast: integer=500); reintroduce; virtual;
  published
    /// associated file name for the SQLite3 database files
    // - contains the folder, and root file name for the storage
    // - each shard would end with its 4 digits index: actual file name would
    // append '0000.dbs' to this ShardRootFileName
    property ShardRootFileName: TFileName read fShardRootFileName;
  end;



/// initialize a Virtual Table Module for a specified database
// - to be used for low-level access to a virtual module, e.g. with
// TSQLVirtualTableLog
// - when using our ORM, you should call TSQLModel.VirtualTableRegister()
// instead to associate a TSQLRecordVirtual class to a module
// - returns the created TSQLVirtualTableModule instance (which will be a
// TSQLVirtualTableModuleSQLite3 instance in fact)
// - will raise an exception of failure
function RegisterVirtualTableModule(aModule: TSQLVirtualTableClass;
  aDatabase: TSQLDataBase): TSQLVirtualTableModule;


implementation

{$ifdef SQLVIRTUALLOGS}
uses
  mORMotDB;
{$endif SQLVIRTUALLOGS}


{ TSQLTableDB }

constructor TSQLTableDB.Create(aDB: TSQLDataBase; const Tables: array of TSQLRecordClass;
  const aSQL: RawUTF8; Expand: boolean);
var JSONCached: RawUTF8;
    R: TSQLRequest;
    n: PtrInt;
begin
  if aDB=nil then
    exit;
  JSONCached := aDB.LockJSON(aSQL,@n);
  if JSONCached='' then // not retrieved from cache -> call SQLite3 engine
    try // faster than sqlite3_get_table(): memory is allocated as a whole
      n := 0;
      JSONCached := R.ExecuteJSON(aDB.DB,aSQL,Expand,@n); // Expand=true for AJAX
      inherited CreateFromTables(Tables,aSQL,JSONCached);
      Assert(n=fRowCount);
    finally
      aDB.UnLockJSON(JSONCached,n);
    end
  else begin
    inherited CreateFromTables(Tables,aSQL,JSONCached);
    Assert(n=fRowCount);
  end;
end;


{ TSQLRestServerDB }

procedure TSQLRestServerDB.PrepareStatement(Cached: boolean);
var wasPrepared: boolean;
    timer: PPPrecisionTimer;
begin
  fStaticStatementTimer.Start;
  if not Cached then begin
    fStaticStatement.Prepare(DB.DB,fStatementGenericSQL);
    fStatementGenericSQL := '';
    fStatement := @fStaticStatement;
    fStatementTimer := @fStaticStatementTimer;
    fStatementMonitor := nil;
    exit;
  end;
  if mlSQLite3 in StatLevels then
    timer := @fStatementTimer else
    timer := nil;
  fStatement := fStatementCache.Prepare(fStatementGenericSQL,@wasPrepared,timer,@fStatementMonitor);
  if wasPrepared then begin
    InternalLog('prepared % % %', [fStaticStatementTimer.Stop,
      DB.FileNameWithoutPath,fStatementGenericSQL],sllDB);
    if fStatementPreparedSelectQueryPlan then
      DB.ExecuteJSON('explain query plan '+
        StringReplaceChars(fStatementGenericSQL,'?','1'), {expand=}true);
  end;
  if timer=nil then begin
    fStaticStatementTimer.Start;
    fStatementTimer := @fStaticStatementTimer;
    fStatementMonitor := nil;
  end;
end;

procedure TSQLRestServerDB.GetAndPrepareStatement(const SQL: RawUTF8;
  ForceCacheStatement: boolean);
var i, sqlite3param: integer;
    Types: TSQLParamTypeDynArray;
    Nulls: TSQLFieldBits;
    Values: TRawUTF8DynArray;
begin
  // prepare statement
  fStatementSQL := SQL;
  fStatementGenericSQL := ExtractInlineParameters(SQL,Types,Values,fStatementMaxParam,Nulls);
  PrepareStatement(ForceCacheStatement or (fStatementMaxParam<>0));
  // bind parameters
  if fStatementMaxParam=0 then
    exit; // no valid :(...): inlined parameter found -> manual bind
  sqlite3param := sqlite3.bind_parameter_count(fStatement^.Request);
  if sqlite3param<>fStatementMaxParam then
    raise EORMException.CreateUTF8(
      '%.GetAndPrepareStatement(%) recognized % params, and % for SQLite3',
      [self,fStatementGenericSQL,fStatementMaxParam,sqlite3param]);
  for i := 0 to fStatementMaxParam-1 do
    if i in Nulls then
      fStatement^.BindNull(i+1) else
      case Types[i] of
        sptDateTime, // date/time are stored as ISO-8601 TEXT in SQLite3
        sptText:    fStatement^.Bind(i+1,Values[i]);
        sptBlob:    fStatement^.BindBlob(i+1,Values[i]);
        sptInteger: fStatement^.Bind(i+1,GetInt64(pointer(Values[i])));
        sptFloat:   fStatement^.Bind(i+1,GetExtended(pointer(Values[i])));
      end;
end;

procedure TSQLRestServerDB.GetAndPrepareStatementRelease(E: Exception;
  const Msg: ShortString; ForceBindReset: boolean);
var
  tmp: TSynTempBuffer;
  P: PAnsiChar;
begin
  try
    if fStatementTimer<>nil then begin
      if fStatementMonitor<>nil then
        fStatementMonitor.ProcessEnd else
        fStatementTimer^.Pause;
      if E=nil then
        if (fStatementTruncateSQLLogLen > 0) and
           (length(fStatementSQL) > fStatementTruncateSQLLogLen) then begin
          tmp.Init(pointer(fStatementSQL),fStatementTruncateSQLLogLen);
          P := tmp.buf;
          PCardinal(P+fStatementTruncateSQLLogLen-3)^ := ord('.')+ord('.')shl 8+ord('.')shl 16;
          InternalLog('% % % len=%',[fStatementTimer^.LastTime,Msg,P,length(fStatementSQL)],sllSQL);
          tmp.Done;
        end else
          InternalLog('% % %',[fStatementTimer^.LastTime,Msg,fStatementSQL],sllSQL) else
        InternalLog('% for % // %',[E,fStatementSQL,fStatementGenericSQL],sllError);
      fStatementTimer := nil;
    end;
    fStatementMonitor := nil;
  finally
    if fStatement<>nil then begin
      if fStatement=@fStaticStatement then
        fStaticStatement.Close else
        if (fStatementMaxParam<>0) or ForceBindReset then
          fStatement^.BindReset; // release bound RawUTF8 ASAP
      fStatement := nil;
    end;
    fStatementSQL := '';
    fStatementGenericSQL := '';
    fStatementMaxParam := 0;
    if E<>nil then
      FormatUTF8('% %',[E,ObjectToJSONDebug(E)],fStatementLastException);
  end;
end;

procedure TSQLRestServerDB.FlushStatementCache;
begin
  DB.Lock;
  try
    fStatementCache.ReleaseAllDBStatements;
  finally
    DB.Unlock;
  end;
end;

function TSQLRestServerDB.TableMaxID(Table: TSQLRecordClass): TID;
var SQL: RawUTF8;
begin
  if StaticTable[Table]<>nil then
    result := inherited TableMaxID(Table) else begin
    SQL := 'select rowid from '+Table.SQLTableName+' order by rowid desc limit 1';
    if not InternalExecute(SQL,true,PInt64(@result)) then
      result := 0;
  end;
end;

function TSQLRestServerDB.MainEngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
var Props: TSQLRecordProperties;
    SQL: RawUTF8;
    Decoder: TJSONObjectDecoder;
begin
  result := 0;
  if TableModelIndex<0 then
    exit;
  Props := fModel.TableProps[TableModelIndex].Props;
  SQL := Props.SQLTableName;
  if fBatchMethod<>mNone then begin
    result := 0; // indicates error
    if SentData='' then
      InternalLog('BATCH with MainEngineAdd(%,SentData="") -> '+
        'DEFAULT VALUES not implemented',[SQL],sllError) else
    if (fBatchMethod=mPOST) and (fBatchIDMax>=0) and
       ((fBatchTableIndex<0) or (fBatchTableIndex=TableModelIndex)) then begin
      fBatchTableIndex := TableModelIndex;
      if JSONGetID(pointer(SentData),result) then begin
        if result>fBatchIDMax then
          fBatchIDMax := result;
      end else begin
        if fBatchIDMax=0 then begin
          fBatchIDMax := TableMaxID(Props.Table);
          if fBatchIDMax<0 then
            exit; // will force error for whole BATCH block
        end;
        inc(fBatchIDMax);
        result := fBatchIDMax;
      end;
      AddID(fBatchID,fBatchIDCount,result);
      AddRawUTF8(fBatchValues,fBatchValuesCount,SentData);
    end;
    exit;
  end;
  SQL := 'INSERT INTO '+SQL;
  if trim(SentData)='' then
    SQL := SQL+' DEFAULT VALUES;' else begin
    JSONGetID(pointer(SentData),result);
    Decoder.Decode(SentData,nil,pInlined,result,false);
    if Props.RecordVersionField<>nil then
      InternalRecordVersionHandle(
        soInsert,TableModelIndex,decoder,Props.RecordVersionField);
    SQL := SQL+Decoder.EncodeAsSQL(false)+';';
  end;
  if InternalExecute(SQL,true,nil,nil,nil,PInt64(@result)) then
    InternalUpdateEvent(seAdd,TableModelIndex,result,SentData,nil);
end;

procedure InternalRTreeIn(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var aRTree: TSQLRecordRTreeClass;
    BlobA, BlobB: pointer;
begin
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  aRTree := sqlite3.user_data(Context);
  BlobA := sqlite3.value_blob(argv[0]);
  BlobB := sqlite3.value_blob(argv[1]);
  if (aRTree=nil) or (BlobA=nil) or (BlobB=nil) then
    sqlite3.result_error(Context,'invalid call') else
    sqlite3.result_int64(Context,byte(aRTree.ContainedIn(BlobA^,BlobB^)));
end;

constructor TSQLRestServerDB.Create(aModel: TSQLModel;
  aHandleUserAuthentication: boolean);
begin
  Create(aModel,SQLITE_MEMORY_DATABASE_NAME,aHandleUserAuthentication);
end;

constructor TSQLRestServerDB.Create(aModel: TSQLModel; aDB: TSQLDataBase;
  aHandleUserAuthentication, aOwnDB: boolean);
begin
  fStatementCache.Init(aDB.DB);
  aDB.UseCache := true; // we better use caching in this JSON oriented use
  fDB := aDB;
  if aOwnDB then
    fOwnedDB := fDB;
  if fDB.InternalState=nil then begin // should be done once
    InternalState := 1;
    fDB.InternalState := @InternalState; // to update TSQLRestServerDB.InternalState
  end;
  inherited Create(aModel,aHandleUserAuthentication);
  InitializeEngine;
end;

{$ifdef WITHLOG}
procedure TSQLRestServerDB.SetLogClass(aClass: TSynLogClass);
begin
  inherited;
  if DB<>nil then
    DB.Log := aClass; // ensure low-level SQLite3 engine will share the same log
end;
{$endif}

procedure TSQLRestServerDB.InitializeEngine;
var i: integer;
    module: TSQLVirtualTableClass;
    registered: array of TSQLVirtualTableClass;
begin
  for i := 0 to high(Model.TableProps) do
    case Model.TableProps[i].Kind of
    rRTree, rRTreeInteger: // register all *_in() SQL functions
      sqlite3_check(DB.DB,sqlite3.create_function_v2(DB.DB,
        pointer(TSQLRecordRTreeClass(Model.Tables[i]).RTreeSQLFunctionName),
        2,SQLITE_ANY,Model.Tables[i],InternalRTreeIn,nil,nil,nil));
    rCustomForcedID, rCustomAutoID: begin
      module := Model.VirtualTableModule(Model.Tables[i]);
      if (module<>nil) and (PtrArrayFind(registered,module)<0) then begin
        TSQLVirtualTableModuleServerDB.Create(module,self);
        PtrArrayAdd(registered,module); // register it once for this DB
      end;
    end;
    end;
end;

constructor TSQLRestServerDB.Create(aModel: TSQLModel; const aDBFileName: TFileName;
  aHandleUserAuthentication: boolean; const aPassword: RawUTF8;
  aDefaultCacheSize, aDefaultPageSize: integer);
begin
  fOwnedDB := TSQLDataBase.Create(aDBFileName,aPassword,0,aDefaultCacheSize,aDefaultPageSize);
  // fOwnedDB.Free done in Destroy
  Create(aModel,fOwnedDB,aHandleUserAuthentication);
end;

constructor TSQLRestServerDB.CreateWithOwnModel(const aTables: array of TSQLRecordClass;
  const aDBFileName: TFileName; aHandleUserAuthentication: boolean;
  const aRoot, aPassword: RawUTF8; aDefaultCacheSize, aDefaultPageSize: integer);
begin
  Create(TSQLModel.Create(aTables,aRoot),aDBFileName,aHandleUserAuthentication,
    aPassword,aDefaultCacheSize,aDefaultPageSize);
  fModel.Owner := self;
end;

constructor TSQLRestServerDB.CreateWithOwnModel(const aTables: array of TSQLRecordClass;
  aHandleUserAuthentication: boolean);
begin
  Create(TSQLModel.Create(aTables),aHandleUserAuthentication);
  fModel.Owner := self;
end;

procedure TSQLRestServerDB.CreateMissingTables(user_version: cardinal;
  Options: TSQLInitializeTableOptions);
var t,f,nt,nf: integer;
    TableNamesAtCreation, aFields: TRawUTF8DynArray;
    TableJustCreated: TSQLFieldTables;
    aSQL: RawUTF8;
begin
  if DB.TransactionActive then
    raise EBusinessLayerException.Create('CreateMissingTables in transaction');
  fDB.GetTableNames(TableNamesAtCreation);
  nt := length(TableNamesAtCreation);
  QuickSortRawUTF8(TableNamesAtCreation,nt,nil,@StrIComp);
  {$ifdef WITHLOG}
  fLogFamily.SynLog.Log(sllDB,'CreateMissingTables on %',[fDB],self);
  fLogFamily.SynLog.Log(sllDB,'GetTables',TypeInfo(TRawUTF8DynArray),TableNamesAtCreation,self);
  {$endif}
  FillcharFast(TableJustCreated,sizeof(TSQLFieldTables),0);
  try
    // create not static and not existing tables
    for t := 0 to high(Model.Tables) do
      if ((fStaticData=nil) or (fStaticData[t]=nil)) then
      // this table is not static -> check if already existing, create if necessary
      with Model.TableProps[t], Props do
      if not NoCreateMissingTable then
      if FastFindPUTF8CharSorted(pointer(TableNamesAtCreation),nt-1,pointer(SQLTableName),@StrIComp)<0 then begin
        if not DB.TransactionActive then
          DB.TransactionBegin; // make initialization faster by using transaction
        DB.Execute(Model.GetSQLCreate(t)); // don't catch exception in constructor
        include(TableJustCreated,t);       // mark to be initialized below
      end else
      if not(itoNoCreateMissingField in Options) then begin
        // this table is existing: check that all fields exist -> create if necessary
        DB.GetFieldNames(aFields,SQLTableName);
        nf := length(aFields);
        QuickSortRawUTF8(aFields,nf,nil,@StrIComp);
        for f := 0 to Fields.Count-1 do
          with Fields.List[f] do
          if SQLFieldType in COPIABLE_FIELDS then
          /// real database columns exist for Simple + Blob fields (not Many)
          if FastFindPUTF8CharSorted(pointer(aFields),nf-1,pointer(Name),@StrIComp)<0 then begin
            aSQL := Model.GetSQLAddField(t,f);
            if aSQL<>'' then begin // need a true field with data
              if not DB.TransactionActive then
                DB.TransactionBegin; // make initialization faster by using transaction
              DB.Execute(aSQL);
            end;
            Model.Tables[t].InitializeTable(self,Name,Options);
          end;
      end;
    if not DB.TransactionActive then
      exit;
    // database schema was modified -> update user version in SQLite3 file
    if user_version<>0 then
      DB.user_version := user_version;
    // initialize new tables AFTER creation of ALL tables
    if not IsZero(@TableJustCreated,sizeof(TSQLFieldTables)) then
      for t := 0 to high(Model.Tables) do
        if t in TableJustCreated then
          if not(Model.TableProps[t].Kind in IS_CUSTOM_VIRTUAL) or
             not TableHasRows(Model.Tables[t]) then // check is really void
            Model.Tables[t].InitializeTable(self,'',Options); // '' for table creation
    DB.Commit;
  except
    on E: Exception do begin
      DB.RollBack; // will close any active Transaction
      raise;     // caller must handle exception
    end;
  end;
end;

function TSQLRestServerDB.MainEngineDelete(TableModelIndex: integer; ID: TID): boolean;
begin
  if (TableModelIndex<0) or (ID<=0) then
    result := false else begin
    // notify BEFORE deletion
    InternalUpdateEvent(seDelete,TableModelIndex,ID,'',nil);
    result := ExecuteFmt('DELETE FROM % WHERE RowID=:(%):;',
      [fModel.TableProps[TableModelIndex].Props.SQLTableName,ID]);
  end;
end;

function TSQLRestServerDB.MainEngineDeleteWhere(TableModelIndex: Integer;
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
var i: integer;
    aSQLWhere: RawUTF8;
begin
  if (TableModelIndex<0) or (IDs=nil) then
    result := false else begin
    // notify BEFORE deletion
    for i := 0 to high(IDs) do
      InternalUpdateEvent(seDelete,TableModelIndex,IDs[i],'',nil);
    if IdemPChar(pointer(SQLWhere),'LIMIT ') or
       IdemPChar(pointer(SQLWhere),'ORDER BY ') then
      // LIMIT is not handled by SQLite3 when built from amalgamation
      // see http://www.sqlite.org/compile.html#enable_update_delete_limit
      aSQLWhere := Int64DynArrayToCSV(pointer(IDs),length(IDs),'RowID IN (',')') else
      aSQLWhere := SQLWhere;
    result := ExecuteFmt('DELETE FROM %%',
      [fModel.TableProps[TableModelIndex].Props.SQLTableName,SQLFromWhere(aSQLWhere)]);
  end;
end;

destructor TSQLRestServerDB.Destroy;
begin
  {$ifdef WITHLOG}
  with fLogClass.Enter('Destroy %', [fModel.SafeRoot], self) do
  {$endif}
  try
    if (fDB<>nil) and (fDB.InternalState=@InternalState) then
      fDB.InternalState := nil; // avoid memory modification on free block 
    inherited Destroy;
  finally
    try
      fStatementCache.ReleaseAllDBStatements;
    finally
      fOwnedDB.Free; // do nothing if DB<>fOwnedDB
    end;
  end;
end;

procedure TSQLRestServerDB.DefinitionTo(Definition: TSynConnectionDefinition);
begin
  if Definition=nil then
    exit;
  inherited; // set Kind
  if fDB<>nil then begin
    Definition.ServerName := StringToUTF8(fDB.FileName);
    Definition.PasswordPlain := fDB.Password;
  end;
end;

constructor TSQLRestServerDB.RegisteredClassCreateFrom(aModel: TSQLModel;
  aServerHandleAuthentication: boolean; aDefinition: TSynConnectionDefinition);
begin
  Create(aModel,UTF8ToString(aDefinition.ServerName),aServerHandleAuthentication,
    aDefinition.PasswordPlain);
end;

function TSQLRestServerDB.PrepareVacuum(const aSQL: RawUTF8): boolean;
begin
  result := not IdemPChar(Pointer(aSQL),'VACUUM');
  if result then
    exit;
  result :=  (fStaticVirtualTable=nil) or
    IsZero(fStaticVirtualTable,length(fStaticVirtualTable)*sizeof(pointer));
  if result then
    // VACUUM will fail if there are one or more active SQL statements
    fStatementCache.ReleaseAllDBStatements;
end;

function TSQLRestServerDB.InternalExecute(const aSQL: RawUTF8;
  ForceCacheStatement: boolean; ValueInt: PInt64; ValueUTF8: PRawUTF8;
  ValueInts: PInt64DynArray; LastInsertedID: PInt64; LastChangeCount: PInteger): boolean;
var ValueIntsCount, Res: Integer;
    msg: shortstring;
begin
  msg := '';
  if (self<>nil) and (DB<>nil) then
  try
    DB.Lock(aSQL);
    try
      result := true;
      if not PrepareVacuum(aSQL) then
        exit; // no-op if there are some static virtual tables around
      try
        GetAndPrepareStatement(aSQL,ForceCacheStatement);
        if ValueInts<>nil then begin
          ValueIntsCount := 0;
          repeat
            res := fStatement^.Step;
            if res=SQLITE_ROW then
              AddInt64(ValueInts^,ValueIntsCount,fStatement^.FieldInt(0));
          until res=SQLITE_DONE;
          SetLength(ValueInts^,ValueIntsCount);
          FormatShort('returned Int64 len=%',[ValueIntsCount],msg);
        end else
        if (ValueInt=nil) and (ValueUTF8=nil) then begin
          // default execution: loop through all rows
          repeat until fStatement^.Step<>SQLITE_ROW;
          if LastInsertedID<>nil then begin
            LastInsertedID^ := DB.LastInsertRowID;
            FormatShort(' lastInsertedID=%',[LastInsertedID^],msg);
          end;
          if LastChangeCount<>nil then begin
            LastChangeCount^ := DB.LastChangeCount;
            FormatShort(' lastChangeCount=%',[LastChangeCount^],msg);
          end;
        end else
          // get one row, and retrieve value
          if fStatement^.Step<>SQLITE_ROW then
            result := false else
            if ValueInt<>nil then begin
              ValueInt^ := fStatement^.FieldInt(0);
              FormatShort('returned=%',[ValueInt^],msg);
            end else begin
              ValueUTF8^ := fStatement^.FieldUTF8(0);
              FormatShort('returned="%"',[ValueUTF8^],msg);
            end;
        GetAndPrepareStatementRelease(nil,msg);
      except
        on E: Exception do begin
          GetAndPrepareStatementRelease(E);
          result := false;
        end;
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: ESQLite3Exception do begin
      {$ifdef WITHLOG}
      InternalLog('% for % // %',[E,aSQL,fStatementGenericSQL],sllError);
      {$else}
      LogToTextFile('TSQLRestServerDB.InternalExecute '+RawUTF8(E.Message)+#13#10+aSQL);
      {$endif}
      result := false;
    end;
  end else
    result := false;
end;

function TSQLRestServerDB.StoredProcExecute(const aSQL: RawUTF8;
  StoredProc: TOnSQLStoredProc): boolean;
var R: TSQLRequest; // we don't use fStatementCache[] here
    Res: integer;
begin
  result := false;
  if (self<>nil) and (DB<>nil) and (aSQL<>'') and Assigned(StoredProc) then
  try
    {$ifdef WITHLOG}
    fLogFamily.SynLog.Enter('StoredProcExecute(%)', [aSQL], self);
    {$endif}
    DB.LockAndFlushCache; // even if aSQL is SELECT, StoredProc may update data
    try
      try
        R.Prepare(DB.DB,aSQL);
        if R.FieldCount>0 then
        repeat
          res := R.Step;
          if res=SQLITE_ROW then
            StoredProc(R); // apply the stored procedure to all rows
        until res=SQLITE_DONE;
        result := true;
      finally
        R.Close; // always release statement
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: ESQLite3Exception do begin
      {$ifdef WITHLOG}
      fLogFamily.SynLog.Log(sllError,'% for %',[E,aSQL],self);
      {$else}
      LogToTextFile(ClassName+'.StoredProcExecute Error: '+RawUTF8(E.Message)+#13#10+aSQL);
      {$endif}
      result := false;
    end;
  end;
end;

function TSQLRestServerDB.EngineExecute(const aSQL: RawUTF8): boolean;
begin
  result := InternalExecute(aSQL,{forcecache=}false);
end;

procedure TSQLRestServerDB.InternalInfo(var info: TDocVariantData);
begin
  inherited InternalInfo(info);
  info.AddValue('db', FormatString('% %', [ExtractFileName(DB.FileName), KB(DB.FileSize)]));
end;

procedure TSQLRestServerDB.InternalStat(Ctxt: TSQLRestServerURIContext; W: TTextWriter);
var i: integer;
    ndx: TIntegerDynArray;
begin
  inherited InternalStat(Ctxt,W);
  if Ctxt.InputExists['withall'] or Ctxt.InputExists['withsqlite3'] then begin
    W.CancelLastChar('}');
    W.AddShort(',"sqlite3":[');
    DB.Lock;
    try
      fStatementCache.SortCacheByTotalTime(ndx);
      with fStatementCache do
      for i := 0 to Count-1 do
        with Cache[ndx[i]] do begin
          W.AddJSONEscape([StatementSQL,Timer]);
          W.Add(',');
        end;
    finally
      DB.UnLock;
    end;
    W.CancelLastComma;
    W.Add(']','}');
  end;
end;

procedure TSQLRestServerDB.ComputeDBStats(out result: variant);
var i: integer;
    ndx: TIntegerDynArray;
    doc: TDocVariantData absolute result;
begin
  if self=nil then
    exit;
  doc.Init(JSON_OPTIONS_FAST_EXTENDED,dvObject);
  DB.Lock;
  try
    fStatementCache.SortCacheByTotalTime(ndx);
    with fStatementCache do
    for i := 0 to Count-1 do
      with Cache[ndx[i]] do
        doc.AddValue(StatementSQL,Timer.ComputeDetails);
  finally
    DB.UnLock;
  end;
end;

function TSQLRestServerDB.ComputeDBStats: variant;
begin
  ComputeDBStats(result);
end;

function TSQLRestServerDB.MainEngineList(const SQL: RawUTF8; ForceAJAX: Boolean;
  ReturnedRowCount: PPtrInt): RawUTF8;
var MS: TRawByteStringStream;
    RowCount: integer;
begin
  result := '';
  RowCount := 0;
  if (self<>nil) and (DB<>nil) and (SQL<>'') then begin
    // need a SQL request for R.Execute() to prepare a statement
    result := DB.LockJSON(SQL,ReturnedRowCount); // lock and try from cache
    if result<>'' then
      exit;
    try // Execute request if was not got from cache
      try
        GetAndPrepareStatement(SQL,{forcecache=}false);
        MS := TRawByteStringStream.Create;
        try
          RowCount := fStatement^.Execute(0,'',MS,ForceAJAX or not NoAJAXJSON);
          result := MS.DataString;
        finally
          MS.Free;
        end;
        GetAndPrepareStatementRelease(nil, FormatToShort('returned % as %',
          [Plural('row',RowCount),KB(result)]));
      except
        on E: ESQLite3Exception do
          GetAndPrepareStatementRelease(E);
      end;
    finally
      DB.UnLockJSON(result,RowCount);
    end;
  end;
  if ReturnedRowCount<>nil then
    ReturnedRowCount^ := RowCount;
end;

function TSQLRestServerDB.MainEngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8;
var aSQL: RawUTF8;
begin
  result := '';
  if (ID<0) or (TableModelIndex<0) then
    exit;
  with Model.TableProps[TableModelIndex] do
    FormatUTF8('SELECT % FROM % WHERE RowID=:(%):;',
      [SQL.TableSimpleFields[true,false],Props.SQLTableName,ID],aSQL);
  result := EngineList(aSQL,true); // ForceAJAX=true -> '[{...}]'#10
  if result<>'' then
    if IsNotAjaxJSON(pointer(result)) then
      // '{"fieldCount":2,"values":["ID","FirstName"]}'#$A -> ID not found
      result := '' else
      // list '[{...}]'#10 -> object '{...}'
      result := copy(result,2,length(result)-3);
end;

function TSQLRestServerDB.MainEngineRetrieveBlob(TableModelIndex: integer; aID: TID;
  BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean;
var SQL: RawUTF8;
begin
  result := false;
  if (aID<0) or (TableModelIndex<0) or not BlobField^.IsBlob then
    exit;
  // retrieve the BLOB using SQL
  try
    SQL := FormatUTF8('SELECT % FROM % WHERE RowID=?',
      [BlobField^.Name,Model.TableProps[TableModelIndex].Props.SQLTableName],[aID]);
    DB.Lock(SQL); // UPDATE for a blob field -> no JSON cache flush, but UI refresh
    try
      GetAndPrepareStatement(SQL,true);
      try
        if (fStatement^.FieldCount=1) and (fStatement^.Step=SQLITE_ROW) then begin
          BlobData := fStatement^.FieldBlob(0);
          result := true;
        end;
        GetAndPrepareStatementRelease(nil,KB(BlobData));
      except
        on E: Exception do
          GetAndPrepareStatementRelease(E);
      end;
    finally
      DB.UnLock;
    end;
  except
    on ESQLite3Exception do
      result := false;
  end;
end;

function TSQLRestServerDB.RetrieveBlobFields(Value: TSQLRecord): boolean;
var Static: TSQLRest;
    SQL: RawUTF8;
    f: PtrInt;
    size: Int64;
    data: TSQLVar;
begin
  result := false;
  if Value=nil then
    exit;
  Static := GetStaticTable(PSQLRecordClass(Value)^);
  if Static<>nil then
    result := Static.RetrieveBlobFields(Value) else
    if (DB<>nil) and (Value.ID>0) and (PSQLRecordClass(Value)^<>nil) then
    with Value.RecordProps do
    if BlobFields<>nil then begin
      SQL := FormatUTF8('SELECT % FROM % WHERE ROWID=?',
        [SQLTableRetrieveBlobFields,SQLTableName],[Value.ID]);
      DB.Lock(SQL);
      try
        GetAndPrepareStatement(SQL,true);
        try
          if fStatement^.Step<>SQLITE_ROW then
            exit;
          size := 0;
          for f := 0 to high(BlobFields) do begin
            SQlite3ValueToSQLVar(fStatement^.FieldValue(f),data);
            BlobFields[f].SetFieldSQLVar(Value,data); // OK for all blobs
            inc(size,SQLVarLength(data));
          end;
          GetAndPrepareStatementRelease(nil,KB(size));
          result := true;
        except
          on E: Exception do
            GetAndPrepareStatementRelease(E);
        end;
      finally
        DB.UnLock;
      end;
    end;
end;

procedure TSQLRestServerDB.SetNoAJAXJSON(const Value: boolean);
begin
  inherited;
  if Value=NoAJAXJSON then
     exit;
  fDB.Cache.Reset; // we changed the JSON format -> cache must be updated
end;

function TSQLRestServerDB.MainEngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUTF8): boolean;
var Props: TSQLRecordProperties;
    Decoder: TJSONObjectDecoder;
    SQL: RawUTF8;
begin
  if (TableModelIndex<0) or (ID<=0) then
    result := false else
  if SentData='' then // update with no simple field -> valid no-op
    result := true else begin
    // this SQL statement use :(inlined params): for all values
    Props := fModel.TableProps[TableModelIndex].Props;
    Decoder.Decode(SentData,nil,pInlined,ID,false);
    if Props.RecordVersionField<>nil then
      InternalRecordVersionHandle(
        soUpdate,TableModelIndex,decoder,Props.RecordVersionField);
    SQL := Decoder.EncodeAsSQL(true);
    result := ExecuteFmt('UPDATE % SET % WHERE RowID=:(%):',
      [Props.SQLTableName,SQL,ID]);
    InternalUpdateEvent(seUpdate,TableModelIndex,ID,SentData,nil);
  end;
end;

function TSQLRestServerDB.MainEngineUpdateBlob(TableModelIndex: integer; aID: TID;
  BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean;
var SQL: RawUTF8;
    AffectedField: TSQLFieldBits;
    Props: TSQLRecordProperties;
begin
  result := false;
  if (aID<0) or (TableModelIndex<0) or not BlobField^.IsBlob then
     exit;
  Props := Model.TableProps[TableModelIndex].Props;
  try
    FormatUTF8('UPDATE % SET %=? WHERE RowID=?',[Props.SQLTableName,BlobField^.Name],SQL);
    DB.Lock(SQL); // UPDATE for a blob field -> no JSON cache flush, but UI refresh
    try
      GetAndPrepareStatement(SQL,true);
      try
        if BlobData='' then
          fStatement^.BindNull(1) else
          fStatement^.BindBlob(1,BlobData);
        fStatement^.Bind(2,aID);
        repeat until fStatement^.Step<>SQLITE_ROW; // Execute
        GetAndPrepareStatementRelease(nil,FormatToShort('stored % in ID=%',
          [KB(BlobData),aID]),true);
        result := true;
      except
        on E: Exception do
          GetAndPrepareStatementRelease(E);
      end;
    finally
      DB.UnLock;
    end;
    Props.FieldBitsFromBlobField(BlobField,AffectedField);
    InternalUpdateEvent(seUpdateBlob,TableModelIndex,aID,'',@AffectedField);
  except
    on ESQLite3Exception do
      result := false;
  end;
end;

function TSQLRestServerDB.MainEngineUpdateFieldIncrement(TableModelIndex: integer;
  ID: TID; const FieldName: RawUTF8; Increment: Int64): boolean;
var Props: TSQLRecordProperties;
    Value: Int64;
begin
  result := false;
  if (TableModelIndex<0) or (FieldName='') then
    exit;
  Props := Model.TableProps[TableModelIndex].Props;
  if Props.Fields.IndexByName(FieldName)<0 then
    Exit;
  if InternalUpdateEventNeeded(TableModelIndex) or
     (Props.RecordVersionField<>nil) then
    result := OneFieldValue(Props.Table,FieldName,'ID=?',[],[ID],Value) and
              UpdateField(Props.Table,ID,FieldName,[Value+Increment]) else
    result := RecordCanBeUpdated(Props.Table,ID,seUpdate) and
      ExecuteFmt('UPDATE % SET %=%+:(%): WHERE ID=:(%):',
        [Props.SQLTableName,FieldName,FieldName,Increment,ID]);
end;

function TSQLRestServerDB.MainEngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
var Props: TSQLRecordProperties;
    WhereID,RecordVersion: TID;
    i: integer;
    JSON,IDs: RawUTF8;
    ID: TIDDynArray;
begin
  result := false;
  if (TableModelIndex<0) or (SetFieldName='') then
    exit;
  Props := Model.TableProps[TableModelIndex].Props;
  if Props.Fields.IndexByName(SetFieldName)<0 then
    Exit;
  if IsRowID(pointer(WhereFieldName)) then begin
    WhereID := GetInt64(Pointer(WhereValue));
    if WhereID<=0 then
      exit;
  end else
    if Props.Fields.IndexByName(WhereFieldName)<0 then
      exit else
      WhereID := 0;
  if InternalUpdateEventNeeded(TableModelIndex) or
     (Props.RecordVersionField<>nil) then begin
    if WhereID>0 then begin
      SetLength(ID,1);
      ID[0] := WhereID;
    end else
      if not InternalExecute(FormatUTF8('select RowID from % where %=:(%):',
         [Props.SQLTableName,WhereFieldName,WhereValue]),true,nil,nil,@ID) then
        exit else
        if ID=nil then begin
          result := true; // nothing to update, but return success
          exit;
        end;
    for i := 0 to high(ID) do
      if not RecordCanBeUpdated(Props.Table,ID[i],seUpdate) then
        exit;
    if Length(ID)=1 then
      if Props.RecordVersionField=nil then
        result := ExecuteFmt('UPDATE % SET %=:(%): WHERE RowID=:(%):',
          [Props.SQLTableName,SetFieldName,SetValue,ID[0]]) else
        result := ExecuteFmt('UPDATE % SET %=:(%):,%=:(%): WHERE RowID=:(%):',
          [Props.SQLTableName,SetFieldName,SetValue,
           Props.RecordVersionField.Name,RecordVersionCompute,ID[0]]) else begin
      IDs := Int64DynArrayToCSV(pointer(ID),length(ID));
      if Props.RecordVersionField=nil then
        result := ExecuteFmt('UPDATE % SET %=% WHERE RowID IN (%)',
          [Props.SQLTableName,SetFieldName,SetValue,IDs]) else begin
        RecordVersion := RecordVersionCompute;
        result := ExecuteFmt('UPDATE % SET %=%,%=% WHERE RowID IN (%)',
          [Props.SQLTableName,SetFieldName,SetValue,
           Props.RecordVersionField.Name,RecordVersion,IDs]);
      end;
    end;
    if not result then
      exit;
    JSONEncodeNameSQLValue(SetFieldName,SetValue,JSON);
    for i := 0 to high(ID) do
      InternalUpdateEvent(seUpdate,TableModelIndex,ID[i],JSON,nil);
  end else
    if (WhereID>0) and not RecordCanBeUpdated(Props.Table,WhereID,seUpdate) then
      exit else // limitation: will only check for update when RowID is provided
      result := ExecuteFmt('UPDATE % SET %=:(%): WHERE %=:(%):',
        [Props.SQLTableName,SetFieldName,SetValue,WhereFieldName,WhereValue]);
end;

function TSQLRestServerDB.UpdateBlobFields(Value: TSQLRecord): boolean;
var Static: TSQLRest;
    SQL: RawUTF8;
    TableModelIndex,f: integer;
    data: TSQLVar;
    size: Int64;
    temp: RawByteString;
begin
  result := false;
  if Value=nil then
    exit;
  TableModelIndex := Model.GetTableIndexExisting(PSQLRecordClass(Value)^);
  Static := GetStaticTableIndex(TableModelIndex);
  if Static<>nil then
    result := Static.UpdateBlobFields(Value) else
    if (DB<>nil) and (Value.ID>0) and (PSQLRecordClass(Value)^<>nil) then
    with Model.TableProps[TableModelIndex].Props do
    if BlobFields<>nil then begin
      FormatUTF8('UPDATE % SET % WHERE ROWID=?',[SQLTableName,SQLTableUpdateBlobFields],SQL);
      DB.Lock(SQL); // UPDATE for all blob fields -> no cache flush, but UI refresh
      try
        GetAndPrepareStatement(SQL,true);
        try
          size := 0;
          for f := 1 to length(BlobFields) do begin
            BlobFields[f-1].GetFieldSQLVar(Value,data,temp); // OK for all blobs
            if data.VType=ftBlob then begin
              fStatement^.Bind(f,data.VBlob,data.VBlobLen);
              inc(size,data.VBlobLen);
            end else
              fStatement^.BindNull(f); // e.g. Value was ''
          end;
          fStatement^.Bind(length(BlobFields)+1,Value.ID);
          repeat until fStatement^.Step<>SQLITE_ROW; // Execute
          GetAndPrepareStatementRelease(nil,FormatToShort('stored % in ID=%',
            [KB(size),Value.ID]),true);
          result := true;
        except
          on E: Exception do
            GetAndPrepareStatementRelease(E);
        end;
      finally
        DB.UnLock;
      end;
      InternalUpdateEvent(seUpdateBlob,TableModelIndex,Value.ID,'',@FieldBits[sftBlob]);
    end else
      result := true; // as TSQLRest.UpdateblobFields()
end;

procedure TSQLRestServerDB.Commit(SessionID: cardinal; RaiseException: boolean);
begin
  inherited Commit(SessionID,RaiseException);
  // reset fTransactionActive + write all TSQLVirtualTableJSON
  try
    DB.Commit; // will call DB.Lock
  except
    on Exception do
      if RaiseException then
        raise;  // default RaiseException=false will just ignore the exception
  end;
end;

procedure TSQLRestServerDB.RollBack(SessionID: cardinal);
begin
  inherited RollBack(SessionID); // reset TSQLRestServerDB.fTransactionActive flag
  try
    DB.RollBack; // will call DB.Lock
  except
    on ESQLite3Exception do
      ; // just catch exception
  end;
end;

function TSQLRestServerDB.TransactionBegin(aTable: TSQLRecordClass; SessionID: cardinal): boolean;
begin
  result := not DB.TransactionActive and inherited TransactionBegin(aTable,SessionID);
  if not result then
    exit; // fTransactionActive flag was already set
  try
    DB.TransactionBegin; // will call DB.Lock
  except
    on ESQLite3Exception do
      result := false;
  end;
end;

{$ifndef KYLIX3}

function TSQLRestServerDB.Backup(Dest: TStream): boolean;
{$ifndef WITHUNSAFEBACKUP} // deprecated - use DB.BackupBackground() instead
begin
  result := false;
end;
{$else}
var Source: TFileStream;
    Closed: boolean;
    user_version: cardinal;
begin
  result := false;
  if (Self=nil) or (DB=nil) then
    exit;
  user_version := DB.user_version;
  DB.LockAndFlushCache;
  try
    try
      fStatementCache.ReleaseAllDBStatements;
      // perform a VACCUM to recreate the database content
      EngineExecute('VACUUM');
      Closed := false;
      try
        Closed := DB.DBClose=SQLITE_OK;
        // compress the database content file
        Source := FileStreamSequentialRead(DB.FileName);
        try
          Dest.CopyFrom(Source,0);  // Count=0 for whole stream copy
          result := true;
        finally
          Source.Free;
        end;
      finally
        if Closed then begin
          // reopen the database if was previously closed
          DB.DBOpen;
          // register functions and modules
          InitializeEngine;
          // register virtual tables
          CreateMissingTables(user_version,fCreateMissingTablesOptions);
        end;
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: Exception do
      result := false;
  end;
end;
{$endif}

function TSQLRestServerDB.BackupGZ(const DestFileName: TFileName;
  CompressionLevel: integer): boolean;
{$ifndef WITHUNSAFEBACKUP} // deprecated - use DB.BackupBackground() instead
begin
  result := false;
end;
{$else}
var D,Z: TStream;
begin
  try
    D := TFileStream.Create(DestFileName,fmCreate);
    try
      Z := TSynZipCompressor.Create(D,CompressionLevel,szcfGZ);
      try
        {$WARN SYMBOL_DEPRECATED OFF} // BackupGZ() itself is marked deprecated
        result := Backup(Z);
        {$WARN SYMBOL_DEPRECATED ON}
      finally
        Z.Free;
      end;
    finally
      D.Free;
    end;
  except
    result := false;
  end;
end;
{$endif}

{$endif KYLIX3}

function TSQLRestServerDB.RestoreGZ(const BackupFileName: TFileName): boolean;
{$ifndef WITHUNSAFEBACKUP} // deprecated - use DB.BackupBackground() instead
begin
  result := false;
end;
{$else}
begin
  try
    with TSynMemoryStreamMapped.Create(BackupFileName) do
    try
      result := Restore(GZRead(Memory,Size));
    finally
      Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;
{$endif}

function TSQLRestServerDB.Restore(const ContentToRestore: RawByteString): boolean;
{$ifndef WITHUNSAFEBACKUP} // deprecated - use DB.BackupBackground() instead
begin
  result := false;
end;
{$else}
var BackupFileName: TFileName;
    user_version: cardinal;
begin
  result := false;
  if (Self=nil) or (DB=nil) or
     not IdemPChar(pointer(ContentToRestore),'SQLITE FORMAT 3') then
    exit; // invalid restore content
  user_version := DB.user_version;
  DB.LockAndFlushCache;
  try
    try
      fStatementCache.ReleaseAllDBStatements;
      if DB.DBClose<>SQLITE_OK then
        exit; // impossible to close DB.FileName (some statement may be opened)
      BackupFileName := ChangeFileExt(DB.FileName,'.bak');
      DeleteFile(BackupFileName);
      try
        {$ifdef MSWINDOWS}
        if MoveFile(pointer(DB.FileName),pointer(BackupFileName)) then
        {$else}
        if RenameFile(DB.FileName,BackupFileName) then
        {$endif}
          if FileFromString(ContentToRestore,DB.FileName,true) and
             (StringFromFile(DB.FileName)=ContentToRestore) then
            result := (DB.DBOpen=SQLITE_OK);
      finally
        if result then
          DeleteFile(BackupFileName) else begin
          // on error, restore previous db file
          DeleteFile(DB.FileName);
          {$ifdef MSWINDOWS}
          MoveFile(pointer(BackupFileName),pointer(DB.FileName));
          {$else}
          RenameFile(BackupFileName,DB.FileName);
          {$endif}
          DB.DBOpen; // always reopen the database
        end;
        // register functions and modules
        InitializeEngine;
        // register virtual tables
        CreateMissingTables(user_version,fCreateMissingTablesOptions);
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: Exception do
      result := false;
  end;
end;
{$endif}

procedure TSQLRestServerDB.AdministrationExecute(const DatabaseName,SQL: RawUTF8;
  var result: TServiceCustomAnswer);
var new,cmd,fn: RawUTF8;
    bfn: TFileName;
    compress: boolean;
    i: integer;
begin
  inherited AdministrationExecute(DatabaseName,SQL,result);
  if (SQL<>'') and (SQL[1]='#') then begin
    case IdemPCharArray(@SQL[2],['VERSION','HELP','DB','BACKUP']) of
    0: new := FormatUTF8('"sqlite3":?}',[],[sqlite3.Version],true);
    1: begin
      result.Content[length(result.Content)] := '|';
      result.Content := result.Content+'#db [*/filename]|#backup [filename]"';
    end;
    2: begin
      split(SQL,' ',cmd,fn);
      if fn='' then begin
        result.Content := ObjectToJSON(DB,[woFullExpand]);
        if fStaticData<>nil then begin
          for i := 0 to high(fStaticData) do
            if fStaticData[i]<>nil then
              new := new+ObjectToJSON(fStaticData[i],[woFullExpand])+',';
          if new<>'' then begin
            new[length(new)] := ']';
            new := '"StaticTables":['+new+'}';
          end;
        end;
      end else
        AdministrationExecuteGetFiles(ExtractFilePath(DB.FileName),
          '*.db;*.db3;*.dbs',fn,result); // *.dbs includes *.dbsynlz
    end;
    3: begin
      split(SQL,' ',cmd,fn);
      if fn='' then
        FormatUTF8('% %',[NowToString(false),ChangeFileExt(DB.FileNameWithoutPath,'.dbsynlz')],fn);
      if (fn<>' ') and (PosEx('..',fn)=0) then begin
        bfn := UTF8ToString(fn);
        if ExtractFilePath(bfn)='' then // put in local data folder if not set
          bfn := ExtractFilePath(DB.FileName)+bfn;
        compress := GetFileNameExtIndex(bfn,'dbsynlz')=0;
        if DB.BackupBackground(bfn,4*1024,1,nil,compress) then // 4*1024*4096=16MB step
          result.Content := JsonEncode(['started',bfn,'compress',compress]) else
          result.Content := '"Backup failed to start"';
      end;
    end;
    else exit;
    end;
    if new<>'' then begin
      if result.Content='' then
        result.Content := '{' else
        result.Content[length(result.Content)] := ',';
      result.Content := result.Content+new;
    end;
  end;
end;

procedure TSQLRestServerDB.FlushInternalDBCache;
begin
  inherited;
  if DB=nil then
    exit;
  DB.Lock;
  try
    DB.CacheFlush;
  finally
    DB.UnLock;
  end;
end;

function TSQLRestServerDB.InternalBatchStart(
  Method: TSQLURIMethod; BatchOptions: TSQLRestBatchOptions): boolean;
begin
  result := false; // means BATCH mode not supported
  if method=mPOST then begin // POST=ADD=INSERT -> MainEngineAdd() to fBatchValues[]
    if (fBatchMethod<>mNone) or (fBatchValuesCount<>0) or (fBatchIDCount<>0) then
      raise EORMBatchException.CreateUTF8('%.InternalBatchStop should have been called',[self]);
    fBatchMethod := method;
    fBatchOptions := BatchOptions;
    fBatchTableIndex := -1;
    fBatchIDMax := 0; // MainEngineAdd() will search for max(id)
    result := true; // means BATCH mode is supported
  end;
end;

procedure TSQLRestServerDB.InternalBatchStop;
const MAX_PARAMS = 500; // pragmatic value (theoritical limit is 999)
var ndx,f,r,prop,fieldCount,valuesCount,
    rowCount,valuesFirstRow: integer;
    P: PUTF8Char;
    DecodeSaved,UpdateEventNeeded: boolean;
    Fields, Values: TRawUTF8DynArray;
    ValuesNull: TByteDynArray;
    Types: TSQLDBFieldTypeDynArray;
    SQL: RawUTF8;
    Props: TSQLRecordProperties;
    Decode: TJSONObjectDecoder;
    tmp: TSynTempBuffer;
begin
  if (fBatchValuesCount=0) or (fBatchTableIndex<0) then
    exit; // nothing to add
  if fBatchMethod<>mPOST then
    raise EORMBatchException.CreateUTF8('%.InternalBatchStop: BatchMethod=%',
      [self,ToText(fBatchMethod)^]);
  try
    if fBatchValuesCount<>fBatchIDCount then
      raise EORMBatchException.CreateUTF8('%.InternalBatchStop(*Count?)',[self]);
    UpdateEventNeeded := InternalUpdateEventNeeded(fBatchTableIndex);
    Props := fModel.Tables[fBatchTableIndex].RecordProps;
    if fBatchValuesCount=1 then begin // handle single record insert
      Decode.Decode(fBatchValues[0],nil,pInlined,fBatchID[0]);
      if Props.RecordVersionField<>nil then
        InternalRecordVersionHandle(
          soInsert,fBatchTableIndex,Decode,Props.RecordVersionField);
      SQL := 'INSERT INTO '+Props.SQLTableName+Decode.EncodeAsSQL(False)+';';
      if not InternalExecute(SQL,true) then // just like ESQLite3Exception below
        raise EORMBatchException.CreateUTF8('%.InternalBatchStop failed on %', [self, SQL]);
      if UpdateEventNeeded then
        InternalUpdateEvent(seAdd,fBatchTableIndex,fBatchID[0],fBatchValues[0],nil);
      exit;
    end;
    DecodeSaved := true;
    valuesCount := 0;
    rowCount := 0;
    valuesFirstRow := 0;
    SetLength(ValuesNull,(MAX_PARAMS shr 3)+1);
    SetLength(Values,32);
    Fields := nil; // makes compiler happy
    fieldCount := 0;
    ndx := 0;
    repeat
      repeat
        // decode a row
        if DecodeSaved then
        try
          if UpdateEventNeeded then begin
            tmp.Init(fBatchValues[ndx]);
            P := tmp.buf;
          end else
            P := pointer(fBatchValues[ndx]);
          if P=nil then
            raise EORMBatchException.CreateUTF8(
              '%.InternalBatchStop: fBatchValues[%]=""',[self,ndx]);
          while P^ in [#1..' ','{','['] do inc(P);
          Decode.Decode(P,nil,pNonQuoted,fBatchID[ndx]);
          if Props.RecordVersionField<>nil then
            InternalRecordVersionHandle(
              soInsert,fBatchTableIndex,Decode,Props.RecordVersionField);
          inc(ndx);
          DecodeSaved := false;
        finally
          if UpdateEventNeeded then
            tmp.Done;
        end;
        if Fields=nil then begin
          Decode.AssignFieldNamesTo(Fields);
          fieldCount := Decode.FieldCount;
          SQL := Decode.EncodeAsSQLPrepared(Props.SQLTableName,soInsert,'',fBatchOptions);
          SetLength(Types,fieldCount);
          for f := 0 to fieldCount-1 do begin
            prop := Props.Fields.IndexByNameOrExcept(Decode.FieldNames[f]);
            if prop<0 then // RowID
              Types[f] := ftInt64 else
              Types[f] := Props.Fields.List[prop].SQLDBFieldType;
          end;
        end else
          if not Decode.SameFieldNames(Fields) then
            break else // this item would break the SQL statement
          if valuesCount+fieldCount>MAX_PARAMS then
            break; // this item would bound too many params
        // if we reached here, we can add this row to Values[]
        if valuesCount+fieldCount>length(Values) then
          SetLength(Values,MAX_PARAMS);
        for f := 0 to fieldCount-1 do
          if Decode.FieldTypeApproximation[f]=ftaNull then
            SetBitPtr(pointer(ValuesNull),valuesCount+f) else
            Values[valuesCount+f] := Decode.FieldValues[f];
        inc(ValuesCount,fieldCount);
        inc(rowCount);
        DecodeSaved := true;
      until ndx=fBatchValuesCount;
      // INSERT Values[] into the DB
      DB.LockAndFlushCache;
      try
        try
          FormatUTF8('% multi %',[rowCount,SQL],fStatementSQL);
          if rowCount>1 then
            SQL := SQL+','+CSVOfValue('('+CSVOfValue('?',fieldCount)+')',rowCount-1);
          fStatementGenericSQL := SQL; // full log on error
          PrepareStatement((rowCount<5) or (valuesCount+fieldCount>MAX_PARAMS));
          prop := 0;
          for f := 0 to valuesCount-1 do begin
            if GetBitPtr(pointer(ValuesNull),f) then
              fStatement^.BindNull(f+1) else
              case Types[prop] of
              ftInt64:
                fStatement^.Bind(f+1,GetInt64(pointer(Values[f])));
              ftDouble, ftCurrency:
                fStatement^.Bind(f+1,GetExtended(pointer(Values[f])));
              ftDate, ftUTF8:
                fStatement^.Bind(f+1,Values[f]);
              ftBlob:
                fStatement^.BindBlob(f+1,Values[f]);
              end;
            inc(prop);
            if prop=fieldCount then
              prop := 0;
          end;
          repeat until fStatement^.Step<>SQLITE_ROW; // ESQLite3Exception on error
          if UpdateEventNeeded then
            for r := valuesFirstRow to valuesFirstRow+rowCount-1 do
              InternalUpdateEvent(seAdd,fBatchTableIndex,fBatchID[r],fBatchValues[r],nil);
          inc(valuesFirstRow,rowCount);
          GetAndPrepareStatementRelease;
        except
          on E: Exception do begin
            GetAndPrepareStatementRelease(E);
            raise;
          end;
        end;
      finally
        DB.UnLock;
      end;
      FillcharFast(ValuesNull[0],(ValuesCount shr 3)+1,0);
      ValuesCount := 0;
      rowCount := 0;
      Fields := nil; // force new SQL statement and Values[]
    until DecodeSaved and (ndx=fBatchValuesCount);
    if valuesFirstRow<>fBatchValuesCount then
      raise EORMBatchException.CreateUTF8('%.InternalBatchStop(valuesFirstRow)',[self]);
  finally
    fBatchMethod := mNone;
    fBatchValuesCount := 0;
    fBatchValues := nil;
    fBatchIDCount := 0;
    fBatchID := nil;
  end;
end;


{ TSQLRestClientDB }

constructor TSQLRestClientDB.Create(aClientModel, aServerModel: TSQLModel; aDB: TSQLDataBase;
   aServerClass: TSQLRestServerDBClass; aHandleUserAuthentication: boolean);
begin
  aDB.UseCache := true;      // we better use caching in this JSON oriented use
  inherited Create(aClientModel);
  if aServerModel=nil then
    aServerModel := TSQLModel.Create(aClientModel); // clone from client
  // next line will create aModel tables if necessary
  fOwnedServer := aServerClass.Create(aServerModel,aDB,aHandleUserAuthentication);
  fServer := fOwnedServer;
  fServer.NoAJAXJSON := true; // use smaller JSON size in this local use (never AJAX)
end;

constructor TSQLRestClientDB.Create(aClientModel, aServerModel: TSQLModel;
  const aDBFileName: TFileName; aServerClass: TSQLRestServerDBClass;
  aHandleUserAuthentication: boolean; const aPassword: RawUTF8;
  aDefaultCacheSize: integer);
begin
  fOwnedDB := TSQLDataBase.Create(aDBFileName,aPassword,0,aDefaultCacheSize);
  Create(aClientModel,aServerModel,fOwnedDB,aServerClass,aHandleUserAuthentication);
end;

constructor TSQLRestClientDB.Create(aRunningServer: TSQLRestServerDB);
var ClientModel: TSQLModel;
begin
  if aRunningServer=nil then
    raise EORMException.Create('TSQLRestClientDB.Create(nil)');
  ClientModel := TSQLModel.Create(aRunningServer.Model);
  ClientModel.Owner := Self; // auto-free ClientModel in TSQLRest.Destroy
  inherited Create(ClientModel);
  fServer := aRunningServer; // leave fOwnedServer=nil
end;

destructor TSQLRestClientDB.Destroy;
var M: TSQLModel;
begin
  try
    inherited Destroy; // UnLock records + SessionClose
  finally
    if fOwnedServer<>nil then begin
      if fServer=nil then
        M := nil else
        M := fServer.Model;
      if (M<>nil) and (M.Owner<>nil) then
        M := nil; // free associated model only if it's owned by nobody
      try
        FreeAndNil(fOwnedServer);
        fServer := nil;
      finally
        M.Free;
        fOwnedDB.Free;
      end;
    end;
  end;
end;

function TSQLRestClientDB.getDB: TSQLDataBase;
begin
  result := fServer.DB;
end;

function TSQLRestClientDB.List(const Tables: array of TSQLRecordClass;
  const SQLSelect, SQLWhere: RawUTF8): TSQLTableJSON;
var aSQL: RawUTF8;
    n: integer;
begin
  result := nil;
  n := length(Tables);
  if (self<>nil) and (n>0) then
  try // will use JSON cache if available:
    aSQL := Model.SQLFromSelectWhere(Tables,SQLSelect,SQLWhere);
    if n=1 then
      // InternalListJSON will handle both static and DB tables
      result := fServer.ExecuteList(Tables,aSQL) else
      // we access localy the DB -> TSQLTableDB handle Tables parameter
      result := TSQLTableDB.Create(fServer.DB,Tables,aSQL,not fServer.NoAJAXJSON);
    if fServer.DB.InternalState<>nil then
      result.InternalState := fServer.DB.InternalState^;
  except
    on ESQLite3Exception do
      result := nil;
  end;
end;

procedure TSQLRestClientDB.InternalURI(var call: TSQLRestURIParams);
begin
  if fInternalHeader='' then
    fInternalHeader := 'RemoteIP: 127.0.0.1'#13#10'ConnectionID: '+PointerToHex(self);
  AddToCSV(fInternalHeader,call.InHead,#13#10);
  call.RestAccessRights := @FULL_ACCESS_RIGHTS;
  fServer.URI(call);
  if (call.OutInternalState=0) and (fServer.DB.InternalState<>nil) then
    call.OutInternalState := fServer.DB.InternalState^; // manual update if necessary
end;

function TSQLRestClientDB.InternalCheckOpen: boolean;
begin
  result := true;
end;

procedure TSQLRestClientDB.InternalClose;
begin
end;


{ TSQLVirtualTableModuleSQLite3 }

procedure Notify(const Format: RawUTF8; const Args: array of const);
begin
  {$ifdef WITHLOG}
  SynSQLite3Log.DebuggerNotify(sllWarning,Format,Args);
  {$endif}
end;

function TSQLVirtualTableModuleSQLite3.FileName(const aTableName: RawUTF8): TFileName;
begin
  if FilePath<>'' then
    // if a file path is specified (e.g. by SynDBExplorer) -> always use this
    result := inherited FileName(aTableName) else
  if SameText(DB.FileName,SQLITE_MEMORY_DATABASE_NAME) then
    // in-memory databases virtual tables should remain in memory
    result := '' else
    // change file path to current DB folder
    result := ExtractFilePath(DB.FileName)+ExtractFileName(inherited FileName(aTableName));
end;

function vt_Create(DB: TSQLite3DB; pAux: Pointer;
  argc: Integer; const argv: PPUTF8CharArray;
  var ppVTab: PSQLite3VTab; var pzErr: PUTF8Char): Integer; cdecl;
var Module: TSQLVirtualTableModuleSQLite3 absolute pAux;
    Table: TSQLVirtualTable;
    Structure: RawUTF8;
    ModuleName: RawUTF8;
begin
  if Module<>nil then
    ModuleName := Module.ModuleName;
  if (Module=nil) or (Module.DB.DB<>DB) or
     (StrIComp(pointer(ModuleName),argv[0])<>0) then begin
    Notify('vt_Create(%<>%)',[argv[0],ModuleName]);
    result := SQLITE_ERROR;
    exit;
  end;
  ppVTab := sqlite3.malloc(sizeof(TSQLite3VTab));
  if ppVTab=nil then begin
    result := SQLITE_NOMEM;
    exit;
  end;
  FillcharFast(ppVTab^,sizeof(ppVTab^),0);
  try
    Table := Module.TableClass.Create(Module,RawUTF8(argv[2]),argc-3,@argv[3]);
  except
    on E: Exception do begin
      ExceptionToSqlite3Err(E,pzErr);
      sqlite3.free_(ppVTab);
      result := SQLITE_ERROR;
      exit;
    end;
  end;
  Structure := Table.Structure;
  result := sqlite3.declare_vtab(DB,pointer(Structure));
  if result<>SQLITE_OK then begin
    Notify('vt_Create(%) declare_vtab(%)',[ModuleName,Structure]);
    Table.Free;
    sqlite3.free_(ppVTab);
    result := SQLITE_ERROR;
  end else
    ppVTab^.pInstance := Table;
end;

function vt_Disconnect(pVTab: PSQLite3VTab): Integer; cdecl;
begin
  TSQLVirtualTable(pvTab^.pInstance).Free;
  sqlite3.free_(pVTab);
  result := SQLITE_OK;
end;

function vt_Destroy(pVTab: PSQLite3VTab): Integer; cdecl;
begin
  if TSQLVirtualTable(pvTab^.pInstance).Drop then
    result := SQLITE_OK else begin
    Notify('vt_Destroy',[]);
    result := SQLITE_ERROR;
  end;
  vt_Disconnect(pVTab); // release memory
end;

function vt_BestIndex(var pVTab: TSQLite3VTab; var pInfo: TSQLite3IndexInfo): Integer;
  cdecl;
const COST: array[TSQLVirtualTablePreparedCost] of double = (1E10,1E8,10,1);
      // costFullScan, costScanWhere, costSecondaryIndex, costPrimaryIndex
var Prepared: PSQLVirtualTablePrepared;
    Table: TSQLVirtualTable;
    i, n: Integer;
begin
  result := SQLITE_ERROR;
  Table := TSQLVirtualTable(pvTab.pInstance);
  if (cardinal(pInfo.nOrderBy)>MAX_SQLFIELDS) or
     (cardinal(pInfo.nConstraint)>MAX_SQLFIELDS) then begin
    Notify('nOrderBy=% nConstraint=%',[pInfo.nOrderBy,pInfo.nConstraint]);
    exit; // avoid buffer overflow
  end;
  Prepared := sqlite3.malloc(sizeof(TSQLVirtualTablePrepared));
  try
    // encode the incoming parameters into Prepared^ record
    Prepared^.WhereCount := pInfo.nConstraint;
    Prepared^.EstimatedCost := costFullScan;
    for i := 0 to pInfo.nConstraint-1 do
      with Prepared^.Where[i], pInfo.aConstraint^[i] do begin
        OmitCheck := False;
        Value.VType := ftUnknown;
        if usable then begin
          Column := iColumn;
          case op of
            SQLITE_INDEX_CONSTRAINT_EQ:    Operation := soEqualTo;
            SQLITE_INDEX_CONSTRAINT_GT:    Operation := soGreaterThan;
            SQLITE_INDEX_CONSTRAINT_LE:    Operation := soLessThanOrEqualTo;
            SQLITE_INDEX_CONSTRAINT_LT:    Operation := soLessThan;
            SQLITE_INDEX_CONSTRAINT_GE:    Operation := soGreaterThanOrEqualTo;
            SQLITE_INDEX_CONSTRAINT_MATCH: Operation := soBeginWith;
            else Column := VIRTUAL_TABLE_IGNORE_COLUMN; // unhandled operator
          end;
        end else
          Column := VIRTUAL_TABLE_IGNORE_COLUMN;
      end;
    Prepared^.OmitOrderBy := false;
    if pInfo.nOrderBy>0 then begin
      assert(sizeof(TSQLVirtualTablePreparedOrderBy)=sizeof(TSQLite3IndexOrderBy));
      Prepared^.OrderByCount := pInfo.nOrderBy;
      MoveFast(pInfo.aOrderBy^[0],Prepared^.OrderBy[0],pInfo.nOrderBy*sizeof(Prepared^.OrderBy[0]));
    end else
      Prepared^.OrderByCount := 0;
    // perform the index query
    if not Table.Prepare(Prepared^) then
      exit;
    // update pInfo and store Prepared into pInfo.idxStr for vt_Filter()
    n := 0;
    for i := 0 to pInfo.nConstraint-1 do
    if Prepared^.Where[i].Value.VType<>ftUnknown then begin
      if i<>n then // expression needed for Search() method to be moved at [n]
        MoveFast(Prepared^.Where[i],Prepared^.Where[n],sizeof(Prepared^.Where[i]));
      inc(n);
      pInfo.aConstraintUsage[i].argvIndex := n;
      pInfo.aConstraintUsage[i].omit := Prepared^.Where[i].OmitCheck;
    end;
    Prepared^.WhereCount := n; // will match argc in vt_Filter()
    if Prepared^.OmitOrderBy then
      pInfo.orderByConsumed := 1 else
      pInfo.orderByConsumed := 0;
    pInfo.estimatedCost := COST[Prepared^.EstimatedCost];
    if sqlite3.VersionNumber>=3008002000 then // starting with SQLite 3.8.2
      case Prepared^.EstimatedCost of
      costFullScan:
        pInfo.estimatedRows := Prepared^.EstimatedRows;
      costScanWhere: // estimate a WHERE clause is a slight performance gain
        pInfo.estimatedRows := Prepared^.EstimatedRows shr 1;
      costSecondaryIndex:
        pInfo.estimatedRows := 10;
      costPrimaryIndex:
        pInfo.estimatedRows := 1;
      else raise EORMException.Create('vt_BestIndex: unexpected EstimatedCost');
      end;
    pInfo.idxStr := pointer(Prepared);
    pInfo.needToFreeIdxStr := 1; // will do sqlite3.free(idxStr) when needed
    result := SQLITE_OK;
    {$ifdef SQLVIRTUALLOGS}
    if Table.Static is TSQLRestStorageExternal then
      TSQLRestStorageExternal(Table.Static).ComputeSQL(prepared^);
    SQLite3Log.Add.Log(sllDebug,'vt_BestIndex(%) plan=% -> cost=% rows=%',
      [sqlite3.VersionNumber,ord(Prepared^.EstimatedCost),pInfo.estimatedCost,pInfo.estimatedRows]);
    {$endif SQLVIRTUALLOGS}
  finally
    if result<>SQLITE_OK then
      sqlite3.free_(Prepared); // avoid memory leak on error
  end;
end;

function vt_Filter(var pVtabCursor: TSQLite3VTabCursor; idxNum: Integer; const idxStr: PAnsiChar;
   argc: Integer; var argv: TSQLite3ValueArray): Integer;
  cdecl;
var Prepared: PSQLVirtualTablePrepared absolute idxStr; // idxNum is not used
    i: integer;
begin
  result := SQLITE_ERROR;
  if Prepared^.WhereCount<>argc then begin
    Notify('vt_Filter WhereCount=% argc=%',[Prepared^.WhereCount,argc]);
    exit; // invalid prepared array (should not happen)
  end;
  for i := 0 to argc-1 do
    SQlite3ValueToSQLVar(argv[i],Prepared^.Where[i].Value);
  if TSQLVirtualTableCursor(pVtabCursor.pInstance).Search(Prepared^) then
    result := SQLITE_OK else
    Notify('vt_Filter Search()',[]);
end;

function vt_Open(var pVTab: TSQLite3VTab; var ppCursor: PSQLite3VTabCursor): Integer;
  cdecl;
var Table: TSQLVirtualTable;
begin
  ppCursor := sqlite3.malloc(sizeof(TSQLite3VTabCursor));
  if ppCursor=nil then begin
    result := SQLITE_NOMEM;
    exit;
  end;
  Table := TSQLVirtualTable(pvTab.pInstance);
  if (Table=nil) or (Table.Module=nil) or (Table.Module.CursorClass=nil) then begin
    Notify('vt_Open',[]);
    sqlite3.free_(ppCursor);
    result := SQLITE_ERROR;
    exit;
  end;
  ppCursor.pInstance := Table.Module.CursorClass.Create(Table);
  result := SQLITE_OK;
end;

function vt_Close(pVtabCursor: PSQLite3VTabCursor): Integer;
  cdecl;
begin
  TSQLVirtualTableCursor(pVtabCursor^.pInstance).Free;
  sqlite3.free_(pVtabCursor);
  result := SQLITE_OK;
end;

function vt_next(var pVtabCursor: TSQLite3VTabCursor): Integer;
  cdecl;
begin
  if TSQLVirtualTableCursor(pVtabCursor.pInstance).Next then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_Eof(var pVtabCursor: TSQLite3VTabCursor): Integer;
  cdecl;
begin
  if TSQLVirtualTableCursor(pVtabCursor.pInstance).HasData then
    result := 0 else
    result := 1; // reached actual EOF
end;

function vt_Column(var pVtabCursor: TSQLite3VTabCursor; sContext: TSQLite3FunctionContext;
  N: Integer): Integer; cdecl;
var Res: TSQLVar;
begin
  Res.VType := ftUnknown;
  if (N>=0) and TSQLVirtualTableCursor(pVtabCursor.pInstance).Column(N,Res) and
     SQLVarToSQlite3Context(Res,sContext) then
    result := SQLITE_OK else begin
    Notify('vt_Column(%) Res=%',[N,ord(Res.VType)]);
    result := SQLITE_ERROR;
  end;
end;

function vt_Rowid(var pVtabCursor: TSQLite3VTabCursor; var pRowid: Int64): Integer;
  cdecl;
var Res: TSQLVar;
begin
  result := SQLITE_ERROR;
  with TSQLVirtualTableCursor(pVtabCursor.pInstance) do
  if Column(-1,Res) then begin
    case Res.VType of
    ftInt64:    pRowID := Res.VInt64;
    ftDouble:   pRowID := trunc(Res.VDouble);
    ftCurrency: pRowID := trunc(Res.VCurrency);
    ftUTF8:     pRowID := GetInt64(Res.VText);
    else begin
      Notify('vt_Rowid Res=%',[ord(Res.VType)]);
      exit;
    end;
    end;
    result := SQLITE_OK;
  end else
    Notify('vt_Rowid Column',[]);
end;

function vt_Update(var pVTab: TSQLite3VTab;
  nArg: Integer; var ppArg: TSQLite3ValueArray;
  var pRowid: Int64): Integer; cdecl;
var Values: TSQLVarDynArray;
    Table: TSQLVirtualTable;
    RowID0, RowID1: Int64;
    i: integer;
    OK: boolean;
begin // call Delete/Insert/Update methods according to supplied parameters
  Table := TSQLVirtualTable(pvTab.pInstance);
  result := SQLITE_ERROR;
  if (nArg<=0) or (nArg>1024) then
    exit;
  case sqlite3.value_type(ppArg[0]) of
    SQLITE_INTEGER: RowID0 := sqlite3.value_int64(ppArg[0]);
    SQLITE_NULL:    RowID0 := 0;
    else exit; // invalid call
  end;
  if nArg=1 then
    OK := Table.Delete(RowID0) else begin
    case sqlite3.value_type(ppArg[1]) of
      SQLITE_INTEGER: RowID1 := sqlite3.value_int64(ppArg[1]);
      SQLITE_NULL:    RowID1 := 0;
      else exit; // invalid call
    end;
    SetLength(Values,nArg-2);
    for i := 0 to nArg-3 do
      SQlite3ValueToSQLVar(ppArg[i+2],Values[i]);
    if RowID0=0 then
      OK := Table.Insert(RowID1,Values,pRowid) else
      OK := Table.Update(RowID0,RowID1,Values);
  end;
  if OK then
    result := SQLITE_OK else
    Notify('vt_Update(%)',[pRowID]);
end;

function InternalTrans(pVTab: TSQLite3VTab; aState: TSQLVirtualTableTransaction;
  aSavePoint: integer): integer;
begin
  if TSQLVirtualTable(pvTab.pInstance).Transaction(aState,aSavePoint) then
    result := SQLITE_OK else begin
    Notify('Transaction(%,%)',[ToText(aState)^,aSavePoint]);
    result := SQLITE_ERROR;
  end;
end;

function vt_Begin(var pVTab: TSQLite3VTab): Integer; cdecl;
begin
  result := InternalTrans(pVTab,vttBegin,0);
end;

function vt_Commit(var pVTab: TSQLite3VTab): Integer; cdecl;
begin
  result := InternalTrans(pVTab,vttCommit,0);
end;

function vt_RollBack(var pVTab: TSQLite3VTab): Integer; cdecl;
begin
  result := InternalTrans(pVTab,vttRollBack,0);
end;

function vt_Sync(var pVTab: TSQLite3VTab): Integer; cdecl;
begin
  result := InternalTrans(pVTab,vttSync,0);
end;

function vt_SavePoint(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; cdecl;
begin
  result := InternalTrans(pVTab,vttSavePoint,iSavePoint);
end;

function vt_Release(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; cdecl;
begin
  result := InternalTrans(pVTab,vttRelease,iSavePoint);
end;

function vt_RollBackTo(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; cdecl;
begin
  result := InternalTrans(pVTab,vttRollBackTo,iSavePoint);
end;

function vt_Rename(var pVTab: TSQLite3VTab; const zNew: PAnsiChar): Integer; cdecl;
begin
  if TSQLVirtualTable(pvTab.pInstance).Rename(RawUTF8(zNew)) then
    result := SQLITE_OK else begin
    Notify('vt_Rename(%)',[zNew]);
    result := SQLITE_ERROR;
  end;
end;

procedure sqlite3InternalFreeModule(p: pointer); cdecl;
begin
  if (p<>nil) and (TSQLVirtualTableModuleSQLite3(p).fDB<>nil) then
    TSQLVirtualTableModuleSQLite3(p).Free;
end;

procedure TSQLVirtualTableModuleSQLite3.Attach(aDB: TSQLDataBase);
begin
  if aDB=nil then
    raise EBusinessLayerException.CreateFmt('aDB=nil at %s.SetDB()',[ClassName]);
  if fDB<>nil then
    raise EBusinessLayerException.CreateFmt('fDB<>nil at %s.SetDB()',[ClassName]);
  FillCharFast(fModule,sizeof(fModule),0);
  fModule.iVersion := 1;
  fModule.xCreate := vt_Create;
  fModule.xConnect := vt_Create;
  fModule.xBestIndex := vt_BestIndex;
  fModule.xDisconnect := vt_Disconnect;
  fModule.xDestroy := vt_Destroy;
  fModule.xOpen := vt_Open;
  fModule.xClose := vt_Close;
  fModule.xFilter := vt_Filter;
  fModule.xNext := vt_Next;
  fModule.xEof := vt_Eof;
  fModule.xColumn := vt_Column;
  fModule.xRowid := vt_Rowid;
  if vtWrite in Features then begin
    fModule.xUpdate := vt_Update;
    if vtTransaction in Features then begin
      fModule.xBegin := vt_Begin;
      fModule.xSync := vt_Sync;
      fModule.xCommit := vt_Commit;
      fModule.xRollback := vt_RollBack;
    end;
    if vtSavePoint in Features then begin
      fModule.iVersion := 2;
      fModule.xSavePoint := vt_SavePoint;
      fModule.xRelease := vt_Release;
      fModule.xRollBackTo := vt_RollBackTo;
    end;
    fModule.xRename := vt_Rename;
  end;
  sqlite3_check(aDB.DB,sqlite3.create_module_v2(aDB.DB,pointer(fModuleName),fModule,
    self,sqlite3InternalFreeModule)); // raise ESQLite3Exception on error
  fDB := aDB; // mark successfull create_module() for sqlite3InternalFreeModule
end;


{ TSQLVirtualTableModuleServerDB }

constructor TSQLVirtualTableModuleServerDB.Create(
  aClass: TSQLVirtualTableClass; aServer: TSQLRestServer);
begin
  if not aServer.InheritsFrom(TSQLRestServerDB) then
    raise EBusinessLayerException.CreateFmt('%.Create expects a DB Server',[ClassName]);
  inherited;
  Attach(TSQLRestServerDB(aServer).DB);
  // any exception in Attach() will let release the instance by the RTL
end;


{ TSQLRestStorageShardDB }

constructor TSQLRestStorageShardDB.Create(aClass: TSQLRecordClass;
  aServer: TSQLRestServer; aShardRange: TID; aOptions: TSQLRestStorageShardOptions;
  const aShardRootFileName: TFileName; aMaxShardCount: integer;
  aSynchronous: TSQLSynchronousMode; aCacheSizePrevious,aCacheSizeLast: Integer);
begin
  fShardRootFileName := aShardRootFileName;
  fSynchronous := aSynchronous;
  fCacheSizePrevious := aCacheSizePrevious;
  fCacheSizeLast := aCacheSizeLast;
  inherited Create(aClass,aServer,aShardRange,aOptions,aMaxShardCount);
end;

function TSQLRestStorageShardDB.DBFileName(ShardIndex: Integer): TFileName;
begin
  result := Format('%s%.4d.dbs',[fShardRootFileName,fShardOffset+ShardIndex]);
end;

function TSQLRestStorageShardDB.InitNewShard: TSQLRest;
var db: TSQLRestServerDB;
    cachesize: integer;
    sql: TSQLDataBase;
    model: TSQLModel;
begin
  inc(fShardLast);
  model := TSQLModel.Create([fStoredClass],FormatUTF8('shard%',[fShardLast]));
  if fInitShardsIsLast then // last/new .dbs = 2MB cache, previous 1MB only
    cachesize := fCacheSizeLast else
    cachesize := fCacheSizePrevious;
  sql := TSQLDatabase.Create(DBFileName(fShardLast),'',0,cachesize);
  sql.LockingMode := lmExclusive;
  sql.Synchronous := fSynchronous;
  db := TSQLRestServerDB.Create(model,sql,false,true);
  model.Owner := db;
  db.CreateMissingTables;
  result := db;
  SetLength(fShards,fShardLast+1);
  fShards[fShardLast] := result;
end;

procedure TSQLRestStorageShardDB.InitShards;
var f,i,num,first: integer;
    db: TFindFilesDynArray;
    mask: TFileName;
begin
  if fShardRootFileName='' then
    fShardRootFileName := ExeVersion.ProgramFilePath+UTF8ToString(fStoredClass.SQLTableName);
  mask := DBFileName(0);
  i := Pos('0000',mask);
  if i>0 then begin
    system.Delete(mask,i,3);
    mask[i] := '*';
  end else
    mask := fShardRootFileName+'*.dbs';
  db := FindFiles(ExtractFilePath(mask),ExtractFileName(mask),'',{sorted=}true); 
  if db=nil then
    exit; // no existing data
  fShardOffset := -1;
  first := length(db)-integer(fMaxShardCount);
  if first<0 then
    first := 0;
  for f := first to high(db) do begin
    i := Pos('.dbs',db[f].Name);
    if (i<=4) or not TryStrToInt(Copy(db[f].Name,i-4,4),num) then begin
      InternalLog('InitShards(%)?',[db[f].Name],sllWarning);
      continue;
    end;
    if fShardOffset<0 then
      fShardOffset := num;
    dec(num,fShardOffset);
    if not SameText(DBFileName(num),db[f].Name) then
      raise EORMException.CreateUTF8('%.InitShards(%)',[self,db[f].Name]);
    if f = high(db) then
      fInitShardsIsLast := true;
    fShardLast := num-1; // 'folder\root0005.dbs' -> fShardLast := 4
    InitNewShard;        // now fShardLast=5, fShards[5] contains root005.dbs
  end;
  if fShardOffset<0 then
    fShardOffset := 0;
  if Integer(fShardLast)<0 then begin
    InternalLog('InitShards?',sllWarning);
    exit;
  end;
  fInitShardsIsLast := true; // any newly appended .dbs would use 2MB of cache
  fShardLastID := fShards[fShardLast].TableMaxID(fStoredClass);
  if fShardLastID<0 then
    fShardLastID := 0; // no data yet
end;


function RegisterVirtualTableModule(aModule: TSQLVirtualTableClass; aDatabase: TSQLDataBase): TSQLVirtualTableModule;
begin
  result := TSQLVirtualTableModuleSQLite3.Create(aModule,nil);
  try
    TSQLVirtualTableModuleSQLite3(result).Attach(aDatabase);
  except
    on Exception do begin
      result.Free; // should be released by hand here
      raise; // e.g. EBusinessLayerException or ESQLite3Exception
    end;
  end;
end;

initialization
  {$ifdef WITHLOG}
  // all our SynSQlite3 related functions shall log to main TSQLLog
  SynSQLite3Log := TSQLLog;
  {$endif}
  TSQLRestServerDB.RegisterClassNameForDefinition;
end.



