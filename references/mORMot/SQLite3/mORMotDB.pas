/// Virtual Tables for external DB access for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotDB;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2018 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2018
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Maciej Izak (hnb)
  - yoanq

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

  Version 1.15
  - first public release, corresponding to mORMot Framework 1.15

  Version 1.16
  - TSQLRestServerStaticExternal.EngineList method now handles an optional
    integer pointer, to return the count of row data (excluding field names)

  Version 1.17
  - changed column named 'RowID' into 'ID' since it is reserved e.g. in Oracle
  - external direct insert, update or delete actions (i.e. when the
    TSQLRestServerStaticExternal instance is called directly) will now
    flush the low-level SQLite3 DB cache, as expected by the virtual tables
  - added TSQLRestServerStaticExternal.AdaptSQLForEngineList overridden method to
    handle most generic SELECT to by-pass the SQLite3 virtual module for speed
  - added TSQLRestServerStaticExternal.EndCurrentThread overridden method which
    will be called e.g. by TSQLite3HttpServer or TSQLRestServerNamedPipeResponse
    for each terminating threads, to release external connection resource
    (calling TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread method)
  - any direct or virtual-table based insertion to the external database will
    now use a binding matching the exact time of each column: it will e.g. allow
    to support DBMS which does not accept date/time to be supplied as ISO-8601
    text, and make more efficient data conversion (like avoid conversion to
    floating-point from a currency value) - code shared with BATCH mode and newly
    added TSQLRestServerStaticExternal.ExecuteFromJSON() protected method
  - inlined parameters in any SQL query will bind explicitely TDateTime values
    if the parameter is transmitted as DateToSQL() or DateTimeToSQL() TEXT
  - removed TSQLRecordExternal class type, to allow any TSQLRecord (e.g.
    TSQLRecordMany) to be used with VirtualTableExternalRegister() - there was
    indeed no implementation requirement to force a specific class type
  - now create properly UNIQUE fields (i.e. "stored AS_UNIQUE") in external tables
  - handle NULL values for BLOBs as expected

  Version 1.18
  - unit SQLite3DB.pas renamed mORMotDB.pas
  - TSQLRestServerStaticExternal renamed TSQLRestStorageExternal
  - huge performance boost when inserting individual data rows, by maintaining
    the IDs in memory instead of executing "select max(id)" - added new property
    EngineAddUseSelectMaxID to unset this optimization, and alternate
    OnEngineAddComputeID and EngineAddForcedID properties for [201348a0af6]
  - new function VirtualTableExternalRegisterAll(), to register all tables
    of a mORMot model to be handled via a specified database
  - TSQLRestStorageExternal.AdaptSQLForEngineList() will now use the generic
    TSynTableStatement parser - as proposed by [94ff704bb1] - to handle
    aggregation functions like 'select count(*),max(ID) from ...' statements
    directly (virtual behavior for count(*) is to loop through all records,
    which may be slow), 'GROUP BY' clause, and 'IN (...)' or 'IS NULL' / 'IS NOT NULL' where
    clauses, several fields or ASC/DESC attributes in 'ORDER BY' clause - see
    also [e48f87b3db]
  - now TSQLRestStorageExternal will call TSQLRestServer.OnUpdateEvent and
    OnBlobUpdateEvent callbacks, if defined (even in BATCH mode)
  - BatchDelete() will now split its batch statement executed following
    TSQLDBConnectionProperties.BatchMaxSentAtOnce property expectations
  - now TSQLRestStorageExternal won't create any columns for external
    tables with unsupported published property types (sftUnknown or sftMany),
    just like TSQLRecord.GetSQLCreate() method
  - TSQLRestStorageExternal will create sftID/sftRecord/sftEnumerate/sftBoolean
    columns as 32 bit integer instead of 64 bit integer
  - now handles TSQLDBConnectionProperties.ForcedSchemaName as expected
  - fixed issue in TSQLRestStorageExternal.EngineDeleteWhere() when
    calling commands like MyDB.Delete(TSQLMyClass, 'PLU < ?', [20000])
  - TSQLRestStorageExternal.EngineDeleteWhere() will handle more border cases,
    and will split DELETE FROM table WHERE ID IN (....) in several intervals
  - fixed errors when executing JOINed queries (e.g. via FillPrepareMany)
  - fixed ticket [3c41462594] in TSQLRestStorageExternal.ExecuteFromJSON()
  - fixed ticket [9a821d26ee] in TSQLRestStorageExternal.Create() not
    creating any missing field
  - fixed unexpected TSQLRestServer time synchronization to the DB local time in
    TSQLRestStorageExternal.Create - now the reference clock is the ORM server
  - fixed ticket [b109c22750] about SQLite3 cache not flushed after CRUD updates
  - ensure no INDEX is created for SQLite3 which generates an index for ID/RowID
  - ensure DESC INDEX is created for Firebird ID column, as expected for
    faster MAX(ID) execution - see http://www.firebirdfaq.org/faq205
  - fix TSQLRestStorageExternal.CreateSQLMultiIndex() to set ColumnIndexed=TRUE,
    and fixed ticket [929cb6fc3047c5f78b95] by ignoring BLOB fields
  - ensure duplicated indexs are not created on ID primary key column
  - fixed TSQLRestStorageExternal.UpdateBlobFields() to return true
    if no BLOB field is defined, and to proper handle multi-field update
  - fixed ticket [21c2d5ae96] when inserting/updating blob-only table content
  - handle null binding in TSQLRestStorageExternal.ExecuteInlined()
  - added TSQLRestStorageExternal.TableHasRows/TableRowCount overrides
  - added TSQLRestStorageExternal.PrepareInlinedForRows() and
    PrepareDirectForRows() methods to call new ExecutePreparedAndFetchAllAsJSON()
    method of ISQLDBStatement as expected by TSQLDBProxyStatement
  - optimized TSQLRestStorageExternal.UpdateBlobFields()/RetrieveBlobFields()
    methods, updating/retrieving all BLOB fields at once in the same SQL statement
  - added VirtualTableExternalMap() function for easier mapping definition
  - handle TSQLRecordPropertiesMapping.MapAutoKeywordFields for automatic
    maping of field which name conflicts with a SQL keyword - see [7fbbd53966]
  - handle TSQLRecordPropertiesMapping.Options settings to customize tables
    or fields automatic creation on external DB
  - this unit will now set SynDBLog := TSQLLog during its initialization
  - replaced confusing TVarData by a new dedicated TSQLVar memory structure,
    shared with SynDB and mORMot units (includes methods refactoring)

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$ifdef KYLIX3}
  LibC,
  {$endif}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  SynTable, // for TSynTableStatement
  mORMot,
  SynDB;

type
  TSQLRestStorageExternal = class;

  /// event handler called to customize the computation of a new ID
  // - should set Handled=TRUE if a new ID has been computed and returned
  // - Handled=FALSE would let the default ID computation take place
  // - note that execution of this method would be protected by a mutex, so
  // it would be thread-safe
  TOnEngineAddComputeID = function(Sender: TSQLRestStorageExternal; var Handled: Boolean): TID of object;

  /// REST server with direct access to a SynDB-based external database
  // - handle all REST commands, using the external SQL database connection,
  // and prepared statements
  // - is used by TSQLRestServer.URI for faster RESTful direct access
  // - for JOINed SQL statements, the external database is also defined as
  // a SQLite3 virtual table, via the TSQLVirtualTableExternal[Cursor] classes
  TSQLRestStorageExternal = class(TSQLRestStorage)
  protected
    /// values retrieved from fStoredClassProps.ExternalDB settings
    fTableName: RawUTF8;
    fProperties: TSQLDBConnectionProperties;
    fSelectOneDirectSQL, fSelectAllDirectSQL, fSelectTableHasRowsSQL: RawUTF8;
    fRetrieveBlobFieldsSQL, fUpdateBlobfieldsSQL: RawUTF8;
    // ID handling during Add/Insert
    fEngineAddUseSelectMaxID: Boolean;
    fEngineLockedMaxID: TID;
    fOnEngineAddComputeID: TOnEngineAddComputeID;
    fEngineAddForcedID: TID;
    /// external column layout as retrieved by fProperties
    // - used internaly to guess e.g. if the column is indexed
    // - fFieldsExternal[] contains the external table info, and the internal
    // column name is available via fFieldsExternalToInternal[]
    fFieldsExternal: TSQLDBColumnDefineDynArray;
    /// gives the index of each fFieldsExternal[] item in Props.Fields[]
    // - is >=0 for index in Props.Fields[], -1 for RowID/ID, -2 if unknown
    // - use InternalFieldNameToFieldExternalIndex() to convert from column name
    fFieldsExternalToInternal: TIntegerDynArray;
    /// gives the index of each in Props.Fields[]+1 in fFieldsExternal[]
    // - expects [0] of RowID/ID, [1..length(fFieldNames)] for others
    fFieldsInternalToExternal: TIntegerDynArray;
    // multi-thread BATCH process is secured via Lock/UnLock critical section
    fBatchMethod: TSQLURIMethod;
    fBatchCapacity, fBatchCount: integer;
    // BATCH sending uses TEXT storage for direct sending to database driver
    fBatchValues: TRawUTF8DynArray;
    fBatchIDs: TIDDynArray;
    /// get fFieldsExternal[] index using fFieldsExternalToInternal[] mapping
    // - do handle ID/RowID fields and published methods
    function InternalFieldNameToFieldExternalIndex(const InternalFieldName: RawUTF8): integer;
    /// create, prepare and bound inlined parameters to a thread-safe statement
    // - this implementation will call the ThreadSafeConnection virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - should return nil on error, and not raise an exception
    function PrepareInlinedForRows(const aSQL: RawUTF8): ISQLDBStatement;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function PrepareDirectForRows(SQLFormat: PUTF8Char; const Args, Params: array of const): ISQLDBStatement;
    /// create, prepare, bound inlined parameters and execute a thread-safe statement
    // - this implementation will call the ThreadSafeConnection virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - should return nil on error, and not raise an exception
    function ExecuteInlined(const aSQL: RawUTF8; ExpectResults: Boolean): ISQLDBRows; overload;
    /// overloaded method using FormatUTF8() and inlined parameters
    function ExecuteInlined(SQLFormat: PUTF8Char; const Args: array of const; ExpectResults: Boolean): ISQLDBRows; overload;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function ExecuteDirect(SQLFormat: PUTF8Char; const Args, Params: array of const;
      ExpectResults: Boolean): ISQLDBRows;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function ExecuteDirectSQLVar(SQLFormat: PUTF8Char; const Args: array of const;
       var Params: TSQLVarDynArray; const LastIntegerParam: Int64;
       ParamsMatchCopiableFields: boolean): boolean;
    // overridden methods calling the external engine with SQL via Execute
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    function EngineExecute(const aSQL: RawUTF8): boolean; override;
    function EngineLockedNextID: TID; virtual;
    function EngineAdd(TableModelIndex: integer; const SentData: RawUTF8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID; const SentData: RawUTF8): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: Boolean=false; ReturnedRowCount: PPtrInt=nil): RawUTF8; override;
    // BLOBs should be access directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    function EngineSearchField(const FieldName: ShortString;
      const FieldValue: array of const; out ResultID: TIDDynArray): boolean;
    // overridden method returning TRUE for next calls to EngineAdd/Update/Delete
    // will properly handle operations until InternalBatchStop is called
    // BatchOptions is ignored with external DB (syntax are too much specific)
    function InternalBatchStart(Method: TSQLURIMethod;
      BatchOptions: TSQLRestBatchOptions): boolean; override;
    // internal method called by TSQLRestServer.RunBatch() to process fast sending
    // to remote database engine (e.g. Oracle bound arrays or MS SQL Bulk insert)
    procedure InternalBatchStop; override;
    /// called internally by EngineAdd/EngineUpdate/EngineDelete in batch mode
    procedure InternalBatchAdd(const aValue: RawUTF8; const aID: TID);
    /// TSQLRestServer.URI use it for Static.EngineList to by-pass virtual table
    // - overridden method to handle most potential simple queries, e.g. like
    // $ SELECT Field1,RowID FROM table WHERE RowID=... AND/OR/NOT Field2=
    // - change 'RowID' into 'ID' column name, internal field names into
    // mapped external field names ('AS [InternalFieldName]' if needed), and
    // SQLTableName into fTableName
    // - any 'LIMIT #' clause will be changed into the appropriate SQL statement
    // - handle also statements to avoid slow virtual table full scan, e.g.
    // $ SELECT count(*) FROM table
    function AdaptSQLForEngineList(var SQL: RawUTF8): boolean; override;
    /// run INSERT of UPDATE from the corresponding JSON object
    // - Occasion parameter shall be only either soInsert or soUpate
    // - each JSON field will be bound with the proper SQL type corresponding to
    // the real external table columns (e.g. as TEXT for variant)
    // - returns 0 on error, or the Updated/Inserted ID
    function ExecuteFromJSON(const SentData: RawUTF8; Occasion: TSQLOccasion;
      UpdatedID: TID): TID;
    /// compute the INSERT or UPDATE statement as decoded from a JSON object
    function JSONDecodedPrepareToSQL(var Decoder: TJSONObjectDecoder;
      out ExternalFields: TRawUTF8DynArray; out Types: TSQLDBFieldTypeArray;
      Occasion: TSQLOccasion; BatchOptions: TSQLRestBatchOptions): RawUTF8;
    function GetConnectionProperties: TSQLDBConnectionProperties;
  public
    /// initialize the remote database connection
    // - you should not use this, but rather call VirtualTableExternalRegister()
    // - RecordProps.ExternalDatabase will map the associated TSQLDBConnectionProperties
    // - RecordProps.ExternalTableName will retrieve the real full table name,
    // e.g. including any databas<e schema prefix
    constructor Create(aClass: TSQLRecordClass; aServer: TSQLRestServer); override;
    /// finalize the remote database connection
    destructor Destroy; override;
    /// delete a row, calling the external engine with SQL
    // - made public since a TSQLRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TSQLRestServer
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    /// search for a numerical field value
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName: RawUTF8; FieldValue: Int64;
      out ResultID: TIDDynArray): boolean; overload; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName, FieldValue: RawUTF8;
      out ResultID: TIDDynArray): boolean; overload; override;
    /// overridden method for direct external database engine call
    function TableRowCount(Table: TSQLRecordClass): Int64; override;
    /// overridden method for direct external database engine call
    function TableHasRows(Table: TSQLRecordClass): boolean; override;
    /// begin a transaction (implements REST BEGIN Member)
    // - to be used to speed up some SQL statements like Insert/Update/Delete
    // - must be ended with Commit on success
    // - must be aborted with Rollback if any SQL statement failed
    // - return true if no transaction is active, false otherwise
    function TransactionBegin(aTable: TSQLRecordClass; SessionID: cardinal=1): boolean; override;
    /// end a transaction (implements REST END Member)
    // - write all pending SQL statements to the external database
    procedure Commit(SessionID: cardinal=1; RaiseException: boolean=false); override;
    /// abort a transaction (implements REST ABORT Member)
    // - restore the previous state of the database, before the call to TransactionBegin
    procedure RollBack(SessionID: cardinal=1); override;
     /// overridden method for direct external database engine call
    function UpdateBlobFields(Value: TSQLRecord): boolean; override;
     /// overridden method for direct external database engine call
    function RetrieveBlobFields(Value: TSQLRecord): boolean; override;
    /// update a field value of the external database
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    /// update a field value of the external database
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
    /// create one index for all specific FieldNames at once
    // - this method will in fact call the SQLAddIndex method, if the index
    // is not already existing
    // - for databases which do not support indexes on BLOB fields (i.e. all
    // engine but SQLite3), such FieldNames will be ignored
    function CreateSQLMultiIndex(Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8=''): boolean; override;
    /// this method is called by TSQLRestServer.EndCurrentThread method just
    // before a thread is finished to ensure that the associated external DB
    // connection will be released for this thread
    // - this overridden implementation will clean thread-specific connections,
    // i.e. call TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread method
    // - this method shall be called directly, nor from the main thread
    procedure EndCurrentThread(Sender: TThread); override;
    /// reset the internal cache of external table maximum ID
    // - next EngineAdd/BatchAdd will execute SELECT max(ID) FROM externaltable
    // - is a lighter alternative to EngineAddUseSelectMaxID=TRUE, since this
    // method may be used only once, when some records have been inserted into
    // the external database outside this class scope (e.g. by legacy code)
    procedure EngineAddForceSelectMaxID;
    /// compute the SQL query corresponding to a prepared request
    // - can be used internally e.g. for debugging purposes
    function ComputeSQL(const Prepared: TSQLVirtualTablePrepared): RawUTF8;

    /// retrieve the REST server instance corresponding to an external TSQLRecord
    // - just map aServer.StaticVirtualTable[] and will return nil if not
    // a TSQLRestStorageExternal
    // - you can use it e.g. to call MapField() method in a fluent interface
    class function Instance(aClass: TSQLRecordClass;
      aServer: TSQLRestServer): TSQLRestStorageExternal;
    /// retrieve the external database connection associated to a TSQLRecord
    // - just map aServer.StaticVirtualTable[] and will return nil if not
    // a TSQLRestStorageExternal
    class function ConnectionProperties(aClass: TSQLRecordClass;
      aServer: TSQLRestServer): TSQLDBConnectionProperties; overload;
  published
    /// the associated external database connection
    property Properties: TSQLDBConnectionProperties read GetConnectionProperties;
    /// by default, any INSERT will compute the new ID from an internal variable
    // - it is very fast and reliable, unless external IDs can be created
    // outside this engine
    // - you can set EngineAddUseSelectMaxID=true to execute a slower
    // 'select max(ID) from TableName' SQL statement before each EngineAdd()
    // - a lighter alternative may be to call EngineAddForceSelectMaxID only
    // when required, i.e. when the external DB has just been modified
    // by a third-party/legacy SQL process
    property EngineAddUseSelectMaxID: Boolean read fEngineAddUseSelectMaxID
      write fEngineAddUseSelectMaxID;
    /// disable internal ID generation for INSERT
    // - by default, a new ID will be set (either with 'select max(ID)' or via
    // the OnEngineLockedNextID event)
    // - if the client supplies a forced ID within its JSON content, it would
    // be used for adding
    // - define this property to a non 0 value if no such ID is expected to be
    // supplied, but a fixed "fake ID" is returned by the Add() method; at
    // external DB level, no such ID field would be computed nor set at INSERT -
    // this feature may be useful when working with a legacy database - of
    // course any ID-based ORM method would probably fail to work 
    property EngineAddForcedID: TID read fEngineAddForcedID write fEngineAddForcedID;
    /// define an alternate method of compute the ID for INSERT
    // - by default, a new ID will be with 'select max(ID)', and an internal
    // counter (unless EngineAddUseSelectMaxID is true)
    // - you can specify a custom callback, which may compute the ID as
    // expected (e.g. using a SQL sequence)
    property OnEngineAddComputeID: TOnEngineAddComputeID read
      fOnEngineAddComputeID write fOnEngineAddComputeID;
  end;

  /// A Virtual Table cursor for reading a TSQLDBStatement content
  // - this is the cursor class associated to TSQLVirtualTableExternal
  TSQLVirtualTableCursorExternal = class(TSQLVirtualTableCursor)
  protected
    fStatement: ISQLDBStatement;
    fSQL: RawUTF8;
    fHasData: boolean;
  public
    /// called to begin a search in the virtual table, creating a SQL query
    // - the TSQLVirtualTablePrepared parameters were set by
    // TSQLVirtualTable.Prepare and will contain both WHERE and ORDER BY statements
    // (retrieved by x_BestIndex from a TSQLite3IndexInfo structure)
    // - Prepared will contain all prepared constraints and the corresponding
    // expressions in the Where[].Value field
    // - will move cursor to first row of matching data
    // - will return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    // - all WHERE and ORDER BY clauses are able to be translated into a plain
    // SQL statement calling the external DB engine
    // - will create the internal fStatement from a SQL query, bind the
    // parameters, then execute it, ready to be accessed via HasData/Next
    function Search(const Prepared: TSQLVirtualTablePrepared): boolean; override;
    /// called to retrieve a column value of the current data row
    // - if aColumn=VIRTUAL_TABLE_ROWID_COLUMN(-1), will return the row ID
    // as varInt64 into aResult
    // - will return false in case of an error, true on success
    function Column(aColumn: integer; var aResult: TSQLVar): boolean; override;
    /// called after Search() to check if there is data to be retrieved
    // - should return false if reached the end of matching data
    function HasData: boolean; override;
    /// called to go to the next row of matching data
    // - should return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    function Next: boolean; override;
    /// read-only access to the SELECT statement
    property SQL: RawUTF8 read fSQL;
  end;

  /// A SynDB-based virtual table for accessing any external database
  // - for ORM access, you should use VirtualTableExternalRegister method to
  //   associate this virtual table module to any TSQLRecord class
  // - transactions are handled by this module, according to the external database
  TSQLVirtualTableExternal = class(TSQLVirtualTable)
  public { overridden methods }
    /// returns the main specifications of the associated TSQLVirtualTableModule
    // - this is a read/write table, without transaction (yet), associated to the
    // TSQLVirtualTableCursorExternal cursor type, with 'External' as module name
    // and TSQLRestStorageExternal as the related static class
    // - no particular class is supplied here, since it will depend on the
    // associated Static TSQLRestStorageExternal instance
    class procedure GetTableModuleProperties(var aProperties: TVirtualTableModuleProperties);
      override;
    /// called to determine the best way to access the virtual table
    // - will prepare the request for TSQLVirtualTableCursor.Search()
    // - this overridden method will let the external DB engine perform the search,
    // using a standard SQL "SELECT * FROM .. WHERE .. ORDER BY .." statement
    // - in Where[], Expr must be set to not 0 if needed for Search method,
    // and OmitCheck always set to true since double check is not necessary
    // - OmitOrderBy will be set to true since double sort is not necessary
    // - EstimatedCost/EstimatedRows will receive the estimated cost, with
    // lowest value if fStatic.fFieldsExternal[].ColumnIndexed is set
    // (i.e. if column has an index)
    function Prepare(var Prepared: TSQLVirtualTablePrepared): boolean; override;
    /// called when a DROP TABLE statement is executed against the virtual table
    // - returns true on success, false otherwise
    function Drop: boolean; override;
    /// called to delete a virtual table row
    // - returns true on success, false otherwise
    function Delete(aRowID: Int64): boolean; override;
    /// called to insert a virtual table row content
    // - column order follows the Structure method, i.e. StoredClassProps.Fields[] order
    // - returns true on success, false otherwise
    // - returns the just created row ID in insertedRowID on success
    function Insert(aRowID: Int64; var Values: TSQLVarDynArray;
      out insertedRowID: Int64): boolean; override;
    /// called to update a virtual table row content
    // - column order follows the Structure method, i.e. StoredClassProps.Fields[] order
    // - returns true on success, false otherwise
    function Update(oldRowID, newRowID: Int64; var Values: TSQLVarDynArray): boolean; override;
  end;


/// register on the Server-side an external database for an ORM class
// - will associate the supplied class with a TSQLVirtualTableExternal module
// (calling aModel.VirtualTableRegister method), even if the class does not
// inherit from TSQLRecordVirtualTableAutoID (it can be any plain TSQLRecord or
// TSQLRecordMany sub-class for instance)
// - note that TSQLModel.Create() will reset all supplied classes to be defined
// as non virtual (i.e. Kind=rSQLite3)
// - this function shall be called BEFORE TSQLRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSQLDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - the full table name, as expected by the external database, could be
// provided here (SQLTableName will be used internaly as table name when
// called via the associated SQLite3 Virtual Table) - if no table name is
// specified (''), will use SQLTableName (e.g. 'Customer' for 'TSQLCustomer')
// - typical usage is therefore for instance:
// !  Props := TOleDBMSSQLConnectionProperties.Create('.\SQLEXPRESS','AdventureWorks2008R2','','');
// !  Model := TSQLModel.Create([TSQLCustomer],'root');
// !  VirtualTableExternalRegister(Model,TSQLCustomer,Props,'Sales.Customer');
// !  Server := TSQLRestServerDB.Create(aModel,'application.db'),true)
// - the supplied aExternalDB parameter is stored within aClass.RecordProps, so
// the instance must stay alive until all database access to this external table
// is finished (e.g. use a private/protected property)
// - server-side may omit a call to VirtualTableExternalRegister() if the need of
// an internal database is expected: it will allow custom database configuration
// at runtime, depending on the customer's expectations (or license)
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
function VirtualTableExternalRegister(aModel: TSQLModel; aClass: TSQLRecordClass;
  aExternalDB: TSQLDBConnectionProperties;
  const aExternalTableName: RawUTF8=''): boolean; overload;

/// register several tables of the model to be external
// - just a wrapper over the overloaded VirtualTableExternalRegister() method
function VirtualTableExternalRegister(aModel: TSQLModel;
  const aClass: array of TSQLRecordClass;
  aExternalDB: TSQLDBConnectionProperties): boolean; overload;

/// register one table of the model to be external, with optional mapping
// - this method would allow to chain MapField() or MapAutoKeywordFields
// definitions, in a fluent interface:
function VirtualTableExternalMap(aModel: TSQLModel;
  aClass: TSQLRecordClass; aExternalDB: TSQLDBConnectionProperties;
  const aExternalTableName: RawUTF8=''): PSQLRecordPropertiesMapping;

/// register all tables of the model to be external
// - by default, all tables are handled by the SQLite3 engine, unless they
// are explicitely declared as external via VirtualTableExternalRegister: this
// function can be used to register all tables to be handled by an external DBs
// - this function shall be called BEFORE TSQLRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSQLDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - by default, TSQLAuthUser and TSQLAuthGroup tables will be handled via the
// external DB, but you can avoid it for speed when handling session and security
// by setting DoNotRegisterUserGroupTables
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
function VirtualTableExternalRegisterAll(aModel: TSQLModel;
  aExternalDB: TSQLDBConnectionProperties; DoNotRegisterUserGroupTables: boolean=false): boolean; overload;

type
  /// all possible options for VirtualTableExternalRegisterAll/TSQLRestExternalDBCreate
  // - by default, TSQLAuthUser and TSQLAuthGroup tables will be handled via the
  // external DB, but you can avoid it for speed when handling session and security
  // by setting regDoNotRegisterUserGroupTables
  // - you can set regMapAutoKeywordFields to ensure that the mapped field names
  // won't conflict with a SQL reserved keyword on the external database by
  // mapping a name with a trailing '_' character for the external column
  TVirtualTableExternalRegisterOption = (
    regDoNotRegisterUserGroupTables,
    regMapAutoKeywordFields
    );
  /// set of options for VirtualTableExternalRegisterAll/TSQLRestExternalDBCreate functions
  TVirtualTableExternalRegisterOptions = set of TVirtualTableExternalRegisterOption;

/// register all tables of the model to be external, with some options
function VirtualTableExternalRegisterAll(aModel: TSQLModel;
  aExternalDB: TSQLDBConnectionProperties;
  aOptions: TVirtualTableExternalRegisterOptions): boolean; overload;


/// create a new TSQLRest instance, and possibly an external database, from its
// Model and stored values
// - if aDefinition.Kind matches a TSQLRest registered class, one new instance
// of this kind will be created and returned
// - if aDefinition.Kind is a registered TSQLDBConnectionProperties class name,
// it will instantiate an in-memory TSQLRestServerDB or a TSQLRestServerFullMemory
// instance, then call VirtualTableExternalRegisterAll() on this connection
// - will return nil if the supplied aDefinition does not match any registered
// TSQLRest or TSQLDBConnectionProperties types
function TSQLRestExternalDBCreate(aModel: TSQLModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aExternalDBOptions: TVirtualTableExternalRegisterOptions): TSQLRest; overload;


implementation

function VirtualTableExternalRegister(aModel: TSQLModel; aClass: TSQLRecordClass;
  aExternalDB: TSQLDBConnectionProperties; const aExternalTableName: RawUTF8): boolean;
var ExternalTableName: RawUTF8;
    Props: TSQLModelRecordProperties;
begin
  result := False;
  if (aModel=nil) or (aClass=nil) or (aExternalDB=nil) then
    exit; // avoid GPF
  Props := aModel.Props[aClass];
  if Props=nil then
    exit; // if aClass is not part of the model
  Props.Kind := rCustomAutoID; // force creation use of SQLite3 virtual table
  if aExternalTableName='' then
    ExternalTableName := Props.Props.SQLTableName else
    ExternalTableName := aExternalTableName;
  result := aModel.VirtualTableRegister(aClass,TSQLVirtualTableExternal,
    aExternalDB.SQLFullTableName(ExternalTableName),aExternalDB);
end;

function VirtualTableExternalRegister(aModel: TSQLModel;
  const aClass: array of TSQLRecordClass;
  aExternalDB: TSQLDBConnectionProperties): boolean;
var i: integer;
begin
  result := true;
  for i := 0 to High(aClass) do
    if not VirtualTableExternalRegister(aModel,aClass[i],aExternalDB) then
      result := false;
end;

function VirtualTableExternalRegisterAll(aModel: TSQLModel;
  aExternalDB: TSQLDBConnectionProperties; DoNotRegisterUserGroupTables: boolean): boolean;
const OPT: array[boolean] of TVirtualTableExternalRegisterOptions =
  ([],[regDoNotRegisterUserGroupTables]);
begin
  result := VirtualTableExternalRegisterAll(
    aModel,aExternalDB,OPT[DoNotRegisterUserGroupTables]);
end;

function VirtualTableExternalRegisterAll(aModel: TSQLModel;
  aExternalDB: TSQLDBConnectionProperties;
  aOptions: TVirtualTableExternalRegisterOptions): boolean; overload;
var i: integer;
begin
  if (aModel=nil) or (aExternalDB=nil) then begin
    result := false;
    exit; // avoid GPF
  end;
  result := true;
  for i := 0 to high(aModel.Tables) do
    if (regDoNotRegisterUserGroupTables in aOptions) and
       (aModel.Tables[i].InheritsFrom(TSQLAuthGroup) or
        aModel.Tables[i].InheritsFrom(TSQLAuthUser)) then
      continue else
    if not VirtualTableExternalRegister(aModel,aModel.Tables[i],aExternalDB,'') then
      result := false else
      if regMapAutoKeywordFields in aOptions then
        aModel.TableProps[i].ExternalDB.MapAutoKeywordFields;
end;

function VirtualTableExternalMap(aModel: TSQLModel;
  aClass: TSQLRecordClass; aExternalDB: TSQLDBConnectionProperties;
  const aExternalTableName: RawUTF8=''): PSQLRecordPropertiesMapping;
begin
  if VirtualTableExternalRegister(aModel,aClass,aExternalDB,aExternalTableName) then
    result := @aModel.Props[aClass].ExternalDB else
    result := nil;
end;

function TSQLRestExternalDBCreate(aModel: TSQLModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aExternalDBOptions: TVirtualTableExternalRegisterOptions): TSQLRest;
var propsClass: TSQLDBConnectionPropertiesClass;
    props: TSQLDBConnectionProperties;
begin
  result := nil;
  if aDefinition=nil then
    exit;
  propsClass := TSQLDBConnectionProperties.ClassFrom(aDefinition);
  if propsClass<>nil then begin
    props := nil;
    try // aDefinition.Kind was a TSQLDBConnectionProperties -> all external DB
      props := propsClass.Create(aDefinition.ServerName,aDefinition.DatabaseName,
        aDefinition.User,aDefinition.PassWordPlain);
      VirtualTableExternalRegisterAll(aModel,props,aExternalDBOptions);
      result := TSQLRestServer.CreateInMemoryForAllVirtualTables(
        aModel,aHandleAuthentication);
    except
      FreeAndNil(result);
      props.Free;  // avoid memory leak
    end;
  end else
    // not external DB -> try if aDefinition.Kind is a TSQLRest class
    result := TSQLRest.CreateTryFrom(aModel,aDefinition,aHandleAuthentication);
end;


{ TSQLRestStorageExternal }

constructor TSQLRestStorageExternal.Create(aClass: TSQLRecordClass;
  aServer: TSQLRestServer);

  procedure FieldsInternalInit;
  var i,n,int: integer;
  begin
    n := length(fFieldsExternal);
    SetLength(fFieldsExternalToInternal,n);
    with StoredClassProps.ExternalDB do begin
      SetLength(fFieldsInternalToExternal,length(ExtFieldNames)+1);
      for i := 0 to high(fFieldsInternalToExternal) do
        fFieldsInternalToExternal[i] := -1;
      for i := 0 to n-1 do begin
        int := ExternalToInternalIndex(fFieldsExternal[i].ColumnName);
        fFieldsExternalToInternal[i] := int;
        inc(int); // fFieldsInternalToExternal[0]=RowID, then follows fFieldsExternal[]
        if int>=0 then
          fFieldsInternalToExternal[int] := i;
      end;
    end;
  end;
  function PropInfoToExternalField(Prop: TSQLPropInfo;
    var Column: TSQLDBColumnCreate): boolean;
  const
    mORMotType: array[TSQLFieldType] of TSQLDBFieldType =
      // ftUnknown is used for Int32 values, ftInt64 for Int64 values
      (ftUnknown,   // sftUnknown
       ftUTF8,      // sftAnsiText
       ftUTF8,      // sftUTF8Text
       ftUnknown,   // sftEnumerate
       ftInt64,     // sftSet
       ftInt64,     // sftInteger
       ftInt64,     // sftID
       ftInt64,     // sftRecord
       ftUnknown,   // sftBoolean
       ftDouble,    // sftFloat
       ftDate,      // sftDateTime
       ftInt64,     // sftTimeLog
       ftCurrency,  // sftCurrency
       ftUTF8,      // sftObject
       {$ifndef NOVARIANTS}
       ftUTF8,      // sftVariant
       ftUTF8,      // sftNullable (retrieved from Prop.SQLFieldTypeStored)
       {$endif}
       ftBlob,      // sftBlob
       ftBlob,      // sftBlobDynArray
       ftBlob,      // sftBlobCustom
       ftUTF8,      // sftUTF8Comp
       ftInt64,     // sftMany
       ftInt64,     // sftModTime
       ftInt64,     // sftCreateTime
       ftInt64,     // sftTID
       ftInt64,     // sftRecordVersion
       ftInt64,     // sftSessionUserID
       ftDate,      // sftDateTimeMS
       ftInt64,     // sftUnixTime
       ftInt64);    // sftUnixMSTime
  begin
    if Prop.SQLFieldType in [sftUnknown,sftMany] then begin
      result := false;
      exit; // ignore unknown/virtual fields
    end;
    Column.DBType := mORMotType[Prop.SQLFieldTypeStored];
    Column.Name := StoredClassProps.ExternalDB.ExtFieldNames[Prop.PropertyIndex];
    if Column.DBType=ftUTF8 then
      Column.Width := Prop.FieldWidth else
      Column.Width := 0;
    Column.Unique := aIsUnique in Prop.Attributes;
    Column.PrimaryKey := false;
    result := true;
  end;

var SQL: RawUTF8;
    i,f: integer;
    nfo: TSQLPropInfo;
    Field: TSQLDBColumnCreate;
    TableCreated,FieldAdded: Boolean;
    CreateColumns: TSQLDBColumnCreateDynArray;
    options: TSQLRecordPropertiesMappingOptions;
    log: TSynLog;
  procedure GetFields;
  begin
    fProperties.GetFields(UnQuotedSQLSymbolName(fTableName),fFieldsExternal);
    log.Log(sllDebug,'GetFields',TypeInfo(TSQLDBColumnDefineDynArray),fFieldsExternal,self);
  end;
  function FieldsExternalIndexOf(const ColName: RawUTF8): integer;
  begin
    if rpmMissingFieldNameCaseSensitive in options then begin
      for result := 0 to high(fFieldsExternal) do
        if fFieldsExternal[result].ColumnName=ColName then
          exit;
    end else
      for result := 0 to high(fFieldsExternal) do
        if IdemPropNameU(fFieldsExternal[result].ColumnName,ColName) then
          exit;
    result := -1;
  end;
begin       
  {$ifdef WITHLOG}
  log := Owner.LogClass.Add;
  log.Enter('Create %',[aClass],self);
  {$else}
  log := nil;
  {$endif}
  inherited Create(aClass,aServer);
  // initialize external DB properties
  options := fStoredClassProps.ExternalDB.Options;
  fTableName := StoredClassProps.ExternalDB.TableName;
  fProperties := StoredClassProps.ExternalDB.ConnectionProperties as TSQLDBConnectionProperties;
  log.Log(sllInfo,'% % Server=%',[StoredClass,fProperties,Owner],self);
  if fProperties=nil then
    raise EBusinessLayerException.CreateUTF8(
      '%.Create: no external DB defined for %',[self,StoredClass]);
  // ensure external field names are compatible with the external DB keywords
  for f := 0 to StoredClassRecordProps.Fields.Count-1 do begin
    nfo := StoredClassRecordProps.Fields.List[f];
    if nfo.SQLFieldType in COPIABLE_FIELDS then begin // ignore sftMany
      SQL := fStoredClassProps.ExternalDB.ExtFieldNames[f];
      if fProperties.IsSQLKeyword(SQL) then begin
        log.Log(sllWarning,'%.%: Field name "%" is not compatible with %',
          [fStoredClass,nfo.Name,SQL,fProperties.DBMSEngineName],self);
        if rpmAutoMapKeywordFields in options then begin
          log.Log(sllWarning,'-> %.% mapped to "%_"',[fStoredClass,nfo.Name,SQL],self);
          fStoredClassProps.ExternalDB.MapField(nfo.Name,SQL+'_');
        end else
          log.Log(sllWarning,'-> you should better use MapAutoKeywordFields',self);
      end;
    end;
  end;
  // create corresponding external table if necessary, and retrieve its fields info
  TableCreated := false;
  GetFields;
  if not (rpmNoCreateMissingTable in options) then
  if fFieldsExternal=nil then begin
    // table is not yet existing -> try to create it
    with aClass.RecordProps do begin
      SetLength(CreateColumns,Fields.Count+1);
      CreateColumns[0].Name := fStoredClassProps.ExternalDB.RowIDFieldName;
      CreateColumns[0].DBType := ftInt64;
      CreateColumns[0].Unique := true;
      CreateColumns[0].NonNullable := true;
      CreateColumns[0].PrimaryKey := true;
      f := 1;
      for i := 0 to Fields.Count-1 do
        if PropInfoToExternalField(Fields.List[i],CreateColumns[f]) then
          inc(f);
      if f<>Length(CreateColumns) then
        SetLength(CreateColumns,f); // just ignore non handled field types
    end;
    SQL := fProperties.SQLCreate(fTableName,CreateColumns,false);
    if SQL<>'' then
      if ExecuteDirect(pointer(SQL),[],[],false)<>nil then begin
        GetFields;
        if fFieldsExternal=nil then
          raise EORMException.CreateUTF8('%.Create: external table creation % failed:'+
            ' GetFields() returned nil - SQL="%"',[self,StoredClass,fTableName,SQL]);
        TableCreated := true;
      end;
  end;
  FieldsInternalInit;
  // create any missing field if necessary
  if not (rpmNoCreateMissingField in options) then
  if not TableCreated then begin
    FieldAdded := false;
    with StoredClassRecordProps do
    for f := 0 to Fields.Count-1 do
      if Fields.List[f].SQLFieldType in COPIABLE_FIELDS then // ignore sftMany
      /// real database columns exist for Simple + Blob fields (not Many)
      if FieldsExternalIndexOf(fStoredClassProps.ExternalDB.ExtFieldNames[f])<0 then begin
        // add new missing Field
        Finalize(Field);
        FillcharFast(Field,sizeof(Field),0);
        if PropInfoToExternalField(Fields.List[f],Field) then begin
          SQL := fProperties.SQLAddColumn(fTableName,Field);
          if (SQL<>'') and (ExecuteDirect(pointer(SQL),[],[],false)<>nil) then
            FieldAdded := true else
            raise EORMException.CreateUTF8('%.Create: %: unable to create external '+
              'missing field %.% - SQL="%"',
              [self,StoredClass,fTableName,Fields.List[f].Name,SQL]);
        end;
      end;
    if FieldAdded then begin
      GetFields; // get from DB after ALTER TABLE
      FieldsInternalInit;
    end;
  end;
  // compute the SQL statements used internaly for external DB requests
  with StoredClassProps.ExternalDB do begin
    fSelectOneDirectSQL := FormatUTF8('select % from % where %=?',
      [SQL.TableSimpleFields[true,false],fTableName,RowIDFieldName]);
    fSelectAllDirectSQL := FormatUTF8('select %,% from %',
      [SQL.InsertSet,RowIDFieldName,fTableName]);
    fRetrieveBlobFieldsSQL := InternalCSVToExternalCSV(
      StoredClassRecordProps.SQLTableRetrieveBlobFields);
    fUpdateBlobFieldsSQL := InternalCSVToExternalCSV(
      StoredClassRecordProps.SQLTableUpdateBlobFields,'=?,','=?');
  end;
  fSelectTableHasRowsSQL := FormatUTF8('select ID from % limit 1',
    [StoredClassRecordProps.SQLTableName]);
  AdaptSQLForEngineList(fSelectTableHasRowsSQL);
end;

destructor TSQLRestStorageExternal.Destroy;
begin
  inherited Destroy;
end;

function TSQLRestStorageExternal.AdaptSQLForEngineList(var SQL: RawUTF8): boolean;
var Stmt: TSynTableStatement;
    W: TTextWriter;
    limit: TSQLDBDefinitionLimitClause;
    limitSQL,name: RawUTF8;
    f,n: integer;
    extFieldName: function(FieldIndex: Integer): RawUTF8 of object;
begin
  result := false;
  if SQL='' then
    exit;
  Stmt := TSynTableStatement.Create(SQL,
    fStoredClassRecordProps.Fields.IndexByName,
    fStoredClassRecordProps.SimpleFieldsBits[soSelect]);
  try
    if (Stmt.SQLStatement='') or // parsing failed
      not IdemPropNameU(Stmt.TableName,fStoredClassRecordProps.SQLTableName) then begin
      InternalLog('%.AdaptSQLForEngineList: statement too complex -> '+
        'would use SQLite3 virtual engine [%]',[ClassType,SQL],sllWarning);
      exit;
    end;
    if Stmt.Offset<>0 then begin
      InternalLog('%.AdaptSQLForEngineList: unsupported OFFSET for [%]',
        [ClassType,SQL],sllWarning);
      exit;
    end;
    if Stmt.Limit=0 then
      limit.Position := posNone else begin
      limit := fProperties.SQLLimitClause(Stmt);
      if limit.Position=posNone then begin
        InternalLog('%.AdaptSQLForEngineList: unknown "%" LIMIT syntax for [%]',
          [ClassType,ToText(fProperties.DBMS)^,SQL],sllWarning);
        exit;
      end;
      if  limit.Position = posOuter then
        FormatUTF8(limit.InsertFmt,['%', Stmt.Limit],limitSQL) else
        FormatUTF8(limit.InsertFmt,[Stmt.Limit],limitSQL);
    end;
    extFieldName := fStoredClassProps.ExternalDB.FieldNameByIndex;
    W := TTextWriter.CreateOwnedStream(1024);
    try
      W.AddShort('select ');
      if limit.Position=posSelect then
        W.AddString(limitSQL);
      for f := 0 to high(Stmt.Select) do
      with Stmt.Select[f] do begin
        if FunctionName<>'' then begin
          W.AddString(FunctionName);
          W.Add('(');
        end;
        if FunctionKnown=funcCountStar then
          W.Add('*') else
          W.AddString(extFieldName(Field-1));
        if FunctionName<>'' then
          W.Add(')');
        if ToBeAdded<>0 then begin
          if ToBeAdded>0 then
            W.Add('+');
          W.Add(ToBeAdded);
        end;
        if Alias<>'' then begin
          W.AddShort(' as ');
          W.AddString(Alias);
        end else
        if not (Field in StoredClassProps.ExternalDB.FieldNamesMatchInternal) then begin
          if Field=0 then
            name := 'ID' else // RowID may be reserved (e.g. for Oracle)
            name := fStoredClassRecordProps.Fields.List[Field-1].Name;
          W.AddShort(' as ');
          if (FunctionName='') or (FunctionKnown in [funcDistinct,funcMax]) then
            W.AddString(name) else begin
            W.Add('"');
            W.AddString(FunctionName);
            W.Add('(');
            W.AddString(name);
            W.Add(')','"');
          end;
        end;
        W.Add(',');
      end;
      W.CancelLastComma;
      W.AddShort(' from ');
      W.AddString(fTableName);
      n := length(Stmt.Where);
      if n=0 then begin
        if limit.Position=posWhere then begin
          W.AddShort(' where ');
          W.AddString(limitSQL);
        end;
      end else begin
        dec(n);
        W.AddShort(' where ');
        if limit.Position=posWhere then begin
          W.AddString(limitSQL);
          W.AddShort(' and ');
        end;
        for f := 0 to n do
        with Stmt.Where[f] do begin
          if (FunctionName<>'') or (Operator>high(DB_SQLOPERATOR)) then begin
            InternalLog('%.AdaptSQLForEngineList: unsupported function %() for [%]',
              [ClassType,FunctionName,SQL],sllWarning);
            exit;
          end;
          if f>0 then
            if JoinedOR then
              W.AddShort(' or ') else
              W.AddShort(' and ');
          if NotClause then
            W.AddShort('not ');
          if ParenthesisBefore<>'' then
            W.AddString(ParenthesisBefore);
          W.AddString(extFieldName(Field-1));
          W.AddString(DB_SQLOPERATOR[Operator]);
          if not (Operator in [opIsNull, opIsNotNull]) then
            W.AddNoJSONEscape(ValueSQL,ValueSQLLen);
          if ParenthesisAfter<>'' then
            W.AddString(ParenthesisAfter);
        end;
      end;
      if Stmt.GroupByField<>nil then begin
        W.AddShort(' group by ');
        for f := 0 to high(Stmt.GroupByField) do begin
          W.AddString(extFieldName(Stmt.GroupByField[f]-1));
          W.Add(',');
        end;
        W.CancelLastComma;
      end;
      if Stmt.OrderByField<>nil then begin
        W.AddShort(' order by ');
        for f := 0 to high(Stmt.OrderByField) do begin
          W.AddString(extFieldName(Stmt.OrderByField[f]-1));
          W.Add(',');
        end;
        W.CancelLastComma;
        if Stmt.OrderByDesc then
          W.AddShort(' desc');
      end;
      if limit.Position=posAfter then
        W.AddString(limitSQL);
      W.SetText(SQL);
      if limit.Position=posOuter then
        SQL := FormatUTF8(limitSQL,[SQL]);
      result := true;
    finally
      W.Free;
    end;
  finally
    Stmt.Free;
  end;
end;

function TSQLRestStorageExternal.EngineLockedNextID: TID;

  procedure RetrieveFromDB;
  // fProperties.SQLCreate: ID Int64 PRIMARY KEY -> compute unique RowID
  // (not all DB engines handle autoincrement feature - e.g. Oracle does not)
  var Rows: ISQLDBRows;
  begin
    Rows := ExecuteDirect('select max(%) from %',
      [StoredClassProps.ExternalDB.RowIDFieldName,fTableName],[],true);
    if (Rows<>nil) and Rows.Step then
      fEngineLockedMaxID := Rows.ColumnInt(0) else
      fEngineLockedMaxID := 0;
  end;

var handled: boolean;
begin
  if fEngineAddForcedID<>0 then begin
    result := fEngineAddForcedID;
    exit;
  end;
  if Assigned(fOnEngineAddComputeID) then begin
    result := fOnEngineAddComputeID(self,handled);
    if handled then
      exit;
  end;
  if (fEngineLockedMaxID=0) or EngineAddUseSelectMaxID then
    RetrieveFromDB;
  inc(fEngineLockedMaxID);
  result := fEngineLockedMaxID;
end;

function TSQLRestStorageExternal.InternalBatchStart(
  Method: TSQLURIMethod; BatchOptions: TSQLRestBatchOptions): boolean;
const BATCH: array[mPOST..mDELETE] of TSQLDBStatementCRUD = (
  cCreate, cUpdate, cDelete);
begin
  result := false; // means BATCH mode not supported
  if (self<>nil) and (method in [mPOST..mDELETE]) and
     (BATCH[method] in fProperties.BatchSendingAbilities) then begin
    StorageLock(true,'InternalBatchStart'); // protected by try..finally in TSQLRestServer.RunBatch
    try
      if fBatchMethod<>mNone then
        raise EORMException.CreateUTF8('Missing previous %.InternalBatchStop(%)',
          [self,StoredClass]);
      fBatchMethod := Method;
      fBatchCount := 0;
      result := true; // means BATCH mode is supported
    finally
      if not result then
        StorageUnLock;
    end;
  end;
end;

procedure TSQLRestStorageExternal.InternalBatchStop;
var i,j,n,max,BatchBegin,BatchEnd,ValuesMax: integer;
    Query: ISQLDBStatement;
    NotifySQLEvent: TSQLEvent;
    SQL: RawUTF8;
    P: PUTF8Char;
    Fields, ExternalFields: TRawUTF8DynArray;
    Types: TSQLDBFieldTypeArray;
    Values: TRawUTF8DynArrayDynArray;
    Occasion: TSQLOccasion;
    Decode: TJSONObjectDecoder;
    tmp: TSynTempBuffer;
begin
  if fBatchMethod=mNone then
    raise EORMException.CreateUTF8('%.InternalBatchStop(%).BatchMethod=mNone',
      [self,StoredClass]);
  try
    if fBatchCount=0 then
      exit; // nothing to do
    if (Owner<>nil) and (fBatchMethod=mDelete) then // notify BEFORE deletion
      for i := 0 to fBatchCount-1 do
        Owner.InternalUpdateEvent(seDelete,fStoredClassProps.TableIndex,fBatchIDs[i],'',nil);
    with fProperties do
      if BatchMaxSentAtOnce>0 then
        max := BatchMaxSentAtOnce else
        max := 1000;
    BatchBegin := 0;
    BatchEnd := fBatchCount-1;
    repeat
      case fBatchMethod of
      mPost, mPut: begin
        assert(fBatchIDs<>nil);
        BatchEnd := fBatchCount-1;
        for i := BatchBegin to BatchEnd do begin
          tmp.Init(fBatchValues[i]);
          try
            P := tmp.buf;
            while P^ in [#1..' ','{','['] do inc(P);
            if fBatchMethod=mPost then
              Occasion := soInsert else
              Occasion := soUpdate;
            case Occasion of
            soInsert: // mPost=INSERT with the supplied fields and computed ID
              Decode.Decode(P,nil,pQuoted,fBatchIDs[i],true);
            soUpdate: // mPut=UPDATE with the supplied fields and ID set appart
              Decode.Decode(P,nil,pQuoted,0,true);
            end;
            RecordVersionFieldHandle(Occasion,Decode);
            if Fields=nil then begin
              Decode.AssignFieldNamesTo(Fields);
              SQL := JSONDecodedPrepareToSQL(Decode,ExternalFields,Types,Occasion,[]);
              SetLength(Values,Decode.FieldCount);
              ValuesMax := fBatchCount-BatchBegin;
              if ValuesMax>max then
                ValuesMax := max;
              for j := 0 to Decode.FieldCount-1 do
                SetLength(Values[j],ValuesMax);
            end else
              if not Decode.SameFieldNames(Fields) then
                break; // this item would break the SQL statement
            n := i-BatchBegin;
            for j := 0 to high(Fields) do
              Values[j,n] := Decode.FieldValues[j]; // regroup by parameter
            if Occasion=soUpdate then // ?=ID parameter
              Values[length(Fields),n] := Int64ToUtf8(fBatchIDs[i]); // D2007 fails with var
            BatchEnd := i; // mark fBatchValues[i] has to be copied in Values[]
            if n+1>=max then
              break; // do not send too much items at once, for better speed
          finally
            tmp.Done;
          end;
        end;
      end;
      mDelete: begin
        SQL := FormatUTF8('delete from % where %=?',
          [fTableName,fStoredClassProps.ExternalDB.RowIDFieldName]);
        n := BatchEnd-BatchBegin+1;
        if n+1>=max then begin
          n := max; // do not send too much items at once, for better speed
          BatchEnd := BatchBegin+max-1;
        end;
        SetLength(Values,1);
        SetLength(Values[0],n);
        for i := 0 to n-1 do
          Values[0,i] := Int64ToUTF8(fBatchIDs[BatchBegin+i]); // var fails on D2007
      end;
      end;
      n := BatchEnd-BatchBegin+1;
      if n<=0 then
        break;
      if (fBatchMethod=mPost) and Assigned(fProperties.OnBatchInsert) then
        // use multiple insert dedicated function if available
        fProperties.OnBatchInsert(
          fProperties,fTableName,ExternalFields,Types,n,Values) else begin
        // use array binding
        Query := fProperties.NewThreadSafeStatementPrepared(SQL,false);
        try
          case fBatchMethod of
          mPost, mPut:
            for i := 0 to high(Values) do
              Query.BindArray(i+1,Types[i],Values[i],n);
          mDelete:
            Query.BindArray(1,ftInt64,Values[0],n);
          end;
          Query.ExecutePrepared;
        finally
          Query := nil;
        end;
      end;
      if Owner<>nil then begin
        // add/update/delete should flush DB cache
        Owner.FlushInternalDBCache;
        // force deletion coherency
        if fBatchMethod=mDelete then
          for i := 0 to high(Values) do
            Owner.AfterDeleteForceCoherency(
              fStoredClassProps.TableIndex,GetInt64(pointer(Values[i])));
      end;
      Fields := nil; // force new sending block
      BatchBegin := BatchEnd+1;
    until BatchBegin>=fBatchCount;
    if Owner<>nil then begin
      if fBatchMethod in [mPost,mPut] then begin
        if fBatchMethod=mPost then
          NotifySQLEvent := seAdd else
          NotifySQLEvent := seUpdate;
        for i := 0 to fBatchCount-1 do
          Owner.InternalUpdateEvent(NotifySQLEvent,fStoredClassProps.TableIndex,
            fBatchIDs[i],fBatchValues[i],nil);
      end;
      Owner.FlushInternalDBCache;
    end;
  finally
    fBatchValues := nil;
    fBatchIDs := nil;
    fBatchCount := 0;
    fBatchCapacity := 0;
    fBatchMethod := mNone;
    StorageUnLock;
  end;
end;

procedure TSQLRestStorageExternal.InternalBatchAdd(
  const aValue: RawUTF8; const aID: TID);
begin
  if fBatchCount>=fBatchCapacity then begin
    fBatchCapacity := fBatchCapacity+64+fBatchCount shr 3;
    SetLength(fBatchIDs,fBatchCapacity);
    if aValue<>'' then
      SetLength(fBatchValues,fBatchCapacity);
  end;
  if aValue<>'' then
    fBatchValues[fBatchCount] := aValue;
  fBatchIDs[fBatchCount] := aID;
  inc(fBatchCount);
end;

function TSQLRestStorageExternal.EngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
begin
  if (TableModelIndex<0) or (fModel.Tables[TableModelIndex]<>fStoredClass) then
    result := 0 else // avoid GPF
  if fBatchMethod<>mNone then
    if fBatchMethod<>mPOST then
      result := 0 else begin
      if not JSONGetID(pointer(SentData),result) then
        result := EngineLockedNextID else
        if result>fEngineLockedMaxID then
          fEngineLockedMaxID := result;
      InternalBatchAdd(SentData,result);
    end else begin
    result := ExecuteFromJSON(SentData,soInsert,0);
    // UpdatedID=0 -> insert with EngineLockedNextID
    if (result>0) and (Owner<>nil) then begin
      if EngineAddForcedID=0 then // only worth it if result is a true ID
        Owner.InternalUpdateEvent(seAdd,TableModelIndex,result,SentData,nil);
      Owner.FlushInternalDBCache;
    end;
  end;
end;

function TSQLRestStorageExternal.EngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUTF8): boolean;
begin
  if (ID<=0) or (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    if fBatchMethod<>mNone then
      if fBatchMethod<>mPUT then
        result := false else begin
        InternalBatchAdd(SentData,ID);
        result := true;
      end else begin
      result := ExecuteFromJSON(SentData,soUpdate,ID)=ID;
      if result and (Owner<>nil) then begin
        Owner.InternalUpdateEvent(seUpdate,TableModelIndex,ID,SentData,nil);
        Owner.FlushInternalDBCache;
      end;
    end;
end;

function TSQLRestStorageExternal.EngineDelete(TableModelIndex: integer; ID: TID): boolean;
begin
  if (ID<=0) or (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    if fBatchMethod<>mNone then
      if fBatchMethod<>mDELETE then
        result := false else begin
        InternalBatchAdd('',ID);
        result := true;
      end else begin
      if Owner<>nil then // notify BEFORE deletion
        Owner.InternalUpdateEvent(seDelete,TableModelIndex,ID,'',nil);
      result := ExecuteDirect('delete from % where %=?',
        [fTableName,StoredClassProps.ExternalDB.RowIDFieldName],[ID],false)<>nil;
      if result and (Owner<>nil) then
        Owner.FlushInternalDBCache;
    end;
end;

function TSQLRestStorageExternal.EngineDeleteWhere(TableModelIndex: integer;
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
const CHUNK_SIZE = 200;
var i,n: integer;
    aSQLWhereUpper: RawUTF8;
    InClause: TIDDynArray;
begin
  result := false;
  if (IDs=nil) or (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    exit;
  if fBatchMethod<>mNone then
    if fBatchMethod<>mDELETE then
      exit else
      for i := 0 to high(IDs) do
        InternalBatchAdd('',IDs[i]) else begin
    if Owner<>nil then // notify BEFORE deletion
      for i := 0 to high(IDs) do
        Owner.InternalUpdateEvent(seDelete,TableModelIndex,IDs[i],'',nil);
    aSQLWhereUpper := UpperCase(SQLWhere);
    if IdemPChar(pointer(aSQLWhereUpper),'LIMIT ') or
       // LIMIT is not handled by SQLite3 when built from amalgamation
       // see http://www.sqlite.org/compile.html#enable_update_delete_limit
       IdemPChar(pointer(aSQLWhereUpper),'ORDER BY ') or
       (PosEx(' FROM ',aSQLWhereUpper)>0) then begin
      SetLength(InClause,CHUNK_SIZE); // send by chunks
      for i := 0 to length(IDs) div CHUNK_SIZE do begin
        n := length(IDs);
        if n<(i+1)*CHUNK_SIZE then
          dec(n,i*CHUNK_SIZE) else
          n := CHUNK_SIZE;
        MoveFast(IDs[i*CHUNK_SIZE],InClause[0],n*sizeof(TID));
        if ExecuteInlined('delete from % where %',[fTableName,Int64DynArrayToCSV(
            TInt64DynArray(InClause),n,'RowID in (',')')],false)=nil then
          exit;
      end;
    end else
    if ExecuteInlined('delete from %%',[fTableName,SQLFromWhere(SQLWhere)],false)=nil then
      exit;
    if Owner<>nil then
      Owner.FlushInternalDBCache;
  end;
  result := true;
end;

function TSQLRestStorageExternal.EngineList(const SQL: RawUTF8;
  ForceAJAX: Boolean; ReturnedRowCount: PPtrInt): RawUTF8;
var Stmt: ISQLDBStatement;
begin
  if ReturnedRowCount<>nil then
    raise ESQLDBException.CreateUTF8('%.EngineList(ReturnedRowCount<>nil) for %',
      [self,StoredClass]);
  Stmt := PrepareInlinedForRows(SQL);
  if Stmt=nil then
    result := '' else
    Stmt.ExecutePreparedAndFetchAllAsJSON(
      ForceAJAX or (Owner=nil) or not Owner.NoAJAXJSON,result);
end;

function TSQLRestStorageExternal.EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8;
var Stmt: ISQLDBStatement;
begin // TableModelIndex is not useful here
  result := '';
  if (self=nil) or (ID<=0) then
    exit;
  Stmt := PrepareDirectForRows(pointer(fSelectOneDirectSQL),[],[ID]);
  if Stmt<>nil then begin
    Stmt.ExecutePreparedAndFetchAllAsJSON(true,result); // Expanded=true -> '[{"ID":10,...}]'#10
    if IsNotAjaxJSON(pointer(result)) then
      // '{"fieldCount":2,"values":["ID","FirstName"]}'#$A -> ID not found
      result := '' else
      // list '[{...}]'#10 -> object '{...}'
      result := copy(result,2,length(result)-3);
  end;
end;

function TSQLRestStorageExternal.EngineExecute(
  const aSQL: RawUTF8): boolean;
begin
  if aSQL='' then
    result := false else
    result := ExecuteInlined(aSQL,false)<>nil;
end;

function TSQLRestStorageExternal.TableHasRows(Table: TSQLRecordClass): boolean;
var Rows: ISQLDBRows;
begin
  if (self=nil) or (Table<>fStoredClass) then
    result := false else begin
    Rows := ExecuteDirect(pointer(fSelectTableHasRowsSQL),[],[],true);
    if Rows=nil then
      result := false else
      result := Rows.Step;
  end;
end;

function TSQLRestStorageExternal.TableRowCount(Table: TSQLRecordClass): Int64;
var Rows: ISQLDBRows;
begin
  if (self=nil) or (Table<>fStoredClass) then
    result := 0 else begin
    Rows := ExecuteDirect('select count(*) from %',[fTableName],[],true);
    if (Rows=nil) or not Rows.Step then
      result := 0 else
      result := Rows.ColumnInt(0);
  end;
end;

function TSQLRestStorageExternal.EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
  BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean;
var Rows: ISQLDBRows;
begin
  result := false;
  if (aID<=0) or not BlobField^.IsBlob or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    exit;
  with StoredClassProps.ExternalDB do
    Rows := ExecuteDirect('select % from % where %=?',
      [InternalToExternal(BlobField^.Name),fTableName,RowIDFieldName],[aID],true);
  if (Rows<>nil) and Rows.Step then
  try
    BlobData := Rows.ColumnBlob(0);
    result := true; // success
    Rows := nil;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestStorageExternal.RetrieveBlobFields(Value: TSQLRecord): boolean;
var Rows: ISQLDBRows;
    f: Integer;
    data: TSQLVar;
    temp: RawByteString;
begin
  result := false;
  if (Value<>nil) and (Value.ID>0) and (PSQLRecordClass(Value)^=fStoredClass) then
  with Value.RecordProps do
  if BlobFields<>nil then begin
    Rows := ExecuteDirect('select % from % where %=?',
      [fRetrieveBlobFieldsSQL,fTableName,StoredClassProps.ExternalDB.RowIDFieldName],
      [Value.ID],true);
    if (Rows<>nil) and Rows.Step then
    try
      for f := 0 to High(BlobFields) do begin
        Rows.ColumnToSQLVar(f,data,temp);
        BlobFields[f].SetFieldSQLVar(Value,data);
      end;
      result := true; // success
      Rows := nil;
    except
      on Exception do
        result := false;
    end;
  end;
end;

function TSQLRestStorageExternal.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
var Rows: ISQLDBRows;
    ExtWhereFieldName, JSON: RawUTF8;
begin
  if (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    with StoredClassProps.ExternalDB do begin
      ExtWhereFieldName := InternalToExternal(WhereFieldName);
      result := ExecuteInlined('update % set %=:(%): where %=:(%):',
        [fTableName,InternalToExternal(SetFieldName),SetValue,
         ExtWhereFieldName,WhereValue],false)<>nil;
      if result and (Owner<>nil) then begin
        if Owner.InternalUpdateEventNeeded(TableModelIndex) then begin
          Rows := ExecuteInlined('select % from % where %=:(%):',
            [RowIDFieldName,fTableName,ExtWhereFieldName,WhereValue],true);
          if Rows=nil then
            exit;
          JSONEncodeNameSQLValue(SetFieldName,SetValue,JSON);
          while Rows.Step do
            Owner.InternalUpdateEvent(seUpdate,TableModelIndex,Rows.ColumnInt(0),JSON,nil);
        end;
        Owner.FlushInternalDBCache;
      end;
    end;
end;

function TSQLRestStorageExternal.EngineUpdateFieldIncrement(TableModelIndex: integer;
  ID: TID; const FieldName: RawUTF8; Increment: Int64): boolean;
var extField: RawUTF8;
    Value: Int64;
begin
  result := false;
  if (ID<=0) or (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    exit;
  if (Owner<>nil) and Owner.InternalUpdateEventNeeded(TableModelIndex) then
    result := OneFieldValue(fStoredClass,FieldName,'ID=?',[],[ID],Value) and
              UpdateField(fStoredClass,ID,FieldName,[Value+Increment]) else
    try
      with StoredClassProps.ExternalDB do begin
        extField := InternalToExternal(FieldName);
        result := ExecuteInlined('update % set %=%+:(%): where %=:(%):',
          [fTableName,extField,extField,Increment,RowIDFieldName,ID],false)<>nil;
      end;
      if result and (Owner<>nil) then
        Owner.FlushInternalDBCache;
    except
      on Exception do
        result := false;
    end;
end;

function TSQLRestStorageExternal.EngineUpdateBlob(TableModelIndex: integer; aID: TID;
  BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean;
var Statement: ISQLDBStatement;
    AffectedField: TSQLFieldBits;
begin
  result := false;
  if (aID<=0) or not BlobField^.IsBlob or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    exit;
  try
    if Owner<>nil then
      Owner.FlushInternalDBCache;
    with StoredClassProps.ExternalDB do
      Statement := fProperties.NewThreadSafeStatementPrepared(
        'update % set %=? where %=?',
        [fTableName,InternalToExternal(BlobField^.Name),RowIDFieldName],false);
    if Statement<>nil then begin
      if BlobData='' then
        Statement.BindNull(1) else
        Statement.BindBlob(1,BlobData); // fast explicit BindBlob() call
      Statement.Bind(2,aID);
      Statement.ExecutePrepared;
      if Owner<>nil then begin
        fStoredClassRecordProps.FieldBitsFromBlobField(BlobField,AffectedField);
        Owner.InternalUpdateEvent(seUpdateBlob,TableModelIndex,aID,'',@AffectedField);
        Owner.FlushInternalDBCache;
      end;
      result := true; // success
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestStorageExternal.UpdateBlobFields(Value: TSQLRecord): boolean;
var f: integer;
    aID: TID;
    temp: array of RawByteString;
    Params: TSQLVarDynArray;
begin
  result := false;
  if (Value<>nil) and (PSQLRecordClass(Value)^=fStoredClass) then
  with Value.RecordProps do
  if BlobFields<>nil then begin
    aID := Value.ID;
    if aID<=0 then
      exit;
    if Owner<>nil then
      Owner.FlushInternalDBCache;
    SetLength(Params,length(BlobFields));
    SetLength(temp,length(BlobFields));
    for f := 0 to high(Params) do
      BlobFields[f].GetFieldSQLVar(Value,Params[f],temp[f]);
    result := ExecuteDirectSQLVar('update % set % where %=?',
      [fTableName,fUpdateBlobFieldsSQL,StoredClassProps.ExternalDB.RowIDFieldName],
       Params,aID,false);
    if result and (Owner<>nil) then begin
      Owner.InternalUpdateEvent(seUpdateBlob,fStoredClassProps.TableIndex,aID,'',
          @fStoredClassRecordProps.FieldBits[sftBlob]);
      Owner.FlushInternalDBCache;
    end;
  end else
    result := true; // as TSQLRest.UpdateblobFields()
end;

function TSQLRestStorageExternal.PrepareInlinedForRows(const aSQL: RawUTF8): ISQLDBStatement;
begin
  result := nil; // returns nil interface on error
  if self=nil then
    exit;
  try
    result := fProperties.PrepareInlined(aSQL,true);
    if (result<>nil) and (sftDateTimeMS in fStoredClassRecordProps.HasTypeFields) then
      result.ForceDateWithMS := true;
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestStorageExternal.ExecuteInlined(const aSQL: RawUTF8;
  ExpectResults: Boolean): ISQLDBRows;
var stmt: ISQLDBStatement;
begin
  result := nil; // returns nil interface on error
  if self=nil then
    exit;
  if not ExpectResults and (Owner<>nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  try
    stmt := fProperties.PrepareInlined(aSQL,ExpectResults);
    if stmt=nil then
      exit;
    if ExpectResults and (sftDateTimeMS in fStoredClassRecordProps.HasTypeFields) then
      stmt.ForceDateWithMS := true;
    stmt.ExecutePrepared;
    result := stmt;
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestStorageExternal.ExecuteInlined(SQLFormat: PUTF8Char;
  const Args: array of const; ExpectResults: Boolean): ISQLDBRows;
begin
  result := ExecuteInlined(FormatUTF8(SQLFormat,Args),ExpectResults);
end;

function TSQLRestStorageExternal.PrepareDirectForRows(SQLFormat: PUTF8Char;
  const Args, Params: array of const): ISQLDBStatement;
var Query: ISQLDBStatement;
begin
  result := nil;
  if self=nil then
    exit;
  Query := fProperties.NewThreadSafeStatementPrepared(SQLFormat,Args,true);
  if Query<>nil then
  try
    Query.Bind(Params);
    if sftDateTimeMS in fStoredClassRecordProps.HasTypeFields then
      Query.ForceDateWithMS := true;
    result := Query;
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestStorageExternal.ExecuteDirect(SQLFormat: PUTF8Char;
  const Args, Params: array of const; ExpectResults: Boolean): ISQLDBRows;
var Query: ISQLDBStatement;
begin
  result := nil;
  if self=nil then
    exit;
  if not ExpectResults and (Owner<>nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  Query := fProperties.NewThreadSafeStatementPrepared(SQLFormat,Args,ExpectResults);
  if Query<>nil then
  try
    Query.Bind(Params);
    if ExpectResults and (sftDateTimeMS in fStoredClassRecordProps.HasTypeFields) then
      Query.ForceDateWithMS := true;
    Query.ExecutePrepared;
    result := Query;
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestStorageExternal.ExecuteDirectSQLVar(SQLFormat: PUTF8Char;
  const Args: array of const; var Params: TSQLVarDynArray; const LastIntegerParam: Int64;
  ParamsMatchCopiableFields: boolean): boolean;
var Query: ISQLDBStatement;
    ParamsCount, f: integer;
begin
  result := false;
  if Self<>nil then
  try
    Query := fProperties.NewThreadSafeStatementPrepared(SQLFormat,Args,false);
    if Query=nil then
      exit;
    ParamsCount := length(Params);
    if ParamsMatchCopiableFields and
       (ParamsCount<>Length(fStoredClassRecordProps.CopiableFields)) then
      raise EORMException.CreateUTF8('%.ExecuteDirectSQLVar(ParamsMatchCopiableFields) for %',
       [self,StoredClass]);
    for f := 0 to ParamsCount-1 do
      if ParamsMatchCopiableFields and
         (fStoredClassRecordProps.CopiableFields[f].SQLFieldTypeStored in
           [sftDateTime,sftDateTimeMS]) and
         (Params[f].VType=ftUTF8) then
        Query.BindDateTime(f+1,Iso8601ToDateTimePUTF8Char(Params[f].VText)) else
        Query.Bind(f+1,Params[f]);
    if LastIntegerParam<>0 then
      Query.Bind(ParamsCount+1,LastIntegerParam);
    Query.ExecutePrepared;
    result := true; // success
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestStorageExternal.EngineSearchField(
  const FieldName: ShortString; const FieldValue: array of const;
  out ResultID: TIDDynArray): boolean;
var n: Integer;
    Rows: ISQLDBRows;
begin
  n := 0;
  Rows := ExecuteDirect('select % from % where %=?',
    [StoredClassProps.ExternalDB.RowIDFieldName,fTableName,FieldName],FieldValue,true);
  if Rows<>nil then
    while Rows.Step do
      AddInt64(TInt64DynArray(ResultID),n,Rows.ColumnInt(0));
  SetLength(ResultID,n);
  result := n>0;
end;

function TSQLRestStorageExternal.SearchField(const FieldName: RawUTF8;
  FieldValue: Int64; out ResultID: TIDDynArray): boolean;
begin
  result := EngineSearchField(FieldName,[FieldValue],ResultID);
end;

function TSQLRestStorageExternal.SearchField(const FieldName, FieldValue: RawUTF8;
  out ResultID: TIDDynArray): boolean;
begin
  result := EngineSearchField(FieldName,[FieldValue],ResultID);
end;

function TSQLRestStorageExternal.TransactionBegin(
  aTable: TSQLRecordClass; SessionID: cardinal): boolean;
begin
  if (aTable=fStoredClass) and inherited TransactionBegin(aTable,SessionID) then
    result := fProperties.SharedTransaction(SessionID,transBegin)<>nil else
    result := false;
end;

procedure TSQLRestStorageExternal.Commit(SessionID: cardinal; RaiseException: boolean);
const ACTION: array[boolean] of TSQLDBSharedTransactionAction = (
        transCommitWithoutException, transCommitWithException);
begin
  inherited Commit(SessionID,RaiseException);
  // reset fTransactionActive + write all TSQLVirtualTableJSON
  fProperties.SharedTransaction(SessionID,ACTION[RaiseException]);
end;

procedure TSQLRestStorageExternal.RollBack(SessionID: cardinal);
begin
  inherited RollBack(SessionID); // reset fTransactionActive
  fProperties.SharedTransaction(SessionID,transRollback);
end;

function TSQLRestStorageExternal.CreateSQLMultiIndex(
  Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
  Unique: boolean; IndexName: RawUTF8): boolean;
var SQL: RawUTF8;
    ExtFieldNames: TRawUTF8DynArray;
    IntFieldIndex: TIntegerDynArray;
    Descending: boolean;
    i,n,extfield: integer;
begin
  result := false;
  Descending := false;
  n := length(FieldNames);
  if (self=nil) or (fProperties=nil) or (Table<>fStoredClass) or (n<=0) then
    exit;
  StoredClassProps.ExternalDB.InternalToExternalDynArray(
    FieldNames,ExtFieldNames,@IntFieldIndex);
  if n=1 then begin // handle case of index over a single column
    if IntFieldIndex[0]<0 then // ID/RowID?
      case fProperties.DBMS of
      dSQLite, // SQLite3 always generates an index for ID/RowID
      dPostgreSQL,dMSSQL,dMySQL,dOracle,dNexusDB: begin // as most DB on primary key
        result := true;
        exit;
      end;
      dFirebird:  // see http://www.firebirdfaq.org/faq205
        Descending := true;
      end;
    if not Descending then begin // we identify just if indexed, not the order
      extfield := fFieldsInternalToExternal[IntFieldIndex[0]+1];
      if (extfield>=0) and (fFieldsExternal[extfield].ColumnIndexed) then begin
        result := true; // column already indexed
        exit;
      end;
    end;
  end;
  if not (fProperties.DBMS in DB_HANDLEINDEXONBLOBS) then
    // BLOB fields cannot be indexed (only in SQLite3+PostgreSQL)
    for i := 0 to n-1 do begin
      extfield := fFieldsInternalToExternal[IntFieldIndex[i]+1];
      if (extfield>=0) and
         (fFieldsExternal[extfield].ColumnType in [ftBlob,ftUTF8]) and
         (fFieldsExternal[extfield].ColumnLength<=0) then begin
        if i=0 then
          exit; // impossible to create an index with no field!
        SetLength(ExtFieldNames,i); // truncate index to the last indexable field
        break;
      end;
    end;
  SQL := fProperties.SQLAddIndex(fTableName,ExtFieldNames,Unique,Descending,IndexName);
  if (SQL='') or (ExecuteDirect(pointer(SQL),[],[],false)=nil) then
    exit;
  result := true;
  extfield := fFieldsInternalToExternal[IntFieldIndex[0]+1];
  if extfield>=0 then // mark first column as indexed by now
    fFieldsExternal[extfield].ColumnIndexed := true;
end;

class function TSQLRestStorageExternal.Instance(
  aClass: TSQLRecordClass; aServer: TSQLRestServer): TSQLRestStorageExternal;
begin
  if (aClass=nil) or (aServer=nil) then
    result := nil else begin
    result := TSQLRestStorageExternal(aServer.StaticVirtualTable[aClass]);
    if result<>nil then
      if not result.InheritsFrom(TSQLRestStorageExternal) then
        result := nil;
  end;
end;

class function TSQLRestStorageExternal.ConnectionProperties(
  aClass: TSQLRecordClass; aServer: TSQLRestServer): TSQLDBConnectionProperties;
begin
  result := Instance(aClass,aServer).GetConnectionProperties;
end;

function TSQLRestStorageExternal.GetConnectionProperties: TSQLDBConnectionProperties;
begin
  if self=nil then
    result := nil else
    result := fProperties;
end;

function TSQLRestStorageExternal.ExecuteFromJSON(
  const SentData: RawUTF8; Occasion: TSQLOccasion; UpdatedID: TID): TID;
var Decoder: TJSONObjectDecoder;
    SQL: RawUTF8;
    Types: TSQLDBFieldTypeArray;
    ExternalFields: TRawUTF8DynArray;
    InsertedID: TID;
    F: integer;
    Query: ISQLDBStatement;
begin
  result := 0;
  StorageLock(false,'ExecuteFromJson'); // avoid race condition against max(ID)
  try
    case Occasion of
    soInsert:
      if not JSONGetID(pointer(SentData),InsertedID) then
        // no specified "ID":... field value -> compute next
        InsertedID := EngineLockedNextID else
        if InsertedID>fEngineLockedMaxID then
          fEngineLockedMaxID := InsertedID;
    soUpdate:
      if UpdatedID<>0 then
        InsertedID := 0 else
        raise ESQLDBException.CreateUTF8('%.ExecuteFromJSON(%,soUpdate,UpdatedID=%)',
          [self,StoredClass,UpdatedID]);
    else raise ESQLDBException.CreateUTF8('%.ExecuteFromJSON(%,Occasion=%)?',
           [self,StoredClass,ToText(Occasion)^]);
    end;
    // decode fields
    if (fEngineAddForcedID<>0) and (InsertedID=fEngineAddForcedID) then
      Decoder.Decode(SentData,nil,pNonQuoted,0,true) else
      Decoder.Decode(SentData,nil,pNonQuoted,InsertedID,true);
    if (Decoder.FieldCount=0) and (Occasion=soUpdate) then begin
      result := UpdatedID; // SentData='' -> no column to update
      exit;
    end;
    RecordVersionFieldHandle(Occasion,Decoder);
    // compute SQL statement and associated bound parameters
    SQL := JSONDecodedPrepareToSQL(Decoder,ExternalFields,Types,Occasion,[]);
    if Occasion=soUpdate then  // Int64ToUTF8(var) fails on D2007
      Decoder.FieldValues[Decoder.FieldCount-1] := Int64ToUTF8(UpdatedID);
    // execute statement
    Query := fProperties.NewThreadSafeStatementPrepared(SQL,false);
    if Query=nil then
      exit;
    try
      for F := 0 to Decoder.FieldCount-1 do
      if Decoder.FieldTypeApproximation[F]=ftaNull then
        Query.BindNull(F+1) else
        Query.Bind(F+1,Types[F],Decoder.FieldValues[F],true);
      Query.ExecutePrepared;
    except
      exit; // leave result=0
    end;
    // mark success
    if UpdatedID=0 then
      result := InsertedID else
      result := UpdatedID;
  finally
    StorageUnLock;
  end;
end;

procedure TSQLRestStorageExternal.EndCurrentThread(Sender: TThread);
begin
  if fProperties.InheritsFrom(TSQLDBConnectionPropertiesThreadSafe) then
    TSQLDBConnectionPropertiesThreadSafe(fProperties).EndCurrentThread;
end;

function TSQLRestStorageExternal.InternalFieldNameToFieldExternalIndex(
  const InternalFieldName: RawUTF8): integer;
begin
  result := fStoredClassRecordProps.Fields.IndexByNameOrExcept(InternalFieldName);
  result := fFieldsInternalToExternal[result+1];
end;

function TSQLRestStorageExternal.JSONDecodedPrepareToSQL(
  var Decoder: TJSONObjectDecoder; out ExternalFields: TRawUTF8DynArray;
  out Types: TSQLDBFieldTypeArray; Occasion: TSQLOccasion;
  BatchOptions: TSQLRestBatchOptions): RawUTF8;
var f,k: Integer;
begin
  SetLength(ExternalFields,Decoder.FieldCount);
  for f := 0 to Decoder.FieldCount-1 do begin
    k := fStoredClassRecordProps.Fields.IndexByNameOrExcept(Decoder.FieldNames[f]);
    ExternalFields[f] := fStoredClassProps.ExternalDB.FieldNameByIndex(k);
    k := fFieldsInternalToExternal[k+1]; // retrieve exact Types[f] from SynDB
    if k<0 then
      raise ESQLDBException.CreateUTF8(
        '%.JSONDecodedPrepareToSQL(%): No column for "%" field in table %',
        [self,StoredClass,Decoder.FieldNames[f],fTableName]);
    Types[f] := fFieldsExternal[k].ColumnType;
  end;
  // compute SQL statement and associated bound parameters
  Decoder.DecodedFieldNames := pointer(ExternalFields);
  result := Decoder.EncodeAsSQLPrepared(fTableName,Occasion,
    StoredClassProps.ExternalDB.RowIDFieldName,BatchOptions);
  if Occasion=soUpdate then
    if Decoder.FieldCount=MAX_SQLFIELDS then
      raise EParsingException.CreateUTF8('Too many fields for '+
        '%.JSONDecodedPrepareToSQL',[self]) else begin
      Types[Decoder.FieldCount] := ftInt64; // add "where ID=?" parameter
      inc(Decoder.FieldCount);
    end;
end;

procedure TSQLRestStorageExternal.EngineAddForceSelectMaxID;
begin
  StorageLock(true,'EngineAddForceSelectMaxID');
  fEngineLockedMaxID := 0;
  StorageUnLock;
end;

const
  SQL_OPER_WITH_PARAM: array[soEqualTo..soGreaterThanOrEqualTo] of RawUTF8 = (
    '=?','<>?','<?','<=?','>?','>=?');

function TSQLRestStorageExternal.ComputeSQL(
  const Prepared: TSQLVirtualTablePrepared): RawUTF8;
var i: integer;
    constraint: PSQLVirtualTablePreparedConstraint;
    {$ifdef SQLVIRTUALLOGS}
    log: RawUTF8;
    {$endif}
begin
  result := fSelectAllDirectSQL;
  for i := 0 to Prepared.WhereCount-1 do begin
    constraint := @Prepared.Where[i];
    {$ifdef SQLVIRTUALLOGS}
    log := FormatUTF8('% [column=% oper=%]',
      [log,constraint^.Column,ToText(constraint^.Operation)^]);
    {$endif}
    if constraint^.Operation>high(SQL_OPER_WITH_PARAM) then
      exit; // invalid specified operator -> abort search
    if i=0 then
      result := result+' where ' else
      result := result+' and ';
    if StoredClassProps.ExternalDB.AppendFieldName(constraint^.Column,result) then
      exit; // invalid column index -> abort search
    result := result+SQL_OPER_WITH_PARAM[constraint^.Operation];
  end;
  // e.g. 'select FirstName,..,ID from PeopleExternal where FirstName=? and LastName=?'
  for i := 0 to Prepared.OrderByCount-1 do
  with Prepared.OrderBy[i] do begin
    if i=0 then
      result := result+' order by ' else
      result := result+', ';
    if StoredClassProps.ExternalDB.AppendFieldName(Column,result) then
      exit; // invalid column index -> abort search
    if Desc then
      result := result+' desc';
  end;
  {$ifdef SQLVIRTUALLOGS}
  SQLite3Log.Add.Log(sllDebug,'%.ComputeSQL "%" %',[ClassType,result,log],self);
  {$endif}
end;


{ TSQLVirtualTableCursorExternal }

function TSQLVirtualTableCursorExternal.Column(aColumn: integer;
  var aResult: TSQLVar): boolean;
var n: cardinal;
begin
  result := false;
  if (self<>nil) and (fStatement<>nil) then
  try
    n := fStatement.ColumnCount-1;
    if aColumn=VIRTUAL_TABLE_ROWID_COLUMN then
      aColumn := n else // RowID is latest column (select %,RowID from..)
      if cardinal(aColumn)>=n then
        exit; // error if aColumn is out of range
    fStatement.ColumnToSQLVar(aColumn,aResult,fColumnTemp);
    result := aResult.VType<>ftUnknown;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLVirtualTableCursorExternal.HasData: boolean;
begin
  result := (self<>nil) and (fStatement<>nil) and fHasData;
end;

function TSQLVirtualTableCursorExternal.Next: boolean;
begin
  result := false;
  if (self<>nil) and (fStatement<>nil) then
  try
    fHasData := fStatement.Step;
    result := true; // success (may be with no more data)
  except
    on Exception do
      fHasData := false; // returns false on error + HasData=false
  end;
end;

function TSQLVirtualTableCursorExternal.Search(
  const Prepared: TSQLVirtualTablePrepared): boolean;
var i: integer;
begin
  result := false;
  if (Self<>nil) and (Table<>nil) and (Table.Static<>nil) then
  with Table.Static as TSQLRestStorageExternal do begin
    {$ifndef SQLVIRTUALLOGS}
    if fSQL='' then
    {$endif}
      fSQL := ComputeSQL(Prepared);
    try
      fStatement := fProperties.NewThreadSafeStatementPrepared(fSQL,true);
      if fStatement<>nil then begin
        if sftDateTimeMS in fStoredClassRecordProps.HasTypeFields then
          fStatement.ForceDateWithMS := true;
        for i := 1 to Prepared.WhereCount do
          fStatement.Bind(i,Prepared.Where[i-1].Value);
        fStatement.ExecutePrepared;
        result := Next; // on execution success, go to the first row
      end;
    except
      on Exception do
        fStatement := nil;
    end;
  end;
end;


{ TSQLVirtualTableExternal }

function TSQLVirtualTableExternal.Drop: boolean;
begin
  if (self=nil) or (Static=nil) then
    result := false else
    with Static as TSQLRestStorageExternal do
      result := ExecuteDirect('drop table %',[fTableName],[],false)<>nil;
end;

class procedure TSQLVirtualTableExternal.GetTableModuleProperties(
  var aProperties: TVirtualTableModuleProperties);
begin
  aProperties.Features := [vtWrite];
  aProperties.CursorClass := TSQLVirtualTableCursorExternal;
  aProperties.StaticClass := TSQLRestStorageExternal;
end;

function TSQLVirtualTableExternal.Prepare(var Prepared: TSQLVirtualTablePrepared): boolean;
var i, col: integer;
    Fields: TSQLPropInfoList;
begin
  result := inherited Prepare(Prepared); // set costFullScan or costPrimaryIndex
  if result and (Static<>nil) then
  with Static as TSQLRestStorageExternal do begin
    // mark Where[] clauses will be handled by SQL
    Fields := StoredClassRecordProps.Fields;
    result := false;
    for i := 0 to Prepared.WhereCount-1 do
      with Prepared.Where[i] do
      if (Column<>VIRTUAL_TABLE_IGNORE_COLUMN) and
         (Operation<=high(SQL_OPER_WITH_PARAM)) then begin
        if Column=VIRTUAL_TABLE_ROWID_COLUMN then // is an indexed primary key
          Prepared.EstimatedCost := costPrimaryIndex else begin
          if cardinal(Column)>=cardinal(Fields.Count) then
            exit; // invalid column index -> abort query
          col := fFieldsInternalToExternal[Column+1];
          if col<0 then
            exit; // column not known in the external database -> abort query
          if fFieldsExternal[col].ColumnIndexed then begin
            if Prepared.EstimatedCost<costSecondaryIndex then
              Prepared.EstimatedCost := costSecondaryIndex;
          end else
            if Prepared.EstimatedCost<costScanWhere then
              Prepared.EstimatedCost := costScanWhere;
        end;
        OmitCheck := true; // search handled via SQL query
        Value.VType := ftNull; // caller vt_BestIndex() expects <> ftUnknown
      end;
    // check the OrderBy[] clauses
    if Prepared.OrderByCount>0 then begin
      for i := 0 to Prepared.OrderByCount-1 do
        with Prepared.OrderBy[i] do
        if (Column<>VIRTUAL_TABLE_ROWID_COLUMN) and
           (cardinal(Column)>=cardinal(Fields.Count)) then
          exit; // invalid column index -> abort query
      Prepared.OmitOrderBy := true; // order handled via SQL query
    end;
    result := true; // success
  end;
end;

// here below, virtual write operations do not call Engine*() but direct SQL
// -> InternalUpdateEvent() were already called by MainEngine*() methods

function TSQLVirtualTableExternal.Delete(aRowID: Int64): boolean;
begin
  if (self<>nil) and (Static<>nil) and (aRowID>0) then
    with Static as TSQLRestStorageExternal do
      result := ExecuteDirect('delete from % where %=?',
        [fTableName,StoredClassProps.ExternalDB.RowIDFieldName],[aRowID],false)<>nil else
    result := false;
end;

function TSQLVirtualTableExternal.Insert(aRowID: Int64;
  var Values: TSQLVarDynArray; out insertedRowID: Int64): boolean;
begin // aRowID is just ignored here since IDs are always auto calculated
  result := false;
  if (self<>nil) and (Static<>nil) then
  with Static as TSQLRestStorageExternal do begin
    StorageLock(false,'Insert'); // to avoid race condition against max(RowID)
    try
      insertedRowID := EngineLockedNextID;
      with StoredClassProps.ExternalDB do
        result := ExecuteDirectSQLVar('insert into % (%,%) values (%,?)',
          [fTableName,SQL.InsertSet,RowIDFieldName,CSVOfValue('?',length(Values))],
          Values,insertedRowID,true);
    finally
      StorageUnLock;
    end;
  end;
end;

function TSQLVirtualTableExternal.Update(oldRowID, newRowID: Int64;
  var Values: TSQLVarDynArray): boolean;
begin
  if (self<>nil) and (Static<>nil) and
     (oldRowID=newRowID) and (newRowID>0) then // don't allow ID change
    with Static as TSQLRestStorageExternal, StoredClassProps.ExternalDB do
      result := ExecuteDirectSQLVar('update % set % where %=?',
        [fTableName,SQL.UpdateSetAll,RowIDFieldName],Values,oldRowID,true) else
    result := false;
end;


initialization
  // all our SynDB related functions shall log to main TSQLLog
  SynDBLog := TSQLLog;
end.
