{******************************************************************************}
{                                                                              }
{                 UNIFIED INTERBASE (UIB) DBExpress Driver                     }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is MainDriver.pas.                                         }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{                                                                              }
{******************************************************************************}

unit MainDriver;

{$I uib.inc}
{$IFDEF LINUX}
{$DEFINE COMPILER7_UP}
{$ENDIF}

interface
uses SysUtils, uiblib, uib, uibmetadata, DBXpress, SqlConst, Classes,
  SqlTimSt {$IFDEF COMPILER6_UP},fmtbcd{$ENDIF};

type
  TTableItem = (Table,View,SystemTable,Synonym,TempTable,Local);
  TTableItems = set of TTableItem;
  PTableItems = ^TTableItems;

  TMetaColumn = record
    Name: DBINAME;
    DataType: Word;
  end;
  TMetaColumns = array[1..High(Word)] of TMetaColumn;
  PMetaColumns = ^TMetaColumns;

  TUIBDbxTransaction = class(TUIBTransaction)
  private
    FTransactionDesc: TTransactionDesc;
    procedure SetTransactionDesc(const Value: TTransactionDesc);
    function GetWaitOnLocks: boolean;
    procedure SetWaitOnLocks(const Value: boolean);
    function GetIsolationLevel: TTransIsolationLevel;
    procedure SetIsolationLevel(const Value: TTransIsolationLevel);
  public
    property TransactionDesc: TTransactionDesc read FTransactionDesc write SetTransactionDesc;
    property WaitOnLocks: boolean read GetWaitOnLocks write SetWaitOnLocks;
    property IsolationLevel: TTransIsolationLevel read GetIsolationLevel write SetIsolationLevel;
    constructor Create(AOwner: TComponent); override;
  end;

  TUIBDbxDriver = class(TInterfacedObject, ISQLDriver)
  private
    FLibrary: String;
  protected
    function getSQLConnection(out pConn: ISQLConnection): SQLResult; stdcall;
    function SetOption(eDOption: TSQLDriverOption;
      PropValue: LongInt): SQLResult; stdcall;
    function GetOption(eDOption: TSQLDriverOption; PropValue: Pointer;
      MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
  public
    constructor Create(const ALibrary: String);
  end;

  TUIBDbxErrorSupport = class(TInterfacedObject)
  private
    FErrorMessage: String;
  protected
    function getErrorMessage(Error: PChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
  end;

  TUIBDbxConnection = class(TUIBDbxErrorSupport, ISQLConnection)
  private
    FOwner: TUIBDbxDriver;
    FDatabase: TUIBDataBase;
    FTransaction: TUIBDbxTransaction;
    FCallBackEvent: TSQLCallbackEvent;
    FTraceClientData: Integer;
    FTrimChar: boolean;
    FTransactionList: TList;
  {$IFDEF COMPILER7_UP}
  {$IFNDEF LINUX}
    FQualifiedName: string;
    FCatalogName: string;
    FSchemaName: string;
  {$ENDIF}
  {$ENDIF}
    procedure ClearTransactions;
    function FindTransaction(TransDesc: pTTransactionDesc;
      out Transaction: TUIBDbxTransaction): boolean;
    function GetLastActiveTransaction: TUIBDbxTransaction;
  protected
    function connect(ServerName: PChar; UserName: PChar;
      Password: PChar): SQLResult; stdcall;
    function disconnect: SQLResult; stdcall;
    function getSQLCommand(out pComm: ISQLCommand): SQLResult; stdcall;
    function getSQLMetaData(out pMetaData: ISQLMetaData): SQLResult; stdcall;
    function SetOption(eConnectOption: TSQLConnectionOption;
      lValue: LongInt): SQLResult; stdcall;
    function GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer;
      MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function beginTransaction(TranID: LongWord): SQLResult; stdcall;
    function commit(TranID: LongWord): SQLResult; stdcall;
    function rollback(TranID: LongWord): SQLResult; stdcall;
  public
    constructor Create(AOwner: TUIBDbxDriver; LibraryName: string);
    destructor Destroy; override;
    property LastActiveTransaction: TUIBDbxTransaction read GetLastActiveTransaction;
  end;

  TUIBDbxCommand = class(TUIBDbxErrorSupport, ISQLCommand)
  private
    FOwner: TUIBDbxConnection;
    FStatement: TUIBStatement;
    FStoredProc: boolean;
    function BuildStoredProc(const ProcName: string): string;
  protected
    function SetOption(eSqlCommandOption: TSQLCommandOption; ulValue: Integer): SQLResult; stdcall;
    {$IFDEF COMPILER6}
    function GetOption(eSqlCommandOption: TSQLCommandOption; var pValue: Integer;
      MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    {$ELSE}
    function GetOption(eSqlCommandOption: TSQLCommandOption; PropValue: Pointer;
      MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    {$ENDIF}
    function setParameter(ulParameter: Word; ulChildPos: Word; eParamType: TSTMTParamType;
      uLogType: Word; uSubType: Word; iPrecision: Integer; iScale: Integer;
      Length: LongWord; pBuffer: Pointer; lInd: Integer): SQLResult; stdcall;
    function getParameter(ParameterNumber: Word; ulChildPos: Word; Value: Pointer;
      Length: Integer; var IsBlank: Integer): SQLResult; stdcall;
    function prepare(SQL: PChar; ParamCount: Word): SQLResult; stdcall;
    function execute(var Cursor: ISQLCursor): SQLResult; stdcall;
    function executeImmediate(SQL: PChar; var Cursor: ISQLCursor): SQLResult; stdcall;
    function getNextCursor(var Cursor: ISQLCursor): SQLResult; stdcall;
    function getRowsAffected(var Rows: LongWord): SQLResult; stdcall;
    function close: SQLResult; stdcall;
  public
    constructor Create(AOwner: TUIBDbxConnection; Transaction: TUIBDbxTransaction);
    destructor Destroy; override;
  end;

  TUIBDbxCursor = class(TUIBDbxErrorSupport, ISQLCursor)
  private
    FOwner: TUIBDbxCommand;
    function getValue(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
  protected
    function SetOption(eOption: TSQLCursorOption; PropValue: LongInt): SQLResult; stdcall;
    function GetOption(eOption: TSQLCursorOption; PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getColumnCount(var pColumns: Word): SQLResult; stdcall;
    function getColumnNameLength(ColumnNumber: Word; var pLen: Word): SQLResult; stdcall;
    function getColumnName(ColumnNumber: Word; pColumnName: PChar): SQLResult; stdcall;
    function getColumnType(ColumnNumber: Word; var puType: Word; var puSubType: Word): SQLResult; stdcall;
    function getColumnLength(ColumnNumber: Word; var pLength: LongWord): SQLResult; stdcall;
    function getColumnPrecision(ColumnNumber: Word; var piPrecision: SmallInt): SQLResult; stdcall;
    function getColumnScale(ColumnNumber: Word; var piScale: SmallInt): SQLResult; stdcall;
    function isNullable(ColumnNumber: Word; var Nullable: LongBool): SQLResult; stdcall;
    function isAutoIncrement(ColumnNumber: Word; var AutoIncr: LongBool): SQLResult; stdcall;
    function isReadOnly(ColumnNumber: Word; var ReadOnly: LongBool): SQLResult; stdcall;
    function isSearchable(ColumnNumber: Word; var Searchable: LongBool): SQLResult; stdcall;
    function isBlobSizeExact(ColumnNumber: Word; var IsExact: LongBool): SQLResult; stdcall;
    function next: SQLResult; stdcall;
    function getString(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getShort(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getLong(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getDouble(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getBcd(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getTimeStamp(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getTime(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getDate(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getBytes(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getBlobSize(ColumnNumber: Word; var Length: LongWord; var IsBlank: LongBool): SQLResult; stdcall;
    function getBlob(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool; Length: LongWord): SQLResult; stdcall;
  public
    constructor Create(AOwner: TUIBDbxCommand);
    destructor Destroy; override;
  end;

  TUIBDbxMetadata = class(TUIBDbxErrorSupport, ISQLMetaData)
  private
    FOwner: TUIBDbxConnection;
  protected
    function SetOption(eDOption: TSQLMetaDataOption;
      PropValue: LongInt): SQLResult; stdcall;
    function GetOption(eDOption: TSQLMetaDataOption; PropValue: Pointer;
      MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getObjectList(eObjType: TSQLObjectType;
      out Cursor: ISQLCursor): SQLResult; stdcall;
    function getTables(TableName: PChar; TableType: LongWord;
      out Cursor: ISQLCursor): SQLResult; stdcall;
    function getProcedures(ProcedureName: PChar; ProcType: LongWord;
      out Cursor: ISQLCursor): SQLResult; stdcall;
    function getColumns(TableName: PChar; ColumnName: PChar; ColType: LongWord;
      out Cursor: ISQLCursor): SQLResult; stdcall;
    function getProcedureParams(ProcName: PChar; ParamName: PChar;
      out Cursor: ISQLCursor): SQLResult; stdcall;
    function getIndices(TableName: PChar; IndexType: LongWord;
      out Cursor: ISQLCursor): SQLResult; stdcall;
  public
    constructor Create(AOwner: TUIBDbxConnection);
  end;

  TUIBDbxMetadataCursor = class(TUIBDbxErrorSupport)
  private
    FOwner: TUIBDbxMetadata;
    FMetaColumns: PMetaColumns;
    FMetaColumnsCount: Integer;
    FCursor: Integer;
  protected
    function SetOption(eOption: TSQLCursorOption; PropValue: LongInt): SQLResult; stdcall;
    function GetOption(eOption: TSQLCursorOption; PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function getColumnCount(var pColumns: Word): SQLResult; stdcall;
    function getColumnNameLength(ColumnNumber: Word; var pLen: Word): SQLResult; stdcall;
    function getColumnName(ColumnNumber: Word; pColumnName: PChar): SQLResult; stdcall;
    function getColumnType(ColumnNumber: Word; var puType: Word; var puSubType: Word): SQLResult; stdcall;
    function getColumnLength(ColumnNumber: Word; var pLength: LongWord): SQLResult; stdcall;
    function getColumnPrecision(ColumnNumber: Word; var piPrecision: SmallInt): SQLResult; stdcall;
    function getColumnScale(ColumnNumber: Word; var piScale: SmallInt): SQLResult; stdcall;
    function isNullable(ColumnNumber: Word; var Nullable: LongBool): SQLResult; stdcall;
    function isAutoIncrement(ColumnNumber: Word; var AutoIncr: LongBool): SQLResult; stdcall;
    function isReadOnly(ColumnNumber: Word; var ReadOnly: LongBool): SQLResult; virtual; stdcall;
    function isSearchable(ColumnNumber: Word; var Searchable: LongBool): SQLResult; stdcall;
    function isBlobSizeExact(ColumnNumber: Word; var IsExact: LongBool): SQLResult; stdcall;
    function getValue(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; virtual; stdcall; abstract;
    function getString(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getShort(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getLong(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getDouble(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getBcd(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getTimeStamp(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getTime(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getDate(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getBytes(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; stdcall;
    function getBlobSize(ColumnNumber: Word; var Length: LongWord; var IsBlank: LongBool): SQLResult; stdcall;
    function getBlob(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool; Length: LongWord): SQLResult; stdcall;
  public
    constructor Create(AOwner: TUIBDbxMetadata; Columns: PMetaColumns; Count: Integer); virtual;
  end;

  TUIBDbxMetadataTables = class(TUIBDbxMetadataCursor, ISQLCursor)
  private
    FItems: TTableItems;
    FCurrentNode: TMetaNode;
    FRecno: Integer;
  protected
    function getValue(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; override; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(AOwner: TUIBDbxMetadata; Columns: PMetaColumns;
      Count: Integer; TableType: Cardinal); reintroduce;
  end;

  TIndicesResult = record
    RECNO           : Integer;  // A record number that uniquely identifies each record.
//  CATALOG_NAME    : string;   // The name of the catalog (database) that contains the index.
//  SCHEMA_NAME     : string;   // The name of the schema that identifies the owner of the index.
    TABLE_NAME      : string;   // The name of the table for which the index is defined.
    INDEX_NAME      : string;   // The name of the index.
    PKEY_NAME       : string;   // The name of the primary key.
    COLUMN_NAME     : string;   // The name of the column (field) in the index.
    COLUMN_POSITION : Smallint; // The position of this field in the index.
    INDEX_TYPE      : Smallint; // An eSQLIndexType value (C++) or index type constant (Delphi) that indicates any special properties of the index.
    SORT_ORDER      : string;   // Indicates whether the index sorts on this field in ascending (a) or descending (d) order.
//  FILTER          : string;    // A string that gives a filter condition limiting indexed records.
  end;

  TUIBDbxMetadataIndices = class(TUIBDbxMetadataCursor, ISQLCursor)
  private
    FResultSet: array of TIndicesResult;
  protected
    function getValue(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult; override; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(AOwner: TUIBDbxMetadata; Columns: PMetaColumns;
      Count: Integer; TableName: String; IndexType: LongWord); reintroduce;
  end;

  TComlumsResult = record
    RECNO           : Integer;  // A record number that uniquely identifies each record.
  //CATALOG_NAME    : string;   // The name of the catalog (database) that contains the table.
  //SCHEMA_NAME     : string;   // The name of the schema that identifies the owner of the table.
    TABLE_NAME      : string;   // The name of the table in which the column appears.
    COLUMN_NAME     : string;   // The name of the field (column).
    COLUMN_POSITION : Smallint; // The position of the column in its table.
    COLUMN_TYPE     : Integer;  // An eSQLColType value (C++) or column type constant (Delphi) that indicates the type of field.
    COLUMN_DATATYPE : Smallint; // The logical data type for the field.
    COLUMN_TYPENAME : string;   // A string describing the datatype. This is the same information as contained in COLUMN_DATATYPE and COLUMN_SUBTYPE, but in a form used in some DDL statements.
    COLUMN_SUBTYPE  : Smallint; // The logical data subtype for the field.
    COLUMN_PRECISION: Integer;  // The size of the field type (number of characters in a string, bytes in a bytes field, significant digits in a BCD value, members of an ADT field, and so on)
    COLUMN_SCALE    : Smallint; // The number of digits to the right of the decimal on BCD values, or descendants on ADT and array fields.
    COLUMN_LENGTH   : Integer;  // The number of bytes required to store field values.
    COLUMN_NULLABLE : Smallint; // 0 if the field requires a value, nonzero if it can be blank.
    ReadOnly        : boolean;   
  end;

  TUIBDbxMetadataColumns = class(TUIBDbxMetadataCursor, ISQLCursor)
  private
    FResultSet: array of TComlumsResult;
  protected
    function getValue(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; override; stdcall;
    function next: SQLResult; stdcall;
  public
    constructor Create(AOwner: TUIBDbxMetadata; Columns: PMetaColumns;
      Count: Integer; TableName: String); reintroduce;
  end;

  TUIBDbxMetadataProcedures = class(TUIBDbxMetadataCursor, ISQLCursor)
  protected
    function getValue(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; override; stdcall;
    function next: SQLResult; stdcall;
  end;

  TUIBDbxMetadataProcedureParams = class(TUIBDbxMetadataCursor, ISQLCursor)
  private
    FProcedure: TMetaProcedure;
  protected
    constructor Create(AOwner: TUIBDbxMetadata; Columns: PMetaColumns;
      Count: Integer; ProcedureName: String); reintroduce;
    function getValue(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; override; stdcall;
    function next: SQLResult; stdcall;
  end;

function getSQLDriverINTERBASE(sVendorLib: PChar; sResourceFile: PChar;
  out Obj): SQLResult; stdcall;

const
  SQL_ERROR = 255 + 1;

implementation
uses
{$IFDEF DEBUG}
  DbugIntf,
{$ENDIF}
  uibase, uibConst;

const
{$IFDEF COMPILER9_UP}
  SQL_SUCCESS = DBXERR_NONE;
{$ENDIF}

{$IFDEF DEBUG}
  MetaDataOptions: array[TSQLMetaDataOption] of string =
  ('eMetaCatalogName','eMetaSchemaName','eMetaDatabaseName',
   'eMetaDatabaseVersion','eMetaTransactionIsoLevel','eMetaSupportsTransaction',
   'eMetaMaxObjectNameLength','eMetaMaxColumnsInTable','eMetaMaxColumnsInSelect',
   'eMetaMaxRowSize','eMetaMaxSQLLength','eMetaObjectQuoteChar','eMetaSQLEscapeChar',
   'eMetaProcSupportsCursor','eMetaProcSupportsCursors','eMetaSupportsTransactions',
   'eMetaPackageName',

    'eMetaDefaultSchemaName');

  UIBFieldTypeStr: array[TUIBFieldType] of string =
  ('uftUnKnown', 'uftNumeric', 'uftChar', 'uftVarchar', 'uftCstring', 'uftSmallint',
    'uftInteger', 'uftQuad', 'uftFloat', 'uftDoublePrecision', 'uftTimestamp', 'uftBlob', 'uftBlobId',
    'uftDate', 'uftTime', 'uftInt64' , 'uftArray'{$IFDEF IB7_UP}, 'uftBoolean'{$ENDIF});

  CommandOptionStr: array[TSQLCommandOption] of string =(
      'eCommRowsetSize', 'eCommBlobSize', 'eCommBlockRead', 'eCommBlockWrite',
      'eCommParamCount', 'eCommNativeHandle', 'eCommCursorName', 'eCommStoredProc',
      'eCommSQLDialect', 'eCommTransactionID', 'eCommPackageName', 'eCommTrimChar',
      'eCommQualifiedName', 'eCommCatalogName', 'eCommSchemaName', 'eCommObjectName',
      'eCommQuotedObjectName',

      'eCommPrepareSQL', 'eCommDecimalSeparator'); 

{$ENDIF}


  InfoTables: array[0..4] of TMetaColumn = (
    (Name: 'RECNO';        DataType: fldINT32),   // A record number that uniquely identifies each record.
    (Name: 'CATALOG_NAME'; DataType: fldZSTRING), // The name of the catalog (database) that contains the table.
    (Name: 'SCHEMA_NAME';  DataType: fldZSTRING), // The name of the schema that identifies the owner of the table.
    (Name: 'TABLE_NAME';   DataType: fldZSTRING), // The name of the table.
    (Name: 'TABLE_TYPE';   DataType: fldINT32)    // An eSQLTableType value (C++) or table type constant (Delphi) that indicates the type of table.
  );
  InfoTablesCount : Integer = length(InfoTables);

  InfoIndices: array[0..10] of TMetaColumn = (
    (Name: 'RECNO';           DataType: fldINT32),   // A record number that uniquely identifies each record.
    (Name: 'CATALOG_NAME';    DataType: fldZSTRING), // The name of the catalog (database) that contains the index.
    (Name: 'SCHEMA_NAME';     DataType: fldZSTRING), // The name of the schema that identifies the owner of the index.
    (Name: 'TABLE_NAME';      DataType: fldZSTRING), // The name of the table for which the index is defined.
    (Name: 'INDEX_NAME';      DataType: fldZSTRING), // The name of the index.
    (Name: 'PKEY_NAME';       DataType: fldZSTRING), // The name of the primary key.
    (Name: 'COLUMN_NAME';     DataType: fldZSTRING), // The name of the column (field) in the index.
    (Name: 'COLUMN_POSITION'; DataType: fldINT16),   // The position of this field in the index.
    (Name: 'INDEX_TYPE';      DataType: fldINT16),   // An eSQLIndexType value (C++) or index type constant (Delphi) that indicates any special properties of the index.
    (Name: 'SORT_ORDER';      DataType: fldZSTRING), // Indicates whether the index sorts on this field in ascending (a) or descending (d) order.
    (Name: 'FILTER';          DataType: fldZSTRING)  // A string that gives a filter condition limiting indexed records.
  );
  InfoIndicesCount = Length(InfoIndices);

  InfoColumns : array[0..13] of TMetaColumn = (
    (Name: 'RECNO';	           DataType: fldINT32),   // A record number that uniquely identifies each record.
    (Name: 'CATALOG_NAME';	   DataType: fldZSTRING), // The name of the catalog (database) that contains the table.
    (Name: 'SCHEMA_NAME';	     DataType: fldZSTRING), // The name of the schema that identifies the owner of the table.
    (Name: 'TABLE_NAME';	     DataType: fldZSTRING), // The name of the table in which the column appears.
    (Name: 'COLUMN_NAME';	     DataType: fldZSTRING), // The name of the field (column).
    (Name: 'COLUMN_POSITION';	 DataType: fldINT16),   // The position of the column in its table.
    (Name: 'COLUMN_TYPE';	     DataType: fldINT32),   // An eSQLColType value (C++) or column type constant (Delphi) that indicates the type of field.
    (Name: 'COLUMN_DATATYPE';	 DataType: fldINT16),   // The logical data type for the field.
    (Name: 'COLUMN_TYPENAME';	 DataType: fldZSTRING), // A string describing the datatype. This is the same information as contained in COLUMN_DATATYPE and COLUMN_SUBTYPE, but in a form used in some DDL statements.
    (Name: 'COLUMN_SUBTYPE';	 DataType: fldINT16),   // The logical data subtype for the field.
    (Name: 'COLUMN_PRECISION'; DataType: fldINT32),   // The size of the field type (number of characters in a string, bytes in a bytes field, significant digits in a BCD value, members of an ADT field, and so on)
    (Name: 'COLUMN_SCALE';	   DataType: fldINT16),   // The number of digits to the right of the decimal on BCD values, or descendants on ADT and array fields.
    (Name: 'COLUMN_LENGTH';	   DataType: fldINT32),   // The number of bytes required to store field values.
    (Name: 'COLUMN_NULLABLE';	 DataType: fldINT16)    // 0 if the field requires a value, nonzero if it can be blank.
  );
  InfoColumnsCount = Length(InfoColumns);

  InfoProcedures: array[0..6] of TMetaColumn = (
    (Name: 'RECNO';        DataType: fldINT32),   // A record number that uniquely identifies each record.
    (Name: 'CATALOG_NAME'; DataType: fldZSTRING), // The name of the catalog (database) that contains the stored procedure.
    (Name: 'SCHEMA_NAME';  DataType: fldZSTRING), // The name of the schema that identifies the owner of the stored procedure.
    (Name: 'PROC_NAME';    DataType: fldZSTRING), // The name of the stored procedure.
    (Name: 'PROC_TYPE';    DataType: fldINT32),   // An eSQLProcType value (C++) or stored procedure type constant (Delphi) that indicates the type of stored procedure.
    (Name: 'IN_PARAMS';    DataType: fldINT16),   // The number of input parameters.
    (Name: 'OUT_PARAMS';   DataType: fldINT16)    // The number of output parameters.
  );
  InfoProceduresCount = Length(InfoProcedures);

  InfoProcedureParams: array[0..13] of TMetaColumn = (
    (Name: 'RECNO';           DataType: fldINT32),   // A record number that uniquely identifies each record.
    (Name: 'CATALOG_NAME';    DataType: fldZSTRING), // The name of the catalog (database) that contains the stored procedure.
    (Name: 'SCHEMA_NAME';     DataType: fldZSTRING), // The name of the schema that identifies the owner of the stored procedure.
    (Name: 'PARAM_POSITION';  DataType: fldINT32),   // UNDOCUMENTED
    (Name: 'PROC_NAME';       DataType: fldZSTRING), // The name of the procedure in which the parameter appears.
    (Name: 'PARAM_NAME';      DataType: fldZSTRING), // The name of the parameter.
    (Name: 'PARAM_TYPE';      DataType: fldINT16),   // A STMTParamType value that indicates whether the parameter is used for input, output, or result.
    (Name: 'PARAM_DATATYPE';  DataType: fldINT16),   // The logical data type for the parameter.
    (Name: 'PARAM_SUBTYPE';   DataType: fldINT16),   // The logical data subtype for the parameter.
    (Name: 'PARAM_TYPENAME';  DataType: fldZSTRING), // A string describing the datatype. This is the same information as contained in PARAM_DATATYPE and PARAM_SUBTYPE, but in a form used in some DDL statements.
    (Name: 'PARAM_PRECISION'; DataType: fldINT32),   // The size of the parameter type (number of characters in a string, bytes in a bytes field, significant digits in a BCD value, members of an ADT, and so on)
    (Name: 'PARAM_SCALE';     DataType: fldINT16),   // The number of digits to the right of the decimal on BCD values, or descendants on ADT and array values.
    (Name: 'PARAM_LENGTH';    DataType: fldINT32),   // The number of bytes required to store parameter values.
    (Name: 'PARAM_NULLABLE';  DataType: fldINT16)    // 0 if the parameter requires a value, nonzero if it can be blank.
  );
  InfoProcedureParamsCount = Length(InfoProcedureParams);

  UIBFieldTypeMap: array[TUIBFieldType] of Smallint =
   (fldUNKNOWN,fldBCD,fldZSTRING,fldZSTRING,fldZSTRING,fldINT16,fldINT32,
    fldUNKNOWN,fldFLOAT,fldFLOAT,fldTIMESTAMP,fldBLOB,fldBLOB,fldDATE,fldTIME,
    fldBCD, fldUNKNOWN{$IFDEF IB7_UP}, fldBOOL{$ENDIF});

function getSQLDriverINTERBASE(sVendorLib: PChar; sResourceFile: PChar;
  out Obj): SQLResult; stdcall;
begin {$IFDEF DEBUG}SendDebug('getSQLDriverINTERBASE');{$ENDIF}
  ISQLDriver(Obj) := TUIBDbxDriver.Create(sVendorLib);
  Result := SQL_SUCCESS;
end;

function DbxFieldLength(Field: TMetaBaseField): Integer;
begin
  case UIBFieldTypeMap[Field.FieldType] of
    fldBCD      : Result := SizeOf(TBCD);
    fldZSTRING  : Result := Field.Length;
    fldINT16    : Result := SizeOf(Smallint);
    fldINT32    : Result := SizeOf(Integer);
    fldINT64    : Result := SizeOf(TBCD);
    fldFLOAT    : Result := SizeOf(Double);
    fldTIMESTAMP: Result := SizeOf(TSQLTimeStamp);
    fldDATE     : Result := SizeOf(Integer);
    fldTIME     : Result := SizeOf(Cardinal);
  {$IFDEF IB7_UP}
    fldBOOL     : Result := SizeOf(WordBool);
  {$ENDIF}
    fldBLOB     : Result := SizeOf(TIscQuad);
  else
    Result := 0;
  end;
end;

{ TUIBDbxTransaction }

constructor TUIBDbxTransaction.Create(AOwner: TComponent);
begin
  inherited;
  FillChar(FTransactionDesc, SizeOf(TTransactionDesc), 0);
  Options := [tpReadCommitted, tpWait, tpRecVersion];
end;

function TUIBDbxTransaction.GetIsolationLevel: TTransIsolationLevel;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxTransaction.GetIsolationLevel:');{$ENDIF}
  Result := FTransactionDesc.IsolationLevel;
end;

function TUIBDbxTransaction.GetWaitOnLocks: boolean;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxTransaction.GetWaitOnLocks:');{$ENDIF}
  result := (tpWait in Options);
end;

procedure TUIBDbxTransaction.SetTransactionDesc(
  const Value: TTransactionDesc);
begin {$IFDEF DEBUG}SendDebug('TUIBDbxTransaction.SetTransactionDesc');{$ENDIF}
  FTransactionDesc := Value;
  IsolationLevel := FTransactionDesc.IsolationLevel;
end;

procedure TUIBDbxTransaction.SetIsolationLevel(
  const Value: TTransIsolationLevel);
begin {$IFDEF DEBUG}SendDebug('TUIBDbxTransaction.SetIsolationLevel');{$ENDIF}
  FTransactionDesc.IsolationLevel := Value;
  if (value in [xilREADCOMMITTED, xilREPEATABLEREAD]) then
    if WaitOnLocks then Options := [tpWait] else Options := [tpNoWait];
  if (value = xilREADCOMMITTED) then
    Options := Options + [tpReadCommitted, tpRecVersion];
end;

procedure TUIBDbxTransaction.SetWaitOnLocks(const Value: boolean);
begin {$IFDEF DEBUG}SendDebug('TUIBDbxTransaction.SetWaitOnLocks');{$ENDIF}
  if Value then
  begin
    Options := Options + [tpWait];
    Options := Options - [tpNoWait];
  end else
  begin
    Options := Options - [tpWait];
    Options := Options + [tpNoWait];
  end;
end;

{ TUIBDriver }

constructor TUIBDbxDriver.Create(const ALibrary: String);
begin
  FLibrary := ALibrary;
end;

function TUIBDbxDriver.GetOption(eDOption: TSQLDriverOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxDriver.GetOption');{$ENDIF}
  Result := SQL_SUCCESS;
end;

function TUIBDbxDriver.getSQLConnection(out pConn: ISQLConnection): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxDriver.getSQLConnection');{$ENDIF}
  Result := SQL_SUCCESS;
  try
    pConn := TUIBDbxConnection.Create(Self, FLibrary);
  except
    Result := SQL_ERROR;
    Exit;
  end;
end;

function TUIBDbxDriver.SetOption(eDOption: TSQLDriverOption;
  PropValue: Integer): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxDriver.SetOption');{$ENDIF}
  Result := SQL_SUCCESS;
end;

{ TUIBDbxErrorSupport }

function TUIBDbxErrorSupport.getErrorMessage(Error: PChar): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxErrorSupport.getErrorMessage');{$ENDIF}
  try
    move(PChar(FErrorMessage)^, Error^, Length(FErrorMessage));
    Result := SQL_SUCCESS;
  except
    on E: Exception do
      begin
        FErrorMessage := E.Message;
        Result := SQL_ERROR;
      end;
  end;
end;

function TUIBDbxErrorSupport.getErrorMessageLen(
  out ErrorLen: SmallInt): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxErrorSupport.getErrorMessageLen');{$ENDIF}
  ErrorLen := Length(FErrorMessage);
  Result := SQL_SUCCESS;
end;

{ TUIBDbxConnection }

function TUIBDbxConnection.beginTransaction(TranID: LongWord): SQLResult;
var Transaction: TUIBDbxTransaction;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.beginTransaction');{$ENDIF}
  try
    if not FindTransaction(pTTransactionDesc(TranID), Transaction) then
    begin
      Transaction := TUIBDbxTransaction.Create(nil);
      Transaction.DataBase := FDatabase;
      Transaction.TransactionDesc := pTTransactionDesc(TranID)^;
      FTransactionList.Add(Transaction);
      Transaction.StartTransaction;
    end;
  except
    on E: Exception do
      begin
        FErrorMessage := E.Message;
        Result := SQL_ERROR;
        Exit;
      end;
  end;
  Result := SQL_SUCCESS;
end;

procedure TUIBDbxConnection.ClearTransactions;
var i: SmallInt;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.ClearTransactions');{$ENDIF}
  for i := 0 to FTransactionList.Count - 1 do
    TObject(FTransactionList[i]).Free;
  FTransactionList.Clear;
end;

function TUIBDbxConnection.commit(TranID: LongWord): SQLResult;
var i: Integer;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.commit');{$ENDIF}
  Result := SQL_ERROR;
  try
    for i := 0 to FTransactionList.Count - 1 do
      with TUIBDbxTransaction(FTransactionList[i]) do
        if FTransactionDesc.TransactionID = pTTransactionDesc(TranID).TransactionID then
        begin
          Commit;
          TObject(FTransactionList[i]).free;
          FTransactionList.Delete(i);
          Result := SQL_SUCCESS;
          Break;
        end;
    if (Result = SQL_ERROR) then
      raise Exception.Create('Transaction ID not found.');
  except
    on E: Exception do
      begin
        FErrorMessage := E.Message;
        Exit;
      end;
  end;
  Result := SQL_SUCCESS;
end;

function TUIBDbxConnection.connect(ServerName, UserName,
  Password: PChar): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.connect');{$ENDIF}
  try
    FDatabase.DatabaseName := ServerName;
    FDatabase.UserName := UserName;
    FDatabase.PassWord := Password;
    FDatabase.Connected := True;
  except
    on E: Exception do
      begin
        FErrorMessage := E.Message;
        Result := SQL_ERROR;
        Exit;
      end;
  end;
  Result := SQL_SUCCESS;
end;

constructor TUIBDbxConnection.Create(AOwner: TUIBDbxDriver; LibraryName: string);
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.Create');{$ENDIF}
  FOwner := AOwner;
  FDatabase := TUIBDataBase.Create(nil);
  FDatabase.LibraryName := LibraryName;
  with FDatabase.MetaDataOptions do
  begin
    Objects := [OIDTable, OIDView, OIDProcedure];
    Tables := [OIDTableField, OIDPrimary, OIDForeign, OIDUnique, OIDIndex];
    Views := [OIDViewField];
    Procedures := [OIDProcFieldIn, OIDProcFieldOut];
    SysInfos := True;
  end;

  FTransaction := TUIBDbxTransaction.Create(nil);
  FTransaction.DataBase := FDatabase;
  FCallBackEvent := nil;
  FTraceClientData := 0;
  FTrimChar := False;
  FTransactionList := TList.Create;
end;

destructor TUIBDbxConnection.Destroy;
begin
  ClearTransactions;
  FTransactionList.Free;
  FTransaction.Free;
  FDatabase.Free;
  inherited;
end;

function TUIBDbxConnection.disconnect: SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.disconnect');{$ENDIF}
  try
    FDatabase.Connected := False;
  except
    on E: Exception do
      begin
        FErrorMessage := E.Message;
        Result := SQL_ERROR;
        Exit;
      end;
  end;
  Result := SQL_SUCCESS;
end;

function TUIBDbxConnection.FindTransaction(TransDesc: pTTransactionDesc;
  out Transaction: TUIBDbxTransaction): boolean;
var i: Integer;
begin
  for i := 0 to FTransactionList.Count - 1 do
    with TUIBDbxTransaction(FTransactionList[i]) do
      if TransactionDesc.TransactionID = TransDesc.TransactionID then
      begin
        Transaction := FTransactionList[i];
        result := True;
        exit;
      end;
  result := False;
  Transaction := nil;
end;

function TUIBDbxConnection.GetLastActiveTransaction: TUIBDbxTransaction;
begin
  if FTransactionList.Count > 0 then
    Result := FTransactionList.Last else
    Result := FTransaction;
end;

{$IFDEF DEBUG}
const
  SQLConnectionOptionStr : array[TSQLConnectionOption] of string =
      ('eConnAutoCommit', 'eConnBlockingMode', 'eConnBlobSize', 'eConnRoleName',
      'eConnWaitOnLocks', 'eConnCommitRetain', 'eConnTxnIsoLevel',
      'eConnNativeHandle', 'eConnServerVersion', 'eConnCallBack', 'eConnHostName',
      'eConnDatabaseName', 'eConnCallBackInfo', 'eConnObjectMode',
      'eConnMaxActiveComm', 'eConnServerCharSet', 'eConnSqlDialect',
      'eConnRollbackRetain', 'eConnObjectQuoteChar', 'eConnConnectionName',
      'eConnOSAuthentication', 'eConnSupportsTransaction', 'eConnMultipleTransaction',
      'eConnServerPort','eConnOnLine', 'eConnTrimChar', 'eConnQualifiedName',
      'eConnCatalogName', 'eConnSchemaName', 'eConnObjectName', 'eConnQuotedObjectName',
      'eConnCustomInfo', 'eConnTimeOut',

      'eConnConnectionString', 'eConnTDSPacketSize', 'eConnClientHostName',
      'eConnClientAppName', 'eConnCompressed', 'eConnEncrypted',
      'eConnPrepareSQL', 'eConnDecimalSeparator');

{$ENDIF}
function TUIBDbxConnection.GetOption(eDOption: TSQLConnectionOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
  procedure WriteString(const str: string);
  var len: Integer;
  begin
    len := System.Length(str)+1;
    if len > MaxLength then
    begin
      Result := SQL_ERROR;
      FErrorMessage := 'Buffer too small.'
    end else
    begin
      move(PChar(str)^, PropValue^, len);
      Length := len -1;
    end;
  end;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.GetOption: ' + SQLConnectionOptionStr[eDOption]);{$ENDIF}
  Result := SQL_SUCCESS;
  case eDOption of
    eConnRoleName: WriteString(FDatabase.Params.Values['sql_role_name']);
    eConnWaitOnLocks: Boolean(PropValue^) := FTransaction.WaitOnLocks;
    //eConnCommitRetain: Boolean(PropValue^) := FTransaction.AutoRetain;
    eConnTxnIsoLevel: TTransIsolationLevel(PropValue^) := FTransaction.IsolationLevel;
    eConnCallBack: TSQLCallBackEvent(PropValue^) := FCallBackEvent;
    eConnCallBackInfo : Integer(PropValue^) := FTraceClientData;
    eConnServerCharSet: WriteString(FDatabase.Params.Values['lc_ctype']);
    eConnSqlDialect: Integer(PropValue^) := FDatabase.SQLDialect;
    eConnNativeHandle: Integer(PropValue^) := Integer(FDatabase.DbHandle);
  {$IFDEF COMPILER7_UP}
    //eConnRollbackRetain: Boolean(PropValue^) := FTransaction.AutoRetain;
    eConnTrimChar: Boolean(PropValue^) := FTrimChar;
    eConnMaxActiveComm: Integer(PropValue^) := 0;
    {$IFNDEF LINUX}
    eConnQuotedObjectName: WriteString(FQualifiedName);
    eConnCatalogName: WriteString(FCatalogName);
    eConnSchemaName: WriteString(FSchemaName);
    eConnObjectName: WriteString(FQualifiedName);
    {$ENDIF}
  {$ENDIF}
  else
    result := DBXERR_NOTSUPPORTED;
  end;
end;

function TUIBDbxConnection.getSQLCommand(
  out pComm: ISQLCommand): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.getSQLCommand');{$ENDIF}
  pComm := TUIBDbxCommand.Create(Self, LastActiveTransaction);
  result := SQL_SUCCESS;
end;

function TUIBDbxConnection.getSQLMetaData(
  out pMetaData: ISQLMetaData): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.getSQLMetaData');{$ENDIF}
  pMetaData := TUIBDbxMetadata.Create(Self);
  result := SQL_SUCCESS;
end;

function TUIBDbxConnection.rollback(TranID: LongWord): SQLResult;
var i: Integer;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.rollback');{$ENDIF}
  Result := SQL_ERROR;
  try
    for i := 0 to FTransactionList.Count - 1 do
      with TUIBDbxTransaction(FTransactionList[i]) do
        if FTransactionDesc.TransactionID = pTTransactionDesc(TranID).TransactionID then
        begin
          RollBack;
          TObject(FTransactionList[i]).free;
          FTransactionList.Delete(i);
          Result := SQL_SUCCESS;
          Break;
        end;
    if (Result = SQL_ERROR) then
      raise Exception.Create('Transaction ID not found.');
  except
    on E: Exception do
      begin
        FErrorMessage := E.Message;
        Result := SQL_ERROR;
        Exit;
      end;
  end;
  Result := SQL_SUCCESS;
end;

function TUIBDbxConnection.SetOption(eConnectOption: TSQLConnectionOption;
  lValue: Integer): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxConnection.SetOption: ' + SQLConnectionOptionStr[eConnectOption]);{$ENDIF}
  Result := SQL_SUCCESS;
  case eConnectOption of
    eConnRoleName: FDatabase.Params.Values['sql_role_name'] := PChar(lValue);
    eConnWaitOnLocks: FTransaction.WaitOnLocks := boolean(lValue);
    //eConnCommitRetain: FTransaction.AutoRetain := boolean(lValue);
    eConnTxnIsoLevel: Ftransaction.IsolationLevel := TTransIsolationLevel(lValue);
    eConnCallBack: FCallBackEvent := TSQLCallBackEvent(lValue);
    eConnCallBackInfo : FTraceClientData := lValue;
    eConnServerCharSet: FDatabase.Params.Values['lc_ctype'] := PChar(lValue);
    eConnSqlDialect: FDatabase.SQLDialect := lValue;
  {$IFDEF COMPILER7_UP}
    //eConnRollbackRetain: FTransaction.AutoRetain := boolean(lValue);
    eConnTrimChar: FTrimChar := boolean(lValue);
    {$IFNDEF LINUX}
    eConnQualifiedName, eConnObjectName:
      begin
        if PChar(lValue)^ = '.' then inc(lValue);
        FQualifiedName := PChar(lValue);
      end;
    eConnCatalogName: FCatalogName := PChar(lValue);
    eConnSchemaName: FSchemaName := PChar(lValue);
    {$ENDIF}
  {$ENDIF}
  else
    result := DBXERR_NOTSUPPORTED;
  end;
end;

{ TUIBDbxCommand }

function TUIBDbxCommand.BuildStoredProc(const ProcName: string): string;
var i: Integer;
begin
  Result := UpperCase(ProcName);
  with TMetaDatabase(FOwner.FDatabase.GetMetadata).FindProcName(Result) do
    if InputFieldsCount > 0 then
    begin
      Result := Result + '(?';
      for i := 1 to InputFieldsCount - 1 do
        Result := Result + ',?';
      Result := Result + ')';
    end;
  Result := 'EXECUTE PROCEDURE ' + Result;
end;

function TUIBDbxCommand.close: SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCommand.close');{$ENDIF}
  try
    FStatement.Close;
    Result := SQL_SUCCESS;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := SQL_ERROR;
    end;
  end;
end;

constructor TUIBDbxCommand.Create(AOwner: TUIBDbxConnection; Transaction: TUIBDbxTransaction);
begin 
  FOwner := AOwner;
  FStatement:= TUIBStatement.Create(nil);
  FStatement.Transaction := Transaction;
  FStatement.CachedFetch := False;
  FStatement.UseCursor := False;
  FStoredProc := False;
end;

destructor TUIBDbxCommand.Destroy;
begin
  FStatement.Free;
  inherited;
end;

function TUIBDbxCommand.execute(var Cursor: ISQLCursor): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCommand.execute');{$ENDIF}
  Result := SQL_SUCCESS;
  try
    if (FStatement.Transaction = nil) then
      FStatement.Transaction := FOwner.LastActiveTransaction;
    if (FStatement.CurrentState < qsPrepare) then
      FStatement.Prepare;
    if FStatement.StatementType = stSelect then
    begin
      //FStatement.Open(False);
      FStatement.Execute;
      if (Cursor = nil) then
        Cursor := TUIBDbxCursor.Create(Self);
    end else
    begin
      FStatement.Execute;
      Cursor := nil;
    end;
  except
    on E: Exception do
      begin
        FErrorMessage := E.Message;
        Result := SQL_ERROR;
      end;
  end;
end;

function TUIBDbxCommand.executeImmediate(SQL: PChar;
  var Cursor: ISQLCursor): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCommand.executeImmediate');{$ENDIF}
  Result := SQL_SUCCESS;
  try
    if (FStatement.Transaction = nil) then
      FStatement.Transaction := FOwner.LastActiveTransaction;
    if FStoredProc then
      FStatement.SQL.Text := BuildStoredProc(SQL)else
      FStatement.SQL.Text := SQL;
    FStatement.ExecSQL;
    Cursor := nil;
  except
    on E: Exception do
    begin
      Result := SQL_ERROR;
      FErrorMessage := E.Message;
    end;
  end;

end;

function TUIBDbxCommand.getNextCursor(var Cursor: ISQLCursor): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCommand.getNextCursor');{$ENDIF}
  Result := SQL_SUCCESS;
  try
    if not FStoredProc then
    begin
      FStatement.Next;
      if (Cursor = nil) then
        Cursor := TUIBDbxCursor.Create(Self);
    end else
      Cursor := nil;
  except
    on E: Exception do
      begin
        FErrorMessage := E.Message;
        Result := SQL_ERROR;
      end;
  end;
end;

{$IFDEF COMPILER6}
function TUIBDbxCommand.GetOption(eSqlCommandOption: TSQLCommandOption;
  var pValue: Integer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult; stdcall;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCommand.GetOption(%s)');{$ENDIF}
  Result := SQL_SUCCESS;
  case eSqlCommandOption of
    eCommParamCount: pValue := Fstatement.Params.FieldCount;// readonly
    eCommNativeHandle: pValue := Integer(FStatement.StHandle);
    eCommStoredProc: pValue := Integer(FStoredProc);
    eCommTransactionID:
      begin
        if FStatement.Transaction <> nil then
          pValue := TUIBDbxTransaction(FStatement.Transaction).TransactionDesc.TransactionID else
          pValue := 0;
      end;
  end;
end;
{$ELSE}
function TUIBDbxCommand.GetOption(eSqlCommandOption: TSQLCommandOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
begin {$IFDEF DEBUG}SendDebugFmt('TUIBDbxCommand.GetOption(%s)', [CommandOptionStr[eSqlCommandOption]]);{$ENDIF}
  Result := SQL_SUCCESS;
  case eSqlCommandOption of
    eCommParamCount: Integer(PropValue^) := Fstatement.Params.FieldCount;// readonly
    eCommNativeHandle: Integer(PropValue^) := Integer(FStatement.StHandle);
    eCommStoredProc: boolean(PropValue^) := FStoredProc;
    eCommTransactionID:
      begin
        if FStatement.Transaction <> nil then
          Integer(PropValue^) := TUIBDbxTransaction(FStatement.Transaction).TransactionDesc.TransactionID else
          Integer(PropValue^) := 0;
      end;
  end;
end;
{$ENDIF}

function TUIBDbxCommand.getParameter(ParameterNumber, ulChildPos: Word;
  Value: Pointer; Length: Integer; var IsBlank: Integer): SQLResult;
var
  BlobHandle: IscBlobHandle;
  DbHandle: IscDbHandle;
  TrHandle: IscTrHandle;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCommand.getParameter');{$ENDIF}
  Result := SQL_SUCCESS;
  try
    dec(ParameterNumber, FStatement.Params.FieldCount + 1); // Zero based for results set
    if FStatement.Fields.IsNull[ParameterNumber] then
      IsBlank := 1 else
    with FStatement.Fields do
    case FieldType[ParameterNumber] of
      uftNumeric, uftInt64: TBCD(Value^) := StrToBCD(AsString[ParameterNumber]);
      uftChar:
        begin
          CheckRange(ParameterNumber);
          if Data.sqlvar[ParameterNumber].SqlLen + 1 < length then
            Length := Data.sqlvar[ParameterNumber].SqlLen;
          move(Data.sqlvar[ParameterNumber].SqlData^, Value^, Length);
          PChar(Value)[Length] := #0;
        end;
      uftVarchar:
        with Data.sqlvar[ParameterNumber] do
        begin
          CheckRange(ParameterNumber);
          if PVary(sqldata).vary_length + 1 < length then
            Length := PVary(sqldata).vary_length;
          move(PVary(sqldata).vary_string, Value^, Length);
          PChar(Value)[Length] := #0;
        end;
      uftSmallint: Smallint(Value^) := AsSmallint[ParameterNumber];
      uftInteger: Integer(Value^) := AsInteger[ParameterNumber];
      uftDoublePrecision, uftFloat: Double(Value^) := AsDouble[ParameterNumber];
      uftTimestamp:
        begin
          CheckRange(ParameterNumber);
          DecodeSQLDate(PISCTimeStamp(data.sqlvar[ParameterNumber].SqlData).timestamp_date,
            TSQLTimeStamp(Value^).Year, TSQLTimeStamp(Value^).Month, TSQLTimeStamp(Value^).Day);
          DecodeSQLTime(PISCTimeStamp(data.sqlvar[ParameterNumber].SqlData).timestamp_time,
            TSQLTimeStamp(Value^).Hour, TSQLTimeStamp(Value^).Minute, TSQLTimeStamp(Value^).Second, TSQLTimeStamp(Value^).Fractions);
        end;
      uftDate: Integer(Value^) := AsDate[ParameterNumber] + 693594;
      uftTime: Cardinal(Value^) := AsTime[ParameterNumber] div 10;
      uftBlob, uftBlobId:
        begin
          CheckRange(ParameterNumber);
          with FOwner.FDatabase.Lib do
          begin
            BlobHandle := nil;
            DbHandle := FOwner.FDatabase.DbHandle;
            TrHandle := FStatement.Transaction.TrHandle;
            BlobOpen(DbHandle, TrHandle, BlobHandle, AsQuad[ParameterNumber]);
            try
              BlobReadSizedBuffer(BlobHandle, Value, Length);
            finally
              BlobClose(BlobHandle);
            end;
          end;
        end;
      {$IFDEF IB7_UP}
      uftBoolean: Smallint(Value^) := AsSmallint[ParameterNumber];
      {$ENDIF}
    else
      Result := SQL_ERROR;
      FErrorMessage := 'Unmanaged datatype.: ' {$IFDEF DEBUG}+ UIBFieldTypeStr[FieldType[ParameterNumber]]{$ENDIF};
    end;
  except
    on E: Exception do
      begin
        Result := SQL_ERROR;
        FErrorMessage := E.Message;
      end;
  end;
end;

function TUIBDbxCommand.getRowsAffected(var Rows: LongWord): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCommand.getRowsAffected');{$ENDIF}
  Result := SQL_SUCCESS;
  try
    if (FStatement.CurrentState < qsPrepare) then
      Rows := 0 else
      Rows := FStatement.RowsAffected;
  except
    on E: Exception do
    begin
      Result := SQL_ERROR;
      FErrorMessage := E.Message;
    end;
  end;
end;

function TUIBDbxCommand.prepare(SQL: PChar; ParamCount: Word): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCommand.prepare');{$ENDIF}
  try
    if FStoredProc then
      FStatement.SQL.Text := BuildStoredProc(SQL) else
      FStatement.SQL.Text := SQL;
    if (FStatement.Transaction = nil) then
      FStatement.Transaction := FOwner.LastActiveTransaction;
    FStatement.Prepare;
    Result := SQL_SUCCESS;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := SQL_ERROR;
    end;
  end;
end;

function TUIBDbxCommand.SetOption(eSqlCommandOption: TSQLCommandOption;
  ulValue: Integer): SQLResult;
var i: Integer;
begin {$IFDEF DEBUG}SendDebugFmt('TUIBDbxCommand.SetOption(%s)', [CommandOptionStr[eSqlCommandOption]]);{$ENDIF}
  Result := SQL_SUCCESS;
  try
    case eSqlCommandOption of
      eCommStoredProc:
        begin
          FStoredProc := boolean(ulValue);
          Result := SQL_SUCCESS;
        end;
      eCommTransactionID:
        begin
          with FOwner.FTransactionList do
            for i := 0 to Count - 1 do
              if TUIBDbxTransaction(Items[i]).FTransactionDesc.TransactionID = LongWord(ulValue) then
              begin
                FStatement.Transaction := Items[i];
                Result := SQL_SUCCESS;
              end;
          if (Result = DBXERR_NOTSUPPORTED) then
            raise Exception.CreateFmt('Transaction ID %d not found', [ulValue]);
        end;
    end;
  except
    on E: Exception do
      begin
        result := SQL_ERROR;
        FErrorMessage := E.Message;
      end;
  end;
end;

function TUIBDbxCommand.setParameter(ulParameter, ulChildPos: Word;
  eParamType: TSTMTParamType; uLogType, uSubType: Word; iPrecision,
  iScale: Integer; Length: LongWord; pBuffer: Pointer;
  lInd: Integer): SQLResult;
var Curr: Currency;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCommand.setParameter');{$ENDIF}
  Result := SQL_SUCCESS;
  if eParamType <> paramIN then exit;;
  dec(ulParameter); // Zero based
  if lInd = 1 then
    FStatement.Params.IsNull[ulParameter] := True else
  case uLogType of
    fldZSTRING   :
      begin
        // FieldType      uSubType     NumBytes
        //---------------------------
        // ftString       0            Length+1
        // ftFixedChar    fldstFIXED   Length + 1
        // ftWideString   fldstUNICODE (Unexpected)
        // ftGuid         0            (Unexpected)
        // iPrecision = lenght s
        FStatement.Params.AsString[ulParameter] := PChar(pBuffer);
      end;
    fldDATE      :
      begin
        // FieldType      uSubType     NumBytes
        // ------------------------------------------------
        // ftDate         0            SizeOf(Integer)   db
        FStatement.Params.AsDate[ulParameter] := Integer(pBuffer^) - 693594;
      end;
    fldBLOB, fldBYTES, fldVARBYTES:
      begin
        // o---------------------------------------------o
        // | FieldType     | uSubType         | NumBytes |
        // |---------------O------------------O----------|
        // | ftBlob        | fldstBINARY      | variable |
        // | ftMemo        | fldstMEMO        | variable |
        // | ftGraphic     | fldstGRAPHIC     | variable |
        // | ftFmtMemo     | fldstFMTMEMO     | variable |
        // | ftParadoxOle  | fldstOLEOBJ      | variable |
        // | ftDBaseOle    | fldstDBSOLEOBJ   | variable |
        // | ftTypedBinary | fldstTYPEDBINARY | variable |
        // | ftOraBlob     | fldstHBINARY     | variable |
        // | ftOraClob     | fldstHMEMO       | variable |
        //   ftBytes         0
        //   ftVarBytes      0
        // o---------------|------------------|----------o
        FStatement.ParamsSetBlob(ulParameter, pBuffer, Length);
      end;
    fldBOOL:
      begin
        // FieldType      uSubType     NumBytes
        //----------------------------------------------
        // ftBoolean      0            SizeOf(WordBool);
        FStatement.Params.AsBoolean[ulParameter] := WordBool(pBuffer^)
      end;
    fldINT16: // ftSmallint: SizeOf(SmallInt);
      begin
        FStatement.Params.AsSmallint[ulParameter] := SmallInt(pBuffer^)
      end;
    fldINT32: // ftInteger ftAutoInc: SizeOf(Integer)
      begin
        FStatement.Params.AsInteger[ulParameter] := Integer(pBuffer^)
      end;
    fldFLOAT: // ftFloat ftCurrency: SizeOf(Double)
      begin
        FStatement.Params.AsDouble[ulParameter] := Double(pBuffer^);
      end;
    fldBCD: // ftBCD ftFMTBcd: SizeOf(TBcd) db
      begin
        if TBCD(pBuffer^).SignSpecialPlaces <= 4 then
        begin
          BCDToCurr(TBCD(pBuffer^), Curr);
          FStatement.Params.AsCurrency[ulParameter] := Curr;
        end else
          FStatement.Params.AsDouble[ulParameter] :=  BcdToDouble(TBCD(pBuffer^));
      end;
    fldTIME: // ftTime: SizeOf(Integer)
      begin
        FStatement.Params.AsTime[ulParameter] := Integer(pBuffer^) * 10;
      end;
    fldTIMESTAMP: // ftDateTime: SizeOf(Double)
      begin
        FStatement.Params.AsDateTime[ulParameter] := TimeStampToDateTime(MSecsToTimeStamp(Double(pBuffer^)));
      end;
    fldUINT16: // ftWord: SizeOf(Word)
      begin
        FStatement.Params.AsInteger[ulParameter] := Word(pBuffer^);
      end;
    fldDATETIME: // ftTimeStamp SizeOf( TSqlTimeStamp )
      begin
        FStatement.Params.AsDateTime[ulParameter] := SQLTimeStampToDateTime(TSQLTimeStamp(pBuffer^));
      end;
    fldINT64:
      begin
        // fldINT64: {ftLargeint}
        FStatement.Params.AsInt64[ulParameter] := Int64(pBuffer^);
      end;
  else
    // Unexpected: fldUINT32 fldFLOATIEEE fldLOCKINFO fldUINT64 fldFMTBCD
    // Unmanaged: fldUNKNOWN: {ftUnknown ftVariant ftInterface ftIDispatch}
    //
    // fldADT       : // ftADT: Length(VarToStr(FData)) + 1
    // fldCURSOR    : // ftCursor: 0
    // fldARRAY     : // ftArray 0
    // fldREF       : // ftReference 0
    // fldTABLE     : // ftDataSet 0
    Result := SQL_ERROR;
    FErrorMessage := 'Unmanaged datatype.';
  end;

end;


{ TUIBDbxCursor }

constructor TUIBDbxCursor.Create(AOwner: TUIBDbxCommand);
begin 
  FOwner := AOwner;
end;

destructor TUIBDbxCursor.Destroy;
begin
  FOwner.FStatement.CloseCursor;
  FOwner.FStatement.Transaction.CommitRetaining;
  inherited;
end;

function TUIBDbxCursor.getBcd(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getBcd');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.getBlob(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool; Length: LongWord): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getBlob');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.getBlobSize(ColumnNumber: Word;
  var Length: LongWord; var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getBlobSize');{$ENDIF}
  Result := SQL_SUCCESS;
  dec(ColumnNumber);
  try
    with FOwner.FStatement do
    begin
      IsBlank := Fields.IsNull[ColumnNumber];
      if not IsBlank then
        Length := FOwner.FStatement.FieldBlobSize(ColumnNumber);
    end;
  except
    on E: Exception do
    begin
      Result := SQL_ERROR;
      FErrorMessage := E.Message;
      {$IFDEF DEBUG}
        SendDebug(FErrorMessage);
      {$ENDIF}
    end;
  end;
  {$IFDEF DEBUG}
  SendDebugFmt('ColumnNumber: %d, Length: %d, IsBlank: %d', [ColumnNumber, Length, Byte(IsBlank)]);
  {$ENDIF}
end;

function TUIBDbxCursor.getBytes(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getBytes');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.getColumnCount(var pColumns: Word): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getColumnCount');{$ENDIF}
  Result := SQL_SUCCESS;
  try
    pColumns := FOwner.FStatement.Fields.FieldCount;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := SQL_ERROR;
    end;
  end;
end;

function TUIBDbxCursor.getColumnLength(ColumnNumber: Word;
  var pLength: LongWord): SQLResult;
begin
  Result := SQL_SUCCESS;
  try
    dec(ColumnNumber);
    with FOwner.FStatement.Fields do
    case FieldType[ColumnNumber] of
      uftNumeric: pLength := SizeOf(TBCD);
      uftChar, uftVarchar: pLength := data.sqlvar[ColumnNumber].SqlLen + 1;
      uftSmallint: pLength := SizeOf(Smallint);
      uftInteger: pLength := SizeOf(Integer);
      uftInt64: pLength := SizeOf(TBcd);
      uftFloat, uftDoublePrecision: pLength := SizeOf(Double);
      uftTimestamp: pLength := SizeOf(TSQLTimeStamp);
      uftBlob: pLength := 0; 
      uftDate: pLength := SizeOf(Integer);
      uftTime: pLength := SizeOf(Cardinal);
    {$IFDEF IB7_UP}
      uftBoolean: pLength := SizeOf(WordBool);
    {$ENDIF}
    else
      raise Exception.CreateFmt('unsuported datatype, column %s', [AliasName[ColumnNumber]]);
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := SQL_ERROR;
    end;
  end;
  {$IFDEF DEBUG}SendDebug(format('TUIBDbxCursor.getColumnLength(ColumnNumber: %d; var pLength: %d)', [ColumnNumber, pLength]));{$ENDIF}
end;

function TUIBDbxCursor.getColumnName(ColumnNumber: Word;
  pColumnName: PChar): SQLResult;
var len: Integer;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getColumnName('+FOwner.FStatement.Fields.AliasName[ColumnNumber-1] + ')');{$ENDIF}
  Result := SQL_SUCCESS;
  try
    dec(ColumnNumber);
    with FOwner.FStatement.Fields do
    begin
      CheckRange(ColumnNumber);
      len := data.sqlvar[ColumnNumber].AliasNameLength;
      if len > 32 then len := 32;
      move(data.sqlvar[ColumnNumber].AliasName, pColumnName^, len) ;
      if len < 32 then pColumnName[len] := #0;
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := SQL_ERROR;
    end;
  end;
end;

function TUIBDbxCursor.getColumnNameLength(ColumnNumber: Word;
  var pLen: Word): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getColumnNameLength');{$ENDIF}
  // never used
  Result := SQL_SUCCESS;
  try
    dec(ColumnNumber);
    with FOwner.FStatement.Fields do
    begin
      CheckRange(ColumnNumber);
      pLen := data.sqlvar[ColumnNumber].AliasNameLength;
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := SQL_ERROR;
    end;
  end;
end;

function TUIBDbxCursor.getColumnPrecision(ColumnNumber: Word;
  var piPrecision: SmallInt): SQLResult;
begin 
  Result := SQL_SUCCESS;
  try             
    dec(ColumnNumber);
    with FOwner.FStatement, Fields do
    case FieldType[ColumnNumber] of
      uftNumeric:
        with data.sqlvar[ColumnNumber] do
        case SqlType and not(1) of
          SQL_SHORT : if SqlScale = -4 then piPrecision := 5 else piPrecision := 4;
          SQL_LONG, SQL_DOUBLE: if SqlScale = -9 then piPrecision := 10 else piPrecision := 9;
          SQL_INT64, SQL_QUAD: if SqlScale = -18 then piPrecision := 19 else piPrecision := 18;
        else
          raise Exception.Create(EUIB_UNEXPECTEDERROR + ' ' + inttostr(data.sqlvar[ColumnNumber].SqlType));
        end;
      uftChar, uftVarchar: piPrecision := Data.sqlvar[ColumnNumber].SqlLen;
      uftInt64: piPrecision := 15;
    else
      piPrecision := 0;
    end;

  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := SQL_ERROR;
    end;
  end;
  {$IFDEF DEBUG}SendDebug(format('TUIBDbxCursor.getColumnPrecision(ColumnNumber: %d; var piPrecision: %d', [ColumnNumber, piPrecision]));{$ENDIF}
end;

function TUIBDbxCursor.getColumnScale(ColumnNumber: Word;
  var piScale: SmallInt): SQLResult;
begin
  Result := SQL_SUCCESS;
  try
    dec(ColumnNumber);
    with FOwner.FStatement.Fields do
    begin
      CheckRange(ColumnNumber);
      piScale := abs(Data.sqlvar[ColumnNumber].SqlScale);
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := SQL_ERROR;
    end;
  end;
  {$IFDEF DEBUG}SendDebug(format('TUIBDbxCursor.getColumnScale(ColumnNumber: %d; var piScale: %d)', [ColumnNumber, piScale]));{$ENDIF}
end;

function TUIBDbxCursor.getColumnType(ColumnNumber: Word; var puType,
  puSubType: Word): SQLResult;
begin
  Result := SQL_SUCCESS;
  try
    dec(ColumnNumber);
    puSubType := 0;
    with FOwner.FStatement.Fields do
    case FieldType[ColumnNumber] of
      uftNumeric: puType := fldBCD;
      uftChar, uftVarchar, uftCstring:
        begin
          puType := fldZSTRING;
          if FieldType[ColumnNumber] = uftChar then
            puSubType := fldstFIXED;
        end;
      uftSmallint: puType := fldINT16;
      uftInt64: puType := fldBCD;
      uftInteger: puType := fldINT32;
      uftFloat, uftDoublePrecision: puType :=fldFLOAT;
      uftTimestamp: puType := fldDATETIME;
      uftBlob:
        begin
          puType := fldBLOB;
          if data.sqlvar[ColumnNumber].SqlSubType = 1 then
            puSubType := fldstMEMO else
            puSubType := fldstGRAPHIC;
        end;
      uftDate: puType := fldDATE;
      uftTime: puType := fldTIME;
    {$IFDEF IB7_UP}
      uftBoolean: puType := fldBOOL;
    {$ENDIF}
    else
      raise Exception.CreateFmt('unsuported datatype, column %s', [AliasName[ColumnNumber]]);
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := SQL_ERROR;
    end;
  end;
  {$IFDEF DEBUG}SendDebug(format('TUIBDbxCursor.getColumnType(ColumnNumber: %d; var puType: %d, puSubType: %d)', [ColumnNumber, puType, puSubType]));{$ENDIF}
end;

function TUIBDbxCursor.getDate(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getDate');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.getDouble(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getDouble');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.getLong(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getLong');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.GetOption(eOption: TSQLCursorOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.GetOption');{$ENDIF}
  Result := SQL_SUCCESS;
end;

function TUIBDbxCursor.getShort(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getShort');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.getString(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getString');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.getTime(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getTime');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.getTimeStamp(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.getTimeStamp');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxCursor.getValue(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin 
  Result := SQL_SUCCESS;
  dec(ColumnNumber);
  try
    with FOwner.FStatement.Fields, Data.sqlvar[ColumnNumber] do
    begin
      if IsNull[ColumnNumber] then Exit;
      case fieldtype[ColumnNumber] of
        uftChar:
          begin
            move(SqlData^, Value^, SqlLen);
            PChar(Value)[SqlLen] := #0;
          end;
        uftVarchar:
          begin
            move(PVary(sqldata).vary_string, Value^, PVary(sqldata).vary_length);
            PChar(Value)[PVary(sqldata).vary_length] := #0;
          end;
        uftSmallint: PSmallint(Value)^ := PSmallint(SqlData)^;
        uftInteger: PInteger(Value)^ := PInteger(SqlData)^;
        uftDoublePrecision: PDouble(Value)^ := PDouble(SqlData)^;
        uftFloat: PDouble(Value)^ := PSingle(SqlData)^;
        uftNumeric, uftInt64: PBCD(Value)^ := StrToBcd(AsString[ColumnNumber]);
        uftTimestamp:
          begin
            DecodeSQLDate(PISCTimeStamp(SqlData).timestamp_date,
              TSQLTimeStamp(Value^).Year, TSQLTimeStamp(Value^).Month, TSQLTimeStamp(Value^).Day);
            DecodeSQLTime(PISCTimeStamp(SqlData).timestamp_time,
              TSQLTimeStamp(Value^).Hour, TSQLTimeStamp(Value^).Minute, TSQLTimeStamp(Value^).Second, TSQLTimeStamp(Value^).Fractions);
          end;
        uftBlob: FOwner.FStatement.ReadBlob(ColumnNumber, Value);
        uftDate: Integer(Value^) := AsDate[ColumnNumber] + 693594;
        uftTime: Cardinal(Value^) := AsTime[ColumnNumber] div 10;
      {$IFDEF IB7_UP}
        uftBoolean: WordBool(Value^) := AsBoolean[ColumnNumber];
      {$ENDIF}
      else
        raise Exception.Create(EUIB_UNEXPECTEDERROR);
      end;
      IsBlank := False;
    end;
  except
    on E: Exception do
    begin
      Result := SQL_ERROR;
      FErrorMessage := E.Message;
    end;
  end;
end;

function TUIBDbxCursor.isAutoIncrement(ColumnNumber: Word;
  var AutoIncr: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.isAutoIncrement');{$ENDIF}
  AutoIncr := False;
  Result := SQL_SUCCESS;
end;

function TUIBDbxCursor.isBlobSizeExact(ColumnNumber: Word;
  var IsExact: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.isBlobSizeExact');{$ENDIF}
  IsExact := True;
  Result := SQL_SUCCESS;
end;

function TUIBDbxCursor.isNullable(ColumnNumber: Word;
  var Nullable: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.isNullable');{$ENDIF}
  result := SQL_SUCCESS;
  dec(ColumnNumber);
  try
    Nullable := FOwner.FStatement.Fields.IsNullable[ColumnNumber];
  except
    on E: Exception do
    begin
      Result := SQL_ERROR;
      FErrorMessage := E.Message;
    end;
  end;
end;

function TUIBDbxCursor.isReadOnly(ColumnNumber: Word;
  var ReadOnly: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.isReadOnly');{$ENDIF}
  Result := SQL_SUCCESS;
  dec(ColumnNumber);
  try
    ReadOnly := FOwner.FStatement.Fields.RelName[ColumnNumber] = '';
  except
    on E: Exception do
    begin
      Result := SQL_ERROR;
      FErrorMessage := E.Message;
    end;
  end;
end;

function TUIBDbxCursor.isSearchable(ColumnNumber: Word;
  var Searchable: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.isSearchable');{$ENDIF}
  Result := SQL_SUCCESS;
  dec(ColumnNumber);
  try
    with FOwner.FStatement.Fields do
     Searchable := (RelName[ColumnNumber] <> '') and
       not(FieldType[ColumnNumber] in [uftBlob, uftBlobId]);
  except
    on E: Exception do
    begin
      Result := SQL_ERROR;
      FErrorMessage := E.Message;
    end;
  end;
end;

function TUIBDbxCursor.next: SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.next:');{$ENDIF}
  try
    FOwner.FStatement.Next;
    if FOwner.FStatement.Eof then
      Result := DBXERR_EOF else
      Result := SQL_SUCCESS
  except
    on E: Exception do
    begin
      Result := SQL_ERROR;
      FErrorMessage := E.Message;
    end;
  end;
end;

function TUIBDbxCursor.SetOption(eOption: TSQLCursorOption;
  PropValue: Integer): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxCursor.SetOption');{$ENDIF}
  Result := SQL_SUCCESS;
end;

{ TUIBDbxMetadata }

constructor TUIBDbxMetadata.Create(AOwner: TUIBDbxConnection);
begin
  FOwner := AOwner;
end;

function TUIBDbxMetadata.getColumns(TableName, ColumnName: PChar;
  ColType: LongWord; out Cursor: ISQLCursor): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadata.getColumns');{$ENDIF}
  Cursor := TUIBDbxMetadataColumns.Create(Self, @InfoColumns, InfoColumnsCount, TableName);
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadata.getIndices(TableName: PChar; IndexType: LongWord;
  out Cursor: ISQLCursor): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadata.getIndices');{$ENDIF}
  Cursor := TUIBDbxMetadataIndices.Create(Self, @InfoIndices, InfoIndicesCount,
   TableName, IndexType);
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadata.getObjectList(eObjType: TSQLObjectType;
  out Cursor: ISQLCursor): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadata.getObjectList');{$ENDIF}
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadata.GetOption(eDOption: TSQLMetaDataOption;
  PropValue: Pointer; MaxLength: SmallInt;
  out Length: SmallInt): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadata.GetOption');{$ENDIF}
  Result := SQL_SUCCESS;
  case eDOption of
    eMetaCatalogName: PChar(PropValue)^ := #0;
    eMetaSchemaName: PChar(PropValue)^ := #0;
    eMetaSupportsTransaction, eMetaSupportsTransactions: LongBool(PropValue^) := True;
    eMetaObjectQuoteChar: PChar(PropValue)^ := #0;
  end;
end;

function TUIBDbxMetadata.getProcedureParams(ProcName, ParamName: PChar;
  out Cursor: ISQLCursor): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadata.getProcedureParams');{$ENDIF}
  Cursor := TUIBDbxMetadataProcedureParams.Create(Self, @InfoProcedureParams,
    InfoProcedureParamsCount, ProcName);
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadata.getProcedures(ProcedureName: PChar;
  ProcType: LongWord; out Cursor: ISQLCursor): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadata.getProcedures');{$ENDIF}
  Cursor := TUIBDbxMetadataProcedures.Create(Self, @InfoProcedures, InfoProceduresCount);
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadata.getTables(TableName: PChar; TableType: LongWord;
  out Cursor: ISQLCursor): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadata.getTables');{$ENDIF}
  Cursor := TUIBDbxMetadataTables.Create(Self, @InfoTables, InfoTablesCount, TableType);
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadata.SetOption(eDOption: TSQLMetaDataOption;
  PropValue: Integer): SQLResult;
begin {$IFDEF DEBUG}SendDebug(format('TUIBDbxMetadata.SetOption(%s)', [MetaDataOptions[eDOption]]));{$ENDIF}
  Result := SQL_SUCCESS;
end;

{ TUIBDbxMetadataCursor }

constructor TUIBDbxMetadataCursor.Create(AOwner: TUIBDbxMetadata;
  Columns: PMetaColumns; Count: Integer);
begin
  FOwner := AOwner;
  FMetaColumns := Columns;
  FMetaColumnsCount := Count;
  FCursor := -1;
end;

function TUIBDbxMetadataCursor.getBcd(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getBcd');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.getBlob(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool; Length: LongWord): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getBlob');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.getBlobSize(ColumnNumber: Word;
  var Length: LongWord; var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getBlobSize');{$ENDIF}
  IsBlank := True;
  Length := 0;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.getBytes(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getBytes');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.getColumnCount(
  var pColumns: Word): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getColumnCount');{$ENDIF}
  pColumns := FMetaColumnsCount;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.getColumnLength(ColumnNumber: Word;
  var pLength: LongWord): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getColumnLength');{$ENDIF}
  Result := SQL_SUCCESS;
  case FMetaColumns[ColumnNumber].DataType of
    fldZSTRING: pLength := METADATALENGTH + 1;
    fldINT32  : pLength := SizeOf(Integer);
    fldINT16  : pLength := SizeOf(SmallInt);
  else
    Result := SQL_ERROR;
    FErrorMessage := EUIB_UNEXPECTEDERROR;
  end;
end;

function TUIBDbxMetadataCursor.getColumnName(ColumnNumber: Word;
  pColumnName: PChar): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getColumnName');{$ENDIF}
  move(FMetaColumns[ColumnNumber].Name, pColumnName^, SizeOf(DBINAME));
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.getColumnNameLength(ColumnNumber: Word;
  var pLen: Word): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getColumnNameLength');{$ENDIF}
  pLen := Length(Trim(FMetaColumns[ColumnNumber].Name));
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.getColumnPrecision(ColumnNumber: Word;
  var piPrecision: SmallInt): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getColumnPrecision');{$ENDIF}
  if FMetaColumns[ColumnNumber].DataType = fldZSTRING then
    piPrecision := METADATALENGTH else
    piPrecision := 0;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.getColumnScale(ColumnNumber: Word;
  var piScale: SmallInt): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getColumnScale');{$ENDIF}
  piScale := 0;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.getColumnType(ColumnNumber: Word;
  var puType, puSubType: Word): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getColumnType');{$ENDIF}
  puType := FMetaColumns[ColumnNumber].DataType;
    if puType = fldZSTRING then
      puSubType := fldstFIXED else
      puSubType := 0;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.getDate(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getDate');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.getDouble(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getDouble');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.getLong(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getLong');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.GetOption(eOption: TSQLCursorOption;
  PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.GetOption');{$ENDIF}
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.getShort(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getShort');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.getString(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getString');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.getTime(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getTime');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.getTimeStamp(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.getTimeStamp');{$ENDIF}
  Result := getValue(ColumnNumber, Value, IsBlank);
end;

function TUIBDbxMetadataCursor.isAutoIncrement(ColumnNumber: Word;
  var AutoIncr: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.isAutoIncrement');{$ENDIF}
  AutoIncr := False;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.isBlobSizeExact(ColumnNumber: Word;
  var IsExact: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.isBlobSizeExact');{$ENDIF}
  IsExact := False;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.isNullable(ColumnNumber: Word;
  var Nullable: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.isNullable');{$ENDIF}
  Nullable := False;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.isReadOnly(ColumnNumber: Word;
  var ReadOnly: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.isReadOnly');{$ENDIF}
  ReadOnly := True;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.isSearchable(ColumnNumber: Word;
  var Searchable: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.isSearchable');{$ENDIF}
  Searchable := True;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataCursor.SetOption(eOption: TSQLCursorOption;
  PropValue: Integer): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataCursor.SetOption');{$ENDIF}
  Result := SQL_SUCCESS;
end;

{ TUIBDbxMetadataTables }

constructor TUIBDbxMetadataTables.Create(AOwner: TUIBDbxMetadata;
  Columns: PMetaColumns; Count: Integer; TableType: Cardinal);
begin
  inherited Create(AOwner, Columns, Count);
  FItems := PTableItems(@TableType)^;
  FRecno := -1;
end;

function TUIBDbxMetadataTables.getValue(ColumnNumber: Word; Value: Pointer;
  var IsBlank: LongBool): SQLResult;
procedure writestring(const str: string);
begin
  move(PChar(str)^, Value^, Length(str) + 1);
end;
begin {$IFDEF DEBUG}SendDebug(format('TUIBDbxMetadataTables.getValue(ColumnNumber: %d)', [ColumnNumber]));{$ENDIF}
  IsBlank := False;
  inc(FRecno);
  case ColumnNumber of
    1: Integer(Value^) := FRecno;
    2: IsBlank := True; // CATALOG_NAME
    3: IsBlank := True; // SCHEMA_NAME
    4: writestring(FCurrentNode.Name);
    5: if (FCurrentNode is TMetaTable) then
      Integer(Value^) := eSQLTable else
      Integer(Value^) := eSQLView;
  end;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataTables.next: SQLResult;
var
  Meta: TMetadatabase;
label NextOne;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataTables.next');{$ENDIF}
  Meta := TMetadatabase(FOwner.FOwner.FDatabase.GetMetadata);

NextOne:
  inc(FCursor);
  if (FCursor < Meta.TablesCount) then
  begin
    if (copy(Meta.Tables[FCursor].Name, 0, 4) = 'RDB$') then
    begin
      if (SystemTable in FItems) then
      begin
        Result := SQL_SUCCESS;
        FCurrentNode := Meta.Tables[FCursor];
        Exit;
      end else
        goto NextOne;
    end else
    if (Table in FItems) then
    begin
      Result := SQL_SUCCESS;
      FCurrentNode := Meta.Tables[FCursor];
      Exit;
    end else
      goto NextOne;
  end else
  if (FCursor < Meta.TablesCount + Meta.ViewsCount) then
  begin
    if (view in FItems) then
    begin
      Result := SQL_SUCCESS;
      FCurrentNode := Meta.Views[FCursor - Meta.TablesCount];
    end else
      Result := DBXERR_EOF;
  end else
    Result := DBXERR_EOF;
end;

{ TUIBDbxMetadataIndices }

constructor TUIBDbxMetadataIndices.Create(AOwner: TUIBDbxMetadata;
  Columns: PMetaColumns; Count: Integer; TableName: String;
  IndexType: LongWord);
var
  Meta: TMetaTable;
  Index, i, j: Integer;
type
  TItem = (NonUnique,Unique,PrimaryKey);
  TITems = set of TItem;
  PItems = ^TITems;
begin
  inherited Create(AOwner, Columns, Count);
  TableName := UpperCase(TableName);
  Meta := TMetaDataBase(FOwner.FOwner.FDatabase.GetMetadata).FindTableName(TableName);
  if Meta = nil then  {it is a view ...}
    Exit;
  if (IndexType = 0) then
    PItems(@IndexType)^ := [NonUnique,Unique,PrimaryKey];

  // Find Size
  Index := 0;
  if (PrimaryKey in PItems(@IndexType)^) or
    (Unique in PItems(@IndexType)^) then
      for i := 0 to Meta.PrimaryCount - 1 do
        inc(Index, Meta.Primary[i].FieldsCount);
  if (Unique in PItems(@IndexType)^) then
  begin
    for i := 0 to Meta.UniquesCount - 1 do
      inc(Index, Meta.Uniques[i].FieldsCount);
    for i := 0 to Meta.IndicesCount - 1 do
      if Meta.Indices[i].Unique then
        inc(Index, Meta.Indices[i].FieldsCount);
  end;
  if (NonUnique in PItems(@IndexType)^) then
  begin
    for i := 0 to Meta.ForeignCount - 1 do
      inc(Index, Meta.Foreign[i].FieldsCount);
    for i := 0 to Meta.IndicesCount - 1 do
      if not Meta.Indices[i].Unique then
        inc(Index, Meta.Indices[i].FieldsCount);
  end;
  SetLength(FResultSet, Index);

  Index := 0;
  // PrimaryKey
  if (PrimaryKey in PItems(@IndexType)^) or
    (Unique in PItems(@IndexType)^) then
  for i := 0 to Meta.PrimaryCount - 1 do
    for j := 0 to Meta.Primary[i].FieldsCount - 1 do
    begin
      FResultSet[Index].RECNO           := Index + 1;
      FResultSet[Index].TABLE_NAME      := Meta.Name;
      FResultSet[Index].INDEX_NAME      := Meta.Primary[i].Name;
      FResultSet[Index].PKEY_NAME       := Meta.Primary[i].Name;
      FResultSet[Index].COLUMN_NAME     := Meta.Primary[i].Fields[j].Name;
      FResultSet[Index].COLUMN_POSITION := j;
      FResultSet[Index].INDEX_TYPE      := eSQLUnique or eSQLPrimaryKey;
      FResultSet[Index].SORT_ORDER      := 'A';
      inc(index);
    end;

  // Unique,
  if (Unique in PItems(@IndexType)^) then
  begin
    for i := 0 to Meta.UniquesCount - 1 do
      for j := 0 to Meta.Uniques[i].FieldsCount - 1 do
      begin
        FResultSet[Index].RECNO           := Index + 1;
        FResultSet[Index].TABLE_NAME      := Meta.Name;
        FResultSet[Index].INDEX_NAME      := Meta.Uniques[i].Name;
        FResultSet[Index].PKEY_NAME       := '';
        FResultSet[Index].COLUMN_NAME     := Meta.Uniques[i].Fields[j].Name;
        FResultSet[Index].COLUMN_POSITION := j;
        FResultSet[Index].INDEX_TYPE      := eSQLUnique;
        FResultSet[Index].SORT_ORDER      := 'A';
        inc(index);
      end;
    for i := 0 to Meta.IndicesCount - 1 do
      for j := 0 to Meta.Indices[i].FieldsCount - 1 do
      if Meta.Indices[i].Unique then
      begin
        FResultSet[Index].RECNO           := Index + 1;
        FResultSet[Index].TABLE_NAME      := Meta.Name;
        FResultSet[Index].INDEX_NAME      := Meta.Indices[i].Name;
        FResultSet[Index].PKEY_NAME       := '';
        FResultSet[Index].COLUMN_NAME     := Meta.Indices[i].Fields[j].Name;
        FResultSet[Index].COLUMN_POSITION := j;
        FResultSet[Index].INDEX_TYPE      := eSQLNonUnique;
        if (Meta.Indices[i].Order = IoAscending) then
          FResultSet[Index].SORT_ORDER := 'A' else
          FResultSet[Index].SORT_ORDER := 'D';
        inc(index);
      end;
  end;

  // NonUnique -> Foreign + Index
  if (NonUnique in PItems(@IndexType)^) then
  begin
    // Foreign
    for i := 0 to Meta.ForeignCount - 1 do
      for j := 0 to Meta.Foreign[i].FieldsCount - 1 do
      begin
        FResultSet[Index].RECNO           := Index + 1;
        FResultSet[Index].TABLE_NAME      := Meta.Name;
        FResultSet[Index].INDEX_NAME      := Meta.Foreign[i].Name;
        FResultSet[Index].PKEY_NAME       := '';
        FResultSet[Index].COLUMN_NAME     := Meta.Foreign[i].Fields[j].Name;
        FResultSet[Index].COLUMN_POSITION := j;
        FResultSet[Index].INDEX_TYPE      := eSQLNonUnique;
        FResultSet[Index].SORT_ORDER      := 'A';
        inc(index);
      end;
    // Indices
    for i := 0 to Meta.IndicesCount - 1 do
      for j := 0 to Meta.Indices[i].FieldsCount - 1 do
      if not Meta.Indices[i].Unique then
      begin
        FResultSet[Index].RECNO           := Index + 1;
        FResultSet[Index].TABLE_NAME      := Meta.Name;
        FResultSet[Index].INDEX_NAME      := Meta.Indices[i].Name;
        FResultSet[Index].PKEY_NAME       := '';
        FResultSet[Index].COLUMN_NAME     := Meta.Indices[i].Fields[j].Name;
        FResultSet[Index].COLUMN_POSITION := j;
        FResultSet[Index].INDEX_TYPE      := eSQLNonUnique;
        if (Meta.Indices[i].Order = IoAscending) then
          FResultSet[Index].SORT_ORDER := 'A' else
          FResultSet[Index].SORT_ORDER := 'D';
        inc(index);
      end;
  end;

end;

function TUIBDbxMetadataIndices.getValue(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataIndices.getValue');{$ENDIF}
  IsBlank := False;
  with FResultSet[FCursor] do
  case ColumnNumber of
     1: Integer(Value^) := RECNO;
     2: IsBlank := True; // CATALOG_NAME
     3: IsBlank := True; // SCHEMA_NAME
     4: move(PChar(TABLE_NAME)^, Value^, Length(TABLE_NAME) + 1);
     5: move(PChar(INDEX_NAME)^, Value^, Length(INDEX_NAME) + 1);
     6: move(PChar(PKEY_NAME)^, Value^, Length(PKEY_NAME) + 1);
     7: move(PChar(COLUMN_NAME)^, Value^, Length(COLUMN_NAME) + 1);
     8: Smallint(Value^) := COLUMN_POSITION;
     9: Smallint(Value^) := INDEX_TYPE;
    10: move(PChar(SORT_ORDER)^, Value^, Length(SORT_ORDER) + 1);
    11: IsBlank := True; // FILTER
  end;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataIndices.next: SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataIndices.next');{$ENDIF}
  inc(FCursor);
  if FCursor < length(FResultSet) then
    Result := SQL_SUCCESS else
    Result := DBXERR_EOF;
end;

{ TUIBDbxMetadataColumns }

constructor TUIBDbxMetadataColumns.Create(AOwner: TUIBDbxMetadata;
  Columns: PMetaColumns; Count: Integer; TableName: String);
procedure DesribeField(F: TMetaBaseField; Recno: Integer);
begin
  // A record number that uniquely identifies each record.
  FResultSet[Recno].RECNO           := Recno+1;
  // The name of the table in which the column appears.
  FResultSet[Recno].TABLE_NAME      := F.Parent.Name;
  // The name of the field (column).
  FResultSet[Recno].COLUMN_NAME     := F.Name;
  // The position of the column in its table.
  FResultSet[Recno].COLUMN_POSITION := Recno;
  // An eSQLColType value (C++) or column type constant (Delphi)
  // that indicates the type of field.
  FResultSet[Recno].COLUMN_TYPE     := eSQLDefault;
  // The logical data type for the field.
  FResultSet[Recno].COLUMN_DATATYPE := UIBFieldTypeMap[F.FieldType];
  // A string describing the datatype. This is the same information as contained
  // in COLUMN_DATATYPE and COLUMN_SUBTYPE, but in a form used in some DDL statements.
  FResultSet[Recno].COLUMN_TYPENAME := F.ShortFieldType;
  // The logical data subtype for the field.
  case F.FieldType of
    uftChar: FResultSet[Recno].COLUMN_SUBTYPE  := fldstFIXED;
    uftBlob, uftBlobId:
      begin
        if F.SubType = 1 then
          FResultSet[Recno].COLUMN_SUBTYPE := fldstMEMO else
          FResultSet[Recno].COLUMN_SUBTYPE := fldstGRAPHIC;
      end;
  else
    FResultSet[Recno].COLUMN_SUBTYPE := 0;
  end;
  // The size of the field type (number of characters in a string, bytes in a bytes field, significant digits in a BCD value, members of an ADT field, and so on)
  if F.FieldType = uftInt64 then
    FResultSet[Recno].COLUMN_PRECISION:= 15 else
    FResultSet[Recno].COLUMN_PRECISION:= F.Precision;
  // The number of digits to the right of the decimal on BCD values, or descendants on ADT and array fields.
  if F.FieldType = uftInt64 then
    FResultSet[Recno].COLUMN_SCALE    := 0 else
    FResultSet[Recno].COLUMN_SCALE    := F.Scale;
  // The number of bytes required to store field values.
  FResultSet[Recno].COLUMN_LENGTH := DbxFieldLength(F);
  // 0 if the field requires a value, nonzero if it can be blank.
  if (F is TMetaTableField) then
  begin
    if TMetaTableField(F).NotNull then
      FResultSet[Recno].COLUMN_NULLABLE := 0 else
      FResultSet[Recno].COLUMN_NULLABLE := 1;
  end else
    FResultSet[Recno].COLUMN_NULLABLE := 1;
end;
var
  Meta: TMetaDataBase;
  i, j: Integer;
begin
  inherited Create(AOwner, Columns, Count);
  TableName := UpperCase(TableName);
  Meta := TMetaDataBase(FOwner.FOwner.FDatabase.GetMetadata);

  // Is it a table ?
  for i := 0 to Meta.TablesCount - 1 do
    if Meta.Tables[i].Name = TableName then
    with Meta.Tables[i] do
    begin
      SetLength(FResultSet, FieldsCount);
      for j := 0 to FieldsCount - 1 do
        DesribeField(Fields[j], j);
      Exit;
    end;

  // Is it a View
  for i := 0 to Meta.ViewsCount - 1 do
    if Meta.Views[i].Name = TableName then
    with Meta.Views[i] do
    begin
      SetLength(FResultSet, FieldsCount);
      for j := 0 to FieldsCount - 1 do
        DesribeField(Fields[j], j);
    end;
end;

function TUIBDbxMetadataColumns.getValue(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
begin{$IFDEF DEBUG}SendDebug('TUIBDbxMetadataColumns.getValue');{$ENDIF}
  IsBlank := False;
  with FResultSet[FCursor] do
  case ColumnNumber of
     1: Integer(Value^) := RECNO;
     2: IsBlank := True; // CATALOG_NAME
     3: IsBlank := True; // SCHEMA_NAME
     4: move(PChar(TABLE_NAME)^, Value^, Length(TABLE_NAME) + 1);
     5: move(PChar(COLUMN_NAME)^, Value^, Length(COLUMN_NAME) + 1);
     6: Smallint(Value^) := COLUMN_POSITION;
     7: Integer(Value^) := COLUMN_TYPE;
     8: Smallint(Value^) := COLUMN_DATATYPE;
     9: move(PChar(COLUMN_TYPENAME)^, Value^, Length(COLUMN_TYPENAME) + 1);
    10: Smallint(Value^) := COLUMN_SUBTYPE;
    11: Integer(Value^) := COLUMN_PRECISION;
    12: Smallint(Value^) := COLUMN_SCALE;
    13: Integer(Value^) := COLUMN_LENGTH;
    14: Smallint(Value^) := COLUMN_NULLABLE;
  end;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataColumns.next: SQLResult;
begin{$IFDEF DEBUG}SendDebug('TUIBDbxMetadataColumns.next');{$ENDIF}
  inc(FCursor);
  if FCursor < length(FResultSet) then
    Result := SQL_SUCCESS else
    Result := DBXERR_EOF;
end;

{ TUIBDbxMetadataProcedure }

function TUIBDbxMetadataProcedures.getValue(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
procedure WriteString(Str: String);
begin
  move(PChar(Str)^, Value^, Length(Str)+1);
end;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataProcedures.getValue');{$ENDIF}
  IsBlank := False;
  with TMetaDatabase(FOwner.FOwner.FDatabase.GetMetadata) do
  case ColumnNumber of
    1: Integer(Value^) := FCursor + 1;        // RECNO
    2: IsBlank := True;                       //CATALOG_NAME
    3: IsBlank := True;                       //SCHEMA_NAME
    4: WriteString(Procedures[FCursor].Name); //PROC_NAME
    5: Integer(Value^) := 0;                  //PROC_TYPE
    6: Smallint(Value^) := Procedures[FCursor].InputFieldsCount; //IN_PARAMS
    7: Smallint(Value^) := Procedures[FCursor].OutputFieldsCount; //OUT_PARAMS
  end;
  Result := SQL_SUCCESS;
end;

function TUIBDbxMetadataProcedures.next: SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataProcedures.next');{$ENDIF}
  inc(FCursor);
  with TMetaDatabase(FOwner.FOwner.FDatabase.GetMetadata) do
  if FCursor < ProceduresCount then
    Result := SQL_SUCCESS else
    Result := DBXERR_EOF;
end;

{ TUIBDbxMetadataProcedureParams }

constructor TUIBDbxMetadataProcedureParams.Create(AOwner: TUIBDbxMetadata;
  Columns: PMetaColumns; Count: Integer; ProcedureName: String);
var i: Integer;
begin
  inherited Create(AOwner, Columns, Count);
  ProcedureName := UpperCase(ProcedureName);
  with TMetaDatabase(FOwner.FOwner.FDatabase.GetMetadata) do
    for i := 0 to ProceduresCount - 1 do
      if Procedures[i].Name = ProcedureName then
      begin
        FProcedure := Procedures[i];
        Exit;
      end;
  FProcedure := nil;
end;

function TUIBDbxMetadataProcedureParams.getValue(ColumnNumber: Word;
  Value: Pointer; var IsBlank: LongBool): SQLResult;
procedure WriteString(Str: String);
begin
  move(PChar(Str)^, Value^, Length(Str)+1);
end;
procedure DescribeField(F: TMetaField; Num: Integer; StmType: TSTMTParamType);
begin
  IsBlank := False;
  case ColumnNumber of
    1: Integer(Value^) := Num + 1; // RECNO
    2: IsBlank := True; // CATALOG_NAME
    3: IsBlank := True; // SCHEMA_NAME
    4: Integer(Value^) := Num + 1; // PARAM_POSITION
    5: WriteString(F.Parent.Name); // PROC_NAME
    6: WriteString(F.Name); // PARAM_NAME
    7: Smallint(Value^) := ord(StmType); // PARAM_TYPE
    8: Smallint(Value^) := UIBFieldTypeMap[F.FieldType]; // PARAM_DATATYPE
    9: case F.FieldType of // PARAM_SUBTYPE
         uftChar: Smallint(Value^) := fldstFIXED;
         uftBlob, uftBlobId:
           begin
             if F.SubType = 1 then
               Smallint(Value^) := fldstMEMO else
               Smallint(Value^) := fldstGRAPHIC;
           end;
       else
         Smallint(Value^) := 0;
       end;
    10: WriteString(F.ShortFieldType);  // PARAM_TYPENAME
    11: // PARAM_PRECISION
        if F.FieldType = uftInt64 then
          Integer(Value^) := 15 else
          Integer(Value^) :=  F.Precision;
    12: // PARAM_SCALE
        if F.FieldType = uftInt64 then
          Smallint(Value^) := 0 else
          Smallint(Value^) := F.Scale;
    13: Integer(Value^) := DbxFieldLength(F); // PARAM_LENGTH
    14: Smallint(Value^) := 1;
  end;
end;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataProcedureParams.getValue');{$ENDIF}
  if FCursor < FProcedure.InputFieldsCount then
    DescribeField(FProcedure.InputFields[FCursor], FCursor, paramIN) else
    DescribeField(FProcedure.OutputFields[FCursor - FProcedure.InputFieldsCount], FCursor, paramOUT);
  Result := SQL_SUCCESS;  
end;

function TUIBDbxMetadataProcedureParams.next: SQLResult;
begin {$IFDEF DEBUG}SendDebug('TUIBDbxMetadataProcedureParams.next');{$ENDIF}
  inc(FCursor);
  if (FProcedure = nil) or
  (FCursor < (FProcedure.InputFieldsCount + FProcedure.OutputFieldsCount)) then
    Result := SQL_SUCCESS else
    Result := DBXERR_EOF;
end;

end.
