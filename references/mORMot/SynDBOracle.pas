/// Oracle DB direct access classes (via OCI)
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBOracle;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
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
  - Adam Siwon (asiwon)
  - richard6688
  - mpv

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

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  dynlibs,
  {$endif}
  SysUtils,
  {$ifndef DELPHI5OROLDER}
  Variants,
  {$endif}
  Classes,
  Contnrs,
  SynCommons,
  SynTable, // for TSynTableStatement
  SynLog,
  SynDB;

{ -------------- Oracle Client Interface native connection  }

type
  /// exception type associated to the native Oracle Client Interface (OCI)
  ESQLDBOracle = class(ESQLDBException);

  POracleDate = ^TOracleDate;
  {$A-}
  /// memory structure used to store a date and time in native Oracle format
  // - follow the SQLT_DAT column type layout
  {$ifdef USERECORDWITHMETHODS}TOracleDate = record
    {$else}TOracleDate = object{$endif}
    Cent, Year, Month, Day, Hour, Min, Sec: byte;
    /// convert an Oracle date and time into Delphi TDateTime
    // - this method will ignore any date before 30 Dec 1899 (i.e. any
    // TDateTime result < 0), to avoid e.g. wrong DecodeTime() computation from
    // retrieved value: if you need to retrieve dates before 1899, you should
    // better retrieve the content using ISO-8601 text encoding
    function ToDateTime: TDateTime;
    /// convert an Oracle date and time into its textual expanded ISO-8601
    // - will fill up to 21 characters, including double quotes
    function ToIso8601(Dest: PUTF8Char): integer; overload;
    /// convert an Oracle date and time into its textual expanded ISO-8601
    // - return the ISO-8601 text, without double quotes
    procedure ToIso8601(var aIso8601: RawByteString); overload;
    /// convert Delphi TDateTime into native Oracle date and time format
    procedure From(const aValue: TDateTime); overload;
    /// convert textual ISO-8601 into native Oracle date and time format
    procedure From(const aIso8601: RawUTF8); overload;
    /// convert textual ISO-8601 into native Oracle date and time format
    procedure From(aIso8601: PUTF8Char; Length: integer); overload;
  end;
  {$A+}
  /// wrapper to an array of TOracleDate items
  TOracleDateArray = array[0..(maxInt div sizeof(TOracleDate))-1] of TOracleDate;

  /// event triggered when an expired password is detected
  // - will allow to provide a new password
  TOnPasswordExpired = function (Sender: TSQLDBConnection; var APassword: RawUTF8): Boolean of object;

  /// will implement properties shared by native Oracle Client Interface connections
  TSQLDBOracleConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  protected
    fRowsPrefetchSize: Integer;
    fBlobPrefetchSize: Integer;
    fStatementCacheSize: integer;
    fInternalBufferSize: integer;
    fEnvironmentInitializationMode: integer;
    fOnPasswordChanged: TNotifyEvent;
    fOnPasswordExpired: TOnPasswordExpired;
    fUseWallet: boolean;
    fIgnoreORA01453OnStartTransaction: boolean;
    function GetClientVersion: RawUTF8;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
    procedure PasswordChanged(const ANewPassword: RawUTF8);
  public
    /// initialize the connection properties
    // - we don't need a database name parameter for Oracle connection: only
    // aServerName is to be set
    // - you may specify the TNSName in aServerName, or a connection string
    // like '//host[:port]/[service_name]', e.g. '//sales-server:1523/sales'
    // - connection is opened globaly as UTF-8, to match the internal encoding
    // of our units; but CHAR / NVARCHAR2 fields will use the Oracle charset
    // as retrieved from the opened connection (to avoid any conversion error)
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBOracleConnection instance
    function NewConnection: TSQLDBConnection; override;
    /// extract the TNS listener name from a Oracle full connection string
    // - e.g. ExtractTnsName('1.2.3.4:1521/dbname') returns 'dbname'
    class function ExtractTnsName(const aServerName: RawUTF8): RawUTF8;
    /// determine if the SQL statement can be cached
    // - always returns false, to force server-side caching only on this driver
    function IsCachable(P: PUTF8Char): boolean; override;
    function SQLLimitClause(AStmt: TSynTableStatement): TSQLDBDefinitionLimitClause; override;
  published
    /// returns the Client version e.g. 'oci.dll rev. 11.2.0.1'
    property ClientVersion: RawUTF8 read GetClientVersion;
    /// the OCI initialization mode used for the connection
    // - equals OCI_EVENTS or OCI_THREADED by default, since will likely be
    // used in a multi-threaded context (even if this class is inheriting from
    // TSQLDBConnectionPropertiesThreadSafe), and  OCI_EVENTS is needed to support
    // Oracle RAC Connection Load Balancing
    // - can be tuned depending on the configuration or the Oracle version
    property EnvironmentInitializationMode: integer
      read fEnvironmentInitializationMode write fEnvironmentInitializationMode;
    /// the size (in bytes) of the internal buffer used to retrieve rows in statements
    // - default is 128 KB, which gives very good results
    property InternalBufferSize: integer read fInternalBufferSize write fInternalBufferSize;
    /// the size (in bytes) of rows data prefecth at OCI driver level
    // - is set to 128 KB by default, but may be changed for tuned performance
    property RowsPrefetchSize: integer read fRowsPrefetchSize write fRowsPrefetchSize;
    /// the size (in bytes) of LOB prefecth
    // - is set to 4096 (4 KB) by default, but may be changed for tuned performance
    property BlobPrefetchSize: integer read fBlobPrefetchSize write fBlobPrefetchSize;
    /// Password Expired event
    property OnPasswordExpired: TOnPasswordExpired read FOnPasswordExpired write FOnPasswordExpired;
    /// Password changed event
    property OnPasswordChanged: TNotifyEvent read FOnPasswordChanged write FOnPasswordChanged;
    /// the number of prepared statements cached by OCI on the Client side
    // - is set to 30 by default
    // - only used if UseCache=true
    property StatementCacheSize: integer read fStatementCacheSize write fStatementCacheSize;
    /// use the Secure External Password Store for Password Credentials
    // - see Oracle documentation
    // http://docs.oracle.com/cd/B28359_01/network.111/b28531/authentication.htm#DBSEG97906
    property UseWallet: boolean read fUseWallet write fUseWallet;
    /// When we execute a SELECT statement across a database link, a transaction lock is placed
    // on the undo segments (transaction is implicity started).
    // Setting this options to true allow to ignore ORA-01453 during
    // TSQLDBOracleConnection.StartTransaction call.
    // - see Oracle documentation
    // http://docs.oracle.com/cd/B28359_01/server.111/b28310/ds_appdev002.htm
    property IgnoreORA01453OnStartTransaction: boolean
      read fIgnoreORA01453OnStartTransaction write fIgnoreORA01453OnStartTransaction;
  end;

  /// implements a direct connection to the native Oracle Client Interface (OCI)
  TSQLDBOracleConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fEnv: pointer;
    fError: pointer;
    fServer: pointer;
    fContext: pointer;
    fSession: pointer;
    fTrans: pointer;
    fOCICharSet: cardinal;
    fType_numList: pointer;
    fType_strList: pointer;
    // match DB charset for CHAR/NVARCHAR2, nil for OCI_UTF8/OCI_AL32UTF8
    fAnsiConvert: TSynAnsiConvert;
    procedure STRToUTF8(P: PAnsiChar; var result: RawUTF8;
      ColumnDBCharSet,ColumnDBForm: Cardinal);
    {$ifndef UNICODE}
    procedure STRToAnsiString(P: PAnsiChar; var result: AnsiString;
      ColumnDBCharSet,ColumnDBForm: Cardinal);
    {$endif}
  public
    /// prepare a connection to a specified Oracle database server
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified Oracle database server
    // - should raise an Exception on error
    // - the connection will be globaly opened with UTF-8 encoding; for CHAR /
    // NVARCHAR2 fields, the DB charset encoding will be retrieved from the
    // server, to avoid any truncation during data retrieval
    // - BlobPrefetchSize, RowsPrefetchSize and StatementCacheSize field values
    // of the associated properties will be used to tune the opened connection
    procedure Connect; override;
    /// stop connection to the specified Oracle database server
    // - should raise an Exception on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// initialize a new SQL query statement for the given connection
    // - if UseCache=true, this overridden implementation will use server-side
    // Oracle statement cache - in this case, StatementCacheSize will define
    // how many statements are to be cached - not that IsCachable() has been
    // overriden to return false, so statement cache on client side is disabled
    // - the caller should free the instance after use
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    // - current implementation do not support nested transaction with those
    // methods: exception will be raised in such case
    // - by default, TSQLDBOracleStatement works in AutoCommit mode, unless
    // StartTransaction is called
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// allows to change the password of the current connected user
    // - will first launch the OnPasswordExpired event to retrieve the new
    // password, then change it and call OnPasswordChanged event on success
    function PasswordChange: Boolean;
  end;

  /// implements a statement via the native Oracle Client Interface (OCI)
  // - those statements can be prepared on the Delphi side, but by default we
  // enabled the OCI-side statement cache, not to reinvent the wheel this time
  // - note that bound OUT ftUTF8 parameters will need to be pre-allocated
  // before calling - e.g. via BindTextU(StringOfChar(3000),paramOut)
  // - you can also bind an TInt64DynArray or TRawUTF8DynArray as parameter to
  // be assigned later as an OCI_OBJECT so that you may write such statements:
  // ! var arr: TInt64DynArray = [1, 2, 3];
  // ! Query := TSQLDBOracleConnectionProperties.NewThreadSafeStatementPrepared(
  // !   'select * from table where table.id in '+
  // !     '(select column_value from table(cast(? as SYS.ODCINUMBERLIST)))');
  // ! Query.BindArray(1,arr);
  // ! Query.ExecutePrepared;
  // (use SYS.ODCIVARCHAR2LIST type cast for TRawUTF8DynArray values)
  TSQLDBOracleStatement = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fStatement: pointer;
    fError: pointer;
    fPreparedParamsCount: integer;
    fRowCount: cardinal;
    fRowBufferCount: cardinal;
    fRowFetched: cardinal;
    fRowFetchedCurrent: cardinal;
    fRowFetchedEnded: boolean;
    fRowBuffer: TByteDynArray;
    fBoundCursor: array of pointer;
    fInternalBufferSize: cardinal;
    // warning: shall be 32 bits aligned!
    fTimeElapsed: TPrecisionTimer;
    fUseServerSideStatementCache: boolean;
    function DateTimeToDescriptor(aDateTime: TDateTime): pointer;
    procedure FreeHandles(AfterError: boolean);
    procedure FetchTest(Status: integer);
    /// Col=0...fColumnCount-1
    function GetCol(Col: Integer; out Column: PSQLDBColumnProperty): pointer;
    // called by Prepare and CreateFromExistingStatement
    procedure SetColumnsForPreparedStatement;
    // called by Step and CreateFromExistingStatement
    procedure FetchRows;
  public
    /// create an OCI statement instance, from an existing OCI connection
    // - the Execute method can be called once per TSQLDBOracleStatement instance,
    // but you can use the Prepare once followed by several  ExecutePrepared methods
    // - if the supplied connection is not of TOleDBConnection type, will raise
    //   an exception
    constructor Create(aConnection: TSQLDBConnection); override;
    /// initialize the class from an existing OCI statement (and connection)
    // - to be called e.g. by ColumnCursor() for SQLT_RSET kind of column
    constructor CreateFromExistingStatement(aConnection: TSQLDBConnection; aStatement: pointer);
    /// release all associated memory and OCI handles
    destructor Destroy; override;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESQLDBOracle on any error
    // - if aSQL requires a trailing ';', you should end it with ';;' e.g. for
    // $ DB.ExecuteNoResult(
    // $  'CREATE OR REPLACE FUNCTION ORA_POC(MAIN_TABLE IN VARCHAR2, REC_COUNT IN NUMBER, BATCH_SIZE IN NUMBER) RETURN VARCHAR2' +
    // $  ' AS LANGUAGE JAVA' +
    // $  ' NAME ''OraMain.selectTable(java.lang.String, int, int) return java.lang.String'';;', []);
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: Boolean=false); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - raise an ESQLDBOracle on any error
    procedure ExecutePrepared; override;

    /// After a statement has been prepared via Prepare() + ExecutePrepared() or
    // Execute(), this method must be called one or more times to evaluate it
    // - you shall call this method before calling any Column*() methods
    // - return TRUE on success, with data ready to be retrieved by Column*()
    // - return FALSE if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - access the first or next row of data from the SQL Statement result:
    // if SeekFirst is TRUE, will put the cursor on the first row of results,
    // otherwise, it will fetch one row of data, to be called within a loop
    // - raise an ESQLDBOracle on any error
    function Step(SeekFirst: boolean=false): boolean; override;
    /// finalize the OCI cursor resources - not implemented yet
    procedure ReleaseRows; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    // - should retrieve directly the 64 bit Currency content, to avoid
    // any rounding/conversion error from floating-point types
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUTF8(Col: integer): RawUTF8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    // - ColumnBlob() will return the binary content of the field is was not ftBlob,
    // e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
    // or a direct mapping of the RawUnicode
    function ColumnBlob(Col: integer): RawByteString; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    // - this function will return the BLOB content as a TBytes
    // - this default virtual method will call ColumnBlob()
    function ColumnBlobBytes(Col: integer): TBytes; override;
    /// read a blob Column into the Stream parameter
    procedure ColumnBlobToStream(Col: integer; Stream: TStream); override;
    /// write a blob Column into the Stream parameter
    // - expected to be used with 'SELECT .. FOR UPDATE' locking statements
    procedure ColumnBlobFromStream(Col: integer; Stream: TStream); override;
    /// return a Column as a variant
    // - this implementation will retrieve the data with no temporary variable
    // (since TQuery calls this method a lot, we tried to optimize it)
    // - a ftUTF8 content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob content will be mapped into a TBlobData AnsiString variant
    function ColumnToVariant(Col: integer; var Value: Variant): TSQLDBFieldType; override;
    /// return a Column as a TSQLVar value, first Col is 0
    // - the specified Temp variable will be used for temporary storage of
    // svtUTF8/svtBlob values
    // - this implementation will retrieve the data with no temporary variable,
    // and handling ftCurrency/NUMBER(22,0) as fast as possible, directly from
    // the memory buffers returned by OCI: it will ensure best performance
    // possible when called from TSQLVirtualTableCursorExternal.Column method
    // as defined in mORMotDB unit (i.e. mORMot external DB access)
    procedure ColumnToSQLVar(Col: Integer; var Value: TSQLVar;
      var Temp: RawByteString); override;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - fast overridden implementation with no temporary variable (about 20%
    // faster when run over high number of data rows)
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure ColumnsToJSON(WR: TJSONWriter); override;
    /// return a special CURSOR Column content as a SynDB result set
    // - Cursors are not handled internally by mORMot, but Oracle usually use
    // such structures to get data from strored procedures
    // - such columns are mapped as ftUTF8, with the rows converted to JSON
    // - this overridden method will allow direct access to the data rows
    function ColumnCursor(Col: integer): ISQLDBRows; override;

    /// bind a special CURSOR parameter to be returned as a SynDB result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such parameters are mapped as ftUnknown, and is always of paramOut type
    // - use BoundCursor() method to retrieve the corresponding ISQLDBRows after
    // execution of the statement
    // - this overridden method will prepare direct access to the data rows
    procedure BindCursor(Param: integer); override;
    /// return a special CURSOR parameter content as a SynDB result set
    // - this method is not about a column, but a parameter defined with
    // BindCursor() before method execution
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - this method allow direct access to the data rows after execution
    // - this overridden method will allow direct access to the data rows
    function BoundCursor(Param: Integer): ISQLDBRows; override;

    /// returns the number of rows updated by the execution of this statement
    function UpdateCount: integer; override;
  end;


var
  /// optional folder where the Oracle Client Library is to be searched
  // - by default, the oci.dll library is searched in the system PATH, then
  // in %ORACLE_HOME%\bin
  // - you can specify here a folder name in which the oci.dll is to be found
  SynDBOracleOCIpath: TFileName;

const
  // defined here for overriding OCI_CHARSET_UTF8/OCI_CHARSET_WIN1252 if needed
  OCI_UTF8 = $367;
  OCI_AL32UTF8 = $369;
  OCI_UTF16ID = 1000;
  OCI_WE8MSWIN1252 = 178;

var
  /// the OCI charset used for UTF-8 encoding
  // - OCI_UTF8 is a deprecated encoding, and OCI_AL32UTF8 should be preferred
  // - but you can fallback for OCI_UTF8 for compatibility purposes
  OCI_CHARSET_UTF8: cardinal = OCI_AL32UTF8;

  /// the OCI charset used for WinAnsi encoding
  OCI_CHARSET_WIN1252: cardinal = OCI_WE8MSWIN1252;

  /// how many blob chunks should be handled at once
  SynDBOracleBlobChunksCount: integer = 250;


implementation

{ TOracleDate }

// see http://download.oracle.com/docs/cd/B28359_01/appdev.111/b28395/oci03typ.htm#sthref389

function TOracleDate.ToDateTime: TDateTime;
begin
  if (PInteger(@self)^=0) and (PInteger(PtrUInt(@self)+3)^=0) then
    // Cent=Year=Month=Day=Hour=Main=Sec=0 -> returns 0
    result := 0 else begin
    if Cent<=100 then // avoid TDateTime values < 0 (generates wrong DecodeTime)
      result := 0 else
      result := EncodeDate((Cent-100)*100+Year-100,Month,Day);
    if (Hour>1) or (Min>1) or (Sec>1) then
      result := result+EncodeTime(Hour-1,Min-1,Sec-1,0);
  end;
end;

procedure TOracleDate.ToIso8601(var aIso8601: RawByteString);
var tmp: array[0..23] of AnsiChar;
begin
  if (PInteger(@self)^=0) and (PInteger(PtrUInt(@self)+3)^=0) then
    // Cent=Year=Month=Day=Hour=Main=Sec=0 -> stored as ""
    aIso8601 := '' else begin
    DateToIso8601PChar(tmp,true,(Cent-100)*100+Year-100,Month,Day);
    if (Hour>1) or (Min>1) or (Sec>1) then begin
      TimeToIso8601PChar(@tmp[10],true,Hour-1,Min-1,Sec-1,0,'T');
      SetString(aIso8601,tmp,19); // we use 'T' as TTextWriter.AddDateTime
    end else
      SetString(aIso8601,tmp,10); // only date
  end;
end;

function TOracleDate.ToIso8601(Dest: PUTF8Char): integer;
var Y: cardinal;
begin
  Dest^ := '"';
  if (PInteger(@self)^=0) and (PInteger(PtrUInt(@self)+3)^=0) then
    // Cent=Year=Month=Day=Hour=Main=Sec=0 -> stored as ""
    result := 2 else begin
    Y := (Cent-100)*100+Year-100;
    if Y>9999 then // avoid integer overflow -> stored as ""
      result := 2 else begin
      DateToIso8601PChar(Dest+1,true,Y,Month,Day);
      if (Hour>1) or (Min>1) or (Sec>1) then begin
        TimeToIso8601PChar(Dest+11,true,Hour-1,Min-1,Sec-1,0,'T');
        result := 21; // we use 'T' as TTextWriter.AddDateTime
      end else
        result := 12; // only date
    end;
  end;
  Dest[result-1] := '"';
end;

procedure TOracleDate.From(const aValue: TDateTime);
var T: TSynSystemTime;
begin
  if aValue<=0 then begin
    PInteger(@self)^ := 0;
    PInteger(PtrUInt(@self)+3)^ := 0; // set Day=Hour=Min=Sec to 0
    exit; // supplied TDateTime value = 0 -> store as null date
  end;
  T.FromDateTime(aValue);
  Cent := (T.Year div 100)+100;
  Year := (T.Year mod 100)+100;
  Month := T.Month;
  Day := T.Day;
  if (T.Hour<>0) or (T.Minute<>0) or (T.Second<>0) then begin
    Hour := T.Hour+1;
    Min := T.Minute+1;
    Sec := T.Second+1;
  end else begin
    Hour := 1;
    Min := 1;
    Sec := 1;
  end;
end;

procedure TOracleDate.From(const aIso8601: RawUTF8);
begin
  From(pointer(aIso8601),length(aIso8601));
end;

procedure TOracleDate.From(aIso8601: PUTF8Char; Length: integer);
var Value: QWord;
    Value32: cardinal absolute Value;
    Y: cardinal;
    NoTime: boolean;
begin
  Value := Iso8601ToTimeLogPUTF8Char(aIso8601,Length,@NoTime);
  if Value=0 then begin
    PInteger(@self)^ := 0;
    PInteger(PtrUInt(@self)+3)^ := 0;  // set Day=Hour=Min=Sec to 0
    exit; // invalid ISO-8601 text -> store as null date
  end;
  Y := Value shr (6+6+5+5+4);
  Cent := (Y div 100)+100;
  Year := (Y mod 100)+100;
  Month := ((Value32 shr (6+6+5+5)) and 15)+1;
  Day := ((Value32 shr (6+6+5)) and 31)+1;
  if NoTime then begin
    Hour := 1;
    Min := 1;
    Sec := 1;
    exit;
  end;
  Hour := ((Value32 shr (6+6)) and 31)+1;
  Min := ((Value32 shr 6) and 63)+1;
  Sec := (Value32 and 63)+1;
end;


{ Native OCI access interface }

type
  { Generic Oracle Types }
  sword   = Integer;
  eword   = Integer;
  uword   = LongInt;
  sb4     = Integer;
  ub4     = LongInt;
  sb2     = SmallInt;
  ub2     = Word;
  sb1     = ShortInt;
  ub1     = Byte;
  dvoid   = Pointer;
  text    = PAnsiChar;
  OraText = PAnsiChar;
  size_T  = PtrUInt;

  pub1 = ^ub1;
  psb1 = ^sb1;
  pub2 = ^ub2;
  psb2 = ^sb2;
  pub4 = ^ub4;
  psb4 = ^sb4;
  pdvoid = ^dvoid;

  { Handle Types }
  POCIHandle = Pointer;
  PPOCIHandle = ^Pointer;
  POCIEnv = POCIHandle;
  POCIServer = POCIHandle;
  POCIError = POCIHandle;
  POCISvcCtx = POCIHandle;
  POCIStmt = POCIHandle;
  POCIDefine = POCIHandle;
  POCISession = POCIHandle;
  POCIBind = POCIHandle;
  POCIDescribe = POCIHandle;
  POCITrans = POCIHandle;

  { Descriptor Types }
  POCIDescriptor = Pointer;
  PPOCIDescriptor = ^POCIDescriptor;
  POCISnapshot = POCIDescriptor;
  POCILobLocator = POCIDescriptor;
  POCIParam = POCIDescriptor;
  POCIRowid = POCIDescriptor;
  POCIComplexObjectComp = POCIDescriptor;
  POCIAQEnqOptions = POCIDescriptor;
  POCIAQDeqOptions = POCIDescriptor;
  POCIAQMsgProperties = POCIDescriptor;
  POCIAQAgent = POCIDescriptor;
  POCIDate = POCIDescriptor;
  POCIDateTime = POCIDescriptor;
  POCIString = POCIDescriptor;
  POCIType = POCIDescriptor;
  POCIArray = POCIDescriptor;
  POCIColl = POCIDescriptor;

  /// OCIDuration - OCI object duration
  // - A client can specify the duration of which an object is pinned (pin
  // duration) and the duration of which the object is in memory (allocation
  // duration).  If the objects are still pinned at the end of the pin duration,
  // the object cache manager will automatically unpin the objects for the
  // client. If the objects still exist at the end of the allocation duration,
  // the object cache manager will automatically free the objects for the client.
  // - Objects that are pinned with the option OCI_DURATION_TRANS will get unpinned
  // automatically at the end of the current transaction.
  // - Objects that are pinned with the option OCI_DURATION_SESSION will get
  // unpinned automatically at the end of the current session (connection).
  // - The option OCI_DURATION_NULL is used when the client does not want to set
  // the pin duration.  If the object is already loaded into the cache, then the
  // pin duration will remain the same.  If the object is not yet loaded, the
  // pin duration of the object will be set to OCI_DURATION_DEFAULT.
  OCIDuration = ub2;
  /// The OCITypeCode type is interchangeable with the existing SQLT type which is a ub2
  OCITypeCode = ub2;

const
  { OCI Handle Types }
  OCI_HTYPE_FIRST               = 1;
  OCI_HTYPE_ENV                 = 1;
  OCI_HTYPE_ERROR               = 2;
  OCI_HTYPE_SVCCTX              = 3;
  OCI_HTYPE_STMT                = 4;
  OCI_HTYPE_BIND                = 5;
  OCI_HTYPE_DEFINE              = 6;
  OCI_HTYPE_DESCRIBE            = 7;
  OCI_HTYPE_SERVER              = 8;
  OCI_HTYPE_SESSION             = 9;
  OCI_HTYPE_TRANS               = 10;
  OCI_HTYPE_COMPLEXOBJECT       = 11;
  OCI_HTYPE_SECURITY            = 12;
  OCI_HTYPE_SUBSCRIPTION        = 13;
  OCI_HTYPE_DIRPATH_CTX         = 14;
  OCI_HTYPE_DIRPATH_COLUMN_ARRAY = 15;
  OCI_HTYPE_DIRPATH_STREAM      = 16;
  OCI_HTYPE_PROC                = 17;
  OCI_HTYPE_LAST                = 17;

  { OCI Descriptor Types }
  OCI_DTYPE_FIRST               = 50;
  OCI_DTYPE_LOB                 = 50;
  OCI_DTYPE_SNAP                = 51;
  OCI_DTYPE_RSET                = 52;
  OCI_DTYPE_PARAM               = 53;
  OCI_DTYPE_ROWID               = 54;
  OCI_DTYPE_COMPLEXOBJECTCOMP   = 55;
  OCI_DTYPE_FILE                = 56;
  OCI_DTYPE_AQENQ_OPTIONS       = 57;
  OCI_DTYPE_AQDEQ_OPTIONS       = 58;
  OCI_DTYPE_AQMSG_PROPERTIES    = 59;
  OCI_DTYPE_AQAGENT             = 60;
  OCI_DTYPE_LOCATOR             = 61;
  OCI_DTYPE_DATETIME            = 62;
  OCI_DTYPE_INTERVAL            = 63;
  OCI_DTYPE_AQNFY_DESCRIPTOR    = 64;
  OCI_DTYPE_LAST                = 64;
  OCI_DTYPE_DATE                = 65;  { Date }
  OCI_DTYPE_TIME                = 66;  { Time }
  OCI_DTYPE_TIME_TZ             = 67;  { Time with timezone }
  OCI_DTYPE_TIMESTAMP           = 68;  { Timestamp }
  OCI_DTYPE_TIMESTAMP_TZ        = 69;  { Timestamp with timezone }
  OCI_DTYPE_TIMESTAMP_LTZ       = 70;  { Timestamp with local tz }

  { OCI Attributes Types }
  OCI_ATTR_FNCODE               = 1;   // the OCI function code
  OCI_ATTR_OBJECT               = 2;   // is the environment initialized in object mode
  OCI_ATTR_NONBLOCKING_MODE     = 3;   // non blocking mode
  OCI_ATTR_SQLCODE              = 4;   // the SQL verb
  OCI_ATTR_ENV                  = 5;   // the environment handle
  OCI_ATTR_SERVER               = 6;   // the server handle
  OCI_ATTR_SESSION              = 7;   // the user session handle
  OCI_ATTR_TRANS                = 8;   // the transaction handle
  OCI_ATTR_ROW_COUNT            = 9;   // the rows processed so far
  OCI_ATTR_SQLFNCODE            = 10;  // the SQL verb of the statement
  OCI_ATTR_PREFETCH_ROWS        = 11;  // sets the number of rows to prefetch
  OCI_ATTR_NESTED_PREFETCH_ROWS = 12;  // the prefetch rows of nested table
  OCI_ATTR_PREFETCH_MEMORY      = 13;  // memory limit for rows fetched
  OCI_ATTR_NESTED_PREFETCH_MEMORY = 14;// memory limit for nested rows
  OCI_ATTR_CHAR_COUNT           = 15;  // this specifies the bind and define size in characters
  OCI_ATTR_PDSCL                = 16;  // packed decimal scale
  OCI_ATTR_FSPRECISION          = OCI_ATTR_PDSCL; // fs prec for datetime data types
  OCI_ATTR_PDPRC                = 17;  // packed decimal format
  OCI_ATTR_LFPRECISION          = OCI_ATTR_PDPRC; // fs prec for datetime data types
  OCI_ATTR_PARAM_COUNT          = 18;  // number of column in the select list
  OCI_ATTR_ROWID                = 19;  // the rowid
  OCI_ATTR_CHARSET              = 20;  // the character set value
  OCI_ATTR_NCHAR                = 21;  // NCHAR type
  OCI_ATTR_USERNAME             = 22;  // username attribute
  OCI_ATTR_PASSWORD             = 23;  // password attribute
  OCI_ATTR_STMT_TYPE            = 24;  // statement type
  OCI_ATTR_INTERNAL_NAME        = 25;  // user friendly global name
  OCI_ATTR_EXTERNAL_NAME        = 26;  // the internal name for global txn
  OCI_ATTR_XID                  = 27;  // XOPEN defined global transaction id
  OCI_ATTR_TRANS_LOCK           = 28;  //
  OCI_ATTR_TRANS_NAME           = 29;  // string to identify a global transaction
  OCI_ATTR_HEAPALLOC            = 30;  // memory allocated on the heap
  OCI_ATTR_CHARSET_ID           = 31;  // Character Set ID
  OCI_ATTR_CHARSET_FORM         = 32;  // Character Set Form
  OCI_ATTR_MAXDATA_SIZE         = 33;  // Maximumsize of data on the server
  OCI_ATTR_CACHE_OPT_SIZE       = 34;  // object cache optimal size
  OCI_ATTR_CACHE_MAX_SIZE       = 35;  // object cache maximum size percentage
  OCI_ATTR_PINOPTION            = 36;  // object cache default pin option
  OCI_ATTR_ALLOC_DURATION       = 37;  // object cache default allocation duration
  OCI_ATTR_PIN_DURATION         = 38;  // object cache default pin duration
  OCI_ATTR_FDO                  = 39;  // Format Descriptor object attribute
  OCI_ATTR_POSTPROCESSING_CALLBACK = 40;  // Callback to process outbind data
  OCI_ATTR_POSTPROCESSING_CONTEXT = 41; // Callback context to process outbind data
  OCI_ATTR_ROWS_RETURNED        = 42;  // Number of rows returned in current iter - for Bind handles
  OCI_ATTR_FOCBK                = 43;  // Failover Callback attribute
  OCI_ATTR_IN_V8_MODE           = 44;  // is the server/service context in V8 mode
  OCI_ATTR_LOBEMPTY             = 45;  // empty lob ?
  OCI_ATTR_SESSLANG             = 46;  // session language handle

  OCI_ATTR_VISIBILITY           = 47;  // visibility
  OCI_ATTR_RELATIVE_MSGID       = 48;  // relative message id
  OCI_ATTR_SEQUENCE_DEVIATION   = 49;  // sequence deviation

  OCI_ATTR_CONSUMER_NAME        = 50;  // consumer name
  OCI_ATTR_DEQ_MODE             = 51;  // dequeue mode
  OCI_ATTR_NAVIGATION           = 52;  // navigation
  OCI_ATTR_WAIT                 = 53;  // wait
  OCI_ATTR_DEQ_MSGID            = 54;  // dequeue message id

  OCI_ATTR_PRIORITY             = 55;  // priority
  OCI_ATTR_DELAY                = 56;  // delay
  OCI_ATTR_EXPIRATION           = 57;  // expiration
  OCI_ATTR_CORRELATION          = 58;  // correlation id
  OCI_ATTR_ATTEMPTS             = 59;  // # of attempts
  OCI_ATTR_RECIPIENT_LIST       = 60;  // recipient list
  OCI_ATTR_EXCEPTION_QUEUE      = 61;  // exception queue name
  OCI_ATTR_ENQ_TIME             = 62;  // enqueue time (only OCIAttrGet)
  OCI_ATTR_MSG_STATE            = 63;  // message state (only OCIAttrGet)
                                       // NOTE: 64-66 used below
  OCI_ATTR_AGENT_NAME           = 64;  // agent name
  OCI_ATTR_AGENT_ADDRESS        = 65;  // agent address
  OCI_ATTR_AGENT_PROTOCOL       = 66;  // agent protocol

  OCI_ATTR_SENDER_ID            = 68;  // sender id
  OCI_ATTR_ORIGINAL_MSGID       = 69;  // original message id

  OCI_ATTR_QUEUE_NAME           = 70;  // queue name
  OCI_ATTR_NFY_MSGID            = 71;  // message id
  OCI_ATTR_MSG_PROP             = 72;  // message properties

  OCI_ATTR_NUM_DML_ERRORS       = 73;  // num of errs in array DML
  OCI_ATTR_DML_ROW_OFFSET       = 74;  // row offset in the array

  OCI_ATTR_DATEFORMAT           = 75;  // default date format string
  OCI_ATTR_BUF_ADDR             = 76;  // buffer address
  OCI_ATTR_BUF_SIZE             = 77;  // buffer size
  OCI_ATTR_DIRPATH_MODE         = 78;  // mode of direct path operation
  OCI_ATTR_DIRPATH_NOLOG        = 79;  // nologging option
  OCI_ATTR_DIRPATH_PARALLEL     = 80;  // parallel (temp seg) option
  OCI_ATTR_NUM_ROWS             = 81;  // number of rows in column array
                                       // NOTE that OCI_ATTR_NUM_COLS is a column
                                       // array attribute too.

  OCI_ATTR_COL_COUNT            = 82;  // columns of column array processed so far.
  OCI_ATTR_STREAM_OFFSET        = 83;  // str off of last row processed
  OCI_ATTR_SHARED_HEAPALLOC     = 84;  // Shared Heap Allocation Size

  OCI_ATTR_SERVER_GROUP         = 85;  // server group name

  OCI_ATTR_MIGSESSION           = 86;  // migratable session attribute

  OCI_ATTR_NOCACHE              = 87;  // Temporary LOBs

  OCI_ATTR_MEMPOOL_SIZE         = 88;  // Pool Size
  OCI_ATTR_MEMPOOL_INSTNAME     = 89;  // Instance name
  OCI_ATTR_MEMPOOL_APPNAME      = 90;  // Application name
  OCI_ATTR_MEMPOOL_HOMENAME     = 91;  // Home Directory name
  OCI_ATTR_MEMPOOL_MODEL        = 92;  // Pool Model (proc,thrd,both)
  OCI_ATTR_MODES                = 93;  // Modes

  OCI_ATTR_SUBSCR_NAME          = 94;  // name of subscription
  OCI_ATTR_SUBSCR_CALLBACK      = 95;  // associated callback
  OCI_ATTR_SUBSCR_CTX           = 96;  // associated callback context
  OCI_ATTR_SUBSCR_PAYLOAD       = 97;  // associated payload
  OCI_ATTR_SUBSCR_NAMESPACE     = 98;  // associated namespace

  OCI_ATTR_PROXY_CREDENTIALS    = 99;  // Proxy user credentials
  OCI_ATTR_INITIAL_CLIENT_ROLES = 100; // Initial client role list

  OCI_ATTR_UNK                  = 101; // unknown attribute
  OCI_ATTR_NUM_COLS             = 102; // number of columns
  OCI_ATTR_LIST_COLUMNS         = 103; // parameter of the column list
  OCI_ATTR_RDBA                 = 104; // DBA of the segment header
  OCI_ATTR_CLUSTERED            = 105; // whether the table is clustered
  OCI_ATTR_PARTITIONED          = 106; // whether the table is partitioned
  OCI_ATTR_INDEX_ONLY           = 107; // whether the table is index only
  OCI_ATTR_LIST_ARGUMENTS       = 108; // parameter of the argument list
  OCI_ATTR_LIST_SUBPROGRAMS     = 109; // parameter of the subprogram list
  OCI_ATTR_REF_TDO              = 110; // REF to the type descriptor
  OCI_ATTR_LINK                 = 111; // the database link name
  OCI_ATTR_MIN                  = 112; // minimum value
  OCI_ATTR_MAX                  = 113; // maximum value
  OCI_ATTR_INCR                 = 114; // increment value
  OCI_ATTR_CACHE                = 115; // number of sequence numbers cached
  OCI_ATTR_ORDER                = 116; // whether the sequence is ordered
  OCI_ATTR_HW_MARK              = 117; // high-water mark
  OCI_ATTR_TYPE_SCHEMA          = 118; // type's schema name
  OCI_ATTR_TIMESTAMP            = 119; // timestamp of the object
  OCI_ATTR_NUM_ATTRS            = 120; // number of sttributes
  OCI_ATTR_NUM_PARAMS           = 121; // number of parameters
  OCI_ATTR_OBJID                = 122; // object id for a table or view
  OCI_ATTR_PTYPE                = 123; // type of info described by
  OCI_ATTR_PARAM                = 124; // parameter descriptor
  OCI_ATTR_OVERLOAD_ID          = 125; // overload ID for funcs and procs
  OCI_ATTR_TABLESPACE           = 126; // table name space
  OCI_ATTR_TDO                  = 127; // TDO of a type
  OCI_ATTR_LTYPE                = 128; // list type
  OCI_ATTR_PARSE_ERROR_OFFSET   = 129; // Parse Error offset
  OCI_ATTR_IS_TEMPORARY         = 130; // whether table is temporary
  OCI_ATTR_IS_TYPED             = 131; // whether table is typed
  OCI_ATTR_DURATION             = 132; // duration of temporary table
  OCI_ATTR_IS_INVOKER_RIGHTS    = 133; // is invoker rights
  OCI_ATTR_OBJ_NAME             = 134; // top level schema obj name
  OCI_ATTR_OBJ_SCHEMA           = 135; // schema name
  OCI_ATTR_OBJ_ID               = 136; // top level schema object id
  OCI_ATTR_STMTCACHESIZE        = 176; // size of the stm cache
  OCI_ATTR_ROWS_FETCHED         = 197; // rows fetched in last call
  OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE = 438; // default prefetch size

  { OCI Error Return Values }
  OCI_SUCCESS             = 0;
  OCI_SUCCESS_WITH_INFO   = 1;
  OCI_NO_DATA             = 100;
  OCI_ERROR               = -1;
  OCI_INVALID_HANDLE      = -2;
  OCI_NEED_DATA           = 99;
  OCI_STILL_EXECUTING     = -3123;
  OCI_CONTINUE            = -24200;
  OCI_PASSWORD_INFO       = 28002; // the password will expire within ... days

  { Generic Default Value for Modes, .... }
  OCI_DEFAULT     = $0;

  { OCI Init Mode }
  OCI_THREADED    = $1;
  OCI_OBJECT      = $2;
  OCI_EVENTS      = $4;
  OCI_SHARED      = $10;
  OCI_NO_UCB      = $40;
  OCI_NO_MUTEX    = $80;

  { OCI Credentials }
  OCI_CRED_RDBMS  = 1;
  OCI_CRED_EXT    = 2;
  OCI_CRED_PROXY  = 3;

  { OCI Authentication Mode }
  OCI_MIGRATE     = $0001;             // migratable auth context
  OCI_SYSDBA      = $0002;             // for SYSDBA authorization
  OCI_SYSOPER     = $0004;             // for SYSOPER authorization
  OCI_PRELIM_AUTH = $0008;             // for preliminary authorization

  { OCIPasswordChange }
  OCI_AUTH        = $08;               // Change the password but do not login

  { OCI Data Types }
  SQLT_CHR = 1;
  SQLT_NUM = 2;
  SQLT_INT = 3;
  SQLT_FLT = 4;
  SQLT_STR = 5;
  SQLT_VNU = 6;
  SQLT_PDN = 7;
  SQLT_LNG = 8;
  SQLT_VCS = 9;
  SQLT_NON = 10;
  SQLT_RID = 11;
  SQLT_DAT = 12;
  SQLT_VBI = 15;
  SQLT_BFLOAT = 21;
  SQLT_BDOUBLE = 22;
  SQLT_BIN = 23;
  SQLT_LBI = 24;
  _SQLT_PLI = 29;
  SQLT_UIN = 68;
  SQLT_SLS = 91;
  SQLT_LVC = 94;
  SQLT_LVB = 95;
  SQLT_AFC = 96;
  SQLT_AVC = 97;
  SQLT_IBFLOAT = 100;
  SQLT_IBDOUBLE = 101;
  SQLT_CUR = 102;
  SQLT_RDD = 104;
  SQLT_LAB = 105;
  SQLT_OSL = 106;
  SQLT_NTY = 108;
  SQLT_REF = 110;
  SQLT_CLOB = 112;
  SQLT_BLOB = 113;
  SQLT_BFILEE = 114;
  SQLT_CFILEE = 115;
  SQLT_RSET = 116;
  SQLT_NCO = 122;
  SQLT_VST = 155;
  SQLT_ODT = 156;
  SQLT_DATE = 184;
  SQLT_TIME = 185;
  SQLT_TIME_TZ = 186;
  SQLT_TIMESTAMP = 187;
  SQLT_TIMESTAMP_TZ = 188;
  SQLT_INTERVAL_YM = 189;
  SQLT_INTERVAL_DS = 190;
  SQLT_TIMESTAMP_LTZ = 232;

  _SQLT_REC = 250;
  _SQLT_TAB = 251;
  _SQLT_BOL = 252;

  { OCI Statement Types }
  OCI_STMT_SELECT  = 1;   // select statement
  OCI_STMT_UPDATE  = 2;   // update statement
  OCI_STMT_DELETE  = 3;   // delete statement
  OCI_STMT_INSERT  = 4;   // Insert Statement
  OCI_STMT_CREATE  = 5;   // create statement
  OCI_STMT_DROP    = 6;   // drop statement
  OCI_STMT_ALTER   = 7;   // alter statement
  OCI_STMT_BEGIN   = 8;   // begin ... (pl/sql statement)
  OCI_STMT_DECLARE = 9;   // declare .. (pl/sql statement)

  { OCI Statement language }
  OCI_NTV_SYNTAX  = 1;    // Use what so ever is the native lang of server
  OCI_V7_SYNTAX   = 2;    // V7 language
  OCI_V8_SYNTAX   = 3;    // V8 language

  { OCI Statement Execute mode }
  OCI_BATCH_MODE        = $01;    // batch the oci statement for execution
  OCI_EXACT_FETCH       = $02;    // fetch the exact rows specified
  OCI_SCROLLABLE_CURSOR = $08;    // cursor scrollable
  OCI_DESCRIBE_ONLY     = $10;    // only describe the statement
  OCI_COMMIT_ON_SUCCESS = $20;    // commit, if successful execution
  OCI_NON_BLOCKING      = $40;    // non-blocking
  OCI_BATCH_ERRORS      = $80;    // batch errors in array dmls
  OCI_PARSE_ONLY        = $100;   // only parse the statement

  { Enable OCI Server-Side Statement Caching }
  OCI_STMT_CACHE       = $40;
  OCI_STMTCACHE_DELETE = $10;

  OCI_DATA_AT_EXEC    = $02;      // data at execute time
  OCI_DYNAMIC_FETCH   = $02;      // fetch dynamically
  OCI_PIECEWISE       = $04;      // piecewise DMLs or fetch

  { OCI Transaction modes }
  OCI_TRANS_NEW          = $00000001; // starts a new transaction branch
  OCI_TRANS_JOIN         = $00000002; // join an existing transaction
  OCI_TRANS_RESUME       = $00000004; // resume this transaction
  OCI_TRANS_STARTMASK    = $000000ff;

  OCI_TRANS_READONLY     = $00000100; // starts a readonly transaction
  OCI_TRANS_READWRITE    = $00000200; // starts a read-write transaction
  OCI_TRANS_SERIALIZABLE = $00000400; // starts a serializable transaction
  OCI_TRANS_ISOLMASK     = $0000ff00;

  OCI_TRANS_LOOSE        = $00010000; // a loosely coupled branch
  OCI_TRANS_TIGHT        = $00020000; // a tightly coupled branch
  OCI_TRANS_TYPEMASK     = $000f0000;

  OCI_TRANS_NOMIGRATE    = $00100000; // non migratable transaction
  OCI_TRANS_TWOPHASE     = $01000000; // use two phase commit

  { OCI pece wise fetch }
  OCI_ONE_PIECE       = 0; // one piece
  OCI_FIRST_PIECE     = 1; // the first piece
  OCI_NEXT_PIECE      = 2; // the next of many pieces
  OCI_LAST_PIECE      = 3; // the last piece

  { OCI fetch modes }
  OCI_FETCH_NEXT      = $02;  // next row
  OCI_FETCH_FIRST     = $04;  // first row of the result set
  OCI_FETCH_LAST      = $08;  // the last row of the result set
  OCI_FETCH_PRIOR     = $10;  // the previous row relative to current
  OCI_FETCH_ABSOLUTE  = $20;  // absolute offset from first
  OCI_FETCH_RELATIVE  = $40;  // offset relative to current

  {****************** Describe Handle Parameter Attributes *****************}

  { Attributes common to Columns and Stored Procs }
  OCI_ATTR_DATA_SIZE      = 1;    // maximum size of the data
  OCI_ATTR_DATA_TYPE      = 2;    // the SQL type of the column/argument
  OCI_ATTR_DISP_SIZE      = 3;    // the display size
  OCI_ATTR_NAME           = 4;    // the name of the column/argument
  OCI_ATTR_PRECISION      = 5;    // precision if number type
  OCI_ATTR_SCALE          = 6;    // scale if number type
  OCI_ATTR_IS_NULL        = 7;    // is it null ?
  OCI_ATTR_TYPE_NAME      = 8;    // name of the named data type or a package name for package private types
  OCI_ATTR_SCHEMA_NAME    = 9;    // the schema name
  OCI_ATTR_SUB_NAME       = 10;   // type name if package private type
  OCI_ATTR_POSITION       = 11;   // relative position of col/arg in the list of cols/args

  { complex object retrieval parameter attributes }
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE         = 50;
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL   = 51;
  OCI_ATTR_COMPLEXOBJECT_LEVEL            = 52;
  OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE   = 53;

  { Only Columns }
  OCI_ATTR_DISP_NAME                 = 100;  // the display name

  { Only Stored Procs }
  OCI_ATTR_OVERLOAD                  = 210;  // is this position overloaded
  OCI_ATTR_LEVEL                     = 211;  // level for structured types
  OCI_ATTR_HAS_DEFAULT               = 212;  // has a default value
  OCI_ATTR_IOMODE                    = 213;  // in, out inout
  OCI_ATTR_RADIX                     = 214;  // returns a radix
  OCI_ATTR_NUM_ARGS                  = 215;  // total number of arguments

  { only named type attributes }
  OCI_ATTR_TYPECODE                  = 216;   // object or collection
  OCI_ATTR_COLLECTION_TYPECODE       = 217;   // varray or nested table
  OCI_ATTR_VERSION                   = 218;   // user assigned version
  OCI_ATTR_IS_INCOMPLETE_TYPE        = 219;   // is this an incomplete type
  OCI_ATTR_IS_SYSTEM_TYPE            = 220;   // a system type
  OCI_ATTR_IS_PREDEFINED_TYPE        = 221;   // a predefined type
  OCI_ATTR_IS_TRANSIENT_TYPE         = 222;   // a transient type
  OCI_ATTR_IS_SYSTEM_GENERATED_TYPE  = 223;   // system generated type
  OCI_ATTR_HAS_NESTED_TABLE          = 224;   // contains nested table attr
  OCI_ATTR_HAS_LOB                   = 225;   // has a lob attribute
  OCI_ATTR_HAS_FILE                  = 226;   // has a file attribute
  OCI_ATTR_COLLECTION_ELEMENT        = 227;   // has a collection attribute
  OCI_ATTR_NUM_TYPE_ATTRS            = 228;   // number of attribute types
  OCI_ATTR_LIST_TYPE_ATTRS           = 229;   // list of type attributes
  OCI_ATTR_NUM_TYPE_METHODS          = 230;   // number of type methods
  OCI_ATTR_LIST_TYPE_METHODS         = 231;   // list of type methods
  OCI_ATTR_MAP_METHOD                = 232;   // map method of type
  OCI_ATTR_ORDER_METHOD              = 233;   // order method of type

  { only collection element }
  OCI_ATTR_NUM_ELEMS                 = 234;   // number of elements

  { only type methods }
  OCI_ATTR_ENCAPSULATION             = 235;   // encapsulation level
  OCI_ATTR_IS_SELFISH                = 236;   // method selfish
  OCI_ATTR_IS_VIRTUAL                = 237;   // virtual
  OCI_ATTR_IS_INLINE                 = 238;   // inline
  OCI_ATTR_IS_CONSTANT               = 239;   // constant
  OCI_ATTR_HAS_RESULT                = 240;   // has result
  OCI_ATTR_IS_CONSTRUCTOR            = 241;   // constructor
  OCI_ATTR_IS_DESTRUCTOR             = 242;   // destructor
  OCI_ATTR_IS_OPERATOR               = 243;   // operator
  OCI_ATTR_IS_MAP                    = 244;   // a map method
  OCI_ATTR_IS_ORDER                  = 245;   // order method
  OCI_ATTR_IS_RNDS                   = 246;   // read no data state method
  OCI_ATTR_IS_RNPS                   = 247;   // read no process state
  OCI_ATTR_IS_WNDS                   = 248;   // write no data state method
  OCI_ATTR_IS_WNPS                   = 249;   // write no process state

  OCI_ATTR_DESC_PUBLIC               = 250;   // public object

  { Object Cache Enhancements : attributes for User Constructed Instances }
  OCI_ATTR_CACHE_CLIENT_CONTEXT      = 251;
  OCI_ATTR_UCI_CONSTRUCT             = 252;
  OCI_ATTR_UCI_DESTRUCT              = 253;
  OCI_ATTR_UCI_COPY                  = 254;
  OCI_ATTR_UCI_PICKLE                = 255;
  OCI_ATTR_UCI_UNPICKLE              = 256;
  OCI_ATTR_UCI_REFRESH               = 257;

  { for type inheritance }
  OCI_ATTR_IS_SUBTYPE                = 258;
  OCI_ATTR_SUPERTYPE_SCHEMA_NAME     = 259;
  OCI_ATTR_SUPERTYPE_NAME            = 260;

  { for schemas }
  OCI_ATTR_LIST_OBJECTS              = 261;   // list of objects in schema

  { for database }
  OCI_ATTR_NCHARSET_ID               = 262;   // char set id
  OCI_ATTR_LIST_SCHEMAS              = 263;   // list of schemas
  OCI_ATTR_MAX_PROC_LEN              = 264;   // max procedure length
  OCI_ATTR_MAX_COLUMN_LEN            = 265;   // max column name length
  OCI_ATTR_CURSOR_COMMIT_BEHAVIOR    = 266;   // cursor commit behavior
  OCI_ATTR_MAX_CATALOG_NAMELEN       = 267;   // catalog namelength
  OCI_ATTR_CATALOG_LOCATION          = 268;   // catalog location
  OCI_ATTR_SAVEPOINT_SUPPORT         = 269;   // savepoint support
  OCI_ATTR_NOWAIT_SUPPORT            = 270;   // nowait support
  OCI_ATTR_AUTOCOMMIT_DDL            = 271;   // autocommit DDL
  OCI_ATTR_LOCKING_MODE              = 272;   // locking mode

  OCI_ATTR_CACHE_ARRAYFLUSH          = $40;
  OCI_ATTR_OBJECT_NEWNOTNULL         = $10;
  OCI_ATTR_OBJECT_DETECTCHANGE       = $20;

  { Piece Information }
  OCI_PARAM_IN                       = $01;  // in parameter
  OCI_PARAM_OUT                      = $02;  // out parameter

  { LOB Buffering Flush Flags }
  OCI_LOB_BUFFER_FREE     = 1;
  OCI_LOB_BUFFER_NOFREE   = 2;

  { FILE open modes }
  OCI_FILE_READONLY   = 1;    // readonly mode open for FILE types
  { LOB open modes }
  OCI_LOB_READONLY    = 1;    // readonly mode open for ILOB types
  OCI_LOB_READWRITE   = 2;    // read write mode open for ILOBs
  { LOB types }
  OCI_TEMP_BLOB       = 1;    // LOB type - BLOB
  OCI_TEMP_CLOB       = 2;    // LOB type - CLOB

  { CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information
    (used e.g. by OCI_ATTR_CHARSET_FORM attribute) }
  SQLCS_IMPLICIT = 1;     // for CHAR, VARCHAR2, CLOB w/o a specified set
  SQLCS_NCHAR    = 2;     // for NCHAR, NCHAR VARYING, NCLOB
  SQLCS_EXPLICIT = 3;     // for CHAR, etc, with "CHARACTER SET ..." syntax
  SQLCS_FLEXIBLE = 4;     // for PL/SQL "flexible" parameters
  SQLCS_LIT_NULL = 5;     // for typecheck of NULL and empty_clob() lits

  { OCI_NUMBER }
  OCI_NUMBER_SIZE     = 22;
  OCI_NUMBER_UNSIGNED = 0;
  OCI_NUMBER_SIGNED   = 2;

  { OBJECT Duration }
  OCI_DURATION_BEGIN_                     = 10;
  OCI_DURATION_CALLOUT_                   = OCI_DURATION_BEGIN_ + 4;

  OCI_DURATION_INVALID: OCIDuration       = $FFFF;                   // Invalid duration
  OCI_DURATION_BEGIN: OCIDuration         = OCI_DURATION_BEGIN_;     // beginning sequence of duration
  OCI_DURATION_NULL: OCIDuration          = OCI_DURATION_BEGIN_ - 1; // null duration
  OCI_DURATION_DEFAULT: OCIDuration       = OCI_DURATION_BEGIN_ - 2; // default
  OCI_DURATION_USER_CALLBACK: OCIDuration = OCI_DURATION_BEGIN_ - 3;
  OCI_DURATION_NEXT: OCIDuration          = OCI_DURATION_BEGIN_ - 4; // next special duration
  OCI_DURATION_SESSION: OCIDuration       = OCI_DURATION_BEGIN_;     // the end of user session
  OCI_DURATION_TRANS: OCIDuration         = OCI_DURATION_BEGIN_ + 1; // the end of user transaction
  // DO NOT USE OCI_DURATION_CALL. IT  IS UNSUPPORTED
  // WILL BE REMOVED/CHANGED IN A FUTURE RELEASE
  OCI_DURATION_CALL: OCIDuration          = OCI_DURATION_BEGIN_ + 2; // the end of user client/server call
  OCI_DURATION_STATEMENT: OCIDuration     = OCI_DURATION_BEGIN_ + 3;
  // This is to be used only during callouts.  It is similar to that
  // of OCI_DURATION_CALL, but lasts only for the duration of a callout.
  // Its heap is from PGA
  OCI_DURATION_CALLOUT: OCIDuration       = OCI_DURATION_CALLOUT_;
  OCI_DURATION_LAST: OCIDuration          = OCI_DURATION_CALLOUT_;   // last of predefined durations
  // This is not being treated as other predefined durations such as
  // SESSION, CALL etc, because this would not have an entry in the duration
  // table and its functionality is primitive such that only allocate, free,
  // resize memory are allowed, but one cannot create subduration out of this
  OCI_DURATION_PROCESS: OCIDuration       = OCI_DURATION_BEGIN_ - 5; // next special duration

  { TYPE CODE }
  /// Type manager typecodes
  // - These are typecodes designed to be used with the type manager;
  // they also include longer, more readable versions of existing SQLT names
  // - Those types that are directly related to existing SQLT types are #define'd
  // to their SQLT equivalents
  // - The type manager typecodes are designed to be useable for all OCI calls.
  // They are in the range from 192 to 320 for typecodes, so as not to conflict
  // with existing OCI SQLT typecodes (see ocidfn.h)
  OCI_TYPECODE_REF             = SQLT_REF;      // SQL/OTS OBJECT REFERENCE
  OCI_TYPECODE_DATE            = SQLT_DAT;      // SQL DATE  OTS DATE
  OCI_TYPECODE_SIGNED8         = 27;            // SQL SIGNED INTEGER(8)  OTS SINT8
  OCI_TYPECODE_SIGNED16        = 28;            // SQL SIGNED INTEGER(16)  OTS SINT16
  OCI_TYPECODE_SIGNED32        = 29;            // SQL SIGNED INTEGER(32)  OTS SINT32
  OCI_TYPECODE_REAL            = 21;            // SQL REAL  OTS SQL_REAL
  OCI_TYPECODE_DOUBLE          = 22;            // SQL DOUBLE PRECISION  OTS SQL_DOUBLE
  OCI_TYPECODE_BFLOAT          = SQLT_IBFLOAT;  // Binary float
  OCI_TYPECODE_BDOUBLE         = SQLT_IBDOUBLE; // Binary double
  OCI_TYPECODE_FLOAT           = SQLT_FLT;      // SQL FLOAT(P)  OTS FLOAT(P)
  OCI_TYPECODE_NUMBER          = SQLT_NUM;      // SQL NUMBER(P S)  OTS NUMBER(P S)
  OCI_TYPECODE_DECIMAL         = SQLT_PDN;      // SQL DECIMAL(P S)  OTS DECIMAL(P S)
  OCI_TYPECODE_UNSIGNED8       = SQLT_BIN;      // SQL UNSIGNED INTEGER(8)  OTS UINT8
  OCI_TYPECODE_UNSIGNED16      = 25;            // SQL UNSIGNED INTEGER(16)  OTS UINT16
  OCI_TYPECODE_UNSIGNED32      = 26;            // SQL UNSIGNED INTEGER(32)  OTS UINT32
  OCI_TYPECODE_OCTET           = 245;           // SQL ???  OTS OCTET
  OCI_TYPECODE_SMALLINT        = 246;           // SQL SMALLINT  OTS SMALLINT
  OCI_TYPECODE_INTEGER         = SQLT_INT;      // SQL INTEGER  OTS INTEGER
  OCI_TYPECODE_RAW             = SQLT_LVB;      // SQL RAW(N)  OTS RAW(N)
  OCI_TYPECODE_PTR             = 32;            // SQL POINTER  OTS POINTER
  OCI_TYPECODE_VARCHAR2        = SQLT_VCS;      // SQL VARCHAR2(N)  OTS SQL_VARCHAR2(N)
  OCI_TYPECODE_CHAR            = SQLT_AFC;      // SQL CHAR(N)  OTS SQL_CHAR(N)
  OCI_TYPECODE_VARCHAR         = SQLT_CHR;      // SQL VARCHAR(N)  OTS SQL_VARCHAR(N)
  OCI_TYPECODE_MLSLABEL        = SQLT_LAB;      // OTS MLSLABEL
  OCI_TYPECODE_VARRAY          = 247;           // SQL VARRAY  OTS PAGED VARRAY
  OCI_TYPECODE_TABLE           = 248;           // SQL TABLE  OTS MULTISET
  OCI_TYPECODE_OBJECT          = SQLT_NTY;      // SQL/OTS NAMED OBJECT TYPE
  OCI_TYPECODE_OPAQUE          = 58;            //  SQL/OTS Opaque Types
  OCI_TYPECODE_NAMEDCOLLECTION = SQLT_NCO;      // SQL/OTS NAMED COLLECTION TYPE
  OCI_TYPECODE_BLOB            = SQLT_BLOB;     // SQL/OTS BINARY LARGE OBJECT
  OCI_TYPECODE_BFILE           = SQLT_BFILEE;   // SQL/OTS BINARY FILE OBJECT
  OCI_TYPECODE_CLOB            = SQLT_CLOB;     // SQL/OTS CHARACTER LARGE OBJECT
  OCI_TYPECODE_CFILE           = SQLT_CFILEE;   // SQL/OTS CHARACTER FILE OBJECT

  // the following are ANSI datetime datatypes added in 8.1
  OCI_TYPECODE_TIME            = SQLT_TIME;          // SQL/OTS TIME
  OCI_TYPECODE_TIME_TZ         = SQLT_TIME_TZ;       // SQL/OTS TIME_TZ
  OCI_TYPECODE_TIMESTAMP       = SQLT_TIMESTAMP;     // SQL/OTS TIMESTAMP
  OCI_TYPECODE_TIMESTAMP_TZ    = SQLT_TIMESTAMP_TZ;  // SQL/OTS TIMESTAMP_TZ

  OCI_TYPECODE_TIMESTAMP_LTZ   = SQLT_TIMESTAMP_LTZ; // TIMESTAMP_LTZ

  OCI_TYPECODE_INTERVAL_YM     = SQLT_INTERVAL_YM;   // SQL/OTS INTRVL YR-MON
  OCI_TYPECODE_INTERVAL_DS     = SQLT_INTERVAL_DS;   // SQL/OTS INTRVL DAY-SEC
  OCI_TYPECODE_UROWID          = SQLT_RDD;           // Urowid type

  OCI_TYPECODE_OTMFIRST        = 228;     // first Open Type Manager typecode
  OCI_TYPECODE_OTMLAST         = 320;     // last OTM typecode
  OCI_TYPECODE_SYSFIRST        = 228;     // first OTM system type (internal)
  OCI_TYPECODE_SYSLAST         = 235;     // last OTM system type (internal)
  OCI_TYPECODE_PLS_INTEGER     = 266;     // type code for PLS_INTEGER

  //// the following are PL/SQL-only internal. They should not be used
  //  OCI_TYPECODE_ITABLE          = SQLT_TAB;    // PLSQL indexed table
  //  OCI_TYPECODE_RECORD          = SQLT_REC;    // PLSQL record
  //  OCI_TYPECODE_BOOLEAN         = SQLT_BOL;    // PLSQL boolean

  // NOTE : The following NCHAR related codes are just short forms for saying
  // OCI_TYPECODE_VARCHAR2 with a charset form of SQLCS_NCHAR. These codes are
  // intended for use in the OCIAnyData API only and nowhere else.
  OCI_TYPECODE_NCHAR           = 286;
  OCI_TYPECODE_NVARCHAR2       = 287;
  OCI_TYPECODE_NCLOB           = 288;

  // To indicate absence of typecode being specified
  OCI_TYPECODE_NONE            = 0;
  // To indicate error has to be taken from error handle - reserved for sqlplus use
  OCI_TYPECODE_ERRHP           = 283;

  { TYPEGET options }
  OCI_TYPEGET_HEADER = 0;
  OCI_TYPEGET_ALL = 1;

  { OBJECT FREE OPTION }
  /// OCIObjectFreeFlag - Object free flag
  // - If OCI_OBJECTCOPY_FORCE is specified when freeing an instance, the instance
  // is freed regardless it is pinned or diritied.
  // If OCI_OBJECTCOPY_NONULL is specified when freeing an instance, the null
  // structure is not freed.
  OCI_OBJECTFREE_FORCE : ub2 = $0001;
  OCI_OBJECTFREE_NONULL: ub2 = $0002;
  OCI_OBJECTFREE_HEADER: ub2 = $0004;

  OCI_PREP2_CACHE_SEARCHONLY: ub4 = $0010;

type
  /// Oracle native number low-level representation
  OCINumber = packed record
    OCINumberPart: array [0..OCI_NUMBER_SIZE-1] of ub1;
  end;

{ TSQLDBOracleLib }

const
  OCI_ENTRIES: array[0..40] of PChar = (
    'OCIClientVersion', 'OCIEnvNlsCreate', 'OCIHandleAlloc', 'OCIHandleFree',
    'OCIServerAttach', 'OCIServerDetach', 'OCIAttrGet', 'OCIAttrSet',
    'OCISessionBegin', 'OCISessionEnd', 'OCIErrorGet', 'OCIStmtPrepare',
    'OCIStmtExecute', 'OCIStmtFetch', 'OCIBindByPos', 'OCIParamGet',
    'OCITransStart', 'OCITransRollback', 'OCITransCommit', 'OCIDescriptorAlloc',
    'OCIDescriptorFree', 'OCIDateTimeConstruct', 'OCIDateTimeGetDate',
    'OCIDefineByPos', 'OCILobGetLength', 'OCILobGetChunkSize', 'OCILobOpen',
    'OCILobRead', 'OCILobClose', 'OCILobWrite',
    'OCINlsCharSetNameToId', 'OCIStmtPrepare2',
    'OCIStmtRelease', 'OCITypeByName', 'OCIObjectNew', 'OCIObjectFree',
    'OCINumberFromInt','OCIStringAssignText', 'OCICollAppend', 'OCIBindObject',
    'OCIPasswordChange');

type
  /// direct access to the native Oracle Client Interface (OCI)
  TSQLDBOracleLib = class(TSQLDBLib)
  protected
    procedure HandleError(Conn: TSQLDBConnection; Stmt: TSQLDBStatement;
      Status: Integer; ErrorHandle: POCIError; InfoRaiseException: Boolean=false;
      LogLevelNoRaise: TSynLogInfo=sllNone);
    procedure RetrieveVersion;
    function BlobOpen(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor): ub4;
    function BlobRead(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; Blob: PByte; BlobLen: ub4;
      csid: ub2=0; csfrm: ub1=SQLCS_IMPLICIT): integer;
    function BlobReadToStream(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; stream: TStream; BlobLen: ub4;
      csid: ub2=0; csfrm: ub1=SQLCS_IMPLICIT): integer;
    function BlobWriteFromStream(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; stream: TStream; BlobLen: ub4;
      csid: ub2=0; csfrm: ub1=SQLCS_IMPLICIT): integer;
  public
    ClientVersion: function(var major_version, minor_version,
      update_num, patch_num, port_update_num: sword): sword; cdecl;
    EnvNlsCreate: function(var envhpp: pointer; mode: ub4; ctxp: Pointer;
      malocfp: Pointer; ralocfp: Pointer; mfreefp: Pointer; xtramemsz: size_T;
      usrmempp: PPointer; charset, ncharset: ub2): sword; cdecl;
    HandleAlloc: function(parenth: POCIHandle; var hndlpp: pointer;
      atype: ub4; xtramem_sz: size_T=0; usrmempp: PPointer=nil): sword; cdecl;
    HandleFree: function(hndlp: Pointer; atype: ub4): sword; cdecl;
    ServerAttach: function(srvhp: POCIServer; errhp: POCIError; dblink: text;
      dblink_len: sb4; mode: ub4): sword; cdecl;
    ServerDetach: function(srvhp: POCIServer; errhp: POCIError;
      mode: ub4): sword; cdecl;
    AttrGet: function(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; sizep: Pointer; attrtype: ub4;
      errhp: POCIError): sword; cdecl;
    AttrSet: function(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; size: ub4; attrtype: ub4; errhp: POCIError): sword; cdecl;
    SessionBegin: function(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; credt: ub4; mode: ub4): sword; cdecl;
    SessionEnd: function(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; mode: ub4): sword; cdecl;
    ErrorGet: function(hndlp: Pointer; recordno: ub4; sqlstate: text;
      var errcodep: sb4; bufp: text; bufsiz: ub4; atype: ub4): sword; cdecl;
    StmtPrepare: function(stmtp: POCIStmt; errhp: POCIError; stmt: text;
      stmt_len: ub4; language:ub4; mode: ub4): sword; cdecl;
    StmtExecute: function(svchp: POCISvcCtx; stmtp: POCIStmt;
      errhp: POCIError; iters: ub4; rowoff: ub4; snap_in: POCISnapshot;
      snap_out: POCISnapshot; mode: ub4): sword; cdecl;
    StmtFetch: function(stmtp: POCIStmt; errhp: POCIError; nrows: ub4;
      orientation: ub2; mode: ub4): sword; cdecl;
    BindByPos: function(stmtp: POCIStmt; var bindpp: POCIBind;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; alenp: Pointer; rcodep: Pointer; maxarr_len: ub4;
      curelep: Pointer; mode: ub4): sword; cdecl;
    ParamGet: function(hndlp: Pointer; htype: ub4; errhp: POCIError;
      var parmdpp: Pointer; pos: ub4): sword; cdecl;
    TransStart: function(svchp: POCISvcCtx; errhp: POCIError; timeout: word;
      flags: ub4): sword; cdecl;
    TransRollback: function(svchp:POCISvcCtx; errhp:POCIError;
      flags: ub4): sword; cdecl;
    TransCommit: function(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4) :sword; cdecl;
    DescriptorAlloc: function(parenth: POCIEnv; var descpp: pointer;
      htype: ub4; xtramem_sz: integer; usrmempp: Pointer): sword; cdecl;
    DescriptorFree: function(descp: Pointer; htype: ub4): sword; cdecl;
    DateTimeConstruct: function(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; year: sb2; month: ub1; day: ub1; hour: ub1;
      min: ub1; sec: ub1; fsec: ub4; timezone: text;
      timezone_length: size_t): sword; cdecl;
    DateTimeGetDate: function(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; var year: sb2; var month: ub1;
      var day: ub1): sword; cdecl;
    DefineByPos: function(stmtp: POCIStmt; var defnpp: POCIDefine;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; rlenp: Pointer; rcodep: Pointer; mode: ub4): sword; cdecl;
    LobGetLength: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var lenp: ub4): sword; cdecl;
    LobGetChunkSize: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var chunk_size: ub4): sword; cdecl;
    LobOpen: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; mode: ub1): sword; cdecl;
    LobRead: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      ctxp: Pointer=nil; cbfp: Pointer=nil; csid: ub2=0; csfrm: ub1=SQLCS_IMPLICIT): sword; cdecl;
    LobClose: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword; cdecl;
    LobWrite: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; buflen: ub4;
      piece: ub1; ctxp: Pointer=nil; cbfp: Pointer=nil; csid: ub2=0; csfrm: ub1=SQLCS_IMPLICIT): sword; cdecl;
    NlsCharSetNameToID: function(env: POCIEnv; name: PUTF8Char): sword; cdecl;
    StmtPrepare2: function(svchp: POCISvcCtx; var stmtp: POCIStmt; errhp: POCIError;
      stmt: text; stmt_len: ub4; key: text; key_len: ub4;
      language:ub4; mode: ub4): sword; cdecl;
    StmtRelease: function(stmtp: POCIStmt; errhp: POCIError; key: text; key_len: ub4;
      mode: ub4): sword; cdecl;
    TypeByName: function(env: POCIEnv; errhp: POCIError; svchp: POCISvcCtx;
      schema_name: text; s_length: ub4; type_name: text; t_length: ub4; version_name: text; v_length: ub4;
      pin_duration: OCIDuration; get_option: ub4; var tdo: POCIType): sword; cdecl;
    ObjectNew: function(env: POCIEnv; errhp: POCIError; svchp: POCISvcCtx; typecode: OCITypeCode;
      tdo: POCIType; table: dvoid; duration: OCIDuration; value: boolean; var instance: dvoid): sword; cdecl;
    ObjectFree: function(env: POCIEnv; errhp: POCIError; instance: dvoid; flag: ub2): sword; cdecl;
    NumberFromInt: function(errhp: POCIError; inum: dvoid; inum_length: uword; inum_s_flag: uword;
      var number: OCINumber): sword; cdecl;
    StringAssignText : function(env: POCIEnv; errhp: POCIError; rhs: OraText; rhs_len: ub4;
      var lhs: POCIString): sword; cdecl;
    CollAppend: function(env: POCIEnv; errhp: POCIError; elem: dvoid; elemind: dvoid;
      coll: POCIColl): sword; cdecl;
    BindObject: function(bindp: POCIBind; errhp: POCIError; type_: POCIType; var pgvpp: dvoid;
      pvszsp: pub4; indpp: pdvoid; indszp: pub4): sword; cdecl;
    PasswordChange: function(svchp: POCISvcCtx; errhp: POCIError; const user_name: text; usernm_len: ub4;
      const opasswd: text; opasswd_len: ub4; const npasswd: text; npasswd_len: sb4; mode: ub4): sword; cdecl;
  public
    // the client verion numbers
    major_version, minor_version, update_num, patch_num, port_update_num: sword;
    /// if OCI handles directly Int64 bound parameters (revision >= 11.2)
    SupportsInt64Params: boolean;
    /// OCI will call OCILobGetChunkSize when retrieving BLOB/CLOB content
    // - is enabled by default, to avoid ORA-2481 errors when reading more than
    // 96 MB of data, but you may disable chunking if you prefer by setting false
    UseLobChunks: boolean;
    /// load the oci.dll library
    // - and retrieve all Oci*() addresses for OCI_ENTRIES[] items
    constructor Create;
    /// retrieve the client version as 'oci.dll rev. 11.2.0.1'
    function ClientRevision: RawUTF8;
    /// retrieve the OCI charset ID from a Windows Code Page
    // - will only handle most known Windows Code Page
    // - if aCodePage=0, will use the NLS_LANG environment variable
    // - will use 'WE8MSWIN1252' (CODEPAGE_US) if the Code Page is unknown
    function CodePageToCharSetID(env: pointer; aCodePage: cardinal): cardinal;
    /// raise an exception on error
    procedure Check(Conn: TSQLDBConnection; Stmt: TSQLDBStatement;
      Status: Integer; ErrorHandle: POCIError;
      InfoRaiseException: Boolean=false; LogLevelNoRaise: TSynLogInfo=sllNone);
      {$ifdef HASINLINE} inline; {$endif}
    procedure CheckSession(Conn: TSQLDBOracleConnection; Stmt: TSQLDBStatement;
      Status: Integer; ErrorHandle: POCIError;
      InfoRaiseException: Boolean=false; LogLevelNoRaise: TSynLogInfo=sllNone);
    /// retrieve some BLOB content
    procedure BlobFromDescriptor(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; out result: RawByteString); overload;
    /// retrieve some BLOB content
    procedure BlobFromDescriptor(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; out result: TBytes); overload;
    /// retrieve some BLOB content, save it to the stream
    procedure BlobFromDescriptorToStream(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; stream: TStream);
    /// write some BLOB content, read it from the stream
    procedure BlobToDescriptorFromStream(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; stream: TStream);
    /// retrieve some CLOB/NCLOB content as UTF-8 text
    function ClobFromDescriptor(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
      errhp: POCIError; locp: POCIDescriptor; ColumnDBForm: integer;
      out Text: RawUTF8; TextResize: boolean=true): ub4;
  end;


procedure TSQLDBOracleLib.RetrieveVersion;
begin
  if major_version=0 then begin
    ClientVersion(major_version, minor_version,
      update_num, patch_num, port_update_num);
    SupportsInt64Params := (major_version>11) or ((major_version=11) and (minor_version>1));
    UseLobChunks := true;
  end;
end;

function TSQLDBOracleLib.BlobOpen(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
  errhp: POCIError; locp: POCIDescriptor): ub4;
begin
  result := 0;
  Check(nil,Stmt,LobOpen(svchp,errhp,locp,OCI_LOB_READONLY),errhp);
  try
    Check(nil,Stmt,LobGetLength(svchp,errhp,locp,result),errhp);
  except
    Check(nil,Stmt,LobClose(svchp,errhp,locp),errhp);
    raise;
  end;
end;

function TSQLDBOracleLib.BlobRead(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
  errhp: POCIError; locp: POCIDescriptor; Blob: PByte; BlobLen: ub4;
  csid: ub2; csfrm: ub1): integer;
var Read, ChunkSize: ub4;
    Status: sword;
begin
  result := BlobLen;
  if BlobLen=0 then
    exit; // nothing to read
  if UseLobChunks then begin
    Check(nil,Stmt,LobGetChunkSize(svchp,errhp,locp,ChunkSize),errhp);
    result := 0;
    repeat
      Read := BlobLen;
      Status := LobRead(svchp,errhp,locp,Read,1,Blob,ChunkSize,nil,nil,csid,csfrm);
      inc(Blob,Read);
      inc(result,Read);
    until Status<>OCI_NEED_DATA;
    Check(nil,Stmt,Status,errhp);
  end else
    Check(nil,Stmt,LobRead(svchp,errhp,locp,result,1,Blob,result,nil,nil,csid,csfrm),errhp);
end;

function TSQLDBOracleLib.BlobReadToStream(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
  errhp: POCIError; locp: POCIDescriptor; stream: TStream; BlobLen: ub4;
  csid: ub2; csfrm: ub1): integer;
var Read, ChunkSize: ub4;
    Status: sword;
    tmp: RawByteString;
begin
  result := BlobLen;
  if BlobLen=0 then
    exit; // nothing to read
  if UseLobChunks then begin
    Check(nil,Stmt,LobGetChunkSize(svchp,errhp,locp,ChunkSize),errhp);
    SetLength(tmp,ChunkSize*SynDBOracleBlobChunksCount);
    result := 0;
    repeat
      Read := BlobLen;
      Status := LobRead(svchp,errhp,locp,Read,1,pointer(tmp),length(tmp),nil,nil,csid,csfrm);
      stream.WriteBuffer(pointer(tmp)^,Read);
      inc(result,Read);
    until Status<>OCI_NEED_DATA;
    Check(nil,Stmt,Status,errhp);
  end else begin
    SetLength(tmp,BlobLen);
    Check(nil,Stmt,LobRead(svchp,errhp,locp,result,1,pointer(tmp),result,nil,nil,csid,csfrm),errhp);
    stream.WriteBuffer(pointer(tmp)^,result);
  end;
end;

procedure TSQLDBOracleLib.BlobFromDescriptor(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
  errhp: POCIError; locp: POCIDescriptor; out result: RawByteString);
var Len, Read: ub4;
begin
  Len := BlobOpen(Stmt,svchp,errhp,locp);
  try
    SetLength(result,Len);
    Read := BlobRead(Stmt,svchp,errhp,locp,pointer(result),Len);
    if Read<>Len then
      SetLength(result,Read);
  finally
    Check(nil,Stmt,LobClose(svchp,errhp,locp),errhp);
  end;
end;

procedure TSQLDBOracleLib.BlobFromDescriptor(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
  errhp: POCIError; locp: POCIDescriptor; out result: TBytes);
var Len, Read: ub4;
begin
  Len := BlobOpen(Stmt,svchp,errhp,locp);
  try
    SetLength(result,Len);
    Read := BlobRead(Stmt,svchp,errhp,locp,pointer(result),Len);
    if Read<>Len then
      SetLength(result,Read);
  finally
    Check(nil,Stmt,LobClose(svchp,errhp,locp),errhp);
  end;
end;

procedure TSQLDBOracleLib.BlobFromDescriptorToStream(Stmt: TSQLDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor; stream: TStream);
var Len: ub4;
begin
  Len := BlobOpen(Stmt,svchp,errhp,locp);
  try
    BlobReadToStream(Stmt,svchp,errhp,locp,stream,Len);
  finally
    Check(nil,Stmt,LobClose(svchp,errhp,locp),errhp);
  end;
end;

procedure TSQLDBOracleLib.BlobToDescriptorFromStream(Stmt: TSQLDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor; stream: TStream);
begin
  BlobWriteFromStream(Stmt,svchp,errhp,locp,stream,stream.Size);
end;

function TSQLDBOracleLib.BlobWriteFromStream(Stmt: TSQLDBStatement;
  svchp: POCISvcCtx; errhp: POCIError; locp: POCIDescriptor; stream: TStream;
  BlobLen: ub4; csid: ub2; csfrm: ub1): integer;
var ChunkSize, l_Read, l_Write, l_Offset: Longint;
    tmp: RawByteString;
begin
  Check(nil,Stmt,LobGetChunkSize(svchp,errhp,locp,ChunkSize),errhp);
  SetLength(tmp,ChunkSize*SynDBOracleBlobChunksCount);
  l_Offset := 1;
  while stream.Position<stream.Size do begin
    l_Read := stream.Read(pointer(tmp)^,length(tmp));
    l_Write := l_Read;
    Check(nil,Stmt,LobWrite(svchp,errhp,locp,l_Write,l_Offset,
      pointer(tmp),l_Read,OCI_ONE_PIECE),errhp);
    inc(l_Offset,l_Write);
  end;
  result := l_Offset;
end;

function TSQLDBOracleLib.ClobFromDescriptor(Stmt: TSQLDBStatement; svchp: POCISvcCtx;
  errhp: POCIError; locp: POCIDescriptor; ColumnDBForm: integer;
  out Text: RawUTF8; TextResize: boolean): ub4;
var Len: ub4;
begin
  Len := BlobOpen(Stmt,svchp,errhp,locp);
  try
    if Len>0 then begin
      Len := Len*3; // max UTF-8 size according to number of characters
      SetLength(Text,Len);
      result := BlobRead(Stmt,svchp,errhp,locp,pointer(Text),Len,OCI_CHARSET_UTF8,ColumnDBForm);
      if TextResize then
        SetLength(Text,result) else
        Text[result+1] := #0; // ensure ASCIIZ (e.g. when escaping to JSON)
    end else
      result := 0;
  finally
    Check(nil,Stmt,LobClose(svchp,errhp,locp),errhp);
  end;
end;

procedure TSQLDBOracleLib.HandleError(Conn: TSQLDBConnection;
  Stmt: TSQLDBStatement; Status: Integer; ErrorHandle: POCIError;
  InfoRaiseException: Boolean; LogLevelNoRaise: TSynLogInfo);
var msg: RawUTF8;
    tmp: array[0..3071] of AnsiChar;
    L, ErrNum: integer;
begin
  case Status of
    OCI_ERROR, OCI_SUCCESS_WITH_INFO: begin
      tmp[0] := #0;
      ErrorGet(ErrorHandle,1,nil,ErrNum,tmp,sizeof(tmp),OCI_HTYPE_ERROR);
      L := SynCommons.StrLen(@tmp);
      while (L>0) and (tmp[L-1]<' ') do begin
        tmp[L-1] := #0; // trim right #10
        dec(L);
      end;
      msg := CurrentAnsiConvert.AnsiBufferToRawUTF8(tmp,L);
      if (Status=OCI_SUCCESS_WITH_INFO) and not InfoRaiseException then begin
        if LogLevelNoRaise=sllNone then // may be e.g. sllWarning
          LogLevelNoRaise := sllInfo;
        if (Conn=nil) and (Stmt<>nil) then
          Conn := Stmt.Connection;
        if Conn<>nil then
          with Conn.Properties do
            if Assigned(OnStatementInfo) then
              OnStatementInfo(Stmt,msg);
      end;
    end;
    OCI_NEED_DATA:
      msg := 'OCI_NEED_DATA';
    OCI_NO_DATA:
      msg := 'OCI_NO_DATA';
    OCI_INVALID_HANDLE:
      msg := 'OCI_INVALID_HANDLE';
    OCI_STILL_EXECUTING:
      msg := 'OCI_STILL_EXECUTING';
    OCI_CONTINUE:
      msg := 'OCI_CONTINUE';
  end;
  if LogLevelNoRaise<>sllNone then
    SynDBLog.Add.Log(LogLevelNoRaise,msg,self) else
    if Stmt=nil then
      raise ESQLDBOracle.CreateUTF8('% error: %',[self,msg]) else
      raise ESQLDBOracle.CreateUTF8('% error: %',[Stmt,msg]);
end;

procedure TSQLDBOracleLib.Check(Conn: TSQLDBConnection; Stmt: TSQLDBStatement;
  Status: Integer; ErrorHandle: POCIError;
  InfoRaiseException: Boolean; LogLevelNoRaise: TSynLogInfo);
begin
  if Status<>OCI_SUCCESS then
    HandleError(Conn,Stmt,Status,ErrorHandle,InfoRaiseException,LogLevelNoRaise);
end;

procedure TSQLDBOracleLib.CheckSession(Conn: TSQLDBOracleConnection; Stmt: TSQLDBStatement; Status: Integer;
  ErrorHandle: POCIError; InfoRaiseException: Boolean; LogLevelNoRaise: TSynLogInfo);
var msg: RawUTF8;
    tmp: array[0..3071] of AnsiChar;
    L, ErrNum: integer;
begin
  if Status <> OCI_ERROR then
    Check(Conn, Stmt, Status, ErrorHandle, InfoRaiseException, LogLevelNoRaise) else begin
    tmp[0] := #0;
    ErrorGet(ErrorHandle,1,nil,ErrNum,tmp,sizeof(tmp),OCI_HTYPE_ERROR);
    L := SynCommons.StrLen(@tmp);
    while (L>0) and (tmp[L-1]<' ') do begin
      tmp[L-1] := #0; // trim right #10
      dec(L);
    end;
    msg := CurrentAnsiConvert.AnsiBufferToRawUTF8(tmp,L);
    if ErrNum = 28001 then
      if Conn <> nil then
        if Conn.PasswordChange then
          Exit;
    if LogLevelNoRaise<>sllNone then
      SynDBLog.Add.Log(LogLevelNoRaise,msg,self) else
      if Stmt=nil then
        raise ESQLDBOracle.CreateUTF8('% error: %',[self,msg]) else
        raise ESQLDBOracle.CreateUTF8('% error: %',[Stmt,msg]);
  end;
end;

function TSQLDBOracleLib.ClientRevision: RawUTF8;
const EXE_FMT: PUTF8Char = '% rev. %.%.%.%';
begin
  if self=nil then
    result := '' else begin
    RetrieveVersion;
    result := FormatUTF8(EXE_FMT,[fLibraryPath,
      major_version,minor_version,update_num,patch_num]);
  end;
end;

const
  // http://download.oracle.com/docs/cd/B19306_01/server.102/b14225/applocaledata.htm#i635016
  // http://www.mydul.net/charsets.html
  CODEPAGES: array[0..26] of record
     Num: cardinal; Charset: cardinal; Text: PUTF8Char end = (
    (Num: 1252; Charset: OCI_WE8MSWIN1252; Text: 'WE8MSWIN1252'),
    (Num: 1250; Charset: 170; Text: 'EE8MSWIN1250'),
    (Num: 1251; Charset: 171; Text: 'CL8MSWIN1251'),
    (Num: 1253; Charset: 174; Text: 'EL8MSWIN1253'),
    (Num: 1254; Charset: 177; Text: 'TR8MSWIN1254'),
    (Num: 1255; Charset: 175; Text: 'IW8MSWIN1255'),
    (Num: 1256; Charset: 560; Text: 'AR8MSWIN1256'),
    (Num: 1257; Charset: 179; Text: 'BLT8MSWIN1257'),
    (Num:  874; Charset: 41;  Text: 'TH8TISASCII'),
    (Num:  932; Charset: 832; Text: 'JA16SJIS'),
    (Num:  949; Charset: 846; Text: 'KO16MSWIN949'),
    (Num:  936; Charset: 852; Text: 'ZHS16GBK'),
    (Num:  950; Charset: 867; Text: 'ZHT16MSWIN950'),
    (Num: 1258; Charset: 45;  Text: 'VN8MSWIN1258'),
    (Num: CP_UTF8; CharSet: OCI_UTF8; Text: 'UTF8'),
    (Num: CP_UTF16; CharSet: OCI_UTF16ID; Text: 'UTF16'),
    (Num: 437; CharSet: 4; Text: 'US8PC437'),
    (Num: 850; CharSet: 10; Text: 'WE8PC850'),
    (Num: 858; CharSet: 28; Text: 'WE8PC858'),
    (Num: 921; Charset: 176; Text: 'LT8MSWIN921'),
    (Num: 923; Charset: 172; Text: 'ET8MSWIN923'),
     // handle some aliases of code page Num values
    (Num: CP_UTF8; CharSet: OCI_AL32UTF8; Text: 'AL32UTF8'),
    (Num: CP_UTF16; CharSet: 2000; Text: 'AL16UTF16'),
    (Num: CP_UTF16; CharSet: 2002; Text: 'AL16UTF16LE'),
    // wrong approximation (to be fixed)
    (Num:  932; Charset: 830; Text: 'JA16EUC'),
    (Num: 1252; Charset: 46;  Text: 'WE8ISO8859P15'),
    (Num: 1252; Charset: 31;  Text: 'WE8ISO8859P1'));

function SimilarCharSet(aCharset1, aCharset2: cardinal): Boolean;
var i1,i2: integer;
begin
  result := true;
  if aCharset1=aCharset2 then
    exit;
  for i1 := 0 to high(CODEPAGES) do
    if CODEPAGES[i1].Charset=aCharset1 then
    for i2 := 0 to High(CODEPAGES) do
      if (CODEPAGES[i2].Charset=aCharset2) and
         (CODEPAGES[i1].Num=CODEPAGES[i2].Num) then
        exit; // aliases are allowed
  result := false;
end;

function OracleCharSetName(aCharsetID: cardinal): PUTF8Char;
var i: integer;
begin
  for i := 0 to high(CODEPAGES) do
    with CODEPAGES[i] do
    if Charset=aCharsetID then begin
      result := Text;
      exit;
    end;
  result := '?';
end;

function CharSetIDToCodePage(aCharSetID: cardinal): cardinal;
var i: integer;
begin
  for i := 0 to high(CODEPAGES) do
    with CODEPAGES[i] do
    if Charset=aCharsetID then begin
      result := Num;
      exit;
    end;
  result := GetACP; // return the default OS code page if not found
end;

function TSQLDBOracleLib.CodePageToCharSetID(env: pointer;
  aCodePage: cardinal): cardinal;
var ocp: PUTF8Char;
    i: integer;
    nlslang: AnsiString;
begin
  case aCodePage of
  0: begin
      nlslang := AnsiString(GetEnvironmentVariable('NLS_LANG'));
      if nlslang<>'' then
        result := NlsCharSetNameToID(env,pointer(nlslang)) else
        result := CodePageToCharSetID(env,GetACP);
  end;
  CP_UTF8:
    result := OCI_CHARSET_UTF8;
  CP_UTF16:
    result := OCI_UTF16ID;
  else begin
    ocp := CODEPAGES[0].Text; // default is MS Windows Code Page 1252
    for i := 0 to high(CODEPAGES) do
      if aCodePage=CODEPAGES[i].Num then begin
        ocp := CODEPAGES[i].Text;
        break;
      end;
    result := NlsCharSetNameToID(env,ocp);
  end;
  end;
  if result=0 then
    result := OCI_WE8MSWIN1252;
end;

constructor TSQLDBOracleLib.Create;
const LIBNAME = {$ifdef MSWINDOWS}'oci.dll'{$else}'libclntsh.so'{$endif};
var P: PPointer;
    i: integer;
    l1, l2, l3: TFileName;
begin
  if (SynDBOracleOCIpath<>'') and DirectoryExists(SynDBOracleOCIpath) then
    l1 := ExtractFilePath(ExpandFileName(SynDBOracleOCIpath+PathDelim))+LIBNAME;
  l2 := ExeVersion.ProgramFilePath+LIBNAME;
  if not FileExists(l2) then begin
    l2 := ExeVersion.ProgramFilePath+'OracleInstantClient';
    if not DirectoryExists(l2) then begin
      l2 := ExeVersion.ProgramFilePath+'OCI';
      if not DirectoryExists(l2) then
        l2 := ExeVersion.ProgramFilePath+'Oracle';
    end;
    l2 := l2+PathDelim+LIBNAME;
  end;
  l3 := GetEnvironmentVariable('ORACLE_HOME');
  if l3<>'' then
    l3 := IncludeTrailingPathDelimiter(l3)+'bin'+PathDelim+LIBNAME;
  TryLoadLibrary([l1, l2, l3, LIBNAME], ESQLDBOracle);
  P := @@ClientVersion;
  for i := 0 to High(OCI_ENTRIES) do begin
    P^ := GetProcAddress(fHandle,OCI_ENTRIES[i]);
    if P^=nil then begin
      FreeLibrary(fHandle);
      fHandle := 0;
      raise ESQLDBOracle.CreateUTF8('Invalid %: missing %',[LIBNAME,OCI_ENTRIES[i]]);
    end;
    inc(P);
  end;
end;

var
  OCI: TSQLDBOracleLib = nil;


{ TSQLDBOracleConnectionProperties }

class function TSQLDBOracleConnectionProperties.ExtractTnsName(
  const aServerName: RawUTF8): RawUTF8;
var i: integer;
begin
  i := PosExChar('/',aServerName);
  if i=0 then
    result := aServerName else
    result := copy(aServerName,i+1,100);
end;

function TSQLDBOracleConnectionProperties.IsCachable(P: PUTF8Char): boolean;
begin
  result := false; // no client-side cache, only server-side
end;

constructor TSQLDBOracleConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  fDBMS := dOracle;
  fBatchSendingAbilities := [cCreate,cUpdate,cDelete]; // array DML feature
  fBatchMaxSentAtOnce := 10000;  // iters <= 32767 for better performance
  inherited Create(aServerName,'',aUserID,aPassWord);
  GlobalLock;
  try
    if OCI=nil then
      GarbageCollectorFreeAndNil(OCI,TSQLDBOracleLib.Create);
  finally
    GlobalUnLock;
  end;
  fBlobPrefetchSize := 4096;
  fRowsPrefetchSize := 128*1024;
  fStatementCacheSize := 30; // default is 20
  fInternalBufferSize := 128*1024; // 128 KB
  fEnvironmentInitializationMode := OCI_EVENTS or OCI_THREADED or OCI_OBJECT;
end;

function TSQLDBOracleConnectionProperties.GetClientVersion: RawUTF8;
begin
  result := OCI.ClientRevision;
end;

procedure TSQLDBOracleConnectionProperties.GetForeignKeys;
begin
  with Execute(
    'select b.owner||''.''||b.table_name||''.''||b.column_name col,'+
    '       c.owner||''.''||c.table_name||''.''||c.column_name ref'+
    '  from all_cons_columns b, all_cons_columns c, all_constraints a'+
    ' where b.constraint_name=a.constraint_name and a.owner=b.owner '+
       'and b.position=c.position and c.constraint_name=a.r_constraint_name '+
       'and c.owner=a.r_owner and a.constraint_type = ''R''',[]) do
   while Step do
     fForeignKeys.Add(ColumnUTF8(0),ColumnUTF8(1));
end;

function TSQLDBOracleConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBOracleConnection.Create(self);
end;

procedure TSQLDBOracleConnectionProperties.PasswordChanged(const ANewPassword: RawUTF8);
begin
  SynDBLog.Add.Log(sllDB, 'PasswordChanged method called',self);
  fPassWord := ANewPassword;
  if Assigned(FOnPasswordChanged) then
    FOnPasswordChanged(Self);
end;

function TSQLDBOracleConnectionProperties.SQLLimitClause(AStmt: TSynTableStatement): TSQLDBDefinitionLimitClause;
begin
  if AStmt.OrderByField<>nil then begin
    result.Position := posOuter;
    result.InsertFmt := 'select * from (%) where rownum<=%';
  end else
    result := inherited SQLLimitClause(AStmt);
end;


{ TSQLDBOracleConnection }

procedure TSQLDBOracleConnection.Commit;
begin
  inherited Commit;
  if fTrans=nil then
    raise ESQLDBOracle.CreateUTF8('Invalid %.Commit call',[self]);
  try
    OCI.Check(self,nil,OCI.TransCommit(fContext,fError,OCI_DEFAULT),fError);
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSQLDBOracleConnection.Connect;
var log: ISynLog;
    Props: TSQLDBOracleConnectionProperties;
    mode: ub4;
    msg: RawUTF8;
    r: sword;
const
    type_owner_name: RawUTF8 = 'SYS';
    type_NymberListName: RawUTF8 = 'ODCINUMBERLIST';
    type_Varchar2ListName: RawUTF8 = 'ODCIVARCHAR2LIST';
    type_Credential: array[boolean] of integer = (OCI_CRED_RDBMS,OCI_CRED_EXT);
begin
  log := SynDBLog.Enter(self,'Connect');
  Disconnect; // force fTrans=fError=fServer=fContext=nil
  Props := Properties as TSQLDBOracleConnectionProperties;
  with OCI do
  try
    if fEnv=nil then begin
      // will use UTF-8 encoding by default, in a multi-threaded context
      // OCI_EVENTS is needed to support Oracle RAC Connection Load Balancing
      r := EnvNlsCreate(fEnv,Props.EnvironmentInitializationMode,
        nil,nil,nil,nil,0,nil,OCI_CHARSET_UTF8,OCI_CHARSET_UTF8);
      if r <> OCI_SUCCESS then
        raise ESQLDBOracle.CreateUTF8('OCIEnvNlsCreate fails with code %', [r]);
    end;
    HandleAlloc(fEnv,fError,OCI_HTYPE_ERROR);
    HandleAlloc(fEnv,fServer,OCI_HTYPE_SERVER);
    HandleAlloc(fEnv,fContext,OCI_HTYPE_SVCCTX);
    Check(self,nil,ServerAttach(fServer,fError,pointer(Props.ServerName),
      length(Props.ServerName),0),fError);
    // we don't catch all errors here, since Client may ignore unhandled ATTR
    AttrSet(fContext,OCI_HTYPE_SVCCTX,fServer,0,OCI_ATTR_SERVER,fError);
    HandleAlloc(fEnv,fSession,OCI_HTYPE_SESSION);
    AttrSet(fSession,OCI_HTYPE_SESSION,pointer(Props.UserID),
      length(Props.UserID),OCI_ATTR_USERNAME,fError);
    AttrSet(fSession,OCI_HTYPE_SESSION,pointer(Props.Password),
      length(Props.Password),OCI_ATTR_PASSWORD,fError);
    AttrSet(fSession,OCI_HTYPE_SESSION,@Props.fBlobPrefetchSize,0,
      OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE,fError);
    AttrSet(fContext,OCI_HTYPE_SVCCTX,fSession,0,OCI_ATTR_SESSION,fError);
    HandleAlloc(fEnv,fTrans,OCI_HTYPE_TRANS);
    AttrSet(fContext,OCI_HTYPE_SVCCTX,fTrans,0,OCI_ATTR_TRANS,fError);
    if Props.UseCache then begin
      AttrSet(fContext,OCI_HTYPE_SVCCTX,@Props.fStatementCacheSize,0,
        OCI_ATTR_STMTCACHESIZE,fError);
      mode := OCI_STMT_CACHE;
    end else
      mode := OCI_DEFAULT;
    if Props.UserID='SYS' then
      mode := mode or OCI_SYSDBA;
    CheckSession(self,nil,SessionBegin(fContext,fError,fSession,type_Credential[Props.UseWallet],mode),fError);
    Check(self,nil,TypeByName(fEnv,fError,fContext,Pointer(type_owner_name),length(type_owner_name),
      Pointer(type_NymberListName),length(type_NymberListName),nil,0,OCI_DURATION_SESSION,OCI_TYPEGET_HEADER,
      fType_numList),fError);
    Check(self,nil,TypeByName(fEnv,fError,fContext,Pointer(type_owner_name),length(type_owner_name),
      Pointer(type_Varchar2ListName),length(type_Varchar2ListName),nil,0,OCI_DURATION_SESSION,OCI_TYPEGET_HEADER,
      fType_strList),fError);
    if fOCICharSet=0 then begin
      // retrieve the charset to be used for inlined CHAR / VARCHAR2 fields
      with NewStatement do
      try
        try
          Execute('SELECT NLS_CHARSET_ID(PROPERTY_VALUE) FROM DATABASE_PROPERTIES'+
            ' WHERE PROPERTY_NAME=''NLS_CHARACTERSET''',true);
          if Step then
            fOCICharSet := ColumnInt(0) else
            fOCICharSet := CodePageToCharSetID(fEnv,0); // retrieve from NLS_LANG
        except // on error, retrieve from NLS_LANG
          fOCICharSet := CodePageToCharSetID(fEnv,0);
        end;
      finally
        Free;
      end;
      fAnsiConvert := TSynAnsiConvert.Engine(CharSetIDToCodePage(fOCICharSet));
    end;
    if Props.UseWallet then
      msg := 'using Oracle Wallet' else
      msg := 'as '+Props.UserID;
    if log<>nil then
      log.log(sllInfo,'Connected to % % with %, codepage % (%/%)',
        [Props.ServerName,msg,Props.ClientVersion,fAnsiConvert.CodePage,
         fOCICharSet,OracleCharSetName(fOCICharSet)],self);
    with NewStatement do
    try // ORM will send date/time as ISO8601 text -> force encoding
      Execute('ALTER SESSION SET NLS_DATE_FORMAT=''YYYY-MM-DD-HH24:MI:SS''',false);
    finally
      Free;
    end;
    with NewStatement do
    try // currency content is returned as SQLT_STR -> force '.' decimal separator
      Execute('alter session set NLS_NUMERIC_CHARACTERS = ". "',false);
    finally
      Free;
    end;
    //Check(TransStart(fContext,fError,0,OCI_DEFAULT),fError);
    inherited Connect; // notify any re-connection
  except
    on E: Exception do begin
      if log<>nil then
        log.log(sllError,E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

constructor TSQLDBOracleConnection.Create(aProperties: TSQLDBConnectionProperties);
var log: ISynLog;
begin
  log := SynDBLog.Enter(self,'Create');
  if not aProperties.InheritsFrom(TSQLDBOracleConnectionProperties) then
    raise ESQLDBOracle.CreateUTF8('Invalid %.Create(%)',[self,aProperties]);
  OCI.RetrieveVersion;
  inherited;
end;

destructor TSQLDBOracleConnection.Destroy;
begin
  inherited Destroy;
  if (OCI<>nil) and (fEnv<>nil) then
    OCI.HandleFree(fEnv,OCI_HTYPE_ENV);
end;

procedure TSQLDBOracleConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if (fError<>nil) and (OCI<>nil) then
    with SynDBLog.Enter(self,'Disconnect'), OCI do begin
      if fTrans<>nil then begin
        // close any opened session
        HandleFree(fTrans,OCI_HTYPE_TRANS);
        fTrans := nil;
        Check(self,nil,SessionEnd(fContext,fError,fSession,OCI_DEFAULT),fError,false,sllError);
        Check(self,nil,ServerDetach(fServer,fError,OCI_DEFAULT),fError,false,sllError);
      end;
      HandleFree(fSession,OCI_HTYPE_SESSION);
      HandleFree(fContext,OCI_HTYPE_SVCCTX);
      HandleFree(fServer,OCI_HTYPE_SERVER);
      HandleFree(fError,OCI_HTYPE_ERROR);
      fSession := nil;
      fContext := nil;
      fServer := nil;
      fError := nil;
    end;
  end;
end;

function TSQLDBOracleConnection.IsConnected: boolean;
begin
  result := fTrans<>nil;
end;

function TSQLDBOracleConnection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBOracleStatement.Create(self);
  if fProperties.UseCache then // client-side cache is disabled in this unit
    TSQLDBOracleStatement(result).fUseServerSideStatementCache := true;
end;

function TSQLDBOracleConnection.PasswordChange: Boolean;
var password: RawUTF8;
begin
  Result := False;
  if Properties is TSQLDBOracleConnectionProperties then
    if Assigned(TSQLDBOracleConnectionProperties(Properties).OnPasswordExpired) then begin
      password := Properties.PassWord;
      if TSQLDBOracleConnectionProperties(Properties).OnPasswordExpired(Self, password) then
        OCI.Check(Self, nil, OCI.PasswordChange(fContext, fError, pointer(Properties.UserID),
          Length(Properties.UserID), Pointer(Properties.PassWord), Length(Properties.PassWord),
          Pointer(password), Length(password), OCI_DEFAULT or OCI_AUTH), fError);
        TSQLDBOracleConnectionProperties(Properties).PasswordChanged(password);
      Result := True;
    end;
end;

procedure TSQLDBOracleConnection.Rollback;
begin
  inherited;
  if fTrans=nil then
    raise ESQLDBOracle.CreateUTF8('Invalid %.RollBack call',[self]);
  OCI.Check(self,nil,OCI.TransRollback(fContext,fError,OCI_DEFAULT),fError);
end;

procedure TSQLDBOracleConnection.StartTransaction;
var log: ISynLog;
begin
  log := SynDBLog.Enter(self,'StartTransaction');
  if TransactionCount>0 then
    raise ESQLDBOracle.CreateUTF8('Invalid %.StartTransaction: nested '+
      'transactions are not supported by the Oracle driver',[self]);
  try
    inherited StartTransaction;
    if fTrans=nil then
      raise ESQLDBOracle.CreateUTF8('Invalid %.StartTransaction call',[self]);
    // Oracle creates implicit transactions, and we'll handle AutoCommit in
    // TSQLDBOracleStatement.ExecutePrepared if TransactionCount=0
    OCI.Check(self,nil,OCI.TransStart(fContext,fError,0,OCI_DEFAULT),fError);
  except
    on E: Exception do begin
      if (Properties as TSQLDBOracleConnectionProperties).IgnoreORA01453OnStartTransaction and
        (Pos('ORA-01453', E.Message ) > 0) then begin
        if Log<>nil then
          Log.Log(sllWarning, 'It seems that we use DBLink, and Oracle implicitly started transaction. ORA-01453 ignored');
      end else begin
        if fTransactionCount > 0 then
          dec(fTransactionCount);
        raise;
      end;
    end;
  end;
end;

procedure TSQLDBOracleConnection.STRToUTF8(P: PAnsiChar; var result: RawUTF8;
  ColumnDBCharSet, ColumnDBForm: cardinal);
var L: integer;
begin
  L := StrLen(PUTF8Char(P));
  if (L=0) or (ColumnDBCharSet=OCI_AL32UTF8) or (ColumnDBCharSet=OCI_UTF8) or
     (ColumnDBForm=SQLCS_NCHAR) then
    FastSetString(result,P,L) else
    result := fAnsiConvert.AnsiBufferToRawUTF8(P,L);
end;

{$ifndef UNICODE}
procedure TSQLDBOracleConnection.STRToAnsiString(P: PAnsiChar; var result: AnsiString;
  ColumnDBCharSet, ColumnDBForm: cardinal);
var L: integer;
begin
  L := StrLen(PUTF8Char(P));
  if (L=0) or ((ColumnDBCharSet<>OCI_AL32UTF8) and (ColumnDBCharSet<>OCI_UTF8) and
      (ColumnDBForm<>SQLCS_NCHAR) and (fAnsiConvert.CodePage=GetACP)) then
    SetString(result,P,L) else
    result := CurrentAnsiConvert.AnsiToAnsi(fAnsiConvert,P,L);
end;
{$endif UNICODE}


{ TSQLDBOracleStatement }

function TSQLDBOracleStatement.ColumnBlob(Col: integer): RawByteString;
var C: PSQLDBColumnProperty;
    V: PPOCIDescriptor;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := '' else
    if C^.ColumnType=ftBlob then
      if C^.ColumnValueInlined then
        SetString(result,PAnsiChar(V),C^.ColumnValueDBSize) else
        // conversion from POCILobLocator
        with TSQLDBOracleConnection(Connection) do
          OCI.BlobFromDescriptor(self,fContext,fError,V^,result) else
      // need conversion to destination type
      ColumnToTypedValue(Col,ftBlob,result);
end;

function TSQLDBOracleStatement.ColumnBlobBytes(Col: integer): TBytes;
var C: PSQLDBColumnProperty;
    V: PPOCIDescriptor;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := nil else
    if C^.ColumnType=ftBlob then
      if C^.ColumnValueInlined then begin
        SetLength(result,C^.ColumnValueDBSize);
        MoveFast(V^,pointer(result)^,C^.ColumnValueDBSize);
      end else
        // conversion from POCILobLocator
        with TSQLDBOracleConnection(Connection) do
          OCI.BlobFromDescriptor(self,fContext,fError,V^,result) else
      // need conversion to destination type
      result := inherited ColumnBlobBytes(Col);
end;

procedure TSQLDBOracleStatement.ColumnBlobToStream(Col: integer; Stream: TStream);
var C: PSQLDBColumnProperty;
    V: PPOCIDescriptor;
begin
  V := GetCol(Col,C);
  if V<>nil then // column is NULL
    if C^.ColumnType=ftBlob then
      if C^.ColumnValueInlined then
        Stream.WriteBuffer(V^,C^.ColumnValueDBSize) else
        // conversion from POCILobLocator
        with TSQLDBOracleConnection(Connection) do
          OCI.BlobFromDescriptorToStream(self,fContext,fError,V^,stream);
end;

procedure TSQLDBOracleStatement.ColumnBlobFromStream(Col: integer; Stream: TStream);
var C: PSQLDBColumnProperty;
    V: PPOCIDescriptor;
begin
  V := GetCol(Col,C);
  if V<>nil then begin // V=nil means column is NULL
    if C^.ColumnType=ftBlob then
      if C^.ColumnValueInlined then
        raise ESQLDBOracle.CreateUTF8('%.ColumnBlobFromStream(ColumnValueInlined) '+
          'not supported',[self]) else
        // conversion from POCILobLocator
        with TSQLDBOracleConnection(Connection) do
          OCI.BlobToDescriptorFromStream(self,fContext,fError,V^,stream);
  end else
    raise ESQLDBOracle.CreateUTF8('Unexpected %.ColumnBlobFromStream(null): '+
      'use EMPTY_BLOB() to initialize it',[self]);
end;

function TSQLDBOracleStatement.ColumnCurrency(Col: integer): currency;
var C: PSQLDBColumnProperty;
    V: PUTF8Char;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := 0 else
    if C^.ColumnType=ftCurrency then  // encoded as SQLT_STR
      PInt64(@result)^ := StrToCurr64(V) else
      ColumnToTypedValue(Col,ftCurrency,result);
end;

function TSQLDBOracleStatement.ColumnDateTime(Col: integer): TDateTime;
var C: PSQLDBColumnProperty;
    V: POracleDate;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := 0 else
    if C^.ColumnType=ftDate then
      if C^.ColumnValueDBType=SQLT_DAT then
        // types match -> fast direct retrieval
        result := V^.ToDateTime else
        // convert from SQLT_INTERVAL_YM/SQLT_INTERVAL_DS text
        IntervalTextToDateTimeVar(pointer(V),result) else
      // need conversion to destination type
      ColumnToTypedValue(Col,ftDate,result);
end;

function TSQLDBOracleStatement.ColumnDouble(Col: integer): double;
var C: PSQLDBColumnProperty;
    V: pointer;
    Curr: currency;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := 0 else
    case C^.ColumnType of // optimized for ToDataSet() in SynDBVCL.pas
    ftDouble:   result := unaligned(PDouble(V)^);
    ftInt64:    result := PInt64(V)^;
    ftCurrency: begin
      PInt64(@Curr)^ := StrToCurr64(V); // handle '.5' - not GetExtended()
      result := Curr;
    end;
    else // need conversion to destination type
      ColumnToTypedValue(Col,ftDouble,result);
    end;
end;

function TSQLDBOracleStatement.ColumnInt(Col: integer): Int64;
var C: PSQLDBColumnProperty;
    V: pointer;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := 0 else
    case C^.ColumnType of
    ftInt64:
      if C^.ColumnValueDBType=SQLT_INT then
        result := PInt64(V)^ else
        SetInt64(V,result);
    ftCurrency:
      SetInt64(V,result); // encoded as SQLT_STR
    else
      ColumnToTypedValue(Col,ftInt64,result);
    end;
end;

function TSQLDBOracleStatement.ColumnNull(Col: integer): boolean;
var C: PSQLDBColumnProperty;
begin
  result := GetCol(Col,C)=nil;
end;

procedure TSQLDBOracleStatement.ColumnsToJSON(WR: TJSONWriter);
var V: pointer;
    col, indicator: integer;
    tmp: array[0..31] of AnsiChar;
    U: RawUTF8;
begin // dedicated version to avoid as much memory allocation than possible
  if not Assigned(fStatement) or (CurrentRow<=0) then
    raise ESQLDBOracle.CreateUTF8('%.ColumnsToJSON() with no prior Step',[self]);
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount-1 do // fast direct conversion from OleDB buffer
  with fColumns[col] do begin
    if WR.Expand then
      WR.AddFieldName(ColumnName); // add '"ColumnName":'
    indicator := PSmallIntArray(fRowBuffer)[cardinal(col)*fRowCount+fRowFetchedCurrent];
    if (indicator=-1) or (ColumnType=ftNull) then // ftNull for SQLT_RSET
      WR.AddShort('null') else begin
      if indicator<>0 then
        LogTruncatedColumn(fColumns[col]);
      V := @fRowBuffer[ColumnAttr+fRowFetchedCurrent*ColumnValueDBSize];
      case ColumnType of
       ftInt64:
         if ColumnValueDBType=SQLT_INT then
           WR.Add(PInt64(V)^) else
           WR.AddNoJSONEscape(V); // already as SQLT_STR
       ftDouble:
         WR.AddDouble(unaligned(PDouble(V)^));
       ftCurrency:
         WR.AddFloatStr(V); // already as SQLT_STR
       ftDate:
         if ColumnValueDBType=SQLT_DAT then
           WR.AddNoJSONEscape(@tmp,POracleDate(V)^.ToIso8601(tmp)) else begin
           WR.Add('"');  // SQLT_INTERVAL_YM/SQLT_INTERVAL_DS
           WR.AddDateTime(IntervalTextToDateTime(V));
           WR.Add('"');
         end;
       ftUTF8: begin
         WR.Add('"');
         with TSQLDBOracleConnection(Connection) do
           if ColumnValueInlined then
             STRToUTF8(V,U,ColumnValueDBCharSet,ColumnValueDBForm) else
             OCI.ClobFromDescriptor(self,fContext,fError,PPOCIDescriptor(V)^,ColumnValueDBForm,U,false);
         WR.AddJSONEscape(pointer(U));
         WR.Add('"');
       end;
       ftBlob:
         if fForceBlobAsNull then
           WR.AddShort('null') else
           if ColumnValueInlined then
             SetString(U,PAnsiChar(V),ColumnValueDBSize) else begin
             with TSQLDBOracleConnection(Connection) do
               OCI.BlobFromDescriptor(self,fContext,fError,PPOCIDescriptor(V)^,RawByteString(U));
             WR.WrBase64(Pointer(U),length(U),true);
           end;
       else assert(false);
      end;
    end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

procedure TSQLDBOracleStatement.ColumnToSQLVar(Col: Integer; var Value: TSQLVar;
  var Temp: RawByteString);
var C: PSQLDBColumnProperty;
    V: pointer;
    NoDecimal: boolean;
begin // dedicated version to avoid as much memory allocation than possible
  Value.Options := [];
  V := GetCol(Col,C);
  if V=nil then
    Value.VType := ftNull else
    Value.VType := C^.ColumnType;
  case Value.VType of
    ftNull: ; // do nothing
    ftInt64:
      if C^.ColumnValueDBType=SQLT_INT then
        Value.VInt64 := PInt64(V)^ else
        SetInt64(V,Value.VInt64);  // encoded as SQLT_STR
    ftCurrency: begin
      Value.VInt64 := StrToCurr64(V,@NoDecimal); // encoded as SQLT_STR
      if NoDecimal then
        Value.VType := ftInt64; // encoded e.g. from SQLT_NUM as NUMBER(22,0)
    end;
    ftDouble:
      Value.VInt64 := PInt64(V)^; // copy 64 bit content
    ftDate:
      if C^.ColumnValueDBType=SQLT_DAT then // types match -> fast direct retrieval
        Value.VDateTime := POracleDate(V)^.ToDateTime else
        Value.VDateTime := IntervalTextToDateTime(V);
    ftUTF8: begin
      with TSQLDBOracleConnection(Connection) do
        if C^.ColumnValueInlined then
          STRToUTF8(V,RawUTF8(Temp),C^.ColumnValueDBCharSet,C^.ColumnValueDBForm) else
          OCI.ClobFromDescriptor(self,fContext,fError,PPOCIDescriptor(V)^,
            C^.ColumnValueDBForm,RawUTF8(Temp),false);
      Value.VText := pointer(Temp);
    end;
    ftBlob:
    if fForceBlobAsNull then begin
      Value.VBlob := nil;
      Value.VBlobLen := 0;
      Value.VType := ftNull;
    end else begin
      if C^.ColumnValueInlined then
        SetString(Temp,PAnsiChar(V),C^.ColumnValueDBSize) else
        with TSQLDBOracleConnection(Connection) do
          OCI.BlobFromDescriptor(self,fContext,fError,PPOCIDescriptor(V)^,Temp);
      Value.VBlob := pointer(Temp);
      Value.VBlobLen := length(Temp);
    end;
    else raise ESQLDBOracle.CreateUTF8('%.ColumnToSQLVar: unexpected VType=%',
      [self,ord(Value.VType)]);
  end;
end;

function TSQLDBOracleStatement.ColumnToVariant(Col: integer;
  var Value: Variant): TSQLDBFieldType;
var C: PSQLDBColumnProperty;
    V: pointer;
    tmp: RawUTF8;
    NoDecimal: boolean;
begin // dedicated version to avoid as much memory allocation than possible
  V := GetCol(Col,C);
  if V=nil then
    result := ftNull else
    result := C^.ColumnType;
  VarClear(Value);
  with TVarData(Value) do begin
    VType := MAP_FIELDTYPE2VARTYPE[result];
    case result of
      ftNull: ; // do nothing
      ftInt64:
        if C^.ColumnValueDBType=SQLT_INT then
          VInt64 := PInt64(V)^ else
          SetInt64(V,VInt64);  // encoded as SQLT_STR
      ftCurrency: begin
        VInt64 := StrToCurr64(V,@NoDecimal); // encoded as SQLT_STR
        if NoDecimal then begin
          VType := varInt64; // encoded e.g. from SQLT_NUM as NUMBER(22,0)
          result := ftInt64;
        end;
      end;
      ftDouble:
        VInt64 := PInt64(V)^; // copy 64 bit content
      ftDate:
        if C^.ColumnValueDBType=SQLT_DAT then
          VDate := POracleDate(V)^.ToDateTime else // direct retrieval
          IntervalTextToDateTimeVar(V,VDate); // from SQLT_INTERVAL_* text
      ftUTF8: begin // see TSQLDBStatement.ColumnToVariant() for reference
        VAny := nil;
        with TSQLDBOracleConnection(Connection) do
          if C^.ColumnValueInlined then
          {$ifndef UNICODE}
            if not Connection.Properties.VariantStringAsWideString then begin
              VType := varString;
              STRToAnsiString(V,AnsiString(VAny),C^.ColumnValueDBCharSet,C^.ColumnValueDBForm);
              exit;
            end else
          {$endif}
            STRToUTF8(V,tmp,C^.ColumnValueDBCharSet,C^.ColumnValueDBForm) else
            OCI.ClobFromDescriptor(self,fContext,fError,PPOCIDescriptor(V)^,
              C^.ColumnValueDBForm,tmp);
        {$ifndef UNICODE}
        if not Connection.Properties.VariantStringAsWideString then begin
          VType := varString;
          AnsiString(VAny) := UTF8DecodeToString(pointer(tmp),length(tmp));
        end else
        {$endif}
          UTF8ToSynUnicode(tmp,SynUnicode(VAny));
      end;
      ftBlob: begin
        VAny := nil;
        if C^.ColumnValueInlined then
          SetString(RawByteString(VAny),PAnsiChar(V),C^.ColumnValueDBSize) else
          with TSQLDBOracleConnection(Connection) do
            OCI.BlobFromDescriptor(self,fContext,fError,PPOCIDescriptor(V)^,RawByteString(VAny));
      end;
      else raise ESQLDBOracle.CreateUTF8('%.ColumnToVariant: unexpected % type',
        [self,ord(result)]);
    end;
  end;
end;

function TSQLDBOracleStatement.ColumnUTF8(Col: integer): RawUTF8;
var C: PSQLDBColumnProperty;
    V: PAnsiChar;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := '' else
    if C^.ColumnType=ftUTF8 then
      with TSQLDBOracleConnection(Connection) do
      if C^.ColumnValueInlined then
        // conversion from SQLT_STR (null-terminated string)
        STRToUTF8(V,result,C^.ColumnValueDBCharSet,C^.ColumnValueDBForm) else
        // conversion from POCILobLocator
        OCI.ClobFromDescriptor(self,fContext,fError,PPOCIDescriptor(V)^,
          C^.ColumnValueDBForm,result) else
      // need conversion to destination type
      ColumnToTypedValue(Col,ftUTF8,result);
end;

function TSQLDBOracleStatement.ColumnCursor(Col: integer): ISQLDBRows;
var C: PSQLDBColumnProperty;
    V: PAnsiChar;
begin
  result := nil;
  V := GetCol(Col,C);
  if V<>nil then // column is NULL
    if C^.ColumnValueDBType=SQLT_RSET then begin
      result := TSQLDBOracleStatement.CreateFromExistingStatement(Connection,PPointer(V)^);
      PPointer(V)^ := nil; // caller will release the POCIStmt instance with its ISQLDBRows
    end else
    result := inherited ColumnCursor(Col); // will raise an exception
end;

procedure TSQLDBOracleStatement.BindCursor(Param: integer);
begin
  CheckParam(Param,ftUnknown,paramOut); // ftUnknown+paramOut indicate SQLT_RSET
end;

function TSQLDBOracleStatement.BoundCursor(Param: Integer): ISQLDBRows;
begin
  dec(Param);
  if (cardinal(Param)>=cardinal(length(fBoundCursor))) or
     (fBoundCursor[Param]=nil) then
    raise ESQLDBOracle.CreateUTF8(
      '%.BoundCursor: no BindCursor() on Param #%',[self,Param+1]);
  result := TSQLDBOracleStatement.CreateFromExistingStatement(Connection,fBoundCursor[Param]);
  fBoundCursor[Param] := nil;
end;

constructor TSQLDBOracleStatement.Create(aConnection: TSQLDBConnection);
begin
  if not aConnection.InheritsFrom(TSQLDBOracleConnection) then
    raise ESQLDBOracle.CreateUTF8('Invalid %.Create(%) call',[self,aConnection]);
  inherited Create(aConnection);
  fInternalBufferSize := TSQLDBOracleConnectionProperties(aConnection.Properties).InternalBufferSize;
  if fInternalBufferSize<16384 then // default is 128 KB
    fInternalBufferSize := 16384; // minimal value
end;

destructor TSQLDBOracleStatement.Destroy;
begin
  try
    fTimeElapsed.Resume;
    FreeHandles(false);
    {$ifndef SYNDB_SILENCE}
    SynDBLog.Add.Log(sllDB,'Destroy: stats = % row(s) in %',
      [TotalRowsRetrieved,fTimeElapsed.Stop],self);
    {$endif}
  finally
    inherited;
  end;
end;

constructor TSQLDBOracleStatement.CreateFromExistingStatement(
  aConnection: TSQLDBConnection; aStatement: pointer);
begin
  Create(aConnection);
  fTimeElapsed.Resume;
  try
    fStatement := aStatement;
    try
      fExpectResults := true;
      SetColumnsForPreparedStatement;
      FetchRows;
      if fRowFetched=0 then
        fCurrentRow := -1 else // no data row available
        fCurrentRow := 0; // mark cursor on the first row
    except
      on E: Exception do begin
        fStatement := nil; // do not release the statement in constructor
        FreeHandles(True);
        raise;
      end;
    end;
  finally
    fTimeElapsed.Pause;
  end;
end;

procedure TSQLDBOracleStatement.FetchRows;
var status: integer;
begin
  fRowFetched := 0;
  status := OCI.StmtFetch(fStatement,fError,fRowCount,OCI_FETCH_NEXT,OCI_DEFAULT);
  case Status of
  OCI_SUCCESS:
    fRowFetched := fRowCount; // all rows successfully retrieved
  OCI_NO_DATA: begin
    OCI.AttrGet(fStatement,OCI_HTYPE_STMT,@fRowFetched,nil,OCI_ATTR_ROWS_FETCHED,fError);
    fRowFetchedEnded := true;
  end;
  else
    OCI.Check(nil,self,Status,fError); // will raise error
  end;
  fRowFetchedCurrent := 0;
end;

type
  /// Oracle VARNUM memory structure
  TSQLT_VNU = array[0..21] of byte;
  /// points to a Oracle VARNUM memory structure
  PSQLT_VNU = ^TSQLT_VNU;

procedure Int64ToSQLT_VNU(Value: Int64; OutData: PSQLT_VNU);
var V: Byte;
    minus: Boolean;         // True, if the sign is positive
    Size, Exp, i: Integer;
    Mant: array[0..19] of byte;
begin
  FillcharFast(Mant,sizeof(Mant),0);
  Exp := 0;
  Size := 1;
  minus := Value>=0;
  if not minus then
    Value := not Value;
  while Value>0 do begin
    if Value>=100 then begin
      V := Value mod 100;
      Value := Value div 100;
      inc(Exp);
    end else begin
      V := Value;
      Value := 0;
    end;
    if (V<>0) or (Size>1) then begin
      if minus then
        inc(V) else
        V := (100+1)-V;
      Mant[Size-1] := V;
      inc(Size);
    end;
  end;
  if Size>1 then
    for i := 0 to Size-1 do
      OutData[Size-i] := Mant[i];
  Exp := (Exp+65) or $80;
  if not minus and (Size<high(TSQLT_VNU)) then begin
    Exp := not Exp;
    inc(Size);
    OutData[Size] := (100+2);
  end;
  OutData[1] := Exp;
  OutData[0] := Size;
end;

procedure UnQuoteSQLString(S,D: PUTF8Char; SLen: integer);
begin // internal method, tuned for our OCI process
  if S=nil then
    D^ := #0 else
  if S^<>'''' then
    MoveFast(S^,D^,SLen+1) else begin // +1 to include #0
    inc(S);
    repeat
      if S[0]='''' then
        if S[1]='''' then
          inc(S) else break;
      D^ := S^;
      inc(S);
      inc(D);
    until S^=#0;
    D^ := #0;
  end;
end;

const
  /// 32 MB of data sent at once sounds enough
  MAX_INLINED_PARAM_SIZE = 32*1024*1024;

procedure TSQLDBOracleStatement.ExecutePrepared;
var i,j: PtrInt;
    Env: POCIEnv;
    Context: POCISvcCtx;
    param: PSQLDBParam;
    Type_List: POCIType;
    oData: pointer;
    oDataDAT: ^TOracleDateArray absolute oData;
    oDataINT: ^TInt64Array absolute oData;
    oDataSTR: PUTF8Char;
    oLength: integer;
    oBind: POCIBind;
    oIndicator: array of sb2;
    aIndicator: array of array of sb2;
    oOCIDateTime: POCIDateTime;
    Status, L: integer;
    mode: cardinal;
    Int32: set of 0..127;
    ociArrays: array of POCIArray;
    ociArraysCount: byte;
    num_val: OCINumber;
    tmp: RawUTF8;
    str_val: POCIString;
    {$ifdef FPC_64}
    wasStringHacked: TByteDynArray;
    {$endif FPC_64}
label txt;
begin
  if (fStatement=nil) then
    raise ESQLDBOracle.CreateUTF8('%.ExecutePrepared without previous Prepare',[self]);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  SQLLogBegin(sllSQL);
  try
    ociArraysCount := 0;
    Env := (Connection as TSQLDBOracleConnection).fEnv;
    Context := TSQLDBOracleConnection(Connection).fContext;
    Status := OCI_ERROR;
    try
      fRowFetchedEnded := false;
      // 1. bind parameters
      if fPreparedParamsCount<>fParamCount then
        raise ESQLDBOracle.CreateUTF8('%.ExecutePrepared expected % bound parameters, got %',
          [self,fPreparedParamsCount,fParamCount]);
      if not fExpectResults then
        fRowCount := 1; // to avoid ORA-24333 error
      if (fParamCount>0) then
      if (fParamsArrayCount>0) and not fExpectResults then begin
        // 1.1. Array DML binding
        SetLength(aIndicator,fParamCount);
        for i := 0 to fParamCount-1 do
        with fParams[i] do begin
          if VArray=nil then
            raise ESQLDBOracle.CreateUTF8(
              '%.ExecutePrepared: Parameter #% should be an array',[self,i]);
          if VInt64<>fParamsArrayCount then
            raise ESQLDBOracle.CreateUTF8(
              '%.ExecutePrepared: Parameter #% expected array count %, got %',
              [self,i,fParamsArrayCount,VInt64]);
          SetLength(aIndicator[i],fParamsArrayCount);
          VDBType := SQLT_STR;
          oLength := 23; // max size for ftInt64/ftDouble/ftCurrency
          case VType of
          ftDate: begin
            VDBType := SQLT_DAT;
            SetString(VData,nil,fParamsArrayCount*sizeof(TOracleDate));
            oData := pointer(VData);
            oLength := sizeof(TOracleDate);
          end;
          ftInt64:
            if OCI.SupportsInt64Params then begin
              // starting with 11.2, OCI supports NUMBER conversion to/from Int64
              VDBType := SQLT_INT;
              SetString(VData,nil,fParamsArrayCount*sizeof(Int64));
              oData := pointer(VData);
              oLength := sizeof(Int64);
            end; // prior to 11.2, we will stay with the default SQLT_STR type
          ftUTF8:
            oLength := 7; // minimal aligned length
          ftBlob: begin
            VDBTYPE := SQLT_LVB;
            oLength := 7; // minimal aligned length
          end;
          end;
          for j := 0 to fParamsArrayCount-1 do
            if VArray[j]='null' then // bind null (ftUTF8 should be '"null"')
              aIndicator[i][j] := -1 else begin
              if VDBType=SQLT_INT then
                SetInt64(pointer(Varray[j]),oDataINT^[j]) else
              case VType of
              ftUTF8,ftDate: begin
                L := length(VArray[j])-2; // -2 since quotes will be removed
                if VType=ftDate then
                  if L<=0 then
                    oDataDAT^[j].From(0) else
                    oDataDAT^[j].From(PUTF8Char(pointer(VArray[j]))+1,L) else
                  if L>oLength then
                    if L*fParamsArrayCount>MAX_INLINED_PARAM_SIZE then
                      raise ESQLDBOracle.CreateUTF8(
                        '%.ExecutePrepared: Array parameter #% STR too big',[self,i+1]) else
                      oLength := L;
              end;
              ftBlob: begin
                L := length(VArray[j])+sizeof(integer);
                if L*fParamsArrayCount>MAX_INLINED_PARAM_SIZE then
                  raise ESQLDBOracle.CreateUTF8(
                    '%.ExecutePrepared: Array parameter #% BLOB too big',[self,i+1]) else
                if L>oLength then
                  oLength := L;
              end;
              end;
            end;
          case VDBType of
          SQLT_STR: begin
            inc(oLength); // space for trailing #0
            SetString(VData,nil,oLength*fParamsArrayCount);
            oData := Pointer(VData); // in-place quote removal in text
            oDataSTR := oData;
            for j := 0 to fParamsArrayCount-1 do begin
              UnQuoteSQLString(pointer(VArray[j]),oDataSTR,length(VArray[j]));
              inc(oDataSTR,oLength);
            end;
          end;
          SQLT_LVB: begin
            SetString(VData,nil,oLength*fParamsArrayCount);
            oData := Pointer(VData);
            oDataSTR := oData;
            for j := 0 to fParamsArrayCount-1 do begin
              {$ifdef FPC}
              PInteger(oDataSTR)^ := length(VArray[j]);
              MoveFast(Pointer(VArray[j])^,oDataSTR[4],length(VArray[j]));
              {$else}
              MoveFast(Pointer(PtrInt(VArray[j])-4)^,oDataSTR^,length(VArray[j])+4);
              {$endif}
              inc(oDataSTR,oLength);
            end;
          end;
          end;
          oBind := nil;
          OCI.Check(nil,self,OCI.BindByPos(fStatement,oBind,fError,i+1,oData,oLength,VDBType,
            pointer(aIndicator[i]),nil,nil,0,nil,OCI_DEFAULT),fError);
        end;
        fRowCount := fParamsArrayCount; // set iters count for OCI.StmtExecute()
      end else begin
        // 1.2. One row DML optimized binding
        FillcharFast(Int32,sizeof(Int32),0);
        SetLength(oIndicator,fParamCount);
        SetLength(ociArrays,fParamCount);
        for i := 0 to fParamCount-1 do
        if Length(fParams[i].VArray)>0 then begin
          // 1.2.1. Bind an array as one object
          param := @fParams[i];
          case param.VType of
          ftInt64:
            Type_List := TSQLDBOracleConnection(Connection).fType_numList;
          ftUTF8:
            Type_List := TSQLDBOracleConnection(Connection).fType_strList;
          else
            Type_List := nil;
          end;
          if Type_List=nil then
           raise ESQLDBOracle.CreateUTF8(
              '%.ExecutePrepared: Unsupported array parameter type #%',[self,i+1]);
          ociArrays[ociArraysCount] := nil;
          OCI.Check(nil,self,OCI.ObjectNew(Env, fError, Context, OCI_TYPECODE_VARRAY, Type_List, nil,
            OCI_DURATION_SESSION, True, ociArrays[ociArraysCount]), fError);
          inc(ociArraysCount);
          SetString(param.VData,nil,Length(param.VArray)*sizeof(Int64));
          oData := pointer(param.VData);
          for j := 0 to Length(param.VArray)-1 do
            case param.VType of
            ftInt64: begin
              SetInt64(pointer(param.Varray[j]),oDataINT^[j]);
              OCI.Check(nil,self,OCI.NumberFromInt(fError, @oDataINT[j], sizeof(Int64), OCI_NUMBER_SIGNED, num_val), fError);
              OCI.Check(nil,self,OCI.CollAppend(Env, fError, @num_val, nil, ociArrays[ociArraysCount-1]),fError);
            end;
            ftUTF8: begin
              str_val := nil;
              SynCommons.UnQuoteSQLStringVar(pointer(param.VArray[j]),tmp);
              OCI.Check(nil,self,OCI.StringAssignText(Env, fError, pointer(tmp), length(tmp), str_val), fError);
              OCI.Check(nil,self,OCI.CollAppend(Env, fError, str_val, nil, ociArrays[ociArraysCount-1]),fError);
            end;
            end;
          oBind := nil;
          OCI.Check(nil,self,OCI.BindByPos(fStatement,oBind,fError,i+1,nil,0,SQLT_NTY,
            nil,nil,nil,0,nil,OCI_DEFAULT),fError);
          OCI.BindObject(oBind,fError,Type_List, ociArrays[ociArraysCount-1], nil, nil, nil);
        end else
        // 1.2.2. Bind one simple parameter value
        with fParams[i] do begin
          if VType=ftNull then begin
            oIndicator[i] := -1; // assign a NULL to the column, ignoring input value
            oLength := 0;
            oData := nil;
            VDBType := SQLT_STR;
          end else begin
            oLength := sizeof(Int64);
            oData := @VInt64;
            case VType of
            ftUnknown: begin
              if VInOut=paramIn then
                raise ESQLDBOracle.CreateUTF8(
                  '%.ExecutePrepared: Unexpected IN cursor parameter #%',[self,i+1]);
              VDBType := SQLT_RSET;
              with OCI do
                Check(nil,self,HandleAlloc(Env,PPointer(oData)^,OCI_HTYPE_STMT,0,nil),fError);
              oLength := sizeof(pointer);
            end;
            ftInt64:
              if OCI.SupportsInt64Params then
                // starting with 11.2, OCI supports NUMBER conversion to/from Int64
                VDBType := SQLT_INT else
                // before 11.2, we will use either SQLT_INT, SQLT_STR or SQLT_FLT
                if VInOut=paramIn then
                  if (VInt64>low(integer)) and (VInt64<high(Integer)) then begin
                    // map to 32 bit will always work
                    VDBType := SQLT_INT;
                    Include(Int32,i);
                    oLength := SizeOf(integer); // truncate to 32 bit integer value
                  end else begin
                    VData := Int64ToUtf8(VInt64);      // (SQLT_VNU did not work)
                    goto txt; // IN huge integers will be managed as text
                  end else begin
                  VDBType := SQLT_FLT; // OUT values will be converted as double
                  unaligned(PDouble(oData)^) := VInt64;
                end;
            ftDouble:
              VDBType := SQLT_FLT;
            ftCurrency:
              if VInOut=paramIn then begin
                VData := Curr64ToStr(VInt64);
                goto txt; // input-only currency values will be managed as text
              end else begin
                VDBType := SQLT_FLT; // OUT values will be converted as double
                unaligned(PDouble(oData)^) := PCurrency(oData)^;
              end;
            ftDate:
              if VInOut=paramIn then begin
                VDBType := SQLT_TIMESTAMP; // SQLT_DAT is wrong within WHERE clause
                oOCIDateTime := DateTimeToDescriptor(PDateTime(@VInt64)^);
                SetString(VData,PAnsiChar(@oOCIDateTime),sizeof(oOCIDateTime));
                oData := pointer(VData);
                oLength := sizeof(oOCIDateTime);
              end else begin
                VDBType := SQLT_DAT;  // will work for OUT parameters
                POracleDate(@VInt64)^.From(PDateTime(@VInt64)^);
              end;
            ftUTF8: begin
    txt:      VDBType := SQLT_STR; // use STR external data type (SQLT_LVC fails)
              oLength := Length(VData)+1; // include #0
              if oLength=1 then // '' will just map one #0
                oData := @VData else
                oData := pointer(VData);
              // for OUT param, input text shall be pre-allocated
            end;
            ftBlob:
              if VInOut<>paramIn then
                raise ESQLDBOracle.CreateUTF8(
                  '%.ExecutePrepared: Unexpected OUT blob parameter #%',[self,i+1]) else begin
              oLength := Length(VData);
              if oLength<2000 then begin
                VDBTYPE := SQLT_BIN;
                oData := pointer(VData);
              end else begin
                VDBTYPE := SQLT_LVB; // layout: raw data prepended by int32 len
                {$ifdef FPC_64}
                // in case of FPC+CPU64 TSQLDBParam.VData is a RawByteString and
                // length is stored as SizeInt = Int64 (not int32) -> patch
                // (no patch needed for Delphi, in which len is always longint)
                if Length(VData)>MaxInt then
                  raise ESQLDBOracle.CreateUTF8('%.ExecutePrepared: % blob length ' +
                    'exceeds max size for parameter #%',[self,KB(oLength),i+1]);
                UniqueString(VData); // for thread-safety
                PInteger(PtrInt(VData)-sizeof(Integer))^ := oLength;
                if wasStringHacked=nil then
                  SetLength(wasStringHacked,fParamCount shr 3+1);
                SetBitPtr(pointer(wasStringHacked),i); // for unpatching below
                {$endif FPC_64}
                oData := Pointer(PtrInt(VData)-sizeof(Integer));
                Inc(oLength,sizeof(Integer));
              end;
            end;
            else
              raise ESQLDBOracle.CreateUTF8(
                '%.ExecutePrepared: Invalid parameter #% type=%',[self,i+1,ord(VType)]);
            end;
          end;
          oBind := nil;
          OCI.Check(nil,self,OCI.BindByPos(fStatement,oBind,fError,i+1,oData,oLength,
            VDBType,@oIndicator[i],nil,nil,0,nil,OCI_DEFAULT),fError);
        end;
      end;
      // 2. retrieve column information (if not already done)
      if fExpectResults and (fColumn.Count = 0) then
        // We move this after params binding to prevent "ORA-00932: inconsistent
        // datatypes" during call to StmtExecute with OCI_DESCRIBE_ONLY.
        // Because if called here sometimes it breaks the Oracle shared pool and
        // only `ALTER system flush shared_pool` seems to fix the DB state
        SetColumnsForPreparedStatement;
      // 3. execute prepared statement and dispatch data in row buffers
      if (fColumnCount=0) and (Connection.TransactionCount=0) then
        // for INSERT/UPDATE/DELETE without a transaction: AutoCommit after execution
        mode := OCI_COMMIT_ON_SUCCESS else
        // for SELECT or inside a transaction: wait for an explicit COMMIT
        mode := OCI_DEFAULT;
      Status := OCI.StmtExecute(TSQLDBOracleConnection(Connection).fContext,
        fStatement,fError,fRowCount,0,nil,nil,mode);
      // 4. check execution error, and retrieve data result range
      FetchTest(Status); // error + set fRowCount+fCurrentRow+fRowFetchedCurrent
      Status := OCI_SUCCESS; // mark OK for fBoundCursor[] below
    finally
      {$ifdef FPC_64}
      if wasStringHacked<>nil then // restore patched strings length ASAP
        for i := 0 to fParamCount-1 do
          if GetBitPtr(pointer(wasStringHacked),i) then
            PInteger(PtrInt(fParams[i].VData)-sizeof(Integer))^ := 0;
      {$endif FPC_64}
      for i := 0 to ociArraysCount-1 do
        OCI.Check(nil,self,OCI.ObjectFree(Env, fError, ociArrays[i], OCI_OBJECTFREE_FORCE), fError, false, sllError);
      // 3. release and/or retrieve OUT bound parameters
      if fParamsArrayCount>0 then
      for i := 0 to fParamCount-1 do
        fParams[i].VData := '' else
      for i := 0 to fParamCount-1 do
      with fParams[i] do
      case VType of
      ftUnknown:
        if VInOut=paramOut then
          if Status=OCI_SUCCESS then begin
            SetLength(fBoundCursor,fParamCount);
            fBoundCursor[i] := PPointer(@VInt64)^; // available via BoundCursor()
          end else // on error, release bound statement resource
            if OCI.HandleFree(PPointer(@VInt64)^,OCI_HTYPE_STMT)<>OCI_SUCCESS then
              SynDBLog.Add.Log(sllError,'ExecutePrepared: HandleFree(SQLT_RSET)',self);
      ftInt64:
        if VDBType=SQLT_FLT then // retrieve OUT integer parameter
          VInt64 := trunc(unaligned(PDouble(@VInt64)^));
      ftCurrency:
        if VDBType=SQLT_FLT then // retrieve OUT currency parameter
          PCurrency(@VInt64)^ := unaligned(PDouble(@VInt64)^);
      ftDate:
        case VDBType of
        SQLT_DAT: // retrieve OUT date parameter
          PDateTime(@VInt64)^ := POracleDate(@VInt64)^.ToDateTime;
        SQLT_TIMESTAMP: begin // release OCIDateTime resource
          oOCIDateTime := PPointer(VData)^;
          if OCI.DescriptorFree(oOCIDateTime,OCI_DTYPE_TIMESTAMP)<>OCI_SUCCESS then
            SynDBLog.Add.Log(sllError,'ExecutePrepared: DescriptorFree(OCI_DTYPE_TIMESTAMP)',self);
          VData := '';
        end;
        end;
      ftUTF8:
        if VInOut<>paramIn then // retrieve OUT text parameter
          SetLength(VData,StrLen(pointer(VData)));
      end;
    end;
  finally
    fTimeElapsed.FromExternalMicroSeconds(SQLLogEnd);
  end;
end;

procedure TSQLDBOracleStatement.FetchTest(Status: integer);
begin
  fRowFetched := 0;
  case Status of
    OCI_SUCCESS, OCI_SUCCESS_WITH_INFO: begin
      if fColumnCount<>0 then
        fRowFetched := fRowCount;
      if Status = OCI_SUCCESS_WITH_INFO then
        OCI.Check(nil,self,Status,fError,false,sllWarning);
    end;
    OCI_NO_DATA: begin
      assert(fColumnCount<>0);
      OCI.AttrGet(fStatement,OCI_HTYPE_STMT,@fRowFetched,nil,OCI_ATTR_ROWS_FETCHED,fError);
      fRowFetchedEnded := true;
    end;
    else OCI.Check(nil,self,Status,fError); // will raise error
  end;
  if fRowFetched=0 then begin
    fRowCount := 0;
    fCurrentRow := -1; // no data
  end else begin
    fCurrentRow := 0; // mark cursor on the first row
    fRowFetchedCurrent := 0;
  end;
end;

function TSQLDBOracleStatement.DateTimeToDescriptor(aDateTime: TDateTime): pointer;
var HH,MM,SS,MS,Y,M,D: word;
    env: pointer;
begin
  env := (Connection as TSQLDBOracleConnection).fEnv;
  OCI.Check(nil,self,OCI.DescriptorAlloc(env,result,OCI_DTYPE_TIMESTAMP,0,nil),fError);
  DecodeDate(aDateTime,Y,M,D);
  if Frac(aDateTime)=0 then begin
    HH := 0; MM := 0; SS := 0;
  end else
    DecodeTime(aDateTime,HH,MM,SS,MS);
  OCI.Check(nil,nil,OCI.DateTimeConstruct(env,fError,result,Y,M,D,HH,MM,SS,0,nil,0),fError);
end;

procedure TSQLDBOracleStatement.ReleaseRows;
begin // not implemented yet
  inherited ReleaseRows;
end;

procedure TSQLDBOracleStatement.FreeHandles(AfterError: boolean);
const // see http://gcov.php.net/PHP_5_3/lcov_html/ext/oci8/oci8_statement.c.gcov.php
  RELEASE_MODE: array[boolean] of integer = (OCI_DEFAULT,OCI_STMTCACHE_DELETE);
var i,j: integer;
    P: PPointer;
begin
  if self=nil then
    exit; // avoid GPF
  if fRowBuffer<>nil then
  for i := 0 to fColumnCount-1 do
    with fColumns[i] do
      if not ColumnValueInlined then begin
        P := @fRowBuffer[ColumnAttr]; // first POCILobLocator/POCIStmt item
        for j := 1 to fRowBufferCount do begin
          if P^<>nil then begin
            case ColumnValueDBType of
            SQLT_CLOB, SQLT_BLOB:
              if OCI.DescriptorFree(P^,OCI_DTYPE_LOB)<>OCI_SUCCESS then
                SynDBLog.Add.Log(sllError,'FreeHandles: Invalid OCI_DTYPE_LOB',self);
            SQLT_RSET:
              if OCI.HandleFree(P^,OCI_HTYPE_STMT)<>OCI_SUCCESS then
                SynDBLog.Add.Log(sllError,'FreeHandles: Invalid SQLT_RSET',self);
            else raise ESQLDBOracle.CreateUTF8(
              '%.FreeHandles: Wrong % type for inlined column %',
              [self,ColumnValueDBType,ColumnName]);
            end;
            P^ := nil;
          end;
          inc(P);
        end;
      end;
  if fBoundCursor<>nil then begin
    for i := 0 to high(fBoundCursor) do
      if fBoundCursor[i]<>nil then
        OCI.HandleFree(fBoundCursor[i],OCI_HTYPE_STMT);
    fBoundCursor := nil;
  end;
  if fStatement<>nil then begin
    if fUseServerSideStatementCache then
      OCI.Check(nil,self,OCI.StmtRelease(fStatement,fError,nil,0,RELEASE_MODE[AfterError]),fError) else
      OCI.HandleFree(fStatement,OCI_HTYPE_STMT);
    fStatement := nil;
  end;
  if fError<>nil then begin
    OCI.HandleFree(fError,OCI_HTYPE_ERROR);
    fError := nil;
  end;
  if fRowBuffer<>nil then
    SetLength(fRowBuffer,0); // release internal buffer memory
  if fColumnCount>0 then
    fColumn.Clear;
end;

function TSQLDBOracleStatement.GetCol(Col: Integer;
  out Column: PSQLDBColumnProperty): pointer;
begin
  CheckCol(Col); // check Col value  against fColumnCount
  if not Assigned(fStatement) or (fColumnCount=0) or (fRowCount=0) or (fRowBuffer=nil) then
    raise ESQLDBOracle.CreateUTF8('%.Column*() with no prior Execute',[self]);
  if CurrentRow<=0 then
    raise ESQLDBOracle.CreateUTF8('%.Column*() with no prior Step',[self]);
  Column := @fColumns[Col];
  result := @fRowBuffer[Column^.ColumnAttr+fRowFetchedCurrent*Column^.ColumnValueDBSize];
  case PSmallIntArray(fRowBuffer)[cardinal(Col)*fRowCount+fRowFetchedCurrent] of
    // 0:OK, >0:untruncated length, -1:NULL, -2:truncated (length>32KB)
   -1: result := nil; // NULL
    0: exit;
    else LogTruncatedColumn(Column^);
  end;
end;

function TSQLDBOracleStatement.UpdateCount: integer;
begin
  result := 0;
  if fStatement<>nil then
    OCI.AttrGet(fStatement,OCI_HTYPE_STMT,@result,nil,OCI_ATTR_ROW_COUNT,fError);
end;

procedure TSQLDBOracleStatement.SetColumnsForPreparedStatement;
var aName: RawUTF8;
    Env: POCIEnv;
    i,j: integer;
    oHandle: POCIHandle;
    oDefine: POCIDefine;
    oName: PAnsiChar;
    oNameLen, oScale, oCharSet: integer;
    ColCount, RowSize: cardinal;
    StatementType, oType, oSize: ub2;
    Prefetch: ub4;
    ColumnLongTypes: set of (hasLOB,hasLONG,hasCURS);
    PP: PPointer;
    Indicators: PAnsiChar;
begin
  Env := (Connection as TSQLDBOracleConnection).fEnv;
  with OCI do begin
    // 1. ensure fStatement is SELECT
    if fError=nil then
      HandleAlloc(Env,fError,OCI_HTYPE_ERROR);
    AttrGet(fStatement,OCI_HTYPE_STMT,@StatementType,nil,OCI_ATTR_STMT_TYPE,fError);
    if fExpectResults<>(StatementType=OCI_STMT_SELECT) then
      raise ESQLDBOracle.CreateUTF8('%.SetColumnsForPreparedStatement called with '+
        'ExpectResults=%, whereas StatementType=%',[self,ord(fExpectResults),StatementType]);
    if not fExpectResults then begin
      fRowCount := 1; // iters=1 by default
      exit; // no row data expected -> leave fColumnCount=0
    end;
    // 2. retrieve rows column types
    Check(nil,self,StmtExecute(TSQLDBOracleConnection(Connection).fContext,fStatement,fError,
      1,0,nil,nil,OCI_DESCRIBE_ONLY),fError);
    ColCount := 0;
    AttrGet(fStatement,OCI_HTYPE_STMT,@ColCount,nil,OCI_ATTR_PARAM_COUNT,fError);
    RowSize := ColCount*sizeof(sb2); // space for indicators
    ColumnLongTypes := [];
    fColumn.Capacity := ColCount;
    for i := 1 to ColCount do begin
      oHandle := nil;
      ParamGet(fStatement,OCI_HTYPE_STMT,fError,oHandle,i);
      AttrGet(oHandle,OCI_DTYPE_PARAM,@oName,@oNameLen,OCI_ATTR_NAME,fError);
      if oNameLen=0 then
        aName := 'col_'+Int32ToUtf8(i) else
        SetString(aName,oName,oNameLen);
      AttrGet(oHandle,OCI_DTYPE_PARAM,@oType,nil,OCI_ATTR_DATA_TYPE,fError);
      AttrGet(oHandle,OCI_DTYPE_PARAM,@oSize,nil,OCI_ATTR_DATA_SIZE,fError);
      with PSQLDBColumnProperty(fColumn.AddAndMakeUniqueName(aName))^ do begin
        ColumnValueDBSize := oSize;
        ColumnValueInlined := true;
        case oType of
        SQLT_CHR, SQLT_VCS, SQLT_AFC, SQLT_AVC, SQLT_STR, SQLT_VST, SQLT_NTY: begin
          ColumnType := ftUTF8;
          ColumnValueDBType := SQLT_STR; // null-terminated string
          inc(ColumnValueDBSize); // must include ending #0
        end;
        SQLT_LNG: begin
          ColumnValueDBSize := 32768; // will be truncated at 32 KB
          ColumnType := ftUTF8;
          ColumnValueDBType := SQLT_STR; // null-terminated string
          include(ColumnLongTypes,hasLONG);
        end;
        SQLT_LVC, SQLT_CLOB: begin
          ColumnType := ftUTF8;
          ColumnValueInlined := false;
          ColumnValueDBType := SQLT_CLOB;
          ColumnValueDBSize := sizeof(POCILobLocator);
          include(ColumnLongTypes,hasLOB);
        end;
        SQLT_RID, SQLT_RDD: begin
          ColumnType := ftUTF8;
          ColumnValueDBType := SQLT_STR; // null-terminated string
          ColumnValueDBSize := 24; // 24 will fit 8 bytes alignment
        end;
        SQLT_VNU, SQLT_FLT, SQLT_BFLOAT, SQLT_BDOUBLE,
        SQLT_IBFLOAT, SQLT_IBDOUBLE: begin
          ColumnType := ftDouble;
          ColumnValueDBType := SQLT_BDOUBLE;
          ColumnValueDBSize := sizeof(Double);
        end;
        SQLT_NUM: begin
          oScale:= 5; // OCI_ATTR_PRECISION is always 38 (on Oracle 11g) :(
          AttrGet(oHandle,OCI_DTYPE_PARAM,@oScale,nil,OCI_ATTR_SCALE,fError);
          ColumnValueDBSize := sizeof(Double);
          case oScale of
          {0: if (major_version>11) or ((major_version=11) and (minor_version>1)) then begin
               // starting with 11.2, OCI supports NUMBER conversion into Int64
               ColumnType := ftInt64;
               ColumnValueDBType := SQLT_INT;
             end else begin
               // we'll work out with null-terminated string
               ColumnType := ftCurrency;
               ColumnValueDBType := SQLT_STR;
               ColumnValueDBSize := 24;
             end;}
             // we found out that a computed column is returned with Scale=0
             // even if it is numeric (OCI 11.2 bug) -> so SQLT_INT won't work
             // in fact, SQLT_STR will make JSON creation faster (already ASCII)
          0..4: begin
            ColumnType := ftCurrency;      // will guess type from results
            ColumnValueDBType := SQLT_STR; // use null-terminated string
            ColumnValueDBSize := 24;
          end else begin
            ColumnType := ftDouble;
            ColumnValueDBType := SQLT_BDOUBLE;
          end;
          end;
        end;
        SQLT_INT, _SQLT_PLI, SQLT_UIN: begin
          ColumnType := ftInt64;
          ColumnValueDBType := SQLT_INT;
          ColumnValueDBSize := sizeof(Int64);
        end;
        SQLT_DAT, SQLT_DATE, SQLT_TIME, SQLT_TIME_TZ,
        SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ: begin
          ColumnType := ftDate;
          ColumnValueDBType := SQLT_DAT;
          ColumnValueDBSize := sizeof(TOracleDate);
        end;
        SQLT_INTERVAL_YM, SQLT_INTERVAL_DS: begin
          ColumnType := ftDate;
          ColumnValueDBType := SQLT_STR; // null-terminated string
          ColumnValueDBSize := 24; // 24 will fit 8 bytes alignment
        end;
        SQLT_BIN: begin
          if fForceBlobAsNull then
            ColumnType := ftNull else
            ColumnType := ftBlob;
          ColumnValueDBType := SQLT_BIN;
        end;
        SQLT_LBI, SQLT_BLOB, SQLT_LVB: begin
          ColumnType := ftBlob;
          ColumnValueInlined := false;
          ColumnValueDBType := SQLT_BLOB;
          ColumnValueDBSize := sizeof(POCILobLocator);
          if fForceBlobAsNull then
            ColumnType := ftNull else
            include(ColumnLongTypes,hasLOB);
        end;
        SQLT_RSET, SQLT_CUR: begin
          ColumnType := ftNull;
          ColumnValueInlined := false;
          ColumnValueDBType := SQLT_RSET;
          ColumnValueDBSize := sizeof(POCIStmt);
          include(ColumnLongTypes,hasCURS);
        end;
        else raise ESQLDBOracle.CreateUTF8('% - Column [%]: unknown type %',
          [self,ColumnName,oType]);
        end;
        inc(RowSize,ColumnValueDBSize);
        if ColumnType=ftUTF8 then begin
          Check(nil,self,AttrGet(oHandle,OCI_DTYPE_PARAM,@ColumnValueDBForm,nil,
            OCI_ATTR_CHARSET_FORM,fError),fError);
          Check(nil,self,AttrGet(oHandle,OCI_DTYPE_PARAM,@ColumnValueDBCharSet,nil,
             OCI_ATTR_CHARSET_ID,fError),fError);
          case ColumnValueDBForm of
          SQLCS_IMPLICIT: begin
            oCharSet := TSQLDBOracleConnection(Connection).fOCICharSet;
            if ColumnValueDBCharSet=SQLCS_IMPLICIT then
              ColumnValueDBCharSet := oCharSet else
              if (ColumnValueDBCharSet<>oCharSet) and
                 not SimilarCharSet(ColumnValueDBCharSet,oCharSet) then
                // log a warning, but use the connection-level code page
                SynDBLog.Add.Log(sllWarning,'Column [%] has % (%) charset - '+
                  'expected % (%) -> possible data loss',[ColumnName,
                  ColumnValueDBCharSet,OracleCharSetName(ColumnValueDBCharSet),
                  oCharSet,OracleCharSetName(oCharSet)],self);
          end;
          SQLCS_NCHAR: // NVARCHAR2 -> set max UTF-8 bytes from chars
            if ColumnValueInlined then begin
              inc(RowSize,ColumnValueDBSize*2);
              ColumnValueDBSize := ColumnValueDBSize*3;
            end;
          end;
        end;
      end;
      // avoid memory leak for cached statement
      if DescriptorFree(oHandle, OCI_DTYPE_PARAM)<>OCI_SUCCESS then
        SynDBLog.Add.Log(sllError, 'Invalid DescriptorFree(OCI_DTYPE_PARAM)',self);
    end;
    assert(fColumn.Count=integer(ColCount));
    // 3. Dispatch data in row buffer
    assert(fRowBuffer=nil);
    fRowCount := (fInternalBufferSize-ColCount shl 4) div RowSize;
    if fRowCount=0 then begin // reserve space for at least one row of data
      fInternalBufferSize := RowSize+ColCount shl 4;
      fRowCount := 1;
    end else
    if (TSQLDBOracleConnectionProperties(Connection.Properties).RowsPrefetchSize>1024)
       and (ColumnLongTypes=[]) then begin // prefetching if no LOB nor LONG column(s)
      Prefetch := 0; // set prefetch by Memory, not by row count
      Check(nil,self,AttrSet(fStatement,OCI_HTYPE_STMT,@Prefetch,0,OCI_ATTR_PREFETCH_ROWS,fError),fError);
      Prefetch := TSQLDBOracleConnectionProperties(Connection.Properties).RowsPrefetchSize;
      Check(nil,self,AttrSet(fStatement,OCI_HTYPE_STMT,@Prefetch,0,OCI_ATTR_PREFETCH_MEMORY,fError),fError);
    end;
    Setlength(fRowBuffer,fInternalBufferSize);
    assert(fRowCount>0);
    if ((hasLOB in ColumnLongTypes) or (hasCURS in ColumnLongTypes)) and
       (fRowCount>100) then
      fRowCount := 100; // do not create too much POCILobLocator items
    fRowBufferCount := fRowCount; // fRowCount may be set to 0: avoid leaking
    // fRowBuffer[] contains Indicators[] + Col0[] + Col1[] + Col2[]...
    Indicators := pointer(fRowBuffer);
    RowSize := fRowBufferCount*ColCount*sizeof(sb2);
    for i := 0 to ColCount-1 do
    with fColumns[i] do begin
      RowSize := ((RowSize-1) shr 3+1)shl 3; // 8 bytes Col*[] alignment
      ColumnAttr := RowSize;
      if not ColumnValueInlined then begin
        PP := @fRowBuffer[RowSize]; // first POCILobLocator item
        for j := 1 to fRowBufferCount do begin
          case ColumnValueDBType of
          SQLT_CLOB, SQLT_BLOB:
            Check(nil,self,DescriptorAlloc(Env,PP^,OCI_DTYPE_LOB,0,nil),fError);
          SQLT_RSET:
            Check(nil,self,HandleAlloc(Env,PP^,OCI_HTYPE_STMT,0,nil),fError);
          else raise ESQLDBOracle.CreateUTF8('%: Wrong % type for %',
            [self,ColumnValueDBType,ColumnName]);
          end;
          inc(PP);
        end;
      end;
      oDefine := nil;
      Check(nil,self,DefineByPos(fStatement,oDefine,fError,i+1,@fRowBuffer[RowSize],
        ColumnValueDBSize,ColumnValueDBType,Indicators,nil,nil,OCI_DEFAULT),fError);
      case ColumnType of
      ftCurrency: // currency content is returned as SQLT_STR
        Check(nil,self,AttrSet(oDefine,OCI_HTYPE_DEFINE,@OCI_CHARSET_WIN1252,0,
          OCI_ATTR_CHARSET_ID,fError),fError);
      ftUTF8:
        case ColumnValueDBForm of
        SQLCS_IMPLICIT: // force CHAR + VARCHAR2 inlined fields charset
          // -> a conversion into UTF-8 will probably truncate the inlined result
          Check(nil,self,AttrSet(oDefine,OCI_HTYPE_DEFINE,@ColumnValueDBCharSet,0,
            OCI_ATTR_CHARSET_ID,fError),fError);
        SQLCS_NCHAR: // NVARCHAR2 + NCLOB will be retrieved directly as UTF-8 content
          Check(nil,self,AttrSet(oDefine,OCI_HTYPE_DEFINE,@OCI_CHARSET_UTF8,0,
            OCI_ATTR_CHARSET_ID,fError),fError);
        end;
      end;
      inc(RowSize,fRowBufferCount*ColumnValueDBSize);
      inc(Indicators,fRowBufferCount*sizeof(sb2));
    end;
    assert(PtrUInt(Indicators-pointer(fRowBuffer))=fRowBufferCount*ColCount*sizeof(sb2));
    assert(RowSize<=fInternalBufferSize);
  end;
end;

procedure TSQLDBOracleStatement.Prepare(const aSQL: RawUTF8;
  ExpectResults: Boolean);
var env: POCIEnv;
    L: PtrInt;
begin
  SQLLogBegin(sllDB);
  try
    try
      if (fStatement<>nil) or (fColumnCount>0) then
        raise ESQLDBOracle.CreateUTF8('%.Prepare should be called only once',[self]);
      // 1. process SQL
      inherited Prepare(aSQL,ExpectResults); // set fSQL + Connect if necessary
      fPreparedParamsCount := ReplaceParamsByNumbers(aSQL,fSQLPrepared,':',true);
      L := Length(fSQLPrepared);
      while (L>0) and (fSQLPrepared[L]<=' ') do // trim right
        dec(L);
      // allow one trailing ';' by writing ';;' or allows 'END;' at the end of a statement
      if (L>5) and (fSQLPrepared[L]=';') and not
         (IdemPChar(@fSQLPrepared[L-3],'END') and (fSQLPrepared[L-4]<='A')) then
        dec(L);
      if L<>Length(fSQLPrepared) then
        SetLength(fSQLPrepared,L); // trim trailing spaces or ';' if needed
      // 2. prepare statement
      env := (Connection as TSQLDBOracleConnection).fEnv;
      with OCI do begin
        HandleAlloc(env,fError,OCI_HTYPE_ERROR);
        if fUseServerSideStatementCache then begin
          if StmtPrepare2(TSQLDBOracleConnection(Connection).fContext,fStatement,
             fError,pointer(fSQLPrepared),length(fSQLPrepared),nil,0,OCI_NTV_SYNTAX,
             OCI_PREP2_CACHE_SEARCHONLY) = OCI_SUCCESS then
            fCacheIndex := 1 else
          Check(nil,self,StmtPrepare2(TSQLDBOracleConnection(Connection).fContext,fStatement,
            fError,pointer(fSQLPrepared),length(fSQLPrepared),nil,0,OCI_NTV_SYNTAX,OCI_DEFAULT),fError);
        end else begin
          HandleAlloc(env,fStatement,OCI_HTYPE_STMT);
          Check(nil,self,StmtPrepare(fStatement,fError,pointer(fSQLPrepared),length(fSQLPrepared),
            OCI_NTV_SYNTAX,OCI_DEFAULT),fError);
        end;
      end;
      // note: if SetColumnsForPreparedStatement is called here, we randomly got
      // "ORA-00932 : inconsistent datatypes" error -> moved to ExecutePrepared
    except
      on E: Exception do begin
        FreeHandles(True);
        raise;
      end;
    end;
  finally
    fTimeElapsed.FromExternalMicroSeconds(SQLLogEnd(' cache=%',[fCacheIndex]));
  end;
end;

function TSQLDBOracleStatement.Step(SeekFirst: boolean): boolean;
var sav, status: integer;
begin
  if not Assigned(fStatement) then
    raise ESQLDBOracle.CreateUTF8('%.Execute should be called before Step',[self]);
  result := false;
  if (fCurrentRow<0) or (fRowCount=0) then
    exit; // no data available at all
  sav := fCurrentRow;
  fCurrentRow := -1;
  if fColumnCount=0 then
    exit; // no row available at all (e.g. for SQL UPDATE) -> return false
  if sav<>0 then begin // ignore if just retrieved ROW #1
    if SeekFirst then begin
      fTimeElapsed.Resume;
      try
{      if OCI.major_version<9 then
        raise ESQLDBOracle.CreateUTF8('Oracle Client % does not support OCI_FETCH_FIRST',
          [OCI.ClientRevision]); }
        status := OCI.StmtFetch(fStatement,fError,fRowCount,OCI_FETCH_FIRST,OCI_DEFAULT);
        FetchTest(Status); // error + set fRowCount+fRowFetchedCurrent
        if fCurrentRow<0 then // should not happen
          raise ESQLDBOracle.Create('OCI_FETCH_FIRST did not reset cursor');
      finally
        fTimeElapsed.Pause;
      end;
    end else begin
      // ensure we have some data in fRowBuffer[] for this row
      inc(fRowFetchedCurrent);
      if fRowFetchedCurrent>=fRowFetched then begin // reached end of buffer
        if fRowFetchedEnded then
          exit; // no more data
        fTimeElapsed.Resume;
        try
          FetchRows;
          if fRowFetched=0 then
            exit; // no more row available -> return false + fCurrentRow=-1
        finally
          fTimeElapsed.Pause;
        end;
      end;
    end;
  end;
  fCurrentRow := sav+1;
  inc(fTotalRowsRetrieved);
  result := true; // mark data available in fRowSetData
end;


initialization
  TSQLDBOracleConnectionProperties.RegisterClassNameForDefinition;
end.

