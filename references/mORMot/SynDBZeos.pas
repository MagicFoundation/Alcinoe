/// ZEOS 7.x direct access classes for SynDB units (not DB.pas based)
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBZeos;

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
  - alexpirate
  - Daniel Kuettner
  - delphinium
  - EgonHugeist (Michael)
  - Joe (jokussoftware)


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

(*
  Note:
  - if you want to work as expected with SQlite3 backend (but how would need to
    do it, since it will be MUCH slower compared to SynDBSQlite3), you need
    to apply some patches for Zeos < 7.2 :

function TZSQLiteCAPIPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if Not Prepared then
     Prepare;
  try
    if LastResultSet <> nil then
      LastResultSet.Close; // reset stmt
    LastResultSet := nil;
    BindInParameters;
    FErrorCode := FPlainDriver.Step(FStmtHandle);
    CheckSQLiteError(FPlainDriver, FStmtHandle, FErrorCode, nil, lcOther,
      ConSettings^.ConvFuncs.ZStringToRaw(SCanNotRetrieveResultsetData, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP),
      ConSettings);
    if ( FErrorCode = SQLITE_ROW ) or ( FErrorCode = SQLITE_DONE) then
      LastResultSet := CreateResultSet(FStmtHandle, FErrorCode);
    Result := LastResultSet;
    inherited ExecuteQueryPrepared;
  except
    raise;
  end;
end;

procedure TZSQLiteResultSet.FreeHandle;
var
  ErrorCode: Integer;
begin
  if FFreeHandle then
  begin
    if Assigned(FStmtHandle) then
      ErrorCode := FPlainDriver.Finalize(FStmtHandle)
    else
      ErrorCode := SQLITE_OK;
    FStmtHandle := nil;
    CheckSQLiteError(FPlainDriver, FStmtHandle, ErrorCode, nil,
      lcOther, 'FINALIZE SQLite VM', ConSettings);
  end
  else
  begin
    if FStmtHandle <> nil then
    begin
      ErrorCode := FPlainDriver.reset(FStmtHandle);
      CheckSQLiteError(FPlainDriver, FStmtHandle, ErrorCode, nil, lcBindPrepStmt, 'Reset Prepared Stmt', ConSettings);
      FStmtHandle := nil;
    end;
    FErrorCode := SQLITE_DONE;
  end;
end;

 ... but you should better upgrade to the latest Zeos 7.3 revision! :)
    See https://synopse.info/forum/viewtopic.php?pid=9146#p9146

*)

{$ifdef NOSYNDBZEOS} // defined in mormot_base.lpk Lazarus package > Custom Options > Defines

interface

implementation

{$else}

{$I Zeos.inc} // define conditionals like ZEOS72UP and ENABLE_*
// for best performance: tune your project options or Zeos.inc
// to define USE_SYNCOMMONS and leverage best of mORMot and ZEOS !

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  Types,
  SysUtils,
  {$ifndef DELPHI5OROLDER}
  Variants,
  {$endif}
  Classes,
  Contnrs,
  // load physical providers as defined by ENABLE_* in Zeos.inc
  // -> you can patch your local Zeos.inc and comment these defines to
  // exclude database engines you don't need
  // or (since 7.2) you simply add the ZEOS_DISABLE_XXX defines to your
  // project options. The units are used but no line code will be compiled.
  {$if defined(ENABLE_ADO) and not defined(ZEOS_DISABLE_ADO)}
  ZDbcAdo,
  {$ifend}
  {$if defined(ENABLE_DBLIB) and not defined(ZEOS_DISABLE_DBLIB)}
  ZDbcDbLib,
  {$ifend}
  {$if defined(ENABLE_MYSQL) and not defined(ZEOS_DISABLE_MYSQL)}
  ZDbcMySql,
  {$ifend}
  {$if defined(ENABLE_POSTGRESQL) and not defined(ZEOS_DISABLE_POSTGRESQL)}
  ZDbcPostgreSql,
  {$ifend}
  {$if defined(ENABLE_INTERBASE) and not defined(ZEOS_DISABLE_INTERBASE)}
  ZDbcInterbase6,
  {$ifend}
  {$if defined(ENABLE_FIREBIRD) and not defined(ZEOS_DISABLE_FIREBIRD)}
  ZDbcFirebird,
  {$ifend}
  {$if defined(ENABLE_SQLITE) and not defined(ZEOS_DISABLE_SQLITE)}
  ZDbcSqLite,
  {$ifend}
  {$if defined(ENABLE_ORACLE) and not defined(ZEOS_DISABLE_ORACLE)}
  ZDbcOracle,
  {$ifend}
  {$if defined(ENABLE_ASA) and not defined(ZEOS_DISABLE_ASA)}
  ZDbcASA,
  {$ifend}
  {$if defined(ENABLE_SQLANY) and not defined(ZEOS_DISABLE_SQLANY)}
  ZDbcSQLAnywhere,
  {$ifend}
  {$if defined(ENABLE_POOLED) and not defined(ZEOS_DISABLE_POOLED)}
  ZDbcPooled,
  {$ifend}
  {$if defined(ENABLE_OLEDB) and not defined(ZEOS_DISABLE_OLEDB)}
  ZDbcOleDB,
  {$ifend}
  {$if defined(ENABLE_ODBC) and not defined(ZEOS_DISABLE_ODBC)}
  ZDbcODBCCon,
  {$ifend}

  // main ZDBC units
  ZCompatibility,
  ZVariant,
  {$ifndef ZEOS80UP}
  ZURL,
  {$endif ZEOS80UP}
  ZDbcIntfs,
  ZDbcResultSet,
  // mORMot units after ZDBC due to some name conflicts (e.g. UTF8ToString)
  SynCommons,
  SynTable,
  SynLog,
  SynDB;


{ -------------- ZEOS database components direct process }

type
  /// Exception type associated to the ZEOS database components
  ESQLDBZEOS = class(ESQLDBException);

  /// implement properties shared by ZEOS connections
  TSQLDBZEOSConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  protected
    fURL: TZURL;
    fStatementParams: TStrings;
    fDBMSName: RawUTF8;
    fSupportsArrayBindings: boolean;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (ZEOS metadata may be used in the future)
    procedure GetForeignKeys; override;
    /// convert ZDBC field type into mORMot fieldtype
    function TZSQLTypeToTSQLDBFieldType(aNativeType: TZSQLType): TSQLDBFieldType;
  public
    /// initialize the properties to connect to the ZEOS engine
    // - aServerName shall contain the ZEOS URI, e.g:
    // $ zdbc:firebird-2.0://127.0.0.1:3050/model?username=sysdba;password=masterkey
    // $ zdbc:mysql://192.168.2.60:3306/world?username=root;password=dev
    // $ sqlite
    // i.e. '[zdbc:]PROTOCOL://HOST:PORT[/DATABASE][?paramname=value]'
    // - you can define the TZConnection.LibraryLocation property by setting a
    // '?LibLocation=...' parameter within the aServerName URL value
    // - or simply use TSQLDBZEOSConnectionProperties.URI() class method
    // - aDatabaseName, aUserID, aPassword are used if not already set as URI
    // in aServerName value
    // - you can use Protocols property to retrieve all available protocol names
    // - note that when run from mORMot's ORM, this class will by default
    // create one connection per thread, which makes some clients (e.g.
    // PostgreSQL) unstable and consuming a lot of resources - you should better
    // maintain one single connection, by setting after Create:
    // ! aExternalDBProperties.ThreadingMode := tmMainConnection;
    // or by adding 'syndb_singleconnection=true' as URI property
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// initialize raw properties to connect to the ZEOS engine
    // - using Zeos' TZURL detailed class - see: src\core\ZURL.pas
    // - this gives all possibilities to add Properties before a connection is opened
    // - you can define the protocol by hand eg. "odbc_w"/"OleDB" and define TSQLDBDefinition
    // to describe the server syntax SynDB and the ORM use behind the abstract driver
    constructor CreateWithZURL(const aURL: TZURL; aDBMS: TSQLDBDefinition; aOwnsURL: Boolean); virtual;
    /// finalize properties internal structures
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBZEOSConnection instance
    function NewConnection: TSQLDBConnection; override;

    /// retrieve the column/field layout of a specified table
    // - this overridden method will use ZDBC metadata to retrieve the information
    procedure GetFields(const aTableName: RawUTF8; out Fields: TSQLDBColumnDefineDynArray); override;
    /// get all table names
    // - this overridden method will use ZDBC metadata to retrieve the information
    // - PostgreSQL note: it was reported that some table names expects to be
    // quoted for this DB engine - and ZDBC won't do it for yourself - please
    // ensure you specify the correct quoted table name e.g. when you register
    // the external PostgreSQL table via function VirtualTableExternalRegister()
    procedure GetTableNames(out Tables: TRawUTF8DynArray); override;
    /// access to the database metadata, as retrieved by ZEOS
    // - returns TRUE if metadata interface has been retrieved
    function GetDatabaseMetadata(out meta: IZDatabaseMetadata): boolean;
    /// compute the ZEOS URI for a given database engine
    // - the optional server name can contain a port number, specified after ':'
    // - you can set an optional full path to the client library name,
    // to be completed on the left side with the executable path
    // - possible use may be:
    // ! PropsOracle := TSQLDBZEOSConnectionProperties.Create(
    // !   TSQLDBZEOSConnectionProperties.URI(dOracle,'','oci64\oci.dll'),
    // !   'tnsname','user',pass');
    // ! PropsFirebird := TSQLDBZEOSConnectionProperties.Create(
    // !   TSQLDBZEOSConnectionProperties.URI(dFirebird,'','Firebird\fbembed.dll'),
    // !   'databasefilename','',');
    // ! PropsFirebird := TSQLDBZEOSConnectionProperties.Create(
    // !   TSQLDBZEOSConnectionProperties.URI(dFirebird,'192.168.1.10:3055',
    // !     'c:\Firebird_2_5\bin\fbclient.dll',false),
    // !  '3camadas', 'sysdba', 'masterkey');
    class function URI(aServer: TSQLDBDefinition;
      const aServerName: RawUTF8; const aLibraryLocation: TFileName='';
      aLibraryLocationAppendExePath: boolean=true): RawUTF8; overload;
    /// compute the ZEOS URI for a given protocol
    // - if a TSQSLDBDefinition may have several protocols (e.g. MSSQL), you
    // can use this overloaded method to select the exact protocol to use if the
    // default one fixed by TSQLDBDefinition does not match your needs
    // - the protocol name should contain the trailing : character, e.g.
    // 'firebird-2.0:' if the default 'firebird-2.5:' is not correct
    class function URI(const aProtocol, aServerName: RawUTF8;
      const aLibraryLocation: TFileName='';
      aLibraryLocationAppendExePath: boolean=true): RawUTF8; overload;
  published
    /// the remote DBMS name, as retrieved from ServerName, i.e. ZEOS URL
    property DBMSName: RawUTF8 read fDBMSName;
    /// direct access to the internal TZURL connection parameters
    property ZeosURL: TZURL read fURL;
    /// direct access to the internal statement parameters
    // - i.e. will be used by IZConnection.PrepareStatementWithParams()
    // - default values (set in Create method) try to achieve best permormance
    property ZeosStatementParams: TStrings read fStatementParams;
    /// if the associated ZDBC provider supports parameters array binding
    // - you should use the BindArray() methods only if this property is TRUE
    property SupportsArrayBindings: boolean read fSupportsArrayBindings;
  end;


  /// implements a connection via the ZEOS access layer
  TSQLDBZEOSConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fDatabase: IZConnection;
  public
    /// prepare a connection to a specified ZEOS database server
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// connect to the specified ZEOS server
    // - should raise an ESQLDBZEOS on error
    procedure Connect; override;
    /// stop connection to the specified ZEOS database server
    // - should raise an ESQLDBZEOS on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// access to the associated ZEOS connection instance
    property Database: IZConnection read fDatabase;
  end;

  /// implements a statement via a ZEOS database connection
  TSQLDBZEOSStatement = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fStatement: IZPreparedStatement;
    fResultSet: IZResultSet;
    fResultInfo: IZResultSetMetaData;
    {$if defined(ZEOS73UP) and defined(USE_SYNCOMMONS)}
    fJSONComposeOptions: TZJSONComposeOptions;
    {$ifend}
  public
    {$if defined(ZEOS73UP) and defined(USE_SYNCOMMONS)}
    procedure AfterConstruction; override;
    {$ifend}
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESQLDBZeos on any error
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: boolean = false); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this implementation will also handle bound array of values (if any),
    // if IZDatabaseInfo.SupportsArrayBindings is true for this provider
    // - this overridden method will log the SQL statement if sllSQL has been
    // enabled in SynDBLog.Family.Level
    // - raise an ESQLDBZeos on any error
    procedure ExecutePrepared; override;
    {$ifdef ZEOS72UP}
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - this overriden implementation will call fReultSet methods to avoid
    // creating most temporary variable
    procedure ColumnsToJSON(WR: TJSONWriter); override;
    {$endif}
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an ESQLDBZeos on any error
    procedure Reset; override;

    /// Access the next or first row of data from the SQL Statement result
    // - return true on success, with data ready to be retrieved by Column*() methods
    // - return false if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - if SeekFirst is TRUE, will put the cursor on the first row of results
    // - raise an ESQLDBZeos on any error
    function Step(SeekFirst: boolean = false): boolean; override;
    /// free IZResultSet/IZResultSetMetaData when ISQLDBStatement is back in cache
    procedure ReleaseRows; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: Integer): Int64; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: Integer): boolean; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: Integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: Integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: Integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUTF8(Col: Integer): RawUTF8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: Integer): RawByteString; override;
  {$if defined(ZEOS73UP) and defined(USE_SYNCOMMONS)}
  public
    /// the ColumnsToJSON options provided by ZDBC
    // - jcoEndJSONObject:
    // cancels last comma, adds the close object bracket '}' and add's the next comma.
    // If not set you can continue writting some custom data into your
    // object json but you also have to finalize each row-object then.
    // - jcoMongoISODate:
    // formats the date,time,datetime values as mongo
    // ISODate("YYYY-MM-DDTHH:NN:ssZ"). Milliseconds are included.
    // So the values are recognized as date type by mongodb.
    // Otherwise mongo threads them as strings.
    // This option might be usefull if you export sql rows using a
    // json streamed file which will be used as an import-file with your mongodb
    // If set the options jcoDATETIME_MAGIC and jcoMilliseconds are ignored
    // - jcoDATETIME_MAGIC add the JSON_SQLDATE_MAGIC on top of data
    // before adding the ISO date,time,datetime value quoted strings
    // - jcoMilliseconds compose the time/datetime values with milliseconds
    // - jcsSkipNulls ignore null columns. So neither fieldname nor the null
    // value will be composed into your JSON. For real big JSON contents
    // it saves loads of space. e.g. if you import a JSON into a mongo cluster
    // you'll have a significant space difference if null's are simply ignored.
    property JSONComposeOptions: TZJSONComposeOptions read fJSONComposeOptions write fJSONComposeOptions 
      default [jcoEndJSONObject];
  {$ifend}
  end;

var
  /// list of all available ZEOS protocols
  // - you have to call SetZEOSProtocols before using it, to update this
  // global list with all initialized ZPlain*Driver units
  // - to be used e.g. within ZEOS URI, as TSQLDBZEOSConnectionProperties.ServerName
  ZEOSProtocols: TRawUTF8DynArray;

/// to be called in order to populate the global ZEOSProtocols list
procedure SetZEOSProtocols;


implementation

{$ifdef ZEOS72UP}
uses ZDbcMetadata {$IFDEF ZEOS73UP} , ZDbcProperties {$ENDIF};
{$else}
const
  FirstDbcIndex = 1;
  TableNameIndex = 3;
  ColumnNameIndex = 4;
  TableColColumnTypeIndex = 5;
  TableColColumnTypeNameIndex = 6;
  TableColColumnSizeIndex = 7;
  TableColColumnDecimalDigitsIndex = 9;
  IndexInfoColColumnNameIndex = 9;
{$endif}

{ TSQLDBZEOSConnectionProperties }

constructor TSQLDBZEOSConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
const
  PCHARS: array[0..8] of PAnsiChar = (
    'ORACLE','FREETDS_MSSQL','MSSQL','INTERBASE','FIREBIRD','MYSQL','SQLITE','POSTGRESQL','JET');
  TYPES: array[-1..high(PCHARS)] of TSQLDBDefinition = (
    dDefault,dOracle,dMSSQL,dMSSQL,dFirebird,dFirebird,dMySQL,dSQLite,dPostgreSQL,dJet{e.g. ADO[JET]});
  // expecting Sybase + ASA support in TSQLDBDefinition
var BrakedPos: Integer;
begin // return e.g. mysql://192.168.2.60:3306/world?username=root;password=dev
  // make syntax like "ADO[ORACLE]"/"ADO[MSSQL]:"/"ADO[JET]" etc... possible
  BrakedPos := PosExChar('[', aServerName);
  if (BrakedPos>0) and ((aServerName[Length(aServerName)]=']') or
     (aServerName[Length(aServerName)-1]=']')) then begin
    fServerName := Copy(aServerName,1,BrakedPos-1);
    fDBMSName := Copy(aServerName,BrakedPos+1,PosExChar(']',aServerName)-1-BrakedPos);
  end else
    fServerName := aServerName;
  if (fServerName<>'') and (PosExChar(':',fServerName)=0) then
    fServerName := fServerName+':';
  if not IdemPChar(Pointer(aServerName),'ZDBC:') then
    fServerName := 'zdbc:'+fServerName;
  fURL := TZURL.Create(UTF8ToString(fServerName));
  if fURL.Database='' then
    fURL.Database := UTF8ToString(aDatabaseName);
  if fURL.UserName='' then
    fURL.UserName := UTF8ToString(aUserID);
  if fURL.Password='' then
    fURL.Password := UTF8ToString(aPassWord);
  if fDBMSName = '' then
    StringToUTF8(fURL.Protocol,fDBMSName);
  CreateWithZURL(fURL, TYPES[IdemPCharArray(pointer(fDBMSName),PCHARS)], true);
end;

procedure TSQLDBZEOSConnectionProperties.GetForeignKeys;
begin
  { TODO : get FOREIGN KEYS from ZEOS metadata ? }
end;

function TSQLDBZEOSConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBZEOSConnection.Create(self);
end;

constructor TSQLDBZEOSConnectionProperties.CreateWithZURL(const aURL: TZURL;
  aDBMS: TSQLDBDefinition; aOwnsURL: Boolean);
{$IFDEF ZEOS73UP}var protocol: RawUTF8;{$endif}
begin // return e.g. mysql://192.168.2.60:3306/world?username=root;password=dev
  if aOwnsURL then 
    fURL := aURL else 
    fURL := TZURL.Create(aURL);
  fDBMS := aDBMS;
  {$ifndef UNICODE}
  fURL.Properties.Values['controls_cp'] := 'CP_UTF8';
  {$endif}
  { Implementation/Enhancements Notes About settings below (from Michael):
    ConnectionProperties:
    Since 7.3 you should look to ZDbcProperties.pas
    All connection Properties including driver dependencies are listed in this file.

    otherwise:
    Make it possible to Assign Parameters on the fly e.g.:
    FireBird:
     - add custom TIL's by hand if tiNone was set. or some more custom settings:
        see ZDbcInterbaseUtils.pas: TransactionParams and DatabaseParams
     - "hard_commit=true" False by default
    PostgreSQL:
     - "oidasblob=true" - False by default
        notify Zeos should use Oid fields as BLob's
     - "CheckFieldVisibility=True/False"
        notify Zeos should determine temporary tables field meta informations too
        required for mORMot?
     - "NoTableInfoCache=True/False" - False by default
        notify Zeos it should use internal TableInfo-cache.
        Set the value to true to save memory
    ADO:
      7.3up
     - "internal_buffer_size=X" in bytes default are 128KB
       this is the max size in bytes you allow Zeos to use for batch-ole array_bindings
       This parameter is only used if "use_ole_update_params=True"
     - "use_ole_update_params=True/False" default = False
       bypassing slow MSADO15.DLL and direct use OleDB parameters for all kind
       of updates including batch Note: current code is also able to handle Out/InOut
       params except for out/inout lob's.
      note: both internal_buffer_size and use_ole_update_params can be used as
       statement parameters as well
    Oracle:
     - "row_prefetch_size=x! in bytes
       this value will be send to OCI and indicates Oracle which
       row_prefetch_size you allow to execute a query
  }
  {$IFDEF ZEOS73UP}
  if fDBMS = dMSSQL then begin
    protocol := LowerCase(StringToUTF8(FURL.Protocol));
    //EH: switch off tds support in any kind -> deprecated and our 64bit lib isn't compiled with libiconv support
    //users permanently run into the encoding issues which can't be resolved using the dblib tds-types:
    //tdsNVarchar/tdsNText is defined but will never be used by DBLIB + SQLServer)
    if {$IFDEF MSWINDOWS}(protocol <> 'ado') and (protocol <> 'oledb') and {$endif}
       (protocol <> 'odbc_w') and (protocol <> 'odbc_a') then
      {$IFDEF MSWINDOWS}
      FURL.Protocol := 'OleDB';
      {$else}
      FURL.Protocol := 'odbc_w'
      {$endif}
  end;
  {$endif}
  inherited Create(StringToUTF8(FURL.HostName),StringToUTF8(FURL.Database),StringToUTF8(FURL.UserName),StringToUTF8(FURL.Password));
  if StrToBoolDef(fURL.Properties.Values['syndb_singleconnection'],false) then
    ThreadingMode := tmMainConnection;
  fUseCache := {$ifdef ZEOS72UP}true{$else}false{$endif}; // caching disabled by default - enabled if stable enough
  case fDBMS of
  dSQLite: begin
    {$ifndef ZEOS72UP}
    fSQLCreateField[ftInt64] := ' BIGINT'; // SQLite3 INTEGER = 32bit for ZDBC!
    {$endif}
  end;
  dFirebird: begin
    if (fURL.HostName='') and // Firebird embedded
       (fURL.Database<>'') then begin
      ThreadingMode := tmMainConnection; // force SINGLE connection
      if not FileExists(fURL.Database) then // create local DB file if needed
        fURL.Properties.Add('createNewDatabase='+UTF8ToString(
          SQLCreateDatabase(StringToUTF8(fURL.Database))));
    end;
    fURL.Properties.Add('codepage=UTF8');
    fUseCache := true; // caching rocks with Firebird ZDBC provider :)
  end;
  dOracle, dPostgreSQL, dMySQL: begin
    fURL.Properties.Add('codepage=UTF8');
    fUseCache := true;
  end;
  end;
  fStatementParams := TStringList.Create;
  case fDBMS of
  dOracle: begin
    {$ifndef ZEOS72UP} // fixed since 7.2up
    // sets OCI_ATTR_PREFETCH_ROWS on prepare a fetch
    // default = 100 on 7.1down
    // since 7.3 the option is ignored
    fStatementParams.Add('prefetch_count=1000');
    {$else}
    //max mem in bytes which OCI(Server) can use for a result on Server-Side
    //fStatementParams.Add('row_prefetch_size=131072');
    //max mem in bytes which Zeos can for batch or resultset buffers
    //fStatementParams.Add('internal_buffer_size=131072');
    {$endif}
  end;
  dSQLite: begin
    {$ifdef ZEOS72UP} // new since 7.2up
    // Bind double values instead of ISO formated DateTime-strings
    //fStatementParams.Add('BindDoubleDateTimeValues=True');
    {$endif}
  end;
  dMySQL: begin
    // use mysql real-prepared api instead of string based once
    // actually it's not realy faster.. just a hint:
    // http://dev.mysql.com/doc/refman/5.0/en/c-api-prepared-statement-problems.html
    //fStatementParams.Add('preferprepared=True');
    // https://mariadb.com/kb/en/library/bulk-insert-row-wise-binding/
    // https://mariadb.com/kb/en/library/bulk-insert-column-wise-binding/
    // if you run into some mysql known issues,
    // since 7.3 you've to explicit unset real_prepared by
    //fStatementParams.Add('emulate_prepares=True'); or
    //fStatementParams.Add('MinExecCountBeforePrepare=-1');
  end;
  dPostgreSQL: begin
    // see https://synopse.info/forum/viewtopic.php?pid=13260#p13260
    fURL.Properties.Add('NoTableInfoCache=true');
    {$IFDEF ZEOS73UP}
    //fURL.Properties.Add(DSProps_BinaryWireResultMode+'=False');
    {$ENDIF}
  end;
  dMSSQL: begin
    fUseCache := true;
    fStatementParams.Add('enhanced_column_info=false');
    //fStatementParams.Add('use_ole_update_params=True'); //see 'ADO'
    //fStatementParams.Add('internal_buffer_size=131072'); //see 'ADO'
    // EH: hooking a syndb bug. Void strings are not accepted by SQLServer if:
    // fixed-width columns are used and option ANSI_PADDING is set to !off!.
    StoreVoidStringAsNull := False;
  end;
  end;
  if fDBMS in [dOracle,dPostgreSQL,dMySQL,dMSSQL] then begin
    // let's set 1024KB / chunk for synopse  or more?
    // retrieving/submitting lob's in chunks. Default is 4096Bytes / Chunk
    // it's depending to your local network speed e.g. bad WLAN or so
    // for Firebird we always using the blob-segment size
    fStatementParams.Add('chunk_size=1048576');
  end;
  if fDBMS in [dPostgreSQL,dFireBird] then begin
    {$ifdef ZEOS72UP} // new since 7.2up
    // Always load the lobs? Or just on accessing them?
    // if you allways copy the data by fetching the row than it doesn't make sense.
    fStatementParams.Add('cachedlob=false'); //default = False
    {$endif}
  end;
end;

destructor TSQLDBZEOSConnectionProperties.Destroy;
begin
  FreeAndNil(fURL);
  FreeAndNil(fStatementParams);
  inherited;
end;

procedure SetZEOSProtocols;
var List: TStringList;
    i,j: integer;
    Protocols: Types.TStringDynArray;
begin
  List := TStringList.Create;
  try
    with DriverManager.GetDrivers do
      for i := 0 to Count-1 do begin
        Protocols := (Items[i] as IZDriver).GetSupportedProtocols;
        for j := 0 to high(Protocols) do
          List.Add(Protocols[j]);
      end;
    List.Sort;
    StringListToRawUTF8DynArray(List,ZEOSProtocols);
  finally
    List.Free;
  end;
end;

function TSQLDBZEOSConnectionProperties.GetDatabaseMetadata(out meta: IZDatabaseMetadata): boolean;
var conn: IZConnection;
begin
  conn := (MainConnection as TSQLDBZEOSConnection).fDatabase;
  result := conn.UseMetadata;
  meta := conn.GetMetadata;
  if result then
    meta.ClearCache; // we need to retrieve the actual metadata
  {$ifdef ZEOS72UP} // new since 7.2up
  if Result and meta.GetDatabaseInfo.SupportsArrayBindings then begin
    case GetDBMS of
      dPostgreSQL: 
        fBatchSendingAbilities := [cCreate, cDelete] // EH: array logic isn't ready 4updates yet
      else
        fBatchSendingAbilities := [cCreate, cUpdate, cDelete];
    end;
    OnBatchInsert := nil;
    fSupportsArrayBindings := True;
  end;
  {$endif}
end;

procedure TSQLDBZEOSConnectionProperties.GetTableNames(out Tables: TRawUTF8DynArray);
var meta: IZDatabaseMetadata;
    res: IZResultSet;
    TableTypes: Types.TStringDynArray;
    n: integer;
begin
  if GetDatabaseMetadata(meta) then begin
    SetLength(TableTypes,1);
    TableTypes[0] := 'TABLE';
    res := meta.GetTables('','','',TableTypes);
    n := 0;
    while res.Next do
      {$IFDEF ZEOS72UP}
      AddSortedRawUTF8(Tables,n,res.GetUTF8String(TableNameIndex));
      {$else}
      AddSortedRawUTF8(Tables,n,SynUnicodeToUtf8(res.GetUnicodeString(TableNameIndex)));
      {$endif}
    SetLength(Tables,n);
  end else
    inherited;
end;

procedure TSQLDBZEOSConnectionProperties.GetFields(
  const aTableName: RawUTF8; out Fields: TSQLDBColumnDefineDynArray);
var meta: IZDatabaseMetadata;
    res: IZResultSet;
    n, i: integer;
    Schema, TableName: RawUTF8;
    sSchema, sTableName: string;
    F: TSQLDBColumnDefine;
    FA: TDynArray;
begin
  if GetDatabaseMetadata(meta) then begin
    SQLSplitTableName(aTableName, Schema,TableName);
    sSchema := UTF8ToString(Schema);
    { mormot does not create the Tables casesensitive but gives mixed cased strings as tablename
      so we normalize the identifiers to database defaults : }
    sTableName :=  meta.{$ifdef ZEOS80UP}GetIdentifierConverter{$else}GetIdentifierConvertor{$endif}.
      ExtractQuote(Utf8ToString(TableName));
    sTableName := meta.AddEscapeCharToWildcards(sTableName); //do not use "like" search patterns ['_','%'] so they need to be escaped
    res := meta.GetColumns('',sSchema,sTableName,'');
    FA.InitSpecific(TypeInfo(TSQLDBColumnDefineDynArray),Fields,djRawUTF8,@n,true);
    FillChar(F,sizeof(F),0);
    while res.Next do begin
      {$IFDEF ZEOS72UP}
      F.ColumnName := res.GetUTF8String(ColumnNameIndex);
      F.ColumnTypeNative := res.GetUTF8String(TableColColumnTypeNameIndex);
      {$else}
      F.ColumnName := SynUnicodeToUtf8(res.GetUnicodeString(ColumnNameIndex));
      F.ColumnTypeNative := SynUnicodeToUtf8(res.GetUnicodeString(TableColColumnTypeNameIndex));
      {$endif}
      F.ColumnType := TZSQLTypeToTSQLDBFieldType(TZSQLType(res.GetInt(TableColColumnTypeIndex)));
      F.ColumnLength := res.GetInt(TableColColumnSizeIndex);  // for char or date types this is the maximum number of characters
      F.ColumnPrecision := res.GetInt(TableColColumnSizeIndex);  // for numeric or decimal types this is precision
      F.ColumnScale := res.GetInt(TableColColumnDecimalDigitsIndex);  // the number of fractional digits
      FA.Add(F);
    end;
    if n>0 then begin
      res := meta.GetIndexInfo('',sSchema,sTableName,false,true);
      while res.Next do begin
        {$IFDEF ZEOS72UP}
        F.ColumnName := res.GetUTF8String(IndexInfoColColumnNameIndex);
        {$else}
        F.ColumnName := SynUnicodeToUtf8(res.GetUnicodeString(IndexInfoColColumnNameIndex));
        {$endif}
        i := FA.Find(F);
        if i>=0 then
          Fields[i].ColumnIndexed := true;
      end;
      SetLength(Fields,n);
    end;
  end;
end;

function TSQLDBZEOSConnectionProperties.TZSQLTypeToTSQLDBFieldType(aNativeType: TZSQLType): TSQLDBFieldType;
begin
  case aNativeType of
    stBoolean, stByte, stShort, stInteger, stLong
    {$ifdef ZEOS72UP}, stSmall, stWord, stLongWord, stULong{$endif}:
      result := ftInt64;
    stFloat, stDouble:
      result := ftDouble;
    stBigDecimal{$ifdef ZEOS72UP}, stCurrency{$endif}:
      result := ftCurrency;
    stDate, stTime, stTimestamp:
      result := ftDate;
    stString, stUnicodeString, {$ifdef ZEOS72UP}stGUID, {$endif} stAsciiStream, stUnicodeStream:
      result := ftUTF8;
    stBytes, stBinaryStream:
      result := ftBlob;
    else raise ESQLDBZEOS.CreateUTF8('%: unexpected TZSQLType "%"',
      [self,GetEnumName(Typeinfo(TZSQLType),ord(aNativeType))^]);
  end;
end;

class function TSQLDBZEOSConnectionProperties.URI(aServer: TSQLDBDefinition;
  const aServerName: RawUTF8; const aLibraryLocation: TFileName;
  aLibraryLocationAppendExePath: boolean): RawUTF8;
const
  /// ZDBC provider names corresponding to SynDB recognized SQL engines
  ZEOS_PROVIDER: array[TSQLDBDefinition] of RawUTF8 = (
    '','','oracle:','mssql:','','mysql:','sqlite:',
    'firebird:','','postgresql:','','');
begin
  result := URI(ZEOS_PROVIDER[aServer],aServerName,
    aLibraryLocation,aLibraryLocationAppendExePath);
end;

class function TSQLDBZEOSConnectionProperties.URI(const aProtocol,aServerName: RawUTF8;
  const aLibraryLocation: TFileName; aLibraryLocationAppendExePath: boolean): RawUTF8;
begin // return e.g. mysql://192.168.2.60:3306/world?username=root;password=dev
  result := trim(aProtocol);
  if result='' then
    exit;
  if aServerName<>'' then
    result := result+'//'+aServerName;
  if aLibraryLocation<>'' then begin
    if (aServerName <> '') and (aServerName[1] = '?') then 
      result := result+';LibLocation=' else 
      result := result+'?LibLocation=';

    if aLibraryLocationAppendExePath then
      result := result+StringToUTF8(ExeVersion.ProgramFilePath);
    result := result+StringToUTF8(aLibraryLocation);
  end;
end;


{ TSQLDBZEOSConnection }

constructor TSQLDBZEOSConnection.Create(aProperties: TSQLDBConnectionProperties);
var url: TZURL;
begin
  inherited Create(aProperties);
  url := (fProperties as TSQLDBZEOSConnectionProperties).fURL;
  fDatabase := DriverManager.GetConnectionWithParams(url.URL,url.Properties);
  // EG: setup the connection transaction behavior now, not once Opened in Connect
  //fDatabase.SetReadOnly(false); // is default
  // about transactions, see https://synopse.info/forum/viewtopic.php?id=2209
  //fDatabase.SetAutoCommit(true); // is default
  fDatabase.SetTransactionIsolation(tiReadCommitted); // will be swapped to tiSerialiable for SQLite
end;

procedure TSQLDBZEOSConnection.Connect;
var log: ISynLog;
begin
  if fDatabase=nil then
    raise ESQLDBZEOS.CreateUTF8('%.Connect() on % failed: Database=nil',
      [self,fProperties.ServerName]);
  log := SynDBLog.Enter(self, 'Connect');
  if log<>nil then
    with (fProperties as TSQLDBZEOSConnectionProperties).fURL do
      log.Log(sllTrace,'Connect to % % for % at %:%',[Protocol,Database,HostName,Port]);
  try
    fDatabase.Open;
    if log<>nil then
      log.log(sllDB,'Connected to % using % %',[fProperties.ServerName,
        fProperties.DatabaseNameSafe,fDatabase.GetClientVersion]);
    inherited Connect; // notify any re-connection
  except
    on E: Exception do begin
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSQLDBZEOSConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if (fDatabase<>nil) and not fDatabase.IsClosed then
      fDatabase.Close;
  end;
end;

function TSQLDBZEOSConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and not fDatabase.IsClosed;
end;

function TSQLDBZEOSConnection.NewStatement: TSQLDBStatement;
begin
  if not IsConnected then
    Connect;
  result := TSQLDBZEOSStatement.Create(self);
end;

procedure TSQLDBZEOSConnection.StartTransaction;
var log: ISynLog;
begin
  log := SynDBLog.Enter(self,'StartTransaction');
  inherited StartTransaction;
  {$IFDEF ZEOS73UP}
  fDatabase.StartTransaction; //returns the txn level
  {$ELSE}
  fDatabase.SetAutoCommit(false);
  {$ENDIF}
end;

procedure TSQLDBZEOSConnection.Commit;
begin
  inherited Commit;
  try
    fDatabase.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
  {$IFNDEF ZEOS73UP} //no longer required zdbc falls back to AC automatically
  fDatabase.SetAutoCommit(true);
  {$ENDIF}
end;

procedure TSQLDBZEOSConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
  {$IFNDEF ZEOS73UP} //no longer required zdbc falls back to AC automatically
  fDatabase.SetAutoCommit(true);
  {$ENDIF}
end;

{ TSQLDBZEOSStatement }

procedure TSQLDBZEOSStatement.Prepare(const aSQL: RawUTF8; ExpectResults: boolean);
begin
  SQLLogBegin(sllDB);
  if (fStatement<>nil) or (fResultSet<>nil) then
    raise ESQLDBZEOS.CreateUTF8('%.Prepare() shall be called once',[self]);
  inherited Prepare(aSQL,ExpectResults); // connect if necessary
  fStatement := (fConnection as TSQLDBZEOSConnection).fDatabase.
    PrepareStatementWithParams({$ifdef UNICODE}UTF8ToString(fSQL){$else}fSQL{$endif}, // see controls_cp=CP_UTF8
    (fConnection.Properties as TSQLDBZEOSConnectionProperties).fStatementParams);
  SQLLogEnd;
end;

{$ifdef ZEOS72UP}
type // see https://synopse.info/forum/viewtopic.php?pid=11946#p11946
  TZeosArrayBinding = class
  protected
    // ZDBC uses pointer references to arrays -> allocated with the class
    fNullArray: array of TBooleanDynArray;
    fInt64Array: array of TInt64DynArray;
    fDoubleArray: array of TDoubleDynArray;
    fCurDynArray: array of TCurrencyDynArray;
    fDateDynArray: array of TDateTimeDynArray;
    fUTF8DynArray: array of TRawUTF8DynArray;
    fBlobDynArray: array of TInterfaceDynArray;
    fDynArraySize: array[ftInt64..ftBlob] of Integer;
  public
    constructor Create(aStatement: TSQLDBZEOSStatement);
  end;

constructor TZeosArrayBinding.Create(aStatement: TSQLDBZEOSStatement);
var p,j,n: integer;
    ndx: array[ftInt64..ftBlob] of integer;
    kind: TSQLDBFieldType;
begin
  with aStatement do begin
    SetLength(fNullArray,fParamCount);
    for p := 0 to fParamCount-1 do
      if fParams[p].VType in [ftInt64..ftBlob] then
        inc(fDynArraySize[fParams[p].VType]);
    SetLength(fInt64Array,fDynArraySize[ftInt64]);
    SetLength(fDoubleArray,fDynArraySize[ftDouble]);
    SetLength(fCurDynArray,fDynArraySize[ftCurrency]);
    SetLength(fDateDynArray,fDynArraySize[ftDate]);
    SetLength(fUTF8DynArray,fDynArraySize[ftUTF8]);
    SetLength(fBlobDynArray,fDynArraySize[ftBlob]);
    fillchar(ndx,sizeof(ndx),0);
    for p := 0 to fParamCount-1 do begin
      if fParams[p].VInt64<>fParamsArrayCount then
        raise ESQLDBZEOS.CreateUTF8(
          '%.ExecutePrepared: #% parameter expected array count %, got %',
          [aStatement,p,fParamsArrayCount,fParams[p].VInt64]);
      SetLength(fNullArray[p],fParamsArrayCount);
      with fParams[p] do begin
        case VType of
        ftUnknown:
          raise ESQLDBZEOS.CreateUTF8(
            '%.ExecutePrepared: Unknown type array parameter #%',[aStatement,p+FirstDbcIndex]);
        ftNull: begin
          // handle null column
          for j := 0 to fParamsArrayCount -1 do
            fNullArray[p][j] := True;
          fStatement.SetDataArray(p+FirstDbcIndex,'',stString,vtUTF8String);
        end;
        else begin
          // array binding of ftInt64..ftBlob values from fParams[p].VArray[]
          for j := 0 to fParamsArrayCount -1 do
            fNullArray[p][j] := VArray[j]='null';
          n := ndx[VType];
          case VType of
          ftInt64: begin
            SetLength(fInt64Array[n],fParamsArrayCount);
            for j := 0 to fParamsArrayCount -1 do
              if not fNullArray[p][j] then
                SetInt64(pointer(VArray[j]),fInt64Array[n][j]);
            fStatement.SetDataArray(p+FirstDbcIndex,fInt64Array[n],stLong);
          end;
          ftDouble: begin
            SetLength(fDoubleArray[n],fParamsArrayCount);
            for j := 0 to fParamsArrayCount -1 do
              if not fNullArray[p][j] then
                fDoubleArray[n][j] := GetExtended(pointer(VArray[j]));
            fStatement.SetDataArray(p+FirstDbcIndex,fDoubleArray[n],stDouble);
          end;
          ftCurrency: begin
            SetLength(fCurDynArray[n],fParamsArrayCount);
            for j := 0 to fParamsArrayCount -1 do
              if not fNullArray[p][j] then
                fCurDynArray[n][j] := StrToCurrency(pointer(VArray[j]));
            fStatement.SetDataArray(p+FirstDbcIndex,fCurDynArray[n],stCurrency);
          end;
          ftDate: begin
            SetLength(fDateDynArray[n],fParamsArrayCount);
            for j := 0 to fParamsArrayCount -1 do
              if not fNullArray[p][j] then
                fDateDynArray[n][j] := Iso8601ToDateTimePUTF8Char(
                  PUTF8Char(pointer(VArray[j]))+1,Length(VArray[j])-2);
            fStatement.SetDataArray(p+FirstDbcIndex,fDateDynArray[n],stTimestamp);
          end;
          ftUTF8: begin
            SetLength(fUTF8DynArray[n],fParamsArrayCount);
            for j := 0 to fParamsArrayCount -1 do
              if not fNullArray[p][j] then
                UnQuoteSQLStringVar(pointer(VArray[j]),fUTF8DynArray[n][j]);
            fStatement.SetDataArray(p+FirstDbcIndex,fUTF8DynArray[n],stString,vtUTF8String);
          end;
          ftBlob: begin
              SetLength(fBlobDynArray[n],fParamsArrayCount);
              for j := 0 to fParamsArrayCount -1 do
                if not fNullArray[p][j] then
                  fBlobDynArray[n][j] := TZAbstractBlob.CreateWithData(
                    Pointer(VArray[j]),length(VArray[j]));
              fStatement.SetDataArray(p+FirstDbcIndex,fBlobDynArray[n],stBinaryStream);
            end;
          end;
          inc(ndx[VType]);
        end;
        end;
      end;
      fStatement.SetNullArray(p+FirstDbcIndex,stBoolean,fNullArray[p]);
    end;
    for kind := low(ndx) to high(ndx) do
      assert(ndx[kind]=fDynArraySize[kind]);
  end;
end;
{$endif ZEOS72UP}

procedure TSQLDBZEOSStatement.ExecutePrepared;
var i,n: integer;
    Props: TSQLDBZEOSConnectionProperties;
    name: string;
    {$ifdef ZEOS72UP}
    arrayBinding: TZeosArrayBinding;
    {$endif}
begin
  SQLLogBegin(sllSQL);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  if fStatement=nil then
    raise ESQLDBZEOS.CreateUTF8('%.ExecutePrepared() invalid call',[self]);
  {$ifndef ZEOS72UP} //commenting this makes it possible to seek cursor pos to 0 and use the interface again -> e.g. ReadOneByOneRate
  if fResultSet<>nil then
    raise ESQLDBZEOS.CreateUTF8('%.ExecutePrepared() miss a Reset',[self]);
  {$endif}
  // 1. bind parameters in fParams[] to fQuery.Params
  {$ifdef ZEOS72UP}
  arrayBinding := nil;
  if fParamsArrayCount>0 then
    with (fConnection.Properties as TSQLDBZEOSConnectionProperties) do
    if fSupportsArrayBindings then
      arrayBinding := TZeosArrayBinding.Create(self) else
      if not fExpectResults then
        raise ESQLDBZEOS.CreateUTF8(
          '%.BindArray() not supported by % provider',[self,DBMSName]);
  try
    if arrayBinding=nil then
  {$else}
  if (fParamsArrayCount>0) and not fExpectResults then
    raise ESQLDBZEOS.CreateUTF8('%.BindArray() not supported',[self]) else
  {$endif}
    for i := fParamCount-1 downto 0 do // EG: downto minimize memallocs
    with fParams[i] do begin
      if (Length(VArray)>0) and (fConnection.Properties.DBMS=dPostgreSQL) then begin
        if VType in [ftInt64,ftCurrency,ftDouble,ftUTF8] then
          VData := BoundArrayToJSONArray(VArray) else
          raise ESQLDBZEOS.CreateUTF8('%.ExecutePrepared: Invalid array type % ' +
            'on bound parameter #%', [Self, ToText(VType)^, i]);
        VType := ftUTF8;
      end;
      case VType of
      ftNull:     fStatement.SetNull(i+FirstDbcIndex,stUnknown);
      ftInt64:    fStatement.SetLong(i+FirstDbcIndex,VInt64);
      ftDouble:   fStatement.SetDouble(i+FirstDbcIndex,unaligned(PDouble(@VInt64)^));
      ftCurrency: {$ifdef ZEOS72UP}
                  fStatement.SetCurrency(i+FirstDbcIndex,PCurrency(@VInt64)^);
                  {$else}
                  fStatement.SetBigDecimal(i+FirstDbcIndex,PCurrency(@VInt64)^);
                  {$endif}
      ftDate:     fStatement.SetTimestamp(i+FirstDbcIndex,PDateTime(@VInt64)^);
      ftUTF8:     {$ifdef ZEOS72UP}
                  fStatement.SetUTF8String(i+FirstDbcIndex,VData);
                  {$else}
                    {$ifdef UNICODE}  // ZWideString = SynUnicode in fact
                    fStatement.SetString(i+FirstDbcIndex,UTF8ToSynUnicode(VData));
                    {$else}
                    fStatement.SetString(i+FirstDbcIndex,VData); // see controls_cp=CP_UTF8
                    {$endif}
                  {$endif}
      ftBlob:     fStatement.SetBlob(i+FirstDbcIndex,stBinaryStream,
                    TZAbstractBlob.CreateWithData(Pointer(VData),length(VData)
                    {$ifndef ZEOS72UP},fStatement.GetConnection{$endif}));
      else
        raise ESQLDBZEOS.CreateUTF8('%.ExecutePrepared: Invalid type parameter #%',
          [self,i]);
      end;
    end;
    // 2. Execute query
    if fExpectResults then begin
      fColumnCount := 0;
      fColumn.ReHash;
      fCurrentRow := -1;
      fResultSet := fStatement.ExecuteQueryPrepared;
      if fResultSet=nil then begin
        // e.g. PRAGMA in TZSQLiteCAPIPreparedStatement.ExecuteQueryPrepared
        SynDBLog.Add.Log(sllWarning,'ZDBC.ExecutePrepared returned nil %',[fSQL],self);
      end else begin
        Props := fConnection.Properties as TSQLDBZEOSConnectionProperties;
        fResultInfo := fResultSet.GetMetadata;
        n := fResultInfo.GetColumnCount;
        fColumn.Capacity := n;
        for i := 0 to n-1 do begin
          name := fResultInfo.GetColumnLabel(i+FirstDbcIndex);
          if name='' then
            name := fResultInfo.GetColumnName(i+FirstDbcIndex);
          PSQLDBColumnProperty(fColumn.AddAndMakeUniqueName(
            // Delphi<2009: already UTF-8 encoded due to controls_cp=CP_UTF8
            {$ifdef UNICODE}StringToUTF8{$endif}(name)))^.ColumnType :=
              Props.TZSQLTypeToTSQLDBFieldType(fResultInfo.GetColumnType(i+FirstDbcIndex));
        end;
      end;
    end else
      fStatement.ExecuteUpdatePrepared; //ExecutePrepared allways trys to determine a possible LastResultSet
    // 3. handle out parameters
    // -> TODO (fStatement is IZCallableStatement)
    // EH: that will propably also be supported with the normal prepared stmts
    // on 7.3 (base implementation in classe is ready for most drivers 01.03.2019
    // but not complete!)
  {$ifdef ZEOS72UP}
  finally
    arrayBinding.Free;
  end;
  {$endif}
  SQLLogEnd;
end;

procedure TSQLDBZEOSStatement.Reset;
begin
  ReleaseRows;
  if fStatement<>nil then
    fStatement.ClearParameters;
  inherited Reset;
end;

procedure TSQLDBZEOSStatement.ReleaseRows;
begin
  if fResultSet<>nil then 
    fResultSet.ResetCursor;
  inherited ReleaseRows;
end;

function TSQLDBZEOSStatement.Step(SeekFirst: boolean): boolean;
begin
  if fColumnCount=0 then // no row returned
    result := false else
  if fResultSet=nil then
    raise ESQLDBZEOS.CreateUTF8('%.Step() invalid self',[self]) else
  if SeekFirst then begin
    result := fResultSet.First;
    if result then
      fCurrentRow := 1 else
      fCurrentRow := 0;
  end else begin
    result := fResultSet.Next;
    if result then
      inc(fCurrentRow);
  end;
end;

{$if defined(ZEOS73UP) and defined(USE_SYNCOMMONS)}
procedure TSQLDBZEOSStatement.AfterConstruction;
begin
  inherited;
  fJSONComposeOptions := [jcoEndJSONObject];
end;
{$ifend}

function TSQLDBZEOSStatement.ColumnBlob(Col: Integer): RawByteString;
var blob: IZBlob;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateUTF8('%.ColumnBlob(%) ResultSet=%',[self,Col,fResultSet]);
  blob := fResultSet.GetBlob(Col+FirstDbcIndex);
  if (blob=nil) or blob.IsEmpty then
    result := '' else
    result := blob.GetString; // ZAnsiString = RawByteString
end;

function TSQLDBZEOSStatement.ColumnCurrency(Col: Integer): currency;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateUTF8('%.ColumnCurrency(%) ResultSet=%',[self,Col,fResultSet]);
  {$ifdef ZEOS72UP}
  result := fResultSet.GetCurrency(Col+FirstDbcIndex);
  {$else}
  result := fResultSet.GetBigDecimal(Col+FirstDbcIndex);
  {$endif}
end;

function TSQLDBZEOSStatement.ColumnDateTime(Col: Integer): TDateTime;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateUTF8('%.ColumnDateTime(%) ResultSet=%',[self,Col,fResultSet]);
  result := fResultSet.GetTimestamp(Col+FirstDbcIndex);
end;

function TSQLDBZEOSStatement.ColumnDouble(Col: Integer): double;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateUTF8('%.ColumnDouble(%) ResultSet=%',[self,Col,fResultSet]);
  result := fResultSet.GetDouble(Col+FirstDbcIndex);
end;

function TSQLDBZEOSStatement.ColumnInt(Col: Integer): Int64;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateUTF8('%.ColumnInt(%) ResultSet=%',[self,Col,fResultSet]);
  result := fResultSet.GetLong(Col+FirstDbcIndex);
end;

function TSQLDBZEOSStatement.ColumnNull(Col: Integer): boolean;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateUTF8('%.ColumnNull(%) ResultSet=%',[self,Col,fResultSet]);
  result := fResultSet.IsNull(Col+FirstDbcIndex);
end;

function TSQLDBZEOSStatement.ColumnUTF8(Col: Integer): RawUTF8;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateUTF8('%.ColumnUTF8(%) ResultSet=%',[self,Col,fResultSet]);
  {$ifdef ZEOS72UP}
  result := fResultSet.GetUTF8String(Col+FirstDbcIndex);
  {$else}
    {$ifdef UNICODE}
    StringToUTF8(fResultSet.GetString(Col+FirstDbcIndex),result);
    {$else}
    result := fResultSet.GetString(Col+FirstDbcIndex); // thanks to controls_cp=CP_UTF8
    {$endif}
  {$endif}
end;

function TSQLDBZEOSStatement.UpdateCount: integer;
begin
  if fStatement<>nil then
    result := fStatement.GetUpdateCount else
    result := 0;
end;

{$ifdef ZEOS72UP}
procedure TSQLDBZEOSStatement.ColumnsToJSON(WR: TJSONWriter);
{$if not (defined(ZEOS73UP) and defined(USE_SYNCOMMONS))}
var col: integer;
    P: PAnsiChar;
    Len: NativeUInt; // required by Zeos for GetPAnsiChar out param (not PtrUInt)
procedure WriteIZBlob;
var blob: IZBlob;
    raw: RawByteString;
begin
  blob := fResultSet.GetBlob(col+FirstDbcIndex);
  raw := blob.GetString;
  WR.WrBase64(pointer(raw),length(raw),{withmagic=}true); // withMagic=true
end;
{$ifend}
begin // take care of the layout of internal ZDBC buffers for each provider
  {$if defined(ZEOS73UP) and defined(USE_SYNCOMMONS)}
  fResultSet.ColumnsToJSON(WR,fJSONComposeOptions);
  {$else}
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount-1 do begin
    if WR.Expand then
      WR.AddFieldName(fColumns[col].ColumnName); // add '"ColumnName":'
    if fResultSet.IsNull(col+FirstDbcIndex) then
      WR.AddShort('null') else begin
    case fColumns[col].ColumnType of
      ftNull:
        WR.AddShort('null');
      ftInt64:
        if fDBMS in [dMySQL,dPostgreSQL] then begin
          P := fResultSet.GetPAnsiChar(col+FirstDbcIndex,Len);
          WR.AddNoJSONEscape(P,Len);
        end else
          WR.Add(fResultSet.GetLong(col+FirstDbcIndex));
      ftDouble:
        if fDBMS in [dMySQL,dPostgreSQL] then begin
          P := fResultSet.GetPAnsiChar(col+FirstDbcIndex,Len);
          WR.AddNoJSONEscape(P,Len);
        end else
          WR.AddDouble(fResultSet.GetDouble(col+FirstDbcIndex));
      ftCurrency:
        if fDBMS = dSQLite then
          WR.AddDouble(fResultSet.GetDouble(col+FirstDbcIndex)) else
          WR.AddCurr64(fResultSet.GetCurrency(col+FirstDbcIndex));
      ftDate: begin
        WR.Add('"');
        WR.AddDateTime(fResultSet.GetTimestamp(col+FirstDbcIndex),fForceDateWithMS);
        WR.Add('"');
      end;
      ftUTF8: begin
        WR.Add('"');
        if fDBMS = dMSSQL then begin
          P := Pointer(fResultSet.GetPWideChar(Col+FirstDbcIndex, Len));
          WR.AddJSONEscapeW(Pointer(P), Len);
        end else begin
          P := fResultSet.GetPAnsiChar(col+FirstDbcIndex,Len);
          WR.AddJSONEscape(P,Len);
        end;
        WR.Add('"');
      end;
      ftBlob:
        if fForceBlobAsNull then
          WR.AddShort('null') else
        if fDBMS in [dMySQL,dSQLite] then begin
          P := fResultSet.GetPAnsiChar(col+FirstDbcIndex,Len);
          WR.WrBase64(p,Len,true); // withMagic=true
        end else
          WriteIZBlob;
      else raise ESQLDBException.CreateUTF8(
        '%.ColumnsToJSON: invalid ColumnType(#% "%")=%',
         [self,col,fColumns[col].ColumnName,ord(fColumns[col].ColumnType)]);
    end; end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
  {$ifend}
end;
{$endif ZEOS72UP}

initialization
  TSQLDBZEOSConnectionProperties.RegisterClassNameForDefinition;

{$endif NOSYNDBZEOS}
end.
