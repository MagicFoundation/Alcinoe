/// experimental Firebird 2.5+ direct access classes to be used with our SynDB architecture
// - not finished, nor working yet: we urge you to use SynDBZeos (or SynDBODBC) instead
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBFirebird;

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

  WARNING: This unit is NOT FINISHED and doesn't work as expected!
    PLEASE USE SynDBZeos instead!

  TODO:
  - share a global database connection for embedded mode, instead of
    setting ForceOnlyOneSharedConnection=true
}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif MSWINDOWS}
  SysUtils,
  {$ifndef DELPHI5OROLDER}
  Variants,
  {$endif}
  Classes,
  Contnrs,
  SynCommons,
  SynTable,
  SynLog,
  SynDB;


{ -------------- TSQLDBFirebird* classes and types implementing a Firebird connection  }

type
  /// generic Exception type, generated for Firebird connection
  EFirebirdException = class(ESQLDBException);

  /// implements properties shared by the Firebird library
  // - will first search for a "Firebird Embedded Server" library in the
  // executable folder or its 'Firebird\' sub-folder
  // - if not found, the Firebird client library will be searched in the path,
  // or, in default, from the Windows registry settings
  // - use TSQLDBFirebirdEmbeddedConnectionProperties or
  // TSQLDBFirebirdConnectionClientProperties to force either of the two modes
  // - we focus on Firebird 2.5.0 and up (released on October 2010), since it
  // was the first multi-thread-capable and thread-safe client implementation
  // - not finished, nor working yet: we urge you to use SynDBZeos (or SynDBODBC) instead
  TSQLDBFirebirdConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  protected
    fDefaultPageSize: Integer;
    /// opaque protected reference to the corresponding TFirebirdLib instance
    fFirebirdInstance: TObject;
    // this overridden method will force dFirebird kind of DBMS
    procedure SetInternalProperties; override;
  public
    /// initialize the connection properties
    // - will raise an exception if no Firebird library is available
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBFirebirdConnection instance
    function NewConnection: TSQLDBConnection; override;
    /// the page size on disk to be used when creating a new database file
    // - only used in embedded mode, if no database file exists
    // - default value is 4096, but you may set 8192 or 16384 for better performance
    property DefaultPageSize: Integer read fDefaultPageSize write fDefaultPageSize;
  end;

  /// implements properties shared by the Firebird embedded engine
  // - will search for a "Firebird Embedded Server" library in the executable
  // folder or its 'Firebird\' sub-folder
  // - if not found, will raise an EFirebirdException
  TSQLDBFirebirdEmbeddedConnectionProperties = class(TSQLDBFirebirdConnectionProperties)
  public
    /// initialize the embedded connection properties
    // - will raise an exception if no embedded Firebird library is available
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
  end;

  /// implements a direct connection to the Firebird client library
  // - the Firebird client libray will be searched in the path, or, in default,
  // from the Windows registry settings
  // - if not found, will raise an EFirebirdException
  TSQLDBFirebirdConnectionClientProperties = class(TSQLDBFirebirdConnectionProperties)
  public
    /// initialize the embedded connection properties
    // - will raise an exception if no embedded Firebird library is available
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
   end;

  TSQLDBFirebirdStatus = array[0..19] of PtrInt;

  /// implements a direct connection to the Firebird library
  TSQLDBFirebirdConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fFirebirdProperties: TSQLDBFirebirdConnectionProperties;
    fEmbedded: boolean;
    fBuf: PByteArray;
    fBufLen: integer;
    fStatus: TSQLDBFirebirdStatus;
    fDatabase: pointer;
    fTransaction: pointer;
    // increased on each StartTransaction call
    fTransactionID: cardinal;
    /// opaque protected reference to the corresponding TFirebirdLib instance
    fFirebirdInstance: TObject;
    procedure BufAddStr(dpb: byte; const Value: RawUTF8; DoNotStoreIfBlank: boolean=true);
    procedure BufAddI8(dpb: byte; Value: byte=0);
    procedure BufAddI32(dpb: byte; Value: cardinal);
    procedure BufClear;
  public
    /// connect to a specified Firebird database
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the Firebird library, i.e. create the DB instance
    // - should raise an Exception on error
    procedure Connect; override;
    /// stop connection to the Firebird library, i.e. release the DB instance
    // - should raise an Exception on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;

    /// initialize a new SQL query statement for the given connection
    // - the caller should free the instance after use
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    // - current implementation do not support nested transaction with those
    // methods: exception will be raised in such case
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
  published
    /// TRUE if the Embedded Firebird Library is used for this connection
    property Embedded: boolean read fEmbedded;
  end;

  TSQLDBFirebirdInternalStatement = record
    Transaction: pointer;
    TransactionID: cardinal;
    Statement: pointer;
  end;

  /// implements a statement using a Firebird connection
  TSQLDBFirebirdStatement = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fMain: TSQLDBFirebirdInternalStatement;
    fAutoCommit: TSQLDBFirebirdInternalStatement;
    fCurrent: ^TSQLDBFirebirdInternalStatement;
    fInput: TByteDynArray;
    fOutput: TByteDynArray;
    fStatus: TSQLDBFirebirdStatus;
    function ReleaseMainStatementIfAny: boolean;
  public
    {{ create a Firebird statement instance, from an existing Firebird connection
     - the Execute method can be called once per TSQLDBFirebirdStatement instance,
       but you can use the Prepare once followed by several ExecutePrepared methods
     - if the supplied connection is not of TOleDBConnection type, will raise
       an exception }
    constructor Create(aConnection: TSQLDBConnection); override;
    {{ release all associated memory and Firebird handles }
    destructor Destroy; override;

    {{ Prepare an UTF-8 encoded SQL statement
     - parameters marked as ? will be bound later, before ExecutePrepared call
     - if ExpectResults is TRUE, then Step() and Column*() methods are available
       to retrieve the data rows
     - raise an EFirebirdException or ESQLDBException on any error }
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: Boolean=false); overload; override;
    {{ Execute a prepared SQL statement
     - parameters marked as ? should have been already bound with Bind*() functions
     - raise an EFirebirdException or ESQLDBException on any error }
    procedure ExecutePrepared; override;
    {/ Reset the previous prepared statement
     - this overridden implementation will reset all bindings and the cursor state
     - raise an EFirebirdException on any error }
    procedure Reset; override;

    {/ After a statement has been prepared via Prepare() + ExecutePrepared() or
       Execute(), this method must be called one or more times to evaluate it
     - you shall call this method before calling any Column*() methods
     - return TRUE on success, with data ready to be retrieved by Column*()
     - return FALSE if no more row is available (e.g. if the SQL statement
      is not a SELECT but an UPDATE or INSERT command)
     - access the first or next row of data from the SQL Statement result:
       if SeekFirst is TRUE, will put the cursor on the first row of results,
       otherwise, it will fetch one row of data, to be called within a loop
     - raise an EFirebirdException or ESQLDBException exception on any error }
    function Step(SeekFirst: boolean=false): boolean; override;
    {{ returns TRUE if the column contains NULL }
    function ColumnNull(Col: integer): boolean; override;
    {{ return a Column integer value of the current Row, first Col is 0 }
    function ColumnInt(Col: integer): Int64; override;
    {{ return a Column floating point value of the current Row, first Col is 0 }
    function ColumnDouble(Col: integer): double; override;
    {{ return a Column floating point value of the current Row, first Col is 0 }
    function ColumnDateTime(Col: integer): TDateTime; override;
    {{ return a Column currency value of the current Row, first Col is 0
     - should retrieve directly the 64 bit Currency content, to avoid
     any rounding/conversion error from floating-point types }
    function ColumnCurrency(Col: integer): currency; override;
    {{ return a Column UTF-8 encoded text value of the current Row, first Col is 0 }
    function ColumnUTF8(Col: integer): RawUTF8; override;
    {{ return a Column as a blob value of the current Row, first Col is 0
    - ColumnBlob() will return the binary content of the field is was not ftBlob,
      e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
      or a direct mapping of the RawUnicode  }
    function ColumnBlob(Col: integer): RawByteString; override;
    {{ append all columns values of the current Row to a JSON stream
     - will use WR.Expand to guess the expected output format
     - fast overridden implementation with no temporary variable
     - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
       format and contains true BLOB data }
    procedure ColumnsToJSON(WR: TJSONWriter); override;
    /// returns the number of rows updated by the execution of this statement
    function UpdateCount: integer; override;
  end;

const
  FBLIBNAME: array[boolean] of TFileName =
  {$ifdef MSWINDOWS}
    ('gds32.dll','fbembed.dll');
  {$endif}
  {$ifdef LINUX}
    ('libfbclient.so','libfbembed.so');
  {$endif}

/// check from the file beginning if sounds like a valid Firebird database file
// - no official signature is available, so it will be a "best-guess"
function IsFirebirdFile(const FileName: TFileName): boolean;


implementation


function IsFirebirdFile(const FileName: TFileName): boolean;
var F: THandle;
    Header: packed record
      pag_type, pag_flags: byte;
      pag_checksum: word;
      pag_generation, pag_scn, reserved: cardinal;
      hdr_page_size, hdr_ods_version: word;
    end; // cf. http://www.firebirdsql.org/manual/fbint-standard-header.html
begin
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if F<=0 then
    result := false else begin
    result := (FileRead(F,Header,sizeof(Header))=SizeOf(Header)) and
      (Header.pag_type=1) and (Header.pag_checksum=12345) and
      (Header.hdr_page_size and $3ff=0) and (Header.hdr_ods_version and $8000<>0);
    FileClose(F);
  end;
end;


{ -------------- Firebird library interfaces, constants and types }


const
  METADATALENGTH = 68;
  MAX_DPB_SIZE = 1024;

  { supports SQL delimited identifier, SQLDATE/DATE, TIME,
    TIMESTAMP, CURRENT_DATE, CURRENT_TIME, CURRENT_TIMESTAMP, and
    64-bit exact numeric type }
  SQL_DIALECT = 3;

  // fits XSQLDA/XSQLVAR layout
  SQLDA_CURRENT_VERSION = 1;

  DSQL_close = 1;
  DSQL_drop = 2;

type
  PIscTrHandle = PPointer;

  ISCStatus = PtrInt;
  PISCStatus = ^ISCStatus;
  PPISCStatus = ^PISCStatus;

  PGDSQuad = ^GDS_Quad;
  PISCQuad = ^GDS_Quad;
  GDS_QUAD = record
    gds_quad_high: integer;
    gds_quad_low: cardinal;
  end;

  PXSQLVar = ^XSQLVar;
  XSQLVAR = object
    // datatype of field
    sqltype: Smallint;
    // scale factor
    sqlscale: Smallint;
    // datatype subtype
    sqlsubtype: Smallint;
    // length of data area
    sqllen: Smallint;
    // address of data
    sqldata: PAnsiChar;
    // address of indicator variable
    sqlind: PSmallintArray;
    // length of sqlname field
    sqlname_length: Smallint;
    // name of field, name length + space for #0
    sqlname: array[0..METADATALENGTH-1] of AnsiChar;
    // length of relation name
    relname_length: Smallint;
    // field's relation name + space for #0
    relname: array[0..METADATALENGTH-1] of AnsiChar;
    // length of owner name
    ownname_length: Smallint;
    // relation's owner name + space for #0
    ownname: array[0..METADATALENGTH-1] of AnsiChar;
    // length of alias name
    aliasname_length: Smallint;
    // relation's alias name + space for #0
    aliasname: array[0..METADATALENGTH-1] of AnsiChar;
    // wrapper to return the column name from sqlname[] content
    function ColumnName: RawUTF8;
  end;

  PXSQLDA = ^XSQLDA;
  XSQLDA = record
    // version of this XSQLDA = SQLDA_CURRENT_VERSION = 1
    version: Smallint;
    // XSQLDA name field   ->  RESERVED
    sqldaid: array[0..7] of AnsiChar;
    // length in bytes of SQLDA   ->  RESERVED
    sqldabc: Integer;
    // number of fields allocated
    sqln: Smallint;
    // actual number of fields
    sqld: Smallint;
    // fields content
    sqlvar: array[0..0] of XSQLVar;
  end;

  // used by isc_start_multiple()
  PISCTEB = ^TISCTEB;
  TISCTEB = record
    Handle: pointer;
    Len: Integer;
    Address: PAnsiChar;
  end;

  // Database parameter block stuff
  TFirebirdDatabaseParameterBlock = (dpb_cdd_pathname=1,
    dpb_allocation, dpb_journal, dpb_page_size, dpb_num_buffers,
    dpb_buffer_length, dpb_debug, dpb_garbage_collect, dpb_verify, dpb_sweep,
    dpb_enable_journal, dpb_disable_journal, dpb_dbkey_scope, dpb_number_of_users,
    dpb_trace, dpb_no_garbage_collect, dpb_damaged, dpb_license, dpb_sys_user_name,
    dpb_encrypt_key, dpb_activate_shadow, dpb_sweep_interval, dpb_delete_shadow,
    dpb_force_write, dpb_begin_log, dpb_quit_log, dpb_no_reserve, dpb_user_name,
    dpb_password, dpb_password_enc, dpb_sys_user_name_enc, dpb_interp, dpb_online_dump,
    dpb_old_file_size, dpb_old_num_files, dpb_old_file, dpb_old_start_page,
    dpb_old_start_seqno, dpb_old_start_file, dpb_drop_walfile, dpb_old_dump_id,
    dpb_wal_backup_dir, dpb_wal_chkptlen, dpb_wal_numbufs, dpb_wal_bufsize,
    dpb_wal_grp_cmt_wait, dpb_lc_messages, dpb_lc_ctype, dpb_cache_manager,
    dpb_shutdown, dpb_online, dpb_shutdown_delay, dpb_reserved,
    dpb_overwrite, dpb_sec_attach, dpb_disable_wal, dpb_connect_timeout,
    dpb_dummy_packet_interval, dpb_gbak_attach, dpb_sql_role_name,
    dpb_set_page_buffers, dpb_working_directory, dpb_sql_dialect,
    dpb_set_db_readonly, dpb_set_db_sql_dialect, dpb_gfix_attach, dpb_gstat_attach);


  /// direct access to the Firebird library
  TFirebirdLib = class(TSQLDBLib)
  protected
    fEmbedded: boolean;
  public
    isc_attach_database: function(var user_status: TSQLDBFirebirdStatus; file_length: Smallint;
      file_name: PAnsiChar; var handle: pointer; dpb_length: Smallint; dpb: PByteArray): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_detach_database: function(var user_status: TSQLDBFirebirdStatus; var handle: pointer): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_create_database: function(var user_status: TSQLDBFirebirdStatus; file_length: Smallint;
      file_name: PAnsiChar; var handle: pointer; dpb_length: Smallint; dpb: PAnsiChar;
      db_type: Smallint): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_sqlcode: function (var user_status: TSQLDBFirebirdStatus): integer;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_sql_interprete: procedure(sqlcode: SmallInt; buf: PAnsiChar; buflen: SmallInt);
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    fb_interpret: function(buf: PAnsiChar; bufsize: integer; status_vector: PPISCStatus): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_dsql_execute_immediate: function(var user_status: TSQLDBFirebirdStatus; var db_handle: pointer;
      tra_handle: PIscTrHandle; buflen: Word; buf: PAnsiChar; dialect: Word=SQL_DIALECT;
      sqlda: PXSQLDA=nil): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_start_multiple: function(var user_status: TSQLDBFirebirdStatus; var tra_handle: pointer;
      count: Smallint; vector: PISCTEB): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_rollback_transaction: function(var user_status: TSQLDBFirebirdStatus; var tra_handle: pointer): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_commit_transaction: function(var user_status: TSQLDBFirebirdStatus; var tra_handle: pointer): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_commit_retaining: function(var user_status: TSQLDBFirebirdStatus; var tra_handle: pointer): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_dsql_allocate_statement: function(var user_status: TSQLDBFirebirdStatus; var db_handle: pointer;
      var stmt_handle: pointer): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_dsql_free_statement: function(var user_status: TSQLDBFirebirdStatus; var stmt_handle: pointer;
      option: Word): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_dsql_execute: function(var user_status: TSQLDBFirebirdStatus; var tra_handle: pointer;
      var stmt_handle: pointer; dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_dsql_describe: function(var user_status: TSQLDBFirebirdStatus; var stmt_handle: pointer;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_dsql_prepare: function(var user_status: TSQLDBFirebirdStatus; var tra_handle: pointer;
      var stmt_handle: pointer; buflen: Word; buf: PAnsiChar; dialect: Word;
      sqlda: PXSQLDA): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_dsql_sql_info: function(var user_status: TSQLDBFirebirdStatus; var stmt_handle: pointer;
      itemlsength: Smallint; items: PAnsiChar; buflen: Smallint; buf: PAnsiChar): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_create_blob2: function(var user_status: TSQLDBFirebirdStatus; var db_handle: pointer;
      var tra_handle: pointer; var blob_handle: pointer; blob_id: PISCQuad;
      bpb_length: Smallint; bpb: PAnsiChar): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_put_segment: function(var user_status: TSQLDBFirebirdStatus; var blob_handle: pointer;
      buffer_length: Word; buffer: PAnsiChar): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_close_blob: function(var user_status: TSQLDBFirebirdStatus; var blob_handle: pointer): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
    isc_dsql_set_cursor_name: function(var user_status: TSQLDBFirebirdStatus; var stmt_handle: pointer;
      cursorname: PAnsiChar; type_: Word=0): ISCStatus;
      {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
  public
    /// load the Firebird library
    // - and retrieve all SQL*() addresses for FIREBIRD_ENTRIES[] items
    constructor Create(aEmbedded: boolean);
    /// raise an EFirebirdException on error
    procedure Check(Status: ISCStatus; var PrivateStatus: TSQLDBFirebirdStatus);
    /// wrapper around isc_start_multiple() function
    procedure StartTransaction(var PrivateStatus: TSQLDBFirebirdStatus;
      var Database, Transaction: pointer);
    /// if the library is the "Firebird Embedded Server" version
    property Embedded: boolean read fEmbedded;
  end;

const
  isc_dpb_version1 = 1;

  FIREBIRD_ENTRIES: array[0..20] of PChar =
    ('isc_attach_database','isc_detach_database','isc_create_database',
     'isc_sqlcode','isc_sql_interprete','fb_interpret',
     'isc_dsql_execute_immediate',
     'isc_start_multiple','isc_rollback_transaction','isc_commit_transaction',
     'isc_commit_retaining',
     'isc_dsql_allocate_statement','isc_dsql_free_statement',
     'isc_dsql_execute','isc_dsql_describe','isc_dsql_prepare',
     'isc_dsql_sql_info','isc_create_blob2','isc_put_segment','isc_close_blob',
     'isc_dsql_set_cursor_name');


{ TSQLDBFirebirdConnection }

procedure TSQLDBFirebirdConnection.Connect;
var Log: ISynLog;
    ServerName, SQL: RawByteString;
begin
  Log := SynDBLog.Enter;
  Disconnect;
  if fProperties.ServerName='' then
    raise EFirebirdException.Create('ServerName missing');
  ServerName := CurrentAnsiConvert.UTF8ToAnsi(fProperties.ServerName);
  with TFirebirdLib(fFirebirdInstance) do begin
    if fEmbedded and not FileExists(
      {$ifdef UNICODE}UTF8ToString(fProperties.ServerName){$else}ServerName{$endif}) then begin
      SQL := fProperties.SQLCreateDatabase(fProperties.ServerName,
        TSQLDBFirebirdConnectionProperties(fProperties).DefaultPageSize);
      Log.Log(sllDB,'No DB file found -> % using %', [SQL,LibraryPath],fProperties);
      Check(isc_dsql_execute_immediate(fStatus,fDatabase,nil,0,pointer(SQL)),fStatus);
      exit;
    end;
    Log.Log(sllDB,'Connect to "%" as "%" using %',
      [fProperties.ServerName,fProperties.UserID,LibraryPath],fProperties);
    BufClear;
    BufAddStr(ord(dpb_user_name),fProperties.UserID);
    BufAddStr(ord(dpb_password),fProperties.PassWord);
    Check(isc_attach_database(fStatus,length(ServerName),pointer(ServerName),
      fDatabase,fBufLen,fBuf),fStatus);
    inherited Connect; // notify any re-connection
  end;
end;

constructor TSQLDBFirebirdConnection.Create(aProperties: TSQLDBConnectionProperties);
begin
  if not aProperties.InheritsFrom(TSQLDBFirebirdConnectionProperties) then
    raise EFirebirdException.Create('Invalid TSQLDBFirebirdConnection.Create');
  fFirebirdProperties := TSQLDBFirebirdConnectionProperties(aProperties);
  fFirebirdInstance := fFirebirdProperties.fFirebirdInstance;
  fEmbedded := TFirebirdLib(fFirebirdProperties.fFirebirdInstance).Embedded;
  GetMem(fBuf,MAX_DPB_SIZE);
  inherited;
end;

destructor TSQLDBFirebirdConnection.Destroy;
begin
  inherited Destroy;
  FreeMem(fBuf);
end;

procedure TSQLDBFirebirdConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if fFirebirdInstance=nil then
      raise EFirebirdException.Create('No lib');
    if fDatabase<>nil then
      with TFirebirdLib(fFirebirdInstance) do begin
        if TransactionCount>0 then
          Rollback;
        Check(isc_detach_database(fStatus,fDatabase),fStatus);
      end;
  end;
end;

function TSQLDBFirebirdConnection.IsConnected: boolean;
begin
  result := fDatabase<>nil;
end;

function TSQLDBFirebirdConnection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBFirebirdStatement.Create(self);
end;

procedure TSQLDBFirebirdConnection.Commit;
begin
  inherited Commit;
  if fTransaction=nil then
    raise EFirebirdException.Create('Commit');
  with TFirebirdLib(fFirebirdInstance) do
  try
    Check(isc_commit_transaction(fStatus,fTransaction),fStatus);
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSQLDBFirebirdConnection.Rollback;
begin
  inherited RollBack;
  if fTransaction=nil then
    raise EFirebirdException.Create('Rollback');
  with TFirebirdLib(fFirebirdInstance) do
    Check(isc_rollback_transaction(fStatus,fTransaction),fStatus);
end;

procedure TSQLDBFirebirdConnection.StartTransaction;
begin
  if TransactionCount>0 then
    raise EFirebirdException.Create('TSQLDBFirebirdConnection do not provide nested transactions');
  inherited StartTransaction;
  if fTransaction<>nil then
    raise EFirebirdException.Create('StartTransaction');
  inc(fTransactionID);
  TFirebirdLib(fFirebirdInstance).StartTransaction(fStatus,fDatabase,fTransaction);
end;

procedure TSQLDBFirebirdConnection.BufClear;
begin
  fBuf[0] := isc_dpb_version1;
  fBufLen := 1;
end;

procedure TSQLDBFirebirdConnection.BufAddStr(dpb: byte; const Value: RawUTF8;
  DoNotStoreIfBlank: boolean);
var L: integer;
begin
  L := length(Value);
  if (L=0) and DoNotStoreIfBlank then
    exit;
  if L>255 then
    raise EFirebirdException.Create('BufAddStr size>255');
  if fBufLen+L>=MAX_DPB_SIZE-5 then
    raise EFirebirdException.Create('BufAdd overflow');
  fBuf[fBufLen] := dpb;
  inc(fBufLen);
  fBuf[fBufLen] := L;
  inc(fBufLen);
  move(pointer(Value)^,fBuf[fBufLen],L);
  inc(fBufLen,L);
end;

procedure TSQLDBFirebirdConnection.BufAddI32(dpb: byte; Value: cardinal);
begin
  fBuf[fBufLen] := dpb;
  inc(fBufLen);
  case Value of
  0..255: begin
    fBuf[fBufLen] := 1;
    inc(fBufLen);
    fBuf[fBufLen] := Value;
    inc(fBufLen);
  end;
  256..65535: begin
    fBuf[fBufLen] := 2;
    inc(fBufLen);
    PWord(@fBuf[fBufLen])^ := Value;
    inc(fBufLen,2);
  end;
  else begin
    fBuf[fBufLen] := 4;
    inc(fBufLen);
    PCardinal(@fBuf[fBufLen])^ := Value;
    inc(fBufLen,4);
  end;
  end;
end;

procedure TSQLDBFirebirdConnection.BufAddI8(dpb, Value: byte);
begin
  fBuf[fBufLen] := dpb;
  inc(fBufLen);
  fBuf[fBufLen] := Value;
  inc(fBufLen);
end;


{ TSQLDBFirebirdStatement }

function TSQLDBFirebirdStatement.ColumnBlob(Col: integer): RawByteString;
begin
end;

function TSQLDBFirebirdStatement.ColumnUTF8(Col: integer): RawUTF8;
begin
end;

function TSQLDBFirebirdStatement.ColumnCurrency(Col: integer): currency;
begin
end;

function TSQLDBFirebirdStatement.ColumnDateTime(Col: integer): TDateTime;
begin
end;

function TSQLDBFirebirdStatement.ColumnDouble(Col: integer): double;
begin
end;

function TSQLDBFirebirdStatement.ColumnInt(Col: integer): Int64;
begin
end;

function TSQLDBFirebirdStatement.ColumnNull(Col: integer): boolean;
begin
end;

procedure TSQLDBFirebirdStatement.ColumnsToJSON(WR: TJSONWriter);
var col, METHODTOBEWRITTEN: integer;
begin
  if (CurrentRow<=0) then
    raise EFirebirdException.Create('TSQLDBFirebirdStatement.ColumnsToJSON() with no prior Step');
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount-1 do // fast direct conversion from OleDB buffer
  with fColumns[col] do begin
    if WR.Expand then
      WR.AddFieldName(ColumnName); // add '"ColumnName":'
    // ....
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

constructor TSQLDBFirebirdStatement.Create(aConnection: TSQLDBConnection);
begin
  if not aConnection.InheritsFrom(TSQLDBFirebirdConnection) then
    raise EFirebirdException.CreateFmt('%s.Create expects a TSQLDBFirebirdConnection',[ClassName]);
  inherited Create(aConnection);
end;

destructor TSQLDBFirebirdStatement.Destroy;
begin
  try
    with TFirebirdLib(TSQLDBFirebirdConnection(fConnection).fFirebirdInstance) do begin
      if fMain.Statement<>nil then
        Check(isc_dsql_free_statement(fStatus,fMain.Statement,DSQL_drop),fStatus);
      if fAutoCommit.Statement<>nil then
        Check(isc_dsql_free_statement(fStatus,fAutoCommit.Statement,DSQL_drop),fStatus);
      if fAutoCommit.Transaction<>nil then
        Check(isc_rollback_transaction(fStatus,fAutoCommit.Transaction),fStatus);
    end;
  finally
    inherited Destroy;
  end;
end;

function TSQLDBFirebirdStatement.UpdateCount: integer;
begin
end;

procedure TSQLDBFirebirdStatement.Prepare(const aSQL: RawUTF8; ExpectResults: Boolean);
var Log: ISynLog;
    Conn: TSQLDBFirebirdConnection;
begin
  Log := SynDBLog.Enter(self, 'Prepare');
  if (fColumnCount>0) or (fAutoCommit.Statement<>nil) or (fCurrent<>nil) then
    raise EFirebirdException.CreateFmt('%s.Prepare should be called only once',[ClassName]);
  // 1. process SQL
  inherited Prepare(aSQL,ExpectResults); // set fSQL + Connect if necessary
  // 2. prepare statement and bind result columns (if any)
  Conn := TSQLDBFirebirdConnection(fConnection);
  with TFirebirdLib(Conn.fFirebirdInstance)  do
  try
    // always prepare auto-commit transaction and associated statement
    StartTransaction(fStatus,Conn.fDatabase,fAutoCommit.Transaction);
    Check(isc_dsql_allocate_statement(fStatus,Conn.fDatabase,fAutoCommit.Statement),fStatus);
    Check(isc_dsql_prepare(fStatus,fAutoCommit.Transaction,fAutoCommit.Statement,length(fSQL),
      pointer(fSQL),SQL_DIALECT,nil),fStatus);
    // execution within a main transaction will be prepared on purpose
  except
    on E: Exception do begin
      Log.Log(sllError,E);
      raise;
    end;
  end;
end;

function TSQLDBFirebirdStatement.ReleaseMainStatementIfAny: boolean;
var MainConn: TSQLDBFirebirdConnection;
begin
  if (fAutoCommit.Statement=nil) or (fAutoCommit.Transaction=nil) then
    raise EFirebirdException.Create('Prepare should be called before ExecutePrepared/Reset');
  MainConn := TSQLDBFirebirdConnection(fConnection);
  with TFirebirdLib(MainConn.fFirebirdInstance)  do
    if (fMain.Transaction<>nil) and (MainConn.fTransactionID<>fMain.TransactionID) then
    try
      Check(isc_dsql_free_statement(fStatus,fMain.Statement,DSQL_drop),fStatus);
      Check(isc_rollback_transaction(fStatus,fMain.Transaction),fStatus);
      result := true;
    finally  // force nil: connection may be lost
      fMain.Statement := nil;
      fMain.Transaction := nil;
    end else
    result := false;
end;

// allocate a TByteDynArray to contain
function XSQLDAAllocate(var Buf: TByteDynArray; number: integer): PXSQLDA;
begin
  SetLength(Buf,sizeof(XSQLDA)+(number-1)*sizeof(XSQLVar));
  result := pointer(Buf);
  result.version := SQLDA_CURRENT_VERSION;
  result.sqln := number;
  result.sqld := number;
end;

procedure TSQLDBFirebirdStatement.ExecutePrepared;
var MainConn: TSQLDBFirebirdConnection;
    i: integer;
begin
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  MainConn := TSQLDBFirebirdConnection(fConnection);
  with TFirebirdLib(MainConn.fFirebirdInstance) do begin
    // 1. release any deprecated main transaction/statement
    ReleaseMainStatementIfAny;
    // 2. decide which transaction/statement is to be used
    if MainConn.fTransaction<>nil then begin
      // main transaction is active -> prepare a statement for it (if needed)
      if fMain.Transaction=nil then begin
        fMain.Transaction := MainConn.fTransaction;
        fMain.TransactionID := MainConn.fTransactionID;
        assert(fMain.Statement=nil);
        Check(isc_dsql_allocate_statement(fStatus,MainConn.fDatabase,fMain.Statement),fStatus);
        Check(isc_dsql_prepare(fStatus,fMain.Transaction,fMain.Statement,length(fSQL),
          pointer(fSQL),SQL_DIALECT,nil),fStatus);
      end;
      fCurrent := @fMain;
    end else
      // we are in auto-commit mode
      fCurrent := @fAutoCommit;
    // 3. bind parameters

    // 4. execute the statement
    if fExpectResults then
      // (re)open result cursor for queries
      Check(isc_dsql_set_cursor_name(fStatus,fCurrent.Statement,'SynDB'),fStatus);
    Check(isc_dsql_execute(fStatus,fCurrent.Transaction,fCurrent.Statement,SQL_DIALECT,pointer(fInput)),fStatus);
    if (fCurrent=@fAutoCommit) and not fExpectResults then
      // emulate AUTOCOMMIT mode
      Check(isc_commit_retaining(fStatus,fAutoCommit.Transaction),fStatus);
    // 5. prepare results
    if fOutput=nil then begin // allocate fOutput buffer (reuse the same)
      Check(isc_dsql_describe(fStatus,fCurrent.Statement,SQL_DIALECT,XSQLDAAllocate(fOutput,32)),fStatus);
      with PXSQLDA(fOutput)^ do
      if sqld>sqln then
        Check(isc_dsql_describe(fStatus,fCurrent.Statement,SQL_DIALECT,XSQLDAAllocate(fOutput,sqld)),fStatus);
      with PXSQLDA(fOutput)^ do begin
        for i := 0 to sqld-1 do
        with PSQLDBColumnProperty(fColumn.AddAndMakeUniqueName(sqlvar[i].ColumnName))^ do begin
          ColumnValueDBType := sqlvar[i].sqltype;
        end;
      end;
    end;
  end;
end;

function TSQLDBFirebirdStatement.Step(SeekFirst: boolean): boolean;
begin
  result := false;
end;

procedure TSQLDBFirebirdStatement.Reset;
begin
  ReleaseMainStatementIfAny; // global transaction context may have changed
  with TFirebirdLib(TSQLDBFirebirdConnection(fConnection).fFirebirdInstance) do
    if fExpectResults and (fCurrent<>nil) then
        // release opened cursor for queries
        Check(isc_dsql_free_statement(fStatus,fCurrent.Statement,DSQL_close),fStatus);
  inherited Reset;
end;



{ TFirebirdLib }

procedure TFirebirdLib.Check(Status: ISCStatus; var PrivateStatus: TSQLDBFirebirdStatus);

  procedure RaiseEFirebirdException;
  var Code: integer;
      sql,tmp: array[byte] of AnsiChar;
      Vec: pointer;
      msg: string;
  begin
    Code := isc_sqlcode(PrivateStatus);
    isc_sql_interprete(Code,sql,sizeof(sql));
    Vec := @PrivateStatus;
    while fb_interpret(tmp,sizeof(tmp),@Vec)>0 do begin
      msg := msg+' '+SysUtils.Trim(string(tmp));
      if msg[length(msg)]<>'.' then
        msg := msg+'.';
    end;
    raise EFirebirdException.CreateFmt('Firebird Error %x:%s [ SQLCODE=%d (%s) ]',
      [Status,msg,Code,sql]);
  end;

const
  CLASS_MASK = $F0000000; // Defines the code as warning, error, info, or other
  CLASS_ERROR   = 0; // Code represents an error
  CLASS_WARNING = 1; // Code represents a warning
  CLASS_INFO    = 2; // Code represents an information msg
begin
  if (Status<>0) and ((Status and CLASS_MASK) shr 30=CLASS_ERROR) then
    RaiseEFirebirdException;
end;

constructor TFirebirdLib.Create(aEmbedded: boolean);
var P: PPointer;
    i: integer;
    {$ifdef MSWINDOWS}
    Key: HKEY;
    Size: Cardinal;
    HR: Integer;
    {$endif}
    l1, l2: TFileName;
begin
  fEmbedded := aEmbedded;
  if aEmbedded then begin
    l1 := ExeVersion.ProgramFilePath;
    if FileExists(l1+FBLIBNAME[true]) then
      l1 := l1+FBLIBNAME[true] else
      l1 := l1+'Firebird\'+FBLIBNAME[true];
  end else begin
    l1 := FBLIBNAME[false];
    {$ifdef MSWINDOWS}  // also search from registry
    HR := RegOpenKeyEx(HKEY_LOCAL_MACHINE,
      'SOFTWARE\Firebird Project\Firebird Server\Instances',0,KEY_READ,Key);
    if HR=ERROR_SUCCESS then begin
      HR := RegQueryValueEx(Key,'DefaultInstance',nil,nil,nil,@Size);
      if HR=ERROR_SUCCESS then begin
        SetLength(l2,Size div SizeOf(Char));
        HR := RegQueryValueEx(Key,'DefaultInstance',nil,nil,pointer(l2),@Size);
        if HR=ERROR_SUCCESS then begin
          l2 := IncludeTrailingPathDelimiter(SysUtils.Trim(l2));
          if FileExists(l2+FBLIBNAME[false]) then
            l2 := l2+FBLIBNAME[false] else
            l2 := l2+('bin\'+FBLIBNAME[false]);
        end;
      end;
      RegCloseKey(Key);
    end;
    {$endif}
  end;
  TryLoadLibrary([l1, l2],EFirebirdException);
  SynDBLog.Add.Log(sllDebug,'Loading %',StringToUTF8(LibraryPath));
  P := @@isc_attach_database;
  for i := 0 to High(FIREBIRD_ENTRIES) do begin
    P^ := GetProcAddress(fHandle,FIREBIRD_ENTRIES[i]);
    if P^=nil then begin
      FreeLibrary(fHandle);
      fHandle := 0;
      raise EFirebirdException.CreateFmt('Invalid %s: missing %s',
        [LibraryPath,Firebird_ENTRIES[i]]);
    end;
    inc(P);
  end;
end;

procedure TFirebirdLib.StartTransaction(var PrivateStatus: TSQLDBFirebirdStatus;
  var Database, Transaction: pointer);
var Vector: TISCTEB;
begin
  Vector.Handle  := @Database;
  Vector.Len     := 0;
  Vector.Address := nil;
  Check(isc_start_multiple(PrivateStatus,Transaction,1,@Vector),PrivateStatus);
end;


{ TSQLDBFirebirdConnectionProperties }

var
  // define two GlobalFirebird[Embedded] libraries instances
  GlobalFirebird: array[boolean] of TFirebirdLib;

function Firebird(Embedded: boolean): TFirebirdLib;
begin
  if GlobalFirebird[Embedded]=nil then
  try
    result := TFirebirdLib.Create(Embedded);
    GarbageCollector.Add(result);
    GlobalFirebird[Embedded] := result;
  except
    on E: EFirebirdException do begin
      GlobalFirebird[Embedded] := pointer(-1);
      result := nil;
    end;
  end else
    if GlobalFirebird[Embedded]=pointer(-1) then
      result := nil else
      result := GlobalFirebird[Embedded];
end;


constructor TSQLDBFirebirdConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  fDefaultPageSize := 4096;
  if fFirebirdInstance=nil then
    fFirebirdInstance := Firebird(true);  // first check embedded
  if fFirebirdInstance=nil then
    fFirebirdInstance := Firebird(false); // then client
  if fFirebirdInstance=nil then
    raise EFirebirdException.CreateFmt('%s.Create: no library',[ClassName]);
  inherited;
end;

function TSQLDBFirebirdConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBFirebirdConnection.Create(self);
end;

procedure TSQLDBFirebirdConnectionProperties.SetInternalProperties;
begin
  if TFirebirdLib(fFirebirdInstance).Embedded then
    // disable multi-thread if embedded (avoid stability/performance issues)
    ThreadingMode := tmMainConnection;
  fDBMS := dFirebird;
end;

{ TSQLDBFirebirdEmbeddedConnectionProperties }

constructor TSQLDBFirebirdEmbeddedConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  if fFirebirdInstance=nil then
    fFirebirdInstance := Firebird(true);
  if fFirebirdInstance=nil then
    raise EFirebirdException.CreateFmt('%s.Create: no embedded library',[ClassName]);
  inherited;
end;

{ TSQLDBFirebirdConnectionClientProperties }

constructor TSQLDBFirebirdConnectionClientProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  if fFirebirdInstance=nil then
    fFirebirdInstance := Firebird(false);
  if fFirebirdInstance=nil then
    raise EFirebirdException.CreateFmt('%s.Create: no client library',[ClassName]);
  inherited;
end;


{ XSQLVAR }

function XSQLVAR.ColumnName: RawUTF8;
begin
  SetString(result,sqlname,sqlname_length);
end;

initialization
  assert(ord(dpb_gstat_attach)=67);
  TSQLDBFirebirdConnectionProperties.RegisterClassNameForDefinition;
end.
