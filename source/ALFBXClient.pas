{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALFBXClient (Alcinoe FireBird Express Client)
Version:      3.52

Description:  Retrieving Data as XML from Firebird Server.

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

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

Link :        http://www.progdigy.com/modules.php?name=UIB

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFBXClient;

interface

uses Windows,
     classes,
     SysUtils,
     Contnrs,
     SyncObjs,
     AlXmlDoc,
     ALFBXLib,
     ALFBXBase,
     AlFcnMisc;

Type

  {--------------------------------}
  TALFBXClientSelectDataSQL = record
    SQL: String;
    RowTag: String;
    ViewTag: String;
    Skip: integer;
    First: Integer;
  end;
  TALFBXClientSelectDataSQLs = array of TALFBXClientSelectDataSQL;

  {--------------------------------}
  TALFBXClientUpdateDataSQL = record
    SQL: String;
    Blobs: array of Tstream;
  end;
  TALFBXClientUpdateDataSQLs = array of TALFBXClientUpdateDataSQL;

  {---------------------------}
  TALFBXClient = Class(Tobject)
  Private
    FSQLDIALECT: word;
    FownLibrary: Boolean;
    FLibrary: TALFBXLibrary;
    fDBHandle: IscDbHandle;
    fTraHandle: IscTrHandle;
    fNullString: String;
    fCharSet: TALFBXCharacterSet;
    function  GetConnected: Boolean;
    function  GetInTransaction: Boolean;
  Protected
    function GetFieldValue(aSQLDA:TALFBXSQLResult;
                           aDBHandle: IscDbHandle;
                           aTraHandle: IscTrHandle;
                           aIndex: Integer;
                           aFormatSettings: TformatSettings): String;
    procedure GetTransactionStats(Var idx_reads: LongInt;
                                  Var seq_reads: LongInt);
    procedure doSQLDone(SQL: String;
                        XmlData: TalXmlNode;
                        StartDate, EndDate: Int64); virtual;
    procedure initObject; virtual;
  Public
    Constructor Create(ApiVer: TALFBXVersion_API; const lib: String = GDS32DLL); overload; virtual;
    Constructor Create(lib: TALFBXLibrary); overload; virtual;
    Destructor Destroy; Override;
    function  GetDataBaseInfoInt(const item: Integer): Integer;
    function  GetDataBaseInfoString(const item: Integer): string;
    function  GetDataBaseInfoDateTime(const item: Integer): TDateTime;
    procedure GetUserNames(UserNames: Tstrings);
    procedure CreateDatabase(SQL: String);
    procedure DropDatabase;
    Procedure Connect(DataBaseName,
                      Login,
                      Password,
                      CharSet: String;
                      const ExtraParams: String = ''); overload;
    Procedure Connect(DataBaseName,
                      Login,
                      Password,
                      CharSet: String;
                      Numbuffers: integer); overload;
    Procedure Disconnect;
    Procedure TransactionStart(Readonly: Boolean; const TPB: String = '');
    Procedure TransactionCommit;
    Procedure TransactionRollback;
    Procedure SelectData(SQLs: TALFBXClientSelectDataSQLs;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings); overload;
    Procedure SelectData(SQL: String;
                         RowTag: String;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings); overload;
    Procedure SelectData(SQL: String;
                         RowTag: String;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings); overload;
    Procedure SelectData(SQL: String;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings); overload;
    procedure UpdateData(SQLs: TALFBXClientUpdateDataSQLs); overload;
    procedure UpdateData(SQLs: Tstrings); overload;
    procedure UpdateData(SQL: String); overload;
    Property  Connected: Boolean Read GetConnected;
    property  SqlDialect: word read FSqlDialect;
    Property  InTransaction: Boolean read GetInTransaction;
    Property  NullString: String Read fNullString Write fNullString;
    property  Lib: TALFBXLibrary read FLibrary;
    property  CharSet: TALFBXCharacterSet read fCharSet;
  end;

  {--------------------------------------------}
  TALFBXConnectionPoolContainer = Class(TObject)
    DBHandle: IscDbHandle;
    LastAccessDate: int64;
  End;

  {-----------------------------------------}
  TALFBXConnectionPoolClient = Class(Tobject)
  Private
    FSQLDIALECT: word;
    FLibrary: TALFBXLibrary;
    FownLibrary: Boolean;
    FConnectionPool: TObjectList;
    FConnectionPoolCS: TCriticalSection;
    FWorkingConnectionCount: Integer;
    FReleasingAllconnections: Boolean;
    FLastConnectionGarbage: Int64;
    FConnectionMaxIdleTime: integer;
    FDataBaseName: String;
    fCharSet: TALFBXCharacterSet;
    fOpenConnectionParams: String;
    FNullString: String;
    FLogin: String;
    FPassword: String;
  Protected
    function GetFieldValue(aSQLDA:TALFBXSQLResult;
                           aDBHandle: IscDbHandle;
                           aTraHandle: IscTrHandle;
                           aIndex: Integer;
                           aFormatSettings: TformatSettings): String; virtual;
    procedure GetTransactionStats(DBHandle: IscDbHandle;
                                  TraHandle: IscTrHandle;
                                  Var idx_reads: LongInt;
                                  Var seq_reads: LongInt);
    procedure doSQLDone(SQL: String;
                        XmlData: TalXmlNode;
                        StartDate, EndDate: Int64;
                        DBHandle: IscDbHandle;
                        TraHandle: IscTrHandle); virtual;
    procedure initObject(aDataBaseName,
                         aLogin,
                         aPassword,
                         aCharSet: String;
                         const aNumbuffers: integer = -1;
                         const aOpenConnectionExtraParams: String = ''); virtual;
  Public
    Constructor Create(aDataBaseName,
                       aLogin,
                       aPassword,
                       aCharSet: String;
                       aApiVer: TALFBXVersion_API;
                       const alib: String = GDS32DLL;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: String = ''); overload; virtual;
    Constructor Create(aDataBaseName,
                       aLogin,
                       aPassword,
                       aCharSet: String;
                       alib: TALFBXLibrary;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: String = ''); overload; virtual;
    Destructor  Destroy; Override;
    Function  AcquireConnection: IscDbHandle; virtual;
    Procedure ReleaseConnection(var DBHandle: IscDbHandle;
                                const CloseConnection: Boolean = False); virtual;
    Procedure ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True); virtual;
    Procedure TransactionStart(Var DBHandle: IscDbHandle;
                               var TraHandle: IscTrHandle;
                               const ReadOnly: boolean = False;
                               const TPB: string = ''); virtual;
    Procedure TransactionCommit(var DBHandle: IscDbHandle;
                                var TraHandle: IscTrHandle); virtual;
    Procedure TransactionRollback(var DBHandle: IscDbHandle;
                                  var TraHandle: IscTrHandle;
                                  const doCloseConnection: Boolean = False); virtual;
    Procedure SelectData(SQLs: TALFBXClientSelectDataSQLs;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const TPB: string = ''); overload; virtual;
    Procedure SelectData(SQL: String;
                         RowTag: String;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const TPB: string = ''); overload; virtual;
    Procedure SelectData(SQL: String;
                         RowTag: String;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const TPB: string = ''); overload; virtual;
    Procedure SelectData(SQL: String;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const TPB: string = ''); overload; virtual;
    procedure UpdateData(SQLs: TALFBXClientUpdateDataSQLs;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const TPB: string = ''); overload; virtual;
    procedure UpdateData(SQLs: Tstrings;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const TPB: string = ''); overload; virtual;
    procedure UpdateData(SQL: String;
                         const DBHandle: IscDbHandle = nil;
                         const TraHandle: IscTrHandle = nil;
                         const TPB: string = ''); overload; virtual;
    Function  ConnectionCount: Integer;
    property  SqlDialect: word read FSqlDialect;
    property  DataBaseName: String read FDataBaseName;
    property  Login: String read FLogin;
    property  Password: String read FPassword;
    property  ConnectionMaxIdleTime: integer read FConnectionMaxIdleTime write fConnectionMaxIdleTime;
    Property  NullString: String Read fNullString Write fNullString;
    property  Lib: TALFBXLibrary read FLibrary;
    property  CharSet: TALFBXCharacterSet read fCharSet;
  end;

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
    FDataBaseName: String;
    fCharSet: TALFBXCharacterSet;
    fOpenConnectionParams: String;
    fEventNamesArr: array[0..14] of AnsiString;
    fEventNamesCount: integer;
  protected
    procedure initObject(aDataBaseName,
                         aLogin,
                         aPassword,
                         aCharSet: String;
                         aEventNames: String;
                         aConnectionMaxIdleTime: integer;
                         aNumbuffers: integer;
                         aOpenConnectionExtraParams: String); virtual;
    procedure DoEvent(const EventName: string; Count: Integer); virtual; abstract;
    procedure DoException(Error: Exception); virtual; abstract;
  public
    constructor Create(aDataBaseName,
                       aLogin,
                       aPassword,
                       aCharSet: String;
                       aEventNames: String; // ; separated value like EVENT1;EVENT2; etc...
                       aApiVer: TALFBXVersion_API;
                       const alib: String = GDS32DLL;
                       const aConnectionMaxIdleTime: integer = -1;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: String = ''); overload; virtual;
    Constructor Create(aDataBaseName,
                       aLogin,
                       aPassword,
                       aCharSet: String;
                       aEventNames: String; // ; separated value like EVENT1;EVENT2; etc...
                       alib: TALFBXLibrary;
                       const aConnectionMaxIdleTime: integer = -1;
                       const aNumbuffers: integer = -1;
                       const aOpenConnectionExtraParams: String = ''); overload; virtual;
    Destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Execute; override;
    property  Signal: Thandle read FSignal;
  end;


implementation

uses ALWindows,
     AlFcnString,
     AlFcnHTML,
     alfbxError;

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

{*********************************************************}
function TALFBXClient.GetFieldValue(aSQLDA:TALFBXSQLResult;
                                    aDBHandle: IscDbHandle;
                                    aTraHandle: IscTrHandle;
                                    aIndex: Integer;
                                    aFormatSettings: TformatSettings): String;
  {-------------------------}
  Procedure InternalReadBlob;
  var BlobHandle: IscBlobHandle;
  begin
    with FLibrary do begin
      Result := '';
      BlobHandle := nil;
      BlobOpen(aDBHandle, aTraHandle, BlobHandle, aSQLDA.AsQuad[aIndex]);
      try
        BlobReadString(BlobHandle, Result);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;

Begin
  If not aSQLDA.IsNull[aIndex] then
    Case aSQLDA.SQLType[aIndex] of
      SQL_TIMESTAMP : Result := datetimetostr(aSQLDA.AsDateTime[aIndex], aFormatSettings);
      SQL_TYPE_TIME : Result := Timetostr(aSQLDA.AsTime[aIndex], aFormatSettings);
      SQL_TYPE_DATE : Result := Datetostr(aSQLDA.AsDate[aIndex], aFormatSettings);
      SQL_DOUBLE    : Result := Floattostr(aSQLDA.AsDouble[aIndex], aFormatSettings);
      SQL_FLOAT,
      SQL_D_FLOAT   : Result := Floattostr(aSQLDA.AsSingle[aIndex], aFormatSettings);
      SQL_INT64,
      SQL_LONG,
      SQL_SHORT     : begin
                        if aSQLDA.SQLScale[aIndex] < 0 then Result := FloatToStr(aSQLDA.asDouble[Aindex],aFormatSettings)
                        else result := aSQLDA.AsString[Aindex];
                      end;
      SQL_BLOB      : InternalReadBlob;
      else Result := aSQLDA.AsString[Aindex];
    end
    else result := fNullString;
end;

{****************************************************************}
procedure TALFBXClient.GetTransactionStats(Var idx_reads: LongInt;
                                           Var seq_reads: LongInt);
Var XMLDATA: TalXmlDocument;
    aFormatSettings: TformatSettings;
Begin

  //security check
  if not InTransaction then raise Exception.Create('No active transaction');

  //get the data from the monitoring table
  XMLDATA := ALCreateEmptyXMLDocument('root');
  try

    SelectData('SELECT '+
                 'MON$RECORD_IDX_READS as IDX_READS, '+
                 'MON$RECORD_SEQ_READS as SEQ_READS '+
               'FROM '+
                 'MON$RECORD_STATS '+
               'JOIN MON$TRANSACTIONS ON MON$TRANSACTIONS.MON$STAT_ID=MON$RECORD_STATS.MON$STAT_ID '+
               'WHERE '+
                 'MON$TRANSACTIONS.MON$TRANSACTION_ID=current_transaction',
               XMLDATA.DocumentElement,
               aFormatSettings);

    if XMLDATA.DocumentElement.ChildNodes.Count <> 2 then raise Exception.Create('Can not get the transaction stats');
    with XMLDATA.DocumentElement do begin
      Idx_reads := strtoint(ChildNodes['idx_reads'].Text);
      Seq_Reads := strtoint(ChildNodes['seq_reads'].Text);
    end;

  finally
    XMLDATA.free;
  end;

end;

{*******************************************}
procedure TALFBXClient.doSQLDone(SQL: String;
                                 XmlData: TalXmlNode;
                                 StartDate, EndDate: Int64);
begin
 // virtual method
end;

{********************************}
procedure TALFBXClient.initObject;
begin
  fDBHandle := nil;
  fTraHandle := nil;
  FSQLDIALECT := 3;
  fCharSet := csnone;
  fNullString := '';
end;

{***************************************************************************************}
constructor TALFBXClient.Create(ApiVer: TALFBXVersion_API; const lib: String = GDS32DLL);
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

{************************************************************************}
function  TALFBXClient.GetDataBaseInfoString(const item: Integer): string;
var size: byte;
    data: RawByteString;
begin
  If not connected then raise Exception.Create('Not connected');
  data := FLibrary.DatabaseInfoString(FDbHandle, item, 256);
  case Item of
    isc_info_cur_logfile_name,
    isc_info_wal_prv_ckpt_fname: begin
                                   size := byte(data[4]);
                                   Move(data[5], data[1], size);
                                   SetLength(data, size);
                                 end;
    else begin
      size := byte(data[5]);
      Move(data[6], data[1], size);
      SetLength(data, size);
    end;
  end;
  Result := string(data);
end;

{*****************************************************************************}
function  TALFBXClient.GetDataBaseInfoDateTime(const item: Integer): TDateTime;
begin
  If not connected then raise Exception.Create('Not connected');
  result := FLibrary.DatabaseInfoDateTime(FDbHandle, item);
end;

{*******************************************************}
procedure TALFBXClient.GetUserNames(UserNames: Tstrings);
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
    UserNames.Add(string(copy(p, 0, len)));
    inc(p, len);
  end;
end;

{*************************************************}
procedure TALFBXClient.CreateDatabase(SQL: String);
begin
  if connected then raise Exception.Create('Already connected!');
  Flibrary.DSQLExecuteImmediate(fDBHandle, fTraHandle, SQL, FSQLDIALECT, nil);
  try
    Flibrary.DetachDatabase(fDBHandle);
  Except
    //i m very curious what we can do here ?
  end;
  fDBHandle := nil;
end;

{**********************************}
procedure TALFBXClient.DropDatabase;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');
  if InTransaction then raise Exception.Create('Another transaction is active!');

  //drop the database
  FLibrary.DatabaseDrop(FDbHandle);

  //close the connection
  disconnect;

end;

{******************************************}
procedure TALFBXClient.Connect(DataBaseName,
                               Login,
                               Password,
                               CharSet: String;
                               const ExtraParams: String = '');
Var aParams: String;
begin
  if connected then raise Exception.Create('Already connected!');
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

{******************************************}
procedure TALFBXClient.connect(DataBaseName,
                               Login,
                               Password,
                               CharSet: String;
                               Numbuffers: integer);
begin
  Connect(DataBaseName,
          Login,
          Password,
          CharSet,
          'num_buffers = '+inttostr(Numbuffers));
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

{****************************************************************************}
procedure TALFBXClient.TransactionStart(Readonly: Boolean; const TPB: String);
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');
  if InTransaction then raise Exception.Create('Another transaction is active!');

  //Start the transaction
  If TPB = '' then begin

    if Readonly then begin

      Flibrary.TransactionStart(
                                fTraHandle,
                                fDBHandle,

                                isc_tpb_version3 + //Transaction version number is used internally by the InterBase engine. It is always be
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

                                isc_tpb_nowait     // Lock resolution describes how a transaction should react if a lock conflict occurs. Valid
                                                   // lock resolutions are:
                                                   // * isc_tpb_wait: Lock resolution specifies that the transaction is to wait until locked
                                                   //                 resources are released before retrying an operation [Default]
                                                   // * isc_tpb_nowait: Lock resolution specifies that the transaction is not to wait for locks to be
                                                   //                   released, but instead, a lock conflict error should be returned immediately
                               )
    end

    else begin

      Flibrary.TransactionStart(
                                fTraHandle,
                                fDBHandle,

                                isc_tpb_version3 + //Transaction version number is used internally by the InterBase engine. It is always be
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

                                isc_tpb_nowait     // Lock resolution describes how a transaction should react if a lock conflict occurs. Valid
                                                   // lock resolutions are:
                                                   // * isc_tpb_wait: Lock resolution specifies that the transaction is to wait until locked
                                                   //                 resources are released before retrying an operation [Default]
                                                   // * isc_tpb_nowait: Lock resolution specifies that the transaction is not to wait for locks to be
                                                   //
                               )

    end

  end

  else begin

    Flibrary.TransactionStart(fTraHandle,
                              fDBHandle,
                              TPB);

  end;

end;

{***************************************}
procedure TALFBXClient.TransactionCommit;
begin

  //Error if we are not connected
  if not InTransaction then raise Exception.Create('No active transaction to commit!');

  //commit the transaction
  Flibrary.TransactionCommit(fTraHandle);
  fTraHandle := nil;

end;

{*****************************************}
procedure TALFBXClient.TransactionRollback;
begin

  //Error if we are not connected
  if not InTransaction then raise Exception.Create('No active transaction to rollback!');

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

{*****************************************************************}
procedure TALFBXClient.SelectData(SQLs: TALFBXClientSelectDataSQLs;
                                  XMLDATA: TalXMLNode;
                                  FormatSettings: TformatSettings);
Var aSqlda: TALFBXSQLResult;
    aStmtHandle: IscStmtHandle;
    aColumnIndex: integer;
    aNewRec: TalXmlNode;
    aValueRec: TalXmlNode;
    aViewRec: TalXmlNode;
    aSQLsindex: integer;
    aRecIndex: integer;
    aRecAdded: integer;
    aStartDate: int64;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //clear the XMLDATA
  XMLDATA.ChildNodes.Clear;

  {loop on all the SQL}
  For aSQLsindex := 0 to length(SQLs) - 1 do begin

    //trim the SQL
    SQLs[aSQLsindex].SQL := trim(SQLs[aSQLsindex].SQL);

    //if the SQL is not empty
    if SQLs[aSQLsindex].SQL <> '' then begin

      //init aStartDate
      aStartDate := ALGetTickCount64;

      //create the sqlda result
      aSqlda := TALFBXSQLResult.Create(fCharSet);
      Try

        //init the aStmtHandle
        aStmtHandle := nil;
        Flibrary.DSQLAllocateStatement(fDBHandle, aStmtHandle);
        try

          //prepare and execute the query
          Flibrary.DSQLPrepare(fDBHandle, fTraHandle, aStmtHandle, SQLs[aSQLsindex].SQL, FSQLDIALECT, aSqlda);
          FLibrary.DSQLExecute(fTraHandle, aStmtHandle, FSQLDIALECT, nil);

          //init the aViewRec
          if SQLs[aSQLsindex].ViewTag <> '' then aViewRec := XMLdata.AddChild(SQLs[aSQLsindex].ViewTag)
          else aViewRec := XMLdata;

          //retrieve all row
          aRecIndex := 0;
          aRecAdded := 0;
          while Flibrary.DSQLFetch(fDBHandle, fTraHandle, aStmtHandle, FSQLDIALECT, asqlda) do begin

            //process if > Skip
            inc(aRecIndex);
            If aRecIndex > SQLs[aSQLsindex].Skip then begin

              //stop if no row are requested
              If (SQLs[aSQLsindex].First = 0) then break;

              //init NewRec
              if SQLs[aSQLsindex].RowTag <> '' then aNewRec := aViewRec.AddChild(SQLs[aSQLsindex].RowTag)
              Else aNewRec := aViewRec;

              //loop throught all column
              For aColumnIndex := 0 to asqlda.FieldCount - 1 do begin
                aValueRec := aNewRec.AddChild(ALlowercase(asqlda.AliasName[aColumnIndex]));
                if (aSQLDA.SQLType[aColumnIndex] = SQL_BLOB) then avalueRec.ChildNodes.Add(
                                                                                           avalueRec.OwnerDocument.CreateNode(
                                                                                                                              GetFieldValue(
                                                                                                                                            asqlda,
                                                                                                                                            fDBHandle,
                                                                                                                                            fTRAHandle,
                                                                                                                                            aColumnIndex,
                                                                                                                                            FormatSettings
                                                                                                                                           ),
                                                                                                                              ntCData
                                                                                                                             )
                                                                                           )
                else aValueRec.Text := GetFieldValue(asqlda, fDBHandle, fTRAHandle, aColumnIndex, FormatSettings);
              end;

              //handle the First
              inc(aRecAdded);
              If (SQLs[aSQLsindex].First >= 0) and (aRecAdded >= SQLs[aSQLsindex].First) then Break;

            end;

          end;

        finally
          Flibrary.DSQLFreeStatement(aStmtHandle, DSQL_drop);
        end;

      finally
        aSqlda.free;
      end;

      //do the onSQLDone
      DoSQLDone(SQLs[aSQLsindex].SQL,
                aViewRec,
                aStartDate,
                ALGetTickCount64);

    End;

  end;

end;

{********************************************}
procedure TALFBXClient.SelectData(SQL: String;
                                  RowTag: String;
                                  Skip: Integer;
                                  First: Integer;
                                  XMLDATA: TalXMLNode;
                                  FormatSettings: TformatSettings);
var aSelectDataSQLs: TALFBXClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := RowTag;
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := Skip;
  aSelectDataSQLs[0].First := First;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             FormatSettings);
end;

{********************************************}
procedure TALFBXClient.SelectData(SQL: String;
                                  RowTag: String;
                                  XMLDATA: TalXMLNode;
                                  FormatSettings: TformatSettings);
var aSelectDataSQLs: TALFBXClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := RowTag;
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             FormatSettings);
end;

{********************************************}
procedure TALFBXClient.SelectData(SQL: String;
                                  XMLDATA: TalXMLNode;
                                  FormatSettings: TformatSettings);
var aSelectDataSQLs: TALFBXClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := '';
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             FormatSettings);
end;

{******************************************************************}
procedure TALFBXClient.UpdateData(SQLs: TALFBXClientUpdateDataSQLs);
Var aSqlpa: TALFBXSQLParams;
    aBlobhandle: IscBlobHandle;
    aCurReqBlobsIndex: integer;
    aSQLsindex: integer;
    aStartDate: int64;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  {loop on all the SQL}
  For aSQLsindex := 0 to length(SQLs) - 1 do begin

    //trim the SQL
    SQLs[aSQLsindex].SQL := trim(SQLs[aSQLsindex].SQL);

    //if the SQL is not empty
    if SQLs[aSQLsindex].SQL <> '' then begin

      //init aStartDate
      aStartDate := ALGetTickCount64;

      //special case if their is blob
      if length(SQLs[aSQLsindex].Blobs) > 0 then begin

        //create the aSqlpa object
        aSqlpa := TALFBXSQLParams.Create(fCharSet);
        try

          //loop throught all blob
          for aCurReqBlobsIndex := 0 to length(SQLs[aSQLsindex].Blobs) - 1 do begin
            aSqlpa.AddFieldType('',uftBlob);
            aBlobhandle := nil;
            aSqlpa.AsQuad[aCurReqBlobsIndex] := Flibrary.BlobCreate(fDBHandle,fTraHandle,aBlobHandle);
            Try
              FLibrary.BlobWriteStream(aBlobHandle,SQLs[aSQLsindex].Blobs[aCurReqBlobsIndex]);
            Finally
              FLibrary.BlobClose(aBlobHandle);
            End;
          end;
          Flibrary.DSQLExecuteImmediate(fDBHandle, fTraHandle, SQLs[aSQLsindex].SQL, FSQLDIALECT, aSqlpa);

        finally
          asqlpa.free;
        end;

      end

      //if their is no blob smply call DSQLExecuteImmediate
      else Flibrary.DSQLExecuteImmediate(fDBHandle, fTraHandle, SQLs[aSQLsindex].SQL, FSQLDIALECT, nil);

      //do the onSQLDone
      DoSQLDone(SQLs[aSQLsindex].SQL,
                nil,
                aStartDate,
                ALGetTickCount64);

    end;

  end;

end;

{************************************************}
procedure TALFBXClient.UpdateData(SQLs: Tstrings);
Var aSQLsindex : integer;
    aUpdateDataSQLs: TALFBXClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,SQLs.Count);
  For aSQLsindex := 0 to SQLs.Count - 1 do begin
    aUpdateDataSQLs[aSQLsindex].SQL := SQLs[aSQLsindex];
    setlength(aUpdateDataSQLs[aSQLsindex].Blobs,0);
  end;
  UpdateData(aUpdateDataSQLs);
end;

{*********************************************}
procedure TALFBXClient.UpdateData(SQL: String);
Var aUpdateDataSQLs: TALFBXClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,1);
  aUpdateDataSQLs[0].SQL := SQL;
  setlength(aUpdateDataSQLs[0].Blobs,0);
  UpdateData(aUpdateDataSQLs);
end;




//////////////////////////////////////
///// TALFBXConnectionPoolClient /////
//////////////////////////////////////

{************************************************************************}
function TALFBXConnectionPoolClient.GetFieldValue(aSQLDA: TALFBXSQLResult;
                                                  aDBHandle: IscDbHandle;
                                                  aTraHandle: IscTrHandle;
                                                  aIndex: Integer;
                                                  aFormatSettings: TformatSettings): String;
  {-------------------------}
  Procedure InternalReadBlob;
  var BlobHandle: IscBlobHandle;
  begin
    with FLibrary do begin
      Result := '';
      BlobHandle := nil;
      BlobOpen(aDBHandle, aTraHandle, BlobHandle, aSQLDA.AsQuad[aIndex]);
      try
        BlobReadString(BlobHandle, Result);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;

Begin
  If not aSQLDA.IsNull[aIndex] then begin
    Case aSQLDA.SQLType[aIndex] of
      SQL_TIMESTAMP : Result := datetimetostr(aSQLDA.AsDateTime[aIndex], aFormatSettings);
      SQL_TYPE_TIME : Result := Timetostr(aSQLDA.AsTime[aIndex], aFormatSettings);
      SQL_TYPE_DATE : Result := Datetostr(aSQLDA.AsDate[aIndex], aFormatSettings);
      SQL_DOUBLE    : Result := Floattostr(aSQLDA.AsDouble[aIndex], aFormatSettings);
      SQL_FLOAT,
      SQL_D_FLOAT   : Result := Floattostr(aSQLDA.AsSingle[aIndex], aFormatSettings);
      SQL_INT64,
      SQL_LONG,
      SQL_SHORT     : begin
                        if aSQLDA.SQLScale[aIndex] < 0 then Result := FloatToStr(aSQLDA.asDouble[Aindex],aFormatSettings)
                        else result := aSQLDA.AsString[Aindex];
                      end;
      SQL_BLOB      : InternalReadBlob;
      else Result := aSQLDA.AsString[Aindex];
    end;
  end
  else result := fNullString;
end;

{*********************************************************}
procedure TALFBXConnectionPoolClient.doSQLDone(SQL: String;
                                               XmlData: TalXmlNode;
                                               StartDate, EndDate: Int64;
                                               DBHandle: IscDbHandle;
                                               TraHandle: IscTrHandle);
begin
  //virtual method
end;

{*****************************************************************************}
procedure TALFBXConnectionPoolClient.GetTransactionStats(DBHandle: IscDbHandle;
                                                         TraHandle: IscTrHandle;
                                                         var idx_reads: LongInt;
                                                         var seq_reads: LongInt);
Var XMLDATA: TalXmlDocument;
    aFormatSettings: TformatSettings;
Begin

  //security check
  if not assigned(DBHandle) or not assigned(TraHandle) then raise exception.Create('Connection handle can not be null');

  //get the data from the monitoring table
  XMLDATA := ALCreateEmptyXMLDocument('root');
  try

    SelectData('SELECT '+
                 'MON$RECORD_IDX_READS as IDX_READS, '+
                 'MON$RECORD_SEQ_READS as SEQ_READS '+
               'FROM '+
                 'MON$RECORD_STATS '+
               'JOIN MON$TRANSACTIONS ON MON$TRANSACTIONS.MON$STAT_ID=MON$RECORD_STATS.MON$STAT_ID '+
               'WHERE '+
                 'MON$TRANSACTIONS.MON$TRANSACTION_ID=current_transaction',
               XMLDATA.DocumentElement,
               aFormatSettings,
               DBHandle,
               TraHandle);

    if XMLDATA.DocumentElement.ChildNodes.Count <> 2 then raise Exception.Create('Can not get the transaction stats');
    with XMLDATA.DocumentElement do begin
      Idx_reads := strtoint(ChildNodes['idx_reads'].Text);
      Seq_Reads := strtoint(ChildNodes['seq_reads'].Text);
    end;

  finally
    XMLDATA.free;
  end;

end;

{************************************************************}
procedure TALFBXConnectionPoolClient.initObject(aDataBaseName,
                                                aLogin,
                                                aPassword,
                                                aCharSet: String;
                                                const aNumbuffers: integer = -1;
                                                const aOpenConnectionExtraParams: String = '');
begin
  FDataBaseName:= aDataBaseName;
  FCharset:= ALFBXStrToCharacterSet(aCharSet);
  fLogin := aLogin;
  fPassword := aPassword;
  fOpenConnectionParams := 'user_name = '+aLogin+'; '+
                           'password = '+aPassword+'; '+
                           'lc_ctype = '+aCharSet;
  if aNumbuffers > -1 then fOpenConnectionParams := fOpenConnectionParams + '; num_buffers = ' + inttostr(aNumbuffers);
  if aOpenConnectionExtraParams <> '' then fOpenConnectionParams := fOpenConnectionParams + '; ' + aOpenConnectionExtraParams;
  FSQLDIALECT := 3;
  FConnectionPool:= TObjectList.Create(True);
  FConnectionPoolCS:= TCriticalSection.create;
  FWorkingConnectionCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionGarbage := ALGettickCount64;
  FConnectionMaxIdleTime := 1200000; // 1000 * 60 * 20 = 20 min
  FNullString := '';
end;

{**********************************************************}
constructor TALFBXConnectionPoolClient.Create(aDataBaseName,
                                              aLogin,
                                              aPassword,
                                              aCharSet: String;
                                              aApiVer: TALFBXVersion_API;
                                              const alib: String = GDS32DLL;
                                              const aNumbuffers: integer = -1;
                                              const aOpenConnectionExtraParams: String = '');
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

{**********************************************************}
constructor TALFBXConnectionPoolClient.Create(aDataBaseName,
                                              aLogin,
                                              aPassword,
                                              aCharSet: String;
                                              alib: TALFBXLibrary;
                                              const aNumbuffers: integer = -1;
                                              const aOpenConnectionExtraParams: String = '');
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
  FConnectionPool.free;
  FConnectionPoolCS.free;
  if FownLibrary then fLibrary.Free;

  //inherite
  inherited;

end;

{*****************************************************************}
function TALFBXConnectionPoolClient.AcquireConnection: IscDbHandle;
Var aConnectionPoolContainer: TalFBXConnectionPoolContainer;
    aTickCount: int64;
Begin

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connection!');

    //delete the old unused connection
    aTickCount := ALGetTickCount64;
    if aTickCount - fLastConnectionGarbage > (FConnectionMaxIdleTime div 100)  then begin
      while FConnectionPool.Count > 0 do begin
        aConnectionPoolContainer := TalFBXConnectionPoolContainer(FConnectionPool[0]);
        if aTickCount - aConnectionPoolContainer.Lastaccessdate > FConnectionMaxIdleTime then begin
          Try
            fLibrary.DetachDatabase(aConnectionPoolContainer.DBHandle);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;
          FConnectionPool.Delete(0); // must be delete here because FConnectionPool free the object also
        end
        else break;
      end;
      FLastConnectionGarbage := aTickCount;
    end;

    //acquire the new connection from the pool
    If FConnectionPool.Count > 0 then begin
      aConnectionPoolContainer := TalFBXConnectionPoolContainer(FConnectionPool[FConnectionPool.count - 1]);
      Result := aConnectionPoolContainer.DBHandle;
      FConnectionPool.Delete(FConnectionPool.count - 1);
    end

    //create a new connection
    else begin
      Result := nil;
      FLibrary.AttachDatabase(FDataBaseName,
                              Result,
                              fOpenConnectionParams);
    end;

    //increase the connection count
    inc(FWorkingConnectionCount);

  //get out of the synchronization
  finally
    FConnectionPoolCS.Release;
  end;
End;

{*******************************************************************************}
procedure TALFBXConnectionPoolClient.ReleaseConnection(var DBHandle: IscDbHandle;
                                                       const CloseConnection: Boolean = False);
Var aConnectionPoolContainer: TalFBXConnectionPoolContainer;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');

  //release the connection
  FConnectionPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      aConnectionPoolContainer := TalFBXConnectionPoolContainer.Create;
      aConnectionPoolContainer.DBHandle := DBHandle;
      aConnectionPoolContainer.LastAccessDate := ALGetTickCount64;
      FConnectionPool.add(aConnectionPoolContainer);
    end

    //close the connection
    else begin
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
    Dec(FWorkingConnectionCount);

  finally
    FConnectionPoolCS.Release;
  end;

end;

{*******************************************************************************************************}
procedure TALFBXConnectionPoolClient.ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True);
Var aConnectionPoolContainer: TalFBXConnectionPoolContainer;
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
        sleep(100);
      end;

    {free all database}
    FConnectionPoolCS.Acquire;
    Try
      while FConnectionPool.Count > 0 do begin
        aConnectionPoolContainer := TalFBXConnectionPoolContainer(FConnectionPool[FConnectionPool.count - 1]);
        Try
          fLibrary.DetachDatabase(aConnectionPoolContainer.DBHandle);
        Except
          //i don't know but it's seam that i receive an exception here after
          //we disconnect the network
        End;
        FConnectionPool.Delete(FConnectionPool.count - 1); // must be delete here because FConnectionPool free the object also
      end;
    finally
      FConnectionPoolCS.Release;
    end;

  finally
    //Do not forbid anymore new thread to create a new transaction
    FReleasingAllconnections := False;
  End;

end;

{******************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionStart(Var DBHandle: IscDbHandle;
                                                      var TraHandle: IscTrHandle;
                                                      const ReadOnly: boolean = False;
                                                      const TPB: string = '');
Var areleaseDBHandleonError: Boolean;
begin

  //init the aConnectionHandle
  TraHandle := nil;
  if not assigned(DBHandle) then begin
    DBHandle := AcquireConnection;
    areleaseDBHandleonError := True;
  end
  else areleaseDBHandleonError := False;
  try

    //Start the transaction
    If TPB = '' then begin

      if Readonly then begin

        Flibrary.TransactionStart(
                                  TraHandle,
                                  DBHandle,

                                  isc_tpb_version3 + //Transaction version number is used internally by the InterBase engine. It is always be
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

                                  isc_tpb_nowait     // Lock resolution describes how a transaction should react if a lock conflict occurs. Valid
                                                     // lock resolutions are:
                                                     // * isc_tpb_wait: Lock resolution specifies that the transaction is to wait until locked
                                                     //                 resources are released before retrying an operation [Default]
                                                     // * isc_tpb_nowait: Lock resolution specifies that the transaction is not to wait for locks to be
                                                     //                   released, but instead, a lock conflict error should be returned immediately
                                 )
      end

      else begin

        Flibrary.TransactionStart(
                                  TraHandle,
                                  DBHandle,

                                  isc_tpb_version3 + //Transaction version number is used internally by the InterBase engine. It is always be
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

                                  isc_tpb_nowait     // Lock resolution describes how a transaction should react if a lock conflict occurs. Valid
                                                     // lock resolutions are:
                                                     // * isc_tpb_wait: Lock resolution specifies that the transaction is to wait until locked
                                                     //                 resources are released before retrying an operation [Default]
                                                     // * isc_tpb_nowait: Lock resolution specifies that the transaction is not to wait for locks to be
                                                     //
                                 )

      end

    end

    else begin

      Flibrary.TransactionStart(TraHandle,
                                DBHandle,
                                TPB);

    end;

  except
    if areleaseDBHandleonError then begin
      ReleaseConnection(DBHandle, True);
      ReleaseAllConnections(False); //because is an error happen before is mostly because of an disconnection
                                    //Unable to complete network request to host "localhost".
                                    //Error writing data to the connection.
                                    //An existing connection was forcibly closed by the remote host.
                                    //Unsuccessful execution caused by a system error that precludes
                                    //successful execution of subsequent statements
                                    //Error Code: 401
    end;
    raise;
  end;

end;

{*******************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionCommit(var DBHandle: IscDbHandle;
                                                       var TraHandle: IscTrHandle);
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');

  //commit the transaction
  FLibrary.TransactionCommit(TraHandle);
  TraHandle := nil;

  //release the connection
  ReleaseConnection(DBHandle);

end;

{*********************************************************************************}
procedure TALFBXConnectionPoolClient.TransactionRollback(var DBHandle: IscDbHandle;
                                                         var TraHandle: IscTrHandle;
                                                         const doCloseConnection: Boolean = False);
var aTmpdoCloseConnection: Boolean;
begin

  //security check
  if not assigned(DBHandle) then raise exception.Create('Connection handle can not be null');

  //rollback the connection
  aTmpdoCloseConnection := doCloseConnection;
  Try
    Try
      FLibrary.TransactionRollback(TraHandle);
    except
      //to not raise an exception, most of the time TransactionRollback
      //are call inside a try ... except
      //raising the exception here will hide the first exception message
      //it's not a problem to hide the error here because closing the
      //connection will normally rollback the data
      aTmpdoCloseConnection := True;
    End;
  Finally

    //release the connection
    TraHandle := nil;
    ReleaseConnection(DBHandle, aTmpdoCloseConnection);

  End;

end;

{*******************************************************************************}
procedure TALFBXConnectionPoolClient.SelectData(SQLs: TALFBXClientSelectDataSQLs;
                                                XMLDATA: TalXMLNode;
                                                FormatSettings: TformatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const TPB: string = '');
Var aStmtHandle: IscStmtHandle;
    aSqlda: TALFBXSQLResult;
    aColumnIndex: integer;
    aNewRec: TalXmlNode;
    aValueRec: TalXmlNode;
    aViewRec: TalXmlNode;
    aSQLsindex: integer;
    aRecIndex: integer;
    aRecAdded: integer;
    aTmpDBHandle: IscDbHandle;
    aTmpTraHandle: IscTrHandle;
    aOwnConnection: Boolean;
    aGDSCode: integer;
    aStartDate: int64;
begin

  //clear the XMLDATA
  XMLDATA.ChildNodes.Clear;

  //acquire a connection and start the transaction if necessary
  aTmpDBHandle := DBHandle;
  aTmpTraHandle := TraHandle;
  aOwnConnection := (not assigned(DBHandle)) and
                    (not assigned(TraHandle));
  if aOwnConnection then TransactionStart(aTmpDBHandle,  // DBHandle
                                          aTmpTraHandle, // TraHandle
                                          True,          // ReadOnly
                                          TPB);          // TPB
  Try

    {loop on all the SQL}
    For aSQLsindex := 0 to length(SQLs) - 1 do begin

      //trim the SQL
      SQLs[aSQLsindex].SQL := trim(SQLs[aSQLsindex].SQL);

      //if the SQL is not empty
      if SQLs[aSQLsindex].SQL <> '' then begin

        //init aStartDate
        aStartDate := ALGetTickCount64;

        //create the sqlda result
        aSqlda := TALFBXSQLResult.Create(fCharSet);
        Try

          //init the aStmtHandle
          aStmtHandle := nil;
          Flibrary.DSQLAllocateStatement(aTmpDBHandle, aStmtHandle);
          try

            //prepare and execute the query
            Flibrary.DSQLPrepare(aTmpDBHandle, aTmpTraHandle, aStmtHandle, SQLs[aSQLsindex].SQL, FSQLDIALECT, aSqlda);
            FLibrary.DSQLExecute(aTmpTraHandle, aStmtHandle, FSQLDIALECT, nil);

            //init the aViewRec
            if SQLs[aSQLsindex].ViewTag <> '' then aViewRec := XMLdata.AddChild(SQLs[aSQLsindex].ViewTag)
            else aViewRec := XMLdata;

            //retrieve all row
            aRecIndex := 0;
            aRecAdded := 0;
            while Flibrary.DSQLFetch(aTmpDBHandle, aTmpTraHandle, aStmtHandle, FSQLDIALECT, asqlda) do begin

              //process if > Skip
              inc(aRecIndex);
              If aRecIndex > SQLs[aSQLsindex].Skip then begin

                //stop if no row are requested
                If (SQLs[aSQLsindex].First = 0) then break;

                //init NewRec
                if SQLs[aSQLsindex].RowTag <> '' then aNewRec := aViewRec.AddChild(SQLs[aSQLsindex].RowTag)
                Else aNewRec := aViewRec;

                //loop throught all column
                For aColumnIndex := 0 to asqlda.FieldCount - 1 do begin
                  aValueRec := aNewRec.AddChild(ALlowercase(asqlda.AliasName[aColumnIndex]));
                  if (aSQLDA.SQLType[aColumnIndex] = SQL_BLOB) then avalueRec.ChildNodes.Add(
                                                                                             avalueRec.OwnerDocument.CreateNode(
                                                                                                                                GetFieldValue(
                                                                                                                                              asqlda,
                                                                                                                                              aTmpDBHandle,
                                                                                                                                              aTmpTRAHandle,
                                                                                                                                              aColumnIndex,
                                                                                                                                              FormatSettings
                                                                                                                                             ),
                                                                                                                                ntCData
                                                                                                                               )
                                                                                             )
                  else aValueRec.Text := GetFieldValue(asqlda, aTmpDBHandle, aTmpTRAHandle, aColumnIndex, FormatSettings);
                end;

                //handle the First
                inc(aRecAdded);
                If (SQLs[aSQLsindex].First >= 0) and (aRecAdded >= SQLs[aSQLsindex].First) then Break;

              end;

            end;

          finally
            Flibrary.DSQLFreeStatement(aStmtHandle, DSQL_drop);
          end;

        finally
          aSqlda.free;
        end;

        //do the onSQLDone
        DoSQLDone(SQLs[aSQLsindex].SQL,
                  aViewRec,
                  aStartDate,
                  ALGetTickCount64,
                  aTmpDBHandle,
                  aTmpTraHandle);

      end;

    end;

    //commit the transaction and release the connection if owned
    if aOwnConnection then TransactionCommit(aTmpDBHandle, aTmpTraHandle);

  except
    On E: Exception do begin

      {get the gdscode}
      if E is EALFBXError then aGDSCode := (E as EALFBXError).GDSCode
      else aGDSCode := -1;

      //rollback the transaction and release the connection if owned
      if aOwnConnection then TransactionRollback(aTmpDBHandle,   // DBHandle
                                                 aTmpTraHandle,  // TraHandle
                                                 true);          // doCloseConnection ... for exemple if the connection is spoiled by network shuntdown

      //Database @1 shutdown
      if aGDSCode = isc_shutdown then ReleaseAllConnections(False);

      //raise the error
      raise;

    end;
  end;

end;

{**********************************************************}
procedure TALFBXConnectionPoolClient.SelectData(SQL: String;
                                                RowTag: String;
                                                Skip: integer;
                                                First: Integer;
                                                XMLDATA: TalXMLNode;
                                                FormatSettings: TformatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const TPB: string = '');
var aSelectDataSQLs: TalFBXClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := RowTag;
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := Skip;
  aSelectDataSQLs[0].First := First;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             FormatSettings,
             DBHandle,
             TraHandle,
             TPB);
end;

{**********************************************************}
procedure TALFBXConnectionPoolClient.SelectData(SQL: String;
                                                RowTag: String;
                                                XMLDATA: TalXMLNode;
                                                FormatSettings: TformatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const TPB: string = '');
var aSelectDataSQLs: TalFBXClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := RowTag;
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             FormatSettings,
             DBHandle,
             TraHandle,
             TPB);
end;

{**********************************************************}
procedure TALFBXConnectionPoolClient.SelectData(SQL: String;
                                                XMLDATA: TalXMLNode;
                                                FormatSettings: TformatSettings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const TPB: string = '');
var aSelectDataSQLs: TalFBXClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := '';
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             FormatSettings,
             DBHandle,
             TraHandle,
             TPB);
end;

{*******************************************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(SQLs: TALFBXClientUpdateDataSQLs;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const TPB: string = '');
Var aSqlpa: TALFBXSQLParams;
    aBlobhandle: IscBlobHandle;
    aCurReqBlobsIndex: integer;
    aSQLsindex: integer;
    aTmpDBHandle: IscDbHandle;
    aTmpTraHandle: IscTrHandle;
    aOwnConnection: Boolean;
    aGDSCode: integer;
    aStartDate: int64;
begin

  //acquire a connection and start the transaction if necessary
  aTmpDBHandle := DBHandle;
  aTmpTraHandle := TraHandle;
  aOwnConnection := (not assigned(DBHandle)) and
                    (not assigned(TraHandle));
  if aOwnConnection then TransactionStart(aTmpDBHandle,  // DBHandle
                                          aTmpTraHandle, // TraHandle
                                          False,         // ReadOnly
                                          TPB);          // TPB
  Try

    {loop on all the SQL}
    For aSQLsindex := 0 to length(SQLs) - 1 do begin

      //trim the SQL
      SQLs[aSQLsindex].SQL := trim(SQLs[aSQLsindex].SQL);

      //if the SQL is not empty
      if SQLs[aSQLsindex].SQL <> '' then begin

        //init aStartDate
        aStartDate := ALGetTickCount64;

        //special case if their is blob
        if length(SQLs[aSQLsindex].Blobs) > 0 then begin

          //create the aSqlpa object
          aSqlpa := TALFBXSQLParams.Create(fCharSet);
          try

            //loop throught all blob
            for aCurReqBlobsIndex := 0 to length(SQLs[aSQLsindex].Blobs) - 1 do begin
              aSqlpa.AddFieldType('',uftBlob);
              aBlobhandle := nil;
              aSqlpa.AsQuad[aCurReqBlobsIndex] := Flibrary.BlobCreate(aTmpDBHandle,aTmpTraHandle,aBlobHandle);
              Try
                FLibrary.BlobWriteStream(aBlobHandle,SQLs[aSQLsindex].Blobs[aCurReqBlobsIndex]);
              Finally
                FLibrary.BlobClose(aBlobHandle);
              End;
            end;
            Flibrary.DSQLExecuteImmediate(aTmpDBHandle, aTmpTraHandle, SQLs[aSQLsindex].SQL, FSQLDIALECT, aSqlpa);

          finally
            asqlpa.free;
          end;

        end

        //if their is no blob smply call DSQLExecuteImmediate
        else Flibrary.DSQLExecuteImmediate(aTmpDBHandle, aTmpTraHandle, SQLs[aSQLsindex].SQL, FSQLDIALECT, nil);

        //do the onSQLDone
        DoSQLDone(SQLs[aSQLsindex].SQL,
                  nil,
                  aStartDate,
                  ALGetTickCount64,
                  aTmpDBHandle,
                  aTmpTraHandle);

      end;

    end;

    //commit the transaction and release the connection if owned
    if aOwnConnection then TransactionCommit(aTmpDBHandle, aTmpTraHandle);

  except
    On E: Exception do begin

      {get the gdscode}
      if E is EALFBXError then aGDSCode := (E as EALFBXError).GDSCode
      else aGDSCode := -1;

      {rollback the transaction}
      if aOwnConnection then begin
        if (aGDSCode = isc_lock_conflict) or    // Lock conflict on no wait transaction
           (aGDSCode = isc_foreign_key) then    // Violation of FOREIGN KEY constraint "@1" on table "@2"
          TransactionRollback(aTmpDBHandle, aTmpTraHandle, False)      // do not close the exception, it's not an connection error
        else TransactionRollback(aTmpDBHandle, aTmpTraHandle, true);   // close the connection, it's can be a connection error
      end;

      //Database @1 shutdown
      if aGDSCode = isc_shutdown then ReleaseAllConnections(False);

      //raise the error
      raise;

    end;
  end;
end;

{*************************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(SQLs: Tstrings;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const TPB: string = '');
Var aSQLsindex : integer;
    aUpdateDataSQLs: TalFBXClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,SQLs.Count);
  For aSQLsindex := 0 to SQLs.Count - 1 do begin
    aUpdateDataSQLs[aSQLsindex].SQL := SQLs[aSQLsindex];
    setlength(aUpdateDataSQLs[aSQLsindex].Blobs,0);
  end;
  UpdateData(aUpdateDataSQLs,
             DBHandle,
             TraHandle,
             TPB);
end;

{**********************************************************}
procedure TALFBXConnectionPoolClient.UpdateData(SQL: String;
                                                const DBHandle: IscDbHandle = nil;
                                                const TraHandle: IscTrHandle = nil;
                                                const TPB: string = '');
Var aUpdateDataSQLs: TalFBXClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,1);
  aUpdateDataSQLs[0].SQL := SQL;
  setlength(aUpdateDataSQLs[0].Blobs,0);
  UpdateData(aUpdateDataSQLs,
             DBHandle,
             TraHandle,
             TPB);
end;

{***********************************************************}
function TALFBXConnectionPoolClient.ConnectionCount: Integer;
begin
  FConnectionPoolCS.Acquire;
  Try
    Result := FConnectionPool.Count + FWorkingConnectionCount;
  finally
    FConnectionPoolCS.Release;
  end;
end;



/////////////////////////////
///// TALFBXEventThread /////
/////////////////////////////

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
      Move(Updated^, fResultBuffer^, Length);
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

{***************************************************}
procedure TALFBXEventThread.initObject(aDataBaseName,
                                       aLogin,
                                       aPassword,
                                       aCharSet: String;
                                       aEventNames: String;
                                       aConnectionMaxIdleTime: integer;
                                       aNumbuffers: integer;
                                       aOpenConnectionExtraParams: String);
Var aLst: TStrings;
    i: integer;
begin
  //if we put lower than tpNormal it seam than the
  //EventThread.Free will never return !
  //Priority := tpNormal;
  FreeOnTerminate := False;
  FConnectionMaxIdleTime := aConnectionMaxIdleTime;
  if FConnectionMaxIdleTime <= 0 then FConnectionMaxIdleTime := INFINITE;
  FDBHandle := nil;
  FQueueEvent := False;
  fResultBuffer := Nil;
  FSignal := CreateEvent(nil, true, false, '');
  fcompleted := False;
  fStarted := False;
  FEventCanceled := False;
  FWaitingSignal := False;
  FDataBaseName:= aDataBaseName;
  FCharset:= ALFBXStrToCharacterSet(aCharSet);
  fOpenConnectionParams := 'user_name = '+aLogin+'; '+
                           'password = '+aPassword+'; '+
                           'lc_ctype = '+aCharSet;
  if aNumbuffers > -1 then fOpenConnectionParams := fOpenConnectionParams + '; num_buffers = ' + inttostr(aNumbuffers);
  if aOpenConnectionExtraParams <> '' then fOpenConnectionParams := fOpenConnectionParams + '; ' + aOpenConnectionExtraParams;
  aLst := TstringList.Create;
  Try
    Alst.Text := Trim(alStringReplace(aEventNames,';',#13#10,[rfReplaceALL]));
    i := 0;
    while (i <= 14) and (i <= Alst.Count - 1) do begin
      fEventNamesArr[i] := Trim(Alst[i]);
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

{*************************************************}
constructor TALFBXEventThread.Create(aDataBaseName,
                                     aLogin,
                                     aPassword,
                                     aCharSet: String;
                                     aEventNames: String; // ; separated value like EVENT1;EVENT2; etc...
                                     aApiVer: TALFBXVersion_API;
                                     const alib: String = GDS32DLL;
                                     const aConnectionMaxIdleTime: integer = -1;
                                     const aNumbuffers: integer = -1;
                                     const aOpenConnectionExtraParams: String = '');
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
             aOpenConnectionExtraParams);
  inherited Create(False); // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010
end;

{*************************************************}
constructor TALFBXEventThread.Create(aDataBaseName,
                                     aLogin,
                                     aPassword,
                                     aCharSet: String;
                                     aEventNames: String; // ; separated value like EVENT1;EVENT2; etc...
                                     alib: TALFBXLibrary;
                                     const aConnectionMaxIdleTime: integer = -1;
                                     const aNumbuffers: integer = -1;
                                     const aOpenConnectionExtraParams: String = '');
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
             aOpenConnectionExtraParams);
  inherited Create(False);  // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010
end;

{********************************************}
procedure TALFBXEventThread.AfterConstruction;
begin
  inherited;
  while (not fStarted) do sleep(10);
end;

{***********************************}
destructor TALFBXEventThread.Destroy;
begin

  //first set terminated to true
  If not Terminated then Terminate;

  //in case the execute in waiting fire the Fsignal
  while (not fWaitingSignal) and (not fCompleted) do sleep(10);
  if (not fCompleted) then setEvent(FSignal);
  while (not fCompleted) do sleep(10);
  //sleep(100);   => i don't know the purpose of this so i comment it !

  //close the fSignal handle
  CloseHandle(FSignal);

  //free the library
  if FownLibrary then fLibrary.Free;

  //destroy the object
  inherited;

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

      //ok, if we remove the instruction below then sometime, when we close
      //the program we can have an eAcessViolation. to see it simply run
      //a program to run and imediatly close and have some delay/sleep
      //in other unit (3seconds it's enalfe). Run Winreguardian -nothingtolaunch
      //for exemple
      //sleep(100);
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
