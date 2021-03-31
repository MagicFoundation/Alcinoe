{*******************************************************************************
An Object to query MySql Server Version 5 and get the Result In Xml Stream
http://dev.mysql.com/doc/refman/5.0/en/
http://dev.mysql.com/doc/refman/5.0/en/string-syntax.html
*******************************************************************************}

unit AlMySqlClient;

interface

uses
  Winapi.Windows,
  System.Contnrs,
  System.SyncObjs,
  ALCommon,
  AlXmlDoc,
  ALStringList,
  ALString,
  AlMySqlWrapper;

Type

  {------------------------------------------------------------------------------------}
  TalMySqlClientSelectDataOnNewRowFunct = reference to Procedure(XMLRowData: TalXmlNode;
                                                                 const ViewTag: AnsiString;
                                                                 ExtData: Pointer;
                                                                 Var Continue: Boolean);

  {---------------------------------}
  EALMySqlError = class(EALException)
  private
    FErrorCode: Integer;
    FSQLstate: AnsiString;
  public
    constructor Create(const aErrorMsg: AnsiString;
                       const aErrorCode: Integer;
                       const aSqlState: AnsiString); overload;
    property ErrorCode: Integer read FErrorCode;
    property SQLState: AnsiString read FSQLState;
  end;

  {---------------------}
  TALMySQLOption = record
    Option: TMySqlOption;
    Value:  PANSIChar;
  end;
  TALMySQLOptions = array of TALMySQLOption;

  {-----------------------------}
  TalMySqlClient = Class(Tobject)
  Private
    fLibrary: TALMySqlLibrary;
    FownLibrary: Boolean;
    fMySql: PMySql;
    fNullString: AnsiString;
    finTransaction: Boolean;
    function  GetConnected: Boolean;
    function  GetInTransaction: Boolean;
  Protected
    function loadCachedData(const Key: AnsiString;
                            var DataStr: AnsiString): Boolean; virtual;
    Procedure SaveDataToCache(const Key: ansiString;
                              const CacheThreshold: integer;
                              const DataStr: ansiString); virtual;
    procedure CheckAPIError(Error: Boolean);
    Function  GetFieldValue(aFieldValue: PAnsiChar;
                            aFieldType: TMysqlFieldTypes;
                            aFieldLength: integer;
                            const aFormatSettings: TALFormatSettings): AnsiString;
    procedure initObject; virtual;
    procedure OnSelectDataDone(const SQL: AnsiString;
                               const RowTag: AnsiString;
                               const ViewTag: AnsiString;
                               Skip: integer;
                               First: Integer;
                               CacheThreshold: Integer;
                               TimeTaken: double); virtual;
    procedure OnUpdateDataDone(const SQL: AnsiString;
                               TimeTaken: double); virtual;
  Public
    Constructor Create(ApiVer: TALMySqlVersion_API;
                       const lib: AnsiString = 'libmysql.dll'); overload; virtual;
    Constructor Create(lib: TALMySqlLibrary); overload; virtual;
    Destructor Destroy; Override;
    Procedure Connect(const Host: AnsiString;
                      Port: integer;
                      const DataBaseName,
                            Login,
                            Password,
                            CharSet: AnsiString;
                      Const ClientFlag: Cardinal = 0;
                      Const Options: TALMySQLOptions = nil); virtual;
    Procedure Disconnect;
    Procedure TransactionStart;
    Procedure TransactionCommit;
    Procedure TransactionRollback;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         const ViewTag: AnsiString;
                         Skip: integer; // used only if value is > 0
                         First: Integer; // used only if value is > 0
                         CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                  // cache or not. Values <= 0 deactivate the cache
                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
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
    procedure UpdateData(SQLs: TALStrings); overload; virtual;
    procedure UpdateData(const SQL: AnsiString); overload; virtual;
    procedure UpdateData(const SQLs: array of AnsiString); overload; virtual;
    function  insert_id(const SQL: AnsiString): ULongLong;
    Property  Connected: Boolean Read GetConnected;
    Property  InTransaction: Boolean read GetInTransaction;
    Property  NullString: AnsiString Read fNullString Write fNullString;
    property  Lib: TALMySqlLibrary read FLibrary;
  end;

  {--------------------------------------}
  TalMySqlConnectionPoolContainer = record
    ConnectionHandle: PMySql;
    LastAccessDate: int64;
  End;
  TalMySqlConnectionPool = array of TalMySqlConnectionPoolContainer;

  {-------------------------------------------}
  TalMySqlConnectionPoolClient = Class(Tobject)
  Private
    FLibrary: TALMySqlLibrary;
    FownLibrary: Boolean;
    FConnectionPool: TalMySqlConnectionPool;
    FConnectionPoolCount: integer;
    FConnectionPoolCapacity: integer;
    FConnectionPoolCS: TCriticalSection;
    FWorkingConnectionCount: Integer;
    FReleasingAllconnections: Boolean;
    FLastConnectionGarbage: Int64;
    FConnectionMaxIdleTime: integer;
    FHost: AnsiString;
    FPort: Integer;
    FDataBaseName: AnsiString;
    fLogin: AnsiString;
    fPassword: AnsiString;
    fCharset: AnsiString;
    fOpenConnectionClientFlag: cardinal;
    FOpenConnectionOptions: TALMySQLOptions;
    FNullString: AnsiString;
  Protected
    function loadCachedData(const Key: AnsiString;
                            var DataStr: AnsiString): Boolean; virtual;
    Procedure SaveDataToCache(const Key: ansiString;
                              const CacheThreshold: integer;
                              const DataStr: ansiString); virtual;
    procedure CheckAPIError(ConnectionHandle: PMySql; Error: Boolean);
    function  GetDataBaseName: AnsiString; virtual;
    function  GetHost: AnsiString; virtual;
    function  GetPort: integer; virtual;
    Function  GetFieldValue(aFieldValue: PAnsiChar;
                            aFieldType: TMysqlFieldTypes;
                            aFieldLength: integer;
                            const aFormatSettings: TALFormatSettings): AnsiString;
    Function  AcquireConnection: PMySql; virtual;
    Procedure ReleaseConnection(var ConnectionHandle: PMySql; const CloseConnection: Boolean = False); virtual;
    procedure initObject(const aHost: AnsiString;
                         aPort: integer;
                         const aDataBaseName,
                               aLogin,
                               aPassword,
                               aCharSet: AnsiString;
                         Const aOpenConnectionClientFlag: Cardinal = 0;
                         Const aOpenConnectionOptions: TALMySQLOptions = nil); virtual;
    procedure OnSelectDataDone(const SQL: AnsiString;
                               const RowTag: AnsiString;
                               const ViewTag: AnsiString;
                               Skip: integer;
                               First: Integer;
                               CacheThreshold: Integer;
                               TimeTaken: double); virtual;
    procedure OnUpdateDataDone(const SQL: AnsiString;
                               TimeTaken: double); virtual;
  Public
    Constructor Create(const aHost: AnsiString;
                       aPort: integer;
                       const aDataBaseName,
                             aLogin,
                             aPassword,
                             aCharSet: AnsiString;
                       aApiVer: TALMySqlVersion_API;
                       Const alib: AnsiString = 'libmysql.dll';
                       Const aOpenConnectionClientFlag: Cardinal = 0;
                       Const aOpenConnectionOptions: TALMySQLOptions = nil); overload; virtual;
    Constructor Create(const aHost: AnsiString;
                       aPort: integer;
                       const aDataBaseName,
                             aLogin,
                             aPassword,
                             aCharSet: AnsiString;
                       alib: TALMySqlLibrary;
                       Const aOpenConnectionClientFlag: Cardinal = 0;
                       Const aOpenConnectionOptions: TALMySQLOptions = nil); overload; virtual;
    Destructor  Destroy; Override;
    Procedure ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True); virtual;
    Procedure TransactionStart(Var ConnectionHandle: PMySql); virtual;
    Procedure TransactionCommit(var ConnectionHandle: PMySql; const CloseConnection: Boolean = False); virtual;
    Procedure TransactionRollback(var ConnectionHandle: PMySql; const CloseConnection: Boolean = False); virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         const ViewTag: AnsiString;
                         Skip: integer; // used only if value is > 0
                         First: Integer; // used only if value is > 0
                         CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                  // cache or not. Values <= 0 deactivate the cache
                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         const RowTag: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(const SQL: AnsiString;
                         XMLDATA: TalXMLNode;
                         const FormatSettings: TALFormatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    procedure UpdateData(SQLs: TALStrings; const ConnectionHandle: PMySql = nil); overload; virtual;
    procedure UpdateData(const SQL: AnsiString; const ConnectionHandle: PMySql = nil); overload; virtual;
    procedure UpdateData(const SQLs: array of AnsiString; const ConnectionHandle: PMySql = nil); overload; virtual;
    Function  insert_id(const SQL: AnsiString; const ConnectionHandle: PMySql = nil): UlongLong; virtual;
    Function  ConnectionCount: Integer;
    Function  WorkingConnectionCount: Integer;
    property  DataBaseName: AnsiString read GetDataBaseName;
    property  Host: AnsiString read GetHost;
    property  Port: integer read GetPort;
    property  ConnectionMaxIdleTime: integer read FConnectionMaxIdleTime write fConnectionMaxIdleTime;
    Property  NullString: AnsiString Read fNullString Write fNullString;
    property  Lib: TALMySqlLibrary read FLibrary;
  end;

Function AlMySqlClientSlashedStr(Const Str: AnsiString): AnsiString;

var
  ALMySqlFormatSettings: TALFormatSettings;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Diagnostics,
  ALCipher,
  ALWindows;

{******************************************************************}
Function AlMySqlClientSlashedStr(Const Str: AnsiString): AnsiString;
var I: Integer;
begin
  Result := Str;
  for I := Length(Result) downto 1 do
    if Result[I] in ['''','"','\',#0] then Insert('\', Result, I);
  Result := '''' + Result + '''';
end;

{***********************************************************}
constructor EALMySqlError.Create(const aErrorMsg: AnsiString;
                                 const aErrorCode: Integer;
                                 const aSqlState: AnsiString);
begin
  fErrorCode := aErrorCode;
  FSQLstate := aSqlState;
  inherited create(aErrorMsg);
end;

{********************************************}
function TalMySqlClient.GetConnected: Boolean;
begin
  result := assigned(fMySql);
end;

{************************************************}
function TalMySqlClient.GetInTransaction: Boolean;
begin
  result := finTransaction;
end;

{***********************************************************}
function TalMySqlClient.loadCachedData(const Key: AnsiString;
                                       var DataStr: AnsiString): Boolean;
begin
  result := false; //virtual need to be overriden
end;

{*************************************************************}
Procedure TalMySqlClient.SaveDataToCache(const Key: ansiString;
                                         const CacheThreshold: integer;
                                         const DataStr: ansiString);
begin
  //virtual need to be overriden
end;

{*****************************************************}
procedure TalMySqlClient.CheckAPIError(Error: Boolean);
Begin
  if Error then begin
    if assigned(FMySql) then raise EALMySqlError.Create(fLibrary.mysql_error(fMySQL),
                                                        fLibrary.mysql_errno(fMySQL),
                                                        fLibrary.mysql_SqlState(fMySQL))
    else raise EALMySqlError.Create('MySql error',
                                    -1,
                                    'HY000'); // The value 'HY000' (general error) is used for unmapped error numbers.
  end;
end;

{***********************************************************}
function TalMySqlClient.GetFieldValue(aFieldValue: PAnsiChar;
                                      aFieldType: TMysqlFieldTypes;
                                      aFieldLength: integer;
                                      const aFormatSettings: TALFormatSettings): AnsiString;
begin
  //The lengths of the field values in the row may be obtained by calling mysql_fetch_lengths().
  //Empty fields and fields containing NULL both have length 0; you can distinguish these
  //by checking the pointer for the field value. If the pointer is NULL, the field
  //is NULL; otherwise, the field is empty.
  IF aFieldValue = nil then result := fNullString
  else begin
    Case aFieldType of
      FIELD_TYPE_DECIMAL,
      FIELD_TYPE_NEWDECIMAL,
      FIELD_TYPE_FLOAT,
      FIELD_TYPE_DOUBLE: result := ALFloatToStr(ALStrToFloat(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_DATETIME: Result := ALDateTimeToStr(ALStrToDateTime(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_DATE,
      FIELD_TYPE_NEWDATE: Result := ALDateToStr(ALStrToDate(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_TIME: Result := ALTimeToStr(ALStrToTime(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_TIMESTAMP: Result := ALDateTimeToStr(ALStrToDateTime(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_NULL: result := fNullString; // Example: SELECT NULL FROM DUAL
      Else SetString(Result, aFieldValue, aFieldLength);
    end;
  end;
end;

{**********************************}
procedure TalMySqlClient.initObject;
begin
  fMySql := nil;
  finTransaction := False;
  fNullString := '';
end;

{************************************************************}
constructor TalMySqlClient.Create(ApiVer: TALMySqlVersion_API;
                                  const lib: AnsiString = 'libmysql.dll');
begin
  fLibrary := TALMySqlLibrary.Create(ApiVer);
  try
    fLibrary.Load(lib);
    FownLibrary := True;
    initObject;
  Except
    fLibrary.free;
    raise;
  end;
end;

{******************************************************}
constructor TalMySqlClient.Create(lib: TALMySqlLibrary);
begin
  fLibrary := lib;
  FownLibrary := False;
  initObject;
end;

{********************************}
destructor TalMySqlClient.Destroy;
begin
  If Connected then disconnect;
  if FownLibrary then fLibrary.Free;
  inherited;
end;

{*************************************************************
http://dev.mysql.com/doc/refman/5.1/en/mysql-real-connect.html}
procedure TalMySqlClient.connect(const Host: AnsiString;
                                 Port: integer;
                                 const DataBaseName,
                                       Login,
                                       Password,
                                       CharSet: AnsiString;
                                 Const ClientFlag: Cardinal = 0;
                                 const Options: TALMySQLOptions = nil);
var I: integer;
begin
  if connected then raise Exception.Create('Already connected');

  // This function must be called early within each
  // created thread to initialize thread-specific variables
  // this look very important because if we comment this and
  // the mysql_thread_end then the Flibrary.unload can take several seconds
  // you can see this in the ALSQLBenchmark.exe project with the loop update button
  CheckAPIError(FLibrary.mysql_thread_init <> 0);
  Try

    //Allocates or initializes a MYSQL object suitable for mysql_real_connect()
  	fMySQL := fLibrary.mysql_init(nil);
  	CheckAPIError(fMySQL = nil);

    //set the The name of the character set to use as the default character set.
    If (CharSet <> '') then CheckAPIError(fLibrary.mysql_options(fMySQL, MYSQL_SET_CHARSET_NAME, PAnsiChar(CharSet)) <> 0);

    // set the options if they are existing
    for I := 0 to length(Options) - 1 do
      CheckAPIError(FLibrary.mysql_options(FMySQL,
                                           Options[I].Option,
                                           Options[I].Value) <> 0);

    //attempts to establish a connection to a MySQL database engine running on host
    CheckAPIError(fLibrary.mysql_real_connect(fMySQL,
                                              PAnsiChar(Host),
                                              PAnsiChar(Login),
                                              PAnsiChar(Password),
                                              PAnsiChar(DatabaseName),
                                              Port,
                                              nil,
                                              ClientFlag) = nil);

  Except

    //close the FMySql and free memory allocated by mysql_thread_init().
    if assigned(FMySql) then fLibrary.MySql_close(FMySql);
    FLibrary.mysql_thread_end;
    FMySql := nil;
    Raise;

  End;
end;

{**********************************}
procedure TalMySqlClient.Disconnect;
begin
  If not connected then exit;
  if InTransaction then TransactionRollback;
  try

    //close the FMySql and free memory allocated by mysql_thread_init().
    FLibrary.MySql_close(FMySql);
    FLibrary.mysql_thread_end;

  Except
    //Disconnect must be a "safe" procedure because it's mostly called in
    //finalization part of the code that it is not protected
  End;
  FMySql := Nil;
end;

{****************************************}
procedure TalMySqlClient.TransactionStart;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');
  if InTransaction then raise Exception.Create('Another transaction is active');

  //execute the query
  UpdateData('START TRANSACTION');
  finTransaction := True;

end;

{*****************************************}
procedure TalMySqlClient.TransactionCommit;
begin

  //Error if we are not connected
  if not InTransaction then raise Exception.Create('No active transaction to commit');

  //Execute the Query
  UpdateData('COMMIT');
  finTransaction := False;

end;

{*******************************************}
procedure TalMySqlClient.TransactionRollback;
begin

  //Error if we are not connected
  if not InTransaction then raise Exception.Create('No active transaction to rollback');

  //Execute the Query
  Try
    UpdateData('ROLLBACK');
  Except
    //some error can happen if the network go down for exemple
    //i don't really know what to do in this case of error
    //but what else we can do ? commit => exept => rollback => except ???
  End;
  finTransaction := False;

end;

{**************************************************************}
procedure TalMySqlClient.OnSelectDataDone(const SQL: AnsiString;
                                          const RowTag: AnsiString;
                                          const ViewTag: AnsiString;
                                          Skip: integer;
                                          First: Integer;
                                          CacheThreshold: Integer;
                                          TimeTaken: Double);
begin
  // virtual
end;

{**************************************************************}
procedure TalMySqlClient.OnUpdateDataDone(const SQL: AnsiString;
                                          TimeTaken: double);
begin
  // virtual
end;

{********************************************************}
procedure TalMySqlClient.SelectData(const SQL: AnsiString;
                                    const RowTag: AnsiString;
                                    const ViewTag: AnsiString;
                                    Skip: integer; // used only if value is > 0
                                    First: Integer; // used only if value is > 0
                                    CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                             // cache or not. Values <= 0 deactivate the cache
                                    XMLDATA: TalXMLNode;
                                    OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                    ExtData: Pointer;
                                    const FormatSettings: TALFormatSettings);
Var LMySqlRes: PMYSQL_RES;
    LMySqlRow: PMYSQL_ROW;
    LMySqlFields: array of PMYSQL_FIELD;
    LMySqlFieldLengths: PMYSQL_LENGTHS;
    LColumnCount: Integer;
    LColumnIndex: integer;
    LNewRec: TalXmlNode;
    LValueRec: TalXmlNode;
    LViewRec: TalXmlNode;
    LRecIndex: integer;
    LRecAdded: integer;
    LContinue: Boolean;
    LXmlDocument: TalXmlDocument;
    LUpdateRowTagByFieldValue: Boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;
    LTmpRowTag: ansiString;

begin

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

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LXmlDocument)) and
       ((XMLdata.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(RowTag + '#' +
                                    alinttostr(Skip) + '#' +
                                    alinttostr(First) + '#' +
                                    ALGetFormatSettingsID(FormatSettings) + '#' +
                                    SQL);
      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := XMLdata.AddChild(ViewTag)
        else LViewRec := XMLdata;

        //assign the tmp data to the XMLData
        LViewRec.LoadFromXML(LCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //start the TstopWatch
    LStopWatch.Reset;
    LStopWatch.Start;

    //prepare the query
    CheckAPIError(fLibrary.mysql_real_query(fMySQL, PAnsiChar(SQL), length(SQL)) <> 0);
    LMySqlRes := fLibrary.mysql_use_result(fMySQL);
    CheckAPIError(LMySqlRes = nil);
    Try

      //Returns the number of columns in a result set.
      LColumnCount := fLibrary.mysql_num_fields(LMySqlRes);

      //init the aMySqlFields array
      //this not work anymore in MYSQL5.5, i don't know why so i use mysql_fetch_field instead
      //aMySqlFields := fLibrary.mysql_fetch_fields(aMySqlRes);
      setlength(LMySqlFields,LColumnCount);
      for LColumnIndex := 0 to LColumnCount - 1 do
        LMySqlFields[LColumnIndex] := fLibrary.mysql_fetch_field(LMySqlRes);

      //init the aViewRec
      if (ViewTag <> '') and (not assigned(LXmlDocument)) then LViewRec := XMLdata.AddChild(ViewTag)
      else LViewRec := XMLdata;

      //init aUpdateRowTagByFieldValue
      if AlPos('&>',RowTag) = 1 then begin
        LTmpRowTag := ALcopyStr(RowTag,3,maxint);
        LUpdateRowTagByFieldValue := LTmpRowTag <> '';
      end
      else begin
        LTmpRowTag := RowTag;
        LUpdateRowTagByFieldValue := False;
      end;

      //loop throught all row
      LRecIndex := 0;
      LRecAdded := 0;
      while True do begin

        //retrieve the next row. return A MYSQL_ROW structure for the next row.
        //NULL if there are no more rows to retrieve or if an error occurred.
        LMySqlRow := fLibrary.mysql_fetch_row(LMySqlRes);

        //break if no more row
        if LMySqlRow = nil then begin
          CheckAPIerror(Flibrary.mysql_errno(fMySQL) <> 0);
          break;
        end

        //download the row
        else begin

          //process if > Skip
          inc(LRecIndex);
          If LRecIndex > Skip then begin

            //init NewRec
            if (LTmpRowTag <> '') and (not assigned(LXmlDocument)) then LNewRec := LViewRec.AddChild(LTmpRowTag)
            Else LNewRec := LViewRec;

            //init aMySqlFieldLengths
            LMySqlFieldLengths := fLibrary.mysql_fetch_lengths(LMySqlRes);
            CheckAPIerror(LMySqlFieldLengths = nil);

            //loop throught all column
            For LColumnIndex := 0 to LColumnCount - 1 do begin
              LValueRec := LNewRec.AddChild(ALlowercase(LMySqlFields[LColumnIndex].name));
              if (LMySqlFields[LColumnIndex]._type in [FIELD_TYPE_TINY_BLOB,
                                                       FIELD_TYPE_MEDIUM_BLOB,
                                                       FIELD_TYPE_LONG_BLOB,
                                                       FIELD_TYPE_BLOB]) then LValueRec.ChildNodes.Add(
                                                                                                       LValueRec.OwnerDocument.CreateNode(
                                                                                                                                          GetFieldValue(LMySqlRow[LColumnIndex],
                                                                                                                                                        LMySqlFields[LColumnIndex]._type,
                                                                                                                                                        LMySqlFieldLengths[LColumnIndex],
                                                                                                                                                        FormatSettings),
                                                                                                                                          ntCData
                                                                                                                                         )
                                                                                                       )
              else LValueRec.Text := GetFieldValue(LMySqlRow[LColumnIndex],
                                                   LMySqlFields[LColumnIndex]._type,
                                                   LMySqlFieldLengths[LColumnIndex],
                                                   FormatSettings);
              if LUpdateRowTagByFieldValue and (LValueRec.NodeName=LNewRec.NodeName) then LNewRec.NodeName := ALLowerCase(LValueRec.Text);
            end;

            //handle OnNewRowFunct
            if assigned(OnNewRowFunct) then begin
              LContinue := True;
              OnNewRowFunct(LNewRec, ViewTag, ExtData, LContinue);
              if Not LContinue then Break;
            end;

            //free the node if aXmlDocument
            if assigned(LXmlDocument) then LXmlDocument.DocumentElement.ChildNodes.Clear;

            //handle the First
            inc(LRecAdded);
            If (First > 0) and (LRecAdded >= First) then Break;

          end;

        end;

      end;

    Finally
      //Frees the memory allocated to aMySqlRes
      fLibrary.mysql_free_result(LMySqlRes);
    End;

    //do the OnSelectDataDone
    LStopWatch.Stop;
    OnSelectDataDone(SQL,
                     RowTag,
                     ViewTag,
                     Skip,
                     First,
                     CacheThreshold,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //save to the cache
    If LCacheKey <> '' then begin

      //save the data
      LViewRec.SaveToXML(LCacheStr, true{SaveOnlyChildNodes});
      SaveDataToCache(LCacheKey,
                      CacheThreshold,
                      LCacheStr);

    end;

  Finally
    if assigned(LXmlDocument) then LXmlDocument.free;
  End;

end;

{********************************************************}
procedure TalMySqlClient.SelectData(const SQL: AnsiString;
                                    Skip: Integer;
                                    First: Integer;
                                    OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                    ExtData: Pointer;
                                    const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             '', // RowTag,
             '', //ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, // XMLDATA,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{********************************************************}
procedure TalMySqlClient.SelectData(const SQL: AnsiString;
                                    OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                    ExtData: Pointer;
                                    const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // XMLDATA,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{********************************************************}
procedure TalMySqlClient.SelectData(const SQL: AnsiString;
                                    const RowTag: AnsiString;
                                    Skip: Integer;
                                    First: Integer;
                                    XMLDATA: TalXMLNode;
                                    const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings);
end;

{********************************************************}
procedure TalMySqlClient.SelectData(const SQL: AnsiString;
                                    const RowTag: AnsiString;
                                    XMLDATA: TalXMLNode;
                                    const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings);
end;

{********************************************************}
procedure TalMySqlClient.SelectData(const SQL: AnsiString;
                                    XMLDATA: TalXMLNode;
                                    const FormatSettings: TALFormatSettings);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings);
end;

{*********************************************************}
procedure TalMySqlClient.UpdateData(const SQL: AnsiString);
Var LStopWatch: TStopWatch;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  LStopWatch := TstopWatch.Create;

  //start the TstopWatch
  LStopWatch.Reset;
  LStopWatch.Start;

  //do the query
  CheckAPIError(fLibrary.mysql_real_query(fMySQL, PAnsiChar(SQL), length(SQL)) <> 0);

  //do the OnUpdateDataDone
  LStopWatch.Stop;
  OnUpdateDataDone(SQL,
                   LStopWatch.Elapsed.TotalMilliseconds);

end;

{*******************************************************************}
procedure TalMySqlClient.UpdateData(const SQLs: array of AnsiString);
var I: integer;
begin
  for I := Low(SQLs) to High(SQLs) do
    UpdateData(SQLs[I]);
end;

{****************************************************}
procedure TalMySqlClient.UpdateData(SQLs: TALStrings);
var I: integer;
begin
  for I := 0 to sqls.Count - 1 do
    UpdateData(SQLs[I]);
end;

{******************************************************************}
function TalMySqlClient.insert_id(const SQL: AnsiString): ULongLong;
begin

  //if the SQL is not empty
  if SQL <> '' then begin

    //Execute the Query
    UpdateData(SQL);

    //Returns the value generated for an AUTO_INCREMENT column
    //by the previous INSERT or UPDATE statement
    Result := fLibrary.mysql_insert_id(fMySQL);

  end

  //else simply gave an 0 result
  else result := 0;

end;

{*************************************************************************}
function TalMySqlConnectionPoolClient.loadCachedData(const Key: AnsiString;
                                                     var DataStr: AnsiString): Boolean;
begin
  result := false; //virtual need to be overriden
end;

{***************************************************************************}
Procedure TalMySqlConnectionPoolClient.SaveDataToCache(const Key: ansiString;
                                                       const CacheThreshold: integer;
                                                       const DataStr: ansiString);
begin
  //virtual need to be overriden
end;

{*********************************************************************************************}
procedure TalMySqlConnectionPoolClient.CheckAPIError(ConnectionHandle: PMySql; Error: Boolean);
begin
  if Error then begin
    if assigned(ConnectionHandle) then raise EALMySqlError.Create(fLibrary.mysql_error(ConnectionHandle),
                                                                  fLibrary.mysql_errno(ConnectionHandle),
                                                                  fLibrary.mysql_SqlState(ConnectionHandle))
    else raise EALMySqlError.Create('MySql error',
                                    -1,
                                    'HY000'); // The value 'HY000' (general error) is used for unmapped error numbers.
  end;
end;

{****************************************************************}
function TalMySqlConnectionPoolClient.GetDataBaseName: AnsiString;
begin
  result := FdatabaseName;
end;

{********************************************************}
function TalMySqlConnectionPoolClient.GetHost: AnsiString;
begin
  result := fHost;
end;

{*****************************************************}
function TalMySqlConnectionPoolClient.GetPort: integer;
begin
  result := fPort;
end;

{*************************************************************************}
function TalMySqlConnectionPoolClient.GetFieldValue(aFieldValue: PAnsiChar;
                                                    aFieldType: TMysqlFieldTypes;
                                                    aFieldLength: integer;
                                                    const aFormatSettings: TALFormatSettings): AnsiString;
begin
  //The lengths of the field values in the row may be obtained by calling mysql_fetch_lengths().
  //Empty fields and fields containing NULL both have length 0; you can distinguish these
  //by checking the pointer for the field value. If the pointer is NULL, the field
  //is NULL; otherwise, the field is empty.
  IF aFieldValue = nil then result := fNullString
  else begin
    Case aFieldType of
      FIELD_TYPE_DECIMAL,
      FIELD_TYPE_NEWDECIMAL,
      FIELD_TYPE_FLOAT,
      FIELD_TYPE_DOUBLE: result := ALFloatToStr(ALStrToFloat(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_DATETIME: Result := ALDateTimeToStr(ALStrToDateTime(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_DATE,
      FIELD_TYPE_NEWDATE: Result := ALDateToStr(ALStrToDate(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_TIME: Result := ALTimeToStr(ALStrToTime(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_TIMESTAMP: Result := ALDateTimeToStr(ALStrToDateTime(aFieldValue,ALMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_NULL: result := fNullString; // Example: SELECT NULL FROM DUAL
      Else SetString(Result, aFieldValue, aFieldLength);
    end;
  end;
end;

{************************************************************************}
procedure TalMySqlConnectionPoolClient.initObject(const aHost: AnsiString;
                                                  aPort: integer;
                                                  const aDataBaseName,
                                                        aLogin,
                                                        aPassword,
                                                        aCharSet: AnsiString;
                                                  Const aOpenConnectionClientFlag: Cardinal = 0;
                                                  Const aOpenConnectionOptions: TALMySQLOptions = nil);
begin
  fHost := aHost;
  fPort := aPort;
  FDataBaseName:= aDataBaseName;
  fLogin := aLogin;
  fPassword := aPassword;
  fCharset := aCharset;
  fOpenConnectionClientFlag := aOpenConnectionClientFlag;
  FOpenConnectionOptions := aOpenConnectionOptions;
  setlength(FConnectionPool,0);
  FConnectionPoolCount := 0;
  FConnectionPoolCapacity := 0;
  FConnectionPoolCS:= TCriticalSection.create;
  FWorkingConnectionCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionGarbage := GettickCount64;
  FConnectionMaxIdleTime := 1200000; // 1000 * 60 * 20 = 20 min
  FNullString := '';
end;

{****************************************************************}
constructor TalMySqlConnectionPoolClient.Create(const aHost: AnsiString;
                                                aPort: integer;
                                                const aDataBaseName,
                                                      aLogin,
                                                      aPassword,
                                                      aCharSet: AnsiString;
                                                aApiVer: TALMySqlVersion_API;
                                                Const alib: AnsiString = 'libmysql.dll';
                                                Const aOpenConnectionClientFlag: Cardinal = 0;
                                                Const aOpenConnectionOptions: TALMySQLOptions = nil);
begin
  fLibrary := TALMySqlLibrary.Create(aApiVer);
  Try
    fLibrary.Load(alib);
    FownLibrary := True;
    initObject(aHost,
               aPort,
               aDataBaseName,
               aLogin,
               aPassword,
               aCharSet,
               aOpenConnectionClientFlag,
               aOpenConnectionOptions);
  except
    FreeandNil(fLibrary);
    raise;
  End;
end;

{**********************************************************************}
constructor TalMySqlConnectionPoolClient.Create(const aHost: AnsiString;
                                                aPort: integer;
                                                const aDataBaseName,
                                                      aLogin,
                                                      aPassword,
                                                      aCharSet: AnsiString;
                                                alib: TALMySqlLibrary;
                                                Const aOpenConnectionClientFlag: Cardinal = 0;
                                                Const aOpenConnectionOptions: TALMySQLOptions = nil);
begin
  fLibrary := alib;
  FownLibrary := False;
  initObject(aHost,
             aPort,
             aDataBaseName,
             aLogin,
             aPassword,
             aCharSet,
             aOpenConnectionClientFlag,
             aOpenConnectionOptions);
end;

{**********************************************}
destructor TalMySqlConnectionPoolClient.Destroy;
begin

  //Release all connections
  if assigned(fLibrary) then ReleaseAllConnections;

  //free object
  FConnectionPoolCS.free;
  if FownLibrary and assigned(fLibrary) then fLibrary.Free;

  //inherite
  inherited;

end;

{*******}
ThreadVar
  _ThreadInitRefCount: Integer;

{**************************************************************}
function TalMySqlConnectionPoolClient.AcquireConnection: PMySql;
Var LTickCount: int64;
    i: integer;
Begin

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    //MYSQL IT's A SHEET
    //http://bugs.mysql.com/bug.php?id=66740
    //https://bugzilla.redhat.com/show_bug.cgi?id=846602
    //MySQL requires that each thread that uses the MySQL API first call
    //mysql_thread_init() and at the end call mysql_thread_end(). If the thread
    //fails to call mysql_thread_end(), then MySQL will block the main thread at
    //program termination and wait for this threads to call mysql_thread_end().
    //If that doesn't happen, then it prints an error message to STDERR.
    //Not a very user-friendly behavior.
    //
    // This function must be called early within each
    // created thread to initialize thread-specific variables
    // this look very important because if we comment this and
    // the mysql_thread_end then the Flibrary.unload can take several seconds
    // you can see this in the ALSQLBenchmark.exe project with the loop update button
    // http://dev.mysql.com/doc/refman/5.6/en/threaded-clients.html
    CheckAPIError(Nil, FLibrary.mysql_thread_init <> 0);
    inc(_ThreadInitRefCount);
    Try

      //raise an exception if currently realeasing all connection
      if FReleasingAllconnections then raise Exception.Create('Can not acquire connection: currently releasing all connections');

      //delete the old unused connection
      LTickCount := GettickCount64;
      if LTickCount - fLastConnectionGarbage > (60000 {every minutes})  then begin
        while FConnectionPoolCount > 0 do begin
          if LTickCount - FConnectionPool[0].Lastaccessdate > FConnectionMaxIdleTime then begin

            Try
              FLibrary.MySql_close(FConnectionPool[0].ConnectionHandle);
            Except
              //Disconnect must be a "safe" procedure because it's mostly called in
              //finalization part of the code that it is not protected
              //that the bulsheet of MySql to answer SQLITE_BUSY instead of free
              //everything
            End;

            Dec(FConnectionPoolCount);
            if  FConnectionPoolCount > 0 then
            begin
              System.Move(FConnectionPool[1], FConnectionPool[0],
                (FConnectionPoolCount) * SizeOf(TalMySqlConnectionPoolContainer));
            end;

          end
          else break;
        end;
        FLastConnectionGarbage := LTickCount;
      end;

      //acquire the new connection from the pool
      If FConnectionPoolCount > 0 then begin
        Result := FConnectionPool[FConnectionPoolCount - 1].ConnectionHandle;
        Dec(FConnectionPoolCount);
      end

      //create a new connection
      else begin
        result := nil;
        Try

          //Allocates or initializes a MYSQL object suitable for mysql_real_connect()
          Result := fLibrary.mysql_init(nil);
          CheckAPIError(nil, result = nil);

          //set the The name of the character set to use as the default character set.
          If (fCharSet <> '') then CheckAPIError(Result, fLibrary.mysql_options(Result, MYSQL_SET_CHARSET_NAME, PAnsiChar(fCharSet)) <> 0);

          // set the options if they are existing
          for i := 0 to length(FOpenConnectionOptions) - 1 do
            CheckAPIError(Result, FLibrary.mysql_options(Result,
                                                         FOpenConnectionOptions[i].Option,
                                                         FOpenConnectionOptions[i].Value) <> 0);

          //attempts to establish a connection to a MySQL database engine running on host
          CheckAPIError(Result, fLibrary.mysql_real_connect(Result,
                                                            PAnsiChar(Host),
                                                            PAnsiChar(fLogin),
                                                            PAnsiChar(fPassword),
                                                            PAnsiChar(fDatabaseName),
                                                            Port,
                                                            nil,
                                                            fOpenConnectionClientFlag) = nil);

        Except

          //close the FMySql
          if assigned(Result) then fLibrary.MySql_close(Result);
          Raise;

        End;
      end;

      //increase the connection count
      inc(FWorkingConnectionCount);

    Except

      // free memory allocated by mysql_thread_init().
      dec(_ThreadInitRefCount);
      if _ThreadInitRefCount = 0 then FLibrary.mysql_thread_end;
      Raise;

    End;

  //get out of the synchronization
  finally
    FConnectionPoolCS.Release;
  end;

End;

{***************************************************************************************}
{Applications must finalize all prepared statements and close all BLOB handles associated
 with the MySql object prior to attempting to close the object. If MySql_close() is
 called on a database connection that still has outstanding prepared statements or
 BLOB handles, then it returns SQLITE_BUSY.
 If MySql_close() is invoked while a transaction is open, the transaction is
 automatically rolled back.}
procedure TalMySqlConnectionPoolClient.ReleaseConnection(var ConnectionHandle: PMySql; const CloseConnection: Boolean = False);
begin

  //security check
  if not assigned(ConnectionHandle) then raise Exception.Create('Connection handle can not be null');

  //release the connection
  FConnectionPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      if FConnectionPoolCount = FConnectionPoolCapacity then begin
        if FConnectionPoolCapacity > 64 then FConnectionPoolCapacity := FConnectionPoolCapacity + (FConnectionPoolCapacity div 4) else
          if FConnectionPoolCapacity > 8 then FConnectionPoolCapacity := FConnectionPoolCapacity + 16 else
            FConnectionPoolCapacity := FConnectionPoolCapacity + 4;
        SetLength(FConnectionPool, FConnectionPoolCapacity);
      end;
      FConnectionPool[FConnectionPoolCount].ConnectionHandle := ConnectionHandle;
      FConnectionPool[FConnectionPoolCount].LastAccessDate := GettickCount64;
      Inc(FConnectionPoolCount);
    end

    //close the connection
    else begin
      try
        FLibrary.MySql_close(ConnectionHandle);
      Except
        //Disconnect must be a "safe" procedure because it's mostly called in
        //finalization part of the code that it is not protected
      end;
    end;

    //set the connectionhandle to nil
    ConnectionHandle := nil;

    //dec the WorkingConnectionCount
    Dec(FWorkingConnectionCount);

    // free memory allocated by mysql_thread_init().
    // http://dev.mysql.com/doc/refman/5.6/en/threaded-clients.html
    Try
      dec(_ThreadInitRefCount);
      if _ThreadInitRefCount = 0 then FLibrary.mysql_thread_end;
    Except
      //Disconnect must be a "safe" procedure because it's mostly called in
      //finalization part of the code that it is not protected
    End;

  finally
    FConnectionPoolCS.Release;
  end;

end;

{*********************************************************************************************************}
procedure TalMySqlConnectionPoolClient.ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True);
begin

  //i m still not sure if the FLibrary.MySql_close
  //need the FLibrary.mysql_thread_init. the mysql doc
  //if a very true bullsheet. i think it's cost
  //nothing to call it here. but of course this mean that
  //releaseconnection could not be call inside a thread
  //that still own a connection because at the end of this
  //function we call mysql_thread_end
  try
    FLibrary.mysql_thread_init;
    inc(_ThreadInitRefCount);
  except
    //must be safe
  end;

  try

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
          sleep(1);
        end;

      {free all database}
      FConnectionPoolCS.Acquire;
      Try
        while FConnectionPoolCount > 0 do begin
          Try
            FLibrary.MySql_close(FConnectionPool[FConnectionPoolcount - 1].ConnectionHandle);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;
          Dec(FConnectionPoolCount);
        end;
        FLastConnectionGarbage := GettickCount64;
      finally
        FConnectionPoolCS.Release;
      end;

    finally
      //Do not forbid anymore new thread to create a new transaction
      FReleasingAllconnections := False;
    End;

  finally
    try
      dec(_ThreadInitRefCount);
      if _ThreadInitRefCount = 0 then FLibrary.mysql_thread_end;
    except
      //must be safe
    end;
  end;
end;

{************************************************************************************}
procedure TalMySqlConnectionPoolClient.TransactionStart(Var ConnectionHandle: PMySql);
begin

  //ConnectionHandle must be null
  if assigned(ConnectionHandle) then raise Exception.Create('Connection handle must be null');

  //init the aConnectionHandle
  ConnectionHandle := AcquireConnection;
  try

    //start the transaction
    UpdateData('START TRANSACTION', ConnectionHandle);

  except
    ReleaseConnection(ConnectionHandle, True);
    raise;
  end;

end;

{*****************************************************************************************************************************}
procedure TalMySqlConnectionPoolClient.TransactionCommit(var ConnectionHandle: PMySql; const CloseConnection: Boolean = False);
begin

  //security check
  if not assigned(ConnectionHandle) then raise Exception.Create('Connection handle can not be null');

  //commit the transaction
  UpdateData('COMMIT', ConnectionHandle);

  //release the connection
  ReleaseConnection(ConnectionHandle, CloseConnection);

end;

{*******************************************************************************************************************************}
procedure TalMySqlConnectionPoolClient.TransactionRollback(var ConnectionHandle: PMySql; const CloseConnection: Boolean = False);
var LTmpCloseConnection: Boolean;
begin

  //security check
  if not assigned(ConnectionHandle) then raise Exception.Create('Connection handle can not be null');

  //rollback the connection
  LTmpCloseConnection := CloseConnection;
  Try
    Try
      UpdateData('ROLLBACK', ConnectionHandle);
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
    ReleaseConnection(ConnectionHandle, LTmpCloseConnection);

  End;

end;

{****************************************************************************}
procedure TalMySqlConnectionPoolClient.OnSelectDataDone(const SQL: AnsiString;
                                                        const RowTag: AnsiString;
                                                        const ViewTag: AnsiString;
                                                        Skip: integer;
                                                        First: Integer;
                                                        CacheThreshold: Integer;
                                                        TimeTaken: double);
begin
  // virtual
end;

{****************************************************************************}
procedure TalMySqlConnectionPoolClient.OnUpdateDataDone(const SQL: AnsiString;
                                                        TimeTaken: double);
begin
  // virtual
end;

{**********************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                  const RowTag: AnsiString;
                                                  const ViewTag: AnsiString;
                                                  Skip: integer; // used only if value is > 0
                                                  First: Integer; // used only if value is > 0
                                                  CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                                           // cache or not. Values <= 0 deactivate the cache
                                                  XMLDATA: TalXMLNode;
                                                  OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                                  ExtData: Pointer;
                                                  const FormatSettings: TALFormatSettings;
                                                  const ConnectionHandle: PMySql = nil);

Var LMySqlRes: PMYSQL_RES;
    LMySqlRow: PMYSQL_ROW;
    LMySqlFields: array of PMYSQL_FIELD;
    LMySqlFieldLengths: PMYSQL_LENGTHS;
    LColumnCount: Integer;
    LColumnIndex: integer;
    LNewRec: TalXmlNode;
    LValueRec: TalXmlNode;
    LViewRec: TalXmlNode;
    LRecIndex: integer;
    LRecAdded: integer;
    LTmpConnectionHandle: PMySql;
    LOwnConnection: Boolean;
    LContinue: Boolean;
    LXmlDocument: TalXmlDocument;
    LUpdateRowTagByFieldValue: Boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;
    LTmpRowTag: ansiString;

begin

  //only OnNewRowFunct / XMLDATA can be used
  if assigned(OnNewRowFunct) then XMLDATA := nil;

  //clear the XMLDATA
  if assigned(XMLDATA) then LXmlDocument := Nil
  else begin
    LXmlDocument := TALXmlDocument.create('root');
    XMLDATA := LXmlDocument.DocumentElement;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LXmlDocument)) and
       ((XMLdata.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(RowTag + '#' +
                                    alinttostr(Skip) + '#' +
                                    alinttostr(First) + '#' +
                                    ALGetFormatSettingsID(FormatSettings) + '#' +
                                    SQL);
      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := XMLdata.AddChild(ViewTag)
        else LViewRec := XMLdata;

        //assign the tmp data to the XMLData
        LViewRec.LoadFromXML(LCacheStr, true{XmlContainOnlyChildNodes}, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //acquire a connection and start the transaction if necessary
    LTmpConnectionHandle := ConnectionHandle;
    LOwnConnection := (not assigned(ConnectionHandle));
    if LOwnConnection then TransactionStart(LTmpConnectionHandle);
    Try

      //start the TstopWatch
      LStopWatch.Reset;
      LStopWatch.Start;

      //prepare the query
      CheckAPIError(LTmpConnectionHandle, fLibrary.mysql_real_query(LTmpConnectionHandle, PAnsiChar(SQL), length(SQL)) <> 0);
      LMySqlRes := fLibrary.mysql_use_result(LTmpConnectionHandle);
      CheckAPIError(LTmpConnectionHandle, LTmpConnectionHandle = nil);
      Try

        //Returns the number of columns in a result set.
        LColumnCount := fLibrary.mysql_num_fields(LMySqlRes);

        //init the aMySqlFields array
        //this not work anymore in MYSQL5.5, i don't know why so i use mysql_fetch_field instead
        //aMySqlFields := fLibrary.mysql_fetch_fields(aMySqlRes);
        setlength(LMySqlFields,LColumnCount);
        for LColumnIndex := 0 to LColumnCount - 1 do
          LMySqlFields[LColumnIndex] := fLibrary.mysql_fetch_field(LMySqlRes);

        //init the aViewRec
        if (ViewTag <> '') and (not assigned(LXmlDocument)) then LViewRec := XMLdata.AddChild(ViewTag)
        else LViewRec := XMLdata;

        //init aUpdateRowTagByFieldValue
        if AlPos('&>',RowTag) = 1 then begin
          LTmpRowTag := ALcopyStr(RowTag,3,maxint);
          LUpdateRowTagByFieldValue := LTmpRowTag <> '';
        end
        else begin
          LTmpRowTag := RowTag;
          LUpdateRowTagByFieldValue := False;
        end;

        //loop throught all row
        LRecIndex := 0;
        LRecAdded := 0;
        while True do begin

          //retrieve the next row. return A MYSQL_ROW structure for the next row.
          //NULL if there are no more rows to retrieve or if an error occurred.
          LMySqlRow := fLibrary.mysql_fetch_row(LMySqlRes);

          //break if no more row
          if LMySqlRow = nil then begin
            CheckAPIerror(LTmpConnectionHandle, Flibrary.mysql_errno(LTmpConnectionHandle) <> 0);
            break;
          end

          //download the row
          else begin

            //process if > Skip
            inc(LRecIndex);
            If LRecIndex > Skip then begin

              //init NewRec
              if (LTmpRowTag <> '') and (not assigned(LXmlDocument)) then LNewRec := LViewRec.AddChild(LTmpRowTag)
              Else LNewRec := LViewRec;

              //init aMySqlFieldLengths
              LMySqlFieldLengths := fLibrary.mysql_fetch_lengths(LMySqlRes);
              CheckAPIerror(LTmpConnectionHandle, LMySqlFieldLengths = nil);

              //loop throught all column
              For LColumnIndex := 0 to LColumnCount - 1 do begin
                LValueRec := LNewRec.AddChild(ALlowercase(LMySqlFields[LColumnIndex].name));
                if (LMySqlFields[LColumnIndex]._type in [FIELD_TYPE_TINY_BLOB,
                                                         FIELD_TYPE_MEDIUM_BLOB,
                                                         FIELD_TYPE_LONG_BLOB,
                                                         FIELD_TYPE_BLOB]) then LValueRec.ChildNodes.Add(
                                                                                                         LValueRec.OwnerDocument.CreateNode(
                                                                                                                                            GetFieldValue(LMySqlRow[LColumnIndex],
                                                                                                                                                          LMySqlFields[LColumnIndex]._type,
                                                                                                                                                          LMySqlFieldLengths[LColumnIndex],
                                                                                                                                                          FormatSettings),
                                                                                                                                            ntCData
                                                                                                                                           )
                                                                                                         )
                else LValueRec.Text := GetFieldValue(LMySqlRow[LColumnIndex],
                                                     LMySqlFields[LColumnIndex]._type,
                                                     LMySqlFieldLengths[LColumnIndex],
                                                     FormatSettings);
                if LUpdateRowTagByFieldValue and (LValueRec.NodeName=LNewRec.NodeName) then LNewRec.NodeName := ALLowerCase(LValueRec.Text);
              end;

              //handle OnNewRowFunct
              if assigned(OnNewRowFunct) then begin
                LContinue := True;
                OnNewRowFunct(LNewRec, ViewTag, ExtData, LContinue);
                if Not LContinue then Break;
              end;

              //free the node if aXmlDocument
              if assigned(LXmlDocument) then LXmlDocument.DocumentElement.ChildNodes.Clear;

              //handle the First
              inc(LRecAdded);
              If (First > 0) and (LRecAdded >= First) then Break;

            end;

          end;

        end;

      Finally
        //Frees the memory allocated to aMySqlRes
        fLibrary.mysql_free_result(LMySqlRes);
      End;

      //do the OnSelectDataDone
      LStopWatch.Stop;
      OnSelectDataDone(SQL,
                       RowTag,
                       ViewTag,
                       Skip,
                       First,
                       CacheThreshold,
                       LStopWatch.Elapsed.TotalMilliseconds);

      //save to the cache
      If LCacheKey <> '' then begin

        //save the data
        LViewRec.SaveToXML(LCacheStr, true{SaveOnlyChildNodes});
        SaveDataToCache(LCacheKey,
                        CacheThreshold,
                        LCacheStr);

      end;

      //commit the transaction and release the connection if owned
      if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

    except
      on E: Exception do begin

        // rollback the transaction and release the connection if owned
        // TODO
        // instead of closing the connection, it's could be better to know
        // if the error is related to the connection, or related to the
        // SQL (like we do in alFBXclient with GetCloseConnectionByErrCode
        if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

        //raise the error
        raise;

      end;
    end;

  finally
    if assigned(LXmlDocument) then LXmlDocument.free;
  end;

end;

{**********************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                  Skip: Integer;
                                                  First: Integer;
                                                  OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                                  ExtData: Pointer;
                                                  const FormatSettings: TALFormatSettings;
                                                  const ConnectionHandle: PMySql = nil);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, // XMLDATA,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{**********************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                  OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                                  ExtData: Pointer;
                                                  const FormatSettings: TALFormatSettings;
                                                  const ConnectionHandle: PMySql = nil);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // XMLDATA,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{**********************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                  const RowTag: AnsiString;
                                                  Skip: Integer;
                                                  First: Integer;
                                                  XMLDATA: TalXMLNode;
                                                  const FormatSettings: TALFormatSettings;
                                                  const ConnectionHandle: PMySql = nil);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{**********************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                  const RowTag: AnsiString;
                                                  XMLDATA: TalXMLNode;
                                                  const FormatSettings: TALFormatSettings;
                                                  const ConnectionHandle: PMySql = nil);
begin
  SelectData(SQL,
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{**********************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(const SQL: AnsiString;
                                                  XMLDATA: TalXMLNode;
                                                  const FormatSettings: TALFormatSettings;
                                                  const ConnectionHandle: PMySql = nil);
begin
  SelectData(SQL,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             XMLDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{*************************************************************************************************************}
procedure TalMySqlConnectionPoolClient.UpdateData(const SQL: AnsiString; const ConnectionHandle: PMySql = nil);
Var LTmpConnectionHandle: PMySql;
    LOwnConnection: Boolean;
    LStopWatch: TStopWatch;
begin

  //acquire a connection and start the transaction if necessary
  LTmpConnectionHandle := ConnectionHandle;
  LOwnConnection := (not assigned(ConnectionHandle));
  if LOwnConnection then TransactionStart(LTmpConnectionHandle);
  Try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //start the TstopWatch
    LStopWatch.Reset;
    LStopWatch.Start;

    //do the query
    CheckAPIError(LTmpConnectionHandle, fLibrary.mysql_real_query(LTmpConnectionHandle, PAnsiChar(SQL), length(SQL)) <> 0);

    //do the OnUpdateDataDone
    LStopWatch.Stop;
    OnUpdateDataDone(SQL,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //commit the transaction and release the connection if owned
    if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

  except
    On E: Exception do begin

      // rollback the transaction and release the connection if owned
      // TODO
      // instead of closing the connection, it's could be better to know
      // if the error is related to the connection, or related to the
      // SQL (like we do in alFBXclient with GetCloseConnectionByErrCode
      if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{********************************************************************************}
procedure TalMySqlConnectionPoolClient.UpdateData(const SQLs: array of AnsiString;
                                                  const ConnectionHandle: PMySql = nil);
Var LTmpConnectionHandle: PMySql;
    LOwnConnection: Boolean;
    I: integer;
begin

  //acquire a connection and start the transaction if necessary
  LTmpConnectionHandle := ConnectionHandle;
  LOwnConnection := (not assigned(ConnectionHandle));
  if LOwnConnection then TransactionStart(LTmpConnectionHandle);
  Try

    //update the data
    for I := Low(SQLs) to High(SQLs) do
      UpdateData(SQLs[I],
                 LTmpConnectionHandle);

    //commit the transaction and release the connection if owned
    if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

  except
    On E: Exception do begin

      // rollback the transaction and release the connection if owned
      // TODO
      // instead of closing the connection, it's could be better to know
      // if the error is related to the connection, or related to the
      // SQL (like we do in alFBXclient with GetCloseConnectionByErrCode
      if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{*****************************************************************}
procedure TalMySqlConnectionPoolClient.UpdateData(SQLs: TALStrings;
                                                  const ConnectionHandle: PMySql = nil);
Var LTmpConnectionHandle: PMySql;
    LOwnConnection: Boolean;
    I: integer;
begin

  //acquire a connection and start the transaction if necessary
  LTmpConnectionHandle := ConnectionHandle;
  LOwnConnection := (not assigned(ConnectionHandle));
  if LOwnConnection then TransactionStart(LTmpConnectionHandle);
  Try

    //update the data
    for I := 0 to SQLs.Count - 1 do
      UpdateData(SQLs[I],
                 LTmpConnectionHandle);

    //commit the transaction and release the connection if owned
    if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

  except
    On E: Exception do begin

      // rollback the transaction and release the connection if owned
      // TODO
      // instead of closing the connection, it's could be better to know
      // if the error is related to the connection, or related to the
      // SQL (like we do in alFBXclient with GetCloseConnectionByErrCode
      if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{**********************************************************************************************************************}
function TalMySqlConnectionPoolClient.insert_id(const SQL: AnsiString; const ConnectionHandle: PMySql = nil): ULongLong;
Var LTmpConnectionHandle: PMySql;
    LOwnConnection: Boolean;
begin

  //acquire a connection and start the transaction if necessary
  LTmpConnectionHandle := ConnectionHandle;
  LOwnConnection := (not assigned(ConnectionHandle));
  if LOwnConnection then TransactionStart(LTmpConnectionHandle);
  Try

    //if the SQL is not empty
    if SQL <> '' then begin

      //do the query
      UpdateData(SQL, LTmpConnectionHandle);

      //Returns the value generated for an AUTO_INCREMENT column
      //by the previous INSERT or UPDATE statement
      Result := fLibrary.mysql_insert_id(LTmpConnectionHandle);

    end

    //if the SQL is empty, simply gave an 0 result
    else result := 0;

    //commit the transaction and release the connection if owned
    if LOwnConnection then TransactionCommit(LTmpConnectionHandle);

  except
    On E: Exception do begin

      // rollback the transaction and release the connection if owned
      // TODO
      // instead of closing the connection, it's could be better to know
      // if the error is related to the connection, or related to the
      // SQL (like we do in alFBXclient with GetCloseConnectionByErrCode
      if LOwnConnection then TransactionRollback(LTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{*************************************************************}
function TalMySqlConnectionPoolClient.ConnectionCount: Integer;
begin
  FConnectionPoolCS.Acquire;
  Try
    Result := FConnectionPoolCount + FWorkingConnectionCount;
  finally
    FConnectionPoolCS.Release;
  end;
end;

{********************************************************************}
function TalMySqlConnectionPoolClient.WorkingConnectionCount: Integer;
begin
  FConnectionPoolCS.Acquire;
  Try
    Result := FWorkingConnectionCount;
  finally
    FConnectionPoolCS.Release;
  end;
end;

initialization
  ALMySQLFormatSettings := TALFormatSettings.Create('en-US'); // 1033 {en-US}
  ALMySQLFormatSettings.DecimalSeparator := '.';
  ALMySQLFormatSettings.ThousandSeparator := ',';
  ALMySQLFormatSettings.DateSeparator := '-';
  ALMySQLFormatSettings.TimeSeparator := ':';
  ALMySQLFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  ALMySQLFormatSettings.ShortTimeFormat := 'hh:nn:ss';

end.
