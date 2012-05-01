{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALMySqlClient
Version:      3.53

Description:  An Object to query MySql Server Version 5 and get
              the Result In Xml Stream

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

Know bug :    30/01/2008: Correct memory leak bug.

History :

Link :        http://dev.mysql.com/doc/refman/5.0/en/
              http://dev.mysql.com/doc/refman/5.0/en/string-syntax.html

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  voting on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit AlMySqlClient;

interface

uses Windows,
     SysUtils,
     classes,
     Contnrs,
     SyncObjs,
     AlXmlDoc,
     AlMySqlWrapper;

Type

  {-----------------------------------------------------------------------}
  TalMySqlClientSelectDataOnNewRowFunct = Procedure(XMLRowData: TalXmlNode;
                                                    ViewTag: String;
                                                    ExtData: Pointer;
                                                    Var Continue: Boolean);

  {------------------------------}
  EALMySqlError = class(Exception)
  private
    FErrorCode: Integer;
    FSQLstate: String;
  public
    constructor Create(aErrorMsg: string;
                       aErrorCode: Integer;
                       aSqlState: String); overload;
    property ErrorCode: Integer read FErrorCode;
    property SQLState: string read FSQLState;
  end;

  {----------------------------------}
  TalMySqlClientSelectDataSQL = record
    SQL: String;
    RowTag: String;
    ViewTag: String;
    Skip: integer;
    First: Integer;
  end;
  TalMySqlClientSelectDataSQLs = array of TalMySqlClientSelectDataSQL;

  {----------------------------------}
  TalMySqlClientUpdateDataSQL = record
    SQL: String;
  end;
  TalMySqlClientUpdateDataSQLs = array of TalMySqlClientUpdateDataSQL;

  {-----------------------------}
  TalMySqlClient = Class(Tobject)
  Private
    fLibrary: TALMySqlLibrary;
    FownLibrary: Boolean;
    fMySql: PMySql;
    fNullString: String;
    finTransaction: Boolean;
    fMySQLFormatSettings: TformatSettings;
    function  GetConnected: Boolean;
    function  GetInTransaction: Boolean;
  Protected
    procedure CheckAPIError(Error: Boolean);
    Function  GetFieldValue(aFieldValue: Pchar;
                            aFieldType: TMysqlFieldTypes;
                            aFieldLength: integer;
                            aFormatSettings: TformatSettings): String;
    procedure initObject; virtual;
  Public
    Constructor Create(ApiVer: TALMySqlVersion_API;
                       const lib: String = 'libmysql.dll'); overload; virtual;
    Constructor Create(lib: TALMySqlLibrary); overload; virtual;
    Destructor Destroy; Override;
    Procedure Connect(Host: String;
                      Port: integer;
                      DataBaseName,
                      Login,
                      Password,
                      CharSet: String;
                      Const ClientFlag: Cardinal = 0); virtual;
    Procedure Disconnect;
    Procedure TransactionStart;
    Procedure TransactionCommit;
    Procedure TransactionRollback;
    Procedure SelectData(SQLs: TalMySqlClientSelectDataSQLs;
                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         FormatSettings: TformatSettings); overload;
    Procedure SelectData(SQL: TalMySqlClientSelectDataSQL;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         FormatSettings: TformatSettings); overload;
    Procedure SelectData(SQL: String;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         FormatSettings: TformatSettings); overload;
    Procedure SelectData(SQL: String;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         FormatSettings: TformatSettings); overload;
    Procedure SelectData(SQLs: TalMySqlClientSelectDataSQLs;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings); overload;
    Procedure SelectData(SQL: TalMySqlClientSelectDataSQL;
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
    procedure UpdateData(SQLs: TalMySqlClientUpdateDataSQLs); overload;
    procedure UpdateData(SQL: TalMySqlClientUpdateDataSQL); overload;
    procedure UpdateData(SQLs: Tstrings); overload;
    procedure UpdateData(SQL: String); overload;
    procedure UpdateData(SQLs: array of string); overload;
    function  insert_id(SQL: String): ULongLong;
    Property  Connected: Boolean Read GetConnected;
    Property  InTransaction: Boolean read GetInTransaction;
    Property  NullString: String Read fNullString Write fNullString;
    property  Lib: TALMySqlLibrary read FLibrary;
  end;

  {------------------------------------------------}
  TalMySqlConnectionPoolContainer = Class(TObject)
    ConnectionHandle: PMySql;
    LastAccessDate: int64;
  End;

  {---------------------------------------------}
  TalMySqlConnectionPoolClient = Class(Tobject)
  Private
    FLibrary: TALMySqlLibrary;
    FownLibrary: Boolean;
    FConnectionPool: TObjectList;
    FConnectionPoolCS: TCriticalSection;
    FWorkingConnectionCount: Integer;
    FReleasingAllconnections: Boolean;
    FLastConnectionGarbage: Int64;
    FConnectionMaxIdleTime: integer;
    FHost: String;
    FPort: Integer;
    FDataBaseName: String;
    fLogin: String;
    fPassword: String;
    fCharset: String;
    fOpenConnectionClientFlag: cardinal;
    FNullString: String;
    fMySQLFormatSettings: TformatSettings;
  Protected
    procedure CheckAPIError(ConnectionHandle: PMySql; Error: Boolean);
    function  GetDataBaseName: String; virtual;
    function  GetHost: String; virtual;
    function  GetPort: integer; virtual;
    Function  GetFieldValue(aFieldValue: Pchar;
                            aFieldType: TMysqlFieldTypes;
                            aFieldLength: integer;
                            aFormatSettings: TformatSettings): String;
    Function  AcquireConnection: PMySql; virtual;
    Procedure ReleaseConnection(var ConnectionHandle: PMySql;
                                const CloseConnection: Boolean = False); virtual;
    procedure initObject(aHost: String;
                         aPort: integer;
                         aDataBaseName,
                         aLogin,
                         aPassword,
                         aCharSet: String;
                         Const aOpenConnectionClientFlag: Cardinal = 0); virtual;
  Public
    Constructor Create(aHost: String;
                       aPort: integer;
                       aDataBaseName,
                       aLogin,
                       aPassword,
                       aCharSet: String;
                       aApiVer: TALMySqlVersion_API;
                       const alib: String = 'libmysql.dll';
                       Const aOpenConnectionClientFlag: Cardinal = 0); overload; virtual;
    Constructor Create(aHost: String;
                       aPort: integer;
                       aDataBaseName,
                       aLogin,
                       aPassword,
                       aCharSet: String;
                       alib: TALMySqlLibrary;
                       Const aOpenConnectionClientFlag: Cardinal = 0); overload; virtual;
    Destructor  Destroy; Override;
    Procedure ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True); virtual;
    Procedure TransactionStart(Var ConnectionHandle: PMySql); virtual;
    Procedure TransactionCommit(var ConnectionHandle: PMySql;
                                const CloseConnection: Boolean = False); virtual;
    Procedure TransactionRollback(var ConnectionHandle: PMySql;
                                  const CloseConnection: Boolean = False); virtual;
    Procedure SelectData(SQLs: TalMySqlClientSelectDataSQLs;
                         XMLDATA: TalXMLNode;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         FormatSettings: TformatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(SQL: TalMySqlClientSelectDataSQL;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         FormatSettings: TformatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(SQL: String;
                         Skip: integer;
                         First: Integer;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         FormatSettings: TformatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(SQL: String;
                         OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         FormatSettings: TformatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(SQLs: TalMySqlClientSelectDataSQLs;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(SQL: TalMySqlClientSelectDataSQL;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(SQL: String;
                         RowTag: String;
                         Skip: integer;
                         First: Integer;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(SQL: String;
                         RowTag: String;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Procedure SelectData(SQL: String;
                         XMLDATA: TalXMLNode;
                         FormatSettings: TformatSettings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    procedure UpdateData(SQLs: TalMySqlClientUpdateDataSQLs;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    procedure UpdateData(SQL: TalMySqlClientUpdateDataSQL;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    procedure UpdateData(SQLs: Tstrings;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    procedure UpdateData(SQL: String;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    procedure UpdateData(SQLs: Array of String;
                         const ConnectionHandle: PMySql = nil); overload; virtual;
    Function  insert_id(SQL: String;
                        const ConnectionHandle: PMySql = nil): UlongLong; virtual;
    Function  ConnectionCount: Integer;
    Function  WorkingConnectionCount: Integer;
    property  DataBaseName: String read GetDataBaseName;
    property  Host: String read GetHost;
    property  Port: integer read GetPort;
    property  ConnectionMaxIdleTime: integer read FConnectionMaxIdleTime write fConnectionMaxIdleTime;
    Property  NullString: String Read fNullString Write fNullString;
    property  Lib: TALMySqlLibrary read FLibrary;
  end;

Function AlMySqlClientSlashedStr(Const Str: String): String;

implementation

Uses ALWindows,
     AlFcnString;

{**********************************************************}
Function AlMySqlClientSlashedStr(Const Str: String): String;
var I: Integer;
begin
  Result := Str;
  for I := Length(Result) downto 1 do
    if Result[I] in ['''','"','\',#0] then Insert('\', Result, I);
  Result := '''' + Result + '''';
end;



/////////////////////////
///// EALMySqlError /////
/////////////////////////

{*************************************************}
constructor EALMySqlError.Create(aErrorMsg: string;
                                 aErrorCode: Integer;
                                 aSqlState: String);
begin
  fErrorCode := aErrorCode;
  FSQLstate := aSqlState;
  inherited create(aErrorMsg);
end;


//////////////////////////
///// TalMySqlClient /////
//////////////////////////

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

{*******************************************************}
function TalMySqlClient.GetFieldValue(aFieldValue: Pchar;
                                      aFieldType: TMysqlFieldTypes;
                                      aFieldLength: integer;
                                      aFormatSettings: TformatSettings): String;
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
      FIELD_TYPE_DOUBLE: result := floattostr(strtofloat(aFieldValue,fMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_DATETIME: Result := DateTimeToStr(StrToDateTime(aFieldValue,fMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_DATE,
      FIELD_TYPE_NEWDATE: Result := DateToStr(StrToDate(aFieldValue,fMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_TIME: Result := TimeToStr(StrToTime(aFieldValue,fMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_TIMESTAMP: Result := DateTimeToStr(StrToDateTime(aFieldValue,fMySqlFormatSettings),aformatSettings);
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
  GetLocaleFormatSettings(1033, fMySQLFormatSettings);
  fMySQLFormatSettings.DecimalSeparator := '.';
  fMySQLFormatSettings.ThousandSeparator := ',';
  fMySQLFormatSettings.DateSeparator := '-';
  fMySQLFormatSettings.TimeSeparator := ':';
  fMySQLFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  fMySQLFormatSettings.ShortTimeFormat := 'hh:nn:ss';
end;

{************************************************************}
constructor TalMySqlClient.Create(ApiVer: TALMySqlVersion_API;
                                  const lib: String = 'libmysql.dll');
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

{**********************************************************
http://dev.mysql.com/doc/refman/5.1/en/mysql-real-connect.html}
procedure TalMySqlClient.connect(Host: String;
                                 Port: integer;
                                 DataBaseName,
                                 Login,
                                 Password,
                                 CharSet: String;
                                 Const ClientFlag: Cardinal = 0);
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
    If (CharSet <> '') then CheckAPIError(fLibrary.mysql_options(fMySQL, MYSQL_SET_CHARSET_NAME, Pchar(CharSet)) <> 0);

    //attempts to establish a connection to a MySQL database engine running on host
    CheckAPIError(fLibrary.mysql_real_connect(fMySQL,
                                              pChar(Host),
                                              pChar(Login),
                                              pChar(Password),
                                              Pchar(DatabaseName),
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

{*********************************************************************}
procedure TalMySqlClient.SelectData(SQLs: TalMySqlClientSelectDataSQLs;
                                    XMLDATA: TalXMLNode;
                                    OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                    ExtData: Pointer;
                                    FormatSettings: TformatSettings);
Var aMySqlRes: PMYSQL_RES;
    aMySqlRow: PMYSQL_ROW;
    aMySqlFields: array of PMYSQL_FIELD;
    aMySqlFieldLengths: PMYSQL_LENGTHS;
    aColumnCount: Integer;
    aColumnIndex: integer;
    aNewRec: TalXmlNode;
    aValueRec: TalXmlNode;
    aViewRec: TalXmlNode;
    aSQLsindex: integer;
    aRecIndex: integer;
    aRecAdded: integer;
    aContinue: Boolean;
    aXmlDocument: TalXmlDocument;
    aUpdateRowTagByFieldValue: Boolean;

begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //clear the XMLDATA
  if assigned(XMLDATA) then begin
    XMLDATA.ChildNodes.Clear;
    aXmlDocument := Nil;
  end
  else begin
    aXmlDocument := ALCreateEmptyXMLDocument('root');
    XMLDATA := aXmlDocument.DocumentElement;
  end;

  Try

    {loop on all the SQL}
    For aSQLsindex := 0 to length(SQLs) - 1 do begin

      //prepare the query
      CheckAPIError(fLibrary.mysql_real_query(fMySQL, Pchar(SQLs[aSQLsindex].SQL), length(SQLs[aSQLsindex].SQL)) <> 0);
      aMySqlRes := fLibrary.mysql_use_result(fMySQL);
      CheckAPIError(aMySqlRes = nil);
      Try

        //Returns the number of columns in a result set.
        aColumnCount := fLibrary.mysql_num_fields(aMySqlRes);

        //init the aMySqlFields array
        //this not work anymore in MYSQL5.5, i don't know why so i use mysql_fetch_field instead
        //aMySqlFields := fLibrary.mysql_fetch_fields(aMySqlRes);
        setlength(aMySqlFields,aColumnCount);
        for aColumnIndex := 0 to aColumnCount - 1 do
          aMySqlFields[aColumnIndex] := fLibrary.mysql_fetch_field(aMySqlRes);

        //init the aViewRec
        if (SQLs[aSQLsindex].ViewTag <> '') and (not assigned(aXmlDocument)) then aViewRec := XMLdata.AddChild(SQLs[aSQLsindex].ViewTag)
        else aViewRec := XMLdata;

        //init aUpdateRowTagByFieldValue
        if AlPos('&>',SQLs[aSQLsindex].RowTag) = 1 then begin
          delete(SQLs[aSQLsindex].RowTag, 1, 2);
          aUpdateRowTagByFieldValue := True;
        end
        else aUpdateRowTagByFieldValue := False;

        //loop throught all row
        aRecIndex := 0;
        aRecAdded := 0;
        while True do begin

          //retrieve the next row. return A MYSQL_ROW structure for the next row.
          //NULL if there are no more rows to retrieve or if an error occurred.
          aMySqlRow := fLibrary.mysql_fetch_row(aMySqlRes);

          //break if no more row
          if aMySqlRow = nil then begin
            CheckAPIerror(Flibrary.mysql_errno(fMySQL) <> 0);
            break;
          end

          //download the row
          else begin

            //process if > Skip
            inc(aRecIndex);
            If aRecIndex > SQLs[aSQLsindex].Skip then begin

              //stop if no row are requested
              If (SQLs[aSQLsindex].First = 0) then break;

              //init NewRec
              if (SQLs[aSQLsindex].RowTag <> '') and (not assigned(aXmlDocument)) then aNewRec := aViewRec.AddChild(SQLs[aSQLsindex].RowTag)
              Else aNewRec := aViewRec;

              //init aMySqlFieldLengths
              aMySqlFieldLengths := fLibrary.mysql_fetch_lengths(aMySqlRes);
              CheckAPIerror(aMySqlFieldLengths = nil);

              //loop throught all column
              For aColumnIndex := 0 to aColumnCount - 1 do begin
                aValueRec := aNewRec.AddChild(ALlowercase(aMySqlFields[aColumnIndex].name));
                if (aMySqlFields[aColumnIndex]._type in [FIELD_TYPE_TINY_BLOB,
                                                         FIELD_TYPE_MEDIUM_BLOB,
                                                         FIELD_TYPE_LONG_BLOB,
                                                         FIELD_TYPE_BLOB]) then avalueRec.ChildNodes.Add(
                                                                                                         avalueRec.OwnerDocument.CreateNode(
                                                                                                                                            GetFieldValue(aMySqlRow[aColumnIndex],
                                                                                                                                                          aMySqlFields[aColumnIndex]._type,
                                                                                                                                                          aMySqlFieldLengths[aColumnIndex],
                                                                                                                                                          FormatSettings),
                                                                                                                                            ntCData
                                                                                                                                           )
                                                                                                         )
                else aValueRec.Text := GetFieldValue(aMySqlRow[aColumnIndex],
                                                     aMySqlFields[aColumnIndex]._type,
                                                     aMySqlFieldLengths[aColumnIndex],
                                                     FormatSettings);
                if aUpdateRowTagByFieldValue and (aValueRec.NodeName=aNewRec.NodeName) then aNewRec.NodeName := ALLowerCase(aValueRec.Text);
              end;

              //handle OnNewRowFunct
              if assigned(OnNewRowFunct) then begin
                aContinue := True;
                OnNewRowFunct(aNewRec, SQLs[aSQLsindex].ViewTag, ExtData, aContinue);
                if Not aContinue then Break;
              end;

              //free the node if aXmlDocument
              if assigned(aXmlDocument) then aXmlDocument.DocumentElement.ChildNodes.Clear;

              //handle the First
              inc(aRecAdded);
              If (SQLs[aSQLsindex].First >= 0) and (aRecAdded >= SQLs[aSQLsindex].First) then Break;

            end;

          end;

        end;

      Finally
        //Frees the memory allocated to aMySqlRes
        fLibrary.mysql_free_result(aMySqlRes);
      End;

    End;

  Finally
    if assigned(aXmlDocument) then aXmlDocument.free;
  End;

end;

{*******************************************************************}
procedure TalMySqlClient.SelectData(SQL: TalMySqlClientSelectDataSQL;
                                    OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                    ExtData: Pointer;
                                    FormatSettings: TformatSettings);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0] := Sql;
  SelectData(aSelectDataSQLs,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{**********************************************}
procedure TalMySqlClient.SelectData(SQL: String;
                                    Skip: Integer;
                                    First: Integer;
                                    OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                    ExtData: Pointer;
                                    FormatSettings: TformatSettings);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := '';
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := Skip;
  aSelectDataSQLs[0].First := First;
  SelectData(aSelectDataSQLs,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{**********************************************}
procedure TalMySqlClient.SelectData(SQL: String;
                                    OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                    ExtData: Pointer;
                                    FormatSettings: TformatSettings);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := '';
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings);
end;

{*********************************************************************}
procedure TalMySqlClient.SelectData(SQLs: TalMySqlClientSelectDataSQLs;
                                    XMLDATA: TalXMLNode;
                                    FormatSettings: TformatSettings);
begin

  SelectData(SQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings);

end;

{*******************************************************************}
procedure TalMySqlClient.SelectData(SQL: TalMySqlClientSelectDataSQL;
                                    XMLDATA: TalXMLNode;
                                    FormatSettings: TformatSettings);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0] := Sql;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{**********************************************}
procedure TalMySqlClient.SelectData(SQL: String;
                                    RowTag: String;
                                    Skip: Integer;
                                    First: Integer;
                                    XMLDATA: TalXMLNode;
                                    FormatSettings: TformatSettings);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := RowTag;
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := Skip;
  aSelectDataSQLs[0].First := First;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{**********************************************}
procedure TalMySqlClient.SelectData(SQL: String;
                                    RowTag: String;
                                    XMLDATA: TalXMLNode;
                                    FormatSettings: TformatSettings);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := RowTag;
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{**********************************************}
procedure TalMySqlClient.SelectData(SQL: String;
                                    XMLDATA: TalXMLNode;
                                    FormatSettings: TformatSettings);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := '';
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings);
end;

{**********************************************************************}
procedure TalMySqlClient.UpdateData(SQLs: TalMySqlClientUpdateDataSQLs);
Var aSQLsindex: integer;
begin

  //exit if no SQL
  if length(SQLs) = 0 then Exit;

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //loop on all the SQL
  For aSQLsindex := 0 to length(SQLs) - 1 do begin

    //do the query
    CheckAPIError(fLibrary.mysql_real_query(fMySQL, Pchar(SQLs[aSQLsindex].SQL), length(SQLs[aSQLsindex].SQL)) <> 0);

  end;

end;

{********************************************************************}
procedure TalMySqlClient.UpdateData(SQL: TalMySqlClientUpdateDataSQL);
Var aUpdateDataSQLs: TalMySqlClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,1);
  aUpdateDataSQLs[0] := SQL;
  UpdateData(aUpdateDataSQLs);
end;

{**************************************************}
procedure TalMySqlClient.UpdateData(SQLs: Tstrings);
Var aSQLsindex : integer;
    aUpdateDataSQLs: TalMySqlClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,SQLs.Count);
  For aSQLsindex := 0 to SQLs.Count - 1 do begin
    aUpdateDataSQLs[aSQLsindex].SQL := SQLs[aSQLsindex];
  end;
  UpdateData(aUpdateDataSQLs);
end;

{***********************************************}
procedure TalMySqlClient.UpdateData(SQL: String);
Var aUpdateDataSQLs: TalMySqlClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,1);
  aUpdateDataSQLs[0].SQL := SQL;
  UpdateData(aUpdateDataSQLs);
end;

{*********************************************************}
procedure TalMySqlClient.UpdateData(SQLs: array of string);
Var aUpdateDataSQLs: TalMySqlClientUpdateDataSQLs;
    i: integer;
begin
  setlength(aUpdateDataSQLs,length(SQLs));
  for I := 0 to length(SQLs) - 1 do begin
    aUpdateDataSQLs[i].SQL := SQLs[i];
  end;
  UpdateData(aUpdateDataSQLs);
end;

{********************************************************}
function TalMySqlClient.insert_id(SQL: String): ULongLong;
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



////////////////////////////////////
///// TalMySqlConnPoolClient /////
////////////////////////////////////

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

{************************************************************}
function TalMySqlConnectionPoolClient.GetDataBaseName: String;
begin
  result := FdatabaseName;
end;

{****************************************************}
function TalMySqlConnectionPoolClient.GetHost: String;
begin
  result := fHost;
end;

{*****************************************************}
function TalMySqlConnectionPoolClient.GetPort: integer;
begin
  result := fPort;
end;

{*********************************************************************}
function TalMySqlConnectionPoolClient.GetFieldValue(aFieldValue: Pchar;
                                                    aFieldType: TMysqlFieldTypes;
                                                    aFieldLength: integer;
                                                    aFormatSettings: TformatSettings): String;
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
      FIELD_TYPE_DOUBLE: result := floattostr(strtofloat(aFieldValue,fMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_DATETIME: Result := DateTimeToStr(StrToDateTime(aFieldValue,fMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_DATE,
      FIELD_TYPE_NEWDATE: Result := DateToStr(StrToDate(aFieldValue,fMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_TIME: Result := TimeToStr(StrToTime(aFieldValue,fMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_TIMESTAMP: Result := DateTimeToStr(StrToDateTime(aFieldValue,fMySqlFormatSettings),aformatSettings);
      FIELD_TYPE_NULL: result := fNullString; // Example: SELECT NULL FROM DUAL
      Else SetString(Result, aFieldValue, aFieldLength);
    end;
  end;
end;

{**************************************************************}
procedure TalMySqlConnectionPoolClient.initObject(aHost: String;
                                                  aPort: integer;
                                                  aDataBaseName,
                                                  aLogin,
                                                  aPassword,
                                                  aCharSet: String;
                                                  Const aOpenConnectionClientFlag: Cardinal = 0);
begin
  fHost := aHost;
  fPort := aPort;
  FDataBaseName:= aDataBaseName;
  fLogin := aLogin;
  fPassword := aPassword;
  fCharset := aCharset;
  fOpenConnectionClientFlag := aOpenConnectionClientFlag;
  FConnectionPool:= TObjectList.Create(True);
  FConnectionPoolCS:= TCriticalSection.create;
  FWorkingConnectionCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionGarbage := ALGettickCount64;
  FConnectionMaxIdleTime := 1200000; // 1000 * 60 * 20 = 20 min
  FNullString := '';
  GetLocaleFormatSettings(1033, fMySQLFormatSettings);
  fMySQLFormatSettings.DecimalSeparator := '.';
  fMySQLFormatSettings.ThousandSeparator := ',';
  fMySQLFormatSettings.DateSeparator := '-';
  fMySQLFormatSettings.TimeSeparator := ':';
  fMySQLFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  fMySQLFormatSettings.ShortTimeFormat := 'hh:nn:ss';
end;

{************************************************************}
constructor TalMySqlConnectionPoolClient.Create(aHost: String;
                                                aPort: integer;
                                                aDataBaseName,
                                                aLogin,
                                                aPassword,
                                                aCharSet: String;
                                                aApiVer: TALMySqlVersion_API;
                                                const alib: String = 'libmysql.dll';
                                                Const aOpenConnectionClientFlag: Cardinal = 0);
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
               aOpenConnectionClientFlag);
  except
    fLibrary.free;
    raise;
  End;
end;

{************************************************************}
constructor TalMySqlConnectionPoolClient.Create(aHost: String;
                                                aPort: integer;
                                                aDataBaseName,
                                                aLogin,
                                                aPassword,
                                                aCharSet: String;
                                                alib: TALMySqlLibrary;
                                                Const aOpenConnectionClientFlag: Cardinal = 0);
begin
  fLibrary := alib;
  FownLibrary := False;
  initObject(aHost,
             aPort,
             aDataBaseName,
             aLogin,
             aPassword,
             aCharSet,
             aOpenConnectionClientFlag);
end;

{**********************************************}
destructor TalMySqlConnectionPoolClient.Destroy;
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

{**************************************************************}
function TalMySqlConnectionPoolClient.AcquireConnection: PMySql;
Var aConnectionPoolContainer: TalMySqlConnectionPoolContainer;
    aTickCount: int64;
Begin

  //for a stupid warning
  result := nil;

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    // This function must be called early within each
    // created thread to initialize thread-specific variables
    // this look very important because if we comment this and
    // the mysql_thread_end then the Flibrary.unload can take several seconds
    // you can see this in the ALSQLBenchmark.exe project with the loop update button 
    CheckAPIError(Nil, FLibrary.mysql_thread_init <> 0);
    Try

      //raise an exception if currently realeasing all connection
      if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

      //delete the old unused connection
      aTickCount := ALGetTickCount64;
      if aTickCount - fLastConnectionGarbage > (60000 {every minutes})  then begin
        while FConnectionPool.Count > 0 do begin
          aConnectionPoolContainer := TalMySqlConnectionPoolContainer(FConnectionPool[0]);
          if aTickCount - aConnectionPoolContainer.Lastaccessdate > FConnectionMaxIdleTime then begin
            Try
              FLibrary.MySql_close(aConnectionPoolContainer.ConnectionHandle);
            Except
              //Disconnect must be a "safe" procedure because it's mostly called in
              //finalization part of the code that it is not protected
              //that the bulsheet of MySql to answer SQLITE_BUSY instead of free
              //everything
            End;
            FConnectionPool.Delete(0); // must be delete here because FConnectionPool free the object also
          end
          else break;
        end;
        FLastConnectionGarbage := aTickCount;        
      end;

      //acquire the new connection from the pool
      If FConnectionPool.Count > 0 then begin
        aConnectionPoolContainer := TalMySqlConnectionPoolContainer(FConnectionPool[FConnectionPool.count - 1]);
        Result := aConnectionPoolContainer.ConnectionHandle;
        FConnectionPool.Delete(FConnectionPool.count - 1);
      end

      //create a new connection
      else begin
        result := nil;
        Try

          //Allocates or initializes a MYSQL object suitable for mysql_real_connect()
          Result := fLibrary.mysql_init(nil);
          CheckAPIError(nil, result = nil);

          //set the The name of the character set to use as the default character set.
          If (fCharSet <> '') then CheckAPIError(Result, fLibrary.mysql_options(Result, MYSQL_SET_CHARSET_NAME, Pchar(fCharSet)) <> 0);

          //attempts to establish a connection to a MySQL database engine running on host
          CheckAPIError(Result, fLibrary.mysql_real_connect(Result,
                                                            pChar(fHost),
                                                            pChar(fLogin),
                                                            pChar(fPassword),
                                                            Pchar(fDatabaseName),
                                                            fPort,
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
      FLibrary.mysql_thread_end;
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
procedure TalMySqlConnectionPoolClient.ReleaseConnection(var ConnectionHandle: PMySql;
                                                         const CloseConnection: Boolean = False);
Var aConnectionPoolContainer: TalMySqlConnectionPoolContainer;
begin

  //security check
  if not assigned(ConnectionHandle) then raise exception.Create('Connection handle can not be null');

  //release the connection
  FConnectionPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      aConnectionPoolContainer := TalMySqlConnectionPoolContainer.Create;
      aConnectionPoolContainer.ConnectionHandle := ConnectionHandle;
      aConnectionPoolContainer.LastAccessDate := ALGetTickCount64;
      FConnectionPool.add(aConnectionPoolContainer);
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
    Try
      FLibrary.mysql_thread_end;
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
Var aConnectionPoolContainer: TalMySqlConnectionPoolContainer;
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
        aConnectionPoolContainer := TalMySqlConnectionPoolContainer(FConnectionPool[FConnectionPool.count - 1]);
        Try
          FLibrary.MySql_close(aConnectionPoolContainer.ConnectionHandle);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;
        FConnectionPool.Delete(FConnectionPool.count - 1); // must be delete here because FConnectionPool free the object also
      end;
      FLastConnectionGarbage := ALGetTickCount64;
    finally
      FConnectionPoolCS.Release;
    end;

  finally
    //Do not forbid anymore new thread to create a new transaction
    FReleasingAllconnections := False;
  End;

end;

{************************************************************************************}
procedure TalMySqlConnectionPoolClient.TransactionStart(Var ConnectionHandle: PMySql);
begin

  //ConnectionHandle must be null
  if assigned(ConnectionHandle) then raise exception.Create('Connection handle must be null');

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

{************************************************************************************}
procedure TalMySqlConnectionPoolClient.TransactionCommit(var ConnectionHandle: PMySql;
                                                         const CloseConnection: Boolean = False);
begin

  //security check
  if not assigned(ConnectionHandle) then raise exception.Create('Connection handle can not be null');

  //commit the transaction
  UpdateData('COMMIT', ConnectionHandle);

  //release the connection
  ReleaseConnection(ConnectionHandle, CloseConnection);

end;

{**************************************************************************************}
procedure TalMySqlConnectionPoolClient.TransactionRollback(var ConnectionHandle: PMySql;
                                                           const CloseConnection: Boolean = False);
var aTmpCloseConnection: Boolean;
begin

  //security check
  if not assigned(ConnectionHandle) then raise exception.Create('Connection handle can not be null');

  //rollback the connection
  aTmpCloseConnection := CloseConnection;
  Try
    Try
      UpdateData('ROLLBACK', ConnectionHandle);
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
    ReleaseConnection(ConnectionHandle, aTmpCloseConnection);

  End;

end;

{***********************************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(SQLs: TalMySqlClientSelectDataSQLs;
                                                  XMLDATA: TalXMLNode;
                                                  OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                                  ExtData: Pointer;
                                                  FormatSettings: TformatSettings;
                                                  const ConnectionHandle: PMySql = nil);

Var aMySqlRes: PMYSQL_RES;
    aMySqlRow: PMYSQL_ROW;
    aMySqlFields: PMYSQL_FIELDS;
    aMySqlFieldLengths: PMYSQL_LENGTHS;
    aColumnCount: Integer;
    aColumnIndex: integer;
    aNewRec: TalXmlNode;
    aValueRec: TalXmlNode;
    aViewRec: TalXmlNode;
    aSQLsindex: integer;
    aRecIndex: integer;
    aRecAdded: integer;
    aTmpConnectionHandle: PMySql;
    aOwnConnection: Boolean;
    aContinue: Boolean;
    aXmlDocument: TalXmlDocument;
    aUpdateRowTagByFieldValue: Boolean;

begin

  //clear the XMLDATA
  if assigned(XMLDATA) then begin
    XMLDATA.ChildNodes.Clear;
    aXmlDocument := Nil;
  end
  else begin
    aXmlDocument := ALCreateEmptyXMLDocument('root');
    XMLDATA := aXmlDocument.DocumentElement;
  end;

  Try

    //acquire a connection and start the transaction if necessary
    aTmpConnectionHandle := ConnectionHandle;
    aOwnConnection := (not assigned(ConnectionHandle));
    if aOwnConnection then TransactionStart(aTmpConnectionHandle);
    Try

      //loop on all the SQL
      For aSQLsindex := 0 to length(SQLs) - 1 do begin

        //prepare the query
        CheckAPIError(aTmpConnectionHandle, fLibrary.mysql_real_query(aTmpConnectionHandle, Pchar(SQLs[aSQLsindex].SQL), length(SQLs[aSQLsindex].SQL)) <> 0);
        aMySqlRes := fLibrary.mysql_use_result(aTmpConnectionHandle);
        CheckAPIError(aTmpConnectionHandle, aTmpConnectionHandle = nil);
        Try

          //Returns the number of columns in a result set.
          aColumnCount := fLibrary.mysql_num_fields(aMySqlRes);

          //init the aMySqlFields array
          aMySqlFields := fLibrary.mysql_fetch_fields(aMySqlRes);

          //init the aViewRec
          if (SQLs[aSQLsindex].ViewTag <> '') and (not assigned(aXmlDocument))  then aViewRec := XMLdata.AddChild(SQLs[aSQLsindex].ViewTag)
          else aViewRec := XMLdata;

          //init aUpdateRowTagByFieldValue
          if AlPos('&>',SQLs[aSQLsindex].RowTag) = 1 then begin
            delete(SQLs[aSQLsindex].RowTag, 1, 2);
            aUpdateRowTagByFieldValue := True;
          end
          else aUpdateRowTagByFieldValue := False;

          //loop throught all row
          aRecIndex := 0;
          aRecAdded := 0;
          while True do begin

            //retrieve the next row. return A MYSQL_ROW structure for the next row.
            //NULL if there are no more rows to retrieve or if an error occurred.
            aMySqlRow := fLibrary.mysql_fetch_row(aMySqlRes);

            //break if no more row
            if aMySqlRow = nil then begin
              CheckAPIerror(aTmpConnectionHandle, Flibrary.mysql_errno(aTmpConnectionHandle) <> 0);
              break;
            end

            //download the row
            else begin

              //process if > Skip
              inc(aRecIndex);
              If aRecIndex > SQLs[aSQLsindex].Skip then begin

                //stop if no row are requested
                If (SQLs[aSQLsindex].First = 0) then break;

                //init NewRec
                if (SQLs[aSQLsindex].RowTag <> '') and (not assigned(aXmlDocument))  then aNewRec := aViewRec.AddChild(SQLs[aSQLsindex].RowTag)
                Else aNewRec := aViewRec;

                //init aMySqlFieldLengths
                aMySqlFieldLengths := fLibrary.mysql_fetch_lengths(aMySqlRes);
                CheckAPIerror(aTmpConnectionHandle, aMySqlFieldLengths = nil);

                //loop throught all column
                For aColumnIndex := 0 to aColumnCount - 1 do begin
                  aValueRec := aNewRec.AddChild(ALlowercase(aMySqlFields[aColumnIndex].name));
                  if (aMySqlFields[aColumnIndex]._type in [FIELD_TYPE_TINY_BLOB,
                                                           FIELD_TYPE_MEDIUM_BLOB,
                                                           FIELD_TYPE_LONG_BLOB,
                                                           FIELD_TYPE_BLOB]) then avalueRec.ChildNodes.Add(
                                                                                                           avalueRec.OwnerDocument.CreateNode(
                                                                                                                                              GetFieldValue(aMySqlRow[aColumnIndex],
                                                                                                                                                            aMySqlFields[aColumnIndex]._type,
                                                                                                                                                            aMySqlFieldLengths[aColumnIndex],
                                                                                                                                                            FormatSettings),
                                                                                                                                              ntCData
                                                                                                                                             )
                                                                                                           )
                  else aValueRec.Text := GetFieldValue(aMySqlRow[aColumnIndex],
                                                       aMySqlFields[aColumnIndex]._type,
                                                       aMySqlFieldLengths[aColumnIndex],
                                                       FormatSettings);
                  if aUpdateRowTagByFieldValue and (aValueRec.NodeName=aNewRec.NodeName) then aNewRec.NodeName := ALLowerCase(aValueRec.Text);
                end;

                //handle OnNewRowFunct
                if assigned(OnNewRowFunct) then begin
                  aContinue := True;
                  OnNewRowFunct(aNewRec, SQLs[aSQLsindex].ViewTag, ExtData, aContinue);
                  if Not aContinue then Break;
                end;

                //free the node if aXmlDocument
                if assigned(aXmlDocument) then aXmlDocument.DocumentElement.ChildNodes.Clear;

                //handle the First
                inc(aRecAdded);
                If (SQLs[aSQLsindex].First >= 0) and (aRecAdded >= SQLs[aSQLsindex].First) then Break;

              end;

            end;

          end;

        Finally
          //Frees the memory allocated to aMySqlRes
          fLibrary.mysql_free_result(aMySqlRes);
        End;

      End;

      //commit the transaction and release the connection if owned
      if aOwnConnection then TransactionCommit(aTmpConnectionHandle);

    except
      On E: Exception do begin

        //rollback the transaction and release the connection if owned
        if aOwnConnection then TransactionRollback(aTmpConnectionHandle, true);

        //raise the error
        raise;

      end;
    end;

  Finally
    if assigned(aXmlDocument) then aXmlDocument.free;
  End;

end;

{*********************************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(SQL: TalMySqlClientSelectDataSQL;
                                                  OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                                  ExtData: Pointer;
                                                  FormatSettings: TformatSettings;
                                                  const ConnectionHandle: PMySql = nil);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0] := Sql;
  SelectData(aSelectDataSQLs,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(SQL: String;
                                                  Skip: Integer;
                                                  First: Integer;
                                                  OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                                  ExtData: Pointer;
                                                  FormatSettings: TformatSettings;
                                                  const ConnectionHandle: PMySql = nil);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := '';
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := Skip;
  aSelectDataSQLs[0].First := First;
  SelectData(aSelectDataSQLs,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(SQL: String;
                                                  OnNewRowFunct: TalMySqlClientSelectDataOnNewRowFunct;
                                                  ExtData: Pointer;
                                                  FormatSettings: TformatSettings;
                                                  const ConnectionHandle: PMySql = nil);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := '';
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             nil,
             OnNewRowFunct,
             ExtData,
             FormatSettings,
             ConnectionHandle);
end;

{***********************************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(SQLs: TalMySqlClientSelectDataSQLs;
                                                  XMLDATA: TalXMLNode;
                                                  FormatSettings: TformatSettings;
                                                  const ConnectionHandle: PMySql = nil);
begin

  SelectData(SQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             ConnectionHandle);

end;

{*********************************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(SQL: TalMySqlClientSelectDataSQL;
                                                  XMLDATA: TalXMLNode;
                                                  FormatSettings: TformatSettings;
                                                  const ConnectionHandle: PMySql = nil);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0] := Sql;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(SQL: String;
                                                  RowTag: String;
                                                  Skip: Integer;
                                                  First: Integer;
                                                  XMLDATA: TalXMLNode;
                                                  FormatSettings: TformatSettings;
                                                  const ConnectionHandle: PMySql = nil);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := RowTag;
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := Skip;
  aSelectDataSQLs[0].First := First;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(SQL: String;
                                                  RowTag: String;
                                                  XMLDATA: TalXMLNode;
                                                  FormatSettings: TformatSettings;
                                                  const ConnectionHandle: PMySql = nil);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := RowTag;
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             ConnectionHandle);
end;

{************************************************************}
procedure TalMySqlConnectionPoolClient.SelectData(SQL: String;
                                                  XMLDATA: TalXMLNode;
                                                  FormatSettings: TformatSettings;
                                                  const ConnectionHandle: PMySql = nil);
var aSelectDataSQLs: TalMySqlClientSelectDataSQLs;
begin
  setlength(aSelectDataSQLs,1);
  aSelectDataSQLs[0].Sql := Sql;
  aSelectDataSQLs[0].RowTag := '';
  aSelectDataSQLs[0].viewTag := '';
  aSelectDataSQLs[0].skip := -1;
  aSelectDataSQLs[0].First := -1;
  SelectData(aSelectDataSQLs,
             XMLDATA,
             nil,
             nil,
             FormatSettings,
             ConnectionHandle);
end;

{***********************************************************************************}
procedure TalMySqlConnectionPoolClient.UpdateData(SQLs: TalMySqlClientUpdateDataSQLs;
                                                  const ConnectionHandle: PMySql = nil);
Var aSQLsindex: integer;
    aTmpConnectionHandle: PMySql;
    aOwnConnection: Boolean;
begin

  //exit if no SQL
  if length(SQLs) = 0 then Exit;

  //acquire a connection and start the transaction if necessary
  aTmpConnectionHandle := ConnectionHandle;
  aOwnConnection := (not assigned(ConnectionHandle));
  if aOwnConnection then TransactionStart(aTmpConnectionHandle);
  Try

    //loop on all the SQL
    For aSQLsindex := 0 to length(SQLs) - 1 do begin

      //do the query
      CheckAPIError(aTmpConnectionHandle, fLibrary.mysql_real_query(aTmpConnectionHandle, Pchar(SQLs[aSQLsindex].SQL), length(SQLs[aSQLsindex].SQL)) <> 0);

    end;

    //commit the transaction and release the connection if owned
    if aOwnConnection then TransactionCommit(aTmpConnectionHandle);

  except
    On E: Exception do begin

      //rollback the transaction and release the connection if owned
      if aOwnConnection then TransactionRollback(aTmpConnectionHandle, true);

      //raise the error
      raise;

    end;
  end;

end;

{*********************************************************************************}
procedure TalMySqlConnectionPoolClient.UpdateData(SQL: TalMySqlClientUpdateDataSQL;
                                                  const ConnectionHandle: PMySql = nil);
Var aUpdateDataSQLs: TalMySqlClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,1);
  aUpdateDataSQLs[0] := SQL;
  UpdateData(aUpdateDataSQLs, ConnectionHandle);
end;

{***************************************************************}
procedure TalMySqlConnectionPoolClient.UpdateData(SQLs: Tstrings;
                                                  const ConnectionHandle: PMySql = nil);
Var aSQLsindex : integer;
    aUpdateDataSQLs: TalMySqlClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,SQLs.Count);
  For aSQLsindex := 0 to SQLs.Count - 1 do begin
    aUpdateDataSQLs[aSQLsindex].SQL := SQLs[aSQLsindex];
  end;
  UpdateData(aUpdateDataSQLs, ConnectionHandle);
end;

{************************************************************}
procedure TalMySqlConnectionPoolClient.UpdateData(SQL: String;
                                                  const ConnectionHandle: PMySql = nil);
Var aUpdateDataSQLs: TalMySqlClientUpdateDataSQLs;
begin
  setlength(aUpdateDataSQLs,1);
  aUpdateDataSQLs[0].SQL := SQL;
  UpdateData(aUpdateDataSQLs, ConnectionHandle);
end;

{**********************************************************************}
procedure TalMySqlConnectionPoolClient.UpdateData(SQLs: array of String;
                                                  const ConnectionHandle: PMySql = nil);
Var aUpdateDataSQLs: TalMySqlClientUpdateDataSQLs;
    i: integer;
begin
  setlength(aUpdateDataSQLs,length(SQLs));
  for I := 0 to length(SQLs) - 1 do begin
    aUpdateDataSQLs[i].SQL := SQLs[i];
  end;
  UpdateData(aUpdateDataSQLs, ConnectionHandle);
end;

{**********************************************************}
function TalMySqlConnectionPoolClient.insert_id(SQL: String;
                                                const ConnectionHandle: PMySql = nil): ULongLong;
Var aTmpConnectionHandle: PMySql;
    aOwnConnection: Boolean;
begin

  //acquire a connection and start the transaction if necessary
  aTmpConnectionHandle := ConnectionHandle;
  aOwnConnection := (not assigned(ConnectionHandle));
  if aOwnConnection then TransactionStart(aTmpConnectionHandle);
  Try

    //if the SQL is not empty
    if SQL <> '' then begin

      //do the query
      UpdateData(SQL, aTmpConnectionHandle);

      //Returns the value generated for an AUTO_INCREMENT column
      //by the previous INSERT or UPDATE statement
      Result := fLibrary.mysql_insert_id(aTmpConnectionHandle);

    end

    //if the SQL is empty, simply gave an 0 result
    else result := 0;

    //commit the transaction and release the connection if owned
    if aOwnConnection then TransactionCommit(aTmpConnectionHandle);

  except
    On E: Exception do begin

      //rollback the transaction and release the connection if owned
      if aOwnConnection then TransactionRollback(aTmpConnectionHandle, true);

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
    Result := FConnectionPool.Count + FWorkingConnectionCount;
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

end.
