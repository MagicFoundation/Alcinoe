{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Plain API Performance          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTestPlainPerformance;
{$I ZPerformance.inc}

interface

uses TestFramework, SysUtils, Classes, ZPerformanceTestCase,
 {$IFDEF ENABLE_DBLIB}
 {$ENDIF}
 {$IFDEF ENABLE_INTERBASE}
   ZDbcInterbase6Utils,
   ZPlainFirebirdDriver,
   ZPlainFirebirdInterbaseConstants,
 {$ENDIF}
 {$IFDEF ENABLE_MYSQL}
   ZPlainMySqlDriver,
   ZPlainMySqlConstants,
 {$ENDIF}
 {$IFDEF ENABLE_POSTGRESQL}
   ZPlainPostgreSqlDriver,
 {$ENDIF}
 {$IFDEF ENABLE_ADO}
 {$ENDIF}
 {$IFDEF ENABLE_ORACLE}
 {$ENDIF}
 {$IFDEF ENABLE_SQLITE}
 {$ENDIF}
 {$IFDEF ENABLE_ASA}
   ZPlainASADriver, ZDbcASAUtils,
 {$ENDIF}

   ZSysUtils, ZDbcUtils, ZCompatibility;

 {$IFDEF ENABLE_MYSQL}
type
  {** Implements a performance test case for Plain MySQL API. }
  TZPlainMySQLPerformanceTestCase = class (TZPerformanceSQLTestCase)
  private
    FPlainDriver: IZMySQLPlainDriver;
    FHandle: PZMySQLConnect;
    FQueryHandle: PZMySQLResult;

  protected
    property PlainDriver: IZMySQLPlainDriver read FPlainDriver write FPlainDriver;
    property Handle: PZMySQLConnect read FHandle write FHandle;
    property QueryHandle: PZMySQLResult read FQueryHandle write FQueryHandle;

    procedure Connect;
    procedure Disconnect;
    procedure ExecuteUpdate(Query: string);
    function ExecuteQuery(Query: string): PZMySQLResult;

  protected
    { Methods which specify test settings. }
    function GetSupportedProtocols: string; override;
    function GetImplementedAPI: string; override;
    procedure SetUp; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure RunTestFetch; override;
    procedure RunTestUpdate; override;
    procedure RunTestDelete; override;
    procedure RunTestDirectUpdate; override;
  end;
 {$ENDIF}

 {$IFDEF ENABLE_POSTGRESQL}
type
  {** Implements a performance test case for Plain PostgreSQL API. }
  TZPlainPostgreSQLPerformanceTestCase = class (TZPerformanceSQLTestCase)
  private
    FPlainDriver: IZPostgreSQLPlainDriver;
    FHandle: PZPostgreSQLConnect;
    FQueryHandle: PZPostgreSQLResult;
  protected
    property PlainDriver: IZPostgreSQLPlainDriver read FPlainDriver write FPlainDriver;
    property Handle: PZPostgreSQLConnect read FHandle write FHandle;
    property QueryHandle: PZPostgreSQLResult read FQueryHandle write FQueryHandle;

    procedure Connect;
    procedure Disconnect;
    procedure ExecuteUpdate(Query: string);
    function ExecuteQuery(Query: string): PZPostgreSQLResult;
    function CheckPostgreSQLError: boolean;
  protected
    { Methods which specify test settings. }
    function GetSupportedProtocols: string; override;
    function GetImplementedAPI: string; override;
    procedure SetUp; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure RunTestFetch; override;
    procedure RunTestUpdate; override;
    procedure RunTestDelete; override;
    procedure RunTestDirectUpdate; override;
  end;
 {$ENDIF}


 {$IFDEF ENABLE_INTERBASE}
type
  {** Implements a performance test case for Plain Interbase API. }
  TZPlainInterbase6SQLPerformanceTestCase = class (TZPerformanceSQLTestCase)
  private
    FDialect: Word;
    FPlainDriver: IZInterbasePlainDriver;
    FHandle: TISC_DB_HANDLE;
    FTrHandle: TISC_TR_HANDLE;
    FStmtHandle: TISC_STMT_HANDLE;
    FStatusVector: TARRAY_ISC_STATUS;
  protected
    property PlainDriver: IZInterbasePlainDriver read FPlainDriver write FPlainDriver;
    property Handle: TISC_DB_HANDLE read FHandle write FHandle;
    property TrHandle: TISC_DB_HANDLE read FTrHandle write FTrHandle;
    property StmtHandle: TISC_STMT_HANDLE read FStmtHandle write FStmtHandle;

    procedure ExecuteSql(SQL: string);
    function ExecuteQuery(SQL: string): IZResultSQLDA;

    procedure Connect;
    procedure Disconnect;
    procedure CheckInterbase6Error(Sql: string = '');
  protected
    { Methods which specify test settings. }
    function GetSupportedProtocols: string; override;
    function GetImplementedAPI: string; override;
    procedure SetUp; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure RunTestFetch; override;
    procedure RunTestUpdate; override;
    procedure RunTestDelete; override;
    procedure RunTestDirectUpdate; override;
  end;
 {$ENDIF}

 {$IFDEF ENABLE_ASA}
type
  {** Implements a performance test case for Plain ASA API. }
  TZPlainASASQLPerformanceTestCase = class (TZPerformanceSQLTestCase)
  private
    FPlainDriver: IZASAPlainDriver;
    FSQLCA: TZASASQLCA;
    FHandle: PZASASQLCA;
    FStmtNum: SmallInt;
    FCursorName: string;
  protected
    property PlainDriver: IZASAPlainDriver read FPlainDriver write FPlainDriver;
    property Handle: PZASASQLCA read FHandle write FHandle;
    property StmtNum: SmallInt read FStmtNum write FStmtNum;
    property CursorName: String read FCursorName write FCursorName;

    procedure ExecuteSql(SQL: string);
    function ExecuteQuery(SQL: string): IZASASQLDA;

    procedure Connect;
    procedure Disconnect;
    procedure CheckASAError(Sql: string = '');
  protected
    { Methods which specify test settings. }
    function GetSupportedProtocols: string; override;
    function GetImplementedAPI: string; override;
    procedure SetUp; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure RunTestFetch; override;
    procedure RunTestUpdate; override;
    procedure RunTestDelete; override;
    procedure RunTestDirectUpdate; override;
  end;
 {$ENDIF}

implementation

uses ZSqlTestCase;

{$IFDEF ENABLE_MYSQL}
 { TZPlainMySQLPerformanceTestCase }
{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZPlainMySQLPerformanceTestCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5';
end;

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZPlainMySQLPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'plain';
end;

{**
   Create objects and allocate memory for variables
}
procedure TZPlainMySQLPerformanceTestCase.SetUp;
var
  MySQL41PlainDriver: IZMySQLPlainDriver;
  MySQL5PlainDriver: IZMySQLPlainDriver;
  MySQLD41PlainDriver: IZMySQLPlainDriver;
  MySQLD5PlainDriver: IZMySQLPlainDriver;
begin
  MySQL41PlainDriver := TZMySQL41PlainDriver.Create;
  MySQL5PlainDriver := TZMySQL5PlainDriver.Create;
  MySQLD41PlainDriver := TZMySQLD41PlainDriver.Create;
  MySQLD5PlainDriver := TZMySQLD5PlainDriver.Create;

  if Protocol = MySQL41PlainDriver.GetProtocol then
    PlainDriver := MySQL41PlainDriver
  else if Protocol = MySQL5PlainDriver.GetProtocol then
    PlainDriver := MySQL5PlainDriver
  else if Protocol = MySQLD41PlainDriver.GetProtocol then
    PlainDriver := MySQLD41PlainDriver
  else if Protocol = MySQLD5PlainDriver.GetProtocol then
    PlainDriver := MySQLD5PlainDriver
  else PlainDriver := MySQL5PlainDriver;

  PlainDriver.Initialize;
end;

{**
   Connect to database server
}
procedure TZPlainMySQLPerformanceTestCase.Connect;
begin
  Handle := nil;
  PlainDriver.Init(FHandle);
//  PlainDriver.SetOptions(Handle, MYSQL_OPT_COMPRESS, nil);
  if PlainDriver.RealConnect(Handle, PChar(HostName),
    PChar(UserName), PChar(Password), PChar(Database),
    Port, nil, _CLIENT_CONNECT_WITH_DB) = nil then
  Fail('Can not connect to MySql server');
end;

{**
   Disconnect to database server
}
procedure TZPlainMySQLPerformanceTestCase.Disconnect;
begin
  if Handle <> nil then
  begin
    PlainDriver.Close(Handle);
    PlainDriver.Despose(FHandle);
  end;
end;

{**
  Executes an update SQL statement.
  @param Query a SQL statement to be executed.
}
procedure TZPlainMySQLPerformanceTestCase.ExecuteUpdate(Query: string);
begin
  if PlainDriver.ExecQuery(Handle, PChar(Query)) <> 0 then
    Fail('Fails to execute SQL statement.');
end;

{**
  Executes a query SQL statement.
  @param Query a SQL statement to be executed.
  @return a query result handle.
}
function TZPlainMySQLPerformanceTestCase.ExecuteQuery(
  Query: string): PZMySQLResult;
begin
  if PlainDriver.ExecQuery(Handle, PChar(Query)) <> 0 then
    Fail('Fail execute query statement.');
  Result := PlainDriver.StoreResult(Handle);
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZPlainMySQLPerformanceTestCase.DefaultSetUpTest;
begin
  Connect;
  QueryHandle := nil;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZPlainMySQLPerformanceTestCase.DefaultTearDownTest;
begin
  if QueryHandle <> nil then
  begin
    PlainDriver.FreeResult(QueryHandle);
    QueryHandle := nil;
  end;
  Disconnect;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZPlainMySQLPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZPlainMySQLPerformanceTestCase.RunTestConnect;
begin
  Connect;
end;

{**
  Performs an insert test.
}
procedure TZPlainMySQLPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    ExecuteUpdate(Format('INSERT INTO high_load VALUES (%d, %s, ''%s'')',
      [I, FloatToSqlStr(RandomFloat(-100, 100)), RandomStr(10)]));
  end;
end;

{**
  Performs an open test.
}
procedure TZPlainMySQLPerformanceTestCase.RunTestOpen;
begin
  QueryHandle := ExecuteQuery('SELECT * FROM high_load');
end;

{**
   Performs a fetch data
}
procedure TZPlainMySQLPerformanceTestCase.RunTestFetch;

  function GetString(ColumnIndex: Integer; var QueryHandle: PZMySQLResult;
    var RowHandle: PZMySQLRow): string;
  var
    LengthPointer: PLongInt;
    Length: LongInt;
    Buffer: PChar;
  begin
    LengthPointer := PlainDriver.FetchLengths(QueryHandle);
    if LengthPointer <> nil then
      Length  := PLongInt(LongInt(LengthPointer) + ColumnIndex * SizeOf(LongInt))^
    else Length := 0;
    Buffer := PlainDriver.GetFieldData(RowHandle, ColumnIndex);
    Result := '';
    if Buffer <> nil then
      SetString(Result, Buffer, Length);
  end;

var
  RowHandle: PZMySQLRow;
begin
  QueryHandle := ExecuteQuery('SELECT * FROM high_load');
  RowHandle := PlainDriver.FetchRow(QueryHandle);
  while RowHandle <> nil do
  begin
    GetString(0, FQueryHandle, RowHandle);
    GetString(1, FQueryHandle, RowHandle);
    GetString(2, FQueryHandle, RowHandle);
    RowHandle := PlainDriver.FetchRow(QueryHandle);
  end;
end;

{**
  Performs an update test.
}
procedure TZPlainMySQLPerformanceTestCase.RunTestUpdate;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    ExecuteUpdate(Format('UPDATE high_load SET data1=%s, data2=''%s'''
      + ' WHERE hl_id = %d', [FloatToSqlStr(RandomFloat(-100, 100)), RandomStr(10), I]));
  end;
end;

{**
  Performs a delete test.
}
procedure TZPlainMySQLPerformanceTestCase.RunTestDelete;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
    ExecuteUpdate(Format('DELETE FROM high_load WHERE hl_id = %d', [I]));
end;

{**
  Performs a direct update test.
}
procedure TZPlainMySQLPerformanceTestCase.RunTestDirectUpdate;
begin
  RunTestUpdate;
end;
{$ENDIF}


{$IFDEF ENABLE_POSTGRESQL}
{ TZPlainPostgreSQLPerformanceTestCase }
{**
   Check error postgresql operation
}
function TZPlainPostgreSQLPerformanceTestCase.CheckPostgreSQLError: boolean;
var
  msg: string;
begin
  msg := StrPas(FPlainDriver.GetErrorMessage(FHandle));

  if Trim(msg) = '' then
    Result := False
  else
    Result := True
end;

{**
   Connect to database server
}
procedure TZPlainPostgreSQLPerformanceTestCase.Connect;
var
  Port: Integer;
  ConnectStr: string;
begin
  Port := inherited Port;
  if Port = 0 then
    Port := 5432;

  if IsIpAddr(HostName) then
    ConnectStr := Format('hostaddr=%s port=%d dbname=%s user=%s',
       [HostName, Port, Database, UserName, Password])
  else
    ConnectStr := Format('host=%s port=%d dbname=%s user=%s',
       [HostName, Port, Database, UserName, Password]);

  if Password <> '' then
    ConnectStr := ConnectStr + ' password=' + Password;

  { Connect to PostgreSQL database. }
  FHandle := FPlainDriver.ConnectDatabase(PChar(ConnectStr));
  if CheckPostgreSQLError then
    Fail('Error conenction to database server');

  { Turn on transaction mode }
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle,
    'SET TRANSACTION ISOLATION LEVEL READ COMMITTED');
  if CheckPostgreSQLError then
    Fail('Error set transaction isolation level');
  FPlainDriver.Clear(QueryHandle);

  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, 'BEGIN');
  if CheckPostgreSQLError then
    Fail('Fail start transaction');
  FPlainDriver.Clear(QueryHandle);
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZPlainPostgreSQLPerformanceTestCase.DefaultSetUpTest;
begin
  Connect;
  QueryHandle := nil;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZPlainPostgreSQLPerformanceTestCase.DefaultTearDownTest;
begin
  if QueryHandle <> nil then
  begin
    FPlainDriver.Clear(FQueryHandle);
    QueryHandle := nil;
  end;
  Disconnect;
end;

{**
   Disconnect to database server
}
procedure TZPlainPostgreSQLPerformanceTestCase.Disconnect;
begin
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, 'COMMIT');
  if CheckPostgreSQLError then
    Fail('Fail stop transaction');
  FPlainDriver.Clear(QueryHandle);

  FPlainDriver.Finish(FHandle);
  FHandle := nil;
end;

{**
  Executes a query SQL statement.
  @param Query a SQL statement to be executed.
  @return a query result handle.
}
function TZPlainPostgreSQLPerformanceTestCase.ExecuteQuery(
  Query: string): PZPostgreSQLResult;
begin
  Result := FPlainDriver.ExecuteQuery(FHandle, PChar(Query));
  if CheckPostgreSQLError then
    Fail('Error sql query execution');
end;

{**
  Executes an update SQL statement.
  @param Query a SQL statement to be executed.
}
procedure TZPlainPostgreSQLPerformanceTestCase.ExecuteUpdate(
  Query: string);
begin
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, PChar(Query));
  if CheckPostgreSQLError then
    Fail('Error sql update excution');
  FPlainDriver.Clear(FQueryHandle);
end;

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZPlainPostgreSQLPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'plain';
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZPlainPostgreSQLPerformanceTestCase.GetSupportedProtocols: string;
begin
  Result := 'postgresql,postgresql-7,postgresql-8';
end;

{**
  Performs a connect test.
}
procedure TZPlainPostgreSQLPerformanceTestCase.RunTestConnect;
begin
  Connect;
end;

{**
  Performs a delete test.
}
procedure TZPlainPostgreSQLPerformanceTestCase.RunTestDelete;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
    ExecuteUpdate(Format('DELETE FROM high_load WHERE hl_id = %d', [I]));
end;

{**
   Performs a fetch data
}
procedure TZPlainPostgreSQLPerformanceTestCase.RunTestFetch;

  function GetString(RowNo, ColumnIndex: integer): string;
  begin
    SetString(Result, FPlainDriver.GetValue(FQueryHandle, RowNo - 1, ColumnIndex),
    FPlainDriver.GetLength(FQueryHandle, RowNo - 1, ColumnIndex));
  end;

var
  I: Integer;
begin
  QueryHandle := ExecuteQuery('SELECT * FROM high_load');
  for I := 1 to GetRecordCount do
  begin
    GetString(I, 0);
    GetString(I, 1);
    GetString(I, 2);
  end;
  FPlainDriver.Clear(FQueryHandle);
end;

{**
  Performs an insert test.
}
procedure TZPlainPostgreSQLPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    ExecuteUpdate(Format('INSERT INTO high_load VALUES (%d, %s, ''%s'')',
      [I, FloatToSqlStr(RandomFloat(-100, 100)), RandomStr(10)]));
  end;
end;

{**
  Performs an open test.
}
procedure TZPlainPostgreSQLPerformanceTestCase.RunTestOpen;
begin
  FQueryHandle := ExecuteQuery('SELECT * FROM high_load');
  FPlainDriver.Clear(FQueryHandle);
end;

{**
  Performs an update test.
}
procedure TZPlainPostgreSQLPerformanceTestCase.RunTestUpdate;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    ExecuteUpdate(Format('UPDATE high_load SET data1=%s, data2=''%s'''
      + ' WHERE hl_id = %d', [FloatToSqlStr(RandomFloat(-100, 100)), RandomStr(10), I]));
  end;
end;

{**
   Create objects and allocate memory for variables
}
procedure TZPlainPostgreSQLPerformanceTestCase.SetUp;
{ TODO
var
  FPostgreSQL65PlainDriver: IZPostgreSQLPlainDriver;
  FPostgreSQL72PlainDriver: IZPostgreSQLPlainDriver;
 }
begin
{ TODO
  FPostgreSQL65PlainDriver := TZPostgreSQL65PlainDriver.Create;
  FPostgreSQL72PlainDriver := TZPostgreSQL72PlainDriver.Create;

  if Protocol = FPostgreSQL65PlainDriver.GetProtocol then
    PlainDriver := FPostgreSQL65PlainDriver
  else if Protocol = FPostgreSQL72PlainDriver.GetProtocol then
    PlainDriver := FPostgreSQL72PlainDriver
  else PlainDriver := FPostgreSQL72PlainDriver;
  PlainDriver.Initialize;
 }
end;

{**
  The empty Set Up method for connect test.
}
procedure TZPlainPostgreSQLPerformanceTestCase.SetUpTestConnect;
begin

end;

{**
  Performs a direct update test.
}
procedure TZPlainPostgreSQLPerformanceTestCase.RunTestDirectUpdate;
begin
  RunTestUpdate;
end;
{$ENDIF}


{$IFDEF ENABLE_INTERBASE}
{ TZPlainInterbase6SQLPerformanceTestCase }
procedure TZPlainInterbase6SQLPerformanceTestCase.CheckInterbase6Error(Sql: string = '');
var
  Msg: array[0..1024] of Char;
  PStatusVector: PISC_STATUS;
  ErrorMessage, ErrorSqlMessage: string;
  ErrorCode: LongInt;
begin
  if (FStatusVector[0] = 1) and (FStatusVector[1] > 0) then
  begin
    PStatusVector := @FStatusVector;
    PlainDriver.isc_interprete(Msg, @PStatusVector);
    ErrorMessage := StrPas(Msg);

    ErrorCode := PlainDriver.isc_sqlcode(@FStatusVector);
    PlainDriver.isc_sql_interprete(ErrorCode, Msg, 1024);
    ErrorSqlMessage := StrPas(Msg);

    if SQL <> '' then
      SQL := Format('The SQL: %s; ', [SQL]);

    if ErrorMessage <> '' then
    begin
      raise Exception.Create(SQL + Format(
        'SQL Error: %s. Erorr code: %d. %s',
        [ErrorMessage, ErrorCode, ErrorSqlMessage]));
    end;
  end;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.Connect;
var
  DPB: PChar;
  NewDPB: String;
  FDPBLength: Word;
  DBName: array[0..512] of Char;
  PTEB: PISC_TEB;
  Params: TStrings;
begin
  PTEB := nil;
  FHandle := nil;
  FTrHandle := nil;
  FStmtHandle := nil;
  Params := TStringList.Create;

  NewDPB := NewDPB + Char(isc_dpb_version1);
  NewDPB := NewDPB + Char(isc_dpb_user_name) + Char(Length(UserName)) + UserName;
  NewDPB := NewDPB + Char(isc_dpb_password) + Char(Length(Password)) + Password;
  FDPBLength := 1;
  Inc(FDPBLength, 2 + Length(UserName));
  Inc(FDPBLength, 2 + Length(Password));
  DPB := StrAlloc(FDPBLength +1);
  StrPCopy(DPB, NewDPB);

  if HostName = '' then
    StrPCopy(DBName, Database)
  else
    StrPCopy(DBName, HostName + ':' + Database);

  try
    { connect to database }
    FPlainDriver.isc_attach_database(@FStatusVector, StrLen(DBName), DBName,
        @FHandle, FDPBLength, DPB);
    CheckInterbase6Error;

    {allocate transaction structure}
    PTEB := AllocMem(Sizeof(TISC_TEB));
    with PTEB^ do
    begin
      db_handle := @FHandle;
      tpb_length := 0;
      tpb_address := nil;
    end;

    { start transaction }
    FPlainDriver.isc_start_multiple(@FStatusVector, @FTrHandle, 1, PTEB);
    CheckInterbase6Error;
  finally
    Params.Free;
    StrDispose(DPB);
    FreeMem(PTEB);
  end;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.DefaultSetUpTest;
begin
  Connect;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.DefaultTearDownTest;
begin
  Disconnect;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.Disconnect;
begin
  if FStmtHandle <> nil then
  begin
    FreeStatement(FPlainDriver, FStmtHandle, DSQL_Drop);
    FStmtHandle := nil;
  end;

  if FTrHandle <> nil then
  begin
    FPlainDriver.isc_commit_transaction(@FStatusVector, @FTrHandle);
    FTrHandle := nil;
    CheckInterbase6Error;
  end;

  if FHandle <> nil then
  begin
    FPlainDriver.isc_detach_database(@FStatusVector, @FHandle);
    FHandle := nil;
    CheckInterbase6Error;
  end;
end;

function TZPlainInterbase6SQLPerformanceTestCase.ExecuteQuery(
  SQL: string): IZResultSQLDA;
var
  Cursor: string;
  SQLData: IZResultSQLDA;
  StatusVector: TARRAY_ISC_STATUS;
begin
  StmtHandle := nil;
  SQLData := TZResultSQLDA.Create(FPlainDriver, FHandle, FTrHandle);

  try
    PrepareStatement(FPlainDriver, @FHandle, @FTrHandle,
      FDialect, SQL, FStmtHandle);
    PrepareResultSqlData(FPlainDriver, @FHandle, FDialect,
      SQL, FStmtHandle, SQLData);

    FPlainDriver.isc_dsql_execute(@StatusVector, @FTrHandle,
      @FStmtHandle, FDialect, SQLData.GetData);
    CheckInterbase6Error(SQL);

    Cursor := RandomString(12);
    FPlainDriver.isc_dsql_set_cursor_name(@StatusVector,
      @FStmtHandle, PChar(Cursor), 0);
    CheckInterbase6Error(SQL);

    Result := SQLData;
  except
    FreeStatement(FPlainDriver, FStmtHandle, DSQL_Drop);
  end;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.ExecuteSQL(SQL: string);
var
  StmtHandle: TISC_STMT_HANDLE;
  StatusVector: TARRAY_ISC_STATUS;
begin
  StmtHandle := nil;

  try
    PrepareStatement(FPlainDriver, @FHandle, @FTrHandle,
      FDialect, SQL, StmtHandle);

    FPlainDriver.isc_dsql_execute2(@StatusVector, @FTrHandle,
      @StmtHandle, FDialect, nil, nil);
    CheckInterbase6Error(SQL);
  finally
    FreeStatement(FPlainDriver, StmtHandle, DSQL_Drop);
  end;
end;

function TZPlainInterbase6SQLPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'plain';
end;

function TZPlainInterbase6SQLPerformanceTestCase.GetSupportedProtocols: string;
begin
  Result := 'interbase-5,interbase-6,firebird-1.0,firebird-1.5,firebird-2.0,firebird-2.1,firebirdd-1.5,firebirdd-2.0,firebirdd-2.1';
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.RunTestConnect;
begin
  Connect;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.RunTestDelete;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
    ExecuteSql(Format('DELETE FROM HIGH_LOAD WHERE HL_ID = %d', [I]));
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.RunTestFetch;
var
  I: integer;
  FetchStat: word;
  SQLData: IZResultSQLDA;
begin
  SQLData := ExecuteQuery('SELECT * FROM HIGH_LOAD');
  for I := 1 to GetRecordCount do
  begin
    FetchStat := FPlainDriver.isc_dsql_fetch(@FStatusVector,
        @FStmtHandle, FDialect, SqlData.GetData);
    CheckInterbase6Error;
    if FetchStat <> 0 then
      raise Exception.Create('Error fetching data');
    SQLData.GetInt(0);
    SQLData.GetFloat(1);
    SQLData.GetString(2);
  end;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    ExecuteSql(Format('INSERT INTO HIGH_LOAD VALUES (%d, %s, ''%s'')',
      [I, FloatToSqlStr(RandomFloat(-100, 100)), RandomStr(10)]));
  end;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.RunTestOpen;
var
  SQLData: IZResultSQLDA;
begin
  SQLData := ExecuteQuery('SELECT * FROM HIGH_LOAD');
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.RunTestUpdate;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    ExecuteSql(Format('UPDATE HIGH_LOAD SET DATA1=%s, DATA2=''%s'''
      + ' WHERE HL_ID = %d', [FloatToSqlStr(RandomFloat(-100, 100)), RandomStr(10), I]));
  end;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.SetUp;
var
  FFirebird10PlainDriver: IZInterbasePlainDriver;
  FFirebird15PlainDriver: IZInterbasePlainDriver;
  FFirebird20PlainDriver: IZInterbasePlainDriver;
  FFirebird21PlainDriver: IZInterbasePlainDriver;
  FFirebirdd15PlainDriver: IZInterbasePlainDriver;
  FFirebirdd20PlainDriver: IZInterbasePlainDriver;
  FFirebirdd21PlainDriver: IZInterbasePlainDriver;
begin
  if Protocol = 'interbase-5' then
    FDialect := 1
  else FDialect := 3;
  FFirebird10PlainDriver := TZFirebird10PlainDriver.Create;
  FFirebird15PlainDriver := TZFirebird15PlainDriver.Create;
  FFirebird20PlainDriver := TZFirebird20PlainDriver.Create; 
  FFirebird21PlainDriver := TZFirebird21PlainDriver.Create; 
  FFirebirdd15PlainDriver := TZFirebird15PlainDriver.Create; 
  FFirebirdd20PlainDriver := TZFirebirdd20PlainDriver.Create;
  FFirebirdd21PlainDriver := TZFirebirdd21PlainDriver.Create;

  if Protocol = FFirebird10PlainDriver.GetProtocol then
    PlainDriver := FFirebird10PlainDriver 
  else if Protocol = FFirebird15PlainDriver.GetProtocol then 
    PlainDriver := FFirebird15PlainDriver 
  else if Protocol = FFirebird20PlainDriver.GetProtocol then 
    PlainDriver := FFirebird20PlainDriver 
  else if Protocol = FFirebird21PlainDriver.GetProtocol then 
    PlainDriver := FFirebird21PlainDriver 
  else if Protocol = FFirebirdd15PlainDriver.GetProtocol then 
    PlainDriver := FFirebirdd15PlainDriver 
  else if Protocol = FFirebirdd21PlainDriver.GetProtocol then 
    PlainDriver := FFirebird21PlainDriver
  else PlainDriver := FFirebird10PlainDriver;

  PlainDriver.Initialize;
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.SetUpTestConnect;
begin
end;

procedure TZPlainInterbase6SQLPerformanceTestCase.RunTestDirectUpdate;
begin
  RunTestUpdate;
end;
{$ENDIF}

{$IFDEF ENABLE_ASA}
{ TZPlainASASQLPerformanceTestCase }
procedure TZPlainASASQLPerformanceTestCase.CheckASAError(Sql: string = '');
var
  ErrorBuf: array[0..1024] of Char;
  ErrorMessage: string;
begin
  if Handle.SqlCode < SQLE_NOERROR then
  begin
    ErrorMessage := PlainDriver.sqlError_Message( Handle, ErrorBuf, SizeOf( ErrorBuf));
    //SyntaxError Position in SQLCount
    if SQL <> '' then
      SQL := Format( 'The SQL: %s; ', [SQL]);

    if ErrorMessage <> '' then
    begin
      raise Exception.Create( SQL + Format(
        'SQL Error: %s. Error code: %d.',
        [ErrorMessage, Handle.SqlCode]));
    end;
  end;
end;

procedure TZPlainASASQLPerformanceTestCase.Connect;
var
  ConnectionString: String;
begin
  FHandle := nil;
  try
    if FPlainDriver.db_init( @FSQLCA) = 0 then
      CheckASAError;
    FHandle := @FSQLCA;

    if HostName <> '' then
      ConnectionString := ConnectionString + 'ENG="' + HostName + '"; ';
    if UserName <> '' then
      ConnectionString := ConnectionString + 'UID="' + UserName + '"; ';
    if Password <> '' then
      ConnectionString := ConnectionString + 'PWD="' + Password + '"; ';
    if Database <> '' then
    begin
      if CompareText( ExtractFileExt( Database), '.db') = 0 then
        ConnectionString := ConnectionString + 'DBF="' + Database + '"; '
      else
        ConnectionString := ConnectionString + 'DBN="' + Database + '"; ';
    end;

    FPlainDriver.db_string_connect( FHandle, PChar( ConnectionString));
    CheckASAError;

  except
    on E: Exception do begin
      if Assigned( FHandle) then
        FPlainDriver.db_fini( FHandle);
      FHandle := nil;
      raise;
    end;
  end;
end;

procedure TZPlainASASQLPerformanceTestCase.DefaultSetUpTest;
begin
  Connect;
end;

procedure TZPlainASASQLPerformanceTestCase.DefaultTearDownTest;
begin
  Disconnect;
end;

procedure TZPlainASASQLPerformanceTestCase.Disconnect;
begin
  if CursorName <> '' then
  begin
    FPlainDriver.db_close( FHandle, PChar( CursorName));
    CursorName := '';
    CheckASAError;
  end;

  if FStmtNum <> 0 then
  begin
    FPlainDriver.db_dropstmt( FHandle, nil,
     nil, @FStmtNum);
    FStmtNum := 0;
    CheckASAError;
  end;

  if FHandle <> nil then
  begin
    FPlainDriver.db_commit( FHandle, 1);
    CheckASAError;

    FPlainDriver.db_string_disconnect( FHandle, nil);
    CheckASAError;

    FHandle := nil;
    if FPlainDriver.db_fini( @FSQLCA) = 0 then
      raise Exception.Create( 'Error closing SQLCA');
  end;
end;

function TZPlainASASQLPerformanceTestCase.ExecuteQuery(
  SQL: string): IZASASQLDA;
var
  SQLData: IZASASQLDA;
begin
  StmtNum := 0;
  CursorName := RandomString(12);
  SQLData := TZASASQLDA.Create(FPlainDriver, FHandle, CursorName);

  try
    FPlainDriver.db_prepare_describe( FHandle, nil, @StmtNum,
      PChar( SQL), SQLData.GetData, SQL_PREPARE_DESCRIBE_STMTNUM +
      SQL_PREPARE_DESCRIBE_OUTPUT, 0);
    CheckASAError( SQL);

    if SQLData.GetData^.sqld > SQLData.GetData^.sqln then
    begin
      SQLData.AllocateSQLDA( SQLData.GetData^.sqld);
      FPlainDriver.db_describe( FHandle, nil, @StmtNum,
        SQLData.GetData, SQL_DESCRIBE_OUTPUT);
      CheckASAError( SQL);
    end;

    SQLData.InitFields;
    FPlainDriver.db_open( FHandle, PChar( CursorName), nil, @StmtNum,
        nil, 20, 0, CUR_OPEN_DECLARE + CUR_UPDATE);
    CheckASAError( SQL);

    Result := SQLData;
  except
    if FStmtNum <> 0 then
    begin
      FPlainDriver.db_dropstmt( FHandle, nil,
         nil, @FStmtNum);
      FStmtNum := 0;
      CursorName := '';
    end;
  end;
end;

procedure TZPlainASASQLPerformanceTestCase.ExecuteSQL(SQL: string);
begin
  FPlainDriver.db_execute_imm( FHandle, PChar( SQL));
  CheckASAError(SQL);
end;

function TZPlainASASQLPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'plain';
end;

function TZPlainASASQLPerformanceTestCase.GetSupportedProtocols: string;
begin
  Result := 'ASA7,ASA8,ASA9';
end;

procedure TZPlainASASQLPerformanceTestCase.RunTestConnect;
begin
  Connect;
end;

procedure TZPlainASASQLPerformanceTestCase.RunTestDelete;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
    ExecuteSql(Format('DELETE FROM HIGH_LOAD WHERE HL_ID = %d', [I]));
end;

procedure TZPlainASASQLPerformanceTestCase.RunTestFetch;
var
  I: integer;
  SQLData: IZASASQLDA;
begin
  SQLData := ExecuteQuery('SELECT * FROM HIGH_LOAD');
  for I := 1 to GetRecordCount do
  begin
    FPlainDriver.db_fetch( FHandle,
      PChar( FCursorName), CUR_RELATIVE, 1, SQLData.GetData, BlockSize, CUR_FORREGULAR);
    CheckASAError;
    SQLData.GetInt(0);
    SQLData.GetFloat(1);
    SQLData.GetString(2);
  end;
end;

procedure TZPlainASASQLPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    ExecuteSql(Format('INSERT INTO HIGH_LOAD VALUES (%d, %s, ''%s'')',
      [I, FloatToSqlStr(RandomFloat(-100, 100)), RandomStr(10)]));
  end;
end;

procedure TZPlainASASQLPerformanceTestCase.RunTestOpen;
var
  SQLData: IZASASQLDA;
begin
  SQLData := ExecuteQuery('SELECT * FROM HIGH_LOAD');
end;

procedure TZPlainASASQLPerformanceTestCase.RunTestUpdate;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    ExecuteSql(Format('UPDATE HIGH_LOAD SET DATA1=%s, DATA2=''%s'''
      + ' WHERE HL_ID = %d', [FloatToSqlStr(RandomFloat(-100, 100)), RandomStr(10), I]));
  end;
end;

procedure TZPlainASASQLPerformanceTestCase.SetUp;
var
  FASA7PlainDriver: IZASA7PlainDriver;
  FASA8PlainDriver: IZASA8PlainDriver;
  FASA9PlainDriver: IZASA9PlainDriver;
begin
  FASA7PlainDriver := TZASA7PlainDriver.Create;
  FASA8PlainDriver := TZASA8PlainDriver.Create;
  FASA9PlainDriver := TZASA9PlainDriver.Create;

  if Protocol = FASA7PlainDriver.GetProtocol then
    PlainDriver := FASA7PlainDriver
  else if Protocol = FASA8PlainDriver.GetProtocol then
    PlainDriver := FASA8PlainDriver
  else  if Protocol = FASA9PlainDriver.GetProtocol then
    PlainDriver := FASA9PlainDriver;

  PlainDriver.Initialize;
end;

procedure TZPlainASASQLPerformanceTestCase.SetUpTestConnect;
begin
end;

procedure TZPlainASASQLPerformanceTestCase.RunTestDirectUpdate;
begin
  RunTestUpdate;
end;
{$ENDIF}

initialization
{$IFDEF ENABLE_MYSQL}
  TestFramework.RegisterTest(TZPlainMySQLPerformanceTestCase.Suite);
{$ENDIF}
{$IFDEF ENABLE_POSTGRESQL}
  TestFramework.RegisterTest(TZPlainPostgreSQLPerformanceTestCase.Suite);
{$ENDIF}
{$IFDEF ENABLE_INTERBASE}
   TestFramework.RegisterTest(TZPlainInterbase6SQLPerformanceTestCase.Suite);
{$ENDIF}
{$IFDEF ENABLE_ASA}
  TestFramework.RegisterTest(TZPlainASASQLPerformanceTestCase.Suite);
{$ENDIF}
end.

