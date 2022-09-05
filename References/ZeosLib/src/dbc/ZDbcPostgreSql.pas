{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcPostgreSql;

interface

{$I ZDbc.inc}

uses
  Types, ZCompatibility, Classes, SysUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainPostgreSqlDriver, ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser;

type

  {** Implements PostgreSQL Database Driver. }
  TZPostgreSQLDriver = class(TZAbstractDriver)
  private
    FPlainDrivers: Array of IZPostgreSQLPlainDriver;
  protected
    function GetPlainDriver(const Url: string): IZPostgreSQLPlainDriver;
  public
    constructor Create;
    function Connect(const Url: string; Info: TStrings): IZConnection; override;

    function GetSupportedProtocols: TStringDynArray; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Defines a PostgreSQL specific connection. }
  IZPostgreSQLConnection = interface(IZConnection)
    ['{8E62EA93-5A49-4F20-928A-0EA44ABCE5DB}']

    function IsOidAsBlob: Boolean;

    function GetTypeNameByOid(Id: Oid): string;
    function GetPlainDriver: IZPostgreSQLPlainDriver;
    function GetConnectionHandle: PZPostgreSQLConnect;
    function GetServerMajorVersion: Integer;
    function GetServerMinorVersion: Integer;
    function GetCharactersetCode: TZPgCharactersetType;
  end;

  {** Implements PostgreSQL Database Connection. }
  TZPostgreSQLConnection = class(TZAbstractConnection, IZPostgreSQLConnection)
  private
    FHandle: PZPostgreSQLConnect;
    FBeginRequired: Boolean;
    FTypeList: TStrings;
    FPlainDriver: IZPostgreSQLPlainDriver;
    FOidAsBlob: Boolean;
    FClientCodePage: string;
    FCharactersetCode: TZPgCharactersetType;
    FServerMajorVersion: Integer;
    FServerMinorVersion: Integer;
    FServerSubVersion: Integer;
    FNoticeProcessor: TZPostgreSQLNoticeProcessor;
  protected
    function BuildConnectStr: AnsiString;
    procedure StartTransactionSupport;
    procedure LoadServerVersion;
  public
    constructor Create(Driver: IZDriver; const Url: string;
      PlainDriver: IZPostgreSQLPlainDriver; const HostName: string; Port: Integer;
      const Database: string; const User: string; const Password: string; Info: TStrings);
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;

    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence; override;

    procedure Commit; override;
    procedure Rollback; override;
    //2Phase Commit Support initially for PostgresSQL (firmos) 21022006
    procedure PrepareTransaction(const transactionid: string);override;
    procedure CommitPrepared(const transactionid:string);override;
    procedure RollbackPrepared(const transactionid:string);override;

    procedure Open; override;
    procedure Close; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    function IsOidAsBlob: Boolean;

    function GetTypeNameByOid(Id: Oid): string;
    function GetPlainDriver: IZPostgreSQLPlainDriver;
    function GetConnectionHandle: PZPostgreSQLConnect;

    function GetHostVersion: Integer; override;
    function GetServerMajorVersion: Integer;
    function GetServerMinorVersion: Integer;
    function GetServerSubVersion: Integer;

    function PingServer: Integer; override;
    function GetCharactersetCode: TZPgCharactersetType;
  end;

  {** Implements a Postgres sequence. }
  TZPostgreSQLSequence = class(TZAbstractSequence)
  public
    function GetCurrentValue: Int64; override;
    function GetNextValue: Int64; override;
    function  GetCurrentValueSQL:String;override;
    function  GetNextValueSQL:String;override;
  end;


var
  {** The common driver manager object. }
  PostgreSQLDriver: IZDriver;

implementation

uses
  ZMessages, ZSysUtils, ZDbcUtils, ZDbcPostgreSqlStatement,
  ZDbcPostgreSqlUtils, ZDbcPostgreSqlMetadata, ZPostgreSqlToken,
  ZPostgreSqlAnalyser;

procedure DefaultNoticeProcessor(arg: Pointer; message: PAnsiChar); cdecl;
begin
DriverManager.LogMessage(lcOther,'Postgres NOTICE',message);
end;
{ TZPostgreSQLDriver }

{**
  Constructs this object with default properties.
}
constructor TZPostgreSQLDriver.Create;
begin
  SetLength(FPlainDrivers,2);
  FPlainDrivers[0]  := TZPostgreSQL7PlainDriver.Create;
  FPlainDrivers[1]  := TZPostgreSQL8PlainDriver.Create;
end;

{**
  Attempts to make a database connection to the given URL.
  The driver should return "null" if it realizes it is the wrong kind
  of driver to connect to the given URL.  This will be common, as when
  the JDBC driver manager is asked to connect to a given URL it passes
  the URL to each loaded driver in turn.

  <P>The driver should raise a SQLException if it is the right
  driver to connect to the given URL, but has trouble connecting to
  the database.

  <P>The java.util.Properties argument can be used to passed arbitrary
  string tag/value pairs as connection arguments.
  Normally at least "user" and "password" properties should be
  included in the Properties.

  @param url the URL of the database to which to connect
  @param info a list of arbitrary string tag/value pairs as
    connection arguments. Normally at least a "user" and
    "password" property should be included.
  @return a <code>Connection</code> object that represents a
    connection to the URL
}
function TZPostgreSQLDriver.Connect(const Url: string; Info: TStrings): IZConnection;
var
  TempInfo: TStrings;
  HostName, Database, UserName, Password: string;
  Port: Integer;
  PlainDriver: IZPostgreSQLPlainDriver;
begin
  TempInfo := TStringList.Create;
  try
    PlainDriver := GetPlainDriver(Url);
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, TempInfo);
    Result := TZPostgreSQLConnection.Create(Self, Url, PlainDriver, HostName,
      Port, Database, UserName, Password, TempInfo);
  finally
    TempInfo.Free;
  end;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZPostgreSQLDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZPostgreSQLDriver.GetMinorVersion: Integer;
begin
  Result := 3;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZPostgreSQLDriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZPostgreSQLTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZPostgreSQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZPostgreSQLStatementAnalyser.Create;
  Result := Analyser;
end;

{**
  Get a name of the supported subprotocol.
  For example: postgresql74 or postgresql81
}
function TZPostgreSQLDriver.GetSupportedProtocols: TStringDynArray;
var
   i: smallint;
begin
  SetLength(Result, high(FPlainDrivers)+2);
  // Generic driver
  Result[0] := 'postgresql';
  For i := 0 to high(FPlainDrivers) do
    Result[i+1] := FPlainDrivers[i].GetProtocol;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected protocol.
}
function TZPostgreSQLDriver.GetPlainDriver(const Url: string): IZPostgreSQLPlainDriver;
var
  Protocol: string;
  i: smallint;
begin
  Protocol := ResolveConnectionProtocol(Url, GetSupportedProtocols);
  For i := 0 to high(FPlainDrivers) do
    if Protocol = FPlainDrivers[i].GetProtocol then
      begin
        Result := FPlainDrivers[i];
        break;
      end;
  // Generic driver
  If result = nil then
    Result := FPlainDrivers[1];    // Postgresql 8
  Result.Initialize;
end;


{ TZPostgreSQLConnection }

{**
  Constructs this object and assignes the main properties.
  @param Driver the parent ZDBC driver.
  @param PlainDriver a PostgreSQL plain driver.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZPostgreSQLConnection.Create(Driver: IZDriver; const Url: string;
  PlainDriver: IZPostgreSQLPlainDriver; const HostName: string; Port: Integer;
  const Database, User, Password: string; Info: TStrings);
begin
  inherited Create(Driver, Url, HostName, Port, Database, User, Password, Info,
    TZPostgreSQLDatabaseMetadata.Create(Self, Url, Info));

  { Sets a default PostgreSQL port }
  if Self.Port = 0 then
     Self.Port := 5432;

  { Define connect options. }
  if Info.Values['beginreq'] <> '' then
    FBeginRequired := StrToBoolEx(Info.Values['beginreq'])
  else
    FBeginRequired := True;

  FPlainDriver := PlainDriver;
  Self.PlainDriver := PlainDriver;
  TransactIsolationLevel := tiNone;

  { Processes connection properties. }
  if Info.Values['oidasblob'] <> '' then
    FOidAsBlob := StrToBoolEx(Info.Values['oidasblob'])
  else
    FOidAsBlob := False;
  FClientCodePage := Trim(Info.Values['codepage']);
  FCharactersetCode := pg_CS_code(FClientCodePage);
//  DriverManager.LogError(lcOther,'','Create',Integer(FCharactersetCode),'');
  FNoticeProcessor := DefaultNoticeProcessor;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLConnection.Destroy;
begin
  if FTypeList <> nil then
    FTypeList.Free;
  inherited Destroy;
end;

{**
  Builds a connection string for PostgreSQL.
  @return a built connection string.
}
function TZPostgreSQLConnection.BuildConnectStr: AnsiString;
var
  ConnectTimeout: Integer;
  // backslashes and single quotes must be escaped with backslashes
  function EscapeValue(AValue: AnsiString):AnsiString;
  begin
    Result := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
    Result := StringReplace(Result, '''', '\''', [rfReplaceAll]);
  end;

  //parameters should be separated by whitespace
  procedure AddParamToResult(AParam, AValue: AnsiString);
  begin
    if Result <> '' then
      Result := Result + ' ';

    Result := Result + AParam+'='+QuotedStr(EscapeValue(AValue));
  end;
begin
  //Init the result to empty string.
  Result := '';
  //Entering parameters from the ZConnection
  If IsIpAddr(HostName) then
    AddParamToResult('hostaddr', HostName)
  else
    AddParamToResult('host', HostName);

  AddParamToResult('port', IntToStr(Port));
  AddParamToResult('dbname', Database);
  AddParamToResult('user', User);
  AddParamToResult('password', Password);

  If Info.Values['sslmode'] <> '' then
  begin
    // the client (>= 7.3) sets the ssl mode for this connection
    // (possible values are: require, prefer, allow, disable)
    AddParamToResult('sslmode', Info.Values['sslmode']);
  end
  else if Info.Values['requiressl'] <> '' then
  begin
    // the client (< 7.3) sets the ssl encription for this connection
    // (possible values are: 0,1)
    AddParamToResult('requiressl', Info.Values['requiressl']);
  end;

  { Sets a connection timeout. }
  ConnectTimeout := StrToIntDef(Info.Values['timeout'], -1);
  if ConnectTimeout >= 0 then
    AddParamToResult('connect_timeout', IntToStr(ConnectTimeout));
end;

{**
  Checks is oid should be treated as Large Object.
  @return <code>True</code> if oid should represent a Large Object. 
}
function TZPostgreSQLConnection.IsOidAsBlob: Boolean;
begin
  Result := FOidAsBlob;
end;

{**
  Starts a transaction support.
}
procedure TZPostgreSQLConnection.StartTransactionSupport;
var
  QueryHandle: PZPostgreSQLResult;
   SQL: PAnsiChar;
begin
  if TransactIsolationLevel <> tiNone then
  begin
    if FBeginRequired then
    begin
      SQL := 'BEGIN';
      QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
      CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
      FPlainDriver.Clear(QueryHandle);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end;

    if TransactIsolationLevel = tiReadCommitted then
    begin
      SQL := 'SET TRANSACTION ISOLATION LEVEL READ COMMITTED';
      QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
      CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
      FPlainDriver.Clear(QueryHandle);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end
    else if TransactIsolationLevel = tiSerializable then
    begin
      SQL := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
      QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
      CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
      FPlainDriver.Clear(QueryHandle);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end
    else
      raise EZSQLException.Create(SIsolationIsNotSupported);
  end;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZPostgreSQLConnection.Open;
var
  LogMessage: string;
  QueryHandle: PZPostgreSQLResult;
  SQL: PAnsiChar;
begin
  if not Closed then
    Exit;

  LogMessage := Format('CONNECT TO "%s" AS USER "%s"', [Database, User]);

  { Connect to PostgreSQL database. }
  FHandle := FPlainDriver.ConnectDatabase(PAnsiChar(BuildConnectStr));
  try
    if FPlainDriver.GetStatus(FHandle) = CONNECTION_BAD then
    begin
      CheckPostgreSQLError(nil, FPlainDriver, FHandle,
                            lcConnect, LogMessage,nil)
    end
    else
      DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol, LogMessage);

    { Set the notice processor (default = nil)}

    FPlainDriver.SetNoticeProcessor(FHandle,FNoticeProcessor,nil);

    { Sets a client codepage. } 
    if FClientCodePage <> '' then
    begin
    {$IFDEF DELPHI12_UP}
      SQL := PAnsiChar(utf8string(Format('SET CLIENT_ENCODING TO ''%s''',
                                          [FClientCodePage])));
    {$ELSE}
      SQL := PAnsiChar(Format('SET CLIENT_ENCODING TO ''%s''',
                              [FClientCodePage]));
    {$ENDIF}
      QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
      CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute,
                            SQL,QueryHandle);
      FPlainDriver.Clear(QueryHandle);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end;

    { Turn on transaction mode }
    StartTransactionSupport;
    { Setup notification mechanism }
    //  PQsetNoticeProcessor(FHandle, NoticeProc, Self);

    inherited Open;
  finally
    if self.IsClosed and (Self.FHandle <> nil) then
    begin
      FPlainDriver.Finish(Self.FHandle);
      Self.FHandle := nil;
    end;
  end;
end;

procedure TZPostgreSQLConnection.PrepareTransaction(const transactionid: string);
var
   QueryHandle: PZPostgreSQLResult;
   SQL: PAnsiChar;
   Temp: string;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    Temp:='PREPARE TRANSACTION '''+copy(transactionid,1,200)+'''';
    SQL := PAnsiChar(Temp);
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    FPlainDriver.Clear(QueryHandle);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    StartTransactionSupport;
  end;
end;

{**
  Creates a <code>Statement</code> object for sending
  SQL statements to the database.
  SQL statements without parameters are normally
  executed using Statement objects. If the same SQL statement
  is executed many times, it is more efficient to use a
  <code>PreparedStatement</code> object.
  <P>
  Result sets created using the returned <code>Statement</code>
  object will by default have forward-only type and read-only concurrency.

  @param Info a statement parameters.
  @return a new Statement object
}
function TZPostgreSQLConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
    Open;
  Result := TZPostgreSQLStatement.Create(FPlainDriver, Self, Info);
end;

{**
  Creates a <code>PreparedStatement</code> object for sending
  parameterized SQL statements to the database.

  A SQL statement with or without IN parameters can be
  pre-compiled and stored in a PreparedStatement object. This
  object can then be used to efficiently execute this statement
  multiple times.

  <P><B>Note:</B> This method is optimized for handling
  parametric SQL statements that benefit from precompilation. If
  the driver supports precompilation,
  the method <code>prepareStatement</code> will send
  the statement to the database for precompilation. Some drivers
  may not support precompilation. In this case, the statement may
  not be sent to the database until the <code>PreparedStatement</code> is
  executed.  This has no direct effect on users; however, it does
  affect which method throws certain SQLExceptions.

  Result sets created using the returned PreparedStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?' IN
    parameter placeholders
  @param Info a statement parameters.
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZPostgreSQLConnection.CreatePreparedStatement(
  const SQL: string; Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  Result := TZPostgreSQLPreparedStatement.Create(FPlainDriver,
    Self, SQL, Info);
end;


{**
  Creates a <code>CallableStatement</code> object for calling
  database stored procedures (functions in PostgreSql).
  The <code>CallableStatement</code> object provides
  methods for setting up its IN and OUT parameters, and
  methods for executing the call to a stored procedure.

  <P><B>Note:</B> This method is optimized for handling stored
  procedure call statements. Some drivers may send the call
  statement to the database when the method <code>prepareCall</code>
  is done; others
  may wait until the <code>CallableStatement</code> object
  is executed. This has no
  direct effect on users; however, it does affect which method
  throws certain SQLExceptions.

  Result sets created using the returned CallableStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?'
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZPostgreSQLConnection.CreateCallableStatement(
  const SQL: string; Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  Result := TZPostgreSQLCallableStatement.Create(Self, SQL, Info);
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.Commit;
var
  QueryHandle: PZPostgreSQLResult;
  SQL: PAnsiChar;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    SQL := 'COMMIT';
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    FPlainDriver.Clear(QueryHandle);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

    StartTransactionSupport;
  end;
end;

procedure TZPostgreSQLConnection.CommitPrepared(const transactionid: string);
var
     QueryHandle: PZPostgreSQLResult;
     SQL: PAnsiChar;
     Temp: string;
begin
  if (TransactIsolationLevel = tiNone) and not Closed then
  begin
    Temp:='COMMIT PREPARED '''+copy(transactionid,1,200)+'''';
    SQL := PAnsiChar(Temp);
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    FPlainDriver.Clear(QueryHandle);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    StartTransactionSupport;
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.Rollback;
var
  QueryHandle: PZPostgreSQLResult;
  SQL: PAnsiChar;
begin
  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    SQL := 'ROLLBACK';
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    FPlainDriver.Clear(QueryHandle);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

    StartTransactionSupport;
  end;
end;

procedure TZPostgreSQLConnection.RollbackPrepared(const transactionid: string);
var
   QueryHandle: PZPostgreSQLResult;
   SQL: PAnsiChar;
   Temp: string;
begin
  if (TransactIsolationLevel = tiNone) and not Closed then
  begin
    Temp:='ROLLBACK PREPARED '''+copy(transactionid,1,200)+'''';
    SQL := PAnsiChar(Temp);
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    FPlainDriver.Clear(QueryHandle);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    StartTransactionSupport;
  end;
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZPostgreSQLConnection.Close;
var
  LogMessage: string;
begin
  if not Closed then
  begin
    FPlainDriver.Finish(FHandle);
    FHandle := nil;
    LogMessage := Format('DISCONNECT FROM "%s"', [Database]);
    DriverManager.LogMessage(lcDisconnect, FPlainDriver.GetProtocol, LogMessage);
  end;
  inherited Close;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZPostgreSQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  QueryHandle: PZPostgreSQLResult;
  SQL: PAnsiChar;
begin
  if not (Level in [tiNone, tiReadCommitted, tiSerializable]) then
    raise EZSQLException.Create(SIsolationIsNotSupported);

  if (TransactIsolationLevel <> tiNone) and not Closed then
  begin
    SQL := 'END';
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    FPlainDriver.Clear(QueryHandle);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  end;

  inherited SetTransactionIsolation(Level);

  if not Closed then
    StartTransactionSupport;
end;

{**
  Gets a reference to PostgreSQL connection handle.
  @return a reference to PostgreSQL connection handle.
}
function TZPostgreSQLConnection.GetConnectionHandle: PZPostgreSQLConnect;
begin
  Result := FHandle;
end;

{**
  Gets a PostgreSQL plain driver interface.
  @return a PostgreSQL plain driver interface.
}
function TZPostgreSQLConnection.GetPlainDriver: IZPostgreSQLPlainDriver;
begin
  Result := FPlainDriver;
end;

{**
  Gets a type name by it's oid number.
  @param Id a type oid number.
  @return a type name or empty string if there was no such type found.
}
function TZPostgreSQLConnection.GetTypeNameByOid(Id: Oid): string;
var
  I, Index: Integer;
  QueryHandle: PZPostgreSQLResult;
  SQL: PAnsiChar;
  TypeCode, BaseTypeCode: Integer;
  TypeName: string;
  LastVersion, IsEnum: boolean;
begin
  if Closed then
     Open;

  if (GetServerMajorVersion < 7 ) or
    ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
    LastVersion := True
  else
    LastVersion := False;

  { Fill the list with existed types }
  if not Assigned(FTypeList) then
  begin
    if LastVersion then
      SQL := 'SELECT oid, typname FROM pg_type WHERE oid<10000'
    else
      SQL := 'SELECT oid, typname, typbasetype,typtype FROM pg_type' + 
             ' WHERE (typtype = ''b'' and oid < 10000) OR typtype = ''p'' OR typtype = ''e'' OR typbasetype<>0 ORDER BY oid'; 

    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
    CheckPostgreSQLError(Self, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

    FTypeList := TStringList.Create;
    for I := 0 to FPlainDriver.GetRowCount(QueryHandle)-1 do
    begin
      TypeCode := StrToIntDef(StrPas(
        FPlainDriver.GetValue(QueryHandle, I, 0)), 0);
      isEnum := LowerCase(StrPas(FPlainDriver.GetValue(QueryHandle, I, 3))) = 'e'; 
      if isEnum then 
        TypeName := 'enum' 
      else 
        TypeName := StrPas(FPlainDriver.GetValue(QueryHandle, I, 1)); 

      if LastVersion then
        BaseTypeCode := 0
      else
        BaseTypeCode := StrToIntDef(StrPas(
          FPlainDriver.GetValue(QueryHandle, I, 2)), 0);

      if BaseTypeCode <> 0 then
      begin
        Index := FTypeList.IndexOfObject(TObject(BaseTypeCode));
        if Index >= 0 then
          TypeName := FTypeList[Index]
        else
          TypeName := '';
      end;
      FTypeList.AddObject(TypeName, TObject(TypeCode));
    end;
    FPlainDriver.Clear(QueryHandle);
  end;

  I := FTypeList.IndexOfObject(TObject(Id));
  if I >= 0 then
    Result := FTypeList[I]
  else
    Result := '';
end;

{**
  Gets the host's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this server's full version number
}
function TZPostgreSQLConnection.GetHostVersion: Integer;
begin
 Result := GetServerMajorVersion*1000000+GetServerMinorversion*1000+GetServerSubversion;
end;

{**
  Gets a server major version.
  @return a server major version number.
}
function TZPostgreSQLConnection.GetServerMajorVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerMajorVersion;
end;

{**
  Gets a server minor version.
  @return a server minor version number.
}
function TZPostgreSQLConnection.GetServerMinorVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerMinorVersion;
end;

{**
  Gets a server sub version.
  @return a server sub version number.
}
function TZPostgreSQLConnection.GetServerSubVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerSubVersion;
end;

{**
  Loads a server major and minor version numbers.
}
procedure TZPostgreSQLConnection.LoadServerVersion;
var
  Temp: string;
  List: TStrings;
  QueryHandle: PZPostgreSQLResult;
  SQL: PAnsiChar;
begin
  if Closed then
    Open;
  SQL := 'SELECT version()';
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, SQL);
  CheckPostgreSQLError(Self, FPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  Temp := FPlainDriver.GetValue(QueryHandle, 0, 0);
  FPlainDriver.Clear(QueryHandle);

  List := TStringList.Create;
  try
    { Splits string by space }
    PutSplitString(List, Temp, ' ');
    { first - PostgreSQL, second X.Y.Z}
    Temp := List.Strings[1];
    { Splits string by dot }
    PutSplitString(List, Temp, '.');

    FServerMajorVersion := StrToIntDef(List.Strings[0], 0);
    if List.Count > 1 then
      FServerMinorVersion := GetMinorVersion(List.Strings[1])
    else
      FServerMinorVersion := 0;
    if List.Count > 2 then
      FServerSubVersion := GetMinorVersion(List.Strings[2])
    else
      FServerSubVersion := 0;
  finally
    List.Free;
  end;
end;

{** 
Ping Current Connection's server, if client was disconnected, 
the connection is resumed. 
@return 0 if succesfull or error code if any error occurs 
} 
function TZPostgreSQLConnection.PingServer: Integer; 
  const 
    PING_ERROR_ZEOSCONNCLOSED = -1; 
  var 
    Closing: boolean;
    res: PZPostgreSQLResult;
    isset: boolean;
begin
  Result := PING_ERROR_ZEOSCONNCLOSED;
  Closing := FHandle = nil;
  if Not(Closed or Closing) then
  begin
    res := FPlainDriver.ExecuteQuery(FHandle,'');
    isset := assigned(res);
    FPlainDriver.Clear(res);
    if isset and (FPlainDriver.GetStatus(FHandle) = CONNECTION_OK) then
      Result := 0
    else
      try
        FPlainDriver.Reset(FHandle);
        res := FPlainDriver.ExecuteQuery(FHandle,'');
        isset := assigned(res);
        FPlainDriver.Clear(res);
        if isset and (FPlainDriver.GetStatus(FHandle) = CONNECTION_OK) then
          Result := 0;
      except
        Result := 1;
      end;
  end; 
end; 

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZPostgreSQLConnection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := TZPostgreSQLSequence.Create(Self, Sequence, BlockSize);
end;

{**
  Get characterset in terms of enumerated number.
  @return characterset in terms of enumerated number.
}
function TZPostgreSQLConnection.GetCharactersetCode: TZPgCharactersetType;
begin
  Result := FCharactersetCode;
end;

{**
  Gets the current unique key generated by this sequence.
  @param the last generated unique key.
}
function TZPostgreSQLSequence.GetCurrentValue: Int64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery(
    Format('SELECT CURRVAL(''%s'')', [Name]));
  if ResultSet.Next then
    Result := ResultSet.GetLong(1)
  else
    Result := inherited GetCurrentValue;
  ResultSet.Close;
  Statement.Close;
end;

{**
  Gets the next unique key generated by this sequence.
  @param the next generated unique key.
}
function TZPostgreSQLSequence.GetCurrentValueSQL: String;
begin
  result:=Format(' CURRVAL(''%s'') ', [Name]);
end;

function TZPostgreSQLSequence.GetNextValue: Int64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery(
    Format('SELECT NEXTVAL(''%s'')', [Name]));
  if ResultSet.Next then
    Result := ResultSet.GetLong(1)
  else
    Result := inherited GetNextValue;
  ResultSet.Close;
  Statement.Close;
end;

function TZPostgreSQLSequence.GetNextValueSQL: String;
begin
 result:=Format(' NEXTVAL(''%s'') ', [Name]);
end;

initialization
  PostgreSQLDriver := TZPostgreSQLDriver.Create;
  DriverManager.RegisterDriver(PostgreSQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(PostgreSQLDriver);
  PostgreSQLDriver := nil;
end.

