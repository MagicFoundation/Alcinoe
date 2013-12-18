{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySql;

interface

{$I ZDbc.inc}

uses
  Types, ZCompatibility, Classes, SysUtils, ZDbcIntfs, ZDbcConnection, ZPlainMySqlDriver,
  ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser, ZPlainMySqlConstants;

type

  {** Implements MySQL Database Driver. }

  { TZMySQLDriver }

  TZMySQLDriver = class(TZAbstractDriver)
  private
    FPlainDrivers: Array of IZMySQLPlainDriver;
  protected
    function GetPlainDriver(const Url: string; Info: TStrings = nil): IZMySQLPlainDriver; // changed by tohenk, 2009-10-11
  public
    constructor Create;
    function Connect(const Url: string; Info: TStrings): IZConnection; override;
    function GetSupportedProtocols: TStringDynArray; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
    function GetClientVersion(const Url: string): Integer; override;
  end;

  {** Represents a MYSQL specific connection interface. }
  IZMySQLConnection = interface (IZConnection)
    ['{68E33DD3-4CDC-4BFC-8A28-E9F2EE94E457}']

    function GetPlainDriver: IZMySQLPlainDriver;
    function GetConnectionHandle: PZMySQLConnect;
  end;

  {** Implements MySQL Database Connection. }
  TZMySQLConnection = class(TZAbstractConnection, IZMySQLConnection)
  private
    FCatalog: string;
    FPlainDriver: IZMySQLPlainDriver;
    FHandle: PZMySQLConnect;
    FClientCodePage: string;
  public
    constructor Create(Driver: IZDriver; const Url: string;
      PlainDriver: IZMySQLPlainDriver; const HostName: string; Port: Integer;
      const Database: string; const User: string; const Password: string; Info: TStrings);
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    function PingServer: Integer; override;
    function EscapeString(Value: AnsiString): AnsiString; override;

    procedure Open; override;
    procedure Close; override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    procedure SetAutoCommit(AutoCommit: Boolean); override;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;
    {END ADDED by fduenas 15-06-2006}
    function GetPlainDriver: IZMySQLPlainDriver;
    function GetConnectionHandle: PZMySQLConnect;
    function GetDescription: AnsiString;
  end;


var
  {** The common driver manager object. }
  MySQLDriver: IZDriver;

implementation

uses
  ZMessages, ZSysUtils, ZDbcUtils, ZDbcMySqlStatement, ZMySqlToken,
  ZDbcMySqlUtils, ZDbcMySqlMetadata, ZMySqlAnalyser, TypInfo, Math;

{ TZMySQLDriver }

{**
  Constructs this object with default properties.
}
constructor TZMySQLDriver.Create;
begin
  SetLength(FPlainDrivers,4);
  FPlainDrivers[0]  := TZMySQL41PlainDriver.Create;
  FPlainDrivers[1]   := TZMySQL5PlainDriver.Create;
  // embedded drivers
  FPlainDrivers[2]  := TZMySQLD41PlainDriver.Create;
  FPlainDrivers[3]   := TZMySQLD5PlainDriver.Create;
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
function TZMySQLDriver.Connect(const Url: string; Info: TStrings): IZConnection;
var
  TempInfo: TStrings;
  HostName, Database, UserName, Password: string;
  Port: Integer;
  PlainDriver: IZMySQLPlainDriver;
begin
  TempInfo := TStringList.Create;
  try
    PlainDriver := GetPlainDriver(Url, Info); // changed by tohenk, 2009-10-11
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, TempInfo);
    // changed by tohenk, 2009-10-11
    // PATCH ADDED BY tohenk
    //if PlainDriver <> nil then
    //  PlainDriver.BuildArguments(TempInfo);
    Result := TZMySQLConnection.Create(Self, Url, PlainDriver, HostName, Port,
      Database, UserName, Password, TempInfo);
  finally
    TempInfo.Free;
  end;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZMySQLDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZMySQLDriver.GetMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZMySQLDriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZMySQLTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZMySQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZMySQLStatementAnalyser.Create;
  Result := Analyser;
end;

{**
  Get a name of the supported subprotocol.
  For example: mysql, oracle8 or postgresql72
}
function TZMySQLDriver.GetSupportedProtocols: TStringDynArray;
var
   i: smallint;
begin
  SetLength(Result, high(FPlainDrivers)+2);
  // Generic driver
  Result[0] := 'mysql';
  For i := 0 to high(FPlainDrivers) do
    Result[i+1] := FPlainDrivers[i].GetProtocol;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected protocol.
}
function TZMySQLDriver.GetPlainDriver(const Url: string; Info: TStrings = nil): IZMySQLPlainDriver; // changed by tohenk, 2009-10-11
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
    Result := FPlainDrivers[1];    // mysql-5
  // added by tohenk, 2009-10-11
  // before PlainDriver is initialized, we can perform pre-library loading
  // requirement check here, e.g. Embedded server argument params
  if Info <> nil then Result.SetDriverOptions(Info);
  // end added by tohenk, 2009-10-11
  Result.Initialize;
end;

{**
  Returns the version of the plain driver library that will be used to open a connection
  to the given URL.
  @param url the URL of the database
  @return the version number of the plain driver library for the give URL
}
function TZMySQLDriver.GetClientVersion(const Url: string): Integer;
begin
  Result := ConvertMySQLVersionToSQLVersion(GetPlainDriver(Url).GetClientVersion);
end;

{ TZMySQLConnection }

{**
  Constructs this object and assignes the main properties.
  @param Driver the parent ZDBC driver.
  @param PlainDriver a MySQL plain driver.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZMySQLConnection.Create(Driver: IZDriver; const Url: string;
  PlainDriver: IZMySQLPlainDriver; const HostName: string; Port: Integer;
  const Database, User, Password: string; Info: TStrings);
begin
  inherited Create(Driver, Url, HostName, Port, Database, User, Password, Info,
    TZMySQLDatabaseMetadata.Create(Self, Url, Info));

  { Sets a default properties }
  FPlainDriver := PlainDriver;
  Self.PlainDriver := PlainDriver;
  if Self.Port = 0 then
     Self.Port := MYSQL_PORT;
  AutoCommit := True;
  TransactIsolationLevel := tiNone;

  { Processes connection properties. }
  FClientCodePage := Trim(Info.Values['codepage']);

  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZMySQLConnection.Destroy;
begin
  inherited Destroy;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZMySQLConnection.Open;
var
  LogMessage: string;
  OldLevel: TZTransactIsolationLevel;
  OldAutoCommit: Boolean;
  ConnectTimeout: Integer;
  SQL: PAnsiChar;
  ClientFlag : Cardinal;
  SslCa, SslCaPath, SslKey, SslCert, SslCypher: PAnsiChar;
  myopt: TMySQLOption;
  sMyOpt: string;
  my_client_Opt:TMYSQL_CLIENT_OPTIONS;
  sMy_client_Opt:String;
begin
   if not Closed then
      Exit;

  LogMessage := Format('CONNECT TO "%s" AS USER "%s"', [Database, User]);

  FPlainDriver.Init(FHandle);
  try
    { Sets a default port number. }
    if Port = 0 then
       Port := MYSQL_PORT;

    { Turn on compression protocol. }
    if StrToBoolEx(Info.Values['compress']) then
      FPlainDriver.SetOptions(FHandle, MYSQL_OPT_COMPRESS, nil);
    { Sets connection timeout. }
    ConnectTimeout := StrToIntDef(Info.Values['timeout'], 0);
    if ConnectTimeout >= 0 then
      FPlainDriver.SetOptions(FHandle, MYSQL_OPT_CONNECT_TIMEOUT, PAnsiChar(@ConnectTimeout));

   (*Added lines to handle option parameters 21 november 2007 marco cotroneo*)
    for myopt := low(TMySQLOption) to high(TMySQLOption) do
    begin
      sMyOpt:= GetEnumName(typeInfo(TMySQLOption), integer(myOpt));
      if Info.Values[sMyOpt] <> '' then
      begin
        FPlainDriver.SetOptions(FHandle, myopt, PAnsiChar(Info.Values[sMyOpt]));
      end;
    end;

    { Set ClientFlag }
    ClientFlag := 0;
    if Not StrToBoolEx(Info.Values['dbless']) 
       then ClientFlag := trunc(power(2, GetEnumValue(   TypeInfo(TMYSQL_CLIENT_OPTIONS),'_CLIENT_CONNECT_WITH_DB')));

    for my_client_Opt := low(TMYSQL_CLIENT_OPTIONS) to high(TMYSQL_CLIENT_OPTIONS) do
    begin
      sMy_client_Opt:= GetEnumName(typeInfo(TMYSQL_CLIENT_OPTIONS), integer(my_client_Opt));
      if StrToBoolEx(Info.Values[sMy_client_Opt]) then
          ClientFlag:= ClientFlag or trunc(power(2, GetEnumValue(TypeInfo(TMYSQL_CLIENT_OPTIONS),sMy_client_Opt)));
    end;

    { Set SSL properties before connect}
    SslKey := nil;
    SslCert := nil;
    SslCa := nil;
    SslCaPath := nil;
    SslCypher := nil;
    if StrToBoolEx(Info.Values['MYSQL_SSL']) then
      begin
         if Info.Values['MYSQL_SSL_KEY'] <> '' then
            {$IFDEF DELPHI12_UP}
            SslKey := PAnsiChar(UTF8String(Info.Values['MYSQL_SSL_KEY']));
            {$ELSE}
            SslKey := PAnsiChar(Info.Values['MYSQL_SSL_KEY']);
            {$ENDIF}
         if Info.Values['MYSQL_SSL_CERT'] <> '' then
            {$IFDEF DELPHI12_UP}
            SslCert := PAnsiChar(UTF8String(Info.Values['MYSQL_SSL_CERT']));
            {$ELSE}
            SslCert := PAnsiChar(Info.Values['MYSQL_SSL_CERT']);
            {$ENDIF}
         if Info.Values['MYSQL_SSL_CA'] <> '' then
            {$IFDEF DELPHI12_UP}
            SslCa := PAnsiChar(UTF8String(Info.Values['MYSQL_SSL_CA']));
            {$ELSE}
            SslCa := PAnsiChar(Info.Values['MYSQL_SSL_CA']);
            {$ENDIF}
         if Info.Values['MYSQL_SSL_CAPATH'] <> '' then
            {$IFDEF DELPHI12_UP}
            SslCaPath := PAnsiChar(UTF8String(Info.Values['MYSQL_SSL_CAPATH']));
            {$ELSE}
            SslCaPath := PAnsiChar(Info.Values['MYSQL_SSL_CAPATH']);
            {$ENDIF}
         if Info.Values['MYSQL_SSL_CYPHER'] <> '' then
            {$IFDEF DELPHI12_UP}
            SslCypher := PAnsiChar(UTF8String(Info.Values['MYSQL_SSL_CYPHER']));
            {$ELSE}
            SslCypher := PAnsiChar(Info.Values['MYSQL_SSL_CYPHER']);
            {$ENDIF}
         FPlainDriver.SslSet(FHandle, SslKey, SslCert, SslCa, SslCaPath,
            SslCypher);
         DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol,
            'SSL options set');
      end;

    { Connect to MySQL database. }
    {$IFDEF DELPHI12_UP}
    if FPlainDriver.RealConnect(FHandle, PAnsiChar(UTF8String(HostName)),
                                PAnsiChar(UTF8String(User)), PAnsiChar(UTF8String(Password)),
                                PAnsiChar(UTF8String(Database)), Port, nil,
                                ClientFlag) = nil then
    {$ELSE}
    if FPlainDriver.RealConnect(FHandle, PAnsiChar(HostName), PAnsiChar(User),
                                PAnsiChar(Password), PAnsiChar(Database), Port, nil,
                                ClientFlag) = nil then
    {$ENDIF}
    begin
      CheckMySQLError(FPlainDriver, FHandle, lcConnect, LogMessage);
      DriverManager.LogError(lcConnect, FPlainDriver.GetProtocol, LogMessage,
        0, SUnknownError);
      raise EZSQLException.Create(SCanNotConnectToServer);
    end;
    DriverManager.LogMessage(lcConnect, FPlainDriver.GetProtocol, LogMessage);

    { Sets a client codepage. }
    if FClientCodePage <> '' then
    begin
      {$IFDEF DELPHI12_UP}
      SQL := PAnsiChar(UTF8String(Format('SET NAMES %s', [FClientCodePage])));
      {$ELSE}
      SQL := PAnsiChar(Format('SET NAMES %s', [FClientCodePage]));
      {$ENDIF}
      FPlainDriver.ExecQuery(FHandle, SQL);
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end;

    { Sets transaction isolation level. }
    OldLevel := TransactIsolationLevel;
    TransactIsolationLevel := tiNone;
    SetTransactionIsolation(OldLevel);

    { Sets an auto commit mode. }
    OldAutoCommit := AutoCommit;
    AutoCommit := True;
    SetAutoCommit(OldAutoCommit);
  except
    FPlainDriver.Close(FHandle);
    FPlainDriver.Despose(FHandle);
    FHandle := nil;
    raise;
  end;

  inherited Open;
end;

{**
  Ping Current Connection's server, if client was disconnected,
  the connection is resumed.
  @return 0 if succesfull or error code if any error occurs
}
function TZMySQLConnection.PingServer: Integer;
const
   PING_ERROR_ZEOSCONNCLOSED = -1;
var
   Closing: boolean;
begin
   Closing := FHandle = nil;
   if Closed or Closing then
      Result := PING_ERROR_ZEOSCONNCLOSED
   else
      Result := FPlainDriver.Ping(FHandle);
end;

{**
  Escape a string so it's acceptable for the Connection's server.
  @param value string that should be escaped
  @return Escaped string
}
function TZMySQLConnection.EscapeString(Value: AnsiString): AnsiString;
var
   Closing: boolean;
   Inlength, outlength: integer;
   Outbuffer: AnsiString;
begin
   InLength := Length(Value);
//   OutLength := 0;
   Setlength(Outbuffer,Inlength*2+1);
   Closing := FHandle = nil;
   //RealConnect needs database connection handle
   if Closed or Closing then
     OutLength := FPlainDriver.GetEscapeString(PAnsiChar(OutBuffer),PAnsiChar(Value),InLength)
   else
     OutLength := FPlainDriver.GetRealEscapeString(FHandle, PAnsiChar(OutBuffer),PAnsiChar(Value),InLength);
   Setlength(Outbuffer,OutLength);
   Result := Outbuffer;
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
function TZMySQLConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZMySQLStatement.Create(FPlainDriver, Self, Info, FHandle);
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
function TZMySQLConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  if Assigned(Info) then
    if StrToBoolEx(Info.Values['preferprepared']) then
      Result := TZMySQLPreparedStatement.Create(FPlainDriver, Self, SQL, Info)
    else
      Result := TZMySQLEmulatedPreparedStatement.Create(FPlainDriver, Self, SQL, Info, FHandle)
  else
    Result := TZMySQLEmulatedPreparedStatement.Create(FPlainDriver, Self, SQL, Info, FHandle);
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.Commit;
begin
  if (TransactIsolationLevel <> tiNone) and (AutoCommit <> True)
    and not Closed then
  begin
    If not FPlaindriver.Commit(FHandle) then
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, 'Native Commit call');
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, 'Native Commit call');
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.Rollback;
begin
  if (TransactIsolationLevel <> tiNone) and (AutoCommit <> True)
    and not Closed then
  begin
    If not FPlaindriver.Rollback(FHandle) then
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, 'Native Rollback call');
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, 'Native Rollback call');
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
procedure TZMySQLConnection.Close;
var
  LogMessage: string;
begin
  if not Closed then
  begin
    FPlainDriver.Close(FHandle);
    FPlainDriver.Despose(FHandle);
    FHandle := nil;
    LogMessage := Format('DISCONNECT FROM "%s"', [Database]);
    DriverManager.LogMessage(lcDisconnect, FPlainDriver.GetProtocol, LogMessage);
  end;
  inherited Close;
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZMySQLConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZMySQLConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZMySQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  SQL: PAnsiChar;
  testResult: Integer;
begin
  if TransactIsolationLevel <> Level then
  begin
    inherited SetTransactionIsolation(Level);
    testResult := 1;
    if not Closed then
    begin
      case TransactIsolationLevel of
        tiNone, tiReadUncommitted:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED';
            testResult := FPlainDriver.ExecQuery(FHandle, SQL);
          end;
        tiReadCommitted:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED';
            testResult := FPlainDriver.ExecQuery(FHandle, SQL);
          end;
        tiRepeatableRead:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ';
            testResult := FPlainDriver.ExecQuery(FHandle, SQL);
          end;
        tiSerializable:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE';
            testResult := FPlainDriver.ExecQuery(FHandle, SQL);
          end;
        else
          SQL := '';
      end;
      if (testResult <> 0) then
          CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
      if SQL <> '' then
        DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    end;
  end;
end;

{**
  Sets this connection's auto-commit mode.
  If a connection is in auto-commit mode, then all its SQL
  statements will be executed and committed as individual
  transactions.  Otherwise, its SQL statements are grouped into
  transactions that are terminated by a call to either
  the method <code>commit</code> or the method <code>rollback</code>.
  By default, new connections are in auto-commit mode.

  The commit occurs when the statement completes or the next
  execute occurs, whichever comes first. In the case of
  statements returning a ResultSet, the statement completes when
  the last row of the ResultSet has been retrieved or the
  ResultSet has been closed. In advanced cases, a single
  statement may return multiple results as well as output
  parameter values. In these cases the commit occurs when all results and
  output parameter values have been retrieved.

  @param autoCommit true enables auto-commit; false disables auto-commit.
}
procedure TZMySQLConnection.SetAutoCommit(AutoCommit: Boolean);
begin
  if AutoCommit <> Self.AutoCommit then
  begin
    inherited SetAutoCommit(AutoCommit);

    if not Closed then
    begin
      if not FPlaindriver.SetAutocommit(FHandle, AutoCommit) then
        CheckMySQLError(FPlainDriver, FHandle, lcExecute, 'Native SetAutoCommit '+BoolToStrEx(AutoCommit)+'call');
      DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, 'Native SetAutoCommit '+BoolToStrEx(AutoCommit)+'call');
    end;
  end;
end;

{**
  Gets client's full version number.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZMySQLConnection.GetClientVersion: Integer;
begin
 Result := ConvertMySQLVersionToSQLVersion( FPlainDriver.GetClientVersion );
end;

{**
  Gets server's full version number.
  The format of the returned version must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZMySQLConnection.GetHostVersion: Integer;
begin
 Result := ConvertMySQLVersionToSQLVersion( FPlainDriver.GetServerVersion(FHandle) );
 CheckMySQLError(FPlainDriver, FHandle, lcExecute, 'mysql_get_server_version()');
end;

{**
  Gets a reference to MySQL connection handle.
  @return a reference to MySQL connection handle.
}
function TZMySQLConnection.GetConnectionHandle: PZMySQLConnect;
begin
  Result := FHandle;
end;

{**
  Gets a MySQL plain driver interface.
  @return a MySQL plain driver interface.
}
function TZMySQLConnection.GetPlainDriver: IZMySQLPlainDriver;
begin
  Result := FPlainDriver;
end;

function TZMySQLConnection.GetDescription: AnsiString;
begin
    Result := self.FPlainDriver.GetDescription;
end;

initialization
  MySQLDriver := TZMySQLDriver.Create;
  DriverManager.RegisterDriver(MySQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(MySQLDriver);
  MySQLDriver := nil;
end.

