{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Classes for Testing Framework         }
{                                                         }
{ Originally written by Sergey Merkuriev, Sergey Seroukhov}
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

unit ZSqlTestCase;

interface

{$I ZTestFramework.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  {$IFDEF FPC}fpcunit{$ELSE}TestFramework{$ENDIF}, Classes, SysUtils, {$IFDEF ENABLE_POOLED}ZClasses,{$ENDIF}
  ZCompatibility, ZDbcIntfs, ZConnection, Contnrs, ZTestCase, ZScriptParser, ZDbcLogging;

type
  {** Represents a SQL test database configuration. }
  TZConnectionConfig = class
  private
    FName: string;
    FAlias: string;
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUserName: string;
    FPassword: string;
    FRebuild: Boolean;
    FDelimiter: string;
    FDelimiterType: TZDelimiterType;
    FCreateScripts: TStringDynArray;
    FDropScripts: TStringDynArray;
    FProperties: TStringDynArray;
  public
    property Name: string read FName write FName;
    property Alias: string read FAlias write FAlias;
    property Protocol: string read FProtocol write FProtocol;
    property HostName: string read FHostName write FHostName;
    property Port: Integer read FPort write FPort;
    property Database: string read FDatabase write FDatabase;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Rebuild: Boolean read FRebuild write FRebuild;
    property Delimiter: string read FDelimiter write FDelimiter;
    property DelimiterType: TZDelimiterType read FDelimiterType
      write FDelimiterType;
    property CreateScripts: TStringDynArray read FCreateScripts
      write FCreateScripts;
    property DropScripts: TStringDynArray read FDropScripts
      write FDropScripts;
    property Properties: TStringDynArray read FProperties
      write FProperties;
  end;

  {** Implements an abstract class for all SQL test cases. }
  TZAbstractSQLTestCase = class(TZAbstractTestCase, IZLoggingListener)
  private
    FConnections: TObjectList;
    FTraceList: TStrings;

    FConnectionName: string;
    FAlias: string;
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUserName: string;
    FPassword: string;
    FRebuild: Boolean;
    FCreateScripts: TStringDynArray;
    FDropScripts: TStringDynArray;
    FProperties: TStringDynArray;
    function GetProtocol : string;
  protected
    property Connections: TObjectList read FConnections write FConnections;
    property TraceList: TStrings read FTraceList write FTraceList;

    procedure LoadConfiguration; override;
    procedure SetActiveConnection(Connection: TZConnectionConfig);
    {$IFNDEF FPC}
    procedure RunWithFixture(TestResult: TTestResult); override;
    {$ELSE}
    procedure Run(TestResult: TTestResult); override;
    {$ENDIF}

    function IsProtocolValid(Name: string): Boolean; virtual;
    function GetSupportedProtocols: string; virtual; abstract;

    procedure StartSQLTrace;
    procedure StopSQLTrace;

  public
    destructor Destroy; override;

    procedure Fail(Msg: string; ErrorAddr: Pointer = nil);{$IFNDEF FPC}  override; {$ENDIF}
    procedure LogEvent(Event: TZLoggingEvent);

    { Difference convinience methods. }
    function CreateDbcConnection: IZConnection;
    function CreateDatasetConnection: TZConnection;
    procedure PrintResultSet(ResultSet: IZResultSet;
      ShowTypes: Boolean; Note: string = '');

    { Properties to access active connection settings. }
    property ConnectionName: string read FConnectionName;
    property Alias: string read FAlias;
    property Protocol: string read GetProtocol;
    property HostName: string read FHostName;
    property Port: Integer read FPort;
    property Database: string read FDatabase;
    property UserName: string read FUserName;
    property Password: string read FPassword;
    property Rebuild: Boolean read FRebuild;
    property CreateScripts: TStringDynArray read FCreateScripts;
    property DropScripts: TStringDynArray read FDropScripts;
    property Properties: TStringDynArray read FProperties;
  end;

  {** Implements a test case which runs all active connections. }
  TZPortableSQLTestCase = class (TZAbstractSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  end;

  {**
    Implements a test case which runs only for active connection
    supported by the test.
  }
  TZSpecificSQLTestCase = class (TZAbstractSQLTestCase);

{**
  Rebuilds test databases for the active connections
  in the specified test group.
  @param TestGroup a test group name. If the group is not set,
    the test group is taken from TestGroup global variable.
}
procedure RebuildTestDatabases(TestGroup: string = '');

implementation

uses ZSysUtils, ZTestConsts, ZTestConfig, ZSqlProcessor;

{ TZAbstractSQLTestCase }

{**
  Destroys this test case and cleanups the memory.
}
destructor TZAbstractSQLTestCase.Destroy;
begin
  if Assigned(FConnections) then
    FConnections.Free;
  if Assigned(FTraceList) then
    FTraceList.Free;

  inherited Destroy;
end;

function TZAbstractSQLTestCase.GetProtocol: string;
begin
  {$IFDEF ENABLE_POOLED}
  If StartsWith(FProtocol,pooledprefix) then
    Result := Copy(FProtocol,Length(PooledPrefix)+1,Length(FProtocol))
  else
  {$ENDIF}
    Result := FProtocol;
end;

{**
  Loads a configuration from the configuration file.
}
procedure TZAbstractSQLTestCase.LoadConfiguration;

  function DefineDelimiterType(Value: string): TZDelimiterType;
  begin
    if LowerCase(Value) = LowerCase(DEFAULT_DELIMITER) then
      Result := dtDefault
    else if LowerCase(Value) = LowerCase(GO_DELIMITER) then
      Result := dtGo
    else if LowerCase(Value) = LowerCase(SET_TERM_DELIMITER) then
      Result := dtSetTerm
    else if LowerCase(Value) = LowerCase(EMPTY_LINE_DELIMITER) then
      Result := dtEmptyLine
    else Result := dtDefault;
  end;

var
  I: Integer;
  _ConnectionName, Temp: string;
  ActiveConnections: TStringDynArray;
  Current: TZConnectionConfig;
begin
  inherited LoadConfiguration;

  { Resets a connection configuration list. }
  if not Assigned(FConnections) then
    FConnections := TObjectList.Create
  else FConnections.Clear;

  { Reads a list with active database connections. }
  Temp := ReadInheritProperty(ACTIVE_CONNECTIONS_KEY, NONE_VALUE);
  if UpperCase(Temp) = UpperCase(NONE_VALUE) then
    Temp := '';
  ActiveConnections := SplitStringToArray(Temp, LIST_DELIMITERS);

  for I := 0 to High(ActiveConnections) do
  begin
    Current := TZConnectionConfig.Create;
    _ConnectionName := ActiveConnections[I];

    Current.Name := _ConnectionName;
    Current.Alias := ReadProperty(_ConnectionName, DATABASE_ALIAS_KEY, '');
    Current.Protocol := ReadProperty(_ConnectionName, DATABASE_PROTOCOL_KEY, '');
    Current.HostName := ReadProperty(_ConnectionName, DATABASE_HOST_KEY,
      DEFAULT_HOST_VALUE);
    Current.Port := StrToIntDef(ReadProperty(_ConnectionName,
      DATABASE_PORT_KEY, ''), DEFAULT_PORT_VALUE);
    Current.Database := ReadProperty(_ConnectionName, DATABASE_NAME_KEY, '');
    Current.UserName := ReadProperty(_ConnectionName, DATABASE_USER_KEY, '');
    Current.Password := ReadProperty(_ConnectionName, DATABASE_PASSWORD_KEY, '');
    Current.Rebuild := StrToBoolEx(ReadProperty(_ConnectionName,
      DATABASE_REBUILD_KEY, FALSE_VALUE));
    Current.DelimiterType := DefineDelimiterType(
      ReadProperty(_ConnectionName, DATABASE_DELIMITER_TYPE_KEY, ''));
    Current.Delimiter := ReadProperty(_ConnectionName,
      DATABASE_DELIMITER_KEY, '');
    Current.CreateScripts := SplitStringToArray(ReadProperty(_ConnectionName,
      DATABASE_CREATE_SCRIPTS_KEY, ''), LIST_DELIMITERS);
    Current.DropScripts := SplitStringToArray(ReadProperty(_ConnectionName,
      DATABASE_DROP_SCRIPTS_KEY, ''), LIST_DELIMITERS);
    Current.Properties := SplitStringToArray(ReadProperty(_ConnectionName,
      DATABASE_PROPERTIES_KEY, ''), LIST_DELIMITERS);
    FConnections.Add(Current);
  end;
end;

{**
  Overrides a fail method which prints an error message.
  @param Msg an error message string.
  @param ErrorAddr an address where error happened.
}
procedure TZAbstractSQLTestCase.Fail(Msg: string; ErrorAddr: Pointer = nil);
begin
  inherited Fail(Format('%s/%s: %s', [ConnectionName, Protocol, Msg]),
    ErrorAddr);
end;

{**
  Handles a new incoming logging event.
  @param Event an incoming logging event.
}
procedure TZAbstractSQLTestCase.LogEvent(Event: TZLoggingEvent);
begin
  if Event.Message <> '' then
    FTraceList.Append(Event.Message);
end;

{**
  Function check name prototocol
  @param Name a protocol name
  @result true if protocol valid
}
function TZAbstractSQLTestCase.IsProtocolValid(Name: string): Boolean;
var
  Temp: TStrings;
  TempName : string;
begin
  if GetSupportedProtocols <> '' then
  begin
    Temp := SplitString(GetSupportedProtocols, LIST_DELIMITERS);
    TempName := Name;
    try
      {$IFDEF ENABLE_POOLED}
      If StartsWith(TempName,pooledprefix) then
        TempName := Copy(TempName,Length(PooledPrefix)+1,Length(TempName));
      {$ENDIF}
      Result := (Temp.IndexOf(TempName) >= 0);
    finally
      Temp.Free;
    end;
  end else
    Result := True;
end;

{**
  Sets an active database connection for the test.
  @param Connection a database connection to be set.
}
procedure TZAbstractSQLTestCase.SetActiveConnection(
  Connection: TZConnectionConfig);
begin
  FConnectionName := Connection.Name;
  FAlias := Connection.Alias;
  FProtocol := Connection.Protocol;
  FHostName := Connection.HostName;
  FPort := Connection.Port;
  FDatabase := Connection.Database;
  FUserName := Connection.UserName;
  FPassword := Connection.Password;
  FRebuild := Connection.Rebuild;
  FCreateScripts := Connection.CreateScripts;
  FDropScripts := Connection.DropScripts;
  FProperties := Connection.Properties;
end;

{**
  Starts logging outgoing SQL statements.
}
procedure TZAbstractSQLTestCase.StartSQLTrace;
begin
  if FTraceList = nil then
    FTraceList := TStringList.Create
  else FTraceList.Clear;
  DriverManager.AddLoggingListener(Self);
end;

{**
  Clean ups test after finish.
}
procedure TZAbstractSQLTestCase.StopSQLTrace;
begin
  DriverManager.RemoveLoggingListener(Self);
end;

{$IFNDEF FPC}
{**
   Function configure test paramters and start test case
   <b>Note:</b> Configuration file ZSqlConfig.ini should exist and contain
    the appropriate section with settings of the protocol
}
procedure TZAbstractSQLTestCase.RunWithFixture(TestResult: TTestResult);
var
  I: Integer;
  Current: TZConnectionConfig;
begin
  if not Assigned(FConnections) or (FConnections.Count = 0) then
    LoadConfiguration;

  for I := 0 to FConnections.Count - 1 do
  begin
    Current := TZConnectionConfig(FConnections[I]);
    if not IsProtocolValid(Current.Protocol) then
      Continue;

    SetActiveConnection(Current);

    inherited RunWithFixture(TestResult);
  end;
end;
{$ELSE}
{**
   Function configure test paramters and start test case
   <b>Note:</b> Configuration file ZSqlConfig.ini should exist and contain
    the appropriate section with settings of the protocol
}
procedure TZAbstractSQLTestCase.Run(TestResult: TTestResult);
var
  I: Integer;
  Current: TZConnectionConfig;
begin
  if not Assigned(FConnections) or (FConnections.Count = 0) then
    LoadConfiguration;

  for I := 0 to FConnections.Count - 1 do
  begin
    Current := TZConnectionConfig(FConnections[I]);
    if not IsProtocolValid(Current.Protocol) then
      Continue;

    SetActiveConnection(Current);

    inherited Run(TestResult);
  end;
end;
{$ENDIF}

{**
  Creates a database ZDBC connection object.
  @return a created database ZDBC connection object.
}
function TZAbstractSQLTestCase.CreateDbcConnection: IZConnection;
var
  URL: string;
  TempProperties :TStrings;
  I: Integer;
begin
  if Port <> 0 then
    URL := Format('zdbc:%s://%s:%d/%s?UID=%s;PWD=%s', [Protocol, HostName, Port, Database, UserName, Password])
  else URL := Format('zdbc:%s://%s/%s?UID=%s;PWD=%s', [Protocol, HostName, Database, UserName, Password]);
  TempProperties := TStringList.Create;
  for I := 0 to High(Properties) do
  begin
    TempProperties.Add(Properties[I])
  end;
  Result := DriverManager.GetConnectionWithParams(URL, TempProperties);
  TempProperties.Free;
end;

{**
  Creates a database connection component compatible with TZDataset.
  @return a created database connection component.
}
function TZAbstractSQLTestCase.CreateDatasetConnection: TZConnection;
var
  I: Integer;
begin
  Result := TZConnection.Create(nil);
  Result.Protocol := Protocol;
  Result.Port := Port;
  Result.HostName := HostName;
  Result.Database := Database;
  Result.User := UserName;
  Result.Password := Password;
  Result.LoginPrompt := False;
  for I := 0 to High(Properties) do
  begin
    Result.Properties.Add(Properties[I])
  end;
end;

{**
  Prints a content of the result set from the first to the last row.
  @param ResultSet a result set object.
  @param ShowTypes a show types flag.
}
procedure TZAbstractSQLTestCase.PrintResultSet(ResultSet: IZResultSet;
  ShowTypes: Boolean; Note: string = '');
var
  I: Integer;
  Metadata: IZResultSetMetadata;
  Stream: TStream;
  Buffer: array[0..100] of Char;
  ReadNum: Integer;
begin
  if not SuppressTestOutput then
  begin
    if Note <> '' then
    begin
      System.Writeln;
      System.Writeln('====================================');
      System.Writeln(Note);
      System.Writeln('====================================');
    end;

    Metadata := ResultSet.GetMetaData;
    for I := 1 to Metadata.GetColumnCount do
    begin
      System.Write(Metadata.GetColumnLabel(I));
      if ShowTypes then
        System.Write(':', Metadata.GetColumnTypeName(I));
      System.Write(' ');
    end;
    System.Writeln;
    System.Writeln('====================================');

    while ResultSet.Next do
    begin
      for I := 1 to Metadata.GetColumnCount do
      begin
        if ResultSet.IsNull(I) then
        begin
          System.Write('NULL ');
          Continue;
        end;

        case Metadata.GetColumnType(I) of
          stAsciiStream:
            begin
              Stream := ResultSet.GetAsciiStream(I);
              if Stream <> nil then
              begin
                try
                  ReadNum := Stream.Read(Buffer, 100);
                  System.Write('''' + BufferToStr(Buffer, ReadNum) + '''');
                finally
                  Stream.Free;
                end;
              end else
                System.Write('!ERROR!');
            end;
          stBinaryStream:
            begin
              Stream := ResultSet.GetBinaryStream(I);
              if Stream <> nil then
              begin
                try
                  ReadNum := Stream.Read(Buffer, 10);
                  System.Write('''' + BufferToStr(Buffer, ReadNum) + '...''');
                finally
                  Stream.Free;
                end;
              end else
                System.Write('!ERROR!');
            end;
          else
            System.Write('''' + ResultSet.GetString(I) + '''');
        end;
        System.Write(' ');
      end;
      System.Writeln;
    end;
    ResultSet.BeforeFirst;
    System.Writeln('====================================');
    System.Writeln;
  end;
end;

{ TZPortableSQLTestCase }

{**
  Gets a comma separated list of all supported by this test protocols.
  @returns a list of all supported protocols.
}
function TZPortableSQLTestCase.GetSupportedProtocols: string;
begin
  Result := '';
end;

type
  {** Implements a supplementary generic SQL test case. }
  TZSupplementarySQLTestCase = class (TZPortableSQLTestCase)
  private
    FTestGroup: string;
    FSQLProcessor: TZSQLProcessor;
  protected
    procedure SuppressError(Processor: TZSQLProcessor; StatementIndex: Integer;
      E: Exception; var ErrorHandleAction: TZErrorHandleAction);
    procedure RaiseError(Processor: TZSQLProcessor; StatementIndex: Integer;
      E: Exception; var ErrorHandleAction: TZErrorHandleAction);

    procedure ExecuteScripts(FileNames: TStringDynArray;
      RaiseException: Boolean);
  public
    constructor CreateWithGroup(TestGroup: string);
    destructor Destroy; override;

    procedure RebuildDatabases;
  end;


{ TZSupplementarySQLTestCase }

{**
  Constructs this test cases and assignes a group name.
  @param TestGroup a name of the test group.
}
constructor TZSupplementarySQLTestCase.CreateWithGroup(TestGroup: string);
begin
  FTestGroup := TestGroup;
  FSQLProcessor := TZSQLProcessor.Create(nil);
end;

{**
  Destroyes this object and clean ups the memory.
}
destructor TZSupplementarySQLTestCase.Destroy;
begin
  FSQLProcessor.Free;
  inherited Destroy;
end;

{**
  Handles exceptions in SQL script processing and raise them.
}
procedure TZSupplementarySQLTestCase.RaiseError(Processor: TZSQLProcessor;
  StatementIndex: Integer; E: Exception;
  var ErrorHandleAction: TZErrorHandleAction);
begin
  ErrorHandleAction := eaFail;
end;

{**
  Handles exceptions in SQL script processing and suppress them.
}
procedure TZSupplementarySQLTestCase.SuppressError(
  Processor: TZSQLProcessor; StatementIndex: Integer; E: Exception;
  var ErrorHandleAction: TZErrorHandleAction);
begin
  System.WriteLn('Database Rebuild Error: ' + E.Message);
  ErrorHandleAction := eaSkip;
end;

{**
  Executes a list of SQL scripts.
  @param FileNames an array with file names.
  @param RaiseException <code>False</code> to suppress exceptions.
}
procedure TZSupplementarySQLTestCase.ExecuteScripts(
  FileNames: TStringDynArray; RaiseException: Boolean);
var
  I: Integer;
  ScriptPath: string;
begin
  { Sets the right error event handler. }
  if RaiseException then
    FSQLProcessor.OnError := RaiseError
  else FSQLProcessor.OnError := SuppressError;

  ScriptPath := TestConfig.ConfigFilePath;

  for I := 0 to High(FileNames) do
  begin
    FSQLProcessor.Script.Clear;
    try
      FSQLProcessor.Script.LoadFromFile(ScriptPath + FileNames[I]);
      // To avoid parameter handling while rebuild! Parameters must not be handled!
      FSQLProcessor.ParamCheck := false;
    except
      System.WriteLn(Format('File %s can not be opened and executed.',
        [FileNames[I]]));
      if RaiseException then
        raise;
    end;

    if FSQLProcessor.Script.Count > 0 then
    begin
      try
        FSQLProcessor.Execute;
      except
        on E: Exception do
        begin
          System.WriteLn('Database Rebuild Error: ' + E.Message);
          if RaiseException then
            raise;
        end;
      end;
    end;
  end;
end;

{**
  Rebuilds databases, configured for this test group.
}
procedure TZSupplementarySQLTestCase.RebuildDatabases;
var
  I: Integer;
  Current: TZConnectionConfig;
  Connection: TZConnection;
begin
  if not Assigned(FConnections) or (FConnections.Count = 0) then
    LoadConfiguration;

  for I := 0 to FConnections.Count - 1 do
  begin
    Current := TZConnectionConfig(FConnections[I]);
    if not IsProtocolValid(Current.Protocol) then
      Continue;

    SetActiveConnection(Current);
    Connection := CreateDatasetConnection;
    try
      FSQLProcessor.Connection := Connection;
      FSQLProcessor.Delimiter := Current.Delimiter;
      FSQLProcessor.DelimiterType := Current.DelimiterType;

      if Current.Rebuild then
      begin
        ExecuteScripts(Current.DropScripts, False);
        ExecuteScripts(Current.CreateScripts, True);
      end;
    finally
      Connection.Free;
    end;
  end;
end;

{**
  Rebuilds databases for the specified test group.
  @param TestGroup a name of the test group. If the name is not set
    test group is takes from TestGroup global variable.
}
procedure RebuildTestDatabases(TestGroup: string = '');
var
  Temp: TZSupplementarySQLTestCase;
begin
  if TestGroup = '' then
    TestGroup := ZTestConfig.TestGroup;

  Temp := TZSupplementarySQLTestCase.CreateWithGroup(TestGroup);
  try
    Temp.RebuildDatabases;
  finally
    Temp.Free;
  end;
end;

end.
