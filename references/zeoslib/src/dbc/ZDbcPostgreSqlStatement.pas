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

unit ZDbcPostgreSqlStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcStatement, ZDbcLogging,
  ZPlainPostgreSqlDriver, ZCompatibility, ZVariant, ZDbcGenericResolver,
  ZDbcCachedResultSet, ZDbcPostgreSql;

type

  {** Defines a PostgreSQL specific statement. }
  IZPostgreSQLStatement = interface(IZStatement)
    ['{E4FAFD96-97CC-4247-8ECC-6E0A168FAFE6}']

    function IsOidAsBlob: Boolean;
  end;

  {** Implements Generic PostgreSQL Statement. }
  TZPostgreSQLStatement = class(TZAbstractStatement, IZPostgreSQLStatement)
  private
    FPlainDriver: IZPostgreSQLPlainDriver;
    FOidAsBlob: Boolean;
  protected
    function CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
    function GetConnectionHandle():PZPostgreSQLConnect;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZConnection; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: string): IZResultSet; override;
    function ExecuteUpdate(const SQL: string): Integer; override;
    function Execute(const SQL: string): Boolean; override;

    function IsOidAsBlob: Boolean;
  end;

  {** Implements Prepared SQL Statement. }
  TZPostgreSQLPreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FPlainDriver: IZPostgreSQLPlainDriver;
    FCharactersetCode : TZPgCharactersetType;
  protected
    function CreateExecStatement: IZStatement; override;
    function PrepareSQLParam(ParamIndex: Integer): string; override;
    function GetConnectionHandle():PZPostgreSQLConnect;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings);
  end;

  {** Implements callable Postgresql Statement. }
  TZPostgreSQLCallableStatement = class(TZAbstractCallableStatement)
  private
    FOidAsBlob: Boolean;
    function GetProcedureSql(): string;
    function FillParams(const ASql:String):String;
    function PrepareSQLParam(ParamIndex: Integer): string;
  protected
    function GetConnectionHandle():PZPostgreSQLConnect;
    function GetPlainDriver():IZPostgreSQLPlainDriver;
    {procedure CheckInterbase6Error(const Sql: string = '');
    procedure FetchOutParams(Value: IZResultSQLDA);
    function GetProcedureSql(SelectProc: boolean): string;
    procedure TrimInParameters;}
    function CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
  public
    constructor Create(Connection: IZConnection; const SQL: string; Info: TStrings);

    function ExecuteQuery(const SQL: string): IZResultSet; override;
    function ExecuteUpdate(const SQL: string): Integer; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    {function ExecuteUpdate(const SQL: string): Integer; override;
    function Execute(const SQL: string): Boolean; override;



    function ExecutePrepared: Boolean; override; }
  end;

  {** Implements a specialized cached resolver for PostgreSQL. }
  TZPostgreSQLCachedResolver = class(TZGenericCachedResolver, IZCachedResolver)
  protected
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;
  end;

implementation

uses
  Types, ZMessages, ZDbcPostgreSqlResultSet, ZDbcPostgreSqlUtils, ZTokenizer;

{ TZPostgreSQLStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLStatement.Create(PlainDriver: IZPostgreSQLPlainDriver;
  Connection: IZConnection; Info: TStrings);
begin
  inherited Create(Connection, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;

  { Processes connection properties. }
  if Self.Info.Values['oidasblob'] <> '' then
    FOidAsBlob := StrToBoolEx(Self.Info.Values['oidasblob'])
  else
    FOidAsBlob := (Connection as IZPostgreSQLConnection).IsOidAsBlob;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Checks is oid should be treated as Large Object.
  @return <code>True</code> if oid should represent a Large Object.
}
function TZPostgreSQLStatement.IsOidAsBlob: Boolean;
begin
  Result := FOidAsBlob;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZPostgreSQLStatement.CreateResultSet(const SQL: string;
  QueryHandle: PZPostgreSQLResult): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  ConnectionHandle := GetConnectionHandle();
  NativeResultSet := TZPostgreSQLResultSet.Create(FPlainDriver, Self, SQL,
    ConnectionHandle, QueryHandle);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLStatement.ExecuteQuery(const SQL: string): IZResultSet;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := nil;
  ConnectionHandle := GetConnectionHandle();
  {$IFDEF DELPHI12_UP}
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle,
    PAnsiChar(UTF8String(SQL)));
  {$ELSE}
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(SQL));
  {$ENDIF}
  CheckPostgreSQLError(Connection, FPlainDriver, ConnectionHandle, lcExecute,
    SQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
  if QueryHandle <> nil then
    Result := CreateResultSet(SQL, QueryHandle)
  else
    Result := nil;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZPostgreSQLStatement.ExecuteUpdate(const SQL: string): Integer;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := -1;
  ConnectionHandle := GetConnectionHandle();
  {$IFDEF DELPHI12_UP}
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle,
    PAnsiChar(UTF8String(SQL)));
  {$ELSE}
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(SQL));
  {$ENDIF}
  CheckPostgreSQLError(Connection, FPlainDriver, ConnectionHandle, lcExecute,
    SQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  if QueryHandle <> nil then
  begin
    Result := StrToIntDef(StrPas(FPlainDriver.GetCommandTuples(QueryHandle)), 0);
    FPlainDriver.Clear(QueryHandle);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
}
function TZPostgreSQLStatement.Execute(const SQL: string): Boolean;
var
  QueryHandle: PZPostgreSQLResult;
  ResultStatus: TZPostgreSQLExecStatusType;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  ConnectionHandle := GetConnectionHandle();
  {$IFDEF DELPHI12_UP}
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle,
    PAnsiChar(UTF8String(SQL)));
  {$ELSE}
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(SQL));
  {$ENDIF}
  CheckPostgreSQLError(Connection, FPlainDriver, ConnectionHandle, lcExecute,
    SQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  { Process queries with result sets }
  ResultStatus := FPlainDriver.GetResultStatus(QueryHandle);
  case ResultStatus of
    PGRES_TUPLES_OK:
      begin
        Result := True;
        LastResultSet := CreateResultSet(SQL, QueryHandle);
      end;
    PGRES_COMMAND_OK:
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(StrPas(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(StrPas(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
  end;

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Provides connection handle from the associated IConnection
}
function TZPostgreSQLStatement.GetConnectionHandle():PZPostgreSQLConnect;
begin
  if Self.Connection = nil then
    Result := nil
  else
    Result := (self.Connection as IZPostgreSQLConnection).GetConnectionHandle;
end;

{ TZPostgreSQLPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLPreparedStatement.Create(
  PlainDriver: IZPostgreSQLPlainDriver; Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
  FCharactersetCode := (Connection as IZPostgreSQLConnection).GetCharactersetCode;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZPostgreSQLPreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZPostgreSQLStatement.Create(FPlainDriver, Connection, Info);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZPostgreSQLPreparedStatement.PrepareSQLParam(
  ParamIndex: Integer): string;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
  TempStream: TStream;
  WriteTempBlob: IZPostgreSQLBlob;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if DefVarManager.IsNull(Value)  then
    Result := 'NULL'
  else
  begin
    case InParamTypes[ParamIndex] of
      stBoolean:
        if SoftVarManager.GetAsBoolean(Value) then
          Result := 'TRUE'
        else
          Result := 'FALSE';
      stByte, stShort, stInteger, stLong, stBigDecimal, stFloat, stDouble:
        Result := SoftVarManager.GetAsString(Value);
      stString, stBytes:
        Result := EncodeString(FCharactersetCode,SoftVarManager.GetAsString(Value));
      stUnicodeString:
        Result := UTF8Encode(EncodeString(FCharactersetCode,SoftVarManager.GetAsUnicodeString(Value)));
      stDate:
        Result := Format('''%s''::date',
          [FormatDateTime('yyyy-mm-dd', SoftVarManager.GetAsDateTime(Value))]);
      stTime:
        Result := Format('''%s''::time',
          [FormatDateTime('hh":"mm":"ss', SoftVarManager.GetAsDateTime(Value))]);
      stTimestamp:
        Result := Format('''%s''::timestamp',
          [FormatDateTime('yyyy-mm-dd hh":"mm":"ss',
            SoftVarManager.GetAsDateTime(Value))]);
      stAsciiStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            Result := EncodeString(TempBlob.GetString)
          end
          else
          begin
            Result := 'NULL';
          end;
        end;
      stUnicodeStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            Result := EncodeString(FCharactersetCode, UTF8Encode(TempBlob.GetUnicodeString))
          end
          else
          begin
            Result := 'NULL';
          end;
        end;
      stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            if (GetConnection as IZPostgreSQLConnection).IsOidAsBlob then
            begin
              TempStream := TempBlob.GetStream;
              try
                WriteTempBlob := TZPostgreSQLBlob.Create(FPlainDriver, nil, 0,
                  Self.GetConnectionHandle, 0);
                WriteTempBlob.SetStream(TempStream);
                WriteTempBlob.WriteBlob;
                Result := IntToStr(WriteTempBlob.GetBlobOid);
              finally
                WriteTempBlob := nil;
                TempStream.Free;
              end;
            end
            else
            begin
              result:= FPlainDriver.EncodeBYTEA(TempBlob.GetString,
                Self.GetConnectionHandle); // FirmOS
              {
               Result := EncodeString(TempBlob.GetString);
               Result := Copy(Result, 2, Length(Result) - 2);
               Result := EncodeString(Result);
              }
            end;
          end
          else
            Result := 'NULL';
        end;
    end;
  end;
end;

{**
  Provides connection handle from the associated IConnection
}
function TZPostgreSQLPreparedStatement.GetConnectionHandle():PZPostgreSQLConnect;
begin
  if Self.Connection = nil then
    Result := nil
  else
    Result := (self.Connection as IZPostgreSQLConnection).GetConnectionHandle;
end;

{ TZPostgreSQLCallableStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLCallableStatement.Create(
  Connection: IZConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  { Processes connection properties. }
  if Self.Info.Values['oidasblob'] <> '' then
    FOidAsBlob := StrToBoolEx(Self.Info.Values['oidasblob'])
  else
    FOidAsBlob := (Connection as IZPostgreSQLConnection).IsOidAsBlob;
end;

{**
  Provides connection handle from the associated IConnection
  @return a PostgreSQL connection handle.
}
function TZPostgreSQLCallableStatement.GetConnectionHandle():PZPostgreSQLConnect;
begin
  if Self.Connection = nil then
    Result := nil
  else
    Result := (self.Connection as IZPostgreSQLConnection).GetConnectionHandle;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZPostgreSQLCallableStatement.CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  ConnectionHandle := GetConnectionHandle();
  NativeResultSet := TZPostgreSQLResultSet.Create(GetPlainDriver, Self, SQL,
    ConnectionHandle, QueryHandle);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
   Returns plain draiver from connection object
   @return a PlainDriver object
}
function TZPostgreSQLCallableStatement.GetPlainDriver():IZPostgreSQLPlainDriver;
begin
  if self.Connection <> nil then
    Result := (self.Connection as IZPostgreSQLConnection).GetPlainDriver
  else
    Result := nil;
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZPostgreSQLCallableStatement.PrepareSQLParam(
  ParamIndex: Integer): string;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
  TempStream: TStream;
  WriteTempBlob: IZPostgreSQLBlob;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if DefVarManager.IsNull(Value)  then
    Result := 'NULL'
  else
  begin
    case InParamTypes[ParamIndex] of
      stBoolean:
        if SoftVarManager.GetAsBoolean(Value) then
          Result := 'TRUE'
        else
          Result := 'FALSE';
      stByte, stShort, stInteger, stLong, stBigDecimal, stFloat, stDouble:
        Result := SoftVarManager.GetAsString(Value);
      stString, stBytes:
        Result := EncodeString(SoftVarManager.GetAsString(Value));
      stUnicodeString:
        Result := UTF8Encode(EncodeString(SoftVarManager.GetAsUnicodeString(Value)));
      stDate:
        Result := Format('''%s''::date',
          [FormatDateTime('yyyy-mm-dd', SoftVarManager.GetAsDateTime(Value))]);
      stTime:
        Result := Format('''%s''::time',
          [FormatDateTime('hh":"mm":"ss', SoftVarManager.GetAsDateTime(Value))]);
      stTimestamp:
        Result := Format('''%s''::timestamp',
          [FormatDateTime('yyyy-mm-dd hh":"mm":"ss',
            SoftVarManager.GetAsDateTime(Value))]);
      stAsciiStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            Result := EncodeString(TempBlob.GetString)
          end
          else
          begin
            Result := 'NULL';
          end;
        end;
      stUnicodeStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            Result := EncodeString( UTF8Encode(TempBlob.GetUnicodeString))
          end
          else
          begin
            Result := 'NULL';
          end;
        end;
      stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            if (GetConnection as IZPostgreSQLConnection).IsOidAsBlob then
            begin
              TempStream := TempBlob.GetStream;
              try
                WriteTempBlob := TZPostgreSQLBlob.Create(GetPlainDriver, nil, 0,
                  Self.GetConnectionHandle, 0);
                WriteTempBlob.SetStream(TempStream);
                WriteTempBlob.WriteBlob;
                Result := IntToStr(WriteTempBlob.GetBlobOid);
              finally
                WriteTempBlob := nil;
                TempStream.Free;
              end;
            end
            else
            begin
              result:= GetPlainDriver.EncodeBYTEA(TempBlob.GetString,
                Self.GetConnectionHandle); // FirmOS
              {
               Result := EncodeString(TempBlob.GetString);
               Result := Copy(Result, 2, Length(Result) - 2);
               Result := EncodeString(Result);
              }
            end;
          end
          else
            Result := 'NULL';
        end;
    end;
  end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLCallableStatement.ExecuteQuery(
  const SQL: string): IZResultSet;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := nil;
  ConnectionHandle := GetConnectionHandle();
  {$IFDEF DELPHI12_UP}
  QueryHandle := GetPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(UTF8String(SQL)));
  {$ELSE}
  QueryHandle := GetPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(SQL));
  {$ENDIF}
  CheckPostgreSQLError(Connection, GetPlainDriver, ConnectionHandle, lcExecute,
    SQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
  if QueryHandle <> nil then
    Result := CreateResultSet(SQL, QueryHandle)
  else
    Result := nil;
end;

{**
  Prepares and executes an SQL statement that returns a single <code>ResultSet</code> object.
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLCallableStatement.ExecuteQueryPrepared: IZResultSet;
Var SQL: String;
begin
  SQL := GetProcedureSql();
  SQL := FillParams(SQL);
  Result := self.ExecuteQuery(SQL);

end;

{**
   Create sql string for calling stored procedure.
   @return a Stored Procedure SQL string
}
function TZPostgreSQLCallableStatement.GetProcedureSql(): string;

  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    for I := 0 to Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '?';
    end;
  end;

var
  InParams: string;
begin

  InParams := GenerateParamsStr(High(InParamValues));
  InParams := '(' + InParams + ')';

  Result := 'SELECT * FROM ' + SQL + InParams
end;

{**
   Fills the parameter (?) tokens with corresponding parameter value
   @return a prepared SQL query for execution
}
function TZPostgreSQLCallableStatement.FillParams(const ASql:String):String;
var I: Integer;
  Tokens: TStrings;
  ParamIndex: Integer;
begin
  if Pos('?', ASql) > 0 then
  begin
    Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(ASql, [toUnifyWhitespaces]);
    try
      ParamIndex := 0;
      for I := 0 to Tokens.Count - 1 do
      begin
        if Tokens[I] = '?' then
        begin
          Inc(ParamIndex);
          Tokens[I] := PrepareSQLParam(ParamIndex);
        end
      end;
      Result := StringReplace(Tokens.Text, #13#10, ' ', [rfReplaceAll]);
    finally
      Tokens.Free;
    end;
  end
  else
    Result := ASql;

end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZPostgreSQLCallableStatement.ExecuteUpdate(const SQL: string): Integer;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := -1;
  ConnectionHandle := GetConnectionHandle();
  {$IFDEF DELPHI12_UP}
  QueryHandle := GetPlainDriver.ExecuteQuery(ConnectionHandle,
    PAnsiChar(UTF8String(SQL)));
  {$ELSE}
  QueryHandle := GetPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(SQL));
  {$ENDIF}
  CheckPostgreSQLError(Connection, GetPlainDriver, ConnectionHandle, lcExecute,
    SQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);

  if QueryHandle <> nil then
  begin
    Result := StrToIntDef(StrPas(GetPlainDriver.GetCommandTuples(QueryHandle)), 0);
    GetPlainDriver.Clear(QueryHandle);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;
end;


function TZPostgreSQLCallableStatement.ExecuteUpdatePrepared: Integer;
Var SQL: String;
begin
  SQL := GetProcedureSql();
  SQL := FillParams(SQL);
  Result := self.ExecuteUpdate(SQL);

end;

{ TZPostgreSQLCachedResolver }

{**
  Checks is the specified column can be used in where clause.
  @param ColumnIndex an index of the column.
  @returns <code>true</code> if column can be included into where clause.
}
function TZPostgreSQLCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex)
    in [stUnknown, stBinaryStream, stUnicodeStream]);
end;



end.
