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

unit ZDbcMySqlStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZClasses, ZDbcIntfs, ZDbcStatement, ZDbcMySql, ZPlainMySqlDriver, ZPlainMySqlConstants,
  ZCompatibility, ZDbcLogging, ZVariant;

type

  {** Represents a MYSQL specific connection interface. }
  IZMySQLStatement = interface (IZStatement)
    ['{A05DB91F-1E40-46C7-BF2E-25D74978AC83}']

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;
    function GetStmtHandle: PZMySqlPrepStmt;
  end;

  {** Represents a MYSQL prepared Statement specific connection interface. }
  IZMySQLPreparedStatement = interface (IZMySQLStatement)
    ['{A05DB91F-1E40-46C7-BF2E-25D74978AC83}']
  end;

  {** Implements Generic MySQL Statement. }
  TZMySQLStatement = class(TZAbstractStatement, IZMySQLStatement)
  private
    FHandle: PZMySQLConnect;
    FPlainDriver: IZMySQLPlainDriver;
    FUseResult: Boolean;
    FSQL: string;

    function CreateResultSet(const SQL: string): IZResultSet;
    function GetStmtHandle : PZMySqlPrepStmt;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; Info: TStrings; Handle: PZMySQLConnect);

    function ExecuteQuery(const SQL: string): IZResultSet; override;
    function ExecuteUpdate(const SQL: string): Integer; override;
    function Execute(const SQL: string): Boolean; override;

    function GetMoreResults: Boolean; override;

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;
  end;

  {** Implements Prepared SQL Statement. }
  TZMySQLEmulatedPreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FHandle: PZMySQLConnect;
    FPlainDriver: IZMySQLPlainDriver;
  protected
    function CreateExecStatement: IZStatement; override;
    function GetEscapeString(const Value: string): string;
    function GetAnsiEscapeString(const Value: AnsiString): AnsiString;
    function PrepareSQLParam(ParamIndex: Integer): string; override;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings;
      Handle: PZMySQLConnect);
  end;

  TZMysqlColumnBuffer = Array of PDOBindRecord2;
  { TZMySQLBindBuffer }
  {** Encapsulates a MySQL bind buffer. }
  TZMySQLBindBuffer = class(TZAbstractObject)
  protected
    FDriverVersion : Integer;
    FAddedColumnCount : Integer;
    FBindArray41: Array of MYSQL_BIND41;
    FBindArray50: Array of MYSQL_BIND50;
    FBindArray51: Array of MYSQL_BIND51;
    FBindArray60: Array of MYSQL_BIND60;
    FPColumnArray: ^TZMysqlColumnBuffer;
  public
    constructor Create(PlainDriver:IZMysqlPlainDriver; NumColumns : Integer; var ColumnArray:TZMysqlColumnBuffer);
    destructor Destroy; override;
    procedure AddColumn(buffertype:TMysqlFieldTypes; display_length:integer);
    function GetColumnArray : TZMysqlColumnBuffer;
    function GetBufferAddress : Pointer;
    function GetBufferType(ColumnIndex: Integer) : TMysqlFieldTypes;
  end;

  {** Implements Prepared SQL Statement. }

  { TZMySQLPreparedStatement }

  TZMySQLPreparedStatement = class(TZAbstractPreparedStatement,IZMySQLPreparedStatement)
  private
    FHandle: PZMySQLConnect;
    FMySQLConnection: IZMySQLConnection;
    FStmtHandle: PZMySqlPrepStmt;
    FPlainDriver: IZMySQLPlainDriver;
    FUseResult: Boolean;

    FColumnArray: TZMysqlColumnBuffer;
    FBindBuffer: TZMysqlBindBuffer;

    function CreateResultSet(const SQL: string): IZResultSet;

    function getFieldType (testVariant: TZVariant): TMysqlFieldTypes;
  protected
    function GetStmtHandle : PZMySqlPrepStmt;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    property StmtHandle: PZMySqlPrepStmt read GetStmtHandle;
    constructor Create(PlainDriver: IZMysqlPlainDriver; Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Prepare; override;

    function ExecuteQuery(const SQL: string): IZResultSet; override;
    function ExecuteUpdate(const SQL: string): Integer; override;
    function Execute(const SQL: string): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;
  end;

implementation

uses
  Types, ZDbcMySqlUtils, ZDbcMySqlResultSet, ZSysUtils, ZDbcResultSetMetadata,
  ZMessages, ZDbcCachedResultSet, ZDbcUtils, DateUtils;

{ TZMySQLStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL plain driver.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Info a statement parameters.
}
constructor TZMySQLStatement.Create(PlainDriver: IZMySQLPlainDriver;
  Connection: IZConnection; Info: TStrings; Handle: PZMySQLConnect);
var
  MySQLConnection: IZMySQLConnection;
begin
  inherited Create(Connection, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;

  MySQLConnection := Connection as IZMySQLConnection;
  FUseResult := StrToBoolEx(DefineStatementParameter(Self, 'useresult', 'false'));
end;

{**
  Checks is use result should be used in result sets.
  @return <code>True</code> use result in result sets,
    <code>False</code> store result in result sets.
}
function TZMySQLStatement.IsUseResult: Boolean;
begin
  Result := FUseResult;
end;

{**
  Checks if this is a prepared mysql statement.
  @return <code>False</code> This is not a prepared mysql statement.
}
function TZMySQLStatement.IsPreparedStatement: Boolean;
begin
  Result := False;
end;

function TZMySQLStatement.GetStmtHandle: PZMySqlPrepStmt;
begin
  Result := nil;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZMySQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZMySQLResultSet.Create(FPlainDriver, Self, SQL, FHandle,
    FUseResult);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (GetResultSetConcurrency <> rcReadOnly) or (FUseResult
    and (GetResultSetType <> rtForwardOnly)) then
  begin
    CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver, FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);
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
function TZMySQLStatement.ExecuteQuery(const SQL: string): IZResultSet;
begin
  Result := nil;
  {$IFDEF DELPHI12_UP}
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(AnsiString(SQL))) = 0 then
  {$ELSE}
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(SQL)) = 0 then
  {$ENDIF}
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    if not FPlainDriver.ResultSetExists(FHandle) then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    Result := CreateResultSet(SQL);
  end
  else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
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
function TZMySQLStatement.ExecuteUpdate(const SQL: string): Integer;
var
  QueryHandle: PZMySQLResult;
  HasResultset : Boolean;
begin
  Result := -1;
  {$IFDEF DELPHI12_UP}
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(AnsiString(SQL))) = 0 then
  {$ELSE}
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(SQL)) = 0 then
  {$ENDIF}
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    HasResultSet := FPlainDriver.ResultSetExists(FHandle);
    { Process queries with result sets }
    if HasResultSet then
    begin
      QueryHandle := FPlainDriver.StoreResult(FHandle);
      if QueryHandle <> nil then
      begin
        Result := FPlainDriver.GetRowCount(QueryHandle);
        FPlainDriver.FreeResult(QueryHandle);
      end
      else
        Result := FPlainDriver.GetAffectedRows(FHandle);
    end
    { Process regular query }
    else
      Result := FPlainDriver.GetAffectedRows(FHandle);
  end
  else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
  LastUpdateCount := Result;
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
function TZMySQLStatement.Execute(const SQL: string): Boolean;
var
  HasResultset : Boolean;
begin
  Result := False;
  FSQL := SQL;
  {$IFDEF DELPHI12_UP}
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(AnsiString(SQL))) = 0 then
  {$ELSE}
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(SQL)) = 0 then
  {$ENDIF}
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    HasResultSet := FPlainDriver.ResultSetExists(FHandle);
    { Process queries with result sets }
    if HasResultSet then
    begin
      Result := True;
      LastResultSet := CreateResultSet(SQL);
    end
    { Processes regular query. }
    else
    begin
      Result := False;
      LastUpdateCount := FPlainDriver.GetAffectedRows(FHandle);
    end;
  end
  else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SQL);
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
function TZMySQLStatement.GetMoreResults: Boolean;
var
  AStatus: integer;
begin
  Result := inherited GetMoreResults;
  if FPlainDriver.GetClientVersion >= 40100 then
  begin
    AStatus := FPlainDriver.RetrieveNextRowset(FHandle);
    if AStatus > 0 then
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, FSQL)
    else
      Result := (AStatus = 0);

    if LastResultSet <> nil then
      LastResultSet.Close;
    LastResultSet := nil;
    LastUpdateCount := -1;
    if FPlainDriver.ResultSetExists(FHandle) then
      LastResultSet := CreateResultSet(FSQL)
    else
      LastUpdateCount := FPlainDriver.GetAffectedRows(FHandle);
  end;
end;

{ TZMySQLEmulatedPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL Plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZMySQLEmulatedPreparedStatement.Create(PlainDriver: IZMySQLPlainDriver;
  Connection: IZConnection; const SQL: string; Info: TStrings; Handle: PZMySQLConnect);
begin
  inherited Create(Connection, SQL, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZMySQLEmulatedPreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZMySQLStatement.Create(FPlainDriver, Connection, Info,FHandle);
end;

{**
  Converts an string into escape MySQL format.
  @param Value a regular string.
  @return a string in MySQL escape format.
}
function TZMySQLEmulatedPreparedStatement.GetEscapeString(const Value: string): string;
var
  BufferLen: Integer;
  Buffer: PAnsiChar;
begin
  BufferLen := Length(Value) * 2 + 1;
  GetMem(Buffer, BufferLen);
  if FHandle = nil then
  {$IFDEF DELPHI12_UP}
    BufferLen := FPlainDriver.GetEscapeString(Buffer, PAnsiChar(UTF8Encode(Value)), Length(PAnsiChar(UTF8Encode(Value))))
  else
    BufferLen := FPlainDriver.GetRealEscapeString(FHandle, Buffer, PAnsiChar(UTF8Encode(Value)), Length(PAnsiChar(UTF8Encode(Value))));
  {$ELSE}
    BufferLen := FPlainDriver.GetEscapeString(Buffer, PAnsiChar(Value), Length(Value))
   else
    BufferLen := FPlainDriver.GetRealEscapeString(FHandle, Buffer, PAnsiChar(Value), Length(Value));
  {$ENDIF}
  Result := '''' + BufferToStr(Buffer, BufferLen) + '''';
  FreeMem(Buffer);
end;

{**
  Converts an ansi string (binary data) into escape MySQL format.
  @param Value a regular string.
  @return a string in MySQL escape format.
}
function TZMySQLEmulatedPreparedStatement.GetAnsiEscapeString(const Value: AnsiString): AnsiString;
var
  BufferLen: Integer;
  Buffer: PAnsiChar;
begin
  BufferLen := Length(Value) * 2 + 1;
  GetMem(Buffer, BufferLen);
  if FHandle = nil then
    BufferLen := FPlainDriver.GetEscapeString(Buffer, PAnsiChar(Value), Length(Value))
   else
    BufferLen := FPlainDriver.GetRealEscapeString(FHandle, Buffer, PAnsiChar(Value), Length(Value));
  Result := '''' + BufferToStr(Buffer, BufferLen) + '''';
  FreeMem(Buffer);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZMySQLEmulatedPreparedStatement.PrepareSQLParam(ParamIndex: Integer): string;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;

  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if DefVarManager.IsNull(Value) then
    if (InParamDefaultValues[ParamIndex] <> '') and
      StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true')) then
      Result := InParamDefaultValues[ParamIndex]
    else
      Result := 'NULL'
  else
  begin
    case InParamTypes[ParamIndex] of
      stBoolean:
            if SoftVarManager.GetAsBoolean(Value) then
               Result := '''Y'''
            else
               Result := '''N''';
      stByte, stShort, stInteger, stLong, stBigDecimal, stFloat, stDouble:
        Result := SoftVarManager.GetAsString(Value);
      stString, stUnicodeString, stBytes:
        Result := GetEscapeString(SoftVarManager.GetAsString(Value));
      stDate:
      begin
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        Result := '''' + Format('%0.4d-%0.2d-%0.2d',
          [AYear, AMonth, ADay]) + '''';
      end;
      stTime:
      begin
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        Result := '''' + Format('%0.2d:%0.2d:%0.2d',
          [AHour, AMinute, ASecond]) + '''';
      end;
      stTimestamp:
      begin
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        Result := '''' + Format('%0.4d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d',
          [AYear, AMonth, ADay, AHour, AMinute, ASecond]) + '''';
      end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
            Result := GetAnsiEscapeString(TempBlob.GetString)
          else
            Result := 'NULL';
        end;
{   ????
     stUnicodeStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
            Result := GetEscapeString(TempBlob.GetString)
          else
            Result := 'NULL';
        end;}
    end;
  end;
end;

{ TZMySQLPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZMySQLPreparedStatement.Create(
  PlainDriver: IZMySQLPlainDriver; Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FMySQLConnection := Connection as IZMySQLConnection;
  FHandle := FMysqlConnection.GetConnectionHandle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;

  FUseResult := StrToBoolEx(DefineStatementParameter(Self, 'useresult', 'false'));

  Prepare;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZMySQLPreparedStatement.Destroy;
begin
  FStmtHandle := FPlainDriver.ClosePrepStmt(FStmtHandle);
  inherited Destroy;
end;

procedure TZMySQLPreparedStatement.Prepare;
begin
  FStmtHandle := FPlainDriver.InitializePrepStmt(FHandle);
  if (FStmtHandle = nil) then
    begin
      CheckMySQLPrepStmtError(FPlainDriver, FStmtHandle, lcPrepStmt, SFailedtoInitPrepStmt);
      exit;
    end;
  {$IFDEF DELPHI12_UP}
  if (FPlainDriver.PrepareStmt(FStmtHandle, PAnsiChar(AnsiString(SQL)), length(SQL)) <> 0) then
  {$ELSE}
  if (FPlainDriver.PrepareStmt(FStmtHandle, PAnsiChar(SQL), length(SQL)) <> 0) then
  {$ENDIF}
    begin
      CheckMySQLPrepStmtError(FPlainDriver, FStmtHandle, lcPrepStmt, SFailedtoPrepareStmt);
      exit;
    end;
  LogPrepStmtMessage(lcPrepStmt, SQL);
  inherited Prepare;
end;

{**
  Checks is use result should be used in result sets.
  @return <code>True</code> use result in result sets,
    <code>False</code> store result in result sets.
}
function TZMySQLPreparedStatement.IsUseResult: Boolean;
begin
  Result := FUseResult;
end;

{**
  Checks if this is a prepared mysql statement.
  @return <code>True</code> This is a prepared mysql statement.
}
function TZMySQLPreparedStatement.IsPreparedStatement: Boolean;
begin
  Result := True;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLPreparedStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZMySQLPreparedResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZMySQLPreparedResultSet.Create(FPlainDriver, Self, SQL, FHandle,
    FUseResult);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (GetResultSetConcurrency <> rcReadOnly) or (FUseResult
    and (GetResultSetType <> rtForwardOnly)) then
  begin
    CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver, FHandle, (Self as IZMysqlStatement),
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

procedure TZMySQLPreparedStatement.PrepareInParameters;
begin
  // Empty : Mysql can't prepare datastructures before the actual parameters are known, because the
  // number/datatype of parameters isn't returned by the server.
  inherited PrepareInParameters;
end;

procedure TZMysqlPreparedStatement.BindInParameters;
var
    caststring : AnsiString;
    PBuffer: Pointer;
    year, month, day, hour, minute, second, millisecond: word;
  I,J : integer;
begin
  if InParamCount = 0 then
     exit;
  { Initialize Bind Array and Column Array }
  FBindBuffer := TZMysqlBindBuffer.Create(FPlainDriver,InParamCount,FColumnArray);

  For I := 0 to InParamCount - 1 do
  begin
    FBindBuffer.AddColumn(GetFieldType(InParamValues[I]),length(InParamValues[I].VString));
    PBuffer := @FColumnArray[I].buffer[0];

        if InParamValues[I].VType=vtNull then
         FColumnArray[I].is_null := 1
        else
         FColumnArray[I].is_null := 0;
            case FBindBuffer.GetBufferType(I+1) of
              FIELD_TYPE_FLOAT:    Single(PBuffer^)     := InParamValues[I].VFloat;
              FIELD_TYPE_STRING:
                begin
                  CastString := InParamValues[I].VString;
                  for J := 1 to system.length(CastString) do
                    begin
                      PAnsiChar(PBuffer)^ := CastString[J];
                      inc(PAnsiChar(PBuffer));
                    end;
                  PAnsiChar(PBuffer)^ := chr(0);
                end;
              FIELD_TYPE_LONGLONG: Int64(PBuffer^) := InParamValues[I].VInteger;
              FIELD_TYPE_DATETIME:
                begin
                  DecodeDateTime(InParamValues[I].VDateTime, Year, Month, Day, hour, minute, second, millisecond);
                  PMYSQL_TIME(PBuffer)^.year := year;
                  PMYSQL_TIME(PBuffer)^.month := month;
                  PMYSQL_TIME(PBuffer)^.day := day;
                  PMYSQL_TIME(PBuffer)^.hour := hour;
                  PMYSQL_TIME(PBuffer)^.minute := minute;
                  PMYSQL_TIME(PBuffer)^.second := second;
                  PMYSQL_TIME(PBuffer)^.second_part := millisecond;
                end;
            end;
  end;

  if (FPlainDriver.BindParameters(FStmtHandle, FBindBuffer.GetBufferAddress) <> 0) then
    begin
      checkMySQLPrepStmtError (FPlainDriver, FStmtHandle, lcPrepStmt, SBindingFailure);
      exit;
    end;
   inherited BindInParameters;
end;

procedure TZMySQLPreparedStatement.UnPrepareInParameters;
begin
  // Empty : Mysql can't prepare datastructures before the actual parameters are known, because the
  // number/datatype of parameters isn't returned by the server.
  inherited UnPrepareInParameters;
end;

function TZMysqlPreparedStatement.getFieldType (testVariant: TZVariant): TMysqlFieldTypes;
begin
    case testVariant.vType of
        vtNull:      Result := FIELD_TYPE_TINY;
        vtBoolean:   Result := FIELD_TYPE_TINY;
        vtInteger:   Result := FIELD_TYPE_LONGLONG;
        vtFloat:    Result := FIELD_TYPE_FLOAT;
        vtString:    Result := FIELD_TYPE_STRING;
        vtDateTime:  Result := FIELD_TYPE_DATETIME;
     else
        raise EZSQLException.Create(SUnsupportedDataType);
     end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZMySQLPreparedStatement.ExecuteQuery(const SQL: string): IZResultSet;
begin
  Self.SQL := SQL;
  Result := ExecuteQueryPrepared;
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
function TZMySQLPreparedStatement.ExecuteUpdate(const SQL: string): Integer;
begin
  Self.SQL := SQL;
  Result := ExecuteUpdatePrepared;
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
function TZMySQLPreparedStatement.Execute(const SQL: string): Boolean;
begin
  Self.SQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZMySQLPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;
  BindInParameters;
  if (self.FPlainDriver.ExecuteStmt(FStmtHandle) <> 0) then
     begin
        checkMySQLPrepStmtError(FPlainDriver,FStmtHandle, lcExecPrepStmt, SPreparedStmtExecFailure);
        exit;
     end;

  FBindBuffer.Free;

  if FPlainDriver.GetPreparedFieldCount(FStmtHandle) = 0 then
      raise EZSQLException.Create(SCanNotOpenResultSet);
  Result := CreateResultSet(SQL);
  inherited ExecuteQueryPrepared;
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZMySQLPreparedStatement.ExecuteUpdatePrepared: Integer;
var
  QueryHandle: PZMySQLResult;
  HasResultset : Boolean;
begin
  Result := -1;
  BindInParameters;
  if (self.FPlainDriver.ExecuteStmt(FStmtHandle) <> 0) then
     begin
        checkMySQLPrepStmtError(FPlainDriver,FStmtHandle, lcExecPrepStmt, SPreparedStmtExecFailure);
        exit;
     end;

  FBindBuffer.Free;

    { Process queries with result sets }
  if FPlainDriver.GetPreparedFieldCount(FStmtHandle) > 0 then
    begin
      FPlainDriver.StorePreparedResult(FStmtHandle);
      Result := FPlainDriver.GetPreparedAffectedRows(FStmtHandle);
    end
    { Process regular query }
  else
    Result := FPlainDriver.GetPreparedAffectedRows(FStmtHandle);
  LastUpdateCount := Result;
  Inherited ExecuteUpdatePrepared;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZMySQLPreparedStatement.ExecutePrepared: Boolean;
var
  HasResultset : Boolean;
begin
  Result := False;
  BindInParameters;
  if (FPlainDriver.ExecuteStmt(FStmtHandle) <> 0) then
     begin
        checkMySQLPrepStmtError(FPlainDriver,FStmtHandle, lcExecPrepStmt, SPreparedStmtExecFailure);
        exit;
     end;

  FBindBuffer.Free;

  if FPlainDriver.GetPreparedFieldCount(FStmtHandle) > 0 then
    begin
      Result := True;
      LastResultSet := CreateResultSet(SQL);
    end
    { Processes regular query. }
  else
    begin
      Result := False;
      LastUpdateCount := FPlainDriver.GetPreparedAffectedRows(FStmtHandle);
    end;
  inherited ExecutePrepared;
end;

function TZMySQLPreparedStatement.GetStmtHandle: PZMySqlPrepStmt;
begin
  Result := FStmtHandle;
end;

{ TZMySQLBindBuffer }

constructor TZMySQLBindBuffer.Create(PlainDriver: IZMysqlPlainDriver; NumColumns: Integer; var ColumnArray:TZMysqlColumnBuffer);
begin
  inherited Create;
  FDriverVersion := PlainDriver.GetClientVersion;
  FPColumnArray := @ColumnArray;
  setlength(FBindArray41,0);
  setlength(FBindArray50,0);
  setlength(FBindArray51,0);
  setlength(FBindArray60,0);
  setlength(ColumnArray,NumColumns);
  Case FDriverVersion of
    40100..40199 : setlength(FBindArray41,NumColumns);
    50000..50099 : setlength(FBindArray50,NumColumns);
    50100..59999 : setlength(FBindArray51,NumColumns);
    60000..60099 : setlength(FBindArray60,NumColumns);
  else
    raise EZSQLException.Create('Unknown dll version : '+IntToStr(FDriverVersion));
  End
end;

destructor TZMySQLBindBuffer.Destroy;
begin
  inherited Destroy;
end;

procedure TZMySQLBindBuffer.AddColumn(buffertype: TMysqlFieldTypes;
  display_length: integer);
  var tempbuffertype: TMysqlFieldTypes;
begin
  Case buffertype of
    FIELD_TYPE_DECIMAL,
    FIELD_TYPE_NEWDECIMAL: tempbuffertype := FIELD_TYPE_DOUBLE;
  Else
    tempbuffertype := buffertype;
  End;
  Inc(FAddedColumnCount);
  With FPColumnArray^[FAddedColumnCount-1] do
    begin
      length := getMySQLFieldSize(tempbuffertype,display_length);
      SetLength(buffer,length);
      is_null := 0;
    end;
  Case FDriverVersion of
    40100..40199 : With FBindArray41[FAddedColumnCount-1] do
                     begin
                       buffer_type   := tempbuffertype;
                       buffer_length := FPColumnArray^[FAddedColumnCount-1].length;
                       is_unsigned   := 0;
                       buffer        := @FPColumnArray^[FAddedColumnCount-1].buffer[0];
                       length        := @FPColumnArray^[FAddedColumnCount-1].length;
                       is_null       := @FPColumnArray^[FAddedColumnCount-1].is_null;
                     end;
    50000..50099 : With FBindArray50[FAddedColumnCount-1] do
                     begin
                       buffer_type   := tempbuffertype;
                       buffer_length := FPColumnArray^[FAddedColumnCount-1].length;
                       is_unsigned   := 0;
                       buffer        := @FPColumnArray^[FAddedColumnCount-1].buffer[0];
                       length        := @FPColumnArray^[FAddedColumnCount-1].length;
                       is_null       := @FPColumnArray^[FAddedColumnCount-1].is_null;
                     end;
    50100..59999 : With FBindArray51[FAddedColumnCount-1] do
                     begin
                       buffer_type   := tempbuffertype;
                       buffer_length := FPColumnArray^[FAddedColumnCount-1].length;
                       is_unsigned   := 0;
                       buffer        := @FPColumnArray^[FAddedColumnCount-1].buffer[0];
                       length        := @FPColumnArray^[FAddedColumnCount-1].length;
                       is_null       := @FPColumnArray^[FAddedColumnCount-1].is_null;
                     end;
    60000..60099 : With FBindArray60[FAddedColumnCount-1] do
                     begin
                       buffer_type   := tempbuffertype;
                       buffer_length := FPColumnArray^[FAddedColumnCount-1].length;
                       is_unsigned   := 0;
                       buffer        := @FPColumnArray^[FAddedColumnCount-1].buffer[0];
                       length        := @FPColumnArray^[FAddedColumnCount-1].length;
                       is_null       := @FPColumnArray^[FAddedColumnCount-1].is_null;
                     end;
  else
    raise EZSQLException.Create('Unknown dll version : '+IntToStr(FDriverVersion));
  End
end;

function TZMySQLBindBuffer.GetColumnArray: TZMysqlColumnBuffer;
begin
  result := FPColumnArray^;
end;

function TZMySQLBindBuffer.GetBufferAddress: Pointer;
begin
  Case FDriverVersion of
    40100..40199 : result := @FBindArray41[0];
    50000..50099 : result := @FBindArray50[0];
    50100..59999 : result := @FBindArray51[0];
    60000..60099 : result := @FBindArray60[0];
  else
    result := nil;
    raise EZSQLException.Create('Unknown dll version : '+IntToStr(FDriverVersion));
  End
end;

function TZMySQLBindBuffer.GetBufferType(ColumnIndex: Integer): TMysqlFieldTypes;
begin
  Case FDriverVersion of
    40100..40199 : result := FBindArray41[ColumnIndex-1].buffer_type;
    50000..50099 : result := FBindArray50[ColumnIndex-1].buffer_type;
    50100..59999 : result := FBindArray51[ColumnIndex-1].buffer_type;
    60000..60099 : result := FBindArray60[ColumnIndex-1].buffer_type;
  else
    result := TMysqlFieldTypes(0);
    raise EZSQLException.Create('Unknown dll version : '+IntToStr(FDriverVersion));
  End

end;
end.
