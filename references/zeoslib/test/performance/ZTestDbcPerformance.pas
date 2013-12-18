{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for ZDBC API Performance           }
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

unit ZTestDbcPerformance;

interface

uses TestFramework, SysUtils, Classes, ZPerformanceTestCase, ZDbcIntfs,
  ZCompatibility
  {$IFDEF ENABLE_MYSQL}
    ZDbcMySql, ZPlainMySqlDriver,
  {$ENDIF}
  ;

type

  {** Implements a performance test case for Native DBC API. }
  TZNativeDbcPerformanceTestCase = class (TZPerformanceSQLTestCase)
  protected
    FConnection: IZConnection;
  protected
    property Connection: IZConnection read FConnection write FConnection;

    function GetImplementedAPI: string; override;
    function CreateResultSet(Query: string): IZResultSet; virtual;

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

  {** Implements a performance test case for Native DBC API. }
  TZCachedDbcPerformanceTestCase = class (TZNativeDbcPerformanceTestCase)
  protected
    function GetImplementedAPI: string; override;
    function CreateResultSet(Query: string): IZResultSet; override;

    procedure RunTestInsert; override;
    procedure RunTestUpdate; override;
    procedure RunTestDelete; override;
  end;

implementation

uses ZTestCase, ZSysUtils;

{ TZNativeDbcPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZNativeDbcPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dbc';
end;

{**
  Creates a specific for this test result set.
  @param Query a SQL query string.
  @return a created Result Set for the SQL query.
}
function TZNativeDbcPerformanceTestCase.CreateResultSet(
  Query: string): IZResultSet;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  Statement.SetFetchDirection(fdForward);
  Statement.SetResultSetConcurrency(rcReadOnly);
  Statement.SetResultSetType(rtForwardOnly);
  Result := Statement.ExecuteQuery(Query);
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZNativeDbcPerformanceTestCase.DefaultSetUpTest;
begin
  Connection := CreateDbcConnection;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZNativeDbcPerformanceTestCase.DefaultTearDownTest;
begin
  if Connection <> nil then
  begin
    Connection.Close;
    Connection := nil;
  end;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZNativeDbcPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestConnect;
begin
  Connection := CreateDbcConnection;
end;

{**
  Performs an insert test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestInsert;
var
  I: Integer;
  Statement: IZPreparedStatement;
begin
  Statement := Connection.PrepareStatement(
    'INSERT INTO high_load VALUES (?,?,?)');
  for I := 1 to GetRecordCount do
  begin
    Statement.SetInt(1, I);
    Statement.SetFloat(2, RandomFloat(-100, 100));
    Statement.SetString(3, RandomStr(10));
    Statement.ExecuteUpdatePrepared;
  end;
end;

{**
  Performs an open test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestOpen;
begin
  CreateResultSet('SELECT * FROM high_load');
end;

{**
   Performs a fetch data
}
procedure TZNativeDbcPerformanceTestCase.RunTestFetch;
var
  ResultSet: IZResultSet;
begin
  ResultSet := CreateResultSet('SELECT * FROM high_load');
  while ResultSet.Next do
  begin
//    ResultSet.GetPChar(1);
//    ResultSet.GetPChar(2);
//    ResultSet.GetPChar(3);
    ResultSet.GetInt(1);
    ResultSet.GetFloat(2);
    ResultSet.GetString(3);
  end;
end;

{**
  Performs an update test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestUpdate;
var
  I: Integer;
  Statement: IZPreparedStatement;
begin
  Statement := Connection.PrepareStatement('UPDATE high_load SET '
    + ' data1=?, data2=?  WHERE hl_id=?');
  for I := 1 to GetRecordCount do
  begin
    Statement.SetFloat(1, RandomFloat(-100, 100));
    Statement.SetString(2, RandomStr(10));
    Statement.SetInt(3, I);
    Statement.ExecuteUpdatePrepared;
  end;
end;

{**
  Performs a delete test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestDelete;
var
  I: Integer;
  Statement: IZPreparedStatement;
begin
  Statement := Connection.PrepareStatement(
    'DELETE from high_load WHERE hl_id=?');
  for I := 1 to GetRecordCount do
  begin
    Statement.SetInt(1, I);
    Statement.ExecutePrepared;
  end;
end;

{**
  Performs a direct update test.
}
procedure TZNativeDbcPerformanceTestCase.RunTestDirectUpdate;
var
  I: Integer;
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  for I := 1 to GetRecordCount do
  begin
    Statement.ExecuteUpdate(Format('UPDATE high_load SET data1=%s, data2=''%s'''
      + ' WHERE hl_id = %d', [FloatToSqlStr(RandomFloat(-100, 100)),
      RandomStr(10), I]));
  end;
end;

{ TZCachedDbcPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZCachedDbcPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dbc-cached';
end;

{**
  Creates a specific for this test result set.
  @param Query a SQL query string.
  @return a created Result Set for the SQL query.
}
function TZCachedDbcPerformanceTestCase.CreateResultSet(
  Query: string): IZResultSet;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  Statement.SetFetchDirection(fdForward);
  Statement.SetResultSetConcurrency(rcUpdatable);
  Statement.SetResultSetType(rtScrollInsensitive);
  Result := Statement.ExecuteQuery(Query);
end;

{**
  Performs an insert test.
}
procedure TZCachedDbcPerformanceTestCase.RunTestInsert;
var
  I: Integer;
  ResultSet: IZResultSet;
begin
  ResultSet := CreateResultSet('SELECT * from high_load');
  for I := 1 to GetRecordCount do
  begin
    ResultSet.MoveToInsertRow;
    ResultSet.UpdateInt(1, I);
    ResultSet.UpdateFloat(2, RandomFloat(-100, 100));
    ResultSet.UpdateString(3, RandomStr(10));
    ResultSet.InsertRow;
  end;
end;

{**
  Performs an update test.
}
procedure TZCachedDbcPerformanceTestCase.RunTestUpdate;
var
  ResultSet: IZResultSet;
begin
  ResultSet := CreateResultSet('SELECT * from high_load');
  while ResultSet.Next do
  begin
    ResultSet.UpdateFloat(2, RandomFloat(-100, 100));
    ResultSet.UpdateString(3, RandomStr(10));
    ResultSet.UpdateRow;
  end;
end;

{**
  Performs a delete test.
}
procedure TZCachedDbcPerformanceTestCase.RunTestDelete;
var
  ResultSet: IZResultSet;
begin
  ResultSet := CreateResultSet('SELECT * from high_load');
  while ResultSet.Next do
    ResultSet.DeleteRow;
end;

initialization
  TestFramework.RegisterTest(TZNativeDbcPerformanceTestCase.Suite);
  TestFramework.RegisterTest(TZCachedDbcPerformanceTestCase.Suite);
end.

