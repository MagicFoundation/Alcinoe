{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for MSSql Database Connectivity Classes     }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZTestDbcMsSql;

interface
{$I ZDbc.inc}
uses
  Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZDbcDbLib, ZDbcDbLibResultSet,
  ZTestDefinitions, ZCompatibility, ZDbcDbLibMsSqlMetadata;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcMSSqlCase = class(TZDbcSpecificSQLTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: IZConnection read FConnection write FConnection;

  published
    procedure TestConnection;
    procedure TestStoredResultSet;
    procedure TestUseResultSet;
    procedure TestPreparedStatement;
    procedure TestStatement;
    procedure TestDefaultValues;
    procedure TestStoredprocedures;
  end;

implementation

uses ZSysUtils, ZTestConsts;

{ TZTestDbcMSSqlCase classes }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcMSSqlCase.GetSupportedProtocols: string;
begin
  Result := 'mssql';
end;

{**
   Create objects and allocate memory for variables
}
procedure TZTestDbcMSSqlCase.SetUp;
begin
  Connection := CreateDbcConnection;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestDbcMSSqlCase.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

{**
  Runs a test for DBC connection.
}
procedure TZTestDbcMSSqlCase.TestConnection;
begin
(*  if Protocol <> 'mssql' then Exit;

  CheckEquals(True, Connection.IsReadOnly);
//  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  Connection.SetAutoCommit(False);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

  { Check without transactions }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Check with transactions }
  Connection.Open;
  Connection.SetAutoCommit(False);
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed); *)
end;

{**
  Runs a test for MySQL DBC PreparedStatement.
}
procedure TZTestDbcMsSQLCase.TestPreparedStatement;
var
  Statement: IZPreparedStatement;
  Stream: TStream;
begin
  Statement := Connection.PrepareStatement(
    'INSERT INTO department(dep_id,dep_name,dep_shname,dep_address)'
    + ' VALUES(?,?,?,?)');
  CheckNotNull(Statement);

  Statement.SetInt(1, TEST_ROW_ID);
  Statement.SetString(2, 'xyz');
  Statement.SetNull(3, stString);
  Stream := TStringStream.Create('abc'#10'def'#13'hgi');
  try
    Statement.SetAsciiStream(4, Stream);
  finally
    Stream.Free;
  end;
  CheckEquals(1, Statement.ExecuteUpdatePrepared);

  Statement := Connection.PrepareStatement(
    'DELETE FROM department WHERE dep_id=?');
  CheckNotNull(Statement);

  Statement.SetInt(1, TEST_ROW_ID);
  CheckEquals(1, Statement.ExecuteUpdatePrepared);
  Statement.ExecutePrepared;
  CheckEquals(0, Statement.GetUpdateCount);
end;

{**
  Runs a test for regular MySQL DBC Statement.
}
procedure TZTestDbcMsSQLCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
  Statement.ExecuteUpdate('SELECT * FROM equipment');

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));
end;

{**
  Runs a test for MySQL DBC ResultSet with stored results.
}
procedure TZTestDbcMsSQLCase.TestStoredResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Runs a test for MySQL DBC ResultSet with use results.
}
procedure TZTestDbcMsSQLCase.TestUseResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtForwardOnly);
  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, False);
  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Runs a test for MySQL default values.
}
procedure TZTestDbcMsSQLCase.TestDefaultValues;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from default_values');

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3,d_fld4,d_fld5,d_fld6 FROM default_values');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(1, 1);
  ResultSet.InsertRow;

  Check(ResultSet.GetInt(1) <> 0);
  CheckEquals(123456, ResultSet.GetInt(2));
  CheckEquals(123.456, ResultSet.GetFloat(3), 0.001);
  CheckEquals('xyz', ResultSet.GetString(4));
  CheckEquals(EncodeDate(2003, 12, 11), ResultSet.GetDate(5), 0);
  CheckEquals(EncodeTime(23, 12, 11, 0), ResultSet.GetTime(6), 3);
  CheckEquals(EncodeDate(2003, 12, 11) +
    EncodeTime(23, 12, 11, 0), ResultSet.GetTimestamp(7), 3);

  ResultSet.DeleteRow;

  ResultSet.Close;
  Statement.Close;
end;

{**
  Runs a test for Interbase stored procedures.
}
procedure TZTestDbcMSSqlCase.TestStoredprocedures;
var
  ResultSet: IZResultSet;
  CallableStatement: IZCallableStatement;
begin
  CallableStatement := Connection.PrepareCallWithParams(
    'procedure1', nil);
  with CallableStatement do
  begin
    RegisterOutParameter(1, Ord(stInteger)); //stupid RETURN_VALUE
    SetInt(2, 12345);
    RegisterOutParameter(3, Ord(stInteger));
    ExecutePrepared;
    CheckEquals(12346, GetInt(2));
  end;
  CallableStatement.Close;

  CallableStatement := Connection.PrepareCallWithParams(
    'procedure2', nil);
  ResultSet := CallableStatement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    CheckEquals(True, Next);
    CheckEquals('Computer', GetString(1));
    CheckEquals(True, Next);
    CheckEquals('Laboratoy', GetString(1));
    CheckEquals(True, Next);
    CheckEquals('Radiostation', GetString(1));
    CheckEquals(True, Next);
    CheckEquals('Volvo', GetString(1));
    Close;
  end;
  CallableStatement.Close;
end;

initialization
  RegisterTest('dbc',TZTestDbcMSSqlCase.Suite);
end.


