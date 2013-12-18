{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for MySql Database Connectivity Classes     }
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

unit ZTestDbcMySql;

interface
{$I ZDbc.inc}

uses Classes, SysUtils, ZDbcIntfs, ZTestDefinitions, ZDbcMySql,
  ZCompatibility, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF};

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcMySQLCase = class(TZDbcSpecificSQLTestCase)
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
    procedure TestAutoIncFields;
    procedure TestDefaultValues;
  end;


implementation

uses ZSysUtils, ZTestConsts;

{ TZTestDbcMySqlCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcMySQLCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5';
end;

{**
   Create objects and allocate memory for variables
}
procedure TZTestDbcMySQLCase.SetUp;
begin
  Connection := CreateDbcConnection;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestDbcMySQLCase.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

{**
  Runs a test for MySQL database connection.
}
procedure TZTestDbcMySQLCase.TestConnection;
begin
  CheckEquals(True, Connection.IsReadOnly);
//  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

  { Checks without transactions. }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiReadCommitted);
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

{**
  Runs a test for MySQL DBC PreparedStatement.
}
procedure TZTestDbcMySQLCase.TestPreparedStatement;
var
  Statement: IZPreparedStatement;
  Stream: TStream;
begin
  Statement := Connection.PrepareStatement(
    'INSERT INTO department(dep_id,dep_name,dep_shname,dep_address)'
    + ' VALUES(?,?,?,?)');
  try
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
  finally
    Statement.Close;
  end;

  Statement := Connection.PrepareStatement(
    'DELETE FROM department WHERE dep_id=?');
  try
    CheckNotNull(Statement);

    Statement.SetInt(1, TEST_ROW_ID);
    CheckEquals(1, Statement.ExecuteUpdatePrepared);
    Statement.ExecutePrepared;
    CheckEquals(0, Statement.GetUpdateCount);
  finally
    Statement.Close;
  end;
end;

{**
  Runs a test for regular MySQL DBC Statement.
}
procedure TZTestDbcMySQLCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  try
    CheckNotNull(Statement);

    Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
    Statement.ExecuteUpdate('SELECT * FROM equipment');

    Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
    Check(Statement.Execute('SELECT * FROM equipment'));
  finally
    Statement.Close;
  end;
end;

{**
  Runs a test for MySQL DBC ResultSet with stored results.
}
procedure TZTestDbcMySQLCase.TestStoredResultSet;
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
procedure TZTestDbcMySQLCase.TestUseResultSet;
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
  Runs a test for MySQL AutoIncremented fields.
}
procedure TZTestDbcMySQLCase.TestAutoIncFields;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  ResultSet := Statement.ExecuteQuery('SELECT c_id, c_name FROM cargo');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateString(2, 'xxx');
  CheckEquals(0, ResultSet.GetInt(1));
  CheckEquals('xxx', ResultSet.GetString(2));

  ResultSet.InsertRow;
  Check(ResultSet.GetInt(1) <> 0);
  CheckEquals('xxx', ResultSet.GetString(2));

  ResultSet.DeleteRow;

  ResultSet.Close;

  Statement.Close;
end;

{**
  Runs a test for MySQL default values.
}
procedure TZTestDbcMySQLCase.TestDefaultValues;
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

initialization
  RegisterTest('dbc',TZTestDbcMySQLCase.Suite);
end.
