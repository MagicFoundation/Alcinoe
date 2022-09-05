{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for Generic Database Connectivity Classes   }
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

unit ZTestDbcGeneric;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils, ZDbcIntfs, ZTestDefinitions,
  ZCompatibility;

type
  {** Implements a test case for . }
  TZGenericTestDbcResultSet = class(TZDbcPortableSQLTestCase)
  private
    FConnection: IZConnection;
  protected
    property Connection: IZConnection read FConnection write FConnection;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestConnection;
    procedure TestStatement;
    procedure TestPreparedStatement;
    procedure TestStoredResultSetUpdate;
    procedure TestCaseSensitive;
    procedure TestAliases;
    procedure TestStoredResultSet;
    procedure TestLastQuery;
  end;

implementation

uses ZSysUtils, ZTestConsts, ZTestCase;

{ TZGenericTestDbcResultSet }

{**
  Creates objects and allocate memory for variables
}
procedure TZGenericTestDbcResultSet.SetUp;
begin
  Connection := CreateDbcConnection;
end;

{**
  Destroys objects and free allocated memory for variables
}
procedure TZGenericTestDbcResultSet.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

{**
   Test table with aliases
}
procedure TZGenericTestDbcResultSet.TestAliases;
var
  Sql: string;
  Statement: IZStatement;
  ResultSet: IZResultSet;
//  StrStream, BinStream: TMemoryStream;
//  StrStream1, BinStream1: TStream;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Sql := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
  Connection.CreateStatement.ExecuteUpdate(Sql);
  Sql := 'DELETE FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
  Connection.CreateStatement.ExecuteUpdate(Sql);

  { Tests the equipment table }
  Sql := 'SELECT a.eq_id as id, a.eq_name as name, a.eq_type as type1,'
    + ' a.eq_cost + 10 as cost FROM equipment a where a.eq_id = '
    + IntToStr(TEST_ROW_ID);
  { Inserts test record to equipment }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '1. ' + Sql);
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateIntByName('id', TEST_ROW_ID);
    UpdateNullByName('name');
    UpdateNullByName('type1');
    UpdateNullByName('cost');
    InsertRow;
    Close;
  end;
  ResultSet := nil;

  { Updates row for equipment}
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '2. ' + Sql);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('id'));
    CheckEquals(True, IsNullByName('name'));
    CheckEquals(True, IsNullByName('type1'));
    CheckEquals(True, IsNullByName('cost'));

    UpdateStringByName('name', 'The some thing');
    UpdateIntByName('type1', 1);
    UpdateDoubleByName('cost', 12345.678);
    UpdateRow;
    Close;
  end;

  { Checks previous updated row}
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '3. ' + Sql);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals('The some thing', GetStringByName('name'));
    CheckEquals(1, GetIntByName('type1'));
// Column cost is calculated is can't be updated
//    CheckEquals(12355.678, GetFloatByName('cost'), 0.01);
    DeleteRow;
    Close;
  end;

  { Checks what record deleted }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '4. ' + Sql);
  CheckEquals(False, ResultSet.Next);
end;

{**
  Tests case sensetive tables
}
procedure TZGenericTestDbcResultSet.TestCaseSensitive;
var
  Sql: string;
  Statement: IZPreparedStatement;
  ResultSet: IZResultSet;
  Metadata: IZDatabaseMetadata;
begin
  if StartsWith(Protocol, 'mysql') then
    Exit;

  Metadata := Connection.GetMetadata;
  if Metadata.GetDatabaseInfo.SupportsMixedCaseIdentifiers then
    Exit;

  Sql := 'DELETE FROM "Case_Sensitive" where cs_id = ' + IntToStr(TEST_ROW_ID);
  Connection.CreateStatement.ExecuteUpdate(Sql);
  Sql := 'DELETE FROM case_sensitive where cs_id = ' + IntToStr(TEST_ROW_ID);
  Connection.CreateStatement.ExecuteUpdate(Sql);

  Sql := 'SELECT * FROM "Case_Sensitive" WHERE cs_id = ?';

  { Inserts row to "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  Statement.SetInt(1, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateInt(1, TEST_ROW_ID);
    UpdateInt(2, 10);
    UpdateInt(3, 11);
    UpdateNull(4);
    InsertRow;
  end;
  ResultSet := nil;
  Statement := nil;

  { Checks inserted row to "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(1, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('cs_id'));
    CheckEquals(10, GetIntByName('Cs_Data1'));
    CheckEquals(11, GetIntByName('cs_data1'));
    CheckEquals(True, IsNullByName('cs data1'));

    UpdateInt(2, 101);
    UpdateNullByName('cs_data1');
    UpdateIntByName('cs data1', 12);
    ResultSet.UpdateRow;
    Close;
  end;
  ResultSet := nil;
  Statement := nil;

  { Checks updated row from "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(1, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('cs_id'));
    CheckEquals(101, GetIntByName('Cs_Data1'));
    CheckEquals(True, IsNullByName('cs_data1'));
    CheckEquals(12, GetIntByName('cs data1'));
    DeleteRow;
    Close;
  end;
  ResultSet := nil;
  Statement := nil;

  { Deletes inserted,updated row in "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(1, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    CheckEquals(False, Next);
  end;
  ResultSet := nil;
  Statement := nil;

  Sql := 'SELECT * FROM case_sensitive WHERE cs_id = ?';

  { Inserts row to "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  Statement.SetInt(1, TEST_ROW_ID);
  CheckNotNull(Statement);
  ResultSet := Statement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateInt(1, TEST_ROW_ID);
    UpdateNull(2);
    UpdateInt(3, 21);
    UpdateInt(4, 22);
    InsertRow;
  end;
  ResultSet := nil;
  Statement := nil;

  { Checks inserted row to "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(1, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('cs_id'));
    CheckEquals(True, IsNullByName('CS_DATA1'));
    CheckEquals(21, GetIntByName('CS_Data2'));
    CheckEquals(22, GetIntByName('Cs_Data3'), 0);

    UpdateInt(2, 20);
    UpdateIntByName('CS_Data2', 212);
    UpdateNullByName('Cs_Data3');
    ResultSet.UpdateRow;
    Close;
  end;
  ResultSet := nil;
  Statement := nil;

  { Checks updated row from "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(1, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('cs_id'));
    CheckEquals(20, GetIntByName('CS_DATA1'));
    CheckEquals(212, GetIntByName('CS_Data2'));
    CheckEquals(True, IsNullByName('Cs_Data3'));
    DeleteRow;
    Close;
  end;
  ResultSet := nil;
  Statement := nil;

  { Deletes inserted,updated row in "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(1, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    CheckEquals(False, Next);
  end;
  ResultSet := nil;
  Statement := nil;
end;

{**
  Tests the DBC connection.
}
procedure TZGenericTestDbcResultSet.TestConnection;
begin
  CheckEquals(True, Connection.IsReadOnly);
//  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  Connection.SetAutoCommit(False);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

  { Checks without transactions. }
  CheckNotNull(Connection.CreateStatement);
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiReadCommitted);
  CheckNotNull(Connection.CreateStatement);
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

{**
  Checks functionality prepared statement
}
procedure TZGenericTestDbcResultSet.TestPreparedStatement;
var
  Sql: string;
  Statement: IZPreparedStatement;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TStream;
  ResultSet: IZResultSet;
begin
  Sql := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
  Connection.CreateStatement.ExecuteUpdate(Sql);
  Sql := 'DELETE FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
  Connection.CreateStatement.ExecuteUpdate(Sql);

  { The test for equipment table }

  { Creates prepared statement for equipment table }
  Statement := Connection.PrepareStatement(
    'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
    + ' woff_date) VALUES(?,?,?,?,?,?)');
  CheckNotNull(Statement);
  with Statement do
  begin
    SetInt(1, TEST_ROW_ID);
    SetString(2, 'xyz');
    SetInt(3, 7);
    SetDouble(4, 1234.567);
    SetDate(5, EncodeDate(1999, 8, 5));
    SetNull(6, stDate);
    CheckEquals(False, ExecutePrepared);
    CheckEquals(1, GetUpdateCount);
  end;
  Statement := nil;

  { Checks inserted row from equipment table }
  Statement := Connection.PrepareStatement(
    'SELECT * FROM equipment WHERE eq_id = ?');
  CheckNotNull(Statement);
  Statement.SetInt(1, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals('xyz', GetStringByName('eq_name'));
    CheckEquals(7, GetIntByName('eq_type'));
    CheckEquals(1234.567, GetDoubleByName('eq_cost'), 0.001);
    CheckEquals(EncodeDate(1999, 8, 5), GetDateByName('eq_date'));
    CheckEquals(True, IsNullByName('woff_date'));
    Close;
  end;
  ResultSet := nil;

  { Updates inserted row from equipment table }
  Statement := Connection.PrepareStatement(
    'UPDATE equipment SET eq_name = ? WHERE eq_id = ?' );
  CheckNotNull(Statement);
  with Statement do
  begin
    SetString(1, 'xyz1');
    SetInt(2, TEST_ROW_ID);
    CheckEquals(1, ExecuteUpdatePrepared);
  end;
  Statement := nil;

  { Deletes inserted row from equipment table }
  Statement := Connection.PrepareStatement(
    'DELETE FROM equipment WHERE eq_id = ?');
  CheckNotNull(Statement);
  with Statement do
  begin
    SetInt(1, TEST_ROW_ID);
    CheckEquals(False, ExecutePrepared);
    CheckEquals(1, GetUpdateCount);
  end;
  Statement := nil;

  { The test for people table }

  { Creates prepared statement for people table }
  Statement := Connection.PrepareStatement(
    'INSERT INTO people (p_id, p_dep_id, p_name, p_begin_work, p_end_work,'
    + ' p_picture, p_resume, p_redundant) VALUES(?,?,?,?,?,?,?,?)');
  CheckNotNull(Statement);
  { Sets prepared statement parameters values. }
  with Statement do
  begin
    SetInt(1, TEST_ROW_ID);
    SetInt(2, 2);
    SetString(3, 'xyz');
    SetTime(4, EncodeTime(8, 0, 0, 0));
    SetTime(5, EncodeTime(17, 30, 0, 0));

    BinStream := TMemoryStream.Create;
    BinStream.LoadFromFile('../../../database/images/dogs.jpg');
    BinStream.Size := 1024;
    SetBinaryStream(6, BinStream);

    StrStream := TMemoryStream.Create;
    StrStream.LoadFromFile('../../../database/text/lgpl.txt');
    StrStream.Size := 1024;
    SetAsciiStream(7, StrStream);

    SetNull(8, stString);
    CheckEquals(False, ExecutePrepared);
    CheckEquals(1, GetUpdateCount);
  end;
  Statement := nil;

  { Checks inserted row. }
  Statement := Connection.PrepareStatement(
    'SELECT * FROM people WHERE p_id = ?');
  CheckNotNull(Statement);
  Statement.SetInt(1, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('p_id'));
    CheckEquals(2, GetIntByName('p_dep_id'));
    CheckEquals('xyz', GetStringByName('p_name'));
    CheckEquals(EncodeTime(8, 0, 0, 0), GetTimeByName('p_begin_work'), 0.0001);
    CheckEquals(EncodeTime(17, 30, 0, 0), GetTimeByName('p_end_work'), 0.0001);
    CheckEquals(False, IsNullByName('p_picture'));
    CheckEquals(False, IsNullByName('p_resume'));
    CheckEquals(0, GetIntByName('p_redundant'));

    { Compares aciistream }
    StrStream1 := GetAsciiStreamByName('p_resume');
    CheckEquals(StrStream, StrStream1, 'Ascii Stream');
    StrStream.Free;
    StrStream1.Free;

    { Compares BinaryStream }
    BinStream1 := GetBinaryStreamByName('p_picture');
    CheckEquals(BinStream, BinStream1, 'Binary Stream');
    BinStream.Free;
    BinStream1.Free;
    Close;
  end;
  ResultSet := nil;


  { Deletes the row. }
  Statement := Connection.PrepareStatement(
    'DELETE FROM people WHERE p_id=?');
  CheckNotNull(Statement);
  with Statement do
  begin
    SetInt(1, TEST_ROW_ID);
    CheckEquals(False, ExecutePrepared);
    CheckEquals(1, GetUpdateCount);
  end;
  Statement := nil;
end;


{**
  Checks functionality execute statement
}
procedure TZGenericTestDbcResultSet.TestStatement;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
//!! Oracle does not support such queries in ExecuteUpdate
//  Statement.ExecuteUpdate('SELECT * FROM equipment');

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));

  Statement.ExecuteUpdate('DELETE FROM department where dep_id = ' +
    IntToStr(TEST_ROW_ID));

  { Inserts row to department table }
  Statement.Execute('INSERT INTO department VALUES (' +
    IntToStr(TEST_ROW_ID) + ',''Some agency'',''ENG'',''Some city'')');
  CheckEquals(1, Statement.GetUpdateCount);

  { Checks what row inserted }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department where dep_id = ' +
    IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  CheckEquals(True, ResultSet.Next);
  CheckEquals(False, ResultSet.Next);
  ResultSet.Close;
  ResultSet := nil;

  { Updates row in department table }
  Statement.ExecuteUpdate(
   'UPDATE department SET dep_name=NULL, dep_shname=NULL, dep_address=NULL WHERE dep_id = ' +
   IntToStr(TEST_ROW_ID));
  { Checks what row updated }
  CheckEquals(1, Statement.GetUpdateCount);

  { Deletes value from department table }
  Statement.ExecuteUpdate('DELETE FROM department where dep_id = ' +
    IntToStr(TEST_ROW_ID));
  CheckEquals(1, Statement.GetUpdateCount);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department where dep_id = ' +
    IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  CheckEquals(False, ResultSet.Next);
  ResultSet.Close;
  ResultSet := nil;

  Statement.Close;
  Statement := nil;
end;

{**
  Checks the functionality ResultSet
}
procedure TZGenericTestDbcResultSet.TestStoredResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  { Creates resultset for equipment table }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment where eq_id > 100');
  CheckNotNull(ResultSet);
  CheckEquals(False, ResultSet.Next);
  ResultSet.Close;

  { Creates resultset for equipment table}
  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment where eq_id = 1');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(1, GetIntByName('eq_id'));
    CheckEquals('Volvo', GetStringByName('eq_name'));
    CheckEquals(1, GetIntByName('eq_type'));
    CheckEquals(15000, GetFloatByName('eq_cost'));
    CheckEquals(EncodeDate(1998, 03, 04), GetDateByName('eq_date'));
    Check(IsNullByName('woff_date'));
    Close;
  end;
  ResultSet := nil;

  { Creates resultset for people table}
  ResultSet := Statement.ExecuteQuery('SELECT * FROM people where p_id <= 2');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(1, GetIntByName('p_id'));
    CheckEquals(1, GetIntByName('p_dep_id'));
    CheckEquals('Vasia Pupkin', GetStringByName('p_name'));
    CheckEquals(EncodeTime(9, 0, 0, 0),
      Frac(Abs(GetTimeByName('p_begin_work'))), 0.0001);
    CheckEquals(EncodeTime(18, 0, 0, 0),
      Frac(Abs(GetTimeByName('p_end_work'))), 0.0001);
    Check(IsNullByName('p_picture'));
    Check(IsNullByName('p_resume'));
    CheckEquals(0, GetIntByName('p_redundant'));

    Check(Next);
    CheckEquals(2, GetIntByName('p_id'));
    CheckEquals(2, GetIntByName('p_dep_id'));
    CheckEquals('Andy Karto', GetStringByName('p_name'));
    CheckEquals(EncodeTime(8, 30, 0, 0),
      Frac(Abs(GetTimeByName('p_begin_work'))), 0.0001);
    CheckEquals(EncodeTime(17, 30, 0, 0),
      Frac(Abs(GetTimeByName('p_end_work'))), 0.0001);
    Check(IsNullByName('p_picture'));
    Check(IsNullByName('p_resume'));
    CheckEquals(0, GetIntByName('p_redundant'));
    Close;
  end;
  ResultSet := nil;

  { Creates resultset for cargo table}
  ResultSet := Statement.ExecuteQuery('SELECT * FROM cargo where c_id = 2');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(2, GetIntByName('c_id'));
    CheckEquals(1, GetIntByName('c_dep_id'));
    CheckEquals('Paper', Trim(GetStringByName('c_name')));
    CheckEquals(2, GetIntByName('c_seal'));
    CheckEquals(EncodeDate(2002, 12, 19) + EncodeTime(14, 0, 0, 0),
      GetTimestampByName('c_date_came'), 0.0001);
    CheckEquals(EncodeDate(2002, 12, 23) + EncodeTime(0, 0, 0, 0),
      GetTimestampByName('c_date_out'), 0.0001);
    CheckEquals(1000, GetFloatByName('c_weight'));
    CheckEquals(10, GetIntByName('c_width'));
    CheckEquals(10, GetIntByName('c_height'));
    CheckEquals(986.47, GetFloatByName('c_cost'), 0.01);
    //CheckEquals('#14#17#Сорт2', GetStringByName('c_attributes'));
    Close;
  end;
  ResultSet := nil;

  { Creates resultset for equipment table }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    Check(Next);
    Check(Next);
    Check(Next);
    CheckEquals(False, Next);
    Close;
  end;
  ResultSet := nil;

  { Creates resultset for equipment table with limit rows}
  Statement.SetMaxRows(2);
  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    Check(Next);
    CheckEquals(False, Next);
    Close;
  end;
  ResultSet := nil;

  Statement.Close;
end;

procedure TZGenericTestDbcResultSet.TestStoredResultSetUpdate;
var
  Sql: string;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TStream;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Sql := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
  Connection.CreateStatement.ExecuteUpdate(Sql);
  Sql := 'DELETE FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
  Connection.CreateStatement.ExecuteUpdate(Sql);

  { Tests the equipment table }
  Sql := 'SELECT * FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
  { Inserts test record to equipment }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '1. ' + Sql);
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateIntByName('eq_id', TEST_ROW_ID);
    UpdateNullByName('eq_name');
    UpdateNullByName('eq_type');
    UpdateNullByName('eq_cost');
    UpdateNullByName('eq_date');
    UpdateNullByName('woff_date');
    InsertRow;
    Close;
  end;
  ResultSet := nil;

  { Updates row for equipment}
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '2. ' + Sql);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('eq_id'));
    CheckEquals(True, IsNullByName('eq_name'));
    CheckEquals(True, IsNullByName('eq_type'));
    CheckEquals(True, IsNullByName('eq_cost'));
    CheckEquals(True, IsNullByName('eq_date'));
    CheckEquals(True, IsNullByName('woff_date'));

    UpdateStringByName('eq_name', 'The some thing');
    UpdateIntByName('eq_type', 1);
    UpdateDoubleByName('eq_cost', 12345.678);
    UpdateDateByName('eq_date', EncodeDate(1989, 07, 07));
    UpdateDateByName('woff_date', EncodeDate(1998, 04, 24));
    UpdateRow;
    Close;
  end;

  { Checks previous updated row}
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '3. ' + Sql);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals('The some thing', GetStringByName('eq_name'));
    CheckEquals(1, GetIntByName('eq_type'));
    CheckEquals(12345.678, GetFloatByName('eq_cost'), 0.01);
    CheckEquals(EncodeDate(1989, 07, 07), GetDateByName('eq_date'));
    CheckEquals(EncodeDate(1998, 04, 24), GetDateByName('woff_date'));
    DeleteRow;
    Close;
  end;

  { Checks what record deleted }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '4. ' + Sql);
  CheckEquals(False, ResultSet.Next);


  { Tests the people table }
  Sql := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
  Statement.ExecuteUpdate(Sql);

  Sql := 'SELECT * FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
  StrStream := TMemoryStream.Create;
  StrStream.LoadFromFile('../../../database/text/lgpl.txt');
  StrStream.Size := 1024;
  BinStream := TMemoryStream.Create;
  BinStream.LoadFromFile('../../../database/images/dogs.jpg');
  BinStream.Size := 1024;

  { Inserts test record to people table }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '1. ' + Sql);
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateIntByName('p_id', TEST_ROW_ID);
    UpdateNullByName('p_dep_id');
    UpdateNullByName('p_name');
    UpdateNullByName('p_begin_work');
    UpdateNullByName('p_end_work');
    UpdateNullByName('p_resume');
    UpdateNullByName('p_picture');
    UpdateNullByName('p_redundant');
    InsertRow;
    Close;
  end;

   { Checks the previous inserted record }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '2. ' + Sql);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('p_id'));
    CheckEquals(True, IsNullByName('p_dep_id'));
    CheckEquals(True, IsNullByName('p_name'));
    CheckEquals(True, IsNullByName('p_begin_work'));
    CheckEquals(True, IsNullByName('p_end_work'));
    CheckEquals(True, IsNullByName('p_resume'));
    CheckEquals(True, IsNullByName('p_picture'));
    CheckEquals(True, IsNullByName('p_redundant'));

    CheckEquals(TEST_ROW_ID, GetIntByName('p_id'));
    CheckEquals(True, IsNullByName('p_dep_id'));
    CheckEquals(True, IsNullByName('p_name'));
    CheckEquals(True, IsNullByName('p_begin_work'));
    CheckEquals(True, IsNullByName('p_end_work'));
    CheckEquals(True, IsNullByName('p_resume'));
    CheckEquals(True, IsNullByName('p_picture'));
    CheckEquals(True, IsNullByName('p_redundant'));
    Close;
  end;

  { Creates and update resultset for people table for p_id = TEST_ROW_ID }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '3. ' + Sql);
  with ResultSet do
  begin
    Check(Next);
    UpdateIntByName('p_dep_id', 1);
    UpdateStringByName('p_name', 'Somebody');
    UpdateTimeByName('p_begin_work', EncodeTime(12, 11, 20, 0));
    UpdateTimeByName('p_end_work', EncodeTime(22, 36, 55, 0));
    UpdateAsciiStreamByName('p_resume', StrStream);
    UpdateBinaryStreamByName('p_picture', BinStream);
    UpdateIntByName('p_redundant', 1);
    UpdateRow;
    Close;
  end;

  { Creates and updates resultset for people table for p_id = TEST_ROW_ID }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '4. ' + Sql);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(1, GetIntByName('p_dep_id'));
    CheckEquals('Somebody', GetStringByName('p_name'));
    CheckEquals(EncodeTime(12, 11, 20, 0), GetTimeByName('p_begin_work'), 0.0001);
    CheckEquals(EncodeTime(22, 36, 55, 0), GetTimeByName('p_end_work'), 0.0001);
    BinStream1 := GetBinaryStreamByName('p_picture');
    StrStream1 := GetAsciiStreamByName('p_resume');
    CheckEquals(BinStream, BinStream1);
    CheckEquals(StrStream, StrStream1);
    CheckEquals(1, GetIntByName('p_redundant'));
    DeleteRow;
  end;

  BinStream.Free;
  BinStream1.Free;
  StrStream.Free;
  StrStream1.Free;

  { Creates and updates resultset for equipment table for eq_id = TEST_ROW_ID }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '5. ' + Sql);
  CheckEquals(False, ResultSet.Next);
end;

{**
  Tests memory issues in Execute and GetLastQuery.
}
procedure TZGenericTestDbcResultSet.TestLastQuery;
var
  SQL: string;
  Statement: IZPreparedStatement;
  Statement1: IZStatement;
  ResultSet: IZResultSet;
begin
  SQL := 'UPDATE people SET p_id=p_id WHERE 1=0';
  Statement := Connection.PrepareStatement(SQL);
  try
    CheckNotNull(Statement);
    Check(not Statement.ExecutePrepared);
  finally
    Statement.Close;
  end;

  SQL := 'SELECT * FROM people';

  Statement1 := Connection.CreateStatement;
  try
    CheckNotNull(Statement1);
    Statement1.SetResultSetType(rtScrollInsensitive);
    Statement1.SetResultSetConcurrency(rcUpdatable);
    Check(Statement1.Execute(SQL));

    ResultSet := Statement1.GetResultSet;
    try
      ResultSet.BeforeFirst;
      ResultSet := nil;
      Statement1.GetConnection;

      ResultSet := Statement1.GetResultSet;
      ResultSet.BeforeFirst;
    finally
      ResultSet.Close;
    end;
  finally
    Statement1.Close;
  end;

  Statement := Connection.PrepareStatement(SQL);
  try
    CheckNotNull(Statement);
    Statement.SetResultSetType(rtScrollInsensitive);
    Statement.SetResultSetConcurrency(rcUpdatable);
    Check(Statement.ExecutePrepared);

    ResultSet := Statement.GetResultSet;
    try
      ResultSet.BeforeFirst;
      ResultSet := nil;
      Statement.GetConnection;

      ResultSet := Statement.GetResultSet;
      ResultSet.BeforeFirst;
    finally
      ResultSet.Close;
    end;
  finally
    Statement.Close;
  end;
end;

initialization
  RegisterTest('dbc',TZGenericTestDbcResultSet.Suite);
end.

