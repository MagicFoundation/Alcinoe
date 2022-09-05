{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Case for Midas (DataSnap) components         }
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

unit ZTestMidas;

{$WARN SYMBOL_PLATFORM OFF}

interface

{$I ZComponent.inc}

uses
  Classes, SysUtils, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZConnection, ZDataset, DBClient,
  ComServ, ZMidasTestCase;

type

  {** Implements a test case for Midas (DataSnap). }
  TZMidasTestCase = class(TZMidasPortableSQLTestCase)
  published
    procedure TestQueryGeneric;
    procedure TestExecSql;
    procedure TestPreparedStatement;
    procedure TestUpdate;
    procedure TestMasterDetail;
    procedure TestIProviderSupport;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZTestConsts, ZSqlTestCase, ZTestCase;

{ TZMidasTestCase }

{**
  Runs a test case for generic SQL functionality.
}
procedure TZMidasTestCase.TestQueryGeneric;
begin
  { delete temp equipment table data }
  DataSet.CommandText := 'DELETE FROM equipment where eq_id > 100';;
  DataSet.Execute;

  { select equipment table }
  DataSet.CommandText := 'SELECT * FROM equipment where eq_id > 100';
  with DataSet do
  begin
    Open;
    CheckEquals(True, IsEmpty);
    Close;
  end;

  { select equipment table}
  DataSet.CommandText := 'SELECT * FROM equipment where eq_id = 1';
  with DataSet do
  begin
    Open;
    CheckEquals(1,     RecordCount);
    CheckEquals(False, IsEmpty);
    CheckEquals(1,       FieldByName('eq_id').AsInteger);
    CheckEquals('Volvo', FieldByName('eq_name').AsString);
    CheckEquals(1,       FieldByName('eq_type').AsInteger);
    CheckEquals(15000,   FieldByName('eq_cost').AsFloat);
    CheckEquals(EncodeDate(1998, 03, 04), FieldByName('eq_date').AsDateTime);
    CheckEquals(True,   FieldByName('woff_date').IsNull);
    Close;
  end;

  { select from people table two records }
  DataSet.CommandText := 'SELECT * FROM people where p_id <= 2';
  with DataSet do
  begin
    Open;
    CheckEquals(False, IsEmpty);
    CheckEquals(2, RecordCount);

    CheckEquals(1, RecNo);
    CheckEquals(True, Bof);
    CheckEquals(False, Eof);
    CheckEquals(1, FieldByName('p_id').AsInteger);
    CheckEquals(1, FieldByName('p_dep_id').AsInteger);
    CheckEquals('Vasia Pupkin', FieldByName('p_name').AsString);
    CheckEquals(EncodeTime(9, 0, 0, 0),
      Frac(Abs(FieldByName('p_begin_work').AsDateTime)), 0.0001);
    CheckEquals(EncodeTime(18, 0, 0, 0),
      Frac(Abs(FieldByName('p_end_work').AsDateTime)), 0.0001);
    CheckEquals(True, FieldByName('p_picture').IsNull);
    CheckEquals(True, FieldByName('p_resume').IsNull);
    CheckEquals(0, FieldByName('p_redundant').AsInteger);

    Next;
    Next; // Fix
    CheckEquals(2, RecNo);
    CheckEquals(False, Bof);
    CheckEquals(True, Eof);
    CheckEquals(2, FieldByName('p_id').AsInteger);
    CheckEquals(2, FieldByName('p_dep_id').AsInteger);
    CheckEquals('Andy Karto', FieldByName('p_name').AsString);
    CheckEquals(EncodeTime(8, 30, 0, 0),
      Frac(Abs(FieldByName('p_begin_work').AsDateTime)), 0.0001);
    CheckEquals(EncodeTime(17, 30, 0, 0),
      Frac(Abs(FieldByName('p_end_work').AsDateTime)), 0.0001);
    Check(FieldByName('p_picture').IsNull);
    Check(FieldByName('p_resume').IsNull);
    CheckEquals(0, FieldByName('p_redundant').AsInteger);
    Close;
  end;

  { select cargo table}
  DataSet.CommandText := 'SELECT * FROM cargo where c_id = 2';
  with DataSet do
  begin
    Open;
    CheckEquals(1, RecordCount);
    CheckEquals(False, IsEmpty);
    CheckEquals(2, FieldByName('c_id').AsInteger);
    CheckEquals(1, FieldByName('c_dep_id').AsInteger);
    CheckEquals('Paper', Trim(FieldByName('c_name').AsString));
    CheckEquals(2, FieldByName('c_seal').AsInteger);
    CheckEquals(EncodeDate(2002, 12, 19) + EncodeTime(14, 0, 0, 0),
      FieldByName('c_date_came').AsDateTime, 0.001);
    CheckEquals(EncodeDate(2002, 12, 23) + EncodeTime(0, 0, 0, 0),
      FieldByName('c_date_out').AsDateTime, 0.001);
    CheckEquals(1000, FieldByName('c_weight').AsFloat);
    CheckEquals(10, FieldByName('c_width').AsInteger);
    CheckEquals(10, FieldByName('c_height').AsInteger);
    CheckEquals(986.47, FieldByName('c_cost').AsFloat, 0.001);
    //CheckEquals('#14#17#Tþ¨ª2', FieldByName('c_attributes').AsString);
    Close;
  end;

  { select cargo table date parameter checking}
  DataSet.CommandText := 'SELECT * FROM cargo where c_date_came = :DateCame';
  with DataSet do
  begin
    Params.ParamByName('DateCame').AsDateTime := EncodeDate(2002, 12, 20) + EncodeTime(2, 0, 0, 0);
    Open;
    CheckEquals(1, RecordCount);
    CheckEquals(EncodeDate(2002, 12, 20) + EncodeTime(2, 0, 0, 0),
      FieldByName('c_date_out').AsDateTime, 0.001);
    Close;
  end;

  { select equipment table }
  DataSet.CommandText := 'SELECT * FROM equipment';
  with DataSet do
  begin
    Open;
    CheckEquals(4, RecordCount);
    CheckEquals(False, IsEmpty);
    CheckEquals(False, IsEmpty);

    CheckEquals(True,  Bof);
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, Eof);
    Next;
    CheckEquals(True, Eof);

    First;
    CheckEquals(True, Bof);
    Last;
    CheckEquals(True, Eof);

    Close;
  end;
end;

{**
  Runs a test for executing SQL queries.
}
procedure TZMidasTestCase.TestExecSql;
begin
 with DataSet do
 begin
   CommandText := 'UPDATE equipment SET eq_name=eq_name';
   Execute;
   CommandText := 'SELECT * FROM equipment';
   Execute;

//  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
//  Check(Statement.Execute('SELECT * FROM equipment'));

   CommandText := 'DELETE FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
   Execute;

   { insert row to department table }
   CommandText := 'INSERT INTO department VALUES (' +
     IntToStr(TEST_ROW_ID) + ',''Some agency'',''ENG'',''Some city'')';
   Execute;
//   CheckEquals(1, RowsAffected);

   { check what row inserted }
   CommandText := 'SELECT * FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
   Open;
   CheckEquals(1, RecordCount);
   Close;

   { update row in department table }
   CommandText := 'UPDATE department SET dep_name=NULL, dep_shname=NULL, dep_address=NULL WHERE dep_id = ' +
   IntToStr(TEST_ROW_ID);
   Execute;
//   CheckEquals(1, RowsAffected);
   Close;

   { delete value from department table }
   CommandText := 'DELETE FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
   Execute;
//   CheckEquals(1, RowsAffected);
   Close;

   { check what row deleted }
   CommandText := 'SELECT * FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
   Open;
   CheckEquals(True, IsEmpty);
   Close;
 end;
end;

{**
  Runs a test of IProviderSupport, used by MIDAS before BDS4
}

procedure TZMidasTestCase.TestIProviderSupport;
const
  TestSql = 'SELECT * FROM department';
var
  Con: TZConnection;
  Q: TZQuery;
  ps: IProviderSupport;
begin
  Con := CreateDatasetConnection;
  try
    Q := TZQuery.Create(Con);
    Q.Connection := Con;
    Check(Supports(Q, IProviderSupport, ps), 'IProviderSupport not supported');
    ps.PSSetCommandText(TestSql);
    CheckEqualsString(TestSql, Trim(Q.SQL.Text), 'PSSetCommandText wrong');
    CheckEqualsString('department', ps.PSGetTableName, 'PSGetTableName wrong');
    ps := nil;
  finally
    Con.Free;
  end;
end;

{**
  Runs a test for master-detail relationships.
}

procedure TZMidasTestCase.TestMasterDetail;
(*
var
  Detail: TDataset;
*)
begin
(*
  DataSet.Close;
  RemoteDM.MasterDetail(0);
  DataSet.Open;

  with DataSet do
  begin
    First;
    CheckEquals(1, FieldByName('dep_id').AsInteger);
    CheckEquals(True, Assigned(FieldByName('DetailQuery')));
    Detail := FieldByName('DetailQuery').DataSet;
    CheckEquals(True, Assigned(Detail));
    CheckEquals(2, Detail.RecordCount);

    Detail.First;
    CheckEquals(2, Detail.FieldByName('c_dep_id').AsInteger);
    CheckEquals('Paper', Trim(Detail.FieldByName('c_name').AsString));
    CheckEquals(2, Detail.FieldByName('c_seal').AsInteger);
    CheckEquals(EncodeDate(2002, 12, 19) + EncodeTime(14, 0, 0, 0),
      Detail.FieldByName('c_date_came').AsDateTime, 0.001);
    CheckEquals(EncodeDate(2002, 12, 23) + EncodeTime(0, 0, 0, 0),
      Detail.FieldByName('c_date_out').AsDateTime, 0.001);
    CheckEquals(1000, Detail.FieldByName('c_weight').AsInteger);
    CheckEquals(10, Detail.FieldByName('c_width').AsInteger);
    CheckEquals(10, Detail.FieldByName('c_height').AsInteger);
    CheckEquals(986.4700, Detail.FieldByName('c_cost').AsFloat);
    Detail.Next;
    CheckEquals(3, Detail.FieldByName('c_dep_id').AsInteger);
    CheckEquals('Wool', Detail.FieldByName('c_name').AsString);
    CheckEquals(0, Detail.FieldByName('c_seal').AsInteger);
    CheckEquals(EncodeDate(2002, 12,20) + EncodeTime(2, 0, 0, 0),
      Detail.FieldByName('c_date_came').AsDateTime, 0.001);
    CheckEquals(True, Detail.FieldByName('c_date_out').IsNull);
    CheckEquals(400, Detail.FieldByName('c_weight').AsInteger);
    CheckEquals(7, Detail.FieldByName('c_width').AsInteger);
    CheckEquals(4, Detail.FieldByName('c_height').AsInteger);
    CheckEquals(643.1100, Detail.FieldByName('c_cost').AsFloat);

    Next;
    Detail := FieldByName('DetailQuery').DataSet;
    CheckEquals(2, Detail.RecordCount);

    Detail.First;
    CheckEquals(2, Detail.FieldByName('c_dep_id').AsInteger);
    CheckEquals('Grain', Detail.FieldByName('c_name').AsString);
    CheckEquals(1, Detail.FieldByName('c_seal').AsInteger);
    CheckEquals(EncodeDate(2002, 12,20) + EncodeTime(2, 0, 0, 0),
      Detail.FieldByName('c_date_came').AsDateTime, 0.001);
    CheckEquals(EncodeDate(2002, 12,20) + EncodeTime(2, 0, 0, 0),
      Detail.FieldByName('c_date_out').AsDateTime, 0.001);
    CheckEquals(5000, Detail.FieldByName('c_weight').AsInteger);
    CheckEquals(True, Detail.FieldByName('c_width').IsNull);
    CheckEquals(True, Detail.FieldByName('c_height').IsNull);
    CheckEquals(1769.4301, Detail.FieldByName('c_cost').AsFloat);

    Next;
    CheckEquals(0, FieldByName('DetailQuery').DataSet.RecordCount);
  end;
*)
end;

{**
  Runs a test for updating SQL data.
}
procedure TZMidasTestCase.TestUpdate;
var
  Sql_: string;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TMemoryStream;
begin
//  CheckEquals(Ord(rtScrollInsensitive), Ord(Query.DbcStatement.GetResultSetType));
//  CheckEquals(Ord(rcUpdatable), Ord(Query.DbcStatement.GetResultSetConcurrency));

  DataSet.CommandText := 'DELETE FROM people where p_id = '
    + IntToStr(TEST_ROW_ID);
  DataSet.Execute;

  DataSet.CommandText := 'DELETE FROM equipment where eq_id = '
    + IntToStr(TEST_ROW_ID);
  DataSet.Execute;

  { The test for equipment table }

  with DataSet do
  begin
    { insert test record to equipment }
    Sql_ := 'SELECT * FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
    CommandText := Sql_;
    Open;
    CheckEquals(True, IsEmpty);

    Append;
    CheckEquals(False, Modified);
    CheckEquals(Ord(dsInsert), Ord(State));
    FieldByName('eq_id').AsInteger := TEST_ROW_ID;
    FieldByName('eq_name').Value := Null;
    FieldByName('eq_type').Value := Null;
    FieldByName('eq_cost').Value := Null;
    FieldByName('eq_date').Value := Null;
    FieldByName('woff_date').Value := Null;
    CheckEquals(True, Modified);
    Post;
    ApplyUpdates(-1);
    CheckEquals(False, Modified);
    Close;

    { update row for equipment}
    CommandText := Sql_;
    Open;
    CheckEquals(False, IsEmpty);
    CheckEquals(True, Bof);

    Edit;
    CheckEquals(Ord(dsEdit), Ord(State));
    FieldByName('eq_name').AsString := 'The some thing';
    FieldByName('eq_type').AsInteger := 1;
    FieldByName('eq_cost').AsFloat := 12345.678;
    FieldByName('eq_date').AsDateTime := EncodeDate(1989, 07, 07);
    FieldByName('woff_date').AsDateTime := EncodeDate(1998, 04, 24);
    CheckEquals(True, Modified);
    Post;
    ApplyUpdates(-1);
    CheckEquals(False, Modified);
    Close;

    { check previous updated row}
    CommandText := Sql_;
    Open;
    CheckEquals(False, IsEmpty);

    CheckEquals(True, Bof);
    CheckEquals(Ord(dsBrowse), Ord(State));
    CheckEquals('The some thing', FieldByName('eq_name').AsString);
    CheckEquals(1, FieldByName('eq_type').AsInteger);
    CheckEquals(12345.678, FieldByName('eq_cost').AsFloat, 0.01);
    CheckEquals(EncodeDate(1989, 07, 07), FieldByName('eq_date').AsDateTime);
    CheckEquals(EncodeDate(1998, 04, 24), FieldByName('woff_date').AsDateTime);
    Delete;
    ApplyUpdates(-1);
    Close;

    { check what record deleted }
    CommandText := Sql_;
    Open;
    CheckEquals(True, IsEmpty);
  end;

    {
      The test for people table
    }
  with DataSet do
  begin
    Sql_ := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
    CommandText := Sql_;
    Execute;

    Sql_ := 'SELECT * FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
    StrStream := TMemoryStream.Create();
    StrStream.LoadFromFile('../../../database/text/lgpl.txt');
    StrStream.Size := 1024;
    BinStream := TMemoryStream.Create();
    BinStream.LoadFromFile('../../../database/images/dogs.jpg');
    BinStream.Size := 1024;
    BinStream1 := TMemoryStream.Create;
    StrStream1 := TMemoryStream.Create;

    { insert test record to people table }
    Close;
    Fields.Clear;
    CommandText := Sql_;
    Open;
    CheckEquals(True, DataSet.IsEmpty);

    Append;
    CheckEquals(False, Modified);
    CheckEquals(Ord(dsInsert), Ord(State));
    FieldByName('p_id').AsInteger := TEST_ROW_ID;
    FieldByName('p_dep_id').Value := Null;
    FieldByName('p_name').Value := Null;
    FieldByName('p_begin_work').Value := Null;
    FieldByName('p_end_work').Value := Null;
    FieldByName('p_resume').Value := Null;
    FieldByName('p_picture').Value := Null;
    FieldByName('p_redundant').Value := Null;
    CheckEquals(True, Modified);
    Post;
    ApplyUpdates(-1);
    CheckEquals(False, Modified);
    Close;


    { check previous inserted record }
    CommandText := Sql_;
    Open;
    CheckEquals(False, IsEmpty);

    CheckEquals(True, Bof);
    CheckEquals(Ord(dsBrowse), Ord(State));
    CheckEquals(TEST_ROW_ID, FieldByName('p_id').AsInteger);
    CheckEquals(True, FieldByName('p_dep_id').IsNull);
    CheckEquals(True, FieldByName('p_name').IsNull);
    CheckEquals(True, FieldByName('p_begin_work').IsNull);
    CheckEquals(True, FieldByName('p_end_work').IsNull);
    CheckEquals(True, FieldByName('p_resume').IsNull);
    CheckEquals(True, FieldByName('p_picture').IsNull);
    CheckEquals(True, FieldByName('p_redundant').IsNull);
    CheckEquals(False, Modified);

    Edit;
    CheckEquals(False, Modified);
    CheckEquals(Ord(dsEdit), Ord(State));
    FieldByName('p_dep_id').AsInteger := 1;
    FieldByName('p_name').AsString := 'Somebody';
    FieldByName('p_begin_work').AsDateTime := EncodeTime(12, 11, 20, 0);
    FieldByName('p_end_work').AsDateTime := EncodeTime(22, 36, 55, 0);

    (FieldByName('p_resume') as TBlobField).LoadFromStream(StrStream);
    (FieldByName('p_picture') as TBlobField).LoadFromStream(BinStream);

    FieldByName('p_redundant').AsInteger := 1;
    CheckEquals(True, Modified);
    Post;
    ApplyUpdates(-1);
    CheckEquals(False, Modified);
    Close;

    { create and update resultset for people table for p_id = TEST_ROW_ID }
    CommandText := Sql_;
    Open;
    CheckEquals(False, IsEmpty);

    CheckEquals(1, FieldByName('p_dep_id').AsInteger);
    CheckEquals('Somebody', FieldByName('p_name').AsString);
    CheckEquals(EncodeTime(12, 11, 20, 0), FieldByName('p_begin_work').AsDateTime, 0.0001);
    CheckEquals(EncodeTime(22, 36, 55, 0), FieldByName('p_end_work').AsDateTime, 0.0001);

    (FieldByName('p_picture')as TBlobField).SaveToStream(BinStream1);
    (FieldByName('p_resume')as TBlobField).SaveToStream(StrStream1);

    CheckEquals(BinStream, BinStream1);
    CheckEquals(StrStream, StrStream1);
    CheckEquals(1, FieldByName('p_redundant').AsInteger);
    Delete;
    ApplyUpdates(-1);

    BinStream.Free;
    BinStream1.Free;
    StrStream.Free;
    StrStream1.Free;

    { create and update resultset for equipment table for eq_id = TEST_ROW_ID }
    CommandText := Sql_;
    Open;
    CheckEquals(True, IsEmpty);
    Close;
  end;
end;

procedure TZMidasTestCase.TestPreparedStatement;
var
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TMemoryStream;
begin
  with DataSet do
  begin
    CommandText := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
    Execute;
    CommandText := 'DELETE FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
    Execute;
  end;

  {
    The test for equipment table
  }
  with DataSet do
  begin
    { Create prepared statement for equipment table }
    CommandText := 'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
        + ' woff_date) VALUES(:q_id, :eq_name, :eq_type, :eq_cost, :eq_date, :woff_date)';
    CheckEquals(6, Params.Count);

    Params[0].DataType := ftInteger;
    Params[1].DataType := ftString;
    Params[2].DataType := ftSmallint;
    Params[3].DataType := ftFloat;
    Params[4].DataType := ftDate;
    Params[5].DataType := ftDate;

    Params[0].AsInteger := TEST_ROW_ID;
    Params[1].AsString := '\xyz\'#13;
    Params[2].AsInteger := 7;
    Params[3].AsFloat := 1234.567;
    Params[4].AsDateTime := EncodeDate(1999, 8, 5);
    Params[5].Value := Null;
    Execute;

//    CheckEquals(1, RowsAffected);

    { check inserted row from equipment table }
    CommandText := 'SELECT * FROM equipment WHERE eq_id = :eq_id';
    CheckEquals(1, DataSet.Params.Count);
    Params[0].DataType := ftInteger;
    Params[0].AsInteger := TEST_ROW_ID;

    Open;
    CheckEquals(1, RecordCount);
    CheckEquals(False, IsEmpty);
    CheckEquals(TEST_ROW_ID, FieldByName('eq_id').AsInteger);
    CheckEquals('\xyz\'#13, FieldByName('eq_name').AsString);
    CheckEquals(7, FieldByName('eq_type').AsInteger);
    CheckEquals(1234.567, FieldByName('eq_cost').AsFloat, 0.001);
    CheckEquals(EncodeDate(1999, 8, 5), FieldByName('eq_date').AsDateTime);
    CheckEquals(True, FieldByName('woff_date').IsNull);
    Close;

    { update inserted row from equipment table }
    CommandText := 'UPDATE equipment SET eq_name = :eq_name WHERE eq_id = :eq_id';
    CheckEquals(2, Params.Count);

    Params[0].DataType := ftString;
    Params[1].DataType := ftInteger;

    Params[0].AsString := 'xyz1';
    Params[1].AsInteger := TEST_ROW_ID;

    Execute;
//    CheckEquals(1, RowsAffected);

    { delete inserted row from equipment table }
    CommandText := 'DELETE FROM equipment WHERE eq_id = :eq_id';

    CheckEquals(1, Params.Count);
    Params[0].DataType := ftInteger;
    Params[0].AsInteger := TEST_ROW_ID;
    Execute;
//    CheckEquals(1, RowsAffected);
  end;

  { The test for people table }
  with DataSet do
  begin
    { Create prepared statement for people table }
    CommandText := 'INSERT INTO people (p_id, p_dep_id, p_name, p_begin_work, p_end_work,' +
        ' p_picture, p_resume, p_redundant) VALUES(:p_id, :p_dep_id, :p_name, ' +
        ' :p_begin_work, :p_end_work, :p_picture, :p_resume, :p_redundant)';
    { Sets prepared statement parameters values. }
    CheckEquals(8, Params.Count);

    Params[0].DataType := ftInteger;
    Params[1].DataType := ftSmallint;
    Params[2].DataType := ftString;
    Params[3].DataType := ftDateTime;
    Params[4].DataType := ftDateTime;
    Params[5].DataType := ftBlob;
    Params[6].DataType := ftMemo;
    Params[7].DataType := ftSmallint;

    Params[0].AsInteger := TEST_ROW_ID;
    Params[1].AsInteger := 2;
    Params[2].AsString := 'xyz';
    Params[3].AsDateTime := EncodeTime(8, 0, 0, 0);
    Params[4].AsDateTime := EncodeTime(17, 30, 0, 0);

    BinStream := TMemoryStream.Create;
    BinStream.LoadFromFile('../../../database/images/dogs.jpg');
    BinStream.Size := 1024;
    Params[5].LoadFromStream(BinStream, ftBlob);

    StrStream := TMemoryStream.Create;
    StrStream.LoadFromFile('../../../database/text/lgpl.txt');
    StrStream.Size := 1024;
    Params[6].LoadFromStream(StrStream, ftMemo);

    Params[7].Value := Null;
    Execute;
//    CheckEquals(1, RowsAffected);

    { Checks inserted row. }
    CommandText := 'SELECT * FROM people WHERE p_id = :p_id';
    CheckEquals(1, Params.Count);
    Params[0].DataType := ftInteger;
    Params[0].AsInteger := TEST_ROW_ID;

    Open;
    CheckEquals(TEST_ROW_ID, FieldByName('p_id').AsInteger);
    CheckEquals(False, FieldByName('p_id').IsNull);
    CheckEquals(2, FieldByName('p_dep_id').AsInteger);
    CheckEquals(False, FieldByName('p_dep_id').IsNull);
    CheckEquals('xyz', FieldByName('p_name').AsString);
    CheckEquals(False, FieldByName('p_name').IsNull);
    CheckEquals(EncodeTime(8, 0, 0, 0), FieldByName('p_begin_work').AsDateTime, 0.0001);
    CheckEquals(False, FieldByName('p_begin_work').IsNull);
    CheckEquals(EncodeTime(17, 30, 0, 0), FieldByName('p_end_work').AsDateTime, 0.0001);
    CheckEquals(False, FieldByName('p_end_work').IsNull);
    CheckEquals(False, FieldByName('p_picture').IsNull);
    CheckEquals(False, FieldByName('p_resume').IsNull);
    CheckEquals(0, FieldByName('p_redundant').AsInteger);
    CheckEquals(True, FieldByName('p_redundant').IsNull);

    { compare aciistream }
    StrStream1 := TMemoryStream.Create;
    (FieldByName('p_resume') as TBlobField).SaveToStream(StrStream1);
    CheckEquals(StrStream, StrStream1, 'Ascii Stream');
    StrStream.Free;
    StrStream1.Free;

    { compare BinaryStream }
    BinStream1 := TMemoryStream.Create;
    (FieldByName('p_picture') as TBlobField).SaveToStream(BinStream1);
    CheckEquals(BinStream, BinStream1, 'Binary Stream');
    BinStream.Free;
    BinStream1.Free;
    Close;

    { Delete the row. }
    CommandText := 'DELETE FROM people WHERE p_id = :p_id';
    CheckEquals(1, Params.Count);

    Params[0].DataType := ftInteger;
    Params[0].AsInteger := TEST_ROW_ID;

    Execute;
//    CheckEquals(1, RowsAffected);
  end;
end;

initialization
  RegisterTest('component',TZMidasTestCase.Suite);
end.
