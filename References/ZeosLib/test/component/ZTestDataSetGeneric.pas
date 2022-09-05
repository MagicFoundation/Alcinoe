{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Test Case for Database Connectivity Classes        }
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

unit ZTestDataSetGeneric;

interface
{$I ZComponent.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDataset, ZConnection, SysUtils, ZDbcIntfs,
  ZTestDefinitions, ZCompatibility;

type
  {** Implements a test case for . }
  TZGenericTestDbcResultSet = class(TZComponentPortableSQLTestCase)
  private
    FConnection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TestQueryGeneric(Query: TDataset);
    procedure TestFilterGeneric(Query: TDataset);
    property Connection: TZConnection read FConnection write FConnection;
  published
    procedure TestConnection;
    procedure TestReadOnlyQuery;
    procedure TestRealPrepReadOnlyQuery;
    procedure TestQuery;
    procedure TestReadOnlyQueryExecSql;
    procedure TestQueryExecSql;
    procedure TestQueryUpdate;
    procedure TestPreparedStatement;
    procedure TestParamChar;
    procedure TestReadOnlyQueryFilter;
    procedure TestQueryFilter;
    procedure TestQueryLocate;
    procedure TestFilterExpression;
    procedure TestDecodingSortedFields;
    procedure TestSmartOpen;
    procedure TestPrepare;
    procedure TestTimeFilterExpression;
    procedure TestDateTimeFilterExpression;
    procedure TestTimeLocateExpression;
    procedure TestDateTimeLocateExpression;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  DateUtils, ZSysUtils, ZTestConsts, ZTestCase, ZAbstractRODataset, ZDatasetUtils;

{ TZGenericTestDbcResultSet }

{**
  Create objects and allocate memory for variables
}
procedure TZGenericTestDbcResultSet.SetUp;
begin
  Connection := CreateDatasetConnection;
end;

{**
  Destroy objects and free allocated memory for variables
}
procedure TZGenericTestDbcResultSet.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

procedure TZGenericTestDbcResultSet.TestConnection;
var
  MetadataList: TStrings;
begin
  CheckEquals(False, Connection.Connected);
  CheckEquals(False, Connection.ReadOnly);
  CheckEquals(True, Connection.AutoCommit);

  Connection.AutoCommit := False;
  CheckEquals(Ord(tiNone), Ord(Connection.TransactIsolationLevel));

  { Checks without transactions. }
  Connection.Connect;
  CheckEquals(True, Connection.Connected);
  Connection.Commit;
  Connection.Rollback;
  Connection.Disconnect;
  CheckEquals(False, Connection.Connected);

  { Checks with transactions. }
  Connection.TransactIsolationLevel := tiReadCommitted;
  Connection.Connect;
  CheckEquals(True, Connection.Connected);
  Connection.Commit;
  Connection.Rollback;
  Connection.Disconnect;
  CheckEquals(False, Connection.Connected);

  MetadataList := TStringList.Create;
  try
    Connection.GetProtocolNames(MetadataList);
    Check(MetadataList.Count > 0, 'GetProtocolNames returns an empty list');
    Check(MetadataList.IndexOf(Protocol) >= 0, 'Error in GetProtocolNames');

    try
      Connection.GetCatalogNames(MetadataList);
      Fail('On closed connection call should throw exception');
    except
      // Ignore.
    end;

    Connection.Connect;

    Connection.GetCatalogNames(MetadataList);
    Connection.GetSchemaNames(MetadataList);
    Connection.GetTableNames('', MetadataList);
    Check(MetadataList.Count > 0, 'Error in GetTableNames');
    Connection.GetStoredProcNames('', MetadataList);
  finally
    MetadataList.Free;
  end;
end;

{**
  Check functionality prepared statement
}
procedure TZGenericTestDbcResultSet.TestPreparedStatement;
var
  Query: TZQuery;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TMemoryStream;
  s:string;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := true;

    with Query do
    begin
      SQL.Text := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;
      SQL.Text := 'DELETE FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;
    end;

    {
      The test for equipment table
    }
    with Query do
    begin
      { Create prepared statement for equipment table }
      Sql.Text := 'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
          + ' woff_date) VALUES(:q_id, :eq_name, :eq_type, :eq_cost, :eq_date, :woff_date)';
      CheckEquals(6, Params.Count);

      Params[0].DataType := ftInteger;
      Params[1].DataType := ftString;
      Params[2].DataType := ftSmallint;
      Params[3].DataType := ftFloat;
      Params[4].DataType := ftDate;
      Params[5].DataType := ftDate;

      Params[0].AsInteger := TEST_ROW_ID;
      Params[1].AsString := '\xyz\'+#13;
      Params[2].AsInteger := 7;
      Params[3].AsFloat := 1234.567;
      Params[4].AsDateTime := EncodeDate(1999, 8, 5);
      Params[5].Value := Null;
      ExecSQL;

      CheckEquals(1, RowsAffected);

      { check inserted row from equipment table }
      SQL.Text := 'SELECT * FROM equipment WHERE eq_id = :eq_id';
      CheckEquals(1, Query.Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;

      Open;
      CheckEquals(1, RecordCount);
      CheckEquals(False, IsEmpty);
      CheckEquals(TEST_ROW_ID, FieldByName('eq_id').AsInteger);
      s:=FieldByName('eq_name').AsString;
      CheckEquals('\xyz\'#13, s);
      CheckEquals(7, FieldByName('eq_type').AsInteger);
      CheckEquals(1234.567, FieldByName('eq_cost').AsFloat, 0.001);
      CheckEquals(EncodeDate(1999, 8, 5), FieldByName('eq_date').AsDateTime);
      CheckEquals(True, FieldByName('woff_date').IsNull);
      Close;

      { update inserted row from equipment table }
      SQL.Text := 'UPDATE equipment SET eq_name = :eq_name WHERE eq_id = :eq_id';
      CheckEquals(2, Params.Count);

      Params[0].DataType := ftString;
      Params[1].DataType := ftInteger;

      Params[0].AsString := 'xyz1';
      Params[1].AsInteger := TEST_ROW_ID;

      ExecSQL;
      CheckEquals(1, RowsAffected);

      { delete inserted row from equipment table }
      SQL.Text := 'DELETE FROM equipment WHERE eq_id = :eq_id';

      CheckEquals(1, Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;
      ExecSQL;
      CheckEquals(1, RowsAffected);
    end;

    { The test for people table }
    with Query do
    begin
      { Create prepared statement for people table }
      SQL.Text := 'INSERT INTO people (p_id, p_dep_id, p_name, p_begin_work, p_end_work,' +
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
      Params[6].LoadFromStream(StrStream, {$IFDEF DELPHI12_UP}ftWideMemo{$ELSE}ftMemo{$ENDIF});

      Params[7].Value := Null;
      ExecSql;
      CheckEquals(1, RowsAffected);

      { Checks inserted row. }
      SQL.Text := 'SELECT * FROM people WHERE p_id = :p_id';
      CheckEquals(1, Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;
      ReadOnly:=True;
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
      SQL.Text := 'DELETE FROM people WHERE p_id = :p_id';
      CheckEquals(1, Params.Count);

      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;

      ExecSQL;
      CheckEquals(1, RowsAffected);
    end;
  finally
    Query.Free;
  end;
end;

{**
  Check functionality ParamChar
}
procedure TZGenericTestDbcResultSet.TestParamChar;
var
  Query: TZQuery;
  s:string;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;

    with Query do
    begin
      SQL.Text := 'DELETE FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;
    end;

    {
      The test for equipment table
    }
    with Query do
    begin
      { Create prepared statement for equipment table }
      ParamChar := '&';
      Sql.Text := 'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
          + ' woff_date) VALUES(:q_id, :eq_name, :eq_type, :eq_cost, :eq_date, :woff_date)';
      CheckEquals(0, Params.Count);

      Sql.Text := 'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
          + ' woff_date) VALUES(&q_id, &eq_name, &eq_type, &eq_cost, &eq_date, &woff_date)';
      CheckEquals(6, Params.Count);

      Params[0].DataType := ftInteger;
      Params[1].DataType := ftString;
      Params[2].DataType := ftSmallint;
      Params[3].DataType := ftFloat;
      Params[4].DataType := ftDate;
      Params[5].DataType := ftDate;

      Params[0].AsInteger := TEST_ROW_ID;
      Params[1].AsString := '\xyz\'+#13;
      Params[2].AsInteger := 7;
      Params[3].AsFloat := 1234.567;
      Params[4].AsDateTime := EncodeDate(1999, 8, 5);
      Params[5].Value := Null;
      ExecSQL;

      CheckEquals(1, RowsAffected);

      { check inserted row from equipment table }
      SQL.Text := 'SELECT * FROM equipment WHERE eq_id = &eq_id';
      CheckEquals(1, Query.Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;

      Open;
      CheckEquals(1, RecordCount);
      CheckEquals(False, IsEmpty);
      CheckEquals(TEST_ROW_ID, FieldByName('eq_id').AsInteger);
      s:=FieldByName('eq_name').AsString;
      CheckEquals('\xyz\'#13, s);
      CheckEquals(7, FieldByName('eq_type').AsInteger);
      CheckEquals(1234.567, FieldByName('eq_cost').AsFloat, 0.001);
      CheckEquals(EncodeDate(1999, 8, 5), FieldByName('eq_date').AsDateTime);
      CheckEquals(True, FieldByName('woff_date').IsNull);
      Close;

      { delete inserted row from equipment table }
      SQL.TEXT := ''; // cleanup beacuse otherwise the previous select would be parsed
                      // resulting in an error because there's a space immediately after the '*' symbol
      ParamChar := '*';
      SQL.Text := 'DELETE FROM equipment WHERE eq_id = *eq_id';

      CheckEquals(1, Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;
      ExecSQL;
      CheckEquals(1, RowsAffected);
    end;

  finally
    Query.Free;
  end;
end;

{**
  Check functionality of TZQuery
}
procedure TZGenericTestDbcResultSet.TestQuery;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    //Query.DbcStatement.SetResultSetType(rtScrollInsensitive);
    //CheckEquals(ord(rcReadOnly), ord(Query.DbcStatement.GetResultSetConcurrency));
    TestQueryGeneric(Query);
  finally
    Query.Free;
  end;
end;


{**
  Check functionality execute statement for  TZReadOnlyQuery
}
procedure TZGenericTestDbcResultSet.TestQueryExecSql;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;

    with Query do
    begin
      SQL.Text := 'UPDATE equipment SET eq_name=eq_name';
      ExecSQL;
      SQL.Text := 'SELECT * FROM equipment';
      ExecSQL;

  //  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  //  Check(Statement.Execute('SELECT * FROM equipment'));

      SQL.Text := 'DELETE FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;

      { insert row to department table }
      SQL.Text := 'INSERT INTO department VALUES (' +
        IntToStr(TEST_ROW_ID) + ',''Some agency'',''ENG'',''Some city'')';
      ExecSQL;
      CheckEquals(1, RowsAffected);

      { check what row inserted }
      SQL.Text := 'SELECT * FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
      Open;
      CheckEquals(1, RecordCount);
      Close;

      { update row in department table }
      SQL.Text := 'UPDATE department SET dep_name=NULL, dep_shname=NULL, dep_address=NULL WHERE dep_id = ' +
      IntToStr(TEST_ROW_ID);
      ExecSQL;
      CheckEquals(1, RowsAffected);
      Close;

      { delete value from department table }
      SQL.Text := 'DELETE FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;
      CheckEquals(1, RowsAffected);
      Close;

      { check what row deleted }
      SQL.Text := 'SELECT * FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
      Open;
      CheckEquals(True, IsEmpty);
      Close;
    end;
  finally
    Query.Free;
  end;
end;

{**
  Check functionality of TZReadOnlyQuery
}
procedure TZGenericTestDbcResultSet.TestReadOnlyQuery;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
  //  Query.DbcStatement.SetResultSetType(rtScrollInsensitive);
  //  CheckEquals(ord(rcReadOnly), ord(Query.DbcStatement.GetResultSetConcurrency));
    TestQueryGeneric(Query);
  finally
    Query.Free;
  end;
end;

{**
  Check functionality of TZReadOnlyQuery Using PrefereRealPrepared
}
procedure TZGenericTestDbcResultSet.TestRealPrepReadOnlyQuery;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  Query.Options:= Query.Options + [doPreferPrepared];
  try
    Query.Connection := Connection;
  //  Query.DbcStatement.SetResultSetType(rtScrollInsensitive);
  //  CheckEquals(ord(rcReadOnly), ord(Query.DbcStatement.GetResultSetConcurrency));
    TestQueryGeneric(Query);
  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDbcResultSet.TestQueryUpdate;
var
  Sql_: string;
  Query: TZQuery;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TMemoryStream;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := true;
  //  CheckEquals(Ord(rtScrollInsensitive), Ord(Query.DbcStatement.GetResultSetType));
  //  CheckEquals(Ord(rcUpdatable), Ord(Query.DbcStatement.GetResultSetConcurrency));

    Query.SQL.Text := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
    Query.ExecSQL;

    Query.SQL.Text := 'DELETE FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
    Query.ExecSQL;

    {
      The test for equipment table
    }

    with Query do
    begin
      { insert test record to equipment }
      Sql_ := 'SELECT * FROM equipment where eq_id = ' + IntToStr(TEST_ROW_ID);
      SQL.Text := Sql_;
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
      CheckEquals(False, Modified);
      Close;

      { update row for equipment}
      SQL.Text := Sql_;
      Open;
      CheckEquals(False, IsEmpty);
      CheckEquals(True, Bof);

      Edit;
      CheckEquals(Ord(dsEdit), Ord(State));
      FieldByName('eq_name').AsString := 'The some thing5678901234567890';
      FieldByName('eq_type').AsInteger := 1;
      FieldByName('eq_cost').AsFloat := 12345.678;
      FieldByName('eq_date').AsDateTime := EncodeDate(1989, 07, 07);
      FieldByName('woff_date').AsDateTime := EncodeDate(1998, 04, 24);
      CheckEquals(True, Modified);
      Post;
      CheckEquals(False, Modified);
      Close;

      { check previous updated row}
      SQL.Text := Sql_;
      Open;
      CheckEquals(False, IsEmpty);

      CheckEquals(True, Bof);
      CheckEquals(Ord(dsBrowse), Ord(State));
      CheckEquals('The some thing5678901234567890', FieldByName('eq_name').AsString);
      CheckEquals(1, FieldByName('eq_type').AsInteger);
      CheckEquals(12345.678, FieldByName('eq_cost').AsFloat, 0.01);
      CheckEquals(EncodeDate(1989, 07, 07), FieldByName('eq_date').AsDateTime);
      CheckEquals(EncodeDate(1998, 04, 24), FieldByName('woff_date').AsDateTime);
      Delete;
      Close;

      { check what record deleted }
      SQL.Text := Sql_;
      Open;
      CheckEquals(True, IsEmpty);
    end;

      {
        The test for people table
      }
    with Query do
    begin
      Sql_ := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
      SQL.Text := Sql_;
      ExecSQL;

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
      SQL.Text := Sql_;
      Open;
      CheckEquals(True, Query.IsEmpty);

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
      CheckEquals(False, Modified);
      Close;


      { check previous inserted record }
      SQL.Text := Sql_;
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
      CheckEquals(False, Modified);
      Close;

      { create and update resultset for people table for p_id = TEST_ROW_ID }
      SQL.Text := Sql_;
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

      BinStream.Free;
      BinStream1.Free;
      StrStream.Free;
      StrStream1.Free;

      { create and update resultset for equipment table for eq_id = TEST_ROW_ID }
      SQL.Text := Sql_;
      Open;
      CheckEquals(True, IsEmpty);
      Close;
    end;
  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDbcResultSet.TestQueryGeneric(Query: TDataset);
var
  SQL: string;
begin
  { select equipment table }
  SQL := 'DELETE FROM equipment where eq_id > 100';
  if GetName = 'TestQuery' then
  begin
    (Query as TZquery).SQL.Text := SQL;
    (Query as TZquery).ExecSQL;
  end
  else
  begin
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
    (Query as TZReadOnlyQuery).ExecSQL;
  end;

  { select equipment table }
  SQL := 'SELECT * FROM equipment where eq_id > 100';
  if GetName = 'TestQuery' then
    (Query as TZquery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    CheckEquals(True, IsEmpty);
    Close;
  end;

  { select equipment table}
  SQL := 'SELECT * FROM equipment where eq_id = 1';
  if GetName = 'TestQuery' then
    (Query as TZquery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    CheckEquals(1,     RecordCount);
    CheckEquals(False, IsEmpty);
    CheckEquals(1,       FieldByName('eq_id').AsInteger);
    CheckEquals('Volvo', FieldByName('eq_name').AsString);
    CheckEquals(1,       FieldByName('eq_type').AsInteger);
    CheckEquals(15000,   FieldByName('eq_cost').AsFloat);
    CheckEquals(EncodeDate(1998, 03, 04), Trunc(FieldByName('eq_date').AsDateTime));
    CheckEquals(True,   FieldByName('woff_date').IsNull);
    Close;
  end;

  { select from people table two records }
  SQL := 'SELECT * FROM people where p_id <= 2';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
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
  SQL := 'SELECT * FROM cargo where c_id = 2';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
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
  SQL := 'SELECT * FROM cargo where c_date_came = :DateCame';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    if GetName = 'TestQuery' then
      (Query as TZQuery).ParamByName('DateCame').AsDateTime := EncodeDate(2002, 12, 20) + EncodeTime(2, 0, 0, 0)
    else
      (Query as TZReadOnlyQuery).ParamByName('DateCame').AsDateTime := EncodeDate(2002, 12, 20) + EncodeTime(2, 0, 0, 0);
    Open;
    CheckEquals(1, RecordCount);
    CheckEquals(EncodeDate(2002, 12, 20) + EncodeTime(2, 0, 0, 0),
      FieldByName('c_date_out').AsDateTime, 0.001);
    Close;
  end;

  { select equipment table }
  SQL := 'SELECT * FROM equipment';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
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

  { create resultset for equipment table with limit rows}
(*
  SQL := 'SELECT * FROM equipment';
  if GetName = 'TestQuery' then
  begin
    (Query as TZQuery).DbcStatement.SetMaxRows(2);
    (Query as TZquery).SQL.Text := SQL;
  end
  else
  begin
    (Query as TZReadOnlyQuery).DbcStatement.SetMaxRows(2);
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  end;
  with Query do
  begin
    Open;
    CheckEquals(False, IsEmpty);
    CheckEquals(2, RecordCount);

    CheckEquals(True,  Bof);
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, True);

    Close;
  end;
*)
end;

{**
   Check functionality execute statement for  TZReadOnlyQuery
}
procedure TZGenericTestDbcResultSet.TestReadOnlyQueryExecSql;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;

    with Query do
    begin
      SQL.Text := 'UPDATE equipment SET eq_name=eq_name';
      ExecSQL;
      SQL.Text := 'SELECT * FROM equipment';
      ExecSQL;

  //  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  //  Check(Statement.Execute('SELECT * FROM equipment'));

      SQL.Text := 'DELETE FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;

      { insert row to department table }
      SQL.Text := 'INSERT INTO department VALUES (' +
      IntToStr(TEST_ROW_ID) + ',''Some agency'',''ENG'',''Some city'')';
      ExecSQL;
      CheckEquals(1, RowsAffected);

      { check what row inserted }
      SQL.Text := 'SELECT * FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
      Open;
      CheckEquals(1, RecordCount);
      Close;

      { update row in department table }
      SQL.Text :=
      'UPDATE department SET dep_name=NULL, dep_shname=NULL, dep_address=NULL WHERE dep_id = ' +
      IntToStr(TEST_ROW_ID);
      ExecSQL;
      CheckEquals(1, RowsAffected);
      Close;

      { delete value from department table }
      SQL.Text := 'DELETE FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;
      CheckEquals(1, RowsAffected);
      Close;

      { chech what recird deleted }
      SQL.Text := 'SELECT * FROM department where dep_id = ' + IntToStr(TEST_ROW_ID);
      Open;
      CheckEquals(True, IsEmpty);
      Close;
    end;
  finally
    Query.Free;
  end;
end;

{**
   Test for filtering recods in TZQuery
}
procedure TZGenericTestDbcResultSet.TestQueryFilter;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
  //!!  TestFilterGeneric(Query);
  finally
    Query.Free;
  end;
end;

{**
   Generic test for filtering recods in dataset
}
procedure TZGenericTestDbcResultSet.TestFilterGeneric(Query: TDataset);
var
  SQL: string;
begin
  { select equipment table }
  SQL := 'SELECT * FROM equipment';
  if GetName = 'TestQueryFilter' then
    (Query as TZquery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    Filtered := True;
    CheckEquals(4, RecordCount);
    CheckEquals(False, IsEmpty);

    Filter := 'eq_id = 3';
    CheckEquals(1, RecordCount);
    FieldByName('eq_name').AsString := 'Computer';

    Filter := 'eq_name = ''Volvo''';
    CheckEquals(1, RecordCount);
    FieldByName('eq_id').AsInteger := 1;

    Filter := 'eq_cost > 1000';
    CheckEquals(2, RecordCount);
    First;
    FieldByName('eq_id').AsInteger := 1;
    Next;
    FieldByName('eq_id').AsInteger := 2;

    Filter := 'eq_type <= 10';
    CheckEquals(3, RecordCount);
    First;
    FieldByName('eq_id').AsInteger := 1;
    Next;
    FieldByName('eq_id').AsInteger := 2;
    Next;
    FieldByName('eq_id').AsInteger := 3;

    Filter := 'eq_type = ''C*''';
    CheckEquals(1, RecordCount);
    FieldByName('eq_id').AsInteger := 3;

    Filter := 'eq_type = ''*o*''';
    CheckEquals(4, RecordCount);
    {check what cursor save position}
    FieldByName('eq_id').AsInteger := 4;
    First;
    FieldByName('eq_id').AsInteger := 1;
    Next;
    FieldByName('eq_id').AsInteger := 2;
    Next;
    FieldByName('eq_id').AsInteger := 3;
    Next;
    FieldByName('eq_id').AsInteger := 4;

    Filter := 'eq_type = ''' + DateToStr(Encodedate(2001, 7, 10)) + '''';
    CheckEquals(1, RecordCount);
    FieldByName('eq_id').AsInteger := 2;

    Filter := 'eq_type > ''' + DateToStr(Encodedate(2000, 1, 1)) + '''';
    CheckEquals(2, RecordCount);
    First;
    FieldByName('eq_id').AsInteger := 2;
    Next;
    FieldByName('eq_id').AsInteger := 4;


    Filter := 'eq_type <= ''' + DateToStr(Encodedate(2000, 7, 8)) + '''';
    CheckEquals(3, RecordCount);
    First;
    FieldByName('eq_id').AsInteger := 1;
    Next;
    FieldByName('eq_id').AsInteger := 3;
    Next;
    FieldByName('eq_id').AsInteger := 4;

    Close;
  end;
end;

{**
  Test for locating recods in TZReadOnlyQuery
}
procedure TZGenericTestDbcResultSet.TestQueryLocate; 
var
  Query: TZReadOnlyQuery;
  ResData : boolean; 
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Add('select * from cargo'); 
    Query.ExecSQL; 
    Query.Open; 
    Check(Query.RecordCount > 0, 'Query return no records'); 
    ResData := Query.Locate('C_DEP_ID;C_WIDTH;C_SEAL',VarArrayOf(['1','10','2']),[loCaseInsensitive]); 
    CheckEquals(true,ResData); 
    ResData := Query.Locate('C_DEP_ID,C_WIDTH,C_SEAL',VarArrayOf(['2',Null,'1']),[loCaseInsensitive]); 
    CheckEquals(true,ResData); 
  finally 
    Query.Free; 
  end; 
end; 

{**
  Test for filtering recods in TZReadOnlyQuery
}
procedure TZGenericTestDbcResultSet.TestReadOnlyQueryFilter;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
  //!!  TestFilterGeneric(Query);
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for filter expressions.
}
procedure TZGenericTestDbcResultSet.TestFilterExpression;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT * FROM people';

    Query.Filter := 'p_id + 1 = 2';
    Query.Filtered := True;
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(1, Query.FieldByName('p_id').AsInteger);

    Query.Filter := '"p_id" = 2';
    CheckEquals(1, Query.RecordCount);
    CheckEquals(2, Query.FieldByName('p_id').AsInteger);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for decoding sorted fields.
}
procedure TZGenericTestDbcResultSet.TestDecodingSortedFields;
var
  Query: TZReadOnlyQuery;
  FieldRefs: TObjectDynArray;
  FieldDirs: TBooleanDynArray;
  OnlyDataFields: Boolean;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT p_id, p_dep_id, p_name FROM people';
    Query.Open;

    DefineSortedFields(Query, 'p_id', FieldRefs, FieldDirs, OnlyDataFields);
    CheckEquals(1, Length(FieldRefs));
    CheckEquals(Integer(Query.Fields[0]), Integer(FieldRefs[0]));
    CheckEquals(1, Length(FieldDirs));
    CheckEquals(True, FieldDirs[0]);
    CheckEquals(True, OnlyDataFields);

    DefineSortedFields(Query, 'p_id ASC, p_name DESC', FieldRefs,
      FieldDirs, OnlyDataFields);
    CheckEquals(2, Length(FieldRefs));
    CheckEquals(Integer(Query.Fields[0]), Integer(FieldRefs[0]));
    CheckEquals(Integer(Query.Fields[2]), Integer(FieldRefs[1]));
    CheckEquals(2, Length(FieldDirs));
    CheckEquals(True, FieldDirs[0]);
    CheckEquals(False, FieldDirs[1]);
    CheckEquals(True, OnlyDataFields);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for SmartOpen option.
}
procedure TZGenericTestDbcResultSet.TestSmartOpen;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from people';
    Check(not (doSmartOpen in Query.Options));

    Query.Open;
    Check(Query.Active);
    Query.Close;

    Query.Active := True;
    Check(Query.Active);
    Query.Active := False;

    Query.SQL.Text := 'update people set p_id=p_id where 1=0';
    try
      Query.Open;
      Fail('Wrong open behaviour without SmartOpen.');
    except
      // Ignore.
    end;

    try
      Query.Active := True;
      Fail('Wrong open behaviour without SmartOpen.');
    except
      // Ignore.
    end;

    Query.Options := Query.Options + [doSmartOpen];
    Query.SQL.Text := 'select * from people';

    Query.Open;
    Check(Query.Active);
    Query.Close;

    Query.Active := True;
    Check(Query.Active);
    Query.Active := False;

    Query.SQL.Text := 'update people set p_id=p_id where 1=0';
    try
      Query.Open;
    except
      Fail('Wrong open behaviour with SmartOpen.');
    end;
//    Check(not Query.Active);

    Query.Active := False;
    try
      Query.Active := True;
    except
      Fail('Wrong open behaviour with SmartOpen.');
    end;
//    Check(not Query.Active);

  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDbcResultSet.TestPrepare;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from people';

    Query.Prepare;
    Check(Query.Prepared);
    Check(Not Query.Active);
    Query.Open;
    Check(Query.Active);
    Check(Query.Prepared);
    Query.Close;
    Check(Not Query.Active);
    Check(Query.Prepared);
    Query.UnPrepare;
    Check(Not Query.Prepared);

    Query.Active := True;
    Check(Query.Active);
    Check(Query.Prepared);
    Query.Unprepare;
    try
      Query.Prepare;
      Fail('Wrong prepare behaviour.');
    except
      // Ignore.
    end;
    Check(Not Query.Prepared);
    Query.Active := False;

  finally
    Query.Free;
  end;
end;


{**
Runs a test for time filter expressions.
}
procedure TZGenericTestDbcResultSet.TestTimeFilterExpression;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT * FROM people';
    //!! Oracle: depend from local settings
    Query.Filter := 'p_begin_work >= "'+TimeToStr(EncodeTime(8,30,0,50))+'"';
    Query.Filtered := True;
    Query.Open;
    CheckEquals(4, Query.RecordCount);
    Query.Last;
    CheckEquals(EncodeTime(8,30,0,0), Query.FieldByName('p_begin_work').AsDateTime);
    Query.Close;
    Query.Filter := '(p_begin_work > "'+TimeToStr(EncodeTime(8,0,0,0))+ '") AND (p_end_work < "'+TimeToStr(EncodeTime(18,0,0,0))+'")';
    Query.Open;
    CheckEquals(2, Query.RecordCount);
    Query.Last;
    CheckEquals(EncodeTime(8,30,0,0), Query.FieldByName('p_begin_work').AsDateTime);
    CheckEquals(EncodeTime(17,30,0,0), Query.FieldByName('p_end_work').AsDateTime);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
Runs a test for Datetime filter expressions.
}
procedure TZGenericTestDbcResultSet.TestDateTimeFilterExpression;
var
  Query: TZQuery;
  Date_came,Date_out : TDateTime;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT * FROM cargo';
    Date_came := EncodeDateTime(2002,12,19,18,30,0,0);
    Query.Filter := 'c_date_came >= "'+DateTimeToStr(Date_came)+'"';
    Query.Filtered := True;
    Query.Open;
    CheckEquals(3, Query.RecordCount);
    Query.Last;
    Date_came := EncodeDateTime(2002,12,21,10,20,0,0);
    CheckEquals(Date_Came, Query.FieldByName('c_date_came').AsDateTime);
    Query.Close;
    Date_came := EncodeDateTime(2002,12,19,14,30,0,0);
    Date_out := EncodeDateTime(2002,12,23,2,0,0,0);
    Query.Filter := '(c_date_came > "'+DateTimeToStr(Date_came)+ '") AND (c_date_out < "'+DateTimeToStr(Date_out)+'")';
    Query.Open;
    CheckEquals(2, Query.RecordCount);
    Query.First;
    Date_came := EncodeDateTime(2002,12,20,2,0,0,0);
    Date_out := EncodeDateTime(2002,12,20,2,0,0,0);
    CheckEquals(Date_came, Query.FieldByName('c_date_came').AsDateTime);
    CheckEquals(Date_out, Query.FieldByName('c_date_out').AsDateTime);
    Query.Close;
    Date_came := EncodeDateTime(2002,12,21,14,30,0,0); 
    Date_out := EncodeDateTime(2002,12,25,2,0,0,0); 
    Query.Filter := '(c_date_came < "'+DateTimeToStr(Date_came)+ '") AND (c_date_out > "'+DateTimeToStr(Date_out)+'")'; 
    Query.Open; 
    CheckEquals(1, Query.RecordCount); 
    Query.First; 
    Date_came := EncodeDateTime(2002,12,21,10,20,0,0); 
    Date_out := EncodeDateTime(2002,12,26,0,0,0,0); 
    CheckEquals(Date_came, Query.FieldByName('c_date_came').AsDateTime); 
    CheckEquals(Date_out, Query.FieldByName('c_date_out').AsDateTime); 
    Query.Close;
   finally 
    Query.Free;
  end;
end;

{**
Runs a test for time locate expressions.
}
procedure TZGenericTestDbcResultSet.TestTimeLocateExpression;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT * FROM people';
    Query.Open;
    if StartsWith(Protocol, 'oracle') then begin
      Check(Query.Locate('p_begin_work',EncodeDate(1, 1, 1) - EncodeTime(8,30,0,0),[]));
      CheckEqualsDate(EncodeDate(1, 1, 1) - EncodeTime(8,30,0,0), Query.FieldByName('p_begin_work').AsDateTime);
      CheckEqualsDate(EncodeDate(1, 1, 1) - EncodeTime(17,30,0,0), Query.FieldByName('p_end_work').AsDateTime);
    end else begin
      CheckEquals(true, Query.Locate('p_begin_work',EncodeTime(8,30,0,0),[]));
      CheckEqualsDate(EncodeTime(8,30,0,0), Query.FieldByName('p_begin_work').AsDateTime);
      CheckEqualsDate(EncodeTime(17,30,0,0), Query.FieldByName('p_end_work').AsDateTime);
    end;
    Query.Close;
    Query.Open;
    if StartsWith(Protocol, 'oracle') then begin
      CheckEquals(false, Query.Locate('p_begin_work',EncodeDate(1, 1, 1) - EncodeTime(8,31,0,0),[]));
    end else begin
      CheckEquals(false, Query.Locate('p_begin_work',EncodeTime(8,31,0,0),[]));
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
Runs a test for Datetime locate expressions.
}
procedure TZGenericTestDbcResultSet.TestDateTimeLocateExpression;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT * FROM cargo';
    Query.Open;
    CheckEquals(true, Query.Locate('c_date_came',EncodeDateTime(2002,12,19,14,0,0,0),[]));
    CheckEquals(EncodeDateTime(2002,12,19,14,0,0,0), Query.FieldByName('c_date_came').AsDateTime);
    CheckEquals(EncodeDateTime(2002,12,23,0,0,0,0), Query.FieldByName('c_date_out').AsDateTime);
    Query.Close;
    Query.Open;
    CheckEquals(false, Query.Locate('c_date_came',EncodeDateTime(2002,12,19,0,0,0,0),[]));
    Query.Close;
  finally
    Query.Free;
  end;
end; 

initialization
  RegisterTest('component',TZGenericTestDbcResultSet.Suite);
end.

