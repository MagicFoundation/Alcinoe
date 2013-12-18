{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Cases for MySQL Component Bug Reports       }
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

unit ZTestBugCompMySql;

interface

{$I ZBugReport.inc}

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  Classes, SysUtils, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDataset, ZConnection, ZDbcIntfs,
  ZBugReport, ZCompatibility, ZTestConsts, ZSqlUpdate, ZSqlProcessor,
  ZAbstractRODataset;

type

  {** Implements a bug report test case for MySQL components. }
  TZTestCompMySQLBugReport = class(TZSpecificSQLBugReportTestCase)
  private
    FConnection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: TZConnection read FConnection write FConnection;

  published
    procedure Test735226;
    procedure Test726788;
    procedure Test735299;
    procedure Test740144;
    procedure Test740899;
    procedure Test724542;
    procedure Test739444;
    procedure Test733236;
    procedure Test727373;
    procedure Test771576;
    procedure Test768163;
    procedure Test779520;
    procedure Test799863;
    procedure Test000001;
    procedure Test817607;
    procedure Test828147;
    procedure Test840608;
    procedure Test844415;
    procedure Test849723;
    procedure Test833489;
    procedure Test869609;
    procedure Test865564;
    procedure Test881634;
    procedure Test884135;
    procedure Test886841;
    procedure Test894367;
    procedure Test914436;
    procedure Test920589;
    procedure Test938705;
    procedure Test952976;
    procedure Test957126;
    procedure Test961337;
    procedure Test981208;
    procedure Test987022;
    procedure Test989474;
    procedure Test1045286;
    procedure Test1023149;
  end;

implementation

uses ZTestCase;

{ TZTestCompMySQLBugReport }

function TZTestCompMySQLBugReport.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5';
end;

procedure TZTestCompMySQLBugReport.SetUp;
begin
  Connection := CreateDatasetConnection;
end;

procedure TZTestCompMySQLBugReport.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Test the bug report #735226.

  "Duplicate field name" exception
  Initial Comment: When I use a query like this:

  SELECT * FROM TableA
  LEFT JOIN TableB ON TableB.TableAId = TableA.Id

  where both TableA and TableB have a column
  named 'Id', I get the exception:' "Duplicate field name 'Id' "
}
procedure TZTestCompMySQLBugReport.Test735226;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT * FROM table735226a'
      + ' LEFT JOIN table735226b ON table735226a.referenceid = table735226b.id';
    Query.Open;
    CheckEquals(3, Query.FieldCount);
    CheckEquals('id', Query.FieldDefs[0].Name);
    CheckEquals('referenceid', Query.FieldDefs[1].Name);
    CheckEquals('id_1', Query.FieldDefs[2].Name);
    Query.Last;
    Query.Close;

    Query.SQL.Text := 'SELECT table735226a.id as Table1Id, table735226b.id as Table2Id'
      + ' FROM table735226a LEFT JOIN table735226b ON table735226a.referenceid = table735226b.id';
    Query.Open;
    CheckEquals(2, Query.FieldCount);
    CheckEquals('Table1Id', Query.FieldDefs[0].Name);
    CheckEquals('Table2Id', Query.FieldDefs[1].Name);
    Query.Last;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #726788.

  Mixed case table problem (6.0.8)

  Initial Comment:
  When a table has got an mixed case tablename, there
  is an EZSQLException raised when the table is opened
  ('Row data is not accessable'). Making the tablename
  upper- or lowercase solves the problem, but isn't
  sollution
}
procedure TZTestCompMySQLBugReport.Test726788;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT * FROM Table726788';
    Query.Open;
    CheckEquals(1, Query.FieldCount);
    CheckEquals('FieldName', Query.FieldDefs[0].Name);
    Query.Last;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #735299.

  The commonly used MySQL Boolean fieldtype for
  MySQL, enum('Y','N'), is not working as expected in
  version 6.0.8. When you use this fieldtype, the values
  returned are strings, not booleans. The fieldtype is
  correct (Boolean) in version 5.4.1.
}
procedure TZTestCompMySQLBugReport.Test735299;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.SQL.Text := 'DELETE FROM table735299';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM table735299';
    Query.Open;
    CheckEquals(3, Query.FieldCount);
    CheckEquals(Ord(ftInteger), Ord(Query.FieldDefs[0].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.FieldDefs[1].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.FieldDefs[2].DataType));
    Query.Append;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsBoolean := True;
    Query.Fields[2].AsBoolean := False;
    Query.Post;
    Query.Close;

    Query.SQL.Text := 'SELECT * FROM table735299';
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(1, Query.Fields[0].AsInteger);
    CheckEquals(True, Query.Fields[1].AsBoolean);
    CheckEquals(False, Query.Fields[2].AsBoolean);
    Query.Delete;
    Query.Close;

    Query.SQL.Text := 'SELECT * FROM table735299';
    Query.Open;
    CheckEquals(0, Query.RecordCount);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #740144.

  Query.Locate('Boolean', VarArrayOf([True]), [])

  Gives the exception in :
  Could not convert variant of type (OleStr) into type
  (Boolean).
}
procedure TZTestCompMySQLBugReport.Test740144;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.SQL.Text := 'DELETE FROM table735299';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM table735299';
    Query.Open;
    Query.Append;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsBoolean := True;
    Query.Fields[2].AsBoolean := False;
    Query.Post;
    Query.Append;
    Query.Fields[0].AsInteger := 2;
    Query.Fields[1].AsBoolean := False;
    Query.Fields[2].AsBoolean := True;
    Query.Post;
    Query.Append;
    Query.Fields[0].AsInteger := 3;
    Query.Post;
    Query.Close;

    Query.SQL.Text := 'SELECT * FROM table735299 ORDER BY id';
    Query.Open;
    CheckEquals(True, Query.Locate('fld1', VarArrayOf([True]), []));
    CheckEquals(1, Query.RecNo);
    CheckEquals(True, Query.Locate('fld1,fld2', VarArrayOf([Null, Null]), []));
    CheckEquals(3, Query.RecNo);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #740899.

  PreparedStatement for Field=? (Null) generated
  Field=NULL instead Field IS NULL.
}
procedure TZTestCompMySQLBugReport.Test740899;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.SQL.Text := 'DELETE FROM table740899';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO table740899 (id) VALUES(1)';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO table740899 (id) VALUES(2)';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO table740899 (id,fld) VALUES(3,''xxx'')';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO table740899 (id,fld) VALUES(4,''yyy'')';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM table740899 ORDER BY id';
    Query.Open;
    CheckEquals(4, Query.RecordCount);
    CheckEquals(True, Query.Fields[1].IsNull);
    Query.Edit;
    Query.Fields[1].AsString := 'zzz';
    Query.Post;
    Query.Refresh;
    CheckEquals('zzz', Query.Fields[1].AsString);

    Query.Next;
    Query.Delete;
    Query.Refresh;
    CheckEquals(3, Query.RecordCount);
    Query.Close;

    Query.SQL.Text := 'DELETE FROM table740899';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #740899.

  Error posting data to update but no changes happened.
}
procedure TZTestCompMySQLBugReport.Test724542;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.SQL.Text := 'DELETE FROM table724542';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM table724542';
    Query.Open;
    Query.Append;
    Query.Fields[0].AsInteger := TEST_ROW_ID;
    Query.Fields[1].AsString := 'abcdefghijklmnopq';
    Query.Fields[2].Value := Null;
    Query.Post;
    Query.Close;

    Query.SQL.Text := 'SELECT * FROM table724542 where fld1 = ' + IntToStr(TEST_ROW_ID);
    Query.Open;
    Query.Edit;
    Query.Fields[2].AsString := 'abcdefg';
    Query.Post;
    Query.Close;

    Query.SQL.Text := 'SELECT * FROM table724542 where fld1 = ' + IntToStr(TEST_ROW_ID);
    Query.Open;
    CheckEquals(TEST_ROW_ID, Query.Fields[0].AsInteger);
    CheckEquals('abcdefghijklmnopq', Query.Fields[1].AsString);
    CheckEquals('abcdefg', Query.Fields[2].AsString);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #739444.

  Aliases for fields do not work. Result Set after
  execution SQL query do not contain the aliased fields.
}
procedure TZTestCompMySQLBugReport.Test739444;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.SQL.Text := 'select count(*) as items, sum(c_weight) as total, '+
      ' AVG(c_width) as average from cargo ';
    Query.Open;

    CheckEquals('items', Query.Fields[0].FieldName);
    CheckEquals('total', Query.Fields[1].FieldName);
    CheckEquals('average', Query.Fields[2].FieldName);
    CheckEquals(4, Query.Fields[0].AsInteger);
    CheckEquals(8434, Query.Fields[1].AsInteger);
    CheckEquals(8.5, Query.Fields[2].AsFloat, 0.01);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #733236.

  Locate for time fields because riased
  "Convertion is not possible for column 1 from Time to Timestamp"
}
procedure TZTestCompMySQLBugReport.Test733236;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.SQL.Text := 'select * from table733236';
    Query.Open;

    Query.Locate('time', EncodeTime(23, 59, 59, 0), []);
    Query.Locate('date', EncodeDate(2001, 01, 01), []);
    Query.Locate('datetime', EncodeDate(2004, 12, 31) +
      EncodeTime(23, 59, 59, 0), []);
    Query.Locate('timestamp', EncodeDate(2004, 12, 31) +
      EncodeTime(23, 59, 59, 0), []);

    Query.Close;
  finally
    Query.Free;
  end;

end;

{**
  Test the bug report #727373.

  The TZQuery with TZUpdateObject can't update records
  when left outer joins is used in sql query.
}
procedure TZTestCompMySQLBugReport.Test727373;
var
  Query: TZQuery;
  UpdateSql: TZUpdateSQL;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    UpdateSql := TZUpdateSQL.Create(nil);
    try
      Query.Connection := Connection;
      // Query.RequestLive := True;

      Query.SQL.Text := 'delete from people where p_id >= ' + IntToStr(TEST_ROW_ID);
      Query.ExecSQL;

      Query.SQL.Text := 'select p.*, d.dep_name from people p ' +
        ' left outer join department d on p.p_id = d.dep_id ' +
        ' where p_id = ' + IntToStr(TEST_ROW_ID);
      Query.UpdateObject := UpdateSql;
      UpdateSql.InsertSQL.Text := 'insert into people (p_id, p_name, p_begin_work, ' +
        ' p_end_work) values (:p_id, :p_name, :p_begin_work, :p_end_work)';
      UpdateSql.ModifySQL.Text := 'update people set p_id = :p_id, p_name = :p_name, ' +
        ' p_begin_work = :p_begin_work, p_end_work = :p_end_work where p_id = :OLD_p_id';
      UpdateSql.DeleteSQL.Text := 'delete from people where p_id = :OLD_p_id';
      UpdateSql.Params.ParamByName('p_id').DataType := ftInteger;
      UpdateSql.Params.ParamByName('p_id').ParamType := ptInput;
      UpdateSql.Params.ParamByName('p_name').DataType := ftString;
      UpdateSql.Params.ParamByName('p_name').ParamType := ptInput;
      UpdateSql.Params.ParamByName('p_begin_work').DataType := ftTime;
      UpdateSql.Params.ParamByName('p_begin_work').ParamType := ptInput;
      UpdateSql.Params.ParamByName('p_end_work').DataType := ftTime;
      UpdateSql.Params.ParamByName('p_end_work').ParamType := ptInput;
      UpdateSql.Params.ParamByName('OLD_p_id').DataType := ftInteger;
      UpdateSql.Params.ParamByName('OLD_p_id').ParamType := ptInput;

      Query.Open;
      Query.Append;
      Query.FieldByName('p_id').AsInteger := TEST_ROW_ID;
      Query.FieldByName('p_name').AsString := 'Vasia';
      Query.FieldByName('p_begin_work').AsDateTime := EncodeTime(9, 30, 0, 0);
      Query.FieldByName('p_end_work').AsDateTime := EncodeTime(18, 30, 0, 0);
      Query.Post;
      Query.ApplyUpdates;
      Query.Close;

      Query.Open;
      CheckEquals(False, Query.IsEmpty);
      CheckEquals(TEST_ROW_ID, Query.FieldByName('p_id').AsInteger);
      CheckEquals('Vasia', Query.FieldByName('p_name').AsString);
      CheckEquals(EncodeTime(9, 30, 0, 0), Query.FieldByName('p_begin_work').AsDateTime);
      CheckEquals(EncodeTime(18, 30, 0, 0), Query.FieldByName('p_end_work').AsDateTime);
      Query.Edit;
      Query.FieldByName('p_id').AsInteger := TEST_ROW_ID;
      Query.FieldByName('p_name').AsString := 'Petia';
      Query.FieldByName('p_begin_work').AsDateTime := EncodeTime(10, 0, 0, 0);
      Query.FieldByName('p_end_work').AsDateTime := EncodeTime(19, 0, 0, 0);
      Query.Post;
      Query.ApplyUpdates;
      Query.Close;

      Query.Open;
      CheckEquals(False, Query.IsEmpty);
      CheckEquals(TEST_ROW_ID, Query.FieldByName('p_id').AsInteger);
      CheckEquals('Petia', Query.FieldByName('p_name').AsString);
      CheckEquals(EncodeTime(10, 0, 0, 0), Query.FieldByName('p_begin_work').AsDateTime, 0.001);
      CheckEquals(EncodeTime(19, 0, 0, 0), Query.FieldByName('p_end_work').AsDateTime, 0.001);
      Query.Delete;
      Query.ApplyUpdates;
      Query.Close;

      Query.Open;
      CheckEquals(True, Query.IsEmpty);
    finally
      UpdateSql.Free;
    end;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #771576.

  Problem related to ':' character in SQL query.
}
procedure TZTestCompMySQLBugReport.Test771576;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'set @user_var=0';
    Query.ExecSQL;

    Query.SQL.Text := 'update people set p_id=(@user_var::=@user_var+1)'
      + ' where 1=0';
    Query.ExecSQL;

    Query.ParamCheck := False;
    Query.SQL.Text := 'update people set p_id=(@user_var:=@user_var+1)'
      + ' where 1=0';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #771576.

  unsigned int field problem.
}
procedure TZTestCompMySQLBugReport.Test768163;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'delete from table768163';
    Query.ExecSQL;

    // Query.RequestLive := True;
    Query.SQL.Text := 'select * from table768163';
    Query.Open;
    Query.Append;
    TLargeIntField(Query.Fields[0]).AsLargeInt := 2147483648;
    Query.Post;
    Query.Close;

    Query.SQL.Text := 'select * from table768163';
    Query.Open;
    CheckEquals(2147483648, TLargeIntField(Query.Fields[0]).AsLargeInt);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #779520.

  Invalid values of float fields when ReadOnly = False
}
procedure TZTestCompMySQLBugReport.Test779520;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'select c_id, c_cost from cargo';
    Query.Open;
    CheckEquals(1, Query.Fields[0].AsInteger);
    CheckEquals(1769.4300, Query.Fields[1].AsFloat, 0.001);
    Query.Close;

    // Query.RequestLive := True;
    Query.Open;
    CheckEquals(1, Query.Fields[0].AsInteger);
    CheckEquals(1769.4300, Query.Fields[1].AsFloat, 0.001);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #799863.

  Cannot see YEAR type in Delphi.
}
procedure TZTestCompMySQLBugReport.Test799863;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from table799863';
    Query.Open;
    CheckEquals(Ord(ftSmallInt), Ord(Query.Fields[0].DataType));
    CheckEquals(3, Query.RecordCount);
    CheckEquals(0, Query.Fields[0].AsInteger);
    Query.Next;
    CheckEquals(1940, Query.Fields[0].AsInteger);
    Query.Next;
    CheckEquals(2003, Query.Fields[0].AsInteger);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #000001

  ZQuery does not display MySQL fields with longtext type.
}
procedure TZTestCompMySQLBugReport.Test000001;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from table000001';
    Query.Open;
    CheckEquals(2, Query.Fields.Count);
    CheckEquals(Ord(ftMemo), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftBlob), Ord(Query.Fields[1].DataType));
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #817607

  Problem when refresh Query component.

  Problem when use Refresh() in Query component who have fields
  which name with space symbol. Ect. filed name : 'zeos cool component'
}
procedure TZTestCompMySQLBugReport.Test817607;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from table817607';
    Query.Open;
    CheckEquals('id', Query.Fields[0].DisplayName);
    CheckEquals('fruit name', Query.Fields[1].DisplayName);
    Query.RecNo := 2;
    Query.Refresh;
    CheckEquals(2, Query.RecNo);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #828147

  No automatic post of memo or blob field.
}
procedure TZTestCompMySQLBugReport.Test828147;
var
  Query: TZQuery;
  Temp: PChar;
  Stream: TStream;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'update table828147 set txt=''abc''';
    Query.ExecSQL;

    // Query.RequestLive := True;
    Query.SQL.Text := 'select id, txt from table828147';
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftMemo), Ord(Query.Fields[1].DataType));
    CheckEquals('abc', Query.Fields[1].AsString);

    Query.Edit;
    Stream := Query.CreateBlobStream(Query.Fields[1], bmWrite);
    try
      Temp := 'xyz';
      Stream.Write(Temp^, StrLen(Temp));
    finally
      Stream.Free;
    end;
    Query.Next;

    Query.Close;
    Query.Open;

    CheckEquals(1, Query.RecordCount);
    CheckEquals('xyz', Query.Fields[1].AsString);

    Query.Close;

    Query.SQL.Text := 'update table828147 set txt=''abc''';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #840608

  Wrong =NULL in where clause of generated DML statements.
}
procedure TZTestCompMySQLBugReport.Test840608;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'update table840608 set name=''PARACETAMOL (GPO)''';
    Query.ExecSQL;

    // Query.RequestLive := True;
    Query.SQL.Text := 'select * from table840608';
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals('PARACETAMOL (GPO)', Query.FieldByName('name').AsString);

    Query.Edit;
    Query.FieldByName('name').AsString := 'xxx';
    Query.Post;

    Query.Refresh;

    CheckEquals(1, Query.RecordCount);
    CheckEquals('xxx', Query.FieldByName('name').AsString);

    Query.Close;

    Query.SQL.Text := 'update table840608 set name=''PARACETAMOL (GPO)''';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #844415

  TZSQLStrings.RebuildAll makes invalid SQL req.
}
procedure TZTestCompMySQLBugReport.Test844415;
var
  Query: TZQuery;
  SQLProcessor: TZSQLProcessor;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'update people set p_name="\"\"\\" where 1=0';
    Query.ExecSQL;
  finally
    Query.Free;
  end;

  SQLProcessor := TZSQLProcessor.Create(nil);
  try
    SQLProcessor.Connection := Connection;
    SQLProcessor.Script.Text := 'update people set p_name="\"\"\\" where 1=0;';
    SQLProcessor.Execute;
  finally
    SQLProcessor.Free;
  end;
end;

{**
  Runs a test for bug report #849723

  Update bug on time field bug.
}
procedure TZTestCompMySQLBugReport.Test849723;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'UPDATE table849723 SET fld2=''abc''';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT fld1, fld2 FROM table849723';
    // Query.RequestLive := True;
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(False, Query.Fields[0].IsNull);
    CheckEquals(0, Query.Fields[0].AsDateTime, 0);
    CheckEquals('abc', Query.Fields[1].AsString);

    Query.Edit;
    Query.Fields[1].AsString := 'xyz';
    Query.Post;

    Query.Refresh;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(False, Query.Fields[0].IsNull);
    CheckEquals(0, Query.Fields[0].AsDateTime, 0);
    CheckEquals('xyz', Query.Fields[1].AsString);
    Query.Close;

    Query.SQL.Text := 'UPDATE table849723 SET fld2=''abc''';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #833489
   AutoCommit=FALSE starting a transaction causing an error
}
procedure TZTestCompMySQLBugReport.Test833489;
begin
  Connection.Disconnect;
  Connection.AutoCommit := False;
  Connection.Connect;
end;

{**
   Runs a test for bug report #869609
   Wrong behaviour of AutoIncremented Fields in MySQL.
}
procedure TZTestCompMySQLBugReport.Test869609;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM table869609';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT id FROM table869609';
    // Query.RequestLive := True;
    Query.Open;
    CheckEquals(0, Query.RecordCount);
    CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[0].DataType));

    Query.Append;
    CheckEquals(True, Query.Fields[0].IsNull);
    Query.Post;

    Query.Refresh;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(False, Query.Fields[0].IsNull);
    CheckEquals(True, Query.Fields[0].AsInteger <> 0);
    Query.Close;

    Query.SQL.Text := 'DELETE FROM table869609';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #865564
  Incorrect precision of float fields in MySQL.
}
procedure TZTestCompMySQLBugReport.Test865564;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT fld1, fld2 FROM table865564';
    Query.Open;
    CheckEquals(1, Query.RecordCount);

    CheckEquals(Ord(ftFloat), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftFloat), Ord(Query.Fields[1].DataType));

//    CheckEquals(2, Query.FieldDefs[0].Precision);
//    CheckEquals(4, Query.FieldDefs[1].Precision);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #881634
  Complex select statement returns wrong field types.
}
procedure TZTestCompMySQLBugReport.Test881634;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT idt2, ft2, table881634a.ft1'
      + ' FROM table881634b INNER JOIN table881634a'
      + ' ON (table881634b.ft1 = table881634a.idt1)';
    Query.Open;

    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftString), Ord(Query.Fields[1].DataType));
    CheckEquals(Ord(ftString), Ord(Query.Fields[2].DataType));

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #881634
   Problem in master-detail links with unsigned int keys.
}
procedure TZTestCompMySQLBugReport.Test884135;
var
  MasterQuery: TZQuery;
  MasterDataSource: TDataSource;
  DetailQuery: TZQuery;
begin
  if SkipClosed then Exit;

  MasterQuery := TZQuery.Create(nil);
  MasterDataSource := TDataSource.Create(nil);
  DetailQuery := TZQuery.Create(nil);
  try
    MasterQuery.Connection := Connection;
    MasterQuery.ReadOnly := True;
    MasterQuery.SQL.Text := 'SELECT * FROM table884135a';
    MasterDataSource.DataSet := MasterQuery;

    DetailQuery.Connection := Connection;
    // DetailQuery.RequestLive := True;
    DetailQuery.CachedUpdates := True;
    DetailQuery.SQL.Text := 'SELECT * FROM table884135b';
    DetailQuery.MasterSource := MasterDataSource;
    DetailQuery.MasterFields := 'id';
    DetailQuery.IndexFieldNames := 'mid';
    DetailQuery.Options := DetailQuery.Options + [doAlwaysDetailResync];

    MasterQuery.Open;
    DetailQuery.Open;

    DetailQuery.Insert;
    DetailQuery.Fields[0].AsInteger := 12345;
    DetailQuery.Post;

    DetailQuery.Close;
    MasterQuery.Close;
  finally
    DetailQuery.Free;
    MasterDataSource.Free;
    MasterQuery.Free;
  end;
end;

{**
  Runs a test for bug report #886841
  Error in processing for default values for columns
  with type enum(y,n) in MySQL.
}
procedure TZTestCompMySQLBugReport.Test886841;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM table886841';
    Query.ExecSQL;

    // Query.RequestLive := True;
    Query.SQL.Text := 'SELECT * FROM table886841';
    Query.Open;

    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[0].DataType));
    Query.Append;
    Query.Post;
    CheckEquals(True, Query.Fields[0].AsBoolean);

    Query.Close;

    Query.SQL.Text := 'DELETE FROM table886841';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #894367
  Wrong processing of queries with non-unique field names.
}
procedure TZTestCompMySQLBugReport.Test894367;
var
  Query: TZQuery;
begin
//  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT a.fld1, b.fld2, 1 + 2 as fld2, a.fld2,'
      + ' c.fld1, b.fld1, c.fld2, ''xyz'' as fld1'
      + ' FROM table894367a as a, table894367b as b, table894367c as c';
    Query.Open;

    CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftFloat), Ord(Query.Fields[1].DataType));
    CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[2].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[3].DataType));
    CheckEquals(Ord(ftBlob), Ord(Query.Fields[4].DataType));
    CheckEquals(Ord(ftInteger), Ord(Query.Fields[5].DataType));
    CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[6].DataType));
    CheckEquals(Ord(ftString), Ord(Query.Fields[7].DataType));

    Query.Close;

    Query.SQL.Text := 'SELECT a.*, 1 + 2 as fld2, b.*,'
      + ' c.fld1, c.fld2, ''xyz'' as fld1'
      + ' FROM table894367a as a, table894367b as b, table894367c as c';
    Query.Open;

    CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[1].DataType));
    CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[2].DataType));
    CheckEquals(Ord(ftInteger), Ord(Query.Fields[3].DataType));
    CheckEquals(Ord(ftFloat), Ord(Query.Fields[4].DataType));
    CheckEquals(Ord(ftBlob), Ord(Query.Fields[5].DataType));
    CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[6].DataType));
    CheckEquals(Ord(ftString), Ord(Query.Fields[7].DataType));

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #914436
  Bug in ZDbcMySqlUtils-TEXT Fields aren't correct identified.
}
procedure TZTestCompMySQLBugReport.Test914436;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
{
    Query.SQL.Text := 'SELECT fld1, fld2 FROM table914436';
    Query.Open;

    CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftString), Ord(Query.Fields[1].DataType));

    Query.Close;
}
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #920589
  Bug in method TZGenericSQLQuoteState.NextToken.
}
procedure TZTestCompMySQLBugReport.Test920589;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;

    Query.SQL.Text := 'SELECT "aa\"aa"';
    Query.Open;

    CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
    CheckEquals('aa"aa', Query.Fields[0].AsString);

    Query.Close;
    
    Query.SQL.Text := 'insert delayed into log_sql'
      + ' (datum, uzivid, lockid, sqlcommand)'
      + ' values (now(), 3, 11952,'
      + ' "update global set value = \"21.3.2004 18:50:17\"'
      + ' where name = \"DAY_JOB_LAST_RUN\"")';
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #938705.
  ZUpdateSQL params behavior when fieldnames have blank spaces.
}
procedure TZTestCompMySQLBugReport.Test938705;
var
  Query: TZQuery;
  UpdateSql: TZUpdateSQL;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    UpdateSql := TZUpdateSQL.Create(nil);
    try
      Query.Connection := Connection;
      // Query.RequestLive := True;

      Query.SQL.Text := 'delete from `Table 938705`';
      Query.ExecSQL;

      Query.SQL.Text := 'select * from `Table 938705`';
      Query.UpdateObject := UpdateSql;

      UpdateSql.InsertSQL.Text := 'insert into `Table 938705` (`Field First`,'
        + ' `Field Second`) values (:`Field First`, :`Field Second`)';
      UpdateSql.ModifySQL.Text := 'update `Table 938705`'
        + ' set `Field First`=:`Field First`, `Field Second`=:`Field Second`'
        + ' where `Field First`=:`OLD_Field First`';
      UpdateSql.DeleteSQL.Text := 'delete from `Table 938705`'
        + ' where `Field First`=:`OLD_Field First`';

      Query.Open;
      Query.Append;
      Query.FieldByName('Field First').AsInteger := 1;
      Query.FieldByName('Field Second').AsString := 'abc';
      Query.Post;
      Query.Close;

      Query.Open;
      CheckEquals(False, Query.IsEmpty);
      CheckEquals(1, Query.FieldByName('Field First').AsInteger);
      CheckEquals('abc', Query.FieldByName('Field Second').AsString);
      Query.Edit;
      Query.FieldByName('FIELD FIRST').AsInteger := TEST_ROW_ID;
      Query.FieldByName('FIELD SECOND').AsString := 'xyz';
      Query.Post;
      Query.Refresh;
      CheckEquals(TEST_ROW_ID, Query.FieldByName('Field FIRST').AsInteger);
      CheckEquals('xyz', Query.FieldByName('Field SECOND').AsString);
      Query.Delete;
      Query.Close;

      Query.Open;
      CheckEquals(True, Query.IsEmpty);

      Query.SQL.Text := 'delete from `Table 938705`';
      Query.ExecSQL;
    finally
      UpdateSql.Free;
    end;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #952976.
  ExecSQL error doesn't raise exception with MySQL 4.1.
}
procedure TZTestCompMySQLBugReport.Test952976;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;

    Query.SQL.Text := 'INSERT INTO people(bad_column) VALUES(''abc'')';
    try
      Query.ExecSQL;

      Fail('Wrong exception handling in MySQL');
    except
    end;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #957126.
  Incorrect processing of empty strings in default values in MySQL driver.
}
procedure TZTestCompMySQLBugReport.Test957126;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM table957126';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT fld1, fld2 FROM table957126';
    // Query.RequestLive := True;
    Query.Open;
    CheckEquals(0, Query.RecordCount);

    Query.Append;
    CheckEquals(True, Query.Fields[0].IsNull);
    CheckEquals(True, Query.Fields[1].IsNull);
    Query.Post;

    Query.Refresh;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(False, Query.Fields[0].IsNull);
    CheckEquals('', Query.Fields[0].AsString);
    CheckEquals(True, Query.Fields[1].IsNull);
    Query.Close;

    Query.SQL.Text := 'DELETE FROM table957126';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Tests the bug report #961337
  ENUM('Y','N') is not recognized as Boolean when column name is renamed.
}
procedure TZTestCompMySQLBugReport.Test961337;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT id, fld1, fld2, fld1 as fld3,'
      + ' fld2 as fld4 FROM table735299';
    // Query.RequestLive := True;
    Query.Open;
    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[1].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[2].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[3].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[4].DataType));
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Tests the bug report #981208
  SELECT * FROM mydb.mytable is not updateable.
}
procedure TZTestCompMySQLBugReport.Test981208;
var
  Query: TZQuery;
  DatabaseName: string;
begin
  if SkipClosed then Exit;

  Connection.Disconnect;
  DatabaseName := Connection.Database;
  Connection.Database := 'mysql';
  Connection.Connect;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM ' + DatabaseName + '.table740899';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM ' + DatabaseName + '.table740899';
    // Query.RequestLive := True;
    Query.Open;

    Query.Append;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsString := 'abc';
    Query.Post;

    Query.Refresh;
    CheckEquals(1, Query.Fields[0].AsInteger);
    CheckEquals('abc', Query.Fields[1].AsString);

    Query.Edit;
    Query.Fields[0].AsInteger := 2;
    Query.Fields[1].AsString := 'xyz';
    Query.Post;

    Query.Close;
    Query.Open;
    CheckEquals(2, Query.Fields[0].AsInteger);
    CheckEquals('xyz', Query.Fields[1].AsString);

    Query.Delete;

    Query.Close;

    Query.SQL.Text := 'DELETE FROM ' + DatabaseName + '.table740899';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Tests the bug report #987022
  LargeInt fields are not filtered by Dataset.Filter.
}
procedure TZTestCompMySQLBugReport.Test987022;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT fld1, fld2 FROM table987022 WHERE 1=0';
    // Query.RequestLive := True;
    Query.CachedUpdates := True;

    Query.Open;
    CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftString), Ord(Query.Fields[1].DataType));

    Query.Append;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsString := 'aaa';
    Query.Post;

    Query.Append;
    TLargeIntField(Query.Fields[0]).AsLargeInt := Int64(MaxInt) + 1;
    Query.Fields[1].AsString := 'bbb';
    Query.Post;

    Query.Append;
    Query.Fields[0].AsInteger := 2;
    Query.Fields[1].AsString := 'ccc';
    Query.Post;

    Query.Append;
    TLargeIntField(Query.Fields[0]).AsLargeInt := Int64(MaxInt) + 2;
    Query.Fields[1].AsString := 'ddd';
    Query.Post;

    CheckEquals(4, Query.RecordCount);

    Query.Filter := 'fld1 > 100';
    Query.Filtered := True;
    CheckEquals(2, Query.RecordCount);

    Query.Filter := 'fld1 < ' + IntToStr(MaxInt);
    CheckEquals(2, Query.RecordCount);

    Query.Filter := 'fld1 > ' + IntToStr(MaxInt) + ' OR fld2 = ''aaa''';
    CheckEquals(3, Query.RecordCount);

    Query.Filtered := False;
    CheckEquals(4, Query.RecordCount);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Tests the bug report #989474
  UpdateSQL doesn't process columns with duplicated names in queries like:
  SELECT T1.MyField, T2.MyField...
}
procedure TZTestCompMySQLBugReport.Test989474;
var
  Query: TZQuery;
  UpdateSQL: TZUpdateSQL;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  UpdateSQL := TZUpdateSQL.Create(nil);
  try
    UpdateSQL.InsertSQL.Text :=
      'INSERT INTO table989474 VALUES(:CustID_1,:CreateDate_1)';

    Query.Connection := Connection;
    Query.UpdateObject := UpdateSQL;
    // Query.RequestLive := True;

    Query.SQL.Text := 'DELETE FROM table989474';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT A.CustID, A.CreateDate, B.CustID, B.CreateDate'
      + ' FROM table989474 A INNER JOIN table989474 B ON A.CustID=B.CustID';
    Query.Open;

    CheckEquals('CustID', Query.Fields[0].FieldName);
    CheckEquals('CreateDate', Query.Fields[1].FieldName);
    CheckEquals('CustID_1', Query.Fields[2].FieldName);
    CheckEquals('CreateDate_1', Query.Fields[3].FieldName);

    Query.Append;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsDateTime := EncodeDate(1949, 07, 06);
    Query.Fields[2].AsInteger := 2;
    Query.Fields[3].AsDateTime := EncodeDate(1975, 04, 08);
    Query.Post;

    Query.Close;
    Query.Open;

    CheckEquals(1, Query.RecordCount);
    CheckEquals(2, Query.Fields[0].AsInteger);
    CheckEquals(EncodeDate(1975, 04, 08), Query.Fields[1].AsDateTime);
    CheckEquals(2, Query.Fields[2].AsInteger);
    CheckEquals(EncodeDate(1975, 04, 08), Query.Fields[3].AsDateTime);

    Query.Close;
  finally
    Query.Free;
    UpdateSQL.Free;
  end;
end;

{**
  Tests the bug report #1045286
  Improper IsNull method for Text columns.
}
procedure TZTestCompMySQLBugReport.Test1045286;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT fld FROM table1045286';

    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(Ord(ftMemo), Ord(Query.Fields[0].DataType));
    CheckEquals('', Query.Fields[0].AsString);
    CheckEquals(False, Query.Fields[0].IsNull);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Tests the bug report #1023149
  Incorrect processing '\' characters in text fields.
}
procedure TZTestCompMySQLBugReport.Test1023149;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    // Query.RequestLive := True;
    Query.CachedUpdates := False;
    Query.Connection := Connection;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM blob_values WHERE b_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO blob_values (b_id, b_text) VALUES (:id, ''c:\\test.jpg'')';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Opens a result set. }
    Query.SQL.Text := 'SELECT * FROM blob_values WHERE b_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.Open;
    CheckEquals(TEST_ROW_ID, Query.FieldByName('b_id').AsInteger);
    CheckEquals('c:\test.jpg', Query.FieldByName('b_text').AsString);

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM blob_values WHERE b_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

initialization
  RegisterTest('bugreport',TZTestCompMySQLBugReport.Suite);
end.
