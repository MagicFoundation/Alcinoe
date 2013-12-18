{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Cases for Interbase Component Bug Reports    }
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

unit ZTestBugCompInterbase;

interface

{$I ZBugReport.inc}

uses
  Classes, SysUtils, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDataset, ZConnection, ZDbcIntfs, ZBugReport,
  {$IFNDEF LINUX}
    DBCtrls,
  {$ENDIF}
  ZCompatibility;
type

  {** Implements a bug report test case for Interbase components. }
  ZTestCompInterbaseBugReport = class(TZSpecificSQLBugReportTestCase)
  private
    FConnection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: TZConnection read FConnection write FConnection;

  published
    procedure Test750912;
    procedure Test789879;
    procedure Test833489;
    procedure Test841559;
    procedure Test843655;
    procedure Test864622;
    procedure Test886194;
    procedure Test886854;
    procedure Test897631;
    procedure Test909181;
    procedure Test984305;
    procedure Test1004584;
    procedure Test1021705;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZTestCase, ZTestConsts, ZSqlUpdate, ZSqlTestCase;

{ ZTestCompInterbaseBugReport }

function ZTestCompInterbaseBugReport.GetSupportedProtocols: string;
begin
  Result := 'interbase-5,interbase-6,firebird-1.0,firebird-1.5,firebird-2.0,firebird-2.1,firebirdd-1.5,firebirdd-2.0,firebirdd-2.1';
end;

procedure ZTestCompInterbaseBugReport.SetUp;
begin
  Connection := CreateDatasetConnection;
end;

procedure ZTestCompInterbaseBugReport.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

{**
   Test for Bug#1004584 - problem start transaction in non autocommit mode
}
procedure ZTestCompInterbaseBugReport.Test1004584;
begin
  if SkipClosed then Exit;

  CheckEquals(Ord(tiNone), Ord(Connection.TransactIsolationLevel));
  Connection.Disconnect;
  Connection.AutoCommit := False;
  Connection.TransactIsolationLevel := tiSerializable;
  try
    Connection.StartTransaction;
    Fail('StartTransaction should be allowed only in AutoCommit mode');
  except
    // Ignore.
  end;
  Connection.Disconnect;
end;

{**
   Test for Bug#1021705 - problem start transaction in non autocommit mode
}
procedure ZTestCompInterbaseBugReport.Test1021705;
var
  Error: Boolean;
  Query: TZQuery;
begin
  Error := True;
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.SQL.Text := 'SELECT * FROM TABLE1021705';
    Query.OPEN;

    Query.Append;
    Query.FieldByName('ID').AsFloat := 1;
    Query.FieldByName('FLD1').AsFloat := 34257.346;
    Query.FieldByName('FLD2').AsFloat := 2.387;
    Query.Post;
    Query.Close;

    Query.Open;
    CheckEquals(1, Query.FieldByName('ID').AsInteger);
    CheckEquals(34257.346, Query.FieldByName('FLD1').AsFloat, 0.0001);
    CheckEquals(2.387, Query.FieldByName('FLD2').AsFloat, 0.0001);
    Query.Delete;
    Query.Close;

    try
      Query.Append;
      Query.FieldByName('ID').AsFloat := 1;
      Query.FieldByName('FLD1').AsFloat := 2.387;
      Query.FieldByName('FLD2').AsFloat := 34257.346;
      Query.Post;
    except
      Error := False
    end;

    if Error then
      Fail('Inserted wrong value 34257.346 into TABLE1021705.FLD2 ');

  finally
    Query.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.Test750912;
{$IFNDEF LINUX}
var
  Query: TZQuery;
  ROQuery: TZReadOnlyQuery;
  DSQuery, DSROQuery: TDataSource;
  LookUp: TDBLookupComboBox;
{$ENDIF}
begin
{$IFNDEF LINUX}
  Query := TZQuery.Create(nil);
  ROQuery := TZReadOnlyQuery.Create(nil);;
  DSQuery := TDataSource.Create(nil);
  DSROQuery := TDataSource.Create(nil);
  LookUp := TDBLookupComboBox.Create(nil);;
  try
    Query.Connection := Connection;
    ROQuery.Connection := Connection;
    Query.SQL.Text := 'select * from people';
    ROQuery.SQL.Text := 'select * from department';
    DSQuery.DataSet := Query;
    DSROQuery.DataSet := ROQuery;
    LookUp := TDBLookupComboBox.Create(nil);
    LookUp.DataSource := DSQuery;
    LookUp.ListSource := DSROQuery;
    LookUp.DataField := 'p_dep_id';
    LookUp.KeyField := 'dep_id';
    LookUp.ListField := 'dep_name';
    Query.Open;
    ROQuery.Open;

    Query.First;
    CheckEquals(1, Query.FieldByName('p_id').AsInteger);
    CheckEquals(1, Query.FieldByName('p_dep_id').AsInteger);
    CheckEquals('Vasia Pupkin', Query.FieldByName('p_name').AsString);
    CheckEquals('Line agency', LookUp.Text);
    {$IFNDEF FPC}
    CheckEquals(1, LookUp.KeyValue);
    {$ENDIF}

    Query.Next;
    CheckEquals(2, Query.FieldByName('p_id').AsInteger);
    CheckEquals(2, Query.FieldByName('p_dep_id').AsInteger);
    CheckEquals('Andy Karto', Query.FieldByName('p_name').AsString);
    CheckEquals('Container agency', LookUp.Text);
    {$IFNDEF FPC}
    CheckEquals(2, LookUp.KeyValue);
    {$ENDIF}
  finally
    LookUp.Free;
    Query.Free;
    ROQuery.Free;
    DSQuery.Free;
    DSROQuery.Free;
  end;
{$ENDIF}
end;

{**
  Float->Numeric problem
}
procedure ZTestCompInterbaseBugReport.Test789879;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM TABLE789879';
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO TABLE789879(FLD) VALUES (:Anum_value)';
    Query.ParamByName('Anum_value').AsFloat := 1.14;
    Query.ExecSQL;

    Query.Sql.Text := 'SELECT * FROM TABLE789879';
    Query.Open;
    CheckEquals(1.14, Query.Fields[0].AsFloat, 0.001);
    Query.Close;
  finally
    Query.Free;
  end;  
end;

{**
   Runs a test for bug report #833489
   AutoCommit=FALSE starting a transaction causing an error
}
procedure ZTestCompInterbaseBugReport.Test833489;
begin
  Connection.Disconnect;
  Connection.AutoCommit := False;
  Connection.Connect;
end;

{**
   Runs a test for bug report #833489
   Can't show messages from triggers  
}
procedure ZTestCompInterbaseBugReport.Test841559;
var
  Temp: boolean;
  Query: TZQuery;
begin
  Temp := False;
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM TABLE841559';
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO TABLE841559 (FLD1, FLD2) VALUES (:FLD1, :FLD2)';
    Query.ParamByName('FLD1').AsInteger := 1;
    Query.ParamByName('FLD2').AsString := '';
    Query.ExecSQL;
  except
    Temp := True;
  end;
  Query.Free;
  CheckEquals(True, Temp, 'Just exception EXCEPTION841559');
end;

{**
   Runs a test for bug report #843655
   Blob fields don't updates
}
procedure ZTestCompInterbaseBugReport.Test843655;
var
  Query: TZQuery;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TMemoryStream;
begin
  Query := TZQuery.Create(nil);

  Query.Connection := Connection;
  // Query.RequestLive := True;
  Query.SQL.Text := 'DELETE FROM BLOB_VALUES';
  Query.ExecSQL;

  BinStream := TMemoryStream.Create;
  StrStream := TMemoryStream.Create;
  BinStream1 := TMemoryStream.Create;
  StrStream1 := TMemoryStream.Create;

  try
    { load data to the stream }
    BinStream.LoadFromFile('../../../database/images/dogs.jpg');
    BinStream.Size := 1024;
    StrStream.LoadFromFile('../../../database/text/lgpl.txt');
    StrStream.Size := 1024;
    { post empty row }
    Query.SQL.Text := 'SELECT * FROM BLOB_VALUES';
    Query.Open;
    Query.Append;
    Query.FieldByName('B_ID').AsInteger := 1;
    Query.Post;
    Query.Close;
    { update data }
    Query.Open;
    Query.Edit;
    (Query.FieldByName('B_TEXT') as TBlobField).LoadFromStream(StrStream);
    (Query.FieldByName('B_IMAGE') as TBlobField).LoadFromStream(BinStream);
    Query.Post;
    Query.Close;
    { check that data updated }
    Query.Open;
    (Query.FieldByName('B_TEXT') as TBlobField).SaveToStream(StrStream1);
    (Query.FieldByName('B_IMAGE') as TBlobField).SaveToStream(BinStream1);
    CheckEquals(StrStream, StrStream1);
    CheckEquals(BinStream, BinStream1);
    Query.Close;
  finally
    BinStream.Free;
    StrStream.Free;
    BinStream1.Free;
    StrStream1.Free;
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #865441
   ZeosLib reports Ex. numeric(3,1) as IntegerField
}
procedure ZTestCompInterbaseBugReport.Test864622;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  // Query.RequestLive := True;
  try
    Query.SQL.Text := 'SELECT * FROM TABLE864622';
    Query.Open;
    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftFloat), Ord(Query.Fields[1].DataType));
    CheckEquals(1, Query.Fields[0].AsInteger);
    CheckEquals(1.2, Query.Fields[1].AsFloat, 0.01);
  finally
    Query.Free;
  end;
end;

{
  Runs a test for bug report #886194
  ARITHMETIC EXCEPTION, NUMERIC OVERFLOW, OR STRING TRUNCATION
}
procedure ZTestCompInterbaseBugReport.Test886194;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  // Query.RequestLive := True;
  try
    Query.SQL.Text := 'DELETE FROM TABLE886194';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM TABLE886194';
    Query.Open;
    Query.Append;
    Query.Fields[0].AsString := 'ABCDEFG';
    Query.Fields[1].AsString := 'KLMNOPQ';
    Query.Post;
  finally
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #886854
   Incorrect field type
}
procedure ZTestCompInterbaseBugReport.Test886854;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  // Query.RequestLive := True;
  try
    Query.SQL.Text := 'select rc.rdb$relation_name as rel_name, ' +
    'rc.rdb$index_name as ind_name, rs.rdb$field_name as field_name, ' +
    'rs.rdb$field_position as field_pos from rdb$relation_constraints rc ' +
    'left join rdb$index_segments rs on rs.rdb$index_name=rc. '+
    'rdb$index_name where rs.rdb$field_name is not null and rs. '+
    'rdb$field_name<>''DEP_ID'' and '+
    'rc.rdb$constraint_type=''PRIMARY KEY'' and rc.rdb$relation_name=''PEOPLE'' ' +
    'order by rc.rdb$relation_name';
    Query.Open;
    CheckEquals(ord(ftString), ord(Query.Fields[0].DataType));
    CheckEquals(ord(ftWideString), ord(Query.Fields[1].DataType));
    CheckEquals(ord(ftWideString), ord(Query.Fields[2].DataType));
    CheckEquals(ord(ftSmallint), ord(Query.Fields[3].DataType));

    CheckEquals('PEOPLE', Query.Fields[0].AsString);
    CheckEquals(Copy('RDB$PRIMARY2598', 1, Length('RDB$PRIMARY')),
      Copy(Query.Fields[1].AsString, 1, Length('RDB$PRIMARY')));
    CheckEquals('P_ID', Query.Fields[2].AsString);
    CheckEquals(0, Query.Fields[3].AsInteger);
  finally
    Query.Free;
  end;

end;

{**
    Numeric 15.2 field which displays as integer
}
procedure ZTestCompInterbaseBugReport.Test897631;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  // Query.RequestLive := True;

  try
    Query.SQL.Text := 'DELETE FROM TABLE897631';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO TABLE897631 VALUES (179.22)';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM TABLE897631';
    Query.Open;
    with Query do
    begin
      CheckEquals(179.22, Fields[0].AsFloat, 0.001);
      Close;
    end;
  finally
    Query.Free;
  end;  
end;

procedure ZTestCompInterbaseBugReport.Test909181;
var
  Error: boolean;
  Query: TZQuery;
  UpdateSQL: TZUpdateSQL;
begin
  Error := True;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  // Query.RequestLive := True;
  UpdateSQL := TZUpdateSQL.Create(nil);

  try
    Query.SQL.Text := 'DELETE FROM TABLE909181';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO TABLE909181 VALUES (''A'', 10, ''2004-03-11'')';
    Query.ExecSQL;

    Query.UpdateObject := UpdateSQL;
    UpdateSQL.ModifySQL.Text := ' UPDATE TABLE909181 SET' +
      ' FLD1 = :FLD1,' +
      ' FLD2 = :FLD2,' +
      ' FLD3 = :FLD3'+
      ' WHERE FLD1 = :OLD_FLD1';

    Query.SQL.Text := 'SELECT * FROM TABLE909181';
    // Query.RequestLive := True;
    Query.Open;
    with Query do
    begin
      Edit;
      Fields[0].Value := Null;
      Fields[1].Value := Null;
      Fields[2].Value := Null;
      Post;
      Close;
    end;
  except
    Error := False;
  end;
  CheckEquals(False, Error, 'Problems with set Null prametrs in SQLDA');
  UpdateSQL.Free;
  Query.Free;
end;

{**
    Test for Bug#984305
}
procedure ZTestCompInterbaseBugReport.Test984305;
var
  Error: boolean;
  Query: TZQuery;
begin
  Error := True;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;

  try
    Query.SQL.Text := 'SELECT * FROM PEOPLE';
    Query.Open;
    Connection.Disconnect;
    CheckEquals(False, Connection.Connected);
    CheckEquals(False, Query.Active);

    try
      Connection.User := '';
      Connection.Connect;
    except
      Error := False;
    end;
    CheckEquals(False, Error, 'Problems with change user name');

  finally
    Query.Free;
  end;
end;

initialization
  RegisterTest('bugreport',ZTestCompInterbaseBugReport.Suite);
end.
