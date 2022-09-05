{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Cases for Generic Component Bug Reports      }
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

unit ZTestBugCompCore;

interface

{$I ZBugReport.inc}

uses
  Classes, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDataset, ZConnection, ZDbcIntfs, ZBugReport,
  ZCompatibility, ZSqlUpdate, ZSqlProcessor, ZSqlMetadata;

type

  {** Implements a bug report test case for core components. }
  ZTestCompCoreBugReport = class(TZPortableSQLBugReportTestCase)
  private
    FConnection: TZConnection;
    FUpdateCounter: Integer;
    FErrorCounter: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    property Connection: TZConnection read FConnection write FConnection;

  public
    procedure DataSetCalcFields(Dataset: TDataSet);
    procedure DataSetBeforeScroll(Dataset: TDataSet);
    procedure DataSetAfterScroll(Dataset: TDataSet);
    procedure DataSetOnError(DataSet: TDataSet;
      E: EDatabaseError; var Action: TDataAction);
  published
    procedure Test707309;
    procedure Test707364;
    procedure Test000001;
    procedure Test000002;
    procedure Test000003;
    procedure Test715099;
    procedure Test722651;
    procedure Test773022;
    procedure Test772926;
    procedure Test793351;
    procedure Test804323;
    procedure Test804640;
    procedure Test802548;
    procedure Test795641;
    procedure Test826886;
    procedure Test000004;
    procedure Test832467;
    procedure Test830804;
    procedure Test833197;
    procedure Test834798;
    procedure Test839540;
    procedure Test840218;
    procedure Test842678;
    procedure Test846377;
    procedure Test880459;
    procedure Test000005;
    procedure Test887103;
    procedure Test919401;
    procedure Test926264;
    procedure Test953557;
    procedure Test966267;
    procedure Test985629;
    procedure TestFloatPrecision;
    procedure Test995080;
    procedure Test996283;
    procedure Test1004534;
    procedure Test1012751;
    procedure Test1022415;
    procedure Test1049821;
    procedure Test1045500;
    procedure Test1036916;
    procedure Test1004584;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  SysUtils, ZSysUtils, ZTestConsts, ZAbstractRODataset, ZDbcCache;

{ ZTestCompCoreBugReport }

procedure ZTestCompCoreBugReport.SetUp;
begin
  Connection := CreateDatasetConnection;
end;

procedure ZTestCompCoreBugReport.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Assignes values to dataset calculated fields.
  @param DataSet a database object.
}
procedure ZTestCompCoreBugReport.DataSetCalcFields(Dataset: TDataSet);
begin
  Dataset.FieldByName('p_calc').AsInteger := Dataset.RecNo + 100;
end;

{**
  Performs an event after dataset is scrolled.
  @param DataSet a database object.
}
procedure ZTestCompCoreBugReport.DataSetAfterScroll(Dataset: TDataSet);
begin
  FUpdateCounter := FUpdateCounter + 100;
end;

{**
  Performs an event before dataset is scrolled.
  @param DataSet a database object.
}
procedure ZTestCompCoreBugReport.DataSetBeforeScroll(Dataset: TDataSet);
begin
  FUpdateCounter := FUpdateCounter + 1;
end;

{**
  Performs an event when some error occures.
  @param DataSet a dataset object.
  @param E a database error.
  @param Action an action to be performed.
}
procedure ZTestCompCoreBugReport.DataSetOnError(DataSet: TDataSet;
  E: EDatabaseError; var Action: TDataAction);
begin
  FErrorCounter := FErrorCounter + 1;
  Action := daAbort;
end;

{**
  Bugs report #000002.

  The program will crash with access violation at the line
  if Length(FieldIndices) = 1 then
    KeyValues := KeyValues[0];
}
procedure ZTestCompCoreBugReport.Test000002;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    CheckEquals(False, Query.CachedUpdates);
    // Query.RequestLive := True;
    Query.Connection := Connection;

    Query.SQL.Text := 'SELECT eq_id FROM equipment';
    Query.Open;
    Check(Query.RecordCount > 0);
    Query.Refresh;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #707309.

  Non-Cached mode doesn't post updates
}
procedure ZTestCompCoreBugReport.Test000001;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    CheckEquals(False, Query.CachedUpdates);
    // Query.RequestLive := True;
    Query.Connection := Connection;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Insert a new record }
    Query.SQL.Text := 'SELECT * FROM people';
    Query.Open;
    Query.Append;
//    Query.FieldByName('p_id').AsInteger := TEST_ROW_ID;
//    Query.Post;
    Query.Close;
(*
    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
    CheckEquals(1, Query.RowsAffected);
*)
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #707309.

  Query.Param.LoadFromStream
}
procedure ZTestCompCoreBugReport.Test707309;
var
  Query: TZQuery;
  TextStream: TMemoryStream;
  BinaryStream: TMemoryStream;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);

  TextStream := TMemoryStream.Create();
  TextStream.LoadFromFile('../../../database/text/gnu.txt');
  TextStream.Position := 0;
  TextStream.Size := 1024;

  BinaryStream := TMemoryStream.Create();
  BinaryStream.LoadFromFile('../../../database/images/coffee.bmp');
  BinaryStream.Position := 0;
  BinaryStream.Size := 1024;

  try
    Query.Connection := Connection;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Creates a record }
    Query.SQL.Text := 'INSERT INTO people (p_id, p_resume, p_picture)'
      + ' VALUES(:id, :resume, :picture)';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ParamByName('resume').LoadFromStream(TextStream, ftMemo);
    Query.ParamByName('picture').LoadFromStream(BinaryStream, ftBlob);
    Query.ExecSQL;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
  finally
    TextStream.Free;
    BinaryStream.Free;
    Query.Free;
  end;
end;

{**
  Bugs report #707364.

  Query.Param.LoadFromStream
}
procedure ZTestCompCoreBugReport.Test707364;
var
  Processor: TZSQLProcessor;
begin
  if SkipClosed then Exit;

  Processor := TZSQLProcessor.Create(nil);
  try
    Processor.Connection := Connection;
    Processor.Script.Text := 'AAAAAAAAAAAA BBBBBBBBBBBBBBB CCCCCCCCCCCCCC';
    try
      Processor.Execute;
      Fail('SQL Processor must throw exception on invalid script.');
    except
      Check(True);
    end;
  finally
    Processor.Free;
  end;
end;

{**
  Bugs report #707364.
  findnext doesnt work in dbo 6.0.6 beta
  hi, the following code:

  procedure TFrMain.loadTables;
  var
    i:Integer;
  begin
    i:=1;
    with dmMain.qrSelSeeFreight do begin
      open;
      repeat
        sgSeefracht.RowCount:=i+1;
        sgSeefracht.Cells[0,i]:=fieldByName('caloadid').AsString;
        ...
        inc(i);
      until not findnext;
      close;
    end;
  end;
}
procedure ZTestCompCoreBugReport.Test715099;
var
  Query: TZQuery;
  RecNo: Integer;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT * FROM people';
    Query.Open;
    RecNo := 0;

    repeat
      Inc(RecNo);
      CheckEquals(Query.RecNo, RecNo);
    until not Query.FindNext;

    CheckEquals(Query.RecordCount, RecNo);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #722651.

  There are error with DateTime convertion when field is
  null since ZeosDBO 6.0.7. ( when you added GetChar and SetChar )
}
procedure ZTestCompCoreBugReport.Test722651;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    CheckEquals(False, Query.CachedUpdates);
    // Query.RequestLive := True;
    Query.Connection := Connection;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM date_values WHERE d_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Insert a new record }
    Query.SQL.Text := 'INSERT INTO date_values(d_id) VALUES (:id)';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
    CheckEquals(1, Query.RowsAffected);

    { Check for null fields }
    Query.SQL.Text := 'SELECT * FROM date_values WHERE d_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.Open;
    Check(Query.FieldByName('d_date').IsNull);
    Check(Query.FieldByName('d_time').IsNull);
    Check(Query.FieldByName('d_datetime').IsNull);
  //  Check(Query.FieldByName('d_timestamp').IsNull);
    Query.Close;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM date_values WHERE d_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
    CheckEquals(1, Query.RowsAffected);
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #000003.

  Problem related with quoted SQL parameters in TZQuery and TZUpdateSQL.
}
procedure ZTestCompCoreBugReport.Test000003;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;

    Query.SQL.Text := 'SELECT * FROM Table'
      + ' WHERE F1=:P_1 AND F2=:"P 2" AND F3=:''P 3''';
    CheckEquals(3, Query.Params.Count);
    CheckEquals('P_1', Query.Params[0].Name);
    CheckEquals('P 2', Query.Params[1].Name);
    CheckEquals('P 3', Query.Params[2].Name);
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #773022.

  RecordCount is not changed after Dataset modification
}
procedure ZTestCompCoreBugReport.Test773022;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.CachedUpdates := True;
    Query.SQL.Text := 'select * from people where 1=0';
    Query.Open;

    CheckEquals(0, Query.RecordCount);

    Query.Append;
    CheckEquals(0, Query.RecordCount);
    Query.Cancel;
    CheckEquals(0, Query.RecordCount);

    Query.Append;
    Query.FieldByName('p_id').AsInteger := 0;
    Query.Post;
    CheckEquals(1, Query.RecordCount);
    Query.Append;
    Query.FieldByName('p_id').AsInteger := 0;
    Query.Post;
    CheckEquals(2, Query.RecordCount);

    Query.Edit;
    CheckEquals(0, Query.FieldByName('p_id').AsInteger);
    Query.FieldByName('p_id').AsInteger := 123;
    CheckEquals(123, Query.FieldByName('p_id').AsInteger);
    Query.Cancel;
    CheckEquals(0, Query.FieldByName('p_id').AsInteger);
    CheckEquals(2, Query.RecordCount);

    Query.Delete;
    CheckEquals(1, Query.RecordCount);
    Query.Delete;
    CheckEquals(0, Query.RecordCount);
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #772926.

  I've noticed that when entering invalid info into a field,
  then updating the database you get an error message,
  after this cancel the error and then press the Esc key,
  the invalid data stays in the field. I would assume it
  should revert back to the original data.
}
procedure ZTestCompCoreBugReport.Test772926;
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
    Query.SQL.Text := 'DELETE FROM people WHERE p_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO people (p_id) VALUES (:id)';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.ExecSQL;
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Opens a result set. }
    Query.SQL.Text := 'SELECT * FROM people WHERE p_id>=:id ORDER BY p_id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.Open;
    CheckEquals(TEST_ROW_ID - 1, Query.FieldByName('p_id').AsInteger);
    Query.Next;
    CheckEquals(TEST_ROW_ID, Query.FieldByName('p_id').AsInteger);

    { Check for rollback. }
    try
      Query.Edit;
      Query.FieldByName('p_id').AsInteger := TEST_ROW_ID - 1;
      CheckEquals(TEST_ROW_ID - 1, Query.FieldByName('p_id').AsInteger);
      Query.Post;
      Fail('Wrong behaviour with duplicated key.');
    except
      CheckEquals(TEST_ROW_ID - 1, Query.FieldByName('p_id').AsInteger);
      Query.Cancel;
      CheckEquals(TEST_ROW_ID, Query.FieldByName('p_id').AsInteger);
    end;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #772926.

  zquery1 has a working connection, fielddefs, and SQL
  statement, but zquery1.state=dsInactive (not opened).
  zquery1.fieldbyname('field1').asstring:='text' causes an
  access violation in ZAbstractRODataset, because it try
  to execute the assign.
  maybe it should check the state and throw an exception.
  (eg can't modify a closed dataset)

  when zquery1 is opened (state=dsBrowse), there is no
  error. ZAbstractRODataset (or else) should check for
  dsBrowse state too, and should throw an other
  exception.
}
procedure ZTestCompCoreBugReport.Test793351;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.ReadOnly := True;
    Query.SQL.Text := 'select p_id from people';
    Query.Open;

    try
      Query.Fields[0].AsInteger := 0;
      Fail('Wrong SetField behaviour');
    except
    end;

    Query.Close;

    // Query.RequestLive := True;
    Query.SQL.Text := 'select p_id from people where 1=0';
    Query.Open;

    try
      Query.Fields[0].AsInteger := 0;
      Fail('Wrong SetField behaviour');
    except
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #804323.

  Calculated fields in read only datasets
  I get the following error, when I work with calculated fields,
  when I want to assign the value to one of those fields: "Operation not
  allowed in a read only dataset."
}
procedure ZTestCompCoreBugReport.Test804323;
var
  I: Integer;
  Query: TZQuery;
  FieldDefs: TFieldDefs;
  CalcField: TField;
begin
//  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    CheckEquals(False, Query.CachedUpdates);
    Query.ReadOnly := True;
    Query.Connection := Connection;
    Query.OnCalcFields := DataSetCalcFields;

    Query.SQL.Text := 'SELECT p_id FROM people';
    FieldDefs := Query.FieldDefs;
    FieldDefs.Update;

    for I := 0 to FieldDefs.Count - 1 do
      FieldDefs[I].CreateField(Query).DataSet := Query;

    CalcField := TIntegerField.Create(nil);
    CalcField.FieldName := 'p_calc';
    CalcField.FieldKind := fkCalculated;
    CalcField.Visible := True;
    CalcField.DataSet := Query;

    Query.Open;
    Query.FieldByName('p_calc').AsInteger;

    while not Query.Eof do
    begin
      CheckEquals(Query.RecNo + 100, Query.FieldByName('p_calc').AsInteger);
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #804640.

  Calculated Fields of type TCurrencyField returns always 0.00.
}
procedure ZTestCompCoreBugReport.Test804640;
var
  I: Integer;
  Query: TZQuery;
  FieldDefs: TFieldDefs;
  CalcField: TField;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  try
    CheckEquals(False, Query.CachedUpdates);
    Query.ReadOnly := True;
    Query.Connection := Connection;
    Query.OnCalcFields := DataSetCalcFields;

    Query.SQL.Text := 'SELECT p_id FROM people';
    FieldDefs := Query.FieldDefs;
    FieldDefs.Update;

    for I := 0 to FieldDefs.Count - 1 do
      FieldDefs[I].CreateField(Query).DataSet := Query;

    CalcField := TCurrencyField.Create(nil);
    CalcField.FieldName := 'p_calc';
    CalcField.FieldKind := fkCalculated;
    CalcField.Visible := True;
    CalcField.DataSet := Query;

    Query.Open;
    while not Query.Eof do
    begin
      Check(Query.FieldByName('p_calc').AsInteger <> 0);
      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #802548.

  When I call the refresh method and the record with the
  pointer has been changed by outside source (e.g. MySQL
  Control Center) the error "List Index out of Bounds
  (-1)" appears. I try remove any DataSources and
  DataControls, keeping Zeos alone and the error occurs
  on same way. I try use ZReadOnlyQuery and ZQuery, with
  ReadOnly set False and True too, the error is same
  in all cases (with MySQL and MsSQL).
}
procedure ZTestCompCoreBugReport.Test802548;
var
  Query: TZQuery;
  RefreshQuery: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  RefreshQuery := TZQuery.Create(nil);
  try
    // Query.RequestLive := True;
    Query.Connection := Connection;

    // RefreshQuery.RequestLive := True;
    RefreshQuery.Connection := Connection;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    RefreshQuery.SQL.Text := 'SELECT * FROM people';
    RefreshQuery.Open;
    RefreshQuery.Last;
    RefreshQuery.Refresh;

    Query.SQL.Text := 'INSERT INTO people(p_id, p_name) VALUES('
      + IntToStr(TEST_ROW_ID) + ', ''abc'')';
    Query.ExecSQL;

    RefreshQuery.Refresh;
    RefreshQuery.Last;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    RefreshQuery.Refresh;
    RefreshQuery.Last;
    RefreshQuery.Close;
  finally
    Query.Free;
    RefreshQuery.Free;
  end;
end;

{**
  Bugs report #795641.

  AV when TZConnection component is destroyed before linked TDataset
}
procedure ZTestCompCoreBugReport.Test795641;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := TZConnection.Create(nil);
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;

  Connection.Free;
  Query.Free;
end;

{**
  Bug report#826886.
  Access Violation with a simple query
}
procedure ZTestCompCoreBugReport.Test826886;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'update people set p_dep_id=p_dep_id where 1=0';

  try
    Connection.Connect;
    Query.ExecSQL;
    Connection.Disconnect;
    Connection.Connect;
    Query.ExecSQL;
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#000004.
  Duplications of commands in SQLProcessor.
}
procedure ZTestCompCoreBugReport.Test000004;
var
  Connection: TZConnection;
  SQLProcessor: TZSQLProcessor;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  SQLProcessor := TZSQLProcessor.Create(nil);
  SQLProcessor.Connection := Connection;

  try
    SQLProcessor.Script.Text := 'update people set p_dep_id=p_dep_id where 1=0';
    SQLProcessor.Execute;

    SQLProcessor.Script.Text := 'update people set p_dep_id=p_dep_id where 1=0';
    SQLProcessor.Execute;
  finally
    Connection.Free;
    SQLProcessor.Free;
  end;
end;

{**
  Bug report#832467.
  Problem with values, who contain ' symbol.
}
procedure ZTestCompCoreBugReport.Test832467;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  // Query.RequestLive := True;
  Query.CachedUpdates := True;
  Query.SQL.Text := 'select p_name from people where 1=0';

  try
    Query.Open;
    Query.Insert;
    Query.Fields[0].AsString := 'abc''def';
    Query.Post;
    Query.Insert;
    Query.Fields[0].AsString := 'abcdef';
    Query.Post;
    CheckEquals(2, Query.RecordCount);

    Query.Filter := 'P_Name=''abc''''def''';
    Query.Filtered := True;
    CheckEquals(1, Query.RecordCount);
    CheckEquals('abc''def', Query.Fields[0].AsString);

    Query.Filter := '"p_name"<>''abc''''def''';
//    Query.Filtered := True
    CheckEquals(1, Query.RecordCount);
    CheckEquals('abcdef', Query.Fields[0].AsString);

    Query.Close;
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#830804.
  Problem with values, who contain ' symbol.
}
procedure ZTestCompCoreBugReport.Test830804;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'select p_id, p_name, p_resume from people'
    + ' where p_id < 4 order by p_id';

  if StartsWith(LowerCase(Connection.Protocol), 'interbase')
    or StartsWith(LowerCase(Connection.Protocol), 'firebird')
    or StartsWith(LowerCase(Connection.Protocol), 'oracle') then
  begin
    try
      Query.Open;
      CheckEquals('P_ID', Query.Fields[0].FieldName);
      CheckEquals('P_NAME', Query.Fields[1].FieldName);
      CheckEquals('P_RESUME', Query.Fields[2].FieldName);

      Query.First;
      CheckEquals(1, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(2, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(3, Query.FieldByName('p_id').AsInteger);

      Query.Fields[0].Index := 1;
      CheckEquals('P_NAME', Query.Fields[0].FieldName);
      CheckEquals('P_ID', Query.Fields[1].FieldName);
      CheckEquals('P_RESUME', Query.Fields[2].FieldName);

      Query.First;
      CheckEquals(1, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(2, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(3, Query.FieldByName('p_id').AsInteger);

      Query.Close;
    finally
      Connection.Free;
      Query.Free;
    end
  end else begin
    try
      Query.Open;
      CheckEquals('p_id', Query.Fields[0].FieldName);
      CheckEquals('p_name', Query.Fields[1].FieldName);
      CheckEquals('p_resume', Query.Fields[2].FieldName);

      Query.First;
      CheckEquals(1, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(2, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(3, Query.FieldByName('p_id').AsInteger);

      Query.Fields[0].Index := 1;
      CheckEquals('p_name', Query.Fields[0].FieldName);
      CheckEquals('p_id', Query.Fields[1].FieldName);
      CheckEquals('p_resume', Query.Fields[2].FieldName);

      Query.First;
      CheckEquals(1, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(2, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(3, Query.FieldByName('p_id').AsInteger);

      Query.Close;
    finally
      Connection.Free;
      Query.Free;
    end;
  end;  
end;

{**
  Bug report#833197.
  Refresh problem with filtered data.
}
procedure ZTestCompCoreBugReport.Test833197;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'select * from people';

  try
    Query.Open;
    Query.Filter := 'p_name=''Aleksey Petrov''';
    Query.Filtered := True;
    CheckEquals(1, Query.RecordCount);

    Query.Refresh;
    CheckEquals(1, Query.RecordCount);

    Query.Close;
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#834798.
  CaseInsensitive broken.
}
procedure ZTestCompCoreBugReport.Test834798;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'select * from people order by p_id';

  try
    Query.Open;
    Query.Locate('p_name', 'Kristen Sato', []);
    Check(Query.Found);
    CheckEquals(3, Query.RecNo);

    Query.First;
    Query.Locate('p_name', 'KRISTEN sato', [loCaseInsensitive]);
    Check(Query.Found);
    CheckEquals(3, Query.RecNo);

    Query.First;
    Query.Locate('p_name', 'Kristen', [loPartialKey]);
    Check(Query.Found);
    CheckEquals(3, Query.RecNo);

    Query.First;
    Query.Locate('p_name', 'KRISTEN', [loPartialKey, loCaseInsensitive]);
    Check(Query.Found);
    CheckEquals(3, Query.RecNo);

    Query.Close;
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#839540.
  Logical operations don't work properly in filter expression.
}
procedure ZTestCompCoreBugReport.Test839540;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'select * from people order by p_id';

  try
    Query.Open;
    Query.Filter := 'P_ID=1 OR p_id=2';
    Query.Filtered := True;

    CheckEquals(2, Query.RecordCount);

    Query.Close;
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#840218.
  Strange behaviour with Lookup fields in ver. 6.1.1 alpha.
}
procedure ZTestCompCoreBugReport.Test840218;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'select * from people order by p_id';

  try
    Query.Open;

    CheckEquals('Kristen Sato', VarToStr(Query.Lookup('p_id', 3, 'p_name')));

    Query.Close;
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#842678.
  AV with ZComponent.bpl when removing ZUpdateSQL from ZQuery.
}
procedure ZTestCompCoreBugReport.Test842678;
var
  Query: TZQuery;
  UpdateSQL: TZUpdateSQL;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  UpdateSQL := TZUpdateSQL.Create(nil);

  try
    Query.UpdateObject := UpdateSQL;
    Query.UpdateObject := nil;
  finally
    UpdateSQL.Free;
    Query.Free;
  end;
end;

{**
  Bug report#846377.
  EVariantTypeCastError in Locate with loCaseInsensitive when Query
  has NULL fields.
}
procedure ZTestCompCoreBugReport.Test846377;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.CachedUpdates := True;
  // Query.RequestLive := True;
  Query.SQL.Text := 'select p_id, p_name from people where 1=0';

  try
    Query.Open;

    Query.Insert;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsString := 'abc';
    Query.Post;

    Query.Insert;
    Query.Fields[0].AsInteger := 2;
    Query.Post;

    Query.First;
    Query.Locate('p_name', 'xyz', [loCaseInsensitive]);
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#880459.
  Access Violation in ZSQLProcessor.Execute method when Connection is not set.
}
procedure ZTestCompCoreBugReport.Test880459;
var
  Processor: TZSQLProcessor;
begin
  if SkipClosed then Exit;

  Processor := TZSQLProcessor.Create(nil);
  try
    Processor.Script.Text := 'SELECT * FROM peoplt';
    try
      Processor.Execute;
      Fail('Processor without Connection must throw exception');
    except
      on E: Exception do
      begin
        if StartsWith(E.Message, 'Access violation') then
          Fail('Exception shouldn''t be an Access Violation');
      end;
    end;
  finally
    Processor.Free;
  end;
end;

{**
  Bug report#000005.
  Access Violation in Query.Open when statement does not return a Resultset.
}
procedure ZTestCompCoreBugReport.Test000005;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  try
    Query := TZQuery.Create(nil);
    try
      Query.Connection := Connection;
      Query.SQL.Text := 'update people set p_name=p_name where 1=0';

      try
        Query.Open;
        Fail('Should be an exception when DML statement is executed via Query.Open');
      except
        on E: Exception do
        begin
          if StartsWith(E.Message, 'Access violation') then
            Fail('Query.Open for DML statement shouldn''t throw Access Violation');
        end;
      end;
    finally
      Query.Free;
    end;
  finally
    Connection.Free;
  end;
end;

{**
  Bug report#887103.
  BeforeScroll and AfterScroll events are not working with SetRecNo.
}
procedure ZTestCompCoreBugReport.Test887103;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.BeforeScroll := DataSetBeforeScroll;
  Query.AfterScroll := DataSetAfterScroll;
  Query.SQL.Text := 'select * from people';

  try
    Query.Open;

    FUpdateCounter := 0;
    Query.RecNo := 3;
    CheckEquals(101, FUpdateCounter);

    Query.Close;
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#919401.
  BeforeScroll and AfterScroll events are not working with SetRecNo.
}
procedure ZTestCompCoreBugReport.Test919401;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.CachedUpdates := True;
  // Query.RequestLive := True;
  Query.SQL.Text := 'select * from people';

  try
    Query.Open;

    CheckEquals(False, Query.UpdatesPending);
    Query.Edit;
    CheckEquals(False, Query.UpdatesPending);
    Query.FieldByName('p_id').AsInteger := 12345;
    CheckEquals(True, Query.UpdatesPending);
    Query.Post;
    CheckEquals(True, Query.UpdatesPending);
    Query.CancelUpdates;
    CheckEquals(False, Query.UpdatesPending);

    Query.Close;
  finally
    Query.Free;
    Connection.Free;
  end;
end;

{**
  Bug report#926264.
  TZAbstractRODataset.UpdateStatus bug.
}
procedure ZTestCompCoreBugReport.Test926264;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.CachedUpdates := True;
  // Query.RequestLive := True;
  Query.SQL.Text := 'select * from people';

  try
    Query.Open;
    Query.DisableControls;

    CheckEquals(5, Query.RecordCount);
    while not Query.Eof do
    begin
      CheckEquals(Ord(usUnmodified), Ord(Query.UpdateStatus));
      Query.Next;
    end;

    Query.First;
    Query.Edit;
    Query.FieldByName('p_id').AsInteger := 567;
    Query.FieldByName('p_name').AsString := 'fgh';
    Query.Post;

    Query.Append;
    Query.FieldByName('p_id').AsInteger := 123;
    Query.FieldByName('p_name').AsString := 'abc';
    Query.Post;

    Query.Append;
    Query.FieldByName('p_id').AsInteger := 321;
    Query.FieldByName('p_name').AsString := 'xyz';
    Query.Post;

    Query.Delete;

    Query.ShowRecordTypes := [usModified,usInserted,usDeleted];
    Query.First;
    CheckEquals(Ord(usModified), Ord(Query.UpdateStatus));
    Query.Next;
    CheckEquals(Ord(usInserted), Ord(Query.UpdateStatus));
    Query.Next;
    CheckEquals(Ord(usDeleted), Ord(Query.UpdateStatus));
    Query.Next;
    Check(Query.Eof);
  finally
    Query.EnableControls;
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#953557.
  First and Last methods call afterscroll repeatedly.
}
procedure ZTestCompCoreBugReport.Test953557;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.BeforeScroll := DataSetBeforeScroll;
  Query.AfterScroll := DataSetAfterScroll;
  Query.SQL.Text := 'select * from people';

  try
    FUpdateCounter := 0;
    Query.Open;
    CheckEquals(100, FUpdateCounter);
    CheckEquals(1, Query.RecNo);
    CheckEquals(5, Query.RecordCount);

    FUpdateCounter := 0;
    Query.RecNo := 3;
    CheckEquals(101, FUpdateCounter);

    FUpdateCounter := 0;
    Query.First;
    CheckEquals(101, FUpdateCounter);
    CheckEquals(1, Query.RecNo);

    FUpdateCounter := 0;
    Query.Last;
    CheckEquals(101, FUpdateCounter);
    CheckEquals(5, Query.RecNo);

    Query.Close;
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#966267.
  TZQuery.OnPostError event doesn't activate.
}
procedure ZTestCompCoreBugReport.Test966267;
var
  Connection: TZConnection;
  Query: TZQuery;
  UpdateSQL: TZUpdateSQL;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  // Query.RequestLive := True;
  Query.SQL.Text := 'select * from people';
  Query.OnDeleteError := DataSetOnError;
  Query.OnPostError := DataSetOnError;
  Query.OnEditError := DataSetOnError;
  Query.OnApplyUpdateError := DataSetOnError;

  UpdateSQL := TZUpdateSQL.Create(nil);
  UpdateSQL.InsertSQL.Text := 'xxxx';
  UpdateSQL.ModifySQL.Text := 'xxxx';
  UpdateSQL.DeleteSQL.Text := 'xxxx';
  Query.UpdateObject := UpdateSQL;

  try
    Query.Open;

    FErrorCounter := 0;
    Query.Edit;
    Query.Fields[0].AsInteger := 12345;
    try
      Query.Post;
      Fail('Wrong Error Processing');
    except on E: EAbort do
      // Ignore.
    end;
    Check(FErrorCounter > 0);
    Query.Cancel;

    FErrorCounter := 0;
    try
      Query.Delete;
      Fail('Wrong Error Processing');
    except on E: EAbort do
      // Ignore.
    end;
    Check(FErrorCounter > 0);

    FErrorCounter := 0;
    Query.Append;
    try
      Query.Post;
      Fail('Wrong Error Processing');
    except on E: EAbort do
      // Ignore.
    end;
    Check(FErrorCounter > 0);
    Query.Cancel;
{
    Query.Close;
    Query.CachedUpdates := True;
    Query.Open;

    Query.Append;
    Query.Post;

    FErrorCounter := 0;
    try
      Query.CommitUpdates;
      Fail('Wrong Error Processing');
    except on E: EAbort do
      // Ignore.
    end;
    Check(FErrorCounter > 0);
    Query.CancelUpdates;
}
    Query.Close;
  finally
    UpdateSQL.Free;
    Query.Free;
    Connection.Free;
  end;
end;

{**
  Bug report#985629.
  Locate and Lookup don't find float fields.
}
procedure ZTestCompCoreBugReport.Test985629;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'select c_cost from cargo order by c_id';

  try
    Query.Open;

    Query.Locate('c_cost', 643.11, []);
    Check(Query.Found);
    CheckEquals(3, Query.RecNo);

    Query.Close;
  finally
    Query.Free;
    Connection.Free;
  end;
end;

{**
  Bug report Float fields precision
}
procedure ZTestCompCoreBugReport.TestFloatPrecision;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'Insert into number_values(n_id, n_money) values(999999,643.11)';
  Query.ExecSQL;

  try
    Query.SQL.Text := 'select n_money from number_values where n_id=999999';
    Query.Open;
    // uses format to avoid local separator differences 
    CheckEquals(trim(Format('%8.2f', [643.11])),Query.Fields[0].AsString);

    Query.Close;
  finally
    Query.SQL.Text := 'delete from number_values where n_id=999999';
    Query.execSql;
    Query.Free;
    Connection.Free;
  end;
end;

{**
  Bug report#995080.
  "List Index Out of Bounds" exception in Query.UpdateStatus.
}
procedure ZTestCompCoreBugReport.Test995080;
var
  Connection: TZConnection;
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  // Query.RequestLive := True;
  Query.CachedUpdates := True;
  Query.SQL.Text := 'select * from people where 1=0';

  try
    Query.Open;

    Check(Query.UpdateStatus = usUnmodified);

    Query.Append;
    Check(Query.UpdateStatus = usUnmodified);

    Query.Post;
    Check(Query.UpdateStatus = usInserted);

    Query.CancelUpdates;
    Check(Query.UpdateStatus = usUnmodified);

    Query.Close;
  finally
    Query.Free;
    Connection.Free;
  end;
end;

{**
  Bug report#996283.
  TZSQLStrings Text property problem.
}
procedure ZTestCompCoreBugReport.Test996283;
var
  Query: TZReadOnlyQuery;
begin
  if SkipClosed then Exit;

  Query := TZReadOnlyQuery.Create(nil);
  Query.Connection := Connection;

  try
    Check(Query.ParamCheck);
    CheckEquals(0, Query.Params.Count);

    Query.SQL.Text := 'xxx :aaa, :bbb, :ccc xxx';
    CheckEquals(3, Query.Params.Count);
    CheckEquals('aaa', Query.Params[0].Name);
    CheckEquals('bbb', Query.Params[1].Name);
    CheckEquals('ccc', Query.Params[2].Name);

    Query.SQL.Text := 'xxx :xyz xxx';
    CheckEquals(1, Query.Params.Count);
    CheckEquals('xyz', Query.Params[0].Name);

    Query.ParamCheck := False;
    CheckEquals(0, Query.Params.Count);

    Query.SQL.Text := 'xxx :aaa, :bbb, :ccc xxx';
    CheckEquals(0, Query.Params.Count);
  finally
    Query.Free;
  end;
end;

{**
  Bug report#1004534.
  Oracle 9 SQL Syntax error when query contains 0x0A symbols.
}
procedure ZTestCompCoreBugReport.Test1004534;
var
  Query: TZReadOnlyQuery;
begin
  if SkipClosed then Exit;

  Query := TZReadOnlyQuery.Create(nil);
  Query.Connection := Connection;

  try
    Check(not Query.Active);
    try
      Query.RecNo := 1;
      Fail('SetRecNo must throw exception when Active=False');
    except
      on E: Exception do
      begin
        Check(E is EDatabaseError);
      end;
    end;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#1012751.
  ErrorHandling in Dataset.SetRecNo when Active=False.
}
procedure ZTestCompCoreBugReport.Test1012751;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'select * '#10'from cargo'#13' order by'#10#13' c_id';

  try
    Query.Open;
    CheckEquals(4, Query.RecordCount);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#1022415.
  Parameters in subquery doesn't exist.
}
procedure ZTestCompCoreBugReport.Test1022415;
var
  Query: TZQuery;
begin
  if SkipClosed then Exit;

  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'SELECT * FROM table1 WHERE field1 NOT IN ('
    + 'SELECT field1 FROM table2 WHERE field2 = :Param1)';

  try
    CheckEquals(1, Query.Params.Count);
    CheckEquals('Param1', Query.Params[0].Name);
  finally
    Query.Free;
  end;
end;

{**
  Bug report#1049821.
  ZSQLScriptParser drops last statement when no closing ';'.
}
procedure ZTestCompCoreBugReport.Test1049821;
var
  Processor: TZSQLProcessor;
begin
  if SkipClosed then Exit;

  Processor := TZSQLProcessor.Create(nil);
  try
    Processor.Connection := Connection;
    Processor.Script.Text := 'SELECT * FROM people;'#10#13'SELECT * FROM cargo';
    Processor.Parse;
    CheckEquals(2, Processor.StatementCount);
    CheckEquals('SELECT * FROM people', Processor.Statements[0]);
    CheckEquals('SELECT * FROM cargo', Processor.Statements[1]);
    Processor.Execute;
  finally
    Processor.Free;
  end;
end;

{**
  Bug report#1045500.
  Exception when TZSQLMetadata is opened twice.
}
procedure ZTestCompCoreBugReport.Test1045500;
var
  Metadata: TZSQLMetadata;
begin
  if SkipClosed then Exit;

  Metadata := TZSQLMetadata.Create(nil);
  try
    Metadata.Connection := Connection;
    Metadata.MetadataType := mdTables;
    Metadata.Active := True;
    Metadata.Active := False;
    Metadata.Active := True;
  finally
    Metadata.Free;
  end;
end;

{**
  Bug report#1036916.
  Incorrect transfer \' to the Query params.
}
procedure ZTestCompCoreBugReport.Test1036916;
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
    Query.SQL.Text := 'DELETE FROM equipment WHERE eq_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO equipment (eq_id, eq_name) VALUES (:id, :name)';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.ParamByName('name').AsString := 'ab''cd''ef';
    Query.ExecSQL;
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.ParamByName('name').AsString := 'ab\cd\ef';
    Query.ExecSQL;
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ParamByName('name').AsString := 'ab\''cd\''ef';
    Query.ExecSQL;

    { Opens a result set. }
    Query.SQL.Text := 'SELECT * FROM equipment WHERE eq_id>=:id ORDER BY eq_id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.Open;
    CheckEquals(TEST_ROW_ID - 2, Query.FieldByName('eq_id').AsInteger);
    CheckEquals('ab''cd''ef', Query.FieldByName('eq_name').AsString);
    Query.Next;
    CheckEquals(TEST_ROW_ID - 1, Query.FieldByName('eq_id').AsInteger);
    CheckEquals('ab\cd\ef', Query.FieldByName('eq_name').AsString);
    Query.Next;
    CheckEquals(TEST_ROW_ID, Query.FieldByName('eq_id').AsInteger);
    CheckEquals('ab\''cd\''ef', Query.FieldByName('eq_name').AsString);

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM equipment WHERE eq_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
   Test for Bug#1004584 - problem start transaction in non autocommit mode
}
procedure ZTestCompCoreBugReport.Test1004584;
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

initialization
  RegisterTest('bugreport',ZTestCompCoreBugReport.Suite);
end.
