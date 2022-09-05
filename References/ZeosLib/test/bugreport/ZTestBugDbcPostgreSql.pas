{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Test Cases for PostgreSql DBC Bug Reports       }
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

unit ZTestBugDbcPostgreSql;

interface

{$I ZBugReport.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZBugReport, ZCompatibility, ZDbcPostgreSql,
  ZTestConsts;

type

  {** Implements a DBC bug report test case for PostgreSQL. }
  TZTestDbcPostgreSQLBugReport = class(TZSpecificSQLBugReportTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;
    function GetConnectionUrl: string;

    property Connection: IZConnection read FConnection write FConnection;
  published
    procedure Test702361;
    procedure Test702365;
    procedure Test702368;
    procedure Test727385;
    procedure Test739514;
    procedure Test739444;
    procedure Test759184;
    procedure Test798336;
    procedure Test815852;
    procedure Test815854;
    procedure Test824786;
    procedure Test815861;
    procedure Test933623;
    procedure Test1014416;
  end;

implementation

uses SysUtils, ZSysUtils, ZTestCase, ZDbcPostgreSqlUtils;

{ TZTestDbcPostgreSQLBugReport }

function TZTestDbcPostgreSQLBugReport.GetConnectionUrl: string;
begin
  if Port <> 0 then
    Result := Format('zdbc:%s://%s:%d/%s', [Protocol, HostName, Port, Database])
  else Result := Format('zdbc:%s://%s/%s', [Protocol, HostName, Database]);
end;

function TZTestDbcPostgreSQLBugReport.GetSupportedProtocols: string;
begin
  Result := 'postgresql,postgresql-7,postgresql-8';
end;

procedure TZTestDbcPostgreSQLBugReport.SetUp;
begin
  Connection := CreateDbcConnection;
end;

procedure TZTestDbcPostgreSQLBugReport.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

{**
  PostgreSQL - inv_open: large object
  Servet: PostgreSQL 7.3.1(NT/Cygwin),
  Environment: Delphi6
  Components: ZeosDBO 6.0.4
  Sql: select * from pg_class

  Error:
  I receive msgbox error "inv_open: large object ..."
  when column in result set is OID datatype.
}
procedure TZTestDbcPostgreSQLBugReport.Test702361;
var
  Connection: IZConnection;
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  if SkipClosed then Exit;

  Connection := DriverManager.GetConnectionWithLogin(
    GetConnectionUrl + '?oidasblob=true', UserName, Password);
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select reltype from pg_class');
  CheckEquals(Ord(stBinaryStream), Ord(ResultSet.GetMetadata.GetColumnType(1)));
  ResultSet.Close;
  Statement.Close;

  Connection := DriverManager.GetConnectionWithLogin(
    GetConnectionUrl + '?oidasblob=false', UserName, Password);
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select reltype from pg_class');
  CheckEquals(Ord(stInteger), Ord(ResultSet.GetMetadata.GetColumnType(1)));
  ResultSet.Close;
  Statement.Close;
end;

{**
  PostgreSQL - empty columns
  Server: PostgreSQL 7.3.1(NT/Cygwin)
  Environment: Delphi6
  Components: ZeosDBO 6.0.4
  Sql: select contype from pg_constraint

  Error:
  I receive empty value in contype column, but this
  column have a value example.
}
procedure TZTestDbcPostgreSQLBugReport.Test702365;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  if SkipClosed then Exit;
  with (Connection as IZPostgreSQLConnection) do
  begin
    if (GetServerMajorVersion < 7 ) or
       ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
      Exit;
  end;

  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select contype from pg_constraint');
  while ResultSet.Next do
    CheckNotEquals('', ResultSet.GetStringByName('contype'));
  ResultSet.Close;
  Statement.Close;
end;

{**
  PostgreSQL - bytea datatype
  Server: PostgreSQL 7.3.1(NT/Cygwin)
  Environment: Delphi6
  Components: ZeosDBO 6.0.4
  Sql: select probin from pg_proc

  Error:
  "... wrong size ..." when column in bytea datatype
}
procedure TZTestDbcPostgreSQLBugReport.Test702368;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select probin from pg_proc');
  CheckEquals(Ord(stBinaryStream), Ord(ResultSet.GetMetadata.GetColumnType(1)));
  while ResultSet.Next do
  begin
    CheckNotEquals(0, Length(ResultSet.GetString(1)));
    CheckNotEquals(0, Length(ResultSet.GetBytes(1)));
    CheckEquals(ResultSet.GetString(1), BytesToStr(ResultSet.GetBytes(1)));
  end;
  ResultSet.Close;
  Statement.Close;
end;

{
  Server: PostgreSql 7.3.2
  Components: ZeosDBO 6.0.8
  The problem is in the

  Error:
  TZRowAccessor.GetBlobObject
  NullPtr^ resolves to zero, so Result is assigned to
  BlobPtr.
}
procedure TZTestDbcPostgreSQLBugReport.Test727385;
var
  Sql: string;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TStream;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Sql := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
  Statement.ExecuteUpdate(Sql);

  StrStream := TMemoryStream.Create;
  StrStream.LoadFromFile('../../../database/text/lgpl.txt');
  StrStream.Size := 1024;
  BinStream := TMemoryStream.Create;
  BinStream.LoadFromFile('../../../database/images/dogs.jpg');
  BinStream.Size := 1024;

  StrStream1 := nil;
  BinStream1 := nil;

  try
    Sql := 'SELECT p_id, p_resume, p_picture FROM people where p_id = '
      + IntToStr(TEST_ROW_ID);
    { Inserts test record to equipment }
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    with ResultSet do
    begin
      MoveToInsertRow;
      UpdateIntByName('p_id', TEST_ROW_ID);
      UpdateAsciiStreamByName('p_resume', StrStream);
      UpdateBinaryStreamByName('p_picture', BinStream);
      InsertRow;
      Close;
    end;
    ResultSet := nil;

    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    with ResultSet do
    begin
      Check(Next);
      CheckEquals(TEST_ROW_ID, GetIntByName('p_id'));
      BinStream1 := GetBinaryStreamByName('p_picture');
      StrStream1 := GetAsciiStreamByName('p_resume');
      CheckEquals(BinStream, BinStream1);
      CheckEquals(StrStream, StrStream1);
      DeleteRow;
      Close;
    end;
  finally
    BinStream.Free;
    StrStream.Free;
    if Assigned(BinStream1) then
      BinStream1.Free;
    if Assigned(StrStream1) then
      StrStream1.Free;
  end;
end;

{**
  Aliases for fields do not work. Result Set after
  execution SQL query do not contain the aliased fields.
}
procedure TZTestDbcPostgreSQLBugReport.Test739444;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  MetaData: IZResultSetMetaData;
begin
  if SkipClosed then Exit;

  {test statement}
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select count(*) as items, sum(c_weight) as total, '+
    ' AVG(c_width) as average from cargo');

  MetaData := ResultSet.GetMetadata;
  CheckEquals(3, MetaData.GetColumnCount);
  CheckEquals('items', MetaData.GetColumnLabel(1));
  CheckEquals('total', MetaData.GetColumnLabel(2));
  CheckEquals('average', MetaData.GetColumnLabel(3));

  ResultSet.Next;
  CheckEquals(4, ResultSet.GetInt(1));
  CheckEquals(8434, ResultSet.GetInt(2));
  CheckEquals(8.5, ResultSet.GetFloat(3), 0.01);
  ResultSet.Close;
  ResultSet := nil;
end;

procedure TZTestDbcPostgreSQLBugReport.Test739514;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  Statement.ExecuteUpdate('delete from test739514 where id<>1');

  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select id, fld from test739514');
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(1, ResultSet.GetInt(1));
    CheckEquals('Абракадабра', ResultSet.GetString(2));

    MoveToInsertRow;
    UpdateIntByName('id', 2);
    UpdateStringByName('fld', '\Победа\');
    InsertRow;
    Close;
  end;

  ResultSet := Statement.ExecuteQuery('select id, fld from test739514 order by id');
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(1, ResultSet.GetInt(1));
    CheckEquals('Абракадабра', ResultSet.GetString(2));

    Check(Next);
    CheckEquals(2, ResultSet.GetInt(1));
    CheckEquals('\Победа\', ResultSet.GetString(2));
    Close;
  end;

  Statement.ExecuteUpdate('delete from test739514 where id<>1');
  Statement.Close;
end;

{**
  Test the bug report #759184.

  Empty fields in string concatination expression.
}
procedure TZTestDbcPostgreSQLBugReport.Test759184;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select p_id || p_name as expr from people where p_id=1');

  with ResultSet do
  begin
    Next;
    CheckEquals('expr', GetMetadata.GetColumnLabel(1));
    CheckEquals('1Vasia Pupkin', GetString(1));
  end;
end;

{**
  Test the bug report #798336.

  Not passing large objects to Postgres DB.
}
procedure TZTestDbcPostgreSQLBugReport.Test798336;
var
  Connection: IZConnection;
  PreparedStatement: IZPreparedStatement;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  TextStream: TStream;
  ImageStream: TMemoryStream;
  TempStream: TStream;
begin
  if SkipClosed then Exit;

  Connection := DriverManager.GetConnectionWithLogin(
    GetConnectionUrl + '?oidasblob=true', UserName, Password);
  Connection.SetTransactionIsolation(tiReadCommitted);
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Statement.ExecuteUpdate('DELETE FROM blob_values WHERE b_id='
    + IntToStr(TEST_ROW_ID));

  TextStream := TStringStream.Create('ABCDEFG');
  ImageStream := TMemoryStream.Create;
  ImageStream.LoadFromFile('../../../database/images/horse.jpg');

  try
    PreparedStatement := Connection.PrepareStatement(
      'INSERT INTO blob_values (b_id,b_text,b_image) VALUES(?,?,?)');
    PreparedStatement.SetInt(1, TEST_ROW_ID);
    PreparedStatement.SetAsciiStream(2, TextStream);
    PreparedStatement.SetBinaryStream(3, ImageStream);
    CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

    ResultSet := Statement.ExecuteQuery('SELECT * FROM blob_values'
      + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);
    CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('b_id'));

    TempStream := ResultSet.GetAsciiStreamByName('b_text');
    CheckEquals(TextStream, TempStream);
    TempStream.Free;

    TempStream := ResultSet.GetBinaryStreamByName('b_image');
    CheckEquals(ImageStream, TempStream);
    TempStream.Free;

    ResultSet.Close;
  finally
    TextStream.Free;
    ImageStream.Free;

    Statement.Close;
    Connection.Close;
  end;
end;

{**
  Test the bug report #815852.

  Metadata Query does not support Domains.
}
procedure TZTestDbcPostgreSQLBugReport.Test815852;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipClosed then Exit;

  with (Connection as IZPostgreSQLConnection) do
  begin
    if (GetServerMajorVersion < 7 ) or
       ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
      Exit;
    if (Protocol = 'postgresql-6.5') or
      (Protocol = 'postgresql-7.2') then
      Exit;
  end;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from test815852 ');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(1)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(2)));

  Statement.ExecuteUpdate('delete from test815852');

  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from test815852');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(1)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(2)));

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(1, 123456);
  ResultSet.UpdateString(2, 'abcdef');
  ResultSet.InsertRow;

  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from test815852');
  Check(ResultSet.Next);
  CheckEquals(123456, ResultSet.GetInt(1));
  CheckEquals('abcdef', ResultSet.GetString(2));

  Statement.ExecuteUpdate('delete from test815852');
end;

{**
  Test the bug report #815854.

  Problem with support for schemas.
}
procedure TZTestDbcPostgreSQLBugReport.Test815854;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipClosed then Exit;

  with (Connection as IZPostgreSQLConnection) do
  begin
    if (GetServerMajorVersion < 7 ) or
       ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
      Exit;
    if (Protocol = 'postgresql-6.5') or
      (Protocol = 'postgresql-7.2') then
      Exit;
  end;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from xyz.test824780 ');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(1)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(2)));

  Statement.ExecuteUpdate('delete from xyz.test824780');

  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from xyz.test824780');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(1)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(2)));

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(1, 123456);
  ResultSet.UpdateString(2, 'abcdef');
  ResultSet.InsertRow;

  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from xyz.test824780');
  Check(ResultSet.Next);
  CheckEquals(123456, ResultSet.GetInt(1));
  CheckEquals('abcdef', ResultSet.GetString(2));

  Statement.ExecuteUpdate('delete from xyz.test824780');
end;

{**
  Test the bug report #824786.
  TZMetadata shows PostgreSQL system tables.
}
procedure TZTestDbcPostgreSQLBugReport.Test824786;
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  if SkipClosed then Exit;

  Metadata := Connection.GetMetadata;
  ResultSet := Metadata.GetTables('', '', '', nil);

  while ResultSet.Next do
  begin
    if StartsWith(ResultSet.GetStringByName('TABLE_NAME'), 'sql_') then
      CheckEquals('SYSTEM TABLE', ResultSet.GetStringByName('TABLE_TYPE'));
  end;
end;

{**
  Test the bug report #815861
  Problem is incorrect parsing of the Version#
}
procedure TZTestDbcPostgreSQLBugReport.Test815861;
const
  MinorVersion1: string = '4beta2';
  MinorVersion2: string = 'tst4beta2';
  MinorVersion3: string = '123beta2';
begin
  CheckEquals(4, GetMinorVersion(MinorVersion1));
  CheckEquals(0, GetMinorVersion(MinorVersion2));
  CheckEquals(123, GetMinorVersion(MinorVersion3));
end;

{**
  Test the bug report #933623.
  Command is aborten until the next of transaction block.
}
procedure TZTestDbcPostgreSQLBugReport.Test933623;
var
  Statement: IZStatement;
begin
  if SkipClosed then Exit;

  Connection.SetAutoCommit(True);
  Connection.SetTransactionIsolation(tiReadCommitted);
  Statement := Connection.CreateStatement;
  try
    Statement.ExecuteQuery('select * from people where xp_id=1');
    Fail('Incorrect syntax error processing');
  except
    // Ignore.
  end;

  Statement.ExecuteQuery('select * from people where p_id=1');
end;

{**
  Test the bug report #1014416
  Problem is incorrect parsing of the Version#
}
procedure TZTestDbcPostgreSQLBugReport.Test1014416;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipClosed then Exit;

  with (Connection as IZPostgreSQLConnection) do
  begin
    if (GetServerMajorVersion < 7 ) or
       ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
      Exit;
    if (Protocol = 'postgresql-6.5') or
      (Protocol = 'postgresql-7.2') then
      Exit;
  end;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2, fld3 from test1014416');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(1)));
  CheckEquals(100, Metadata.GetPrecision(1));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(2)));
  CheckEquals(100, Metadata.GetPrecision(2));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(3)));
  CheckEquals(17, Metadata.GetPrecision(3));

  Check(ResultSet.Next);
  CheckEquals('192.168.100.128/25', ResultSet.GetString(1));
  CheckEquals('192.168.100.128/25', ResultSet.GetString(2));
  CheckEquals('08:00:2b:01:02:03', ResultSet.GetString(3));

  Check(ResultSet.Next);
  CheckEquals('2001:4f8:3:ba:2e0:81ff:fe22:d1f1/128', ResultSet.GetString(1));
  CheckEquals('2001:4f8:3:ba:2e0:81ff:fe22:d1f1', ResultSet.GetString(2));
  CheckEquals('08:00:2b:01:02:03', ResultSet.GetString(3));

  Check(not ResultSet.Next);
end;

initialization
  RegisterTest('bugreport',TZTestDbcPostgreSQLBugReport.Suite);
end.
