{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Test Cases for MySQL DBC Bug Reports            }
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

unit ZTestBugDbcMySql;

interface

{$I ZBugReport.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZBugReport, ZCompatibility, ZDbcMySql,
  ZDbcMySqlResultSet;

type
  {** Implements a DBC bug report test case for MySql. }
  TZTestDbcMySQLBugReport = class(TZSpecificSQLBugReportTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: IZConnection read FConnection write FConnection;
  published
    procedure Test702352;
    procedure Test739448;
    procedure Test739444;
    procedure Test768163;
    procedure Test816925;
    procedure Test881634;
    procedure Test924861;
    procedure Test961337;
  end;

implementation

uses ZTestCase;

{ TZTestDbcMySQLBugReport }

function TZTestDbcMySQLBugReport.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5';
end;

procedure TZTestDbcMySQLBugReport.SetUp;
begin
  Connection := CreateDbcConnection;
end;

procedure TZTestDbcMySQLBugReport.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

{**
  Test Case for Bug Report #702352
  Access violation
  Server: MySQL 4.0.3 beta-nt,
  Environment: Delphi6
  Components: ZeosDBO 6.0.4
  Sql: select * from mysql.user

  Access violation at address 006DE326 in module 'myprog.exe'.
}
procedure TZTestDbcMySQLBugReport.Test702352;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  PrStatement: IZPreparedStatement;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select * from mysql.user');
  while ResultSet.Next do
    CheckNotEquals('', ResultSet.GetStringByName('Host'));
  ResultSet.Close;
  Statement.Close;

  PrStatement := Connection.PrepareStatement('select * from mysql.user');
  ResultSet := PrStatement.ExecuteQuery('select * from mysql.user');

  while ResultSet.Next do
    CheckNotEquals('', ResultSet.GetStringByName('Host'));
  ResultSet.Close;
  PrStatement.Close;
end;

{**
  Aliases for fields do not work. Result Set after
  execution SQL query do not contain the aliased fields.
}
procedure TZTestDbcMySQLBugReport.Test739444;
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

{**
Dublicate field names. Do not show properly fields name
if query have two alike field names and different tables.
}
procedure TZTestDbcMySQLBugReport.Test739448;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  MetaData: IZResultSetMetaData;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select table739448a.fld1, table739448a.fld2, table739448a.fld3, '+
    ' table739448b.fld1, table739448b.fld2, table739448b.fld3 from table739448a, table739448b where '+
    ' table739448a.fld1 = table739448b.fld1 ');

  MetaData := ResultSet.GetMetadata;
  CheckEquals(6, MetaData.GetColumnCount);
  CheckEquals('fld1', MetaData.GetColumnLabel(1));
  CheckEquals('fld2', MetaData.GetColumnLabel(2));
  CheckEquals('fld3', MetaData.GetColumnLabel(3));
  CheckEquals('fld1_1', MetaData.GetColumnLabel(4));
  CheckEquals('fld2_1', MetaData.GetColumnLabel(5));
  CheckEquals('fld3_1', MetaData.GetColumnLabel(6));
  CheckEquals('fld1', MetaData.GetColumnName(1));
  CheckEquals('fld2', MetaData.GetColumnName(2));
  CheckEquals('fld3', MetaData.GetColumnName(3));
  CheckEquals('fld1', MetaData.GetColumnName(4));
  CheckEquals('fld2', MetaData.GetColumnName(5));
  CheckEquals('fld3', MetaData.GetColumnName(6));
  ResultSet.Close;
end;

{**
  Test the bug report #771576.

  unsigned int field problem.
}
procedure TZTestDbcMySQLBugReport.Test768163;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  if SkipClosed then Exit;

  {create statement}
  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  { clear table }
  Statement.Execute('delete from table768163');
  { insert data }
  ResultSet := Statement.ExecuteQuery('select * from table768163');
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateBigDecimal(1, 2147483648);
    InsertRow;
    Close;
  end;
  ResultSet := nil;
  { check inserted data }
  ResultSet := Statement.ExecuteQuery('select * from table768163');
  with ResultSet do
  begin
    Next;
    CheckEquals(2147483648, GetBigDecimal(1));
    Close;
  end;
    ResultSet := nil;
end;

{**
  Test the bug report #816925.

  Problems with ZeosDBO 6.0.2, Delphi 6 and Infopower 3000
}
procedure TZTestDbcMySQLBugReport.Test816925;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2, fld3, fld4 from table816925');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(1)));
  CheckEquals(Ord(stDouble), Ord(Metadata.GetColumnType(2)));
  CheckEquals(Ord(stLong), Ord(Metadata.GetColumnType(3)));
  CheckEquals(Ord(stDouble), Ord(Metadata.GetColumnType(4)));

  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2, fld3, fld4 from table816925');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(1)));
  CheckEquals(Ord(stDouble), Ord(Metadata.GetColumnType(2)));
  CheckEquals(Ord(stLong), Ord(Metadata.GetColumnType(3)));
  CheckEquals(Ord(stDouble), Ord(Metadata.GetColumnType(4)));
end;

{**
   Runs a test for bug report #881634
   Complex select statement returns wrong field types.
}
procedure TZTestDbcMySQLBugReport.Test881634;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('SELECT idt2, ft2, table881634a.ft1'
    + ' FROM table881634b INNER JOIN table881634a'
    + ' ON (table881634b.ft1 = table881634a.idt1)');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(1)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(2)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(3)));
end;

{**
  Runs a test for bug report #924861
  Memory leak, when client cannot connect to server
}
procedure TZTestDbcMySQLBugReport.Test924861;
var
  Connection: IZConnection;
begin
  if SkipClosed then Exit;

  try
    Connection := DriverManager.GetConnection('zdbc:mysql://xxx:12345/db');
    Fail('Incorrect processing of wrong connection URL.');
  except
    // Ignore.
  end;
end;

{**
  Runs a test for bug report #961337
  ENUM('Y','N') is not recognized as Boolean when column name is renamed.
}
procedure TZTestDbcMySQLBugReport.Test961337;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  try
    Statement.SetResultSetConcurrency(rcUpdatable);
    ResultSet := Statement.ExecuteQuery('SELECT id, fld1, fld2, fld1 as fld3,'
      + ' fld2 as fld4 FROM table735299');
    try
      Metadata := ResultSet.GetMetadata;
      CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(1)));
      CheckEquals(Ord(stBoolean), Ord(Metadata.GetColumnType(2)));
      CheckEquals(Ord(stBoolean), Ord(Metadata.GetColumnType(3)));
      CheckEquals(Ord(stBoolean), Ord(Metadata.GetColumnType(4)));
      CheckEquals(Ord(stBoolean), Ord(Metadata.GetColumnType(5)));
    finally
      ResultSet.Close;
    end;
  finally
    Statement.Close;
  end;
end;

initialization
  RegisterTest('bugreport',TZTestDbcMySQLBugReport.Suite);
end.
