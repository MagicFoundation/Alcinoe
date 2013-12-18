{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Case for Old ZeosDBO Performance          }
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

unit ZTestOldZeosPerformance;

interface

{$I ZPerformance.inc}

uses TestFramework, SysUtils, Classes,
{$IFNDEF EXCLUDE_OLD_ZEOS_TEST}
  ZMySqlCon, ZQuery, ZMySqlQuery, ZMySqlTr, ZConnect, ZTransact,
  ZPgSqlCon, ZPgSqlQuery, ZPgSqlTr, ZIbSqlCon, ZIbSqlQuery, ZIbSqlTr,
  ZMsSqlQuery, ZMsSqlTr, ZMsSqlCon, ZUpdateSql,
{$ENDIF}
  ZCompatibility, ZPerformanceTestCase;

type

  {** Implements a performance test case for Old ZeosDBO TDataset API. }
  TZOldZeosPerformanceTestCase = class (TZPerformanceSQLTestCase)
  protected
    function GetImplementedAPI: string; override;
    function GetSupportedProtocols: string; override;

{$IFNDEF EXCLUDE_OLD_ZEOS_TEST}
  private
    FQuery: TZZDataSet;
    FDatabase: TZZDatabase;
    FTransact: TZZTransact;

  protected
    property Query: TZZDataSet read FQuery write FQuery;
    property Database: TZZDatabase read FDatabase write FDatabase;
    property Transact: TZZTransact read FTransact write FTransact;

    procedure SetUp; override;
    procedure TearDown; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure SetUpTestInsert; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure SetUpTestFetch; override;
    procedure RunTestFetch; override;
    procedure SetUpTestUpdate; override;
    procedure RunTestUpdate; override;
    procedure SetUpTestDelete; override;
    procedure RunTestDelete; override;
    procedure RunTestDirectUpdate; override;
    procedure SetUpTestLocate; override;
    procedure RunTestLocate; override;
    procedure SetUpTestLookup; override;
    procedure RunTestLookup; override;
{$ENDIF}
  end;

implementation

uses ZSqlTestCase, ZSysUtils;

{ TZOldZeosPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZOldZeosPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'old-zeos';
end;

function TZOldZeosPerformanceTestCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5s'
    + ',sybase,interbase,interbase-5,interbase-6'
    + ',postgresql,postgresql-7,postgresql-8';
{$IFNDEF LINUX}
  Result := Result + ',mssql';
{$ENDIF}
end;

{$IFNDEF EXCLUDE_OLD_ZEOS_TEST}

{**
   Create objects and allocate memory for variables
}
procedure TZOldZeosPerformanceTestCase.SetUp;
var
  TempQuery1: TZZMySqlQuery;
  TempQuery2: TZZPgSqlQuery;
  TempQuery3: TZZIbSqlQuery;
  TempQuery4: TZZMsSqlQuery;
begin
  if StrLComp(PChar(GetProtocol), PChar('interbase'),
    Length('interbase')) = 0 then
  begin
    Database := TZZIbSqlDatabase.Create(nil);
    Transact := TZZIbSqlTransact.Create(nil);
    TempQuery3 := TZZIbSqlQuery.Create(nil);

    Database.AddTransaction(Transact);
    Transact.Database := Database;
    Transact.AutoCommit := True;

    TempQuery3.Database := (Database as TZZIbSqlDatabase);
    TempQuery3.Transaction := (Transact as TZZIbSqlTransact);
    // TempQuery3.RequestLive := True;
    Query := TempQuery3;

    with TZZIbSqlDatabase(Database) do
    begin
      Host := GetHostName;
      Database := GetDatabase;
      Login := GetUserName;
      Password := GetPassword;
    end;
  end;

  if StrLComp(PChar(GetProtocol), PChar('postgresql'),
    Length('postgresql')) = 0 then
  begin
    Database := TZZPgSqlDatabase.Create(nil);
    Transact := TZZPgSqlTransact.Create(nil);
    TempQuery2 := TZZPgSqlQuery.Create(nil);

    Database.AddTransaction(Transact);
    Transact.Database := Database;
    Transact.AutoCommit := True;

    TempQuery2.Database := (Database as TZZPgSqlDatabase);
    TempQuery2.Transaction := (Transact as TZZPgSqlTransact);
    // TempQuery2.RequestLive := True;
    Query := TempQuery2;

    with TZZPgSqlDatabase(Database) do
    begin
      Port := IntToStr(GetPort);
      Host := GetHostName;
      Database := GetDatabase;
      Login := GetUserName;
      Password := GetPassword;
    end;
  end;

  if StrLComp(PChar(GetProtocol), PChar('mysql'),
    Length('mysql')) = 0 then
  begin
    Database := TZZMySqlDatabase.Create(nil);
    Transact := TZZMySqlTransact.Create(nil);
    TempQuery1 := TZZMySqlQuery.Create(nil);

    Database.AddTransaction(Transact);
    Transact.Database := Database;
    Transact.AutoCommit := True;

    TempQuery1.Database := (Database as TZZMySqlDatabase);
    TempQuery1.Transaction := (Transact as TZZMySqlTransact);
    // TempQuery1.RequestLive := True;
    Query := TempQuery1;

    with TZZMySqlDatabase(Database) do
    begin
      Port := IntToStr(GetPort);
      Host := GetHostName;
      Database := GetDatabase;
      Login := GetUserName;
      Password := GetPassword;
    end;
  end;

  if StrLComp(PChar(GetProtocol), PChar('mssql'),
    Length('mssql')) = 0 then
  begin
    Database := TZZMsSqlDatabase.Create(nil);
    Transact := TZZMsSqlTransact.Create(nil);
    TempQuery4 := TZZMsSqlQuery.Create(nil);

    Database.AddTransaction(Transact);
    Transact.Database := Database;
    Transact.AutoCommit := True;

    TempQuery4.Database := (Database as TZZMsSqlDatabase);
    TempQuery4.Transaction := (Transact as TZZMsSqlTransact);
    // TempQuery4.RequestLive := True;
    Query := TempQuery4;

    with TZZMsSqlDatabase(Database) do
    begin
      //Port := IntToStr(GetPort);
      Host := GetHostName;
      Database := GetDatabase;
      Login := GetUserName;
      Password := GetPassword;
    end;
  end;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZOldZeosPerformanceTestCase.TearDown;
begin
  if Query <> nil then
  begin
    Query.Free;
    Query := nil;
  end;

  if Transact <> nil then
  begin
    Transact.Free;
    Transact := nil;
  end;

  if Database <> nil then
  begin
    Database.Free;
    Database := nil;
  end;
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZOldZeosPerformanceTestCase.DefaultSetUpTest;
begin
  Database.Connect;
  Transact.Connect;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZOldZeosPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  Transact.Disconnect;
  Database.Disconnect;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZOldZeosPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZOldZeosPerformanceTestCase.RunTestConnect;
begin
  Database.Connect;
  Transact.Connect;
end;

{**
  The empty Set Up method for insert test.
}
procedure TZOldZeosPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs an insert test.
}
procedure TZOldZeosPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with Query do
    begin
      Append;
      Fields[0].AsInteger := I;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
    end;
  end;
end;

{**
  Performs an open test.
}
procedure TZOldZeosPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}

procedure TZOldZeosPerformanceTestCase.SetUpTestFetch;
begin
  inherited SetUpTestFetch;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZOldZeosPerformanceTestCase.RunTestFetch;
begin
  while not Query.EOF do
  begin
    with Query do
    begin
      Fields[0].AsInteger;
      Fields[1].AsFloat;
      Fields[2].AsString;
      Next;
    end;
  end;
end;

{**
  Performs an update test.
}
procedure TZOldZeosPerformanceTestCase.SetUpTestUpdate;
begin
  inherited SetUpTestUpdate;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs an update test.
}
procedure TZOldZeosPerformanceTestCase.RunTestUpdate;
begin
  while not Query.EOF do
  begin
    with Query do
    begin
      Edit;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
      Next;
    end;
  end;
end;

procedure TZOldZeosPerformanceTestCase.SetUpTestDelete;
begin
  inherited SetUpTestDelete;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs a delete test.
}
procedure TZOldZeosPerformanceTestCase.RunTestDelete;
begin
  while not Query.EOF do
    Query.Delete;
end;

{**
  Performs a direct update test.
}
procedure TZOldZeosPerformanceTestCase.RunTestDirectUpdate;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    Query.SQL.Text := Format('UPDATE high_load SET data1=%s, data2=''%s'''
      + ' WHERE hl_id = %d', [FloatToSqlStr(RandomFloat(-100, 100)),
      RandomStr(10), I]);
    Query.ExecSQL;
  end;
end;

{**
  The empty Set Up method for locate test.
}
procedure TZOldZeosPerformanceTestCase.SetUpTestLocate;
begin
  inherited SetUpTestLocate;
  Query.SQL.Text := 'SELECT * FROM high_load ORDER BY hl_id';
  Query.Open;
  Query.Last;
  Query.First;
end;

{**
  Performs a locate test.
}
procedure TZOldZeosPerformanceTestCase.RunTestLocate;
begin
  Query.Locate('data2','AAAAAAAAAA',[]);
end;

{**
  The empty Set Up method for lookup test.
}
procedure TZOldZeosPerformanceTestCase.SetUpTestLookup;
begin
  inherited SetUpTestLookup;
  Query.SQL.Text := 'SELECT * FROM high_load ORDER BY hl_id';
  Query.Open;
  Query.Last;
  Query.First;
end;

{**
  Performs a lookup test.
}
procedure TZOldZeosPerformanceTestCase.RunTestLookup;
begin
  Query.Lookup('data2','AAAAAAAAAA','hl_id');
end;

{$ENDIF}

initialization
  TestFramework.RegisterTest(TZOldZeosPerformanceTestCase.Suite);
end.

