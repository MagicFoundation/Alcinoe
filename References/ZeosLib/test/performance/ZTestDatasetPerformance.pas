{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Test Case for TDataset Performance          }
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

unit ZTestDatasetPerformance;

interface

uses TestFramework, SysUtils, Classes, ZPerformanceTestCase, ZConnection,
  ZAbstractRODataset, ZDataset, ZDbcIntfs, DB;

type

  {** Implements a performance test case for ZeosDBO TDataset API. }
  TZDatasetPerformanceTestCase = class (TZPerformanceSQLTestCase)
  private
    FConnection: TZConnection;
    FQuery: TZQuery;
  protected
    property Query: TZQuery read FQuery write FQuery;
    property Connection: TZConnection read FConnection write FConnection;

    function GetImplementedAPI: string; override;
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
  end;

implementation

uses ZSysUtils;

{ TZDatasetPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZDatasetPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dataset';
end;

{**
   Create objects and allocate memory for variables
}
procedure TZDatasetPerformanceTestCase.SetUp;
var
  TempQuery: TZQuery;
begin
  Connection := CreateDatasetConnection;
  Connection.TransactIsolationLevel := tiReadCommitted;
  Connection.AutoCommit := True;

  TempQuery := TZQuery.Create(nil);
  TempQuery.Connection := Connection;
  // TempQuery.RequestLive := True;
  Query := TempQuery;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZDatasetPerformanceTestCase.TearDown;
begin
  if Query <> nil then
  begin
    Query.Free;
    Query := nil;
  end;

  if Connection <> nil then
  begin
    Connection.Free;
    Connection := nil;
  end;
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZDatasetPerformanceTestCase.DefaultSetUpTest;
begin
  Connection.Connect;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZDatasetPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  Connection.Disconnect;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZDatasetPerformanceTestCase.RunTestConnect;
begin
  Connection.Connect;
end;

{**
  The empty Set Up method for insert test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'SELECT * FROM high_load';
  // Query.RequestLive := True;
  Query.Open;
end;

{**
  Performs an insert test.
}
procedure TZDatasetPerformanceTestCase.RunTestInsert;
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
procedure TZDatasetPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  The empty Set Up method for fetch test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestFetch;
begin
  inherited SetUpTestFetch;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.ReadOnly := True;
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZDatasetPerformanceTestCase.RunTestFetch;
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
  The empty Set Up method for update test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestUpdate;
begin
  inherited SetUpTestUpdate;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs an update test.
}
procedure TZDatasetPerformanceTestCase.RunTestUpdate;
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

{**
  The empty Set Up method for delete test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestDelete;
begin
  inherited SetUpTestDelete;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs a delete test.
}
procedure TZDatasetPerformanceTestCase.RunTestDelete;
begin
  while not Query.EOF do
    Query.Delete;
end;

{**
  Performs a direct update test.
}
procedure TZDatasetPerformanceTestCase.RunTestDirectUpdate;
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
procedure TZDatasetPerformanceTestCase.SetUpTestLocate;
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
procedure TZDatasetPerformanceTestCase.RunTestLocate;
begin
  Query.Locate('data2','AAAAAAAAAA',[]);
end;

{**
  The empty Set Up method for lookup test.
}
procedure TZDatasetPerformanceTestCase.SetUpTestLookup;
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
procedure TZDatasetPerformanceTestCase.RunTestLookup;
begin
  Query.Lookup('data2','AAAAAAAAAA','hl_id');
end;

initialization
  TestFramework.RegisterTest(TZDatasetPerformanceTestCase.Suite);
end.

