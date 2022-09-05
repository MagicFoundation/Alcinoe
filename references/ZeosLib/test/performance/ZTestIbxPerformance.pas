{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for IBX API Performance            }
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

unit ZTestIBXPerformance;

interface
{$I ..\..\test\performance\ZPerformance.inc}

uses TestFramework, SysUtils, Classes,
{$IFNDEF EXCLUDE_IBX_TEST}
  IBDatabase, DB, IBQuery, IBUpdateSQL,
{$ENDIF}
  ZPerformanceTestCase;

type

  {** Implements a performance test case for IBX TQuery API. }
  TZIBXPerformanceTestCase = class (TZPerformanceSQLTestCase)
  protected
    function GetImplementedAPI: string; override;
    function IsProtocolValid(Value: string): boolean; override;

{$IFNDEF EXCLUDE_IBX_TEST}
  private
    FQuery: TIBQuery;
    FDatabase: TIBDatabase;
    FTransaction: TIBTransaction;
    FUpdateSQL :TIBUpdateSQL;

  protected
    property Query: TIBQuery read FQuery write FQuery;
    property Database: TIBDatabase read FDatabase write FDatabase;
    property Transaction: TIBTransaction read FTransaction write FTransaction;
    property UpdateSQL: TIBUpdateSQL read FUpdateSQL write FUpdateSQL;

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
{$ENDIF}
  end;

  {** Implements a performance test case for IBX TQuery API. }
  TZIBXCPerformanceTestCase = class (TZPerformanceSQLTestCase)
  protected
    function GetImplementedAPI: string; override;
    function IsProtocolValid(Value: string): boolean; override;

{$IFNDEF EXCLUDE_IBX_TEST}
  private
    FQuery: TIBQuery;
    FDatabase: TIBDatabase;
    FTransaction: TIBTransaction;
    FUpdateSQL :TIBUpdateSQL;

  protected
    property Query: TIBQuery read FQuery write FQuery;
    property Database: TIBDatabase read FDatabase write FDatabase;
    property Transaction: TIBTransaction read FTransaction write FTransaction;
    property UpdateSQL: TIBUpdateSQL read FUpdateSQL write FUpdateSQL;

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
{$ENDIF}
  end;

implementation

{ TZIBXPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZIBXPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'ibx';
end;


function TZIBXPerformanceTestCase.IsProtocolValid(Value: string): boolean;
begin
  if (Value = 'interbase-5') or (Value = 'interbase-6') or
     (Value = 'firebird-1.0') or (Value = 'firebird-1.5') or 
     (Value = 'firebird-2.0') or (Value = 'firebird-2.1') or 
     (Value = 'firebirdd-1.5') or (Value = 'firebirdd-2.0') or 
     (Value = 'firebirdd-2.1') then
    Result := True
  else
    Result := False;
end;


{$IFNDEF EXCLUDE_IBX_TEST}

{**
   Create objects and allocate memory for variables
}
procedure TZIBXPerformanceTestCase.SetUp;
begin

  Database := TIBDatabase.Create(nil);
  Query := TIBQuery.Create(nil);
  Transaction := TIBTransaction.Create(nil);
  UpdateSQL := TIBUpdateSQL.Create(nil);

  Database.DefaultTransaction := Transaction;
  Query.Database := Database;
  Query.Transaction := Transaction;
  Query.CachedUpdates := True;
  Query.UpdateObject := UpdateSQL;
  Transaction.DefaultDatabase := Database;

  Database.DatabaseName := HostName + ':' + inherited Database;
  Database.SQLDialect := 1;
  Database.LoginPrompt := false;
  Database.Params.Add('User_name=' + UserName);
  Database.Params.Add('Password=' + Password);

  UpdateSQL.InsertSQL.Text := 'insert into high_load (hl_id, data1, data2) values (:hl_id, :data1, :data2)';
  UpdateSQL.DeleteSQL.Text := 'delete from high_load where hl_id=:hl_id';
  UpdateSQL.ModifySQL.Text := 'update high_load set data1 =:data1, data2 =:data2 where hl_id = :hl_id';
  UpdateSQL.RefreshSQL.Text := 'select * from high_load';
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZIBXPerformanceTestCase.TearDown;
begin

  if UpdateSQL <> nil then
  begin
    UpdateSQL.Free;
    UpdateSQL := nil;
  end;

  if Query <> nil then
  begin
    Query.Free;
    Query := nil;
  end;

  if Transaction <> nil then
  begin
    Transaction.Free;
    Transaction := nil;
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
procedure TZIBXPerformanceTestCase.DefaultSetUpTest;
begin
  Database.Open;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZIBXPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  Database.Close;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZIBXPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZIBXPerformanceTestCase.RunTestConnect;
begin
  Database.Open;
end;

{**
  Performs an insert test.
}
procedure TZIBXPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs an insert test.
}
procedure TZIBXPerformanceTestCase.RunTestInsert;
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
      ApplyUpdates;
    end;
  end;
end;

{**
  Performs an open test.
}
procedure TZIBXPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs an fetch test.
}
procedure TZIBXPerformanceTestCase.SetUpTestFetch;
begin
  inherited SetUpTestFetch;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZIBXPerformanceTestCase.RunTestFetch;
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
procedure TZIBXPerformanceTestCase.SetUpTestUpdate;
begin
  inherited SetUpTestUpdate;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs an update test.
}
procedure TZIBXPerformanceTestCase.RunTestUpdate;
begin
  while not Query.EOF do
  begin
    with Query do
    begin
      Edit;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
      ApplyUpdates;
      Next;
    end;
  end;
end;

{**
  Performs an delete test.
}
procedure TZIBXPerformanceTestCase.SetUpTestDelete;
begin
  inherited SetUpTestDelete;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs a delete test.
}
procedure TZIBXPerformanceTestCase.RunTestDelete;
begin
  while not Query.EOF do
    with Query do
    begin
    Delete;
    ApplyUpdates;
    end;
end;

{$ENDIF}

{ TZIBXCPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZIBXCPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'ibxc';
end;


function TZIBXCPerformanceTestCase.IsProtocolValid(Value: string): boolean;
begin
  if (Value = 'interbase-5') or (Value = 'interbase-6') or
     (Value = 'firebird-1.0') or (Value = 'firebird-1.5') then
    Result := True
  else
    Result := False;
end;


{$IFNDEF EXCLUDE_IBX_TEST}

{**
   Create objects and allocate memory for variables
}
procedure TZIBXCPerformanceTestCase.SetUp;
begin

  Database := TIBDatabase.Create(nil);
  Query := TIBQuery.Create(nil);
  Transaction := TIBTransaction.Create(nil);
  UpdateSQL := TIBUpdateSQL.Create(nil);

  Database.DefaultTransaction := Transaction;
  Query.Database := Database;
  Query.Transaction := Transaction;
  Query.CachedUpdates := True;
  Query.UpdateObject := UpdateSQL;
  Transaction.DefaultDatabase := Database;

  Database.DatabaseName := HostName + ':' + inherited Database;
  Database.SQLDialect := 1;
  Database.LoginPrompt := false;
  Database.Params.Add('User_name=' + UserName);
  Database.Params.Add('Password=' + Password);

  UpdateSQL.InsertSQL.Text := 'insert into high_load (hl_id, data1, data2) values (:hl_id, :data1, :data2)';
  UpdateSQL.DeleteSQL.Text := 'delete from high_load where hl_id=:hl_id';
  UpdateSQL.ModifySQL.Text := 'update high_load set data1 =:data1, data2 =:data2 where hl_id = :hl_id';
  UpdateSQL.RefreshSQL.Text := 'select * from high_load';
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZIBXCPerformanceTestCase.TearDown;
begin

  if UpdateSQL <> nil then
  begin
    UpdateSQL.Free;
    UpdateSQL := nil;
  end;

  if Query <> nil then
  begin
    Query.Free;
    Query := nil;
  end;

  if Transaction <> nil then
  begin
    Transaction.Free;
    Transaction := nil;
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
procedure TZIBXCPerformanceTestCase.DefaultSetUpTest;
begin
  Database.Open;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZIBXCPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  Database.Close;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZIBXCPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZIBXCPerformanceTestCase.RunTestConnect;
begin
  Database.Open;
end;

{**
  Performs an insert test.
}
procedure TZIBXCPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs an insert test.
}
procedure TZIBXCPerformanceTestCase.RunTestInsert;
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
  Query.ApplyUpdates;
end;

{**
  Performs an open test.
}
procedure TZIBXCPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Setup a fetch data
}
procedure TZIBXCPerformanceTestCase.SetUpTestFetch;
begin
  inherited SetUpTestFetch;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZIBXCPerformanceTestCase.RunTestFetch;
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
procedure TZIBXCPerformanceTestCase.SetUpTestUpdate;
begin
  inherited SetUpTestUpdate;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs an update test.
}
procedure TZIBXCPerformanceTestCase.RunTestUpdate;
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
  Query.ApplyUpdates;
end;

{**
  Performs a delete test.
}
procedure TZIBXCPerformanceTestCase.SetUpTestDelete;
begin
  inherited SetUpTestDelete;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs a delete test.
}
procedure TZIBXCPerformanceTestCase.RunTestDelete;
begin
  while not Query.EOF do
    with Query do
    Delete;
    Query.ApplyUpdates;
end;

{$ENDIF}


initialization
  TestFramework.RegisterTest(TZIBXPerformanceTestCase.Suite);
  TestFramework.RegisterTest(TZIBXCPerformanceTestCase.Suite);
end.

