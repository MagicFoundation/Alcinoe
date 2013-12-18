{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 Test Case for Sorting                   }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZTestSorting;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken,
  ZConnection, ZDataset, ZTestDefinitions, ZDbcMySql, ZDbcPostgreSql, ZDbcDbLib,
  ZCompatibility;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestSortingCase = class(TZComponentPortableSQLTestCase)
  private
    Connection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckDatasetColumns(Dataset: TDataset; FieldIndex: Integer;
      Values: array of string);
  published
    procedure TestResultSetSort;
    procedure TestCachedResultSetSort;
  end;

implementation

uses Classes, ZDbcUtils, ZTestConsts, ZDbcIntfs;

{ TZTestSortingCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSortingCase.SetUp;
begin
  Connection := CreateDatasetConnection;
end;

{**
  Removes data after each test.
}
procedure TZTestSortingCase.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Checks dataset field values.
  @param Dataset a dataset object.
  @param FieldNo a field index to check.
  @param Values a field expected values.
}
procedure TZTestSortingCase.CheckDatasetColumns(Dataset: TDataset;
  FieldIndex: Integer; Values: array of string);
var
  I: Integer;
begin
  Dataset.First;
  for I := Low(Values) to High(Values) do
  begin
    Check(not Dataset.Eof);
    CheckEquals(Values[I], Dataset.Fields[FieldIndex].AsString);
    Dataset.Next;
  end;
end;

{**
  Runs a test for sorting on the resultset level.
}
procedure TZTestSortingCase.TestResultSetSort;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from people where p_id < 6 order by p_id';
    Query.SortedFields := 'p_id Desc';
    Query.Open;

    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['5','4','3','2','1']);

    Query.RecNo := 1;
    Query.SortedFields := '';
    CheckEquals(5, Query.RecNo);
    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['1','2','3','4','5']);

    Query.RecNo := 1;
    Query.SortedFields := 'p_dep_id DESC, p_name';
    CheckEquals(5, Query.RecNo);
    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['5','4','2','3','1']);
    CheckDatasetColumns(Query, Query.FieldByName('p_dep_id').Index,
      ['3','2','2','1','1']);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for sorting on the cached resultset level.
}
procedure TZTestSortingCase.TestCachedResultSetSort;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.SQL.Text := 'select * from people where p_id < 6 order by p_id';
    Query.SortedFields := 'p_id Desc';
    Query.Open;

    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['5','4','3','2','1']);

    Query.RecNo := 1;
    Query.SortedFields := '';
    CheckEquals(5, Query.RecNo);
    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['1','2','3','4','5']);

    Query.RecNo := 1;
    Query.SortedFields := 'p_dep_id DESC, p_name';
    CheckEquals(5, Query.RecNo);
    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['5','4','2','3','1']);
    CheckDatasetColumns(Query, Query.FieldByName('p_dep_id').Index,
      ['3','2','2','1','1']);

    Query.Close;
  finally
    Query.Free;
  end;
end;

initialization
  RegisterTest('component',TZTestSortingCase.Suite);
end.
