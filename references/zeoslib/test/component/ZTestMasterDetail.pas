{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Test Case for Master-Detail Links             }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZTestMasterDetail;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken,
  ZConnection, ZDataset, ZTestDefinitions, ZDbcMySql, ZDbcPostgreSql, ZDbcDbLib;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestMasterDetailCase = class(TZComponentPortableSQLTestCase)
  private
    Connection: TZConnection;
    MasterDataSource: TDataSource;
    MasterQuery: TZQuery;
    DetailQuery: TZQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDataSource;
    procedure TestMasterFields;
  end;

implementation

uses Classes, ZDbcUtils, ZTestConsts, ZDbcIntfs;

{ TZTestMasterDetailCase }

{**
  Prepares initial data before each test.
}
procedure TZTestMasterDetailCase.SetUp;
begin
  Connection := CreateDatasetConnection;

  MasterQuery := TZQuery.Create(nil);
  MasterQuery.Connection := Connection;

  MasterDataSource := TDataSource.Create(nil);
  MasterDataSource.DataSet := MasterQuery;

  DetailQuery := TZQuery.Create(nil);
  DetailQuery.Connection := Connection;
end;

{**
  Removes data after each test.
}
procedure TZTestMasterDetailCase.TearDown;
begin
  DetailQuery.Close;
  DetailQuery.Free;

  MasterQuery.Close;
  MasterQuery.Free;

  MasterDataSource.Free;

  Connection.Disconnect;
  Connection.Free;
end;

{**
  Runs a test for SQL parameters.
}
procedure TZTestMasterDetailCase.TestDataSource;
begin
  MasterQuery.SQL.Text := 'SELECT * FROM department ORDER BY dep_id';
  MasterQuery.Open;

  DetailQuery.SQL.Text := 'SELECT * FROM people WHERE p_dep_id=:dep_id';
  DetailQuery.DataSource := MasterDataSource;
  DetailQuery.Open;

  MasterQuery.First;
  CheckEquals(1, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(1, DetailQuery.FieldByName('p_dep_id').AsInteger);

  MasterQuery.Next;
  CheckEquals(2, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(2, DetailQuery.FieldByName('p_dep_id').AsInteger);
end;

{**
  Runs a test for master-detail links.
}
procedure TZTestMasterDetailCase.TestMasterFields;
begin
  MasterQuery.SQL.Text := 'SELECT * FROM department ORDER BY dep_id';
  MasterQuery.Open;

  DetailQuery.SQL.Text := 'SELECT * FROM people';
  DetailQuery.MasterSource := MasterDataSource;
  DetailQuery.MasterFields := 'dep_id';
  DetailQuery.IndexFieldNames := 'p_dep_id';
  DetailQuery.Open;

  MasterQuery.First;
  CheckEquals(1, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(1, DetailQuery.FieldByName('p_dep_id').AsInteger);

  MasterQuery.Next;
  CheckEquals(2, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(2, DetailQuery.FieldByName('p_dep_id').AsInteger);
end;

initialization
  RegisterTest('component',TZTestMasterDetailCase.Suite);
end.
