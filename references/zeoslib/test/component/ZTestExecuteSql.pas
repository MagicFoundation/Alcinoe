{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Query Components               }
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

unit ZTestExecuteSql;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken,
  ZConnection, ZDataset, ZTestDefinitions;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestExecSQLCase = class(TZComponentPortableSQLTestCase)
  private
    Connection: TZConnection;
    Query: TZReadOnlyQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParams;
    procedure TestKeepParams;
  end;

implementation

uses Classes, ZDbcUtils, ZTestConsts, ZDbcIntfs;

{ TZTestExecSQLCase }

{**
  Prepares initial data before each test.
}
procedure TZTestExecSQLCase.SetUp;
begin
  Connection := CreateDatasetConnection;

  Query := TZReadOnlyQuery.Create(nil);
  Query.Connection := Connection;
  Query.ParamCheck := True;
end;

{**
  Removes data after each test.
}
procedure TZTestExecSQLCase.TearDown;
begin
  Query.Close;
  Query.Free;
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Runs a test for SQL parameters.
}
procedure TZTestExecSQLCase.TestParams;
begin
  Query.SQL.Text := 'DELETE FROM department WHERE dep_id=:Id';
  CheckEquals(1, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;
  Query.ExecSQL;

  Query.SQL.Text := 'INSERT INTO department (dep_id, dep_name, dep_address)'
    + ' VALUES(:Id, :Name, :Address)';
  CheckEquals(3, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;
  CheckEquals('Name', Query.Params[1].Name);
  Query.Params[1].DataType := ftString;
  Query.Params[1].Value := 'AAA';
  CheckEquals('Address', Query.Params[2].Name);
  Query.Params[2].DataType := ftString;
  Query.Params[2].Value := 'BBB';
  Query.ExecSQL;
  CheckEquals(1, Query.RowsAffected);

  Query.SQL.Text := 'DELETE FROM department WHERE dep_id=:Id';
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;
  Query.ExecSQL;
  CheckEquals(1, Query.RowsAffected);
end;

{**
  Tests keeping parameters across changes in statements.
}
procedure TZTestExecSQLCase.TestKeepParams;
begin
  Query.SQL.Text := 'DELETE FROM department WHERE dep_id=:Id';
  CheckEquals(1, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;

  Query.SQL.Text := 'INSERT INTO department (dep_id, dep_name, dep_address)'
    + ' VALUES(:Id, :Name, :Address)';
  CheckEquals(3, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  CheckEquals(TEST_ROW_ID, Query.Params[0].Value);
  CheckEquals(Ord(ftInteger), Ord(Query.Params[0].DataType));
  CheckEquals('Name', Query.Params[1].Name);
  CheckEquals(True, Query.Params[1].IsNull);
  CheckEquals('Address', Query.Params[2].Name);
  CheckEquals(True, Query.Params[2].IsNull);
end;

initialization
  RegisterTest('component',TZTestExecSQLCase.Suite);
end.
