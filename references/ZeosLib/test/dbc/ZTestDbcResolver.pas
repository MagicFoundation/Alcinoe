{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Case for Cached Resolver Classes          }
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

unit ZTestDbcResolver;

interface
{$I ZDbc.inc}
uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Classes, SysUtils, ZDbcIntfs, ZClasses, ZCompatibility,
  ZCollections, ZDbcGenericResolver, ZTestDefinitions;

type

 {** Implements a test case for CachedResolver classes. }
  TZTestCachedResolverCase = class(TZDbcPortableSQLTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    property Connection: IZConnection read FConnection write FConnection;
  published
    procedure TestGenericResolver;
    {$IFDEF ENABLE_INTERBASE}procedure TestInterbaseResolver;{$ENDIF}
    {$IFDEF ENABLE_MYSQL}procedure TestMySqlResolverPosts;{$ENDIF}
  end;

implementation

uses ZSysUtils, ZTestConsts
     {$IFDEF ENABLE_POSTGRESQL}, ZDbcPostgreSql{$ENDIF}
     {$IFDEF ENABLE_MYSQL}, ZDbcMySql, ZDbcMySqlStatement, ZDbcMySqlResultSet{$ENDIF};

{ TZTestCachedResolverCase }

{**
   Create objects and allocate memory for variables
}
procedure TZTestCachedResolverCase.SetUp;
begin
  Connection := CreateDbcConnection;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestCachedResolverCase.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

{**
  Runs a test for GenericCachedResolver class.
}
procedure TZTestCachedResolverCase.TestGenericResolver;
(*
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Resolver: TZGenericCachedResolver;
  Columns: TObjectList;
*)
begin
  if StartsWith(Protocol, 'interbase')
    or StartsWith(Protocol, 'firebird') then
    Exit;
(*
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  Resolver := TZGenericCachedResolver.Create(Statement, ResultSet.GetMetadata);
  Columns := TObjectList.Create;

  try
    CheckEquals('INSERT INTO department (dep_id,dep_name,dep_shname,dep_address)'
      + ' VALUES (?,?,?,?)', Resolver.GetInsertSQL);
    CheckEquals(4, Resolver.GetInsertParams.Count);
    CheckEquals('UPDATE department SET dep_id=?,dep_name=?,dep_shname=?,'
      + 'dep_address=? WHERE dep_id=?', Resolver.GetUpdateSQL);
    CheckEquals(5, Resolver.GetUpdateParams.Count);
    CheckEquals('DELETE FROM department WHERE dep_id=?', Resolver.GetDeleteSQL);
    CheckEquals(1, Resolver.GetDeleteParams.Count);

    ResultSet := Statement.ExecuteQuery('SELECT t.dep_id AS id, dep_name as name, '
      + ' t.dep_shname, 2+2 as dep_address FROM department as t where dep_id < 100');
    Resolver := TZGenericCachedResolver.Create(Statement, ResultSet.GetMetadata);

    CheckEquals('INSERT INTO department (dep_id,dep_name,dep_shname)'
      + ' VALUES (?,?,?)', Resolver.GetInsertSQL);
    CheckEquals(3, Resolver.GetInsertParams.Count);
    CheckEquals('UPDATE department SET dep_id=?,dep_name=?,dep_shname=?'
      + ' WHERE dep_id=?', Resolver.GetUpdateSQL);
    CheckEquals(4, Resolver.GetUpdateParams.Count);
    CheckEquals('DELETE FROM department WHERE dep_id=?', Resolver.GetDeleteSQL);
    CheckEquals(1, Resolver.GetDeleteParams.Count);
  finally
    Resolver.Free;
    Columns.Free;
  end;
*)
end;

{$IFDEF ENABLE_INTERBASE}
{**
  Runs a test for posts of Interbase CachedResolver class.
}
procedure TZTestCachedResolverCase.TestInterbaseResolver;
(*
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Resolver: TZGenericCachedResolver;
*)
begin
  if not StartsWith(Protocol, 'interbase') then Exit;
(*
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  Resolver := TZGenericCachedResolver.Create(Statement, ResultSet.GetMetadata);

  try
    CheckEquals('INSERT INTO DEPARTMENT (DEP_ID,DEP_NAME,DEP_SHNAME,DEP_ADDRESS)'
      + ' VALUES (?,?,?,?)', Resolver.GetInsertSQL);
    CheckEquals(4, Resolver.GetInsertParams.Count);

    CheckEquals('UPDATE DEPARTMENT SET DEP_ID=?,DEP_NAME=?,DEP_SHNAME=?,'
      + 'DEP_ADDRESS=? WHERE DEP_ID=?', Resolver.GetUpdateSQL);
    CheckEquals(5, Resolver.GetUpdateParams.Count);

    CheckEquals('DELETE FROM DEPARTMENT WHERE DEP_ID=?', Resolver.GetDeleteSQL);
    CheckEquals(1, Resolver.GetDeleteParams.Count);

    ResultSet := Statement.ExecuteQuery('SELECT t.dep_id AS id, dep_name as name, '
      + ' t.dep_shname, 2+2 as dep_address FROM DEPARTMENT t where DEP_ID < 100');
    Resolver := TZGenericCachedResolver.Create(Statement, ResultSet.GetMetadata);

    CheckEquals('INSERT INTO DEPARTMENT (DEP_ID,DEP_NAME,DEP_SHNAME)'
      + ' VALUES (?,?,?)', Resolver.GetInsertSQL);
    CheckEquals(3, Resolver.GetInsertParams.Count);

    CheckEquals('UPDATE DEPARTMENT SET DEP_ID=?,DEP_NAME=?,DEP_SHNAME=?'
      + ' WHERE DEP_ID=?', Resolver.GetUpdateSQL);
    CheckEquals(4, Resolver.GetUpdateParams.Count);

    CheckEquals('DELETE FROM DEPARTMENT WHERE DEP_ID=?', Resolver.GetDeleteSQL);
    CheckEquals(1, Resolver.GetDeleteParams.Count);
  finally
    Resolver.Free;
  end;
*)
end;
{$ENDIF}

{$IFDEF ENABLE_MYSQL}
{**
  Runs a test for MySQL Resolver.
}
procedure TZTestCachedResolverCase.TestMySqlResolverPosts;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  WhereClause: string;
begin
  if not StartsWith(Protocol, 'mysql') then
    Exit;

  Statement := Connection.CreateStatement;
  WhereClause := 'WHERE dep_id=' + IntToStr(TEST_ROW_ID);

  { Deletes all existed rows. }
  Statement.ExecuteUpdate('DELETE FROM department ' + WhereClause);

  { Inserts a new row. }
  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department ' + WhereClause);
  CheckEquals(False, ResultSet.Next);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(1, TEST_ROW_ID);
  ResultSet.UpdateString(2, 'AAA');
  ResultSet.UpdateString(3, 'BBB');
  ResultSet.UpdateString(4, 'XXX');
  ResultSet.InsertRow;

  { Updates the row. }
  ResultSet.UpdateString(4, 'CCC');
  ResultSet.UpdateRow;

  { Reads the row and removes it. }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department ' + WhereClause);
  CheckEquals(True, ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetInt(1));
  CheckEquals('AAA', ResultSet.GetString(2));
  CheckEquals('BBB', ResultSet.GetString(3));
  CheckEquals('CCC', ResultSet.GetString(4));
  ResultSet.DeleteRow;

  { Checks the removed row. }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department ' + WhereClause);
  CheckEquals(False, ResultSet.Next);
end;
{$ENDIF}

initialization
  RegisterTest('dbc',TZTestCachedResolverCase.Suite);
end.
