{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for SQL String Classes             }
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

unit ZTestSqlStrings;

interface
{$I ZComponent.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTestCase, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken;

type

  {** Implements a test case for class TZSqlStrings. }
  TZTestSQLStringsCase = class(TZAbstractTestCase)
  private
    SQLStrings: TZSQLStrings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStatements;
    procedure TestParams;
    procedure TestParamChar;
    procedure TestUncompleted;
 end;

implementation

uses Classes, ZDbcUtils;

{ TZTestSqlStringsCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLStringsCase.SetUp;
begin
  SQLStrings := TZSQLStrings.Create;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLStringsCase.TearDown;
begin
  SQLStrings.Free;
  SQLStrings := nil;
end;

{**
  Runs a test for SQL parameters.
}
procedure TZTestSQLStringsCase.TestParams;
var
  SQLScript: string;
begin
  SQLScript := 'INSERT INTO department VALUES (:ID, :NAME, :NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=:NEW_NAME, dep_address=:NEW_ADDRESS'
    + ' WHERE id_dep=:Id AND dep_name=:Name;';
  SQLStrings.Text := SQLScript;

  CheckEquals(4, SQLStrings.ParamCount);
  CheckEquals('ID', SQLStrings.ParamNames[0]);
  CheckEquals('NAME', SQLStrings.ParamNames[1]);
  CheckEquals('NEW_ADDRESS', SQLStrings.ParamNames[2]);
  CheckEquals('NEW_NAME', SQLStrings.ParamNames[3]);

  SQLStrings.Clear;
  CheckEquals(0, SQLStrings.ParamCount);
end;

{**
  Runs a test for SQL parameters delimited by non standard parameter marker.
}
procedure TZTestSQLStringsCase.TestParamChar;
var
  SQLScript: string;
begin
  SQLScript := 'INSERT INTO department VALUES (:ID, :NAME, :NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=:NEW_NAME, dep_address=:NEW_ADDRESS'
    + ' WHERE id_dep=:Id AND dep_name=:Name;';
  SQLStrings.Text := SQLScript;
  CheckEquals(4, SQLStrings.ParamCount);
  SQLStrings.ParamChar := '&';
  CheckEquals(0, SQLStrings.ParamCount);

  SQLScript := 'INSERT INTO department VALUES (&ID, &NAME, &NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=&NEW_NAME, dep_address=&NEW_ADDRESS'
    + ' WHERE id_dep=&Id AND dep_name=&Name;';
  SQLStrings.Text := SQLScript;
  CheckEquals(4, SQLStrings.ParamCount);
  CheckEquals('ID', SQLStrings.ParamNames[0]);
  CheckEquals('NAME', SQLStrings.ParamNames[1]);
  CheckEquals('NEW_ADDRESS', SQLStrings.ParamNames[2]);
  CheckEquals('NEW_NAME', SQLStrings.ParamNames[3]);

  Try
    // Failure expected when ParamChar isn't seen as a Symbol by the Tokenizer
    // U is interpreted as the start of a normal word by all tokenizers
    SQLStrings.ParamChar := 'U';
    Fail('Wrong behaviour when setting ParamChar to U');
    except
      // Ignore.
  end;
end;

{**
  Runs a test for SQL statements.
}
procedure TZTestSQLStringsCase.TestStatements;
var
  SQLScript: string;
begin
  SQLScript := 'INSERT INTO department VALUES (:ID, :NAME, :NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=:NEW_NAME, dep_address=:NEW_ADDRESS'
    + ' WHERE id_dep=:Id AND dep_name=:Name;';
  SQLStrings.Text := SQLScript;

  CheckEquals(2, SQLStrings.StatementCount);
  CheckEquals('INSERT INTO department VALUES (?, ?, ?)',
    SQLStrings.Statements[0].SQL);
  CheckEquals(3, SQLStrings.Statements[0].ParamCount);
  CheckEquals('ID', SQLStrings.Statements[0].ParamNames[0]);
  CheckEquals('NAME', SQLStrings.Statements[0].ParamNames[1]);
  CheckEquals('NEW_ADDRESS', SQLStrings.Statements[0].ParamNames[2]);

  CheckEquals('UPDATE department SET dep_name=?, dep_address=?'
    + ' WHERE id_dep=? AND dep_name=?', SQLStrings.Statements[1].SQL);
  CheckEquals(4, SQLStrings.Statements[1].ParamCount);
  CheckEquals('NEW_NAME', SQLStrings.Statements[1].ParamNames[0]);
  CheckEquals('NEW_ADDRESS', SQLStrings.Statements[1].ParamNames[1]);
  CheckEquals('ID', SQLStrings.Statements[1].ParamNames[2]);
  CheckEquals('NAME', SQLStrings.Statements[1].ParamNames[3]);
end;

{**
  Runs a test for uncompleted SQL statements.
}
procedure TZTestSQLStringsCase.TestUncompleted;
var
  SQLScript: string;
begin
  SQLScript := 'SELECT * FROM people;' + #10 + 'SELECT * FROM cargo;';
  SQLStrings.Text := SQLScript;

  CheckEquals(2, SQLStrings.StatementCount);
  CheckEquals('SELECT * FROM people', SQLStrings.Statements[0].SQL);
  CheckEquals('SELECT * FROM cargo', SQLStrings.Statements[1].SQL);

  SQLScript := 'SELECT * FROM people;' + #10 + 'SELECT * FROM cargo';
  SQLStrings.Text := SQLScript;

  CheckEquals(2, SQLStrings.StatementCount);
  CheckEquals('SELECT * FROM people', SQLStrings.Statements[0].SQL);
  CheckEquals('SELECT * FROM cargo', SQLStrings.Statements[1].SQL);
end;

initialization
  RegisterTest('component',TZTestSQLStringsCase.Suite);
end.
