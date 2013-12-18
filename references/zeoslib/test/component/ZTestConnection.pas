{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Connection Components          }
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

unit ZTestConnection;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, ZSqlStrings, SysUtils, ZConnection, ZTestDefinitions;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestConnectionCase = class(TZComponentPortableSQLTestCase)
  private
    gloUserName,gloPassword : string;
    Connection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ConnLogin(Sender: TObject; var Username:string ; var Password: string);
  published
    procedure TestExecuteDirect;
    procedure TestExecuteDirect2;
    procedure TestLoginPromptConnection;
  end;

implementation

uses Classes, ZDbcUtils, ZTestConsts, ZDbcIntfs;

{ TZTestExecSQLCase }

{**
  Prepares initial data before each test.
}
procedure TZTestConnectionCase.SetUp;
begin
  Connection := CreateDatasetConnection;
  Connection.Connect;
end;

{**
  Removes data after each test.
}
procedure TZTestConnectionCase.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Runs a test for ExecuteDirect.
}
procedure TZTestConnectionCase.TestExecuteDirect;
var
  l_bool : boolean;
begin
  l_bool := Connection.ExecuteDirect('insert into department (dep_id,dep_name) Values (89,''Dept89'')');
  CheckEquals(true, l_bool);
  l_bool := Connection.ExecuteDirect('delete from department where dep_id = 89');
  CheckEquals(true, l_bool);
end;

{**
  Runs a test for ExecuteUpdateDirect.
}
procedure TZTestConnectionCase.TestExecuteDirect2;
var
  l_int  : integer;
  l_bool : boolean;
begin
  l_bool := Connection.ExecuteDirect('insert into department (dep_id,dep_name) Values (87,''Dept87'')',l_int);
  CheckEquals(true, l_bool);
  CheckEquals(1, l_int);
  l_bool := Connection.ExecuteDirect('insert into department (dep_id,dep_name) Values (88,''Dept88'')',l_int);
  CheckEquals(true, l_bool);
  CheckEquals(1, l_int);
  l_bool := Connection.ExecuteDirect('delete from department where dep_id between 87 and 88',l_int);
  CheckEquals(true, l_bool);
  CheckEquals(2, l_int);
  l_bool := Connection.ExecuteDirect('delete from department where dep_id between 87 and 88',l_int);
  CheckEquals(true, l_bool);
  CheckEquals(0, l_int);
end;

procedure TZTestConnectionCase.TestLoginPromptConnection;
var
    locUserName,locPassword : string;
begin
   locUserName := Connection.User;
   locPassword := Connection.Password;
   Connection.Disconnect;
   Connection.LoginPrompt := true;
   Connection.User := '';
   Connection.Password := '';
   gloUserName := '';
   gloPassword := '';
   Connection.OnLogin := ConnLogin;
   try
      Connection.Connect;
   except
      CheckEquals(false,Connection.Connected);
   end;
   gloUserName := locUserName;
   gloPassword := locPassword;
   Connection.Connect;
   CheckEquals(true,Connection.Connected);
end;

procedure TZTestConnectionCase.ConnLogin(Sender: TObject; var Username:string ; var Password: string);
begin
   UserName := gloUserName;
   Password := gloPassword;
end;

initialization
  RegisterTest('component',TZTestConnectionCase.Suite);
end.
