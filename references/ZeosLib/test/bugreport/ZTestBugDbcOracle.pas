{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Cases for Interbase DBC Bug Reports         }
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

unit ZTestBugDbcOracle;

interface

{$I ZBugReport.inc}

uses
  Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZBugReport, ZCompatibility,
  ZDbcOracle;

type

  {** Implements a DBC bug report test case for Oracle }
  TZTestDbcOracleBugReport = class(TZSpecificSQLBugReportTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: IZConnection read FConnection write FConnection;
  published
    procedure TestNum1;
  end;

implementation

uses ZTestCase, ZTestConsts;

{ TZTestDbcOracleBugReport }

function TZTestDbcOracleBugReport.GetSupportedProtocols: string;
begin
  Result := 'oracle-9i';
end;

procedure TZTestDbcOracleBugReport.SetUp;
begin
  Connection := CreateDbcConnection;
end;

procedure TZTestDbcOracleBugReport.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

{**
  NUMBER must be froat
}
procedure TZTestDbcOracleBugReport.TestNum1;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  if SkipClosed then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM Table_Num1');
  with ResultSet do
  begin
    with GetMetadata do
    begin
      CheckEquals(ord(stInteger), Ord(GetColumnType(1)), 'id column type');
      CheckEquals(ord(stDouble), Ord(GetColumnType(2)), 'Num column type');
    end;
    CheckEquals(True, Next, 'ResultSet.Next');
    CheckEquals(1, GetInt(1), 'id value');
    CheckEquals(54321.0123456789, GetDouble(2), 1E-11, 'Num value');
    Close;
  end;
end;

initialization
  RegisterTest('bugreport',TZTestDbcOracleBugReport.Suite);
end.
