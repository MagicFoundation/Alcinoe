{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Cases for MSSql Component Bug Reports       }
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

unit ZTestBugCompMSSql;

interface

{$I ZBugReport.inc}

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  Classes, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDataset, ZConnection,
  ZDbcIntfs, ZBugReport,ZCompatibility;

type

  {** Implements a bug report test case for MSSql components. }
  TZTestCompMSSqlBugReport = class(TZSpecificSQLBugReportTestCase)
  private
    FConnection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: TZConnection read FConnection write FConnection;
    procedure Test728955;
    procedure Test833489;
    procedure Test907497;
    procedure Test959307;
  published
    procedure Test953072;
  end;

implementation

uses ZStoredProcedure, ZTestCase;

{ TZTestCompMSSqlBugReport }

function TZTestCompMSSqlBugReport.GetSupportedProtocols: string;
begin
  Result := 'mssql';
end;

procedure TZTestCompMSSqlBugReport.SetUp;
begin
  Connection := CreateDatasetConnection;
  Connection.Connect;
end;

procedure TZTestCompMSSqlBugReport.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Access Violation during ZReadOnlyQuery.Open
  In method TZAbstractRODataset.InternalInitFieldDefs: 
}
procedure TZTestCompMSSqlBugReport.Test728955;
var
  Query: TZReadOnlyQuery;
begin
  if SkipClosed then Exit;

  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT * FROM department';
    Query.Open;
    CheckEquals(2, Query.FieldCount);

    CheckEquals(1, Query.FieldByName('dep_id').AsInteger);
    CheckEquals('Container agency', Query.FieldByName('dep_name').AsString);
    Query.Next;
    CheckEquals(2, Query.FieldByName('dep_id').AsInteger);
    CheckEquals('Line agency', Query.FieldByName('dep_name').AsString);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #833489
   AutoCommit=FALSE starting a transaction causing an error
}
procedure TZTestCompMSSqlBugReport.Test833489;
begin
  Connection.Disconnect;
  Connection.AutoCommit := False;
  Connection.Connect;
end;

procedure TZTestCompMSSqlBugReport.Test907497;
var
  StoredProc: TZStoredProc;
begin
  StoredProc := TZStoredProc.Create(nil);
  try
    StoredProc.Connection := Connection;
    StoredProc.StoredProcName := 'proc907497';
    StoredProc.ParamByName('@zzz').AsInteger := 12345;
    StoredProc.ExecProc;
    CheckEquals(7890, StoredProc.ParamByName('@zzz').AsInteger);
  finally
    StoredProc.Free;
  end;
end;

{**
   test for Bug#953072 - problem with queries with empty owner name
}
procedure TZTestCompMSSqlBugReport.Test953072;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from master..sysobjects';
    Query.Open;
    CheckEquals(False, Query.IsEmpty);
  finally
    Query.Free;
  end;  
end;

{**
  test for Bug#959307 - empty string parameter translate as null
}
procedure TZTestCompMSSqlBugReport.Test959307;
var
  Query: TZQuery;
  StoredProc: TZStoredProc;
begin
  StoredProc := TZStoredProc.Create(nil);
  Query := TZQuery.Create(nil);
  try
    StoredProc.Connection := Connection;
    StoredProc.StoredProcName := 'proc959307';
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from table959307';

    StoredProc.ParamByName('@p').AsString := 'xyz';
    StoredProc.ExecProc;
    Query.Open;
    CheckEquals('xyz', Query.FieldByName('fld1').AsString);
    Query.Close;

    StoredProc.ParamByName('@p').AsString := '';
    StoredProc.ExecProc;
    Query.Open;
    CheckEquals('', Query.FieldByName('fld1').AsString);
    CheckEquals(False, Query.FieldByName('fld1').IsNull);
    Query.Close;

    StoredProc.ParamByName('@p').Value := Null;
    StoredProc.ExecProc;
    CheckEquals('', Query.FieldByName('fld1').AsString);
    CheckEquals(True, Query.FieldByName('fld1').IsNull);
    Query.Close;
  finally
    StoredProc.Free;
    Query.Free;
  end;
end;

initialization
  RegisterTest('bugreport',TZTestCompMSSqlBugReport.Suite);
end.
