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

unit ZTestStoredProcedure;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken,
  ZConnection, ZDataset, ZTestDefinitions, ZStoredProcedure;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestInterbaseStoredProcedure = class(TZComponentSpecificSQLTestCase)
  private
    Connection: TZConnection;
    StoredProc: TZStoredProc;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;
    function GetConnectionUrl: string;

  published
    procedure TestStoredProc;
  end;

    {** Implements a test case for class TZReadOnlyQuery. }
  TZTestDbLibStoredProcedure = class(TZComponentSpecificSQLTestCase)
  private
    Connection: TZConnection;
    StoredProc: TZStoredProc;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;
    function GetConnectionUrl: string;

  published
    procedure TestStoredProc;
  end;

implementation

uses Classes, ZSysUtils, ZDbcUtils, ZTestConsts, ZDbcIntfs, ZAbstractDataset,
  ZTestCase;

{ TZTestStoredProcedure }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestInterbaseStoredProcedure.GetSupportedProtocols: string;
begin
  Result := 'interbase,interbase-6.5,interbase-7.2,firebird-1.0,firebird-1.5,firebird-2.0,firebird-2.1,firebirdd-1.5,firebirdd-2.0,firebirdd-2.1';
end;

{**
  Gets a connection URL string.
  @return a built connection URL string.
}
function TZTestInterbaseStoredProcedure.GetConnectionUrl: string;
begin
  if Port <> 0 then
    Result := Format('zdbc:%s://%s:%d/%s', [Protocol, HostName, Port, Database])
  else Result := Format('zdbc:%s://%s/%s', [Protocol, HostName, Database]);
end;

{**
  Prepares initial data before each test.
}
procedure TZTestInterbaseStoredProcedure.SetUp;
begin
  Connection := CreateDatasetConnection;
  Connection.Connect;
  StoredProc := TZStoredProc.Create(nil);
  StoredProc.Connection := Connection;
  StoredProc.ParamCheck := True;
end;

{**
  Removes data after each test.
}
procedure TZTestInterbaseStoredProcedure.TearDown;
begin
  StoredProc.Close;
  StoredProc.Free;
  Connection.Disconnect;
  Connection.Free;
end;

{**
   Testing executil stored procedures
}
procedure TZTestInterbaseStoredProcedure.TestStoredProc;
begin
  StoredProc.StoredProcName := 'PROCEDURE1';

  CheckEquals(2, StoredProc.Params.Count);
  CheckEquals('R1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[0].ParamType));
  CheckEquals('P1', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  StoredProc.Params[1].AsInteger := 12345;
  StoredProc.ExecProc;
  CheckEquals(12346, StoredProc.Params[0].AsInteger);
  CheckEquals(2, StoredProc.Params.Count);
end;

{ TZTestDbLibStoredProcedure }

{**
  Gets a connection URL string.
  @return a built connection URL string.
}
function TZTestDbLibStoredProcedure.GetConnectionUrl: string;
begin
  if Port <> 0 then
    Result := Format('zdbc:%s://%s:%d/%s', [Protocol, HostName, Port, Database])
  else Result := Format('zdbc:%s://%s/%s', [Protocol, HostName, Database]);
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbLibStoredProcedure.GetSupportedProtocols: string;
begin
  Result := 'sybase, mssql';
end;

{**
  Prepares initial data before each test.
}
procedure TZTestDbLibStoredProcedure.SetUp;
begin
  Connection := CreateDatasetConnection;
  Connection.Connect;
  StoredProc := TZStoredProc.Create(nil);
  StoredProc.Connection := Connection;
  StoredProc.ParamCheck := True;
end;

{**
  Removes data after each test.
}
procedure TZTestDbLibStoredProcedure.TearDown;
begin
  StoredProc.Close;
  StoredProc.Free;
  Connection.Disconnect;
  Connection.Free;
end;

{**
   Testing executil stored procedures
}
procedure TZTestDbLibStoredProcedure.TestStoredProc;
begin
  StoredProc.StoredProcName := 'procedure1';

  CheckEquals(3, StoredProc.Params.Count);
  CheckEquals('@RETURN_VALUE', StoredProc.Params[0].Name);
  CheckEquals('@p1', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('@r1', StoredProc.Params[2].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[0].ParamType));

  StoredProc.Params[1].AsInteger := 12345;
  StoredProc.ExecProc;
  CheckEquals(12346, StoredProc.Params[1].AsInteger);
  CheckEquals(2, StoredProc.Params.Count);
end;

initialization
  RegisterTest('component',TZTestInterbaseStoredProcedure.Suite);
  RegisterTest('component',TZTestDbLibStoredProcedure.Suite);
end.
