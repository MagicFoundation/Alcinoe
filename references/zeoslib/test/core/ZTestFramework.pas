{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Testing Framework              }
{                                                         }
{         Originally written by Sergey Merkuriev          }
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

unit ZTestFramework;

interface

{$I ZCore.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZSysUtils, SysUtils, ZTestDefinitions, ZCompatibility;

type

  {** Implements a test case for TZPortableSQLTestCase. }
  TZTestPortableSQLTestCase = class(TZCorePortableSQLTestCase)
  published
    procedure TestOne;
    procedure TestTwo;
    procedure TestTree;
  end;

  {** Implements a test case for TZSpecificSQLTestCase. }
  TZTestSpecificSQLTestCase = class(TZCoreSpecificSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestOne;
    procedure TestTwo;
    procedure TestTree;
  end;

implementation


{ TZTestPortableSQLTestCase }

{**
  Runs the first test.
}
procedure TZTestPortableSQLTestCase.TestOne;
begin
  Check(True);
  PrintLn('*** Test # 1 ***');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'mysql') then
    PrintLn('--- Part specific for mysql');
  PrintLn;
end;

{**
  Runs the second test.
}
procedure TZTestPortableSQLTestCase.TestTwo;
begin
  Check(True);
  PrintLn('*** Test # 2 ***');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'postgresql') then
    PrintLn('--- Part specific for postgresql');
  PrintLn;
end;

{**
  Runs the third test.
}
procedure TZTestPortableSQLTestCase.TestTree;
begin
  Check(True);
  PrintLn('*** Test # 3 ***');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'mssql') then
    PrintLn('--- Part specific for mssql');
  PrintLn;
end;

{ TZTestSpecificSQLTestCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestSpecificSQLTestCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,postgresq,postgresql-7,postgresql-8';
end;

{**
  Runs the first test.
}
procedure TZTestSpecificSQLTestCase.TestOne;
begin
  Check(True);
  PrintLn('### Test # 1 ###');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'mysql') then
    PrintLn('--- Part specific for mysql');
  PrintLn;
end;

{**
  Runs the second test.
}
procedure TZTestSpecificSQLTestCase.TestTwo;
begin
  Check(True);
  PrintLn('### Test # 2 ###');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'postgresql') then
    PrintLn('--- Part specific for postgresql');
  PrintLn;
end;

{**
  Runs the third test.
}
procedure TZTestSpecificSQLTestCase.TestTree;
begin
  Check(True);
  PrintLn('### Test # 3 ###');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'mysql') then
    PrintLn('--- Part specific for mysql');
  PrintLn;
end;

initialization
  RegisterTest('core',TZTestPortableSQLTestCase.Suite);
  RegisterTest('core',TZTestSpecificSQLTestCase.Suite);
end.
