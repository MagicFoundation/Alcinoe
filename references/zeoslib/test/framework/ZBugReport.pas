{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Classes for Testing Framework         }
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

unit ZBugReport;

interface
{$I ZTestFramework.inc}

uses ZCompatibility, ZSqlTestCase;

type

  {** Implements an abstract bug test case. }
  TZAbstractBugReportTestCase = class (TZAbstractSQLTestCase)
  protected
    function SkipClosed: Boolean;
  end;

  {** Implements a bug test case which runs all active protocols. }
  TZPortableSQLBugReportTestCase = class (TZAbstractBugReportTestCase)
  protected
    function IsProtocolValid(Name: string): Boolean; override;
    function GetSupportedProtocols: string; override;
  end;

  {**
    Implements a bug test case which runs only active protocols,
      specified by user.
  }
  TZSpecificSQLBugReportTestCase = class (TZAbstractBugReportTestCase);

implementation

uses ZSysUtils, ZTestConfig;

{ TZAbstractBugReportTestCase }

{**
  Checks is closed test cases should be skipped.
  @return <code>True</code> to skip closed test cases.
}
function TZAbstractBugReportTestCase.SkipClosed: Boolean;
begin
  Check(True);
  Result := StrToBoolEx(ReadInheritProperty(SKIP_CLOSED_KEY, FALSE_VALUE));
end;

{ TZPortableSQLBugReportTestCase }

{**
  Gets a comma separated list of all supported by this test protocols.
  @returns a list of all supported protocols.
}
function TZPortableSQLBugReportTestCase.GetSupportedProtocols: string;
begin
  Result := '';
end;

{**
  Function check name prototocol
  @param Name a protocol name
  @result true if protocol valid
}
function TZPortableSQLBugReportTestCase.IsProtocolValid(Name: string): Boolean;
begin
  Result := True;
end;

end.

