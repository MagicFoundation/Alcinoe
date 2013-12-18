{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Test Suite for Bug Reports                 }
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

program ZTestBugReport;

{$IFNDEF TESTGUI}
{$APPTYPE CONSOLE}
{$ENDIF}

{$I ../../test/bugreport/ZBugReport.inc}

uses
  TestFrameWork,
  TextTestRunner,
  ZTestConfig,
  ZSqlTestCase,
  ZTestBugDbcCore in '../../test/bugreport/ZTestDbcCore.pas',
  ZTestBugDbcMySql in '../../test/bugreport/ZTestDbcMySql.pas',
  ZTestBugDbcOracle in '../../test/dbc/ZTestDbcOracle.pas',
  ZTestBugDbcPostgreSql in '../../test/bugreport/ZTestDbcBugPostgreSql.pas',
  ZTestBugDbcInterbase in '../../test/bugreport/ZTestBugDbcInterbase.pas',
  ZTestBugDbcDbLib in '../../test/bugreport/ZTestBugDbcDbLib.pas',
  ZTestBugCompCore in '../../test/bugreport/ZTestBugCompCore.pas',
  ZTestBugCompMySql in '../../test/bugreport/ZTestBugCompMySql.pas',
  ZTestBugCompMSSql in '../../test/bugreport/ZTestBugCompMSSql.pas',
  ZTestBugCompOracle in '../../test/dbc/ZTestDbcCompOracle.pas',
  ZTestBugCompPostgreSql in '../../test/bugreport/ZTestBugCompPostgreSql.pas',
  ZTestBugCompInterbase in '../../test/bugreport/ZTestBugCompInterbase.pas',
  ZTestBugCompDbLib in '../../test/bugreport/ZTestBugCompDbLib.pas';

begin
  TestGroup := BUGREPORT_TEST_GROUP;
  RebuildTestDatabases;
{$IFDEF TESTGUI}
  GUITestRunner.RunRegisteredTests;
{$ELSE}
  TextTestRunner.RunRegisteredTests;
{$ENDIF}
end.
