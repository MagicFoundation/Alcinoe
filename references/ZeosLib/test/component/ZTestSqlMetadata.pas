{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Test Case for SQL Metadata Dataset            }
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

unit ZTestSqlMetadata;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, SysUtils, ZConnection, ZSqlMetadata, ZTestDefinitions;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestSQLMetadataCase = class(TZComponentPortableSQLTestCase)
  private
    Connection: TZConnection;
    Metadata: TZSQLMetadata;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMainDatasets;
  end;

implementation

uses Classes, ZDbcUtils, ZSysUtils, ZTestConsts, ZDbcIntfs;

{ TZTestSQLMetadataCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLMetadataCase.SetUp;
begin
  Connection := CreateDatasetConnection;

  Metadata := TZSQLMetadata.Create(nil);
  Metadata.Connection := Connection;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLMetadataCase.TearDown;
begin
  Metadata.Close;
  Metadata.Free;

  Connection.Disconnect;
  Connection.Free;
end;

{**
  Runs a test for main datasets.
}
procedure TZTestSQLMetadataCase.TestMainDatasets;
begin
  Metadata.MetadataType := mdTables;
  Metadata.Open;
  try
    Check(Metadata.RecordCount > 0);
  finally
    Metadata.Close;
  end;

  if StartsWith(Protocol, 'interbase')
    or StartsWith(Protocol, 'firebird')
    or StartsWith(Protocol, 'oracle') then
    Metadata.TableName := 'PEOPLE'
  else
    Metadata.TableName := 'people';

  Metadata.MetadataType := mdColumns;
  Metadata.Open;
  try
    Check(Metadata.RecordCount > 0);
  finally
    Metadata.Close;
  end;
end;

initialization
  RegisterTest('component',TZTestSQLMetadataCase.Suite);
end.
