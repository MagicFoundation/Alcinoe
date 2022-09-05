{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Generic Test Case for Midas (DataSnap) components     }
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

unit ZMidasTestCase;

interface

{$I ZTestFramework.inc}

{$WARN SYMBOL_PLATFORM OFF}

uses
{$IFNDEF VER140BELOW}
  TConnect,
{$ENDIF}
  Classes, DB, TestFramework, ZConnection, ZDataset, ZSqlTestCase, DataBkr,
  Provider, DBClient, MConnect, ZMidasTestServer_TLB;

type

  {** Implements a Data Module for Local Data Provider. }
  TZRemoteDM = class(TRemoteDataModule, IZRemoteDM)
    DSProvider: TDataSetProvider;
    FMasterSource: TDataSource;

    procedure RemoteDataModuleCreate(Sender: TObject);

  private
    FConnection: TZConnection;
    FQuery: TZQuery;
    FDetailQuery: TZQuery;

  public
    procedure SetOptions(const Protocol: WideString;
      const HostName: WideString; Port: Integer;
      const Database: WideString; const UserName: WideString;
      const Password: WideString); safecall;
    procedure MasterDetail(const Value: Integer); safecall;

    class procedure UpdateRegistry(Register: Boolean;
      const ClassID, ProgID: string); override;

    property Provider: TDataSetProvider read DSProvider write DSProvider;
    property MasterSource: TDataSource read FMasterSource write FMasterSource;
    property Connection: TZConnection read FConnection write FConnection;
    property Query: TZQuery read FQuery write FQuery;
    property DetailQuery: TZQuery read FDetailQuery write FDetailQuery;
  end;

  {** Implements a generic test case for Midas (DataSnap) components. }
  TZMidasPortableSQLTestCase = class(TZPortableSQLTestCase)
  private
    FRemoteDM: TZRemoteDM;
    FConnection: TLocalConnection;
    FDataSet: TClientDataSet;

  protected
    property RemoteDM: TZRemoteDM read FRemoteDM write FRemoteDM;
    property Connection: TLocalConnection read FConnection write FConnection;
    property DataSet: TClientDataSet read FDataSet write FDataSet;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

{$R *.tlb}
{$R *.dfm}

{ TZRemoteDM }

{**
  Performs initialization of the data module on create event.
  @param Sender a sender object reference.
}
procedure TZRemoteDM.RemoteDataModuleCreate(Sender: TObject);
begin
  Connection := TZConnection.Create(Self);

  Query := TZQuery.Create(Self);
  Query.Connection := Connection;

  DetailQuery := TZQuery.Create(Self);
  DetailQuery.Connection := Connection;

  MasterSource.DataSet := Query;
  Provider.DataSet := Query;
end;

procedure TZRemoteDM.SetOptions(const Protocol, HostName: WideString;
  Port: Integer; const Database, UserName, Password: WideString);
begin
  Connection.Protocol := Protocol;
  Connection.HostName := HostName;
  Connection.Port := Port;
  Connection.Database := Database;
  Connection.User := UserName;
  Connection.Password := Password;
  Connection.Connect;
end;

procedure TZRemoteDM.MasterDetail(const Value: Integer);
begin
  if Value = 0 then
  begin
    Query.SQL.Text := 'select * from department';
    DetailQuery.SQL.Text := 'select * from people';
    DetailQuery.MasterSource := MasterSource;
    DetailQuery.IndexFieldNames := 'p_dep_id';
    DetailQuery.MasterFields := 'dep_id';
    Query.Open;
    DetailQuery.Open;
  end
  else
  begin
    DetailQuery.MasterSource := nil;
    DetailQuery.IndexFieldNames := '';
    DetailQuery.MasterFields := '';
    DetailQuery.Close;
  end;
end;

{**
  Updates a data module in a registry.
  @param Register <code>True</code> to register data module
    and <code>False</code> to unregister it.
  @param ClassID a GUID of data module class.
  @param ProgID a GUID of data module program.
}
class procedure TZRemoteDM.UpdateRegistry(Register: Boolean; const ClassID,
  ProgID: string);
begin
  if Register then
  begin
    inherited UpdateRegistry(Register, ClassID, ProgID);
    EnableSocketTransport(ClassID);
    EnableWebTransport(ClassID);
  end
  else
  begin
    DisableSocketTransport(ClassID);
    DisableWebTransport(ClassID);
    inherited UpdateRegistry(Register, ClassID, ProgID);
  end;
end;

{ TZMidasPortableSQLTestCase }

{**
  Sets up the test main properties.
}
procedure TZMidasPortableSQLTestCase.SetUp;
begin
  FRemoteDM := TZRemoteDM.Create(nil);
  FConnection := TLocalConnection.Create(FRemoteDM);
  FRemoteDM.SetOptions(Protocol, HostName, Port,
    Database, UserName, Password);

  FDataSet := TClientDataSet.Create(nil);
  FDataSet.ProviderName := 'DSProvider';
  FDataSet.RemoteServer := FConnection;
end;

{**
  Frees the test main properties.
}
procedure TZMidasPortableSQLTestCase.TearDown;
begin
  FDataSet.Close;
  FDataSet.Free;
  FConnection.Free;
  FRemoteDM.Free;
end;

end.
