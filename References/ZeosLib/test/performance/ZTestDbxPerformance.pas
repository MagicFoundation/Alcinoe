{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for DBX API Performance            }
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

unit ZTestDbxPerformance;

interface

{$I ZPerformance.inc}

uses TestFramework,
{$IFNDEF EXCLUDE_DBX_TEST}
  DBXpress, FMTBcd, SqlExpr, DB, Provider, DBClient,
{$ENDIF}
  ZCompatibility, ZPerformanceTestCase;

type

  {** Implements a performance test case for DBX TDataset API. }
  TZDBXPerformanceTestCase = class (TZPerformanceSQLTestCase)
  protected
    function GetImplementedAPI: string; override;
    function GetSupportedProtocols: string; override;

{$IFNDEF EXCLUDE_DBX_TEST}
  private
    FQuery: TSQLQuery;
    FConnection: TSQLConnection;
    FClientDataSet : TClientDataSet;
    FDataSetProvider : TDataSetProvider;
  protected
    property Query: TSQLQuery read FQuery write FQuery;
    property Connection: TSQLConnection read FConnection write FConnection;
    property ClientDataSet: TClientDataSet read FClientDataSet write FClientDataSet;
    property DataSetProvider: TDataSetProvider read FDataSetProvider write FDataSetProvider;

    procedure SetUp; override;
    procedure TearDown; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure SetUpTestInsert; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure SetUpTestFetch; override;
    procedure RunTestFetch; override;
    procedure SetUpTestUpdate; override;
    procedure RunTestUpdate; override;
    procedure SetUpTestDelete; override;
    procedure RunTestDelete; override;
{$ENDIF}
  end;

    {** Implements a performance test case for DBXC TDataset API. }
  TZDBXCPerformanceTestCase = class (TZPerformanceSQLTestCase)
  protected
    function GetImplementedAPI: string; override;
    function GetSupportedProtocols: string; override;

{$IFNDEF EXCLUDE_DBX_TEST}
  private
    FQuery: TSQLQuery;
    FConnection: TSQLConnection;
    FClientDataSet : TClientDataSet;
    FDataSetProvider : TDataSetProvider;
  protected
    property Query: TSQLQuery read FQuery write FQuery;
    property Connection: TSQLConnection read FConnection write FConnection;
    property ClientDataSet: TClientDataSet read FClientDataSet write FClientDataSet;
    property DataSetProvider: TDataSetProvider read FDataSetProvider write FDataSetProvider;

    procedure SetUp; override;
    procedure TearDown; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure SetUpTestInsert; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure SetUpTestFetch; override;
    procedure RunTestFetch; override;
    procedure SetUpTestUpdate; override;
    procedure RunTestUpdate; override;
    procedure SetUpTestDelete; override;
    procedure RunTestDelete; override;
{$ENDIF}
  end;

    {** Implements a performance test case for DBXODBC TDataset API. }
  TZDBXODBCPerformanceTestCase = class (TZPerformanceSQLTestCase)
  protected
    function GetImplementedAPI: string; override;
    function GetSupportedProtocols: string; override;

{$IFNDEF EXCLUDE_DBX_TEST}
  private
    FQuery: TSQLQuery;
    FConnection: TSQLConnection;
    FClientDataSet : TClientDataSet;
    FDataSetProvider : TDataSetProvider;
  protected
    property Query: TSQLQuery read FQuery write FQuery;
    property Connection: TSQLConnection read FConnection write FConnection;
    property ClientDataSet: TClientDataSet read FClientDataSet write FClientDataSet;
    property DataSetProvider: TDataSetProvider read FDataSetProvider write FDataSetProvider;

    procedure SetUp; override;
    procedure TearDown; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure SetUpTestInsert; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure SetUpTestFetch; override;
    procedure RunTestFetch; override;
    procedure SetUpTestUpdate; override;
    procedure RunTestUpdate; override;
    procedure SetUpTestDelete; override;
    procedure RunTestDelete; override;
{$ENDIF}
  end;

    {** Implements a performance test case for DBXODBCC TDataset API. }
  TZDBXODBCCPerformanceTestCase = class (TZPerformanceSQLTestCase)
  protected
    function GetImplementedAPI: string; override;
    function GetSupportedProtocols: string; override;

{$IFNDEF EXCLUDE_DBX_TEST}
  private
    FQuery: TSQLQuery;
    FConnection: TSQLConnection;
    FClientDataSet : TClientDataSet;
    FDataSetProvider : TDataSetProvider;
  protected
    property Query: TSQLQuery read FQuery write FQuery;
    property Connection: TSQLConnection read FConnection write FConnection;
    property ClientDataSet: TClientDataSet read FClientDataSet write FClientDataSet;
    property DataSetProvider: TDataSetProvider read FDataSetProvider write FDataSetProvider;

    procedure SetUp; override;
    procedure TearDown; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure SetUpTestInsert; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure SetUpTestFetch; override;
    procedure RunTestFetch; override;
    procedure SetUpTestUpdate; override;
    procedure RunTestUpdate; override;
    procedure SetUpTestDelete; override;
    procedure RunTestDelete; override;
{$ENDIF}
  end;

implementation

uses ZTestCase, ZSysUtils;

{ TZDBXPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZDBXPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dbx';
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZDBXPerformanceTestCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5'
    + ',sybase,interbase,interbase-5,interbase-6'
    + ',firebird-1.0,firebird-1.5,firebird-2.0,firebird-2.1,firebirdd-1.5,firebirdd-2.0,firebirdd-2.1'
    + ',postgresql,postgresql-7,postgresql-8';
{$IFNDEF LINUX}
  Result := Result + ',mssql';
{$ENDIF}
end;

{$IFNDEF EXCLUDE_DBX_TEST}

{**
   Create objects and allocate memory for variables
}
procedure TZDBXPerformanceTestCase.SetUp;
begin
  Connection := TSQLConnection.Create(nil);
  Query := TSQLQuery.Create(nil);
  ClientDataSet := TClientdataset.Create(nil);
  DataSetProvider := TDataSetProvider.Create(nil);
  Connection.ConnectionName := 'SQLConnection';
  Connection.Params.Add('Hostname=' + GetHostName);
  Connection.Params.Add('User_Name=' + GetUserName);
  Connection.Params.Add('Password=' + GetPassword);
  Connection.Params.Add('BlobSize=-1');
  Connection.Params.Add('LocaleCode=0000');
  Connection.LoginPrompt := false;
  Query.SQLConnection := Connection;
  DataSetProvider.DataSet := Query;
  ClientDataSet.SetProvider(DataSetProvider);
  DataSetProvider.Options := [poAllowMultiRecordUpdates];
  DataSetProvider.UpdateMode := upWhereKeyOnly;

  if StartsWith(GetProtocol, 'mysql') then
  begin
    Connection.DriverName := 'MySQL';
    Connection.GetDriverFunc := 'getSQLDriverMYSQL';
    Connection.Params.Add('Database=' + GetDatabase);
{$IFDEF LINUX}
    Connection.LibraryName := 'libsqlmy.so';
    Connection.VendorLib := 'libmysqlclient.so';
    Connection.Params.Add('ErrorResourceFile=./DbxMySqlErr.msg');
{$ELSE}
    Connection.LibraryName := 'dbexpmysql.dll';
    Connection.VendorLib := 'libmysql.dll';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if StartsWith(GetProtocol, 'interbase') then
  begin
    Connection.DriverName := 'IBConnection';
    Connection.GetDriverFunc := 'getSQLDriverINTERBASE';
    Connection.Params.Add('Database='+ GetHostname + ':' + GetDatabase);
    Connection.Params.Add('SQLDialect=1');
    Connection.Params.Add('CommitRetain=False');
    Connection.Params.Add('WaitOnLocks=True');
    Connection.Params.Add('ServerCharSet=');
    //Connection.Params.Add('RoleName=');
{$IFDEF LINUX}
    Connection.LibraryName := 'libsqlib.so';
    Connection.VendorLib := 'libgds.so';
    Connection.Params.Add('ErrorResourceFile=./DbxIbErr.msg');
{$ELSE}
    Connection.LibraryName := 'dbexpint.dll';
    Connection.VendorLib := 'gds32.dll';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if GetProtocol = 'mssql' then
  begin
    Connection.DriverName := 'MSSQL';
    Connection.GetDriverFunc := 'getSQLDriverMSSQL';
    Connection.Params.Add('Database=' + GetDatabase);
{$IFDEF LINUX}
    Connection.LibraryName := 'dbexpmss.dll';
    Connection.VendorLib := 'libgds.so';
    Connection.Params.Add('ErrorResourceFile=./DbxErr.msg');
{$ELSE}
    Connection.LibraryName := 'dbexpmss.dll';
    Connection.VendorLib := 'oledb';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if StartsWith(GetProtocol, 'firebird') then
  begin
    Connection.DriverName := 'IBConnection';
    Connection.GetDriverFunc := 'getSQLDriverINTERBASE';
    Connection.Params.Add('SQLDialect=1');
    Connection.Params.Add('CommitRetain=False');
    Connection.Params.Add('WaitOnLocks=True');
    Connection.Params.Add('ServerCharSet=');
    Connection.Params.Add('Database=' + GetHostname + ':' + GetDatabase);
    //Connection.Params.Add('RoleName=');
{$IFDEF LINUX}
    Connection.LibraryName := 'libsqlib.so';
    Connection.VendorLib := 'libgds.so';
    Connection.Params.Add('ErrorResourceFile=./DbxIbErr.msg');
{$ELSE}
    Connection.LibraryName := 'dbexpint.dll';
    Connection.VendorLib := 'gds32.dll';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if GetProtocol = 'sybase' then
  begin
    Connection.DriverName := 'Sybase';
    Connection.GetDriverFunc := 'getSQLDriverSYBASE';
    Connection.Params.Add('Database=' + GetDatabase);
{$IFDEF LINUX}
    Connection.LibraryName := '';
    Connection.VendorLib := '';
    Connection.Params.Add('ErrorResourceFile=./DbxSYBASEErr.msg');
{$ELSE}
    Connection.LibraryName := '';
    Connection.VendorLib := '';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if StartsWith(GetProtocol, 'postgresql') then
  begin
    Connection.DriverName := 'PostgreSQL';
    Connection.GetDriverFunc := 'getSQLDriverMYSQL';
    Connection.Params.Add('Database=' + GetDatabase);
{$IFDEF LINUX}
    Connection.LibraryName := 'libsqlpg.so';
    Connection.VendorLib := 'libpq.so;
    Connection.Params.Add('ErrorResourceFile=./DbxPGSQLErr.msg');
{$ELSE}
    Connection.LibraryName := '';
    Connection.VendorLib := '';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end;
end;

{**
  Destroy objects and free allocated memory for variables
}
procedure TZDBXPerformanceTestCase.TearDown;
begin
  if Query <> nil then
  begin
    Query.Free;
    Query := nil;
  end;

  if Connection <> nil then
  begin
    Connection.Free;
    Connection := nil;
  end;

  if DataSetProvider <> nil then
  begin
    DataSetProvider.Free;
    DataSetProvider := nil;
  end;

  if ClientDataSet <> nil then
  begin
    ClientDataSet.Free;
    ClientDataSet := nil;
  end;
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZDBXPerformanceTestCase.DefaultSetUpTest;
begin
  Connection.Open;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZDBXPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  Connection.Close;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZDBXPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZDBXPerformanceTestCase.RunTestConnect;
begin
  Connection.Open;
end;

{**
  Performs an insert test.
}
procedure TZDBXPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs an insert test.
}
procedure TZDBXPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with ClientDataSet do
    begin
      Append;
      Fields[0].AsInteger := I;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
      ApplyUpdates(0);
    end;
  end;
end;

{**
  Performs an open test.
}
procedure TZDBXPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZDBXPerformanceTestCase.SetUpTestFetch;
begin
  inherited SetUpTestFetch;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZDBXPerformanceTestCase.RunTestFetch;
var
  I: integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with Query do
    begin
      Fields[0].AsInteger;
      Fields[1].AsFloat;
      Fields[2].AsString;
      Next;
    end;
  end;
end;

{**
  Performs an update test.
}
procedure TZDBXPerformanceTestCase.SetUpTestUpdate;
begin
  inherited SetUpTestUpdate;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs an update test.
}
procedure TZDBXPerformanceTestCase.RunTestUpdate;
var
  I: integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with ClientDataSet do
    begin
      Edit;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
      ApplyUpdates(0);
      Next;
    end;
  end;
end;

{**
  Performs an delete test.
}
procedure TZDBXPerformanceTestCase.SetUpTestDelete;
begin
  inherited SetUpTestDelete;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs a delete test.
}
procedure TZDBXPerformanceTestCase.RunTestDelete;
begin
  while not ClientDataSet.EOF do
  begin
    ClientDataSet.Delete;
    ClientDataSet.ApplyUpdates(0);
  end;
end;

{$ENDIF}


{ TZDBXCPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZDBXCPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dbxc';
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZDBXCPerformanceTestCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5'
    + ',sybase,interbase,interbase-5,interbase-6'
    + ',firebird-1.0,firebird-1.5'
    + ',postgresql,postgresql-7,postgresql-8';
{$IFNDEF LINUX}
  Result := Result + ',mssql';
{$ENDIF}
end;

{$IFNDEF EXCLUDE_DBX_TEST}

{**
   Create objects and allocate memory for variables
}
procedure TZDBXCPerformanceTestCase.SetUp;
begin
  Connection := TSQLConnection.Create(nil);
  Query := TSQLQuery.Create(nil);
  ClientDataSet := TClientdataset.Create(nil);
  DataSetProvider := TDataSetProvider.Create(nil);
  Connection.ConnectionName := 'SQLConnection';
  Connection.Params.Add('Hostname=' + GetHostName);
  Connection.Params.Add('User_Name=' + GetUserName);
  Connection.Params.Add('Password=' + GetPassword);
  Connection.Params.Add('BlobSize=-1');
  Connection.Params.Add('LocaleCode=0000');
  Connection.LoginPrompt := false;
  Query.SQLConnection := Connection;
  DataSetProvider.DataSet := Query;
  ClientDataSet.SetProvider(DataSetProvider);
  DataSetProvider.Options := [poAllowMultiRecordUpdates];
  DataSetProvider.UpdateMode := upWhereKeyOnly;
  
  if StartsWith(GetProtocol, 'mysql') then
  begin
    Connection.DriverName := 'MySQL';
    Connection.GetDriverFunc := 'getSQLDriverMYSQL';
    Connection.Params.Add('Database=' + GetDatabase);
{$IFDEF LINUX}
    Connection.LibraryName := 'libsqlmy.so';
    Connection.VendorLib := 'libmysqlclient.so';
    Connection.Params.Add('ErrorResourceFile=./DbxMySqlErr.msg');
{$ELSE}
    Connection.LibraryName := 'dbexpmysql.dll';
    Connection.VendorLib := 'libmysql.dll';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if StartsWith(GetProtocol, 'interbase') then
  begin
    Connection.DriverName := 'IBConnection';
    Connection.GetDriverFunc := 'getSQLDriverINTERBASE';
    Connection.Params.Add('Database='+ GetHostname + ':' + GetDatabase);
    Connection.Params.Add('SQLDialect=1');
    Connection.Params.Add('CommitRetain=False');
    Connection.Params.Add('WaitOnLocks=True');
    Connection.Params.Add('ServerCharSet=');
    //Connection.Params.Add('RoleName=');
{$IFDEF LINUX}
    Connection.LibraryName := 'libsqlib.so';
    Connection.VendorLib := 'libgds.so';
    Connection.Params.Add('ErrorResourceFile=./DbxIbErr.msg');
{$ELSE}
    Connection.LibraryName := 'dbexpint.dll';
    Connection.VendorLib := 'gds32.dll';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if GetProtocol = 'mssql' then
  begin
    Connection.DriverName := 'MSSQL';
    Connection.GetDriverFunc := 'getSQLDriverMSSQL';
    Connection.Params.Add('Database=' + GetDatabase);
{$IFDEF LINUX}
    Connection.LibraryName := 'dbexpmss.dll';
    Connection.VendorLib := 'libgds.so';
    Connection.Params.Add('ErrorResourceFile=./DbxErr.msg');
{$ELSE}
    Connection.LibraryName := 'dbexpmss.dll';
    Connection.VendorLib := 'oledb';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if StartsWith(GetProtocol, 'firebird') then
  begin
    Connection.DriverName := 'IBConnection';
    Connection.GetDriverFunc := 'getSQLDriverINTERBASE';
    Connection.Params.Add('SQLDialect=1');
    Connection.Params.Add('CommitRetain=False');
    Connection.Params.Add('WaitOnLocks=True');
    Connection.Params.Add('ServerCharSet=');
    Connection.Params.Add('Database=' + GetHostname + ':' + GetDatabase);
    //Connection.Params.Add('RoleName=');
{$IFDEF LINUX}
    Connection.LibraryName := 'libsqlib.so';
    Connection.VendorLib := 'libgds.so';
    Connection.Params.Add('ErrorResourceFile=./DbxIbErr.msg');
{$ELSE}
    Connection.LibraryName := 'dbexpint.dll';
    Connection.VendorLib := 'gds32.dll';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if GetProtocol = 'sybase' then
  begin
    Connection.DriverName := 'Sybase';
    Connection.GetDriverFunc := 'getSQLDriverSYBASE';
    Connection.Params.Add('Database=' + GetDatabase);
{$IFDEF LINUX}
    Connection.LibraryName := '';
    Connection.VendorLib := '';
    Connection.Params.Add('ErrorResourceFile=./DbxSYBASEErr.msg');
{$ELSE}
    Connection.LibraryName := '';
    Connection.VendorLib := '';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end else
  if StartsWith(GetProtocol, 'postgresql') then
  begin
    Connection.DriverName := 'PostgreSQL';
    Connection.GetDriverFunc := 'getSQLDriverMYSQL';
    Connection.Params.Add('Database=' + GetDatabase);
{$IFDEF LINUX}
    Connection.LibraryName := 'libsqlpg.so';
    Connection.VendorLib := 'libpq.so;
    Connection.Params.Add('ErrorResourceFile=./DbxPGSQLErr.msg');
{$ELSE}
    Connection.LibraryName := '';
    Connection.VendorLib := '';
    Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}
  end;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZDBXCPerformanceTestCase.TearDown;
begin
  if Query <> nil then
  begin
    Query.Free;
    Query := nil;
  end;

  if Connection <> nil then
  begin
    Connection.Free;
    Connection := nil;
  end;

  if DataSetProvider <> nil then
  begin
    DataSetProvider.Free;
    DataSetProvider := nil;
  end;

  if ClientDataSet <> nil then
  begin
    ClientDataSet.Free;
    ClientDataSet := nil;
  end;
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZDBXCPerformanceTestCase.DefaultSetUpTest;
begin
  Connection.Open;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZDBXCPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  Connection.Close;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZDBXCPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZDBXCPerformanceTestCase.RunTestConnect;
begin
  Connection.Open;
end;

{**
  Performs an insert test.
}
procedure TZDBXCPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs an insert test.
}
procedure TZDBXCPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with ClientDataSet do
    begin
      Append;
      Fields[0].AsInteger := I;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
    end;
  end;
  ClientDataSet.ApplyUpdates(0);
end;

{**
  Performs an open test.
}
procedure TZDBXCPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZDBXCPerformanceTestCase.SetUpTestFetch;
begin
  inherited SetUpTestFetch;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs a fetch data
}
procedure TZDBXCPerformanceTestCase.RunTestFetch;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with Query do
    begin
      Fields[0].AsInteger;
      Fields[1].AsFloat;
      Fields[2].AsString;
      Next;
    end;
  end;
end;

{**
  Performs an update test.
}
procedure TZDBXCPerformanceTestCase.SetUpTestUpdate;
begin
  inherited SetUpTestUpdate;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs an update test.
}
procedure TZDBXCPerformanceTestCase.RunTestUpdate;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with ClientDataSet do
    begin
      Edit;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
      Next;
    end;
  end;
  ClientDataSet.ApplyUpdates(0);
end;

{**
  Performs an delete test.
}
procedure TZDBXCPerformanceTestCase.SetUpTestDelete;
begin
  inherited SetUpTestDelete;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs a delete test.
}
procedure TZDBXCPerformanceTestCase.RunTestDelete;
begin
  while not ClientDataSet.EOF do
  begin
    ClientDataSet.Delete;
  end;
  ClientDataSet.ApplyUpdates(0);
end;

{$ENDIF}


{ TZDBXODBCPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZDBXODBCPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dbxodbc';
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZDBXODBCPerformanceTestCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5'
    + ',sybase,interbase,interbase-5,interbase-6'
    + ',firebird-1.0,firebird-1.5'
    + ',postgresql,postgresql-6.5,postgresql-7.2,postgresql-8.1';
{$IFNDEF LINUX}
  Result := Result + ',mssql';
{$ENDIF}
end;

{$IFNDEF EXCLUDE_DBX_TEST}

{**
   Create objects and allocate memory for variables
}
procedure TZDBXODBCPerformanceTestCase.SetUp;
var
  Protocol: string;
begin
  Protocol := GetProtocol;
  if Pos('-', Protocol) > 0 then
    Protocol := Copy(Protocol, 1, Pos('-', Protocol) - 1);

  Connection := TSQLConnection.Create(nil);
  Query := TSQLQuery.Create(nil);
  ClientDataSet := TClientdataset.Create(nil);
  DataSetProvider := TDataSetProvider.Create(nil);
  Connection.ConnectionName := 'SQLConnection';
  Connection.Params.Add('Hostname=' + GetHostName);
  Connection.Params.Add('User_Name=' + GetUserName);
  Connection.Params.Add('Password=' + GetPassword);
  Connection.Params.Add('BlobSize=-1');
  Connection.Params.Add('LocaleCode=0000');
  Connection.LoginPrompt := false;
  Query.SQLConnection := Connection;
  DataSetProvider.DataSet := Query;
  ClientDataSet.SetProvider(DataSetProvider);
  DataSetProvider.Options := [poAllowMultiRecordUpdates];
  DataSetProvider.UpdateMode := upWhereKeyOnly;
  
  Connection.DriverName := 'OpenOdbc';
  Connection.GetDriverFunc := 'getSQLDriverODBC';
  Connection.Params.Add('Database=' + GetDatabase + '_' + Protocol);
{$IFDEF LINUX}
  Connection.LibraryName := 'dbxoodbc.so';
  Connection.VendorLib := 'odbc32.so';
  Connection.Params.Add('ErrorResourceFile=./DbxODBCErr.msg');
{$ELSE}
  Connection.LibraryName := 'dbxoodbc.dll';
  Connection.VendorLib := 'odbc32.dll';
  Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}

end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZDBXODBCPerformanceTestCase.TearDown;
begin
  if Query <> nil then
  begin
    Query.Free;
    Query := nil;
  end;

  if Connection <> nil then
  begin
    Connection.Free;
    Connection := nil;
  end;

  if DataSetProvider <> nil then
  begin
    DataSetProvider.Free;
    DataSetProvider := nil;
  end;

  if ClientDataSet <> nil then
  begin
    ClientDataSet.Free;
    ClientDataSet := nil;
  end;
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZDBXODBCPerformanceTestCase.DefaultSetUpTest;
begin
  Connection.Open;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZDBXODBCPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  Connection.Close;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZDBXODBCPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZDBXODBCPerformanceTestCase.RunTestConnect;
begin
  Connection.Open;
end;

{**
  Performs an insert test.
}
procedure TZDBXODBCPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs an insert test.
}
procedure TZDBXODBCPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with ClientDataSet do
    begin
      Append;
      Fields[0].AsInteger := I;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
      ClientDataSet.ApplyUpdates(0);
    end;
  end;
end;

{**
  Performs an open test.
}
procedure TZDBXODBCPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZDBXODBCPerformanceTestCase.SetUpTestFetch;
begin
  inherited SetUpTestFetch;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs a fetch data
}
procedure TZDBXODBCPerformanceTestCase.RunTestFetch;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with Query do
    begin
      Fields[0].AsInteger;
      Fields[1].AsFloat;
      Fields[2].AsString;
      Next;
    end;
  end;
end;

{**
  Performs an update test.
}
procedure TZDBXODBCPerformanceTestCase.SetUpTestUpdate;
begin
  inherited SetUpTestUpdate;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs an update test.
}
procedure TZDBXODBCPerformanceTestCase.RunTestUpdate;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with ClientDataSet do
    begin
      Edit;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
      ClientDataSet.ApplyUpdates(0);
      Next;
    end;
  end;
end;

{**
  Performs an delete test.
}
procedure TZDBXODBCPerformanceTestCase.SetUpTestDelete;
begin
  inherited SetUpTestDelete;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs a delete test.
}
procedure TZDBXODBCPerformanceTestCase.RunTestDelete;
begin
  while not ClientDataSet.EOF do
  begin
    ClientDataSet.Delete;
    ClientDataSet.ApplyUpdates(0);
  end;
end;

{$ENDIF}


{ TZDBXODBCCPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZDBXODBCCPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'dbxodbcc';
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZDBXODBCCPerformanceTestCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5'
    + ',sybase,interbase,interbase-5,interbase-6'
    + ',firebird-1.0,firebird-1.5'
    + ',postgresql,postgresql-6.5,postgresql-7.2,postgresql-8.1';
{$IFNDEF LINUX}
  Result := Result + ',mssql';
{$ENDIF}
end;

{$IFNDEF EXCLUDE_DBX_TEST}

{**
   Create objects and allocate memory for variables
}
procedure TZDBXODBCCPerformanceTestCase.SetUp;
var
  Protocol: string;
begin
  Protocol := GetProtocol;
  if Pos('-', Protocol) > 0 then
    Protocol := Copy(Protocol, 1, Pos('-', Protocol) - 1);

  Connection := TSQLConnection.Create(nil);
  Query := TSQLQuery.Create(nil);
  ClientDataSet := TClientdataset.Create(nil);
  DataSetProvider := TDataSetProvider.Create(nil);
  Connection.ConnectionName := 'SQLConnection';
  Connection.Params.Add('Hostname=' + GetHostName);
  Connection.Params.Add('User_Name=' + GetUserName);
  Connection.Params.Add('Password=' + GetPassword);
  Connection.Params.Add('BlobSize=-1');
  Connection.Params.Add('LocaleCode=0000');
  Connection.LoginPrompt := false;
  Query.SQLConnection := Connection;
  DataSetProvider.DataSet := Query;
  ClientDataSet.SetProvider(DataSetProvider);
  DataSetProvider.Options := [poAllowMultiRecordUpdates];
  DataSetProvider.UpdateMode := upWhereKeyOnly;

  Connection.DriverName := 'OpenOdbc';
  Connection.GetDriverFunc := 'getSQLDriverODBC';
  Connection.Params.Add('Database=' + GetDatabase + '_' + Protocol);
{$IFDEF LINUX}
  Connection.LibraryName := 'dbxoodbc.so';
  Connection.VendorLib := 'odbc32.so';
  Connection.Params.Add('ErrorResourceFile=./DbxODBCErr.msg');
{$ELSE}
  Connection.LibraryName := 'dbxoodbc.dll';
  Connection.VendorLib := 'odbc32.dll';
  Connection.Params.Add('ErrorResourceFile=');
{$ENDIF}

end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZDBXODBCCPerformanceTestCase.TearDown;
begin
  if Query <> nil then
  begin
    Query.Free;
    Query := nil;
  end;

  if Connection <> nil then
  begin
    Connection.Free;
    Connection := nil;
  end;

  if DataSetProvider <> nil then
  begin
    DataSetProvider.Free;
    DataSetProvider := nil;
  end;

  if ClientDataSet <> nil then
  begin
    ClientDataSet.Free;
    ClientDataSet := nil;
  end;
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZDBXODBCCPerformanceTestCase.DefaultSetUpTest;
begin
  Connection.Open;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZDBXODBCCPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  Connection.Close;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZDBXODBCCPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZDBXODBCCPerformanceTestCase.RunTestConnect;
begin
  Connection.Open;
end;

{**
  Performs an insert test.
}
procedure TZDBXODBCCPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs an insert test.
}
procedure TZDBXODBCCPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with ClientDataSet do
    begin
      Append;
      Fields[0].AsInteger := I;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
    end;
  end;
  ClientDataSet.ApplyUpdates(0);
end;

{**
  Performs an open test.
}
procedure TZDBXODBCCPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZDBXODBCCPerformanceTestCase.SetUpTestFetch;
begin
  inherited SetUpTestFetch;
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
  Performs a fetch data
}
procedure TZDBXODBCCPerformanceTestCase.RunTestFetch;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with Query do
    begin
      Fields[0].AsInteger;
      Fields[1].AsFloat;
      Fields[2].AsString;
      Next;
    end;
  end;
end;

{**
  Performs an update test.
}
procedure TZDBXODBCCPerformanceTestCase.SetUpTestUpdate;
begin
  inherited SetUpTestUpdate;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs an update test.
}
procedure TZDBXODBCCPerformanceTestCase.RunTestUpdate;
var
  I: Integer;
begin
  for I := 1 to GetRecordCount do
  begin
    with ClientDataSet do
    begin
      Edit;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
      Next;
    end;
  end;
  ClientDataSet.ApplyUpdates(0);
end;

{**
  Performs an delete test.
}
procedure TZDBXODBCCPerformanceTestCase.SetUpTestDelete;
begin
  inherited SetUpTestDelete;
  Query.SQL.Text := 'SELECT * FROM high_load';
  ClientDataSet.Open;
end;

{**
  Performs a delete test.
}
procedure TZDBXODBCCPerformanceTestCase.RunTestDelete;
begin
  while not ClientDataSet.EOF do
  begin
    ClientDataSet.Delete;
  end;
  ClientDataSet.ApplyUpdates(0);
end;

{$ENDIF}

initialization
  TestFramework.RegisterTest(TZDBXPerformanceTestCase.Suite);
  TestFramework.RegisterTest(TZDBXCPerformanceTestCase.Suite);
  TestFramework.RegisterTest(TZDBXODBCPerformanceTestCase.Suite);
  TestFramework.RegisterTest(TZDBXODBCCPerformanceTestCase.Suite);
end.

