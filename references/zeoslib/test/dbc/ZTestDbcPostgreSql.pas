{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{ Test Case for PostgreSql Database Connectivity Classes  }
{                                                         }
{       Originally written by Sergey Seroukhov            }
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

unit ZTestDbcPostgreSql;

interface
{$I ZDbc.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZDbcPostgreSql, ZTestDefinitions,
  ZCompatibility;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcPostgreSQLCase = class(TZDbcSpecificSQLTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;
    function GetConnectionUrl: string;

    property Connection: IZConnection read FConnection write FConnection;

  published
    procedure TestConnection;
    procedure TestStatement;
    procedure TestRegularResultSet;
    procedure TestBlobs;
    procedure TestCaseSensitive;
    procedure TestDefaultValues;
    procedure TestEnumValues;
  end;

implementation

uses SysUtils, ZSysUtils, ZTestConsts;

{ TZTestDbcPostgreSQLCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcPostgreSQLCase.GetSupportedProtocols: string;
begin
  Result := 'postgresql,postgresql-7,postgresql-8';
end;

{**
  Gets a connection URL string.
  @return a built connection URL string. 
}
function TZTestDbcPostgreSQLCase.GetConnectionUrl: string;
begin
  if Port <> 0 then
    Result := Format('zdbc:%s://%s:%d/%s', [Protocol, HostName, Port, Database])
  else Result := Format('zdbc:%s://%s/%s', [Protocol, HostName, Database]);
end;

{**
   Create objects and allocate memory for variables
}
procedure TZTestDbcPostgreSQLCase.SetUp;
begin
  Connection := CreateDbcConnection;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestDbcPostgreSQLCase.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

procedure TZTestDbcPostgreSQLCase.TestConnection;
begin
  CheckEquals(True, Connection.IsReadOnly);
  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

  CheckEquals('inet', (Connection as IZPostgreSQLConnection).
    GetTypeNameByOid(869));

  { Checks without transactions. }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiSerializable);
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

procedure TZTestDbcPostgreSQLCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
  Statement.ExecuteUpdate('SELECT * FROM equipment');

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));
end;

procedure TZTestDbcPostgreSQLCase.TestRegularResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM blob_values');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

procedure TZTestDbcPostgreSQLCase.TestBlobs;
var
  Connection: IZConnection;
  PreparedStatement: IZPreparedStatement;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  TextStream: TStream;
  ImageStream: TMemoryStream;
  TempStream: TStream;
begin
  Connection := DriverManager.GetConnectionWithLogin(
    GetConnectionUrl + '?oidasblob=true', UserName, Password);
  Connection.SetTransactionIsolation(tiReadCommitted);
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Statement.ExecuteUpdate('DELETE FROM blob_values WHERE b_id='
    + IntToStr(TEST_ROW_ID));

  TextStream := TStringStream.Create('ABCDEFG');
  ImageStream := TMemoryStream.Create;
  ImageStream.LoadFromFile('../../../database/images/zapotec.bmp');

  PreparedStatement := Connection.PrepareStatement(
    'INSERT INTO blob_values (b_id,b_text,b_image) VALUES(?,?,?)');
  PreparedStatement.SetInt(1, TEST_ROW_ID);
  PreparedStatement.SetAsciiStream(2, TextStream);
  PreparedStatement.SetBinaryStream(3, ImageStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM blob_values'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('b_id'));
  TempStream := ResultSet.GetAsciiStreamByName('b_text');
  CheckEquals(TextStream, TempStream);
  TempStream.Free;
  TempStream := ResultSet.GetBinaryStreamByName('b_image');
  CheckEquals(ImageStream, TempStream);
  TempStream.Free;
  ResultSet.Close;

  TextStream.Free;
  ImageStream.Free;

  Statement.Close;
  Connection.Close;
end;

procedure TZTestDbcPostgreSQLCase.TestCaseSensitive;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM "Case_Sensitive"');
  CheckNotNull(ResultSet);
  Metadata := ResultSet.GetMetadata;
  CheckNotNull(Metadata);

  CheckEquals('cs_id', Metadata.GetColumnName(1));
  CheckEquals(False, Metadata.IsCaseSensitive(1));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(1));

  CheckEquals('Cs_Data1', Metadata.GetColumnName(2));
  CheckEquals(True, Metadata.IsCaseSensitive(2));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(2));

  CheckEquals('cs_data1', Metadata.GetColumnName(3));
  CheckEquals(False, Metadata.IsCaseSensitive(3));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(3));

  CheckEquals('cs data1', Metadata.GetColumnName(4));
  CheckEquals(True, Metadata.IsCaseSensitive(4));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(4));

  ResultSet.Close;
  Statement.Close;
  Connection.Close;
end;

{**
  Runs a test for PostgreSQL default values.
}
procedure TZTestDbcPostgreSQLCase.TestDefaultValues;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from default_values');

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3,d_fld4,d_fld5,d_fld6 FROM default_values');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.InsertRow;

  Check(ResultSet.GetInt(1) <> 0);
  CheckEquals(123456, ResultSet.GetInt(2));
  CheckEquals(123.456, ResultSet.GetFloat(3), 0.001);
  CheckEquals('xyz', ResultSet.GetString(4));
  CheckEquals(EncodeDate(2003, 12, 11), ResultSet.GetDate(5), 0);
  CheckEquals(EncodeTime(23, 12, 11, 0), ResultSet.GetTime(6), 3);
  CheckEquals(EncodeDate(2003, 12, 11) +
    EncodeTime(23, 12, 11, 0), ResultSet.GetTimestamp(7), 3);

  ResultSet.DeleteRow;

  ResultSet.Close;
  Statement.Close;
end;

procedure TZTestDbcPostgreSQLCase.TestEnumValues; 
var 
  Statement: IZStatement; 
  ResultSet: IZResultSet; 
begin 
    Statement := Connection.CreateStatement; 
    CheckNotNull(Statement); 
    Statement.SetResultSetType(rtScrollInsensitive); 
    Statement.SetResultSetConcurrency(rcUpdatable); 

    // Select case 
    ResultSet := Statement.ExecuteQuery('SELECT * FROM extension where ext_id = 1'); 
    CheckNotNull(ResultSet); 
    ResultSet.First; 
    Check(ResultSet.GetInt(1) = 1); 
    CheckEquals('Car', ResultSet.GetString(2)); 
    ResultSet.Close; 
    Statement.Close; 

    // Update case 
    ResultSet := Statement.ExecuteQuery('UPDATE extension set ext_enum = ''House'' where ext_id = 1'); 
    ResultSet.Close; 

    ResultSet := Statement.ExecuteQuery('SELECT * FROM extension where ext_id = 1'); 
    CheckNotNull(ResultSet); 
    ResultSet.First; 
    Check(ResultSet.GetInt(1) = 1); 
    CheckEquals('House', ResultSet.GetString(2)); 
    ResultSet.Close; 
    Statement.Close; 

    // Insert case 
    ResultSet := Statement.ExecuteQuery('DELETE FROM extension where ext_id = 1'); 
    ResultSet.Close; 

    ResultSet := Statement.ExecuteQuery('INSERT INTO extension VALUES(1,''Car'')'); 
    ResultSet.Close; 

    ResultSet := Statement.ExecuteQuery('SELECT * FROM extension where ext_id = 1'); 
    CheckNotNull(ResultSet); 
    ResultSet.First; 
    Check(ResultSet.GetInt(1) = 1); 
    CheckEquals('Car', ResultSet.GetString(2)); 
    ResultSet.Close; 
    Statement.Close; 

end; 

initialization
  RegisterTest('dbc',TZTestDbcPostgreSQLCase.Suite);
end.
