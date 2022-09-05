{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{ Test Case for Interbase Database Connectivity Classes   }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZTestDbcASA;

interface
{$I ZDbc.inc}
uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZDbcASA, ZTestDefinitions,
  ZCompatibility;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcASACase = class(TZDbcSpecificSQLTestCase)
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
//    procedure TestCaseSensitive;
    procedure TestDefaultValues;
    procedure TestDomainValues;
    procedure TestStoredprocedures;
  end;

implementation

uses SysUtils, ZSysUtils, ZTestConsts, ZTestCase;

{ TZTestDbcASACase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcASACase.GetSupportedProtocols: string;
begin
  Result := 'ASA7,ASA8,ASA9';
end;

{**
  Gets a connection URL string.
  @return a built connection URL string. 
}
function TZTestDbcASACase.GetConnectionUrl: string;
begin
  if Port <> 0 then
    Result := Format('zdbc:%s://%s:%d/%s', [Protocol, HostName, Port, Database])
  else Result := Format('zdbc:%s://%s/%s', [Protocol, HostName, Database]);
end;

{**
   Create objects and allocate memory for variables
}
procedure TZTestDbcASACase.SetUp;
begin
  Connection := CreateDbcConnection;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestDbcASACase.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

procedure TZTestDbcASACase.TestConnection;
begin
  CheckEquals(True, Connection.IsReadOnly);
  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

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

procedure TZTestDbcASACase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
//  Statement.ExecuteUpdate('SELECT * FROM equipment'); 

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));
end;

procedure TZTestDbcASACase.TestRegularResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM DEPARTMENT');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
end;

procedure TZTestDbcASACase.TestBlobs;
var
  Connection: IZConnection;
  PreparedStatement: IZPreparedStatement;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  TextStream: TStream;
  ImageStream: TMemoryStream;
  TempStream: TStream;
begin
  Connection := CreateDbcConnection;
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Statement.ExecuteUpdate('DELETE FROM BLOB_VALUES WHERE B_ID='
    + IntToStr(TEST_ROW_ID));

  TextStream := TStringStream.Create('ABCDEFG');
  ImageStream := TMemoryStream.Create;
  ImageStream.LoadFromFile('../../../database/images/zapotec.bmp');

  PreparedStatement := Connection.PrepareStatement(
    'INSERT INTO BLOB_VALUES (B_ID, B_TEXT, B_IMAGE) VALUES(?,?,?)');
  PreparedStatement.SetInt(1, TEST_ROW_ID);
  PreparedStatement.SetAsciiStream(2, TextStream);
  PreparedStatement.SetBinaryStream(3, ImageStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
  TempStream := ResultSet.GetAsciiStreamByName('B_TEXT');
  CheckEquals(TextStream, TempStream);
  TempStream.Free;
  TempStream := ResultSet.GetBinaryStreamByName('B_IMAGE');
  CheckEquals(ImageStream, TempStream);
  TempStream.Free;
  ResultSet.Close;

  TextStream.Free;
  ImageStream.Free;

  Statement.Close;
end;
{ Sybase ASA isn't case sensitive per default
procedure TZTestDbcASACase.TestCaseSensitive;
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

  CheckEquals('CS_ID', Metadata.GetColumnName(1));
  CheckEquals(False, Metadata.IsCaseSensitive(1));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(1));

  CheckEquals('Cs_Data1', Metadata.GetColumnName(2));
  CheckEquals(True, Metadata.IsCaseSensitive(2));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(2));

  CheckEquals('cs_data1', Metadata.GetColumnName(3));
  CheckEquals(True, Metadata.IsCaseSensitive(3));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(3));

  CheckEquals('cs data1', Metadata.GetColumnName(4));
  CheckEquals(True, Metadata.IsCaseSensitive(4));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(4));

  ResultSet.Close;
  Statement.Close;
end;
}
{**
  Runs a test for Interbase default values.
}
procedure TZTestDbcASACase.TestDefaultValues;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from DEFAULT_VALUES');

  ResultSet := Statement.ExecuteQuery('SELECT D_ID,D_FLD1,D_FLD2,D_FLD3,D_FLD4,D_FLD5,D_FLD6 FROM DEFAULT_VALUES');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(1, 1);
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

{**
  Runs a test for Interbase domain fields.
}
procedure TZTestDbcASACase.TestDomainValues;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from DOMAIN_VALUES');

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3 FROM DOMAIN_VALUES');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(1, 1);
  ResultSet.InsertRow;

  Check(ResultSet.GetInt(1) <> 0);
  CheckEquals(123456, ResultSet.GetInt(2));
  CheckEquals(123.456, ResultSet.GetFloat(3), 0.001);
  CheckEquals('xyz', ResultSet.GetString(4));

  ResultSet.Close;
  ResultSet := nil;

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3 FROM DOMAIN_VALUES');
  CheckNotNull(ResultSet);

  ResultSet.Next;

  Check(ResultSet.GetInt(1) <> 0);
  CheckEquals(123456, ResultSet.GetInt(2));
  CheckEquals(123.456, ResultSet.GetFloat(3), 0.001);
  CheckEquals('xyz', ResultSet.GetString(4));

  ResultSet.Close;
  Statement.Close;
end;

{**
  Runs a test for Interbase stored procedures.
}
procedure TZTestDbcASACase.TestStoredprocedures;
var
  ResultSet: IZResultSet;
  CallableStatement: IZCallableStatement;
begin
  CallableStatement := Connection.PrepareCallWithParams(
    'PROCEDURE1', nil);
  with CallableStatement do
  begin
    RegisterOutParameter(1, Ord(stInteger));
    SetInt(1, 12345);
    ExecutePrepared;
    CheckEquals(12346, GetInt(1));
  end;
  CallableStatement.Close;

  CallableStatement := Connection.PrepareCallWithParams(
    'PROCEDURE2', nil);
  ResultSet := CallableStatement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    CheckEquals(True, Next);
    CheckEquals('Computer', GetString(1));
    CheckEquals(True, Next);
    CheckEquals('Laboratoy', GetString(1));
    CheckEquals(True, Next);
    CheckEquals('Radiostation', GetString(1));
    CheckEquals(True, Next);
    CheckEquals('Volvo', GetString(1));
    Close;
  end;
  CallableStatement.Close;
end;

initialization
  RegisterTest('dbc',TZTestDbcASACase.Suite);
end.
