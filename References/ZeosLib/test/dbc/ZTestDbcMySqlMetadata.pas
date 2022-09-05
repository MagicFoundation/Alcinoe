{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Tests for MySQL Database Metadata Class       }
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

unit ZTestDbcMySqlMetadata;

interface
{$I ZDbc.inc}

uses SysUtils, ZDbcIntfs, ZClasses, ZCompatibility,
  ZTestDefinitions, ZDbcResultSet, ZDbcResultSetMetadata, ZDbcMySql,{$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF};

type

 {** Implements a test case for TZMySqlMetadata. }
  TZTestMySqlMetadataCase = class(TZDbcSpecificSQLTestCase)
  private
    FConnection: IZConnection;
    FMetadata: IZDatabaseMetadata;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: IZConnection read FConnection write FConnection;
    property Metadata: IZDatabaseMetadata read FMetadata write FMetadata;
  published
    procedure TestGetProcedures;
    procedure TestGetProcedureColumns;
    procedure TestGetTables;
    procedure TestGetSchemas;
    procedure TestGetCatalogs;
    procedure TestGetTableTypes;
    procedure TestGetColumns;
    procedure TestGetColumnPrivileges;
    procedure TestGetTablePrivileges;
    procedure TestGetBestRowIdentifier;
  end;

implementation

{ TZTestMySqlMetadataCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestMySqlMetadataCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysqld-4.1,mysql-5,mysqld-5';
end;

{**
   Create objects and allocate memory for variables
}
procedure TZTestMySqlMetadataCase.SetUp;
begin
  Connection := CreateDbcConnection;
  Metadata := Connection.GetMetadata;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestMySqlMetadataCase.TearDown;
begin
  Connection.Close;
  Connection := nil;
  Metadata := nil;
end;

{**
   Test method GetBestRowIdentifier
   <p><b>Note:</b><br>
   For adventure of the test it is necessary to execute sql
   <i>grant select(p_resume, p_redundant) on zeoslib.people to root@"%"</i></p>
}
procedure TZTestMySqlMetadataCase.TestGetBestRowIdentifier;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetBestRowIdentifier('', '', 'people', 0, false);
  CheckEquals(1, ResultSet.FindColumn('SCOPE'));
  CheckEquals(2, ResultSet.FindColumn('COLUMN_NAME'));
  CheckEquals(3, ResultSet.FindColumn('DATA_TYPE'));
  CheckEquals(4, ResultSet.FindColumn('TYPE_NAME'));
  CheckEquals(5, ResultSet.FindColumn('COLUMN_SIZE'));
  CheckEquals(6, ResultSet.FindColumn('BUFFER_LENGTH'));
  CheckEquals(7, ResultSet.FindColumn('DECIMAL_DIGITS'));
  CheckEquals(8, ResultSet.FindColumn('PSEUDO_COLUMN'));

  ResultSet.Next;
  CheckEquals('2', ResultSet.GetStringByName('SCOPE'));
  CheckEquals('p_id', ResultSet.GetStringByName('COLUMN_NAME'));
  CheckEquals(ord(stShort), ResultSet.GetIntByName('DATA_TYPE'));
  CheckEquals('smallint', ResultSet.GetStringByName('TYPE_NAME'));
  CheckEquals('6', ResultSet.GetStringByName('COLUMN_SIZE'));
  CheckEquals('65535', ResultSet.GetStringByName('BUFFER_LENGTH'));
  CheckEquals('0', ResultSet.GetStringByName('DECIMAL_DIGITS'));
  CheckEquals('1', ResultSet.GetStringByName('PSEUDO_COLUMN'));
  ResultSet.Close;
  ResultSet := nil;
end;

procedure TZTestMySqlMetadataCase.TestGetCatalogs;
var
  ResultSet: IZResultSet;
  DBFound: boolean;
  TableName: string;
begin
  DBFound := False;
  ResultSet := Metadata.GetCatalogs;
  CheckEquals(1, ResultSet.FindColumn('TABLE_CAT'));

  while ResultSet.Next do
  begin
    TableName := ResultSet.GetString(1);
    if TableName = 'zeoslib' then
      DBFound := True;
  end;
  Check(DBFound);
  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test method GetBestRowIdentifier
   <p><b>Note:</b><br>
   For adventure of the test it is necessary to execute sql
   <i>grant select privileges on zeoslib.people to root@localhist;</i></p>
}
procedure TZTestMySqlMetadataCase.TestGetColumnPrivileges;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetColumnPrivileges('', '', 'people', 'p_r%');
  CheckEquals(1, ResultSet.FindColumn('TABLE_CAT'));
  CheckEquals(2, ResultSet.FindColumn('TABLE_SCHEM'));
  CheckEquals(3, ResultSet.FindColumn('TABLE_NAME'));
  CheckEquals(4, ResultSet.FindColumn('COLUMN_NAME'));
  CheckEquals(5, ResultSet.FindColumn('GRANTOR'));
  CheckEquals(6, ResultSet.FindColumn('GRANTEE'));
  CheckEquals(7, ResultSet.FindColumn('PRIVILEGE'));
  CheckEquals(8, ResultSet.FindColumn('IS_GRANTABLE'));

  CheckEquals(True, ResultSet.Next);
  CheckEquals('zeoslib', ResultSet.GetStringByName('TABLE_CAT'));
  CheckEquals('', ResultSet.GetStringByName('TABLE_SCHEM'));
  CheckEquals('people', ResultSet.GetStringByName('TABLE_NAME'));
  CheckEquals('p_resume', ResultSet.GetStringByName('COLUMN_NAME'));
  CheckEquals('root', ResultSet.GetStringByName('GRANTOR'));
  CheckEquals('people@%', ResultSet.GetStringByName('GRANTEE'));
  CheckEquals('Update', ResultSet.GetStringByName('PRIVILEGE'));
  CheckEquals('', ResultSet.GetStringByName('IS_GRANTABLE'));

  CheckEquals(True, ResultSet.Next);
  CheckEquals('zeoslib', ResultSet.GetStringByName('TABLE_CAT'));
  CheckEquals('', ResultSet.GetStringByName('TABLE_SCHEM'));
  CheckEquals('people', ResultSet.GetStringByName('TABLE_NAME'));
  CheckEquals('p_redundant', ResultSet.GetStringByName('COLUMN_NAME'));
  CheckEquals('root', ResultSet.GetStringByName('GRANTOR'));
  CheckEquals('people@%', ResultSet.GetStringByName('GRANTEE'));
  CheckEquals('Update', ResultSet.GetStringByName('PRIVILEGE'));
  CheckEquals('', ResultSet.GetStringByName('IS_GRANTABLE'));

  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test for method GetColumns
}
procedure TZTestMySqlMetadataCase.TestGetColumns;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetColumns('', '', 'people', 'p_r%');
  CheckEquals(1, ResultSet.FindColumn('TABLE_CAT'));
  CheckEquals(2, ResultSet.FindColumn('TABLE_SCHEM'));
  CheckEquals(3, ResultSet.FindColumn('TABLE_NAME'));
  CheckEquals(4, ResultSet.FindColumn('COLUMN_NAME'));
  CheckEquals(5, ResultSet.FindColumn('DATA_TYPE'));
  CheckEquals(6, ResultSet.FindColumn('TYPE_NAME'));
  CheckEquals(7, ResultSet.FindColumn('COLUMN_SIZE'));
  CheckEquals(8, ResultSet.FindColumn('BUFFER_LENGTH'));
  CheckEquals(9, ResultSet.FindColumn('DECIMAL_DIGITS'));
  CheckEquals(10, ResultSet.FindColumn('NUM_PREC_RADIX'));
  CheckEquals(11, ResultSet.FindColumn('NULLABLE'));
  CheckEquals(12, ResultSet.FindColumn('REMARKS'));
  CheckEquals(13, ResultSet.FindColumn('COLUMN_DEF'));
  CheckEquals(14, ResultSet.FindColumn('SQL_DATA_TYPE'));
  CheckEquals(15, ResultSet.FindColumn('SQL_DATETIME_SUB'));
  CheckEquals(16, ResultSet.FindColumn('CHAR_OCTET_LENGTH'));
  CheckEquals(17, ResultSet.FindColumn('ORDINAL_POSITION'));
  CheckEquals(18, ResultSet.FindColumn('IS_NULLABLE'));

  ResultSet.Next;
  CheckEquals('zeoslib', ResultSet.GetStringByName('TABLE_CAT'));
  CheckEquals('', ResultSet.GetStringByName('TABLE_SCHEM'));
  CheckEquals('people', ResultSet.GetStringByName('TABLE_NAME'));
  CheckEquals('p_resume', ResultSet.GetStringByName('COLUMN_NAME'));
  CheckEquals(ord(stAsciiStream), ResultSet.GetIntByName('DATA_TYPE'));
  CheckEquals('TEXT', UpperCase(ResultSet.GetStringByName('TYPE_NAME')));
  CheckEquals(65535, ResultSet.GetIntByName('COLUMN_SIZE'));
  CheckEquals(65535, ResultSet.GetIntByName('BUFFER_LENGTH'));
  CheckEquals(0, ResultSet.GetIntByName('DECIMAL_DIGITS'));
  CheckEquals(0, ResultSet.GetIntByName('NUM_PREC_RADIX'));
  CheckEquals(1, ResultSet.GetIntByName('NULLABLE'));
  CheckEquals('', ResultSet.GetStringByName('REMARKS'));
  CheckEquals('', ResultSet.GetStringByName('COLUMN_DEF'));
  CheckEquals(0, ResultSet.GetIntByName('SQL_DATA_TYPE'));
  CheckEquals(0, ResultSet.GetIntByName('SQL_DATETIME_SUB'));
  CheckEquals(0, ResultSet.GetIntByName('CHAR_OCTET_LENGTH'));
  CheckEquals(1, ResultSet.GetIntByName('ORDINAL_POSITION'));
  CheckEquals('YES', ResultSet.GetStringByName('IS_NULLABLE'));

  ResultSet.Next;
  CheckEquals('zeoslib', ResultSet.GetStringByName('TABLE_CAT'));
  CheckEquals('', ResultSet.GetStringByName('TABLE_SCHEM'));
  CheckEquals('people', ResultSet.GetStringByName('TABLE_NAME'));
  CheckEquals('p_redundant', ResultSet.GetStringByName('COLUMN_NAME'));
  CheckEquals(ord(stByte), ResultSet.GetIntByName('DATA_TYPE'));
  CheckEquals('TINYINT', UpperCase(ResultSet.GetStringByName('TYPE_NAME')));
  CheckEquals(1, ResultSet.GetIntByName('COLUMN_SIZE'));
  CheckEquals(65535, ResultSet.GetIntByName('BUFFER_LENGTH'));
  CheckEquals(0, ResultSet.GetIntByName('DECIMAL_DIGITS'));
  CheckEquals(0, ResultSet.GetIntByName('NUM_PREC_RADIX'));
  CheckEquals(1, ResultSet.GetIntByName('NULLABLE'));
  CheckEquals('', ResultSet.GetStringByName('REMARKS'));
  CheckEquals('', ResultSet.GetStringByName('COLUMN_DEF'));
  CheckEquals(0, ResultSet.GetIntByName('SQL_DATA_TYPE'));
  CheckEquals(0, ResultSet.GetIntByName('SQL_DATETIME_SUB'));
  CheckEquals(0, ResultSet.GetIntByName('CHAR_OCTET_LENGTH'));
  CheckEquals(2, ResultSet.GetIntByName('ORDINAL_POSITION'));
  CheckEquals('YES', ResultSet.GetStringByName('IS_NULLABLE'));

  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test for method GetProcedureColumns
}
procedure TZTestMySqlMetadataCase.TestGetProcedureColumns;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetProcedureColumns('', '', '', '');
  CheckEquals(1, ResultSet.FindColumn('PROCEDURE_CAT'));
  CheckEquals(2, ResultSet.FindColumn('PROCEDURE_SCHEM'));
  CheckEquals(3, ResultSet.FindColumn('PROCEDURE_NAME'));
  CheckEquals(4, ResultSet.FindColumn('COLUMN_NAME'));
  CheckEquals(5, ResultSet.FindColumn('COLUMN_TYPE'));
  CheckEquals(6, ResultSet.FindColumn('DATA_TYPE'));
  CheckEquals(7, ResultSet.FindColumn('TYPE_NAME'));
  CheckEquals(8, ResultSet.FindColumn('PRECISION'));
  CheckEquals(9, ResultSet.FindColumn('LENGTH'));
  CheckEquals(10, ResultSet.FindColumn('SCALE'));
  CheckEquals(11, ResultSet.FindColumn('RADIX'));
  CheckEquals(12, ResultSet.FindColumn('NULLABLE'));
  CheckEquals(13, ResultSet.FindColumn('REMARKS'));
  Check(not ResultSet.Next);
  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test for method GetProcedures
}
procedure TZTestMySqlMetadataCase.TestGetProcedures;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetProcedures('', '', '');
  CheckEquals(1, ResultSet.FindColumn('PROCEDURE_CAT'));
  CheckEquals(2, ResultSet.FindColumn('PROCEDURE_SCHEM'));
  CheckEquals(3, ResultSet.FindColumn('PROCEDURE_NAME'));
  CheckEquals(7, ResultSet.FindColumn('REMARKS'));
  CheckEquals(8, ResultSet.FindColumn('PROCEDURE_TYPE'));
  Check(not ResultSet.Next);
  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test for method GetSchemas
}
procedure TZTestMySqlMetadataCase.TestGetSchemas;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetSchemas;
  CheckEquals(1, ResultSet.FindColumn('TABLE_SCHEM'));
  Check(not ResultSet.Next);
  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test for method GetTablePrivileges
}
procedure TZTestMySqlMetadataCase.TestGetTablePrivileges;
var
  ResultSet: IZResultSet;
begin
  { To grant privileges
    grant select privileges on zeoslib.people to root@localhist;
    The result sql is:
    SELECT host,db,table_name,grantor,user,table_priv from mysql.tables_priv
    WHERE table_name LIKE 'people';}
  ResultSet := Metadata.GetTablePrivileges('', '', 'people');
  CheckEquals(1, ResultSet.FindColumn('TABLE_CAT'));
  CheckEquals(2, ResultSet.FindColumn('TABLE_SCHEM'));
  CheckEquals(3, ResultSet.FindColumn('TABLE_NAME'));
  CheckEquals(4, ResultSet.FindColumn('GRANTOR'));
  CheckEquals(5, ResultSet.FindColumn('GRANTEE'));
  CheckEquals(6, ResultSet.FindColumn('PRIVILEGE'));
  CheckEquals(7, ResultSet.FindColumn('IS_GRANTABLE'));

  Check(ResultSet.Next);
  CheckEquals('zeoslib', ResultSet.GetStringByName('TABLE_CAT'));
  CheckEquals('', ResultSet.GetStringByName('TABLE_SCHEM'));
  CheckEquals('people', ResultSet.GetStringByName('TABLE_NAME'));
//!!!  CheckEquals('root@localhost', ResultSet.GetStringByName('GRANTOR'));
  CheckEquals('root@localhost', ResultSet.GetStringByName('GRANTEE'));
  CheckEquals('Select', ResultSet.GetStringByName('PRIVILEGE'));
  CheckEquals('', ResultSet.GetStringByName('IS_GRANTABLE'));
  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test for method GetTables
}
procedure TZTestMySqlMetadataCase.TestGetTables;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetTables('', '', 'people', nil);
  CheckEquals(1, ResultSet.FindColumn('TABLE_CAT'));
  CheckEquals(2, ResultSet.FindColumn('TABLE_SCHEM'));
  CheckEquals(3, ResultSet.FindColumn('TABLE_NAME'));
  CheckEquals(4, ResultSet.FindColumn('TABLE_TYPE'));
  CheckEquals(5, ResultSet.FindColumn('REMARKS'));

  ResultSet.Next;
  CheckEquals('zeoslib', ResultSet.GetStringByName('TABLE_CAT'));
  CheckEquals('', ResultSet.GetStringByName('TABLE_SCHEM'));
  CheckEquals('people', ResultSet.GetStringByName('TABLE_NAME'));
  CheckEquals('TABLE', ResultSet.GetStringByName('TABLE_TYPE'));
  CheckEquals('', ResultSet.GetStringByName('REMARKS'));
  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test for method GetTableTypes
}
procedure TZTestMySqlMetadataCase.TestGetTableTypes;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetTableTypes;
  CheckEquals(1, ResultSet.FindColumn('TABLE_TYPE'));

  ResultSet.Next;
  CheckEquals('TABLE', ResultSet.GetStringByName('TABLE_TYPE'));
  ResultSet.Close;
  ResultSet := nil;
end;

initialization
  RegisterTest('dbc',TZTestMySqlMetadataCase.Suite);
end.
