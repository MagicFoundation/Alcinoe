{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Tests for PostgreSQL Database Metadata Class     }
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

unit ZTestDbcPostgreSqlMetadata;

interface
{$I ZDbc.inc}
uses
  SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZClasses, ZCompatibility,
  ZTestDefinitions, ZDbcResultSet, ZDbcResultSetMetadata, ZDbcPostgreSql;

type

 {** Implements a test case for TZMySqlMetadata. }
  TZTestPostgreSqlMetadataCase = class(TZDbcSpecificSQLTestCase)
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
    procedure TestGetVersionColumns;
    procedure TestGetPrimaryKeys;
    procedure TestGetImportedKeys;
    procedure TestGetExportedKeys;
    procedure TestGetCrossReference;
    procedure TestGetTypeInfo;
    procedure TestGetIndexInfo;
    procedure TestIdentifierQuoting;
  end;

implementation

uses ZTestCase;

{ TZTestPostgreSqlMetadataCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestPostgreSqlMetadataCase.GetSupportedProtocols: string;
begin
  Result := 'postgresql,postgresql-7,postgresql-8';
end;

{**
   Test version columns
}
procedure TZTestPostgreSqlMetadataCase.TestGetVersionColumns;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetVersionColumns('', '', '');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('SCOPE'));
    CheckEquals(2, FindColumn('COLUMN_NAME'));
    CheckEquals(3, FindColumn('DATA_TYPE'));
    CheckEquals(4, FindColumn('TYPE_NAME'));
    CheckEquals(5, FindColumn('COLUMN_SIZE'));
    CheckEquals(6, FindColumn('BUFFER_LENGTH'));
    CheckEquals(7, FindColumn('DECIMAL_DIGITS'));
    CheckEquals(8, FindColumn('PSEUDO_COLUMN'));

    CheckEquals(True, Next);
    CheckEquals(True, IsNullByName('SCOPE'));
    CheckEquals('ctid', GetStringByName('COLUMN_NAME'));
    CheckEquals(0, GetIntByName('DATA_TYPE'));
    CheckEquals(2, GetIntByName('TYPE_NAME'));
    CheckEquals(True, IsNullByName('COLUMN_SIZE'));
    CheckEquals(True, IsNullByName('BUFFER_LENGTH'));
    CheckEquals(True, IsNullByName('DECIMAL_DIGITS'));
//    CheckEquals(ord(vcPseudo), GetIntByName('PSEUDO_COLUMN')); FIX IT
    CheckEquals(False, Next);
    Close;
  end;
  ResultSet := nil;
end;

{**
   Create objects and allocate memory for variables
}
procedure TZTestPostgreSqlMetadataCase.SetUp;
begin
  Connection := CreateDbcConnection;
  Metadata := Connection.GetMetadata;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestPostgreSqlMetadataCase.TearDown;
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
procedure TZTestPostgreSqlMetadataCase.TestGetBestRowIdentifier;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetBestRowIdentifier('', '', 'people', 0, false);
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('SCOPE'));
    CheckEquals(2, FindColumn('COLUMN_NAME'));
    CheckEquals(3, FindColumn('DATA_TYPE'));
    CheckEquals(4, FindColumn('TYPE_NAME'));
    CheckEquals(5, FindColumn('COLUMN_SIZE'));
    CheckEquals(6, FindColumn('BUFFER_LENGTH'));
    CheckEquals(7, FindColumn('DECIMAL_DIGITS'));
    CheckEquals(8, FindColumn('PSEUDO_COLUMN'));

    CheckEquals(True, Next);
    CheckEquals(2, GetIntByName('SCOPE'));
    CheckEquals('p_id', GetStringByName('COLUMN_NAME'));
    CheckEquals(4, GetIntByName('DATA_TYPE'));
    CheckEquals('int4', GetStringByName('TYPE_NAME'));
    CheckEquals(4, GetIntByName('COLUMN_SIZE'));
    CheckEquals(0, GetIntByName('BUFFER_LENGTH'));
    CheckEquals(0, GetIntByName('DECIMAL_DIGITS'));
    CheckEquals(1, GetIntByName('PSEUDO_COLUMN'));
    Close;
  end;
  ResultSet := nil;
end;

procedure TZTestPostgreSqlMetadataCase.TestGetCatalogs;
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
procedure TZTestPostgreSqlMetadataCase.TestGetColumnPrivileges;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetColumnPrivileges('', '', 'people', 'p_r%');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('TABLE_CAT'));
    CheckEquals(2, FindColumn('TABLE_SCHEM'));
    CheckEquals(3, FindColumn('TABLE_NAME'));
    CheckEquals(4, FindColumn('COLUMN_NAME'));
    CheckEquals(5, FindColumn('GRANTOR'));
    CheckEquals(6, FindColumn('GRANTEE'));
    CheckEquals(7, FindColumn('PRIVILEGE'));
    CheckEquals(8, FindColumn('IS_GRANTABLE'));

{    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('p_redundant', GetStringByName('COLUMN_NAME'));
//    CheckEquals('root', GetStringByName('GRANTOR'));
//    CheckEquals('root', GetStringByName('GRANTEE'));
    CheckEquals('INSERT', GetStringByName('PRIVILEGE'));
    CheckEquals('YES', GetStringByName('IS_GRANTABLE'));}
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetColumns
}
procedure TZTestPostgreSqlMetadataCase.TestGetColumns;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetColumns('', '', 'people', 'p_r%');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('TABLE_CAT'));
    CheckEquals(2, FindColumn('TABLE_SCHEM'));
    CheckEquals(3, FindColumn('TABLE_NAME'));
    CheckEquals(4, FindColumn('COLUMN_NAME'));
    CheckEquals(5, FindColumn('DATA_TYPE'));
    CheckEquals(6, FindColumn('TYPE_NAME'));
    CheckEquals(7, FindColumn('COLUMN_SIZE'));
    CheckEquals(8, FindColumn('BUFFER_LENGTH'));
    CheckEquals(9, FindColumn('DECIMAL_DIGITS'));
    CheckEquals(10, FindColumn('NUM_PREC_RADIX'));
    CheckEquals(11, FindColumn('NULLABLE'));
    CheckEquals(12, FindColumn('REMARKS'));
    CheckEquals(13, FindColumn('COLUMN_DEF'));
    CheckEquals(14, FindColumn('SQL_DATA_TYPE'));
    CheckEquals(15, FindColumn('SQL_DATETIME_SUB'));
    CheckEquals(16, FindColumn('CHAR_OCTET_LENGTH'));
    CheckEquals(17, FindColumn('ORDINAL_POSITION'));
    CheckEquals(18, FindColumn('IS_NULLABLE'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('p_resume', GetStringByName('COLUMN_NAME'));
    CheckEquals(ord(stAsciiStream), GetIntByName('DATA_TYPE'));
    CheckEquals('TEXT', UpperCase(GetStringByName('TYPE_NAME')));
    CheckEquals(-1, GetIntByName('COLUMN_SIZE'));
    CheckEquals(0, GetIntByName('BUFFER_LENGTH'));
    CheckEquals(0, GetIntByName('DECIMAL_DIGITS'));
    CheckEquals(2, GetIntByName('NUM_PREC_RADIX'));
    CheckEquals(1, GetIntByName('NULLABLE'));
    CheckEquals('', GetStringByName('REMARKS'));
    CheckEquals('', GetStringByName('COLUMN_DEF'));
    CheckEquals(0, GetIntByName('SQL_DATA_TYPE'));
    CheckEquals(0, GetIntByName('SQL_DATETIME_SUB'));
    CheckEquals(-1, GetIntByName('CHAR_OCTET_LENGTH'));
    CheckEquals(7, GetIntByName('ORDINAL_POSITION'));
    CheckEquals('YES', GetStringByName('IS_NULLABLE'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('p_redundant', GetStringByName('COLUMN_NAME'));
    CheckEquals(ord(stShort), GetIntByName('DATA_TYPE'));
    CheckEquals('INT2', UpperCase(GetStringByName('TYPE_NAME')));
    CheckEquals(2, GetIntByName('COLUMN_SIZE'));
    CheckEquals(0, GetIntByName('BUFFER_LENGTH'));
    CheckEquals(0, GetIntByName('DECIMAL_DIGITS'));
    CheckEquals(2, GetIntByName('NUM_PREC_RADIX'));
    CheckEquals(1, GetIntByName('NULLABLE'));
    CheckEquals('', GetStringByName('REMARKS'));
    CheckEquals('', GetStringByName('COLUMN_DEF'));
    CheckEquals(0, GetIntByName('SQL_DATA_TYPE'));
    CheckEquals(0, GetIntByName('SQL_DATETIME_SUB'));
    CheckEquals(2, GetIntByName('CHAR_OCTET_LENGTH'));
    CheckEquals(8, GetIntByName('ORDINAL_POSITION'));
    CheckEquals('YES', GetStringByName('IS_NULLABLE'));

    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetProcedureColumns
}
procedure TZTestPostgreSqlMetadataCase.TestGetProcedureColumns;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetProcedureColumns('', '', 'procedure1', '');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('PROCEDURE_CAT'));
    CheckEquals(2, FindColumn('PROCEDURE_SCHEM'));
    CheckEquals(3, FindColumn('PROCEDURE_NAME'));
    CheckEquals(4, FindColumn('COLUMN_NAME'));
    CheckEquals(5, FindColumn('COLUMN_TYPE'));
    CheckEquals(6, FindColumn('DATA_TYPE'));
    CheckEquals(7, FindColumn('TYPE_NAME'));
    CheckEquals(8, FindColumn('PRECISION'));
    CheckEquals(9, FindColumn('LENGTH'));
    CheckEquals(10, FindColumn('SCALE'));
    CheckEquals(11, FindColumn('RADIX'));
    CheckEquals(12, FindColumn('NULLABLE'));
    CheckEquals(13, FindColumn('REMARKS'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PROCEDURE_CAT'));
//    CheckEquals('public', GetStringByName('PROCEDURE_SCHEM'));
    CheckEquals('procedure1', GetStringByName('PROCEDURE_NAME'));
    CheckEquals('returnValue', GetStringByName('COLUMN_NAME'));
    CheckEquals('4', GetStringByName('COLUMN_TYPE'));
    CheckEquals('4', GetStringByName('DATA_TYPE'));
    CheckEquals('int4', GetStringByName('TYPE_NAME'));
    CheckEquals('', GetStringByName('PRECISION'));
    CheckEquals('', GetStringByName('LENGTH'));
    CheckEquals('', GetStringByName('SCALE'));
    CheckEquals('', GetStringByName('RADIX'));
    CheckEquals('2', GetStringByName('NULLABLE'));
    CheckEquals('', GetStringByName('REMARKS'));
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetProcedures
}
procedure TZTestPostgreSqlMetadataCase.TestGetProcedures;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetProcedures('', '', 'procedure%');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('PROCEDURE_CAT'));
    CheckEquals(2, FindColumn('PROCEDURE_SCHEM'));
    CheckEquals(3, FindColumn('PROCEDURE_NAME'));
    CheckEquals(7, FindColumn('REMARKS'));
    CheckEquals(8, FindColumn('PROCEDURE_TYPE'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PROCEDURE_CAT'));
//    CheckEquals('public', GetStringByName('PROCEDURE_SCHEM'));
    CheckEquals('procedure1', GetStringByName('PROCEDURE_NAME'));
    CheckEquals('', GetStringByName('REMARKS'));
    CheckEquals(2, GetIntByName('PROCEDURE_TYPE'));

    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetSchemas
}
procedure TZTestPostgreSqlMetadataCase.TestGetSchemas;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetSchemas;
  CheckEquals(1, ResultSet.FindColumn('TABLE_SCHEM'));
  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test for method GetTablePrivileges
}
procedure TZTestPostgreSqlMetadataCase.TestGetTablePrivileges;
var
  ResultSet: IZResultSet;
begin
  { To grant privileges
    grant select on people to root }
  ResultSet := Metadata.GetTablePrivileges('', '', 'people');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('TABLE_CAT'));
    CheckEquals(2, FindColumn('TABLE_SCHEM'));
    CheckEquals(3, FindColumn('TABLE_NAME'));
    CheckEquals(4, FindColumn('GRANTOR'));
    CheckEquals(5, FindColumn('GRANTEE'));
    CheckEquals(6, FindColumn('PRIVILEGE'));
    CheckEquals(7, FindColumn('IS_GRANTABLE'));

{    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    //CheckEquals('root', GetStringByName('GRANTOR'));
    CheckEquals('root', GetStringByName('GRANTEE'));
    CheckEquals('SELECT', GetStringByName('PRIVILEGE'));
    CheckEquals('NO', GetStringByName('IS_GRANTABLE'));}
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetTables
}
procedure TZTestPostgreSqlMetadataCase.TestGetTables;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetTables('', '', 'people', nil);
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('TABLE_CAT'));
    CheckEquals(2, FindColumn('TABLE_SCHEM'));
    CheckEquals(3, FindColumn('TABLE_NAME'));
    CheckEquals(4, FindColumn('TABLE_TYPE'));
    CheckEquals(5, FindColumn('REMARKS'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('TABLE', GetStringByName('TABLE_TYPE'));
    CheckEquals('', GetStringByName('REMARKS'));
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetTableTypes
}
procedure TZTestPostgreSqlMetadataCase.TestGetTableTypes;
const
  Types: array [0..10] of string = ('TABLE', 'VIEW', 'INDEX',
    'SEQUENCE', 'SYSTEM TABLE', 'SYSTEM TOAST TABLE',
    'SYSTEM TOAST INDEX', 'SYSTEM VIEW', 'SYSTEM INDEX',
    'TEMPORARY TABLE', 'TEMPORARY INDEX');
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetTableTypes;
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[0], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[1], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[2], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[3], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[4], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[5], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[6], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[7], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[8], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[9], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[10], GetStringByName('TABLE_TYPE'));
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetPrimaryKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetPrimaryKeys;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetPrimaryKeys('', '', 'people');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('TABLE_CAT'));
    CheckEquals(2, FindColumn('TABLE_SCHEM'));
    CheckEquals(3, FindColumn('TABLE_NAME'));
    CheckEquals(4, FindColumn('COLUMN_NAME'));
    CheckEquals(5, FindColumn('KEY_SEQ'));
    CheckEquals(6, FindColumn('PK_NAME'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('p_id', GetStringByName('COLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals('people_pkey', GetStringByName('PK_NAME'));
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetImportedKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetImportedKeys;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetImportedKeys('', '', 'people');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('PKTABLE_CAT'));
    CheckEquals(2, FindColumn('PKTABLE_SCHEM'));
    CheckEquals(3, FindColumn('PKTABLE_NAME'));
    CheckEquals(4, FindColumn('PKCOLUMN_NAME'));
    CheckEquals(5, FindColumn('FKTABLE_CAT'));
    CheckEquals(6, FindColumn('FKTABLE_SCHEM'));
    CheckEquals(7, FindColumn('FKTABLE_NAME'));
    CheckEquals(8, FindColumn('FKCOLUMN_NAME'));
    CheckEquals(9, FindColumn('KEY_SEQ'));
    CheckEquals(10, FindColumn('UPDATE_RULE'));
    CheckEquals(11, FindColumn('DELETE_RULE'));
    CheckEquals(12, FindColumn('FK_NAME'));
    CheckEquals(13, FindColumn('PK_NAME'));
    CheckEquals(14, FindColumn('DEFERRABILITY'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));

    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    CheckEquals('', GetStringByName('FKTABLE_CAT'));

//    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('people', GetStringByName('FKTABLE_NAME'));
    CheckEquals('p_dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
//    CheckEquals('<unnamed>', GetStringByName('FK_NAME'));
    CheckEquals('1', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));
    CheckEquals(False, Next);
  end;
  ResultSet := nil;
end;

{**
   Test for method GetExportedKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetExportedKeys;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetExportedKeys('', '', 'department');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('PKTABLE_CAT'));
    CheckEquals(2, FindColumn('PKTABLE_SCHEM'));
    CheckEquals(3, FindColumn('PKTABLE_NAME'));
    CheckEquals(4, FindColumn('PKCOLUMN_NAME'));
    CheckEquals(5, FindColumn('FKTABLE_CAT'));
    CheckEquals(6, FindColumn('FKTABLE_SCHEM'));
    CheckEquals(7, FindColumn('FKTABLE_NAME'));
    CheckEquals(8, FindColumn('FKCOLUMN_NAME'));
    CheckEquals(9, FindColumn('KEY_SEQ'));
    CheckEquals(10, FindColumn('UPDATE_RULE'));
    CheckEquals(11, FindColumn('DELETE_RULE'));
    CheckEquals(12, FindColumn('FK_NAME'));
    CheckEquals(13, FindColumn('PK_NAME'));
    CheckEquals(14, FindColumn('DEFERRABILITY'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));
    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    CheckEquals('', GetStringByName('FKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('cargo', GetStringByName('FKTABLE_NAME'));
    CheckEquals('c_dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
//    CheckEquals('<unnamed>', GetStringByName('FK_NAME'));
    CheckEquals('1', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));
    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    CheckEquals('', GetStringByName('FKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('equipment2', GetStringByName('FKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
//    CheckEquals('<unnamed>', GetStringByName('FK_NAME'));
    CheckEquals('1', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));
    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    CheckEquals('', GetStringByName('FKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('people', GetStringByName('FKTABLE_NAME'));
    CheckEquals('p_dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
//    CheckEquals('<unnamed>', GetStringByName('FK_NAME'));
    CheckEquals('1', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));
    CheckEquals(False, Next);
  end;
  ResultSet := nil;
end;

{**
   Test for method GetExportedKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetCrossReference;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetCrossReference('', '', 'department', '', '', 'people');
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('PKTABLE_CAT'));
    CheckEquals(2, FindColumn('PKTABLE_SCHEM'));
    CheckEquals(3, FindColumn('PKTABLE_NAME'));
    CheckEquals(4, FindColumn('PKCOLUMN_NAME'));
    CheckEquals(5, FindColumn('FKTABLE_CAT'));
    CheckEquals(6, FindColumn('FKTABLE_SCHEM'));
    CheckEquals(7, FindColumn('FKTABLE_NAME'));
    CheckEquals(8, FindColumn('FKCOLUMN_NAME'));
    CheckEquals(9, FindColumn('KEY_SEQ'));
    CheckEquals(10, FindColumn('UPDATE_RULE'));
    CheckEquals(11, FindColumn('DELETE_RULE'));
    CheckEquals(12, FindColumn('FK_NAME'));
    CheckEquals(13, FindColumn('PK_NAME'));
    CheckEquals(14, FindColumn('DEFERRABILITY'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));
    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    CheckEquals('', GetStringByName('FKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('people', GetStringByName('FKTABLE_NAME'));
    CheckEquals('p_dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
//    CheckEquals('<unnamed>', GetStringByName('FK_NAME'));
    CheckEquals('1', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));
    CheckEquals(False, Next);
  end;
  ResultSet := nil;
end;

{**
   Test for method GetExportedKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetTypeInfo;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetTypeInfo;
  with ResultSet do
  begin
    CheckEquals(True, Next);
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetIndexInfo
}
procedure TZTestPostgreSqlMetadataCase.TestGetIndexInfo;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetIndexInfo('', '', 'department', True, True);
  with ResultSet do
  begin
    CheckEquals(1, FindColumn('TABLE_CAT'));
    CheckEquals(2, FindColumn('TABLE_SCHEM'));
    CheckEquals(3, FindColumn('TABLE_NAME'));
    CheckEquals(4, FindColumn('NON_UNIQUE'));
    CheckEquals(5, FindColumn('INDEX_QUALIFIER'));
    CheckEquals(6, FindColumn('INDEX_NAME'));
    CheckEquals(7, FindColumn('TYPE'));
    CheckEquals(8, FindColumn('ORDINAL_POSITION'));
    CheckEquals(9, FindColumn('COLUMN_NAME'));
    CheckEquals(10, FindColumn('ASC_OR_DESC'));
    CheckEquals(11, FindColumn('CARDINALITY'));
    CheckEquals(12, FindColumn('PAGES'));
    CheckEquals(13, FindColumn('FILTER_CONDITION'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('department', GetStringByName('TABLE_NAME'));
    CheckEquals(False, GetBooleanByName('NON_UNIQUE'));
    CheckEquals('', GetStringByName('INDEX_QUALIFIER'));
    CheckEquals('department_pkey', GetStringByName('INDEX_NAME'));
    CheckEquals(3, GetIntByName('TYPE'));
    CheckEquals(1, GetIntByName('ORDINAL_POSITION'));
    CheckEquals('dep_id', GetStringByName('COLUMN_NAME'));
    CheckEquals('', GetStringByName('ASC_OR_DESC'));
//    CheckEquals(1000, GetIntByName('CARDINALITY'));
    CheckEquals(1, GetIntByName('PAGES'));
    CheckEquals('', GetStringByName('FILTER_CONDITION'));
    Close;
  end;
  ResultSet := nil;
end;

procedure TZTestPostgreSqlMetadataCase.TestIdentifierQuoting;
begin
      Check(Metadata.GetIdentifierConvertor.Quote('A9A')=Metadata.GetDatabaseInfo.GetIdentifierQuoteString[1]+'A9A'+Metadata.GetDatabaseInfo.GetIdentifierQuoteString[length(Metadata.GetDatabaseInfo.GetIdentifierQuoteString)]);
      Check(Metadata.GetIdentifierConvertor.Quote('a9A')=Metadata.GetDatabaseInfo.GetIdentifierQuoteString[1]+'a9A'+Metadata.GetDatabaseInfo.GetIdentifierQuoteString[length(Metadata.GetDatabaseInfo.GetIdentifierQuoteString)]);
end;

initialization
  RegisterTest('dbc',TZTestPostgreSqlMetadataCase.Suite);
end.
