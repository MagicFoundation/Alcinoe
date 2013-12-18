{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         ASA Database Connectivity Classes               }
{                                                         }
{       Originally written by Sergey Merkuriev            }
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

unit ZTestDbcASAMetadata;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils, ZDbcIntfs, ZTestDefinitions, ZCompatibility,
  ZDbcMySql, ZDbcPostgreSql, ZDbcDbLib, ZDbcASA;

type
  {** Implements a test case for. }
  TZASATestDbcMetadata = class(TZDbcSpecificSQLTestCase)
  private
    FConnection: IZConnection;
    MD: IZDatabaseMetadata;
    Catalog, Schema: string;
    ResultSet: IZResultSet;
    TableTypes: TStringDynArray;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: IZConnection read FConnection write FConnection;
  published
    procedure TestMetadataGetCatalogs;
    procedure TestMetadataGetSchemas;
    procedure TestMetadataGetTableTypes;
    procedure TestMetadataGetTables;
    procedure TestMetadataGetColumns;
    procedure TestMetadataGetTablePrivileges;
    procedure TestMetadataGetColumnPrivileges;
    procedure TestMetadataGetBestRowIdentifier;
    procedure TestMetadataGetVersionColumns;
    procedure TestMetadataGetPrimaryKeys;
    procedure TestMetadataGetImportedKeys;
    procedure TestMetadataGetExportedKeys;
    procedure TestMetadataGetCrossReference;
    procedure TestMetadataGetIndexInfo;
    procedure TestMetadataGetProcedures;
    procedure TestMetadataGetProcedureColumns;
    procedure TestMetadataGetTypeInfo;
  end;

implementation

uses ZSysUtils, ZTestConsts;

{ TZASATestDbcMetadata }

{**
   Create objects and allocate memory for variables
}
procedure TZASATestDbcMetadata.SetUp;
begin
  Connection := CreateDbcConnection;
  CheckNotNull(Connection);
  MD := Connection.GetMetadata;
  CheckNotNull(MD);

  SetLength(TableTypes, 1);
  TableTypes[0] := 'TABLE';

  ResultSet := MD.GetTables('', '', 'PEOPLE', TableTypes);
  CheckEquals(ResultSet.First, True, 'No people table');
  Catalog := ResultSet.GetStringByName('TABLE_CAT');
  Schema := ResultSet.GetStringByName('TABLE_SCHEM');
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZASATestDbcMetadata.TearDown;
begin
  ResultSet := nil;
  MD := nil;
  Connection.Close;
  Connection := nil;
end;

procedure TZASATestDbcMetadata.TestMetadataGetTableTypes;
begin
  Resultset := MD.GetTableTypes;
  CheckNotNull(ResultSet, 'The resultset is nil');
  PrintResultset(Resultset, False, 'GetTableTypes');
  CheckEquals(1, Resultset.FindColumn('TABLE_TYPE'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetCatalogs;
begin
  Resultset := MD.GetCatalogs;
  CheckNotNull(ResultSet, 'The resultset is nil');
  PrintResultset(Resultset, False, 'GetCatalogs');
  CheckEquals(1, Resultset.FindColumn('TABLE_CAT'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetSchemas;
begin
  Resultset := MD.GetSchemas;
  CheckNotNull(ResultSet, 'The resultset is nil');
  PrintResultset(Resultset, False, 'GetSchemas');
  CheckEquals(1, Resultset.FindColumn('TABLE_SCHEM'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetTables;
const
  Tables: array[0..8] of string = ('PEOPLE', 'BLOB_VALUES', 'CARGO', 'DATE_VALUES', 'DEPARTMENT', 'EQUIPMENT', 'EQUIPMENT2', 'NUMBER_VALUES', 'STRING_VALUES');
var
  I: Integer;
begin
  ResultSet := MD.GetTables(Catalog, Schema, '%', TableTypes);
  CheckNotNull(ResultSet);
  PrintResultSet(Resultset, False, 'GetTables');

  for I := Low(Tables) to High(Tables) do
  begin
    ResultSet := MD.GetTables(Catalog, Schema, Tables[I], TableTypes);
    CheckNotNull(ResultSet, 'The resultset is nil');
    CheckEquals(ResultSet.First, True, 'No ' + Tables[I] + ' table');
    CheckEquals(Catalog, Resultset.GetStringByName('TABLE_CAT'));
    CheckEquals(Schema, Resultset.GetStringByName('TABLE_SCHEM'));
    CheckEquals(UpperCase(Tables[I]), UpperCase(Resultset.GetStringByName('TABLE_NAME')));
    CheckEquals(UpperCase('table'), UpperCase(Resultset.GetStringByName('TABLE_TYPE')));
    CheckEquals('', Resultset.GetStringByName('REMARKS'));
  end;
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetColumns;
  procedure CheckColumns(Catalog, Schema, TableName, ColumnName: string;
  DataType: SmallInt; TypeName: string; ColumnSize, BufferLength, DecimalDigits,
  Radix, Nullable: Integer; Remarks, ColumnDef: string; SqlDataType,
  SqlDateTimeSub, CharOctetLength, OrdinalPosition: Integer; IsNullable: string);
  begin
    CheckEquals(ResultSet.Next, True, 'The column is missing: ' + ColumnName);
    CheckEquals(Catalog, ResultSet.GetStringByName('TABLE_CAT'));
    CheckEquals(Schema, ResultSet.GetStringByName('TABLE_SCHEM'));
    CheckEquals(UpperCase(TableName), UpperCase(ResultSet.GetStringByName('TABLE_NAME')));
    CheckEquals(UpperCase(ColumnName), UpperCase(ResultSet.GetStringByName('COLUMN_NAME')));
//    CheckEquals(DataType, ResultSet.GetShortByName('DATA_TYPE'));
//    CheckEquals(TypeName, ResultSet.GetStringByName('TYPE_NAME'));
//    CheckEquals(ColumnSize, ResultSet.GetIntByName('COLUMN_SIZE'));
//    CheckEquals(BufferLength, ResultSet.GetIntByName('BUFFER_LENGTH'));
//    CheckEquals(DecimalDigits, ResultSet.GetIntByName('DECIMAL_DIGITS'));
//    CheckEquals(Radix, ResultSet.GetIntByName('NUM_PREC_RADIX'));
    CheckEquals(Nullable, ResultSet.GetIntByName('NULLABLE'));
//    CheckEquals(UpperCase(Remarks), UpperCase(ResultSet.GetStringByName('REMARKS')));
//    CheckEquals(UpperCase(ColumnDef), UpperCase(ResultSet.GetStringByName('COLUMN_DEF')));
//    CheckEquals(SqlDataType, ResultSet.GetIntByName('SQL_DATA_TYPE'));
//    CheckEquals(SqlDateTimeSub, ResultSet.GetIntByName('SQL_DATETIME_SUB'));
//    CheckEquals(CharOctetLength, ResultSet.GetIntByName('CHAR_OCTET_LENGTH'));
    CheckEquals(OrdinalPosition, ResultSet.GetIntByName('ORDINAL_POSITION'));
    CheckEquals(UpperCase(IsNullable), UpperCase(ResultSet.GetStringByName('IS_NULLABLE')));
  end;
begin
  ResultSet := MD.GetColumns(Catalog, Schema, 'PEOPLE', '');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, False);

  CheckColumns(Catalog, Schema, 'PEOPLE', 'P_ID', 5, '', 2, 2, 0, 10, 0, '', '', 5, 0, 0, 1, 'NO');
  CheckColumns(Catalog, Schema, 'PEOPLE', 'P_DEP_ID', 5, '', 2, 2, 0, 10, 1, '', '', 5, 0, 0, 2, 'YES');
  CheckColumns(Catalog, Schema, 'PEOPLE', 'P_NAME', 12, '', 40, 40, 0, 0, 1, '', '', 12, 0, 40, 3, 'YES');
  CheckColumns(Catalog, Schema, 'PEOPLE', 'P_BEGIN_WORK', 11, '', 16, 16, 0, 0, 1, '', '', 9, 3, 0, 4, 'YES');
  CheckColumns(Catalog, Schema, 'PEOPLE', 'P_END_WORK', 11, '', 16, 16, 0, 0, 1, '', '', 9, 3, 0, 5, 'YES');
  CheckColumns(Catalog, Schema, 'PEOPLE', 'P_PICTURE', -4, '', 2147483647, 2147483647, 0, 0, 1, '', '', -4, 0, 2147483647, 6, 'YES');
  CheckColumns(Catalog, Schema, 'PEOPLE', 'P_RESUME', -1, '', 2147483647, 2147483647, 0, 0, 1, '', '', -1, 0, 2147483647, 7, 'YES');
  CheckColumns(Catalog, Schema, 'PEOPLE', 'P_REDUNDANT', -6, '', 1, 1, 0, 10, 1, '', '', -6, 0, 0, 8, 'YES');
  Check(not Resultset.Next, 'There should not be more columns');
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetTablePrivileges;
begin
  ResultSet := MD.GetTablePrivileges(Catalog, Schema, 'PEOPLE');
  PrintResultSet(ResultSet, False);
  while ResultSet.Next do
  begin
    CheckEquals(Catalog, Resultset.GetStringByName('TABLE_CAT'));
    CheckEquals(Schema, Resultset.GetStringByName('TABLE_SCHEM'));
    CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('TABLE_NAME')));
    CheckEquals(4, Resultset.FindColumn('GRANTOR'));
    CheckEquals(5, Resultset.FindColumn('GRANTEE'));
    CheckEquals(6, Resultset.FindColumn('PRIVILEGE'));
    CheckEquals(7, Resultset.FindColumn('IS_GRANTABLE'));
  end;
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetColumnPrivileges;
begin
  ResultSet := MD.GetColumnPrivileges(Catalog, Schema, 'people', '');
  PrintResultSet(ResultSet, False);
  while ResultSet.Next do
  begin
    CheckEquals(Catalog, Resultset.GetStringByName('TABLE_CAT'));
    CheckEquals(Schema, Resultset.GetStringByName('TABLE_SCHEM'));
    CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('TABLE_NAME')));
    CheckEquals(4, Resultset.FindColumn('COLUMN_NAME'));
    CheckEquals(5, Resultset.FindColumn('GRANTOR'));
    CheckEquals(6, Resultset.FindColumn('GRANTEE'));
    CheckEquals(7, Resultset.FindColumn('PRIVILEGE'));
    CheckEquals(8, Resultset.FindColumn('IS_GRANTABLE'));
  end;
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetBestRowIdentifier;
begin
  ResultSet := MD.GetBestRowIdentifier(Catalog, Schema, 'PEOPLE', 0, True);
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be 1 bestRow Identifier in the people table');
  CheckEquals(1, Resultset.FindColumn('SCOPE'));
  CheckEquals(UpperCase('p_id'), UpperCase(Resultset.GetStringByName('COLUMN_NAME')));
  CheckEquals(3, Resultset.FindColumn('DATA_TYPE'));
  CheckEquals(4, Resultset.FindColumn('TYPE_NAME'));
  CheckEquals(5, Resultset.FindColumn('COLUMN_SIZE'));
  CheckEquals(6, Resultset.FindColumn('BUFFER_LENGTH'));
  CheckEquals(7, Resultset.FindColumn('DECIMAL_DIGITS'));
  CheckEquals(8, Resultset.FindColumn('PSEUDO_COLUMN'));
  CheckEquals(False, ResultSet.Next, 'There should not be more than 1 bestRow Identifier in the people table');
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetVersionColumns;
begin
  ResultSet := MD.GetVersionColumns(Catalog, Schema, 'PEOPLE');
  PrintResultSet(ResultSet, False);
  CheckEquals(1, Resultset.FindColumn('SCOPE'));
  CheckEquals(2, Resultset.FindColumn('COLUMN_NAME'));
  CheckEquals(3, Resultset.FindColumn('DATA_TYPE'));
  CheckEquals(4, Resultset.FindColumn('TYPE_NAME'));
  CheckEquals(5, Resultset.FindColumn('COLUMN_SIZE'));
  CheckEquals(6, Resultset.FindColumn('BUFFER_LENGTH'));
  CheckEquals(7, Resultset.FindColumn('DECIMAL_DIGITS'));
  CheckEquals(8, Resultset.FindColumn('PSEUDO_COLUMN'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetPrimaryKeys;
begin
  ResultSet := MD.GetPrimaryKeys(Catalog, Schema, 'PEOPLE');
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be primary key in the people table');
  CheckEquals(Catalog, Resultset.GetStringByName('TABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('TABLE_SCHEM'));
  CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('TABLE_NAME')));
  CheckEquals('P_ID', UpperCase(Resultset.GetStringByName('COLUMN_NAME')));
  CheckEquals(1, Resultset.GetShortByName('KEY_SEQ'));
  CheckEquals(6, Resultset.FindColumn('PK_NAME'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetImportedKeys;
begin
  ResultSet := MD.GetImportedKeys(Catalog, Schema, 'CARGO');
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be an imported key in the people table');
  CheckEquals(Catalog, Resultset.GetStringByName('PKTABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('PKTABLE_SCHEM'));
  CheckEquals('DEPARTMENT', UpperCase(Resultset.GetStringByName('PKTABLE_NAME')));
  CheckEquals('DEP_ID', UpperCase(Resultset.GetStringByName('PKCOLUMN_NAME')));
  CheckEquals(Catalog, Resultset.GetStringByName('FKTABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('FKTABLE_SCHEM'));
  CheckEquals('CARGO', UpperCase(Resultset.GetStringByName('FKTABLE_NAME')));
  CheckEquals('C_DEP_ID', UpperCase(Resultset.GetStringByName('FKCOLUMN_NAME')));
  CheckEquals(1, Resultset.GetShortByName('KEY_SEQ'));
  CheckEquals(1, Resultset.GetShortByName('UPDATE_RULE'));
  CheckEquals(1, Resultset.GetShortByName('DELETE_RULE'));
  CheckEquals(12, Resultset.FindColumn('FK_NAME'));
  CheckEquals(13, Resultset.FindColumn('PK_NAME'));
  CheckEquals(14, Resultset.FindColumn('DEFERRABILITY'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetExportedKeys;
  procedure CheckExportedKey(PKTable, PKColumn, FKTable, FKColumn: string; KeySeq, UpdateRule, DeleteRule: Integer);
  begin
    CheckEquals(Catalog, Resultset.GetStringByName('PKTABLE_CAT'));
    CheckEquals(Schema, Resultset.GetStringByName('PKTABLE_SCHEM'));
    CheckEquals(PKTable, UpperCase(Resultset.GetStringByName('PKTABLE_NAME')));
    CheckEquals(PKColumn, UpperCase(Resultset.GetStringByName('PKCOLUMN_NAME')));
    CheckEquals(Catalog, Resultset.GetStringByName('FKTABLE_CAT'));
    CheckEquals(Schema, Resultset.GetStringByName('FKTABLE_SCHEM'));
    CheckEquals(FKTable, UpperCase(Resultset.GetStringByName('FKTABLE_NAME')));
    CheckEquals(FKColumn, UpperCase(Resultset.GetStringByName('FKCOLUMN_NAME')));
    CheckEquals(KeySeq, Resultset.GetShortByName('KEY_SEQ'));
    CheckEquals(UpdateRule, Resultset.GetShortByName('UPDATE_RULE'));
    CheckEquals(DeleteRule, Resultset.GetShortByName('DELETE_RULE'));
    CheckEquals(12, Resultset.FindColumn('FK_NAME'));
    CheckEquals(13, Resultset.FindColumn('PK_NAME'));
    CheckEquals(14, Resultset.FindColumn('DEFERRABILITY'));
  end;
begin
  ResultSet := MD.GetExportedKeys(Catalog, Schema, 'DEPARTMENT');
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be more imported key in the department table');
  CheckExportedKey('DEPARTMENT', 'DEP_ID', 'CARGO', 'C_DEP_ID', 1, 1, 1);
  CheckEquals(True, ResultSet.Next, 'There should be more imported key in the department table');
  CheckExportedKey('DEPARTMENT', 'DEP_ID', 'EQUIPMENT2', 'DEP_ID', 1, 1, 1);
  //CheckEquals(True, ResultSet.Next, 'There should be more imported key in the department table');
  //CheckExportedKey('DEPARTMENT', 'DEP_ID', 'PEOPLE', 'P_DEP_ID', 1, 1, 1);
  //CheckEquals(False, ResultSet.Next, 'There should not be more imported key in the department table');
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetCrossReference;
begin
  ResultSet := MD.GetCrossReference(Catalog, Schema, 'DEPARTMENT', Catalog, Schema, 'people');
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be a cross reference between people and department table');
  CheckEquals(Catalog, Resultset.GetStringByName('PKTABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('PKTABLE_SCHEM'));
  CheckEquals('DEPARTMENT', UpperCase(Resultset.GetStringByName('PKTABLE_NAME')));
  CheckEquals('DEP_ID', UpperCase(Resultset.GetStringByName('PKCOLUMN_NAME')));
  CheckEquals(Catalog, Resultset.GetStringByName('FKTABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('FKTABLE_SCHEM'));
  CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('FKTABLE_NAME')));
  CheckEquals('P_DEP_ID', UpperCase(Resultset.GetStringByName('FKCOLUMN_NAME')));
  CheckEquals(1, Resultset.GetShortByName('KEY_SEQ'));
  CheckEquals(1, Resultset.GetShortByName('UPDATE_RULE'));
  CheckEquals(1, Resultset.GetShortByName('DELETE_RULE'));
  CheckEquals(12, Resultset.FindColumn('FK_NAME'));
  CheckEquals(13, Resultset.FindColumn('PK_NAME'));
  CheckEquals(14, Resultset.FindColumn('DEFERRABILITY'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetIndexInfo;
begin
  ResultSet := MD.GetIndexInfo(Catalog, Schema, 'PEOPLE', False, False);
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be an index on the people table');
  CheckEquals(Catalog, Resultset.GetStringByName('TABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('TABLE_SCHEM'));
  CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('TABLE_NAME')));
  CheckEquals(4, Resultset.FindColumn('NON_UNIQUE'));
  CheckEquals(5, Resultset.FindColumn('INDEX_QUALIFIER'));
  CheckEquals(6, Resultset.FindColumn('INDEX_NAME'));
  CheckEquals(7, Resultset.FindColumn('TYPE'));
  CheckEquals(8, Resultset.FindColumn('ORDINAL_POSITION'));
  CheckEquals(9, Resultset.FindColumn('COLUMN_NAME'));
  CheckEquals(10, Resultset.FindColumn('ASC_OR_DESC'));
  CheckEquals(11, Resultset.FindColumn('CARDINALITY'));
  CheckEquals(12, Resultset.FindColumn('PAGES'));
  CheckEquals(13, Resultset.FindColumn('FILTER_CONDITION'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetProcedures;
begin
  ResultSet := MD.GetProcedures(Catalog, Schema, '%');
  PrintResultSet(ResultSet, False);
  CheckEquals(1, Resultset.FindColumn('PROCEDURE_CAT'));
  CheckEquals(2, Resultset.FindColumn('PROCEDURE_SCHEM'));
  CheckEquals(3, Resultset.FindColumn('PROCEDURE_NAME'));
  CheckEquals(4, Resultset.FindColumn('RESERVED1'));
  CheckEquals(5, Resultset.FindColumn('RESERVED2'));
  CheckEquals(6, Resultset.FindColumn('RESERVED3'));
  CheckEquals(7, Resultset.FindColumn('REMARKS'));
  CheckEquals(8, Resultset.FindColumn('PROCEDURE_TYPE'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetProcedureColumns;
begin
  ResultSet := MD.GetProcedureColumns(Catalog, Schema, '%', '');
  PrintResultSet(ResultSet, False);
  CheckEquals(1, Resultset.FindColumn('PROCEDURE_CAT'));
  CheckEquals(2, Resultset.FindColumn('PROCEDURE_SCHEM'));
  CheckEquals(3, Resultset.FindColumn('PROCEDURE_NAME'));
  CheckEquals(4, Resultset.FindColumn('COLUMN_NAME'));
  CheckEquals(5, Resultset.FindColumn('COLUMN_TYPE'));
  CheckEquals(6, Resultset.FindColumn('DATA_TYPE'));
  CheckEquals(7, Resultset.FindColumn('TYPE_NAME'));
  CheckEquals(8, Resultset.FindColumn('PRECISION'));
  CheckEquals(9, Resultset.FindColumn('LENGTH'));
  CheckEquals(10, Resultset.FindColumn('SCALE'));
  CheckEquals(11, Resultset.FindColumn('RADIX'));
  CheckEquals(12, Resultset.FindColumn('NULLABLE'));
  CheckEquals(13, Resultset.FindColumn('REMARKS'));
  ResultSet.Close;
end;

procedure TZASATestDbcMetadata.TestMetadataGetTypeInfo;
begin
  ResultSet := MD.GetTypeInfo;
  PrintResultSet(ResultSet, False);
  ResultSet.Close;
end;


function TZASATestDbcMetadata.GetSupportedProtocols: string;
begin
  Result := 'ASA7,ASA8,ASA9';
end;

initialization
  RegisterTest('dbc',TZASATestDbcMetadata.Suite);
end.
