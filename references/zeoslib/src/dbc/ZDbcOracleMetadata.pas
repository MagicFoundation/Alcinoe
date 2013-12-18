{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Oracle Database Connectivity Classes           }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcOracleMetadata;

interface

{$I ZDbc.inc}

uses
  Types, Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcMetadata,
  ZCompatibility, ZDbcOracleUtils, ZDbcConnection;

type

  // technobot 2008-06-28 - methods moved as is from TZOracleDatabaseMetadata:
  {** Implements Oracle Database Information. }
  TZOracleDatabaseInfo = class(TZAbstractDatabaseInfo)
//    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
//      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;
  public
    constructor Create(const Metadata: TZAbstractDatabaseMetadata);
    destructor Destroy; override;

    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
//    function GetDriverVersion: string; override; -> Same as parent
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
//    function GetServerVersion: string; -> Not implemented

    // capabilities (what it can/cannot do):
//    function AllProceduresAreCallable: Boolean; override; -> Not implemented
//    function AllTablesAreSelectable: Boolean; override; -> Not implemented
    function SupportsMixedCaseIdentifiers: Boolean; override;
    function SupportsMixedCaseQuotedIdentifiers: Boolean; override;
//    function SupportsAlterTableWithAddColumn: Boolean; override; -> Not implemented
//    function SupportsAlterTableWithDropColumn: Boolean; override; -> Not implemented
//    function SupportsColumnAliasing: Boolean; override; -> Not implemented
//    function SupportsConvert: Boolean; override; -> Not implemented
//    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
//      Boolean; override; -> Not implemented
//    function SupportsTableCorrelationNames: Boolean; override; -> Not implemented
//    function SupportsDifferentTableCorrelationNames: Boolean; override; -> Not implemented
    function SupportsExpressionsInOrderBy: Boolean; override;
    function SupportsOrderByUnrelated: Boolean; override;
    function SupportsGroupBy: Boolean; override;
    function SupportsGroupByUnrelated: Boolean; override;
    function SupportsGroupByBeyondSelect: Boolean; override;
//    function SupportsLikeEscapeClause: Boolean; override; -> Not implemented
//    function SupportsMultipleResultSets: Boolean; override; -> Not implemented
//    function SupportsMultipleTransactions: Boolean; override; -> Not implemented
//    function SupportsNonNullableColumns: Boolean; override; -> Not implemented
//    function SupportsMinimumSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsCoreSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsExtendedSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsANSI92EntryLevelSQL: Boolean; override; -> Not implemented
//    function SupportsANSI92IntermediateSQL: Boolean; override; -> Not implemented
//    function SupportsANSI92FullSQL: Boolean; override; -> Not implemented
    function SupportsIntegrityEnhancementFacility: Boolean; override;
//    function SupportsOuterJoins: Boolean; override; -> Not implemented
//    function SupportsFullOuterJoins: Boolean; override; -> Not implemented
//    function SupportsLimitedOuterJoins: Boolean; override; -> Not implemented
    function SupportsSchemasInDataManipulation: Boolean; override;
    function SupportsSchemasInProcedureCalls: Boolean; override;
    function SupportsSchemasInTableDefinitions: Boolean; override;
    function SupportsSchemasInIndexDefinitions: Boolean; override;
    function SupportsSchemasInPrivilegeDefinitions: Boolean; override;
    function SupportsCatalogsInDataManipulation: Boolean; override;
    function SupportsCatalogsInProcedureCalls: Boolean; override;
    function SupportsCatalogsInTableDefinitions: Boolean; override;
    function SupportsCatalogsInIndexDefinitions: Boolean; override;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean; override;
    function SupportsPositionedDelete: Boolean; override;
    function SupportsPositionedUpdate: Boolean; override;
    function SupportsSelectForUpdate: Boolean; override;
    function SupportsStoredProcedures: Boolean; override;
    function SupportsSubqueriesInComparisons: Boolean; override;
    function SupportsSubqueriesInExists: Boolean; override;
    function SupportsSubqueriesInIns: Boolean; override;
    function SupportsSubqueriesInQuantifieds: Boolean; override;
    function SupportsCorrelatedSubqueries: Boolean; override;
    function SupportsUnion: Boolean; override;
    function SupportsUnionAll: Boolean; override;
    function SupportsOpenCursorsAcrossCommit: Boolean; override;
    function SupportsOpenCursorsAcrossRollback: Boolean; override;
    function SupportsOpenStatementsAcrossCommit: Boolean; override;
    function SupportsOpenStatementsAcrossRollback: Boolean; override;
    function SupportsTransactions: Boolean; override;
    function SupportsTransactionIsolationLevel(Level: TZTransactIsolationLevel):
      Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
    function SupportsResultSetType(_Type: TZResultSetType): Boolean; override;
    function SupportsResultSetConcurrency(_Type: TZResultSetType;
      Concurrency: TZResultSetConcurrency): Boolean; override;
//    function SupportsBatchUpdates: Boolean; override; -> Not implemented

    // maxima:
    function GetMaxBinaryLiteralLength: Integer; override;
    function GetMaxCharLiteralLength: Integer; override;
    function GetMaxColumnNameLength: Integer; override;
    function GetMaxColumnsInGroupBy: Integer; override;
    function GetMaxColumnsInIndex: Integer; override;
    function GetMaxColumnsInOrderBy: Integer; override;
    function GetMaxColumnsInSelect: Integer; override;
    function GetMaxColumnsInTable: Integer; override;
    function GetMaxConnections: Integer; override;
    function GetMaxCursorNameLength: Integer; override;
    function GetMaxIndexLength: Integer; override;
    function GetMaxSchemaNameLength: Integer; override;
    function GetMaxProcedureNameLength: Integer; override;
    function GetMaxCatalogNameLength: Integer; override;
    function GetMaxRowSize: Integer; override;
    function GetMaxStatementLength: Integer; override;
    function GetMaxStatements: Integer; override;
    function GetMaxTableNameLength: Integer; override;
    function GetMaxTablesInSelect: Integer; override;
    function GetMaxUserNameLength: Integer; override;

    // policies (how are various data and operations handled):
//    function IsReadOnly: Boolean; override; -> Not implemented
//    function IsCatalogAtStart: Boolean; override; -> Not implemented
    function DoesMaxRowSizeIncludeBlobs: Boolean; override;
//    function NullsAreSortedHigh: Boolean; override; -> Not implemented
//    function NullsAreSortedLow: Boolean; override; -> Not implemented
//    function NullsAreSortedAtStart: Boolean; override; -> Not implemented
//    function NullsAreSortedAtEnd: Boolean; override; -> Not implemented
//    function NullPlusNonNullIsNull: Boolean; override; -> Not implemented
//    function UsesLocalFiles: Boolean; override; -> Not implemented
    function UsesLocalFilePerTable: Boolean; override;
    function StoresUpperCaseIdentifiers: Boolean; override;
    function StoresLowerCaseIdentifiers: Boolean; override;
    function StoresMixedCaseIdentifiers: Boolean; override;
    function StoresUpperCaseQuotedIdentifiers: Boolean; override;
    function StoresLowerCaseQuotedIdentifiers: Boolean; override;
    function StoresMixedCaseQuotedIdentifiers: Boolean; override;
    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; override;
    function DataDefinitionCausesTransactionCommit: Boolean; override;
    function DataDefinitionIgnoredInTransactions: Boolean; override;

    // interface details (terms, keywords, etc):
//    function GetIdentifierQuoteString: string; override; -> Not implemented
    function GetSchemaTerm: string; override;
    function GetProcedureTerm: string; override;
    function GetCatalogTerm: string; override;
    function GetCatalogSeparator: string; override;
    function GetSQLKeywords: string; override;
    function GetNumericFunctions: string; override;
    function GetStringFunctions: string; override;
    function GetSystemFunctions: string; override;
    function GetTimeDateFunctions: string; override;
    function GetSearchStringEscape: string; override;
    function GetExtraNameCharacters: string; override;
  end;

  {** Implements Oracle Database Metadata. }
  TZOracleDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    FDatabase: string;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-28

    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetSchemas: IZResultSet; override;
//    function UncachedGetCatalogs: IZResultSet; override; -> Not implemented
    function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; override;
    function UncachedGetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
//    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
//      const Table: string): IZResultSet; override;
//    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
//      const Table: string): IZResultSet; override;
//    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
//      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
//      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
//     function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
//      const SequenceNamePattern: string): IZResultSet; virtual; -> Not implemented
//    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
//      const ProcedureNamePattern: string): IZResultSet; override;
//    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
//      const ProcedureNamePattern: string; const ColumnNamePattern: string):
//      IZResultSet; override;
//    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
//      const Table: string): IZResultSet; override;
//    function UncachedGetTypeInfo: IZResultSet; override;
  public
    constructor Create(Connection: TZAbstractConnection; Url: string;
      Info: TStrings);
    destructor Destroy; override;
  end;

implementation

uses
  ZDbcUtils;

{ TZOracleDatabaseInfo }

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
}
constructor TZOracleDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata);
begin
  inherited;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOracleDatabaseInfo.Destroy;
begin
  inherited;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZOracleDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'Oracle';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZOracleDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZOracleDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for Oracle';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZOracleDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZOracleDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZOracleDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZOracleDatabaseInfo.GetSQLKeywords: string;
begin
  Result := 'ACCESS,ADD,ALTER,AUDIT,CLUSTER,COLUMN,COMMENT,COMPRESS,CONNECT,'
    + 'DATE,DROP,EXCLUSIVE,FILE,IDENTIFIED,IMMEDIATE,INCREMENT,INDEX,INITIAL,'
    + 'INTERSECT,LEVEL,LOCK,LONG,MAXEXTENTS,MINUS,MODE,NOAUDIT,NOCOMPRESS,'
    + 'NOWAIT,NUMBER,OFFLINE,ONLINE,PCTFREE,PRIOR';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZOracleDatabaseInfo.GetNumericFunctions: string;
begin
  Result := 'ABS,ACOS,ASIN,ATAN,ATAN2,CEILING,COS,EXP,FLOOR,LOG,LOG10,MOD,PI,'
    + 'POWER,ROUND,SIGN,SIN,SQRT,TAN,TRUNCATE';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZOracleDatabaseInfo.GetStringFunctions: string;
begin
  Result := 'ASCII,CHAR,CONCAT,LCASE,LENGTH,LTRIM,REPLACE,RTRIM,SOUNDEX,'
    + 'SUBSTRING,UCASE';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZOracleDatabaseInfo.GetSystemFunctions: string;
begin
  Result := 'USER';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZOracleDatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := 'CURDATE,CURTIME,DAYOFMONTH,HOUR,MINUTE,MONTH,NOW,SECOND,YEAR';
end;

{**
  Gets the string that can be used to escape wildcard characters.
  This is the string that can be used to escape '_' or '%' in
  the string pattern style catalog search parameters.

  <P>The '_' character represents any single character.
  <P>The '%' character represents any sequence of zero or
  more characters.

  @return the string used to escape wildcard characters
}
function TZOracleDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '//';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZOracleDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '$#';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := True;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := True;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZOracleDatabaseInfo.GetSchemaTerm: string;
begin
  Result := 'schema';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZOracleDatabaseInfo.GetProcedureTerm: string;
begin
  Result := 'procedure';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZOracleDatabaseInfo.GetCatalogTerm: string;
begin
  Result := '';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZOracleDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := False;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := False;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := True;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZOracleDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZOracleDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZOracleDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZOracleDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := False;
end;

//----------------------------------------------------------------------
// The following group of methods exposes various limitations
// based on the target database with the current driver.
// Unless otherwise specified, a result of zero means there is no
// limit, or the limit is not known.

{**
  How many hex characters can you have in an inline binary literal?
  @return max binary literal length in hex characters;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 1000;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 2000;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 30;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 32;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 1000;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := 0;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := 30;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := 30;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 2000;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := True;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 65535;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 30;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 30;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZOracleDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZOracleDatabaseInfo.SupportsTransactionIsolationLevel(
  Level: TZTransactIsolationLevel): Boolean;
begin
  Result := True;
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := True;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := True;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsResultSetType(
  _Type: TZResultSetType): Boolean;
begin
  Result := _Type = rtForwardOnly;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsResultSetConcurrency(
  _Type: TZResultSetType; Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := (_Type = rtForwardOnly) and (Concurrency = rcReadOnly);
end;


{ TZOracleDatabaseMetadata }


{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Url a database connection url string.
  @param Info an extra connection properties.
}
constructor TZOracleDatabaseMetadata.Create(
  Connection: TZAbstractConnection; Url: string; Info: TStrings);
var
  TempInfo: TStrings;
  HostName, UserName, Password: string;
  Port: Integer;
begin
  inherited Create(Connection, Url, Info);

  TempInfo := TStringList.Create;
  try
    ResolveDatabaseUrl(Url, Info, HostName, Port, FDatabase,
      UserName, Password, TempInfo);
  finally
    TempInfo.Free;
  end;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOracleDatabaseMetadata.Destroy;
begin
  inherited Destroy;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZOracleDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZOracleDatabaseInfo.Create(Self);
end;

{**
  Gets a description of tables available in a catalog.

  <P>Only table descriptions matching the catalog, schema, table
  name and type criteria are returned.  They are ordered by
  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.

  <P>Each table description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
 	<LI><B>REMARKS</B> String => explanatory comment on the table
   </OL>

  <P><B>Note:</B> Some databases may not return information for
  all tables.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param types a list of table types to include; null returns all types
  @return <code>ResultSet</code> - each row is a table description
  @see #getSearchStringEscape
}
function TZOracleDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  PartSQL, SQL: string;

  function IncludedType(const TypeName: string): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I := Low(Types) to High(Types) do
      Result := Result or (UpperCase(Types[I]) = TypeName);
    Result := Result or (Length(Types) = 0);
  end;

begin
    if IncludedType('TABLE') then
    begin
      SQL := 'SELECT NULL AS TABLE_CAT, OWNER AS TABLE_SCHEM, TABLE_NAME,'
        + ' ''TABLE'' AS TABLE_TYPE, NULL AS REMARKS FROM SYS.ALL_TABLES'
        + ' WHERE OWNER LIKE ''' + ToLikeString(SchemaPattern) + ''' AND TABLE_NAME LIKE '''
        + ToLikeString(TableNamePattern) + '''';
    end else
      SQL := '';

    if IncludedType('SYNONYM') then
    begin
      PartSQL := 'SELECT NULL AS TABLE_CAT, OWNER AS TABLE_SCHEM,'
        + ' SYNONYM_NAME AS TABLE_NAME, ''SYNONYM'' AS TABLE_TYPE,'
        + ' NULL AS REMARKS FROM SYS.ALL_SYNONYMS WHERE OWNER LIKE '''
        + ToLikeString(SchemaPattern) + ''' AND SYNONYM_NAME LIKE ''' + ToLikeString(TableNamePattern) + '''';

      if SQL <> '' then
        SQL := SQL + ' UNION ';
      SQL := SQL + PartSQL;
    end;

    if IncludedType('VIEW') then
    begin
      PartSQL := 'SELECT NULL AS TABLE_CAT, OWNER AS TABLE_SCHEM,'
        + ' VIEW_NAME AS TABLE_NAME, ''VIEW'' AS TABLE_TYPE,'
        + ' NULL AS REMARKS FROM SYS.ALL_VIEWS WHERE OWNER LIKE '''
        + ToLikeString(SchemaPattern) + ''' AND VIEW_NAME LIKE ''' + ToLikeString(TableNamePattern) + '''';

      if SQL <> '' then
        SQL := SQL + ' UNION ';
      SQL := SQL + PartSQL;
    end;

    if IncludedType('SEQUENCE') then
    begin
      PartSQL := 'SELECT NULL AS TABLE_CAT, SEQUENCE_OWNER AS TABLE_SCHEM,'
        + ' SEQUENCE_NAME AS TABLE_NAME, ''SEQUENCE'' AS TABLE_TYPE,'
        + ' NULL AS REMARKS FROM SYS.ALL_SEQUENCES WHERE SEQUENCE_OWNER LIKE '''
        + ToLikeString(SchemaPattern) + ''' AND SEQUENCE_NAME LIKE ''' + ToLikeString(TableNamePattern) + '''';

      if SQL <> '' then
        SQL := SQL + ' UNION ';
      SQL := SQL + PartSQL;
    end;

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(TableColumnsDynArray));
end;

{**
  Gets the schema names available in this database.  The results
  are ordered by schema name.

  <P>The schema column is:
   <OL>
 	<LI><B>TABLE_SCHEM</B> String => schema name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  schema name
}
function TZOracleDatabaseMetadata.UncachedGetSchemas: IZResultSet;
begin
    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(
        'SELECT USERNAME AS TABLE_SCHEM FROM SYS.ALL_USERS'),
      ConstructVirtualResultSet(SchemaColumnsDynArray));
end;

{**
  Gets the table types available in this database.  The results
  are ordered by table type.

  <P>The table type is:
   <OL>
 	<LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  table type
}
function TZOracleDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TableTypeCount = 4;
  Types: array [1..TableTypeCount] of string = (
    'TABLE', 'SYNONYM', 'VIEW', 'SEQUENCE'
  );
var
  I: Integer;
begin
    Result := ConstructVirtualResultSet(TableTypeColumnsDynArray);
    for I := 1 to TableTypeCount do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(1, Types[I]);
      Result.InsertRow;
    end;
end;

{**
  Gets a description of table columns available in
  the specified catalog.

  <P>Only column descriptions matching the catalog, schema, table
  and column name criteria are returned.  They are ordered by
  TABLE_SCHEM, TABLE_NAME and ORDINAL_POSITION.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name,
   for a UDT the type name is fully qualified
 	<LI><B>COLUMN_SIZE</B> int => column size.  For char or date
 	    types this is the maximum number of characters, for numeric or
 	    decimal types this is precision.
 	<LI><B>BUFFER_LENGTH</B> is not used.
 	<LI><B>DECIMAL_DIGITS</B> int => the number of fractional digits
 	<LI><B>NUM_PREC_RADIX</B> int => Radix (typically either 10 or 2)
 	<LI><B>NULLABLE</B> int => is NULL allowed?
       <UL>
       <LI> columnNoNulls - might not allow NULL values
       <LI> columnNullable - definitely allows NULL values
       <LI> columnNullableUnknown - nullability unknown
       </UL>
 	<LI><B>REMARKS</B> String => comment describing column (may be null)
  	<LI><B>COLUMN_DEF</B> String => default value (may be null)
 	<LI><B>SQL_DATA_TYPE</B> int => unused
 	<LI><B>SQL_DATETIME_SUB</B> int => unused
 	<LI><B>CHAR_OCTET_LENGTH</B> int => for char types the
        maximum number of bytes in the column
 	<LI><B>ORDINAL_POSITION</B> int	=> index of column in table
       (starting at 1)
 	<LI><B>IS_NULLABLE</B> String => "NO" means column definitely
       does not allow NULL values; "YES" means the column might
       allow NULL values.  An empty string means nobody knows.
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column description
  @see #getSearchStringEscape
}
function TZOracleDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  SQL: string;
begin
    Result := ConstructVirtualResultSet(TableColColumnsDynArray);

    SQL := 'SELECT NULL, OWNER, TABLE_NAME, COLUMN_NAME, NULL, DATA_TYPE,'
      + ' DATA_LENGTH, NULL, DATA_PRECISION, DATA_SCALE, NULLABLE, NULL,'
      + ' DATA_DEFAULT, NULL, NULL, NULL, COLUMN_ID, NULLABLE'
      + ' FROM SYS.ALL_TAB_COLUMNS'
      + ' WHERE OWNER LIKE ''' + ToLikeString(SchemaPattern) + ''' AND TABLE_NAME LIKE '''
      + ToLikeString(TableNamePattern) + ''' AND COLUMN_NAME LIKE '''
      + ToLikeString(ColumnNamePattern) + '''';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(1);
        Result.UpdateString(2, GetString(2));
        Result.UpdateString(3, GetString(3));
        Result.UpdateString(4, GetString(4));
        Result.UpdateInt(5, Ord(ConvertOracleTypeToSQLType(
          GetString(6), GetInt(9), GetInt(10))));
        Result.UpdateString(6, GetString(6));
        Result.UpdateInt(7, GetInt(7));
        Result.UpdateNull(8);
        Result.UpdateInt(9, GetInt(9));
        Result.UpdateInt(10, GetInt(10));

        if UpperCase(GetString(11)) = 'N' then
        begin
          Result.UpdateInt(11, Ord(ntNoNulls));
          Result.UpdateString(18, 'NO');
        end
        else
        begin
          Result.UpdateInt(11, Ord(ntNullable));
          Result.UpdateString(18, 'YES');
        end;

        Result.UpdateNull(12);
        Result.UpdateString(13, GetString(13));
        Result.UpdateNull(14);
        Result.UpdateNull(15);
        Result.UpdateNull(16);
        Result.UpdateInt(17, GetInt(17));

        Result.UpdateNull(19);   //AUTO_INCREMENT
        Result.UpdateBoolean(20, //CASE_SENSITIVE
          GetIdentifierConvertor.IsCaseSensitive(GetString(4)));
        Result.UpdateBoolean(21, True);  //SEARCHABLE
        Result.UpdateBoolean(22, True);  //WRITABLE
        Result.UpdateBoolean(23, True);  //DEFINITELYWRITABLE
        Result.UpdateBoolean(24, False); //READONLY

        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of the access rights for a table's columns.

  <P>Only privileges matching the column name criteria are
  returned.  They are ordered by COLUMN_NAME and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>GRANTOR</B> => grantor of access (may be null)
 	<LI><B>GRANTEE</B> String => grantee of access
 	<LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
 	<LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column privilege description
  @see #getSearchStringEscape
}
function TZOracleDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  SQL: string;
begin
    SQL := 'SELECT NULL AS TABLE_CAT, TABLE_SCHEMA AS TABLE_SCHEM, TABLE_NAME,'
      + ' COLUMN_NAME, GRANTOR, GRANTEE, PRIVILEGE, GRANTABLE AS IS_GRANTABLE'
      + ' FROM SYS.ALL_COL_PRIVS WHERE';
    if Schema <> '' then
      SQL := SQL + ' TABLE_SCHEMA=''' + Schema + ''' AND';
    if Table <> '' then
      SQL := SQL + ' TABLE_NAME=''' + Table + ''' AND';
    SQL := SQL + ' COLUMN_NAME LIKE ''' + ToLikeString(ColumnNamePattern) + '''';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(TableColPrivColumnsDynArray));
end;

{**
  Gets a description of the access rights for each table available
  in a catalog. Note that a table privilege applies to one or
  more columns in the table. It would be wrong to assume that
  this priviledge applies to all columns (this may be true for
  some systems but is not true for all.)

  <P>Only privileges matching the schema and table name
  criteria are returned.  They are ordered by TABLE_SCHEM,
  TABLE_NAME, and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>GRANTOR</B> => grantor of access (may be null)
 	<LI><B>GRANTEE</B> String => grantee of access
 	<LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
 	<LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @return <code>ResultSet</code> - each row is a table privilege description
  @see #getSearchStringEscape
}
function TZOracleDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  SQL: string;
begin
    SQL := 'SELECT NULL AS TABLE_CAT, TABLE_SCHEMA AS TABLE_SCHEM, TABLE_NAME,'
      + ' GRANTOR, GRANTEE, PRIVILEGE, GRANTABLE AS IS_GRANTABLE'
      + ' FROM SYS.ALL_TAB_PRIVS WHERE TABLE_SCHEMA LIKE ''' + ToLikeString(SchemaPattern)
      + ''' AND TABLE_NAME LIKE ''' + ToLikeString(TableNamePattern) + '''';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(TablePrivColumnsDynArray));
end;

{**
  Gets a description of a table's primary key columns.  They
  are ordered by COLUMN_NAME.

  <P>Each primary key column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>KEY_SEQ</B> short => sequence number within primary key
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @exception SQLException if a database access error occurs
}
function TZOracleDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  SQL: string;
begin
    SQL := 'SELECT NULL AS TABLE_CAT, A.OWNER AS TABLE_SCHEM, A.TABLE_NAME,'
      + ' B.COLUMN_NAME, B.COLUMN_POSITION AS KEY_SEQ, A.INDEX_NAME AS PK_NAME'
      + ' FROM ALL_INDEXES A, ALL_IND_COLUMNS B'
      + ' WHERE A.OWNER=B.INDEX_OWNER AND A.INDEX_NAME=B.INDEX_NAME'
      + ' AND A.TABLE_OWNER=B.TABLE_OWNER AND A.TABLE_NAME=B.TABLE_NAME'
      + ' AND A.UNIQUENESS=''UNIQUE'' AND A.GENERATED=''Y'''
      + ' AND A.INDEX_NAME LIKE ''SYS_%''';
    if Schema <> '' then
      SQL := SQL + ' AND A.OWNER=''' + Schema + '''';
    if Table <> '' then
       SQL := SQL + ' AND A.TABLE_OWNER=''' + Table + '''';
    SQL := SQL + ' ORDER BY A.INDEX_NAME, B.COLUMN_POSITION';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(PrimaryKeyColumnsDynArray));
end;

{**
  Gets a description of a table's indices and statistics. They are
  ordered by NON_UNIQUE, TYPE, INDEX_NAME, and ORDINAL_POSITION.

  <P>Each index column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>NON_UNIQUE</B> Boolean => Can index values be non-unique?
       false when TYPE is tableIndexStatistic
 	<LI><B>INDEX_QUALIFIER</B> String => index catalog (may be null);
       null when TYPE is tableIndexStatistic
 	<LI><B>INDEX_NAME</B> String => index name; null when TYPE is
       tableIndexStatistic
 	<LI><B>TYPE</B> short => index type:
       <UL>
       <LI> tableIndexStatistic - this identifies table statistics that are
            returned in conjuction with a table's index descriptions
       <LI> tableIndexClustered - this is a clustered index
       <LI> tableIndexHashed - this is a hashed index
       <LI> tableIndexOther - this is some other style of index
       </UL>
 	<LI><B>ORDINAL_POSITION</B> short => column sequence number
       within index; zero when TYPE is tableIndexStatistic
 	<LI><B>COLUMN_NAME</B> String => column name; null when TYPE is
       tableIndexStatistic
 	<LI><B>ASC_OR_DESC</B> String => column sort sequence, "A" => ascending,
       "D" => descending, may be null if sort sequence is not supported;
       null when TYPE is tableIndexStatistic
 	<LI><B>CARDINALITY</B> int => When TYPE is tableIndexStatistic, then
       this is the number of rows in the table; otherwise, it is the
       number of unique values in the index.
 	<LI><B>PAGES</B> int => When TYPE is  tableIndexStatisic then
       this is the number of pages used for the table, otherwise it
       is the number of pages used for the current index.
 	<LI><B>FILTER_CONDITION</B> String => Filter condition, if any.
       (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param unique when true, return only indices for unique values;
      when false, return indices regardless of whether unique or not
  @param approximate when true, result is allowed to reflect approximate
      or out of data values; when false, results are requested to be
      accurate
  @return <code>ResultSet</code> - each row is an index column description
}
function TZOracleDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  SQL: string;
begin
    Result := ConstructVirtualResultSet(IndexInfoColumnsDynArray);

    SQL := 'SELECT NULL, A.OWNER, A.TABLE_NAME, A.UNIQUENESS, NULL,'
      + ' A.INDEX_NAME, 3, B.COLUMN_POSITION, B.COLUMN_NAME, B.DESCEND,'
      + ' 0, 0, NULL FROM ALL_INDEXES A, ALL_IND_COLUMNS B'
      + ' WHERE A.OWNER=B.INDEX_OWNER AND A.INDEX_NAME=B.INDEX_NAME'
      + ' AND A.TABLE_OWNER=B.TABLE_OWNER AND A.TABLE_NAME=B.TABLE_NAME';
    if Schema <> '' then
      SQL := SQL + ' AND A.TABLE_OWNER=''' + Schema + '''';
    if Table <> '' then
      SQL := SQL + ' AND A.TABLE_NAME=''' + Table + '''';
    if Unique then
      SQL := SQL + ' AND A.UNIQUENESS=''UNIQUE''';
    SQL := SQL + ' ORDER BY A.UNIQUENESS DESC, A.INDEX_NAME, B.COLUMN_POSITION';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;

        Result.UpdateNull(1);
        Result.UpdateString(2, GetString(2));
        Result.UpdateString(3, GetString(3));
        Result.UpdateBoolean(4,
          UpperCase(GetString(4)) <> 'UNIQUE');
        Result.UpdateNull(5);
        Result.UpdateString(6, GetString(6));
        Result.UpdateInt(7, GetInt(7));
        Result.UpdateInt(8, GetInt(8));
        Result.UpdateString(9, GetString(9));
        if GetString(10) = 'ASC' then
          Result.UpdateString(10, 'A')
        else Result.UpdateString(10, 'D');
        Result.UpdateInt(11, GetInt(11));
        Result.UpdateInt(12, GetInt(12));
        Result.UpdateNull(13);

        Result.InsertRow;
      end;
      Close;
    end;
end;

end.
