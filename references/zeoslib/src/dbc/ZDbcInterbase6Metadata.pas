{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
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

unit ZDbcInterbase6Metadata;

interface

{$I ZDbc.inc}

uses
  Types, Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcMetadata, ZCompatibility,
  ZDbcConnection, ZDbcInterbase6;

type

  // technobot 2008-06-25 - methods moved as is from TZInterbase6DatabaseMetadata:
  {** Implements Interbase6 Database Information. }
  TZInterbase6DatabaseInfo = class(TZAbstractDatabaseInfo)
  private
    FServerVersion: string;
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
    function GetServerVersion: string;

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

  {** Implements Interbase6 Database Metadata. }
  TZInterbase6DatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    function StripEscape(const Pattern: string): string;
    function HasNoWildcards(const Pattern: string): boolean;
    function GetPrivilege(Privilege: string): string;
    function ConstructNameCondition(Pattern: string; Column: string): string;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-25

    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
//    function UncachedGetSchemas: IZResultSet; override; -> Not implemented
//    function UncachedGetCatalogs: IZResultSet; override; -> Not Implemented
    function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; override;
    function UncachedGetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
//    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
//      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
//      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
    function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): IZResultSet; override;
    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; override;
    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
  public
    constructor Create(Connection: TZAbstractConnection; Url: string; Info: TStrings);
    destructor Destroy; override;
  end;

implementation

uses ZMessages, ZDbcInterbase6Utils;

{ TZInterbase6DatabaseInfo }

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
}
constructor TZInterbase6DatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata);
begin
  inherited;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZInterbase6DatabaseInfo.Destroy;
begin
  inherited;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZInterbase6DatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'Interbase/Firebird';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZInterbase6DatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '6.0+';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZInterbase6DatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for Interbase and Firebird';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZInterbase6DatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZInterbase6DatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets the version of the server.
  @returns the version of the server.
}
function TZInterbase6DatabaseInfo.GetServerVersion: string;
var
  FIBConnection: IZInterbase6Connection;
begin
  if FServerVersion = '' then
  begin
    FIBConnection := Metadata.GetConnection as IZInterbase6Connection;
    FServerVersion := GetVersion(FIBConnection.GetPlainDriver,
      FIBConnection.GetDBHandle);
  end;
  Result := FServerVersion;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZInterbase6DatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZInterbase6DatabaseInfo.GetSQLKeywords: string;
begin
  Result := 'ACTIVE,AFTER,ASCENDING,BASE_NAME,BEFORE,BLOB,' +
    'CACHE,CHECK_POINT_LENGTH,COMPUTED,CONDITIONAL,CONTAINING,' +
    'CSTRING,DATABASE,RDB$DB_KEY,DEBUG,DESCENDING,DO,ENTRY_POINT,' +
    'EXIT,FILE,FILTER,FUNCTION,GDSCODE,GENERATOR,GEN_ID,' +
    'GROUP_COMMIT_WAIT_TIME,IF,INACTIVE,INPUT_TYPE,INDEX,' +
    'LOGFILE,LOG_BUFFER_SIZE,MANUAL,MAXIMUM_SEGMENT,MERGE, MESSAGE,' +
    'MODULE_NAME,NCHAR,NUM_LOG_BUFFERS,OUTPUT_TYPE,OVERFLOW,PAGE,' +
    'PAGES,PAGE_SIZE,PARAMETER,PASSWORD,PLAN,POST_EVENT,PROTECTED,' +
    'RAW_PARTITIONS,RESERV,RESERVING,RETAIN,RETURNING_VALUES,RETURNS,' +
    'SEGMENT,SHADOW,SHARED,SINGULAR,SNAPSHOT,SORT,STABILITY,STARTS,' +
    'STARTING,STATISTICS,SUB_TYPE,SUSPEND,TRIGGER,VARIABLE,RECORD_VERSION,' +
    'WAIT,WHILE,WORK';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZInterbase6DatabaseInfo.GetNumericFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZInterbase6DatabaseInfo.GetStringFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZInterbase6DatabaseInfo.GetSystemFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZInterbase6DatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := '';
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
function TZInterbase6DatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '\';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZInterbase6DatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '$';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := False;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZInterbase6DatabaseInfo.GetSchemaTerm: string;
begin
  Result := '';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZInterbase6DatabaseInfo.GetProcedureTerm: string;
begin
  Result := 'PROCEDURE';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZInterbase6DatabaseInfo.GetCatalogTerm: string;
begin
  Result := '';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZInterbase6DatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := True;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := True;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := True;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := True;
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
function TZInterbase6DatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 1024;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 31;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 32767;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 32767;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := 31;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 198;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 27;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 32664;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := False;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 640;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 31;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 31;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZInterbase6DatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiSerializable;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZInterbase6DatabaseInfo.SupportsTransactionIsolationLevel(
  Level: TZTransactIsolationLevel): Boolean;
begin
  case Level of
    tiRepeatableRead, tiReadCommitted, tiSerializable: Result := True;
    tiReadUncommitted: Result := False;
    tiNone: Result := False; //MAY BE FIX IT
    else
      Result := False;
  end;    
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := False;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := True;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsResultSetType(
  _Type: TZResultSetType): Boolean;
begin
  Result := _Type = rtScrollInsensitive;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsResultSetConcurrency(
  _Type: TZResultSetType; Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := (_Type = rtScrollInsensitive) and (Concurrency = rcReadOnly);
end;


{ TZInterbase6DatabaseMetadata }


{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Url a database connection url string.
  @param Info an extra connection properties.
}
constructor TZInterbase6DatabaseMetadata.Create(Connection: TZAbstractConnection;
  Url: string; Info: TStrings);
begin
  inherited Create(Connection, Url, Info);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZInterbase6DatabaseMetadata.Destroy;
begin
  inherited Destroy;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZInterbase6DatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZInterbase6DatabaseInfo.Create(Self);
end;

{**
  Gets a description of the stored procedures available in a
  catalog.

  <P>Only procedure descriptions matching the schema and
  procedure name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM, and PROCEDURE_NAME.

  <P>Each procedure description has the the following columns:
   <OL>
 	<LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
 	<LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
 	<LI><B>PROCEDURE_NAME</B> String => procedure name
   <LI> reserved for future use
   <LI> reserved for future use
   <LI> reserved for future use
 	<LI><B>REMARKS</B> String => explanatory comment on the procedure
 	<LI><B>PROCEDURE_TYPE</B> short => kind of procedure:
       <UL>
       <LI> procedureResultUnknown - May return a result
       <LI> procedureNoResult - Does not return a result
       <LI> procedureReturnsResult - Returns a result
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @return <code>ResultSet</code> - each row is a procedure description
  @see #getSearchStringEscape
}
function TZInterbase6DatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  SQL: string;
  LProcedureNamePattern: string;
begin
    Result := ConstructVirtualResultSet(ProceduresColumnsDynArray);

    LProcedureNamePattern := ConstructNameCondition(ProcedureNamePattern,
      'RDB$PROCEDURE_NAME');
    SQL := 'SELECT RDB$PROCEDURE_NAME, RDB$PROCEDURE_OUTPUTS,'
      + ' RDB$DESCRIPTION FROM RDB$PROCEDURES';
    if LProcedureNamePattern <> '' then
      SQL := SQL + ' WHERE ' + LProcedureNamePattern;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(1);
        Result.UpdateNull(2);
        Result.UpdateString(3, GetString(1)); //RDB$PROCEDURE_NAME
        Result.UpdateNull(4);
        Result.UpdateNull(5);
        Result.UpdateNull(6);
        Result.UpdateString(7, GetString(3)); //RDB$DESCRIPTION
        if IsNull(2) then //RDB$PROCEDURE_OUTPUTS
          Result.UpdateInt(8, Ord(prtNoResult))
        else Result.UpdateInt(8, Ord(prtReturnsResult));
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of a catalog's stored procedure parameters
  and result columns.

  <P>Only descriptions matching the schema, procedure and
  parameter name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
  if any, is first. Next are the parameter descriptions in call
  order. The column descriptions follow in column number order.

  <P>Each row in the <code>ResultSet</code> is a parameter description or
  column description with the following fields:
   <OL>
 	<LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
 	<LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
 	<LI><B>PROCEDURE_NAME</B> String => procedure name
 	<LI><B>COLUMN_NAME</B> String => column/parameter name
 	<LI><B>COLUMN_TYPE</B> Short => kind of column/parameter:
       <UL>
       <LI> procedureColumnUnknown - nobody knows
       <LI> procedureColumnIn - IN parameter
       <LI> procedureColumnInOut - INOUT parameter
       <LI> procedureColumnOut - OUT parameter
       <LI> procedureColumnReturn - procedure return value
       <LI> procedureColumnResult - result column in <code>ResultSet</code>
       </UL>
   <LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => SQL type name, for a UDT type the
   type name is fully qualified
 	<LI><B>PRECISION</B> int => precision
 	<LI><B>LENGTH</B> int => length in bytes of data
 	<LI><B>SCALE</B> short => scale
 	<LI><B>RADIX</B> short => radix
 	<LI><B>NULLABLE</B> short => can it contain NULL?
       <UL>
       <LI> procedureNoNulls - does not allow NULL values
       <LI> procedureNullable - allows NULL values
       <LI> procedureNullableUnknown - nullability unknown
       </UL>
 	<LI><B>REMARKS</B> String => comment describing parameter/column
   </OL>

  <P><B>Note:</B> Some databases may not return the column
  descriptions for a procedure. Additional columns beyond
  REMARKS can be defined by the database.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row describes a stored procedure parameter or
       column
  @see #getSearchStringEscape
}
function TZInterbase6DatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  SQL, Where: string;
  LProcedureNamePattern, LColumnNamePattern: string;
  TypeName, SubTypeName: Integer;
  ColumnIndexes : Array[1..8] of integer;
begin
    Result := ConstructVirtualResultSet(ProceduresColColumnsDynArray);

    LProcedureNamePattern := ConstructNameCondition(ProcedureNamePattern, 
      'P.RDB$PROCEDURE_NAME');
    LColumnNamePattern := ConstructNameCondition(ColumnNamePattern,
      'PP.RDB$PARAMETER_NAME');

  if (StrPos(PChar(GetDatabaseInfo.GetServerVersion), 'Interbase 5') <> nil)
     or (StrPos(PChar(GetDatabaseInfo.GetServerVersion), 'V5.') <> nil) then
    begin
      SQL := ' SELECT P.RDB$PROCEDURE_NAME, PP.RDB$PARAMETER_NAME,'
        + ' PP.RDB$PARAMETER_TYPE, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE,'
        + ' F.RDB$FIELD_SCALE, F.RDB$FIELD_LENGTH, F.RDB$NULL_FLAG,'
        + ' PP.RDB$DESCRIPTION, F.RDB$FIELD_SCALE as RDB$FIELD_PRECISION,'
        + ' F.RDB$NULL_FLAG FROM RDB$PROCEDURES P'
        + ' JOIN RDB$PROCEDURE_PARAMETERS PP ON P.RDB$PROCEDURE_NAME'
        + '=PP.RDB$PROCEDURE_NAME JOIN RDB$FIELDS F ON PP.RDB$FIELD_SOURCE'
        + '=F.RDB$FIELD_NAME ';

      Where := LProcedureNamePattern;
      if LColumnNamePattern <> '' then
      begin
        if Where = '' then
          Where := LColumnNamePattern
        else
          Where := Where + ' AND ' + LColumnNamePattern;
      end;
      if Where <> '' then
        Where := ' WHERE ' + Where;

      SQL := SQL + Where + ' ORDER BY  P.RDB$PROCEDURE_NAME,'
        + ' PP.RDB$PARAMETER_TYPE desc, PP.RDB$PARAMETER_NUMBER';
    end
    else
    begin
      SQL := ' SELECT P.RDB$PROCEDURE_NAME, PP.RDB$PARAMETER_NAME,'
        + ' PP.RDB$PARAMETER_TYPE, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE,'
        + ' F.RDB$FIELD_SCALE, F.RDB$FIELD_LENGTH, F.RDB$NULL_FLAG,'
        + ' PP.RDB$DESCRIPTION, F.RDB$FIELD_PRECISION, F.RDB$NULL_FLAG '
        + ' FROM RDB$PROCEDURES P JOIN RDB$PROCEDURE_PARAMETERS PP ON'
        + ' P.RDB$PROCEDURE_NAME = PP.RDB$PROCEDURE_NAME '
        + ' JOIN RDB$FIELDS F ON PP.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME ';

      Where := LProcedureNamePattern;
      if LColumnNamePattern <> '' then
      begin
        if Where = '' then
          Where := LColumnNamePattern
        else
          Where := Where + ' AND ' + LColumnNamePattern;
      end;
      if Where <> '' then
        Where := ' WHERE ' + Where;

      SQL := SQL + Where + ' ORDER BY  P.RDB$PROCEDURE_NAME,'
        + ' PP.RDB$PARAMETER_TYPE desc, PP.RDB$PARAMETER_NUMBER';
    end;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      ColumnIndexes[1] := FindColumn('RDB$PROCEDURE_NAME');
      ColumnIndexes[2] := FindColumn('RDB$PARAMETER_NAME');
      ColumnIndexes[3] := FindColumn('RDB$PARAMETER_TYPE');
      ColumnIndexes[4] := FindColumn('RDB$FIELD_TYPE');
      ColumnIndexes[5] := FindColumn('RDB$FIELD_SUB_TYPE');
      ColumnIndexes[6] := FindColumn('RDB$FIELD_PRECISION');
      ColumnIndexes[7] := FindColumn('RDB$FIELD_SCALE');
      ColumnIndexes[8] := FindColumn('RDB$NULL_FLAG');
      while Next do
      begin
        TypeName := GetInt(ColumnIndexes[4]);
        SubTypeName := GetInt(ColumnIndexes[5]);

        Result.MoveToInsertRow;
        Result.UpdateNull(1);    //PROCEDURE_CAT
        Result.UpdateNull(2);    //PROCEDURE_SCHEM
        Result.UpdateString(3, GetString(ColumnIndexes[1]));    //TABLE_NAME
        Result.UpdateString(4, GetString(ColumnIndexes[2]));    //COLUMN_NAME
        case GetInt(ColumnIndexes[3]) of
          0: Result.UpdateInt(5, 1);//ptInput
          1: Result.UpdateInt(5, 4);//ptResult
        else
            Result.UpdateInt(5, 0); //ptUnknown
        end;

        Result.UpdateInt(6,
          Ord(ConvertInterbase6ToSqlType(TypeName, SubTypeName))); //DATA_TYPE
        Result.UpdateString(7,GetString(ColumnIndexes[4]));    //TYPE_NAME
        Result.UpdateInt(10, GetInt(ColumnIndexes[6]));
        Result.UpdateNull(9);    //BUFFER_LENGTH
        Result.UpdateInt(10, GetInt(ColumnIndexes[7]));
        Result.UpdateInt(11, 10);
        Result.UpdateInt(12, GetInt(ColumnIndexes[8]));
        Result.UpdateString(12, GetString(ColumnIndexes[6]));
        Result.InsertRow;
      end;
      Close;
    end;
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

function TZInterbase6DatabaseMetadata.UncachedGetTables(const Catalog: string; 
  const SchemaPattern: string; const TableNamePattern: string; 
  const Types: TStringDynArray): IZResultSet; 
var 
  SQL, TableType: string; 
  LTableNamePattern: string; 
  BLR: IZBlob; 
  I, SystemFlag, ViewContext: Integer; 
begin 
    Result := ConstructVirtualResultSet(TableColumnsDynArray); 

    LTableNamePattern := ConstructNameCondition(TableNamePattern, 
      'a.RDB$RELATION_NAME'); 
    SQL := 'SELECT DISTINCT a.RDB$RELATION_NAME, a.RDB$SYSTEM_FLAG, ' 
      + ' a.RDB$VIEW_SOURCE, a.RDB$DESCRIPTION FROM RDB$RELATIONS a'; 

    if LTableNamePattern <> '' then 
      SQL := SQL + ' WHERE ' + LTableNamePattern; 

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do 
    begin    
       while Next do 
      begin 
        SystemFlag := GetInt(2); //RDB$SYSTEM_FLAG

        if SystemFlag = 0 then 
        begin 
          if IsNull(3) then //RDB$VIEW_SOURCE
            TableType := 'TABLE' 
          else 
            TableType := 'VIEW'; 
        end
        else
          TableType := 'SYSTEM TABLE'; 

        if Length(Types) = 0 then 
        begin 
          Result.MoveToInsertRow; 
          Result.UpdateNull(1); 
          Result.UpdateNull(2); 
          Result.UpdateString(3, GetString(1)); //RDB$RELATION_NAME
          Result.UpdateString(4, TableType); 
          Result.UpdateString(5, Copy(GetString(4),1,255)); //RDB$DESCRIPTION
          Result.InsertRow; 
        end 
        else
        begin
          for I := 0 to High(Types) do 
          begin 
            if Types[I] = TableType then 
            begin 
              Result.MoveToInsertRow; 
              Result.UpdateNull(1); 
              Result.UpdateNull(2); 
              Result.UpdateString(3, GetString(1)); //RDB$RELATION_NAME
              Result.UpdateString(4, TableType); 
              Result.UpdateString(5, Copy(GetString(4),1,255)); //RDB$DESCRIPTION 
              Result.InsertRow; 
            end; 
          end; 
        end; 
            
      end; 
      Close; 
    end; 
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
function TZInterbase6DatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TablesTypes: array [0..2] of string = ('TABLE', 'VIEW', 'SYSTEM TABLE');
var
  I: Integer;
begin
    Result := ConstructVirtualResultSet(TableTypeColumnsDynArray);
    for I := 0 to 2 do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(1, TablesTypes[I]);
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

function TZInterbase6DatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  SQL, Where, ColumnName, DefaultValue: string;
  TypeName, SubTypeName, FieldScale: integer;
  LTableNamePattern, LColumnNamePattern: string;
  ColumnIndexes : Array[1..14] of integer;
begin
    Result := ConstructVirtualResultSet(TableColColumnsDynArray);

    LTableNamePattern := ConstructNameCondition(TableNamePattern, 
      'a.RDB$RELATION_NAME');
    LColumnNamePattern := ConstructNameCondition(ColumnNamePattern,
      'a.RDB$FIELD_NAME');

  if (StrPos(PChar(GetDatabaseInfo.GetServerVersion), 'Interbase 5') <> nil)
     or (StrPos(PChar(GetDatabaseInfo.GetServerVersion), 'V5.') <> nil) then
    begin
      SQL := 'SELECT a.RDB$RELATION_NAME, a.RDB$FIELD_NAME, a.RDB$FIELD_POSITION,'
        + ' a.RDB$NULL_FLAG, b. RDB$FIELD_LENGTH, b.RDB$FIELD_SCALE,'
        + ' c.RDB$TYPE_NAME, b.RDB$FIELD_TYPE, b.RDB$FIELD_SUB_TYPE,'
        + ' b.RDB$DESCRIPTION, b.RDB$CHARACTER_LENGTH, b.RDB$FIELD_SCALE'
        + ' as RDB$FIELD_PRECISION, a.RDB$DEFAULT_SOURCE, b.RDB$DEFAULT_SOURCE'
        + ' as RDB$DEFAULT_SOURCE_DOMAIN, b.RDB$COMPUTED_SOURCE as RDB$COMPUTED_SOURCE'
        + ' FROM RDB$RELATION_FIELDS a'
        + ' JOIN RDB$FIELDS b ON (b.RDB$FIELD_NAME = a.RDB$FIELD_SOURCE)'
        + ' LEFT JOIN RDB$TYPES c ON b.RDB$FIELD_TYPE = c.RDB$TYPE'
        + ' and c.RDB$FIELD_NAME = ''RDB$FIELD_TYPE''';

      Where := LTableNamePattern;
      if LColumnNamePattern <> '' then
      begin
        if Where = '' then
          Where := LColumnNamePattern
        else
          Where := Where + ' AND ' + LColumnNamePattern;
      end;
      if Where <> '' then
        Where := ' WHERE ' + Where;

      SQL := SQL + Where + ' ORDER BY a.RDB$RELATION_NAME, a.RDB$FIELD_POSITION';
    end
    else
    begin
      SQL := ' SELECT a.RDB$RELATION_NAME, a.RDB$FIELD_NAME, a.RDB$FIELD_POSITION,'
        + ' a.RDB$NULL_FLAG, a.RDB$DEFAULT_VALUE, b. RDB$FIELD_LENGTH,'
        + ' b.RDB$FIELD_SCALE, c.RDB$TYPE_NAME, b.RDB$FIELD_TYPE,'
        + ' b.RDB$FIELD_SUB_TYPE, b.RDB$DESCRIPTION, b.RDB$CHARACTER_LENGTH,'
        + ' b.RDB$FIELD_PRECISION, a.RDB$DEFAULT_SOURCE, b.RDB$DEFAULT_SOURCE'
        + ' as RDB$DEFAULT_SOURCE_DOMAIN,b.RDB$COMPUTED_SOURCE as RDB$COMPUTED_SOURCE'
        + ' FROM RDB$RELATION_FIELDS a'
        + ' JOIN RDB$FIELDS b ON (b.RDB$FIELD_NAME = a.RDB$FIELD_SOURCE)'
        + ' LEFT JOIN RDB$TYPES c ON (b.RDB$FIELD_TYPE = c.RDB$TYPE'
        + ' and c.RDB$FIELD_NAME = ''RDB$FIELD_TYPE'')';

      Where := LTableNamePattern;
      if LColumnNamePattern <> '' then
      begin
        if Where = '' then
          Where := LColumnNamePattern
        else
          Where := Where + ' AND ' + LColumnNamePattern;
      end;
      if Where <> '' then
        Where := ' WHERE ' + Where;

      SQL := SQL + Where + ' ORDER BY a.RDB$RELATION_NAME, a.RDB$FIELD_POSITION';
    end;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      ColumnIndexes[1] := FindColumn('RDB$FIELD_TYPE');
      ColumnIndexes[2] := FindColumn('RDB$FIELD_SUB_TYPE');
      ColumnIndexes[3] := FindColumn('RDB$FIELD_SCALE');
      ColumnIndexes[4] := FindColumn('RDB$FIELD_NAME');
      ColumnIndexes[5] := FindColumn('RDB$DEFAULT_SOURCE');
      ColumnIndexes[6] := FindColumn('RDB$DEFAULT_SOURCE_DOMAIN');
      ColumnIndexes[7] := FindColumn('RDB$RELATION_NAME');
      ColumnIndexes[8] := FindColumn('RDB$TYPE_NAME');
      ColumnIndexes[9] := FindColumn('RDB$FIELD_PRECISION');
      ColumnIndexes[10] := FindColumn('RDB$FIELD_LENGTH');
      ColumnIndexes[11] := FindColumn('RDB$NULL_FLAG');
      ColumnIndexes[12] := FindColumn('RDB$DESCRIPTION');
      ColumnIndexes[13] := FindColumn('RDB$FIELD_POSITION');
      ColumnIndexes[14] := FindColumn('RDB$COMPUTED_SOURCE');
      while Next do
      begin
        TypeName := GetInt(ColumnIndexes[1]);
        SubTypeName := GetInt(ColumnIndexes[2]);
        FieldScale := GetInt(ColumnIndexes[3]);
        ColumnName := GetString(ColumnIndexes[4]);


        if (GetString(ColumnIndexes[14]) <> '') then  //AVZ -- not isNull(14) was not working correcly here could be ' ' - subselect
        begin //Computed by Source  & Sub Selects  //AVZ
          if ((TypeName = 16) and (FieldScale < 0)) then SubTypeName := 1; // Fix for 0 subtype which removes decimals
        end;

        DefaultValue := GetString(ColumnIndexes[5]);
        if DefaultValue = '' then
          DefaultValue := GetString(ColumnIndexes[6]);
        if StartsWith(Trim(UpperCase(DefaultValue)), 'DEFAULT') then
        begin
          DefaultValue := Trim(StringReplace(DefaultValue, 'DEFAULT ', '',
            [rfIgnoreCase]));
        end;

        IF (UpperCase(DefaultValue)= '''NOW''') or (UpperCase(DefaultValue)= '"NOW"')then
          case TypeName of
          12: DefaultValue := 'CURRENT_DATE';
          13: DefaultValue := 'CURRENT_TIME';
          35: DefaultValue := 'CURRENT_TIMESTAMP';
          else begin end;
          end;

        Result.MoveToInsertRow;
        Result.UpdateNull(1);    //TABLE_CAT
        Result.UpdateNull(2);    //TABLE_SCHEM
        Result.UpdateString(3, GetString(ColumnIndexes[7]));    //TABLE_NAME
        Result.UpdateString(4, ColumnName);    //COLUMN_NAME
        Result.UpdateInt(5, Ord(ConvertInterbase6ToSqlType(TypeName, SubTypeName))); //DATA_TYPE
        // TYPE_NAME
        case TypeName of
          7  : Result.UpdateString(6, 'SMALLINT');
          8  : Result.UpdateString(6, 'INTEGER' );
          16 :
            begin
              if (SubTypeName = 0) then
                Result.UpdateString(6, GetString(ColumnIndexes[8]));
              if (SubTypeName = 1) then
                Result.UpdateString(6, 'NUMERIC');
              if (SubTypeName = 2) then
                Result.UpdateString(6, 'DECIMAL');
            end;
          37 : Result.UpdateString(6, 'VARCHAR'); // Instead of VARYING
        else
            Result.UpdateString(6, GetString(ColumnIndexes[8]));
        end;
        // COLUMN_SIZE.
        case TypeName of
          7, 8 : Result.UpdateInt(7, 0);
          16   : Result.UpdateInt(7, GetInt(ColumnIndexes[9]));
        else
            Result.UpdateInt(7, GetInt(ColumnIndexes[10]));
        end;

        Result.UpdateNull(8);    //BUFFER_LENGTH

        if FieldScale < 0 then
          Result.UpdateInt(9, -1 * FieldScale)    //DECIMAL_DIGITS
        else
          Result.UpdateInt(9, 0); //DECIMAL_DIGITS

        Result.UpdateInt(10, 10);   //NUM_PREC_RADIX

        if GetInt(ColumnIndexes[11]) <> 0 then
          Result.UpdateInt(11, Ord(ntNoNulls))   //NULLABLE
        else
          Result.UpdateInt(11, Ord(ntNullable));

        Result.UpdateString(12, Copy(GetString(ColumnIndexes[12]),1,255));   //REMARKS
        Result.UpdateString(13, DefaultValue);   //COLUMN_DEF
        Result.UpdateNull(14);   //SQL_DATA_TYPE
        Result.UpdateNull(15);   //SQL_DATETIME_SUB
        Result.UpdateInt(16,
          GetInt(7));   //CHAR_OCTET_LENGTH
        Result.UpdateInt(17, GetInt(ColumnIndexes[13]) + 1);   //ORDINAL_POSITION

        if IsNull(ColumnIndexes[11]) then
          Result.UpdateString(18, 'YES')   //IS_NULLABLE
        else
          Result.UpdateString(18, 'NO'); //IS_NULLABLE

        Result.UpdateNull(19); //AUTO_INCREMENT

        if CompareStr(ColumnName, UpperCase(ColumnName)) = 0 then
          Result.UpdateBoolean(20, False) //CASE_SENSITIVE
        else
          Result.UpdateBoolean(20, True); //CASE_SENSITIVE

        Result.UpdateBoolean(21, True); //SEARCHABLE
        if isNull(ColumnIndexes[14]) then
          begin
            Result.UpdateBoolean(22, True); //WRITABLE
            Result.UpdateBoolean(23, True); //DEFINITELYWRITABLE
            Result.UpdateBoolean(24, False); //READONLY
          end
        else
          begin
            Result.UpdateBoolean(22, False); //WRITABLE
            Result.UpdateBoolean(23, False); //DEFINITELYWRITABLE
            Result.UpdateBoolean(24, True); //READONLY
          end;
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
function TZInterbase6DatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  SQL: string;
  TableName, FieldName, Privilege: string;
  Grantor, Grantee, Grantable: string;
  LColumnNamePattern, LTable: string;
begin
    Result := ConstructVirtualResultSet(TableColPrivColumnsDynArray);

    LTable := ConstructNameCondition(AddEscapeCharToWildcards(Table), 'a.RDB$RELATION_NAME');// Modified by cipto 6/12/2007 2:26:18 PM
    LColumnNamePattern := ConstructNameCondition(ColumnNamePattern,
      'a.RDB$FIELD_NAME');

    SQL := 'SELECT a.RDB$USER, a.RDB$GRANTOR, a.RDB$PRIVILEGE,'
      + ' a.RDB$GRANT_OPTION, a.RDB$RELATION_NAME, a.RDB$FIELD_NAME '
      + ' FROM RDB$USER_PRIVILEGES a, RDB$TYPES b '
      + ' WHERE a.RDB$OBJECT_TYPE = b.RDB$TYPE AND ';
    if LTable <> '' then
      SQL := SQL + LTable + ' AND ';
    if LColumnNamePattern <> '' then
      SQL := SQL + LColumnNamePattern + ' AND ';
    SQL := SQL + ' b.RDB$TYPE_NAME IN (''RELATION'', ''VIEW'','
      + ' ''COMPUTED_FIELD'', ''FIELD'' ) AND b.RDB$FIELD_NAME'
      + '=''RDB$OBJECT_TYPE'' ORDER BY a.RDB$FIELD_NAME, a.RDB$PRIVILEGE  ' ;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        TableName := GetString(5); //RDB$RELATION_NAME
        FieldName := GetString(6); //RDB$FIELD_NAME
        Privilege := GetPrivilege(GetString(3)); //RDB$PRIVILEGE
        Grantor := GetString(2); //RDB$GRANTOR
        Grantee := GetString(1); //RDB$USER

        if Grantor = Grantee then
          Grantable := 'YES'
        else
          Grantable := 'NO';
        if FieldName = '' then
        begin
          SQL := 'SELECT RDB$FIELD_NAME FROM RDB$RELATION_FIELDS '
            + ' WHERE RDB$RELATION_NAME = ''' + TableName + ''' AND '
            + ' RDB$FIELD_NAME = ''' + LColumnNamePattern + ''' AND ';
          with GetConnection.CreateStatement.ExecuteQuery(SQL) do
          begin
            while Next do
            begin
              Result.MoveToInsertRow;
              Result.UpdateNull(1);
              Result.UpdateNull(2);
              Result.UpdateString(3, TableName);
              Result.UpdateString(4, GetString(1));
              Result.UpdateString(5, Grantor);
              Result.UpdateString(6, Grantee);
              Result.UpdateString(7, Privilege);
              Result.UpdateString(8, Grantable);
              Result.InsertRow;
            end;
            Close;
          end;
        end
        else
        begin
          Result.MoveToInsertRow;
          Result.UpdateNull(1);
          Result.UpdateNull(2);
          Result.UpdateString(3, TableName);
          Result.UpdateString(4, FieldName);
          Result.UpdateString(5, Grantor);
          Result.UpdateString(6, Grantee);
          Result.UpdateString(7, Privilege);
          Result.UpdateString(8, Grantable);
          Result.InsertRow;
        end;
      end;
      Close;
    end;
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
function TZInterbase6DatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  SQL: string;
  TableName, Privilege, Grantor: string;
  Grantee, Grantable: string;
  LTableNamePattern: string;
begin
    Result := ConstructVirtualResultSet(TablePrivColumnsDynArray);

    LTableNamePattern := ConstructNameCondition(TableNamePattern, 'a.RDB$RELATION_NAME');

    SQL := 'SELECT a.RDB$USER, a.RDB$GRANTOR, a.RDB$PRIVILEGE,'
      + ' a.RDB$GRANT_OPTION, a.RDB$RELATION_NAME FROM RDB$USER_PRIVILEGES a,'
      + ' RDB$TYPES b WHERE a.RDB$OBJECT_TYPE = b.RDB$TYPE AND '
      + ' b.RDB$TYPE_NAME IN (''RELATION'', ''VIEW'', ''COMPUTED_FIELD'','
      + ' ''FIELD'' ) AND a.RDB$FIELD_NAME IS NULL ';
    if LTableNamePattern <> '' then
      SQL := SQL + ' AND ' + LTableNamePattern;
    SQL := SQL + ' ORDER BY a.RDB$RELATION_NAME, a.RDB$PRIVILEGE';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        TableName := GetString(5); //RDB$RELATION_NAME
        Privilege := GetPrivilege(GetString(3)); //RDB$PRIVILEGE
        Grantor := GetString(2); //RDB$GRANTOR
        Grantee := GetString(1); //RDB$USER

        if Grantor = Grantee then
          Grantable := 'YES'
        else
          Grantable := 'NO';

        Result.MoveToInsertRow;
        Result.UpdateNull(1);
        Result.UpdateNull(2);
        Result.UpdateString(3, TableName);
        Result.UpdateString(4, Grantor);
        Result.UpdateString(5, Grantee);
        Result.UpdateString(6, Privilege);
        Result.UpdateString(7, Grantable);
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of a table's columns that are automatically
  updated when any value in a row is updated.  They are
  unordered.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>SCOPE</B> short => is not used
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name
 	<LI><B>COLUMN_SIZE</B> int => precision
 	<LI><B>BUFFER_LENGTH</B> int => length of column value in bytes
 	<LI><B>DECIMAL_DIGITS</B> short	 => scale
 	<LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> versionColumnUnknown - may or may not be pseudo column
       <LI> versionColumnNotPseudo - is NOT a pseudo column
       <LI> versionColumnPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a column description
  @exception SQLException if a database access error occurs
}
function TZInterbase6DatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(TableColVerColumnsDynArray);

    Result.MoveToInsertRow;
    Result.UpdateNull(1);
    Result.UpdateString(2, 'ctid');
    //  Result.UpdateInt(3, GetSQLType('tid')); //FIX IT
    Result.UpdateString(4, 'tid');
    Result.UpdateNull(5);
    Result.UpdateNull(6);
    Result.UpdateNull(7);
    Result.UpdateInt(4, Ord(vcPseudo));
    Result.InsertRow;
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
function TZInterbase6DatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  SQL: string;
  LTable: string;
begin
    LTable := ConstructNameCondition(AddEscapeCharToWildcards(Table), 'a.RDB$RELATION_NAME'); // Modified by cipto 6/12/2007 2:03:20 PM
    SQL := ' SELECT null as TABLE_CAT, null as TABLE_SCHEM,'
      + ' a.RDB$RELATION_NAME as TABLE_NAME, b.RDB$FIELD_NAME as COLUMN_NAME,'
      + ' b.RDB$FIELD_POSITION+1 as KEY_SEQ, a.RDB$INDEX_NAME as PK_NAME'
      + ' FROM RDB$RELATION_CONSTRAINTS a JOIN RDB$INDEX_SEGMENTS b ON'
      + ' (a.RDB$INDEX_NAME = b.RDB$INDEX_NAME)'
      + ' WHERE  RDB$CONSTRAINT_TYPE = ''PRIMARY KEY''';
    if LTable <> '' then
      SQL := SQL + ' AND ' + LTable;
    SQL := SQL + ' ORDER BY a.RDB$RELATION_NAME, b.RDB$FIELD_NAME';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(PrimaryKeyColumnsDynArray));
end;

{**
  Gets a description of the primary key columns that are
  referenced by a table's foreign key columns (the primary keys
  imported by a table).  They are ordered by PKTABLE_CAT,
  PKTABLE_SCHEM, PKTABLE_NAME, and KEY_SEQ.

  <P>Each primary key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog
       being imported (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema
       being imported (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
       being imported
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
       being imported
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @see #getExportedKeys
}
function TZInterbase6DatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  SQL: string;
  LTable: string;
begin
    Result := ConstructVirtualResultSet(ImportedKeyColumnsDynArray);

    LTable := ConstructNameCondition(AddEscapeCharToWildcards(Table), 'RELC_FOR.RDB$RELATION_NAME'); // Modified by cipto 6/11/2007 4:53:02 PM 
    SQL := 'SELECT RELC_PRIM.RDB$RELATION_NAME, '    // 1 prim.RDB$ key table name
      + ' IS_PRIM.RDB$FIELD_NAME, '         // 2 prim.RDB$ key column name
      + ' RELC_FOR.RDB$RELATION_NAME, '     // 3 foreign key table name
      + ' IS_FOR.RDB$FIELD_NAME, '          // 4 foreign key column name
      + ' IS_FOR.RDB$FIELD_POSITION, '      // 5 key sequence
      + ' REFC_PRIM.RDB$UPDATE_RULE, '      // 6
      + ' REFC_PRIM.RDB$DELETE_RULE, '      // 7
      + ' RELC_FOR.RDB$CONSTRAINT_NAME, '   // 8 foreign key constraint name
      + ' RELC_PRIM.RDB$CONSTRAINT_NAME '   // 9 primary key constraint name
      + ' FROM RDB$RELATION_CONSTRAINTS RELC_FOR, RDB$REF_CONSTRAINTS REFC_FOR, '
      + ' RDB$RELATION_CONSTRAINTS RELC_PRIM, RDB$REF_CONSTRAINTS REFC_PRIM, '
      + ' RDB$INDEX_SEGMENTS IS_PRIM,  RDB$INDEX_SEGMENTS IS_FOR '
      + ' WHERE RELC_FOR.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND ';
     if LTable <> '' then
       SQL := SQL + LTable + ' AND ';
     SQL := SQL + ' RELC_FOR.RDB$CONSTRAINT_NAME=REFC_FOR.RDB$CONSTRAINT_NAME'
       + ' and REFC_FOR.RDB$CONST_NAME_UQ = RELC_PRIM.RDB$CONSTRAINT_NAME and '
       + ' RELC_PRIM.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' and ' // useful check, anyay
       + ' RELC_PRIM.RDB$INDEX_NAME = IS_PRIM.RDB$INDEX_NAME and '
       + ' IS_FOR.RDB$INDEX_NAME = RELC_FOR.RDB$INDEX_NAME   and '
       + ' IS_PRIM.RDB$FIELD_POSITION = IS_FOR.RDB$FIELD_POSITION  and '
       + ' REFC_PRIM.RDB$CONSTRAINT_NAME = RELC_FOR.RDB$CONSTRAINT_NAME '
       + ' ORDER BY RELC_PRIM.RDB$RELATION_NAME, IS_FOR.RDB$FIELD_POSITION ';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(1); //PKTABLE_CAT
        Result.UpdateNull(2); //PKTABLE_SCHEM
        Result.UpdateString(3, GetString(1)); //PKTABLE_NAME
        Result.UpdateString(4, GetString(2)); //PKCOLUMN_NAME
        Result.UpdateNull(5); //FKTABLE_CAT
        Result.UpdateNull(6); //FKTABLE_SCHEM
        Result.UpdateString(7, GetString(3)); //FKTABLE_NAME
        Result.UpdateString(8, GetString(4)); //FKCOLUMN_NAME
        Result.UpdateInt(9, GetInt(5) + 1); //KEY_SEQ

        if GetString(6) = 'RESTRICT' then  //UPDATE_RULE
          Result.UpdateInt(10, Ord(ikRestrict))
        else if GetString(6) = 'NO ACTION' then
          Result.UpdateInt(10, Ord(ikNoAction))
        else if GetString(6) = 'SET DEFAULT' then
          Result.UpdateInt(10, Ord(ikSetDefault))
        else if GetString(6) = 'CASCADE' then
          Result.UpdateInt(10, Ord(ikCascade))
        else if GetString(6) = 'SET NULL' then
          Result.UpdateInt(10, Ord(ikSetNull));

        if GetString(7) = 'RESTRICT' then //DELETE_RULE
          Result.UpdateInt(11, Ord(ikRestrict))
        else if GetString(7) = 'NO ACTION' then
          Result.UpdateInt(11, Ord(ikNoAction))
        else if GetString(7) = 'SET DEFAULT' then
          Result.UpdateInt(11, Ord(ikSetDefault))
        else if GetString(7) = 'CASCADE' then
          Result.UpdateInt(11, Ord(ikCascade))
        else if GetString(7) = 'SET NULL' then
          Result.UpdateInt(11, Ord(ikSetNull));

        Result.UpdateString(12, GetString(8)); //FK_NAME
        Result.UpdateString(13, GetString(9)); //PK_NAME
        Result.UpdateNull(14); //DEFERABILITY
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of the foreign key columns that reference a
  table's primary key columns (the foreign keys exported by a
  table).  They are ordered by FKTABLE_CAT, FKTABLE_SCHEM,
  FKTABLE_NAME, and KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZInterbase6DatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  SQL: string;
  LTable: string;
begin
    Result := ConstructVirtualResultSet(ExportedKeyColumnsDynArray);

    LTable := ConstructNameCondition(AddEscapeCharToWildcards(Table), 'RC_PRIM.RDB$RELATION_NAME'); // Modified by cipto 6/11/2007 4:54:02 PM
    SQL := ' SELECT RC_PRIM.RDB$RELATION_NAME, ' // prim.RDB$ key Table name
      + ' IS_PRIM.RDB$FIELD_NAME, '       // prim.RDB$ key column name
      + ' RC_FOR.RDB$RELATION_NAME, '     // foreign key Table name
      + ' IS_FOR.RDB$FIELD_NAME, '        // foreign key column name
      + ' IS_FOR.RDB$FIELD_POSITION, '    // key sequence
      + ' REFC_PRIM.RDB$UPDATE_RULE, '    // if update or delete rule is null, interpret as RESTRICT
      + ' REFC_PRIM.RDB$DELETE_RULE, '
      + ' RC_FOR.RDB$CONSTRAINT_NAME, '   // foreign key constraint name
      + ' RC_PRIM.RDB$CONSTRAINT_NAME '  // primary key constraint name
      + ' FROM RDB$RELATION_CONSTRAINTS RC_FOR, RDB$REF_CONSTRAINTS REFC_FOR, '
      + ' RDB$RELATION_CONSTRAINTS RC_PRIM, RDB$REF_CONSTRAINTS REFC_PRIM, '
      + ' RDB$INDEX_SEGMENTS IS_PRIM, RDB$INDEX_SEGMENTS IS_FOR '
      + ' WHERE RC_PRIM.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' and ';
    if LTable <> '' then
      SQL := SQL + LTable + ' AND ';
    SQL := SQL + ' REFC_FOR.RDB$CONST_NAME_UQ = RC_PRIM.RDB$CONSTRAINT_NAME'
      + ' and RC_FOR.RDB$CONSTRAINT_NAME = REFC_FOR.RDB$CONSTRAINT_NAME and '
      + ' RC_FOR.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' and '// useful check, anyay
      + ' RC_PRIM.RDB$INDEX_NAME = IS_PRIM.RDB$INDEX_NAME and '
      + ' IS_FOR.RDB$INDEX_NAME = RC_FOR.RDB$INDEX_NAME   and '
      + ' IS_PRIM.RDB$FIELD_POSITION = IS_FOR.RDB$FIELD_POSITION  and '
      + ' REFC_PRIM.RDB$CONSTRAINT_NAME = RC_FOR.RDB$CONSTRAINT_NAME '
      + ' ORDER BY RC_FOR.RDB$RELATION_NAME, IS_FOR.RDB$FIELD_POSITION ';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(1); //PKTABLE_CAT
        Result.UpdateNull(2); //PKTABLE_SCHEM
        Result.UpdateString(3, GetString(1)); //PKTABLE_NAME
        Result.UpdateString(4, GetString(2)); //PKCOLUMN_NAME
        Result.UpdateNull(5); //FKTABLE_CAT
        Result.UpdateNull(6); //FKTABLE_SCHEM'
        Result.UpdateString(7, GetString(3)); //FKTABLE_NAME
        Result.UpdateString(8, GetString(4)); //FKCOLUMN_NAME
        Result.UpdateInt(9, GetInt(5) + 1); //KEY_SEQ

        if GetString(6) = 'RESTRICT' then //UPDATE_RULE
          Result.UpdateInt(10, Ord(ikRestrict))
        else if GetString(6) = 'NO ACTION' then
          Result.UpdateInt(10, Ord(ikNoAction))
        else if GetString(6) = 'SET DEFAULT' then
          Result.UpdateInt(10, Ord(ikSetDefault))
        else if GetString(6) = 'CASCADE' then
          Result.UpdateInt(10, Ord(ikCascade))
        else if GetString(6) = 'SET NULL' then
          Result.UpdateInt(10, Ord(ikSetNull));

        if GetString(7) = 'RESTRICT' then //DELETE_RULE
          Result.UpdateInt(11, Ord(ikRestrict))
        else if GetString(7) = 'NO ACTION' then
          Result.UpdateInt(11, Ord(ikNoAction))
        else if GetString(7) = 'SET DEFAULT' then
          Result.UpdateInt(11, Ord(ikSetDefault))
        else if GetString(7) = 'CASCADE' then
          Result.UpdateInt(11, Ord(ikCascade))
        else if GetString(7) = 'SET NULL' then
          Result.UpdateInt(11, Ord(ikSetNull));

        Result.UpdateString(12, GetString(8)); //FK_NAME
        Result.UpdateString(13, GetString(9)); //PK_NAME
        Result.UpdateNull(14); //DEFERABILITY
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of all the standard SQL types supported by
  this database. They are ordered by DATA_TYPE and then by how
  closely the data type maps to the corresponding JDBC SQL type.

  <P>Each type description has the following columns:
   <OL>
 	<LI><B>TYPE_NAME</B> String => Type name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>PRECISION</B> int => maximum precision
 	<LI><B>LITERAL_PREFIX</B> String => prefix used to quote a literal
       (may be null)
 	<LI><B>LITERAL_SUFFIX</B> String => suffix used to quote a literal
        (may be null)
 	<LI><B>CREATE_PARAMS</B> String => parameters used in creating
       the type (may be null)
 	<LI><B>NULLABLE</B> short => can you use NULL for this type?
       <UL>
       <LI> typeNoNulls - does not allow NULL values
       <LI> typeNullable - allows NULL values
       <LI> typeNullableUnknown - nullability unknown
       </UL>
 	<LI><B>CASE_SENSITIVE</B> boolean=> is it case sensitive?
 	<LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
 	<LI><B>UNSIGNED_ATTRIBUTE</B> boolean => is it unsigned?
 	<LI><B>FIXED_PREC_SCALE</B> boolean => can it be a money value?
 	<LI><B>AUTO_INCREMENT</B> boolean => can it be used for an
       auto-increment value?
 	<LI><B>LOCAL_TYPE_NAME</B> String => localized version of type name
       (may be null)
 	<LI><B>MINIMUM_SCALE</B> short => minimum scale supported
 	<LI><B>MAXIMUM_SCALE</B> short => maximum scale supported
 	<LI><B>SQL_DATA_TYPE</B> int => unused
 	<LI><B>SQL_DATETIME_SUB</B> int => unused
 	<LI><B>NUM_PREC_RADIX</B> int => usually 2 or 10
   </OL>

  @return <code>ResultSet</code> - each row is an SQL type description
}
function TZInterbase6DatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
var
  SQL: string;
begin
    Result := ConstructVirtualResultSet(TypeInfoColumnsDynArray);

    SQL := ' SELECT RDB$TYPE, RDB$TYPE_NAME FROM RDB$TYPES ' +
      ' WHERE RDB$FIELD_NAME = ''RDB$FIELD_TYPE'' ';
    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(1, GetString(2));
        Result.UpdateInt(2, Ord(ConvertInterbase6ToSqlType(GetInt(1), 0)));
        Result.UpdateInt(3, 9);
        Result.UpdateInt(7, Ord(ntNoNulls));
        Result.UpdateBoolean(8, false);
        Result.UpdateBoolean(9, false);
        Result.UpdateBoolean(11, false);
        Result.UpdateBoolean(12, false);
        Result.UpdateInt(18, 10);
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of a table's indices and statistics. They are
  ordered by NON_UNIQUE, TYPE, INDEX_NAME, and ORDINAL_POSITION.

  <P>Each index column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>NON_UNIQUE</B> boolean => Can index values be non-unique?
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
function TZInterbase6DatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  SQL : string;
begin
    Result := ConstructVirtualResultSet(IndexInfoColumnsDynArray);

    SQL :=  ' SELECT I.RDB$RELATION_NAME, I.RDB$UNIQUE_FLAG, I.RDB$INDEX_NAME,'
      + ' ISGMT.RDB$FIELD_POSITION,	ISGMT.RDB$FIELD_NAME, I.RDB$INDEX_TYPE,'
      + ' I.RDB$SEGMENT_COUNT, COUNT (DISTINCT P.RDB$PAGE_NUMBER) '
      + ' FROM RDB$INDICES I JOIN RDB$INDEX_SEGMENTS ISGMT ON'
      + ' I.RDB$INDEX_NAME = ISGMT.RDB$INDEX_NAME JOIN RDB$RELATIONS R ON'
      + ' (R.RDB$RELATION_NAME = I.RDB$RELATION_NAME) JOIN RDB$PAGES P ON'
      + ' (P.RDB$RELATION_ID = R.RDB$RELATION_ID AND P.RDB$PAGE_TYPE = 7'
      + ' OR P.RDB$PAGE_TYPE = 6) WHERE ';
    if Unique then
      SQL := SQL + ' I.RDB$UNIQUE_FLAG = 1 AND ';
    SQL := SQL + ' I.RDB$RELATION_NAME = ''' + Table + ''' GROUP BY '
      + ' I.RDB$INDEX_NAME, I.RDB$RELATION_NAME, I.RDB$UNIQUE_FLAG, '
      + ' ISGMT.RDB$FIELD_POSITION, ISGMT.RDB$FIELD_NAME, I.RDB$INDEX_TYPE, '
      + ' I.RDB$SEGMENT_COUNT ORDER BY 2,3,4';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(1); //TABLE_CAT
        Result.UpdateNull(2); //TABLE_SCHEM
        Result.UpdateString(3, GetString(1)); //TABLE_NAME, RDB$RELATION_NAME
        Result.UpdateBoolean(4, not GetBoolean(2)); //NON_UNIQUE, RDB$UNIQUE_FLAG
        Result.UpdateNull(5); //INDEX_QUALIFIER
        Result.UpdateString(6, GetString(3)); //INDEX_NAME, RDB$INDEX_NAME
        Result.UpdateInt(7, Ord(ntNoNulls)); //TYPE
        Result.UpdateInt(8, GetInt(4) + 1); //ORDINAL_POSITION, RDB$FIELD_POSITION
        Result.UpdateString(9, GetString(5)); //COLUMN_NAME, RDB$FIELD_NAME
        Result.UpdateNull(10); //ASC_OR_DESC
        Result.UpdateNull(11); //CARDINALITY
        Result.UpdateInt(12, GetInt(7)); //PAGES, RDB$SEGMENT_COUNT
        Result.UpdateNull(13); //FILTER_CONDITION
        Result.InsertRow;
      end;
      Close;
    end;
end;

function TZInterbase6DatabaseMetadata.UncachedGetSequences(const Catalog, SchemaPattern,
  SequenceNamePattern: string): IZResultSet;
var
  SQL: string;
  LSequenceNamePattern: string;
begin
    Result := ConstructVirtualResultSet(SequenceColumnsDynArray);

    LSequenceNamePattern := ConstructNameCondition(SequenceNamePattern, 
      'RDB$GENERATOR_NAME');

    SQL := ' SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS ' + 
      'WHERE (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0)';

    if LSequenceNamePattern <> '' then
      SQL := SQL + ' AND ' + LSequenceNamePattern;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(1);
        Result.UpdateNull(2);
        Result.UpdateString(3, GetString(1)); //RDB$GENERATOR_NAME
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a privilege name.
  @param  Interbase privilege name
  @returns a JDBC privilege name.
}
function TZInterbase6DatabaseMetadata.GetPrivilege(Privilege: string): string;
begin
  if Privilege = 'S' then
    Result := 'SELECT'
  else if Privilege = 'I' then
    Result := 'INSERT'
  else if Privilege = 'U' then
    Result := 'UPDATE'
  else if Privilege = 'D' then
    Result := 'DELETE'
  else if Privilege = 'R' then
    Result := 'REFERENCE'
  else
    Result := '';
end;

{**
   Takes a name patternand column name and retuen an appropriate SQL clause
    @param Pattern a sql pattren
    @parma Column a sql column name
    @return processed string for query
}
function TZInterbase6DatabaseMetadata.ConstructNameCondition(Pattern, Column: string): string;
const
  Spaces = '';
var
  StrippedPattern: string;
begin
  if (Length(Pattern) > 2 * 31) then
    raise EZSQLException.Create(SPattern2Long);

  if (Pattern = '%') or (Pattern = '') then
     Exit;

  if HasNoWildcards(Pattern) then
  begin
    StrippedPattern := StripEscape(Pattern);
    Result := Format('%s = ''%s''', [Column, StrippedPattern]);
  end
  else
  begin
    Result := Format('%s || ''%s'' like ''%s%s%%''',
      [Column, Spaces, Pattern, Spaces]);
  end;
end;

{**
   Check what pattern do not contain wildcards
   @param Pattern a sql pattern
   @return if pattern contain wildcards return true otherwise false
}
function TZInterbase6DatabaseMetadata.HasNoWildcards(
  const Pattern: string): Boolean;
var
  I: Integer;
  PreviousChar: string[1];
  PreviousCharWasEscape: Boolean;
  EscapeChar : string;
  WildcardsSet: TZWildcardsSet;
begin
  Result := False;
  PreviousChar := '';
  PreviousCharWasEscape := False;
  EscapeChar := GetDatabaseInfo.GetSearchStringEscape;
  WildcardsSet := GetWildcardsSet;
  for I := 1 to Length(Pattern) do
  begin
    if (not PreviousCharWasEscape) and (Pattern[I] in WildcardsSet) then
     Exit;

    PreviousCharWasEscape := (Pattern[I] = EscapeChar) and (PreviousChar <> EscapeChar);
    if (PreviousCharWasEscape) and (Pattern[I] = EscapeChar) then
      PreviousChar := ''
    else
      PreviousChar := Pattern[I];
  end;
  Result := True;
end;

{**
   Remove escapes from pattren string
   @param Pattern a sql pattern
   @return string without escapes
}
function TZInterbase6DatabaseMetadata.StripEscape(
  const Pattern: string): string;
var
  I: Integer;
  PreviousChar: string[1];
  EscapeChar: string;
begin
  PreviousChar := '';
  Result := '';
  EscapeChar := GetDatabaseInfo.GetSearchStringEscape;
  for I := 1 to Length(Pattern) do
  begin
    if (Pattern[i] <> EscapeChar) then
    begin
      Result := Result + Pattern[I];
      PreviousChar := Pattern[I];
    end
    else
    begin
      if (PreviousChar = EscapeChar) then
      begin
        Result := Result + Pattern[I];
        PreviousChar := '';
      end
      else
        PreviousChar := Pattern[i];
    end;
  end;
end;

end.
