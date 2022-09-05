{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
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

unit ZDbcMySqlMetadata;

interface

{$I ZDbc.inc}

uses
  Types, Classes, SysUtils, ZClasses, ZSysUtils, ZDbcIntfs, ZDbcMetadata,
  ZDbcResultSet, ZDbcCachedResultSet, ZDbcResultSetMetadata,
   ZCompatibility, ZDbcConnection{$IFDEF MS_WINDOWS}, Windows{$ENDIF};

type

  // technobot 2008-06-26 - methods moved as is from TZMySQLDatabaseMetadata:
  {** Implements MySQL Database Information. }
  TZMySQLDatabaseInfo = class(TZAbstractDatabaseInfo)
  protected
    procedure GetVersion(var MajorVersion, MinorVersion: integer);
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
//    function SupportsMixedCaseIdentifiers: Boolean; override; -> Not implemented
//    function SupportsMixedCaseQuotedIdentifiers: Boolean; override; -> Not implemented
//    function SupportsAlterTableWithAddColumn: Boolean; override; -> Not implemented
//    function SupportsAlterTableWithDropColumn: Boolean; override; -> Not implemented
//    function SupportsColumnAliasing: Boolean; override; -> Not implemented
//    function SupportsConvert: Boolean; override; -> Not implemented
//    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
//      Boolean; override; -> Not implemented
//    function SupportsTableCorrelationNames: Boolean; override; -> Not implemented
//    function SupportsDifferentTableCorrelationNames: Boolean; override; -> Not implemented
//    function SupportsExpressionsInOrderBy: Boolean; override; -> Not implemented
    function SupportsOrderByUnrelated: Boolean; override;
//    function SupportsGroupBy: Boolean; override; -> Not implemented
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
//    function SupportsSchemasInDataManipulation: Boolean; override; -> Not implemented
//    function SupportsSchemasInProcedureCalls: Boolean; override; -> Not implemented
//    function SupportsSchemasInTableDefinitions: Boolean; override; -> Not implemented
//    function SupportsSchemasInIndexDefinitions: Boolean; override; -> Not implemented
//    function SupportsSchemasInPrivilegeDefinitions: Boolean; override; -> Not implemented
    function SupportsCatalogsInDataManipulation: Boolean; override;
//    function SupportsCatalogsInProcedureCalls: Boolean; override; -> Not implemented
    function SupportsCatalogsInTableDefinitions: Boolean; override;
//    function SupportsCatalogsInIndexDefinitions: Boolean; override; -> Not implemented
//    function SupportsCatalogsInPrivilegeDefinitions: Boolean; override; -> Not implemented
//    function SupportsPositionedDelete: Boolean; override; -> Not implemented
//    function SupportsPositionedUpdate: Boolean; override; -> Not implemented
//    function SupportsSelectForUpdate: Boolean; override; -> Not implemented
//    function SupportsStoredProcedures: Boolean; override; -> Not implemented
    function SupportsSubqueriesInComparisons: Boolean; override;
//    function SupportsSubqueriesInExists: Boolean; override; -> Not implemented
//    function SupportsSubqueriesInIns: Boolean; override; -> Not implemented
//    function SupportsSubqueriesInQuantifieds: Boolean; override; -> Not implemented
//    function SupportsCorrelatedSubqueries: Boolean; override; -> Not implemented
//    function SupportsUnion: Boolean; override; -> Not implemented
    function SupportsUnionAll: Boolean; override;
//    function SupportsOpenCursorsAcrossCommit: Boolean; override; -> Not implemented
//    function SupportsOpenCursorsAcrossRollback: Boolean; override; -> Not implemented
    function SupportsOpenStatementsAcrossCommit: Boolean; override;
    function SupportsOpenStatementsAcrossRollback: Boolean; override;
//    function SupportsTransactions: Boolean; override; -> Not implemented
//    function SupportsTransactionIsolationLevel(Level: TZTransactIsolationLevel):
//      Boolean; override; -> Not implemented
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
//    function SupportsResultSetType(_Type: TZResultSetType): Boolean; override; -> Not implemented
//    function SupportsResultSetConcurrency(_Type: TZResultSetType;
//      Concurrency: TZResultSetConcurrency): Boolean; override; -> Not implemented
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
//    function GetMaxSchemaNameLength: Integer; override; -> Not implemented
//    function GetMaxProcedureNameLength: Integer; override; -> Not implemented
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
//    function StoresUpperCaseIdentifiers: Boolean; override; -> Not implemented
//    function StoresLowerCaseIdentifiers: Boolean; override; -> Not implemented
    function StoresMixedCaseIdentifiers: Boolean; override;
//    function StoresUpperCaseQuotedIdentifiers: Boolean; override; -> Not implemented
//    function StoresLowerCaseQuotedIdentifiers: Boolean; override; -> Not implemented
//    function StoresMixedCaseQuotedIdentifiers: Boolean; override; -> Not implemented
    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; override;
//    function DataDefinitionCausesTransactionCommit: Boolean; override; -> Not implemented
//    function DataDefinitionIgnoredInTransactions: Boolean; override; -> Not implemented

    // interface details (terms, keywords, etc):
    function GetIdentifierQuoteString: string; override;
    function GetSchemaTerm: string; override;
    function GetProcedureTerm: string; override;
    function GetCatalogTerm: string; override;
//    function GetCatalogSeparator: string; override; -> Not implemented
    function GetSQLKeywords: string; override;
    function GetNumericFunctions: string; override;
    function GetStringFunctions: string; override;
    function GetSystemFunctions: string; override;
    function GetTimeDateFunctions: string; override;
    function GetSearchStringEscape: string; override;
    function GetExtraNameCharacters: string; override;
  end;

  {** Implements MySQL Database Metadata. }
  TZMySQLDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    FDatabase: string;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-26

    procedure GetCatalogAndNamePattern(const Catalog, SchemaPattern,
      NamePattern: string; out OutCatalog, OutNamePattern: string);
    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
//    function UncachedGetSchemas: IZResultSet; override; -> Not implemented
    function UncachedGetCatalogs: IZResultSet; override;
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
    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
//     function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
//      const SequenceNamePattern: string): IZResultSet; override; -> Not implemented
//    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
//      const ProcedureNamePattern: string): IZResultSet; override;
//    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
//      const ProcedureNamePattern: string; const ColumnNamePattern: string):
//      IZResultSet; override;
    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
//    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
//      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;

  public
    constructor Create(Connection: TZAbstractConnection; Url: string; Info: TStrings);
    destructor Destroy; override;
  end;

implementation

uses
  Math, ZMessages, ZDbcUtils, ZCollections, ZDbcMySqlUtils;

{ TZMySQLDatabaseInfo }

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
}
constructor TZMySQLDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata);
begin
  inherited;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZMySQLDatabaseInfo.Destroy;
begin
  inherited;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZMySQLDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'MySQL';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZMySQLDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '3+';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZMySQLDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for MySQL';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZMySQLDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZMySQLDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZMySQLDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  What's the string used to quote SQL identifiers?
  This returns a space " " if identifier quoting isn't supported.
  A JDBC Compliant<sup><font size=-2>TM</font></sup>
  driver always uses a double quote character.
  @return the quoting string
}
function TZMySQLDatabaseInfo.GetIdentifierQuoteString: string;
begin
  Result := '`';
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZMySQLDatabaseInfo.GetSQLKeywords: string;
begin
  Result := 'AUTO_INCREMENT,BINARY,BLOB,ENUM,INFILE,LOAD,MEDIUMINT,OPTION,'
    + 'OUTFILE,REPLACE,SET,TEXT,UNSIGNED,ZEROFILL';
  { mdaems : added all reserved words indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'ACCESSIBLE,ADD,ALL,ALTER,ANALYZE,AND,ASC,ASENSITIVE,'
    + ' BEFORE,BETWEEN,BIGINT,BOTH,CALL,CASCADE,CASE,CHANGE,CHARACTER,CHECK,'
    + 'COLLATE,COLUMN,CONDITION,CONSTRAINT,CONTINUE,CONVERT,CROSS,'
    + 'CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,CURSOR,'
    + 'DATABASE,DATABASES,DAY_HOUR,DAY_MICROSECOND,DAY_MINUTE,DAY_SECOND,'
    + 'DEC,DECIMAL,DECLARE,DEFAULT,DELAYED,DESC,DESCRIBE,DETERMINISTIC,'
    + 'DISTINCT,DISTINCTROW,DIV,DOUBLE,DUAL,EACH,ELSE,ELSEIF,ENCLOSED,'
    + 'ESCAPED,EXISTS,EXIT,EXPLAIN,FALSE,FETCH,FLOAT,FLOAT4,FLOAT8,FOR,'
    + 'FORCE,FOREIGN,FULLTEXT,GRANT,HIGH_PRIORITY,HOUR_MICROSECOND,'
    + 'HOUR_MINUTE,HOUR_SECOND,IF,IGNORE,IN,INNER,INOUT,INSENSITIVE,INT,'
    + 'INT1,INT2,INT3,INT4,INT8,INTERVAL,ITERATE,JOIN,KEYS,KILL,LEADING,'
    + 'LEAVE,LEFT,LIKE,LIMIT,LINEAR,LINES,LOCALTIME,LOCALTIMESTAMP,LOCK,'
    + 'LONG,LONGBLOB,LONGTEXT,LOOP,LOW_PRIORITY,MASTER_SSL_VERIFY_SERVER_CERT,'
    + 'MATCH,MEDIUMBLOB,MEDIUMTEXT,MIDDLEINT,MINUTE_MICROSECOND,MINUTE_SECOND,'
    + 'MOD,MODIFIES,NATURAL,NOT,NO_WRITE_TO_BINLOG,NUMERIC,OPTIMIZE,'
    + 'OPTIONALLY,OR,OUT,OUTER,PRECISION,PROCEDURE,PURGE,RANGE,READ,READS,'
    + 'READ_ONLY,READ_WRITE,REAL,REFERENCES,REGEXP,RELEASE,RENAME,REPEAT,'
    + 'REQUIRE,RESTRICT,RETURN,REVOKE,RIGHT,RLIKE,SCHEMA,SCHEMAS,'
    + 'SECOND_MICROSECOND,SENSITIVE,SEPARATOR,SHOW,SMALLINT,SPATIAL,'
    + 'SPECIFIC,SQL,SQLEXCEPTION,SQLSTATE,SQLWARNING,SQL_BIG_RESULT,'
    + 'SQL_CALC_FOUND_ROWS,SQL_SMALL_RESULT,SSL,STARTING,STRAIGHT_JOIN,'
    + 'TERMINATED,THEN,TINYBLOB,TINYINT,TINYTEXT,TO,TRAILING,TRIGGER,'
    + 'TRUE,UNDO,UNION,UNIQUE,UNLOCK,USAGE,USE,USING,UTC_DATE,UTC_TIME,'
    + 'UTC_TIMESTAMP,VARBINARY,VARCHARACTER,VARYING,WHEN,WHILE,WITH,'
    + 'WRITE,X509,XOR,YEAR_MONTH,ACCESSIBLE,LINEAR,'
    + 'MASTER_SSL_VERIFY_SERVER_CERT,RANGE,READ_ONLY,READ_WRITE';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZMySQLDatabaseInfo.GetNumericFunctions: string;
begin
  Result := 'ABS,ACOS,ASIN,ATAN,ATAN2,BIT_COUNT,CEILING,COS,COT,DEGREES,EXP,'
    + 'FLOOR,LOG,LOG10,MAX,MIN,MOD,PI,POW,POWER,RADIANS,RAND,ROUND,SIN,SQRT,'
    + 'TAN,TRUNCATE';
  { mdaems : added all numeric functions indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'BIT_COUNT,CEIL,CRC32,LN,LOG2,SIGN,UUID';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZMySQLDatabaseInfo.GetStringFunctions: string;
begin
  Result := 'ASCII,CHAR,CHAR_LENGTH,CHARACTER_LENGTH,CONCAT,ELT,FIELD,'
    + 'FIND_IN_SET,INSERT,INSTR,INTERVAL,LCASE,LEFT,LENGTH,LOCATE,LOWER,LTRIM,'
    + 'MID,POSITION,OCTET_LENGTH,REPEAT,REPLACE,REVERSE,RIGHT,RTRIM,SPACE,'
    + 'SOUNDEX,SUBSTRING,SUBSTRING_INDEX,TRIM,UCASE,UPPER';
  { mdaems : added all string functions indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'AES_DECRYPT,AES_ENCRYPT,BIN,BIT_LENGTH,CHARSET,'
    + 'COERCIBILITY,COLLATION,COMPRESS,CONCAT_WS,DECODE,DES_DECRYPT,DES_ENCRYPT,'
    + 'ENCODE,ENCRYPT,EXPORT_SET,FORMAT,HEX,LOAD_FILE,LPAD,MAKE_SET,MD5,OCT,ORD,'
    + 'QUOTE,RPAD,STRCMP,SHA,SHA1,SUBSTR,UNHEX,EXTRACTVALUE,UPDATEXML,'
    + 'UNCOMPRESS,UNCOMPRESSED_LENGTH';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZMySQLDatabaseInfo.GetSystemFunctions: string;
begin
  Result := 'DATABASE,USER,SYSTEM_USER,SESSION_USER,PASSWORD,ENCRYPT,'
    + 'LAST_INSERT_ID,VERSION';
  { mdaems : added all system functions indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'BENCHMARK,CONNECTION_ID,CURRENT_USER,DEFAULT,FOUND_ROWS,'
    + 'GET_LOCK,INET_ATON,INET_NTOA,IS_FREE_LOCK,IS_USED_LOCK,MASTER_POS_WAIT,'
    + 'NAME_CONST,OLD_PASSWORD,RELEASE_LOCK,ROW_COUNT,SCHEMA,SLEEP';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZMySQLDatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := 'DAYOFWEEK,WEEKDAY,DAYOFMONTH,DAYOFYEAR,MONTH,DAYNAME,MONTHNAME,'
    + 'QUARTER,WEEK,YEAR,HOUR,MINUTE,SECOND,PERIOD_ADD,PERIOD_DIFF,TO_DAYS,'
    + 'FROM_DAYS,DATE_FORMAT,TIME_FORMAT,CURDATE,CURRENT_DATE,CURTIME,'
    + 'CURRENT_TIME,NOW,SYSDATE,CURRENT_TIMESTAMP,UNIX_TIMESTAMP,FROM_UNIXTIME,'
    + 'SEC_TO_TIME,TIME_TO_SEC';
  { mdaems : added all time and date functions indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'ADDDATE,ADDTIME,CONVERT_TZ,CURRENT_TIMESTAMP,DATE_ADD,'
    + 'DATE_SUB,DATE,DATEDIFF,DAYOFWEEK,GET_FORMAT,LAST_DAY,LOCALTIME,'
    + 'LOCALTIMESTAMP,MAKEDATE,MAKETIME,MICROSECOND,STR_TO_DATE,SUBDATE,SUBTIME,'
    + 'TIMEDIFF,TIMESTAMP,TIMESTAMPADD,TIMESTAMPDIFF,UTC_DATE,UTC_TIME,'
    + 'UTC_TIMESTAMP,WEEKOFYEAR,YEARWEEK';
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
function TZMySQLDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '\';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZMySQLDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsOrderByUnrelated: Boolean;
var
  MajorVersion: Integer;
  MinorVersion: Integer;
begin
  GetVersion(MajorVersion, MinorVersion);
  // changed from False by mdaems. After testing with lower versions, please correct.
  Result := MajorVersion >= 5;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := False;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZMySQLDatabaseInfo.GetSchemaTerm: string;
begin
  Result := '';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZMySQLDatabaseInfo.GetProcedureTerm: string;
begin
  Result := '';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZMySQLDatabaseInfo.GetCatalogTerm: string;
begin
  Result := 'Database';
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
var
  MajorVersion: Integer;
  MinorVersion: Integer;
begin
  GetVersion(MajorVersion, MinorVersion);
  Result := ((MajorVersion = 3) and (MinorVersion >= 22)) or (MajorVersion > 3);
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsUnionAll: Boolean;
var
  MajorVersion: Integer;
  MinorVersion: Integer;
begin
  GetVersion(MajorVersion, MinorVersion);
  Result := MajorVersion >= 4;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZMySQLDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZMySQLDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
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
function TZMySQLDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 16777208;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 16777208;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 64;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 256;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 512;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := 64;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 32;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 2147483639;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := True;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 65531;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
    a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
    a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 64;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 256;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
    a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 16;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZMySQLDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiNone;
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsDataManipulationTransactionsOnly: Boolean;
begin
  case Metadata.GetConnection.GetTransactionIsolation of
    tiReadUncommitted: Result := True;
    tiReadCommitted: Result := True;
    tiRepeatableRead: Result := True;
    tiSerializable: Result := True;
  else
    Result := False;
  end;
end;

{**
  Gets the MySQL version info.
  @param MajorVesion the major version of MySQL server.
  @param MinorVersion the minor version of MySQL server.
}
procedure TZMySQLDatabaseInfo.GetVersion(var MajorVersion,
  MinorVersion: Integer);
var
  VersionList: TStrings;
  Subversion : integer;
begin
  DecodeSqlVersioning(Metadata.GetConnection.GetHostVersion,
    MajorVersion,MinorVersion, Subversion);
  if (Majorversion < 4) or ((majorversion=4) and (Minorversion = 0)) then
   with Metadata.GetConnection.CreateStatement.ExecuteQuery('SELECT VERSION()') do
    begin
      VersionList := SplitString(GetString(1), '.-');
      try
        if VersionList.Count >= 2 then
        begin
          MajorVersion := StrToIntDef(VersionList.Strings[0], 0);
          MinorVersion := StrToIntDef(VersionList.Strings[1], 0);
        end;
      finally
        VersionList.Free;
      end;
      Close;
    end;
end;

{ TZMySQLDatabaseMetadata }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Url a database connection url string.
  @param Info an extra connection properties.
}
constructor TZMySQLDatabaseMetadata.Create(Connection: TZAbstractConnection;
  Url: string; Info: TStrings);
var
  TempInfo: TStrings;
  Hostname, UserName, Password: string;
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
destructor TZMySQLDatabaseMetadata.Destroy;
begin
  inherited Destroy;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZMySQLDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZMySQLDatabaseInfo.Create(Self);
end;

procedure TZMySQLDatabaseMetadata.GetCatalogAndNamePattern(const Catalog,
  SchemaPattern, NamePattern: string; out OutCatalog, OutNamePattern: string);
begin
  if Catalog = '' then
  begin
    if SchemaPattern <> '' then
      OutCatalog := SchemaPattern
    else
      OutCatalog := FDatabase;
  end
  else
    OutCatalog := Catalog;

  if NamePattern = '' then
    OutNamePattern := '%'
  else
    OutNamePattern := NamePattern;
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
function TZMySQLDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  LCatalog, LTableNamePattern: string;
begin
    Result := ConstructVirtualResultSet(TableColumnsDynArray);

    GetCatalogAndNamePattern(Catalog, SchemaPattern, TableNamePattern,
      LCatalog, LTableNamePattern);

    with GetConnection.CreateStatement.ExecuteQuery(
      Format('SHOW TABLES FROM %s LIKE ''%s''',
      [GetIdentifierConvertor.Quote(LCatalog), LTableNamePattern])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(1, LCatalog);
        Result.UpdateString(3, GetString(1));
        Result.UpdateString(4, 'TABLE');
        Result.InsertRow;
      end;
      Close;
    end;

    // If a table was specified but not found, check if it could be a temporary table
    if not Result.First and (LTableNamePattern <> '%') then
    begin
      try
        EnterSilentMySQLError;
        try
          if GetConnection.CreateStatement.ExecuteQuery(
            Format('SHOW COLUMNS FROM %s.%s',
            [GetIdentifierConvertor.Quote(LCatalog),
             GetIdentifierConvertor.Quote(LTableNamePattern)])).Next then
          begin
            Result.MoveToInsertRow;
            Result.UpdateString(1, LCatalog);
            Result.UpdateString(3, LTableNamePattern);
            Result.UpdateString(4, 'TABLE');
            Result.InsertRow;
          end;
        finally
          LeaveSilentMySQLError;
        end;
      except
        on EZMySQLSilentException do ;
        on EZSQLException do ;
      end;
    end;
end;

{**
  Gets the catalog names available in this database.  The results
  are ordered by catalog name.

  <P>The catalog column is:
   <OL>
 	<LI><B>TABLE_CAT</B> String => catalog name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  catalog name
}
function TZMySQLDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
begin
    Result := ConstructVirtualResultSet(CatalogColumnsDynArray);

    with GetConnection.CreateStatement.ExecuteQuery('SHOW DATABASES') do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(1, GetString(1));
        Result.InsertRow;
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
function TZMySQLDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
begin
    Result := ConstructVirtualResultSet(TableTypeColumnsDynArray);
    Result.MoveToInsertRow;
    Result.UpdateString(1, 'TABLE');
    Result.InsertRow;
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
function TZMySQLDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  I, J: Integer;
  MySQLType: TZSQLType;
  TempCatalog, TempColumnNamePattern, TempTableNamePattern: string;

  { TODO : TempStr is not set to a value in the whole method. => Length(TempStr) = 0 }
  TempStr: string;
  TempPos: Integer;

  TypeInfoList: TStrings;
  TypeInfo, TypeInfoFirst, TypeInfoSecond, Collation: string;
  Nullable, DefaultValue: string;
  HasDefaultValue: Boolean;
  ColumnSize, ColumnDecimals: Integer;
  OrdPosition: Integer;

  TableNameList: TStrings;
  TableNameLength: Integer;
  ColumnIndexes : Array[1..6] of integer;
  Res : IZResultset;

begin
    Res := ConstructVirtualResultSet(TableColColumnsDynArray);

    GetCatalogAndNamePattern(Catalog, SchemaPattern, ColumnNamePattern,
      TempCatalog, TempColumnNamePattern);

    TableNameLength := 0;
    TableNameList := TStringList.Create;
    TypeInfoList := TStringList.Create;
    try
      with GetTables(Catalog, SchemaPattern, TableNamePattern, nil) do
      begin
        while Next do
        begin
          TableNameList.Add(GetString(3)); //TABLE_NAME
          TableNameLength := Max(TableNameLength, Length(TableNameList[TableNameList.Count - 1]));
        end;
        Close;
      end;

      for I := 0 to TableNameList.Count - 1 do
      begin
        OrdPosition := 1;
        TempTableNamePattern := TableNameList.Strings[I];

        with GetConnection.CreateStatement.ExecuteQuery(
          Format('SHOW FULL COLUMNS FROM %s.%s LIKE ''%s''',
          [GetIdentifierConvertor.Quote(TempCatalog),
          GetIdentifierConvertor.Quote(TempTableNamePattern),
          TempColumnNamePattern])) do
        begin
          ColumnIndexes[1] := FindColumn('Field');
          ColumnIndexes[2] := FindColumn('Type');
          ColumnIndexes[3] := FindColumn('Null');
          ColumnIndexes[4] := FindColumn('Extra');
          ColumnIndexes[5] := FindColumn('Default');
          ColumnIndexes[6] := FindColumn('Collation');
          while Next do
          begin
            {initialise some variables}
            ColumnSize := 0;
            TypeInfoFirst := '';
            TypeInfoSecond := '';
            Collation:='';

            Res.MoveToInsertRow;
            Res.UpdateString(1, TempCatalog);
            Res.UpdateString(2, '');
            Res.UpdateString(3, TempTableNamePattern);
            Res.UpdateString(4, GetString(ColumnIndexes[1]));

            TypeInfo := GetString(ColumnIndexes[2]);
            if StrPos(PChar(TypeInfo), '(') <> nil then
            begin
              PutSplitString(TypeInfoList, TypeInfo, '()');
              TypeInfoFirst := TypeInfoList.Strings[0];
              TypeInfoSecond := TypeInfoList.Strings[1];
            end
            else
              TypeInfoFirst := TypeInfo;

            Collation:=GetString(ColumnIndexes[6]);

            TypeInfoFirst := LowerCase(TypeInfoFirst);
            MySQLType := ConvertMySQLTypeToSQLType(TypeInfoFirst, TypeInfo, Collation);
            Res.UpdateInt(5, Ord(MySQLType));
            Res.UpdateString(6, TypeInfoFirst);

            Res.UpdateInt(7, 0);
            Res.UpdateInt(9, 0);
            { the column type is ENUM}
            if TypeInfoFirst = 'enum' then
            begin
              PutSplitString(TypeInfoList, TypeInfoSecond, ',');
              for J := 0 to TypeInfoList.Count-1 do
                ColumnSize := Max(ColumnSize, Length(TypeInfoList.Strings[J]));

              Res.UpdateInt(7, ColumnSize);
              Res.UpdateInt(9, 0);
            end
            else
              { the column type is decimal }
              if StrPos(PChar(TypeInfo), ',') <> nil then
              begin
                TempPos := FirstDelimiter(',', TypeInfoSecond);
                ColumnSize := StrToIntDef(Copy(TypeInfoSecond, 1, TempPos - 1), 0);
                ColumnDecimals := StrToIntDef(Copy(TypeInfoSecond, TempPos + 1,
                  Length(TempStr) - TempPos), 0);
                Res.UpdateInt(7, ColumnSize);
                Res.UpdateInt(9, ColumnDecimals);
              end
              else
              begin
                { the column type is other }
                 if TypeInfoSecond <> '' then
                    ColumnSize := StrToIntDef(TypeInfoSecond, 0)
                 else if TypeInfoFirst = 'tinyint' then
                    ColumnSize := 1
                 else if TypeInfoFirst = 'smallint' then
                    ColumnSize := 6
                 else if TypeInfoFirst = 'mediumint' then
                    ColumnSize := 6
                 else if TypeInfoFirst = 'int' then
                    ColumnSize := 11
                 else if TypeInfoFirst = 'integer' then
                    ColumnSize := 11
                 else if TypeInfoFirst = 'bigint' then
                    ColumnSize := 25
                 else if TypeInfoFirst = 'int24' then
                    ColumnSize := 25
                 else if TypeInfoFirst = 'real' then
                    ColumnSize := 12
                 else if TypeInfoFirst = 'float' then
                    ColumnSize := 12
                 else if TypeInfoFirst = 'decimal' then
                    ColumnSize := 12
                 else if TypeInfoFirst = 'numeric' then
                    ColumnSize := 12
                 else if TypeInfoFirst = 'double' then
                    ColumnSize := 22
                 else if TypeInfoFirst = 'char' then
                    ColumnSize := 1
                 else if TypeInfoFirst = 'varchar' then
                    ColumnSize := 255
                 else if TypeInfoFirst = 'date' then
                    ColumnSize := 10
                 else if TypeInfoFirst = 'time' then
                    ColumnSize := 8
                 else if TypeInfoFirst = 'timestamp' then
                    ColumnSize := 19
                 else if TypeInfoFirst = 'datetime' then
                    ColumnSize := 19
                 else if TypeInfoFirst = 'tinyblob' then
                    ColumnSize := 255
                 else if TypeInfoFirst = 'blob' then
                    ColumnSize := MAXBUF
                 else if TypeInfoFirst = 'mediumblob' then
                    ColumnSize := 16277215//may be 65535
                 else if TypeInfoFirst = 'longblob' then
                    ColumnSize := High(Integer)//2147483657//may be 65535
                 else if TypeInfoFirst = 'tinytext' then
                    ColumnSize := 255
                 else if TypeInfoFirst = 'text' then
                    ColumnSize := 65535
                 else if TypeInfoFirst = 'mediumtext' then
                    ColumnSize := 16277215 //may be 65535
                 else if TypeInfoFirst = 'enum' then
                    ColumnSize := 255
                 else if TypeInfoFirst = 'set' then
                    ColumnSize := 255;
                Res.UpdateInt(7, ColumnSize);
                Res.UpdateInt(9, 0);
              end;

            Res.UpdateInt(8, MAXBUF);
            Res.UpdateNull(10);

            { Sets nullable fields. }
            Nullable := GetString(ColumnIndexes[3]);
            if Nullable <> '' then
              if Nullable = 'YES' then
              begin
                Res.UpdateInt(11, Ord(ntNullable));
                Res.UpdateString(18, 'YES');
              end
              else
              begin
                Res.UpdateInt(11, Ord(ntNoNulls));
                Res.UpdateString(18, 'NO');
              end
            else
            begin
              Res.UpdateInt(11, 0);
              Res.UpdateString(18, 'NO');
            end;
            Res.UpdateString(12, GetString(ColumnIndexes[4]));
            // MySQL is a bit bizarre.
            if IsNull(ColumnIndexes[5]) then
            begin
              // MySQL bizarity 1:
              // NULL actually means that the default is NULL.
              // Superfluous, since there's a NULL / NOT NULL flag to control whether the field may have no value.
              // So we just ignore this, the field gets set to NULL if nothing was specified...
              HasDefaultValue := false;
              DefaultValue := '';
            end
            else
            begin
              DefaultValue := GetString(ColumnIndexes[5]);
              if not (DefaultValue = '') then
                 HasDefaultValue := true
              else
              begin
                // MySQL bizarity 2:
                // For CHAR, BLOB, TEXT and SET types, '' either means: default value is '' or: no default value
                // There's absolutely no way of telling when using SHOW COLUMNS FROM,
                // the correct information can /only/ be discerned by using information_schema.
                // TODO: For now, just use '' as default value for these types, but this should really be fixed to use information_schema.
                // For ENUM types, '' means: default value is first value in enum set
                // For other types, '' means: no default value
                HasDefaultValue := false;
                if Pos('blob', TypeInfoFirst) > 0 then HasDefaultValue := true;
                if Pos('text', TypeInfoFirst) > 0 then HasDefaultValue := true;
                if Pos('char', TypeInfoFirst) > 0 then HasDefaultValue := true;
                if 'set' = TypeInfoFirst then HasDefaultValue := true;
                if 'enum' =  TypeInfoFirst then
                  begin
                    HasDefaultValue := true;
                    DefaultValue := Copy(TypeInfoSecond, 2,length(TypeInfoSecond)-1);
                    DefaultValue := Copy(DefaultValue, 1, Pos('''', DefaultValue) - 1);
                  end;
              end;
            end;
            if HasDefaultValue then
            begin
              // String values in the 'Default value' field are not escaped with apostrophes.
              // Guess this makes it impossible to specify a function call or similar via default values.
              if (MySQLType in [stString, stUnicodeString, stBinaryStream, stAsciiStream]) then
              begin
                // Since we changed date/time-related columntypes to be presented
                // as strings, we need to move the CURRENT_TIMESTAMP-check to here.
                // Also left the other line in order to minimize the changes in ZeosLib
                if DefaultValue <> 'CURRENT_TIMESTAMP' then
                DefaultValue := '''' + DefaultValue + ''''
              end
              else if (MySQLType in [stDate, stTime, stTimestamp]) then
              begin
                if DefaultValue <> 'CURRENT_TIMESTAMP' then
                  DefaultValue := '''' + DefaultValue + ''''
              end
              else if (MySQLType = stBoolean) and (TypeInfoFirst = 'enum') then
              begin
                if (DefaultValue = 'y') or (DefaultValue = 'Y') then
                  DefaultValue := '1'
                else
                  DefaultValue := '0';
              end;
            end;
              Res.UpdateString(13, DefaultValue);
            Res.UpdateNull(14);
            Res.UpdateNull(15);
            Res.UpdateInt(17, OrdPosition);

            Res.UpdateBoolean(19, //AUTO_INCREMENT
              Trim(LowerCase(GetString(ColumnIndexes[4]))) = 'auto_increment'); //Extra
            Res.UpdateBoolean(20, //CASE_SENSITIVE
              GetIdentifierConvertor.IsCaseSensitive(
              GetString(ColumnIndexes[1]))); //Field
            Res.UpdateBoolean(21, True);  //SEARCHABLE
            Res.UpdateBoolean(22, True);  //WRITABLE
            Res.UpdateBoolean(23, True);  //DEFINITELYWRITABLE
            Res.UpdateBoolean(24, False); //READONLY

            Inc(OrdPosition);
            Res.InsertRow;
          end;
          Close;
        end;
      end;
    finally
      TableNameList.Free;
      TypeInfoList.Free;
    end;
    Result := Res;
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
function TZMySQLDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  I: Integer;
  LCatalog, LColumnNamePattern: string;
  Host, Database, Grantor, User, FullUser: string;
  AllPrivileges, ColumnName, Privilege: string;
  PrivilegesList: TStrings;
begin
    Result := ConstructVirtualResultSet(TableColPrivColumnsDynArray);

    GetCatalogAndNamePattern(Catalog, Schema, ColumnNamePattern,
      LCatalog, LColumnNamePattern);

    PrivilegesList := TStringList.Create;
    try
      with GetConnection.CreateStatement.ExecuteQuery(
        Format('SELECT c.host, c.db, t.grantor, c.user, c.table_name,'
          + ' c.column_name, c.column_priv FROM mysql.columns_priv c,'
          + ' mysql.tables_priv t WHERE c.host=t.host AND c.db=t.db'
          + ' AND c.table_name=t.table_name AND c.db=''%s'''
          + ' AND c.table_name=''%s'' AND c.column_name LIKE ''%s''',
          [LCatalog, Table, LColumnNamePattern])) do
      begin
        while Next do
        begin
          Host := GetString(1);
          Database := GetString(2);
          Grantor := GetString(4);
          User := GetString(5);
          if User = '' then
            User := '%';
          if Host <> '' then
            FullUser := User + '@' + Host;
          ColumnName := GetString(6);

          AllPrivileges := GetString(7);
          PutSplitString(PrivilegesList, AllPrivileges, ',');

          for I := 0 to PrivilegesList.Count - 1 do
          begin
            Result.MoveToInsertRow;
            Privilege := Trim(PrivilegesList.Strings[I]);
            Result.UpdateString(1, LCatalog);
            Result.UpdateNull(2);
            Result.UpdateString(3, Table);
            Result.UpdateString(4, ColumnName);
            Result.UpdateString(5, Grantor);
            Result.UpdateString(6, FullUser);
            Result.UpdateString(7, Privilege);
            Result.UpdateNull(8);
            Result.InsertRow;
          end;
        end;
        Close;
      end;
    finally
      PrivilegesList.Free;
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
function TZMySQLDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  I: Integer;
  LCatalog, LTableNamePattern: string;
  Host, Database, Table, Grantor, User, FullUser: string;
  AllPrivileges, Privilege: string;
  PrivilegesList: TStrings;
begin
    Result := ConstructVirtualResultSet(TablePrivColumnsDynArray);

    GetCatalogAndNamePattern(Catalog, SchemaPattern, TableNamePattern,
      LCatalog, LTableNamePattern);

    PrivilegesList := TStringList.Create;
    try
      with GetConnection.CreateStatement.ExecuteQuery(
        Format('SELECT host,db,table_name,grantor,user,table_priv'
        + ' from mysql.tables_priv WHERE db=''%s'' AND table_name LIKE ''%s''',
        [LCatalog, LTableNamePattern])) do
      begin
        while Next do
        begin
          Host := GetString(1);
          Database := GetString(2);
          Table := GetString(3);
          Grantor := GetString(4);
          User := GetString(5);
          if User = '' then
            User := '%';
          if Host <> '' then
            FullUser := User + '@' + Host;

          AllPrivileges := GetString(6);
          PutSplitString(PrivilegesList, AllPrivileges, ',');

          for I := 0 to PrivilegesList.Count - 1 do
          begin
            Result.MoveToInsertRow;
            Privilege := Trim(PrivilegesList.Strings[I]);
            Result.UpdateString(1, Database);
            Result.UpdateNull(2);
            Result.UpdateString(3, Table);
            Result.UpdateString(4, Grantor);
            Result.UpdateString(5, FullUser);
            Result.UpdateString(6, Privilege);
            Result.UpdateNull(7);
            Result.InsertRow;
          end;
        end;
        Close;
      end;
    finally
      PrivilegesList.Free;
    end;
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
function TZMySQLDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  KeyType: string;
  LCatalog: string;
  ColumnIndexes : Array[1..3] of integer;
begin
    if Table = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!
    Result := ConstructVirtualResultSet(PrimaryKeyColumnsDynArray);

    if Catalog = '' then
    begin
      if Schema <> '' then
        LCatalog := Schema
      else
        LCatalog := FDatabase;
    end
    else
      LCatalog := Catalog;

    with GetConnection.CreateStatement.ExecuteQuery(
      Format('SHOW KEYS FROM %s.%s',
      [GetIdentifierConvertor.Quote(LCatalog),
      GetIdentifierConvertor.Quote(Table)])) do
    begin
      ColumnIndexes[1] := FindColumn('Key_name');
      ColumnIndexes[2] := FindColumn('Column_name');
      ColumnIndexes[3] := FindColumn('Seq_in_index');
      while Next do
      begin
        KeyType := UpperCase(GetString(ColumnIndexes[1]));
        KeyType := Copy(KeyType, 1, 3);
        if KeyType = 'PRI' then
        begin
          Result.MoveToInsertRow;
          Result.UpdateString(1, LCatalog);
          Result.UpdateString(2, '');
          Result.UpdateString(3, Table);
          Result.UpdateString(4, GetString(ColumnIndexes[2]));
          Result.UpdateString(5, GetString(ColumnIndexes[3]));
          Result.UpdateNull(6);
          Result.InsertRow;
        end;
      end;
      Close;
    end;
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
function TZMySQLDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  I: Integer;
  KeySeq: Integer;
  LCatalog: string;
  TableType, Comment, Keys: string;
  CommentList, KeyList: TStrings;
  ColumnIndexes : Array[1..2] of integer;
begin
    if Table = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!
    Result := ConstructVirtualResultSet(ImportedKeyColumnsDynArray);

    if Catalog = '' then
    begin
      if Schema <> '' then
        LCatalog := Schema
      else
        LCatalog := FDatabase;
    end
    else
      LCatalog := Catalog;

    KeyList := TStringList.Create;
    CommentList := TStringList.Create;
    try
      with GetConnection.CreateStatement.ExecuteQuery(
        Format('SHOW TABLE STATUS FROM %s LIKE ''%s''',
        [GetIdentifierConvertor.Quote(LCatalog), Table])) do
      begin
        ColumnIndexes[1] := FindColumn('Type');
        ColumnIndexes[2] := FindColumn('Comment');
        while Next do
        begin
          TableType := GetString(ColumnIndexes[1]);
          if (TableType <> '') and (LowerCase(TableType) = 'innodb') then
          begin
            Comment := GetString(ColumnIndexes[2]);
            if Comment <> '' then
            begin
              PutSplitString(CommentList, Comment, ';');
              KeySeq := 0;

              if CommentList.Count > 4 then
              begin
                for I := 0 to CommentList.Count - 1 do
                begin
                  Keys := CommentList.Strings[1];
                  Result.MoveToInsertRow;
                  PutSplitString(KeyList, Keys, '() /');

                  Result.UpdateString(5, LCatalog);
                  Result.UpdateNull(6);// FKTABLE_SCHEM
                  Result.UpdateString(7, Table); // FKTABLE_NAME
                  Result.UpdateString(8, KeyList.Strings[0]); // FKCOLUMN_NAME

                  Result.UpdateString(1, KeyList.Strings[2]); // PKTABLE_CAT
                  Result.UpdateNull(2); // PKTABLE_SCHEM
                  Result.UpdateString(3, KeyList.Strings[3]); // PKTABLE_NAME
                  Result.UpdateString(4, KeyList.Strings[4]); // PKCOLUMN_NAME
                  Result.UpdateInt(9, KeySeq); // KEY_SEQ
                  Result.UpdateInt(10, Ord(ikSetDefault)); // UPDATE_RULE
                  Result.UpdateInt(11, Ord(ikSetDefault)); // DELETE_RULE
                  Result.UpdateNull(12); // FK_NAME
                  Result.UpdateNull(13); // PK_NAME
                  Result.UpdateInt(14, Ord(ikSetDefault)); // DEFERRABILITY
                  Inc(KeySeq);
                  Result.InsertRow;
                end;
              end;
            end;
          end;
        end;
        Close;
      end;
    finally
      KeyList.Free;
      CommentList.Free;
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
function TZMySQLDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  I: Integer;
  KeySeq: Integer;
  LCatalog: string;
  TableType, Comment, Keys: string;
  CommentList, KeyList: TStrings;
  ColumnIndexes : Array[1..3] of integer;
begin
    if Table = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!
    Result := ConstructVirtualResultSet(ExportedKeyColumnsDynArray);

    if Catalog = '' then
    begin
      if Schema <> '' then
        LCatalog := Schema
      else
        LCatalog := FDatabase;
    end
    else
      LCatalog := Catalog;

    KeyList := TStringList.Create;
    CommentList := TStringList.Create;
    try
      with GetConnection.CreateStatement.ExecuteQuery(
        Format('SHOW TABLE STATUS FROM %s',
        [GetIdentifierConvertor.Quote(LCatalog)])) do
      begin
        ColumnIndexes[1] := FindColumn('Type');
        ColumnIndexes[2] := FindColumn('Comment');
        ColumnIndexes[3] := FindColumn('Name');
        while Next do
        begin
          TableType := GetString(ColumnIndexes[1]);
          if (TableType <> '') and (LowerCase(TableType) = 'innodb') then
          begin
            Comment := GetString(ColumnIndexes[2]);
            if Comment <> '' then
            begin
              PutSplitString(CommentList, Comment, ';');
              KeySeq := 0;
              if CommentList.Count > 4 then
              begin
                for I := 0 to CommentList.Count-1 do
                begin
                  Keys := CommentList.Strings[1];
                  Result.MoveToInsertRow;
                  PutSplitString(KeyList, Keys, '() /');

                  Result.UpdateString(5, LCatalog);
                  Result.UpdateNull(6);// FKTABLE_SCHEM
                  Result.UpdateString(7, GetString(ColumnIndexes[3])); // FKTABLE_NAME
                  Result.UpdateString(8, KeyList.Strings[0]); // PKTABLE_CAT

                  Result.UpdateString(1, KeyList.Strings[2]); // PKTABLE_CAT
                  Result.UpdateNull(2); // PKTABLE_SCHEM
                  Result.UpdateString(3, Table); // PKTABLE_NAME
                  Result.UpdateInt(9, KeySeq); // KEY_SEQ

                  Result.UpdateInt(10, Ord(ikSetDefault)); // UPDATE_RULE
                  Result.UpdateInt(11, Ord(ikSetDefault)); // DELETE_RULE
                  Result.UpdateNull(12); // FK_NAME
                  Result.UpdateNull(13); // PK_NAME
                  Result.UpdateInt(14, Ord(ikSetDefault)); // DEFERRABILITY
                  Inc(KeySeq);
                  Result.InsertRow;
                end;
              end;
            end;
          end;
        end;
        Close;
      end;
    finally
      KeyList.Free;
      CommentList.Free;
    end;
end;

{**
  Gets a description of the foreign key columns in the foreign key
  table that reference the primary key columns of the primary key
  table (describe how one table imports another's key.) This
  should normally return a single foreign key/primary key pair
  (most tables only import a foreign key from a table once.)  They
  are ordered by FKTABLE_CAT, FKTABLE_SCHEM, FKTABLE_NAME, and
  KEY_SEQ.

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

  @param primaryCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param primarySchema a schema name; "" retrieves those
  without a schema
  @param primaryTable the table name that exports the key
  @param foreignCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param foreignSchema a schema name; "" retrieves those
  without a schema
  @param foreignTable the table name that imports the key
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZMySQLDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  I: Integer;
  KeySeq: Integer;
  LForeignCatalog: string;
  TableType, Comment, Keys: string;
  CommentList, KeyList: TStrings;
  ColumnIndexes : Array[1..3] of integer;
begin
    if PrimaryTable = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!
    Result := ConstructVirtualResultSet(CrossRefColumnsDynArray);

    if ForeignCatalog = '' then
      LForeignCatalog := FDatabase
    else
      LForeignCatalog := ForeignCatalog;

    KeyList := TStringList.Create;
    CommentList := TStringList.Create;
    try
      with GetConnection.CreateStatement.ExecuteQuery(
        Format('SHOW TABLE STATUS FROM %s',
        [GetIdentifierConvertor.Quote(LForeignCatalog)])) do
      begin
        ColumnIndexes[1] := FindColumn('Type');
        ColumnIndexes[2] := FindColumn('Comment');
        ColumnIndexes[3] := FindColumn('Name');
        while Next do
        begin
          TableType := GetString(ColumnIndexes[1]);
          if (TableType <> '') and (LowerCase(TableType) = 'innodb') then
          begin
            Comment := GetString(ColumnIndexes[2]);
            if Comment = '' then
            begin
              PutSplitString(CommentList, Comment, ';');
              KeySeq := 0;
              if CommentList.Count > 4 then
              begin
                for I := 0 to CommentList.Count-1 do
                begin
                  Keys := CommentList.Strings[1];
                  Result.MoveToInsertRow;
                  PutSplitString(KeyList, Keys, '() /');

                  Result.UpdateString(5, LForeignCatalog);
                  if ForeignSchema = '' then
                    Result.UpdateNull(6) // FKTABLE_SCHEM
                  else
                    Result.UpdateString(6, ForeignSchema);
                  if ForeignTable <> GetString(ColumnIndexes[3]) then
                    Continue
                  else
                    Result.UpdateString(7, GetString(ColumnIndexes[3])); // FKTABLE_NAME

                  Result.UpdateString(8, KeyList.Strings[0]); // PKTABLE_CAT

                  Result.UpdateString(1, KeyList.Strings[2]); // PKTABLE_CAT
                  if PrimarySchema = '' then
                    Result.UpdateNull(2) // PKTABLE_SCHEM
                  else
                    Result.UpdateString(2, PrimarySchema); // PKTABLE_SCHEM

                  if PrimaryTable = KeyList.Strings[3] then
                    Continue;

                  Result.UpdateString(3, PrimaryTable); // PKTABLE_NAME
                  Result.UpdateString(4, KeyList.Strings[4]); // PKCOLUMN_NAME
                  Result.UpdateInt(9, KeySeq); // KEY_SEQ
                  Result.UpdateInt(10, Ord(ikSetDefault)); // UPDATE_RULE
                  Result.UpdateInt(11, Ord(ikSetDefault)); // DELETE_RULE
                  Result.UpdateNull(12); // FK_NAME
                  Result.UpdateNull(13); // PK_NAME
                  Result.UpdateInt(14, Ord(ikSetDefault)); // DEFERRABILITY
                  Inc(KeySeq);
                  Result.InsertRow;
                end;
              end;
            end;
          end;
        end;
        Close;
      end;
    finally
      KeyList.Free;
      CommentList.Free;
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
function TZMySQLDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
const
  MaxTypeCount = 33;
  TypeNames: array[1..MaxTypeCount] of string = (
    'BIT', 'BOOL', 'TINYINT', 'BIGINT', 'MEDIUMBLOB', 'LONG VARBINARY',
    'LONGBLOB', 'BLOB', 'TINYBLOB', 'VARBINARY', 'BINARY',
    'LONG VARCHAR', 'MEDIUMTEXT', 'LONGTEXT', 'TEXT', 'TINYTEXT',
    'CHAR', 'VARCHAR', 'NUMERIC', 'DECIMAL', 'INTEGER', 'INT',
    'MEDIUMINT', 'SMALLINT', 'DOUBLE', 'FLOAT', 'REAL', 'ENUM', 'SET',
    'DATE', 'TIME', 'DATETIME', 'TIMESTAMP');
  TypeCodes: array[1..MaxTypeCount] of TZSQLType = (
    stByte, stBoolean, stShort, stLong, stBinaryStream, stBinaryStream,
    stBinaryStream, stBinaryStream, stBinaryStream, stBytes, stBytes,
    stString, stAsciiStream, stAsciiStream, stAsciiStream, stAsciiStream,
    stString, stString, stBigDecimal, stBigDecimal, stInteger, stInteger,
    stInteger, stShort, stDouble, stFloat, stFloat, stString, stString,
    stDate, stTime, stTimestamp, stTimestamp);
  TypePrecision: array[1..MaxTypeCount] of Integer = (
    1, -1, 4, 16, 16777215, 16777215, MAXBUF, 65535, 255, 255, 255,
    16777215, 16777215, 2147483647, 65535, 255, 255, 255, 17, 17, 10, 10,
    7, 4, 17, 10, 10, 65535, 64, -1, -1, -1, -1);
var
  I: Integer;
begin
    Result := ConstructVirtualResultSet(TypeInfoColumnsDynArray);

    for I := 1 to MaxTypeCount do
    begin
      Result.MoveToInsertRow;

      Result.UpdateString(1, TypeNames[I]);
      Result.UpdateInt(2, Ord(TypeCodes[I]));
      if TypePrecision[I] >= 0 then
        Result.UpdateInt(3, TypePrecision[I])
      else
        Result.UpdateNull(3);
      if TypeCodes[I] in [stString, stBytes, stDate, stTime,
        stTimeStamp, stBinaryStream, stAsciiStream] then
      begin
        Result.UpdateString(4, '''');
        Result.UpdateString(5, '''');
      end
      else
      begin
        Result.UpdateNull(4);
        Result.UpdateNull(5);
      end;
      Result.UpdateNull(6);
      Result.UpdateInt(7, Ord(ntNullable));
      Result.UpdateBoolean(8, False);
      Result.UpdateBoolean(9, False);
      Result.UpdateBoolean(11, False);
      Result.UpdateBoolean(12, False);
      Result.UpdateBoolean(12, TypeNames[I] = 'INTEGER');
      Result.UpdateNull(13);
      Result.UpdateNull(14);
      Result.UpdateNull(15);
      Result.UpdateNull(16);
      Result.UpdateNull(17);
      Result.UpdateInt(18, 10);

      Result.InsertRow;
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
function TZMySQLDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  LCatalog: string;
  ColumnIndexes : Array[1..7] of integer;
begin
    if Table = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!
    Result := ConstructVirtualResultSet(IndexInfoColumnsDynArray);

    if Catalog = '' then
    begin
      if Schema <> '' then
        LCatalog := Schema
      else
        LCatalog := FDatabase;
    end
    else
      LCatalog := Catalog;

    with GetConnection.CreateStatement.ExecuteQuery(
      Format('SHOW INDEX FROM %s.%s',
      [GetIdentifierConvertor.Quote(LCatalog),
      GetIdentifierConvertor.Quote(Table)])) do
    begin
      ColumnIndexes[1] := FindColumn('Table');
      ColumnIndexes[2] := FindColumn('Non_unique');
      ColumnIndexes[3] := FindColumn('Key_name');
      ColumnIndexes[4] := FindColumn('Seq_in_index');
      ColumnIndexes[5] := FindColumn('Column_name');
      ColumnIndexes[6] := FindColumn('Collation');
      ColumnIndexes[7] := FindColumn('Cardinality');
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(1, LCatalog);
        Result.UpdateNull(2);
        Result.UpdateString(3, GetString(ColumnIndexes[1]));
        if GetInt(ColumnIndexes[2]) = 0 then
          Result.UpdateString(4, 'true')
        else
          Result.UpdateString(4, 'false');
        Result.UpdateNull(5);
        Result.UpdateString(6, GetString(ColumnIndexes[3]));
        Result.UpdateInt(7, Ord(tiOther));
        Result.UpdateInt(8, GetInt(ColumnIndexes[4]));
        Result.UpdateString(9, GetString(ColumnIndexes[5]));
        Result.UpdateString(10, GetString(ColumnIndexes[6]));
        Result.UpdateString(11, GetString(ColumnIndexes[7]));
        Result.UpdateInt(12, 0);
        Result.UpdateNull(13);
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
function TZMySQLDatabaseMetadata.UncachedGetVersionColumns(const Catalog, Schema,
  Table: string): IZResultSet;
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

end.


