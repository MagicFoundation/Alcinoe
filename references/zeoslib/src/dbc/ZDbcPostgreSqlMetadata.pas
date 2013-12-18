{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSqlMetadata;

interface

{$I ZDbc.inc}

uses
  Types, Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcMetadata,
  ZCompatibility, ZDbcPostgreSqlUtils, ZDbcConnection, ZSelectSchema;

type
  {** Implements a PostgreSQL Case Sensitive/Unsensitive identifier convertor. } 
  TZPostgreSQLIdentifierConvertor = class (TZDefaultIdentifierConvertor) 
  protected 
    function IsSpecialCase(const Value: string): Boolean; override; 
  public 
    function IsQuoted(const Value: string): Boolean; override; 
    function Quote(const Value: string): string; override; 
    function ExtractQuote(const Value: string): string; override; 
  end; 
 
  {**
    Database information interface for PostgreSQL. Adds some PostgreSQL-specific
     methods to IZDatabaseInfo.
  } // technobot 2008-06-27
  IZPostgreSQLDatabaseInfo = interface(IZDatabaseInfo)
    ['{7D48BBAA-FAE2-48EA-8B9E-663CCA5690EC}']
    // database and driver info:
    function HasMinimumServerVersion(MajorVersion: Integer;
      MinorVersion: Integer): Boolean;
  end;
  IZPostgreDBInfo = IZPostgreSQLDatabaseInfo; // shorthand alias

  // technobot 2008-06-27 - methods moved as is from TZPostgreSQLDatabaseMetadata:
  {** Implements PostgreSQL Database Information. }
  TZPostgreSQLDatabaseInfo = class(TZAbstractDatabaseInfo, IZPostgreSQLDatabaseInfo)
  protected
    function GetMaxIndexKeys: Integer;
    function GetMaxNameLength: Integer;
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
    function HasMinimumServerVersion(MajorVersion: Integer;
      MinorVersion: Integer): Boolean; // was TZPostgreSQLDatabaseMetadata.HaveMinimumServerVersion

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

  {** Implements PostgreSQL Database Metadata. }
  TZPostgreSQLDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    FDatabase: string;
    function EscapeString(const S: string): string;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-27

    // (technobot) should any of these be moved to TZPostgreSQLDatabaseInfo?:
    function GetPostgreSQLType(Oid: Integer): string;
    function GetSQLTypeByOid(Oid: Integer): TZSQLType;
    function GetSQLTypeByName(TypeName: string): TZSQLType;
    function TableTypeSQLExpression(TableType: string; UseSchemas: Boolean):
      string;
    procedure ParseACLArray(List: TStrings; AclString: string);
    function GetPrivilegeName(Permission: char): string;
    // (technobot) end of questioned section

    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetSchemas: IZResultSet; override;
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
    constructor Create(Connection: TZAbstractConnection; Url: string;
      Info: TStrings);
    destructor Destroy; override;
    function GetIdentifierConvertor: IZIdentifierConvertor; override; 
 end;

implementation

uses
  ZMessages, ZDbcUtils, ZDbcPostgreSql;

{ TZPostgreSQLDatabaseInfo }

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
}
constructor TZPostgreSQLDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata);
begin
  inherited;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLDatabaseInfo.Destroy;
begin
  inherited;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZPostgreSQLDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'PostgreSQL';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZPostgreSQLDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZPostgreSQLDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for PostgreSQL';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZPostgreSQLDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZPostgreSQLDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Returns the server version
  @return the server version string
}
function TZPostgreSQLDatabaseInfo.GetServerVersion: string;
begin
  with Metadata.GetConnection as IZPostgreSQLConnection do
    Result := Format('%s.%s', [GetServerMajorVersion, GetServerMinorVersion]);
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZPostgreSQLDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetSQLKeywords: string;
begin
  Result := 'abort,absolute,access,action,add,admin,after,aggregate,all,also,'+
  			'alter,always,analyse,analyze,and,any,array,asc,assertion,assignment,'+
            'asymmetric,at,authorization,'+
            'backward,before,begin,between,bigint,binary,bit,boolean,both,'+
            'cache,called,cascade,cascaded,case,cast,catalog,chain,character,'+
            'characteristics,check,checkpoint,class,close,cluster,coalesce,'+
            'collate,column,comment,commit,committed,concurrently,configuration,'+
            'connect,connection,constraint,constraints,content,continue,'+
            'conversion,convert,copy,cost,createdb,createrole,createuser,cross,'+
            'csv,current,current_catalog,current_date,current_role,current_schema,'+
            'current_time,current_timestamp,current_user,cursor,cycle,'+
            'data,database,day,deallocate,dec,decimal,declare,default,defaults,'+
            'deferrable,deferred,definer,delimiter,delimiters,desc,dictionary,'+
            'disable,discard,distinct,do,document,domain,double,'+
            'each,else,enable,encoding,encrypted,end,end-exec,enum,escape,except,'+
            'excluding,exclusive,exec,execute,exists,explain,external,extract,'+
            'false,family,fetch,first,float,following,for,force,foreign,forward,'+
            'freeze,full,function,'+
            'global,grant,granted,greatest,'+
            'handler,header,hold,hour,'+
            'identity,if,ilike,immediate,immutable,implicit,in,including,'+
            'increment,indexes,inherit,inherits,initially,inner,inout,input,'+
            'insensitive,instead,int,intersect,interval,invoker,isnull,isolation,'+
            'join,'+
            'lancompiler,language,large,last,lc_collate,lc_ctype,leading,least,'+
            'left,level,like,limit,listen,load,local,localtime,localtimestamp,'+
            'location,lock,login,'+
            'mapping,match,maxvalue,minute,minvalue,mode,month,move,'+
            'name,names,national,natural,nchar,new,next,no,nocreatedb,nocreaterole,'+
            'nocreateuser,noinherit,nologin,none,nosuperuser,not,nothing,notify,'+
            'notnull,nowait,nullif,nulls,numeric,'+
            'object,of,off,offset,oids,old,only,operator,option,options,or,out,'+
            'outer,over,overlaps,overlay,owned,owner,'+
            'parser,partial,partition,password,placing,plans,position,preceding,'+
            'precision,prepare,prepared,preserve,prior,privileges,procedural,'+
            'procedure,'+
            'quote,'+
            'range,read,real,reassign,recheck,recursive,references,reindex,'+
            'relative,release,rename,repeatable,replace,replica,reset,restart,'+
            'restrict,return,returning,returns,revoke,right,role,rollback,row,'+
            'rows,rule,'+
            'savepoint,schema,scroll,search,second,security,sequence,serializable,'+
            'server,session,session_user,setof,share,show,similar,simple,smallint,'+
            'some,stable,standalone,start,statement,statistics,stdin,stdout,'+
            'storage,strict,strip,substring,superuser,symmetric,sysid,system,'+
            'tablespace,temp,template,temporary,text,then,time,timestamp,to,'+
            'trailing,transaction,treat,trigger,trim,true,truncate,trusted,type,'+
            'unbounded,uncommitted,unencrypted,union,unique,unknown,unlisten,'+
            'until,user,using,'+
            'vacuum,valid,validator,value,variadic,varying,verbose,version,view,'+
            'volatile,'+
            'when,whitespace,window,with,without,work,wrapper,write,'+
            'xml,xmlattributes,xmlconcat,xmlelement,xmlforest,xmlparse,xmlpi,'+
            'xmlroot,xmlserialize,'+
            'year,yes,'+
            'zone';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetNumericFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetStringFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetSystemFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetTimeDateFunctions: string;
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
function TZPostgreSQLDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '\';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZPostgreSQLDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := True;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := HasMinimumServerVersion(6, 4);
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := HasMinimumServerVersion(6, 4);
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := HasMinimumServerVersion(6, 4);
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZPostgreSQLDatabaseInfo.GetSchemaTerm: string;
begin
  Result := 'schema';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZPostgreSQLDatabaseInfo.GetProcedureTerm: string;
begin
  Result := 'function';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZPostgreSQLDatabaseInfo.GetCatalogTerm: string;
begin
  Result := 'database';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZPostgreSQLDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '.';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := False;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := False;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := HasMinimumServerVersion(6, 5);
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := HasMinimumServerVersion(7, 1);
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := HasMinimumServerVersion(7, 1);
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZPostgreSQLDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZPostgreSQLDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZPostgreSQLDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZPostgreSQLDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
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
function TZPostgreSQLDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := GetMaxIndexKeys;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 1600;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 8192;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxRowSize: Integer;
begin
  if HasMinimumServerVersion(7, 1) then
    Result := 1073741824
  else Result := 8192;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := True;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxStatementLength: Integer;
begin
  if HasMinimumServerVersion(7, 0) then
    Result := 0
  else Result := 16348
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 1;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZPostgreSQLDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZPostgreSQLDatabaseInfo.SupportsTransactionIsolationLevel(
  Level: TZTransactIsolationLevel): Boolean;
begin
  Result := (Level = tiSerializable) or (Level = tiReadCommitted);
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := False;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := False;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsResultSetType(
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
function TZPostgreSQLDatabaseInfo.SupportsResultSetConcurrency(
  _Type: TZResultSetType; Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := (_Type = rtScrollInsensitive) and (Concurrency = rcReadOnly);
end;

//----------------------------------------------------------------------
// Additional functions.

function TZPostgreSQLDatabaseInfo.HasMinimumServerVersion(
  MajorVersion: Integer; MinorVersion: Integer): Boolean;
var
  PostgreSQLConnection: IZPostgreSQLConnection;
begin
  PostgreSQLConnection := Metadata.GetConnection as IZPostgreSQLConnection;
  Result := (MajorVersion < PostgreSQLConnection.GetServerMajorVersion)
    or ((MajorVersion = PostgreSQLConnection.GetServerMajorVersion)
    and (MinorVersion <= PostgreSQLConnection.GetServerMinorVersion));
end;

function TZPostgreSQLDatabaseInfo.GetMaxIndexKeys: Integer;
var
  SQL, From: string;
begin
  if HasMinimumServerVersion(7, 3) then
  begin
    From := ' pg_catalog.pg_namespace n, pg_catalog.pg_type t1,'
      + ' pg_catalog.pg_type t2 WHERE t1.typnamespace=n.oid'
      + ' AND n.nspname=''pg_catalog'' AND ';
  end else
    From := ' pg_type t1, pg_type t2 WHERE ';
  SQL := ' SELECT t1.typlen/t2.typlen FROM ' + From
    + ' t1.typelem=t2.oid AND t1.typname=''oidvector'' ';

  with Metadata.GetConnection.CreateStatement.ExecuteQuery(SQL) do
  begin
    if not Next then
      raise Exception.Create(SUnknownError); //CHANGE IT!
    Result := GetInt(1);
    Close;
  end;
end;

function TZPostgreSQLDatabaseInfo.GetMaxNameLength: Integer;
var
  SQL: string;
begin
  if HasMinimumServerVersion(7, 3) then
  begin
    SQL := ' SELECT t.typlen FROM pg_catalog.pg_type t,'
      + ' pg_catalog.pg_namespace n WHERE t.typnamespace=n.oid'
      + ' AND t.typname=''name'' AND n.nspname=''pg_catalog'' ';
  end else
    SQL := ' SELECT typlen FROM pg_type WHERE typname=''name'' ';

  with Metadata.GetConnection.CreateStatement.ExecuteQuery(SQL) do
  begin
    if not Next then
      raise Exception.Create(SUnknownError); //CHANGE IT!
    Result := GetIntByName('typlen');
    Close;
  end;
end;


{ TZPostgreSQLDatabaseMetadata }


{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Url a database connection url string.
  @param Info an extra connection properties.
}
constructor TZPostgreSQLDatabaseMetadata.Create(
  Connection: TZAbstractConnection; Url: string; Info: TStrings);
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
destructor TZPostgreSQLDatabaseMetadata.Destroy;
begin
  inherited Destroy;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZPostgreSQLDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZPostgreSQLDatabaseInfo.Create(Self);
end;

{**
  @param S a string.
  @return escaped string
}
function TZPostgreSQLDatabaseMetadata.EscapeString(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if (Result[I] = '''') or (Result[I] = '\') then
      Insert('\', Result, I);
  Result := '''' + Result + '''';
  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(8, 1) then
    Result := 'E' + Result;
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
function TZPostgreSQLDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  SQL, LProcedureNamePattern: string;
begin
    if ProcedureNamePattern = '' then
      LProcedureNamePattern := '%'
    else
      LProcedureNamePattern := ProcedureNamePattern;

    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := 'SELECT NULL AS PROCEDURE_CAT, n.nspname AS PROCEDURE_SCHEM,'
        + ' p.proname AS PROCEDURE_NAME, NULL AS RESERVED1, NULL AS RESERVED2,'
        + ' NULL AS RESERVED3, d.description AS REMARKS, '
        + IntToStr(ProcedureReturnsResult) + ' AS PROCEDURE_TYPE '
        + ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_proc p  '
        + ' LEFT JOIN pg_catalog.pg_description d ON (p.oid=d.objoid) '
        + ' LEFT JOIN pg_catalog.pg_class c ON (d.classoid=c.oid AND'
        + ' c.relname=''pg_proc'') LEFT JOIN pg_catalog.pg_namespace pn ON'
        + ' (c.relnamespace=pn.oid AND pn.nspname=''pg_catalog'') '
        + ' WHERE p.pronamespace=n.oid';
      if SchemaPattern <> '' then
        SQL := SQL + ' AND n.nspname LIKE ' + EscapeString(SchemaPattern);
      SQL := SQL + ' AND p.proname LIKE ' + EscapeString(LProcedureNamePattern)
        + ' ORDER BY PROCEDURE_SCHEM, PROCEDURE_NAME';
    end
    else
    begin
      SQL := 'SELECT NULL AS PROCEDURE_CAT, NULL AS PROCEDURE_SCHEM,'
        + ' p.proname AS PROCEDURE_NAME, NULL AS RESERVED1, NULL AS RESERVED2,'
        + ' NULL AS RESERVED3, NULL AS REMARKS, '
        + IntToStr(ProcedureReturnsResult) + ' AS PROCEDURE_TYPE'
        + ' FROM pg_proc p WHERE p.proname LIKE '
        + EscapeString(LProcedureNamePattern)
        + ' ORDER BY PROCEDURE_NAME';
    end;

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(ProceduresColumnsDynArray));
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
function TZPostgreSQLDatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  I, ReturnType, ColumnTypeOid, ArgOid: Integer;
  SQL, ReturnTypeType: string;
  ArgTypes: TStrings;
  ResultSet,
  ColumnsRS: IZResultSet;
begin
    Result := ConstructVirtualResultSet(ProceduresColColumnsDynArray);

    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := 'SELECT n.nspname,p.proname,p.prorettype,p.proargtypes,t.typtype,'
        + 't.typrelid FROM pg_catalog.pg_proc p, pg_catalog.pg_namespace n,'
        + ' pg_catalog.pg_type t WHERE p.pronamespace=n.oid AND p.prorettype=t.oid';
      if SchemaPattern <> '' then
        SQL := SQL + ' AND n.nspname LIKE ' + EscapeString(SchemaPattern);
      SQL := SQL + ' AND p.proname LIKE ' + EscapeString(ToLikeString(ProcedureNamePattern))
        + ' ORDER BY n.nspname, p.proname';
    end
    else
      SQL := 'SELECT NULL AS nspname,p.proname,p.prorettype,p.proargtypes,'
        + 't.typtype,t.typrelid FROM pg_proc p, pg_type t'
        + ' WHERE p.prorettype=t.oid'
        + ' AND p.proname LIKE '
        +   EscapeString(ToLikeString(ProcedureNamePattern))
        + ' ORDER BY p.proname';

    ArgTypes := TStringList.Create;
    try
      ResultSet:=GetConnection.CreateStatement.ExecuteQuery(SQL); //FirmOS Patch
      with ResultSet do
      begin
        while Next do
        begin
          ReturnType := StrToInt(GetStringByName('prorettype'));
          ReturnTypeType := GetStringByName('typtype');
          PutSplitString(ArgTypes, GetStringByName('proargtypes'), #10#13#9' ');

          if ReturnTypeType <> 'c' then
          begin
            Result.MoveToInsertRow;
            Result.UpdateNull(1);
            Result.UpdateString(2, GetStringByName('nspname'));
            Result.UpdateString(3, GetStringByName('proname'));
            Result.UpdateString(4, 'returnValue');
            Result.UpdateInt(5, Ord(pctReturn));
            Result.UpdateInt(6, Ord(GetSQLTypeByOid(ReturnType)));
            Result.UpdateString(7, GetPostgreSQLType(ReturnType));
            Result.UpdateNull(8);
            Result.UpdateNull(9);
            Result.UpdateNull(10);
            Result.UpdateNull(11);
            Result.UpdateInt(12, Ord(ntNullableUnknown));
            Result.UpdateNull(13);
            Result.InsertRow;
          end;

          for I := 0 to ArgTypes.Count-1 do
          begin
            ArgOid := StrToInt(ArgTypes.Strings[i]);
            Result.MoveToInsertRow;
            Result.UpdateNull(1);
            Result.UpdateString(2, GetStringByName('nspname'));
            Result.UpdateString(3, GetStringByName('proname'));
            Result.UpdateString(4, '$' + IntToStr(I));
            Result.UpdateInt(5, Ord(pctIn));
            Result.UpdateInt(6, Ord(GetSQLTypeByOid(ArgOid)));
            Result.UpdateString(7, GetPostgreSQLType(ArgOid));
            Result.UpdateNull(8);
            Result.UpdateNull(9);
            Result.UpdateNull(10);
            Result.UpdateNull(11);
            Result.UpdateInt(12, Ord(ntNullableUnknown));
            Result.UpdateNull(13);
            Result.InsertRow;
          end;

          if ReturnTypeType = 'c' then
          begin
            ColumnsRS := GetConnection.CreateStatement.ExecuteQuery(
              Format('SELECT a.attname,a.atttypid'
                + ' FROM pg_catalog.pg_attribute a WHERE a.attrelid=%s'
                + ' ORDER BY a.attnum',
                [ResultSet.GetStringByName('typrelid')]));
            while ColumnsRS.Next do
            begin
              ColumnTypeOid := ColumnsRS.GetIntByName('atttypid');
              Result.MoveToInsertRow;
              Result.UpdateNull(1);
              Result.UpdateString(2, GetStringByName('nspname'));
              Result.UpdateString(3, GetStringByName('proname'));
              Result.UpdateString(4, ColumnsRS.GetStringByName('attname'));
              Result.UpdateInt(5, Ord(pctResultSet));
              Result.UpdateInt(6, Ord(GetSQLTypeByOid(ColumnTypeOid)));
              Result.UpdateString(7, GetPostgreSQLType(ColumnTypeOid));
              Result.UpdateNull(8);
              Result.UpdateNull(9);
              Result.UpdateNull(10);
              Result.UpdateNull(11);
              Result.UpdateInt(12, Ord(ntNullableUnknown));
              Result.UpdateNull(13);
              Result.InsertRow;
            end;
            ColumnsRS.Close;
          end;
        end;
        Close;
      end;
    finally
      ArgTypes.Free;
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
function TZPostgreSQLDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  I: Integer;
  TableType, OrderBy, SQL: string;
  UseSchemas: Boolean;
  LTypes: TStringDynArray;
begin
    UseSchemas := True;

    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := ' SELECT NULL AS TABLE_CAT, n.nspname AS TABLE_SCHEM,'
        + ' c.relname AS TABLE_NAME,  '
        + ' CASE (n.nspname LIKE ''pg\\_%'')'
        + '   OR (n.nspname=''information_schema'')'
        + ' WHEN true THEN CASE n.nspname '
        + '   WHEN ''pg_catalog'' THEN CASE c.relkind '
        + '     WHEN ''r'' THEN ''SYSTEM TABLE'''
        + '     WHEN ''v'' THEN ''SYSTEM VIEW'' '
        + '     WHEN ''i'' THEN ''SYSTEM INDEX'' '
        + '     ELSE NULL '
        + '   END '
        + '   WHEN ''information_schema'' THEN CASE c.relkind '
        + '     WHEN ''r'' THEN ''SYSTEM TABLE'''
        + '     WHEN ''v'' THEN ''SYSTEM VIEW'' '
        + '     WHEN ''i'' THEN ''SYSTEM INDEX'' '
        + '     ELSE NULL '
        + '   END '
        + '   WHEN ''pg_toast'' THEN CASE c.relkind '
        + '     WHEN ''r'' THEN ''SYSTEM TOAST TABLE'' '
        + '     WHEN ''i'' THEN ''SYSTEM TOAST INDEX'' '
        + '     ELSE NULL '
        + '   END '
        + '   ELSE CASE c.relkind '
        + '	WHEN ''r'' THEN ''TEMPORARY TABLE'' '
        + '	WHEN ''i'' THEN ''TEMPORARY INDEX'' '
        + '	ELSE NULL '
        + '   END '
        + ' END '
        + ' WHEN false THEN CASE c.relkind '
        + '   WHEN ''r'' THEN ''TABLE'' '
        + '   WHEN ''i'' THEN ''INDEX'' '
        + '   WHEN ''S'' THEN ''SEQUENCE'' '
        + '   WHEN ''v'' THEN ''VIEW'' '
        + '   ELSE NULL '
        + ' END '
        + ' ELSE NULL '
        + ' END '
        + ' AS TABLE_TYPE, d.description AS REMARKS '
        + ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class c '
        + ' LEFT JOIN pg_catalog.pg_description d'
        + ' ON (c.oid = d.objoid AND d.objsubid = 0) '
        + ' LEFT JOIN pg_catalog.pg_class dc ON (d.classoid=dc.oid'
        + ' AND dc.relname=''pg_class'') LEFT JOIN pg_catalog.pg_namespace dn'
        + ' ON (dn.oid=dc.relnamespace AND dn.nspname=''pg_catalog'') '
        + ' WHERE c.relnamespace = n.oid ';
      //if SchemaPattern <> '' then // cannot happen due to SchemaPattern := '%'
      begin
        SQL := SQL + ' AND n.nspname LIKE '
          + EscapeString(ToLikeString(SchemaPattern));
      end;
      OrderBy := ' ORDER BY TABLE_TYPE,TABLE_SCHEM,TABLE_NAME';
    end
    else
    begin
      UseSchemas := False;
      TableType := ' CASE c.relname LIKE ''pg\\_%'' '
        + 'WHEN true THEN CASE c.relname LIKE ''pg\\_toast\\_%'' '
        + 'WHEN true THEN CASE c.relkind '
        + '  WHEN ''r'' THEN ''SYSTEM TOAST TABLE'' '
        + '  WHEN ''i'' THEN ''SYSTEM TOAST INDEX'' '
        + '  ELSE NULL '
        + 'END '
        + 'WHEN false THEN CASE c.relname LIKE ''pg\\_temp\\_%'' '
        + '  WHEN true THEN CASE c.relkind '
        + '    WHEN ''r'' THEN ''TEMPORARY TABLE'' '
        + '    WHEN ''i'' THEN ''TEMPORARY INDEX'' '
        + '    ELSE NULL '
        + '  END '
        + '  WHEN false THEN CASE c.relkind '
        + '    WHEN ''r'' THEN ''SYSTEM TABLE'' '
        + '    WHEN ''v'' THEN ''SYSTEM VIEW'' '
        + '    WHEN ''i'' THEN ''SYSTEM INDEX'' '
        + '    ELSE NULL '
        + '  END '
        + '  ELSE NULL '
        + 'END '
        + 'ELSE NULL '
        + 'END '
        + 'WHEN false THEN CASE c.relkind '
        + '  WHEN ''r'' THEN ''TABLE'' '
        + '  WHEN ''i'' THEN ''INDEX'' '
        + '  WHEN ''S'' THEN ''SEQUENCE'' '
        + '  WHEN ''v'' THEN ''VIEW'' '
        + '  ELSE NULL '
        + 'END '
        + 'ELSE NULL '
        + ' END ';
      OrderBy := ' ORDER BY TABLE_TYPE,TABLE_NAME ';
      SQL := 'SELECT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM,'
        + ' c.relname AS TABLE_NAME, ' + TableType + ' AS TABLE_TYPE,'
        + ' NULL AS REMARKS FROM pg_class c WHERE true ';
    end;

    if (Types = nil) or (Length(Types) = 0) then
    begin
      SetLength(LTypes, 3);
      // SetLength(LTypes, 6);
      LTypes[0] := 'TABLE';
      LTypes[1] := 'VIEW';
      LTypes[2] := 'TEMPORARY TABLE';
      // LTypes[3] := 'SYSTEM TABLE';
      // LTypes[4] := 'SYSTEM TOAST TABLE';
      // LTypes[5] := 'SYSTEM VIEW';
    end
    else
      LTypes := Types;

    SQL := SQL + ' AND c.relname LIKE ' + EscapeString(ToLikeString(TableNamePattern))
      + ' AND (false';
    for I := 0 to High(LTypes) do
      SQL := SQL + ' OR (' + TableTypeSQLExpression(LTypes[i], UseSchemas) + ')';
    SQL := SQL + ')' + OrderBy;

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
function TZPostgreSQLDatabaseMetadata.UncachedGetSchemas: IZResultSet;
var
  SQL: string;
begin
    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := 'SELECT nspname AS TABLE_SCHEM FROM pg_catalog.pg_namespace'
        + ' WHERE nspname <> ''pg_toast'' AND nspname NOT'
        + ' LIKE ''pg\\_temp\\_%'' ORDER BY TABLE_SCHEM';
    end else
      SQL := 'SELECT ''''::text AS TABLE_SCHEM ORDER BY TABLE_SCHEM';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(SchemaColumnsDynArray));
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
function TZPostgreSQLDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
var
  SQL: string;
begin
    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := 'SELECT datname AS TABLE_CAT FROM pg_catalog.pg_database'
        + ' ORDER BY TABLE_CAT';
    end else
      SQL := 'SELECT datname AS TABLE_CAT FROM pg_database ORDER BY TABLE_CAT';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(CatalogColumnsDynArray));
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
function TZPostgreSQLDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  Types: array [0..10] of string = ('TABLE', 'VIEW', 'INDEX',
    'SEQUENCE', 'SYSTEM TABLE', 'SYSTEM TOAST TABLE',
    'SYSTEM TOAST INDEX', 'SYSTEM VIEW', 'SYSTEM INDEX',
    'TEMPORARY TABLE', 'TEMPORARY INDEX');
var
  I: Integer;
begin
    Result := ConstructVirtualResultSet(TableTypeColumnsDynArray);
    for I := 0 to 10 do
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
function TZPostgreSQLDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
{const
  VARHDRSZ = 4;
}var
  TypeOid, AttTypMod: Integer;
  SQL, PgType: string;
begin
    Result := ConstructVirtualResultSet(TableColColumnsDynArray);

    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := 'SELECT n.nspname,' {1}
        + 'c.relname,' {2}
        + 'a.attname,' {3}
        + 'a.atttypid,' {4}
        + 'a.attnotnull,' {5}
        + 'a.atttypmod,' {6}
        + 'a.attlen,' {7}
        + 'a.attnum,' {8}
        + 'pg_get_expr(def.adbin, def.adrelid) as adsrc,' {9}
        + 'dsc.description ' {10}
        + ' FROM pg_catalog.pg_namespace n '
        + ' JOIN pg_catalog.pg_class c ON (c.relnamespace = n.oid) '
        + ' JOIN pg_catalog.pg_attribute a ON (a.attrelid=c.oid) '
        + ' LEFT JOIN pg_catalog.pg_attrdef def ON (a.attrelid=def.adrelid'
        + ' AND a.attnum = def.adnum) LEFT JOIN pg_catalog.pg_description dsc'
        + ' ON (c.oid=dsc.objoid AND a.attnum = dsc.objsubid) '
        + ' LEFT JOIN pg_catalog.pg_class dc ON (dc.oid=dsc.classoid'
        + ' AND dc.relname=''pg_class'') LEFT JOIN pg_catalog.pg_namespace dn'
        + ' ON (dc.relnamespace=dn.oid AND dn.nspname=''pg_catalog'') '
        + ' WHERE a.attnum > 0 AND NOT a.attisdropped';
      if SchemaPattern <> '' then
      begin
        SQL := SQL + ' AND n.nspname LIKE '
          + EscapeString(SchemaPattern);
      end;
    end
    else
    begin
      SQL := 'SELECT NULL::text AS nspname,' {1}
        + 'c.relname,' {2}
        + 'a.attname,' {3}
        + 'a.atttypid,' {4}
        + 'a.attnotnull,' {5}
        + 'a.atttypmod,' {6}
        + 'a.attlen,' {7}
        + 'a.attnum,' {8}
        + 'NULL AS adsrc,' {9}
        + 'NULL AS description' {10}
        + 'FROM pg_class c, pg_attribute a '
        + ' WHERE a.attrelid=c.oid AND a.attnum > 0 ';
    end;

    SQL := SQL + ' AND c.relname LIKE ' + EscapeString(ToLikeString(TableNamePattern))
      + ' AND a.attname LIKE ' + EscapeString(ToLikeString(ColumnNamePattern))
      + ' ORDER BY nspname,relname,attnum';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        AttTypMod := GetInt(6 {atttypmod});

        TypeOid := GetInt(4 {atttypid});
        PgType := GetPostgreSQLType(TypeOid);

        Result.MoveToInsertRow;
        Result.UpdateNull(1);
        Result.UpdateString(2, GetString(1 {nspname}));
        Result.UpdateString(3, GetString(2 {relname}));
        Result.UpdateString(4, GetString(3 {attname})); 
        Result.UpdateInt(5, Ord(GetSQLTypeByOid(TypeOid)));
        Result.UpdateString(6, PgType);
        Result.UpdateInt(8, 0);

        if (PgType = 'bpchar') or (PgType = 'varchar') or (PgType = 'enum') then
        begin
          if AttTypMod <> -1 then
            Result.UpdateInt(7, AttTypMod - 4)
          else Result.UpdateInt(7, 0);
        end
        else if (PgType = 'numeric') or (PgType = 'decimal') then
        begin
          Result.UpdateInt(7, ((AttTypMod - 4) div 65536)); //precision
          Result.UpdateInt(9, ((AttTypMod -4) mod 65536)); //scale
          Result.UpdateInt(10, 10); //base? ten as default
        end
        else if (PgType = 'bit') or (PgType = 'varbit') then
        begin
          Result.UpdateInt(7, AttTypMod);
          Result.UpdateInt(10, 2);
        end
        else
        begin
          Result.UpdateInt(7, GetInt(7 {attlen}));
          Result.UpdateInt(10, 2);
        end;

        Result.UpdateNull(8);
        if GetBoolean(5 {attnotnull}) then
        begin
          Result.UpdateString(18, 'NO');
          Result.UpdateInt(11, Ord(ntNoNulls));
        end
        else
        begin
          Result.UpdateString(18, 'YES');
          Result.UpdateInt(11, Ord(ntNullable));
        end;

        Result.UpdateString(12, GetString(10 {description}));
        Result.UpdateString(13, GetString(9 {adsrc}));
        Result.UpdateNull(14);
        Result.UpdateNull(15);
        Result.UpdateInt(16, Result.GetInt(7));
        Result.UpdateInt(17, GetInt(8 {attnum}));

        Result.UpdateNullByName('AUTO_INCREMENT');
        Result.UpdateBooleanByName('CASE_SENSITIVE',
          GetIdentifierConvertor.IsCaseSensitive(GetString(3 {attname})));
        Result.UpdateBooleanByName('SEARCHABLE', True);
        Result.UpdateBooleanByName('WRITABLE', True);
        Result.UpdateBooleanByName('DEFINITELYWRITABLE', True);
        Result.UpdateBooleanByName('READONLY', False);

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
function TZPostgreSQLDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  I, J: Integer;
  SQL, Column, Owner: string;
  Privileges, Grantable, Grantee: string;
  Permissions, PermissionsExp: TStrings;
begin
    Result := ConstructVirtualResultSet(TableColPrivColumnsDynArray);

    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := 'SELECT n.nspname,c.relname,u.usename,c.relacl,a.attname '
        + ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class c,'
        + ' pg_catalog.pg_user u, pg_catalog.pg_attribute a '
        + ' WHERE c.relnamespace = n.oid AND u.usesysid = c.relowner '
        + ' AND c.oid = a.attrelid AND c.relkind = ''r'''
        + ' AND a.attnum > 0 AND NOT a.attisdropped';
      if Schema <> '' then
        SQL := SQL + ' AND n.nspname = ' + EscapeString(Schema);
    end
    else
    begin
      SQL := 'SELECT NULL::text AS nspname,c.relname,u.usename,c.relacl,'
        + 'a.attname FROM pg_class c, pg_user u,pg_attribute a '
        + ' WHERE u.usesysid = c.relowner AND c.oid = a.attrelid '
        + ' AND a.attnum > 0 AND c.relkind = ''r''';
    end;

    SQL := SQL + ' AND c.relname = ' + EscapeString(Table)
      + ' AND a.attname LIKE ' + EscapeString(ToLikeString(ToLikeString(ColumnNamePattern)))
      + ' ORDER BY attname';

    Permissions := TStringList.Create;
    PermissionsExp := TStringList.Create;
    try
      with GetConnection.CreateStatement.ExecuteQuery(SQL) do
      begin
        while Next do
        begin
          //SchemaName := GetStringByName('nspname');
          //TableName := GetStringByName('relname');
          Column := GetStringByName('attname');
          Owner := GetStringByName('usename');
          Permissions.Clear;
          ParseACLArray(Permissions, GetStringByName('relacl'));
          for I := 0 to Permissions.Count-1 do
          begin
            PutSplitString(PermissionsExp, Permissions.Strings[I], '=');
            if PermissionsExp.Count < 2 then
              Continue;
            Grantee := PermissionsExp.Strings[0];
            if Grantee = '' then
              Grantee := 'PUBLIC';
            Privileges := PermissionsExp.Strings[1];
            for J := 1 to Length(Privileges) do
            begin
              if Owner = Grantee then
                Grantable := 'YES'
              else Grantable := 'NO';
              Result.MoveToInsertRow;
              Result.UpdateNull(1);
              Result.UpdateString(2, Schema);
              Result.UpdateString(3, Table);
              Result.UpdateString(4, Column);
              Result.UpdateString(5, Owner);
              Result.UpdateString(6, Grantee);
              Result.UpdateString(7, GetPrivilegeName(Privileges[J]));
              Result.UpdateString(8, grantable);
              Result.InsertRow;
            end;
          end;
        end;
        Close;
      end;
    finally
      Permissions.Free;
      PermissionsExp.Free;
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
function TZPostgreSQLDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  I, J: Integer;
  SQL, SchemaName, TableName, Owner: string;
  Privileges, Grantable, Grantee: string;
  Permissions, PermissionsExp: TStringList;
begin
    Result := ConstructVirtualResultSet(TablePrivColumnsDynArray);

    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := 'SELECT n.nspname,c.relname,u.usename,c.relacl '
        + ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class c,'
        + ' pg_catalog.pg_user u WHERE c.relnamespace = n.oid '
        + ' AND u.usesysid = c.relowner AND c.relkind = ''r'' ';
      if SchemaPattern <> '' then
        SQL := SQL + ' AND n.nspname LIKE ' + EscapeString(SchemaPattern);
    end
    else
    begin
      SQL := 'SELECT NULL::text AS nspname,c.relname,u.usename,c.relacl '
        + ' FROM pg_class c, pg_user u WHERE u.usesysid = c.relowner '
        + ' AND c.relkind = ''r'' ';
    end;

    SQL := SQL + ' AND c.relname LIKE ' + EscapeString(ToLikeString(TableNamePattern))
      + ' ORDER BY nspname, relname';

    Permissions := TStringList.Create;
    PermissionsExp := TStringList.Create;
    try
      with GetConnection.CreateStatement.ExecuteQuery(SQL) do
      begin
        while Next do
        begin
          SchemaName := GetStringByName('nspname');
          TableName := GetStringByName('relname');
          Owner := GetStringByName('usename');
          SchemaName := GetStringByName('nspname');
          Permissions.Clear;
          ParseACLArray(Permissions, GetStringByName('relacl'));
          Permissions.Sort;
          for I := 0 to Permissions.Count-1 do
          begin
            PutSplitString(PermissionsExp, Permissions.Strings[I], '=');
            if PermissionsExp.Count < 2 then
              Continue;
            Grantee := PermissionsExp.Strings[0];
            if Grantee = '' then
            Grantee := 'PUBLIC';
            Privileges := PermissionsExp.Strings[1];
            for J := 1 to Length(Privileges) do
            begin
              if Owner = Grantee then
                Grantable := 'YES'
              else Grantable := 'NO';
              Result.MoveToInsertRow;
              Result.UpdateNull(1);
              Result.UpdateString(2, SchemaName);
              Result.UpdateString(3, TableName);
              Result.UpdateString(4, Owner);
              Result.UpdateString(5, Grantee);
              Result.UpdateString(6, GetPrivilegeName(Privileges[J]));
              Result.UpdateString(7, grantable);
              Result.InsertRow;
            end;
          end;
        end;
        Close;
      end;
    finally
      Permissions.Free;
      PermissionsExp.Free;
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
function TZPostgreSQLDatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(TableColVerColumnsDynArray);

    Result.MoveToInsertRow;
    Result.UpdateNull(1);
    Result.UpdateString(2, 'ctid');
    Result.UpdateInt(3, Ord(GetSQLTypeByName('tid')));
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
function TZPostgreSQLDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  SQL, Select, From, Where: string;
begin
    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      Select := 'SELECT NULL AS TABLE_CAT, n.nspname AS TABLE_SCHEM,';
      From := ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class ct,'
        + ' pg_catalog.pg_class ci, pg_catalog.pg_attribute a,'
        + ' pg_catalog.pg_index i';
      Where := ' AND ct.relnamespace = n.oid';
      if Schema <> '' then
        Where := Where + ' AND n.nspname = ' + EscapeString(Schema);
    end
    else
    begin
      Select := 'SELECT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM,';
      From := ' FROM pg_class ct, pg_class ci, pg_attribute a, pg_index i';
    end;
    SQL := Select + ' ct.relname AS TABLE_NAME, a.attname AS COLUMN_NAME,'
      + ' a.attnum AS KEY_SEQ, ci.relname AS PK_NAME'
      + From
      + ' WHERE ct.oid=i.indrelid AND ci.oid=i.indexrelid'
      + ' AND a.attrelid=ci.oid AND i.indisprimary';
    if Table <> '' then
       SQL := SQL + ' AND ct.relname = ' + EscapeString(Table);
    SQL := SQL + Where + ' ORDER BY table_name, pk_name, key_seq';

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
function TZPostgreSQLDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference('', '', '', Catalog, Schema, Table);
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
function TZPostgreSQLDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference(Catalog, Schema, Table, '', '', '');
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
function TZPostgreSQLDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  SQL, Select, From, Where: string;
  DeleteRule, UpdateRule, Rule: string;
  {FKeyName, }FKeyColumn, PKeyColumn, Targs: string;
  Action, KeySequence, Advance: Integer;
  List: TStrings;
  Deferrability: Integer;
  Deferrable, InitiallyDeferred: Boolean;
begin
    Result := ConstructVirtualResultSet(CrossRefColumnsDynArray);

    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      Select := 'SELECT DISTINCT n1.nspname as pnspname,n2.nspname as fnspname,';
      From := ' FROM pg_catalog.pg_namespace n1 JOIN pg_catalog.pg_class c1'
        + ' ON (c1.relnamespace = n1.oid) JOIN pg_catalog.pg_index i'
        + ' ON (c1.oid=i.indrelid) JOIN pg_catalog.pg_class ic'
        + ' ON (i.indexrelid=ic.oid) JOIN pg_catalog.pg_attribute a'
        + ' ON (ic.oid=a.attrelid), pg_catalog.pg_namespace n2'
        + ' JOIN pg_catalog.pg_class c2 ON (c2.relnamespace=n2.oid),'
        + ' pg_catalog.pg_trigger t1 JOIN pg_catalog.pg_proc p1'
        + ' ON (t1.tgfoid=p1.oid), pg_catalog.pg_trigger t2'
        + ' JOIN pg_catalog.pg_proc p2 ON (t2.tgfoid=p2.oid)';
      Where := '';
      if PrimarySchema <> ''then
      begin
        Where := Where + ' AND n1.nspname = '
          + EscapeString(PrimarySchema);
      end;
      if ForeignSchema <> '' then
      begin
        Where := Where + ' AND n2.nspname = '
          + EscapeString(ForeignSchema);
      end;
    end
    else
    begin
      Select := 'SELECT DISTINCT NULL::text as pnspname, NULL::text as fnspname,';
      From := ' FROM pg_class c1 JOIN pg_index i ON (c1.oid=i.indrelid)'
        + ' JOIN pg_class ic ON (i.indexrelid=ic.oid) JOIN pg_attribute a'
        + ' ON (ic.oid=a.attrelid), pg_class c2, pg_trigger t1'
        + ' JOIN pg_proc p1 ON (t1.tgfoid=p1.oid), pg_trigger t2'
        + ' JOIN pg_proc p2 ON (t2.tgfoid=p2.oid)';
    end;

    SQL := Select + ' c1.relname as prelname, c2.relname as frelname,'
      + ' t1.tgconstrname, a.attnum as keyseq, ic.relname as fkeyname,'
      + ' t1.tgdeferrable, t1.tginitdeferred, t1.tgnargs,t1.tgargs,'
      + ' p1.proname as updaterule, p2.proname as deleterule'
      + From
      + ' WHERE (t1.tgrelid=c1.oid AND t1.tgisconstraint'
      + ' AND t1.tgconstrrelid=c2.oid AND p1.proname'
      + ' LIKE ' + EscapeString('RI\_FKey\_%\_upd')
      + ') AND (t2.tgrelid=c1.oid'
      + ' AND t2.tgisconstraint AND t2.tgconstrrelid=c2.oid '
      + ' AND p2.proname LIKE ' + EscapeString('RI\_FKey\_%\_del')
      + ') AND i.indisprimary'
      + Where;
    if PrimaryTable <> '' then
      SQL := SQL + ' AND c1.relname=' + EscapeString(PrimaryTable);
    if ForeignTable <> '' then
      SQL := SQL + ' AND c2.relname=' + EscapeString(ForeignTable);
    SQL := SQL + ' ORDER BY ';

    if PrimaryTable <> '' then
    begin
      if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
        SQL := SQL + 'fnspname, ';
      SQL := SQL + 'frelname';
    end
    else
    begin
      if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
        SQL := SQL + 'pnspname, ';
      SQL := SQL + 'prelname';
    end;

    SQL := SQL + ', keyseq';

    List := TStringList.Create;
    try
      with GetConnection.CreateStatement.ExecuteQuery(SQL) do
      begin
        while Next do
        begin
          Result.MoveToInsertRow;
          Result.UpdateString(2, GetString(1));
          Result.UpdateString(6, GetString(2));
          Result.UpdateString(3, GetString(3));
          Result.UpdateString(7, GetString(4));

          //FKeyName := GetString(5);
          UpdateRule := GetString(12);
          if UpdateRule <> '' then
          begin
            Rule := Copy(UpdateRule, 9, Length(UpdateRule) - 12);
            Action := Ord(ikNoAction);
            if (Rule = '') or (Rule = 'noaction') then
              Action := Ord(ikNoAction);
            if Rule = 'cascade' then
              Action := Ord(ikCascade);
            if Rule = 'setnull' then
              Action := Ord(ikSetNull);
            if Rule = 'setdefault' then
              Action := Ord(ikSetDefault);
            if Rule = 'restrict' then
             Action := Ord(ikRestrict);
            Result.UpdateInt(10, Action);
          end;

          DeleteRule := GetString(13);
          if DeleteRule <> '' then
          begin
            Rule := Copy(DeleteRule, 9, Length(DeleteRule) - 12);
            Action := Ord(ikNoAction);
            if Rule = 'cascade' then
              Action := Ord(ikCascade);
            if Rule = 'setnull' then
              Action := Ord(ikSetNull);
            if Rule = 'setdefault' then
              Action := Ord(ikSetDefault);
            if Rule = 'restrict' then
              Action := Ord(ikRestrict);
            Result.UpdateInt(11, Action);
          end;

          KeySequence := GetInt(6);
          Targs := GetString(11);

          //<unnamed>\000ww\000vv\000UNSPECIFIED\000m\000a\000n\000b\000
          //for Postgresql 7.3
          //$1\000ww\000vv\000UNSPECIFIED\000m\000a\000n\000b\000
          //$2\000ww\000vv\000UNSPECIFIED\000m\000a\000n\000b\000

          Advance := 4 + (KeySequence - 1) * 2;
          PutSplitStringEx(List, Targs, '\000');

          if Advance <= List.Count-1 then
            FKeyColumn := List.Strings[Advance];
          if Advance + 1 <= List.Count-1 then
            PKeyColumn := List.Strings[Advance+1];
          Result.UpdateString(4, PKeyColumn);
          Result.UpdateString(8, FKeyColumn);
          Result.UpdateString(9, GetString(6)); //KEY_SEQ

          if List.Strings[0] = '<unnamed>' then
            Result.UpdateString(12, Targs) //FK_NAME
          else Result.UpdateString(12, List.Strings[0]); //FK_NAME

          Result.UpdateString(13, GetString(6)); //PK_ NAME

          Deferrability := Ord(ikNotDeferrable);
          Deferrable := GetBoolean(8);
          InitiallyDeferred := GetBoolean(9);
          if Deferrable then
          begin
            if InitiallyDeferred then
              Deferrability := Ord(ikInitiallyDeferred)
            else Deferrability := Ord(ikInitiallyImmediate);
          end;
          Result.UpdateInt(14, Deferrability);
          Result.InsertRow;
        end;
        Close;
      end;
    finally
      List.Free;
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
 	<LI><B>CASE_SENSITIVE</B> Boolean=> is it case sensitive?
 	<LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
 	<LI><B>UNSIGNED_ATTRIBUTE</B> Boolean => is it unsigned?
 	<LI><B>FIXED_PREC_SCALE</B> Boolean => can it be a money value?
 	<LI><B>AUTO_INCREMENT</B> Boolean => can it be used for an
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
function TZPostgreSQLDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
var
  SQL: string;
begin
    Result := ConstructVirtualResultSet(TypeInfoColumnsDynArray);

    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
      SQL := ' SELECT typname FROM pg_catalog.pg_type '
    else SQL := ' SELECT typname FROM pg_type ';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(1, GetString(1));
        Result.UpdateInt(2, Ord(GetSQLTypeByName(GetString(1))));
        Result.UpdateInt(3, 9);
        Result.UpdateInt(7, Ord(ntNoNulls));
        Result.UpdateBoolean(8, False);
        Result.UpdateBoolean(9, False);
        Result.UpdateBoolean(11, False);
        Result.UpdateBoolean(12, False);
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
function TZPostgreSQLDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  SQL, Select, From, Where: string;
begin
    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      Select := 'SELECT NULL AS TABLE_CAT, n.nspname AS TABLE_SCHEM,';
      From := ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class ct,'
        + ' pg_catalog.pg_class ci, pg_catalog.pg_index i,'
        + ' pg_catalog.pg_attribute a, pg_catalog.pg_am am';
      Where := ' AND n.oid = ct.relnamespace';
      if Schema <> '' then
        Where := Where + ' AND n.nspname = ' + EscapeString(Schema);
    end
    else
    begin
      Select := 'SELECT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM,';
      From := ' FROM pg_class ct, pg_class ci, pg_index i, pg_attribute a,'
        + ' pg_am am';
    end;

    SQL := Select + ' ct.relname AS TABLE_NAME, NOT i.indisunique'
      + ' AS NON_UNIQUE, NULL AS INDEX_QUALIFIER, ci.relname AS INDEX_NAME,'
      + ' CASE i.indisclustered WHEN true THEN ' + IntToStr(Ord(tiClustered))
      + ' ELSE CASE am.amname WHEN ''hash'' THEN ' + IntToStr(Ord(tiHashed))
      + ' ELSE ' + IntToStr(Ord(tiOther)) + ' END END AS TYPE,'
      + ' a.attnum AS ORDINAL_POSITION, a.attname AS COLUMN_NAME,'
      + ' NULL AS ASC_OR_DESC, ci.reltuples AS CARDINALITY,'
      + ' ci.relpages AS PAGES, NULL AS FILTER_CONDITION'
      + From
      + ' WHERE ct.oid=i.indrelid AND ci.oid=i.indexrelid'
      + ' AND a.attrelid=ci.oid AND ci.relam=am.oid' + Where
      + ' AND ct.relname = ' + EscapeString(Table);

    if Unique then
      SQL := SQL + ' AND i.indisunique';
    SQL := SQL + ' ORDER BY NON_UNIQUE, TYPE, INDEX_NAME, ORDINAL_POSITION';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(IndexInfoColumnsDynArray));
end;

function TZPostgreSQLDatabaseMetadata.UncachedGetSequences(const Catalog, SchemaPattern,
  SequenceNamePattern: string): IZResultSet;
var
  SQL: string;
begin
    Result := ConstructVirtualResultSet(SequenceColumnsDynArray);

    SQL := ' SELECT nspname, relname ' +
      'FROM pg_catalog.pg_namespace n, pg_catalog.pg_class ct ' +
      'WHERE relkind = ''S'' ' +
      'AND n.oid = ct.relnamespace';

    if SequenceNamePattern <> '' then
      SQL := SQL + ' AND ' + Format('relname = ''%s''', [SequenceNamePattern]);
    if SchemaPattern <> '' then
      SQL := SQL + ' AND ' + Format('nspname = ''%s''', [SchemaPattern]);

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(1);
        Result.UpdateString(2, GetStringByName('nspname'));
        Result.UpdateString(3, GetStringByName('relname'));
        Result.InsertRow;
      end;
      Close;
    end;
end;

function TZPostgreSQLDatabaseMetadata.GetPostgreSQLType(Oid: Integer): string;
begin
  Result := (GetConnection as IZPostgreSQLConnection).GetTypeNameByOid(Oid);
end;

function TZPostgreSQLDatabaseMetadata.GetSQLTypeByOid(Oid: Integer): TZSQLType;
var
  PostgreSQLConnection: IZPostgreSQLConnection;
begin
  PostgreSQLConnection := GetConnection as IZPostgreSQLConnection;
  Result := PostgreSQLToSQLType(PostgreSQLConnection,
    PostgreSQLConnection.GetTypeNameByOid(Oid));
end;

function TZPostgreSQLDatabaseMetadata.GetSQLTypeByName(
  TypeName: string): TZSQLType;
begin
  Result := PostgreSQLToSQLType(
    GetConnection as IZPostgreSQLConnection, TypeName);
end;

function TZPostgreSQLDatabaseMetadata.TableTypeSQLExpression(
  TableType: string; UseSchemas: Boolean): string;
begin
  if UseSchemas then
  begin
    if TableType = 'TABLE' then
      Result := ' c.relkind = ''r'' AND n.nspname NOT LIKE ''pg\\_%'' '
    else if TableType = 'VIEW' then
      Result := ' c.relkind = ''v'' AND n.nspname <> ''pg_catalog'' '
    else if TableType = 'INDEX' then
      Result := ' c.relkind = ''i'' AND n.nspname NOT LIKE ''pg\\_%'' '
    else if TableType = 'SEQUENCE' then
      Result := ' c.relkind = ''S'' '
    else if TableType = 'SYSTEM TABLE' then
      Result := ' c.relkind = ''r'' AND n.nspname = ''pg_catalog'' '
    else if TableType = 'SYSTEM TOAST TABLE' then
      Result := ' c.relkind = ''r'' AND n.nspname = ''pg_toast'' '
    else if TableType = 'SYSTEM TOAST INDEX' then
      Result := ' c.relkind = ''i'' AND n.nspname = ''pg_toast'' '
    else if TableType = 'SYSTEM VIEW' then
      Result := ' c.relkind = ''v'' AND n.nspname = ''pg_catalog'' '
    else if TableType = 'SYSTEM INDEX' then
      Result := ' c.relkind = ''i'' AND n.nspname = ''pg_catalog'' '
    else if TableType = 'TEMPORARY TABLE' then
      Result := ' c.relkind = ''r'' AND n.nspname LIKE ''pg\\_temp\\_%'' '
    else if TableType = 'TEMPORARY INDEX' then
      Result := 'c.relkind = ''i'' AND n.nspname LIKE ''pg\\_temp\\_%'' ';
  end
  else
  begin
    if TableType = 'TABLE' then
      Result := ' c.relkind = ''r'' AND c.relname NOT LIKE ''pg\\_%'' '
    else if TableType = 'VIEW' then
      Result := ' c.relkind = ''v'' AND c.relname NOT LIKE ''pg\\_%'' '
    else if TableType = 'INDEX' then
      Result := ' c.relkind = ''i'' AND c.relname NOT LIKE ''pg\\_%'' '
    else if TableType = 'SEQUENCE' then
      Result := ' c.relkind = ''S'' '
    else if TableType = 'SYSTEM TABLE' then
      Result := ' c.relkind = ''r'' AND c.relname LIKE ''pg\\_%'' AND c.relname '+
        'NOT LIKE ''pg\\_toast\\_%'' AND c.relname NOT LIKE ''pg\\_temp\\_%'' '
    else if TableType = 'SYSTEM TOAST TABLE' then
      Result := ' c.relkind = ''r'' AND c.relname LIKE ''pg\\_toast\\_%'' '
    else if TableType = 'SYSTEM TOAST INDEX' then
      Result := ' c.relkind = ''i'' AND c.relname LIKE ''pg\\_toast\\_%'' '
    else if TableType = 'SYSTEM VIEW' then
      Result := 'c.relkind = ''v'' AND c.relname LIKE ''pg\\_%'''
    else if TableType = 'SYSTEM INDEX' then
    begin
      Result := ' c.relkind = ''v'' AND c.relname LIKE ''pg\\_%'' AND '+
        'c.relname NOT LIKE ''pg\\_toast\\_%'' AND c.relname '+
        'NOT LIKE ''pg\\_temp\\_%'' '
    end
    else if TableType = 'TEMPORARY TABLE' then
      Result := ' c.relkind = ''r'' AND c.relname LIKE ''pg\\_temp\\_%'' '
    else if TableType = 'TEMPORARY INDEX' then
      Result := ' c.relkind = ''i'' AND c.relname LIKE ''pg\\_temp\\_%'' '
  end;
end;

procedure TZPostgreSQLDatabaseMetadata.ParseACLArray(
  List: TStrings; AclString: string);
var
  PrevChar: Char;
  InQuotes: Boolean;
  I, BeginIndex: Integer;
begin
  if AclString = '' then Exit;
  InQuotes := False;
  PrevChar := ' ';
  BeginIndex := 2;
  for I := BeginIndex to Length(AclString) do
  begin
    if (AclString[I] = '"') and (PrevChar <> '\' ) then
      InQuotes := not InQuotes
    else if (AclString[I] = ',') and not InQuotes then
    begin
      List.Add(Copy(AclString, BeginIndex, I - BeginIndex));
      BeginIndex := I+1;
    end;
    PrevChar := AclString[I];
  end;

  // add last element removing the trailing "}"
  List.Add(Copy(AclString, BeginIndex, Length(AclString) - BeginIndex));

  // Strip out enclosing quotes, if any.
  for I := 0 to List.Count-1 do
  begin
    if (List.Strings[i][1] = '"')
      and (List.Strings[i][Length(List.Strings[i])] = '"') then
      List.Strings[i] := Copy(List.Strings[i], 2, Length(List.Strings[i])-2);
  end;
end;

function TZPostgreSQLDatabaseMetadata.GetPrivilegeName(Permission: Char): string;
begin
 case Permission of
   'a': Result := 'INSERT';
   'r': Result := 'SELECT';
   'w': Result := 'UPDATE';
   'd': Result := 'DELETE';
   'R': Result := 'RULE';
   'x': Result := 'REFERENCES';
   't': Result := 'TRIGGER';
   'X': Result := 'EXECUTE';
   'U': Result := 'USAGE';
   'C': Result := 'CREATE';
   'T': Result := 'CREATE TEMP';
   else Result := 'UNKNOWN';
 end;
end;

function TZPostgreSQLDatabaseMetadata.GetIdentifierConvertor: IZIdentifierConvertor;
begin
  Result:=TZPostgreSQLIdentifierConvertor.Create(Self);
end; 

{ TZPostgresIdentifierConvertor } 
 
function TZPostgreSQLIdentifierConvertor.ExtractQuote( 
  const Value: string): string; 
var 
  QuoteDelim: string; 
begin 
  QuoteDelim := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;
  Result := Value; 
  if (QuoteDelim <> '') and (Value <> '') then 
    if (copy(Value,1,1)=QuoteDelim) and 
       (copy(Value,length(Value),1)=QuoteDelim) then 
    begin 
      Result:=copy(Value,2,length(Value)-2); 
      Result:=StringReplace(Result,QuoteDelim+QuoteDelim,QuoteDelim,[rfReplaceAll]); 
    end; 
 
end; 
 
function TZPostgreSQLIdentifierConvertor.IsQuoted(const Value: string): Boolean; 
var 
  QuoteDelim: string; 
begin 
  QuoteDelim := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;
  Result := (QuoteDelim <> '') and (Value <> '') and 
            (copy(Value,1,1)=QuoteDelim) and 
            (copy(Value,length(Value),1)=QuoteDelim); 
end; 
 
function TZPostgreSQLIdentifierConvertor.IsSpecialCase( 
  const Value: string): Boolean; 
var 
  I: Integer; 
begin 
  Result := False; 
  if not (Value[1] in ['a'..'z','_']) then 
  begin 
    Result := True; 
    Exit; 
  end; 
  for I := 1 to Length(Value) do 
  begin 
    if not (Value[I] in ['A'..'Z','a'..'z','0'..'9','_']) then 
    begin 
      Result := True; 
      Break; 
    end; 
  end; 
end; 
 
function TZPostgreSQLIdentifierConvertor.Quote(const Value: string): string; 
var 
  QuoteDelim: string; 
begin 
  Result := Value; 
  if IsCaseSensitive(Value) then 
  begin 
    QuoteDelim := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;
    Result := QuoteDelim + 
              StringReplace(Result,QuoteDelim,QuoteDelim+QuoteDelim,[rfReplaceAll]) + 
              QuoteDelim; 
  end; 
end; 
 
end.
