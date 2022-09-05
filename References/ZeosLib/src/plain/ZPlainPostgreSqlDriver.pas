{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for PostgreSQL           }
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

unit ZPlainPostgreSqlDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZCompatibility, ZPlainDriver;

const
  WINDOWS_DLL_LOCATION   = 'libpq.dll';
  WINDOWS_DLL7_LOCATION   = 'libpq74.dll';
  WINDOWS_DLL8_LOCATION   = 'libpq81.dll';
  LINUX_DLL_LOCATION   = 'libpq.so';
  LINUX_DLL8_LOCATION  = 'libpq.so.4';
  LINUX_DLL82_LOCATION = 'libpq.so.5';

{ Type Lengths }
  NAMEDATALEN  = 32;

{ OIDNAMELEN should be set to NAMEDATALEN + sizeof(Oid) }
  OIDNAMELEN   = 36;

  INV_WRITE    = $00020000;
  INV_READ     = $00040000;

  BLOB_SEEK_SET     = 0;
  BLOB_SEEK_CUR     = 1;
  BLOB_SEEK_END     = 2;

type

{ Application-visible enum types }
  TZPostgreSQLConnectStatusType = (
    CONNECTION_OK,
    CONNECTION_BAD
  );

  TZPostgreSQLFieldCode=( // FirmOS
            PG_DIAG_SEVERITY=ord('S'),
            PG_DIAG_SQLSTATE=ord('C'),
            PG_DIAG_MESSAGE_PRIMARY=ord('M'),
            PG_DIAG_MESSAGE_DETAIL=ord('D'),
            PG_DIAG_MESSAGE_HINT=ord('H'),
            PG_DIAG_STATEMENT_POSITION=ord('P'),
            PG_DIAG_INTERNAL_POSITION=ord('p'),
            PG_DIAG_INTERNAL_QUERY=ord('q'),
            PG_DIAG_CONTEXT=ord('W'),
            PG_DIAG_SOURCE_FILE=ord('F'),
            PG_DIAG_SOURCE_LINE=ord('L'),
            PG_DIAG_SOURCE_FUNCTION=ord('R')
            );

  TZPostgreSQLExecStatusType = (
    PGRES_EMPTY_QUERY,
    PGRES_COMMAND_OK,		{ a query command that doesn't return anything
				  was executed properly by the backend }
    PGRES_TUPLES_OK,		{ a query command that returns tuples
				  was executed properly by the backend,
				  PGresult contains the result tuples }
    PGRES_COPY_OUT,		{ Copy Out data transfer in progress }
    PGRES_COPY_IN,		{ Copy In data transfer in progress }
    PGRES_BAD_RESPONSE,		{ an unexpected response was recv'd from
				  the backend }
    PGRES_NONFATAL_ERROR,
    PGRES_FATAL_ERROR
  );

{ PGnotify represents the occurrence of a NOTIFY message.
  Ideally this would be an opaque typedef, but it's so simple that it's
  unlikely to change.
  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  whereas in earlier versions it was always your own backend's PID.
}
  TZPostgreSQLNotify = packed record
    relname: PAnsiChar;   { name of relation containing data }
    be_pid:  Integer; { process id of backend }
  end;

  PZPostgreSQLNotify = ^TZPostgreSQLNotify;

{ PQnoticeProcessor is the function type for the notice-message callback. }

  TZPostgreSQLNoticeProcessor = procedure(arg: Pointer; message: PAnsiChar); cdecl;

{ Structure for the conninfo parameter definitions returned by PQconndefaults }

  TZPostgreSQLConnectInfoOption = packed record
    keyword:  PAnsiChar;	{ The keyword of the option }
    envvar:   PAnsiChar;	{ Fallback environment variable name }
    compiled: PAnsiChar;	{ Fallback compiled in default value  }
    val:      PAnsiChar;	{ Options value	}
    lab:      PAnsiChar;	{ Label for field in connect dialog }
    disPAnsiChar: PAnsiChar;	{ Character to display for this field
			  in a connect dialog. Values are:
			  ""	Display entered value as is
			  "*"	Password field - hide value
			  "D"	Debug options - don't
			  create a field by default }
    dispsize: Integer;	{ Field size in characters for dialog }
  end;

  PZPostgreSQLConnectInfoOption = ^TZPostgreSQLConnectInfoOption;

{ PQArgBlock -- structure for PQfn() arguments }

  TZPostgreSQLArgBlock = packed record
    len:     Integer;
    isint:   Integer;
    case u: Boolean of
      True:  (ptr: PInteger);	{ can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PZPostgreSQLArgBlock = ^TZPostgreSQLArgBlock;

  PZPostgreSQLConnect = Pointer;
  PZPostgreSQLResult = Pointer;
  Oid = Integer;

TZPgCharactersetType = (
	csSQL_ASCII,	{ SQL/ASCII }
	csEUC_JP,	{ EUC for Japanese }
	csEUC_CN,	{ EUC for Chinese }
	csEUC_KR,	{ EUC for Korean }
	csEUC_TW,	{ EUC for Taiwan }
	csJOHAB,
	csUTF8,		{ Unicode UTF-8 }
	csMULE_INTERNAL,	{ Mule internal code }
	csLATIN1,	{ ISO-8859 Latin 1 }
	csLATIN2,	{ ISO-8859 Latin 2 }
	csLATIN3,	{ ISO-8859 Latin 3 }
	csLATIN4,	{ ISO-8859 Latin 4 }
	csLATIN5,	{ ISO-8859 Latin 5 }
	csLATIN6,	{ ISO-8859 Latin 6 }
	csLATIN7,	{ ISO-8859 Latin 7 }
	csLATIN8,	{ ISO-8859 Latin 8 }
	csLATIN9,	{ ISO-8859 Latin 9 }
	csLATIN10,	{ ISO-8859 Latin 10 }
	csWIN1256,	{ Arabic Windows }
	csWIN1258,	{ Vietnamese Windows }
	csWIN874,	{ Thai Windows }
	csKOI8R,	{ KOI8-R/U }
	csWIN1251,	{ windows-1251 }
	csWIN866,	{ Alternativny Variant (MS-DOS CP866) }
	csISO_8859_5,	{ ISO-8859-5 }
	csISO_8859_6,	{ ISO-8859-6 }
	csISO_8859_7,	{ ISO-8859-7 }
	csISO_8859_8,	{ ISO-8859-8 }
	csSJIS,		{ Shift JIS }
	csBIG5,		{ Big5 }
	csGBK,		{ GBK }
	csUHC,		{ UHC }
	csWIN1250,	{ windows-1250 }
	csGB18030,	{ GB18030 }
	csUNICODE_PODBC,{ UNICODE ( < Ver8.1). Can't call it UNICODE as that's already used }
	csTCVN,		{ TCVN ( < Ver8.1) }
	csALT,		{ ALT ( < Var8.1) }
	csWIN,		{ WIN ( < Ver8.1) }
	csOTHER
);

{ ****************** Plain API Types definition ***************** }

type
{ String descriptions of the ExecStatusTypes }
  pgresStatus = array[$00..$ff] of PAnsiChar;

{ PGconn encapsulates a connection to the backend.
  The contents of this struct are not supposed to be known to applications.
}
  PGconn = Pointer;
  PPGconn = Pointer;

{ PGresult encapsulates the result of a query (or more precisely, of a single
  SQL command --- a query string given to PQsendQuery can contain multiple
  commands and thus return multiple PGresult objects).
  The contents of this struct are not supposed to be known to applications.
}
  PGresult = Pointer;
  PPGresult = Pointer;

{ PGnotify represents the occurrence of a NOTIFY message.
  Ideally this would be an opaque typedef, but it's so simple that it's
  unlikely to change.
  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  whereas in earlier versions it was always your own backend's PID.
}
  PGnotify = packed record
    relname: array [0..NAMEDATALEN-1] of AnsiChar; { name of relation containing data }
    be_pid:  Integer;			      { process id of backend }
  end;

  PPGnotify = ^PGnotify;

{ PQnoticeProcessor is the function type for the notice-message callback. }

  PQnoticeProcessor = procedure(arg: Pointer; message: PAnsiChar); cdecl;

{ Print options for PQprint() }

{
  We can't use the conventional "bool", because we are designed to be
  included in a user's program, and user may already have that type
  defined.  Pqbool, on the other hand, is unlikely to be used.
}

  PPAnsiChar = array[00..$ff] of PAnsiChar;

  PQprintOpt = packed record
    header:    Byte;	   { print output field headings and row count }
    align:     Byte;	   { fill align the fields }
    standard:  Byte;	   { old brain dead format }
    html3:     Byte;	   { output html tables }
    expanded:  Byte;	   { expand tables }
    pager:     Byte;	   { use pager for output if needed }
    fieldSep:  PAnsiChar;	   { field separator }
    tableOpt:  PAnsiChar;      { insert to HTML <table ...> }
    caption:   PAnsiChar;	   { HTML <caption> }
    fieldName: PPAnsiChar; 	   { null terminated array of repalcement field names }
  end;

  PPQprintOpt = ^PQprintOpt;

{ ----------------
  Structure for the conninfo parameter definitions returned by PQconndefaults
  ----------------
}
  PQconninfoOption = packed record
    keyword:  PAnsiChar;	{ The keyword of the option }
    envvar:   PAnsiChar;	{ Fallback environment variable name }
    compiled: PAnsiChar;	{ Fallback compiled in default value  }
    val:      PAnsiChar;	{ Options value	}
    lab:      PAnsiChar;	{ Label for field in connect dialog }
    disPAnsiChar: PAnsiChar;	{ Character to display for this field
			  in a connect dialog. Values are:
			  ""	Display entered value as is
			  "*"	Password field - hide value
			  "D"	Debug options - don't
			  create a field by default }
    dispsize: Integer;	{ Field size in characters for dialog }
  end;

  PPQConninfoOption = ^PQconninfoOption;

{ ----------------
  PQArgBlock -- structure for PQfn() arguments
  ----------------
}
  PQArgBlock = packed record
    len:     Integer;
    isint:   Integer;
    case u: Boolean of
      True:  (ptr: PInteger);	{ can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PPQArgBlock = ^PQArgBlock;


{ ************** Plain API Function types definition ************* }
{ ===	in fe-connect.c === }
  TPQconnectdb     = function(ConnInfo: PAnsiChar): PPGconn; cdecl; // FirmOS 8.1 OK
  TPQsetdbLogin    = function(Host, Port, Options, Tty, Db, User, Passwd: PAnsiChar): PPGconn; cdecl; // FirmOS 8.1 OK
//15022006 FirmOS: omitting   PQconnectStart
//15022006 FirmOS: omitting  PQconnectPoll
  TPQconndefaults  = function: PPQconninfoOption; cdecl;
  TPQfinish        = procedure(Handle: PPGconn); cdecl;
  TPQreset         = procedure(Handle: PPGconn); cdecl;
//15022006 FirmOS: omitting PQresetStart
//15022006 FirmOS: omitting PQresetPoll
  TPQrequestCancel = function(Handle: PPGconn): Integer; cdecl;
  TPQdb            = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQuser          = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQpass          = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQhost          = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQport          = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQtty           = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQoptions       = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQstatus        = function(Handle: PPGconn): TZPostgreSQLConnectStatusType; cdecl;
//TBD  PGTransactionStatusType PQtransactionStatus(const PGconn *conn);
//15022006 FirmOS: omitting const char *PQparameterStatus(const PGconn *conn, const char *paramName);
//15022006 FirmOS: omitting  PQprotocolVersion
//15022006 FirmOS: omitting  PQserverVersion
  TPQerrorMessage  = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQsocket        = function(Handle: PPGconn): Integer; cdecl;
  TPQbackendPID    = function(Handle: PPGconn): Integer; cdecl;
//15022006 FirmOS: omitting  SSL *PQgetssl(const PGconn *conn);
  TPQtrace         = procedure(Handle: PPGconn; DebugPort: Pointer); cdecl;
  TPQuntrace       = procedure(Handle: PPGconn); cdecl;
  TPQsetNoticeProcessor = procedure(Handle: PPGconn; Proc: PQnoticeProcessor; Arg: Pointer); cdecl;
{ === in fe-exec.c === }
  TPQexec          = function(Handle: PPGconn; Query: PAnsiChar): PPGresult; cdecl;
  TPQnotifies      = function(Handle: PPGconn): PPGnotify; cdecl;
  TPQfreeNotify    = procedure(Handle: PPGnotify);cdecl;
  TPQsendQuery     = function(Handle: PPGconn; Query: PAnsiChar): Integer; cdecl;
  TPQgetResult     = function(Handle: PPGconn): PPGresult; cdecl;
  TPQisBusy        = function(Handle: PPGconn): Integer; cdecl;
  TPQconsumeInput  = function(Handle: PPGconn): Integer; cdecl;
  TPQgetline       = function(Handle: PPGconn; Str: PAnsiChar; length: Integer): Integer; cdecl;
  TPQputline       = function(Handle: PPGconn; Str: PAnsiChar): Integer; cdecl;
  TPQgetlineAsync  = function(Handle: PPGconn; Buffer: PAnsiChar; BufSize: Integer): Integer; cdecl;
  TPQputnbytes     = function(Handle: PPGconn; Buffer: PAnsiChar; NBytes: Integer): Integer; cdecl;
  TPQendcopy       = function(Handle: PPGconn): Integer; cdecl;
  TPQfn            = function(Handle: PPGconn; fnid: Integer; result_buf, result_len: PInteger; result_is_int: Integer; args: PPQArgBlock; nargs: Integer): PPGresult; cdecl;
  TPQresultStatus  = function(Result: PPGresult): TZPostgreSQLExecStatusType; cdecl;
  TPQresultErrorMessage = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQresultErrorField=function(result: PPGResult; fieldcode:integer):PAnsiChar;cdecl; // postgresql 8
  TPQntuples       = function(Result: PPGresult): Integer; cdecl;
  TPQnfields       = function(Result: PPGresult): Integer; cdecl;
  TPQbinaryTuples  = function(Result: PPGresult): Integer; cdecl;
  TPQfname         = function(Result: PPGresult; field_num: Integer): PAnsiChar; cdecl;
  TPQfnumber       = function(Result: PPGresult; field_name: PAnsiChar): Integer; cdecl;
  TPQftype         = function(Result: PPGresult; field_num: Integer): Oid; cdecl;
  TPQfsize         = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQfmod          = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQcmdStatus     = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQoidValue      = function(Result: PPGresult): Oid; cdecl;
  TPQoidStatus     = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQcmdTuples     = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQgetvalue      = function(Result: PPGresult; tup_num, field_num: Integer): PAnsiChar; cdecl;
  TPQgetlength     = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQgetisnull     = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQclear         = procedure(Result: PPGresult); cdecl;
  TPQmakeEmptyPGresult  = function(Handle: PPGconn; status: TZPostgreSQLExecStatusType): PPGresult; cdecl;
  // postgresql 8
  TPQescapeByteaConn =function(Handle: PPGconn;const from:PAnsiChar;from_length:longword;to_lenght:PLongword):PAnsiChar;cdecl;
  TPQescapeBytea     =function(const from:PAnsiChar;from_length:longword;to_lenght:PLongword):PAnsiChar;cdecl;
  TPQunescapeBytea     =function(const from:PAnsiChar;to_lenght:PLongword):PAnsiChar;cdecl;
  TPQFreemem       = procedure(ptr:Pointer);cdecl;
{ === in fe-lobj.c === }
  Tlo_open         = function(Handle: PPGconn; lobjId: Oid; mode: Integer): Integer; cdecl;
  Tlo_close        = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_read         = function(Handle: PPGconn; fd: Integer; buf: PAnsiChar; len: Integer): Integer; cdecl;
  Tlo_write        = function(Handle: PPGconn; fd: Integer; buf: PAnsiChar; len: Integer): Integer; cdecl;
  Tlo_lseek        = function(Handle: PPGconn; fd, offset, whence: Integer): Integer; cdecl;
  Tlo_creat        = function(Handle: PPGconn; mode: Integer): Oid; cdecl;
  Tlo_tell         = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_unlink       = function(Handle: PPGconn; lobjId: Oid): Integer; cdecl;
  Tlo_import       = function(Handle: PPGconn; filename: PAnsiChar): Oid; cdecl;
  Tlo_export       = function(Handle: PPGconn; lobjId: Oid; filename: PAnsiChar): Integer; cdecl;

{ ************** Collection of Plain API Function types definition ************* }
TZPOSTGRESQL_API = record
{ ===	in fe-connect.c === }
  PQconnectdb:     TPQconnectdb;
  PQsetdbLogin:    TPQsetdbLogin;
  PQconndefaults:  TPQconndefaults;
  PQfinish:        TPQfinish;
  PQreset:         TPQreset;
  PQrequestCancel: TPQrequestCancel;
  PQdb:            TPQdb;
  PQuser:          TPQuser;
  PQpass:          TPQpass;
  PQhost:          TPQhost;
  PQport:          TPQport;
  PQtty:           TPQtty;
  PQoptions:       TPQoptions;
  PQstatus:        TPQstatus;
  PQerrorMessage:  TPQerrorMessage;
  PQsocket:        TPQsocket;
  PQbackendPID:    TPQbackendPID;
  PQtrace:         TPQtrace;
  PQuntrace:       TPQuntrace;
  PQsetNoticeProcessor: TPQsetNoticeProcessor;
{ === in fe-exec.c === }
  PQexec:          TPQexec;
  PQnotifies:      TPQnotifies;
  PQfreeNotify:    TPQfreeNotify;
  PQsendQuery:     TPQsendQuery;
  PQgetResult:     TPQgetResult;
  PQisBusy:        TPQisBusy;
  PQconsumeInput:  TPQconsumeInput;
  PQgetline:       TPQgetline;
  PQputline:       TPQputline;
  PQgetlineAsync:  TPQgetlineAsync;
  PQputnbytes:     TPQputnbytes;
  PQendcopy:       TPQendcopy;
  PQfn:            TPQfn;
  PQresultStatus:  TPQresultStatus;
  PQresultErrorMessage: TPQresultErrorMessage;
  PQresultErrorField: TPQresultErrorField; // postgresql 8
  PQntuples:       TPQntuples;
  PQnfields:       TPQnfields;
  PQbinaryTuples:  TPQbinaryTuples;
  PQfname:         TPQfname;
  PQfnumber:       TPQfnumber;
  PQftype:         TPQftype;
  PQfsize:         TPQfsize;
  PQfmod:          TPQfmod;
  PQcmdStatus:     TPQcmdStatus;
  PQoidValue:      TPQoidValue;
  PQoidStatus:     TPQoidStatus;
  PQcmdTuples:     TPQcmdTuples;
  PQgetvalue:      TPQgetvalue;
  PQgetlength:     TPQgetlength;
  PQgetisnull:     TPQgetisnull;
  PQclear:         TPQclear;
  PQmakeEmptyPGresult:  TPQmakeEmptyPGresult;
  PQescapeByteaConn:TPQescapeByteaConn; // postgresql 8
  PQescapeBytea:TPQescapeBytea; // postgresql 8
  PQunescapeBytea:TPQunescapeBytea; // postgresql 8
  PQFreemem:TPQFreemem; // postgresql 8
{ === in fe-lobj.c === }
  lo_open:         Tlo_open;
  lo_close:        Tlo_close;
  lo_read:         Tlo_read;
  lo_write:        Tlo_write;
  lo_lseek:        Tlo_lseek;
  lo_creat:        Tlo_creat;
  lo_tell:         Tlo_tell;
  lo_unlink:       Tlo_unlink;
  lo_import:       Tlo_import;
  lo_export:       Tlo_export;
end;

type

  {** Represents a generic interface to PostgreSQL native API. }
  IZPostgreSQLPlainDriver = interface (IZPlainDriver)
    ['{03CD6345-2D7A-4FE2-B03D-3C5656789FEB}']

    function  EncodeBYTEA(const Value: AnsiString; Handle: PZPostgreSQLConnect): AnsiString;
    function  DecodeBYTEA(const value: AnsiString): AnsiString;

    function ConnectDatabase(ConnInfo: PAnsiChar): PZPostgreSQLConnect;
    function SetDatabaseLogin(Host, Port, Options, TTY, Db, User,Passwd: PAnsiChar): PZPostgreSQLConnect;
    function GetConnectDefaults: PZPostgreSQLConnectInfoOption;

    procedure Finish(Handle: PZPostgreSQLConnect);
    procedure Reset(Handle: PZPostgreSQLConnect);
    function RequestCancel(Handle: PZPostgreSQLConnect): Integer;
    function GetDatabase(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetUser(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetPassword(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetHost(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetPort(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetTTY(Handle: PZPostgreSQLConnect): PAnsiChar; cdecl;
    function GetOptions(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetStatus(Handle: PZPostgreSQLConnect):TZPostgreSQLConnectStatusType;

    function GetErrorMessage(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetSocket(Handle: PZPostgreSQLConnect): Integer;
    function GetBackendPID(Handle: PZPostgreSQLConnect): Integer;
    procedure Trace(Handle: PZPostgreSQLConnect; DebugPort: Pointer);
    procedure Untrace(Handle: PZPostgreSQLConnect);
    procedure SetNoticeProcessor(Handle: PZPostgreSQLConnect;Proc: TZPostgreSQLNoticeProcessor; Arg: Pointer);

    function ExecuteQuery(Handle: PZPostgreSQLConnect;Query: PAnsiChar): PZPostgreSQLResult;

    function Notifies(Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
    procedure FreeNotify(Handle: PZPostgreSQLNotify);

    function SendQuery(Handle: PZPostgreSQLConnect; Query: PAnsiChar): Integer;
    function GetResult(Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
    function IsBusy(Handle: PZPostgreSQLConnect): Integer;
    function ConsumeInput(Handle: PZPostgreSQLConnect): Integer;
    function GetLine(Handle: PZPostgreSQLConnect; Str: PAnsiChar;
      Length: Integer): Integer;
    function PutLine(Handle: PZPostgreSQLConnect; Str: PAnsiChar): Integer;
    function GetLineAsync(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;

    function PutBytes(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;
    function EndCopy(Handle: PZPostgreSQLConnect): Integer;
    function ExecuteFunction(Handle: PZPostgreSQLConnect; fnid: Integer;
      result_buf, result_len: PInteger; result_is_int: Integer;
      args: PZPostgreSQLArgBlock; nargs: Integer): PZPostgreSQLResult;
    function GetResultStatus(Res: PZPostgreSQLResult):
      TZPostgreSQLExecStatusType;

    function GetResultErrorMessage(Res: PZPostgreSQLResult): PAnsiChar;
    function GetResultErrorField(Res: PZPostgreSQLResult; FieldCode: TZPostgreSQLFieldCode): PAnsiChar;

    function GetRowCount(Res: PZPostgreSQLResult): Integer;
    function GetFieldCount(Res: PZPostgreSQLResult): Integer;

    function GetBinaryTuples(Res: PZPostgreSQLResult): Integer;
    function GetFieldName(Res: PZPostgreSQLResult; FieldNum: Integer): PAnsiChar;
    function GetFieldNumber(Res: PZPostgreSQLResult; FieldName: PAnsiChar): Integer;
    function GetFieldType(Res: PZPostgreSQLResult; FieldNum: Integer): Oid;
    function GetFieldSize(Res: PZPostgreSQLResult;  FieldNum: Integer): Integer;
    function GetFieldMode(Res: PZPostgreSQLResult; FieldNum: Integer): Integer;
    function GetCommandStatus(Res: PZPostgreSQLResult): PAnsiChar;
    function GetOidValue(Res: PZPostgreSQLResult): Oid;
    function GetOidStatus(Res: PZPostgreSQLResult): PAnsiChar;
    function GetCommandTuples(Res: PZPostgreSQLResult): PAnsiChar;

    function GetValue(Res: PZPostgreSQLResult;  TupNum, FieldNum: Integer): PAnsiChar;
    function GetLength(Res: PZPostgreSQLResult; TupNum, FieldNum: Integer): Integer;
    function GetIsNull(Res: PZPostgreSQLResult; TupNum, FieldNum: Integer): Integer;
    procedure Clear(Res: PZPostgreSQLResult);

    function MakeEmptyResult(Handle: PZPostgreSQLConnect;
      Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;

    function OpenLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      Mode: Integer): Integer;
    function CloseLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function ReadLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PAnsiChar; Length: Integer): Integer;
    function WriteLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PAnsiChar; Length: Integer): Integer;
    function SeekLargeObject(Handle: PZPostgreSQLConnect;
      Fd, Offset, Whence: Integer): Integer;
    function CreateLargeObject(Handle: PZPostgreSQLConnect;
      Mode: Integer): Oid;
    function TellLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function UnlinkLargeObject(Handle: PZPostgreSQLConnect;
      ObjId: Oid): Integer;
    function ImportLargeObject(Handle: PZPostgreSQLConnect;
      FileName: PAnsiChar): Oid;
    function ExportLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      FileName: PAnsiChar): Integer;
  end;

  {** Implements a base driver for PostgreSQL}
  TZPostgreSQLBaseDriver = class(TZAbstractPlainDriver, IZPlainDriver, IZPostgreSQLPlainDriver)
  protected
    POSTGRESQL_API : TZPOSTGRESQL_API;
    procedure LoadApi; override;
  public
    constructor Create;

    function EncodeBYTEA(const Value: AnsiString; Handle: PZPostgreSQLConnect): AnsiString; virtual;
    function DecodeBYTEA(const value: AnsiString): AnsiString; virtual;

    function ConnectDatabase(ConnInfo: PAnsiChar): PZPostgreSQLConnect;
    function SetDatabaseLogin(Host, Port, Options, TTY, Db, User, Passwd: PAnsiChar): PZPostgreSQLConnect;
    function GetConnectDefaults: PZPostgreSQLConnectInfoOption;

    procedure Finish(Handle: PZPostgreSQLConnect);
    procedure Reset(Handle: PZPostgreSQLConnect);
    function RequestCancel(Handle: PZPostgreSQLConnect): Integer;
    function GetDatabase(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetUser(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetPassword(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetHost(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetPort(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetTTY(Handle: PZPostgreSQLConnect): PAnsiChar; cdecl;
    function GetOptions(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetStatus(Handle: PZPostgreSQLConnect):
      TZPostgreSQLConnectStatusType;

    function GetErrorMessage(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetSocket(Handle: PZPostgreSQLConnect): Integer;
    function GetBackendPID(Handle: PZPostgreSQLConnect): Integer;
    procedure Trace(Handle: PZPostgreSQLConnect; DebugPort: Pointer);
    procedure Untrace(Handle: PZPostgreSQLConnect);
    procedure SetNoticeProcessor(Handle: PZPostgreSQLConnect;
      Proc: TZPostgreSQLNoticeProcessor; Arg: Pointer);

    function ExecuteQuery(Handle: PZPostgreSQLConnect;
      Query: PAnsiChar): PZPostgreSQLResult;

    function Notifies(Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
    procedure FreeNotify(Handle: PZPostgreSQLNotify);

    function SendQuery(Handle: PZPostgreSQLConnect; Query: PAnsiChar): Integer;
    function GetResult(Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
    function IsBusy(Handle: PZPostgreSQLConnect): Integer;
    function ConsumeInput(Handle: PZPostgreSQLConnect): Integer;
    function GetLine(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;
    function PutLine(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar): Integer;
    function GetLineAsync(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;

    function PutBytes(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;
    function EndCopy(Handle: PZPostgreSQLConnect): Integer;
    function ExecuteFunction(Handle: PZPostgreSQLConnect; fnid: Integer;
      result_buf, result_len: PInteger; result_is_int: Integer;
      args: PZPostgreSQLArgBlock; nargs: Integer): PZPostgreSQLResult;
    function GetResultStatus(Res: PZPostgreSQLResult): TZPostgreSQLExecStatusType;
    function GetResultErrorMessage(Res: PZPostgreSQLResult): PAnsiChar;
    function GetResultErrorField(Res: PZPostgreSQLResult;FieldCode:TZPostgreSQLFieldCode):PAnsiChar; virtual;

    function GetRowCount(Res: PZPostgreSQLResult): Integer;
    function GetFieldCount(Res: PZPostgreSQLResult): Integer;

    function GetBinaryTuples(Res: PZPostgreSQLResult): Integer;
    function GetFieldName(Res: PZPostgreSQLResult;
      FieldNum: Integer): PAnsiChar;
    function GetFieldNumber(Res: PZPostgreSQLResult;
      FieldName: PAnsiChar): Integer;
    function GetFieldType(Res: PZPostgreSQLResult;
      FieldNum: Integer): Oid;
    function GetFieldSize(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetFieldMode(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetCommandStatus(Res: PZPostgreSQLResult): PAnsiChar;
    function GetOidValue(Res: PZPostgreSQLResult): Oid;
    function GetOidStatus(Res: PZPostgreSQLResult): PAnsiChar;
    function GetCommandTuples(Res: PZPostgreSQLResult): PAnsiChar;

    function GetValue(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): PAnsiChar;
    function GetLength(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    function GetIsNull(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    procedure Clear(Res: PZPostgreSQLResult);

    function MakeEmptyResult(Handle: PZPostgreSQLConnect;
      Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;

    function OpenLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      Mode: Integer): Integer;
    function CloseLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function ReadLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PAnsiChar; Length: Integer): Integer;
    function WriteLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PAnsiChar; Length: Integer): Integer;
    function SeekLargeObject(Handle: PZPostgreSQLConnect;
      Fd, Offset, Whence: Integer): Integer;
    function CreateLargeObject(Handle: PZPostgreSQLConnect;
      Mode: Integer): Oid;
    function TellLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function UnlinkLargeObject(Handle: PZPostgreSQLConnect;
      ObjId: Oid): Integer;
    function ImportLargeObject(Handle: PZPostgreSQLConnect;
      FileName: PAnsiChar): Oid;
    function ExportLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      FileName: PAnsiChar): Integer;
  end;

  {** Implements a driver for PostgreSQL 7.4 }
  TZPostgreSQL7PlainDriver = class(TZPostgreSQLBaseDriver, IZPlainDriver,
    IZPostgreSQLPlainDriver)
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;

    function  EncodeBYTEA(const Value: AnsiString; Handle: PZPostgreSQLConnect): AnsiString; override;
    function  DecodeBYTEA(const value: AnsiString): AnsiString; override;

    function GetResultErrorField(Res: PZPostgreSQLResult;FieldCode:TZPostgreSQLFieldCode):PAnsiChar; override;
  end;


  {** Implements a driver for PostgreSQL 8.1 }

  { TZPostgreSQL8PlainDriver }

  TZPostgreSQL8PlainDriver = class(TZPostgreSQLBaseDriver, IZPlainDriver,IZPostgreSQLPlainDriver)
  protected
    procedure LoadApi; override;
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;


implementation

uses SysUtils, ZPlainLoader;

{ TZPostgreSQLBaseDriver }
procedure TZPostgreSQLBaseDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  { ===	in fe-connect.c === }
    @POSTGRESQL_API.PQconnectdb    := GetAddress('PQconnectdb');
    @POSTGRESQL_API.PQsetdbLogin   := GetAddress('PQsetdbLogin');
    @POSTGRESQL_API.PQconndefaults := GetAddress('PQconndefaults');
    @POSTGRESQL_API.PQfinish       := GetAddress('PQfinish');
    @POSTGRESQL_API.PQreset        := GetAddress('PQreset');
    @POSTGRESQL_API.PQrequestCancel := GetAddress('PQrequestCancel');
    @POSTGRESQL_API.PQdb           := GetAddress('PQdb');
    @POSTGRESQL_API.PQuser         := GetAddress('PQuser');
    @POSTGRESQL_API.PQpass         := GetAddress('PQpass');
    @POSTGRESQL_API.PQhost         := GetAddress('PQhost');
    @POSTGRESQL_API.PQport         := GetAddress('PQport');
    @POSTGRESQL_API.PQtty          := GetAddress('PQtty');
    @POSTGRESQL_API.PQoptions      := GetAddress('PQoptions');
    @POSTGRESQL_API.PQstatus       := GetAddress('PQstatus');
    @POSTGRESQL_API.PQerrorMessage := GetAddress('PQerrorMessage');
    @POSTGRESQL_API.PQsocket       := GetAddress('PQsocket');
    @POSTGRESQL_API.PQbackendPID   := GetAddress('PQbackendPID');
    @POSTGRESQL_API.PQtrace        := GetAddress('PQtrace');
    @POSTGRESQL_API.PQuntrace      := GetAddress('PQuntrace');
    @POSTGRESQL_API.PQsetNoticeProcessor := GetAddress('PQsetNoticeProcessor');

  { === in fe-exec.c === }
    @POSTGRESQL_API.PQexec         := GetAddress('PQexec');
    @POSTGRESQL_API.PQnotifies     := GetAddress('PQnotifies');
    @POSTGRESQL_API.PQfreeNotify   := GetAddress('PQfreeNotify');
    @POSTGRESQL_API.PQsendQuery    := GetAddress('PQsendQuery');
    @POSTGRESQL_API.PQgetResult    := GetAddress('PQgetResult');
    @POSTGRESQL_API.PQisBusy       := GetAddress('PQisBusy');
    @POSTGRESQL_API.PQconsumeInput := GetAddress('PQconsumeInput');
    @POSTGRESQL_API.PQgetline      := GetAddress('PQgetline');
    @POSTGRESQL_API.PQputline      := GetAddress('PQputline');
    @POSTGRESQL_API.PQgetlineAsync := GetAddress('PQgetlineAsync');
    @POSTGRESQL_API.PQputnbytes    := GetAddress('PQputnbytes');
    @POSTGRESQL_API.PQendcopy      := GetAddress('PQendcopy');
    @POSTGRESQL_API.PQfn           := GetAddress('PQfn');
    @POSTGRESQL_API.PQresultStatus := GetAddress('PQresultStatus');
    @POSTGRESQL_API.PQresultErrorMessage := GetAddress('PQresultErrorMessage');
    @POSTGRESQL_API.PQntuples      := GetAddress('PQntuples');
    @POSTGRESQL_API.PQnfields      := GetAddress('PQnfields');
    @POSTGRESQL_API.PQbinaryTuples := GetAddress('PQbinaryTuples');
    @POSTGRESQL_API.PQfname        := GetAddress('PQfname');
    @POSTGRESQL_API.PQfnumber      := GetAddress('PQfnumber');
    @POSTGRESQL_API.PQftype        := GetAddress('PQftype');
    @POSTGRESQL_API.PQfsize        := GetAddress('PQfsize');
    @POSTGRESQL_API.PQfmod         := GetAddress('PQfmod');
    @POSTGRESQL_API.PQcmdStatus    := GetAddress('PQcmdStatus');
    @POSTGRESQL_API.PQoidValue     := GetAddress('PQoidValue');
    @POSTGRESQL_API.PQoidStatus    := GetAddress('PQoidStatus');
    @POSTGRESQL_API.PQcmdTuples    := GetAddress('PQcmdTuples');
    @POSTGRESQL_API.PQgetvalue     := GetAddress('PQgetvalue');
    @POSTGRESQL_API.PQgetlength    := GetAddress('PQgetlength');
    @POSTGRESQL_API.PQgetisnull    := GetAddress('PQgetisnull');
    @POSTGRESQL_API.PQclear        := GetAddress('PQclear');
    @POSTGRESQL_API.PQmakeEmptyPGresult := GetAddress('PQmakeEmptyPGresult');

  { === in fe-lobj.c === }
    @POSTGRESQL_API.lo_open        := GetAddress('lo_open');
    @POSTGRESQL_API.lo_close       := GetAddress('lo_close');
    @POSTGRESQL_API.lo_read        := GetAddress('lo_read');
    @POSTGRESQL_API.lo_write       := GetAddress('lo_write');
    @POSTGRESQL_API.lo_lseek       := GetAddress('lo_lseek');
    @POSTGRESQL_API.lo_creat       := GetAddress('lo_creat');
    @POSTGRESQL_API.lo_tell        := GetAddress('lo_tell');
    @POSTGRESQL_API.lo_unlink      := GetAddress('lo_unlink');
    @POSTGRESQL_API.lo_import      := GetAddress('lo_import');
    @POSTGRESQL_API.lo_export      := GetAddress('lo_export');
  end;
end;

constructor TZPostgreSQLBaseDriver.Create;
begin
   inherited create;
   FLoader := TZNativeLibraryLoader.Create([]);
{$IFNDEF STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
  {$ENDIF}
{$ENDIF}
end;

procedure TZPostgreSQLBaseDriver.Clear(Res: PZPostgreSQLResult);
begin
  POSTGRESQL_API.PQclear(Res);
end;

function TZPostgreSQLBaseDriver.CloseLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_close(Handle, Fd);
end;

function TZPostgreSQLBaseDriver.ConnectDatabase(
  ConnInfo: PAnsiChar): PZPostgreSQLConnect;
begin
  Result := POSTGRESQL_API.PQconnectdb(ConnInfo);
end;

function TZPostgreSQLBaseDriver.ConsumeInput(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQconsumeInput(Handle);
end;

function TZPostgreSQLBaseDriver.CreateLargeObject(
  Handle: PZPostgreSQLConnect; Mode: Integer): Oid;
begin
  Result := POSTGRESQL_API.lo_creat(Handle, Mode);
end;

function TZPostgreSQLBaseDriver.DecodeBYTEA(const value: AnsiString): AnsiString;
var
   decoded: PAnsiChar;
   len: Longword;
begin
  decoded := POSTGRESQL_API.PQUnescapeBytea(PAnsiChar(value), @len);
  SetLength(result, len);
  if (len > 0) then
     Move(decoded^, result[1], len);
  POSTGRESQL_API.PQFreemem(decoded);
end;

function TZPostgreSQLBaseDriver.EncodeBYTEA(const Value: AnsiString;  Handle: PZPostgreSQLConnect): AnsiString;
var
   encoded: PAnsiChar;
   len: Longword;
   leng: cardinal;
   a : String;
begin
 leng := Length(Value);
 if assigned(POSTGRESQL_API.PQescapeByteaConn) then
   encoded := POSTGRESQL_API.PQescapeByteaConn(Handle, PAnsiChar(value), leng, @len)
 else
   encoded := POSTGRESQL_API.PQescapeBytea(PAnsiChar(value),leng,@len);
 SetLength(result, len - 1);
 StrCopy(PAnsiChar(result), encoded);
 POSTGRESQL_API.PQFreemem(encoded);
 result := ''''+result+'''';
end;

function TZPostgreSQLBaseDriver.EndCopy( Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQendcopy(Handle);
end;

function TZPostgreSQLBaseDriver.ExecuteFunction(
  Handle: PZPostgreSQLConnect; fnid: Integer; result_buf,
  result_len: PInteger; result_is_int: Integer; args: PZPostgreSQLArgBlock;
  nargs: Integer): PZPostgreSQLResult;
begin
  Result := POSTGRESQL_API.PQfn(Handle, fnid, result_buf,
    result_len, result_is_int, PPQArgBlock(args), nargs);
end;

function TZPostgreSQLBaseDriver.ExecuteQuery(
  Handle: PZPostgreSQLConnect; Query: PAnsiChar): PZPostgreSQLResult;
begin
  Result := POSTGRESQL_API.PQexec(Handle, Query);
end;

function TZPostgreSQLBaseDriver.ExportLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; FileName: PAnsiChar): Integer;
begin
  Result := POSTGRESQL_API.lo_export(Handle, ObjId, FileName);
end;

procedure TZPostgreSQLBaseDriver.Finish(Handle: PZPostgreSQLConnect);
begin
  POSTGRESQL_API.PQfinish(Handle);
end;

procedure TZPostgreSQLBaseDriver.FreeNotify(Handle: PZPostgreSQLNotify);
begin
  POSTGRESQL_API.PQfreeNotify(PPGnotify(Handle));
end;

function TZPostgreSQLBaseDriver.GetBackendPID(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQbackendPID(Handle);
end;

function TZPostgreSQLBaseDriver.GetBinaryTuples(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := POSTGRESQL_API.PQbinaryTuples(Res);
end;

function TZPostgreSQLBaseDriver.GetCommandStatus(
  Res: PZPostgreSQLResult): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQcmdStatus(Res);
end;

function TZPostgreSQLBaseDriver.GetCommandTuples(
  Res: PZPostgreSQLResult): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQcmdTuples(Res);
end;

function TZPostgreSQLBaseDriver.GetConnectDefaults:
  PZPostgreSQLConnectInfoOption;
begin
  Result := PZPostgreSQLConnectInfoOption(POSTGRESQL_API.PQconndefaults);
end;

function TZPostgreSQLBaseDriver.GetDatabase(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQdb(Handle);
end;

function TZPostgreSQLBaseDriver.GetErrorMessage(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQerrorMessage(Handle);
end;

function TZPostgreSQLBaseDriver.GetFieldCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := POSTGRESQL_API.PQnfields(Res);
end;

function TZPostgreSQLBaseDriver.GetFieldMode(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQfmod(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetFieldName(Res: PZPostgreSQLResult;
  FieldNum: Integer): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQfname(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetFieldNumber(
  Res: PZPostgreSQLResult; FieldName: PAnsiChar): Integer;
begin
  Result := POSTGRESQL_API.PQfnumber(Res, FieldName);
end;

function TZPostgreSQLBaseDriver.GetFieldSize(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQfsize(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetFieldType(Res: PZPostgreSQLResult;
  FieldNum: Integer): Oid;
begin
  Result := POSTGRESQL_API.PQftype(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetHost(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQhost(Handle);
end;

function TZPostgreSQLBaseDriver.GetIsNull(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQgetisnull(Res, TupNum, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetLength(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQgetlength(Res, TupNum, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetLine(Handle: PZPostgreSQLConnect;
  Buffer: PAnsiChar; Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQgetline(Handle, Buffer, Length);
end;

function TZPostgreSQLBaseDriver.GetLineAsync(
  Handle: PZPostgreSQLConnect; Buffer: PAnsiChar; Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQgetlineAsync(Handle, Buffer, Length);
end;

function TZPostgreSQLBaseDriver.GetOidStatus(
  Res: PZPostgreSQLResult): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQoidStatus(Res);
end;

function TZPostgreSQLBaseDriver.GetOidValue(
  Res: PZPostgreSQLResult): Oid;
begin
  Result := POSTGRESQL_API.PQoidValue(Res);
end;

function TZPostgreSQLBaseDriver.GetOptions(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQoptions(Handle);
end;

function TZPostgreSQLBaseDriver.GetPassword(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQpass(Handle);
end;

function TZPostgreSQLBaseDriver.GetPort(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQport(Handle);
end;

function TZPostgreSQLBaseDriver.GetResult(
  Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
begin
  Result := POSTGRESQL_API.PQgetResult(Handle);
end;

function TZPostgreSQLBaseDriver.GetResultErrorField(Res: PZPostgreSQLResult;  FieldCode: TZPostgreSQLFieldCode): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQresultErrorField(Res,ord(FieldCode));
end;


function TZPostgreSQLBaseDriver.GetResultErrorMessage(
  Res: PZPostgreSQLResult): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQresultErrorMessage(Res);
end;

function TZPostgreSQLBaseDriver.GetResultStatus(
  Res: PZPostgreSQLResult): TZPostgreSQLExecStatusType;
begin
  Result := TZPostgreSQLExecStatusType(POSTGRESQL_API.PQresultStatus(Res));
end;

function TZPostgreSQLBaseDriver.GetRowCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := POSTGRESQL_API.PQntuples(Res);
end;

function TZPostgreSQLBaseDriver.GetSocket(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQsocket(Handle);
end;

function TZPostgreSQLBaseDriver.GetStatus(
  Handle: PZPostgreSQLConnect): TZPostgreSQLConnectStatusType;
begin
  Result := TZPostgreSQLConnectStatusType(POSTGRESQL_API.PQstatus(Handle));
end;

function TZPostgreSQLBaseDriver.GetTTY(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQtty(Handle);
end;

function TZPostgreSQLBaseDriver.GetUser(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQuser(Handle);
end;

function TZPostgreSQLBaseDriver.GetValue(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQgetvalue(Res, TupNum, FieldNum);
end;

function TZPostgreSQLBaseDriver.ImportLargeObject(
  Handle: PZPostgreSQLConnect; FileName: PAnsiChar): Oid;
begin
  Result := POSTGRESQL_API.lo_import(Handle, FileName);
end;

function TZPostgreSQLBaseDriver.IsBusy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQisBusy(Handle);
end;

function TZPostgreSQLBaseDriver.MakeEmptyResult(
  Handle: PZPostgreSQLConnect;
  Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;
begin
  Result := POSTGRESQL_API.PQmakeEmptyPGresult(Handle,
    TZPostgreSQLExecStatusType(Status));
end;

function TZPostgreSQLBaseDriver.Notifies(
  Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
begin
  Result := PZPostgreSQLNotify(POSTGRESQL_API.PQnotifies(Handle));
end;

function TZPostgreSQLBaseDriver.OpenLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; Mode: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_open(Handle, ObjId, Mode);
end;

function TZPostgreSQLBaseDriver.PutBytes(Handle: PZPostgreSQLConnect;
  Buffer: PAnsiChar; Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQputnbytes(Handle, Buffer, Length);
end;

function TZPostgreSQLBaseDriver.PutLine(Handle: PZPostgreSQLConnect;
  Buffer: PAnsiChar): Integer;
begin
  Result := POSTGRESQL_API.PQputline(Handle, Buffer);
end;

function TZPostgreSQLBaseDriver.ReadLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PAnsiChar;
  Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_read(Handle, Fd, Buffer, Length);
end;

function TZPostgreSQLBaseDriver.RequestCancel(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQrequestCancel(Handle);
end;

procedure TZPostgreSQLBaseDriver.Reset(Handle: PZPostgreSQLConnect);
begin
  POSTGRESQL_API.PQreset(Handle);
end;

function TZPostgreSQLBaseDriver.SeekLargeObject(
  Handle: PZPostgreSQLConnect; Fd, Offset, Whence: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_lseek(Handle, Fd, Offset, Whence);
end;

function TZPostgreSQLBaseDriver.SendQuery(Handle: PZPostgreSQLConnect;
  Query: PAnsiChar): Integer;
begin
  Result := POSTGRESQL_API.PQsendQuery(Handle, Query);
end;

function TZPostgreSQLBaseDriver.SetDatabaseLogin(Host, Port, Options,
  TTY, Db, User, Passwd: PAnsiChar): PZPostgreSQLConnect;
begin
  Result := POSTGRESQL_API.PQsetdbLogin(Host, Port, Options, TTY, Db,
    User, Passwd);
end;

procedure TZPostgreSQLBaseDriver.SetNoticeProcessor(
  Handle: PZPostgreSQLConnect; Proc: TZPostgreSQLNoticeProcessor;
  Arg: Pointer);
begin
  POSTGRESQL_API.PQsetNoticeProcessor(Handle, Proc, Arg);
end;

function TZPostgreSQLBaseDriver.TellLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_tell(Handle, Fd);
end;

procedure TZPostgreSQLBaseDriver.Trace(Handle: PZPostgreSQLConnect;
  DebugPort: Pointer);
begin
  POSTGRESQL_API.PQtrace(Handle, DebugPort);
end;

function TZPostgreSQLBaseDriver.UnlinkLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid): Integer;
begin
  Result := POSTGRESQL_API.lo_unlink(Handle, ObjId);
end;

procedure TZPostgreSQLBaseDriver.Untrace(Handle: PZPostgreSQLConnect);
begin
  POSTGRESQL_API.PQuntrace(Handle);
end;

function TZPostgreSQLBaseDriver.WriteLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PAnsiChar;
  Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_write(Handle, Fd, Buffer, Length);
end;

{ TZPostgreSQL7PlainDriver }

constructor TZPostgreSQL7PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL7_LOCATION);
  {$ENDIF}
end;

function TZPostgreSQL7PlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-7';
end;

function TZPostgreSQL7PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 7.x';
end;

function TZPostgreSQL7PlainDriver.DecodeBYTEA(const value: AnsiString): AnsiString;
begin
 result:=value;
end;

function TZPostgreSQL7PlainDriver.EncodeBYTEA(const Value: AnsiString;  Handle: PZPostgreSQLConnect): AnsiString;
begin
 result:=value;
end;

function TZPostgreSQL7PlainDriver.GetResultErrorField(Res: PZPostgreSQLResult;  FieldCode: TZPostgreSQLFieldCode): PAnsiChar;
begin
 // Not implemented for 7
 result:='';
end;

{ TZPostgreSQL8PlainDriver }

procedure TZPostgreSQL8PlainDriver.LoadApi;
begin
  inherited LoadApi;

  with Loader do
  begin
  @POSTGRESQL_API.PQfreemem           := GetAddress('PQfreemem');
  @POSTGRESQL_API.PQescapeByteaConn   := GetAddress('PQescapeByteaConn');
  @POSTGRESQL_API.PQescapeBytea       := GetAddress('PQescapeBytea');
  @POSTGRESQL_API.PQunescapeBytea     := GetAddress('PQunescapeBytea');
  @POSTGRESQL_API.PQresultErrorField  := GetAddress('PQresultErrorField');
  end;
end;

constructor TZPostgreSQL8PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL8_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL82_LOCATION);
    FLoader.AddLocation(LINUX_DLL8_LOCATION);
  {$ENDIF}
end;

function TZPostgreSQL8PlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-8';
end;

function TZPostgreSQL8PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 8.x';
end;

end.
