{*******************************************************************************
Author(s):
Henri Gourvest <hgourvest@progdigy.com>
Olivier Guilbaud <oguilb@free.fr>
Volkan Ceylan <volkance@hotmail.com>
The Original Code is the UIB code (version 2.1)

The Initial Developer of the Original Code is
Henri Gourvest <hgourvest@progdigy.com>. Portions
created by the Initial Developer are Copyright (C)
by the Initial Developer. All Rights Reserved.

Description:
ALFBX (Alcinoe FireBird Express) does for the Firebird
API what Delphi does for the WINDOWS API! Create high
performance client/server applications based on FireBird
without the BDE or ODBC.

Link :
https://uib.svn.sourceforge.net/svnroot/uib (current code is from the trunk rev 419)
http://www.progdigy.com/modules.php?name=UIB
*******************************************************************************}

unit ALFBXLib;

{$IFNDEF CPUX64}
  {$ALIGN ON}
{$ENDIF}
  {$MINENUMSIZE 4}

interface

uses Winapi.Windows,
     System.SysUtils,
     System.Classes,
     ALFBXbase;

type

//{$IFNDEF UNICODE}
//  UnicodeString = WideString;
//  RawByteString = AnsiString;
//{$ENDIF}

  TALFBXFieldType = (uftUnKnown, uftNumeric, uftChar, uftVarchar, uftCstring, uftSmallint,
    uftInteger, uftQuad, uftFloat, uftDoublePrecision, uftTimestamp, uftBlob, uftBlobId,
    uftDate, uftTime, uftInt64, uftArray
    {FB25_UP}, uftNull {FB25_UP});

  TALFBXScale = 1..15;

//******************************************************************************
// Errors handling
//******************************************************************************
  EALFBXConvertError = class(Exception);

  EALFBXError = class(Exception)
  private
    FGDSCode: Integer;
    FErrorCode: Integer;
    FSQLCode  : Integer;
{FB25_UP}
    FSQLState: {RawByteString}AnsiString;
{FB25_UP}
  public
    property ErrorCode: Integer read FErrorCode;
    property SQLCode: Integer read FSQLCode;
    property GDSCode: Integer read FGDSCode;
{FB25_UP}
    property SQLState: {RawByteString}AnsiString read FSQLState;
{FB25_UP}
  end;

  EALFBXException = class(EALFBXError)
  private
    FNumber: Integer;
  public
    property Number: Integer read FNumber;
  end;

  EALFBXGFixError    = class(EALFBXError);
  EALFBXDSQLError    = class(EALFBXError);
  EALFBXDynError     = class(EALFBXError);
  EALFBXGBakError    = class(EALFBXError);
  EALFBXGSecError    = class(EALFBXError);
  EALFBXLicenseError = class(EALFBXError);
  EALFBXGStatError   = class(EALFBXError);


  EALFBXExceptionClass = class of EALFBXError;


const
  cALFBXQuadNull: TISCQuad = (gds_quad_high: 0; gds_quad_low: 0);

//******************************************************************************
// Database
//******************************************************************************

type
  TALFBXCharacterSet = (csNONE, csASCII, csBIG_5, csCYRL, csDOS437, csDOS850,
  csDOS852, csDOS857, csDOS860, csDOS861, csDOS863, csDOS865, csEUCJ_0208,
  csGB_2312, csISO8859_1, csISO8859_2, csKSC_5601, csNEXT, csOCTETS, csSJIS_0208,
  csUNICODE_FSS, csUTF8, csWIN1250, csWIN1251, csWIN1252, csWIN1253, csWIN1254
{FB15_UP}
  ,csDOS737, csDOS775, csDOS858, csDOS862, csDOS864, csDOS866, csDOS869, csWIN1255,
  csWIN1256, csWIN1257, csISO8859_3, csISO8859_4, csISO8859_5, csISO8859_6, csISO8859_7,
  csISO8859_8, csISO8859_9, csISO8859_13
{FB15_UP}
{FB20_UP}
  ,csKOI8R, csKOI8U
{FB20_UP}
{FB21_UP}
  ,csWIN1258
  ,csTIS620
  ,csGBK
  ,csCP943C
{FB21_UP}
  );

  // Transaction parameters
  TALFBXTransParam = (
    { prevents a transaction from accessing tables if they are written to by
      other transactions.}
    tpConsistency,
    { allows concurrent transactions to read and write shared data. }
    tpConcurrency,
    { Concurrent, shared access of a specified table among all transactions. }
    tpShared,
    { Concurrent, restricted access of a specified table. }
    tpProtected,
    tpExclusive,
    { Specifies that the transaction is to wait until the conflicting resource
      is released before retrying an operation [Default]. }
    tpWait,
    { Specifies that the transaction is not to wait for the resource to be
      released, but instead, should return an update conflict error immediately. }
    tpNowait,
    { Read-only access mode that allows a transaction only to select data from tables. }
    tpRead,
    { Read-write access mode of that allows a transaction to select, insert,
      update, and delete table data [Default]. }
    tpWrite,
    { Read-only access of a specified table. Use in conjunction with tpShared,
      tpProtected, and tpExclusive to establish the lock option. }
    tpLockRead,
    { Read-write access of a specified table. Use in conjunction with tpShared,
      tpProtected, and tpExclusive to establish the lock option [Default]. }
    tpLockWrite,
    tpVerbTime,
    tpCommitTime,
    tpIgnoreLimbo,
    { Unlike a concurrency transaction, a read committed transaction sees changes
      made and committed by transactions that were active after this transaction started. }
    tpReadCommitted,
    tpAutoCommit,
    { Enables an tpReadCommitted transaction to read only the latest committed
      version of a record. }
    tpRecVersion,
    tpNoRecVersion,
    tpRestartRequests,
    tpNoAutoUndo
  {FB20_UP}
    ,tpLockTimeout
  {FB20_UP}
  );

  { Set of transaction parameters. }
  TALFBXTransParams = set of TALFBXTransParam;

const
  cALFBXCharacterSetStr : array[TALFBXCharacterSet] of AnsiString = (
    'NONE', 'ASCII', 'BIG_5', 'CYRL', 'DOS437', 'DOS850', 'DOS852', 'DOS857',
    'DOS860', 'DOS861', 'DOS863', 'DOS865', 'EUCJ_0208', 'GB_2312', 'ISO8859_1',
    'ISO8859_2', 'KSC_5601', 'NEXT', 'OCTETS', 'SJIS_0208', 'UNICODE_FSS', 'UTF8',
    'WIN1250', 'WIN1251', 'WIN1252', 'WIN1253', 'WIN1254'
{FB15_UP}
    ,'DOS737', 'DOS775', 'DOS858', 'DOS862', 'DOS864', 'DOS866', 'DOS869',
    'WIN1255', 'WIN1256', 'WIN1257', 'ISO8859_3', 'ISO8859_4', 'ISO8859_5',
    'ISO8859_6', 'ISO8859_7', 'ISO8859_8', 'ISO8859_9', 'ISO8859_13'
{FB15_UP}
{FB20_UP}
    ,'KOI8R', 'KOI8U'
{FB20_UP}
{FB21_UP}
    ,'WIN1258'
    ,'TIS620'
    ,'GBK'
    ,'CP943C'
{FB21_UP}
    );

  cALFBXCharacterSetCP: array[TALFBXCharacterSet] of Word =
  (
  0, //csNONE,
  20127, //csASCII,
  950, //csBIG_5,
  1251, // csCYRL,
  437, // csDOS437, IBM437	OEM United States
  850, // csDOS850, ibm850	OEM Multilingual Latin 1; Western European (DOS)
  852, // csDOS852,
  857, // csDOS857,
  860, // csDOS860,
  861, // csDOS861,
  863, // csDOS863,
  865, // csDOS865,
  20932, // csEUCJ_0208, EUC-JP Japanese (JIS 0208-1990 and 0121-1990)
  936, //csGB_2312 gb2312	ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)
  28591, //csISO8859_1, iso-8859-1	ISO 8859-1 Latin 1; Western European (ISO)
  28592, // csISO8859_2, iso-8859-2	ISO 8859-2 Central European; Central European (ISO)
  949, // csKSC_5601 ks_c_5601-1987	ANSI/OEM Korean (Unified Hangul Code)
  0, //csNEXT,
  0, //csOCTETS,
  932, //csSJIS_0208, shift_jis	ANSI/OEM Japanese; Japanese (Shift-JIS)
  65001, //csUNICODE_FSS utf-8	Unicode (UTF-8)
  65001, // csUTF8 utf-8	Unicode (UTF-8)
  1250, //csWIN1250, windows-1250	ANSI Central European; Central European (Windows)
  1251, //csWIN1251,windows-1251	ANSI Cyrillic; Cyrillic (Windows)
  1252, //csWIN1252, windows-1252	ANSI Latin 1; Western European (Windows)
  1253, //csWIN1253, windows-1253	ANSI Greek; Greek (Windows)
  1254 // csWIN1254 windows-1254	ANSI Turkish; Turkish (Windows)
{FB15_UP}
  ,737 //csDOS737, ibm737	OEM Greek (formerly 437G); Greek (DOS)
  ,775 // csDOS775, ibm775	OEM Baltic; Baltic (DOS)
  ,858 //csDOS858, IBM00858	OEM Multilingual Latin 1 + Euro symbol
  ,862 // csDOS862, DOS-862	OEM Hebrew; Hebrew (DOS)
  ,864 //csDOS864, IBM864	OEM Arabic; Arabic (864)
  ,866 // csDOS866, cp866	OEM Russian; Cyrillic (DOS)
  ,869 //csDOS869, 	ibm869	OEM Modern Greek; Greek, Modern (DOS)
  ,1255 //csWIN1255, 	windows-1255	ANSI Hebrew; Hebrew (Windows)
  ,1256 //csWIN1256, 	windows-1256	ANSI Arabic; Arabic (Windows)
  ,1257 // csWIN1257, windows-1257	ANSI Baltic; Baltic (Windows)
  ,28593 //csISO8859_3, iso-8859-3	ISO 8859-3 Latin 3
  ,28594 //csISO8859_4, iso-8859-4	ISO 8859-4 Baltic
  ,28595 //csISO8859_5, 	iso-8859-5	ISO 8859-5 Cyrillic
  ,28596 //csISO8859_6, iso-8859-6	ISO 8859-6 Arabic
  ,28597 //csISO8859_7, 	iso-8859-7	ISO 8859-7 Greek
  ,28598 //csISO8859_8, 	iso-8859-8	ISO 8859-8 Hebrew; Hebrew (ISO-Visual)
  ,28599 //csISO8859_9, 	iso-8859-9	ISO 8859-9 Turkish
  ,28603 //csISO8859_13 iso-8859-13	ISO 8859-13 Estonian
{FB15_UP}
{FB20_UP}
  ,20866 // csKOI8R koi8-r	Russian (KOI8-R); Cyrillic (KOI8-R)
  ,21866 //csKOI8U koi8-u	Ukrainian (KOI8-U); Cyrillic (KOI8-U)
{FB20_UP}
{FB21_UP}
  ,1258 //csWIN1258 ANSI/OEM Vietnamese; Vietnamese (Windows)
  ,874 //csTIS620 windows-874	ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows)
  ,936 //gb2312	ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)
  ,932 //csCP943C Shift_JIS
{FB21_UP}
  );

  cALFBXBytesPerCharacter: array[TALFBXCharacterSet] of Byte =
  (
    1, // NONE
    1, // ASCII
    2, // BIG_5
    1, // CYRL
    1, // DOS437
    1, // DOS850
    1, // DOS852
    1, // DOS857
    1, // DOS860
    1, // DOS861
    1, // DOS863
    1, // DOS865
    2, // EUCJ_0208
    2, // GB_2312
    1, // ISO8859_1
    1, // ISO8859_2
    2, // KSC_5601
    1, // NEXT
    1, // OCTETS
    2, // SJIS_0208
    3, // UNICODE_FSS
{FB20_UP}
    4,  // UTF8 !! 3 for FB < FB20 (UTF8 ALIAS UNICODE_FSS) !!
{FB20_UP}
    1, // WIN1250
    1, // WIN1251
    1, // WIN1252
    1, // WIN1253
    1  // WIN1254
{FB15_UP}
   ,1  // DOS737'
   ,1  // DOS775
   ,1  // DOS858
   ,1  // DOS862
   ,1  // DOS864
   ,1  // DOS866
   ,1  // DOS869
   ,1  // WIN1255
   ,1  // WIN1256
   ,1  // WIN1257
   ,1  // ISO8859_3
   ,1  // ISO8859_4
   ,1  // ISO8859_5
   ,1  // ISO8859_6
   ,1  // ISO8859_7
   ,1  // ISO8859_8
   ,1  // ISO8859_9
   ,1  // ISO8859_13
{FB15_UP}
{FB20_UP}
   ,1  // KOI8R
   ,1  // KOI8U
{FB20_UP}
{FB21_UP}
   ,1  // WIN1258
   ,1  // TIS620
   ,2  // GBK
   ,2  // CP943C
{FB21_UP}
  );

  //function ALFBXMBUEncode(const str: UnicodeString; cp: Word): RawByteString;
  //function ALFBXMBUDecode(const str: RawByteString; cp: Word): UnicodeString; overload;
  //procedure ALFBXMBUDecode(str: PAnsiChar; size: Integer; cp: Word; buffer: PWideChar); overload;

  function ALFBXStrToCharacterSet(const CharacterSet: {RawByteString}AnsiString): TALFBXCharacterSet;
  function ALFBXCreateDBParams(Params: AnsiString; Delimiter: AnsiChar = ';'): AnsiString;

  //function ALFBXCreateTRParams(Options: TALFBXTransParams; const LockRead: string = ''; const LockWrite: string = ''{FB20_UP}; LockTimeout: Word = 0{FB20_UP}): RawByteString;
  function ALFBXCreateBlobParams(Params: AnsiString; Delimiter: AnsiChar = ';'): AnsiString;

//******************************************************************************
// Transaction
//******************************************************************************

const
  // Default Transaction Parameter
  TALFBXPBDefault = isc_tpb_version3 + isc_tpb_write + isc_tpb_concurrency + isc_tpb_wait;

//******************************************************************************
//  DSQL
//******************************************************************************

  //****************************************
  // TALFBXSQLDA
  //****************************************

const
  cALFBXMaxParamLength = 125;

type
  TALFBXParamsFlag = (pfNotInitialized, pfNotNullable);
  TALFBXParamsFlags = set of TALFBXParamsFlag;
  PALFBXSQLVar = ^TALFBXSQLVar;
  TALFBXSQLVar = record // size must be 152
    SqlType      : Smallint;
    SqlScale     : Smallint;
    SqlSubType   : Smallint;
    SqlLen       : Smallint;
    SqlData      : PAnsiChar;
    SqlInd       : PSmallint;
    case byte of
    // TALFBXSQLResult
    0 : ( SqlNameLength   : Smallint;
          SqlName         : array[0..METADATALENGTH-1] of AnsiChar;
          RelNameLength   : Smallint;
          RelName         : array[0..METADATALENGTH-1] of AnsiChar;
          OwnNameLength   : Smallint;
          OwnName         : array[0..METADATALENGTH-1] of AnsiChar;
          AliasNameLength : Smallint;
          AliasName       : array[0..METADATALENGTH-1] of AnsiChar;
          );
    // TALFBXSQLParam
    1 : ( Flags           : TALFBXParamsFlags;
          ID              : Word;
          MaxSqlLen       : Smallint;
          ParamNameLength : Smallint;
          ParamName       : array[0..cALFBXMaxParamLength-1] of AnsiChar;
          );
  end;

  PALFBXSQLDaData = ^TALFBXSQLDaData;
  TALFBXSQLDaData = record
    version : Smallint;                // version of this XSQLDA
    sqldaid : array[0..7] of AnsiChar; // XSQLDA name field          ->  RESERVED
    sqldabc : ISCLong;                 // length in bytes of SQLDA   ->  RESERVED
    sqln    : Smallint;                // number of fields allocated
    sqld    : Smallint;                // actual number of fields
    sqlvar: array[Word] of TALFBXSQLVar; // first field address
  end;

  TALFBXStatementType = (
    stSelect,             //  select                 SELECT
    stInsert,             //  insert                 INSERT INTO
    stUpdate,             //  update                 UPDATE
    stDelete,             //  delete                 DELETE FROM
    stDDL,                //
    stGetSegment,         //  blob                   READ BLOB
    stPutSegment,         //  blob                   INSERT BLOB
    stExecProcedure,      //  invoke_procedure       EXECUTE PROCEDURE
    stStartTrans,         //  declare                DECLARE
    stCommit,             //  commit                 COMMIT
    stRollback,           //  rollback               ROLLBACK [WORK]
    stSelectForUpdate,    //                         SELECT ... FOR UPDATE
    stSetGenerator
  {FB15_UP}
    ,stSavePoint          //  user_savepoint | undo_savepoint       SAVEPOINT | ROLLBACK [WORK] TO
  {FB15_UP}
  );

(******************************************************************************)
(* Abstract Class                                                             *)
(******************************************************************************)

type
  TALFBXSQLDA = class
  private
    FXSQLDA: PALFBXSQLDaData;
    FCharacterSet: TALFBXCharacterSet;
  protected
    function GetAllocatedFields: Word; virtual;
    procedure SetAllocatedFields(Fields: Word); virtual;
    function GetFieldCount: Integer; virtual;
    function GetSQLType(const Index: Word): Smallint; virtual;
    function GetSQLLen(const Index: Word): Smallint; virtual;
    function GetSQLScale(const Index: Word): Smallint; virtual;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Int64); overload; virtual;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Double); overload; virtual;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Integer); overload; virtual;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Single); overload; virtual;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Smallint); overload; virtual;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: TDateTime); overload; virtual;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Currency); overload; virtual;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: boolean); overload; virtual;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Cardinal); overload; virtual;
    procedure ConvertStringToDate(const Code: Smallint; Index: Word; out value: Integer); virtual;
    //procedure DecodeStringB(const Code: Smallint; Index: Word; out Str: RawByteString); virtual;
    //procedure DecodeStringW(const Code: Smallint; Index: Word; out Str: UnicodeString); virtual;
    procedure DecodeStringA(const Code: Smallint; Index: Word; out Str: AnsiString); overload; virtual;
    //procedure DecodeString(const Code: Smallint; Index: Word; out Str: string); overload; virtual;
    //function DecodeString(const Code: Smallint; Index: Word): string; overload; virtual;
    function DecodeStringA(const Code: Smallint; Index: Word): AnsiString; overload; virtual;
    procedure EncodeStringA(Code: Smallint; Index: Word; const str: AnsiString); virtual;
    //procedure EncodeStringW(Code: Smallint; Index: Word; const str: UnicodeString); virtual;
    //procedure EncodeStringB(Code: Smallint; Index: Word; const str: RawByteString); virtual;
    //procedure EncodeString(Code: Smallint; Index: Word; const str: string); virtual;
    //function GetAsString(const Index: Word): string; virtual;
    //procedure SetAsString(const Index: Word; const Value: string); virtual;
    //function GetByNameAsString(const name: string): string; virtual;
    //procedure SetByNameAsString(const name, Value: string); virtual;
    //function GetByNameAsRawByteString(const name: string): RawByteString; virtual;
    //procedure SetByNameAsRawByteString(const name: string;
    //  const Value: RawByteString); virtual;
  protected
    function GetSqlName(const Index: Word): AnsiString; virtual;
    function GetRelName(const Index: Word): AnsiString; virtual;
    function GetOwnName(const Index: Word): AnsiString; virtual;
    function GetAliasName(const Index: Word): AnsiString; virtual;

    function GetFieldType(const Index: Word): TALFBXFieldType; virtual;

    function GetIsNumeric(const Index: Word): boolean; virtual;
    function GetIsBlob(const Index: Word): boolean; virtual;
    function GetIsBlobText(const Index: Word): boolean; virtual;
    function GetIsArray(const Index: Word): boolean; virtual;
    function GetIsNullable(const Index: Word): boolean; virtual;

    function GetIsNull(const Index: Word): boolean; virtual;
    function GetAsDouble(const Index: Word): Double; virtual;
    function GetAsCurrency(const Index: Word): Currency; virtual;
    function GetAsInt64(const Index: Word): Int64; virtual;
    function GetAsInteger(const Index: Word): Integer; virtual;
    function GetAsSingle(const Index: Word): Single; virtual;
    function GetAsSmallint(const Index: Word): Smallint; virtual;
    //function GetAsRawByteString(const Index: Word): RawByteString; virtual;
    function GetAsAnsiString(const Index: Word): AnsiString; virtual;
    //function GetAsUnicodeString(const Index: Word): UnicodeString; virtual;
    function GetAsQuad(const Index: Word): TISCQuad; virtual;
    //function GetAsVariant(const Index: Word): Variant; virtual;
    function GetAsDateTime(const Index: Word): TDateTime; virtual;
    function GetAsDate(const Index: Word): Integer; virtual;
    function GetAsTime(const Index: Word): Cardinal; virtual;
    function GetAsBoolean(const Index: Word): boolean; virtual;

    procedure SetIsNull(const Index: Word; const Value: boolean); virtual;
    procedure SetAsDouble(const Index: Word; const Value: Double); virtual;
    procedure SetAsCurrency(const Index: Word; const Value: Currency); virtual;
    procedure SetAsInt64(const Index: Word; const Value: Int64); virtual;
    procedure SetAsInteger(const Index: Word; const Value: Integer); virtual;
    procedure SetAsSingle(const Index: Word; const Value: Single); virtual;
    procedure SetAsSmallint(const Index: Word; const Value: Smallint); virtual;
    //procedure SetAsRawByteString(const Index: Word; const Value: RawByteString); virtual;
    procedure SetAsAnsiString(const Index: Word; const Value: AnsiString); virtual;
    //procedure SetAsUnicodeString(const Index: Word; const Value: UnicodeString); virtual;
    procedure SetAsQuad(const Index: Word; const Value: TISCQuad); virtual;
    procedure SetAsDateTime(const Index: Word; const Value: TDateTime); virtual;
    procedure SetAsBoolean(const Index: Word; const Value: Boolean); virtual;
    procedure SetAsDate(const Index: Word; const Value: Integer); virtual;
    procedure SetAsTime(const Index: Word; const Value: Cardinal); virtual;
    //procedure SetAsVariant(const Index: Word; const Value: Variant); virtual;

    function GetByNameIsNumeric(const Name: AnsiString): boolean;
    function GetByNameIsBlob(const Name: AnsiString): boolean;
    function GetByNameIsBlobText(const Name: AnsiString): boolean;
    function GetByNameIsNull(const Name: AnsiString): boolean;
    function GetByNameIsNullable(const Name: AnsiString): boolean;

    function GetByNameAsDouble(const Name: AnsiString): Double;
    function GetByNameAsCurrency(const Name: AnsiString): Currency;
    function GetByNameAsInt64(const Name: AnsiString): Int64;
    function GetByNameAsInteger(const Name: AnsiString): Integer;
    function GetByNameAsSingle(const Name: AnsiString): Single;
    function GetByNameAsSmallint(const Name: AnsiString): Smallint;
    function GetByNameAsAnsiString(const Name: AnsiString): AnsiString;
    //function GetByNameAsUnicodeString(const Name: string): UnicodeString;
    function GetByNameAsQuad(const Name: AnsiString): TISCQuad;
    //function GetByNameAsVariant(const Name: string): Variant;
    function GetByNameAsDateTime(const Name: AnsiString): TDateTime;
    function GetByNameAsBoolean(const Name: AnsiString): boolean;
    function GetByNameAsDate(const Name: AnsiString): Integer;
    function GetByNameAsTime(const Name: AnsiString): Cardinal;

    procedure SetByNameIsNull(const Name: AnsiString; const Value: boolean);
    procedure SetByNameAsDouble(const Name: AnsiString; const Value: Double);
    procedure SetByNameAsCurrency(const Name: AnsiString; const Value: Currency);
    procedure SetByNameAsInt64(const Name: AnsiString; const Value: Int64);
    procedure SetByNameAsInteger(const Name: AnsiString; const Value: Integer);
    procedure SetByNameAsSingle(const Name: AnsiString; const Value: Single);
    procedure SetByNameAsSmallint(const Name: AnsiString; const Value: Smallint);
    procedure SetByNameAsAnsiString(const Name: AnsiString; const Value: AnsiString);
    //procedure SetByNameAsUnicodeString(const Name: string; const Value: UnicodeString);
    procedure SetByNameAsQuad(const Name: AnsiString; const Value: TISCQuad);
    procedure SetByNameAsDateTime(const Name: AnsiString; const Value: TDateTime);
    procedure SetByNameAsBoolean(const Name: AnsiString; const Value: boolean);
    procedure SetByNameAsDate(const Name: AnsiString; const Value: Integer);
    //procedure SetByNameAsVariant(const Name: string; const Value: Variant);
  public
    constructor Create(aCharacterSet: TALFBXCharacterSet); virtual;
    procedure CheckRange(const Index: Word); virtual;
    function GetFieldIndex(const name: AnsiString): Word; virtual;
    function TryGetFieldIndex(const name: AnsiString; out index: Word): Boolean; virtual;
    property Data: PALFBXSQLDaData read FXSQLDA;
    property IsBlob[const Index: Word]: boolean read GetIsBlob;
    property IsBlobText[const Index: Word]: boolean read GetIsBlobText;
    property IsNullable[const Index: Word]: boolean read GetIsNullable;
    property IsNumeric[const Index: Word]: boolean read GetIsNumeric;

    property FieldCount: Integer read GetFieldCount;
    property SQLType[const Index: Word]: Smallint read GetSQLType;
    property SQLLen[const Index: Word]: Smallint read GetSQLLen;
    property SQLScale[const Index: Word]: Smallint read GetSQLScale;
    property FieldType[const Index: Word]: TALFBXFieldType read GetFieldType;

    property CharacterSet: TALFBXCharacterSet read FCharacterSet write FCharacterSet;

    property IsNull       [const Index: Word]: boolean    read GetIsNull       write SetIsNull;
    property AsSmallint   [const Index: Word]: Smallint   read GetAsSmallint   write SetAsSmallint;
    property AsInteger    [const Index: Word]: Integer    read GetAsInteger    write SetAsInteger;
    property AsSingle     [const Index: Word]: Single     read GetAsSingle     write SetAsSingle;
    property AsDouble     [const Index: Word]: Double     read GetAsDouble     write SetAsDouble;
    property AsCurrency   [const Index: Word]: Currency   read GetAsCurrency   write SetAsCurrency;
    property AsInt64      [const Index: Word]: Int64      read GetAsInt64      write SetAsInt64;
    //property AsString     [const Index: Word]: string     read GetAsString write SetAsString;
    //property AsRawByteString[const Index: Word]: RawByteString  read GetAsRawByteString write SetAsRawByteString;
    property AsAnsiString [const Index: Word]: AnsiString  read GetAsAnsiString     write SetAsAnsiString;
    //property AsUnicodeString [const Index: Word]: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
    property AsQuad       [const Index: Word]: TISCQuad   read GetAsQuad       write SetAsQuad;
    property AsDateTime   [const Index: Word]: TDateTime  read GetAsDateTime   write SetAsDateTime;
    property AsBoolean    [const Index: Word]: Boolean    read GetAsBoolean    write SetAsBoolean;
    property AsDate       [const Index: Word]: Integer    read GetAsDate       write SetAsDate;
    property AsTime       [const Index: Word]: Cardinal   read GetAsTime       write SetAsTime;
    //property AsVariant    [const Index: Word]: Variant    read GetAsVariant    write SetAsVariant;

    property ByNameIsBlob       [const name: AnsiString]: boolean    read GetByNameIsBlob;
    property ByNameIsBlobText   [const name: AnsiString]: boolean    read GetByNameIsBlobText;
    property ByNameIsNull       [const name: AnsiString]: boolean    read GetByNameIsNull       write SetByNameIsNull;
    property ByNameAsSmallint   [const name: AnsiString]: Smallint   read GetByNameAsSmallint   write SetByNameAsSmallint;
    property ByNameAsInteger    [const name: AnsiString]: Integer    read GetByNameAsInteger    write SetByNameAsInteger;
    property ByNameAsSingle     [const name: AnsiString]: Single     read GetByNameAsSingle     write SetByNameAsSingle;
    property ByNameAsDouble     [const name: AnsiString]: Double     read GetByNameAsDouble     write SetByNameAsDouble;
    property ByNameAsCurrency   [const name: AnsiString]: Currency   read GetByNameAsCurrency   write SetByNameAsCurrency;
    property ByNameAsInt64      [const name: AnsiString]: Int64      read GetByNameAsInt64      write SetByNameAsInt64;
    //property ByNameAsString     [const name: string]: string     read GetByNameAsString     write SetByNameAsString;
    property ByNameAsAnsiString     [const name: AnsiString]: AnsiString     read GetByNameAsAnsiString     write SetByNameAsAnsiString;
    //property ByNameAsRawByteString  [const name: string]: RawByteString     read GetByNameAsRawByteString     write SetByNameAsRawByteString;
    //property ByNameAsUnicodeString [const name: string]: UnicodeString read GetByNameAsUnicodeString write SetByNameAsUnicodeString;
    property ByNameAsQuad       [const name: AnsiString]: TISCQuad   read GetByNameAsQuad       write SetByNameAsQuad;
    //property ByNameAsVariant    [const name: string]: Variant    read GetByNameAsVariant    write SetByNameAsVariant;
    property ByNameAsDateTime   [const name: AnsiString]: TDateTime  read GetByNameAsDateTime   write SetByNameAsDateTime;
    property ByNameAsBoolean    [const name: AnsiString]: Boolean    read GetByNameAsBoolean    write SetByNameAsBoolean;
    property ByNameAsDate       [const name: AnsiString]: Integer    read GetByNameAsDate       write SetByNameAsDate;
  end;
{ TALFBXPoolStream }

  PALFBXPtrArray = ^TALFBXPtrArray;
  TALFBXPtrArray = array[0..(MAXLONGINT div SizeOf(Pointer)) - 1] of Pointer;

  TALFBXPoolStream = class(TStream)
  private
    FItemsInPage: Integer;
    FItemSize: Integer;
    FItemCount: Integer;
    FPageSize: Integer;
    FInternalPageSize: Integer;
    FPages: PALFBXPtrArray;
    FPageCount: integer;
    FSize: Integer;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(ItemsInPage, ItemSize: Integer); virtual;
    destructor Destroy; override;
    procedure Clear;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function SeekTo(Item: Integer): Longint;
    function Get(Item: Integer): Pointer;
    function Add: Pointer;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: AnsiString);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: AnsiString);
    property ItemsInPage: Integer read FItemsInPage;
    property ItemSize: Integer read FItemSize;
    property PageSize: Integer read FPageSize;
    property ItemCount: Integer read FItemCount;
    property Items[index: integer]: Pointer read Get; default;
  end;

(******************************************************************************)
(* SQL Result set                                                             *)
(******************************************************************************)

  PALFBXBlobData = ^TALFBXBlobData;
  TALFBXBlobData = packed record
    Size: Integer;
    Buffer: Pointer;
  end;

  PALFBXArrayDesc = ^TALFBXArrayDesc;
  TALFBXArrayDesc = TISCArrayDesc;
  TALFBXBlobDesc = TISCBlobDesc;

  PALFBXArrayInfo = ^TALFBXArrayInfo;
  TALFBXArrayInfo = record
    index: Integer;
    size: integer;
    info: TALFBXArrayDesc;
  end;

  TALFBXSQLResult = class(TALFBXSQLDA)
  private
    FRecordPool: TALFBXPoolStream;
    FCachedFetch: boolean;
    FFetchBlobs: boolean;
    FDataBuffer: Pointer;
    FDataBufferLength: PtrInt;
    FBlobsIndex: array of Word;
    FCurrentRecord: Integer;
    FBufferChunks: Cardinal;
    FScrollEOF: boolean;
    FInMemoryEOF: boolean;
    FArrayInfos: array of TALFBXArrayInfo;
    FStatBlobsSize: Int64;
  protected
    procedure AddCurrentRecord; virtual;
    procedure FreeBlobs(Buffer: Pointer); virtual;
    function GetRecordCount: Integer; virtual;
    function GetCurrentRecord: Integer; virtual;
    procedure AllocateDataBuffer; virtual;
    function GetEof: boolean; virtual;
    function GetUniqueRelationName: AnsiString; virtual;
    function GetBof: boolean; virtual;
    function GetDataQuadOffset(const index: word): Pointer; virtual;
    function GetBlobData(const index: word): PALFBXBlobData; virtual;
    function GetArrayData(const index: word): Pointer; virtual;
    function GetArrayCount: Word; virtual;
    function GetArrayInfos(const index: word): PALFBXArrayInfo; virtual;
  protected
    //function GetAsRawByteString(const Index: Word): RawByteString; override;
    function GetAsAnsiString(const Index: Word): AnsiString; override;
    //function GetAsUnicodeString(const Index: Word): UnicodeString; override;
    //function GetAsVariant(const Index: Word): Variant; override;
  public
    constructor Create(Charset: TALFBXCharacterSet; Fields: SmallInt = 0;
      CachedFetch: Boolean = False;
      FetchBlobs: boolean = false;
      BufferChunks: Cardinal = 1000); reintroduce;
    destructor Destroy; override;
    procedure ClearRecords; virtual;
    procedure GetRecord(const Index: Integer); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Next; virtual;

    property BlobData[const index: word]: PALFBXBlobData read GetBlobData;

    property ArrayData[const index: word]: Pointer read GetArrayData;
    property ArrayInfos[const index: word]: PALFBXArrayInfo read GetArrayInfos;
    property ArrayCount: Word read GetArrayCount;

    procedure ReadBlob(const Index: Word; Stream: TStream); overload; virtual;
    //procedure ReadBlobB(const Index: Word; var data: RawByteString); overload; virtual;
    procedure ReadBlobA(const Index: Word; var str: AnsiString); overload; virtual;
    //procedure ReadBlobW(const Index: Word; var str: UnicodeString); overload; virtual;
    //procedure ReadBlob(const Index: Word; var str: string); overload; virtual;
    //function ReadBlobB(const Index: Word): RawByteString; overload; virtual;
    function ReadBlobA(const Index: Word): AnsiString; overload; virtual;
    //function ReadBlobW(const Index: Word): UnicodeString; overload; virtual;
    //function ReadBlob(const Index: Word): string; overload; virtual;
    //procedure ReadBlob(const Index: Word; var Value: Variant); overload; virtual;
    procedure ReadBlob(const Index: Word; Data: Pointer); overload; virtual;
    procedure ReadBlob(const name: AnsiString; Stream: TStream); overload; virtual;
    //procedure ReadBlobB(const name: string; var data: RawByteString); overload; virtual;
    procedure ReadBlobA(const name: AnsiString; var str: AnsiString); overload; virtual;
    //procedure ReadBlobW(const name: string; var str: UnicodeString); overload; virtual;
    //procedure ReadBlob(const name: string; var str: string); overload; virtual;
    //function ReadBlobB(const name: string): RawByteString; overload; virtual;
    function ReadBlobA(const name: AnsiString): AnsiString; overload; virtual;
    //function ReadBlobW(const name: string): UnicodeString; overload; virtual;
    //function ReadBlob(const name: string): string; overload; virtual;
    //procedure ReadBlob(const name: string; var Value: Variant); overload; virtual;
    procedure ReadBlob(const name: AnsiString; Data: Pointer); overload; virtual;

    function GetBlobSize(const Index: Word): Cardinal; virtual;

    property Eof: boolean read GetEof;
    property ScrollEOF: boolean read FScrollEOF;
    property Bof: boolean read GetBof;

    property CachedFetch: boolean read FCachedFetch;
    property FetchBlobs: boolean read FFetchBlobs;
    property RecordCount: Integer read GetRecordCount;
    property CurrentRecord: Integer read GetCurrentRecord write GetRecord;
    property BufferChunks: Cardinal read FBufferChunks;
    property UniqueRelationName: AnsiString read GetUniqueRelationName;

    property SqlName[const Index: Word]: AnsiString read GetSqlName;
    property RelName[const Index: Word]: AnsiString read GetRelName;
    property OwnName[const Index: Word]: AnsiString read GetOwnName;
    property AliasName[const Index: Word]: AnsiString read GetAliasName;

    property AsSmallint   [const Index: Word]: Smallint   read GetAsSmallint;
    property AsInteger    [const Index: Word]: Integer    read GetAsInteger;
    property AsSingle     [const Index: Word]: Single     read GetAsSingle;
    property AsDouble     [const Index: Word]: Double     read GetAsDouble;
    property AsCurrency   [const Index: Word]: Currency   read GetAsCurrency;
    property AsInt64      [const Index: Word]: Int64      read GetAsInt64;
    //property AsRawByteString [const Index: Word]: RawByteString read GetAsRawByteString;
    property AsAnsiString [const Index: Word]: AnsiString read GetAsAnsiString;
    //property AsUnicodeString [const Index: Word]: UnicodeString read GetAsUnicodeString;
    //property AsVariant    [const Index: Word]: Variant    read GetAsVariant;
    property AsDateTime   [const Index: Word]: TDateTime  read GetAsDateTime;
    property AsDate       [const Index: Word]: Integer    read GetAsDate;
    property AsTime       [const Index: Word]: Cardinal   read GetAsTime;
    property AsBoolean    [const Index: Word]: Boolean    read GetAsBoolean;

    property ByNameIsNull[const name: AnsiString]: boolean read GetByNameIsNull;
    property ByNameIsNullable[const name: AnsiString]: boolean read GetByNameIsNullable;

    property ByNameAsSmallint   [const name: AnsiString]: Smallint   read GetByNameAsSmallint;
    property ByNameAsInteger    [const name: AnsiString]: Integer    read GetByNameAsInteger;
    property ByNameAsSingle     [const name: AnsiString]: Single     read GetByNameAsSingle;
    property ByNameAsDouble     [const name: AnsiString]: Double     read GetByNameAsDouble;
    property ByNameAsCurrency   [const name: AnsiString]: Currency   read GetByNameAsCurrency;
    property ByNameAsInt64      [const name: AnsiString]: Int64      read GetByNameAsInt64;
    property ByNameAsAnsiString     [const name: AnsiString]: AnsiString     read GetByNameAsAnsiString;
    //property ByNameAsUnicodeString [const name: string]: UnicodeString read GetByNameAsUnicodeString;
    property ByNameAsQuad       [const name: AnsiString]: TISCQuad   read GetByNameAsQuad;
    //property ByNameAsVariant    [const name: string]: Variant    read GetByNameAsVariant;
    property ByNameAsDateTime   [const name: AnsiString]: TDateTime  read GetByNameAsDateTime;
    property ByNameAsBoolean    [const name: AnsiString]: Boolean    read GetByNameAsBoolean;
    property ByNameAsDate       [const name: AnsiString]: Integer    read GetByNameAsDate;
    property ByNameAsTime       [const name: AnsiString]: Cardinal   read GetByNameAsTime;

    //property Values[const name: string]: Variant read GetByNameAsVariant; default;
  end;

  TALFBXSQLResultClass = class of TALFBXSQLResult;

(******************************************************************************)
(* SQL Params                                                                 *)
(******************************************************************************)

  TALFBXSQLParams = class(TALFBXSQLDA)
  private
    FParamCount: Word;
  protected
    function GetFieldName(const Index: Word): AnsiString; virtual;
    procedure AllocateDataBuffer(AInit: boolean = true); virtual;
    function GetMaxSqlLen(const Index: Word): SmallInt; virtual;

    function AddFieldA(const name: AnsiString): Word; virtual;
    //function AddFieldW(const name: UnicodeString): Word; virtual;
    //function AddField(const name: string): Word; virtual;
    procedure SetFieldType(const Index: Word; Size: Integer; Code: SmallInt; Scale: Smallint); virtual;

    function GetIsNullable(const Index: Word): boolean; override;

    procedure SetAsDouble(const Index: Word; const Value: Double); override;
    procedure SetAsCurrency(const Index: Word; const Value: Currency); override;
    procedure SetAsInt64(const Index: Word; const Value: Int64); override;
    procedure SetAsInteger(const Index: Word; const Value: Integer); override;
    procedure SetAsSingle(const Index: Word; const Value: Single); override;
    procedure SetAsSmallint(const Index: Word; const Value: Smallint); override;
    //procedure SetAsRawByteString(const Index: Word; const Value: RawByteString); override;
    procedure SetAsAnsiString(const Index: Word; const Value: AnsiString); override;
    //procedure SetAsUnicodeString(const Index: Word; const Value: UnicodeString); override;
    procedure SetAsQuad(const Index: Word; const Value: TISCQuad); override;
    procedure SetAsDateTime(const Index: Word; const Value: TDateTime); override;
    procedure SetAsBoolean(const Index: Word; const Value: Boolean); override;
    procedure SetAsDate(const Index: Word; const Value: Integer); override;
    procedure SetAsTime(const Index: Word; const Value: Cardinal); override;

    function GetFieldType(const Index: Word): TALFBXFieldType; override;
  public
    constructor Create(Charset: TALFBXCharacterSet); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Parse(const SQL: AnsiString): AnsiString; virtual;
    function TryGetFieldIndex(const name: AnsiString; out Index: Word): Boolean; override;
    function GetFieldIndex(const name: AnsiString): Word; override;
    // don't use this method
    procedure AddFieldType(const Name: AnsiString; FieldType: TALFBXFieldType;
      Scale: TALFBXScale = 1; Precision: byte = 0); virtual;

    //property Values[const name: string]: Variant read GetByNameAsVariant; default;
    property FieldName[const Index: Word]: AnsiString read GetFieldName;
    property ParamCount : Word read FParamCount;
    property MaxSqlLen[const Index: Word]: Smallint read GetMaxSqlLen;

  end;

  TALFBXSQLParamsClass = class of TALFBXSQLParams;

(******************************************************************************)
(* Library                                                                    *)
(******************************************************************************)

type
  TALFBXDSQLInfoData = packed record
    InfoCode: byte;
    InfoLen : Word; // isc_portable_integer convert a SmallInt to Word ??? so just say it is a word
    case byte of
      isc_info_sql_stmt_type: (StatementType: TALFBXStatementType);
      isc_info_sql_get_plan : (PlanDesc     : array[0..255] of AnsiChar);
  end;

  TALFBXLibrary = class;

  TALFBXStatusVector = array[0..ISC_STATUS_LENGTH - 1] of ISCStatus;
  PALFBXStatusVector = ^TALFBXStatusVector;

  TALFBXOnConnectionLost = procedure(Lib: TALFBXLibrary) of object;
  TALFBXOnGetDBExceptionClass = procedure(Number: Integer; out Excep: EALFBXExceptionClass) of object;

  TALFBXLibrary = class(TALFBXBaseLibrary)
  private
    FOnConnectionLost: TALFBXOnConnectionLost;
    FOnGetDBExceptionClass: TALFBXOnGetDBExceptionClass;
    FRaiseErrors: boolean;
    FSegmentSize: Word;
    function GetSegmentSize: Word;
    procedure SetSegmentSize(Value: Word);
  public
    constructor Create(ApiVer: TALFBXVersion_Api); override;
    procedure CheckFBXApiCall(const Status: ISCStatus; StatusVector: TALFBXStatusVector);

    property OnConnectionLost: TALFBXOnConnectionLost read FOnConnectionLost write FOnConnectionLost;
    property OnGetDBExceptionClass: TALFBXOnGetDBExceptionClass read FOnGetDBExceptionClass write FOnGetDBExceptionClass;
    property RaiseErrors: boolean read FRaiseErrors write FRaiseErrors default True;


{FB25_UP}
    function ServerShutdown(timeout: Cardinal; const reason: Integer): Integer;
    procedure ServerShutdownCallback(callBack: FB_SHUTDOWN_CALLBACK;
      const mask: Integer; arg: Pointer);
{FB25_UP}

    {Attaches to an existing database.
     Ex: AttachDatabase('c:\DataBase.gdb', DBHandle, 'user_name=SYSDBA; password=masterkey'); }
    procedure AttachDatabase(const FileName: {RawByteString}AnsiString; var DbHandle: IscDbHandle; Params: AnsiString; Sep: AnsiChar = ';');
    {Detaches from a database previously connected with AttachDatabase.}
    procedure DetachDatabase(var DBHandle: IscDbHandle);
    procedure DatabaseInfo(var DBHandle: IscDbHandle; const Items: AnsiString; var Buffer: AnsiString); overload;
    function DatabaseInfoIntValue(var DBHandle: IscDbHandle; const item: AnsiChar): Integer;
    function DatabaseInfoString(var DBHandle: IscDbHandle; item: byte; size: Integer): AnsiString;
    function DatabaseInfoDateTime(var DBHandle: IscDbHandle; item: byte): TDateTime;
    procedure DatabaseDrop(DbHandle: IscDbHandle);
{FB25_UP}
    function DatabaseCancelOperation(var DBHandle: IscDbHandle; option: ISC_USHORT): Boolean;
{FB25_UP}

    procedure TransactionStart(var TraHandle: IscTrHandle; var DbHandle: IscDbHandle; const TPB: AnsiString = '');
    procedure TransactionStartMultiple(var TraHandle: IscTrHandle; DBCount: Smallint; Vector: PISCTEB);
    procedure TransactionCommit(var TraHandle: IscTrHandle);
    procedure TransactionRollback(var TraHandle: IscTrHandle);
    procedure TransactionCommitRetaining(var TraHandle: IscTrHandle);
    procedure TransactionPrepare(var TraHandle: IscTrHandle);
    procedure TransactionRollbackRetaining(var TraHandle: IscTrHandle);
    function TransactionGetId(var TraHandle: IscTrHandle): Cardinal;
    procedure DSQLExecuteImmediate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
      const Statement: {RawByteString}AnsiString; Dialect: Word; Sqlda: TALFBXSQLDA = nil); overload;
    procedure DSQLExecuteImmediate(const Statement: {RawByteString}AnsiString; Dialect: Word; Sqlda: TALFBXSQLDA = nil); overload;
    procedure DSQLAllocateStatement(var DBHandle: IscDbHandle; var StmtHandle: IscStmtHandle);
    function DSQLPrepare(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
      Statement: {RawByteString}AnsiString; Dialect: Word; Sqlda: TALFBXSQLResult = nil): TALFBXStatementType;
    procedure DSQLExecute(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
      Dialect: Word; Sqlda: TALFBXSQLDA = nil);
    procedure DSQLExecute2(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
      Dialect: Word; InSqlda: TALFBXSQLDA; OutSQLDA: TALFBXSQLResult);
    procedure DSQLFreeStatement(var StmtHandle: IscStmtHandle; Option: Word);
    function  DSQLFetch(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLResult): boolean;
    function  DSQLFetchWithBlobs(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle;
      var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLResult): boolean;
    procedure DSQLDescribe(var DbHandle: IscDbHandle; var TrHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLResult);
    procedure DSQLDescribeBind(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLParams);
    procedure DSQLSetCursorName(var StmtHandle: IscStmtHandle; const cursor: AnsiString);
    procedure DSQLExecImmed2(var DBHhandle: IscDbHandle; var TraHandle: IscTrHandle;
      const Statement: {RawByteString}AnsiString; dialect: Word; InSqlda, OutSQLDA: TALFBXSQLDA);

    procedure DSQLInfo(var StmtHandle: IscStmtHandle; const Items: array of byte; var buffer: AnsiString);
    function  DSQLInfoPlan(var StmtHandle: IscStmtHandle): AnsiString;
    function  DSQLInfoStatementType(var StmtHandle: IscStmtHandle): TALFBXStatementType;
    function  DSQLInfoRowsAffected(var StmtHandle: IscStmtHandle; StatementType: TALFBXStatementType): Cardinal;
    procedure DSQLInfoRowsAffected2(var StmtHandle: IscStmtHandle; out SelectedRows, InsertedRows, UpdatedRows, DeletedRows: Cardinal);

    procedure DDLExecute(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle; const ddl: AnsiString);

    function ArrayLookupBounds(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      const RelationName, FieldName: AnsiString): TALFBXArrayDesc;
    procedure ArrayGetSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      ArrayId: TISCQuad; var desc: TALFBXArrayDesc; DestArray: PPointer; var SliceLength: Integer);
    procedure ArrayPutSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      var ArrayId: TISCQuad; var desc: TALFBXArrayDesc; DestArray: Pointer;
      var SliceLength: Integer);
    procedure ArraySetDesc(const RelationName, FieldName: AnsiString; var SqlDtype,
      SqlLength, Dimensions: Smallint; var desc: TISCArrayDesc);

    procedure ServiceAttach(const ServiceName: {RawByteString}AnsiString;
      var SvcHandle: IscSvcHandle; const Spb: {RawByteString}AnsiString);
    procedure ServiceDetach(var SvcHandle: IscSvcHandle);
    procedure ServiceQuery(var SvcHandle: IscSvcHandle;
      const SendSpb, RequestSpb: {RawByteString}AnsiString; var Buffer: {RawByteString}AnsiString);
    procedure ServiceStart(var SvcHandle: IscSvcHandle; const Spb: {RawByteString}AnsiString);

    function ErrSqlcode(StatusVector: TALFBXStatusVector): ISCLong;
    function ErrInterprete(StatusVector: TALFBXStatusVector): {RawByteString}AnsiString;
    function ErrSQLInterprete(SQLCODE: Smallint): {RawByteString}AnsiString;
{FB25_UP}
    function ErrSqlState(StatusVector: TALFBXStatusVector): FB_SQLSTATE_STRING;
{FB25_UP}

    procedure BlobOpen(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
      var BlobHandle: IscBlobHandle; BlobId: TISCQuad; BPB: AnsiString = '');
    function  BlobGetSegment(var BlobHandle: IscBlobHandle;
      out length: Word; BufferLength: Cardinal; Buffer: Pointer): boolean;
    procedure BlobClose(var BlobHandle: IscBlobHandle);
    procedure BlobInfo(var BlobHandle: IscBlobHandle;
      out NumSegments, MaxSegment, TotalLength: Cardinal; out btype : byte);
    procedure BlobSize(var BlobHandle: IscBlobHandle; out Size: Cardinal);
    procedure BlobMaxSegment(var BlobHandle: IscBlobHandle; out Size: Cardinal);
    procedure BlobDefaultDesc(var Desc: TALFBXBlobDesc; const RelationName, FieldName: AnsiString);
    procedure BlobSaveToStream(var BlobHandle: IscBlobHandle; Stream: TStream);
    function  BlobReadString(var BlobHandle: IscBlobHandle): {RawByteString}AnsiString; overload;
    procedure BlobReadString(var BlobHandle: IscBlobHandle; var Str: {RawByteString}AnsiString); overload;
    procedure BlobReadVariant(var BlobHandle: IscBlobHandle; var Value: Variant);
    // you must free memory allocated by this method !!
    procedure BlobReadBuffer(var BlobHandle: IscBlobHandle; var Size: Integer;
      var Buffer: Pointer; realloc: boolean = false);
    // the buffer size if known and Pointer allocated.
    procedure BlobReadSizedBuffer(var BlobHandle: IscBlobHandle; Buffer: Pointer); overload;
    // DBexpress and SP: the component set the max blob size
    procedure BlobReadSizedBuffer(var BlobHandle: IscBlobHandle;
      Buffer: Pointer; MaxSize: Integer); overload;
    function  BlobCreate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
      var BlobHandle: IscBlobHandle; BPB: AnsiString = ''): TISCQuad;
    procedure BlobWriteSegment(var BlobHandle: IscBlobHandle; BufferLength: Cardinal; Buffer: Pointer);
    procedure BlobWriteString(var BlobHandle: IscBlobHandle; const Str: {RawByteString}AnsiString); overload;
    procedure BlobWriteStream(var BlobHandle: IscBlobHandle; Stream: TStream);

    function StreamBlobOpen(var BlobId: TISCQuad; var Database: IscDbHandle;
      var Transaction: IscTrHandle; mode: AnsiChar): PBStream;
    function StreamBlobClose(Stream: PBStream): integer;
    function EventBlock(var EventBuffer, ResultBuffer: PAnsiChar; Count: Smallint;
      v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: PAnsiChar): Integer;
    procedure EventQueue(var handle: IscDbHandle; var id: Integer; length: Word;
      events: PAnsiChar; ast: ISC_EVENT_CALLBACK; arg: Pointer);
    procedure EventCounts(var ResultVector: TALFBXStatusVector;
      BufferLength: Smallint; EventBuffer, ResultBuffer: PAnsiChar);
    procedure EventCancel(var DbHandle: IscDbHandle; var id: integer);
    procedure EventWaitFor(var handle: IscDbHandle; length: Smallint; events, buffer: Pointer);

    function IscFree(data: Pointer): Integer;
    property SegMentSize: Word read GetSegmentSize write SetSegmentSize;
  end;

//******************************************************************************
// Conversion
//******************************************************************************

const
  cAlFBXDateOffset = 15018;
  cALFBXTimeCoeff = 864000000;

  procedure ALFBXDecodeTimeStamp(v: PISCTimeStamp; out DateTime: Double); overload;
  procedure ALFBXDecodeTimeStamp(v: PISCTimeStamp; out TimeStamp: TTimeStamp); overload;
  function  ALFBXDecodeTimeStamp(v: PISCTimeStamp): Double; overload;
  procedure ALFBXDecodeSQLDate(v: Integer; out Year: SmallInt; out Month, Day: Word);
  procedure ALFBXDecodeSQLTime(v: Cardinal; out Hour, Minute, Second: Word;
    out Fractions: LongWord);
  procedure ALFBXEncodeTimeStamp(const DateTime: TDateTime; v: PISCTimeStamp); overload;
  procedure ALFBXEncodeTimeStamp(const Date: Integer; v: PISCTimeStamp); overload;
  procedure ALFBXEncodeTimeStamp(const Time: Cardinal; v: PISCTimeStamp); overload;
  function ALFBXEncodeSQLDate(Year: Integer; Month, Day: Integer): Integer;
  function ALFBXEncodeSQLTime(Hour, Minute, Second: Word;
    var Fractions: LongWord): Cardinal;

type
  TALFBXParamType = (
    prNone, // no param
    prByte, // Byte Param
    prShrt, // Short Param
    prCard, // Cardinal Param
    prStrg, // string Param
    prIgno  // Ignore Command
  );

  TALFBXDPBInfo = record
    Name      : AnsiString;
    ParamType : TALFBXParamType;
  end;

const

  cALFBXDPBInfos : array[1..isc_dpb_Max_Value] of TALFBXDPBInfo =
   ((Name: 'cdd_pathname';           ParamType: prIgno), // not implemented
    (Name: 'allocation';             ParamType: prIgno), // not implemented
    (Name: 'journal';                ParamType: prIgno), // not implemented
    (Name: 'page_size';              ParamType: prCard), // ok
    (Name: 'num_buffers';            ParamType: prCard), // ok
    (Name: 'buffer_length';          ParamType: prIgno), // not implemented
    (Name: 'debug';                  ParamType: prCard), // ok
    (Name: 'garbage_collect';        ParamType: prIgno), // not implemented
    (Name: 'verify';                 ParamType: prCard), // ok
    (Name: 'sweep';                  ParamType: prCard), // ok

    (Name: 'enable_journal';         ParamType: prStrg), // ok
    (Name: 'disable_journal';        ParamType: prNone), // ok
    (Name: 'dbkey_scope';            ParamType: prCard), // ok
    (Name: 'number_of_users';        ParamType: prIgno), // not implemented
    (Name: 'trace';                  ParamType: prNone), // ok
    (Name: 'no_garbage_collect';     ParamType: prNone), // not implemented
    (Name: 'damaged';                ParamType: prNone), // ok
    (Name: 'license';                ParamType: prStrg),
    (Name: 'sys_user_name';          ParamType: prStrg), // ok
    (Name: 'encrypt_key';            ParamType: prStrg), // ok

    (Name: 'activate_shadow';        ParamType: prNone), // ok deprecated
    (Name: 'sweep_interval';         ParamType: prCard), // ok
    (Name: 'delete_shadow';          ParamType: prNone), // ok
    (Name: 'force_write';            ParamType: prCard), // ok
    (Name: 'begin_log';              ParamType: prStrg), // ok
    (Name: 'quit_log';               ParamType: prNone), // ok
    (Name: 'no_reserve';             ParamType: prCard), // ok
    (Name: 'user_name';              ParamType: prStrg), // ok
    (Name: 'password';               ParamType: prStrg), // ok
    (Name: 'password_enc';           ParamType: prStrg), // ok

    (Name: 'sys_user_name_enc';      ParamType: prNone),
    (Name: 'interp';                 ParamType: prCard), // ok
    (Name: 'online_dump';            ParamType: prCard), // ok
    (Name: 'old_file_size';          ParamType: prCard), // ok
    (Name: 'old_num_files';          ParamType: prCard), // ok
    (Name: 'old_file';               ParamType: prStrg), // ok
    (Name: 'old_start_page';         ParamType: prCard), // ok
    (Name: 'old_start_seqno';        ParamType: prCard), // ok
    (Name: 'old_start_file';         ParamType: prCard), // ok
    (Name: 'drop_walfile';           ParamType: prCard), // ok

    (Name: 'old_dump_id';            ParamType: prCard), // ok
    (Name: 'wal_backup_dir';         ParamType: prStrg), // ok
    (Name: 'wal_chkptlen';           ParamType: prCard), // ok
    (Name: 'wal_numbufs';            ParamType: prCard), // ok
    (Name: 'wal_bufsize';            ParamType: prCard), // ok
    (Name: 'wal_grp_cmt_wait';       ParamType: prCard), // ok
    (Name: 'lc_messages';            ParamType: prStrg), // ok
    (Name: 'lc_ctype';               ParamType: prStrg), // ok
    (Name: 'cache_manager';          ParamType: prIgno), // not used in fb1.5
    (Name: 'shutdown';               ParamType: prCard), // ok

    (Name: 'online';                 ParamType: prNone), // ok
    (Name: 'shutdown_delay';         ParamType: prCard), // ok
    (Name: 'reserved';               ParamType: prStrg), // ok
    (Name: 'overwrite';              ParamType: prCard), // ok
    (Name: 'sec_attach';             ParamType: prCard), // ok
    (Name: 'disable_wal';            ParamType: prNone), // ok
    (Name: 'connect_timeout';        ParamType: prCard), // ok
    (Name: 'dummy_packet_interval';  ParamType: prCard), // ok
    (Name: 'gbak_attach';            ParamType: prStrg), // ok
    (Name: 'sql_role_name';          ParamType: prStrg), // ok rolename

    (Name: 'set_page_buffers';       ParamType: prCard), // ok Change age buffer 50 >= buf >= 65535 (default 2048)
    (Name: 'working_directory';      ParamType: prStrg), // ok
    (Name: 'sql_dialect';            ParamType: prCard), // ok Set SQL Dialect for this connection (1,2,3)
    (Name: 'set_db_readonly';        ParamType: prCard), // ok
    (Name: 'set_db_sql_dialect';     ParamType: prCard), // ok Change sqldialect (1,2,3))
    (Name: 'gfix_attach';            ParamType: prNone), // ok FB15: don't work
    (Name: 'gstat_attach';           ParamType: prNone)  // ok FB15: don't work

{FB103_UP}
   ,(Name: 'set_db_charset';         ParamType: prStrg) // ok
{FB103_UP}
{FB20_UP}
  ,(Name: 'gsec_attach';            ParamType: prByte)
  ,(Name: 'address_path';           ParamType: prStrg)
{FB20_UP}
{FB21_UP}
  ,(Name: 'process_id';             ParamType: prCard)
  ,(Name: 'no_db_triggers';         ParamType: prByte)
  ,(Name: 'trusted_auth';           ParamType: prStrg)
  ,(Name: 'process_name';           ParamType: prStrg)
{FB21_UP}
{FB25_UP}
  ,(Name: 'trusted_role';           ParamType: prNone)
  ,(Name: 'org_filename';           ParamType: prStrg)
  ,(Name: 'utf8_filename';          ParamType: prNone)
  ,(Name: 'ext_call_depth';         ParamType: prCard)
{FB25_UP}
   );

const

  cALFBXBPBInfos : array[1..isc_bpb_Max_Value] of TALFBXDPBInfo =
   ((Name: 'source_type';      ParamType: prShrt),
    (Name: 'target_type';      ParamType: prShrt),
    (Name: 'type';             ParamType: prShrt),
    (Name: 'source_interp';    ParamType: prShrt),
    (Name: 'target_interp';    ParamType: prShrt),
    (Name: 'filter_parameter'; ParamType: prIgno) // not implemented (FB 2.0)
   );

function ALFBXSQLQuote(const name: AnsiString): AnsiString;
function ALFBXSQLUnQuote(const name: AnsiString): AnsiString;

const
  cALFBXScaleDivisor: array[-15..-1] of Int64 = (1000000000000000,100000000000000,
    10000000000000,1000000000000,100000000000,10000000000,1000000000,100000000,
    10000000,1000000,100000,10000,1000,100,10);

  cALFBXScaleFormat: array[-15..-1] of AnsiString = (
    '0.0##############', '0.0#############', '0.0############', '0.0###########',
    '0.0##########', '0.0#########', '0.0########', '0.0#######', '0.0######',
    '0.0#####', '0.0####', '0.0###', '0.0##', '0.0#', '0.0');

  cALFBXCurrencyDivisor: array[-15..-1] of int64 = (100000000000,10000000000,
    1000000000,100000000,10000000,1000000,100000,10000,1000,100,10,1,10,100,
    1000);

implementation

uses System.Math,
     System.Variants,
     System.Ansistrings,
     ALFBXerror,
     ALFBXConst,
     ALCommon,
     ALString;

//function ALFBXMBUEncode(const str: UnicodeString; cp: Word): RawByteString;
//begin
//  if cp > 0 then
//  begin
//    SetLength(Result, WideCharToMultiByte(cp, 0, PWideChar(str), length(str), nil, 0, nil, nil));
//    WideCharToMultiByte(cp, 0, PWideChar(str), length(str), PAnsiChar(Result), Length(Result), nil, nil);
//  end else
//    Result := AnsiString(str);
//end;

//function ALFBXMBUDecode(const str: RawByteString; cp: Word): UnicodeString;
//begin
//  if cp > 0 then
//  begin
//    SetLength(Result, MultiByteToWideChar(cp, 0, PAnsiChar(str), length(str), nil, 0));
//    MultiByteToWideChar(cp, 0, PAnsiChar(str), length(str), PWideChar(Result), Length(Result));
//  end else
//    Result := UnicodeString(str);
//end;

//procedure ALFBXMBUDecode(str: PAnsiChar; size: Integer; cp: Word; buffer: PWideChar);
//var
//  len: Integer;
//begin
//  len := MultiByteToWideChar(cp, 0, str, size, nil, 0);
//  MultiByteToWideChar(cp, 0, str, size, buffer, len);
//  inc(buffer, len);
//  buffer^ := #0;
//end;

(******************************************************************************)
(* Errors handling                                                            *)
(******************************************************************************)

function ALFBXSQLQuote(const name: AnsiString): AnsiString;
var
  i, len: PtrInt;
begin
  len := Length(name);
  if (len > 1) and (name[1] = '"') and (name[len] = '"') then
  begin // allready quoted: keep case
    Result := name;
    Exit;
  end;
  if (len > 1) and (not (AnsiChar(name[1]) in ['A'..'Z'])) then
  begin // non standard carracter: keep case
    Result := '"' + name + '"';
    Exit;
  end;

  for i := 1 to len do
    if not (AnsiChar(name[i]) in ['A'..'Z', '0'..'9', '_', '$']) then
    begin // non standard carracter: keep case
      Result := '"' + name + '"';
      Exit;
    end;
  Result := ALUpperCase(name);
end;

function ALFBXSQLUnQuote(const name: AnsiString): AnsiString;
var
  i, len: PtrInt;
begin
  len := Length(name);
  if (len > 1) and (name[1] = '"') and (name[len] = '"') then
  begin  // allready quoted: keep case
    Result := ALCopyStr(name, 2, len-2);
    Exit;
  end;

  if (len > 1) and (not (AnsiChar(name[1]) in ['A'..'Z'])) then
  begin // non standard carracter: keep case
    Result := name;
    Exit;
  end;

  for i := 1 to len do
    if not (AnsiChar(name[i]) in ['A'..'Z', '0'..'9', '_', '$']) then
    begin // non standard carracter: keep case
      Result := name;
      Exit;
    end;
  Result := ALUpperCase(name);
end;

const
  ISC_MASK   = $14000000; // Defines the code as a valid ISC code
  FAC_MASK   = $00FF0000; // Specifies the facility where the code is located
  CODE_MASK  = $0000FFFF; // Specifies the code in the message file
  CLASS_MASK = $F0000000; // Defines the code as warning, error, info, or other

  // Note: Perhaps a debug level could be interesting !!!
  CLASS_ERROR   = 0; // Code represents an error
  CLASS_WARNING = 1; // Code represents a warning
  CLASS_INFO    = 2; // Code represents an information msg

  FAC_JRD        =  0;  // In Use
  FAC_QLI        =  1;
  FAC_GDEF       =  2;
  FAC_GFIX       =  3;  // In Use
  FAC_GPRE       =  4;
  FAC_GLTJ       =  5;
  FAC_GRST       =  6;
  FAC_DSQL       =  7;  // In Use
  FAC_DYN        =  8;  // In Use
  FAC_FRED       =  9;
  FAC_INSTALL    = 10;
  FAC_TEST       = 11;
  FAC_GBAK       = 12;  // In Use
  FAC_SQLERR     = 13;
  FAC_SQLWARN    = 14;
  FAC_JRD_BUGCHK = 15;
  FAC_GJRN       = 16;
  FAC_ISQL       = 17;
  FAC_GSEC       = 18;  // In Use
  FAC_LICENSE    = 19;  // In Use
  FAC_DOS        = 20;
  FAC_GSTAT      = 21;  // In Use


  function GetFacility(code: ISCStatus): Word;
  begin
    Result := (code and FAC_MASK) shr 16;
  end;

  function GetClass(code: ISCStatus): Word;
  begin
    Result := (code and CLASS_MASK) shr 30;
  end;

  function GETCode(code: ISCStatus): Word;
  begin
    Result := (code and CODE_MASK) shr 0;
  end;

  procedure TALFBXLibrary.CheckFBXApiCall(const Status: ISCStatus; StatusVector: TALFBXStatusVector);
  var
    Number: Integer;
    Excep: EALFBXExceptionClass;
    procedure RaiseException(const Excep: EALFBXExceptionClass);
    var
      Exception: EALFBXError;
    begin
      Exception := Excep.Create(String(ErrInterprete(StatusVector)));
      if Excep = EALFBXException then
        EALFBXException(Exception).FNumber := Number;
      Exception.FSQLCode   := ErrSqlcode(StatusVector);
      if Exception.FSQLCode <> 0 then
        Exception.Message := Exception.Message + String(ErrSQLInterprete(Exception.FSQLCode));
      Exception.FGDSCode := Status;
      Exception.FErrorCode := GETCode(Status);
{FB25_UP}
      Exception.FSQLState := ErrSqlState(StatusVector);
{FB25_UP}
      Exception.Message := Exception.Message + cALFBXNewLine +
        Format('GDS Code: %d - SQL Code: %d - Error Code: %d', [Exception.FGDSCode, Exception.FSQLCode, Exception.FErrorCode]);

      if ((Status = isc_lost_db_connection) or (Status = isc_network_error)
        or (Status = isc_shutdown)) and Assigned(FOnConnectionLost) then
          FOnConnectionLost(Self);

      raise Exception;
    end;
  begin
    if (Status <> 0) and FRaiseErrors then
	if (GetClass(Status) = CLASS_ERROR) then // only raise CLASS_ERROR
    begin
      case GetFacility(Status) of
        FAC_JRD     :
          if Status = isc_except then
        begin
          Number := StatusVector[3];
          if assigned(FOnGetDBExceptionClass) then
            FOnGetDBExceptionClass(Number, Excep) else
            Excep := EALFBXException;
        end else
          Excep := EALFBXError;
        FAC_GFIX    : Excep := EALFBXGFIXError;
        FAC_DSQL    : Excep := EALFBXDSQLError;
        FAC_DYN     : Excep := EALFBXDYNError;
        FAC_GBAK    : Excep := EALFBXGBAKError;
        FAC_GSEC    : Excep := EALFBXGSECError;
        FAC_LICENSE : Excep := EALFBXLICENSEError;
        FAC_GSTAT   : Excep := EALFBXGSTATError;
      else
        Excep := EALFBXError;
      end;
      RaiseException(Excep)
    end;
  end;

//******************************************************************************
// Database
//******************************************************************************

  constructor TALFBXLibrary.Create(ApiVer: TALFBXVersion_Api);
  begin
    inherited;
    FRaiseErrors := True;
    FSegmentSize := 16*1024;
  end;

  function ALFBXCreateDBParams(Params: AnsiString; Delimiter: AnsiChar = ';'): AnsiString;
  var
    BufferSize: Integer;
    CurPos, NextPos: PAnsiChar;
    CurStr, CurValue: AnsiString;
    EqualPos: PtrInt;
    Code: Byte;
    AValue: Integer;
    FinalSize: PtrInt;
    function Min(v1, v2: PtrInt): PtrInt;
    begin
      if v1 > v2 then Result := v2 else Result := v1;
    end;
    // dont reallocate memory each time, step by step ...
    procedure CheckBufferSize;
    begin
      while (FinalSize > BufferSize) do
        begin
          Inc(BufferSize, 32);
          SetLength(Result, BufferSize);
        end;
    end;
    procedure AddByte(AByte: Byte);
    begin
      inc(FinalSize);
      CheckBufferSize;
      Result[FinalSize] := AnsiChar(AByte);
    end;
    procedure AddWord(AWord: Word);
    begin
      inc(FinalSize,2);
      CheckBufferSize;
      PWord(@Result[FinalSize-1])^ := AWord;
    end;
    procedure AddCard(ACard: Cardinal);
    begin
      case ACard of
      0  ..   255 :
        begin
          AddByte(1);
          AddByte(Byte(ACard))
        end;
      256.. 65535 :
        begin
          AddByte(2);
          AddWord(Word(ACard))
        end;
      else
        AddByte(4);
        inc(FinalSize,4);
        CheckBufferSize;
        PCardinal(@Result[FinalSize-3])^ := ACard;
      end;
    end;
    procedure AddString(var AString: AnsiString);
    var l: PtrInt;
    begin
      l := Min(Length(AString), 255);
      inc(FinalSize,l+1);
      CheckBufferSize;
      Result[FinalSize-l] := AnsiChar(l);
      ALMove(PAnsiChar(AString)^, Result[FinalSize-l+1], l);
    end;
  begin
    FinalSize := 1;
    BufferSize := 32;
    SetLength(Result, BufferSize);
    Result[1] := AnsiChar(isc_dpb_version1);
    CurPos  := PAnsiChar(Params);
    while (CurPos <> nil) do
    begin
      NextPos := {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrScan(CurPos, Delimiter);
      if (NextPos = nil) then
        CurStr := CurPos else
        begin
          CurStr := ALCopyStr(CurPos, 0, NextPos-CurPos);
          Inc(NextPos);
        end;
      CurPos := NextPos;
      if (CurStr = '') then Continue;
      begin
        CurValue := '';
        EqualPos := ALPos(AnsiString('='), CurStr);
        if EqualPos <> 0 then
        begin
          CurValue := ALCopyStr(CurStr, EqualPos+1, Length(CurStr) - EqualPos);
          CurStr   := ALCopyStr(CurStr, 0, EqualPos-1);
        end;
        {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrLower(PAnsiChar(CurStr));
        CurStr := ALTrim(CurStr);
        CurValue := ALTrim(CurValue);
        for Code := 1 to isc_dpb_Max_Value do
          with cALFBXDPBInfos[Code] do
            if (Name = CurStr) then
            begin
              case ParamType of
                prNone :
                  begin
                    AddByte(Code);
                    AddByte(0);
                  end;
                prByte :
                  if ALTryStrToInt(CurValue, AValue) and (AValue >= 0) and (AValue <= 255) then
                  begin
                    AddByte(Code);
                    AddByte(Byte(AValue));
                  end;
                prCard :
                  if ALTryStrToInt(CurValue, AValue) and (AValue > 0) then
                  begin
                    AddByte(Code);
                    AddCard(AValue);
                  end;
                prStrg :
                  if (Length(CurValue) > 0) then
                  begin
                    AddByte(Code);
                    AddString(CurValue)
                  end;
              end;
              break;
            end;
      end;
    end;
    SetLength(Result, FinalSize);
  end;

//  function ALFBXCreateTRParams(Options: TALFBXTransParams; const LockRead, LockWrite: string{FB20_UP}; LockTimeout: Word{FB20_UP}): RawByteString;
//  var
//    tp: TALFBXTransParam;
//    procedure ParseStrOption(const code: AnsiChar; const Value: AnsiString);
//    var
//      P, Start: PAnsiChar;
//      S: AnsiString;
//    begin
//      P := Pointer(Value);
//      if P <> nil then
//        while (P^ <> #0) do
//        begin
//          Start := P;
//          while not (P^ in [#0, ';']) do Inc(P);
//          if (P - Start) > 0 then
//          begin
//            SetString(S, Start, P - Start);
//            Result := Result + code + AnsiChar(P - Start) + S;
//          end;
//          if P^ =';' then inc(P);
//        end;
//    end;
//  const
//    tpc: array[TALFBXTransParam] of AnsiChar = (
//      isc_tpb_consistency,
//      isc_tpb_concurrency,
//      isc_tpb_shared,
//      isc_tpb_protected,
//      isc_tpb_exclusive,
//      isc_tpb_wait,
//      isc_tpb_nowait,
//      isc_tpb_read,
//      isc_tpb_write,
//      isc_tpb_lock_read,
//      isc_tpb_lock_write,
//      isc_tpb_verb_time,
//      isc_tpb_commit_time,
//      isc_tpb_ignore_limbo,
//      isc_tpb_read_committed,
//      isc_tpb_autocommit,
//      isc_tpb_rec_version,
//      isc_tpb_no_rec_version,
//      isc_tpb_restart_requests,
//      isc_tpb_no_auto_undo
//    {FB20_UP}
//      ,isc_tpb_lock_timeout
//    {FB20_UP}
//      );
//
//  begin
//    if Options = [tpConcurrency,tpWait,tpWrite] then
//      result := ''
//    else
//      begin
//        Result := isc_tpb_version3;
//        for tp := Low(TALFBXTransParam) to High(TALFBXTransParam) do
//          if (tp in Options) then
//          begin
//            case tp of
//              tpLockRead    : ParseStrOption(tpc[tp], AnsiString(LockRead));
//              tpLockWrite   : ParseStrOption(tpc[tp], AnsiString(LockWrite));
//            {FB20_UP}
//              tpLockTimeout : Result := Result + tpc[tp] + PAnsiChar(@LockTimeout)[0] + PAnsiChar(LockTimeout)[1];
//            {FB20_UP}
//            else
//              Result := Result + tpc[tp];
//            end;
//          end;
//      end;
//  end;

{FB25_UP}
  function TALFBXLibrary.ServerShutdown(timeout: Cardinal; const reason: Integer): Integer;
  begin
     Result := fb_shutdown(timeout, reason);
  end;

  procedure TALFBXLibrary.ServerShutdownCallback(callBack: FB_SHUTDOWN_CALLBACK;
    const mask: Integer; arg: Pointer);
  var aStatusVector: TALFBXStatusVector;
  begin
    CheckFBXApiCall(fb_shutdown_callback(@aStatusVector, callBack, mask, arg), aStatusVector);
  end;
{FB25_UP}

  procedure TALFBXLibrary.AttachDatabase(const FileName: AnsiString; var DbHandle: IscDbHandle;
    Params: AnsiString; Sep: AnsiChar = ';');
  var aStatusVector: TALFBXStatusVector;
  begin
    Params := ALFBXCreateDBParams(Params, Sep);

      CheckFBXApiCall(isc_attach_database(@aStatusVector, Length(FileName), Pointer(FileName),
        @DBHandle, Length(Params), PAnsiChar(Params)), aStatusVector);

  end;

  procedure TALFBXLibrary.DetachDatabase(var DBHandle: IscDbHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_detach_database(@aStatusVector, @DBHandle), aStatusVector);
      // if connection lost DBHandle must be set manually to nil.
      DBHandle := nil;

  end;

  procedure TALFBXLibrary.DatabaseInfo(var DBHandle: IscDbHandle;
    const Items: AnsiString; var Buffer: AnsiString);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_database_info(@aStatusVector, @DBHandle, Length(Items),
        Pointer(Items), Length(Buffer), Pointer(Buffer)), aStatusVector);

  end;

  function TALFBXLibrary.DatabaseInfoIntValue(var DBHandle: IscDbHandle;
    const item: AnsiChar): Integer;
  var
    data: packed record
      item: AnsiChar;
      len: word;
      case byte of
        0: (vByte: Byte);
        1: (vSmallint: Smallint);
        2: (vInteger: Integer);
        3: (dummy: array[0..5] of byte);
    end;
    aStatusVector: TALFBXStatusVector;
  begin
    result := 0;

      CheckFBXApiCall(isc_database_info(@aStatusVector, @DBHandle, 1, @item,
        sizeof(data), @data), aStatusVector);
      if (data.item = item) then
        case data.len of
          0: ;
          1: result := data.vByte;
          2: result := data.vSmallint;
          4: result := data.vInteger;
        else
          raise exception.Create('Unexpected data size.');
        end else
          raise exception.Create('Invalid item identifier.');

  end;

  function TALFBXLibrary.DatabaseInfoDateTime(var DBHandle: IscDbHandle; item: byte): TDateTime;
  var
    data: packed record
      item: AnsiChar;
      len: word;
      date: TISCTimeStamp;
      dummy: word;
    end;
    aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_database_info(@aStatusVector, @DBHandle, 1, @item,
        sizeof(data), @data), aStatusVector);
      Result := ALFBXDecodeTimeStamp(@data.date);

  end;

  function TALFBXLibrary.DatabaseInfoString(var DBHandle: IscDbHandle;
    item: byte; size: Integer): AnsiString;
  var aStatusVector: TALFBXStatusVector;
  begin
    SetLength(result, size);

      while true do
      begin
        CheckFBXApiCall(isc_database_info(@aStatusVector, @DBHandle, 1, @item,
          Length(result), PAnsiChar(result)), aStatusVector);
        if result[1] = AnsiChar(isc_info_truncated) then
          SetLength(result, Length(result) + size) else
            if (byte(result[1]) = item) or (result[1] = #1) then
              Break else
              begin
                result := '';
                raise Exception.Create('');
              end;
      end;

  end;

  function ALFBXStrToCharacterSet(const CharacterSet: {RawByteString}AnsiString): TALFBXCharacterSet;
  var
    len: Integer;
  begin
    len := length(CharacterSet);
    if (len = 0) then
      Result := csNONE else
    begin
      for Result := low(TALFBXCharacterSet) to High(TALFBXCharacterSet) do
        if (len = Length(cALFBXCharacterSetStr[Result])) and
          ({$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrIComp(PAnsiChar(cALFBXCharacterSetStr[Result]), PAnsiChar(CharacterSet)) = 0) then
            Exit;
      raise Exception.CreateFmt(cALFBX_CHARSETNOTFOUND, [CharacterSet]);
    end;
  end;

  procedure TALFBXLibrary.DatabaseDrop(DbHandle: IscDbHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_drop_database(@aStatusVector, @DbHandle), aStatusVector);

  end;


{FB25_UP}
  function TALFBXLibrary.DatabaseCancelOperation(var DBHandle: IscDbHandle; option: ISC_USHORT): Boolean;
  var
    sv: TALFBXStatusVector;
  begin
    FillChar(sv, SizeOf(sv), 0);
    Result := fb_cancel_operation(@sv, @DbHandle, option) = 0;
  end;
{FB25_UP}

//******************************************************************************
// Transaction
//******************************************************************************

  procedure TALFBXLibrary.TransactionStart(var TraHandle: IscTrHandle; var DbHandle: IscDbHandle;
    const TPB: AnsiString = '');
  var Vector: TISCTEB;
  begin
    Vector.Handle  := @DbHandle;
    Vector.Len     := Length(TPB);
    Vector.Address := PAnsiChar(TPB);
    TransactionStartMultiple(TraHandle, 1, @Vector);
  end;

  procedure TALFBXLibrary.TransactionStartMultiple(var TraHandle: IscTrHandle; DBCount: Smallint; Vector: PISCTEB);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_start_multiple(@aStatusVector, @TraHandle, DBCount, Vector), aStatusVector);

  end;

  procedure TALFBXLibrary.TransactionCommit(var TraHandle: IscTrHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_commit_transaction(@aStatusVector, @TraHandle), aStatusVector);
      // if connection lost TraHandle must be set manually to nil.
      TraHandle := nil;

  end;

  procedure TALFBXLibrary.TransactionRollback(var TraHandle: IscTrHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_rollback_transaction(@aStatusVector, @TraHandle), aStatusVector);
      // if connection lost TraHandle must be set manually to nil.
      TraHandle := nil;

  end;

  procedure TALFBXLibrary.TransactionCommitRetaining(var TraHandle: IscTrHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_commit_retaining(@aStatusVector, @TraHandle), aStatusVector);

  end;

  procedure TALFBXLibrary.TransactionPrepare(var TraHandle: IscTrHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_prepare_transaction(@aStatusVector, @TraHandle), aStatusVector);

  end;

  procedure TALFBXLibrary.TransactionRollbackRetaining(var TraHandle: IscTrHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_rollback_retaining(@aStatusVector, @TraHandle), aStatusVector);

  end;

  function TALFBXLibrary.TransactionGetId(var TraHandle: IscTrHandle): Cardinal;
  var
    tra_items: AnsiChar;
    tra_info: array [0..31] of AnsiChar;
    aStatusVector: TALFBXStatusVector;
  begin
      tra_items := AnsiChar(isc_info_tra_id);
      CheckFBXApiCall(isc_transaction_info(@aStatusVector, @TraHandle,
        sizeof(tra_items), @tra_items, sizeof(tra_info), tra_info), aStatusVector);
      Result := PCardinal(tra_info + 3)^;
  end;

//******************************************************************************
// DSQL
//******************************************************************************

  function ALFBXGetSQLDAData(SQLDA: TALFBXSQLDA): Pointer;
  begin
    if (SQLDA <> nil) then
      Result := SQLDA.FXSQLDA else
      Result := nil;
  end;

  //****************************************
  // API CALLS
  //****************************************

  procedure TALFBXLibrary.DSQLExecuteImmediate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
    const Statement: {RawByteString}AnsiString; Dialect: Word; Sqlda: TALFBXSQLDA = nil);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_dsql_execute_immediate(@aStatusVector, @DBHandle, @TraHandle,
        length(Statement), Pointer(Statement), Dialect, ALFBXGetSQLDAData(Sqlda)), aStatusVector);

  end;

  procedure TALFBXLibrary.DSQLExecuteImmediate(const Statement: {RawByteString}AnsiString; Dialect: Word; Sqlda: TALFBXSQLDA = nil);
  var p: pointer;
      aStatusVector: TALFBXStatusVector;
  begin

      p := nil;
      CheckFBXApiCall(isc_dsql_execute_immediate(@aStatusVector, @p, @p,
        length(Statement), Pointer(Statement), Dialect, ALFBXGetSQLDAData(Sqlda)), aStatusVector);

  end;

  procedure TALFBXLibrary.DSQLAllocateStatement(var DBHandle: IscDbHandle; var StmtHandle: IscStmtHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_dsql_allocate_statement(@aStatusVector, @DBHandle, @StmtHandle), aStatusVector);

  end;

  function TALFBXLibrary.DSQLPrepare(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
    Statement: {RawByteString}AnsiString; Dialect: Word; Sqlda: TALFBXSQLResult = nil): TALFBXStatementType;
  var
    STInfo: packed record
      InfoCode: byte;
      InfoLen : Word; // isc_portable_integer convert a SmallInt to Word ??? so just say it is a word
      InfoType: TALFBXStatementType;
      Filler: byte;
    end;
    InfoIn: byte;
    aStatusVector: TALFBXStatusVector;
  begin

	  CheckFBXApiCall(isc_dsql_prepare(@aStatusVector, @TraHandle, @StmtHandle, Length(Statement),
        PAnsiChar(Statement), Dialect, ALFBXGetSQLDAData(Sqlda)), aStatusVector);
      InfoIn := isc_info_sql_stmt_type;
      isc_dsql_sql_info(@aStatusVector, @StmtHandle, 1, @InfoIn, SizeOf(STInfo), @STInfo);
      dec(STInfo.InfoType);
      Result := STInfo.InfoType;


    if (Sqlda <> nil) then
    begin
      Sqlda.ClearRecords;
      if Sqlda.FXSQLDA.sqld > 0 then
      begin
        Sqlda.SetAllocatedFields(Sqlda.FXSQLDA.sqld);
        DSQLDescribe(DbHandle, TraHandle, StmtHandle, Dialect, Sqlda);
      end;
    end;
  end;

  procedure TALFBXLibrary.DSQLExecute(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
    Dialect: Word; Sqlda: TALFBXSQLDA = nil);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_dsql_execute(@aStatusVector, @TraHandle, @StmtHandle,
        Dialect, ALFBXGetSQLDAData(Sqlda)), aStatusVector);

  end;

  procedure TALFBXLibrary.DSQLExecute2(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word;
    InSqlda: TALFBXSQLDA; OutSQLDA: TALFBXSQLResult);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_dsql_execute2(@aStatusVector, @TraHandle, @StmtHandle, Dialect,
        ALFBXGetSQLDAData(InSqlda), ALFBXGetSQLDAData(OutSQLDA)), aStatusVector);

  end;

  procedure TALFBXLibrary.DSQLFreeStatement(var StmtHandle: IscStmtHandle; Option: Word);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_dsql_free_statement(@aStatusVector, @StmtHandle, Option), aStatusVector);
      // if connection lost StmtHandle must be set manually to nil.
      if option = DSQL_DROP then
         StmtHandle := nil;

  end;

  function TALFBXLibrary.DSQLFetch(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
    var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLResult): boolean;
  var
    Status: ISCStatus;
    i, j: integer;
    destArray: pointer;
    SliceLen: integer;
    aStatusVector: TALFBXStatusVector;
  begin
    Result := True;
    if (Sqlda <> nil) then
      Sqlda.FScrollEOF := False;

      Status := isc_dsql_fetch(@aStatusVector, @StmtHandle, Dialect, ALFBXGetSQLDAData(Sqlda));


    case Status of
      0   : if (Sqlda <> nil) then
            begin
              // get array data
              for i := 0 to length(sqlda.FArrayInfos) - 1 do
              begin
                j := sqlda.FArrayInfos[i].index;
                if not Sqlda.IsNull[j] then
                begin
                  destArray := sqlda.FXSQLDA.sqlvar[j].SqlData;
                  inc(PtrInt(destArray), SizeOf(TISCQuad));
                  SliceLen := sqlda.FArrayInfos[i].size;
                  ArrayGetSlice(DBHandle, TransHandle, sqlda.AsQuad[j],
                    sqlda.FArrayInfos[i].info, destArray, SliceLen);
                end;
              end;
              if Sqlda.FCachedFetch then
                Sqlda.AddCurrentRecord;
            end;
      100 :
        begin
          Result := False; // end of fetch
          if (Sqlda <> nil) then
          begin
            Sqlda.FScrollEOF := True;
            Sqlda.FInMemoryEOF := true;
          end;
        end;
    else
      CheckFBXApiCall(Status, aStatusVector);
    end;
  end;

  function  TALFBXLibrary.DSQLFetchWithBlobs(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle;
    var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLResult): boolean;
  var
    Status: ISCStatus;
    BlobHandle: IscBlobHandle;
    i, j: Integer;
    destArray: Pointer;
    SliceLen: integer;
    BlobData: PALFBXBlobData;
    aStatusVector: TALFBXStatusVector;
  begin
    Result := True;
    if (Sqlda <> nil) then
      sqlda.FScrollEOF := False;

      Status := isc_dsql_fetch(@aStatusVector, @StmtHandle, Dialect, ALFBXGetSQLDAData(Sqlda));


    case Status of
      0   :
            begin
              if (Sqlda <> nil) then
              begin
                // read array data
                for i := 0 to length(sqlda.FArrayInfos) - 1 do
                begin
                  j := sqlda.FArrayInfos[i].index;
                  if not Sqlda.IsNull[j] then
                  begin
                    destArray := sqlda.FXSQLDA.sqlvar[j].SqlData;
                    inc(PtrInt(destArray), SizeOf(TISCQuad));
                    SliceLen := sqlda.FArrayInfos[i].size;
                    ArrayGetSlice(DbHandle, TraHandle, sqlda.AsQuad[j],
                      sqlda.FArrayInfos[i].info, destArray, SliceLen);
                  end;
                end;

                // read blobs
                for i := 0 to Length(Sqlda.FBlobsIndex) - 1 do
                begin
                  BlobData := sqlda.GetDataQuadOffset(Sqlda.FBlobsIndex[i]);
                  if (not Sqlda.FCachedFetch) and        // not stored
                    (BlobData.Size > 0)  then // not null (null if the first one)
                      FreeMem(BlobData.Buffer);

                  if Sqlda.IsNull[Sqlda.FBlobsIndex[i]] then
                  begin
                    BlobData.Size := 0;
                    BlobData.Buffer := nil;
                  end else
                  begin
                    BlobHandle := nil;
                    BlobOpen(DbHandle, TraHandle, BlobHandle, Sqlda.AsQuad[Sqlda.FBlobsIndex[i]]);
                    try
                      BlobReadBuffer(BlobHandle, BlobData.Size, BlobData.Buffer); // memory allocated here !!
                      inc(Sqlda.FStatBlobsSize, BlobData.Size);
                    finally
                      BlobClose(BlobHandle);
                    end;
                  end;
                end;
                // add to list after the blobs are fetched
                if Sqlda.FCachedFetch then Sqlda.AddCurrentRecord;
              end;
            end;
      100 :
        begin
          Result := False; // end of fetch
          if (Sqlda <> nil) then
          begin
            Sqlda.FScrollEOF := True;
            Sqlda.FInMemoryEOF := true;
          end;
        end;
    else
      CheckFBXApiCall(Status, aStatusVector);
    end;
  end;

  procedure TALFBXLibrary.DSQLDescribe(var DbHandle: IscDbHandle; var TrHandle: IscTrHandle;
    var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLResult);
  var
    i: integer;
    ArrayCount: integer;
    aStatusVector: TALFBXStatusVector;
  begin

	  CheckFBXApiCall(isc_dsql_describe(@aStatusVector, @StmtHandle, Dialect, ALFBXGetSQLDAData(Sqlda)), aStatusVector);

    if (Sqlda <> nil) then
    begin
      ArrayCount := 0;
      SetLength(sqlda.FArrayInfos, ArrayCount);
      for i := 0 to sqlda.FieldCount - 1 do
       if sqlda.FieldType[i] = uftArray then
         inc(ArrayCount);
      if ArrayCount > 0 then
      begin
        SetLength(sqlda.FArrayInfos, ArrayCount);
        for i := sqlda.FieldCount - 1 downto 0  do
          if sqlda.FieldType[i] = uftArray then
          begin
            dec(ArrayCount);
            Sqlda.FArrayInfos[ArrayCount].info :=
              ArrayLookupBounds(DbHandle, TrHandle, Sqlda.RelName[i], Sqlda.SqlName[i]);
            Sqlda.FArrayInfos[ArrayCount].index := i;
          end;
      end;
      Sqlda.AllocateDataBuffer;
    end;
  end;

  procedure TALFBXLibrary.DSQLDescribeBind(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLParams);
  var
    i, len: Integer;
    da: PALFBXSQLDaData;
    src, dst: PALFBXSQLVar;
    aStatusVector: TALFBXStatusVector;
  begin
    if Sqlda = nil then Exit;

      len := XSQLDA_LENGTH(sqlda.Data.sqln);
      GetMem(da, len);
      try
        FillChar(da^, len, 0);
        da.version := Sqlda.Data.version;
        da.sqln := sqlda.Data.sqln;
        da.sqld := sqlda.Data.sqld;
        CheckFBXApiCall(isc_dsql_describe_bind(@aStatusVector, @StmtHandle, Dialect, PXSQLDA(da)), aStatusVector);
        for i := 0 to Sqlda.FieldCount - 1 do
          with Sqlda.Data^.sqlvar[i] do
          begin
            src := @Sqlda.Data^.sqlvar[i];
            dst := @da^.sqlvar[i];
            if dst^.SqlType and 1 = 0 then
              Include(src^.Flags, pfNotNullable);
            src^.SqlType := dst^.sqltype or 1;
            src^.SqlLen  := dst^.sqllen;
            src^.MaxSqlLen := dst^.sqllen;
            src^.SqlSubType := dst^.sqlsubtype;
            src^.SqlScale := dst^.SqlScale;
            src^.SqlInd^ := -1;
          end;
         Sqlda.AllocateDataBuffer(false);
      finally
        FreeMem(da);
      end;

  end;

  procedure  TALFBXLibrary.DSQLSetCursorName(var StmtHandle: IscStmtHandle; const cursor: AnsiString);
  Var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_dsql_set_cursor_name(@aStatusVector, @StmtHandle, PAnsiChar(cursor), 0), aStatusVector);

  end;

  procedure TALFBXLibrary.DSQLExecImmed2(var DBHhandle: IscDbHandle; var TraHandle: IscTrHandle;
    const Statement: {RawByteString}AnsiString; dialect: Word; InSqlda, OutSQLDA: TALFBXSQLDA);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_dsql_exec_immed2(@aStatusVector, @DBHhandle, @TraHandle, Length(Statement),
        PAnsiChar(Statement), dialect, ALFBXGetSQLDAData(InSqlda), ALFBXGetSQLDAData(OutSQLDA)), aStatusVector);

  end;

  procedure TALFBXLibrary.DSQLInfo(var StmtHandle: IscStmtHandle; const Items: array of byte; var buffer: AnsiString);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_dsql_sql_info(@aStatusVector, @StmtHandle, Length(Items), @Items[0],
        Length(buffer), PAnsiChar(buffer)), aStatusVector);

  end;

  function TALFBXLibrary.DSQLInfoPlan(var StmtHandle: IscStmtHandle): AnsiString;
  var
    STInfo : packed record
      InfoCode: byte;
      InfoLen : Word;
      PlanDesc: array[0..1024] of AnsiChar;
    end;
    InfoType: Byte;
    aStatusVector: TALFBXStatusVector;
  begin
    InfoType := isc_info_sql_get_plan;

      CheckFBXApiCall(isc_dsql_sql_info(@aStatusVector, @StmtHandle, 1, @InfoType,
        SizeOf(STInfo), @STInfo), aStatusVector);

    SetString(Result, PAnsiChar(@STInfo.PlanDesc[1]), STInfo.InfoLen - 1);
  end;

  function TALFBXLibrary.DSQLInfoStatementType(var StmtHandle: IscStmtHandle): TALFBXStatementType;
  var
    STInfo: packed record
      InfoCode: byte;
      InfoLen : Word;
      InfoType: TALFBXStatementType;
      Filler: byte;
    end;
    InfoIn: byte;
    aStatusVector: TALFBXStatusVector;
  begin
    InfoIn := isc_info_sql_stmt_type;

      CheckFBXApiCall(isc_dsql_sql_info(@aStatusVector, @StmtHandle, 1,
        @InfoIn, SizeOf(STInfo), @STInfo), aStatusVector);

    dec(STInfo.InfoType);
    Result := STInfo.InfoType;
  end;

  procedure TALFBXLibrary.DSQLInfoRowsAffected2(var StmtHandle: IscStmtHandle;
    out SelectedRows, InsertedRows, UpdatedRows, DeletedRows: Cardinal);
  var
    InfoData : packed record
      InfoCode: byte;
      InfoLen : Word;
      Infos: packed array[0..3] of record
        InfoCode: byte;
        InfoLen : Word;
        Rows: Cardinal;
      end;
      Filler: Word;
    end;
    Command: byte;
    aStatusVector: TALFBXStatusVector;
  begin
    Command := isc_info_sql_records;
    CheckFBXApiCall(isc_dsql_sql_info(@aStatusVector, @StmtHandle, 1, @Command,
      SizeOf(InfoData), @InfoData), aStatusVector);
    for command := 0 to 3 do
      with InfoData.Infos[command] do
      case InfoCode of
        isc_info_req_select_count: SelectedRows := Rows;
        isc_info_req_insert_count: InsertedRows := Rows;
        isc_info_req_update_count: UpdatedRows := Rows;
        isc_info_req_delete_count: DeletedRows := Rows;
      end;
  end;

  function TALFBXLibrary.DSQLInfoRowsAffected(var StmtHandle: IscStmtHandle;
    StatementType: TALFBXStatementType): Cardinal;
  var
    SelectedRows, InsertedRows, UpdatedRows, DeletedRows: Cardinal;
  begin
    if not (StatementType in [stUpdate, stDelete, stInsert, stExecProcedure]) then
      Result := 0 else
    begin
      DSQLInfoRowsAffected2(StmtHandle, SelectedRows, InsertedRows, UpdatedRows, DeletedRows);
      case StatementType of
        stUpdate: Result := UpdatedRows;
        stDelete: Result := DeletedRows;
        stInsert: Result := InsertedRows;
        stExecProcedure: Result := InsertedRows + UpdatedRows + DeletedRows;
      else
        Result := 0;
      end;
    end;
  end;

  procedure TALFBXLibrary.DDLExecute(var DBHandle: IscDbHandle;
    var TraHandle: IscTrHandle; const ddl: AnsiString);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_ddl(@aStatusVector, @DBHandle, @TraHandle,
        length(ddl), Pointer(ddl)), aStatusVector);

  end;

//******************************************************************************
//  Array
//******************************************************************************
  function TALFBXLibrary.ArrayLookupBounds(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
    const RelationName, FieldName: AnsiString): TALFBXArrayDesc;
  var aStatusVector: TALFBXStatusVector;
  begin

        CheckFBXApiCall(isc_array_lookup_bounds(@aStatusVector, @DBHandle, @TransHandle,
          PAnsiChar(RelationName), PAnsiChar(FieldName), @Result), aStatusVector);

  end;

  procedure TALFBXLibrary.ArrayGetSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle; ArrayId: TISCQuad;
    var desc: TALFBXArrayDesc; DestArray: PPointer; var SliceLength: Integer);
  var aStatusVector: TALFBXStatusVector;
  begin

        CheckFBXApiCall(isc_array_get_slice(@aStatusVector, @DBHandle, @TransHandle, @ArrayId,
          @desc, DestArray, @SliceLength), aStatusVector);

  end;

  procedure TALFBXLibrary.ArrayPutSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
    var ArrayId: TISCQuad; var desc: TALFBXArrayDesc; DestArray: Pointer; var SliceLength: Integer);
  var aStatusVector: TALFBXStatusVector;
  begin

        CheckFBXApiCall(isc_array_put_slice(@aStatusVector, @DBHandle, @TransHandle, @ArrayId,
          @desc, DestArray, @SliceLength), aStatusVector);

  end;

  procedure TALFBXLibrary.ArraySetDesc(const RelationName, FieldName: AnsiString; var SqlDtype,
    SqlLength, Dimensions: Smallint; var desc: TISCArrayDesc);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_array_set_desc(@aStatusVector, PAnsiChar(RelationName),
        PAnsiChar(FieldName), @SqlDtype, @SqlLength, @Dimensions, @desc), aStatusVector);

  end;


//******************************************************************************
//  Error-handling
//******************************************************************************

  function  TALFBXLibrary.ErrSqlcode(StatusVector: TALFBXStatusVector): ISCLong;
  begin

      Result := isc_sqlcode(@StatusVector);

  end;

  function TALFBXLibrary.ErrInterprete(StatusVector: TALFBXStatusVector): {RawByteString}AnsiString;
  var
    pStatusVector: PALFBXStatusVector;
    len: Integer;
    buffer: array[0..512] of AnsiChar;
  begin
    Result := '';
    pStatusVector := @StatusVector;

      repeat
        if Version_api_IS_FB20_UP then
          len := fb_interpret(buffer, sizeof(buffer), @pStatusVector)
        else
          len := isc_interprete(buffer, @pStatusVector);
        if len > 0 then
          Result := Result + ALCopyStr(buffer, 0, len) + cALFBXNewLine else
          Break;
      until False;

  end;

  function TALFBXLibrary.ErrSQLInterprete(SQLCODE: Smallint): {RawByteString}AnsiString;
  var
    i : Integer;
  begin
    SetLength(Result, 255);

      isc_sql_interprete(SQLCODE, PAnsiChar(Result), 255);

    for i := 1 to 255 do if Result[i] = #0 then Break; // Quick trim
    SetLength(Result, i-1);
  end;

{FB25_UP}
  function TALFBXLibrary.ErrSqlState(StatusVector: TALFBXStatusVector): FB_SQLSTATE_STRING;
  begin
    if Version_Api_Is_FB25_Up then fb_sqlstate(@Result, @StatusVector)
    else result := 'ZZZZZZ'; // 'ZZZZZZ' for FB < 2.5
  end;
{FB25_UP}

//******************************************************************************
// Services
//******************************************************************************

  procedure TALFBXLibrary.ServiceAttach(const ServiceName: {RawByteString}AnsiString; var SvcHandle: IscSvcHandle; const Spb: {RawByteString}AnsiString);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_service_attach(@aStatusVector, Length(ServiceName),
        PAnsiChar(ServiceName), @SvcHandle, Length(Spb), PAnsiChar(Spb)), aStatusVector);

  end;

  procedure TALFBXLibrary.ServiceDetach(var SvcHandle: IscSvcHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_service_detach(@aStatusVector, @SvcHandle), aStatusVector);

  end;

  procedure TALFBXLibrary.ServiceQuery(var SvcHandle: IscSvcHandle; const SendSpb, RequestSpb: {RawByteString}AnsiString; var Buffer: {RawByteString}AnsiString);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_service_query(@aStatusVector, @SvcHandle, nil,
        Length(SendSpb), PAnsiChar(SendSpb), Length(RequestSpb), PAnsiChar(RequestSpb),
        Length(Buffer), PAnsiChar(Buffer)), aStatusVector);

  end;

  procedure TALFBXLibrary.ServiceStart(var SvcHandle: IscSvcHandle; const Spb: {RawByteString}AnsiString);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_service_start(@aStatusVector, @SvcHandle, nil, Length(Spb), PAnsiChar(Spb)), aStatusVector);

  end;

//******************************************************************************
//  Blob
//******************************************************************************

  function ALFBXCreateBlobParams(Params: AnsiString; Delimiter: AnsiChar = ';'): AnsiString;
  var
    BufferSize: Integer;
    CurPos, NextPos: PAnsiChar;
    CurStr, CurValue: AnsiString;
    EqualPos: Integer;
    Code: Byte;
    AValue: Integer;
    FinalSize: Integer;
    function Min(v1, v2: Integer): Integer;
    begin
      if v1 > v2 then Result := v2 else Result := v1;
    end;
    // dont reallocate memory each time, step by step ...
    procedure CheckBufferSize;
    begin
      while (FinalSize > BufferSize) do
        begin
          Inc(BufferSize, 32);
          SetLength(Result, BufferSize);
        end;
    end;
    procedure AddByte(AByte: Byte);
    begin
      inc(FinalSize);
      CheckBufferSize;
      Result[FinalSize] := AnsiChar(AByte);
    end;
    procedure AddShort(AShort: ShortInt);
    begin
      AddByte(2); //len
      inc(FinalSize,2);
      CheckBufferSize;
      PShortInt(@Result[FinalSize-1])^ := AShort;
      PShortInt(@Result[FinalSize])^ := AShort shr 8;
    end;
  begin
    if Params = '' then
    begin
      Result := '';
      Exit;
    end;

    FinalSize := 1;
    BufferSize := 32;
    Result := StringOfChar(AnsiChar(#0), BufferSize);
    Result[1] := isc_bpb_version1;
    CurPos  := PAnsiChar(Params);
    while (CurPos <> nil) do
    begin
      NextPos := {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrScan(CurPos, Delimiter);
      if (NextPos = nil) then
        CurStr := CurPos else
        begin
          CurStr := ALCopyStr(CurPos, 0, NextPos-CurPos);
          Inc(NextPos);
        end;
      CurPos := NextPos;
      if (CurStr = '') then Continue;
      begin
        CurValue := '';
        EqualPos := ALPos(AnsiChar('='), CurStr);
        if EqualPos <> 0 then
        begin
          CurValue := ALCopyStr(CurStr, EqualPos+1, Length(CurStr) - EqualPos);
          CurStr   := ALCopyStr(CurStr, 0, EqualPos-1);
        end;
        {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrLower(PAnsiChar(CurStr));
        CurStr := ALTrim(CurStr);
        CurValue := ALTrim(CurValue);
        for Code := 1 to isc_bpb_Max_Value do
          with cALFBXBPBInfos[Code] do
            if (Name = CurStr) then
            begin
              case ParamType of
                prShrt :
                  if ALTryStrToInt(CurValue, AValue) and (AValue >= -128) and (AValue <= 127) then
                  begin
                    AddByte(Code);
                    AddShort(ShortInt(AValue));
                  end;
              end;
              break;
            end;
      end;
    end;
    SetLength(Result, FinalSize);
  end;

  procedure TALFBXLibrary.BlobOpen(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
    var BlobHandle: IscBlobHandle; BlobId: TISCQuad; BPB: AnsiString = '');
  var aStatusVector: TALFBXStatusVector;
  begin
    BPB := ALFBXCreateBlobParams(BPB,';');

      CheckFBXApiCall(isc_open_blob2(@aStatusVector, @DBHandle, @TraHandle, @BlobHandle,
        @BlobId, Length(BPB), PAnsiChar(BPB)), aStatusVector);

  end;

  function TALFBXLibrary.BlobGetSegment(var BlobHandle: IscBlobHandle; out length: Word;
    BufferLength: Cardinal; Buffer: Pointer): boolean;
  var
    AStatus: ISCStatus;
    aStatusVector: TALFBXStatusVector;
  begin
    if BufferLength > High(Word) then
      BufferLength := High(Word);

      AStatus := isc_get_segment(@aStatusVector, @BlobHandle, @length, Word(BufferLength), Buffer);

    Result := (AStatus = 0) or (aStatusVector[1] = isc_segment);
    if not Result then
      if (aStatusVector[1] <> isc_segstr_eof) then
        CheckFBXApiCall(AStatus, aStatusVector);
  end;

  procedure TALFBXLibrary.BlobClose(var BlobHandle: IscBlobHandle);
  var aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_close_blob(@aStatusVector, @BlobHandle), aStatusVector);

  end;

type
  TBlobInfo = packed record
    Info: AnsiChar;
    Length: Word;
    case byte of
      0: (CardType: Integer);
      1: (ByteType: Byte);
  end;

  procedure TALFBXLibrary.BlobSize(var BlobHandle: IscBlobHandle; out Size: Cardinal);
  var
    BlobInfo : packed record
      Code: AnsiChar;
      Length: Word;
      Value: Cardinal;
      reserved: Word; // alignement (8)
    end;
    aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 1,
        isc_info_blob_total_length, SizeOf(BlobInfo), @BlobInfo), aStatusVector);
      Size := BlobInfo.Value;

  end;

  procedure TALFBXLibrary.BlobMaxSegment(var BlobHandle: IscBlobHandle; out Size: Cardinal);
  var BlobInfo: array[0..1] of TBlobInfo;
      aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 1,
        isc_info_blob_max_segment, SizeOf(BlobInfo), @BlobInfo), aStatusVector);
      Size := BlobInfo[0].CardType;

  end;

  procedure TALFBXLibrary.BlobInfo(var BlobHandle: IscBlobHandle; out NumSegments, MaxSegment,
    TotalLength: Cardinal; out btype : byte);
  var BlobInfos: array[0..3] of TBlobInfo;
      aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 4,
        isc_info_blob_num_segments + isc_info_blob_max_segment +
        isc_info_blob_total_length + isc_info_blob_type, SizeOf(BlobInfos), @BlobInfos), aStatusVector);

    NumSegments := BlobInfos[0].CardType;
    MaxSegment  := BlobInfos[1].CardType;
    TotalLength := BlobInfos[2].CardType;
    btype       := BlobInfos[3].ByteType;
  end;

  procedure TALFBXLibrary.BlobDefaultDesc(var Desc: TALFBXBlobDesc; const RelationName, FieldName: AnsiString);
  begin

        isc_blob_default_desc(@Desc, PAnsiChar(RelationName), PAnsiChar(FieldName));

  end;

  procedure TALFBXLibrary.BlobSaveToStream(var BlobHandle: IscBlobHandle; Stream: TStream);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    Buffer: Pointer;
    CurrentLength: Word;
    aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 2,
        isc_info_blob_max_segment + isc_info_blob_total_length,
        SizeOf(BlobInfos), @BlobInfos), aStatusVector);


    Stream.Seek(0, soFromBeginning);
    Getmem(Buffer, BlobInfos[0].CardType);
    try
      while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[0].CardType, Buffer) do
        Stream.Write(Buffer^, CurrentLength);
    finally
      FreeMem(Buffer);
    end;
    Stream.Seek(0, soFromBeginning);
  end;

  function TALFBXLibrary.BlobReadString(var BlobHandle: IscBlobHandle): {RawByteString}AnsiString;
  begin
    BlobReadString(BlobHandle, Result);
  end;

  procedure TALFBXLibrary.BlobReadString(var BlobHandle: IscBlobHandle; var Str: {RawByteString}AnsiString);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    Buffer: Pointer;
    Len: Integer;
    aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 2,
        isc_info_blob_max_segment + isc_info_blob_total_length,
        SizeOf(BlobInfos), @BlobInfos), aStatusVector);

    SetLength(Str, BlobInfos[1].CardType);
    Buffer := PAnsiChar(Str);
    len := 0;
    while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - len, Buffer) do
    begin
      inc(PtrInt(Buffer), CurrentLength);
      inc(len, CurrentLength);
      if len = BlobInfos[1].CardType then
        Break;
    end;
  end;

  procedure TALFBXLibrary.BlobReadBuffer(var BlobHandle: IscBlobHandle; var Size: Integer;
    var Buffer: Pointer; realloc: boolean);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    TMP: Pointer;
    Len: Integer;
    aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 2,
        isc_info_blob_max_segment + isc_info_blob_total_length,
        SizeOf(BlobInfos), @BlobInfos), aStatusVector);

    if realloc and (Buffer <> nil) then
    begin
      if Size <> BlobInfos[1].CardType then
      begin
        Size := BlobInfos[1].CardType;
        ReallocMem(Buffer, Size);
      end;
    end else
    begin
      Size := BlobInfos[1].CardType;
      GetMem(Buffer, Size);
    end;
    TMP := Buffer;
    Len := 0;
    while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - len, TMP) do
    begin
      inc(PtrInt(TMP), CurrentLength);
      inc(Len, CurrentLength);
      if len = Size then
        break;
    end;
  end;

  procedure TALFBXLibrary.BlobReadSizedBuffer(var BlobHandle: IscBlobHandle;
    Buffer: Pointer);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    TMP: Pointer;
    Len: Integer;
    aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 2,
        isc_info_blob_max_segment + isc_info_blob_total_length,
        SizeOf(BlobInfos), @BlobInfos), aStatusVector);

    TMP := Buffer;
    Len := 0;
    while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - len, TMP) do
    begin
      inc(PtrInt(TMP), CurrentLength);
      inc(Len, CurrentLength);
      if len = BlobInfos[1].CardType then
        break;
    end;
  end;

  procedure TALFBXLibrary.BlobReadSizedBuffer(var BlobHandle: IscBlobHandle;
    Buffer: Pointer; MaxSize: Integer);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    TMP: Pointer;
    Len: Integer;
    aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 2,
        isc_info_blob_max_segment + isc_info_blob_total_length,
        SizeOf(BlobInfos), @BlobInfos), aStatusVector);

    if MaxSize > BlobInfos[1].CardType then
      MaxSize := BlobInfos[1].CardType;

    TMP := Buffer;
    Len := 0;
    while BlobGetSegment(BlobHandle, CurrentLength, MaxSize - len, TMP) do
    begin
      inc(PtrInt(TMP), CurrentLength);
      inc(Len, CurrentLength);
      if len = MaxSize then
        break;
    end;
  end;

  procedure TALFBXLibrary.BlobReadVariant(var BlobHandle: IscBlobHandle; var Value: Variant);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    Len: Integer;
    Buffer: Pointer;
    aStatusVector: TALFBXStatusVector;
  begin

      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 2,
        isc_info_blob_max_segment + isc_info_blob_total_length,
        SizeOf(BlobInfos), @BlobInfos), aStatusVector);


    Value := VarArrayCreate([0, BlobInfos[1].CardType - 1], varByte);
    Len := 0;
    Buffer := VarArrayLock(Value);
    try
      while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - len, Buffer) do
      begin
        inc(PtrInt(Buffer), CurrentLength);
        inc(Len, CurrentLength);
        if Len = BlobInfos[1].CardType then
          Break;
      end;
    finally
      VarArrayUnlock(Value);
    end;
  end;

  function TALFBXLibrary.BlobCreate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
    var BlobHandle: IscBlobHandle; BPB: AnsiString = ''): TISCQuad;
  var aStatusVector: TALFBXStatusVector;
  begin
    BPB := ALFBXCreateBlobParams(BPB,';');

      CheckFBXApiCall(isc_create_blob2(@aStatusVector, @DBHandle, @TraHandle, @BlobHandle, @Result, Length(BPB), PAnsiChar(BPB)), aStatusVector);

  end;

  procedure TALFBXLibrary.BlobWriteSegment(var BlobHandle: IscBlobHandle; BufferLength: Cardinal; Buffer: Pointer);
  var size: Word;
      aStatusVector: TALFBXStatusVector;
  begin

      while BufferLength > 0 do
      begin
        if BufferLength > FSegmentSize then
          size := FSegmentSize else
          size := Word(BufferLength);
        CheckFBXApiCall(isc_put_segment(@aStatusVector, @BlobHandle, Size, Buffer), aStatusVector);
        dec(BufferLength, size);
        inc(PByte(Buffer), size);
      end;

  end;

  procedure TALFBXLibrary.BlobWriteString(var BlobHandle: IscBlobHandle; const Str: {RawByteString}AnsiString);
  begin
    BlobWriteSegment(BlobHandle, Length(Str), PAnsiChar(Str));
  end;

  procedure TALFBXLibrary.BlobWriteStream(var BlobHandle: IscBlobHandle; Stream: TStream);
  var
    Buffer: Pointer;
  begin
    Stream.Seek(0, soFromBeginning);
    if Stream is TCustomMemoryStream then
      BlobWriteSegment(BlobHandle, Cardinal(TCustomMemoryStream(Stream).Size),
        TCustomMemoryStream(Stream).Memory) else

    begin
      GetMem(Buffer, Cardinal(Stream.Size));
      try
        Stream.Read(Buffer^, Cardinal(Stream.Size));
        BlobWriteSegment(BlobHandle, Cardinal(Stream.Size), Buffer);
        Stream.Seek(0, soFromBeginning);
      finally
        FreeMem(buffer);
      end;
    end;
  end;

  function TALFBXLibrary.StreamBlobOpen(var BlobId: TISCQuad;
    var Database: IscDbHandle; var Transaction: IscTrHandle;
    Mode: AnsiChar): PBStream;
  begin

      Result := Bopen(@BlobId, @Database, @Transaction, @Mode);

  end;

  function TALFBXLibrary.StreamBlobClose(Stream: PBStream): integer;
  begin

      Result := BLOB_close(Stream);

  end;

//******************************************************************************
//  Events
//******************************************************************************

  procedure TALFBXLibrary.EventCancel(var DbHandle: IscDbHandle; var id: Integer);
  var aStatusVector: TALFBXStatusVector;
  begin

     CheckFBXApiCall(isc_cancel_events(@aStatusVector, @DbHandle, @id), aStatusVector);

  end;

  function TALFBXLibrary.EventBlock(var EventBuffer, ResultBuffer: PAnsiChar; Count: Smallint;
    v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: PAnsiChar): Integer;
  begin

      result := isc_event_block(@EventBuffer, @ResultBuffer, Count, v1, v2,
        v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);

  end;

  procedure TALFBXLibrary.EventQueue(var handle: IscDbHandle; var id: Integer; length: Word;
      events: PAnsiChar; ast: ISC_EVENT_CALLBACK; arg: Pointer);
  var aStatusVector: TALFBXStatusVector;
  begin

     CheckFBXApiCall(isc_que_events(@aStatusVector, @handle, @id, length,
       events, ast, arg), aStatusVector);

  end;

  procedure TALFBXLibrary.EventCounts(var ResultVector: TALFBXStatusVector;
    BufferLength: Smallint; EventBuffer, ResultBuffer: PAnsiChar);
  begin

      isc_event_counts(@ResultVector, BufferLength, EventBuffer, ResultBuffer);

  end;

  procedure TALFBXLibrary.EventWaitFor(var handle: IscDbHandle; length: Smallint;
    events, buffer: Pointer);
  var aStatusVector: TALFBXStatusVector;
  begin

     CheckFBXApiCall(isc_wait_for_event(@aStatusVector, @handle, length, events, buffer), aStatusVector);

  end;


  function TALFBXLibrary.IscFree(data: Pointer): Integer;
  begin

     result := isc_free(data);

  end;

//******************************************************************************
//  Save Points
//******************************************************************************

  function TALFBXLibrary.GetSegmentSize: Word;
  begin

      Result := FSegMentSize;

  end;

  procedure TALFBXLibrary.SetSegmentSize(Value: Word);
  begin

      system.Assert(Value > 0);
      FSegmentSize := Value;

  end;


//******************************************************************************
// Conversion
// Making a delphi conversion will help to transport data buffer and use it
// without GDS32 ;)
//******************************************************************************

  procedure ALFBXDecodeTimeStamp(v: PISCTimeStamp; out DateTime: Double);
  begin
    DateTime := v.timestamp_date - cAlFBXDateOffset + (v.timestamp_time / cALFBXTimeCoeff);
  end;

  procedure ALFBXDecodeTimeStamp(v: PISCTimeStamp; out TimeStamp: TTimeStamp);
  begin
    TimeStamp.Date := v.timestamp_date - cAlFBXDateOffset + 693594;
    TimeStamp.Time := v.timestamp_time div 10;
  end;

  function  ALFBXDecodeTimeStamp(v: PISCTimeStamp): Double;
  begin
    ALFBXDecodeTimeStamp(v, Result);
  end;

  procedure ALFBXEncodeTimeStamp(const DateTime: TDateTime; v: PISCTimeStamp);
  begin
    v.timestamp_date := Round(int(DateTime)) + cAlFBXDateOffset;
    v.timestamp_time := ISC_TIME(Round(Abs(Frac(DateTime) * cALFBXTimeCoeff)));
  end;

  procedure ALFBXEncodeTimeStamp(const Date: Integer; v: PISCTimeStamp);
  begin
    v.timestamp_date := Date + cAlFBXDateOffset;
    v.timestamp_time := 0;
  end;

  procedure ALFBXEncodeTimeStamp(const Time: Cardinal; v: PISCTimeStamp);
  begin
    v.timestamp_date := cAlFBXDateOffset;
    v.timestamp_time := Time;
  end;

  procedure ALFBXDecodeSQLDate(v: Integer; out Year: SmallInt; out Month, Day: Word);
  var c: Word;
  begin
    inc(v, 678882);
    c     := (4 * v - 1) div 146097; // century
    v     := 4 * v - 1  - 146097 * c;
    day   := v div 4;
    v     := (4 * day + 3) div 1461;
    day   := 4 * day + 3 - 1461 * v;
    day   := (day + 4) div 4;
    month := (5 * day - 3) div 153;
    day   := 5 * day - 3 - 153 * month;
    day   := (day + 5) div 5;
    year  := 100 * c + v;
    if (month < 10) then inc(month, 3) else
      begin
        dec(month, 9);
        inc(year, 1);
      end;
  end;

  function ALFBXEncodeSQLDate(Year: Integer; Month, Day: Integer): Integer;
  var
    c, ya: integer;
  begin
    inc(month);
    inc(year, 1900);

    if (month > 2) then
      dec(month, 3) else
    begin
      inc(month, 9);
      dec(year, 1);
    end;

    c := year div 100;
    ya := year - 100 * c;

    Result := ((int64(146097) * c) div 4 +
              (1461 * ya) div 4 +
              (153 * month + 2) div 5 + day + 1721119 - 2400001);
  end;

  procedure ALFBXDecodeSQLTime(v: Cardinal; out Hour, Minute, Second: Word;
    out Fractions: LongWord);
  begin
    Hour      := v div 36000000;
    v         := v mod 36000000;
    if (v > 0) then
    begin
      Minute    := v div 600000;
      v         := v mod 600000;
      if (v > 0) then
      begin
        Second    := v div 10000;
        v         := v mod 10000;
        if (v > 0) then
          Fractions := v div 10 else
          Fractions := 0;
      end else
        begin
          Second    := 0;
          Fractions := 0;
        end;
    end else
      begin
        Minute    := 0;
        Second    := 0;
        Fractions := 0;
      end;
  end;

  function ALFBXEncodeSQLTime(Hour, Minute, Second: Word;
    var Fractions: LongWord): Cardinal;
  begin
    Result := Cardinal((Hour * 60 + Minute) * 60 + Second) * ISC_TIME_SECONDS_PRECISION + Fractions;
  end;

 { TALFBXSQLDA }

  function TALFBXSQLDA.GetAllocatedFields: Word;
  begin
    Result := FXSQLDA.sqln;
  end;

  procedure TALFBXSQLDA.SetAllocatedFields(Fields: Word);
  begin
    if Fields <= 0 then Fields := 1;
    ReallocMem(FXSQLDA, XSQLDA_LENGTH(Fields));
    FXSQLDA.sqln := Fields;
    FXSQLDA.sqld := Fields;
    FXSQLDA.version := SQLDA_CURRENT_VERSION;
  end;

  function TALFBXSQLDA.GetSqlName(const Index: Word): AnsiString;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].SqlName,
      FXSQLDA.sqlvar[Index].SqlNameLength);
  end;

  function TALFBXSQLDA.GetAliasName(const Index: Word): AnsiString;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].AliasName,
      FXSQLDA.sqlvar[Index].AliasNameLength);
  end;

  function TALFBXSQLDA.GetOwnName(const Index: Word): AnsiString;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].OwnName,
      FXSQLDA.sqlvar[Index].OwnNameLength);
  end;

  function TALFBXSQLDA.GetRelName(const Index: Word): AnsiString;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].RelName,
      FXSQLDA.sqlvar[Index].RelNameLength);
  end;

  function TALFBXSQLDA.GetIsNull(const Index: Word): boolean;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      Result := (sqlind <> nil) and (sqlind^ = -1)
  end;

  procedure TALFBXSQLDA.CheckRange(const Index: Word);
  begin
    if Index >= Word(FXSQLDA.sqln) then
      raise Exception.CreateFmt(cALFBX_FIELDNUMNOTFOUND, [index]);
  end;

//  function TALFBXSQLDA.DecodeString(const Code: Smallint; Index: Word): string;
//  begin
//{$IFDEF UNICODE}
//    DecodeStringW(Code, Index, Result);
//{$ELSE}
//    DecodeStringA(Code, Index, Result);
//{$ENDIF}
//  end;

  function TALFBXSQLDA.DecodeStringA(const Code: Smallint; Index: Word): AnsiString;
  begin
    DecodeStringA(Code, Index, Result);
  end;

//  procedure TALFBXSQLDA.DecodeString(const Code: Smallint; Index: Word; out Str: string);
//  begin
//{$IFDEF UNICODE}
//    DecodeStringW(Code, Index, Str);
//{$ELSE}
//    DecodeStringA(Code, Index, Str);
//{$ENDIF}
//  end;

  procedure TALFBXSQLDA.DecodeStringA(const Code: Smallint; Index: Word; out Str: AnsiString);
  begin
//{$IFDEF UNICODE}
//    Str := AnsiString(DecodeString(Code, Index));
//{$ELSE}
    with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT    : SetString(Str, sqldata, sqllen);
      SQL_VARYING : SetString(Str, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
    end;
//{$ENDIF}
  end;

//  procedure TALFBXSQLDA.DecodeStringW(const Code: Smallint; Index: Word; out Str: UnicodeString);
//  begin
//    with FXSQLDA.sqlvar[Index] do
//    case Code of
//      SQL_TEXT    :
//        begin
//          Str := ALFBXMBUDecode(Copy(sqldata, 0, sqllen), cALFBXCharacterSetCP[FCharacterSet]);
//          if SqlSubType > 0 then
//            SetLength(Str, sqllen div cALFBXBytesPerCharacter[FCharacterSet]);
//        end;
//      SQL_VARYING :
//        Str := ALFBXMBUDecode(
//          Copy(PAnsiChar(@PVary(sqldata).vary_string), 0, PVary(sqldata).vary_length),
//          cALFBXCharacterSetCP[FCharacterSet]);
//    end;
//  end;

//  procedure TALFBXSQLDA.EncodeStringB(Code: Smallint; Index: Word; const str: RawByteString);
//  var
//    i: smallint;
//    OldLen: SmallInt;
//    NewLen: Integer;
//  begin
//    OldLen  := FXSQLDA.sqlvar[Index].SqlLen;
//    with FXSQLDA.sqlvar[Index] do
//    case Code of
//      SQL_TEXT :
//        begin
//          NewLen := Length(str);
//          if NewLen = 0 then
//          begin
//            // interbase need a valid pointer
//            if sqldata = nil then
//              getmem(sqldata, 4);
//            sqllen := 0;
//          end else
//          begin
//            if sqldata = nil then
//              getmem(sqldata, NewLen) else
//              ReallocMem(sqldata, NewLen);
//            if MaxSqlLen <= 0 then
//              sqllen := NewLen else
//              sqllen := Min(NewLen, MaxSqlLen);
//            Move(PAnsiChar(str)^, sqldata^, sqllen);
//          end;
//        end;
//      SQL_VARYING :
//        begin
//          NewLen := Length(str);
//          if NewLen = 0 then
//          begin
//            if sqldata = nil then
//            begin
//              // interbase need a valid pointer :(
//              getmem(sqldata, 4);
//              sqllen := 2;
//            end;
//            PVary(sqldata).vary_length := 0;
//          end else
//          begin
//            if sqldata = nil then
//              getmem(sqldata, NewLen+2) else
//              ReallocMem(sqldata, NewLen+2);
//            if MaxSqlLen > 0 then
//              NewLen := Min(NewLen, MaxSqlLen);
//            sqllen := NewLen + 2;
//            PVary(sqldata).vary_length := NewLen;
//            Move(PAnsiChar(str)^, PVary(sqldata).vary_string,PVary(sqldata).vary_length);
//          end;
//        end;
//    end;

//    // named parametters share the same memory !!
//    with FXSQLDA.sqlvar[Index] do
//      if (ParamNameLength > 0) and (OldLen <> SqlLen) then
//         for i := 0 to FXSQLDA.sqln - 1 do
//           if (FXSQLDA.sqlvar[i].ID = ID) then
//           begin
//             FXSQLDA.sqlvar[i].SqlData := SqlData;
//             FXSQLDA.sqlvar[i].SqlLen  := SqlLen;
//           end;
//  end;

  procedure TALFBXSQLDA.EncodeStringA(Code: Smallint; Index: Word; const str: AnsiString);
//  begin
//  {$IFDEF UNICODE}
//    EncodeStringB(Code, Index, ALFBXMBUEncode(UniCodeString(str), cALFBXCharacterSetCP[FCharacterSet]));
//  {$ELSE}
//    EncodeStringB(Code, Index, str);
//  {$ENDIF}
//  end;
  var
    i: smallint;
    OldLen: SmallInt;
    NewLen: Integer;
  begin
    OldLen  := FXSQLDA.sqlvar[Index].SqlLen;
    with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT :
        begin
          NewLen := Length(str);
          if NewLen = 0 then
          begin
            // interbase need a valid pointer
            if sqldata = nil then
              getmem(sqldata, 4);
            sqllen := 0;
          end else
          begin
            if sqldata = nil then
              getmem(sqldata, NewLen) else
              ReallocMem(sqldata, NewLen);
            if MaxSqlLen <= 0 then
              sqllen := NewLen else
              sqllen := Min(NewLen, MaxSqlLen);
            ALMove(PAnsiChar(str)^, sqldata^, sqllen);
          end;
        end;
      SQL_VARYING :
        begin
          NewLen := Length(str);
          if NewLen = 0 then
          begin
            if sqldata = nil then
            begin
              // interbase need a valid pointer :(
              getmem(sqldata, 4);
              sqllen := 2;
            end;
            PVary(sqldata).vary_length := 0;
          end else
          begin
            if sqldata = nil then
              getmem(sqldata, NewLen+2) else
              ReallocMem(sqldata, NewLen+2);
            if MaxSqlLen > 0 then
              NewLen := Min(NewLen, MaxSqlLen);
            sqllen := NewLen + 2;
            PVary(sqldata).vary_length := NewLen;
            ALMove(PAnsiChar(str)^, PVary(sqldata).vary_string,PVary(sqldata).vary_length);
          end;
        end;
    end;

    // named parametters share the same memory !!
    with FXSQLDA.sqlvar[Index] do
      if (ParamNameLength > 0) and (OldLen <> SqlLen) then
         for i := 0 to FXSQLDA.sqln - 1 do
           if (FXSQLDA.sqlvar[i].ID = ID) then
           begin
             FXSQLDA.sqlvar[i].SqlData := SqlData;
             FXSQLDA.sqlvar[i].SqlLen  := SqlLen;
           end;
  end;

//  procedure TALFBXSQLDA.EncodeStringW(Code: Smallint; Index: Word; const str: UnicodeString);
//  begin
//    EncodeStringB(Code, Index, ALFBXMBUEncode(str, cALFBXCharacterSetCP[FCharacterSet]));
//  end;

//  procedure TALFBXSQLDA.EncodeString(Code: Smallint; Index: Word; const str: string);
//  begin
//  {$IFDEF UNICODE}
//    EncodeStringW(Code, Index, str);
//  {$ELSE}
//    EncodeStringA(Code, Index, str);
//  {$ENDIF}
//  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Int64);
  begin
    //value := StrToInt64(DecodeString(Code, Index));
    value := ALStrToInt64(DecodeStringA(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Double);
  begin
    //value := StrToFloat(DecodeString(Code, Index));
    value := ALStrToFloat(DecodeStringA(Code, Index), ALDefaultFormatSettings);
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Integer);
  begin
    //value := StrToInt(DecodeString(Code, Index));
    value := ALStrToInt(DecodeStringA(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Single);
  begin
    //value := StrToFloat(DecodeString(Code, Index));
    value := ALStrToFloat(DecodeStringA(Code, Index), ALDefaultFormatSettings);
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Smallint);
  begin
    //value := StrToInt(DecodeString(Code, Index));
    value := ALStrToInt(DecodeStringA(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: TDateTime);
  begin
    //value := StrToDateTime(DecodeString(Code, Index));
    value := ALStrToDateTime(DecodeStringA(Code, Index), ALDefaultFormatSettings);
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Currency);
  begin
    //value := StrToCurr(DecodeString(Code, Index));
    value := ALStrToCurr(DecodeStringA(Code, Index), ALDefaultFormatSettings);
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: boolean);
  begin
    //value := StrToInt(DecodeString(Code, Index)) <> 0;
    value := ALStrToInt(DecodeStringA(Code, Index)) <> 0;
  end;

  procedure TALFBXSQLDA.ConvertStringToDate(const Code: Smallint; Index: Word; out value: Integer);
  begin
    //Value := Trunc(StrToDate(DecodeString(Code, Index)));
    Value := Trunc(ALStrToDate(DecodeStringA(Code, Index), ALDefaultFormatSettings));
  end;

  constructor TALFBXSQLDA.Create(aCharacterSet: TALFBXCharacterSet);
  begin
    FCharacterSet := aCharacterSet;
  end;

//  procedure TALFBXSQLDA.DecodeStringB(const Code: Smallint; Index: Word; out Str: RawByteString);
//  begin
//    with FXSQLDA.sqlvar[Index] do
//    case Code of
//      SQL_TEXT    : SetString(Str, sqldata, sqllen);
//      SQL_VARYING : SetString(Str, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
//    end;
//  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Cardinal);
  begin
    //value := StrToInt(DecodeString(Code, Index));
    value := ALStrToInt(DecodeStringA(Code, Index));
  end;

  function TALFBXSQLDA.GetFieldCount: Integer;
  begin
    Result := FXSQLDA.sqln;
  end;

  function TALFBXSQLDA.GetSQLType(const Index: Word): Smallint;
  begin
    CheckRange(Index);
    result := FXSQLDA.sqlvar[Index].sqltype and not (1);
  end;

  function TALFBXSQLDA.GetSQLLen(const Index: Word): Smallint;
  begin
    CheckRange(Index);
    result := FXSQLDA.sqlvar[Index].sqllen;
  end;

  function TALFBXSQLDA.GetSQLScale(const Index: Word): Smallint;
  begin
    CheckRange(Index);
    result := FXSQLDA.sqlvar[Index].SqlScale;
  end;

  function TALFBXSQLDA.GetIsBlob(const Index: Word): boolean;
  var
    ASQLType: Word;
  begin
    CheckRange(Index);
    ASQLType := (FXSQLDA.sqlvar[Index].sqltype and not(1));
    result := (ASQLType = SQL_BLOB) or (ASQLType = SQL_QUAD);
  end;

  function TALFBXSQLDA.GetIsBlobText(const Index: Word): boolean;
  var
    ASQLType: Word;
    ASubType: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLType := sqltype and not(1);
      ASubType := SqlSubType;
    end;
    result := ((ASQLType = SQL_BLOB) or (ASQLType = SQL_QUAD)) and (ASubType = 1);
  end;

  function TALFBXSQLDA.GetIsArray(const Index: Word): boolean;
  begin
    CheckRange(Index);
    result := ((FXSQLDA.sqlvar[Index].sqltype and not(1)) = SQL_ARRAY);
  end;

  function TALFBXSQLDA.GetIsNullable(const Index: Word): boolean;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      Result := (sqlind <> nil) and (sqltype <> sqltype and not(1));
  end;

  function TALFBXSQLDA.GetIsNumeric(const Index: Word): boolean;
  begin
    CheckRange(Index);
    result := (FXSQLDA.sqlvar[Index].SqlScale < 0);
  end;

  // TALFBXSQLDA.GetAs...

  function TALFBXSQLDA.GetAsDouble(const Index: Word): Double;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata), Result);
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cAlFBXDateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, Result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, Result);
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLDA.GetAsInt64(const Index: Word): Int64;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - cAlFBXDateOffset; // Only Date
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cAlFBXDateOffset;
          SQL_TYPE_TIME : ; // Result := 0; What else ??
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, Result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, Result);
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLDA.GetAsInteger(const Index: Word): Integer;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - cAlFBXDateOffset; // Only Date
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cAlFBXDateOffset;
          SQL_TYPE_TIME : ; // Result := 0; What else ??
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLDA.GetAsSingle(const Index: Word): Single;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : Result := ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cAlFBXDateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLDA.GetAsSmallint(const Index: Word): Smallint;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - cAlFBXDateOffset; // Only Date
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cAlFBXDateOffset;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
          SQL_TYPE_TIME : ; // Result := 0; What else ??
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

//  function TALFBXSQLDA.GetAsString(const Index: Word): string;
//  begin
//{$IFDEF UNICODE}
//    Result := GetAsUnicodeString(Index)
//{$ELSE}
//    Result := GetAsAnsiString(Index)
//{$ENDIF}
//  end;

  function TALFBXSQLDA.GetAsAnsiString(const Index: Word): AnsiString;
    function BoolToStr(const Value: boolean): AnsiString;
    begin if Value then result := cALFBXTrue else result := cALFBXFalse; end;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    Result := '';
    with FXSQLDA.sqlvar[Index] do
    begin
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := ALFormatFloat(cALFBXScaleFormat[sqlscale], PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale], ALDefaultFormatSettings);
          SQL_LONG   : Result := ALFormatFloat(cALFBXScaleFormat[sqlscale], PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale], ALDefaultFormatSettings);
          SQL_INT64,
          SQL_QUAD   : Result := ALFormatFloat(cALFBXScaleFormat[sqlscale], PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale], ALDefaultFormatSettings);
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := ALFormatFloat(cALFBXScaleFormat[sqlscale], PDouble(sqldata)^, ALDefaultFormatSettings);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_VARYING   : DecodeStringA(SQL_VARYING, Index, Result);
          SQL_TEXT      : DecodeStringA(SQL_TEXT, Index, Result);
          SQL_TIMESTAMP : Result := ALDateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)), ALDefaultFormatSettings);
          SQL_TYPE_DATE : Result := ALDateToStr(PInteger(sqldata)^ - cAlFBXDateOffset, ALDefaultFormatSettings);
          SQL_TYPE_TIME : Result := ALTimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff, ALDefaultFormatSettings);
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := ALFloatToStr(PDouble(sqldata)^, ALDefaultFormatSettings);
          SQL_LONG      : Result := ALIntToStr(PInteger(sqldata)^);
          SQL_FLOAT     : Result := ALFloatToStr(PSingle(sqldata)^, ALDefaultFormatSettings);
          SQL_SHORT     : Result := ALIntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := ALIntToStr(PInt64(sqldata)^);
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLDA.GetAsQuad(const Index: Word): TISCQuad;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      if not ((sqlind <> nil) and (sqlind^ = -1)) then
        case (sqltype and not(1)) of
          SQL_QUAD, SQL_DOUBLE, SQL_D_FLOAT, SQL_INT64, SQL_BLOB, SQL_ARRAY: result := PISCQuad(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end
      else
        Result := cALFBXQuadNull;
  end;

//  function TALFBXSQLDA.GetAsRawByteString(const Index: Word): RawByteString;
//    function BoolToStr(const Value: boolean): string;
//    begin if Value then result := cALFBXTrue else result := cALFBXFalse; end;
//  var ASQLCode: SmallInt;
//  begin
//    CheckRange(Index);
//    Result := '';
//    with FXSQLDA.sqlvar[Index] do
//    begin
//      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
//      ASQLCode := (sqltype and not(1));
//      // Is Numeric ?
//      if (sqlscale < 0)  then
//      begin
//        case ASQLCode of
//          SQL_SHORT  : Result := RawByteString(FormatFloat(cALFBXScaleFormat[sqlscale], PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale]));
//          SQL_LONG   : Result := RawByteString(FormatFloat(cALFBXScaleFormat[sqlscale], PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale]));
//          SQL_INT64,
//          SQL_QUAD   : Result := RawByteString(FormatFloat(cALFBXScaleFormat[sqlscale], PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale]));
//          SQL_D_FLOAT,
//          SQL_DOUBLE : Result := RawByteString(FormatFloat(cALFBXScaleFormat[sqlscale], PDouble(sqldata)^));
//        else
//          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
//        end;
//      end else
//        case ASQLCode of
//          SQL_VARYING   : DecodeStringB(SQL_VARYING, Index, Result);
//          SQL_TEXT      : DecodeStringB(SQL_TEXT, Index, Result);
//          SQL_TIMESTAMP : Result := RawByteString(DateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata))));
//          SQL_TYPE_DATE : Result := RawByteString(DateToStr(PInteger(sqldata)^ - cAlFBXDateOffset));
//          SQL_TYPE_TIME : Result := RawByteString(TimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff));
//          SQL_D_FLOAT,
//          SQL_DOUBLE    : Result := RawByteString(FloatToStr(PDouble(sqldata)^));
//          SQL_LONG      : Result := RawByteString(IntToStr(PInteger(sqldata)^));
//          SQL_FLOAT     : Result := RawByteString(FloatToStr(PSingle(sqldata)^));
//          SQL_SHORT     : Result := RawByteString(IntToStr(PSmallint(sqldata)^));
//          SQL_INT64     : Result := RawByteString(IntToStr(PInt64(sqldata)^));
//{FB25_UP}
//          SQL_NULL: ;
//{FB25_UP}
//        else
//          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
//        end;
//    end;
//  end;

//  function TALFBXSQLDA.GetAsVariant(const Index: Word): Variant;
//  var
//    ASQLCode: SmallInt;
//    Dbl: Double;
//  begin
//    CheckRange(Index);
//    with FXSQLDA.sqlvar[Index] do
//    begin
//      Result := NULL;
//      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
//      ASQLCode := (sqltype and not(1));
//      // Is Numeric ?
//      if (sqlscale < 0)  then
//      begin
//        case ASQLCode of
//          SQL_SHORT  : Result := PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale];
//          SQL_LONG   : Result := PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale];
//          SQL_INT64,
//          SQL_QUAD   : Result := PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale];
//          SQL_D_FLOAT,
//          SQL_DOUBLE : Result := PDouble(sqldata)^;
//        else
//          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
//        end;
//      end else
//        case ASQLCode of
//          SQL_D_FLOAT,
//          SQL_DOUBLE    : Result := PDouble(sqldata)^;
//          SQL_TIMESTAMP : Result := TDateTime(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
//          SQL_TYPE_DATE :
//            begin
//              Dbl := PInteger(sqldata)^ - cAlFBXDateOffset;
//              Result := TDateTime(Dbl);
//            end;
//          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
//          SQL_LONG      : Result := PInteger(sqldata)^;
//          SQL_FLOAT     : Result := PSingle(sqldata)^;
//          SQL_SHORT     : Result := PSmallint(sqldata)^;
//          SQL_INT64     : Result := PInt64(sqldata)^;
//          SQL_TEXT      : Result := DecodeString(SQL_TEXT, Index);
//          SQL_VARYING   : Result := DecodeString(SQL_VARYING, Index);
//{FB25_UP}
//          SQL_NULL: ;
//{FB25_UP}
//        else
//          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
//        end;
//    end;
//  end;

  function TALFBXSQLDA.GetAsDateTime(const Index: Word): TDateTime;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TIMESTAMP : ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata), Double(Result));
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cAlFBXDateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLDA.GetAsCurrency(const Index: Word): Currency;
  var
    ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_INT64,
          SQL_QUAD   : if (SqlScale = -4) then
                        PInt64(@Result)^ := PInt64(sqldata)^ else
                        if SqlScale > -4 then
                          PInt64(@Result)^ := PInt64(sqldata)^ * cALFBXCurrencyDivisor[SqlScale] else
                          PInt64(@Result)^ := PInt64(sqldata)^ div cALFBXCurrencyDivisor[SqlScale];
          SQL_LONG   : if (SqlScale = -4) then
                        PInt64(@Result)^ := PInteger(sqldata)^ else
                        if SqlScale > -14 then
                        begin
                          if SqlScale > -4 then
                            PInt64(@Result)^ := PInteger(sqldata)^ * Integer(cALFBXCurrencyDivisor[SqlScale]) else
                            PInt64(@Result)^ := PInteger(sqldata)^ div Integer(cALFBXCurrencyDivisor[SqlScale]);
                        end else
                        begin
                          if SqlScale > -4 then
                            PInt64(@Result)^ := PInteger(sqldata)^ * cALFBXCurrencyDivisor[SqlScale] else
                            PInt64(@Result)^ := PInteger(sqldata)^ div cALFBXCurrencyDivisor[SqlScale];
                        end;
          SQL_SHORT  : if (SqlScale = -4) then
                        PInt64(@Result)^ := PSmallint(sqldata)^ else
                        if SqlScale > -14 then
                        begin
                          if SqlScale > -4 then
                            PInt64(@Result)^ := PSmallint(sqldata)^ * Integer(cALFBXCurrencyDivisor[SqlScale]) else
                            PInt64(@Result)^ := PSmallint(sqldata)^ div Integer(cALFBXCurrencyDivisor[SqlScale]);
                        end else
                        begin
                          if SqlScale > -4 then
                            PInt64(@Result)^ := PSmallint(sqldata)^ * cALFBXCurrencyDivisor[SqlScale] else
                            PInt64(@Result)^ := PSmallint(sqldata)^ div cALFBXCurrencyDivisor[SqlScale];
                        end;
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_TIMESTAMP : Result := ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cAlFBXDateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
end;

  function TALFBXSQLDA.GetAsBoolean(const Index: Word): boolean;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := False;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div cALFBXScaleDivisor[sqlscale] <> 0;
          SQL_LONG   : Result := PInteger(sqldata)^  div cALFBXScaleDivisor[sqlscale] <> 0;
          SQL_INT64,
          SQL_QUAD   : Result := (PInt64(sqldata)^ div cALFBXScaleDivisor[sqlscale]) <> 0;
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^) > 0;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_SHORT     : Result := PSmallint(sqldata)^ <> 0;
          SQL_LONG      : Result := PInteger(sqldata)^ <> 0;
          SQL_INT64     : Result := PInt64(sqldata)^ <> 0;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^) <> 0;
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^) <> 0;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

//  function TALFBXSQLDA.GetAsUnicodeString(const Index: Word): UnicodeString;
//    function BoolToStr(const Value: boolean): string;
//    begin if Value then result := cALFBXTrue else result := cALFBXFalse; end;
//  var ASQLCode: SmallInt;
//  begin
//    CheckRange(Index);
//    Result := '';
//    with FXSQLDA.sqlvar[Index] do
//    begin
//      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
//      ASQLCode := (sqltype and not(1));
//      // Is Numeric ?
//      if (sqlscale < 0)  then
//      begin
//        case ASQLCode of
//          SQL_SHORT  : Result := FormatFloat(cALFBXScaleFormat[sqlscale], PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale]);
//          SQL_LONG   : Result := FormatFloat(cALFBXScaleFormat[sqlscale], PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale]);
//          SQL_INT64,
//          SQL_QUAD   : Result := FormatFloat(cALFBXScaleFormat[sqlscale], PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale]);
//          SQL_D_FLOAT,
//          SQL_DOUBLE : Result := FormatFloat(cALFBXScaleFormat[sqlscale], PDouble(sqldata)^);
//        else
//          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
//        end;
//      end else
//        case ASQLCode of
//          SQL_TEXT      : DecodeStringW(SQL_TEXT, Index, Result);
//          SQL_VARYING   : DecodeStringW(SQL_VARYING, Index, Result);
//          SQL_TIMESTAMP : Result := DateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
//          SQL_TYPE_DATE : Result := DateToStr(PInteger(sqldata)^ - cAlFBXDateOffset);
//          SQL_TYPE_TIME : Result := TimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff);
//          SQL_D_FLOAT,
//          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
//          SQL_LONG      : Result := IntToStr(PInteger(sqldata)^);
//          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
//          SQL_SHORT     : Result := IntToStr(PSmallint(sqldata)^);
//          SQL_INT64     : Result := IntToStr(PInt64(sqldata)^);
//{FB25_UP}
//          SQL_NULL: ;
//{FB25_UP}
//        else
//          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
//        end;
//    end;
//  end;

  function TALFBXSQLDA.GetAsDate(const Index: Word): Integer;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cAlFBXDateOffset;
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - cAlFBXDateOffset;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_TEXT      : ConvertStringToDate(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertStringToDate(SQL_VARYING, Index, result);
          SQL_TYPE_TIME : Result := 0;
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLDA.GetAsTime(const Index: Word): Cardinal;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^;
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_time;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
          SQL_TYPE_DATE : Result := 0;
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

 // TALFBXSQLDA.SetAs...

  procedure TALFBXSQLDA.SetIsNull(const Index: Word; const Value: boolean);
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      if (sqlind <> nil) then
        case Value of
          True  : sqlind^ := -1;
          False : sqlind^ :=  0;
        end;
  end;

  procedure TALFBXSQLDA.SetAsQuad(const Index: Word; const Value: TISCQuad);
  begin
    with FXSQLDA.sqlvar[Index] do
      begin
        case (sqltype and not(1)) of
          SQL_QUAD, SQL_DOUBLE, SQL_D_FLOAT, SQL_INT64, SQL_BLOB, SQL_ARRAY: PISCQuad(sqldata)^ := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
        if (sqlind <> nil) then
          if CompareMem(@Value, @cALFBXQuadNull, SizeOf(TIscQuad)) then
            sqlind^ := -1 else
            sqlind^ := 0;
      end;
  end;

//  procedure TALFBXSQLDA.SetAsRawByteString(const Index: Word; const Value: RawByteString);
//  var
//    ASQLCode: SmallInt;
//  begin
//    with FXSQLDA.sqlvar[Index] do
//    begin
//      ASQLCode := (sqltype and not(1));
//      // Is Numeric ?
//      if (sqlscale < 0)  then
//      begin
//        case ASQLCode of
//          SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(StrToFloat(string(Value)) * cALFBXScaleDivisor[sqlscale]);
//          SQL_LONG   : PInteger(sqldata)^  := Trunc(StrToFloat(string(Value)) * cALFBXScaleDivisor[sqlscale]);
//          SQL_INT64,
//          SQL_QUAD   : PInt64(sqldata)^    := Trunc(StrToFloat(string(Value)) * cALFBXScaleDivisor[sqlscale]);
//          SQL_D_FLOAT,
//          SQL_DOUBLE : PDouble(sqldata)^   := StrToFloat(string(Value));
//        else
//          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
//        end;
//      end else
//        case ASQLCode of
//          SQL_TEXT      : EncodeStringB(SQL_TEXT, Index, Value);
//          SQL_VARYING   : EncodeStringB(SQL_VARYING, Index, Value);
//          SQL_D_FLOAT,
//          SQL_DOUBLE    : PDouble(sqldata)^   := StrToFloat(string(Value));
//          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(StrToDateTime(string(Value)), PISCTimeStamp(sqldata));
//          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(StrToDate(string(Value))) + cAlFBXDateOffset);
//          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(StrToTime(string(Value))) * cALFBXTimeCoeff);
//          SQL_LONG      : PInteger(sqldata)^ := StrToInt(string(Value));
//          SQL_FLOAT     : PSingle(sqldata)^ := StrToFloat(string(Value));
//          SQL_SHORT     : PSmallint(sqldata)^ := StrToInt(string(Value));
//          SQL_INT64     : PInt64(sqldata)^ := StrToInt64(string(Value));
//{FB25_UP}
//          SQL_NULL: ;
//{FB25_UP}
//        else
//          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
//        end;
//        if (sqlind <> nil) then
//          sqlind^ := 0;
//    end;
//  end;

  procedure TALFBXSQLDA.SetAsDateTime(const Index: Word; const Value: TDateTime);
  var
    ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value)) + cAlFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, DateTimeToStr(Value));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, DateTimeToStr(Value));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALDateTimeToStr(Value, ALDefaultFormatSettings));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALDateTimeToStr(Value, ALDefaultFormatSettings));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TALFBXSQLDA.SetAsDate(const Index: Word; const Value: Integer);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + cAlFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, DateToStr(Value));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, DateToStr(Value));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALDateToStr(Value, ALDefaultFormatSettings));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALDateToStr(Value, ALDefaultFormatSettings));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TALFBXSQLDA.SetAsTime(const Index: Word; const Value: Cardinal);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := 0;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Value;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, TimeToStr(Value));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, TimeToStr(Value));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALTimeToStr(Value, ALDefaultFormatSettings));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALTimeToStr(Value, ALDefaultFormatSettings));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TALFBXSQLDA.SetAsBoolean(const Index: Word; const Value: Boolean);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := ord(Value) * cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := ord(Value) * cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := ord(Value) * cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := ord(Value);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := ord(Value);
          SQL_LONG      : PInteger(sqldata)^ := ord(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := ord(Value);
          SQL_SHORT     : PSmallint(sqldata)^ := ord(Value);
          SQL_INT64     : PInt64(sqldata)^ := ord(Value);
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(ord(Value)));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(ord(Value)));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALIntToStr(ord(Value)));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALIntToStr(ord(Value)));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TALFBXSQLDA.SetAsInteger(const Index: Word; const Value: Integer);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + cAlFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALIntToStr(Value));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALIntToStr(Value));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TALFBXSQLDA.SetAsSingle(const Index: Word; const Value: Single);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value)) + cAlFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALFloatToStr(Value, ALDefaultFormatSettings));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALFloatToStr(Value, ALDefaultFormatSettings));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TALFBXSQLDA.SetAsSmallint(const Index: Word; const Value: Smallint);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + cAlFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALIntToStr(Value));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALIntToStr(Value));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

//  procedure TALFBXSQLDA.SetAsString(const Index: Word; const Value: string);
//  begin
//{$IFDEF UNICODE}
//    SetAsUnicodeString(Index, Value);
//{$ELSE}
//    SetAsAnsiString(Index, Value);
//{$ENDIF}
//  end;

  procedure TALFBXSQLDA.SetAsAnsiString(const Index: Word; const Value: AnsiString);
  var
    ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(ALStrToFloat(Value, ALDefaultFormatSettings) * cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Trunc(ALStrToFloat(Value, ALDefaultFormatSettings) * cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Trunc(ALStrToFloat(Value, ALDefaultFormatSettings) * cALFBXScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := ALStrToFloat(Value, ALDefaultFormatSettings);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, Value);
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, Value);
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := ALStrToFloat(Value, ALDefaultFormatSettings);
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(ALStrToDateTime(Value, ALDefaultFormatSettings), PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(ALStrToDate(Value, ALDefaultFormatSettings)) + cAlFBXDateOffset);
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(ALStrToTime(Value, ALDefaultFormatSettings)) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := ALStrToInt(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := ALStrToFloat(Value, ALDefaultFormatSettings);
          SQL_SHORT     : PSmallint(sqldata)^ := ALStrToInt(Value);
          SQL_INT64     : PInt64(sqldata)^ := ALStrToInt64(Value);
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then
          sqlind^ := 0;
    end;
  end;

//  procedure TALFBXSQLDA.SetAsUnicodeString(const Index: Word;
//    const Value: UnicodeString);
//  var
//    ASQLCode: SmallInt;
//  begin
//    with FXSQLDA.sqlvar[Index] do
//    begin
//      ASQLCode := (sqltype and not(1));
//      // Is Numeric ?
//      if (sqlscale < 0)  then
//      begin
//        case ASQLCode of
//          SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(StrToFloat(Value) * cALFBXScaleDivisor[sqlscale]);
//          SQL_LONG   : PInteger(sqldata)^  := Trunc(StrToFloat(Value) * cALFBXScaleDivisor[sqlscale]);
//          SQL_INT64,
//          SQL_QUAD   : PInt64(sqldata)^    := Trunc(StrToFloat(Value) * cALFBXScaleDivisor[sqlscale]);
//          SQL_D_FLOAT,
//          SQL_DOUBLE : PDouble(sqldata)^   := StrToFloat(Value);
//        else
//          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
//        end;
//      end else
//        case ASQLCode of
//          SQL_TEXT      : EncodeStringW(SQL_TEXT, Index, Value);
//          SQL_VARYING   : EncodeStringW(SQL_VARYING, Index, Value);
//          SQL_D_FLOAT,
//          SQL_DOUBLE    : PDouble(sqldata)^   := StrToFloat(Value);
//          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(StrToDateTime(Value), PISCTimeStamp(sqldata));
//          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(StrToDate(Value)) + cAlFBXDateOffset);
//          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(StrToTime(Value)) * cALFBXTimeCoeff);
//          SQL_LONG      : PInteger(sqldata)^ := StrToInt(Value);
//          SQL_FLOAT     : PSingle(sqldata)^ := StrToFloat(Value);
//          SQL_SHORT     : PSmallint(sqldata)^ := StrToInt(Value);
//          SQL_INT64     : PInt64(sqldata)^ := StrToInt64(Value);
//{FB25_UP}
//          SQL_NULL: ;
//{FB25_UP}
//        else
//          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
//        end;
//        if (sqlind <> nil) then
//          sqlind^ := 0;
//    end;
//  end;

  procedure TALFBXSQLDA.SetAsInt64(const Index: Word; const Value: Int64);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * cALFBXScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Integer(Value), PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + cAlFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALIntToStr(Value));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALIntToStr(Value));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TALFBXSQLDA.SetAsDouble(const Index: Word; const Value: Double);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value)) + cAlFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALFloatToStr(Value, ALDefaultFormatSettings));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALFloatToStr(Value, ALDefaultFormatSettings));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TALFBXSQLDA.SetAsCurrency(const Index: Word;
    const Value: Currency);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Round(Value * cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : if (sqlscale = -4) then
                         PInt64(sqldata)^ := PInt64(@Value)^ else
                         if sqlscale > -4 then
                           PInt64(sqldata)^ := PInt64(@Value)^ div cALFBXCurrencyDivisor[sqlscale] else
                           PInt64(sqldata)^ := PInt64(@Value)^ * cALFBXCurrencyDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value) + cAlFBXDateOffset);
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          //SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
          //SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, ALFloatToStr(Value, ALDefaultFormatSettings));
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, ALFloatToStr(Value, ALDefaultFormatSettings));
{FB25_UP}
          SQL_NULL: ;
{FB25_UP}
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

//  procedure TALFBXSQLDA.SetAsVariant(const Index: Word; const Value: Variant);
//  begin
//    case TVarData(Value).VType of
//      varShortInt,
//      varSmallInt, varByte: SetAsSmallint(Index, Value);
//      varWord,
//      varInteger:               SetAsInteger(Index, Value);
//      varLongWord, varInt64:             SetAsInt64(Index, Value);
//      varSingle:                         SetAsSingle(Index, Value);
//      varDouble:                         SetAsDouble(Index, Value);
//      varCurrency:                       SetAsCurrency(Index, Value);
//      varDate:                           SetAsDateTime(Index, Value);
//      varOleStr, varString:              SetAsString(Index, Value);
//{$IFDEF UNICODE}
//      varUString:                        SetAsString(Index, Value);
//{$ENDIF}
//      varNull, VarEmpty:                 SetIsNull(index, true);
//    else
//      raise EALFBXConvertError.Create(cALFBX_CASTERROR);
//    end;
//  end;

  // TALFBXSQLDA.GetByName...

  function TALFBXSQLDA.GetFieldIndex(const name: AnsiString): Word;
  begin
    if not TryGetFieldIndex(name, Result) then
      raise Exception.CreateFmt(cALFBX_FIELDSTRNOTFOUND, [name]);
  end;

  function TALFBXSQLDA.TryGetFieldIndex(const name: AnsiString; out index: Word): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to GetAllocatedFields - 1 do
      if FXSQLDA.sqlvar[i].AliasNameLength = Length(name) then
        if {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrLIComp(PansiChar(@FXSQLDA.sqlvar[i].aliasname), PAnsiChar(Name),
          FXSQLDA.sqlvar[i].AliasNameLength) = 0 then
          begin
            index := i;
            Result := True;
            Exit;
          end;
    Result := False;
  end;

  function TALFBXSQLDA.GetByNameAsDouble(const Name: AnsiString): Double;
  begin
    Result := GetAsDouble(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsInt64(const Name: AnsiString): Int64;
  begin
    Result := GetAsInt64(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsInteger(const Name: AnsiString): Integer;
  begin
    Result := GetAsInteger(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsSingle(const Name: AnsiString): Single;
  begin
    Result := GetAsSingle(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsSmallint(const Name: AnsiString): Smallint;
  begin
    Result := GetAsSmallint(GetFieldIndex(Name));
  end;

//  function TALFBXSQLDA.GetByNameAsString(const name: string): string;
//  begin
//  {$IFDEF UNICODE}
//    Result := GetByNameAsUnicodeString(name);
//  {$ELSE}
//    Result := GetByNameAsAnsiString(name);
//  {$ENDIF}
//  end;

  function TALFBXSQLDA.GetByNameAsAnsiString(const Name: AnsiString): AnsiString;
  begin
    Result := GetAsAnsiString(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsQuad(const Name: AnsiString): TISCQuad;
  begin
    Result := GetAsQuad(GetFieldIndex(Name));
  end;

//  function TALFBXSQLDA.GetByNameAsRawByteString(const name: string): RawByteString;
//  begin
//    Result := GetAsRawByteString(GetFieldIndex(AnsiString(name)));
//  end;

//  function TALFBXSQLDA.GetByNameAsVariant(const Name: string): Variant;
//  begin
//    Result := GetAsVariant(GetFieldIndex(AnsiString(Name)));
//  end;

  function TALFBXSQLDA.GetByNameIsBlob(const Name: AnsiString): boolean;
  begin
    Result := GetIsBlob(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameIsBlobText(const Name: AnsiString): boolean;
  begin
    Result := GetIsBlobText(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameIsNull(const Name: AnsiString): boolean;
  begin
    Result := GetIsNull(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsDateTime(const Name: AnsiString): TDateTime;
  begin
    Result := GetAsDateTime(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsCurrency(const Name: AnsiString): Currency;
  begin
    Result := GetAsCurrency(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsBoolean(const Name: AnsiString): boolean;
  begin
    Result := GetAsBoolean(GetFieldIndex(Name));
  end;

//  function TALFBXSQLDA.GetByNameAsUnicodeString(const Name: string): UnicodeString;
//  begin
//    Result := GetAsUnicodeString(GetFieldIndex(AnsiString(Name)));
//  end;

  function TALFBXSQLDA.GetByNameAsDate(const Name: AnsiString): Integer;
  begin
    Result := GetAsDate(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsTime(const Name: AnsiString): Cardinal;
  begin
    Result := GetAsTime(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameIsNumeric(const Name: AnsiString): boolean;
  begin
    result := GetIsNumeric(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameIsNullable(const Name: AnsiString): boolean;
  begin
    Result := GetIsNullable(GetFieldIndex(Name));
  end;

  // TALFBXSQLDA.SetByNameAs

  procedure TALFBXSQLDA.SetByNameIsNull(const Name: AnsiString;
    const Value: boolean);
  begin
    SetIsNull(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsBoolean(const Name: AnsiString;
    const Value: boolean);
  begin
     SetAsBoolean(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsDate(const Name: AnsiString;
    const Value: Integer);
  begin
    SetAsDate(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsCurrency(const Name: AnsiString;
    const Value: Currency);
  begin
    SetAsCurrency(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsDateTime(const Name: AnsiString;
    const Value: TDateTime);
  begin
    SetAsDateTime(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsDouble(const Name: AnsiString;
    const Value: Double);
  begin
    SetAsDouble(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsInt64(const Name: AnsiString;
    const Value: Int64);
  begin
    SetAsInt64(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsInteger(const Name: AnsiString;
    const Value: Integer);
  begin
    SetAsInteger(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsQuad(const Name: AnsiString;
    const Value: TISCQuad);
  begin
    SetAsQuad(GetFieldIndex(Name), Value);
  end;

//  procedure TALFBXSQLDA.SetByNameAsRawByteString(const name: string; const Value: RawByteString);
//  begin
//    SetAsRawByteString(GetFieldIndex(AnsiString(Name)), Value);
//  end;

  procedure TALFBXSQLDA.SetByNameAsSingle(const Name: AnsiString; const Value: Single);
  begin
    SetAsSingle(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsSmallint(const Name: AnsiString;
    const Value: Smallint);
  begin
    SetAsSmallint(GetFieldIndex(Name), Value);
  end;

//  procedure TALFBXSQLDA.SetByNameAsString(const name, Value: string);
//  begin
//  {$IFDEF UNICODE}
//    SetByNameAsUnicodeString(name, Value);
//  {$ELSE}
//    SetByNameAsAnsiString(name, Value);;
//  {$ENDIF}
//  end;

  procedure TALFBXSQLDA.SetByNameAsAnsiString(const Name: AnsiString; const Value: AnsiString);
  begin
    SetAsAnsiString(GetFieldIndex(Name), Value);
  end;

//  procedure TALFBXSQLDA.SetByNameAsUnicodeString(const Name: string; const Value: UnicodeString);
//  begin
//    SetAsUnicodeString(GetFieldIndex(AnsiString(Name)), Value);
//  end;

//  procedure TALFBXSQLDA.SetByNameAsVariant(const Name: string;
//    const Value: Variant);
//  begin
//    SetAsVariant(GetFieldIndex(AnsiString(Name)), Value);
//  end;

  function TALFBXSQLDA.GetFieldType(const Index: Word): TALFBXFieldType;
  var
    typ: integer;
  begin
    CheckRange(Index);
    if (FXSQLDA.sqlvar[Index].SqlScale < 0) then
    begin
      typ := FXSQLDA.sqlvar[Index].sqltype and not (1);
      if (typ = SQL_DOUBLE) or (typ = SQL_D_FLOAT)  then
        Result := uftDoublePrecision else
        Result := uftNumeric;
    end else
    case FXSQLDA.sqlvar[Index].sqltype and not (1) of
      SQL_TEXT        : Result := uftChar;
      SQL_VARYING     : Result := uftVarchar;
      SQL_SHORT       : Result := uftSmallint;
      SQL_LONG        : Result := uftInteger;
      SQL_FLOAT       : Result := uftFloat;
      SQL_DOUBLE,
      SQL_D_FLOAT     : Result := uftDoublePrecision;
      SQL_TIMESTAMP   : Result := uftTimestamp;
      SQL_BLOB        : Result := uftBlob;
      SQL_QUAD        : Result := uftQuad;
      SQL_TYPE_TIME   : Result := uftTime;
      SQL_TYPE_DATE   : Result := uftDate;
      SQL_INT64       : Result := uftInt64;
      SQL_ARRAY       : Result := uftArray;
{FB25_UP}
      SQL_NULL        : Result := uftNull;
{FB25_UP}
    else
      Result := uftUnKnown;
    end;
  end;

{ TALFBXSQLResult }

  constructor TALFBXSQLResult.Create(Charset: TALFBXCharacterSet; Fields: SmallInt = 0;
    CachedFetch: Boolean = False;  FetchBlobs: boolean = false;
    BufferChunks: Cardinal = 1000);
  begin
    inherited Create(Charset);
    FStatBlobsSize := 0;
    FCachedFetch := CachedFetch;
    FFetchBlobs := FetchBlobs;
    FDataBufferLength := 0;
    FDataBuffer := nil;
    if Fields <= 0 then Fields := 0;
    GetMem(FXSQLDA, XSQLDA_LENGTH(Fields));
    FXSQLDA.sqln := Fields;
    FXSQLDA.sqld := Fields;
    FXSQLDA.version := SQLDA_CURRENT_VERSION;
    FBufferChunks := BufferChunks;
  end;

  destructor TALFBXSQLResult.Destroy;
  begin
    ClearRecords;
    if FDataBuffer <> nil then
    begin
      if (not FCachedFetch) and FFetchBlobs then
        FreeBlobs(FDataBuffer);
      FreeMem(FDataBuffer)
    end;
    FreeMem(FXSQLDA);
    inherited;
  end;

  procedure TALFBXSQLResult.AddCurrentRecord;
  begin
    if FRecordPool = nil then
      FRecordPool := TALFBXPoolStream.Create(FBufferChunks, FDataBufferLength);
    ALMove(FDataBuffer^, FRecordPool.Add^, FDataBufferLength);
    FCurrentRecord := FRecordPool.ItemCount - 1;
  end;

  procedure TALFBXSQLResult.ClearRecords;
  var
    i: Integer;
  begin
    FScrollEOF := False;
    FInMemoryEOF := False;
    if Assigned(FRecordPool) then
    begin
      if FFetchBlobs then
        for i := 0 to FRecordPool.ItemCount - 1 do
          FreeBlobs(FRecordPool[i]);
      FRecordPool.Free;
      FRecordPool := nil;
    end;
    FStatBlobsSize := 0;
  end;

  procedure TALFBXSQLResult.GetRecord(const Index: Integer);
  begin
    if (Index <> FCurrentRecord) and (FRecordPool <> nil) then
    begin
      ALMove(FRecordPool[Index]^, FDataBuffer^, FDataBufferLength);
      FCurrentRecord := Index;
    end;
    FInMemoryEOF := false;
  end;

  function TALFBXSQLResult.GetRecordCount: Integer;
  begin
    if Assigned(FRecordPool) then
      Result := FRecordPool.ItemCount else
      Result := 0;
  end;

  procedure TALFBXSQLResult.AllocateDataBuffer;
  var
    i, j, LastLen: SmallInt;
    BlobCount: Word;

    ArrayIndex: integer;
    ArrayItemLen: integer;
    ArrayItemCount: integer;
    PDesc: PALFBXArrayInfo;
    PBound: PISCArrayBound;
  begin
    FDataBufferLength := 0;
    LastLen    := 0;
    BlobCount := 0;
    ArrayIndex := 0;
    SetLength(FBlobsIndex, BlobCount);
    // calculate offsets and store them instead of pointers ;)
    for i := 0 to FXSQLDA.sqln - 1 do
    begin
      Inc(FDataBufferLength, LastLen);
      FXSQLDA.sqlvar[i].sqldata := Pointer(FDataBufferLength);
      case FXSQLDA.sqlvar[i].sqltype and not 1 of
        SQL_VARYING: LastLen := FXSQLDA.sqlvar[i].sqllen + 2;
        SQL_BLOB:
          begin
            LastLen := FXSQLDA.sqlvar[i].sqllen + SizeOf(TALFBXBlobData); // quad + datainfo
            inc(BlobCount);
            SetLength(FBlobsIndex, BlobCount);
            FBlobsIndex[BlobCount-1] := i;
          end;
        SQL_ARRAY:
          begin
            PDesc := @FArrayInfos[ArrayIndex];
            if (PDesc.info.array_desc_dtype in [blr_varying, blr_varying2]) then
              ArrayItemLen := PDesc.info.array_desc_length + 2 else
              ArrayItemLen := PDesc.info.array_desc_length;
            ArrayItemCount := 0;
            for j := 0 to PDesc.info.array_desc_dimensions - 1 do
            begin
              PBound := @PDesc.info.array_desc_bounds[j];
              inc(ArrayItemCount, PBound.array_bound_upper - PBound.array_bound_lower + 1);
            end;
            PDesc.size := ArrayItemCount * ArrayItemLen;
            LastLen :=  FXSQLDA.sqlvar[i].sqllen + PDesc.size; // quad + data
            inc(ArrayIndex);
          end;
      else
        LastLen := FXSQLDA.sqlvar[i].sqllen;
      end;

      if ((FXSQLDA.sqlvar[i].sqltype and 1) = 1) then
      begin
        Inc(FDataBufferLength, LastLen);
        FXSQLDA.sqlvar[i].sqlind := Pointer(FDataBufferLength);
        LastLen := SizeOf(SmallInt);
      end else
        FXSQLDA.sqlvar[i].sqlind := nil;
    end;
    Inc(FDataBufferLength, LastLen);

    // Now we have the total length needed
    if (FDataBuffer = nil) then
      GetMem(FDataBuffer, FDataBufferLength ) else
      ReallocMem(FDataBuffer, FDataBufferLength);
    FillChar(FDataBuffer^, FDataBufferLength, #0);

    // increment Offsets with the buffer
    for i := 0 to FXSQLDA.sqln - 1 do
    begin
      // I don't use cardinal for FPC compatibility
      inc(PtrInt(FXSQLDA.sqlvar[i].sqldata), PtrInt(FDataBuffer));
      if (FXSQLDA.sqlvar[i].sqlind <> nil) then
        inc(PtrInt(FXSQLDA.sqlvar[i].sqlind), PtrInt(FDataBuffer));
    end;
  end;

  // this method must be portable between 32/64 bit os
  procedure TALFBXSQLResult.SaveToStream(Stream: TStream);
  var
    Count, i, j: Integer;
    BlobData: PALFBXBlobData;
    ArrayData: Pointer;
    ArrayIndex: integer;
    null: boolean;
    OldRecord: integer;
    MaxStreamSize: int64;

  begin
    Stream.Write(FCachedFetch, SizeOf(FCachedFetch));
    Stream.Write(FFetchBlobs, SizeOf(FFetchBlobs));
    Stream.Write(FXSQLDA^.sqln, sizeof(FXSQLDA^.sqln));
    MaxStreamSize := 0;
    for i := 0 to FXSQLDA.sqln - 1 do
      with FXSQLDA.sqlvar[i] do
      begin
        Stream.Write(SqlType, sizeof(SqlType));
        Stream.Write(SqlScale, sizeof(SqlScale));
        Stream.Write(SqlSubType, sizeof(SqlSubType));
        Stream.Write(SqlLen, sizeof(SqlLen));
        Stream.Write(SqlNameLength, sizeof(SqlNameLength));
        Stream.Write(SqlName, sizeof(SqlName));
        Stream.Write(RelNameLength, sizeof(RelNameLength));
        Stream.Write(RelName, sizeof(RelName));
        Stream.Write(OwnNameLength, sizeof(OwnNameLength));
        Stream.Write(OwnName, sizeof(OwnName));
        Stream.Write(AliasNameLength, sizeof(AliasNameLength));
        Stream.Write(AliasName, sizeof(AliasName));

        if (sqltype and not(1)) = SQL_BLOB then
          inc(MaxStreamSize, Sizeof(Cardinal) + sizeof(null)) else // blob size
          inc(MaxStreamSize, SqlLen + sizeof(null)); // datalen  property
      end;

    // array informations
    count := length(FArrayInfos);
    Stream.Write(Count, SizeOf(Count));
    for i := 0 to Count - 1 do
      with FArrayInfos[i] do
      begin
        stream.Write(index, sizeof(index));
        stream.Write(size, sizeof(size));
        stream.Write(info.array_desc_dtype, sizeof(info.array_desc_dtype));
        stream.Write(info.array_desc_scale, sizeof(info.array_desc_scale));
        stream.Write(info.array_desc_length, sizeof(info.array_desc_length));
        stream.Write(info.array_desc_field_name, sizeof(info.array_desc_field_name));
        stream.Write(info.array_desc_relation_name, sizeof(info.array_desc_relation_name));
        stream.Write(info.array_desc_dimensions, sizeof(info.array_desc_dimensions));
        stream.Write(info.array_desc_flags, sizeof(info.array_desc_flags));
        for j := low(info.array_desc_bounds) to high(info.array_desc_bounds) do
          with info.array_desc_bounds[j] do
          begin
            stream.Write(array_bound_lower, sizeof(array_bound_lower));
            stream.Write(array_bound_upper, sizeof(array_bound_upper));
          end;
        inc(MaxStreamSize, size);
      end;

    Count := RecordCount;
    Stream.Write(Count, SizeOf(Count));
    OldRecord := FCurrentRecord;
    try
      MaxStreamSize := (MaxStreamSize * count) + FStatBlobsSize;
      Stream.Size := Stream.Size + MaxStreamSize;

      for i := 0 to Count - 1 do
      begin
        GetRecord(i);
        ArrayIndex := 0;
        for j := 0 to FXSQLDA.sqln - 1 do
          with FXSQLDA.sqlvar[j] do
            begin
              null := (sqlind <> nil) and (sqlind^ = -1);
              stream.Write(null, sizeof(null));
              case (sqltype and not(1)) of
                SQL_BLOB:
                  if not null then
                  begin
                    BlobData := GetDataQuadOffset(j);
                    Stream.Write(BlobData.Size,sizeof(BlobData.Size));
                    if BlobData.Size > 0 then
                      Stream.Write(BlobData.Buffer^,BlobData.Size);
                  end;
                SQL_ARRAY:
                  begin
                    if not null then
                    begin
                      ArrayData := GetDataQuadOffset(j);
                      stream.Write(ArrayData^, FArrayInfos[ArrayIndex].size);
                    end;
                    inc(ArrayIndex);
                  end;
                else
                  stream.Write(sqldata^, SqlLen);
                end;
            end;
      end;
    finally
      GetRecord(OldRecord);
      Stream.Size := Stream.Position;
    end;
  end;

  // this method must be portable between 32/64 bit os
  procedure TALFBXSQLResult.LoadFromStream(Stream: TStream);
  var
    Fields: SmallInt;
    Count, i, j: Integer;
    BlobData: PALFBXBlobData;
    ArrayData: Pointer;
    ArrayIndex: integer;
    null: boolean;
  begin
    // CleanUp
    ClearRecords;
    if (not FCachedFetch) and FFetchBlobs then
      FreeBlobs(FDataBuffer);

    Stream.Read(FCachedFetch, SizeOf(FCachedFetch));
    Stream.Read(FFetchBlobs, SizeOf(FFetchBlobs));

    Stream.Read(Fields, SizeOf(Fields));
    SetAllocatedFields(Fields);

    for i := 0 to Fields - 1 do
      with FXSQLDA.sqlvar[i] do
      begin
        Stream.Read(SqlType, sizeof(SqlType));
        Stream.Read(SqlScale, sizeof(SqlScale));
        Stream.Read(SqlSubType, sizeof(SqlSubType));
        Stream.Read(SqlLen, sizeof(SqlLen));
        Stream.Read(SqlNameLength, sizeof(SqlNameLength));
        Stream.Read(SqlName, sizeof(SqlName));
        Stream.Read(RelNameLength, sizeof(RelNameLength));
        Stream.Read(RelName, sizeof(RelName));
        Stream.Read(OwnNameLength, sizeof(OwnNameLength));
        Stream.Read(OwnName, sizeof(OwnName));
        Stream.Read(AliasNameLength, sizeof(AliasNameLength));
        Stream.Read(AliasName, sizeof(AliasName));
      end;

    // array informations
    Stream.Read(Count, SizeOf(Count));
    SetLength(FArrayInfos, Count);
    for i := 0 to Count - 1 do
      with FArrayInfos[i] do
      begin
        stream.Read(index, sizeof(index));
        stream.Read(size, sizeof(size));
        stream.Read(info.array_desc_dtype, sizeof(info.array_desc_dtype));
        stream.Read(info.array_desc_scale, sizeof(info.array_desc_scale));
        stream.Read(info.array_desc_length, sizeof(info.array_desc_length));
        stream.Read(info.array_desc_field_name, sizeof(info.array_desc_field_name));
        stream.Read(info.array_desc_relation_name, sizeof(info.array_desc_relation_name));
        stream.Read(info.array_desc_dimensions, sizeof(info.array_desc_dimensions));
        stream.Read(info.array_desc_flags, sizeof(info.array_desc_flags));
        for j := low(info.array_desc_bounds) to high(info.array_desc_bounds) do
          with info.array_desc_bounds[j] do
          begin
            stream.Read(array_bound_lower, sizeof(array_bound_lower));
            stream.Read(array_bound_upper, sizeof(array_bound_upper));
          end;
      end;

    // realloc & index buffer
    AllocateDataBuffer;
    Stream.Read(Count, SizeOf(Count));
    FBufferChunks := Count; // Inprove memory allocation
    for i := 0 to Count - 1 do
    begin
      ArrayIndex := 0;
      for j := 0 to FXSQLDA.sqln - 1 do
      with FXSQLDA.sqlvar[j] do
        begin
          stream.Read(null, sizeof(null));
          if SqlInd <> nil then
            case null of
              True  : sqlind^ := -1;
              False : sqlind^ :=  0;
            end;
            case (sqltype and not(1)) of
              SQL_BLOB:
                begin
                BlobData := GetDataQuadOffset(j);
                if not null then
                begin
                  Stream.Read(BlobData.Size, sizeof(BlobData.Size));
                  if BlobData.Size > 0 then
                  begin
                    GetMem(BlobData.Buffer, BlobData.Size);
                    Stream.Read(BlobData.Buffer^, BlobData.Size);
                  end else
                    BlobData.Buffer := nil;
                end else
                begin
                  BlobData.buffer := nil;
                  BlobData.Size := 0;
                end;
                end;
              SQL_ARRAY:
                begin
                  if not null then
                  begin
                    ArrayData := GetDataQuadOffset(j);
                    Stream.Read(ArrayData^, FArrayInfos[ArrayIndex].size)
                  end;
                  inc(ArrayIndex);
                end; // todo
            else
              stream.Read(sqldata^, SqlLen);
            end;
        end;
      AddCurrentRecord;
    end;
    FScrollEOF := True;
    FInMemoryEOF := true;
  end;

  function TALFBXSQLResult.GetCurrentRecord: Integer;
  begin
    if (FRecordPool = nil) then
      Result := -1 else
      Result := FCurrentRecord;
  end;

  procedure TALFBXSQLResult.FreeBlobs(Buffer: Pointer);
  var
    BlobData: PALFBXBlobData;
    I: integer;
  begin
    for I := 0 to Length(FBlobsIndex) - 1 do
    begin
      BlobData := PALFBXBlobData(PtrInt(Buffer) + (PtrInt(GetDataQuadOffset(FBlobsIndex[I])) - PtrInt(FDataBuffer)));
      if BlobData.Size > 0 then
        FreeMem(BlobData.Buffer);
    end;
  end;

  function TALFBXSQLResult.GetEof: boolean;
  begin
    Result := FScrollEOF and (
       (not CachedFetch) or
       (RecordCount = 0) or
       FInMemoryEOF);
  end;

  function TALFBXSQLResult.GetBof: boolean;
  begin
    Result := (FCurrentRecord = 0) or (RecordCount = 0);
  end;

  procedure TALFBXSQLResult.ReadBlobA(const Index: Word; var str: AnsiString);
//{$IFDEF UNICODE}
//  var
//    data: UnicodeString;
//{$ENDIF}
//  begin
//{$IFDEF UNICODE}
//    ReadBlobW(Index, data);
//    str := AnsiString(data);
//{$ELSE}
//    ReadBlobB(Index, str);
//{$ENDIF}
//  end;
  var
    BlobData: PALFBXBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    SetLength(str, BlobData.Size);
    ALMove(BlobData.Buffer^, PAnsiChar(data)^, BlobData.Size);
  end;

  procedure TALFBXSQLResult.ReadBlob(const Index: Word; Stream: TStream);
  var BlobData: PALFBXBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    Stream.Seek(0, 0);
    Stream.Write(BlobData.Buffer^, BlobData.Size);
    Stream.Seek(0, 0);
  end;

//  procedure TALFBXSQLResult.ReadBlob(const Index: Word; var Value: Variant);
//  var
//    PData: Pointer;
//    BlobData: PALFBXBlobData;
//    str: string;
//  begin
//    if IsBlobText[Index] then
//    begin
//      ReadBlob(Index, str);
//      Value := str;
//    end else
//    begin
//      if not FFetchBlobs then
//        raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
//      BlobData := GetDataQuadOffset(Index);
//      Value := VarArrayCreate([0, BlobData.Size-1], varByte);
//      PData := VarArrayLock(Value);
//      try
//        Move(BlobData.Buffer^, PData^, BlobData.Size);
//      finally
//        VarArrayUnlock(Value);
//      end;
//    end;
//  end;

//  procedure TALFBXSQLResult.ReadBlobW(const Index: Word; var str: UnicodeString);
//  var
//    aStr: RawByteString;
//  begin
//    ReadBlobB(Index, aStr);
//    if FXSQLDA.sqlvar[Index].SqlSubType = 1 then  // is text ?
//      str := ALFBXMBUDecode(aStr, cALFBXCharacterSetCP[FCharacterSet]) else
//      begin
//        SetLength(str, Length(aStr) div 2);
//        Move(PByte(aStr)^, PByte(str)^, Length(str) * 2);
//      end;
//  end;

//  procedure TALFBXSQLResult.ReadBlob(const Index: Word; var str: string);
//  begin
//  {$IFDEF UNICODE}
//     ReadBlobW(Index, str);
//  {$ELSE}
//     ReadBlobA(Index, str);
//  {$ENDIF}
//  end;

  procedure TALFBXSQLResult.ReadBlob(const Index: Word; Data: Pointer);
  var BlobData: PALFBXBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    ALMove(BlobData.Buffer^, Data^, BlobData.Size);
  end;

  procedure TALFBXSQLResult.ReadBlob(const name: AnsiString; Data: Pointer);
  begin
    ReadBlob(GetFieldIndex(Name), Data);
  end;

//  function TALFBXSQLResult.ReadBlob(const Index: Word): string;
//  begin
//    ReadBlob(Index, Result);
//  end;

//  function TALFBXSQLResult.ReadBlob(const name: string): string;
//  begin
//    ReadBlob(name, Result);
//  end;

//  procedure TALFBXSQLResult.ReadBlobB(const Index: Word; var data: RawByteString);
//  var
//    BlobData: PALFBXBlobData;
//  begin
//    CheckRange(Index);
//    if not FFetchBlobs then
//      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
//    BlobData := GetDataQuadOffset(Index);
//    SetLength(data, BlobData.Size);
//    Move(BlobData.Buffer^, PAnsiChar(data)^, BlobData.Size);
//  end;

//  procedure TALFBXSQLResult.ReadBlobW(const name: string; var str: UnicodeString);
//  begin
//    ReadBlobW(GetFieldIndex(AnsiString(Name)), str);
//  end;

//  procedure TALFBXSQLResult.ReadBlob(const name: string; var str: string);
//  begin
//  {$IFDEF UNICODE}
//     ReadBlobW(name, str);
//  {$ELSE}
//     ReadBlobA(name, str);
//  {$ENDIF}
//  end;


  procedure TALFBXSQLResult.ReadBlobA(const name: AnsiString; var str: AnsiString);
  begin
    ReadBlobA(GetFieldIndex(Name), str);
  end;

//  procedure TALFBXSQLResult.ReadBlobB(const name: string; var data: RawByteString);
//  begin
//    ReadBlobB(GetFieldIndex(AnsiString(Name)), data);
//  end;

  procedure TALFBXSQLResult.ReadBlob(const name: AnsiString; Stream: TStream);
  begin
    ReadBlob(GetFieldIndex(Name), Stream);
  end;

//  procedure TALFBXSQLResult.ReadBlob(const name: string; var Value: Variant);
//  begin
//    ReadBlob(GetFieldIndex(AnsiString(Name)), Value);
//  end;

  function TALFBXSQLResult.GetBlobSize(const Index: Word): Cardinal;
  var BlobData: PALFBXBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    Result := BlobData.Size;
  end;

  function TALFBXSQLResult.GetAsAnsiString(const Index: Word): AnsiString;
    function BoolToStr(const Value: boolean): AnsiString;
    begin if Value then result := cALFBXTrue else result := cALFBXFalse; end;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    Result := '';
    with FXSQLDA.sqlvar[Index] do
    begin
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := ALFormatFloat(cALFBXScaleFormat[sqlscale], PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale], ALDefaultFormatSettings);
          SQL_LONG   : Result := ALFormatFloat(cALFBXScaleFormat[sqlscale], PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale], ALDefaultFormatSettings);
          SQL_INT64,
          SQL_QUAD   : Result := ALFormatFloat(cALFBXScaleFormat[sqlscale], PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale], ALDefaultFormatSettings);
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := ALFormatFloat(cALFBXScaleFormat[sqlscale], PDouble(sqldata)^, ALDefaultFormatSettings);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := ALFloatToStr(PDouble(sqldata)^, ALDefaultFormatSettings);
          SQL_TIMESTAMP : Result := ALDateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)), ALDefaultFormatSettings);
          SQL_TYPE_DATE : Result := ALDateToStr(PInteger(sqldata)^ - cAlFBXDateOffset, ALDefaultFormatSettings);
          SQL_TYPE_TIME : Result := ALTimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff, ALDefaultFormatSettings);
          SQL_LONG      : Result := ALIntToStr(PInteger(sqldata)^);
          SQL_FLOAT     : Result := ALFloatToStr(PSingle(sqldata)^, ALDefaultFormatSettings);
          SQL_SHORT     : Result := ALIntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := ALIntToStr(PInt64(sqldata)^);
          SQL_TEXT      : DecodeStringA(SQL_TEXT, Index, Result);
          SQL_VARYING   : DecodeStringA(SQL_VARYING, Index, Result);
          SQL_BLOB      : ReadBlobA(Index, Result);
          SQL_ARRAY     : Result := '(Array)';
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

//  function TALFBXSQLResult.GetAsRawByteString(const Index: Word): RawByteString;
//    function BoolToStr(const Value: boolean): string;
//    begin if Value then result := cALFBXTrue else result := cALFBXFalse; end;
//  var ASQLCode: SmallInt;
//  begin
//    CheckRange(Index);
//    Result := '';
//    with FXSQLDA.sqlvar[Index] do
//    begin
//      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
//      ASQLCode := (sqltype and not(1));
//      // Is Numeric ?
//      if (sqlscale < 0)  then
//      begin
//        case ASQLCode of
//          SQL_SHORT  : Result := RawByteString(FormatFloat(cALFBXScaleFormat[sqlscale], PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale]));
//          SQL_LONG   : Result := RawByteString(FormatFloat(cALFBXScaleFormat[sqlscale], PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale]));
//          SQL_INT64,
//          SQL_QUAD   : Result := RawByteString(FormatFloat(cALFBXScaleFormat[sqlscale], PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale]));
//          SQL_D_FLOAT,
//          SQL_DOUBLE : Result := RawByteString(FormatFloat(cALFBXScaleFormat[sqlscale], PDouble(sqldata)^));
//        else
//          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
//        end;
//      end else
//        case ASQLCode of
//          SQL_D_FLOAT,
//          SQL_DOUBLE    : Result := RawByteString(FloatToStr(PDouble(sqldata)^));
//          SQL_TIMESTAMP : Result := RawByteString(DateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata))));
//          SQL_TYPE_DATE : Result := RawByteString(DateToStr(PInteger(sqldata)^ - cAlFBXDateOffset));
//          SQL_TYPE_TIME : Result := RawByteString(TimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff));
//          SQL_LONG      : Result := RawByteString(IntToStr(PInteger(sqldata)^));
//          SQL_FLOAT     : Result := RawByteString(FloatToStr(PSingle(sqldata)^));
//          SQL_SHORT     : Result := RawByteString(IntToStr(PSmallint(sqldata)^));
//          SQL_INT64     : Result := RawByteString(IntToStr(PInt64(sqldata)^));
//          SQL_TEXT      : DecodeStringB(SQL_TEXT, Index, Result);
//          SQL_VARYING   : DecodeStringB(SQL_VARYING, Index, Result);
//          SQL_BLOB      : ReadBlobB(Index, Result);
//          SQL_ARRAY     : Result := '(Array)';
//        else
//          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
//        end;
//    end;
//  end;

//  function TALFBXSQLResult.GetAsUnicodeString(const Index: Word): UnicodeString;
//    function BoolToStr(const Value: boolean): string;
//    begin if Value then result := cALFBXTrue else result := cALFBXFalse; end;
//  var ASQLCode: SmallInt;
//  begin
//    CheckRange(Index);
//    Result := '';
//    with FXSQLDA.sqlvar[Index] do
//    begin
//      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
//      ASQLCode := (sqltype and not(1));
//      // Is Numeric ?
//      if (sqlscale < 0)  then
//      begin
//        case ASQLCode of
//          SQL_SHORT  : Result := FormatFloat(cALFBXScaleFormat[sqlscale], PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale]);
//          SQL_LONG   : Result := FormatFloat(cALFBXScaleFormat[sqlscale], PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale]);
//          SQL_INT64,
//          SQL_QUAD   : Result := FormatFloat(cALFBXScaleFormat[sqlscale], PInt64(sqldata)^ / cALFBXScaleDivisor[sqlscale]);
//          SQL_D_FLOAT,
//          SQL_DOUBLE : Result := FormatFloat(cALFBXScaleFormat[sqlscale], PDouble(sqldata)^);
//        else
//          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
//        end;
//      end else
//        case ASQLCode of
//          SQL_D_FLOAT,
//          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
//          SQL_TIMESTAMP : Result := DateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
//          SQL_TYPE_DATE : Result := DateToStr(PInteger(sqldata)^ - cAlFBXDateOffset);
//          SQL_TYPE_TIME : Result := TimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff);
//          SQL_LONG      : Result := IntToStr(PInteger(sqldata)^);
//          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
//          SQL_SHORT     : Result := IntToStr(PSmallint(sqldata)^);
//          SQL_INT64     : Result := IntToStr(PInt64(sqldata)^);
//          SQL_TEXT      : DecodeStringW(SQL_TEXT, Index, Result);
//          SQL_VARYING   : DecodeStringW(SQL_VARYING, Index, Result);
//          SQL_BLOB      : ReadBlobW(Index, Result);
//          SQL_ARRAY     : Result := '(Array)';
//        else
//          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
//        end;
//    end;
//  end;

//  function TALFBXSQLResult.GetAsVariant(const Index: Word): Variant;
//  var
//    ASQLCode: SmallInt;
//    Dbl: Double;
//  begin
//    CheckRange(Index);
//    with FXSQLDA.sqlvar[Index] do
//    begin
//      Result := NULL;
//      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
//      ASQLCode := (sqltype and not(1));
//      // Is Numeric ?
//      if (sqlscale < 0)  then
//      begin
//        case ASQLCode of
//          SQL_SHORT  : Result := PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale];
//          SQL_LONG   : Result := PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale];
//          SQL_INT64,
//          SQL_QUAD   : Result := PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale];
//          SQL_D_FLOAT,
//          SQL_DOUBLE : Result := PDouble(sqldata)^;
//        else
//          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
//        end;
//      end else
//        case ASQLCode of
//          SQL_D_FLOAT,
//          SQL_DOUBLE    : Result := PDouble(sqldata)^;
//          SQL_TIMESTAMP : Result := TDateTime(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
//          SQL_TYPE_DATE :
//            begin
//              Dbl := PInteger(sqldata)^ - cAlFBXDateOffset;
//              Result := TDateTime(Dbl);
//            end;
//          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
//          SQL_LONG      : Result := PInteger(sqldata)^;
//          SQL_FLOAT     : Result := PSingle(sqldata)^;
//          SQL_SHORT     : Result := PSmallint(sqldata)^;
//          SQL_INT64     : Result := PInt64(sqldata)^;
//          SQL_TEXT      : Result := DecodeString(SQL_TEXT, Index);
//          SQL_VARYING   : Result := DecodeString(SQL_VARYING, Index);
//          SQL_BLOB      : ReadBlob(Index, Result);
//          SQL_ARRAY     : Result := '(Array)';
//        else
//          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
//        end;
//    end;
//  end;

  function TALFBXSQLResult.GetUniqueRelationName: AnsiString;
  var
    i: integer;
  begin
    result := '';
    if FXSQLDA.sqln > 1 then
      for i := 0 to FXSQLDA.sqln - 2 do
        if not ((FXSQLDA.sqlvar[i].RelNameLength = FXSQLDA.sqlvar[i+1].RelNameLength) and
          ({$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrIComp(FXSQLDA.sqlvar[i].RelName, FXSQLDA.sqlvar[i+1].RelName) = 0)) then
            exit;
    if FXSQLDA.sqln > 0 then
      SetString(Result, FXSQLDA.sqlvar[0].RelName, FXSQLDA.sqlvar[0].RelNameLength);
  end;

  function TALFBXSQLResult.GetDataQuadOffset(const index: word): Pointer;
  begin
    result := FXSQLDA.sqlvar[index].SqlData;
    inc(PtrInt(result), sizeof(TIscQuad));
  end;

  function TALFBXSQLResult.GetArrayData(const index: word): Pointer;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      result := nil;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      if (sqltype and not 1) = SQL_ARRAY then
        result := GetDataQuadOffset(index) else
        raise EALFBXConvertError.Create(cALFBX_CASTERROR);
    end;
  end;

  function TALFBXSQLResult.GetBlobData(const index: word): PALFBXBlobData;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      result := nil;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      if (sqltype and not 1) = SQL_BLOB then
        result := GetDataQuadOffset(index) else
        raise EALFBXConvertError.Create(cALFBX_CASTERROR);
    end;
  end;

  function TALFBXSQLResult.GetArrayCount: Word;
  begin
    result := length(FArrayInfos);
  end;

  function TALFBXSQLResult.GetArrayInfos(const index: Word): PALFBXArrayInfo;
  begin
    if (index < length(FArrayInfos)) then
      result := @FArrayInfos[index] else
      raise Exception.CreateFmt(cALFBX_INDEXERROR, [index]);
  end;

  procedure TALFBXSQLResult.Next;
  begin
    if CurrentRecord + 1 = RecordCount then
      FInMemoryEOF := True else
      CurrentRecord := CurrentRecord + 1;
  end;

function TALFBXSQLResult.ReadBlobA(const name: AnsiString): AnsiString;
begin
  ReadBlobA(name, Result);
end;

function TALFBXSQLResult.ReadBlobA(const Index: Word): AnsiString;
begin
  ReadBlobA(Index, Result);
end;

//function TALFBXSQLResult.ReadBlobB(const name: string): RawByteString;
//begin
//  ReadBlobB(name, result);
//end;

//function TALFBXSQLResult.ReadBlobB(const Index: Word): RawByteString;
//begin
//  ReadBlobB(Index, Result);
//end;

//function TALFBXSQLResult.ReadBlobW(const name: string): UnicodeString;
//begin
//  ReadBlobW(name, Result);
//end;

//function TALFBXSQLResult.ReadBlobW(const Index: Word): UnicodeString;
//begin
//  ReadBlobW(Index, Result);
//end;

{ TALFBXPoolStream }

function TALFBXPoolStream.Add: Pointer;
var
  item: integer;
begin
  item := FItemCount;
  SetSize((item + 1) * FItemSize);
  Result := Pointer(PtrInt(FPages[item div FItemsInPage]) + (Item mod FItemsInPage) * FItemSize);
end;

procedure TALFBXPoolStream.Clear;
var
  i: integer;
begin
  if (FPageCount > 0) then
  begin
    for i := 0 to FPageCount - 1 do
      FreeMem(FPages[i]);
    FreeMem(FPages);
    FPages := nil;
    FPageCount := 0;
    FSize := 0;
    FPosition := 0;
    FItemCount := 0;
  end;
end;

constructor TALFBXPoolStream.Create(ItemsInPage, ItemSize: Integer);
begin
  System.Assert((ItemSize > 0) and (ItemsInPage > 0));
  FItemSize := ItemSize;
  FItemsInPage := ItemsInPage;
  FPageSize := FItemSize * FItemsInPage;
  // round to 4 bytes
  FInternalPageSize := ((FPageSize + 3) div 4) shl 2;
  FPageCount := 0;
  FPages := nil;
  FSize := 0;
  FPosition := 0;
  FItemCount := 0;
end;

destructor TALFBXPoolStream.Destroy;
begin
  Clear;
  inherited;
end;

function TALFBXPoolStream.Get(Item: Integer): Pointer;
begin
  system.assert(Item * FItemSize <= FSize);
  Result := Pointer(PtrInt(FPages[Item div FItemsInPage]) + (Item mod FItemsInPage) * FItemSize);
end;

procedure TALFBXPoolStream.LoadFromFile(const FileName: AnsiString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(String(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TALFBXPoolStream.LoadFromStream(Stream: TStream);
var
  s, count, i: integer;
begin
  Stream.Position := 0;
  SetSize(Stream.Size);
  count := FSize;
  i := 0;
  while count > 0 do
  begin
    if count > FPageSize then
      s := FPageSize else
      s := count;
    stream.ReadBuffer(FPages[i]^, s);
    dec(count, s);
    inc(i);
  end;
end;

function TALFBXPoolStream.Read(var Buffer; Count: Integer): Longint;
var
  Pos, n: Integer;
  p, c: Pointer;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
        count := FSize - FPosition;
      Result := Count;
      c := @buffer;
      n := FPageSize - (FPosition mod FPageSize);
      if n > count then n := count;
      while n > 0 do
      begin
        p := Pointer(PtrInt(FPages[FPosition div FPageSize]) + (FPosition mod FPageSize));
        ALMove(p^, c^, n);
        dec(count, n);
        inc(PtrInt(c), n);
        inc(FPosition, n);
        if count >= FPageSize then
          n := FPageSize else
          n := count;
      end;
      Exit;
    end;
  end;
  Result := 0;
end;

procedure TALFBXPoolStream.SaveToFile(const FileName: AnsiString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(String(FileName), fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TALFBXPoolStream.SaveToStream(Stream: TStream);
var
  s, count, i: integer;
begin
  count := FSize;
  i := 0;
  while count > 0 do
  begin
    if count >= FPageSize then
      s := FPageSize else
      s := count;
    stream.WriteBuffer(FPages[i]^, s);
    dec(count, s);
    inc(i);
  end;
end;

function TALFBXPoolStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

function TALFBXPoolStream.SeekTo(Item: Integer): Longint;
begin
  Result := Seek(soFromBeginning, Item * FItemSize);
end;

procedure TALFBXPoolStream.SetSize(NewSize: Integer);
var
  count, i: integer;
begin
  if (NewSize mod FPageSize) > 0 then
    count := (NewSize div FPageSize) + 1 else
    count := (NewSize div FPageSize);
  if (count > FPageCount) then
  begin
    ReallocMem(FPages, count * sizeof(Pointer));
    for i := FPageCount to count - 1 do
      GetMem(FPages[i], FInternalPageSize);
  end else
  if (count < FPageCount) then
  begin
    for i := FPageCount - 1 downto Count do
      FreeMem(FPages[i]);
    ReallocMem(FPages, count * sizeof(Pointer));
  end;
  FSize := NewSize;
  FPageCount := count;
  FItemCount := FSize div FItemSize;
  if FPosition > FSize then
    Seek(0, soFromEnd);
end;

function TALFBXPoolStream.Write(const Buffer; Count: Integer): Longint;
var
  Pos, n: Integer;
  p, c: Pointer;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      Result := Count;
      if Pos > FSize then
        SetSize(Pos);
      c := @buffer;
      n := FPageSize - (FPosition mod FPageSize);
      if n > count then n := count;
      while n > 0 do
      begin
        p := Pointer(PtrInt(FPages[FPosition div FPageSize]) + (FPosition mod FPageSize));
        ALMove(c^, p^, n);
        dec(count, n);
        inc(PtrInt(c), n);
        inc(FPosition, n);
        if count >= FPageSize then
          n := FPageSize else
          n := count;
      end;
      Exit;
    end;
  end;
  Result := 0;
end;

{ TALFBXSQLParams }

  constructor TALFBXSQLParams.Create(Charset: TALFBXCharacterSet);
  begin
    inherited Create(Charset);
    GetMem(FXSQLDA, XSQLDA_LENGTH(0));
    FillChar(FXSQLDA^, XSQLDA_LENGTH(0), #0);
    FXSQLDA.sqln := 0;
    FXSQLDA.sqld := 0;
    FXSQLDA.version := SQLDA_CURRENT_VERSION;
    FParamCount := 0;
  end;

  destructor TALFBXSQLParams.Destroy;
  begin
    Clear;
    FreeMem(FXSQLDA);
    inherited;
  end;

  function TALFBXSQLParams.GetFieldName(const Index: Word): AnsiString;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].ParamName,
      FXSQLDA.sqlvar[Index].ParamNameLength);
  end;

procedure TALFBXSQLParams.AddFieldType(const Name: AnsiString; FieldType: TALFBXFieldType;
    Scale: TALFBXScale = 1; Precision: byte = 0);
  begin
    case FieldType of
      uftNumeric   :
        begin
          case Precision of
            0..4: SetFieldType(AddFieldA(name), SizeOf(Smallint), SQL_SHORT + 1, -scale);
            5..7: SetFieldType(AddFieldA(name), SizeOf(Integer) , SQL_LONG + 1 , -scale);
          else
            SetFieldType(AddFieldA(name), SizeOf(Int64), SQL_INT64 + 1, -scale);
          end;
        end;
      uftChar,
      uftVarchar,
      uftCstring         : SetFieldType(AddFieldA(name), 0                    , SQL_TEXT      + 1, 0);
      uftSmallint        : SetFieldType(AddFieldA(name), SizeOf(Smallint)     , SQL_SHORT     + 1, 0);
      uftInteger         : SetFieldType(AddFieldA(name), SizeOf(Integer)      , SQL_LONG      + 1, 0);
      uftQuad            : SetFieldType(AddFieldA(name), SizeOf(TISCQuad)     , SQL_QUAD      + 1, 0);
      uftFloat           : SetFieldType(AddFieldA(name), SizeOf(Single)       , SQL_FLOAT     + 1, 0);
      uftDoublePrecision : SetFieldType(AddFieldA(name), SizeOf(Double)       , SQL_DOUBLE    + 1, 0);
      uftTimestamp       : SetFieldType(AddFieldA(name), SizeOf(TISCTimeStamp), SQL_TIMESTAMP + 1, 0);
      uftBlob,
      uftBlobId          : SetFieldType(AddFieldA(name), SizeOf(TISCQuad)     , SQL_BLOB      + 1, 0);
      uftDate            : SetFieldType(AddFieldA(name), SizeOf(Integer)      , SQL_TYPE_DATE + 1, 0);
      uftTime            : SetFieldType(AddFieldA(name), SizeOf(Cardinal)     , SQL_TYPE_TIME + 1, 0);
      uftInt64           : SetFieldType(AddFieldA(name), SizeOf(Int64)        , SQL_INT64     + 1, 0);
      uftArray           : SetFieldType(AddFieldA(name), SizeOf(TISCQuad)     , SQL_ARRAY     + 1, 0);
    end;
  end;

  procedure TALFBXSQLParams.AllocateDataBuffer(AInit: boolean);
  var
    i, j: integer;
  begin
    for i := 0 to FXSQLDA.sqln - 1 do
      with FXSQLDA.sqlvar[i] do
      begin
        if AInit then
          Include(Flags, pfNotInitialized) else
          Exclude(Flags, pfNotInitialized);

        if (SqlLen > 0) and (SqlData = nil) then
          GetMem(sqldata, SqlLen);

        if ParamNameLength > 0 then
          for j := i + 1 to FXSQLDA.sqln - 1 do
            if (ID = FXSQLDA.sqlvar[j].ID) then
            begin
              ALMove(FXSQLDA.sqlvar[i], FXSQLDA.sqlvar[j], SizeOf(TALFBXSQLVar)-cALFBXMaxParamLength-2);
              Break;
            end;
      end;
  end;

  procedure TALFBXSQLParams.SetFieldType(const Index: Word; Size: Integer; Code,
    Scale: Smallint);
  var i: Word;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      if pfNotInitialized in Flags then  // need to be set, cf addfield
      begin
        Exclude(Flags, pfNotInitialized); // don't need to be set
        sqltype := Code;
        sqlscale := Scale;
        sqllen := Size;
        if (Size > 0) then
          GetMem(sqldata, Size) else
          sqldata := nil;
        if ParamNameLength > 0 then
          for i := 0 to GetAllocatedFields - 1 do
            if (i <> Index) and (ID = FXSQLDA.sqlvar[i].ID) then
              ALMove(FXSQLDA.sqlvar[Index], FXSQLDA.sqlvar[i], SizeOf(TALFBXSQLVar)-cALFBXMaxParamLength-2);
      end;
  end;

  function TALFBXSQLParams.Parse(const SQL: AnsiString): AnsiString;
  const
    Identifiers: set of AnsiChar = ['a'..'z', 'A'..'Z', '0'..'9', '_', '$'];
  var
    Src: PAnsiChar;
    Dest, idlen: Word;

    procedure next;
    begin
      inc(dest);
      Result[dest] := AnsiChar(Src^);
      inc(Src);
    end;

    procedure Skip(c: AnsiChar);
    begin
      repeat
        next;
        if Src^ = c then
        begin
          Next;
          Break;
        end;
      until (Src^ = #0);
    end;

  begin
    Clear;
    Src := PAnsiChar(SQL);
    Dest := 0;
    SetLength(Result, Length(SQL));
    while true do
      case Src^ of
        // eof
        #0 :  begin
                SetLength(Result, Dest);
                Exit;
              end;
        // normal comments
        '/' : if Src[1] = '*' then
              begin
                inc(Src, 2);
                while (Src^ <> #0) do
                  if (Src^ = '*') and (Src[1] = '/') then
                  begin
                    inc(Src, 2);
                    Break;
                  end else
                    inc(Src);
              end else
                next;
        // Firebird comments -- My comment + (eol or eof)
        {.$IFDEF FB15_UP}
        '-' : if Src[1] = '-' then
              begin
                inc(Src, 2);
                while not (AnsiChar(Src^) in [#0, #13, #10]) do
                  inc(Src);
              end else
                next;
        {.$ENDIF}
        // text ''
        '''': Skip('''');
        // text ""
        '"' : Skip('"');
        // Unnamed Input
        '?' : begin
                AddFieldA('');
                Next;
              end;
        '[' : Skip(']');
        // Named Input
        ':' : begin
                inc(dest);
                Result[dest] := '?';
                inc(Src);
                idlen := 0;
                // quoted identifiers
                if Src[idlen] = '"' then
                begin
                  inc(Src);
                  while true do
                    if (AnsiChar(Src[idlen]) in [#0, '"']) then
                      Break else
                      inc(idlen);
                end else
                // unquoted identifiers
                  while (AnsiChar(Src[idlen]) in Identifiers) do inc(idlen);
                AddFieldA(ALCopyStr(Src, 1, idlen));
                inc(Src, idlen);
                if Src^ = '"' then inc(Src);
              end;
        // skip everything when begin identifier found !
        // in procedures
        'b','B':
          begin
            if not ((dest > 0) and (AnsiChar(src[-1])
              in Identifiers)) and ({$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrIComp(PAnsiChar(AlCopyStr(Src, 0, 5)), 'begin') = 0) and
                not (AnsiChar(Src[5]) in Identifiers) then
                  while (Src^ <> #0) do Next else next;
          end;
        // declare should also stop param parsing, as a declare cursor statement
        // may contain variables.
        'd','D':
          begin
            if not ((dest > 0) and (AnsiChar(src[-1])
              in Identifiers)) and ({$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrIComp(PAnsiChar(AlCopyStr(Src, 0, 7)), 'declare') = 0) and
                not (AnsiChar(Src[7]) in Identifiers) then
                  while (Src^ <> #0) do Next else next;
          end;
      else
        next;
      end;
  end;

  function TALFBXSQLParams.GetFieldType(const Index: Word): TALFBXFieldType;
  begin
    if IsNull[Index] and (pfNotInitialized in FXSQLDA.sqlvar[Index].Flags) then
      Result := uftUnKnown else
      Result := inherited GetFieldType(Index);
  end;

  function TALFBXSQLParams.GetIsNullable(const Index: Word): boolean;
  begin
    CheckRange(Index);
    Result := not(pfNotNullable in FXSQLDA.sqlvar[Index].Flags);
  end;

  function TALFBXSQLParams.GetMaxSqlLen(const Index: Word): SmallInt;
  begin
    CheckRange(Index);
    Result := FXSQLDA.sqlvar[Index].MaxSqlLen
  end;

  function TALFBXSQLParams.GetFieldIndex(const name: AnsiString): Word;
  begin
    if not TryGetFieldIndex(name, Result) then
      raise Exception.CreateFmt(cALFBX_PARAMSTRNOTFOUND, [name]);
  end;

  function TALFBXSQLParams.TryGetFieldIndex(const name: AnsiString; out Index: Word): Boolean;
  var
    Field: Integer;
  begin
    for Field := 0 to FXSQLDA.sqln - 1 do
      if FXSQLDA.sqlvar[Field].ParamNameLength = Length(name) then
        if {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrLIComp(@FXSQLDA.sqlvar[Field].ParamName, PAnsiChar(Name),
          FXSQLDA.sqlvar[Field].ParamNameLength) = 0 then
          begin
            Result := true;
            Index  := Field;
            Exit;
          end;
    Result := False;
  end;

  function TALFBXSQLParams.AddFieldA(const Name: AnsiString): Word;
  var
    num: Word;
    len: Cardinal;
    p: PALFBXSQLVar;
  begin
    len := Length(Name);
    if len > cALFBXMaxParamLength then
      raise Exception.CreateFmt(cALFBX_SIZENAME, [Name]);

    Result := FXSQLDA.sqln;
    if (len > 0) and TryGetFieldIndex(Name, num) then
    begin
      inc(FXSQLDA.sqln);
      inc(FXSQLDA.sqld);
      ReallocMem(FXSQLDA, XSQLDA_LENGTH(FXSQLDA.sqln));
      ALMove(FXSQLDA.sqlvar[num], FXSQLDA.sqlvar[Result], SizeOf(TALFBXSQLVar));
    end else
    begin
      inc(FXSQLDA.sqln);
      inc(FXSQLDA.sqld);
      ReallocMem(FXSQLDA, XSQLDA_LENGTH(FXSQLDA.sqln));
      inc(FParamCount);
      p := @FXSQLDA.sqlvar[Result];
      Include(p^.Flags, pfNotInitialized);
      p^.ID := FParamCount;
      p^.MaxSqlLen := 0;
      p^.ParamNameLength := len;
      if p^.ParamNameLength > 0 then
        ALMove(PAnsiChar(Name)^, p^.ParamName[0], p^.ParamNameLength);

      {FB25_UP}
      p^.sqltype    := SQL_NULL + 1; // !! p^.sqltype    := SQL_TEXT + 1; for FB < 2.5 !!
      {FB25_UP}

      p^.sqlscale   := 0;
      p^.sqlsubtype := 0;
      p^.sqllen     := 0;
      p^.sqldata    := nil;
      GetMem(p^.sqlind, 2); // Can be NULL
      p^.sqlind^ := -1; // NULL
    end;
  end;

//  function TALFBXSQLParams.AddFieldW(const name: UnicodeString): Word;
//  begin
//    Result := AddFieldA(AnsiString(name));
//  end;

//  function TALFBXSQLParams.AddField(const name: string): Word;
//  begin
//{$IFDEF UNICODE}
//    Result := AddFieldW(name);
//{$ELSE}
//    Result := AddFieldA(name);
//{$ENDIF}
//  end;

  procedure TALFBXSQLParams.Clear;
  var i, j: Smallint;
  begin
    for i := 0 to FXSQLDA.sqln - 1 do
    begin
      if (FXSQLDA.sqlvar[i].sqlind <> nil) then
        freemem(FXSQLDA.sqlvar[i].sqlind);
        if FXSQLDA.sqlvar[i].sqldata <> nil then
          freemem(FXSQLDA.sqlvar[i].sqldata);
        // don't free shared pointers
        for j := i + 1 to FXSQLDA.sqln - 1 do
          if (FXSQLDA.sqlvar[i].ID = FXSQLDA.sqlvar[j].ID) then
            begin
              FXSQLDA.sqlvar[j].sqldata := nil;
              FXSQLDA.sqlvar[j].sqlind  := nil;
            end;
    end;
    FXSQLDA.sqln := 0;
    FXSQLDA.sqld := 0;
    ReallocMem(FXSQLDA, XSQLDA_LENGTH(0));
    FParamCount := 0;
  end;

 // TALFBXSQLParams.SetAs...

  procedure TALFBXSQLParams.SetAsQuad(const Index: Word; const Value: TISCQuad);
  begin
    SetFieldType(Index, sizeof(TISCQuad), SQL_QUAD + 1, 0);
    inherited;
  end;

//  procedure TALFBXSQLParams.SetAsRawByteString(const Index: Word; const Value: RawByteString);
//  begin
//    SetFieldType(Index, Length(Value), SQL_TEXT + 1, 0);
//    inherited;
//  end;

  procedure TALFBXSQLParams.SetAsDateTime(const Index: Word;
    const Value: TDateTime);
  begin
    SetFieldType(Index, sizeof(TISCQuad), SQL_TIMESTAMP + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsDate(const Index: Word; const Value: Integer);
  begin
    SetFieldType(Index, sizeof(Integer), SQL_TYPE_DATE + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsTime(const Index: Word; const Value: Cardinal);
  begin
    SetFieldType(Index, sizeof(Cardinal), SQL_TYPE_TIME + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsBoolean(const Index: Word; const Value: Boolean);
  begin
    SetFieldType(Index, sizeof(Smallint), SQL_SHORT + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsInteger(const Index: Word; const Value: Integer);
  begin
    SetFieldType(Index, sizeof(Integer), SQL_LONG + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsSingle(const Index: Word; const Value: Single);
  begin
    SetFieldType(Index, sizeof(Single), SQL_FLOAT + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsSmallint(const Index: Word; const Value: Smallint);
  begin
    SetFieldType(Index, sizeof(Smallint), SQL_SHORT + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsAnsiString(const Index: Word; const Value: AnsiString);
  begin
    SetFieldType(Index, Length(Value), SQL_TEXT + 1, 0);
    inherited;
  end;

//  procedure TALFBXSQLParams.SetAsUnicodeString(const Index: Word;
//    const Value: UnicodeString);
//  begin
//    SetFieldType(Index, Length(Value) * 2, SQL_TEXT + 1, 0);
//    inherited;
//  end;

  procedure TALFBXSQLParams.SetAsInt64(const Index: Word; const Value: Int64);
  begin
    SetFieldType(Index, sizeof(Int64), SQL_INT64 + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsDouble(const Index: Word; const Value: Double);
  begin
    SetFieldType(Index, sizeof(double), SQL_DOUBLE + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsCurrency(const Index: Word;
    const Value: Currency);
  begin
    SetFieldType(Index, sizeof(Int64), SQL_INT64 + 1, -4);
    inherited;
  end;

end.
