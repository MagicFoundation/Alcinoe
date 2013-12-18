(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(* Contributors:                                                                *)
(*     Olivier Guilbaud <oguilb@free.fr>                                        *)
(*     Volkan Ceylan <volkance@hotmail.com>                                     *)
(*                                                                              *)
(********************************************************************************)

unit uiblib;

{$I uib.inc}
{$IFNDEF CPU64}
  {$ALIGN ON}
{$ENDIF}
  {$MINENUMSIZE 4}

interface
uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
{$IFDEF DELPHI14_UP}
  rtti, typinfo,
{$ENDIF}
  {$IFDEF FPC}
  Variants,
  {$ENDIF FPC}
  uibase, uiberror, Classes, SysUtils;

type

{$IFNDEF UNICODE}
{$IFNDEF FPC}
  UnicodeString = WideString;
{$ENDIF}
  RawByteString = AnsiString;
{$ENDIF}

  TUIBFieldType = (uftUnKnown, uftNumeric, uftChar, uftVarchar, uftCstring, uftSmallint,
    uftInteger, uftQuad, uftFloat, uftDoublePrecision, uftTimestamp, uftBlob, uftBlobId,
    uftDate, uftTime, uftInt64, uftArray {$IFDEF IB7_UP}, uftBoolean{$ENDIF}
    {$IFDEF FB25_UP}, uftNull{$ENDIF});

  TScale = 1..15;

//******************************************************************************
// Errors handling
//******************************************************************************
  EUIBConvertError = class(Exception);

  EUIBError = class(Exception)
  private
    FGDSCode: Integer;
    FErrorCode: Integer;
    FSQLCode: Integer;
{$IFDEF FB25_UP}
    FSQLState: RawByteString;
{$ENDIF}
  public
    property ErrorCode: Integer read FErrorCode;
    property SQLCode: Integer read FSQLCode;
    property GDSCode: Integer read FGDSCode;
{$IFDEF FB25_UP}
    property SQLState: RawByteString read FSQLState;
{$ENDIF}
  end;

  EUIBException = class(EUIBError)
  private
    FNumber: Integer;
  public
    property Number: Integer read FNumber;
  end;

  EUIBGFixError    = class(EUIBError);
  EUIBDSQLError    = class(EUIBError);
  EUIBDynError     = class(EUIBError);
  EUIBGBakError    = class(EUIBError);
  EUIBGSecError    = class(EUIBError);
  EUIBLicenseError = class(EUIBError);
  EUIBGStatError   = class(EUIBError);


  EUIBExceptionClass = class of EUIBError;


const
  QuadNull: TISCQuad = (gds_quad_high: 0; gds_quad_low: 0);

{$IFDEF GUID_TYPE}
const
  GUIDNull: TGUID = (D1:0;D2:0;D3:0;D4:(0,0,0,0,0,0,0,0));
  GUIDUndefined: TGUID = (D1:$FFFFFFFF;D2:$FFFF;D3:$FFFF;D4:($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF));
{$ENDIF}

//******************************************************************************
// Database
//******************************************************************************

type
  TCharacterSet = (csNONE, csASCII, csBIG_5, csCYRL, csDOS437, csDOS850,
  csDOS852, csDOS857, csDOS860, csDOS861, csDOS863, csDOS865, csEUCJ_0208,
  csGB_2312, csISO8859_1, csISO8859_2, csKSC_5601, csNEXT, csOCTETS, csSJIS_0208,
  csUNICODE_FSS, csUTF8, csWIN1250, csWIN1251, csWIN1252, csWIN1253, csWIN1254
{$IFDEF FB15_UP}
  ,csDOS737, csDOS775, csDOS858, csDOS862, csDOS864, csDOS866, csDOS869, csWIN1255,
  csWIN1256, csWIN1257, csISO8859_3, csISO8859_4, csISO8859_5, csISO8859_6, csISO8859_7,
  csISO8859_8, csISO8859_9, csISO8859_13
{$ENDIF}
{$IFDEF IB71_UP}
  ,csISO8859_15 ,csKOI8R
{$ENDIF}
{$IFDEF FB20_UP}
  ,csKOI8R, csKOI8U
{$ENDIF}
{$IFDEF FB21_UP}
  ,csWIN1258
  ,csTIS620
  ,csGBK
  ,csCP943C
{$ENDIF}
  );

  // Transaction parameters
  TTransParam = (
    { prevents a transaction from accessing tables if they are written to by
      other transactions.}
    tpConsistency,
    { allows concurrent transactions to read and write shared data. }
    tpConcurrency,
    { Concurrent, shared access of a specified table among all transactions. }
  {$IFNDEF FB_21UP}
    tpShared,
    { Concurrent, restricted access of a specified table. }
    tpProtected,
    tpExclusive,
  {$ENDIF}
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
  {$IFDEF FB20_UP}
    ,tpLockTimeout
  {$ENDIF}
  );

  { Set of transaction parameters. }
  TTransParams = set of TTransParam;

const
  CharacterSetStr : array[TCharacterSet] of AnsiString = (
    'NONE', 'ASCII', 'BIG_5', 'CYRL', 'DOS437', 'DOS850', 'DOS852', 'DOS857',
    'DOS860', 'DOS861', 'DOS863', 'DOS865', 'EUCJ_0208', 'GB_2312', 'ISO8859_1',
    'ISO8859_2', 'KSC_5601', 'NEXT', 'OCTETS', 'SJIS_0208', 'UNICODE_FSS', 'UTF8',
    'WIN1250', 'WIN1251', 'WIN1252', 'WIN1253', 'WIN1254'
{$IFDEF FB15_UP}
    ,'DOS737', 'DOS775', 'DOS858', 'DOS862', 'DOS864', 'DOS866', 'DOS869',
    'WIN1255', 'WIN1256', 'WIN1257', 'ISO8859_3', 'ISO8859_4', 'ISO8859_5',
    'ISO8859_6', 'ISO8859_7', 'ISO8859_8', 'ISO8859_9', 'ISO8859_13'
{$ENDIF}
{$IFDEF IB71_UP}
    ,'ISO8859_15', 'KOI8R'
{$ENDIF}
{$IFDEF FB20_UP}
    ,'KOI8R', 'KOI8U'
{$ENDIF}
{$IFDEF FB21_UP}
    ,'WIN1258'
    ,'TIS620'
    ,'GBK'
    ,'CP943C'
{$ENDIF}
    );

  CharacterSetCP: array[TCharacterSet] of Word =
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
{$IFDEF FB15_UP}
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
{$ENDIF}
{$IFDEF IB71_UP}
  ,28605 // csISO8859_15 iso-8859-15	ISO 8859-15 Latin 9
  ,20866 // csKOI8R koi8-r	Russian (KOI8-R); Cyrillic (KOI8-R)
{$ENDIF}
{$IFDEF FB20_UP}
  ,20866 // csKOI8R koi8-r	Russian (KOI8-R); Cyrillic (KOI8-R)
  ,21866 //csKOI8U koi8-u	Ukrainian (KOI8-U); Cyrillic (KOI8-U)
{$ENDIF}
{$IFDEF FB21_UP}
  ,1258 //csWIN1258 ANSI/OEM Vietnamese; Vietnamese (Windows)
  ,874 //csTIS620 windows-874	ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows)
  ,936 //gb2312	ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)
  ,932 //csCP943C Shift_JIS
{$ENDIF}
  );

  BytesPerCharacter: array[TCharacterSet] of Byte =
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
{$IFDEF FB20_UP}
    4,  // UTF8
{$ELSE}
    3,  // UTF8 ALIAS UNICODE_FSS
{$ENDIF}
    1, // WIN1250
    1, // WIN1251
    1, // WIN1252
    1, // WIN1253
    1  // WIN1254
{$IFDEF FB15_UP}
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
{$ENDIF}
{$IFDEF IB71_UP}
   ,1  // ISO8859_15
   ,1  // KOI8R
{$ENDIF}
{$IFDEF FB20_UP}
   ,1  // KOI8R
   ,1  // KOI8U
{$ENDIF}
{$IFDEF FB21_UP}
   ,1  // WIN1258
   ,1  // TIS620
   ,2  // GBK
   ,2  // CP943C
{$ENDIF}
  );

{$IFDEF DLLREGISTRY}
  FBINSTANCES = 'SOFTWARE\Firebird Project\Firebird Server\Instances';
{$ENDIF}

  function GetSystemCharacterset: TCharacterSet;

  function MBUEncode(const str: UnicodeString; cp: Word): RawByteString;
  function MBUDecode(const str: RawByteString; cp: Word): UnicodeString; overload;
  procedure MBUDecode(str: PAnsiChar; size: Integer; cp: Word; buffer: PWideChar); overload;

  function StrToCharacterSet(const CharacterSet: RawByteString): TCharacterSet;
  function CreateDBParams(Params: AnsiString; Delimiter: AnsiChar = ';'): AnsiString;

  function CreateTRParams(Options: TTransParams; const LockRead: string = ''; const LockWrite: string = ''{$IFDEF FB20_UP}; LockTimeout: Word = 0{$ENDIF}): RawByteString;
  function GetClientLibrary: string;
  function CreateBlobParams(Params: AnsiString; Delimiter: AnsiChar = ';'): AnsiString;

//******************************************************************************
// Transaction
//******************************************************************************

const
  // Default Transaction Parameter
  TPBDefault = isc_tpb_version3 + isc_tpb_write + isc_tpb_concurrency + isc_tpb_wait;

//******************************************************************************
//  DSQL
//******************************************************************************

  //****************************************
  // TSQLDA
  //****************************************

const
{$IFDEF IB7_UP}
  MaxParamLength = 274;
{$ELSE}
{$IFDEF CPUX86_64}
  MaxParamLength = 130;
{$ELSE}
  MaxParamLength = 125;
{$ENDIF}
{$ENDIF}

type

  TUIBParamsFlag = (pfNotInitialized, pfNotNullable);
  TUIBParamsFlags = set of TUIBParamsFlag;
  PUIBSQLVar = ^TUIBSQLVar;
  TUIBSQLVar = record // size must be 152
    SqlType      : Smallint;
    SqlScale     : Smallint;
{$IFDEF IB7_UP}
    SqlPrecision : Smallint;
{$ENDIF}
    SqlSubType   : Smallint;
    SqlLen       : Smallint;
    SqlData      : PAnsiChar;
    SqlInd       : PSmallint;
    case byte of
    // TSQLResult
    0 : ( SqlNameLength   : Smallint;
          SqlName         : array[0..METADATALENGTH-1] of AnsiChar;
          RelNameLength   : Smallint;
          RelName         : array[0..METADATALENGTH-1] of AnsiChar;
          OwnNameLength   : Smallint;
          OwnName         : array[0..METADATALENGTH-1] of AnsiChar;
          AliasNameLength : Smallint;
          AliasName       : array[0..METADATALENGTH-1] of AnsiChar;
          );
    // TSQLParam
    1 : ( Flags           : TUIBParamsFlags;
          ID              : Word;
          MaxSqlLen       : Smallint;
          ParamNameLength : Smallint;
          ParamName       : array[0..MaxParamLength-1] of AnsiChar;
          );
  end;

  PUIBSQLDa = ^TUIBSQLDa;
  TUIBSQLDa = record
    version : Smallint;                // version of this XSQLDA
    sqldaid : array[0..7] of AnsiChar; // XSQLDA name field          ->  RESERVED
    sqldabc : ISCLong;                 // length in bytes of SQLDA   ->  RESERVED
    sqln    : Smallint;                // number of fields allocated
    sqld    : Smallint;                // actual number of fields
    sqlvar: array[Word] of TUIBSQLVar; // first field address
  end;

  TUIBStatementType = (
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
  {$IFDEF FB15_UP}
    ,stSavePoint          //  user_savepoint | undo_savepoint       SAVEPOINT | ROLLBACK [WORK] TO
  {$ENDIF}
  );

(******************************************************************************)
(* Abstract Class                                                             *)
(******************************************************************************)

type
  TSQLDA = class
  private
    FXSQLDA: PUIBSQLDa;
    FCharacterSet: TCharacterSet;
    function GetAllocatedFields: Word;
    procedure SetAllocatedFields(Fields: Word);
    function GetFieldCount: Integer;
    function GetSQLType(const Index: Word): Smallint;
    function GetSQLLen(const Index: Word): Smallint;
    function GetSQLScale(const Index: Word): Smallint;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Int64); overload;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Double); overload;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Integer); overload;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Single); overload;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Smallint); overload;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: TDateTime); overload;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Currency); overload;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: boolean); overload;
    procedure ConvertString(const Code: Smallint; Index: Word; out value: Cardinal); overload;
    procedure ConvertStringToDate(const Code: Smallint; Index: Word; out value: Integer);
    procedure DecodeStringB(const Code: Smallint; Index: Word; out Str: RawByteString);
    procedure DecodeStringW(const Code: Smallint; Index: Word; out Str: UnicodeString);
    procedure DecodeStringA(const Code: Smallint; Index: Word; out Str: AnsiString);
    procedure DecodeString(const Code: Smallint; Index: Word; out Str: string); overload;
    function DecodeString(const Code: Smallint; Index: Word): string; overload;
    procedure EncodeStringA(Code: Smallint; Index: Word; const str: AnsiString);
    procedure EncodeStringW(Code: Smallint; Index: Word; const str: UnicodeString);
    procedure EncodeStringB(Code: Smallint; Index: Word; const str: RawByteString);
    procedure EncodeString(Code: Smallint; Index: Word; const str: string);
    function GetAsString(const Index: Word): string;
    procedure SetAsString(const Index: Word; const Value: string);
    function GetByNameAsString(const name: string): string;
    procedure SetByNameAsString(const name, Value: string);
    function GetByNameAsRawByteString(const name: string): RawByteString;
    procedure SetByNameAsRawByteString(const name: string;
      const Value: RawByteString);
  {$IFDEF GUID_TYPE}
    procedure EncodeGUID(Code: Smallint; Index: Word; const G: TGUID);
  {$ENDIF}
  protected
    function GetSqlName(const Index: Word): string;
    function GetRelName(const Index: Word): string;
    function GetOwnName(const Index: Word): string;
    function GetAliasName(const Index: Word): string;

    function GetFieldType(const Index: Word): TUIBFieldType; virtual;

    function GetIsNumeric(const Index: Word): boolean;
    function GetIsBlob(const Index: Word): boolean;
    function GetIsBlobText(const Index: Word): boolean;
    function GetIsArray(const Index: Word): boolean;
    function GetIsNullable(const Index: Word): boolean; virtual;

    function GetIsNull(const Index: Word): boolean;
    function GetAsDouble(const Index: Word): Double;
    function GetAsCurrency(const Index: Word): Currency;
    function GetAsInt64(const Index: Word): Int64;
    function GetAsInteger(const Index: Word): Integer;
    function GetAsSingle(const Index: Word): Single;
    function GetAsSmallint(const Index: Word): Smallint;
    function GetAsRawByteString(const Index: Word): RawByteString; virtual;
    function GetAsAnsiString(const Index: Word): AnsiString; virtual;
    function GetAsUnicodeString(const Index: Word): UnicodeString; virtual;
    function GetAsQuad(const Index: Word): TISCQuad;
    function GetAsVariant(const Index: Word): Variant; virtual;
    function GetAsDateTime(const Index: Word): TDateTime;
    function GetAsDate(const Index: Word): Integer;
    function GetAsTime(const Index: Word): Cardinal;
    function GetAsBoolean(const Index: Word): boolean;
{$IFDEF DELPHI14_UP}
    function GetAsTValue(const Index: Word): TValue;
{$ENDIF}
  {$IFDEF GUID_TYPE}
    function GetAsGUID(const Index: Word): TGUID;
  {$ENDIF}

    procedure SetIsNull(const Index: Word; const Value: boolean); virtual;
    procedure SetAsDouble(const Index: Word; const Value: Double); virtual;
    procedure SetAsCurrency(const Index: Word; const Value: Currency); virtual;
    procedure SetAsInt64(const Index: Word; const Value: Int64); virtual;
    procedure SetAsInteger(const Index: Word; const Value: Integer); virtual;
    procedure SetAsSingle(const Index: Word; const Value: Single); virtual;
    procedure SetAsSmallint(const Index: Word; const Value: Smallint); virtual;
    procedure SetAsRawByteString(const Index: Word; const Value: RawByteString); virtual;
    procedure SetAsAnsiString(const Index: Word; const Value: AnsiString); virtual;
    procedure SetAsUnicodeString(const Index: Word; const Value: UnicodeString); virtual;
    procedure SetAsQuad(const Index: Word; const Value: TISCQuad); virtual;
    procedure SetAsDateTime(const Index: Word; const Value: TDateTime); virtual;
    procedure SetAsBoolean(const Index: Word; const Value: Boolean); virtual;
    procedure SetAsDate(const Index: Word; const Value: Integer); virtual;
    procedure SetAsTime(const Index: Word; const Value: Cardinal); virtual;
    procedure SetAsVariant(const Index: Word; const Value: Variant); virtual;
  {$IFDEF GUID_TYPE}
    procedure SetAsGUID(const Index: Word; const Value: TGUID); virtual;
  {$ENDIF}

    function GetByNameIsNumeric(const Name: string): boolean;
    function GetByNameIsBlob(const Name: string): boolean;
    function GetByNameIsBlobText(const Name: string): boolean;
    function GetByNameIsNull(const Name: string): boolean;
    function GetByNameIsNullable(const Name: string): boolean;

    function GetByNameAsDouble(const Name: string): Double;
    function GetByNameAsCurrency(const Name: string): Currency;
    function GetByNameAsInt64(const Name: string): Int64;
    function GetByNameAsInteger(const Name: string): Integer;
    function GetByNameAsSingle(const Name: string): Single;
    function GetByNameAsSmallint(const Name: string): Smallint;
    function GetByNameAsAnsiString(const Name: string): AnsiString;
    function GetByNameAsUnicodeString(const Name: string): UnicodeString;
    function GetByNameAsQuad(const Name: string): TISCQuad;
    function GetByNameAsVariant(const Name: string): Variant;
    function GetByNameAsDateTime(const Name: string): TDateTime;
    function GetByNameAsBoolean(const Name: string): boolean;
    function GetByNameAsDate(const Name: string): Integer;
    function GetByNameAsTime(const Name: string): Cardinal;
  {$IFDEF DELPHI14_UP}
    function GetByNameAsTValue(const Name: string): TValue;
  {$ENDIF}

  {$IFDEF GUID_TYPE}
    function GetByNameAsGUID(const Name: string): TGUID;
  {$ENDIF}

    procedure SetByNameIsNull(const Name: string; const Value: boolean);
    procedure SetByNameAsDouble(const Name: string; const Value: Double);
    procedure SetByNameAsCurrency(const Name: string; const Value: Currency);
    procedure SetByNameAsInt64(const Name: string; const Value: Int64);
    procedure SetByNameAsInteger(const Name: string; const Value: Integer);
    procedure SetByNameAsSingle(const Name: string; const Value: Single);
    procedure SetByNameAsSmallint(const Name: string; const Value: Smallint);
    procedure SetByNameAsAnsiString(const Name: string; const Value: AnsiString);
    procedure SetByNameAsUnicodeString(const Name: string; const Value: UnicodeString);
    procedure SetByNameAsQuad(const Name: string; const Value: TISCQuad);
    procedure SetByNameAsDateTime(const Name: string; const Value: TDateTime);
    procedure SetByNameAsBoolean(const Name: string; const Value: boolean);
    procedure SetByNameAsDate(const Name: string; const Value: Integer);
    procedure SetByNameAsVariant(const Name: string; const Value: Variant);
  {$IFDEF GUID_TYPE}
    procedure SetByNameAsGUID(const Name: string; const Value: TGUID);
  {$ENDIF}
  public
    constructor Create(aCharacterSet: TCharacterSet); virtual;
    procedure CheckRange(const Index: Word);
    function GetFieldIndex(const name: AnsiString): Word; virtual;
    function TryGetFieldIndex(const name: AnsiString; out index: Word): Boolean; virtual;
{$IFDEF DELPHI14_UP}
    function GetAsType<T>(var ctx: TRttiContext): T;
{$ENDIF}
    property Data: PUIBSQLDa read FXSQLDA;
    property IsBlob[const Index: Word]: boolean read GetIsBlob;
    property IsBlobText[const Index: Word]: boolean read GetIsBlobText;
    property IsNullable[const Index: Word]: boolean read GetIsNullable;
    property IsNumeric[const Index: Word]: boolean read GetIsNumeric;

    property FieldCount: Integer read GetFieldCount;
    property SQLType[const Index: Word]: Smallint read GetSQLType;
    property SQLLen[const Index: Word]: Smallint read GetSQLLen;
    property SQLScale[const Index: Word]: Smallint read GetSQLScale;
    property FieldType[const Index: Word]: TUIBFieldType read GetFieldType;

    property CharacterSet: TCharacterSet read FCharacterSet write FCharacterSet;

    property IsNull       [const Index: Word]: boolean    read GetIsNull       write SetIsNull;
    property AsSmallint   [const Index: Word]: Smallint   read GetAsSmallint   write SetAsSmallint;
    property AsInteger    [const Index: Word]: Integer    read GetAsInteger    write SetAsInteger;
    property AsSingle     [const Index: Word]: Single     read GetAsSingle     write SetAsSingle;
    property AsDouble     [const Index: Word]: Double     read GetAsDouble     write SetAsDouble;
    property AsCurrency   [const Index: Word]: Currency   read GetAsCurrency   write SetAsCurrency;
    property AsInt64      [const Index: Word]: Int64      read GetAsInt64      write SetAsInt64;
    property AsString     [const Index: Word]: string     read GetAsString write SetAsString;
    property AsRawByteString[const Index: Word]: RawByteString  read GetAsRawByteString write SetAsRawByteString;
    property AsAnsiString [const Index: Word]: AnsiString  read GetAsAnsiString     write SetAsAnsiString;
    property AsUnicodeString [const Index: Word]: UnicodeString read GetAsUnicodeString write SetAsUnicodeString;
    property AsQuad       [const Index: Word]: TISCQuad   read GetAsQuad       write SetAsQuad;
    property AsDateTime   [const Index: Word]: TDateTime  read GetAsDateTime   write SetAsDateTime;
    property AsBoolean    [const Index: Word]: Boolean    read GetAsBoolean    write SetAsBoolean;
    property AsDate       [const Index: Word]: Integer    read GetAsDate       write SetAsDate;
    property AsTime       [const Index: Word]: Cardinal   read GetAsTime       write SetAsTime;
    property AsVariant    [const Index: Word]: Variant    read GetAsVariant    write SetAsVariant;
  {$IFDEF DELPHI14_UP}
    property AsTValue     [const Index: Word]: TValue     read GetAsTValue;
  {$ENDIF}
  {$IFDEF GUID_TYPE}
    property AsGUID       [const Index: Word]: TGUID      read GetAsGUID       write SetAsGUID;
  {$ENDIF}

    property ByNameIsBlob       [const name: string]: boolean    read GetByNameIsBlob;
    property ByNameIsBlobText   [const name: string]: boolean    read GetByNameIsBlobText;
    property ByNameIsNull       [const name: string]: boolean    read GetByNameIsNull       write SetByNameIsNull;
    property ByNameAsSmallint   [const name: string]: Smallint   read GetByNameAsSmallint   write SetByNameAsSmallint;
    property ByNameAsInteger    [const name: string]: Integer    read GetByNameAsInteger    write SetByNameAsInteger;
    property ByNameAsSingle     [const name: string]: Single     read GetByNameAsSingle     write SetByNameAsSingle;
    property ByNameAsDouble     [const name: string]: Double     read GetByNameAsDouble     write SetByNameAsDouble;
    property ByNameAsCurrency   [const name: string]: Currency   read GetByNameAsCurrency   write SetByNameAsCurrency;
    property ByNameAsInt64      [const name: string]: Int64      read GetByNameAsInt64      write SetByNameAsInt64;
    property ByNameAsString     [const name: string]: string     read GetByNameAsString     write SetByNameAsString;
    property ByNameAsAnsiString     [const name: string]: AnsiString     read GetByNameAsAnsiString     write SetByNameAsAnsiString;
    property ByNameAsRawByteString  [const name: string]: RawByteString     read GetByNameAsRawByteString     write SetByNameAsRawByteString;
    property ByNameAsUnicodeString [const name: string]: UnicodeString read GetByNameAsUnicodeString write SetByNameAsUnicodeString;
    property ByNameAsQuad       [const name: string]: TISCQuad   read GetByNameAsQuad       write SetByNameAsQuad;
    property ByNameAsVariant    [const name: string]: Variant    read GetByNameAsVariant    write SetByNameAsVariant;
    property ByNameAsDateTime   [const name: string]: TDateTime  read GetByNameAsDateTime   write SetByNameAsDateTime;
    property ByNameAsBoolean    [const name: string]: Boolean    read GetByNameAsBoolean    write SetByNameAsBoolean;
    property ByNameAsDate       [const name: string]: Integer    read GetByNameAsDate       write SetByNameAsDate;
  {$IFDEF GUID_TYPE}
    property ByNameAsGUID       [const name: string]: TGUID      read GetByNameAsGUID       write SetByNameAsGUID;
  {$ENDIF}
  end;
{ TPoolStream }

  PPtrArray = ^TPtrArray;
  TPtrArray = array[0..(MAXLONGINT div SizeOf(Pointer)) - 1] of Pointer;

  TPoolStream = class(TStream)
  private
    FItemsInPage: Integer;
    FItemSize: Integer;
    FItemCount: Integer;
    FPageSize: Integer;
    FInternalPageSize: Integer;
    FPages: PPtrArray;
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
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    property ItemsInPage: Integer read FItemsInPage;
    property ItemSize: Integer read FItemSize;
    property PageSize: Integer read FPageSize;
    property ItemCount: Integer read FItemCount;
    property Items[index: integer]: Pointer read Get; default;
  end;

(******************************************************************************)
(* SQL Result set                                                             *)
(******************************************************************************)

  PBlobData = ^TBlobData;
  TBlobData = packed record
    Size: Integer;
    Buffer: Pointer;
  end;

  PArrayDesc = ^TArrayDesc;
{$IFDEF IB7_UP}
  TArrayDesc = TISCArrayDescV2;
  TBlobDesc = TISCBlobDescV2;
{$ELSE}
  TArrayDesc = TISCArrayDesc;
  TBlobDesc = TISCBlobDesc;
{$ENDIF}

  PArrayInfo = ^TArrayInfo;
  TArrayInfo = record
    index: Integer;
    size: integer;
    info: TArrayDesc;
  end;

  TSQLResult = class(TSQLDA)
  private
    FRecordPool: TPoolStream;
    FCachedFetch: boolean;
    FFetchBlobs: boolean;
    FDataBuffer: Pointer;
    FDataBufferLength: PtrInt;
    FBlobsIndex: array of Word;
    FCurrentRecord: Integer;
    FBufferChunks: Cardinal;
    FScrollEOF: boolean;
    FInMemoryEOF: boolean;
    FArrayInfos: array of TArrayInfo;
    FStatBlobsSize: Int64;
    procedure AddCurrentRecord;
    procedure FreeBlobs(Buffer: Pointer);
    function GetRecordCount: Integer;
    function GetCurrentRecord: Integer;
    procedure AllocateDataBuffer;
    function GetEof: boolean;
    function GetUniqueRelationName: string;
    function GetBof: boolean;
    function GetDataQuadOffset(const index: word): Pointer;
    function GetBlobData(const index: word): PBlobData;
    function GetArrayData(const index: word): Pointer;
    function GetArrayCount: Word;
    function GetArrayInfos(const index: word): PArrayInfo;
  protected
    function GetAsRawByteString(const Index: Word): RawByteString; override;
    function GetAsAnsiString(const Index: Word): AnsiString; override;
    function GetAsUnicodeString(const Index: Word): UnicodeString; override;
    function GetAsVariant(const Index: Word): Variant; override;
  public
    constructor Create(Charset: TCharacterSet; Fields: SmallInt = 0;
      CachedFetch: Boolean = False;
      FetchBlobs: boolean = false;
      BufferChunks: Cardinal = 1000); reintroduce;
    destructor Destroy; override;
    procedure ClearRecords;
    procedure GetRecord(const Index: Integer);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure Next;

    property BlobData[const index: word]: PBlobData read GetBlobData;

    property ArrayData[const index: word]: Pointer read GetArrayData;
    property ArrayInfos[const index: word]: PArrayInfo read GetArrayInfos;
    property ArrayCount: Word read GetArrayCount;

    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    procedure ReadBlobB(const Index: Word; var data: RawByteString); overload;
    procedure ReadBlobA(const Index: Word; var str: AnsiString); overload;
    procedure ReadBlobW(const Index: Word; var str: UnicodeString); overload;
    procedure ReadBlob(const Index: Word; var str: string); overload;
    function ReadBlobB(const Index: Word): RawByteString; overload;
    function ReadBlobA(const Index: Word): AnsiString; overload;
    function ReadBlobW(const Index: Word): UnicodeString; overload;
    function ReadBlob(const Index: Word): string; overload;
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    procedure ReadBlob(const Index: Word; Data: Pointer); overload;
    procedure ReadBlob(const name: string; Stream: TStream); overload;
    procedure ReadBlobB(const name: string; var data: RawByteString); overload;
    procedure ReadBlobA(const name: string; var str: AnsiString); overload;
    procedure ReadBlobW(const name: string; var str: UnicodeString); overload;
    procedure ReadBlob(const name: string; var str: string); overload;
    function ReadBlobB(const name: string): RawByteString; overload;
    function ReadBlobA(const name: string): AnsiString; overload;
    function ReadBlobW(const name: string): UnicodeString; overload;
    function ReadBlob(const name: string): string; overload;
    procedure ReadBlob(const name: string; var Value: Variant); overload;
    procedure ReadBlob(const name: string; Data: Pointer); overload;

    function GetBlobSize(const Index: Word): Cardinal;

    property Eof: boolean read GetEof;
    property ScrollEOF: boolean read FScrollEOF;
    property Bof: boolean read GetBof;

    property CachedFetch: boolean read FCachedFetch;
    property FetchBlobs: boolean read FFetchBlobs;
    property RecordCount: Integer read GetRecordCount;
    property CurrentRecord: Integer read GetCurrentRecord write GetRecord;
    property BufferChunks: Cardinal read FBufferChunks;
    property UniqueRelationName: string read GetUniqueRelationName;

    property SqlName[const Index: Word]: string read GetSqlName;
    property RelName[const Index: Word]: string read GetRelName;
    property OwnName[const Index: Word]: string read GetOwnName;
    property AliasName[const Index: Word]: string read GetAliasName;

    property AsSmallint   [const Index: Word]: Smallint   read GetAsSmallint;
    property AsInteger    [const Index: Word]: Integer    read GetAsInteger;
    property AsSingle     [const Index: Word]: Single     read GetAsSingle;
    property AsDouble     [const Index: Word]: Double     read GetAsDouble;
    property AsCurrency   [const Index: Word]: Currency   read GetAsCurrency;
    property AsInt64      [const Index: Word]: Int64      read GetAsInt64;
    property AsRawByteString [const Index: Word]: RawByteString read GetAsRawByteString;
    property AsAnsiString [const Index: Word]: AnsiString read GetAsAnsiString;
    property AsUnicodeString [const Index: Word]: UnicodeString read GetAsUnicodeString;
    property AsVariant    [const Index: Word]: Variant    read GetAsVariant;
    property AsDateTime   [const Index: Word]: TDateTime  read GetAsDateTime;
    property AsDate       [const Index: Word]: Integer    read GetAsDate;
    property AsTime       [const Index: Word]: Cardinal   read GetAsTime;
    property AsBoolean    [const Index: Word]: Boolean    read GetAsBoolean;

    property ByNameIsNull[const name: string]: boolean read GetByNameIsNull;
    property ByNameIsNullable[const name: string]: boolean read GetByNameIsNullable;

    property ByNameAsSmallint   [const name: string]: Smallint   read GetByNameAsSmallint;
    property ByNameAsInteger    [const name: string]: Integer    read GetByNameAsInteger;
    property ByNameAsSingle     [const name: string]: Single     read GetByNameAsSingle;
    property ByNameAsDouble     [const name: string]: Double     read GetByNameAsDouble;
    property ByNameAsCurrency   [const name: string]: Currency   read GetByNameAsCurrency;
    property ByNameAsInt64      [const name: string]: Int64      read GetByNameAsInt64;
    property ByNameAsAnsiString     [const name: string]: AnsiString     read GetByNameAsAnsiString;
    property ByNameAsUnicodeString [const name: string]: UnicodeString read GetByNameAsUnicodeString;
    property ByNameAsQuad       [const name: string]: TISCQuad   read GetByNameAsQuad;
    property ByNameAsVariant    [const name: string]: Variant    read GetByNameAsVariant;
    property ByNameAsDateTime   [const name: string]: TDateTime  read GetByNameAsDateTime;
    property ByNameAsBoolean    [const name: string]: Boolean    read GetByNameAsBoolean;
    property ByNameAsDate       [const name: string]: Integer    read GetByNameAsDate;
    property ByNameAsTime       [const name: string]: Cardinal   read GetByNameAsTime;

    property Values[const name: string]: Variant read GetByNameAsVariant; default;
  end;

  TSQLResultClass = class of TSQLResult;

(******************************************************************************)
(* SQL Params                                                                 *)
(******************************************************************************)

  TSQLParams = class(TSQLDA)
  private
    FParamCount: Word;
    function GetFieldName(const Index: Word): string;
    procedure AllocateDataBuffer(AInit: boolean = true);
    function GetMaxSqlLen(const Index: Word): SmallInt;
  protected
    function AddFieldA(const name: AnsiString): Word;
    function AddFieldW(const name: UnicodeString): Word;
    function AddField(const name: string): Word;
    procedure SetFieldType(const Index: Word; Size: Integer; Code: SmallInt; Scale: Smallint);

    function GetIsNullable(const Index: Word): boolean; override;

    procedure SetAsDouble(const Index: Word; const Value: Double); override;
    procedure SetAsCurrency(const Index: Word; const Value: Currency); override;
    procedure SetAsInt64(const Index: Word; const Value: Int64); override;
    procedure SetAsInteger(const Index: Word; const Value: Integer); override;
    procedure SetAsSingle(const Index: Word; const Value: Single); override;
    procedure SetAsSmallint(const Index: Word; const Value: Smallint); override;
    procedure SetAsRawByteString(const Index: Word; const Value: RawByteString); override;
    procedure SetAsAnsiString(const Index: Word; const Value: AnsiString); override;
    procedure SetAsUnicodeString(const Index: Word; const Value: UnicodeString); override;
    procedure SetAsQuad(const Index: Word; const Value: TISCQuad); override;
    procedure SetAsDateTime(const Index: Word; const Value: TDateTime); override;
    procedure SetAsBoolean(const Index: Word; const Value: Boolean); override;
    procedure SetAsDate(const Index: Word; const Value: Integer); override;
    procedure SetAsTime(const Index: Word; const Value: Cardinal); override;
  {$IFDEF GUID_TYPE}
    procedure SetAsGUID(const Index: Word; const Value: TGUID); override;
  {$ENDIF}
    function GetFieldType(const Index: Word): TUIBFieldType; override;
  public
    constructor Create(Charset: TCharacterSet); override;
    destructor Destroy; override;
    procedure Clear;
    function Parse(const SQL: string): string;
    function TryGetFieldIndex(const name: AnsiString; out Index: Word): Boolean; override;
    function GetFieldIndex(const name: AnsiString): Word; override;
    // don't use this method
    procedure AddFieldType(const Name: string; FieldType: TUIBFieldType;
      Scale: TScale = 1; Precision: byte = 0);

    property Values[const name: string]: Variant read GetByNameAsVariant; default;
    property FieldName[const Index: Word]: string read GetFieldName;
    property ParamCount : Word read FParamCount;
    property MaxSqlLen[const Index: Word]: Smallint read GetMaxSqlLen;
  end;

  TSQLParamsClass = class of TSQLParams;

(******************************************************************************)
(* Library                                                                    *)
(******************************************************************************)

type
  TDSQLInfoData = packed record
    InfoCode: byte;
    InfoLen : Word; // isc_portable_integer convert a SmallInt to Word ??? so just say it is a word
    case byte of
      isc_info_sql_stmt_type: (StatementType: TUIBStatementType);
      isc_info_sql_get_plan : (PlanDesc     : array[0..255] of AnsiChar);
  end;

  TUIBLibrary = class;

  TStatusVector = array[0..ISC_STATUS_LENGTH - 1] of ISCStatus;
  PStatusVector = ^TStatusVector;

  TOnConnectionLost = procedure(Lib: TUIBLibrary) of object;
  TOnGetDBExceptionClass = procedure(Number: Integer; out Excep: EUIBExceptionClass) of object;

  TUIBLibrary = class(TUIBaseLibrary)
  private
    FStatusVector: TStatusVector;
    FOnConnectionLost: TOnConnectionLost;
    FOnGetDBExceptionClass: TOnGetDBExceptionClass;
    FRaiseErrors: boolean;
    FSegmentSize: Word;
    function GetSegmentSize: Word;
    procedure SetSegmentSize(Value: Word);
    procedure CheckUIBApiCall(const Status: ISCStatus);
  public
    constructor Create; override;

    property OnConnectionLost: TOnConnectionLost read FOnConnectionLost write FOnConnectionLost;
    property OnGetDBExceptionClass: TOnGetDBExceptionClass read FOnGetDBExceptionClass write FOnGetDBExceptionClass;
    property RaiseErrors: boolean read FRaiseErrors write FRaiseErrors default True;

{$IFDEF FB25_UP}
    function ServerShutdown(timeout: Cardinal; const reason: Integer): Integer;
    procedure ServerShutdownCallback(callBack: FB_SHUTDOWN_CALLBACK;
      const mask: Integer; arg: Pointer);
{$ENDIF}

    {Attaches to an existing database.
     Ex: AttachDatabase('c:\DataBase.gdb', DBHandle, 'user_name=SYSDBA; password=masterkey'); }
    procedure AttachDatabase(const FileName: RawByteString; var DbHandle: IscDbHandle; Params: AnsiString; Sep: AnsiChar = ';');
    {Detaches from a database previously connected with AttachDatabase.}
    procedure DetachDatabase(var DBHandle: IscDbHandle);
    procedure DatabaseInfo(var DBHandle: IscDbHandle; const Items: AnsiString; var Buffer: AnsiString); overload;
    function DatabaseInfoIntValue(var DBHandle: IscDbHandle; const item: AnsiChar): Integer;
    function DatabaseInfoString(var DBHandle: IscDbHandle; item: byte; size: Integer): AnsiString;
    function DatabaseInfoDateTime(var DBHandle: IscDbHandle; item: byte): TDateTime;
    procedure DatabaseDrop(DbHandle: IscDbHandle);
{$IFDEF FB25_UP}
    function DatabaseCancelOperation(var DBHandle: IscDbHandle; option: ISC_USHORT): Boolean;
{$ENDIF}

    procedure TransactionStart(var TraHandle: IscTrHandle; var DbHandle: IscDbHandle; const TPB: AnsiString = '');
    procedure TransactionStartMultiple(var TraHandle: IscTrHandle; DBCount: Smallint; Vector: PISCTEB);
    procedure TransactionCommit(var TraHandle: IscTrHandle);
    procedure TransactionRollback(var TraHandle: IscTrHandle);
    procedure TransactionCommitRetaining(var TraHandle: IscTrHandle);
    procedure TransactionPrepare(var TraHandle: IscTrHandle);
    procedure TransactionRollbackRetaining(var TraHandle: IscTrHandle);
    function TransactionGetId(var TraHandle: IscTrHandle): Cardinal;

    procedure DSQLExecuteImmediate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
      const Statement: RawbyteString; Dialect: Word; Sqlda: TSQLDA = nil); overload;
    procedure DSQLExecuteImmediate(const Statement: RawbyteString; Dialect: Word; Sqlda: TSQLDA = nil); overload;
    procedure DSQLAllocateStatement(var DBHandle: IscDbHandle; var StmtHandle: IscStmtHandle);
    function DSQLPrepare(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
      Statement: RawbyteString; Dialect: Word; Sqlda: TSQLResult = nil): TUIBStatementType;
    procedure DSQLExecute(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
      Dialect: Word; Sqlda: TSQLDA = nil);
    procedure DSQLExecute2(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
      Dialect: Word; InSqlda: TSQLDA; OutSqlda: TSQLResult);
    procedure DSQLFreeStatement(var StmtHandle: IscStmtHandle; Option: Word);
    function  DSQLFetch(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult): boolean;
    function  DSQLFetchWithBlobs(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle;
      var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult): boolean;
    procedure DSQLDescribe(var DbHandle: IscDbHandle; var TrHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult);
    procedure DSQLDescribeBind(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLParams);
    procedure DSQLSetCursorName(var StmtHandle: IscStmtHandle; const cursor: AnsiString);
    procedure DSQLExecImmed2(var DBHhandle: IscDbHandle; var TraHandle: IscTrHandle;
      const Statement: RawbyteString; dialect: Word; InSqlda, OutSqlda: TSQLDA);

    procedure DSQLInfo(var StmtHandle: IscStmtHandle; const Items: array of byte; var buffer: AnsiString);
    function  DSQLInfoPlan(var StmtHandle: IscStmtHandle): string;
    function  DSQLInfoStatementType(var StmtHandle: IscStmtHandle): TUIBStatementType;
    function  DSQLInfoRowsAffected(var StmtHandle: IscStmtHandle; StatementType: TUIBStatementType): Cardinal;
    procedure DSQLInfoRowsAffected2(var StmtHandle: IscStmtHandle; out SelectedRows, InsertedRows, UpdatedRows, DeletedRows: Cardinal);

    procedure DDLExecute(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle; const ddl: AnsiString);

    function ArrayLookupBounds(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      const RelationName, FieldName: AnsiString): TArrayDesc;
    procedure ArrayGetSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      ArrayId: TISCQuad; var desc: TArrayDesc; DestArray: PPointer; var SliceLength: Integer);
    procedure ArrayPutSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      var ArrayId: TISCQuad; var desc: TArrayDesc; DestArray: Pointer;
      var SliceLength: Integer);
    procedure ArraySetDesc(const RelationName, FieldName: AnsiString; var SqlDtype,
      SqlLength, Dimensions: Smallint; var desc: TISCArrayDesc);

    procedure ServiceAttach(const ServiceName: RawByteString;
      var SvcHandle: IscSvcHandle; const Spb: RawByteString);
    procedure ServiceDetach(var SvcHandle: IscSvcHandle);
    procedure ServiceQuery(var SvcHandle: IscSvcHandle;
      const SendSpb, RequestSpb: RawByteString; var Buffer: RawByteString);
    procedure ServiceStart(var SvcHandle: IscSvcHandle; const Spb: RawByteString);

    function ErrSqlcode: ISCLong;
    function ErrInterprete: RawByteString;
    function ErrSQLInterprete(SQLCODE: Smallint): RawByteString;
{$IFDEF FB25_UP}
    function ErrSqlState: FB_SQLSTATE_STRING;
{$ENDIF}

    procedure BlobOpen(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
      var BlobHandle: IscBlobHandle; BlobId: TISCQuad; BPB: AnsiString = '');
    function  BlobGetSegment(var BlobHandle: IscBlobHandle;
      out length: Word; BufferLength: Cardinal; Buffer: Pointer): boolean;
    procedure BlobClose(var BlobHandle: IscBlobHandle);
    procedure BlobInfo(var BlobHandle: IscBlobHandle;
      out NumSegments, MaxSegment, TotalLength: Cardinal; out btype : byte);
    procedure BlobSize(var BlobHandle: IscBlobHandle; out Size: Cardinal);
    procedure BlobMaxSegment(var BlobHandle: IscBlobHandle; out Size: Cardinal);
    procedure BlobDefaultDesc(var Desc: TBlobDesc; const RelationName, FieldName: AnsiString);
    procedure BlobSaveToStream(var BlobHandle: IscBlobHandle; Stream: TStream);
    function  BlobReadString(var BlobHandle: IscBlobHandle): RawByteString; overload;
    procedure BlobReadString(var BlobHandle: IscBlobHandle; var Str: RawByteString); overload;
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
    procedure BlobWriteString(var BlobHandle: IscBlobHandle; const Str: RawByteString); overload;
    procedure BlobWriteStream(var BlobHandle: IscBlobHandle; Stream: TStream);

    function StreamBlobOpen(var BlobId: TISCQuad; var Database: IscDbHandle;
      var Transaction: IscTrHandle; mode: AnsiChar): PBStream;
    function StreamBlobClose(Stream: PBStream): integer;
    function EventBlock(var EventBuffer, ResultBuffer: PAnsiChar; Count: Smallint;
      v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: PAnsiChar): Integer;
    procedure EventQueue(var handle: IscDbHandle; var id: Integer; length: Word;
      events: PAnsiChar; ast: ISC_EVENT_CALLBACK; arg: Pointer);
    procedure EventCounts(var ResultVector: TStatusVector;
      BufferLength: Smallint; EventBuffer, ResultBuffer: PAnsiChar);
    procedure EventCancel(var DbHandle: IscDbHandle; var id: integer);
    procedure EventWaitFor(var handle: IscDbHandle; length: Smallint; events, buffer: Pointer);

    function IscFree(data: Pointer): Integer;
{$IFDEF IB71_UP}
    procedure SavepointRelease(var TrHandle: IscTrHandle; const Name: AnsiString);
    procedure SavepointRollback(var TrHandle: IscTrHandle; const Name: AnsiString; Option: Word);
    procedure SavepointStart(var TrHandle: IscTrHandle; const Name: AnsiString);
{$ENDIF}
    property SegMentSize: Word read GetSegmentSize write SetSegmentSize;
  end;

//******************************************************************************
// Conversion
//******************************************************************************

const
  DateOffset = 15018;
  TimeCoeff = 864000000;

  procedure DecodeTimeStamp(v: PISCTimeStamp; out DateTime: Double); overload;
  procedure DecodeTimeStamp(v: PISCTimeStamp; out TimeStamp: TTimeStamp); overload;
  function  DecodeTimeStamp(v: PISCTimeStamp): Double; overload;
  procedure DecodeSQLDate(v: Integer; out Year: SmallInt; out Month, Day: Word);
  procedure DecodeSQLTime(v: Cardinal; out Hour, Minute, Second: Word;
    out Fractions: LongWord);
  procedure EncodeTimeStamp(const DateTime: TDateTime; v: PISCTimeStamp); overload;
  procedure EncodeTimeStamp(const Date: Integer; v: PISCTimeStamp); overload;
  procedure EncodeTimeStamp(const Time: Cardinal; v: PISCTimeStamp); overload;
  function EncodeSQLDate(Year: Integer; Month, Day: Integer): Integer;
  function EncodeSQLTime(Hour, Minute, Second: Word;
    var Fractions: LongWord): Cardinal;

type
  TParamType = (
    prNone, // no param
    prByte, // Byte Param
    prShrt, // Short Param
    prCard, // Cardinal Param
    prStrg, // string Param
    prIgno  // Ignore Command
  );

  TDPBInfo = record
    Name      : AnsiString;
    ParamType : TParamType;
  end;

const

  DPBInfos : array[1..isc_dpb_Max_Value] of TDPBInfo =
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
{$IFDEF IB65_UP}
   ,(Name: 'gbak_ods_version';       ParamType: prCard) // ??
   ,(Name: 'gbak_ods_minor_version'; ParamType: prCard) // ??
{$ENDIF}

{$IFDEF IB7_UP}
   ,(Name: 'set_group_commit';       ParamType: prNone) // ??
{$ENDIF}
{$IFDEF IB71_UP}
   ,(Name: 'gbak_validate';          ParamType: prNone) // ??
{$ENDIF}
{$IFDEF FB103_UP}
   ,(Name: 'set_db_charset';         ParamType: prStrg) // ok
{$ENDIF}
{$IFDEF FB20_UP}
  ,(Name: 'gsec_attach';            ParamType: prByte)
  ,(Name: 'address_path';           ParamType: prStrg)
{$ENDIF}
{$IFDEF FB21_UP}
  ,(Name: 'process_id';             ParamType: prCard)
  ,(Name: 'no_db_triggers';         ParamType: prByte)
  ,(Name: 'trusted_auth';           ParamType: prStrg)
  ,(Name: 'process_name';           ParamType: prStrg)
{$ENDIF}
{$IFDEF FB25_UP}
  ,(Name: 'trusted_role';           ParamType: prNone)
  ,(Name: 'org_filename';           ParamType: prStrg)
  ,(Name: 'utf8_filename';          ParamType: prNone)
  ,(Name: 'ext_call_depth';         ParamType: prCard)
{$ENDIF}
   );

const

  BPBInfos : array[1..isc_bpb_Max_Value] of TDPBInfo =
   ((Name: 'source_type';      ParamType: prShrt),
    (Name: 'target_type';      ParamType: prShrt),
    (Name: 'type';             ParamType: prShrt),
    (Name: 'source_interp';    ParamType: prShrt),
    (Name: 'target_interp';    ParamType: prShrt),
    (Name: 'filter_parameter'; ParamType: prIgno) // not implemented (FB 2.0)
   );

{$IFNDEF COMPILER6_UP}
function TryStrToInt(const S: string; out Value: Integer): Boolean;
{$ENDIF}

function SQLQuote(const name: string): string;
function SQLUnQuote(const name: string): string;

const
  ScaleDivisor: array[-15..-1] of Int64 = (1000000000000000,100000000000000,
    10000000000000,1000000000000,100000000000,10000000000,1000000000,100000000,
    10000000,1000000,100000,10000,1000,100,10);

  ScaleFormat: array[-15..-1] of string = (
    '0.0##############', '0.0#############', '0.0############', '0.0###########',
    '0.0##########', '0.0#########', '0.0########', '0.0#######', '0.0######',
    '0.0#####', '0.0####', '0.0###', '0.0##', '0.0#', '0.0');

  CurrencyDivisor: array[-15..-1] of int64 = (100000000000,10000000000,
    1000000000,100000000,10000000,1000000,100000,10000,1000,100,10,1,10,100,
    1000);

implementation

uses
{$IFDEF UNICODE}
  AnsiStrings,
{$ENDIF}
  Math, uibconst, uibkeywords;

function MBUEncode(const str: UnicodeString; cp: Word): RawByteString;
begin
{$IFDEF MSWINDOWS}
  if cp > 0 then
  begin
    SetLength(Result, WideCharToMultiByte(cp, 0, PWideChar(str), length(str), nil, 0, nil, nil));
    WideCharToMultiByte(cp, 0, PWideChar(str), length(str), PAnsiChar(Result), Length(Result), nil, nil);
  end else
{$ENDIF}
    Result := AnsiString(str);
end;

function MBUDecode(const str: RawByteString; cp: Word): UnicodeString;
begin
{$IFDEF MSWINDOWS}
  if cp > 0 then
  begin
    SetLength(Result, MultiByteToWideChar(cp, 0, PAnsiChar(str), length(str), nil, 0));
    MultiByteToWideChar(cp, 0, PAnsiChar(str), length(str), PWideChar(Result), Length(Result));
  end else
{$ENDIF}
    Result := UnicodeString(str);
end;

procedure MBUDecode(str: PAnsiChar; size: Integer; cp: Word; buffer: PWideChar);
{$IFDEF MSWINDOWS}
var
  len: Integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  len := MultiByteToWideChar(cp, 0, str, size, nil, 0);
  MultiByteToWideChar(cp, 0, str, size, buffer, len);
  inc(buffer, len);
  buffer^ := #0;
{$ENDIF}
end;

function GetSystemCharacterset: TCharacterSet;
{$IFDEF MSWINDOWS}
  function GetAnsiCS: TCharacterSet;
  begin
    case GetACP of
      20127: Result := csASCII;
      950: Result := csBIG_5;
      437: Result := csDOS437;
      850: Result := csDOS850;
      852: Result := csDOS852;
      857: Result := csDOS857;
      860: Result := csDOS860;
      861: Result := csDOS861;
      863: Result := csDOS863;
      865: Result := csDOS865;
      20932: Result := csEUCJ_0208;
      28591: Result := csISO8859_1;
      28592: Result := csISO8859_2;
      949: Result := csKSC_5601;
      1250: Result := csWIN1250;
      1251: Result := csWIN1251;
      1252: Result := csWIN1252;
      1253: Result := csWIN1253;
      1254: Result := csWIN1254;
    {$IFDEF FB15_UP}
      737: Result := csDOS737;
      775: Result :=  csDOS775;
      858: Result := csDOS858;
      862: Result :=  csDOS862;
      864: Result := csDOS864;
      866: Result :=  csDOS866;
      869: Result := csDOS869;
      1255: Result := csWIN1255;
      1256: Result := csWIN1256;
      1257: Result :=  csWIN1257;
      28593: Result := csISO8859_3;
      28594: Result := csISO8859_4;
      28595: Result := csISO8859_5;
      28596: Result := csISO8859_6;
      28597: Result := csISO8859_7;
      28598: Result := csISO8859_8;
      28599: Result := csISO8859_9;
      28603: Result := csISO8859_13;
    {$ENDIF}
    {$IFDEF IB71_UP}
      28605: Result := csISO8859_15;
      20866: Result := csKOI8R;
    {$ENDIF}
    {$IFDEF FB20_UP}
      20866: Result := csKOI8R;
      21866: Result := csKOI8U;
    {$ENDIF}
    {$IFDEF FB21_UP}
      1258: Result := csWIN1258;
      874: Result := csTIS620;
      936: Result := csGBK;
      932: Result := csCP943C;
    {$ELSE}
      936: Result := csGB_2312;
      932: Result := csSJIS_0208;
    {$ENDIF}
    else
      Result := csNONE;
    end;
  end;
{$ENDIF}
begin
{$IFDEF UNICODE}
  {$IFDEF FB20_UP}
     Result := csUTF8;
  {$ELSE}
     Result := GetAnsiCS;
  {$ENDIF}
{$ELSE}
  {$IFDEF MSWINDOWS}
    Result := GetAnsiCS;
  {$ELSE}
    Result := csNONE;
  {$ENDIF}
{$ENDIF}
end;

(******************************************************************************)
(* Errors handling                                                            *)
(******************************************************************************)

{$IFNDEF COMPILER6_UP}
function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;
{$ENDIF}

function SQLQuote(const name: string): string;
var
  i, len: PtrInt;
begin
  len := Length(name);
  if (len > 1) and (name[1] = '"') and (name[len] = '"') then
  begin // allready quoted: keep case
    Result := name;
    Exit;
  end;
  if (len > 1) and (not ({$IFDEF UNICODE}(name[1] < #256) and {$ENDIF} (AnsiChar(name[1]) in ['A'..'Z']))) then
  begin // non standard carracter: keep case
    Result := '"' + name + '"';
    Exit;
  end;

  for i := 1 to len do
    if not ({$IFDEF UNICODE}(name[1] < #256) and {$ENDIF} (AnsiChar(name[i]) in ['A'..'Z', '0'..'9', '_', '$'])) then
    begin // non standard carracter: keep case
      Result := '"' + name + '"';
      Exit;
    end;
  Result := UpperCase(name);
end;

function SQLUnQuote(const name: string): string;
var
  i, len: PtrInt;
begin
  len := Length(name);
  if (len > 1) and (name[1] = '"') and (name[len] = '"') then
  begin  // allready quoted: keep case
    Result := copy(name, 2, len-2);
    Exit;
  end;

  if (len > 1) and (not ({$IFDEF UNICODE}(name[1] < #256) and {$ENDIF} (AnsiChar(name[1]) in ['A'..'Z']))) then
  begin // non standard carracter: keep case
    Result := name;
    Exit;
  end;

  for i := 1 to len do
    if not ({$IFDEF UNICODE}(name[1] < #256) and {$ENDIF} (AnsiChar(name[i]) in ['A'..'Z', '0'..'9', '_', '$'])) then
    begin // non standard carracter: keep case
      Result := name;
      Exit;
    end;
  Result := UpperCase(name);
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

  procedure TUIBLibrary.CheckUIBApiCall(const Status: ISCStatus);
  var
    Number: Integer;
    Excep: EUIBExceptionClass;
    procedure RaiseException(const Excep: EUIBExceptionClass);
    var Exception: EUIBError;
    begin
      Exception := Excep.Create(string(ErrInterprete));
      if Excep = EUIBException then
        EUIBException(Exception).FNumber := Number;
      Exception.FSQLCode   := ErrSqlcode;
      if Exception.FSQLCode <> 0 then
        Exception.Message := Exception.Message + string(ErrSQLInterprete(Exception.FSQLCode)) + NewLine;
      Exception.FGDSCode := Status;
      Exception.FErrorCode := GETCode(Status);
{$IFDEF FB25_UP}
      Exception.FSQLState := ErrSqlState;
{$ENDIF}
      Exception.Message := Exception.Message + 'Error Code: ' + IntToStr(Exception.FErrorCode);
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
          Number := FStatusVector[3];
          if assigned(FOnGetDBExceptionClass) then
            FOnGetDBExceptionClass(Number, Excep) else
            Excep := EUIBException;
        end else
          Excep := EUIBError;
        FAC_GFIX    : Excep := EUIBGFIXError;
        FAC_DSQL    : Excep := EUIBDSQLError;
        FAC_DYN     : Excep := EUIBDYNError;
        FAC_GBAK    : Excep := EUIBGBAKError;
        FAC_GSEC    : Excep := EUIBGSECError;
        FAC_LICENSE : Excep := EUIBLICENSEError;
        FAC_GSTAT   : Excep := EUIBGSTATError;
      else
        Excep := EUIBError;
      end;
      if ((Status = isc_lost_db_connection) or (Status = isc_network_error)) and
        Assigned(FOnConnectionLost) then
          FOnConnectionLost(Self);
      RaiseException(Excep)
    end;
  end;

//******************************************************************************
// Database
//******************************************************************************

  constructor TUIBLibrary.Create;
  begin
    inherited;
    FRaiseErrors := True;
    FSegmentSize := 16*1024;
  end;

  function GetClientLibrary: string;
  {$IFDEF DLLREGISTRY}
  var
    Key: HKEY;
    Size: Cardinal;
    HR: Integer;
  {$ENDIF}
  begin
  {$IFDEF DLLREGISTRY}
    if FileExists(ExtractFilePath(ParamStr(0)) + GDS32DLL) then
      Result := GDS32DLL else
    begin
      HR := RegOpenKeyEx(HKEY_LOCAL_MACHINE, FBINSTANCES, 0, KEY_READ, Key);
      if (HR = ERROR_SUCCESS) then
      begin
        HR := RegQueryValueEx(Key, 'DefaultInstance', nil, nil, nil, @Size);
        if (HR = ERROR_SUCCESS) then
        begin
          SetLength(Result, Size div sizeof(Char));
          HR := RegQueryValueEx(Key, 'DefaultInstance', nil, nil, Pointer(Result), @Size);
          if (HR = ERROR_SUCCESS) then
            Result := Trim(Result)+ 'bin\' + GDS32DLL;
        end;
        RegCloseKey(Key);
      end;
      if (HR <> ERROR_SUCCESS) then
        Result := GDS32DLL;
    end;
  {$ELSE}
    Result := GDS32DLL;
  {$ENDIF}
  end;

  function CreateDBParams(Params: AnsiString; Delimiter: AnsiChar = ';'): AnsiString;
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
      Move(PAnsiChar(AString)^, Result[FinalSize-l+1], l);
    end;
  begin
    FinalSize := 1;
    BufferSize := 32;
    SetLength(Result, BufferSize);
    Result[1] := chr(isc_dpb_version1);
    CurPos  := PAnsiChar(Params);
    while (CurPos <> nil) do
    begin
      NextPos := StrScan(CurPos, Delimiter);
      if (NextPos = nil) then
        CurStr := CurPos else
        begin
          CurStr := Copy(CurPos, 0, NextPos-CurPos);
          Inc(NextPos);
        end;
      CurPos := NextPos;
      if (CurStr = '') then Continue;
      begin
        CurValue := '';
        EqualPos := Pos(AnsiString('='), CurStr);
        if EqualPos <> 0 then
        begin
          CurValue := Copy(CurStr, EqualPos+1, Length(CurStr) - EqualPos);
          CurStr   := Copy(CurStr, 0, EqualPos-1);
        end;
        StrLower(PAnsiChar(CurStr));
        CurStr := Trim(CurStr);
        CurValue := Trim(CurValue);
        for Code := 1 to isc_dpb_Max_Value do
          with DPBInfos[Code] do
            if (Name = CurStr) then
            begin
              case ParamType of
                prNone :
                  begin
                    AddByte(Code);
                    AddByte(0);
                  end;
                prByte :
                  if TryStrToInt(string(CurValue), AValue) and (AValue >= 0) and (AValue <= 255) then
                  begin
                    AddByte(Code);
                    AddByte(Byte(AValue));
                  end;
                prCard :
                  if TryStrToInt(string(CurValue), AValue) and (AValue > 0) then
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

  function CreateTRParams(Options: TTransParams; const LockRead, LockWrite: string{$IFDEF FB20_UP}; LockTimeout: Word{$ENDIF}): RawByteString;
  var
    tp: TTransParam;
    procedure ParseStrOption(const code: AnsiChar; const Value: AnsiString);
    var
      P, Start: PAnsiChar;
      S: AnsiString;
    begin
      P := Pointer(Value);
      if P <> nil then
        while (P^ <> #0) do
        begin
          Start := P;
          while not (P^ in [#0, ';']) do Inc(P);
          if (P - Start) > 0 then
          begin
            SetString(S, Start, P - Start);
            Result := Result + code + AnsiChar(P - Start) + S;
          end;
          if P^ =';' then inc(P);
        end;
    end;
  const
    tpc: array[TTransParam] of AnsiChar = (
      isc_tpb_consistency,
      isc_tpb_concurrency,
    {$IFNDEF FB_21UP}
      isc_tpb_shared,
      isc_tpb_protected,
      isc_tpb_exclusive,
    {$ENDIF}
      isc_tpb_wait,
      isc_tpb_nowait,
      isc_tpb_read,
      isc_tpb_write,
      isc_tpb_lock_read,
      isc_tpb_lock_write,
      isc_tpb_verb_time,
      isc_tpb_commit_time,
      isc_tpb_ignore_limbo,
      isc_tpb_read_committed,
      isc_tpb_autocommit,
      isc_tpb_rec_version,
      isc_tpb_no_rec_version,
      isc_tpb_restart_requests,
      isc_tpb_no_auto_undo
    {$IFDEF FB20_UP}
      ,isc_tpb_lock_timeout
    {$ENDIF}
      );

  begin
    if Options = [tpConcurrency,tpWait,tpWrite] then
      result := ''
    else
      begin
        Result := isc_tpb_version3;
        for tp := Low(TTransParam) to High(TTransParam) do
          if (tp in Options) then
          begin
            case tp of
              tpLockRead    : ParseStrOption(tpc[tp], AnsiString(LockRead));
              tpLockWrite   : ParseStrOption(tpc[tp], AnsiString(LockWrite));
            {$IFDEF FB20_UP}
              tpLockTimeout : Result := Result + tpc[tp] + PAnsiChar(@LockTimeout)[0] + PAnsiChar(LockTimeout)[1];
            {$ENDIF}
            else
              Result := Result + tpc[tp];
            end;
          end;
      end;
  end;

{$IFDEF FB25_UP}
  function TUIBLibrary.ServerShutdown(timeout: Cardinal; const reason: Integer): Integer;
  begin
     Result := fb_shutdown(timeout, reason);
  end;

  procedure TUIBLibrary.ServerShutdownCallback(callBack: FB_SHUTDOWN_CALLBACK;
    const mask: Integer; arg: Pointer);
  begin
    CheckUIBApiCall(fb_shutdown_callback(@FStatusVector, callBack, mask, arg));
  end;


{$ENDIF}

  procedure TUIBLibrary.AttachDatabase(const FileName: RawByteString; var DbHandle: IscDbHandle;
    Params: AnsiString; Sep: AnsiChar = ';');
  begin
    Params := CreateDBParams(Params, Sep);
    CheckUIBApiCall(isc_attach_database(@FStatusVector, Length(FileName), Pointer(FileName),
      @DBHandle, Length(Params), PAnsiChar(Params)));
  end;

  procedure TUIBLibrary.DetachDatabase(var DBHandle: IscDbHandle);
  begin
    CheckUIBApiCall(isc_detach_database(@FStatusVector, @DBHandle));
    // if connection lost DBHandle must be set manually to nil.
    DBHandle := nil;
  end;

  procedure TUIBLibrary.DatabaseInfo(var DBHandle: IscDbHandle;
    const Items: AnsiString; var Buffer: AnsiString);
  begin
    CheckUIBApiCall(isc_database_info(@FStatusVector, @DBHandle, Length(Items),
      Pointer(Items), Length(Buffer), Pointer(Buffer)));
  end;

  function TUIBLibrary.DatabaseInfoIntValue(var DBHandle: IscDbHandle;
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
  begin
    result := 0;
    CheckUIBApiCall(isc_database_info(@FStatusVector, @DBHandle, 1, @item,
      sizeof(data), @data));
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

  function TUIBLibrary.DatabaseInfoDateTime(var DBHandle: IscDbHandle; item: byte): TDateTime;
  var
    data: packed record
      item: AnsiChar;
      len: word;
      date: TISCTimeStamp;
      dummy: word;
    end;
  begin
    CheckUIBApiCall(isc_database_info(@FStatusVector, @DBHandle, 1, @item,
      sizeof(data), @data));
    Result := DecodeTimeStamp(@data.date);
  end;

  function TUIBLibrary.DatabaseInfoString(var DBHandle: IscDbHandle;
    item: byte; size: Integer): AnsiString;
  begin
    SetLength(result, size);
    while true do
    begin
      CheckUIBApiCall(isc_database_info(@FStatusVector, @DBHandle, 1, @item,
        Length(result), PAnsiChar(result)));
      if result[1] = chr(isc_info_truncated) then
        SetLength(result, Length(result) + size) else
          if (byte(result[1]) = item) or (result[1] = #1) then
            Break else
            begin
              result := '';
              raise Exception.Create('');
            end;
    end;
  end;

  function StrToCharacterSet(const CharacterSet: RawByteString): TCharacterSet;
  var
    len: Integer;
  begin
    len := length(CharacterSet);
    if (len = 0) then
      Result := csNONE else
    begin
      for Result := low(TCharacterSet) to High(TCharacterSet) do
        if (len = Length(CharacterSetStr[Result])) and
          (StrIComp(PAnsiChar(CharacterSetStr[Result]), PAnsiChar(CharacterSet)) = 0) then
            Exit;
      raise Exception.CreateFmt(EUIB_CHARSETNOTFOUND, [CharacterSet]);
    end;
  end;

  procedure TUIBLibrary.DatabaseDrop(DbHandle: IscDbHandle);
  begin
    CheckUIBApiCall(isc_drop_database(@FStatusVector, @DbHandle));
  end;

{$IFDEF FB25_UP}
  function TUIBLibrary.DatabaseCancelOperation(var DBHandle: IscDbHandle; option: ISC_USHORT): Boolean;
  var
    sv: TStatusVector;
  begin
    FillChar(sv, SizeOf(sv), 0);
    Result := fb_cancel_operation(@sv, @DbHandle, option) = 0;
  end;
{$ENDIF}


//******************************************************************************
// Transaction
//******************************************************************************

  procedure TUIBLibrary.TransactionStart(var TraHandle: IscTrHandle; var DbHandle: IscDbHandle;
    const TPB: AnsiString = '');
  var Vector: TISCTEB;
  begin
    Vector.Handle  := @DbHandle;
    Vector.Len     := Length(TPB);
    Vector.Address := PAnsiChar(TPB);
    TransactionStartMultiple(TraHandle, 1, @Vector);
  end;

  procedure TUIBLibrary.TransactionStartMultiple(var TraHandle: IscTrHandle; DBCount: Smallint; Vector: PISCTEB);
  begin
    CheckUIBApiCall(isc_start_multiple(@FStatusVector, @TraHandle, DBCount, Vector));
  end;

  procedure TUIBLibrary.TransactionCommit(var TraHandle: IscTrHandle);
  begin
    CheckUIBApiCall(isc_commit_transaction(@FStatusVector, @TraHandle));
    // if connection lost TraHandle must be set manually to nil.
    TraHandle := nil;
  end;

  procedure TUIBLibrary.TransactionRollback(var TraHandle: IscTrHandle);
  begin
    CheckUIBApiCall(isc_rollback_transaction(@FStatusVector, @TraHandle));
    // if connection lost TraHandle must be set manually to nil.
    TraHandle := nil;
  end;

  procedure TUIBLibrary.TransactionCommitRetaining(var TraHandle: IscTrHandle);
  begin
    CheckUIBApiCall(isc_commit_retaining(@FStatusVector, @TraHandle));
  end;

  procedure TUIBLibrary.TransactionPrepare(var TraHandle: IscTrHandle);
  begin
    CheckUIBApiCall(isc_prepare_transaction(@FStatusVector, @TraHandle));
  end;

  procedure TUIBLibrary.TransactionRollbackRetaining(var TraHandle: IscTrHandle);
  begin
    CheckUIBApiCall(isc_rollback_retaining(@FStatusVector, @TraHandle));
  end;

  function TUIBLibrary.TransactionGetId(var TraHandle: IscTrHandle): Cardinal;
  var
    tra_items: AnsiChar;
    tra_info: array [0..31] of AnsiChar;
  begin
    tra_items := AnsiChar(isc_info_tra_id);
    CheckUIBApiCall(isc_transaction_info(@FStatusVector, @TraHandle,
      sizeof(tra_items), @tra_items, sizeof(tra_info), tra_info));
    Result := PCardinal(tra_info + 3)^;
  end;

//******************************************************************************
// DSQL
//******************************************************************************

  function GetSQLDAData(SQLDA: TSQLDA): Pointer;
  begin
    if (SQLDA <> nil) then
      Result := SQLDA.FXSQLDA else
      Result := nil;
  end;

  //****************************************
  // API CALLS
  //****************************************

  procedure TUIBLibrary.DSQLExecuteImmediate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
    const Statement: RawbyteString; Dialect: Word; Sqlda: TSQLDA = nil);
  begin
    CheckUIBApiCall(isc_dsql_execute_immediate(@FStatusVector, @DBHandle, @TraHandle,
      length(Statement), Pointer(Statement), Dialect, GetSQLDAData(Sqlda)));
  end;

  procedure TUIBLibrary.DSQLExecuteImmediate(const Statement: RawbyteString; Dialect: Word; Sqlda: TSQLDA = nil);
  var p: pointer;
  begin
    p := nil;
    CheckUIBApiCall(isc_dsql_execute_immediate(@FStatusVector, @p, @p,
      length(Statement), Pointer(Statement), Dialect, GetSQLDAData(Sqlda)));
  end;

  procedure TUIBLibrary.DSQLAllocateStatement(var DBHandle: IscDbHandle; var StmtHandle: IscStmtHandle);
  begin
    CheckUIBApiCall(isc_dsql_allocate_statement(@FStatusVector, @DBHandle, @StmtHandle));
  end;

  function TUIBLibrary.DSQLPrepare(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
    Statement: RawbyteString; Dialect: Word; Sqlda: TSQLResult = nil): TUIBStatementType;
  var
    STInfo: packed record
      InfoCode: byte;
      InfoLen : Word; // isc_portable_integer convert a SmallInt to Word ??? so just say it is a word
      InfoType: TUIBStatementType;
      Filler: byte;
    end;
    InfoIn: byte;
  begin
	  CheckUIBApiCall(isc_dsql_prepare(@FStatusVector, @TraHandle, @StmtHandle, Length(Statement),
        PAnsiChar(Statement), Dialect, GetSQLDAData(Sqlda)));
      InfoIn := isc_info_sql_stmt_type;
      isc_dsql_sql_info(@FStatusVector, @StmtHandle, 1, @InfoIn, SizeOf(STInfo), @STInfo);
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

  procedure TUIBLibrary.DSQLExecute(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
    Dialect: Word; Sqlda: TSQLDA = nil);
  begin
    CheckUIBApiCall(isc_dsql_execute(@FStatusVector, @TraHandle, @StmtHandle,
      Dialect, GetSQLDAData(Sqlda)));
  end;

  procedure TUIBLibrary.DSQLExecute2(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word;
    InSqlda: TSQLDA; OutSqlda: TSQLResult);
  begin
    CheckUIBApiCall(isc_dsql_execute2(@FStatusVector, @TraHandle, @StmtHandle, Dialect,
      GetSQLDAData(InSqlda), GetSQLDAData(OutSqlda)));
  end;

  procedure TUIBLibrary.DSQLFreeStatement(var StmtHandle: IscStmtHandle; Option: Word);
  begin
    CheckUIBApiCall(isc_dsql_free_statement(@FStatusVector, @StmtHandle, Option));
    // if connection lost StmtHandle must be set manually to nil.
    if option = DSQL_DROP then
       StmtHandle := nil;
  end;

  function TUIBLibrary.DSQLFetch(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
    var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult): boolean;
  var
    Status: ISCStatus;
    i, j: integer;
    destArray: pointer;
    SliceLen: integer;
  begin
    Result := True;
    if (Sqlda <> nil) then
      Sqlda.FScrollEOF := False;
    Status := isc_dsql_fetch(@FStatusVector, @StmtHandle, Dialect, GetSQLDAData(Sqlda));

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
      CheckUIBApiCall(Status);
    end;
  end;

  function  TUIBLibrary.DSQLFetchWithBlobs(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle;
    var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult): boolean;
  var
    Status: ISCStatus;
    BlobHandle: IscBlobHandle;
    i, j: Integer;
    destArray: Pointer;
    SliceLen: integer;
    BlobData: PBlobData;
  begin
    Result := True;
    if (Sqlda <> nil) then
      sqlda.FScrollEOF := False;
    Status := isc_dsql_fetch(@FStatusVector, @StmtHandle, Dialect, GetSQLDAData(Sqlda));

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
      CheckUIBApiCall(Status);
    end;
  end;

  procedure TUIBLibrary.DSQLDescribe(var DbHandle: IscDbHandle; var TrHandle: IscTrHandle;
    var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult);
  var
    i: integer;
    ArrayCount: integer;
  begin
	  CheckUIBApiCall(isc_dsql_describe(@FStatusVector, @StmtHandle, Dialect, GetSQLDAData(Sqlda)));
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
              ArrayLookupBounds(DbHandle, TrHandle, AnsiString(Sqlda.RelName[i]), AnsiString(Sqlda.SqlName[i]));
            Sqlda.FArrayInfos[ArrayCount].index := i;
          end;
      end;
      Sqlda.AllocateDataBuffer;
    end;
  end;

  procedure TUIBLibrary.DSQLDescribeBind(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLParams);
  var
    i, len: Integer;
    da: PUIBSQLDa;
    src, dst: PUIBSQLVar;
  begin
    if Sqlda = nil then Exit;
    len := XSQLDA_LENGTH(sqlda.Data.sqln);
    GetMem(da, len);
    try
      FillChar(da^, len, 0);
      da.version := Sqlda.Data.version;
      da.sqln := sqlda.Data.sqln;
      da.sqld := sqlda.Data.sqld;
      CheckUIBApiCall(isc_dsql_describe_bind(@FStatusVector, @StmtHandle, Dialect, PXSQLDA(da)));
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
{$IFDEF IB7_UP}
          src^.SqlPrecision := dst^.SqlPrecision;
{$ENDIF}
        end;
       Sqlda.AllocateDataBuffer(False);
    finally
      FreeMem(da);
    end;
  end;

  procedure  TUIBLibrary.DSQLSetCursorName(var StmtHandle: IscStmtHandle; const cursor: AnsiString);
  begin
    CheckUIBApiCall(isc_dsql_set_cursor_name(@FStatusVector, @StmtHandle, PAnsiChar(cursor), 0));
  end;

  procedure TUIBLibrary.DSQLExecImmed2(var DBHhandle: IscDbHandle; var TraHandle: IscTrHandle;
    const Statement: RawbyteString; dialect: Word; InSqlda, OutSqlda: TSQLDA);
  begin
    CheckUIBApiCall(isc_dsql_exec_immed2(@FStatusVector, @DBHhandle, @TraHandle, Length(Statement),
      PAnsiChar(Statement), dialect, GetSQLDAData(InSqlda), GetSQLDAData(OutSqlda)));
  end;

  procedure TUIBLibrary.DSQLInfo(var StmtHandle: IscStmtHandle; const Items: array of byte; var buffer: AnsiString);
  begin
    CheckUIBApiCall(isc_dsql_sql_info(@FStatusVector, @StmtHandle, Length(Items), @Items[0],
      Length(buffer), PAnsiChar(buffer)));
  end;

  function TUIBLibrary.DSQLInfoPlan(var StmtHandle: IscStmtHandle): string;
  var
    STInfo : packed record
      InfoCode: byte;
      InfoLen : Word;
      PlanDesc: array[0..1024] of AnsiChar;
    end;
    InfoType: Byte;
  begin
    InfoType := isc_info_sql_get_plan;
    CheckUIBApiCall(isc_dsql_sql_info(@FStatusVector, @StmtHandle, 1, @InfoType,
      SizeOf(STInfo), @STInfo));
    SetString(Result, PAnsiChar(@STInfo.PlanDesc[1]), STInfo.InfoLen - 1);
  end;

  function TUIBLibrary.DSQLInfoStatementType(var StmtHandle: IscStmtHandle): TUIBStatementType;
  var
    STInfo: packed record
      InfoCode: byte;
      InfoLen : Word;
      InfoType: TUIBStatementType;
      Filler: byte;
    end;
    InfoIn: byte;
  begin
    InfoIn := isc_info_sql_stmt_type;
    CheckUIBApiCall(isc_dsql_sql_info(@FStatusVector, @StmtHandle, 1,
      @InfoIn, SizeOf(STInfo), @STInfo));
    dec(STInfo.InfoType);
    Result := STInfo.InfoType;
  end;

  procedure TUIBLibrary.DSQLInfoRowsAffected2(var StmtHandle: IscStmtHandle;
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
  begin
    Command := isc_info_sql_records;
    CheckUIBApiCall(isc_dsql_sql_info(@FStatusVector, @StmtHandle, 1, @Command,
      SizeOf(InfoData), @InfoData));
    for command := 0 to 3 do
      with InfoData.Infos[command] do
      case InfoCode of
        isc_info_req_select_count: SelectedRows := Rows;
        isc_info_req_insert_count: InsertedRows := Rows;
        isc_info_req_update_count: UpdatedRows := Rows;
        isc_info_req_delete_count: DeletedRows := Rows;
      end;
  end;

  function TUIBLibrary.DSQLInfoRowsAffected(var StmtHandle: IscStmtHandle;
    StatementType: TUIBStatementType): Cardinal;
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

  procedure TUIBLibrary.DDLExecute(var DBHandle: IscDbHandle;
    var TraHandle: IscTrHandle; const ddl: AnsiString);
  begin
    CheckUIBApiCall(isc_ddl(@FStatusVector, @DBHandle, @TraHandle,
      length(ddl), Pointer(ddl)));
  end;

//******************************************************************************
//  Array
//******************************************************************************
  function TUIBLibrary.ArrayLookupBounds(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
    const RelationName, FieldName: AnsiString): TArrayDesc;
  begin
    {$IFDEF IB7_UP}
      CheckUIBApiCall(isc_array_lookup_bounds2(@FStatusVector, @DBHandle, @TransHandle,
        PAnsiChar(RelationName), PAnsiChar(FieldName), @Result));
    {$ELSE}
      CheckUIBApiCall(isc_array_lookup_bounds(@FStatusVector, @DBHandle, @TransHandle,
        PAnsiChar(RelationName), PAnsiChar(FieldName), @Result));
    {$ENDIF}
  end;

  procedure TUIBLibrary.ArrayGetSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle; ArrayId: TISCQuad;
    var desc: TArrayDesc; DestArray: PPointer; var SliceLength: Integer);
  begin
    {$IFDEF IB7_UP}
      CheckUIBApiCall(isc_array_get_slice2(@FStatusVector, @DBHandle, @TransHandle, @ArrayId,
        @desc, DestArray, @SliceLength));
    {$ELSE}
      CheckUIBApiCall(isc_array_get_slice(@FStatusVector, @DBHandle, @TransHandle, @ArrayId,
        @desc, DestArray, @SliceLength));
    {$ENDIF}
  end;

  procedure TUIBLibrary.ArrayPutSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
    var ArrayId: TISCQuad; var desc: TArrayDesc; DestArray: Pointer; var SliceLength: Integer);
  begin
    {$IFDEF IB7_UP}
      CheckUIBApiCall(isc_array_put_slice2(@FStatusVector, @DBHandle, @TransHandle, @ArrayId,
        @desc, DestArray, @SliceLength));
    {$ELSE}
      CheckUIBApiCall(isc_array_put_slice(@FStatusVector, @DBHandle, @TransHandle, @ArrayId,
        @desc, DestArray, @SliceLength));
    {$ENDIF}
  end;

  procedure TUIBLibrary.ArraySetDesc(const RelationName, FieldName: AnsiString; var SqlDtype,
    SqlLength, Dimensions: Smallint; var desc: TISCArrayDesc);
  begin
    CheckUIBApiCall(isc_array_set_desc(@FStatusVector, PAnsiChar(RelationName),
      PAnsiChar(FieldName), @SqlDtype, @SqlLength, @Dimensions, @desc));
  end;


//******************************************************************************
//  Error-handling
//******************************************************************************

  function  TUIBLibrary.ErrSqlcode: ISCLong;
  begin
    Result := isc_sqlcode(@FStatusVector);
  end;

  function TUIBLibrary.ErrInterprete: RawByteString;
  var
    StatusVector: PStatusVector;
    len: Integer;
    buffer: array[0..512] of AnsiChar;
  begin
    Result := '';
    StatusVector := @FStatusVector;
    repeat
    {$IFDEF FB20_UP}
      len := fb_interpret(buffer, sizeof(buffer), @StatusVector);
    {$ELSE}
      len := isc_interprete(buffer, @StatusVector);
    {$ENDIF}
      if len > 0 then
        Result := Result + copy(buffer, 0, len) + NewLine else
        Break;
    until False;
  end;

  function TUIBLibrary.ErrSQLInterprete(SQLCODE: Smallint): RawByteString;
  var
    i : Integer;
  begin
    SetLength(Result, 255);
    isc_sql_interprete(SQLCODE, PAnsiChar(Result), 255);
    for i := 1 to 255 do if Result[i] = #0 then Break; // Quick trim
    SetLength(Result, i-1);
  end;

{$IFDEF FB25_UP}
  function TUIBLibrary.ErrSqlState: FB_SQLSTATE_STRING;
  begin
    fb_sqlstate(@Result, @FStatusVector);
  end;
{$ENDIF}

//******************************************************************************
// Services
//******************************************************************************

  procedure TUIBLibrary.ServiceAttach(const ServiceName: RawByteString; var SvcHandle: IscSvcHandle; const Spb: RawByteString);
  begin
    CheckUIBApiCall(isc_service_attach(@FStatusVector, Length(ServiceName),
      PAnsiChar(ServiceName), @SvcHandle, Length(Spb), PAnsiChar(Spb)));
  end;

  procedure TUIBLibrary.ServiceDetach(var SvcHandle: IscSvcHandle);
  begin
    CheckUIBApiCall(isc_service_detach(@FStatusVector, @SvcHandle));
  end;

  procedure TUIBLibrary.ServiceQuery(var SvcHandle: IscSvcHandle; const SendSpb, RequestSpb: RawByteString; var Buffer: RawByteString);
  begin
    CheckUIBApiCall(isc_service_query(@FStatusVector, @SvcHandle, nil,
      Length(SendSpb), PAnsiChar(SendSpb), Length(RequestSpb), PAnsiChar(RequestSpb),
      Length(Buffer), PAnsiChar(Buffer)));
  end;

  procedure TUIBLibrary.ServiceStart(var SvcHandle: IscSvcHandle; const Spb: RawByteString);
  begin
    CheckUIBApiCall(isc_service_start(@FStatusVector, @SvcHandle, nil, Length(Spb), PAnsiChar(Spb)));
  end;

//******************************************************************************
//  Blob
//******************************************************************************

  function CreateBlobParams(Params: AnsiString; Delimiter: AnsiChar = ';'): AnsiString;
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
      NextPos := StrScan(CurPos, Delimiter);
      if (NextPos = nil) then
        CurStr := CurPos else
        begin
          CurStr := Copy(CurPos, 0, NextPos-CurPos);
          Inc(NextPos);
        end;
      CurPos := NextPos;
      if (CurStr = '') then Continue;
      begin
        CurValue := '';
        EqualPos := Pos(AnsiChar('='), CurStr);
        if EqualPos <> 0 then
        begin
          CurValue := Copy(CurStr, EqualPos+1, Length(CurStr) - EqualPos);
          CurStr   := Copy(CurStr, 0, EqualPos-1);
        end;
        StrLower(PAnsiChar(CurStr));
        CurStr := Trim(CurStr);
        CurValue := Trim(CurValue);
        for Code := 1 to isc_bpb_Max_Value do
          with BPBInfos[Code] do
            if (Name = CurStr) then
            begin
              case ParamType of
                prShrt :
                  if TryStrToInt(string(CurValue), AValue) and (AValue >= -128) and (AValue <= 127) then
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

  procedure TUIBLibrary.BlobOpen(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
    var BlobHandle: IscBlobHandle; BlobId: TISCQuad; BPB: AnsiString = '');
  begin
    BPB := CreateBlobParams(BPB,';');
    CheckUIBApiCall(isc_open_blob2(@FStatusVector, @DBHandle, @TraHandle, @BlobHandle,
      @BlobId, Length(BPB), PAnsiChar(BPB)));
  end;

  function TUIBLibrary.BlobGetSegment(var BlobHandle: IscBlobHandle; out length: Word;
    BufferLength: Cardinal; Buffer: Pointer): boolean;
  var
    AStatus: ISCStatus;
  begin
    if BufferLength > High(Word) then
      BufferLength := High(Word);
    AStatus := isc_get_segment(@FStatusVector, @BlobHandle, @length, Word(BufferLength), Buffer);
    Result := (AStatus = 0) or (FStatusVector[1] = isc_segment);
    if not Result then
      if (FStatusVector[1] <> isc_segstr_eof) then
        CheckUIBApiCall(AStatus);
  end;

  procedure TUIBLibrary.BlobClose(var BlobHandle: IscBlobHandle);
  begin
    CheckUIBApiCall(isc_close_blob(@FStatusVector, @BlobHandle));
  end;

type
  TBlobInfo = packed record
    Info: AnsiChar;
    Length: Word;
    case byte of
      0: (CardType: Integer);
      1: (ByteType: Byte);
  end;

  procedure TUIBLibrary.BlobSize(var BlobHandle: IscBlobHandle; out Size: Cardinal);
  var
    BlobInfo : packed record
      Code: Char;
      Length: Word;
      Value: Cardinal;
      reserved: Word; // alignement (8)
    end;
  begin
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 1,
      isc_info_blob_total_length, SizeOf(BlobInfo), @BlobInfo));
    Size := BlobInfo.Value;
  end;

  procedure TUIBLibrary.BlobMaxSegment(var BlobHandle: IscBlobHandle; out Size: Cardinal);
  var BlobInfo: array[0..1] of TBlobInfo;
  begin
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 1,
      isc_info_blob_max_segment, SizeOf(BlobInfo), @BlobInfo));
    Size := BlobInfo[0].CardType;
  end;

  procedure TUIBLibrary.BlobInfo(var BlobHandle: IscBlobHandle; out NumSegments, MaxSegment,
    TotalLength: Cardinal; out btype : byte);
  var BlobInfos: array[0..3] of TBlobInfo;
  begin
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 4,
      isc_info_blob_num_segments + isc_info_blob_max_segment +
      isc_info_blob_total_length + isc_info_blob_type, SizeOf(BlobInfos), @BlobInfos));
    NumSegments := BlobInfos[0].CardType;
    MaxSegment  := BlobInfos[1].CardType;
    TotalLength := BlobInfos[2].CardType;
    btype       := BlobInfos[3].ByteType;
  end;

  procedure TUIBLibrary.BlobDefaultDesc(var Desc: TBlobDesc; const RelationName, FieldName: AnsiString);
  begin
    {$IFDEF IB7_UP}
      isc_blob_default_desc2(@Desc, PAnsiChar(RelationName), PAnsiChar(FieldName));
    {$ELSE}
      isc_blob_default_desc(@Desc, PAnsiChar(RelationName), PAnsiChar(FieldName));
    {$ENDIF}
  end;

  procedure TUIBLibrary.BlobSaveToStream(var BlobHandle: IscBlobHandle; Stream: TStream);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    Buffer: Pointer;
    CurrentLength: Word;
  begin
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));

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

  function TUIBLibrary.BlobReadString(var BlobHandle: IscBlobHandle): RawByteString;
  begin
    BlobReadString(BlobHandle, Result);
  end;

  procedure TUIBLibrary.BlobReadString(var BlobHandle: IscBlobHandle; var Str: RawByteString);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    Buffer: Pointer;
    Len: Integer;
  begin
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
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

  procedure TUIBLibrary.BlobReadBuffer(var BlobHandle: IscBlobHandle; var Size: Integer;
    var Buffer: Pointer; realloc: boolean);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    TMP: Pointer;
    Len: Integer;
  begin
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
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

  procedure TUIBLibrary.BlobReadSizedBuffer(var BlobHandle: IscBlobHandle;
    Buffer: Pointer);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    TMP: Pointer;
    Len: Integer;
  begin
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
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

  procedure TUIBLibrary.BlobReadSizedBuffer(var BlobHandle: IscBlobHandle;
    Buffer: Pointer; MaxSize: Integer);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    TMP: Pointer;
    Len: Integer;
  begin
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
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

  procedure TUIBLibrary.BlobReadVariant(var BlobHandle: IscBlobHandle; var Value: Variant);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    Len: Integer;
    Buffer: Pointer;
  begin
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));

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

  function TUIBLibrary.BlobCreate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
    var BlobHandle: IscBlobHandle; BPB: AnsiString = ''): TISCQuad;
  begin
    BPB := CreateBlobParams(BPB,';');
    CheckUIBApiCall(isc_create_blob2(@FStatusVector, @DBHandle, @TraHandle, @BlobHandle, @Result, Length(BPB), PAnsiChar(BPB)));
  end;

  procedure TUIBLibrary.BlobWriteSegment(var BlobHandle: IscBlobHandle; BufferLength: Cardinal; Buffer: Pointer);
  var size: Word;
  begin
    while BufferLength > 0 do
    begin
      if BufferLength > FSegmentSize then
        size := FSegmentSize else
        size := Word(BufferLength);
      CheckUIBApiCall(isc_put_segment(@FStatusVector, @BlobHandle, Size, Buffer));
      dec(BufferLength, size);
      inc(PByte(Buffer), size);
    end;
  end;

  procedure TUIBLibrary.BlobWriteString(var BlobHandle: IscBlobHandle; const Str: RawByteString);
  begin
    BlobWriteSegment(BlobHandle, Length(Str), PAnsiChar(Str));
  end;

  procedure TUIBLibrary.BlobWriteStream(var BlobHandle: IscBlobHandle; Stream: TStream);
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

  function TUIBLibrary.StreamBlobOpen(var BlobId: TISCQuad;
    var Database: IscDbHandle; var Transaction: IscTrHandle;
    Mode: AnsiChar): PBStream;
  begin
    Result := Bopen(@BlobId, @Database, @Transaction, @Mode);
  end;

  function TUIBLibrary.StreamBlobClose(Stream: PBStream): integer;
  begin
    Result := BLOB_close(Stream);
  end;

//******************************************************************************
//  Events
//******************************************************************************

  procedure TUIBLibrary.EventCancel(var DbHandle: IscDbHandle; var id: Integer);
  begin
    CheckUIBApiCall(isc_cancel_events(@FStatusVector, @DbHandle, @id));
  end;

  function TUIBLibrary.EventBlock(var EventBuffer, ResultBuffer: PAnsiChar; Count: Smallint;
    v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: PAnsiChar): Integer;
  begin
    result := isc_event_block(@EventBuffer, @ResultBuffer, Count, v1, v2,
      v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);
  end;

  procedure TUIBLibrary.EventQueue(var handle: IscDbHandle; var id: Integer; length: Word;
      events: PAnsiChar; ast: ISC_EVENT_CALLBACK; arg: Pointer);
  begin
    CheckUIBApiCall(isc_que_events(@FStatusVector, @handle, @id, length,
      events, ast, arg));
  end;

  procedure TUIBLibrary.EventCounts(var ResultVector: TStatusVector;
    BufferLength: Smallint; EventBuffer, ResultBuffer: PAnsiChar);
  begin
    isc_event_counts(@ResultVector, BufferLength, EventBuffer, ResultBuffer);
  end;

  procedure TUIBLibrary.EventWaitFor(var handle: IscDbHandle; length: Smallint;
    events, buffer: Pointer);
  begin
    CheckUIBApiCall(isc_wait_for_event(@FStatusVector, @handle, length, events, buffer));
  end;


  function TUIBLibrary.IscFree(data: Pointer): Integer;
  begin
    result := isc_free(data);
  end;

//******************************************************************************
//  Save Points
//******************************************************************************

{$IFDEF IB71_UP}
  procedure TUIBLibrary.SavepointRelease(var TrHandle: IscTrHandle;
    const Name: AnsiString);
  begin
    CheckUIBApiCall(isc_release_savepoint(@FStatusVector, @TrHandle, PAnsiChar(Name)));
  end;

  procedure TUIBLibrary.SavepointRollback(var TrHandle: IscTrHandle;
    const Name: string; Option: Word);
  begin
    CheckUIBApiCall(isc_rollback_savepoint(@FStatusVector, @TrHandle, PAnsiChar(Name), Option));
  end;

  procedure TUIBLibrary.SavepointStart(var TrHandle: IscTrHandle;
    const Name: string);
  begin
    CheckUIBApiCall(isc_start_savepoint(@FStatusVector, @TrHandle, PAnsiChar(Name)));
  end;
{$ENDIF}


  function TUIBLibrary.GetSegmentSize: Word;
  begin
    Result := FSegMentSize;
  end;

  procedure TUIBLibrary.SetSegmentSize(Value: Word);
  begin
    Assert(Value > 0);
    FSegmentSize := Value;
  end;


//******************************************************************************
// Conversion
// Making a delphi conversion will help to transport data buffer and use it
// without GDS32 ;)
//******************************************************************************

  procedure DecodeTimeStamp(v: PISCTimeStamp; out DateTime: Double);
  begin
    DateTime := v.timestamp_date - DateOffset + (v.timestamp_time / TimeCoeff);
  end;

  procedure DecodeTimeStamp(v: PISCTimeStamp; out TimeStamp: TTimeStamp);
  begin
    TimeStamp.Date := v.timestamp_date - DateOffset + 693594;
    TimeStamp.Time := v.timestamp_time div 10;
  end;

  function  DecodeTimeStamp(v: PISCTimeStamp): Double;
  begin
    DecodeTimeStamp(v, Result);
  end;

  procedure EncodeTimeStamp(const DateTime: TDateTime; v: PISCTimeStamp);
  begin
    v.timestamp_date := Round(int(DateTime)) + DateOffset;
    v.timestamp_time := ISC_TIME(Round(Abs(Frac(DateTime) * TimeCoeff)));
  end;

  procedure EncodeTimeStamp(const Date: Integer; v: PISCTimeStamp);
  begin
    v.timestamp_date := Date + DateOffset;
    v.timestamp_time := 0;
  end;

  procedure EncodeTimeStamp(const Time: Cardinal; v: PISCTimeStamp);
  begin
    v.timestamp_date := DateOffset;
    v.timestamp_time := Time;
  end;

  procedure DecodeSQLDate(v: Integer; out Year: SmallInt; out Month, Day: Word);
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

  function EncodeSQLDate(Year: Integer; Month, Day: Integer): Integer;
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

  procedure DecodeSQLTime(v: Cardinal; out Hour, Minute, Second: Word;
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

  function EncodeSQLTime(Hour, Minute, Second: Word;
    var Fractions: LongWord): Cardinal;
  begin
    Result := Cardinal((Hour * 60 + Minute) * 60 + Second) * ISC_TIME_SECONDS_PRECISION + Fractions;
  end;

 { TSQLDA }

  function TSQLDA.GetAllocatedFields: Word;
  begin
    Result := FXSQLDA.sqln;
  end;

  procedure TSQLDA.SetAllocatedFields(Fields: Word);
  begin
    if Fields <= 0 then Fields := 1;
    ReallocMem(FXSQLDA, XSQLDA_LENGTH(Fields));
    FXSQLDA.sqln := Fields;
    FXSQLDA.sqld := Fields;
    FXSQLDA.version := SQLDA_CURRENT_VERSION;
  end;

  function TSQLDA.GetSqlName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].SqlName,
      FXSQLDA.sqlvar[Index].SqlNameLength);
  end;

  function TSQLDA.GetAliasName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].AliasName,
      FXSQLDA.sqlvar[Index].AliasNameLength);
  end;

  function TSQLDA.GetOwnName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].OwnName,
      FXSQLDA.sqlvar[Index].OwnNameLength);
  end;

  function TSQLDA.GetRelName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].RelName,
      FXSQLDA.sqlvar[Index].RelNameLength);
  end;

  function TSQLDA.GetIsNull(const Index: Word): boolean;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      Result := (sqlind <> nil) and (sqlind^ = -1)
  end;

  procedure TSQLDA.CheckRange(const Index: Word);
  begin
    if Index >= Word(FXSQLDA.sqln) then
      raise Exception.CreateFmt(EUIB_FIELDNUMNOTFOUND, [index]);
  end;

  function TSQLDA.DecodeString(const Code: Smallint; Index: Word): string;
  begin
{$IFDEF UNICODE}
    DecodeStringW(Code, Index, Result);
{$ELSE}
    DecodeStringA(Code, Index, Result);
{$ENDIF}
  end;

  procedure TSQLDA.DecodeString(const Code: Smallint; Index: Word; out Str: string);
  begin
{$IFDEF UNICODE}
    DecodeStringW(Code, Index, Str);
{$ELSE}
    DecodeStringA(Code, Index, Str);
{$ENDIF}
  end;

  procedure TSQLDA.DecodeStringA(const Code: Smallint; Index: Word; out Str: AnsiString);
  begin
{$IFDEF UNICODE}
    Str := AnsiString(DecodeString(Code, Index));
{$ELSE}
    with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT    : SetString(Str, sqldata, sqllen);
      SQL_VARYING : SetString(Str, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
    end;
{$ENDIF}
  end;

  procedure TSQLDA.DecodeStringW(const Code: Smallint; Index: Word; out Str: UnicodeString);
  begin
    with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT    :
        begin
          Str := MBUDecode(Copy(sqldata, 0, sqllen), CharacterSetCP[FCharacterSet]);
          if SqlSubType > 0 then
            SetLength(Str, sqllen div BytesPerCharacter[FCharacterSet]);
        end;
      SQL_VARYING :
        Str := MBUDecode(
          Copy(PAnsiChar(@PVary(sqldata).vary_string), 0, PVary(sqldata).vary_length),
          CharacterSetCP[FCharacterSet]);
    end;
  end;

  procedure TSQLDA.EncodeStringB(Code: Smallint; Index: Word; const str: RawByteString);
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
            Move(PAnsiChar(str)^, sqldata^, sqllen);
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
            Move(PAnsiChar(str)^, PVary(sqldata).vary_string,PVary(sqldata).vary_length);
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

  procedure TSQLDA.EncodeStringA(Code: Smallint; Index: Word; const str: AnsiString);
  begin
  {$IFDEF UNICODE}
    EncodeStringB(Code, Index, MBUEncode(UniCodeString(str), CharacterSetCP[FCharacterSet]));
  {$ELSE}
    EncodeStringB(Code, Index, str);
  {$ENDIF}
  end;

  procedure TSQLDA.EncodeStringW(Code: Smallint; Index: Word; const str: UnicodeString);
  begin
    EncodeStringB(Code, Index, MBUEncode(str, CharacterSetCP[FCharacterSet]));
  end;

  procedure TSQLDA.EncodeString(Code: Smallint; Index: Word; const str: string);
  begin
  {$IFDEF UNICODE}
    EncodeStringW(Code, Index, str);
  {$ELSE}
    EncodeStringA(Code, Index, str);
  {$ENDIF}
  end;

{$IFDEF GUID_TYPE}
  procedure TSQLDA.EncodeGUID(Code: Smallint; Index: Word; const G: TGUID);
  var
    i: Smallint;
    OldLen: SmallInt;
    NewLen: Integer;
  begin
    OldLen  := FXSQLDA.sqlvar[Index].SqlLen;

    with FXSQLDA.sqlvar[Index] do
    begin
      // Guid is GuidNull
      if CompareMem(@G, @GuidNull,SizeOf(TGUID)) and (sqlind <> nil) then
        sqlind^ := -1 // NULL
      else
      begin
        case Code of
          SQL_TEXT :
            begin
            {$IFDEF GUID_AS_TEXT}
              NewLen := 38;
            {$ELSE}
              NewLen := SizeOf(TGUID);
            {$ENDIF}
              if sqldata = nil then
                getmem(sqldata, NewLen) else
                ReallocMem(sqldata, NewLen);
              sqllen := NewLen;
            {$IFDEF GUID_AS_TEXT}
              Move(PAnsiChar(AnsiString(GUIDToString(G)))^, sqldata^, sqllen);
            {$ELSE}
              Move(G, sqldata^, sqllen);
            {$ENDIF}
            end;
          SQL_VARYING :
            begin
            {$IFDEF GUID_AS_TEXT}
              NewLen := 38;
            {$ELSE}
              NewLen := SizeOf(TGUID);
            {$ENDIF}
              if sqldata = nil then
                getmem(sqldata, NewLen + 2) else
                ReallocMem(sqldata, NewLen + 2);
              sqllen := NewLen + 2;
              PVary(sqldata).vary_length := NewLen;
            {$IFDEF GUID_AS_TEXT}
              Move(PAnsiChar(AnsiString(GUIDToString(G)))^, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
            {$ELSE}
              Move(G, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
            {$ENDIF}
            end;
        end;
        if (sqlind <> nil) then
          sqlind^ := 0;
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
{$ENDIF}

  procedure TSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Int64);
  begin
    value := StrToInt64(DecodeString(Code, Index));
  end;

  procedure TSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Double);
  begin
    value := StrToFloat(DecodeString(Code, Index));
  end;

  procedure TSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Integer);
  begin
    value := StrToInt(DecodeString(Code, Index));
  end;

  procedure TSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Single);
  begin
    value := StrToFloat(DecodeString(Code, Index));
  end;

  procedure TSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Smallint);
  begin
    value := StrToInt(DecodeString(Code, Index));
  end;

  procedure TSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: TDateTime);
  begin
    value := StrToDateTime(DecodeString(Code, Index));
  end;

  procedure TSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Currency);
  begin
    value := StrToCurr(DecodeString(Code, Index));
  end;

  procedure TSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: boolean);
  begin
    value := StrToInt(DecodeString(Code, Index)) <> 0;
  end;

  procedure TSQLDA.ConvertStringToDate(const Code: Smallint; Index: Word; out value: Integer);
  begin
    Value := Trunc(StrToDate(DecodeString(Code, Index)));
  end;

  constructor TSQLDA.Create(aCharacterSet: TCharacterSet);
  begin
    FCharacterSet := aCharacterSet;
  end;

  procedure TSQLDA.DecodeStringB(const Code: Smallint; Index: Word; out Str: RawByteString);
  begin
    with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT    : SetString(Str, sqldata, sqllen);
      SQL_VARYING : SetString(Str, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
    end;
  end;

  procedure TSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Cardinal);
  begin
    value := StrToInt(DecodeString(Code, Index));
  end;

  function TSQLDA.GetFieldCount: Integer;
  begin
    Result := FXSQLDA.sqln;
  end;

  function TSQLDA.GetSQLType(const Index: Word): Smallint;
  begin
    CheckRange(Index);
    result := FXSQLDA.sqlvar[Index].sqltype and not (1);
  end;

  function TSQLDA.GetSQLLen(const Index: Word): Smallint;
  begin
    CheckRange(Index);
    result := FXSQLDA.sqlvar[Index].sqllen;
  end;

  function TSQLDA.GetSQLScale(const Index: Word): Smallint;
  begin
    CheckRange(Index);
    result := FXSQLDA.sqlvar[Index].SqlScale;
  end;

  function TSQLDA.GetIsBlob(const Index: Word): boolean;
  var
    ASQLType: Word;
  begin
    CheckRange(Index);
    ASQLType := (FXSQLDA.sqlvar[Index].sqltype and not(1));
    result := (ASQLType = SQL_BLOB) or (ASQLType = SQL_QUAD);
  end;

  function TSQLDA.GetIsBlobText(const Index: Word): boolean;
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

  function TSQLDA.GetIsArray(const Index: Word): boolean;
  begin
    CheckRange(Index);
    result := ((FXSQLDA.sqlvar[Index].sqltype and not(1)) = SQL_ARRAY);
  end;

  function TSQLDA.GetIsNullable(const Index: Word): boolean;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      Result := (sqlind <> nil) and (sqltype <> sqltype and not(1));
  end;

  function TSQLDA.GetIsNumeric(const Index: Word): boolean;
  begin
    CheckRange(Index);
    result := (FXSQLDA.sqlvar[Index].SqlScale < 0);
  end;

  // TSQLDA.GetAs...

  function TSQLDA.GetAsDouble(const Index: Word): Double;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : DecodeTimeStamp(PISCTimeStamp(sqldata), Result);
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - DateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / TimeCoeff;
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, Result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, Result);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsInt64(const Index: Word): Int64;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - DateOffset; // Only Date
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - DateOffset;
          SQL_TYPE_TIME : ; // Result := 0; What else ??
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, Result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, Result);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsInteger(const Index: Word): Integer;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - DateOffset; // Only Date
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - DateOffset;
          SQL_TYPE_TIME : ; // Result := 0; What else ??
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsSingle(const Index: Word): Single;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : Result := DecodeTimeStamp(PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - DateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / TimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsSmallint(const Index: Word): Smallint;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - DateOffset; // Only Date
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - DateOffset;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
          SQL_TYPE_TIME : ; // Result := 0; What else ??
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsString(const Index: Word): string;
  begin
{$IFDEF UNICODE}
    Result := GetAsUnicodeString(Index)
{$ELSE}
    Result := GetAsAnsiString(Index)
{$ENDIF}
  end;

  function TSQLDA.GetAsAnsiString(const Index: Word): AnsiString;
    function BoolToStr(const Value: boolean): string;
    begin if Value then result := sUIBTrue else result := sUIBFalse; end;
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
          SQL_SHORT  : Result := AnsiString(FormatFloat(ScaleFormat[sqlscale], PSmallInt(sqldata)^ / ScaleDivisor[sqlscale]));
          SQL_LONG   : Result := AnsiString(FormatFloat(ScaleFormat[sqlscale], PInteger(sqldata)^  / ScaleDivisor[sqlscale]));
          SQL_INT64,
          SQL_QUAD   : Result := AnsiString(FormatFloat(ScaleFormat[sqlscale], PInt64(sqldata)^    / ScaleDivisor[sqlscale]));
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := AnsiString(FormatFloat(ScaleFormat[sqlscale], PDouble(sqldata)^));
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_VARYING   : DecodeStringA(SQL_VARYING, Index, Result);
          SQL_TEXT      : DecodeStringA(SQL_TEXT, Index, Result);
          SQL_TIMESTAMP : Result := AnsiString(DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(sqldata))));
          SQL_TYPE_DATE : Result := AnsiString(DateToStr(PInteger(sqldata)^ - DateOffset));
          SQL_TYPE_TIME : Result := AnsiString(TimeToStr(PCardinal(sqldata)^ / TimeCoeff));
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := AnsiString(FloatToStr(PDouble(sqldata)^));
          SQL_LONG      : Result := AnsiString(IntToStr(PInteger(sqldata)^));
          SQL_FLOAT     : Result := AnsiString(FloatToStr(PSingle(sqldata)^));
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := AnsiString(BoolToStr(PSmallint(sqldata)^ = 1));
{$ENDIF}
          SQL_SHORT     : Result := AnsiString(IntToStr(PSmallint(sqldata)^));
          SQL_INT64     : Result := AnsiString(IntToStr(PInt64(sqldata)^));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsQuad(const Index: Word): TISCQuad;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      if not ((sqlind <> nil) and (sqlind^ = -1)) then
        case (sqltype and not(1)) of
          SQL_QUAD, SQL_DOUBLE, SQL_D_FLOAT, SQL_INT64, SQL_BLOB, SQL_ARRAY: result := PISCQuad(sqldata)^;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end
      else
        Result := QuadNull;
  end;

  function TSQLDA.GetAsRawByteString(const Index: Word): RawByteString;
    function BoolToStr(const Value: boolean): string;
    begin if Value then result := sUIBTrue else result := sUIBFalse; end;
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
          SQL_SHORT  : Result := RawByteString(FormatFloat(ScaleFormat[sqlscale], PSmallInt(sqldata)^ / ScaleDivisor[sqlscale]));
          SQL_LONG   : Result := RawByteString(FormatFloat(ScaleFormat[sqlscale], PInteger(sqldata)^  / ScaleDivisor[sqlscale]));
          SQL_INT64,
          SQL_QUAD   : Result := RawByteString(FormatFloat(ScaleFormat[sqlscale], PInt64(sqldata)^    / ScaleDivisor[sqlscale]));
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := RawByteString(FormatFloat(ScaleFormat[sqlscale], PDouble(sqldata)^));
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_VARYING   : DecodeStringB(SQL_VARYING, Index, Result);
          SQL_TEXT      : DecodeStringB(SQL_TEXT, Index, Result);
          SQL_TIMESTAMP : Result := RawByteString(DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(sqldata))));
          SQL_TYPE_DATE : Result := RawByteString(DateToStr(PInteger(sqldata)^ - DateOffset));
          SQL_TYPE_TIME : Result := RawByteString(TimeToStr(PCardinal(sqldata)^ / TimeCoeff));
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := RawByteString(FloatToStr(PDouble(sqldata)^));
          SQL_LONG      : Result := RawByteString(IntToStr(PInteger(sqldata)^));
          SQL_FLOAT     : Result := RawByteString(FloatToStr(PSingle(sqldata)^));
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := RawByteString(BoolToStr(PSmallint(sqldata)^ = 1));
{$ENDIF}
          SQL_SHORT     : Result := RawByteString(IntToStr(PSmallint(sqldata)^));
          SQL_INT64     : Result := RawByteString(IntToStr(PInt64(sqldata)^));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsVariant(const Index: Word): Variant;
  var
    ASQLCode: SmallInt;
    Dbl: Double;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := NULL;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : Result := TDateTime(DecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE :
            begin
              Dbl := PInteger(sqldata)^ - DateOffset;
              Result := TDateTime(Dbl);
            end;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / TimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_FLOAT     : Result := PSingle(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^ = 1;
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
{$IFDEF COMPILER6_UP}
          SQL_INT64     : Result := PInt64(sqldata)^;
{$ELSE}
  {$IFDEF FPC}
          SQL_INT64     : Result := PInt64(sqldata)^;
  {$ELSE}
          SQL_INT64     : Result := Integer(PInt64(sqldata)^);
  {$ENDIF}
{$ENDIF}
          SQL_TEXT      : Result := DecodeString(SQL_TEXT, Index);
          SQL_VARYING   : Result := DecodeString(SQL_VARYING, Index);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

{$IFDEF DELPHI14_UP}
  function TSQLDA.GetAsTValue(const Index: Word): TValue;
  var
    ASQLCode: SmallInt;
    Dbl: Double;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      if (sqlind <> nil) and (sqlind^ = -1) then
        Exit(TValue.Empty);
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   :
            if (SqlScale = -4) then
              TValue.Make(@sqldata, TypeInfo(Currency), Result) else
              Result := PInt64(sqldata)^ / ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : Result := TDateTime(DecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE :
            begin
              Dbl := PInteger(sqldata)^ - DateOffset;
              Result := TDateTime(Dbl);
            end;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / TimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_FLOAT     : Result := PSingle(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^ = 1;
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
{$IFDEF COMPILER6_UP}
          SQL_INT64     : Result := PInt64(sqldata)^;
{$ELSE}
  {$IFDEF FPC}
          SQL_INT64     : Result := PInt64(sqldata)^;
  {$ELSE}
          SQL_INT64     : Result := Integer(PInt64(sqldata)^);
  {$ENDIF}
{$ENDIF}
          SQL_TEXT      : Result := DecodeString(SQL_TEXT, Index);
          SQL_VARYING   : Result := DecodeString(SQL_VARYING, Index);
          SQL_BLOB      :
            if SqlSubType = 1 then
              Result := GetAsString(Index) else
              Result := TValue.From<RawByteString>(GetAsRawByteString(Index));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetByNameAsTValue(const Name: string): TValue;
  begin
    Result := GetAsTValue(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetAsType<T>(var ctx: TRttiContext): T;
  var
    v: TValue;
    p: PTypeInfo;
    r: TRttiType;
    f: TRttiField;
    o: TObject;
    str: string;
  begin
    p := TypeInfo(T);
    if p <> nil then
    case p.Kind of
      tkRecord:
        begin
          TValue.Make(@Result, p, v);
          r := ctx.GetType(p);
          for f in r.GetFields do
            f.SetValue(@Result, GetByNameAsTValue(f.Name));
        end;
      tkClass:
        begin
          o := GetTypeData(p).ClassType.Create;
          r := ctx.GetType(p);
          for f in r.GetFields do
            f.SetValue(o, GetByNameAsTValue(f.Name));
          PPointer(@Result)^ := o;
        end;
      tkInteger .. tkSet, tkWChar..tkVariant, tkInt64, tkUString:
        Result := GetAsTValue(0).AsType<T>;
    else
      raise EUIBException.Create('Invalid data type');
    end;
  end;

{$ENDIF}

  function TSQLDA.GetAsDateTime(const Index: Word): TDateTime;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TIMESTAMP : DecodeTimeStamp(PISCTimeStamp(sqldata), Double(Result));
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - DateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / TimeCoeff;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsCurrency(const Index: Word): Currency;
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
                          PInt64(@Result)^ := PInt64(sqldata)^ * CurrencyDivisor[SqlScale] else
                          PInt64(@Result)^ := PInt64(sqldata)^ div CurrencyDivisor[SqlScale];
          SQL_LONG   : if (SqlScale = -4) then
                        PInt64(@Result)^ := PInteger(sqldata)^ else
                        if SqlScale > -14 then
                        begin
                          if SqlScale > -4 then
                            PInt64(@Result)^ := PInteger(sqldata)^ * Integer(CurrencyDivisor[SqlScale]) else
                            PInt64(@Result)^ := PInteger(sqldata)^ div Integer(CurrencyDivisor[SqlScale]);
                        end else
                        begin
                          if SqlScale > -4 then
                            PInt64(@Result)^ := PInteger(sqldata)^ * CurrencyDivisor[SqlScale] else
                            PInt64(@Result)^ := PInteger(sqldata)^ div CurrencyDivisor[SqlScale];
                        end;
          SQL_SHORT  : if (SqlScale = -4) then
                        PInt64(@Result)^ := PSmallint(sqldata)^ else
                        if SqlScale > -14 then
                        begin
                          if SqlScale > -4 then
                            PInt64(@Result)^ := PSmallint(sqldata)^ * Integer(CurrencyDivisor[SqlScale]) else
                            PInt64(@Result)^ := PSmallint(sqldata)^ div Integer(CurrencyDivisor[SqlScale]);
                        end else
                        begin
                          if SqlScale > -4 then
                            PInt64(@Result)^ := PSmallint(sqldata)^ * CurrencyDivisor[SqlScale] else
                            PInt64(@Result)^ := PSmallint(sqldata)^ div CurrencyDivisor[SqlScale];
                        end;
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_TIMESTAMP : Result := DecodeTimeStamp(PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - DateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / TimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
end;

  function TSQLDA.GetAsBoolean(const Index: Word): boolean;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div ScaleDivisor[sqlscale] <> 0;
          SQL_LONG   : Result := PInteger(sqldata)^  div ScaleDivisor[sqlscale] <> 0;
          SQL_INT64,
          SQL_QUAD   : Result := (PInt64(sqldata)^ div ScaleDivisor[sqlscale]) <> 0;
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^) > 0;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^ <> 0;
          SQL_LONG      : Result := PInteger(sqldata)^ <> 0;
          SQL_INT64     : Result := PInt64(sqldata)^ <> 0;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^) <> 0;
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^) <> 0;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsUnicodeString(const Index: Word): UnicodeString;
    function BoolToStr(const Value: boolean): string;
    begin if Value then result := sUIBTrue else result := sUIBFalse; end;
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
          SQL_SHORT  : Result := FormatFloat(ScaleFormat[sqlscale], PSmallInt(sqldata)^ / ScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FormatFloat(ScaleFormat[sqlscale], PInteger(sqldata)^  / ScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FormatFloat(ScaleFormat[sqlscale], PInt64(sqldata)^    / ScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := FormatFloat(ScaleFormat[sqlscale], PDouble(sqldata)^);
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TEXT      : DecodeStringW(SQL_TEXT, Index, Result);
          SQL_VARYING   : DecodeStringW(SQL_VARYING, Index, Result);
          SQL_TIMESTAMP : Result := DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE : Result := DateToStr(PInteger(sqldata)^ - DateOffset);
          SQL_TYPE_TIME : Result := TimeToStr(PCardinal(sqldata)^ / TimeCoeff);
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
          SQL_LONG      : Result := IntToStr(PInteger(sqldata)^);
          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := BoolToStr(PSmallint(sqldata)^ = 1);
{$ENDIF}
          SQL_SHORT     : Result := IntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToStr(PInt64(sqldata)^);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsDate(const Index: Word): Integer;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - DateOffset;
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - DateOffset;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_TEXT      : ConvertStringToDate(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertStringToDate(SQL_VARYING, Index, result);
          SQL_TYPE_TIME : Result := 0;
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLDA.GetAsTime(const Index: Word): Cardinal;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^ div ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^;
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_time;
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
        {$IFDEF IB7_UP}
          SQL_BOOLEAN,
        {$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
          SQL_TYPE_DATE : Result := 0;
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

{$IFDEF GUID_TYPE}
  function TSQLDA.GetAsGUID(const Index: Word): TGUID;
  var ASQLCode: SmallInt;
  begin
    CheckRange(Index);
    Result := GuidNull;
    with FXSQLDA.sqlvar[Index] do
    begin
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // GUID can't be converted from numeric fields
      if (sqlscale < 0)  then
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR)
      else
        case ASQLCode of
          SQL_TEXT    :
          begin
          {$IFDEF GUID_AS_TEXT}
            if sqllen = 38 then
              Result := StringToGUID(DecodeString(SQL_TEXT, Index))
          {$ELSE}
            if sqllen = SizeOf(TGUID) then
              Move(sqldata^, Result, sqllen)
          {$ENDIF}
            else
              raise EUIBConvertError.Create(EUIB_CASTERROR);
          end;
          SQL_VARYING :
          begin
          {$IFDEF GUID_AS_TEXT}
            if PVary(sqldata).vary_length  = 38 then
              Result := StringToGUID(DecodeString(SQL_VARYING, Index))
          {$ELSE}
            if PVary(sqldata).vary_length  = SizeOf(TGUID) then
              Move(PVary(sqldata).vary_string, Result, PVary(sqldata).vary_length)
          {$ENDIF}
            else
              raise EUIBConvertError.Create(EUIB_CASTERROR);
          end;
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;
{$ENDIF}

 // TSQLDA.SetAs...

  procedure TSQLDA.SetIsNull(const Index: Word; const Value: boolean);
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      if (sqlind <> nil) then
        case Value of
          True : sqlind^ := -1;
          False: sqlind^ :=  0;
        end;
  end;

  procedure TSQLDA.SetAsQuad(const Index: Word; const Value: TISCQuad);
  begin
    with FXSQLDA.sqlvar[Index] do
      begin
        case (sqltype and not(1)) of
          SQL_QUAD, SQL_DOUBLE, SQL_D_FLOAT, SQL_INT64, SQL_BLOB, SQL_ARRAY: PISCQuad(sqldata)^ := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
        if (sqlind <> nil) then
          if CompareMem(@Value, @QuadNull, SizeOf(TIscQuad)) then
            sqlind^ := -1 else
            sqlind^ := 0;
      end;
  end;

  procedure TSQLDA.SetAsRawByteString(const Index: Word; const Value: RawByteString);
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
          SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(StrToFloat(string(Value)) * ScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Trunc(StrToFloat(string(Value)) * ScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Trunc(StrToFloat(string(Value)) * ScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := StrToFloat(string(Value));
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TEXT      : EncodeStringB(SQL_TEXT, Index, Value);
          SQL_VARYING   : EncodeStringB(SQL_VARYING, Index, Value);
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := StrToFloat(string(Value));
          SQL_TIMESTAMP : EncodeTimeStamp(StrToDateTime(string(Value)), PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(StrToDate(string(Value))) + dateoffset);
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(StrToTime(string(Value))) * TimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := StrToInt(string(Value));
          SQL_FLOAT     : PSingle(sqldata)^ := StrToFloat(string(Value));
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := StrToInt(string(Value));
          SQL_INT64     : PInt64(sqldata)^ := StrToInt64(string(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then
          sqlind^ := 0;
    end;
  end;

  procedure TSQLDA.SetAsDateTime(const Index: Word; const Value: TDateTime);
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
          SQL_SHORT  : PSmallInt(sqldata)^ := Round(Value * ScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Round(Value * ScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Round(Value * ScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : EncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value)) + DateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * TimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, DateTimeToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, DateTimeToStr(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsDate(const Index: Word; const Value: Integer);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * ScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : EncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + DateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, DateToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, DateToStr(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsTime(const Index: Word; const Value: Cardinal);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * ScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : EncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := 0;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Value;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, TimeToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, TimeToStr(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsBoolean(const Index: Word; const Value: Boolean);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := ord(Value) * ScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := ord(Value) * ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := ord(Value) * ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := ord(Value);
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := ord(Value);
          SQL_LONG      : PInteger(sqldata)^ := ord(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := ord(Value);
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := ord(Value);
          SQL_INT64     : PInt64(sqldata)^ := ord(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(ord(Value)));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(ord(Value)));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsInteger(const Index: Word; const Value: Integer);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * ScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : EncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + DateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsSingle(const Index: Word; const Value: Single);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Round(Value * ScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Round(Value * ScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Round(Value * ScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : EncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value)) + DateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * TimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsSmallint(const Index: Word; const Value: Smallint);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * ScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : EncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + DateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsString(const Index: Word; const Value: string);
  begin
{$IFDEF UNICODE}
    SetAsUnicodeString(Index, Value);
{$ELSE}
    SetAsAnsiString(Index, Value);
{$ENDIF}
  end;

  procedure TSQLDA.SetAsAnsiString(const Index: Word; const Value: AnsiString);
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
          SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(StrToFloat(string(Value)) * ScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Trunc(StrToFloat(string(Value)) * ScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Trunc(StrToFloat(string(Value)) * ScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := StrToFloat(string(Value));
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TEXT      : EncodeStringA(SQL_TEXT, Index, Value);
          SQL_VARYING   : EncodeStringA(SQL_VARYING, Index, Value);
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := StrToFloat(string(Value));
          SQL_TIMESTAMP : EncodeTimeStamp(StrToDateTime(string(Value)), PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(StrToDate(string(Value))) + dateoffset);
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(StrToTime(string(Value))) * TimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := StrToInt(string(Value));
          SQL_FLOAT     : PSingle(sqldata)^ := StrToFloat(string(Value));
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := StrToInt(string(Value));
          SQL_INT64     : PInt64(sqldata)^ := StrToInt64(string(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then
          sqlind^ := 0;
    end;
  end;

  procedure TSQLDA.SetAsUnicodeString(const Index: Word;
    const Value: UnicodeString);
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
          SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(StrToFloat(Value) * ScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Trunc(StrToFloat(Value) * ScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Trunc(StrToFloat(Value) * ScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := StrToFloat(Value);
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TEXT      : EncodeStringW(SQL_TEXT, Index, Value);
          SQL_VARYING   : EncodeStringW(SQL_VARYING, Index, Value);
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := StrToFloat(Value);
          SQL_TIMESTAMP : EncodeTimeStamp(StrToDateTime(Value), PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(StrToDate(Value)) + DateOffset);
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(StrToTime(Value)) * TimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := StrToInt(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := StrToFloat(Value);
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := StrToInt(Value);
          SQL_INT64     : PInt64(sqldata)^ := StrToInt64(Value);
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then
          sqlind^ := 0;
    end;
  end;

  procedure TSQLDA.SetAsInt64(const Index: Word; const Value: Int64);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Value * ScaleDivisor[sqlscale];
          SQL_LONG   : PInteger(sqldata)^  := Value * ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Value * ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : EncodeTimeStamp(Integer(Value), PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + DateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsDouble(const Index: Word; const Value: Double);
  var ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : PSmallInt(sqldata)^ := Round(Value * ScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Round(Value * ScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Round(Value * ScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : EncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value)) + DateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * TimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsCurrency(const Index: Word;
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
          SQL_SHORT  : PSmallInt(sqldata)^ := Round(Value * ScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Round(Value * ScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : if (sqlscale = -4) then
                         PInt64(sqldata)^ := PInt64(@Value)^ else
                         if sqlscale > -4 then
                           PInt64(sqldata)^ := PInt64(@Value)^ div CurrencyDivisor[sqlscale] else
                           PInt64(sqldata)^ := PInt64(@Value)^ * CurrencyDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : EncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value) + DateOffset);
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * TimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
{$IFDEF IB7_UP}
          SQL_BOOLEAN,
{$ENDIF}
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
{$IFDEF FB25_UP}
          SQL_NULL: ;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TSQLDA.SetAsVariant(const Index: Word; const Value: Variant);
  begin
    case TVarData(Value).VType of
{$IFNDEF COMPILER5}
      varShortInt,
{$ENDIF}
      varSmallInt, varByte: SetAsSmallint(Index, Value);
{$IFNDEF COMPILER5}
      varWord,
{$ENDIF}
      varInteger:               SetAsInteger(Index, Value);
{$IFNDEF COMPILER5}
      varLongWord, varInt64:             SetAsInt64(Index, Value);
{$ENDIF}
      varSingle:                         SetAsSingle(Index, Value);
      varDouble:                         SetAsDouble(Index, Value);
      varCurrency:                       SetAsCurrency(Index, Value);
      varDate:                           SetAsDateTime(Index, Value);
      varOleStr, varString:              SetAsString(Index, Value);
{$IFDEF UNICODE}
      varUString:                        SetAsString(Index, Value);
{$ENDIF}
{$IFDEF IB7_UP}
      varBoolean:                        SetAsBoolean(Index, Value);
{$ENDIF}
      varNull, VarEmpty:                 SetIsNull(index, true);
    else
      raise EUIBConvertError.Create(EUIB_CASTERROR);
    end;
  end;

{$IFDEF GUID_TYPE}
  procedure TSQLDA.SetAsGUID(const Index: Word; const Value: TGUID);
  var
    ASQLCode: SmallInt;
  begin
    with FXSQLDA.sqlvar[Index] do
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR)
      else
        case ASQLCode of
          SQL_TEXT      : EncodeGUID(SQL_TEXT, Index, Value);
          SQL_VARYING   : EncodeGUID(SQL_VARYING, Index, Value);
{$IFDEF FB25_UP}
          SQL_NULL: IsNull[index] := False;
{$ENDIF}
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;
{$ENDIF}

  // TSQLDA.GetByName...

  function TSQLDA.GetFieldIndex(const name: AnsiString): Word;
  begin
    if not TryGetFieldIndex(name, Result) then
      raise Exception.CreateFmt(EUIB_FIELDSTRNOTFOUND, [name]);
  end;

  function TSQLDA.TryGetFieldIndex(const name: AnsiString; out index: Word): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to GetAllocatedFields - 1 do
      if FXSQLDA.sqlvar[i].AliasNameLength = Length(name) then
        if StrLIComp(PansiChar(@FXSQLDA.sqlvar[i].aliasname), PAnsiChar(Name),
          FXSQLDA.sqlvar[i].AliasNameLength) = 0 then
          begin
            index := i;
            Result := True;
            Exit;
          end;
    Result := False;
  end;

  function TSQLDA.GetByNameAsDouble(const Name: string): Double;
  begin
    Result := GetAsDouble(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsInt64(const Name: string): Int64;
  begin
    Result := GetAsInt64(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsInteger(const Name: string): Integer;
  begin
    Result := GetAsInteger(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsSingle(const Name: string): Single;
  begin
    Result := GetAsSingle(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsSmallint(const Name: string): Smallint;
  begin
    Result := GetAsSmallint(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsString(const name: string): string;
  begin
  {$IFDEF UNICODE}
    Result := GetByNameAsUnicodeString(name);
  {$ELSE}
    Result := GetByNameAsAnsiString(name);
  {$ENDIF}
  end;

  function TSQLDA.GetByNameAsAnsiString(const Name: string): AnsiString;
  begin
    Result := GetAsAnsiString(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsQuad(const Name: string): TISCQuad;
  begin
    Result := GetAsQuad(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsRawByteString(const name: string): RawByteString;
  begin
    Result := GetAsRawByteString(GetFieldIndex(AnsiString(name)));
  end;

  function TSQLDA.GetByNameAsVariant(const Name: string): Variant;
  begin
    Result := GetAsVariant(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameIsBlob(const Name: string): boolean;
  begin
    Result := GetIsBlob(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameIsBlobText(const Name: string): boolean;
  begin
    Result := GetIsBlobText(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameIsNull(const Name: string): boolean;
  begin
    Result := GetIsNull(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsDateTime(const Name: string): TDateTime;
  begin
    Result := GetAsDateTime(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsCurrency(const Name: string): Currency;
  begin
    Result := GetAsCurrency(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsBoolean(const Name: string): boolean;
  begin
    Result := GetAsBoolean(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsUnicodeString(const Name: string): UnicodeString;
  begin
    Result := GetAsUnicodeString(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsDate(const Name: string): Integer;
  begin
    Result := GetAsDate(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameAsTime(const Name: string): Cardinal;
  begin
    Result := GetAsTime(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameIsNumeric(const Name: string): boolean;
  begin
    result := GetIsNumeric(GetFieldIndex(AnsiString(Name)));
  end;

  function TSQLDA.GetByNameIsNullable(const Name: string): boolean;
  begin
    Result := GetIsNullable(GetFieldIndex(AnsiString(Name)));
  end;

{$IFDEF GUID_TYPE}
  function TSQLDA.GetByNameAsGUID(const Name: string): TGUID;
  begin
    result := GetAsGUID(GetFieldIndex(AnsiString(Name)));
  end;
{$ENDIF}

  // TSQLDA.SetByNameAs

  procedure TSQLDA.SetByNameIsNull(const Name: string;
    const Value: boolean);
  begin
    SetIsNull(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsBoolean(const Name: string;
    const Value: boolean);
  begin
     SetAsBoolean(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsDate(const Name: string;
    const Value: Integer);
  begin
    SetAsDate(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsCurrency(const Name: string;
    const Value: Currency);
  begin
    SetAsCurrency(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsDateTime(const Name: string;
    const Value: TDateTime);
  begin
    SetAsDateTime(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsDouble(const Name: string;
    const Value: Double);
  begin
    SetAsDouble(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsInt64(const Name: string;
    const Value: Int64);
  begin
    SetAsInt64(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsInteger(const Name: string;
    const Value: Integer);
  begin
    SetAsInteger(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsQuad(const Name: string;
    const Value: TISCQuad);
  begin
    SetAsQuad(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsRawByteString(const name: string; const Value: RawByteString);
  begin
    SetAsRawByteString(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsSingle(const Name: string; const Value: Single);
  begin
    SetAsSingle(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsSmallint(const Name: string;
    const Value: Smallint);
  begin
    SetAsSmallint(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsString(const name, Value: string);
  begin
  {$IFDEF UNICODE}
    SetByNameAsUnicodeString(name, Value);
  {$ELSE}
    SetByNameAsAnsiString(name, Value);;
  {$ENDIF}
  end;

  procedure TSQLDA.SetByNameAsAnsiString(const Name: string; const Value: AnsiString);
  begin
    SetAsAnsiString(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsUnicodeString(const Name: string; const Value: UnicodeString);
  begin
    SetAsUnicodeString(GetFieldIndex(AnsiString(Name)), Value);
  end;

  procedure TSQLDA.SetByNameAsVariant(const Name: string;
    const Value: Variant);
  begin
    SetAsVariant(GetFieldIndex(AnsiString(Name)), Value);
  end;

{$IFDEF GUID_TYPE}
  procedure TSQLDA.SetByNameAsGUID(const Name: string; const Value: TGUID);
  begin
    SetAsGUID(GetFieldIndex(AnsiString(Name)), Value);
  end;
{$ENDIF}

  function TSQLDA.GetFieldType(const Index: Word): TUIBFieldType;
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
    {$IFDEF IB7_UP}
      SQL_BOOLEAN     : Result := uftBoolean;
    {$ENDIF}
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
{$IFDEF FB25_UP}
      SQL_NULL        : Result := uftNull;
{$ENDIF}
    else
      Result := uftUnKnown;
    end;
  end;

{ TSQLResult }

  constructor TSQLResult.Create(Charset: TCharacterSet; Fields: SmallInt = 0;
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

  destructor TSQLResult.Destroy;
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

  procedure TSQLResult.AddCurrentRecord;
  begin
    if FRecordPool = nil then
      FRecordPool := TPoolStream.Create(FBufferChunks, FDataBufferLength);
    Move(FDataBuffer^, FRecordPool.Add^, FDataBufferLength);
    FCurrentRecord := FRecordPool.ItemCount - 1;
  end;

  procedure TSQLResult.ClearRecords;
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

  procedure TSQLResult.GetRecord(const Index: Integer);
  begin
    if (Index <> FCurrentRecord) and (FRecordPool <> nil) then
    begin
      Move(FRecordPool[Index]^, FDataBuffer^, FDataBufferLength);
      FCurrentRecord := Index;
    end;
    FInMemoryEOF := false;
  end;

  function TSQLResult.GetRecordCount: Integer;
  begin
    if Assigned(FRecordPool) then
      Result := FRecordPool.ItemCount else
      Result := 0;
  end;

  procedure TSQLResult.AllocateDataBuffer;
  var
    i, j, LastLen: SmallInt;
    BlobCount: Word;

    ArrayIndex: integer;
    ArrayItemLen: integer;
    ArrayItemCount: integer;
    PDesc: PArrayInfo;
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
            LastLen := FXSQLDA.sqlvar[i].sqllen + SizeOf(TBlobData); // quad + datainfo
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
  procedure TSQLResult.SaveToStream(Stream: TStream);
  var
    Count, i, j: Integer;
    BlobData: PBlobData;
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
{$IFDEF IB7_UP}
        Stream.Write(SqlPrecision, sizeof(SqlPrecision));
{$ENDIF}
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
  procedure TSQLResult.LoadFromStream(Stream: TStream);
  var
    Fields: SmallInt;
    Count, i, j: Integer;
    BlobData: PBlobData;
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
{$IFDEF IB7_UP}
        Stream.Read(SqlPrecision, sizeof(SqlPrecision));
{$ENDIF}
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

  function TSQLResult.GetCurrentRecord: Integer;
  begin
    if (FRecordPool = nil) then
      Result := -1 else
      Result := FCurrentRecord;
  end;

  procedure TSQLResult.FreeBlobs(Buffer: Pointer);
  var
    BlobData: PBlobData;
    I: integer;
  begin
    for I := 0 to Length(FBlobsIndex) - 1 do
    begin
      BlobData := PBlobData(PtrInt(Buffer) + (PtrInt(GetDataQuadOffset(FBlobsIndex[I])) - PtrInt(FDataBuffer)));
      if BlobData.Size > 0 then
        FreeMem(BlobData.Buffer);
    end;
  end;

  function TSQLResult.GetEof: boolean;
  begin
    Result := FScrollEOF and (
       (not CachedFetch) or
       (RecordCount = 0) or
       FInMemoryEOF);
  end;

  function TSQLResult.GetBof: boolean;
  begin
    Result := (FCurrentRecord = 0) or (RecordCount = 0);
  end;

  procedure TSQLResult.ReadBlobA(const Index: Word; var str: AnsiString);
{$IFDEF UNICODE}
  var
    data: UnicodeString;
{$ENDIF}
  begin
{$IFDEF UNICODE}
    ReadBlobW(Index, data);
    str := AnsiString(data);
{$ELSE}
    ReadBlobB(Index, str);
{$ENDIF}
  end;

  procedure TSQLResult.ReadBlob(const Index: Word; Stream: TStream);
  var BlobData: PBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(EUIB_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    Stream.Seek(0, 0);
    Stream.Write(BlobData.Buffer^, BlobData.Size);
    Stream.Seek(0, 0);
  end;

  procedure TSQLResult.ReadBlob(const Index: Word; var Value: Variant);
  var
    PData: Pointer;
    BlobData: PBlobData;
    str: string;
  begin
    if IsBlobText[Index] then
    begin
      ReadBlob(Index, str);
      Value := str;
    end else
    begin
      if not FFetchBlobs then
        raise Exception.Create(EUIB_FETCHBLOBNOTSET);
      BlobData := GetDataQuadOffset(Index);
      Value := VarArrayCreate([0, BlobData.Size-1], varByte);
      PData := VarArrayLock(Value);
      try
        Move(BlobData.Buffer^, PData^, BlobData.Size);
      finally
        VarArrayUnlock(Value);
      end;
    end;
  end;

  procedure TSQLResult.ReadBlobW(const Index: Word; var str: UnicodeString);
  var
    aStr: RawByteString;
  begin
    ReadBlobB(Index, aStr);
    if FXSQLDA.sqlvar[Index].SqlSubType = 1 then  // is text ?
      str := MBUDecode(aStr, CharacterSetCP[FCharacterSet]) else
      begin
        SetLength(str, Length(aStr) div 2);
        Move(PByte(aStr)^, PByte(str)^, Length(str) * 2);
      end;
  end;

  procedure TSQLResult.ReadBlob(const Index: Word; var str: string);
  begin
  {$IFDEF UNICODE}
     ReadBlobW(Index, str);
  {$ELSE}
     ReadBlobA(Index, str);
  {$ENDIF}
  end;

  procedure TSQLResult.ReadBlob(const Index: Word; Data: Pointer);
  var BlobData: PBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(EUIB_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    Move(BlobData.Buffer^, Data^, BlobData.Size);
  end;

  procedure TSQLResult.ReadBlob(const name: string; Data: Pointer);
  begin
    ReadBlob(GetFieldIndex(AnsiString(Name)), Data);
  end;

  function TSQLResult.ReadBlob(const Index: Word): string;
  begin
    ReadBlob(Index, Result);
  end;

  function TSQLResult.ReadBlob(const name: string): string;
  begin
    ReadBlob(name, Result);
  end;

  procedure TSQLResult.ReadBlobB(const Index: Word; var data: RawByteString);
  var
    BlobData: PBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(EUIB_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    SetLength(data, BlobData.Size);
    Move(BlobData.Buffer^, PAnsiChar(data)^, BlobData.Size);
  end;

  procedure TSQLResult.ReadBlobW(const name: string; var str: UnicodeString);
  begin
    ReadBlobW(GetFieldIndex(AnsiString(Name)), str);
  end;

  procedure TSQLResult.ReadBlob(const name: string; var str: string);
  begin
  {$IFDEF UNICODE}
     ReadBlobW(name, str);
  {$ELSE}
     ReadBlobA(name, str);
  {$ENDIF}
  end;


  procedure TSQLResult.ReadBlobA(const name: string; var str: AnsiString);
  begin
    ReadBlobA(GetFieldIndex(AnsiString(Name)), str);
  end;

  procedure TSQLResult.ReadBlobB(const name: string; var data: RawByteString);
  begin
    ReadBlobB(GetFieldIndex(AnsiString(Name)), data);
  end;

  procedure TSQLResult.ReadBlob(const name: string; Stream: TStream);
  begin
    ReadBlob(GetFieldIndex(AnsiString(Name)), Stream);
  end;

  procedure TSQLResult.ReadBlob(const name: string; var Value: Variant);
  begin
    ReadBlob(GetFieldIndex(AnsiString(Name)), Value);
  end;

  function TSQLResult.GetBlobSize(const Index: Word): Cardinal;
  var BlobData: PBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(EUIB_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    Result := BlobData.Size;
  end;

  function TSQLResult.GetAsAnsiString(const Index: Word): AnsiString;
    function BoolToStr(const Value: boolean): string;
    begin if Value then result := sUIBTrue else result := sUIBFalse; end;
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
          SQL_SHORT  : Result := AnsiString(FormatFloat(ScaleFormat[sqlscale], PSmallInt(sqldata)^ / ScaleDivisor[sqlscale]));
          SQL_LONG   : Result := AnsiString(FormatFloat(ScaleFormat[sqlscale], PInteger(sqldata)^  / ScaleDivisor[sqlscale]));
          SQL_INT64,
          SQL_QUAD   : Result := AnsiString(FormatFloat(ScaleFormat[sqlscale], PInt64(sqldata)^    / ScaleDivisor[sqlscale]));
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := AnsiString(FormatFloat(ScaleFormat[sqlscale], PDouble(sqldata)^));
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := AnsiString(FloatToStr(PDouble(sqldata)^));
          SQL_TIMESTAMP : Result := AnsiString(DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(sqldata))));
          SQL_TYPE_DATE : Result := AnsiString(DateToStr(PInteger(sqldata)^ - DateOffset));
          SQL_TYPE_TIME : Result := AnsiString(TimeToStr(PCardinal(sqldata)^ / TimeCoeff));
          SQL_LONG      : Result := AnsiString(IntToStr(PInteger(sqldata)^));
          SQL_FLOAT     : Result := AnsiString(FloatToStr(PSingle(sqldata)^));
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := AnsiString(BoolToStr(PSmallint(sqldata)^ = 1));
{$ENDIF}
          SQL_SHORT     : Result := AnsiString(IntToStr(PSmallint(sqldata)^));
          SQL_INT64     : Result := AnsiString(IntToStr(PInt64(sqldata)^));
          SQL_TEXT      : DecodeStringA(SQL_TEXT, Index, Result);
          SQL_VARYING   : DecodeStringA(SQL_VARYING, Index, Result);
          SQL_BLOB      : ReadBlobA(Index, Result);
          SQL_ARRAY     : Result := '(Array)';
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLResult.GetAsRawByteString(const Index: Word): RawByteString;
    function BoolToStr(const Value: boolean): string;
    begin if Value then result := sUIBTrue else result := sUIBFalse; end;
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
          SQL_SHORT  : Result := RawByteString(FormatFloat(ScaleFormat[sqlscale], PSmallInt(sqldata)^ / ScaleDivisor[sqlscale]));
          SQL_LONG   : Result := RawByteString(FormatFloat(ScaleFormat[sqlscale], PInteger(sqldata)^  / ScaleDivisor[sqlscale]));
          SQL_INT64,
          SQL_QUAD   : Result := RawByteString(FormatFloat(ScaleFormat[sqlscale], PInt64(sqldata)^    / ScaleDivisor[sqlscale]));
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := RawByteString(FormatFloat(ScaleFormat[sqlscale], PDouble(sqldata)^));
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := RawByteString(FloatToStr(PDouble(sqldata)^));
          SQL_TIMESTAMP : Result := RawByteString(DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(sqldata))));
          SQL_TYPE_DATE : Result := RawByteString(DateToStr(PInteger(sqldata)^ - DateOffset));
          SQL_TYPE_TIME : Result := RawByteString(TimeToStr(PCardinal(sqldata)^ / TimeCoeff));
          SQL_LONG      : Result := RawByteString(IntToStr(PInteger(sqldata)^));
          SQL_FLOAT     : Result := RawByteString(FloatToStr(PSingle(sqldata)^));
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := RawByteString(BoolToStr(PSmallint(sqldata)^ = 1));
{$ENDIF}
          SQL_SHORT     : Result := RawByteString(IntToStr(PSmallint(sqldata)^));
          SQL_INT64     : Result := RawByteString(IntToStr(PInt64(sqldata)^));
          SQL_TEXT      : DecodeStringB(SQL_TEXT, Index, Result);
          SQL_VARYING   : DecodeStringB(SQL_VARYING, Index, Result);
          SQL_BLOB      : ReadBlobB(Index, Result);
          SQL_ARRAY     : Result := '(Array)';
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLResult.GetAsUnicodeString(const Index: Word): UnicodeString;
    function BoolToStr(const Value: boolean): string;
    begin if Value then result := sUIBTrue else result := sUIBFalse; end;
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
          SQL_SHORT  : Result := FormatFloat(ScaleFormat[sqlscale], PSmallInt(sqldata)^ / ScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FormatFloat(ScaleFormat[sqlscale], PInteger(sqldata)^  / ScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FormatFloat(ScaleFormat[sqlscale], PInt64(sqldata)^ / ScaleDivisor[sqlscale]);
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := FormatFloat(ScaleFormat[sqlscale], PDouble(sqldata)^);
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
          SQL_TIMESTAMP : Result := DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE : Result := DateToStr(PInteger(sqldata)^ - DateOffset);
          SQL_TYPE_TIME : Result := TimeToStr(PCardinal(sqldata)^ / TimeCoeff);
          SQL_LONG      : Result := IntToStr(PInteger(sqldata)^);
          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := BoolToStr(PSmallint(sqldata)^ = 1);
{$ENDIF}
          SQL_SHORT     : Result := IntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToStr(PInt64(sqldata)^);
          SQL_TEXT      : DecodeStringW(SQL_TEXT, Index, Result);
          SQL_VARYING   : DecodeStringW(SQL_VARYING, Index, Result);
          SQL_BLOB      : ReadBlobW(Index, Result);
          SQL_ARRAY     : Result := '(Array)';
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLResult.GetAsVariant(const Index: Word): Variant;
  var
    ASQLCode: SmallInt;
    Dbl: Double;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := NULL;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / ScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / ScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / ScaleDivisor[sqlscale];
          SQL_D_FLOAT,
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EUIBConvertError.Create(EUIB_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : Result := TDateTime(DecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE :
            begin
              Dbl := PInteger(sqldata)^ - DateOffset;
              Result := TDateTime(Dbl);
            end;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / TimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_FLOAT     : Result := PSingle(sqldata)^;
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result :=  WordBool(PSmallint(sqldata)^);
{$ENDIF}
          SQL_SHORT     : Result := PSmallint(sqldata)^;
{$IFDEF COMPILER6_UP}
          SQL_INT64     : Result := PInt64(sqldata)^;
{$ELSE}
  {$IFDEF FPC}
          SQL_INT64     : Result := PInt64(sqldata)^;
  {$ELSE}
          SQL_INT64     : Result := Integer(PInt64(sqldata)^);
  {$ENDIF}
{$ENDIF}
          SQL_TEXT      : Result := DecodeString(SQL_TEXT, Index);
          SQL_VARYING   : Result := DecodeString(SQL_VARYING, Index);
          SQL_BLOB      : ReadBlob(Index, Result);
          SQL_ARRAY     : Result := '(Array)';
        else
          raise EUIBConvertError.Create(EUIB_CASTERROR);
        end;
    end;
  end;

  function TSQLResult.GetUniqueRelationName: string;
  var
    i: integer;
  begin
    result := '';
    if FXSQLDA.sqln > 1 then
      for i := 0 to FXSQLDA.sqln - 2 do
        if not ((FXSQLDA.sqlvar[i].RelNameLength = FXSQLDA.sqlvar[i+1].RelNameLength) and
          (StrIComp(FXSQLDA.sqlvar[i].RelName, FXSQLDA.sqlvar[i+1].RelName) = 0)) then
            exit;
    if FXSQLDA.sqln > 0 then
      SetString(Result, FXSQLDA.sqlvar[0].RelName, FXSQLDA.sqlvar[0].RelNameLength);
  end;

  function TSQLResult.GetDataQuadOffset(const index: word): Pointer;
  begin
    result := FXSQLDA.sqlvar[index].SqlData;
    inc(PtrInt(result), sizeof(TIscQuad));
  end;

  function TSQLResult.GetArrayData(const index: word): Pointer;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      result := nil;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      if (sqltype and not 1) = SQL_ARRAY then
        result := GetDataQuadOffset(index) else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
    end;
  end;

  function TSQLResult.GetBlobData(const index: word): PBlobData;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
    begin
      result := nil;
      if (sqlind <> nil) and (sqlind^ = -1) then Exit;
      if (sqltype and not 1) = SQL_BLOB then
        result := GetDataQuadOffset(index) else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
    end;
  end;

  function TSQLResult.GetArrayCount: Word;
  begin
    result := length(FArrayInfos);
  end;

  function TSQLResult.GetArrayInfos(const index: Word): PArrayInfo;
  begin
    if (index < length(FArrayInfos)) then
      result := @FArrayInfos[index] else
      raise Exception.CreateFmt(EUIB_INDEXERROR, [index]);
  end;

  procedure TSQLResult.Next;
  begin
    if CurrentRecord + 1 = RecordCount then
      FInMemoryEOF := True else
      CurrentRecord := CurrentRecord + 1;
  end;

function TSQLResult.ReadBlobA(const name: string): AnsiString;
begin
  ReadBlobA(name, Result);
end;

function TSQLResult.ReadBlobA(const Index: Word): AnsiString;
begin
  ReadBlobA(Index, Result);
end;

function TSQLResult.ReadBlobB(const name: string): RawByteString;
begin
  ReadBlobB(name, result);
end;

function TSQLResult.ReadBlobB(const Index: Word): RawByteString;
begin
  ReadBlobB(Index, Result);
end;

function TSQLResult.ReadBlobW(const name: string): UnicodeString;
begin
  ReadBlobW(name, Result);
end;

function TSQLResult.ReadBlobW(const Index: Word): UnicodeString;
begin
  ReadBlobW(Index, Result);
end;

{ TPoolStream }

function TPoolStream.Add: Pointer;
var
  item: integer;
begin
  item := FItemCount;
  SetSize((item + 1) * FItemSize);
  Result := Pointer(PtrInt(FPages[item div FItemsInPage]) + (Item mod FItemsInPage) * FItemSize);
end;

procedure TPoolStream.Clear;
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

constructor TPoolStream.Create(ItemsInPage, ItemSize: Integer);
begin
  Assert((ItemSize > 0) and (ItemsInPage > 0));
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

destructor TPoolStream.Destroy;
begin
  Clear;
  inherited;
end;

function TPoolStream.Get(Item: Integer): Pointer;
begin
  assert(Item * FItemSize <= FSize);
  Result := Pointer(PtrInt(FPages[Item div FItemsInPage]) + (Item mod FItemsInPage) * FItemSize);
end;

procedure TPoolStream.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TPoolStream.LoadFromStream(Stream: TStream);
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

function TPoolStream.Read(var Buffer; Count: Integer): Longint;
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
        Move(p^, c^, n);
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

procedure TPoolStream.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TPoolStream.SaveToStream(Stream: TStream);
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

function TPoolStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

function TPoolStream.SeekTo(Item: Integer): Longint;
begin
  Result := Seek(soFromBeginning, Item * FItemSize);
end;

procedure TPoolStream.SetSize(NewSize: Integer);
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

function TPoolStream.Write(const Buffer; Count: Integer): Longint;
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
        Move(c^, p^, n);
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

{ TSQLParams }

  constructor TSQLParams.Create(Charset: TCharacterSet);
  begin
    inherited Create(Charset);
    GetMem(FXSQLDA, XSQLDA_LENGTH(0));
    FillChar(FXSQLDA^, XSQLDA_LENGTH(0), #0);
    FXSQLDA.sqln := 0;
    FXSQLDA.sqld := 0;
    FXSQLDA.version := SQLDA_CURRENT_VERSION;
    FParamCount := 0;
  end;

  destructor TSQLParams.Destroy;
  begin
    Clear;
    FreeMem(FXSQLDA);
    inherited;
  end;

  function TSQLParams.GetFieldName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].ParamName,
      FXSQLDA.sqlvar[Index].ParamNameLength);
  end;

procedure TSQLParams.AddFieldType(const Name: string; FieldType: TUIBFieldType;
    Scale: TScale = 1; Precision: byte = 0);
  begin
    case FieldType of
      uftNumeric   :
        begin
          case Precision of
            0..4: SetFieldType(AddField(name), SizeOf(Smallint), SQL_SHORT + 1, -scale);
            5..7: SetFieldType(AddField(name), SizeOf(Integer) , SQL_LONG + 1 , -scale);
          else
            SetFieldType(AddField(name), SizeOf(Int64), SQL_INT64 + 1, -scale);
          end;
        end;
      uftChar,
      uftVarchar,
      uftCstring         : SetFieldType(AddField(name), 0                    , SQL_TEXT      + 1, 0);
      uftSmallint        : SetFieldType(AddField(name), SizeOf(Smallint)     , SQL_SHORT     + 1, 0);
      uftInteger         : SetFieldType(AddField(name), SizeOf(Integer)      , SQL_LONG      + 1, 0);
      uftQuad            : SetFieldType(AddField(name), SizeOf(TISCQuad)     , SQL_QUAD      + 1, 0);
      uftFloat           : SetFieldType(AddField(name), SizeOf(Single)       , SQL_FLOAT     + 1, 0);
      uftDoublePrecision : SetFieldType(AddField(name), SizeOf(Double)       , SQL_DOUBLE    + 1, 0);
      uftTimestamp       : SetFieldType(AddField(name), SizeOf(TISCTimeStamp), SQL_TIMESTAMP + 1, 0);
      uftBlob,
      uftBlobId          : SetFieldType(AddField(name), SizeOf(TISCQuad)     , SQL_BLOB      + 1, 0);
      uftDate            : SetFieldType(AddField(name), SizeOf(Integer)      , SQL_TYPE_DATE + 1, 0);
      uftTime            : SetFieldType(AddField(name), SizeOf(Cardinal)     , SQL_TYPE_TIME + 1, 0);
      uftInt64           : SetFieldType(AddField(name), SizeOf(Int64)        , SQL_INT64     + 1, 0);
      uftArray           : SetFieldType(AddField(name), SizeOf(TISCQuad)     , SQL_ARRAY     + 1, 0);
  {$IFDEF IB7_UP}
      uftBoolean         : SetFieldType(AddField(name), SizeOf(Smallint)     , SQL_BOOLEAN   + 1, 0);
  {$ENDIF}
    end;
  end;

  procedure TSQLParams.AllocateDataBuffer(AInit: boolean);
  var
    i, j: integer;
  begin
    for i := 0 to FXSQLDA.sqln - 1 do
      with FXSQLDA.sqlvar[i] do
      begin
        if AInit then
          Include(Flags, pfNotInitialized) else
          Exclude(Flags, pfNotInitialized);
        if SqlLen > 0 then
          GetMem(sqldata, SqlLen) else
          sqldata := nil;
        if ParamNameLength > 0 then
          for j := 0 to GetAllocatedFields - 1 do
            if (j <> i) and (ID = FXSQLDA.sqlvar[j].ID) then
              Move(FXSQLDA.sqlvar[i], FXSQLDA.sqlvar[j], SizeOf(TUIBSQLVar)-MaxParamLength-2);
      end;
  end;

  procedure TSQLParams.SetFieldType(const Index: Word; Size: Integer; Code,
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
              Move(FXSQLDA.sqlvar[Index], FXSQLDA.sqlvar[i], SizeOf(TUIBSQLVar)-MaxParamLength-2);
      end;
  end;

  function TSQLParams.Parse(const SQL: string): string;
  const
    Identifiers: set of AnsiChar = ['a'..'z', 'A'..'Z', '0'..'9', '_', '$'];
  var
    Src: PChar;
    Dest, idlen: Word;

    procedure next;
    begin
      inc(dest);
      Result[dest] := Char(Src^);
      inc(Src);
    end;

    procedure Skip(c: Char);
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

    {$IFDEF FPC}
    function PrevChar(c: PAnsiChar): AnsiChar;
    begin
      dec(c);
      result := c^;
    end;
    {$ENDIF}
  begin
    Clear;
    Src := PChar(SQL);
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
                while not ({$IFDEF UNICODE}(Src^ < #256) and {$ENDIF}(AnsiChar(Src^) in [#0, #13, #10])) do
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
                AddField('');
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
                    if {$IFDEF UNICODE}(Src[idlen] < #256) and {$ENDIF}(AnsiChar(Src[idlen]) in [#0, '"']) then
                      Break else
                      inc(idlen);
                end else
                // unquoted identifiers
                  while {$IFDEF UNICODE}(Src[idlen] < #256) and {$ENDIF}(AnsiChar(Src[idlen]) in Identifiers) do inc(idlen);
                AddField(copy(Src, 1, idlen));
                inc(Src, idlen);
                if Src^ = '"' then inc(Src);
              end;
        // skip everything when begin identifier found !
        // in procedures
        'b','B':
          begin
            if not ((dest > 0) and {$IFDEF UNICODE}(src[-1] < #256) and {$ENDIF} ({$IFDEF FPC}PrevChar(src){$ELSE}AnsiChar(src[-1]){$ENDIF}
              in Identifiers)) and (StrIComp(PChar(copy(Src, 0, 5)), 'begin') = 0) and
                not ({$IFDEF UNICODE}(Src[5] < #256) and{$ENDIF}(AnsiChar(Src[5]) in Identifiers)) then
                  while (Src^ <> #0) do Next else next;
          end;
        // declare should also stop param parsing, as a declare cursor statement
        // may contain variables.
        'd','D':
          begin
            if not ((dest > 0) and {$IFDEF UNICODE}(src[-1] < #256) and {$ENDIF} ({$IFDEF FPC}PrevChar(src){$ELSE}AnsiChar(src[-1]){$ENDIF}
              in Identifiers)) and (StrIComp(PChar(copy(Src, 0, 7)), 'declare') = 0) and
                not ({$IFDEF UNICODE} (Src[7] < #256) and {$ENDIF}(AnsiChar(Src[7]) in Identifiers)) then
                  while (Src^ <> #0) do Next else next;
          end;
      else
        next;
      end;
  end;

  function TSQLParams.GetFieldType(const Index: Word): TUIBFieldType;
  begin
    if IsNull[Index] and (pfNotInitialized in FXSQLDA.sqlvar[Index].Flags) then
      Result := uftUnKnown else
      Result := inherited GetFieldType(Index);
  end;

  function TSQLParams.GetIsNullable(const Index: Word): boolean;
  begin
    CheckRange(Index);
    Result := not(pfNotNullable in FXSQLDA.sqlvar[Index].Flags);
  end;

  function TSQLParams.GetMaxSqlLen(const Index: Word): SmallInt;
  begin
    CheckRange(Index);
    Result := FXSQLDA.sqlvar[Index].MaxSqlLen
  end;

  function TSQLParams.GetFieldIndex(const name: AnsiString): Word;
  begin
    if not TryGetFieldIndex(name, Result) then
      raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [name]);
  end;

  function TSQLParams.TryGetFieldIndex(const name: AnsiString; out Index: Word): Boolean;
  var
    Field: Integer;
  begin
    for Field := 0 to FXSQLDA.sqln - 1 do
      if FXSQLDA.sqlvar[Field].ParamNameLength = Length(name) then
        if StrLIComp(@FXSQLDA.sqlvar[Field].ParamName, PAnsiChar(Name),
          FXSQLDA.sqlvar[Field].ParamNameLength) = 0 then
          begin
            Result := true;
            Index  := Field;
            Exit;
          end;
    Result := False;
  end;

  function TSQLParams.AddFieldA(const Name: AnsiString): Word;
  var
    num: Word;
    len: Cardinal;
    p: PUIBSQLVar;
  begin
    len := Length(Name);
    if len > MaxParamLength then
      raise Exception.CreateFmt(EUIB_SIZENAME, [Name]);

    Result := FXSQLDA.sqln;
    if (len > 0) and TryGetFieldIndex(Name, num) then
    begin
      inc(FXSQLDA.sqln);
      inc(FXSQLDA.sqld);
      ReallocMem(FXSQLDA, XSQLDA_LENGTH(FXSQLDA.sqln));
      Move(FXSQLDA.sqlvar[num], FXSQLDA.sqlvar[Result], SizeOf(TUIBSQLVar));
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
        Move(PAnsiChar(Name)^, p^.ParamName[0], p^.ParamNameLength);
      p^.sqltype    := SQL_TEXT + 1; // tip: don't allocate memory if not defined
      p^.sqlscale   := 0;
      p^.sqlsubtype := 0;
      p^.sqllen     := 0;
      p^.sqldata    := nil;
      GetMem(p^.sqlind, 2); // Can be NULL
      p^.sqlind^ := -1; // NULL
    end;
  end;

  function TSQLParams.AddFieldW(const name: UnicodeString): Word;
  begin
    Result := AddFieldA(AnsiString(name));
  end;

  function TSQLParams.AddField(const name: string): Word;
  begin
{$IFDEF UNICODE}
    Result := AddFieldW(name);
{$ELSE}
    Result := AddFieldA(name);
{$ENDIF}
  end;

  procedure TSQLParams.Clear;
  var i, j: Smallint;
  begin
    for i := 0 to FXSQLDA.sqln - 1 do
    begin
      if (FXSQLDA.sqlvar[i].sqlind <> nil) then
      begin
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
    end;
    FXSQLDA.sqln := 0;
    FXSQLDA.sqld := 0;
    ReallocMem(FXSQLDA, XSQLDA_LENGTH(0));
    FParamCount := 0;
  end;

 // TSQLParams.SetAs...

  procedure TSQLParams.SetAsQuad(const Index: Word; const Value: TISCQuad);
  begin
    SetFieldType(Index, sizeof(TISCQuad), SQL_QUAD + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsRawByteString(const Index: Word; const Value: RawByteString);
  begin
    SetFieldType(Index, Length(Value), SQL_TEXT + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsDateTime(const Index: Word;
    const Value: TDateTime);
  begin
    SetFieldType(Index, sizeof(TISCQuad), SQL_TIMESTAMP + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsDate(const Index: Word; const Value: Integer);
  begin
    SetFieldType(Index, sizeof(Integer), SQL_TYPE_DATE + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsTime(const Index: Word; const Value: Cardinal);
  begin
    SetFieldType(Index, sizeof(Cardinal), SQL_TYPE_TIME + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsBoolean(const Index: Word; const Value: Boolean);
  begin
{$IFDEF IB7_UP}
    SetFieldType(Index, sizeof(Smallint), SQL_BOOLEAN + 1, 0);
{$ELSE}
    SetFieldType(Index, sizeof(Smallint), SQL_SHORT + 1, 0);
{$ENDIF}
    inherited;
  end;

  procedure TSQLParams.SetAsInteger(const Index: Word; const Value: Integer);
  begin
    SetFieldType(Index, sizeof(Integer), SQL_LONG + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsSingle(const Index: Word; const Value: Single);
  begin
    SetFieldType(Index, sizeof(Single), SQL_FLOAT + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsSmallint(const Index: Word; const Value: Smallint);
  begin
    SetFieldType(Index, sizeof(Smallint), SQL_SHORT + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsAnsiString(const Index: Word; const Value: AnsiString);
  begin
    SetFieldType(Index, Length(Value), SQL_TEXT + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsUnicodeString(const Index: Word;
    const Value: UnicodeString);
  begin
    SetFieldType(Index, Length(Value) * 2, SQL_TEXT + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsInt64(const Index: Word; const Value: Int64);
  begin
    SetFieldType(Index, sizeof(Int64), SQL_INT64 + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsDouble(const Index: Word; const Value: Double);
  begin
    SetFieldType(Index, sizeof(double), SQL_DOUBLE + 1, 0);
    inherited;
  end;

  procedure TSQLParams.SetAsCurrency(const Index: Word;
    const Value: Currency);
  begin
    SetFieldType(Index, sizeof(Int64), SQL_INT64 + 1, -4);
    inherited;
  end;

{$IFDEF GUID_TYPE}
  procedure TSQLParams.SetAsGUID(const Index: Word; const Value: TGUID);
  begin
  {$IFDEF GUID_AS_TEXT}
    SetFieldType(Index, 38, SQL_TEXT + 1, 0);
  {$ELSE}
    SetFieldType(Index, SizeOf(TGUID), SQL_TEXT + 1, 0);
  {$ENDIF}
    inherited;
  end;
{$ENDIF}

end.


