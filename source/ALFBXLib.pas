{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Henri Gourvest <hgourvest@progdigy.com>
              Olivier Guilbaud <oguilb@free.fr>
              Volkan Ceylan <volkance@hotmail.com>
              Stéphane Vander Clock (svanderclock@arkadia.com)
              The Original Code is the UIB code (version 2.1)

              The Initial Developer of the Original Code is
              Henri Gourvest <hgourvest@progdigy.com>. Portions
              created by the Initial Developer are Copyright (C)
              by the Initial Developer. All Rights Reserved.

Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALFBX (Alcinoe FireBird Express) - ALFBXLib
Version:      3.50

Description:  ALFBX (Alcinoe FireBird Express) does for the Firebird
              API what Delphi does for the WINDOWS API! Create high
              performance client/server applications based on FireBird
              without the BDE or ODBC.

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :

Link :        http://www.progdigy.com/modules.php?name=UIB

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}

unit ALFBXLib;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface
uses
  Windows,
  Variants,
  ALFBXBase,
  ALFBXError,
  Classes,
  SysUtils;

type

  TALFBXFieldType = (uftUnKnown, uftNumeric, uftChar, uftVarchar, uftCstring, uftSmallint,
    uftInteger, uftQuad, uftFloat, uftDoublePrecision, uftTimestamp, uftBlob, uftBlobId,
    uftDate, uftTime, uftInt64, uftArray);

  TALFBXScale = 1..15;

//******************************************************************************
// Errors handling
//******************************************************************************
  EALFBXConvertError = class(Exception);

  EALFBXError = class(Exception)
  private
    FErrorCode: Integer;
    FSQLCode  : Integer;
  public
    property ErrorCode: Integer read FErrorCode;
    property SQLCode: Integer read FSQLCode;
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
  csUNICODE_FSS, csWIN1250, csWIN1251, csWIN1252, csWIN1253, csWIN1254
  ,csDOS737, csDOS775, csDOS858, csDOS862, csDOS864, csDOS866, csDOS869, csWIN1255,
  csWIN1256, csWIN1257, csISO8859_3, csISO8859_4, csISO8859_5, csISO8859_6, csISO8859_7,
  csISO8859_8, csISO8859_9, csISO8859_13
  ,csKOI8R, csKOI8U, csUTF8);


const
  cALFBXCharacterSetStr : array[TALFBXCharacterSet] of string = (
    'NONE', 'ASCII', 'BIG_5', 'CYRL', 'DOS437', 'DOS850', 'DOS852', 'DOS857',
    'DOS860', 'DOS861', 'DOS863', 'DOS865', 'EUCJ_0208', 'GB_2312', 'ISO8859_1',
    'ISO8859_2', 'KSC_5601', 'NEXT', 'OCTETS', 'SJIS_0208', 'UNICODE_FSS',
    'WIN1250', 'WIN1251', 'WIN1252', 'WIN1253', 'WIN1254','DOS737', 'DOS775',
    'DOS858', 'DOS862', 'DOS864', 'DOS866', 'DOS869',
    'WIN1255', 'WIN1256', 'WIN1257', 'ISO8859_3', 'ISO8859_4', 'ISO8859_5',
    'ISO8859_6', 'ISO8859_7', 'ISO8859_8', 'ISO8859_9', 'ISO8859_13','KOI8R',
    'KOI8U', 'UTF8');

  function ALFBXStrToCharacterSet(const CharacterSet: string): TALFBXCharacterSet;
  function ALFBXCreateDBParams(Params: String; Delimiter: Char = ';'): string;
  function ALFBXCreateBlobParams(Params: String; Delimiter: Char = ';'): string;

//******************************************************************************
// Transaction
//******************************************************************************

const
  // Default Transaction Parameter
  TALFBXTPBDefault = isc_tpb_version3 + isc_tpb_write + isc_tpb_concurrency + isc_tpb_wait;

//******************************************************************************
//  DSQL
//******************************************************************************

  //****************************************
  // TALFBXSQLDA
  //****************************************

const
  cALFBXMaxParamLength = 125;

type
  PALFBXSQLVar = ^TALFBXSQLVar;
  TALFBXSQLVar = record
    SqlType      : Smallint;
    SqlScale     : Smallint;
    SqlSubType   : Smallint;
    SqlLen       : Smallint;
    SqlData      : Pchar;
    SqlInd       : PSmallint;
    case byte of
    // TALFBXSQLResult
    0 : ( SqlNameLength   : Smallint;
          SqlName         : array[0..METADATALENGTH-1] of char;
          RelNameLength   : Smallint;
          RelName         : array[0..METADATALENGTH-1] of char;
          OwnNameLength   : Smallint;
          OwnName         : array[0..METADATALENGTH-1] of char;
          AliasNameLength : Smallint;
          AliasName       : array[0..METADATALENGTH-1] of char;
          );
    // TSQLParam
    1 : ( Init            : boolean;
          ID              : Word;
          ParamNameLength : Smallint;
          ParamName       : array[0..cALFBXMaxParamLength-1] of char;
          );
  end;

  PALFBXSQLDaData = ^TALFBXSQLDaData;
  TALFBXSQLDaData = record
    version : Smallint;                // version of this XSQLDA
    sqldaid : array[0..7] of char;     // XSQLDA name field          ->  RESERVED
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
    function  GetAllocatedFields: Word;
    procedure SetAllocatedFields(Fields: Word);
    function  GetFieldCount: Integer;
    function  GetSQLType(const Index: Word): Smallint;
    function  GetSQLLen(const Index: Word): Smallint;
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
    procedure DecodeWideString(const Code: Smallint; Index: Word; out Str: WideString);
    procedure DecodeString(const Code: Smallint; Index: Word; out Str: String); overload;
    function  DecodeString(const Code: Smallint; Index: Word): String; overload;
    procedure EncodeString(Code: Smallint; Index: Word; const str: String);
    procedure EncodeWideString(Code: Smallint; Index: Word; const str: WideString);
  protected
    function GetSqlName(const Index: Word): string;
    function GetRelName(const Index: Word): string;
    function GetOwnName(const Index: Word): string;
    function GetAliasName(const Index: Word): string;
    function GetFieldType(const Index: Word): TALFBXFieldType; virtual;
    function GetIsNumeric(const Index: Word): boolean;
    function GetIsBlob(const Index: Word): boolean;
    function GetIsNullable(const Index: Word): boolean;
    function GetIsNull(const Index: Word): boolean;
    function GetAsDouble(const Index: Word): Double;
    function GetAsCurrency(const Index: Word): Currency;
    function GetAsInt64(const Index: Word): Int64;
    function GetAsInteger(const Index: Word): Integer;
    function GetAsSingle(const Index: Word): Single;
    function GetAsSmallint(const Index: Word): Smallint;
    function GetAsString(const Index: Word): String; virtual;
    function GetAsWideString(const Index: Word): WideString; virtual;
    function GetAsQuad(const Index: Word): TISCQuad;
    function GetAsVariant(const Index: Word): Variant; virtual;
    function GetAsDateTime(const Index: Word): TDateTime;
    function GetAsDate(const Index: Word): Integer;
    function GetAsTime(const Index: Word): Cardinal;
    function GetAsBoolean(const Index: Word): boolean;
    procedure SetIsNull(const Index: Word; const Value: boolean);
    procedure SetAsDouble(const Index: Word; const Value: Double); virtual;
    procedure SetAsCurrency(const Index: Word; const Value: Currency); virtual;
    procedure SetAsInt64(const Index: Word; const Value: Int64); virtual;
    procedure SetAsInteger(const Index: Word; const Value: Integer); virtual;
    procedure SetAsSingle(const Index: Word; const Value: Single); virtual;
    procedure SetAsSmallint(const Index: Word; const Value: Smallint); virtual;
    procedure SetAsString(const Index: Word; const Value: String); virtual;
    procedure SetAsWideString(const Index: Word; const Value: WideString); virtual;
    procedure SetAsQuad(const Index: Word; const Value: TISCQuad); virtual;
    procedure SetAsDateTime(const Index: Word; const Value: TDateTime); virtual;
    procedure SetAsBoolean(const Index: Word; const Value: Boolean); virtual;
    procedure SetAsDate(const Index: Word; const Value: Integer); virtual;
    procedure SetAsTime(const Index: Word; const Value: Cardinal); virtual;
    function GetByNameIsNumeric(const Name: String): boolean;
    function GetByNameIsBlob(const Name: String): boolean;
    function GetByNameIsNull(const Name: String): boolean;
    function GetByNameIsNullable(const Name: String): boolean;
    function GetByNameAsDouble(const Name: String): Double;
    function GetByNameAsCurrency(const Name: String): Currency;
    function GetByNameAsInt64(const Name: String): Int64;
    function GetByNameAsInteger(const Name: String): Integer;
    function GetByNameAsSingle(const Name: String): Single;
    function GetByNameAsSmallint(const Name: String): Smallint;
    function GetByNameAsString(const Name: String): String;
    function GetByNameAsWideString(const Name: String): WideString;
    function GetByNameAsQuad(const Name: String): TISCQuad;
    function GetByNameAsVariant(const Name: String): Variant;
    function GetByNameAsDateTime(const Name: String): TDateTime;
    function GetByNameAsBoolean(const Name: String): boolean;
    function GetByNameAsDate(const Name: String): Integer;
    function GetByNameAsTime(const Name: String): Cardinal;
    procedure SetByNameIsNull(const Name: String; const Value: boolean);
    procedure SetByNameAsDouble(const Name: String; const Value: Double);
    procedure SetByNameAsCurrency(const Name: String; const Value: Currency);
    procedure SetByNameAsInt64(const Name: String; const Value: Int64);
    procedure SetByNameAsInteger(const Name: String; const Value: Integer);
    procedure SetByNameAsSingle(const Name: String; const Value: Single);
    procedure SetByNameAsSmallint(const Name: String; const Value: Smallint);
    procedure SetByNameAsString(const Name: String; const Value: String);
    procedure SetByNameAsWideString(const Name: String; const Value: WideString);
    procedure SetByNameAsQuad(const Name: String; const Value: TISCQuad);
    procedure SetByNameAsDateTime(const Name: String; const Value: TDateTime);
    procedure SetByNameAsBoolean(const Name: String; const Value: boolean);
    procedure SetByNameAsDate(const Name: String; const Value: Integer);
  public
    procedure CheckRange(const Index: Word);
    function GetFieldIndex(const name: String): Word; virtual;
    property Data: PALFBXSQLDaData read FXSQLDA;
    property IsBlob[const Index: Word]: boolean read GetIsBlob;
    property IsNullable[const Index: Word]: boolean read GetIsNullable;
    property IsNumeric[const Index: Word]: boolean read GetIsNumeric;
    property FieldCount: Integer read GetFieldCount;
    property SQLType[const Index: Word]: Smallint read GetSQLType;
    property SQLLen[const Index: Word]: Smallint read GetSQLLen;
    property FieldType[const Index: Word]: TALFBXFieldType read GetFieldType;
    property IsNull       [const Index: Word]: boolean    read GetIsNull       write SetIsNull;
    property AsSmallint   [const Index: Word]: Smallint   read GetAsSmallint   write SetAsSmallint;
    property AsInteger    [const Index: Word]: Integer    read GetAsInteger    write SetAsInteger;
    property AsSingle     [const Index: Word]: Single     read GetAsSingle     write SetAsSingle;
    property AsDouble     [const Index: Word]: Double     read GetAsDouble     write SetAsDouble;
    property AsCurrency   [const Index: Word]: Currency   read GetAsCurrency   write SetAsCurrency;
    property AsInt64      [const Index: Word]: Int64      read GetAsInt64      write SetAsInt64;
    property AsString     [const Index: Word]: String     read GetAsString     write SetAsString;
    property AsWideString [const Index: Word]: WideString read GetAsWideString write SetAsWideString;
    property AsQuad       [const Index: Word]: TISCQuad   read GetAsQuad       write SetAsQuad;
    property AsDateTime   [const Index: Word]: TDateTime  read GetAsDateTime   write SetAsDateTime;
    property AsBoolean    [const Index: Word]: Boolean    read GetAsBoolean    write SetAsBoolean;
    property AsDate       [const Index: Word]: Integer    read GetAsDate       write SetAsDate;
    property AsTime       [const Index: Word]: Cardinal   read GetAsTime       write SetAsTime;
    property AsVariant    [const Index: Word]: Variant    read GetAsVariant;
    property ByNameIsNull       [const name: String]: boolean    read GetByNameIsNull       write SetByNameIsNull;
    property ByNameAsSmallint   [const name: String]: Smallint   read GetByNameAsSmallint   write SetByNameAsSmallint;
    property ByNameAsInteger    [const name: String]: Integer    read GetByNameAsInteger    write SetByNameAsInteger;
    property ByNameAsSingle     [const name: String]: Single     read GetByNameAsSingle     write SetByNameAsSingle;
    property ByNameAsDouble     [const name: String]: Double     read GetByNameAsDouble     write SetByNameAsDouble;
    property ByNameAsCurrency   [const name: String]: Currency   read GetByNameAsCurrency   write SetByNameAsCurrency;
    property ByNameAsInt64      [const name: String]: Int64      read GetByNameAsInt64      write SetByNameAsInt64;
    property ByNameAsString     [const name: String]: String     read GetByNameAsString     write SetByNameAsString;
    property ByNameAsWideString [const name: String]: WideString read GetByNameAsWideString write SetByNameAsWideString;
    property ByNameAsQuad       [const name: String]: TISCQuad   read GetByNameAsQuad       write SetByNameAsQuad;
    property ByNameAsVariant    [const name: String]: Variant    read GetByNameAsVariant;
    property ByNameAsDateTime   [const name: String]: TDateTime  read GetByNameAsDateTime   write SetByNameAsDateTime;
    property ByNameAsBoolean    [const name: String]: Boolean    read GetByNameAsBoolean    write SetByNameAsBoolean;
    property ByNameAsDate       [const name: String]: Integer    read GetByNameAsDate       write SetByNameAsDate;
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
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(Item: Integer): Longint; overload;
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

  PALFBXBlobData = ^TALFBXBlobData;
  TALFBXBlobData = packed record
    Size: Cardinal;
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
    FDataBufferLength: Word;
    FBlobsIndex: array of Word;
    FCurrentRecord: Integer;
    FBufferChunks: Cardinal;
    FScrollEOF: boolean;
    FInMemoryEOF: boolean;
    FArrayInfos: array of TALFBXArrayInfo;
    procedure AddCurrentRecord;
    procedure FreeBlobs(Buffer: Pointer);
    function GetRecordCount: Integer;
    function GetCurrentRecord: Integer;
    procedure AllocateDataBuffer;
    function GetEof: boolean;
    function GetUniqueRelationName: string;
    function GetBof: boolean;
    function GetDataQuadOffset(const index: word): Pointer;
    function GetBlobData(const index: word): PALFBXBlobData;
    function GetArrayData(const index: word): Pointer;
    function GetArrayCount: Word;
    function GetArrayInfos(const index: word): PALFBXArrayInfo;
  protected
    function GetAsString(const Index: Word): String; override;
    function GetAsWideString(const Index: Word): WideString; override;
    function GetAsVariant(const Index: Word): Variant; override;
  public
    constructor Create(Fields: SmallInt = 0; CachedFetch: Boolean = False; FetchBlobs: boolean = false; BufferChunks: Cardinal = 1000);
    destructor Destroy; override;
    procedure ClearRecords;
    procedure GetRecord(const Index: Integer);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure Next;
    property BlobData[const index: word]: PALFBXBlobData read GetBlobData;
    property ArrayData[const index: word]: Pointer read GetArrayData;
    property ArrayInfos[const index: word]: PALFBXArrayInfo read GetArrayInfos;
    property ArrayCount: Word read GetArrayCount;
    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    procedure ReadBlob(const Index: Word; var str: string); overload;
    procedure ReadBlob(const Index: Word; var str: WideString); overload;
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    procedure ReadBlob(const Index: Word; Data: Pointer); overload;
    procedure ReadBlob(const name: string; Stream: TStream); overload;
    procedure ReadBlob(const name: string; var str: string); overload;
    procedure ReadBlob(const name: string; var str: WideString); overload;
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
    property AsString     [const Index: Word]: String     read GetAsString;
    property AsWideString [const Index: Word]: WideString read GetAsWideString;
    property AsVariant    [const Index: Word]: Variant    read GetAsVariant;
    property AsDateTime   [const Index: Word]: TDateTime  read GetAsDateTime;
    property AsDate       [const Index: Word]: Integer    read GetAsDate;
    property AsTime       [const Index: Word]: Cardinal   read GetAsTime;
    property AsBoolean    [const Index: Word]: Boolean    read GetAsBoolean;
    property ByNameIsNull[const name: String]: boolean read GetByNameIsNull;
    property ByNameIsNullable[const name: String]: boolean read GetByNameIsNullable;
    property ByNameAsSmallint   [const name: String]: Smallint   read GetByNameAsSmallint;
    property ByNameAsInteger    [const name: String]: Integer    read GetByNameAsInteger;
    property ByNameAsSingle     [const name: String]: Single     read GetByNameAsSingle;
    property ByNameAsDouble     [const name: String]: Double     read GetByNameAsDouble;
    property ByNameAsCurrency   [const name: String]: Currency   read GetByNameAsCurrency;
    property ByNameAsInt64      [const name: String]: Int64      read GetByNameAsInt64;
    property ByNameAsString     [const name: String]: String     read GetByNameAsString;
    property ByNameAsWideString [const name: String]: WideString read GetByNameAsWideString;
    property ByNameAsQuad       [const name: String]: TISCQuad   read GetByNameAsQuad;
    property ByNameAsVariant    [const name: String]: Variant    read GetByNameAsVariant;
    property ByNameAsDateTime   [const name: String]: TDateTime  read GetByNameAsDateTime;
    property ByNameAsBoolean    [const name: String]: Boolean    read GetByNameAsBoolean;
    property ByNameAsDate       [const name: String]: Integer    read GetByNameAsDate;
    property ByNameAsTime       [const name: String]: Cardinal   read GetByNameAsTime;
    property Values[const name: String]: Variant read GetByNameAsVariant; default;
  end;

  TALFBXSQLResultClass = class of TALFBXSQLResult;

(******************************************************************************)
(* SQL Params                                                                 *)
(******************************************************************************)

  TALFBXSQLParams = class(TALFBXSQLDA)
  private
    FParamCount: Word;
    function FindParam(const name: string; out Index: Word): boolean;
    function GetFieldName(const Index: Word): string;
  protected
    function AddField(const name: string): Word;
    procedure SetFieldType(const Index: Word; Size: Integer; Code: SmallInt; Scale: Smallint);
    procedure SetAsDouble(const Index: Word; const Value: Double); override;
    procedure SetAsCurrency(const Index: Word; const Value: Currency); override;
    procedure SetAsInt64(const Index: Word; const Value: Int64); override;
    procedure SetAsInteger(const Index: Word; const Value: Integer); override;
    procedure SetAsSingle(const Index: Word; const Value: Single); override;
    procedure SetAsSmallint(const Index: Word; const Value: Smallint); override;
    procedure SetAsString(const Index: Word; const Value: String); override;
    procedure SetAsWideString(const Index: Word; const Value: WideString); override;
    procedure SetAsQuad(const Index: Word; const Value: TISCQuad); override;
    procedure SetAsDateTime(const Index: Word; const Value: TDateTime); override;
    procedure SetAsBoolean(const Index: Word; const Value: Boolean); override;
    procedure SetAsDate(const Index: Word; const Value: Integer); override;
    procedure SetAsTime(const Index: Word; const Value: Cardinal); override;
    function GetFieldType(const Index: Word): TALFBXFieldType; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Parse(const SQL: string): string;
    function GetFieldIndex(const name: String): Word; override;
    // don't use this method
    procedure AddFieldType(const Name: string; FieldType: TALFBXFieldType; Scale: TALFBXScale = 1; Precision: byte = 0);
    property Values[const name: String]: Variant read GetByNameAsVariant; default;
    property FieldName[const Index: Word]: string read GetFieldName;
    property ParamCount : Word read FParamCount;

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
      isc_info_sql_get_plan : (PlanDesc     : array[0..255] of Char);
  end;

  TALFBXLibrary = class;

  TALFBXStatusVector = array[0..19] of ISCStatus;
  PALFBXStatusVector = ^TALFBXStatusVector;

  TALFBXOnConnectionLost = procedure(Lib: TALFBXLibrary) of object;
  TALFBXOnGetDBExceptionClass = procedure(Number: Integer; out Excep: EALFBXExceptionClass) of object;

  TALFBXLibrary = class(TALFBXBaseLibrary)
  private
    FOnConnectionLost: TALFBXOnConnectionLost;
    FOnGetDBExceptionClass: TALFBXOnGetDBExceptionClass;
    FRaiseErrors: boolean;
    FSegmentSize: Word;
    function  GetSegmentSize: Word;
    procedure SetSegmentSize(Value: Word);
    procedure CheckFBXApiCall(const Status: ISCStatus; StatusVector: TALFBXStatusVector);
  public
    constructor Create(ApiVer: TALFBXVersion_Api); override;
    property  OnConnectionLost: TALFBXOnConnectionLost read FOnConnectionLost write FOnConnectionLost;
    property  OnGetDBExceptionClass: TALFBXOnGetDBExceptionClass read FOnGetDBExceptionClass write FOnGetDBExceptionClass;
    property  RaiseErrors: boolean read FRaiseErrors write FRaiseErrors default True;
    procedure AttachDatabase(FileName: String; var DbHandle: IscDbHandle; Params: String; Sep: Char = ';');
    procedure DetachDatabase(var DBHandle: IscDbHandle);
    procedure DatabaseInfo(var DBHandle: IscDbHandle; const Items: string; var Buffer: string); overload;
    function  DatabaseInfoIntValue(var DBHandle: IscDbHandle; const item: char): Integer;
    function  DatabaseInfoString(var DBHandle: IscDbHandle; item: byte; size: Integer): string;
    function  DatabaseInfoDateTime(var DBHandle: IscDbHandle; item: byte): TDateTime;
    procedure TransactionStart(var TraHandle: IscTrHandle; var DbHandle: IscDbHandle; const TPB: string = '');
    procedure TransactionStartMultiple(var TraHandle: IscTrHandle; DBCount: Smallint; Vector: PISCTEB);
    procedure TransactionCommit(var TraHandle: IscTrHandle);
    procedure TransactionRollback(var TraHandle: IscTrHandle);
    procedure TransactionCommitRetaining(var TraHandle: IscTrHandle);
    procedure TransactionPrepare(var TraHandle: IscTrHandle);
    procedure TransactionRollbackRetaining(var TraHandle: IscTrHandle);
    procedure DSQLExecuteImmediate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle; const Statement: string; Dialect: Word; Sqlda: TALFBXSQLDA = nil); overload;
    procedure DSQLExecuteImmediate(const Statement: string; Dialect: Word; Sqlda: TALFBXSQLDA = nil); overload;
    procedure DSQLAllocateStatement(var DBHandle: IscDbHandle; var StmtHandle: IscStmtHandle);
    function  DSQLPrepare(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Statement: string; Dialect: Word; Sqlda: TALFBXSQLResult = nil): TALFBXStatementType;
    procedure DSQLExecute(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLDA = nil);
    procedure DSQLExecute2(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word; InSqlda: TALFBXSQLDA; OutSqlda: TALFBXSQLResult);
    procedure DSQLFreeStatement(var StmtHandle: IscStmtHandle; Option: Word);
    function  DSQLFetch(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLResult): boolean;
    function  DSQLFetchWithBlobs(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLResult): boolean;
    procedure DSQLDescribe(var DbHandle: IscDbHandle; var TrHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLResult);
    procedure DSQLDescribeBind(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLDA);
    procedure DSQLSetCursorName(var StmtHandle: IscStmtHandle; const cursor: string);
    procedure DSQLExecImmed2(var DBHhandle: IscDbHandle; var TraHandle: IscTrHandle; const Statement: string; dialect: Word; InSqlda, OutSqlda: TALFBXSQLDA);
    procedure DSQLInfo(var StmtHandle: IscStmtHandle; const Items: array of byte; var buffer: String);
    function  DSQLInfoPlan(var StmtHandle: IscStmtHandle): string;
    function  DSQLInfoStatementType(var StmtHandle: IscStmtHandle): TALFBXStatementType;
    function  DSQLInfoRowsAffected(var StmtHandle: IscStmtHandle; StatementType: TALFBXStatementType): Cardinal;
    procedure DDLExecute(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle; const ddl: string);
    function  ArrayLookupBounds(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle; const RelationName, FieldName: String): TALFBXArrayDesc;
    procedure ArrayGetSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle; ArrayId: TISCQuad; var desc: TALFBXArrayDesc; DestArray: PPointer; var SliceLength: Integer);
    procedure ArrayPutSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle; var ArrayId: TISCQuad; var desc: TALFBXArrayDesc; DestArray: Pointer; var SliceLength: Integer);
    procedure ArraySetDesc(const RelationName, FieldName: string; var SqlDtype, SqlLength, Dimensions: Smallint; var desc: TISCArrayDesc);
    procedure ServiceAttach(const ServiceName: string; var SvcHandle: IscSvcHandle; const Spb: string);
    procedure ServiceDetach(var SvcHandle: IscSvcHandle);
    procedure ServiceQuery(var SvcHandle: IscSvcHandle; const SendSpb, RequestSpb: string; var Buffer: string);
    procedure ServiceStart(var SvcHandle: IscSvcHandle; const Spb: string);
    function  ErrSqlcode(StatusVector: TALFBXStatusVector): ISCLong;
    function  ErrInterprete(StatusVector: TALFBXStatusVector): String;
    function  ErrSQLInterprete(SQLCODE: Smallint): String;
    procedure BlobOpen(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle; var BlobHandle: IscBlobHandle; BlobId: TISCQuad; BPB: string = '');
    function  BlobGetSegment(var BlobHandle: IscBlobHandle; out length: Word; BufferLength: Cardinal; Buffer: PChar): boolean;
    procedure BlobClose(var BlobHandle: IscBlobHandle);
    procedure BlobInfo(var BlobHandle: IscBlobHandle; out NumSegments, MaxSegment, TotalLength: Cardinal; out btype : byte);
    procedure BlobSize(var BlobHandle: IscBlobHandle; out Size: Cardinal);
    procedure BlobMaxSegment(var BlobHandle: IscBlobHandle; out Size: Cardinal);
    procedure BlobDefaultDesc(var Desc: TALFBXBlobDesc; const RelationName, FieldName: string);
    procedure BlobSaveToStream(var BlobHandle: IscBlobHandle; Stream: TStream);
    function  BlobReadString(var BlobHandle: IscBlobHandle): string; overload;
    procedure BlobReadString(var BlobHandle: IscBlobHandle; var Str: String); overload;
    procedure BlobReadVariant(var BlobHandle: IscBlobHandle; var Value: Variant);
    // you must free memory allocated by this method !!
    procedure BlobReadBuffer(var BlobHandle: IscBlobHandle; var Size: Cardinal; var Buffer: Pointer; realloc: boolean = false);
    // the buffer size if known and Pointer allocated.
    procedure BlobReadSizedBuffer(var BlobHandle: IscBlobHandle; Buffer: Pointer); overload;
    // DBexpress and SP: the component set the max blob size
    procedure BlobReadSizedBuffer(var BlobHandle: IscBlobHandle; Buffer: Pointer; MaxSize: Cardinal); overload;
    function  BlobCreate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle; var BlobHandle: IscBlobHandle; BPB: string = ''): TISCQuad;
    procedure BlobWriteSegment(var BlobHandle: IscBlobHandle; BufferLength: Cardinal; Buffer: PChar);
    procedure BlobWriteString(var BlobHandle: IscBlobHandle; var Str: String);
    procedure BlobWriteStream(var BlobHandle: IscBlobHandle; Stream: TStream);
    function  StreamBlobOpen(var BlobId: TISCQuad; var Database: IscDbHandle; var Transaction: IscTrHandle; mode: Char): PBStream;
    function  StreamBlobClose(Stream: PBStream): integer;
    function  EventBlock(var EventBuffer, ResultBuffer: PChar; Count: Smallint; v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: PChar): Integer;
    procedure EventQueue(var handle: IscDbHandle; var id: Integer; length: Word; events: PChar; ast: IscCallback; arg: Pointer);
    procedure EventCounts(var ResultVector: TALFBXStatusVector; BufferLength: Smallint; EventBuffer, ResultBuffer: PChar);
    procedure EventCancel(var DbHandle: IscDbHandle; var id: integer);
    procedure EventWaitFor(var handle: IscDbHandle; length: Smallint; events, buffer: PChar);
    function  IscFree(data: Pointer): Integer;
    property  SegMentSize: Word read GetSegmentSize write SetSegmentSize;
  end;

//******************************************************************************
// Conversion
//******************************************************************************

const
  cALFBXDateOffset = 15018;
  cALFBXTimeCoeff = 864000000;

  procedure ALFBXDecodeTimeStamp(v: PISCTimeStamp; out DateTime: Double); overload;
  procedure ALFBXDecodeTimeStamp(v: PISCTimeStamp; out TimeStamp: TTimeStamp); overload;
  function  ALFBXDecodeTimeStamp(v: PISCTimeStamp): Double; overload;
  procedure ALFBXDecodeSQLDate(v: Integer; out Year: SmallInt; out Month, Day: Word);
  procedure ALFBXDecodeSQLTime(v: Cardinal; out Hour, Minute, Second: Word; out Fractions: LongWord);
  procedure ALFBXEncodeTimeStamp(const DateTime: TDateTime; v: PISCTimeStamp); overload;
  procedure ALFBXEncodeTimeStamp(const Date: Integer; v: PISCTimeStamp); overload;
  procedure ALFBXEncodeTimeStamp(const Time: Cardinal; v: PISCTimeStamp); overload;

type
  TALFBXParamType = (
    prNone, // no param
    prByte, // Byte Param
    prShrt, // Short Param
    prCard, // Cardinal Param
    prStrg, // String Param
    prIgno  // Ignore Command
  );

  TALFBXDPBInfo = record
    Name      : String;
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
    (Name: 'no_garbage_collect';     ParamType: prIgno), // not implemented
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
    (Name: 'gstat_attach';           ParamType: prNone), // ok FB15: don't work
    (Name: 'set_db_charset';         ParamType: prStrg), // ok
    (Name: 'gsec_attach';            ParamType: prNone),
    (Name: 'address_path';           ParamType: prStrg)
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


function ALFBXSQLQuote(const name: string): string;
function ALFBXSQLUnQuote(const name: string): string;

const
  cALFBXScaleDivisor: array[-15..-1] of Int64 = (1000000000000000,100000000000000,
    10000000000000,1000000000000,100000000000,10000000000,1000000000,100000000,
    10000000,1000000,100000,10000,1000,100,10);

  cALFBXCurrencyDivisor: array[-15..-1] of int64 = (100000000000,10000000000,
    1000000000,100000000,10000000,1000000,100000,10000,1000,100,10,1,10,100,
    1000);

implementation

uses ALFBXconst, AlFcnString, Math;

(******************************************************************************)
(* Errors handling                                                            *)
(******************************************************************************)

function ALFBXSQLQuote(const name: string): string;
var
  i, len: integer;
begin
  len := Length(name);
  if (len > 1) and (name[1] = '"') and (name[len] = '"') then
  begin // allready quoted: keep case
    Result := name;
    Exit;
  end;
  for i := 1 to len do
    if not (name[i] in ['a'..'z','A'..'Z', '0'..'9', '_', '$']) then
    begin // non standard carracter: keep case
      Result := '"' + name + '"';
      Exit;
    end;
  Result := ALUpperCase(name);
end;

function ALFBXSQLUnQuote(const name: string): string;
var
  i, len: integer;
begin
  len := Length(name);
  if (len > 1) and (name[1] = '"') and (name[len] = '"') then
  begin  // allready quoted: keep case
    Result := ALcopyStr(name, 2, len-2);
    Exit;
  end;
  for i := 1 to len do
    if not (name[i] in ['a'..'z','A'..'Z', '0'..'9', '_', '$']) then
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
    var Exception: EALFBXError;
    begin
      Exception := Excep.Create(ErrInterprete(StatusVector));
      if Excep = EALFBXException then
        EALFBXException(Exception).FNumber := Number;
      Exception.FSQLCode   := ErrSqlcode(StatusVector);
      if Exception.FSQLCode <> 0 then
        Exception.Message := Exception.Message + ErrSQLInterprete(Exception.FSQLCode) + cALFBXNewLine;
      Exception.FErrorCode := GETCode(Status);
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
      if ((Status = isc_lost_db_connection) or (Status = isc_network_error)) and
        Assigned(FOnConnectionLost) then
          FOnConnectionLost(Self);
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

  function ALFBXCreateDBParams(Params: String; Delimiter: Char = ';'): string;
  var
    BufferSize: Integer;
    CurPos, NextPos: PChar;
    CurStr, CurValue: String;
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
      Result[FinalSize] := chr(AByte);
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
    procedure AddString(var AString: String);
    var l: Integer;
    begin
      l := Min(Length(AString), 255);
      inc(FinalSize,l+1);
      CheckBufferSize;
      Result[FinalSize-l] := chr(l);
      ALMove(PChar(AString)^, Result[FinalSize-l+1], l);
    end;

  begin
    FinalSize := 1;
    BufferSize := 32;
    SetLength(Result, BufferSize);
    Result[1] := chr(isc_dpb_version1);
    CurPos  := PChar(Params);
    while (CurPos <> nil) do
    begin
      NextPos := StrScan(CurPos, Delimiter);
      if (NextPos = nil) then
        CurStr := CurPos else
        begin
          CurStr := ALcopyStr(CurPos, 0, NextPos-CurPos);
          Inc(NextPos);
        end;
      CurPos := NextPos;
      if (CurStr = '') then Continue;
      begin
        CurValue := '';
        EqualPos := Pos('=', CurStr);
        if EqualPos <> 0 then
        begin
          CurValue := ALcopyStr(CurStr, EqualPos+1, Length(CurStr) - EqualPos);
          CurStr   := ALcopyStr(CurStr, 0, EqualPos-1);
        end;
        CurStr := Trim(ALLowerCase(CurStr));
        CurValue := Trim(CurValue);
        for Code := 1 to isc_dpb_Max_Value do
          with cALFBXDPBInfos[Code] do
            if (Name = CurStr) then
            begin
              case ParamType of
                prNone : AddByte(Code);
                prByte :
                  if TryStrToInt(CurValue, AValue) and (AValue >= 0) and (AValue <= 255) then
                  begin
                    AddByte(Code);
                    AddByte(Byte(AValue));
                  end;
                prCard :
                  if TryStrToInt(CurValue, AValue) and (AValue > 0) then
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

  procedure TALFBXLibrary.AttachDatabase(FileName: String; var DbHandle: IscDbHandle;
    Params: String; Sep: Char = ';');
  Var aStatusVector: TALFBXStatusVector;
  begin
    Params := ALFBXCreateDBParams(Params, Sep);
      CheckFBXApiCall(isc_attach_database(@aStatusVector, Length(FileName), Pointer(FileName),
        @DBHandle, Length(Params), PChar(Params)), aStatusVector);
  end;

  procedure TALFBXLibrary.DetachDatabase(var DBHandle: IscDbHandle);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_detach_database(@aStatusVector, @DBHandle), aStatusVector);
      // if connection lost DBHandle must be set manually to nil.
      DBHandle := nil;
  end;

  procedure TALFBXLibrary.DatabaseInfo(var DBHandle: IscDbHandle;
    const Items: string; var Buffer: string);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_database_info(@aStatusVector, @DBHandle, Length(Items),
        Pointer(Items), Length(Buffer), Pointer(Buffer)), aStatusVector);
  end;

  function TALFBXLibrary.DatabaseInfoIntValue(var DBHandle: IscDbHandle;
    const item: char): Integer;
  var
    data: packed record
      item: char;
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
      item: char;
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
    item: byte; size: Integer): string;
  Var aStatusVector: TALFBXStatusVector;
  begin
    SetLength(result, size);
      while true do
      begin
        CheckFBXApiCall(isc_database_info(@aStatusVector, @DBHandle, 1, @item,
          Length(result), PChar(result)), aStatusVector);
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

  function ALFBXStrToCharacterSet(const CharacterSet: string): TALFBXCharacterSet;
  var
    len: Integer;
  begin
    len := length(CharacterSet);
    if (len = 0) then
      Result := csNONE else
    begin
      for Result := low(TALFBXCharacterSet) to High(TALFBXCharacterSet) do
        if (len = Length(cALFBXCharacterSetStr[Result])) and
          (ALCompareText(cALFBXCharacterSetStr[Result], CharacterSet) = 0) then
            Exit;
      raise Exception.CreateFmt(cALFBX_CHARSETNOTFOUND, [CharacterSet]);
    end;
  end;

//******************************************************************************
// Transaction
//******************************************************************************

  procedure TALFBXLibrary.TransactionStart(var TraHandle: IscTrHandle; var DbHandle: IscDbHandle;
    const TPB: string = '');
  var Vector: TISCTEB;
  begin
    Vector.Handle  := @DbHandle;
    Vector.Len     := Length(TPB);
    Vector.Address := PChar(TPB);
    TransactionStartMultiple(TraHandle, 1, @Vector);
  end;

  procedure TALFBXLibrary.TransactionStartMultiple(var TraHandle: IscTrHandle; DBCount: Smallint; Vector: PISCTEB);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_start_multiple(@aStatusVector, @TraHandle, DBCount, Vector), aStatusVector);
  end;

  procedure TALFBXLibrary.TransactionCommit(var TraHandle: IscTrHandle);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_commit_transaction(@aStatusVector, @TraHandle), aStatusVector);
      // if connection lost TraHandle must be set manually to nil.
      TraHandle := nil;
  end;

  procedure TALFBXLibrary.TransactionRollback(var TraHandle: IscTrHandle);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_rollback_transaction(@aStatusVector, @TraHandle), aStatusVector);
      // if connection lost TraHandle must be set manually to nil.
      TraHandle := nil;
  end;

  procedure TALFBXLibrary.TransactionCommitRetaining(var TraHandle: IscTrHandle);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_commit_retaining(@aStatusVector, @TraHandle), aStatusVector);
  end;

  procedure TALFBXLibrary.TransactionPrepare(var TraHandle: IscTrHandle);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_prepare_transaction(@aStatusVector, @TraHandle), aStatusVector);
  end;

  procedure TALFBXLibrary.TransactionRollbackRetaining(var TraHandle: IscTrHandle);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_rollback_retaining(@aStatusVector, @TraHandle), aStatusVector);
  end;

//******************************************************************************
// DSQL
//******************************************************************************

  function GetSQLDAData(SQLDA: TALFBXSQLDA): Pointer;
  begin
    if (SQLDA <> nil) then
      Result := SQLDA.FXSQLDA else
      Result := nil;
  end;

  //****************************************
  // API CALLS
  //****************************************

  procedure TALFBXLibrary.DSQLExecuteImmediate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
    const Statement: string; Dialect: Word; Sqlda: TALFBXSQLDA = nil);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_dsql_execute_immediate(@aStatusVector, @DBHandle, @TraHandle,
        length(Statement), Pointer(Statement), Dialect, GetSQLDAData(Sqlda)), aStatusVector);
  end;

  procedure TALFBXLibrary.DSQLExecuteImmediate(const Statement: string; Dialect: Word; Sqlda: TALFBXSQLDA = nil);
  var p: pointer;
      aStatusVector: TALFBXStatusVector;
  begin
      p := nil;
      CheckFBXApiCall(isc_dsql_execute_immediate(@aStatusVector, @p, @p,
        length(Statement), Pointer(Statement), Dialect, GetSQLDAData(Sqlda)), aStatusVector);
  end;

  procedure TALFBXLibrary.DSQLAllocateStatement(var DBHandle: IscDbHandle; var StmtHandle: IscStmtHandle);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_dsql_allocate_statement(@aStatusVector, @DBHandle, @StmtHandle), aStatusVector);
  end;

  function TALFBXLibrary.DSQLPrepare(var DbHandle: IscDbHandle; var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
    Statement: string; Dialect: Word; Sqlda: TALFBXSQLResult = nil): TALFBXStatementType;
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
        PChar(Statement), Dialect, GetSQLDAData(Sqlda)), aStatusVector);
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
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_dsql_execute(@aStatusVector, @TraHandle, @StmtHandle,
        Dialect, GetSQLDAData(Sqlda)), aStatusVector);
  end;

  procedure TALFBXLibrary.DSQLExecute2(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word;
    InSqlda: TALFBXSQLDA; OutSqlda: TALFBXSQLResult);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_dsql_execute2(@aStatusVector, @TraHandle, @StmtHandle, Dialect,
        GetSQLDAData(InSqlda), GetSQLDAData(OutSqlda)), aStatusVector);
  end;

  procedure TALFBXLibrary.DSQLFreeStatement(var StmtHandle: IscStmtHandle; Option: Word);
  Var aStatusVector: TALFBXStatusVector;
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
      Status := isc_dsql_fetch(@aStatusVector, @StmtHandle, Dialect, GetSQLDAData(Sqlda));

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
                  inc(integer(destArray), SizeOf(TISCQuad));
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
      Status := isc_dsql_fetch(@aStatusVector, @StmtHandle, Dialect, GetSQLDAData(Sqlda));

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
                    inc(integer(destArray), SizeOf(TISCQuad));
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
      CheckFBXApiCall(isc_dsql_describe(@aStatusVector, @StmtHandle, Dialect, GetSQLDAData(Sqlda)), aStatusVector);
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
            Sqlda.FArrayInfos[ArrayCount].info := ArrayLookupBounds(DbHandle, TrHandle, Sqlda.RelName[i], Sqlda.SqlName[i]);
            Sqlda.FArrayInfos[ArrayCount].index := i;
          end;
      end;
      Sqlda.AllocateDataBuffer;
    end;
  end;

  procedure TALFBXLibrary.DSQLDescribeBind(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TALFBXSQLDA);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_dsql_describe_bind(@aStatusVector, @StmtHandle, Dialect,
        GetSQLDAData(Sqlda)), aStatusVector);
  end;

  procedure  TALFBXLibrary.DSQLSetCursorName(var StmtHandle: IscStmtHandle; const cursor: string);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_dsql_set_cursor_name(@aStatusVector, @StmtHandle, PChar(cursor), 0), aStatusVector);
  end;

  procedure TALFBXLibrary.DSQLExecImmed2(var DBHhandle: IscDbHandle; var TraHandle: IscTrHandle;
    const Statement: string; dialect: Word; InSqlda, OutSqlda: TALFBXSQLDA);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_dsql_exec_immed2(@aStatusVector, @DBHhandle, @TraHandle, Length(Statement),
        PChar(Statement), dialect, GetSQLDAData(InSqlda), GetSQLDAData(OutSqlda)), aStatusVector);
  end;

  procedure TALFBXLibrary.DSQLInfo(var StmtHandle: IscStmtHandle; const Items: array of byte; var buffer: String);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_dsql_sql_info(@aStatusVector, @StmtHandle, Length(Items), @Items[0],
        Length(buffer), PChar(buffer)), aStatusVector);
  end;

  function TALFBXLibrary.DSQLInfoPlan(var StmtHandle: IscStmtHandle): string;
  var
    STInfo : packed record
      InfoCode: byte;
      InfoLen : Word;
      PlanDesc: array[0..1024] of Char;
    end;
    InfoType: Byte;
    aStatusVector: TALFBXStatusVector;
  begin
    InfoType := isc_info_sql_get_plan;
      CheckFBXApiCall(isc_dsql_sql_info(@aStatusVector, @StmtHandle, 1, @InfoType,
        SizeOf(STInfo), @STInfo), aStatusVector);
    SetString(Result, PChar(@STInfo.PlanDesc[1]), STInfo.InfoLen - 1);
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

  function TALFBXLibrary.DSQLInfoRowsAffected(var StmtHandle: IscStmtHandle;
    StatementType: TALFBXStatementType): Cardinal;
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
    if not (StatementType in [stUpdate, stDelete, stInsert, stExecProcedure]) then
      Result := 0 else
    begin
        Command := isc_info_sql_records;
        CheckFBXApiCall(isc_dsql_sql_info(@aStatusVector, @StmtHandle, 1, @Command,
          SizeOf(InfoData), @InfoData), aStatusVector);
        case StatementType of
          stUpdate: Result := InfoData.Infos[0].Rows;
          stDelete: Result := InfoData.Infos[1].Rows;
          stInsert: Result := InfoData.Infos[3].Rows;
          stExecProcedure: Result := InfoData.Infos[0].Rows + InfoData.Infos[1].Rows + InfoData.Infos[3].Rows;
        else
          Result := 0;
        end;
    end;
  end;

  procedure TALFBXLibrary.DDLExecute(var DBHandle: IscDbHandle;
    var TraHandle: IscTrHandle; const ddl: string);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_ddl(@aStatusVector, @DBHandle, @TraHandle,
        length(ddl), Pointer(ddl)), aStatusVector);
  end;

//******************************************************************************
//  Array
//******************************************************************************
  function TALFBXLibrary.ArrayLookupBounds(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
    const RelationName, FieldName: String): TALFBXArrayDesc;
  Var aStatusVector: TALFBXStatusVector;
  begin
        CheckFBXApiCall(isc_array_lookup_bounds(@aStatusVector, @DBHandle, @TransHandle,
          PChar(RelationName), PChar(FieldName), @Result), aStatusVector);
  end;

  procedure TALFBXLibrary.ArrayGetSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle; ArrayId: TISCQuad;
    var desc: TALFBXArrayDesc; DestArray: PPointer; var SliceLength: Integer);
  Var aStatusVector: TALFBXStatusVector;
  begin
        CheckFBXApiCall(isc_array_get_slice(@aStatusVector, @DBHandle, @TransHandle, @ArrayId,
          @desc, DestArray, @SliceLength), aStatusVector);
  end;

  procedure TALFBXLibrary.ArrayPutSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
    var ArrayId: TISCQuad; var desc: TALFBXArrayDesc; DestArray: Pointer; var SliceLength: Integer);
  Var aStatusVector: TALFBXStatusVector;
  begin
        CheckFBXApiCall(isc_array_put_slice(@aStatusVector, @DBHandle, @TransHandle, @ArrayId,
          @desc, DestArray, @SliceLength), aStatusVector);
  end;

  procedure TALFBXLibrary.ArraySetDesc(const RelationName, FieldName: string; var SqlDtype,
    SqlLength, Dimensions: Smallint; var desc: TISCArrayDesc);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_array_set_desc(@aStatusVector, PChar(RelationName),
        PChar(FieldName), @SqlDtype, @SqlLength, @Dimensions, @desc), aStatusVector);
  end;


//******************************************************************************
//  Error-handling
//******************************************************************************

  function  TALFBXLibrary.ErrSqlcode(StatusVector: TALFBXStatusVector): ISCLong;
  begin
      Result := isc_sqlcode(@StatusVector);
  end;

  function TALFBXLibrary.ErrInterprete(StatusVector: TALFBXStatusVector): String;
  var
    PStatusVector: PALFBXStatusVector;
    len: Integer;
    buffer: array[0..512] of char;
  begin
    Result := '';
    PStatusVector := @StatusVector;
      repeat
        If Version_api_IS_FB20_UP then len := fb_interpret(buffer, sizeof(buffer), @PStatusVector)
        else len := isc_interprete(buffer, @PStatusVector);
        if len > 0 then
          Result := Result + ALcopyStr(buffer, 0, len) + cALFBXNewLine else
          Break;
      until False;
  end;

  function TALFBXLibrary.ErrSQLInterprete(SQLCODE: Smallint): String;
  var
    i : Integer;
  begin
    SetLength(Result, 255);
      isc_sql_interprete(SQLCODE, PChar(Result), 255);
    for i := 1 to 255 do if Result[i] = #0 then Break; // Quick trim
    SetLength(Result, i-1);
  end;

//******************************************************************************
// Services
//******************************************************************************

  procedure TALFBXLibrary.ServiceAttach(const ServiceName: string; var SvcHandle: IscSvcHandle; const Spb: string);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_service_attach(@aStatusVector, Length(ServiceName),
        PChar(ServiceName), @SvcHandle, Length(Spb), PChar(Spb)), aStatusVector);
  end;

  procedure TALFBXLibrary.ServiceDetach(var SvcHandle: IscSvcHandle);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_service_detach(@aStatusVector, @SvcHandle), aStatusVector);
  end;

  procedure TALFBXLibrary.ServiceQuery(var SvcHandle: IscSvcHandle; const SendSpb, RequestSpb: string; var Buffer: string);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_service_query(@aStatusVector, @SvcHandle, nil,
        Length(SendSpb), PChar(SendSpb), Length(RequestSpb), PChar(RequestSpb),
        Length(Buffer), PChar(Buffer)), aStatusVector);
  end;

  procedure TALFBXLibrary.ServiceStart(var SvcHandle: IscSvcHandle; const Spb: string);
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_service_start(@aStatusVector, @SvcHandle, nil, Length(Spb), PChar(Spb)), aStatusVector);
  end;

//******************************************************************************
//  Blob
//******************************************************************************

  function ALFBXCreateBlobParams(Params: String; Delimiter: Char = ';'): string;
  var
    BufferSize: Integer;
    CurPos, NextPos: PChar;
    CurStr, CurValue: String;
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
      Result[FinalSize] := chr(AByte);
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
    Result := StringOfChar(#0,BufferSize);
    Result[1] := isc_bpb_version1;
    CurPos  := PChar(Params);
    while (CurPos <> nil) do
    begin
      NextPos := StrScan(CurPos, Delimiter);
      if (NextPos = nil) then
        CurStr := CurPos else
        begin
          CurStr := ALcopyStr(CurPos, 0, NextPos-CurPos);
          Inc(NextPos);
        end;
      CurPos := NextPos;
      if (CurStr = '') then Continue;
      begin
        CurValue := '';
        EqualPos := Pos('=', CurStr);
        if EqualPos <> 0 then
        begin
          CurValue := ALcopyStr(CurStr, EqualPos+1, Length(CurStr) - EqualPos);
          CurStr   := ALcopyStr(CurStr, 0, EqualPos-1);
        end;
        CurStr := Trim(ALLowerCase(CurStr));
        CurValue := Trim(CurValue);
        for Code := 1 to isc_bpb_Max_Value do
          with cALFBXBPBInfos[Code] do
            if (Name = CurStr) then
            begin
              case ParamType of
                prShrt :
                  if TryStrToInt(CurValue, AValue) and (AValue >= -128) and (AValue <= 127) then
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
    var BlobHandle: IscBlobHandle; BlobId: TISCQuad; BPB: string = '');
  Var aStatusVector: TALFBXStatusVector;
  begin
    BPB := ALFBXCreateBlobParams(BPB,';');
      CheckFBXApiCall(isc_open_blob2(@aStatusVector, @DBHandle, @TraHandle, @BlobHandle,
        @BlobId, Length(BPB), PChar(BPB)), aStatusVector);
  end;

  function TALFBXLibrary.BlobGetSegment(var BlobHandle: IscBlobHandle; out length: Word;
    BufferLength: Cardinal; Buffer: PChar): boolean;
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
  Var aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_close_blob(@aStatusVector, @BlobHandle), aStatusVector);
  end;

type
  TBlobInfo = packed record
    Info: Char;
    Length: Word;
    case byte of
      0: (CardType: Cardinal);
      1: (ByteType: Byte);
  end;

  procedure TALFBXLibrary.BlobSize(var BlobHandle: IscBlobHandle; out Size: Cardinal);
  var
    BlobInfo : packed record
      Code: Char;
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

  procedure TALFBXLibrary.BlobDefaultDesc(var Desc: TALFBXBlobDesc; const RelationName, FieldName: string);
  begin
        isc_blob_default_desc(@Desc, PChar(RelationName), PChar(FieldName));
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

  function TALFBXLibrary.BlobReadString(var BlobHandle: IscBlobHandle): string;
  begin
    BlobReadString(BlobHandle, Result);
  end;

  procedure TALFBXLibrary.BlobReadString(var BlobHandle: IscBlobHandle; var Str: String);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    Buffer: Pointer;
    Len: Cardinal;
    aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 2,
        isc_info_blob_max_segment + isc_info_blob_total_length,
        SizeOf(BlobInfos), @BlobInfos), aStatusVector);
    SetLength(Str, BlobInfos[1].CardType);
    Buffer := PChar(Str);
    len := 0;
    while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - len, Buffer) do
    begin
      inc(Integer(Buffer), CurrentLength);
      inc(len, CurrentLength);
      if len = BlobInfos[1].CardType then
        Break;
    end;
  end;

  procedure TALFBXLibrary.BlobReadBuffer(var BlobHandle: IscBlobHandle; var Size: Cardinal;
    var Buffer: Pointer; realloc: boolean);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    TMP: Pointer;
    Len: Cardinal;
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
      inc(Integer(TMP), CurrentLength);
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
    Len: Cardinal;
    aStatusVector: TALFBXStatusVector;
  begin
      CheckFBXApiCall(isc_blob_info(@aStatusVector, @BlobHandle, 2,
        isc_info_blob_max_segment + isc_info_blob_total_length,
        SizeOf(BlobInfos), @BlobInfos), aStatusVector);
    TMP := Buffer;
    Len := 0;
    while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - len, TMP) do
    begin
      inc(Integer(TMP), CurrentLength);
      inc(Len, CurrentLength);
      if len = BlobInfos[1].CardType then
        break;
    end;
  end;

  procedure TALFBXLibrary.BlobReadSizedBuffer(var BlobHandle: IscBlobHandle;
    Buffer: Pointer; MaxSize: Cardinal);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    TMP: Pointer;
    Len: Cardinal;
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
      inc(Integer(TMP), CurrentLength);
      inc(Len, CurrentLength);
      if len = MaxSize then
        break;
    end;
  end;

  procedure TALFBXLibrary.BlobReadVariant(var BlobHandle: IscBlobHandle; var Value: Variant);
  var
    BlobInfos: array[0..2] of TBlobInfo;
    CurrentLength: Word;
    Len: Cardinal;
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
        inc(Integer(Buffer), CurrentLength);
        inc(Len, CurrentLength);
        if Len = BlobInfos[1].CardType then
          Break;
      end;
    finally
      VarArrayUnlock(Value);
    end;
  end;

  function TALFBXLibrary.BlobCreate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
    var BlobHandle: IscBlobHandle; BPB: string = ''): TISCQuad;
  Var aStatusVector: TALFBXStatusVector;
  begin
    BPB := ALFBXCreateBlobParams(BPB,';');
      CheckFBXApiCall(isc_create_blob2(@aStatusVector, @DBHandle, @TraHandle, @BlobHandle, @Result, Length(BPB), PChar(BPB)), aStatusVector);
  end;

  procedure TALFBXLibrary.BlobWriteSegment(var BlobHandle: IscBlobHandle; BufferLength: Cardinal; Buffer: PChar);
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
        inc(Buffer, size);
      end;
  end;

  procedure TALFBXLibrary.BlobWriteString(var BlobHandle: IscBlobHandle; var Str: String);
  begin
    BlobWriteSegment(BlobHandle, Length(Str), PChar(Str));
  end;

  procedure TALFBXLibrary.BlobWriteStream(var BlobHandle: IscBlobHandle; Stream: TStream);
  var
    Buffer: PChar;
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
    Mode: Char): PBStream;
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
  Var aStatusVector: TALFBXStatusVector;
  begin
     CheckFBXApiCall(isc_cancel_events(@aStatusVector, @DbHandle, @id), aStatusVector);
  end;

  function TALFBXLibrary.EventBlock(var EventBuffer, ResultBuffer: PChar; Count: Smallint;
    v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: PChar): Integer;
  begin
      result := isc_event_block(@EventBuffer, @ResultBuffer, Count, v1, v2,
        v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);
  end;

  procedure TALFBXLibrary.EventQueue(var handle: IscDbHandle; var id: Integer; length: Word;
      events: PChar; ast: IscCallback; arg: Pointer);
  Var aStatusVector: TALFBXStatusVector;
  begin
     CheckFBXApiCall(isc_que_events(@aStatusVector, @handle, @id, length,
       events, ast, arg), aStatusVector);
  end;

  procedure TALFBXLibrary.EventCounts(var ResultVector: TALFBXStatusVector;
    BufferLength: Smallint; EventBuffer, ResultBuffer: PChar);
  begin
      isc_event_counts(@ResultVector, BufferLength, EventBuffer, ResultBuffer);
  end;

  procedure TALFBXLibrary.EventWaitFor(var handle: IscDbHandle; length: Smallint;
    events, buffer: PChar);
  Var aStatusVector: TALFBXStatusVector;
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
      Assert(Value > 0);
      FSegmentSize := Value;
  end;


//******************************************************************************
// Conversion
// Making a delphi conversion will help to transport data buffer and use it
// without GDS32 ;)
//******************************************************************************

  procedure ALFBXDecodeTimeStamp(v: PISCTimeStamp; out DateTime: Double);
  begin
    DateTime := v.timestamp_date - cALFBXDateOffset + (v.timestamp_time / cALFBXTimeCoeff);
  end;

  procedure ALFBXDecodeTimeStamp(v: PISCTimeStamp; out TimeStamp: TTimeStamp);
  begin
    TimeStamp.Date := v.timestamp_date - cALFBXDateOffset + 693594;
    TimeStamp.Time := v.timestamp_time div 10;
  end;

  function  ALFBXDecodeTimeStamp(v: PISCTimeStamp): Double;
  begin
    ALFBXDecodeTimeStamp(v, Result);
  end;

  procedure ALFBXEncodeTimeStamp(const DateTime: TDateTime; v: PISCTimeStamp);
  begin
    v.timestamp_date := Round(int(DateTime)) + cALFBXDateOffset;
    v.timestamp_time := ISC_TIME(Round(Frac(DateTime) * cALFBXTimeCoeff));
  end;

  procedure ALFBXEncodeTimeStamp(const Date: Integer; v: PISCTimeStamp);
  begin
    v.timestamp_date := Date + cALFBXDateOffset;
    v.timestamp_time := 0;
  end;

  procedure ALFBXEncodeTimeStamp(const Time: Cardinal; v: PISCTimeStamp);
  begin
    v.timestamp_date := cALFBXDateOffset;
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

  function TALFBXSQLDA.GetSqlName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].SqlName,
      FXSQLDA.sqlvar[Index].SqlNameLength);
  end;

  function TALFBXSQLDA.GetAliasName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].AliasName,
      FXSQLDA.sqlvar[Index].AliasNameLength);
  end;

  function TALFBXSQLDA.GetOwnName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].OwnName,
      FXSQLDA.sqlvar[Index].OwnNameLength);
  end;

  function TALFBXSQLDA.GetRelName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].RelName,
      FXSQLDA.sqlvar[Index].RelNameLength);
  end;

  function TALFBXSQLDA.GetIsNull(const Index: Word): boolean;
  begin
    CheckRange(Index);
    Result := (FXSQLDA.sqlvar[Index].sqlind <> nil) and
              (FXSQLDA.sqlvar[Index].sqlind^ = -1)
  end;

  procedure TALFBXSQLDA.CheckRange(const Index: Word);
  begin
    if Index >= Word(FXSQLDA.sqln) then
      raise Exception.CreateFmt(cALFBX_FIELDNUMNOTFOUND, [index]);
  end;

  function TALFBXSQLDA.DecodeString(const Code: Smallint; Index: Word): String;
  begin
    with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT    : SetString(Result, sqldata, sqllen);
      SQL_VARYING : SetString(Result, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
    end;
  end;

  procedure TALFBXSQLDA.DecodeString(const Code: Smallint; Index: Word; out Str: String);
  begin
    with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT    : SetString(Str, sqldata, sqllen);
      SQL_VARYING : SetString(Str, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
    end;
  end;

  procedure TALFBXSQLDA.DecodeWideString(const Code: Smallint; Index: Word; out Str: WideString);
    procedure SetWideString(var s: WideString; buffer: PChar; len: Integer);
    begin
      SetLength(s, len div 2);
      ALMove(buffer^, PWideChar(s)^, len);
    end;
  begin
    with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT    : SetWideString(Str, sqldata, sqllen);
      SQL_VARYING : SetWideString(Str, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
    end;
  end;

  procedure TALFBXSQLDA.EncodeString(Code: Smallint; Index: Word; const str: String);
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
            sqllen := NewLen;
            ALMove(PChar(str)^, sqldata^, sqllen);
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
            sqllen := NewLen + 2;
            PVary(sqldata).vary_length := NewLen;
            ALMove(PChar(str)^, PVary(sqldata).vary_string,PVary(sqldata).vary_length);
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

  procedure TALFBXSQLDA.EncodeWideString(Code: Smallint; Index: Word; const str: WideString);
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
          NewLen := Length(str) * 2;
          if NewLen = 0 then
          begin
            // interbase need a valid pointer :(
            if sqldata = nil then
              getmem(sqldata, 4);
            sqllen := 0;
          end else
          begin
            if sqldata = nil then
              getmem(sqldata, NewLen) else
              ReallocMem(sqldata, NewLen);
            sqllen := NewLen;
            ALMove(PWideChar(str)^, sqldata^, sqllen);
          end;
        end;
      SQL_VARYING :
        begin
          NewLen := Length(str) * 2;
          if NewLen = 0 then
          begin
            if sqldata = nil then
            begin
              getmem(sqldata, 4);
              sqllen := 2;
            end;
            PVary(sqldata).vary_length := 0;
          end else
          begin
            if sqllen = 0 then
              getmem(sqldata, NewLen+2) else
              ReallocMem(sqldata, NewLen+2);
            sqllen := NewLen + 2;
            PVary(sqldata).vary_length := NewLen;
            ALMove(PWideChar(str)^, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
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

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Int64);
  begin
    value := StrToInt64(DecodeString(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Double);
  begin
    value := StrToFloat(DecodeString(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Integer);
  begin
    value := StrToInt(DecodeString(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Single);
  begin
    value := StrToFloat(DecodeString(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Smallint);
  begin
    value := StrToInt(DecodeString(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: TDateTime);
  begin
    value := StrToDateTime(DecodeString(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Currency);
  begin
    value := StrToCurr(DecodeString(Code, Index));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: boolean);
  begin
    value := StrToInt(DecodeString(Code, Index)) <> 0;
  end;

  procedure TALFBXSQLDA.ConvertStringToDate(const Code: Smallint; Index: Word; out value: Integer);
  begin
    Value := Trunc(StrToDate(DecodeString(Code, Index)));
  end;

  procedure TALFBXSQLDA.ConvertString(const Code: Smallint; Index: Word; out value: Cardinal);
  begin
    value := StrToInt(DecodeString(Code, Index));
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

  function TALFBXSQLDA.GetIsBlob(const Index: Word): boolean;
  begin
    CheckRange(Index);
    result := ((FXSQLDA.sqlvar[Index].sqltype and not(1)) = SQL_BLOB);
  end;

  function TALFBXSQLDA.GetIsNullable(const Index: Word): boolean;
  begin
    CheckRange(Index);
    Result := (FXSQLDA.sqlvar[Index].sqlind <> nil);
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
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata), Result);
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cALFBXDateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, Result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, Result);
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
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - cALFBXDateOffset; // Only Date
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cALFBXDateOffset;
          SQL_TYPE_TIME : ; // Result := 0; What else ??
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, Result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, Result);
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
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - cALFBXDateOffset; // Only Date
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cALFBXDateOffset;
          SQL_TYPE_TIME : ; // Result := 0; What else ??
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
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
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : Result := ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cALFBXDateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
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
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - cALFBXDateOffset; // Only Date
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cALFBXDateOffset;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
          SQL_TYPE_TIME : ; // Result := 0; What else ??
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLDA.GetAsString(const Index: Word): String;
    function BoolToStr(const Value: boolean): string;
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
          SQL_SHORT  : Result := FloatToStr(PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToStr(PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToStr(PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToStr(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_VARYING   : DecodeString(SQL_VARYING, Index, Result);
          SQL_TEXT      : DecodeString(SQL_TEXT, Index, Result);
          SQL_TIMESTAMP : Result := DateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE : Result := DateToStr(PInteger(sqldata)^ - cALFBXDateOffset);
          SQL_TYPE_TIME : Result := TimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff);
          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
          SQL_LONG      : Result := IntToStr(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
          SQL_SHORT     : Result := IntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToStr(PInt64(sqldata)^);
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
          SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY: result := PISCQuad(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end
      else
        Result := cALFBXQuadNull;
  end;

  function TALFBXSQLDA.GetAsVariant(const Index: Word): Variant;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : Result := TDateTime(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE :
            begin
              Dbl := PInteger(sqldata)^ - cALFBXDateOffset;
              Result := TDateTime(Dbl);
            end;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := DecodeString(SQL_TEXT, Index);
          SQL_VARYING   : Result := DecodeString(SQL_VARYING, Index);
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

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
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TIMESTAMP : ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata), Double(Result));
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cALFBXDateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
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
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_TIMESTAMP : Result := ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cALFBXDateOffset;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
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
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^) > 0;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_SHORT     : Result := PSmallint(sqldata)^ <> 0;
          SQL_LONG      : Result := PInteger(sqldata)^ <> 0;
          SQL_INT64     : Result := PInt64(sqldata)^ <> 0;
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^) <> 0;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^) <> 0;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLDA.GetAsWideString(const Index: Word): WideString;
    function BoolToStr(const Value: boolean): string;
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
          SQL_SHORT  : Result := FloatToStr(PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToStr(PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToStr(PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToStr(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TEXT      : DecodeWideString(SQL_TEXT, Index, Result);
          SQL_VARYING   : DecodeWideString(SQL_VARYING, Index, Result);
          SQL_TIMESTAMP : Result := DateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE : Result := DateToStr(PInteger(sqldata)^ - cALFBXDateOffset);
          SQL_TYPE_TIME : Result := TimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff);
          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
          SQL_LONG      : Result := IntToStr(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
          SQL_SHORT     : Result := IntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToStr(PInt64(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

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
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TYPE_DATE : Result := PInteger(sqldata)^ - cALFBXDateOffset;
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_date - cALFBXDateOffset;
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_TEXT      : ConvertStringToDate(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertStringToDate(SQL_VARYING, Index, result);
          SQL_TYPE_TIME : Result := 0;
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
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^;
          SQL_TIMESTAMP : Result := PISCTimeStamp(sqldata).timestamp_time;
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : ConvertString(SQL_TEXT, Index, result);
          SQL_VARYING   : ConvertString(SQL_VARYING, Index, result);
          SQL_TYPE_DATE : Result := 0;
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
          SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY: PISCQuad(sqldata)^ := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
        if (sqlind <> nil) then
          if CompareMem(@Value, @cALFBXQuadNull, SizeOf(TIscQuad)) then
            sqlind^ := -1 else
            sqlind^ := 0;
      end;
  end;

  procedure TALFBXSQLDA.SetAsDateTime(const Index: Word;
    const Value: TDateTime);
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
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value)) + cALFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, DateTimeToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, DateTimeToStr(Value));
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
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + cALFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, DateToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, DateToStr(Value));
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
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := 0;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Value;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, TimeToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, TimeToStr(Value));
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
          SQL_DOUBLE : PDouble(sqldata)^   := ord(Value);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := ord(Value);
          SQL_LONG      : PInteger(sqldata)^ := ord(Value);
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := ord(Value);
          SQL_SHORT     : PSmallint(sqldata)^ := ord(Value);
          SQL_INT64     : PInt64(sqldata)^ := ord(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(ord(Value)));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(ord(Value)));
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
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + cALFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
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
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value)) + cALFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
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
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + cALFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  procedure TALFBXSQLDA.SetAsString(const Index: Word; const Value: String);
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
          SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(StrToFloat(Value) * cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Trunc(StrToFloat(Value) * cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Trunc(StrToFloat(Value) * cALFBXScaleDivisor[sqlscale]);
          SQL_DOUBLE : PDouble(sqldata)^   := StrToFloat(Value);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := StrToFloat(Value);
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(StrToDateTime(Value), PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(StrToDate(Value)) + cALFBXDateOffset);
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(StrToTime(Value)) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(StrToFloat(Value));
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := StrToFloat(Value);
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(StrToFloat(Value));
          SQL_INT64     : PInt64(sqldata)^ := Trunc(StrToFloat(Value));
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, Value);
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, Value);
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then
          sqlind^ := 0;
    end;
  end;

  procedure TALFBXSQLDA.SetAsWideString(const Index: Word;
    const Value: WideString);
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
          SQL_SHORT  : PSmallInt(sqldata)^ := Trunc(StrToFloat(Value) * cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : PInteger(sqldata)^  := Trunc(StrToFloat(Value) * cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : PInt64(sqldata)^    := Trunc(StrToFloat(Value) * cALFBXScaleDivisor[sqlscale]);
          SQL_DOUBLE : PDouble(sqldata)^   := StrToFloat(Value);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := StrToFloat(Value);
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(StrToDateTime(Value), PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(StrToDate(Value)) + cALFBXDateOffset);
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(StrToTime(Value)) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(StrToFloat(Value));
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := StrToFloat(Value);
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(StrToFloat(Value));
          SQL_INT64     : PInt64(sqldata)^ := Trunc(StrToFloat(Value));
          SQL_TEXT      : EncodeWideString(SQL_TEXT, Index, Value);
          SQL_VARYING   : EncodeWideString(SQL_VARYING, Index, Value);
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then
          sqlind^ := 0;
    end;
  end;

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
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Value + cALFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := 0;
          SQL_LONG      : PInteger(sqldata)^ := Value;
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Value;
          SQL_INT64     : PInt64(sqldata)^ := Value;
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToStr(Value));
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
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value)) + cALFBXDateOffset;
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
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
          SQL_DOUBLE : PDouble(sqldata)^   := Value;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : PDouble(sqldata)^   := Value;
          SQL_TIMESTAMP : ALFBXEncodeTimeStamp(Value, PISCTimeStamp(sqldata));
          SQL_TYPE_DATE : PInteger(sqldata)^ := Round(int(Value) + cALFBXDateOffset);
          SQL_TYPE_TIME : PCardinal(sqldata)^ := Round(Frac(Value) * cALFBXTimeCoeff);
          SQL_LONG      : PInteger(sqldata)^ := Trunc(Value);
          SQL_D_FLOAT,
          SQL_FLOAT     : PSingle(sqldata)^ := Value;
          SQL_SHORT     : PSmallint(sqldata)^ := Trunc(Value);
          SQL_INT64     : PInt64(sqldata)^ := Trunc(Value);
          SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToStr(Value));
          SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToStr(Value));
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
        if (sqlind <> nil) then sqlind^ := 0; // not null
    end;
  end;

  // TALFBXSQLDA.GetByName...

  function TALFBXSQLDA.GetFieldIndex(const name: String): Word;
  begin
    for Result := 0 to GetAllocatedFields - 1 do
      if FXSQLDA.sqlvar[Result].AliasNameLength = Length(name) then
        if StrLIComp(@FXSQLDA.sqlvar[Result].aliasname, PChar(Name),
          FXSQLDA.sqlvar[Result].AliasNameLength) = 0 then Exit;
    raise Exception.CreateFmt(cALFBX_FIELDSTRNOTFOUND, [name]);
  end;

  function TALFBXSQLDA.GetByNameAsDouble(const Name: String): Double;
  begin
    Result := GetAsDouble(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsInt64(const Name: String): Int64;
  begin
    Result := GetAsInt64(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsInteger(const Name: String): Integer;
  begin
    Result := GetAsInteger(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsSingle(const Name: String): Single;
  begin
    Result := GetAsSingle(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsSmallint(const Name: String): Smallint;
  begin
    Result := GetAsSmallint(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsString(const Name: String): String;
  begin
    Result := GetAsString(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsQuad(const Name: String): TISCQuad;
  begin
    Result := GetAsQuad(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsVariant(const Name: String): Variant;
  begin
    Result := GetAsVariant(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameIsBlob(const Name: String): boolean;
  begin
    Result := GetIsBlob(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameIsNull(const Name: String): boolean;
  begin
    Result := GetIsNull(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsDateTime(const Name: String): TDateTime;
  begin
    Result := GetAsDateTime(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsCurrency(const Name: String): Currency;
  begin
    Result := GetAsCurrency(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsBoolean(const Name: String): boolean;
  begin
    Result := GetAsBoolean(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsWideString(const Name: String): WideString;
  begin
    Result := GetAsWideString(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsDate(const Name: String): Integer;
  begin
    Result := GetAsDate(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameAsTime(const Name: String): Cardinal;
  begin
    Result := GetAsTime(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameIsNumeric(const Name: String): boolean;
  begin
    result := GetIsNumeric(GetFieldIndex(Name));
  end;

  function TALFBXSQLDA.GetByNameIsNullable(const Name: String): boolean;
  begin
    Result := GetIsNullable(GetFieldIndex(Name));
  end;

  // TALFBXSQLDA.SetByNameAs

  procedure TALFBXSQLDA.SetByNameIsNull(const Name: String;
    const Value: boolean);
  begin
    SetIsNull(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsBoolean(const Name: String;
    const Value: boolean);
  begin
     SetAsBoolean(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsDate(const Name: String;
    const Value: Integer);
  begin
    SetAsDate(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsCurrency(const Name: String;
    const Value: Currency);
  begin
    SetAsCurrency(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsDateTime(const Name: String;
    const Value: TDateTime);
  begin
    SetAsDateTime(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsDouble(const Name: String;
    const Value: Double);
  begin
    SetAsDouble(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsInt64(const Name: String;
    const Value: Int64);
  begin
    SetAsInt64(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsInteger(const Name: String;
    const Value: Integer);
  begin
    SetAsInteger(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsQuad(const Name: String;
    const Value: TISCQuad);
  begin
    SetAsQuad(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsSingle(const Name: String;
    const Value: Single);
  begin
    SetAsSingle(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsSmallint(const Name: String;
    const Value: Smallint);
  begin
    SetAsSmallint(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsString(const Name, Value: String);
  begin
    SetAsString(GetFieldIndex(Name), Value);
  end;

  procedure TALFBXSQLDA.SetByNameAsWideString(const Name: String; const Value: WideString);
  begin
    SetAsWideString(GetFieldIndex(Name), Value);
  end;

  function TALFBXSQLDA.GetFieldType(const Index: Word): TALFBXFieldType;
  begin
    CheckRange(Index);
    if (FXSQLDA.sqlvar[Index].SqlScale < 0) then
    begin
      if FXSQLDA.sqlvar[Index].sqltype and not (1) = SQL_DOUBLE  then
        Result := uftDoublePrecision else
        Result := uftNumeric;
    end else
    case FXSQLDA.sqlvar[Index].sqltype and not (1) of
      SQL_TEXT        : Result := uftChar;
      SQL_VARYING     : Result := uftVarchar;
      SQL_SHORT       : Result := uftSmallint;
      SQL_LONG        : Result := uftInteger;
      SQL_FLOAT,
      SQL_D_FLOAT     : Result := uftFloat;
      SQL_DOUBLE      : Result := uftDoublePrecision;
      SQL_TIMESTAMP   : Result := uftTimestamp;
      SQL_BLOB        : Result := uftBlob;
      SQL_QUAD        : Result := uftQuad;
      SQL_TYPE_TIME   : Result := uftTime;
      SQL_TYPE_DATE   : Result := uftDate;
      SQL_INT64       : Result := uftInt64;
      SQL_ARRAY       : Result := uftArray;
    else
      Result := uftUnKnown;
    end;
  end;



{ TALFBXSQLResult }

  constructor TALFBXSQLResult.Create(Fields: SmallInt = 0;
    CachedFetch: Boolean = False;
    FetchBlobs: boolean = false;
    BufferChunks: Cardinal = 1000);
  begin
    inherited Create;
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
      inc(Integer(FXSQLDA.sqlvar[i].sqldata), Integer(FDataBuffer));
      if (FXSQLDA.sqlvar[i].sqlind <> nil) then
        inc(Integer(FXSQLDA.sqlvar[i].sqlind), Integer(FDataBuffer));
    end;
  end;

  procedure TALFBXSQLResult.SaveToStream(Stream: TStream);
  var
    Count, i, j: Integer;
    BlobData: PALFBXBlobData;
  begin
    Stream.Write(FCachedFetch, SizeOf(FCachedFetch));
    Stream.Write(FFetchBlobs, SizeOf(FFetchBlobs));

    Stream.Write(FXSQLDA.sqln, SizeOf(FXSQLDA.sqln));
    Stream.Write(FXSQLDA^, XSQLDA_LENGTH(FXSQLDA.sqln)); // MetaData

    // array informations
    count := length(FArrayInfos);
    Stream.Write(Count, SizeOf(Count));
    for i := 0 to Count - 1 do
      Stream.Write(FArrayInfos[i], SizeOf(TALFBXArrayInfo));

    Count := RecordCount;
    Stream.Write(Count, SizeOf(Count));
    if (Stream is TCustomMemoryStream) then
      Stream.Size := Stream.Size + (FDataBufferLength * Count);
    for i := 0 to Count - 1 do
    begin
      Stream.Write(FRecordPool[i]^, FDataBufferLength);
      for j := 0 to Length(FBlobsIndex) - 1 do
      begin
        BlobData := Pointer(Integer(FRecordPool[I]) + (integer(GetDataQuadOffset(FBlobsIndex[J])) - Integer(FDataBuffer)));
        Stream.Write(BlobData.Buffer^,BlobData.Size);
      end;
    end;
  end;

  procedure TALFBXSQLResult.LoadFromStream(Stream: TStream);
  var
    Fields: SmallInt;
    Count, i, j: Integer;
    BlobData: PALFBXBlobData;
  begin
    // CleanUp
    ClearRecords;
    if (not FCachedFetch) and FFetchBlobs then
      FreeBlobs(FDataBuffer);

    Stream.Read(FCachedFetch, SizeOf(FCachedFetch));
    Stream.Read(FFetchBlobs, SizeOf(FFetchBlobs));

    Stream.Read(Fields, SizeOf(Fields));
    SetAllocatedFields(Fields);
    Stream.Read(FXSQLDA^, XSQLDA_LENGTH(Fields));

    // array informations
    Stream.Read(Count, SizeOf(Count));
    SetLength(FArrayInfos, Count);
    for i := 0 to Count - 1 do
      Stream.Read(FArrayInfos[i], SizeOf(TALFBXArrayInfo));

    // realloc & index buffer
    AllocateDataBuffer;
    Stream.Read(Count, SizeOf(Count));
    FBufferChunks := Count; // Inprove memory allocation
    for i := 0 to Count - 1 do
    begin
      Stream.Read(FDataBuffer^, FDataBufferLength);
      for j := 0 to Length(FBlobsIndex) - 1 do
      begin
        BlobData := GetDataQuadOffset(FBlobsIndex[j]);
        if BlobData.Size > 0 then
        begin
          GetMem(BlobData.Buffer, BlobData.Size);
          Stream.Read(BlobData.Buffer^, BlobData.Size);
        end else
          BlobData.Buffer := nil;
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
      BlobData := Pointer(Integer(Buffer) + (integer(GetDataQuadOffset(FBlobsIndex[I])) - Integer(FDataBuffer)));
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

  procedure TALFBXSQLResult.ReadBlob(const Index: Word; var str: string);
  var BlobData: PALFBXBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    SetLength(str, BlobData.Size);
    ALMove(BlobData.Buffer^, PChar(Str)^, BlobData.Size);
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

  procedure TALFBXSQLResult.ReadBlob(const Index: Word; var Value: Variant);
  var
    PData: Pointer;
    BlobData: PALFBXBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    Value := VarArrayCreate([0, BlobData.Size-1], varByte);
    PData := VarArrayLock(Value);
    try
      ALMove(BlobData.Buffer^, PData^, BlobData.Size);
    finally
      VarArrayUnlock(Value);
    end;
  end;

  procedure TALFBXSQLResult.ReadBlob(const Index: Word; var str: WideString);
  var BlobData: PALFBXBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    SetLength(str, BlobData.Size);
    ALMove(BlobData.Buffer^, PWideChar(Str)^, BlobData.Size);
  end;

  procedure TALFBXSQLResult.ReadBlob(const Index: Word; Data: Pointer);
  var BlobData: PALFBXBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    ALMove(BlobData.Buffer^, Data^, BlobData.Size);
  end;

  procedure TALFBXSQLResult.ReadBlob(const name: string; Data: Pointer);
  begin
    ReadBlob(GetFieldIndex(name), Data);
  end;

  procedure TALFBXSQLResult.ReadBlob(const name: string; var str: WideString);
  begin
    ReadBlob(GetFieldIndex(name), str);
  end;

  procedure TALFBXSQLResult.ReadBlob(const name: string; var str: string);
  begin
    ReadBlob(GetFieldIndex(name), str);
  end;

  procedure TALFBXSQLResult.ReadBlob(const name: string; Stream: TStream);
  begin
    ReadBlob(GetFieldIndex(name), Stream);
  end;

  procedure TALFBXSQLResult.ReadBlob(const name: string; var Value: Variant);
  begin
    ReadBlob(GetFieldIndex(name), Value);
  end;

  function TALFBXSQLResult.GetBlobSize(const Index: Word): Cardinal;
  var BlobData: PALFBXBlobData;
  begin
    CheckRange(Index);
    if not FFetchBlobs then
      raise Exception.Create(cALFBX_FETCHBLOBNOTSET);
    BlobData := GetDataQuadOffset(Index);
    Result := BlobData.Size;
  end;

  function TALFBXSQLResult.GetAsString(const Index: Word): String;
    function BoolToStr(const Value: boolean): string;
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
          SQL_SHORT  : Result := FloatToStr(PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToStr(PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToStr(PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToStr(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
          SQL_TIMESTAMP : Result := DateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE : Result := DateToStr(PInteger(sqldata)^ - cALFBXDateOffset);
          SQL_TYPE_TIME : Result := TimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff);
          SQL_LONG      : Result := IntToStr(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
          SQL_SHORT     : Result := IntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToStr(PInt64(sqldata)^);
          SQL_TEXT      : DecodeString(SQL_TEXT, Index, Result);
          SQL_VARYING   : DecodeString(SQL_VARYING, Index, Result);
          SQL_BLOB      : ReadBlob(Index, Result);
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLResult.GetAsWideString(const Index: Word): WideString;
    function BoolToStr(const Value: boolean): string;
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
          SQL_SHORT  : Result := FloatToStr(PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToStr(PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToStr(PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToStr(PDouble(sqldata)^);
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
          SQL_TIMESTAMP : Result := DateTimeToStr(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE : Result := DateToStr(PInteger(sqldata)^ - cALFBXDateOffset);
          SQL_TYPE_TIME : Result := TimeToStr(PCardinal(sqldata)^ / cALFBXTimeCoeff);
          SQL_LONG      : Result := IntToStr(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
          SQL_SHORT     : Result := IntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToStr(PInt64(sqldata)^);
          SQL_TEXT      : DecodeWideString(SQL_TEXT, Index, Result);
          SQL_VARYING   : DecodeWideString(SQL_VARYING, Index, Result);
          SQL_BLOB      : ReadBlob(Index, Result);
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLResult.GetAsVariant(const Index: Word): Variant;
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
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / cALFBXScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / cALFBXScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / cALFBXScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EALFBXConvertError.Create(cALFBX_UNEXPECTEDERROR);
        end;
      end else
        case ASQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_TIMESTAMP : Result := TDateTime(ALFBXDecodeTimeStamp(PISCTimeStamp(sqldata)));
          SQL_TYPE_DATE :
            begin
              Dbl := PInteger(sqldata)^ - cALFBXDateOffset;
              Result := TDateTime(Dbl);
            end;
          SQL_TYPE_TIME : Result := PCardinal(sqldata)^ / cALFBXTimeCoeff;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := DecodeString(SQL_TEXT, Index);
          SQL_VARYING   : Result := DecodeString(SQL_VARYING, Index);
          SQL_BLOB      : ReadBlob(Index, Result);
        else
          raise EALFBXConvertError.Create(cALFBX_CASTERROR);
        end;
    end;
  end;

  function TALFBXSQLResult.GetUniqueRelationName: string;
  var
    i: integer;
  begin
    result := '';
    if FXSQLDA.sqln > 1 then
      for i := 0 to FXSQLDA.sqln - 2 do
        if not ((FXSQLDA.sqlvar[i].RelNameLength = FXSQLDA.sqlvar[i+1].RelNameLength) and
          (ALCompareText(FXSQLDA.sqlvar[i].RelName, FXSQLDA.sqlvar[i+1].RelName) = 0)) then
            exit;
    if FXSQLDA.sqln > 0 then
      SetString(Result, FXSQLDA.sqlvar[0].RelName, FXSQLDA.sqlvar[0].RelNameLength);
  end;

  function TALFBXSQLResult.GetDataQuadOffset(const index: word): Pointer;
  begin
    result := FXSQLDA.sqlvar[index].SqlData;
    inc(integer(result), sizeof(TIscQuad));
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

{ TALFBXPoolStream }

function TALFBXPoolStream.Add: Pointer;
var
  item: integer;
begin
  item := FItemCount;
  SetSize((item + 1) * FItemSize);
  Result := Pointer(Integer(FPages[item div FItemsInPage]) + (Item mod FItemsInPage) * FItemSize);
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

destructor TALFBXPoolStream.Destroy;
begin
  Clear;
  inherited;
end;

function TALFBXPoolStream.Get(Item: Integer): Pointer;
begin
  assert(Item * FItemSize <= FSize);
  Result := Pointer(Integer(FPages[Item div FItemsInPage]) + (Item mod FItemsInPage) * FItemSize);
end;

procedure TALFBXPoolStream.LoadFromFile(const FileName: string);
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
        p := Pointer(Integer(FPages[FPosition div FPageSize]) + (FPosition mod FPageSize));
        ALMove(p^, c^, n);
        dec(count, n);
        inc(Integer(c), n);
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

procedure TALFBXPoolStream.SaveToFile(const FileName: string);
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

function TALFBXPoolStream.Seek(Item: Integer): Longint;
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
        p := Pointer(Integer(FPages[FPosition div FPageSize]) + (FPosition mod FPageSize));
        ALMove(c^, p^, n);
        dec(count, n);
        inc(Integer(c), n);
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

  constructor TALFBXSQLParams.Create;
  begin
    inherited Create;
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

  function TALFBXSQLParams.GetFieldName(const Index: Word): string;
  begin
    CheckRange(Index);
    SetString(Result, FXSQLDA.sqlvar[Index].ParamName,
      FXSQLDA.sqlvar[Index].ParamNameLength);
  end;

  procedure TALFBXSQLParams.AddFieldType(const Name: string; FieldType: TALFBXFieldType;
    Scale: TALFBXScale = 1; Precision: byte = 0);
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
    end;
  end;

  procedure TALFBXSQLParams.SetFieldType(const Index: Word; Size: Integer; Code,
    Scale: Smallint);
  var i: Word;
  begin
    CheckRange(Index);
    with FXSQLDA.sqlvar[Index] do
      if Init then  // need to be set, cf addfield
      begin
        Init := False; // don't need to be set
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

  function TALFBXSQLParams.Parse(const SQL: string): string;
  const
    Identifiers: set of char = ['a'..'z', 'A'..'Z', '0'..'9', '_', '$'];
  var
    Src: PChar;
    Dest, idlen: Word;

    procedure next;
    begin
      inc(dest);
      Result[dest] := Src^;
      inc(Src);
    end;

    procedure Skip(c: char);
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
                while not(Src^ in [#0, #13, #10]) do
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
                    if (Src[idlen] in [#0, '"']) then
                      Break else
                      inc(idlen);
                end else
                // unquoted identifiers
                  while (Src[idlen] in Identifiers) do inc(idlen);
                AddField(ALcopyStr(Src, 1, idlen));
                inc(Src, idlen);
                if Src^ = '"' then inc(Src);
              end;
        // skip everything when begin identifier found !
        // in procedures
        'b','B':
          begin
            if not ((dest > 0) and (src[-1]
              in Identifiers)) and (ALCompareText(ALcopyStr(Src, 0, 5), 'begin') = 0) and
                not (Src[5] in Identifiers) then
                  while (Src^ <> #0) do Next else next;
          end;
        // declare should also stop param parsing, as a declare cursor statement
        // may contain variables.
        'd','D':
          begin
            if not ((dest > 0) and (src[-1]
              in Identifiers)) and (ALCompareText(ALcopyStr(Src, 0, 7), 'declare') = 0) and
                not (Src[7] in Identifiers) then
                  while (Src^ <> #0) do Next else next;
          end; 
      else
        next;
      end;
  end;

  function TALFBXSQLParams.GetFieldType(const Index: Word): TALFBXFieldType;
  begin
    if IsNull[Index] and FXSQLDA.sqlvar[Index].Init then
      Result := uftUnKnown else
      Result := inherited GetFieldType(Index);
  end;

  function TALFBXSQLParams.GetFieldIndex(const name: String): Word;
  begin
    if not FindParam(name, Result) then
      raise Exception.CreateFmt(cALFBX_PARAMSTRNOTFOUND, [name]);
  end;

  function TALFBXSQLParams.FindParam(const name: string; out Index: Word): boolean;
  var Field: Smallint;
  begin
    for Field := 0 to FXSQLDA.sqln - 1 do
      if FXSQLDA.sqlvar[Field].ParamNameLength = Length(name) then
        if StrLIComp(@FXSQLDA.sqlvar[Field].ParamName, PChar(Name),
          FXSQLDA.sqlvar[Field].ParamNameLength) = 0 then
          begin
            Result := true;
            Index  := Field;
            Exit;
          end;
    Result := False;
  end;

  function TALFBXSQLParams.AddField(const Name: string): Word;
  var
    num: Word;
    len: Cardinal;
    p: PALFBXSQLVar;
  begin
    len := Length(Name);
    if len > cALFBXMaxParamLength then
      raise Exception.CreateFmt(cALFBX_SIZENAME, [Name]);

    Result := FXSQLDA.sqln;
    if (len > 0) and FindParam(Name, num) then
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
      p^.Init := True;
      p^.ID := FParamCount;
      p^.ParamNameLength := len;
      if p^.ParamNameLength > 0 then
        ALMove(Pointer(Name)^, p^.ParamName[0], p^.ParamNameLength);
      p^.sqltype    := SQL_TEXT + 1; // tip: don't allocate memory if not defined
      p^.sqlscale   := 0;
      p^.sqlsubtype := 0;
      p^.sqllen     := 0;
      p^.sqldata    := nil;
      GetMem(p^.sqlind, 2); // Can be NULL
      p^.sqlind^ := -1; // NULL
    end;
  end;

  procedure TALFBXSQLParams.Clear;
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

 // TALFBXSQLParams.SetAs...

  procedure TALFBXSQLParams.SetAsQuad(const Index: Word; const Value: TISCQuad);
  begin
    SetFieldType(Index, sizeof(TISCQuad), SQL_QUAD + 1, 0);
    inherited;
  end;

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

  procedure TALFBXSQLParams.SetAsString(const Index: Word; const Value: String);
  begin
    SetFieldType(Index, Length(Value), SQL_TEXT + 1, 0);
    inherited;
  end;

  procedure TALFBXSQLParams.SetAsWideString(const Index: Word;
    const Value: WideString);
  begin
    SetFieldType(Index, Length(Value), SQL_TEXT + 1, 0);
    inherited;
  end;

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
