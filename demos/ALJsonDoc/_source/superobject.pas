(*
 *                         Super Object Toolkit
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Embarcadero Technologies Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 *
 * This unit is inspired from the json c lib:
 *   Michael Clark <michael@metaparadigm.com>
 *   http://oss.metaparadigm.com/json-c/
 *
 *  CHANGES:
 *  v1.2
 *   + support of currency data type
 *   + right trim unquoted string
 *   + read Unicode Files and streams (Litle Endian with BOM)
 *   + Fix bug on javadate functions + windows nt compatibility
 *   + Now you can force to parse only the canonical syntax of JSON using the stric parameter
 *   + Delphi 2010 RTTI marshalling
 *  v1.1
 *   + Double licence MPL or LGPL.
 *   + Delphi 2009 compatibility & Unicode support.
 *   + AsString return a string instead of PChar.
 *   + Escaped and Unascaped JSON serialiser.
 *   + Missed FormFeed added \f
 *   - Removed @ trick, uses forcepath() method instead.
 *   + Fixed parse error with uppercase E symbol in numbers.
 *   + Fixed possible buffer overflow when enlarging array.
 *   + Added "delete", "pack", "insert" methods for arrays and/or objects
 *   + Multi parametters when calling methods
 *   + Delphi Enumerator (for obj1 in obj2 do ...)
 *   + Format method ex: obj.format('<%name%>%tab[1]%</%name%>')
 *   + ParseFile and ParseStream methods
 *   + Parser now understand hexdecimal c syntax ex: \xFF
 *   + Null Object Design Patern (ex: for obj in values.N['path'] do ...)
 *  v1.0
 *   + renamed class
 *   + interfaced object
 *   + added a new data type: the method
 *   + parser can now evaluate properties and call methods
 *   - removed obselet rpc class
 *   - removed "find" method, now you can use "parse" method instead
 *  v0.6
 *   + refactoring
 *  v0.5
 *   + new find method to get or set value using a path syntax
 *       ex: obj.s['obj.prop[1]'] := 'string value';
 *           obj.a['@obj.array'].b[n] := true; // @ -> create property if necessary
 *  v0.4
 *   + bug corrected: AVL tree badly balanced.
 *  v0.3
 *   + New validator partially based on the Kwalify syntax.
 *   + extended syntax to parse unquoted fields.
 *   + Freepascal compatibility win32/64 Linux32/64.
 *   + JavaToDelphiDateTime and DelphiToJavaDateTime improved for UTC.
 *   + new TJsonObject.Compare function.
 *  v0.2
 *   + Hashed string list replaced with a faster AVL tree
 *   + JsonInt data type can be changed to int64
 *   + JavaToDelphiDateTime and DelphiToJavaDateTime helper fonctions
 *   + from json-c v0.7
 *     + Add escaping of backslash to json output
 *     + Add escaping of foward slash on tokenizing and output
 *     + Changes to internal tokenizer from using recursion to
 *       using a depth state structure to allow incremental parsing
 *  v0.1
 *   + first release
 *)

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$DEFINE SUPER_METHOD}
{.$DEFINE DEBUG} // track memory leack


{$if defined(VER210) or defined(VER220)}
  {$define VER210ORGREATER}
{$ifend}

{$if defined(VER230) or defined(VER240)  or defined(VER250) or
     defined(VER260) or defined(VER270)  or defined(VER280)}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$ifend}

{$if defined(FPC) or defined(VER170) or defined(VER180) or defined(VER190)
  or defined(VER200) or defined(VER210ORGREATER)}
  {$DEFINE HAVE_INLINE}
{$ifend}

{$if defined(VER210ORGREATER)}
  {$define HAVE_RTTI}
{$ifend}

{$if defined(VER230ORGREATER)}
  {$define NEED_FORMATSETTINGS}
{$ifend}

{$if defined(FPC) and defined(VER2_6)}
  {$define NEED_FORMATSETTINGS}
{$ifend}

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

unit superobject;

interface
uses
  Classes, supertypes
{$IFDEF HAVE_RTTI}
  ,Generics.Collections, RTTI, TypInfo
{$ENDIF}
  ;

const
  SUPER_ARRAY_LIST_DEFAULT_SIZE = 32;
  SUPER_TOKENER_MAX_DEPTH = 32;

  SUPER_AVL_MAX_DEPTH = sizeof(longint) * 8;
  SUPER_AVL_MASK_HIGH_BIT = not ((not longword(0)) shr 1);

type
  // forward declarations
  TSuperObject = class;
  ISuperObject = interface;
  TSuperArray = class;

(* AVL Tree
 *  This is a "special" autobalanced AVL tree
 *  It use a hash value for fast compare
 *)

{$IFDEF SUPER_METHOD}
  TSuperMethod = procedure(const This, Params: ISuperObject; var Result: ISuperObject);
{$ENDIF}


  TSuperAvlBitArray = set of 0..SUPER_AVL_MAX_DEPTH - 1;

  TSuperAvlSearchType = (stEQual, stLess, stGreater);
  TSuperAvlSearchTypes = set of TSuperAvlSearchType;
  TSuperAvlIterator = class;

  TSuperAvlEntry = class
  private
    FGt, FLt: TSuperAvlEntry;
    FBf: integer;
    FHash: Cardinal;
    FName: SOString;
    FPtr: Pointer;
    function GetValue: ISuperObject;
    procedure SetValue(const val: ISuperObject);
  public
    class function Hash(const k: SOString): Cardinal; virtual;
    constructor Create(const AName: SOString; Obj: Pointer); virtual;
    property Name: SOString read FName;
    property Ptr: Pointer read FPtr;
    property Value: ISuperObject read GetValue write SetValue;
  end;

  TSuperAvlTree = class
  private
    FRoot: TSuperAvlEntry;
    FCount: Integer;
    function balance(bal: TSuperAvlEntry): TSuperAvlEntry;
  protected
    procedure doDeleteEntry(Entry: TSuperAvlEntry; all: boolean); virtual;
    function CompareNodeNode(node1, node2: TSuperAvlEntry): integer; virtual;
    function CompareKeyNode(const k: SOString; h: TSuperAvlEntry): integer; virtual;
    function Insert(h: TSuperAvlEntry): TSuperAvlEntry; virtual;
    function Search(const k: SOString; st: TSuperAvlSearchTypes = [stEqual]): TSuperAvlEntry; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsEmpty: boolean;
    procedure Clear(all: boolean = false); virtual;
    procedure Pack(all: boolean);
    function Delete(const k: SOString): ISuperObject;
    function GetEnumerator: TSuperAvlIterator;
    property count: Integer read FCount;
  end;

  TSuperTableString = class(TSuperAvlTree)
  protected
    procedure doDeleteEntry(Entry: TSuperAvlEntry; all: boolean); override;
    procedure PutO(const k: SOString; const value: ISuperObject);
    function GetO(const k: SOString): ISuperObject;
    procedure PutS(const k: SOString; const value: SOString);
    function GetS(const k: SOString): SOString;
    procedure PutI(const k: SOString; value: SuperInt);
    function GetI(const k: SOString): SuperInt;
    procedure PutD(const k: SOString; value: Double);
    function GetD(const k: SOString): Double;
    procedure PutB(const k: SOString; value: Boolean);
    function GetB(const k: SOString): Boolean;
{$IFDEF SUPER_METHOD}
    procedure PutM(const k: SOString; value: TSuperMethod);
    function GetM(const k: SOString): TSuperMethod;
{$ENDIF}
    procedure PutN(const k: SOString; const value: ISuperObject);
    function GetN(const k: SOString): ISuperObject;
    procedure PutC(const k: SOString; value: Currency);
    function GetC(const k: SOString): Currency;
  public
    property O[const k: SOString]: ISuperObject read GetO write PutO; default;
    property S[const k: SOString]: SOString read GetS write PutS;
    property I[const k: SOString]: SuperInt read GetI write PutI;
    property D[const k: SOString]: Double read GetD write PutD;
    property B[const k: SOString]: Boolean read GetB write PutB;
{$IFDEF SUPER_METHOD}
    property M[const k: SOString]: TSuperMethod read GetM write PutM;
{$ENDIF}
    property N[const k: SOString]: ISuperObject read GetN write PutN;
    property C[const k: SOString]: Currency read GetC write PutC;

    function GetValues: ISuperObject;
    function GetNames: ISuperObject;
    function Find(const k: SOString; var value: ISuperObject): Boolean;
    function Exists(const k: SOString): Boolean;
  end;

  TSuperAvlIterator = class
  private
    FTree: TSuperAvlTree;
    FBranch: TSuperAvlBitArray;
    FDepth: LongInt;
    FPath: array[0..SUPER_AVL_MAX_DEPTH - 2] of TSuperAvlEntry;
  public
    constructor Create(tree: TSuperAvlTree); virtual;
    procedure Search(const k: SOString; st: TSuperAvlSearchTypes = [stEQual]);
    procedure First;
    procedure Last;
    function GetIter: TSuperAvlEntry;
    procedure Next;
    procedure Prior;
    // delphi enumerator
    function MoveNext: Boolean;
    property Current: TSuperAvlEntry read GetIter;
  end;

  TSuperObjectArray = array[0..(high(Integer) div sizeof(TSuperObject))-1] of ISuperObject;
  PSuperObjectArray = ^TSuperObjectArray;

  TSuperArray = class
  private
    FArray: PSuperObjectArray;
    FLength: Integer;
    FSize: Integer;
    procedure Expand(max: Integer);
  protected
    function GetO(const index: integer): ISuperObject;
    procedure PutO(const index: integer; const Value: ISuperObject);
    function GetB(const index: integer): Boolean;
    procedure PutB(const index: integer; Value: Boolean);
    function GetI(const index: integer): SuperInt;
    procedure PutI(const index: integer; Value: SuperInt);
    function GetD(const index: integer): Double;
    procedure PutD(const index: integer; Value: Double);
    function GetC(const index: integer): Currency;
    procedure PutC(const index: integer; Value: Currency);
    function GetS(const index: integer): SOString;
    procedure PutS(const index: integer; const Value: SOString);
{$IFDEF SUPER_METHOD}
    function GetM(const index: integer): TSuperMethod;
    procedure PutM(const index: integer; Value: TSuperMethod);
{$ENDIF}
    function GetN(const index: integer): ISuperObject;
    procedure PutN(const index: integer; const Value: ISuperObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const Data: ISuperObject): Integer; overload;
    function Add(Data: SuperInt): Integer; overload;
    function Add(const Data: SOString): Integer; overload;
    function Add(Data: Boolean): Integer; overload;
    function Add(Data: Double): Integer; overload;
    function AddC(const Data: Currency): Integer;
    function Delete(index: Integer): ISuperObject;
    procedure Insert(index: Integer; const value: ISuperObject);
    procedure Clear(all: boolean = false);
    procedure Pack(all: boolean);
    property Length: Integer read FLength;

    property N[const index: integer]: ISuperObject read GetN write PutN;
    property O[const index: integer]: ISuperObject read GetO write PutO; default;
    property B[const index: integer]: boolean read GetB write PutB;
    property I[const index: integer]: SuperInt read GetI write PutI;
    property D[const index: integer]: Double read GetD write PutD;
    property C[const index: integer]: Currency read GetC write PutC;
    property S[const index: integer]: SOString read GetS write PutS;
{$IFDEF SUPER_METHOD}
    property M[const index: integer]: TSuperMethod read GetM write PutM;
{$ENDIF}
  end;

  TSuperWriter = class
  public
    // abstact methods to overide
    function Append(buf: PSOChar; Size: Integer): Integer; overload; virtual; abstract;
    function Append(buf: PSOChar): Integer; overload; virtual; abstract;
    procedure Reset; virtual; abstract;
  end;

  TSuperWriterString = class(TSuperWriter)
  private
    FBuf: PSOChar;
    FBPos: integer;
    FSize: integer;
  public
    function Append(buf: PSOChar; Size: Integer): Integer; overload; override;
    function Append(buf: PSOChar): Integer; overload; override;
    procedure Reset; override;
    procedure TrimRight;
    constructor Create; virtual;
    destructor Destroy; override;
    function GetString: SOString;
    property Data: PSOChar read FBuf;
    property Size: Integer read FSize;
    property Position: integer read FBPos;
  end;

  TSuperWriterStream = class(TSuperWriter)
  private
    FStream: TStream;
  public
    function Append(buf: PSOChar): Integer; override;
    procedure Reset; override;
    constructor Create(AStream: TStream); reintroduce; virtual;
  end;

  TSuperAnsiWriterStream = class(TSuperWriterStream)
  public
    function Append(buf: PSOChar; Size: Integer): Integer; override;
  end;

  TSuperUnicodeWriterStream = class(TSuperWriterStream)
  public
    function Append(buf: PSOChar; Size: Integer): Integer; override;
  end;

  TSuperWriterFake = class(TSuperWriter)
  private
    FSize: Integer;
  public
    function Append(buf: PSOChar; Size: Integer): Integer; override;
    function Append(buf: PSOChar): Integer; override;
    procedure Reset; override;
    constructor Create; reintroduce; virtual;
    property size: integer read FSize;
  end;

  TSuperWriterSock = class(TSuperWriter)
  private
    FSocket: longint;
    FSize: Integer;
  public
    function Append(buf: PSOChar; Size: Integer): Integer; override;
    function Append(buf: PSOChar): Integer; override;
    procedure Reset; override;
    constructor Create(ASocket: longint); reintroduce; virtual;
    property Socket: longint read FSocket;
    property Size: Integer read FSize;
  end;

  TSuperTokenizerError = (
    teSuccess,
    teContinue,
    teDepth,
    teParseEof,
    teParseUnexpected,
    teParseNull,
    teParseBoolean,
    teParseNumber,
    teParseArray,
    teParseObjectKeyName,
    teParseObjectKeySep,
    teParseObjectValueSep,
    teParseString,
    teParseComment,
    teEvalObject,
    teEvalArray,
    teEvalMethod,
    teEvalInt
  );

  TSuperTokenerState = (
    tsEatws,
    tsStart,
    tsFinish,
    tsNull,
    tsCommentStart,
    tsComment,
    tsCommentEol,
    tsCommentEnd,
    tsString,
    tsStringEscape,
    tsIdentifier,
    tsEscapeUnicode,
    tsEscapeHexadecimal,
    tsBoolean,
    tsNumber,
    tsArray,
    tsArrayAdd,
    tsArraySep,
    tsObjectFieldStart,
    tsObjectField,
    tsObjectUnquotedField,
    tsObjectFieldEnd,
    tsObjectValue,
    tsObjectValueAdd,
    tsObjectSep,
    tsEvalProperty,
    tsEvalArray,
    tsEvalMethod,
    tsParamValue,
    tsParamPut,
    tsMethodValue,
    tsMethodPut
  );

  PSuperTokenerSrec = ^TSuperTokenerSrec;
  TSuperTokenerSrec = record
    state, saved_state: TSuperTokenerState;
    obj: ISuperObject;
    current: ISuperObject;
    field_name: SOString;
    parent: ISuperObject;
    gparent: ISuperObject;
  end;

  TSuperTokenizer = class
  public
    str: PSOChar;
    pb: TSuperWriterString;
    depth, is_double, floatcount, st_pos, char_offset: Integer;
    err:  TSuperTokenizerError;
    ucs_char: Word;
    quote_char: SOChar;
    stack: array[0..SUPER_TOKENER_MAX_DEPTH-1] of TSuperTokenerSrec;
    line, col: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ResetLevel(adepth: integer);
    procedure Reset;
  end;

  // supported object types
  TSuperType = (
    stNull,
    stBoolean,
    stDouble,
    stCurrency,
    stInt,
    stObject,
    stArray,
    stString
{$IFDEF SUPER_METHOD}
    ,stMethod
{$ENDIF}
  );

  TSuperValidateError = (
    veRuleMalformated,
    veFieldIsRequired,
    veInvalidDataType,
    veFieldNotFound,
    veUnexpectedField,
    veDuplicateEntry,
    veValueNotInEnum,
    veInvalidLength,
    veInvalidRange
  );

  TSuperFindOption = (
    foCreatePath,
    foPutValue,
    foDelete
{$IFDEF SUPER_METHOD}
    ,foCallMethod
{$ENDIF}
  );

  TSuperFindOptions = set of TSuperFindOption;
  TSuperCompareResult = (cpLess, cpEqu, cpGreat, cpError);
  TSuperOnValidateError = procedure(sender: Pointer; error: TSuperValidateError; const objpath: SOString);

  TSuperEnumerator = class
  private
    FObj: ISuperObject;
    FObjEnum: TSuperAvlIterator;
    FCount: Integer;
  public
    constructor Create(const obj: ISuperObject); virtual;
    destructor Destroy; override;
    function MoveNext: Boolean;
    function GetCurrent: ISuperObject;
    property Current: ISuperObject read GetCurrent;
  end;

  ISuperObject = interface
  ['{4B86A9E3-E094-4E5A-954A-69048B7B6327}']
    function GetEnumerator: TSuperEnumerator;
    function GetDataType: TSuperType;
    function GetProcessing: boolean;
    procedure SetProcessing(value: boolean);
    function ForcePath(const path: SOString; dataType: TSuperType = stObject): ISuperObject;
    function Format(const str: SOString; BeginSep: SOChar = '%'; EndSep: SOChar = '%'): SOString;

    function GetO(const path: SOString): ISuperObject;
    procedure PutO(const path: SOString; const Value: ISuperObject);
    function GetB(const path: SOString): Boolean;
    procedure PutB(const path: SOString; Value: Boolean);
    function GetI(const path: SOString): SuperInt;
    procedure PutI(const path: SOString; Value: SuperInt);
    function GetD(const path: SOString): Double;
    procedure PutC(const path: SOString; Value: Currency);
    function GetC(const path: SOString): Currency;
    procedure PutD(const path: SOString; Value: Double);
    function GetS(const path: SOString): SOString;
    procedure PutS(const path: SOString; const Value: SOString);
{$IFDEF SUPER_METHOD}
    function GetM(const path: SOString): TSuperMethod;
    procedure PutM(const path: SOString; Value: TSuperMethod);
{$ENDIF}
    function GetA(const path: SOString): TSuperArray;

    // Null Object Design patern
    function GetN(const path: SOString): ISuperObject;
    procedure PutN(const path: SOString; const Value: ISuperObject);

    // Writers
    function Write(writer: TSuperWriter; indent: boolean; escape: boolean; level: integer): Integer;
    function SaveTo(stream: TStream; indent: boolean = false; escape: boolean = true): integer; overload;
    function SaveTo(const FileName: string; indent: boolean = false; escape: boolean = true): integer; overload;
    function SaveTo(socket: longint; indent: boolean = false; escape: boolean = true): integer; overload;
    function CalcSize(indent: boolean = false; escape: boolean = true): integer;

    // convert
    function AsBoolean: Boolean;
    function AsInteger: SuperInt;
    function AsDouble: Double;
    function AsCurrency: Currency;
    function AsString: SOString;
    function AsArray: TSuperArray;
    function AsObject: TSuperTableString;
{$IFDEF SUPER_METHOD}
    function AsMethod: TSuperMethod;
{$ENDIF}
    function AsJSon(indent: boolean = false; escape: boolean = true): SOString;

    procedure Clear(all: boolean = false);
    procedure Pack(all: boolean = false);

    property N[const path: SOString]: ISuperObject read GetN write PutN;
    property O[const path: SOString]: ISuperObject read GetO write PutO; default;
    property B[const path: SOString]: boolean read GetB write PutB;
    property I[const path: SOString]: SuperInt read GetI write PutI;
    property D[const path: SOString]: Double read GetD write PutD;
    property C[const path: SOString]: Currency read GetC write PutC;
    property S[const path: SOString]: SOString read GetS write PutS;
{$IFDEF SUPER_METHOD}
    property M[const path: SOString]: TSuperMethod read GetM write PutM;
{$ENDIF}
    property A[const path: SOString]: TSuperArray read GetA;

{$IFDEF SUPER_METHOD}
    function call(const path: SOString; const param: ISuperObject = nil): ISuperObject; overload;
    function call(const path, param: SOString): ISuperObject; overload;
{$ENDIF}

    // clone a node
    function Clone: ISuperObject;
    function Delete(const path: SOString): ISuperObject;
    // merges tow objects of same type, if reference is true then nodes are not cloned
    procedure Merge(const obj: ISuperObject; reference: boolean = false); overload;
    procedure Merge(const str: SOString); overload;

    // validate methods
    function Validate(const rules: SOString; const defs: SOString = ''; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean; overload;
    function Validate(const rules: ISuperObject; const defs: ISuperObject = nil; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean; overload;

    // compare
    function Compare(const obj: ISuperObject): TSuperCompareResult; overload;
    function Compare(const str: SOString): TSuperCompareResult; overload;

    // the data type
    function IsType(AType: TSuperType): boolean;
    property DataType: TSuperType read GetDataType;
    property Processing: boolean read GetProcessing write SetProcessing;

    function GetDataPtr: Pointer;
    procedure SetDataPtr(const Value: Pointer);
    property DataPtr: Pointer read GetDataPtr write SetDataPtr;
  end;

  TSuperObject = class(TObject, ISuperObject)
  private
    FRefCount: Integer;
    FProcessing: boolean;
    FDataType: TSuperType;
    FDataPtr: Pointer;
{.$if true}
    FO: record
      case TSuperType of
        stBoolean: (c_boolean: boolean);
        stDouble: (c_double: double);
        stCurrency: (c_currency: Currency);
        stInt: (c_int: SuperInt);
        stObject: (c_object: TSuperTableString);
        stArray: (c_array: TSuperArray);
{$IFDEF SUPER_METHOD}
        stMethod: (c_method: TSuperMethod);
{$ENDIF}
      end;
{.$ifend}
    FOString: SOString;
    function GetDataType: TSuperType;
    function GetDataPtr: Pointer;
    procedure SetDataPtr(const Value: Pointer);
  protected
{$IFDEF FPC}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: tguid; out obj): longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
{$ENDIF}
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;

    function GetO(const path: SOString): ISuperObject;
    procedure PutO(const path: SOString; const Value: ISuperObject);
    function GetB(const path: SOString): Boolean;
    procedure PutB(const path: SOString; Value: Boolean);
    function GetI(const path: SOString): SuperInt;
    procedure PutI(const path: SOString; Value: SuperInt);
    function GetD(const path: SOString): Double;
    procedure PutD(const path: SOString; Value: Double);
    procedure PutC(const path: SOString; Value: Currency);
    function GetC(const path: SOString): Currency;
    function GetS(const path: SOString): SOString;
    procedure PutS(const path: SOString; const Value: SOString);
{$IFDEF SUPER_METHOD}
    function GetM(const path: SOString): TSuperMethod;
    procedure PutM(const path: SOString; Value: TSuperMethod);
{$ENDIF}
    function GetA(const path: SOString): TSuperArray;
    function Write(writer: TSuperWriter; indent: boolean; escape: boolean; level: integer): Integer; virtual;
  public
    function GetEnumerator: TSuperEnumerator;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;

    function GetProcessing: boolean;
    procedure SetProcessing(value: boolean);

    // Writers
    function SaveTo(stream: TStream; indent: boolean = false; escape: boolean = true): integer; overload;
    function SaveTo(const FileName: string; indent: boolean = false; escape: boolean = true): integer; overload;
    function SaveTo(socket: longint; indent: boolean = false; escape: boolean = true): integer; overload;
    function CalcSize(indent: boolean = false; escape: boolean = true): integer;
    function AsJSon(indent: boolean = false; escape: boolean = true): SOString;

    // parser  ... owned!
    class function ParseString(s: PSOChar; strict: Boolean; partial: boolean = true; const this: ISuperObject = nil; options: TSuperFindOptions = [];
       const put: ISuperObject = nil; dt: TSuperType = stNull): ISuperObject;
    class function ParseStream(stream: TStream; strict: Boolean; partial: boolean = true; const this: ISuperObject = nil; options: TSuperFindOptions = [];
       const put: ISuperObject = nil; dt: TSuperType = stNull): ISuperObject;
    class function ParseFile(const FileName: string; strict: Boolean; partial: boolean = true; const this: ISuperObject = nil; options: TSuperFindOptions = [];
       const put: ISuperObject = nil; dt: TSuperType = stNull): ISuperObject;
    class function ParseEx(tok: TSuperTokenizer; str: PSOChar; len: integer; strict: Boolean; const this: ISuperObject = nil;
      options: TSuperFindOptions = []; const put: ISuperObject = nil; dt: TSuperType = stNull): ISuperObject;

    // constructors / destructor
    constructor Create(jt: TSuperType = stObject); overload; virtual;
    constructor Create(b: boolean); overload; virtual;
    constructor Create(i: SuperInt); overload; virtual;
    constructor Create(d: double); overload; virtual;
    constructor CreateCurrency(c: Currency); overload; virtual;
    constructor Create(const s: SOString); overload; virtual;
{$IFDEF SUPER_METHOD}
    constructor Create(m: TSuperMethod); overload; virtual;
{$ENDIF}
    destructor Destroy; override;

    // convert
    function AsBoolean: Boolean; virtual;
    function AsInteger: SuperInt; virtual;
    function AsDouble: Double; virtual;
    function AsCurrency: Currency; virtual;
    function AsString: SOString; virtual;
    function AsArray: TSuperArray; virtual;
    function AsObject: TSuperTableString; virtual;
{$IFDEF SUPER_METHOD}
    function AsMethod: TSuperMethod; virtual;
{$ENDIF}
    procedure Clear(all: boolean = false); virtual;
    procedure Pack(all: boolean = false); virtual;
    function GetN(const path: SOString): ISuperObject;
    procedure PutN(const path: SOString; const Value: ISuperObject);
    function ForcePath(const path: SOString; dataType: TSuperType = stObject): ISuperObject;
    function Format(const str: SOString; BeginSep: SOChar = '%'; EndSep: SOChar = '%'): SOString;

    property N[const path: SOString]: ISuperObject read GetN write PutN;
    property O[const path: SOString]: ISuperObject read GetO write PutO; default;
    property B[const path: SOString]: boolean read GetB write PutB;
    property I[const path: SOString]: SuperInt read GetI write PutI;
    property D[const path: SOString]: Double read GetD write PutD;
    property C[const path: SOString]: Currency read GetC write PutC;
    property S[const path: SOString]: SOString read GetS write PutS;
{$IFDEF SUPER_METHOD}
    property M[const path: SOString]: TSuperMethod read GetM write PutM;
{$ENDIF}
    property A[const path: SOString]: TSuperArray read GetA;

    {$IFDEF SUPER_METHOD}
    function call(const path: SOString; const param: ISuperObject = nil): ISuperObject; overload; virtual;
    function call(const path, param: SOString): ISuperObject; overload; virtual;
{$ENDIF}
    // clone a node
    function Clone: ISuperObject; virtual;
    function Delete(const path: SOString): ISuperObject;
    // merges tow objects of same type, if reference is true then nodes are not cloned
    procedure Merge(const obj: ISuperObject; reference: boolean = false); overload;
    procedure Merge(const str: SOString); overload;

    // validate methods
    function Validate(const rules: SOString; const defs: SOString = ''; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean; overload;
    function Validate(const rules: ISuperObject; const defs: ISuperObject = nil; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean; overload;

    // compare
    function Compare(const obj: ISuperObject): TSuperCompareResult; overload;
    function Compare(const str: SOString): TSuperCompareResult; overload;

    // the data type
    function IsType(AType: TSuperType): boolean;
    property DataType: TSuperType read GetDataType;
    // a data pointer to link to something ele, a treeview for example
    property DataPtr: Pointer read GetDataPtr write SetDataPtr;
    property Processing: boolean read GetProcessing;
  end;

{$IFDEF HAVE_RTTI}
  TSuperRttiContext = class;

  TSerialFromJson = function(ctx: TSuperRttiContext; const obj: ISuperObject; var Value: TValue): Boolean;
  TSerialToJson = function(ctx: TSuperRttiContext; var value: TValue; const index: ISuperObject): ISuperObject;

  TSuperAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  SOName = class(TSuperAttribute);
  SODefault = class(TSuperAttribute);


  TSuperRttiContext = class
  private
    class function GetFieldName(r: TRttiField): string;
    class function GetFieldDefault(r: TRttiField; const obj: ISuperObject): ISuperObject;
  public
    Context: TRttiContext;
    SerialFromJson: TDictionary<PTypeInfo, TSerialFromJson>;
    SerialToJson: TDictionary<PTypeInfo, TSerialToJson>;
    constructor Create; virtual;
    destructor Destroy; override;
    function FromJson(TypeInfo: PTypeInfo; const obj: ISuperObject; var Value: TValue): Boolean; virtual;
    function ToJson(var value: TValue; const index: ISuperObject): ISuperObject; virtual;
    function AsType<T>(const obj: ISuperObject): T;
    function AsJson<T>(const obj: T; const index: ISuperObject = nil): ISuperObject;
  end;

  TSuperObjectHelper = class helper for TObject
  public
    function ToJson(ctx: TSuperRttiContext = nil): ISuperObject;
    constructor FromJson(const obj: ISuperObject; ctx: TSuperRttiContext = nil); overload;
    constructor FromJson(const str: string; ctx: TSuperRttiContext = nil); overload;
  end;
{$ENDIF}

  TSuperObjectIter = record
    key: SOString;
    val: ISuperObject;
    Ite: TSuperAvlIterator;
  end;

function ObjectIsError(obj: TSuperObject): boolean;
function ObjectIsType(const obj: ISuperObject; typ: TSuperType): boolean;
function ObjectGetType(const obj: ISuperObject): TSuperType;
function ObjectIsNull(const obj: ISuperObject): Boolean;

function ObjectFindFirst(const obj: ISuperObject; var F: TSuperObjectIter): boolean;
function ObjectFindNext(var F: TSuperObjectIter): boolean;
procedure ObjectFindClose(var F: TSuperObjectIter);

function SO(const s: SOString = '{}'): ISuperObject; overload;
function SO(const value: Variant): ISuperObject; overload;
function SO(const Args: array of const): ISuperObject; overload;

function SA(const Args: array of const): ISuperObject; overload;

function TryObjectToDate(const obj: ISuperObject; var dt: TDateTime): Boolean;
function UUIDToString(const g: TGUID): SOString;
function StringToUUID(const str: SOString; var g: TGUID): Boolean;

{$IFDEF HAVE_RTTI}

type
  TSuperInvokeResult = (
    irSuccess,
    irMethothodError,  // method don't exist
    irParamError,     // invalid parametters
    irError            // other error
  );

function TrySOInvoke(var ctx: TSuperRttiContext; const obj: TValue; const method: string; const params: ISuperObject; var Return: ISuperObject): TSuperInvokeResult; overload;
function SOInvoke(const obj: TValue; const method: string; const params: ISuperObject; ctx: TSuperRttiContext = nil): ISuperObject; overload;
function SOInvoke(const obj: TValue; const method: string; const params: string; ctx: TSuperRttiContext = nil): ISuperObject; overload;
{$ENDIF}

implementation
uses
  sysutils, Windows, superdate
{$IFDEF FPC}
  ,sockets
{$ELSE}
  ,WinSock
{$ENDIF}
  ;

{$IFDEF DEBUG}
var
  debugcount: integer = 0;
{$ENDIF}

const
  super_number_chars_set = ['0'..'9','.','+','-','e','E'];
  super_hex_chars: PSOChar = '0123456789abcdef';
  super_hex_chars_set = ['0'..'9','a'..'f','A'..'F'];

  ESC_BS: PSOChar = '\b';
  ESC_LF: PSOChar = '\n';
  ESC_CR: PSOChar = '\r';
  ESC_TAB: PSOChar = '\t';
  ESC_FF: PSOChar = '\f';
  ESC_QUOT: PSOChar = '\"';
  ESC_SL: PSOChar = '\\';
  ESC_SR: PSOChar = '\/';
  ESC_ZERO: PSOChar = '\u0000';

  TOK_CRLF: PSOChar = #13#10;
  TOK_SP: PSOChar = #32;
  TOK_BS: PSOChar = #8;
  TOK_TAB: PSOChar = #9;
  TOK_LF: PSOChar = #10;
  TOK_FF: PSOChar = #12;
  TOK_CR: PSOChar = #13;
//  TOK_SL: PSOChar = '\';
//  TOK_SR: PSOChar = '/';
  TOK_NULL: PSOChar = 'null';
  TOK_CBL: PSOChar = '{'; // curly bracket left
  TOK_CBR: PSOChar = '}'; // curly bracket right
  TOK_ARL: PSOChar = '[';
  TOK_ARR: PSOChar = ']';
  TOK_ARRAY: PSOChar = '[]';
  TOK_OBJ: PSOChar = '{}'; // empty object
  TOK_COM: PSOChar = ','; // Comma
  TOK_DQT: PSOChar = '"'; // Double Quote
  TOK_TRUE: PSOChar = 'true';
  TOK_FALSE: PSOChar = 'false';

{$if (sizeof(Char) = 1)}
function StrLComp(const Str1, Str2: PSOChar; MaxLen: Cardinal): Integer;
var
  P1, P2: PWideChar;
  I: Cardinal;
  C1, C2: WideChar;
begin
  P1 := Str1;
  P2 := Str2;
  I := 0;
  while I < MaxLen do
  begin
    C1 := P1^;
    C2 := P2^;

    if (C1 <> C2) or (C1 = #0) then
    begin
      Result := Ord(C1) - Ord(C2);
      Exit;
    end;

    Inc(P1);
    Inc(P2);
    Inc(I);
  end;
  Result := 0;
end;

function StrComp(const Str1, Str2: PSOChar): Integer;
var
  P1, P2: PWideChar;
  C1, C2: WideChar;
begin
  P1 := Str1;
  P2 := Str2;
  while True do
  begin
    C1 := P1^;
    C2 := P2^;

    if (C1 <> C2) or (C1 = #0) then
    begin
      Result := Ord(C1) - Ord(C2);
      Exit;
    end;

    Inc(P1);
    Inc(P2);
  end;
end;

function StrLen(const Str: PSOChar): Cardinal;
var
  p: PSOChar;
begin
  Result := 0;
  if Str <> nil then
  begin
    p := Str;
    while p^ <> #0 do inc(p);
    Result := (p - Str);
  end;
end;
{$ifend}

function FloatToJson(const value: Double): SOString;
var
  p: PSOChar;
begin
  Result := FloatToStr(value);
  if {$if defined(NEED_FORMATSETTINGS)}FormatSettings.{$ifend}DecimalSeparator <> '.' then
  begin
    p := PSOChar(Result);
    while p^ <> #0 do
      if p^ <> SOChar({$if defined(NEED_FORMATSETTINGS)}FormatSettings.{$ifend}DecimalSeparator) then
      inc(p) else
      begin
        p^ := '.';
        Exit;
      end;
  end;
end;

function CurrToJson(const value: Currency): SOString;
var
  p: PSOChar;
begin
  Result := CurrToStr(value);
  if {$if defined(NEED_FORMATSETTINGS)}FormatSettings.{$ifend}DecimalSeparator <> '.' then
  begin
    p := PSOChar(Result);
    while p^ <> #0 do
      if p^ <> SOChar({$if defined(NEED_FORMATSETTINGS)}FormatSettings.{$ifend}DecimalSeparator) then
      inc(p) else
      begin
        p^ := '.';
        Exit;
      end;
  end;
end;

function TryObjectToDate(const obj: ISuperObject; var dt: TDateTime): Boolean;
var
  i: Int64;
begin
  case ObjectGetType(obj) of
  stInt:
    begin
      dt := JavaToDelphiDateTime(obj.AsInteger);
      Result := True;
    end;
  stString:
    begin
      if ISO8601DateToJavaDateTime(obj.AsString, i) then
      begin
        dt := JavaToDelphiDateTime(i);
        Result := True;
      end else
        Result := TryStrToDateTime(obj.AsString, dt);
    end;
  else
    Result := False;
  end;
end;

function SO(const s: SOString): ISuperObject; overload;
begin
  Result := TSuperObject.ParseString(PSOChar(s), False);
end;

function SA(const Args: array of const): ISuperObject; overload;
type
  TByteArray = array[0..sizeof(integer) - 1] of byte;
  PByteArray = ^TByteArray;
var
  j: Integer;
  intf: IInterface;
begin
  Result := TSuperObject.Create(stArray);
  for j := 0 to length(Args) - 1 do
    with Result.AsArray do
    case TVarRec(Args[j]).VType of
      vtInteger : Add(TSuperObject.Create(TVarRec(Args[j]).VInteger));
      vtInt64   : Add(TSuperObject.Create(TVarRec(Args[j]).VInt64^));
      vtBoolean : Add(TSuperObject.Create(TVarRec(Args[j]).VBoolean));
      vtChar    : Add(TSuperObject.Create(SOString(TVarRec(Args[j]).VChar)));
      vtWideChar: Add(TSuperObject.Create(SOChar(TVarRec(Args[j]).VWideChar)));
      vtExtended: Add(TSuperObject.Create(TVarRec(Args[j]).VExtended^));
      vtCurrency: Add(TSuperObject.CreateCurrency(TVarRec(Args[j]).VCurrency^));
      vtString  : Add(TSuperObject.Create(SOString(TVarRec(Args[j]).VString^)));
      vtPChar   : Add(TSuperObject.Create(SOString(TVarRec(Args[j]).VPChar^)));
      vtAnsiString: Add(TSuperObject.Create(SOString(AnsiString(TVarRec(Args[j]).VAnsiString))));
      vtWideString: Add(TSuperObject.Create(SOString(PWideChar(TVarRec(Args[j]).VWideString))));
      vtInterface:
        if TVarRec(Args[j]).VInterface = nil then
          Add(nil) else
          if IInterface(TVarRec(Args[j]).VInterface).QueryInterface(ISuperObject, intf) = 0 then
            Add(ISuperObject(intf)) else
            Add(nil);
      vtPointer :
        if TVarRec(Args[j]).VPointer = nil then
          Add(nil) else
          Add(TSuperObject.Create(PtrInt(TVarRec(Args[j]).VPointer)));
      vtVariant:
        Add(SO(TVarRec(Args[j]).VVariant^));
      vtObject:
        if TVarRec(Args[j]).VPointer = nil then
          Add(nil) else
          Add(TSuperObject.Create(PtrInt(TVarRec(Args[j]).VPointer)));
      vtClass:
        if TVarRec(Args[j]).VPointer = nil then
          Add(nil) else
          Add(TSuperObject.Create(PtrInt(TVarRec(Args[j]).VPointer)));
{$if declared(vtUnicodeString)}
      vtUnicodeString:
          Add(TSuperObject.Create(SOString(string(TVarRec(Args[j]).VUnicodeString))));
{$ifend}
    else
      assert(false);
    end;
end;

function SO(const Args: array of const): ISuperObject; overload;
var
  j: Integer;
  arr: ISuperObject;
begin
  Result := TSuperObject.Create(stObject);
  arr := SA(Args);
  with arr.AsArray do
    for j := 0 to (Length div 2) - 1 do
      Result.AsObject.PutO(O[j*2].AsString, O[(j*2) + 1]);
end;

function SO(const value: Variant): ISuperObject; overload;
begin
  with TVarData(value) do
  case VType of
    varNull:     Result := nil;
    varEmpty:    Result := nil;
    varSmallInt: Result := TSuperObject.Create(VSmallInt);
    varInteger:  Result := TSuperObject.Create(VInteger);
    varSingle:   Result := TSuperObject.Create(VSingle);
    varDouble:   Result := TSuperObject.Create(VDouble);
    varCurrency: Result := TSuperObject.CreateCurrency(VCurrency);
    varDate:     Result := TSuperObject.Create(DelphiToJavaDateTime(vDate));
    varOleStr:   Result := TSuperObject.Create(SOString(VOleStr));
    varBoolean:  Result := TSuperObject.Create(VBoolean);
    varShortInt: Result := TSuperObject.Create(VShortInt);
    varByte:     Result := TSuperObject.Create(VByte);
    varWord:     Result := TSuperObject.Create(VWord);
    varLongWord: Result := TSuperObject.Create(VLongWord);
    varInt64:    Result := TSuperObject.Create(VInt64);
    varString:   Result := TSuperObject.Create(SOString(AnsiString(VString)));
{$if declared(varUString)}
  {$IFDEF FPC}
    varUString:  Result := TSuperObject.Create(SOString(UnicodeString(VString)));
  {$ELSE}
    varUString:  Result := TSuperObject.Create(SOString(string(VUString)));
  {$ENDIF}
{$ifend}
  else
    raise Exception.CreateFmt('Unsuported variant data type: %d', [VType]);
  end;
end;

function ObjectIsError(obj: TSuperObject): boolean;
begin
  Result := PtrUInt(obj) > PtrUInt(-4000);
end;

function ObjectIsType(const obj: ISuperObject; typ: TSuperType): boolean;
begin
  if obj <> nil then
    Result := typ = obj.DataType else
    Result := typ = stNull;
end;

function ObjectGetType(const obj: ISuperObject): TSuperType;
begin
  if obj <> nil then
    Result := obj.DataType else
    Result := stNull;
end;

function ObjectIsNull(const obj: ISuperObject): Boolean;
begin
  Result := ObjectIsType(obj, stNull);
end;

function ObjectFindFirst(const obj: ISuperObject; var F: TSuperObjectIter): boolean;
var
  i: TSuperAvlEntry;
begin
  if ObjectIsType(obj, stObject) then
  begin
    F.Ite := TSuperAvlIterator.Create(obj.AsObject);
    F.Ite.First;
    i := F.Ite.GetIter;
    if i <> nil then
    begin
      F.key := i.Name;
      F.val := i.Value;
      Result := True;
    end else
    begin
      FreeAndNil(F.Ite);
      Result := False;
    end;
  end else
    Result := False;
end;

function ObjectFindNext(var F: TSuperObjectIter): boolean;
var
  i: TSuperAvlEntry;
begin
  if Assigned(F.Ite) then
  begin
    F.Ite.Next;
    i := F.Ite.GetIter;
    if i <> nil then
    begin
      F.key := i.FName;
      F.val := i.Value;
      Result := True;
    end else
      Result := False;
  end
  else
    Result := False;
end;

procedure ObjectFindClose(var F: TSuperObjectIter);
begin
  if Assigned(F.Ite) then
    FreeAndNil(F.Ite);
  F.val := nil;
end;

function UuidFromString(p: PSOChar; Uuid: PGUID): Boolean;
const
  hex2bin: array[48..102] of Byte = (
     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0,
     0,10,11,12,13,14,15, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0,10,11,12,13,14,15);
type
  TState = (stEatSpaces, stStart, stHEX, stBracket, stEnd);
  TUUID = record
    case byte of
      0: (guid: TGUID);
      1: (bytes: array[0..15] of Byte);
      2: (words: array[0..7] of Word);
      3: (ints: array[0..3] of Cardinal);
      4: (i64s: array[0..1] of UInt64);
  end;

  function ishex(const c: SOChar): Boolean; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
  begin
    result := (c < #256) and (AnsiChar(c) in ['0'..'9', 'a'..'z', 'A'..'Z'])
  end;
var
  pos: Byte;
  state, saved: TState;
  bracket, separator: Boolean;
label
  redo;
begin
  FillChar(Uuid^, SizeOf(TGUID), 0);
  saved := stStart;
  state := stEatSpaces;
  bracket := false;
  separator := false;
  pos := 0;
  while true do
redo:
  case state of
    stEatSpaces:
      begin
        while true do
          case p^ of
            ' ', #13, #10, #9: inc(p);
          else
            state := saved;
            goto redo;
          end;
      end;
    stStart:
      case p^ of
        '{':
          begin
            bracket := true;
            inc(p);
            state := stEatSpaces;
            saved := stHEX;
            pos := 0;
          end;
      else
        state := stHEX;
      end;
    stHEX:
      case pos of
        0..7:
          if ishex(p^) then
          begin
            Uuid^.D1 := (Uuid^.D1 * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        8:
          if (p^ = '-') then
          begin
            separator := true;
            inc(p);
            inc(pos)
          end else
            inc(pos);
        13,18,23:
           if separator then
           begin
             if p^ <> '-' then
             begin
               Result := False;
               Exit;
             end;
             inc(p);
             inc(pos);
           end else
             inc(pos);
        9..12:
          if ishex(p^) then
          begin
            TUUID(Uuid^).words[2] := (TUUID(Uuid^).words[2] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        14..17:
          if ishex(p^) then
          begin
            TUUID(Uuid^).words[3] := (TUUID(Uuid^).words[3] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        19..20:
          if ishex(p^) then
          begin
            TUUID(Uuid^).bytes[8] := (TUUID(Uuid^).bytes[8] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        21..22:
          if ishex(p^) then
          begin
            TUUID(Uuid^).bytes[9] := (TUUID(Uuid^).bytes[9] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        24..25:
          if ishex(p^) then
          begin
            TUUID(Uuid^).bytes[10] := (TUUID(Uuid^).bytes[10] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        26..27:
          if ishex(p^) then
          begin
            TUUID(Uuid^).bytes[11] := (TUUID(Uuid^).bytes[11] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        28..29:
          if ishex(p^) then
          begin
            TUUID(Uuid^).bytes[12] := (TUUID(Uuid^).bytes[12] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        30..31:
          if ishex(p^) then
          begin
            TUUID(Uuid^).bytes[13] := (TUUID(Uuid^).bytes[13] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        32..33:
          if ishex(p^) then
          begin
            TUUID(Uuid^).bytes[14] := (TUUID(Uuid^).bytes[14] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        34..35:
          if ishex(p^) then
          begin
            TUUID(Uuid^).bytes[15] := (TUUID(Uuid^).bytes[15] * 16) + hex2bin[Ord(p^)];
            inc(p);
            inc(pos);
          end else
          begin
            Result := False;
            Exit;
          end;
        36: if bracket then
            begin
              state := stEatSpaces;
              saved := stBracket;
            end else
            begin
              state := stEatSpaces;
              saved := stEnd;
            end;
      end;
    stBracket:
      begin
        if p^ <> '}' then
        begin
          Result := False;
          Exit;
        end;
        inc(p);
        state := stEatSpaces;
        saved := stEnd;
      end;
    stEnd:
      begin
        if p^ <> #0 then
        begin
          Result := False;
          Exit;
        end;
        Break;
      end;
  end;
  Result := True;
end;

function UUIDToString(const g: TGUID): SOString;
begin
  Result := format('%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
    [g.D1, g.D2, g.D3,
     g.D4[0], g.D4[1], g.D4[2],
     g.D4[3], g.D4[4], g.D4[5],
     g.D4[6], g.D4[7]]);
end;

function StringToUUID(const str: SOString; var g: TGUID): Boolean;
begin
  Result := UuidFromString(PSOChar(str), @g);
end;

{$IFDEF HAVE_RTTI}

function serialtoboolean(ctx: TSuperRttiContext; var value: TValue; const index: ISuperObject): ISuperObject;
begin
  Result := TSuperObject.Create(TValueData(value).FAsSLong <> 0);
end;

function serialtodatetime(ctx: TSuperRttiContext; var value: TValue; const index: ISuperObject): ISuperObject;
begin
  Result := TSuperObject.Create(DelphiToJavaDateTime(TValueData(value).FAsDouble));
end;

function serialtoguid(ctx: TSuperRttiContext; var value: TValue; const index: ISuperObject): ISuperObject;
var
  g: TGUID;
begin
  value.ExtractRawData(@g);
  Result := TSuperObject.Create(
    format('%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',
              [g.D1, g.D2, g.D3,
               g.D4[0], g.D4[1], g.D4[2],
               g.D4[3], g.D4[4], g.D4[5],
               g.D4[6], g.D4[7]])
  );
end;

function serialfromboolean(ctx: TSuperRttiContext; const obj: ISuperObject; var Value: TValue): Boolean;
var
  o: ISuperObject;
begin
  case ObjectGetType(obj) of
  stBoolean:
    begin
      TValueData(Value).FAsSLong := obj.AsInteger;
      Result := True;
    end;
  stInt:
    begin
      TValueData(Value).FAsSLong := ord(obj.AsInteger <> 0);
      Result := True;
    end;
  stString:
    begin
      o := SO(obj.AsString);
      if not ObjectIsType(o, stString) then
        Result := serialfromboolean(ctx, SO(obj.AsString), Value) else
        Result := False;
    end;
  else
    Result := False;
  end;
end;

function serialfromdatetime(ctx: TSuperRttiContext; const obj: ISuperObject; var Value: TValue): Boolean;
var
  dt: TDateTime;
  i: Int64;
begin
  case ObjectGetType(obj) of
  stInt:
    begin
      TValueData(Value).FAsDouble := JavaToDelphiDateTime(obj.AsInteger);
      Result := True;
    end;
  stString:
    begin
      if ISO8601DateToJavaDateTime(obj.AsString, i) then
      begin
        TValueData(Value).FAsDouble := JavaToDelphiDateTime(i);
        Result := True;
      end else
      if TryStrToDateTime(obj.AsString, dt) then
      begin
        TValueData(Value).FAsDouble := dt;
        Result := True;
      end else
        Result := False;
    end;
  else
    Result := False;
  end;
end;

function serialfromguid(ctx: TSuperRttiContext; const obj: ISuperObject; var Value: TValue): Boolean;
begin
  case ObjectGetType(obj) of
    stNull:
      begin
        FillChar(Value.GetReferenceToRawData^, SizeOf(TGUID), 0);
        Result := True;
      end;
    stString: Result := UuidFromString(PSOChar(obj.AsString), Value.GetReferenceToRawData);
  else
    Result := False;
  end;
end;

function SOInvoke(const obj: TValue; const method: string; const params: ISuperObject; ctx: TSuperRttiContext): ISuperObject; overload;
var
  owned: Boolean;
begin
  if ctx = nil then
  begin
    ctx := TSuperRttiContext.Create;
    owned := True;
  end else
    owned := False;
  try
    if TrySOInvoke(ctx, obj, method, params, Result) <> irSuccess then
      raise Exception.Create('Invalid method call');
  finally
    if owned then
      ctx.Free;
  end;
end;

function SOInvoke(const obj: TValue; const method: string; const params: string; ctx: TSuperRttiContext): ISuperObject; overload;
begin
  Result := SOInvoke(obj, method, so(params), ctx)
end;

function TrySOInvoke(var ctx: TSuperRttiContext; const obj: TValue;
  const method: string; const params: ISuperObject;
  var Return: ISuperObject): TSuperInvokeResult;
var
  t: TRttiInstanceType;
  m: TRttiMethod;
  a: TArray<TValue>;
  ps: TArray<TRttiParameter>;
  v: TValue;
  index: ISuperObject;

  function GetParams: Boolean;
  var
    i: Integer;
  begin
    case ObjectGetType(params) of
      stArray:
        for i := 0 to Length(ps) - 1 do
          if (pfOut in ps[i].Flags) then
            TValue.Make(nil, ps[i].ParamType.Handle, a[i]) else
            if not ctx.FromJson(ps[i].ParamType.Handle, params.AsArray[i], a[i]) then
              Exit(False);
      stObject:
        for i := 0 to Length(ps) - 1 do
          if (pfOut in ps[i].Flags) then
            TValue.Make(nil, ps[i].ParamType.Handle, a[i]) else
            if not ctx.FromJson(ps[i].ParamType.Handle, params.AsObject[ps[i].Name], a[i]) then
              Exit(False);
      stNull: ;
    else
      Exit(False);
    end;
    Result := True;
  end;

  procedure SetParams;
  var
    i: Integer;
  begin
    case ObjectGetType(params) of
      stArray:
        for i := 0 to Length(ps) - 1 do
          if (ps[i].Flags * [pfVar, pfOut]) <> [] then
            params.AsArray[i] := ctx.ToJson(a[i], index);
      stObject:
        for i := 0 to Length(ps) - 1 do
          if (ps[i].Flags * [pfVar, pfOut]) <> [] then
            params.AsObject[ps[i].Name] := ctx.ToJson(a[i], index);
    end;
  end;

begin
  Result := irSuccess;
  index := SO;
  case obj.Kind of
    tkClass:
      begin
        t := TRttiInstanceType(ctx.Context.GetType(obj.AsObject.ClassType));
        m := t.GetMethod(method);
        if m = nil then Exit(irMethothodError);
        ps := m.GetParameters;
        SetLength(a, Length(ps));
        if not GetParams then Exit(irParamError);
        if m.IsClassMethod then
        begin
          v := m.Invoke(obj.AsObject.ClassType, a);
          Return := ctx.ToJson(v, index);
          SetParams;
        end else
        begin
          v := m.Invoke(obj, a);
          Return := ctx.ToJson(v, index);
          SetParams;
        end;
      end;
    tkClassRef:
      begin
        t := TRttiInstanceType(ctx.Context.GetType(obj.AsClass));
        m := t.GetMethod(method);
        if m = nil then Exit(irMethothodError);
        ps := m.GetParameters;
        SetLength(a, Length(ps));

        if not GetParams then Exit(irParamError);
        if m.IsClassMethod then
        begin
          v := m.Invoke(obj, a);
          Return := ctx.ToJson(v, index);
          SetParams;
        end else
          Exit(irError);
      end;
  else
    Exit(irError);
  end;
end;

{$ENDIF}

{ TSuperEnumerator }

constructor TSuperEnumerator.Create(const obj: ISuperObject);
begin
  FObj := obj;
  FCount := -1;
  if ObjectIsType(FObj, stObject) then
    FObjEnum := FObj.AsObject.GetEnumerator else
    FObjEnum := nil;
end;

destructor TSuperEnumerator.Destroy;
begin
  if FObjEnum <> nil then
    FObjEnum.Free;
end;

function TSuperEnumerator.MoveNext: Boolean;
begin
  case ObjectGetType(FObj) of
    stObject: Result := FObjEnum.MoveNext;
    stArray:
      begin
        inc(FCount);
        if FCount < FObj.AsArray.Length then
          Result := True else
          Result := False;
      end;
  else
    Result := false;
  end;
end;

function TSuperEnumerator.GetCurrent: ISuperObject;
begin
  case ObjectGetType(FObj) of
    stObject: Result := FObjEnum.Current.Value;
    stArray: Result := FObj.AsArray.GetO(FCount);
  else
    Result := FObj;
  end;
end;

{ TSuperObject }

constructor TSuperObject.Create(jt: TSuperType);
begin
  inherited Create;
{$IFDEF DEBUG}
  InterlockedIncrement(debugcount);
{$ENDIF}

  FProcessing := false;
  FDataPtr := nil;
  FDataType := jt;
  case FDataType of
    stObject: FO.c_object := TSuperTableString.Create;
    stArray: FO.c_array := TSuperArray.Create;
    stString: FOString := '';
  else
    FO.c_object := nil;
  end;
end;

constructor TSuperObject.Create(b: boolean);
begin
  Create(stBoolean);
  FO.c_boolean := b;
end;

constructor TSuperObject.Create(i: SuperInt);
begin
  Create(stInt);
  FO.c_int := i;
end;

constructor TSuperObject.Create(d: double);
begin
  Create(stDouble);
  FO.c_double := d;
end;

constructor TSuperObject.CreateCurrency(c: Currency);
begin
  Create(stCurrency);
  FO.c_currency := c;
end;

destructor TSuperObject.Destroy;
begin
{$IFDEF DEBUG}
  InterlockedDecrement(debugcount);
{$ENDIF}
  case FDataType of
    stObject: FO.c_object.Free;
    stArray: FO.c_array.Free;
  end;
  inherited;
end;

function TSuperObject.Write(writer: TSuperWriter; indent: boolean; escape: boolean; level: integer): Integer;
function DoEscape(str: PSOChar; len: Integer): Integer;
var
  pos, start_offset: Integer;
  c: SOChar;
  buf: array[0..5] of SOChar;
type
  TByteChar = record
  case integer of
    0: (a, b: Byte);
    1: (c: WideChar);
  end;
  begin
    if str = nil then
    begin
      Result := 0;
      exit;
    end;
    pos := 0; start_offset := 0;
    with writer do
    while pos < len do
    begin
      c := str[pos];
      case c of
        #8,#9,#10,#12,#13,'"','\','/':
          begin
            if(pos - start_offset > 0) then
              Append(str + start_offset, pos - start_offset);

            if(c = #8) then Append(ESC_BS, 2)
            else if (c = #9) then Append(ESC_TAB, 2)
            else if (c = #10) then Append(ESC_LF, 2)
            else if (c = #12) then Append(ESC_FF, 2)
            else if (c = #13) then Append(ESC_CR, 2)
            else if (c = '"') then Append(ESC_QUOT, 2)
            else if (c = '\') then Append(ESC_SL, 2)
            else if (c = '/') then Append(ESC_SR, 2);
            inc(pos);
            start_offset := pos;
          end;
      else
        if (SOIChar(c) > 255) then
        begin
          if(pos - start_offset > 0) then
            Append(str + start_offset, pos - start_offset);
          buf[0] := '\';
          buf[1] := 'u';
          buf[2] := super_hex_chars[TByteChar(c).b shr 4];
          buf[3] := super_hex_chars[TByteChar(c).b and $f];
          buf[4] := super_hex_chars[TByteChar(c).a shr 4];
          buf[5] := super_hex_chars[TByteChar(c).a and $f];
          Append(@buf, 6);
          inc(pos);
          start_offset := pos;
        end else
        if (c < #32) or (c > #127) then
        begin
          if(pos - start_offset > 0) then
            Append(str + start_offset, pos - start_offset);
          buf[0] := '\';
          buf[1] := 'u';
          buf[2] := '0';
          buf[3] := '0';
          buf[4] := super_hex_chars[ord(c) shr 4];
          buf[5] := super_hex_chars[ord(c) and $f];
          Append(buf, 6);
          inc(pos);
          start_offset := pos;
        end else
          inc(pos);
      end;
    end;
    if(pos - start_offset > 0) then
      writer.Append(str + start_offset, pos - start_offset);
    Result := 0;
  end;

function DoMinimalEscape(str: PSOChar; len: Integer): Integer;
var
  pos, start_offset: Integer;
  c: SOChar;
type
  TByteChar = record
  case integer of
    0: (a, b: Byte);
    1: (c: WideChar);
  end;
  begin
    if str = nil then
    begin
      Result := 0;
      exit;
    end;
    pos := 0; start_offset := 0;
    with writer do
    while pos < len do
    begin
      c := str[pos];
      case c of
        #0:
          begin
            if(pos - start_offset > 0) then
              Append(str + start_offset, pos - start_offset);
            Append(ESC_ZERO, 6);
            inc(pos);
            start_offset := pos;
          end;
        '"':
          begin
            if(pos - start_offset > 0) then
              Append(str + start_offset, pos - start_offset);
            Append(ESC_QUOT, 2);
            inc(pos);
            start_offset := pos;
          end;
        '\':
          begin
            if(pos - start_offset > 0) then
              Append(str + start_offset, pos - start_offset);
            Append(ESC_SL, 2);
            inc(pos);
            start_offset := pos;
          end;
      else
        inc(pos);
      end;
    end;
    if(pos - start_offset > 0) then
      writer.Append(str + start_offset, pos - start_offset);
    Result := 0;
  end;


  procedure _indent(i: shortint; r: boolean);
  begin
    inc(level, i);
    if r then
      with writer do
      begin
{$IFDEF MSWINDOWS}
        Append(TOK_CRLF, 2);
{$ELSE}
        Append(TOK_LF, 1);
{$ENDIF}
        for i := 0 to level - 1 do
          Append(TOK_SP, 1);
      end;
  end;
var
  k,j: Integer;
  iter: TSuperObjectIter;
  st: AnsiString;
  val: ISuperObject;
const
  ENDSTR_A: PSOChar = '": ';
  ENDSTR_B: PSOChar = '":';
begin

  if FProcessing then
  begin
    Result := writer.Append(TOK_NULL, 4);
    Exit;
  end;

  FProcessing := true;
  with writer do
  try
    case FDataType of
      stObject:
        if FO.c_object.FCount > 0 then
        begin
          k := 0;
          Append(TOK_CBL, 1);
          if indent then _indent(1, false);
          if ObjectFindFirst(Self, iter) then
          repeat
  {$IFDEF SUPER_METHOD}
            if (iter.val = nil) or not ObjectIsType(iter.val, stMethod) then
            begin
  {$ENDIF}
              if (iter.val = nil) or (not iter.val.Processing) then
              begin
                if(k <> 0) then
                  Append(TOK_COM, 1);
                if indent then _indent(0, true);
                Append(TOK_DQT, 1);
                if escape then
                  doEscape(PSOChar(iter.key), Length(iter.key)) else
                  DoMinimalEscape(PSOChar(iter.key), Length(iter.key));
                if indent then
                  Append(ENDSTR_A, 3) else
                  Append(ENDSTR_B, 2);
                if(iter.val = nil) then
                  Append(TOK_NULL, 4) else
                  iter.val.write(writer, indent, escape, level);
                inc(k);
              end;
  {$IFDEF SUPER_METHOD}
            end;
  {$ENDIF}
          until not ObjectFindNext(iter);
          ObjectFindClose(iter);
          if indent then _indent(-1, true);
          Result := Append(TOK_CBR, 1);
        end else
          Result := Append(TOK_OBJ, 2);
      stBoolean:
        begin
          if (FO.c_boolean) then
            Result := Append(TOK_TRUE, 4) else
            Result := Append(TOK_FALSE, 5);
        end;
      stInt:
        begin
          str(FO.c_int, st);
          Result := Append(PSOChar(SOString(st)));
        end;
      stDouble:
        Result := Append(PSOChar(FloatToJson(FO.c_double)));
      stCurrency:
        begin
          Result := Append(PSOChar(CurrToJson(FO.c_currency)));
        end;
      stString:
        begin
          Append(TOK_DQT, 1);
          if escape then
            doEscape(PSOChar(FOString), Length(FOString)) else
            DoMinimalEscape(PSOChar(FOString), Length(FOString));
          Append(TOK_DQT, 1);
          Result := 0;
        end;
      stArray:
        if FO.c_array.FLength > 0 then
        begin
          Append(TOK_ARL, 1);
          if indent then _indent(1, true);
          k := 0;
          j := 0;
          while k < FO.c_array.FLength do
          begin

            val :=  FO.c_array.GetO(k);
  {$IFDEF SUPER_METHOD}
            if not ObjectIsType(val, stMethod) then
            begin
  {$ENDIF}
              if (val = nil) or (not val.Processing) then
              begin
                if (j <> 0) then
                  Append(TOK_COM, 1);
                if(val = nil) then
                  Append(TOK_NULL, 4) else
                  val.write(writer, indent, escape, level);
                inc(j);
              end;
  {$IFDEF SUPER_METHOD}
            end;
  {$ENDIF}
            inc(k);
          end;
          if indent then _indent(-1, false);
          Result := Append(TOK_ARR, 1);
        end else
          Result := Append(TOK_ARRAY, 2);
      stNull:
          Result := Append(TOK_NULL, 4);
    else
      Result := 0;
    end;
  finally
    FProcessing := false;
  end;
end;

function TSuperObject.IsType(AType: TSuperType): boolean;
begin
  Result := AType = FDataType;
end;

function TSuperObject.AsBoolean: boolean;
begin
  case FDataType of
    stBoolean: Result := FO.c_boolean;
    stInt: Result := (FO.c_int <> 0);
    stDouble: Result := (FO.c_double <> 0);
    stCurrency: Result := (FO.c_currency <> 0);
    stString: Result := (Length(FOString) <> 0);
    stNull: Result := False;
  else
    Result := True;
  end;
end;

function TSuperObject.AsInteger: SuperInt;
var
  code: integer;
  cint: SuperInt;
begin
  case FDataType of
    stInt: Result := FO.c_int;
    stDouble: Result := round(FO.c_double);
    stCurrency: Result := round(FO.c_currency);
    stBoolean: Result := ord(FO.c_boolean);
    stString:
      begin
        Val(FOString, cint, code);
        if code = 0 then
          Result := cint else
          Result := 0;
      end;
  else
    Result := 0;
  end;
end;

function TSuperObject.AsDouble: Double;
var
  code: integer;
  cdouble: double;
begin
  case FDataType of
    stDouble: Result := FO.c_double;
    stCurrency: Result := FO.c_currency;
    stInt: Result := FO.c_int;
    stBoolean: Result := ord(FO.c_boolean);
    stString:
      begin
        Val(FOString, cdouble, code);
        if code = 0 then
          Result := cdouble else
          Result := 0.0;
      end;
  else
    Result := 0.0;
  end;
end;

function TSuperObject.AsCurrency: Currency;
var
  code: integer;
  cdouble: double;
begin
  case FDataType of
    stDouble: Result := FO.c_double;
    stCurrency: Result := FO.c_currency;
    stInt: Result := FO.c_int;
    stBoolean: Result := ord(FO.c_boolean);
    stString:
      begin
        Val(FOString, cdouble, code);
        if code = 0 then
          Result := cdouble else
          Result := 0.0;
      end;
  else
    Result := 0.0;
  end;
end;

function TSuperObject.AsString: SOString;
begin
  case FDataType of
    stString: Result := FOString;
    stNull: Result := '';
  else
    Result := AsJSon(false, false);
  end;
end;

function TSuperObject.GetEnumerator: TSuperEnumerator;
begin
  Result := TSuperEnumerator.Create(Self);
end;

procedure TSuperObject.AfterConstruction;
begin
  InterlockedDecrement(FRefCount);
end;

procedure TSuperObject.BeforeDestruction;
begin
  if RefCount <> 0 then
    raise Exception.Create('Invalid pointer');
end;

function TSuperObject.AsArray: TSuperArray;
begin
  if FDataType = stArray then
    Result := FO.c_array else
    Result := nil;
end;

function TSuperObject.AsObject: TSuperTableString;
begin
  if FDataType = stObject then
    Result := FO.c_object else
    Result := nil;
end;

function TSuperObject.AsJSon(indent, escape: boolean): SOString;
var
  pb: TSuperWriterString;
begin
  pb := TSuperWriterString.Create;
  try
    if(Write(pb, indent, escape, 0) < 0) then
    begin
      Result := '';
      Exit;
    end;
    if pb.FBPos > 0 then
      Result := pb.FBuf else
      Result := '';
  finally
    pb.Free;
  end;
end;

class function TSuperObject.ParseString(s: PSOChar; strict: Boolean; partial: boolean; const this: ISuperObject;
  options: TSuperFindOptions; const put: ISuperObject; dt: TSuperType): ISuperObject;
var
  tok: TSuperTokenizer;
  obj: ISuperObject;
begin
  tok := TSuperTokenizer.Create;
  obj := ParseEx(tok, s, -1, strict, this, options, put, dt);
  if(tok.err <> teSuccess) or (not partial and (s[tok.char_offset] <> #0)) then
    Result := nil else
    Result := obj;
  tok.Free;
end;

class function TSuperObject.ParseStream(stream: TStream; strict: Boolean;
  partial: boolean; const this: ISuperObject; options: TSuperFindOptions;
   const put: ISuperObject; dt: TSuperType): ISuperObject;
const
  BUFFER_SIZE = 1024;
var
  tok: TSuperTokenizer;
  buffera: array[0..BUFFER_SIZE-1] of AnsiChar;
  bufferw: array[0..BUFFER_SIZE-1] of SOChar;
  bom: array[0..1] of byte;
  unicode: boolean;
  j, size: Integer;
  st: string;
begin
  st := '';
  tok := TSuperTokenizer.Create;

  if (stream.Read(bom, sizeof(bom)) = 2) and (bom[0] = $FF) and (bom[1] = $FE) then
  begin
    unicode := true;
    size := stream.Read(bufferw, BUFFER_SIZE * SizeOf(SoChar)) div SizeOf(SoChar);
  end else
    begin
      unicode := false;
      stream.Seek(0, soFromBeginning);
      size := stream.Read(buffera, BUFFER_SIZE);
    end;

  while size > 0 do
  begin
    if not unicode then
      for j := 0 to size - 1 do
        bufferw[j] := SOChar(buffera[j]);
    ParseEx(tok, bufferw, size, strict, this, options, put, dt);

    if tok.err = teContinue then
      begin
        if not unicode then
          size := stream.Read(buffera, BUFFER_SIZE) else
          size := stream.Read(bufferw, BUFFER_SIZE * SizeOf(SoChar)) div SizeOf(SoChar);
      end else
      Break;
  end;
  if(tok.err <> teSuccess) or (not partial and (st[tok.char_offset] <> #0)) then
    Result := nil else
    Result := tok.stack[tok.depth].current;
  tok.Free;
end;

class function TSuperObject.ParseFile(const FileName: string; strict: Boolean;
  partial: boolean; const this: ISuperObject; options: TSuperFindOptions;
  const put: ISuperObject; dt: TSuperType): ISuperObject;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
  try
    Result := ParseStream(stream, strict, partial, this, options, put, dt);
  finally
    stream.Free;
  end;
end;

class function TSuperObject.ParseEx(tok: TSuperTokenizer; str: PSOChar; len: integer;
  strict: Boolean; const this: ISuperObject; options: TSuperFindOptions; const put: ISuperObject; dt: TSuperType): ISuperObject;

const
  spaces = [#32,#8,#9,#10,#12,#13];
  delimiters = ['"', '.', '[', ']', '{', '}', '(', ')', ',', ':', #0];
  reserved = delimiters + spaces;
  path = ['a'..'z', 'A'..'Z', '.', '_'];

  function hexdigit(x: SOChar): byte; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
  begin
    if x <= '9' then
      Result := byte(x) - byte('0') else
      Result := (byte(x) and 7) + 9;
  end;
  function min(v1, v2: integer): integer;{$IFDEF HAVE_INLINE} inline;{$ENDIF}
  begin if v1 < v2 then result := v1 else result := v2 end;

var
  obj: ISuperObject;
  v: SOChar;
{$IFDEF SUPER_METHOD}
  sm: TSuperMethod;
{$ENDIF}
  numi: SuperInt;
  numd: Double;
  code: integer;
  TokRec: PSuperTokenerSrec;
  evalstack: integer;
  p: PSOChar;

  function IsEndDelimiter(v: AnsiChar): Boolean;
  begin
    if tok.depth > 0 then
      case tok.stack[tok.depth - 1].state of
        tsArrayAdd: Result := v in [',', ']', #0];
        tsObjectValueAdd: Result := v in [',', '}', #0];
      else
        Result := v = #0;
      end else
        Result := v = #0;
  end;

label out, redo_char;
begin
  evalstack := 0;
  obj := nil;
  Result := nil;
  TokRec := @tok.stack[tok.depth];

  tok.char_offset := 0;
  tok.err := teSuccess;

  repeat
    if (tok.char_offset = len) then
    begin
      if (tok.depth = 0) and (TokRec^.state = tsEatws) and
         (TokRec^.saved_state = tsFinish) then
        tok.err := teSuccess else
        tok.err := teContinue;
      goto out;
    end;

    v := str^;

    case v of
    #10:
      begin
        inc(tok.line);
        tok.col := 0;
      end;
    #9: inc(tok.col, 4);
    else
      inc(tok.col);
    end;

redo_char:
    case TokRec^.state of
    tsEatws:
      begin
        if (SOIChar(v) < 256) and (AnsiChar(v) in spaces) then {nop} else
        if (v = '/') then
        begin
          tok.pb.Reset;
          tok.pb.Append(@v, 1);
          TokRec^.state := tsCommentStart;
        end else begin
          TokRec^.state := TokRec^.saved_state;
          goto redo_char;
        end
      end;

    tsStart:
      case v of
      '"',
      '''':
        begin
          TokRec^.state := tsString;
          tok.pb.Reset;
          tok.quote_char := v;
        end;
      '-':
        begin
          TokRec^.state := tsNumber;
          tok.pb.Reset;
          tok.is_double := 0;
          tok.floatcount := -1;
          goto redo_char;
        end;

      '0'..'9':
        begin
          if (tok.depth = 0) then
            case ObjectGetType(this) of
            stObject:
              begin
                TokRec^.state := tsIdentifier;
                TokRec^.current := this;
                goto redo_char;
              end;
          end;
          TokRec^.state := tsNumber;
          tok.pb.Reset;
          tok.is_double := 0;
          tok.floatcount := -1;
          goto redo_char;
        end;
      '{':
        begin
          TokRec^.state := tsEatws;
          TokRec^.saved_state := tsObjectFieldStart;
          TokRec^.current := TSuperObject.Create(stObject);
        end;
      '[':
        begin
          TokRec^.state := tsEatws;
          TokRec^.saved_state := tsArray;
          TokRec^.current := TSuperObject.Create(stArray);
        end;
{$IFDEF SUPER_METHOD}
      '(':
        begin
          if (tok.depth = 0) and ObjectIsType(this, stMethod) then
          begin
            TokRec^.current := this;
            TokRec^.state := tsParamValue;
          end;
        end;
{$ENDIF}
      'N',
      'n':
        begin
          TokRec^.state := tsNull;
          tok.pb.Reset;
          tok.st_pos := 0;
          goto redo_char;
        end;
      'T',
      't',
      'F',
      'f':
        begin
          TokRec^.state := tsBoolean;
          tok.pb.Reset;
          tok.st_pos := 0;
          goto redo_char;
        end;
      else
        TokRec^.state := tsIdentifier;
        tok.pb.Reset;
        goto redo_char;
      end;

    tsFinish:
      begin
        if(tok.depth = 0) then goto out;
        obj := TokRec^.current;
        tok.ResetLevel(tok.depth);
        dec(tok.depth);
        TokRec := @tok.stack[tok.depth];
        goto redo_char;
      end;

    tsNull:
      begin
        tok.pb.Append(@v, 1);
        if (StrLComp(TOK_NULL, PSOChar(tok.pb.FBuf), min(tok.st_pos + 1, 4)) = 0) then
        begin
          if (tok.st_pos = 4) then
          if (((SOIChar(v) < 256) and (AnsiChar(v) in path)) or (SOIChar(v) >= 256)) then
            TokRec^.state := tsIdentifier else
          begin
            TokRec^.current := TSuperObject.Create(stNull);
            TokRec^.saved_state := tsFinish;
            TokRec^.state := tsEatws;
            goto redo_char;
          end;
        end else
        begin
          TokRec^.state := tsIdentifier;
          tok.pb.FBuf[tok.st_pos] := #0;
          dec(tok.pb.FBPos);
          goto redo_char;
        end;
        inc(tok.st_pos);
      end;

    tsCommentStart:
      begin
        if(v = '*') then
        begin
          TokRec^.state := tsComment;
        end else
        if (v = '/') then
        begin
          TokRec^.state := tsCommentEol;
        end else
        begin
          tok.err := teParseComment;
          goto out;
        end;
        tok.pb.Append(@v, 1);
      end;

    tsComment:
      begin
        if(v = '*') then
          TokRec^.state := tsCommentEnd;
        tok.pb.Append(@v, 1);
      end;

    tsCommentEol:
      begin
        if (v = #10) then
          TokRec^.state := tsEatws else
          tok.pb.Append(@v, 1);
      end;

    tsCommentEnd:
      begin
        tok.pb.Append(@v, 1);
        if (v = '/') then
          TokRec^.state := tsEatws else
          TokRec^.state := tsComment;
      end;

    tsString:
      begin
        if (v = tok.quote_char) then
        begin
          TokRec^.current := TSuperObject.Create(SOString(tok.pb.GetString));
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        if (v = '\') then
        begin
          TokRec^.saved_state := tsString;
          TokRec^.state := tsStringEscape;
        end else
        begin
          tok.pb.Append(@v, 1);
        end
      end;

    tsEvalProperty:
      begin
        if (TokRec^.current = nil) and (foCreatePath in options) then
        begin
          TokRec^.current := TSuperObject.Create(stObject);
          TokRec^.parent.AsObject.PutO(tok.pb.Fbuf, TokRec^.current)
        end else
        if not ObjectIsType(TokRec^.current, stObject) then
        begin
          tok.err := teEvalObject;
          goto out;
        end;
        tok.pb.Reset;
        TokRec^.state := tsIdentifier;
        goto redo_char;
      end;

    tsEvalArray:
      begin
        if (TokRec^.current = nil) and (foCreatePath in options) then
        begin
          TokRec^.current := TSuperObject.Create(stArray);
          TokRec^.parent.AsObject.PutO(tok.pb.Fbuf, TokRec^.current)
        end else
        if not ObjectIsType(TokRec^.current, stArray) then
        begin
          tok.err := teEvalArray;
          goto out;
        end;
        tok.pb.Reset;
        TokRec^.state := tsParamValue;
        goto redo_char;
      end;
{$IFDEF SUPER_METHOD}
    tsEvalMethod:
      begin
        if ObjectIsType(TokRec^.current, stMethod) and assigned(TokRec^.current.AsMethod) then
        begin
          tok.pb.Reset;
          TokRec^.obj := TSuperObject.Create(stArray);
          TokRec^.state := tsMethodValue;
          goto redo_char;
        end else
        begin
          tok.err := teEvalMethod;
          goto out;
        end;
      end;

    tsMethodValue:
      begin
        case v of
        ')':
            TokRec^.state := tsIdentifier;
        else
          if (tok.depth >= SUPER_TOKENER_MAX_DEPTH-1) then
          begin
            tok.err := teDepth;
            goto out;
          end;
          inc(evalstack);
          TokRec^.state := tsMethodPut;
          inc(tok.depth);
          tok.ResetLevel(tok.depth);
          TokRec := @tok.stack[tok.depth];
          goto redo_char;
        end;
      end;

    tsMethodPut:
      begin
        TokRec^.obj.AsArray.Add(obj);
        case v of
          ',':
            begin
              tok.pb.Reset;
              TokRec^.saved_state := tsMethodValue;
              TokRec^.state := tsEatws;
            end;
          ')':
            begin
              if TokRec^.obj.AsArray.Length = 1 then
                TokRec^.obj := TokRec^.obj.AsArray.GetO(0);
              dec(evalstack);
              tok.pb.Reset;
              TokRec^.saved_state := tsIdentifier;
              TokRec^.state := tsEatws;
            end;
        else
          tok.err := teEvalMethod;
          goto out;
        end;
      end;
{$ENDIF}
    tsParamValue:
      begin
        case v of
        ']':
            TokRec^.state := tsIdentifier;
        else
          if (tok.depth >= SUPER_TOKENER_MAX_DEPTH-1) then
          begin
            tok.err := teDepth;
            goto out;
          end;
          inc(evalstack);
          TokRec^.state := tsParamPut;
          inc(tok.depth);
          tok.ResetLevel(tok.depth);
          TokRec := @tok.stack[tok.depth];
          goto redo_char;
        end;
      end;

    tsParamPut:
      begin
        dec(evalstack);
        TokRec^.obj := obj;
        tok.pb.Reset;
        TokRec^.saved_state := tsIdentifier;
        TokRec^.state := tsEatws;
        if v <> ']' then
        begin
          tok.err := teEvalArray;
          goto out;
        end;
      end;

    tsIdentifier:
      begin
        if (this = nil) then
        begin
          if (SOIChar(v) < 256) and IsEndDelimiter(AnsiChar(v)) then
          begin
            if not strict then
            begin
              tok.pb.TrimRight;
              TokRec^.current := TSuperObject.Create(tok.pb.Fbuf);
              TokRec^.saved_state := tsFinish;
              TokRec^.state := tsEatws;
              goto redo_char;
            end else
            begin
              tok.err := teParseString;
              goto out;
            end;
          end else
          if (v = '\') then
          begin
            TokRec^.saved_state := tsIdentifier;
            TokRec^.state := tsStringEscape;
          end else
            tok.pb.Append(@v, 1);
        end else
        begin
         if (SOIChar(v) < 256) and (AnsiChar(v) in reserved) then
         begin
           TokRec^.gparent := TokRec^.parent;
           if TokRec^.current = nil then
             TokRec^.parent := this else
             TokRec^.parent := TokRec^.current;

             case ObjectGetType(TokRec^.parent) of
               stObject:
                 case v of
                   '.':
                     begin
                       TokRec^.state := tsEvalProperty;
                       if tok.pb.FBPos > 0 then
                         TokRec^.current := TokRec^.parent.AsObject.GetO(tok.pb.Fbuf);
                     end;
                   '[':
                     begin
                       TokRec^.state := tsEvalArray;
                       if tok.pb.FBPos > 0 then
                         TokRec^.current := TokRec^.parent.AsObject.GetO(tok.pb.Fbuf);
                     end;
                   '(':
                     begin
                       TokRec^.state := tsEvalMethod;
                       if tok.pb.FBPos > 0 then
                         TokRec^.current := TokRec^.parent.AsObject.GetO(tok.pb.Fbuf);
                     end;
                 else
                   if tok.pb.FBPos > 0 then
                     TokRec^.current := TokRec^.parent.AsObject.GetO(tok.pb.Fbuf);
                   if (foPutValue in options) and (evalstack = 0) then
                   begin
                     TokRec^.parent.AsObject.PutO(tok.pb.Fbuf, put);
                     TokRec^.current := put
                   end else
                   if (foDelete in options) and (evalstack = 0) then
                   begin
                     TokRec^.current := TokRec^.parent.AsObject.Delete(tok.pb.Fbuf);
                   end else
                   if (TokRec^.current = nil) and (foCreatePath in options) then
                   begin
                     TokRec^.current := TSuperObject.Create(dt);
                     TokRec^.parent.AsObject.PutO(tok.pb.Fbuf, TokRec^.current);
                   end;
                   if not (foDelete in options) then
                     TokRec^.current := TokRec^.parent.AsObject.GetO(tok.pb.Fbuf);
                   TokRec^.state := tsFinish;
                   goto redo_char;
                 end;
               stArray:
                 begin
                   if TokRec^.obj <> nil then
                   begin
                     if not ObjectIsType(TokRec^.obj, stInt) or (TokRec^.obj.AsInteger < 0) then
                     begin
                       tok.err := teEvalInt;
                       TokRec^.obj := nil;
                       goto out;
                     end;
                     numi := TokRec^.obj.AsInteger;
                     TokRec^.obj := nil;

                     TokRec^.current := TokRec^.parent.AsArray.GetO(numi);
                     case v of
                       '.':
                         if (TokRec^.current = nil) and (foCreatePath in options) then
                         begin
                           TokRec^.current := TSuperObject.Create(stObject);
                           TokRec^.parent.AsArray.PutO(numi, TokRec^.current);
                         end else
                         if (TokRec^.current = nil) then
                         begin
                           tok.err := teEvalObject;
                           goto out;
                         end;
                       '[':
                         begin
                           if (TokRec^.current = nil) and (foCreatePath in options) then
                           begin
                             TokRec^.current := TSuperObject.Create(stArray);
                             TokRec^.parent.AsArray.Add(TokRec^.current);
                           end else
                           if (TokRec^.current = nil) then
                           begin
                             tok.err := teEvalArray;
                             goto out;
                           end;
                           TokRec^.state := tsEvalArray;
                         end;
                       '(': TokRec^.state := tsEvalMethod;
                     else
                       if (foPutValue in options) and (evalstack = 0) then
                       begin
                         TokRec^.parent.AsArray.PutO(numi, put);
                         TokRec^.current := put;
                       end else
                       if (foDelete in options) and (evalstack = 0) then
                       begin
                         TokRec^.current := TokRec^.parent.AsArray.Delete(numi);
                       end else
                         TokRec^.current := TokRec^.parent.AsArray.GetO(numi);
                       TokRec^.state := tsFinish;
                       goto redo_char
                     end;
                   end else
                   begin
                     case v of
                       '.':
                         begin
                           if (foPutValue in options) then
                           begin
                             TokRec^.current := TSuperObject.Create(stObject);
                             TokRec^.parent.AsArray.Add(TokRec^.current);
                           end else
                             TokRec^.current := TokRec^.parent.AsArray.GetO(TokRec^.parent.AsArray.FLength - 1);
                         end;
                       '[':
                         begin
                           if (foPutValue in options) then
                           begin
                             TokRec^.current := TSuperObject.Create(stArray);
                             TokRec^.parent.AsArray.Add(TokRec^.current);
                           end else
                             TokRec^.current := TokRec^.parent.AsArray.GetO(TokRec^.parent.AsArray.FLength - 1);
                           TokRec^.state := tsEvalArray;
                         end;
                       '(':
                         begin
                           if not (foPutValue in options) then
                             TokRec^.current := TokRec^.parent.AsArray.GetO(TokRec^.parent.AsArray.FLength - 1) else
                             TokRec^.current := nil;

                           TokRec^.state := tsEvalMethod;
                         end;
                     else
                       if (foPutValue in options) and (evalstack = 0) then
                       begin
                         TokRec^.parent.AsArray.Add(put);
                         TokRec^.current := put;
                       end else
                         if tok.pb.FBPos = 0 then
                           TokRec^.current := TokRec^.parent.AsArray.GetO(TokRec^.parent.AsArray.FLength - 1);
                       TokRec^.state := tsFinish;
                       goto redo_char
                     end;
                   end;
                 end;
{$IFDEF SUPER_METHOD}
               stMethod:
                 case v of
                   '.':
                     begin
                       TokRec^.current := nil;
                       sm := TokRec^.parent.AsMethod;
                       sm(TokRec^.gparent, TokRec^.obj, TokRec^.current);
                       TokRec^.obj := nil;
                     end;
                   '[':
                     begin
                       TokRec^.current := nil;
                       sm := TokRec^.parent.AsMethod;
                       sm(TokRec^.gparent, TokRec^.obj, TokRec^.current);
                       TokRec^.state := tsEvalArray;
                       TokRec^.obj := nil;
                     end;
                   '(':
                     begin
                       TokRec^.current := nil;
                       sm := TokRec^.parent.AsMethod;
                       sm(TokRec^.gparent, TokRec^.obj, TokRec^.current);
                       TokRec^.state := tsEvalMethod;
                       TokRec^.obj := nil;
                     end;
                 else
                   if not (foPutValue in options) or (evalstack > 0) then
                   begin
                     TokRec^.current := nil;
                     sm := TokRec^.parent.AsMethod;
                     sm(TokRec^.gparent, TokRec^.obj, TokRec^.current);
                     TokRec^.obj := nil;
                     TokRec^.state := tsFinish;
                     goto redo_char
                   end else
                   begin
                     tok.err := teEvalMethod;
                     TokRec^.obj := nil;
                     goto out;
                   end;
                 end;
{$ENDIF}
             end;
          end else
            tok.pb.Append(@v, 1);
        end;
      end;

    tsStringEscape:
      case v of
      'b',
      'n',
      'r',
      't',
      'f':
        begin
          if(v = 'b') then tok.pb.Append(TOK_BS, 1)
          else if(v = 'n') then tok.pb.Append(TOK_LF, 1)
          else if(v = 'r') then tok.pb.Append(TOK_CR, 1)
          else if(v = 't') then tok.pb.Append(TOK_TAB, 1)
          else if(v = 'f') then tok.pb.Append(TOK_FF, 1);
          TokRec^.state := TokRec^.saved_state;
        end;
      'u':
        begin
          tok.ucs_char := 0;
          tok.st_pos := 0;
          TokRec^.state := tsEscapeUnicode;
        end;
      'x':
        begin
          tok.ucs_char := 0;
          tok.st_pos := 0;
          TokRec^.state := tsEscapeHexadecimal;
        end
      else
        tok.pb.Append(@v, 1);
        TokRec^.state := TokRec^.saved_state;
      end;

    tsEscapeUnicode:
      begin
        if ((SOIChar(v) < 256) and (AnsiChar(v) in super_hex_chars_set)) then
        begin
          inc(tok.ucs_char, (Word(hexdigit(v)) shl ((3-tok.st_pos)*4)));
          inc(tok.st_pos);
          if (tok.st_pos = 4) then
          begin
            tok.pb.Append(@tok.ucs_char, 1);
            TokRec^.state := TokRec^.saved_state;
          end
        end else
        begin
          tok.err := teParseString;
          goto out;
        end
      end;
    tsEscapeHexadecimal:
      begin
        if ((SOIChar(v) < 256) and (AnsiChar(v) in super_hex_chars_set)) then
        begin
          inc(tok.ucs_char, (Word(hexdigit(v)) shl ((1-tok.st_pos)*4)));
          inc(tok.st_pos);
          if (tok.st_pos = 2) then
          begin
            tok.pb.Append(@tok.ucs_char, 1);
            TokRec^.state := TokRec^.saved_state;
          end
        end else
        begin
          tok.err := teParseString;
          goto out;
        end
      end;
    tsBoolean:
      begin
        tok.pb.Append(@v, 1);
        if (StrLComp('true', PSOChar(tok.pb.FBuf), min(tok.st_pos + 1, 4)) = 0) then
        begin
          if (tok.st_pos = 4) then
          if (((SOIChar(v) < 256) and (AnsiChar(v) in path)) or (SOIChar(v) >= 256)) then
            TokRec^.state := tsIdentifier else
          begin
            TokRec^.current := TSuperObject.Create(true);
            TokRec^.saved_state := tsFinish;
            TokRec^.state := tsEatws;
            goto redo_char;
          end
        end else
        if (StrLComp('false', PSOChar(tok.pb.FBuf), min(tok.st_pos + 1, 5)) = 0) then
        begin
          if (tok.st_pos = 5) then
          if (((SOIChar(v) < 256) and (AnsiChar(v) in path)) or (SOIChar(v) >= 256)) then
            TokRec^.state := tsIdentifier else
          begin
            TokRec^.current := TSuperObject.Create(false);
            TokRec^.saved_state := tsFinish;
            TokRec^.state := tsEatws;
            goto redo_char;
          end
        end else
        begin
          TokRec^.state := tsIdentifier;
          tok.pb.FBuf[tok.st_pos] := #0;
          dec(tok.pb.FBPos);
          goto redo_char;
        end;
        inc(tok.st_pos);
      end;

    tsNumber:
      begin
        if (SOIChar(v) < 256) and (AnsiChar(v) in super_number_chars_set) then
        begin
          tok.pb.Append(@v, 1);
          if (SOIChar(v) < 256) then
          case v of
          '.': begin
                 tok.is_double := 1;
                 tok.floatcount := 0;
               end;
          'e','E':
            begin
              tok.is_double := 1;
              tok.floatcount := -1;
            end;
          '0'..'9':
            begin

              if (tok.is_double = 1) and (tok.floatcount >= 0) then
              begin
                inc(tok.floatcount);
                if tok.floatcount > 4 then
                  tok.floatcount := -1;
              end;
            end;
          end;
        end else
        begin
          if (tok.is_double = 0) then
          begin
            val(tok.pb.FBuf, numi, code);
            if ObjectIsType(this, stArray) then
            begin
              if (foPutValue in options) and (evalstack = 0) then
              begin
                this.AsArray.PutO(numi, put);
                TokRec^.current := put;
              end else
              if (foDelete in options) and (evalstack = 0) then
                TokRec^.current := this.AsArray.Delete(numi) else
                TokRec^.current := this.AsArray.GetO(numi);
            end else
              TokRec^.current := TSuperObject.Create(numi);

          end else
          if (tok.is_double <> 0) then
          begin
            if tok.floatcount >= 0 then
            begin
              p := tok.pb.FBuf;
              while p^ <> '.' do inc(p);
              for code := 0 to tok.floatcount - 1 do
              begin
                p^ := p[1];
                inc(p);
              end;
              p^ := #0;
              val(tok.pb.FBuf, numi, code);
              case tok.floatcount of
                0: numi := numi * 10000;
                1: numi := numi * 1000;
                2: numi := numi * 100;
                3: numi := numi * 10;
              end;
              TokRec^.current := TSuperObject.CreateCurrency(PCurrency(@numi)^);
            end else
            begin
              val(tok.pb.FBuf, numd, code);
              TokRec^.current := TSuperObject.Create(numd);
            end;
          end else
          begin
            tok.err := teParseNumber;
            goto out;
          end;
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
          goto redo_char;
        end
      end;

    tsArray:
      begin
        if (v = ']') then
        begin
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        begin
          if(tok.depth >= SUPER_TOKENER_MAX_DEPTH-1) then
          begin
            tok.err := teDepth;
            goto out;
          end;
          TokRec^.state := tsArrayAdd;
          inc(tok.depth);
          tok.ResetLevel(tok.depth);
          TokRec := @tok.stack[tok.depth];
          goto redo_char;
        end
      end;

    tsArrayAdd:
      begin
        TokRec^.current.AsArray.Add(obj);
        TokRec^.saved_state := tsArraySep;
        TokRec^.state := tsEatws;
        goto redo_char;
      end;

    tsArraySep:
      begin
        if (v = ']') then
        begin
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        if (v = ',') then
        begin
          TokRec^.saved_state := tsArray;
          TokRec^.state := tsEatws;
        end else
        begin
          tok.err := teParseArray;
          goto out;
        end
      end;

    tsObjectFieldStart:
      begin
        if (v = '}') then
        begin
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        if (SOIChar(v) < 256) and (AnsiChar(v) in ['"', '''']) then
        begin
          tok.quote_char := v;
          tok.pb.Reset;
          TokRec^.state := tsObjectField;
        end else
        if not((SOIChar(v) < 256) and ((AnsiChar(v) in reserved) or strict)) then
        begin
          TokRec^.state := tsObjectUnquotedField;
          tok.pb.Reset;
          goto redo_char;
        end else
        begin
          tok.err := teParseObjectKeyName;
          goto out;
        end
      end;

    tsObjectField:
      begin
        if (v = tok.quote_char) then
        begin
          TokRec^.field_name := tok.pb.FBuf;
          TokRec^.saved_state := tsObjectFieldEnd;
          TokRec^.state := tsEatws;
        end else
        if (v = '\') then
        begin
          TokRec^.saved_state := tsObjectField;
          TokRec^.state := tsStringEscape;
        end else
        begin
          tok.pb.Append(@v, 1);
        end
      end;

    tsObjectUnquotedField:
      begin
        if (SOIChar(v) < 256) and (AnsiChar(v) in [':', #0]) then
        begin
          TokRec^.field_name := tok.pb.FBuf;
          TokRec^.saved_state := tsObjectFieldEnd;
          TokRec^.state := tsEatws;
          goto redo_char;
        end else
        if (v = '\') then
        begin
          TokRec^.saved_state := tsObjectUnquotedField;
          TokRec^.state := tsStringEscape;
        end else
          tok.pb.Append(@v, 1);
      end;

    tsObjectFieldEnd:
      begin
        if (v = ':') then
        begin
          TokRec^.saved_state := tsObjectValue;
          TokRec^.state := tsEatws;
        end else
        begin
          tok.err := teParseObjectKeySep;
          goto out;
        end
      end;

    tsObjectValue:
      begin
        if (tok.depth >= SUPER_TOKENER_MAX_DEPTH-1) then
        begin
          tok.err := teDepth;
          goto out;
        end;
        TokRec^.state := tsObjectValueAdd;
        inc(tok.depth);
        tok.ResetLevel(tok.depth);
        TokRec := @tok.stack[tok.depth];
        goto redo_char;
      end;

    tsObjectValueAdd:
      begin
        TokRec^.current.AsObject.PutO(TokRec^.field_name, obj);
        TokRec^.field_name := '';
        TokRec^.saved_state := tsObjectSep;
        TokRec^.state := tsEatws;
        goto redo_char;
      end;

    tsObjectSep:
      begin
        if (v = '}') then
        begin
          TokRec^.saved_state := tsFinish;
          TokRec^.state := tsEatws;
        end else
        if (v = ',') then
        begin
          TokRec^.saved_state := tsObjectFieldStart;
          TokRec^.state := tsEatws;
        end else
        begin
          tok.err := teParseObjectValueSep;
          goto out;
        end
      end;
    end;
    inc(str);
    inc(tok.char_offset);
  until v = #0;

  if(TokRec^.state <> tsFinish) and
     (TokRec^.saved_state <> tsFinish) then
    tok.err := teParseEof;

 out:
  if(tok.err in [teSuccess]) then
  begin
{$IFDEF SUPER_METHOD}
    if (foCallMethod in options) and ObjectIsType(TokRec^.current, stMethod) and assigned(TokRec^.current.AsMethod) then
    begin
      sm := TokRec^.current.AsMethod;
      sm(TokRec^.parent, put, Result);
    end else
{$ENDIF}
    Result := TokRec^.current;
  end else
    Result := nil;
end;

procedure TSuperObject.PutO(const path: SOString; const Value: ISuperObject);
begin
  ParseString(PSOChar(path), true, False, self, [foCreatePath, foPutValue], Value);
end;

procedure TSuperObject.PutB(const path: SOString; Value: Boolean);
begin
  ParseString(PSOChar(path), true, False, self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;

procedure TSuperObject.PutD(const path: SOString; Value: Double);
begin
  ParseString(PSOChar(path), true, False, self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;

procedure TSuperObject.PutC(const path: SOString; Value: Currency);
begin
  ParseString(PSOChar(path), true, False, self, [foCreatePath, foPutValue], TSuperObject.CreateCurrency(Value));
end;

procedure TSuperObject.PutI(const path: SOString; Value: SuperInt);
begin
  ParseString(PSOChar(path), true, False, self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;

procedure TSuperObject.PutS(const path: SOString; const Value: SOString);
begin
  ParseString(PSOChar(path), true, False, self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;


{$IFDEF FPC}
function TSuperObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: tguid; out obj): longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TSuperObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
{$ENDIF}
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TSuperObject.SaveTo(stream: TStream; indent, escape: boolean): integer;
var
  pb: TSuperWriterStream;
begin
  if escape then
    pb := TSuperAnsiWriterStream.Create(stream) else
    pb := TSuperUnicodeWriterStream.Create(stream);

  if(Write(pb, indent, escape, 0) < 0) then
  begin
    pb.Reset;
    pb.Free;
    Result := 0;
    Exit;
  end;
  Result := stream.Size;
  pb.Free;
end;

function TSuperObject.CalcSize(indent, escape: boolean): integer;
var
  pb: TSuperWriterFake;
begin
  pb := TSuperWriterFake.Create;
  if(Write(pb, indent, escape, 0) < 0) then
  begin
    pb.Free;
    Result := 0;
    Exit;
  end;
  Result := pb.FSize;
  pb.Free;
end;

function TSuperObject.SaveTo(socket: Integer; indent, escape: boolean): integer;
var
  pb: TSuperWriterSock;
begin
  pb := TSuperWriterSock.Create(socket);
  if(Write(pb, indent, escape, 0) < 0) then
  begin
    pb.Free;
    Result := 0;
    Exit;
  end;
  Result := pb.FSize;
  pb.Free;
end;

constructor TSuperObject.Create(const s: SOString);
begin
  Create(stString);
  FOString := s;
end;

procedure TSuperObject.Clear(all: boolean);
begin
  if FProcessing then exit;
  FProcessing := true;
  try
    case FDataType of
      stBoolean: FO.c_boolean := false;
      stDouble: FO.c_double := 0.0;
      stCurrency: FO.c_currency := 0.0;
      stInt: FO.c_int := 0;
      stObject: FO.c_object.Clear(all);
      stArray: FO.c_array.Clear(all);
      stString: FOString := '';
{$IFDEF SUPER_METHOD}
      stMethod: FO.c_method := nil;
{$ENDIF}
    end;
  finally
    FProcessing := false;
  end;
end;

procedure TSuperObject.Pack(all: boolean = false);
begin
  if FProcessing then exit;
  FProcessing := true;
  try
    case FDataType of
      stObject: FO.c_object.Pack(all);
      stArray: FO.c_array.Pack(all);
    end;
  finally
    FProcessing := false;
  end;
end;

function TSuperObject.GetN(const path: SOString): ISuperObject;
begin
  Result := ParseString(PSOChar(path), False, true, self);
  if Result = nil then
    Result := TSuperObject.Create(stNull);
end;

procedure TSuperObject.PutN(const path: SOString; const Value: ISuperObject);
begin
  if Value = nil then
    ParseString(PSOChar(path), False, True, self, [foCreatePath, foPutValue], TSuperObject.Create(stNull)) else
    ParseString(PSOChar(path), False, True, self, [foCreatePath, foPutValue], Value);
end;

function TSuperObject.Delete(const path: SOString): ISuperObject;
begin
  Result := ParseString(PSOChar(path), False, true, self, [foDelete]);
end;

function TSuperObject.Clone: ISuperObject;
var
  ite: TSuperObjectIter;
  arr: TSuperArray;
  j: integer;
begin
  case FDataType of
    stBoolean: Result := TSuperObject.Create(FO.c_boolean);
    stDouble: Result := TSuperObject.Create(FO.c_double);
    stCurrency: Result := TSuperObject.CreateCurrency(FO.c_currency);
    stInt: Result := TSuperObject.Create(FO.c_int);
    stString: Result := TSuperObject.Create(FOString);
{$IFDEF SUPER_METHOD}
    stMethod: Result := TSuperObject.Create(FO.c_method);
{$ENDIF}
    stObject:
      begin
        Result := TSuperObject.Create(stObject);
        if ObjectFindFirst(self, ite) then
        with Result.AsObject do
        repeat
          PutO(ite.key, ite.val.Clone);
        until not ObjectFindNext(ite);
        ObjectFindClose(ite);
      end;
    stArray:
      begin
        Result := TSuperObject.Create(stArray);
        arr := AsArray;
        with Result.AsArray do
        for j := 0 to arr.Length - 1 do
          Add(arr.GetO(j).Clone);
      end;
    stNull:
      Result := TSuperObject.Create(stNull);
  else
    Result := nil;
  end;
end;

procedure TSuperObject.Merge(const obj: ISuperObject; reference: boolean);
var
  prop1, prop2: ISuperObject;
  ite: TSuperObjectIter;
  arr: TSuperArray;
  j: integer;
begin
  if ObjectIsType(obj, FDataType) then
  case FDataType of
    stBoolean: FO.c_boolean := obj.AsBoolean;
    stDouble: FO.c_double := obj.AsDouble;
    stCurrency: FO.c_currency := obj.AsCurrency;
    stInt: FO.c_int := obj.AsInteger;
    stString: FOString := obj.AsString;
{$IFDEF SUPER_METHOD}
    stMethod: FO.c_method := obj.AsMethod;
{$ENDIF}
    stObject:
      begin
        if ObjectFindFirst(obj, ite) then
        with FO.c_object do
        repeat
          prop1 := FO.c_object.GetO(ite.key);
          if (prop1 <> nil) and (ite.val <> nil) and (prop1.DataType = ite.val.DataType) then
            prop1.Merge(ite.val) else
            if reference then
              PutO(ite.key, ite.val) else
              if ite.val <> nil then
                PutO(ite.key, ite.val.Clone) else
                PutO(ite.key, nil)

        until not ObjectFindNext(ite);
        ObjectFindClose(ite);
      end;
    stArray:
      begin
        arr := obj.AsArray;
        with FO.c_array do
        for j := 0 to arr.Length - 1 do
        begin
          prop1 := GetO(j);
          prop2 := arr.GetO(j);
          if (prop1 <> nil) and (prop2 <> nil) and (prop1.DataType = prop2.DataType) then
            prop1.Merge(prop2) else
            if reference then
              PutO(j, prop2) else
              if prop2 <> nil then
                PutO(j, prop2.Clone) else
                PutO(j, nil);
        end;
      end;
  end;
end;

procedure TSuperObject.Merge(const str: SOString);
begin
  Merge(TSuperObject.ParseString(PSOChar(str), False), true);
end;

class function TSuperObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSuperObject(Result).FRefCount := 1;
end;

function TSuperObject.ForcePath(const path: SOString; dataType: TSuperType = stObject): ISuperObject;
begin
  Result := ParseString(PSOChar(path), False, True, Self, [foCreatePath], nil, dataType);
end;

function TSuperObject.Format(const str: SOString; BeginSep: SOChar; EndSep: SOChar): SOString;
var
  p1, p2: PSOChar;
begin
  Result := '';
  p2 := PSOChar(str);
  p1 := p2;
  while true do
    if p2^ = BeginSep then
      begin
        if p2 > p1 then
          Result := Result + Copy(p1, 0, p2-p1);
        inc(p2);
        p1 := p2;
        while true do
          if p2^ = EndSep then Break else
          if p2^ = #0     then Exit else
            inc(p2);
        Result := Result + GetS(copy(p1, 0, p2-p1));
        inc(p2);
        p1 := p2;
      end
    else if p2^ = #0 then
      begin
        if p2 > p1 then
          Result := Result + Copy(p1, 0, p2-p1);
        Break;
      end else
        inc(p2);
end;

function TSuperObject.GetO(const path: SOString): ISuperObject;
begin
  Result := ParseString(PSOChar(path), False, True, Self);
end;

function TSuperObject.GetA(const path: SOString): TSuperArray;
var
  obj: ISuperObject;
begin
  obj := ParseString(PSOChar(path), False, True, Self);
  if obj <> nil then
    Result := obj.AsArray else
    Result := nil;
end;

function TSuperObject.GetB(const path: SOString): Boolean;
var
  obj: ISuperObject;
begin
  obj := GetO(path);
  if obj <> nil then
    Result := obj.AsBoolean else
    Result := false;
end;

function TSuperObject.GetD(const path: SOString): Double;
var
  obj: ISuperObject;
begin
  obj := GetO(path);
  if obj <> nil then
    Result := obj.AsDouble else
    Result := 0.0;
end;

function TSuperObject.GetC(const path: SOString): Currency;
var
  obj: ISuperObject;
begin
  obj := GetO(path);
  if obj <> nil then
    Result := obj.AsCurrency else
    Result := 0.0;
end;

function TSuperObject.GetI(const path: SOString): SuperInt;
var
  obj: ISuperObject;
begin
  obj := GetO(path);
  if obj <> nil then
    Result := obj.AsInteger else
    Result := 0;
end;

function TSuperObject.GetDataPtr: Pointer;
begin
  Result := FDataPtr;
end;

function TSuperObject.GetDataType: TSuperType;
begin
  Result := FDataType
end;

function TSuperObject.GetS(const path: SOString): SOString;
var
  obj: ISuperObject;
begin
  obj := GetO(path);
  if obj <> nil then
    Result := obj.AsString else
    Result := '';
end;

function TSuperObject.SaveTo(const FileName: string; indent, escape: boolean): integer;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmCreate);
  try
    Result := SaveTo(stream, indent, escape);
  finally
    stream.Free;
  end;
end;

function TSuperObject.Validate(const rules: SOString; const defs: SOString = ''; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean;
begin
  Result := Validate(TSuperObject.ParseString(PSOChar(rules), False), TSuperObject.ParseString(PSOChar(defs), False), callback, sender);
end;

function TSuperObject.Validate(const rules: ISuperObject; const defs: ISuperObject = nil; callback: TSuperOnValidateError = nil; sender: Pointer = nil): boolean;
type
  TDataType = (dtUnknown, dtStr, dtInt, dtFloat, dtNumber, dtText, dtBool,
               dtMap, dtSeq, dtScalar, dtAny);
var
  datatypes: ISuperObject;
  names: ISuperObject;

  function FindInheritedProperty(const prop: PSOChar; p: ISuperObject): ISuperObject;
  var
    o: ISuperObject;
    e: TSuperAvlEntry;
  begin
    o := p[prop];
    if o <> nil then
      result := o else
      begin
        o := p['inherit'];
        if (o <> nil) and ObjectIsType(o, stString) then
          begin
            e := names.AsObject.Search(o.AsString);
            if (e <> nil) then
              Result := FindInheritedProperty(prop, e.Value) else
              Result := nil;
          end else
            Result := nil;
      end;
  end;

  function FindDataType(o: ISuperObject): TDataType;
  var
    e: TSuperAvlEntry;
    obj: ISuperObject;
  begin
    obj := FindInheritedProperty('type', o);
    if obj <> nil then
    begin
      e := datatypes.AsObject.Search(obj.AsString);
      if  e <> nil then
        Result := TDataType(e.Value.AsInteger) else
        Result := dtUnknown;
    end else
      Result := dtUnknown;
  end;

  procedure GetNames(o: ISuperObject);
  var
    obj: ISuperObject;
    f: TSuperObjectIter;
  begin
    obj := o['name'];
    if ObjectIsType(obj, stString) then
      names[obj.AsString] := o;

    case FindDataType(o) of
      dtMap:
        begin
          obj := o['mapping'];
          if ObjectIsType(obj, stObject) then
          begin
            if ObjectFindFirst(obj, f) then
            repeat
              if ObjectIsType(f.val, stObject) then
                GetNames(f.val);
            until not ObjectFindNext(f);
            ObjectFindClose(f);
          end;
        end;
      dtSeq:
        begin
          obj := o['sequence'];
          if ObjectIsType(obj, stObject) then
            GetNames(obj);
        end;
    end;
  end;

  function FindInheritedField(const prop: SOString; p: ISuperObject): ISuperObject;
  var
    o: ISuperObject;
    e: TSuperAvlEntry;
  begin
    o := p['mapping'];
    if ObjectIsType(o, stObject) then
    begin
      o := o.AsObject.GetO(prop);
      if o <> nil then
      begin
        Result := o;
        Exit;
      end;
    end;

    o := p['inherit'];
    if ObjectIsType(o, stString) then
    begin
      e := names.AsObject.Search(o.AsString);
      if (e <> nil) then
        Result := FindInheritedField(prop, e.Value) else
        Result := nil;
    end else
      Result := nil;
  end;

  function InheritedFieldExist(const obj: ISuperObject; p: ISuperObject; const name: SOString = ''): boolean;
  var
   o: ISuperObject;
   e: TSuperAvlEntry;
   j: TSuperAvlIterator;
  begin
    Result := true;
    o := p['mapping'];
    if ObjectIsType(o, stObject) then
    begin
      j := TSuperAvlIterator.Create(o.AsObject);
      try
        j.First;
        e := j.GetIter;
        while e <> nil do
        begin
          if obj.AsObject.Search(e.Name) = nil then
          begin
            Result := False;
            if assigned(callback) then
              callback(sender, veFieldNotFound, name + '.' + e.Name);
          end;
          j.Next;
          e := j.GetIter;
        end;

      finally
        j.Free;
      end;
    end;

    o := p['inherit'];
    if ObjectIsType(o, stString) then
    begin
      e := names.AsObject.Search(o.AsString);
      if (e <> nil) then
        Result := InheritedFieldExist(obj, e.Value, name) and Result;
    end;
  end;

  function getInheritedBool(f: PSOChar; p: ISuperObject; default: boolean = false): boolean;
  var
    o: ISuperObject;
  begin
    o := FindInheritedProperty(f, p);
    case ObjectGetType(o) of
      stBoolean: Result := o.AsBoolean;
      stNull: Result := Default;
    else
      Result := default;
      if assigned(callback) then
        callback(sender, veRuleMalformated, f);
    end;
  end;

  procedure GetInheritedFieldList(list: ISuperObject; p: ISuperObject);
  var
   o: ISuperObject;
   e: TSuperAvlEntry;
   i: TSuperAvlIterator;
  begin
    Result := true;
    o := p['mapping'];
    if ObjectIsType(o, stObject) then
    begin
      i := TSuperAvlIterator.Create(o.AsObject);
      try
        i.First;
        e := i.GetIter;
        while e <> nil do
        begin
          if list.AsObject.Search(e.Name) = nil then
            list[e.Name] := e.Value;
          i.Next;
          e := i.GetIter;
        end;

      finally
        i.Free;
      end;
    end;

    o := p['inherit'];
    if ObjectIsType(o, stString) then
    begin
      e := names.AsObject.Search(o.AsString);
      if (e <> nil) then
        GetInheritedFieldList(list, e.Value);
    end;
  end;

  function CheckEnum(o: ISuperObject; p: ISuperObject; name: SOString = ''): boolean;
  var
    enum: ISuperObject;
    i: integer;
  begin
    Result := false;
    enum := FindInheritedProperty('enum', p);
    case ObjectGetType(enum) of
      stArray:
        for i := 0 to enum.AsArray.Length - 1 do
          if (o.AsString = enum.AsArray[i].AsString) then
          begin
            Result := true;
            exit;
          end;
      stNull: Result := true;
    else
      Result := false;
      if assigned(callback) then
        callback(sender, veRuleMalformated, '');
      Exit;
    end;

    if (not Result) and assigned(callback) then
      callback(sender, veValueNotInEnum, name);
  end;

  function CheckLength(len: integer; p: ISuperObject; const objpath: SOString): boolean;
  var
    length, o: ISuperObject;
  begin
    result := true;
    length := FindInheritedProperty('length', p);
    case ObjectGetType(length) of
      stObject:
        begin
          o := length.AsObject.GetO('min');
          if (o <> nil) and (o.AsInteger > len) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidLength, objpath);
          end;
          o := length.AsObject.GetO('max');
          if (o <> nil) and (o.AsInteger < len) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidLength, objpath);
          end;
          o := length.AsObject.GetO('minex');
          if (o <> nil) and (o.AsInteger >= len) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidLength, objpath);
          end;
          o := length.AsObject.GetO('maxex');
          if (o <> nil) and (o.AsInteger <= len) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidLength, objpath);
          end;
        end;
      stNull: ;
    else
      Result := false;
      if assigned(callback) then
        callback(sender, veRuleMalformated, '');
    end;
  end;

  function CheckRange(obj: ISuperObject; p: ISuperObject; const objpath: SOString): boolean;
  var
    length, o: ISuperObject;
  begin
    result := true;
    length := FindInheritedProperty('range', p);
    case ObjectGetType(length) of
      stObject:
        begin
          o := length.AsObject.GetO('min');
          if (o <> nil) and (o.Compare(obj) = cpGreat) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidRange, objpath);
          end;
          o := length.AsObject.GetO('max');
          if (o <> nil) and (o.Compare(obj) = cpLess) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidRange, objpath);
          end;
          o := length.AsObject.GetO('minex');
          if (o <> nil) and (o.Compare(obj) in [cpGreat, cpEqu]) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidRange, objpath);
          end;
          o := length.AsObject.GetO('maxex');
          if (o <> nil) and (o.Compare(obj) in [cpLess, cpEqu]) then
          begin
            Result := false;
            if assigned(callback) then
              callback(sender, veInvalidRange, objpath);
          end;
        end;
      stNull: ;
    else
      Result := false;
      if assigned(callback) then
        callback(sender, veRuleMalformated, '');
    end;
  end;


  function process(o: ISuperObject; p: ISuperObject; objpath: SOString = ''): boolean;
  var
    ite: TSuperAvlIterator;
    ent: TSuperAvlEntry;
    p2, o2, sequence: ISuperObject;
    s: SOString;
    i: integer;
    uniquelist, fieldlist: ISuperObject;
  begin
    Result := true;
    if (o = nil) then
    begin
      if getInheritedBool('required', p) then
      begin
        if assigned(callback) then
          callback(sender, veFieldIsRequired, objpath);
        result := false;
      end;
    end else
      case FindDataType(p) of
        dtStr:
          case ObjectGetType(o) of
            stString:
              begin
                Result := Result and CheckLength(Length(o.AsString), p, objpath);
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtBool:
          case ObjectGetType(o) of
            stBoolean:
              begin
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtInt:
          case ObjectGetType(o) of
            stInt:
              begin
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtFloat:
          case ObjectGetType(o) of
            stDouble, stCurrency:
              begin
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtMap:
          case ObjectGetType(o) of
            stObject:
              begin
                // all objects have and match a rule ?
                ite := TSuperAvlIterator.Create(o.AsObject);
                try
                  ite.First;
                  ent := ite.GetIter;
                  while ent <> nil do
                  begin
                    p2 :=  FindInheritedField(ent.Name, p);
                    if ObjectIsType(p2, stObject) then
                      result := process(ent.Value, p2, objpath + '.' + ent.Name) and result else
                    begin
                      if assigned(callback) then
                        callback(sender, veUnexpectedField, objpath + '.' + ent.Name);
                      result := false; // field have no rule
                    end;
                    ite.Next;
                    ent := ite.GetIter;
                  end;
                finally
                  ite.Free;
                end;

                // all expected field exists ?
                Result :=  InheritedFieldExist(o, p, objpath) and Result;
              end;
            stNull: {nop};
          else
            result := false;
            if assigned(callback) then
              callback(sender, veRuleMalformated, objpath);
          end;
        dtSeq:
          case ObjectGetType(o) of
            stArray:
              begin
                sequence := FindInheritedProperty('sequence', p);
                if sequence <> nil then
                case ObjectGetType(sequence) of
                  stObject:
                    begin
                      for i := 0 to o.AsArray.Length - 1 do
                        result := process(o.AsArray.GetO(i), sequence, objpath + '[' + IntToStr(i) + ']') and result;
                      if getInheritedBool('unique', sequence) then
                      begin
                        // type is unique ?
                        uniquelist := TSuperObject.Create(stObject);
                        try
                          for i := 0 to o.AsArray.Length - 1 do
                          begin
                            s := o.AsArray.GetO(i).AsString;
                            if (s <> '') then
                            begin
                              if uniquelist.AsObject.Search(s) = nil then
                                uniquelist[s] := nil else
                                begin
                                  Result := False;
                                  if Assigned(callback) then
                                    callback(sender, veDuplicateEntry, objpath + '[' + IntToStr(i) + ']');
                                end;
                            end;
                          end;
                        finally
                          uniquelist := nil;
                        end;
                      end;

                      // field is unique ?
                      if (FindDataType(sequence) = dtMap) then
                      begin
                        fieldlist := TSuperObject.Create(stObject);
                        try
                          GetInheritedFieldList(fieldlist, sequence);
                          ite := TSuperAvlIterator.Create(fieldlist.AsObject);
                          try
                            ite.First;
                            ent := ite.GetIter;
                            while ent <> nil do
                            begin
                              if getInheritedBool('unique', ent.Value) then
                              begin
                                uniquelist := TSuperObject.Create(stObject);
                                try
                                  for i := 0 to o.AsArray.Length - 1 do
                                  begin
                                    o2 := o.AsArray.GetO(i);
                                    if o2 <> nil then
                                    begin
                                      s := o2.AsObject.GetO(ent.Name).AsString;
                                      if (s <> '') then
                                      if uniquelist.AsObject.Search(s) = nil then
                                        uniquelist[s] := nil else
                                        begin
                                          Result := False;
                                          if Assigned(callback) then
                                            callback(sender, veDuplicateEntry, objpath + '[' + IntToStr(i) + '].' + ent.name);
                                        end;
                                    end;
                                  end;
                                finally
                                  uniquelist := nil;
                                end;
                              end;
                              ite.Next;
                              ent := ite.GetIter;
                            end;
                          finally
                            ite.Free;
                          end;
                        finally
                          fieldlist := nil;
                        end;
                      end;


                    end;
                  stNull: {nop};
                else
                  result := false;
                  if assigned(callback) then
                    callback(sender, veRuleMalformated, objpath);
                end;
                Result := Result and CheckLength(o.AsArray.Length, p, objpath);

              end;
          else
            result := false;
            if assigned(callback) then
              callback(sender, veRuleMalformated, objpath);
          end;
        dtNumber:
          case ObjectGetType(o) of
            stInt,
            stDouble, stCurrency:
              begin
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtText:
          case ObjectGetType(o) of
            stInt,
            stDouble,
            stCurrency,
            stString:
              begin
                result := result and CheckLength(Length(o.AsString), p, objpath);
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtScalar:
          case ObjectGetType(o) of
            stBoolean,
            stDouble,
            stCurrency,
            stInt,
            stString:
              begin
                result := result and CheckLength(Length(o.AsString), p, objpath);
                Result := Result and CheckRange(o, p, objpath);
              end;
          else
            if assigned(callback) then
              callback(sender, veInvalidDataType, objpath);
            result := false;
          end;
        dtAny:;
      else
        if assigned(callback) then
          callback(sender, veRuleMalformated, objpath);
        result := false;
      end;
      Result := Result and CheckEnum(o, p, objpath)

  end;
var
  j: integer;

begin
  Result := False;
  datatypes := TSuperObject.Create(stObject);
  names := TSuperObject.Create;
  try
    datatypes.I['str'] := ord(dtStr);
    datatypes.I['int'] := ord(dtInt);
    datatypes.I['float'] := ord(dtFloat);
    datatypes.I['number'] := ord(dtNumber);
    datatypes.I['text'] := ord(dtText);
    datatypes.I['bool'] := ord(dtBool);
    datatypes.I['map'] := ord(dtMap);
    datatypes.I['seq'] := ord(dtSeq);
    datatypes.I['scalar'] := ord(dtScalar);
    datatypes.I['any'] := ord(dtAny);

    if ObjectIsType(defs, stArray) then
      for j := 0 to defs.AsArray.Length - 1 do
        if ObjectIsType(defs.AsArray[j], stObject) then
          GetNames(defs.AsArray[j]) else
          begin
            if assigned(callback) then
              callback(sender, veRuleMalformated, '');
            Exit;
          end;


    if ObjectIsType(rules, stObject) then
      GetNames(rules) else
      begin
        if assigned(callback) then
          callback(sender, veRuleMalformated, '');
        Exit;
      end;

    Result := process(self, rules);

  finally
    datatypes := nil;
    names := nil;
  end;
end;

function TSuperObject._AddRef: Integer; stdcall;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TSuperObject._Release: Integer; stdcall;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TSuperObject.Compare(const str: SOString): TSuperCompareResult;
begin
  Result := Compare(TSuperObject.ParseString(PSOChar(str), False));
end;

function TSuperObject.Compare(const obj: ISuperObject): TSuperCompareResult;
  function GetIntCompResult(const i: int64): TSuperCompareResult;
  begin
    if i < 0 then result := cpLess else
    if i = 0 then result := cpEqu else
      Result := cpGreat;
  end;

  function GetDblCompResult(const d: double): TSuperCompareResult;
  begin
    if d < 0 then result := cpLess else
    if d = 0 then result := cpEqu else
      Result := cpGreat;
  end;

begin
  case DataType of
    stBoolean:
      case ObjectGetType(obj) of
        stBoolean: Result := GetIntCompResult(ord(FO.c_boolean) - ord(obj.AsBoolean));
        stDouble:  Result := GetDblCompResult(ord(FO.c_boolean) - obj.AsDouble);
        stCurrency:Result := GetDblCompResult(ord(FO.c_boolean) - obj.AsCurrency);
        stInt:     Result := GetIntCompResult(ord(FO.c_boolean) - obj.AsInteger);
        stString:  Result := GetIntCompResult(StrComp(PSOChar(AsString), PSOChar(obj.AsString)));
      else
        Result := cpError;
      end;
    stDouble:
      case ObjectGetType(obj) of
        stBoolean: Result := GetDblCompResult(FO.c_double - ord(obj.AsBoolean));
        stDouble:  Result := GetDblCompResult(FO.c_double - obj.AsDouble);
        stCurrency:Result := GetDblCompResult(FO.c_double - obj.AsCurrency);
        stInt:     Result := GetDblCompResult(FO.c_double - obj.AsInteger);
        stString:  Result := GetIntCompResult(StrComp(PSOChar(AsString), PSOChar(obj.AsString)));
      else
        Result := cpError;
      end;
    stCurrency:
      case ObjectGetType(obj) of
        stBoolean: Result := GetDblCompResult(FO.c_currency - ord(obj.AsBoolean));
        stDouble:  Result := GetDblCompResult(FO.c_currency - obj.AsDouble);
        stCurrency:Result := GetDblCompResult(FO.c_currency - obj.AsCurrency);
        stInt:     Result := GetDblCompResult(FO.c_currency - obj.AsInteger);
        stString:  Result := GetIntCompResult(StrComp(PSOChar(AsString), PSOChar(obj.AsString)));
      else
        Result := cpError;
      end;
    stInt:
      case ObjectGetType(obj) of
        stBoolean: Result := GetIntCompResult(FO.c_int - ord(obj.AsBoolean));
        stDouble:  Result := GetDblCompResult(FO.c_int - obj.AsDouble);
        stCurrency:Result := GetDblCompResult(FO.c_int - obj.AsCurrency);
        stInt:     Result := GetIntCompResult(FO.c_int - obj.AsInteger);
        stString:  Result := GetIntCompResult(StrComp(PSOChar(AsString), PSOChar(obj.AsString)));
      else
        Result := cpError;
      end;
    stString:
      case ObjectGetType(obj) of
        stBoolean,
        stDouble,
        stCurrency,
        stInt,
        stString:  Result := GetIntCompResult(StrComp(PSOChar(AsString), PSOChar(obj.AsString)));
      else
        Result := cpError;
      end;
  else
    Result := cpError;
  end;
end;

{$IFDEF SUPER_METHOD}
function TSuperObject.AsMethod: TSuperMethod;
begin
  if FDataType = stMethod then
    Result := FO.c_method else
    Result := nil;
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
constructor TSuperObject.Create(m: TSuperMethod);
begin
  Create(stMethod);
  FO.c_method := m;
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
function TSuperObject.GetM(const path: SOString): TSuperMethod;
var
  v: ISuperObject;
begin
  v := ParseString(PSOChar(path), False, True, Self);
  if (v <> nil) and (ObjectGetType(v) = stMethod) then
    Result := v.AsMethod else
    Result := nil;
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
procedure TSuperObject.PutM(const path: SOString; Value: TSuperMethod);
begin
  ParseString(PSOChar(path), False, True, Self, [foCreatePath, foPutValue], TSuperObject.Create(Value));
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
function TSuperObject.call(const path: SOString; const param: ISuperObject): ISuperObject;
begin
  Result := ParseString(PSOChar(path), False, True, Self, [foCallMethod], param);
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
function TSuperObject.call(const path, param: SOString): ISuperObject;
begin
  Result := ParseString(PSOChar(path), False, True, Self, [foCallMethod], TSuperObject.ParseString(PSOChar(param), False));
end;
{$ENDIF}

function TSuperObject.GetProcessing: boolean;
begin
  Result := FProcessing;
end;

procedure TSuperObject.SetDataPtr(const Value: Pointer);
begin
  FDataPtr := Value;
end;

procedure TSuperObject.SetProcessing(value: boolean);
begin
  FProcessing := value;
end;

{ TSuperArray }

function TSuperArray.Add(const Data: ISuperObject): Integer;
begin
  Result := FLength;
  PutO(Result, data);
end;

function TSuperArray.Add(Data: SuperInt): Integer;
begin
  Result := Add(TSuperObject.Create(Data));
end;

function TSuperArray.Add(const Data: SOString): Integer;
begin
  Result := Add(TSuperObject.Create(Data));
end;

function TSuperArray.Add(Data: Boolean): Integer;
begin
  Result := Add(TSuperObject.Create(Data));
end;

function TSuperArray.Add(Data: Double): Integer;
begin
  Result := Add(TSuperObject.Create(Data));
end;

function TSuperArray.AddC(const Data: Currency): Integer;
begin
  Result := Add(TSuperObject.CreateCurrency(Data));
end;

function TSuperArray.Delete(index: Integer): ISuperObject;
begin
  if (Index >= 0) and (Index < FLength) then
  begin
    Result := FArray^[index];
    FArray^[index] := nil;
    Dec(FLength);
    if Index < FLength then
    begin
      Move(FArray^[index + 1], FArray^[index],
        (FLength - index) * SizeOf(Pointer));
      Pointer(FArray^[FLength]) := nil;
    end;
  end;
end;

procedure TSuperArray.Insert(index: Integer; const value: ISuperObject);
begin
  if (Index >= 0) then
  if (index < FLength) then
  begin
    if FLength = FSize then
      Expand(index);
    if Index < FLength then
      Move(FArray^[index], FArray^[index + 1],
        (FLength - index) * SizeOf(Pointer));
    Pointer(FArray^[index]) := nil;
    FArray^[index] := value;
    Inc(FLength);
  end else
    PutO(index, value);
end;

procedure TSuperArray.Clear(all: boolean);
var
  j: Integer;
begin
  for j := 0 to FLength - 1 do
    if FArray^[j] <> nil then
    begin
      if all then
        FArray^[j].Clear(all);
      FArray^[j] := nil;
    end;
  FLength := 0;
end;

procedure TSuperArray.Pack(all: boolean);
var
  PackedCount, StartIndex, EndIndex, j: Integer;
begin
  if FLength > 0 then
  begin
    PackedCount := 0;
    StartIndex := 0;
    repeat
      while (StartIndex < FLength) and (FArray^[StartIndex] = nil) do
        Inc(StartIndex);
      if StartIndex < FLength then
        begin
          EndIndex := StartIndex;
          while (EndIndex < FLength) and  (FArray^[EndIndex] <> nil) do
            Inc(EndIndex);

          Dec(EndIndex);

          if StartIndex > PackedCount then
            Move(FArray^[StartIndex], FArray^[PackedCount], (EndIndex - StartIndex + 1) * SizeOf(Pointer));

          Inc(PackedCount, EndIndex - StartIndex + 1);
          StartIndex := EndIndex + 1;
        end;
    until StartIndex >= FLength;
    FillChar(FArray^[PackedCount], (FLength - PackedCount) * sizeof(Pointer), 0);
    FLength := PackedCount;
    if all then
      for j := 0 to FLength - 1 do
        FArray^[j].Pack(all);
  end;
end;

constructor TSuperArray.Create;
begin
  inherited Create;
  FSize := SUPER_ARRAY_LIST_DEFAULT_SIZE;
  FLength := 0;
  GetMem(FArray, sizeof(Pointer) * FSize);
  FillChar(FArray^, sizeof(Pointer) * FSize, 0);
end;

destructor TSuperArray.Destroy;
begin
  Clear;
  FreeMem(FArray);
  inherited;
end;

procedure TSuperArray.Expand(max: Integer);
var
  new_size: Integer;
begin
  if (max < FSize) then
    Exit;
  if max < (FSize shl 1) then
    new_size := (FSize shl 1) else
    new_size := max + 1;
  ReallocMem(FArray, new_size * sizeof(Pointer));
  FillChar(FArray^[FSize], (new_size - FSize) * sizeof(Pointer), 0);
  FSize := new_size;
end;

function TSuperArray.GetO(const index: Integer): ISuperObject;
begin
  if(index >= FLength) then
    Result := nil else
    Result := FArray^[index];
end;

function TSuperArray.GetB(const index: integer): Boolean;
var
  obj: ISuperObject;
begin
  obj := GetO(index);
  if obj <> nil then
    Result := obj.AsBoolean else
    Result := false;
end;

function TSuperArray.GetD(const index: integer): Double;
var
  obj: ISuperObject;
begin
  obj := GetO(index);
  if obj <> nil then
    Result := obj.AsDouble else
    Result := 0.0;
end;

function TSuperArray.GetI(const index: integer): SuperInt;
var
  obj: ISuperObject;
begin
  obj := GetO(index);
  if obj <> nil then
    Result := obj.AsInteger else
    Result := 0;
end;

function TSuperArray.GetS(const index: integer): SOString;
var
  obj: ISuperObject;
begin
  obj := GetO(index);
  if obj <> nil then
    Result := obj.AsString else
    Result := '';
end;

procedure TSuperArray.PutO(const index: Integer; const Value: ISuperObject);
begin
  Expand(index);
  FArray^[index] := value;
  if(FLength <= index) then FLength := index + 1;
end;

function TSuperArray.GetN(const index: integer): ISuperObject;
begin
  Result := GetO(index);
  if Result = nil then
    Result := TSuperObject.Create(stNull);
end;

procedure TSuperArray.PutN(const index: integer; const Value: ISuperObject);
begin
  if Value <> nil then
    PutO(index, Value) else
    PutO(index, TSuperObject.Create(stNull));
end;

procedure TSuperArray.PutB(const index: integer; Value: Boolean);
begin
  PutO(index, TSuperObject.Create(Value));
end;

procedure TSuperArray.PutD(const index: integer; Value: Double);
begin
  PutO(index, TSuperObject.Create(Value));
end;

function TSuperArray.GetC(const index: integer): Currency;
var
  obj: ISuperObject;
begin
  obj := GetO(index);
  if obj <> nil then
    Result := obj.AsCurrency else
    Result := 0.0;
end;

procedure TSuperArray.PutC(const index: integer; Value: Currency);
begin
  PutO(index, TSuperObject.CreateCurrency(Value));
end;

procedure TSuperArray.PutI(const index: integer; Value: SuperInt);
begin
  PutO(index, TSuperObject.Create(Value));
end;

procedure TSuperArray.PutS(const index: integer; const Value: SOString);
begin
  PutO(index, TSuperObject.Create(Value));
end;

{$IFDEF SUPER_METHOD}
function TSuperArray.GetM(const index: integer): TSuperMethod;
var
  v: ISuperObject;
begin
  v := GetO(index);
  if (ObjectGetType(v) = stMethod) then
    Result := v.AsMethod else
    Result := nil;
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
procedure TSuperArray.PutM(const index: integer; Value: TSuperMethod);
begin
  PutO(index, TSuperObject.Create(Value));
end;
{$ENDIF}

{ TSuperWriterString }

function TSuperWriterString.Append(buf: PSOChar; Size: Integer): Integer;
  function max(a, b: Integer): integer; begin if a > b then  Result := a else Result := b end;
begin
  Result := size;
  if Size > 0 then
  begin
    if (FSize - FBPos <= size) then
    begin
      FSize := max(FSize * 2, FBPos + size + 8);
      ReallocMem(FBuf, FSize * SizeOf(SOChar));
    end;
    // fast move
    case size of
    1: FBuf[FBPos] := buf^;
    2: PInteger(@FBuf[FBPos])^ := PInteger(buf)^;
    4: PInt64(@FBuf[FBPos])^ := PInt64(buf)^;
    else
      move(buf^, FBuf[FBPos], size * SizeOf(SOChar));
    end;
    inc(FBPos, size);
    FBuf[FBPos] := #0;
  end;
end;

function TSuperWriterString.Append(buf: PSOChar): Integer;
begin
  Result := Append(buf, strlen(buf));
end;

constructor TSuperWriterString.Create;
begin
  inherited;
  FSize := 32;
  FBPos := 0;
  GetMem(FBuf, FSize * SizeOf(SOChar));
end;

destructor TSuperWriterString.Destroy;
begin
  inherited;
  if FBuf <> nil then
    FreeMem(FBuf)
end;

function TSuperWriterString.GetString: SOString;
begin
  SetString(Result, FBuf, FBPos);
end;

procedure TSuperWriterString.Reset;
begin
  FBuf[0] := #0;
  FBPos := 0;
end;

procedure TSuperWriterString.TrimRight;
begin
  while (FBPos > 0) and (FBuf[FBPos-1] < #256) and (AnsiChar(FBuf[FBPos-1]) in [#32, #13, #10]) do
  begin
    dec(FBPos);
    FBuf[FBPos] := #0;
  end;
end;

{ TSuperWriterStream }

function TSuperWriterStream.Append(buf: PSOChar): Integer;
begin
  Result := Append(buf, StrLen(buf));
end;

constructor TSuperWriterStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

procedure TSuperWriterStream.Reset;
begin
  FStream.Size := 0;
end;

{ TSuperWriterStream }

function TSuperAnsiWriterStream.Append(buf: PSOChar; Size: Integer): Integer;
var
  Buffer: array[0..1023] of AnsiChar;
  pBuffer: PAnsiChar;
  i: Integer;
begin
  if Size = 1 then
    Result := FStream.Write(buf^, Size) else
  begin
    if Size > SizeOf(Buffer) then
      GetMem(pBuffer, Size) else
      pBuffer := @Buffer;
    try
      for i :=  0 to Size - 1 do
        pBuffer[i] := AnsiChar(buf[i]);
      Result := FStream.Write(pBuffer^, Size);
    finally
      if pBuffer <> @Buffer then
        FreeMem(pBuffer);
    end;
  end;
end;

{ TSuperUnicodeWriterStream }

function TSuperUnicodeWriterStream.Append(buf: PSOChar; Size: Integer): Integer;
begin
  Result := FStream.Write(buf^, Size * 2);
end;

{ TSuperWriterFake }

function TSuperWriterFake.Append(buf: PSOChar; Size: Integer): Integer;
begin
  inc(FSize, Size);
  Result := FSize;
end;

function TSuperWriterFake.Append(buf: PSOChar): Integer;
begin
  inc(FSize, Strlen(buf));
  Result := FSize;
end;

constructor TSuperWriterFake.Create;
begin
  inherited Create;
  FSize := 0;
end;

procedure TSuperWriterFake.Reset;
begin
  FSize := 0;
end;

{ TSuperWriterSock }

function TSuperWriterSock.Append(buf: PSOChar; Size: Integer): Integer;
var
  Buffer: array[0..1023] of AnsiChar;
  pBuffer: PAnsiChar;
  i: Integer;
begin
  if Size = 1 then
{$IFDEF FPC}
    Result := fpsend(FSocket, buf, size, 0) else
{$ELSE}
    Result := send(FSocket, buf^, size, 0) else
{$ENDIF}
  begin
    if Size > SizeOf(Buffer) then
      GetMem(pBuffer, Size) else
      pBuffer := @Buffer;
    try
      for i :=  0 to Size - 1 do
        pBuffer[i] := AnsiChar(buf[i]);
{$IFDEF FPC}
      Result := fpsend(FSocket, pBuffer, size, 0);
{$ELSE}
      Result := send(FSocket, pBuffer^, size, 0);
{$ENDIF}
    finally
      if pBuffer <> @Buffer then
        FreeMem(pBuffer);
    end;
  end;
  inc(FSize, Result);
end;

function TSuperWriterSock.Append(buf: PSOChar): Integer;
begin
  Result := Append(buf, StrLen(buf));
end;

constructor TSuperWriterSock.Create(ASocket: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FSize := 0;
end;

procedure TSuperWriterSock.Reset;
begin
  FSize := 0;
end;

{ TSuperTokenizer }

constructor TSuperTokenizer.Create;
begin
  pb := TSuperWriterString.Create;
  line := 1;
  col := 0;
  Reset;
end;

destructor TSuperTokenizer.Destroy;
begin
  Reset;
  pb.Free;
  inherited;
end;

procedure TSuperTokenizer.Reset;
var
  i: integer;
begin
  for i := depth downto 0 do
    ResetLevel(i);
  depth := 0;
  err := teSuccess;
end;

procedure TSuperTokenizer.ResetLevel(adepth: integer);
begin
  stack[adepth].state := tsEatws;
  stack[adepth].saved_state := tsStart;
  stack[adepth].current := nil;
  stack[adepth].field_name := '';
  stack[adepth].obj := nil;
  stack[adepth].parent := nil;
  stack[adepth].gparent := nil;
end;

{ TSuperAvlTree }

constructor TSuperAvlTree.Create;
begin
  FRoot := nil;
  FCount := 0;
end;

destructor TSuperAvlTree.Destroy;
begin
  Clear;
  inherited;
end;

function TSuperAvlTree.IsEmpty: boolean;
begin
  result := FRoot = nil;
end;

function TSuperAvlTree.balance(bal: TSuperAvlEntry): TSuperAvlEntry;
var
  deep, old: TSuperAvlEntry;
  bf: integer;
begin
  if (bal.FBf > 0) then
  begin
    deep := bal.FGt;
    if (deep.FBf < 0) then
    begin
      old := bal;
      bal := deep.FLt;
      old.FGt := bal.FLt;
      deep.FLt := bal.FGt;
      bal.FLt := old;
      bal.FGt := deep;
      bf := bal.FBf;
      if (bf <> 0) then
      begin
        if (bf > 0) then
        begin
          old.FBf := -1;
          deep.FBf := 0;
        end else
        begin
          deep.FBf := 1;
          old.FBf := 0;
        end;
        bal.FBf := 0;
      end else
      begin
        old.FBf := 0;
        deep.FBf := 0;
      end;
    end else
    begin
      bal.FGt := deep.FLt;
      deep.FLt := bal;
      if (deep.FBf = 0) then
      begin
        deep.FBf := -1;
        bal.FBf := 1;
      end else
      begin
        deep.FBf := 0;
        bal.FBf := 0;
      end;
      bal := deep;
    end;
  end else
  begin
    (* "Less than" subtree is deeper. *)

    deep := bal.FLt;
    if (deep.FBf > 0) then
    begin
      old := bal;
      bal := deep.FGt;
      old.FLt := bal.FGt;
      deep.FGt := bal.FLt;
      bal.FGt := old;
      bal.FLt := deep;

      bf := bal.FBf;
      if (bf <> 0) then
      begin
        if (bf < 0) then
        begin
          old.FBf := 1;
          deep.FBf := 0;
        end else
        begin
          deep.FBf := -1;
          old.FBf := 0;
        end;
        bal.FBf := 0;
      end else
      begin
        old.FBf := 0;
        deep.FBf := 0;
      end;
    end else
    begin
      bal.FLt := deep.FGt;
      deep.FGt := bal;
      if (deep.FBf = 0) then
      begin
        deep.FBf := 1;
        bal.FBf := -1;
      end else
      begin
        deep.FBf := 0;
        bal.FBf := 0;
      end;
      bal := deep;
    end;
  end;
  Result := bal;
end;

function TSuperAvlTree.Insert(h: TSuperAvlEntry): TSuperAvlEntry;
var
  unbal, parentunbal, hh, parent: TSuperAvlEntry;
  depth, unbaldepth: longint;
  cmp: integer;
  unbalbf: integer;
  branch: TSuperAvlBitArray;
  p: Pointer;
begin
  inc(FCount);
  h.FLt := nil;
  h.FGt := nil;
  h.FBf := 0;
  branch := [];

  if (FRoot = nil) then
    FRoot := h
  else
  begin
    unbal := nil;
    parentunbal := nil;
    depth := 0;
    unbaldepth := 0;
    hh := FRoot;
    parent := nil;
    repeat
      if (hh.FBf <> 0) then
      begin
        unbal := hh;
        parentunbal := parent;
        unbaldepth := depth;
      end;
      if hh.FHash <> h.FHash then
      begin
        if hh.FHash < h.FHash then cmp := -1 else
        if hh.FHash > h.FHash then cmp := 1 else
          cmp := 0;
      end else
        cmp := CompareNodeNode(h, hh);
      if (cmp = 0) then
      begin
        Result := hh;
        //exchange data
        p := hh.Ptr;
        hh.FPtr := h.Ptr;
        h.FPtr := p;
        doDeleteEntry(h, false);
        dec(FCount);
        exit;
      end;
      parent := hh;
      if (cmp > 0) then
      begin
        hh := hh.FGt;
        include(branch, depth);
      end else
      begin
        hh := hh.FLt;
        exclude(branch, depth);
      end;
      inc(depth);
    until (hh = nil);

    if (cmp < 0) then
      parent.FLt := h else
      parent.FGt := h;

    depth := unbaldepth;

    if (unbal = nil) then
      hh := FRoot
    else
    begin
      if depth in branch then
        cmp := 1 else
        cmp := -1;
      inc(depth);
      unbalbf := unbal.FBf;
      if (cmp < 0) then
        dec(unbalbf) else
        inc(unbalbf);
      if cmp < 0 then
        hh := unbal.FLt else
        hh := unbal.FGt;
      if ((unbalbf <> -2) and (unbalbf <> 2)) then
      begin
        unbal.FBf := unbalbf;
        unbal := nil;
      end;
    end;

    if (hh <> nil) then
      while (h <> hh) do
      begin
        if depth in branch then
          cmp := 1 else
          cmp := -1;
        inc(depth);
        if (cmp < 0) then
        begin
          hh.FBf := -1;
          hh := hh.FLt;
        end else (* cmp > 0 *)
        begin
          hh.FBf := 1;
          hh := hh.FGt;
        end;
      end;

    if (unbal <> nil) then
    begin
      unbal := balance(unbal);
      if (parentunbal = nil) then
        FRoot := unbal
      else
      begin
        depth := unbaldepth - 1;
        if depth in branch then
          cmp := 1 else
          cmp := -1;
        if (cmp < 0) then
          parentunbal.FLt := unbal else
          parentunbal.FGt := unbal;
      end;
    end;
  end;
  result := h;
end;

function TSuperAvlTree.Search(const k: SOString; st: TSuperAvlSearchTypes): TSuperAvlEntry;
var
  cmp, target_cmp: integer;
  match_h, h: TSuperAvlEntry;
  ha: Cardinal;
begin
  ha := TSuperAvlEntry.Hash(k);

  match_h := nil;
  h := FRoot;

  if (stLess in st) then
    target_cmp := 1 else
    if (stGreater in st) then
      target_cmp := -1 else
      target_cmp := 0;

  while (h <> nil) do
  begin
    if h.FHash < ha then cmp := -1 else
    if h.FHash > ha then cmp := 1 else
      cmp := 0;

    if cmp = 0 then
      cmp := CompareKeyNode(PSOChar(k), h);
    if (cmp = 0) then
    begin
      if (stEqual in st) then
      begin
        match_h := h;
        break;
      end;
      cmp := -target_cmp;
    end
    else
    if (target_cmp <> 0) then
      if ((cmp xor target_cmp) and SUPER_AVL_MASK_HIGH_BIT) = 0 then
        match_h := h;
    if cmp < 0 then
      h := h.FLt else
      h := h.FGt;
  end;
  result := match_h;
end;

function TSuperAvlTree.Delete(const k: SOString): ISuperObject;
var
  depth, rm_depth: longint;
  branch: TSuperAvlBitArray;
  h, parent, child, path, rm, parent_rm: TSuperAvlEntry;
  cmp, cmp_shortened_sub_with_path, reduced_depth, bf: integer;
  ha: Cardinal;
begin
  ha := TSuperAvlEntry.Hash(k);
  cmp_shortened_sub_with_path := 0;
  branch := [];

  depth := 0;
  h := FRoot;
  parent := nil;
  while true do
  begin
    if (h = nil) then
      exit;
    if h.FHash < ha then cmp := -1 else
    if h.FHash > ha then cmp := 1 else
      cmp := 0;

    if cmp = 0 then
      cmp := CompareKeyNode(k, h);
    if (cmp = 0) then
      break;
    parent := h;
    if (cmp > 0) then
    begin
      h := h.FGt;
      include(branch, depth)
    end else
    begin
      h := h.FLt;
      exclude(branch, depth)
    end;
    inc(depth);
    cmp_shortened_sub_with_path := cmp;
  end;
  rm := h;
  parent_rm := parent;
  rm_depth := depth;

  if (h.FBf < 0) then
  begin
    child := h.FLt;
    exclude(branch, depth);
    cmp := -1;
  end else
  begin
    child := h.FGt;
    include(branch, depth);
    cmp := 1;
  end;
  inc(depth);

  if (child <> nil) then
  begin
    cmp := -cmp;
    repeat
      parent := h;
      h := child;
      if (cmp < 0) then
      begin
        child := h.FLt;
        exclude(branch, depth);
      end else
      begin
        child := h.FGt;
        include(branch, depth);
      end;
      inc(depth);
    until (child = nil);

    if (parent = rm) then
      cmp_shortened_sub_with_path := -cmp else
      cmp_shortened_sub_with_path := cmp;

    if cmp > 0 then
      child := h.FLt else
      child := h.FGt;
  end;

  if (parent = nil) then
    FRoot := child else
    if (cmp_shortened_sub_with_path < 0) then
      parent.FLt := child else
      parent.FGt := child;

  if parent = rm then
    path := h else
    path := parent;

  if (h <> rm) then
  begin
    h.FLt := rm.FLt;
    h.FGt := rm.FGt;
    h.FBf := rm.FBf;
    if (parent_rm = nil) then
      FRoot := h
    else
    begin
      depth := rm_depth - 1;
      if (depth in branch) then
        parent_rm.FGt := h else
        parent_rm.FLt := h;
    end;
  end;

  if (path <> nil) then
  begin
    h := FRoot;
    parent := nil;
    depth := 0;
    while (h <> path) do
    begin
      if (depth in branch) then
      begin
        child := h.FGt;
        h.FGt := parent;
      end else
      begin
        child := h.FLt;
        h.FLt := parent;
      end;
      inc(depth);
      parent := h;
      h := child;
    end;

    reduced_depth := 1;
    cmp := cmp_shortened_sub_with_path;
    while true do
    begin
      if (reduced_depth <> 0) then
      begin
        bf := h.FBf;
        if (cmp < 0) then
          inc(bf) else
          dec(bf);
        if ((bf = -2) or (bf = 2)) then
        begin
          h := balance(h);
          bf := h.FBf;
        end else
          h.FBf := bf;
        reduced_depth := integer(bf = 0);
      end;
      if (parent = nil) then
        break;
      child := h;
      h := parent;
      dec(depth);
      if depth in branch then
        cmp := 1 else
        cmp := -1;
      if (cmp < 0) then
      begin
        parent := h.FLt;
        h.FLt := child;
      end else
      begin
        parent := h.FGt;
        h.FGt := child;
      end;
    end;
    FRoot := h;
  end;
  if rm <> nil then
  begin
    Result := rm.GetValue;
    doDeleteEntry(rm, false);
    dec(FCount);
  end;
end;

procedure TSuperAvlTree.Pack(all: boolean);
var
  node1, node2: TSuperAvlEntry;
  list: TList;
  i: Integer;
begin
  node1 := FRoot;
  list := TList.Create;
  while node1 <> nil do
  begin
    if (node1.FLt = nil) then
    begin
      node2 := node1.FGt;
      if (node1.FPtr = nil) then
        list.Add(node1) else
        if all then
          node1.Value.Pack(all);
    end
    else
    begin
      node2 := node1.FLt;
      node1.FLt := node2.FGt;
      node2.FGt := node1;
    end;
    node1 := node2;
  end;
  for i := 0 to list.Count - 1 do
    Delete(TSuperAvlEntry(list[i]).FName);
  list.Free;
end;

procedure TSuperAvlTree.Clear(all: boolean);
var
  node1, node2: TSuperAvlEntry;
begin
  node1 := FRoot;
  while node1 <> nil do
  begin
    if (node1.FLt = nil) then
    begin
      node2 := node1.FGt;
      doDeleteEntry(node1, all);
    end
    else
    begin
      node2 := node1.FLt;
      node1.FLt := node2.FGt;
      node2.FGt := node1;
    end;
    node1 := node2;
  end;
  FRoot := nil;
  FCount := 0;
end;

function TSuperAvlTree.CompareKeyNode(const k: SOString; h: TSuperAvlEntry): integer;
begin
  Result := StrComp(PSOChar(k), PSOChar(h.FName));
end;

function TSuperAvlTree.CompareNodeNode(node1, node2: TSuperAvlEntry): integer;
begin
  Result := StrComp(PSOChar(node1.FName), PSOChar(node2.FName));
end;

{ TSuperAvlIterator }

(* Initialize depth to invalid value, to indicate iterator is
** invalid.   (Depth is zero-base.)  It's not necessary to initialize
** iterators prior to passing them to the "start" function.
*)

constructor TSuperAvlIterator.Create(tree: TSuperAvlTree);
begin
  FDepth := not 0;
  FTree := tree;
end;

procedure TSuperAvlIterator.Search(const k: SOString; st: TSuperAvlSearchTypes);
var
  h: TSuperAvlEntry;
  d: longint;
  cmp, target_cmp: integer;
  ha: Cardinal;
begin
  ha := TSuperAvlEntry.Hash(k);
  h := FTree.FRoot;
  d := 0;
  FDepth := not 0;
  if (h = nil) then
    exit;

  if (stLess in st) then
    target_cmp := 1 else
      if (stGreater in st) then
        target_cmp := -1 else
          target_cmp := 0;

  while true do
  begin
    if h.FHash < ha then cmp := -1 else
    if h.FHash > ha then cmp := 1 else
      cmp := 0;

    if cmp = 0 then
      cmp := FTree.CompareKeyNode(k, h);
    if (cmp = 0) then
    begin
      if (stEqual in st) then
      begin
        FDepth := d;
        break;
      end;
      cmp := -target_cmp;
    end
    else
    if (target_cmp <> 0) then
      if ((cmp xor target_cmp) and SUPER_AVL_MASK_HIGH_BIT) = 0 then
        FDepth := d;
    if cmp < 0 then
      h := h.FLt else
      h := h.FGt;
    if (h = nil) then
      break;
    if (cmp > 0) then
      include(FBranch, d) else
      exclude(FBranch, d);
    FPath[d] := h;
    inc(d);
  end;
end;

procedure TSuperAvlIterator.First;
var
  h: TSuperAvlEntry;
begin
  h := FTree.FRoot;
  FDepth := not 0;
  FBranch := [];
  while (h <> nil) do
  begin
    if (FDepth <> not 0) then
      FPath[FDepth] := h;
    inc(FDepth);
    h := h.FLt;
  end;
end;

procedure TSuperAvlIterator.Last;
var
  h: TSuperAvlEntry;
begin
  h := FTree.FRoot;
  FDepth := not 0;
  FBranch := [0..SUPER_AVL_MAX_DEPTH - 1];
  while (h <> nil) do
  begin
    if (FDepth <> not 0) then
      FPath[FDepth] := h;
    inc(FDepth);
    h := h.FGt;
  end;
end;

function TSuperAvlIterator.MoveNext: boolean;
begin
  if FDepth = not 0 then
    First else
    Next;
  Result := GetIter <> nil;
end;

function TSuperAvlIterator.GetIter: TSuperAvlEntry;
begin
  if (FDepth = not 0) then
  begin
    result := nil;
    exit;
  end;
  if FDepth = 0 then
    Result := FTree.FRoot else
    Result := FPath[FDepth - 1];
end;

procedure TSuperAvlIterator.Next;
var
  h: TSuperAvlEntry;
begin
  if (FDepth <> not 0) then
  begin
    if FDepth = 0 then
      h := FTree.FRoot.FGt else
      h := FPath[FDepth - 1].FGt;

    if (h = nil) then
      repeat
        if (FDepth = 0) then
        begin
          FDepth := not 0;
          break;
        end;
        dec(FDepth);
      until (not (FDepth in FBranch))
    else
    begin
      include(FBranch, FDepth);
      FPath[FDepth] := h;
      inc(FDepth);
      while true do
      begin
        h := h.FLt;
        if (h = nil) then
          break;
        exclude(FBranch, FDepth);
        FPath[FDepth] := h;
        inc(FDepth);
      end;
    end;
  end;
end;

procedure TSuperAvlIterator.Prior;
var
  h: TSuperAvlEntry;
begin
  if (FDepth <> not 0) then
  begin
    if FDepth = 0 then
      h := FTree.FRoot.FLt else
      h := FPath[FDepth - 1].FLt;
    if (h = nil) then
      repeat
        if (FDepth = 0) then
        begin
          FDepth := not 0;
          break;
        end;
        dec(FDepth);
      until (FDepth in FBranch)
    else
    begin
      exclude(FBranch, FDepth);
      FPath[FDepth] := h;
      inc(FDepth);
      while true do
      begin
        h := h.FGt;
        if (h = nil) then
          break;
        include(FBranch, FDepth);
        FPath[FDepth] := h;
        inc(FDepth);
      end;
    end;
  end;
end;

procedure TSuperAvlTree.doDeleteEntry(Entry: TSuperAvlEntry; all: boolean);
begin
  Entry.Free;
end;

function TSuperAvlTree.GetEnumerator: TSuperAvlIterator;
begin
  Result := TSuperAvlIterator.Create(Self);
end;

{ TSuperAvlEntry }

constructor TSuperAvlEntry.Create(const AName: SOString; Obj: Pointer);
begin
  FName := AName;
  FPtr := Obj;
  FHash := Hash(FName);
end;

function TSuperAvlEntry.GetValue: ISuperObject;
begin
  Result := ISuperObject(FPtr)
end;

class function TSuperAvlEntry.Hash(const k: SOString): Cardinal;
var
  h: cardinal;
  i: Integer;
begin
  h := 0;
  for i := 1 to Length(k) do
    h := h*129 + ord(k[i]) + $9e370001;
  Result := h;
end;

procedure TSuperAvlEntry.SetValue(const val: ISuperObject);
begin
  ISuperObject(FPtr) := val;
end;

{ TSuperTableString }

function TSuperTableString.GetValues: ISuperObject;
var
  ite: TSuperAvlIterator;
  obj: TSuperAvlEntry;
begin
  Result := TSuperObject.Create(stArray);
  ite := TSuperAvlIterator.Create(Self);
  try
    ite.First;
    obj := ite.GetIter;
    while obj <> nil do
    begin
      Result.AsArray.Add(obj.Value);
      ite.Next;
      obj := ite.GetIter;
    end;
  finally
    ite.Free;
  end;
end;

function TSuperTableString.GetNames: ISuperObject;
var
  ite: TSuperAvlIterator;
  obj: TSuperAvlEntry;
begin
  Result := TSuperObject.Create(stArray);
  ite := TSuperAvlIterator.Create(Self);
  try
    ite.First;
    obj := ite.GetIter;
    while obj <> nil do
    begin
      Result.AsArray.Add(TSuperObject.Create(obj.FName));
      ite.Next;
      obj := ite.GetIter;
    end;
  finally
    ite.Free;
  end;
end;

procedure TSuperTableString.doDeleteEntry(Entry: TSuperAvlEntry; all: boolean);
begin
  if Entry.Ptr <> nil then
  begin
    if all then Entry.Value.Clear(true);
    Entry.Value := nil;
  end;
  inherited;
end;

function TSuperTableString.Find(const k: SOString; var value: ISuperObject): Boolean;
var
  e: TSuperAvlEntry;
begin
  e := Search(k);
  if e <> nil then
  begin
    value := e.Value;
    Result := True;
  end else
    Result := False;
end;

function TSuperTableString.Exists(const k: SOString): Boolean;
begin
  Result := Search(k) <> nil;
end;

function TSuperTableString.GetO(const k: SOString): ISuperObject;
var
  e: TSuperAvlEntry;
begin
  e := Search(k);
  if e <> nil then
    Result := e.Value else
    Result := nil
end;

procedure TSuperTableString.PutO(const k: SOString; const value: ISuperObject);
var
  entry: TSuperAvlEntry;
begin
  entry := Insert(TSuperAvlEntry.Create(k, Pointer(value)));
  if entry.FPtr <> nil then
    ISuperObject(entry.FPtr)._AddRef;
end;

procedure TSuperTableString.PutS(const k: SOString; const value: SOString);
begin
  PutO(k, TSuperObject.Create(Value));
end;

function TSuperTableString.GetS(const k: SOString): SOString;
var
  obj: ISuperObject;
begin
 obj := GetO(k);
 if obj <> nil then
   Result := obj.AsString else
   Result := '';
end;

procedure TSuperTableString.PutI(const k: SOString; value: SuperInt);
begin
  PutO(k, TSuperObject.Create(Value));
end;

function TSuperTableString.GetI(const k: SOString): SuperInt;
var
  obj: ISuperObject;
begin
 obj := GetO(k);
 if obj <> nil then
   Result := obj.AsInteger else
   Result := 0;
end;

procedure TSuperTableString.PutD(const k: SOString; value: Double);
begin
  PutO(k, TSuperObject.Create(Value));
end;

procedure TSuperTableString.PutC(const k: SOString; value: Currency);
begin
  PutO(k, TSuperObject.CreateCurrency(Value));
end;

function TSuperTableString.GetC(const k: SOString): Currency;
var
  obj: ISuperObject;
begin
 obj := GetO(k);
 if obj <> nil then
   Result := obj.AsCurrency else
   Result := 0.0;
end;

function TSuperTableString.GetD(const k: SOString): Double;
var
  obj: ISuperObject;
begin
 obj := GetO(k);
 if obj <> nil then
   Result := obj.AsDouble else
   Result := 0.0;
end;

procedure TSuperTableString.PutB(const k: SOString; value: Boolean);
begin
  PutO(k, TSuperObject.Create(Value));
end;

function TSuperTableString.GetB(const k: SOString): Boolean;
var
  obj: ISuperObject;
begin
 obj := GetO(k);
 if obj <> nil then
   Result := obj.AsBoolean else
   Result := False;
end;

{$IFDEF SUPER_METHOD}
procedure TSuperTableString.PutM(const k: SOString; value: TSuperMethod);
begin
  PutO(k, TSuperObject.Create(Value));
end;
{$ENDIF}

{$IFDEF SUPER_METHOD}
function TSuperTableString.GetM(const k: SOString): TSuperMethod;
var
  obj: ISuperObject;
begin
 obj := GetO(k);
 if obj <> nil then
   Result := obj.AsMethod else
   Result := nil;
end;
{$ENDIF}

procedure TSuperTableString.PutN(const k: SOString; const value: ISuperObject);
begin
  if value <> nil then
    PutO(k, TSuperObject.Create(stNull)) else
    PutO(k, value);
end;

function TSuperTableString.GetN(const k: SOString): ISuperObject;
var
  obj: ISuperObject;
begin
 obj := GetO(k);
 if obj <> nil then
   Result := obj else
   Result := TSuperObject.Create(stNull);
end;


{$IFDEF HAVE_RTTI}

{ TSuperAttribute }

constructor TSuperAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ TSuperRttiContext }

constructor TSuperRttiContext.Create;
begin
  Context := TRttiContext.Create;
  SerialFromJson := TDictionary<PTypeInfo, TSerialFromJson>.Create;
  SerialToJson := TDictionary<PTypeInfo, TSerialToJson>.Create;

  SerialFromJson.Add(TypeInfo(Boolean), serialfromboolean);
  SerialFromJson.Add(TypeInfo(TDateTime), serialfromdatetime);
  SerialFromJson.Add(TypeInfo(TGUID), serialfromguid);
  SerialToJson.Add(TypeInfo(Boolean), serialtoboolean);
  SerialToJson.Add(TypeInfo(TDateTime), serialtodatetime);
  SerialToJson.Add(TypeInfo(TGUID), serialtoguid);
end;

destructor TSuperRttiContext.Destroy;
begin
  SerialFromJson.Free;
  SerialToJson.Free;
  Context.Free;
end;

class function TSuperRttiContext.GetFieldName(r: TRttiField): string;
var
  o: TCustomAttribute;
begin
  for o in r.GetAttributes do
    if o is SOName then
      Exit(SOName(o).Name);
  Result := r.Name;
end;

class function TSuperRttiContext.GetFieldDefault(r: TRttiField; const obj: ISuperObject): ISuperObject;
var
  o: TCustomAttribute;
begin
  if not ObjectIsType(obj, stNull) then Exit(obj);
  for o in r.GetAttributes do
    if o is SODefault then
      Exit(SO(SODefault(o).Name));
  Result := obj;
end;

function TSuperRttiContext.AsType<T>(const obj: ISuperObject): T;
var
  ret: TValue;
begin
  if FromJson(TypeInfo(T), obj, ret) then
    Result := ret.AsType<T> else
    raise exception.Create('Marshalling error');
end;

function TSuperRttiContext.AsJson<T>(const obj: T; const index: ISuperObject = nil): ISuperObject;
var
  v: TValue;
begin
  TValue.Make(@obj, TypeInfo(T), v);
  if index <> nil then
    Result := ToJson(v, index) else
    Result := ToJson(v, so);
end;

function TSuperRttiContext.FromJson(TypeInfo: PTypeInfo; const obj: ISuperObject;
  var Value: TValue): Boolean;

  procedure FromChar;
  begin
    if ObjectIsType(obj, stString) and (Length(obj.AsString) = 1) then
      begin
        Value := string(AnsiString(obj.AsString)[1]);
        Result := True;
      end else
        Result := False;
  end;

  procedure FromWideChar;
  begin
    if ObjectIsType(obj, stString) and (Length(obj.AsString) = 1) then
    begin
      Value := obj.AsString[1];
      Result := True;
    end else
      Result := False;
  end;

  procedure FromInt64;
  var
    i: Int64;
  begin
    case ObjectGetType(obj) of
    stInt:
      begin
        TValue.Make(nil, TypeInfo, Value);
        TValueData(Value).FAsSInt64 := obj.AsInteger;
        Result := True;
      end;
    stString:
      begin
        if TryStrToInt64(obj.AsString, i) then
        begin
          TValue.Make(nil, TypeInfo, Value);
          TValueData(Value).FAsSInt64 := i;
          Result := True;
        end else
          Result := False;
      end;
    else
      Result := False;
    end;
  end;

  procedure FromInt(const obj: ISuperObject);
  var
    TypeData: PTypeData;
    i: Integer;
    o: ISuperObject;
  begin
    case ObjectGetType(obj) of
    stInt, stBoolean:
      begin
        i := obj.AsInteger;
        TypeData := GetTypeData(TypeInfo);
        if TypeData.MaxValue > TypeData.MinValue then
          Result := (i >= TypeData.MinValue) and (i <= TypeData.MaxValue) else
          Result := (i >= TypeData.MinValue) and (i <= Int64(PCardinal(@TypeData.MaxValue)^));
        if Result then
          TValue.Make(@i, TypeInfo, Value);
      end;
    stString:
      begin
        o := SO(obj.AsString);
        if not ObjectIsType(o, stString) then
          FromInt(o) else
          Result := False;
      end;
    else
      Result := False;
    end;
  end;

  procedure fromSet;
  var
    i: Integer;
  begin
    case ObjectGetType(obj) of
    stInt:
      begin
        TValue.Make(nil, TypeInfo, Value);
        TValueData(Value).FAsSLong := obj.AsInteger;
        Result := True;
      end;
    stString:
      begin
        if TryStrToInt(obj.AsString, i) then
        begin
          TValue.Make(nil, TypeInfo, Value);
          TValueData(Value).FAsSLong := i;
          Result := True;
        end else
          Result := False;
      end;
    else
      Result := False;
    end;
  end;

  procedure FromFloat(const obj: ISuperObject);
  var
    o: ISuperObject;
  begin
    case ObjectGetType(obj) of
    stInt, stDouble, stCurrency:
      begin
        TValue.Make(nil, TypeInfo, Value);
        case GetTypeData(TypeInfo).FloatType of
          ftSingle: TValueData(Value).FAsSingle := obj.AsDouble;
          ftDouble: TValueData(Value).FAsDouble := obj.AsDouble;
          ftExtended: TValueData(Value).FAsExtended := obj.AsDouble;
          ftComp: TValueData(Value).FAsSInt64 := obj.AsInteger;
          ftCurr: TValueData(Value).FAsCurr := obj.AsCurrency;
        end;
        Result := True;
      end;
    stString:
      begin
        o := SO(obj.AsString);
        if not ObjectIsType(o, stString) then
          FromFloat(o) else
          Result := False;
      end
    else
       Result := False;
    end;
  end;

  procedure FromString;
  begin
    case ObjectGetType(obj) of
    stObject, stArray:
      Result := False;
    stnull:
      begin
        Value := '';
        Result := True;
      end;
    else
      Value := obj.AsString;
      Result := True;
    end;
  end;

  procedure FromClass;
  var
    f: TRttiField;
    v: TValue;
  begin
    case ObjectGetType(obj) of
      stObject:
        begin
          Result := True;
          if Value.Kind <> tkClass then
            Value := GetTypeData(TypeInfo).ClassType.Create;
          for f in Context.GetType(Value.AsObject.ClassType).GetFields do
            if f.FieldType <> nil then
            begin
              v := TValue.Empty;
              Result := FromJson(f.FieldType.Handle, GetFieldDefault(f, obj.AsObject[GetFieldName(f)]), v);
              if Result then
                f.SetValue(Value.AsObject, v) else
                Exit;
            end;
        end;
      stNull:
        begin
          Value := nil;
          Result := True;
        end
    else
      // error
      Value := nil;
      Result := False;
    end;
  end;

  procedure FromRecord;
  var
    f: TRttiField;
    p: Pointer;
    v: TValue;
  begin
    Result := True;
    TValue.Make(nil, TypeInfo, Value);
    for f in Context.GetType(TypeInfo).GetFields do
    begin
      if ObjectIsType(obj, stObject) and (f.FieldType <> nil) then
      begin
{$IFDEF VER210}
        p := IValueData(TValueData(Value).FHeapData).GetReferenceToRawData;
{$ELSE}
        p := TValueData(Value).FValueData.GetReferenceToRawData;
{$ENDIF}
        Result := FromJson(f.FieldType.Handle, GetFieldDefault(f, obj.AsObject[GetFieldName(f)]), v);
        if Result then
          f.SetValue(p, v) else
          begin
            //Writeln(f.Name);
            Exit;
          end;
      end else
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

  procedure FromDynArray;
  var
    i: Integer;
    p: Pointer;
    pb: PByte;
    val: TValue;
    typ: PTypeData;
    el: PTypeInfo;
  begin
    case ObjectGetType(obj) of
    stArray:
      begin
        i := obj.AsArray.Length;
        p := nil;
        DynArraySetLength(p, TypeInfo, 1, @i);
        pb := p;
        typ := GetTypeData(TypeInfo);
        if typ.elType <> nil then
          el := typ.elType^ else
          el := typ.elType2^;

        Result := True;
        for i := 0 to i - 1 do
        begin
          Result := FromJson(el, obj.AsArray[i], val);
          if not Result then
            Break;
          val.ExtractRawData(pb);
          val := TValue.Empty;
          Inc(pb, typ.elSize);
        end;
        if Result then
          TValue.MakeWithoutCopy(@p, TypeInfo, Value) else
          DynArrayClear(p, TypeInfo);
      end;
    stNull:
      begin
        TValue.MakeWithoutCopy(nil, TypeInfo, Value);
        Result := True;
      end;
    else
      i := 1;
      p := nil;
      DynArraySetLength(p, TypeInfo, 1, @i);
      pb := p;
      typ := GetTypeData(TypeInfo);
      if typ.elType <> nil then
        el := typ.elType^ else
        el := typ.elType2^;

      Result := FromJson(el, obj, val);
      val.ExtractRawData(pb);
      val := TValue.Empty;

      if Result then
        TValue.MakeWithoutCopy(@p, TypeInfo, Value) else
        DynArrayClear(p, TypeInfo);
    end;
  end;

  procedure FromArray;
  var
    ArrayData: PArrayTypeData;
    idx: Integer;
    function ProcessDim(dim: Byte; const o: ISuperobject): Boolean;
    var
      i: Integer;
      v: TValue;
      a: PTypeData;
    begin
      if ObjectIsType(o, stArray) and (ArrayData.Dims[dim-1] <> nil) then
      begin
        a := @GetTypeData(ArrayData.Dims[dim-1]^).ArrayData;
        if (a.MaxValue - a.MinValue + 1) <> o.AsArray.Length then
        begin
          Result := False;
          Exit;
        end;
        Result := True;
        if dim = ArrayData.DimCount then
          for i := a.MinValue to a.MaxValue do
          begin
            Result := FromJson(ArrayData.ElType^, o.AsArray[i], v);
            if not Result then
              Exit;
            Value.SetArrayElement(idx, v);
            inc(idx);
          end
        else
          for i := a.MinValue to a.MaxValue do
          begin
            Result := ProcessDim(dim + 1, o.AsArray[i]);
            if not Result then
              Exit;
          end;
      end else
        Result := False;
    end;
  var
    i: Integer;
    v: TValue;
  begin
    TValue.Make(nil, TypeInfo, Value);
    ArrayData := @GetTypeData(TypeInfo).ArrayData;
    idx := 0;
    if ArrayData.DimCount = 1 then
    begin
      if ObjectIsType(obj, stArray) and (obj.AsArray.Length = ArrayData.ElCount) then
      begin
        Result := True;
        for i := 0 to ArrayData.ElCount - 1 do
        begin
          Result := FromJson(ArrayData.ElType^, obj.AsArray[i], v);
          if not Result then
            Exit;
          Value.SetArrayElement(idx, v);
          v := TValue.Empty;
          inc(idx);
        end;
      end else
        Result := False;
    end else
      Result := ProcessDim(1, obj);
  end;

  procedure FromClassRef;
  var
    r: TRttiType;
  begin
    if ObjectIsType(obj, stString) then
    begin
      r := Context.FindType(obj.AsString);
      if r <> nil then
      begin
        Value := TRttiInstanceType(r).MetaclassType;
        Result := True;
      end else
        Result := False;
    end else
      Result := False;
  end;

  procedure FromUnknown;
  begin
    case ObjectGetType(obj) of
      stBoolean:
        begin
          Value := obj.AsBoolean;
          Result := True;
        end;
      stDouble:
        begin
          Value := obj.AsDouble;
          Result := True;
        end;
      stCurrency:
        begin
          Value := obj.AsCurrency;
          Result := True;
        end;
      stInt:
        begin
          Value := obj.AsInteger;
          Result := True;
        end;
      stString:
        begin
          Value := obj.AsString;
          Result := True;
        end
    else
      Value := nil;
      Result := False;
    end;
  end;

  procedure FromInterface;
  const soguid: TGuid = '{4B86A9E3-E094-4E5A-954A-69048B7B6327}';
  var
    o: ISuperObject;
  begin
    if CompareMem(@GetTypeData(TypeInfo).Guid, @soguid, SizeOf(TGUID)) then
    begin
      if obj <> nil then
        TValue.Make(@obj, TypeInfo, Value) else
        begin
          o := TSuperObject.Create(stNull);
          TValue.Make(@o, TypeInfo, Value);
        end;
      Result := True;
    end else
      Result := False;
  end;
var
  Serial: TSerialFromJson;
begin

  if TypeInfo <> nil then
  begin
    if not SerialFromJson.TryGetValue(TypeInfo, Serial) then
      case TypeInfo.Kind of
        tkChar: FromChar;
        tkInt64: FromInt64;
        tkEnumeration, tkInteger: FromInt(obj);
        tkSet: fromSet;
        tkFloat: FromFloat(obj);
        tkString, tkLString, tkUString, tkWString: FromString;
        tkClass: FromClass;
        tkMethod: ;
        tkWChar: FromWideChar;
        tkRecord: FromRecord;
        tkPointer: ;
        tkInterface: FromInterface;
        tkArray: FromArray;
        tkDynArray: FromDynArray;
        tkClassRef: FromClassRef;
      else
        FromUnknown
      end else
      begin
        TValue.Make(nil, TypeInfo, Value);
        Result := Serial(Self, obj, Value);
      end;
  end else
    Result := False;
end;

function TSuperRttiContext.ToJson(var value: TValue; const index: ISuperObject): ISuperObject;
  procedure ToInt64;
  begin
    Result := TSuperObject.Create(SuperInt(Value.AsInt64));
  end;

  procedure ToChar;
  begin
    Result := TSuperObject.Create(string(Value.AsType<AnsiChar>));
  end;

  procedure ToInteger;
  begin
    Result := TSuperObject.Create(TValueData(Value).FAsSLong);
  end;

  procedure ToFloat;
  begin
    case Value.TypeData.FloatType of
      ftSingle: Result := TSuperObject.Create(TValueData(Value).FAsSingle);
      ftDouble: Result := TSuperObject.Create(TValueData(Value).FAsDouble);
      ftExtended: Result := TSuperObject.Create(TValueData(Value).FAsExtended);
      ftComp: Result := TSuperObject.Create(TValueData(Value).FAsSInt64);
      ftCurr: Result := TSuperObject.CreateCurrency(TValueData(Value).FAsCurr);
    end;
  end;

  procedure ToString;
  begin
    Result := TSuperObject.Create(string(Value.AsType<string>));
  end;

  procedure ToClass;
  var
    o: ISuperObject;
    f: TRttiField;
    v: TValue;
  begin
    if TValueData(Value).FAsObject <> nil then
    begin
      o := index[IntToStr(NativeInt(Value.AsObject))];
      if o = nil then
      begin
        Result := TSuperObject.Create(stObject);
        index[IntToStr(NativeInt(Value.AsObject))] := Result;
        for f in Context.GetType(Value.AsObject.ClassType).GetFields do
          if f.FieldType <> nil then
          begin
            v := f.GetValue(Value.AsObject);
            Result.AsObject[GetFieldName(f)] := ToJson(v, index);
          end
      end else
        Result := o;
    end else
      Result := nil;
  end;

  procedure ToWChar;
  begin
    Result :=  TSuperObject.Create(string(Value.AsType<WideChar>));
  end;

  procedure ToVariant;
  begin
    Result := SO(Value.AsVariant);
  end;

  procedure ToRecord;
  var
    f: TRttiField;
    v: TValue;
  begin
    Result := TSuperObject.Create(stObject);
    for f in Context.GetType(Value.TypeInfo).GetFields do
    begin
{$IFDEF VER210}
      v := f.GetValue(IValueData(TValueData(Value).FHeapData).GetReferenceToRawData);
{$ELSE}
      v := f.GetValue(TValueData(Value).FValueData.GetReferenceToRawData);
{$ENDIF}
      Result.AsObject[GetFieldName(f)] := ToJson(v, index);
    end;
  end;

  procedure ToArray;
  var
    idx: Integer;
    ArrayData: PArrayTypeData;

    procedure ProcessDim(dim: Byte; const o: ISuperObject);
    var
      dt: PTypeData;
      i: Integer;
      o2: ISuperObject;
      v: TValue;
    begin
      if ArrayData.Dims[dim-1] = nil then Exit;
      dt := GetTypeData(ArrayData.Dims[dim-1]^);
      if Dim = ArrayData.DimCount then
        for i := dt.MinValue to dt.MaxValue do
        begin
          v := Value.GetArrayElement(idx);
          o.AsArray.Add(toJSon(v, index));
          inc(idx);
        end
      else
        for i := dt.MinValue to dt.MaxValue do
        begin
          o2 := TSuperObject.Create(stArray);
          o.AsArray.Add(o2);
          ProcessDim(dim + 1, o2);
        end;
    end;
  var
    i: Integer;
    v: TValue;
  begin
    Result := TSuperObject.Create(stArray);
    ArrayData := @Value.TypeData.ArrayData;
    idx := 0;
    if ArrayData.DimCount = 1 then
      for i := 0 to ArrayData.ElCount - 1 do
      begin
        v := Value.GetArrayElement(i);
        Result.AsArray.Add(toJSon(v, index))
      end
    else
      ProcessDim(1, Result);
  end;

  procedure ToDynArray;
  var
    i: Integer;
    v: TValue;
  begin
    Result := TSuperObject.Create(stArray);
    for i := 0 to Value.GetArrayLength - 1 do
    begin
      v := Value.GetArrayElement(i);
      Result.AsArray.Add(toJSon(v, index));
    end;
  end;

  procedure ToClassRef;
  begin
    if TValueData(Value).FAsClass <> nil then
      Result :=  TSuperObject.Create(string(
        TValueData(Value).FAsClass.UnitName + '.' +
        TValueData(Value).FAsClass.ClassName)) else
      Result := nil;
  end;

  procedure ToInterface;
{$IFNDEF VER210}
  var
    intf: IInterface;
{$ENDIF}
  begin
{$IFDEF VER210}
    if TValueData(Value).FHeapData <> nil then
      TValueData(Value).FHeapData.QueryInterface(ISuperObject, Result) else
      Result := nil;
{$ELSE}
    if TValueData(Value).FValueData <> nil then
    begin
      intf := IInterface(PPointer(TValueData(Value).FValueData.GetReferenceToRawData)^);
      if intf <> nil then
        intf.QueryInterface(ISuperObject, Result) else
        Result := nil;
    end else
      Result := nil;
{$ENDIF}
  end;

var
  Serial: TSerialToJson;
begin
  if not SerialToJson.TryGetValue(value.TypeInfo, Serial) then
    case Value.Kind of
      tkInt64: ToInt64;
      tkChar: ToChar;
      tkSet, tkInteger, tkEnumeration: ToInteger;
      tkFloat: ToFloat;
      tkString, tkLString, tkUString, tkWString: ToString;
      tkClass: ToClass;
      tkWChar: ToWChar;
      tkVariant: ToVariant;
      tkRecord: ToRecord;
      tkArray: ToArray;
      tkDynArray: ToDynArray;
      tkClassRef: ToClassRef;
      tkInterface: ToInterface;
    else
      result := nil;
    end else
      Result := Serial(Self, value, index);
end;

{ TSuperObjectHelper }

constructor TSuperObjectHelper.FromJson(const obj: ISuperObject; ctx: TSuperRttiContext = nil);
var
  v: TValue;
  ctxowned: Boolean;
begin
  if ctx = nil then
  begin
    ctx := TSuperRttiContext.Create;
    ctxowned := True;
  end else
    ctxowned := False;
  try
    v := Self;
    if not ctx.FromJson(v.TypeInfo, obj, v) then
      raise Exception.Create('Invalid object');
  finally
    if ctxowned then
      ctx.Free;
  end;
end;

constructor TSuperObjectHelper.FromJson(const str: string; ctx: TSuperRttiContext = nil);
begin
  FromJson(SO(str), ctx);
end;

function TSuperObjectHelper.ToJson(ctx: TSuperRttiContext = nil): ISuperObject;
var
  v: TValue;
  ctxowned: boolean;
begin
  if ctx = nil then
  begin
    ctx := TSuperRttiContext.Create;
    ctxowned := True;
  end else
    ctxowned := False;
  try
    v := Self;
    Result := ctx.ToJson(v, SO);
  finally
    if ctxowned then
      ctx.Free;
  end;
end;

{$ENDIF}

{$IFDEF DEBUG}
initialization

finalization
  Assert(debugcount = 0, 'Memory leak');
{$ENDIF}
end.

