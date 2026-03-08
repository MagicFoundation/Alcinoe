unit Alcinoe.JSONDoc;

interface

{$I Alcinoe.inc}
{$SCOPEDENUMS OFF}

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  Alcinoe.Localization,
  Alcinoe.StringUtils,
  Alcinoe.StringList;

const
  ALJSONNodeNotFound                   = 'Node "%s" not found';
  ALJSONInvalidNodeType                = 'Invalid node type';
  ALJSONInvalidNodeSubType             = 'Invalid node sub type';
  ALJSONNodeNameCanNotBeEmpty          = 'Node name cannot be empty';
  ALJSONBinaryNotStoredAsBytes         = 'Binary is not stored as Bytes';
  ALJSONBinaryNotStoredAsStream        = 'Binary is not stored as Stream';
  ALJSONObjectIDInvalidSize            = 'ObjectID must have 12 Bytes';
  ALJSONBinaryAsPtrStreamInvalidSource = 'poBinaryAsPtrStream requires TCustomMemoryStream source';
  ALJSONListCapacityError              = 'Node list capacity out of bounds (%d)';
  ALJSONListCountError                 = 'Node list count out of bounds (%d)';
  ALJSONListIndexError                 = 'Node list index out of bounds (%d)';
  ALJSONOperationError                 = 'This operation cannot be performed on a node of type %s';
  ALJSONParseError                     = 'JSON Parse error';
  ALBSONParseError                     = 'BSON Parse error';

type

  TALJSONNodeType = (
    ntObject, // The node represents an object: { ... } or "name": { ... }
    ntArray,  // The node represents an array: [ ... ] or "name": [ ... ]
    ntText);  // The node represents a text content (string, number, true, false, null, etc.): "..." or "name": "..."

  // From https://www.mongodb.com/docs/manual/reference/bson-types/
  TALJSONNodeSubType = (
    nstFloat,      // \x01 | Floating point             | ex { a: 123.4 }
    nstText,       // \x02 | UTF-8 string               | ex { a: "xxx" }
    nstObject,     // \x03 | Embedded document          | ex { a: {} }
    nstArray,      // \x04 | Array                      | ex { a: [] }
    nstBinary,     // \x05 | Binary data
                   // \x06 | Undefined                  | Deprecated
    nstObjectID,   // \x07 | ObjectId                   | ex { a: ObjectId("507f1f77bcf86cd799439011") }
    nstBoolean,    // \x08 | Boolean                    | ex { a: False }
    nstDateTime,   // \x09 | UTC datetime               | ex { a: ISODate("yyyy-mm-ddThh:nn:ss.zzzZ") }
    nstNull,       // \x0A | Null value                 | ex { a: null }
    nstRegEx,      // \x0B | Regular expression
                   // \x0C | DBPointer                  | Deprecated
    nstJavascript, // \x0D | JavaScript code            | ex { a: function() }
                   // \x0E | Symbol                     | Deprecated
                   // \x0F | JavaScript code w/ scope   | Deprecated
    nstInt32,      // \x10 | 32-bit Integer             | ex { a: NumberInt(123) }
    nstTimestamp,  // \x11 | Timestamp                  | ex { a: Timestamp(0, 0) }
    nstInt64);     // \x12 | 64-bit Integer             | ex { a: NumberLong(123) }
                   // \x13 | Decimal128
                   // \xFF | Min key
                   // \x7F | Max key

  TALJSONStorageKind = (skInt64, skString, skBytes, skOwnedStream, skBorrowedStream);

  // Special internal type used by MongoDB replication and sharding.
  // First 4 Bytes are an increment, second 4 are a timestamp. Setting the
  // timestamp to 0 has special semantics.
  TALBSONTimestamp = packed record
    case Integer of
      0: (I64: Int64);
      1: (W1:  LongWord;
          W2:  LongWord);
  end;

  TALJSONSaveOption = (
    soNodeAutoIndent, // Automatically indents the JSON output for improved readability.
    soIgnoreControlCharacters, // Don't encode escaped characters (like \").
    soSkipNodeSubTypeHelper, // Don't use helper functions like NumberLong() to handle 64-bit Integers or NumberInt() to handle 32-bit Integers.
    soSaveInt64AsText, // JSON represents numbers as double and loses precision for large Integers. Use this option to return Int64 as string.
    soProtectedSave); // Save first to a tmp file and then later rename the tmp file to the desired filename.
  TALJSONSaveOptions = set of TALJSONSaveOption;

  TALJSONParseOption = (
    poIgnoreControlCharacters, // Don't decode escaped characters (like \").
    poClearChildNodes,         // Removes all child nodes.
    poAllowComments,           // Allow comments inside the JSON source file. ex:
                               //   {
                               //     "nodename": "nodevalue",  // your comments here
                               //   }
    poBinaryAsPtrStream);      // Parse binary values as pointer-backed streams. Requires TBytes or a TCustomMemoryStream source.
  TALJSONParseOptions = set of TALJSONParseOption;

  EALJSONDocError = class(Exception);

  // Mirror of TPerlRegExOptions to avoid to include System.RegularExpressionsCore
  TALPerlRegExOptions = set of (
    preCaseLess,       // /i -> Case insensitive
    preMultiLine,      // /m -> ^ and $ also match before/after a newline, not just at the beginning and the end of the string
    preSingleLine,     // /s -> Dot matches any character, including \n (newline). Otherwise, it matches anything except \n
    preExtended,       // /x -> Allow regex to contain extra whitespace, newlines and Perl-style comments, all of which will be filtered out
    preAnchored,       // /A -> Successful match can only occur at the start of the subject or right after the previous match
    preUnGreedy,       // Repeat operators (+, *, ?) are not greedy by default (i.e. they try to match the minimum number of characters instead of the maximum)
    preNoAutoCapture); // (group) is a non-capturing group; only named groups capture

type

  TALJSONNodeA = class;
  TALJSONNodeListA = class;
  TALJSONDocumentA = class;

  TALJSONParseTextEventA = reference to procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType);
  TALJSONParseObjectEventA = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  TALJSONParseArrayEventA = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  TALJSONNodeListSortCompareA = reference to function(List: TALJSONNodeListA; Index1, Index2: Integer): Integer;
  TALJSONPointerListA = array of TALJSONNodeA;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONNodeListA = class(TObject)
  Private
    FCapacity: Integer; // 4 Bytes
    FCount: Integer; // 4 Bytes
    FList: TALJSONPointerListA; // 8 Bytes
    procedure QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareA);
  protected
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function Get(Index: Integer): TALJSONNodeA;
    function GetNodeByIndex(const Index: Integer): TALJSONNodeA;
    function GetNodeByName(const Name: AnsiString): TALJSONNodeA;
    function CompareNodeNames(const S1, S2: AnsiString): Integer; inline;
    function Find(const NodeName: AnsiString; var Index: Integer): Boolean;
    procedure InternalInsert(Index: Integer; const Node: TALJSONNodeA);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Sort;
    procedure CustomSort(Compare: TALJSONNodeListSortCompareA);
    function Add(const Node: TALJSONNodeA): Integer;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: AnsiString): Integer; overload;
    function Extract(const index: Integer): TALJSONNodeA; overload;
    function Extract(const Node: TALJSONNodeA): TALJSONNodeA; overload;
    procedure Exchange(Index1, Index2: Integer);
    function FindNode(const NodeName: AnsiString; const Direction: TDirection = TDirection.FromBeginning): TALJSONNodeA;
    function FindSibling(const Node: TALJSONNodeA; Delta: Integer): TALJSONNodeA;
    function First: TALJSONNodeA;
    function IndexOf(const Name: AnsiString; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOf(const Node: TALJSONNodeA; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: AnsiString; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Integer; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Int64; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function Last: TALJSONNodeA;
    function Remove(const Node: TALJSONNodeA): Integer;
    function ReplaceNode(const OldNode, NewNode: TALJSONNodeA): TALJSONNodeA;
    procedure Clear;
    procedure Insert(Index: Integer; const Node: TALJSONNodeA);
    property Count: Integer read fCount;
    property Nodes[const Name: AnsiString]: TALJSONNodeA read GetNodeByName; default;
    property Nodes[const Index: Integer]: TALJSONNodeA read GetNodeByIndex; default;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONNodeA = class(TObject)
  private
    FNodeName: AnsiString; // 8 Bytes
  protected
    function CreateChildList: TALJSONNodeListA;
    function InternalGetChildNodes: TALJSONNodeListA; virtual;
    function GetChildNodes: TALJSONNodeListA; virtual;
    function GetHasChildNodes: Boolean;
    function GetNodeType: TALJSONNodeType; virtual; abstract;
    function GetNodeSubType: TALJSONNodeSubType; virtual; abstract;
    procedure SetNodeName(const NodeName: AnsiString);
    function GetInterchangeValue(const SkipNodeSubTypeHelper: Boolean = False): AnsiString; virtual;
    function GetJSON: AnsiString;
    procedure SetJSON(const Value: AnsiString);
    function GetBSON: AnsiString;
    procedure SetBSON(const Value: AnsiString);
    procedure ParseJSON(
                const RawJSONStream: TStream;
                const RawJSONString: AnsiString;
                const SaxMode: Boolean;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions);
    procedure ParseBSON(
                const RawBSONStream: TStream;
                const RawBSONBytes: TBytes;
                const SaxMode: Boolean;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions);
    procedure SaveToBSON(
                const Stream: TStream;
                var Buffer: AnsiString;
                const Options: TALJSONSaveOptions);
    procedure SaveToJSON(
                const Stream: TStream;
                var Buffer: AnsiString;
                const Options: TALJSONSaveOptions);
  public
    constructor Create(const NodeName: AnsiString); virtual;
    function GetText(const Default: AnsiString): AnsiString; overload; virtual;
    function GetText: AnsiString; overload; virtual;
    procedure SetText(const Value: AnsiString); virtual;
    function GetFloat(const Default: Double): Double; overload; virtual;
    function GetFloat: Double; overload; virtual;
    procedure SetFloat(const Value: Double); virtual;
    function GetDateTime(const Default: TDateTime): TDateTime; overload; virtual;
    function GetDateTime: TDateTime; overload; virtual;
    procedure SetDateTime(const Value: TDateTime); virtual;
    function GetTimestamp(const Default: TALBSONTimestamp): TALBSONTimestamp; overload; virtual;
    function GetTimestamp: TALBSONTimestamp; overload; virtual;
    procedure SetTimestamp(const Value: TALBSONTimestamp); virtual;
    function GetObjectID(const Default: TBytes): TBytes; overload; virtual;
    function GetObjectID: TBytes; overload; virtual;
    procedure SetObjectID(const Value: TBytes); virtual;
    function GetInt32(const Default: Integer): Integer; overload; virtual;
    function GetInt32: Integer; overload; virtual;
    procedure SetInt32(const Value: Integer); virtual;
    function GetInt64(const Default: Int64): Int64; overload; virtual;
    function GetInt64: Int64; overload; virtual;
    procedure SetInt64(const Value: Int64); virtual;
    function GetBool(const Default: Boolean): Boolean; overload; virtual;
    function GetBool: Boolean; overload; virtual;
    procedure SetBool(const Value: Boolean); virtual;
    function GetNull: Boolean; virtual;
    procedure SetNull(const Value: Boolean); virtual;
    function GetJavascript(const Default: AnsiString): AnsiString; overload; virtual;
    function GetJavascript: AnsiString; overload; virtual;
    procedure SetJavascript(const Value: AnsiString); virtual;
    function GetRegEx(const Default: AnsiString): AnsiString; overload; virtual;
    function GetRegEx: AnsiString; overload; virtual;
    procedure SetRegEx(const Pattern: AnsiString); virtual;
    function GetRegExOptions(const Default: TALPerlRegExOptions): TALPerlRegExOptions; overload; virtual;
    function GetRegExOptions: TALPerlRegExOptions; overload; virtual;
    procedure SetRegExOptions(const Value: TALPerlRegExOptions); virtual;
    function GetBinaryAsBytes(const Default: TBytes): TBytes; overload; virtual;
    function GetBinaryAsBytes: TBytes; overload; virtual;
    function GetBinaryAsStream(const Default: TStream): TStream; overload; virtual;
    function GetBinaryAsStream: TStream; overload; virtual;
    procedure SetBinaryAsBytes(const Data: TBytes); overload; virtual;
    procedure SetBinaryAsStream(const Data: TStream); overload; virtual;
    function GetOwnsBinaryStream: Boolean; virtual;
    procedure SetOwnsBinaryStream(const Value: Boolean); virtual;
    function GetBinarySubType(const Default: Byte): Byte; overload; virtual;
    function GetBinarySubType: Byte; overload; virtual;
    procedure SetBinarySubType(const Subtype: Byte); virtual;
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA; overload;
    function AddChild(const Path: array of AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA; overload;
    function DeleteChild(const NodeName: AnsiString): Boolean; overload;
    function DeleteChild(const Path: array of AnsiString): Boolean; overload;
    function CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNodeA;
    function Clone: TALJSONNodeA;
    procedure SaveToJSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
    procedure SaveToJSONFile(const FileName: String; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONFile(const FileName: AnsiString; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONString(var Str: AnsiString; const Options: TALJSONSaveOptions = []);
    procedure SaveToBSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
    procedure SaveToBSONFile(const FileName: String; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToBSONFile(const FileName: AnsiString; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToBSONString(var Str: AnsiString; const Options: TALJSONSaveOptions = []);
    procedure LoadFromJSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]); overload;
    procedure LoadFromJSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]); overload;
    procedure LoadFromBSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONBytes(const Bytes: TBytes; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]); overload;
    procedure LoadFromBSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]); overload;
    procedure ParseJSONString(
                const Str: AnsiString;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []);
    procedure ParseJSONStream(
                const Stream: TStream;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []);
    procedure ParseJSONFile(
                const FileName: String;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []); overload;
    procedure ParseJSONFile(
                const FileName: AnsiString;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []); overload;
    procedure ParseBSONString(
                const Str: AnsiString;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONBytes(
                const Bytes: TBytes;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONStream(
                const Stream: TStream;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONFile(
                const FileName: String;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []); overload;
    procedure ParseBSONFile(
                const FileName: AnsiString;
                const OnParseText: TALJSONParseTextEventA;
                const OnParseStartObject: TALJSONParseObjectEventA;
                const OnParseEndObject: TALJSONParseObjectEventA;
                const OnParseStartArray: TALJSONParseArrayEventA;
                const OnParseEndArray: TALJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []); overload;
    property ChildNodes: TALJSONNodeListA read GetChildNodes;
    function GetChildNode(const NodeName: AnsiString): TALJSONNodeA; overload;
    function GetChildValueText(const NodeName: AnsiString; const Default: AnsiString): AnsiString; overload;
    function GetChildValueFloat(const NodeName: AnsiString; const Default: Double): Double; overload;
    function GetChildValueDateTime(const NodeName: AnsiString; const Default: TDateTime): TDateTime; overload;
    function GetChildValueTimestamp(const NodeName: AnsiString; const Default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetChildValueObjectID(const NodeName: AnsiString; const Default: TBytes): TBytes; overload;
    function GetChildValueInt32(const NodeName: AnsiString; const Default: Integer): Integer; overload;
    function GetChildValueInt64(const NodeName: AnsiString; const Default: Int64): Int64; overload;
    function GetChildValueBool(const NodeName: AnsiString; const Default: Boolean): Boolean; overload;
    function GetChildValueJavascript(const NodeName: AnsiString; const Default: AnsiString): AnsiString; overload;
    function GetChildValueRegEx(const NodeName: AnsiString; const Default: AnsiString): AnsiString; overload;
    function GetChildValueRegExOptions(const NodeName: AnsiString; const Default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetChildValueBinaryAsBytes(const NodeName: AnsiString; const Default: TBytes): TBytes; overload;
    function GetChildValueBinaryAsStream(const NodeName: AnsiString; const Default: TStream): TStream; overload;
    function GetChildValueBinarySubType(const NodeName: AnsiString; const Default: Byte): Byte; overload;
    function GetChildValueNull(const NodeName: AnsiString): Boolean; overload;
    function GetChildNode(const Path: array of AnsiString): TALJSONNodeA; overload;
    function GetChildValueText(const Path: array of AnsiString; const Default: AnsiString): AnsiString; overload;
    function GetChildValueFloat(const Path: array of AnsiString; const Default: Double): Double; overload;
    function GetChildValueDateTime(const Path: array of AnsiString; const Default: TDateTime): TDateTime; overload;
    function GetChildValueTimestamp(const Path: array of AnsiString; const Default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetChildValueObjectID(const Path: array of AnsiString; const Default: TBytes): TBytes; overload;
    function GetChildValueInt32(const Path: array of AnsiString; const Default: Integer): Integer; overload;
    function GetChildValueInt64(const Path: array of AnsiString; const Default: Int64): Int64; overload;
    function GetChildValueBool(const Path: array of AnsiString; const Default: Boolean): Boolean; overload;
    function GetChildValueJavascript(const Path: array of AnsiString; const Default: AnsiString): AnsiString; overload;
    function GetChildValueRegEx(const Path: array of AnsiString; const Default: AnsiString): AnsiString; overload;
    function GetChildValueRegExOptions(const Path: array of AnsiString; const Default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetChildValueBinaryAsBytes(const Path: array of AnsiString; const Default: TBytes): TBytes; overload;
    function GetChildValueBinaryAsStream(const Path: array of AnsiString; const Default: TStream): TStream; overload;
    function GetChildValueBinarySubType(const Path: array of AnsiString; const Default: Byte): Byte; overload;
    function GetChildValueNull(const Path: array of AnsiString): Boolean; overload;
    procedure SetChildValueText(const NodeName: AnsiString; const Value: AnsiString); overload;
    procedure SetChildValueFloat(const NodeName: AnsiString; const Value: Double); overload;
    procedure SetChildValueDateTime(const NodeName: AnsiString; const Value: TDateTime); overload;
    procedure SetChildValueTimestamp(const NodeName: AnsiString; const Value: TALBSONTimestamp); overload;
    procedure SetChildValueObjectID(const NodeName: AnsiString; const Value: TBytes); overload;
    procedure SetChildValueInt32(const NodeName: AnsiString; const Value: Integer); overload;
    procedure SetChildValueInt64(const NodeName: AnsiString; const Value: Int64); overload;
    procedure SetChildValueBool(const NodeName: AnsiString; const Value: Boolean); overload;
    procedure SetChildValueJavascript(const NodeName: AnsiString; const Value: AnsiString); overload;
    procedure SetChildValueRegEx(const NodeName: AnsiString; const Value: AnsiString); overload;
    procedure SetChildValueRegExOptions(const NodeName: AnsiString; const Value: TALPerlRegExOptions); overload;
    procedure SetChildValueBinaryAsBytes(const NodeName: AnsiString; const Value: TBytes); overload;
    procedure SetChildValueBinaryAsStream(const NodeName: AnsiString; const Value: TStream); overload;
    procedure SetChildValueBinarySubType(const NodeName: AnsiString; const Value: Byte); overload;
    procedure SetChildValueNull(const NodeName: AnsiString); overload;
    procedure SetChildValueText(const Path: array of AnsiString; const Value: AnsiString); overload;
    procedure SetChildValueFloat(const Path: array of AnsiString; const Value: Double); overload;
    procedure SetChildValueDateTime(const Path: array of AnsiString; const Value: TDateTime); overload;
    procedure SetChildValueTimestamp(const Path: array of AnsiString; const Value: TALBSONTimestamp); overload;
    procedure SetChildValueObjectID(const Path: array of AnsiString; const Value: TBytes); overload;
    procedure SetChildValueInt32(const Path: array of AnsiString; const Value: Integer); overload;
    procedure SetChildValueInt64(const Path: array of AnsiString; const Value: Int64); overload;
    procedure SetChildValueBool(const Path: array of AnsiString; const Value: Boolean); overload;
    procedure SetChildValueJavascript(const Path: array of AnsiString; const Value: AnsiString); overload;
    procedure SetChildValueRegEx(const Path: array of AnsiString; const Value: AnsiString); overload;
    procedure SetChildValueRegExOptions(const Path: array of AnsiString; const Value: TALPerlRegExOptions); overload;
    procedure SetChildValueBinaryAsBytes(const Path: array of AnsiString; const Value: TBytes); overload;
    procedure SetChildValueBinaryAsStream(const Path: array of AnsiString; const Value: TStream); overload;
    procedure SetChildValueBinarySubType(const Path: array of AnsiString; const Value: Byte); overload;
    procedure SetChildValueNull(const Path: array of AnsiString); overload;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property NodeName: AnsiString read FNodeName write SetNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property NodeSubType: TALJSONNodeSubType read GetNodeSubType;
    property Text: AnsiString read GetText write SetText;
    property Int32: Integer read GetInt32 write SetInt32;
    property Int64: Int64 read GetInt64 write SetInt64;
    property Float: Double read GetFloat write SetFloat;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Timestamp: TALBSONTimestamp read GetTimestamp write SetTimestamp; // Used only by MongoDB. Do not use directly; use DateTime instead.
    property ObjectID: TBytes read GetObjectID write SetObjectID;
    property Bool: Boolean read GetBool write SetBool;
    property Null: Boolean read GetNull write SetNull;
    property Javascript: AnsiString read GetJavascript write SetJavascript;
    property RegEx: AnsiString read GetRegEx write SetRegEx;
    property RegExOptions: TALPerlRegExOptions read GetRegExOptions write SetRegExOptions;
    property BinaryAsBytes: TBytes read GetBinaryAsBytes write SetBinaryAsBytes;
    property BinaryAsStream: TStream read GetBinaryAsStream write SetBinaryAsStream;
    property OwnsBinaryStream: Boolean read GetOwnsBinaryStream write SetOwnsBinaryStream;
    property BinarySubType: Byte read GetBinarySubType write SetBinarySubType;
    property JSON: AnsiString read GetJSON write SetJSON;
    property BSON: AnsiString read GetBSON write SetBSON;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONObjectNodeA = class(TALJSONNodeA)
  private
    FChildNodes: TALJSONNodeListA; // 8 Bytes
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListA; override;
    function GetChildNodes: TALJSONNodeListA; override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    destructor Destroy; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONArrayNodeA = class(TALJSONNodeA)
  private
    FChildNodes: TALJSONNodeListA; // 8 Bytes
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListA; override;
    function GetChildNodes: TALJSONNodeListA; override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    destructor Destroy; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONTextNodeA = class(TALJSONNodeA)
  private
    FNodeValue: Int64; // 8 Bytes
    FNodeSubType: TALJSONNodeSubType; // 1 Byte
    FStorageKind: TALJSONStorageKind; // 1 Byte
    FBinarySubType: Byte; // 1 Byte
    FRegExOptions: TALPerlRegExOptions; // 1 Byte
    procedure ClearNodeValue;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function GetInterchangeValue(const SkipNodeSubTypeHelper: Boolean = False): AnsiString; override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    destructor Destroy; override;
    function GetText(const Default: AnsiString): AnsiString; overload; override;
    function GetText: AnsiString; overload; override;
    procedure SetText(const Value: AnsiString); override;
    function GetFloat(const Default: Double): Double; overload; override;
    function GetFloat: Double; overload; override;
    procedure SetFloat(const Value: Double); override;
    function GetDateTime(const Default: TDateTime): TDateTime; overload; override;
    function GetDateTime: TDateTime; overload; override;
    procedure SetDateTime(const Value: TDateTime); override;
    function GetTimestamp(const Default: TALBSONTimestamp): TALBSONTimestamp; overload; override;
    function GetTimestamp: TALBSONTimestamp; overload; override;
    procedure SetTimestamp(const Value: TALBSONTimestamp); override;
    function GetObjectID(const Default: TBytes): TBytes; overload; override;
    function GetObjectID: TBytes; overload; override;
    procedure SetObjectID(const Value: TBytes); override;
    function GetInt32(const Default: Integer): Integer; overload; override;
    function GetInt32: Integer; overload; override;
    procedure SetInt32(const Value: Integer); override;
    function GetInt64(const Default: Int64): Int64; overload; override;
    function GetInt64: Int64; overload; override;
    procedure SetInt64(const Value: Int64); override;
    function GetBool(const Default: Boolean): Boolean; overload; override;
    function GetBool: Boolean; overload; override;
    procedure SetBool(const Value: Boolean); override;
    function GetNull: Boolean; override;
    procedure SetNull(const Value: Boolean); override;
    function GetJavascript(const Default: AnsiString): AnsiString; overload; override;
    function GetJavascript: AnsiString; overload; override;
    procedure SetJavascript(const Value: AnsiString); override;
    function GetRegEx(const Default: AnsiString): AnsiString; overload; override;
    function GetRegEx: AnsiString; overload; override;
    procedure SetRegEx(const Pattern: AnsiString); override;
    function GetRegExOptions(const Default: TALPerlRegExOptions): TALPerlRegExOptions; overload; override;
    function GetRegExOptions: TALPerlRegExOptions; overload; override;
    procedure SetRegExOptions(const Value: TALPerlRegExOptions); override;
    function GetBinaryAsBytes(const Default: TBytes): TBytes; overload; override;
    function GetBinaryAsBytes: TBytes; overload; override;
    function GetBinaryAsStream(const Default: TStream): TStream; overload; override;
    function GetBinaryAsStream: TStream; overload; override;
    procedure SetBinaryAsBytes(const Data: TBytes); overload; override;
    procedure SetBinaryAsStream(const Data: TStream); overload; override;
    function GetOwnsBinaryStream: Boolean; override;
    procedure SetOwnsBinaryStream(const Value: Boolean); override;
    function GetBinarySubType(const Default: Byte): Byte; overload; override;
    function GetBinarySubType: Byte; overload; override;
    procedure SetBinarySubType(const Subtype: Byte); override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONDocumentA = class(TObject)
  private
    class function DetectNodeTypeFromJSON(
                     const RawJSONStream: TStream;
                     const RawJSONString: AnsiString): TALJSONNodeType;
  public
    class function Create: TALJSONNodeA;
    class function CreateFromJSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
    class function CreateFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
    class function CreateFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA; overload;
    class function CreateFromJSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA; overload;
    class function CreateFromBSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
    class function CreateFromBSONBytes(const Bytes: TBytes; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
    class function CreateFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
    class function CreateFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA; overload;
    class function CreateFromBSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA; overload;
    class procedure ParseJSONString(
                      const Str: AnsiString;
                      const OnParseText: TALJSONParseTextEventA;
                      const OnParseStartObject: TALJSONParseObjectEventA;
                      const OnParseEndObject: TALJSONParseObjectEventA;
                      const OnParseStartArray: TALJSONParseArrayEventA;
                      const OnParseEndArray: TALJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseJSONStream(
                      const Stream: TStream;
                      const OnParseText: TALJSONParseTextEventA;
                      const OnParseStartObject: TALJSONParseObjectEventA;
                      const OnParseEndObject: TALJSONParseObjectEventA;
                      const OnParseStartArray: TALJSONParseArrayEventA;
                      const OnParseEndArray: TALJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseJSONFile(
                      const FileName: String;
                      const OnParseText: TALJSONParseTextEventA;
                      const OnParseStartObject: TALJSONParseObjectEventA;
                      const OnParseEndObject: TALJSONParseObjectEventA;
                      const OnParseStartArray: TALJSONParseArrayEventA;
                      const OnParseEndArray: TALJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []); overload;
    class procedure ParseJSONFile(
                      const FileName: AnsiString;
                      const OnParseText: TALJSONParseTextEventA;
                      const OnParseStartObject: TALJSONParseObjectEventA;
                      const OnParseEndObject: TALJSONParseObjectEventA;
                      const OnParseStartArray: TALJSONParseArrayEventA;
                      const OnParseEndArray: TALJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []); overload;
    class procedure ParseBSONString(
                      const Str: AnsiString;
                      const OnParseText: TALJSONParseTextEventA;
                      const OnParseStartObject: TALJSONParseObjectEventA;
                      const OnParseEndObject: TALJSONParseObjectEventA;
                      const OnParseStartArray: TALJSONParseArrayEventA;
                      const OnParseEndArray: TALJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONStream(
                      const Stream: TStream;
                      const OnParseText: TALJSONParseTextEventA;
                      const OnParseStartObject: TALJSONParseObjectEventA;
                      const OnParseEndObject: TALJSONParseObjectEventA;
                      const OnParseStartArray: TALJSONParseArrayEventA;
                      const OnParseEndArray: TALJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONFile(
                      const FileName: String;
                      const OnParseText: TALJSONParseTextEventA;
                      const OnParseStartObject: TALJSONParseObjectEventA;
                      const OnParseEndObject: TALJSONParseObjectEventA;
                      const OnParseStartArray: TALJSONParseArrayEventA;
                      const OnParseEndArray: TALJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []); overload;
    class procedure ParseBSONFile(
                      const FileName: AnsiString;
                      const OnParseText: TALJSONParseTextEventA;
                      const OnParseStartObject: TALJSONParseObjectEventA;
                      const OnParseEndObject: TALJSONParseObjectEventA;
                      const OnParseStartArray: TALJSONParseArrayEventA;
                      const OnParseEndArray: TALJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []); overload;
  end;

var
  ALDefaultJsonNodeIndentA: AnsiString;  // var instead of const to avoid new ansitring on assign
  ALDefaultJsonPathSeparatorA: AnsiChar;
  ALJsonISODateFormatSettingsA: TALFormatSettingsA;

procedure ALJSONToTStringsA(
            const AJsonStr: AnsiString;
            const AFormatSettings: TALFormatSettingsA;
            const APath: AnsiString;
            const ALst: TALStringsA;
            const ANullStr: AnsiString = 'null';
            const ATrueStr: AnsiString = 'true';
            const AFalseStr: AnsiString = 'false'); overload;
procedure ALJSONToTStringsA(
            const AJsonStr: AnsiString;
            const AFormatSettings: TALFormatSettingsA;
            const ALst: TALStringsA;
            const ANullStr: AnsiString = 'null';
            const ATrueStr: AnsiString = 'true';
            const AFalseStr: AnsiString = 'false'); overload;
procedure ALJSONToTStringsA(
            const AJsonNode: TALJSONNodeA;
            const APath: AnsiString;
            const ALst: TALStringsA;
            const ANullStr: AnsiString = 'null';
            const ATrueStr: AnsiString = 'true';
            const AFalseStr: AnsiString = 'false'); overload;
procedure ALJSONToTStringsA(
            const AJsonNode: TALJSONNodeA;
            const ALst: TALStringsA;
            const ANullStr: AnsiString = 'null';
            const ATrueStr: AnsiString = 'true';
            const AFalseStr: AnsiString = 'false'); overload;
procedure ALTStringsToJsonA(
            const ALst: TALStringsA;
            const AJsonNode: TALJSONNodeA;
            const APath: AnsiString = '';
            const ANameToLowerCase: Boolean = false;
            const ANullStr: AnsiString = 'null');

function ALJsonEncodeFloatWithNodeSubTypeHelperA(const AValue: double): AnsiString;
function ALJsonEncodeTextWithNodeSubTypeHelperA(const AValue: AnsiString): AnsiString;
function ALJsonEncodeBinaryWithNodeSubTypeHelperA(const AValue: TBytes): AnsiString;
function ALJsonEncodeObjectIDWithNodeSubTypeHelperA(const AValue: TBytes): AnsiString;
function ALJsonEncodeBooleanWithNodeSubTypeHelperA(const AValue: Boolean): AnsiString;
function ALJsonEncodeDateTimeA(const AValue: TDateTime): AnsiString;
function ALJsonEncodeDateTimeWithNodeSubTypeHelperA(const AValue: TDateTime): AnsiString;
function ALJsonEncodeJavascriptWithNodeSubTypeHelperA(const AValue: AnsiString): AnsiString;
function ALJsonEncodeInt64WithNodeSubTypeHelperA(const AValue: Int64): AnsiString;
function ALJsonEncodeInt32WithNodeSubTypeHelperA(const AValue: Int32): AnsiString;
function ALJsonEncodeNullWithNodeSubTypeHelperA: AnsiString;

function ALJSONTryStrToRegExA(const S: AnsiString; out RegEx: AnsiString; out RegExOptions: TALPerlRegExOptions): Boolean;
function ALJSONTryStrToBinaryA(const S: AnsiString; out Data: TBytes; out Subtype: Byte): Boolean;
function ALJSONTryStrToDateTimeA(const S: AnsiString; out Value: TDateTime): Boolean;
function ALJSONTryStrToObjectIDA(const S: AnsiString; out Value: TBytes): Boolean;
function ALJSONTryStrToTimestampA(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
function ALJSONTryStrToInt32A(const S: AnsiString; out Value: Integer): Boolean;
function ALJSONTryStrToInt64A(const S: AnsiString; out Value: Int64): Boolean;

function ALFindJsonNodeByChildValueInt32A(
           const AJsonNode: TALJSONNodeA;
           const AChildName: AnsiString;
           const AChildValue : Int32;
           const ARecurse: Boolean = False): TALJSONNodeA;
function ALFindJsonNodeByChildValueInt64A(
           const AJsonNode: TALJSONNodeA;
           const AChildName: AnsiString;
           const AChildValue : Int64;
           const ARecurse: Boolean = False): TALJSONNodeA;
function ALFindJsonNodeByChildValueTextA(
           const AJsonNode: TALJSONNodeA;
           const AChildName: AnsiString;
           const AChildValue : AnsiString;
           const ARecurse: Boolean = False): TALJSONNodeA;

type

  TALJSONNodeW = class;
  TALJSONNodeListW = class;
  TALJSONDocumentW = class;

  TALJSONParseTextEventW = reference to procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType);
  TALJSONParseObjectEventW = reference to procedure (Sender: TObject; const Path: String; const Name: String);
  TALJSONParseArrayEventW = reference to procedure (Sender: TObject; const Path: String; const Name: String);
  TALJSONNodeListSortCompareW = reference to function(List: TALJSONNodeListW; Index1, Index2: Integer): Integer;
  TALJSONPointerListW = array of TALJSONNodeW;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONNodeListW = class(TObject)
  Private
    FCapacity: Integer; // 4 Bytes
    FCount: Integer; // 4 Bytes
    FList: TALJSONPointerListW; // 8 Bytes
    procedure QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareW);
  protected
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function Get(Index: Integer): TALJSONNodeW;
    function GetNodeByIndex(const Index: Integer): TALJSONNodeW;
    function GetNodeByName(const Name: String): TALJSONNodeW;
    function CompareNodeNames(const S1, S2: String): Integer; inline;
    function Find(const NodeName: String; var Index: Integer): Boolean;
    procedure InternalInsert(Index: Integer; const Node: TALJSONNodeW);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Sort;
    procedure CustomSort(Compare: TALJSONNodeListSortCompareW);
    function Add(const Node: TALJSONNodeW): Integer;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: String): Integer; overload;
    function Extract(const index: Integer): TALJSONNodeW; overload;
    function Extract(const Node: TALJSONNodeW): TALJSONNodeW; overload;
    procedure Exchange(Index1, Index2: Integer);
    function FindNode(const NodeName: String; const Direction: TDirection = TDirection.FromBeginning): TALJSONNodeW;
    function FindSibling(const Node: TALJSONNodeW; Delta: Integer): TALJSONNodeW;
    function First: TALJSONNodeW;
    function IndexOf(const Name: String; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOf(const Node: TALJSONNodeW; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: String; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Integer; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Int64; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function Last: TALJSONNodeW;
    function Remove(const Node: TALJSONNodeW): Integer;
    function ReplaceNode(const OldNode, NewNode: TALJSONNodeW): TALJSONNodeW;
    procedure Clear;
    procedure Insert(Index: Integer; const Node: TALJSONNodeW);
    property Count: Integer read fCount;
    property Nodes[const Name: String]: TALJSONNodeW read GetNodeByName; default;
    property Nodes[const Index: Integer]: TALJSONNodeW read GetNodeByIndex; default;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONNodeW = class(TObject)
  private
    FNodeName: String; // 8 Bytes
  protected
    function CreateChildList: TALJSONNodeListW;
    function InternalGetChildNodes: TALJSONNodeListW; virtual;
    function GetChildNodes: TALJSONNodeListW; virtual;
    function GetHasChildNodes: Boolean;
    function GetNodeType: TALJSONNodeType; virtual; abstract;
    function GetNodeSubType: TALJSONNodeSubType; virtual; abstract;
    procedure SetNodeName(const NodeName: String);
    function GetInterchangeValue(const SkipNodeSubTypeHelper: Boolean = False): String; virtual;
    function GetJSON: String;
    procedure SetJSON(const Value: String);
    function GetBSON: TBytes;
    procedure SetBSON(const Value: TBytes);
    procedure ParseJSON(
                const Buffer: String;
                const SaxMode: Boolean;
                const OnParseText: TALJSONParseTextEventW;
                const OnParseStartObject: TALJSONParseObjectEventW;
                const OnParseEndObject: TALJSONParseObjectEventW;
                const OnParseStartArray: TALJSONParseArrayEventW;
                const OnParseEndArray: TALJSONParseArrayEventW;
                const Options: TALJSONParseOptions);
    procedure ParseBSON(
                const RawBSONStream: TStream;
                const RawBSONBytes: TBytes;
                const SaxMode: Boolean;
                const OnParseText: TALJSONParseTextEventW;
                const OnParseStartObject: TALJSONParseObjectEventW;
                const OnParseEndObject: TALJSONParseObjectEventW;
                const OnParseStartArray: TALJSONParseArrayEventW;
                const OnParseEndArray: TALJSONParseArrayEventW;
                const Options: TALJSONParseOptions);
    procedure SaveToBSON(
                const Stream: TStream;
                var Buffer: TBytes;
                const Options: TALJSONSaveOptions);
    procedure SaveToJSON(
                const Stream: TStream;
                const StreamEncoding: TEncoding;
                var Buffer: String;
                const Options: TALJSONSaveOptions);
  public
    constructor Create(const NodeName: String); virtual;
    function GetText(const Default: String): String; overload; virtual;
    function GetText: String; overload; virtual;
    procedure SetText(const Value: String); virtual;
    function GetFloat(const Default: Double): Double; overload; virtual;
    function GetFloat: Double; overload; virtual;
    procedure SetFloat(const Value: Double); virtual;
    function GetDateTime(const Default: TDateTime): TDateTime; overload; virtual;
    function GetDateTime: TDateTime; overload; virtual;
    procedure SetDateTime(const Value: TDateTime); virtual;
    function GetTimestamp(const Default: TALBSONTimestamp): TALBSONTimestamp; overload; virtual;
    function GetTimestamp: TALBSONTimestamp; overload; virtual;
    procedure SetTimestamp(const Value: TALBSONTimestamp); virtual;
    function GetObjectID(const Default: TBytes): TBytes; overload; virtual;
    function GetObjectID: TBytes; overload; virtual;
    procedure SetObjectID(const Value: TBytes); virtual;
    function GetInt32(const Default: Integer): Integer; overload; virtual;
    function GetInt32: Integer; overload; virtual;
    procedure SetInt32(const Value: Integer); virtual;
    function GetInt64(const Default: Int64): Int64; overload; virtual;
    function GetInt64: Int64; overload; virtual;
    procedure SetInt64(const Value: Int64); virtual;
    function GetBool(const Default: Boolean): Boolean; overload; virtual;
    function GetBool: Boolean; overload; virtual;
    procedure SetBool(const Value: Boolean); virtual;
    function GetNull: Boolean; virtual;
    procedure SetNull(const Value: Boolean); virtual;
    function GetJavascript(const Default: String): String; overload; virtual;
    function GetJavascript: String; overload; virtual;
    procedure SetJavascript(const Value: String); virtual;
    function GetRegEx(const Default: String): String; overload; virtual;
    function GetRegEx: String; overload; virtual;
    procedure SetRegEx(const Pattern: String); virtual;
    function GetRegExOptions(const Default: TALPerlRegExOptions): TALPerlRegExOptions; overload; virtual;
    function GetRegExOptions: TALPerlRegExOptions; overload; virtual;
    procedure SetRegExOptions(const Value: TALPerlRegExOptions); virtual;
    function GetBinaryAsBytes(const Default: TBytes): TBytes; overload; virtual;
    function GetBinaryAsBytes: TBytes; overload; virtual;
    function GetBinaryAsStream(const Default: TStream): TStream; overload; virtual;
    function GetBinaryAsStream: TStream; overload; virtual;
    procedure SetBinaryAsBytes(const Data: TBytes); overload; virtual;
    procedure SetBinaryAsStream(const Data: TStream); overload; virtual;
    function GetOwnsBinaryStream: Boolean; virtual;
    procedure SetOwnsBinaryStream(const Value: Boolean); virtual;
    function GetBinarySubType(const Default: Byte): Byte; overload; virtual;
    function GetBinarySubType: Byte; overload; virtual;
    procedure SetBinarySubType(const Subtype: Byte); virtual;
    function AddChild(const NodeName: String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW; overload;
    function AddChild(const Path: array of String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW; overload;
    function DeleteChild(const NodeName: String): Boolean; overload;
    function DeleteChild(const Path: array of String): Boolean; overload;
    function CreateNode(const NodeName: String; NodeType: TALJSONNodeType): TALJSONNodeW;
    function Clone: TALJSONNodeW;
    procedure SaveToJSONStream(const Stream: TStream; const Encoding: TEncoding; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONFile(const FileName: String; const Encoding: TEncoding; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONFile(const FileName: String; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONString(var Str: String; const Options: TALJSONSaveOptions = []);
    procedure SaveToBSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
    procedure SaveToBSONFile(const FileName: String; const Options: TALJSONSaveOptions = []);
    procedure SaveToBSONBytes(var Bytes: TBytes; const Options: TALJSONSaveOptions = []);
    procedure LoadFromJSONString(const Str: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONBytes(const Bytes: TBytes; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure ParseJSONString(
                const Str: String;
                const OnParseText: TALJSONParseTextEventW;
                const OnParseStartObject: TALJSONParseObjectEventW;
                const OnParseEndObject: TALJSONParseObjectEventW;
                const OnParseStartArray: TALJSONParseArrayEventW;
                const OnParseEndArray: TALJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseJSONStream(
                const Stream: TStream;
                const OnParseText: TALJSONParseTextEventW;
                const OnParseStartObject: TALJSONParseObjectEventW;
                const OnParseEndObject: TALJSONParseObjectEventW;
                const OnParseStartArray: TALJSONParseArrayEventW;
                const OnParseEndArray: TALJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseJSONFile(
                const FileName: String;
                const OnParseText: TALJSONParseTextEventW;
                const OnParseStartObject: TALJSONParseObjectEventW;
                const OnParseEndObject: TALJSONParseObjectEventW;
                const OnParseStartArray: TALJSONParseArrayEventW;
                const OnParseEndArray: TALJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONBytes(
                const Bytes: TBytes;
                const OnParseText: TALJSONParseTextEventW;
                const OnParseStartObject: TALJSONParseObjectEventW;
                const OnParseEndObject: TALJSONParseObjectEventW;
                const OnParseStartArray: TALJSONParseArrayEventW;
                const OnParseEndArray: TALJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONStream(
                const Stream: TStream;
                const OnParseText: TALJSONParseTextEventW;
                const OnParseStartObject: TALJSONParseObjectEventW;
                const OnParseEndObject: TALJSONParseObjectEventW;
                const OnParseStartArray: TALJSONParseArrayEventW;
                const OnParseEndArray: TALJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONFile(
                const FileName: String;
                const OnParseText: TALJSONParseTextEventW;
                const OnParseStartObject: TALJSONParseObjectEventW;
                const OnParseEndObject: TALJSONParseObjectEventW;
                const OnParseStartArray: TALJSONParseArrayEventW;
                const OnParseEndArray: TALJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    property ChildNodes: TALJSONNodeListW read GetChildNodes;
    function GetChildNode(const NodeName: String): TALJSONNodeW; overload;
    function GetChildValueText(const NodeName: String; const Default: String): String; overload;
    function GetChildValueFloat(const NodeName: String; const Default: Double): Double; overload;
    function GetChildValueDateTime(const NodeName: String; const Default: TDateTime): TDateTime; overload;
    function GetChildValueTimestamp(const NodeName: String; const Default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetChildValueObjectID(const NodeName: String; const Default: TBytes): TBytes; overload;
    function GetChildValueInt32(const NodeName: String; const Default: Integer): Integer; overload;
    function GetChildValueInt64(const NodeName: String; const Default: Int64): Int64; overload;
    function GetChildValueBool(const NodeName: String; const Default: Boolean): Boolean; overload;
    function GetChildValueJavascript(const NodeName: String; const Default: String): String; overload;
    function GetChildValueRegEx(const NodeName: String; const Default: String): String; overload;
    function GetChildValueRegExOptions(const NodeName: String; const Default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetChildValueBinaryAsBytes(const NodeName: String; const Default: TBytes): TBytes; overload;
    function GetChildValueBinaryAsStream(const NodeName: String; const Default: TStream): TStream; overload;
    function GetChildValueBinarySubType(const NodeName: String; const Default: Byte): Byte; overload;
    function GetChildValueNull(const NodeName: String): Boolean; overload;
    function GetChildNode(const Path: array of String): TALJSONNodeW; overload;
    function GetChildValueText(const Path: array of String; const Default: String): String; overload;
    function GetChildValueFloat(const Path: array of String; const Default: Double): Double; overload;
    function GetChildValueDateTime(const Path: array of String; const Default: TDateTime): TDateTime; overload;
    function GetChildValueTimestamp(const Path: array of String; const Default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetChildValueObjectID(const Path: array of String; const Default: TBytes): TBytes; overload;
    function GetChildValueInt32(const Path: array of String; const Default: Integer): Integer; overload;
    function GetChildValueInt64(const Path: array of String; const Default: Int64): Int64; overload;
    function GetChildValueBool(const Path: array of String; const Default: Boolean): Boolean; overload;
    function GetChildValueJavascript(const Path: array of String; const Default: String): String; overload;
    function GetChildValueRegEx(const Path: array of String; const Default: String): String; overload;
    function GetChildValueRegExOptions(const Path: array of String; const Default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetChildValueBinaryAsBytes(const Path: array of String; const Default: TBytes): TBytes; overload;
    function GetChildValueBinaryAsStream(const Path: array of String; const Default: TStream): TStream; overload;
    function GetChildValueBinarySubType(const Path: array of String; const Default: Byte): Byte; overload;
    function GetChildValueNull(const Path: array of String): Boolean; overload;
    procedure SetChildValueText(const NodeName: String; const Value: String); overload;
    procedure SetChildValueFloat(const NodeName: String; const Value: Double); overload;
    procedure SetChildValueDateTime(const NodeName: String; const Value: TDateTime); overload;
    procedure SetChildValueTimestamp(const NodeName: String; const Value: TALBSONTimestamp); overload;
    procedure SetChildValueObjectID(const NodeName: String; const Value: TBytes); overload;
    procedure SetChildValueInt32(const NodeName: String; const Value: Integer); overload;
    procedure SetChildValueInt64(const NodeName: String; const Value: Int64); overload;
    procedure SetChildValueBool(const NodeName: String; const Value: Boolean); overload;
    procedure SetChildValueJavascript(const NodeName: String; const Value: String); overload;
    procedure SetChildValueRegEx(const NodeName: String; const Value: String); overload;
    procedure SetChildValueRegExOptions(const NodeName: String; const Value: TALPerlRegExOptions); overload;
    procedure SetChildValueBinaryAsBytes(const NodeName: String; const Value: TBytes); overload;
    procedure SetChildValueBinaryAsStream(const NodeName: String; const Value: TStream); overload;
    procedure SetChildValueBinarySubType(const NodeName: String; const Value: Byte); overload;
    procedure SetChildValueNull(const NodeName: String); overload;
    procedure SetChildValueText(const Path: array of String; const Value: String); overload;
    procedure SetChildValueFloat(const Path: array of String; const Value: Double); overload;
    procedure SetChildValueDateTime(const Path: array of String; const Value: TDateTime); overload;
    procedure SetChildValueTimestamp(const Path: array of String; const Value: TALBSONTimestamp); overload;
    procedure SetChildValueObjectID(const Path: array of String; const Value: TBytes); overload;
    procedure SetChildValueInt32(const Path: array of String; const Value: Integer); overload;
    procedure SetChildValueInt64(const Path: array of String; const Value: Int64); overload;
    procedure SetChildValueBool(const Path: array of String; const Value: Boolean); overload;
    procedure SetChildValueJavascript(const Path: array of String; const Value: String); overload;
    procedure SetChildValueRegEx(const Path: array of String; const Value: String); overload;
    procedure SetChildValueRegExOptions(const Path: array of String; const Value: TALPerlRegExOptions); overload;
    procedure SetChildValueBinaryAsBytes(const Path: array of String; const Value: TBytes); overload;
    procedure SetChildValueBinaryAsStream(const Path: array of String; const Value: TStream); overload;
    procedure SetChildValueBinarySubType(const Path: array of String; const Value: Byte); overload;
    procedure SetChildValueNull(const Path: array of String); overload;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property NodeName: String read FNodeName write SetNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property NodeSubType: TALJSONNodeSubType read GetNodeSubType;
    property Text: String read GetText write SetText;
    property Int32: Integer read GetInt32 write SetInt32;
    property Int64: Int64 read GetInt64 write SetInt64;
    property Float: Double read GetFloat write SetFloat;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Timestamp: TALBSONTimestamp read GetTimestamp write SetTimestamp; // Used only by MongoDB. Do not use directly; use DateTime instead.
    property ObjectID: TBytes read GetObjectID write SetObjectID;
    property Bool: Boolean read GetBool write SetBool;
    property Null: Boolean read GetNull write SetNull;
    property Javascript: String read GetJavascript write SetJavascript;
    property RegEx: String read GetRegEx write SetRegEx;
    property RegExOptions: TALPerlRegExOptions read GetRegExOptions write SetRegExOptions;
    property BinaryAsBytes: TBytes read GetBinaryAsBytes write SetBinaryAsBytes;
    property BinaryAsStream: TStream read GetBinaryAsStream write SetBinaryAsStream;
    property OwnsBinaryStream: Boolean read GetOwnsBinaryStream write SetOwnsBinaryStream;
    property BinarySubType: Byte read GetBinarySubType write SetBinarySubType;
    property JSON: String read GetJSON write SetJSON;
    property BSON: TBytes read GetBSON write SetBSON;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONObjectNodeW = class(TALJSONNodeW)
  private
    FChildNodes: TALJSONNodeListW; // 8 Bytes
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListW; override;
    function GetChildNodes: TALJSONNodeListW; override;
  public
    constructor Create(const NodeName: String = ''); override;
    destructor Destroy; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONArrayNodeW = class(TALJSONNodeW)
  private
    FChildNodes: TALJSONNodeListW; // 8 Bytes
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListW; override;
    function GetChildNodes: TALJSONNodeListW; override;
  public
    constructor Create(const NodeName: String = ''); override;
    destructor Destroy; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONTextNodeW = class(TALJSONNodeW)
  private
    FNodeValue: Int64; // 8 Bytes
    FNodeSubType: TALJSONNodeSubType; // 1 Byte
    FStorageKind: TALJSONStorageKind; // 1 Byte
    FBinarySubType: Byte; // 1 Byte
    FRegExOptions: TALPerlRegExOptions; // 1 Byte
    procedure ClearNodeValue;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function GetInterchangeValue(const SkipNodeSubTypeHelper: Boolean = False): String; override;
  public
    constructor Create(const NodeName: String = ''); override;
    destructor Destroy; override;
    function GetText(const Default: String): String; overload; override;
    function GetText: String; overload; override;
    procedure SetText(const Value: String); override;
    function GetFloat(const Default: Double): Double; overload; override;
    function GetFloat: Double; overload; override;
    procedure SetFloat(const Value: Double); override;
    function GetDateTime(const Default: TDateTime): TDateTime; overload; override;
    function GetDateTime: TDateTime; overload; override;
    procedure SetDateTime(const Value: TDateTime); override;
    function GetTimestamp(const Default: TALBSONTimestamp): TALBSONTimestamp; overload; override;
    function GetTimestamp: TALBSONTimestamp; overload; override;
    procedure SetTimestamp(const Value: TALBSONTimestamp); override;
    function GetObjectID(const Default: TBytes): TBytes; overload; override;
    function GetObjectID: TBytes; overload; override;
    procedure SetObjectID(const Value: TBytes); override;
    function GetInt32(const Default: Integer): Integer; overload; override;
    function GetInt32: Integer; overload; override;
    procedure SetInt32(const Value: Integer); override;
    function GetInt64(const Default: Int64): Int64; overload; override;
    function GetInt64: Int64; overload; override;
    procedure SetInt64(const Value: Int64); override;
    function GetBool(const Default: Boolean): Boolean; overload; override;
    function GetBool: Boolean; overload; override;
    procedure SetBool(const Value: Boolean); override;
    function GetNull: Boolean; override;
    procedure SetNull(const Value: Boolean); override;
    function GetJavascript(const Default: String): String; overload; override;
    function GetJavascript: String; overload; override;
    procedure SetJavascript(const Value: String); override;
    function GetRegEx(const Default: String): String; overload; override;
    function GetRegEx: String; overload; override;
    procedure SetRegEx(const Pattern: String); override;
    function GetRegExOptions(const Default: TALPerlRegExOptions): TALPerlRegExOptions; overload; override;
    function GetRegExOptions: TALPerlRegExOptions; overload; override;
    procedure SetRegExOptions(const Value: TALPerlRegExOptions); override;
    function GetBinaryAsBytes(const Default: TBytes): TBytes; overload; override;
    function GetBinaryAsBytes: TBytes; overload; override;
    function GetBinaryAsStream(const Default: TStream): TStream; overload; override;
    function GetBinaryAsStream: TStream; overload; override;
    procedure SetBinaryAsBytes(const Data: TBytes); overload; override;
    procedure SetBinaryAsStream(const Data: TStream); overload; override;
    function GetOwnsBinaryStream: Boolean; override;
    procedure SetOwnsBinaryStream(const Value: Boolean); override;
    function GetBinarySubType(const Default: Byte): Byte; overload; override;
    function GetBinarySubType: Byte; overload; override;
    procedure SetBinarySubType(const Subtype: Byte); override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJSONDocumentW = class(TObject)
  private
    class function DetectNodeTypeFromJSON(const Buffer: String): TALJSONNodeType;
  public
    class function Create: TALJSONNodeW;
    class function CreateFromJSONString(const Str: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromBSONBytes(const Bytes: TBytes; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class procedure ParseJSONString(
                      const Str: String;
                      const OnParseText: TALJSONParseTextEventW;
                      const OnParseStartObject: TALJSONParseObjectEventW;
                      const OnParseEndObject: TALJSONParseObjectEventW;
                      const OnParseStartArray: TALJSONParseArrayEventW;
                      const OnParseEndArray: TALJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseJSONStream(
                      const Stream: TStream;
                      const OnParseText: TALJSONParseTextEventW;
                      const OnParseStartObject: TALJSONParseObjectEventW;
                      const OnParseEndObject: TALJSONParseObjectEventW;
                      const OnParseStartArray: TALJSONParseArrayEventW;
                      const OnParseEndArray: TALJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseJSONFile(
                      const FileName: String;
                      const OnParseText: TALJSONParseTextEventW;
                      const OnParseStartObject: TALJSONParseObjectEventW;
                      const OnParseEndObject: TALJSONParseObjectEventW;
                      const OnParseStartArray: TALJSONParseArrayEventW;
                      const OnParseEndArray: TALJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONBytes(
                      const Bytes: TBytes;
                      const OnParseText: TALJSONParseTextEventW;
                      const OnParseStartObject: TALJSONParseObjectEventW;
                      const OnParseEndObject: TALJSONParseObjectEventW;
                      const OnParseStartArray: TALJSONParseArrayEventW;
                      const OnParseEndArray: TALJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONStream(
                      const Stream: TStream;
                      const OnParseText: TALJSONParseTextEventW;
                      const OnParseStartObject: TALJSONParseObjectEventW;
                      const OnParseEndObject: TALJSONParseObjectEventW;
                      const OnParseStartArray: TALJSONParseArrayEventW;
                      const OnParseEndArray: TALJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONFile(
                      const FileName: String;
                      const OnParseText: TALJSONParseTextEventW;
                      const OnParseStartObject: TALJSONParseObjectEventW;
                      const OnParseEndObject: TALJSONParseObjectEventW;
                      const OnParseStartArray: TALJSONParseArrayEventW;
                      const OnParseEndArray: TALJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
  end;

var
  ALDefaultJsonNodeIndentW: String;  // var instead of const to avoid new ansitring on assign
  ALDefaultJsonPathSeparatorW: Char;
  ALJsonISODateFormatSettingsW: TALFormatSettingsW;

procedure ALJSONToTStringsW(
            const AJsonStr: String;
            const AFormatSettings: TALFormatSettingsW;
            const APath: String;
            const ALst: TALStringsW;
            const ANullStr: String = 'null';
            const ATrueStr: String = 'true';
            const AFalseStr: String = 'false'); overload;
procedure ALJSONToTStringsW(
            const AJsonStr: String;
            const AFormatSettings: TALFormatSettingsW;
            const ALst: TALStringsW;
            const ANullStr: String = 'null';
            const ATrueStr: String = 'true';
            const AFalseStr: String = 'false'); overload;
procedure ALJSONToTStringsW(
            const AJsonNode: TALJSONNodeW;
            const APath: String;
            const ALst: TALStringsW;
            const ANullStr: String = 'null';
            const ATrueStr: String = 'true';
            const AFalseStr: String = 'false'); overload;
procedure ALJSONToTStringsW(
            const AJsonNode: TALJSONNodeW;
            const ALst: TALStringsW;
            const ANullStr: String = 'null';
            const ATrueStr: String = 'true';
            const AFalseStr: String = 'false'); overload;
procedure ALTStringsToJsonW(
            const ALst: TALStringsW;
            const AJsonNode: TALJSONNodeW;
            const APath: String = '';
            const ANameToLowerCase: Boolean = false;
            const ANullStr: String = 'null');

function ALJsonEncodeFloatWithNodeSubTypeHelperW(const AValue: double): String;
function ALJsonEncodeTextWithNodeSubTypeHelperW(const AValue: String): String;
function ALJsonEncodeBinaryWithNodeSubTypeHelperW(const AValue: TBytes): String;
function ALJsonEncodeObjectIDWithNodeSubTypeHelperW(const AValue: TBytes): String;
function ALJsonEncodeBooleanWithNodeSubTypeHelperW(const AValue: Boolean): String;
function ALJsonEncodeDateTimeW(const AValue: TDateTime): String;
function ALJsonEncodeDateTimeWithNodeSubTypeHelperW(const AValue: TDateTime): String;
function ALJsonEncodeJavascriptWithNodeSubTypeHelperW(const AValue: String): String;
function ALJsonEncodeInt64WithNodeSubTypeHelperW(const AValue: Int64): String;
function ALJsonEncodeInt32WithNodeSubTypeHelperW(const AValue: Int32): String;
function ALJsonEncodeNullWithNodeSubTypeHelperW: String;

function ALJSONTryStrToRegExW(const S: String; out RegEx: String; out RegExOptions: TALPerlRegExOptions): Boolean;
function ALJSONTryStrToBinaryW(const S: String; out Data: TBytes; out Subtype: Byte): Boolean;
function ALJSONTryStrToDateTimeW(const S: String; out Value: TDateTime): Boolean;
function ALJSONTryStrToObjectIDW(const S: String; out Value: TBytes): Boolean;
function ALJSONTryStrToTimestampW(const S: String; out Value: TALBSONTimestamp): Boolean;
function ALJSONTryStrToInt32W(const S: String; out Value: Integer): Boolean;
function ALJSONTryStrToInt64W(const S: String; out Value: Int64): Boolean;

function ALFindJsonNodeByChildValueInt32W(
           const AJsonNode: TALJSONNodeW;
           const AChildName: String;
           const AChildValue : Int32;
           const ARecurse: Boolean = False): TALJSONNodeW;
function ALFindJsonNodeByChildValueInt64W(
           const AJsonNode: TALJSONNodeW;
           const AChildName: String;
           const AChildValue : Int64;
           const ARecurse: Boolean = False): TALJSONNodeW;
function ALFindJsonNodeByChildValueTextW(
           const AJsonNode: TALJSONNodeW;
           const AChildName: String;
           const AChildValue : String;
           const ARecurse: Boolean = False): TALJSONNodeW;

implementation

uses
  System.Math,
  System.Generics.Collections,
  system.IOUtils,
  System.DateUtils,
  System.AnsiStrings,
  Alcinoe.HTML,
  Alcinoe.Common;

type
  PBytes = ^TBytes;

{****************************************}
function ALFindJsonNodeByChildValueInt32A(
           const AJsonNode: TALJSONNodeA;
           const AChildName: AnsiString;
           const AChildValue : Int32;
           const ARecurse: Boolean = False): TALJSONNodeA;
begin
  Result := nil;
  if not (AJsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for var I := 0 to AJsonNode.ChildNodes.Count - 1 do begin
    var LCandidateNode := AJsonNode.ChildNodes[I];
    var LPropertyNode := LCandidateNode.ChildNodes.FindNode(AChildName);
    If (assigned(LPropertyNode)) and
       (LPropertyNode.NodeType = nttext) and
       (LPropertyNode.NodesubType = nstInt32) and
       (LPropertyNode.Int32 = AChildValue) then begin
      Result := LCandidateNode;
      exit;
    end;
    if ARecurse then begin
      Result := ALFindJsonNodeByChildValueInt32A(
                  LCandidateNode,
                  AChildName,
                  AChildValue,
                  ARecurse);
      if assigned(Result) then break;
    end;
  end;
end;

{****************************************}
function ALFindJsonNodeByChildValueInt64A(
           const AJsonNode: TALJSONNodeA;
           const AChildName: AnsiString;
           const AChildValue : Int64;
           const ARecurse: Boolean = False): TALJSONNodeA;
begin
  Result := nil;
  if not (AJsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for var I := 0 to AJsonNode.ChildNodes.Count - 1 do begin
    var LCandidateNode := AJsonNode.ChildNodes[I];
    var LPropertyNode := LCandidateNode.ChildNodes.FindNode(AChildName);
    If (assigned(LPropertyNode)) and
       (LPropertyNode.NodeType = nttext) and
       (LPropertyNode.NodesubType = nstInt64) and
       (LPropertyNode.Int64 = AChildValue) then begin
      Result := LCandidateNode;
      exit;
    end;
    if ARecurse then begin
      Result := ALFindJsonNodeByChildValueInt64A(
                  LCandidateNode,
                  AChildName,
                  AChildValue,
                  ARecurse);
      if assigned(Result) then break;
    end;
  end;
end;

{***************************************}
function ALFindJsonNodeByChildValueTextA(
           const AJsonNode: TALJSONNodeA;
           const AChildName: AnsiString;
           const AChildValue : AnsiString;
           const ARecurse: Boolean = False): TALJSONNodeA;
begin
  Result := nil;
  if not (AJsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for var I := 0 to AJsonNode.ChildNodes.Count - 1 do begin
    var LCandidateNode := AJsonNode.ChildNodes[I];
    var LPropertyNode := LCandidateNode.ChildNodes.FindNode(AChildName);
    If (assigned(LPropertyNode)) and
       (LPropertyNode.NodeType = nttext) and
       (LPropertyNode.NodesubType = nstText) and
       (LPropertyNode.text = AChildValue) then begin
      Result := LCandidateNode;
      exit;
    end;
    if ARecurse then begin
      Result := ALFindJsonNodeByChildValueTextA(
                  LCandidateNode,
                  AChildName,
                  AChildValue,
                  ARecurse);
      if assigned(Result) then break;
    end;
  end;
end;

{************************************************************************************************************************}
function ALJSONTryStrToRegExA(const S: AnsiString; out RegEx: AnsiString; out RegExOptions: TALPerlRegExOptions): Boolean;
begin

  // regular expression in JSON must look like: /pattern/options
  // list of valid options is:
  //  'i' for case insensitive matching,
  //  'm' for multiline matching,
  //  'x' for verbose mode,
  //  'l' to make \w, \W, etc. locale dependent,
  //  's' for dotall mode ('.' matches everything),
  //  'u' to make \w, \W, etc. match unicode.
  Result := false;

  // check that first character is /
  if (S <> '') and (S[1] = '/') then begin

    var P1 := ALLastDelimiterA('/', S);
    if P1 <> 1 then begin

      //init Value
      RegEx := ALCopyStr(S, 2, P1 - 2);
      RegExOptions := [];

      // loop on all the options characters
      // to check if they are allowed.
      for var I := P1 + 1 to Length(S) do
        case s[I] of
          'i': RegExOptions := RegExOptions + [preCaseLess];
          'm': RegExOptions := RegExOptions + [preMultiLine];
          'x': RegExOptions := RegExOptions + [preExtended];
          'l':;
          's': RegExOptions := RegExOptions + [preSingleLine];
          'u':;
          else exit;
        end;

      //set the Result to true
      Result := true;

      //Check if it's compiling
      //var LRegEx := TALPerlRegEx.Create;
      //try
      //  LRegEx.RegEx := Value.Expression;
      //  Result := LRegEx.Compile(false{RaiseException});
      //finally
      //  ALFreeAndNil(LRegEx);
      //end;

    end;

  end;

end;

{************************************************************************************************}
function ALJSONTryStrToBinaryA(const S: AnsiString; out Data: TBytes; out Subtype: Byte): Boolean;
begin

  // s must look like
  // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // BinData ( 0 , "JliB6gIMRuSphAD2KmhzgQ==" )
  Result := false;
  var Ln := length(s);
  var P1 := 1;

  while (P1 <= ln) and (s[P1] in [#9, ' ']) do inc(P1);

  if (P1 + 6 > ln) or
     (s[P1] <> 'B') or
     (s[P1+1] <> 'i') or
     (s[P1+2] <> 'n') or
     (s[P1+3] <> 'D') or
     (s[P1+4] <> 'a') or
     (s[P1+5] <> 't') or
     (s[P1+6] <> 'a') then exit; // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                 // ^

  P1 := p1 + 7{Length('BinData')}; // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                   //        ^
  while (P1 <= ln) and (s[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (s[P1] <> '(') then exit; // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                            //        ^P1

  inc(P1); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
           //         ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                                        //          ^P1
  if (P1 > ln) then exit;

  var P2 := P1;
  while (P2 <= ln) and (S[P2] in ['0'..'9']) do inc(P2); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                                         //           ^P2
  if P2 > ln then exit;
  var LInt: Integer;
  if not ALTryStrToInt(ALcopyStr(S,P1,P2-P1), LInt) then Exit;
  subtype := LInt;

  p1 := P2;
  while (P1 <= ln) and (s[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (s[P1] <> ',') then exit; // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                            //            ^P2

  inc(P1); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
           //             ^P1
  while (P1 <= ln) and (s[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (not (s[P1] in ['"',''''])) then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                                                         //            ^P1

  P2 := length(s);
  while (P2 > p1) and (s[P2] in [#9, ' ']) do dec(P2);
  if (P2 <= p1) or (s[P2] <> ')') then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                                             //                                      ^P2

  dec(p2);
  if (P2 <= p1) then exit;
  while (P2 > p1) and (s[P2] in [#9, ' ']) do dec(P2);
  if (P2 <= p1) or (s[P2] <> s[P1]) then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                                               //                                     ^P2

  inc(p1);
  Data := ALBase64DecodeBytes(ALCopyStr(s, P1, P2-P1));

  // set the result
  Result := true;

end;

{***********************************************************************************}
function ALJSONTryStrToDateTimeA(const S: AnsiString; out Value: TDateTime): Boolean;
begin

  // s must look like
  // new Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // new ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  // ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  Result := false;
  var Ln := length(s);
  var P1: Integer;
  if ALPosA('new', s) = 1 then P1 := 4{length('new') + 1} // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                          //    ^P1
  else P1 := 1; // Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                // ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <= ln - 3) and
     (S[P1]   = 'D') and
     (S[P1+1] = 'a') and
     (S[P1+2] = 't') and
     (S[P1+3] = 'e') then inc(p1, 4)  // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                      //          ^P1
  else if (P1 <= ln - 6) and
          (S[P1]   = 'I') and
          (S[P1+1] = 'S') and
          (S[P1+2] = 'O') and
          (S[P1+3] = 'D') and
          (S[P1+4] = 'a') and
          (S[P1+5] = 't') and
          (S[P1+6] = 'e') then inc(p1, 7)  // ISODate ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                           //        ^P1
  else exit;
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                            //           ^P1
  inc(P1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
           //            ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (not (S[P1] in ['''','"'])) then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                         //             ^P1
  var LQuoteChar := S[P1]; // "
  inc(p1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
           //              ^P1
  var P2 := P1;
  while (P1 <= ln) and (S[P1] <> LQuoteChar) do inc(P1);
  if (P1 > ln) then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                          //                                      ^P1
  dec(P1);
  if S[P1] <> 'Z' then exit;
  var LTmpStr := AlcopyStr(S,P2,P1-P2); // yyyy-mm-ddThh:nn:ss.zzz

  P2 := 1;
  var LTmpLn := length(LTmpStr);
  while (P2 <= LTmpLn) and (LTmpStr[P2] <> 'T') do inc(P2);
  if P2 > LTmpLn then exit;
  LTmpStr[P2] := ' '; // yyyy-mm-dd hh:nn:ss.zzz

  Result := ALTryStrToDateTime(LTmpStr, Value, ALJsonISODateFormatSettingsA);
  if not Result then exit;

  inc(p1,2);  // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
              //                                       ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> ln) or (S[P1] <> ')') then begin
    Result := false;
    exit;
  end;

end;

{****************************************************}
// ObjectId is a 12-Byte BSON type, constructed using:
// a 4-Byte value representing the seconds since the Unix epoch,
// a 3-Byte machine identifier,
// a 2-Byte process id, and
// a 3-Byte counter, starting with a random value.
function ALJSONTryStrToObjectIDA(const S: AnsiString; out Value: TBytes): Boolean;
begin

  // s must look like
  // ObjectId ( "507f1f77bcf86cd799439011" )
  Result := false;
  if ALPosA('ObjectId', S) <> 1 then exit;
  var Ln := length(s);
  var P1 := 9{length('ObjectId') + 1}; // ObjectId ( "507f1f77bcf86cd799439011" )
                                       //         ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                            //          ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //           ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (not (S[P1] in ['''','"'])) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                         //            ^P1
  var LQuoteChar := S[P1]; // "
  inc(p1); // ObjectId ( "507f1f77bcf86cd799439011" )
           //             ^P1
  if (P1 + 23{(length(aObjectIDhex)) - 1} > ln) then exit;
  var LObjectIDhex := ALcopyStr(S,P1,24{length(aObjectIDhex)}); // 507f1f77bcf86cd799439011
  inc(P1, 24{length(aObjectIDhex)}); // ObjectId ( "507f1f77bcf86cd799439011" )
                                     //                                     ^P1
  if (P1 > ln) or (S[P1] <> LQuoteChar) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                   //                                     ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //                                      ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> ln) or (S[P1] <> ')') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                             //                                       ^P1
  //convert 507f1f77bcf86cd799439011 to binary
  Result := ALTryHexToBin(LObjectIDhex, Value) and
            (length(Value) = 12);

end;

{*******************************************************************************************}
function ALJSONTryStrToTimestampA(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
begin

  // s must look like
  // Timestamp(0, 0)
  Result        := false;
  if ALPosA('Timestamp', S) <> 1 then Exit;
  var Ln := length(s);
  var P1 := 10{Length('Timestamp') + 1}; // Timestamp(0, 0)
                                     //          ^
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // Timestamp(0, 0)
                                            //          ^P1
  var P2 := ALPosA(')', S, P1);
  if P2 <> ln then exit; // Timestamp(0, 0)
                         //               ^P2
  var LArgs := ALCopyStr(S, P1+1, P2 - P1-1); // 0, 0

  // take arguments of function Timestamp
  P1 := ALPosA(',', LArgs);
  var LArg1: Integer;
  var LArg2: Integer;
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, 1,      P1 - 1)), LArg1) then Exit;
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, P1 + 1, maxint)), LArg2) then Exit;

  // build result
  Result := true;
  Value.W1 := LArg1; // higher 4 Bytes - increment
  Value.W2 := LArg2; // lower  4 Bytes - timestamp
end;

{******************************************************************************}
function ALJSONTryStrToInt32A(const S: AnsiString; out Value: Integer): Boolean;
begin

  // s must look like
  // NumberInt ( "12391293" )
  // NumberInt ( 12391293 )
  // 12391293
  Result := ALTryStrToInt(S, Value);
  if Result then exit;
  if ALPosA('NumberInt', S) <> 1 then exit;
  var Ln := length(s);
  var P1 := 10{length('NumberInt') + 1}; // NumberInt ( "12391293" )
                                         //          ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // NumberInt ( "12391293" )
                                            //           ^P1
  inc(p1);  // NumberInt ( "12391293" )
            //            ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  var LTmpStr: AnsiString;
  if (P1 > ln) then exit
  else if (not (S[P1] in ['''','"'])) then begin // NumberInt ( 12391293 )
                                                 //             ^P1
    var P2 := P1+1;
    while (P2 <= ln) and (S[P2] in ['0'..'9']) do inc(P2); // NumberInt ( 12391293 )
                                                           //                     ^P2
    if P2 > ln then exit;
    LTmpStr := ALcopyStr(S,P1,P2-P1); // 12391293
    P1 := P2; // NumberInt ( 12391293 )
              //                     ^P2

    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberInt ( "12391293" )
                                               //                        ^P1
  end
  else begin // NumberInt ( "12391293" )
             //             ^P1

    var LQuoteChar := S[P1]; // "
    inc(p1); // NumberInt ( "12391293" )
             //              ^P1
    var P2 := P1;
    while P2 <= Ln do
      if S[P2] = LQuoteChar then break
      else inc(P2);
    if P2 > ln then exit;
    LTmpStr := ALcopyStr(S,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberInt ( "12391293" )
                  //                       ^P1
    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberInt ( "12391293" )
                                               //                        ^P1
  end;

  //convert 12391293 to Integer
  Result := ALTryStrToInt(LTmpStr, Value);

end;

{****************************************************************************}
function ALJSONTryStrToInt64A(const S: AnsiString; out Value: Int64): Boolean;
begin

  // s must look like
  // NumberLong ( "12391293" )
  // NumberLong ( 12391293 )
  // 12391293
  Result := ALTryStrToInt64(S, Value);
  if Result then exit;
  if ALPosA('NumberLong', S) <> 1 then exit;
  var Ln := length(s);
  var P1 := 11{length('NumberLong') + 1}; // NumberLong ( "12391293" )
                                          //           ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // NumberLong ( "12391293" )
                                            //            ^P1
  inc(p1);  // NumberLong ( "12391293" )
            //             ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  var LTmpStr: AnsiString;
  if (P1 > ln) then exit
  else if (not (S[P1] in ['''','"'])) then begin // NumberLong ( 12391293 )
                                                 //              ^P1
    var P2 := P1+1;
    while (P2 <= ln) and (S[P2] in ['0'..'9']) do inc(P2); // NumberLong ( 12391293 )
                                                           //                      ^P2
    if P2 > ln then exit;
    LTmpStr := ALcopyStr(S,P1,P2-P1); // 12391293
    P1 := P2; // NumberLong ( 12391293 )
              //                      ^P2

    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberLong ( "12391293" )
                                               //                         ^P1
  end
  else begin // NumberLong ( "12391293" )
             //              ^P1

    var LQuoteChar := S[P1]; // "
    inc(p1); // NumberLong ( "12391293" )
             //               ^P1
    var P2 := P1;
    while P2 <= Ln do
      if S[P2] = LQuoteChar then break
      else inc(P2);
    if P2 > ln then exit;
    LTmpStr := ALcopyStr(S,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberLong ( "12391293" )
                  //                        ^P1
    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberLong ( "12391293" )
                                               //                         ^P1
  end;

  //convert 12391293 to Integer
  Result := ALTryStrToInt64(LTmpStr, Value);

end;

{***************************************************************}
procedure AlJSONDocErrorA(const Msg: String); noreturn; overload;
begin
  raise EALJSONDocError.Create(Msg);
end;

{*******************************************************************************************}
procedure AlJSONDocErrorA(const Msg: String; const Args: array of const); noreturn; overload;
begin
  raise EALJSONDocError.CreateFmt(Msg, Args);
end;

{************************************************************************************************}
procedure AlJSONDocErrorA(const Msg: String; const NodeType: TALJSONNodeType); noreturn; overload;
begin
  case NodeType of
    ntObject: AlJSONDocErrorA(Msg, ['ntObject']);
    ntArray: AlJSONDocErrorA(Msg, ['ntArray']);
    ntText: AlJSONDocErrorA(Msg, ['ntText']);
    else AlJSONDocErrorA(ALJSONInvalidNodeType);
  end;
end;

{********************************************************************************************}
{Call CreateNode to create a new generic JSON node. The resulting node does not have a parent,
 but can be added to the ChildNodes list of any node in the document.}
function ALCreateJSONNodeA(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNodeA;
begin
  case NodeType of
    ntObject: Result := TALJSONObjectNodeA.Create(NodeName);
    ntArray: Result := TALJSONArrayNodeA.Create(NodeName);
    ntText: Result := TALJSONTextNodeA.Create(NodeName);
    else AlJSONDocErrorA(ALJSONInvalidNodeType);
  end;
end;

{*****************************************************}
class function TALJSONDocumentA.DetectNodeTypeFromJSON(
                 const RawJSONStream: TStream;
                 const RawJSONString: AnsiString): TALJSONNodeType;

Const
  BufferSize: Integer = 32768;

Var
  Buffer: AnsiString;
  BufferLength: Integer;
  BufferPos: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer: Boolean;
  begin
    if not assigned(RawJSONStream) then begin
      Result := false;
      exit;
    end;

    var Byte2Read: Integer;
    If (BufferLength > 0) and (BufferPos > 1) then begin
      if (BufferPos > BufferLength) then RawJSONStream.Position := RawJSONStream.Position - BufferLength + BufferPos - 1;
      Byte2Read := min(BufferPos - 1, BufferLength);
      if BufferPos <= length(Buffer) then ALMove(
                                            PByte(Buffer)[BufferPos - 1],
                                            pointer(Buffer)^,
                                            BufferLength-BufferPos+1);
      BufferPos := 1;
    end
    else begin
      Byte2Read := BufferSize;
      BufferLength := BufferLength + BufferSize;
      SetLength(Buffer, BufferLength);
    end;

    //range check error is we not do so
    var ByteReaded: Integer;
    if RawJSONStream.Position < RawJSONStream.Size then ByteReaded := RawJSONStream.Read(PByte(Buffer)[BufferLength - Byte2Read{+ 1 - 1}],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      BufferLength := BufferLength - Byte2Read + ByteReaded;
      SetLength(Buffer, BufferLength);
      Result := ByteReaded > 0;
    end
    else Result := True;
  end;

begin

  //--
  Result := ntText;

  //--
  var InitialStreamPosition: Int64;
  if assigned(RawJSONStream) then begin
    InitialStreamPosition := RawJSONStream.Position;
    RawJSONStream.Position := 0;
    Buffer := '';
    BufferLength := 0;
    BufferPos := 1;
    ExpandBuffer;
  end
  else begin
    InitialStreamPosition := 0; // To hide warning bug
    Buffer := RawJSONString;
    BufferLength := length(RawJSONString);
    BufferPos := 1;
  end;

  //--
  var BOMSequence: Integer := 0; // hide warnings
  While (BufferPos <= BufferLength) or ExpandBuffer do begin
    var c := Buffer[BufferPos];
    If c <= ' ' then inc(bufferPos)
    else if ((bufferPos = 1) and (c=#$EF)) then begin
      BOMSequence := 1;
      inc(bufferPos);
    end
    else if ((bufferPos = 2) and (BOMSequence=1) and (c=#$BB)) then begin
      BOMSequence := 2;
      inc(bufferPos);
    end
    else if ((bufferPos = 3) and (BOMSequence=2) and (c=#$BF)) then begin
      BOMSequence := 0;
      inc(bufferPos);
    end
    else begin
      if c = '{' then Result := ntObject
      else if c = '[' then Result := ntarray
      else Result := ntText;
      break;
    end;
  end;

  //--
  if assigned(RawJSONStream) then
    RawJSONStream.Position := InitialStreamPosition;

end;

{***************************************************}
class function TALJSONDocumentA.Create: TALJSONNodeA;
begin
  Result := ALCreateJSONNodeA('', ntObject);
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromJSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  var LNodeType := DetectNodeTypeFromJSON(nil, Str);
  if LNodeType in [ntObject, ntArray] then Result := ALCreateJSONNodeA('', LNodeType)
  else AlJSONDocErrorA(ALJSONParseError);
  try
    Result.LoadFromJSONString(Str, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  var LNodeType := DetectNodeTypeFromJSON(Stream, '');
  if LNodeType in [ntObject, ntArray] then Result := ALCreateJSONNodeA('', LNodeType)
  else AlJSONDocErrorA(ALJSONParseError);
  try
    Result.LoadFromJSONStream(Stream, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{*************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    Result := CreateFromJSONStream(LFileStream, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*****************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromJSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  Result := CreateFromJSONFile(String(FileName), Options);
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromBSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  Result := ALCreateJSONNodeA('', ntObject);
  try
    Result.LoadFromBSONString(Str, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{***********************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromBSONBytes(const Bytes: TBytes; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  Result := ALCreateJSONNodeA('', ntObject);
  try
    Result.LoadFromBSONBytes(Bytes, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  Result := ALCreateJSONNodeA('', ntObject);
  try
    Result.LoadFromBSONStream(Stream, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{*************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    Result := CreateFromBSONStream(LFileStream, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*****************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromBSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  Result := CreateFromBSONFile(String(FileName), Options);
end;

{***********************************************}
class procedure TALJSONDocumentA.ParseJSONString(
                  const Str: AnsiString;
                  const OnParseText: TALJSONParseTextEventA;
                  const OnParseStartObject: TALJSONParseObjectEventA;
                  const OnParseEndObject: TALJSONParseObjectEventA;
                  const OnParseStartArray: TALJSONParseArrayEventA;
                  const OnParseEndArray: TALJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  var LJsonNode: TALJSONNodeA;
  var LNodeType := DetectNodeTypeFromJSON(nil, Str);
  if LNodeType in [ntObject, ntArray] then LJsonNode := ALCreateJSONNodeA('', LNodeType)
  else AlJSONDocErrorA(ALJSONParseError);
  try
    LJsonNode.ParseJSONString(
      Str,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LJsonNode);
  end;
end;

{***********************************************}
class procedure TALJSONDocumentA.ParseJSONStream(
                  const Stream: TStream;
                  const OnParseText: TALJSONParseTextEventA;
                  const OnParseStartObject: TALJSONParseObjectEventA;
                  const OnParseEndObject: TALJSONParseObjectEventA;
                  const OnParseStartArray: TALJSONParseArrayEventA;
                  const OnParseEndArray: TALJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  var LJsonNode: TALJSONNodeA;
  var LNodeType := DetectNodeTypeFromJSON(Stream, '');
  if LNodeType in [ntObject, ntArray] then LJsonNode := ALCreateJSONNodeA('', LNodeType)
  else AlJSONDocErrorA(ALJSONParseError);
  try
    LJsonNode.ParseJSONStream(
      Stream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LJsonNode);
  end;
end;

{*********************************************}
class procedure TALJSONDocumentA.ParseJSONFile(
                  const FileName: String;
                  const OnParseText: TALJSONParseTextEventA;
                  const OnParseStartObject: TALJSONParseObjectEventA;
                  const OnParseEndObject: TALJSONParseObjectEventA;
                  const OnParseStartArray: TALJSONParseArrayEventA;
                  const OnParseEndArray: TALJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseJSONStream(
      LFileStream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*********************************************}
class procedure TALJSONDocumentA.ParseJSONFile(
                  const FileName: AnsiString;
                  const OnParseText: TALJSONParseTextEventA;
                  const OnParseStartObject: TALJSONParseObjectEventA;
                  const OnParseEndObject: TALJSONParseObjectEventA;
                  const OnParseStartArray: TALJSONParseArrayEventA;
                  const OnParseEndArray: TALJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  ParseJSONFile(
    String(FileName),
    OnParseText,
    OnParseStartObject,
    OnParseEndObject,
    OnParseStartArray,
    OnParseEndArray,
    Options);
end;

{***********************************************}
class procedure TALJSONDocumentA.ParseBSONString(
                  const Str: AnsiString;
                  const OnParseText: TALJSONParseTextEventA;
                  const OnParseStartObject: TALJSONParseObjectEventA;
                  const OnParseEndObject: TALJSONParseObjectEventA;
                  const OnParseStartArray: TALJSONParseArrayEventA;
                  const OnParseEndArray: TALJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  var LJsonNode := ALCreateJSONNodeA('', ntObject);
  try
    LJsonNode.ParseBSONString(
      Str,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LJsonNode);
  end;
end;

{***********************************************}
class procedure TALJSONDocumentA.ParseBSONStream(
                  const Stream: TStream;
                  const OnParseText: TALJSONParseTextEventA;
                  const OnParseStartObject: TALJSONParseObjectEventA;
                  const OnParseEndObject: TALJSONParseObjectEventA;
                  const OnParseStartArray: TALJSONParseArrayEventA;
                  const OnParseEndArray: TALJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  var LJsonNode := ALCreateJSONNodeA('', ntObject);
  try
    LJsonNode.ParseBSONStream(
      Stream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LJsonNode);
  end;
end;

{*********************************************}
class procedure TALJSONDocumentA.ParseBSONFile(
                  const FileName: String;
                  const OnParseText: TALJSONParseTextEventA;
                  const OnParseStartObject: TALJSONParseObjectEventA;
                  const OnParseEndObject: TALJSONParseObjectEventA;
                  const OnParseStartArray: TALJSONParseArrayEventA;
                  const OnParseEndArray: TALJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseBSONStream(
      LFileStream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*********************************************}
class procedure TALJSONDocumentA.ParseBSONFile(
                  const FileName: AnsiString;
                  const OnParseText: TALJSONParseTextEventA;
                  const OnParseStartObject: TALJSONParseObjectEventA;
                  const OnParseEndObject: TALJSONParseObjectEventA;
                  const OnParseStartArray: TALJSONParseArrayEventA;
                  const OnParseEndArray: TALJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  ParseBSONFile(
    String(FileName),
    OnParseText,
    OnParseStartObject,
    OnParseEndObject,
    OnParseStartArray,
    OnParseEndArray,
    Options);
end;

{**********************************************************}
{Creates the object that implements the ChildNodes property}
function TALJSONNodeA.CreateChildList: TALJSONNodeListA;
begin
  Result := TALJSONNodeListA.Create;
end;

{********************************************}
function TALJSONNodeA.InternalGetChildNodes: TALJSONNodeListA;
begin
  Result := nil;
end;

{****************************************************}
function TALJSONNodeA.GetChildNodes: TALJSONNodeListA;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType)
end;

{***************************************************************************}
function TALJSONNodeA.GetChildNode(const NodeName: AnsiString): TALJSONNodeA;
begin
  Result := ChildNodes.findNode(nodeName);
end;

{*********************************************************************************************************}
function TALJSONNodeA.GetChildValueText(const NodeName: AnsiString; const Default: AnsiString): AnsiString;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetText(default);
end;

{**************************************************************************************************}
function TALJSONNodeA.GetChildValueFloat(const NodeName: AnsiString; const Default: Double): Double;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetFloat(default);
end;

{***********************************************************************************************************}
function TALJSONNodeA.GetChildValueDateTime(const NodeName: AnsiString; const Default: TDateTime): TDateTime;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetDateTime(default);
end;

{**************************************************************************************************************************}
function TALJSONNodeA.GetChildValueTimestamp(const NodeName: AnsiString; const Default: TALBSONTimestamp): TALBSONTimestamp;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetTimestamp(default);
end;

{*************************************************************************************************************}
function TALJSONNodeA.GetChildValueObjectID(const NodeName: AnsiString; const Default: TBytes): TBytes;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetObjectID(default);
end;

{****************************************************************************************************}
function TALJSONNodeA.GetChildValueInt32(const NodeName: AnsiString; const Default: Integer): Integer;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetInt32(default);
end;

{************************************************************************************************}
function TALJSONNodeA.GetChildValueInt64(const NodeName: AnsiString; const Default: Int64): Int64;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetInt64(default);
end;

{***************************************************************************************************}
function TALJSONNodeA.GetChildValueBool(const NodeName: AnsiString; const Default: Boolean): Boolean;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBool(default);
end;

{***************************************************************************************************************}
function TALJSONNodeA.GetChildValueJavascript(const NodeName: AnsiString; const Default: AnsiString): AnsiString;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetJavascript(default);
end;

{**********************************************************************************************************}
function TALJSONNodeA.GetChildValueRegEx(const NodeName: AnsiString; const Default: AnsiString): AnsiString;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetRegEx(default);
end;

{***********************************************************************************************************************************}
function TALJSONNodeA.GetChildValueRegExOptions(const NodeName: AnsiString; const Default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetRegExOptions(default);
end;

{******************************************************************************************************}
function TALJSONNodeA.GetChildValueBinaryAsBytes(const NodeName: AnsiString; const Default: TBytes): TBytes;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinaryAsBytes(default);
end;

{******************************************************************************************************}
function TALJSONNodeA.GetChildValueBinaryAsStream(const NodeName: AnsiString; const Default: TStream): TStream;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinaryAsStream(default);
end;

{******************************************************************************************************}
function TALJSONNodeA.GetChildValueBinarySubType(const NodeName: AnsiString; const Default: Byte): Byte;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinarySubType(default);
end;

{***************************************************************************}
function TALJSONNodeA.GetChildValueNull(const NodeName: AnsiString): Boolean;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := true
  else Result := LNode.GetNull;
end;

{********************************************************************************}
function TALJSONNodeA.GetChildNode(const Path: array of AnsiString): TALJSONNodeA;
begin
  Result := Self;
  for var I := low(path) to high(path) do begin
    Result := Result.ChildNodes.findNode(path[I]);
    if (result = nil) then exit;
  end;
end;

{**************************************************************************************************************}
function TALJSONNodeA.GetChildValueText(const Path: array of AnsiString; const Default: AnsiString): AnsiString;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetText(default);
end;

{*******************************************************************************************************}
function TALJSONNodeA.GetChildValueFloat(const Path: array of AnsiString; const Default: Double): Double;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetFloat(default);
end;

{****************************************************************************************************************}
function TALJSONNodeA.GetChildValueDateTime(const Path: array of AnsiString; const Default: TDateTime): TDateTime;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetDateTime(default);
end;

{*******************************************************************************************************************************}
function TALJSONNodeA.GetChildValueTimestamp(const Path: array of AnsiString; const Default: TALBSONTimestamp): TALBSONTimestamp;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetTimestamp(default);
end;

{******************************************************************************************************************}
function TALJSONNodeA.GetChildValueObjectID(const Path: array of AnsiString; const Default: TBytes): TBytes;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetObjectID(default);
end;

{*********************************************************************************************************}
function TALJSONNodeA.GetChildValueInt32(const Path: array of AnsiString; const Default: Integer): Integer;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetInt32(default);
end;

{*****************************************************************************************************}
function TALJSONNodeA.GetChildValueInt64(const Path: array of AnsiString; const Default: Int64): Int64;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetInt64(default);
end;

{********************************************************************************************************}
function TALJSONNodeA.GetChildValueBool(const Path: array of AnsiString; const Default: Boolean): Boolean;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBool(default);
end;

{********************************************************************************************************************}
function TALJSONNodeA.GetChildValueJavascript(const Path: array of AnsiString; const Default: AnsiString): AnsiString;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetJavascript(default);
end;

{***************************************************************************************************************}
function TALJSONNodeA.GetChildValueRegEx(const Path: array of AnsiString; const Default: AnsiString): AnsiString;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetRegEx(default);
end;

{****************************************************************************************************************************************}
function TALJSONNodeA.GetChildValueRegExOptions(const Path: array of AnsiString; const Default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetRegExOptions(default);
end;

{******************************************************************************************************}
function TALJSONNodeA.GetChildValueBinaryAsBytes(const Path: array of AnsiString; const Default: TBytes): TBytes;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinaryAsBytes(default);
end;

{******************************************************************************************************}
function TALJSONNodeA.GetChildValueBinaryAsStream(const Path: array of AnsiString; const Default: TStream): TStream;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinaryAsStream(default);
end;

{***********************************************************************************************************}
function TALJSONNodeA.GetChildValueBinarySubType(const Path: array of AnsiString; const Default: Byte): Byte;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinarySubType(default);
end;

{********************************************************************************}
function TALJSONNodeA.GetChildValueNull(const Path: array of AnsiString): Boolean;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := True;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := true
  else Result := LNode.GetNull;
end;

{********************************************************************************************}
procedure TALJSONNodeA.SetChildValueText(const NodeName: AnsiString; const Value: AnsiString);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetText(value)
  else LNode.SetText(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeA.SetChildValueFloat(const NodeName: AnsiString; const Value: Double);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetFloat(value)
  else LNode.SetFloat(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeA.SetChildValueDateTime(const NodeName: AnsiString; const Value: TDateTime);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetDateTime(value)
  else LNode.SetDateTime(value);
end;

{*******************************************************************************************************}
procedure TALJSONNodeA.SetChildValueTimestamp(const NodeName: AnsiString; const Value: TALBSONTimestamp);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetTimestamp(value)
  else LNode.SetTimestamp(value);
end;

{************************************************************************************************}
procedure TALJSONNodeA.SetChildValueObjectID(const NodeName: AnsiString; const Value: TBytes);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetObjectID(value)
  else LNode.SetObjectID(value);
end;

{******************************************************************************************}
procedure TALJSONNodeA.SetChildValueInt32(const NodeName: AnsiString; const Value: Integer);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt32(value)
  else LNode.SetInt32(value);
end;

{****************************************************************************************}
procedure TALJSONNodeA.SetChildValueInt64(const NodeName: AnsiString; const Value: Int64);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt64(value)
  else LNode.SetInt64(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeA.SetChildValueBool(const NodeName: AnsiString; const Value: Boolean);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBool(value)
  else LNode.SetBool(value);
end;

{**************************************************************************************************}
procedure TALJSONNodeA.SetChildValueJavascript(const NodeName: AnsiString; const Value: AnsiString);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetJavascript(value)
  else LNode.SetJavascript(value);
end;

{*********************************************************************************************}
procedure TALJSONNodeA.SetChildValueRegEx(const NodeName: AnsiString; const Value: AnsiString);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegEx(value)
  else LNode.SetRegEx(value);
end;

{*************************************************************************************************************}
procedure TALJSONNodeA.SetChildValueRegExOptions(const NodeName: AnsiString; const Value: TALPerlRegExOptions);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegExOptions(value)
  else LNode.SetRegExOptions(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeA.SetChildValueBinaryAsBytes(const NodeName: AnsiString; const Value: TBytes);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinaryAsBytes(value)
  else LNode.SetBinaryAsBytes(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeA.SetChildValueBinaryAsStream(const NodeName: AnsiString; const Value: TStream);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinaryAsStream(value)
  else LNode.SetBinaryAsStream(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeA.SetChildValueBinarySubType(const NodeName: AnsiString; const Value: Byte);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinarySubType(value)
  else LNode.SetBinarySubType(value);
end;

{*******************************************************************}
procedure TALJSONNodeA.SetChildValueNull(const NodeName: AnsiString);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetNull(true)
  else LNode.SetNull(true);
end;

{*************************************************************************************************}
procedure TALJSONNodeA.SetChildValueText(const Path: array of AnsiString; const Value: AnsiString);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetText(value)
  else LTmpNode.SetText(value);
end;

{**********************************************************************************************}
procedure TALJSONNodeA.SetChildValueFloat(const Path: array of AnsiString; const Value: Double);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetFloat(value)
  else LTmpNode.SetFloat(value);
end;

{****************************************************************************************************}
procedure TALJSONNodeA.SetChildValueDateTime(const Path: array of AnsiString; const Value: TDateTime);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetDateTime(value)
  else LTmpNode.SetDateTime(value);
end;

{************************************************************************************************************}
procedure TALJSONNodeA.SetChildValueTimestamp(const Path: array of AnsiString; const Value: TALBSONTimestamp);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetTimestamp(value)
  else LTmpNode.SetTimestamp(value);
end;

{*****************************************************************************************************}
procedure TALJSONNodeA.SetChildValueObjectID(const Path: array of AnsiString; const Value: TBytes);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetObjectID(value)
  else LTmpNode.SetObjectID(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeA.SetChildValueInt32(const Path: array of AnsiString; const Value: Integer);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetInt32(value)
  else LTmpNode.SetInt32(value);
end;

{*********************************************************************************************}
procedure TALJSONNodeA.SetChildValueInt64(const Path: array of AnsiString; const Value: Int64);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetInt64(value)
  else LTmpNode.SetInt64(value);
end;

{**********************************************************************************************}
procedure TALJSONNodeA.SetChildValueBool(const Path: array of AnsiString; const Value: Boolean);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBool(value)
  else LTmpNode.SetBool(value);
end;

{*******************************************************************************************************}
procedure TALJSONNodeA.SetChildValueJavascript(const Path: array of AnsiString; const Value: AnsiString);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetJavascript(value)
  else LTmpNode.SetJavascript(value);
end;

{**************************************************************************************************}
procedure TALJSONNodeA.SetChildValueRegEx(const Path: array of AnsiString; const Value: AnsiString);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetRegEx(value)
  else LTmpNode.SetRegEx(value);
end;

{******************************************************************************************************************}
procedure TALJSONNodeA.SetChildValueRegExOptions(const Path: array of AnsiString; const Value: TALPerlRegExOptions);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetRegExOptions(value)
  else LTmpNode.SetRegExOptions(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeA.SetChildValueBinaryAsBytes(const Path: array of AnsiString; const Value: TBytes);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinaryAsBytes(value)
  else LTmpNode.SetBinaryAsBytes(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeA.SetChildValueBinaryAsStream(const Path: array of AnsiString; const Value: TStream);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinaryAsStream(value)
  else LTmpNode.SetBinaryAsStream(value);
end;

{****************************************************************************************************}
procedure TALJSONNodeA.SetChildValueBinarySubType(const Path: array of AnsiString; const Value: Byte);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinarySubType(value)
  else LTmpNode.SetBinarySubType(value);
end;

{************************************************************************}
procedure TALJSONNodeA.SetChildValueNull(const Path: array of AnsiString);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetNull(true)
  else LTmpNode.SetNull(true);
end;

{***********************************************}
{Indicates whether this node has any child nodes}
function TALJSONNodeA.GetHasChildNodes: Boolean;
begin
  var LNodeList := InternalGetChildNodes;
  Result := assigned(LNodeList) and (LNodeList.Count > 0);
end;

{*************************************************************}
procedure TALJSONNodeA.SetNodeName(const NodeName: AnsiString);
begin
  if FNodeName <> NodeName then
    FNodeName := NodeName;
end;

{******************************************************************************}
// By default json (ie: javascript) treats all numbers as floating-point values.
// To let other system (ie: mongoDB) understand the type of the number
// we provide the helper functions NumberLong() to handle 64-bit Integers
// and NumberInt() to handle 32-bit Integers (and some others). theses helper functions are
// used when saving the json document.
function TALJSONNodeA.GetInterchangeValue(const SkipNodeSubTypeHelper: Boolean = False): AnsiString;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONNodeA.GetText: AnsiString;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*******************************************************************}
function TALJSONNodeA.GetText(const Default: AnsiString): AnsiString;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONNodeA.SetText(const Value: AnsiString);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*************************************}
function TALJSONNodeA.GetFloat: Double;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{************************************************************}
function TALJSONNodeA.GetFloat(const Default: Double): Double;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{***************************************************}
procedure TALJSONNodeA.SetFloat(const Value: Double);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*******************************************}
function TALJSONNodeA.GetDateTime: TDateTime;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*********************************************************************}
function TALJSONNodeA.GetDateTime(const Default: TDateTime): TDateTime;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*********************************************************}
procedure TALJSONNodeA.SetDateTime(const Value: TDateTime);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{***************************************************}
function TALJSONNodeA.GetTimestamp: TALBSONTimestamp;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{************************************************************************************}
function TALJSONNodeA.GetTimestamp(const Default: TALBSONTimestamp): TALBSONTimestamp;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*****************************************************************}
procedure TALJSONNodeA.SetTimestamp(const Value: TALBSONTimestamp);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************}
function TALJSONNodeA.GetObjectID: TBytes;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{***************************************************************}
function TALJSONNodeA.GetObjectID(const Default: TBytes): TBytes;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{******************************************************}
procedure TALJSONNodeA.SetObjectID(const Value: TBytes);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{**************************************}
function TALJSONNodeA.GetInt32: Integer;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{**************************************************************}
function TALJSONNodeA.GetInt32(const Default: Integer): Integer;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************}
procedure TALJSONNodeA.SetInt32(const Value: Integer);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{************************************}
function TALJSONNodeA.GetInt64: Int64;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{**********************************************************}
function TALJSONNodeA.GetInt64(const Default: Int64): Int64;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{**************************************************}
procedure TALJSONNodeA.SetInt64(const Value: Int64);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*************************************}
function TALJSONNodeA.GetBool: Boolean;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*************************************************************}
function TALJSONNodeA.GetBool(const Default: Boolean): Boolean;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{***************************************************}
procedure TALJSONNodeA.SetBool(const Value: Boolean);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*************************************}
function TALJSONNodeA.GetNull: Boolean;
begin
  Result := False;
end;

{***************************************************}
procedure TALJSONNodeA.SetNull(const Value: Boolean);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{**********************************************}
function TALJSONNodeA.GetJavascript: AnsiString;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*************************************************************************}
function TALJSONNodeA.GetJavascript(const Default: AnsiString): AnsiString;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{************************************************************}
procedure TALJSONNodeA.SetJavascript(const Value: AnsiString);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*****************************************}
function TALJSONNodeA.GetRegEx: AnsiString;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{********************************************************************}
function TALJSONNodeA.GetRegEx(const Default: AnsiString): AnsiString;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*********************************************************}
procedure TALJSONNodeA.SetRegEx(const Pattern: AnsiString);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*********************************************************}
function TALJSONNodeA.GetRegExOptions: TALPerlRegExOptions;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*********************************************************************************************}
function TALJSONNodeA.GetRegExOptions(const Default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{***********************************************************************}
procedure TALJSONNodeA.SetRegExOptions(const Value: TALPerlRegExOptions);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeA.GetBinaryAsBytes: TBytes;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeA.GetBinaryAsBytes(const Default: TBytes): TBytes;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeA.GetBinaryAsStream: TStream;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeA.GetBinaryAsStream(const Default: TStream): TStream;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
procedure TALJSONNodeA.SetBinaryAsBytes(const Data: TBytes);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
procedure TALJSONNodeA.SetBinaryAsStream(const Data: TStream);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeA.GetOwnsBinaryStream: Boolean;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
procedure TALJSONNodeA.SetOwnsBinaryStream(const Value: Boolean);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{*******************************************}
function TALJSONNodeA.GetBinarySubType: Byte;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeA.GetBinarySubType(const Default: Byte): Byte;
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{***********************************************************}
procedure TALJSONNodeA.SetBinarySubType(const Subtype: Byte);
begin
  AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
end;

{********************************************************************}
{Returns the JSON that corresponds to the subtree rooted at this node.
 GetJSON returns the JSON that corresponds to this node and any child nodes it contains.}
function TALJSONNodeA.GetJSON: AnsiString;
begin
  SaveToJSONString(result);
end;

{*************************************************}
{SetJSON reload the node with the new given value }
procedure TALJSONNodeA.SetJSON(const Value: AnsiString);
begin
  LoadFromJSONString(Value);
end;

{********************************************************************}
{Returns the BSON that corresponds to the subtree rooted at this node.
 GetBSON returns the BSON that corresponds to this node and any child nodes it contains.}
function TALJSONNodeA.GetBSON: AnsiString;
begin
  SaveToBSONString(result);
end;

{*************************************************}
{SetBSON reload the node with the new given value }
procedure TALJSONNodeA.SetBSON(const Value: AnsiString);
begin
  LoadFromBSONString(Value);
end;

{**********************************************************}
constructor TALJSONNodeA.Create(const NodeName: AnsiString);
begin
  FNodeName := NodeName;
end;

{********************************************************************************************************************************************}
function TALJSONNodeA.AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA;
begin
  Result := ALCreateJSONNodeA(NodeName,NodeType);
  Try
    ChildNodes.Insert(Index, Result);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{*************************************************************************************************************************************************}
function TALJSONNodeA.AddChild(const Path: array of AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I], TDirection.FromEnd);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  Result := LNode.addChild(path[high(path)], NodeType, Index);
end;

{****************************************************************************************************************}
function TALJSONNodeA.AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA;
begin
  Result := AddChild('', NodeType, Index);
end;

{*********************************************************************}
function TALJSONNodeA.DeleteChild(const NodeName: AnsiString): Boolean;
begin
  var I := ChildNodes.IndexOf(NodeName);
  if I >= 0 then begin
    ChildNodes.Delete(I);
    Result := True;
  end
  else Result := False;
end;

{**************************************************************************}
function TALJSONNodeA.DeleteChild(const Path: array of AnsiString): Boolean;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then exit(false)
    else LNode := LTmpNode;
  end;
  var I := LNode.ChildNodes.IndexOf(path[high(path)]);
  if I >= 0 then begin
    LNode.ChildNodes.Delete(I);
    Result := True;
  end
  else Result := False;
end;

{****************************************************************************************************}
function TALJSONNodeA.CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNodeA;
begin
  Result := ALCreateJSONNodeA(NodeName, NodeType);
end;

{****************************************}
function TALJSONNodeA.Clone: TALJSONNodeA;
begin
  case NodeType of
    ntObject: Result := TALJSONObjectNodeA.Create(NodeName);
    ntArray: Result := TALJSONArrayNodeA.Create(NodeName);
    ntText: begin
      Result := TALJSONTextNodeA.Create(NodeName);
      var LDestNode := TALJSONTextNodeA(Result);
      var LSrcNode := TALJSONTextNodeA(Self);
      LDestNode.FNodeSubType := LSrcNode.FNodeSubType;
      LDestNode.FStorageKind := LSrcNode.FStorageKind;
      LDestNode.FBinarySubType := LSrcNode.FBinarySubType;
      LDestNode.FRegExOptions := LSrcNode.FRegExOptions;
      case LDestNode.FStorageKind of
        skString: PAnsiString(@LDestNode.FNodeValue)^ := PAnsiString(@LSrcNode.FNodeValue)^;
        skBytes: PBytes(@LDestNode.FNodeValue)^ := PBytes(@LSrcNode.FNodeValue)^;
        skOwnedStream: begin
          var LDstStream := TMemoryStream.Create;
          try
            var LSrcStream := TStream(Pointer(NativeInt(LSrcNode.FNodeValue)));
            LDstStream.CopyFrom(LSrcStream);
            LDestNode.FNodeValue := System.Int64(Pointer(LDstStream));
          except
            ALFreeAndNil(LDstStream);
            raise;
          end;
        end
        else
          LDestNode.FNodeValue := LSrcNode.FNodeValue;
      end;
    end;
    else
      AlJSONDocErrorA(ALJSONInvalidNodeType);
  end;
  if HasChildNodes then
    for var I := 0 to ChildNodes.Count - 1 do
      Result.ChildNodes.Add(ChildNodes[I].Clone);
end;

{*******************************}
procedure TALJSONNodeA.ParseJSON(
            const RawJSONStream: TStream;
            const RawJSONString: AnsiString;
            const SaxMode: Boolean;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions);

Const
  BufferSize: Integer = 32768;

Var
  Buffer: AnsiString;
  BufferLength: Integer;
  BufferPos: Integer;
  CurrName: AnsiString;
  CurrIndex: Integer;
  CurrValue: AnsiString;
  NotSaxMode: Boolean;
  WorkingNode: TALJSONNodeA;
  NamePaths: TALNVStringListA;
  ObjectPaths: TList<TPair<Integer, TALJSONNodeA>>;
  DecodeJSONReferences: Boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer: Boolean; overload;
  begin
    if not assigned(RawJSONStream) then begin
      Result := false;
      exit;
    end;

    var Byte2Read: Integer;
    If (BufferLength > 0) and (BufferPos > 1) then begin
      if (BufferPos > BufferLength) then RawJSONStream.Position := RawJSONStream.Position - BufferLength + BufferPos - 1;
      Byte2Read := min(BufferPos - 1, BufferLength);
      if BufferPos <= length(Buffer) then ALMove(
                                            PByte(Buffer)[BufferPos - 1],
                                            pointer(Buffer)^,
                                            BufferLength-BufferPos+1);
      BufferPos := 1;
    end
    else begin
      Byte2Read := BufferSize;
      BufferLength := BufferLength + BufferSize;
      SetLength(Buffer, BufferLength);
    end;

    //range check error is we not do so
    var ByteReaded: Integer;
    if RawJSONStream.Position < RawJSONStream.Size then ByteReaded := RawJSONStream.Read(PByte(Buffer)[BufferLength - Byte2Read{+ 1 - 1}],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      BufferLength := BufferLength - Byte2Read + ByteReaded;
      SetLength(Buffer, BufferLength);
      Result := ByteReaded > 0;
    end
    else Result := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer(var PosToKeepSync: Integer): Boolean; overload;
  begin
    var P1 := BufferPos;
    Result := ExpandBuffer;
    PosToKeepSync := PosToKeepSync - (P1 - BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(const ExtraItems: AnsiString = ''): AnsiString;
  begin
    var LB := ALDefaultJsonPathSeparatorA;
    var Size: Integer := length(ExtraItems);
    if size <> 0 then Inc(Size, 1{length(LB)});
    for var I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths.Names[I]) + 1{length(LB)});
    SetLength(Result, Size);
    var P: Integer := 1;
    for var I := 1 to NamePaths.Count - 1 do begin
      var S := NamePaths.Names[I];
      var L: Integer := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, PByte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
        Inc(P, L);
      end;
      L := 1{length(LB)};
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNodeA(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJSONNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, PByte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      var L: Integer := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, PByte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
      Inc(P, L);
    end;
    setlength(result,P-1);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(
              const index: Integer;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr('[' + ALIntToStrA(index) + ']'), '', Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithName(
              const name: AnsiString;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr(Name), Name, Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseText(
              const Index: Integer;
              const Name: AnsiString;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then begin
      if notSaxMode then begin
        if WorkingNode.nodetype=ntarray then _DoParseTextWithIndex(Index, Args, NodeSubType)
        else _DoParseTextWithName(Name, Args, NodeSubType);
      end
      else begin
        if NamePaths.Count = 0 then AlJSONDocErrorA(ALJSONParseError);
        if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntArray then _DoParseTextWithIndex(Index, Args, NodeSubType)
        else _DoParseTextWithName(Name, Args, NodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartObject(const Name: AnsiString);
  begin
    if Assigned(OnParseStartObject) then OnParseStartObject(Self, GetPathStr, Name);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndObject;
  begin
    if Assigned(OnParseEndObject) then begin
      if NamePaths.Count = 0 then AlJSONDocErrorA(ALJSONParseError);
      OnParseEndObject(Self, GetPathStr, NamePaths.Names[NamePaths.Count - 1]);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: AnsiString);
  begin
    if Assigned(OnParseStartArray) then OnParseStartArray(Self, GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if Assigned(OnParseEndArray) then begin
      if NamePaths.Count = 0 then AlJSONDocErrorA(ALJSONParseError);
      OnParseEndArray(Self, GetPathStr, NamePaths.Names[NamePaths.Count - 1]);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexItemToNamePath(const index: Integer; Obj: Pointer);
  begin
    var S1: AnsiString;
    setlength(S1,sizeOf(Integer) {div sizeOF(ansiChar)}); // off course sizeOf(Integer) must be a multiple of sizeOf(ansiChar) but it's always the case
    ALmove(index, pointer(S1)^, sizeOf(Integer));
    NamePaths.AddNameValueObject('[' + ALIntToStrA(Index) + ']', S1, Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddNameItemToNamePath(const name: AnsiString; Obj: Pointer);
  begin
    NamePaths.AddNameValueObject(Name, #$ff#$ff#$ff#$ff, Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddItemToNamePath(index: Integer; const name: AnsiString; Obj: Pointer);
  begin
    if notSaxMode then begin
      if WorkingNode.nodetype=ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end
    else begin
      if NamePaths.Count = 0 then AlJSONDocErrorA(ALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt64Node(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    var LInt64: System.Int64;
    if ALJSONTryStrToInt64A(value, LInt64) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetInt64(LInt64);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LInt64], nstInt64)
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt32Node(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    var LInt32: System.Int32;
    if ALJSONTryStrToInt32A(value, LInt32) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetInt32(LInt32);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LInt32], nstInt32)
      end
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTextNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    Result := true;
    if NotSaxMode then begin
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(index, Name, [value], nstText)
    end
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createFloatNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    var LDouble: Double;
    if ALTryStrToFloat(value, LDouble) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetFloat(LDouble);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LDouble], nstFloat)
      end
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBinaryNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    var LBinData: TBytes;
    var LBinSubtype: Byte;
    if ALJSONTryStrToBinaryA(value, LBinData, LBinSubtype) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetBinaryAsBytes(LBinData);
          LNode.SetBinarySubType(LBinSubtype);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LBinData, LBinSubtype], nstBinary);
      end
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createObjectIDNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    var LObjectID: TBytes;
    if ALJSONTryStrToObjectIDA(value, LObjectID) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetObjectID(LObjectID);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LObjectID], nstObjectID)
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBooleanNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    var LBool: Boolean;
    if value = 'true' then LBool := true
    else if value = 'false' then LBool := false
    else begin
      Result := False;
      exit;
    end;
    Result := true;
    if NotSaxMode then begin
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setbool(LBool);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(index, Name, [LBool], nstBoolean);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createDateTimeNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    var LDateTime: TDateTime;
    if ALJSONTryStrToDateTimeA(value, LDateTime) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setdatetime(LDateTime);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LDateTime], nstDateTime);
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTimestampNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    var LTimestamp: TALBSONTimestamp;
    if ALJSONTryStrToTimestampA(value, LTimestamp) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetTimestamp(LTimestamp);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LTimestamp.W1, LTimestamp.W2], nstTimeStamp);
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createnullNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    if value = 'null' then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setnull(true);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, ['null'], nstNull);
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createRegExNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    var LRegEx: AnsiString;
    var LRegExOptions: TALPerlRegExOptions;
    if ALJSONTryStrToRegExA(value, LRegEx, LRegExOptions) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetRegEx(LRegEx);
          LNode.SetRegExOptions(LRegExOptions);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LRegEx, Byte(LRegExOptions)], nstRegEx)
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createJavascriptNode(index: Integer; const name: AnsiString; const Value: AnsiString): Boolean;
  begin
    Result := true;
    if NotSaxMode then begin
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(index, Name, [value], nstJavascript);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createNode(index: Integer; const name: AnsiString; const Value: AnsiString; AQuotedValue: Boolean);
  begin
    if AQuotedValue then begin
      _createTextNode(index, Name, Value);
      exit;
    end;
    if _createFloatNode(index, Name, Value) then exit;  // << we have the same problem as javascript, if we put here a big number like (by exemple) 9223372036854775808
                                                        // << then the stored value will be different because of double precision that is less than Int64 precision
                                                        // << it's the way javascript json work, it's have no room for int / Int64 :(
                                                        // << if we want to have the possibility to store Int64 precision then we must use node subtype helper
                                                        // << like NumberLong(9223372036854775808)
    if _createBooleanNode(index, Name, Value) then exit;
    if _createNullNode(index, Name, Value) then exit;
    if _createInt32Node(index, Name, Value) then exit;
    if _createInt64Node(index, Name, Value) then exit;
    if _createDateTimeNode(index, Name, Value) then exit;
    if _createBinaryNode(index, Name, Value) then exit;
    if _createObjectIDNode(index, Name, Value) then exit;
    if _createRegExNode(index, Name, Value) then exit;
    if _createTimeStampNode(index, Name, Value) then exit;
    _createJavascriptNode(index, Name, Value);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _extractLastIndexFromNamePath: Integer;
  begin
    if NamePaths.Count = 0 then AlJSONDocErrorA(ALJSONParseError);
    ALMove(pointer(namePaths.ValueFromIndex[namepaths.Count - 1])^,result,sizeOf(Integer));
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  begin

    {$REGION 'init current char (c)'}
    var c: ansiChar := Buffer[BufferPos];
    {$ENDREGION}

    {$REGION 'end Object/Array'}
    // ... } ....
    // ... ] ....
    if c in ['}',']'] then begin // ... } ...
                                 //     ^BufferPos

      //Reset the CurrIndex
      CurrIndex := -1;

      //error if Paths.Count = 0 (mean one end object/array without any starting)
      if assigned(ObjectPaths) then begin
        if (ObjectPaths.Count = 0) then AlJSONDocErrorA(ALJSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then AlJSONDocErrorA(ALJSONParseError);
      end;

      // Declare LNodeType
      var LNodeType: TALJSONNodeType;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init LNode to one level up
        var LNode: TALJSONNodeA;
        if assigned(ObjectPaths) then LNode := ObjectPaths[ObjectPaths.Count - 1].Value
        else LNode := TALJSONNodeA(NamePaths.Objects[NamePaths.Count - 1]);

        //if LNode <> WorkingNode aie aie aie
        if (LNode <> WorkingNode) then AlJSONDocErrorA(ALJSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then AlJSONDocErrorA(ALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then AlJSONDocErrorA(ALJSONParseError);

        //if working node <> Self then we can go to one level up
        If WorkingNode <> Self then begin

          //update CurrIndex if WorkingNode.NodeType = ntArray
          if assigned(ObjectPaths) then begin
            WorkingNode := ObjectPaths[ObjectPaths.Count - 2].Value;
            if WorkingNode.NodeType = ntArray then CurrIndex := ObjectPaths[Objectpaths.Count - 1].Key + 1;
          end
          else begin
            WorkingNode := TALJSONNodeA(NamePaths.Objects[NamePaths.Count - 2]);
            if WorkingNode.NodeType = ntArray then CurrIndex := _extractLastIndexFromNamePath + 1;
          end;

        end

        //if working node = Self then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

         //calculate anodeTypeInt
        LNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then AlJSONDocErrorA(ALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then AlJSONDocErrorA(ALJSONParseError);

        //update CurrIndex if WorkingNode.NodeType = ntArray
        if (Namepaths.Count >= 2) and
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 2]) = ntarray) then CurrIndex := _extractLastIndexFromNamePath + 1;

        //call the DoParseEndObject/array event
        if LNodeType = ntObject then _DoParseEndObject
        else _DoParseEndArray;

      end;

      //delete the last entry from the path
      if assigned(ObjectPaths) then ObjectPaths.Delete(ObjectPaths.Count - 1)
      else NamePaths.Delete(NamePaths.Count - 1);

      //update BufferPos
      BufferPos := BufferPos + 1; // ... } ...
                                  //      ^BufferPos

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'begin Object/Array Without NAME'}
    // ... { ....
    // ... [ ....
    if c in ['{','['] then begin // ... { ...
                                 //     ^BufferPos

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then AlJSONDocErrorA(ALJSONParseError);

        //Node without name can be ONLY present inside an array node
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then AlJSONDocErrorA(ALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
        var LNode: TALJSONNodeA;
        if c = '{' then LNode := CreateNode('', ntObject)
        else LNode := CreateNode('', ntarray);
        try
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := LNode;

        //update the path
        if assigned(ObjectPaths) then ObjectPaths.Add(TPair<Integer, TALJSONNodeA>.create(CurrIndex, WorkingNode))
        else _AddItemToNamePath(CurrIndex, '', WorkingNode);

      end

      //if we are in sax mode
      else begin

          //Node without name can be ONLY present inside an array node
          if (CurrIndex < 0) or
             (NamePaths.Count = 0) or
             (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then AlJSONDocErrorA(ALJSONParseError);

        //update the path
        var LNodeType: TALJSONNodeType;
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrIndex, '', pointer(LNodeType));

        //call the DoParseEndObject/array event
        if LNodeType = ntObject then _DoParseStartObject('')
        else _DoParseStartArray('');

      end;

      //Update CurrIndex
      if c = '{' then CurrIndex := -1
      else CurrIndex := 0;

      //update BufferPos
      BufferPos := BufferPos + 1; // ... { ...
                                  //      ^BufferPos

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'extract the quoted name part'}
    // "" : ""
    // "name" : "value"
    // "name" : 1.23
    // "name" : true
    // "name" : false
    // "name" : null
    // "name" : ISODATE('1/1/2001')
    // "name" : function(){return(new Date).getTime()}, ...}
    // "name" : new Date(''Dec 03, 1924'')
    // "name" : { ... }
    // "name" : [ ... ]
    // 'name' : '...'
    // "value"
    // 'value'
    var LQuoteChar: AnsiChar := #0;
    if c in ['"',''''] then begin  // ... " ...
                                   //     ^BufferPos
      LQuoteChar := c; // "
      var P1 := BufferPos + 1; // ... "...\"..."
                               //      ^P1
      If P1 + 1 > BufferLength then ExpandBuffer(P1);
      While P1 <= BufferLength do begin

       c := Buffer[P1];

       If (c = '\') and
          (P1 < BufferLength) and
          (Buffer[P1 + 1] in ['\', LQuoteChar]) then inc(p1, 2) // ... "...\"..."
                                                                //         ^^^P1
       else if c = LQuoteChar then begin
         ALCopyStr(Buffer,CurrName,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALJavascriptDecodeInPlace(CurrName); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

       if P1 + 1 > BufferLength then ExpandBuffer(P1);

      end;
      if P1 > BufferLength then AlJSONDocErrorA(ALJSONParseError);
      BufferPos := P1 + 1; // ... "...\"..."
                           //      ^^^^^^^^^^BufferPos
    end
    {$ENDREGION}

    {$REGION 'extract the unquoted name part'}
    // name : "value"
    // name : 1.23
    // name : true
    // name : false
    // name : null
    // name : ISODATE('1/1/2001')
    // name : function(){return(new Date).getTime()}, ...}
    // name : new Date('Dec 03, 1924')
    // name : { ... }
    // name : [ ... ]
    // 1.23
    // true
    // false
    // null
    // ISODATE('1/1/2001')
    // function(){return(new Date).getTime()}, ...}
    // new Date('Dec 03, 1924')
    else begin

      var LInSingleQuote := False;
      var LInDoubleQuote := False;
      var LInSquareBracket := 0;
      var LInRoundBracket := 0;
      var LInCurlyBracket := 0;

      While (BufferPos <= BufferLength) or ExpandBuffer do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then AlJSONDocErrorA(ALJSONParseError);

      var P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
                           //     ^P1
      While (P1 <= BufferLength) or ExpandBuffer(P1) do begin

        c := Buffer[P1];

        if (not LInSingleQuote) and
           (not LInDoubleQuote) and
           (LInSquareBracket = 0) and
           (LInRoundBracket = 0) and
           (LInCurlyBracket = 0) and
           (c in [',', '}', ']', ':']) then begin
          var P2 := P1-1;
          While P2 >= BufferPos do begin
            If Buffer[P2] <= ' ' then dec(P2)
            else break;
          end;
          ALCopyStr(Buffer,CurrName,BufferPos,P2-BufferPos+1); // new Date('Dec 03, 1924')
          break;
        end
        else if (c = '"') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInDoubleQuote := (not LInDoubleQuote) and (not LInSingleQuote);
        end
        else if (c = '''') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInSingleQuote := (not LInSingleQuote) and (not LInDoubleQuote)
        end
        else if (not LInSingleQuote) and
                (not LInDoubleQuote) then begin
          if (c = '[') then inc(LInSquareBracket)
          else if (c = ']') then dec(LInSquareBracket)
          else if (c = '(') then inc(LInRoundBracket)
          else if (c = ')') then dec(LInRoundBracket)
          else if (c = '{') then inc(LInCurlyBracket)
          else if (c = '}') then dec(LInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then AlJSONDocErrorA(ALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos

    end;
    {$ENDREGION}

    {$REGION 'extract the name value separator part'}
    var LNameValueSeparator: ansiChar := #0;
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      If Buffer[BufferPos] <= ' ' then inc(BufferPos)
      else begin
        LNameValueSeparator := Buffer[BufferPos];
        break;
      end;
    end;
    if BufferPos > BufferLength then AlJSONDocErrorA(ALJSONParseError);  // .... : ....
                                                                          //      ^BufferPos
    {$ENDREGION}

    {$REGION 'if aNameValueSeparator is absent then it is just a value'}
    if LNameValueSeparator <> ':' then begin

      //Node without name can be ONLY present inside an array node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then AlJSONDocErrorA(ALJSONParseError);
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then AlJSONDocErrorA(ALJSONParseError);
      end
      else begin
        if (CurrIndex < 0) or
           (NamePaths.Count = 0) or
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then AlJSONDocErrorA(ALJSONParseError);
      end;

      //create the node
      _createNode(CurrIndex,'',CurrName,LQuoteChar in ['"','''']);

      //increase the CurrIndex
      inc(CurrIndex);

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'remove the blank space between the name valueeparator and the value'}
    inc(BufferPos); // ... : ....
                    //      ^BufferPos
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      If Buffer[BufferPos] <= ' ' then inc(BufferPos)
      else break;
    end;
    if BufferPos > BufferLength then AlJSONDocErrorA(ALJSONParseError); // .... " ....
                                                                         //      ^BufferPos
    {$ENDREGION}

    {$REGION 'init current char (c)'}
    c := Buffer[BufferPos];
    {$ENDREGION}

    {$REGION 'if the value is an object/array'}
    // name : { ... }
    // name : [ ... ]
    if c in ['{','['] then begin // ... { ...
                                 //     ^BufferPos

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then AlJSONDocErrorA(ALJSONParseError);

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0)  or
           (WorkingNode.nodetype <> ntObject) then AlJSONDocErrorA(ALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
        var LNode: TALJSONNodeA;
        if c = '{' then LNode := CreateNode(CurrName, ntObject)
        else LNode := CreateNode(CurrName, ntarray);
        try
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := LNode;

        //update the path
        if assigned(ObjectPaths) then ObjectPaths.Add(TPair<Integer, TALJSONNodeA>.create(-1, WorkingNode))
        else _AddItemToNamePath(-1, CurrName, WorkingNode);

      end

      //if we are in sax mode
      else begin

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0) or
           (NamePaths.Count = 0) or
           (TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) <> ntobject) then AlJSONDocErrorA(ALJSONParseError);

        //update the path
        var LNodeType: TALJSONNodeType;
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(-1, CurrName, pointer(LNodeType));

        //call the DoParseEndObject/array event
        if LNodeType = ntObject then _DoParseStartObject(CurrName)
        else _DoParseStartArray(CurrName);

      end;

      //update the CurrIndex if it's an array
      if c <> '{' then CurrIndex := 0;

      //update BufferPos
      BufferPos := BufferPos + 1; // ... { ...
                                  //      ^BufferPos

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'if the value is a quoted string'}
    // name : "value"
    // name : 'value'
    LQuoteChar := #0;
    if c in ['"',''''] then begin  // ... " ...
                                   //     ^BufferPos

      LQuoteChar := c; // "
      var P1 := BufferPos + 1; // ... "...\"..."
                               //      ^P1
      If P1 + 1 > BufferLength then ExpandBuffer(P1);
      While P1 <= BufferLength do begin

       c := Buffer[P1];

       If (c = '\') and
          (P1 < BufferLength) and
          (Buffer[P1 + 1] in ['\', LQuoteChar]) then inc(p1, 2) // ... "...\"..."
                                                                //         ^^^P1
       else if c = LQuoteChar then begin
         ALCopyStr(Buffer,currValue,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALJavascriptDecodeInPlace(currValue); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

       if P1 + 1 > BufferLength then ExpandBuffer(P1);

      end;
      if P1 > BufferLength then AlJSONDocErrorA(ALJSONParseError);
      BufferPos := P1 + 1; // ... "...\"..."
                           //      ^^^^^^^^^^BufferPos

    end
    {$ENDREGION}

    {$REGION 'if the value is a UNquoted string'}
    // name : 1.23
    // name : true
    // name : false
    // name : null
    // name : ISODATE('1/1/2001')
    // name : function(){return(new Date).getTime()}, ...}
    // name : new Date(''Dec 03, 1924'')
    // name : /test/i
    else begin

      var LInSingleQuote := False;
      var LInDoubleQuote := False;
      var LInSlashQuote := False;
      var LInSquareBracket := 0;
      var LInRoundBracket := 0;
      var LInCurlyBracket := 0;

      While (BufferPos <= BufferLength) or ExpandBuffer do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then AlJSONDocErrorA(ALJSONParseError);

      var P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
                           //     ^P1
      While (P1 <= BufferLength) or ExpandBuffer(P1) do begin

        c := Buffer[P1];

        if (not LInSingleQuote) and
           (not LInDoubleQuote) and
           (not LInSlashQuote) and
           (LInSquareBracket = 0) and
           (LInRoundBracket = 0) and
           (LInCurlyBracket = 0) and
           (c in [',', '}', ']']) then begin
          var P2 := P1-1;
          While P2 >= BufferPos do begin
            If Buffer[P2] <= ' ' then dec(P2)
            else break;
          end;
          ALCopyStr(Buffer,currValue,BufferPos,P2-BufferPos+1); // new Date('Dec 03, 1924')
          break;
        end
        else if (c = '"') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInDoubleQuote := (not LInDoubleQuote) and (not LInSingleQuote) and (not LInSlashQuote);
        end
        else if (c = '''') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInSingleQuote := (not LInSingleQuote) and (not LInDoubleQuote) and (not LInSlashQuote);
        end
        else if (c = '/') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInSlashQuote := (not LInSingleQuote) and (not LInDoubleQuote) and (not LInSlashQuote);
        end
        else if (not LInSingleQuote) and
                (not LInDoubleQuote) and
                (not LInSlashQuote) then begin
          if (c = '[') then inc(LInSquareBracket)
          else if (c = ']') then dec(LInSquareBracket)
          else if (c = '(') then inc(LInRoundBracket)
          else if (c = ')') then dec(LInRoundBracket)
          else if (c = '{') then inc(LInCurlyBracket)
          else if (c = '}') then dec(LInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then AlJSONDocErrorA(ALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos


    end;
    {$ENDREGION}

    {$REGION 'create the named text node'}

    //Node withe name MUST be ONLY present inside an object node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALJSONParseError);
      if (CurrIndex >= 0)  or
         (WorkingNode.nodetype <> ntObject) then AlJSONDocErrorA(ALJSONParseError);
    end
    else begin
      if (CurrIndex >= 0) or
         (NamePaths.Count = 0) or
         (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntObject) then AlJSONDocErrorA(ALJSONParseError);
    end;

    //create the node
    _createNode(currIndex,CurrName,CurrValue,LQuoteChar in ['"','''']);

    {$ENDREGION}

  end;

begin

  //clear the childnodes
  if poClearChildNodes in Options then ChildNodes.Clear;

  //init WorkingNode and NotSaxMode and DecodeJSONReferences
  WorkingNode := Self;
  NotSaxMode := not SaxMode;
  DecodeJSONReferences := not (poIgnoreControlCharacters in Options);

  //init ObjectPaths or NamePaths
  if (NotSaxMode) and
     (not assigned(OnParseText)) and
     (not assigned(OnParseStartObject)) and
     (not assigned(OnParseEndObject)) and
     (not assigned(OnParseStartArray)) and
     (not assigned(OnParseEndArray)) then begin
    ObjectPaths := TList<TPair<Integer, TALJSONNodeA>>.Create;
    NamePaths := nil;
  end
  else begin
    ObjectPaths := nil;
    NamePaths := TALNVStringListA.Create;
  end;
  Try

    //init Buffer
    if assigned(RawJSONStream) then begin
      Buffer := '';
      BufferLength := 0;
      BufferPos := 1;
      ExpandBuffer;
    end
    else begin
      Buffer := RawJSONString;
      BufferLength := length(RawJSONString);
      BufferPos := 1;
    end;

    //add first node in ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.Add(TPair<Integer, TALJSONNodeA>.create(-1, WorkingNode))
    else begin
      if NotSaxMode then _AddNameItemToNamePath('', WorkingNode)
      else _AddNameItemToNamePath('', pointer(NodeType));
    end;

    //skip the first {
    var BOMSequence: Integer := 0; // hide warnings
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      var c: ansiChar := Buffer[BufferPos];
      If c <= ' ' then inc(bufferPos)
      else if ((bufferPos = 1) and (c=#$EF)) then begin
        BOMSequence := 1;
        inc(bufferPos);
      end
      else if ((bufferPos = 2) and (BOMSequence=1) and (c=#$BB)) then begin
        BOMSequence := 2;
        inc(bufferPos);
      end
      else if ((bufferPos = 3) and (BOMSequence=2) and (c=#$BF)) then begin
        BOMSequence := 0;
        inc(bufferPos);
      end
      else begin
        if (c = '{') then begin
          if (Nodetype <> ntObject) then AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
          CurrIndex := -1;
          if not notSaxMode then _DoParseStartObject('');
        end
        else if (c = '[') then begin
          if (Nodetype <> ntArray) then AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
          CurrIndex := 0;
          if not notSaxMode then _DoParseStartArray('');
        end
        else AlJSONDocErrorA(ALJSONParseError);
        inc(bufferPos);
        break;
      end;
    end;

    //analyze all the nodes
    if poAllowComments in Options then begin
      var InCommentLine: Integer := 0;
      While (BufferPos <= BufferLength) or ExpandBuffer do begin
        var c: ansiChar := Buffer[BufferPos];
        If (InCommentLine = 0) and ((c <= ' ') or (c = ',')) then inc(bufferPos)
        else if (InCommentLine <= 1) and (c = '/')  then begin
          inc(InCommentLine);
          inc(bufferPos);
        end
        else if (InCommentLine = 2) then begin
          if ((c = #13) or (c = #10)) then InCommentLine := 0;
          inc(bufferPos);
        end
        else begin
          if InCommentLine = 1 then begin
            InCommentLine := 0;
            dec(BufferPos);
          end;
          AnalyzeNode;
        end;
      end;
    end
    else begin
      While (BufferPos <= BufferLength) or ExpandBuffer do begin
        var c: ansiChar := Buffer[BufferPos];
        If (c <= ' ') or (c = ',') then inc(bufferPos)
        else AnalyzeNode;
      end;
    end;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then AlJSONDocErrorA(ALJSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then AlJSONDocErrorA(ALJSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if NotSaxMode and (WorkingNode <> nil) then AlJSONDocErrorA(ALJSONParseError);

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ALFreeAndNil(ObjectPaths)
    else ALFreeAndNil(NamePaths);

  end;

end;

{*************************************************************}
{Last version of the spec: http://bsonspec.org/#/specification}
procedure TALJSONNodeA.ParseBSON(
            const RawBSONStream: TStream;
            const RawBSONBytes: TBytes;
            const SaxMode: Boolean;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions);

Const
  BufferSize: Integer = 32768;

Var
  Buffer: TBytes;
  BufferLength: Integer;
  BufferPos: Integer;
  CurrName: AnsiString;
  NotSaxMode: Boolean;
  BinaryAsPtrStream: Boolean;
  WorkingNode: TALJSONNodeA;
  NamePaths: TALStringListA;
  ObjectPaths: TObjectList<TALJSONNodeA>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer: Boolean; overload;
  begin
    if not assigned(RawBSONStream) then begin
      Result := false;
      exit;
    end;

    var Byte2Read: Integer;
    If (BufferLength > 0) and (BufferPos > 0) then begin
      if (BufferPos > BufferLength) then RawBSONStream.Position := RawBSONStream.Position - BufferLength + BufferPos;
      Byte2Read := min(BufferPos, BufferLength);
      if BufferPos < BufferLength then
        ALMove(
          PByte(Buffer)[BufferPos],
          pointer(Buffer)^,
          BufferLength-BufferPos);
      BufferPos := 0;
    end
    else begin
      Byte2Read := BufferSize;
      BufferLength := BufferLength + BufferSize;
      SetLength(Buffer, BufferLength);
    end;

    //range check error is we not do so
    var ByteReaded: Integer;
    if RawBSONStream.Position < RawBSONStream.Size then ByteReaded := RawBSONStream.Read(PByte(Buffer)[BufferLength - Byte2Read{+ 1 - 1}],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      BufferLength := BufferLength - Byte2Read + ByteReaded;
      SetLength(Buffer, BufferLength);
      Result := ByteReaded > 0;
    end
    else Result := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer(var PosToKeepSync: Integer): Boolean; overload;
  begin
    var P1 := BufferPos;
    Result := ExpandBuffer;
    PosToKeepSync := PosToKeepSync - (P1 - BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(const ExtraItems: AnsiString = ''): AnsiString;
  begin
    var LB: ansiChar := ALDefaultJsonPathSeparatorA;
    var Size: Integer := length(ExtraItems);
    if size <> 0 then Inc(Size, 1{length(LB)});
    for var I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths[I]) + 1{length(LB)});
    SetLength(Result, Size);
    var P: Integer := 1;
    for var I := 1 to NamePaths.Count - 1 do begin
      var S: AnsiString := NamePaths[I];
      var L: Integer := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, PByte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
        Inc(P, L);
      end;
      L := 1{length(LB)};
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNodeA(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJSONNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, PByte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      var L: Integer := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, PByte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
      Inc(P, L);
    end;
    setlength(result,P-1);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(
              const index: AnsiString;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr('[' + index + ']'), '', Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithName(
              const name: AnsiString;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr(Name), Name, Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseText(
              const NameOrIndex: AnsiString;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then begin
      if notSaxMode then begin
        if WorkingNode.nodetype=ntarray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
      end
      else begin
        if NamePaths.Count = 0 then AlJSONDocErrorA(ALJSONParseError);
        if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntArray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartObject(const Name: AnsiString);
  begin
    if Assigned(OnParseStartObject) then OnParseStartObject(Self, GetPathStr, Name);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndObject;
  begin
    if Assigned(OnParseEndObject) then begin
      if NamePaths.Count = 0 then AlJSONDocErrorA(ALJSONParseError);
      OnParseEndObject(Self, GetPathStr, NamePaths[NamePaths.Count - 1]);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: AnsiString);
  begin
    if Assigned(OnParseStartArray) then OnParseStartArray(Self, GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if Assigned(OnParseEndArray) then begin
      if NamePaths.Count = 0 then AlJSONDocErrorA(ALJSONParseError);
      OnParseEndArray(Self, GetPathStr, NamePaths[NamePaths.Count - 1]);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexItemToNamePath(const index: AnsiString; Obj: Pointer);
  begin
    NamePaths.AddObject('[' + Index + ']', Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddNameItemToNamePath(const name: AnsiString; Obj: Pointer);
  begin
    NamePaths.AddObject(Name, Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddItemToNamePath(const nameOrIndex: AnsiString; Obj: Pointer);
  begin
    if notSaxMode then begin
      if WorkingNode.nodetype=ntarray then _AddIndexItemToNamePath(nameOrIndex, Obj)
      else _AddNameItemToNamePath(nameOrIndex, Obj);
    end
    else begin
      if NamePaths.Count = 0 then AlJSONDocErrorA(ALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(nameOrIndex, Obj)
      else _AddNameItemToNamePath(nameOrIndex, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt64Node(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt64: System.Int64;
    if BufferPos > BufferLength - sizeof(LInt64) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) then AlJSONDocErrorA(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt64, sizeof(LInt64));
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetInt64(LInt64);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LInt64], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt32Node(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt32: System.Int32;
    if BufferPos > BufferLength - sizeof(LInt32) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) then AlJSONDocErrorA(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetInt32(LInt32);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LInt32], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTextNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt32: System.Int32;
    if BufferPos > BufferLength - sizeof(LInt32) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) then AlJSONDocErrorA(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    while (BufferPos + LInt32 > BufferLength) do
      if not ExpandBuffer then AlJSONDocErrorA(ALBSONParseError);
    var LText: AnsiString;
    ALCopyStr(Buffer,LText,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(LText);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LText], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createFloatNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LDouble: Double;
    if BufferPos > BufferLength - sizeof(Double) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(Double) then AlJSONDocErrorA(ALBSONParseError);
    end;
    ALMove(pByte(Buffer)[BufferPos], LDouble, sizeof(Double));
    BufferPos := BufferPos + sizeof(Double);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetFloat(LDouble);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LDouble], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBinaryNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    //Get size
    var LInt32: System.Int32;
    if BufferPos > BufferLength - sizeof(LInt32) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) then AlJSONDocErrorA(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    //Get the subtype
    if BufferPos >= BufferLength then begin
      ExpandBuffer;
      if BufferPos >= BufferLength then AlJSONDocErrorA(ALBSONParseError);
    end;
    var LBinSubtype: Byte := Buffer[BufferPos];
    BufferPos := BufferPos + 1;

    //BinaryAsStream
    if BinaryAsPtrStream then begin

      //Get the data
      var LPointerStream: TPointerStream;
      if RawBSONStream <> nil then begin
        if RawBSONStream.Position - BufferLength + BufferPos + LInt32 > RawBSONStream.Size then AlJSONDocErrorA(ALBSONParseError);
        LPointerStream := TPointerStream.Create(
                            @PByte(TCustomMemoryStream(RawBSONStream).Memory)[RawBSONStream.Position - BufferLength + BufferPos], //Ptr: Pointer;
                            LInt32, // const Size: NativeInt;
                            true); // ReadOnly: Boolean)
      end
      else begin
        if BufferPos + LInt32 > BufferLength then AlJSONDocErrorA(ALBSONParseError);
        LPointerStream := TPointerStream.Create(
                            @PByte(RawBSONBytes)[BufferPos], //Ptr: Pointer;
                            LInt32, // const Size: NativeInt;
                            true); // ReadOnly: Boolean)
      end;
      BufferPos := BufferPos + LInt32;

      //create the node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetBinaryAsStream(LPointerStream);
          LNode.SetBinarySubType(LBinSubtype);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(Name, [LPointerStream, LBinSubtype], NodeSubType);
      end;

    end

    //BinaryAsBytes
    else begin

      //Get the data
      while (BufferPos + LInt32 > BufferLength) do
        if not ExpandBuffer then AlJSONDocErrorA(ALBSONParseError);
      var LBinData: TBytes;
      setlength(LBinData, LInt32);
      if LInt32 > 0 then
        ALMove(PByte(Buffer)[BufferPos], pointer(LBinData)^, LInt32);
      BufferPos := BufferPos + LInt32;

      //create the node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
        var LNode: TALJSONNodeA;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetBinaryAsBytes(LBinData);
          LNode.SetBinarySubType(LBinSubtype);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(Name, [LBinData, LBinSubtype], NodeSubType);
      end;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createObjectIDNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if BufferPos > BufferLength - 12{length(aObjectID)} then begin
      ExpandBuffer;
      if BufferPos > BufferLength - 12{length(aObjectID)} then AlJSONDocErrorA(ALBSONParseError);
    end;
    var LObjectID: TBytes;
    Setlength(LObjectID, 12); // ObjectId is a 12-Byte BSON type
    ALMove(PByte(Buffer)[BufferPos], pByte(LObjectID)[0], 12{length(aObjectID)});
    BufferPos := BufferPos + 12{length(aObjectID)};

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetObjectID(LObjectID);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LObjectID], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBooleanNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if BufferPos >= BufferLength then begin
      ExpandBuffer;
      if BufferPos >= BufferLength then AlJSONDocErrorA(ALBSONParseError);
    end;
    var LBool: Boolean;
    if Buffer[BufferPos] = $00 then LBool := False
    else if Buffer[BufferPos] = $01 then LBool := true
    else begin
      AlJSONDocErrorA(ALBSONParseError);
      LBool := False; // to hide a warning;
    end;
    BufferPos := BufferPos + 1;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setbool(LBool);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LBool], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createDateTimeNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt64: System.Int64;
    if BufferPos > BufferLength - sizeof(LInt64) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) then AlJSONDocErrorA(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt64, sizeof(LInt64));
    var LDateTime: TDateTime := ALUnixMsToDateTime(LInt64);
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setdatetime(LDateTime);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LDateTime], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTimestampNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt64: System.Int64;
    if BufferPos > BufferLength - sizeof(LInt64) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) then AlJSONDocErrorA(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt64, sizeof(LInt64));
    var LTimestamp: TALBSONTimestamp;
    LTimestamp.I64 := LInt64;
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetTimestamp(LTimestamp);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LTimestamp.W1, LTimestamp.W2], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createnullNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setnull(true);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, ['null'], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createRegExNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    //Get pattern
    var LRegEx: AnsiString;
    var P1: Integer := BufferPos;
    While (P1 < BufferLength) or ExpandBuffer(P1) do begin
      If Buffer[P1] <> $00 then inc(P1)
      else begin
        LRegEx := AlCopyStr(Buffer, BufferPos, P1 - BufferPos);
        break;
      end;
    end;
    if P1 >= BufferLength then AlJSONDocErrorA(ALBSONParseError);
    BufferPos := P1 + 1;
    if BufferPos >= BufferLength then ExpandBuffer;

    //Get options
    var LRegExOptions: TALPerlRegExOptions := [];
    While (BufferPos < BufferLength) or ExpandBuffer do begin
      case Buffer[BufferPos] of
        ord('i'): LRegExOptions := LRegExOptions + [preCaseLess];
        ord('m'): LRegExOptions := LRegExOptions + [preMultiLine];
        ord('x'): LRegExOptions := LRegExOptions + [preExtended];
        ord('l'):;
        ord('s'): LRegExOptions := LRegExOptions + [preSingleLine];
        ord('u'):;
        $00: break;
      end;
      inc(BufferPos);
    end;
    if BufferPos >= BufferLength then AlJSONDocErrorA(ALBSONParseError);
    inc(BufferPos);
    if BufferPos >= BufferLength then ExpandBuffer;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetRegEx(LRegEx);
        LNode.SetRegExOptions(LRegExOptions);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LRegEx, Byte(LRegExOptions)], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createJavascriptNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt32: System.Int32;
    if BufferPos > BufferLength - sizeof(LInt32) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) then AlJSONDocErrorA(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    while (BufferPos + LInt32 > BufferLength) do
      if not ExpandBuffer then AlJSONDocErrorA(ALBSONParseError);
    var LJavascript: AnsiString;
    ALCopyStr(Buffer,LJavascript,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);
      var LNode: TALJSONNodeA;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(LJavascript);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LJavascript], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  begin

    {$REGION 'init current char (c)'}
    var c: Byte := Buffer[BufferPos];
    {$ENDREGION}

    {$REGION 'End Object/Array'}
    // ... } ....
    // ... ] ....
    if c = $00 then begin

      //error if Paths.Count = 0 (mean one end object/array without any starting)
      if assigned(ObjectPaths) then begin
        if (ObjectPaths.Count = 0) then AlJSONDocErrorA(ALBSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then AlJSONDocErrorA(ALBSONParseError);
      end;

      // Declare LNodeType
      var LNodeType: TALJSONNodeType;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init LNode to one level up
        var LNode: TALJSONNodeA;
        if assigned(ObjectPaths) then LNode := ObjectPaths[ObjectPaths.Count - 1]
        else LNode := TALJSONNodeA(NamePaths.Objects[NamePaths.Count - 1]);

        //if LNode <> WorkingNode aie aie aie
        if (LNode <> WorkingNode) then AlJSONDocErrorA(ALBSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then AlJSONDocErrorA(ALBSONParseError);

        //if working node <> Self then we can go to one level up
        If WorkingNode <> Self then begin

          //init WorkingNode to the parentNode
          if assigned(ObjectPaths) then WorkingNode := ObjectPaths[ObjectPaths.Count - 2]
          else WorkingNode := TALJSONNodeA(NamePaths.Objects[NamePaths.Count - 2]);

        end

        //if working node = Self then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

        //calculate anodeTypeInt
        LNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then AlJSONDocErrorA(ALBSONParseError);

        //call the DoParseEndObject/array event
        if LNodeType = ntObject then _DoParseEndObject
        else _DoParseEndArray;

      end;

      //delete the last entry from the path
      if assigned(ObjectPaths) then ObjectPaths.Delete(ObjectPaths.Count - 1)
      else NamePaths.Delete(NamePaths.Count - 1);

      //update BufferPos
      BufferPos := BufferPos + 1;

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'Get the node sub type'}
    var LNodeSubType: TALJSONNodeSubType;
    case c of
      $01: LNodeSubType := nstFloat;
      $02: LNodeSubType := nstText;
      $03: LNodeSubType := nstObject;
      $04: LNodeSubType := nstArray;
      $05: LNodeSubType := nstbinary;
      $07: LNodeSubType := nstObjectID;
      $08: LNodeSubType := nstBoolean;
      $09: LNodeSubType := nstDateTime;
      $0A: LNodeSubType := nstNull;
      $0B: LNodeSubType := nstRegEx;
      $0D: LNodeSubType := nstJavascript;
      $10: LNodeSubType := nstInt32;
      $11: LNodeSubType := nstTimestamp;
      $12: LNodeSubType := nstInt64;
      else AlJSONDocErrorA(ALBSONParseError);
    end;
    BufferPos := BufferPos + 1;
    If BufferPos >= BufferLength then ExpandBuffer;
    {$ENDREGION}

    {$REGION 'Get the node name'}
    var P1: Integer := BufferPos;
    While (P1 < BufferLength) or ExpandBuffer(P1) do begin
      If Buffer[P1] <> $00 then inc(P1)
      else begin
        AlCopyStr(Buffer, CurrName, BufferPos, P1-BufferPos);
        break;
      end;
    end;
    if P1 >= BufferLength then AlJSONDocErrorA(ALBSONParseError);
    BufferPos := P1 + 1;
    if BufferPos >= BufferLength then ExpandBuffer;
    {$ENDREGION}

    {$REGION 'begin Object/Array'}
    // ... { ....
    // ... [ ....
    if LNodeSubType in [nstObject,nstArray] then begin

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then AlJSONDocErrorA(ALBSONParseError);

        //create the node according the the braket char and add it to the workingnode
        var LNode: TALJSONNodeA;
        if LNodeSubType = nstObject then begin
          if WorkingNode.nodetype=ntarray then LNode := CreateNode('', ntObject)
          else LNode := CreateNode(CurrName, ntObject);
        end
        else begin
          if WorkingNode.nodetype=ntarray then LNode := CreateNode('', ntarray)
          else LNode := CreateNode(CurrName, ntarray);
        end;
        try
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := LNode;

        //update the path
        if assigned(ObjectPaths) then ObjectPaths.Add(WorkingNode)
        else _AddItemToNamePath(CurrName, WorkingNode);

      end

      //if we are in sax mode
      else begin

        //update the path
        var LNodeType: TALJSONNodeType;
        if LNodeSubType = nstObject then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrName, pointer(LNodeType));

        //call the DoParseStartObject/array event
        if LNodeSubType = nstObject then _DoParseStartObject(CurrName)
        else _DoParseStartArray(CurrName);

      end;

      //update BufferPos
      BufferPos := BufferPos + 4; // we don't need the size of the object/array (4 Bytes)

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'create the node'}
    case LNodeSubType of
      // \x01 + name + \x00 + double
      nstFloat: _createFloatNode(CurrName, LNodeSubType);

      // \x02 + name + \x00 + length (Int32) + string + \x00
      nstText: _createTextNode(CurrName, LNodeSubType);

      // \x05 + name + \x00 + Int32 + subtype + (Byte*)
      nstbinary: _createBinaryNode(CurrName, LNodeSubType);

      // \x07 + name + \x00 + (Byte*12)
      nstObjectID: _createObjectIDNode(CurrName, LNodeSubType);

      // \x08 + name + \x00 + \x00 => Boolean "false"
      // \x08 + name + \x00 + \x01	=> Boolean "true"
      nstBoolean: _createBooleanNode(CurrName, LNodeSubType);

      // \x09 + name + \x00 + Int64
      nstDateTime: _createDateTimeNode(CurrName, LNodeSubType);

      // \x11 + name + \x00 + Int64
      nstTimestamp: _createTimestampNode(CurrName, LNodeSubType);

      // \x0A + name + \x00
      nstnull: _createNullNode(CurrName, LNodeSubType);

      // \x0B + name + \x00 + (Byte*) + \x00 + (Byte*) + \x00
      nstRegEx: _createRegExNode(CurrName, LNodeSubType);

      // \x0D + name + \x00 + length (Int32) + string + \x00
      nstJavascript: _createJavascriptNode(CurrName, LNodeSubType);

      // \x10 + name + \x00 + Int32
      nstInt32: _createInt32Node(CurrName, LNodeSubType);

      // \x12 + name + \x00 + Int64
      nstInt64: _createInt64Node(CurrName, LNodeSubType);

      else AlJSONDocErrorA(ALBSONParseError);
    end;
    {$ENDREGION}

  end;

begin

  //Only Object Node can be loaded from BSON
  If NodeType <> ntObject then AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
  if poClearChildNodes in Options then ChildNodes.Clear;
  BinaryAsPtrStream := poBinaryAsPtrStream in Options;
  if (BinaryAsPtrStream) and
     (RawBSONStream <> nil) and
     (RawBSONStream is not TCustomMemoryStream) then
    AlJSONDocErrorA(ALJSONBinaryAsPtrStreamInvalidSource);

  //init WorkingNode and NotSaxMode
  WorkingNode := Self;
  NotSaxMode := not SaxMode;

  //init ObjectPaths or NamePaths
  if (NotSaxMode) and
     (not assigned(OnParseText)) and
     (not assigned(OnParseStartObject)) and
     (not assigned(OnParseEndObject)) and
     (not assigned(OnParseStartArray)) and
     (not assigned(OnParseEndArray)) then begin
    ObjectPaths := TObjectList<TALJSONNodeA>.Create(false{OwnsObjects});
    NamePaths := nil;
  end
  else begin
    ObjectPaths := nil;
    NamePaths := TALStringListA.Create;
  end;
  Try

    //init Buffer
    if assigned(RawBSONStream) then begin
      Buffer := nil;
      BufferLength := 0;
      BufferPos := 4; // the first 4 Bytes are the length of the document and we don't need it
      ExpandBuffer;
    end
    else begin
      Buffer := RawBSONBytes;
      BufferLength := length(RawBSONBytes);
      BufferPos := 4; // the first 4 Bytes are the length of the document and we don't need it
    end;

    //add first node in ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.Add(WorkingNode)
    else begin
      if NotSaxMode then NamePaths.AddObject('', WorkingNode)
      else NamePaths.AddObject('', pointer(ntObject));
    end;
    if not notSaxMode then _DoParseStartObject('');

    //analyze all the nodes
    While (BufferPos < BufferLength) or ExpandBuffer do
      AnalyzeNode;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then AlJSONDocErrorA(ALBSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then AlJSONDocErrorA(ALBSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if NotSaxMode and (WorkingNode <> nil) then AlJSONDocErrorA(ALBSONParseError);

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ALFreeAndNil(ObjectPaths)
    else ALFreeAndNil(NamePaths);

  end;

end;

{********************************}
procedure TALJSONNodeA.SaveToJSON(
            const Stream: TStream;
            var Buffer: AnsiString;
            const Options: TALJSONSaveOptions);

type
  TNodeStackEntry = record
    Node: TALJSONNodeA;
    ParentNode: TALJSONNodeA;
  end;

Const
  BufferSize: Integer = 32768;

Var
  NodeStack: Tstack<TNodeStackEntry>;
  CurrentIndentStr: AnsiString;
  IndentStr: AnsiString;
  EncodeControlCharacters: Boolean;
  SkipNodeSubTypeHelper: Boolean;
  SaveInt64AsText: Boolean;
  AutoIndentNode: Boolean;
  BufferPos: Integer;
  LastWrittenChar: AnsiChar;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteBuffer2Stream(const buffer: AnsiString; BufferLength: Integer);
  begin
    if assigned(Stream) then begin
      If BufferLength > 0 then stream.Writebuffer(pointer(buffer)^,BufferLength);
      BufferPos := 0;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _Write2Buffer(const Source; Count: NativeInt);
  begin
    if Count = 0 then exit;
    if Count + BufferPos > length(Buffer) then setlength(Buffer, Count + BufferPos + BufferSize);
    ALMove(Source, pByte(Buffer)[BufferPos{*sizeOf(ansiChar)}], Count{*sizeOf(ansiChar)});
    BufferPos := BufferPos + Count;
    if BufferPos >= 65536 then _WriteBuffer2Stream(Buffer,BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStr2Buffer(const str:AnsiString);
  begin
    var L: Integer := Length(Str);
    if L = 0 then exit;
    LastWrittenChar := Str[L];
    _Write2Buffer(pointer(str)^,L);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteTextNode2Buffer(aTextNode:TALJSONNodeA; aParentNode: TALJSONNodeA);
  begin
    with aTextNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode then begin
        _WriteStr2Buffer(#13#10);
        _WriteStr2Buffer(CurrentIndentStr);
      end;

      if (assigned(AParentNode)) and
         (AParentNode.NodeType <> ntArray) then begin
        _WriteStr2Buffer('"');
        if EncodeControlCharacters then _WriteStr2Buffer(ALJavascriptEncode(NodeName))
        else _WriteStr2Buffer(NodeName);
        if AutoIndentNode then _WriteStr2Buffer('": ')
        else _WriteStr2Buffer('":');
      end;

      if (NodeSubType = NstText) or
         ((NodeSubType = nstInt64) and SaveInt64AsText) then begin
        if (NodeSubType = NstText) and EncodeControlCharacters then begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(ALJavascriptEncode(GetText));
          _WriteStr2Buffer('"');
        end
        else begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(Text);
          _WriteStr2Buffer('"');
        end;
      end
      else _WriteStr2Buffer(GetInterchangeValue(SkipNodeSubTypeHelper));

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNodeA; aParentNode: TALJSONNodeA);
  begin
    with aObjectNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode and (CurrentIndentStr <> '') then begin
         _WriteStr2Buffer(#13#10);
         _WriteStr2Buffer(CurrentIndentStr);
      end;

      if aObjectNode = self then _WriteStr2Buffer('{')
      else if (assigned(AParentNode)) and
              (AParentNode.NodeType <> ntArray) then begin
        _WriteStr2Buffer('"');
        if EncodeControlCharacters then _WriteStr2Buffer(ALJavascriptEncode(NodeName))
        else _WriteStr2Buffer(NodeName);
        if AutoIndentNode then _WriteStr2Buffer('": {')
        else _WriteStr2Buffer('":{');
      end
      else _WriteStr2Buffer('{');

      var LEmptyNode := True;
      var LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            var LNodeStackEntry: TNodeStackEntry;
            LNodeStackEntry.Node := aObjectNode;
            LNodeStackEntry.ParentNode := aObjectNode;
            NodeStack.Push(LNodeStackEntry);
            For var I := Count - 1 downto 0 do begin
              LNodeStackEntry.Node := Nodes[I];
              LNodeStackEntry.ParentNode := aObjectNode;
              NodeStack.Push(LNodeStackEntry);
            end;
          end
      end;

      If LEmptyNode then _WriteStr2Buffer('}')
      else CurrentIndentStr := CurrentIndentStr + IndentStr;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNodeA);
  begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr)+1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer('}');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNodeA; aParentNode: TALJSONNodeA);
  begin
    with aArrayNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode and (CurrentIndentStr <> '') then begin
        _WriteStr2Buffer(#13#10);
        _WriteStr2Buffer(CurrentIndentStr);
      end;

      if aArrayNode = self then _WriteStr2Buffer('[')
      else if (assigned(AParentNode)) and
              (AParentNode.NodeType <> ntArray) then begin
        _WriteStr2Buffer('"');
        if EncodeControlCharacters then _WriteStr2Buffer(ALJavascriptEncode(NodeName))
        else _WriteStr2Buffer(NodeName);
        if AutoIndentNode then _WriteStr2Buffer('": [')
        else _WriteStr2Buffer('":[');
      end
      else _WriteStr2Buffer('[');

      var LEmptyNode := True;
      var LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            var LNodeStackEntry: TNodeStackEntry;
            LNodeStackEntry.Node := aArrayNode;
            LNodeStackEntry.ParentNode := aArrayNode;
            NodeStack.Push(LNodeStackEntry);
            For var I := Count - 1 downto 0 do begin
              LNodeStackEntry.Node := Nodes[I];
              LNodeStackEntry.ParentNode := aArrayNode;
              NodeStack.Push(LNodeStackEntry);
            end;
          end
      end;

      If LEmptyNode then _WriteStr2Buffer(']')
      else CurrentIndentStr := CurrentIndentStr + IndentStr;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNodeA);
  begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr) + 1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer(']');
  end;

begin
  If not (NodeType in [ntObject, ntArray]) then exit;
  NodeStack := Tstack<TNodeStackEntry>.Create;
  Try

    {init buffer string}
    Setlength(Buffer, BufferSize); // will make buffer uniquestring
    BufferPos := 0;
    LastWrittenChar := '{';
    EncodeControlCharacters := not (soIgnoreControlCharacters in Options);
    SkipNodeSubTypeHelper := soSkipNodeSubTypeHelper in Options;
    SaveInt64AsText := SkipNodeSubTypeHelper and (soSaveInt64AsText in Options);
    AutoIndentNode := soNodeAutoIndent in Options;
    IndentStr := ALDefaultJsonNodeIndentA;
    CurrentIndentStr := '';

    {SaveOnlyChildNode}
    var LNodeStackEntry: TNodeStackEntry;
    LNodeStackEntry.Node := self;
    LNodeStackEntry.ParentNode := nil;
    NodeStack.Push(LNodeStackEntry);

    {loop on all nodes}
    While NodeStack.Count > 0 Do begin
      LNodeStackEntry := NodeStack.Pop;
      with LNodeStackEntry.Node do
        case NodeType of
          ntObject: begin
                      if LNodeStackEntry.Node = LNodeStackEntry.ParentNode then _WriteEndObjectNode2Buffer(LNodeStackEntry.Node)
                      else _WriteStartObjectNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode);
                    end;
          ntArray: begin
                      if LNodeStackEntry.Node = LNodeStackEntry.ParentNode then _WriteEndArrayNode2Buffer(LNodeStackEntry.Node)
                      else _WriteStartArrayNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode);
                   end;
          ntText: _WriteTextNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode);
          else AlJSONDocErrorA(ALJSONInvalidNodeType);
        end;
    end;

    {Write the buffer}
    if assigned(Stream) then _WriteBuffer2Stream(Buffer, BufferPos)
    else setlength(Buffer,BufferPos);

  finally
    ALFreeAndNil(NodeStack);
  end;
end;

{***********************************}
{Saves the JSON document to a stream.
 Call SaveToStream to save the contents of the JSON document to the stream specified by Stream.}
procedure TALJSONNodeA.SaveToJSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
begin
  var Buffer: AnsiString;
  SaveToJSON(Stream, buffer, Options);
end;

{*******************************}
{Saves the JSON document to disk.
 Call SaveToFile to save any modifications you have made to the parsed JSON document.
 AFileName is the name of the file to save.}
procedure TALJSONNodeA.SaveToJSONFile(const FileName: String; const Options: TALJSONSaveOptions = []);
begin
  var LTmpFilename: String;
  if soProtectedSave in Options then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    var LFileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToJSONStream(LFileStream, Options);
    finally
      ALFreeAndNil(LFileStream);
    end;

    if LTmpFilename <> FileName then begin
      if TFile.Exists(FileName) then TFile.Delete(FileName);
      TFile.Move(LTmpFilename, FileName);
    end;

  except
    if (LTmpFilename <> FileName) and
       (TFile.Exists(LTmpFilename)) then TFile.Delete(LTmpFilename);
    raise;
  end;
end;

{********************************************************************************************************}
procedure TALJSONNodeA.SaveToJSONFile(const FileName: AnsiString; const Options: TALJSONSaveOptions = []);
begin
  SaveToJSONFile(String(FileName), Options);
end;

{*************************************************}
{Saves the JSON document to a string-type variable.
 Call SaveToJSON to save the contents of the JSON document to the string-type variable specified by JSON. SaveToJSON writes the contents of JSON document
 using 8 bits char (utf-8, iso-8859-1, etc) as an encoding system, depending on the type of the JSON parameter.
 Unlike the JSON property, which lets you write individual lines from the JSON document, SaveToJSON writes the entire text of the JSON document.}
procedure TALJSONNodeA.SaveToJSONString(var str: AnsiString; const Options: TALJSONSaveOptions = []);
begin
  SaveToJSON(nil, Str, Options);
end;

{********************************}
procedure TALJSONNodeA.SaveToBSON(
            const Stream: TStream;
            var Buffer: AnsiString;
            const Options: TALJSONSaveOptions);

type
  TNodeStackEntry = record
    Node: TALJSONNodeA;
    ParentNode: TALJSONNodeA;
    Index: Integer;
    StartPos: System.Int64;
  end;

Const
  BufferSize: Integer = 32768;

Var
  NodeStack: Tstack<TNodeStackEntry>;
  BufferPos: NativeInt;
  StreamPos: system.Int64;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteBuffer2Stream(const buffer: AnsiString; BufferLength: Integer);
  begin
    if assigned(Stream) then begin
      If BufferLength > 0 then stream.Writebuffer(pointer(buffer)^,BufferLength);
      BufferPos := 0;
      StreamPos := stream.Position;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _Write2Buffer(const Source; Count: NativeInt);
  begin
    if Count = 0 then exit;
    if Count + BufferPos > length(Buffer) then setlength(Buffer, Count + BufferPos + BufferSize);
    ALMove(Source, pByte(Buffer)[BufferPos], Count);
    BufferPos := BufferPos + Count;
    if BufferPos >= 65536 then _WriteBuffer2Stream(Buffer,BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStr2Buffer(const str:AnsiString); overload; inline;
  begin
    _Write2Buffer(pointer(str)^,length(Str));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStr2Buffer(const index:Integer); overload; inline;
  begin
    _WriteStr2Buffer(ALIntToStrA(index));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteByte2Buffer(const AByte:Byte); inline;
  begin
    _Write2Buffer(aByte, sizeOF(aByte));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteBytes2Buffer(const ABytes:TBytes); inline;
  begin
    _Write2Buffer(pointer(ABytes)^,length(ABytes));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteInt2Pos(const AInt:Integer; const APos: system.Int64);
  begin
    if aPos < StreamPos then begin
      Stream.position := aPos;
      stream.Writebuffer(aInt,sizeof(aInt));
      Stream.position := StreamPos;
    end
    else ALMove(aInt, Buffer[aPos - StreamPos + 1], sizeOf(aInt));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x01 + name + \x00 + double
  procedure _WriteFloatValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    var LDouble: Double := aTextNode.Float;
    _Write2Buffer(LDouble, sizeOf(LDouble));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x02 + name + \x00 + length (Int32) + string + \x00
  procedure _WriteTextValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    var LText: AnsiString := aTextNode.Text;
    var LInt32: system.Int32 := length(LText) + 1 {for the trailing #0};
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _WriteStr2Buffer(LText);
    _WriteByte2Buffer($00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x05 + name + \x00 + Int32 + subtype + (Byte*)
  procedure _WriteBinaryValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    var LBinary: TBytes := aTextNode.BinaryAsBytes;
    var LBinarySubType: Byte := aTextNode.BinarySubType;
    var LInt32: system.Int32 := length(LBinary);
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _Write2Buffer(LBinarySubType, sizeOF(LBinarySubType));
    _WriteBytes2Buffer(LBinary);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x07 + name + \x00 + (Byte*12)
  procedure _WriteObjectIDValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    _WriteBytes2Buffer(aTextNode.ObjectID);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x08 + name + \x00 + \x00 => Boolean "false"
  // \x08 + name + \x00 + \x01	=> Boolean "true"
  procedure _WriteBooleanValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    if not aTextNode.bool then _WriteByte2Buffer($00)
    else _WriteByte2Buffer($01);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x09 + name + \x00 + Int64
  procedure _WriteDateTimeValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    var LInt64: system.Int64 := ALDateTimeToUnixMs(aTextNode.DateTime);
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x11 + name + \x00 + Int64
  procedure _WriteTimestampValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    var LInt64: system.Int64 := aTextNode.Timestamp.I64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \xOB + name + \x00 + (Byte*) + \x00 + (Byte*) + \x00
  procedure _WriteRegExValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    var LRegExOptionsStr: AnsiString := '';
    var LRegExOptions: TALPerlRegExOptions := aTextNode.RegExOptions;
    if preCaseLess in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 'i';
    if preMultiLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'m';
    if preExtended in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'x';
    //'l':;
    if preSingleLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 's';
    //'u':;
    _WriteStr2Buffer(aTextNode.RegEx);
    _WriteByte2Buffer($00);
    _WriteStr2Buffer(LRegExOptionsStr);
    _WriteByte2Buffer($00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x0D + name + \x00 + length (Int32) + string + \x00
  procedure _WriteJavascriptValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    var LJavascript: AnsiString := aTextNode.Javascript;
    var LInt32: system.Int32 := length(LJavascript) + 1 {for the trailing #0};
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _WriteStr2Buffer(LJavascript);
    _WriteByte2Buffer($00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x10 + name + \x00 + Int32
  procedure _WriteInt32Value2Buffer(aTextNode:TALJSONNodeA);
  begin
    var LInt32: system.Int32 := aTextNode.Int32;
    _Write2Buffer(LInt32, sizeOf(LInt32));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x12 + name + \x00 + Int64
  procedure _WriteInt64Value2Buffer(aTextNode:TALJSONNodeA);
  begin
    var LInt64: system.Int64 := aTextNode.Int64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteTextNode2Buffer(aTextNode:TALJSONNodeA; aParentNode: TALJSONNodeA; aNodeIndex: Integer);
  begin
    with aTextNode do begin

      // write the node subtype
      case NodeSubType of
        // \x01 + name + \x00 + double
        nstFloat: _WriteByte2Buffer($01);
        // \x02 + name + \x00 + length (Int32) + string + \x00
        nstText: _WriteByte2Buffer($02);
        // \x05 + name + \x00 + Int32 + subtype + (Byte*)
        nstbinary: _WriteByte2Buffer($05);
        // \x07 + name + \x00 + (Byte*12)
        nstObjectID: _WriteByte2Buffer($07);
        // \x08 + name + \x00 + \x00 => Boolean "false"
        // \x08 + name + \x00 + \x01	=> Boolean "true"
        nstBoolean: _WriteByte2Buffer($08);
        // \x09 + name + \x00 + Int64
        nstDateTime: _WriteByte2Buffer($09);
        // \x11 + name + \x00 + Int64
        nstTimestamp: _WriteByte2Buffer($11);
        // \x0A + name + \x00
        nstNull: _WriteByte2Buffer($0A);
        // \xOB + name + \x00 + (Byte*) + \x00 + (Byte*) + \x00
        nstRegEx: _WriteByte2Buffer($0B);
        // \x0D + name + \x00 + length (Int32) + string + \x00
        nstJavascript: _WriteByte2Buffer($0D);
        // \x10 + name + \x00 + Int32
        nstInt32: _WriteByte2Buffer($10);
        // \x12 + name + \x00 + Int64
        nstInt64: _WriteByte2Buffer($12);
        else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
      end;

      // write the nodename
      if (assigned(AParentNode)) and
         (AParentNode.NodeType = ntArray) then _WriteStr2Buffer(aNodeIndex)
      else _WriteStr2Buffer(NodeName);
      _WriteByte2Buffer($00);

      // add the nodevalue to the buffer
      case NodeSubType of
        nstFloat: _WriteFloatValue2Buffer(aTextNode);
        nstText: _WriteTextValue2Buffer(aTextNode);
        nstbinary: _WritebinaryValue2Buffer(aTextNode);
        nstObjectID: _WriteObjectIDValue2Buffer(aTextNode);
        nstBoolean: _WriteBooleanValue2Buffer(aTextNode);
        nstDateTime: _WriteDateTimeValue2Buffer(aTextNode);
        nstTimestamp: _WriteTimestampValue2Buffer(aTextNode);
        nstNull:;
        nstRegEx: _WriteRegExValue2Buffer(aTextNode);
        nstJavascript: _WriteJavascriptValue2Buffer(aTextNode);
        nstInt32: _WriteInt32Value2Buffer(aTextNode);
        nstInt64: _WriteInt64Value2Buffer(aTextNode);
        else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNodeA; aParentNode: TALJSONNodeA; aNodeIndex: Integer);
  begin
    with aObjectNode do begin

      if aObjectNode = self then _WriteStr2Buffer(#$00#$00#$00#$00)
      else if (assigned(AParentNode)) and
              (AParentNode.NodeType = ntArray) then begin
        _WriteByte2Buffer($03);
        _WriteStr2Buffer(aNodeIndex);
        _WriteStr2Buffer(#$00#$00#$00#$00#$00);
      end
      else begin
        _WriteByte2Buffer($03);
        _WriteStr2Buffer(NodeName);
        _WriteStr2Buffer(#$00#$00#$00#$00#$00);
      end;

      var LPos: system.Int64 := StreamPos + BufferPos - 4{length of the #$00#$00#$00#$00};

      var LEmptyNode := True;
      var LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            var LNodeStackEntry: TNodeStackEntry;
            LNodeStackEntry.Node := aObjectNode;
            LNodeStackEntry.ParentNode := aObjectNode;
            LNodeStackEntry.Index := aNodeIndex;
            LNodeStackEntry.StartPos := LPos;
            NodeStack.Push(LNodeStackEntry);
            For var I := Count - 1 downto 0 do begin
              LNodeStackEntry.Node := Nodes[I];
              LNodeStackEntry.ParentNode := aObjectNode;
              LNodeStackEntry.Index := I;
              LNodeStackEntry.StartPos := -1;
              NodeStack.Push(LNodeStackEntry);
            end;
          end
      end;

      If LEmptyNode then begin
        _WriteByte2Buffer($00);
        _WriteInt2Pos(5{length of the object},LPos);
      end;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNodeA; aNodeStartPos: system.Int64);
  begin
    _WriteByte2Buffer($00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNodeA; aParentNode: TALJSONNodeA; aNodeIndex: Integer);
  begin
    with aArrayNode do begin

      if (assigned(AParentNode)) and
         (AParentNode.NodeType = ntArray) then begin
        _WriteByte2Buffer($04);
        _WriteStr2Buffer(aNodeIndex);
        _WriteStr2Buffer(#$00#$00#$00#$00#$00);
      end
      else begin
        _WriteByte2Buffer($04);
        _WriteStr2Buffer(NodeName);
        _WriteStr2Buffer(#$00#$00#$00#$00#$00);
      end;

      var LPos: system.Int64 := StreamPos + BufferPos - 4{length of the #$00+#$00+#$00+#$00};

      var LEmptyNode := True;
      var LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            var LNodeStackEntry: TNodeStackEntry;
            LNodeStackEntry.Node := aArrayNode;
            LNodeStackEntry.ParentNode := aArrayNode;
            LNodeStackEntry.Index := aNodeIndex;
            LNodeStackEntry.StartPos := LPos;
            NodeStack.Push(LNodeStackEntry);
            For var I := Count - 1 downto 0 do begin
              LNodeStackEntry.Node := Nodes[I];
              LNodeStackEntry.ParentNode := aArrayNode;
              LNodeStackEntry.Index := I;
              LNodeStackEntry.StartPos := -1;
              NodeStack.Push(LNodeStackEntry);
            end;
          end
      end;

      If LEmptyNode then begin
        _WriteByte2Buffer($00);
        _WriteInt2Pos(5{length of the object},LPos);
      end;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNodeA; aNodeStartPos: system.Int64);
  begin
    _WriteByte2Buffer($00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

begin
  If NodeType <> ntobject then exit;

  NodeStack := Tstack<TNodeStackEntry>.Create;
  Try

    {init buffer string}
    Setlength(Buffer, BufferSize); // will make buffer uniquestring
    BufferPos := 0;
    if assigned(Stream) then StreamPos := Stream.Position
    else StreamPos := 0;

    {SaveOnlyChildNode}
    var LNodeStackEntry: TNodeStackEntry;
    LNodeStackEntry.Node := self;
    LNodeStackEntry.ParentNode := Nil;
    LNodeStackEntry.Index := 0;
    LNodeStackEntry.StartPos := StreamPos;
    NodeStack.Push(LNodeStackEntry);

    {loop on all nodes}
    While NodeStack.Count > 0 Do begin
      LNodeStackEntry := NodeStack.Pop;
      with LNodeStackEntry.Node do
        case NodeType of
          ntObject: begin
                      if LNodeStackEntry.Node = LNodeStackEntry.ParentNode then _WriteEndObjectNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.StartPos)
                      else _WriteStartObjectNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode, LNodeStackEntry.Index);
                    end;
          ntArray: begin
                      if LNodeStackEntry.Node = LNodeStackEntry.ParentNode then _WriteEndArrayNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.StartPos)
                      else _WriteStartArrayNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode, LNodeStackEntry.Index);
                   end;
          ntText: _WriteTextNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode, LNodeStackEntry.Index);
          else AlJSONDocErrorA(ALJSONInvalidNodeType);
        end;
    end;

    {Write the buffer}
    if assigned(Stream) then _WriteBuffer2Stream(Buffer, BufferPos)
    else setlength(Buffer,BufferPos);

  finally
    ALFreeAndNil(NodeStack);
  end;
end;

{*****************************************************************************************************}
procedure TALJSONNodeA.SaveToBSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
begin
  var Buffer: AnsiString;
  SaveToBSON(Stream, buffer, Options);
end;

{****************************************************************************************************}
procedure TALJSONNodeA.SaveToBSONFile(const FileName: String; const Options: TALJSONSaveOptions = []);
begin
  var LTmpFilename: String;
  if soProtectedSave in Options then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    var LFileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToBSONStream(LFileStream, Options);
    finally
      ALFreeAndNil(LFileStream);
    end;

    if LTmpFilename <> FileName then begin
      if TFile.Exists(FileName) then TFile.Delete(FileName);
      TFile.Move(LTmpFilename, FileName);
    end;

  except
    if (LTmpFilename <> FileName) and
       (TFile.Exists(LTmpFilename)) then TFile.Delete(LTmpFilename);
    raise;
  end;
end;

{********************************************************************************************************}
procedure TALJSONNodeA.SaveToBSONFile(const FileName: AnsiString; const Options: TALJSONSaveOptions = []);
begin
  SaveToBSONFile(String(FileName), Options);
end;

{***************************************************************************************************}
procedure TALJSONNodeA.SaveToBSONString(var str: AnsiString; const Options: TALJSONSaveOptions = []);
begin
  SaveToBSON(nil, Str, Options);
end;

{*************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromJSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  Try
    ParseJSON(nil, Str, False{SaxMode}, nil{OnParseText}, nil{OnParseStartObject}, nil{OnParseEndObject}, nil{OnParseStartArray}, nil{OnParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  Try
    Stream.Position := 0;
    ParseJSON(Stream, '', False{SaxMode}, nil{OnParseText}, nil{OnParseStartObject}, nil{OnParseEndObject}, nil{OnParseStartArray}, nil{OnParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromJSONStream(LFileStream, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{****************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromJSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  LoadFromJSONFile(String(FileName), Options);
end;

{*************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromBSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  Try
    var LCodePage: Word;
    var LElemSize: Word;
    var LBytes: TBytes := ALConvertStringToBytes(Str, LCodePage, LElemSize);
    try
      ParseBSON(nil, LBytes, False{SaxMode}, nil{OnParseText}, nil{OnParseStartObject}, nil{OnParseEndObject}, nil{OnParseStartArray}, nil{OnParseEndArray}, Options);
    finally
      ALRevertBytesToString(LBytes, LCodePage, LElemSize);
    end;
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{**********************************************************************************************************************}
procedure TALJSONNodeA.LoadFromBSONBytes(const Bytes: TBytes; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  Try
    ParseBSON(nil, Bytes, False{SaxMode}, nil{OnParseText}, nil{OnParseStartObject}, nil{OnParseEndObject}, nil{OnParseStartArray}, nil{OnParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  Try
    Stream.Position := 0;
    ParseBSON(Stream, nil, False{SaxMode}, nil{OnParseText}, nil{OnParseStartObject}, nil{OnParseEndObject}, nil{OnParseStartArray}, nil{OnParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromBSONStream(LFileStream, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{****************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromBSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  LoadFromBSONFile(String(FileName), Options);
end;

{*************************************}
procedure TALJSONNodeA.ParseJSONString(
            const Str: AnsiString;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
begin
  ParseJSON(nil, Str, true{SaxMode}, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeA.ParseJSONStream(
            const Stream: TStream;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
begin
  Stream.Position := 0;
  ParseJSON(Stream, '', true{SaxMode}, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{***********************************}
procedure TALJSONNodeA.ParseJSONFile(
            const FileName: String;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseJSONStream(LFileStream, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{***********************************}
procedure TALJSONNodeA.ParseJSONFile(
            const FileName: AnsiString;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
begin
  ParseJSONFile(String(FileName), OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeA.ParseBSONString(
            const Str: AnsiString;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
begin
  Try
    var LCodePage: Word;
    var LElemSize: Word;
    var LBytes: TBytes := ALConvertStringToBytes(Str, LCodePage, LElemSize);
    try
      ParseBSON(nil, LBytes, true{SaxMode}, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
    finally
      ALRevertBytesToString(LBytes, LCodePage, LElemSize);
    end;
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{************************************}
procedure TALJSONNodeA.ParseBSONBytes(
            const Bytes: TBytes;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
begin
  ParseBSON(nil, Bytes, true{SaxMode}, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeA.ParseBSONStream(
            const Stream: TStream;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
begin
  Stream.Position := 0;
  ParseBSON(Stream, nil, true{SaxMode}, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{***********************************}
procedure TALJSONNodeA.ParseBSONFile(
            const FileName: String;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseBSONStream(LFileStream, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{***********************************}
procedure TALJSONNodeA.ParseBSONFile(
            const FileName: AnsiString;
            const OnParseText: TALJSONParseTextEventA;
            const OnParseStartObject: TALJSONParseObjectEventA;
            const OnParseEndObject: TALJSONParseObjectEventA;
            const OnParseStartArray: TALJSONParseArrayEventA;
            const OnParseEndArray: TALJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
begin
  ParseBSONFile(String(FileName), OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{*********************************************************************}
constructor TALJSONObjectNodeA.Create(const NodeName: AnsiString = '');
begin
  inherited create(NodeName);
  FChildNodes := CreateChildList;
end;

{************************************}
destructor TALJSONObjectNodeA.Destroy;
begin
  ALFreeAndNil(FChildNodes);
  inherited;
end;

{**********************************************************}
function TALJSONObjectNodeA.GetChildNodes: TALJSONNodeListA;
begin
  Result := FChildNodes;
end;

{*******************************************************}
function TALJSONObjectNodeA.GetNodeType: TALJSONNodeType;
begin
  Result := NtObject;
end;

{*************************************************************}
function TALJSONObjectNodeA.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := NstObject;
end;

{******************************************************************}
function TALJSONObjectNodeA.InternalGetChildNodes: TALJSONNodeListA;
begin
  Result := FChildNodes;
end;

{********************************************************************}
constructor TALJSONArrayNodeA.Create(const NodeName: AnsiString = '');
begin
  inherited create(NodeName);
  FChildNodes := CreateChildList;
end;

{***********************************}
destructor TALJSONArrayNodeA.Destroy;
begin
  ALFreeAndNil(FChildNodes);
  inherited;
end;

{*********************************************************}
function TALJSONArrayNodeA.GetChildNodes: TALJSONNodeListA;
begin
  Result := FChildNodes;
end;

{******************************************************}
function TALJSONArrayNodeA.GetNodeType: TALJSONNodeType;
begin
  Result := NtArray;
end;

{************************************************************}
function TALJSONArrayNodeA.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := NstArray;
end;

{*****************************************************************}
function TALJSONArrayNodeA.InternalGetChildNodes: TALJSONNodeListA;
begin
  Result := FChildNodes;
end;

{*******************************************************************}
constructor TALJSONTextNodeA.Create(const NodeName: AnsiString = '');
begin
  inherited create(NodeName);
  FNodeValue := 0;
  FNodeSubType := nstText;
  FStorageKind := TALJSONStorageKind.skString;
  FBinarySubType := 0;
  FRegExOptions := [];
end;

{**********************************}
destructor TALJSONTextNodeA.Destroy;
begin
  ClearNodeValue;
  inherited;
end;

{******************************************************************************}
// By default json (ie: javascript) treats all numbers as floating-point values.
// To let other system (ie: mongoDB) understand the type of the number
// we provide the helper functions NumberLong() to handle 64-bit Integers
// and NumberInt() to handle 32-bit Integers (and some others). theses helper functions are
// used when saving the json document.
function TALJSONTextNodeA.GetInterchangeValue(const SkipNodeSubTypeHelper: Boolean = False): AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetObjectID;
  begin
    if SkipNodeSubTypeHelper then Result := '"'+ALBinToHexA(ObjectID)+'"'
    else Result := 'ObjectId("'+ALBinToHexA(ObjectID)+'")';
  end;

  {~~~~~~~~~~~~~~~~~~~}
  procedure _GetBinary;
  begin
    if FStorageKind = skBytes then begin
      if SkipNodeSubTypeHelper then Result := '"'+ALBase64EncodeBytesA(GetBinaryAsBytes)+'"'
      else Result := 'BinData('+ALIntToStrA(BinarySubType)+', "'+ALBase64EncodeBytesA(GetBinaryAsBytes)+'")';
    end
    else begin
      if SkipNodeSubTypeHelper then Result := '"'+ALBase64EncodeBytesA(ALGetBytesFromStream(GetBinaryAsStream))+'"'
      else Result := 'BinData('+ALIntToStrA(BinarySubType)+', "'+ALBase64EncodeBytesA(ALGetBytesFromStream(GetBinaryAsStream))+'")';
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetDateTime;
  begin
    if SkipNodeSubTypeHelper then Result := ALFormatDateTimeA('''"''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z"''', DateTime)
    else Result := ALFormatDateTimeA('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', DateTime)
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _GetInt32;
  begin
    if SkipNodeSubTypeHelper then Result := text
    else Result := 'NumberInt(' + text + ')'
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _GetInt64;
  begin
    if SkipNodeSubTypeHelper then Result := text
    else Result := 'NumberLong(' + text + ')';
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _GetRegEx;
  begin
    var LRegExOptionsStr: AnsiString := '';
    var LRegExOptions := RegExOptions;
    if preCaseLess in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 'i';
    if preMultiLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'m';
    if preExtended in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'x';
    //'l':;
    if preSingleLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 's';
    //'u':;
    Result := '/'+regex+'/' + LRegExOptionsStr;
    if SkipNodeSubTypeHelper then Result := '"' + ALJavascriptEncode(result) + '"'
    else Result := ALJavascriptEncode(result);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetTimestamp;
  begin
    if SkipNodeSubTypeHelper then Result := '"Timestamp('+ALIntToStrA(GetTimeStamp.W1)+', '+ALIntToStrA(GetTimeStamp.W2)+')"'
    else Result := 'Timestamp('+ALIntToStrA(GetTimeStamp.W1)+', '+ALIntToStrA(GetTimeStamp.W2)+')';
  end;

begin
  case FNodeSubType of
    nstFloat:      Result := GetText;
    nstText:       Result := GetText;
    nstBinary:     _GetBinary;
    nstObjectID:   _GetObjectID;
    nstBoolean:    Result := GetText;
    nstDateTime:   _GetDateTime;
    nstJavascript: Result := GetJavascript;
    nstInt32:      _GetInt32;
    nstInt64:      _GetInt64;
    nstNull:       Result := GetText;
    nstRegEx:      _GetRegEx;
    nstTimestamp:  _GetTimestamp;
    else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
  end;
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONTextNodeA.GetText: AnsiString;
begin
  case FNodeSubType of
    nstFloat: ALFloatToStrA(GetFloat, Result);
    nstText: Result := PAnsiString(@FNodeValue)^;
    //nstObject: Error
    //nstArray: Error
    //nstBinary: Error;
    //nstObjectID: Error;
    nstBoolean: ALBoolToStrA(Result, getBool, 'true', 'false');
    nstDateTime: ALDateTimeToStrA(GetDateTime, Result);
    nstNull: Result := 'null';
    nstRegEx: Result := GetRegEx;
    nstJavascript: Result := GetJavascript;
    nstInt32: ALIntToStrA(GetInt32, Result);
    //nstTimestamp: Error;
    nstInt64: ALIntToStrA(GetInt64, Result);
    else AlJSONDocErrorA(ALJSONOperationError, GetNodeType);
  end;
end;

{*******************************************************************}
function TALJSONTextNodeA.GetText(const Default: AnsiString): AnsiString;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetText;
end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONTextNodeA.SetText(const Value: AnsiString);
begin
  ClearNodeValue;
  PAnsiString(@FNodeValue)^ := Value;
  FNodeSubType := nstText;
  FStorageKind := TALJSONStorageKind.skString;
end;

{*************************************}
function TALJSONTextNodeA.GetFloat: Double;
begin
  case FNodeSubType of
    nstFloat: PInt64(@result)^ := FNodeValue;
    nstInt32,
    nstInt64: Result := FNodeValue;
    else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
  end;
end;

{************************************************************}
function TALJSONTextNodeA.GetFloat(const Default: Double): Double;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetFloat;
end;

{***************************************************}
procedure TALJSONTextNodeA.SetFloat(const Value: Double);
begin
  ClearNodeValue;
  FNodeValue := PInt64(@Value)^;
  FNodeSubType := nstFloat;
end;

{*******************************************}
function TALJSONTextNodeA.GetDateTime: TDateTime;
begin
  if FNodeSubType = nstDateTime then PInt64(@result)^ := FNodeValue
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{*********************************************************************}
function TALJSONTextNodeA.GetDateTime(const Default: TDateTime): TDateTime;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetDateTime;
end;

{*********************************************************}
procedure TALJSONTextNodeA.SetDateTime(const Value: TDateTime);
begin
  ClearNodeValue;
  FNodeValue := PInt64(@Value)^;
  FNodeSubType := nstDateTime;
end;

{***************************************************}
function TALJSONTextNodeA.GetTimestamp: TALBSONTimestamp;
begin
  if FNodeSubType = nstTimestamp then Result.I64 := FNodeValue
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{************************************************************************************}
function TALJSONTextNodeA.GetTimestamp(const Default: TALBSONTimestamp): TALBSONTimestamp;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetTimestamp;
end;

{*****************************************************************}
procedure TALJSONTextNodeA.SetTimestamp(const Value: TALBSONTimestamp);
begin
  ClearNodeValue;
  FNodeValue := Value.I64;
  FNodeSubType := nstTimestamp;
end;

{********************************************}
function TALJSONTextNodeA.GetObjectID: TBytes;
begin
  if FNodeSubType = nstObjectID then Result := PBytes(@FNodeValue)^
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{***************************************************************}
function TALJSONTextNodeA.GetObjectID(const Default: TBytes): TBytes;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetObjectID;
end;

{**********************************************************}
procedure TALJSONTextNodeA.SetObjectID(const Value: TBytes);
begin
  if length(Value) <> 12 then AlJSONDocErrorA(ALJSONObjectIDInvalidSize);
  ClearNodeValue;
  PBytes(@FNodeValue)^ := Value;
  FNodeSubType := nstObjectID;
  FStorageKind := TALJSONStorageKind.skBytes;
end;

{**************************************}
function TALJSONTextNodeA.GetInt32: Integer;
begin
  case FNodeSubType of
    nstFloat: begin
                var LDouble: Double;
                PInt64(@LDouble)^ := FNodeValue;
                var LInt64: system.Int64 := trunc(LDouble);
                // https://stackoverflow.com/questions/41779801/single-double-and-precision
                // Only values that are in form m*2^e, where m and e are Integers can be stored in a floating point variable
                // so all Integer can be store in the form m*2^e (ie: m = m*2^0)
                // so we can compare aInt64 <> aDouble without the need of samevalue
                if (LInt64 <> LDouble) or
                   (LInt64 > system.Int32.MaxValue) or
                   (LInt64 < system.Int32.MinValue) then AlJSONDocErrorA(ALJSONInvalidNodeSubType);
                Result := LInt64;
              end;
    nstInt32: begin
                var LInt64: system.Int64 := FNodeValue;
                if (LInt64 > system.Int32.MaxValue) or
                   (LInt64 < system.Int32.MinValue) then AlJSONDocErrorA(ALJSONInvalidNodeSubType);
                Result := LInt64;
              end;
    nstInt64: Result := FNodeValue;
    else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
  end;
end;

{**************************************************************}
function TALJSONTextNodeA.GetInt32(const Default: Integer): Integer;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetInt32;
end;

{****************************************************}
procedure TALJSONTextNodeA.SetInt32(const Value: Integer);
begin
  ClearNodeValue;
  FNodeValue := Value;
  FNodeSubType := nstInt32;
end;

{************************************}
function TALJSONTextNodeA.GetInt64: Int64;
begin
  case FNodeSubType of
    nstFloat: begin
                var LDouble: Double;
                PInt64(@LDouble)^ := FNodeValue;
                Result := trunc(LDouble);
                // https://stackoverflow.com/questions/41779801/single-double-and-precision
                // Only values that are in form m*2^e, where m and e are Integers can be stored in a floating point variable
                // so all Integer can be store in the form m*2^e (ie: m = m*2^0)
                // so we can compare Result <> aDouble without the need of samevalue
                if Result <> LDouble then AlJSONDocErrorA(ALJSONInvalidNodeSubType);
              end;
    nstInt32,
    nstInt64: Result := FNodeValue;
    else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
  end;
end;

{**********************************************************}
function TALJSONTextNodeA.GetInt64(const Default: Int64): Int64;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetInt64;
end;

{**************************************************}
procedure TALJSONTextNodeA.SetInt64(const Value: Int64);
begin
  ClearNodeValue;
  FNodeValue := Value;
  FNodeSubType := nstInt64;
end;

{*************************************}
function TALJSONTextNodeA.GetBool: Boolean;
begin
  if FNodeSubType = nstBoolean then begin
    if FNodeValue = 0 then Result := False
    else Result := true;
  end
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{*************************************************************}
function TALJSONTextNodeA.GetBool(const Default: Boolean): Boolean;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetBool;
end;

{***************************************************}
procedure TALJSONTextNodeA.SetBool(const Value: Boolean);
begin
  ClearNodeValue;
  if Value then FNodeValue := 1
  else FNodeValue := 0;
  FNodeSubType := nstBoolean;
end;

{*************************************}
function TALJSONTextNodeA.GetNull: Boolean;
begin
  Result := FNodeSubType = nstNull;
end;

{***************************************************}
procedure TALJSONTextNodeA.SetNull(const Value: Boolean);
begin
  if Value then begin
    ClearNodeValue;
    FNodeValue := 0
  end
  else AlJSONDocErrorA('Only "true" is allowed for setNull property');
  FNodeSubType := nstNull;
end;

{**********************************************}
function TALJSONTextNodeA.GetJavascript: AnsiString;
begin
  if FNodeSubType = nstJavascript then Result := PAnsiString(@FNodeValue)^
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{*************************************************************************}
function TALJSONTextNodeA.GetJavascript(const Default: AnsiString): AnsiString;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetJavascript;
end;

{************************************************************}
procedure TALJSONTextNodeA.SetJavascript(const Value: AnsiString);
begin
  ClearNodeValue;
  PAnsiString(@FNodeValue)^ := Value;
  FNodeSubType := nstJavascript;
  FStorageKind := TALJSONStorageKind.skString;
end;

{*****************************************}
function TALJSONTextNodeA.GetRegEx: AnsiString;
begin
  if FNodeSubType = nstRegEx then Result := PAnsiString(@FNodeValue)^
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{********************************************************************}
function TALJSONTextNodeA.GetRegEx(const Default: AnsiString): AnsiString;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetRegEx;
end;

{*********************************************************}
procedure TALJSONTextNodeA.SetRegEx(const Pattern: AnsiString);
begin
  ClearNodeValue;
  PAnsiString(@FNodeValue)^ := Pattern;
  FNodeSubType := nstRegEx;
  FStorageKind := TALJSONStorageKind.skString;
  FRegExOptions := [];
end;

{*********************************************************}
function TALJSONTextNodeA.GetRegExOptions: TALPerlRegExOptions;
begin
  if FNodeSubType = nstRegEx then Result := FRegExOptions
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{*********************************************************************************************}
function TALJSONTextNodeA.GetRegExOptions(const Default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetRegExOptions;
end;

{***********************************************************************}
procedure TALJSONTextNodeA.SetRegExOptions(const Value: TALPerlRegExOptions);
begin
  if FNodeSubType = nstRegEx then FRegExOptions := Value
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
function TALJSONTextNodeA.GetBinaryAsBytes: TBytes;
begin
  if FNodeSubType = nstBinary then begin
    if FStorageKind <> skBytes then AlJSONDocErrorA(ALJSONBinaryNotStoredAsBytes);
    Result := PBytes(@FNodeValue)^;
  end
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
function TALJSONTextNodeA.GetBinaryAsBytes(const Default: TBytes): TBytes;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetBinaryAsBytes;
end;

{****************************************************************}
function TALJSONTextNodeA.GetBinaryAsStream: TStream;
begin
  if FNodeSubType = nstBinary then begin
    if FStorageKind not in [skOwnedStream, skBorrowedStream] then AlJSONDocErrorA(ALJSONBinaryNotStoredAsStream);
    Result := TStream(Pointer(NativeInt(FNodeValue)));
  end
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
function TALJSONTextNodeA.GetBinaryAsStream(const Default: TStream): TStream;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetBinaryAsStream;
end;

{****************************************************************}
procedure TALJSONTextNodeA.SetBinaryAsBytes(const Data: TBytes);
begin
  ClearNodeValue;
  PBytes(@FNodeValue)^ := Data;
  FNodeSubType := nstBinary;
  FStorageKind := TALJSONStorageKind.skBytes;
end;

{****************************************************************}
procedure TALJSONTextNodeA.SetBinaryAsStream(const Data: TStream);
begin
  ClearNodeValue;
  FNodeValue := System.Int64(Pointer(Data));
  FNodeSubType := nstBinary;
  FStorageKind := TALJSONStorageKind.skOwnedStream;
end;

{****************************************************************}
function TALJSONTextNodeA.GetOwnsBinaryStream: Boolean;
begin
  if FNodeSubType = nstBinary then Result := FStorageKind = TALJSONStorageKind.skOwnedStream
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
procedure TALJSONTextNodeA.SetOwnsBinaryStream(const Value: Boolean);
begin
  if FNodeSubType = nstBinary then begin
    if FStorageKind not in [skOwnedStream, skBorrowedStream] then AlJSONDocErrorA(ALJSONBinaryNotStoredAsStream);
    if Value then FStorageKind := skOwnedStream
    else FStorageKind := skBorrowedStream;
  end
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{*******************************************}
function TALJSONTextNodeA.GetBinarySubType: Byte;
begin
  if FNodeSubType = nstBinary then Result := FBinarySubType
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
function TALJSONTextNodeA.GetBinarySubType(const Default: Byte): Byte;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetBinarySubType;
end;

{***********************************************************}
procedure TALJSONTextNodeA.SetBinarySubType(const Subtype: Byte);
begin
  if FNodeSubType = nstBinary then FBinarySubType := Subtype
  else AlJSONDocErrorA(ALJSONInvalidNodeSubType);
end;

{*****************************************************}
function TALJSONTextNodeA.GetNodeType: TALJSONNodeType;
begin
  Result := NtText;
end;

{***********************************************************}
function TALJSONTextNodeA.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := FNodeSubType;
end;

{****************************************}
procedure TALJSONTextNodeA.ClearNodeValue;
begin
  case FStorageKind of
    skString: PAnsiString(@FNodeValue)^ := '';
    skBytes: PBytes(@FNodeValue)^ := nil;
    skOwnedStream: TStream(Pointer(NativeInt(FNodeValue))).Free;
  end;
  FStorageKind := skInt64;
end;

{**********************************}
constructor TALJSONNodeListA.Create;
begin
  FList:= nil;
  FCount:= 0;
  FCapacity := 0;
end;

{**********************************}
destructor TALJSONNodeListA.Destroy;
begin
  Clear;
  inherited;
end;

{***********************************************************************************}
{Locates the index for a node name in a sorted list and indicates whether a node name
 with that value already exists in the list. Use Find to obtain the index in a
 sorted list where the node name S should be added. If the node name S, or a node
 name that differs from S only in case, already exists in the list, Find returns true.
 If the list does not contain a node name that matches S, Find returns false.
 The index where S should go is returned in the Index parameter.
 The value of Index is zero-based, where the first node name has the index 0, the
 second node name has the index 1, and so on.
 Note: Only use Find with sorted lists. For unsorted lists, use the IndexOf method instead.
 Tip: If the node name string is not found (thus return value of Find is False) then Index
      is set to the index of the first node name in the list that sorts immediately before
      or after S.}
function TALJSONNodeListA.Find(const NodeName: AnsiString; var Index: Integer): Boolean;
begin
  Result := False;
  var L: Integer := 0;
  var H: Integer := FCount - 1;
  while L <= H do
  begin
    var I: Integer := (L + H) shr 1;
    var C: Integer := CompareNodeNames(FList[I].NodeName, NodeName);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
        Result := True;
    end;
  end;
  Index := L;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Node is the object node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALJSONNodeListA.IndexOf(const Node: TALJSONNodeA; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if FList[Result] = Node then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if FList[Result] = Node then Exit;
  end;
  Result := -1;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Name is the NodeName property of the node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALJSONNodeListA.IndexOf(const Name: AnsiString; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if ALSameStrA(FList[Result].NodeName, Name) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if ALSameStrA(FList[Result].NodeName, Name) then Exit;
  end;
  Result := -1;
end;

{*******************************************************************************************************************************}
function TALJSONNodeListA.IndexOfValue(const Value: AnsiString; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].Text = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].Text = Value) then Exit;
  end;
  Result := -1;
end;

{****************************************************************************************************************************}
function TALJSONNodeListA.IndexOfValue(const Value: Integer; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].Int32 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].Int32 = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************************************************************************************************}
function TALJSONNodeListA.IndexOfValue(const Value: Int64; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].Int64 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].Int64 = Value) then Exit;
  end;
  Result := -1;
end;

{***************************************************************************************************************************}
function TALJSONNodeListA.IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].float = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].float = Value) then Exit;
  end;
  Result := -1;
end;

{******************************************************************************************************************************}
function TALJSONNodeListA.IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].DateTime = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].DateTime = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************}
{Returns a specified node from the list.
 Call FindNode to access a particular node in the list.
 *NodeName is the node to access. It specifies the NodeName property of the desired node.
 FindNode returns the object of the node if it is in the list. If NodeName does not specify a node in the list,
 FindNode returns nil (Delphi) or NULL (C++).}
function TALJSONNodeListA.FindNode(const NodeName: AnsiString; const Direction: TDirection = TDirection.FromBeginning): TALJSONNodeA;
begin
  var Index := IndexOf(NodeName, Direction);
  if Index >= 0 then Result := Get(Index)
  else Result := nil;
end;

{**********************************}
{Returns the first node in the list.
Call First to access the first node in the list. If the list is empty, First raises an exception}
function TALJSONNodeListA.First: TALJSONNodeA;
begin
  if Count > 0 then Result := Get(0)
  else Result := nil;
end;

{*********************************}
{Returns the last node in the list.
 Call Last to access the last node in the list. If the list is empty, Last raises an exception.}
function TALJSONNodeListA.Last: TALJSONNodeA;
begin
  if Count > 0 then Result := Get(FCount - 1)
  else Result := nil;
end;

{***************************************************************************}
{Returns a node that appears a specified amount before or after another node.
 Call FindSibling to access the node whose position has a specified relationship to another node.
 *Node is a node in the list to use as a reference point.
 *Delta indicates where the desired node appears, relative to Node. If Delta is positive, FindSibling returns
  the node that appears Delta positions after Node. If Delta is negative, FindSibling returns a node that appears before Node.
 FindSibling returns the node that appears at the position offset by Delta, relative to the position of Node. If Delta
 specifies a position before the first node or after the last node in the list, FindSibling returns nil (Delphi) or NULL (C++).}
function TALJSONNodeListA.FindSibling(const Node: TALJSONNodeA; Delta: Integer): TALJSONNodeA;
begin
  var Index := IndexOf(Node) + Delta;
  if (Index >= 0) and (Index < FCount) then Result := Get(Index)
  else Result := nil;
end;

{************************************}
{Returns a specified node in the list.
 Call Get to retrieve a node from the list, given its index.
 *Index specifies the node to fetch, where 0 identifies the first node, 1 identifies the second node, and so on.
  Index should be less than the value of the Count property.}
function TALJSONNodeListA.Get(Index: Integer): TALJSONNodeA;
begin
  if (Index < 0) or (Index >= FCount) then AlJSONDocErrorA(ALJSONListIndexError, [Index]);
  Result := FList[Index];
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Index identify the desired node. 0 is the index of the first node,
  1 is the index of the second node, and so on}
function TALJSONNodeListA.GetNodeByIndex(const Index: Integer): TALJSONNodeA;
begin
  Result := Get(Index);
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Name identify the desired node. it is the NodeName property of a node in the list.
 If Name does not identify a node in the list, GetNode tries to create a new node with the name specified by
 Name. If it can’t create the new node, GetNode raises an exception.}
function TALJSONNodeListA.GetNodeByName(const Name: AnsiString): TALJSONNodeA;
begin
  Result := FindNode(Name);
  if not Assigned(Result) then AlJSONDocErrorA(ALJSONNodeNotFound, [Name]);
end;

{****************************************************************************}
function TALJSONNodeListA.CompareNodeNames(const S1, S2: AnsiString): Integer;
begin
  Result := ALCompareStrA(S1, S2)
end;

{*****************************************************************************************}
procedure TALJSONNodeListA.QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareA);
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        Exchange(L, R);
      break;
    end;
    var I: Integer := L;
    var J: Integer := R;
    var P: Integer := (L + R) shr 1;
    repeat
      while (I <> P) and (ACompare(Self, I, P) < 0) do Inc(I);
      while (J <> P) and (ACompare(Self, J, P) > 0) do Dec(J);
      if I <= J then
      begin
        if I <> J then
          Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if (J - L) > (R - I) then
    begin
      if I < R then
        QuickSort(I, R, ACompare);
      R := J;
    end
    else
    begin
      if L < J then
        QuickSort(L, J, ACompare);
      L := I;
    end;
  end;
end;

{************************************************************************************************}
function ALJSONNodeListCompareNodeNameA(List: TALJSONNodeListA; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareNodeNames(
              List[Index1].NodeName,
              List[Index2].NodeName);
end;

{******************************}
procedure TALJSONNodeListA.Sort;
begin
  CustomSort(ALJSONNodeListCompareNodeNameA);
end;

{**************************************************************************}
procedure TALJSONNodeListA.CustomSort(Compare: TALJSONNodeListSortCompareA);
begin
  if (FList <> nil) and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

{**************************************}
{Adds a new node to the end of the list.
 Call Add to add a node to the end of the list. Add returns the index of the node once it is added, where 0 is the index
 of the first node in the list, 1 is the index of the second node, and so on.
 *Node is the node to add to the list.}
function TALJSONNodeListA.Add(const Node: TALJSONNodeA): Integer;
begin
  Result := FCount;
  InternalInsert(Result, Node);
end;

{**********************************************************************************}
procedure TALJSONNodeListA.InternalInsert(Index: Integer; const Node: TALJSONNodeA);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    ALMove(
      FList[Index],
      FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  Pointer(FList[index]) := nil;
  FList[index] := Node;
  Inc(FCount);
end;

{********************************************************}
{Inserts a new node into a specified position in the list.
 Call Insert to add a node at the position specified by Index.
 *Index specifies where to insert the node, where 0 is the first position, 1 is second position, and so on. If Index does not
  specify a valid index, Insert raises an exception.
 *Node is the node to add to the list.}
procedure TALJSONNodeListA.Insert(Index: Integer; const Node: TALJSONNodeA);
begin
  if Index = -1 then Add(Node)
  else begin
    if (Index < 0) or (Index > FCount) then AlJSONDocErrorA(ALJSONListIndexError, [Index]);
    InternalInsert(Index, Node);
  end;
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Index identifies the node to remove by index rather than name. Index ranges from 0 to one less than the value of the Count property.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Index Delete returns –1.}
function TALJSONNodeListA.Delete(const Index: Integer): Integer;
begin
  var LNode := Get(Index);
  FList[Index] := nil; // to decrease the refcount of Node
  Dec(FCount);
  if Index < FCount then begin
    ALMove(
      FList[Index + 1],
      FList[Index],
      (FCount - Index) * SizeOf(Pointer));
    Pointer(FList[FCount]) := nil;
  end;
  if assigned(LNode) then ALFreeAndNil(LNode);
  Result := Index;
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Name identifies the node to remove from the list. This is the local name of the node to remove.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Name, Delete returns –1.}
function TALJSONNodeListA.Delete(const Name: AnsiString): Integer;
begin
  Result := indexOf(Name);
  if Result >= 0 then Delete(Result);
end;

{**************************************}
{Removes a specified node from the list.
 Remove removes the specified node from the list.
 *Node is the node to remove from the list.
 Remove returns the index of Node before it was removed. If node is not a node in the list, Remove returns -1.}
function TALJSONNodeListA.Remove(const Node: TALJSONNodeA): Integer;
begin
  Result := IndexOf(Node);
  if Result >= 0 then Delete(Result);
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeListA.Extract(const Node: TALJSONNodeA): TALJSONNodeA;
begin
  Result := nil;
  var I := IndexOf(Node);
  if I >= 0 then Result := Extract(i);
end;

{***********************************************************}
procedure TALJSONNodeListA.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then AlJSONDocErrorA(ALJSONListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then AlJSONDocErrorA(ALJSONListIndexError, [Index2]);
  var Item := pointer(FList[Index1]);
  pointer(FList[Index1]) := pointer(FList[Index2]);
  pointer(FList[Index2]) := Item;
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeListA.Extract(const index: Integer): TALJSONNodeA;
begin
  Result := Get(index);
  FList[index] := nil;
  Delete(index);
end;

{*********************************************}
{Replaces a node in the list with another node.
 Call ReplaceNode to replace the node specified by OldNode with the node specified by NewNode.
 *OldNode is the node to replace. If OldNode does not appear in the list, then ReplaceNode adds the new node to the end of the list.
 *NewNode is the node to add to the list in place of OldNode.
 ReplaceNode returns OldNode (even if OldNode did not appear in the list).}
function TALJSONNodeListA.ReplaceNode(const OldNode, NewNode: TALJSONNodeA): TALJSONNodeA;
begin
  var Index := indexOf(OldNode);
  Result := Extract(Index);
  Insert(Index, NewNode);
end;

{*******************************}
{Removes all nodes from the list.
 Call Clear to empty the list.
 Note:	Clear does not call the beginUpdate and EndUpdate methods, even though it may Result in the
 deletion of more than one node.}
procedure TALJSONNodeListA.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{******************************}
procedure TALJSONNodeListA.Grow;
begin
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
end;

{***********************************************************}
procedure TALJSONNodeListA.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) then AlJSONDocErrorA(ALJSONListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*****************************************************}
procedure TALJSONNodeListA.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) then AlJSONDocErrorA(ALJSONListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for var I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{**************************}
procedure ALJSONToTStringsA(
            const AJsonStr: AnsiString;
            const AFormatSettings: TALFormatSettingsA;
            const APath: AnsiString;
            const ALst: TALStringsA;
            const ANullStr: AnsiString = 'null';
            const ATrueStr: AnsiString = 'true';
            const AFalseStr: AnsiString = 'false');
begin
  var LContainChilds := False;
  TALJSONDocumentA.ParseJSONString(
    AJsonStr,
    //--
    procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
    begin
      if (NodeSubType = nstFloat) then          aLst.Add(aPath + Path + aLst.NameValueSeparator + ALFloatToStrA(Args[0].VExtended^, aFormatSettings))
      else if (NodeSubType = nstDateTime) then  aLst.Add(aPath + Path + aLst.NameValueSeparator + ALDateTimeToStrA(Args[0].VExtended^, aFormatSettings))
      else if (NodeSubType = nstBoolean) then   aLst.Add(aPath + Path + aLst.NameValueSeparator + ALBoolToStrA(Args[0].VBoolean,aTrueStr,aFalseStr))
      else if (NodeSubType = nstnull) then      aLst.Add(aPath + Path + aLst.NameValueSeparator + aNullStr)
      else                                      aLst.Add(aPath + Path + aLst.NameValueSeparator + AnsiString(Args[0].VAnsiString));
      LContainChilds := True;
    end{OnParseText},
    //--
    procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
    begin
      LContainChilds := False;
    end{OnParseStartObject},
    //--
    procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
    begin
      if (not LContainChilds) and (aPath + Path <> ''{Path = '' mean it's the root object}) then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '{}');
      LContainChilds := True;
    end{OnParseEndObject},
    //--
    procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
    begin
      LContainChilds := False;
    end{OnParseStartArray},
    //--
    procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
    begin
      if not LContainChilds then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '[]');
      LContainChilds := True;
    end{OnParseEndArray});
end;

{**************************}
procedure ALJSONToTStringsA(
            const AJsonStr: AnsiString;
            const AFormatSettings: TALFormatSettingsA;
            const ALst: TALStringsA;
            const ANullStr: AnsiString = 'null';
            const ATrueStr: AnsiString = 'true';
            const AFalseStr: AnsiString = 'false');
begin
  ALJSONToTStringsA(
    AJsonStr,
    aFormatSettings,
    '',
    aLst,
    aNullStr,
    aTrueStr,
    aFalseStr);
end;

{**************************}
procedure ALJSONToTStringsA(
            const AJsonNode: TALJSONNodeA;
            const APath: AnsiString;
            const ALst: TALStringsA;
            const ANullStr: AnsiString = 'null';
            const ATrueStr: AnsiString = 'true';
            const AFalseStr: AnsiString = 'false');
begin
  if aJsonNode.ChildNodes.Count > 0 then begin
    for var I := 0 to aJsonNode.ChildNodes.Count - 1 do begin

      var LTmpPath: AnsiString;
      if aJsonNode.NodeType = ntArray then LTmpPath := aPath + '[' + ALIntToStrA(I) + ']'
      else begin
        if aJsonNode.ChildNodes[I].NodeName = '' then AlJSONDocErrorA(ALJSONNodeNameCanNotBeEmpty);
        LTmpPath := aPath + alIfThenA(aPath <> '', '.', '') + aJsonNode.ChildNodes[I].NodeName;
      end;

      case aJsonNode.ChildNodes[I].NodeType of

        ntObject: ALJSONToTStringsA(
                    aJsonNode.ChildNodes[I],
                    LTmpPath,
                    aLst,
                    aNullStr,
                    aTrueStr,
                    aFalseStr);

        ntArray: ALJSONToTStringsA(
                   aJsonNode.ChildNodes[I],
                   LTmpPath,
                   aLst,
                   aNullStr,
                   aTrueStr,
                   aFalseStr);

        ntText: begin
                  if (aJsonNode.ChildNodes[I].NodeSubType = nstBoolean) then   aLst.Add(LTmpPath + aLst.NameValueSeparator + ALBoolToStrA(aJsonNode.ChildNodes[I].Bool,aTrueStr,aFalseStr))
                  else if (aJsonNode.ChildNodes[I].NodeSubType = nstnull) then aLst.Add(LTmpPath + aLst.NameValueSeparator + aNullStr)
                  else                                                         aLst.Add(LTmpPath + aLst.NameValueSeparator + aJsonNode.ChildNodes[I].Text);
                end;

        else AlJSONDocErrorA(ALJSONInvalidNodeType);

      end;
    end;
  end
  else if (aPath <> ''{aPath = '' mean it's the root object}) then begin
    if      aJsonNode.NodeType = ntArray  then aLst.Add(aPath + aLst.NameValueSeparator + '[]')
    else if aJsonNode.NodeType = ntObject then aLst.Add(aPath + aLst.NameValueSeparator + '{}');
  end;
end;

{**************************}
procedure ALJSONToTStringsA(
            const AJsonNode: TALJSONNodeA;
            const ALst: TALStringsA;
            const ANullStr: AnsiString = 'null';
            const ATrueStr: AnsiString = 'true';
            const AFalseStr: AnsiString = 'false');
begin
  ALJSONToTStringsA(
    aJsonNode,
    '',
    aLst,
    aNullStr,
    aTrueStr,
    aFalseStr)
end;

{**************************}
procedure ALTStringsToJsonA(
            const ALst: TALStringsA;
            const AJsonNode: TALJSONNodeA;
            const APath: AnsiString = '';
            const ANameToLowerCase: Boolean = false;
            const ANullStr: AnsiString = 'null');
begin

  // create list of the part of name,
  // from "aggregated_data.properties.types[3].translations.usa" =>
  //   aggregated_data
  //   properties
  //   types
  //   [3]
  //   translations
  //   usa
  var LNames := TALStringListA.Create;
  try

    //init aNames.linebreak
    LNames.LineBreak := '.';

    // scroll the aLst
    for var I := 0 to aLst.Count - 1 do begin

      //if it's contain path
      if (aPath = '') or
         (ALPosIgnoreCaseA(aPath + '.',aLst.Names[I]) = 1) then begin

        // path.aggregated_data.properties.types[3].translations.usa =>
        //   aggregated_data
        //   properties
        //   types
        //   [3]
        //   translations
        //   usa
        if (aPath <> '') then LNames.Text := ALStringReplaceA(
                                               ALStringReplaceA(
                                                 aLst.Names[I],
                                                 aPath + '.',
                                                 '',
                                                 [rfIgnoreCase]),
                                               '[',
                                               '.[',
                                               [rfReplaceAll])
        else LNames.Text := ALStringReplaceA(
                              aLst.Names[I],
                              '[',
                              '.[',
                              [rfReplaceAll]);

        //loop on all the name
        var LCurrJsonNode := aJsonNode;
        for var J := 0 to LNames.Count - 1 do begin

          //if we are in array
          if LCurrJsonNode.NodeType = ntArray then begin
            var LIndex: Integer;
            if (length(LNames[J]) <= 2) or
               (LNames[J][1] <> '[') or
               (LNames[J][length(LNames[J])] <> ']') or
               (not ALTryStrToInt(ALCopyStr(LNames[J], 2, Length(LNames[J]) - 2), LIndex)) then raise EALException.CreateFmt('Wrong path: "%s"', [aLst.Names[I]]);
            while LIndex > LCurrJsonNode.ChildNodes.Count - 1 do begin
              if J = LNames.Count - 1 then LCurrJsonNode.AddChild(ntText)
              else if (LNames[J+1] <> '') and
                      (LNames[J+1][1] = '[') then LCurrJsonNode.AddChild(ntarray)
              else LCurrJsonNode.AddChild(ntObject);
            end;
            LCurrJsonNode := LCurrJsonNode.ChildNodes[LIndex];
          end

          //if we are not in array
          else begin
            var LLowerName := alifThenA(aNameToLowerCase, allowercase(LNames[J]), LNames[J]);
            var LTmpJsonNode := LCurrJsonNode.ChildNodes.FindNode(LLowerName);
            if not assigned(LTmpJsonNode) then begin
              if J = LNames.Count - 1 then LCurrJsonNode := LCurrJsonNode.AddChild(LLowerName, ntText)
              else if (LNames[J+1] <> '') and
                      (LNames[J+1][1] = '[') then LCurrJsonNode := LCurrJsonNode.AddChild(LLowerName, ntarray)
              else LCurrJsonNode := LCurrJsonNode.AddChild(LLowerName, ntObject);
            end
            else LCurrJsonNode := LTmpJsonNode;
          end;

          //set the value
          if J = LNames.Count - 1 then begin
            if aLst.ValueFromIndex[I] = aNullStr then LCurrJsonNode.Null := true
            else LCurrJsonNode.Text := aLst.ValueFromIndex[I];
          end;

        end;

      end;

    end;

  finally
    ALFreeAndNil(LNames);
  end;

end;

{*********************************************************************************}
function ALJsonEncodeFloatWithNodeSubTypeHelperA(const AValue: double): AnsiString;
begin
  Result := ALFloatToStrA(aValue);
end;

{************************************************************************************}
function ALJsonEncodeTextWithNodeSubTypeHelperA(const AValue: AnsiString): AnsiString;
begin
  Result := '"'+ALJavascriptEncode(aValue)+'"';
end;

{**********************************************************************************}
function ALJsonEncodeBinaryWithNodeSubTypeHelperA(const AValue: TBytes): AnsiString;
begin
  Result := 'BinData(0, "' + ALBase64EncodeBytesA(aValue) + '")';
end;

{****************************************************************************************}
function ALJsonEncodeObjectIDWithNodeSubTypeHelperA(const AValue: TBytes): AnsiString;
begin
  Result := 'ObjectId("'+ALBinToHexA(aValue)+'")';
end;

{************************************************************************************}
function ALJsonEncodeBooleanWithNodeSubTypeHelperA(const AValue: Boolean): AnsiString;
begin
  if aValue then Result := 'true'
  else Result := 'false';
end;

{******************************************************************}
function ALJsonEncodeDateTimeA(const AValue: TDateTime): AnsiString;
begin
  Result := ALFormatDateTimeA('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''', aValue);
end;

{***************************************************************************************}
function ALJsonEncodeDateTimeWithNodeSubTypeHelperA(const AValue: TDateTime): AnsiString;
begin
  Result := ALFormatDateTimeA('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', aValue);
end;

{******************************************************************************************}
function ALJsonEncodeJavascriptWithNodeSubTypeHelperA(const AValue: AnsiString): AnsiString;
begin
  Result := aValue;
end;

{********************************************************************************}
function ALJsonEncodeInt64WithNodeSubTypeHelperA(const AValue: Int64): AnsiString;
begin
  Result := 'NumberLong(' + ALIntToStrA(aValue) + ')';
end;

{********************************************************************************}
function ALJsonEncodeInt32WithNodeSubTypeHelperA(const AValue: Int32): AnsiString;
begin
  Result := 'NumberInt(' + ALIntToStrA(aValue) + ')';
end;

{**********************************************************}
function ALJsonEncodeNullWithNodeSubTypeHelperA: AnsiString;
begin
  Result := 'null';
end;

{****************************************}
function ALFindJsonNodeByChildValueInt32W(
           const AJsonNode: TALJSONNodeW;
           const AChildName: String;
           const AChildValue : Int32;
           const ARecurse: Boolean = False): TALJSONNodeW;
begin
  Result := nil;
  if not (AJsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for var I := 0 to AJsonNode.ChildNodes.Count - 1 do begin
    var LCandidateNode := AJsonNode.ChildNodes[I];
    var LPropertyNode := LCandidateNode.ChildNodes.FindNode(AChildName);
    If (assigned(LPropertyNode)) and
       (LPropertyNode.NodeType = nttext) and
       (LPropertyNode.NodesubType = nstInt32) and
       (LPropertyNode.Int32 = AChildValue) then begin
      Result := LCandidateNode;
      exit;
    end;
    if ARecurse then begin
      Result := ALFindJsonNodeByChildValueInt32W(
                  LCandidateNode,
                  AChildName,
                  AChildValue,
                  ARecurse);
      if assigned(Result) then break;
    end;
  end;
end;

{****************************************}
function ALFindJsonNodeByChildValueInt64W(
           const AJsonNode: TALJSONNodeW;
           const AChildName: String;
           const AChildValue : Int64;
           const ARecurse: Boolean = False): TALJSONNodeW;
begin
  Result := nil;
  if not (AJsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for var I := 0 to AJsonNode.ChildNodes.Count - 1 do begin
    var LCandidateNode := AJsonNode.ChildNodes[I];
    var LPropertyNode := LCandidateNode.ChildNodes.FindNode(AChildName);
    If (assigned(LPropertyNode)) and
       (LPropertyNode.NodeType = nttext) and
       (LPropertyNode.NodesubType = nstInt64) and
       (LPropertyNode.Int64 = AChildValue) then begin
      Result := LCandidateNode;
      exit;
    end;
    if ARecurse then begin
      Result := ALFindJsonNodeByChildValueInt64W(
                  LCandidateNode,
                  AChildName,
                  AChildValue,
                  ARecurse);
      if assigned(Result) then break;
    end;
  end;
end;

{***************************************}
function ALFindJsonNodeByChildValueTextW(
           const AJsonNode: TALJSONNodeW;
           const AChildName: String;
           const AChildValue : String;
           const ARecurse: Boolean = False): TALJSONNodeW;
begin
  Result := nil;
  if not (AJsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for var I := 0 to AJsonNode.ChildNodes.Count - 1 do begin
    var LCandidateNode := AJsonNode.ChildNodes[I];
    var LPropertyNode := LCandidateNode.ChildNodes.FindNode(AChildName);
    If (assigned(LPropertyNode)) and
       (LPropertyNode.NodeType = nttext) and
       (LPropertyNode.NodesubType = nstText) and
       (LPropertyNode.text = AChildValue) then begin
      Result := LCandidateNode;
      exit;
    end;
    if ARecurse then begin
      Result := ALFindJsonNodeByChildValueTextW(
                  LCandidateNode,
                  AChildName,
                  AChildValue,
                  ARecurse);
      if assigned(Result) then break;
    end;
  end;
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
function ALJSONTryStrToRegExW(const S: String; out RegEx: String; out RegExOptions: TALPerlRegExOptions): Boolean;
begin

  // regular expression in JSON must look like: /pattern/options
  // list of valid options is:
  //  'i' for case insensitive matching,
  //  'm' for multiline matching,
  //  'x' for verbose mode,
  //  'l' to make \w, \W, etc. locale dependent,
  //  's' for dotall mode ('.' matches everything),
  //  'u' to make \w, \W, etc. match unicode.
  Result := false;

  // check that first character is /
  if (S <> '') and (S[1] = '/') then begin

    var P1 := ALLastDelimiterW('/', S);
    if P1 <> 1 then begin

      //init Value
      RegEx := ALCopyStr(S, 2, P1 - 2);
      RegExOptions := [];

      // loop on all the options characters
      // to check if they are allowed.
      for var I := P1 + 1 to Length(S) do
        case s[I] of
          'i': RegExOptions := RegExOptions + [preCaseLess];
          'm': RegExOptions := RegExOptions + [preMultiLine];
          'x': RegExOptions := RegExOptions + [preExtended];
          'l':;
          's': RegExOptions := RegExOptions + [preSingleLine];
          'u':;
          else exit;
        end;

      //set the Result to true
      Result := true;

      //Check if it's compiling
      //var LRegEx := TALPerlRegEx.Create;
      //try
      //  LRegEx.RegEx := Value.Expression;
      //  Result := LRegEx.Compile(false{RaiseException});
      //finally
      //  ALFreeAndNil(LRegEx);
      //end;

    end;

  end;

end;
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToBinaryW(const S: String; out Data: TBytes; out Subtype: Byte): Boolean;
begin

  // s must look like
  // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // BinData ( 0 , "JliB6gIMRuSphAD2KmhzgQ==" )
  Result := false;
  var Ln := length(s);
  var P1 := 1;

  while (P1 <= ln) and (s[P1] in [#9, ' ']) do inc(P1);

  if (P1 + 6 > ln) or
     (s[P1] <> 'B') or
     (s[P1+1] <> 'i') or
     (s[P1+2] <> 'n') or
     (s[P1+3] <> 'D') or
     (s[P1+4] <> 'a') or
     (s[P1+5] <> 't') or
     (s[P1+6] <> 'a') then exit; // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                 // ^

  P1 := p1 + 7{Length('BinData')}; // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                   //        ^
  while (P1 <= ln) and (s[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (s[P1] <> '(') then exit; // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                            //        ^P1

  inc(P1); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
           //         ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                                        //          ^P1
  if (P1 > ln) then exit;

  var P2 := P1;
  while (P2 <= ln) and (S[P2] in ['0'..'9']) do inc(P2); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                                         //           ^P2
  if P2 > ln then exit;
  var LInt: Integer;
  if not ALTryStrToInt(ALCopyStr(S,P1,P2-P1), LInt) then Exit;
  subtype := LInt;

  p1 := P2;
  while (P1 <= ln) and (s[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (s[P1] <> ',') then exit; // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                            //            ^P2

  inc(P1); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
           //             ^P1
  while (P1 <= ln) and (s[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (not (s[P1] in ['"',''''])) then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                                                         //            ^P1

  P2 := length(s);
  while (P2 > p1) and (s[P2] in [#9, ' ']) do dec(P2);
  if (P2 <= p1) or (s[P2] <> ')') then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                                             //                                      ^P2

  dec(p2);
  if (P2 <= p1) then exit;
  while (P2 > p1) and (s[P2] in [#9, ' ']) do dec(P2);
  if (P2 <= p1) or (s[P2] <> s[P1]) then exit; // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
                                               //                                     ^P2

  inc(p1);
  Data := ALBase64DecodeBytes(ALCopyStr(s, P1, P2-P1));

  // set the result
  Result := true;

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToDateTimeW(const S: String; out Value: TDateTime): Boolean;
begin

  // s must look like
  // new Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // new ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  // ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  Result := false;
  var Ln := length(s);
  var P1: Integer;
  if ALPosW('new', s) = 1 then P1 := 4{length('new') + 1} // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                          //    ^P1
  else P1 := 1; // Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                // ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <= ln - 3) and
     (S[P1]   = 'D') and
     (S[P1+1] = 'a') and
     (S[P1+2] = 't') and
     (S[P1+3] = 'e') then inc(p1, 4)  // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                      //          ^P1
  else if (P1 <= ln - 6) and
          (S[P1]   = 'I') and
          (S[P1+1] = 'S') and
          (S[P1+2] = 'O') and
          (S[P1+3] = 'D') and
          (S[P1+4] = 'a') and
          (S[P1+5] = 't') and
          (S[P1+6] = 'e') then inc(p1, 7)  // ISODate ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                           //        ^P1
  else exit;
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                            //           ^P1
  inc(P1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
           //            ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (not (S[P1] in ['''','"'])) then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                         //             ^P1
  var LQuoteChar := S[P1]; // "
  inc(p1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
           //              ^P1
  var P2 := P1;
  while (P1 <= ln) and (S[P1] <> LQuoteChar) do inc(P1);
  if (P1 > ln) then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                          //                                      ^P1
  dec(P1);
  if S[P1] <> 'Z' then exit;
  var LTmpStr := ALCopyStr(S,P2,P1-P2); // yyyy-mm-ddThh:nn:ss.zzz

  P2 := 1;
  var LTmpLn := length(LTmpStr);
  while (P2 <= LTmpLn) and (LTmpStr[P2] <> 'T') do inc(P2);
  if P2 > LTmpLn then exit;
  LTmpStr[P2] := ' '; // yyyy-mm-dd hh:nn:ss.zzz

  Result := ALTryStrToDateTime(LTmpStr, Value, ALJsonISODateFormatSettingsW);
  if not Result then exit;

  inc(p1,2);  // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
              //                                       ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> ln) or (S[P1] <> ')') then begin
    Result := false;
    exit;
  end;

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
// ObjectId is a 12-Byte BSON type, constructed using:
// a 4-Byte value representing the seconds since the Unix epoch,
// a 3-Byte machine identifier,
// a 2-Byte process id, and
// a 3-Byte counter, starting with a random value.
function ALJSONTryStrToObjectIDW(const S: String; out Value: TBytes): Boolean;
begin

  // s must look like
  // ObjectId ( "507f1f77bcf86cd799439011" )
  Result := false;
  if ALPosW('ObjectId', S) <> 1 then exit;
  var Ln := length(s);
  var P1 := 9{length('ObjectId') + 1}; // ObjectId ( "507f1f77bcf86cd799439011" )
                                       //         ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                            //          ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //           ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (not (S[P1] in ['''','"'])) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                         //            ^P1
  var LQuoteChar := S[P1]; // "
  inc(p1); // ObjectId ( "507f1f77bcf86cd799439011" )
           //             ^P1
  if (P1 + 23{(length(aObjectIDhex)) - 1} > ln) then exit;
  var LObjectIDhex := ALCopyStr(S,P1,24{length(aObjectIDhex)}); // 507f1f77bcf86cd799439011
  inc(P1, 24{length(aObjectIDhex)}); // ObjectId ( "507f1f77bcf86cd799439011" )
                                     //                                     ^P1
  if (P1 > ln) or (S[P1] <> LQuoteChar) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                   //                                     ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //                                      ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> ln) or (S[P1] <> ')') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                             //                                       ^P1
  //convert 507f1f77bcf86cd799439011 to binary
  Result := ALTryHexToBin(LObjectIDhex, Value) and
            (length(Value) = 12);

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToTimestampW(const S: String; out Value: TALBSONTimestamp): Boolean;
begin

  // s must look like
  // Timestamp(0, 0)
  Result        := false;
  if ALPosW('Timestamp', S) <> 1 then Exit;
  var Ln := length(s);
  var P1 := 10{Length('Timestamp') + 1}; // Timestamp(0, 0)
                                     //          ^
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // Timestamp(0, 0)
                                            //          ^P1
  var P2 := ALPosW(')', S, P1);
  if P2 <> ln then exit; // Timestamp(0, 0)
                         //               ^P2
  var LArgs := ALCopyStr(S, P1+1, P2 - P1-1); // 0, 0

  // take arguments of function Timestamp
  P1 := ALPosW(',', LArgs);
  var LArg1: Integer;
  var LArg2: Integer;
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, 1,      P1 - 1)), LArg1) then Exit;
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, P1 + 1, maxint)), LArg2) then Exit;

  // build result
  Result := true;
  Value.W1 := LArg1; // higher 4 Bytes - increment
  Value.W2 := LArg2; // lower  4 Bytes - timestamp

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToInt32W(const S: String; out Value: Integer): Boolean;
begin

  // s must look like
  // NumberInt ( "12391293" )
  // NumberInt ( 12391293 )
  // 12391293
  Result := ALTryStrToInt(S, Value);
  if Result then exit;
  if ALPosW('NumberInt', S) <> 1 then exit;
  var Ln := length(s);
  var P1 := 10{length('NumberInt') + 1}; // NumberInt ( "12391293" )
                                         //          ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // NumberInt ( "12391293" )
                                            //           ^P1
  inc(p1);  // NumberInt ( "12391293" )
            //            ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  var LTmpStr: String;
  if (P1 > ln) then exit
  else if (not (S[P1] in ['''','"'])) then begin // NumberInt ( 12391293 )
                                                 //             ^P1
    var P2 := P1+1;
    while (P2 <= ln) and (S[P2] in ['0'..'9']) do inc(P2); // NumberInt ( 12391293 )
                                                           //                     ^P2
    if P2 > ln then exit;
    LTmpStr := ALCopyStr(S,P1,P2-P1); // 12391293
    P1 := P2; // NumberInt ( 12391293 )
              //                     ^P2

    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberInt ( "12391293" )
                                               //                        ^P1
  end
  else begin // NumberInt ( "12391293" )
             //             ^P1

    var LQuoteChar := S[P1]; // "
    inc(p1); // NumberInt ( "12391293" )
             //              ^P1
    var P2 := P1;
    while P2 <= Ln do
      if S[P2] = LQuoteChar then break
      else inc(P2);
    if P2 > ln then exit;
    LTmpStr := ALCopyStr(S,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberInt ( "12391293" )
                  //                       ^P1
    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberInt ( "12391293" )
                                               //                        ^P1
  end;

  //convert 12391293 to Integer
  Result := ALTryStrToInt(LTmpStr, Value);

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToInt64W(const S: String; out Value: Int64): Boolean;
begin

  // s must look like
  // NumberLong ( "12391293" )
  // NumberLong ( 12391293 )
  // 12391293
  Result := ALTryStrToInt64(S, Value);
  if Result then exit;
  if ALPosW('NumberLong', S) <> 1 then exit;
  var Ln := length(s);
  var P1 := 11{length('NumberLong') + 1}; // NumberLong ( "12391293" )
                                          //           ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // NumberLong ( "12391293" )
                                            //            ^P1
  inc(p1);  // NumberLong ( "12391293" )
            //             ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  var LTmpStr: String;
  if (P1 > ln) then exit
  else if (not (S[P1] in ['''','"'])) then begin // NumberLong ( 12391293 )
                                                 //              ^P1
    var P2 := P1+1;
    while (P2 <= ln) and (S[P2] in ['0'..'9']) do inc(P2); // NumberLong ( 12391293 )
                                                           //                      ^P2
    if P2 > ln then exit;
    LTmpStr := ALCopyStr(S,P1,P2-P1); // 12391293
    P1 := P2; // NumberLong ( 12391293 )
              //                      ^P2

    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberLong ( "12391293" )
                                               //                         ^P1
  end
  else begin // NumberLong ( "12391293" )
             //              ^P1

    var LQuoteChar := S[P1]; // "
    inc(p1); // NumberLong ( "12391293" )
             //               ^P1
    var P2 := P1;
    while P2 <= Ln do
      if S[P2] = LQuoteChar then break
      else inc(P2);
    if P2 > ln then exit;
    LTmpStr := ALCopyStr(S,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberLong ( "12391293" )
                  //                        ^P1
    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberLong ( "12391293" )
                                               //                         ^P1
  end;

  //convert 12391293 to Integer
  Result := ALTryStrToInt64(LTmpStr, Value);

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{***************************************************************}
procedure ALJSONDocErrorW(const Msg: String); noreturn; overload;
begin
  raise EALJSONDocError.Create(Msg);
end;

{*******************************************************************************************}
procedure ALJSONDocErrorW(const Msg: String; const Args: array of const); noreturn; overload;
begin
  raise EALJSONDocError.CreateFmt(Msg, Args);
end;

{************************************************************************************************}
procedure ALJSONDocErrorW(const Msg: String; const NodeType: TALJSONNodeType); noreturn; overload;
begin
  case NodeType of
    ntObject: ALJSONDocErrorW(Msg, ['ntObject']);
    ntArray: ALJSONDocErrorW(Msg, ['ntArray']);
    ntText: ALJSONDocErrorW(Msg, ['ntText']);
    else ALJSONDocErrorW(ALJSONInvalidNodeType);
  end;
end;

{********************************************************************************************}
{Call CreateNode to create a new generic JSON node. The resulting node does not have a parent,
 but can be added to the ChildNodes list of any node in the document.}
function ALCreateJSONNodeW(const NodeName: String; NodeType: TALJSONNodeType): TALJSONNodeW;
begin
  case NodeType of
    ntObject: Result := TALJSONObjectNodeW.Create(NodeName);
    ntArray: Result := TALJSONArrayNodeW.Create(NodeName);
    ntText: Result := TALJSONTextNodeW.Create(NodeName);
    else ALJSONDocErrorW(ALJSONInvalidNodeType);
  end;
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
class function TALJSONDocumentW.DetectNodeTypeFromJSON(const Buffer: String): TALJSONNodeType;
begin

  //--
  Result := ntText;

  //--
  var BufferLength := length(Buffer);
  var BufferPos := 1;

  //--
  While (BufferPos <= BufferLength) do begin
    var c := Buffer[BufferPos];
    If c <= ' ' then inc(bufferPos)
    else begin
      if c = '{' then Result := ntObject
      else if c = '[' then Result := ntarray
      else Result := ntText;
      break;
    end;
  end;

end;
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{***************************************************}
class function TALJSONDocumentW.Create: TALJSONNodeW;
begin
  Result := ALCreateJSONNodeW('', ntObject);
end;

{**********************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromJSONString(const Str: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  var LNodeType := DetectNodeTypeFromJSON(Str);
  if LNodeType in [ntObject, ntArray] then Result := ALCreateJSONNodeW('', LNodeType)
  else AlJSONDocErrorW(ALJSONParseError);
  try
    Result.LoadFromJSONString(Str, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  Result := CreateFromJSONString(ALGetStringFromStream(Stream, TEncoding.UTF8), Options);
end;

{*************************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    Result := CreateFromJSONStream(LFileStream, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{***********************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromBSONBytes(const Bytes: TBytes; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  Result := ALCreateJSONNodeW('', ntObject);
  try
    Result.LoadFromBSONBytes(Bytes, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  Result := ALCreateJSONNodeW('', ntObject);
  try
    Result.LoadFromBSONStream(Stream, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{*************************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    Result := CreateFromBSONStream(LFileStream, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{***********************************************}
class procedure TALJSONDocumentW.ParseJSONString(
                  const Str: String;
                  const OnParseText: TALJSONParseTextEventW;
                  const OnParseStartObject: TALJSONParseObjectEventW;
                  const OnParseEndObject: TALJSONParseObjectEventW;
                  const OnParseStartArray: TALJSONParseArrayEventW;
                  const OnParseEndArray: TALJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
begin
  var LJsonNode: TALJSONNodeW;
  var LNodeType := DetectNodeTypeFromJSON(Str);
  if LNodeType in [ntObject, ntArray] then LJsonNode := ALCreateJSONNodeW('', LNodeType)
  else AlJSONDocErrorW(ALJSONParseError);
  try
    LJsonNode.ParseJSONString(
      Str,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LJsonNode);
  end;
end;

{***********************************************}
class procedure TALJSONDocumentW.ParseJSONStream(
                  const Stream: TStream;
                  const OnParseText: TALJSONParseTextEventW;
                  const OnParseStartObject: TALJSONParseObjectEventW;
                  const OnParseEndObject: TALJSONParseObjectEventW;
                  const OnParseStartArray: TALJSONParseArrayEventW;
                  const OnParseEndArray: TALJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
begin
  ParseJSONString(
    ALGetStringFromStream(Stream, TEncoding.UTF8),
    OnParseText,
    OnParseStartObject,
    OnParseEndObject,
    OnParseStartArray,
    OnParseEndArray,
    Options);
end;

{*********************************************}
class procedure TALJSONDocumentW.ParseJSONFile(
                  const FileName: String;
                  const OnParseText: TALJSONParseTextEventW;
                  const OnParseStartObject: TALJSONParseObjectEventW;
                  const OnParseEndObject: TALJSONParseObjectEventW;
                  const OnParseStartArray: TALJSONParseArrayEventW;
                  const OnParseEndArray: TALJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseJSONStream(
      LFileStream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{**********************************************}
class procedure TALJSONDocumentW.ParseBSONBytes(
                  const Bytes: TBytes;
                  const OnParseText: TALJSONParseTextEventW;
                  const OnParseStartObject: TALJSONParseObjectEventW;
                  const OnParseEndObject: TALJSONParseObjectEventW;
                  const OnParseStartArray: TALJSONParseArrayEventW;
                  const OnParseEndArray: TALJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
begin
  var LJsonNode := ALCreateJSONNodeW('', ntObject);
  try
    LJsonNode.ParseBSONBytes(
      Bytes,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LJsonNode);
  end;
end;

{***********************************************}
class procedure TALJSONDocumentW.ParseBSONStream(
                  const Stream: TStream;
                  const OnParseText: TALJSONParseTextEventW;
                  const OnParseStartObject: TALJSONParseObjectEventW;
                  const OnParseEndObject: TALJSONParseObjectEventW;
                  const OnParseStartArray: TALJSONParseArrayEventW;
                  const OnParseEndArray: TALJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
begin
  ParseBSONStream(
    Stream,
    OnParseText,
    OnParseStartObject,
    OnParseEndObject,
    OnParseStartArray,
    OnParseEndArray,
    Options);
end;

{*********************************************}
class procedure TALJSONDocumentW.ParseBSONFile(
                  const FileName: String;
                  const OnParseText: TALJSONParseTextEventW;
                  const OnParseStartObject: TALJSONParseObjectEventW;
                  const OnParseEndObject: TALJSONParseObjectEventW;
                  const OnParseStartArray: TALJSONParseArrayEventW;
                  const OnParseEndArray: TALJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseBSONStream(
      LFileStream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{**********************************************************}
{Creates the object that implements the ChildNodes property}
function TALJSONNodeW.CreateChildList: TALJSONNodeListW;
begin
  Result := TALJSONNodeListW.Create;
end;

{************************************************************}
function TALJSONNodeW.InternalGetChildNodes: TALJSONNodeListW;
begin
  Result := nil;
end;

{****************************************************}
function TALJSONNodeW.GetChildNodes: TALJSONNodeListW;
begin
  ALJSONDocErrorW(ALJSONOperationError,GetNodeType)
end;

{***********************************************************************}
function TALJSONNodeW.GetChildNode(const NodeName: String): TALJSONNodeW;
begin
  Result := ChildNodes.findNode(nodeName);
end;

{*********************************************************************************************}
function TALJSONNodeW.GetChildValueText(const NodeName: String; const Default: String): String;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetText(default);
end;

{**********************************************************************************************}
function TALJSONNodeW.GetChildValueFloat(const NodeName: String; const Default: Double): Double;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetFloat(default);
end;

{*******************************************************************************************************}
function TALJSONNodeW.GetChildValueDateTime(const NodeName: String; const Default: TDateTime): TDateTime;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetDateTime(default);
end;

{**********************************************************************************************************************}
function TALJSONNodeW.GetChildValueTimestamp(const NodeName: String; const Default: TALBSONTimestamp): TALBSONTimestamp;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetTimestamp(default);
end;

{*************************************************************************************************}
function TALJSONNodeW.GetChildValueObjectID(const NodeName: String; const Default: TBytes): TBytes;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetObjectID(default);
end;

{************************************************************************************************}
function TALJSONNodeW.GetChildValueInt32(const NodeName: String; const Default: Integer): Integer;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetInt32(default);
end;

{********************************************************************************************}
function TALJSONNodeW.GetChildValueInt64(const NodeName: String; const Default: Int64): Int64;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetInt64(default);
end;

{***********************************************************************************************}
function TALJSONNodeW.GetChildValueBool(const NodeName: String; const Default: Boolean): Boolean;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBool(default);
end;

{***************************************************************************************************}
function TALJSONNodeW.GetChildValueJavascript(const NodeName: String; const Default: String): String;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetJavascript(default);
end;

{**********************************************************************************************}
function TALJSONNodeW.GetChildValueRegEx(const NodeName: String; const Default: String): String;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetRegEx(default);
end;

{*******************************************************************************************************************************}
function TALJSONNodeW.GetChildValueRegExOptions(const NodeName: String; const Default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetRegExOptions(default);
end;

{******************************************************************************************************}
function TALJSONNodeW.GetChildValueBinaryAsBytes(const NodeName: String; const Default: TBytes): TBytes;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinaryAsBytes(default);
end;

{******************************************************************************************************}
function TALJSONNodeW.GetChildValueBinaryAsStream(const NodeName: String; const Default: TStream): TStream;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinaryAsStream(default);
end;

{**************************************************************************************************}
function TALJSONNodeW.GetChildValueBinarySubType(const NodeName: String; const Default: Byte): Byte;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinarySubType(default);
end;

{***********************************************************************}
function TALJSONNodeW.GetChildValueNull(const NodeName: String): Boolean;
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then Result := true
  else Result := LNode.GetNull;
end;

{****************************************************************************}
function TALJSONNodeW.GetChildNode(const Path: array of String): TALJSONNodeW;
begin
  Result := Self;
  for var I := low(path) to high(path) do begin
    Result := Result.ChildNodes.findNode(path[I]);
    if (result = nil) then exit;
  end;
end;

{**************************************************************************************************}
function TALJSONNodeW.GetChildValueText(const Path: array of String; const Default: String): String;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetText(default);
end;

{***************************************************************************************************}
function TALJSONNodeW.GetChildValueFloat(const Path: array of String; const Default: Double): Double;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetFloat(default);
end;

{************************************************************************************************************}
function TALJSONNodeW.GetChildValueDateTime(const Path: array of String; const Default: TDateTime): TDateTime;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetDateTime(default);
end;

{***************************************************************************************************************************}
function TALJSONNodeW.GetChildValueTimestamp(const Path: array of String; const Default: TALBSONTimestamp): TALBSONTimestamp;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetTimestamp(default);
end;

{******************************************************************************************************}
function TALJSONNodeW.GetChildValueObjectID(const Path: array of String; const Default: TBytes): TBytes;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetObjectID(default);
end;

{*****************************************************************************************************}
function TALJSONNodeW.GetChildValueInt32(const Path: array of String; const Default: Integer): Integer;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetInt32(default);
end;

{*************************************************************************************************}
function TALJSONNodeW.GetChildValueInt64(const Path: array of String; const Default: Int64): Int64;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetInt64(default);
end;

{****************************************************************************************************}
function TALJSONNodeW.GetChildValueBool(const Path: array of String; const Default: Boolean): Boolean;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBool(default);
end;

{********************************************************************************************************}
function TALJSONNodeW.GetChildValueJavascript(const Path: array of String; const Default: String): String;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetJavascript(default);
end;

{***************************************************************************************************}
function TALJSONNodeW.GetChildValueRegEx(const Path: array of String; const Default: String): String;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetRegEx(default);
end;

{************************************************************************************************************************************}
function TALJSONNodeW.GetChildValueRegExOptions(const Path: array of String; const Default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetRegExOptions(default);
end;

{******************************************************************************************************}
function TALJSONNodeW.GetChildValueBinaryAsBytes(const Path: array of String; const Default: TBytes): TBytes;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinaryAsBytes(default);
end;

{******************************************************************************************************}
function TALJSONNodeW.GetChildValueBinaryAsStream(const Path: array of String; const Default: TStream): TStream;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinaryAsStream(default);
end;

{*******************************************************************************************************}
function TALJSONNodeW.GetChildValueBinarySubType(const Path: array of String; const Default: Byte): Byte;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := default
  else Result := LNode.GetBinarySubType(default);
end;

{****************************************************************************}
function TALJSONNodeW.GetChildValueNull(const Path: array of String): Boolean;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      Result := True;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then Result := true
  else Result := LNode.GetNull;
end;

{************************************************************************************}
procedure TALJSONNodeW.SetChildValueText(const NodeName: String; const Value: String);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetText(value)
  else LNode.SetText(value);
end;

{*************************************************************************************}
procedure TALJSONNodeW.SetChildValueFloat(const NodeName: String; const Value: Double);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetFloat(value)
  else LNode.SetFloat(value);
end;

{*******************************************************************************************}
procedure TALJSONNodeW.SetChildValueDateTime(const NodeName: String; const Value: TDateTime);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetDateTime(value)
  else LNode.SetDateTime(value);
end;

{***************************************************************************************************}
procedure TALJSONNodeW.SetChildValueTimestamp(const NodeName: String; const Value: TALBSONTimestamp);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetTimestamp(value)
  else LNode.SetTimestamp(value);
end;

{****************************************************************************************}
procedure TALJSONNodeW.SetChildValueObjectID(const NodeName: String; const Value: TBytes);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetObjectID(value)
  else LNode.SetObjectID(value);
end;

{**************************************************************************************}
procedure TALJSONNodeW.SetChildValueInt32(const NodeName: String; const Value: Integer);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt32(value)
  else LNode.SetInt32(value);
end;

{************************************************************************************}
procedure TALJSONNodeW.SetChildValueInt64(const NodeName: String; const Value: Int64);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt64(value)
  else LNode.SetInt64(value);
end;

{*************************************************************************************}
procedure TALJSONNodeW.SetChildValueBool(const NodeName: String; const Value: Boolean);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBool(value)
  else LNode.SetBool(value);
end;

{******************************************************************************************}
procedure TALJSONNodeW.SetChildValueJavascript(const NodeName: String; const Value: String);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetJavascript(value)
  else LNode.SetJavascript(value);
end;

{*************************************************************************************}
procedure TALJSONNodeW.SetChildValueRegEx(const NodeName: String; const Value: String);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegEx(value)
  else LNode.SetRegEx(value);
end;

{*********************************************************************************************************}
procedure TALJSONNodeW.SetChildValueRegExOptions(const NodeName: String; const Value: TALPerlRegExOptions);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegExOptions(value)
  else LNode.SetRegExOptions(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetChildValueBinaryAsBytes(const NodeName: String; const Value: TBytes);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinaryAsBytes(value)
  else LNode.SetBinaryAsBytes(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetChildValueBinaryAsStream(const NodeName: String; const Value: TStream);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinaryAsStream(value)
  else LNode.SetBinaryAsStream(value);
end;

{*******************************************************************************************}
procedure TALJSONNodeW.SetChildValueBinarySubType(const NodeName: String; const Value: Byte);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinarySubType(value)
  else LNode.SetBinarySubType(value);
end;

{***************************************************************}
procedure TALJSONNodeW.SetChildValueNull(const NodeName: String);
begin
  var LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetNull(true)
  else LNode.SetNull(true);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetChildValueText(const Path: array of String; const Value: String);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetText(value)
  else LTmpNode.SetText(value);
end;

{******************************************************************************************}
procedure TALJSONNodeW.SetChildValueFloat(const Path: array of String; const Value: Double);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetFloat(value)
  else LTmpNode.SetFloat(value);
end;

{************************************************************************************************}
procedure TALJSONNodeW.SetChildValueDateTime(const Path: array of String; const Value: TDateTime);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetDateTime(value)
  else LTmpNode.SetDateTime(value);
end;

{********************************************************************************************************}
procedure TALJSONNodeW.SetChildValueTimestamp(const Path: array of String; const Value: TALBSONTimestamp);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetTimestamp(value)
  else LTmpNode.SetTimestamp(value);
end;

{*********************************************************************************************}
procedure TALJSONNodeW.SetChildValueObjectID(const Path: array of String; const Value: TBytes);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetObjectID(value)
  else LTmpNode.SetObjectID(value);
end;

{*******************************************************************************************}
procedure TALJSONNodeW.SetChildValueInt32(const Path: array of String; const Value: Integer);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetInt32(value)
  else LTmpNode.SetInt32(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetChildValueInt64(const Path: array of String; const Value: Int64);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetInt64(value)
  else LTmpNode.SetInt64(value);
end;

{******************************************************************************************}
procedure TALJSONNodeW.SetChildValueBool(const Path: array of String; const Value: Boolean);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBool(value)
  else LTmpNode.SetBool(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeW.SetChildValueJavascript(const Path: array of String; const Value: String);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetJavascript(value)
  else LTmpNode.SetJavascript(value);
end;

{******************************************************************************************}
procedure TALJSONNodeW.SetChildValueRegEx(const Path: array of String; const Value: String);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetRegEx(value)
  else LTmpNode.SetRegEx(value);
end;

{**************************************************************************************************************}
procedure TALJSONNodeW.SetChildValueRegExOptions(const Path: array of String; const Value: TALPerlRegExOptions);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetRegExOptions(value)
  else LTmpNode.SetRegExOptions(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetChildValueBinaryAsBytes(const Path: array of String; const Value: TBytes);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinaryAsBytes(value)
  else LTmpNode.SetBinaryAsBytes(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetChildValueBinaryAsStream(const Path: array of String; const Value: TStream);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinaryAsStream(value)
  else LTmpNode.SetBinaryAsStream(value);
end;

{************************************************************************************************}
procedure TALJSONNodeW.SetChildValueBinarySubType(const Path: array of String; const Value: Byte);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinarySubType(value)
  else LTmpNode.SetBinarySubType(value);
end;

{********************************************************************}
procedure TALJSONNodeW.SetChildValueNull(const Path: array of String);
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  var LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetNull(true)
  else LTmpNode.SetNull(true);
end;

{***********************************************}
{Indicates whether this node has any child nodes}
function TALJSONNodeW.GetHasChildNodes: Boolean;
begin
  var LNodeList := InternalGetChildNodes;
  Result := assigned(LNodeList) and (LNodeList.Count > 0);
end;

{*********************************************************}
procedure TALJSONNodeW.SetNodeName(const NodeName: String);
begin
  if FNodeName <> NodeName then
    FNodeName := NodeName;
end;

{******************************************************************************}
// By default json (ie: javascript) treats all numbers as floating-point values.
// To let other system (ie: mongoDB) understand the type of the number
// we provide the helper functions NumberLong() to handle 64-bit Integers
// and NumberInt() to handle 32-bit Integers (and some others). theses helper functions are
// used when saving the json document.
function TALJSONNodeW.GetInterchangeValue(const SkipNodeSubTypeHelper: Boolean = False): String;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONNodeW.GetText: String;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{***********************************************************}
function TALJSONNodeW.GetText(const Default: String): String;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONNodeW.SetText(const Value: String);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*************************************}
function TALJSONNodeW.GetFloat: Double;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{************************************************************}
function TALJSONNodeW.GetFloat(const Default: Double): Double;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{***************************************************}
procedure TALJSONNodeW.SetFloat(const Value: Double);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*******************************************}
function TALJSONNodeW.GetDateTime: TDateTime;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*********************************************************************}
function TALJSONNodeW.GetDateTime(const Default: TDateTime): TDateTime;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*********************************************************}
procedure TALJSONNodeW.SetDateTime(const Value: TDateTime);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{***************************************************}
function TALJSONNodeW.GetTimestamp: TALBSONTimestamp;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{************************************************************************************}
function TALJSONNodeW.GetTimestamp(const Default: TALBSONTimestamp): TALBSONTimestamp;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*****************************************************************}
procedure TALJSONNodeW.SetTimestamp(const Value: TALBSONTimestamp);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************}
function TALJSONNodeW.GetObjectID: TBytes;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{***************************************************************}
function TALJSONNodeW.GetObjectID(const Default: TBytes): TBytes;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{******************************************************}
procedure TALJSONNodeW.SetObjectID(const Value: TBytes);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{**************************************}
function TALJSONNodeW.GetInt32: Integer;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{**************************************************************}
function TALJSONNodeW.GetInt32(const Default: Integer): Integer;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************}
procedure TALJSONNodeW.SetInt32(const Value: Integer);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{************************************}
function TALJSONNodeW.GetInt64: Int64;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{**********************************************************}
function TALJSONNodeW.GetInt64(const Default: Int64): Int64;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{**************************************************}
procedure TALJSONNodeW.SetInt64(const Value: Int64);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*************************************}
function TALJSONNodeW.GetBool: Boolean;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*************************************************************}
function TALJSONNodeW.GetBool(const Default: Boolean): Boolean;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{***************************************************}
procedure TALJSONNodeW.SetBool(const Value: Boolean);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*************************************}
function TALJSONNodeW.GetNull: Boolean;
begin
  Result := False;
end;

{***************************************************}
procedure TALJSONNodeW.SetNull(const Value: Boolean);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{******************************************}
function TALJSONNodeW.GetJavascript: String;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*****************************************************************}
function TALJSONNodeW.GetJavascript(const Default: String): String;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{********************************************************}
procedure TALJSONNodeW.SetJavascript(const Value: String);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*************************************}
function TALJSONNodeW.GetRegEx: String;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{************************************************************}
function TALJSONNodeW.GetRegEx(const Default: String): String;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*****************************************************}
procedure TALJSONNodeW.SetRegEx(const Pattern: String);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*********************************************************}
function TALJSONNodeW.GetRegExOptions: TALPerlRegExOptions;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*********************************************************************************************}
function TALJSONNodeW.GetRegExOptions(const Default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{***********************************************************************}
procedure TALJSONNodeW.SetRegExOptions(const Value: TALPerlRegExOptions);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeW.GetBinaryAsBytes: TBytes;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeW.GetBinaryAsBytes(const Default: TBytes): TBytes;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeW.GetBinaryAsStream: TStream;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeW.GetBinaryAsStream(const Default: TStream): TStream;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
procedure TALJSONNodeW.SetBinaryAsBytes(const Data: TBytes);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
procedure TALJSONNodeW.SetBinaryAsStream(const Data: TStream);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeW.GetOwnsBinaryStream: Boolean;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
procedure TALJSONNodeW.SetOwnsBinaryStream(const Value: Boolean);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{*******************************************}
function TALJSONNodeW.GetBinarySubType: Byte;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{****************************************************************}
function TALJSONNodeW.GetBinarySubType(const Default: Byte): Byte;
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{***********************************************************}
procedure TALJSONNodeW.SetBinarySubType(const Subtype: Byte);
begin
  AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
end;

{********************************************************************}
{Returns the JSON that corresponds to the subtree rooted at this node.
 GetJSON returns the JSON that corresponds to this node and any child nodes it contains.}
function TALJSONNodeW.GetJSON: String;
begin
  SaveToJSONString(result);
end;

{*************************************************}
{SetJSON reload the node with the new given value }
procedure TALJSONNodeW.SetJSON(const Value: String);
begin
  LoadFromJSONString(Value);
end;

{********************************************************************}
{Returns the BSON that corresponds to the subtree rooted at this node.
 GetBSON returns the BSON that corresponds to this node and any child nodes it contains.}
function TALJSONNodeW.GetBSON: TBytes;
begin
  SaveToBSONBytes(result);
end;

{*************************************************}
{SetBSON reload the node with the new given value }
procedure TALJSONNodeW.SetBSON(const Value: TBytes);
begin
  LoadFromBSONBytes(Value);
end;

{******************************************************}
constructor TALJSONNodeW.Create(const NodeName: String);
begin
  FNodeName := NodeName;
end;

{****************************************************************************************************************************************}
function TALJSONNodeW.AddChild(const NodeName: String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW;
begin
  Result := ALCreateJSONNodeW(NodeName,NodeType);
  Try
    ChildNodes.Insert(Index, Result);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{*********************************************************************************************************************************************}
function TALJSONNodeW.AddChild(const Path: array of String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I], TDirection.FromEnd);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  Result := LNode.addChild(path[high(path)], NodeType, Index);
end;

{****************************************************************************************************************}
function TALJSONNodeW.AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW;
begin
  Result := AddChild('', NodeType, Index);
end;

{*****************************************************************}
function TALJSONNodeW.DeleteChild(const NodeName: String): Boolean;
begin
  var I := ChildNodes.IndexOf(NodeName);
  if I >= 0 then begin
    ChildNodes.Delete(I);
    Result := True;
  end
  else Result := False;
end;

{**********************************************************************}
function TALJSONNodeW.DeleteChild(const Path: array of String): Boolean;
begin
  var LNode := Self;
  for var I := low(path) to high(path) - 1 do begin
    var LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then exit(false)
    else LNode := LTmpNode;
  end;
  var I := LNode.ChildNodes.IndexOf(path[high(path)]);
  if I >= 0 then begin
    LNode.ChildNodes.Delete(I);
    Result := True;
  end
  else Result := False;
end;

{************************************************************************************************}
function TALJSONNodeW.CreateNode(const NodeName: String; NodeType: TALJSONNodeType): TALJSONNodeW;
begin
  Result := ALCreateJSONNodeW(NodeName, NodeType);
end;

{****************************************}
function TALJSONNodeW.Clone: TALJSONNodeW;
begin
  case NodeType of
    ntObject: Result := TALJSONObjectNodeW.Create(NodeName);
    ntArray: Result := TALJSONArrayNodeW.Create(NodeName);
    ntText: begin
      Result := TALJSONTextNodeW.Create(NodeName);
      var LDestNode := TALJSONTextNodeW(Result);
      var LSrcNode := TALJSONTextNodeW(Self);
      LDestNode.FNodeSubType := LSrcNode.FNodeSubType;
      LDestNode.FStorageKind := LSrcNode.FStorageKind;
      LDestNode.FBinarySubType := LSrcNode.FBinarySubType;
      LDestNode.FRegExOptions := LSrcNode.FRegExOptions;
      case LDestNode.FStorageKind of
        skString: PString(@LDestNode.FNodeValue)^ := PString(@LSrcNode.FNodeValue)^;
        skBytes: PBytes(@LDestNode.FNodeValue)^ := PBytes(@LSrcNode.FNodeValue)^;
        skOwnedStream: begin
          var LDstStream := TMemoryStream.Create;
          try
            var LSrcStream := TStream(Pointer(NativeInt(LSrcNode.FNodeValue)));
            LDstStream.CopyFrom(LSrcStream);
            LDestNode.FNodeValue := System.Int64(Pointer(LDstStream));
          except
            ALFreeAndNil(LDstStream);
            raise;
          end;
        end
        else
          LDestNode.FNodeValue := LSrcNode.FNodeValue;
      end;
    end;
    else
      AlJSONDocErrorW(ALJSONInvalidNodeType);
  end;
  if HasChildNodes then
    for var I := 0 to ChildNodes.Count - 1 do
      Result.ChildNodes.Add(ChildNodes[I].Clone);
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
procedure TALJSONNodeW.ParseJSON(
            const Buffer: String;
            const SaxMode: Boolean;
            const OnParseText: TALJSONParseTextEventW;
            const OnParseStartObject: TALJSONParseObjectEventW;
            const OnParseEndObject: TALJSONParseObjectEventW;
            const OnParseStartArray: TALJSONParseArrayEventW;
            const OnParseEndArray: TALJSONParseArrayEventW;
            const Options: TALJSONParseOptions);

Var
  BufferLength: Integer;
  BufferPos: Integer;
  CurrName: String;
  CurrIndex: Integer;
  CurrValue: String;
  NotSaxMode: Boolean;
  WorkingNode: TALJSONNodeW;
  NamePaths: TALNVStringListW;
  ObjectPaths: TList<TPair<Integer, TALJSONNodeW>>;
  DecodeJSONReferences: Boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(const ExtraItems: String = ''): String;
  begin
    var LB := ALDefaultJsonPathSeparatorW;
    var Size: Integer := length(ExtraItems);
    if size <> 0 then Inc(Size, 1{length(LB)});
    for var I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths.Names[I]) + 1{length(LB)});
    SetLength(Result, Size);
    var P: Integer := 1;
    for var I := 1 to NamePaths.Count - 1 do begin
      var S := NamePaths.Names[I];
      var L: Integer := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, PByte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
        Inc(P, L);
      end;
      L := 1{length(LB)};
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNodeW(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJSONNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, PByte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      var L: Integer := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, PByte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
      Inc(P, L);
    end;
    setlength(result,P-1);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(
              const index: Integer;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr('[' + ALIntToStrW(index) + ']'), '', Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithName(
              const name: String;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr(Name), Name, Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseText(
              const Index: Integer;
              const Name: String;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then begin
      if notSaxMode then begin
        if WorkingNode.nodetype=ntarray then _DoParseTextWithIndex(Index, Args, NodeSubType)
        else _DoParseTextWithName(Name, Args, NodeSubType);
      end
      else begin
        if NamePaths.Count = 0 then ALJSONDocErrorW(ALJSONParseError);
        if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntArray then _DoParseTextWithIndex(Index, Args, NodeSubType)
        else _DoParseTextWithName(Name, Args, NodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartObject(const Name: String);
  begin
    if Assigned(OnParseStartObject) then OnParseStartObject(Self, GetPathStr, Name);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndObject;
  begin
    if Assigned(OnParseEndObject) then begin
      if NamePaths.Count = 0 then ALJSONDocErrorW(ALJSONParseError);
      OnParseEndObject(Self, GetPathStr, NamePaths.Names[NamePaths.Count - 1]);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: String);
  begin
    if Assigned(OnParseStartArray) then OnParseStartArray(Self, GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if Assigned(OnParseEndArray) then begin
      if NamePaths.Count = 0 then ALJSONDocErrorW(ALJSONParseError);
      OnParseEndArray(Self, GetPathStr, NamePaths.Names[NamePaths.Count - 1]);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexItemToNamePath(const index: Integer; Obj: Pointer);
  begin
    var S1: String;
    setlength(S1,sizeOf(Integer) div sizeOF(Char)); // off course sizeOf(Integer) must be a multiple of sizeOf(char) but it's always the case
    ALmove(index, pointer(S1)^, sizeOf(Integer));
    NamePaths.AddNameValueObject('[' + ALIntToStrW(Index) + ']', S1, Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddNameItemToNamePath(const name: String; Obj: Pointer);
  begin
    NamePaths.AddNameValueObject(Name, #$ffff#$ffff, Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddItemToNamePath(index: Integer; const name: String; Obj: Pointer);
  begin
    if notSaxMode then begin
      if WorkingNode.nodetype=ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end
    else begin
      if NamePaths.Count = 0 then ALJSONDocErrorW(ALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt64Node(index: Integer; const name: String; const Value: String): Boolean;
  begin
    var LInt64: System.Int64;
    if ALJSONTryStrToInt64W(value, LInt64) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetInt64(LInt64);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LInt64], nstInt64)
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt32Node(index: Integer; const name: String; const Value: String): Boolean;
  begin
    var LInt32: System.Int32;
    if ALJSONTryStrToInt32W(value, LInt32) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetInt32(LInt32);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LInt32], nstInt32)
      end
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTextNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    Result := true;
    if NotSaxMode then begin
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(index, Name, [value], nstText)
    end
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createFloatNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    var LDouble: Double;
    if ALTryStrToFloat(value, LDouble) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetFloat(LDouble);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LDouble], nstFloat)
      end
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBinaryNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    var LBinData: TBytes;
    var LBinSubtype: Byte;
    if ALJSONTryStrToBinaryW(value, LBinData, LBinSubtype) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetBinaryAsBytes(LBinData);
          LNode.SetBinarySubType(LBinSubtype);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LBinData, LBinSubtype], nstBinary);
      end
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createObjectIDNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    var LObjectID: TBytes;
    if ALJSONTryStrToObjectIDW(value, LObjectID) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetObjectID(LObjectID);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LObjectID], nstObjectID)
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBooleanNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    var LBool: Boolean;
    if value = 'true' then LBool := true
    else if value = 'false' then LBool := false
    else begin
      Result := False;
      exit;
    end;
    Result := true;
    if NotSaxMode then begin
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setbool(LBool);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(index, Name, [LBool], nstBoolean);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createDateTimeNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    var LDateTime: TDateTime;
    if ALJSONTryStrToDateTimeW(value, LDateTime) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setdatetime(LDateTime);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LDateTime], nstDateTime);
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTimestampNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    var LTimestamp: TALBSONTimestamp;
    if ALJSONTryStrToTimestampW(value, LTimestamp) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetTimestamp(LTimestamp);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LTimestamp.W1, LTimestamp.W2], nstTimeStamp);
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createnullNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    if value = 'null' then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setnull(true);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, ['null'], nstNull);
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createRegExNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    var LRegEx: String;
    var LRegExOptions: TALPerlRegExOptions;
    if ALJSONTryStrToRegExW(value, LRegEx, LRegExOptions) then begin
      Result := true;
      if NotSaxMode then begin
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetRegEx(LRegEx);
          LNode.SetRegExOptions(LRegExOptions);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(index, Name, [LRegEx, Byte(LRegExOptions)], nstRegEx)
      end;
    end
    else Result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createJavascriptNode(index: Integer; const name: String; const Value: String): Boolean;
  begin
    Result := true;
    if NotSaxMode then begin
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(index, Name, [value], nstJavascript);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createNode(index: Integer; const name: String; const Value: String; AQuotedValue: Boolean);
  begin
    if AQuotedValue then begin
      _createTextNode(index, Name, Value);
      exit;
    end;
    if _createFloatNode(index, Name, Value) then exit;  // << we have the same problem as javascript, if we put here a big number like (by exemple) 9223372036854775808
                                                        // << then the stored value will be different because of double precision that is less than Int64 precision
                                                        // << it's the way javascript json work, it's have no room for int / Int64 :(
                                                        // << if we want to have the possibility to store Int64 precision then we must use node subtype helper
                                                        // << like NumberLong(9223372036854775808)
    if _createBooleanNode(index, Name, Value) then exit;
    if _createNullNode(index, Name, Value) then exit;
    if _createInt32Node(index, Name, Value) then exit;
    if _createInt64Node(index, Name, Value) then exit;
    if _createDateTimeNode(index, Name, Value) then exit;
    if _createBinaryNode(index, Name, Value) then exit;
    if _createObjectIDNode(index, Name, Value) then exit;
    if _createRegExNode(index, Name, Value) then exit;
    if _createTimeStampNode(index, Name, Value) then exit;
    _createJavascriptNode(index, Name, Value);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _extractLastIndexFromNamePath: Integer;
  begin
    if NamePaths.Count = 0 then ALJSONDocErrorW(ALJSONParseError);
    ALMove(pointer(namePaths.ValueFromIndex[namepaths.Count - 1])^,result,sizeOf(Integer));
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  begin

    {$REGION 'init current char (c)'}
    var c: Char := Buffer[BufferPos];
    {$ENDREGION}

    {$REGION 'end Object/Array'}
    // ... } ....
    // ... ] ....
    if c in ['}',']'] then begin // ... } ...
                                 //     ^BufferPos

      //Reset the CurrIndex
      CurrIndex := -1;

      //error if Paths.Count = 0 (mean one end object/array without any starting)
      if assigned(ObjectPaths) then begin
        if (ObjectPaths.Count = 0) then ALJSONDocErrorW(ALJSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then ALJSONDocErrorW(ALJSONParseError);
      end;

      // Declare LNodeType
      var LNodeType: TALJSONNodeType;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init LNode to one level up
        var LNode: TALJSONNodeW;
        if assigned(ObjectPaths) then LNode := ObjectPaths[ObjectPaths.Count - 1].Value
        else LNode := TALJSONNodeW(NamePaths.Objects[NamePaths.Count - 1]);

        //if LNode <> WorkingNode aie aie aie
        if (LNode <> WorkingNode) then ALJSONDocErrorW(ALJSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then ALJSONDocErrorW(ALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then ALJSONDocErrorW(ALJSONParseError);

        //if working node <> Self then we can go to one level up
        If WorkingNode <> Self then begin

          //update CurrIndex if WorkingNode.NodeType = ntArray
          if assigned(ObjectPaths) then begin
            WorkingNode := ObjectPaths[ObjectPaths.Count - 2].Value;
            if WorkingNode.NodeType = ntArray then CurrIndex := ObjectPaths[Objectpaths.Count - 1].Key + 1;
          end
          else begin
            WorkingNode := TALJSONNodeW(NamePaths.Objects[NamePaths.Count - 2]);
            if WorkingNode.NodeType = ntArray then CurrIndex := _extractLastIndexFromNamePath + 1;
          end;

        end

        //if working node = Self then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

         //calculate anodeTypeInt
        LNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then ALJSONDocErrorW(ALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then ALJSONDocErrorW(ALJSONParseError);

        //update CurrIndex if WorkingNode.NodeType = ntArray
        if (Namepaths.Count >= 2) and
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 2]) = ntarray) then CurrIndex := _extractLastIndexFromNamePath + 1;

        //call the DoParseEndObject/array event
        if LNodeType = ntObject then _DoParseEndObject
        else _DoParseEndArray;

      end;

      //delete the last entry from the path
      if assigned(ObjectPaths) then ObjectPaths.Delete(ObjectPaths.Count - 1)
      else NamePaths.Delete(NamePaths.Count - 1);

      //update BufferPos
      BufferPos := BufferPos + 1; // ... } ...
                                  //      ^BufferPos

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'begin Object/Array Without NAME'}
    // ... { ....
    // ... [ ....
    if c in ['{','['] then begin // ... { ...
                                 //     ^BufferPos

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then ALJSONDocErrorW(ALJSONParseError);

        //Node without name can be ONLY present inside an array node
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then ALJSONDocErrorW(ALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
        var LNode: TALJSONNodeW;
        if c = '{' then LNode := CreateNode('', ntObject)
        else LNode := CreateNode('', ntarray);
        try
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := LNode;

        //update the path
        if assigned(ObjectPaths) then ObjectPaths.Add(TPair<Integer, TALJSONNodeW>.create(CurrIndex, WorkingNode))
        else _AddItemToNamePath(CurrIndex, '', WorkingNode);

      end

      //if we are in sax mode
      else begin

          //Node without name can be ONLY present inside an array node
          if (CurrIndex < 0) or
             (NamePaths.Count = 0) or
             (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then ALJSONDocErrorW(ALJSONParseError);

        //update the path
        var LNodeType: TALJSONNodeType;
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrIndex, '', pointer(LNodeType));

        //call the DoParseEndObject/array event
        if LNodeType = ntObject then _DoParseStartObject('')
        else _DoParseStartArray('');

      end;

      //Update CurrIndex
      if c = '{' then CurrIndex := -1
      else CurrIndex := 0;

      //update BufferPos
      BufferPos := BufferPos + 1; // ... { ...
                                  //      ^BufferPos

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'extract the quoted name part'}
    // "" : ""
    // "name" : "value"
    // "name" : 1.23
    // "name" : true
    // "name" : false
    // "name" : null
    // "name" : ISODATE('1/1/2001')
    // "name" : function(){return(new Date).getTime()}, ...}
    // "name" : new Date(''Dec 03, 1924'')
    // "name" : { ... }
    // "name" : [ ... ]
    // 'name' : '...'
    // "value"
    // 'value'
    var LQuoteChar: Char := #0;
    if c in ['"',''''] then begin  // ... " ...
                                   //     ^BufferPos
      LQuoteChar := c; // "
      var P1 := BufferPos + 1; // ... "...\"..."
                               //      ^P1
      While P1 <= BufferLength do begin

       c := Buffer[P1];

       If (c = '\') and
          (P1 < BufferLength) and
          (Buffer[P1 + 1] in ['\', LQuoteChar]) then inc(p1, 2) // ... "...\"..."
                                                                //         ^^^P1
       else if c = LQuoteChar then begin
         ALCopyStr(Buffer,CurrName,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALJavascriptDecodeInPlace(CurrName); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorW(ALJSONParseError);
      BufferPos := P1 + 1; // ... "...\"..."
                           //      ^^^^^^^^^^BufferPos
    end
    {$ENDREGION}

    {$REGION 'extract the unquoted name part'}
    // name : "value"
    // name : 1.23
    // name : true
    // name : false
    // name : null
    // name : ISODATE('1/1/2001')
    // name : function(){return(new Date).getTime()}, ...}
    // name : new Date('Dec 03, 1924')
    // name : { ... }
    // name : [ ... ]
    // 1.23
    // true
    // false
    // null
    // ISODATE('1/1/2001')
    // function(){return(new Date).getTime()}, ...}
    // new Date('Dec 03, 1924')
    else begin

      var LInSingleQuote := False;
      var LInDoubleQuote := False;
      var LInSquareBracket := 0;
      var LInRoundBracket := 0;
      var LInCurlyBracket := 0;

      While (BufferPos <= BufferLength) do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then ALJSONDocErrorW(ALJSONParseError);

      var P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
                           //     ^P1
      While (P1 <= BufferLength) do begin

        c := Buffer[P1];

        if (not LInSingleQuote) and
           (not LInDoubleQuote) and
           (LInSquareBracket = 0) and
           (LInRoundBracket = 0) and
           (LInCurlyBracket = 0) and
           (c in [',', '}', ']', ':']) then begin
          var P2 := P1-1;
          While P2 >= BufferPos do begin
            If Buffer[P2] <= ' ' then dec(P2)
            else break;
          end;
          ALCopyStr(Buffer,CurrName,BufferPos,P2-BufferPos+1); // new Date('Dec 03, 1924')
          break;
        end
        else if (c = '"') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInDoubleQuote := (not LInDoubleQuote) and (not LInSingleQuote);
        end
        else if (c = '''') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInSingleQuote := (not LInSingleQuote) and (not LInDoubleQuote)
        end
        else if (not LInSingleQuote) and
                (not LInDoubleQuote) then begin
          if (c = '[') then inc(LInSquareBracket)
          else if (c = ']') then dec(LInSquareBracket)
          else if (c = '(') then inc(LInRoundBracket)
          else if (c = ')') then dec(LInRoundBracket)
          else if (c = '{') then inc(LInCurlyBracket)
          else if (c = '}') then dec(LInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorW(ALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos

    end;
    {$ENDREGION}

    {$REGION 'extract the name value separator part'}
    var LNameValueSeparator: Char := #0;
    While (BufferPos <= BufferLength) do begin
      If Buffer[BufferPos] <= ' ' then inc(BufferPos)
      else begin
        LNameValueSeparator := Buffer[BufferPos];
        break;
      end;
    end;
    if BufferPos > BufferLength then ALJSONDocErrorW(ALJSONParseError);  // .... : ....
                                                                          //      ^BufferPos
    {$ENDREGION}

    {$REGION 'if aNameValueSeparator is absent then it is just a value'}
    if LNameValueSeparator <> ':' then begin

      //Node without name can be ONLY present inside an array node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then ALJSONDocErrorW(ALJSONParseError);
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then ALJSONDocErrorW(ALJSONParseError);
      end
      else begin
        if (CurrIndex < 0) or
           (NamePaths.Count = 0) or
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then ALJSONDocErrorW(ALJSONParseError);
      end;

      //create the node
      _createNode(CurrIndex,'',CurrName,LQuoteChar in ['"','''']);

      //increase the CurrIndex
      inc(CurrIndex);

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'remove the blank space between the name valueeparator and the value'}
    inc(BufferPos); // ... : ....
                    //      ^BufferPos
    While (BufferPos <= BufferLength) do begin
      If Buffer[BufferPos] <= ' ' then inc(BufferPos)
      else break;
    end;
    if BufferPos > BufferLength then ALJSONDocErrorW(ALJSONParseError); // .... " ....
                                                                         //      ^BufferPos
    {$ENDREGION}

    {$REGION 'init current char (c)'}
    c := Buffer[BufferPos];
    {$ENDREGION}

    {$REGION 'if the value is an object/array'}
    // name : { ... }
    // name : [ ... ]
    if c in ['{','['] then begin // ... { ...
                                 //     ^BufferPos

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then ALJSONDocErrorW(ALJSONParseError);

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0)  or
           (WorkingNode.nodetype <> ntObject) then ALJSONDocErrorW(ALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
        var LNode: TALJSONNodeW;
        if c = '{' then LNode := CreateNode(CurrName, ntObject)
        else LNode := CreateNode(CurrName, ntarray);
        try
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := LNode;

        //update the path
        if assigned(ObjectPaths) then ObjectPaths.Add(TPair<Integer, TALJSONNodeW>.create(-1, WorkingNode))
        else _AddItemToNamePath(-1, CurrName, WorkingNode);

      end

      //if we are in sax mode
      else begin

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0) or
           (NamePaths.Count = 0) or
           (TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) <> ntobject) then ALJSONDocErrorW(ALJSONParseError);

        //update the path
        var LNodeType: TALJSONNodeType;
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(-1, CurrName, pointer(LNodeType));

        //call the DoParseEndObject/array event
        if LNodeType = ntObject then _DoParseStartObject(CurrName)
        else _DoParseStartArray(CurrName);

      end;

      //update the CurrIndex if it's an array
      if c <> '{' then CurrIndex := 0;

      //update BufferPos
      BufferPos := BufferPos + 1; // ... { ...
                                  //      ^BufferPos

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'if the value is a quoted string'}
    // name : "value"
    // name : 'value'
    LQuoteChar := #0;
    if c in ['"',''''] then begin  // ... " ...
                                   //     ^BufferPos

      LQuoteChar := c; // "
      var P1 := BufferPos + 1; // ... "...\"..."
                               //      ^P1
      While P1 <= BufferLength do begin

       c := Buffer[P1];

       If (c = '\') and
          (P1 < BufferLength) and
          (Buffer[P1 + 1] in ['\', LQuoteChar]) then inc(p1, 2) // ... "...\"..."
                                                                //         ^^^P1
       else if c = LQuoteChar then begin
         ALCopyStr(Buffer,currValue,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALJavascriptDecodeInPlace(currValue); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorW(ALJSONParseError);
      BufferPos := P1 + 1; // ... "...\"..."
                           //      ^^^^^^^^^^BufferPos

    end
    {$ENDREGION}

    {$REGION 'if the value is a UNquoted string'}
    // name : 1.23
    // name : true
    // name : false
    // name : null
    // name : ISODATE('1/1/2001')
    // name : function(){return(new Date).getTime()}, ...}
    // name : new Date(''Dec 03, 1924'')
    // name : /test/i
    else begin

      var LInSingleQuote := False;
      var LInDoubleQuote := False;
      var LInSlashQuote := False;
      var LInSquareBracket := 0;
      var LInRoundBracket := 0;
      var LInCurlyBracket := 0;

      While (BufferPos <= BufferLength) do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then ALJSONDocErrorW(ALJSONParseError);

      var P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
                           //     ^P1
      While (P1 <= BufferLength) do begin

        c := Buffer[P1];

        if (not LInSingleQuote) and
           (not LInDoubleQuote) and
           (not LInSlashQuote) and
           (LInSquareBracket = 0) and
           (LInRoundBracket = 0) and
           (LInCurlyBracket = 0) and
           (c in [',', '}', ']']) then begin
          var P2 := P1-1;
          While P2 >= BufferPos do begin
            If Buffer[P2] <= ' ' then dec(P2)
            else break;
          end;
          ALCopyStr(Buffer,currValue,BufferPos,P2-BufferPos+1); // new Date('Dec 03, 1924')
          break;
        end
        else if (c = '"') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInDoubleQuote := (not LInDoubleQuote) and (not LInSingleQuote) and (not LInSlashQuote);
        end
        else if (c = '''') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInSingleQuote := (not LInSingleQuote) and (not LInDoubleQuote) and (not LInSlashQuote);
        end
        else if (c = '/') then begin
          if (P1 <= 1) or
             (Buffer[P1 - 1] <> '\') then LInSlashQuote := (not LInSingleQuote) and (not LInDoubleQuote) and (not LInSlashQuote);
        end
        else if (not LInSingleQuote) and
                (not LInDoubleQuote) and
                (not LInSlashQuote) then begin
          if (c = '[') then inc(LInSquareBracket)
          else if (c = ']') then dec(LInSquareBracket)
          else if (c = '(') then inc(LInRoundBracket)
          else if (c = ')') then dec(LInRoundBracket)
          else if (c = '{') then inc(LInCurlyBracket)
          else if (c = '}') then dec(LInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorW(ALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos


    end;
    {$ENDREGION}

    {$REGION 'create the named text node'}

    //Node withe name MUST be ONLY present inside an object node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(ALJSONParseError);
      if (CurrIndex >= 0)  or
         (WorkingNode.nodetype <> ntObject) then ALJSONDocErrorW(ALJSONParseError);
    end
    else begin
      if (CurrIndex >= 0) or
         (NamePaths.Count = 0) or
         (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntObject) then ALJSONDocErrorW(ALJSONParseError);
    end;

    //create the node
    _createNode(currIndex,CurrName,CurrValue,LQuoteChar in ['"','''']);

    {$ENDREGION}

  end;

begin

  //clear the childnodes
  if poClearChildNodes in Options then ChildNodes.Clear;

  //init WorkingNode and NotSaxMode and DecodeJSONReferences
  WorkingNode := Self;
  NotSaxMode := not SaxMode;
  DecodeJSONReferences := not (poIgnoreControlCharacters in Options);

  //init ObjectPaths or NamePaths
  if (NotSaxMode) and
     (not assigned(OnParseText)) and
     (not assigned(OnParseStartObject)) and
     (not assigned(OnParseEndObject)) and
     (not assigned(OnParseStartArray)) and
     (not assigned(OnParseEndArray)) then begin
    ObjectPaths := TList<TPair<Integer, TALJSONNodeW>>.Create;
    NamePaths := nil;
  end
  else begin
    ObjectPaths := nil;
    NamePaths := TALNVStringListW.Create;
  end;
  Try

    //init Buffer
    BufferLength := length(Buffer);
    BufferPos := 1;

    //add first node in ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.Add(TPair<Integer, TALJSONNodeW>.create(-1, WorkingNode))
    else begin
      if NotSaxMode then _AddNameItemToNamePath('', WorkingNode)
      else _AddNameItemToNamePath('', pointer(NodeType));
    end;

    //skip the first {
    While (BufferPos <= BufferLength) do begin
      var c: Char := Buffer[BufferPos];
      If c <= ' ' then inc(bufferPos)
      else begin
        if (c = '{') then begin
          if (Nodetype <> ntObject) then ALJSONDocErrorW(ALJSONOperationError,GetNodeType);
          CurrIndex := -1;
          if not notSaxMode then _DoParseStartObject('');
        end
        else if (c = '[') then begin
          if (Nodetype <> ntArray) then ALJSONDocErrorW(ALJSONOperationError,GetNodeType);
          CurrIndex := 0;
          if not notSaxMode then _DoParseStartArray('');
        end
        else AlJSONDocErrorW(ALJSONParseError);
        inc(bufferPos);
        break;
      end;
    end;

    //analyze all the nodes
    if poAllowComments in Options then begin
      var InCommentLine: Integer := 0;
      While (BufferPos <= BufferLength) do begin
        var c: Char := Buffer[BufferPos];
        If (InCommentLine = 0) and ((c <= ' ') or (c = ',')) then inc(bufferPos)
        else if (InCommentLine <= 1) and (c = '/')  then begin
          inc(InCommentLine);
          inc(bufferPos);
        end
        else if (InCommentLine = 2) then begin
          if ((c = #13) or (c = #10)) then InCommentLine := 0;
          inc(bufferPos);
        end
        else begin
          if InCommentLine = 1 then begin
            InCommentLine := 0;
            dec(BufferPos);
          end;
          AnalyzeNode;
        end;
      end;
    end
    else begin
      While (BufferPos <= BufferLength) do begin
        var c: Char := Buffer[BufferPos];
        If (c <= ' ') or (c = ',') then inc(bufferPos)
        else AnalyzeNode;
      end;
    end;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then ALJSONDocErrorW(ALJSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then ALJSONDocErrorW(ALJSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if NotSaxMode and (WorkingNode <> nil) then ALJSONDocErrorW(ALJSONParseError);

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ALFreeAndNil(ObjectPaths)
    else ALFreeAndNil(NamePaths);

  end;

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*************************************************************}
{Last version of the spec: http://bsonspec.org/#/specification}
procedure TALJSONNodeW.ParseBSON(
            const RawBSONStream: TStream;
            const RawBSONBytes: TBytes;
            const SaxMode: Boolean;
            const OnParseText: TALJSONParseTextEventW;
            const OnParseStartObject: TALJSONParseObjectEventW;
            const OnParseEndObject: TALJSONParseObjectEventW;
            const OnParseStartArray: TALJSONParseArrayEventW;
            const OnParseEndArray: TALJSONParseArrayEventW;
            const Options: TALJSONParseOptions);

Const
  BufferSize: Integer = 32768;

Var
  Buffer: TBytes;
  BufferLength: Integer;
  BufferPos: Integer;
  CurrName: String;
  NotSaxMode: Boolean;
  BinaryAsPtrStream: Boolean;
  WorkingNode: TALJSONNodeW;
  NamePaths: TALStringListW;
  ObjectPaths: TObjectList<TALJSONNodeW>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer: Boolean; overload;
  begin
    if not assigned(RawBSONStream) then begin
      Result := false;
      exit;
    end;

    var Byte2Read: Integer;
    If (BufferLength > 0) and (BufferPos > 0) then begin
      if (BufferPos > BufferLength) then RawBSONStream.Position := RawBSONStream.Position - BufferLength + BufferPos;
      Byte2Read := min(BufferPos, BufferLength);
      if BufferPos < BufferLength then
        ALMove(
          PByte(Buffer)[BufferPos],
          pointer(Buffer)^,
          BufferLength-BufferPos);
      BufferPos := 0;
    end
    else begin
      Byte2Read := BufferSize;
      BufferLength := BufferLength + BufferSize;
      SetLength(Buffer, BufferLength);
    end;

    //range check error is we not do so
    var ByteReaded: Integer;
    if RawBSONStream.Position < RawBSONStream.Size then ByteReaded := RawBSONStream.Read(PByte(Buffer)[BufferLength - Byte2Read{+ 1 - 1}],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      BufferLength := BufferLength - Byte2Read + ByteReaded;
      SetLength(Buffer, BufferLength);
      Result := ByteReaded > 0;
    end
    else Result := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer(var PosToKeepSync: Integer): Boolean; overload;
  begin
    var P1 := BufferPos;
    Result := ExpandBuffer;
    PosToKeepSync := PosToKeepSync - (P1 - BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(const ExtraItems: String = ''): String;
  begin
    var LB: Char := ALDefaultJsonPathSeparatorW;
    var Size: Integer := length(ExtraItems);
    if size <> 0 then Inc(Size, 1{length(LB)});
    for var I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths[I]) + 1{length(LB)});
    SetLength(Result, Size);
    var P: Integer := 1;
    for var I := 1 to NamePaths.Count - 1 do begin
      var S: String := NamePaths[I];
      var L: Integer := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, PByte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
        Inc(P, L);
      end;
      L := 1{length(LB)};
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNodeW(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJSONNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, PByte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      var L: Integer := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, PByte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
      Inc(P, L);
    end;
    setlength(result,P-1);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(
              const index: String;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr('[' + index + ']'), '', Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithName(
              const name: String;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr(Name), Name, Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseText(
              const NameOrIndex: String;
              const Args: array of const;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(OnParseText) then begin
      if notSaxMode then begin
        if WorkingNode.nodetype=ntarray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
      end
      else begin
        if NamePaths.Count = 0 then AlJSONDocErrorW(ALJSONParseError);
        if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntArray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartObject(const Name: String);
  begin
    if Assigned(OnParseStartObject) then OnParseStartObject(Self, GetPathStr, Name);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndObject;
  begin
    if Assigned(OnParseEndObject) then begin
      if NamePaths.Count = 0 then AlJSONDocErrorW(ALJSONParseError);
      OnParseEndObject(Self, GetPathStr, NamePaths[NamePaths.Count - 1]);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: String);
  begin
    if Assigned(OnParseStartArray) then OnParseStartArray(Self, GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if Assigned(OnParseEndArray) then begin
      if NamePaths.Count = 0 then AlJSONDocErrorW(ALJSONParseError);
      OnParseEndArray(Self, GetPathStr, NamePaths[NamePaths.Count - 1]);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexItemToNamePath(const index: String; Obj: Pointer);
  begin
    NamePaths.AddObject('[' + Index + ']', Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddNameItemToNamePath(const name: String; Obj: Pointer);
  begin
    NamePaths.AddObject(Name, Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddItemToNamePath(const nameOrIndex: String; Obj: Pointer);
  begin
    if notSaxMode then begin
      if WorkingNode.nodetype=ntarray then _AddIndexItemToNamePath(nameOrIndex, Obj)
      else _AddNameItemToNamePath(nameOrIndex, Obj);
    end
    else begin
      if NamePaths.Count = 0 then AlJSONDocErrorW(ALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(nameOrIndex, Obj)
      else _AddNameItemToNamePath(nameOrIndex, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt64Node(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt64: System.Int64;
    if BufferPos > BufferLength - sizeof(LInt64) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) then AlJSONDocErrorW(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt64, sizeof(LInt64));
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetInt64(LInt64);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LInt64], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt32Node(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt32: System.Int32;
    if BufferPos > BufferLength - sizeof(LInt32) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) then AlJSONDocErrorW(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetInt32(LInt32);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LInt32], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTextNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt32: System.Int32;
    if BufferPos > BufferLength - sizeof(LInt32) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) then AlJSONDocErrorW(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    while (BufferPos + LInt32 > BufferLength) do
      if not ExpandBuffer then AlJSONDocErrorW(ALBSONParseError);
    var LText := TEncoding.UTF8.GetString(Buffer,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(LText);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LText], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createFloatNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LDouble: Double;
    if BufferPos > BufferLength - sizeof(Double) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(Double) then AlJSONDocErrorW(ALBSONParseError);
    end;
    ALMove(pByte(Buffer)[BufferPos], LDouble, sizeof(Double));
    BufferPos := BufferPos + sizeof(Double);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetFloat(LDouble);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LDouble], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBinaryNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    //Get size
    var LInt32: System.Int32;
    if BufferPos > BufferLength - sizeof(LInt32) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) then AlJSONDocErrorW(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    //Get the subtype
    if BufferPos >= BufferLength then begin
      ExpandBuffer;
      if BufferPos >= BufferLength then AlJSONDocErrorW(ALBSONParseError);
    end;
    var LBinSubtype: Byte := Buffer[BufferPos];
    BufferPos := BufferPos + 1;

    //BinaryAsStream
    if BinaryAsPtrStream then begin

      //Get the data
      var LPointerStream: TPointerStream;
      if RawBSONStream <> nil then begin
        if RawBSONStream.Position - BufferLength + BufferPos + LInt32 > RawBSONStream.Size then AlJSONDocErrorW(ALBSONParseError);
        LPointerStream := TPointerStream.Create(
                            @PByte(TCustomMemoryStream(RawBSONStream).Memory)[RawBSONStream.Position - BufferLength + BufferPos], //Ptr: Pointer;
                            LInt32, // const Size: NativeInt;
                            true); // ReadOnly: Boolean)
      end
      else begin
        if BufferPos + LInt32 > BufferLength then AlJSONDocErrorW(ALBSONParseError);
        LPointerStream := TPointerStream.Create(
                            @PByte(RawBSONBytes)[BufferPos], //Ptr: Pointer;
                            LInt32, // const Size: NativeInt;
                            true); // ReadOnly: Boolean)
      end;
      BufferPos := BufferPos + LInt32;

      //create the node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetBinaryAsStream(LPointerStream);
          LNode.SetBinarySubType(LBinSubtype);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(Name, [LPointerStream, LBinSubtype], NodeSubType);
      end;

    end

    //BinaryAsBytes
    else begin

      //Get the data
      while (BufferPos + LInt32 > BufferLength) do
        if not ExpandBuffer then AlJSONDocErrorW(ALBSONParseError);
      var LBinData: TBytes;
      setlength(LBinData, LInt32);
      if LInt32 > 0 then
        ALMove(PByte(Buffer)[BufferPos], pointer(LBinData)^, LInt32);
      BufferPos := BufferPos + LInt32;

      //create the node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
        var LNode: TALJSONNodeW;
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetBinaryAsBytes(LBinData);
          LNode.SetBinarySubType(LBinSubtype);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
      end
      else begin
        _DoParseText(Name, [LBinData, LBinSubtype], NodeSubType);
      end;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createObjectIDNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if BufferPos > BufferLength - 12{length(aObjectID)} then begin
      ExpandBuffer;
      if BufferPos > BufferLength - 12{length(aObjectID)} then AlJSONDocErrorW(ALBSONParseError);
    end;
    var LObjectID: TBytes;
    Setlength(LObjectID, 12); // ObjectId is a 12-Byte BSON type
    ALMove(PByte(Buffer)[BufferPos], pByte(LObjectID)[0], 12{length(aObjectID)});
    BufferPos := BufferPos + 12{length(aObjectID)};

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetObjectID(LObjectID);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LObjectID], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBooleanNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if BufferPos >= BufferLength then begin
      ExpandBuffer;
      if BufferPos >= BufferLength then AlJSONDocErrorW(ALBSONParseError);
    end;
    var LBool: Boolean;
    if Buffer[BufferPos] = $00 then LBool := False
    else if Buffer[BufferPos] = $01 then LBool := true
    else begin
      AlJSONDocErrorW(ALBSONParseError);
      LBool := False; // to hide a warning;
    end;
    BufferPos := BufferPos + 1;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setbool(LBool);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LBool], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createDateTimeNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt64: System.Int64;
    if BufferPos > BufferLength - sizeof(LInt64) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) then AlJSONDocErrorW(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt64, sizeof(LInt64));
    var LDateTime: TDateTime := ALUnixMsToDateTime(LInt64);
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setdatetime(LDateTime);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LDateTime], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTimestampNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt64: System.Int64;
    if BufferPos > BufferLength - sizeof(LInt64) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) then AlJSONDocErrorW(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt64, sizeof(LInt64));
    var LTimestamp: TALBSONTimestamp;
    LTimestamp.I64 := LInt64;
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetTimestamp(LTimestamp);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LTimestamp.W1, LTimestamp.W2], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createnullNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setnull(true);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, ['null'], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createRegExNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    //Get pattern
    var LRegEx: String;
    var P1: Integer := BufferPos;
    While (P1 < BufferLength) or ExpandBuffer(P1) do begin
      If Buffer[P1] <> $00 then inc(P1)
      else begin
        LRegEx := TEncoding.UTF8.GetString(Buffer, BufferPos, P1 - BufferPos);
        break;
      end;
    end;
    if P1 >= BufferLength then AlJSONDocErrorW(ALBSONParseError);
    BufferPos := P1 + 1;
    if BufferPos >= BufferLength then ExpandBuffer;

    //Get options
    var LRegExOptions: TALPerlRegExOptions := [];
    While (BufferPos < BufferLength) or ExpandBuffer do begin
      case Buffer[BufferPos] of
        ord('i'): LRegExOptions := LRegExOptions + [preCaseLess];
        ord('m'): LRegExOptions := LRegExOptions + [preMultiLine];
        ord('x'): LRegExOptions := LRegExOptions + [preExtended];
        ord('l'):;
        ord('s'): LRegExOptions := LRegExOptions + [preSingleLine];
        ord('u'):;
        $00: break;
      end;
      inc(BufferPos);
    end;
    if BufferPos >= BufferLength then AlJSONDocErrorW(ALBSONParseError);
    inc(BufferPos);
    if BufferPos >= BufferLength then ExpandBuffer;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetRegEx(LRegEx);
        LNode.SetRegExOptions(LRegExOptions);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LRegEx, Byte(LRegExOptions)], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createJavascriptNode(
              const name: String;
              const NodeSubType: TALJSONNodeSubType);
  begin
    var LInt32: System.Int32;
    if BufferPos > BufferLength - sizeof(LInt32) then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) then AlJSONDocErrorW(ALBSONParseError);
    end;
    ALMove(PByte(Buffer)[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    while (BufferPos + LInt32 > BufferLength) do
      if not ExpandBuffer then AlJSONDocErrorW(ALBSONParseError);
    var LJavascript := TEncoding.UTF8.GetString(Buffer,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);
      var LNode: TALJSONNodeW;
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(LJavascript);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
    end
    else begin
      _DoParseText(Name, [LJavascript], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  begin

    {$REGION 'init current char (c)'}
    var c: Byte := Buffer[BufferPos];
    {$ENDREGION}

    {$REGION 'End Object/Array'}
    // ... } ....
    // ... ] ....
    if c = $00 then begin

      //error if Paths.Count = 0 (mean one end object/array without any starting)
      if assigned(ObjectPaths) then begin
        if (ObjectPaths.Count = 0) then AlJSONDocErrorW(ALBSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then AlJSONDocErrorW(ALBSONParseError);
      end;

      // Declare LNodeType
      var LNodeType: TALJSONNodeType;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init LNode to one level up
        var LNode: TALJSONNodeW;
        if assigned(ObjectPaths) then LNode := ObjectPaths[ObjectPaths.Count - 1]
        else LNode := TALJSONNodeW(NamePaths.Objects[NamePaths.Count - 1]);

        //if LNode <> WorkingNode aie aie aie
        if (LNode <> WorkingNode) then AlJSONDocErrorW(ALBSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then AlJSONDocErrorW(ALBSONParseError);

        //if working node <> Self then we can go to one level up
        If WorkingNode <> Self then begin

          //init WorkingNode to the parentNode
          if assigned(ObjectPaths) then WorkingNode := ObjectPaths[ObjectPaths.Count - 2]
          else WorkingNode := TALJSONNodeW(NamePaths.Objects[NamePaths.Count - 2]);

        end

        //if working node = Self then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

        //calculate anodeTypeInt
        LNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then AlJSONDocErrorW(ALBSONParseError);

        //call the DoParseEndObject/array event
        if LNodeType = ntObject then _DoParseEndObject
        else _DoParseEndArray;

      end;

      //delete the last entry from the path
      if assigned(ObjectPaths) then ObjectPaths.Delete(ObjectPaths.Count - 1)
      else NamePaths.Delete(NamePaths.Count - 1);

      //update BufferPos
      BufferPos := BufferPos + 1;

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'Get the node sub type'}
    var LNodeSubType: TALJSONNodeSubType;
    case c of
      $01: LNodeSubType := nstFloat;
      $02: LNodeSubType := nstText;
      $03: LNodeSubType := nstObject;
      $04: LNodeSubType := nstArray;
      $05: LNodeSubType := nstbinary;
      $07: LNodeSubType := nstObjectID;
      $08: LNodeSubType := nstBoolean;
      $09: LNodeSubType := nstDateTime;
      $0A: LNodeSubType := nstNull;
      $0B: LNodeSubType := nstRegEx;
      $0D: LNodeSubType := nstJavascript;
      $10: LNodeSubType := nstInt32;
      $11: LNodeSubType := nstTimestamp;
      $12: LNodeSubType := nstInt64;
      else AlJSONDocErrorW(ALBSONParseError);
    end;
    BufferPos := BufferPos + 1;
    If BufferPos >= BufferLength then ExpandBuffer;
    {$ENDREGION}

    {$REGION 'Get the node name'}
    var P1: Integer := BufferPos;
    While (P1 < BufferLength) or ExpandBuffer(P1) do begin
      If Buffer[P1] <> $00 then inc(P1)
      else begin
        CurrName := TEncoding.UTF8.GetString(Buffer, BufferPos, P1-BufferPos);
        break;
      end;
    end;
    if P1 >= BufferLength then AlJSONDocErrorW(ALBSONParseError);
    BufferPos := P1 + 1;
    if BufferPos >= BufferLength then ExpandBuffer;
    {$ENDREGION}

    {$REGION 'begin Object/Array'}
    // ... { ....
    // ... [ ....
    if LNodeSubType in [nstObject,nstArray] then begin

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then AlJSONDocErrorW(ALBSONParseError);

        //create the node according the the braket char and add it to the workingnode
        var LNode: TALJSONNodeW;
        if LNodeSubType = nstObject then begin
          if WorkingNode.nodetype=ntarray then LNode := CreateNode('', ntObject)
          else LNode := CreateNode(CurrName, ntObject);
        end
        else begin
          if WorkingNode.nodetype=ntarray then LNode := CreateNode('', ntarray)
          else LNode := CreateNode(CurrName, ntarray);
        end;
        try
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;

        //set that the current working node will be now the new node newly created
        WorkingNode := LNode;

        //update the path
        if assigned(ObjectPaths) then ObjectPaths.Add(WorkingNode)
        else _AddItemToNamePath(CurrName, WorkingNode);

      end

      //if we are in sax mode
      else begin

        //update the path
        var LNodeType: TALJSONNodeType;
        if LNodeSubType = nstObject then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrName, pointer(LNodeType));

        //call the DoParseStartObject/array event
        if LNodeSubType = nstObject then _DoParseStartObject(CurrName)
        else _DoParseStartArray(CurrName);

      end;

      //update BufferPos
      BufferPos := BufferPos + 4; // we don't need the size of the object/array (4 Bytes)

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'create the node'}
    case LNodeSubType of
      // \x01 + name + \x00 + double
      nstFloat: _createFloatNode(CurrName, LNodeSubType);

      // \x02 + name + \x00 + length (Int32) + string + \x00
      nstText: _createTextNode(CurrName, LNodeSubType);

      // \x05 + name + \x00 + Int32 + subtype + (Byte*)
      nstbinary: _createBinaryNode(CurrName, LNodeSubType);

      // \x07 + name + \x00 + (Byte*12)
      nstObjectID: _createObjectIDNode(CurrName, LNodeSubType);

      // \x08 + name + \x00 + \x00 => Boolean "false"
      // \x08 + name + \x00 + \x01	=> Boolean "true"
      nstBoolean: _createBooleanNode(CurrName, LNodeSubType);

      // \x09 + name + \x00 + Int64
      nstDateTime: _createDateTimeNode(CurrName, LNodeSubType);

      // \x11 + name + \x00 + Int64
      nstTimestamp: _createTimestampNode(CurrName, LNodeSubType);

      // \x0A + name + \x00
      nstnull: _createNullNode(CurrName, LNodeSubType);

      // \x0B + name + \x00 + (Byte*) + \x00 + (Byte*) + \x00
      nstRegEx: _createRegExNode(CurrName, LNodeSubType);

      // \x0D + name + \x00 + length (Int32) + string + \x00
      nstJavascript: _createJavascriptNode(CurrName, LNodeSubType);

      // \x10 + name + \x00 + Int32
      nstInt32: _createInt32Node(CurrName, LNodeSubType);

      // \x12 + name + \x00 + Int64
      nstInt64: _createInt64Node(CurrName, LNodeSubType);

      else AlJSONDocErrorW(ALBSONParseError);
    end;
    {$ENDREGION}

  end;

begin

  //Only Object Node can be loaded from BSON
  If NodeType <> ntObject then AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
  if poClearChildNodes in Options then ChildNodes.Clear;
  BinaryAsPtrStream := poBinaryAsPtrStream in Options;
  if (BinaryAsPtrStream) and
     (RawBSONStream <> nil) and
     (RawBSONStream is not TCustomMemoryStream) then
    AlJSONDocErrorW(ALJSONBinaryAsPtrStreamInvalidSource);

  //init WorkingNode and NotSaxMode
  WorkingNode := Self;
  NotSaxMode := not SaxMode;

  //init ObjectPaths or NamePaths
  if (NotSaxMode) and
     (not assigned(OnParseText)) and
     (not assigned(OnParseStartObject)) and
     (not assigned(OnParseEndObject)) and
     (not assigned(OnParseStartArray)) and
     (not assigned(OnParseEndArray)) then begin
    ObjectPaths := TObjectList<TALJSONNodeW>.Create(false{OwnsObjects});
    NamePaths := nil;
  end
  else begin
    ObjectPaths := nil;
    NamePaths := TALStringListW.Create;
  end;
  Try

    //init Buffer
    if assigned(RawBSONStream) then begin
      Buffer := nil;
      BufferLength := 0;
      BufferPos := 4; // the first 4 Bytes are the length of the document and we don't need it
      ExpandBuffer;
    end
    else begin
      Buffer := RawBSONBytes;
      BufferLength := length(RawBSONBytes);
      BufferPos := 4; // the first 4 Bytes are the length of the document and we don't need it
    end;

    //add first node in ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.Add(WorkingNode)
    else begin
      if NotSaxMode then NamePaths.AddObject('', WorkingNode)
      else NamePaths.AddObject('', pointer(ntObject));
    end;
    if not notSaxMode then _DoParseStartObject('');

    //analyze all the nodes
    While (BufferPos < BufferLength) or ExpandBuffer do
      AnalyzeNode;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then AlJSONDocErrorW(ALBSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then AlJSONDocErrorW(ALBSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if NotSaxMode and (WorkingNode <> nil) then AlJSONDocErrorW(ALBSONParseError);

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ALFreeAndNil(ObjectPaths)
    else ALFreeAndNil(NamePaths);

  end;

end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
procedure TALJSONNodeW.SaveToJSON(
            const Stream: TStream;
            const StreamEncoding: TEncoding;
            var Buffer: String;
            const Options: TALJSONSaveOptions);

type
  TNodeStackEntry = record
    Node: TALJSONNodeW;
    ParentNode: TALJSONNodeW;
  end;

Const
  BufferSize: Integer = 32768;

Var
  NodeStack: Tstack<TNodeStackEntry>;
  CurrentIndentStr: String;
  IndentStr: String;
  EncodeControlCharacters: Boolean;
  SkipNodeSubTypeHelper: Boolean;
  SaveInt64AsText: Boolean;
  AutoIndentNode: Boolean;
  BufferPos: Integer;
  LastWrittenChar: Char;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteBuffer2Stream(const buffer: String; BufferLength: Integer);
  begin
    if assigned(Stream) then begin
      If BufferLength > 0 then begin
        var LBytes: TBytes := StreamEncoding.GetBytes(TCharArray(buffer), 0{CharIndex}, BufferLength{CharCount});
        stream.Writebuffer(pointer(LBytes)^,length(LBytes));
      end;
      BufferPos := 0;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _Write2Buffer(const Source; Count: NativeInt);
  begin
    if Count = 0 then exit;
    if Count + BufferPos > length(Buffer) then setlength(Buffer, Count + BufferPos + BufferSize);
    ALMove(Source, pByte(Buffer)[BufferPos*sizeOf(Char)], Count*sizeOf(Char));
    BufferPos := BufferPos + Count;
    if BufferPos >= 65536 then _WriteBuffer2Stream(Buffer,BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStr2Buffer(const str:String);
  begin
    var L: Integer := Length(Str);
    if L = 0 then exit;
    LastWrittenChar := Str[L];
    _Write2Buffer(pointer(str)^,L);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteTextNode2Buffer(aTextNode:TALJSONNodeW; aParentNode: TALJSONNodeW);
  begin
    with aTextNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode then begin
        _WriteStr2Buffer(#13#10);
        _WriteStr2Buffer(CurrentIndentStr);
      end;

      if (assigned(AParentNode)) and
         (AParentNode.NodeType <> ntArray) then begin
        _WriteStr2Buffer('"');
        if EncodeControlCharacters then _WriteStr2Buffer(ALJavascriptEncode(NodeName))
        else _WriteStr2Buffer(NodeName);
        if AutoIndentNode then _WriteStr2Buffer('": ')
        else _WriteStr2Buffer('":');
      end;

      if (NodeSubType = NstText) or
         ((NodeSubType = nstInt64) and SaveInt64AsText) then begin
        if (NodeSubType = NstText) and EncodeControlCharacters then begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(ALJavascriptEncode(GetText));
          _WriteStr2Buffer('"');
        end
        else begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(Text);
          _WriteStr2Buffer('"');
        end;
      end
      else _WriteStr2Buffer(GetInterchangeValue(SkipNodeSubTypeHelper));

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNodeW; aParentNode: TALJSONNodeW);
  begin
    with aObjectNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode and (CurrentIndentStr <> '') then begin
         _WriteStr2Buffer(#13#10);
         _WriteStr2Buffer(CurrentIndentStr);
      end;

      if aObjectNode = self then _WriteStr2Buffer('{')
      else if (assigned(AParentNode)) and
              (AParentNode.NodeType <> ntArray) then begin
        _WriteStr2Buffer('"');
        if EncodeControlCharacters then _WriteStr2Buffer(ALJavascriptEncode(NodeName))
        else _WriteStr2Buffer(NodeName);
        if AutoIndentNode then _WriteStr2Buffer('": {')
        else _WriteStr2Buffer('":{');
      end
      else _WriteStr2Buffer('{');

      var LEmptyNode := True;
      var LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            var LNodeStackEntry: TNodeStackEntry;
            LNodeStackEntry.Node := aObjectNode;
            LNodeStackEntry.ParentNode := aObjectNode;
            NodeStack.Push(LNodeStackEntry);
            For var I := Count - 1 downto 0 do begin
              LNodeStackEntry.Node := Nodes[I];
              LNodeStackEntry.ParentNode := aObjectNode;
              NodeStack.Push(LNodeStackEntry);
            end;
          end
      end;

      If LEmptyNode then _WriteStr2Buffer('}')
      else CurrentIndentStr := CurrentIndentStr + IndentStr;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNodeW);
  begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr)+1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer('}');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNodeW; aParentNode: TALJSONNodeW);
  begin
    with aArrayNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode and (CurrentIndentStr <> '') then begin
        _WriteStr2Buffer(#13#10);
        _WriteStr2Buffer(CurrentIndentStr);
      end;

      if aArrayNode = self then _WriteStr2Buffer('[')
      else if (assigned(AParentNode)) and
              (AParentNode.NodeType <> ntArray) then begin
        _WriteStr2Buffer('"');
        if EncodeControlCharacters then _WriteStr2Buffer(ALJavascriptEncode(NodeName))
        else _WriteStr2Buffer(NodeName);
        if AutoIndentNode then _WriteStr2Buffer('": [')
        else _WriteStr2Buffer('":[');
      end
      else _WriteStr2Buffer('[');

      var LEmptyNode := True;
      var LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            var LNodeStackEntry: TNodeStackEntry;
            LNodeStackEntry.Node := aArrayNode;
            LNodeStackEntry.ParentNode := aArrayNode;
            NodeStack.Push(LNodeStackEntry);
            For var I := Count - 1 downto 0 do begin
              LNodeStackEntry.Node := Nodes[I];
              LNodeStackEntry.ParentNode := aArrayNode;
              NodeStack.Push(LNodeStackEntry);
            end;
          end
      end;

      If LEmptyNode then _WriteStr2Buffer(']')
      else CurrentIndentStr := CurrentIndentStr + IndentStr;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNodeW);
  begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr) + 1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer(']');
  end;

begin
  If not (NodeType in [ntObject, ntArray]) then exit;
  NodeStack := Tstack<TNodeStackEntry>.Create;
  Try

    {init buffer string}
    Setlength(Buffer, BufferSize); // will make buffer uniquestring
    BufferPos := 0;
    LastWrittenChar := '{';
    EncodeControlCharacters := not (soIgnoreControlCharacters in Options);
    SkipNodeSubTypeHelper := soSkipNodeSubTypeHelper in Options;
    SaveInt64AsText := SkipNodeSubTypeHelper and (soSaveInt64AsText in Options);
    AutoIndentNode := soNodeAutoIndent in Options;
    IndentStr := ALDefaultJsonNodeIndentW;
    CurrentIndentStr := '';

    {SaveOnlyChildNode}
    var LNodeStackEntry: TNodeStackEntry;
    LNodeStackEntry.Node := self;
    LNodeStackEntry.ParentNode := nil;
    NodeStack.Push(LNodeStackEntry);

    {loop on all nodes}
    While NodeStack.Count > 0 Do begin
      LNodeStackEntry := NodeStack.Pop;
      with LNodeStackEntry.Node do
        case NodeType of
          ntObject: begin
                      if LNodeStackEntry.Node = LNodeStackEntry.ParentNode then _WriteEndObjectNode2Buffer(LNodeStackEntry.Node)
                      else _WriteStartObjectNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode);
                    end;
          ntArray: begin
                      if LNodeStackEntry.Node = LNodeStackEntry.ParentNode then _WriteEndArrayNode2Buffer(LNodeStackEntry.Node)
                      else _WriteStartArrayNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode);
                   end;
          ntText: _WriteTextNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode);
          else ALJSONDocErrorW(ALJSONInvalidNodeType);
        end;
    end;

    {Write the buffer}
    if assigned(Stream) then _WriteBuffer2Stream(Buffer, BufferPos)
    else setlength(Buffer,BufferPos);

  finally
    ALFreeAndNil(NodeStack);
  end;
end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{***********************************}
{Saves the JSON document to a stream.
 Call SaveToStream to save the contents of the JSON document to the stream specified by Stream.}
procedure TALJSONNodeW.SaveToJSONStream(const Stream: TStream; const Encoding: TEncoding; const Options: TALJSONSaveOptions = []);
begin
  var Buffer: String;
  SaveToJSON(Stream, Encoding, buffer, Options);
end;

{*****************************************************************************************************}
procedure TALJSONNodeW.SaveToJSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
begin
  SaveToJSONStream(Stream, TEncoding.UTF8, Options);
end;

{*******************************}
{Saves the JSON document to disk.
 Call SaveToFile to save any modifications you have made to the parsed JSON document.
 AFileName is the name of the file to save.}
procedure TALJSONNodeW.SaveToJSONFile(const FileName: String; const Encoding: TEncoding; const Options: TALJSONSaveOptions = []);
begin
  var LTmpFilename: String;
  if soProtectedSave in Options then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    var LFileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToJSONStream(LFileStream, Encoding, Options);
    finally
      ALFreeAndNil(LFileStream);
    end;

    if LTmpFilename <> FileName then begin
      if TFile.Exists(FileName) then TFile.Delete(FileName);
      TFile.Move(LTmpFilename, FileName);
    end;

  except
    if (LTmpFilename <> FileName) and
       (TFile.Exists(LTmpFilename)) then TFile.Delete(LTmpFilename);
    raise;
  end;
end;

{****************************************************************************************************}
procedure TALJSONNodeW.SaveToJSONFile(const FileName: String; const Options: TALJSONSaveOptions = []);
begin
  SaveToJSONFile(FileName, TEncoding.UTF8, Options);
end;

{*************************************************}
{Saves the JSON document to a string-type variable.
 Call SaveToJSON to save the contents of the JSON document to the string-type variable specified by JSON. SaveToJSON writes the contents of JSON document
 using 8 bits char (utf-8, iso-8859-1, etc) as an encoding system, depending on the type of the JSON parameter.
 Unlike the JSON property, which lets you write individual lines from the JSON document, SaveToJSON writes the entire text of the JSON document.}
procedure TALJSONNodeW.SaveToJSONString(var str: String; const Options: TALJSONSaveOptions = []);
begin
  SaveToJSON(nil, nil, Str, Options);
end;

{********************************}
procedure TALJSONNodeW.SaveToBSON(
            const Stream: TStream;
            var Buffer: TBytes;
            const Options: TALJSONSaveOptions);

type
  TNodeStackEntry = record
    Node: TALJSONNodeW;
    ParentNode: TALJSONNodeW;
    Index: Integer;
    StartPos: System.Int64;
  end;

Const
  BufferSize: Integer = 32768;

Var
  NodeStack: Tstack<TNodeStackEntry>;
  BufferPos: NativeInt;
  StreamPos: system.Int64;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteBuffer2Stream(const buffer: TBytes; BufferLength: Integer);
  begin
    if assigned(Stream) then begin
      If BufferLength > 0 then stream.Writebuffer(pointer(buffer)^,BufferLength);
      BufferPos := 0;
      StreamPos := stream.Position;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _Write2Buffer(const Source; Count: NativeInt);
  begin
    if Count = 0 then exit;
    if Count + BufferPos > length(Buffer) then setlength(Buffer, Count + BufferPos + BufferSize);
    ALMove(Source, Buffer[BufferPos], Count);
    BufferPos := BufferPos + Count;
    if BufferPos >= 65536 then _WriteBuffer2Stream(Buffer,BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteUTF8Str2Buffer(const str:String); overload; inline;
  begin
    var LBytes: TBytes := Tencoding.UTF8.GetBytes(str);
    _Write2Buffer(pointer(LBytes)^,length(LBytes));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteUTF8Str2Buffer(const index:Integer); overload; inline;
  begin
    _WriteUTF8Str2Buffer(ALIntToStrW(index));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteByte2Buffer(const AByte:Byte); inline;
  begin
    _Write2Buffer(aByte, sizeOF(aByte));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteBytes2Buffer(const ABytes: TBytes); inline;
  begin
    _Write2Buffer(pointer(ABytes)^,length(ABytes));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteInt2Pos(const AInt:Integer; const APos: system.Int64);
  begin
    if aPos < StreamPos then begin
      Stream.position := aPos;
      stream.Writebuffer(aInt,sizeof(aInt));
      Stream.position := StreamPos;
    end
    else ALMove(aInt, Buffer[aPos - StreamPos], sizeOf(aInt));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x01 + name + \x00 + double
  procedure _WriteFloatValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    var LDouble: Double := aTextNode.Float;
    _Write2Buffer(LDouble, sizeOf(LDouble));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x02 + name + \x00 + length (Int32) + string + \x00
  procedure _WriteTextValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    var LText: TBytes := Tencoding.UTF8.GetBytes(aTextNode.Text);
    var LInt32: system.Int32 := length(LText) + 1 {for the trailing #0};
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _WriteBytes2Buffer(LText);
    _WriteByte2Buffer($00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x05 + name + \x00 + Int32 + subtype + (Byte*)
  procedure _WriteBinaryValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    var LBinary: TBytes := aTextNode.BinaryAsBytes;
    var LBinarySubType: Byte := aTextNode.BinarySubType;
    var LInt32: system.Int32 := length(LBinary);
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _Write2Buffer(LBinarySubType, sizeOF(LBinarySubType));
    _WriteBytes2Buffer(LBinary);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x07 + name + \x00 + (Byte*12)
  procedure _WriteObjectIDValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    _WriteBytes2Buffer(aTextNode.ObjectID);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x08 + name + \x00 + \x00 => Boolean "false"
  // \x08 + name + \x00 + \x01	=> Boolean "true"
  procedure _WriteBooleanValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    if not aTextNode.bool then _WriteByte2Buffer($00)
    else _WriteByte2Buffer($01);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x09 + name + \x00 + Int64
  procedure _WriteDateTimeValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    var LInt64: system.Int64 := ALDateTimeToUnixMs(aTextNode.DateTime);
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x11 + name + \x00 + Int64
  procedure _WriteTimestampValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    var LInt64: system.Int64 := aTextNode.Timestamp.I64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \xOB + name + \x00 + (Byte*) + \x00 + (Byte*) + \x00
  procedure _WriteRegExValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    var LRegExOptionsStr: String := '';
    var LRegExOptions: TALPerlRegExOptions := aTextNode.RegExOptions;
    if preCaseLess in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 'i';
    if preMultiLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'m';
    if preExtended in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'x';
    //'l':;
    if preSingleLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 's';
    //'u':;
    _WriteUTF8Str2Buffer(aTextNode.RegEx);
    _WriteByte2Buffer($00);
    _WriteUTF8Str2Buffer(LRegExOptionsStr);
    _WriteByte2Buffer($00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x0D + name + \x00 + length (Int32) + string + \x00
  procedure _WriteJavascriptValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    var LJavascript: TBytes := Tencoding.UTF8.GetBytes(aTextNode.Javascript);
    var LInt32: system.Int32 := length(LJavascript) + 1 {for the trailing #0};
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _WriteBytes2Buffer(LJavascript);
    _WriteByte2Buffer($00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x10 + name + \x00 + Int32
  procedure _WriteInt32Value2Buffer(aTextNode:TALJSONNodeW);
  begin
    var LInt32: system.Int32 := aTextNode.Int32;
    _Write2Buffer(LInt32, sizeOf(LInt32));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x12 + name + \x00 + Int64
  procedure _WriteInt64Value2Buffer(aTextNode:TALJSONNodeW);
  begin
    var LInt64: system.Int64 := aTextNode.Int64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteTextNode2Buffer(aTextNode:TALJSONNodeW; aParentNode: TALJSONNodeW; aNodeIndex: Integer);
  begin
    with aTextNode do begin

      // write the node subtype
      case NodeSubType of
        // \x01 + name + \x00 + double
        nstFloat: _WriteByte2Buffer($01);
        // \x02 + name + \x00 + length (Int32) + string + \x00
        nstText: _WriteByte2Buffer($02);
        // \x05 + name + \x00 + Int32 + subtype + (Byte*)
        nstbinary: _WriteByte2Buffer($05);
        // \x07 + name + \x00 + (Byte*12)
        nstObjectID: _WriteByte2Buffer($07);
        // \x08 + name + \x00 + \x00 => Boolean "false"
        // \x08 + name + \x00 + \x01	=> Boolean "true"
        nstBoolean: _WriteByte2Buffer($08);
        // \x09 + name + \x00 + Int64
        nstDateTime: _WriteByte2Buffer($09);
        // \x11 + name + \x00 + Int64
        nstTimestamp: _WriteByte2Buffer($11);
        // \x0A + name + \x00
        nstNull: _WriteByte2Buffer($0A);
        // \xOB + name + \x00 + (Byte*) + \x00 + (Byte*) + \x00
        nstRegEx: _WriteByte2Buffer($0B);
        // \x0D + name + \x00 + length (Int32) + string + \x00
        nstJavascript: _WriteByte2Buffer($0D);
        // \x10 + name + \x00 + Int32
        nstInt32: _WriteByte2Buffer($10);
        // \x12 + name + \x00 + Int64
        nstInt64: _WriteByte2Buffer($12);
        else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
      end;

      // write the nodename
      if (assigned(AParentNode)) and
         (AParentNode.NodeType = ntArray) then _WriteUTF8Str2Buffer(aNodeIndex)
      else _WriteUTF8Str2Buffer(NodeName);
      _WriteByte2Buffer($00);

      // add the nodevalue to the buffer
      case NodeSubType of
        nstFloat: _WriteFloatValue2Buffer(aTextNode);
        nstText: _WriteTextValue2Buffer(aTextNode);
        nstbinary: _WritebinaryValue2Buffer(aTextNode);
        nstObjectID: _WriteObjectIDValue2Buffer(aTextNode);
        nstBoolean: _WriteBooleanValue2Buffer(aTextNode);
        nstDateTime: _WriteDateTimeValue2Buffer(aTextNode);
        nstTimestamp: _WriteTimestampValue2Buffer(aTextNode);
        nstNull:;
        nstRegEx: _WriteRegExValue2Buffer(aTextNode);
        nstJavascript: _WriteJavascriptValue2Buffer(aTextNode);
        nstInt32: _WriteInt32Value2Buffer(aTextNode);
        nstInt64: _WriteInt64Value2Buffer(aTextNode);
        else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNodeW; aParentNode: TALJSONNodeW; aNodeIndex: Integer);
  begin
    with aObjectNode do begin

      if aObjectNode = self then _WriteBytes2Buffer([$00,$00,$00,$00])
      else if (assigned(AParentNode)) and
              (AParentNode.NodeType = ntArray) then begin
        _WriteByte2Buffer($03);
        _WriteUTF8Str2Buffer(aNodeIndex);
        _WriteBytes2Buffer([$00,$00,$00,$00,$00]);
      end
      else begin
        _WriteByte2Buffer($03);
        _WriteUTF8Str2Buffer(NodeName);
        _WriteBytes2Buffer([$00,$00,$00,$00,$00]);
      end;

      var LPos: system.Int64 := StreamPos + BufferPos - 4{length of the #$00#$00#$00#$00};

      var LEmptyNode := True;
      var LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            var LNodeStackEntry: TNodeStackEntry;
            LNodeStackEntry.Node := aObjectNode;
            LNodeStackEntry.ParentNode := aObjectNode;
            LNodeStackEntry.Index := aNodeIndex;
            LNodeStackEntry.StartPos := LPos;
            NodeStack.Push(LNodeStackEntry);
            For var I := Count - 1 downto 0 do begin
              LNodeStackEntry.Node := Nodes[I];
              LNodeStackEntry.ParentNode := aObjectNode;
              LNodeStackEntry.Index := I;
              LNodeStackEntry.StartPos := -1;
              NodeStack.Push(LNodeStackEntry);
            end;
          end
      end;

      If LEmptyNode then begin
        _WriteByte2Buffer($00);
        _WriteInt2Pos(5{length of the object},LPos);
      end;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNodeW; aNodeStartPos: system.Int64);
  begin
    _WriteByte2Buffer($00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNodeW; aParentNode: TALJSONNodeW; aNodeIndex: Integer);
  begin
    with aArrayNode do begin

      if (assigned(AParentNode)) and
         (AParentNode.NodeType = ntArray) then begin
        _WriteByte2Buffer($04);
        _WriteUTF8Str2Buffer(aNodeIndex);
        _WriteBytes2Buffer([$00,$00,$00,$00,$00]);
      end
      else begin
        _WriteByte2Buffer($04);
        _WriteUTF8Str2Buffer(NodeName);
        _WriteBytes2Buffer([$00,$00,$00,$00,$00]);
      end;

      var LPos: system.Int64 := StreamPos + BufferPos - 4{length of the #$00+#$00+#$00+#$00};

      var LEmptyNode := True;
      var LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            var LNodeStackEntry: TNodeStackEntry;
            LNodeStackEntry.Node := aArrayNode;
            LNodeStackEntry.ParentNode := aArrayNode;
            LNodeStackEntry.Index := aNodeIndex;
            LNodeStackEntry.StartPos := LPos;
            NodeStack.Push(LNodeStackEntry);
            For var I := Count - 1 downto 0 do begin
              LNodeStackEntry.Node := Nodes[I];
              LNodeStackEntry.ParentNode := aArrayNode;
              LNodeStackEntry.Index := I;
              LNodeStackEntry.StartPos := -1;
              NodeStack.Push(LNodeStackEntry);
            end;
          end
      end;

      If LEmptyNode then begin
        _WriteByte2Buffer($00);
        _WriteInt2Pos(5{length of the object},LPos);
      end;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNodeW; aNodeStartPos: system.Int64);
  begin
    _WriteByte2Buffer($00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

begin
  If NodeType <> ntobject then exit;

  NodeStack := Tstack<TNodeStackEntry>.Create;
  Try

    {init buffer string}
    Setlength(Buffer, BufferSize); // will make buffer uniquestring
    BufferPos := 0;
    if assigned(Stream) then StreamPos := Stream.Position
    else StreamPos := 0;

    {SaveOnlyChildNode}
    var LNodeStackEntry: TNodeStackEntry;
    LNodeStackEntry.Node := self;
    LNodeStackEntry.ParentNode := Nil;
    LNodeStackEntry.Index := 0;
    LNodeStackEntry.StartPos := StreamPos;
    NodeStack.Push(LNodeStackEntry);

    {loop on all nodes}
    While NodeStack.Count > 0 Do begin
      LNodeStackEntry := NodeStack.Pop;
      with LNodeStackEntry.Node do
        case NodeType of
          ntObject: begin
                      if LNodeStackEntry.Node = LNodeStackEntry.ParentNode then _WriteEndObjectNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.StartPos)
                      else _WriteStartObjectNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode, LNodeStackEntry.Index);
                    end;
          ntArray: begin
                      if LNodeStackEntry.Node = LNodeStackEntry.ParentNode then _WriteEndArrayNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.StartPos)
                      else _WriteStartArrayNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode, LNodeStackEntry.Index);
                   end;
          ntText: _WriteTextNode2Buffer(LNodeStackEntry.Node, LNodeStackEntry.ParentNode, LNodeStackEntry.Index);
          else ALJSONDocErrorW(ALJSONInvalidNodeType);
        end;
    end;

    {Write the buffer}
    if assigned(Stream) then _WriteBuffer2Stream(Buffer, BufferPos)
    else setlength(Buffer,BufferPos);

  finally
    ALFreeAndNil(NodeStack);
  end;
end;

{*****************************************************************************************************}
procedure TALJSONNodeW.SaveToBSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
begin
  var Buffer: TBytes;
  SaveToBSON(Stream, buffer, Options);
end;

{****************************************************************************************************}
procedure TALJSONNodeW.SaveToBSONFile(const FileName: String; const Options: TALJSONSaveOptions = []);
begin
  var LTmpFilename: String;
  if soProtectedSave in Options then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    var LFileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToBSONStream(LFileStream, Options);
    finally
      ALFreeAndNil(LFileStream);
    end;

    if LTmpFilename <> FileName then begin
      if TFile.Exists(FileName) then TFile.Delete(FileName);
      TFile.Move(LTmpFilename, FileName);
    end;

  except
    if (LTmpFilename <> FileName) and
       (TFile.Exists(LTmpFilename)) then TFile.Delete(LTmpFilename);
    raise;
  end;
end;

{************************************************************************************************}
procedure TALJSONNodeW.SaveToBSONBytes(var Bytes: TBytes; const Options: TALJSONSaveOptions = []);
begin
  SaveToBSON(nil, Bytes, Options);
end;

{*********************************************************************************************************************}
procedure TALJSONNodeW.LoadFromJSONString(const Str: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  Try
    ParseJSON(Str, False{SaxMode}, nil{OnParseText}, nil{OnParseStartObject}, nil{OnParseEndObject}, nil{OnParseStartArray}, nil{OnParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*************************************************************************************************************************}
procedure TALJSONNodeW.LoadFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  LoadFromJSONString(ALGetStringFromStream(Stream, TEncoding.UTF8), Options);
end;

{************************************************************************************************************************}
procedure TALJSONNodeW.LoadFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromJSONStream(LFileStream, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{**********************************************************************************************************************}
procedure TALJSONNodeW.LoadFromBSONBytes(const Bytes: TBytes; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  Try
    ParseBSON(nil, Bytes, False{SaxMode}, nil{OnParseText}, nil{OnParseStartObject}, nil{OnParseEndObject}, nil{OnParseStartArray}, nil{OnParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*************************************************************************************************************************}
procedure TALJSONNodeW.LoadFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  Try
    Stream.Position := 0;
    ParseBSON(Stream, nil, False{SaxMode}, nil{OnParseText}, nil{OnParseStartObject}, nil{OnParseEndObject}, nil{OnParseStartArray}, nil{OnParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{************************************************************************************************************************}
procedure TALJSONNodeW.LoadFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromBSONStream(LFileStream, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*************************************}
procedure TALJSONNodeW.ParseJSONString(
            const Str: String;
            const OnParseText: TALJSONParseTextEventW;
            const OnParseStartObject: TALJSONParseObjectEventW;
            const OnParseEndObject: TALJSONParseObjectEventW;
            const OnParseStartArray: TALJSONParseArrayEventW;
            const OnParseEndArray: TALJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
begin
  ParseJSON(Str, true{SaxMode}, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeW.ParseJSONStream(
            const Stream: TStream;
            const OnParseText: TALJSONParseTextEventW;
            const OnParseStartObject: TALJSONParseObjectEventW;
            const OnParseEndObject: TALJSONParseObjectEventW;
            const OnParseStartArray: TALJSONParseArrayEventW;
            const OnParseEndArray: TALJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
begin
  ParseJSONString(ALGetStringFromStream(Stream, TEncoding.UTF8), OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{***********************************}
procedure TALJSONNodeW.ParseJSONFile(
            const FileName: String;
            const OnParseText: TALJSONParseTextEventW;
            const OnParseStartObject: TALJSONParseObjectEventW;
            const OnParseEndObject: TALJSONParseObjectEventW;
            const OnParseStartArray: TALJSONParseArrayEventW;
            const OnParseEndArray: TALJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseJSONStream(LFileStream, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{************************************}
procedure TALJSONNodeW.ParseBSONBytes(
            const Bytes: TBytes;
            const OnParseText: TALJSONParseTextEventW;
            const OnParseStartObject: TALJSONParseObjectEventW;
            const OnParseEndObject: TALJSONParseObjectEventW;
            const OnParseStartArray: TALJSONParseArrayEventW;
            const OnParseEndArray: TALJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
begin
  ParseBSON(nil, Bytes, true{SaxMode}, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeW.ParseBSONStream(
            const Stream: TStream;
            const OnParseText: TALJSONParseTextEventW;
            const OnParseStartObject: TALJSONParseObjectEventW;
            const OnParseEndObject: TALJSONParseObjectEventW;
            const OnParseStartArray: TALJSONParseArrayEventW;
            const OnParseEndArray: TALJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
begin
  Stream.Position := 0;
  ParseBSON(Stream, nil, true{SaxMode}, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
end;

{***********************************}
procedure TALJSONNodeW.ParseBSONFile(
            const FileName: String;
            const OnParseText: TALJSONParseTextEventW;
            const OnParseStartObject: TALJSONParseObjectEventW;
            const OnParseEndObject: TALJSONParseObjectEventW;
            const OnParseStartArray: TALJSONParseArrayEventW;
            const OnParseEndArray: TALJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
begin
  var LFileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseBSONStream(LFileStream, OnParseText, OnParseStartObject, OnParseEndObject, OnParseStartArray, OnParseEndArray, Options);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*****************************************************************}
constructor TALJSONObjectNodeW.Create(const NodeName: String = '');
begin
  inherited create(NodeName);
  FChildNodes := CreateChildList;
end;

{************************************}
destructor TALJSONObjectNodeW.Destroy;
begin
  ALFreeAndNil(FChildNodes);
  inherited;
end;

{**********************************************************}
function TALJSONObjectNodeW.GetChildNodes: TALJSONNodeListW;
begin
  Result := FChildNodes;
end;

{*******************************************************}
function TALJSONObjectNodeW.GetNodeType: TALJSONNodeType;
begin
  Result := NtObject;
end;

{*************************************************************}
function TALJSONObjectNodeW.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := NstObject;
end;

{******************************************************************}
function TALJSONObjectNodeW.InternalGetChildNodes: TALJSONNodeListW;
begin
  Result := FChildNodes;
end;

{****************************************************************}
constructor TALJSONArrayNodeW.Create(const NodeName: String = '');
begin
  inherited create(NodeName);
  FChildNodes := CreateChildList;
end;

{***********************************}
destructor TALJSONArrayNodeW.Destroy;
begin
  ALFreeAndNil(FChildNodes);
  inherited;
end;

{*********************************************************}
function TALJSONArrayNodeW.GetChildNodes: TALJSONNodeListW;
begin
  Result := FChildNodes;
end;

{******************************************************}
function TALJSONArrayNodeW.GetNodeType: TALJSONNodeType;
begin
  Result := NtArray;
end;

{************************************************************}
function TALJSONArrayNodeW.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := NstArray;
end;

{*****************************************************************}
function TALJSONArrayNodeW.InternalGetChildNodes: TALJSONNodeListW;
begin
  Result := FChildNodes;
end;

{***************************************************************}
constructor TALJSONTextNodeW.Create(const NodeName: String = '');
begin
  inherited create(NodeName);
  FNodeValue := 0;
  FNodeSubType := nstText;
  FStorageKind := TALJSONStorageKind.skString;
  FBinarySubType := 0;
  FRegExOptions := [];
end;

{**********************************}
destructor TALJSONTextNodeW.Destroy;
begin
  ClearNodeValue;
  inherited;
end;

{******************************************************************************}
// By default json (ie: javascript) treats all numbers as floating-point values.
// To let other system (ie: mongoDB) understand the type of the number
// we provide the helper functions NumberLong() to handle 64-bit Integers
// and NumberInt() to handle 32-bit Integers (and some others). theses helper functions are
// used when saving the json document.
function TALJSONTextNodeW.GetInterchangeValue(const SkipNodeSubTypeHelper: Boolean = False): String;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetObjectID;
  begin
    if SkipNodeSubTypeHelper then Result := '"'+ALBinToHexW(ObjectID)+'"'
    else Result := 'ObjectId("'+ALBinToHexW(ObjectID)+'")';
  end;

  {~~~~~~~~~~~~~~~~~~~}
  procedure _GetBinary;
  begin
    if FStorageKind = skBytes then begin
      if SkipNodeSubTypeHelper then Result := '"'+ALBase64EncodeBytesW(GetBinaryAsBytes)+'"'
      else Result := 'BinData('+ALIntToStrW(BinarySubType)+', "'+ALBase64EncodeBytesW(GetBinaryAsBytes)+'")';
    end
    else begin
      if SkipNodeSubTypeHelper then Result := '"'+ALBase64EncodeBytesW(ALGetBytesFromStream(GetBinaryAsStream))+'"'
      else Result := 'BinData('+ALIntToStrW(BinarySubType)+', "'+ALBase64EncodeBytesW(ALGetBytesFromStream(GetBinaryAsStream))+'")';
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetDateTime;
  begin
    if SkipNodeSubTypeHelper then Result := ALFormatDateTimeW('''"''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z"''', DateTime)
    else Result := ALFormatDateTimeW('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', DateTime)
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _GetInt32;
  begin
    if SkipNodeSubTypeHelper then Result := text
    else Result := 'NumberInt(' + text + ')'
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _GetInt64;
  begin
    if SkipNodeSubTypeHelper then Result := text
    else Result := 'NumberLong(' + text + ')';
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _GetRegEx;
  begin
    var LRegExOptionsStr: String := '';
    var LRegExOptions := RegExOptions;
    if preCaseLess in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 'i';
    if preMultiLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'m';
    if preExtended in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'x';
    //'l':;
    if preSingleLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 's';
    //'u':;
    Result := '/'+regex+'/' + LRegExOptionsStr;
    if SkipNodeSubTypeHelper then Result := '"' + ALJavascriptEncode(result) + '"'
    else Result := ALJavascriptEncode(result);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetTimestamp;
  begin
    if SkipNodeSubTypeHelper then Result := '"Timestamp('+ALIntToStrW(GetTimeStamp.W1)+', '+ALIntToStrW(GetTimeStamp.W2)+')"'
    else Result := 'Timestamp('+ALIntToStrW(GetTimeStamp.W1)+', '+ALIntToStrW(GetTimeStamp.W2)+')';
  end;

begin
  case FNodeSubType of
    nstFloat:      Result := GetText;
    nstText:       Result := GetText;
    nstBinary:     _GetBinary;
    nstObjectID:   _GetObjectID;
    nstBoolean:    Result := GetText;
    nstDateTime:   _GetDateTime;
    nstJavascript: Result := GetJavascript;
    nstInt32:      _GetInt32;
    nstInt64:      _GetInt64;
    nstNull:       Result := GetText;
    nstRegEx:      _GetRegEx;
    nstTimestamp:  _GetTimestamp;
    else AlJSONDocErrorW(ALJSONInvalidNodeSubType);
  end;
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONTextNodeW.GetText: String;
begin
  case FNodeSubType of
    nstFloat: ALFloatToStrW(GetFloat, Result);
    nstText: Result := PString(@FNodeValue)^;
    //nstObject: Error
    //nstArray: Error
    //nstBinary: Error;
    //nstObjectID: Error;
    nstBoolean: ALBoolToStrW(Result, getBool, 'true', 'false');
    nstDateTime: ALDateTimeToStrW(GetDateTime, Result);
    nstNull: Result := 'null';
    nstRegEx: Result := GetRegEx;
    nstJavascript: Result := GetJavascript;
    nstInt32: ALIntToStrW(GetInt32, Result);
    //nstTimestamp: Error;
    nstInt64: ALIntToStrW(GetInt64, Result);
    else AlJSONDocErrorW(ALJSONOperationError, GetNodeType);
  end;
end;

{***********************************************************}
function TALJSONTextNodeW.GetText(const Default: String): String;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetText;
end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONTextNodeW.SetText(const Value: String);
begin
  ClearNodeValue;
  PString(@FNodeValue)^ := Value;
  FNodeSubType := nstText;
  FStorageKind := TALJSONStorageKind.skString;
end;

{*************************************}
function TALJSONTextNodeW.GetFloat: Double;
begin
  case FNodeSubType of
    nstFloat: PInt64(@result)^ := FNodeValue;
    nstInt32,
    nstInt64: Result := FNodeValue;
    else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
  end;
end;

{************************************************************}
function TALJSONTextNodeW.GetFloat(const Default: Double): Double;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetFloat;
end;

{***************************************************}
procedure TALJSONTextNodeW.SetFloat(const Value: Double);
begin
  ClearNodeValue;
  FNodeValue := PInt64(@Value)^;
  FNodeSubType := nstFloat;
end;

{*******************************************}
function TALJSONTextNodeW.GetDateTime: TDateTime;
begin
  if FNodeSubType = nstDateTime then PInt64(@result)^ := FNodeValue
  else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{*********************************************************************}
function TALJSONTextNodeW.GetDateTime(const Default: TDateTime): TDateTime;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetDateTime;
end;

{*********************************************************}
procedure TALJSONTextNodeW.SetDateTime(const Value: TDateTime);
begin
  ClearNodeValue;
  FNodeValue := PInt64(@Value)^;
  FNodeSubType := nstDateTime;
end;

{***************************************************}
function TALJSONTextNodeW.GetTimestamp: TALBSONTimestamp;
begin
  if FNodeSubType = nstTimestamp then Result.I64 := FNodeValue
  else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{************************************************************************************}
function TALJSONTextNodeW.GetTimestamp(const Default: TALBSONTimestamp): TALBSONTimestamp;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetTimestamp;
end;

{*****************************************************************}
procedure TALJSONTextNodeW.SetTimestamp(const Value: TALBSONTimestamp);
begin
  ClearNodeValue;
  FNodeValue := Value.I64;
  FNodeSubType := nstTimestamp;
end;

{****************************************}
function TALJSONTextNodeW.GetObjectID: TBytes;
begin
  if FNodeSubType = nstObjectID then Result := PBytes(@FNodeValue)^
  else AlJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{***************************************************************}
function TALJSONTextNodeW.GetObjectID(const Default: TBytes): TBytes;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetObjectID;
end;

{******************************************************}
procedure TALJSONTextNodeW.SetObjectID(const Value: TBytes);
begin
  if length(Value) <> 12 then AlJSONDocErrorW(ALJSONObjectIDInvalidSize);
  ClearNodeValue;
  PBytes(@FNodeValue)^ := Value;
  FNodeSubType := nstObjectID;
  FStorageKind := TALJSONStorageKind.skBytes;
end;

{**************************************}
function TALJSONTextNodeW.GetInt32: Integer;
begin
  case FNodeSubType of
    nstFloat: begin
                var LDouble: Double;
                PInt64(@LDouble)^ := FNodeValue;
                var LInt64: system.Int64 := trunc(LDouble);
                // https://stackoverflow.com/questions/41779801/single-double-and-precision
                // Only values that are in form m*2^e, where m and e are Integers can be stored in a floating point variable
                // so all Integer can be store in the form m*2^e (ie: m = m*2^0)
                // so we can compare aInt64 <> aDouble without the need of samevalue
                if (LInt64 <> LDouble) or
                   (LInt64 > system.Int32.MaxValue) or
                   (LInt64 < system.Int32.MinValue) then ALJSONDocErrorW(ALJSONInvalidNodeSubType);
                Result := LInt64;
              end;
    nstInt32: begin
                var LInt64: system.Int64 := FNodeValue;
                if (LInt64 > system.Int32.MaxValue) or
                   (LInt64 < system.Int32.MinValue) then ALJSONDocErrorW(ALJSONInvalidNodeSubType);
                Result := LInt64;
              end;
    nstInt64: Result := FNodeValue;
    else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
  end;
end;

{**************************************************************}
function TALJSONTextNodeW.GetInt32(const Default: Integer): Integer;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetInt32;
end;

{****************************************************}
procedure TALJSONTextNodeW.SetInt32(const Value: Integer);
begin
  ClearNodeValue;
  FNodeValue := Value;
  FNodeSubType := nstInt32;
end;

{************************************}
function TALJSONTextNodeW.GetInt64: Int64;
begin
  case FNodeSubType of
    nstFloat: begin
                var LDouble: Double;
                PInt64(@LDouble)^ := FNodeValue;
                Result := trunc(LDouble);
                // https://stackoverflow.com/questions/41779801/single-double-and-precision
                // Only values that are in form m*2^e, where m and e are Integers can be stored in a floating point variable
                // so all Integer can be store in the form m*2^e (ie: m = m*2^0)
                // so we can compare Result <> aDouble without the need of samevalue
                if Result <> LDouble then ALJSONDocErrorW(ALJSONInvalidNodeSubType);
              end;
    nstInt32,
    nstInt64: Result := FNodeValue;
    else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
  end;
end;

{**********************************************************}
function TALJSONTextNodeW.GetInt64(const Default: Int64): Int64;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetInt64;
end;

{**************************************************}
procedure TALJSONTextNodeW.SetInt64(const Value: Int64);
begin
  ClearNodeValue;
  FNodeValue := Value;
  FNodeSubType := nstInt64;
end;

{*************************************}
function TALJSONTextNodeW.GetBool: Boolean;
begin
  if FNodeSubType = nstBoolean then begin
    if FNodeValue = 0 then Result := False
    else Result := true;
  end
  else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{*************************************************************}
function TALJSONTextNodeW.GetBool(const Default: Boolean): Boolean;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetBool;
end;

{***************************************************}
procedure TALJSONTextNodeW.SetBool(const Value: Boolean);
begin
  ClearNodeValue;
  if Value then FNodeValue := 1
  else FNodeValue := 0;
  FNodeSubType := nstBoolean;
end;

{*************************************}
function TALJSONTextNodeW.GetNull: Boolean;
begin
  Result := FNodeSubType = nstNull;
end;

{***************************************************}
procedure TALJSONTextNodeW.SetNull(const Value: Boolean);
begin
  if Value then begin
    ClearNodeValue;
    FNodeValue := 0
  end
  else ALJSONDocErrorW('Only "true" is allowed for setNull property');
  FNodeSubType := nstNull;
end;

{******************************************}
function TALJSONTextNodeW.GetJavascript: String;
begin
  if FNodeSubType = nstJavascript then Result := PString(@FNodeValue)^
  else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{*****************************************************************}
function TALJSONTextNodeW.GetJavascript(const Default: String): String;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetJavascript;
end;

{********************************************************}
procedure TALJSONTextNodeW.SetJavascript(const Value: String);
begin
  ClearNodeValue;
  PString(@FNodeValue)^ := Value;
  FNodeSubType := nstJavascript;
  FStorageKind := TALJSONStorageKind.skString;
end;

{*************************************}
function TALJSONTextNodeW.GetRegEx: String;
begin
  if FNodeSubType = nstRegEx then Result := PString(@FNodeValue)^
  else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{************************************************************}
function TALJSONTextNodeW.GetRegEx(const Default: String): String;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetRegEx;
end;

{*****************************************************}
procedure TALJSONTextNodeW.SetRegEx(const Pattern: String);
begin
  ClearNodeValue;
  PString(@FNodeValue)^ := Pattern;
  FNodeSubType := nstRegEx;
  FStorageKind := TALJSONStorageKind.skString;
  FRegExOptions := [];
end;

{*********************************************************}
function TALJSONTextNodeW.GetRegExOptions: TALPerlRegExOptions;
begin
  if FNodeSubType = nstRegEx then Result := FRegExOptions
  else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{*********************************************************************************************}
function TALJSONTextNodeW.GetRegExOptions(const Default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetRegExOptions;
end;

{***********************************************************************}
procedure TALJSONTextNodeW.SetRegExOptions(const Value: TALPerlRegExOptions);
begin
  if FNodeSubType = nstRegEx then FRegExOptions := Value
  else AlJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
function TALJSONTextNodeW.GetBinaryAsBytes: TBytes;
begin
  if FNodeSubType = nstBinary then begin
    if FStorageKind <> skBytes then AlJSONDocErrorW(ALJSONBinaryNotStoredAsBytes);
    Result := PBytes(@FNodeValue)^;
  end
  else AlJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
function TALJSONTextNodeW.GetBinaryAsBytes(const Default: TBytes): TBytes;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetBinaryAsBytes;
end;

{****************************************************************}
function TALJSONTextNodeW.GetBinaryAsStream: TStream;
begin
  if FNodeSubType = nstBinary then begin
    if FStorageKind not in [skOwnedStream, skBorrowedStream] then AlJSONDocErrorW(ALJSONBinaryNotStoredAsStream);
    Result := TStream(Pointer(NativeInt(FNodeValue)));
  end
  else AlJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
function TALJSONTextNodeW.GetBinaryAsStream(const Default: TStream): TStream;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetBinaryAsStream;
end;

{****************************************************************}
procedure TALJSONTextNodeW.SetBinaryAsBytes(const Data: TBytes);
begin
  ClearNodeValue;
  PBytes(@FNodeValue)^ := Data;
  FNodeSubType := nstBinary;
  FStorageKind := TALJSONStorageKind.skBytes;
end;

{****************************************************************}
procedure TALJSONTextNodeW.SetBinaryAsStream(const Data: TStream);
begin
  ClearNodeValue;
  FNodeValue := System.Int64(Pointer(Data));
  FNodeSubType := nstBinary;
  FStorageKind := TALJSONStorageKind.skOwnedStream;
end;

{****************************************************************}
function TALJSONTextNodeW.GetOwnsBinaryStream: Boolean;
begin
  if FNodeSubType = nstBinary then Result := FStorageKind = TALJSONStorageKind.skOwnedStream
  else AlJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
procedure TALJSONTextNodeW.SetOwnsBinaryStream(const Value: Boolean);
begin
  if FNodeSubType = nstBinary then begin
    if FStorageKind not in [skOwnedStream, skBorrowedStream] then AlJSONDocErrorW(ALJSONBinaryNotStoredAsStream);
    if Value then FStorageKind := skOwnedStream
    else FStorageKind := skBorrowedStream;
  end
  else AlJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{*******************************************}
function TALJSONTextNodeW.GetBinarySubType: Byte;
begin
  if FNodeSubType = nstBinary then Result := FBinarySubType
  else ALJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{****************************************************************}
function TALJSONTextNodeW.GetBinarySubType(const Default: Byte): Byte;
begin
  if FNodeSubType = nstNull then Result := default
  else Result := GetBinarySubType;
end;

{***********************************************************}
procedure TALJSONTextNodeW.SetBinarySubType(const Subtype: Byte);
begin
  if FNodeSubType = nstBinary then FBinarySubType := Subtype
  else AlJSONDocErrorW(ALJSONInvalidNodeSubType);
end;

{*****************************************************}
function TALJSONTextNodeW.GetNodeType: TALJSONNodeType;
begin
  Result := NtText;
end;

{***********************************************************}
function TALJSONTextNodeW.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := FNodeSubType;
end;

{****************************************}
procedure TALJSONTextNodeW.ClearNodeValue;
begin
  case FStorageKind of
    skString: PString(@FNodeValue)^ := '';
    skBytes: PBytes(@FNodeValue)^ := nil;
    skOwnedStream: TStream(Pointer(NativeInt(FNodeValue))).Free;
  end;
  FStorageKind := skInt64;
end;

{**********************************}
constructor TALJSONNodeListW.Create;
begin
  FList:= nil;
  FCount:= 0;
  FCapacity := 0;
end;

{**********************************}
destructor TALJSONNodeListW.Destroy;
begin
  Clear;
  inherited;
end;

{***********************************************************************************}
{Locates the index for a node name in a sorted list and indicates whether a node name
 with that value already exists in the list. Use Find to obtain the index in a
 sorted list where the node name S should be added. If the node name S, or a node
 name that differs from S only in case, already exists in the list, Find returns true.
 If the list does not contain a node name that matches S, Find returns false.
 The index where S should go is returned in the Index parameter.
 The value of Index is zero-based, where the first node name has the index 0, the
 second node name has the index 1, and so on.
 Note: Only use Find with sorted lists. For unsorted lists, use the IndexOf method instead.
 Tip: If the node name string is not found (thus return value of Find is False) then Index
      is set to the index of the first node name in the list that sorts immediately before
      or after S.}
function TALJSONNodeListW.Find(const NodeName: String; var Index: Integer): Boolean;
begin
  Result := False;
  var L: Integer := 0;
  var H: Integer := FCount - 1;
  while L <= H do
  begin
    var I: Integer := (L + H) shr 1;
    var C: Integer := CompareNodeNames(FList[I].NodeName, NodeName);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
        Result := True;
    end;
  end;
  Index := L;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Node is the object node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALJSONNodeListW.IndexOf(const Node: TALJSONNodeW; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if FList[Result] = Node then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if FList[Result] = Node then Exit;
  end;
  Result := -1;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Name is the NodeName property of the node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALJSONNodeListW.IndexOf(const Name: String; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if ALSameStrW(FList[Result].NodeName, Name) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if ALSameStrW(FList[Result].NodeName, Name) then Exit;
  end;
  Result := -1;
end;

{***************************************************************************************************************************}
function TALJSONNodeListW.IndexOfValue(const Value: String; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].Text = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].Text = Value) then Exit;
  end;
  Result := -1;
end;

{****************************************************************************************************************************}
function TALJSONNodeListW.IndexOfValue(const Value: Integer; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].Int32 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].Int32 = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************************************************************************************************}
function TALJSONNodeListW.IndexOfValue(const Value: Int64; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].Int64 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].Int64 = Value) then Exit;
  end;
  Result := -1;
end;

{***************************************************************************************************************************}
function TALJSONNodeListW.IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].float = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].float = Value) then Exit;
  end;
  Result := -1;
end;

{******************************************************************************************************************************}
function TALJSONNodeListW.IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].DateTime = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].DateTime = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************}
{Returns a specified node from the list.
 Call FindNode to access a particular node in the list.
 *NodeName is the node to access. It specifies the NodeName property of the desired node.
 FindNode returns the object of the node if it is in the list. If NodeName does not specify a node in the list,
 FindNode returns nil (Delphi) or NULL (C++).}
function TALJSONNodeListW.FindNode(const NodeName: String; const Direction: TDirection = TDirection.FromBeginning): TALJSONNodeW;
begin
  var Index := IndexOf(NodeName, Direction);
  if Index >= 0 then Result := Get(Index)
  else Result := nil;
end;

{**********************************}
{Returns the first node in the list.
Call First to access the first node in the list. If the list is empty, First raises an exception}
function TALJSONNodeListW.First: TALJSONNodeW;
begin
  if Count > 0 then Result := Get(0)
  else Result := nil;
end;

{*********************************}
{Returns the last node in the list.
 Call Last to access the last node in the list. If the list is empty, Last raises an exception.}
function TALJSONNodeListW.Last: TALJSONNodeW;
begin
  if Count > 0 then Result := Get(FCount - 1)
  else Result := nil;
end;

{***************************************************************************}
{Returns a node that appears a specified amount before or after another node.
 Call FindSibling to access the node whose position has a specified relationship to another node.
 *Node is a node in the list to use as a reference point.
 *Delta indicates where the desired node appears, relative to Node. If Delta is positive, FindSibling returns
  the node that appears Delta positions after Node. If Delta is negative, FindSibling returns a node that appears before Node.
 FindSibling returns the node that appears at the position offset by Delta, relative to the position of Node. If Delta
 specifies a position before the first node or after the last node in the list, FindSibling returns nil (Delphi) or NULL (C++).}
function TALJSONNodeListW.FindSibling(const Node: TALJSONNodeW; Delta: Integer): TALJSONNodeW;
begin
  var Index := IndexOf(Node) + Delta;
  if (Index >= 0) and (Index < FCount) then Result := Get(Index)
  else Result := nil;
end;

{************************************}
{Returns a specified node in the list.
 Call Get to retrieve a node from the list, given its index.
 *Index specifies the node to fetch, where 0 identifies the first node, 1 identifies the second node, and so on.
  Index should be less than the value of the Count property.}
function TALJSONNodeListW.Get(Index: Integer): TALJSONNodeW;
begin
  if (Index < 0) or (Index >= FCount) then ALJSONDocErrorW(ALJSONListIndexError, [Index]);
  Result := FList[Index];
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Index identify the desired node. 0 is the index of the first node,
  1 is the index of the second node, and so on}
function TALJSONNodeListW.GetNodeByIndex(const Index: Integer): TALJSONNodeW;
begin
  Result := Get(Index);
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Name identify the desired node. it is the NodeName property of a node in the list.
 If Name does not identify a node in the list, GetNode tries to create a new node with the name specified by
 Name. If it can’t create the new node, GetNode raises an exception.}
function TALJSONNodeListW.GetNodeByName(const Name: String): TALJSONNodeW;
begin
  Result := FindNode(Name);
  if not Assigned(Result) then ALJSONDocErrorW(ALJSONNodeNotFound, [Name]);
end;

{************************************************************************}
function TALJSONNodeListW.CompareNodeNames(const S1, S2: String): Integer;
begin
  Result := ALCompareStrW(S1, S2)
end;

{*****************************************************************************************}
procedure TALJSONNodeListW.QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareW);
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        Exchange(L, R);
      break;
    end;
    var I: Integer := L;
    var J: Integer := R;
    var P: Integer := (L + R) shr 1;
    repeat
      while (I <> P) and (ACompare(Self, I, P) < 0) do Inc(I);
      while (J <> P) and (ACompare(Self, J, P) > 0) do Dec(J);
      if I <= J then
      begin
        if I <> J then
          Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if (J - L) > (R - I) then
    begin
      if I < R then
        QuickSort(I, R, ACompare);
      R := J;
    end
    else
    begin
      if L < J then
        QuickSort(L, J, ACompare);
      L := I;
    end;
  end;
end;

{************************************************************************************************}
function ALJSONNodeListCompareNodeNameW(List: TALJSONNodeListW; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareNodeNames(
              List[Index1].NodeName,
              List[Index2].NodeName);
end;

{******************************}
procedure TALJSONNodeListW.Sort;
begin
  CustomSort(ALJSONNodeListCompareNodeNameW);
end;

{**************************************************************************}
procedure TALJSONNodeListW.CustomSort(Compare: TALJSONNodeListSortCompareW);
begin
  if (FList <> nil) and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

{**************************************}
{Adds a new node to the end of the list.
 Call Add to add a node to the end of the list. Add returns the index of the node once it is added, where 0 is the index
 of the first node in the list, 1 is the index of the second node, and so on.
 *Node is the node to add to the list.}
function TALJSONNodeListW.Add(const Node: TALJSONNodeW): Integer;
begin
  Result := FCount;
  InternalInsert(Result, Node);
end;

{**********************************************************************************}
procedure TALJSONNodeListW.InternalInsert(Index: Integer; const Node: TALJSONNodeW);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    ALMove(
      FList[Index],
      FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  Pointer(FList[index]) := nil;
  FList[index] := Node;
  Inc(FCount);
end;

{********************************************************}
{Inserts a new node into a specified position in the list.
 Call Insert to add a node at the position specified by Index.
 *Index specifies where to insert the node, where 0 is the first position, 1 is second position, and so on. If Index does not
  specify a valid index, Insert raises an exception.
 *Node is the node to add to the list.}
procedure TALJSONNodeListW.Insert(Index: Integer; const Node: TALJSONNodeW);
begin
  if Index = -1 then Add(Node)
  else begin
    if (Index < 0) or (Index > FCount) then ALJSONDocErrorW(ALJSONListIndexError, [Index]);
    InternalInsert(Index, Node);
  end;
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Index identifies the node to remove by index rather than name. Index ranges from 0 to one less than the value of the Count property.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Index Delete returns –1.}
function TALJSONNodeListW.Delete(const Index: Integer): Integer;
begin
  var LNode := Get(Index);
  FList[Index] := nil; // to decrease the refcount of Node
  Dec(FCount);
  if Index < FCount then begin
    ALMove(
      FList[Index + 1],
      FList[Index],
      (FCount - Index) * SizeOf(Pointer));
    Pointer(FList[FCount]) := nil;
  end;
  if assigned(LNode) then ALFreeAndNil(LNode);
  Result := Index;
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Name identifies the node to remove from the list. This is the local name of the node to remove.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Name, Delete returns –1.}
function TALJSONNodeListW.Delete(const Name: String): Integer;
begin
  Result := indexOf(Name);
  if Result >= 0 then Delete(Result);
end;

{**************************************}
{Removes a specified node from the list.
 Remove removes the specified node from the list.
 *Node is the node to remove from the list.
 Remove returns the index of Node before it was removed. If node is not a node in the list, Remove returns -1.}
function TALJSONNodeListW.Remove(const Node: TALJSONNodeW): Integer;
begin
  Result := IndexOf(Node);
  if Result >= 0 then Delete(Result);
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeListW.Extract(const Node: TALJSONNodeW): TALJSONNodeW;
begin
  Result := nil;
  var I := IndexOf(Node);
  if I >= 0 then Result := Extract(i);
end;

{***********************************************************}
procedure TALJSONNodeListW.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then ALJSONDocErrorW(ALJSONListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then ALJSONDocErrorW(ALJSONListIndexError, [Index2]);
  var Item := pointer(FList[Index1]);
  pointer(FList[Index1]) := pointer(FList[Index2]);
  pointer(FList[Index2]) := Item;
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeListW.Extract(const index: Integer): TALJSONNodeW;
begin
  Result := Get(index);
  FList[index] := nil;
  Delete(index);
end;

{*********************************************}
{Replaces a node in the list with another node.
 Call ReplaceNode to replace the node specified by OldNode with the node specified by NewNode.
 *OldNode is the node to replace. If OldNode does not appear in the list, then ReplaceNode adds the new node to the end of the list.
 *NewNode is the node to add to the list in place of OldNode.
 ReplaceNode returns OldNode (even if OldNode did not appear in the list).}
function TALJSONNodeListW.ReplaceNode(const OldNode, NewNode: TALJSONNodeW): TALJSONNodeW;
begin
  var Index := indexOf(OldNode);
  Result := Extract(Index);
  Insert(Index, NewNode);
end;

{*******************************}
{Removes all nodes from the list.
 Call Clear to empty the list.
 Note:	Clear does not call the beginUpdate and EndUpdate methods, even though it may Result in the
 deletion of more than one node.}
procedure TALJSONNodeListW.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{******************************}
procedure TALJSONNodeListW.Grow;
begin
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
end;

{***********************************************************}
procedure TALJSONNodeListW.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) then ALJSONDocErrorW(ALJSONListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*****************************************************}
procedure TALJSONNodeListW.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) then ALJSONDocErrorW(ALJSONListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for var I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{**************************}
procedure ALJSONToTStringsW(
            const AJsonStr: String;
            const AFormatSettings: TALFormatSettingsW;
            const APath: String;
            const ALst: TALStringsW;
            const ANullStr: String = 'null';
            const ATrueStr: String = 'true';
            const AFalseStr: String = 'false');
begin
  var LContainChilds := False;
  TALJSONDocumentW.ParseJSONString(
    AJsonStr,
    //--
    procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType)
    begin
      if (NodeSubType = nstFloat) then          aLst.Add(aPath + Path + aLst.NameValueSeparator + ALFloatToStrW(Args[0].VExtended^, aFormatSettings))
      else if (NodeSubType = nstDateTime) then  aLst.Add(aPath + Path + aLst.NameValueSeparator + ALDateTimeToStrW(Args[0].VExtended^, aFormatSettings))
      else if (NodeSubType = nstBoolean) then   aLst.Add(aPath + Path + aLst.NameValueSeparator + ALBoolToStrW(Args[0].VBoolean,aTrueStr,aFalseStr))
      else if (NodeSubType = nstnull) then      aLst.Add(aPath + Path + aLst.NameValueSeparator + aNullStr)
      else                                      aLst.Add(aPath + Path + aLst.NameValueSeparator + String(Args[0].VUnicodeString));
      LContainChilds := True;
    end{OnParseText},
    //--
    procedure (Sender: TObject; const Path: String; const Name: String)
    begin
      LContainChilds := False;
    end{OnParseStartObject},
    //--
    procedure (Sender: TObject; const Path: String; const Name: String)
    begin
      if (not LContainChilds) and (aPath + Path <> ''{Path = '' mean it's the root object}) then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '{}');
      LContainChilds := True;
    end{OnParseEndObject},
    //--
    procedure (Sender: TObject; const Path: String; const Name: String)
    begin
      LContainChilds := False;
    end{OnParseStartArray},
    //--
    procedure (Sender: TObject; const Path: String; const Name: String)
    begin
      if not LContainChilds then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '[]');
      LContainChilds := True;
    end{OnParseEndArray});
end;

{**************************}
procedure ALJSONToTStringsW(
            const AJsonStr: String;
            const AFormatSettings: TALFormatSettingsW;
            const ALst: TALStringsW;
            const ANullStr: String = 'null';
            const ATrueStr: String = 'true';
            const AFalseStr: String = 'false');
begin
 ALJSONToTStringsW(
   AJsonStr,
   aFormatSettings,
   '',
   aLst,
   aNullStr,
   aTrueStr,
   aFalseStr);
end;

{**************************}
procedure ALJSONToTStringsW(
            const AJsonNode: TALJSONNodeW;
            const APath: String;
            const ALst: TALStringsW;
            const ANullStr: String = 'null';
            const ATrueStr: String = 'true';
            const AFalseStr: String = 'false');
begin
  if aJsonNode.ChildNodes.Count > 0 then begin
    for var I := 0 to aJsonNode.ChildNodes.Count - 1 do begin

      var LTmpPath: String;
      if aJsonNode.NodeType = ntArray then LTmpPath := aPath + '[' + ALIntToStrW(I) + ']'
      else begin
        if aJsonNode.ChildNodes[I].NodeName = '' then AlJSONDocErrorW(ALJSONNodeNameCanNotBeEmpty);
        LTmpPath := aPath + alIfThenW(aPath <> '', '.', '') + aJsonNode.ChildNodes[I].NodeName;
      end;

      case aJsonNode.ChildNodes[I].NodeType of

        ntObject: ALJSONToTStringsW(
                    aJsonNode.ChildNodes[I],
                    LTmpPath,
                    aLst,
                    aNullStr,
                    aTrueStr,
                    aFalseStr);

        ntArray: ALJSONToTStringsW(
                   aJsonNode.ChildNodes[I],
                   LTmpPath,
                   aLst,
                   aNullStr,
                   aTrueStr,
                   aFalseStr);

        ntText: begin
                  if (aJsonNode.ChildNodes[I].NodeSubType = nstBoolean) then   aLst.Add(LTmpPath + aLst.NameValueSeparator + ALBoolToStrW(aJsonNode.ChildNodes[I].Bool,aTrueStr,aFalseStr))
                  else if (aJsonNode.ChildNodes[I].NodeSubType = nstnull) then aLst.Add(LTmpPath + aLst.NameValueSeparator + aNullStr)
                  else                                                         aLst.Add(LTmpPath + aLst.NameValueSeparator + aJsonNode.ChildNodes[I].Text);
                end;

        else AlJSONDocErrorW(ALJSONInvalidNodeType);

      end;
    end;
  end
  else if (aPath <> ''{aPath = '' mean it's the root object}) then begin
    if      aJsonNode.NodeType = ntArray  then aLst.Add(aPath + aLst.NameValueSeparator + '[]')
    else if aJsonNode.NodeType = ntObject then aLst.Add(aPath + aLst.NameValueSeparator + '{}');
  end;
end;

{**************************}
procedure ALJSONToTStringsW(
            const AJsonNode: TALJSONNodeW;
            const ALst: TALStringsW;
            const ANullStr: String = 'null';
            const ATrueStr: String = 'true';
            const AFalseStr: String = 'false');
begin
  ALJSONToTStringsW(
    aJsonNode,
    '',
    aLst,
    aNullStr,
    aTrueStr,
    aFalseStr)
end;

{**************************}
procedure ALTStringsToJsonW(
            const ALst: TALStringsW;
            const AJsonNode: TALJSONNodeW;
            const APath: String = '';
            const ANameToLowerCase: Boolean = false;
            const ANullStr: String = 'null');
begin

  // create list of the part of name,
  // from "aggregated_data.properties.types[3].translations.usa" =>
  //   aggregated_data
  //   properties
  //   types
  //   [3]
  //   translations
  //   usa
  var LNames := TALStringListW.Create;
  try

    //init aNames.linebreak
    LNames.LineBreak := '.';

    // scroll the aLst
    for var I := 0 to aLst.Count - 1 do begin

      //if it's contain path
      if (aPath = '') or
         (ALPosIgnoreCaseW(aPath + '.',aLst.Names[I]) = 1) then begin

        // path.aggregated_data.properties.types[3].translations.usa =>
        //   aggregated_data
        //   properties
        //   types
        //   [3]
        //   translations
        //   usa
        if (aPath <> '') then LNames.Text := ALStringReplaceW(
                                               ALStringReplaceW(
                                                 aLst.Names[I],
                                                 aPath + '.',
                                                 '',
                                                 [rfIgnoreCase]),
                                               '[',
                                               '.[',
                                               [rfReplaceAll])
        else LNames.Text := ALStringReplaceW(
                              aLst.Names[I],
                              '[',
                              '.[',
                              [rfReplaceAll]);

        //loop on all the name
        var LCurrJsonNode := aJsonNode;
        for var J := 0 to LNames.Count - 1 do begin

          //if we are in array
          if LCurrJsonNode.NodeType = ntArray then begin
            var LIndex: Integer;
            if (length(LNames[J]) <= 2) or
               (LNames[J][1] <> '[') or
               (LNames[J][length(LNames[J])] <> ']') or
               (not ALTryStrToInt(ALCopyStr(LNames[J], 2, Length(LNames[J]) - 2), LIndex)) then raise EALException.CreateFmt('Wrong path: "%s"', [aLst.Names[I]]);
            while LIndex > LCurrJsonNode.ChildNodes.Count - 1 do begin
              if J = LNames.Count - 1 then LCurrJsonNode.AddChild(ntText)
              else if (LNames[J+1] <> '') and
                      (LNames[J+1][1] = '[') then LCurrJsonNode.AddChild(ntarray)
              else LCurrJsonNode.AddChild(ntObject);
            end;
            LCurrJsonNode := LCurrJsonNode.ChildNodes[LIndex];
          end

          //if we are not in array
          else begin
            var LLowerName := alifThenW(aNameToLowerCase, AlLowerCase(LNames[J]), LNames[J]);
            var LTmpJsonNode := LCurrJsonNode.ChildNodes.FindNode(LLowerName);
            if not assigned(LTmpJsonNode) then begin
              if J = LNames.Count - 1 then LCurrJsonNode := LCurrJsonNode.AddChild(LLowerName, ntText)
              else if (LNames[J+1] <> '') and
                      (LNames[J+1][1] = '[') then LCurrJsonNode := LCurrJsonNode.AddChild(LLowerName, ntarray)
              else LCurrJsonNode := LCurrJsonNode.AddChild(LLowerName, ntObject);
            end
            else LCurrJsonNode := LTmpJsonNode;
          end;

          //set the value
          if J = LNames.Count - 1 then begin
            if aLst.ValueFromIndex[I] = aNullStr then LCurrJsonNode.Null := true
            else LCurrJsonNode.Text := aLst.ValueFromIndex[I];
          end;

        end;

      end;

    end;

  finally
    ALFreeAndNil(LNames);
  end;

end;

{*****************************************************************************}
function ALJsonEncodeFloatWithNodeSubTypeHelperW(const AValue: double): String;
begin
  Result := ALFloatToStrW(aValue);
end;

{****************************************************************************}
function ALJsonEncodeTextWithNodeSubTypeHelperW(const AValue: String): String;
begin
  Result := '"'+ALJavascriptEncode(aValue)+'"';
end;

{******************************************************************************}
function ALJsonEncodeBinaryWithNodeSubTypeHelperW(const AValue: TBytes): String;
begin
  Result := 'BinData(0, "' + ALBase64EncodeBytesW(aValue) + '")';
end;

{********************************************************************************}
function ALJsonEncodeObjectIDWithNodeSubTypeHelperW(const AValue: TBytes): String;
begin
  Result := 'ObjectId("'+ALBinToHexW(aValue)+'")';
end;

{********************************************************************************}
function ALJsonEncodeBooleanWithNodeSubTypeHelperW(const AValue: Boolean): String;
begin
  if aValue then Result := 'true'
  else Result := 'false';
end;

{**************************************************************}
function ALJsonEncodeDateTimeW(const AValue: TDateTime): String;
begin
  Result := ALFormatDateTimeW('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''', aValue);
end;

{***********************************************************************************}
function ALJsonEncodeDateTimeWithNodeSubTypeHelperW(const AValue: TDateTime): String;
begin
  Result := ALFormatDateTimeW('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', aValue);
end;

{**********************************************************************************}
function ALJsonEncodeJavascriptWithNodeSubTypeHelperW(const AValue: String): String;
begin
  Result := aValue;
end;

{****************************************************************************}
function ALJsonEncodeInt64WithNodeSubTypeHelperW(const AValue: Int64): String;
begin
  Result := 'NumberLong(' + ALIntToStrW(aValue) + ')';
end;

{****************************************************************************}
function ALJsonEncodeInt32WithNodeSubTypeHelperW(const AValue: Int32): String;
begin
  Result := 'NumberInt(' + ALIntToStrW(aValue) + ')';
end;

{******************************************************}
function ALJsonEncodeNullWithNodeSubTypeHelperW: String;
begin
  Result := 'null';
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.JSONDoc','initialization');
  {$ENDIF}

  ALJsonISODateFormatSettingsA := TALFormatSettingsA.Create('en-US');
  ALJsonISODateFormatSettingsA.DateSeparator := '-';
  ALJsonISODateFormatSettingsA.TimeSeparator := ':';
  ALJsonISODateFormatSettingsA.ShortDateFormat := 'yyyy-mm-dd';
  ALJsonISODateFormatSettingsA.ShortTimeFormat := 'hh:nn:ss.zzz';
  ALDefaultJsonNodeIndentA := '  '; { 2 spaces }
  ALDefaultJsonPathSeparatorA := '.';

  ALJsonISODateFormatSettingsW := TALFormatSettingsW.Create('en-US');
  ALJsonISODateFormatSettingsW.DateSeparator := '-';
  ALJsonISODateFormatSettingsW.TimeSeparator := ':';
  ALJsonISODateFormatSettingsW.ShortDateFormat := 'yyyy-mm-dd';
  ALJsonISODateFormatSettingsW.ShortTimeFormat := 'hh:nn:ss.zzz';
  ALDefaultJsonNodeIndentW := '  '; { 2 spaces }
  ALDefaultJsonPathSeparatorW := '.';

end.
