(*******************************************************************************
TALJSONDocument is a Delphi parser/writer for JSON/BSON
data formats. It supports both DOM and SAX parsers. (Note
that a better name for SAX could be SAJ for Simple API for
JSON instead of Simple API for XML, but as the concept of
SAX is well-known, I will keep this name.) TALJSONDocument
also supports BSON format and uses a syntax similar to
TALXMLDocument/TXMLDocument. Additionally, TALJSONDocument
can export Json/Bson data to TALStringListA.

Exemple :

{
  _id: 1,
  name: { first: "John", last: "Backus" },
  birth: new Date('1999-10-21T21:04:54.234Z'),
  contribs: [ "Fortran", "ALGOL", "Backus-Naur Form", "FP" ],
  awards: [
            { award: "National Medal of Science",
              year: 1975,
              by: "National Science Foundation" },
            { award: "Turing Award",
              year: 1977,
              by: "ACM" }
          ],
  spouse: "",
  address: {},
  phones: []
}

------------------------------
To access the document nodes :

MyJsonDoc := TALJsonDocumentW.CreateFromJSONString('{...}');
MyJsonDoc.GetChildNodeValueInt32('_id', 0{default if node not exists});
MyJsonDoc.GetChildNodeValueText(['name','first'], ''{default if node not exists});
MyJsonDoc.GetChildNodeValueDateTime('birth', Now{default if node not exists});
for i := 0 to MyJsonDoc.childnodes['contribs'].ChildNodes.count - 1 do
  MyJsonDoc.childnodes['contribs'].childnodes[i].text;

------------------------------
To create the document nodes :

MyJsonDoc := TALJsonDocumentW.Create;
MyJsonDoc.addchild('_id').int32 := 1;
with MyJsonDoc.addchild('name', ntObject) do begin
  addchild('first').text := 'John';
  addchild('last').text := 'Backus';
end;
MyJsonDoc.addchild('birth').dateTime := Now;
with MyJsonDoc.addchild('contribs', ntArray) do begin
  addchild.text := 'Fortran';
  addchild.text := 'ALGOL';
  addchild.text := 'Backus-Naur Form';
  addchild.text := 'FP';
end;
with MyJsonDoc.addchild('awards', ntArray) do begin
  with addchild(ntObject) do begin
    addchild('award').text := 'National Medal of Science';
    addchild('year').int32 := 1975;
    addchild('by').text := 'National Science Foundation';
  end;
  with addchild(ntObject) do begin
    addchild('award').text := 'Turing Award';
    addchild('year').int32 := 1977;
    addchild('by').text := 'ACM';
  end;
end;
MyJsonDoc.addchild('spouse');
MyJsonDoc.addchild('address', ntObject);
MyJsonDoc.addchild('phones', ntArray);

----------------------------
To load and save from BSON :

MyJsonDoc := TALJsonDocumentW.CreateFromBSONBytes(Bytes);
MyJsonDoc.SaveToBSONFile(FileName);

---------------------------------------
To parse an JSON document in Sax Mode :

TALJSONDocumentW.ParseJSONString(
      '{...}',
      //--
      procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType)
      begin
        case NodeSubType of
          nstFloat:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + ALFloatToStrW(Args[0].VExtended^, ALDefaultFormatSettingsW));
          nstText:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + String(Args[0].VUnicodeString));
          nstObject: ;
          nstArray: ;
          nstObjectID:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'ObjectId("'+string(Args[0].VUnicodeString)+'")');
          nstBoolean:    MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + ALBoolToStrW(Args[0].VBoolean,'true','false'));
          nstDateTime:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + ALFormatDateTimeW('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsW));
          nstNull:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'null');
          nstRegEx:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + String(Args[0].VUnicodeString));
          nstBinary:     MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'BinData('+ALIntToStrW(Args[1].VInteger)+', "'+String(Args[0].VunicodeString)+'")');
          nstJavascript: MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + String(Args[0].VUnicodeString));
          nstInt32:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'NumberInt('+ALIntToStrW(Args[0].VInteger)+')');
          nstTimestamp:  MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'Timestamp('+ALIntToStrW(int64(cardinal(Args[0].VInteger)))+', '+ALIntToStrW(int64(cardinal(Args[1].VInteger)))+')');
          nstInt64:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
        end;
      end{onParseText},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('STARTOBJECT  =>  ' + Name);
      end{onParseStartObject},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('ENDOBJECT    =>  ' + Name);
      end{onParseEndObject},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('STARTARRAY   =>  ' + Name);
      end{onParseStartArray},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('ENDARRAY     =>  ' + Name);
      end{onParseEndArray});

-------------------------------------------------------
list of changes made with the 1.0.5 release of Alcinoe:

* The support for doNodeAutoCreate has been removed.
* The properties TALJsonDocument.Duplicates and TALJsonDocument.Sorted are now
  only applied to the child nodes list and are not inherited anymore.
* The properties TALJsonDocument.ParseOptions and TALJsonDocument.Options have
  been moved as parameters for the methods loadFromJson/saveToJson and
  loadFromBson/saveToBson.
* TALJsonDocument.Duplicates has been moved to the child nodes list.
* The ClearChildNodes parameter from loadFromJson/saveToJson has been moved to
  the options of loadFromJson/saveToJson.
* The property TALJSONNode.ownerDocument has been removed.
* FormatSettings has been removed.
* The property TALJsonDocument.Tag has been removed.
* TALJsonDocument.PathSeparator has been replaced by ALDefaultJsonPathSeparator.
* TALJsonDocument.NodeIndentStr has been replaced by ALDefaultJsonNodeIndentA.
* TALJsonDocument.node has been removed.
* TALJsonDocument.create now returns a TALJsonNode.
* TALJsonDocument.IsEmptyDoc has been replaced by hasChildNodes.
* The method TALJsonDocument.ExtractNode has been removed.
* The property TALJsonDocument.Active has been removed.
* The method TALJsonDocument.ReleaseDoc has been removed.
* The ParseStartDocument and ParseEndDocument events have been removed.
* TALJsonDocument.clear has been moved to childnodes.clear.
* The events onParseText, onParseStartObject, onParseEndObject,
  onParseStartArray, and onParseEndArray have been moved to ParseBSON/ParseJSON.

------
DEMO :
https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALJsonDoc
*******************************************************************************)
unit Alcinoe.JSONDoc;

interface

{$I Alcinoe.inc}
{$SCOPEDENUMS OFF}

uses
  system.Classes,
  system.sysutils,
  system.types,
  Alcinoe.XMLDoc,
  Alcinoe.StringUtils,
  Alcinoe.StringList;

const
  cAlJSONNodeNotFound           = 'Node "%s" not found';
  cALJSONInvalidNodeType        = 'Invalid node type';
  cALJSONInvalidBSONNodeSubType = 'Invalid node sub type';
  cALJSONListCapacityError      = 'Node list capacity out of bounds (%d)';
  cALJSONListCountError         = 'Node list count out of bounds (%d)';
  cALJSONListIndexError         = 'Node list index out of bounds (%d)';
  cALJSONOperationError         = 'This operation can not be performed with a node of type %s';
  cALJSONParseError             = 'JSON Parse error';
  cALBSONParseError             = 'BSON Parse error';
  CALJSONSortedListError        = 'Operation not allowed on sorted node list';
  cALJSONDuplicateNodeName      = 'Node list does not allow duplicates';

type

  TALJSONNodeType = (ntObject, //The node represents an object: { ... } or "name": { ... }
                     ntArray,  //The node represents an array: [ ... ] or "name": [ ... ]
                     ntText);  //The node represents a text content (javascript, string, number, true, false, null): "..." or "name": "..."

  //from http://bsonspec.org/#/specification
  TALJSONNodeSubType = (nstFloat,      // \x01 | Floating point             | ex { a: 123.4 }
                        nstText,       // \x02 | UTF-8 string               | ex { a: "xxx" }
                        nstObject,     // \x03 | Embedded document          | ex { a: {} }
                        nstArray,      // \x04 | Array                      | ex { a: [] }
                        nstBinary,     // \x05 | Binary data
                                       // \x06 | Undefined — Deprecated
                        nstObjectID,   // \x07 | ObjectId                   | ex { a: ObjectId("507f1f77bcf86cd799439011") }
                        nstBoolean,    // \x08 | Boolean "false"            | ex { a: False }
                                       // \x08 | Boolean "true"             | ex { a: true }
                        nstDateTime,   // \x09 | UTC datetime               | ex { a: ISODate("yyyy-mm-ddThh:nn:ss.zzzZ") }
                        nstNull,       // \x0A | Null value                 | ex { a: null }
                        nstRegEx,      // \x0B | Regular expression
                                       // \x0C | DBPointer — Deprecated
                        nstJavascript, // \x0D | JavaScript code            | ex { a: function() }
                                       // \x0E | Symbol — Deprecated
                                       // \x0F | JavaScript code w/ scope
                        nstInt32,      // \x10 | 32-bit Integer             | ex { a: NumberInt(123) }
                        nstTimestamp,  // \x11 | Timestamp                  | ex { a: Timestamp(0, 0) }
                        nstInt64);     // \x12 | 64-bit integer             | ex { a: NumberLong(123) }
                                       // \xFF | Min key
                                       // \x7F | Max key

  TALBSONTimestamp = packed record      // Special internal type used by MongoDB replication and sharding.
    case integer of                     // First 4 bytes are an increment, second 4 are a timestamp. Setting the
      0: (I64: Int64);                  // timestamp to 0 has special semantics.
      1: (W1:  LongWord;
          W2:  LongWord);
  end;

  TALJSONSaveOption = (soNodeAutoIndent, // Automatically indents the JSON output for improved readability
                       soIgnoreControlCharacters, // Don't decode escaped characters (like \") and not encode them also (when save / load)
                       soSkipNodeSubTypeHelper, // Don't use helper functions like NumberLong() to handle 64-bit integers or NumberInt()
                                                // to handle 32-bit integers
                       soSaveInt64AsText, // JS represents all numbers as double, and with growing integers you loose precision at some point
                                          // use this option to return Int64 as string
                       soProtectedSave); // Save first to a tmp file and then later rename the tmp file to the desired filename
  TALJSONSaveOptions = set of TALJSONSaveOption;

  TALJSONParseOption = (poIgnoreControlCharacters, // Don't decode escaped characters (like \") and not encode them also (when save / load)
                        poClearChildNodes,         // Removes all child nodes
                        poAllowComments);          // Allow comments inside the Json Source file. ex:
                                                   //   {
                                                   //     "nodename": "nodevalue",  // your comments here
                                                   //   }
  TALJSONParseOptions = set of TALJSONParseOption;

  {Exception}
  EALJSONDocError = class(Exception);

  TALJSONTextNodeValueDefined = set of (nvStr, nvInt64);

  //Mirror of TPerlRegExOptions to avoid to include System.RegularExpressionsCore
  TALPerlRegExOptions = set of (
    preCaseLess,       // /i -> Case insensitive
    preMultiLine,      // /m -> ^ and $ also match before/after a newline, not just at the beginning and the end of the string
    preSingleLine,     // /s -> Dot matches any character, including \n (newline). Otherwise, it matches anything except \n
    preExtended,       // /x -> Allow regex to contain extra whitespace, newlines and Perl-style comments, all of which will be filtered out
    preAnchored,       // /A -> Successful match can only occur at the start of the subject or right after the previous match
    preUnGreedy,       // Repeat operators (+, *, ?) are not greedy by default (i.e. they try to match the minimum number of characters instead of the maximum)
    preNoAutoCapture   // (group) is a non-capturing group; only named groups capture
  );

type

  {class definition}
  TALJSONNodeA = Class;
  TALJSONNodeListA= Class;
  TALJSONDocumentA= Class;

  TAlJSONParseTextEventA = reference to procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType);
  TAlJSONParseObjectEventA = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  TAlJSONParseArrayEventA = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);

  TALJSONNodeListSortCompareA = reference to function(List: TALJSONNodeListA; Index1, Index2: Integer): Integer;

  TALJSONPointerListA = array of TALJSONNodeA;

  {TALJSONNodeListA}
  {TALJSONNodeListA is used to represent a set of related nodes (TALJSONNodeA object) in an JSON document. For example, TALJSONNodeListA is used to
   represent all of the children of a node, or all of the attributes of a node. TALJSONNodeListA can be used to add or delete nodes from the
   List, or to access specific nodes.}
  TALJSONNodeListA = class(Tobject)
  Private
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCount: integer;
    FList: TALJSONPointerListA;
    [weak] FOwner: TALJSONNodeA;
    procedure QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareA);
  protected
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    property Owner: TALJSONNodeA read FOwner;
    function Get(Index: Integer): TALJSONNodeA;
    function GetNodeByIndex(Const Index: Integer): TALJSONNodeA;
    function GetNodeByName(Const Name: AnsiString): TALJSONNodeA;
    function CompareNodeNames(const S1, S2: AnsiString): Integer; inline;
    function Find(const NodeName: AnsiString; var Index: Integer): Boolean;
    procedure InternalInsert(Index: Integer; const Node: TALJSONNodeA);
  public
    constructor Create(Owner: TALJSONNodeA);
    destructor Destroy; override;
    procedure Sort;
    procedure CustomSort(Compare: TALJSONNodeListSortCompareA);
    procedure SetDuplicates(Value: TDuplicates; Recurse: Boolean); overload;
    procedure SetDuplicates(Value: TDuplicates); overload;
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    procedure SetSorted(Value: Boolean; Recurse: Boolean); overload;
    procedure SetSorted(Value: Boolean); overload;
    property Sorted: Boolean read FSorted write SetSorted;
    function Add(const Node: TALJSONNodeA): Integer;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: AnsiString): Integer; overload;
    function Extract(const index: integer): TALJSONNodeA; overload;
    function Extract(const Node: TALJSONNodeA): TALJSONNodeA; overload;
    procedure Exchange(Index1, Index2: Integer);
    function FindNode(const NodeName: AnsiString; const Direction: TDirection = TDirection.FromBeginning): TALJSONNodeA;
    function FindSibling(const Node: TALJSONNodeA; Delta: Integer): TALJSONNodeA;
    function First: TALJSONNodeA;
    function IndexOf(const Name: AnsiString; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOf(const Node: TALJSONNodeA; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: ansiString; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: integer; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: int64; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function Last: TALJSONNodeA;
    function Remove(const Node: TALJSONNodeA): Integer;
    function ReplaceNode(const OldNode, NewNode: TALJSONNodeA): TALJSONNodeA;
    procedure Clear;
    procedure Insert(Index: Integer; const Node: TALJSONNodeA);
    property Count: Integer read fCount;
    property Nodes[const Name: AnsiString]: TALJSONNodeA read GetNodeByName; default;
    property Nodes[const Index: integer]: TALJSONNodeA read GetNodeByIndex; default;
  end;

  {TALJSONNodeA}
  {TALJSONNodeA represents a node in an JSON document.}
  TALJSONNodeA = class(TObject)
  private
    [weak] FParentNode: TALJSONNodeA;
    fNodeName: AnsiString;
  protected
    function CreateChildList: TALJSONNodeListA;
    function InternalGetChildNodes: TALJSONNodeListA; virtual;
    function GetChildNodes: TALJSONNodeListA; virtual;
    procedure SetChildNodes(const Value: TALJSONNodeListA); virtual;
    function GetHasChildNodes: Boolean;
    function GetNodeType: TALJSONNodeType; virtual; abstract;
    function GetNodeSubType: TALJSONNodeSubType; virtual; abstract;
    function GetNodeValueStr: ansiString; virtual;
    function GetNodeValueInt64: int64; virtual;
    function GetNodeValueInterchange(const SkipNodeSubTypeHelper: boolean = False): AnsiString;
    procedure SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const StrValue: AnsiString; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeName(const NodeName: AnsiString);
    function GetParentNode: TALJSONNodeA;
    procedure SetParentNode(const Value: TALJSONNodeA);
    function GetJSON: AnsiString;
    procedure SetJSON(const Value: AnsiString);
    function GetBSON: AnsiString;
    procedure SetBSON(const Value: AnsiString);
    function NestingLevel: Integer;
    Procedure ParseJSON(
                const RawJSONStream: TStream;
                const RawJSONString: AnsiString;
                const SaxMode: Boolean;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions);
    Procedure ParseBSON(
                const RawBSONStream: TStream;
                const RawBSONString: AnsiString;
                const SaxMode: Boolean;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions);
    procedure SaveToBson(
                const Stream: TStream;
                Var buffer: ansiString;
                const Options: TALJSONSaveOptions);
    procedure SaveToJson(
                const Stream: TStream;
                Var buffer: ansiString;
                const Options: TALJSONSaveOptions);
  public
    constructor Create(const NodeName: AnsiString); virtual;
    procedure MultiThreadPrepare(const aOnlyChildList: Boolean = False);
    function GetText(const default: AnsiString): AnsiString; overload;
    function GetText: AnsiString; overload;
    procedure SetText(const Value: AnsiString);
    function GetFloat(const default: Double): Double; overload;
    function GetFloat: Double; overload;
    procedure SetFloat(const Value: Double);
    function GetDateTime(const default: TDateTime): TDateTime; overload;
    function GetDateTime: TDateTime; overload;
    procedure SetDateTime(const Value: TDateTime);
    function GetTimestamp(const default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetTimestamp: TALBSONTimestamp; overload;
    procedure SetTimestamp(const Value: TALBSONTimestamp);
    function GetObjectID(const default: AnsiString): AnsiString; overload; // return a "byte" string
    function GetObjectID: AnsiString; overload; // return a "byte" string
    procedure SetObjectID(const Value: AnsiString); // Value is a "byte" string
    function GetInt32(const default: Integer): Integer; overload;
    function GetInt32: Integer; overload;
    procedure SetInt32(const Value: Integer);
    function GetInt64(const default: Int64): Int64; overload;
    function GetInt64: Int64; overload;
    procedure SetInt64(const Value: Int64);
    function GetBool(const default: Boolean): Boolean; overload;
    function GetBool: Boolean; overload;
    procedure SetBool(const Value: Boolean);
    function GetNull: Boolean;
    procedure SetNull(const Value: Boolean);
    function GetJavascript(const default: AnsiString): AnsiString; overload;
    function GetJavascript: AnsiString; overload;
    procedure SetJavascript(const Value: AnsiString);
    function GetRegEx(const default: ansiString): ansiString; overload;
    function GetRegEx: ansiString; overload;
    procedure SetRegEx(const Pattern: ansiString); overload;
    procedure SetRegEx(const Pattern: ansiString; const Options: TALPerlRegExOptions); overload;
    function GetRegExOptions(const default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetRegExOptions: TALPerlRegExOptions; overload;
    procedure SetRegExOptions(const Value: TALPerlRegExOptions);
    function GetBinary(const default: AnsiString): AnsiString; overload; // return a "byte" string
    function GetBinary: AnsiString; overload; // return a "byte" string
    procedure SetBinary(const Data: AnsiString); overload; // data is a "byte" string
    procedure SetBinary(const Data: AnsiString; const Subtype: byte); overload; // data is a "byte" string
    function GetBinarySubType(const default: byte): byte; overload;
    function GetBinarySubType: byte; overload;
    procedure SetBinarySubType(const Subtype: byte);
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA; overload;
    function AddChild(const Path: array of AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA; overload;
    function DeleteChild(const NodeName: AnsiString): boolean; overload;
    function DeleteChild(const Path: array of AnsiString): boolean; overload;
    function CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNodeA;
    function NextSibling: TALJSONNodeA;
    function PreviousSibling: TALJSONNodeA;
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
    procedure LoadFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]); overload;
    procedure LoadFromBSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]); overload;
    procedure ParseJSONString(
                const Str: AnsiString;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []);
    procedure ParseJSONStream(
                const Stream: TStream;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []);
    procedure ParseJSONFile(
                const FileName: String;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []); overload;
    procedure ParseJSONFile(
                const FileName: AnsiString;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []); overload;
    procedure ParseBSONString(
                const Str: AnsiString;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONStream(
                const Stream: TStream;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONFile(
                const FileName: String;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []); overload;
    procedure ParseBSONFile(
                const FileName: AnsiString;
                const onParseText: TAlJSONParseTextEventA;
                const onParseStartObject: TAlJSONParseObjectEventA;
                const onParseEndObject: TAlJSONParseObjectEventA;
                const onParseStartArray: TAlJSONParseArrayEventA;
                const onParseEndArray: TAlJSONParseArrayEventA;
                const Options: TALJSONParseOptions = []); overload;
    property ChildNodes: TALJSONNodeListA read GetChildNodes write SetChildNodes;
    function GetChildNode(const nodeName: ansiString): TALJSONNodeA; overload;
    function GetChildNodeValueText(const nodeName: ansiString; const default: AnsiString): AnsiString; overload;
    function GetChildNodeValueFloat(const nodeName: ansiString; const default: Double): Double; overload;
    function GetChildNodeValueDateTime(const nodeName: ansiString; const default: TDateTime): TDateTime; overload;
    function GetChildNodeValueTimestamp(const nodeName: ansiString; const default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetChildNodeValueObjectID(const nodeName: ansiString; const default: AnsiString): AnsiString; overload;  // return a "byte" string
    function GetChildNodeValueInt32(const nodeName: ansiString; const default: Integer): Integer; overload;
    function GetChildNodeValueInt64(const nodeName: ansiString; const default: Int64): Int64; overload;
    function GetChildNodeValueBool(const nodeName: ansiString; const default: Boolean): Boolean; overload;
    function GetChildNodeValueJavascript(const nodeName: ansiString; const default: AnsiString): AnsiString; overload;
    function GetChildNodeValueRegEx(const nodeName: ansiString; const default: ansiString): ansiString; overload;
    function GetChildNodeValueRegExOptions(const nodeName: ansiString; const default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetChildNodeValueBinary(const nodeName: ansiString; const default: AnsiString): AnsiString; overload;  // return a "byte" string
    function GetChildNodeValueBinarySubType(const nodeName: ansiString; const default: byte): byte; overload;
    function GetChildNodeValueNull(const nodeName: ansiString): Boolean; overload;
    function GetChildNode(const path: array of ansiString): TALJSONNodeA; overload;
    function GetChildNodeValueText(const path: array of ansiString; const default: AnsiString): AnsiString; overload;
    function GetChildNodeValueFloat(const path: array of ansiString; const default: Double): Double; overload;
    function GetChildNodeValueDateTime(const path: array of ansiString; const default: TDateTime): TDateTime; overload;
    function GetChildNodeValueTimestamp(const path: array of ansiString; const default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetChildNodeValueObjectID(const path: array of ansiString; const default: AnsiString): AnsiString; overload;  // return a "byte" string
    function GetChildNodeValueInt32(const path: array of ansiString; const default: Integer): Integer; overload;
    function GetChildNodeValueInt64(const path: array of ansiString; const default: Int64): Int64; overload;
    function GetChildNodeValueBool(const path: array of ansiString; const default: Boolean): Boolean; overload;
    function GetChildNodeValueJavascript(const path: array of ansiString; const default: AnsiString): AnsiString; overload;
    function GetChildNodeValueRegEx(const path: array of ansiString; const default: ansiString): ansiString; overload;
    function GetChildNodeValueRegExOptions(const path: array of ansiString; const default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetChildNodeValueBinary(const path: array of ansiString; const default: AnsiString): AnsiString; overload;  // return a "byte" string
    function GetChildNodeValueBinarySubType(const path: array of ansiString; const default: byte): byte; overload;
    function GetChildNodeValueNull(const path: array of ansiString): Boolean; overload;
    procedure SetChildNodeValueText(const nodeName: ansiString; const value: AnsiString); overload;
    procedure SetChildNodeValueFloat(const nodeName: ansiString; const value: Double); overload;
    procedure SetChildNodeValueDateTime(const nodeName: ansiString; const value: TDateTime); overload;
    procedure SetChildNodeValueTimestamp(const nodeName: ansiString; const value: TALBSONTimestamp); overload;
    procedure SetChildNodeValueObjectID(const nodeName: ansiString; const value: AnsiString); overload;
    procedure SetChildNodeValueInt32(const nodeName: ansiString; const value: Integer); overload;
    procedure SetChildNodeValueInt64(const nodeName: ansiString; const value: Int64); overload;
    procedure SetChildNodeValueBool(const nodeName: ansiString; const value: Boolean); overload;
    procedure SetChildNodeValueJavascript(const nodeName: ansiString; const value: AnsiString); overload;
    procedure SetChildNodeValueRegEx(const nodeName: ansiString; const value: ansiString); overload;
    procedure SetChildNodeValueRegExOptions(const nodeName: ansiString; const value: TALPerlRegExOptions); overload;
    procedure SetChildNodeValueBinary(const nodeName: ansiString; const value: AnsiString); overload;
    procedure SetChildNodeValueBinarySubType(const nodeName: ansiString; const value: byte); overload;
    procedure SetChildNodeValueNull(const nodeName: ansiString); overload;
    procedure SetChildNodeValueText(const path: array of ansiString; const value: AnsiString); overload;
    procedure SetChildNodeValueFloat(const path: array of ansiString; const value: Double); overload;
    procedure SetChildNodeValueDateTime(const path: array of ansiString; const value: TDateTime); overload;
    procedure SetChildNodeValueTimestamp(const path: array of ansiString; const value: TALBSONTimestamp); overload;
    procedure SetChildNodeValueObjectID(const path: array of ansiString; const value: AnsiString); overload;
    procedure SetChildNodeValueInt32(const path: array of ansiString; const value: Integer); overload;
    procedure SetChildNodeValueInt64(const path: array of ansiString; const value: Int64); overload;
    procedure SetChildNodeValueBool(const path: array of ansiString; const value: Boolean); overload;
    procedure SetChildNodeValueJavascript(const path: array of ansiString; const value: AnsiString); overload;
    procedure SetChildNodeValueRegEx(const path: array of ansiString; const value: ansiString); overload;
    procedure SetChildNodeValueRegExOptions(const path: array of ansiString; const value: TALPerlRegExOptions); overload;
    procedure SetChildNodeValueBinary(const path: array of ansiString; const value: AnsiString); overload;
    procedure SetChildNodeValueBinarySubType(const path: array of ansiString; const value: byte); overload;
    procedure SetChildNodeValueNull(const path: array of ansiString); overload;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property NodeName: AnsiString read fNodeName write SetNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property NodeValue: AnsiString read GetNodeValueStr; // same as text property but without formating
    property NodeSubType: TALJSONNodeSubType read GetNodeSubType;
    property ParentNode: TALJSONNodeA read GetParentNode;
    property Text: AnsiString read GetText write SetText;
    property int32: integer read GetInt32 write SetInt32;
    property int64: int64 read Getint64 write Setint64;
    property Float: Double read GetFloat write SetFloat;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Timestamp: TALBSONTimestamp read GetTimestamp write SetTimestamp; // Use only by MongoDB, do not use it, use DateTime instead !
    property ObjectID: AnsiString read GetObjectID write SetObjectID; // return a "byte" string
    property Bool: Boolean read GetBool write SetBool;
    property Null: Boolean read GetNull write SetNull;
    property Javascript: AnsiString read GetJavascript write SetJavascript;
    property RegEx: AnsiString read GetRegEx write SetRegEx;
    property RegExOptions: TALPerlRegExOptions read GetRegExOptions write SetRegExOptions;
    property Binary: ansiString read GetBinary write SetBinary; // return a "byte" string
    property BinarySubType: Byte read GetBinarySubType write SetBinarySubType;
    property JSON: AnsiString read GetJSON write SetJSON;
    property BSON: AnsiString read GetBSON write SetBSON;
  end;

  //JSON object represents {} or { members }
  TALJSONObjectNodeA = Class(TALJSONNodeA)
  private
    FChildNodes: TALJSONNodeListA;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListA; override;
    function GetChildNodes: TALJSONNodeListA; override;
    procedure SetChildNodes(const Value: TALJSONNodeListA); override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    Destructor Destroy; override;
  end;

  {implements JSON array [] | [ elements ]}
  TALJSONArrayNodeA = Class(TALJSONNodeA)
  private
    FChildNodes: TALJSONNodeListA;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListA; override;
    function GetChildNodes: TALJSONNodeListA; override;
    procedure SetChildNodes(const Value: TALJSONNodeListA); override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    Destructor Destroy; override;
  end;

  {Groups javascript, string, number, true, false, null}
  TALJSONTextNodeA = Class(TALJSONNodeA)
  private
    FRawNodeValueInt64: int64;    // contain the value Stored in an int64 (if the
                                  // value can be store in an int64)
    fRawNodeValueStr: AnsiString; // contain the text representation of the node
                                  // WITHOUT any node subtype helper
                                  // for exemple for NumberLong(12391293) it's
                                  // store only 12391293
    fRawNodeValueDefined: TALJSONTextNodeValueDefined;
    fNodeSubType: TALJSONNodeSubType;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function GetNodeValueStr: ansiString; override;
    function GetNodeValueInt64: int64; override;
    procedure SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType); overload; override;
    procedure SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType); overload; override;
    procedure SetNodeValue(const StrValue: AnsiString; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType); overload; override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    property RawNodeValueDefined: TALJSONTextNodeValueDefined read fRawNodeValueDefined;
    property RawNodeValueStr: AnsiString read fRawNodeValueStr;
    property RawNodeValueint64: int64 read fRawNodeValueint64;
  end;

  {TALJSONDocumentA}
  TALJSONDocumentA = class(TObject)
  private
    class function DetectNodeTypeFromJSon(
                     const RawJSONStream: TStream;
                     const RawJSONString: AnsiString): TALJSONNodeType;
  public
    class function Create: TALJSONNodeA;
    class function CreateFromJSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
    class function CreateFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
    class function CreateFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA; overload;
    class function CreateFromJSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA; overload;
    class function CreateFromBSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
    class function CreateFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
    class function CreateFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA; overload;
    class function CreateFromBSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA; overload;
    class procedure ParseJSONString(
                      const Str: AnsiString;
                      const onParseText: TAlJSONParseTextEventA;
                      const onParseStartObject: TAlJSONParseObjectEventA;
                      const onParseEndObject: TAlJSONParseObjectEventA;
                      const onParseStartArray: TAlJSONParseArrayEventA;
                      const onParseEndArray: TAlJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseJSONStream(
                      const Stream: TStream;
                      const onParseText: TAlJSONParseTextEventA;
                      const onParseStartObject: TAlJSONParseObjectEventA;
                      const onParseEndObject: TAlJSONParseObjectEventA;
                      const onParseStartArray: TAlJSONParseArrayEventA;
                      const onParseEndArray: TAlJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseJSONFile(
                      const FileName: String;
                      const onParseText: TAlJSONParseTextEventA;
                      const onParseStartObject: TAlJSONParseObjectEventA;
                      const onParseEndObject: TAlJSONParseObjectEventA;
                      const onParseStartArray: TAlJSONParseArrayEventA;
                      const onParseEndArray: TAlJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []); overload;
    class procedure ParseJSONFile(
                      const FileName: AnsiString;
                      const onParseText: TAlJSONParseTextEventA;
                      const onParseStartObject: TAlJSONParseObjectEventA;
                      const onParseEndObject: TAlJSONParseObjectEventA;
                      const onParseStartArray: TAlJSONParseArrayEventA;
                      const onParseEndArray: TAlJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []); overload;
    class procedure ParseBSONString(
                      const Str: AnsiString;
                      const onParseText: TAlJSONParseTextEventA;
                      const onParseStartObject: TAlJSONParseObjectEventA;
                      const onParseEndObject: TAlJSONParseObjectEventA;
                      const onParseStartArray: TAlJSONParseArrayEventA;
                      const onParseEndArray: TAlJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONStream(
                      const Stream: TStream;
                      const onParseText: TAlJSONParseTextEventA;
                      const onParseStartObject: TAlJSONParseObjectEventA;
                      const onParseEndObject: TAlJSONParseObjectEventA;
                      const onParseStartArray: TAlJSONParseArrayEventA;
                      const onParseEndArray: TAlJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONFile(
                      const FileName: String;
                      const onParseText: TAlJSONParseTextEventA;
                      const onParseStartObject: TAlJSONParseObjectEventA;
                      const onParseEndObject: TAlJSONParseObjectEventA;
                      const onParseStartArray: TAlJSONParseArrayEventA;
                      const onParseEndArray: TAlJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []); overload;
    class procedure ParseBSONFile(
                      const FileName: AnsiString;
                      const onParseText: TAlJSONParseTextEventA;
                      const onParseStartObject: TAlJSONParseObjectEventA;
                      const onParseEndObject: TAlJSONParseObjectEventA;
                      const onParseStartArray: TAlJSONParseArrayEventA;
                      const onParseEndArray: TAlJSONParseArrayEventA;
                      const Options: TALJSONParseOptions = []); overload;
  end;

{misc constants}
var
  ALDefaultJsonNodeIndentA: ansiString;  // var instead of const to avoid new ansitring on assign
  ALDefaultJsonPathSeparatorA: AnsiChar;
  ALJsonISODateFormatSettingsA: TALFormatSettingsA;

{misc function}
Procedure ALJSONToTStringsA(
            const AJsonStr: AnsiString;
            const aFormatSettings: TALFormatSettingsA;
            const aPath: AnsiString;
            const aLst: TALStringsA;
            Const aNullStr: AnsiString = 'null';
            Const aTrueStr: AnsiString = 'true';
            Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStringsA(
            const AJsonStr: AnsiString;
            const aFormatSettings: TALFormatSettingsA;
            const aLst: TALStringsA;
            Const aNullStr: AnsiString = 'null';
            Const aTrueStr: AnsiString = 'true';
            Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStringsA(
            const aJsonNode: TALJSONNodeA;
            Const aPath: AnsiString;
            const aLst: TALStringsA;
            Const aNullStr: AnsiString = 'null';
            Const aTrueStr: AnsiString = 'true';
            Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStringsA(
            const aJsonNode: TALJSONNodeA;
            const aLst: TALStringsA;
            Const aNullStr: AnsiString = 'null';
            Const aTrueStr: AnsiString = 'true';
            Const aFalseStr: AnsiString = 'false'); overload;
procedure ALTStringsToJsonA(
            const aLst: TALStringsA;
            const aJsonNode: TALJSONNodeA;
            Const aPath: AnsiString = '';
            Const aNameToLowerCase: boolean = false;
            Const aNullStr: AnsiString = 'null');

Procedure ALJSONToXMLA(
            const aJSONNode: TALJSONNodeA;
            const aXMLNode: TALXmlNode;
            const aXMLElementNameForJSONArrayEntries: TALStringsA; // JSONArrayNodeName=XMLElementName
            const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec'); overload;
Procedure ALJSONToXMLA(
            const aJSONNode: TALJSONNodeA;
            const aXMLNode: TALXmlNode;
            const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec'); overload;

function ALJsonEncodeFloatWithNodeSubTypeHelperA(const aValue: double): AnsiString;
function ALJsonEncodeTextWithNodeSubTypeHelperA(const aValue: AnsiString): AnsiString;
function ALJsonEncodeBinaryWithNodeSubTypeHelperA(const aValue: AnsiString): AnsiString;
function ALJsonEncodeObjectIDWithNodeSubTypeHelperA(const aValue: AnsiString): AnsiString;
function ALJsonEncodeBooleanWithNodeSubTypeHelperA(const aValue: Boolean): AnsiString;
function ALJsonEncodeDateTimeWithNodeSubTypeHelperA(const aValue: TdateTime): AnsiString;
function ALJsonEncodeJavascriptWithNodeSubTypeHelperA(const aValue: AnsiString): AnsiString;
function ALJsonEncodeInt64WithNodeSubTypeHelperA(const aValue: int64): AnsiString;
function ALJsonEncodeInt32WithNodeSubTypeHelperA(const aValue: int32): AnsiString;
function ALJsonEncodeNullWithNodeSubTypeHelperA: AnsiString;
function ALJsonEncodeWithNodeSubTypeHelperA(
           const aValue: AnsiString;
           const aNodeSubType: TALJSONNodeSubType;
           const aFormatSettings: TALFormatSettingsA): AnsiString;

function ALJSONTryStrToRegExA(const S: AnsiString; out RegEx: AnsiString; out RegExOptions: TALPerlRegExOptions): boolean;
function ALJSONTryStrToBinaryA(const S: AnsiString; out Data: AnsiString; out Subtype: byte): boolean; // return a "byte" string
function ALJSONTryStrToDateTimeA(const S: AnsiString; out Value: TDateTime): Boolean;
function ALJSONTryStrToObjectIDA(const S: AnsiString; out Value: ansiString): Boolean; // return a "byte" string
function ALJSONTryStrToTimestampA(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
function ALJSONTryStrToInt32A(const S: AnsiString; out Value: integer): Boolean;
function ALJSONTryStrToInt64A(const S: AnsiString; out Value: int64): Boolean;

Function ALFindJsonNodeByInt32ChildNodeValueA(
           const JsonNode:TALJSONNodeA;
           Const ChildNodeName: AnsiString;
           Const ChildNodeValue : Int32;
           Const Recurse: Boolean = False): TALJSONNodeA;
Function ALFindJsonNodeByTextChildNodeValueA(
           const JsonNode:TALJSONNodeA;
           Const ChildNodeName: AnsiString;
           Const ChildNodeValue : AnsiString;
           Const Recurse: Boolean = False): TALJSONNodeA;

type

  {class definition}
  TALJSONNodeW = Class;
  TALJSONNodeListW = Class;
  TALJSONDocumentW = Class;

  TAlJSONParseDocumentW = reference to procedure (Sender: TObject);
  TAlJSONParseTextEventW = reference to procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType);
  TAlJSONParseObjectEventW = reference to procedure (Sender: TObject; const Path: String; const Name: String);
  TAlJSONParseArrayEventW = reference to procedure (Sender: TObject; const Path: String; const Name: String);

  TALJSONNodeListSortCompareW = reference to function(List: TALJSONNodeListW; Index1, Index2: Integer): Integer;

  TALJSONPointerListW = array of TALJSONNodeW;

  {TALJSONNodeListW}
  {TALJSONNodeListW is used to represent a set of related nodes (TALJSONNodeW object) in an JSON document. For example, TALJSONNodeListW is used to
   represent all of the children of a node, or all of the attributes of a node. TALJSONNodeListW can be used to add or delete nodes from the
   List, or to access specific nodes.}
  TALJSONNodeListW = class(Tobject)
  Private
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCount: integer;
    FList: TALJSONPointerListW;
    [weak] FOwner: TALJSONNodeW;
    procedure QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareW);
  protected
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    property Owner: TALJSONNodeW read FOwner;
    function Get(Index: Integer): TALJSONNodeW;
    function GetNodeByIndex(Const Index: Integer): TALJSONNodeW;
    function GetNodeByName(Const Name: String): TALJSONNodeW;
    function CompareNodeNames(const S1, S2: String): Integer; inline;
    function Find(const NodeName: String; var Index: Integer): Boolean;
    procedure InternalInsert(Index: Integer; const Node: TALJSONNodeW);
  public
    constructor Create(Owner: TALJSONNodeW);
    destructor Destroy; override;
    procedure Sort;
    procedure CustomSort(Compare: TALJSONNodeListSortCompareW);
    procedure SetDuplicates(Value: TDuplicates; Recurse: Boolean); overload;
    procedure SetDuplicates(Value: TDuplicates); overload;
    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    procedure SetSorted(Value: Boolean; Recurse: Boolean); overload;
    procedure SetSorted(Value: Boolean); overload;
    property Sorted: Boolean read FSorted write SetSorted;
    function Add(const Node: TALJSONNodeW): Integer;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: String): Integer; overload;
    function Extract(const index: integer): TALJSONNodeW; overload;
    function Extract(const Node: TALJSONNodeW): TALJSONNodeW; overload;
    procedure Exchange(Index1, Index2: Integer);
    function FindNode(const NodeName: String; const Direction: TDirection = TDirection.FromBeginning): TALJSONNodeW;
    function FindSibling(const Node: TALJSONNodeW; Delta: Integer): TALJSONNodeW;
    function First: TALJSONNodeW;
    function IndexOf(const Name: String; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOf(const Node: TALJSONNodeW; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: String; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: integer; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: int64; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function Last: TALJSONNodeW;
    function Remove(const Node: TALJSONNodeW): Integer;
    function ReplaceNode(const OldNode, NewNode: TALJSONNodeW): TALJSONNodeW;
    procedure Clear;
    procedure Insert(Index: Integer; const Node: TALJSONNodeW);
    property Count: Integer read fCount;
    property Nodes[const Name: String]: TALJSONNodeW read GetNodeByName; default;
    property Nodes[const Index: integer]: TALJSONNodeW read GetNodeByIndex; default;
  end;

  {TALJSONNodeW}
  {TALJSONNodeW represents a node in an JSON document.}
  TALJSONNodeW = class(TObject)
  private
    [weak] FParentNode: TALJSONNodeW;
    fNodeName: String;
  protected
    function CreateChildList: TALJSONNodeListW;
    function InternalGetChildNodes: TALJSONNodeListW; virtual;
    function GetChildNodes: TALJSONNodeListW; virtual;
    procedure SetChildNodes(const Value: TALJSONNodeListW); virtual;
    function GetHasChildNodes: Boolean;
    function GetNodeType: TALJSONNodeType; virtual; abstract;
    function GetNodeSubType: TALJSONNodeSubType; virtual; abstract;
    function GetNodeValueStr: String; virtual;
    function GetNodeValueInt64: int64; virtual;
    function GetNodeValueInterchange(const SkipNodeSubTypeHelper: boolean = False): String;
    procedure SetNodeValue(const Value: String; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const StrValue: String; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeName(const NodeName: String);
    function GetParentNode: TALJSONNodeW;
    procedure SetParentNode(const Value: TALJSONNodeW);
    function GetJSON: String;
    procedure SetJSON(const Value: String);
    function GetBSON: Tbytes;
    procedure SetBSON(const Value: Tbytes);
    function NestingLevel: Integer;
    Procedure ParseJSON(
                const Buffer: String;
                const SaxMode: Boolean;
                const onParseText: TAlJSONParseTextEventW;
                const onParseStartObject: TAlJSONParseObjectEventW;
                const onParseEndObject: TAlJSONParseObjectEventW;
                const onParseStartArray: TAlJSONParseArrayEventW;
                const onParseEndArray: TAlJSONParseArrayEventW;
                const Options: TALJSONParseOptions);
    Procedure ParseBSON(
                const Buffer: Tbytes;
                const SaxMode: Boolean;
                const onParseText: TAlJSONParseTextEventW;
                const onParseStartObject: TAlJSONParseObjectEventW;
                const onParseEndObject: TAlJSONParseObjectEventW;
                const onParseStartArray: TAlJSONParseArrayEventW;
                const onParseEndArray: TAlJSONParseArrayEventW;
                const Options: TALJSONParseOptions);
    procedure SaveToBson(
                const Stream: TStream;
                Var buffer: Tbytes;
                const Options: TALJSONSaveOptions);
    procedure SaveToJson(
                const Stream: TStream;
                const StreamEncoding: TEncoding;
                Var buffer: String;
                const Options: TALJSONSaveOptions);
  public
    constructor Create(const NodeName: String); virtual;
    procedure MultiThreadPrepare(const aOnlyChildList: Boolean = False);
    function GetText(const default: String): String; overload;
    function GetText: String; overload;
    procedure SetText(const Value: String);
    function GetFloat(const default: Double): Double; overload;
    function GetFloat: Double; overload;
    procedure SetFloat(const Value: Double);
    function GetDateTime(const default: TDateTime): TDateTime; overload;
    function GetDateTime: TDateTime; overload;
    procedure SetDateTime(const Value: TDateTime);
    function GetTimestamp(const default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetTimestamp: TALBSONTimestamp; overload;
    procedure SetTimestamp(const Value: TALBSONTimestamp);
    function GetObjectID(const default: String): String; overload; // return a hex string
    function GetObjectID: String; overload; // return a hex string
    procedure SetObjectID(const Value: String); // return a hex string
    function GetInt32(const default: Integer): Integer; overload;
    function GetInt32: Integer; overload;
    procedure SetInt32(const Value: Integer);
    function GetInt64(const default: Int64): Int64; overload;
    function GetInt64: Int64; overload;
    procedure SetInt64(const Value: Int64);
    function GetBool(const default: Boolean): Boolean; overload;
    function GetBool: Boolean; overload;
    procedure SetBool(const Value: Boolean);
    function GetNull: Boolean;
    procedure SetNull(const Value: Boolean);
    function GetJavascript(const default: String): String; overload;
    function GetJavascript: String; overload;
    procedure SetJavascript(const Value: String);
    function GetRegEx(const default: String): String; overload;
    function GetRegEx: String; overload;
    procedure SetRegEx(const Pattern: String); overload;
    procedure SetRegEx(const Pattern: String; const Options: TALPerlRegExOptions); overload;
    function GetRegExOptions(const default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetRegExOptions: TALPerlRegExOptions; overload;
    procedure SetRegExOptions(const Value: TALPerlRegExOptions);
    function GetBinary(const default: String): String; overload; // return a base64 encoded string
    function GetBinary: String; overload; // return a base64 encoded string
    procedure SetBinary(const Data: String); overload; // data is a base64 encoded string
    procedure SetBinary(const Data: String; const Subtype: byte); overload; // data is a base64 encoded string
    function GetBinarySubType(const default: byte): byte; overload;
    function GetBinarySubType: byte; overload;
    procedure SetBinarySubType(const Subtype: byte);
    function AddChild(const NodeName: String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW; overload;
    function AddChild(const Path: array of String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW; overload;
    function DeleteChild(const NodeName: String): boolean; overload;
    function DeleteChild(const Path: array of String): boolean; overload;
    function CreateNode(const NodeName: String; NodeType: TALJSONNodeType): TALJSONNodeW;
    function NextSibling: TALJSONNodeW;
    function PreviousSibling: TALJSONNodeW;
    procedure SaveToJSONStream(const Stream: TStream; const Encoding: TEncoding; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONFile(const FileName: String; const Encoding: TEncoding; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONFile(const FileName: String; const Options: TALJSONSaveOptions = []); overload;
    procedure SaveToJSONString(var Str: String; const Options: TALJSONSaveOptions = []);
    procedure SaveToBSONStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
    procedure SaveToBSONFile(const FileName: String; const Options: TALJSONSaveOptions = []);
    procedure SaveToBSONBytes(var Bytes: Tbytes; const Options: TALJSONSaveOptions = []);
    procedure LoadFromJSONString(const Str: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONBytes(const Bytes: Tbytes; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure LoadFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
    procedure ParseJSONString(
                const Str: String;
                const onParseText: TAlJSONParseTextEventW;
                const onParseStartObject: TAlJSONParseObjectEventW;
                const onParseEndObject: TAlJSONParseObjectEventW;
                const onParseStartArray: TAlJSONParseArrayEventW;
                const onParseEndArray: TAlJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseJSONStream(
                const Stream: TStream;
                const onParseText: TAlJSONParseTextEventW;
                const onParseStartObject: TAlJSONParseObjectEventW;
                const onParseEndObject: TAlJSONParseObjectEventW;
                const onParseStartArray: TAlJSONParseArrayEventW;
                const onParseEndArray: TAlJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseJSONFile(
                const FileName: String;
                const onParseText: TAlJSONParseTextEventW;
                const onParseStartObject: TAlJSONParseObjectEventW;
                const onParseEndObject: TAlJSONParseObjectEventW;
                const onParseStartArray: TAlJSONParseArrayEventW;
                const onParseEndArray: TAlJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONBytes(
                const Bytes: Tbytes;
                const onParseText: TAlJSONParseTextEventW;
                const onParseStartObject: TAlJSONParseObjectEventW;
                const onParseEndObject: TAlJSONParseObjectEventW;
                const onParseStartArray: TAlJSONParseArrayEventW;
                const onParseEndArray: TAlJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONStream(
                const Stream: TStream;
                const onParseText: TAlJSONParseTextEventW;
                const onParseStartObject: TAlJSONParseObjectEventW;
                const onParseEndObject: TAlJSONParseObjectEventW;
                const onParseStartArray: TAlJSONParseArrayEventW;
                const onParseEndArray: TAlJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    procedure ParseBSONFile(
                const FileName: String;
                const onParseText: TAlJSONParseTextEventW;
                const onParseStartObject: TAlJSONParseObjectEventW;
                const onParseEndObject: TAlJSONParseObjectEventW;
                const onParseStartArray: TAlJSONParseArrayEventW;
                const onParseEndArray: TAlJSONParseArrayEventW;
                const Options: TALJSONParseOptions = []);
    property ChildNodes: TALJSONNodeListW read GetChildNodes write SetChildNodes;
    function GetChildNode(const nodeName: String): TALJSONNodeW; overload;
    function GetChildNodeValueText(const nodeName: String; const default: String): String; overload;
    function GetChildNodeValueFloat(const nodeName: String; const default: Double): Double; overload;
    function GetChildNodeValueDateTime(const nodeName: String; const default: TDateTime): TDateTime; overload;
    function GetChildNodeValueTimestamp(const nodeName: String; const default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetChildNodeValueObjectID(const nodeName: String; const default: String): String; overload; // return a hex string
    function GetChildNodeValueInt32(const nodeName: String; const default: Integer): Integer; overload;
    function GetChildNodeValueInt64(const nodeName: String; const default: Int64): Int64; overload;
    function GetChildNodeValueBool(const nodeName: String; const default: Boolean): Boolean; overload;
    function GetChildNodeValueJavascript(const nodeName: String; const default: String): String; overload;
    function GetChildNodeValueRegEx(const nodeName: String; const default: String): String; overload;
    function GetChildNodeValueRegExOptions(const nodeName: String; const default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetChildNodeValueBinary(const nodeName: String; const default: String): String; overload;  // return a base64 encoded string
    function GetChildNodeValueBinarySubType(const nodeName: String; const default: byte): byte; overload;
    function GetChildNodeValueNull(const nodeName: String): Boolean; overload;
    function GetChildNode(const path: array of String): TALJSONNodeW; overload;
    function GetChildNodeValueText(const path: array of String; const default: String): String; overload;
    function GetChildNodeValueFloat(const path: array of String; const default: Double): Double; overload;
    function GetChildNodeValueDateTime(const path: array of String; const default: TDateTime): TDateTime; overload;
    function GetChildNodeValueTimestamp(const path: array of String; const default: TALBSONTimestamp): TALBSONTimestamp; overload;
    function GetChildNodeValueObjectID(const path: array of String; const default: String): String; overload;  // return a hex string
    function GetChildNodeValueInt32(const path: array of String; const default: Integer): Integer; overload;
    function GetChildNodeValueInt64(const path: array of String; const default: Int64): Int64; overload;
    function GetChildNodeValueBool(const path: array of String; const default: Boolean): Boolean; overload;
    function GetChildNodeValueJavascript(const path: array of String; const default: String): String; overload;
    function GetChildNodeValueRegEx(const path: array of String; const default: String): String; overload;
    function GetChildNodeValueRegExOptions(const path: array of String; const default: TALPerlRegExOptions): TALPerlRegExOptions; overload;
    function GetChildNodeValueBinary(const path: array of String; const default: String): String; overload;  // return a base64 encoded string
    function GetChildNodeValueBinarySubType(const path: array of String; const default: byte): byte; overload;
    function GetChildNodeValueNull(const path: array of String): Boolean; overload;
    procedure SetChildNodeValueText(const nodeName: String; const value: String); overload;
    procedure SetChildNodeValueFloat(const nodeName: String; const value: Double); overload;
    procedure SetChildNodeValueDateTime(const nodeName: String; const value: TDateTime); overload;
    procedure SetChildNodeValueTimestamp(const nodeName: String; const value: TALBSONTimestamp); overload;
    procedure SetChildNodeValueObjectID(const nodeName: String; const value: String); overload;
    procedure SetChildNodeValueInt32(const nodeName: String; const value: Integer); overload;
    procedure SetChildNodeValueInt64(const nodeName: String; const value: Int64); overload;
    procedure SetChildNodeValueBool(const nodeName: String; const value: Boolean); overload;
    procedure SetChildNodeValueJavascript(const nodeName: String; const value: String); overload;
    procedure SetChildNodeValueRegEx(const nodeName: String; const value: String); overload;
    procedure SetChildNodeValueRegExOptions(const nodeName: String; const value: TALPerlRegExOptions); overload;
    procedure SetChildNodeValueBinary(const nodeName: String; const value: String); overload;
    procedure SetChildNodeValueBinarySubType(const nodeName: String; const value: byte); overload;
    procedure SetChildNodeValueNull(const nodeName: String); overload;
    procedure SetChildNodeValueText(const path: array of String; const value: String); overload;
    procedure SetChildNodeValueFloat(const path: array of String; const value: Double); overload;
    procedure SetChildNodeValueDateTime(const path: array of String; const value: TDateTime); overload;
    procedure SetChildNodeValueTimestamp(const path: array of String; const value: TALBSONTimestamp); overload;
    procedure SetChildNodeValueObjectID(const path: array of String; const value: String); overload;
    procedure SetChildNodeValueInt32(const path: array of String; const value: Integer); overload;
    procedure SetChildNodeValueInt64(const path: array of String; const value: Int64); overload;
    procedure SetChildNodeValueBool(const path: array of String; const value: Boolean); overload;
    procedure SetChildNodeValueJavascript(const path: array of String; const value: String); overload;
    procedure SetChildNodeValueRegEx(const path: array of String; const value: String); overload;
    procedure SetChildNodeValueRegExOptions(const path: array of String; const value: TALPerlRegExOptions); overload;
    procedure SetChildNodeValueBinary(const path: array of String; const value: String); overload;
    procedure SetChildNodeValueBinarySubType(const path: array of String; const value: byte); overload;
    procedure SetChildNodeValueNull(const path: array of String); overload;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property NodeName: String read fNodeName write SetNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property NodeValue: String read GetNodeValueStr; // same as text property but without formating
    property NodeSubType: TALJSONNodeSubType read GetNodeSubType;
    property ParentNode: TALJSONNodeW read GetParentNode;
    property Text: String read GetText write SetText;
    property int32: integer read GetInt32 write SetInt32;
    property int64: int64 read Getint64 write Setint64;
    property Float: Double read GetFloat write SetFloat;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Timestamp: TALBSONTimestamp read GetTimestamp write SetTimestamp; // Use only by MongoDB, do not use it, use DateTime instead !
    property ObjectID: String read GetObjectID write SetObjectID; // return a hex string
    property Bool: Boolean read GetBool write SetBool;
    property Null: Boolean read GetNull write SetNull;
    property Javascript: String read GetJavascript write SetJavascript;
    property RegEx: String read GetRegEx write SetRegEx;
    property RegExOptions: TALPerlRegExOptions read GetRegExOptions write SetRegExOptions;
    property Binary: String read GetBinary write SetBinary; // return a base64 encoded string
    property BinarySubType: Byte read GetBinarySubType write SetBinarySubType;
    property JSON: String read GetJSON write SetJSON;
    property BSON: Tbytes read GetBSON write SetBSON;
  end;

  //JSON object represents {} or { members }
  TALJSONObjectNodeW = Class(TALJSONNodeW)
  private
    FChildNodes: TALJSONNodeListW;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListW; override;
    function GetChildNodes: TALJSONNodeListW; override;
    procedure SetChildNodes(const Value: TALJSONNodeListW); override;
  public
    constructor Create(const NodeName: String = ''); override;
    Destructor Destroy; override;
  end;

  {implements JSON array [] | [ elements ]}
  TALJSONArrayNodeW = Class(TALJSONNodeW)
  private
    FChildNodes: TALJSONNodeListW;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListW; override;
    function GetChildNodes: TALJSONNodeListW; override;
    procedure SetChildNodes(const Value: TALJSONNodeListW); override;
  public
    constructor Create(const NodeName: String = ''); override;
    Destructor Destroy; override;
  end;

  {Groups javascript, string, number, true, false, null}
  TALJSONTextNodeW = Class(TALJSONNodeW)
  private
    FRawNodeValueInt64: int64; // contain the value Stored in an int64 (if the
                               // value can be store in an int64)
    fRawNodeValueStr: String;  // contain the text representation of the node
                               // WITHOUT any node subtype helper
                               // for exemple for NumberLong(12391293) it's
                               // store only 12391293
    fRawNodeValueDefined: TALJSONTextNodeValueDefined;
    fNodeSubType: TALJSONNodeSubType;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function GetNodeValueStr: String; override;
    function GetNodeValueInt64: int64; override;
    procedure SetNodeValue(const Value: String; const NodeSubType: TALJSONNodeSubType); overload; override;
    procedure SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType); overload; override;
    procedure SetNodeValue(const StrValue: String; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType); overload; override;
  public
    constructor Create(const NodeName: String = ''); override;
    property RawNodeValueDefined: TALJSONTextNodeValueDefined read fRawNodeValueDefined;
    property RawNodeValueStr: String read fRawNodeValueStr;
    property RawNodeValueint64: int64 read fRawNodeValueint64;
  end;

  {TALJSONDocumentW}
  TALJSONDocumentW = class(TObject)
  private
    class function DetectNodeTypeFromJSon(const Buffer: String): TALJSONNodeType;
  public
    class function Create: TALJSONNodeW;
    class function CreateFromJSONString(const Str: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromBSONBytes(const Bytes: Tbytes; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class function CreateFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
    class procedure ParseJSONString(
                      const Str: String;
                      const onParseText: TAlJSONParseTextEventW;
                      const onParseStartObject: TAlJSONParseObjectEventW;
                      const onParseEndObject: TAlJSONParseObjectEventW;
                      const onParseStartArray: TAlJSONParseArrayEventW;
                      const onParseEndArray: TAlJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseJSONStream(
                      const Stream: TStream;
                      const onParseText: TAlJSONParseTextEventW;
                      const onParseStartObject: TAlJSONParseObjectEventW;
                      const onParseEndObject: TAlJSONParseObjectEventW;
                      const onParseStartArray: TAlJSONParseArrayEventW;
                      const onParseEndArray: TAlJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseJSONFile(
                      const FileName: String;
                      const onParseText: TAlJSONParseTextEventW;
                      const onParseStartObject: TAlJSONParseObjectEventW;
                      const onParseEndObject: TAlJSONParseObjectEventW;
                      const onParseStartArray: TAlJSONParseArrayEventW;
                      const onParseEndArray: TAlJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONBytes(
                      const Bytes: Tbytes;
                      const onParseText: TAlJSONParseTextEventW;
                      const onParseStartObject: TAlJSONParseObjectEventW;
                      const onParseEndObject: TAlJSONParseObjectEventW;
                      const onParseStartArray: TAlJSONParseArrayEventW;
                      const onParseEndArray: TAlJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONStream(
                      const Stream: TStream;
                      const onParseText: TAlJSONParseTextEventW;
                      const onParseStartObject: TAlJSONParseObjectEventW;
                      const onParseEndObject: TAlJSONParseObjectEventW;
                      const onParseStartArray: TAlJSONParseArrayEventW;
                      const onParseEndArray: TAlJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
    class procedure ParseBSONFile(
                      const FileName: String;
                      const onParseText: TAlJSONParseTextEventW;
                      const onParseStartObject: TAlJSONParseObjectEventW;
                      const onParseEndObject: TAlJSONParseObjectEventW;
                      const onParseStartArray: TAlJSONParseArrayEventW;
                      const onParseEndArray: TAlJSONParseArrayEventW;
                      const Options: TALJSONParseOptions = []);
  end;

{misc constants}
var
  ALDefaultJsonNodeIndentW: String;  // var instead of const to avoid new ansitring on assign
  ALDefaultJsonPathSeparatorW: Char;
  ALJsonISODateFormatSettingsW: TALFormatSettingsW;

{misc function}
Procedure ALJSONToTStringsW(
            const AJsonStr: String;
            const aFormatSettings: TALFormatSettingsW;
            const aPath: String;
            const aLst: TALStringsW;
            Const aNullStr: String = 'null';
            Const aTrueStr: String = 'true';
            Const aFalseStr: String = 'false'); overload;
Procedure ALJSONToTStringsW(
            const AJsonStr: String;
            const aFormatSettings: TALFormatSettingsW;
            const aLst: TALStringsW;
            Const aNullStr: String = 'null';
            Const aTrueStr: String = 'true';
            Const aFalseStr: String = 'false'); overload;
Procedure ALJSONToTStringsW(
            const aJsonNode: TALJSONNodeW;
            Const aPath: String;
            const aLst: TALStringsW;
            Const aNullStr: String = 'null';
            Const aTrueStr: String = 'true';
            Const aFalseStr: String = 'false'); overload;
Procedure ALJSONToTStringsW(
            const aJsonNode: TALJSONNodeW;
            const aLst: TALStringsW;
            Const aNullStr: String = 'null';
            Const aTrueStr: String = 'true';
            Const aFalseStr: String = 'false'); overload;
procedure ALTStringsToJsonW(
            const aLst: TALStringsW;
            const aJsonNode: TALJSONNodeW;
            Const aPath: String = '';
            Const aNameToLowerCase: boolean = false;
            Const aNullStr: String = 'null');

function ALJsonEncodeFloatWithNodeSubTypeHelperW(const aValue: double): String;
function ALJsonEncodeTextWithNodeSubTypeHelperW(const aValue: String): String;
function ALJsonEncodeBinaryWithNodeSubTypeHelperW(const aValue: String): String;
function ALJsonEncodeObjectIDWithNodeSubTypeHelperW(const aValue: String): String;
function ALJsonEncodeBooleanWithNodeSubTypeHelperW(const aValue: Boolean): String;
function ALJsonEncodeDateTimeWithNodeSubTypeHelperW(const aValue: TdateTime): String;
function ALJsonEncodeJavascriptWithNodeSubTypeHelperW(const aValue: String): String;
function ALJsonEncodeInt64WithNodeSubTypeHelperW(const aValue: int64): String;
function ALJsonEncodeInt32WithNodeSubTypeHelperW(const aValue: int32): String;
function ALJsonEncodeNullWithNodeSubTypeHelperW: String;
function ALJsonEncodeWithNodeSubTypeHelperW(
           const aValue: String;
           const aNodeSubType: TALJSONNodeSubType;
           const aFormatSettings: TALFormatSettingsW): String;

function ALJSONTryStrToRegExW(const S: String; out RegEx: String; out RegExOptions: TALPerlRegExOptions): boolean;
function ALJSONTryStrToBinaryW(const S: String; out Data: String; out Subtype: byte): boolean; // return a base64 encoded string
function ALJSONTryStrToDateTimeW(const S: String; out Value: TDateTime): Boolean;
function ALJSONTryStrToObjectIDW(const S: String; out Value: String): Boolean; // return a hex string
function ALJSONTryStrToTimestampW(const S: String; out Value: TALBSONTimestamp): Boolean;
function ALJSONTryStrToInt32W(const S: String; out Value: integer): Boolean;
function ALJSONTryStrToInt64W(const S: String; out Value: int64): Boolean;

Function ALFindJsonNodeByInt32ChildNodeValueW(
           const JsonNode:TALJSONNodeW;
           Const ChildNodeName: String;
           Const ChildNodeValue : Int32;
           Const Recurse: Boolean = False): TALJSONNodeW;
Function ALFindJsonNodeByTextChildNodeValueW(
           const JsonNode:TALJSONNodeW;
           Const ChildNodeName: String;
           Const ChildNodeValue : String;
           Const Recurse: Boolean = False): TALJSONNodeW;

implementation

uses
  System.Math,
  System.Generics.Collections,
  system.IOUtils,
  System.DateUtils,
  System.AnsiStrings,
  Alcinoe.QuickSortList,
  Alcinoe.HTML,
  Alcinoe.Common;

{********************************************}
Function ALFindJsonNodeByInt32ChildNodeValueA(
           const JsonNode:TALJSONNodeA;
           Const ChildNodeName: AnsiString;
           Const ChildNodeValue : Int32;
           Const Recurse: Boolean = False): TALJSONNodeA;
var I, J : integer;
Begin
  result := nil;
  if not (JsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for I := 0 to JsonNode.ChildNodes.Count - 1 do begin
    for J := 0 to JsonNode.ChildNodes[I].ChildNodes.Count - 1 do begin
      If (JsonNode.ChildNodes[I].ChildNodes[j].NodeType = nttext) and
         (JsonNode.ChildNodes[I].ChildNodes[j].NodesubType = nstint32) and
         (ALSameTextA(JsonNode.ChildNodes[I].ChildNodes[j].NodeName, ChildNodeName)) and
         (JsonNode.ChildNodes[I].ChildNodes[j].int32 = ChildNodeValue) then begin
        result := JsonNode.ChildNodes[I];
        exit;
      end;
    end;
    if Recurse then begin
      result := ALFindJsonNodeByInt32ChildNodeValueA(
                  JsonNode.ChildNodes[I],
                  ChildNodeName,
                  ChildNodeValue,
                  Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{*******************************************}
Function ALFindJsonNodeByTextChildNodeValueA(
           const JsonNode:TALJSONNodeA;
           Const ChildNodeName: AnsiString;
           Const ChildNodeValue : AnsiString;
           Const Recurse: Boolean = False): TALJSONNodeA;
var I, J : integer;
Begin
  result := nil;
  if not (JsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for I := 0 to JsonNode.ChildNodes.Count - 1 do begin
    for J := 0 to JsonNode.ChildNodes[I].ChildNodes.Count - 1 do begin
      If (JsonNode.ChildNodes[I].ChildNodes[j].NodeType = nttext) and
         (JsonNode.ChildNodes[I].ChildNodes[j].NodesubType = nstText) and
         (ALSameTextA(JsonNode.ChildNodes[I].ChildNodes[j].NodeName, ChildNodeName)) and
         (JsonNode.ChildNodes[I].ChildNodes[j].text = ChildNodeValue) then begin
        result := JsonNode.ChildNodes[I];
        exit;
      end;
    end;
    if Recurse then begin
      result := ALFindJsonNodeByTextChildNodeValueA(
                  JsonNode.ChildNodes[I],
                  ChildNodeName,
                  ChildNodeValue,
                  Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{************************************************************************************************************************}
function ALJSONTryStrToRegExA(const S: AnsiString; out RegEx: AnsiString; out RegExOptions: TALPerlRegExOptions): boolean;
var P1: integer;
    I: integer;
begin

  // regular expression in JSON must look like: /pattern/options
  // list of valid options is:
  //  'i' for case insensitive matching,
  //  'm' for multiline matching,
  //  'x' for verbose mode,
  //  'l' to make \w, \W, etc. locale dependent,
  //  's' for dotall mode ('.' matches everything),
  //  'u' to make \w, \W, etc. match unicode.
  result := false;

  // check that first character is /
  if (S <> '') and (S[1] = '/') then begin

    P1 := ALLastDelimiterA('/', S);
    if P1 <> 1 then begin

      //init Value
      RegEx := ALCopyStr(S, 2, P1 - 2);
      RegExOptions := [];

      // loop on all the options characters
      // to check if they are allowed.
      for I := P1 + 1 to Length(S) do
        case s[I] of
          'i': RegExOptions := RegExOptions + [preCaseLess];
          'm': RegExOptions := RegExOptions + [preMultiLine];
          'x': RegExOptions := RegExOptions + [preExtended];
          'l':;
          's': RegExOptions := RegExOptions + [preSingleLine];
          'u':;
          else exit;
        end;

      //set the result to true
      result := true;

      // check if it's compiling
      //aRegEx := TALPerlRegEx.Create;
      //try
      //  aRegEx.RegEx := Value.Expression;
      //  result := aRegEx.Compile(false{RaiseException});
      //finally
      //  ALFreeAndNil(aRegEx);
      //end;

    end;

  end;

end;

{****************************************************************************************************}
function ALJSONTryStrToBinaryA(const S: AnsiString; out Data: AnsiString; out Subtype: byte): boolean;
var LInt: integer;
    Ln: integer;
    P1, P2: integer;
begin

  // s must look like
  // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // BinData ( 0 , "JliB6gIMRuSphAD2KmhzgQ==" )
  result := false;
  Ln := length(s);
  P1 := 1;

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

  P2 := P1;
  while (P2 <= ln) and (S[P2] in ['0'..'9']) do inc(P2); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                                         //           ^P2
  if P2 > ln then exit;
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
  Data := ALBase64DecodeString(ALCopyStr(s, P1, P2-P1));

  // set the result
  result := true;

end;

{***********************************************************************************}
function ALJSONTryStrToDateTimeA(const S: AnsiString; out Value: TDateTime): Boolean;
var LQuoteChar: ansiChar;
    LTmpStr: AnsiString;
    LTmpLn: integer;
    P1, P2: integer;
    Ln: integer;
begin

  // s must look like
  // new Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // new ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  // ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  result := false;
  Ln := length(s);
  if ALPosA('new', s) = 1 then P1 := 4{length('new') + 1} // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                         //    ^P1
  else P1 := 1;// Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
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
  LQuoteChar := S[P1]; // "
  inc(p1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
           //              ^P1
  P2 := P1;
  while (P1 <= ln) and (S[P1] <> LQuoteChar) do inc(P1);
  if (P1 > ln) then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                          //                                      ^P1
  dec(P1);
  if S[P1] <> 'Z' then exit;
  LTmpStr := AlcopyStr(S,P2,P1-P2); // yyyy-mm-ddThh:nn:ss.zzz

  P2 := 1;
  LTmpLn := length(LTmpStr);
  while (P2 <= LTmpLn) and (LTmpStr[P2] <> 'T') do inc(P2);
  if P2 > LTmpLn then exit;
  LTmpStr[P2] := ' '; // yyyy-mm-dd hh:nn:ss.zzz

  result := ALTryStrToDateTime(LTmpStr, Value, ALJsonISODateFormatSettingsA);
  if not result then exit;

  inc(p1,2);  // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
              //                                       ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> ln) or (S[P1] <> ')') then begin
    result := false;
    exit;
  end;

end;

{****************************************************}
// ObjectId is a 12-byte BSON type, constructed using:
// a 4-byte value representing the seconds since the Unix epoch,
// a 3-byte machine identifier,
// a 2-byte process id, and
// a 3-byte counter, starting with a random value.
function ALJSONTryStrToObjectIDA(const S: AnsiString; out Value: AnsiString): Boolean;
var LObjectIDhex: AnsiString;
    LQuoteChar: ansiChar;
    P1: integer;
    Ln: integer;
begin

  // s must look like
  // ObjectId ( "507f1f77bcf86cd799439011" )
  result := false;
  if ALPosA('ObjectId', S) <> 1 then exit;
  Ln := length(s);
  P1 := 9{length('ObjectId') + 1}; // ObjectId ( "507f1f77bcf86cd799439011" )
                                   //         ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                            //          ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //           ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (not (S[P1] in ['''','"'])) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                         //            ^P1
  LQuoteChar := S[P1]; // "
  inc(p1); // ObjectId ( "507f1f77bcf86cd799439011" )
           //             ^P1
  if (P1 + 23{(length(aObjectIDhex)) - 1} > ln) then exit;
  LObjectIDhex := ALcopyStr(S,P1,24{length(aObjectIDhex)}); // 507f1f77bcf86cd799439011
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
  result := ALTryHexToBin(LObjectIDhex, Value) and
            (length(Value) = 12 {div Sizeof(ansichar)});

end;

{*******************************************************************************************}
function ALJSONTryStrToTimestampA(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
var P1, P2: integer;
    LArgs: AnsiString;
    LArg1: integer;
    LArg2: integer;
    Ln: integer;
begin

  // s must look like
  // Timestamp(0, 0)
  result        := false;
  if ALPosA('Timestamp', S) <> 1 then Exit;
  Ln := length(s);
  P1 := 10{Length('Timestamp') + 1}; // Timestamp(0, 0)
                                     //          ^
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // Timestamp(0, 0)
                                            //          ^P1
  P2 := ALPosA(')', S, P1);
  if P2 <> ln then exit; // Timestamp(0, 0)
                         //               ^P2
  LArgs := ALCopyStr(S, P1+1, P2 - P1-1); // 0, 0

  // take arguments of function Timestamp
  P1 := ALPosA(',', LArgs);
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, 1,      P1 - 1)), LArg1) then Exit;
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, P1 + 1, maxint)), LArg2) then Exit;

  // build result
  result := true;
  Value.W1 := LArg1; // higher 4 bytes - increment
  Value.W2 := LArg2; // lower  4 bytes - timestamp
end;

{******************************************************************************}
function ALJSONTryStrToInt32A(const S: AnsiString; out Value: integer): Boolean;
var LTmpStr: AnsiString;
    LQuoteChar: ansiChar;
    P1, P2: integer;
    Ln: integer;
begin

  // s must look like
  // NumberInt ( "12391293" )
  // NumberInt ( 12391293 )
  // 12391293
  result := ALTryStrToInt(S, Value);
  if result then exit;
  if ALPosA('NumberInt', S) <> 1 then exit;
  Ln := length(s);
  P1 := 10{length('NumberInt') + 1}; // NumberInt ( "12391293" )
                                     //          ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // NumberInt ( "12391293" )
                                            //           ^P1
  inc(p1);  // NumberInt ( "12391293" )
            //            ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) then exit
  else if (not (S[P1] in ['''','"'])) then begin // NumberInt ( 12391293 )
                                                 //             ^P1
    P2 := P1+1;
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

    LQuoteChar := S[P1]; // "
    inc(p1); // NumberInt ( "12391293" )
             //              ^P1
    P2 := P1;
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

  //convert 12391293 to integer
  result := ALTryStrToInt(LTmpStr, Value);

end;

{****************************************************************************}
function ALJSONTryStrToInt64A(const S: AnsiString; out Value: int64): Boolean;
var LTmpStr: AnsiString;
    LQuoteChar: ansiChar;
    P1, P2: integer;
    Ln: integer;
begin

  // s must look like
  // NumberLong ( "12391293" )
  // NumberLong ( 12391293 )
  // 12391293
  result := ALTryStrToInt64(S, Value);
  if result then exit;
  if ALPosA('NumberLong', S) <> 1 then exit;
  Ln := length(s);
  P1 := 11{length('NumberLong') + 1}; // NumberLong ( "12391293" )
                                      //           ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // NumberLong ( "12391293" )
                                            //            ^P1
  inc(p1);  // NumberLong ( "12391293" )
            //             ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) then exit
  else if (not (S[P1] in ['''','"'])) then begin // NumberLong ( 12391293 )
                                                 //              ^P1
    P2 := P1+1;
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

    LQuoteChar := S[P1]; // "
    inc(p1); // NumberLong ( "12391293" )
             //               ^P1
    P2 := P1;
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

  //convert 12391293 to integer
  result := ALTryStrToInt64(LTmpStr, Value);

end;

{*****************************************************}
procedure AlJSONDocErrorA(const Msg: String); overload;
begin
  raise EALJSONDocError.Create(Msg);
end;

{*********************************************************************************}
procedure AlJSONDocErrorA(const Msg: String; const Args: array of const); overload;
begin
  raise EALJSONDocError.CreateFmt(Msg, Args);
end;

{**************************************************************************************}
procedure AlJSONDocErrorA(const Msg: String; const NodeType: TalJsonNodeType); overload;
begin
  case NodeType of
    ntObject: AlJSONDocErrorA(Msg, ['ntObject']);
    ntArray: AlJSONDocErrorA(Msg, ['ntArray']);
    ntText: AlJSONDocErrorA(Msg, ['ntText']);
    else AlJSONDocErrorA(cAlJSONInvalidNodeType);
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
    else begin
      Result := nil; //for hide warning
      AlJSONDocErrorA(cAlJSONInvalidNodeType);
    end;
  end;
end;

{*****************************************************}
class function TALJSONDocumentA.DetectNodeTypeFromJSON(
                 const RawJSONStream: TStream;
                 const RawJSONString: AnsiString): TALJSONNodeType;

Const BufferSize: integer = 8192;

Var Buffer: AnsiString;
    BufferLength: Integer;
    BufferPos: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer: boolean;
  Var ByteReaded, Byte2Read: Integer;
  Begin
    if not assigned(RawJSONStream) then begin
      result := false;
      exit;
    end;

    If (BufferLength > 0) and (BufferPos > 1) then begin
      if (BufferPos > BufferLength) then RawJSONStream.Position := RawJSONStream.Position - BufferLength + BufferPos - 1;
      Byte2Read := min(BufferPos - 1, BufferLength);
      if BufferPos <= length(Buffer) then ALMove(
                                            Pbyte(Buffer)[BufferPos - 1],
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
    if RawJSONStream.Position < RawJSONStream.Size then ByteReaded := RawJSONStream.Read(Pbyte(Buffer)[BufferLength - Byte2Read{+ 1 - 1}],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      BufferLength := BufferLength - Byte2Read + ByteReaded;
      SetLength(Buffer, BufferLength);
      Result := ByteReaded > 0;
    end
    else result := True;
  end;

var InitialStreamPosition: int64;
    BOMSequence: integer;
    c: ansiChar;

Begin

  //--
  result := ntText;

  //--
  if assigned(RawJSONStream) then begin
    InitialStreamPosition := RawJSONStream.Position;
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
  BOMSequence := 0; // hide warnings
  While (BufferPos <= BufferLength) or ExpandBuffer do begin
    c := Buffer[BufferPos];
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
      if c = '{' then result := ntObject
      else if c = '[' then result := ntarray
      else result := ntText;
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
  result := ALCreateJSONNodeA('', ntObject);
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromJSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  var LNodeType := DetectNodeTypeFromJSON(nil, Str);
  if LNodeType in [ntObject, ntArray] then result := ALCreateJSONNodeA('', LNodeType)
  else AlJSONDocErrorA(cALJSONParseError);
  try
    result.LoadFromJSONString(Str, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  var LNodeType := DetectNodeTypeFromJSON(Stream, '');
  if LNodeType in [ntObject, ntArray] then result := ALCreateJSONNodeA('', LNodeType)
  else AlJSONDocErrorA(cALJSONParseError);
  try
    result.LoadFromJSONStream(Stream, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{*************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    Result := CreateFromJSONStream(LfileStream, Options);
  finally
    ALFreeAndNil(LfileStream);
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
  result := ALCreateJSONNodeA('', ntObject);
  try
    result.LoadFromBSONString(Str, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
begin
  result := ALCreateJSONNodeA('', ntObject);
  try
    result.LoadFromBSONStream(Stream, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{*************************************************************************************************************************************************}
class function TALJSONDocumentA.CreateFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeA;
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    result := CreateFromBSONStream(LfileStream, Options);
  finally
    ALFreeAndNil(LfileStream);
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
                  const onParseText: TAlJSONParseTextEventA;
                  const onParseStartObject: TAlJSONParseObjectEventA;
                  const onParseEndObject: TAlJSONParseObjectEventA;
                  const onParseStartArray: TAlJSONParseArrayEventA;
                  const onParseEndArray: TAlJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  var LJsonNode: TALJsonNodeA;
  var LNodeType := DetectNodeTypeFromJSON(nil, Str);
  if LNodeType in [ntObject, ntArray] then LJsonNode := ALCreateJSONNodeA('', LNodeType)
  else AlJSONDocErrorA(cALJSONParseError);
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
                  const onParseText: TAlJSONParseTextEventA;
                  const onParseStartObject: TAlJSONParseObjectEventA;
                  const onParseEndObject: TAlJSONParseObjectEventA;
                  const onParseStartArray: TAlJSONParseArrayEventA;
                  const onParseEndArray: TAlJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
begin
  var LJsonNode: TALJsonNodeA;
  var LNodeType := DetectNodeTypeFromJSON(Stream, '');
  if LNodeType in [ntObject, ntArray] then LJsonNode := ALCreateJSONNodeA('', LNodeType)
  else AlJSONDocErrorA(cALJSONParseError);
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
                  const onParseText: TAlJSONParseTextEventA;
                  const onParseStartObject: TAlJSONParseObjectEventA;
                  const onParseEndObject: TAlJSONParseObjectEventA;
                  const onParseStartArray: TAlJSONParseArrayEventA;
                  const onParseEndArray: TAlJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseJSONStream(
      LfileStream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{*********************************************}
class procedure TALJSONDocumentA.ParseJSONFile(
                  const FileName: AnsiString;
                  const onParseText: TAlJSONParseTextEventA;
                  const onParseStartObject: TAlJSONParseObjectEventA;
                  const onParseEndObject: TAlJSONParseObjectEventA;
                  const onParseStartArray: TAlJSONParseArrayEventA;
                  const onParseEndArray: TAlJSONParseArrayEventA;
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
                  const onParseText: TAlJSONParseTextEventA;
                  const onParseStartObject: TAlJSONParseObjectEventA;
                  const onParseEndObject: TAlJSONParseObjectEventA;
                  const onParseStartArray: TAlJSONParseArrayEventA;
                  const onParseEndArray: TAlJSONParseArrayEventA;
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
                  const onParseText: TAlJSONParseTextEventA;
                  const onParseStartObject: TAlJSONParseObjectEventA;
                  const onParseEndObject: TAlJSONParseObjectEventA;
                  const onParseStartArray: TAlJSONParseArrayEventA;
                  const onParseEndArray: TAlJSONParseArrayEventA;
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
                  const onParseText: TAlJSONParseTextEventA;
                  const onParseStartObject: TAlJSONParseObjectEventA;
                  const onParseEndObject: TAlJSONParseObjectEventA;
                  const onParseStartArray: TAlJSONParseArrayEventA;
                  const onParseEndArray: TAlJSONParseArrayEventA;
                  const Options: TALJSONParseOptions = []);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseBSONStream(
      LfileStream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{*********************************************}
class procedure TALJSONDocumentA.ParseBSONFile(
                  const FileName: AnsiString;
                  const onParseText: TAlJSONParseTextEventA;
                  const onParseStartObject: TAlJSONParseObjectEventA;
                  const onParseEndObject: TAlJSONParseObjectEventA;
                  const onParseStartArray: TAlJSONParseArrayEventA;
                  const onParseEndArray: TAlJSONParseArrayEventA;
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
  result := TALJSONNodeListA.Create(Self);
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONNodeA.InternalGetChildNodes: TALJSONNodeListA;
begin
  Result := nil; //virtual;
end;

{****************************************************}
function TALJSONNodeA.GetChildNodes: TALJSONNodeListA;
begin
  Result := nil; // hide warning
  AlJSONDocErrorA(CALJsonOperationError,GetNodeType)
end;

{******************************************************************}
procedure TALJSONNodeA.SetChildNodes(const Value: TALJSONNodeListA);
begin
  AlJSONDocErrorA(CALJsonOperationError,GetNodeType)
end;

{***************************************************************************}
function TALJSONNodeA.GetChildNode(const nodeName: ansiString): TALJSONNodeA;
begin
  result := ChildNodes.findNode(nodeName);
end;

{*************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueText(const nodeName: ansiString; const default: AnsiString): AnsiString;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetText(default);
end;

{******************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueFloat(const nodeName: ansiString; const default: Double): Double;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetFloat(default);
end;

{***************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueDateTime(const nodeName: ansiString; const default: TDateTime): TDateTime;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetDateTime(default);
end;

{******************************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueTimestamp(const nodeName: ansiString; const default: TALBSONTimestamp): TALBSONTimestamp;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetTimestamp(default);
end;

{********************************************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueObjectID(const nodeName: ansiString; const default: AnsiString): AnsiString;  // return a "byte" string
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetObjectID(default);
end;

{********************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueInt32(const nodeName: ansiString; const default: Integer): Integer;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetInt32(default);
end;

{****************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueInt64(const nodeName: ansiString; const default: Int64): Int64;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetInt64(default);
end;

{*******************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueBool(const nodeName: ansiString; const default: Boolean): Boolean;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBool(default);
end;

{*******************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueJavascript(const nodeName: ansiString; const default: AnsiString): AnsiString;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetJavascript(default);
end;

{**************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueRegEx(const nodeName: ansiString; const default: ansiString): ansiString;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetRegEx(default);
end;

{***************************************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueRegExOptions(const nodeName: ansiString; const default: TALPerlRegExOptions): TALPerlRegExOptions;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetRegExOptions(default);
end;

{******************************************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueBinary(const nodeName: ansiString; const default: AnsiString): AnsiString;  // return a "byte" string
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBinary(default);
end;

{**********************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueBinarySubType(const nodeName: ansiString; const default: byte): byte;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBinarySubType(default);
end;

{*******************************************************************************}
function TALJSONNodeA.GetChildNodeValueNull(const nodeName: ansiString): Boolean;
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := true
  else result := LNode.GetNull;
end;

{********************************************************************************}
function TALJSONNodeA.GetChildNode(const path: array of ansiString): TALJSONNodeA;
var I: integer;
begin
  result := Self;
  for I := low(path) to high(path) do begin
    result := result.ChildNodes.findNode(path[I]);
    if (result = nil) then exit;
  end;
end;

{******************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueText(const path: array of ansiString; const default: AnsiString): AnsiString;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetText(default);
end;

{***********************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueFloat(const path: array of ansiString; const default: Double): Double;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetFloat(default);
end;

{********************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueDateTime(const path: array of ansiString; const default: TDateTime): TDateTime;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetDateTime(default);
end;

{***********************************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueTimestamp(const path: array of ansiString; const default: TALBSONTimestamp): TALBSONTimestamp;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetTimestamp(default);
end;

{*************************************************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueObjectID(const path: array of ansiString; const default: AnsiString): AnsiString;  // return a "byte" string
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetObjectID(default);
end;

{*************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueInt32(const path: array of ansiString; const default: Integer): Integer;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetInt32(default);
end;

{*********************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueInt64(const path: array of ansiString; const default: Int64): Int64;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetInt64(default);
end;

{************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueBool(const path: array of ansiString; const default: Boolean): Boolean;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetBool(default);
end;

{************************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueJavascript(const path: array of ansiString; const default: AnsiString): AnsiString;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetJavascript(default);
end;

{*******************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueRegEx(const path: array of ansiString; const default: ansiString): ansiString;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetRegEx(default);
end;

{********************************************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueRegExOptions(const path: array of ansiString; const default: TALPerlRegExOptions): TALPerlRegExOptions;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetRegExOptions(default);
end;

{***********************************************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueBinary(const path: array of ansiString; const default: AnsiString): AnsiString;  // return a "byte" string
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetBinary(default);
end;

{***************************************************************************************************************}
function TALJSONNodeA.GetChildNodeValueBinarySubType(const path: array of ansiString; const default: byte): byte;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetBinarySubType(default);
end;

{************************************************************************************}
function TALJSONNodeA.GetChildNodeValueNull(const path: array of ansiString): Boolean;
var LNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := True;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := true
  else result := LNode.GetNull;
end;

{************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueText(const nodeName: ansiString; const value: AnsiString);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetText(value)
  else LNode.SetText(value);
end;

{*********************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueFloat(const nodeName: ansiString; const value: Double);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetFloat(value)
  else LNode.SetFloat(value);
end;

{***************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueDateTime(const nodeName: ansiString; const value: TDateTime);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetDateTime(value)
  else LNode.SetDateTime(value);
end;

{***********************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueTimestamp(const nodeName: ansiString; const value: TALBSONTimestamp);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetTimestamp(value)
  else LNode.SetTimestamp(value);
end;

{****************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueObjectID(const nodeName: ansiString; const value: AnsiString);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetObjectID(value)
  else LNode.SetObjectID(value);
end;

{**********************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueInt32(const nodeName: ansiString; const value: Integer);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt32(value)
  else LNode.SetInt32(value);
end;

{********************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueInt64(const nodeName: ansiString; const value: Int64);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt64(value)
  else LNode.SetInt64(value);
end;

{*********************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueBool(const nodeName: ansiString; const value: Boolean);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBool(value)
  else LNode.SetBool(value);
end;

{******************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueJavascript(const nodeName: ansiString; const value: AnsiString);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetJavascript(value)
  else LNode.SetJavascript(value);
end;

{*************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueRegEx(const nodeName: ansiString; const value: ansiString);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegEx(value)
  else LNode.SetRegEx(value);
end;

{*****************************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueRegExOptions(const nodeName: ansiString; const value: TALPerlRegExOptions);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegExOptions(value)
  else LNode.SetRegExOptions(value);
end;

{**************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueBinary(const nodeName: ansiString; const value: AnsiString);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinary(value)
  else LNode.SetBinary(value);
end;

{***************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueBinarySubType(const nodeName: ansiString; const value: byte);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinarySubType(value)
  else LNode.SetBinarySubType(value);
end;

{***********************************************************************}
procedure TALJSONNodeA.SetChildNodeValueNull(const nodeName: ansiString);
var LNode: TALJSONNodeA;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetNull(true)
  else LNode.SetNull(true);
end;

{*****************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueText(const path: array of ansiString; const value: AnsiString);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetText(value)
  else LTmpNode.SetText(value);
end;

{**************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueFloat(const path: array of ansiString; const value: Double);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetFloat(value)
  else LTmpNode.SetFloat(value);
end;

{********************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueDateTime(const path: array of ansiString; const value: TDateTime);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetDateTime(value)
  else LTmpNode.SetDateTime(value);
end;

{****************************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueTimestamp(const path: array of ansiString; const value: TALBSONTimestamp);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetTimestamp(value)
  else LTmpNode.SetTimestamp(value);
end;

{*********************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueObjectID(const path: array of ansiString; const value: AnsiString);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetObjectID(value)
  else LTmpNode.SetObjectID(value);
end;

{***************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueInt32(const path: array of ansiString; const value: Integer);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetInt32(value)
  else LTmpNode.SetInt32(value);
end;

{*************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueInt64(const path: array of ansiString; const value: Int64);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetInt64(value)
  else LTmpNode.SetInt64(value);
end;

{**************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueBool(const path: array of ansiString; const value: Boolean);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBool(value)
  else LTmpNode.SetBool(value);
end;

{***********************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueJavascript(const path: array of ansiString; const value: AnsiString);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetJavascript(value)
  else LTmpNode.SetJavascript(value);
end;

{******************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueRegEx(const path: array of ansiString; const value: ansiString);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetRegEx(value)
  else LTmpNode.SetRegEx(value);
end;

{**********************************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueRegExOptions(const path: array of ansiString; const value: TALPerlRegExOptions);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetRegExOptions(value)
  else LTmpNode.SetRegExOptions(value);
end;

{*******************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueBinary(const path: array of ansiString; const value: AnsiString);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinary(value)
  else LTmpNode.SetBinary(value);
end;

{********************************************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueBinarySubType(const path: array of ansiString; const value: byte);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinarySubType(value)
  else LTmpNode.SetBinarySubType(value);
end;

{****************************************************************************}
procedure TALJSONNodeA.SetChildNodeValueNull(const path: array of ansiString);
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetNull(true)
  else LTmpNode.SetNull(true);
end;

{***********************************************}
{Indicates whether this node has any child nodes}
function TALJSONNodeA.GetHasChildNodes: Boolean;
Var LNodeList: TALJSONNodeListA;
begin
  LNodeList := InternalGetChildNodes;
  Result := assigned(LNodeList) and (LNodeList.Count > 0);
end;

{************************************************}
function TALJSONNodeA.GetNodeValueStr: ansiString;
begin
  AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
  result := ''; // hide warning
end;

{*********************************************}
function TALJSONNodeA.GetNodeValueInt64: int64;
begin
  AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
  result := 0; // hide warning
end;

{**************************************************************************************************}
procedure TALJSONNodeA.SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType);
begin
  AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
end;

{*********************************************************************************************}
procedure TALJSONNodeA.SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
end;

{******************************************************************************************************************************}
procedure TALJSONNodeA.SetNodeValue(const StrValue: AnsiString; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
end;

{*************************************************************}
procedure TALJSONNodeA.SetNodeName(const NodeName: AnsiString);
begin
  if fNodeName <> NodeName then begin
    fNodeName := NodeName;
    Var LParentNode := FParentNode;
    if (LParentNode <> nil) and (LParentNode.ChildNodes.Sorted) then begin
      var LNode := LParentNode.ChildNodes.Extract(self);
      Try
        LParentNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      End;
    end;
  end;
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONNodeA.GetText: AnsiString;
begin

  case NodeSubType of
    nstFloat: result := GetNodeValueStr; // return the formated float
    nstText: result := GetNodeValueStr;  // return the raw text
    nstObject: result := GetNodeValueStr;  // return the raw objectID
    nstArray: result := GetNodeValueStr;  // error
    nstObjectID: result := GetNodeValueStr; // error
    nstBoolean: result := GetNodeValueStr;  // return true or false
    nstDateTime: result := GetNodeValueStr; // return the formated datetime
    nstNull: result := GetNodeValueStr; // return null
    nstRegEx: result := GetNodeValueStr; // return the raw regex (without the options)
    nstBinary: result := GetNodeValueStr; // return the raw binary (without the binary subtype)
    nstJavascript: result := GetNodeValueStr; // return the raw javascript
    nstInt32: result := GetNodeValueStr;  // return the number
    nstTimestamp: result := GetNodeValueStr;  // return the number (as int64)
    nstInt64: result := GetNodeValueStr;  // return the number
    else AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
  end;

end;

{*******************************************************************}
function TALJSONNodeA.GetText(const default: AnsiString): AnsiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetText;
end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONNodeA.SetText(const Value: AnsiString);
begin
  setNodeValue(Value, nstText);
end;

{******************************************************************************}
// By default json (ie: javascript) treats all numbers as floating-point values.
// To let other system (ie: mongoDB) understand the type of the number
// we provide the helper functions NumberLong() to handle 64-bit integers
// and NumberInt() to handle 32-bit integers (and some others). theses helper functions are
// used when saving the json document.
function TALJSONNodeA.GetNodeValueInterchange(const SkipNodeSubTypeHelper: boolean = False): AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetObjectID;
  begin
    if SkipNodeSubTypeHelper then result := '"'+ALBinToHexA(ObjectID)+'"'
    else result := 'ObjectId("'+ALBinToHexA(ObjectID)+'")';
  end;

  {~~~~~~~~~~~~~~~~~~~}
  procedure _GetBinary;
  begin
    if SkipNodeSubTypeHelper then result := '"'+ALBase64EncodeString(Binary)+'"'
    else result := 'BinData('+ALIntToStrA(BinarySubType)+', "'+ALBase64EncodeString(Binary)+'")';
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetDateTime;
  begin
    if SkipNodeSubTypeHelper then result := ALFormatDateTimeA('''"''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z"''', DateTime, ALDefaultFormatSettingsA)
    else result := ALFormatDateTimeA('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', DateTime, ALDefaultFormatSettingsA)
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _Getint32;
  begin
    if SkipNodeSubTypeHelper then result := text
    else result := 'NumberInt(' + text + ')'
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _Getint64;
  begin
    if SkipNodeSubTypeHelper then result := text
    else result := 'NumberLong(' + text + ')';
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _GetRegEx;
  var LRegExOptions: TALPerlRegExOptions;
      LRegExOptionsStr: ansiString;
  begin
    LRegExOptionsStr := '';
    LRegExOptions := RegExOptions;
    if preCaseLess in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 'i';
    if preMultiLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'m';
    if preExtended in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'x';
    //'l':;
    if preSingleLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 's';
    //'u':;
    result := '/'+regex+'/' + LRegExOptionsStr;
    if not SkipNodeSubTypeHelper then result := '"' + ALJavascriptEncode(result) + '"';
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetTimestamp;
  begin
    if SkipNodeSubTypeHelper then result := '"Timestamp('+ALIntToStrA(GetTimeStamp.W1)+', '+ALIntToStrA(GetTimeStamp.W2)+')"'
    else result := 'Timestamp('+ALIntToStrA(GetTimeStamp.W1)+', '+ALIntToStrA(GetTimeStamp.W2)+')';
  end;

begin

  case NodeSubType of
    nstFloat:      result := GetNodeValueStr;
    nstText:       result := GetNodeValueStr;
    nstBinary:     _GetBinary;
    nstObjectID:   _GetObjectID;
    nstBoolean:    result := GetNodeValueStr;
    nstDateTime:   _GetDateTime;
    nstJavascript: result := GetNodeValueStr;
    nstInt32:      _Getint32;
    nstInt64:      _Getint64;
    nstNull:       result := GetNodeValueStr;
    nstObject:     result := GetNodeValueStr;
    nstArray:      result := GetNodeValueStr;
    nstRegEx:      _GetRegEx;
    nstTimestamp:  _GetTimestamp;
    else raise Exception.Create('Unknown Node SubType');
  end;

end;

{*************************************}
function TALJSONNodeA.GetFloat: Double;
begin
  case NodeSubType of
    nstFloat: PInt64(@result)^ := GetNodeValueInt64;
    nstInt32,
    nstInt64: Result := GetNodeValueInt64;
    else begin
      AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{************************************************************}
function TALJSONNodeA.GetFloat(const default: Double): Double;
begin
  if NodeSubType = nstNull then result := default
  else result := GetFloat;
end;

{***************************************************}
procedure TALJSONNodeA.SetFloat(const Value: Double);
begin
  setNodeValue(PInt64(@Value)^, nstFloat);
end;

{*******************************************}
function TALJSONNodeA.GetDateTime: TDateTime;
begin
  if NodeSubType = nstDateTime then PInt64(@result)^ := GetNodeValueInt64
  else begin
    AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
    result := 0; // to hide a warning;
  end;
end;

{*********************************************************************}
function TALJSONNodeA.GetDateTime(const default: TDateTime): TDateTime;
begin
  if NodeSubType = nstNull then result := default
  else result := GetDateTime;
end;

{*********************************************************}
procedure TALJSONNodeA.SetDateTime(const Value: TDateTime);
begin
  setNodeValue(PInt64(@Value)^, nstDateTime);
end;

{***************************************************}
function TALJSONNodeA.GetTimestamp: TALBSONTimestamp;
begin
  if NodeSubType = nstTimestamp then result.I64 := GetNodeValueInt64
  else begin
    AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
    result.I64 := 0; // to hide a warning;
  end;
end;

{************************************************************************************}
function TALJSONNodeA.GetTimestamp(const default: TALBSONTimestamp): TALBSONTimestamp;
begin
  if NodeSubType = nstNull then result := default
  else result := GetTimestamp;
end;

{*****************************************************************}
procedure TALJSONNodeA.SetTimestamp(const Value: TALBSONTimestamp);
begin
  setNodeValue(Value.I64, nstTimestamp);
end;

{********************************************}
function TALJSONNodeA.GetObjectID: ansiString;
begin
  if NodeSubType = nstObjectID then result := GetNodeValueStr
  else begin
    AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{***********************************************************************}
function TALJSONNodeA.GetObjectID(const default: AnsiString): AnsiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetObjectID;
end;

{**********************************************************}
procedure TALJSONNodeA.SetObjectID(const Value: AnsiString);
begin
  if length(Value) <> 12 {div sizeof(ansiChar)} then AlJSONDocErrorA('ObjectID must have 12 bytes');
  setNodeValue(Value, nstObjectID);
end;

{**************************************}
function TALJSONNodeA.GetInt32: Integer;
var LDouble: Double;
    LInt64: system.int64;
begin
  case NodeSubType of
    nstFloat: begin
                PInt64(@LDouble)^ := GetNodeValueInt64;
                LInt64 := trunc(LDouble);
                if (LInt64 <> LDouble) or // https://stackoverflow.com/questions/41779801/single-double-and-precision
                                          // Only values that are in form m*2^e, where m and e are integers can be stored in a floating point variable
                                          // so all integer can be store in the form m*2^e (ie: m = m*2^0)
                                          // so we can compare aInt64 <> aDouble without the need of samevalue
                   (LInt64 > system.int32.MaxValue) or
                   (LInt64 < system.int32.MinValue) then AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
                result := LInt64;
              end;
    nstInt32: begin
                LInt64 := GetNodeValueInt64;
                if (LInt64 > system.int32.MaxValue) or
                   (LInt64 < system.int32.MinValue) then AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
                result := LInt64;
              end;
    nstInt64: Result := GetNodeValueInt64;
    else begin
      AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{**************************************************************}
function TALJSONNodeA.GetInt32(const default: Integer): Integer;
begin
  if NodeSubType = nstNull then result := default
  else result := GetInt32;
end;

{****************************************************}
procedure TALJSONNodeA.SetInt32(const Value: Integer);
begin
  setNodeValue(Value, nstInt32);
end;

{************************************}
function TALJSONNodeA.GetInt64: Int64;
var LDouble: Double;
begin
  case NodeSubType of
    nstFloat: begin
                PInt64(@LDouble)^ := GetNodeValueInt64;
                result := trunc(LDouble);
                if result <> LDouble then AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType); // https://stackoverflow.com/questions/41779801/single-double-and-precision
                                                                                         // Only values that are in form m*2^e, where m and e are integers can be stored in a floating point variable
                                                                                         // so all integer can be store in the form m*2^e (ie: m = m*2^0)
                                                                                         // so we can compare result <> aDouble without the need of samevalue
              end;
    nstInt32,
    nstInt64: Result := GetNodeValueInt64;
    else begin
      AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{**********************************************************}
function TALJSONNodeA.GetInt64(const default: Int64): Int64;
begin
  if NodeSubType = nstNull then result := default
  else result := GetInt64;
end;

{**************************************************}
procedure TALJSONNodeA.SetInt64(const Value: Int64);
begin
  setNodeValue(Value, nstInt64);
end;

{*************************************}
function TALJSONNodeA.GetBool: Boolean;
begin
  if NodeSubType = nstBoolean then begin
    if GetNodeValueInt64 = 0 then result := False
    else result := true;
  end
  else begin
    AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
    result := False; // to hide a warning;
  end;
end;

{*************************************************************}
function TALJSONNodeA.GetBool(const default: Boolean): Boolean;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBool;
end;

{***************************************************}
procedure TALJSONNodeA.SetBool(const Value: Boolean);
begin
  if Value then setNodeValue(1, nstBoolean)
  else setNodeValue(0, nstBoolean);
end;

{*************************************}
function TALJSONNodeA.GetNull: Boolean;
begin
  result := NodeSubType = nstNull;
end;

{***************************************************}
procedure TALJSONNodeA.SetNull(const Value: Boolean);
begin
  if Value then setNodeValue(0, nstNull)
  else AlJSONDocErrorA('Only "true" is allowed for setNull property');
end;

{**********************************************}
function TALJSONNodeA.GetJavascript: AnsiString;
begin
  if NodeSubType = nstJavascript then result := GetNodeValueStr
  else begin
    AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{*************************************************************************}
function TALJSONNodeA.GetJavascript(const default: AnsiString): AnsiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetJavascript;
end;

{************************************************************}
procedure TALJSONNodeA.SetJavascript(const Value: AnsiString);
begin
  setNodeValue(Value, nstJavascript);
end;

{*****************************************}
function TALJSONNodeA.GetRegEx: ansiString;
begin
  if NodeSubType = nstRegEx then result := GetNodeValueStr
  else begin
    AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{********************************************************************}
function TALJSONNodeA.GetRegEx(const default: ansiString): ansiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetRegEx;
end;

{*********************************************************}
procedure TALJSONNodeA.SetRegEx(const Pattern: ansiString);
begin
  setNodeValue(Pattern, 0, nstRegEx);
end;

{*********************************************************************************************}
procedure TALJSONNodeA.SetRegEx(const Pattern: ansiString; const Options: TALPerlRegExOptions);
begin
  setNodeValue(Pattern, byte(Options), nstRegEx);
end;

{*********************************************************}
function TALJSONNodeA.GetRegExOptions: TALPerlRegExOptions;
begin
  if NodeSubType = nstRegEx then result := TALPerlRegExOptions(byte(GetNodeValueInt64))
  else begin
    AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
    result := []; // to hide a warning;
  end;
end;

{*********************************************************************************************}
function TALJSONNodeA.GetRegExOptions(const default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  if NodeSubType = nstNull then result := default
  else result := GetRegExOptions;
end;

{***********************************************************************}
procedure TALJSONNodeA.SetRegExOptions(const Value: TALPerlRegExOptions);
begin
  if NodeSubType <> nstRegEx then AlJSONDocErrorA('You can set regex options only to a regex node');
  setNodeValue(byte(Value), nstRegEx);
end;

{******************************************}
function TALJSONNodeA.GetBinary: AnsiString;
begin
  if NodeSubType = nstBinary then result := GetNodeValueStr
  else begin
    AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{*********************************************************************}
function TALJSONNodeA.GetBinary(const default: AnsiString): AnsiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBinary;
end;

{*******************************************************}
procedure TALJSONNodeA.SetBinary(const Data: AnsiString);
begin
  setNodeValue(Data, 0, nstBinary); // 0 = Default BSON type
end;

{****************************************************************************}
procedure TALJSONNodeA.SetBinary(const Data: AnsiString; const Subtype: byte);
begin
  setNodeValue(Data, Subtype, nstBinary);
end;

{*******************************************}
function TALJSONNodeA.GetBinarySubType: byte;
begin
  if NodeSubType = nstBinary then result := byte(GetNodeValueInt64)
  else begin
    AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
    result := 0; // to hide a warning;
  end;
end;

{****************************************************************}
function TALJSONNodeA.GetBinarySubType(const default: byte): byte;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBinarySubType;
end;

{***********************************************************}
procedure TALJSONNodeA.SetBinarySubType(const Subtype: byte);
begin
  if NodeSubType <> nstBinary then AlJSONDocErrorA('You can set binary subtype only to a binary node');
  setNodeValue(Subtype, nstBinary);
end;

{************************}
{returns the parent node.}
function TALJSONNodeA.GetParentNode: TALJSONNodeA;
begin
  Result := FParentNode;
end;

{******************************************}
{Sets the value of the ParentNode property.}
procedure TALJSONNodeA.SetParentNode(const Value: TALJSONNodeA);
begin
  if FParentNode <> Value then
    FParentNode := Value;
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
Begin
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
Begin
  LoadFromBSONString(Value);
end;

{*****************************************************************}
{Returns the number of parents for this node in the node hierarchy.
 NestingLevel returns the number of ancestors for this node in the node hierarchy.}
function TALJSONNodeA.NestingLevel: Integer;
var PNode: TALJSONNodeA;
begin
  Result := 0;
  PNode := ParentNode;
  while PNode <> nil do begin
    Inc(Result);
    PNode := PNode.ParentNode;
  end;
end;

{**********************************************************}
constructor TALJSONNodeA.Create(const NodeName: AnsiString);
Begin
  FParentNode := nil;
  fNodeName := NodeName;
end;

{***************************************************************}
//will create all the nodevalue and childnodelist to be sure that
//multiple thread can safely read at the same time the node
procedure TALJSONNodeA.MultiThreadPrepare(const aOnlyChildList: Boolean = False);
var I: integer;
begin
  if (not aOnlyChildList) and (NodeType = ntText) then begin

    case NodeSubType of
      nstFloat,
      nstBoolean,
      nstDateTime,
      nstNull,
      nstInt32,
      nstTimestamp,
      nstInt64: GetNodeValueStr;
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
    end;

    case NodeSubType of
      nstFloat,
      nstBoolean,
      nstDateTime,
      nstNull,
      nstInt32,
      nstTimestamp,
      nstInt64: GetNodeValueInt64;
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
    end;

  end

  else if (NodeType in [ntObject,ntArray]) then begin
    For I := 0 to ChildNodes.Count - 1 do
      ChildNodes[I].MultiThreadPrepare(aOnlyChildList);
  end;
end;

{********************************************************************************************************************************************}
function TALJSONNodeA.AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA;
begin
  Result := ALCreateJSONNodeA(NodeName,NodeType);
  Try
    ChildNodes.Insert(Index, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{*************************************************************************************************************************************************}
function TALJSONNodeA.AddChild(const Path: array of AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA;
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I], TDirection.FromEnd);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  result := LNode.addChild(path[high(path)], NodeType, Index);
end;

{****************************************************************************************************************}
function TALJSONNodeA.AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeA;
begin
  Result := AddChild('', NodeType, Index);
end;

{*********************************************************************}
function TALJSONNodeA.DeleteChild(const NodeName: AnsiString): boolean;
var I: integer;
begin
  I := ChildNodes.IndexOf(NodeName);
  if I >= 0 then begin
    ChildNodes.Delete(I);
    result := True;
  end
  else result := False;
end;

{**************************************************************************}
function TALJSONNodeA.DeleteChild(const Path: array of AnsiString): boolean;
var LNode: TALJSONNodeA;
    LTmpNode: TALJSONNodeA;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then exit(false)
    else LNode := LTmpNode;
  end;
  I := LNode.ChildNodes.IndexOf(path[high(path)]);
  if I >= 0 then begin
    LNode.ChildNodes.Delete(I);
    result := True;
  end
  else result := False;
end;

{****************************************************************************************************}
function TALJSONNodeA.CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNodeA;
begin
  Result := ALCreateJSONNodeA(NodeName, NodeType);
end;

{********************************************}
{Returns the next child of this node’s parent.
 NextSibling returns the node that follows this one in the parent node’s ChildNodes property list.
 If this node is the last node in its parent’s child list, NextSibling raises an exception.}
function TALJSONNodeA.NextSibling: TALJSONNodeA;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, 1)
  else Result := nil;
end;

{************************************************}
{Returns the previous child of this node’s parent.
 PreviousSibling returns the node that precedes this one in the parent node’s ChildNodes property list.
 If this node is the first node in its parent’s child list, PreviousSibling raises an exception.}
function TALJSONNodeA.PreviousSibling: TALJSONNodeA;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, -1)
  else Result := nil;
end;

{**************}
{The JSON format
 There are just a few rules that you need to remember:
 *Objects are encapsulated within opening and closing brackets { } {
 *An empty object can be represented by { } {
 *Arrays are encapsulated within opening and closing square brackets [ ]
 *An empty array can be represented by [ ]
 *A member is represented by a key-value pair
 *The key of a member should be contained in double quotes. (JavaScript does not require this. JavaScript and some parsers will tolerate single-quotes)
 *Each member should have a unique key within an object structure
 *The value of a member must be contained in double quotes if it's a string (JavaScript and some parsers will tolerates single-quotes)
 *Boolean values are represented using the true or false literals in lower case
 *Number values are represented using double-precision floating-point format. Scientific notation is supported
 *Numbers should not have leading zeroes
 *"Offensive"" characters in a string need to be escaped using the backslash character
 *Null values are represented by the null literal in lower case
 *Other object types, such as dates, are not properly supported and should be converted to strings. It becomes the responsability of the parser/client to manage this.
 *Each member of an object or each array value must be followed by a comma if it's not the last one
 *The common extension for json files is '.json'
 *The mime type for json files is 'application/json'}
Procedure TALJSONNodeA.ParseJSON(
            const RawJSONStream: TStream;
            const RawJSONString: AnsiString;
            const SaxMode: Boolean;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions);

Const BufferSize: integer = 8192;

Var Buffer: AnsiString;
    BufferLength: Integer;
    BufferPos: Integer;
    CurrName: AnsiString;
    CurrIndex: integer;
    CurrValue: ansiString;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNodeA;
    NamePaths: TALNVStringListA;
    ObjectPaths: TALIntegerList;
    DecodeJSONReferences: boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer: boolean; overload;
  Var ByteReaded, Byte2Read: Integer;
  Begin
    if not assigned(RawJSONStream) then begin
      result := false;
      exit;
    end;

    If (BufferLength > 0) and (BufferPos > 1) then begin
      if (BufferPos > BufferLength) then RawJSONStream.Position := RawJSONStream.Position - BufferLength + BufferPos - 1;
      Byte2Read := min(BufferPos - 1, BufferLength);
      if BufferPos <= length(Buffer) then ALMove(
                                            Pbyte(Buffer)[BufferPos - 1],
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
    if RawJSONStream.Position < RawJSONStream.Size then ByteReaded := RawJSONStream.Read(Pbyte(Buffer)[BufferLength - Byte2Read{+ 1 - 1}],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      BufferLength := BufferLength - Byte2Read + ByteReaded;
      SetLength(Buffer, BufferLength);
      Result := ByteReaded > 0;
    end
    else result := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer(var PosToKeepSync: Integer): boolean; overload;
  var P1: integer;
  begin
    P1 := BufferPos;
    result := ExpandBuffer;
    PosToKeepSync := PosToKeepSync - (P1 - BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(Const ExtraItems: ansiString = ''): ansiString;
  var I, L, P, Size: Integer;
      LB: ansiChar;
      S: AnsiString;
  begin
    LB := ALDefaultJsonPathSeparatorA;
    Size := length(ExtraItems);
    if size <> 0 then Inc(Size, 1{length(LB)});
    for I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths.Names[I]) + 1{length(LB)});
    SetLength(Result, Size);
    P := 1;
    for I := 1 to NamePaths.Count - 1 do begin
      S := NamePaths.Names[I];
      L := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, Pbyte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
        Inc(P, L);
      end;
      L := 1{length(LB)};
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNodeA(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJSONNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, Pbyte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      L := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, Pbyte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
      Inc(P, L);
    end;
    setlength(result,P-1);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(
              const index: integer;
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
              const Index: integer;
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
        if NamePaths.Count = 0 then AlJSONDocErrorA(CALJSONParseError);
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
    if NamePaths.Count = 0 then AlJSONDocErrorA(CALJSONParseError);
    if Assigned(OnParseEndObject) then OnParseEndObject(Self, GetPathStr, NamePaths.Names[NamePaths.Count - 1])
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: AnsiString);
  begin
    if Assigned(OnParseStartArray) then OnParseStartArray(Self, GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if NamePaths.Count = 0 then AlJSONDocErrorA(CALJSONParseError);
    if Assigned(OnParseEndArray) then OnParseEndArray(Self, GetPathStr, NamePaths.Names[NamePaths.Count - 1]);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexItemToNamePath(const index: integer; Obj: Pointer);
  var S1: ansiString;
  begin
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
  procedure _AddItemToNamePath(index: integer; const name: AnsiString; Obj: Pointer);
  begin
    if notSaxMode then begin
      if WorkingNode.nodetype=ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end
    else begin
      if NamePaths.Count = 0 then AlJSONDocErrorA(CALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt64Node(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
      LInt64: System.Int64;
  begin
    if ALJSONTryStrToInt64A(value, LInt64) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetInt64(LInt64);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LInt64], nstInt64)
      end
      else begin
        _DoParseText(index, Name, [LInt64], nstInt64)
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt32Node(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
      LInt32: System.Int32;
  begin
    if ALJSONTryStrToInt32A(value, LInt32) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setint32(LInt32);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LInt32], nstInt32)
      end
      else begin
        _DoParseText(index, Name, [LInt32], nstInt32)
      end
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTextNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
  begin
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(index, Name, [value], nstText)
    end
    else begin
      _DoParseText(index, Name, [value], nstText)
    end
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createFloatNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
      LDouble: Double;
  begin
    if ALTryStrToFloat(value, LDouble, ALDefaultFormatSettingsA) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetFloat(LDouble);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LDouble], nstFloat)
      end
      else begin
        _DoParseText(index, Name, [LDouble], nstFloat)
      end
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBinaryNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
      LBinSubtype: byte;
      LBinData: ansiString;
  begin
    if ALJSONTryStrToBinaryA(value, LBinData, LBinSubtype) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.setbinary(LBinData, LBinSubtype);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LBinData, LBinSubtype], nstBinary);
      end
      else begin
        _DoParseText(index, Name, [LBinData, LBinSubtype], nstBinary);
      end
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createObjectIDNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
      LObjectID: AnsiString;
  begin
    if ALJSONTryStrToObjectIDA(value, LObjectID) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetObjectID(LObjectID);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LObjectID], nstObjectID)
      end
      else begin
        _DoParseText(index, Name, [LObjectID], nstObjectID)
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBooleanNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
      LBool: Boolean;
  begin
    if value = 'true' then LBool := true
    else if value = 'false' then LBool := false
    else begin
      result := False;
      exit;
    end;
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setbool(LBool);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(index, Name, [LBool], nstBoolean);
    end
    else begin
      _DoParseText(index, Name, [LBool], nstBoolean);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createDateTimeNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
      LDateTime: TdateTime;
  begin
    if ALJSONTryStrToDateTimeA(value, LDateTime) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setdatetime(LDateTime);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LDateTime], nstDateTime);
      end
      else begin
        _DoParseText(index, Name, [LDateTime], nstDateTime);
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTimestampNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
      LTimestamp: TALBSONTimestamp;
  begin
    if ALJSONTryStrToTimestampA(value, LTimestamp) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetTimestamp(LTimestamp);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LTimestamp.W1, LTimestamp.W2], nstTimeStamp);
      end
      else begin
        _DoParseText(index, Name, [LTimestamp.W1, LTimestamp.W2], nstTimeStamp);
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createnullNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
  begin
    if value = 'null' then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setnull(true);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, ['null'], nstNull);
      end
      else begin
        _DoParseText(index, Name, ['null'], nstNull);
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createRegExNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
      LRegEx: ansiString;
      LRegExOptions: TALPerlRegExOptions;
  begin
    if ALJSONTryStrToRegExA(value, LRegEx, LRegExOptions) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetRegEx(LRegEx, LRegExOptions);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LRegEx, Byte(LRegExOptions)], nstRegEx)
      end
      else begin
        _DoParseText(index, Name, [LRegEx, Byte(LRegExOptions)], nstRegEx)
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createJavascriptNode(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJSONNodeA;
  begin
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(index, Name, [value], nstJavascript);
    end
    else begin
      _DoParseText(index, Name, [value], nstJavascript);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createNode(index: integer; const name: AnsiString; const value: ansiString; AQuotedValue: Boolean);
  begin
    if AQuotedValue then begin
      _createTextNode(index, Name, Value);
      exit;
    end;
    if _createFloatNode(index, Name, Value) then exit;  // << we have the same problem as javascript, if we put here a big number like (by exemple) 9223372036854775808
                                                        // << then the stored value will be different because of double precision that is less than int64 precision
                                                        // << it's the way javascript json work, it's have no room for int / int64 :(
                                                        // << if we want to have the possibility to store int64 precision then we must use node subtype helper
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
  function _extractLastIndexFromNamePath: integer;
  begin
    if NamePaths.Count = 0 then AlJSONDocErrorA(CALJSONParseError);
    ALMove(pointer(namePaths.ValueFromIndex[namepaths.Count - 1])^,result,sizeOf(integer));
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var LNode: TALJSONNodeA;
      LNodeType: TALJSONNodeType;
      LQuoteChar: AnsiChar;
      LNameValueSeparator: ansiChar;
      LInSingleQuote: boolean;
      LInDoubleQuote: boolean;
      LInSlashQuote: boolean;
      LInSquareBracket: integer;
      LInRoundBracket: integer;
      LInCurlyBracket: integer;
      P1, P2: Integer;
      c: ansiChar;
  Begin

    {$REGION 'init current char (c)'}
    c := Buffer[BufferPos];
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
        if (ObjectPaths.Count = 0) then AlJSONDocErrorA(cALJSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then AlJSONDocErrorA(cALJSONParseError);
      end;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        if assigned(ObjectPaths) then LNode := TALJSONNodeA(ObjectPaths.Objects[ObjectPaths.Count - 1])
        else LNode := TALJSONNodeA(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (LNode <> WorkingNode) then AlJSONDocErrorA(CALJSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then AlJSONDocErrorA(cALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then AlJSONDocErrorA(CALJSONParseError);

        //if working node <> Self then we can go to one level up
        If WorkingNode <> Self then begin

          //init WorkingNode to the parentNode
          WorkingNode := WorkingNode.ParentNode;

          //update CurrIndex if WorkingNode.NodeType = ntArray
          if assigned(ObjectPaths) then begin
            if WorkingNode.NodeType = ntArray then CurrIndex := ObjectPaths[Objectpaths.Count - 1] + 1;
          end
          else begin
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
        if not (LNodeType in [ntObject,ntarray]) then AlJSONDocErrorA(cALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then AlJSONDocErrorA(CALJSONParseError);

        //update CurrIndex if WorkingNode.NodeType = ntArray
        if (Namepaths.Count >= 2) and
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 2]) = ntarray) then CurrIndex := _extractLastIndexFromNamePath + 1;

      end;

      //call the DoParseEndObject/array event
      if Assigned(OnParseEndObject) then begin
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

    {$REGION 'Begin Object/Array Without NAME'}
    // ... { ....
    // ... [ ....
    if c in ['{','['] then begin // ... { ...
                                 //     ^BufferPos

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then AlJSONDocErrorA(CALJSONParseError);

        //Node without name can be ONLY present inside an array node
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then AlJSONDocErrorA(CALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
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
        if assigned(ObjectPaths) then ObjectPaths.AddObject(CurrIndex, WorkingNode)
        else _AddItemToNamePath(CurrIndex, '', WorkingNode);

      end

      //if we are in sax mode
      else begin

          //Node without name can be ONLY present inside an array node
          if (CurrIndex < 0) or
             (NamePaths.Count = 0) or
             (TALJsonNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then AlJSONDocErrorA(CALJSONParseError);

        //update the path
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrIndex, '', pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event
      if c = '{' then begin
        if Assigned(OnParseStartObject) then _DoParseStartObject('');
        CurrIndex := -1;
      end
      else begin
        if Assigned(OnParseStartArray) then _DoParseStartArray('');
        CurrIndex := 0;
      end;

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
    LQuoteChar := #0;
    if c in ['"',''''] then begin  // ... " ...
                                   //     ^BufferPos
      LQuoteChar := c; // "
      P1 := BufferPos + 1; // ... "...\"..."
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
         if DecodeJSONReferences then ALJavascriptDecodeV(CurrName); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

       if P1 + 1 > BufferLength then ExpandBuffer(P1);

      end;
      if P1 > BufferLength then AlJSONDocErrorA(CALJSONParseError);
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

      LInSingleQuote := False;
      LInDoubleQuote := False;
      LInSquareBracket := 0;
      LInRoundBracket := 0;
      LInCurlyBracket := 0;

      While (BufferPos <= BufferLength) or ExpandBuffer do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then AlJSONDocErrorA(CALJSONParseError);

      P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
                       //     ^P1
      While (P1 <= BufferLength) or ExpandBuffer(P1) do begin

        c := Buffer[P1];

        if (not LInSingleQuote) and
           (not LInDoubleQuote) and
           (LInSquareBracket = 0) and
           (LInRoundBracket = 0) and
           (LInCurlyBracket = 0) and
           (c in [',', '}', ']', ':']) then begin
          P2 := P1-1;
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
          else if (c = '}') then inc(LInCurlyBracket)
          else if (c = '{') then dec(LInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then AlJSONDocErrorA(CALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos

    end;
    {$ENDREGION}

    {$REGION 'extract the name value separator part'}
    LNameValueSeparator := #0;
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      If Buffer[BufferPos] <= ' ' then inc(BufferPos)
      else begin
        LNameValueSeparator := Buffer[BufferPos];
        break;
      end;
    end;
    if BufferPos > BufferLength then AlJSONDocErrorA(CALJSONParseError);  // .... : ....
                                                                          //      ^BufferPos
    {$ENDREGION}

    {$REGION 'if aNameValueSeparator is absent then it is just a value'}
    if LNameValueSeparator <> ':' then begin

      //Node without name can be ONLY present inside an array node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then AlJSONDocErrorA(CALJSONParseError);
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then AlJSONDocErrorA(CALJSONParseError);
      end
      else begin
        if (CurrIndex < 0) or
           (NamePaths.Count = 0) or
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then AlJSONDocErrorA(CALJSONParseError);
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
    if BufferPos > BufferLength then AlJSONDocErrorA(CALJSONParseError); // .... " ....
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
        if not assigned(WorkingNode) then AlJSONDocErrorA(CALJSONParseError);

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0)  or
           (WorkingNode.nodetype <> ntObject) then AlJSONDocErrorA(CALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
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
        if assigned(ObjectPaths) then ObjectPaths.AddObject(-1, WorkingNode)
        else _AddItemToNamePath(-1, CurrName, WorkingNode);

      end

      //if we are in sax mode
      else begin

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0) or
           (NamePaths.Count = 0) or
           (TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]) <> ntobject) then AlJSONDocErrorA(CALJSONParseError);

        //update the path
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(-1, CurrName, pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event and update the CurrIndex if it's an array
      if c = '{' then begin
        if Assigned(OnParseStartObject) then _DoParseStartObject(CurrName)
      end
      else begin
        if Assigned(OnParseStartArray) then _DoParseStartArray(CurrName);
        CurrIndex := 0;
      end;

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
      P1 := BufferPos + 1; // ... "...\"..."
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
         if DecodeJSONReferences then ALJavascriptDecodeV(currValue); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

       if P1 + 1 > BufferLength then ExpandBuffer(P1);

      end;
      if P1 > BufferLength then AlJSONDocErrorA(CALJSONParseError);
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

      LInSingleQuote := False;
      LInDoubleQuote := False;
      LInSlashQuote := False;
      LInSquareBracket := 0;
      LInRoundBracket := 0;
      LInCurlyBracket := 0;

      While (BufferPos <= BufferLength) or ExpandBuffer do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then AlJSONDocErrorA(CALJSONParseError);

      P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
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
          P2 := P1-1;
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
             (Buffer[P1 - 1] <> '\') then LInSingleQuote := (not LInSingleQuote) and (not LInDoubleQuote) and (not LInSlashQuote)
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
          else if (c = '}') then inc(LInCurlyBracket)
          else if (c = '{') then dec(LInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then AlJSONDocErrorA(CALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos


    end;
    {$ENDREGION}

    {$REGION 'create the named text node'}

    //Node withe name MUST be ONLY present inside an object node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(CALJSONParseError);
      if (CurrIndex >= 0)  or
         (WorkingNode.nodetype <> ntObject) then AlJSONDocErrorA(CALJSONParseError);
    end
    else begin
      if (CurrIndex >= 0) or
         (NamePaths.Count = 0) or
         (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntObject) then AlJSONDocErrorA(CALJSONParseError);
    end;

    //create the node
    _createNode(currIndex,CurrName,CurrValue,LQuoteChar in ['"','''']);

    {$ENDREGION}

  end;

var BOMSequence: integer;
    InCommentLine: integer;
    c: ansiChar;

Begin

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
    ObjectPaths := TALIntegerList.Create(false{OwnsObjects});
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
    if assigned(ObjectPaths) then ObjectPaths.AddObject(-1, WorkingNode)
    else begin
      if NotSaxMode then _AddNameItemToNamePath('', WorkingNode)
      else _AddNameItemToNamePath('', pointer(NodeType));
    end;

    //skip the first {
    BOMSequence := 0; // hide warnings
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      c := Buffer[BufferPos];
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
          if (Nodetype <> ntObject) then AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
          CurrIndex := -1;
          _DoParseStartObject('');
        end
        else if (c = '[') then begin
          if (Nodetype <> ntArray) then AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
          CurrIndex := 0;
          _DoParseStartArray('');
        end
        else AlJSONDocErrorA(cALJSONParseError);
        inc(bufferPos);
        break;
      end;
    end;

    //analyze all the nodes
    if poAllowComments in Options then begin
      InCommentLine := 0;
      While (BufferPos <= BufferLength) or ExpandBuffer do begin
        c := Buffer[BufferPos];
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
        c := Buffer[BufferPos];
        If (c <= ' ') or (c = ',') then inc(bufferPos)
        else AnalyzeNode;
      end;
    end;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then AlJSONDocErrorA(cALJSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then AlJSONDocErrorA(cALJSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if NotSaxMode and (WorkingNode <> nil) then AlJSONDocErrorA(cALJSONParseError);

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
            const RawBSONString: AnsiString;
            const SaxMode: Boolean;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions);

Const BufferSize: integer = 8192;

Var Buffer: AnsiString;
    BufferLength: Integer;
    BufferPos: Integer;
    CurrName: AnsiString;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNodeA;
    NamePaths: TALStringListA;
    ObjectPaths: TObjectList<TALJSONNodeA>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer: boolean; overload;
  Var ByteReaded, Byte2Read: Integer;
  Begin
    if not assigned(RawBSONStream) then begin
      result := false;
      exit;
    end;

    If (BufferLength > 0) and (BufferPos > 1) then begin
      if (BufferPos > BufferLength) then RawBSONStream.Position := RawBSONStream.Position - BufferLength + BufferPos - 1;
      Byte2Read := min(BufferPos - 1, BufferLength);
      if BufferPos <= length(Buffer) then ALMove(
                                            Pbyte(Buffer)[BufferPos - 1],
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
    if RawBSONStream.Position < RawBSONStream.Size then ByteReaded := RawBSONStream.Read(Pbyte(Buffer)[BufferLength - Byte2Read{+ 1 - 1}],Byte2Read)
    else ByteReaded := 0;

    If ByteReaded <> Byte2Read then begin
      BufferLength := BufferLength - Byte2Read + ByteReaded;
      SetLength(Buffer, BufferLength);
      Result := ByteReaded > 0;
    end
    else result := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function ExpandBuffer(var PosToKeepSync: Integer): boolean; overload;
  var P1: integer;
  begin
    P1 := BufferPos;
    result := ExpandBuffer;
    PosToKeepSync := PosToKeepSync - (P1 - BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(Const ExtraItems: ansiString = ''): ansiString;
  var I, L, P, Size: Integer;
      LB: ansiChar;
      S: AnsiString;
  begin
    LB := ALDefaultJsonPathSeparatorA;
    Size := length(ExtraItems);
    if size <> 0 then Inc(Size, 1{length(LB)});
    for I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths[I]) + 1{length(LB)});
    SetLength(Result, Size);
    P := 1;
    for I := 1 to NamePaths.Count - 1 do begin
      S := NamePaths[I];
      L := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, Pbyte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
        Inc(P, L);
      end;
      L := 1{length(LB)};
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNodeA(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJSONNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, Pbyte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      L := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, Pbyte(Result)[(P-1){*sizeOf(ansiChar)}], L{*sizeOf(ansiChar)});
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
        if NamePaths.Count = 0 then AlJSONDocErrorA(CALJSONParseError);
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
    if NamePaths.Count = 0 then AlJSONDocErrorA(CALJSONParseError);
    if Assigned(OnParseEndObject) then OnParseEndObject(Self, GetPathStr, NamePaths[NamePaths.Count - 1])
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: AnsiString);
  begin
    if Assigned(OnParseStartArray) then OnParseStartArray(Self, GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if NamePaths.Count = 0 then AlJSONDocErrorA(CALJSONParseError);
    if Assigned(OnParseEndArray) then OnParseEndArray(Self, GetPathStr, NamePaths[NamePaths.Count - 1]);
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
      if NamePaths.Count = 0 then AlJSONDocErrorA(CALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(nameOrIndex, Obj)
      else _AddNameItemToNamePath(nameOrIndex, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt64Node(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LInt64: System.Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) + 1 then AlJSONDocErrorA(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt64, sizeof(LInt64));
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetInt64(LInt64);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LInt64], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LInt64], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt32Node(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LInt32: System.Int32;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) + 1 then AlJSONDocErrorA(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setint32(LInt32);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LInt32], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LInt32], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTextNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LInt32: System.Int32;
      LText: ansiString;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) + 1 then AlJSONDocErrorA(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    while (BufferPos + LInt32 - 1 > BufferLength) do
      if not ExpandBuffer then AlJSONDocErrorA(cALBSONParseError);
    ALCopyStr(Buffer,LText,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(LText);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LText], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LText], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createFloatNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LDouble: Double;
  begin
    if BufferPos > BufferLength - sizeof(Double) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(Double) + 1 then AlJSONDocErrorA(cALBSONParseError);
    end;
    ALMove(pbyte(Buffer)[BufferPos-1], LDouble, sizeof(Double));
    BufferPos := BufferPos + sizeof(Double);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetFloat(LDouble);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LDouble], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LDouble], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBinaryNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LInt32: System.Int32;
      LBinSubtype: byte;
      LBinData: ansiString;
  begin
    //Get size
    if BufferPos > BufferLength - sizeof(LInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) + 1 then AlJSONDocErrorA(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    //Get the subtype
    if BufferPos > BufferLength then begin
      ExpandBuffer;
      if BufferPos > BufferLength then AlJSONDocErrorA(cALBSONParseError);
    end;
    LBinSubtype := Byte(Buffer[BufferPos]);
    BufferPos := BufferPos + 1;

    //Get the data
    while (BufferPos + LInt32 - 1 > BufferLength) do
      if not ExpandBuffer then AlJSONDocErrorA(cALBSONParseError);
    ALCopyStr(Buffer,LBinData,BufferPos,LInt32);
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.setbinary(LBinData, LBinSubtype);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LBinData, LBinSubtype], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LBinData, LBinSubtype], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createObjectIDNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LObjectID: AnsiString;
  begin
    if BufferPos > BufferLength - 12{length(aObjectID)} + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - 12{length(aObjectID)} + 1 then AlJSONDocErrorA(cALBSONParseError);
    end;
    Setlength(LObjectID, 12); // ObjectId is a 12-byte BSON type
    ALMove(Pbyte(Buffer)[BufferPos-1], pbyte(LObjectID)[0], 12{length(aObjectID)}); // pbyte(aObjectID)[0] to not have a jump in uniqueString (aObjectID is already unique thanks to Setlength)
    BufferPos := BufferPos + 12{length(aObjectID)};

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetObjectID(LObjectID);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LObjectID], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LObjectID], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBooleanNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LBool: Boolean;
  begin
    if BufferPos > BufferLength then begin
      ExpandBuffer;
      if BufferPos > BufferLength then AlJSONDocErrorA(cALBSONParseError);
    end;
    if Buffer[BufferPos] = #$00 then LBool := False
    else if Buffer[BufferPos] = #$01 then LBool := true
    else begin
      AlJSONDocErrorA(cALBSONParseError);
      LBool := False; // to hide a warning;
    end;
    BufferPos := BufferPos + 1;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setbool(LBool);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LBool], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LBool], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createDateTimeNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LDateTime: TdateTime;
      LInt64: System.Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) + 1 then AlJSONDocErrorA(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt64, sizeof(LInt64));
    LDateTime := ALUnixMsToDateTime(LInt64);
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setdatetime(LDateTime);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LDateTime], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LDateTime], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTimestampNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LTimestamp: TALBSONTimestamp;
      LInt64: System.Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) + 1 then AlJSONDocErrorA(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt64, sizeof(LInt64));
    LTimestamp.I64 := LInt64;
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetTimestamp(LTimestamp);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LTimestamp.W1, LTimestamp.W2], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LTimestamp.W1, LTimestamp.W2], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createnullNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
  begin
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setnull(true);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, ['null'], NodeSubType);
    end
    else begin
      _DoParseText(Name, ['null'], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createRegExNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LRegEx: ansiString;
      LRegExOptions: TALPerlRegExOptions;
      P1: integer;
  begin
    //Get pattern
    P1 := BufferPos;
    While (P1 <= BufferLength) or ExpandBuffer(P1) do begin
      If Buffer[P1] <> #$00 then inc(P1)
      else begin
        LRegEx := AlCopyStr(Buffer, BufferPos, P1 - BufferPos);
        break;
      end;
    end;
    if P1 > BufferLength then AlJSONDocErrorA(cALBSONParseError);
    BufferPos := P1 + 1;
    if BufferPos > BufferLength then ExpandBuffer;

    //Get options
    LRegExOptions := [];
    While (BufferPos <= BufferLength) or ExpandBuffer do begin
      case Buffer[BufferPos] of
        'i': LRegExOptions := LRegExOptions + [preCaseLess];
        'm': LRegExOptions := LRegExOptions + [preMultiLine];
        'x': LRegExOptions := LRegExOptions + [preExtended];
        'l':;
        's': LRegExOptions := LRegExOptions + [preSingleLine];
        'u':;
        #$00: break;
      end;
      inc(BufferPos);
    end;
    if BufferPos > BufferLength then AlJSONDocErrorA(cALBSONParseError);
    inc(BufferPos);
    if BufferPos > BufferLength then ExpandBuffer;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetRegEx(LRegEx, LRegExOptions);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LRegEx, Byte(LRegExOptions)], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LRegEx, Byte(LRegExOptions)], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createJavascriptNode(
              const name: AnsiString;
              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJSONNodeA;
      LJavascript: ansiString;
      LInt32: System.Int32;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) + 1 then AlJSONDocErrorA(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    while (BufferPos + LInt32 - 1 > BufferLength) do
      if not ExpandBuffer then AlJSONDocErrorA(cALBSONParseError);
    ALCopyStr(Buffer,LJavascript,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(LJavascript);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LJavascript], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LJavascript], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var LNode: TALJSONNodeA;
      LNodeType: TALJSONNodeType;
      LNodeSubType: TALJSONNodeSubType;
      P1: Integer;
      c: ansiChar;
  Begin

    {$REGION 'init current char (c)'}
    c := Buffer[BufferPos];
    {$ENDREGION}

    {$REGION 'End Object/Array'}
    // ... } ....
    // ... ] ....
    if c = #$00 then begin

      //error if Paths.Count = 0 (mean one end object/array without any starting)
      if assigned(ObjectPaths) then begin
        if (ObjectPaths.Count = 0) then AlJSONDocErrorA(cALBSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then AlJSONDocErrorA(cALBSONParseError);
      end;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        if assigned(ObjectPaths) then LNode := ObjectPaths[ObjectPaths.Count - 1]
        else LNode := TALJSONNodeA(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (LNode <> WorkingNode) then AlJSONDocErrorA(cALBSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then AlJSONDocErrorA(cALBSONParseError);

        //if working node <> Self then we can go to one level up
        If WorkingNode <> Self then begin

          //init WorkingNode to the parentNode
          WorkingNode := WorkingNode.ParentNode;

        end

        //if working node = Self then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

        //calculate anodeTypeInt
        LNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then AlJSONDocErrorA(cALBSONParseError);

      end;

      //call the DoParseEndObject/array event
      if Assigned(OnParseEndObject) then begin
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
    LNodeSubType := nstText; // to hide fucking warning
    case c of
      #$01: LNodeSubType := nstFloat;
      #$02: LNodeSubType := nstText;
      #$03: LNodeSubType := nstObject;
      #$04: LNodeSubType := nstArray;
      #$05: LNodeSubType := nstbinary;
      #$07: LNodeSubType := nstObjectID;
      #$08: LNodeSubType := nstBoolean;
      #$09: LNodeSubType := nstDateTime;
      #$0A: LNodeSubType := nstNull;
      #$0B: LNodeSubType := nstRegEx;
      #$0D: LNodeSubType := nstJavascript;
      #$10: LNodeSubType := nstint32;
      #$11: LNodeSubType := nstTimestamp;
      #$12: LNodeSubType := nstint64;
      else AlJSONDocErrorA(cALBSONParseError);
    end;
    BufferPos := BufferPos + 1;
    If BufferPos > BufferLength then ExpandBuffer;
    {$ENDREGION}

    {$REGION 'Get the node name'}
    P1 := BufferPos;
    While (P1 <= BufferLength) or ExpandBuffer(P1) do begin
      If Buffer[P1] <> #$00 then inc(P1)
      else begin
        AlCopyStr(Buffer, CurrName, BufferPos, P1-BufferPos);
        break;
      end;
    end;
    if P1 > BufferLength then AlJSONDocErrorA(cALBSONParseError);
    BufferPos := P1 + 1;
    if BufferPos > BufferLength then ExpandBuffer;
    {$ENDREGION}

    {$REGION 'Begin Object/Array'}
    // ... { ....
    // ... [ ....
    if LNodeSubType in [nstObject,nstArray] then begin

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then AlJSONDocErrorA(cALBSONParseError);

        //create the node according the the braket char and add it to the workingnode
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
        if LNodeSubType = nstObject then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrName, pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event
      if LNodeSubType = nstObject then begin
        if Assigned(OnParseStartObject) then _DoParseStartObject(CurrName)
      end
      else begin
         if Assigned(OnParseStartArray) then _DoParseStartArray(CurrName);
      end;

      //update BufferPos
      BufferPos := BufferPos + 4; // we don't need the size of the object/array (4 bytes)

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'create the node'}
    case LNodeSubType of
      // \x01 + name + \x00 + double
      nstFloat: _createFloatNode(CurrName, LNodeSubType);

      // \x02 + name + \x00 + length (int32) + string + \x00
      nstText: _createTextNode(CurrName, LNodeSubType);

      // \x05 + name + \x00 + int32 + subtype + (byte*)
      nstbinary: _createBinaryNode(CurrName, LNodeSubType);

      // \x07 + name + \x00 + (byte*12)
      nstObjectID: _createObjectIDNode(CurrName, LNodeSubType);

      // \x08 + name + \x00 + \x00 => Boolean "false"
      // \x08 + name + \x00 + \x01	=> Boolean "true"
      nstBoolean: _createBooleanNode(CurrName, LNodeSubType);

      // \x09 + name + \x00 + int64
      nstDateTime: _createDateTimeNode(CurrName, LNodeSubType);

      // \x11 + name + \x00 + int64
      nstTimestamp: _createTimestampNode(CurrName, LNodeSubType);

      // \x0A + name + \x00
      nstnull: _createNullNode(CurrName, LNodeSubType);

      // \x0B + name + \x00 + (byte*) + \x00 + (byte*) + \x00
      nstRegEx: _createRegExNode(CurrName, LNodeSubType);

      // \x0D + name + \x00 + length (int32) + string + \x00
      nstJavascript: _createJavascriptNode(CurrName, LNodeSubType);

      // \x10 + name + \x00 + int32
      nstint32: _createInt32Node(CurrName, LNodeSubType);

      // \x12 + name + \x00 + int64
      nstint64: _createInt64Node(CurrName, LNodeSubType);

      else AlJSONDocErrorA(cALBSONParseError);
    end;
    {$ENDREGION}

  end;

Begin

  //Only Object Node can be loaded from BSON
  If NodeType <> ntObject then AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
  if poClearChildNodes in Options then ChildNodes.Clear;

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
      Buffer := '';
      BufferLength := 0;
      BufferPos := 5; // the first 4 bytes are the length of the document and we don't need it
      ExpandBuffer;
    end
    else begin
      Buffer := RawBSONString;
      BufferLength := length(RawBSONString);
      BufferPos := 5; // the first 4 bytes are the length of the document and we don't need it
    end;

    //add first node in ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.Add(WorkingNode)
    else begin
      if NotSaxMode then NamePaths.AddObject('', WorkingNode)
      else NamePaths.AddObject('', pointer(ntObject));
    end;
    _DoParseStartObject('');

    //analyze all the nodes
    While (BufferPos <= BufferLength) or ExpandBuffer do
      AnalyzeNode;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then AlJSONDocErrorA(cALBSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then AlJSONDocErrorA(cALBSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if NotSaxMode and (WorkingNode <> nil) then AlJSONDocErrorA(cALBSONParseError);

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ALFreeAndNil(ObjectPaths)
    else ALFreeAndNil(NamePaths);

  end;

end;

{********************************}
procedure TALJSONNodeA.SaveToJson(
            const Stream: TStream;
            Var buffer: ansiString;
            const Options: TALJSONSaveOptions);

Const BufferSize: integer = 8192;

Var NodeStack: Tstack<TALJSONNodeA>;
    CurrentNode: TALJSONNodeA;
    CurrentParentNode: TALJSONNodeA;
    CurrentIndentStr: AnsiString;
    IndentStr: AnsiString;
    EncodeControlCharacters: Boolean;
    SkipNodeSubTypeHelper: boolean;
    SaveInt64AsText: Boolean;
    AutoIndentNode: Boolean;
    BufferPos: Integer;
    LastWrittenChar: AnsiChar;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteBuffer2Stream(const buffer: ansiString; BufferLength: Integer);
  Begin
    if assigned(Stream) then begin
      If BufferLength > 0 then stream.Writebuffer(pointer(buffer)^,BufferLength);
      BufferPos := 0;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _Write2Buffer(const Source; Count: NativeInt);
  Begin
    if Count = 0 then exit;
    if Count + BufferPos > length(Buffer) then setlength(Buffer, Count + BufferPos + BufferSize);
    ALMove(Source, pbyte(Buffer)[BufferPos{*sizeOf(ansiChar)}], Count{*sizeOf(ansiChar)});
    BufferPos := BufferPos + Count;
    if BufferPos >= 32768 then _WriteBuffer2Stream(Buffer,BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStr2Buffer(const str:AnsiString);
  var L: integer;
  Begin
    L := Length(Str);
    if L = 0 then exit;
    LastWrittenChar := Str[L];
    _Write2Buffer(pointer(str)^,L);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteTextNode2Buffer(aTextNode:TALJSONNodeA);
  Begin
    with aTextNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode then begin
        _WriteStr2Buffer(#13#10);
        _WriteStr2Buffer(CurrentIndentStr);
      end;

      if (assigned(ParentNode)) and
         (ParentNode.NodeType <> ntArray) then begin
        if EncodeControlCharacters then begin
           _WriteStr2Buffer('"');
           _WriteStr2Buffer(ALJavascriptEncode(NodeName));
           _WriteStr2Buffer('":');
        end
        else begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(NodeName);
          _WriteStr2Buffer('":');
        end;
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
      else _WriteStr2Buffer(GetNodeValueInterchange(SkipNodeSubTypeHelper));

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNodeA);
  var LNodeList: TALJSONNodeListA;
      LEmptyNode: Boolean;
      I: integer;
  Begin
    with aObjectNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode and (CurrentIndentStr <> '') then begin
         _WriteStr2Buffer(#13#10);
         _WriteStr2Buffer(CurrentIndentStr);
      end;

      if aObjectNode = self then _WriteStr2Buffer('{')
      else if (assigned(ParentNode)) and
              (ParentNode.NodeType <> ntArray) then begin
        if EncodeControlCharacters then begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(ALJavascriptEncode(NodeName));
          _WriteStr2Buffer('":{')
        end
        else begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(NodeName);
          _WriteStr2Buffer('":{');
        end;
      end
      else _WriteStr2Buffer('{');

      LEmptyNode := True;
      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            NodeStack.Push(aObjectNode);
            For I := Count - 1 downto 0 do NodeStack.Push(Nodes[I]);
          end
      end;

      If LEmptyNode then _WriteStr2Buffer('}')
      else CurrentIndentStr := CurrentIndentStr + IndentStr;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNodeA);
  Begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr)+1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer('}');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNodeA);
  var LNodeList: TALJSONNodeListA;
      LEmptyNode: Boolean;
      I: integer;
  Begin
    with aArrayNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode and (CurrentIndentStr <> '') then begin
        _WriteStr2Buffer(#13#10);
        _WriteStr2Buffer(CurrentIndentStr);
      end;

      if aArrayNode = self then _WriteStr2Buffer('[')
      else if (assigned(ParentNode)) and
              (ParentNode.NodeType <> ntArray) then begin
        if EncodeControlCharacters then begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(ALJavascriptEncode(NodeName));
          _WriteStr2Buffer('":[');
        end
        else begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(NodeName);
          _WriteStr2Buffer('":[');
        end;
      end
      else _WriteStr2Buffer('[');

      LEmptyNode := True;
      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            NodeStack.Push(aArrayNode);
            For I := Count - 1 downto 0 do NodeStack.Push(Nodes[I]);
          end
      end;

      If LEmptyNode then _WriteStr2Buffer(']')
      else CurrentIndentStr := CurrentIndentStr + IndentStr;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNodeA);
  Begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr) + 1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer(']');
  end;

begin
  If not (NodeType in [ntObject, ntArray]) then exit;  // normally only Object node can gave a valid json stream
                                                       // but their is some situation where the array (containing json node)
                                                       // is also usefull
  CurrentParentNode := nil;
  NodeStack := Tstack<TALJSONNodeA>.Create;
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
    NodeStack.Push(self);

    {loop on all nodes}
    While NodeStack.Count > 0 Do begin
      CurrentNode := NodeStack.Pop;

      with CurrentNode do
        case NodeType of
          ntObject: begin
                      if currentNode = CurrentParentNode then _WriteEndObjectNode2Buffer(CurrentNode)
                      else _WriteStartObjectNode2Buffer(CurrentNode);
                    end;
          ntArray: begin
                      if currentNode = CurrentParentNode then _WriteEndArrayNode2Buffer(CurrentNode)
                      else _WriteStartArrayNode2Buffer(CurrentNode);
                   end;
          ntText: _WriteTextNode2Buffer(CurrentNode);
          else AlJSONDocErrorA(cAlJSONInvalidNodeType);
        end;

      CurrentParentNode := CurrentNode.ParentNode;
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
var buffer: ansiString;
begin
  SaveToJson(Stream, buffer, Options);
end;

{*******************************}
{Saves the JSON document to disk.
 Call SaveToFile to save any modifications you have made to the parsed JSON document.
 AFileName is the name of the file to save.}
procedure TALJSONNodeA.SaveToJSONFile(const FileName: String; const Options: TALJSONSaveOptions = []);
Var LfileStream: TfileStream;
    LTmpFilename: String;
begin
  if soProtectedSave in Options then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LfileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToJSONStream(LfileStream, Options);
    finally
      ALFreeAndNil(LfileStream);
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
  SaveToJson(nil, Str, Options);
end;

{********************************}
procedure TALJSONNodeA.SaveToBson(
            const Stream: TStream;
            Var buffer: ansiString;
            const Options: TALJSONSaveOptions);

Const BufferSize: integer = 8192;

Var NodeStack: Tstack<TALJSONNodeA>;
    NodeIndexStack: TALintegerList;
    NodeStartPosStack: TALInt64List;
    CurrentNode: TALJSONNodeA;
    CurrentParentNode: TALJSONNodeA;
    CurrentNodeIndex: integer;
    CurrentNodeStartPos: System.int64;
    BufferPos: NativeInt;
    StreamPos: system.int64;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteBuffer2Stream(const buffer: ansiString; BufferLength: Integer);
  Begin
    if assigned(Stream) then begin
      If BufferLength > 0 then stream.Writebuffer(pointer(buffer)^,BufferLength);
      BufferPos := 0;
      StreamPos := stream.Position;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _Write2Buffer(const Source; Count: NativeInt);
  Begin
    if Count = 0 then exit;
    if Count + BufferPos > length(Buffer) then setlength(Buffer, Count + BufferPos + BufferSize);
    ALMove(Source, pbyte(Buffer)[BufferPos], Count);
    BufferPos := BufferPos + Count;
    if BufferPos >= 32768 then _WriteBuffer2Stream(Buffer,BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStr2Buffer(const str:AnsiString); overload;
  Begin
    _Write2Buffer(pointer(str)^,length(Str));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStr2Buffer(const index:integer); overload;
  Begin
    _WriteStr2Buffer(ALIntToStrA(index));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //take care because fucking TStringStream (for exemple) do not permit
  //to write previous to the current position (it's set the size of the
  //new stream to the current position ... unbelievable!)
  Procedure _WriteInt2Pos(const aInt:integer; const aPos: system.Int64);
  Begin
    if aPos < StreamPos then begin
      Stream.position := aPos;
      stream.Writebuffer(aInt,sizeof(aInt));
      Stream.position := StreamPos;
    end
    else ALMove(aInt, Buffer[aPos - StreamPos + 1], sizeOf(aInt));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x01 + name + \x00 + double
  Procedure _WriteFloatValue2Buffer(aTextNode:TALJSONNodeA);
  var LDouble: Double;
  begin
    LDouble := aTextNode.Float;
    _Write2Buffer(LDouble, sizeOf(LDouble));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x02 + name + \x00 + length (int32) + string + \x00
  Procedure _WriteTextValue2Buffer(aTextNode:TALJSONNodeA);
  var LInt32: system.int32;
      LText: ansiString;
  begin
    LText := aTextNode.Text;
    LInt32 := length(LText) + 1 {for the trailing #0};
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _WriteStr2Buffer(LText);
    _WriteStr2Buffer(#$00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x05 + name + \x00 + int32 + subtype + (byte*)
  Procedure _WriteBinaryValue2Buffer(aTextNode:TALJSONNodeA);
  var LInt32: system.int32;
      LBinary: ansiString;
      LBinarySubType: Byte;
  begin
    LBinary := aTextNode.binary;
    LBinarySubType := aTextNode.BinarySubType;
    LInt32 := length(LBinary);
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _Write2Buffer(LBinarySubType, sizeOF(LBinarySubType));
    _WriteStr2Buffer(LBinary);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x07 + name + \x00 + (byte*12)
  Procedure _WriteObjectIDValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    _WriteStr2Buffer(aTextNode.ObjectID);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x08 + name + \x00 + \x00 => Boolean "false"
  // \x08 + name + \x00 + \x01	=> Boolean "true"
  Procedure _WriteBooleanValue2Buffer(aTextNode:TALJSONNodeA);
  begin
    if not aTextNode.bool then _WriteStr2Buffer(#$00)
    else _WriteStr2Buffer(#$01);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x09 + name + \x00 + int64
  Procedure _WriteDateTimeValue2Buffer(aTextNode:TALJSONNodeA);
  var LInt64: system.Int64;
  begin
    LInt64 := ALDateTimeToUnixMs(aTextNode.DateTime);
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x11 + name + \x00 + int64
  Procedure _WriteTimestampValue2Buffer(aTextNode:TALJSONNodeA);
  var LInt64: system.Int64;
  begin
    LInt64 := aTextNode.Timestamp.I64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \xOB + name + \x00 + (byte*) + \x00 + (byte*) + \x00
  Procedure _WriteRegExValue2Buffer(aTextNode:TALJSONNodeA);
  var LRegExOptions: TALPerlRegExOptions;
      LRegExOptionsStr: ansiString;
  begin
    LRegExOptionsStr := '';
    LRegExOptions := aTextNode.RegExOptions;
    if preCaseLess in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 'i';
    if preMultiLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'m';
    if preExtended in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'x';
    //'l':;
    if preSingleLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 's';
    //'u':;
    _WriteStr2Buffer(aTextNode.RegEx);
    _WriteStr2Buffer(#$00);
    _WriteStr2Buffer(LRegExOptionsStr);
    _WriteStr2Buffer(#$00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x0D + name + \x00 + length (int32) + string + \x00
  Procedure _WriteJavascriptValue2Buffer(aTextNode:TALJSONNodeA);
  var LInt32: system.int32;
      LJavascript: ansiString;
  begin
    LJavascript := aTextNode.Javascript;
    LInt32 := length(LJavascript) + 1 {for the trailing #0};
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _WriteStr2Buffer(LJavascript);
    _WriteStr2Buffer(#$00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x10 + name + \x00 + int32
  Procedure _WriteInt32Value2Buffer(aTextNode:TALJSONNodeA);
  var LInt32: system.Int32;
  begin
    LInt32 := aTextNode.int32;
    _Write2Buffer(LInt32, sizeOf(LInt32));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x12 + name + \x00 + int64
  Procedure _WriteInt64Value2Buffer(aTextNode:TALJSONNodeA);
  var LInt64: system.Int64;
  begin
    LInt64 := aTextNode.int64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteTextNode2Buffer(aTextNode:TALJSONNodeA; aNodeIndex: integer);
  Begin
    with aTextNode do begin

      // write the node subtype
      case NodeSubType of
        // \x01 + name + \x00 + double
        nstFloat: _WriteStr2Buffer(#$01);
        // \x02 + name + \x00 + length (int32) + string + \x00
        nstText: _WriteStr2Buffer(#$02);
        // \x05 + name + \x00 + int32 + subtype + (byte*)
        nstbinary: _WriteStr2Buffer(#$05);
        // \x07 + name + \x00 + (byte*12)
        nstObjectID: _WriteStr2Buffer(#$07);
        // \x08 + name + \x00 + \x00 => Boolean "false"
        // \x08 + name + \x00 + \x01	=> Boolean "true"
        nstBoolean: _WriteStr2Buffer(#$08);
        // \x09 + name + \x00 + int64
        nstDateTime: _WriteStr2Buffer(#$09);
        // \x11 + name + \x00 + int64
        nstTimestamp: _WriteStr2Buffer(#$11);
        // \x0A + name + \x00
        nstNull: _WriteStr2Buffer(#$0A);
        // \xOB + name + \x00 + (byte*) + \x00 + (byte*) + \x00
        nstRegEx: _WriteStr2Buffer(#$0B);
        // \x0D + name + \x00 + length (int32) + string + \x00
        nstJavascript: _WriteStr2Buffer(#$0D);
        // \x10 + name + \x00 + int32
        nstInt32: _WriteStr2Buffer(#$10);
        // \x12 + name + \x00 + int64
        nstInt64: _WriteStr2Buffer(#$12);
        else AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
      end;

      // write the nodename
      if (assigned(ParentNode)) and
         (ParentNode.NodeType = ntArray) then _WriteStr2Buffer(aNodeIndex)
      else _WriteStr2Buffer(NodeName);
      _WriteStr2Buffer(#$00);

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
        else AlJSONDocErrorA(cALJSONInvalidBSONNodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNodeA; aNodeIndex: integer);
  var LNodeList: TALJSONNodeListA;
      LEmptyNode: Boolean;
      LPos: system.int64;
      I: integer;
  Begin
    with aObjectNode do begin

      if aObjectNode = self then _WriteStr2Buffer(#$00#$00#$00#$00)
      else if (assigned(ParentNode)) and
              (ParentNode.NodeType = ntArray) then begin
        _WriteStr2Buffer(#$03);
        _WriteStr2Buffer(aNodeIndex);
        _WriteStr2Buffer(#$00#$00#$00#$00#$00);
      end
      else begin
        _WriteStr2Buffer(#$03);
        _WriteStr2Buffer(NodeName);
        _WriteStr2Buffer(#$00#$00#$00#$00#$00);
      end;

      LPos := StreamPos + BufferPos - 4{length of the #$00#$00#$00#$00};

      LEmptyNode := True;
      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            NodeStack.Push(aObjectNode);
            NodeIndexStack.Push(aNodeIndex);
            NodeStartPosStack.Push(LPos);
            For I := Count - 1 downto 0 do begin
              NodeStack.Push(Nodes[I]);
              NodeIndexStack.Push(I);
              NodeStartPosStack.Push(-1);
            end;
          end
      end;

      If LEmptyNode then begin
        _WriteStr2Buffer(#$00);
        _WriteInt2Pos(5{length of the object},LPos);
      end;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNodeA; aNodeStartPos: system.Int64);
  Begin
    _WriteStr2Buffer(#$00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNodeA; aNodeIndex: integer);
  var LNodeList: TALJSONNodeListA;
      LEmptyNode: Boolean;
      LPos: system.int64;
      I: integer;
  Begin
    with aArrayNode do begin

      if (assigned(ParentNode)) and
         (ParentNode.NodeType = ntArray) then begin
        _WriteStr2Buffer(#$04);
        _WriteStr2Buffer(aNodeIndex);
        _WriteStr2Buffer(#$00#$00#$00#$00#$00);
      end
      else begin
        _WriteStr2Buffer(#$04);
        _WriteStr2Buffer(NodeName);
        _WriteStr2Buffer(#$00#$00#$00#$00#$00);
      end;

      LPos := StreamPos + BufferPos - 4{length of the #$00+#$00+#$00+#$00};

      LEmptyNode := True;
      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            NodeStack.Push(aArrayNode);
            NodeIndexStack.Push(aNodeIndex);
            NodeStartPosStack.Push(LPos);
            For I := Count - 1 downto 0 do begin
              NodeStack.Push(Nodes[I]);
              NodeIndexStack.Push(I);
              NodeStartPosStack.Push(-1);
            end;
          end
      end;

      If LEmptyNode then begin
        _WriteStr2Buffer(#$00);
        _WriteInt2Pos(5{length of the object},LPos);
      end;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNodeA; aNodeStartPos: system.Int64);
  Begin
    _WriteStr2Buffer(#$00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

begin
  If NodeType <> ntobject then exit;

  CurrentParentNode := nil;
  NodeStack := Tstack<TALJSONNodeA>.Create;
  NodeIndexStack := TALintegerList.Create;
  NodeStartPosStack := TALInt64List.Create;
  Try

    {init buffer string}
    Setlength(Buffer, BufferSize); // will make buffer uniquestring
    BufferPos := 0;
    if assigned(Stream) then StreamPos := Stream.Position
    else StreamPos := 0;

    {SaveOnlyChildNode}
    NodeStack.Push(self);
    NodeIndexStack.Push(0);
    NodeStartPosStack.Push(StreamPos);


    {loop on all nodes}
    While NodeStack.Count > 0 Do begin
      CurrentNode := NodeStack.Pop;
      CurrentNodeIndex := integer(NodeIndexStack.Pop);
      CurrentNodeStartPos := integer(NodeStartPosStack.Pop);

      with CurrentNode do
        case NodeType of
          ntObject: begin
                      if currentNode = CurrentParentNode then _WriteEndObjectNode2Buffer(CurrentNode, CurrentNodeStartPos)
                      else _WriteStartObjectNode2Buffer(CurrentNode, CurrentNodeIndex);
                    end;
          ntArray: begin
                      if currentNode = CurrentParentNode then _WriteEndArrayNode2Buffer(CurrentNode, CurrentNodeStartPos)
                      else _WriteStartArrayNode2Buffer(CurrentNode, CurrentNodeIndex);
                   end;
          ntText: _WriteTextNode2Buffer(CurrentNode, CurrentNodeIndex);
          else AlJSONDocErrorA(cAlJSONInvalidNodeType);
        end;

      CurrentParentNode := CurrentNode.ParentNode;
    end;

    {Write the buffer}
    if assigned(Stream) then _WriteBuffer2Stream(Buffer, BufferPos)
    else setlength(Buffer,BufferPos);

  finally
    ALFreeAndNil(NodeStack);
    ALFreeAndNil(NodeIndexStack);
    ALFreeAndNil(NodeStartPosStack);
  end;
end;

{*****************************************************************************************************}
procedure TALJSONNodeA.SaveToBsonStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
var buffer: ansiString;
begin
  SaveToBson(Stream, buffer, Options);
end;

{****************************************************************************************************}
procedure TALJSONNodeA.SaveToBsonFile(const FileName: String; const Options: TALJSONSaveOptions = []);
Var LfileStream: TfileStream;
    LTmpFilename: String;
begin
  if soProtectedSave in Options then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LfileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToBsonStream(LfileStream, Options);
    finally
      ALFreeAndNil(LfileStream);
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
procedure TALJSONNodeA.SaveToBsonFile(const FileName: AnsiString; const Options: TALJSONSaveOptions = []);
begin
  SaveToBsonFile(String(FileName), Options);
end;

{***************************************************************************************************}
procedure TALJSONNodeA.SaveToBsonString(var str: AnsiString; const Options: TALJSONSaveOptions = []);
begin
  SaveToBson(nil, Str, Options);
end;

{*************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromJSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  Try
    ParseJson(nil, Str, False{SaxMode}, nil{onParseText}, nil{onParseStartObject}, nil{onParseEndObject}, nil{onParseStartArray}, nil{onParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  Try
    ParseJSON(Stream, '', False{SaxMode}, nil{onParseText}, nil{onParseStartObject}, nil{onParseEndObject}, nil{onParseStartArray}, nil{onParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromJSONStream(LfileStream, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{****************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromJSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  LoadFromJSONFile(String(FileName), Options);
end;

{*************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromBSONString(const Str: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  Try
    ParseBSON(nil, Str, False{SaxMode}, nil{onParseText}, nil{onParseStartObject}, nil{onParseEndObject}, nil{onParseStartArray}, nil{onParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  Try
    ParseBSON(Stream, '', False{SaxMode}, nil{onParseText}, nil{onParseStartObject}, nil{onParseEndObject}, nil{onParseStartArray}, nil{onParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromBSONStream(LfileStream, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{****************************************************************************************************************************}
procedure TALJSONNodeA.LoadFromBSONFile(const FileName: AnsiString; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  LoadFromBSONFile(String(FileName), Options);
end;

{*************************************}
procedure TALJSONNodeA.ParseJSONString(
            const Str: AnsiString;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
Begin
  ParseJson(nil, Str, true{SaxMode}, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeA.ParseJSONStream(
            const Stream: TStream;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
Begin
  ParseJSON(Stream, '', true{SaxMode}, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{***********************************}
procedure TALJSONNodeA.ParseJSONFile(
            const FileName: String;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseJSONStream(LfileStream, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{***********************************}
procedure TALJSONNodeA.ParseJSONFile(
            const FileName: AnsiString;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
Begin
  ParseJSONFile(String(FileName), onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeA.ParseBSONString(
            const Str: AnsiString;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
Begin
  ParseBSON(nil, Str, true{SaxMode}, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeA.ParseBSONStream(
            const Stream: TStream;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
Begin
  ParseBSON(Stream, '', true{SaxMode}, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{***********************************}
procedure TALJSONNodeA.ParseBSONFile(
            const FileName: String;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseBSONStream(LfileStream, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{***********************************}
procedure TALJSONNodeA.ParseBSONFile(
            const FileName: AnsiString;
            const onParseText: TAlJSONParseTextEventA;
            const onParseStartObject: TAlJSONParseObjectEventA;
            const onParseEndObject: TAlJSONParseObjectEventA;
            const onParseStartArray: TAlJSONParseArrayEventA;
            const onParseEndArray: TAlJSONParseArrayEventA;
            const Options: TALJSONParseOptions = []);
Begin
  ParseBSONFile(String(FileName), onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{*********************************************************************}
constructor TALJSONObjectNodeA.Create(const NodeName: AnsiString = '');
begin
  inherited create(NodeName);
  FChildNodes := nil;
end;

{************************************}
destructor TALJSONObjectNodeA.Destroy;
begin
  If assigned(FChildNodes) then FreeAndNil(FchildNodes);
  inherited;
end;

{**********************************************************}
function TALJSONObjectNodeA.GetChildNodes: TALJSONNodeListA;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{************************************************************************}
procedure TALJSONObjectNodeA.SetChildNodes(const Value: TALJSONNodeListA);
begin
  If Assigned(FChildNodes) then FreeAndNil(FchildNodes);
  FChildNodes := Value;
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

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONObjectNodeA.InternalGetChildNodes: TALJSONNodeListA;
begin
  Result := FChildNodes;
end;

{********************************************************************}
constructor TALJSONArrayNodeA.Create(const NodeName: AnsiString = '');
begin
  inherited create(NodeName);
  FChildNodes := nil;
end;

{***********************************}
destructor TALJSONArrayNodeA.Destroy;
begin
  If assigned(FChildNodes) then FreeAndNil(FchildNodes);
  inherited;
end;

{*********************************************************}
function TALJSONArrayNodeA.GetChildNodes: TALJSONNodeListA;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{***********************************************************************}
procedure TALJSONArrayNodeA.SetChildNodes(const Value: TALJSONNodeListA);
begin
  If Assigned(FChildNodes) then FreeAndNil(FchildNodes);
  FChildNodes := Value;
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

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONArrayNodeA.InternalGetChildNodes: TALJSONNodeListA;
begin
  Result := FChildNodes;
end;

{*******************************************************************}
constructor TALJSONTextNodeA.Create(const NodeName: AnsiString = '');
begin
  inherited create(NodeName);
  fNodeSubType := nstText;
  fRawNodeValueStr := '';
  FRawNodeValueInt64 := 0;
  fRawNodeValueDefined := [nvStr];
end;

{*****************************************************}
function TALJSONTextNodeA.GetNodeType: TALJSONNodeType;
begin
  Result := NtText;
end;

{***********************************************************}
function TALJSONTextNodeA.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := fNodeSubType;
end;

{****************************************************}
function TALJSONTextNodeA.GetNodeValueStr: ansiString;
begin
  if nvStr in fRawNodeValueDefined then result := fRawNodeValueStr
  else begin

    if not (nvInt64 in fRawNodeValueDefined) then AlJSONDocErrorA(CALJsonOperationError,GetNodeType);

    case fNodeSubType of
      nstFloat: ALFloatToStrA(GetFloat, fRawNodeValueStr, ALDefaultFormatSettingsA);
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      nstBoolean: ALBoolToStrA(fRawNodeValueStr, getBool, 'true', 'false');
      nstDateTime: ALDateTimeToStrA(GetDateTime, fRawNodeValueStr, ALDefaultFormatSettingsA);
      nstNull: fRawNodeValueStr := 'null';
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
      nstInt32: ALIntToStrA(GetInt32, fRawNodeValueStr);
      nstTimestamp: ALFormatA('Timestamp(%u, %u)', [GetTimestamp.W1,GetTimestamp.W2], fRawNodeValueStr);
      nstInt64: ALIntToStrA(GetInt64, fRawNodeValueStr);
      else AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
    end;

    fRawNodeValueDefined := fRawNodeValueDefined + [nvStr];
    result := fRawNodeValueStr;

  end;
end;

{*************************************************}
function TALJSONTextNodeA.GetNodeValueInt64: int64;
var LDouble: Double;
    LBool: boolean;
    LDateTime: TdateTime;
    LInt32: system.int32;
    LTimestamp: TALBSONTimestamp;
begin
  if nvInt64 in fRawNodeValueDefined then result := fRawNodeValueInt64
  else begin

    if not (nvStr in fRawNodeValueDefined) then AlJSONDocErrorA(CALJsonOperationError,GetNodeType);

    case fNodeSubType of
      nstFloat: begin
                  IF not ALTryStrToFloat(fRawNodeValueStr, LDouble, ALDefaultFormatSettingsA) then AlJSONDocErrorA('%s is not a valid Float', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := Pint64(@LDouble)^;
                end;
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      nstBoolean: begin
                    IF not ALTryStrToBool(fRawNodeValueStr, LBool) then AlJSONDocErrorA('%s is not a valid Boolean', [fRawNodeValueStr]);
                    fRawNodeValueInt64 := ALBoolToInt(LBool);
                  end;
      nstDateTime: begin
                     IF not ALTryStrToDateTime(fRawNodeValueStr, LDateTime, ALDefaultFormatSettingsA) then AlJSONDocErrorA('%s is not a valid Datetime', [fRawNodeValueStr]);
                     fRawNodeValueInt64 := Pint64(@LDateTime)^;
                   end;
      nstNull:  begin
                  fRawNodeValueInt64 := 0;
                end;
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
      nstInt32: begin
                  IF not ALTryStrToInt(fRawNodeValueStr, LInt32) then AlJSONDocErrorA('%s is not a valid Int32', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := LInt32;
                end;
      nstTimestamp: begin
                      IF not ALJSONTryStrToTimestampA(fRawNodeValueStr, LTimestamp) then AlJSONDocErrorA('%s is not a valid Timestamp', [fRawNodeValueStr]);
                      fRawNodeValueInt64 := LTimestamp.I64;
                    end;
      nstInt64: begin
                  IF not ALTryStrToInt64(fRawNodeValueStr, fRawNodeValueInt64) then AlJSONDocErrorA('%s is not a valid Int64', [fRawNodeValueStr]);
                end;
      else AlJSONDocErrorA(CALJsonOperationError,GetNodeType);
    end;

    fRawNodeValueDefined := fRawNodeValueDefined + [nvInt64];
    result := fRawNodeValueInt64;

  end;
end;

{******************************************************************************************************}
procedure TALJSONTextNodeA.SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType);
begin
  fNodeSubType := NodeSubType;
  fRawNodeValueStr := Value;
  fRawNodeValueDefined := [nvStr];
end;

{*************************************************************************************************}
procedure TALJSONTextNodeA.SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  fNodeSubType := NodeSubType;
  fRawNodeValueInt64 := Value;
  if (NodeSubType in [nstBinary, nstRegEx]) then fRawNodeValueDefined := fRawNodeValueDefined + [nvInt64] // keep the fNodeValueStr
  else fRawNodeValueDefined := [nvInt64];
end;

{**********************************************************************************************************************************}
procedure TALJSONTextNodeA.SetNodeValue(const StrValue: AnsiString; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  fNodeSubType := NodeSubType;
  fRawNodeValueStr := StrValue;
  fRawNodeValueInt64 := Int64Value;
  fRawNodeValueDefined := [nvStr, nvInt64];
end;

{*******************************************************}
constructor TALJSONNodeListA.Create(Owner: TALJSONNodeA);
begin
  FList:= nil;
  FCount:= 0;
  FCapacity := 0;
  FOwner := Owner;
  FSorted := False;
  FDuplicates := dupAccept;
  SetSorted(False);
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
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareNodeNames(FList[I].NodeName, NodeName);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
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
  if not Sorted then begin
    if Direction = TDirection.FromBeginning then begin
      for Result := 0 to Count - 1 do
        if FList[Result].NodeName = Name then Exit;
    end
    else begin
      for Result := Count - 1 downto 0 do
        if FList[Result].NodeName = Name then Exit;
    end;
    Result := -1;
  end
  else if not Find(Name, Result) then Result := -1;
end;

{*******************************************************************************************************************************}
function TALJSONNodeListA.IndexOfValue(const Value: ansiString; const Direction: TDirection = TDirection.FromBeginning): Integer;
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
function TALJSONNodeListA.IndexOfValue(const Value: integer; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].int32 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].int32 = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************************************************************************************************}
function TALJSONNodeListA.IndexOfValue(const Value: int64; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].int64 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].int64 = Value) then Exit;
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
var Index: Integer;
begin
  Index := IndexOf(NodeName, Direction);
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
  else result := nil;
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
var Index: Integer;
begin
  Index := IndexOf(Node) + Delta;
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
  if (Index < 0) or (Index >= FCount) then AlJSONDocErrorA(CALJSONListIndexError, [Index]);
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
  if not Assigned(Result) then AlJSONDocErrorA(CALJSONNodeNotFound, [Name]);
end;

{****************************************************************************}
function TALJSONNodeListA.CompareNodeNames(const S1, S2: AnsiString): Integer;
begin
  Result := ALCompareStrA(S1, S2)
end;

{*****************************************************************************************}
procedure TALJSONNodeListA.QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareA);
var
  I, J, P: Integer;
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        Exchange(L, R);
      break;
    end;
    I := L;
    J := R;
    P := (L + R) shr 1;
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
  if (not Sorted) and (FList <> nil) and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

{**************************************}
{Adds a new node to the end of the list.
 Call Add to add a node to the end of the list. Add returns the index of the node once it is added, where 0 is the index
 of the first node in the list, 1 is the index of the second node, and so on.
 *Node is the node to add to the list.}
function TALJSONNodeListA.Add(const Node: TALJSONNodeA): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(Node.NodeName, Result) then
      case Duplicates of
        dupIgnore: begin
                     ALFreeAndNil(Node);
                     Exit;
                   end;
        dupError: AlJSONDocErrorA(cALJSONDuplicateNodeName);
      end;
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
  Node.SetParentNode(Fowner);
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
    if Sorted then AlJSONDocErrorA(CALJSONSortedListError);
    if (Index < 0) or (Index > FCount) then AlJSONDocErrorA(CALJSONListIndexError, [Index]);
    InternalInsert(Index, Node);
  end;
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Index identifies the node to remove by index rather than name. Index ranges from 0 to one less than the value of the Count property.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Index Delete returns –1.}
function TALJSONNodeListA.Delete(const Index: Integer): Integer;
var Node: TALJSONNodeA;
begin
  Node := Get(Index);
  FList[Index] := nil; // to decrease the refcount of Node
  Dec(FCount);
  if Index < FCount then begin
    ALMove(
      FList[Index + 1],
      FList[Index],
      (FCount - Index) * SizeOf(Pointer));
    Pointer(FList[FCount]) := nil;
  end;
  if assigned(Node) then FreeAndNil(Node);
  result := Index;
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Name identifies the node to remove from the list. This is the local name of the node to remove.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Name, Delete returns –1.}
function TALJSONNodeListA.Delete(const Name: AnsiString): Integer;
begin
  result := indexOf(Name);
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
var I: Integer;
begin
  Result := nil;
  I := IndexOf(Node);
  if I >= 0 then result := Extract(i);
end;

{***********************************************************}
procedure TALJSONNodeListA.Exchange(Index1, Index2: Integer);
var Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then AlJSONDocErrorA(cALJSONListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then AlJSONDocErrorA(cALJSONListIndexError, [Index2]);
  Item := pointer(FList[Index1]);
  pointer(FList[Index1]) := pointer(FList[Index2]);
  pointer(FList[Index2]) := Item;
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeListA.Extract(const index: integer): TALJSONNodeA;
begin
  Result := Get(index);
  Result.SetParentNode(nil);
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
var Index: Integer;
begin
  Index := indexOf(OldNode);
  Result := Extract(Index);
  if not sorted then Insert(Index, NewNode)
  else Add(NewNode);
end;

{*******************************}
{Removes all nodes from the list.
 Call Clear to empty the list.
 Note:	Clear does not call the BeginUpdate and EndUpdate methods, even though it may result in the
 deletion of more than one node.}
procedure TALJSONNodeListA.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{******************************}
procedure TALJSONNodeListA.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{***********************************************************}
procedure TALJSONNodeListA.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) then AlJSONDocErrorA(CALJSONListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*****************************************************************************}
procedure TALJSONNodeListA.SetDuplicates(Value: TDuplicates; Recurse: Boolean);
begin
  FDuplicates := Value;
  if Recurse then begin
    for Var I := 0 to count-1 do begin
      Var LNodeList := Get(i).InternalGetChildNodes;
      if LNodeList <> nil then LNodeList.SetDuplicates(Value,Recurse);
    end;
  end;
end;

{***********************************************************}
procedure TALJSONNodeListA.SetDuplicates(Value: TDuplicates);
begin
  SetDuplicates(Value, False);
end;

{*********************************************************************}
procedure TALJSONNodeListA.SetSorted(Value: Boolean; Recurse: Boolean);
begin
  if FSorted <> Value then
  begin
    if owner is TALJSONObjectNodeA then begin
      if Value then Sort;
      FSorted := Value;
    end
    else FSorted := False;
  end;
  if Recurse then begin
    for Var I := 0 to count-1 do begin
      Var LNodeList := Get(i).InternalGetChildNodes;
      if LNodeList <> nil then LNodeList.SetSorted(Value,Recurse);
    end;
  end;
end;

{***************************************************}
procedure TALJSONNodeListA.SetSorted(Value: Boolean);
begin
  SetSorted(Value, False);
end;

{*****************************************************}
procedure TALJSONNodeListA.SetCount(NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) then AlJSONDocErrorA(CALJSONListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{**************************}
Procedure ALJSONToTStringsA(
            const AJsonStr: AnsiString;
            const aFormatSettings: TALFormatSettingsA;
            const aPath: AnsiString;
            const aLst: TALStringsA;
            Const aNullStr: AnsiString = 'null';
            Const aTrueStr: AnsiString = 'true';
            Const aFalseStr: AnsiString = 'false');
var LContainChilds: boolean;
begin
  LContainChilds := False;
  TALJSONDocumentA.ParseJSONString(
    AJsonStr,
    //--
    procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
    begin
      if (NodeSubType = nstFloat) then          aLst.Add(aPath + Path + aLst.NameValueSeparator + ALFloatToStrA(Args[0].VExtended^, aFormatSettings))
      else if (NodeSubType = nstDateTime) then  aLst.Add(aPath + Path + aLst.NameValueSeparator + ALDateTimeToStrA(Args[0].VExtended^, aFormatSettings))
      else if (NodeSubType = nstBoolean) then   aLst.Add(aPath + Path + aLst.NameValueSeparator + ALBoolToStrA(Args[0].VBoolean,aTrueStr,aFalseStr))
      else if (NodeSubType = nstnull) then      aLst.Add(aPath + Path + aLst.NameValueSeparator + aNullStr)
      else                                      aLst.Add(aPath + Path + aLst.NameValueSeparator + ansiString(Args[0].VAnsiString));
      LContainChilds := True;
    end{onParseText},
    //--
    procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
    begin
      LContainChilds := False;
    end{onParseStartObject},
    //--
    procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
    begin
      if (not LContainChilds) and (aPath + Path <> ''{Path = '' mean it's the root object}) then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '{}');
      LContainChilds := True;
    end{onParseEndObject},
    //--
    procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
    begin
      LContainChilds := False;
    end{onParseStartArray},
    //--
    procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
    begin
      if not LContainChilds then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '[]');
      LContainChilds := True;
    end{onParseEndArray});
end;

{**************************}
Procedure ALJSONToTStringsA(
            const AJsonStr: AnsiString;
            const aFormatSettings: TALFormatSettingsA;
            const aLst: TALStringsA;
            Const aNullStr: AnsiString = 'null';
            Const aTrueStr: AnsiString = 'true';
            Const aFalseStr: AnsiString = 'false');
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
Procedure ALJSONToTStringsA(
            const aJsonNode: TALJSONNodeA;
            Const aPath: AnsiString;
            const aLst: TALStringsA;
            Const aNullStr: AnsiString = 'null';
            Const aTrueStr: AnsiString = 'true';
            Const aFalseStr: AnsiString = 'false');
var LTmpPath: AnsiString;
    I: integer;
begin
  if aJsonNode.ChildNodes.Count > 0 then begin
    for I := 0 to aJsonNode.ChildNodes.Count - 1 do begin

      if aJsonNode.NodeType = ntArray then LTmpPath := aPath + '[' + ALIntToStrA(I) + ']'
      else begin
        if aJsonNode.ChildNodes[I].NodeName = '' then raise Exception.Create('Nodename can not be empty');
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

        else raise Exception.Create('Unknown NodeType');

      end;
    end;
  end
  else if (aPath <> ''{aPath = '' mean it's the root object}) then begin
    if      aJsonNode.NodeType = ntArray  then aLst.Add(aPath + aLst.NameValueSeparator + '[]')
    else if aJsonNode.NodeType = ntObject then aLst.Add(aPath + aLst.NameValueSeparator + '{}');
  end;
end;

{**************************}
Procedure ALJSONToTStringsA(
            const aJsonNode: TALJSONNodeA;
            const aLst: TALStringsA;
            Const aNullStr: AnsiString = 'null';
            Const aTrueStr: AnsiString = 'true';
            Const aFalseStr: AnsiString = 'false');
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
            const aLst: TALStringsA;
            const aJsonNode: TALJSONNodeA;
            Const aPath: AnsiString = '';
            Const aNameToLowerCase: boolean = false;
            Const aNullStr: AnsiString = 'null');

var LIndex: Integer;
    LNames:  TALStringListA;
    LLowerName: AnsiString;
    LCurrJsonNode, LTmpJsonNode: TALJSONNodeA;
    I, J: integer;

begin

  // create list of the part of name,
  // from "aggregated_data.properties.types[3].translations.usa" =>
  //   aggregated_data
  //   properties
  //   types
  //   [3]
  //   translations
  //   usa
  LNames := TALStringListA.Create;
  try

    //init aNames.linebreak
    LNames.LineBreak := '.';

    // scroll the aLst
    for I := 0 to aLst.Count - 1 do begin

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
        LCurrJsonNode := aJsonNode;
        for J := 0 to LNames.Count - 1 do begin

          //if we are in array
          if LCurrJsonNode.NodeType = ntArray then begin
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
            LLowerName := alifThenA(aNameToLowerCase, allowercase(LNames[J]), LNames[J]);
            LTmpJsonNode := LCurrJsonNode.ChildNodes.FindNode(LLowerName);
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

{*********************}
Procedure ALJSONToXMLA(
            const aJSONNode: TALJSONNodeA;
            const aXMLNode: TALXmlNode;
            const aXMLElementNameForJSONArrayEntries: TALStringsA; // JSONArrayNodeName=XMLElementName
            const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec');
var LNodeName: AnsiString;
    I: integer;
begin
  for I := 0 to aJSONNode.ChildNodes.Count - 1 do begin

    if (aJSONNode.NodeType = ntarray) then begin
      if assigned(aXMLElementNameForJSONArrayEntries) then LNodeName := aXMLElementNameForJSONArrayEntries.Values[aJSONNode.NodeName]
      else LNodeName := '';
      if LNodeName = '' then LNodeName := aDefaultXMLElementNameForJSONArrayEntries;
    end
    else LNodeName := aJSONNode.ChildNodes[I].NodeName;

    if aJSONNode.ChildNodes[I].NodeType = ntText then aXMLNode.AddChild(LNodeName).text := aJSONNode.ChildNodes[I].text
    else ALJSONToXMLA(aJSONNode.ChildNodes[I], aXMLNode.AddChild(LNodeName));

  end;
end;

{*********************}
Procedure ALJSONToXMLA(
            const aJSONNode: TALJSONNodeA;
            const aXMLNode: TALXmlNode;
            const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec');
begin
  ALJSONToXMLA(
    aJSONNode,
    aXMLNode,
    nil,
    aDefaultXMLElementNameForJSONArrayEntries);
end;

{*********************************************************************************}
function ALJsonEncodeFloatWithNodeSubTypeHelperA(const aValue: double): AnsiString;
begin
  result := ALFloatToStrA(aValue, ALDefaultFormatSettingsA);
end;

{************************************************************************************}
function ALJsonEncodeTextWithNodeSubTypeHelperA(const aValue: AnsiString): AnsiString;
begin
  result := '"'+ALJavascriptEncode(aValue)+'"';
end;

{**************************************************************************************}
function ALJsonEncodeBinaryWithNodeSubTypeHelperA(const aValue: AnsiString): AnsiString;
begin
  result := 'BinData(0, "' + ALBase64EncodeString(aValue) + '")';
end;

{****************************************************************************************}
function ALJsonEncodeObjectIDWithNodeSubTypeHelperA(const aValue: AnsiString): AnsiString;
begin
  result := 'ObjectId("'+ALBinToHexA(aValue)+'")';
end;

{************************************************************************************}
function ALJsonEncodeBooleanWithNodeSubTypeHelperA(const aValue: Boolean): AnsiString;
begin
  if aValue then result := 'true'
  else result := 'false';
end;

{***************************************************************************************}
function ALJsonEncodeDateTimeWithNodeSubTypeHelperA(const aValue: TdateTime): AnsiString;
begin
  result := ALFormatDateTimeA('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', aValue, ALDefaultFormatSettingsA);
end;

{******************************************************************************************}
function ALJsonEncodeJavascriptWithNodeSubTypeHelperA(const aValue: AnsiString): AnsiString;
begin
  result := aValue;
end;

{********************************************************************************}
function ALJsonEncodeInt64WithNodeSubTypeHelperA(const aValue: int64): AnsiString;
begin
  result := 'NumberLong(' + ALIntToStrA(aValue) + ')';
end;

{********************************************************************************}
function ALJsonEncodeInt32WithNodeSubTypeHelperA(const aValue: int32): AnsiString;
begin
  result := 'NumberInt(' + ALIntToStrA(aValue) + ')';
end;

{**********************************************************}
function ALJsonEncodeNullWithNodeSubTypeHelperA: AnsiString;
begin
  result := 'null';
end;

{******************************************}
function ALJsonEncodeWithNodeSubTypeHelperA(
           const aValue: AnsiString;
           const aNodeSubType: TALJSONNodeSubType;
           const aFormatSettings: TALFormatSettingsA): AnsiString;
begin
  case aNodeSubType of
    nstFloat:      begin
                     if @aFormatSettings <> @ALDefaultFormatSettingsA then result := ALJsonEncodeFloatWithNodeSubTypeHelperA(ALStrToFloat(aValue, aFormatSettings))
                     else result := aValue;
                   end;
    nstText:       result := ALJsonEncodeTextWithNodeSubTypeHelperA(aValue);
    nstBinary:     result := ALJsonEncodeBinaryWithNodeSubTypeHelperA(aValue);
    nstObjectID:   result := ALJsonEncodeObjectIDWithNodeSubTypeHelperA(aValue);
    nstBoolean:    result := ALJsonEncodeBooleanWithNodeSubTypeHelperA(ALStrToBool(aValue));
    nstDateTime:   begin
                     if aValue = 'NOW' then result := ALJsonEncodeDateTimeWithNodeSubTypeHelperA(ALUtcNow)
                     else result := ALJsonEncodeDateTimeWithNodeSubTypeHelperA(ALStrToDateTime(aValue, aFormatSettings));
                   end;
    nstJavascript: result := ALJsonEncodeJavascriptWithNodeSubTypeHelperA(aValue);
    nstInt32:      result := ALJsonEncodeInt32WithNodeSubTypeHelperA(ALstrToInt(aValue));
    nstInt64:      result := ALJsonEncodeInt64WithNodeSubTypeHelperA(ALstrToInt64(aValue));
    nstNull:       result := ALJsonEncodeNullWithNodeSubTypeHelperA;
    nstObject:     raise Exception.Create('Unsupported Node SubType');
    nstArray:      raise Exception.Create('Unsupported Node SubType');
    nstRegEx:      raise Exception.Create('Unsupported Node SubType');
    nstTimestamp:  raise Exception.Create('Unsupported Node SubType');
    else raise Exception.Create('Unknown Node SubType');
  end;
end;

{********************************************}
Function ALFindJsonNodeByInt32ChildNodeValueW(
           const JsonNode:TALJSONNodeW;
           Const ChildNodeName: String;
           Const ChildNodeValue : Int32;
           Const Recurse: Boolean = False): TALJSONNodeW;
var I, J : integer;
Begin
  result := nil;
  if not (JsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for I := 0 to JsonNode.ChildNodes.Count - 1 do begin
    for J := 0 to JsonNode.ChildNodes[I].ChildNodes.Count - 1 do begin
      If (JsonNode.ChildNodes[I].ChildNodes[j].NodeType = nttext) and
         (JsonNode.ChildNodes[I].ChildNodes[j].NodesubType = nstint32) and
         (ALSameTextW(JsonNode.ChildNodes[I].ChildNodes[j].NodeName, ChildNodeName)) and
         (JsonNode.ChildNodes[I].ChildNodes[j].int32 = ChildNodeValue) then begin
        result := JsonNode.ChildNodes[I];
        exit;
      end;
    end;
    if Recurse then begin
      result := ALFindJsonNodeByInt32ChildNodeValueW(
                  JsonNode.ChildNodes[I],
                  ChildNodeName,
                  ChildNodeValue,
                  Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{*******************************************}
Function ALFindJsonNodeByTextChildNodeValueW(
           const JsonNode:TALJSONNodeW;
           Const ChildNodeName: String;
           Const ChildNodeValue : String;
           Const Recurse: Boolean = False): TALJSONNodeW;
var I, J : integer;
Begin
  result := nil;
  if not (JsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for I := 0 to JsonNode.ChildNodes.Count - 1 do begin
    for J := 0 to JsonNode.ChildNodes[I].ChildNodes.Count - 1 do begin
      If (JsonNode.ChildNodes[I].ChildNodes[j].NodeType = nttext) and
         (JsonNode.ChildNodes[I].ChildNodes[j].NodesubType = nstText) and
         (ALSameTextW(JsonNode.ChildNodes[I].ChildNodes[j].NodeName, ChildNodeName)) and
         (JsonNode.ChildNodes[I].ChildNodes[j].text = ChildNodeValue) then begin
        result := JsonNode.ChildNodes[I];
        exit;
      end;
    end;
    if Recurse then begin
      result := ALFindJsonNodeByTextChildNodeValueW(
                  JsonNode.ChildNodes[I],
                  ChildNodeName,
                  ChildNodeValue,
                  Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
function ALJSONTryStrToRegExW(const S: String; out RegEx: String; out RegExOptions: TALPerlRegExOptions): boolean;
var P1: integer;
    I: integer;
begin

  // regular expression in JSON must look like: /pattern/options
  // list of valid options is:
  //  'i' for case insensitive matching,
  //  'm' for multiline matching,
  //  'x' for verbose mode,
  //  'l' to make \w, \W, etc. locale dependent,
  //  's' for dotall mode ('.' matches everything),
  //  'u' to make \w, \W, etc. match unicode.
  result := false;

  // check that first character is /
  if (S <> '') and (S[1] = '/') then begin

    P1 := ALLastDelimiterW('/', S);
    if P1 <> 1 then begin

      //init Value
      RegEx := ALCopyStr(S, 2, P1 - 2);
      RegExOptions := [];

      // loop on all the options characters
      // to check if they are allowed.
      for I := P1 + 1 to Length(S) do
        case s[I] of
          'i': RegExOptions := RegExOptions + [preCaseLess];
          'm': RegExOptions := RegExOptions + [preMultiLine];
          'x': RegExOptions := RegExOptions + [preExtended];
          'l':;
          's': RegExOptions := RegExOptions + [preSingleLine];
          'u':;
          else exit;
        end;

      //set the result to true
      result := true;

      // check if it's compiling
      //aRegEx := TALPerlRegEx.Create;
      //try
      //  aRegEx.RegEx := Value.Expression;
      //  result := aRegEx.Compile(false{RaiseException});
      //finally
      //  ALFreeAndNil(aRegEx);
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
function ALJSONTryStrToBinaryW(const S: String; out Data: String; out Subtype: byte): boolean;
var LInt: integer;
    Ln: integer;
    P1, P2: integer;
begin

  // s must look like
  // BinData(0, "JliB6gIMRuSphAD2KmhzgQ==")
  // BinData ( 0 , "JliB6gIMRuSphAD2KmhzgQ==" )
  result := false;
  Ln := length(s);
  P1 := 1;

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

  P2 := P1;
  while (P2 <= ln) and (S[P2] in ['0'..'9']) do inc(P2); // BinData( 0 , "JliB6gIMRuSphAD2KmhzgQ==")
                                                         //           ^P2
  if P2 > ln then exit;
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
  Data := ALCopyStr(s, P1, P2-P1); // notmally i would like to do ALBase64DecodeString()
                                    // and return in data the byte string but this is not possible
                                    // because the source byte array is probably not a multiple of 2
                                    // and unicode string is obligatory a multiple of 2

  // set the result
  result := true;

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToDateTimeW(const S: String; out Value: TDateTime): Boolean;
var LQuoteChar: Char;
    LTmpStr: String;
    LTmpLn: integer;
    P1, P2: integer;
    Ln: integer;
begin

  // s must look like
  // new Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // Date('yyyy-mm-ddThh:nn:ss.zzzZ')
  // new ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  // ISODate('yyyy-mm-ddThh:nn:ss.zzzZ')
  result := false;
  Ln := length(s);
  if ALPosW('new', s) = 1 then P1 := 4{length('new') + 1} // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                                                         //    ^P1
  else P1 := 1;// Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
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
  LQuoteChar := S[P1]; // "
  inc(p1); // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
           //              ^P1
  P2 := P1;
  while (P1 <= ln) and (S[P1] <> LQuoteChar) do inc(P1);
  if (P1 > ln) then exit; // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
                          //                                      ^P1
  dec(P1);
  if S[P1] <> 'Z' then exit;
  LTmpStr := ALCopyStr(S,P2,P1-P2); // yyyy-mm-ddThh:nn:ss.zzz

  P2 := 1;
  LTmpLn := length(LTmpStr);
  while (P2 <= LTmpLn) and (LTmpStr[P2] <> 'T') do inc(P2);
  if P2 > LTmpLn then exit;
  LTmpStr[P2] := ' '; // yyyy-mm-dd hh:nn:ss.zzz

  result := ALTryStrToDateTime(LTmpStr, Value, ALJsonISODateFormatSettingsW);
  if not result then exit;

  inc(p1,2);  // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
              //                                       ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> ln) or (S[P1] <> ')') then begin
    result := false;
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
// ObjectId is a 12-byte BSON type, constructed using:
// a 4-byte value representing the seconds since the Unix epoch,
// a 3-byte machine identifier,
// a 2-byte process id, and
// a 3-byte counter, starting with a random value.
function ALJSONTryStrToObjectIDW(const S: String; out Value: String): Boolean;
var LBinValue: Tbytes;
    LQuoteChar: Char;
    P1: integer;
    Ln: integer;
begin

  // s must look like
  // ObjectId ( "507f1f77bcf86cd799439011" )
  result := false;
  if ALPosW('ObjectId', S) <> 1 then exit;
  Ln := length(s);
  P1 := 9{length('ObjectId') + 1}; // ObjectId ( "507f1f77bcf86cd799439011" )
                                   //         ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                            //          ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //           ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (not (S[P1] in ['''','"'])) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                         //            ^P1
  LQuoteChar := S[P1]; // "
  inc(p1); // ObjectId ( "507f1f77bcf86cd799439011" )
           //             ^P1
  if (P1 + 23{(length(aObjectIDhex)) - 1} > ln) then exit;
  Value := ALCopyStr(S,P1,24{length(aObjectIDhex)}); // 507f1f77bcf86cd799439011
  inc(P1, 24{length(aObjectIDhex)}); // ObjectId ( "507f1f77bcf86cd799439011" )
                                     //                                     ^P1
  if (P1 > ln) or (S[P1] <> LQuoteChar) then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                                   //                                     ^P1
  inc(p1);  // ObjectId ( "507f1f77bcf86cd799439011" )
            //                                      ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 <> ln) or (S[P1] <> ')') then exit; // ObjectId ( "507f1f77bcf86cd799439011" )
                                             //                                       ^P1
  //check that 507f1f77bcf86cd799439011 is a good hex value
  result := ALTryHexToBin(Value, LBinValue) and
            (length(LBinValue) = 12);

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToTimestampW(const S: String; out Value: TALBSONTimestamp): Boolean;
var P1, P2: integer;
    LArgs: String;
    LArg1: integer;
    LArg2: integer;
    Ln: integer;
begin

  // s must look like
  // Timestamp(0, 0)
  result        := false;
  if ALPosW('Timestamp', S) <> 1 then Exit;
  Ln := length(s);
  P1 := 10{Length('Timestamp') + 1}; // Timestamp(0, 0)
                                     //          ^
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // Timestamp(0, 0)
                                            //          ^P1
  P2 := ALPosW(')', S, P1);
  if P2 <> ln then exit; // Timestamp(0, 0)
                         //               ^P2
  LArgs := ALCopyStr(S, P1+1, P2 - P1-1); // 0, 0

  // take arguments of function Timestamp
  P1 := ALPosW(',', LArgs);
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, 1,      P1 - 1)), LArg1) then Exit;
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, P1 + 1, maxint)), LArg2) then Exit;

  // build result
  result := true;
  Value.W1 := LArg1; // higher 4 bytes - increment
  Value.W2 := LArg2; // lower  4 bytes - timestamp

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToInt32W(const S: String; out Value: integer): Boolean;
var LTmpStr: String;
    LQuoteChar: Char;
    P1, P2: integer;
    Ln: integer;
begin

  // s must look like
  // NumberInt ( "12391293" )
  // NumberInt ( 12391293 )
  // 12391293
  result := ALTryStrToInt(S, Value);
  if result then exit;
  if ALPosW('NumberInt', S) <> 1 then exit;
  Ln := length(s);
  P1 := 10{length('NumberInt') + 1}; // NumberInt ( "12391293" )
                                     //          ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // NumberInt ( "12391293" )
                                            //           ^P1
  inc(p1);  // NumberInt ( "12391293" )
            //            ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) then exit
  else if (not (S[P1] in ['''','"'])) then begin // NumberInt ( 12391293 )
                                                 //             ^P1
    P2 := P1+1;
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

    LQuoteChar := S[P1]; // "
    inc(p1); // NumberInt ( "12391293" )
             //              ^P1
    P2 := P1;
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

  //convert 12391293 to integer
  result := ALTryStrToInt(LTmpStr, Value);

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToInt64W(const S: String; out Value: int64): Boolean;
var LTmpStr: String;
    LQuoteChar: Char;
    P1, P2: integer;
    Ln: integer;
begin

  // s must look like
  // NumberLong ( "12391293" )
  // NumberLong ( 12391293 )
  // 12391293
  result := ALTryStrToInt64(S, Value);
  if result then exit;
  if ALPosW('NumberLong', S) <> 1 then exit;
  Ln := length(s);
  P1 := 11{length('NumberLong') + 1}; // NumberLong ( "12391293" )
                                      //           ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // NumberLong ( "12391293" )
                                            //            ^P1
  inc(p1);  // NumberLong ( "12391293" )
            //             ^P1
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) then exit
  else if (not (S[P1] in ['''','"'])) then begin // NumberLong ( 12391293 )
                                                 //              ^P1
    P2 := P1+1;
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

    LQuoteChar := S[P1]; // "
    inc(p1); // NumberLong ( "12391293" )
             //               ^P1
    P2 := P1;
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

  //convert 12391293 to integer
  result := ALTryStrToInt64(LTmpStr, Value);

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*****************************************************}
procedure ALJSONDocErrorW(const Msg: String); overload;
begin
  raise EALJSONDocError.Create(Msg);
end;

{*********************************************************************************}
procedure ALJSONDocErrorW(const Msg: String; const Args: array of const); overload;
begin
  raise EALJSONDocError.CreateFmt(Msg, Args);
end;

{**************************************************************************************}
procedure ALJSONDocErrorW(const Msg: String; const NodeType: TalJsonNodeType); overload;
begin
  case NodeType of
    ntObject: ALJSONDocErrorW(Msg, ['ntObject']);
    ntArray: ALJSONDocErrorW(Msg, ['ntArray']);
    ntText: ALJSONDocErrorW(Msg, ['ntText']);
    else ALJSONDocErrorW(cAlJSONInvalidNodeType);
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
    else begin
      Result := nil; //for hide warning
      ALJSONDocErrorW(cAlJSONInvalidNodeType);
    end;
  end;
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
class function TALJSONDocumentW.DetectNodeTypeFromJSon(const Buffer: String): TALJSONNodeType;
Var BufferLength: Integer;
    BufferPos: Integer;
    c: Char;
Begin

  //--
  result := ntText;

  //--
  BufferLength := length(Buffer);
  BufferPos := 1;

  //--
  While (BufferPos <= BufferLength) do begin
    c := Buffer[BufferPos];
    If c <= ' ' then inc(bufferPos)
    else begin
      if c = '{' then result := ntObject
      else if c = '[' then result := ntarray
      else result := ntText;
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
  result := ALCreateJSONNodeW('', ntObject);
end;

{**********************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromJSONString(const Str: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  var LNodeType := DetectNodeTypeFromJSON(Str);
  if LNodeType in [ntObject, ntArray] then result := ALCreateJSONNodeW('', LNodeType)
  else AlJSONDocErrorW(cALJSONParseError);
  try
    result.LoadFromJSONString(Str, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  result := CreateFromJSONString(ALGetStringFromStream(Stream, TEncoding.UTF8), Options);
end;

{*************************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    Result := CreateFromJSONStream(LfileStream, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{***********************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromBSONBytes(const Bytes: Tbytes; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  result := ALCreateJSONNodeW('', ntObject);
  try
    result.LoadFromBSONBytes(Bytes, Options);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{**************************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
begin
  result := CreateFromBSONBytes(ALGetBytesFromStream(Stream), Options);
end;

{*************************************************************************************************************************************************}
class function TALJSONDocumentW.CreateFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]): TALJSONNodeW;
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    result := CreateFromBSONStream(LfileStream, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{***********************************************}
class procedure TALJSONDocumentW.ParseJSONString(
                  const Str: String;
                  const onParseText: TAlJSONParseTextEventW;
                  const onParseStartObject: TAlJSONParseObjectEventW;
                  const onParseEndObject: TAlJSONParseObjectEventW;
                  const onParseStartArray: TAlJSONParseArrayEventW;
                  const onParseEndArray: TAlJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
begin
  var LJsonNode: TALJsonNodeW;
  var LNodeType := DetectNodeTypeFromJSON(Str);
  if LNodeType in [ntObject, ntArray] then LJsonNode := ALCreateJSONNodeW('', LNodeType)
  else AlJSONDocErrorW(cALJSONParseError);
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
                  const onParseText: TAlJSONParseTextEventW;
                  const onParseStartObject: TAlJSONParseObjectEventW;
                  const onParseEndObject: TAlJSONParseObjectEventW;
                  const onParseStartArray: TAlJSONParseArrayEventW;
                  const onParseEndArray: TAlJSONParseArrayEventW;
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
                  const onParseText: TAlJSONParseTextEventW;
                  const onParseStartObject: TAlJSONParseObjectEventW;
                  const onParseEndObject: TAlJSONParseObjectEventW;
                  const onParseStartArray: TAlJSONParseArrayEventW;
                  const onParseEndArray: TAlJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseJSONStream(
      LfileStream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{**********************************************}
class procedure TALJSONDocumentW.ParseBSONBytes(
                  const Bytes: Tbytes;
                  const onParseText: TAlJSONParseTextEventW;
                  const onParseStartObject: TAlJSONParseObjectEventW;
                  const onParseEndObject: TAlJSONParseObjectEventW;
                  const onParseStartArray: TAlJSONParseArrayEventW;
                  const onParseEndArray: TAlJSONParseArrayEventW;
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
                  const onParseText: TAlJSONParseTextEventW;
                  const onParseStartObject: TAlJSONParseObjectEventW;
                  const onParseEndObject: TAlJSONParseObjectEventW;
                  const onParseStartArray: TAlJSONParseArrayEventW;
                  const onParseEndArray: TAlJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
begin
  ParseBSONBytes(
    ALGetBytesFromStream(Stream),
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
                  const onParseText: TAlJSONParseTextEventW;
                  const onParseStartObject: TAlJSONParseObjectEventW;
                  const onParseEndObject: TAlJSONParseObjectEventW;
                  const onParseStartArray: TAlJSONParseArrayEventW;
                  const onParseEndArray: TAlJSONParseArrayEventW;
                  const Options: TALJSONParseOptions = []);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseBSONStream(
      LfileStream,
      OnParseText,
      OnParseStartObject,
      OnParseEndObject,
      OnParseStartArray,
      OnParseEndArray,
      Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{**********************************************************}
{Creates the object that implements the ChildNodes property}
function TALJSONNodeW.CreateChildList: TALJSONNodeListW;
begin
  result := TALJSONNodeListW.Create(Self);
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONNodeW.InternalGetChildNodes: TALJSONNodeListW;
begin
  Result := nil; //virtual;
end;

{****************************************************}
function TALJSONNodeW.GetChildNodes: TALJSONNodeListW;
begin
  Result := nil; // hide warning
  ALJSONDocErrorW(CALJsonOperationError,GetNodeType)
end;

{******************************************************************}
procedure TALJSONNodeW.SetChildNodes(const Value: TALJSONNodeListW);
begin
  ALJSONDocErrorW(CALJsonOperationError,GetNodeType)
end;

{***********************************************************************}
function TALJSONNodeW.GetChildNode(const nodeName: String): TALJSONNodeW;
begin
  result := ChildNodes.findNode(nodeName);
end;

{*************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueText(const nodeName: String; const default: String): String;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetText(default);
end;

{**************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueFloat(const nodeName: String; const default: Double): Double;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetFloat(default);
end;

{***********************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueDateTime(const nodeName: String; const default: TDateTime): TDateTime;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetDateTime(default);
end;

{**************************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueTimestamp(const nodeName: String; const default: TALBSONTimestamp): TALBSONTimestamp;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetTimestamp(default);
end;

{****************************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueObjectID(const nodeName: String; const default: String): String; // return a hex string
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetObjectID(default);
end;

{****************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueInt32(const nodeName: String; const default: Integer): Integer;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetInt32(default);
end;

{************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueInt64(const nodeName: String; const default: Int64): Int64;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetInt64(default);
end;

{***************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueBool(const nodeName: String; const default: Boolean): Boolean;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBool(default);
end;

{*******************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueJavascript(const nodeName: String; const default: String): String;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetJavascript(default);
end;

{**************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueRegEx(const nodeName: String; const default: String): String;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetRegEx(default);
end;

{***********************************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueRegExOptions(const nodeName: String; const default: TALPerlRegExOptions): TALPerlRegExOptions;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetRegExOptions(default);
end;

{**************************************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueBinary(const nodeName: String; const default: String): String;  // return a base64 encoded string
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBinary(default);
end;

{******************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueBinarySubType(const nodeName: String; const default: byte): byte;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBinarySubType(default);
end;

{***************************************************************************}
function TALJSONNodeW.GetChildNodeValueNull(const nodeName: String): Boolean;
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := true
  else result := LNode.GetNull;
end;

{****************************************************************************}
function TALJSONNodeW.GetChildNode(const path: array of String): TALJSONNodeW;
var I: integer;
begin
  result := Self;
  for I := low(path) to high(path) do begin
    result := result.ChildNodes.findNode(path[I]);
    if (result = nil) then exit;
  end;
end;

{******************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueText(const path: array of String; const default: String): String;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetText(default);
end;

{*******************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueFloat(const path: array of String; const default: Double): Double;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetFloat(default);
end;

{****************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueDateTime(const path: array of String; const default: TDateTime): TDateTime;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetDateTime(default);
end;

{*******************************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueTimestamp(const path: array of String; const default: TALBSONTimestamp): TALBSONTimestamp;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetTimestamp(default);
end;

{*********************************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueObjectID(const path: array of String; const default: String): String; // return a hex string
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetObjectID(default);
end;

{*********************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueInt32(const path: array of String; const default: Integer): Integer;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetInt32(default);
end;

{*****************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueInt64(const path: array of String; const default: Int64): Int64;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetInt64(default);
end;

{********************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueBool(const path: array of String; const default: Boolean): Boolean;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetBool(default);
end;

{************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueJavascript(const path: array of String; const default: String): String;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetJavascript(default);
end;

{*******************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueRegEx(const path: array of String; const default: String): String;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetRegEx(default);
end;

{****************************************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueRegExOptions(const path: array of String; const default: TALPerlRegExOptions): TALPerlRegExOptions;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetRegExOptions(default);
end;

{*******************************************************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueBinary(const path: array of String; const default: String): String;  // return a base64 encoded string
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetBinary(default);
end;

{***********************************************************************************************************}
function TALJSONNodeW.GetChildNodeValueBinarySubType(const path: array of String; const default: byte): byte;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := default;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := default
  else result := LNode.GetBinarySubType(default);
end;

{********************************************************************************}
function TALJSONNodeW.GetChildNodeValueNull(const path: array of String): Boolean;
var LNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LNode := LNode.ChildNodes.findNode(path[I]);
    if (LNode = nil) then begin
      result := True;
      exit;
    end;
  end;
  LNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LNode = nil) then result := true
  else result := LNode.GetNull;
end;

{****************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueText(const nodeName: String; const value: String);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetText(value)
  else LNode.SetText(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueFloat(const nodeName: String; const value: Double);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetFloat(value)
  else LNode.SetFloat(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueDateTime(const nodeName: String; const value: TDateTime);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetDateTime(value)
  else LNode.SetDateTime(value);
end;

{*******************************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueTimestamp(const nodeName: String; const value: TALBSONTimestamp);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetTimestamp(value)
  else LNode.SetTimestamp(value);
end;

{********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueObjectID(const nodeName: String; const value: String);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetObjectID(value)
  else LNode.SetObjectID(value);
end;

{******************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueInt32(const nodeName: String; const value: Integer);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt32(value)
  else LNode.SetInt32(value);
end;

{****************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueInt64(const nodeName: String; const value: Int64);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt64(value)
  else LNode.SetInt64(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueBool(const nodeName: String; const value: Boolean);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBool(value)
  else LNode.SetBool(value);
end;

{**********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueJavascript(const nodeName: String; const value: String);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetJavascript(value)
  else LNode.SetJavascript(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueRegEx(const nodeName: String; const value: String);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegEx(value)
  else LNode.SetRegEx(value);
end;

{*************************************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueRegExOptions(const nodeName: String; const value: TALPerlRegExOptions);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegExOptions(value)
  else LNode.SetRegExOptions(value);
end;

{******************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueBinary(const nodeName: String; const value: String);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinary(value)
  else LNode.SetBinary(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueBinarySubType(const nodeName: String; const value: byte);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinarySubType(value)
  else LNode.SetBinarySubType(value);
end;

{*******************************************************************}
procedure TALJSONNodeW.SetChildNodeValueNull(const nodeName: String);
var LNode: TALJSONNodeW;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetNull(true)
  else LNode.SetNull(true);
end;

{*********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueText(const path: array of String; const value: String);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetText(value)
  else LTmpNode.SetText(value);
end;

{**********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueFloat(const path: array of String; const value: Double);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetFloat(value)
  else LTmpNode.SetFloat(value);
end;

{****************************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueDateTime(const path: array of String; const value: TDateTime);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetDateTime(value)
  else LTmpNode.SetDateTime(value);
end;

{************************************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueTimestamp(const path: array of String; const value: TALBSONTimestamp);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetTimestamp(value)
  else LTmpNode.SetTimestamp(value);
end;

{*************************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueObjectID(const path: array of String; const value: String);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetObjectID(value)
  else LTmpNode.SetObjectID(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueInt32(const path: array of String; const value: Integer);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetInt32(value)
  else LTmpNode.SetInt32(value);
end;

{*********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueInt64(const path: array of String; const value: Int64);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetInt64(value)
  else LTmpNode.SetInt64(value);
end;

{**********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueBool(const path: array of String; const value: Boolean);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBool(value)
  else LTmpNode.SetBool(value);
end;

{***************************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueJavascript(const path: array of String; const value: String);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetJavascript(value)
  else LTmpNode.SetJavascript(value);
end;

{**********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueRegEx(const path: array of String; const value: String);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetRegEx(value)
  else LTmpNode.SetRegEx(value);
end;

{******************************************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueRegExOptions(const path: array of String; const value: TALPerlRegExOptions);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetRegExOptions(value)
  else LTmpNode.SetRegExOptions(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueBinary(const path: array of String; const value: String);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinary(value)
  else LTmpNode.SetBinary(value);
end;

{****************************************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueBinarySubType(const path: array of String; const value: byte);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetBinarySubType(value)
  else LTmpNode.SetBinarySubType(value);
end;

{************************************************************************}
procedure TALJSONNodeW.SetChildNodeValueNull(const path: array of String);
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  LTmpNode := LNode.ChildNodes.findNode(path[high(path)]);
  if (LTmpNode = nil) then LNode.addChild(path[high(path)]).SetNull(true)
  else LTmpNode.SetNull(true);
end;

{***********************************************}
{Indicates whether this node has any child nodes}
function TALJSONNodeW.GetHasChildNodes: Boolean;
Var LNodeList: TALJSONNodeListW;
begin
  LNodeList := InternalGetChildNodes;
  Result := assigned(LNodeList) and (LNodeList.Count > 0);
end;

{********************************************}
function TALJSONNodeW.GetNodeValueStr: String;
begin
  ALJSONDocErrorW(CALJsonOperationError,GetNodeType);
  result := ''; // hide warning
end;

{*********************************************}
function TALJSONNodeW.GetNodeValueInt64: int64;
begin
  ALJSONDocErrorW(CALJsonOperationError,GetNodeType);
  result := 0; // hide warning
end;

{**********************************************************************************************}
procedure TALJSONNodeW.SetNodeValue(const Value: String; const NodeSubType: TALJSONNodeSubType);
begin
  ALJSONDocErrorW(CALJsonOperationError,GetNodeType);
end;

{*********************************************************************************************}
procedure TALJSONNodeW.SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  ALJSONDocErrorW(CALJsonOperationError,GetNodeType);
end;

{**************************************************************************************************************************}
procedure TALJSONNodeW.SetNodeValue(const StrValue: String; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  ALJSONDocErrorW(CALJsonOperationError,GetNodeType);
end;

{*********************************************************}
procedure TALJSONNodeW.SetNodeName(const NodeName: String);
begin
  if fNodeName <> NodeName then begin
    fNodeName := NodeName;
    Var LParentNode := FParentNode;
    if (LParentNode <> nil) and (LParentNode.ChildNodes.Sorted) then begin
      var LNode := LParentNode.ChildNodes.Extract(self);
      Try
        LParentNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      End;
    end;
  end;
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONNodeW.GetText: String;
begin

  case NodeSubType of
    nstFloat: result := GetNodeValueStr; // return the formated float
    nstText: result := GetNodeValueStr;  // return the raw text
    nstObject: result := GetNodeValueStr;  // return the raw objectID
    nstArray: result := GetNodeValueStr;  // error
    nstObjectID: result := GetNodeValueStr; // error
    nstBoolean: result := GetNodeValueStr;  // return true or false
    nstDateTime: result := GetNodeValueStr; // return the formated datetime
    nstNull: result := GetNodeValueStr; // return null
    nstRegEx: result := GetNodeValueStr; // return the raw regex (without the options)
    nstBinary: result := GetNodeValueStr; // return the base64 encoded binary (without the binary subtype)
    nstJavascript: result := GetNodeValueStr; // return the raw javascript
    nstInt32: result := GetNodeValueStr;  // return the number
    nstTimestamp: result := GetNodeValueStr;  // return the number (as int64)
    nstInt64: result := GetNodeValueStr;  // return the number
    else ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
  end;

end;

{***********************************************************}
function TALJSONNodeW.GetText(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetText;
end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONNodeW.SetText(const Value: String);
begin
  setNodeValue(Value, nstText);
end;

{******************************************************************************}
// By default json (ie: javascript) treats all numbers as floating-point values.
// To let other system (ie: mongoDB) understand the type of the number
// we provide the helper functions NumberLong() to handle 64-bit integers
// and NumberInt() to handle 32-bit integers (and some others). theses helper functions are
// used when saving the json document.
function TALJSONNodeW.GetNodeValueInterchange(const SkipNodeSubTypeHelper: boolean = False): String;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetObjectID;
  begin
    if SkipNodeSubTypeHelper then result := '"'+ObjectID+'"'
    else result := 'ObjectId("'+ObjectID+'")';
  end;

  {~~~~~~~~~~~~~~~~~~~}
  procedure _GetBinary;
  begin
    if SkipNodeSubTypeHelper then result := '"'+Binary+'"'
    else result := 'BinData('+ALIntToStrW(BinarySubType)+', "'+Binary+'")';
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetDateTime;
  begin
    if SkipNodeSubTypeHelper then result := ALFormatDateTimeW('''"''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z"''', DateTime, ALDefaultFormatSettingsW)
    else result := ALFormatDateTimeW('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', DateTime, ALDefaultFormatSettingsW)
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _Getint32;
  begin
    if SkipNodeSubTypeHelper then result := text
    else result := 'NumberInt(' + text + ')'
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _Getint64;
  begin
    if SkipNodeSubTypeHelper then result := text
    else result := 'NumberLong(' + text + ')';
  end;

  {~~~~~~~~~~~~~~~~~~}
  procedure _GetRegEx;
  var LRegExOptions: TALPerlRegExOptions;
      LRegExOptionsStr: String;
  begin
    LRegExOptionsStr := '';
    LRegExOptions := RegExOptions;
    if preCaseLess in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 'i';
    if preMultiLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'m';
    if preExtended in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr +'x';
    //'l':;
    if preSingleLine in LRegExOptions then LRegExOptionsStr := LRegExOptionsStr + 's';
    //'u':;
    result := '/'+regex+'/' + LRegExOptionsStr;
    if not SkipNodeSubTypeHelper then result := '"' + ALJavascriptEncode(result) + '"'
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _GetTimestamp;
  begin
    if SkipNodeSubTypeHelper then result := '"Timestamp('+ALIntToStrW(GetTimeStamp.W1)+', '+ALIntToStrW(GetTimeStamp.W2)+')"'
    else result := 'Timestamp('+ALIntToStrW(GetTimeStamp.W1)+', '+ALIntToStrW(GetTimeStamp.W2)+')';
  end;

begin

  case NodeSubType of
    nstFloat:      result := GetNodeValueStr;
    nstText:       result := GetNodeValueStr;
    nstBinary:     _GetBinary;
    nstObjectID:   _GetObjectID;
    nstBoolean:    result := GetNodeValueStr;
    nstDateTime:   _GetDateTime;
    nstJavascript: result := GetNodeValueStr;
    nstInt32:      _Getint32;
    nstInt64:      _Getint64;
    nstNull:       result := GetNodeValueStr;
    nstObject:     result := GetNodeValueStr;
    nstArray:      result := GetNodeValueStr;
    nstRegEx:      _GetRegEx;
    nstTimestamp:  _GetTimestamp;
    else raise Exception.Create('Unknown Node SubType');
  end;

end;

{*************************************}
function TALJSONNodeW.GetFloat: Double;
begin
  case NodeSubType of
    nstFloat: PInt64(@result)^ := GetNodeValueInt64;
    nstInt32,
    nstInt64: Result := GetNodeValueInt64;
    else begin
      ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{************************************************************}
function TALJSONNodeW.GetFloat(const default: Double): Double;
begin
  if NodeSubType = nstNull then result := default
  else result := GetFloat;
end;

{***************************************************}
procedure TALJSONNodeW.SetFloat(const Value: Double);
begin
  setNodeValue(PInt64(@Value)^, nstFloat);
end;

{*******************************************}
function TALJSONNodeW.GetDateTime: TDateTime;
begin
  if NodeSubType = nstDateTime then PInt64(@result)^ := GetNodeValueInt64
  else begin
    ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
    result := 0; // to hide a warning;
  end;
end;

{*********************************************************************}
function TALJSONNodeW.GetDateTime(const default: TDateTime): TDateTime;
begin
  if NodeSubType = nstNull then result := default
  else result := GetDateTime;
end;

{*********************************************************}
procedure TALJSONNodeW.SetDateTime(const Value: TDateTime);
begin
  setNodeValue(PInt64(@Value)^, nstDateTime);
end;

{***************************************************}
function TALJSONNodeW.GetTimestamp: TALBSONTimestamp;
begin
  if NodeSubType = nstTimestamp then result.I64 := GetNodeValueInt64
  else begin
    ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
    result.I64 := 0; // to hide a warning;
  end;
end;

{************************************************************************************}
function TALJSONNodeW.GetTimestamp(const default: TALBSONTimestamp): TALBSONTimestamp;
begin
  if NodeSubType = nstNull then result := default
  else result := GetTimestamp;
end;

{*****************************************************************}
procedure TALJSONNodeW.SetTimestamp(const Value: TALBSONTimestamp);
begin
  setNodeValue(Value.I64, nstTimestamp);
end;

{****************************************}
function TALJSONNodeW.GetObjectID: String;
begin
  if NodeSubType = nstObjectID then result := GetNodeValueStr
  else begin
    ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{***************************************************************}
function TALJSONNodeW.GetObjectID(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetObjectID;
end;

{******************************************************}
procedure TALJSONNodeW.SetObjectID(const Value: String);
begin
  if length(Value) <> 24 then ALJSONDocErrorW('ObjectID must have 12 bytes');
  setNodeValue(Value, nstObjectID);
end;

{**************************************}
function TALJSONNodeW.GetInt32: Integer;
var LDouble: Double;
    LInt64: system.int64;
begin
  case NodeSubType of
    nstFloat: begin
                PInt64(@LDouble)^ := GetNodeValueInt64;
                LInt64 := trunc(LDouble);
                if (LInt64 <> LDouble) or // https://stackoverflow.com/questions/41779801/single-double-and-precision
                                          // Only values that are in form m*2^e, where m and e are integers can be stored in a floating point variable
                                          // so all integer can be store in the form m*2^e (ie: m = m*2^0)
                                          // so we can compare aInt64 <> aDouble without the need of samevalue
                   (LInt64 > system.int32.MaxValue) or
                   (LInt64 < system.int32.MinValue) then ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
                result := LInt64;
              end;
    nstInt32: begin
                LInt64 := GetNodeValueInt64;
                if (LInt64 > system.int32.MaxValue) or
                   (LInt64 < system.int32.MinValue) then ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
                result := LInt64;
              end;
    nstInt64: Result := GetNodeValueInt64;
    else begin
      ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{**************************************************************}
function TALJSONNodeW.GetInt32(const default: Integer): Integer;
begin
  if NodeSubType = nstNull then result := default
  else result := GetInt32;
end;

{****************************************************}
procedure TALJSONNodeW.SetInt32(const Value: Integer);
begin
  setNodeValue(Value, nstInt32);
end;

{************************************}
function TALJSONNodeW.GetInt64: Int64;
var LDouble: Double;
begin
  case NodeSubType of
    nstFloat: begin
                PInt64(@LDouble)^ := GetNodeValueInt64;
                result := trunc(LDouble);
                if result <> LDouble then ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType); // https://stackoverflow.com/questions/41779801/single-double-and-precision
                                                                                          // Only values that are in form m*2^e, where m and e are integers can be stored in a floating point variable
                                                                                          // so all integer can be store in the form m*2^e (ie: m = m*2^0)
                                                                                          // so we can compare result <> aDouble without the need of samevalue
              end;
    nstInt32,
    nstInt64: Result := GetNodeValueInt64;
    else begin
      ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{**********************************************************}
function TALJSONNodeW.GetInt64(const default: Int64): Int64;
begin
  if NodeSubType = nstNull then result := default
  else result := GetInt64;
end;

{**************************************************}
procedure TALJSONNodeW.SetInt64(const Value: Int64);
begin
  setNodeValue(Value, nstInt64);
end;

{*************************************}
function TALJSONNodeW.GetBool: Boolean;
begin
  if NodeSubType = nstBoolean then begin
    if GetNodeValueInt64 = 0 then result := False
    else result := true;
  end
  else begin
    ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
    result := False; // to hide a warning;
  end;
end;

{*************************************************************}
function TALJSONNodeW.GetBool(const default: Boolean): Boolean;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBool;
end;

{***************************************************}
procedure TALJSONNodeW.SetBool(const Value: Boolean);
begin
  if Value then setNodeValue(1, nstBoolean)
  else setNodeValue(0, nstBoolean);
end;

{*************************************}
function TALJSONNodeW.GetNull: Boolean;
begin
  result := NodeSubType = nstNull;
end;

{***************************************************}
procedure TALJSONNodeW.SetNull(const Value: Boolean);
begin
  if Value then setNodeValue(0, nstNull)
  else ALJSONDocErrorW('Only "true" is allowed for setNull property');
end;

{******************************************}
function TALJSONNodeW.GetJavascript: String;
begin
  if NodeSubType = nstJavascript then result := GetNodeValueStr
  else begin
    ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{*****************************************************************}
function TALJSONNodeW.GetJavascript(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetJavascript;
end;

{********************************************************}
procedure TALJSONNodeW.SetJavascript(const Value: String);
begin
  setNodeValue(Value, nstJavascript);
end;

{*************************************}
function TALJSONNodeW.GetRegEx: String;
begin
  if NodeSubType = nstRegEx then result := GetNodeValueStr
  else begin
    ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{************************************************************}
function TALJSONNodeW.GetRegEx(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetRegEx;
end;

{*****************************************************}
procedure TALJSONNodeW.SetRegEx(const Pattern: String);
begin
  setNodeValue(Pattern, 0, nstRegEx);
end;

{*****************************************************************************************}
procedure TALJSONNodeW.SetRegEx(const Pattern: String; const Options: TALPerlRegExOptions);
begin
  setNodeValue(Pattern, byte(Options), nstRegEx);
end;

{*********************************************************}
function TALJSONNodeW.GetRegExOptions: TALPerlRegExOptions;
begin
  if NodeSubType = nstRegEx then result := TALPerlRegExOptions(byte(GetNodeValueInt64))
  else begin
    ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
    result := []; // to hide a warning;
  end;
end;

{*********************************************************************************************}
function TALJSONNodeW.GetRegExOptions(const default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  if NodeSubType = nstNull then result := default
  else result := GetRegExOptions;
end;

{***********************************************************************}
procedure TALJSONNodeW.SetRegExOptions(const Value: TALPerlRegExOptions);
begin
  if NodeSubType <> nstRegEx then ALJSONDocErrorW('You can set regex options only to a regex node');
  setNodeValue(byte(Value), nstRegEx);
end;

{**************************************}
function TALJSONNodeW.GetBinary: String;
begin
  if NodeSubType = nstBinary then result := GetNodeValueStr
  else begin
    ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{*************************************************************}
function TALJSONNodeW.GetBinary(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBinary;
end;

{***************************************************}
procedure TALJSONNodeW.SetBinary(const Data: String);
begin
  setNodeValue(Data, 0, nstBinary); // 0 = Default BSON type
end;

{************************************************************************}
procedure TALJSONNodeW.SetBinary(const Data: String; const Subtype: byte);
begin
  setNodeValue(Data, Subtype, nstBinary);
end;

{*******************************************}
function TALJSONNodeW.GetBinarySubType: byte;
begin
  if NodeSubType = nstBinary then result := byte(GetNodeValueInt64)
  else begin
    ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
    result := 0; // to hide a warning;
  end;
end;

{****************************************************************}
function TALJSONNodeW.GetBinarySubType(const default: byte): byte;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBinarySubType;
end;

{***********************************************************}
procedure TALJSONNodeW.SetBinarySubType(const Subtype: byte);
begin
  if NodeSubType <> nstBinary then ALJSONDocErrorW('You can set binary subtype only to a binary node');
  setNodeValue(Subtype, nstBinary);
end;

{************************}
{returns the parent node.}
function TALJSONNodeW.GetParentNode: TALJSONNodeW;
begin
  Result := FParentNode;
end;

{******************************************}
{Sets the value of the ParentNode property.}
procedure TALJSONNodeW.SetParentNode(const Value: TALJSONNodeW);
begin
  if FParentNode <> Value then
    FParentNode := Value;
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
Begin
  LoadFromJSONString(Value);
end;

{********************************************************************}
{Returns the BSON that corresponds to the subtree rooted at this node.
 GetBSON returns the BSON that corresponds to this node and any child nodes it contains.}
function TALJSONNodeW.GetBSON: Tbytes;
begin
  SaveToBSONBytes(result);
end;

{*************************************************}
{SetBSON reload the node with the new given value }
procedure TALJSONNodeW.SetBSON(const Value: Tbytes);
Begin
  LoadFromBSONBytes(Value);
end;

{*****************************************************************}
{Returns the number of parents for this node in the node hierarchy.
 NestingLevel returns the number of ancestors for this node in the node hierarchy.}
function TALJSONNodeW.NestingLevel: Integer;
var PNode: TALJSONNodeW;
begin
  Result := 0;
  PNode := ParentNode;
  while PNode <> nil do begin
    Inc(Result);
    PNode := PNode.ParentNode;
  end;
end;

{******************************************************}
constructor TALJSONNodeW.Create(const NodeName: String);
Begin
  FParentNode := nil;
  fNodeName := NodeName;
end;

{***************************************************************}
//will create all the nodevalue and childnodelist to be sure that
//multiple thread can safely read at the same time the node
procedure TALJSONNodeW.MultiThreadPrepare(const aOnlyChildList: Boolean = False);
var I: integer;
begin
  if (not aOnlyChildList) and (NodeType = ntText) then begin

    case NodeSubType of
      nstFloat,
      nstBoolean,
      nstDateTime,
      nstNull,
      nstInt32,
      nstTimestamp,
      nstInt64: GetNodeValueStr;
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
    end;

    case NodeSubType of
      nstFloat,
      nstBoolean,
      nstDateTime,
      nstNull,
      nstInt32,
      nstTimestamp,
      nstInt64: GetNodeValueInt64;
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
    end;

  end

  else if (NodeType in [ntObject,ntArray]) then begin
    For I := 0 to ChildNodes.Count - 1 do
      ChildNodes[I].MultiThreadPrepare(aOnlyChildList);
  end;
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
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I], TDirection.FromEnd);
    if (LTmpNode = nil) then LNode := LNode.addChild(path[I], ntObject)
    else LNode := LTmpNode;
  end;
  result := LNode.addChild(path[high(path)], NodeType, Index);
end;

{****************************************************************************************************************}
function TALJSONNodeW.AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeW;
begin
  Result := AddChild('', NodeType, Index);
end;

{*****************************************************************}
function TALJSONNodeW.DeleteChild(const NodeName: String): boolean;
var I: integer;
begin
  I := ChildNodes.IndexOf(NodeName);
  if I >= 0 then begin
    ChildNodes.Delete(I);
    result := True;
  end
  else result := False;
end;

{**********************************************************************}
function TALJSONNodeW.DeleteChild(const Path: array of String): boolean;
var LNode: TALJSONNodeW;
    LTmpNode: TALJSONNodeW;
    I: integer;
begin
  LNode := Self;
  for I := low(path) to high(path) - 1 do begin
    LTmpNode := LNode.ChildNodes.findNode(path[I]);
    if (LTmpNode = nil) then exit(false)
    else LNode := LTmpNode;
  end;
  I := LNode.ChildNodes.IndexOf(path[high(path)]);
  if I >= 0 then begin
    LNode.ChildNodes.Delete(I);
    result := True;
  end
  else result := False;
end;

{************************************************************************************************}
function TALJSONNodeW.CreateNode(const NodeName: String; NodeType: TALJSONNodeType): TALJSONNodeW;
begin
  Result := ALCreateJSONNodeW(NodeName, NodeType);
end;

{********************************************}
{Returns the next child of this node’s parent.
 NextSibling returns the node that follows this one in the parent node’s ChildNodes property list.
 If this node is the last node in its parent’s child list, NextSibling raises an exception.}
function TALJSONNodeW.NextSibling: TALJSONNodeW;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, 1)
  else Result := nil;
end;

{************************************************}
{Returns the previous child of this node’s parent.
 PreviousSibling returns the node that precedes this one in the parent node’s ChildNodes property list.
 If this node is the first node in its parent’s child list, PreviousSibling raises an exception.}
function TALJSONNodeW.PreviousSibling: TALJSONNodeW;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, -1)
  else Result := nil;
end;

{**************}
{The JSON format
 There are just a few rules that you need to remember:
 *Objects are encapsulated within opening and closing brackets { } {
 *An empty object can be represented by { } {
 *Arrays are encapsulated within opening and closing square brackets [ ]
 *An empty array can be represented by [ ]
 *A member is represented by a key-value pair
 *The key of a member should be contained in double quotes. (JavaScript does not require this. JavaScript and some parsers will tolerate single-quotes)
 *Each member should have a unique key within an object structure
 *The value of a member must be contained in double quotes if it's a string (JavaScript and some parsers will tolerates single-quotes)
 *Boolean values are represented using the true or false literals in lower case
 *Number values are represented using double-precision floating-point format. Scientific notation is supported
 *Numbers should not have leading zeroes
 *"Offensive"" characters in a string need to be escaped using the backslash character
 *Null values are represented by the null literal in lower case
 *Other object types, such as dates, are not properly supported and should be converted to strings. It becomes the responsability of the parser/client to manage this.
 *Each member of an object or each array value must be followed by a comma if it's not the last one
 *The common extension for json files is '.json'
 *The mime type for json files is 'application/json'}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
Procedure TALJSONNodeW.ParseJSON(
            const Buffer: String;
            const SaxMode: Boolean;
            const onParseText: TAlJSONParseTextEventW;
            const onParseStartObject: TAlJSONParseObjectEventW;
            const onParseEndObject: TAlJSONParseObjectEventW;
            const onParseStartArray: TAlJSONParseArrayEventW;
            const onParseEndArray: TAlJSONParseArrayEventW;
            const Options: TALJSONParseOptions);

Var BufferLength: Integer;
    BufferPos: Integer;
    CurrName: String;
    CurrIndex: integer;
    CurrValue: String;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNodeW;
    NamePaths: TALNVStringListW;
    ObjectPaths: TALIntegerList;
    DecodeJSONReferences: boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(Const ExtraItems: String = ''): String;
  var I, L, P, Size: Integer;
      LB: Char;
      S: String;
  begin
    LB := ALDefaultJsonPathSeparatorW;
    Size := length(ExtraItems);
    if size <> 0 then Inc(Size, 1{length(LB)});
    for I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths.Names[I]) + 1{length(LB)});
    SetLength(Result, Size);
    P := 1;
    for I := 1 to NamePaths.Count - 1 do begin
      S := NamePaths.Names[I];
      L := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, Pbyte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
        Inc(P, L);
      end;
      L := 1{length(LB)};
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNodeW(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJSONNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, Pbyte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      L := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, Pbyte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
      Inc(P, L);
    end;
    setlength(result,P-1);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(
              const index: integer;
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
              const Index: integer;
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
        if NamePaths.Count = 0 then ALJSONDocErrorW(CALJSONParseError);
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
    if NamePaths.Count = 0 then ALJSONDocErrorW(CALJSONParseError);
    if Assigned(OnParseEndObject) then OnParseEndObject(Self, GetPathStr, NamePaths.Names[NamePaths.Count - 1])
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: String);
  begin
    if Assigned(OnParseStartArray) then OnParseStartArray(Self, GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if NamePaths.Count = 0 then ALJSONDocErrorW(CALJSONParseError);
    if Assigned(OnParseEndArray) then OnParseEndArray(Self, GetPathStr, NamePaths.Names[NamePaths.Count - 1]);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexItemToNamePath(const index: integer; Obj: Pointer);
  var S1: String;
  begin
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
  procedure _AddItemToNamePath(index: integer; const name: String; Obj: Pointer);
  begin
    if notSaxMode then begin
      if WorkingNode.nodetype=ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end
    else begin
      if NamePaths.Count = 0 then ALJSONDocErrorW(CALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt64Node(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
      LInt64: System.Int64;
  begin
    if ALJSONTryStrToInt64W(value, LInt64) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetInt64(LInt64);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LInt64], nstInt64)
      end
      else begin
        _DoParseText(index, Name, [LInt64], nstInt64)
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt32Node(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
      LInt32: System.Int32;
  begin
    if ALJSONTryStrToInt32W(value, LInt32) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setint32(LInt32);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LInt32], nstInt32)
      end
      else begin
        _DoParseText(index, Name, [LInt32], nstInt32)
      end
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTextNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
  begin
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(index, Name, [value], nstText)
    end
    else begin
      _DoParseText(index, Name, [value], nstText)
    end
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createFloatNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
      LDouble: Double;
  begin
    if ALTryStrToFloat(value, LDouble, ALDefaultFormatSettingsW) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetFloat(LDouble);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LDouble], nstFloat)
      end
      else begin
        _DoParseText(index, Name, [LDouble], nstFloat)
      end
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBinaryNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
      LBinSubtype: byte;
      LBinData: String;
  begin
    if ALJSONTryStrToBinaryW(value, LBinData, LBinSubtype) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.setbinary(LBinData, LBinSubtype);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LBinData, LBinSubtype], nstBinary);
      end
      else begin
        _DoParseText(index, Name, [LBinData, LBinSubtype], nstBinary);
      end
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createObjectIDNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
      LObjectID: String;
  begin
    if ALJSONTryStrToObjectIDW(value, LObjectID) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetObjectID(LObjectID);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LObjectID], nstObjectID)
      end
      else begin
        _DoParseText(index, Name, [LObjectID], nstObjectID)
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createBooleanNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
      LBool: Boolean;
  begin
    if value = 'true' then LBool := true
    else if value = 'false' then LBool := false
    else begin
      result := False;
      exit;
    end;
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setbool(LBool);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(index, Name, [LBool], nstBoolean);
    end
    else begin
      _DoParseText(index, Name, [LBool], nstBoolean);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createDateTimeNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
      LDateTime: TdateTime;
  begin
    if ALJSONTryStrToDateTimeW(value, LDateTime) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setdatetime(LDateTime);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LDateTime], nstDateTime);
      end
      else begin
        _DoParseText(index, Name, [LDateTime], nstDateTime);
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createTimestampNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
      LTimestamp: TALBSONTimestamp;
  begin
    if ALJSONTryStrToTimestampW(value, LTimestamp) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetTimestamp(LTimestamp);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LTimestamp.W1, LTimestamp.W2], nstTimeStamp);
      end
      else begin
        _DoParseText(index, Name, [LTimestamp.W1, LTimestamp.W2], nstTimeStamp);
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createnullNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
  begin
    if value = 'null' then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setnull(true);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, ['null'], nstNull);
      end
      else begin
        _DoParseText(index, Name, ['null'], nstNull);
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createRegExNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
      LRegEx: String;
      LRegExOptions: TALPerlRegExOptions;
  begin
    if ALJSONTryStrToRegExW(value, LRegEx, LRegExOptions) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetRegEx(LRegEx, LRegExOptions);
          WorkingNode.ChildNodes.Add(LNode);
        except
          ALFreeAndNil(LNode);
          raise;
        end;
        _DoParseText(index, Name, [LRegEx, Byte(LRegExOptions)], nstRegEx)
      end
      else begin
        _DoParseText(index, Name, [LRegEx, Byte(LRegExOptions)], nstRegEx)
      end;
    end
    else result := False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createJavascriptNode(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJSONNodeW;
  begin
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(index, Name, [value], nstJavascript);
    end
    else begin
      _DoParseText(index, Name, [value], nstJavascript);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createNode(index: integer; const name: String; const value: String; AQuotedValue: Boolean);
  begin
    if AQuotedValue then begin
      _createTextNode(index, Name, Value);
      exit;
    end;
    if _createFloatNode(index, Name, Value) then exit;  // << we have the same problem as javascript, if we put here a big number like (by exemple) 9223372036854775808
                                                        // << then the stored value will be different because of double precision that is less than int64 precision
                                                        // << it's the way javascript json work, it's have no room for int / int64 :(
                                                        // << if we want to have the possibility to store int64 precision then we must use node subtype helper
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
  function _extractLastIndexFromNamePath: integer;
  begin
    if NamePaths.Count = 0 then ALJSONDocErrorW(CALJSONParseError);
    ALMove(pointer(namePaths.ValueFromIndex[namepaths.Count - 1])^,result,sizeOf(integer));
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var LNode: TALJSONNodeW;
      LNodeType: TALJSONNodeType;
      LQuoteChar: Char;
      LNameValueSeparator: Char;
      LInSingleQuote: boolean;
      LInDoubleQuote: boolean;
      LInSlashQuote: boolean;
      LInSquareBracket: integer;
      LInRoundBracket: integer;
      LInCurlyBracket: integer;
      P1, P2: Integer;
      c: Char;
  Begin

    {$REGION 'init current char (c)'}
    c := Buffer[BufferPos];
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
        if (ObjectPaths.Count = 0) then ALJSONDocErrorW(cALJSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then ALJSONDocErrorW(cALJSONParseError);
      end;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        if assigned(ObjectPaths) then LNode := TALJSONNodeW(ObjectPaths.Objects[ObjectPaths.Count - 1])
        else LNode := TALJSONNodeW(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (LNode <> WorkingNode) then ALJSONDocErrorW(CALJSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then ALJSONDocErrorW(cALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then ALJSONDocErrorW(CALJSONParseError);

        //if working node <> Self then we can go to one level up
        If WorkingNode <> Self then begin

          //init WorkingNode to the parentNode
          WorkingNode := WorkingNode.ParentNode;

          //update CurrIndex if WorkingNode.NodeType = ntArray
          if assigned(ObjectPaths) then begin
            if WorkingNode.NodeType = ntArray then CurrIndex := ObjectPaths[Objectpaths.Count - 1] + 1;
          end
          else begin
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
        if not (LNodeType in [ntObject,ntarray]) then ALJSONDocErrorW(cALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then ALJSONDocErrorW(CALJSONParseError);

        //update CurrIndex if WorkingNode.NodeType = ntArray
        if (Namepaths.Count >= 2) and
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 2]) = ntarray) then CurrIndex := _extractLastIndexFromNamePath + 1;

      end;

      //call the DoParseEndObject/array event
      if Assigned(OnParseEndObject) then begin
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

    {$REGION 'Begin Object/Array Without NAME'}
    // ... { ....
    // ... [ ....
    if c in ['{','['] then begin // ... { ...
                                 //     ^BufferPos

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then ALJSONDocErrorW(CALJSONParseError);

        //Node without name can be ONLY present inside an array node
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then ALJSONDocErrorW(CALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
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
        if assigned(ObjectPaths) then ObjectPaths.AddObject(CurrIndex, WorkingNode)
        else _AddItemToNamePath(CurrIndex, '', WorkingNode);

      end

      //if we are in sax mode
      else begin

          //Node without name can be ONLY present inside an array node
          if (CurrIndex < 0) or
             (NamePaths.Count = 0) or
             (TALJsonNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then ALJSONDocErrorW(CALJSONParseError);

        //update the path
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrIndex, '', pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event
      if c = '{' then begin
        if Assigned(OnParseStartObject) then _DoParseStartObject('');
        CurrIndex := -1;
      end
      else begin
        if Assigned(OnParseStartArray) then _DoParseStartArray('');
        CurrIndex := 0;
      end;

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
    LQuoteChar := #0;
    if c in ['"',''''] then begin  // ... " ...
                                   //     ^BufferPos
      LQuoteChar := c; // "
      P1 := BufferPos + 1; // ... "...\"..."
                           //      ^P1
      While P1 <= BufferLength do begin

       c := Buffer[P1];

       If (c = '\') and
          (P1 < BufferLength) and
          (Buffer[P1 + 1] in ['\', LQuoteChar]) then inc(p1, 2) // ... "...\"..."
                                                                //         ^^^P1
       else if c = LQuoteChar then begin
         ALCopyStr(Buffer,CurrName,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALJavascriptDecodeV(CurrName); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorW(CALJSONParseError);
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

      LInSingleQuote := False;
      LInDoubleQuote := False;
      LInSquareBracket := 0;
      LInRoundBracket := 0;
      LInCurlyBracket := 0;

      While (BufferPos <= BufferLength) do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then ALJSONDocErrorW(CALJSONParseError);

      P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
                       //     ^P1
      While (P1 <= BufferLength) do begin

        c := Buffer[P1];

        if (not LInSingleQuote) and
           (not LInDoubleQuote) and
           (LInSquareBracket = 0) and
           (LInRoundBracket = 0) and
           (LInCurlyBracket = 0) and
           (c in [',', '}', ']', ':']) then begin
          P2 := P1-1;
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
          else if (c = '}') then inc(LInCurlyBracket)
          else if (c = '{') then dec(LInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorW(CALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos

    end;
    {$ENDREGION}

    {$REGION 'extract the name value separator part'}
    LNameValueSeparator := #0;
    While (BufferPos <= BufferLength) do begin
      If Buffer[BufferPos] <= ' ' then inc(BufferPos)
      else begin
        LNameValueSeparator := Buffer[BufferPos];
        break;
      end;
    end;
    if BufferPos > BufferLength then ALJSONDocErrorW(CALJSONParseError);  // .... : ....
                                                                          //      ^BufferPos
    {$ENDREGION}

    {$REGION 'if aNameValueSeparator is absent then it is just a value'}
    if LNameValueSeparator <> ':' then begin

      //Node without name can be ONLY present inside an array node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then ALJSONDocErrorW(CALJSONParseError);
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then ALJSONDocErrorW(CALJSONParseError);
      end
      else begin
        if (CurrIndex < 0) or
           (NamePaths.Count = 0) or
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then ALJSONDocErrorW(CALJSONParseError);
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
    if BufferPos > BufferLength then ALJSONDocErrorW(CALJSONParseError); // .... " ....
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
        if not assigned(WorkingNode) then ALJSONDocErrorW(CALJSONParseError);

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0)  or
           (WorkingNode.nodetype <> ntObject) then ALJSONDocErrorW(CALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
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
        if assigned(ObjectPaths) then ObjectPaths.AddObject(-1, WorkingNode)
        else _AddItemToNamePath(-1, CurrName, WorkingNode);

      end

      //if we are in sax mode
      else begin

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0) or
           (NamePaths.Count = 0) or
           (TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]) <> ntobject) then ALJSONDocErrorW(CALJSONParseError);

        //update the path
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(-1, CurrName, pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event and update the CurrIndex if it's an array
      if c = '{' then begin
        if Assigned(OnParseStartObject) then _DoParseStartObject(CurrName)
      end
      else begin
        if Assigned(OnParseStartArray) then _DoParseStartArray(CurrName);
        CurrIndex := 0;
      end;

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
      P1 := BufferPos + 1; // ... "...\"..."
                           //      ^P1
      While P1 <= BufferLength do begin

       c := Buffer[P1];

       If (c = '\') and
          (P1 < BufferLength) and
          (Buffer[P1 + 1] in ['\', LQuoteChar]) then inc(p1, 2) // ... "...\"..."
                                                                //         ^^^P1
       else if c = LQuoteChar then begin
         ALCopyStr(Buffer,currValue,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALJavascriptDecodeV(currValue); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorW(CALJSONParseError);
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

      LInSingleQuote := False;
      LInDoubleQuote := False;
      LInSlashQuote := False;
      LInSquareBracket := 0;
      LInRoundBracket := 0;
      LInCurlyBracket := 0;

      While (BufferPos <= BufferLength) do begin
        If Buffer[BufferPos] <= ' ' then inc(bufferPos)
        else break;
      end;
      if BufferPos > BufferLength then ALJSONDocErrorW(CALJSONParseError);

      P1 := BufferPos; // ... new Date('Dec 03, 1924'), ....
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
          P2 := P1-1;
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
          else if (c = '}') then inc(LInCurlyBracket)
          else if (c = '{') then dec(LInCurlyBracket);
        end;

        inc(P1); // ... new Date('Dec 03, 1924'), ....
                 //     ^^^^^^^^^^^^^^^^^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorW(CALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos


    end;
    {$ENDREGION}

    {$REGION 'create the named text node'}

    //Node withe name MUST be ONLY present inside an object node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(CALJSONParseError);
      if (CurrIndex >= 0)  or
         (WorkingNode.nodetype <> ntObject) then ALJSONDocErrorW(CALJSONParseError);
    end
    else begin
      if (CurrIndex >= 0) or
         (NamePaths.Count = 0) or
         (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntObject) then ALJSONDocErrorW(CALJSONParseError);
    end;

    //create the node
    _createNode(currIndex,CurrName,CurrValue,LQuoteChar in ['"','''']);

    {$ENDREGION}

  end;

var InCommentLine: integer;
    c: Char;

Begin

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
    ObjectPaths := TALIntegerList.Create(false{OwnsObjects});
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
    if assigned(ObjectPaths) then ObjectPaths.AddObject(-1, WorkingNode)
    else begin
      if NotSaxMode then _AddNameItemToNamePath('', WorkingNode)
      else _AddNameItemToNamePath('', pointer(NodeType));
    end;

    //skip the first {
    While (BufferPos <= BufferLength) do begin
      c := Buffer[BufferPos];
      If c <= ' ' then inc(bufferPos)
      else begin
        if (c = '{') then begin
          if (Nodetype <> ntObject) then ALJSONDocErrorW(CALJsonOperationError,GetNodeType);
          CurrIndex := -1;
          _DoParseStartObject('');
        end
        else if (c = '[') then begin
          if (Nodetype <> ntArray) then ALJSONDocErrorW(CALJsonOperationError,GetNodeType);
          CurrIndex := 0;
          _DoParseStartArray('');
        end
        else AlJSONDocErrorW(cALJSONParseError);
        inc(bufferPos);
        break;
      end;
    end;

    //analyze all the nodes
    if poAllowComments in Options then begin
      InCommentLine := 0;
      While (BufferPos <= BufferLength) do begin
        c := Buffer[BufferPos];
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
        c := Buffer[BufferPos];
        If (c <= ' ') or (c = ',') then inc(bufferPos)
        else AnalyzeNode;
      end;
    end;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then ALJSONDocErrorW(cALJSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then ALJSONDocErrorW(cALJSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if NotSaxMode and (WorkingNode <> nil) then ALJSONDocErrorW(cALJSONParseError);

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
            const Buffer: Tbytes;
            const SaxMode: Boolean;
            const onParseText: TAlJSONParseTextEventW;
            const onParseStartObject: TAlJSONParseObjectEventW;
            const onParseEndObject: TAlJSONParseObjectEventW;
            const onParseStartArray: TAlJSONParseArrayEventW;
            const onParseEndArray: TAlJSONParseArrayEventW;
            const Options: TALJSONParseOptions);

Var BufferLength: Integer;
    BufferPos: Integer;
    CurrName: String;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNodeW;
    NamePaths: TALStringListW;
    ObjectPaths: TObjectList<TALJSONNodeW>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(Const ExtraItems: String = ''): String;
  var I, L, P, Size: Integer;
      LB: Char;
      S: String;
  begin
    LB := ALDefaultJsonPathSeparatorW;
    Size := length(ExtraItems);
    if size <> 0 then Inc(Size, 1{length(LB)});
    for I := 1 to NamePaths.Count - 1 do Inc(Size, Length(NamePaths[I]) + 1{length(LB)});
    SetLength(Result, Size);
    P := 1;
    for I := 1 to NamePaths.Count - 1 do begin
      S := NamePaths[I];
      L := Length(S);
      if L <> 0 then begin
        ALMove(pointer(S)^, Pbyte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
        Inc(P, L);
      end;
      L := 1{length(LB)};
      if ((i <> NamePaths.Count - 1) or
          (ExtraItems <> '')) and
         (((NotSaxMode) and (TALJSONNodeW(NamePaths.Objects[I]).nodetype <> ntarray)) or
          ((not NotSaxMode) and (TALJsonNodeType(NamePaths.Objects[I]) <> ntarray))) then begin
        ALMove(LB, Pbyte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
        Inc(P, L);
      end;
    end;
    if ExtraItems <> '' then begin
      L := length(ExtraItems);
      ALMove(pointer(ExtraItems)^, Pbyte(Result)[(P-1)*sizeOf(Char)], L*sizeOf(Char));
      Inc(P, L);
    end;
    setlength(result,P-1);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(
              const index: String;
              const Args: array of const;
              const NodeSubType: TALJsonNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr('[' + index + ']'), '', Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithName(
              const name: String;
              const Args: array of const;
              const NodeSubType: TALJsonNodeSubType);
  begin
    if Assigned(OnParseText) then OnParseText(Self, GetPathStr(Name), Name, Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseText(
              const NameOrIndex: String;
              const Args: array of const;
              const NodeSubType: TALJsonNodeSubType);
  begin
    if Assigned(OnParseText) then begin
      if notSaxMode then begin
        if WorkingNode.nodetype=ntarray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
      end
      else begin
        if NamePaths.Count = 0 then ALJSONDocErrorW(CALJSONParseError);
        if TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntArray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
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
    if NamePaths.Count = 0 then ALJSONDocErrorW(CALJSONParseError);
    if Assigned(OnParseEndObject) then OnParseEndObject(Self, GetPathStr, NamePaths[NamePaths.Count - 1])
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: String);
  begin
    if Assigned(OnParseStartArray) then OnParseStartArray(Self, GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if NamePaths.Count = 0 then ALJSONDocErrorW(CALJSONParseError);
    if Assigned(OnParseEndArray) then OnParseEndArray(Self, GetPathStr, NamePaths[NamePaths.Count - 1]);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexItemToNamePath(const index: String; Obj: Pointer);
  begin
    NamePaths.AddObject('[' + Index + ']', Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddNameItemToNamePath(const name: String; Obj: Pointer);
  begin
    NamePaths.AddObject(Name, Obj)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddItemToNamePath(const nameOrIndex: String; Obj: Pointer);
  begin
    if notSaxMode then begin
      if WorkingNode.nodetype=ntarray then _AddIndexItemToNamePath(nameOrIndex, Obj)
      else _AddNameItemToNamePath(nameOrIndex, Obj);
    end
    else begin
      if NamePaths.Count = 0 then ALJSONDocErrorW(CALJSONParseError);
      if TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(nameOrIndex, Obj)
      else _AddNameItemToNamePath(nameOrIndex, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt64Node(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LInt64: System.Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) then ALJSONDocErrorW(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt64, sizeof(LInt64));
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetInt64(LInt64);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LInt64], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LInt64], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt32Node(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LInt32: System.Int32;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) then ALJSONDocErrorW(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setint32(LInt32);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LInt32], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LInt32], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTextNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LInt32: System.Int32;
      LText: String;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) then ALJSONDocErrorW(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    if (BufferPos + LInt32 > BufferLength) then ALJSONDocErrorW(cALBSONParseError);
    LText := Tencoding.UTF8.GetString(Buffer,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(LText);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LText], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LText], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createFloatNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LDouble: Double;
  begin
    if BufferPos > BufferLength - sizeof(Double) then ALJSONDocErrorW(cALBSONParseError);
    ALMove(Buffer[BufferPos], LDouble, sizeof(Double));
    BufferPos := BufferPos + sizeof(Double);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetFloat(LDouble);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LDouble], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LDouble], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBinaryNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LInt32: System.Int32;
      LBinSubtype: byte;
      LBinData: Tbytes;
      LBase64Data: String;
  begin
    //Get size
    if BufferPos > BufferLength - sizeof(LInt32) then ALJSONDocErrorW(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    //Get the subtype
    if BufferPos >= BufferLength then ALJSONDocErrorW(cALBSONParseError);
    LBinSubtype := Buffer[BufferPos];
    BufferPos := BufferPos + 1;

    //Get the data
    if (BufferPos + LInt32 > BufferLength) then ALJSONDocErrorW(cALBSONParseError);
    setlength(LBinData, LInt32);
    ALMove(Buffer[BufferPos], pointer(LBinData)^, LInt32);
    LBase64Data := ALBase64EncodeBytesW(LBinData);
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.setbinary(LBase64Data, LBinSubtype);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LBase64Data, LBinSubtype], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LBase64Data, LBinSubtype], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createObjectIDNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LObjectID: Tbytes;
      LHexData: String;
  begin
    if BufferPos > BufferLength - 12{length(aObjectID)} then ALJSONDocErrorW(cALBSONParseError);
    setlength(LObjectID, 12);
    ALMove(Buffer[BufferPos], pointer(LObjectID)^, 12{length(aObjectID)});
    LHexData := ALBinToHexW(LObjectID);
    BufferPos := BufferPos + 12{length(aObjectID)};

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetObjectID(LHexData);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LHexData], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LHexData], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBooleanNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LBool: Boolean;
  begin
    if BufferPos >= BufferLength then ALJSONDocErrorW(cALBSONParseError);
    if Buffer[BufferPos] = $00 then LBool := False
    else if Buffer[BufferPos] = $01 then LBool := true
    else begin
      ALJSONDocErrorW(cALBSONParseError);
      LBool := False; // to hide a warning;
    end;
    BufferPos := BufferPos + 1;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setbool(LBool);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LBool], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LBool], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createDateTimeNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LDateTime: TdateTime;
      LInt64: System.Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) then ALJSONDocErrorW(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt64, sizeof(LInt64));
    LDateTime := ALUnixMsToDateTime(LInt64);
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setdatetime(LDateTime);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LDateTime], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LDateTime], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTimestampNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LTimestamp: TALBSONTimestamp;
      LInt64: System.Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) then ALJSONDocErrorW(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt64, sizeof(LInt64));
    LTimestamp.I64 := LInt64;
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetTimestamp(LTimestamp);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LTimestamp.W1, LTimestamp.W2], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LTimestamp.W1, LTimestamp.W2], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createnullNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
  begin
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setnull(true);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, ['null'], NodeSubType);
    end
    else begin
      _DoParseText(Name, ['null'], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createRegExNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LRegEx: String;
      LRegExOptions: TALPerlRegExOptions;
      P1: integer;
  begin
    //Get pattern
    P1 := BufferPos;
    While (P1 < BufferLength) do begin
      If Buffer[P1] <> $00 then inc(P1)
      else begin
        LRegEx := Tencoding.UTF8.GetString(Buffer,BufferPos,P1 - BufferPos);
        break;
      end;
    end;
    if P1 >= BufferLength then ALJSONDocErrorW(cALBSONParseError);
    BufferPos := P1 + 1;

    //Get options
    LRegExOptions := [];
    While (BufferPos < BufferLength) do begin
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
    if BufferPos >= BufferLength then ALJSONDocErrorW(cALBSONParseError);
    inc(BufferPos);

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetRegEx(LRegEx, LRegExOptions);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LRegEx, Byte(LRegExOptions)], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LRegEx, Byte(LRegExOptions)], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createJavascriptNode(
              const name: String;
              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJSONNodeW;
      LJavascript: String;
      LInt32: System.Int32;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) then ALJSONDocErrorW(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    if (BufferPos + LInt32 > BufferLength) then ALJSONDocErrorW(cALBSONParseError);
    LJavascript := Tencoding.UTF8.GetString(Buffer,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(LJavascript);
        WorkingNode.ChildNodes.Add(LNode);
      except
        ALFreeAndNil(LNode);
        raise;
      end;
      _DoParseText(Name, [LJavascript], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LJavascript], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var LNode: TALJSONNodeW;
      LNodeType: TALJsonNodeType;
      LNodeSubType: TALJsonNodeSubType;
      P1: Integer;
      c: byte;
  Begin

    {$REGION 'init current char (c)'}
    c := Buffer[BufferPos];
    {$ENDREGION}

    {$REGION 'End Object/Array'}
    // ... } ....
    // ... ] ....
    if c = $00 then begin

      //error if Paths.Count = 0 (mean one end object/array without any starting)
      if assigned(ObjectPaths) then begin
        if (ObjectPaths.Count = 0) then ALJSONDocErrorW(cALBSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then ALJSONDocErrorW(cALBSONParseError);
      end;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        if assigned(ObjectPaths) then LNode := ObjectPaths[ObjectPaths.Count - 1]
        else LNode := TALJSONNodeW(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (LNode <> WorkingNode) then ALJSONDocErrorW(cALBSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then ALJSONDocErrorW(cALBSONParseError);

        //if working node <> Self then we can go to one level up
        If WorkingNode <> Self then begin

          //init WorkingNode to the parentNode
          WorkingNode := WorkingNode.ParentNode;

        end

        //if working node = Self then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

        //calculate anodeTypeInt
        LNodeType := TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then ALJSONDocErrorW(cALBSONParseError);

      end;

      //call the DoParseEndObject/array event
      if Assigned(OnParseEndObject) then begin
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
    LNodeSubType := nstText; // to hide fucking warning
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
      $10: LNodeSubType := nstint32;
      $11: LNodeSubType := nstTimestamp;
      $12: LNodeSubType := nstint64;
      else ALJSONDocErrorW(cALBSONParseError);
    end;
    BufferPos := BufferPos + 1;
    {$ENDREGION}

    {$REGION 'Get the node name'}
    P1 := BufferPos;
    While (P1 < BufferLength) do begin
      If Buffer[P1] <> $00 then inc(P1)
      else begin
        CurrName := Tencoding.UTF8.GetString(Buffer,BufferPos,P1-BufferPos);
        break;
      end;
    end;
    if P1 >= BufferLength then ALJSONDocErrorW(cALBSONParseError);
    BufferPos := P1 + 1;
    {$ENDREGION}

    {$REGION 'Begin Object/Array'}
    // ... { ....
    // ... [ ....
    if LNodeSubType in [nstObject,nstArray] then begin

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside Self
        if not assigned(WorkingNode) then ALJSONDocErrorW(cALBSONParseError);

        //create the node according the the braket char and add it to the workingnode
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
        if LNodeSubType = nstObject then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrName, pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event
      if LNodeSubType = nstObject then begin
        if Assigned(OnParseStartObject) then _DoParseStartObject(CurrName)
      end
      else begin
         if Assigned(OnParseStartArray) then _DoParseStartArray(CurrName);
      end;

      //update BufferPos
      BufferPos := BufferPos + 4; // we don't need the size of the object/array (4 bytes)

      //finallly exit from this procedure, everything was done
      exit;

    end;
    {$ENDREGION}

    {$REGION 'create the node'}
    case LNodeSubType of
      // \x01 + name + \x00 + double
      nstFloat: _createFloatNode(CurrName, LNodeSubType);

      // \x02 + name + \x00 + length (int32) + string + \x00
      nstText: _createTextNode(CurrName, LNodeSubType);

      // \x05 + name + \x00 + int32 + subtype + (byte*)
      nstbinary: _createBinaryNode(CurrName, LNodeSubType);

      // \x07 + name + \x00 + (byte*12)
      nstObjectID: _createObjectIDNode(CurrName, LNodeSubType);

      // \x08 + name + \x00 + \x00 => Boolean "false"
      // \x08 + name + \x00 + \x01	=> Boolean "true"
      nstBoolean: _createBooleanNode(CurrName, LNodeSubType);

      // \x09 + name + \x00 + int64
      nstDateTime: _createDateTimeNode(CurrName, LNodeSubType);

      // \x11 + name + \x00 + int64
      nstTimestamp: _createTimestampNode(CurrName, LNodeSubType);

      // \x0A + name + \x00
      nstnull: _createNullNode(CurrName, LNodeSubType);

      // \x0B + name + \x00 + (byte*) + \x00 + (byte*) + \x00
      nstRegEx: _createRegExNode(CurrName, LNodeSubType);

      // \x0D + name + \x00 + length (int32) + string + \x00
      nstJavascript: _createJavascriptNode(CurrName, LNodeSubType);

      // \x10 + name + \x00 + int32
      nstint32: _createInt32Node(CurrName, LNodeSubType);

      // \x12 + name + \x00 + int64
      nstint64: _createInt64Node(CurrName, LNodeSubType);

      else ALJSONDocErrorW(cALBSONParseError);
    end;
    {$ENDREGION}

  end;

Begin

  //Only Object Node can be loaded from BSON
  If NodeType <> ntObject then AlJSONDocErrorW(CALJsonOperationError,GetNodeType);
  if poClearChildNodes in Options then ChildNodes.Clear;

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
    BufferLength := length(Buffer);
    BufferPos := 4; // the first 4 bytes are the length of the document and we don't need it

    //add first node in ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.Add(WorkingNode)
    else begin
      if NotSaxMode then NamePaths.AddObject('', WorkingNode)
      else NamePaths.AddObject('', pointer(ntObject));
    end;
    _DoParseStartObject('');

    //analyze all the nodes
    While (BufferPos < BufferLength) do
      AnalyzeNode;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then ALJSONDocErrorW(cALBSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then ALJSONDocErrorW(cALBSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if NotSaxMode and (WorkingNode <> nil) then ALJSONDocErrorW(cALBSONParseError);

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ALFreeAndNil(ObjectPaths)
    else ALFreeAndNil(NamePaths);

  end;

end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
procedure TALJSONNodeW.SaveToJson(
            const Stream: TStream;
            const StreamEncoding: TEncoding;
            Var buffer: String;
            const Options: TALJSONSaveOptions);

Const BufferSize: integer = 8192;

Var NodeStack: Tstack<TALJSONNodeW>;
    CurrentNode: TALJSONNodeW;
    CurrentParentNode: TALJSONNodeW;
    CurrentIndentStr: String;
    IndentStr: String;
    EncodeControlCharacters: Boolean;
    SkipNodeSubTypeHelper: boolean;
    SaveInt64AsText: Boolean;
    AutoIndentNode: Boolean;
    BufferPos: Integer;
    LastWrittenChar: Char;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteBuffer2Stream(const buffer: String; BufferLength: Integer);
  Var LBytes: Tbytes;
  Begin
    if assigned(Stream) then begin
      If BufferLength > 0 then begin
        LBytes := StreamEncoding.GetBytes(TCharArray(buffer), 0{CharIndex}, BufferLength{CharCount});
        stream.Writebuffer(pointer(LBytes)^,length(LBytes));
      end;
      BufferPos := 0;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _Write2Buffer(const Source; Count: NativeInt);
  Begin
    if Count = 0 then exit;
    if Count + BufferPos > length(Buffer) then setlength(Buffer, Count + BufferPos + BufferSize);
    ALMove(Source, pbyte(Buffer)[BufferPos*sizeOf(Char)], Count*sizeOf(Char));
    BufferPos := BufferPos + Count;
    if BufferPos >= 32768 then _WriteBuffer2Stream(Buffer,BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStr2Buffer(const str:String);
  var L: integer;
  Begin
    L := Length(Str);
    if L = 0 then exit;
    LastWrittenChar := Str[L];
    _Write2Buffer(pointer(str)^,L);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteTextNode2Buffer(aTextNode:TALJSONNodeW);
  Begin
    with aTextNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode then begin
        _WriteStr2Buffer(#13#10);
        _WriteStr2Buffer(CurrentIndentStr);
      end;

      if (assigned(ParentNode)) and
         (ParentNode.NodeType <> ntArray) then begin
        if EncodeControlCharacters then begin
           _WriteStr2Buffer('"');
           _WriteStr2Buffer(ALJavascriptEncode(NodeName));
           _WriteStr2Buffer('":');
        end
        else begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(NodeName);
          _WriteStr2Buffer('":');
        end;
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
      else _WriteStr2Buffer(GetNodeValueInterchange(SkipNodeSubTypeHelper));

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNodeW);
  var LNodeList: TALJSONNodeListW;
      LEmptyNode: Boolean;
      I: integer;
  Begin
    with aObjectNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode and (CurrentIndentStr <> '') then begin
         _WriteStr2Buffer(#13#10);
         _WriteStr2Buffer(CurrentIndentStr);
      end;

      if aObjectNode = self then _WriteStr2Buffer('{')
      else if (assigned(ParentNode)) and
              (ParentNode.NodeType <> ntArray) then begin
        if EncodeControlCharacters then begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(ALJavascriptEncode(NodeName));
          _WriteStr2Buffer('":{')
        end
        else begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(NodeName);
          _WriteStr2Buffer('":{');
        end;
      end
      else _WriteStr2Buffer('{');

      LEmptyNode := True;
      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            NodeStack.Push(aObjectNode);
            For I := Count - 1 downto 0 do NodeStack.Push(Nodes[I]);
          end
      end;

      If LEmptyNode then _WriteStr2Buffer('}')
      else CurrentIndentStr := CurrentIndentStr + IndentStr;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNodeW);
  Begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr)+1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer('}');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNodeW);
  var LNodeList: TALJSONNodeListW;
      LEmptyNode: Boolean;
      I: integer;
  Begin
    with aArrayNode do begin

      if not (LastWrittenChar in ['{','[']) then _WriteStr2Buffer(',');

      if AutoIndentNode and (CurrentIndentStr <> '') then begin
        _WriteStr2Buffer(#13#10);
        _WriteStr2Buffer(CurrentIndentStr);
      end;

      if aArrayNode = self then _WriteStr2Buffer('[')
      else if (assigned(ParentNode)) and
              (ParentNode.NodeType <> ntArray) then begin
        if EncodeControlCharacters then begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(ALJavascriptEncode(NodeName));
          _WriteStr2Buffer('":[');
        end
        else begin
          _WriteStr2Buffer('"');
          _WriteStr2Buffer(NodeName);
          _WriteStr2Buffer('":[');
        end;
      end
      else _WriteStr2Buffer('[');

      LEmptyNode := True;
      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            NodeStack.Push(aArrayNode);
            For I := Count - 1 downto 0 do NodeStack.Push(Nodes[I]);
          end
      end;

      If LEmptyNode then _WriteStr2Buffer(']')
      else CurrentIndentStr := CurrentIndentStr + IndentStr;

    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNodeW);
  Begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr) + 1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer(']');
  end;

begin
  If not (NodeType in [ntObject, ntArray]) then exit;  // normally only Object node can gave a valid json stream
                                                       // but their is some situation where the array (containing json node)
                                                       // is also usefull
  CurrentParentNode := nil;
  NodeStack := Tstack<TALJSONNodeW>.Create;
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
    NodeStack.Push(self);

    {loop on all nodes}
    While NodeStack.Count > 0 Do begin
      CurrentNode := NodeStack.Pop;

      with CurrentNode do
        case NodeType of
          ntObject: begin
                      if currentNode = CurrentParentNode then _WriteEndObjectNode2Buffer(CurrentNode)
                      else _WriteStartObjectNode2Buffer(CurrentNode);
                    end;
          ntArray: begin
                      if currentNode = CurrentParentNode then _WriteEndArrayNode2Buffer(CurrentNode)
                      else _WriteStartArrayNode2Buffer(CurrentNode);
                   end;
          ntText: _WriteTextNode2Buffer(CurrentNode);
          else ALJSONDocErrorW(cAlJSONInvalidNodeType);
        end;

      CurrentParentNode := CurrentNode.ParentNode;
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
var buffer: String;
begin
  SaveToJson(Stream, Encoding, buffer, Options);
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
Var LfileStream: TfileStream;
    LTmpFilename: String;
begin
  if soProtectedSave in Options then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LfileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToJSONStream(LfileStream, Encoding, Options);
    finally
      ALFreeAndNil(LfileStream);
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
  SaveToJson(nil, nil, Str, Options);
end;

{********************************}
procedure TALJSONNodeW.SaveToBson(
            const Stream: TStream;
            Var buffer: Tbytes;
            const Options: TALJSONSaveOptions);

Const BufferSize: integer = 8192;

Var NodeStack: Tstack<TALJSONNodeW>;
    NodeIndexStack: TALintegerList;
    NodeStartPosStack: TALInt64List;
    CurrentNode: TALJSONNodeW;
    CurrentParentNode: TALJSONNodeW;
    CurrentNodeIndex: integer;
    CurrentNodeStartPos: System.int64;
    BufferPos: NativeInt;
    StreamPos: system.int64;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteBuffer2Stream(const buffer: Tbytes; BufferLength: Integer);
  Begin
    if assigned(Stream) then begin
      If BufferLength > 0 then stream.Writebuffer(pointer(buffer)^,BufferLength);
      BufferPos := 0;
      StreamPos := stream.Position;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _Write2Buffer(const Source; Count: NativeInt);
  Begin
    if Count = 0 then exit;
    if Count + BufferPos > length(Buffer) then setlength(Buffer, Count + BufferPos + BufferSize);
    ALMove(Source, Buffer[BufferPos], Count);
    BufferPos := BufferPos + Count;
    if BufferPos >= 32768 then _WriteBuffer2Stream(Buffer,BufferPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteUTF8Str2Buffer(const str:String); overload;
  var LBytes: Tbytes;
  Begin
    LBytes := Tencoding.UTF8.GetBytes(str);
    _Write2Buffer(pointer(LBytes)^,length(LBytes));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteUTF8Str2Buffer(const index:integer); overload;
  Begin
    _WriteUTF8Str2Buffer(ALIntToStrW(index));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteByte2Buffer(const aByte:byte);
  Begin
    _Write2Buffer(aByte, sizeOF(aByte));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteBytes2Buffer(const aBytes: array of byte);
  Begin
    if length(aBytes)  > 0 then
      _Write2Buffer(aBytes[0], length(aBytes));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //take care because fucking TStringStream (for exemple) do not permit
  //to write previous to the current position (it's set the size of the
  //new stream to the current position ... unbelievable!)
  Procedure _WriteInt2Pos(const aInt:integer; const aPos: system.Int64);
  Begin
    if aPos < StreamPos then begin
      Stream.position := aPos;
      stream.Writebuffer(aInt,sizeof(aInt));
      Stream.position := StreamPos;
    end
    else ALMove(aInt, Buffer[aPos - StreamPos], sizeOf(aInt));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x01 + name + \x00 + double
  Procedure _WriteFloatValue2Buffer(aTextNode:TALJSONNodeW);
  var LDouble: Double;
  begin
    LDouble := aTextNode.Float;
    _Write2Buffer(LDouble, sizeOf(LDouble));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x02 + name + \x00 + length (int32) + string + \x00
  Procedure _WriteTextValue2Buffer(aTextNode:TALJSONNodeW);
  var LInt32: system.int32;
      LText: Tbytes;
  begin
    LText := Tencoding.UTF8.GetBytes(aTextNode.Text);
    LInt32 := length(LText) + 1 {for the trailing #0};
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _WriteBytes2Buffer(LText);
    _WriteByte2Buffer($00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x05 + name + \x00 + int32 + subtype + (byte*)
  Procedure _WriteBinaryValue2Buffer(aTextNode:TALJSONNodeW);
  var LInt32: system.int32;
      LBinary: Tbytes;
      LBinarySubType: Byte;
  begin
    LBinary := ALBase64DecodeBytes(aTextNode.binary);
    LBinarySubType := aTextNode.BinarySubType;
    LInt32 := length(LBinary);
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _Write2Buffer(LBinarySubType, sizeOF(LBinarySubType));
    _WriteBytes2Buffer(LBinary);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x07 + name + \x00 + (byte*12)
  Procedure _WriteObjectIDValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    _WriteBytes2Buffer(ALHexToBin(aTextNode.ObjectID));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x08 + name + \x00 + \x00 => Boolean "false"
  // \x08 + name + \x00 + \x01	=> Boolean "true"
  Procedure _WriteBooleanValue2Buffer(aTextNode:TALJSONNodeW);
  begin
    if not aTextNode.bool then _WriteByte2Buffer($00)
    else _WriteByte2Buffer($01);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x09 + name + \x00 + int64
  Procedure _WriteDateTimeValue2Buffer(aTextNode:TALJSONNodeW);
  var LInt64: system.Int64;
  begin
    LInt64 := ALDateTimeToUnixMs(aTextNode.DateTime);
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x11 + name + \x00 + int64
  Procedure _WriteTimestampValue2Buffer(aTextNode:TALJSONNodeW);
  var LInt64: system.Int64;
  begin
    LInt64 := aTextNode.Timestamp.I64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \xOB + name + \x00 + (byte*) + \x00 + (byte*) + \x00
  Procedure _WriteRegExValue2Buffer(aTextNode:TALJSONNodeW);
  var LRegExOptions: TALPerlRegExOptions;
      LRegExOptionsStr: String;
  begin
    LRegExOptionsStr := '';
    LRegExOptions := aTextNode.RegExOptions;
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
  // \x0D + name + \x00 + length (int32) + string + \x00
  Procedure _WriteJavascriptValue2Buffer(aTextNode:TALJSONNodeW);
  var LInt32: system.int32;
      LJavascript: Tbytes;
  begin
    LJavascript := Tencoding.UTF8.GetBytes(aTextNode.Javascript);
    LInt32 := length(LJavascript) + 1 {for the trailing #0};
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _WriteBytes2Buffer(LJavascript);
    _WriteByte2Buffer($00);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x10 + name + \x00 + int32
  Procedure _WriteInt32Value2Buffer(aTextNode:TALJSONNodeW);
  var LInt32: system.Int32;
  begin
    LInt32 := aTextNode.int32;
    _Write2Buffer(LInt32, sizeOf(LInt32));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x12 + name + \x00 + int64
  Procedure _WriteInt64Value2Buffer(aTextNode:TALJSONNodeW);
  var LInt64: system.Int64;
  begin
    LInt64 := aTextNode.int64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteTextNode2Buffer(aTextNode:TALJSONNodeW; aNodeIndex: integer);
  Begin
    with aTextNode do begin

      // write the node subtype
      case NodeSubType of
        // \x01 + name + \x00 + double
        nstFloat: _WriteByte2Buffer($01);
        // \x02 + name + \x00 + length (int32) + string + \x00
        nstText: _WriteByte2Buffer($02);
        // \x05 + name + \x00 + int32 + subtype + (byte*)
        nstbinary: _WriteByte2Buffer($05);
        // \x07 + name + \x00 + (byte*12)
        nstObjectID: _WriteByte2Buffer($07);
        // \x08 + name + \x00 + \x00 => Boolean "false"
        // \x08 + name + \x00 + \x01	=> Boolean "true"
        nstBoolean: _WriteByte2Buffer($08);
        // \x09 + name + \x00 + int64
        nstDateTime: _WriteByte2Buffer($09);
        // \x11 + name + \x00 + int64
        nstTimestamp: _WriteByte2Buffer($11);
        // \x0A + name + \x00
        nstNull: _WriteByte2Buffer($0A);
        // \xOB + name + \x00 + (byte*) + \x00 + (byte*) + \x00
        nstRegEx: _WriteByte2Buffer($0B);
        // \x0D + name + \x00 + length (int32) + string + \x00
        nstJavascript: _WriteByte2Buffer($0D);
        // \x10 + name + \x00 + int32
        nstInt32: _WriteByte2Buffer($10);
        // \x12 + name + \x00 + int64
        nstInt64: _WriteByte2Buffer($12);
        else ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
      end;

      // write the nodename
      if (assigned(ParentNode)) and
         (ParentNode.NodeType = ntArray) then _WriteUTF8Str2Buffer(aNodeIndex)
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
        else ALJSONDocErrorW(cALJSONInvalidBSONNodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNodeW; aNodeIndex: integer);
  var LNodeList: TALJSONNodeListW;
      LEmptyNode: Boolean;
      LPos: system.int64;
      I: integer;
  Begin
    with aObjectNode do begin

      if aObjectNode = self then _WriteBytes2Buffer([$00,$00,$00,$00])
      else if (assigned(ParentNode)) and
              (ParentNode.NodeType = ntArray) then begin
        _WriteByte2Buffer($03);
        _WriteUTF8Str2Buffer(aNodeIndex);
        _WriteBytes2Buffer([$00,$00,$00,$00,$00]);
      end
      else begin
        _WriteByte2Buffer($03);
        _WriteUTF8Str2Buffer(NodeName);
        _WriteBytes2Buffer([$00,$00,$00,$00,$00]);
      end;

      LPos := StreamPos + BufferPos - 4{length of the #$00#$00#$00#$00};

      LEmptyNode := True;
      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            NodeStack.Push(aObjectNode);
            NodeIndexStack.Push(aNodeIndex);
            NodeStartPosStack.Push(LPos);
            For I := Count - 1 downto 0 do begin
              NodeStack.Push(Nodes[I]);
              NodeIndexStack.Push(I);
              NodeStartPosStack.Push(-1);
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
  Procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNodeW; aNodeStartPos: system.Int64);
  Begin
    _WriteByte2Buffer($00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNodeW; aNodeIndex: integer);
  var LNodeList: TALJSONNodeListW;
      LEmptyNode: Boolean;
      LPos: system.int64;
      I: integer;
  Begin
    with aArrayNode do begin

      if (assigned(ParentNode)) and
         (ParentNode.NodeType = ntArray) then begin
        _WriteByte2Buffer($04);
        _WriteUTF8Str2Buffer(aNodeIndex);
        _WriteBytes2Buffer([$00,$00,$00,$00,$00]);
      end
      else begin
        _WriteByte2Buffer($04);
        _WriteUTF8Str2Buffer(NodeName);
        _WriteBytes2Buffer([$00,$00,$00,$00,$00]);
      end;

      LPos := StreamPos + BufferPos - 4{length of the #$00+#$00+#$00+#$00};

      LEmptyNode := True;
      LNodeList := InternalGetChildNodes;
      If assigned(LNodeList) then begin
        with LNodeList do
          If count > 0 then begin
            LEmptyNode := False;
            NodeStack.Push(aArrayNode);
            NodeIndexStack.Push(aNodeIndex);
            NodeStartPosStack.Push(LPos);
            For I := Count - 1 downto 0 do begin
              NodeStack.Push(Nodes[I]);
              NodeIndexStack.Push(I);
              NodeStartPosStack.Push(-1);
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
  Procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNodeW; aNodeStartPos: system.Int64);
  Begin
    _WriteByte2Buffer($00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

begin
  If NodeType <> ntobject then exit;

  CurrentParentNode := nil;
  NodeStack := Tstack<TALJSONNodeW>.Create;
  NodeIndexStack := TALintegerList.Create;
  NodeStartPosStack := TALInt64List.Create;
  Try

    {init buffer string}
    Setlength(Buffer, BufferSize); // will make buffer uniquestring
    BufferPos := 0;
    if assigned(Stream) then StreamPos := Stream.Position
    else StreamPos := 0;

    {SaveOnlyChildNode}
    NodeStack.Push(self);
    NodeIndexStack.Push(0);
    NodeStartPosStack.Push(StreamPos);


    {loop on all nodes}
    While NodeStack.Count > 0 Do begin
      CurrentNode := NodeStack.Pop;
      CurrentNodeIndex := integer(NodeIndexStack.Pop);
      CurrentNodeStartPos := integer(NodeStartPosStack.Pop);

      with CurrentNode do
        case NodeType of
          ntObject: begin
                      if currentNode = CurrentParentNode then _WriteEndObjectNode2Buffer(CurrentNode, CurrentNodeStartPos)
                      else _WriteStartObjectNode2Buffer(CurrentNode, CurrentNodeIndex);
                    end;
          ntArray: begin
                      if currentNode = CurrentParentNode then _WriteEndArrayNode2Buffer(CurrentNode, CurrentNodeStartPos)
                      else _WriteStartArrayNode2Buffer(CurrentNode, CurrentNodeIndex);
                   end;
          ntText: _WriteTextNode2Buffer(CurrentNode, CurrentNodeIndex);
          else ALJSONDocErrorW(cAlJSONInvalidNodeType);
        end;

      CurrentParentNode := CurrentNode.ParentNode;
    end;

    {Write the buffer}
    if assigned(Stream) then _WriteBuffer2Stream(Buffer, BufferPos)
    else setlength(Buffer,BufferPos);

  finally
    ALFreeAndNil(NodeStack);
    ALFreeAndNil(NodeIndexStack);
    ALFreeAndNil(NodeStartPosStack);
  end;
end;

{*****************************************************************************************************}
procedure TALJSONNodeW.SaveToBsonStream(const Stream: TStream; const Options: TALJSONSaveOptions = []);
var buffer: Tbytes;
begin
  SaveToBson(Stream, buffer, Options);
end;

{****************************************************************************************************}
procedure TALJSONNodeW.SaveToBsonFile(const FileName: String; const Options: TALJSONSaveOptions = []);
Var LfileStream: TfileStream;
    LTmpFilename: String;
begin
  if soProtectedSave in Options then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LfileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToBsonStream(LfileStream, Options);
    finally
      ALFreeAndNil(LfileStream);
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
procedure TALJSONNodeW.SaveToBsonBytes(var Bytes: Tbytes; const Options: TALJSONSaveOptions = []);
begin
  SaveToBson(nil, Bytes, Options);
end;

{*********************************************************************************************************************}
procedure TALJSONNodeW.LoadFromJSONString(const Str: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  Try
    ParseJson(Str, False{SaxMode}, nil{onParseText}, nil{onParseStartObject}, nil{onParseEndObject}, nil{onParseStartArray}, nil{onParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*************************************************************************************************************************}
procedure TALJSONNodeW.LoadFromJSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  LoadFromJSONString(ALGetStringFromStream(Stream, TEncoding.UTF8), Options);
end;

{************************************************************************************************************************}
procedure TALJSONNodeW.LoadFromJSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromJSONStream(LfileStream, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{**********************************************************************************************************************}
procedure TALJSONNodeW.LoadFromBSONBytes(const Bytes: Tbytes; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  Try
    ParseBSON(Bytes, False{SaxMode}, nil{onParseText}, nil{onParseStartObject}, nil{onParseEndObject}, nil{onParseStartArray}, nil{onParseEndArray}, Options);
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*************************************************************************************************************************}
procedure TALJSONNodeW.LoadFromBSONStream(const Stream: TStream; const Options: TALJSONParseOptions = [poClearChildNodes]);
Begin
  LoadFromBSONBytes(ALGetBytesFromStream(Stream), Options);
end;

{************************************************************************************************************************}
procedure TALJSONNodeW.LoadFromBSONFile(const FileName: String; const Options: TALJSONParseOptions = [poClearChildNodes]);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromBSONStream(LfileStream, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{*************************************}
procedure TALJSONNodeW.ParseJSONString(
            const Str: String;
            const onParseText: TAlJSONParseTextEventW;
            const onParseStartObject: TAlJSONParseObjectEventW;
            const onParseEndObject: TAlJSONParseObjectEventW;
            const onParseStartArray: TAlJSONParseArrayEventW;
            const onParseEndArray: TAlJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
Begin
  ParseJson(Str, true{SaxMode}, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeW.ParseJSONStream(
            const Stream: TStream;
            const onParseText: TAlJSONParseTextEventW;
            const onParseStartObject: TAlJSONParseObjectEventW;
            const onParseEndObject: TAlJSONParseObjectEventW;
            const onParseStartArray: TAlJSONParseArrayEventW;
            const onParseEndArray: TAlJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
Begin
  ParseJSONString(ALGetStringFromStream(Stream, TEncoding.UTF8), onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{***********************************}
procedure TALJSONNodeW.ParseJSONFile(
            const FileName: String;
            const onParseText: TAlJSONParseTextEventW;
            const onParseStartObject: TAlJSONParseObjectEventW;
            const onParseEndObject: TAlJSONParseObjectEventW;
            const onParseStartArray: TAlJSONParseArrayEventW;
            const onParseEndArray: TAlJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseJSONStream(LfileStream, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{************************************}
procedure TALJSONNodeW.ParseBSONBytes(
            const Bytes: Tbytes;
            const onParseText: TAlJSONParseTextEventW;
            const onParseStartObject: TAlJSONParseObjectEventW;
            const onParseEndObject: TAlJSONParseObjectEventW;
            const onParseStartArray: TAlJSONParseArrayEventW;
            const onParseEndArray: TAlJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
Begin
  ParseBSON(Bytes, true{SaxMode}, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{*************************************}
procedure TALJSONNodeW.ParseBSONStream(
            const Stream: TStream;
            const onParseText: TAlJSONParseTextEventW;
            const onParseStartObject: TAlJSONParseObjectEventW;
            const onParseEndObject: TAlJSONParseObjectEventW;
            const onParseStartArray: TAlJSONParseArrayEventW;
            const onParseEndArray: TAlJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
Begin
  ParseBSONBytes(ALGetBytesFromStream(Stream), onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
end;

{***********************************}
procedure TALJSONNodeW.ParseBSONFile(
            const FileName: String;
            const onParseText: TAlJSONParseTextEventW;
            const onParseStartObject: TAlJSONParseObjectEventW;
            const onParseEndObject: TAlJSONParseObjectEventW;
            const onParseStartArray: TAlJSONParseArrayEventW;
            const onParseEndArray: TAlJSONParseArrayEventW;
            const Options: TALJSONParseOptions = []);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    ParseBSONStream(LfileStream, onParseText, onParseStartObject, onParseEndObject, onParseStartArray, onParseEndArray, Options);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{*****************************************************************}
constructor TALJSONObjectNodeW.Create(const NodeName: String = '');
begin
  inherited create(NodeName);
  FChildNodes := nil;
end;

{************************************}
destructor TALJSONObjectNodeW.Destroy;
begin
  If assigned(FChildNodes) then ALFreeAndNil(FchildNodes);
  inherited;
end;

{**********************************************************}
function TALJSONObjectNodeW.GetChildNodes: TALJSONNodeListW;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{************************************************************************}
procedure TALJSONObjectNodeW.SetChildNodes(const Value: TALJSONNodeListW);
begin
  If Assigned(FChildNodes) then ALFreeAndNil(FchildNodes);
  FChildNodes := Value;
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

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONObjectNodeW.InternalGetChildNodes: TALJSONNodeListW;
begin
  Result := FChildNodes;
end;

{****************************************************************}
constructor TALJSONArrayNodeW.Create(const NodeName: String = '');
begin
  inherited create(NodeName);
  FChildNodes := nil;
end;

{***********************************}
destructor TALJSONArrayNodeW.Destroy;
begin
  If assigned(FChildNodes) then ALFreeAndNil(FchildNodes);
  inherited;
end;

{*********************************************************}
function TALJSONArrayNodeW.GetChildNodes: TALJSONNodeListW;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{***********************************************************************}
procedure TALJSONArrayNodeW.SetChildNodes(const Value: TALJSONNodeListW);
begin
  If Assigned(FChildNodes) then ALFreeAndNil(FchildNodes);
  FChildNodes := Value;
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

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONArrayNodeW.InternalGetChildNodes: TALJSONNodeListW;
begin
  Result := FChildNodes;
end;

{***************************************************************}
constructor TALJSONTextNodeW.Create(const NodeName: String = '');
begin
  inherited create(NodeName);
  fNodeSubType := nstText;
  fRawNodeValueStr := '';
  FRawNodeValueInt64 := 0;
  fRawNodeValueDefined := [nvStr];
end;

{*****************************************************}
function TALJSONTextNodeW.GetNodeType: TALJSONNodeType;
begin
  Result := NtText;
end;

{***********************************************************}
function TALJSONTextNodeW.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := fNodeSubType;
end;

{************************************************}
function TALJSONTextNodeW.GetNodeValueStr: String;
begin
  if nvStr in fRawNodeValueDefined then result := fRawNodeValueStr
  else begin

    if not (nvInt64 in fRawNodeValueDefined) then ALJSONDocErrorW(CALJsonOperationError,GetNodeType);

    case fNodeSubType of
      nstFloat: ALFloatToStrW(GetFloat, fRawNodeValueStr, ALDefaultFormatSettingsW);
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      nstBoolean: ALBoolToStrW(fRawNodeValueStr, getBool, 'true', 'false');
      nstDateTime: ALDateTimeToStrW(GetDateTime, fRawNodeValueStr, ALDefaultFormatSettingsW);
      nstNull: fRawNodeValueStr := 'null';
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
      nstInt32: ALIntToStrW(GetInt32, fRawNodeValueStr);
      nstTimestamp: ALFormatW('Timestamp(%u, %u)', [GetTimestamp.W1,GetTimestamp.W2], fRawNodeValueStr);
      nstInt64: ALIntToStrW(GetInt64, fRawNodeValueStr);
      else ALJSONDocErrorW(CALJsonOperationError,GetNodeType);
    end;

    fRawNodeValueDefined := fRawNodeValueDefined + [nvStr];
    result := fRawNodeValueStr;

  end;
end;

{*************************************************}
function TALJSONTextNodeW.GetNodeValueInt64: int64;
var LDouble: Double;
    LBool: boolean;
    LDateTime: TdateTime;
    LInt32: system.int32;
    LTimestamp: TALBSONTimestamp;
begin
  if nvInt64 in fRawNodeValueDefined then result := fRawNodeValueInt64
  else begin

    if not (nvStr in fRawNodeValueDefined) then ALJSONDocErrorW(CALJsonOperationError,GetNodeType);

    case fNodeSubType of
      nstFloat: begin
                  IF not ALTryStrToFloat(fRawNodeValueStr, LDouble, ALDefaultFormatSettingsW) then ALJSONDocErrorW('%s is not a valid Float', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := Pint64(@LDouble)^;
                end;
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      nstBoolean: begin
                    IF not ALTryStrToBool(fRawNodeValueStr, LBool) then ALJSONDocErrorW('%s is not a valid Boolean', [fRawNodeValueStr]);
                    fRawNodeValueInt64 := ALBoolToInt(LBool);
                  end;
      nstDateTime: begin
                     IF not ALTryStrToDateTime(fRawNodeValueStr, LDateTime, ALDefaultFormatSettingsW) then ALJSONDocErrorW('%s is not a valid Datetime', [fRawNodeValueStr]);
                     fRawNodeValueInt64 := Pint64(@LDateTime)^;
                   end;
      nstNull:  begin
                  fRawNodeValueInt64 := 0;
                end;
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
      nstInt32: begin
                  IF not ALTryStrToInt(fRawNodeValueStr, LInt32) then ALJSONDocErrorW('%s is not a valid Int32', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := LInt32;
                end;
      nstTimestamp: begin
                      IF not ALJSONTryStrToTimestampW(fRawNodeValueStr, LTimestamp) then ALJSONDocErrorW('%s is not a valid Timestamp', [fRawNodeValueStr]);
                      fRawNodeValueInt64 := LTimestamp.I64;
                    end;
      nstInt64: begin
                  IF not ALTryStrToInt64(fRawNodeValueStr, fRawNodeValueInt64) then ALJSONDocErrorW('%s is not a valid Int64', [fRawNodeValueStr]);
                end;
      else ALJSONDocErrorW(CALJsonOperationError,GetNodeType);
    end;

    fRawNodeValueDefined := fRawNodeValueDefined + [nvInt64];
    result := fRawNodeValueInt64;

  end;
end;

{**************************************************************************************************}
procedure TALJSONTextNodeW.SetNodeValue(const Value: String; const NodeSubType: TALJSONNodeSubType);
begin
  fNodeSubType := NodeSubType;
  fRawNodeValueStr := Value;
  fRawNodeValueDefined := [nvStr];
end;

{*************************************************************************************************}
procedure TALJSONTextNodeW.SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  fNodeSubType := NodeSubType;
  fRawNodeValueInt64 := Value;
  if (NodeSubType in [nstBinary, nstRegEx]) then fRawNodeValueDefined := fRawNodeValueDefined + [nvInt64] // keep the fNodeValueStr
  else fRawNodeValueDefined := [nvInt64];
end;

{******************************************************************************************************************************}
procedure TALJSONTextNodeW.SetNodeValue(const StrValue: String; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  fNodeSubType := NodeSubType;
  fRawNodeValueStr := StrValue;
  fRawNodeValueInt64 := Int64Value;
  fRawNodeValueDefined := [nvStr, nvInt64];
end;

{*******************************************************}
constructor TALJSONNodeListW.Create(Owner: TALJSONNodeW);
begin
  FList:= nil;
  FCount:= 0;
  FCapacity := 0;
  FOwner := Owner;
  FSorted := False;
  FDuplicates := dupAccept;
  SetSorted(False);
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
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareNodeNames(FList[I].NodeName, NodeName);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
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
  if not Sorted then begin
    if Direction = TDirection.FromBeginning then begin
      for Result := 0 to Count - 1 do
        if FList[Result].NodeName = Name then Exit;
    end
    else begin
      for Result := Count - 1 downto 0 do
        if FList[Result].NodeName = Name then Exit;
    end;
    Result := -1;
  end
  else if not Find(Name, Result) then Result := -1;
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
function TALJSONNodeListW.IndexOfValue(const Value: integer; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].int32 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].int32 = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************************************************************************************************}
function TALJSONNodeListW.IndexOfValue(const Value: int64; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (FList[Result].int64 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (FList[Result].int64 = Value) then Exit;
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
var Index: Integer;
begin
  Index := IndexOf(NodeName, Direction);
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
  else result := nil;
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
var Index: Integer;
begin
  Index := IndexOf(Node) + Delta;
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
  if (Index < 0) or (Index >= FCount) then ALJSONDocErrorW(CALJSONListIndexError, [Index]);
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
  if not Assigned(Result) then ALJSONDocErrorW(CALJSONNodeNotFound, [Name]);
end;

{************************************************************************}
function TALJSONNodeListW.CompareNodeNames(const S1, S2: String): Integer;
begin
  Result := ALCompareStrW(S1, S2)
end;

{*****************************************************************************************}
procedure TALJSONNodeListW.QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareW);
var
  I, J, P: Integer;
begin
  while L < R do
  begin
    if (R - L) = 1 then
    begin
      if ACompare(Self, L, R) > 0 then
        Exchange(L, R);
      break;
    end;
    I := L;
    J := R;
    P := (L + R) shr 1;
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
  if (not Sorted) and (FList <> nil) and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

{**************************************}
{Adds a new node to the end of the list.
 Call Add to add a node to the end of the list. Add returns the index of the node once it is added, where 0 is the index
 of the first node in the list, 1 is the index of the second node, and so on.
 *Node is the node to add to the list.}
function TALJSONNodeListW.Add(const Node: TALJSONNodeW): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(Node.NodeName, Result) then
      case Duplicates of
        dupIgnore: begin
                     ALFreeAndNil(Node);
                     Exit;
                   end;
        dupError: ALJSONDocErrorW(cALJSONDuplicateNodeName);
      end;
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
  Node.SetParentNode(Fowner);
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
    if Sorted then ALJSONDocErrorW(CALJSONSortedListError);
    if (Index < 0) or (Index > FCount) then ALJSONDocErrorW(CALJSONListIndexError, [Index]);
    InternalInsert(Index, Node);
  end;
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Index identifies the node to remove by index rather than name. Index ranges from 0 to one less than the value of the Count property.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Index Delete returns –1.}
function TALJSONNodeListW.Delete(const Index: Integer): Integer;
var Node: TALJSONNodeW;
begin
  Node := Get(Index);
  FList[Index] := nil; // to decrease the refcount of Node
  Dec(FCount);
  if Index < FCount then begin
    ALMove(
      FList[Index + 1],
      FList[Index],
      (FCount - Index) * SizeOf(Pointer));
    Pointer(FList[FCount]) := nil;
  end;
  if assigned(Node) then ALFreeAndNil(Node);
  result := Index;
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Name identifies the node to remove from the list. This is the local name of the node to remove.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Name, Delete returns –1.}
function TALJSONNodeListW.Delete(const Name: String): Integer;
begin
  result := indexOf(Name);
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
var I: Integer;
begin
  Result := nil;
  I := IndexOf(Node);
  if I >= 0 then result := Extract(i);
end;

{***********************************************************}
procedure TALJSONNodeListW.Exchange(Index1, Index2: Integer);
var Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then ALJSONDocErrorW(cALJSONListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then ALJSONDocErrorW(cALJSONListIndexError, [Index2]);
  Item := pointer(FList[Index1]);
  pointer(FList[Index1]) := pointer(FList[Index2]);
  pointer(FList[Index2]) := Item;
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeListW.Extract(const index: integer): TALJSONNodeW;
begin
  Result := Get(index);
  Result.SetParentNode(nil);
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
var Index: Integer;
begin
  Index := indexOf(OldNode);
  Result := Extract(Index);
  if not sorted then Insert(Index, NewNode)
  else Add(NewNode);
end;

{*******************************}
{Removes all nodes from the list.
 Call Clear to empty the list.
 Note:	Clear does not call the BeginUpdate and EndUpdate methods, even though it may result in the
 deletion of more than one node.}
procedure TALJSONNodeListW.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{******************************}
procedure TALJSONNodeListW.Grow;
{$IF CompilerVersion <= 32}{tokyo}
var Delta: Integer;
{$endif}
begin
  {$IF CompilerVersion <= 32}{tokyo}
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
  {$else}
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
  {$endif}
end;

{***********************************************************}
procedure TALJSONNodeListW.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) then ALJSONDocErrorW(CALJSONListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*****************************************************************************}
procedure TALJSONNodeListW.SetDuplicates(Value: TDuplicates; Recurse: Boolean);
begin
  FDuplicates := Value;
  if Recurse then begin
    for Var I := 0 to count-1 do begin
      Var LNodeList := Get(i).InternalGetChildNodes;
      if LNodeList <> nil then LNodeList.SetDuplicates(Value,Recurse);
    end;
  end;
end;

{***********************************************************}
procedure TALJSONNodeListW.SetDuplicates(Value: TDuplicates);
begin
  SetDuplicates(Value, False);
end;

{*********************************************************************}
procedure TALJSONNodeListW.SetSorted(Value: Boolean; Recurse: Boolean);
begin
  if FSorted <> Value then
  begin
    if owner is TALJSONObjectNodeW then begin
      if Value then Sort;
      FSorted := Value;
    end
    else FSorted := False;
  end;
  if Recurse then begin
    for Var I := 0 to count-1 do begin
      Var LNodeList := Get(i).InternalGetChildNodes;
      if LNodeList <> nil then LNodeList.SetSorted(Value,Recurse);
    end;
  end;
end;

{***************************************************}
procedure TALJSONNodeListW.SetSorted(Value: Boolean);
begin
  SetSorted(Value, False);
end;

{*****************************************************}
procedure TALJSONNodeListW.SetCount(NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) then ALJSONDocErrorW(CALJSONListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{**************************}
Procedure ALJSONToTStringsW(
            const AJsonStr: String;
            const aFormatSettings: TALFormatSettingsW;
            const aPath: String;
            const aLst: TALStringsW;
            Const aNullStr: String = 'null';
            Const aTrueStr: String = 'true';
            Const aFalseStr: String = 'false');
var LContainChilds: boolean;
begin
  LContainChilds := False;
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
    end{onParseText},
    //--
    procedure (Sender: TObject; const Path: String; const Name: String)
    begin
      LContainChilds := False;
    end{onParseStartObject},
    //--
    procedure (Sender: TObject; const Path: String; const Name: String)
    begin
      if (not LContainChilds) and (aPath + Path <> ''{Path = '' mean it's the root object}) then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '{}');
      LContainChilds := True;
    end{onParseEndObject},
    //--
    procedure (Sender: TObject; const Path: String; const Name: String)
    begin
      LContainChilds := False;
    end{onParseStartArray},
    //--
    procedure (Sender: TObject; const Path: String; const Name: String)
    begin
      if not LContainChilds then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '[]');
      LContainChilds := True;
    end{onParseEndArray});
end;

{**************************}
Procedure ALJSONToTStringsW(
            const AJsonStr: String;
            const aFormatSettings: TALFormatSettingsW;
            const aLst: TALStringsW;
            Const aNullStr: String = 'null';
            Const aTrueStr: String = 'true';
            Const aFalseStr: String = 'false');
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
Procedure ALJSONToTStringsW(
            const aJsonNode: TALJSONNodeW;
            Const aPath: String;
            const aLst: TALStringsW;
            Const aNullStr: String = 'null';
            Const aTrueStr: String = 'true';
            Const aFalseStr: String = 'false');
var LTmpPath: String;
    I: integer;
begin
  if aJsonNode.ChildNodes.Count > 0 then begin
    for I := 0 to aJsonNode.ChildNodes.Count - 1 do begin

      if aJsonNode.NodeType = ntArray then LTmpPath := aPath + '[' + ALIntToStrW(I) + ']'
      else begin
        if aJsonNode.ChildNodes[I].NodeName = '' then raise Exception.Create('Nodename can not be empty');
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

        else raise Exception.Create('Unknown NodeType');

      end;
    end;
  end
  else if (aPath <> ''{aPath = '' mean it's the root object}) then begin
    if      aJsonNode.NodeType = ntArray  then aLst.Add(aPath + aLst.NameValueSeparator + '[]')
    else if aJsonNode.NodeType = ntObject then aLst.Add(aPath + aLst.NameValueSeparator + '{}');
  end;
end;

{**************************}
Procedure ALJSONToTStringsW(
            const aJsonNode: TALJSONNodeW;
            const aLst: TALStringsW;
            Const aNullStr: String = 'null';
            Const aTrueStr: String = 'true';
            Const aFalseStr: String = 'false');
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
            const aLst: TALStringsW;
            const aJsonNode: TALJSONNodeW;
            Const aPath: String = '';
            Const aNameToLowerCase: boolean = false;
            Const aNullStr: String = 'null');

var LIndex: Integer;
    LNames:  TALStringListW;
    LLowerName: String;
    LCurrJsonNode, aTmpJsonNode: TALJSONNodeW;
    I, J: integer;

begin

  // create list of the part of name,
  // from "aggregated_data.properties.types[3].translations.usa" =>
  //   aggregated_data
  //   properties
  //   types
  //   [3]
  //   translations
  //   usa
  LNames := TALStringListW.Create;
  try

    //init aNames.linebreak
    LNames.LineBreak := '.';

    // scroll the aLst
    for I := 0 to aLst.Count - 1 do begin

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
        LCurrJsonNode := aJsonNode;
        for J := 0 to LNames.Count - 1 do begin

          //if we are in array
          if LCurrJsonNode.NodeType = ntArray then begin
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
            LLowerName := alifThenW(aNameToLowerCase, AlLowerCase(LNames[J]), LNames[J]);
            aTmpJsonNode := LCurrJsonNode.ChildNodes.FindNode(LLowerName);
            if not assigned(aTmpJsonNode) then begin
              if J = LNames.Count - 1 then LCurrJsonNode := LCurrJsonNode.AddChild(LLowerName, ntText)
              else if (LNames[J+1] <> '') and
                      (LNames[J+1][1] = '[') then LCurrJsonNode := LCurrJsonNode.AddChild(LLowerName, ntarray)
              else LCurrJsonNode := LCurrJsonNode.AddChild(LLowerName, ntObject);
            end
            else LCurrJsonNode := aTmpJsonNode;
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
function ALJsonEncodeFloatWithNodeSubTypeHelperW(const aValue: double): String;
begin
  result := ALFloatToStrW(aValue, ALDefaultFormatSettingsW);
end;

{****************************************************************************}
function ALJsonEncodeTextWithNodeSubTypeHelperW(const aValue: String): String;
begin
  result := '"'+ALJavascriptEncode(aValue)+'"';
end;

{******************************************************************************}
function ALJsonEncodeBinaryWithNodeSubTypeHelperW(const aValue: String): String;
begin
  result := 'BinData(0, "' + aValue + '")';
end;

{********************************************************************************}
function ALJsonEncodeObjectIDWithNodeSubTypeHelperW(const aValue: String): String;
begin
  result := 'ObjectId("'+aValue+'")';
end;

{********************************************************************************}
function ALJsonEncodeBooleanWithNodeSubTypeHelperW(const aValue: Boolean): String;
begin
  if aValue then result := 'true'
  else result := 'false';
end;

{***********************************************************************************}
function ALJsonEncodeDateTimeWithNodeSubTypeHelperW(const aValue: TdateTime): String;
begin
  result := ALFormatDateTimeW('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', aValue, ALDefaultFormatSettingsW);
end;

{**********************************************************************************}
function ALJsonEncodeJavascriptWithNodeSubTypeHelperW(const aValue: String): String;
begin
  result := aValue;
end;

{****************************************************************************}
function ALJsonEncodeInt64WithNodeSubTypeHelperW(const aValue: int64): String;
begin
  result := 'NumberLong(' + ALIntToStrW(aValue) + ')';
end;

{****************************************************************************}
function ALJsonEncodeInt32WithNodeSubTypeHelperW(const aValue: int32): String;
begin
  result := 'NumberInt(' + ALIntToStrW(aValue) + ')';
end;

{******************************************************}
function ALJsonEncodeNullWithNodeSubTypeHelperW: String;
begin
  result := 'null';
end;

{******************************************}
function ALJsonEncodeWithNodeSubTypeHelperW(
           const aValue: String;
           const aNodeSubType: TALJSONNodeSubType;
           const aFormatSettings: TALFormatSettingsW): String;
begin
  case aNodeSubType of
    nstFloat:      begin
                     if @aFormatSettings <> @ALDefaultFormatSettingsW then result := ALJsonEncodeFloatWithNodeSubTypeHelperW(ALStrToFloat(aValue, aFormatSettings))
                     else result := aValue;
                   end;
    nstText:       result := ALJsonEncodeTextWithNodeSubTypeHelperW(aValue);
    nstBinary:     result := ALJsonEncodeBinaryWithNodeSubTypeHelperW(aValue);
    nstObjectID:   result := ALJsonEncodeObjectIDWithNodeSubTypeHelperW(aValue);
    nstBoolean:    result := ALJsonEncodeBooleanWithNodeSubTypeHelperW(AlStrToBool(aValue));
    nstDateTime:   begin
                     if aValue = 'NOW' then result := ALJsonEncodeDateTimeWithNodeSubTypeHelperW(ALUtcNow)
                     else result := ALJsonEncodeDateTimeWithNodeSubTypeHelperW(ALStrToDateTime(aValue, aFormatSettings));
                   end;
    nstJavascript: result := ALJsonEncodeJavascriptWithNodeSubTypeHelperW(aValue);
    nstInt32:      result := ALJsonEncodeInt32WithNodeSubTypeHelperW(ALStrToInt(aValue));
    nstInt64:      result := ALJsonEncodeInt64WithNodeSubTypeHelperW(ALStrToInt64(aValue));
    nstNull:       result := ALJsonEncodeNullWithNodeSubTypeHelperW;
    nstObject:     raise Exception.Create('Unsupported Node SubType');
    nstArray:      raise Exception.Create('Unsupported Node SubType');
    nstRegEx:      raise Exception.Create('Unsupported Node SubType');
    nstTimestamp:  raise Exception.Create('Unsupported Node SubType');
    else raise Exception.Create('Unknown Node SubType');
  end;
end;

initialization

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
