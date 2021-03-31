(*******************************************************************************
TALJsonDocument is a Delphi parser/writer for JSON / BSON data
format. it's support DOM and SAX parser, support BSON format,
and use a similar syntax than TALXMLDocument / TXMLDocument.
TALJsonDocument can also export Json / Bson data in TALStringList.

When it deals with parsing some (textual) content, two directions
are usually envisaged. In the JSON world, you have usually to
make a choice between:
- A DOM parser, which creates an in-memory tree structure of
  objects mapping the JSON content;
- A SAX parser, which reads the JSON content, then call pre-defined
  events for each JSON content element.

In fact, DOM parsers use internally a SAX parser to read the JSON
content. Therefore, with the overhead of object creation and
their property initialization, DOM parsers are typically three
to five times slower than SAX (and use much much more memory to
store all the nodes). But, DOM parsers are much more powerful for
handling the data: as soon as it's mapped in native objects,
code can access with no time to any given node, whereas a
SAX-based access will have to read again the whole JSON content.

Most JSON parser available in Delphi use a DOM-like approach.
For instance, the DBXJSON unit included since Delphi 2010
or the SuperObject library create a class instance mapping
each JSON node. In order to achieve best speed, TALJsonDocument
implement DOM parser and also a SAX parser.

TALJsonDocument syntax is very similar
to TALXMLdocument / TXMLDocument

exemple :

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

MyJsonDoc.loadFromJson(AJsonStr, False);
MyJsonDoc.childnodes['_id'].int32;
MyJsonDoc.childnodes['name'].childnodes['first'].text;
MyJsonDoc.childnodes['name'].childnodes['last'].text;
MyJsonDoc.childnodes['birth'].datetime;
for i := 0 to MyJsonDoc.childnodes['contribs'].ChildNodes.count - 1 do
  MyJsonDoc.childnodes['contribs'].childnodes[i].text;
for i := 0 to MyJsonDoc.childnodes['awards'].ChildNodes.count - 1 do begin
  MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['award'].text;
  MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['year'].text;
  MyJsonDoc.childnodes['awards'].childnodes[i].childnodes['by'].text;
end;

------------------------------
To create the document nodes :

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

MyJsonDoc.LoadFromFile(aBSONFileName, False{saxMode}, True{BSON});
MyJsonDoc.SaveToFile(aBSONFileName, False{saxMode}, True{BSON});

---------------------------------------
To parse an JSON document in Sax Mode :

MyJsonDoc.onParseText := procedure (Sender: TObject;
                                    const Path: AnsiString;
                                    const name: AnsiString;
                                    const Args: array of const;
                                    NodeSubType: TALJSONNodeSubType)
                         begin
                           case NodeSubType of
                             nstFloat: Writeln(Path + '=' + ALFloatToStr(Args[0].VExtended^, ALDefaultFormatSettings));
                             nstText: Writeln(Path + '=' + ansiString(Args[0].VAnsiString));
                             nstObjectID: Writeln(Path + '=' + 'ObjectId("'+ALBinToHex(ansiString(Args[0].VAnsiString))+'")');
                             nstBoolean: Writeln(Path + '=' + ALBoolToStr(Args[0].VBoolean,'true','false'));
                             nstDateTime: Writeln(Path + '=' + ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettings));
                             nstNull: Writeln(Path + '=' + 'null');
                             nstRegEx: Writeln(Path + '=' + ansiString(Args[0].VAnsiString));
                             nstBinary: Writeln(Path + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+ansiString(ALBase64EncodeStringNoCRLF(ansiString(Args[0].VAnsiString)))+'")');
                             nstJavascript: Writeln(Path + '=' + ansiString(Args[0].VAnsiString));
                             nstInt32: Writeln(Path + '=' + 'NumberInt('+inttostr(Args[0].VInteger)+')');
                             nstTimestamp: Writeln(Path + '=' + 'Timestamp('+inttostr(int64(cardinal(Args[0].VInteger)))+', '+inttostr(int64(cardinal(Args[1].VInteger)))+')');
                             nstInt64: Writeln(Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                           end;
                         end;
MyJsonDoc.LoadFromJSON(AJsonStr, true{saxMode});
*******************************************************************************)

unit ALJSONDoc;

interface

{$I Alcinoe.inc}

uses
  system.Classes,
  system.sysutils,
  system.types,
  {$IFNDEF ALHideAnsiString}
    ALXmlDoc,
  {$ENDIF}
  ALString,
  AlStringList;

const
  cALJSONNotActive              = 'No active document';
  cAlJSONNodeNotFound           = 'Node "%s" not found';
  cALJSONInvalidNodeType        = 'Invalid node type';
  cALJSONInvalidBSONNodeSubType = 'Invalid node sub type';
  cALJSONListCapacityError      = 'Node list capacity out of bounds (%d)';
  cALJSONListCountError         = 'Node list count out of bounds (%d)';
  cALJSONListIndexError         = 'Node list index out of bounds (%d)';
  cALJSONOperationError         = 'This operation can not be performed with a node of type %s';
  cALJSONParseError             = 'JSON Parse error';
  cALBSONParseError             = 'BSON Parse error';
  cALJSONImmutable              = 'The document is immutable';

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

  TALJSONDocOption = (doNodeAutoCreate, // create only ntText Node !
                      doNodeAutoIndent, // affect only the SaveToStream
                      doProtectedSave,     // save first to a tmp file and then later rename the tmp file to the desired filename
                      doImmutable); // the doc is immutable and can not be updated (usefull when sharing a document across multiple threads)
  TALJSONDocOptions = set of TALJSONDocOption;

  TALJSONParseOption = (poIgnoreControlCharacters, // don't decode escaped characters (like \") and not encode them also (when save / load)
                        poSkipNodeSubTypeHelper,   // don't use helper functions like NumberLong() to handle 64-bit integers or NumberInt()
                                                   // to handle 32-bit integers
                        poSaveInt64AsText,         // JS represents all numbers as double, and with growing integers you loose precision at some point
                                                   // use this option to return Int64 as string
                        poAllowComments);          // allow comments inside the Json Source file. ex:
                                                   //{
                                                   //  "nodename": "nodevalue",  // your comments here
                                                   //}
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

{$IFNDEF ALHideAnsiString}

type

  {class definition}
  TALJSONNode = Class;
  TALJSONNodeList= Class;
  TALJSONDocument= Class;

  TAlJSONParseDocument = reference to procedure (Sender: TObject);
  TAlJSONParseTextEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType);
  TAlJSONParseObjectEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);
  TAlJSONParseArrayEvent = reference to procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString);

  TALJSONNodeListSortCompare = reference to function(List: TALJSONNodeList; Index1, Index2: Integer): Integer;

  TALJSONPointerList = array of TALJSONNode;

  {TALJSONNodeList}
  {TALJSONNodeList is used to represent a set of related nodes (TALJSONNode object) in an JSON document. For example, TALJSONNodeList is used to
   represent all of the children of a node, or all of the attributes of a node. TALJSONNodeList can be used to add or delete nodes from the
   List, or to access specific nodes.}
  TALJSONNodeList = class(Tobject)
  Private
    FCapacity: Integer;
    FCount: integer;
    FList: TALJSONPointerList;
    [weak] FOwner: TALJSONNode;
    procedure QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompare);
  protected
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    property Owner: TALJSONNode read FOwner;
    function Get(Index: Integer): TALJSONNode;
    function GetNodeByIndex(Const Index: Integer): TALJSONNode;
    function GetNodeByName(Const Name: AnsiString): TALJSONNode;
  public
    constructor Create(Owner: TALJSONNode);
    destructor Destroy; override;
    procedure CustomSort(Compare: TALJSONNodeListSortCompare);
    function Add(const Node: TALJSONNode): Integer;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: AnsiString): Integer; overload;
    function Extract(const index: integer): TALJSONNode; overload;
    function Extract(const Node: TALJSONNode): TALJSONNode; overload;
    procedure Exchange(Index1, Index2: Integer);
    function FindNode(const NodeName: AnsiString; const Direction: TDirection = TDirection.FromBeginning): TALJSONNode; overload;
    function FindSibling(const Node: TALJSONNode; Delta: Integer): TALJSONNode;
    function First: TALJSONNode;
    function IndexOf(const Name: AnsiString; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOf(const Node: TALJSONNode; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: ansiString; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: integer; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: int64; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function Last: TALJSONNode;
    function Remove(const Node: TALJSONNode): Integer;
    function ReplaceNode(const OldNode, NewNode: TALJSONNode): TALJSONNode;
    procedure Clear;
    procedure Insert(Index: Integer; const Node: TALJSONNode);
    property Count: Integer read fCount;
    property Nodes[const Name: AnsiString]: TALJSONNode read GetNodeByName; default;
    property Nodes[const Index: integer]: TALJSONNode read GetNodeByIndex; default;
  end;

  {TALJSONNode}
  {TALJSONNode represents a node in an JSON document.}
  TALJSONNode = class(TObject)
  private
    [weak] FDocument: TALJSONDocument;
    [weak] FParentNode: TALJSONNode;
    fNodeName: AnsiString;
  protected
    function CreateChildList: TALJSONNodeList;
    function InternalGetChildNodes: TALJSONNodeList; virtual;
    function GetChildNodes: TALJSONNodeList; virtual;
    procedure SetChildNodes(const Value: TALJSONNodeList); virtual;
    function GetHasChildNodes: Boolean;
    function GetNodeType: TALJSONNodeType; virtual; abstract;
    function GetNodeSubType: TALJSONNodeSubType; virtual; abstract;
    function GetNodeValueStr: ansiString; virtual;
    function GetNodeValueInt64: int64; virtual;
    function GetNodeValueInterchange(const SkipNodeSubTypeHelper: boolean = False): AnsiString;
    procedure SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const StrValue: AnsiString; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    function GetOwnerDocument: TALJSONDocument;
    procedure SetOwnerDocument(const Value: TALJSONDocument);
    function GetParentNode: TALJSONNode;
    procedure SetParentNode(const Value: TALJSONNode);
    function GetJSON: AnsiString;
    procedure SetJSON(const Value: AnsiString);
    function GetBSON: AnsiString;
    procedure SetBSON(const Value: AnsiString);
    function NestingLevel: Integer;
    procedure SaveToBson(const Stream: TStream;
                         Var buffer: ansiString);
    procedure SaveToJson(const Stream: TStream;
                         Var buffer: ansiString);
  public
    constructor Create(const NodeName: AnsiString); virtual;
    procedure MultiThreadPrepare;
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
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function AddChild(const Path: array of AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function DeleteChild(const NodeName: AnsiString): boolean; overload;
    function DeleteChild(const Path: array of AnsiString): boolean; overload;
    function NextSibling: TALJSONNode;
    function PreviousSibling: TALJSONNode;
    procedure SaveToJSONStream(const Stream: TStream);
    procedure SaveToJSONFile(const FileName: AnsiString);
    procedure SaveToJSONString(var Str: AnsiString);
    procedure SaveToBSONStream(const Stream: TStream);
    procedure SaveToBSONFile(const FileName: AnsiString);
    procedure SaveToBSONString(var Str: AnsiString);
    procedure LoadFromJSONString(const Str: AnsiString; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONStream(const Stream: TStream; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONFile(const FileName: AnsiString; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONString(const Str: AnsiString; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONStream(const Stream: TStream; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONFile(const FileName: AnsiString; Const ClearChildNodes: Boolean = True);
    property ChildNodes: TALJSONNodeList read GetChildNodes write SetChildNodes;
    function GetChildNode(const nodeName: ansiString): TALJSONNode; overload;
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
    function GetChildNode(const path: array of ansiString): TALJSONNode; overload;
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
    property NodeName: AnsiString read fNodeName write fNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property NodeValue: AnsiString read GetNodeValueStr; // same as text property but without formating
    property NodeSubType: TALJSONNodeSubType read GetNodeSubType;
    property OwnerDocument: TALJSONDocument read GetOwnerDocument;
    property ParentNode: TALJSONNode read GetParentNode;
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
  TALJSONObjectNode = Class(TALJSONNode)
  private
    FChildNodes: TALJSONNodeList;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeList; override;
    function GetChildNodes: TALJSONNodeList; override;
    procedure SetChildNodes(const Value: TALJSONNodeList); override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    Destructor Destroy; override;
  end;

  {implements JSON array [] | [ elements ]}
  TALJSONArrayNode = Class(TALJSONNode)
  private
    FChildNodes: TALJSONNodeList;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeList; override;
    function GetChildNodes: TALJSONNodeList; override;
    procedure SetChildNodes(const Value: TALJSONNodeList); override;
  public
    constructor Create(const NodeName: AnsiString = ''); override;
    Destructor Destroy; override;
  end;

  {Groups javascript, string, number, true, false, null}
  TALJSONTextNode = Class(TALJSONNode)
  private
    fNodeSubType: TALJSONNodeSubType;
    fRawNodeValueStr: AnsiString; // contain the text representation of the node
                                  // WITHOUT any node subtype helper
                                  // for exemple for NumberLong(12391293) it's
                                  // store only 12391293
    FRawNodeValueInt64: int64;    // contain the value Stored in an int64 (if the
                                  // value can be store in an int64)
    fRawNodeValueDefined: TALJSONTextNodeValueDefined;
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

  {TALJSONDocument}
  TALJSONDocument = class(TObject)
  private
    FTag: NativeInt;
    FDocumentNode: TALJSONNode;
    FNodeIndentStr: AnsiString;
    FOptions: TALJSONDocOptions;
    FParseOptions: TALJSONParseOptions;
    fPathSeparator: AnsiChar;
    FOnParseStartDocument: TAlJSONParseDocument;
    FOnParseEndDocument: TAlJSONParseDocument;
    FonParseText: TAlJSONParseTextEvent;
    FonParseStartObject: TAlJSONParseObjectEvent;
    FonParseEndObject: TAlJSONParseObjectEvent;
    FonParseStartArray: TAlJSONParseArrayEvent;
    FonParseEndArray: TAlJSONParseArrayEvent;
    fFormatSettings: PALFormatSettings;
  protected
    procedure CheckActive;
    procedure DoParseStartDocument;
    procedure DoParseEndDocument;
    procedure DoParseText(const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType);
    procedure DoParseStartObject(const Path: AnsiString; const Name: AnsiString);
    procedure DoParseEndObject(const Path: AnsiString; const Name: AnsiString);
    procedure DoParseStartArray(const Path: AnsiString; const Name: AnsiString);
    procedure DoParseEndArray(const Path: AnsiString; const Name: AnsiString);
    Procedure ParseJSON(const RawJSONStream: TStream;
                        const RawJSONString: AnsiString;
                        const ContainerNode: TALJSONNode);
    Procedure ParseBSON(const RawBSONStream: TStream;
                        const RawBSONString: AnsiString;
                        const ContainerNode: TALJSONNode);
    procedure ReleaseDoc;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetChildNodes: TALJSONNodeList;
    function GetDocumentNode: TALJSONNode;
    function GetNodeIndentStr: AnsiString;
    function GetOptions: TALJSONDocOptions;
    function GetParseOptions: TALJSONParseOptions;
    function GetPathSeparator: ansiChar;
    procedure SetPathSeparator(const Value: ansiChar);
    function  GetJSON: AnsiString;
    function  GetBSON: AnsiString;
    procedure SetOptions(const Value: TALJSONDocOptions);
    procedure SetParseOptions(const Value: TALJSONParseOptions);
    procedure SetJSON(const Value: ansiString);
    procedure SetBSON(const Value: ansiString);
    procedure SetNodeIndentStr(const Value: AnsiString);
  public
    constructor Create(const aActive: Boolean = True); overload; virtual;
    constructor Create(const aFormatSettings: TALformatSettings; const aActive: Boolean = True); overload; virtual;
    destructor Destroy; override;
    procedure MultiThreadPrepare;
    procedure Clear;
    function AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function AddChild(const Path: array of AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode; overload;
    function CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNode;
    function IsEmptyDoc: Boolean;
    procedure LoadFromJSONString(const Str: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONStream(const Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONFile(const FileName: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONString(const Str: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONStream(const Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONFile(const FileName: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure SaveToJSONStream(const Stream: TStream);
    procedure SaveToJSONFile(const FileName: AnsiString);
    procedure SaveToJSONString(var Str: AnsiString);
    procedure SaveToBSONStream(const Stream: TStream);
    procedure SaveToBSONFile(const FileName: AnsiString);
    procedure SaveToBSONString(var Str: AnsiString);
    property ChildNodes: TALJSONNodeList read GetChildNodes;
    function GetChildNode(const nodeName: ansiString): TALJSONNode; overload;
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
    function GetChildNode(const path: array of ansiString): TALJSONNode; overload;
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
    property Node: TALJSONNode read GetDocumentNode;
    property Active: Boolean read GetActive write SetActive;
    property NodeIndentStr: AnsiString read GetNodeIndentStr write SetNodeIndentStr;
    property Options: TALJSONDocOptions read GetOptions write SetOptions;
    property ParseOptions: TALJSONParseOptions read GetParseOptions write SetParseOptions;
    property PathSeparator: ansiChar read GetPathSeparator write SetPathSeparator;
    property JSON: AnsiString read GetJSON write SetJSON;
    property BSON: AnsiString read GetBSON write SetBSON;
    property OnParseStartDocument: TAlJSONParseDocument read fOnParseStartDocument write fOnParseStartDocument;
    property OnParseEndDocument: TAlJSONParseDocument read fOnParseEndDocument write fOnParseEndDocument;
    property onParseText: TAlJSONParseTextEvent read fonParseText write fonParseText;
    property onParseStartObject: TAlJSONParseObjectEvent read fonParseStartObject write fonParseStartObject;
    property onParseEndObject: TAlJSONParseObjectEvent read fonParseEndObject write fonParseEndObject;
    property onParseStartArray: TAlJSONParseArrayEvent read fonParseStartArray write fonParseStartArray;
    property onParseEndArray: TAlJSONParseArrayEvent read fonParseEndArray write fonParseEndArray;
    property FormatSettings: PALFormatSettings read fFormatSettings; // this is use only on GetText/OnParseText to retrieve float and DateTime formatted according to FormatSettings
    property Tag: NativeInt read FTag write FTag;
  end;

{misc constants}
var
  vALDefaultNodeIndent: ansiString;  // var instead of const to avoid new ansitring on assign
  vALJsonISODateFormatSettings: TALFormatSettings;

{misc function}
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           const aFormatSettings: TALFormatSettings;
                           const aPath: AnsiString;
                           const aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           const aFormatSettings: TALFormatSettings;
                           const aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStrings(const aJsonNode: TAlJsonNode;
                           Const aPath: AnsiString;
                           const aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false'); overload;
Procedure ALJSONToTStrings(const aJsonNode: TAlJsonNode;
                           const aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false'); overload;
procedure ALTStringsToJson(const aLst: TALStrings;
                           const aJsonNode: TALJSONNode;
                           Const aPath: AnsiString = '';
                           Const aNameToLowerCase: boolean = false;
                           Const aNullStr: AnsiString = 'null');

Procedure ALJSONToXML(const aJSONNode: TALJsonNode;
                      const aXMLNode: TALXmlNode;
                      const aXMLElementNameForJSONArrayEntries: TalStrings; // JSONArrayNodeName=XMLElementName | ex: transactions=transaction
                                                                      //                                  |     features=feature
                      const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec'); overload;
Procedure ALJSONToXML(const aJSONNode: TALJsonNode;
                      const aXMLNode: TALXmlNode;
                      const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec'); overload;

function ALJsonEncodeFloatWithNodeSubTypeHelper(const aValue: double): AnsiString;
function ALJsonEncodeTextWithNodeSubTypeHelper(const aValue: AnsiString): AnsiString;
function ALJsonEncodeBinaryWithNodeSubTypeHelper(const aValue: AnsiString): AnsiString;
function ALJsonEncodeObjectIDWithNodeSubTypeHelper(const aValue: AnsiString): AnsiString;
function ALJsonEncodeBooleanWithNodeSubTypeHelper(const aValue: Boolean): AnsiString;
function ALJsonEncodeDateTimeWithNodeSubTypeHelper(const aValue: TdateTime): AnsiString;
function ALJsonEncodeJavascriptWithNodeSubTypeHelper(const aValue: AnsiString): AnsiString;
function ALJsonEncodeInt64WithNodeSubTypeHelper(const aValue: int64): AnsiString;
function ALJsonEncodeInt32WithNodeSubTypeHelper(const aValue: int32): AnsiString;
function ALJsonEncodeNullWithNodeSubTypeHelper: AnsiString;
function ALJsonEncodeWithNodeSubTypeHelper(const aValue: AnsiString;
                                           const aNodeSubType: TALJSONNodeSubType;
                                           const aFormatSettings: TALFormatSettings): AnsiString;

function ALJSONTryStrToRegEx(const S: AnsiString; out RegEx: AnsiString; out RegExOptions: TALPerlRegExOptions): boolean;
function ALJSONTryStrTobinary(const S: AnsiString; out Data: AnsiString; out Subtype: byte): boolean; // return a "byte" string
function ALJSONTryStrToDateTime(const S: AnsiString; out Value: TDateTime): Boolean;
function ALJSONTryStrToObjectID(const S: AnsiString; out Value: ansiString): Boolean; // return a "byte" string
function ALJSONTryStrToTimestamp(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
function ALJSONTryStrToInt32(const S: AnsiString; out Value: integer): Boolean;
function ALJSONTryStrToInt64(const S: AnsiString; out Value: int64): Boolean;

Function ALFindJsonNodeByInt32ChildNodeValue(const JsonNode:TalJsonNode;
                                             Const ChildNodeName: AnsiString;
                                             Const ChildNodeValue : Int32;
                                             Const Recurse: Boolean = False): TALJsonNode;
Function ALFindJsonNodeByTextChildNodeValue(const JsonNode:TalJsonNode;
                                            Const ChildNodeName: AnsiString;
                                            Const ChildNodeValue : AnsiString;
                                            Const Recurse: Boolean = False): TALJsonNode;

{$ENDIF !ALHideAnsiString}

type

  {class definition}
  TALJSONNodeU = Class;
  TALJSONNodeListU = Class;
  TALJSONDocumentU = Class;

  TAlJSONParseDocumentU = reference to procedure (Sender: TObject);
  TAlJSONParseTextEventU = reference to procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType);
  TAlJSONParseObjectEventU = reference to procedure (Sender: TObject; const Path: String; const Name: String);
  TAlJSONParseArrayEventU = reference to procedure (Sender: TObject; const Path: String; const Name: String);

  TALJSONNodeListSortCompareU = reference to function(List: TALJSONNodeListU; Index1, Index2: Integer): Integer;

  TALJSONPointerListU = array of TALJSONNodeU;

  {TALJSONNodeListU}
  {TALJSONNodeListU is used to represent a set of related nodes (TALJSONNodeU object) in an JSON document. For example, TALJSONNodeListU is used to
   represent all of the children of a node, or all of the attributes of a node. TALJSONNodeListU can be used to add or delete nodes from the
   List, or to access specific nodes.}
  TALJSONNodeListU = class(Tobject)
  Private
    FCapacity: Integer;
    FCount: integer;
    FList: TALJSONPointerListU;
    [weak] FOwner: TALJSONNodeU;
    procedure QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareU);
  protected
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    property Owner: TALJSONNodeU read FOwner;
    function Get(Index: Integer): TALJSONNodeU;
    function GetNodeByIndex(Const Index: Integer): TALJSONNodeU;
    function GetNodeByName(Const Name: String): TALJSONNodeU;
  public
    constructor Create(Owner: TALJSONNodeU);
    destructor Destroy; override;
    procedure CustomSort(Compare: TALJSONNodeListSortCompareU);
    function Add(const Node: TALJSONNodeU): Integer;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: String): Integer; overload;
    function Extract(const index: integer): TALJSONNodeU; overload;
    function Extract(const Node: TALJSONNodeU): TALJSONNodeU; overload;
    procedure Exchange(Index1, Index2: Integer);
    function FindNode(const NodeName: String; const Direction: TDirection = TDirection.FromBeginning): TALJSONNodeU; overload;
    function FindSibling(const Node: TALJSONNodeU; Delta: Integer): TALJSONNodeU;
    function First: TALJSONNodeU;
    function IndexOf(const Name: String; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOf(const Node: TALJSONNodeU; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: String; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: integer; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: int64; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer; overload;
    function Last: TALJSONNodeU;
    function Remove(const Node: TALJSONNodeU): Integer;
    function ReplaceNode(const OldNode, NewNode: TALJSONNodeU): TALJSONNodeU;
    procedure Clear;
    procedure Insert(Index: Integer; const Node: TALJSONNodeU);
    property Count: Integer read fCount;
    property Nodes[const Name: String]: TALJSONNodeU read GetNodeByName; default;
    property Nodes[const Index: integer]: TALJSONNodeU read GetNodeByIndex; default;
  end;

  {TALJSONNode}
  {TALJSONNodeU represents a node in an JSON document.}
  TALJSONNodeU = class(TObject)
  private
    [weak] FDocument: TALJSONDocumentU;
    [weak] FParentNode: TALJSONNodeU;
    fNodeName: String;
  protected
    function CreateChildList: TALJSONNodeListU;
    function InternalGetChildNodes: TALJSONNodeListU; virtual;
    function GetChildNodes: TALJSONNodeListU; virtual;
    procedure SetChildNodes(const Value: TALJSONNodeListU); virtual;
    function GetHasChildNodes: Boolean;
    function GetNodeType: TALJSONNodeType; virtual; abstract;
    function GetNodeSubType: TALJSONNodeSubType; virtual; abstract;
    function GetNodeValueStr: String; virtual;
    function GetNodeValueInt64: int64; virtual;
    function GetNodeValueInterchange(const SkipNodeSubTypeHelper: boolean = False): String;
    procedure SetNodeValue(const Value: String; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    procedure SetNodeValue(const StrValue: String; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType); overload; virtual;
    function GetOwnerDocument: TALJSONDocumentU;
    procedure SetOwnerDocument(const Value: TALJSONDocumentU);
    function GetParentNode: TALJSONNodeU;
    procedure SetParentNode(const Value: TALJSONNodeU);
    function GetJSON: String;
    procedure SetJSON(const Value: String);
    function GetBSON: Tbytes;
    procedure SetBSON(const Value: Tbytes);
    function NestingLevel: Integer;
    procedure SaveToBson(const Stream: TStream;
                         Var buffer: Tbytes);
    procedure SaveToJson(const Stream: TStream;
                         const StreamEncoding: TEncoding;
                         Var buffer: String);
  public
    constructor Create(const NodeName: String); virtual;
    procedure MultiThreadPrepare;
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
    function AddChild(const NodeName: String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU; overload;
    function AddChild(const Path: array of String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU; overload;
    function AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU; overload;
    function DeleteChild(const NodeName: String): boolean; overload;
    function DeleteChild(const Path: array of String): boolean; overload;
    function NextSibling: TALJSONNodeU;
    function PreviousSibling: TALJSONNodeU;
    procedure SaveToJSONStream(const Stream: TStream; const Encoding: TEncoding); overload;
    procedure SaveToJSONStream(const Stream: TStream); overload;
    procedure SaveToJSONFile(const FileName: String; const Encoding: TEncoding); overload;
    procedure SaveToJSONFile(const FileName: String); overload;
    procedure SaveToJSONString(var Str: String);
    procedure SaveToBSONStream(const Stream: TStream);
    procedure SaveToBSONFile(const FileName: String);
    procedure SaveToBSONBytes(var Bytes: Tbytes);
    procedure LoadFromJSONString(const Str: String; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONStream(const Stream: TStream; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONFile(const FileName: String; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONBytes(const Bytes: Tbytes; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONStream(const Stream: TStream; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONFile(const FileName: String; Const ClearChildNodes: Boolean = True);
    property ChildNodes: TALJSONNodeListU read GetChildNodes write SetChildNodes;
    function GetChildNode(const nodeName: String): TALJSONNodeU; overload;
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
    function GetChildNode(const path: array of String): TALJSONNodeU; overload;
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
    property NodeName: String read fNodeName write fNodeName;
    property NodeType: TALJSONNodeType read GetNodeType;
    property NodeValue: String read GetNodeValueStr; // same as text property but without formating
    property NodeSubType: TALJSONNodeSubType read GetNodeSubType;
    property OwnerDocument: TALJSONDocumentU read GetOwnerDocument;
    property ParentNode: TALJSONNodeU read GetParentNode;
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
  TALJSONObjectNodeU = Class(TALJSONNodeU)
  private
    FChildNodes: TALJSONNodeListU;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListU; override;
    function GetChildNodes: TALJSONNodeListU; override;
    procedure SetChildNodes(const Value: TALJSONNodeListU); override;
  public
    constructor Create(const NodeName: String = ''); override;
    Destructor Destroy; override;
  end;

  {implements JSON array [] | [ elements ]}
  TALJSONArrayNodeU = Class(TALJSONNodeU)
  private
    FChildNodes: TALJSONNodeListU;
  protected
    function GetNodeType: TALJSONNodeType; override;
    function GetNodeSubType: TALJSONNodeSubType; override;
    function InternalGetChildNodes: TALJSONNodeListU; override;
    function GetChildNodes: TALJSONNodeListU; override;
    procedure SetChildNodes(const Value: TALJSONNodeListU); override;
  public
    constructor Create(const NodeName: String = ''); override;
    Destructor Destroy; override;
  end;

  {Groups javascript, string, number, true, false, null}
  TALJSONTextNodeU = Class(TALJSONNodeU)
  private
    fNodeSubType: TALJSONNodeSubType;
    fRawNodeValueStr: String;  // contain the text representation of the node
                               // WITHOUT any node subtype helper
                               // for exemple for NumberLong(12391293) it's
                               // store only 12391293
    FRawNodeValueInt64: int64; // contain the value Stored in an int64 (if the
                               // value can be store in an int64)
    fRawNodeValueDefined: TALJSONTextNodeValueDefined;
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

  {TALJSONDocument}
  TALJSONDocumentU = class(TObject)
  private
    FTag: NativeInt;
    FDocumentNode: TALJSONNodeU;
    FNodeIndentStr: String;
    FOptions: TALJSONDocOptions;
    FParseOptions: TALJSONParseOptions;
    fPathSeparator: Char;
    FOnParseStartDocument: TAlJSONParseDocumentU;
    FOnParseEndDocument: TAlJSONParseDocumentU;
    FonParseText: TAlJSONParseTextEventU;
    FonParseStartObject: TAlJSONParseObjectEventU;
    FonParseEndObject: TAlJSONParseObjectEventU;
    FonParseStartArray: TAlJSONParseArrayEventU;
    FonParseEndArray: TAlJSONParseArrayEventU;
    fFormatSettings: PALFormatSettingsU;
  protected
    procedure CheckActive;
    procedure DoParseStartDocument;
    procedure DoParseEndDocument;
    procedure DoParseText(const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType);
    procedure DoParseStartObject(const Path: String; const Name: String);
    procedure DoParseEndObject(const Path: String; const Name: String);
    procedure DoParseStartArray(const Path: String; const Name: String);
    procedure DoParseEndArray(const Path: String; const Name: String);
    Procedure ParseJSON(const Buffer: String;
                        const ContainerNode: TALJSONNodeU);
    Procedure ParseBSON(const Buffer: Tbytes;
                        const ContainerNode: TALJSONNodeU);
    procedure ReleaseDoc;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetChildNodes: TALJSONNodeListU;
    function GetDocumentNode: TALJSONNodeU;
    function GetNodeIndentStr: String;
    function GetOptions: TALJSONDocOptions;
    function GetParseOptions: TALJSONParseOptions;
    function GetPathSeparator: Char;
    procedure SetPathSeparator(const Value: Char);
    function  GetJSON: String;
    function  GetBSON: Tbytes;
    procedure SetOptions(const Value: TALJSONDocOptions);
    procedure SetParseOptions(const Value: TALJSONParseOptions);
    procedure SetJSON(const Value: String);
    procedure SetBSON(const Value: Tbytes);
    procedure SetNodeIndentStr(const Value: String);
  public
    constructor Create(const aActive: Boolean = True); overload; virtual;
    constructor Create(const aFormatSettings: TALformatSettingsU; const aActive: Boolean = True); overload; virtual;
    destructor Destroy; override;
    procedure MultiThreadPrepare;
    procedure Clear;
    function AddChild(const NodeName: String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU; overload;
    function AddChild(const Path: array of String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU; overload;
    function CreateNode(const NodeName: String; NodeType: TALJSONNodeType): TALJSONNodeU;
    function IsEmptyDoc: Boolean;
    procedure LoadFromJSONString(const Str: String; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONStream(const Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromJSONFile(const FileName: String; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONBytes(const Bytes: Tbytes; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONStream(const Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure LoadFromBSONFile(const FileName: String; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
    procedure SaveToJSONStream(const Stream: TStream; const Encoding: TEncoding); overload;
    procedure SaveToJSONStream(const Stream: TStream); overload;
    procedure SaveToJSONFile(const FileName: String; const Encoding: TEncoding); overload;
    procedure SaveToJSONFile(const FileName: String); overload;
    procedure SaveToJSONString(var Str: String);
    procedure SaveToBSONStream(const Stream: TStream);
    procedure SaveToBSONFile(const FileName: String);
    procedure SaveToBSONBytes(var Bytes: Tbytes);
    property ChildNodes: TALJSONNodeListU read GetChildNodes;
    function GetChildNode(const nodeName: String): TALJSONNodeU; overload;
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
    function GetChildNode(const path: array of String): TALJSONNodeU; overload;
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
    property Node: TALJSONNodeU read GetDocumentNode;
    property Active: Boolean read GetActive write SetActive;
    property NodeIndentStr: String read GetNodeIndentStr write SetNodeIndentStr;
    property Options: TALJSONDocOptions read GetOptions write SetOptions;
    property ParseOptions: TALJSONParseOptions read GetParseOptions write SetParseOptions;
    property PathSeparator: Char read GetPathSeparator write SetPathSeparator;
    property JSON: String read GetJSON write SetJSON;
    property BSON: Tbytes read GetBSON write SetBSON;
    property OnParseStartDocument: TAlJSONParseDocumentU read fOnParseStartDocument write fOnParseStartDocument;
    property OnParseEndDocument: TAlJSONParseDocumentU read fOnParseEndDocument write fOnParseEndDocument;
    property onParseText: TAlJSONParseTextEventU read fonParseText write fonParseText;
    property onParseStartObject: TAlJSONParseObjectEventU read fonParseStartObject write fonParseStartObject;
    property onParseEndObject: TAlJSONParseObjectEventU read fonParseEndObject write fonParseEndObject;
    property onParseStartArray: TAlJSONParseArrayEventU read fonParseStartArray write fonParseStartArray;
    property onParseEndArray: TAlJSONParseArrayEventU read fonParseEndArray write fonParseEndArray;
    property FormatSettings: PALFormatSettingsU read fFormatSettings; // this is use only on GetText/OnParseText to retrieve float and DateTime formatted according to FormatSettings
    property Tag: NativeInt read FTag write FTag;
  end;

{misc constants}
var
  vALDefaultNodeIndentU: String;  // var instead of const to avoid new ansitring on assign
  vALJsonISODateFormatSettingsU: TALFormatSettingsU;

{misc function}
Procedure ALJSONToTStringsU(const AJsonStr: String;
                            const aFormatSettings: TALformatSettingsU;
                            const aPath: String;
                            const aLst: TALStringsU;
                            Const aNullStr: String = 'null';
                            Const aTrueStr: String = 'true';
                            Const aFalseStr: String = 'false'); overload;
Procedure ALJSONToTStringsU(const AJsonStr: String;
                            const aFormatSettings: TALformatSettingsU;
                            const aLst: TALStringsU;
                            Const aNullStr: String = 'null';
                            Const aTrueStr: String = 'true';
                            Const aFalseStr: String = 'false'); overload;
Procedure ALJSONToTStringsU(const aJsonNode: TALJSONNodeU;
                            Const aPath: String;
                            const aLst: TALStringsU;
                            Const aNullStr: String = 'null';
                            Const aTrueStr: String = 'true';
                            Const aFalseStr: String = 'false'); overload;
Procedure ALJSONToTStringsU(const aJsonNode: TALJSONNodeU;
                            const aLst: TALStringsU;
                            Const aNullStr: String = 'null';
                            Const aTrueStr: String = 'true';
                            Const aFalseStr: String = 'false'); overload;
procedure ALTStringsToJsonU(const aLst: TALStringsU;
                            const aJsonNode: TALJSONNodeU;
                            Const aPath: String = '';
                            Const aNameToLowerCase: boolean = false;
                            Const aNullStr: String = 'null');

function ALJsonEncodeFloatWithNodeSubTypeHelperU(const aValue: double): String;
function ALJsonEncodeTextWithNodeSubTypeHelperU(const aValue: String): String;
function ALJsonEncodeBinaryWithNodeSubTypeHelperU(const aValue: String): String;
function ALJsonEncodeObjectIDWithNodeSubTypeHelperU(const aValue: String): String;
function ALJsonEncodeBooleanWithNodeSubTypeHelperU(const aValue: Boolean): String;
function ALJsonEncodeDateTimeWithNodeSubTypeHelperU(const aValue: TdateTime): String;
function ALJsonEncodeJavascriptWithNodeSubTypeHelperU(const aValue: String): String;
function ALJsonEncodeInt64WithNodeSubTypeHelperU(const aValue: int64): String;
function ALJsonEncodeInt32WithNodeSubTypeHelperU(const aValue: int32): String;
function ALJsonEncodeNullWithNodeSubTypeHelperU: String;
function ALJsonEncodeWithNodeSubTypeHelperU(const aValue: String;
                                            const aNodeSubType: TALJSONNodeSubType;
                                            const aFormatSettings: TALformatSettingsU): String;

function ALJSONTryStrToRegExU(const S: String; out RegEx: String; out RegExOptions: TALPerlRegExOptions): boolean;
function ALJSONTryStrTobinaryU(const S: String; out Data: String; out Subtype: byte): boolean; // return a base64 encoded string
function ALJSONTryStrToDateTimeU(const S: String; out Value: TDateTime): Boolean;
function ALJSONTryStrToObjectIDU(const S: String; out Value: String): Boolean; // return a hex string
function ALJSONTryStrToTimestampU(const S: String; out Value: TALBSONTimestamp): Boolean;
function ALJSONTryStrToInt32U(const S: String; out Value: integer): Boolean;
function ALJSONTryStrToInt64U(const S: String; out Value: int64): Boolean;

Function ALFindJsonNodeByInt32ChildNodeValueU(const JsonNode:TalJsonNodeU;
                                              Const ChildNodeName: String;
                                              Const ChildNodeValue : Int32;
                                              Const Recurse: Boolean = False): TalJsonNodeU;
Function ALFindJsonNodeByTextChildNodeValueU(const JsonNode:TalJsonNodeU;
                                             Const ChildNodeName: String;
                                             Const ChildNodeValue : String;
                                             Const Recurse: Boolean = False): TALJsonNodeU;

implementation

uses
  System.Math,
  System.Generics.Collections,
  system.IOUtils,
  System.DateUtils,
  ALquickSortList,
  AlHTML,
  ALCommon;

{$IFNDEF ALHideAnsiString}

{*********************************************************************}
Function ALFindJsonNodeByInt32ChildNodeValue(const JsonNode:TalJsonNode;
                                             Const ChildNodeName: AnsiString;
                                             Const ChildNodeValue : Int32;
                                             Const Recurse: Boolean = False): TALJsonNode;
var I, J : integer;
Begin
  result := nil;
  if not (JsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for I := 0 to JsonNode.ChildNodes.Count - 1 do begin
    for J := 0 to JsonNode.ChildNodes[I].ChildNodes.Count - 1 do begin
      If (JsonNode.ChildNodes[I].ChildNodes[j].NodeType = nttext) and
         (JsonNode.ChildNodes[I].ChildNodes[j].NodesubType = nstint32) and
         (ALSametext(JsonNode.ChildNodes[I].ChildNodes[j].NodeName, ChildNodeName)) and
         (JsonNode.ChildNodes[I].ChildNodes[j].int32 = ChildNodeValue) then begin
        result := JsonNode.ChildNodes[I];
        exit;
      end;
    end;
    if Recurse then begin
      result := ALFindJsonNodeByInt32ChildNodeValue(JsonNode.ChildNodes[I],
                                                    ChildNodeName,
                                                    ChildNodeValue,
                                                    Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{*********************************************************************}
Function ALFindJsonNodeByTextChildNodeValue(const JsonNode:TalJsonNode;
                                            Const ChildNodeName: AnsiString;
                                            Const ChildNodeValue : AnsiString;
                                            Const Recurse: Boolean = False): TALJsonNode;
var I, J : integer;
Begin
  result := nil;
  if not (JsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for I := 0 to JsonNode.ChildNodes.Count - 1 do begin
    for J := 0 to JsonNode.ChildNodes[I].ChildNodes.Count - 1 do begin
      If (JsonNode.ChildNodes[I].ChildNodes[j].NodeType = nttext) and
         (JsonNode.ChildNodes[I].ChildNodes[j].NodesubType = nstText) and
         (ALSametext(JsonNode.ChildNodes[I].ChildNodes[j].NodeName, ChildNodeName)) and
         (JsonNode.ChildNodes[I].ChildNodes[j].text = ChildNodeValue) then begin
        result := JsonNode.ChildNodes[I];
        exit;
      end;
    end;
    if Recurse then begin
      result := ALFindJsonNodeByTextChildNodeValue(JsonNode.ChildNodes[I],
                                                   ChildNodeName,
                                                   ChildNodeValue,
                                                   Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{***********************************************************************************************************************}
function ALJSONTryStrToRegEx(const S: AnsiString; out RegEx: AnsiString; out RegExOptions: TALPerlRegExOptions): boolean;
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

    P1 := ALLastDelimiter('/', S);
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
      //  aRegEx.Free;
      //end;

    end;

  end;

end;

{***************************************************************************************************}
function ALJSONTryStrTobinary(const S: AnsiString; out Data: AnsiString; out Subtype: byte): boolean;
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

{**********************************************************************************}
function ALJSONTryStrToDateTime(const S: AnsiString; out Value: TDateTime): Boolean;
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
  if alpos('new', s) = 1 then P1 := 4{length('new') + 1} // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
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

  result := ALTryStrToDateTime(LTmpStr, Value, vALJsonISODateFormatSettings);
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
function ALJSONTryStrToObjectID(const S: AnsiString; out Value: AnsiString): Boolean;
var LObjectIDhex: AnsiString;
    LQuoteChar: ansiChar;
    P1: integer;
    Ln: integer;
begin

  // s must look like
  // ObjectId ( "507f1f77bcf86cd799439011" )
  result := false;
  if alpos('ObjectId', S) <> 1 then exit;
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

{******************************************************************************************}
function ALJSONTryStrToTimestamp(const S: AnsiString; out Value: TALBSONTimestamp): Boolean;
var P1, P2: integer;
    LArgs: AnsiString;
    LArg1: integer;
    LArg2: integer;
    Ln: integer;
begin

  // s must look like
  // Timestamp(0, 0)
  result        := false;
  if ALPos('Timestamp', S) <> 1 then Exit;
  Ln := length(s);
  P1 := 10{Length('Timestamp') + 1}; // Timestamp(0, 0)
                                     //          ^
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // Timestamp(0, 0)
                                            //          ^P1
  P2 := ALPosEx(')', S, P1);
  if P2 <> ln then exit; // Timestamp(0, 0)
                         //               ^P2
  LArgs := ALCopyStr(S, P1+1, P2 - P1-1); // 0, 0

  // take arguments of function Timestamp
  P1 := ALPos(',', LArgs);
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, 1,      P1 - 1)), LArg1) then Exit;
  if not ALTryStrToInt(ALTrim(ALCopyStr(LArgs, P1 + 1, maxint)), LArg2) then Exit;

  // build result
  result := true;
  Value.W1 := LArg1; // higher 4 bytes - increment
  Value.W2 := LArg2; // lower  4 bytes - timestamp
end;

{*****************************************************************************}
function ALJSONTryStrToInt32(const S: AnsiString; out Value: integer): Boolean;
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
  if alpos('NumberInt', S) <> 1 then exit;
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

{***************************************************************************}
function ALJSONTryStrToInt64(const S: AnsiString; out Value: int64): Boolean;
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
  if alpos('NumberLong', S) <> 1 then exit;
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

{****************************************************}
procedure ALJSONDocError(const Msg: String); overload;
begin
  raise EALJSONDocError.Create(Msg);
end;

{********************************************************************************}
procedure ALJSONDocError(const Msg: String; const Args: array of const); overload;
begin
  raise EALJSONDocError.CreateFmt(Msg, Args);
end;

{*************************************************************************************}
procedure ALJSONDocError(const Msg: String; const NodeType: TalJsonNodeType); overload;
begin
  case NodeType of
    ntObject: ALJSONDocError(Msg, ['ntObject']);
    ntArray: ALJSONDocError(Msg, ['ntArray']);
    ntText: ALJSONDocError(Msg, ['ntText']);
    else AlJSONDocError(cAlJSONInvalidNodeType);
  end;
end;

{***********************************************************************************}
function ALNodeMatches(const Node: TALJSONNode; const NodeName: AnsiString): Boolean;
begin
  Result := (Node.NodeName = NodeName);
end;

{********************************************************************************************}
{Call CreateNode to create a new generic JSON node. The resulting node does not have a parent,
 but can be added to the ChildNodes list of any node in the document.}
function ALCreateJSONNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNode;
begin
  case NodeType of
    ntObject: Result := TALJSONObjectNode.Create(NodeName);
    ntArray: Result := TALJSONArrayNode.Create(NodeName);
    ntText: Result := TALJSONTextNode.Create(NodeName);
    else begin
      Result := nil; //for hide warning
      AlJSONDocError(cAlJSONInvalidNodeType);
    end;
  end;
end;

{****************************************************************}
constructor TALJSONDocument.create(const aActive: Boolean = True);
begin
  inherited create;
  FDocumentNode:= nil;
  FParseOptions:= [];
  fPathSeparator := '.';
  FOnParseStartDocument := nil;
  FOnParseEndDocument := nil;
  FonParseText := nil;
  FonParseStartObject := nil;
  FonParseEndObject := nil;
  FonParseStartArray := nil;
  FonParseEndArray := nil;
  FOptions := [];
  NodeIndentStr := vALDefaultNodeIndent;
  fFormatSettings := @ALDefaultFormatSettings;
  FTag := 0;
  SetActive(aActive);
end;

{**********************************************************************************************************}
constructor TALJSONDocument.Create(const aFormatSettings: TALformatSettings; const aActive: Boolean = True);
begin
  create(aActive);
  if @aFormatSettings <> @ALDefaultFormatSettings then begin
    new(fFormatSettings);
    fFormatSettings^ := aFormatSettings;
  end;
end;

{*********************************}
destructor TALJSONDocument.Destroy;
begin
  if fFormatSettings <> @ALDefaultFormatSettings then dispose(fFormatSettings);
  Options := Options - [doImmutable];
  ReleaseDoc;
  inherited;
end;

{*******************************************}
procedure TALJSONDocument.MultiThreadPrepare;
begin
  node.MultiThreadPrepare;
end;

{******************************}
procedure TALJSONDocument.Clear;
begin
  releaseDoc;
  Active := true;
end;

{****************************************}
{Returns the value of the Active property.
 GetActive is the read implementation of the Active property.}
function TALJSONDocument.GetActive: Boolean;
begin
  Result := Assigned(FDocumentNode);
end;

{*************************************}
{Sets the value of the Active property.
 SetActive is the write implementation of the Active property.
 *Value is the new value to set.}
procedure TALJSONDocument.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then begin
    if Value then begin
      if (doImmutable in Options) then ALJSONDocError(cALJSONImmutable);
      FDocumentNode := TALJSONObjectNode.Create;
      FDocumentNode.SetOwnerDocument(Self);
    end
    else ReleaseDoc;
  end;
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
Procedure TALJSONDocument.ParseJSON(const RawJSONStream: TStream;
                                    const RawJSONString: AnsiString;
                                    const ContainerNode: TALJSONNode);

Const BufferSize: integer = 8192;

Var Buffer: AnsiString;
    BufferLength: Integer;
    BufferPos: Integer;
    CurrName: AnsiString;
    CurrIndex: integer;
    CurrValue: ansiString;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNode;
    NamePaths: TALNvStringList;
    ObjectPaths: TALIntegerList;
    DecodeJSONReferences: boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
      if BufferPos <= length(Buffer) then ALMove(Pbyte(Buffer)[BufferPos - 1],
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
    LB := PathSeparator;
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
         (((NotSaxMode) and (TALJSONNode(NamePaths.Objects[I]).nodetype <> ntarray)) or
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(const index: integer;
                                  const Args: array of const;
                                  const NodeSubType: TALJSONNodeSubType);
  begin
    DoParseText(GetPathStr('[' + alinttostr(index) + ']'), '', Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithName(const name: AnsiString;
                                 const Args: array of const;
                                 const NodeSubType: TALJSONNodeSubType);
  begin
    DoParseText(GetPathStr(Name), Name, Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseText(const Index: integer;
                         const Name: AnsiString;
                         const Args: array of const;
                         const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(fonParseText) then begin
      if notSaxMode then begin
        if WorkingNode.nodetype=ntarray then _DoParseTextWithIndex(Index, Args, NodeSubType)
        else _DoParseTextWithName(Name, Args, NodeSubType);
      end
      else begin
        if NamePaths.Count = 0 then ALJSONDocError(CALJSONParseError);
        if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntArray then _DoParseTextWithIndex(Index, Args, NodeSubType)
        else _DoParseTextWithName(Name, Args, NodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartObject(const Name: AnsiString);
  begin
    DoParseStartObject(GetPathStr, Name);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndObject;
  begin
    if NamePaths.Count = 0 then ALJSONDocError(CALJSONParseError);
    DoParseEndObject(GetPathStr, NamePaths.Names[NamePaths.Count - 1])
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: AnsiString);
  begin
    DoParseStartArray(GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if NamePaths.Count = 0 then ALJSONDocError(CALJSONParseError);
    DoParseEndArray(GetPathStr, NamePaths.Names[NamePaths.Count - 1]);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexItemToNamePath(const index: integer; Obj: Pointer);
  var S1: ansiString;
  begin
    setlength(S1,sizeOf(Integer) {div sizeOF(ansiChar)}); // off course sizeOf(Integer) must be a multiple of sizeOf(ansiChar) but it's always the case
    ALmove(index, pointer(S1)^, sizeOf(Integer));
    NamePaths.AddNameValueObject('[' + alinttostr(Index) + ']', S1, Obj)
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
      if NamePaths.Count = 0 then ALJSONDocError(CALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt64Node(index: integer; const name: AnsiString; const value: ansiString): boolean;
  var LNode: TALJsonNode;
      LInt64: Int64;
  begin
    if ALJSONTryStrToInt64(value, LInt64) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetInt64(LInt64);
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
  var LNode: TALJsonNode;
      LInt32: Int32;
  begin
    if ALJSONTryStrToInt32(value, LInt32) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setint32(LInt32);
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
  var LNode: TALJsonNode;
  begin
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
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
  var LNode: TALJsonNode;
      LDouble: Double;
  begin
    if ALTryStrToFloat(value, LDouble, ALDefaultFormatSettings) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetFloat(LDouble);
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
  var LNode: TALJsonNode;
      LBinSubtype: byte;
      LBinData: ansiString;
  begin
    if ALJSONTryStrToBinary(value, LBinData, LBinSubtype) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.setbinary(LBinData, LBinSubtype);
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
  var LNode: TALJsonNode;
      LObjectID: AnsiString;
  begin
    if ALJSONTryStrToObjectID(value, LObjectID) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetObjectID(LObjectID);
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
  var LNode: TALJsonNode;
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
        LNode.Free;
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
  var LNode: TALJsonNode;
      LDateTime: TdateTime;
  begin
    if ALJSONTryStrToDateTime(value, LDateTime) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.Setdatetime(LDateTime);
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
  var LNode: TALJsonNode;
      LTimestamp: TALBSONTimestamp;
  begin
    if ALJSONTryStrToTimestamp(value, LTimestamp) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetTimestamp(LTimestamp);
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
  var LNode: TALJsonNode;
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
          LNode.Free;
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
  var LNode: TALJsonNode;
      LRegEx: ansiString;
      LRegExOptions: TALPerlRegExOptions;
  begin
    if ALJSONTryStrToRegEx(value, LRegEx, LRegExOptions) then begin
      result := true;
      if NotSaxMode then begin
        if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
        else LNode := CreateNode(Name, nttext);
        try
          LNode.SetRegEx(LRegEx, LRegExOptions);
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
  var LNode: TALJsonNode;
  begin
    result := true;
    if NotSaxMode then begin
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(value);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
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
    if NamePaths.Count = 0 then ALJSONDocError(CALJSONParseError);
    ALMove(pointer(namePaths.ValueFromIndex[namepaths.Count - 1])^,result,sizeOf(integer));
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var LNode: TALJsonNode;
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
        if (ObjectPaths.Count = 0) then ALJSONDocError(cALJSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then ALJSONDocError(cALJSONParseError);
      end;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        if assigned(ObjectPaths) then LNode := TALJSONNode(ObjectPaths.Objects[ObjectPaths.Count - 1])
        else LNode := TALJSONNode(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (LNode <> WorkingNode) then ALJSONDocError(CALJSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then ALJSONDocError(cALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then ALJSONDocError(CALJSONParseError);

        //if working node <> containernode then we can go to one level up
        If WorkingNode<>ContainerNode then begin

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

        //if working node = containernode then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

         //calculate anodeTypeInt
        LNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then ALJSONDocError(cALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then ALJSONDocError(CALJSONParseError);

        //update CurrIndex if WorkingNode.NodeType = ntArray
        if (Namepaths.Count >= 2) and
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 2]) = ntarray) then CurrIndex := _extractLastIndexFromNamePath + 1;

      end;

      //call the DoParseEndObject/array event
      if Assigned(fonParseEndObject) then begin
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

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocError(CALJSONParseError);

        //Node without name can be ONLY present inside an array node
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then ALJSONDocError(CALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
        if c = '{' then LNode := CreateNode('', ntObject)
        else LNode := CreateNode('', ntarray);
        try
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
             (TALJsonNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then ALJSONDocError(CALJSONParseError);

        //update the path
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrIndex, '', pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event
      if c = '{' then begin
        if Assigned(fonParseStartObject) then _DoParseStartObject('');
        CurrIndex := -1;
      end
      else begin
        if Assigned(fonParseStartArray) then _DoParseStartArray('');
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
      if P1 > BufferLength then ALJSONDocError(CALJSONParseError);
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
      if BufferPos > BufferLength then ALJSONDocError(CALJSONParseError);

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
      if P1 > BufferLength then ALJSONDocError(CALJSONParseError);
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
    if BufferPos > BufferLength then ALJSONDocError(CALJSONParseError);  // .... : ....
                                                                         //      ^BufferPos
    {$ENDREGION}

    {$REGION 'if aNameValueSeparator is absent then it is just a value'}
    if LNameValueSeparator <> ':' then begin

      //Node without name can be ONLY present inside an array node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then ALJSONDocError(CALJSONParseError);
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then ALJSONDocError(CALJSONParseError);
      end
      else begin
        if (CurrIndex < 0) or
           (NamePaths.Count = 0) or
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then ALJSONDocError(CALJSONParseError);
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
    if BufferPos > BufferLength then ALJSONDocError(CALJSONParseError); // .... " ....
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

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocError(CALJSONParseError);

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0)  or
           (WorkingNode.nodetype <> ntObject) then ALJSONDocError(CALJSONParseError);

        //create the node according the the braket char and add it to the workingnode
        if c = '{' then LNode := CreateNode(CurrName, ntObject)
        else LNode := CreateNode(CurrName, ntarray);
        try
          WorkingNode.ChildNodes.Add(LNode);
        except
          LNode.Free;
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
           (TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]) <> ntobject) then ALJSONDocError(CALJSONParseError);

        //update the path
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(-1, CurrName, pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event and update the CurrIndex if it's an array
      if c = '{' then begin
        if Assigned(fonParseStartObject) then _DoParseStartObject(CurrName)
      end
      else begin
        if Assigned(fonParseStartArray) then _DoParseStartArray(CurrName);
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
      if P1 > BufferLength then ALJSONDocError(CALJSONParseError);
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
      if BufferPos > BufferLength then ALJSONDocError(CALJSONParseError);

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
      if P1 > BufferLength then ALJSONDocError(CALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos


    end;
    {$ENDREGION}

    {$REGION 'create the named text node'}

    //Node withe name MUST be ONLY present inside an object node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(CALJSONParseError);
      if (CurrIndex >= 0)  or
         (WorkingNode.nodetype <> ntObject) then ALJSONDocError(CALJSONParseError);
    end
    else begin
      if (CurrIndex >= 0) or
         (NamePaths.Count = 0) or
         (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntObject) then ALJSONDocError(CALJSONParseError);
    end;

    //create the node
    _createNode(currIndex,CurrName,CurrValue,LQuoteChar in ['"','''']);

    {$ENDREGION}

  end;

var BOMSequence: integer;
    InCommentLine: integer;
    c: ansiChar;

Begin

  //
  // NOTE: the childNodes of the ContainerNode
  //       must have been cleared by the calling function!
  //
  // NOTE: ContainerNode must have fDocument assigned
  //
  // NOTE: ContainerNode must be ntobject or nil (sax mode)
  //

  //error if Immutable
  if (doImmutable in Options) then ALJSONDocError(cALJSONImmutable);

  //event fonParseStartDocument
  DoParseStartDocument;

  //init WorkingNode and NotSaxMode, CurrIndex and DecodeJSONReferences
  WorkingNode := ContainerNode;
  NotSaxMode := assigned(ContainerNode);
  DecodeJSONReferences := not (poIgnoreControlCharacters in ParseOptions);
  CurrIndex := -1;

  //init ObjectPaths or NamePaths
  if (NotSaxMode) and
     (not assigned(fonParseText)) and
     (not assigned(FonParseStartObject)) and
     (not assigned(FonParseEndObject)) and
     (not assigned(FonParseStartArray)) and
     (not assigned(FonParseEndArray)) then begin
    ObjectPaths := TALIntegerList.Create(false{OwnsObjects});
    NamePaths := nil;
  end
  else begin
    ObjectPaths := nil;
    NamePaths := TALNvStringList.Create;
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
      else _AddNameItemToNamePath('', pointer(ntObject));
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
        if c <> '{' then ALJSONDocError(cALJSONParseError);
        inc(bufferPos);
        break;
      end;
    end;

    //analyze all the nodes
    if poAllowComments in ParseOptions then begin
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
      if ObjectPaths.Count > 0 then ALJSONDocError(cALJSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then ALJSONDocError(cALJSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if WorkingNode <> nil then ALJSONDocError(cALJSONParseError);

    //event fonParseEndDocument
    DoParseEndDocument;

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.Free
    else NamePaths.Free;

  end;

end;


{*************************************************************}
{Last version of the spec: http://bsonspec.org/#/specification}
procedure TALJSONDocument.ParseBSON(const RawBSONStream: TStream;
                                    const RawBSONString: AnsiString;
                                    const ContainerNode: TALJSONNode);

Const BufferSize: integer = 8192;

Var Buffer: AnsiString;
    BufferLength: Integer;
    BufferPos: Integer;
    CurrName: AnsiString;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNode;
    NamePaths: TALStringList;
    ObjectPaths: TObjectList<TALJSONNode>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
      if BufferPos <= length(Buffer) then ALMove(Pbyte(Buffer)[BufferPos - 1],
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
    LB := PathSeparator;
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
         (((NotSaxMode) and (TALJSONNode(NamePaths.Objects[I]).nodetype <> ntarray)) or
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(const index: AnsiString;
                                  const Args: array of const;
                                  const NodeSubType: TALJSONNodeSubType);
  begin
    DoParseText(GetPathStr('[' + index + ']'), '', Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithName(const name: AnsiString;
                                 const Args: array of const;
                                 const NodeSubType: TALJSONNodeSubType);
  begin
    DoParseText(GetPathStr(Name), Name, Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseText(const NameOrIndex: AnsiString;
                         const Args: array of const;
                         const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(fonParseText) then begin
      if notSaxMode then begin
        if WorkingNode.nodetype=ntarray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
      end
      else begin
        if NamePaths.Count = 0 then ALJSONDocError(CALJSONParseError);
        if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntArray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartObject(const Name: AnsiString);
  begin
    DoParseStartObject(GetPathStr, Name);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndObject;
  begin
    if NamePaths.Count = 0 then ALJSONDocError(CALJSONParseError);
    DoParseEndObject(GetPathStr, NamePaths[NamePaths.Count - 1])
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: AnsiString);
  begin
    DoParseStartArray(GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if NamePaths.Count = 0 then ALJSONDocError(CALJSONParseError);
    DoParseEndArray(GetPathStr, NamePaths[NamePaths.Count - 1]);
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
      if NamePaths.Count = 0 then ALJSONDocError(CALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(nameOrIndex, Obj)
      else _AddNameItemToNamePath(nameOrIndex, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt64Node(const name: AnsiString;
                             const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LInt64: Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt64, sizeof(LInt64));
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetInt64(LInt64);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LInt64], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LInt64], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt32Node(const name: AnsiString;
                             const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LInt32: Int32;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setint32(LInt32);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LInt32], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LInt32], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTextNode(const name: AnsiString;
                            const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LInt32: Int32;
      LText: ansiString;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    while (BufferPos + LInt32 - 1 > BufferLength) do
      if not ExpandBuffer then ALJSONDocError(cALBSONParseError);
    ALCopyStr(Buffer,LText,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Settext(LText);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LText], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LText], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createFloatNode(const name: AnsiString;
                             const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LDouble: Double;
  begin
    if BufferPos > BufferLength - sizeof(Double) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(Double) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(pbyte(Buffer)[BufferPos-1], LDouble, sizeof(Double));
    BufferPos := BufferPos + sizeof(Double);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetFloat(LDouble);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LDouble], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LDouble], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBinaryNode(const name: AnsiString;
                              const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LInt32: Int32;
      LBinSubtype: byte;
      LBinData: ansiString;
  begin
    //Get size
    if BufferPos > BufferLength - sizeof(LInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    //Get the subtype
    if BufferPos > BufferLength then begin
      ExpandBuffer;
      if BufferPos > BufferLength then ALJSONDocError(cALBSONParseError);
    end;
    LBinSubtype := Byte(Buffer[BufferPos]);
    BufferPos := BufferPos + 1;

    //Get the data
    while (BufferPos + LInt32 - 1 > BufferLength) do
      if not ExpandBuffer then ALJSONDocError(cALBSONParseError);
    ALCopyStr(Buffer,LBinData,BufferPos,LInt32);
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.setbinary(LBinData, LBinSubtype);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LBinData, LBinSubtype], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LBinData, LBinSubtype], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createObjectIDNode(const name: AnsiString;
                                const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LObjectID: AnsiString;
  begin
    if BufferPos > BufferLength - 12{length(aObjectID)} + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - 12{length(aObjectID)} + 1 then ALJSONDocError(cALBSONParseError);
    end;
    Setlength(LObjectID, 12); // ObjectId is a 12-byte BSON type
    ALMove(Pbyte(Buffer)[BufferPos-1], pbyte(LObjectID)[0], 12{length(aObjectID)}); // pbyte(aObjectID)[0] to not have a jump in uniqueString (aObjectID is already unique thanks to Setlength)
    BufferPos := BufferPos + 12{length(aObjectID)};

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetObjectID(LObjectID);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LObjectID], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LObjectID], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBooleanNode(const name: AnsiString;
                               const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LBool: Boolean;
  begin
    if BufferPos > BufferLength then begin
      ExpandBuffer;
      if BufferPos > BufferLength then ALJSONDocError(cALBSONParseError);
    end;
    if Buffer[BufferPos] = #$00 then LBool := False
    else if Buffer[BufferPos] = #$01 then LBool := true
    else begin
      ALJSONDocError(cALBSONParseError);
      LBool := False; // to hide a warning;
    end;
    BufferPos := BufferPos + 1;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setbool(LBool);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LBool], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LBool], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createDateTimeNode(const name: AnsiString;
                                const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LDateTime: TdateTime;
      LInt64: Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt64, sizeof(LInt64));
    LDateTime := ALUnixMsToDateTime(LInt64);
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setdatetime(LDateTime);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LDateTime], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LDateTime], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTimestampNode(const name: AnsiString;
                                 const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LTimestamp: TALBSONTimestamp;
      LInt64: Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt64) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt64, sizeof(LInt64));
    LTimestamp.I64 := LInt64;
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetTimestamp(LTimestamp);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LTimestamp.W1, LTimestamp.W2], NodeSubType);
    end
    else begin
      _DoParseText(Name, [LTimestamp.W1, LTimestamp.W2], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createnullNode(const name: AnsiString;
                            const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
  begin
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.Setnull(true);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, ['null'], NodeSubType);
    end
    else begin
      _DoParseText(Name, ['null'], NodeSubType);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createRegExNode(const name: AnsiString;
                             const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
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
    if P1 > BufferLength then ALJSONDocError(cALBSONParseError);
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
    if BufferPos > BufferLength then ALJSONDocError(cALBSONParseError);
    inc(BufferPos);
    if BufferPos > BufferLength then ExpandBuffer;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetRegEx(LRegEx, LRegExOptions);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
        raise;
      end;
      _DoParseText(Name, [LRegEx, Byte(LRegExOptions)], NodeSubType)
    end
    else begin
      _DoParseText(Name, [LRegEx, Byte(LRegExOptions)], NodeSubType)
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createJavascriptNode(const name: AnsiString;
                                  const NodeSubType: TALJSONNodeSubType);
  var LNode: TALJsonNode;
      LJavascript: ansiString;
      LInt32: Int32;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) + 1 then begin
      ExpandBuffer;
      if BufferPos > BufferLength - sizeof(LInt32) + 1 then ALJSONDocError(cALBSONParseError);
    end;
    ALMove(Pbyte(Buffer)[BufferPos-1], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    while (BufferPos + LInt32 - 1 > BufferLength) do
      if not ExpandBuffer then ALJSONDocError(cALBSONParseError);
    ALCopyStr(Buffer,LJavascript,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);
      if WorkingNode.nodetype=ntarray then LNode := CreateNode('', nttext)
      else LNode := CreateNode(Name, nttext);
      try
        LNode.SetJavascript(LJavascript);
        WorkingNode.ChildNodes.Add(LNode);
      except
        LNode.Free;
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
  Var LNode: TALJsonNode;
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
        if (ObjectPaths.Count = 0) then ALJSONDocError(cALBSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then ALJSONDocError(cALBSONParseError);
      end;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        if assigned(ObjectPaths) then LNode := ObjectPaths[ObjectPaths.Count - 1]
        else LNode := TALJSONNode(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (LNode <> WorkingNode) then ALJSONDocError(cALBSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then ALJSONDocError(cALBSONParseError);

        //if working node <> containernode then we can go to one level up
        If WorkingNode<>ContainerNode then begin

          //init WorkingNode to the parentNode
          WorkingNode := WorkingNode.ParentNode;

        end

        //if working node = containernode then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

        //calculate anodeTypeInt
        LNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then ALJSONDocError(cALBSONParseError);

      end;

      //call the DoParseEndObject/array event
      if Assigned(fonParseEndObject) then begin
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
      else ALJSONDocError(cALBSONParseError);
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
    if P1 > BufferLength then ALJSONDocError(cALBSONParseError);
    BufferPos := P1 + 1;
    if BufferPos > BufferLength then ExpandBuffer;
    {$ENDREGION}

    {$REGION 'Begin Object/Array'}
    // ... { ....
    // ... [ ....
    if LNodeSubType in [nstObject,nstArray] then begin

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocError(cALBSONParseError);

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
          LNode.Free;
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
        if Assigned(fonParseStartObject) then _DoParseStartObject(CurrName)
      end
      else begin
         if Assigned(fonParseStartArray) then _DoParseStartArray(CurrName);
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

      else ALJSONDocError(cALBSONParseError);
    end;
    {$ENDREGION}

  end;

Begin

  //
  // NOTE: the childNodes of the ContainerNode
  //       must have been cleared by the calling function!
  //
  // NOTE: ContainerNode must have fDocument assigned
  //
  // NOTE: ContainerNode must be ntobject or nil (sax mode)
  //

  //error if Immutable
  if (doImmutable in Options) then ALJSONDocError(cALJSONImmutable);

  //event fonParseStartDocument
  DoParseStartDocument;

  //init WorkingNode and NotSaxMode
  WorkingNode := ContainerNode;
  NotSaxMode := assigned(ContainerNode);

  //init ObjectPaths or NamePaths
  if (NotSaxMode) and
     (not assigned(fonParseText)) and
     (not assigned(FonParseStartObject)) and
     (not assigned(FonParseEndObject)) and
     (not assigned(FonParseStartArray)) and
     (not assigned(FonParseEndArray)) then begin
    ObjectPaths := TObjectList<TALJSONNode>.Create(false{OwnsObjects});
    NamePaths := nil;
  end
  else begin
    ObjectPaths := nil;
    NamePaths := TALStringList.Create;
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
      if NotSaxMode then NamePaths.AddObject('[-1]', WorkingNode)
      else NamePaths.AddObject('[-1]', pointer(ntObject));
    end;

    //analyze all the nodes
    While (BufferPos <= BufferLength) or ExpandBuffer do
      AnalyzeNode;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then ALJSONDocError(cALBSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then ALJSONDocError(cALBSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if WorkingNode <> nil then ALJSONDocError(cALBSONParseError);

    //event fonParseEndDocument
    DoParseEndDocument;

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.Free
    else NamePaths.Free;

  end;

end;

{***********************************}
procedure TALJSONDocument.ReleaseDoc;
begin
  if (doImmutable in Options) then ALJSONDocError(cALJSONImmutable);
  if assigned(FDocumentNode) then FreeAndNil(FDocumentNode);
end;

{*****************************************************************}
{Loads a string representation of an JSON document and activates it.
 Call LoadFromJSONString to assign a string as the value of the JSON document. Unlike the JSON property, which lets you assign JSON on a line-by-line
 basis, LoadFromJSONString treats the text of the JSON document as a whole.
 The str parameter is a string containing the text of an JSON document. It should represent the JSON text encoded using 8 bits char (utf-8, iso-8859-1, etc)
 After assigning the JSON property as the contents of the document, LoadFromJSONString sets the Active property to true.}
procedure TALJSONDocument.LoadFromJSONString(const Str: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
begin
  if saxMode then SetActive(False)
  else begin
    if ClearChildNodes then releaseDoc;
    SetActive(True);
  end;
  ParseJSON(nil, Str, FDocumentNode)
end;

{****************************************************}
{Loads an JSON document from a stream and activates it.
 Call LoadFromJSONStream to load the JSON document from a stream.
 *Stream is a stream object that can be used to read the string of JSON that makes up the document.
 After loading the document from Stream, LoadFromJSONStream sets the Active property to true.}
procedure TALJSONDocument.LoadFromJSONStream(const Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
begin
  if saxMode then SetActive(False)
  else begin
    if ClearChildNodes then releaseDoc;
    SetActive(True);
  end;
  ParseJSON(Stream, '', FDocumentNode)
end;

{**************************************}
{Loads an JSON document and activates it.
 Call LoadFromJSONFile to load the JSON document specified by AFileName and set the Active property to true so
 that you can examine or modify the document.
 *AFileName is the name of the JSON document to load from disk. If AFileName is an empty string, TALJSONDocument uses the value of the
  FileName property. If AFileName is not an empty string, TALJSONDocument changes the FileName property to AFileName.
 Once you have loaded an JSON document, any changes you make to the document are not saved back to disk until you call the SaveToFile method.}
procedure TALJSONDocument.LoadFromJSONFile(const FileName: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
var FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromJSONStream(FileStream, saxMode, ClearChildNodes);
  finally
    FileStream.Free;
  end;
end;

{*****************************************************************************************************************************************}
procedure TALJSONDocument.LoadFromBSONString(const Str: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
begin
  if saxMode then SetActive(False)
  else begin
    if ClearChildNodes then releaseDoc;
    SetActive(True);
  end;
  ParseBSON(nil, Str, FDocumentNode)
end;

{*****************************************************************************************************************************************}
procedure TALJSONDocument.LoadFromBSONStream(const Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
begin
  if saxMode then SetActive(False)
  else begin
    if ClearChildNodes then releaseDoc;
    SetActive(True);
  end;
  ParseBSON(Stream, '', FDocumentNode)
end;

{********************************************************************************************************************************************}
procedure TALJSONDocument.LoadFromBSONFile(const FileName: AnsiString; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
var FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromBSONStream(FileStream, saxMode, ClearChildNodes);
  finally
    FileStream.Free;
  end;
end;

{***********************************}
{Saves the JSON document to a stream.
 Call SaveToStream to save the contents of the JSON document to the stream specified by Stream.}
procedure TALJSONDocument.SaveToJSONStream(const Stream: TStream);
begin
  CheckActive;
  node.SaveToJSONStream(Stream);
end;

{******************************}
{Saves the JSON document to disk.
 Call SaveToFile to save any modifications you have made to the parsed JSON document.
 AFileName is the name of the file to save.}
procedure TALJSONDocument.SaveToJSONFile(const FileName: AnsiString);
begin
  CheckActive;
  node.SaveToJSONFile(FileName);
end;

{************************************************}
{Saves the JSON document to a string-type variable.
 Call SaveToJSON to save the contents of the JSON document to the string-type variable specified by JSON. SaveToJSON writes the contents of JSON document
 using 8 bits char (utf-8, iso-8859-1, etc) as an encoding system, depending on the type of the JSON parameter.
 Unlike the JSON property, which lets you write individual lines from the JSON document, SaveToJSON writes the entire text of the JSON document.}
procedure TALJSONDocument.SaveToJSONString(var str: AnsiString);
begin
  CheckActive;
  node.SaveToJSONString(Str);
end;

{****************************************************************}
procedure TALJSONDocument.SaveToBsonStream(const Stream: TStream);
begin
  CheckActive;
  node.SaveToBsonStream(Stream);
end;

{*******************************************************************}
procedure TALJSONDocument.SaveToBsonFile(const FileName: AnsiString);
begin
  CheckActive;
  node.SaveToBsonFile(FileName);
end;

{**************************************************************}
procedure TALJSONDocument.SaveToBsonString(var str: AnsiString);
begin
  CheckActive;
  node.SaveToBsonString(Str);
end;

{*************************************}
{Returns the value of the JSON property.
 GetJSON is the read implementation of the JSON property.}
function TALJSONDocument.GetJSON: AnsiString;
begin
  SaveToJSONString(Result);
end;

{*************************************}
{Returns the value of the BSON property.
 GetBSON is the read implementation of the BSON property.}
function TALJSONDocument.GetBSON: AnsiString;
begin
  SaveToBSONString(Result);
end;

{**********************************}
{Sets the value of the JSON property.
 SetJSON is the write implementation of the JSON property.
 *Value contains the raw (unparsed) JSON to assign.}
procedure TALJSONDocument.SetJSON(const Value: AnsiString);
begin
  LoadFromJSONString(Value, False{saxMode}, true{ClearChildNodes});
end;

{**********************************}
{Sets the value of the BSON property.
 SetBSON is the write implementation of the BSON property.
 *Value contains the raw (unparsed) BSON to assign.}
procedure TALJSONDocument.SetBSON(const Value: AnsiString);
begin
  LoadFromBSONString(Value, False{saxMode}, true{ClearChildNodes});
end;

{***********************************}
procedure TALJSONDocument.CheckActive;
begin
  if not Assigned(FDocumentNode) then ALJSONDocError(CALJSONNotActive);
end;

{**********************************************************************************************************************************************}
function TALJSONDocument.AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
begin
  Result := Node.AddChild(NodeName, NodeType, Index);
end;

{***************************************************************************************************************************************************}
function TALJSONDocument.AddChild(const Path: array of AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
begin
  Result := Node.AddChild(Path, NodeType, Index);
end;

{******************************************************************************************************}
function TALJSONDocument.CreateNode(const NodeName: AnsiString; NodeType: TALJSONNodeType): TALJSONNode;
begin
  Result := ALCreateJSONNode(NodeName, NodeType);
end;

{********************************************}
{Returns the value of the ChildNodes property.
 GetChildNodes is the read implementation of the ChildNodes property.}
function TALJSONDocument.GetChildNodes: TALJSONNodeList;
begin
  Result := Node.ChildNodes;
end;

{*****************************************************************************}
function TALJSONDocument.GetChildNode(const nodeName: ansiString): TALJSONNode;
begin
  result := Node.GetChildNode(nodeName);
end;

{****************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueText(const nodeName: ansiString; const default: AnsiString): AnsiString;
begin
  result := Node.GetChildNodeValueText(nodeName, default);
end;

{*********************************************************************************************************}
function TALJSONDocument.GetChildNodeValueFloat(const nodeName: ansiString; const default: Double): Double;
begin
  result := Node.GetChildNodeValueFloat(nodeName, default);
end;

{******************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueDateTime(const nodeName: ansiString; const default: TDateTime): TDateTime;
begin
  result := Node.GetChildNodeValueDateTime(nodeName, default);
end;

{*********************************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueTimestamp(const nodeName: ansiString; const default: TALBSONTimestamp): TALBSONTimestamp;
begin
  result := Node.GetChildNodeValueTimestamp(nodeName, default);
end;

{********************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueObjectID(const nodeName: ansiString; const default: AnsiString): AnsiString;   // return a "byte" string
begin
  result := Node.GetChildNodeValueObjectID(nodeName, default);
end;

{***********************************************************************************************************}
function TALJSONDocument.GetChildNodeValueInt32(const nodeName: ansiString; const default: Integer): Integer;
begin
  result := Node.GetChildNodeValueInt32(nodeName, default);
end;

{*******************************************************************************************************}
function TALJSONDocument.GetChildNodeValueInt64(const nodeName: ansiString; const default: Int64): Int64;
begin
  result := Node.GetChildNodeValueInt64(nodeName, default);
end;

{**********************************************************************************************************}
function TALJSONDocument.GetChildNodeValueBool(const nodeName: ansiString; const default: Boolean): Boolean;
begin
  result := Node.GetChildNodeValueBool(nodeName, default);
end;

{**********************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueJavascript(const nodeName: ansiString; const default: AnsiString): AnsiString;
begin
  result := Node.GetChildNodeValueJavascript(nodeName, default);
end;

{*****************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueRegEx(const nodeName: ansiString; const default: ansiString): ansiString;
begin
  result := Node.GetChildNodeValueRegEx(nodeName, default);
end;

{******************************************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueRegExOptions(const nodeName: ansiString; const default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  result := Node.GetChildNodeValueRegExOptions(nodeName, default);
end;

{******************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueBinary(const nodeName: ansiString; const default: AnsiString): AnsiString;   // return a "byte" string
begin
  result := Node.GetChildNodeValueBinary(nodeName, default);
end;

{*************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueBinarySubType(const nodeName: ansiString; const default: byte): byte;
begin
  result := Node.GetChildNodeValueBinarySubType(nodeName, default);
end;

{**********************************************************************************}
function TALJSONDocument.GetChildNode(const path: array of ansiString): TALJSONNode;
begin
  result := Node.GetChildNode(path);
end;

{*********************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueText(const path: array of ansiString; const default: AnsiString): AnsiString;
begin
  result := Node.GetChildNodeValueText(path, default);
end;

{**************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueFloat(const path: array of ansiString; const default: Double): Double;
begin
  result := Node.GetChildNodeValueFloat(path, default);
end;

{***********************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueDateTime(const path: array of ansiString; const default: TDateTime): TDateTime;
begin
  result := Node.GetChildNodeValueDateTime(path, default);
end;

{**************************************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueTimestamp(const path: array of ansiString; const default: TALBSONTimestamp): TALBSONTimestamp;
begin
  result := Node.GetChildNodeValueTimestamp(path, default);
end;

{*************************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueObjectID(const path: array of ansiString; const default: AnsiString): AnsiString;   // return a "byte" string
begin
  result := Node.GetChildNodeValueObjectID(path, default);
end;

{****************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueInt32(const path: array of ansiString; const default: Integer): Integer;
begin
  result := Node.GetChildNodeValueInt32(path, default);
end;

{************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueInt64(const path: array of ansiString; const default: Int64): Int64;
begin
  result := Node.GetChildNodeValueInt64(path, default);
end;

{***************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueBool(const path: array of ansiString; const default: Boolean): Boolean;
begin
  result := Node.GetChildNodeValueBool(path, default);
end;

{***************************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueJavascript(const path: array of ansiString; const default: AnsiString): AnsiString;
begin
  result := Node.GetChildNodeValueJavascript(path, default);
end;

{**********************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueRegEx(const path: array of ansiString; const default: ansiString): ansiString;
begin
  result := Node.GetChildNodeValueRegEx(path, default);
end;

{***********************************************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueRegExOptions(const path: array of ansiString; const default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  result := Node.GetChildNodeValueRegExOptions(path, default);
end;

{***********************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueBinary(const path: array of ansiString; const default: AnsiString): AnsiString;   // return a "byte" string
begin
  result := Node.GetChildNodeValueBinary(path, default);
end;

{******************************************************************************************************************}
function TALJSONDocument.GetChildNodeValueBinarySubType(const path: array of ansiString; const default: byte): byte;
begin
  result := Node.GetChildNodeValueBinarySubType(path, default);
end;

{***************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueText(const nodeName: ansiString; const value: AnsiString);
begin
  Node.SetChildNodeValueText(nodeName, value);
end;

{************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueFloat(const nodeName: ansiString; const value: Double);
begin
  Node.SetChildNodeValueFloat(nodeName, value);
end;

{******************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueDateTime(const nodeName: ansiString; const value: TDateTime);
begin
  Node.SetChildNodeValueDateTime(nodeName, value);
end;

{**************************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueTimestamp(const nodeName: ansiString; const value: TALBSONTimestamp);
begin
  Node.SetChildNodeValueTimestamp(nodeName, value);
end;

{*******************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueObjectID(const nodeName: ansiString; const value: AnsiString);
begin
  Node.SetChildNodeValueObjectID(nodeName, value);
end;

{*************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueInt32(const nodeName: ansiString; const value: Integer);
begin
  Node.SetChildNodeValueInt32(nodeName, value);
end;

{***********************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueInt64(const nodeName: ansiString; const value: Int64);
begin
  Node.SetChildNodeValueInt64(nodeName, value);
end;

{************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueBool(const nodeName: ansiString; const value: Boolean);
begin
  Node.SetChildNodeValueBool(nodeName, value);
end;

{*********************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueJavascript(const nodeName: ansiString; const value: AnsiString);
begin
  Node.SetChildNodeValueJavascript(nodeName, value);
end;

{****************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueRegEx(const nodeName: ansiString; const value: ansiString);
begin
  Node.SetChildNodeValueRegEx(nodeName, value);
end;

{********************************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueRegExOptions(const nodeName: ansiString; const value: TALPerlRegExOptions);
begin
  Node.SetChildNodeValueRegExOptions(nodeName, value);
end;

{*****************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueBinary(const nodeName: ansiString; const value: AnsiString);
begin
  Node.SetChildNodeValueBinary(nodeName, value);
end;

{******************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueBinarySubType(const nodeName: ansiString; const value: byte);
begin
  Node.SetChildNodeValueBinarySubType(nodeName, value);
end;

{**************************************************************************}
procedure TALJSONDocument.SetChildNodeValueNull(const nodeName: ansiString);
begin
  Node.SetChildNodeValueNull(nodeName);
end;

{********************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueText(const path: array of ansiString; const value: AnsiString);
begin
  Node.SetChildNodeValueText(path, value);
end;

{*****************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueFloat(const path: array of ansiString; const value: Double);
begin
  Node.SetChildNodeValueFloat(path, value);
end;

{***********************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueDateTime(const path: array of ansiString; const value: TDateTime);
begin
  Node.SetChildNodeValueDateTime(path, value);
end;

{*******************************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueTimestamp(const path: array of ansiString; const value: TALBSONTimestamp);
begin
  Node.SetChildNodeValueTimestamp(path, value);
end;

{************************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueObjectID(const path: array of ansiString; const value: AnsiString);
begin
  Node.SetChildNodeValueObjectID(path, value);
end;

{******************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueInt32(const path: array of ansiString; const value: Integer);
begin
  Node.SetChildNodeValueInt32(path, value);
end;

{****************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueInt64(const path: array of ansiString; const value: Int64);
begin
  Node.SetChildNodeValueInt64(path, value);
end;

{*****************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueBool(const path: array of ansiString; const value: Boolean);
begin
  Node.SetChildNodeValueBool(path, value);
end;

{**************************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueJavascript(const path: array of ansiString; const value: AnsiString);
begin
  Node.SetChildNodeValueJavascript(path, value);
end;

{*********************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueRegEx(const path: array of ansiString; const value: ansiString);
begin
  Node.SetChildNodeValueRegEx(path, value);
end;

{*************************************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueRegExOptions(const path: array of ansiString; const value: TALPerlRegExOptions);
begin
  Node.SetChildNodeValueRegExOptions(path, value);
end;

{**********************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueBinary(const path: array of ansiString; const value: AnsiString);
begin
  Node.SetChildNodeValueBinary(path, value);
end;

{***********************************************************************************************************}
procedure TALJSONDocument.SetChildNodeValueBinarySubType(const path: array of ansiString; const value: byte);
begin
  Node.SetChildNodeValueBinarySubType(path, value);
end;

{*******************************************************************************}
procedure TALJSONDocument.SetChildNodeValueNull(const path: array of ansiString);
begin
  Node.SetChildNodeValueNull(path);
end;

{************************************************************************}
{Indicates whether the TJSONDocument instance represents an empty document.
 Call IsEmptyDoc to determine whether the TALJSONDocument instance represents an empty document.
 IsEmptyDoc returns true if the Document property is not set or if this object represents a
 document with no child nodes.}
function TALJSONDocument.IsEmptyDoc: Boolean;
begin
  Result := not (Assigned(FDocumentNode) and FDocumentNode.hasChildNodes);
end;

{**************************************}
{Returns the value of the Node property.
 GetDocumentNode is the read implementation of the Node property.}
function TALJSONDocument.GetDocumentNode: TALJSONNode;
begin
  CheckActive;
  Result := FDocumentNode;
end;

{***********************************************}
{Returns the value of the NodeIndentStr property.
 GetNodeIndentStr is the read implementation of the NodeIndentStr property.}
function TALJSONDocument.GetNodeIndentStr: AnsiString;
begin
  Result := FNodeIndentStr;
end;

{********************************************}
{Sets the value of the NodeIndentStr property.
 SetNodeIndentStr is the write implementation of the NodeIndentStr property.
 *Value is the string that is inserted before nested nodes to indicate a level of nesting.}
procedure TALJSONDocument.SetNodeIndentStr(const Value: AnsiString);
begin
  FNodeIndentStr := Value;
end;

{*****************************************}
{Returns the value of the Options property.
 GetOptions is the read implementation of the Options property.}
function TALJSONDocument.GetOptions: TALJSONDocOptions;
begin
  Result := FOptions;
end;

{**************************************}
{Sets the value of the Options property.
 GetOptions is the write implementation of the Options property.
 *Value is the set of options to assign.}
procedure TALJSONDocument.SetOptions(const Value: TALJSONDocOptions);
begin
  FOptions := Value;
end;

{**********************************************}
{Returns the value of the ParseOptions property.
 GetParseOptions is the read implementation of the ParseOptions property.}
function TALJSONDocument.GetParseOptions: TALJSONParseOptions;
begin
  Result := FParseOptions;
end;

{*******************************************}
{Sets the value of the ParseOptions property.
 GetParseOptions is the write implementation of the ParseOptions property.
 *Value is the set of parser options to assign.}
procedure TALJSONDocument.SetParseOptions(const Value: TALJSONParseOptions);
begin
  FParseOptions := Value;
end;

{****************************************************************}
procedure TALJSONDocument.SetPathSeparator(const Value: ansiChar);
begin
  FPathSeparator := Value;
end;

{**************************************************}
function TALJSONDocument.GetPathSeparator: ansiChar;
begin
  result := fPathSeparator;
end;

{*********************************************}
procedure TALJSONDocument.DoParseStartDocument;
begin
  if Assigned(fonParseStartDocument) then fonParseStartDocument(Self);
end;

{*******************************************}
procedure TALJSONDocument.DoParseEndDocument;
begin
  if Assigned(fonParseEndDocument) then fonParseEndDocument(Self);
end;

{*************************************************************************************************************************************************}
procedure TALJSONDocument.DoParseText(const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(fonParseText) then fonParseText(Self, Path, name, Args, NodeSubType);
end;

{*******************************************************************************************}
procedure TALJSONDocument.DoParseStartObject(const Path: AnsiString; const Name: AnsiString);
begin
  if Assigned(fonParseStartObject) then fonParseStartObject(Self, Path, name);
end;

{*****************************************************************************************}
procedure TALJSONDocument.DoParseEndObject(const Path: AnsiString; const Name: AnsiString);
begin
  if Assigned(fonParseEndObject) then fonParseEndObject(Self, Path, name);
end;

{******************************************************************************************}
procedure TALJSONDocument.DoParseStartArray(const Path: AnsiString; const Name: AnsiString);
begin
  if Assigned(fonParseStartArray) then fonParseStartArray(Self, Path, name);
end;

{****************************************************************************************}
procedure TALJSONDocument.DoParseEndArray(const Path: AnsiString; const Name: AnsiString);
begin
  if Assigned(fonParseEndArray) then fonParseEndArray(Self, Path, name);
end;

{**********************************************************}
{Creates the object that implements the ChildNodes property}
function TALJSONNode.CreateChildList: TALJSONNodeList;
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
  result := TALJSONNodeList.Create(Self);
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONNode.InternalGetChildNodes: TALJSONNodeList;
begin
  Result := nil; //virtual;
end;

{**************************************************}
function TALJSONNode.GetChildNodes: TALJSONNodeList;
begin
  Result := nil; // hide warning
  ALJsonDocError(CALJsonOperationError,GetNodeType)
end;

{****************************************************************}
procedure TALJSONNode.SetChildNodes(const Value: TALJSONNodeList);
begin
  ALJsonDocError(CALJsonOperationError,GetNodeType)
end;

{*************************************************************************}
function TALJSONNode.GetChildNode(const nodeName: ansiString): TALJSONNode;
begin
  result := ChildNodes.findNode(nodeName);
end;

{************************************************************************************************************}
function TALJSONNode.GetChildNodeValueText(const nodeName: ansiString; const default: AnsiString): AnsiString;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetText(default);
end;

{*****************************************************************************************************}
function TALJSONNode.GetChildNodeValueFloat(const nodeName: ansiString; const default: Double): Double;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetFloat(default);
end;

{**************************************************************************************************************}
function TALJSONNode.GetChildNodeValueDateTime(const nodeName: ansiString; const default: TDateTime): TDateTime;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetDateTime(default);
end;

{*****************************************************************************************************************************}
function TALJSONNode.GetChildNodeValueTimestamp(const nodeName: ansiString; const default: TALBSONTimestamp): TALBSONTimestamp;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetTimestamp(default);
end;

{****************************************************************************************************************}
function TALJSONNode.GetChildNodeValueObjectID(const nodeName: ansiString; const default: AnsiString): AnsiString;  // return a "byte" string
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetObjectID(default);
end;

{*******************************************************************************************************}
function TALJSONNode.GetChildNodeValueInt32(const nodeName: ansiString; const default: Integer): Integer;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetInt32(default);
end;

{***************************************************************************************************}
function TALJSONNode.GetChildNodeValueInt64(const nodeName: ansiString; const default: Int64): Int64;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetInt64(default);
end;

{******************************************************************************************************}
function TALJSONNode.GetChildNodeValueBool(const nodeName: ansiString; const default: Boolean): Boolean;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBool(default);
end;

{******************************************************************************************************************}
function TALJSONNode.GetChildNodeValueJavascript(const nodeName: ansiString; const default: AnsiString): AnsiString;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetJavascript(default);
end;

{*************************************************************************************************************}
function TALJSONNode.GetChildNodeValueRegEx(const nodeName: ansiString; const default: ansiString): ansiString;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetRegEx(default);
end;

{**************************************************************************************************************************************}
function TALJSONNode.GetChildNodeValueRegExOptions(const nodeName: ansiString; const default: TALPerlRegExOptions): TALPerlRegExOptions;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetRegExOptions(default);
end;

{**************************************************************************************************************}
function TALJSONNode.GetChildNodeValueBinary(const nodeName: ansiString; const default: AnsiString): AnsiString;  // return a "byte" string
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBinary(default);
end;

{*********************************************************************************************************}
function TALJSONNode.GetChildNodeValueBinarySubType(const nodeName: ansiString; const default: byte): byte;
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBinarySubType(default);
end;

{******************************************************************************}
function TALJSONNode.GetChildNode(const path: array of ansiString): TALJSONNode;
var I: integer;
begin
  result := Self;
  for I := low(path) to high(path) do begin
    result := result.ChildNodes.findNode(path[I]);
    if (result = nil) then exit;
  end;
end;

{*****************************************************************************************************************}
function TALJSONNode.GetChildNodeValueText(const path: array of ansiString; const default: AnsiString): AnsiString;
var LNode: TALJSONNode;
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

{**********************************************************************************************************}
function TALJSONNode.GetChildNodeValueFloat(const path: array of ansiString; const default: Double): Double;
var LNode: TALJSONNode;
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

{*******************************************************************************************************************}
function TALJSONNode.GetChildNodeValueDateTime(const path: array of ansiString; const default: TDateTime): TDateTime;
var LNode: TALJSONNode;
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

{**********************************************************************************************************************************}
function TALJSONNode.GetChildNodeValueTimestamp(const path: array of ansiString; const default: TALBSONTimestamp): TALBSONTimestamp;
var LNode: TALJSONNode;
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

{*********************************************************************************************************************}
function TALJSONNode.GetChildNodeValueObjectID(const path: array of ansiString; const default: AnsiString): AnsiString;  // return a "byte" string
var LNode: TALJSONNode;
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

{************************************************************************************************************}
function TALJSONNode.GetChildNodeValueInt32(const path: array of ansiString; const default: Integer): Integer;
var LNode: TALJSONNode;
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
function TALJSONNode.GetChildNodeValueInt64(const path: array of ansiString; const default: Int64): Int64;
var LNode: TALJSONNode;
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

{***********************************************************************************************************}
function TALJSONNode.GetChildNodeValueBool(const path: array of ansiString; const default: Boolean): Boolean;
var LNode: TALJSONNode;
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

{***********************************************************************************************************************}
function TALJSONNode.GetChildNodeValueJavascript(const path: array of ansiString; const default: AnsiString): AnsiString;
var LNode: TALJSONNode;
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

{******************************************************************************************************************}
function TALJSONNode.GetChildNodeValueRegEx(const path: array of ansiString; const default: ansiString): ansiString;
var LNode: TALJSONNode;
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

{*******************************************************************************************************************************************}
function TALJSONNode.GetChildNodeValueRegExOptions(const path: array of ansiString; const default: TALPerlRegExOptions): TALPerlRegExOptions;
var LNode: TALJSONNode;
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

{*******************************************************************************************************************}
function TALJSONNode.GetChildNodeValueBinary(const path: array of ansiString; const default: AnsiString): AnsiString;  // return a "byte" string
var LNode: TALJSONNode;
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

{**************************************************************************************************************}
function TALJSONNode.GetChildNodeValueBinarySubType(const path: array of ansiString; const default: byte): byte;
var LNode: TALJSONNode;
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

{***********************************************************************************************}
procedure TALJSONNode.SetChildNodeValueText(const nodeName: ansiString; const value: AnsiString);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetText(value)
  else LNode.SetText(value);
end;

{********************************************************************************************}
procedure TALJSONNode.SetChildNodeValueFloat(const nodeName: ansiString; const value: Double);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetFloat(value)
  else LNode.SetFloat(value);
end;

{**************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueDateTime(const nodeName: ansiString; const value: TDateTime);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetDateTime(value)
  else LNode.SetDateTime(value);
end;

{**********************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueTimestamp(const nodeName: ansiString; const value: TALBSONTimestamp);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetTimestamp(value)
  else LNode.SetTimestamp(value);
end;

{***************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueObjectID(const nodeName: ansiString; const value: AnsiString);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetObjectID(value)
  else LNode.SetObjectID(value);
end;

{*********************************************************************************************}
procedure TALJSONNode.SetChildNodeValueInt32(const nodeName: ansiString; const value: Integer);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt32(value)
  else LNode.SetInt32(value);
end;

{*******************************************************************************************}
procedure TALJSONNode.SetChildNodeValueInt64(const nodeName: ansiString; const value: Int64);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt64(value)
  else LNode.SetInt64(value);
end;

{********************************************************************************************}
procedure TALJSONNode.SetChildNodeValueBool(const nodeName: ansiString; const value: Boolean);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBool(value)
  else LNode.SetBool(value);
end;

{*****************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueJavascript(const nodeName: ansiString; const value: AnsiString);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetJavascript(value)
  else LNode.SetJavascript(value);
end;

{************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueRegEx(const nodeName: ansiString; const value: ansiString);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegEx(value)
  else LNode.SetRegEx(value);
end;

{****************************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueRegExOptions(const nodeName: ansiString; const value: TALPerlRegExOptions);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegExOptions(value)
  else LNode.SetRegExOptions(value);
end;

{*************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueBinary(const nodeName: ansiString; const value: AnsiString);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinary(value)
  else LNode.SetBinary(value);
end;

{**************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueBinarySubType(const nodeName: ansiString; const value: byte);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinarySubType(value)
  else LNode.SetBinarySubType(value);
end;

{**********************************************************************}
procedure TALJSONNode.SetChildNodeValueNull(const nodeName: ansiString);
var LNode: TALJSONNode;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetNull(true)
  else LNode.SetNull(true);
end;

{****************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueText(const path: array of ansiString; const value: AnsiString);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{*************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueFloat(const path: array of ansiString; const value: Double);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{*******************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueDateTime(const path: array of ansiString; const value: TDateTime);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{***************************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueTimestamp(const path: array of ansiString; const value: TALBSONTimestamp);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{********************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueObjectID(const path: array of ansiString; const value: AnsiString);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{**************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueInt32(const path: array of ansiString; const value: Integer);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueInt64(const path: array of ansiString; const value: Int64);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{*************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueBool(const path: array of ansiString; const value: Boolean);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{**********************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueJavascript(const path: array of ansiString; const value: AnsiString);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{*****************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueRegEx(const path: array of ansiString; const value: ansiString);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{*********************************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueRegExOptions(const path: array of ansiString; const value: TALPerlRegExOptions);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{******************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueBinary(const path: array of ansiString; const value: AnsiString);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{*******************************************************************************************************}
procedure TALJSONNode.SetChildNodeValueBinarySubType(const path: array of ansiString; const value: byte);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{***************************************************************************}
procedure TALJSONNode.SetChildNodeValueNull(const path: array of ansiString);
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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
function TALJSONNode.GetHasChildNodes: Boolean;
Var LNodeList: TALJSONNodeList;
begin
  LNodeList := InternalGetChildNodes;
  Result := assigned(LNodeList) and (LNodeList.Count > 0);
end;

{***********************************************}
function TALJSONNode.GetNodeValueStr: ansiString;
begin
  ALJsonDocError(CALJsonOperationError,GetNodeType);
  result := ''; // hide warning
end;

{********************************************}
function TALJSONNode.GetNodeValueInt64: int64;
begin
  ALJsonDocError(CALJsonOperationError,GetNodeType);
  result := 0; // hide warning
end;

{*************************************************************************************************}
procedure TALJSONNode.SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType);
begin
  ALJsonDocError(CALJsonOperationError,GetNodeType);
end;

{********************************************************************************************}
procedure TALJSONNode.SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  ALJsonDocError(CALJsonOperationError,GetNodeType);
end;

{*****************************************************************************************************************************}
procedure TALJSONNode.SetNodeValue(const StrValue: AnsiString; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  ALJsonDocError(CALJsonOperationError,GetNodeType);
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONNode.GetText: AnsiString;
begin

  case NodeSubType of
    nstFloat: begin // return the formated float
                if Assigned(FDocument) and (Fdocument.FormatSettings <> @ALDefaultFormatSettings) then result := ALFloatToStr(GetFloat, Fdocument.FormatSettings^)
                else result := GetNodeValueStr;
              end;
    nstText: result := GetNodeValueStr;  // return the raw text
    nstObject: result := GetNodeValueStr;  // return the raw objectID
    nstArray: result := GetNodeValueStr;  // error
    nstObjectID: result := GetNodeValueStr; // error
    nstBoolean: result := GetNodeValueStr;  // return true or false
    nstDateTime: begin // return the formated datetime
                   if Assigned(FDocument) and (Fdocument.FormatSettings <> @ALDefaultFormatSettings) then result := ALDateTimeToStr(GetDateTime, Fdocument.FormatSettings^)
                   else result := GetNodeValueStr;
                 end;
    nstNull: result := GetNodeValueStr; // return null
    nstRegEx: result := GetNodeValueStr; // return the raw regex (without the options)
    nstBinary: result := GetNodeValueStr; // return the raw binary (without the binary subtype)
    nstJavascript: result := GetNodeValueStr; // return the raw javascript
    nstInt32: result := GetNodeValueStr;  // return the number
    nstTimestamp: result := GetNodeValueStr;  // return the number (as int64)
    nstInt64: result := GetNodeValueStr;  // return the number
    else AlJSONDocError(cALJSONInvalidBSONNodeSubType);
  end;

end;

{******************************************************************}
function TALJSONNode.GetText(const default: AnsiString): AnsiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetText;
end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONNode.SetText(const Value: AnsiString);
begin
  setNodeValue(Value, nstText);
end;

{***********************************}
// By default json (ie: javascript) treats all numbers as floating-point values.
// To let other system (ie: mongoDB) understand the type of the number
// we provide the helper functions NumberLong() to handle 64-bit integers
// and NumberInt() to handle 32-bit integers (and some others). theses helper functions are
// used when saving the json document.
function TALJSONNode.GetNodeValueInterchange(const SkipNodeSubTypeHelper: boolean = False): AnsiString;

  procedure _GetObjectID;
  begin
    if SkipNodeSubTypeHelper then result := '"'+ALBinToHex(ObjectID)+'"'
    else result := 'ObjectId("'+ALBinToHex(ObjectID)+'")';
  end;

  procedure _GetBinary;
  begin
    if SkipNodeSubTypeHelper then result := '"'+ALBase64EncodeString(Binary)+'"'
    else result := 'BinData('+alinttostr(BinarySubType)+', "'+ALBase64EncodeString(Binary)+'")';
  end;

  procedure _GetDateTime;
  begin
    if SkipNodeSubTypeHelper then result := ALFormatDateTime('''"''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z"''', DateTime, ALDefaultFormatSettings)
    else result := ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', DateTime, ALDefaultFormatSettings)
  end;

  procedure _Getint32;
  begin
    if SkipNodeSubTypeHelper then result := text
    else result := 'NumberInt(' + text + ')'
  end;

  procedure _Getint64;
  begin
    if SkipNodeSubTypeHelper then result := text
    else result := 'NumberLong(' + text + ')';
  end;

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

  procedure _GetTimestamp;
  begin
    if SkipNodeSubTypeHelper then result := '"Timestamp('+alinttostr(GetTimeStamp.W1)+', '+alinttostr(GetTimeStamp.W2)+')"'
    else result := 'Timestamp('+alinttostr(GetTimeStamp.W1)+', '+alinttostr(GetTimeStamp.W2)+')';
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

{************************************}
function TALJSONNode.GetFloat: Double;
begin
  case NodeSubType of
    nstFloat: PInt64(@result)^ := GetNodeValueInt64;
    nstInt32,
    nstInt64: Result := GetNodeValueInt64;
    else begin
      AlJSONDocError(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{***********************************************************}
function TALJSONNode.GetFloat(const default: Double): Double;
begin
  if NodeSubType = nstNull then result := default
  else result := GetFloat;
end;

{**************************************************}
procedure TALJSONNode.SetFloat(const Value: Double);
begin
  setNodeValue(PInt64(@Value)^, nstFloat);
end;

{******************************************}
function TALJSONNode.GetDateTime: TDateTime;
begin
  if NodeSubType = nstDateTime then PInt64(@result)^ := GetNodeValueInt64
  else begin
    AlJSONDocError(cALJSONInvalidBSONNodeSubType);
    result := 0; // to hide a warning;
  end;
end;

{********************************************************************}
function TALJSONNode.GetDateTime(const default: TDateTime): TDateTime;
begin
  if NodeSubType = nstNull then result := default
  else result := GetDateTime;
end;

{********************************************************}
procedure TALJSONNode.SetDateTime(const Value: TDateTime);
begin
  setNodeValue(PInt64(@Value)^, nstDateTime);
end;

{**************************************************}
function TALJSONNode.GetTimestamp: TALBSONTimestamp;
begin
  if NodeSubType = nstTimestamp then result.I64 := GetNodeValueInt64
  else begin
    AlJSONDocError(cALJSONInvalidBSONNodeSubType);
    result.I64 := 0; // to hide a warning;
  end;
end;

{***********************************************************************************}
function TALJSONNode.GetTimestamp(const default: TALBSONTimestamp): TALBSONTimestamp;
begin
  if NodeSubType = nstNull then result := default
  else result := GetTimestamp;
end;

{***************************************************************}
procedure TALJSONNode.SetTimestamp(const Value: TALBSONTimestamp);
begin
  setNodeValue(Value.I64, nstTimestamp);
end;

{*******************************************}
function TALJSONNode.GetObjectID: ansiString;
begin
  if NodeSubType = nstObjectID then result := GetNodeValueStr
  else begin
    AlJSONDocError(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{**********************************************************************}
function TALJSONNode.GetObjectID(const default: AnsiString): AnsiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetObjectID;
end;

{*********************************************************}
procedure TALJSONNode.SetObjectID(const Value: AnsiString);
begin
  if length(Value) <> 12 {div sizeof(ansiChar)} then AlJSONDocError('ObjectID must have 12 bytes');
  setNodeValue(Value, nstObjectID);
end;

{*************************************}
function TALJSONNode.GetInt32: Integer;
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
                   (LInt64 < system.int32.MinValue) then AlJSONDocError(cALJSONInvalidBSONNodeSubType);
                result := LInt64;
              end;
    nstInt32: begin
                LInt64 := GetNodeValueInt64;
                if (LInt64 > system.int32.MaxValue) or
                   (LInt64 < system.int32.MinValue) then AlJSONDocError(cALJSONInvalidBSONNodeSubType);
                result := LInt64;
              end;
    nstInt64: Result := GetNodeValueInt64;
    else begin
      AlJSONDocError(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{*************************************************************}
function TALJSONNode.GetInt32(const default: Integer): Integer;
begin
  if NodeSubType = nstNull then result := default
  else result := GetInt32;
end;

{***************************************************}
procedure TALJSONNode.SetInt32(const Value: Integer);
begin
  setNodeValue(Value, nstInt32);
end;

{***********************************}
function TALJSONNode.GetInt64: Int64;
var LDouble: Double;
begin
  case NodeSubType of
    nstFloat: begin
                PInt64(@LDouble)^ := GetNodeValueInt64;
                result := trunc(LDouble);
                if result <> LDouble then AlJSONDocError(cALJSONInvalidBSONNodeSubType); // https://stackoverflow.com/questions/41779801/single-double-and-precision
                                                                                         // Only values that are in form m*2^e, where m and e are integers can be stored in a floating point variable
                                                                                         // so all integer can be store in the form m*2^e (ie: m = m*2^0)
                                                                                         // so we can compare result <> aDouble without the need of samevalue
              end;
    nstInt32,
    nstInt64: Result := GetNodeValueInt64;
    else begin
      AlJSONDocError(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{*********************************************************}
function TALJSONNode.GetInt64(const default: Int64): Int64;
begin
  if NodeSubType = nstNull then result := default
  else result := GetInt64;
end;

{*************************************************}
procedure TALJSONNode.SetInt64(const Value: Int64);
begin
  setNodeValue(Value, nstInt64);
end;

{************************************}
function TALJSONNode.GetBool: Boolean;
begin
  if NodeSubType = nstBoolean then begin
    if GetNodeValueInt64 = 0 then result := False
    else result := true;
  end
  else begin
    AlJSONDocError(cALJSONInvalidBSONNodeSubType);
    result := False; // to hide a warning;
  end;
end;

{************************************************************}
function TALJSONNode.GetBool(const default: Boolean): Boolean;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBool;
end;

{**************************************************}
procedure TALJSONNode.SetBool(const Value: Boolean);
begin
  if Value then setNodeValue(1, nstBoolean)
  else setNodeValue(0, nstBoolean);
end;

{************************************}
function TALJSONNode.GetNull: Boolean;
begin
  result := NodeSubType = nstNull;
end;

{**************************************************}
procedure TALJSONNode.SetNull(const Value: Boolean);
begin
  if Value then setNodeValue(0, nstNull)
  else ALJSONDocError('Only "true" is allowed for setNull property');
end;

{*********************************************}
function TALJSONNode.GetJavascript: AnsiString;
begin
  if NodeSubType = nstJavascript then result := GetNodeValueStr
  else begin
    AlJSONDocError(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{************************************************************************}
function TALJSONNode.GetJavascript(const default: AnsiString): AnsiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetJavascript;
end;

{***********************************************************}
procedure TALJSONNode.SetJavascript(const Value: AnsiString);
begin
  setNodeValue(Value, nstJavascript);
end;

{****************************************}
function TALJSONNode.GetRegEx: ansiString;
begin
  if NodeSubType = nstRegEx then result := GetNodeValueStr
  else begin
    AlJSONDocError(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{*******************************************************************}
function TALJSONNode.GetRegEx(const default: ansiString): ansiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetRegEx;
end;

{********************************************************}
procedure TALJSONNode.SetRegEx(const Pattern: ansiString);
begin
  setNodeValue(Pattern, 0, nstRegEx);
end;

{********************************************************************************************}
procedure TALJSONNode.SetRegEx(const Pattern: ansiString; const Options: TALPerlRegExOptions);
begin
  setNodeValue(Pattern, byte(Options), nstRegEx);
end;

{********************************************************}
function TALJSONNode.GetRegExOptions: TALPerlRegExOptions;
begin
  if NodeSubType = nstRegEx then result := TALPerlRegExOptions(byte(GetNodeValueInt64))
  else begin
    AlJSONDocError(cALJSONInvalidBSONNodeSubType);
    result := []; // to hide a warning;
  end;
end;

{********************************************************************************************}
function TALJSONNode.GetRegExOptions(const default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  if NodeSubType = nstNull then result := default
  else result := GetRegExOptions;
end;

{**********************************************************************}
procedure TALJSONNode.SetRegExOptions(const Value: TALPerlRegExOptions);
begin
  if NodeSubType <> nstRegEx then ALJSONDocError('You can set regex options only to a regex node');
  setNodeValue(byte(Value), nstRegEx);
end;

{*****************************************}
function TALJSONNode.GetBinary: AnsiString;
begin
  if NodeSubType = nstBinary then result := GetNodeValueStr
  else begin
    AlJSONDocError(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{********************************************************************}
function TALJSONNode.GetBinary(const default: AnsiString): AnsiString;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBinary;
end;

{******************************************************}
procedure TALJSONNode.SetBinary(const Data: AnsiString);
begin
  setNodeValue(Data, 0, nstBinary); // 0 = Default BSON type
end;

{***************************************************************************}
procedure TALJSONNode.SetBinary(const Data: AnsiString; const Subtype: byte);
begin
  setNodeValue(Data, Subtype, nstBinary);
end;

{******************************************}
function TALJSONNode.GetBinarySubType: byte;
begin
  if NodeSubType = nstBinary then result := byte(GetNodeValueInt64)
  else begin
    AlJSONDocError(cALJSONInvalidBSONNodeSubType);
    result := 0; // to hide a warning;
  end;
end;

{***************************************************************}
function TALJSONNode.GetBinarySubType(const default: byte): byte;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBinarySubType;
end;

{**********************************************************}
procedure TALJSONNode.SetBinarySubType(const Subtype: byte);
begin
  if NodeSubType <> nstBinary then ALJSONDocError('You can set binary subtype only to a binary node');
  setNodeValue(Subtype, nstBinary);
end;

{*******************************************************}
{Returns the document object in which this node appears.}
function TALJSONNode.GetOwnerDocument: TALJSONDocument;
begin
  Result := FDocument;
end;

{*******************************************************************}
procedure TALJSONNode.SetOwnerDocument(const Value: TALJSONDocument);
var I: Integer;
    LNodeList: TALJSONNodeList;
begin
  if FDocument <> Value then begin
    if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
    FDocument := Value;
    LNodeList := InternalGetChildNodes;
    if Assigned(LNodeList) then
      for I := 0 to LNodeList.Count - 1 do
        LNodeList[I].SetOwnerDocument(Value);
  end;
end;

{************************}
{returns the parent node.}
function TALJSONNode.GetParentNode: TALJSONNode;
begin
  Result := FParentNode;
end;

{******************************************}
{Sets the value of the ParentNode property.}
procedure TALJSONNode.SetParentNode(const Value: TALJSONNode);
begin
  if FParentNode <> Value then begin
    if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
    If assigned(Value) then SetOwnerDocument(Value.OwnerDocument)
    else SetOwnerDocument(nil);
    FParentNode := Value;
  end;
end;

{*******************************************************************}
{Returns the JSON that corresponds to the subtree rooted at this node.
 GetJSON returns the JSON that corresponds to this node and any child nodes it contains.}
function TALJSONNode.GetJSON: AnsiString;
begin
  SaveToJSONString(result);
end;

{************************************************}
{SetJSON reload the node with the new given value }
procedure TALJSONNode.SetJSON(const Value: AnsiString);
Begin
  LoadFromJSONString(Value, true{ClearChildNodes});
end;

{*******************************************************************}
{Returns the BSON that corresponds to the subtree rooted at this node.
 GetBSON returns the BSON that corresponds to this node and any child nodes it contains.}
function TALJSONNode.GetBSON: AnsiString;
begin
  SaveToBSONString(result);
end;

{************************************************}
{SetBSON reload the node with the new given value }
procedure TALJSONNode.SetBSON(const Value: AnsiString);
Begin
  LoadFromBSONString(Value, true{ClearChildNodes});
end;

{*****************************************************************}
{Returns the number of parents for this node in the node hierarchy.
 NestingLevel returns the number of ancestors for this node in the node hierarchy.}
function TALJSONNode.NestingLevel: Integer;
var PNode: TALJSONNode;
begin
  Result := 0;
  PNode := ParentNode;
  while PNode <> nil do begin
    Inc(Result);
    PNode := PNode.ParentNode;
  end;
end;

{*********************************************************}
constructor TALJSONNode.Create(const NodeName: AnsiString);
Begin
  FDocument := nil;
  FParentNode := nil;
  fNodeName := NodeName;
end;

{***************************************************************}
//will create all the nodevalue and childnodelist to be sure that
//multiple thread can safely read at the same time the node
procedure TALJSONNode.MultiThreadPrepare;
var I: integer;
begin
  if NodeType = ntText then begin

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

  else begin
    For I := 0 to ChildNodes.Count - 1 do
      ChildNodes[I].MultiThreadPrepare;
  end;
end;

{******************************************************************************************************************************************}
function TALJSONNode.AddChild(const NodeName: AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
  Result := ALCreateJSONNode(NodeName,NodeType);
  Try
    ChildNodes.Insert(Index, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{***********************************************************************************************************************************************}
function TALJSONNode.AddChild(const Path: array of AnsiString; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
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

{**************************************************************************************************************}
function TALJSONNode.AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNode;
begin
  Result := AddChild('', NodeType, Index);
end;

{********************************************************************}
function TALJSONNode.DeleteChild(const NodeName: AnsiString): boolean;
var I: integer;
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
  I := ChildNodes.IndexOf(NodeName);
  if I >= 0 then begin
    ChildNodes.Delete(I);
    result := True;
  end
  else result := False;
end;

{*************************************************************************}
function TALJSONNode.DeleteChild(const Path: array of AnsiString): boolean;
var LNode: TALJSONNode;
    LTmpNode: TALJSONNode;
    I: integer;
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
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

{******************************************}
{Returns the next child of this node’s parent.
 NextSibling returns the node that follows this one in the parent node’s ChildNodes property list.
 If this node is the last node in its parent’s child list, NextSibling raises an exception.}
function TALJSONNode.NextSibling: TALJSONNode;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, 1)
  else Result := nil;
end;

{************************************************}
{Returns the previous child of this node’s parent.
 PreviousSibling returns the node that precedes this one in the parent node’s ChildNodes property list.
 If this node is the first node in its parent’s child list, PreviousSibling raises an exception.}
function TALJSONNode.PreviousSibling: TALJSONNode;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, -1)
  else Result := nil;
end;

{*****************************************************}
procedure TALJSONNode.SaveToJson(const Stream: TStream;
                                 Var buffer: ansiString);

Const BufferSize: integer = 8192;

Var NodeStack: Tstack<TALJSONNode>;
    CurrentNode: TalJSONNode;
    CurrentParentNode: TalJSONNode;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteTextNode2Buffer(aTextNode:TALJSONNode);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNode);
  var LNodeList: TALJSONNodeList;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNode);
  Begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr)+1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer('}');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNode);
  var LNodeList: TALJSONNodeList;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNode);
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
  NodeStack := Tstack<TALJSONNode>.Create;
  Try

    {init buffer string}
    Setlength(Buffer, BufferSize); // will make buffer uniquestring
    BufferPos := 0;
    LastWrittenChar := '{';
    EncodeControlCharacters := (FDocument = nil) or (not (poIgnoreControlCharacters in FDocument.ParseOptions));
    SkipNodeSubTypeHelper := (FDocument <> nil) and (poSkipNodeSubTypeHelper in FDocument.ParseOptions);
    SaveInt64AsText := SkipNodeSubTypeHelper and (FDocument <> nil) and (poSaveInt64AsText in FDocument.ParseOptions);
    AutoIndentNode := (FDocument <> nil) and (doNodeAutoIndent in FDocument.Options);
    if FDocument <> nil then IndentStr := FDocument.NodeIndentStr
    else IndentStr := vALDefaultNodeIndent;
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
          else AlJSONDocError(cAlJSONInvalidNodeType);
        end;

      CurrentParentNode := CurrentNode.ParentNode;
    end;

    {Write the buffer}
    if assigned(Stream) then _WriteBuffer2Stream(Buffer, BufferPos)
    else setlength(Buffer,BufferPos);

  finally
    NodeStack.Free;
  end;
end;

{***********************************}
{Saves the JSON document to a stream.
 Call SaveToStream to save the contents of the JSON document to the stream specified by Stream.}
procedure TALJSONNode.SaveToJSONStream(const Stream: TStream);
var buffer: ansiString;
begin
  SaveToJson(Stream, buffer);
end;

{******************************}
{Saves the JSON document to disk.
 Call SaveToFile to save any modifications you have made to the parsed JSON document.
 AFileName is the name of the file to save.}
procedure TALJSONNode.SaveToJSONFile(const FileName: AnsiString);
Var LfileStream: TfileStream;
    LTmpFilename: AnsiString;
begin
  if (assigned(FDocument)) and
     (doProtectedSave in fDocument.Options) then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LfileStream := TfileStream.Create(String(LTmpFilename),fmCreate);
    Try
      SaveToJSONStream(LfileStream);
    finally
      LfileStream.Free;
    end;

    if LTmpFilename <> FileName then begin
      if TFile.Exists(string(FileName)) then TFile.Delete(string(FileName));
      TFile.Move(string(LTmpFilename), string(FileName));
    end;

  except
    if (LTmpFilename <> FileName) and
       (TFile.Exists(string(LTmpFilename))) then TFile.Delete(string(LTmpFilename));
    raise;
  end;
end;

{************************************************}
{Saves the JSON document to a string-type variable.
 Call SaveToJSON to save the contents of the JSON document to the string-type variable specified by JSON. SaveToJSON writes the contents of JSON document
 using 8 bits char (utf-8, iso-8859-1, etc) as an encoding system, depending on the type of the JSON parameter.
 Unlike the JSON property, which lets you write individual lines from the JSON document, SaveToJSON writes the entire text of the JSON document.}
procedure TALJSONNode.SaveToJSONString(var str: AnsiString);
begin
  SaveToJson(nil, Str);
end;

{*****************************************************}
procedure TALJSONNode.SaveToBson(const Stream: TStream;
                                 Var buffer: ansiString);

Const BufferSize: integer = 8192;

Var NodeStack: Tstack<TALJSONNode>;
    NodeIndexStack: TALintegerList;
    NodeStartPosStack: TALInt64List;
    CurrentNode: TalJSONNode;
    CurrentParentNode: TalJSONNode;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStr2Buffer(const str:AnsiString); overload;
  Begin
    _Write2Buffer(pointer(str)^,length(Str));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStr2Buffer(const index:integer); overload;
  Begin
    _WriteStr2Buffer(alinttostr(index));
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
  Procedure _WriteFloatValue2Buffer(aTextNode:TALJSONNode);
  var LDouble: Double;
  begin
    LDouble := aTextNode.Float;
    _Write2Buffer(LDouble, sizeOf(LDouble));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x02 + name + \x00 + length (int32) + string + \x00
  Procedure _WriteTextValue2Buffer(aTextNode:TALJSONNode);
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
  Procedure _WriteBinaryValue2Buffer(aTextNode:TALJSONNode);
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
  Procedure _WriteObjectIDValue2Buffer(aTextNode:TALJSONNode);
  begin
    _WriteStr2Buffer(aTextNode.ObjectID);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x08 + name + \x00 + \x00 => Boolean "false"
  // \x08 + name + \x00 + \x01	=> Boolean "true"
  Procedure _WriteBooleanValue2Buffer(aTextNode:TALJSONNode);
  begin
    if not aTextNode.bool then _WriteStr2Buffer(#$00)
    else _WriteStr2Buffer(#$01);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x09 + name + \x00 + int64
  Procedure _WriteDateTimeValue2Buffer(aTextNode:TALJSONNode);
  var LInt64: system.Int64;
  begin
    LInt64 := ALDateTimeToUnixMs(aTextNode.DateTime);
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x11 + name + \x00 + int64
  Procedure _WriteTimestampValue2Buffer(aTextNode:TALJSONNode);
  var LInt64: system.Int64;
  begin
    LInt64 := aTextNode.Timestamp.I64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \xOB + name + \x00 + (byte*) + \x00 + (byte*) + \x00
  Procedure _WriteRegExValue2Buffer(aTextNode:TALJSONNode);
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
  Procedure _WriteJavascriptValue2Buffer(aTextNode:TALJSONNode);
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
  Procedure _WriteInt32Value2Buffer(aTextNode:TALJSONNode);
  var LInt32: system.Int32;
  begin
    LInt32 := aTextNode.int32;
    _Write2Buffer(LInt32, sizeOf(LInt32));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x12 + name + \x00 + int64
  Procedure _WriteInt64Value2Buffer(aTextNode:TALJSONNode);
  var LInt64: system.Int64;
  begin
    LInt64 := aTextNode.int64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteTextNode2Buffer(aTextNode:TALJSONNode; aNodeIndex: integer);
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
        else AlJSONDocError(cALJSONInvalidBSONNodeSubType);
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
        else AlJSONDocError(cALJSONInvalidBSONNodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNode; aNodeIndex: integer);
  var LNodeList: TALJSONNodeList;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNode; aNodeStartPos: system.Int64);
  Begin
    _WriteStr2Buffer(#$00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNode; aNodeIndex: integer);
  var LNodeList: TALJSONNodeList;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNode; aNodeStartPos: system.Int64);
  Begin
    _WriteStr2Buffer(#$00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

begin
  If NodeType <> ntobject then exit;

  CurrentParentNode := nil;
  NodeStack := Tstack<TALJSONNode>.Create;
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
          else AlJSONDocError(cAlJSONInvalidNodeType);
        end;

      CurrentParentNode := CurrentNode.ParentNode;
    end;

    {Write the buffer}
    if assigned(Stream) then _WriteBuffer2Stream(Buffer, BufferPos)
    else setlength(Buffer,BufferPos);

  finally
    NodeStack.Free;
    NodeIndexStack.Free;
    NodeStartPosStack.Free;
  end;
end;

{************************************************************}
procedure TALJSONNode.SaveToBsonStream(const Stream: TStream);
var buffer: ansiString;
begin
  SaveToBson(Stream, buffer);
end;

{***************************************************************}
procedure TALJSONNode.SaveToBsonFile(const FileName: AnsiString);
Var LfileStream: TfileStream;
    LTmpFilename: AnsiString;
begin
  if (assigned(FDocument)) and
     (doProtectedSave in fDocument.Options) then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LfileStream := TfileStream.Create(String(LTmpFilename),fmCreate);
    Try
      SaveToBsonStream(LfileStream);
    finally
      LfileStream.Free;
    end;

    if LTmpFilename <> FileName then begin
      if TFile.Exists(string(FileName)) then TFile.Delete(string(FileName));
      TFile.Move(string(LTmpFilename), string(FileName));
    end;

  except
    if (LTmpFilename <> FileName) and
       (TFile.Exists(string(LTmpFilename))) then TFile.Delete(string(LTmpFilename));
    raise;
  end;
end;

{**********************************************************}
procedure TALJSONNode.SaveToBsonString(var str: AnsiString);
begin
  SaveToBson(nil, Str);
end;

{*****************************************************************************************************}
procedure TALJSONNode.LoadFromJSONString(const Str: AnsiString; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then ALJsonDocError(CALJsonOperationError,GetNodeType);
  if ClearChildNodes then ChildNodes.Clear;
  Try
    FDocument.ParseJson(nil, Str, self)
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*****************************************************************************************************}
procedure TALJSONNode.LoadFromJSONStream(const Stream: TStream; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then ALJsonDocError(CALJsonOperationError,GetNodeType);
  if ClearChildNodes then ChildNodes.Clear;
  Try
    FDocument.ParseJSON(Stream, '', self)
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{********************************************************************************************************}
procedure TALJSONNode.LoadFromJSONFile(const FileName: AnsiString; Const ClearChildNodes: Boolean = True);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromJSONStream(LfileStream, ClearChildNodes);
  finally
    LfileStream.Free;
  end;
end;

{*****************************************************************************************************}
procedure TALJSONNode.LoadFromBSONString(const Str: AnsiString; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then ALJsonDocError(CALJsonOperationError,GetNodeType);
  if ClearChildNodes then ChildNodes.Clear;
  Try
    FDocument.ParseBSON(nil, Str, self)
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*****************************************************************************************************}
procedure TALJSONNode.LoadFromBSONStream(const Stream: TStream; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then ALJsonDocError(CALJsonOperationError,GetNodeType);
  if ClearChildNodes then ChildNodes.Clear;
  Try
    FDocument.ParseBSON(Stream, '', self)
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{********************************************************************************************************}
procedure TALJSONNode.LoadFromBSONFile(const FileName: AnsiString; Const ClearChildNodes: Boolean = True);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromBSONStream(LfileStream, ClearChildNodes);
  finally
    LfileStream.Free;
  end;
end;

{********************************************************************}
constructor TALJSONObjectNode.Create(const NodeName: AnsiString = '');
begin
  inherited create(NodeName);
  FChildNodes := nil;
end;

{***********************************}
destructor TALJSONObjectNode.Destroy;
begin
  If assigned(FChildNodes) then FreeAndNil(FchildNodes);
  inherited;
end;

{*******************************************************}
function TALJSONObjectNode.GetChildNodes: TALJSONNodeList;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{*********************************************************************}
procedure TALJSONObjectNode.SetChildNodes(const Value: TALJSONNodeList);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
  If Assigned(FChildNodes) then FreeAndNil(FchildNodes);
  FChildNodes := Value;
end;

{******************************************************}
function TALJSONObjectNode.GetNodeType: TALJSONNodeType;
begin
  Result := NtObject;
end;

{************************************************************}
function TALJSONObjectNode.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := NstObject;
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONObjectNode.InternalGetChildNodes: TALJSONNodeList;
begin
  Result := FChildNodes;
end;

{*******************************************************************}
constructor TALJSONArrayNode.Create(const NodeName: AnsiString = '');
begin
  inherited create(NodeName);
  FChildNodes := nil;
end;

{***********************************}
destructor TALJSONArrayNode.Destroy;
begin
  If assigned(FChildNodes) then FreeAndNil(FchildNodes);
  inherited;
end;

{*******************************************************}
function TALJSONArrayNode.GetChildNodes: TALJSONNodeList;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{*********************************************************************}
procedure TALJSONArrayNode.SetChildNodes(const Value: TALJSONNodeList);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
  If Assigned(FChildNodes) then FreeAndNil(FchildNodes);
  FChildNodes := Value;
end;

{***************************************************}
function TALJSONArrayNode.GetNodeType: TALJSONNodeType;
begin
  Result := NtArray;
end;

{***********************************************************}
function TALJSONArrayNode.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := NstArray;
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONArrayNode.InternalGetChildNodes: TALJSONNodeList;
begin
  Result := FChildNodes;
end;

{******************************************************************}
constructor TALJSONTextNode.Create(const NodeName: AnsiString = '');
begin
  inherited create(NodeName);
  fNodeSubType := nstText;
  fRawNodeValueStr := '';
  FRawNodeValueInt64 := 0;
  fRawNodeValueDefined := [nvStr];
end;

{****************************************************}
function TALJSONTextNode.GetNodeType: TALJSONNodeType;
begin
  Result := NtText;
end;

{**********************************************************}
function TALJSONTextNode.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := fNodeSubType;
end;

{***************************************************}
function TALJSONTextNode.GetNodeValueStr: ansiString;
begin
  if nvStr in fRawNodeValueDefined then result := fRawNodeValueStr
  else begin

    if not (nvInt64 in fRawNodeValueDefined) then ALJsonDocError(CALJsonOperationError,GetNodeType);
    if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);

    case fNodeSubType of
      nstFloat: ALFloatToStr(GetFloat, fRawNodeValueStr, ALDefaultFormatSettings);
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      nstBoolean: ALBoolToStr(fRawNodeValueStr, getBool, 'true', 'false');
      nstDateTime: ALDateTimeToStr(GetDateTime, fRawNodeValueStr, ALDefaultFormatSettings);
      nstNull: fRawNodeValueStr := 'null';
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
      nstInt32: ALintToStr(GetInt32, fRawNodeValueStr);
      nstTimestamp: ALformat('Timestamp(%u, %u)', [GetTimestamp.W1,GetTimestamp.W2], fRawNodeValueStr);
      nstInt64: ALintToStr(GetInt64, fRawNodeValueStr);
      else ALJsonDocError(CALJsonOperationError,GetNodeType);
    end;

    fRawNodeValueDefined := fRawNodeValueDefined + [nvStr];
    result := fRawNodeValueStr;

  end;
end;

{************************************************}
function TALJSONTextNode.GetNodeValueInt64: int64;
var LDouble: Double;
    LBool: boolean;
    LDateTime: TdateTime;
    LInt32: system.int32;
    LTimestamp: TALBSONTimestamp;
begin
  if nvInt64 in fRawNodeValueDefined then result := fRawNodeValueInt64
  else begin

    if not (nvStr in fRawNodeValueDefined) then ALJsonDocError(CALJsonOperationError,GetNodeType);
    if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);

    case fNodeSubType of
      nstFloat: begin
                  IF not ALTryStrToFloat(fRawNodeValueStr, LDouble, ALDefaultFormatSettings) then ALJSONDocError('%s is not a valid Float', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := Pint64(@LDouble)^;
                end;
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      nstBoolean: begin
                    IF not ALTryStrToBool(fRawNodeValueStr, LBool) then ALJSONDocError('%s is not a valid Boolean', [fRawNodeValueStr]);
                    fRawNodeValueInt64 := ALBoolToInt(LBool);
                  end;
      nstDateTime: begin
                     IF not ALTryStrToDateTime(fRawNodeValueStr, LDateTime, ALdefaultFormatSettings) then ALJSONDocError('%s is not a valid Datetime', [fRawNodeValueStr]);
                     fRawNodeValueInt64 := Pint64(@LDateTime)^;
                   end;
      nstNull:  begin
                  fRawNodeValueInt64 := 0;
                end;
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
      nstInt32: begin
                  IF not ALTryStrToInt(fRawNodeValueStr, LInt32) then ALJSONDocError('%s is not a valid Int32', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := LInt32;
                end;
      nstTimestamp: begin
                      IF not ALJSONTryStrToTimestamp(fRawNodeValueStr, LTimestamp) then ALJSONDocError('%s is not a valid Timestamp', [fRawNodeValueStr]);
                      fRawNodeValueInt64 := LTimestamp.I64;
                    end;
      nstInt64: begin
                  IF not ALTryStrToInt64(fRawNodeValueStr, fRawNodeValueInt64) then ALJSONDocError('%s is not a valid Int64', [fRawNodeValueStr]);
                end;
      else ALJsonDocError(CALJsonOperationError,GetNodeType);
    end;

    fRawNodeValueDefined := fRawNodeValueDefined + [nvInt64];
    result := fRawNodeValueInt64;

  end;
end;

{*****************************************************************************************************}
procedure TALJSONTextNode.SetNodeValue(const Value: AnsiString; const NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
  fNodeSubType := NodeSubType;
  fRawNodeValueStr := Value;
  fRawNodeValueDefined := [nvStr];
end;

{************************************************************************************************}
procedure TALJSONTextNode.SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
  fNodeSubType := NodeSubType;
  fRawNodeValueInt64 := Value;
  if (NodeSubType in [nstBinary, nstRegEx]) then fRawNodeValueDefined := fRawNodeValueDefined + [nvInt64] // keep the fNodeValueStr
  else fRawNodeValueDefined := [nvInt64];
end;

{*********************************************************************************************************************************}
procedure TALJSONTextNode.SetNodeValue(const StrValue: AnsiString; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocError(cALJSONImmutable);
  fNodeSubType := NodeSubType;
  fRawNodeValueStr := StrValue;
  fRawNodeValueInt64 := Int64Value;
  fRawNodeValueDefined := [nvStr, nvInt64];
end;

{*****************************************************}
constructor TALJSONNodeList.Create(Owner: TALJSONNode);
begin
  FList:= nil;
  FCount:= 0;
  FCapacity := 0;
  FOwner := Owner;
end;

{*********************************}
destructor TALJSONNodeList.Destroy;
begin
  Clear;
  inherited;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Node is the object node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALJSONNodeList.IndexOf(const Node: TALJSONNode; const Direction: TDirection = TDirection.FromBeginning): Integer;
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
function TALJSONNodeList.IndexOf(const Name: AnsiString; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if ALNodeMatches(Get(Result), Name) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if ALNodeMatches(Get(Result), Name) then Exit;
  end;
  Result := -1;
end;

{******************************************************************************************************************************}
function TALJSONNodeList.IndexOfValue(const Value: ansiString; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).Text = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).Text = Value) then Exit;
  end;
  Result := -1;
end;

{***************************************************************************************************************************}
function TALJSONNodeList.IndexOfValue(const Value: integer; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).int32 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).int32 = Value) then Exit;
  end;
  Result := -1;
end;

{*************************************************************************************************************************}
function TALJSONNodeList.IndexOfValue(const Value: int64; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).int64 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).int64 = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************************************************************************************************}
function TALJSONNodeList.IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).float = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).float = Value) then Exit;
  end;
  Result := -1;
end;

{*****************************************************************************************************************************}
function TALJSONNodeList.IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).DateTime = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).DateTime = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************}
{Returns a specified node from the list.
 Call FindNode to access a particular node in the list.
 *NodeName is the node to access. It specifies the NodeName property of the desired node.
 FindNode returns the object of the node if it is in the list. If NodeName does not specify a node in the list,
 FindNode returns nil (Delphi) or NULL (C++).}
function TALJSONNodeList.FindNode(const NodeName: AnsiString; const Direction: TDirection = TDirection.FromBeginning): TALJSONNode;
var Index: Integer;
begin
  Index := IndexOf(NodeName, Direction);
  if Index >= 0 then Result := Get(Index)
  else Result := nil;
end;

{**********************************}
{Returns the first node in the list.
Call First to access the first node in the list. If the list is empty, First raises an exception}
function TALJSONNodeList.First: TALJSONNode;
begin
  if Count > 0 then Result := Get(0)
  else Result := nil;
end;

{*********************************}
{Returns the last node in the list.
 Call Last to access the last node in the list. If the list is empty, Last raises an exception.}
function TALJSONNodeList.Last: TALJSONNode;
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
function TALJSONNodeList.FindSibling(const Node: TALJSONNode; Delta: Integer): TALJSONNode;
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
function TALJSONNodeList.Get(Index: Integer): TALJSONNode;
begin
  if (Index < 0) or (Index >= FCount) then ALJSONDocError(CALJSONListIndexError, [Index]);
  Result := FList[Index];
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Index identify the desired node. 0 is the index of the first node,
  1 is the index of the second node, and so on}
function TALJSONNodeList.GetNodeByIndex(const Index: Integer): TALJSONNode;
begin
  Result := Get(Index);
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Name identify the desired node. it is the NodeName property of a node in the list.
 If Name does not identify a node in the list, GetNode tries to create a new node with the name specified by
 Name. If it can’t create the new node, GetNode raises an exception.}
function TALJSONNodeList.GetNodeByName(const Name: AnsiString): TALJSONNode;
begin
  Result := FindNode(Name);
  if (not Assigned(Result)) and
     (assigned(fOwner.OwnerDocument)) and
     (doNodeAutoCreate in fOwner.OwnerDocument.Options) then Result := FOwner.AddChild(Name); // only text node will be added via doNodeAutoCreate
  if not Assigned(Result) then ALJSONDocError(CALJSONNodeNotFound, [Name]);
end;

{***************************************************************************************}
procedure TALJSONNodeList.QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompare);
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

{************************************************************************}
procedure TALJSONNodeList.CustomSort(Compare: TALJSONNodeListSortCompare);
begin
  if (FList <> nil) and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

{**************************************}
{Adds a new node to the end of the list.
 Call Add to add a node to the end of the list. Add returns the index of the node once it is added, where 0 is the index
 of the first node in the list, 1 is the index of the second node, and so on.
 *Node is the node to add to the list.}
function TALJSONNodeList.Add(const Node: TALJSONNode): Integer;
begin
  Insert(-1, Node);
  Result := FCount - 1;
end;

{********************************************************}
{Inserts a new node into a specified position in the list.
 Call Insert to add a node at the position specified by Index.
 *Index specifies where to insert the node, where 0 is the first position, 1 is second position, and so on. If Index does not
  specify a valid index, Insert raises an exception.
 *Node is the node to add to the list.}
procedure TALJSONNodeList.Insert(Index: Integer; const Node: TALJSONNode);
begin
  if Index = -1 then begin
    index := FCount;
    if index = FCapacity then Grow;
  end
  else begin
    if (Index < 0) or (Index > FCount) then ALJSONDocError(CALJSONListIndexError, [Index]);
    if FCount = FCapacity then Grow;
    if Index < FCount then ALMove(FList[Index],
                                  FList[Index + 1],
                                  (FCount - Index) * SizeOf(Pointer));
  end;
  Pointer(FList[index]) := nil;
  FList[index] := Node;
  Inc(FCount);
  Node.SetParentNode(Fowner);
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Index identifies the node to remove by index rather than name. Index ranges from 0 to one less than the value of the Count property.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Index Delete returns –1.}
function TALJSONNodeList.Delete(const Index: Integer): Integer;
var Node: TALJSONNode;
begin
  Node := Get(Index);
  FList[Index] := nil; // to decrease the refcount of Node
  Dec(FCount);
  if Index < FCount then begin
    ALMove(FList[Index + 1],
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
function TALJSONNodeList.Delete(const Name: AnsiString): Integer;
begin
  result := indexOf(Name);
  if Result >= 0 then Delete(Result);
end;

{**************************************}
{Removes a specified node from the list.
 Remove removes the specified node from the list.
 *Node is the node to remove from the list.
 Remove returns the index of Node before it was removed. If node is not a node in the list, Remove returns -1.}
function TALJSONNodeList.Remove(const Node: TALJSONNode): Integer;
begin
  Result := IndexOf(Node);
  if Result >= 0 then Delete(Result);
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeList.Extract(const Node: TALJSONNode): TALJSONNode;
var I: Integer;
begin
  Result := nil;
  I := IndexOf(Node);
  if I >= 0 then result := Extract(i);
end;

{*********************************************************}
procedure TALJSONNodeList.Exchange(Index1, Index2: Integer);
var Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then ALJSONDocError(cALJSONListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then ALJSONDocError(cALJSONListIndexError, [Index2]);
  Item := pointer(FList[Index1]);
  pointer(FList[Index1]) := pointer(FList[Index2]);
  pointer(FList[Index2]) := Item;
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeList.Extract(const index: integer): TALJSONNode;
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
function TALJSONNodeList.ReplaceNode(const OldNode, NewNode: TALJSONNode): TALJSONNode;
var Index: Integer;
begin
  Index := indexOf(OldNode);
  Result := Extract(Index);
  Insert(Index, NewNode);
end;

{*******************************}
{Removes all nodes from the list.
 Call Clear to empty the list.
 Note:	Clear does not call the BeginUpdate and EndUpdate methods, even though it may result in the
 deletion of more than one node.}
procedure TALJSONNodeList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{*****************************}
procedure TALJSONNodeList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{**********************************************************}
procedure TALJSONNodeList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) then ALJSONDocError(CALJSONListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{****************************************************}
procedure TALJSONNodeList.SetCount(NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) then ALJSONDocError(CALJSONListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{****************************************************}
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           const aFormatSettings: TALFormatSettings;
                           const aPath: AnsiString;
                           const aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');

var LJsonDocument: TALJsonDocument;
    LContainChilds: boolean;
begin
  LJsonDocument := TALJsonDocument.Create(aFormatSettings);
  try

    LJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                 begin
                                   if (NodeSubType = nstBoolean)   then aLst.Add(aPath + Path + aLst.NameValueSeparator + ALBoolToStr(Args[0].VBoolean,aTrueStr,aFalseStr))
                                   else if (NodeSubType = nstnull) then aLst.Add(aPath + Path + aLst.NameValueSeparator + aNullStr)
                                   else                                 aLst.Add(aPath + Path + aLst.NameValueSeparator + ansiString(Args[0].VAnsiString));
                                   LContainChilds := True;
                                 end;

    LJsonDocument.onParseStartObject := procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
                                        begin
                                          LContainChilds := False;
                                        end;

    LJsonDocument.onParseEndObject := procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
                                      begin
                                        if (not LContainChilds) and (aPath + Path <> ''{Path = '' mean it's the root object}) then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '{}');
                                        LContainChilds := True;
                                      end;

    LJsonDocument.onParseStartArray := procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
                                       begin
                                         LContainChilds := False;
                                       end;

    LJsonDocument.onParseEndArray := procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
                                     begin
                                       if not LContainChilds then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '[]');
                                       LContainChilds := True;
                                     end;

    LJsonDocument.LoadFromJSONString(AJsonStr, true{saxMode});
  finally
    LJsonDocument.Free;
  end;
end;

{****************************************************}
Procedure ALJSONToTStrings(const AJsonStr: AnsiString;
                           const aFormatSettings: TALFormatSettings;
                           const aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');
begin
 ALJSONToTStrings(AJsonStr,
                  aFormatSettings,
                  '',
                  aLst,
                  aNullStr,
                  aTrueStr,
                  aFalseStr);
end;

{******************************************************}
Procedure ALJSONToTStrings(const aJsonNode: TAlJsonNode;
                           Const aPath: AnsiString;
                           const aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');
var LTmpPath: AnsiString;
    I: integer;
begin
  if aJsonNode.ChildNodes.Count > 0 then begin
    for I := 0 to aJsonNode.ChildNodes.Count - 1 do begin

      if aJsonNode.NodeType = ntArray then LTmpPath := aPath + '[' + alinttostr(I) + ']'
      else begin
        if aJsonNode.ChildNodes[I].NodeName = '' then raise Exception.Create('Nodename can not be empty');
        LTmpPath := aPath + alIfThen(aPath <> '', '.', '') + aJsonNode.ChildNodes[I].NodeName;
      end;

      case aJsonNode.ChildNodes[I].NodeType of

        ntObject: ALJSONToTStrings(aJsonNode.ChildNodes[I],
                                   LTmpPath,
                                   aLst,
                                   aNullStr,
                                   aTrueStr,
                                   aFalseStr);

        ntArray: ALJSONToTStrings(aJsonNode.ChildNodes[I],
                                  LTmpPath,
                                  aLst,
                                  aNullStr,
                                  aTrueStr,
                                  aFalseStr);

        ntText: begin
                  if (aJsonNode.ChildNodes[I].NodeSubType = nstBoolean) then   aLst.Add(LTmpPath + aLst.NameValueSeparator + ALBoolToStr(aJsonNode.ChildNodes[I].Bool,aTrueStr,aFalseStr))
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

{******************************************************}
Procedure ALJSONToTStrings(const aJsonNode: TAlJsonNode;
                           const aLst: TALStrings;
                           Const aNullStr: AnsiString = 'null';
                           Const aTrueStr: AnsiString = 'true';
                           Const aFalseStr: AnsiString = 'false');
begin
  ALJSONToTStrings(aJsonNode,
                   '',
                   aLst,
                   aNullStr,
                   aTrueStr,
                   aFalseStr)
end;

{************************************************}
procedure ALTStringsToJson(const aLst: TALStrings;
                           const aJsonNode: TALJSONNode;
                           Const aPath: AnsiString = '';
                           Const aNameToLowerCase: boolean = false;
                           Const aNullStr: AnsiString = 'null');

var LIndex: Integer;
    LNames:  TALStringList;
    LLowerName: AnsiString;
    LCurrJsonNode, LTmpJsonNode: TALJSONNode;
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
  LNames := TALStringList.Create;
  try

    //init aNames.linebreak
    LNames.LineBreak := '.';

    // scroll the aLst
    for I := 0 to aLst.Count - 1 do begin

      //if it's contain path
      if (aPath = '') or
         (alposExIgnoreCase(aPath + '.',aLst.Names[I]) = 1) then begin

        // path.aggregated_data.properties.types[3].translations.usa =>
        //   aggregated_data
        //   properties
        //   types
        //   [3]
        //   translations
        //   usa
        if (aPath <> '') then LNames.Text := ALStringReplace(ALStringReplace(aLst.Names[I],
                                                                             aPath + '.',
                                                                             '',
                                                                             [rfIgnoreCase]),
                                                             '[',
                                                             '.[',
                                                             [rfReplaceAll])
        else LNames.Text := ALStringReplace(aLst.Names[I],
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
            LLowerName := alifThen(aNameToLowerCase, allowercase(LNames[J]), LNames[J]);
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
    LNames.Free;
  end;

end;

{*************************************************}
Procedure ALJSONToXML(const aJSONNode: TALJsonNode;
                      const aXMLNode: TALXmlNode;
                      const aXMLElementNameForJSONArrayEntries: TalStrings; // JSONArrayNodeName=XMLElementName | ex: transactions=transaction
                                                                            //                                  |     features=feature
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
    else ALJsonToXML(aJSONNode.ChildNodes[I], aXMLNode.AddChild(LNodeName));

  end;
end;

{*************************************************}
Procedure ALJSONToXML(const aJSONNode: TALJsonNode;
                      const aXMLNode: TALXmlNode;
                      const aDefaultXMLElementNameForJSONArrayEntries: AnsiString = 'rec');
begin
  ALJSONToXML(aJSONNode,
              aXMLNode,
              nil,
              aDefaultXMLElementNameForJSONArrayEntries);
end;

{********************************************************************************}
function ALJsonEncodeFloatWithNodeSubTypeHelper(const aValue: double): AnsiString;
begin
  result := ALFloatToStr(aValue, ALDefaultFormatSettings);
end;

{***********************************************************************************}
function ALJsonEncodeTextWithNodeSubTypeHelper(const aValue: AnsiString): AnsiString;
begin
  result := '"'+ALJavascriptEncode(aValue)+'"';
end;

{*************************************************************************************}
function ALJsonEncodeBinaryWithNodeSubTypeHelper(const aValue: AnsiString): AnsiString;
begin
  result := 'BinData(0, "' + ALBase64EncodeString(aValue) + '")';
end;

{***************************************************************************************}
function ALJsonEncodeObjectIDWithNodeSubTypeHelper(const aValue: AnsiString): AnsiString;
begin
  result := 'ObjectId("'+albintohex(aValue)+'")';
end;

{***********************************************************************************}
function ALJsonEncodeBooleanWithNodeSubTypeHelper(const aValue: Boolean): AnsiString;
begin
  if aValue then result := 'true'
  else result := 'false';
end;

{**************************************************************************************}
function ALJsonEncodeDateTimeWithNodeSubTypeHelper(const aValue: TdateTime): AnsiString;
begin
  result := ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', aValue, ALDefaultFormatSettings);
end;

{*****************************************************************************************}
function ALJsonEncodeJavascriptWithNodeSubTypeHelper(const aValue: AnsiString): AnsiString;
begin
  result := aValue;
end;

{*******************************************************************************}
function ALJsonEncodeInt64WithNodeSubTypeHelper(const aValue: int64): AnsiString;
begin
  result := 'NumberLong(' + ALIntToStr(aValue) + ')';
end;

{*******************************************************************************}
function ALJsonEncodeInt32WithNodeSubTypeHelper(const aValue: int32): AnsiString;
begin
  result := 'NumberInt(' + ALIntToStr(aValue) + ')';
end;

{*********************************************************}
function ALJsonEncodeNullWithNodeSubTypeHelper: AnsiString;
begin
  result := 'null';
end;

{******************************************************************}
function ALJsonEncodeWithNodeSubTypeHelper(const aValue: AnsiString;
                                           const aNodeSubType: TALJSONNodeSubType;
                                           const aFormatSettings: TALFormatSettings): AnsiString;
begin
  case aNodeSubType of
    nstFloat:      begin
                     if @aFormatSettings <> @ALDefaultFormatSettings then result := ALJsonEncodeFloatWithNodeSubTypeHelper(ALStrToFloat(aValue, aFormatSettings))
                     else result := aValue;
                   end;
    nstText:       result := ALJsonEncodeTextWithNodeSubTypeHelper(aValue);
    nstBinary:     result := ALJsonEncodeBinaryWithNodeSubTypeHelper(aValue);
    nstObjectID:   result := ALJsonEncodeObjectIDWithNodeSubTypeHelper(aValue);
    nstBoolean:    result := ALJsonEncodeBooleanWithNodeSubTypeHelper(ALStrToBool(aValue));
    nstDateTime:   begin
                     if aValue = 'NOW' then result := ALJsonEncodeDateTimeWithNodeSubTypeHelper(ALUtcNow)
                     else result := ALJsonEncodeDateTimeWithNodeSubTypeHelper(ALStrToDateTime(aValue, aFormatSettings));
                   end;
    nstJavascript: result := ALJsonEncodeJavascriptWithNodeSubTypeHelper(aValue);
    nstInt32:      result := ALJsonEncodeInt32WithNodeSubTypeHelper(ALstrToInt(aValue));
    nstInt64:      result := ALJsonEncodeInt64WithNodeSubTypeHelper(ALstrToInt64(aValue));
    nstNull:       result := ALJsonEncodeNullWithNodeSubTypeHelper;
    nstObject:     raise Exception.Create('Unsupported Node SubType');
    nstArray:      raise Exception.Create('Unsupported Node SubType');
    nstRegEx:      raise Exception.Create('Unsupported Node SubType');
    nstTimestamp:  raise Exception.Create('Unsupported Node SubType');
    else raise Exception.Create('Unknown Node SubType');
  end;
end;

{$ENDIF !ALHideAnsiString}

{************************************************************************}
Function ALFindJsonNodeByInt32ChildNodeValueU(const JsonNode:TalJsonNodeU;
                                              Const ChildNodeName: String;
                                              Const ChildNodeValue : Int32;
                                              Const Recurse: Boolean = False): TalJsonNodeU;
var I, J : integer;
Begin
  result := nil;
  if not (JsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for I := 0 to JsonNode.ChildNodes.Count - 1 do begin
    for J := 0 to JsonNode.ChildNodes[I].ChildNodes.Count - 1 do begin
      If (JsonNode.ChildNodes[I].ChildNodes[j].NodeType = nttext) and
         (JsonNode.ChildNodes[I].ChildNodes[j].NodesubType = nstint32) and
         (ALSametextU(JsonNode.ChildNodes[I].ChildNodes[j].NodeName, ChildNodeName)) and
         (JsonNode.ChildNodes[I].ChildNodes[j].int32 = ChildNodeValue) then begin
        result := JsonNode.ChildNodes[I];
        exit;
      end;
    end;
    if Recurse then begin
      result := ALFindJsonNodeByInt32ChildNodeValueU(JsonNode.ChildNodes[I],
                                                     ChildNodeName,
                                                     ChildNodeValue,
                                                     Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{***********************************************************************}
Function ALFindJsonNodeByTextChildNodeValueU(const JsonNode:TalJsonNodeU;
                                             Const ChildNodeName: String;
                                             Const ChildNodeValue : String;
                                             Const Recurse: Boolean = False): TALJsonNodeU;
var I, J : integer;
Begin
  result := nil;
  if not (JsonNode.NodeType in [ntObject, ntArray]) then Exit;
  for I := 0 to JsonNode.ChildNodes.Count - 1 do begin
    for J := 0 to JsonNode.ChildNodes[I].ChildNodes.Count - 1 do begin
      If (JsonNode.ChildNodes[I].ChildNodes[j].NodeType = nttext) and
         (JsonNode.ChildNodes[I].ChildNodes[j].NodesubType = nstText) and
         (ALSametextU(JsonNode.ChildNodes[I].ChildNodes[j].NodeName, ChildNodeName)) and
         (JsonNode.ChildNodes[I].ChildNodes[j].text = ChildNodeValue) then begin
        result := JsonNode.ChildNodes[I];
        exit;
      end;
    end;
    if Recurse then begin
      result := ALFindJsonNodeByTextChildNodeValueU(JsonNode.ChildNodes[I],
                                                    ChildNodeName,
                                                    ChildNodeValue,
                                                    Recurse);
      if assigned(Result) then break;
    end;
  end;
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
function ALJSONTryStrToRegExU(const S: String; out RegEx: String; out RegExOptions: TALPerlRegExOptions): boolean;
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

    P1 := ALLastDelimiterU('/', S);
    if P1 <> 1 then begin

      //init Value
      RegEx := ALCopyStrU(S, 2, P1 - 2);
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
      //  aRegEx.Free;
      //end;

    end;

  end;

end;
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrTobinaryU(const S: String; out Data: String; out Subtype: byte): boolean;
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
  if not ALTryStrToIntU(ALCopyStrU(S,P1,P2-P1), LInt) then Exit;
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
  Data := ALCopyStrU(s, P1, P2-P1); // notmally i would like to do ALBase64DecodeStringU()
                                    // and return in data the byte string but this is not possible
                                    // because the source byte array is probably not a multiple of 2
                                    // and unicode string is obligatory a multiple of 2

  // set the result
  result := true;

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToDateTimeU(const S: String; out Value: TDateTime): Boolean;
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
  if alposU('new', s) = 1 then P1 := 4{length('new') + 1} // new  Date ( 'yyyy-mm-ddThh:nn:ss.zzzZ' )
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
  LTmpStr := ALCopyStrU(S,P2,P1-P2); // yyyy-mm-ddThh:nn:ss.zzz

  P2 := 1;
  LTmpLn := length(LTmpStr);
  while (P2 <= LTmpLn) and (LTmpStr[P2] <> 'T') do inc(P2);
  if P2 > LTmpLn then exit;
  LTmpStr[P2] := ' '; // yyyy-mm-dd hh:nn:ss.zzz

  result := ALTryStrToDateTimeU(LTmpStr, Value, vALJsonISODateFormatSettingsU);
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
{$IFEND}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
// ObjectId is a 12-byte BSON type, constructed using:
// a 4-byte value representing the seconds since the Unix epoch,
// a 3-byte machine identifier,
// a 2-byte process id, and
// a 3-byte counter, starting with a random value.
function ALJSONTryStrToObjectIDU(const S: String; out Value: String): Boolean;
var LBinValue: Tbytes;
    LQuoteChar: Char;
    P1: integer;
    Ln: integer;
begin

  // s must look like
  // ObjectId ( "507f1f77bcf86cd799439011" )
  result := false;
  if alposU('ObjectId', S) <> 1 then exit;
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
  Value := ALCopyStrU(S,P1,24{length(aObjectIDhex)}); // 507f1f77bcf86cd799439011
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
  result := ALTryHexToBinU(Value, LBinValue) and
            (length(LBinValue) = 12);

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToTimestampU(const S: String; out Value: TALBSONTimestamp): Boolean;
var P1, P2: integer;
    LArgs: String;
    LArg1: integer;
    LArg2: integer;
    Ln: integer;
begin

  // s must look like
  // Timestamp(0, 0)
  result        := false;
  if alposU('Timestamp', S) <> 1 then Exit;
  Ln := length(s);
  P1 := 10{Length('Timestamp') + 1}; // Timestamp(0, 0)
                                     //          ^
  while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
  if (P1 > ln) or (S[P1] <> '(') then exit; // Timestamp(0, 0)
                                            //          ^P1
  P2 := ALPosExU(')', S, P1);
  if P2 <> ln then exit; // Timestamp(0, 0)
                         //               ^P2
  LArgs := ALCopyStrU(S, P1+1, P2 - P1-1); // 0, 0

  // take arguments of function Timestamp
  P1 := alposU(',', LArgs);
  if not ALTryStrToIntU(ALTrimU(ALCopyStrU(LArgs, 1,      P1 - 1)), LArg1) then Exit;
  if not ALTryStrToIntU(ALTrimU(ALCopyStrU(LArgs, P1 + 1, maxint)), LArg2) then Exit;

  // build result
  result := true;
  Value.W1 := LArg1; // higher 4 bytes - increment
  Value.W2 := LArg2; // lower  4 bytes - timestamp

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToInt32U(const S: String; out Value: integer): Boolean;
var LTmpStr: String;
    LQuoteChar: Char;
    P1, P2: integer;
    Ln: integer;
begin

  // s must look like
  // NumberInt ( "12391293" )
  // NumberInt ( 12391293 )
  // 12391293
  result := ALTryStrToIntU(S, Value);
  if result then exit;
  if alposU('NumberInt', S) <> 1 then exit;
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
    LTmpStr := ALCopyStrU(S,P1,P2-P1); // 12391293
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
    LTmpStr := ALCopyStrU(S,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberInt ( "12391293" )
                  //                       ^P1
    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberInt ( "12391293" )
                                               //                        ^P1
  end;

  //convert 12391293 to integer
  result := ALTryStrToIntU(LTmpStr, Value);

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
function ALJSONTryStrToInt64U(const S: String; out Value: int64): Boolean;
var LTmpStr: String;
    LQuoteChar: Char;
    P1, P2: integer;
    Ln: integer;
begin

  // s must look like
  // NumberLong ( "12391293" )
  // NumberLong ( 12391293 )
  // 12391293
  result := ALTryStrToInt64U(S, Value);
  if result then exit;
  if alposU('NumberLong', S) <> 1 then exit;
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
    LTmpStr := ALCopyStrU(S,P1,P2-P1); // 12391293
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
    LTmpStr := ALCopyStrU(S,P1,P2-P1); // 12391293
    P1 := P2 + 1; // NumberLong ( "12391293" )
                  //                        ^P1
    while (P1 <= ln) and (S[P1] in [#9, ' ']) do inc(P1);
    if (P1 <> ln) or (S[P1] <> ')') then exit; // NumberLong ( "12391293" )
                                               //                         ^P1
  end;

  //convert 12391293 to integer
  result := ALTryStrToInt64U(LTmpStr, Value);

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{*******************************************}
procedure ALJSONDocErrorU(const Msg: String); overload;
begin
  raise EALJSONDocError.Create(Msg);
end;

{***********************************************************************}
procedure ALJSONDocErrorU(const Msg: String; const Args: array of const); overload;
begin
  raise EALJSONDocError.CreateFmt(Msg, Args);
end;

{****************************************************************************}
procedure ALJSONDocErrorU(const Msg: String; const NodeType: TalJsonNodeType); overload;
begin
  case NodeType of
    ntObject: ALJSONDocErrorU(Msg, ['ntObject']);
    ntArray: ALJSONDocErrorU(Msg, ['ntArray']);
    ntText: ALJSONDocErrorU(Msg, ['ntText']);
    else AlJSONDocErrorU(cAlJSONInvalidNodeType);
  end;
end;

{*********************************************************************************}
function ALNodeMatchesU(const Node: TALJSONNodeU; const NodeName: String): Boolean;
begin
  Result := (Node.NodeName = NodeName);
end;

{********************************************************************************************}
{Call CreateNode to create a new generic JSON node. The resulting node does not have a parent,
 but can be added to the ChildNodes list of any node in the document.}
function ALCreateJSONNodeU(const NodeName: String; NodeType: TALJSONNodeType): TALJSONNodeU;
begin
  case NodeType of
    ntObject: Result := TALJSONObjectNodeU.Create(NodeName);
    ntArray: Result := TALJSONArrayNodeU.Create(NodeName);
    ntText: Result := TALJSONTextNodeU.Create(NodeName);
    else begin
      Result := nil; //for hide warning
      AlJSONDocErrorU(cAlJSONInvalidNodeType);
    end;
  end;
end;

{*****************************************************************}
constructor TALJSONDocumentU.create(const aActive: Boolean = True);
begin
  inherited create;
  FDocumentNode:= nil;
  FParseOptions:= [];
  fPathSeparator := '.';
  FOnParseStartDocument := nil;
  FOnParseEndDocument := nil;
  FonParseText := nil;
  FonParseStartObject := nil;
  FonParseEndObject := nil;
  FonParseStartArray := nil;
  FonParseEndArray := nil;
  FOptions := [];
  NodeIndentStr := vALDefaultNodeIndentU;
  fFormatSettings := @ALDefaultFormatSettingsU;
  FTag := 0;
  SetActive(aActive);
end;

{************************************************************************************************************}
constructor TALJSONDocumentU.Create(const aFormatSettings: TALformatSettingsU; const aActive: Boolean = True);
begin
  create(aActive);
  if @aFormatSettings <> @ALDefaultFormatSettingsU then begin
    new(fFormatSettings);
    fFormatSettings^ := aFormatSettings;
  end;
end;

{**********************************}
destructor TALJSONDocumentU.Destroy;
begin
  if fFormatSettings <> @ALDefaultFormatSettingsU then dispose(fFormatSettings);
  Options := Options - [doImmutable];
  ReleaseDoc;
  inherited;
end;

{********************************************}
procedure TALJSONDocumentU.MultiThreadPrepare;
begin
  node.MultiThreadPrepare;
end;

{*******************************}
procedure TALJSONDocumentU.Clear;
begin
  releaseDoc;
  Active := true;
end;

{****************************************}
{Returns the value of the Active property.
 GetActive is the read implementation of the Active property.}
function TALJSONDocumentU.GetActive: Boolean;
begin
  Result := Assigned(FDocumentNode);
end;

{*************************************}
{Sets the value of the Active property.
 SetActive is the write implementation of the Active property.
 *Value is the new value to set.}
procedure TALJSONDocumentU.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then begin
    if Value then begin
      if (doImmutable in Options) then ALJSONDocErrorU(cALJSONImmutable);
      FDocumentNode := TALJSONObjectNodeU.Create;
      FDocumentNode.SetOwnerDocument(Self);
    end
    else ReleaseDoc;
  end;
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
Procedure TALJSONDocumentU.ParseJSON(const Buffer: String;
                                     const ContainerNode: TALJSONNodeU);

Var BufferLength: Integer;
    BufferPos: Integer;
    CurrName: String;
    CurrIndex: integer;
    CurrValue: String;
    NotSaxMode: Boolean;
    WorkingNode: TALJSONNodeU;
    NamePaths: TALNvStringListU;
    ObjectPaths: TALIntegerList;
    DecodeJSONReferences: boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(Const ExtraItems: String = ''): String;
  var I, L, P, Size: Integer;
      LB: Char;
      S: String;
  begin
    LB := PathSeparator;
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
         (((NotSaxMode) and (TALJSONNodeU(NamePaths.Objects[I]).nodetype <> ntarray)) or
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(const index: integer;
                                  const Args: array of const;
                                  const NodeSubType: TALJSONNodeSubType);
  begin
    DoParseText(GetPathStr('[' + alinttostrU(index) + ']'), '', Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithName(const name: String;
                                 const Args: array of const;
                                 const NodeSubType: TALJSONNodeSubType);
  begin
    DoParseText(GetPathStr(Name), Name, Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseText(const Index: integer;
                         const Name: String;
                         const Args: array of const;
                         const NodeSubType: TALJSONNodeSubType);
  begin
    if Assigned(fonParseText) then begin
      if notSaxMode then begin
        if WorkingNode.nodetype=ntarray then _DoParseTextWithIndex(Index, Args, NodeSubType)
        else _DoParseTextWithName(Name, Args, NodeSubType);
      end
      else begin
        if NamePaths.Count = 0 then ALJSONDocErrorU(CALJSONParseError);
        if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntArray then _DoParseTextWithIndex(Index, Args, NodeSubType)
        else _DoParseTextWithName(Name, Args, NodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartObject(const Name: String);
  begin
    DoParseStartObject(GetPathStr, Name);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndObject;
  begin
    if NamePaths.Count = 0 then ALJSONDocErrorU(CALJSONParseError);
    DoParseEndObject(GetPathStr, NamePaths.Names[NamePaths.Count - 1])
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: String);
  begin
    DoParseStartArray(GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if NamePaths.Count = 0 then ALJSONDocErrorU(CALJSONParseError);
    DoParseEndArray(GetPathStr, NamePaths.Names[NamePaths.Count - 1]);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AddIndexItemToNamePath(const index: integer; Obj: Pointer);
  var S1: String;
  begin
    setlength(S1,sizeOf(Integer) div sizeOF(Char)); // off course sizeOf(Integer) must be a multiple of sizeOf(char) but it's always the case
    ALmove(index, pointer(S1)^, sizeOf(Integer));
    NamePaths.AddNameValueObject('[' + alinttostrU(Index) + ']', S1, Obj)
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
      if NamePaths.Count = 0 then ALJSONDocErrorU(CALJSONParseError);
      if TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(Index, Obj)
      else _AddNameItemToNamePath(name, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _createInt64Node(index: integer; const name: String; const value: String): boolean;
  var LNode: TALJsonNodeU;
      LInt64: Int64;
  begin
    if ALJSONTryStrToInt64U(value, LInt64) then begin
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
  var LNode: TALJsonNodeU;
      LInt32: Int32;
  begin
    if ALJSONTryStrToInt32U(value, LInt32) then begin
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
  var LNode: TALJsonNodeU;
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
  var LNode: TALJsonNodeU;
      LDouble: Double;
  begin
    if ALTryStrToFloatU(value, LDouble, ALDefaultFormatSettingsU) then begin
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
  var LNode: TALJsonNodeU;
      LBinSubtype: byte;
      LBinData: String;
  begin
    if ALJSONTryStrToBinaryU(value, LBinData, LBinSubtype) then begin
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
  var LNode: TALJsonNodeU;
      LObjectID: String;
  begin
    if ALJSONTryStrToObjectIDU(value, LObjectID) then begin
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
  var LNode: TALJsonNodeU;
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
  var LNode: TALJsonNodeU;
      LDateTime: TdateTime;
  begin
    if ALJSONTryStrToDateTimeU(value, LDateTime) then begin
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
  var LNode: TALJsonNodeU;
      LTimestamp: TALBSONTimestamp;
  begin
    if ALJSONTryStrToTimestampU(value, LTimestamp) then begin
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
  var LNode: TALJsonNodeU;
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
  var LNode: TALJsonNodeU;
      LRegEx: String;
      LRegExOptions: TALPerlRegExOptions;
  begin
    if ALJSONTryStrToRegExU(value, LRegEx, LRegExOptions) then begin
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
  var LNode: TALJsonNodeU;
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
    if NamePaths.Count = 0 then ALJSONDocErrorU(CALJSONParseError);
    ALMove(pointer(namePaths.ValueFromIndex[namepaths.Count - 1])^,result,sizeOf(integer));
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  procedure AnalyzeNode;
  Var LNode: TALJsonNodeU;
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
        if (ObjectPaths.Count = 0) then ALJSONDocErrorU(cALJSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then ALJSONDocErrorU(cALJSONParseError);
      end;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        if assigned(ObjectPaths) then LNode := TALJSONNodeU(ObjectPaths.Objects[ObjectPaths.Count - 1])
        else LNode := TALJSONNodeU(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (LNode <> WorkingNode) then ALJSONDocErrorU(CALJSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then ALJSONDocErrorU(cALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then ALJSONDocErrorU(CALJSONParseError);

        //if working node <> containernode then we can go to one level up
        If WorkingNode<>ContainerNode then begin

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

        //if working node = containernode then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

         //calculate anodeTypeInt
        LNodeType := TALJSONNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then ALJSONDocErrorU(cALJSONParseError);

        //check that the end object/array correspond to the aNodeType
        if ((c = '}') and
            (LNodeType <> ntObject)) or
           ((c = ']') and
            (LNodeType <> ntarray)) then ALJSONDocErrorU(CALJSONParseError);

        //update CurrIndex if WorkingNode.NodeType = ntArray
        if (Namepaths.Count >= 2) and
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 2]) = ntarray) then CurrIndex := _extractLastIndexFromNamePath + 1;

      end;

      //call the DoParseEndObject/array event
      if Assigned(fonParseEndObject) then begin
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

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocErrorU(CALJSONParseError);

        //Node without name can be ONLY present inside an array node
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then ALJSONDocErrorU(CALJSONParseError);

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
             (TALJsonNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then ALJSONDocErrorU(CALJSONParseError);

        //update the path
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(CurrIndex, '', pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event
      if c = '{' then begin
        if Assigned(fonParseStartObject) then _DoParseStartObject('');
        CurrIndex := -1;
      end
      else begin
        if Assigned(fonParseStartArray) then _DoParseStartArray('');
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
         ALCopyStrU(Buffer,CurrName,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALJavascriptDecodeVU(CurrName); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorU(CALJSONParseError);
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
      if BufferPos > BufferLength then ALJSONDocErrorU(CALJSONParseError);

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
          ALCopyStrU(Buffer,CurrName,BufferPos,P2-BufferPos+1); // new Date('Dec 03, 1924')
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
      if P1 > BufferLength then ALJSONDocErrorU(CALJSONParseError);
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
    if BufferPos > BufferLength then ALJSONDocErrorU(CALJSONParseError);  // .... : ....
                                                                         //      ^BufferPos
    {$ENDREGION}

    {$REGION 'if aNameValueSeparator is absent then it is just a value'}
    if LNameValueSeparator <> ':' then begin

      //Node without name can be ONLY present inside an array node
      if NotSaxMode then begin
        if not assigned(WorkingNode) then ALJSONDocErrorU(CALJSONParseError);
        if (CurrIndex < 0)  or
           (WorkingNode.nodetype <> ntarray) then ALJSONDocErrorU(CALJSONParseError);
      end
      else begin
        if (CurrIndex < 0) or
           (NamePaths.Count = 0) or
           (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntarray) then ALJSONDocErrorU(CALJSONParseError);
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
    if BufferPos > BufferLength then ALJSONDocErrorU(CALJSONParseError); // .... " ....
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

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocErrorU(CALJSONParseError);

        //Node withe name MUST be ONLY present inside an object node
        if (CurrIndex >= 0)  or
           (WorkingNode.nodetype <> ntObject) then ALJSONDocErrorU(CALJSONParseError);

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
           (TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]) <> ntobject) then ALJSONDocErrorU(CALJSONParseError);

        //update the path
        if c = '{' then LNodeType := ntObject
        else LNodeType := ntArray;
        _AddItemToNamePath(-1, CurrName, pointer(LNodeType));

      end;

      //call the DoParseStartObject/array event and update the CurrIndex if it's an array
      if c = '{' then begin
        if Assigned(fonParseStartObject) then _DoParseStartObject(CurrName)
      end
      else begin
        if Assigned(fonParseStartArray) then _DoParseStartArray(CurrName);
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
         ALCopyStrU(Buffer,currValue,BufferPos + 1,P1-BufferPos - 1);
         if DecodeJSONReferences then ALJavascriptDecodeVU(currValue); // ..."...
         break;
       end
       else inc(P1); // ... "...\"..."
                     //      ^^^^^^^^^P1

      end;
      if P1 > BufferLength then ALJSONDocErrorU(CALJSONParseError);
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
      if BufferPos > BufferLength then ALJSONDocErrorU(CALJSONParseError);

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
          ALCopyStrU(Buffer,currValue,BufferPos,P2-BufferPos+1); // new Date('Dec 03, 1924')
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
      if P1 > BufferLength then ALJSONDocErrorU(CALJSONParseError);
      BufferPos := P1; // ... new Date('Dec 03, 1924'), ....
                       //                             ^BufferPos


    end;
    {$ENDREGION}

    {$REGION 'create the named text node'}

    //Node withe name MUST be ONLY present inside an object node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(CALJSONParseError);
      if (CurrIndex >= 0)  or
         (WorkingNode.nodetype <> ntObject) then ALJSONDocErrorU(CALJSONParseError);
    end
    else begin
      if (CurrIndex >= 0) or
         (NamePaths.Count = 0) or
         (TALJSONNodeType(NamePaths.Objects[Namepaths.Count - 1]) <> ntObject) then ALJSONDocErrorU(CALJSONParseError);
    end;

    //create the node
    _createNode(currIndex,CurrName,CurrValue,LQuoteChar in ['"','''']);

    {$ENDREGION}

  end;

var InCommentLine: integer;
    c: Char;

Begin

  //
  // NOTE: the childNodes of the ContainerNode
  //       must have been cleared by the calling function!
  //
  // NOTE: ContainerNode must have fDocument assigned
  //
  // NOTE: ContainerNode must be ntobject or nil (sax mode)
  //

  //error if Immutable
  if (doImmutable in Options) then ALJSONDocErrorU(cALJSONImmutable);

  //event fonParseStartDocument
  DoParseStartDocument;

  //init WorkingNode and NotSaxMode, CurrIndex and DecodeJSONReferences
  WorkingNode := ContainerNode;
  NotSaxMode := assigned(ContainerNode);
  DecodeJSONReferences := not (poIgnoreControlCharacters in ParseOptions);
  CurrIndex := -1;

  //init ObjectPaths or NamePaths
  if (NotSaxMode) and
     (not assigned(fonParseText)) and
     (not assigned(FonParseStartObject)) and
     (not assigned(FonParseEndObject)) and
     (not assigned(FonParseStartArray)) and
     (not assigned(FonParseEndArray)) then begin
    ObjectPaths := TALIntegerList.Create(false{OwnsObjects});
    NamePaths := nil;
  end
  else begin
    ObjectPaths := nil;
    NamePaths := TALNvStringListU.Create;
  end;
  Try

    //init Buffer
    BufferLength := length(Buffer);
    BufferPos := 1;

    //add first node in ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.AddObject(-1, WorkingNode)
    else begin
      if NotSaxMode then _AddNameItemToNamePath('', WorkingNode)
      else _AddNameItemToNamePath('', pointer(ntObject));
    end;

    //skip the first {
    While (BufferPos <= BufferLength) do begin
      c := Buffer[BufferPos];
      If c <= ' ' then inc(bufferPos)
      else begin
        if c <> '{' then ALJSONDocErrorU(cALJSONParseError);
        inc(bufferPos);
        break;
      end;
    end;

    //analyze all the nodes
    if poAllowComments in ParseOptions then begin
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
      if ObjectPaths.Count > 0 then ALJSONDocErrorU(cALJSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then ALJSONDocErrorU(cALJSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if WorkingNode <> nil then ALJSONDocErrorU(cALJSONParseError);

    //event fonParseEndDocument
    DoParseEndDocument;

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ALFreeAndNil(ObjectPaths)
    else ALFreeAndNil(NamePaths);

  end;

end;
{$WARN WIDECHAR_REDUCED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{*************************************************************}
{Last version of the spec: http://bsonspec.org/#/specification}
procedure TALJSONDocumentU.ParseBSON(const Buffer: Tbytes;
                                     const ContainerNode: TALJsonNodeU);

Var BufferLength: Integer;
    BufferPos: Integer;
    CurrName: String;
    NotSaxMode: Boolean;
    WorkingNode: TALJsonNodeU;
    NamePaths: TALStringListU;
    ObjectPaths: TObjectList<TALJsonNodeU>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetPathStr(Const ExtraItems: String = ''): String;
  var I, L, P, Size: Integer;
      LB: Char;
      S: String;
  begin
    LB := PathSeparator;
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
         (((NotSaxMode) and (TALJsonNodeU(NamePaths.Objects[I]).nodetype <> ntarray)) or
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithIndex(const index: String;
                                  const Args: array of const;
                                  const NodeSubType: TALJsonNodeSubType);
  begin
    DoParseText(GetPathStr('[' + index + ']'), '', Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseTextWithName(const name: String;
                                 const Args: array of const;
                                 const NodeSubType: TALJsonNodeSubType);
  begin
    DoParseText(GetPathStr(Name), Name, Args, NodeSubType)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseText(const NameOrIndex: String;
                         const Args: array of const;
                         const NodeSubType: TALJsonNodeSubType);
  begin
    if Assigned(fonParseText) then begin
      if notSaxMode then begin
        if WorkingNode.nodetype=ntarray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
      end
      else begin
        if NamePaths.Count = 0 then ALJSONDocErrorU(CALJSONParseError);
        if TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntArray then _DoParseTextWithIndex(NameOrIndex, Args, NodeSubType)
        else _DoParseTextWithName(NameOrIndex, Args, NodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartObject(const Name: String);
  begin
    DoParseStartObject(GetPathStr, Name);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndObject;
  begin
    if NamePaths.Count = 0 then ALJSONDocErrorU(CALJSONParseError);
    DoParseEndObject(GetPathStr, NamePaths[NamePaths.Count - 1])
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseStartArray(const index: String);
  begin
    DoParseStartArray(GetPathStr, index)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoParseEndArray;
  begin
    if NamePaths.Count = 0 then ALJSONDocErrorU(CALJSONParseError);
    DoParseEndArray(GetPathStr, NamePaths[NamePaths.Count - 1]);
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
      if NamePaths.Count = 0 then ALJSONDocErrorU(CALJSONParseError);
      if TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]) = ntarray then _AddIndexItemToNamePath(nameOrIndex, Obj)
      else _AddNameItemToNamePath(nameOrIndex, Obj);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt64Node(const name: String;
                             const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LInt64: Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) then ALJSONDocErrorU(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt64, sizeof(LInt64));
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createInt32Node(const name: String;
                             const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LInt32: Int32;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) then ALJSONDocErrorU(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTextNode(const name: String;
                            const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LInt32: Int32;
      LText: String;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) then ALJSONDocErrorU(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    if (BufferPos + LInt32 > BufferLength) then ALJSONDocErrorU(cALBSONParseError);
    LText := Tencoding.UTF8.GetString(Buffer,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createFloatNode(const name: String;
                             const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LDouble: Double;
  begin
    if BufferPos > BufferLength - sizeof(Double) then ALJSONDocErrorU(cALBSONParseError);
    ALMove(Buffer[BufferPos], LDouble, sizeof(Double));
    BufferPos := BufferPos + sizeof(Double);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBinaryNode(const name: String;
                              const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LInt32: Int32;
      LBinSubtype: byte;
      LBinData: Tbytes;
      LBase64Data: String;
  begin
    //Get size
    if BufferPos > BufferLength - sizeof(LInt32) then ALJSONDocErrorU(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);

    //Get the subtype
    if BufferPos >= BufferLength then ALJSONDocErrorU(cALBSONParseError);
    LBinSubtype := Buffer[BufferPos];
    BufferPos := BufferPos + 1;

    //Get the data
    if (BufferPos + LInt32 > BufferLength) then ALJSONDocErrorU(cALBSONParseError);
    setlength(LBinData, LInt32);
    ALMove(Buffer[BufferPos], pointer(LBinData)^, LInt32);
    LBase64Data := ALBase64EncodeBytesU(LBinData);
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createObjectIDNode(const name: String;
                                const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LObjectID: Tbytes;
      LHexData: String;
  begin
    if BufferPos > BufferLength - 12{length(aObjectID)} then ALJSONDocErrorU(cALBSONParseError);
    setlength(LObjectID, 12);
    ALMove(Buffer[BufferPos], pointer(LObjectID)^, 12{length(aObjectID)});
    LHexData := ALBinToHexU(LObjectID);
    BufferPos := BufferPos + 12{length(aObjectID)};

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createBooleanNode(const name: String;
                               const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LBool: Boolean;
  begin
    if BufferPos >= BufferLength then ALJSONDocErrorU(cALBSONParseError);
    if Buffer[BufferPos] = $00 then LBool := False
    else if Buffer[BufferPos] = $01 then LBool := true
    else begin
      ALJSONDocErrorU(cALBSONParseError);
      LBool := False; // to hide a warning;
    end;
    BufferPos := BufferPos + 1;

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createDateTimeNode(const name: String;
                                const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LDateTime: TdateTime;
      LInt64: Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) then ALJSONDocErrorU(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt64, sizeof(LInt64));
    LDateTime := ALUnixMsToDateTime(LInt64);
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createTimestampNode(const name: String;
                                 const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LTimestamp: TALBSONTimestamp;
      LInt64: Int64;
  begin
    if BufferPos > BufferLength - sizeof(LInt64) then ALJSONDocErrorU(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt64, sizeof(LInt64));
    LTimestamp.I64 := LInt64;
    BufferPos := BufferPos + sizeof(LInt64);

    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createnullNode(const name: String;
                            const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
  begin
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createRegExNode(const name: String;
                             const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
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
    if P1 >= BufferLength then ALJSONDocErrorU(cALBSONParseError);
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
    if BufferPos >= BufferLength then ALJSONDocErrorU(cALBSONParseError);
    inc(BufferPos);

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _createJavascriptNode(const name: String;
                                  const NodeSubType: TALJsonNodeSubType);
  var LNode: TALJsonNodeU;
      LJavascript: String;
      LInt32: Int32;
  begin
    if BufferPos > BufferLength - sizeof(LInt32) then ALJSONDocErrorU(cALBSONParseError);
    ALMove(Buffer[BufferPos], LInt32, sizeof(LInt32));
    BufferPos := BufferPos + sizeof(LInt32);
    if (BufferPos + LInt32 > BufferLength) then ALJSONDocErrorU(cALBSONParseError);
    LJavascript := Tencoding.UTF8.GetString(Buffer,BufferPos,LInt32 - 1{for the trailing #0});
    BufferPos := BufferPos + LInt32;

    //create the node
    if NotSaxMode then begin
      if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);
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
  Var LNode: TALJsonNodeU;
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
        if (ObjectPaths.Count = 0) then ALJSONDocErrorU(cALBSONParseError);
      end
      else begin
        if (NamePaths.Count = 0) then ALJSONDocErrorU(cALBSONParseError);
      end;

      //if we are not in sax mode
      if NotSaxMode then begin

        //init anode to one level up
        if assigned(ObjectPaths) then LNode := ObjectPaths[ObjectPaths.Count - 1]
        else LNode := TALJsonNodeU(NamePaths.Objects[NamePaths.Count - 1]);

        //if anode <> workingNode aie aie aie
        if (LNode <> WorkingNode) then ALJSONDocErrorU(cALBSONParseError);

        //calculate anodeTypeInt
        LNodeType := LNode.NodeType;
        if not (LNodeType in [ntObject, ntarray]) then ALJSONDocErrorU(cALBSONParseError);

        //if working node <> containernode then we can go to one level up
        If WorkingNode<>ContainerNode then begin

          //init WorkingNode to the parentNode
          WorkingNode := WorkingNode.ParentNode;

        end

        //if working node = containernode then we can no go to the parent node so set WorkingNode to nil
        Else WorkingNode := nil;

      end

      //if we are in sax mode
      else begin

        //calculate anodeTypeInt
        LNodeType := TALJsonNodeType(NamePaths.Objects[NamePaths.Count - 1]);
        if not (LNodeType in [ntObject,ntarray]) then ALJSONDocErrorU(cALBSONParseError);

      end;

      //call the DoParseEndObject/array event
      if Assigned(fonParseEndObject) then begin
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
      else ALJSONDocErrorU(cALBSONParseError);
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
    if P1 >= BufferLength then ALJSONDocErrorU(cALBSONParseError);
    BufferPos := P1 + 1;
    {$ENDREGION}

    {$REGION 'Begin Object/Array'}
    // ... { ....
    // ... [ ....
    if LNodeSubType in [nstObject,nstArray] then begin

      //if we are not in sax mode
      if NotSaxMode then begin

        //if workingnode = nil then it's mean we are outside the containerNode
        if not assigned(WorkingNode) then ALJSONDocErrorU(cALBSONParseError);

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
        if Assigned(fonParseStartObject) then _DoParseStartObject(CurrName)
      end
      else begin
         if Assigned(fonParseStartArray) then _DoParseStartArray(CurrName);
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

      else ALJSONDocErrorU(cALBSONParseError);
    end;
    {$ENDREGION}

  end;

Begin

  //
  // NOTE: the childNodes of the ContainerNode
  //       must have been cleared by the calling function!
  //
  // NOTE: ContainerNode must have fDocument assigned
  //
  // NOTE: ContainerNode must be ntobject or nil (sax mode)
  //

  //error if Immutable
  if (doImmutable in Options) then ALJSONDocErrorU(cALJSONImmutable);

  //event fonParseStartDocument
  DoParseStartDocument;

  //init WorkingNode and NotSaxMode
  WorkingNode := ContainerNode;
  NotSaxMode := assigned(ContainerNode);

  //init ObjectPaths or NamePaths
  if (NotSaxMode) and
     (not assigned(fonParseText)) and
     (not assigned(FonParseStartObject)) and
     (not assigned(FonParseEndObject)) and
     (not assigned(FonParseStartArray)) and
     (not assigned(FonParseEndArray)) then begin
    ObjectPaths := TObjectList<TALJsonNodeU>.Create(false{OwnsObjects});
    NamePaths := nil;
  end
  else begin
    ObjectPaths := nil;
    NamePaths := TALStringListU.Create;
  end;
  Try

    //init Buffer
    BufferLength := length(Buffer);
    BufferPos := 4; // the first 4 bytes are the length of the document and we don't need it

    //add first node in ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ObjectPaths.Add(WorkingNode)
    else begin
      if NotSaxMode then NamePaths.AddObject('[-1]', WorkingNode)
      else NamePaths.AddObject('[-1]', pointer(ntObject));
    end;

    //analyze all the nodes
    While (BufferPos < BufferLength) do
      AnalyzeNode;

    //some tags are not closed
    if assigned(ObjectPaths) then begin
      if ObjectPaths.Count > 0 then ALJSONDocErrorU(cALBSONParseError);
    end
    else begin
      if NamePaths.Count > 0 then ALJSONDocErrorU(cALBSONParseError);
    end;

    //mean the node was not update (empty stream?) or not weel closed
    if WorkingNode <> nil then ALJSONDocErrorU(cALBSONParseError);

    //event fonParseEndDocument
    DoParseEndDocument;

  finally

    //free ObjectPaths/NamePaths
    if assigned(ObjectPaths) then ALFreeAndNil(ObjectPaths)
    else ALFreeAndNil(NamePaths);

  end;

end;

{************************************}
procedure TALJSONDocumentU.ReleaseDoc;
begin
  if (doImmutable in Options) then ALJSONDocErrorU(cALJSONImmutable);
  if assigned(FDocumentNode) then ALFreeAndNil(FDocumentNode);
end;

{*****************************************************************}
{Loads a string representation of an JSON document and activates it.
 Call LoadFromJSONString to assign a string as the value of the JSON document. Unlike the JSON property, which lets you assign JSON on a line-by-line
 basis, LoadFromJSONString treats the text of the JSON document as a whole.
 The str parameter is a string containing the text of an JSON document. It should represent the JSON text encoded using 8 bits char (utf-8, iso-8859-1, etc)
 After assigning the JSON property as the contents of the document, LoadFromJSONString sets the Active property to true.}
procedure TALJSONDocumentU.LoadFromJSONString(const Str: String; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
begin
  if saxMode then SetActive(False)
  else begin
    if ClearChildNodes then releaseDoc;
    SetActive(True);
  end;
  ParseJSON(Str, FDocumentNode)
end;

{****************************************************}
{Loads an JSON document from a stream and activates it.
 Call LoadFromJSONStream to load the JSON document from a stream.
 *Stream is a stream object that can be used to read the string of JSON that makes up the document.
 After loading the document from Stream, LoadFromJSONStream sets the Active property to true.}
procedure TALJSONDocumentU.LoadFromJSONStream(const Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
begin
  if saxMode then SetActive(False)
  else begin
    if ClearChildNodes then releaseDoc;
    SetActive(True);
  end;
  ParseJSON(ALGetStringFromStreamU(Stream, TEncoding.UTF8), FDocumentNode)
end;

{**************************************}
{Loads an JSON document and activates it.
 Call LoadFromJSONFile to load the JSON document specified by AFileName and set the Active property to true so
 that you can examine or modify the document.
 *AFileName is the name of the JSON document to load from disk. If AFileName is an empty string, TALJSONDocumentU uses the value of the
  FileName property. If AFileName is not an empty string, TALJSONDocumentU changes the FileName property to AFileName.
 Once you have loaded an JSON document, any changes you make to the document are not saved back to disk until you call the SaveToFile method.}
procedure TALJSONDocumentU.LoadFromJSONFile(const FileName: String; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
var FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromJSONStream(FileStream, saxMode, ClearChildNodes);
  finally
    ALFreeAndNil(FileStream);
  end;
end;

{**************************************************************************************************************************************}
procedure TALJSONDocumentU.LoadFromBSONBytes(const Bytes: Tbytes; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
begin
  if saxMode then SetActive(False)
  else begin
    if ClearChildNodes then releaseDoc;
    SetActive(True);
  end;
  ParseBSON(Bytes, FDocumentNode)
end;

{******************************************************************************************************************************************}
procedure TALJSONDocumentU.LoadFromBSONStream(const Stream: TStream; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
begin
  if saxMode then SetActive(False)
  else begin
    if ClearChildNodes then releaseDoc;
    SetActive(True);
  end;
  ParseBSON(ALGetBytesFromStream(Stream), FDocumentNode)
end;

{*****************************************************************************************************************************************}
procedure TALJSONDocumentU.LoadFromBSONFile(const FileName: String; const saxMode: Boolean = False; Const ClearChildNodes: Boolean = True);
var FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromBSONStream(FileStream, saxMode, ClearChildNodes);
  finally
    ALFreeAndNil(FileStream);
  end;
end;

{***********************************}
{Saves the JSON document to a stream.
 Call SaveToStream to save the contents of the JSON document to the stream specified by Stream.}
procedure TALJSONDocumentU.SaveToJSONStream(const Stream: TStream; const Encoding: TEncoding);
begin
  CheckActive;
  node.SaveToJSONStream(Stream, Encoding);
end;

{*****************************************************************}
procedure TALJSONDocumentU.SaveToJSONStream(const Stream: TStream);
begin
  SaveToJSONStream(Stream, TEncoding.UTF8);
end;

{******************************}
{Saves the JSON document to disk.
 Call SaveToFile to save any modifications you have made to the parsed JSON document.
 AFileName is the name of the file to save.}
procedure TALJSONDocumentU.SaveToJSONFile(const FileName: String; const Encoding: TEncoding);
begin
  CheckActive;
  node.SaveToJSONFile(FileName, Encoding);
end;

{****************************************************************}
procedure TALJSONDocumentU.SaveToJSONFile(const FileName: String);
begin
  SaveToJSONFile(FileName, TEncoding.UTF8);
end;

{************************************************}
{Saves the JSON document to a string-type variable.
 Call SaveToJSON to save the contents of the JSON document to the string-type variable specified by JSON. SaveToJSON writes the contents of JSON document
 using 8 bits char (utf-8, iso-8859-1, etc) as an encoding system, depending on the type of the JSON parameter.
 Unlike the JSON property, which lets you write individual lines from the JSON document, SaveToJSON writes the entire text of the JSON document.}
procedure TALJSONDocumentU.SaveToJSONString(var str: String);
begin
  CheckActive;
  node.SaveToJSONString(Str);
end;

{*****************************************************************}
procedure TALJSONDocumentU.SaveToBsonStream(const Stream: TStream);
begin
  CheckActive;
  node.SaveToBsonStream(Stream);
end;

{****************************************************************}
procedure TALJSONDocumentU.SaveToBsonFile(const FileName: String);
begin
  CheckActive;
  node.SaveToBsonFile(FileName);
end;

{************************************************************}
procedure TALJSONDocumentU.SaveToBsonBytes(var bytes: Tbytes);
begin
  CheckActive;
  node.SaveToBsonBytes(Bytes);
end;

{*************************************}
{Returns the value of the JSON property.
 GetJSON is the read implementation of the JSON property.}
function TALJSONDocumentU.GetJSON: String;
begin
  SaveToJSONString(Result);
end;

{*************************************}
{Returns the value of the BSON property.
 GetBSON is the read implementation of the BSON property.}
function TALJSONDocumentU.GetBSON: Tbytes;
begin
  SaveToBSONBytes(Result);
end;

{**********************************}
{Sets the value of the JSON property.
 SetJSON is the write implementation of the JSON property.
 *Value contains the raw (unparsed) JSON to assign.}
procedure TALJSONDocumentU.SetJSON(const Value: String);
begin
  LoadFromJSONString(Value, False{saxMode}, true{ClearChildNodes});
end;

{**********************************}
{Sets the value of the BSON property.
 SetBSON is the write implementation of the BSON property.
 *Value contains the raw (unparsed) BSON to assign.}
procedure TALJSONDocumentU.SetBSON(const Value: Tbytes);
begin
  LoadFromBSONBytes(Value, False{saxMode}, true{ClearChildNodes});
end;

{*************************************}
procedure TALJSONDocumentU.CheckActive;
begin
  if not Assigned(FDocumentNode) then ALJSONDocErrorU(CALJSONNotActive);
end;

{********************************************************************************************************************************************}
function TALJSONDocumentU.AddChild(const NodeName: String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU;
begin
  Result := Node.AddChild(NodeName, NodeType, Index);
end;

{*************************************************************************************************************************************************}
function TALJSONDocumentU.AddChild(const Path: array of String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU;
begin
  Result := Node.AddChild(Path, NodeType, Index);
end;

{****************************************************************************************************}
function TALJSONDocumentU.CreateNode(const NodeName: String; NodeType: TALJSONNodeType): TALJSONNodeU;
begin
  Result := ALCreateJSONNodeU(NodeName, NodeType);
end;

{********************************************}
{Returns the value of the ChildNodes property.
 GetChildNodes is the read implementation of the ChildNodes property.}
function TALJSONDocumentU.GetChildNodes: TALJSONNodeListU;
begin
  Result := Node.ChildNodes;
end;

{***************************************************************************}
function TALJSONDocumentU.GetChildNode(const nodeName: String): TALJSONNodeU;
begin
  result := Node.GetChildNode(nodeName);
end;

{*****************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueText(const nodeName: String; const default: String): String;
begin
  result := Node.GetChildNodeValueText(nodeName, default);
end;

{******************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueFloat(const nodeName: String; const default: Double): Double;
begin
  result := Node.GetChildNodeValueFloat(nodeName, default);
end;

{***************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueDateTime(const nodeName: String; const default: TDateTime): TDateTime;
begin
  result := Node.GetChildNodeValueDateTime(nodeName, default);
end;

{******************************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueTimestamp(const nodeName: String; const default: TALBSONTimestamp): TALBSONTimestamp;
begin
  result := Node.GetChildNodeValueTimestamp(nodeName, default);
end;

{*********************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueObjectID(const nodeName: String; const default: String): String;  // return a hex string
begin
  result := Node.GetChildNodeValueObjectID(nodeName, default);
end;

{********************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueInt32(const nodeName: String; const default: Integer): Integer;
begin
  result := Node.GetChildNodeValueInt32(nodeName, default);
end;

{****************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueInt64(const nodeName: String; const default: Int64): Int64;
begin
  result := Node.GetChildNodeValueInt64(nodeName, default);
end;

{*******************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueBool(const nodeName: String; const default: Boolean): Boolean;
begin
  result := Node.GetChildNodeValueBool(nodeName, default);
end;

{***********************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueJavascript(const nodeName: String; const default: String): String;
begin
  result := Node.GetChildNodeValueJavascript(nodeName, default);
end;

{******************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueRegEx(const nodeName: String; const default: String): String;
begin
  result := Node.GetChildNodeValueRegEx(nodeName, default);
end;

{***************************************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueRegExOptions(const nodeName: String; const default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  result := Node.GetChildNodeValueRegExOptions(nodeName, default);
end;

{*******************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueBinary(const nodeName: String; const default: String): String;   // return a base64 encoded string
begin
  result := Node.GetChildNodeValueBinary(nodeName, default);
end;

{**********************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueBinarySubType(const nodeName: String; const default: byte): byte;
begin
  result := Node.GetChildNodeValueBinarySubType(nodeName, default);
end;

{********************************************************************************}
function TALJSONDocumentU.GetChildNode(const path: array of String): TALJSONNodeU;
begin
  result := Node.GetChildNode(path);
end;

{**********************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueText(const path: array of String; const default: String): String;
begin
  result := Node.GetChildNodeValueText(path, default);
end;

{***********************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueFloat(const path: array of String; const default: Double): Double;
begin
  result := Node.GetChildNodeValueFloat(path, default);
end;

{********************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueDateTime(const path: array of String; const default: TDateTime): TDateTime;
begin
  result := Node.GetChildNodeValueDateTime(path, default);
end;

{***********************************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueTimestamp(const path: array of String; const default: TALBSONTimestamp): TALBSONTimestamp;
begin
  result := Node.GetChildNodeValueTimestamp(path, default);
end;

{**************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueObjectID(const path: array of String; const default: String): String;   // return a hex string
begin
  result := Node.GetChildNodeValueObjectID(path, default);
end;

{*************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueInt32(const path: array of String; const default: Integer): Integer;
begin
  result := Node.GetChildNodeValueInt32(path, default);
end;

{*********************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueInt64(const path: array of String; const default: Int64): Int64;
begin
  result := Node.GetChildNodeValueInt64(path, default);
end;

{************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueBool(const path: array of String; const default: Boolean): Boolean;
begin
  result := Node.GetChildNodeValueBool(path, default);
end;

{****************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueJavascript(const path: array of String; const default: String): String;
begin
  result := Node.GetChildNodeValueJavascript(path, default);
end;

{***********************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueRegEx(const path: array of String; const default: String): String;
begin
  result := Node.GetChildNodeValueRegEx(path, default);
end;

{********************************************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueRegExOptions(const path: array of String; const default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  result := Node.GetChildNodeValueRegExOptions(path, default);
end;

{************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueBinary(const path: array of String; const default: String): String;   // return a base64 encoded string
begin
  result := Node.GetChildNodeValueBinary(path, default);
end;

{***************************************************************************************************************}
function TALJSONDocumentU.GetChildNodeValueBinarySubType(const path: array of String; const default: byte): byte;
begin
  result := Node.GetChildNodeValueBinarySubType(path, default);
end;

{********************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueText(const nodeName: String; const value: String);
begin
  Node.SetChildNodeValueText(nodeName, value);
end;

{*********************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueFloat(const nodeName: String; const value: Double);
begin
  Node.SetChildNodeValueFloat(nodeName, value);
end;

{***************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueDateTime(const nodeName: String; const value: TDateTime);
begin
  Node.SetChildNodeValueDateTime(nodeName, value);
end;

{***********************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueTimestamp(const nodeName: String; const value: TALBSONTimestamp);
begin
  Node.SetChildNodeValueTimestamp(nodeName, value);
end;

{************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueObjectID(const nodeName: String; const value: String);
begin
  Node.SetChildNodeValueObjectID(nodeName, value);
end;

{**********************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueInt32(const nodeName: String; const value: Integer);
begin
  Node.SetChildNodeValueInt32(nodeName, value);
end;

{********************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueInt64(const nodeName: String; const value: Int64);
begin
  Node.SetChildNodeValueInt64(nodeName, value);
end;

{*********************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueBool(const nodeName: String; const value: Boolean);
begin
  Node.SetChildNodeValueBool(nodeName, value);
end;

{**************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueJavascript(const nodeName: String; const value: String);
begin
  Node.SetChildNodeValueJavascript(nodeName, value);
end;

{*********************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueRegEx(const nodeName: String; const value: String);
begin
  Node.SetChildNodeValueRegEx(nodeName, value);
end;

{*****************************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueRegExOptions(const nodeName: String; const value: TALPerlRegExOptions);
begin
  Node.SetChildNodeValueRegExOptions(nodeName, value);
end;

{**********************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueBinary(const nodeName: String; const value: String);
begin
  Node.SetChildNodeValueBinary(nodeName, value);
end;

{***************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueBinarySubType(const nodeName: String; const value: byte);
begin
  Node.SetChildNodeValueBinarySubType(nodeName, value);
end;

{***********************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueNull(const nodeName: String);
begin
  Node.SetChildNodeValueNull(nodeName);
end;

{*************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueText(const path: array of String; const value: String);
begin
  Node.SetChildNodeValueText(path, value);
end;

{**************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueFloat(const path: array of String; const value: Double);
begin
  Node.SetChildNodeValueFloat(path, value);
end;

{********************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueDateTime(const path: array of String; const value: TDateTime);
begin
  Node.SetChildNodeValueDateTime(path, value);
end;

{****************************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueTimestamp(const path: array of String; const value: TALBSONTimestamp);
begin
  Node.SetChildNodeValueTimestamp(path, value);
end;

{*****************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueObjectID(const path: array of String; const value: String);
begin
  Node.SetChildNodeValueObjectID(path, value);
end;

{***************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueInt32(const path: array of String; const value: Integer);
begin
  Node.SetChildNodeValueInt32(path, value);
end;

{*************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueInt64(const path: array of String; const value: Int64);
begin
  Node.SetChildNodeValueInt64(path, value);
end;

{**************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueBool(const path: array of String; const value: Boolean);
begin
  Node.SetChildNodeValueBool(path, value);
end;

{*******************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueJavascript(const path: array of String; const value: String);
begin
  Node.SetChildNodeValueJavascript(path, value);
end;

{**************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueRegEx(const path: array of String; const value: String);
begin
  Node.SetChildNodeValueRegEx(path, value);
end;

{**********************************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueRegExOptions(const path: array of String; const value: TALPerlRegExOptions);
begin
  Node.SetChildNodeValueRegExOptions(path, value);
end;

{***************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueBinary(const path: array of String; const value: String);
begin
  Node.SetChildNodeValueBinary(path, value);
end;

{********************************************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueBinarySubType(const path: array of String; const value: byte);
begin
  Node.SetChildNodeValueBinarySubType(path, value);
end;

{****************************************************************************}
procedure TALJSONDocumentU.SetChildNodeValueNull(const path: array of String);
begin
  Node.SetChildNodeValueNull(path);
end;

{************************************************************************}
{Indicates whether the TJSONDocument instance represents an empty document.
 Call IsEmptyDoc to determine whether the TALJSONDocumentU instance represents an empty document.
 IsEmptyDoc returns true if the Document property is not set or if this object represents a
 document with no child nodes.}
function TALJSONDocumentU.IsEmptyDoc: Boolean;
begin
  Result := not (Assigned(FDocumentNode) and FDocumentNode.hasChildNodes);
end;

{**************************************}
{Returns the value of the Node property.
 GetDocumentNode is the read implementation of the Node property.}
function TALJSONDocumentU.GetDocumentNode: TALJSONNodeU;
begin
  CheckActive;
  Result := FDocumentNode;
end;

{***********************************************}
{Returns the value of the NodeIndentStr property.
 GetNodeIndentStr is the read implementation of the NodeIndentStr property.}
function TALJSONDocumentU.GetNodeIndentStr: String;
begin
  Result := FNodeIndentStr;
end;

{********************************************}
{Sets the value of the NodeIndentStr property.
 SetNodeIndentStr is the write implementation of the NodeIndentStr property.
 *Value is the string that is inserted before nested nodes to indicate a level of nesting.}
procedure TALJSONDocumentU.SetNodeIndentStr(const Value: String);
begin
  FNodeIndentStr := Value;
end;

{*****************************************}
{Returns the value of the Options property.
 GetOptions is the read implementation of the Options property.}
function TALJSONDocumentU.GetOptions: TALJSONDocOptions;
begin
  Result := FOptions;
end;

{**************************************}
{Sets the value of the Options property.
 GetOptions is the write implementation of the Options property.
 *Value is the set of options to assign.}
procedure TALJSONDocumentU.SetOptions(const Value: TALJSONDocOptions);
begin
  FOptions := Value;
end;

{**********************************************}
{Returns the value of the ParseOptions property.
 GetParseOptions is the read implementation of the ParseOptions property.}
function TALJSONDocumentU.GetParseOptions: TALJSONParseOptions;
begin
  Result := FParseOptions;
end;

{*******************************************}
{Sets the value of the ParseOptions property.
 GetParseOptions is the write implementation of the ParseOptions property.
 *Value is the set of parser options to assign.}
procedure TALJSONDocumentU.SetParseOptions(const Value: TALJSONParseOptions);
begin
  FParseOptions := Value;
end;

{*************************************************************}
procedure TALJSONDocumentU.SetPathSeparator(const Value: Char);
begin
  FPathSeparator := Value;
end;

{***********************************************}
function TALJSONDocumentU.GetPathSeparator: Char;
begin
  result := fPathSeparator;
end;

{**********************************************}
procedure TALJSONDocumentU.DoParseStartDocument;
begin
  if Assigned(fonParseStartDocument) then fonParseStartDocument(Self);
end;

{********************************************}
procedure TALJSONDocumentU.DoParseEndDocument;
begin
  if Assigned(fonParseEndDocument) then fonParseEndDocument(Self);
end;

{******************************************************************************************************************************************}
procedure TALJSONDocumentU.DoParseText(const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(fonParseText) then fonParseText(Self, Path, name, Args, NodeSubType);
end;

{************************************************************************************}
procedure TALJSONDocumentU.DoParseStartObject(const Path: String; const Name: String);
begin
  if Assigned(fonParseStartObject) then fonParseStartObject(Self, Path, name);
end;

{**********************************************************************************}
procedure TALJSONDocumentU.DoParseEndObject(const Path: String; const Name: String);
begin
  if Assigned(fonParseEndObject) then fonParseEndObject(Self, Path, name);
end;

{***********************************************************************************}
procedure TALJSONDocumentU.DoParseStartArray(const Path: String; const Name: String);
begin
  if Assigned(fonParseStartArray) then fonParseStartArray(Self, Path, name);
end;

{*********************************************************************************}
procedure TALJSONDocumentU.DoParseEndArray(const Path: String; const Name: String);
begin
  if Assigned(fonParseEndArray) then fonParseEndArray(Self, Path, name);
end;

{**********************************************************}
{Creates the object that implements the ChildNodes property}
function TALJSONNodeU.CreateChildList: TALJSONNodeListU;
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
  result := TALJSONNodeListU.Create(Self);
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONNodeU.InternalGetChildNodes: TALJSONNodeListU;
begin
  Result := nil; //virtual;
end;

{****************************************************}
function TALJSONNodeU.GetChildNodes: TALJSONNodeListU;
begin
  Result := nil; // hide warning
  ALJSONDocErrorU(CALJsonOperationError,GetNodeType)
end;

{******************************************************************}
procedure TALJSONNodeU.SetChildNodes(const Value: TALJSONNodeListU);
begin
  ALJSONDocErrorU(CALJsonOperationError,GetNodeType)
end;

{***********************************************************************}
function TALJSONNodeU.GetChildNode(const nodeName: String): TALJSONNodeU;
begin
  result := ChildNodes.findNode(nodeName);
end;

{*************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueText(const nodeName: String; const default: String): String;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetText(default);
end;

{**************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueFloat(const nodeName: String; const default: Double): Double;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetFloat(default);
end;

{***********************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueDateTime(const nodeName: String; const default: TDateTime): TDateTime;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetDateTime(default);
end;

{**************************************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueTimestamp(const nodeName: String; const default: TALBSONTimestamp): TALBSONTimestamp;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetTimestamp(default);
end;

{*****************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueObjectID(const nodeName: String; const default: String): String; // return a hex string
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetObjectID(default);
end;

{****************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueInt32(const nodeName: String; const default: Integer): Integer;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetInt32(default);
end;

{************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueInt64(const nodeName: String; const default: Int64): Int64;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetInt64(default);
end;

{***************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueBool(const nodeName: String; const default: Boolean): Boolean;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBool(default);
end;

{*******************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueJavascript(const nodeName: String; const default: String): String;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetJavascript(default);
end;

{**************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueRegEx(const nodeName: String; const default: String): String;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetRegEx(default);
end;

{***********************************************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueRegExOptions(const nodeName: String; const default: TALPerlRegExOptions): TALPerlRegExOptions;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetRegExOptions(default);
end;

{***************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueBinary(const nodeName: String; const default: String): String;  // return a base64 encoded string
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBinary(default);
end;

{******************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueBinarySubType(const nodeName: String; const default: byte): byte;
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then result := default
  else result := LNode.GetBinarySubType(default);
end;

{****************************************************************************}
function TALJSONNodeU.GetChildNode(const path: array of String): TALJSONNodeU;
var I: integer;
begin
  result := Self;
  for I := low(path) to high(path) do begin
    result := result.ChildNodes.findNode(path[I]);
    if (result = nil) then exit;
  end;
end;

{******************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueText(const path: array of String; const default: String): String;
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueFloat(const path: array of String; const default: Double): Double;
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueDateTime(const path: array of String; const default: TDateTime): TDateTime;
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueTimestamp(const path: array of String; const default: TALBSONTimestamp): TALBSONTimestamp;
var LNode: TALJSONNodeU;
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

{**********************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueObjectID(const path: array of String; const default: String): String; // return a hex string
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueInt32(const path: array of String; const default: Integer): Integer;
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueInt64(const path: array of String; const default: Int64): Int64;
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueBool(const path: array of String; const default: Boolean): Boolean;
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueJavascript(const path: array of String; const default: String): String;
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueRegEx(const path: array of String; const default: String): String;
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueRegExOptions(const path: array of String; const default: TALPerlRegExOptions): TALPerlRegExOptions;
var LNode: TALJSONNodeU;
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

{********************************************************************************************************}
function TALJSONNodeU.GetChildNodeValueBinary(const path: array of String; const default: String): String;  // return a base64 encoded string
var LNode: TALJSONNodeU;
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
function TALJSONNodeU.GetChildNodeValueBinarySubType(const path: array of String; const default: byte): byte;
var LNode: TALJSONNodeU;
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

{****************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueText(const nodeName: String; const value: String);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetText(value)
  else LNode.SetText(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueFloat(const nodeName: String; const value: Double);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetFloat(value)
  else LNode.SetFloat(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueDateTime(const nodeName: String; const value: TDateTime);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetDateTime(value)
  else LNode.SetDateTime(value);
end;

{*******************************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueTimestamp(const nodeName: String; const value: TALBSONTimestamp);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetTimestamp(value)
  else LNode.SetTimestamp(value);
end;

{********************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueObjectID(const nodeName: String; const value: String);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetObjectID(value)
  else LNode.SetObjectID(value);
end;

{******************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueInt32(const nodeName: String; const value: Integer);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt32(value)
  else LNode.SetInt32(value);
end;

{****************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueInt64(const nodeName: String; const value: Int64);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetInt64(value)
  else LNode.SetInt64(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueBool(const nodeName: String; const value: Boolean);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBool(value)
  else LNode.SetBool(value);
end;

{**********************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueJavascript(const nodeName: String; const value: String);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetJavascript(value)
  else LNode.SetJavascript(value);
end;

{*****************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueRegEx(const nodeName: String; const value: String);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegEx(value)
  else LNode.SetRegEx(value);
end;

{*************************************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueRegExOptions(const nodeName: String; const value: TALPerlRegExOptions);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetRegExOptions(value)
  else LNode.SetRegExOptions(value);
end;

{******************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueBinary(const nodeName: String; const value: String);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinary(value)
  else LNode.SetBinary(value);
end;

{***********************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueBinarySubType(const nodeName: String; const value: byte);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetBinarySubType(value)
  else LNode.SetBinarySubType(value);
end;

{*******************************************************************}
procedure TALJSONNodeU.SetChildNodeValueNull(const nodeName: String);
var LNode: TALJSONNodeU;
begin
  LNode := ChildNodes.findNode(nodeName);
  if (LNode = nil) then addChild(nodeName).SetNull(true)
  else LNode.SetNull(true);
end;

{*********************************************************************************************}
procedure TALJSONNodeU.SetChildNodeValueText(const path: array of String; const value: String);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueFloat(const path: array of String; const value: Double);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueDateTime(const path: array of String; const value: TDateTime);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueTimestamp(const path: array of String; const value: TALBSONTimestamp);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueObjectID(const path: array of String; const value: String);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueInt32(const path: array of String; const value: Integer);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueInt64(const path: array of String; const value: Int64);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueBool(const path: array of String; const value: Boolean);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueJavascript(const path: array of String; const value: String);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueRegEx(const path: array of String; const value: String);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueRegExOptions(const path: array of String; const value: TALPerlRegExOptions);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueBinary(const path: array of String; const value: String);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueBinarySubType(const path: array of String; const value: byte);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
procedure TALJSONNodeU.SetChildNodeValueNull(const path: array of String);
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
function TALJSONNodeU.GetHasChildNodes: Boolean;
Var LNodeList: TALJSONNodeListU;
begin
  LNodeList := InternalGetChildNodes;
  Result := assigned(LNodeList) and (LNodeList.Count > 0);
end;

{********************************************}
function TALJSONNodeU.GetNodeValueStr: String;
begin
  ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
  result := ''; // hide warning
end;

{*********************************************}
function TALJSONNodeU.GetNodeValueInt64: int64;
begin
  ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
  result := 0; // hide warning
end;

{**********************************************************************************************}
procedure TALJSONNodeU.SetNodeValue(const Value: String; const NodeSubType: TALJSONNodeSubType);
begin
  ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
end;

{*********************************************************************************************}
procedure TALJSONNodeU.SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
end;

{**************************************************************************************************************************}
procedure TALJSONNodeU.SetNodeValue(const StrValue: String; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
end;

{***********************************}
{Returns the text value of the node.}
function TALJSONNodeU.GetText: String;
begin

  case NodeSubType of
    nstFloat: begin // return the formated float
                if Assigned(FDocument) and (Fdocument.FormatSettings <> @ALDefaultFormatSettingsU) then result := ALFloatToStrU(GetFloat, Fdocument.FormatSettings^)
                else result := GetNodeValueStr;
              end;
    nstText: result := GetNodeValueStr;  // return the raw text
    nstObject: result := GetNodeValueStr;  // return the raw objectID
    nstArray: result := GetNodeValueStr;  // error
    nstObjectID: result := GetNodeValueStr; // error
    nstBoolean: result := GetNodeValueStr;  // return true or false
    nstDateTime: begin // return the formated datetime
                   if Assigned(FDocument) and (Fdocument.FormatSettings <> @ALDefaultFormatSettingsU) then result := ALDateTimeToStrU(GetDateTime, Fdocument.FormatSettings^)
                   else result := GetNodeValueStr;
                 end;
    nstNull: result := GetNodeValueStr; // return null
    nstRegEx: result := GetNodeValueStr; // return the raw regex (without the options)
    nstBinary: result := GetNodeValueStr; // return the base64 encoded binary (without the binary subtype)
    nstJavascript: result := GetNodeValueStr; // return the raw javascript
    nstInt32: result := GetNodeValueStr;  // return the number
    nstTimestamp: result := GetNodeValueStr;  // return the number (as int64)
    nstInt64: result := GetNodeValueStr;  // return the number
    else ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
  end;

end;

{***********************************************************}
function TALJSONNodeU.GetText(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetText;
end;

{********************************}
{Sets the text value of the node.}
procedure TALJSONNodeU.SetText(const Value: String);
begin
  setNodeValue(Value, nstText);
end;

{******************************************************************************}
// By default json (ie: javascript) treats all numbers as floating-point values.
// To let other system (ie: mongoDB) understand the type of the number
// we provide the helper functions NumberLong() to handle 64-bit integers
// and NumberInt() to handle 32-bit integers (and some others). theses helper functions are
// used when saving the json document.
function TALJSONNodeU.GetNodeValueInterchange(const SkipNodeSubTypeHelper: boolean = False): String;

  procedure _GetObjectID;
  begin
    if SkipNodeSubTypeHelper then result := '"'+ObjectID+'"'
    else result := 'ObjectId("'+ObjectID+'")';
  end;

  procedure _GetBinary;
  begin
    if SkipNodeSubTypeHelper then result := '"'+Binary+'"'
    else result := 'BinData('+alinttostrU(BinarySubType)+', "'+Binary+'")';
  end;

  procedure _GetDateTime;
  begin
    if SkipNodeSubTypeHelper then result := ALFormatDateTimeU('''"''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z"''', DateTime, ALDefaultFormatSettingsU)
    else result := ALFormatDateTimeU('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', DateTime, ALDefaultFormatSettingsU)
  end;

  procedure _Getint32;
  begin
    if SkipNodeSubTypeHelper then result := text
    else result := 'NumberInt(' + text + ')'
  end;

  procedure _Getint64;
  begin
    if SkipNodeSubTypeHelper then result := text
    else result := 'NumberLong(' + text + ')';
  end;

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
    if not SkipNodeSubTypeHelper then result := '"' + ALJavascriptEncodeU(result) + '"'
  end;

  procedure _GetTimestamp;
  begin
    if SkipNodeSubTypeHelper then result := '"Timestamp('+alinttostrU(GetTimeStamp.W1)+', '+alinttostrU(GetTimeStamp.W2)+')"'
    else result := 'Timestamp('+alinttostrU(GetTimeStamp.W1)+', '+alinttostrU(GetTimeStamp.W2)+')';
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
function TALJSONNodeU.GetFloat: Double;
begin
  case NodeSubType of
    nstFloat: PInt64(@result)^ := GetNodeValueInt64;
    nstInt32,
    nstInt64: Result := GetNodeValueInt64;
    else begin
      ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{************************************************************}
function TALJSONNodeU.GetFloat(const default: Double): Double;
begin
  if NodeSubType = nstNull then result := default
  else result := GetFloat;
end;

{***************************************************}
procedure TALJSONNodeU.SetFloat(const Value: Double);
begin
  setNodeValue(PInt64(@Value)^, nstFloat);
end;

{*******************************************}
function TALJSONNodeU.GetDateTime: TDateTime;
begin
  if NodeSubType = nstDateTime then PInt64(@result)^ := GetNodeValueInt64
  else begin
    ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
    result := 0; // to hide a warning;
  end;
end;

{*********************************************************************}
function TALJSONNodeU.GetDateTime(const default: TDateTime): TDateTime;
begin
  if NodeSubType = nstNull then result := default
  else result := GetDateTime;
end;

{*********************************************************}
procedure TALJSONNodeU.SetDateTime(const Value: TDateTime);
begin
  setNodeValue(PInt64(@Value)^, nstDateTime);
end;

{***************************************************}
function TALJSONNodeU.GetTimestamp: TALBSONTimestamp;
begin
  if NodeSubType = nstTimestamp then result.I64 := GetNodeValueInt64
  else begin
    ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
    result.I64 := 0; // to hide a warning;
  end;
end;

{************************************************************************************}
function TALJSONNodeU.GetTimestamp(const default: TALBSONTimestamp): TALBSONTimestamp;
begin
  if NodeSubType = nstNull then result := default
  else result := GetTimestamp;
end;

{*****************************************************************}
procedure TALJSONNodeU.SetTimestamp(const Value: TALBSONTimestamp);
begin
  setNodeValue(Value.I64, nstTimestamp);
end;

{****************************************}
function TALJSONNodeU.GetObjectID: String;
begin
  if NodeSubType = nstObjectID then result := GetNodeValueStr
  else begin
    ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{***************************************************************}
function TALJSONNodeU.GetObjectID(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetObjectID;
end;

{******************************************************}
procedure TALJSONNodeU.SetObjectID(const Value: String);
begin
  if length(Value) <> 24 then ALJSONDocErrorU('ObjectID must have 12 bytes');
  setNodeValue(Value, nstObjectID);
end;

{**************************************}
function TALJSONNodeU.GetInt32: Integer;
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
                   (LInt64 < system.int32.MinValue) then ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
                result := LInt64;
              end;
    nstInt32: begin
                LInt64 := GetNodeValueInt64;
                if (LInt64 > system.int32.MaxValue) or
                   (LInt64 < system.int32.MinValue) then ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
                result := LInt64;
              end;
    nstInt64: Result := GetNodeValueInt64;
    else begin
      ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{**************************************************************}
function TALJSONNodeU.GetInt32(const default: Integer): Integer;
begin
  if NodeSubType = nstNull then result := default
  else result := GetInt32;
end;

{****************************************************}
procedure TALJSONNodeU.SetInt32(const Value: Integer);
begin
  setNodeValue(Value, nstInt32);
end;

{************************************}
function TALJSONNodeU.GetInt64: Int64;
var LDouble: Double;
begin
  case NodeSubType of
    nstFloat: begin
                PInt64(@LDouble)^ := GetNodeValueInt64;
                result := trunc(LDouble);
                if result <> LDouble then ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType); // https://stackoverflow.com/questions/41779801/single-double-and-precision
                                                                                          // Only values that are in form m*2^e, where m and e are integers can be stored in a floating point variable
                                                                                          // so all integer can be store in the form m*2^e (ie: m = m*2^0)
                                                                                          // so we can compare result <> aDouble without the need of samevalue
              end;
    nstInt32,
    nstInt64: Result := GetNodeValueInt64;
    else begin
      ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
      result := 0; // to hide a warning;
    end;
  end;
end;

{**********************************************************}
function TALJSONNodeU.GetInt64(const default: Int64): Int64;
begin
  if NodeSubType = nstNull then result := default
  else result := GetInt64;
end;

{**************************************************}
procedure TALJSONNodeU.SetInt64(const Value: Int64);
begin
  setNodeValue(Value, nstInt64);
end;

{*************************************}
function TALJSONNodeU.GetBool: Boolean;
begin
  if NodeSubType = nstBoolean then begin
    if GetNodeValueInt64 = 0 then result := False
    else result := true;
  end
  else begin
    ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
    result := False; // to hide a warning;
  end;
end;

{*************************************************************}
function TALJSONNodeU.GetBool(const default: Boolean): Boolean;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBool;
end;

{***************************************************}
procedure TALJSONNodeU.SetBool(const Value: Boolean);
begin
  if Value then setNodeValue(1, nstBoolean)
  else setNodeValue(0, nstBoolean);
end;

{*************************************}
function TALJSONNodeU.GetNull: Boolean;
begin
  result := NodeSubType = nstNull;
end;

{***************************************************}
procedure TALJSONNodeU.SetNull(const Value: Boolean);
begin
  if Value then setNodeValue(0, nstNull)
  else ALJSONDocErrorU('Only "true" is allowed for setNull property');
end;

{******************************************}
function TALJSONNodeU.GetJavascript: String;
begin
  if NodeSubType = nstJavascript then result := GetNodeValueStr
  else begin
    ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{*****************************************************************}
function TALJSONNodeU.GetJavascript(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetJavascript;
end;

{********************************************************}
procedure TALJSONNodeU.SetJavascript(const Value: String);
begin
  setNodeValue(Value, nstJavascript);
end;

{*************************************}
function TALJSONNodeU.GetRegEx: String;
begin
  if NodeSubType = nstRegEx then result := GetNodeValueStr
  else begin
    ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{************************************************************}
function TALJSONNodeU.GetRegEx(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetRegEx;
end;

{*****************************************************}
procedure TALJSONNodeU.SetRegEx(const Pattern: String);
begin
  setNodeValue(Pattern, 0, nstRegEx);
end;

{*****************************************************************************************}
procedure TALJSONNodeU.SetRegEx(const Pattern: String; const Options: TALPerlRegExOptions);
begin
  setNodeValue(Pattern, byte(Options), nstRegEx);
end;

{*********************************************************}
function TALJSONNodeU.GetRegExOptions: TALPerlRegExOptions;
begin
  if NodeSubType = nstRegEx then result := TALPerlRegExOptions(byte(GetNodeValueInt64))
  else begin
    ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
    result := []; // to hide a warning;
  end;
end;

{*********************************************************************************************}
function TALJSONNodeU.GetRegExOptions(const default: TALPerlRegExOptions): TALPerlRegExOptions;
begin
  if NodeSubType = nstNull then result := default
  else result := GetRegExOptions;
end;

{***********************************************************************}
procedure TALJSONNodeU.SetRegExOptions(const Value: TALPerlRegExOptions);
begin
  if NodeSubType <> nstRegEx then ALJSONDocErrorU('You can set regex options only to a regex node');
  setNodeValue(byte(Value), nstRegEx);
end;

{**************************************}
function TALJSONNodeU.GetBinary: String;
begin
  if NodeSubType = nstBinary then result := GetNodeValueStr
  else begin
    ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
    result := ''; // to hide a warning;
  end;
end;

{*************************************************************}
function TALJSONNodeU.GetBinary(const default: String): String;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBinary;
end;

{***************************************************}
procedure TALJSONNodeU.SetBinary(const Data: String);
begin
  setNodeValue(Data, 0, nstBinary); // 0 = Default BSON type
end;

{************************************************************************}
procedure TALJSONNodeU.SetBinary(const Data: String; const Subtype: byte);
begin
  setNodeValue(Data, Subtype, nstBinary);
end;

{*******************************************}
function TALJSONNodeU.GetBinarySubType: byte;
begin
  if NodeSubType = nstBinary then result := byte(GetNodeValueInt64)
  else begin
    ALJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
    result := 0; // to hide a warning;
  end;
end;

{****************************************************************}
function TALJSONNodeU.GetBinarySubType(const default: byte): byte;
begin
  if NodeSubType = nstNull then result := default
  else result := GetBinarySubType;
end;

{***********************************************************}
procedure TALJSONNodeU.SetBinarySubType(const Subtype: byte);
begin
  if NodeSubType <> nstBinary then ALJSONDocErrorU('You can set binary subtype only to a binary node');
  setNodeValue(Subtype, nstBinary);
end;

{*******************************************************}
{Returns the document object in which this node appears.}
function TALJSONNodeU.GetOwnerDocument: TALJSONDocumentU;
begin
  Result := FDocument;
end;

{********************************************************************}
procedure TALJSONNodeU.SetOwnerDocument(const Value: TALJSONDocumentU);
var I: Integer;
    LNodeList: TALJSONNodeListU;
begin
  if FDocument <> Value then begin
    if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
    FDocument := Value;
    LNodeList := InternalGetChildNodes;
    if Assigned(LNodeList) then
      for I := 0 to LNodeList.Count - 1 do
        LNodeList[I].SetOwnerDocument(Value);
  end;
end;

{************************}
{returns the parent node.}
function TALJSONNodeU.GetParentNode: TALJSONNodeU;
begin
  Result := FParentNode;
end;

{******************************************}
{Sets the value of the ParentNode property.}
procedure TALJSONNodeU.SetParentNode(const Value: TALJSONNodeU);
begin
  if FParentNode <> Value then begin
    if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
    If assigned(Value) then SetOwnerDocument(Value.OwnerDocument)
    else SetOwnerDocument(nil);
    FParentNode := Value;
  end;
end;

{*******************************************************************}
{Returns the JSON that corresponds to the subtree rooted at this node.
 GetJSON returns the JSON that corresponds to this node and any child nodes it contains.}
function TALJSONNodeU.GetJSON: String;
begin
  SaveToJSONString(result);
end;

{************************************************}
{SetJSON reload the node with the new given value }
procedure TALJSONNodeU.SetJSON(const Value: String);
Begin
  LoadFromJSONString(Value, true{ClearChildNodes});
end;

{*******************************************************************}
{Returns the BSON that corresponds to the subtree rooted at this node.
 GetBSON returns the BSON that corresponds to this node and any child nodes it contains.}
function TALJSONNodeU.GetBSON: Tbytes;
begin
  SaveToBSONBytes(result);
end;

{************************************************}
{SetBSON reload the node with the new given value }
procedure TALJSONNodeU.SetBSON(const Value: Tbytes);
Begin
  LoadFromBSONBytes(Value, true{ClearChildNodes});
end;

{*****************************************************************}
{Returns the number of parents for this node in the node hierarchy.
 NestingLevel returns the number of ancestors for this node in the node hierarchy.}
function TALJSONNodeU.NestingLevel: Integer;
var PNode: TALJSONNodeU;
begin
  Result := 0;
  PNode := ParentNode;
  while PNode <> nil do begin
    Inc(Result);
    PNode := PNode.ParentNode;
  end;
end;

{******************************************************}
constructor TALJSONNodeU.Create(const NodeName: String);
Begin
  FDocument := nil;
  FParentNode := nil;
  fNodeName := NodeName;
end;

{***************************************************************}
//will create all the nodevalue and childnodelist to be sure that
//multiple thread can safely read at the same time the node
procedure TALJSONNodeU.MultiThreadPrepare;
var I: integer;
begin
  if NodeType = ntText then begin

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

  else begin
    For I := 0 to ChildNodes.Count - 1 do
      ChildNodes[I].MultiThreadPrepare;
  end;
end;

{****************************************************************************************************************************************}
function TALJSONNodeU.AddChild(const NodeName: String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU;
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
  Result := ALCreateJSONNodeU(NodeName,NodeType);
  Try
    ChildNodes.Insert(Index, Result);
  except
    ALFreeAndNil(Result);
    raise;
  end;
end;

{*********************************************************************************************************************************************}
function TALJSONNodeU.AddChild(const Path: array of String; const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU;
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
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
function TALJSONNodeU.AddChild(const NodeType: TALJSONNodeType = ntText; const Index: Integer = -1): TALJSONNodeU;
begin
  Result := AddChild('', NodeType, Index);
end;

{*****************************************************************}
function TALJSONNodeU.DeleteChild(const NodeName: String): boolean;
var I: integer;
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
  I := ChildNodes.IndexOf(NodeName);
  if I >= 0 then begin
    ChildNodes.Delete(I);
    result := True;
  end
  else result := False;
end;

{**********************************************************************}
function TALJSONNodeU.DeleteChild(const Path: array of String): boolean;
var LNode: TALJSONNodeU;
    LTmpNode: TALJSONNodeU;
    I: integer;
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
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

{********************************************}
{Returns the next child of this node’s parent.
 NextSibling returns the node that follows this one in the parent node’s ChildNodes property list.
 If this node is the last node in its parent’s child list, NextSibling raises an exception.}
function TALJSONNodeU.NextSibling: TALJSONNodeU;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, 1)
  else Result := nil;
end;

{************************************************}
{Returns the previous child of this node’s parent.
 PreviousSibling returns the node that precedes this one in the parent node’s ChildNodes property list.
 If this node is the first node in its parent’s child list, PreviousSibling raises an exception.}
function TALJSONNodeU.PreviousSibling: TALJSONNodeU;
begin
  if Assigned(ParentNode) then Result := ParentNode.ChildNodes.FindSibling(Self, -1)
  else Result := nil;
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN WIDECHAR_REDUCED OFF}
procedure TALJSONNodeU.SaveToJson(const Stream: TStream;
                                  const StreamEncoding: TEncoding;
                                  Var buffer: String);

Const BufferSize: integer = 8192;

Var NodeStack: Tstack<TALJSONNodeU>;
    CurrentNode: TalJSONNodeU;
    CurrentParentNode: TalJSONNodeU;
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
  Procedure _WriteTextNode2Buffer(aTextNode:TALJSONNodeU);
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
           _WriteStr2Buffer(ALJavascriptEncodeU(NodeName));
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
          _WriteStr2Buffer(ALJavascriptEncodeU(GetText));
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
  Procedure _WriteStartObjectNode2Buffer(aObjectNode:TALJSONNodeU);
  var LNodeList: TALJSONNodeListU;
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
          _WriteStr2Buffer(ALJavascriptEncodeU(NodeName));
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
  Procedure _WriteEndObjectNode2Buffer(aObjectNode:TALJSONNodeU);
  Begin
    if AutoIndentNode then begin
      delete(CurrentIndentStr, length(CurrentIndentStr) - length(IndentStr)+1, maxint);
      _WriteStr2Buffer(#13#10);
      _WriteStr2Buffer(CurrentIndentStr);
    end;
    _WriteStr2Buffer('}');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartArrayNode2Buffer(aArrayNode:TALJSONNodeU);
  var LNodeList: TALJSONNodeListU;
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
          _WriteStr2Buffer(ALJavascriptEncodeU(NodeName));
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
  Procedure _WriteEndArrayNode2Buffer(aArrayNode:TALJSONNodeU);
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
  NodeStack := Tstack<TALJSONNodeU>.Create;
  Try

    {init buffer string}
    Setlength(Buffer, BufferSize); // will make buffer uniquestring
    BufferPos := 0;
    LastWrittenChar := '{';
    EncodeControlCharacters := (FDocument = nil) or (not (poIgnoreControlCharacters in FDocument.ParseOptions));
    SkipNodeSubTypeHelper := (FDocument <> nil) and (poSkipNodeSubTypeHelper in FDocument.ParseOptions);
    SaveInt64AsText := SkipNodeSubTypeHelper and (FDocument <> nil) and (poSaveInt64AsText in FDocument.ParseOptions);
    AutoIndentNode := (FDocument <> nil) and (doNodeAutoIndent in FDocument.Options);
    if FDocument <> nil then IndentStr := FDocument.NodeIndentStr
    else IndentStr := vALDefaultNodeIndentU;
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
          else AlJSONDocErrorU(cAlJSONInvalidNodeType);
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
{$IFEND}

{***********************************}
{Saves the JSON document to a stream.
 Call SaveToStream to save the contents of the JSON document to the stream specified by Stream.}
procedure TALJSONNodeU.SaveToJSONStream(const Stream: TStream; const Encoding: TEncoding);
var buffer: String;
begin
  SaveToJson(Stream, Encoding, buffer);
end;

{*************************************************************}
procedure TALJSONNodeU.SaveToJSONStream(const Stream: TStream);
begin
  SaveToJSONStream(Stream, TEncoding.UTF8);
end;

{******************************}
{Saves the JSON document to disk.
 Call SaveToFile to save any modifications you have made to the parsed JSON document.
 AFileName is the name of the file to save.}
procedure TALJSONNodeU.SaveToJSONFile(const FileName: String; const Encoding: TEncoding);
Var LfileStream: TfileStream;
    LTmpFilename: String;
begin
  if (assigned(FDocument)) and
     (doProtectedSave in fDocument.Options) then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LfileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToJSONStream(LfileStream, Encoding);
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

{************************************************************}
procedure TALJSONNodeU.SaveToJSONFile(const FileName: String);
begin
  SaveToJSONFile(FileName, TEncoding.UTF8);
end;

{************************************************}
{Saves the JSON document to a string-type variable.
 Call SaveToJSON to save the contents of the JSON document to the string-type variable specified by JSON. SaveToJSON writes the contents of JSON document
 using 8 bits char (utf-8, iso-8859-1, etc) as an encoding system, depending on the type of the JSON parameter.
 Unlike the JSON property, which lets you write individual lines from the JSON document, SaveToJSON writes the entire text of the JSON document.}
procedure TALJSONNodeU.SaveToJSONString(var str: String);
begin
  SaveToJson(nil, nil, Str);
end;

{******************************************************}
procedure TalJSONNodeU.SaveToBson(const Stream: TStream;
                                  Var buffer: Tbytes);

Const BufferSize: integer = 8192;

Var NodeStack: Tstack<TalJSONNodeU>;
    NodeIndexStack: TALintegerList;
    NodeStartPosStack: TALInt64List;
    CurrentNode: TalJSONNodeU;
    CurrentParentNode: TalJSONNodeU;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteUTF8Str2Buffer(const str:String); overload;
  var LBytes: Tbytes;
  Begin
    LBytes := Tencoding.UTF8.GetBytes(str);
    _Write2Buffer(pointer(LBytes)^,length(LBytes));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteUTF8Str2Buffer(const index:integer); overload;
  Begin
    _WriteUTF8Str2Buffer(alinttostrU(index));
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
  Procedure _WriteFloatValue2Buffer(aTextNode:TalJSONNodeU);
  var LDouble: Double;
  begin
    LDouble := aTextNode.Float;
    _Write2Buffer(LDouble, sizeOf(LDouble));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x02 + name + \x00 + length (int32) + string + \x00
  Procedure _WriteTextValue2Buffer(aTextNode:TalJSONNodeU);
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
  Procedure _WriteBinaryValue2Buffer(aTextNode:TalJSONNodeU);
  var LInt32: system.int32;
      LBinary: Tbytes;
      LBinarySubType: Byte;
  begin
    LBinary := ALBase64DecodeBytesU(aTextNode.binary);
    LBinarySubType := aTextNode.BinarySubType;
    LInt32 := length(LBinary);
    _Write2Buffer(LInt32, sizeOf(LInt32));
    _Write2Buffer(LBinarySubType, sizeOF(LBinarySubType));
    _WriteBytes2Buffer(LBinary);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x07 + name + \x00 + (byte*12)
  Procedure _WriteObjectIDValue2Buffer(aTextNode:TalJSONNodeU);
  begin
    _WriteBytes2Buffer(ALHexToBinU(aTextNode.ObjectID));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x08 + name + \x00 + \x00 => Boolean "false"
  // \x08 + name + \x00 + \x01	=> Boolean "true"
  Procedure _WriteBooleanValue2Buffer(aTextNode:TalJSONNodeU);
  begin
    if not aTextNode.bool then _WriteByte2Buffer($00)
    else _WriteByte2Buffer($01);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x09 + name + \x00 + int64
  Procedure _WriteDateTimeValue2Buffer(aTextNode:TalJSONNodeU);
  var LInt64: system.Int64;
  begin
    LInt64 := ALDateTimeToUnixMs(aTextNode.DateTime);
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x11 + name + \x00 + int64
  Procedure _WriteTimestampValue2Buffer(aTextNode:TalJSONNodeU);
  var LInt64: system.Int64;
  begin
    LInt64 := aTextNode.Timestamp.I64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \xOB + name + \x00 + (byte*) + \x00 + (byte*) + \x00
  Procedure _WriteRegExValue2Buffer(aTextNode:TalJSONNodeU);
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
  Procedure _WriteJavascriptValue2Buffer(aTextNode:TalJSONNodeU);
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
  Procedure _WriteInt32Value2Buffer(aTextNode:TalJSONNodeU);
  var LInt32: system.Int32;
  begin
    LInt32 := aTextNode.int32;
    _Write2Buffer(LInt32, sizeOf(LInt32));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // \x12 + name + \x00 + int64
  Procedure _WriteInt64Value2Buffer(aTextNode:TalJSONNodeU);
  var LInt64: system.Int64;
  begin
    LInt64 := aTextNode.int64;
    _Write2Buffer(LInt64, sizeOf(LInt64));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteTextNode2Buffer(aTextNode:TalJSONNodeU; aNodeIndex: integer);
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
        else AlJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
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
        else AlJSONDocErrorU(cALJSONInvalidBSONNodeSubType);
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartObjectNode2Buffer(aObjectNode:TalJSONNodeU; aNodeIndex: integer);
  var LNodeList: TalJSONNodeListU;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndObjectNode2Buffer(aObjectNode:TalJSONNodeU; aNodeStartPos: system.Int64);
  Begin
    _WriteByte2Buffer($00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteStartArrayNode2Buffer(aArrayNode:TalJSONNodeU; aNodeIndex: integer);
  var LNodeList: TalJSONNodeListU;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure _WriteEndArrayNode2Buffer(aArrayNode:TalJSONNodeU; aNodeStartPos: system.Int64);
  Begin
    _WriteByte2Buffer($00);
    _WriteInt2Pos(StreamPos + BufferPos - aNodeStartPos, aNodeStartPos);
  end;

begin
  If NodeType <> ntobject then exit;

  CurrentParentNode := nil;
  NodeStack := Tstack<TalJSONNodeU>.Create;
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
          else AlJSONDocErrorU(cAlJSONInvalidNodeType);
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

{*************************************************************}
procedure TALJSONNodeU.SaveToBsonStream(const Stream: TStream);
var buffer: Tbytes;
begin
  SaveToBson(Stream, buffer);
end;

{************************************************************}
procedure TALJSONNodeU.SaveToBsonFile(const FileName: String);
Var LfileStream: TfileStream;
    LTmpFilename: String;
begin
  if (assigned(FDocument)) and
     (doProtectedSave in fDocument.Options) then LTmpFilename := FileName + '.~tmp'
  else LTmpFilename := FileName;
  try

    LfileStream := TfileStream.Create(LTmpFilename,fmCreate);
    Try
      SaveToBsonStream(LfileStream);
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

{********************************************************}
procedure TALJSONNodeU.SaveToBsonBytes(var Bytes: Tbytes);
begin
  SaveToBson(nil, Bytes);
end;

{**************************************************************************************************}
procedure TALJSONNodeU.LoadFromJSONString(const Str: String; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
  if ClearChildNodes then ChildNodes.Clear;
  Try
    FDocument.ParseJson(Str, self)
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{******************************************************************************************************}
procedure TALJSONNodeU.LoadFromJSONStream(const Stream: TStream; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
  if ClearChildNodes then ChildNodes.Clear;
  Try
    FDocument.ParseJSON(ALGetStringFromStreamU(Stream, TEncoding.UTF8), self)
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*****************************************************************************************************}
procedure TALJSONNodeU.LoadFromJSONFile(const FileName: String; Const ClearChildNodes: Boolean = True);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromJSONStream(LfileStream, ClearChildNodes);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{***************************************************************************************************}
procedure TALJSONNodeU.LoadFromBSONBytes(const Bytes: Tbytes; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
  if ClearChildNodes then ChildNodes.Clear;
  Try
    FDocument.ParseBSON(Bytes, self)
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{******************************************************************************************************}
procedure TALJSONNodeU.LoadFromBSONStream(const Stream: TStream; Const ClearChildNodes: Boolean = True);
Begin
  If NodeType <> ntObject then ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
  if ClearChildNodes then ChildNodes.Clear;
  Try
    FDocument.ParseBSON(ALGetBytesFromStream(Stream), self)
  except
    ChildNodes.Clear;
    raise;
  end;
end;

{*****************************************************************************************************}
procedure TALJSONNodeU.LoadFromBSONFile(const FileName: String; Const ClearChildNodes: Boolean = True);
Var LfileStream: TfileStream;
Begin
  LfileStream := TfileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromBSONStream(LfileStream, ClearChildNodes);
  finally
    ALFreeAndNil(LfileStream);
  end;
end;

{*****************************************************************}
constructor TALJSONObjectNodeU.Create(const NodeName: String = '');
begin
  inherited create(NodeName);
  FChildNodes := nil;
end;

{************************************}
destructor TALJSONObjectNodeU.Destroy;
begin
  If assigned(FChildNodes) then ALFreeAndNil(FchildNodes);
  inherited;
end;

{**********************************************************}
function TALJSONObjectNodeU.GetChildNodes: TALJSONNodeListU;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{************************************************************************}
procedure TALJSONObjectNodeU.SetChildNodes(const Value: TALJSONNodeListU);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
  If Assigned(FChildNodes) then ALFreeAndNil(FchildNodes);
  FChildNodes := Value;
end;

{*******************************************************}
function TALJSONObjectNodeU.GetNodeType: TALJSONNodeType;
begin
  Result := NtObject;
end;

{*************************************************************}
function TALJSONObjectNodeU.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := NstObject;
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONObjectNodeU.InternalGetChildNodes: TALJSONNodeListU;
begin
  Result := FChildNodes;
end;

{****************************************************************}
constructor TALJSONArrayNodeU.Create(const NodeName: String = '');
begin
  inherited create(NodeName);
  FChildNodes := nil;
end;

{***********************************}
destructor TALJSONArrayNodeU.Destroy;
begin
  If assigned(FChildNodes) then ALFreeAndNil(FchildNodes);
  inherited;
end;

{*********************************************************}
function TALJSONArrayNodeU.GetChildNodes: TALJSONNodeListU;
begin
  if not Assigned(FChildNodes) then SetChildNodes(CreateChildList);
  Result := FChildNodes;
end;

{***********************************************************************}
procedure TALJSONArrayNodeU.SetChildNodes(const Value: TALJSONNodeListU);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
  If Assigned(FChildNodes) then ALFreeAndNil(FchildNodes);
  FChildNodes := Value;
end;

{******************************************************}
function TALJSONArrayNodeU.GetNodeType: TALJSONNodeType;
begin
  Result := NtArray;
end;

{************************************************************}
function TALJSONArrayNodeU.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := NstArray;
end;

{********************************************}
{Get Childnode without create it if not exist}
function TALJSONArrayNodeU.InternalGetChildNodes: TALJSONNodeListU;
begin
  Result := FChildNodes;
end;

{***************************************************************}
constructor TALJSONTextNodeU.Create(const NodeName: String = '');
begin
  inherited create(NodeName);
  fNodeSubType := nstText;
  fRawNodeValueStr := '';
  FRawNodeValueInt64 := 0;
  fRawNodeValueDefined := [nvStr];
end;

{*****************************************************}
function TALJSONTextNodeU.GetNodeType: TALJSONNodeType;
begin
  Result := NtText;
end;

{***********************************************************}
function TALJSONTextNodeU.GetNodeSubType: TALJSONNodeSubType;
begin
  Result := fNodeSubType;
end;

{************************************************}
function TALJSONTextNodeU.GetNodeValueStr: String;
begin
  if nvStr in fRawNodeValueDefined then result := fRawNodeValueStr
  else begin

    if not (nvInt64 in fRawNodeValueDefined) then ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
    if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);

    case fNodeSubType of
      nstFloat: ALFloatToStrU(GetFloat, fRawNodeValueStr, ALDefaultFormatSettingsU);
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      nstBoolean: ALBoolToStrU(fRawNodeValueStr, getBool, 'true', 'false');
      nstDateTime: ALDateTimeToStrU(GetDateTime, fRawNodeValueStr, ALDefaultFormatSettingsU);
      nstNull: fRawNodeValueStr := 'null';
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
      nstInt32: alinttostrU(GetInt32, fRawNodeValueStr);
      nstTimestamp: ALformatU('Timestamp(%u, %u)', [GetTimestamp.W1,GetTimestamp.W2], fRawNodeValueStr);
      nstInt64: alinttostrU(GetInt64, fRawNodeValueStr);
      else ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
    end;

    fRawNodeValueDefined := fRawNodeValueDefined + [nvStr];
    result := fRawNodeValueStr;

  end;
end;

{*************************************************}
function TALJSONTextNodeU.GetNodeValueInt64: int64;
var LDouble: Double;
    LBool: boolean;
    LDateTime: TdateTime;
    LInt32: system.int32;
    LTimestamp: TALBSONTimestamp;
begin
  if nvInt64 in fRawNodeValueDefined then result := fRawNodeValueInt64
  else begin

    if not (nvStr in fRawNodeValueDefined) then ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
    if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(CALJsonOperationError,GetNodeType);

    case fNodeSubType of
      nstFloat: begin
                  IF not ALTryStrToFloatU(fRawNodeValueStr, LDouble, ALDefaultFormatSettingsU) then ALJSONDocErrorU('%s is not a valid Float', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := Pint64(@LDouble)^;
                end;
      //nstText: can not be retrieve from int64
      //nstObject: can not be retrieve from int64
      //nstArray: can not be retrieve from int64
      //nstBinary: only the binarysubtype is store in int64
      //nstObjectID: can not be retrieve from int64
      nstBoolean: begin
                    IF not ALTryStrToBoolU(fRawNodeValueStr, LBool) then ALJSONDocErrorU('%s is not a valid Boolean', [fRawNodeValueStr]);
                    fRawNodeValueInt64 := ALBoolToInt(LBool);
                  end;
      nstDateTime: begin
                     IF not ALTryStrToDateTimeU(fRawNodeValueStr, LDateTime, ALDefaultFormatSettingsU) then ALJSONDocErrorU('%s is not a valid Datetime', [fRawNodeValueStr]);
                     fRawNodeValueInt64 := Pint64(@LDateTime)^;
                   end;
      nstNull:  begin
                  fRawNodeValueInt64 := 0;
                end;
      //nstRegEx: only the regex options is store in the int64
      //nstJavascript: can not be retrieve from int64
      nstInt32: begin
                  IF not ALTryStrToIntU(fRawNodeValueStr, LInt32) then ALJSONDocErrorU('%s is not a valid Int32', [fRawNodeValueStr]);
                  fRawNodeValueInt64 := LInt32;
                end;
      nstTimestamp: begin
                      IF not ALJSONTryStrToTimestampU(fRawNodeValueStr, LTimestamp) then ALJSONDocErrorU('%s is not a valid Timestamp', [fRawNodeValueStr]);
                      fRawNodeValueInt64 := LTimestamp.I64;
                    end;
      nstInt64: begin
                  IF not ALTryStrToInt64U(fRawNodeValueStr, fRawNodeValueInt64) then ALJSONDocErrorU('%s is not a valid Int64', [fRawNodeValueStr]);
                end;
      else ALJSONDocErrorU(CALJsonOperationError,GetNodeType);
    end;

    fRawNodeValueDefined := fRawNodeValueDefined + [nvInt64];
    result := fRawNodeValueInt64;

  end;
end;

{**************************************************************************************************}
procedure TALJSONTextNodeU.SetNodeValue(const Value: String; const NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
  fNodeSubType := NodeSubType;
  fRawNodeValueStr := Value;
  fRawNodeValueDefined := [nvStr];
end;

{*************************************************************************************************}
procedure TALJSONTextNodeU.SetNodeValue(const Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
  fNodeSubType := NodeSubType;
  fRawNodeValueInt64 := Value;
  if (NodeSubType in [nstBinary, nstRegEx]) then fRawNodeValueDefined := fRawNodeValueDefined + [nvInt64] // keep the fNodeValueStr
  else fRawNodeValueDefined := [nvInt64];
end;

{******************************************************************************************************************************}
procedure TALJSONTextNodeU.SetNodeValue(const StrValue: String; const Int64Value: int64; const NodeSubType: TALJSONNodeSubType);
begin
  if Assigned(FDocument) and (doImmutable in FDocument.Options) then ALJSONDocErrorU(cALJSONImmutable);
  fNodeSubType := NodeSubType;
  fRawNodeValueStr := StrValue;
  fRawNodeValueInt64 := Int64Value;
  fRawNodeValueDefined := [nvStr, nvInt64];
end;

{*******************************************************}
constructor TALJSONNodeListU.Create(Owner: TALJSONNodeU);
begin
  FList:= nil;
  FCount:= 0;
  FCapacity := 0;
  FOwner := Owner;
end;

{**********************************}
destructor TALJSONNodeListU.Destroy;
begin
  Clear;
  inherited;
end;

{*************************************}
{Returns the index of a specified node.
 Call IndexOf to locate a node in the list.
 *Node is the object node to locate.
 IndexOf returns the index of the specified node, where 0 is the index of the first node, 1 is the
 index of the second node, and so on. If the specified node is not in the list, IndexOf returns -1.}
function TALJSONNodeListU.IndexOf(const Node: TALJSONNodeU; const Direction: TDirection = TDirection.FromBeginning): Integer;
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
function TALJSONNodeListU.IndexOf(const Name: String; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if ALNodeMatchesU(Get(Result), Name) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if ALNodeMatchesU(Get(Result), Name) then Exit;
  end;
  Result := -1;
end;

{***************************************************************************************************************************}
function TALJSONNodeListU.IndexOfValue(const Value: String; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).Text = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).Text = Value) then Exit;
  end;
  Result := -1;
end;

{****************************************************************************************************************************}
function TALJSONNodeListU.IndexOfValue(const Value: integer; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).int32 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).int32 = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************************************************************************************************}
function TALJSONNodeListU.IndexOfValue(const Value: int64; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).int64 = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).int64 = Value) then Exit;
  end;
  Result := -1;
end;

{***************************************************************************************************************************}
function TALJSONNodeListU.IndexOfValue(const Value: Double; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).float = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).float = Value) then Exit;
  end;
  Result := -1;
end;

{******************************************************************************************************************************}
function TALJSONNodeListU.IndexOfValue(const Value: TDateTime; const Direction: TDirection = TDirection.FromBeginning): Integer;
begin
  if Direction = TDirection.FromBeginning then begin
    for Result := 0 to Count - 1 do
      if (Get(Result).DateTime = Value) then Exit;
  end
  else begin
    for Result := Count - 1 downto 0 do
      if (Get(Result).DateTime = Value) then Exit;
  end;
  Result := -1;
end;

{**************************************}
{Returns a specified node from the list.
 Call FindNode to access a particular node in the list.
 *NodeName is the node to access. It specifies the NodeName property of the desired node.
 FindNode returns the object of the node if it is in the list. If NodeName does not specify a node in the list,
 FindNode returns nil (Delphi) or NULL (C++).}
function TALJSONNodeListU.FindNode(const NodeName: String; const Direction: TDirection = TDirection.FromBeginning): TALJSONNodeU;
var Index: Integer;
begin
  Index := IndexOf(NodeName, Direction);
  if Index >= 0 then Result := Get(Index)
  else Result := nil;
end;

{**********************************}
{Returns the first node in the list.
Call First to access the first node in the list. If the list is empty, First raises an exception}
function TALJSONNodeListU.First: TALJSONNodeU;
begin
  if Count > 0 then Result := Get(0)
  else Result := nil;
end;

{*********************************}
{Returns the last node in the list.
 Call Last to access the last node in the list. If the list is empty, Last raises an exception.}
function TALJSONNodeListU.Last: TALJSONNodeU;
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
function TALJSONNodeListU.FindSibling(const Node: TALJSONNodeU; Delta: Integer): TALJSONNodeU;
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
function TALJSONNodeListU.Get(Index: Integer): TALJSONNodeU;
begin
  if (Index < 0) or (Index >= FCount) then ALJSONDocErrorU(CALJSONListIndexError, [Index]);
  Result := FList[Index];
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Index identify the desired node. 0 is the index of the first node,
  1 is the index of the second node, and so on}
function TALJSONNodeListU.GetNodeByIndex(const Index: Integer): TALJSONNodeU;
begin
  Result := Get(Index);
end;

{**************************************}
{Returns a specified node from the list.
 GetNode is the read implementation of the Nodes property.
 *Name identify the desired node. it is the NodeName property of a node in the list.
 If Name does not identify a node in the list, GetNode tries to create a new node with the name specified by
 Name. If it can’t create the new node, GetNode raises an exception.}
function TALJSONNodeListU.GetNodeByName(const Name: String): TALJSONNodeU;
begin
  Result := FindNode(Name);
  if (not Assigned(Result)) and
     (assigned(fOwner.OwnerDocument)) and
     (doNodeAutoCreate in fOwner.OwnerDocument.Options) then Result := FOwner.AddChild(Name); // only text node will be added via doNodeAutoCreate
  if not Assigned(Result) then ALJSONDocErrorU(CALJSONNodeNotFound, [Name]);
end;

{*****************************************************************************************}
procedure TALJSONNodeListU.QuickSort(L, R: Integer; ACompare: TALJSONNodeListSortCompareU);
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

{**************************************************************************}
procedure TALJSONNodeListU.CustomSort(Compare: TALJSONNodeListSortCompareU);
begin
  if (FList <> nil) and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

{**************************************}
{Adds a new node to the end of the list.
 Call Add to add a node to the end of the list. Add returns the index of the node once it is added, where 0 is the index
 of the first node in the list, 1 is the index of the second node, and so on.
 *Node is the node to add to the list.}
function TALJSONNodeListU.Add(const Node: TALJSONNodeU): Integer;
begin
  Insert(-1, Node);
  Result := FCount - 1;
end;

{********************************************************}
{Inserts a new node into a specified position in the list.
 Call Insert to add a node at the position specified by Index.
 *Index specifies where to insert the node, where 0 is the first position, 1 is second position, and so on. If Index does not
  specify a valid index, Insert raises an exception.
 *Node is the node to add to the list.}
procedure TALJSONNodeListU.Insert(Index: Integer; const Node: TALJSONNodeU);
begin
  if Index = -1 then begin
    index := FCount;
    if index = FCapacity then Grow;
  end
  else begin
    if (Index < 0) or (Index > FCount) then ALJSONDocErrorU(CALJSONListIndexError, [Index]);
    if FCount = FCapacity then Grow;
    if Index < FCount then ALMove(FList[Index],
                                  FList[Index + 1],
                                  (FCount - Index) * SizeOf(Pointer));
  end;
  Pointer(FList[index]) := nil;
  FList[index] := Node;
  Inc(FCount);
  Node.SetParentNode(Fowner);
end;

{**************************************}
{Removes a specified node from the list.
 Delete removes the node specified by the Index or Name parameter.
 *Index identifies the node to remove by index rather than name. Index ranges from 0 to one less than the value of the Count property.
 Delete returns the index of the node that was removed. If there was no node that matched the value of Index Delete returns –1.}
function TALJSONNodeListU.Delete(const Index: Integer): Integer;
var Node: TALJSONNodeU;
begin
  Node := Get(Index);
  FList[Index] := nil; // to decrease the refcount of Node
  Dec(FCount);
  if Index < FCount then begin
    ALMove(FList[Index + 1],
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
function TALJSONNodeListU.Delete(const Name: String): Integer;
begin
  result := indexOf(Name);
  if Result >= 0 then Delete(Result);
end;

{**************************************}
{Removes a specified node from the list.
 Remove removes the specified node from the list.
 *Node is the node to remove from the list.
 Remove returns the index of Node before it was removed. If node is not a node in the list, Remove returns -1.}
function TALJSONNodeListU.Remove(const Node: TALJSONNodeU): Integer;
begin
  Result := IndexOf(Node);
  if Result >= 0 then Delete(Result);
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeListU.Extract(const Node: TALJSONNodeU): TALJSONNodeU;
var I: Integer;
begin
  Result := nil;
  I := IndexOf(Node);
  if I >= 0 then result := Extract(i);
end;

{***********************************************************}
procedure TALJSONNodeListU.Exchange(Index1, Index2: Integer);
var Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then ALJSONDocErrorU(cALJSONListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then ALJSONDocErrorU(cALJSONListIndexError, [Index2]);
  Item := pointer(FList[Index1]);
  pointer(FList[Index1]) := pointer(FList[Index2]);
  pointer(FList[Index2]) := Item;
end;

{***********************************************************}
{Removes a specified object from the list without freeing it.
 Call Extract to remove an object from the list without freeing the object itself.
 After an object is removed, all the objects that follow it are moved up in index position and Count is decremented.}
function TALJSONNodeListU.Extract(const index: integer): TALJSONNodeU;
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
function TALJSONNodeListU.ReplaceNode(const OldNode, NewNode: TALJSONNodeU): TALJSONNodeU;
var Index: Integer;
begin
  Index := indexOf(OldNode);
  Result := Extract(Index);
  Insert(Index, NewNode);
end;

{*******************************}
{Removes all nodes from the list.
 Call Clear to empty the list.
 Note:	Clear does not call the BeginUpdate and EndUpdate methods, even though it may result in the
 deletion of more than one node.}
procedure TALJSONNodeListU.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{******************************}
procedure TALJSONNodeListU.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{***********************************************************}
procedure TALJSONNodeListU.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) then ALJSONDocErrorU(CALJSONListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{*****************************************************}
procedure TALJSONNodeListU.SetCount(NewCount: Integer);
var I: Integer;
begin
  if (NewCount < 0) then ALJSONDocErrorU(CALJSONListCountError, [NewCount]);
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
  FCount := NewCount;
end;

{*************************************************}
Procedure ALJSONToTStringsU(const AJsonStr: String;
                            const aFormatSettings: TALformatSettingsU;
                            const aPath: String;
                            const aLst: TALStringsU;
                            Const aNullStr: String = 'null';
                            Const aTrueStr: String = 'true';
                            Const aFalseStr: String = 'false');

var LJsonDocument: TALJSONDocumentU;
    LContainChilds: boolean;
begin
  LJsonDocument := TALJSONDocumentU.Create(aFormatSettings);
  try

    LJsonDocument.onParseText := procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                 begin
                                   if (NodeSubType = nstBoolean)   then aLst.Add(aPath + Path + aLst.NameValueSeparator + ALBoolToStrU(Args[0].VBoolean,aTrueStr,aFalseStr))
                                   else if (NodeSubType = nstnull) then aLst.Add(aPath + Path + aLst.NameValueSeparator + aNullStr)
                                   else                                 aLst.Add(aPath + Path + aLst.NameValueSeparator + String(Args[0].VUnicodeString));
                                   LContainChilds := True;
                                 end;

    LJsonDocument.onParseStartObject := procedure (Sender: TObject; const Path: String; const Name: String)
                                        begin
                                          LContainChilds := False;
                                        end;

    LJsonDocument.onParseEndObject := procedure (Sender: TObject; const Path: String; const Name: String)
                                      begin
                                        if (not LContainChilds) and (aPath + Path <> ''{Path = '' mean it's the root object}) then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '{}');
                                        LContainChilds := True;
                                      end;

    LJsonDocument.onParseStartArray := procedure (Sender: TObject; const Path: String; const Name: String)
                                       begin
                                         LContainChilds := False;
                                       end;

    LJsonDocument.onParseEndArray := procedure (Sender: TObject; const Path: String; const Name: String)
                                     begin
                                       if not LContainChilds then aLst.Add(aPath+ Path + aLst.NameValueSeparator + '[]');
                                       LContainChilds := True;
                                     end;

    LJsonDocument.LoadFromJSONString(AJsonStr, true{saxMode});
  finally
    ALFreeAndNil(LJsonDocument);
  end;
end;

{*************************************************}
Procedure ALJSONToTStringsU(const AJsonStr: String;
                            const aFormatSettings: TALformatSettingsU;
                            const aLst: TALStringsU;
                            Const aNullStr: String = 'null';
                            Const aTrueStr: String = 'true';
                            Const aFalseStr: String = 'false');
begin
 ALJSONToTStringsU(AJsonStr,
                   aFormatSettings,
                   '',
                   aLst,
                   aNullStr,
                   aTrueStr,
                   aFalseStr);
end;

{********************************************************}
Procedure ALJSONToTStringsU(const aJsonNode: TALJSONNodeU;
                            Const aPath: String;
                            const aLst: TALStringsU;
                            Const aNullStr: String = 'null';
                            Const aTrueStr: String = 'true';
                            Const aFalseStr: String = 'false');
var LTmpPath: String;
    I: integer;
begin
  if aJsonNode.ChildNodes.Count > 0 then begin
    for I := 0 to aJsonNode.ChildNodes.Count - 1 do begin

      if aJsonNode.NodeType = ntArray then LTmpPath := aPath + '[' + alinttostrU(I) + ']'
      else begin
        if aJsonNode.ChildNodes[I].NodeName = '' then raise Exception.Create('Nodename can not be empty');
        LTmpPath := aPath + alIfThenU(aPath <> '', '.', '') + aJsonNode.ChildNodes[I].NodeName;
      end;

      case aJsonNode.ChildNodes[I].NodeType of

        ntObject: ALJSONToTStringsU(aJsonNode.ChildNodes[I],
                                    LTmpPath,
                                    aLst,
                                    aNullStr,
                                    aTrueStr,
                                    aFalseStr);

        ntArray: ALJSONToTStringsU(aJsonNode.ChildNodes[I],
                                   LTmpPath,
                                   aLst,
                                   aNullStr,
                                   aTrueStr,
                                   aFalseStr);

        ntText: begin
                  if (aJsonNode.ChildNodes[I].NodeSubType = nstBoolean) then   aLst.Add(LTmpPath + aLst.NameValueSeparator + ALBoolToStrU(aJsonNode.ChildNodes[I].Bool,aTrueStr,aFalseStr))
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

{********************************************************}
Procedure ALJSONToTStringsU(const aJsonNode: TALJSONNodeU;
                            const aLst: TALStringsU;
                            Const aNullStr: String = 'null';
                            Const aTrueStr: String = 'true';
                            Const aFalseStr: String = 'false');
begin
  ALJSONToTStringsU(aJsonNode,
                    '',
                    aLst,
                    aNullStr,
                    aTrueStr,
                    aFalseStr)
end;

{**************************************************}
procedure ALTStringsToJsonU(const aLst: TALStringsU;
                            const aJsonNode: TALJSONNodeU;
                            Const aPath: String = '';
                            Const aNameToLowerCase: boolean = false;
                            Const aNullStr: String = 'null');

var LIndex: Integer;
    LNames:  TALStringListU;
    LLowerName: String;
    LCurrJsonNode, aTmpJsonNode: TALJSONNodeU;
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
  LNames := TALStringListU.Create;
  try

    //init aNames.linebreak
    LNames.LineBreak := '.';

    // scroll the aLst
    for I := 0 to aLst.Count - 1 do begin

      //if it's contain path
      if (aPath = '') or
         (alposExIgnoreCaseU(aPath + '.',aLst.Names[I]) = 1) then begin

        // path.aggregated_data.properties.types[3].translations.usa =>
        //   aggregated_data
        //   properties
        //   types
        //   [3]
        //   translations
        //   usa
        if (aPath <> '') then LNames.Text := ALStringReplaceU(ALStringReplaceU(aLst.Names[I],
                                                                              aPath + '.',
                                                                              '',
                                                                              [rfIgnoreCase]),
                                                              '[',
                                                              '.[',
                                                              [rfReplaceAll])
        else LNames.Text := ALStringReplaceU(aLst.Names[I],
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
               (not ALTryStrToIntU(ALCopyStrU(LNames[J], 2, Length(LNames[J]) - 2), LIndex)) then raise EALExceptionU.CreateFmt('Wrong path: "%s"', [aLst.Names[I]]);
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
            LLowerName := alifThenU(aNameToLowerCase, allowercaseU(LNames[J]), LNames[J]);
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
function ALJsonEncodeFloatWithNodeSubTypeHelperU(const aValue: double): String;
begin
  result := ALFloatToStrU(aValue, ALDefaultFormatSettingsU);
end;

{****************************************************************************}
function ALJsonEncodeTextWithNodeSubTypeHelperU(const aValue: String): String;
begin
  result := '"'+ALJavascriptEncodeU(aValue)+'"';
end;

{******************************************************************************}
function ALJsonEncodeBinaryWithNodeSubTypeHelperU(const aValue: String): String;
begin
  result := 'BinData(0, "' + aValue + '")';
end;

{********************************************************************************}
function ALJsonEncodeObjectIDWithNodeSubTypeHelperU(const aValue: String): String;
begin
  result := 'ObjectId("'+aValue+'")';
end;

{********************************************************************************}
function ALJsonEncodeBooleanWithNodeSubTypeHelperU(const aValue: Boolean): String;
begin
  if aValue then result := 'true'
  else result := 'false';
end;

{***********************************************************************************}
function ALJsonEncodeDateTimeWithNodeSubTypeHelperU(const aValue: TdateTime): String;
begin
  result := ALFormatDateTimeU('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', aValue, ALDefaultFormatSettingsU);
end;

{**********************************************************************************}
function ALJsonEncodeJavascriptWithNodeSubTypeHelperU(const aValue: String): String;
begin
  result := aValue;
end;

{****************************************************************************}
function ALJsonEncodeInt64WithNodeSubTypeHelperU(const aValue: int64): String;
begin
  result := 'NumberLong(' + ALIntToStrU(aValue) + ')';
end;

{****************************************************************************}
function ALJsonEncodeInt32WithNodeSubTypeHelperU(const aValue: int32): String;
begin
  result := 'NumberInt(' + ALIntToStrU(aValue) + ')';
end;

{******************************************************}
function ALJsonEncodeNullWithNodeSubTypeHelperU: String;
begin
  result := 'null';
end;

{***************************************************************}
function ALJsonEncodeWithNodeSubTypeHelperU(const aValue: String;
                                            const aNodeSubType: TALJSONNodeSubType;
                                            const aFormatSettings: TALformatSettingsU): String;
begin
  case aNodeSubType of
    nstFloat:      begin
                     if @aFormatSettings <> @ALDefaultFormatSettingsU then result := ALJsonEncodeFloatWithNodeSubTypeHelperU(ALStrToFloatU(aValue, aFormatSettings))
                     else result := aValue;
                   end;
    nstText:       result := ALJsonEncodeTextWithNodeSubTypeHelperU(aValue);
    nstBinary:     result := ALJsonEncodeBinaryWithNodeSubTypeHelperU(aValue);
    nstObjectID:   result := ALJsonEncodeObjectIDWithNodeSubTypeHelperU(aValue);
    nstBoolean:    result := ALJsonEncodeBooleanWithNodeSubTypeHelperU(ALStrToBoolU(aValue));
    nstDateTime:   begin
                     if aValue = 'NOW' then result := ALJsonEncodeDateTimeWithNodeSubTypeHelperU(ALUtcNow)
                     else result := ALJsonEncodeDateTimeWithNodeSubTypeHelperU(ALStrToDateTimeU(aValue, aFormatSettings));
                   end;
    nstJavascript: result := ALJsonEncodeJavascriptWithNodeSubTypeHelperU(aValue);
    nstInt32:      result := ALJsonEncodeInt32WithNodeSubTypeHelperU(ALstrToIntU(aValue));
    nstInt64:      result := ALJsonEncodeInt64WithNodeSubTypeHelperU(ALstrToInt64U(aValue));
    nstNull:       result := ALJsonEncodeNullWithNodeSubTypeHelperU;
    nstObject:     raise Exception.Create('Unsupported Node SubType');
    nstArray:      raise Exception.Create('Unsupported Node SubType');
    nstRegEx:      raise Exception.Create('Unsupported Node SubType');
    nstTimestamp:  raise Exception.Create('Unsupported Node SubType');
    else raise Exception.Create('Unknown Node SubType');
  end;
end;

initialization

{$IFNDEF ALHideAnsiString}
  vALJsonISODateFormatSettings := TalFormatSettings.Create('en-US');
  vALJsonISODateFormatSettings.DateSeparator := '-';
  vALJsonISODateFormatSettings.TimeSeparator := ':';
  vALJsonISODateFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  vALJsonISODateFormatSettings.ShortTimeFormat := 'hh:nn:ss.zzz';
  vALDefaultNodeIndent := '  '; { 2 spaces }
{$ENDIF}

  vALJsonISODateFormatSettingsU := TalFormatSettingsU.Create('en-US');
  vALJsonISODateFormatSettingsU.DateSeparator := '-';
  vALJsonISODateFormatSettingsU.TimeSeparator := ':';
  vALJsonISODateFormatSettingsU.ShortDateFormat := 'yyyy-mm-dd';
  vALJsonISODateFormatSettingsU.ShortTimeFormat := 'hh:nn:ss.zzz';
  vALDefaultNodeIndentU := '  '; { 2 spaces }

end.
